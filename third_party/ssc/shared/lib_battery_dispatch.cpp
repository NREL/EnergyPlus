/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "lib_battery_dispatch.h"
#include "lib_battery_powerflow.h"
#include "lib_shared_inverter.h"
#include "lib_utility_rate.h"

#include <math.h>
#include <algorithm>
#include <numeric>

/*
Dispatch base class
*/
dispatch_t::dispatch_t(battery_t * Battery, double dt_hour, double SOC_min, double SOC_max, int current_choice, double Ic_max, double Id_max,
	double Pc_max_kwdc, double Pd_max_kwdc, double Pc_max_kwac, double Pd_max_kwac,
	double t_min, int mode, int battMeterPosition)
{
	// initialize battery power flow
	std::unique_ptr<BatteryPowerFlow> tmp(new BatteryPowerFlow(dt_hour));
	m_batteryPowerFlow = std::move(tmp);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();
	m_batteryPower->currentChargeMax = Ic_max;
	m_batteryPower->currentDischargeMax = Id_max;
	m_batteryPower->stateOfChargeMax = SOC_max;
	m_batteryPower->stateOfChargeMin = SOC_min;
	m_batteryPower->depthOfDischargeMax = SOC_max - SOC_min;
	m_batteryPower->powerBatteryChargeMaxDC = Pc_max_kwdc;
	m_batteryPower->powerBatteryDischargeMaxDC = Pd_max_kwdc;
	m_batteryPower->powerBatteryChargeMaxAC = Pc_max_kwac;
	m_batteryPower->powerBatteryDischargeMaxAC = Pd_max_kwac;
	m_batteryPower->meterPosition = battMeterPosition;

	// initalize Battery and a copy of the Battery for iteration
	_Battery = Battery;
	_Battery_initial = new battery_t(*_Battery);

	// Call the dispatch init method
	init(_Battery, dt_hour, current_choice, t_min, mode);
}

void dispatch_t::init(battery_t * Battery, double dt_hour, int current_choice, double t_min, int mode)
{
	_dt_hour = dt_hour;
	_current_choice = current_choice;
	_t_min = t_min;
	_mode = mode;

	// limit the switch from charging to discharge so that doesn't flip-flop subhourly
	_t_at_mode = 1000;
	_prev_charging = false;
	_charging = false;
	_e_max = Battery->battery_voltage()*Battery->battery_charge_maximum()*util::watt_to_kilowatt*0.01*(m_batteryPower->stateOfChargeMax - m_batteryPower->stateOfChargeMin);
	_grid_recharge = false;

	// initialize powerflow model
	m_batteryPower->canClipCharge = false;
	m_batteryPower->canPVCharge = false;
	m_batteryPower->canGridCharge = false;
	m_batteryPower->canDischarge = false;
}

// deep copy
dispatch_t::dispatch_t(const dispatch_t& dispatch)
{
	std::unique_ptr<BatteryPowerFlow> tmp(new BatteryPowerFlow(*dispatch.m_batteryPowerFlow));
	m_batteryPowerFlow = std::move(tmp);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();

	_Battery = new battery_t(*dispatch._Battery);
	_Battery_initial = new battery_t(*dispatch._Battery_initial);
	init(_Battery, dispatch._dt_hour, dispatch._current_choice, dispatch._t_min, dispatch._mode);
}

// shallow copy from dispatch to this
void dispatch_t::copy(const dispatch_t * dispatch)
{
	_Battery->copy(dispatch->_Battery);
	_Battery_initial->copy(dispatch->_Battery_initial);
	init(_Battery, dispatch->_dt_hour,  dispatch->_current_choice, dispatch->_t_min, dispatch->_mode);

	// can't create shallow copy of unique ptr
	std::unique_ptr<BatteryPowerFlow> tmp(new BatteryPowerFlow(*dispatch->m_batteryPowerFlow));
	m_batteryPowerFlow = std::move(tmp);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();
}
void dispatch_t::delete_clone()
{
	// need to delete both, since allocated memory for both in deep copy
	if (_Battery){
        _Battery->delete_clone();
        delete _Battery;
	}
	if (_Battery_initial){
        _Battery_initial->delete_clone();
	    delete _Battery_initial;
	    _Battery_initial = nullptr;
	}
}
dispatch_t::~dispatch_t()
{
	// original _Battery doesn't need deleted, since was a pointer passed in
	if (_Battery_initial) _Battery_initial->delete_clone();
	delete _Battery_initial;
}
void dispatch_t::finalize(size_t idx, double &I)
{
	_Battery->copy(_Battery_initial);
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerGridToBattery = 0;
	m_batteryPower->powerBatteryToGrid = 0;
	m_batteryPower->powerPVToGrid = 0;
	_Battery->run(idx, I);
}

bool dispatch_t::check_constraints(double &I, size_t count)
{
	bool iterate;
	double I_initial = I;
	bool current_iterate = false;
	bool power_iterate = false;

	// don't allow any changes to violate current limits
	if (restrict_current(I))
	{
		current_iterate = true;
	}
	// don't allow violations of power limits
	else if (restrict_power(I))
	{
		power_iterate = true;
	}
	// decrease the current draw if took too much
	if (I > 0 && _Battery->battery_soc() < m_batteryPower->stateOfChargeMin - tolerance)
	{
		m_batteryPower->powerBatteryTarget = _Battery_initial->calculate_max_discharge_kw(&I);
	}
	// decrease the current charging if charged too much
	else if (I < 0 &&_Battery->battery_soc() > m_batteryPower->stateOfChargeMax + tolerance)
	{
		m_batteryPower->powerBatteryTarget = _Battery_initial->calculate_max_charge_kw(&I);
    }
	// Don't allow grid charging unless explicitly allowed (reduce charging)
	if (!m_batteryPower->canGridCharge && I < 0 && m_batteryPower->powerGridToBattery > tolerance)
	{
        m_batteryPower->powerBatteryTarget += m_batteryPower->powerGridToBattery;
        I = _Battery->calculate_current_for_power_kw(m_batteryPower->powerBatteryTarget);
        m_batteryPower->powerGridToBattery = 0;
	}
	// Don't allow grid charging if producing PV
	else if (m_batteryPower->connectionMode == dispatch_t::DC_CONNECTED &&
		m_batteryPower->powerGridToBattery > 0 &&
		(m_batteryPower->powerPVToGrid > 0 || m_batteryPower->powerPVToLoad > 0))
	{
        m_batteryPower->powerBatteryTarget += m_batteryPower->powerGridToBattery;
        I = _Battery->calculate_current_for_power_kw(m_batteryPower->powerBatteryTarget);
	}
    // Error checking for battery charging
    double power_to_batt = m_batteryPower->powerBatteryDC;
	if (m_batteryPower->connectionMode == dispatch_t::DC_CONNECTED){
	    power_to_batt = -(m_batteryPower->powerPVToBattery + m_batteryPower->powerFuelCellToBattery);
	    if (m_batteryPower->sharedInverter->powerDC_kW < 0)
	        power_to_batt += m_batteryPower->sharedInverter->powerDC_kW;    // charging from grid
	    power_to_batt *= m_batteryPower->singlePointEfficiencyDCToDC;
	    // if error is from from numerical solution, may not need to adjust battery
	}
	else {
	    power_to_batt = -(m_batteryPower->powerPVToBattery + m_batteryPower->powerGridToBattery + m_batteryPower->powerFuelCellToBattery);
	    power_to_batt *= m_batteryPower->singlePointEfficiencyACToDC;
    }

    if (m_batteryPower->powerBatteryTarget < 0 && abs(power_to_batt - m_batteryPower->powerBatteryTarget) > tolerance * fabs(power_to_batt)) {
        m_batteryPower->powerBatteryTarget = power_to_batt;
        m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;
        I = _Battery_initial->calculate_current_for_power_kw(m_batteryPower->powerBatteryTarget);
    }
	// Don't allow battery to discharge if it gets wasted due to inverter efficiency limitations
	// Typically, this would be due to low power flow, so just cut off battery.
	if (m_batteryPower->connectionMode == dispatch_t::DC_CONNECTED && m_batteryPower->sharedInverter->efficiencyAC < m_batteryPower->inverterEfficiencyCutoff)
	{
		// The requested DC power
		double powerBatterykWdc = _Battery->capacity_model()->I() * _Battery->battery_voltage() * util::watt_to_kilowatt;

		// if battery discharging, see if can back off to get higher efficiency
		if (m_batteryPower->powerBatteryDC > 0) {
			if (powerBatterykWdc + m_batteryPower->powerPV > m_batteryPower->sharedInverter->getACNameplateCapacitykW()) {
				powerBatterykWdc = m_batteryPower->sharedInverter->getACNameplateCapacitykW() - m_batteryPower->powerPV;
				powerBatterykWdc = fmax(powerBatterykWdc, 0);
                m_batteryPower->powerBatteryTarget = powerBatterykWdc;
                I = _Battery->calculate_current_for_power_kw(m_batteryPower->powerBatteryTarget);
            }
		}
		// if charging, this will also be due to low powerflow from grid-charging, just cut off that component
		else if (m_batteryPower->powerBatteryDC < 0 && m_batteryPower->powerGridToBattery > 0){
			I *= fmax(1.0 - fabs(m_batteryPower->powerGridToBattery * m_batteryPower->sharedInverter->efficiencyAC * 0.01 / m_batteryPower->powerBatteryDC), 0);
            m_batteryPower->powerBatteryTarget = _Battery->calculate_voltage_for_current(I) * I * util::watt_to_kilowatt;
        }
	}

	iterate = abs(I_initial - I) > tolerance;

	// update constraints for current, power, if they are now violated
	if (!current_iterate) {
		current_iterate = restrict_current(I);
	}
	if (!power_iterate) {
		power_iterate = restrict_power(I);
	}

	// iterate if any of the conditions are met
	if (iterate || current_iterate || power_iterate)
		iterate = true;

	// stop iterating after n tries
	if (count > battery_dispatch::constraintCount)
		iterate = false;

	// don't allow battery to flip from charging to discharging or vice versa
	if (fabs(I) > tolerance && (I_initial / I) < 0) {
		I = 0;
		iterate = false;
	}


    // reset
	if (iterate)
	{
		_Battery->copy(_Battery_initial);
        m_batteryPowerFlow->calculate();
    }

	return iterate;
}
message dispatch_t::get_messages() { return _message; };
void dispatch_t::SOC_controller()
{
	_charging = _prev_charging;

	// Implement minimum SOC cut-off
	if (m_batteryPower->powerBatteryDC > 0)
	{
		if (_Battery->battery_soc() <= m_batteryPower->stateOfChargeMin + tolerance) {
			m_batteryPower->powerBatteryDC = 0;
		}
		else {
			_charging = false;
		}
	}
	// Maximum SOC cut-off
	else if (m_batteryPower->powerBatteryDC < 0)
	{
		if (_Battery->battery_soc() >= m_batteryPower->stateOfChargeMax - tolerance) {
			m_batteryPower->powerBatteryDC = 0;
		}
		else {
			_charging = true;
		}
	}
}

void dispatch_t::switch_controller()
{
	// Implement rapid switching check
	if (_charging != _prev_charging)
	{
		if (_t_at_mode <= _t_min)
		{
			m_batteryPower->powerBatteryDC = 0.;
			_charging = _prev_charging;
		}
		else
			_t_at_mode = 0;
	}
	_t_at_mode += (int)(round(_dt_hour * util::hour_to_min));
}
double dispatch_t::current_controller(double power_kw)
{
    double I = _Battery->calculate_current_for_power_kw(power_kw);
	restrict_current(I);
	return I;
}
bool dispatch_t::restrict_current(double &I)
{
	bool iterate = false;
	if (_current_choice == RESTRICT_CURRENT || _current_choice == RESTRICT_BOTH)
	{
		if (I < 0)
		{
			if (fabs(I) > m_batteryPower->currentChargeMax)
			{
				I = -m_batteryPower->currentChargeMax;
				iterate = true;
			}
		}
		else
		{
			if (I > m_batteryPower->currentDischargeMax)
			{
				I = m_batteryPower->currentDischargeMax;
				iterate = true;
			}
		}
	}
	return iterate;
}
bool dispatch_t::restrict_power(double &I)
{
	bool iterate = false;
	if (_current_choice == RESTRICT_POWER || _current_choice == RESTRICT_BOTH)
	{
		double powerBattery = I * _Battery->battery_voltage() * util::watt_to_kilowatt;
		double powerBatteryAC = powerBattery;
        if (powerBattery < 0)
            powerBatteryAC = powerBattery / m_batteryPower->singlePointEfficiencyACToDC;
        else if (powerBattery > 0)
            powerBatteryAC = powerBattery * m_batteryPower->singlePointEfficiencyDCToAC;

        double dP = 0.;

		// charging
		if (powerBattery < 0)
		{
			if (fabs(powerBattery) > m_batteryPower->powerBatteryChargeMaxDC * (1 + low_tolerance))
			{
				dP = fabs(m_batteryPower->powerBatteryChargeMaxDC - fabs(powerBattery));

				// increase (reduce) charging magnitude by percentage
				I -= (dP / fabs(powerBattery)) * I;
				iterate = true;
			}
			else if (m_batteryPower->connectionMode == m_batteryPower->AC_CONNECTED &&
				fabs(powerBatteryAC) > m_batteryPower->powerBatteryChargeMaxAC * (1 + low_tolerance))
			{
				dP = fabs(m_batteryPower->powerBatteryChargeMaxAC - fabs(powerBatteryAC));

				// increase (reduce) charging magnitude by percentage
				I -= (dP / fabs(powerBattery)) * I;
				iterate = true;
			}
			// This could just be grid power since that's technically the only AC component.  But, limit all to this
			else if (m_batteryPower->connectionMode == m_batteryPower->DC_CONNECTED &&
				fabs(powerBatteryAC) > m_batteryPower->powerBatteryChargeMaxAC * (1 + low_tolerance))
			{
				dP = fabs(m_batteryPower->powerBatteryChargeMaxAC - fabs(powerBatteryAC));

				// increase (reduce) charging magnitude by percentage
				I -= (dP / fabs(powerBattery)) * I;
				iterate = true;
			}
		}
		else
		{
			if (fabs(powerBattery) > m_batteryPower->powerBatteryDischargeMaxDC * (1 + low_tolerance))
			{
				dP = fabs(m_batteryPower->powerBatteryDischargeMaxDC - powerBattery);

				// decrease discharging magnitude
				I -= (dP / fabs(powerBattery)) * I;
				iterate = true;
			}
			else if (fabs(powerBatteryAC) > m_batteryPower->powerBatteryDischargeMaxAC * (1 + low_tolerance))
			{
				dP = fabs(m_batteryPower->powerBatteryDischargeMaxAC - powerBatteryAC);

				// decrease discharging magnitude
				I -= (dP / fabs(powerBattery)) * I;
				iterate = true;
			}
		}
	}
	return iterate;
}

void dispatch_t::runDispatch(size_t year, size_t hour_of_year, size_t step)
{
	// Ensure the battery operates within the state-of-charge limits
	SOC_controller();

	// Ensure the battery isn't switching rapidly between charging and dischaging
	switch_controller();

	// Calculate current, and ensure the battery falls within the current limits
	double I = current_controller(m_batteryPower->powerBatteryDC);

	// Setup battery iteration
	_Battery_initial->copy(_Battery);
	bool iterate = true;
	size_t count = 0;
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, static_cast<size_t>(1 / _dt_hour));

	do {

		// Run Battery Model to update charge based on charge/discharge
		m_batteryPower->powerBatteryDC = _Battery->run(lifetimeIndex, I);

		// Update power flow calculations, calculate AC power, and check the constraints
		m_batteryPowerFlow->calculate();
		iterate = check_constraints(I, count);

		// If current changed during last iteration of constraints checker, recalculate internal battery state
		if (!iterate) {
			finalize(lifetimeIndex, I);
			m_batteryPower->powerBatteryDC = I * _Battery->battery_voltage() * util::watt_to_kilowatt;
		}
		else {
			_Battery->copy(_Battery_initial);
		}
		
		count++;

	} while (iterate);

	// finalize AC power flow calculation and update for next step
	m_batteryPowerFlow->calculate();
	_prev_charging = _charging;
}

double dispatch_t::power_tofrom_battery() { return m_batteryPower->powerBatteryAC; }
double dispatch_t::power_tofrom_grid() { return m_batteryPower->powerGrid; }
double dispatch_t::power_gen() { return m_batteryPower->powerGeneratedBySystem; }
double dispatch_t::power_pv_to_load() { return m_batteryPower->powerPVToLoad; }
double dispatch_t::power_battery_to_load() { return m_batteryPower->powerBatteryToLoad; }
double dispatch_t::power_grid_to_load() { return m_batteryPower->powerGridToLoad; }
double dispatch_t::power_fuelcell_to_load() { return m_batteryPower->powerFuelCellToLoad; }
double dispatch_t::power_pv_to_batt() { return m_batteryPower->powerPVToBattery; }
double dispatch_t::power_grid_to_batt() { return m_batteryPower->powerGridToBattery; }
double dispatch_t::power_fuelcell_to_batt() { return m_batteryPower->powerFuelCellToBattery; }
double dispatch_t::power_pv_to_grid() { return m_batteryPower->powerPVToGrid; }
double dispatch_t::power_battery_to_grid() { return m_batteryPower->powerBatteryToGrid; }
double dispatch_t::power_fuelcell_to_grid() {return m_batteryPower->powerFuelCellToGrid;}
double dispatch_t::power_conversion_loss() { return m_batteryPower->powerConversionLoss; }
double dispatch_t::power_system_loss() { return m_batteryPower->powerSystemLoss; }
double dispatch_t::battery_power_to_fill() { return _Battery->battery_power_to_fill(m_batteryPower->stateOfChargeMax); }
double dispatch_t::battery_soc() { return _Battery->battery_soc(); }
BatteryPowerFlow * dispatch_t::getBatteryPowerFlow() { return m_batteryPowerFlow.get(); }
BatteryPower * dispatch_t::getBatteryPower() { return m_batteryPower; }
/*
Manual Dispatch
*/
dispatch_manual_t::dispatch_manual_t(battery_t * Battery, double dt, double SOC_min, double SOC_max, int current_choice, double Ic_max, double Id_max,
	double Pc_max_kwdc, double Pd_max_kwdc, double Pc_max_kwac, double Pd_max_kwac,
	double t_min, int mode, int battMeterPosition,
	util::matrix_t<size_t> dm_dynamic_sched, util::matrix_t<size_t> dm_dynamic_sched_weekend,
	std::vector<bool> dm_charge, std::vector<bool> dm_discharge, std::vector<bool> dm_gridcharge, std::vector<bool> dm_fuelcellcharge,
	std::map<size_t, double>  dm_percent_discharge, std::map<size_t, double>  dm_percent_gridcharge)
	: dispatch_t(Battery, dt, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
	t_min, mode, battMeterPosition)
{
	init_with_vects(dm_dynamic_sched, dm_dynamic_sched_weekend, dm_charge, dm_discharge, dm_gridcharge, dm_fuelcellcharge, dm_percent_discharge, dm_percent_gridcharge);
}

void dispatch_manual_t::init_with_vects(
	util::matrix_t<size_t> dm_dynamic_sched,
	util::matrix_t<size_t> dm_dynamic_sched_weekend,
	std::vector<bool> dm_charge,
	std::vector<bool> dm_discharge,
	std::vector<bool> dm_gridcharge,
	std::vector<bool> dm_fuelcellcharge,
	std::map<size_t, double> dm_percent_discharge,
	std::map<size_t, double> dm_percent_gridcharge)
{
	_sched = dm_dynamic_sched;
	_sched_weekend = dm_dynamic_sched_weekend;
	_charge_array = dm_charge;
	_discharge_array = dm_discharge;
	_gridcharge_array = dm_gridcharge;
	_fuelcellcharge_array = dm_fuelcellcharge;
	_percent_discharge_array = dm_percent_discharge;
	_percent_charge_array = dm_percent_gridcharge;
}

// deep copy from dispatch to this
dispatch_manual_t::dispatch_manual_t(const dispatch_t & dispatch) :
dispatch_t(dispatch)
{
	const dispatch_manual_t * tmp = dynamic_cast<const dispatch_manual_t *>(&dispatch);
	init_with_vects(tmp->_sched, tmp->_sched_weekend,
		tmp->_charge_array, tmp->_discharge_array, tmp->_gridcharge_array, tmp->_fuelcellcharge_array,
		tmp->_percent_discharge_array, tmp->_percent_charge_array);
}

// shallow copy from dispatch to this
void dispatch_manual_t::copy(const dispatch_t * dispatch)
{
	dispatch_t::copy(dispatch);
	const dispatch_manual_t * tmp = dynamic_cast<const dispatch_manual_t *>(dispatch);
	init_with_vects(tmp->_sched, tmp->_sched_weekend,
		tmp->_charge_array, tmp->_discharge_array, tmp->_gridcharge_array, tmp->_fuelcellcharge_array,
		tmp->_percent_discharge_array, tmp->_percent_charge_array);
}

void dispatch_manual_t::prepareDispatch(size_t hour_of_year, size_t )
{
	size_t m, h;
	util::month_hour(hour_of_year, m, h);
	size_t column = h - 1;
	size_t iprofile = 0;

	bool is_weekday = util::weekday(hour_of_year);
	if (!is_weekday && _mode == MANUAL)
		iprofile = _sched_weekend(m - 1, column);
	else
		iprofile = _sched(m - 1, column);  // 1-based

	m_batteryPower->canPVCharge = _charge_array[iprofile - 1];
	m_batteryPower->canDischarge = _discharge_array[iprofile - 1];
	m_batteryPower->canGridCharge = _gridcharge_array[iprofile - 1];

	if (iprofile < _fuelcellcharge_array.size()) {
		m_batteryPower->canFuelCellCharge = _fuelcellcharge_array[iprofile - 1];
	}

	_percent_discharge = 0.;
	_percent_charge = 0.;

	if (m_batteryPower->canDischarge){ _percent_discharge = _percent_discharge_array[iprofile]; }
	if (m_batteryPower->canPVCharge || m_batteryPower->canFuelCellCharge){ _percent_charge = 100.; }
	if (m_batteryPower->canGridCharge){ _percent_charge = _percent_charge_array[iprofile]; }
}
void dispatch_manual_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	prepareDispatch(hour_of_year, step);

	// Initialize power flow model by calculating the battery power to dispatch
	m_batteryPowerFlow->initialize(_Battery->capacity_model()->SOC());

	// Run the dispatch
	runDispatch(year, hour_of_year, step);
}

bool dispatch_manual_t::check_constraints(double &I, size_t count)
{
	// check common constraints before checking manual dispatch specific ones
	bool iterate = dispatch_t::check_constraints(I, count);


	if (!iterate)
	{
		double I_initial = I;
		iterate = true;

		// Don't let PV export to grid if can still charge battery (increase charging)
		if (m_batteryPower->powerPVToGrid > low_tolerance &&
			m_batteryPower->canPVCharge &&					// only do if battery is allowed to charge
			_Battery->battery_soc() < m_batteryPower->stateOfChargeMax - 1.0 &&		// and battery SOC is less than max
			fabs(I) < fabs(m_batteryPower->currentChargeMax) &&						// and battery current is less than max charge current
			fabs(m_batteryPower->powerBatteryDC) < (m_batteryPower->powerBatteryChargeMaxDC - 1.0) &&// and battery power is less than max charge power
			I <= 0)											// and battery was not discharging
		{
			double dI = 0;
			if (fabs(m_batteryPower->powerBatteryDC) < tolerance)
				dI = (m_batteryPower->powerPVToGrid  * util::kilowatt_to_watt / _Battery->battery_voltage());
			else
				dI = (m_batteryPower->powerPVToGrid  / fabs(m_batteryPower->powerBatteryAC)) *fabs(I);

			// Main problem will be that this tends to overcharge battery maximum SOC, so check
			double dQ = 0.01 * (m_batteryPower->stateOfChargeMax - _Battery->battery_soc()) * _Battery->battery_charge_maximum();

			I -= fmin(dI, dQ / _dt_hour);
		}
		// Don't let PV serve battery before load (decrease charging)
		else if (m_batteryPower->meterPosition == dispatch_t::BEHIND && I < 0 && m_batteryPower->powerGridToLoad > tolerance &&
			m_batteryPower->powerPVToBattery > 0) {

			double dP = m_batteryPower->powerGridToLoad;
			if (dP > m_batteryPower->powerPVToBattery) {
				dP = m_batteryPower->powerPVToBattery;
			}

			double dI = 0;
			if (dP < tolerance)
				dI = dP / _Battery->battery_voltage();
			else
				dI = (dP / fabs(m_batteryPower->powerBatteryAC)) *fabs(I);

			I += dI;
		}
		// Don't let battery export to the grid if behind the meter
		else if (m_batteryPower->meterPosition == dispatch_t::BEHIND && I > 0 && m_batteryPower->powerBatteryToGrid > tolerance)
		{
			if (fabs(m_batteryPower->powerBatteryAC) < tolerance)
				I -= (m_batteryPower->powerBatteryToGrid * util::kilowatt_to_watt / _Battery->battery_voltage());
			else
				I -= (m_batteryPower->powerBatteryToGrid / fabs(m_batteryPower->powerBatteryAC)) * fabs(I);
		}
		else
			iterate = false;

		// don't allow any changes to violate current limits
		bool current_iterate = restrict_current(I);

		// don't allow any changes to violate power limites
		bool power_iterate = restrict_power(I);

		// iterate if any of the conditions are met
		if (iterate || current_iterate || power_iterate)
			iterate = true;

		// stop iterating after 5 tries
		if (count > battery_dispatch::constraintCount)
			iterate = false;

		// don't allow battery to flip from charging to discharging or vice versa
		if ((I_initial / I) < 0)
			I = 0;

		// reset
		if (iterate)
		{
			_Battery->copy(_Battery_initial);
			m_batteryPower->powerBatteryAC = 0;
			m_batteryPower->powerGridToBattery = 0;
			m_batteryPower->powerBatteryToGrid = 0;
			m_batteryPower->powerPVToGrid  = 0;
		}
	}
	return iterate;
}

void dispatch_manual_t::SOC_controller()
{
	// Implement minimum SOC cut-off
	if (m_batteryPower->powerBatteryDC > 0)
	{
		_charging = false;

		if (m_batteryPower->powerBatteryDC*_dt_hour > _e_max)
			m_batteryPower->powerBatteryDC = _e_max / _dt_hour;

		//  discharge percent
		double e_percent = _e_max*_percent_discharge*0.01;

		if (m_batteryPower->powerBatteryDC*_dt_hour > e_percent)
			m_batteryPower->powerBatteryDC = e_percent / _dt_hour;
	}
	// Maximum SOC cut-off
	else if (m_batteryPower->powerBatteryDC < 0)
	{
		_charging = true;

		if (m_batteryPower->powerBatteryDC*_dt_hour < -_e_max)
			m_batteryPower->powerBatteryDC = -_e_max / _dt_hour;

		//  charge percent for automated grid charging
		double e_percent = _e_max*_percent_charge*0.01;

		if (fabs(m_batteryPower->powerBatteryDC) > fabs(e_percent) / _dt_hour)
			m_batteryPower->powerBatteryDC = -e_percent / _dt_hour;
	}
	else
		_charging = _prev_charging;
}

dispatch_automatic_t::dispatch_automatic_t(
	battery_t * Battery,
	double dt_hour,
	double SOC_min,
	double SOC_max,
	int current_choice,
	double Ic_max,
	double Id_max,
	double Pc_max_kwdc,
	double Pd_max_kwdc,
	double Pc_max_kwac,
	double Pd_max_kwac,
	double t_min,
	int dispatch_mode,
	int pv_dispatch,
	size_t nyears,
	size_t look_ahead_hours,
	double dispatch_update_frequency_hours,
	bool can_charge,
	bool can_clip_charge,
	bool can_grid_charge,
	bool can_fuelcell_charge
	) : dispatch_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,

	t_min, dispatch_mode, pv_dispatch)
{

	_dt_hour = dt_hour;
	_dt_hour_update = dispatch_update_frequency_hours;

	_hour_last_updated = SIZE_MAX;

	_look_ahead_hours = look_ahead_hours;
	_steps_per_hour = (size_t)(1. / dt_hour);
	_num_steps = 24 * _steps_per_hour;

	_day_index = 0;
	_month = 1;
	_nyears = nyears;

	_mode = dispatch_mode;
	_safety_factor = 0.03;

	m_batteryPower->canClipCharge = can_clip_charge;
	m_batteryPower->canPVCharge = can_charge;
	m_batteryPower->canGridCharge = can_grid_charge;
	m_batteryPower->canFuelCellCharge = can_fuelcell_charge;
	m_batteryPower->canDischarge = true;
}

void dispatch_automatic_t::init_with_pointer(const dispatch_automatic_t * tmp)
{
	_day_index = tmp->_day_index;
	_month = tmp->_month;
	_num_steps = tmp->_num_steps;
	_hour_last_updated = tmp->_hour_last_updated;
	_dt_hour = tmp->_dt_hour;
	_dt_hour_update = tmp->_dt_hour_update;
	_steps_per_hour = tmp->_steps_per_hour;
	_nyears = tmp->_nyears;
	_mode = tmp->_mode;
	_safety_factor = tmp->_safety_factor;
	_look_ahead_hours = tmp->_look_ahead_hours;
}

// deep copy from dispatch to this
dispatch_automatic_t::dispatch_automatic_t(const dispatch_t & dispatch) :
dispatch_t(dispatch)
{
	const dispatch_automatic_t * tmp = dynamic_cast<const dispatch_automatic_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_automatic_t::copy(const dispatch_t * dispatch)
{
	dispatch_t::copy(dispatch);
	const dispatch_automatic_t * tmp = dynamic_cast<const dispatch_automatic_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_automatic_t::update_pv_data(std::vector<double> P_pv_dc){ _P_pv_dc = P_pv_dc;}
void dispatch_automatic_t::set_custom_dispatch(std::vector<double> P_batt_dc) { _P_battery_use = P_batt_dc; }
int dispatch_automatic_t::get_mode(){ return _mode; }
double dispatch_automatic_t::power_batt_target() { return m_batteryPower->powerBatteryTarget; }

void dispatch_automatic_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	runDispatch(year, hour_of_year, step);
}


bool dispatch_automatic_t::check_constraints(double &I, size_t count)
{
    // check common constraints before checking automatic dispatch specific ones
	bool iterate = dispatch_t::check_constraints(I, count);

	if (!iterate)
	{
		double I_initial = I;
		double P_battery = I * _Battery->battery_voltage() * util::watt_to_kilowatt;
		double P_target = m_batteryPower->powerBatteryTarget;

		// Common to automated behind the meter and front of meter
		iterate = true;


		// Don't respect target if bidirectional inverter efficiency is low while charging
		if (m_batteryPower->connectionMode == dispatch_t::DC_CONNECTED &&
			m_batteryPower->sharedInverter->efficiencyAC <= m_batteryPower->inverterEfficiencyCutoff &&
			P_target < 0)
		{
			iterate = false;
			//double dP = fabs(P_battery) - m_batteryPower->powerPVToBattery;
			//I += dP * util::kilowatt_to_watt / _Battery->battery_voltage();
			//m_batteryPower->powerBatteryTarget += dP;
		}

		// Try and force controller to meet target or custom dispatch
		else if (P_battery > P_target + tolerance || P_battery < P_target - tolerance)
		{
			// Difference between the dispatch and the desired dispatch
			double dP = P_battery - m_batteryPower->powerBatteryTarget;
			double SOC = _Battery->battery_soc();

			// Case 1: Charging, need to increase charging to meet target (P_battery < 0, dP > 0)
			if (P_battery <= 0 && dP > 0) {
				// Don't charge more if can't grid charge (assumes PV and fuel cell have met max possible)
				if (!m_batteryPower->canGridCharge) {
					iterate = false;
				}
				// Don't charge more if battery is already close to full
				if (SOC > m_batteryPower->stateOfChargeMax - tolerance) {
					iterate = false;
				}
				// Don't charge more if would violate current or power charge limits
				if (I > m_batteryPower->currentChargeMax - tolerance ||
					fabs(P_battery) > m_batteryPower->powerBatteryChargeMaxDC - tolerance ||
					fabs(m_batteryPower->powerBatteryAC) > m_batteryPower->powerBatteryChargeMaxAC - tolerance){
					iterate = false;
				}
				// restrict based on power limits
				else {
					double dP_max = fmin(fmin(dP, m_batteryPower->powerBatteryChargeMaxDC - fabs(P_battery)),
						m_batteryPower->powerBatteryChargeMaxAC - fabs(m_batteryPower->powerBatteryAC));
					dP = fmax(dP_max, 0);
				}
			}
			// Case 2: Discharging, need to increase discharge to meet target (P_battery > 0, dP < 0)
			else if (P_battery > 0 && dP < 0) {
				// Don't discharge more if already near min SOC
				if (SOC < m_batteryPower->stateOfChargeMin + tolerance) {
					iterate = false;
				}
				// Don't discharge more if would violate current or power discharge limits
				if (I > m_batteryPower->currentDischargeMax - tolerance ||
					P_battery > m_batteryPower->powerBatteryDischargeMaxDC - tolerance ||
					m_batteryPower->powerBatteryAC > m_batteryPower->powerBatteryDischargeMaxAC - tolerance) {
					iterate = false;
				}
				// restrict based on power limits
				else {
					double dP_max = fmax(fmax(dP, P_battery - m_batteryPower->powerBatteryDischargeMaxDC),
						m_batteryPower->powerBatteryAC - m_batteryPower->powerBatteryChargeMaxAC);
					dP = fmin(dP_max, 0);
				}
			}
			// Otherwise safe, (decreasing charging, decreasing discharging)
			double dQ = dP * _dt_hour * util::kilowatt_to_watt / _Battery->battery_voltage();
			double dSOC = 100 * dQ / _Battery->battery_charge_maximum();

			if (iterate) {

				double dI = dP * util::kilowatt_to_watt / _Battery->battery_voltage();
				if (SOC + dSOC > m_batteryPower->stateOfChargeMax + tolerance) {
					double dSOC_use = (m_batteryPower->stateOfChargeMax - SOC);
					double dQ_use = dSOC_use * 0.01 * _Battery->battery_charge_maximum();
					dI = dQ_use / _dt_hour;
				}
				else if (SOC + dSOC < m_batteryPower->stateOfChargeMin - tolerance) {
					double dSOC_use = (m_batteryPower->stateOfChargeMin - SOC);
					double dQ_use = dSOC_use * 0.01 * _Battery->battery_charge_maximum();
					dI = dQ_use / _dt_hour;
				}
				I -= dI;
			}
		}
		// Behind the meter
		else if (m_batteryPower->meterPosition == dispatch_t::BEHIND)
		{
			// Don't let PV export to grid if can still charge battery (increase charging) (unless following custom dispatch)
			if (_mode != dispatch_t::CUSTOM_DISPATCH && m_batteryPower->powerPVToGrid  > tolerance && m_batteryPower->canPVCharge && _Battery->battery_soc() < m_batteryPower->stateOfChargeMax - tolerance && fabs(I) < fabs(m_batteryPower->currentChargeMax))
			{
				if (fabs(m_batteryPower->powerBatteryAC) < tolerance)
					I -= (m_batteryPower->powerPVToGrid  * util::kilowatt_to_watt / _Battery->battery_voltage());
				else
					I -= (m_batteryPower->powerPVToGrid  / fabs(m_batteryPower->powerBatteryAC)) *fabs(I);
			}
			// Don't let battery export to the grid if behind the meter
			else if (m_batteryPower->powerBatteryToGrid > tolerance)
			{
				if (fabs(m_batteryPower->powerBatteryAC) < tolerance)
					I -= (m_batteryPower->powerBatteryToGrid * util::kilowatt_to_watt / _Battery->battery_voltage());
				else
					I -= (m_batteryPower->powerBatteryToGrid / fabs(m_batteryPower->powerBatteryAC)) * fabs(I);
			}
			else
				iterate = false;
		}
		else
			iterate = false;

		// don't allow any changes to violate current limits
		bool current_iterate = restrict_current(I);

		// don't allow any changes to violate power limites
		bool power_iterate = restrict_power(I);

		// iterate if any of the conditions are met
		if (iterate || current_iterate || power_iterate)
			iterate = true;

		// stop iterating after n tries
		if (count > battery_dispatch::constraintCount)
			iterate = false;

		// don't allow battery to flip from charging to discharging or vice versa
		if ((I_initial / I) < 0)
			I = 0;

		// reset
		if (iterate)
		{
			_Battery->copy(_Battery_initial);
//			m_batteryPower->powerBatteryAC = 0;
//			m_batteryPower->powerGridToBattery = 0;
//			m_batteryPower->powerBatteryToGrid = 0;
//			m_batteryPower->powerPVToGrid  = 0;
		}
	}
	return iterate;
}

dispatch_automatic_behind_the_meter_t::dispatch_automatic_behind_the_meter_t(
	battery_t * Battery,
	double dt_hour,
	double SOC_min,
	double SOC_max,
	int current_choice,
	double Ic_max,
	double Id_max,
	double Pc_max_kwdc,
	double Pd_max_kwdc,
	double Pc_max_kwac,
	double Pd_max_kwac,
	double t_min,
	int dispatch_mode,
	int pv_dispatch,
	size_t nyears,
	size_t look_ahead_hours,
	double dispatch_update_frequency_hours,
	bool can_charge,
	bool can_clip_charge,
	bool can_grid_charge,
	bool can_fuelcell_charge
	) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge)
{
	_P_target_month = -1e16;
	_P_target_current = -1e16;
	_P_target_use.reserve(_num_steps);
	_P_battery_use.reserve(_num_steps);

	grid.reserve(_num_steps);
	sorted_grid.reserve(_num_steps);

	for (size_t ii = 0; ii != _num_steps; ii++)
	{
		grid.push_back(grid_point(0., 0, 0));
		sorted_grid.push_back(grid[ii]);
	}
}

void dispatch_automatic_behind_the_meter_t::init_with_pointer(const dispatch_automatic_behind_the_meter_t* tmp)
{
	_P_target_input = tmp->_P_target_input;
	_P_target_month = tmp->_P_target_month;
	_P_target_current = tmp->_P_target_current;
	grid = tmp->grid;

	// time series data which could be slow to copy. Since this doesn't change, should probably make const and have copy point to common memory
	_P_load_dc = tmp->_P_load_dc;
	_P_target_use = tmp->_P_target_use;
	sorted_grid = tmp->sorted_grid;
}

// deep copy from dispatch to this
dispatch_automatic_behind_the_meter_t::dispatch_automatic_behind_the_meter_t(const dispatch_t & dispatch) :
dispatch_automatic_t(dispatch)
{
	const dispatch_automatic_behind_the_meter_t * tmp = dynamic_cast<const dispatch_automatic_behind_the_meter_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_automatic_behind_the_meter_t::copy(const dispatch_t * dispatch)
{
	dispatch_automatic_t::copy(dispatch);
	const dispatch_automatic_behind_the_meter_t * tmp = dynamic_cast<const dispatch_automatic_behind_the_meter_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_automatic_behind_the_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
}

void dispatch_automatic_behind_the_meter_t::update_load_data(std::vector<double> P_load_dc){ _P_load_dc = P_load_dc; }
void dispatch_automatic_behind_the_meter_t::set_target_power(std::vector<double> P_target){ _P_target_input = P_target; }
double dispatch_automatic_behind_the_meter_t::power_grid_target() { return _P_target_current; };
void dispatch_automatic_behind_the_meter_t::update_dispatch(size_t hour_of_year, size_t step, size_t idx)
{
	bool debug = false;
	FILE *p;
	check_debug(p, debug, hour_of_year, idx);
	size_t hour_of_day = util::hour_of_day(hour_of_year);
	_day_index = (hour_of_day * _steps_per_hour + step);

	if (_mode != dispatch_t::CUSTOM_DISPATCH)
	{
		// Currently hardcoded to have 24 hour look ahead and 24 dispatch_update
		if (hour_of_day == 0 && hour_of_year != _hour_last_updated)
		{

			// [kWh] - the maximum energy that can be cycled
			double E_max = 0;

			check_new_month(hour_of_year, step);

			// setup vectors
			initialize(hour_of_year);

			// compute grid power, sort highest to lowest
			sort_grid(p, debug, idx);

			// Peak shaving scheme
			compute_energy(p, debug, E_max);
			target_power(p, debug, E_max, idx);

			// Set battery power profile
			set_battery_power(p, debug);
		}
		// save for extraction
		_P_target_current = _P_target_use[_day_index];
		m_batteryPower->powerBatteryTarget = _P_battery_use[_day_index];
	}
	else
	{
		m_batteryPower->powerBatteryTarget = _P_battery_use[idx % (8760 *_steps_per_hour)];
        if (m_batteryPower->connectionMode == AC_CONNECTED){
            if (m_batteryPower->powerBatteryTarget < 0)
                m_batteryPower->powerBatteryTarget *= m_batteryPower->singlePointEfficiencyDCToAC;
            else
                m_batteryPower->powerBatteryTarget /= m_batteryPower->singlePointEfficiencyDCToAC;
        }
	}

	m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;

	if (debug)
		fclose(p);
}
void dispatch_automatic_behind_the_meter_t::initialize(size_t hour_of_year)
{
	_hour_last_updated = hour_of_year;
	_P_target_use.clear();
	_P_battery_use.clear();
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;

	// clean up vectors
	for (size_t ii = 0; ii != _num_steps; ii++)
	{
		grid[ii] = grid_point(0., 0, 0);
		sorted_grid[ii] = grid[ii];
		_P_target_use.push_back(0.);
		_P_battery_use.push_back(0.);
	}
}
void dispatch_automatic_behind_the_meter_t::check_new_month(size_t hour_of_year, size_t step)
{
	size_t hours = 0;
	for (size_t month = 1; month <= _month; month++)
		hours += util::hours_in_month(month);

	if (hours == 8760)
		hours = 0;

	if ((hour_of_year == hours) && step == 0)
	{
		_P_target_month = -1e16;
		_month < 12 ? _month++ : _month = 1;
	}
}
void dispatch_automatic_behind_the_meter_t::check_debug(FILE *&p, bool & debug, size_t hour_of_year, size_t)
{
	// for now, don't enable
	// debug = true;

	if (hour_of_year == 0 && hour_of_year != _hour_last_updated)
	{
		// debug = true;
		if (debug)
		{
			p = fopen("dispatch.txt", "w");
			fprintf(p, "Hour of Year: %zu\t Hour Last Updated: %zu \t Steps per Hour: %zu\n", hour_of_year, _hour_last_updated, _steps_per_hour);
		}
		// failed for some reason
		if (p == NULL)
			debug = false;
	}
}

void dispatch_automatic_behind_the_meter_t::sort_grid(FILE *p, bool debug, size_t idx)
{

	if (debug)
		fprintf(p, "Index\t P_load (kW)\t P_pv (kW)\t P_grid (kW)\n");

	// compute grid net from pv and load (no battery)
	size_t count = 0;
	for (size_t hour = 0; hour != 24; hour++)
	{
		for (size_t step = 0; step != _steps_per_hour; step++)
		{
			grid[count] = grid_point(_P_load_dc[idx] - _P_pv_dc[idx], hour, step);
			sorted_grid[count] = grid[count];

			if (debug)
				fprintf(p, "%zu\t %.1f\t %.1f\t %.1f\n", count, _P_load_dc[idx], _P_pv_dc[idx], _P_load_dc[idx] - _P_pv_dc[idx]);

			idx++;
			count++;
		}
	}
	std::sort(sorted_grid.begin(), sorted_grid.end(), byGrid());
}

void dispatch_automatic_behind_the_meter_t::compute_energy(FILE *p, bool debug, double & E_max)
{

	E_max = _Battery->battery_voltage() *_Battery->battery_charge_maximum()*(m_batteryPower->stateOfChargeMax - m_batteryPower->stateOfChargeMin) *0.01 *util::watt_to_kilowatt;

	if (debug)
	{
		fprintf(p, "Energy Max: %.3f\t", E_max);
		fprintf(p, "Battery Voltage: %.3f\n", _Battery->battery_voltage());
	}
}

void dispatch_automatic_behind_the_meter_t::target_power(FILE*p, bool debug, double E_useful, size_t idx)
{
	// if target power set, use that
	if (_P_target_input.size() > idx && _P_target_input[idx] >= 0)
	{
		double_vec::const_iterator first = _P_target_input.begin() + idx;
		double_vec::const_iterator last = _P_target_input.begin() + idx + _num_steps;
		double_vec tmp(first, last);
		_P_target_use = tmp;
		return;
	}
	// don't calculate if peak grid demand is less than a previous target in the month
	else if (sorted_grid[0].Grid() < _P_target_month)
	{
		for (size_t i = 0; i != _num_steps; i++)
			_P_target_use[i] = _P_target_month;
		return;
	}
	// otherwise, compute one target for the next 24 hours.
	else
	{
		// First compute target power which will allow battery to charge up to E_useful over 24 hour period
		if (debug)
			fprintf(p, "Index\tRecharge_target\t charge_energy\n");

		double P_target = sorted_grid[0].Grid();
		double P_target_min = 1e16;
		double E_charge = 0.;
		int index = (int)_num_steps - 1;
		std::vector<double> E_charge_vec;
		for (int jj = (int)_num_steps - 1; jj >= 0; jj--)
		{
			E_charge = 0.;
			P_target_min = sorted_grid[index].Grid();

			for (int ii = (int)_num_steps - 1; ii >= 0; ii--)
			{
				if (sorted_grid[ii].Grid() > P_target_min)
					break;

				E_charge += (P_target_min - sorted_grid[ii].Grid())*_dt_hour;
			}
			E_charge_vec.push_back(E_charge);
			if (debug)
				fprintf(p, "%u: index\t%.3f\t %.3f\n", index, P_target_min, E_charge);
			index--;

			if (index < 0)
				break;
		}
		std::reverse(E_charge_vec.begin(), E_charge_vec.end());

		// Calculate target power
		std::vector<double> sorted_grid_diff;
		sorted_grid_diff.reserve(_num_steps - 1);

		for (size_t ii = 0; ii != _num_steps - 1; ii++)
			sorted_grid_diff.push_back(sorted_grid[ii].Grid() - sorted_grid[ii + 1].Grid());

		P_target = sorted_grid[0].Grid(); // target power to shave to [kW]
		double sum = 0;			   // energy [kWh];
		if (debug)
			fprintf(p, "Step\tTarget_Power\tEnergy_Sum\tEnergy_charged\n");

		for (size_t ii = 0; ii != _num_steps - 1; ii++)
		{
			// don't look at negative grid power
			if (sorted_grid[ii + 1].Grid() < 0)
				break;
			// Update power target
			else
				P_target = sorted_grid[ii + 1].Grid();

			if (debug)
				fprintf(p, "%zu\t %.3f\t", ii, P_target);

			// implies a repeated power
			if (sorted_grid_diff[ii] == 0)
			{
				if (debug)
					fprintf(p, "\n");
				continue;
			}
			// add to energy we are trimming
			else
				sum += sorted_grid_diff[ii] * (ii + 1)*_dt_hour;

			if (debug)
				fprintf(p, "%.3f\t%.3f\n", sum, E_charge_vec[ii + 1]);

			if (sum < E_charge_vec[ii + 1] && sum < E_useful)
				continue;
			// we have limited power, we'll shave what more we can
			else if (sum > E_charge_vec[ii + 1])
			{
				P_target += (sum - E_charge_vec[ii]) / ((ii + 1)*_dt_hour);
				sum = E_charge_vec[ii];
				if (debug)
					fprintf(p, "%zu\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
			// only allow one cycle per day
			else if (sum > E_useful)
			{
				P_target += (sum - E_useful) / ((ii + 1)*_dt_hour);
				sum = E_useful;
				if (debug)
					fprintf(p, "%zu\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
		}
		// set safety factor in case voltage differences make it impossible to achieve target without violated minimum SOC
		P_target *= (1 + _safety_factor);

		// don't set target lower than previous high in month
		if (P_target < _P_target_month)
		{
			P_target = _P_target_month;
			if (debug)
				fprintf(p, "P_target exceeds monthly target, move to  %.3f\n", P_target);
		}
		else
			_P_target_month = P_target;

		// write vector of targets
		for (size_t i = 0; i != _num_steps; i++)
			_P_target_use[i] = P_target;
	}
}

void dispatch_automatic_behind_the_meter_t::set_battery_power(FILE *p, bool debug)
{
	for (size_t i = 0; i != _P_target_use.size(); i++) {
		_P_battery_use[i] = grid[i].Grid() - _P_target_use[i];

		// At this point the target power is expressed in AC, must convert to DC for battery
		if (m_batteryPower->connectionMode == m_batteryPower->AC_CONNECTED) {
			if (_P_battery_use[i] > 0) {
				_P_battery_use[i] /= m_batteryPower->singlePointEfficiencyDCToAC;
			}
			else {
				_P_battery_use[i] *= m_batteryPower->singlePointEfficiencyACToDC;
			}
		}
		// DC-connected is harder to convert to AC, must make assumptions about inverter efficiency and charge shource
		else {
			if (_P_battery_use[i] > 0) {
				_P_battery_use[i] /= (m_batteryPower->singlePointEfficiencyDCToDC * m_batteryPower->singlePointEfficiencyACToDC);
			}
			// Assuming just charging from PV not grid
			else {
				_P_battery_use[i] *= m_batteryPower->singlePointEfficiencyDCToDC;
			}
		}
	}

	if (debug)
	{
		for (size_t i = 0; i != _P_target_use.size(); i++)
			fprintf(p, "i=%zu  P_battery: %.2f\n", i, _P_battery_use[i]);
	}
}

dispatch_automatic_front_of_meter_t::dispatch_automatic_front_of_meter_t(
	battery_t * Battery,
	double dt_hour,
	double SOC_min,
	double SOC_max,
	int current_choice,
	double Ic_max,
	double Id_max,
	double Pc_max_kwdc,
	double Pd_max_kwdc,
	double Pc_max_kwac,
	double Pd_max_kwac,
	double t_min,
	int dispatch_mode,
	int pv_dispatch,
	size_t nyears,
	size_t look_ahead_hours,
	double dispatch_update_frequency_hours,
	bool can_charge,
	bool can_clip_charge,
	bool can_grid_charge,
	bool can_fuelcell_charge,
	double inverter_paco,
	double batt_cost_per_kwh,
	int battCycleCostChoice,
	double battCycleCost,
	std::vector<double> forecast_price_series_dollar_per_kwh,
	UtilityRate * utilityRate,
	double etaPVCharge,
	double etaGridCharge,
	double etaDischarge) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge)
{
	// if look behind, only allow 24 hours
	if (_mode == dispatch_t::FOM_LOOK_BEHIND)
		_look_ahead_hours = 24;

	_inverter_paco = inverter_paco;
	_forecast_price_rt_series = forecast_price_series_dollar_per_kwh;

	// only create utility rate calculator if utility rate is defined
	if (utilityRate) {
		std::unique_ptr<UtilityRateCalculator> tmp(new UtilityRateCalculator(utilityRate, _steps_per_hour));
		m_utilityRateCalculator = std::move(tmp);
	}

	m_battReplacementCostPerKWH = batt_cost_per_kwh;
	m_etaPVCharge = etaPVCharge * 0.01;
	m_etaGridCharge = etaGridCharge * 0.01;
	m_etaDischarge = etaDischarge * 0.01;

	m_battCycleCostChoice = battCycleCostChoice;
	m_cycleCost = 0.05;
	if (battCycleCostChoice == dispatch_t::INPUT_CYCLE_COST) {
		m_cycleCost = battCycleCost;
	}

	revenueToClipCharge = revenueToDischarge = revenueToGridCharge = revenueToPVCharge = 0;

	setup_cost_forecast_vector();
}
dispatch_automatic_front_of_meter_t::~dispatch_automatic_front_of_meter_t(){ /* NOTHING TO DO */}
void dispatch_automatic_front_of_meter_t::init_with_pointer(const dispatch_automatic_front_of_meter_t* tmp)
{
	_look_ahead_hours = tmp->_look_ahead_hours;
	_inverter_paco = tmp->_inverter_paco;
	_forecast_price_rt_series = tmp->_forecast_price_rt_series;

	m_battReplacementCostPerKWH = tmp->m_battReplacementCostPerKWH;
	m_etaPVCharge = tmp->m_etaPVCharge;
	m_etaGridCharge = tmp->m_etaGridCharge;
	m_etaDischarge = tmp->m_etaDischarge;
}

void dispatch_automatic_front_of_meter_t::setup_cost_forecast_vector()
{
	std::vector<double> ppa_price_series;
	ppa_price_series.reserve(_forecast_price_rt_series.size());

	// add elements at beginning, so our forecast is looking at yesterday's prices
	if (_mode == dispatch_t::FOM_LOOK_BEHIND) {
		for (size_t i = 0; i != _look_ahead_hours * _steps_per_hour; i++)
			ppa_price_series.push_back(0);
	}

	// add elements at the end, so we have forecast information at end of year
	for (size_t i = 0; i != _forecast_price_rt_series.size(); i++){
		ppa_price_series.push_back(_forecast_price_rt_series[i]);
	}
	for (size_t i = 0; i != _look_ahead_hours * _steps_per_hour; i++) {
		ppa_price_series.push_back(_forecast_price_rt_series[i]);
	}
	_forecast_price_rt_series = ppa_price_series;
}

// deep copy from dispatch to this
dispatch_automatic_front_of_meter_t::dispatch_automatic_front_of_meter_t(const dispatch_t & dispatch) :
dispatch_automatic_t(dispatch)
{
	const dispatch_automatic_front_of_meter_t * tmp = dynamic_cast<const dispatch_automatic_front_of_meter_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_automatic_front_of_meter_t::copy(const dispatch_t * dispatch)
{
	dispatch_automatic_t::copy(dispatch);
	const dispatch_automatic_front_of_meter_t * tmp = dynamic_cast<const dispatch_automatic_front_of_meter_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_automatic_front_of_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
}

void dispatch_automatic_front_of_meter_t::update_dispatch(size_t hour_of_year, size_t , size_t lifetimeIndex)
{
	// Initialize
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;


	if (_mode != dispatch_t::FOM_CUSTOM_DISPATCH)
	{

		// Power to charge (<0) or discharge (>0)
		double powerBattery = 0;

        /*! Cost to cycle the battery at all, using maximum DOD or user input */
        costToCycle();

			// Compute forecast variables which don't change from year to year
			size_t idx_year1 = hour_of_year * _steps_per_hour;
			size_t idx_lookahead = _look_ahead_hours * _steps_per_hour;
			auto max_ppa_cost = std::max_element(_forecast_price_rt_series.begin() + idx_year1, _forecast_price_rt_series.begin() + idx_year1 + idx_lookahead);
			auto min_ppa_cost = std::min_element(_forecast_price_rt_series.begin() + idx_year1, _forecast_price_rt_series.begin() + idx_year1 + idx_lookahead);
			double ppa_cost = _forecast_price_rt_series[idx_year1];

			/*! Cost to purchase electricity from the utility */
			double usage_cost = ppa_cost;
			std::vector<double> usage_cost_forecast;
			if (m_utilityRateCalculator) {
				usage_cost = m_utilityRateCalculator->getEnergyRate(hour_of_year);
				for (size_t i = hour_of_year; i < hour_of_year + _look_ahead_hours; i++)
				{
					for (size_t s = 0; s < _steps_per_hour; s++) {
						usage_cost_forecast.push_back(m_utilityRateCalculator->getEnergyRate(i % 8760));
					}
				}
			}

			// Compute forecast variables which potentially do change from year to year
			double energyToStoreClipped = 0;
			if (_P_cliploss_dc.size() > lifetimeIndex + _look_ahead_hours) {
				energyToStoreClipped = std::accumulate(_P_cliploss_dc.begin() + lifetimeIndex, _P_cliploss_dc.begin() + lifetimeIndex + _look_ahead_hours * _steps_per_hour, 0.0) * _dt_hour;
			}

			/*! Economic benefit of charging from the grid in current time step to discharge sometime in next X hours ($/kWh)*/
			revenueToGridCharge = *max_ppa_cost * m_etaDischarge - usage_cost / m_etaGridCharge - m_cycleCost;

			/*! Computed revenue to charge from Grid in each of next X hours ($/kWh)*/
			double revenueToGridChargeMax = 0;
			if (m_batteryPower->canGridCharge) {
				std::vector<double> revenueToGridChargeForecast;
				size_t j = 0;
				for (size_t i = idx_year1; i < idx_year1 + idx_lookahead; i++) {
					if (m_utilityRateCalculator) {
						revenueToGridChargeForecast.push_back(*max_ppa_cost * m_etaDischarge - usage_cost_forecast[j] / m_etaGridCharge - m_cycleCost);
					}
					else {
						revenueToGridChargeForecast.push_back(*max_ppa_cost * m_etaDischarge - _forecast_price_rt_series[i] / m_etaGridCharge - m_cycleCost);
					}
					j++;
				}
				revenueToGridChargeMax = *std::max_element(std::begin(revenueToGridChargeForecast), std::end(revenueToGridChargeForecast));
			}

			/*! Economic benefit of charging from regular PV in current time step to discharge sometime in next X hours ($/kWh)*/
			revenueToPVCharge = _P_pv_dc[idx_year1] > 0 ? *max_ppa_cost * m_etaDischarge - ppa_cost / m_etaPVCharge - m_cycleCost : 0;

			/*! Computed revenue to charge from PV in each of next X hours ($/kWh)*/
			size_t t_duration = static_cast<size_t>(ceilf(_Battery->battery_energy_nominal() / m_batteryPower->powerBatteryChargeMaxDC));
			size_t pv_hours_on;
			double revenueToPVChargeMax = 0;
			if (m_batteryPower->canPVCharge) {
				std::vector<double> revenueToPVChargeForecast;
				for (size_t i = idx_year1; i < idx_year1 + idx_lookahead; i++) {
					// when considering grid charging, require PV output to exceed battery input capacity before accepting as a better option
					bool system_on = _P_pv_dc[i] >= m_batteryPower->powerBatteryChargeMaxDC ? 1 : 0;
					if (system_on) {
						revenueToPVChargeForecast.push_back(system_on * (*max_ppa_cost * m_etaDischarge - _forecast_price_rt_series[i] / m_etaPVCharge - m_cycleCost));
					}
				}
				pv_hours_on = revenueToPVChargeForecast.size() / _steps_per_hour;
				revenueToPVChargeMax = pv_hours_on >= t_duration ? *std::max_element(std::begin(revenueToPVChargeForecast), std::end(revenueToPVChargeForecast)): 0;
			}

			/*! Economic benefit of charging from clipped PV in current time step to discharge sometime in the next X hours (clipped PV is free) ($/kWh) */
			revenueToClipCharge = *max_ppa_cost * m_etaDischarge - m_cycleCost;

			/*! Economic benefit of discharging in current time step ($/kWh) */
			revenueToDischarge = ppa_cost * m_etaDischarge - m_cycleCost;

			/*! Energy need to charge the battery (kWh) */
			double energyNeededToFillBattery = _Battery->battery_energy_to_fill(m_batteryPower->stateOfChargeMax);

			/* Booleans to assist decisions */
			bool highDischargeValuePeriod = ppa_cost == *max_ppa_cost;
			bool highChargeValuePeriod = ppa_cost == *min_ppa_cost;
			bool excessAcCapacity = _inverter_paco > m_batteryPower->powerPVThroughSharedInverter;
			bool batteryHasDischargeCapacity = _Battery->battery_soc() >= m_batteryPower->stateOfChargeMin + 1.0;

			// Always Charge if PV is clipping
			if (m_batteryPower->canClipCharge && m_batteryPower->powerPVClipped > 0 && revenueToClipCharge > 0)
			{
				powerBattery = -m_batteryPower->powerPVClipped;
			}

			// Increase charge from PV if it is more valuable later than selling now
			if (m_batteryPower->canPVCharge &&
				//revenueToPVCharge >= revenueToGridChargeMax &&
				revenueToPVCharge > 0 &&
				highChargeValuePeriod &&
				m_batteryPower->powerPV > 0)
			{
				// leave EnergyToStoreClipped capacity in battery
				if (m_batteryPower->canClipCharge)
				{
					if (energyToStoreClipped < energyNeededToFillBattery)
					{
						double energyCanCharge = (energyNeededToFillBattery - energyToStoreClipped);
						if (energyCanCharge <= m_batteryPower->powerPV * _dt_hour)
							powerBattery = -std::fmax(energyCanCharge / _dt_hour, m_batteryPower->powerPVClipped);
						else
							powerBattery = -std::fmax(m_batteryPower->powerPV, m_batteryPower->powerPVClipped);

						energyNeededToFillBattery = std::fmax(0, energyNeededToFillBattery + (powerBattery * _dt_hour));
					}

				}
				// otherwise, don't reserve capacity for clipping
				else {
					powerBattery = -m_batteryPower->powerPV;
				}
			}

			// Also charge from grid if it is valuable to do so, still leaving EnergyToStoreClipped capacity in battery
			if (m_batteryPower->canGridCharge &&
				revenueToGridCharge >= revenueToPVChargeMax &&
				revenueToGridCharge > 0 &&
				highChargeValuePeriod &&
				energyNeededToFillBattery > 0)
			{
				// leave EnergyToStoreClipped capacity in battery
				if (m_batteryPower->canClipCharge)
				{
					if (energyToStoreClipped < energyNeededToFillBattery)
					{
						double energyCanCharge = (energyNeededToFillBattery - energyToStoreClipped);
						powerBattery -= energyCanCharge / _dt_hour;
					}
				}
				else
					powerBattery = -energyNeededToFillBattery / _dt_hour;
			}

			// Discharge if we are in a high-price period and have battery and inverter capacity
			if (highDischargeValuePeriod && revenueToDischarge > 0 && excessAcCapacity && batteryHasDischargeCapacity) {
				if (m_batteryPower->connectionMode == BatteryPower::DC_CONNECTED) {
					powerBattery = _inverter_paco - m_batteryPower->powerPV;
				}
				else {
                powerBattery = _inverter_paco;
            }
        }
		// save for extraction
		m_batteryPower->powerBatteryTarget = powerBattery;
	}
	else
	{
		// extract input power by modifying lifetime index to year 1
		m_batteryPower->powerBatteryTarget = _P_battery_use[lifetimeIndex % (8760 * _steps_per_hour)];
        if (m_batteryPower->connectionMode == AC_CONNECTED){
            if (m_batteryPower->powerBatteryTarget < 0)
                m_batteryPower->powerBatteryTarget *= m_batteryPower->singlePointEfficiencyDCToAC;
            else
                m_batteryPower->powerBatteryTarget /= m_batteryPower->singlePointEfficiencyDCToAC;
        }
	}

	m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;
}

void dispatch_automatic_front_of_meter_t::update_cliploss_data(double_vec P_cliploss)
{
	_P_cliploss_dc = P_cliploss;

	// append to end to allow for look-ahead
	for (size_t i = 0; i != _look_ahead_hours * _steps_per_hour; i++)
		_P_cliploss_dc.push_back(P_cliploss[i]);
}

void dispatch_automatic_front_of_meter_t::update_pv_data(double_vec P_pv_dc)
{
	_P_pv_dc = P_pv_dc;

	// append to end to allow for look-ahead
	for (size_t i = 0; i != _look_ahead_hours * _steps_per_hour; i++)
		_P_pv_dc.push_back(P_pv_dc[i]);
}

void dispatch_automatic_front_of_meter_t::costToCycle()
{
	// Calculate assuming maximum depth of discharge (most conservative assumption)
	if (m_battCycleCostChoice == dispatch_t::MODEL_CYCLE_COST)
	{
		double capacityPercentDamagePerCycle = _Battery->lifetime_model()->cycleModel()->estimateCycleDamage();
		m_cycleCost = 0.01 * capacityPercentDamagePerCycle * m_battReplacementCostPerKWH;
	}
}

battery_metrics_t::battery_metrics_t(double dt_hour)
{
	_dt_hour = dt_hour;

	// single value metrics
	_e_charge_accumulated = 0;
	_e_charge_from_pv = 0.;
	_e_charge_from_grid = _e_charge_accumulated; // assumes initial charge from grid
	_e_discharge_accumulated = 0.;
	_e_loss_system = 0.;
	_average_efficiency = 100.;
	_average_roundtrip_efficiency = 100.;
	_pv_charge_percent = 0.;

	// annual metrics
	_e_charge_from_pv_annual = 0.;
	_e_charge_from_grid_annual = _e_charge_from_grid;
	_e_charge_annual = _e_charge_accumulated;
	_e_discharge_annual = 0.;
	_e_loss_system_annual = _e_loss_system;
	_e_grid_import_annual = 0.;
	_e_grid_export_annual = 0.;
	_e_loss_annual = 0.;
}
double battery_metrics_t::average_battery_conversion_efficiency(){ return _average_efficiency; }
double battery_metrics_t::average_battery_roundtrip_efficiency(){ return _average_roundtrip_efficiency; }
double battery_metrics_t::pv_charge_percent(){ return _pv_charge_percent; }
double battery_metrics_t::energy_pv_charge_annual(){ return _e_charge_from_pv_annual; }
double battery_metrics_t::energy_grid_charge_annual(){ return _e_charge_from_grid_annual; }
double battery_metrics_t::energy_charge_annual(){ return _e_charge_annual; }
double battery_metrics_t::energy_discharge_annual(){ return _e_discharge_annual; }
double battery_metrics_t::energy_grid_import_annual(){ return _e_grid_import_annual; }
double battery_metrics_t::energy_grid_export_annual(){ return _e_grid_export_annual; }
double battery_metrics_t::energy_loss_annual(){ return _e_loss_annual; }
double battery_metrics_t::energy_system_loss_annual(){ return _e_loss_system_annual; };

void battery_metrics_t::compute_metrics_ac(const BatteryPower * batteryPower)
{
	accumulate_grid_annual(batteryPower->powerGrid);
	accumulate_battery_charge_components(batteryPower->powerBatteryAC, batteryPower->powerPVToBattery, batteryPower->powerGridToBattery);
	accumulate_energy_charge(batteryPower->powerBatteryAC);
	accumulate_energy_discharge(batteryPower->powerBatteryAC);
	accumulate_energy_system_loss(batteryPower->powerSystemLoss);
	compute_annual_loss();
}
void battery_metrics_t::compute_annual_loss()
{
	double e_conversion_loss = 0.;
	if (_e_charge_annual > _e_discharge_annual)
		e_conversion_loss = _e_charge_annual - _e_discharge_annual;
	_e_loss_annual = e_conversion_loss + _e_loss_system_annual;
}
void battery_metrics_t::accumulate_energy_charge(double P_tofrom_batt)
{
	if (P_tofrom_batt < 0.)
	{
		_e_charge_accumulated += (-P_tofrom_batt)*_dt_hour;
		_e_charge_annual += (-P_tofrom_batt)*_dt_hour;
	}
}
void battery_metrics_t::accumulate_energy_discharge(double P_tofrom_batt)
{
	if (P_tofrom_batt > 0.)
	{
		_e_discharge_accumulated += P_tofrom_batt*_dt_hour;
		_e_discharge_annual += P_tofrom_batt*_dt_hour;
	}
}
void battery_metrics_t::accumulate_energy_system_loss(double P_system_loss)
{
	_e_loss_system += P_system_loss * _dt_hour;
	_e_loss_system_annual += P_system_loss * _dt_hour;
}
void battery_metrics_t::accumulate_battery_charge_components(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt)
{
	if (P_tofrom_batt < 0.)
	{
		_e_charge_from_pv += P_pv_to_batt * _dt_hour;
		_e_charge_from_pv_annual += P_pv_to_batt * _dt_hour;
		_e_charge_from_grid += P_grid_to_batt * _dt_hour;
		_e_charge_from_grid_annual += P_grid_to_batt * _dt_hour;
	}
	_average_efficiency = 100.*(_e_discharge_accumulated / _e_charge_accumulated);
	_average_roundtrip_efficiency = 100.*(_e_discharge_accumulated / (_e_charge_accumulated + _e_loss_system));
	_pv_charge_percent = 100.*(_e_charge_from_pv / _e_charge_accumulated);
}
void battery_metrics_t::accumulate_grid_annual(double P_tofrom_grid)
{
	// e_grid > 0 (export to grid)
	// e_grid < 0 (import from grid)

	if (P_tofrom_grid > 0)
		_e_grid_export_annual += P_tofrom_grid*_dt_hour;
	else
		_e_grid_import_annual += (-P_tofrom_grid)*_dt_hour;
}

void battery_metrics_t::new_year()
{
	_e_charge_from_pv_annual = 0.;
	_e_charge_from_grid_annual = 0;
	_e_charge_annual = 0.;
	_e_discharge_annual = 0.;
	_e_grid_import_annual = 0.;
	_e_grid_export_annual = 0.;
	_e_loss_system_annual = 0.;
}
