#include "lib_battery_dispatch_manual.h"
#include "lib_battery_powerflow.h"

#include <math.h>

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

	m_batteryPower->canSystemCharge = _charge_array[iprofile - 1];
	m_batteryPower->canDischarge = _discharge_array[iprofile - 1];
	m_batteryPower->canGridCharge = _gridcharge_array[iprofile - 1];

	if (iprofile < _fuelcellcharge_array.size()) {
		m_batteryPower->canFuelCellCharge = _fuelcellcharge_array[iprofile - 1];
	}

	_percent_discharge = 0.;
	_percent_charge = 0.;

	if (m_batteryPower->canDischarge){ _percent_discharge = _percent_discharge_array[iprofile]; }
	if (m_batteryPower->canSystemCharge || m_batteryPower->canFuelCellCharge){ _percent_charge = 100.; }
	if (m_batteryPower->canGridCharge){ _percent_charge = _percent_charge_array[iprofile]; }
}
void dispatch_manual_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	prepareDispatch(hour_of_year, step);

	// Initialize power flow model by calculating the battery power to dispatch
	m_batteryPowerFlow->initialize(_Battery->SOC());

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

		// Don't let system (PV) export to grid if can still charge battery (increase charging)
		if (m_batteryPower->powerSystemToGrid > low_tolerance &&
            m_batteryPower->canSystemCharge &&					// only do if battery is allowed to charge
                                                              _Battery->SOC() < m_batteryPower->stateOfChargeMax - 1.0 &&		// and battery SOC is less than max
			fabs(I) < fabs(m_batteryPower->currentChargeMax) &&						// and battery current is less than max charge current
			fabs(m_batteryPower->powerBatteryDC) < (m_batteryPower->powerBatteryChargeMaxDC - 1.0) &&// and battery power is less than max charge power
			I <= 0)											// and battery was not discharging
		{
			double dI = 0;
			if (fabs(m_batteryPower->powerBatteryDC) < tolerance)
				dI = (m_batteryPower->powerSystemToGrid  * util::kilowatt_to_watt / _Battery->V());
			else
				dI = (m_batteryPower->powerSystemToGrid  / fabs(m_batteryPower->powerBatteryAC)) *fabs(I);

			// Main problem will be that this tends to overcharge battery maximum SOC, so check
			double dQ = 0.01 * (m_batteryPower->stateOfChargeMax - _Battery->SOC()) *
                    _Battery->charge_maximum_lifetime();

			I -= fmin(dI, dQ / _dt_hour);
		}
		// Don't let PV serve battery before load (decrease charging)
		else if (m_batteryPower->meterPosition == dispatch_t::BEHIND && I < 0 && m_batteryPower->powerGridToLoad > tolerance &&
			m_batteryPower->powerSystemToBattery > 0) {

			double dP = m_batteryPower->powerGridToLoad;
			if (dP > m_batteryPower->powerSystemToBattery) {
				dP = m_batteryPower->powerSystemToBattery;
			}

			double dI = 0;
			if (dP < tolerance)
				dI = dP / _Battery->V();
			else
				dI = (dP / fabs(m_batteryPower->powerBatteryAC)) *fabs(I);

			I += dI;
		}
		// Don't let battery export to the grid if behind the meter
		else if (m_batteryPower->meterPosition == dispatch_t::BEHIND && I > 0 && m_batteryPower->powerBatteryToGrid > tolerance)
		{
			if (fabs(m_batteryPower->powerBatteryAC) < tolerance)
				I -= (m_batteryPower->powerBatteryToGrid * util::kilowatt_to_watt / _Battery->V());
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
            _Battery->set_state(_Battery_initial->get_state());
			m_batteryPower->powerBatteryAC = 0;
			m_batteryPower->powerGridToBattery = 0;
			m_batteryPower->powerBatteryToGrid = 0;
			m_batteryPower->powerSystemToGrid  = 0;
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
