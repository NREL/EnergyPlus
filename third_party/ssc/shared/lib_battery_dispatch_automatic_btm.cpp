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

#include "lib_battery_dispatch_automatic_btm.h"
#include "lib_battery_powerflow.h"
#include "lib_shared_inverter.h"

#include <math.h>

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
	bool can_fuelcell_charge,
    rate_data* util_rate,
    std::vector<double> battReplacementCostPerkWh,
    int battCycleCostChoice,
    std::vector<double> battCycleCost
	) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge,
        battReplacementCostPerkWh, battCycleCostChoice, battCycleCost)
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

    if (util_rate)
    {
        rate = std::shared_ptr<rate_data>(new rate_data(*util_rate));
    }

    costToCycle();
}

void dispatch_automatic_behind_the_meter_t::init_with_pointer(const dispatch_automatic_behind_the_meter_t* tmp)
{
	_P_target_input = tmp->_P_target_input;
	_P_target_month = tmp->_P_target_month;
	_P_target_current = tmp->_P_target_current;
	grid = tmp->grid;

	// time series data which could be slow to copy. Since this doesn't change, should probably make const and have copy point to common memory
	_P_load_ac = tmp->_P_load_ac;
	_P_target_use = tmp->_P_target_use;
	sorted_grid = tmp->sorted_grid;

    if (tmp->rate)
    {
        rate = std::shared_ptr<rate_data>(new rate_data(*tmp->rate));
        rate_forecast = std::shared_ptr<UtilityRateForecast>(new UtilityRateForecast(*tmp->rate_forecast));
    }
}

// deep copy from dispatch to thisq
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
    curr_year = year;
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(year, hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
    if (rate_forecast)
    {
        std::vector<double> actual_dispatch = { m_batteryPower->powerGrid };
        rate_forecast->forecastCost(actual_dispatch, year, hour_of_year, step);
    }
}

void dispatch_automatic_behind_the_meter_t::update_load_data(std::vector<double> P_load_ac){ _P_load_ac = P_load_ac; }
void dispatch_automatic_behind_the_meter_t::set_target_power(std::vector<double> P_target){ _P_target_input = P_target; }
double dispatch_automatic_behind_the_meter_t::power_grid_target() { return _P_target_current; };

void dispatch_automatic_behind_the_meter_t::setup_rate_forecast()
{
    if (_mode == dispatch_t::FORECAST)
    {
        // Process load and pv forecasts to get _monthly_ expected gen, load, and peak
        // Do we need new member variables, or can these just be passed off to UtilityRateForecast?
        std::vector<double> monthly_gross_load;
        std::vector<double> monthly_gen;
        std::vector<double> monthly_net_load;

        // Load here is every step for the full analysis period. Load escalation has already been applied (TODO in compute modules)
        size_t num_recs = util::hours_per_year * _steps_per_hour * _nyears;
        size_t step = 0; size_t hour_of_year = 0;
        int curr_month = 1;
        double load_during_month = 0.0; double gen_during_month = 0.0; double gross_load_during_month = 0.0;
        size_t array_size = std::min(_P_pv_ac.size(), _P_load_ac.size()); // Cover smaller arrays to make testing easier
        for (size_t idx = 0; idx < num_recs && idx < array_size; idx++)
        {
            double grid_power = _P_pv_ac[idx] - _P_load_ac[idx];

            gross_load_during_month += _P_load_ac[idx] * _dt_hour;
            

            if (grid_power < 0)
            {
                load_during_month += grid_power * _dt_hour;
            }
            else
            {
                gen_during_month += grid_power * _dt_hour;
            }

            step++;
            if (step == _steps_per_hour)
            {
                step = 0;
                hour_of_year++;
                if (hour_of_year >= 8760) {
                    hour_of_year = 0;
                }
            }
            if (util::month_of((double) hour_of_year) != curr_month || (idx == array_size - (size_t) 1))
            {
                // Push back vectors
                // Note: this is a net-billing approach. To be accurate for net metering, we'd have to invoke tou periods here, this overestimates costs for NM
                monthly_gross_load.push_back(gross_load_during_month / util::hours_in_month(curr_month));
                monthly_net_load.push_back(-1.0 * load_during_month);
                monthly_gen.push_back(gen_during_month);

                gross_load_during_month = 0.0; load_during_month = 0.0; gen_during_month = 0.0;
                curr_month < 12 ? curr_month++ : curr_month = 1;
            }
        }

        rate_forecast = std::shared_ptr<UtilityRateForecast>(new UtilityRateForecast(rate.get(), _steps_per_hour, monthly_net_load, monthly_gen, monthly_gross_load, _nyears));
        rate_forecast->initializeMonth(0, 0);
        rate_forecast->copyTOUForecast();
    }
}

void dispatch_automatic_behind_the_meter_t::update_dispatch(size_t year, size_t hour_of_year, size_t step, size_t idx)
{
	bool debug = false;
	FILE *p;
	check_debug(hour_of_year, idx, p, debug);
	size_t hour_of_day = util::hour_of_day(hour_of_year);
	_day_index = (hour_of_day * _steps_per_hour + step);

    // [kWh] - the maximum energy that can be cycled
    double E_max = 0;

    if (_mode == dispatch_t::FORECAST)
    {
        // Hourly rolling forecast horizon
        if (hour_of_year != _hour_last_updated)
        {
            costToCycle();
            bool new_month = check_new_month(hour_of_year, step);
            if (new_month)
            {
                rate_forecast->copyTOUForecast();
            }
            initialize(hour_of_year, idx);

            double no_dispatch_cost = compute_costs(idx, year, hour_of_year, p, debug);

            compute_energy(E_max, p, debug);
            cost_based_target_power(idx, year, hour_of_year, no_dispatch_cost, E_max, p, debug);

            // Set battery power profile
            set_battery_power(idx, p, debug);            
        }
        m_batteryPower->powerBatteryTarget = _P_battery_use[step];
    }
	else if (_mode != dispatch_t::CUSTOM_DISPATCH)
	{
		// Currently hardcoded to have 24 hour look ahead and 24 dispatch_update
		if (hour_of_day == 0 && hour_of_year != _hour_last_updated)
		{
			check_new_month(hour_of_year, step);

			// setup vectors
			initialize(hour_of_year, idx);

			// compute grid power, sort highest to lowest
			sort_grid(idx, p, debug);

			// Peak shaving scheme
			compute_energy(E_max, p, debug);
			target_power(E_max, idx, p, debug);

			// Set battery power profile
			set_battery_power(idx, p, debug);
		}
		// save for extraction
		_P_target_current = _P_target_use[_day_index];
		m_batteryPower->powerBatteryTarget = _P_battery_use[_day_index];
	}
	else
	{
        // extract input power by modifying lifetime index to year 1
        m_batteryPower->powerBatteryTarget = _P_battery_use[idx % (8760 * _steps_per_hour)];
        double loss_kw = _Battery->calculate_loss(m_batteryPower->powerBatteryTarget, idx); // Battery is responsible for covering discharge losses
        if (m_batteryPower->connectionMode == AC_CONNECTED) {
            m_batteryPower->powerBatteryTarget = m_batteryPower->adjustForACEfficiencies(m_batteryPower->powerBatteryTarget, loss_kw);
        }
        else if (m_batteryPower->powerBatteryTarget > 0) {
            // Adjust for DC discharge losses
            m_batteryPower->powerBatteryTarget += loss_kw;
        }
	}

	m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;

	if (debug)
		fclose(p);
}
void dispatch_automatic_behind_the_meter_t::initialize(size_t hour_of_year, size_t lifetimeIndex)
{
	_hour_last_updated = hour_of_year;
	_P_target_use.clear();
	_P_battery_use.clear();
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;

	// clean up vectors
    size_t lifetimeMax = _P_pv_ac.size();
	for (size_t ii = 0; ii != _num_steps && lifetimeIndex < lifetimeMax; ii++)
	{
		grid[ii] = grid_point(0., 0, 0, 0., 0.);
		sorted_grid[ii] = grid[ii];
		_P_target_use.push_back(0.);
		_P_battery_use.push_back(0.);
        lifetimeIndex++;
	}
}
bool dispatch_automatic_behind_the_meter_t::check_new_month(size_t hour_of_year, size_t step)
{
    bool ret_value = false;
	size_t hours = 0;
	for (size_t month = 1; month <= _month; month++)
		hours += util::hours_in_month(month);

	if (hours == 8760)
		hours = 0;

	if ((hour_of_year == hours) && step == 0)
	{
		_P_target_month = -1e16;
		_month < 12 ? _month++ : _month = 1;
        ret_value = true;
	}
    return ret_value;
}
void dispatch_automatic_behind_the_meter_t::check_debug(size_t hour_of_year, size_t, FILE*& p, bool& debug)
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

void dispatch_automatic_behind_the_meter_t::sort_grid(size_t idx, FILE *p, const bool debug)
{

	if (debug)
		fprintf(p, "Index\t P_load (kW)\t P_pv (kW)\t P_grid (kW)\n");

	// compute grid net from pv and load (no battery)
	size_t count = 0;
	for (size_t hour = 0; hour != 24; hour++)
	{
		for (size_t step = 0; step != _steps_per_hour; step++)
		{
            // + is load, - is gen
			grid[count] = grid_point(_P_load_ac[idx] - _P_pv_ac[idx], hour, step);
			sorted_grid[count] = grid[count];

			if (debug)
				fprintf(p, "%zu\t %.1f\t %.1f\t %.1f\n", count, _P_load_ac[idx], _P_pv_ac[idx], _P_load_ac[idx] - _P_pv_ac[idx]);

			idx++;
			count++;
		}
	}
	std::sort(sorted_grid.begin(), sorted_grid.end(), byGrid());
}

void dispatch_automatic_behind_the_meter_t::compute_energy(double & E_max, FILE* p, const bool debug)
{

	E_max = _Battery->energy_max(m_batteryPower->stateOfChargeMax, m_batteryPower->stateOfChargeMin);

	if (debug)
	{
		fprintf(p, "Energy Max: %.3f\t", E_max);
		fprintf(p, "Battery Voltage: %.3f\n", _Battery->V());
	}
}

double dispatch_automatic_behind_the_meter_t::compute_available_energy(FILE* p, const bool debug)
{
    double E_available = _Battery->energy_available(m_batteryPower->stateOfChargeMin);

    if (debug)
    {
        fprintf(p, "Energy Available: %.3f\t", E_available);
        fprintf(p, "Battery Voltage: %.3f\n", _Battery->V());
    }

    return E_available;
}

double dispatch_automatic_behind_the_meter_t::compute_costs(size_t idx, size_t year, size_t hour_of_year, FILE* p, const bool debug)
{
    if (debug)
        fprintf(p, "Index\t P_load (kW)\t P_pv (kW)\t P_grid (kW)\n");

    // Copy utility rate calculator to do "no dispatch" forecast
    std::unique_ptr<UtilityRateForecast> noDispatchForecast = std::unique_ptr<UtilityRateForecast>(new UtilityRateForecast(*rate_forecast));
    std::unique_ptr<UtilityRateForecast> marginalForecast = std::unique_ptr<UtilityRateForecast>(new UtilityRateForecast(*rate_forecast));
    double no_dispatch_cost = 0;

    // compute grid net from pv and load (no battery)
    size_t count = 0;
    for (size_t hour = 0; hour != 24; hour++)
    {
        for (size_t step = 0; step != _steps_per_hour && idx < _P_load_ac.size(); step++)
        {
            double power = _P_load_ac[idx] - _P_pv_ac[idx];
            // One at a time so we can sort grid points by no-dispatch cost
            std::vector<double> forecast_power = { -power }; // Correct sign convention for cost forecast
            double step_cost = noDispatchForecast->forecastCost(forecast_power, year, (hour_of_year + hour) % 8760, step);
            no_dispatch_cost += step_cost;

            std::vector<double> marginal_power = { -1.0 };
            double marginal_cost = marginalForecast->forecastCost(marginal_power, year, (hour_of_year + hour) % 8760, step);

            grid[count] = grid_point(power, hour, step, step_cost, marginal_cost);
            sorted_grid[count] = grid[count];

            if (debug)
                fprintf(p, "%zu\t %.1f\t %.1f\t %.1f\n", count, _P_load_ac[idx], _P_pv_ac[idx], _P_load_ac[idx] - _P_pv_ac[idx]);

            idx++;
            count++;
        }
    }
    std::sort(sorted_grid.begin(), sorted_grid.end(), byCost());
    return no_dispatch_cost;
}

void dispatch_automatic_behind_the_meter_t::target_power(double E_useful, size_t idx, FILE*p, const bool debug)
{
	// if target power set, use that
	if (_P_target_input.size() > idx && _P_target_input[idx] >= 0)
	{
		double_vec::const_iterator first = _P_target_input.begin() + idx;
		double_vec::const_iterator last = _P_target_input.begin() + idx + _num_steps;
		double_vec tmp(first, last);
		_P_target_use = tmp;
	}
	// don't calculate if peak grid demand is less than a previous target in the month
	else if (sorted_grid[0].Grid() < _P_target_month)
	{
		for (size_t i = 0; i != _num_steps; i++)
			_P_target_use[i] = _P_target_month;
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

		// Iterate over sorted load to determine target power
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
    for (size_t i = 0; i != _P_battery_use.size(); i++)
        _P_battery_use[i] = grid[i].Grid() - _P_target_use[i];
}

void dispatch_automatic_behind_the_meter_t::cost_based_target_power(size_t idx, size_t year, size_t hour_of_year, double no_dispatch_cost, double E_max, FILE* p, const bool debug)
{
    double startingEnergy = compute_available_energy(p, debug);
    std::vector<dispatch_plan> plans(_num_steps / _steps_per_hour / 2);
    
    plans[0].dispatch_hours = 0;
    plans[0].plannedDispatch.resize(_num_steps);
    plans[0].cost = no_dispatch_cost;

    double lowest_cost = no_dispatch_cost;
    size_t lowest_index = 0;

    for (size_t i = 1; i < plans.size(); i++)
    {
        plans[i].dispatch_hours = i;
        plans[i].plannedDispatch.resize(_num_steps);
        plans[i].plannedGridUse.clear();
        plans[i].plannedDispatch = std::vector<double>(plans[i].plannedDispatch.size());
        plans[i].num_cycles = 0;
        plan_dispatch_for_cost(plans[i], idx, E_max, startingEnergy);
        UtilityRateForecast midDispatchForecast(*rate_forecast);
        plans[i].cost = midDispatchForecast.forecastCost(plans[i].plannedGridUse, year, hour_of_year, 0) + cost_to_cycle() * plans[i].num_cycles - plans[i].kWhRemaining * plans[i].lowestMarginalCost;

        if (plans[i].cost < lowest_cost)
        {
            lowest_index = i;
            lowest_cost = plans[i].cost;
        }
    }

    // Copy from best dispatch plan to _P_battery_use.
    _P_battery_use.assign(plans[lowest_index].plannedDispatch.begin(), plans[lowest_index].plannedDispatch.end());

}

void dispatch_automatic_behind_the_meter_t::plan_dispatch_for_cost(dispatch_plan& plan, size_t idx, double E_max, double startingEnergy)
{
    size_t i = 0, index = 0;

    std::sort(sorted_grid.begin(), sorted_grid.end(), byCost());
    // Iterating over sorted grid
    double costDuringDispatchHours = 0.0;
    double costAtStep = 0.0;
    // Sum no-dispatch cost of top n grid points (dispatch hours * steps per hour). Units: % of cost -> don't need to record this, can re-compute after iteration
    for (i = 0; (i < plan.dispatch_hours * _steps_per_hour) && (i < sorted_grid.size()); i++)
    {
        costAtStep = sorted_grid[i].Cost();
        // In case forecast is testing hours that include negative cost, don't dispatch during those
        if (costAtStep > 1e-7)
        {
            costDuringDispatchHours += sorted_grid[i].Cost();
        }
    }
    double remainingEnergy = E_max;
    double powerAtMaxCost = 0;
    plan.lowestMarginalCost = sorted_grid[0].MarginalCost();
    for (i = 0; i < (plan.dispatch_hours * _steps_per_hour) && (i < sorted_grid.size()); i++)
    {
        costAtStep = sorted_grid[i].Cost();
        if (costAtStep > 1e-7)
        {
            double costPercent = costAtStep / costDuringDispatchHours;
            double desiredPower = remainingEnergy * costPercent / _dt_hour;

            // Prevent the wierd signals from demand charges from reducing dispatch (maybe fix this upstream in the future)
            if (desiredPower < powerAtMaxCost && sorted_grid[i].Grid() >= powerAtMaxCost) {
                desiredPower = powerAtMaxCost;
            }

            if (desiredPower > sorted_grid[i].Grid())
            {
                desiredPower = sorted_grid[i].Grid();
            }
            
            // Account for discharging constraints assuming voltage is constant over forecast period
            check_power_restrictions(desiredPower);

            // Re-apportion based on actual energy used
            remainingEnergy -= desiredPower * _dt_hour;
            costDuringDispatchHours -= costAtStep;

            // Add to dispatch plan
            index = sorted_grid[i].Hour() * _steps_per_hour + sorted_grid[i].Step(); // Assumes we're always running this function on the hour
            plan.plannedDispatch[index] = desiredPower;

            if (powerAtMaxCost == 0) {
                powerAtMaxCost = desiredPower;
            }
        }
    }

    for (i = 0; i < _steps_per_hour && (i < sorted_grid.size()); i++)
    {
        if (sorted_grid[i].Cost() > 0)
        {
            if (sorted_grid[i].MarginalCost() < plan.lowestMarginalCost)
            {
                plan.lowestMarginalCost = sorted_grid[i].MarginalCost();
            }
        }
        else {
            break;
        }
    }

    // Aim to keep the battery at 50%
    double chargeEnergy = E_max - remainingEnergy;
    chargeEnergy = std::max(chargeEnergy, E_max / 2.0);

    // Need to plan on charging extra to account for round trip losses
    double requiredEnergy = chargeEnergy / (m_batteryPower->singlePointEfficiencyACToDC * m_batteryPower->singlePointEfficiencyDCToAC);

    // Iterating over hours
    // Apply clipped energy first, if available
    if (_P_cliploss_dc.size() > 0 && m_batteryPower->canClipCharge)
    {
        size_t idx_clip = idx;
        for (i = 0; i < _num_steps && idx_clip < _P_cliploss_dc.size(); i++)
        {
            double clippedPower = _P_cliploss_dc[idx_clip];
            if (clippedPower > 0)
            {
                // Convert to charging sign convention
                clippedPower *= -1.0;
                check_power_restrictions(clippedPower);
                plan.plannedDispatch[i] = clippedPower; // Could overwrite discharge. Not going to be able to use the DC connected battery if we're already clipping
                requiredEnergy += clippedPower;
            }
        }
    }
    // Get max grid use during charging. Choose highest percentile < 25% where we aren't planning on discharging
    std::sort(sorted_grid.begin(), sorted_grid.end(), byGrid());
    bool lookingForGridUse = true;
    double peakDesiredGridUse = 0.0;
    i = _num_steps / 4;
    while (lookingForGridUse && i < _num_steps)
    {
        index = sorted_grid[i].Hour() * _steps_per_hour + sorted_grid[i].Step();
        if (plan.plannedDispatch[index] < 0)
        {
            i++;
        }
        else {
            peakDesiredGridUse = sorted_grid[i].Grid() > 0 ? sorted_grid[i].Grid() : 0.0;
            lookingForGridUse = false;
        }

        if (lookingForGridUse && sorted_grid[i].Grid() <= 0)
        {
            lookingForGridUse = false;
        }
    }

    // Iterating over sorted grid
    std::sort(sorted_grid.begin(), sorted_grid.end(), byLowestMarginalCost());
    // Find m hours to get required energy - hope we got today's energy yesterday (for morning peaks). Apportion between hrs of lowest marginal cost
    i = 0;
    while (requiredEnergy > 0 && i < _num_steps)
    {
        index = sorted_grid[i].Hour() * _steps_per_hour + sorted_grid[i].Step();
        // Don't plan to charge if we were already planning to discharge. 0 is no plan, negative is clipped energy
        if (plan.plannedDispatch[index] <= 0.0)
        {
            double requiredPower = 0.0;

            if (m_batteryPower->canGridCharge)
            {
                // If can grid charge, plan to take as much energy as needed
                if (idx + index < _P_pv_ac.size() && _P_pv_ac[idx + index] > 0)
                {
                    requiredPower = -_P_pv_ac[idx + index];
                }
                else {
                    requiredPower = -requiredEnergy / _dt_hour;
                }
            }
            else if (m_batteryPower->canSystemCharge)
            {
                // Powerflow considerations are different between AC and DC connected batteries for system charging
                if (m_batteryPower->connectionMode == m_batteryPower->AC_CONNECTED) {
                    // AC connected assumes PV goes to load first. Need net generation in this case
                    if (sorted_grid[i].Grid() < 0) {
                        requiredPower = sorted_grid[i].Grid();
                    }
                }
                else {
                    // DC connected can charge the battery before sending power to load
                    if (idx + index < _P_pv_ac.size() && _P_pv_ac[idx + index] > 0) {
                        requiredPower = -_P_pv_ac[idx + index];
                    }
                }
                
            }

            if (requiredPower < 0)
            {
                check_power_restrictions(requiredPower);
                // Restrict to up to 25th percentile grid use to avoid creating new peaks
                double projectedGrid = sorted_grid[i].Grid() - requiredPower;
                if (projectedGrid > peakDesiredGridUse)
                {
                    requiredPower = -(peakDesiredGridUse - sorted_grid[i].Grid());
                    requiredPower = requiredPower < 0.0 ? requiredPower : 0.0;
                }

                // Add to existing clipped energy
                requiredPower += plan.plannedDispatch[index];
                check_power_restrictions(requiredPower);

                // Clipped energy was already counted once, so subtract that off incase requiredPower + clipped hit a current restriction
                requiredEnergy += (requiredPower - plan.plannedDispatch[index]) * _dt_hour;
            }

            plan.plannedDispatch[index] = requiredPower;

        }
        i++;
    }

    double energy = startingEnergy;

    int cycleState = 0;
    bool halfCycle = false;
    // Curtail planned dispatch if not enough energy or capacity is available
    for (i = 0; i < plan.plannedDispatch.size(); i++)
    {
        double projectedEnergy = energy - plan.plannedDispatch[i] * _dt_hour;
        if (projectedEnergy < 0)
        {
            plan.plannedDispatch[i] = energy / _dt_hour;
        }
        else if (projectedEnergy > E_max)
        {
            plan.plannedDispatch[i] = (energy - E_max) / _dt_hour;
        }
        energy -= plan.plannedDispatch[i] * _dt_hour;

        if (fabs(plan.plannedDispatch[i] - 0) < 1e-7) {
            plan.plannedDispatch[i] = 0;
        }

        if (plan.plannedDispatch[i] < 0)
        {
            if (cycleState != -1) {
                if (halfCycle) {
                    plan.num_cycles++;
                    halfCycle = false;
                }
                else {
                    halfCycle = true;
                }
            }
            cycleState = -1;
        }

        if (plan.plannedDispatch[i] > 0) {
            if (cycleState != 1) {
                if (halfCycle) {
                    plan.num_cycles++;
                    halfCycle = false;
                }
                else {
                    halfCycle = true;
                }
            }
            cycleState = 1;
        }

        // - is load, + is gen for utility rate code
        double projectedGrid = -grid[i].Grid() + plan.plannedDispatch[i];
        // Remove clip loss charging from projected grid use
        if (i + idx < _P_cliploss_dc.size() && plan.plannedDispatch[i] <= 0)
        {
            double clipLoss = -_P_cliploss_dc[i + idx];
            if (plan.plannedDispatch[i] <= clipLoss)
                projectedGrid -= clipLoss;
            else
                projectedGrid -= plan.plannedDispatch[i];
        }
        plan.plannedGridUse.push_back(projectedGrid);
    }

    plan.kWhRemaining = energy;
}

void dispatch_automatic_behind_the_meter_t::check_power_restrictions(double& power)
{
    double desiredCurrent = power * util::kilowatt_to_watt / _Battery->V();
    restrict_current(desiredCurrent);
    restrict_power(desiredCurrent);
    power = desiredCurrent * _Battery->V() * util::watt_to_kilowatt;
}

void dispatch_automatic_behind_the_meter_t::set_battery_power(size_t idx, FILE *p, const bool debug)
{
	for (size_t i = 0; i != _P_target_use.size(); i++) {

        double loss_kw = _Battery->calculate_loss(_P_battery_use[i], idx + i); // Units are kWac for AC connected batteries, and kWdc for DC connected

		// At this point the target power is expressed in AC, must convert to DC for battery
		if (m_batteryPower->connectionMode == m_batteryPower->AC_CONNECTED) {
            _P_battery_use[i] = m_batteryPower->adjustForACEfficiencies(_P_battery_use[i], loss_kw);
		}
        else {
            _P_battery_use[i] = m_batteryPower->adjustForDCEfficiencies(_P_battery_use[i], loss_kw);
        }
	}

	if (debug)
	{
		for (size_t i = 0; i != _P_target_use.size(); i++)
			fprintf(p, "i=%zu  P_battery: %.2f\n", i, _P_battery_use[i]);
	}
}

void dispatch_automatic_behind_the_meter_t::costToCycle()
{
    // Calculate assuming maximum depth of discharge (most conservative assumption)
    if (m_battCycleCostChoice == dispatch_t::MODEL_CYCLE_COST)
    {
        if (curr_year < m_battReplacementCostPerKWH.size()) {
            double capacityPercentDamagePerCycle = _Battery->estimateCycleDamage();
            m_cycleCost = 0.01 * capacityPercentDamagePerCycle * m_battReplacementCostPerKWH[curr_year] * _Battery->get_params().nominal_energy;
        }
        else {
            // Should only apply to BattWatts. BattWatts doesn't have price signal dispatch, so this is fine.
            m_cycleCost = 0.0;
        }
    }
    else if (m_battCycleCostChoice == dispatch_t::INPUT_CYCLE_COST)
    {
        m_cycleCost = cycle_costs_by_year[curr_year] * _Battery->get_params().nominal_energy;
    }
}

double dispatch_automatic_behind_the_meter_t::cost_to_cycle_per_kwh()
{
    return m_cycleCost / _Battery->get_params().nominal_energy;
}
