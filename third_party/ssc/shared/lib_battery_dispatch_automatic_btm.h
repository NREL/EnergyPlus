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
#ifndef __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__
#define __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__

#include "lib_battery_dispatch.h"
#include "lib_utility_rate.h"

/*
 * Data for price signal dispatch (FORECAST) to compare dispatch plans in the cost_based_target_power function
 */
struct dispatch_plan
{
    std::vector<double> plannedDispatch;
    std::vector<double> plannedGridUse;
    double cost;
    size_t dispatch_hours;
    int num_cycles;
    double kWhRemaining; // Stored to anticipate the value of energy outside the forecast period
    double lowestMarginalCost;
};

/*! Automated dispatch class for behind-the-meter connections */
class dispatch_automatic_behind_the_meter_t : public dispatch_automatic_t
{
	/**
	Class contains methods and data required to automate dispatch for a behind-the-meter battery targeting peak-shaving applications.
	This includes:
		1. Methods to set or compute the grid power target (desired grid power at every step over the next 24 hours)
		2. Methods to program the dispatch to acheive the target
		3. Method to update the electric load forecast
        4. Methods to generate dispatch targets based on utility rates
	*/
public:
	dispatch_automatic_behind_the_meter_t(
		battery_t * Battery,
		double dt,
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
		bool can_clipcharge,
		bool can_grid_charge,
		bool can_fuelcell_charge,
        rate_data* util_rate,
        std::vector<double> battReplacementCostPerkWh,
        int battCycleCostChoice,
        std::vector<double> battCycleCost
		);

	~dispatch_automatic_behind_the_meter_t() override {};

	// deep copy constructor (new memory), from dispatch to this
	dispatch_automatic_behind_the_meter_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	void copy(const dispatch_t * dispatch) override;

	/// Public API to run the battery dispatch model for the current timestep, given the system power, and optionally the electric load, amount of system clipping, or specified battery power
	void dispatch(size_t year,
		size_t hour_of_year,
		size_t step) override;

	/*! Compute the updated power to send to the battery over the next N hours */
	void update_dispatch(size_t year, size_t hour_of_year, size_t step, size_t idx) override;

	/*! Pass in the load forecast */
	void update_load_data(std::vector<double> P_load_ac);

	/*! Pass in the grid power target vector */
	void set_target_power(std::vector<double> P_target);

    /* Call after pv, load, and cliploss forecasts have been set, only relevant to FORECAST dispatch type */
    void setup_rate_forecast();

	/*! Grid target power */
	double power_grid_target() override;

    /*! Return the calculated cost to cycle for battery outputs */
    double cost_to_cycle_per_kwh() override;

	enum BTM_TARGET_MODES {TARGET_SINGLE_MONTHLY, TARGET_TIME_SERIES};

protected:

	/*! Initialize with a pointer (used by copy constructor) */
	void init_with_pointer(const dispatch_automatic_behind_the_meter_t * tmp);

    /*! Functions that are common to all dispatch methods */
	void initialize(size_t hour_of_year, size_t lifetimeIndex);
    // check_debug can modify the values of bool debug, so p and debug arguments are mandatory here
	void check_debug(size_t hour_of_year, size_t idx, FILE*& p, bool& debug);
    bool check_new_month(size_t hour_of_year, size_t step);
    void compute_energy(double& E_max, FILE* p = NULL, const bool debug = false);
    void set_battery_power(size_t idx, FILE* p = NULL, const bool debug = false);

    /*! Functions used by grid power target algorithms (peak shaving, input grid power targets) */
    void sort_grid(size_t idx, FILE *p = NULL, const bool debug = false);
    void target_power(double E_max, size_t idx, FILE* p = NULL, bool debug = false);

    /*! Functions used by price signal dispatch */
    double compute_costs(size_t idx, size_t year, size_t hour_of_year, FILE* p = NULL, bool debug = false); // Initial computation of no-dispatch costs, assigned hourly to grid points
    void cost_based_target_power(size_t idx, size_t year, size_t hour_of_year, double no_dispatch_cost, double E_max, FILE* p = NULL, const bool debug = false); // Optimizing loop, runs twelve possible dispatch scenarios
    void plan_dispatch_for_cost(dispatch_plan& plan, size_t idx, double E_max, double startingEnergy); // Generates each dispatch plan (input argument)
    double compute_available_energy(FILE* p = NULL, const bool debug = false); // Determine how much energy is available at the start of a dispatch plan
    void check_power_restrictions(double& power); // Call some constraints functions to ensure dispatch doesn't exceed power/current limits

    /*! Calculate the cost to cycle, updates m_cycleCost */
    void costToCycle();

	/*! Full time-series of loads [kW] */
	double_vec _P_load_ac;

	/*! Full time-series of target power [kW] */
	double_vec _P_target_input;

	/*! Time series of length (24 hours * steps_per_hour) of target powers [kW] */
	double_vec _P_target_use;

	/*! The target grid power for the month [kW] */
	double _P_target_month;

	/*! The grid power target at the current time [kW] */
	double _P_target_current;

	/* Vector of length (24 hours * steps_per_hour) containing grid calculation [P_grid, hour, step] */
	grid_vec grid;

	/* Vector of length (24 hours * steps_per_hour) containing sorted grid calculation [P_grid, hour, step] */
	grid_vec sorted_grid;

    /* Utility rate data structure for cost aware dispatch algorithms */
    std::shared_ptr<rate_data> rate;

    /* Forecasting class for cost aware dispatch algorithms. Dispatch will make many copies of this, and keep one master copy tracking the actual grid use. */
    std::shared_ptr <UtilityRateForecast> rate_forecast;

};

#endif // __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__
