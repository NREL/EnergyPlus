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

#ifndef _CMOD_BATTERY_COMMON_
#define _CMOD_BATTERY_COMMON_ 1

#include <map>

#include "core.h"
#include "lib_battery.h"
#include "lib_utility_rate.h"
#include "cmod_utilityrate5.h"

// forward declarations to speed up build
class SharedInverter;
class battery_metrics_t;
class dispatch_t;
class ChargeController;
class UtilityRate;

extern var_info vtab_battery_inputs[];
extern var_info vtab_battery_outputs[];

struct batt_variables
{
	bool system_use_lifetime_output;
	bool en_batt;
	bool en_fuelcell;
	int analysis_period;
	int batt_chem;
	int batt_dispatch;
	int batt_voltage_choice;
	int batt_current_choice;
	int batt_meter_position;
	int batt_target_choice;
	int batt_loss_choice;
	int batt_life_model;
	int batt_calendar_choice;

	ssc_number_t *pcharge = 0;
	ssc_number_t *pdischarge = 0;
	ssc_number_t *pdischarge_percent = 0;
	ssc_number_t *pgridcharge_percent = 0;
	ssc_number_t *pgridcharge = 0;
	ssc_number_t *psched = 0;
	ssc_number_t *psched_weekend = 0;

	/*! The custom dispatch power input by user (<0 = charging, >0 = discharging) in kW */
	std::vector<double> batt_custom_dispatch;

	/*! Determines if the battery is allowed to charge from the grid using automated control*/
	bool batt_dispatch_auto_can_gridcharge;

	/*! Determines if the battery is allowed to charge from the RE using automated control*/
	bool batt_dispatch_auto_can_charge;

	/*! Determines if the battery is allowed to charge from PV clipping using automated control*/
	bool batt_dispatch_auto_can_clipcharge;

	/*! Determines if the battery is allowed to charge from fuel cell using automated control*/
	bool batt_dispatch_auto_can_fuelcellcharge;

	/*! Vector of periods and if battery can charge from PV*/
	std::vector<bool> batt_can_charge;

	/*! Vector of periods if battery can charge from Fuel Cell*/
	std::vector<bool> batt_can_fuelcellcharge;

	/*! Vector of periods and if battery can discharge*/
	std::vector<bool> batt_can_discharge;

	/*! Vector of periods and if battery can charge from the grid*/
	std::vector<bool> batt_can_gridcharge;

	/*! Vector of percentages that battery is allowed to charge for periods*/
	std::vector<double> batt_discharge_percent;

	/*! Vector of percentages that battery is allowed to gridcharge for periods*/
	std::vector<double> batt_gridcharge_percent;

	/*! Schedule of manual discharge for weekday*/
	util::matrix_t<size_t> batt_discharge_schedule_weekday;

	/*! Schedule of manual discharge for weekend*/
	util::matrix_t<size_t> batt_discharge_schedule_weekend;

	/*! The number of hours to look-ahead in automated dispatch */
	size_t batt_look_ahead_hours;

	/*! The frequency to update the look-ahead automated dispatch */
	double batt_dispatch_update_frequency_hours;

	util::matrix_t<double>  batt_lifetime_matrix;
	util::matrix_t<double> batt_calendar_lifetime_matrix;
	util::matrix_t<double> batt_voltage_matrix;

	std::vector<double> target_power_monthly;
	std::vector<double> target_power;

	std::vector<double> batt_losses_charging;
	std::vector<double> batt_losses_discharging;
	std::vector<double> batt_losses_idle;
	std::vector<double> batt_losses;

	int batt_computed_series;
	int batt_computed_strings;

	double batt_kw;
	double batt_kwh;

	double batt_Vnom_default;
	double batt_Vfull;
	double batt_Vexp;
	double batt_Vnom;
	double batt_Qfull;
	double batt_Qfull_flow;
	double batt_Qexp;
	double batt_Qnom;
	double batt_C_rate;
	double batt_resistance;

	double batt_replacement_capacity;
	util::matrix_t<double> cap_vs_temp;
	double batt_mass;
	double batt_surface_area;
	double batt_Cp;
	double batt_h_to_ambient;
	std::vector<double> T_room;

	double LeadAcid_q20_computed;
	double LeadAcid_tn;
	double LeadAcid_qn_computed;
	double LeadAcid_q10_computed;

	double batt_initial_SOC;
	double batt_maximum_SOC;
	double batt_minimum_SOC;
	double batt_current_charge_max;
	double batt_current_discharge_max;
	double batt_power_charge_max_kwdc;
	double batt_power_discharge_max_kwdc;
	double batt_power_charge_max_kwac;
	double batt_power_discharge_max_kwac;
	double batt_minimum_modetime;

	int batt_topology;
	double batt_ac_dc_efficiency;
	double batt_dc_ac_efficiency;
	double batt_dc_dc_bms_efficiency;
	double pv_dc_dc_mppt_efficiency;

	size_t inverter_model;
	double inverter_efficiency;
	double inverter_paco;
	size_t inverter_count;
	double batt_inverter_efficiency_cutoff;

	double batt_calendar_q0;
	double batt_calendar_a;
	double batt_calendar_b;
	double batt_calendar_c;

	/*! Battery costs */
	std::vector<double> batt_cost_per_kwh;

	/*! PPA price */
	std::vector<double> forecast_price_series_dollar_per_kwh;

	/*! Energy rates */
	bool ec_rate_defined, ec_use_realtime;
	util::matrix_t<size_t> ec_weekday_schedule;
	util::matrix_t<size_t> ec_weekend_schedule;
	util::matrix_t<double> ec_tou_matrix;
	std::vector<double> ec_realtime_buy;

	/* Battery replacement options */
	int batt_replacement_option;
	std::vector<double> batt_replacement_schedule_percent;

	/* Battery cycle costs */
	int batt_cycle_cost_choice;
    std::vector<double> batt_cycle_cost;
};

struct battstor
{
	/// Pass in the single-year number of records
	battstor(var_table &vt, bool setup_model, size_t nrec, double dt_hr, const std::shared_ptr<batt_variables>& batt_vars_in=0);

    battstor(const battstor& orig);

	void parse_configuration();

	/// Initialize automated dispatch with lifetime vectors
	void initialize_automated_dispatch(std::vector<ssc_number_t> pv= std::vector<ssc_number_t>(),
									   std::vector<ssc_number_t> load= std::vector<ssc_number_t>(),
									   std::vector<ssc_number_t> cliploss= std::vector<ssc_number_t>());
	~battstor();


	void initialize_time(size_t year, size_t hour_of_year, size_t step);

	/// Run the battery for the current timestep, given the System power, load, and clipped power
	void advance(var_table *vt, double P_gen, double V_gen=0, double P_load=0, double P_gen_clipped=0);

	/// Given a DC connected battery, set the shared system (typically PV) and battery inverter
	void setSharedInverter(SharedInverter * sharedInverter);

	void outputs_fixed();
	void outputs_topology_dependent();
	void metrics();
	void update_grid_power(compute_module &cm, double P_gen_ac, double P_load_ac, size_t index);

	/*! Manual dispatch*/
	bool manual_dispatch = false;

	/*! Automated dispatch look ahead*/
	bool look_ahead = false;

	/*! Automated dispatch look behind*/
	bool look_behind = false;

	/*! Automated dispatch use custom input forecast (look ahead)*/
	bool input_forecast = false;

	/*! Automated dispatch override algorithm grid target calculation*/
	bool input_target = false;

	/*! Use user-input battery dispatch */
	bool input_custom_dispatch = false;

	// for user schedule
	void check_replacement_schedule();
	void calculate_monthly_and_annual_outputs( compute_module &cm );

	// time quantities
	size_t step_per_hour;
	size_t step_per_year;
	size_t nyears;
	size_t total_steps;
	double _dt_hour;

	size_t year;
	size_t hour;
	size_t step;
	size_t index; // lifetime_index (0 - nyears * steps_per_hour * 8760)
	size_t year_index; // index for one year (0- steps_per_hour * 8760)

	// member data
	battery_t *battery_model;
	battery_metrics_t *battery_metrics;
	dispatch_t *dispatch_model;
	ChargeController *charge_control;
	UtilityRate * utilityRate;
    rate_data* util_rate_data;

	bool en;
	int chem;

	std::shared_ptr<batt_variables> batt_vars;
	bool make_vars;

	/*! Map of profile to discharge percent */
	std::map<size_t, double> dm_percent_discharge;

	/*! Map of profile to gridcharge percent*/
	std::map<size_t, double> dm_percent_gridcharge;

	std::vector<double> target_power;
	std::vector<double> target_power_monthly;

	double e_charge;
	double e_discharge;

	/*! Variables to store forecast data */
	std::vector<double> pv_prediction;
	std::vector<double> load_prediction;
	std::vector<double> cliploss_prediction;
	int prediction_index;

	/*! If fuel cell is attached */
	std::vector<double> fuelcellPower;

	// outputs
	ssc_number_t
		*outTotalCharge,
		*outAvailableCharge,
		*outBoundCharge,
		*outMaxChargeAtCurrent,
		*outMaxCharge,
		*outMaxChargeThermal,
		*outSOC,
		*outDOD,
		*outCurrent,
		*outCellVoltage,
		*outBatteryVoltage,
		*outCapacityPercent,
		*outCapacityPercentCycle,
		*outCapacityPercentCalendar,
		*outCycles,
		*outDODCycleAverage,
		*outBatteryBankReplacement,
		*outBatteryTemperature,
		*outCapacityThermalPercent,
		*outDispatchMode,
		*outBatteryPower,
		*outGenPower,
        *outGenWithoutBattery,
		*outGridPower,
		*outSystemToLoad,
		*outBatteryToLoad,
		*outGridToLoad,
		*outFuelCellToLoad,
		*outGridPowerTarget,
		*outBattPowerTarget,
		*outSystemToBatt,
		*outGridToBatt,
		*outFuelCellToBatt,
		*outSystemToGrid,
		*outBatteryToGrid,
		*outFuelCellToGrid,
		*outBatteryConversionPowerLoss,
		*outBatterySystemLoss,
		*outAnnualSystemChargeEnergy,
		*outAnnualGridChargeEnergy,
		*outAnnualChargeEnergy,
		*outAnnualDischargeEnergy,
		*outAnnualGridImportEnergy,
		*outAnnualGridExportEnergy,
		*outAnnualEnergySystemLoss,
		*outAnnualEnergyLoss,
		*outMarketPrice,
		*outCostToCycle,
		*outBenefitCharge,
		*outBenefitGridcharge,
		*outBenefitClipcharge,
		*outBenefitDischarge;

	double outAverageCycleEfficiency;
	double outAverageRoundtripEfficiency;
	double outSystemChargePercent;
};

#endif
