#ifndef _BATTERY_COMMON_DATA_H_
#define _BATTERY_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

namespace {
	char load_profile_path_batt[256];
	char temperature_path[256];
	char temperature_path_30min[256];
	char gen_path[256];
	int nb1 = sprintf(gen_path, "%s/test/input_cases/battery_data/lifetime_gen.csv", SSCDIR);
	int nb2 = sprintf(load_profile_path_batt, "%s/test/input_cases/general_data/commercial_load.csv", SSCDIR);
	int nb3 = sprintf(temperature_path, "%s/test/input_cases/battery_data/batt_room_temperature_celsius_60min.csv", SSCDIR);
	int nb4 = sprintf(temperature_path_30min, "%s/test/input_cases/battery_data/batt_room_temperature_celsius_30min.csv", SSCDIR);


	/**
	*  data for commercial peak-shaving battery run that can be further modified
	*/
	void battery_commercial_peak_shaving_lifetime(ssc_data_t &data)
	{
		set_array(data, "gen", gen_path, 175200);
		ssc_data_set_number(data, "system_use_lifetime_output", 1);
		ssc_data_set_number(data, "analysis_period", 20);
		set_array(data, "load", load_profile_path_batt, 8760);
		ssc_data_set_number(data, "en_batt", 1);
		ssc_data_set_number(data, "batt_replacement_option", 2);
		ssc_data_set_number(data, "batt_chem", 1);
		ssc_data_set_number(data, "batt_ac_or_dc", 1);
		ssc_data_set_number(data, "batt_dc_dc_efficiency", 99);
		ssc_data_set_number(data, "batt_dc_ac_efficiency", 98);
		ssc_data_set_number(data, "batt_ac_dc_efficiency", 98);
		ssc_data_set_number(data, "batt_meter_position", 0);
		ssc_number_t p_batt_losses[1] = { 0 };
		ssc_data_set_array(data, "batt_losses", p_batt_losses, 1);
		ssc_number_t p_batt_losses_charging[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		ssc_data_set_array(data, "batt_losses_charging", p_batt_losses_charging, 12);
		ssc_number_t p_batt_losses_discharging[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		ssc_data_set_array(data, "batt_losses_discharging", p_batt_losses_discharging, 12);
		ssc_number_t p_batt_losses_idle[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		ssc_data_set_array(data, "batt_losses_idle", p_batt_losses_idle, 12);
		ssc_data_set_number(data, "batt_loss_choice", 0);
		ssc_data_set_number(data, "batt_current_choice", 1);
		ssc_data_set_number(data, "batt_computed_strings", 12148);
		ssc_data_set_number(data, "batt_computed_series", 14);
		ssc_data_set_number(data, "batt_computed_bank_capacity", 612.2591552734375);
		ssc_data_set_number(data, "batt_current_charge_max", 3037);
		ssc_data_set_number(data, "batt_current_discharge_max", 3037);
		ssc_data_set_number(data, "batt_inverter_efficiency_cutoff", 90);
		ssc_data_set_number(data, "batt_power_charge_max_kwdc", 153.06478881835938);
		ssc_data_set_number(data, "batt_power_discharge_max_kwdc", 153.06478881835938);
		ssc_data_set_number(data, "batt_power_charge_max_kwac", 153.06478881835938);
		ssc_data_set_number(data, "batt_power_discharge_max_kwac", 153.06478881835938);
		ssc_data_set_number(data, "batt_voltage_choice", 0);
		ssc_data_set_number(data, "batt_Vfull", 4.0999999046325684);
		ssc_data_set_number(data, "batt_Vexp", 4.0500001907348633);
		ssc_data_set_number(data, "batt_Vnom", 3.4000000953674316);
		ssc_data_set_number(data, "batt_Vnom_default", 3.5999999046325684);
		ssc_data_set_number(data, "batt_Qfull", 1);
		ssc_data_set_number(data, "batt_Qfull_flow", 12148);
		ssc_data_set_number(data, "batt_Qexp", 0.017799999564886093);
		ssc_data_set_number(data, "batt_Qnom", 0.88899999856948853);
		ssc_data_set_number(data, "batt_C_rate", 0.20000000298023224);
		ssc_data_set_number(data, "batt_resistance", 0.0002);
		ssc_number_t p_batt_voltage_matrix[2] = { 0, 0 };
		ssc_data_set_matrix(data, "batt_voltage_matrix", p_batt_voltage_matrix, 1, 2);
		ssc_data_set_number(data, "LeadAcid_q20_computed", 12148);
		ssc_data_set_number(data, "LeadAcid_q10_computed", 11297.6396484375);
		ssc_data_set_number(data, "LeadAcid_qn_computed", 7288.7998046875);
		ssc_data_set_number(data, "LeadAcid_tn", 1);
		ssc_data_set_number(data, "batt_initial_SOC", 50);
		ssc_data_set_number(data, "batt_minimum_SOC", 15);
		ssc_data_set_number(data, "batt_maximum_SOC", 95);
		ssc_data_set_number(data, "batt_minimum_modetime", 10);
        ssc_data_set_number(data, "batt_life_model", 0);
        ssc_number_t p_batt_lifetime_matrix[18] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
		ssc_data_set_matrix(data, "batt_lifetime_matrix", p_batt_lifetime_matrix, 6, 3);
		ssc_data_set_number(data, "batt_calendar_choice", 1);
		ssc_number_t p_batt_calendar_lifetime_matrix[6] = { 0, 100, 3650, 80, 7300, 50 };
		ssc_data_set_matrix(data, "batt_calendar_lifetime_matrix", p_batt_calendar_lifetime_matrix, 3, 2);
		ssc_data_set_number(data, "batt_calendar_q0", 1.0199999809265137);
		ssc_data_set_number(data, "batt_calendar_a", 0.0026599999982863665);
		ssc_data_set_number(data, "batt_calendar_b", -7280);
		ssc_data_set_number(data, "batt_calendar_c", 930);
		ssc_data_set_number(data, "batt_replacement_capacity", 0);
		ssc_number_t p_batt_replacement_schedule[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };
		ssc_data_set_array(data, "batt_replacement_schedule", p_batt_replacement_schedule, 10);
		ssc_number_t p_batt_replacement_schedule_percent[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 100 };
		ssc_data_set_array(data, "batt_replacement_schedule_percent", p_batt_replacement_schedule_percent, 10);
		ssc_number_t p_replacement_cost[1] = { 68 };
		ssc_data_set_array(data, "om_replacement_cost1", p_replacement_cost, 1);
		ssc_data_set_number(data, "batt_mass", 15);
		ssc_data_set_number(data, "batt_surface_area", 11.92);
		ssc_data_set_number(data, "batt_Cp", 1004);
		ssc_data_set_number(data, "batt_h_to_ambient", 20);
		set_array(data, "batt_room_temperature_celsius", temperature_path, 8760);
		ssc_number_t p_cap_vs_temp[8] = { -10, 60, 0, 80, 25, 100, 40, 100 };
		ssc_data_set_matrix(data, "cap_vs_temp", p_cap_vs_temp, 4, 2);
		ssc_number_t p_dispatch_manual_charge[6] = { 1, 1, 1, 0, 0, 0 };
		ssc_data_set_array(data, "dispatch_manual_charge", p_dispatch_manual_charge, 6);
		ssc_number_t p_dispatch_manual_discharge[6] = { 0, 0, 1, 0, 0, 0 };
		ssc_data_set_array(data, "dispatch_manual_discharge", p_dispatch_manual_discharge, 6);
		ssc_number_t p_dispatch_manual_gridcharge[6] = { 0, 1, 0, 0, 0, 0 };
		ssc_data_set_array(data, "dispatch_manual_gridcharge", p_dispatch_manual_gridcharge, 6);
		ssc_number_t p_dispatch_manual_percent_discharge[2] = { 25, 0 };
		ssc_data_set_array(data, "dispatch_manual_percent_discharge", p_dispatch_manual_percent_discharge, 2);
		ssc_number_t p_dispatch_manual_percent_gridcharge[2] = { 100, 0 };
		ssc_data_set_array(data, "dispatch_manual_percent_gridcharge", p_dispatch_manual_percent_gridcharge, 2);
		ssc_number_t p_batt_target_power[1] = { 15 };
		ssc_data_set_array(data, "batt_target_power", p_batt_target_power, 1);
		ssc_number_t p_batt_target_power_monthly[1] = { 0 };
		ssc_data_set_array(data, "batt_target_power_monthly", p_batt_target_power_monthly, 1);
		ssc_data_set_number(data, "batt_target_choice", 0);
		ssc_number_t p_batt_custom_dispatch[1] = { 0 };
		ssc_data_set_array(data, "batt_custom_dispatch", p_batt_custom_dispatch, 1);
		ssc_data_set_number(data, "batt_dispatch_choice", 0);
		ssc_data_set_number(data, "batt_dispatch_auto_can_gridcharge", 1);
		ssc_data_set_number(data, "batt_dispatch_auto_can_charge", 1);
		ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
		ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
		ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
		ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
		ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 200, 1, 0.21174600720405579, 0.028000000864267349, 1, 2, 400, 1, 0.057693801820278168, 0.028000000864267349, 1, 3, 600, 1, 0.052770901471376419, 0.028000000864267349, 1, 4, 10000, 1, 0.049003798514604568, 0.028000000864267349 };
		ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
        ssc_data_set_number(data, "batt_cycle_cost_choice", 0);
	}
}
#endif
