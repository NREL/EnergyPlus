#ifndef _TCSDIRECT_STEAM_DEFAULTS_H_
#define _TCSDIRECT_STEAM_DEFAULTS_H_

#include <stdio.h>
#include "../input_cases/code_generator_utilities.h"

//const char * SSCDIR = std::getenv("SSCDIR");
//const char * SAMNTDIR = std::getenv("SAMNTDIR");

char ndspt_dispatch_factors_path[512];
char ndspt_helio_positions_path[512];
int ndspt1 = sprintf(ndspt_dispatch_factors_path, "%s/test/input_cases/directsteam_data/dispatch_factors_ts.csv", std::getenv("SSCDIR"));
int ndspt2 = sprintf(ndspt_helio_positions_path, "%s/test/input_cases/directsteam_data/helio_positions.csv", std::getenv("SSCDIR"));

/**
*  Default data for tcsdirect_steam technology model
*/
ssc_data_t tcsdirect_steam_defaults()
{
    ssc_data_t data = ssc_data_create();

	char solar_resource_path_default[512];
	int n1 = sprintf(solar_resource_path_default, "%s/test/input_cases/directsteam_data/daggett_ca_34.865371_-116.783023_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
	ssc_data_set_string(data, "solar_resource_file", solar_resource_path_default);

    ssc_data_set_number(data, "system_capacity", 100125);
    ssc_number_t p_weekday_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekday_schedule", p_weekday_schedule, 12, 24);
    ssc_number_t p_weekend_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekend_schedule", p_weekend_schedule, 12, 24);
    ssc_data_set_number(data, "run_type", 1);
    ssc_data_set_number(data, "helio_width", 12.199999999999999);
    ssc_data_set_number(data, "helio_height", 12.199999999999999);
    ssc_data_set_number(data, "helio_optical_error", 0.0015300000086426735);
    ssc_data_set_number(data, "helio_active_fraction", 0.98999999999999999);
    ssc_data_set_number(data, "helio_reflectance", 0.90000000000000002);
    ssc_data_set_number(data, "rec_absorptance", 0.93999999999999995);
    ssc_data_set_number(data, "rec_aspect", 1.2279255390167236);
    ssc_data_set_number(data, "rec_height", 23.808370590209961);
    ssc_data_set_number(data, "rec_hl_perm2", 30);
    ssc_data_set_number(data, "land_max", 8.5);
    ssc_data_set_number(data, "land_min", 0.75);
    ssc_data_set_number(data, "dni_des", 950);
    ssc_data_set_number(data, "p_start", 0.025000000000000001);
    ssc_data_set_number(data, "p_track", 0.055);
    ssc_data_set_number(data, "hel_stow_deploy", 8);
    ssc_data_set_number(data, "v_wind_max", 15);
    ssc_data_set_number(data, "n_flux_x", 12);
    ssc_data_set_number(data, "n_flux_y", 1);
    ssc_data_set_number(data, "dens_mirror", 0.96999999999999997);
    set_matrix(data, "helio_positions", ndspt_helio_positions_path, 5451, 2);
    ssc_data_set_number(data, "c_atm_0", 0.0067889999999999999);
    ssc_data_set_number(data, "c_atm_1", 0.1046);
    ssc_data_set_number(data, "c_atm_2", -0.017000000000000001);
    ssc_data_set_number(data, "c_atm_3", 0.0028449999999999999);
    ssc_data_set_number(data, "n_facet_x", 2);
    ssc_data_set_number(data, "n_facet_y", 8);
    ssc_data_set_number(data, "focus_type", 1);
    ssc_data_set_number(data, "cant_type", 1);
    ssc_data_set_number(data, "n_flux_days", 8);
    ssc_data_set_number(data, "delta_flux_hrs", 2);
    ssc_data_set_number(data, "water_usage_per_wash", 0.69999999999999996);
    ssc_data_set_number(data, "washing_frequency", 63);
    ssc_data_set_number(data, "H_rec", 23.808370590209961);
    ssc_data_set_number(data, "THT", 154.95500183105469);
    ssc_data_set_number(data, "q_design", 413.05694580078125);
    ssc_data_set_number(data, "tower_fixed_cost", 3000000);
    ssc_data_set_number(data, "tower_exp", 0.011300000362098217);
    ssc_data_set_number(data, "rec_ref_cost", 48800000);
    ssc_data_set_number(data, "rec_ref_area", 1110);
    ssc_data_set_number(data, "rec_cost_exp", 0.69999998807907104);
    ssc_data_set_number(data, "site_spec_cost", 16);
    ssc_data_set_number(data, "heliostat_spec_cost", 140);
    ssc_data_set_number(data, "plant_spec_cost", 1040);
    ssc_data_set_number(data, "bop_spec_cost", 290);
    ssc_data_set_number(data, "tes_spec_cost", 22);
    ssc_data_set_number(data, "land_spec_cost", 10000);
    ssc_data_set_number(data, "contingency_rate", 7);
    ssc_data_set_number(data, "sales_tax_rate", 5);
    ssc_data_set_number(data, "sales_tax_frac", 80);
    ssc_data_set_number(data, "cost_sf_fixed", 0);
    ssc_data_set_number(data, "fossil_spec_cost", 0);
    ssc_data_set_number(data, "is_optimize", 0);
    ssc_data_set_number(data, "flux_max", -1);
    ssc_data_set_number(data, "opt_init_step", 0.050000000000000003);
    ssc_data_set_number(data, "opt_max_iter", 200);
    ssc_data_set_number(data, "opt_conv_tol", 0.001);
    ssc_data_set_number(data, "opt_flux_penalty", 0.25);
    ssc_data_set_number(data, "opt_algorithm", 1);
    ssc_data_set_number(data, "check_max_flux", 0);
    ssc_data_set_number(data, "csp.pt.cost.epc.per_acre", 0);
    ssc_data_set_number(data, "csp.pt.cost.epc.percent", 13);
    ssc_data_set_number(data, "csp.pt.cost.epc.per_watt", 0);
    ssc_data_set_number(data, "csp.pt.cost.epc.fixed", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.per_acre", 10000);
    ssc_data_set_number(data, "csp.pt.cost.plm.percent", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.per_watt", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.fixed", 0);
    ssc_data_set_number(data, "csp.pt.sf.fixed_land_area", 45);
    ssc_data_set_number(data, "csp.pt.sf.land_overhead_factor", 1);
    ssc_data_set_number(data, "total_installed_cost", 444602016);
    ssc_data_set_number(data, "fossil_mode", 1);
    ssc_data_set_number(data, "q_pb_design", 275.37127685546875);
    ssc_data_set_number(data, "q_aux_max", 275.37127685546875);
    ssc_data_set_number(data, "lhv_eff", 0.89999997615814209);
    ssc_data_set_number(data, "h_tower", 154.95500000000001);
    ssc_data_set_number(data, "n_panels", 12);
    ssc_data_set_number(data, "flowtype", 2);
    ssc_data_set_number(data, "d_rec", 19.389099999999999);
    ssc_data_set_number(data, "q_rec_des", 413.05694580078125);
    ssc_data_set_number(data, "f_rec_min", 0.25);
    ssc_data_set_number(data, "rec_qf_delay", 0.25);
    ssc_data_set_number(data, "rec_su_delay", 0.20000000000000001);
    ssc_data_set_number(data, "f_pb_cutoff", 0.20000000298023224);
    ssc_data_set_number(data, "f_pb_sb", 0.20000000298023224);
    ssc_data_set_number(data, "t_standby_ini", 2);
    ssc_data_set_number(data, "x_b_target", 0.5);
    ssc_data_set_number(data, "eta_rec_pump", 0.84999999999999998);
    ssc_data_set_number(data, "P_hp_in_des", 160);
    ssc_data_set_number(data, "P_hp_out_des", 40);
    ssc_data_set_number(data, "f_mdotrh_des", 0.85000002384185791);
    ssc_data_set_number(data, "p_cycle_design", 111.25);
    ssc_data_set_number(data, "ct", 2);
    ssc_data_set_number(data, "T_amb_des", 42);
    ssc_data_set_number(data, "dT_cw_ref", 10);
    ssc_data_set_number(data, "T_approach", 5);
    ssc_data_set_number(data, "T_ITD_des", 16);
    ssc_data_set_number(data, "hl_ffact", 1);
    ssc_data_set_number(data, "h_boiler", 10.428699999999999);
    ssc_data_set_number(data, "d_t_boiler", 0.025399999999999999);
    ssc_data_set_number(data, "th_t_boiler", 0.0021589999999999999);
    ssc_data_set_number(data, "rec_emis", 0.88);
    ssc_data_set_number(data, "mat_boiler", 2);
    ssc_data_set_number(data, "h_sh", 8.4755199999999995);
    ssc_data_set_number(data, "d_sh", 0.019050000000000001);
    ssc_data_set_number(data, "th_sh", 0.0016509999999999999);
    ssc_data_set_number(data, "mat_sh", 2);
    ssc_data_set_number(data, "T_sh_out_des", 550);
    ssc_data_set_number(data, "h_rh", 4.9041499999999996);
    ssc_data_set_number(data, "d_rh", 0.038100000000000002);
    ssc_data_set_number(data, "th_rh", 0.0021589999999999999);
    ssc_data_set_number(data, "mat_rh", 2);
    ssc_data_set_number(data, "T_rh_out_des", 500);
    ssc_data_set_number(data, "cycle_max_frac", 1.0499999523162842);
    ssc_data_set_number(data, "A_sf", 786987.0625);
    ssc_number_t p_ffrac[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_array(data, "ffrac", p_ffrac, 9);
    ssc_data_set_number(data, "P_b_in_init", 160);
    ssc_data_set_number(data, "f_mdot_rh_init", 0.85000002384185791);
    ssc_data_set_number(data, "P_hp_out", 40);
    ssc_data_set_number(data, "T_hp_out", 300);
    ssc_data_set_number(data, "T_rh_target", 500);
    ssc_data_set_number(data, "T_fw_init", 340);
    ssc_data_set_number(data, "P_cond_init", 40);
    ssc_data_set_number(data, "P_ref", 111.25);
    ssc_data_set_number(data, "eta_ref", 0.40400000000000003);
    ssc_data_set_number(data, "T_hot_ref", 550);
    ssc_data_set_number(data, "T_cold_ref", -1.2300000190734863);
    ssc_data_set_number(data, "q_sby_frac", 0.20000000000000001);
    ssc_data_set_number(data, "P_boil_des", 160);
    ssc_data_set_number(data, "P_rh_ref", 40);
    ssc_data_set_number(data, "rh_frac_ref", 0.84999999999999998);
    ssc_data_set_number(data, "startup_time", 0.5);
    ssc_data_set_number(data, "startup_frac", 0.5);
    ssc_data_set_number(data, "P_cond_ratio", 1.0027999999999999);
    ssc_data_set_number(data, "pb_bd_frac", 0.02);
    ssc_data_set_number(data, "P_cond_min", 2);
    ssc_data_set_number(data, "n_pl_inc", 8);
    ssc_number_t p_F_wc[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_array(data, "F_wc", p_F_wc, 9);
    ssc_data_set_number(data, "T_hot", 550);
    ssc_data_set_number(data, "Piping_loss", 10200);
    ssc_data_set_number(data, "Piping_length", 402.88299560546875);
    ssc_data_set_number(data, "piping_length_mult", 2.6000000000000001);
    ssc_data_set_number(data, "piping_length_add", 0);
    ssc_data_set_number(data, "Design_power", 111.25);
    ssc_data_set_number(data, "design_eff", 0.40400001406669617);
    ssc_data_set_number(data, "pb_fixed_par", 0.0054999999999999997);
    ssc_data_set_number(data, "aux_par", 0.023);
    ssc_data_set_number(data, "aux_par_f", 1);
    ssc_data_set_number(data, "aux_par_0", 0.48299999999999998);
    ssc_data_set_number(data, "aux_par_1", 0.57099999999999995);
    ssc_data_set_number(data, "aux_par_2", 0);
    ssc_data_set_number(data, "bop_par", 0);
    ssc_data_set_number(data, "bop_par_f", 1);
    ssc_data_set_number(data, "bop_par_0", 0);
    ssc_data_set_number(data, "bop_par_1", 0.48299999999999998);
    ssc_data_set_number(data, "bop_par_2", 0);
    ssc_data_set_number(data, "adjust:constant", 4);
    ssc_data_set_number(data, "sf_adjust:constant", 0);

    return data;
}

#endif 
