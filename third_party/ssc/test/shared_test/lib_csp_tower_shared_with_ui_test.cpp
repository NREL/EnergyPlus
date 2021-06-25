#include <gtest/gtest.h>
#include "cmod_csp_tower_eqns.h"
#include "cmod_financial_eqns.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_common {}
using namespace csp_common;

double GetNum(var_table* vd, std::string name) {
    return vd->lookup(name.c_str())->num;
}
double GetNum(ssc_data_t data, std::string name) {
    auto data_vtab = static_cast<var_table*>(data);
    return data_vtab->as_number(name.c_str());
}

//=======Testing Molten Salt Power Tower UI Equations=============================================
NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemDesign) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("design_eff", 0.412);
    vd->assign("gross_net_conversion_factor", 0.9);
    vd->assign("P_ref", 115.);
    vd->assign("solarm", 2.4);
    vd->assign("tshours", 10.);

    MSPT_System_Design_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "nameplate"), 103.5, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_pb_design"), 279., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_rec_des"), 670., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "tshours_sf"), 4.16667, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SolarPilotField) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("c_atm_0", 0.006789);
    vd->assign("c_atm_1", 0.1046);
    vd->assign("c_atm_2", -0.017);
    vd->assign("c_atm_3", 0.002845);
    vd->assign("csp_pt_sf_fixed_land_area", 45.);
    vd->assign("csp_pt_sf_land_overhead_factor", 1.);
    vd->assign("dens_mirror", 0.97);
    vd->assign("dni_des", 950.);
    vd->assign("h_tower", 193.458);
    vd->assign("helio_height", 12.2);
    vd->assign("helio_optical_error_mrad", 1.53);
    util::matrix_t<double> helio_positions(8790, 2, 1.e3);
    vd->assign("helio_positions", helio_positions);
    vd->assign("helio_width", 12.2);
    vd->assign("land_area_base", 1847.04);
    vd->assign("land_max", 9.5);
    vd->assign("land_min", 0.75);
    vd->assign("override_layout", 0);
    vd->assign("override_opt", 0);
    vd->assign("q_rec_des", 670.);

    Tower_SolarPilot_Solar_Field_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "a_sf_ui"), 1269055., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "c_atm_info"), 12.97, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_heliostat_area"), 144.375, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_total_land_area"), 1892., kErrorToleranceHi);
    //ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_total_reflective_area"), 1269056.25, kErrorToleranceHi);			//  This one is not being read in the UI
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_tower_height"), 193.458, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "dni_des_calc"), 950., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "error_equiv"), 4.32749, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "field_model_type"), 2., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "helio_area_tot"), 1269055., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "is_optimize"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "land_max_calc"), 1837.85, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "land_min_calc"), 145.094, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "n_hel"), 8790., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "opt_algorithm"), 1., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "opt_flux_penalty"), 0.25, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_design"), 670., kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SolarPilotFieldWithPeriodUse) {
    // Testing period use in variable names
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("c_atm_0", 0.006789);
    vd->assign("c_atm_1", 0.1046);
    vd->assign("c_atm_2", -0.017);
    vd->assign("c_atm_3", 0.002845);
    vd->assign("csp.pt.sf.fixed_land_area", 45.);
    vd->assign("csp.pt.sf.land_overhead_factor", 1.);
    vd->assign("dens_mirror", 0.97);
    vd->assign("dni_des", 950.);
    vd->assign("h_tower", 193.458);
    vd->assign("helio_height", 12.2);
    vd->assign("helio_optical_error_mrad", 1.53);
    util::matrix_t<double> helio_positions(8790, 2, 1.e3);
    vd->assign("helio_positions", helio_positions);
    vd->assign("helio_width", 12.2);
    vd->assign("land_area_base", 1847.04);
    vd->assign("land_max", 9.5);
    vd->assign("land_min", 0.75);
    vd->assign("override_layout", 0);
    vd->assign("override_opt", 0);
    vd->assign("q_rec_des", 670.);

    Tower_SolarPilot_Solar_Field_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "a_sf_ui"), 1269055., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "c_atm_info"), 12.97, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_heliostat_area"), 144.375, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_total_land_area"), 1892., kErrorToleranceHi);
    //ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_total_reflective_area"), 1269056.25, kErrorToleranceHi);			//  This one is not being read in the UI
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_sf_tower_height"), 193.458, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "dni_des_calc"), 950., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "error_equiv"), 4.32749, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "field_model_type"), 2., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "helio_area_tot"), 1269055., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "is_optimize"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "land_max_calc"), 1837.85, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "land_min_calc"), 145.094, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "n_hel"), 8790., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "opt_algorithm"), 1., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "opt_flux_penalty"), 0.25, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_design"), 670., kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, Receiver) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("t_htf_cold_des", 290.);
    vd->assign("t_htf_hot_des", 574.);
    vd->assign("rec_htf", 17);
    vd->assign("csp_pt_rec_max_oper_frac", 1.2);
    vd->assign("q_rec_des", 660.9);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp_pt_rec_cav_ap_hw_ratio", 1.2);
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 23.8084);
    vd->assign("h_tower", 193.458);
    vd->assign("piping_length_mult", 2.6);
    vd->assign("piping_length_const", 0.);
    vd->assign("piping_loss", 10200.);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    vd->assign("field_fl_props", field_fl_props);

    MSPT_Receiver_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_htf_t_avg"), 432., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_htf_c_avg"), 1.5066, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_max_flow_to_rec"), 1853.5, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_cav_ap_height"), 18., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "rec_aspect"), 1.349, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "piping_length"), 502.991, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "piping_loss_tot"), 5130.51, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, ReceiverWithPeriodUse) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("t_htf_cold_des", 290.);
    vd->assign("t_htf_hot_des", 574.);
    vd->assign("rec_htf", 17);
    vd->assign("csp.pt.rec.max_oper_frac", 1.2);
    vd->assign("q_rec_des", 660.9);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_hw_ratio", 1.2);
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 23.8084);
    vd->assign("h_tower", 193.458);
    vd->assign("piping_length_mult", 2.6);
    vd->assign("piping_length_const", 0.);
    vd->assign("piping_loss", 10200.);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    vd->assign("field_fl_props", field_fl_props);

    MSPT_Receiver_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_htf_t_avg"), 432., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_htf_c_avg"), 1.5066, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_max_flow_to_rec"), 1853.5, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_rec_cav_ap_height"), 18., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "rec_aspect"), 1.349, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "piping_length"), 502.991, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "piping_loss_tot"), 5130.51, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, Tes) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("rec_htf", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    ASSERT_NEAR_FRAC(GetNum(data, "q_tes"), 2791.3, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "tes_avail_vol"), 12986., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "vol_tank"), 14166., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_tank_diameter"), 38.8, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "q_dot_tes_est"), 0.73, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_htf_density"), 1808.48, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, TesWithPeriodUse) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("rec_htf", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    ASSERT_NEAR_FRAC(GetNum(data, "q_tes"), 2791.3, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "tes_avail_vol"), 12986., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "vol_tank"), 14166., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_tank_diameter"), 38.8, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "q_dot_tes_est"), 0.73, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_htf_density"), 1808.48, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemControl) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("bop_par", 0.);
    vd->assign("bop_par_f", 1.);
    vd->assign("bop_par_0", 0.);
    vd->assign("bop_par_1", 0.483);
    vd->assign("bop_par_2", 0.);
    vd->assign("p_ref", 115.);
    vd->assign("aux_par", 0.023);
    vd->assign("aux_par_f", 1.);
    vd->assign("aux_par_0", 0.483);
    vd->assign("aux_par_1", 0.571);
    vd->assign("aux_par_2", 0.);
    vd->assign("disp_wlim_maxspec", 1.);
    vd->assign("constant", 4.);

    MSPT_System_Control_Equations(vd);

    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_par_calc_bop"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_par_calc_aux"), 2.78783, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "disp_wlim_max"), 0.96, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR_FRAC(wlim_series.at(0, 0), 960., kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemControlWithPeriods) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("bop_par", 0.);
    vd->assign("bop_par_f", 1.);
    vd->assign("bop_par_0", 0.);
    vd->assign("bop_par_1", 0.483);
    vd->assign("bop_par_2", 0.);
    vd->assign("p_ref", 115.);
    vd->assign("aux_par", 0.023);
    vd->assign("aux_par_f", 1.);
    vd->assign("aux_par_0", 0.483);
    vd->assign("aux_par_1", 0.571);
    vd->assign("aux_par_2", 0.);
    vd->assign("disp_wlim_maxspec", 1.);
    vd->assign("constant", 4.);

    MSPT_System_Control_Equations(vd);

    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_par_calc_bop"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_par_calc_aux"), 2.78783, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "disp_wlim_max"), 0.96, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR_FRAC(wlim_series.at(0, 0), 960., kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, CapitalCosts) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 21.60);
    vd->assign("receiver_type", 0);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp_pt_rec_cav_ap_height", 18.);
    vd->assign("p_ref", 115.);
    vd->assign("design_eff", 0.412);
    vd->assign("tshours", 10.);
    vd->assign("demand_var", 0);
    vd->assign("a_sf_ui", 1269055.);
    vd->assign("site_spec_cost", 16.);
    vd->assign("heliostat_spec_cost", 140.);
    vd->assign("cost_sf_fixed", 0.);
    vd->assign("h_tower", 193.458);
    vd->assign("rec_height", 21.6029);
    vd->assign("helio_height", 12.2);
    vd->assign("tower_fixed_cost", 3000000.);
    vd->assign("tower_exp", 0.0113);
    vd->assign("csp_pt_cost_receiver_area", 1269055.);
    vd->assign("rec_ref_cost", 103000000.);
    vd->assign("rec_ref_area", 1571.);
    vd->assign("rec_cost_exp", 0.7);
    vd->assign("csp_pt_cost_storage_mwht", 2791.26);
    vd->assign("tes_spec_cost", 22.);
    vd->assign("csp_pt_cost_power_block_mwe", 115.);
    vd->assign("plant_spec_cost", 1040.);
    vd->assign("bop_spec_cost", 290.);
    vd->assign("fossil_spec_cost", 0.);
    vd->assign("contingency_rate", 7.);
    vd->assign("csp_pt_sf_total_land_area", 1892.);
    vd->assign("nameplate", 104.);
    vd->assign("csp_pt_cost_epc_per_acre", 0.);
    vd->assign("csp_pt_cost_epc_percent", 13.);
    vd->assign("csp_pt_cost_epc_per_watt", 0.);
    vd->assign("csp_pt_cost_epc_fixed", 0.);
    vd->assign("land_spec_cost", 10000.);
    vd->assign("csp_pt_cost_plm_percent", 0.);
    vd->assign("csp_pt_cost_plm_per_watt", 0.);
    vd->assign("csp_pt_cost_plm_fixed", 0.);
    vd->assign("sales_tax_frac", 80.);
    vd->assign("sales_tax_rate", 5.);

    Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_receiver_area"), 1197.86, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_storage_mwht"), 2791.26, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_power_block_mwe"), 115., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_site_improvements"), 20304872., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_heliostats"), 177667632., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_tower"), 25319156., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_receiver"), 85191944., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_storage"), 61407768., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_power_block"), 119600000., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_bop"), 33350000., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_fossil"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "ui_direct_subtotal"), 522841376., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_contingency"), 36598896., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_direct_cost"), 559440256., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_epc_total"), 72727232., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_plm_total"), 18920378., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_sales_tax_total"), 22377610., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_indirect_cost"), 114025224., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_installed_cost"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_installed_per_capacity"), 6506.91, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, CapitalCostsWithPeriods) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 21.60);
    vd->assign("receiver_type", 0);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_height", 18.);
    vd->assign("p_ref", 115.);
    vd->assign("design_eff", 0.412);
    vd->assign("tshours", 10.);
    vd->assign("demand_var", 0);
    vd->assign("a_sf_ui", 1269055.);
    vd->assign("site_spec_cost", 16.);
    vd->assign("heliostat_spec_cost", 140.);
    vd->assign("cost_sf_fixed", 0.);
    vd->assign("h_tower", 193.458);
    vd->assign("rec_height", 21.6029);
    vd->assign("helio_height", 12.2);
    vd->assign("tower_fixed_cost", 3000000.);
    vd->assign("tower_exp", 0.0113);
    vd->assign("csp.pt.cost.receiver.area", 1269055.);
    vd->assign("rec_ref_cost", 103000000.);
    vd->assign("rec_ref_area", 1571.);
    vd->assign("rec_cost_exp", 0.7);
    vd->assign("csp.pt.cost.storage_mwht", 2791.26);
    vd->assign("tes_spec_cost", 22.);
    vd->assign("csp.pt.cost.power_block_mwe", 115.);
    vd->assign("plant_spec_cost", 1040.);
    vd->assign("bop_spec_cost", 290.);
    vd->assign("fossil_spec_cost", 0.);
    vd->assign("contingency_rate", 7.);
    vd->assign("csp.pt.sf.total_land_area", 1892.);
    vd->assign("nameplate", 104.);
    vd->assign("csp.pt.cost.epc.per_acre", 0.);
    vd->assign("csp.pt.cost.epc.percent", 13.);
    vd->assign("csp.pt.cost.epc.per_watt", 0.);
    vd->assign("csp.pt.cost.epc.fixed", 0.);
    vd->assign("land_spec_cost", 10000.);
    vd->assign("csp.pt.cost.plm.percent", 0.);
    vd->assign("csp.pt.cost.plm.per_watt", 0.);
    vd->assign("csp.pt.cost.plm.fixed", 0.);
    vd->assign("sales_tax_frac", 80.);
    vd->assign("sales_tax_rate", 5.);

    Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_receiver_area"), 1197.86, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_storage_mwht"), 2791.26, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_power_block_mwe"), 115., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_site_improvements"), 20304872., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_heliostats"), 177667632., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_tower"), 25319156., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_receiver"), 85191944., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_storage"), 61407768., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_power_block"), 119600000., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_bop"), 33350000., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_fossil"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "ui_direct_subtotal"), 522841376., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_contingency"), 36598896., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_direct_cost"), 559440256., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_epc_total"), 72727232., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_plm_total"), 18920378., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_sales_tax_total"), 22377610., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_indirect_cost"), 114025224., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_installed_cost"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_pt_cost_installed_per_capacity"), 6506.91, kErrorToleranceHi);
}

//======Financial Equations=======================================================================
NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase1) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("total_installed_cost", 673465536.);

    vd->assign("const_per_percent1", 100.);
    vd->assign("const_per_upfront_rate1", 1.);
    vd->assign("const_per_months1", 24.);
    vd->assign("const_per_interest_rate1", 4.);

    vd->assign("const_per_percent2", 0.);
    vd->assign("const_per_upfront_rate2", 0.);
    vd->assign("const_per_months2", 0.);
    vd->assign("const_per_interest_rate2", 0.);

    vd->assign("const_per_percent3", 0.);
    vd->assign("const_per_upfront_rate3", 0.);
    vd->assign("const_per_months3", 0.);
    vd->assign("const_per_interest_rate3", 0.);

    vd->assign("const_per_percent4", 0.);
    vd->assign("const_per_upfront_rate4", 0.);
    vd->assign("const_per_months4", 0.);
    vd->assign("const_per_interest_rate4", 0.);

    vd->assign("const_per_percent5", 0.);
    vd->assign("const_per_upfront_rate5", 0.);
    vd->assign("const_per_months5", 0.);
    vd->assign("const_per_interest_rate5", 0.);

    Financial_Construction_Financing_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal1"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest1"), 26938618., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total1"), 33673272., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal2"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest2"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total2"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal3"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest3"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total3"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal4"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest4"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total4"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal5"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest5"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total5"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_percent_total"), 100., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal_total"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest_total"), 26938618., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "construction_financing_cost"), 33673272., kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase2) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("system_capacity", 103500.);

    Financial_Capacity_Payments_Equations(vd);

    //double cp_system_nameplate = vd->lookup("cp_system_nameplate")->num;
    ASSERT_NEAR_FRAC(GetNum(vd, "cp_system_nameplate"), 103.5, kErrorToleranceHi);
}

//======/Testing Molten Salt Power Tower UI Equations=============================================


//TEST(Mspt_cmod_csp_tower_eqns, NoData) {
//	ASSERT_THROW(MSPT_System_Design_Equations(nullptr), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(nullptr), std::runtime_error);
//}

//TEST(Mspt_cmod_csp_tower_eqns, MissingVariables) {
//	var_table* vd = new var_table;
//	ASSERT_THROW(MSPT_System_Design_Equations(vd), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(vd), std::runtime_error);
//}
