#include <cmath>
#include <math.h>
#include "cmod_csp_common_eqns.h"
#include "vartab.h"
#include "csp_system_costs.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'




HTFProperties GetHtfProperties(int fluid_number, const util::matrix_t<double> &specified_fluid_properties) {       // [-]

    HTFProperties htf_properties;

    if (fluid_number != HTFProperties::User_defined)
    {
        if (!htf_properties.SetFluid(fluid_number))
        {
            throw("Fluid number is not recognized");
        }
    }
    else if (fluid_number == HTFProperties::User_defined)
    {
        std::size_t n_rows = specified_fluid_properties.nrows();
        std::size_t n_cols = specified_fluid_properties.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!htf_properties.SetUserDefinedFluid(specified_fluid_properties))
            {
                std::string error_msg = util::format(htf_properties.UserFluidErrMessage(), n_rows, n_cols);
                throw(error_msg);
            }
        }
        else
        {
            std::string error_msg = util::format("The user defined fluid properties table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(error_msg);
        }
    }
    else
    {
        throw("Fluid code is not recognized");
    }

    return htf_properties;
}



// Originally from 'MSPT System Design' UI form
double Nameplate(double P_ref /*MWe*/, double gross_net_conversion_factor /*-*/) {      // MWe
    return P_ref * gross_net_conversion_factor;
}

double Q_pb_design(double P_ref /*MWe*/, double design_eff /*-*/) {     // MWt
    return P_ref / design_eff;
}

double Q_rec_des(double solarm /*-*/, double q_pb_design /*MWt*/) {     // MWt
    return solarm * q_pb_design;
}

double Tshours_sf(double tshours /*hr*/, double solarm /*-*/) {         // hr
    return tshours / solarm;
}



// Originally from 'Tower SolarPilot Solar Field' UI Form
double Land_max_calc(double land_max /*-*/, double h_tower /*m*/) {      // [m]
    return land_max * h_tower;
}

int N_hel(const util::matrix_t<ssc_number_t> &helio_positions /*m*/) {      // [-]
    return static_cast<int>(helio_positions.nrows());
}

double Csp_pt_sf_heliostat_area(double helio_height /*m*/, double helio_width /*m*/, double dens_mirror /*-*/) {     // [m2]
    return helio_height * helio_width * dens_mirror;
}

double Csp_pt_sf_total_reflective_area(int n_hel /*-*/, double csp_pt_sf_heliostat_area /*m2*/) {     // [m2]
    return n_hel * csp_pt_sf_heliostat_area;
}

double Land_min_calc(double land_min /*-*/, double h_tower /*m*/) {      // [m]
    return land_min * h_tower;
}

double Csp_pt_sf_total_land_area(double csp_pt_sf_fixed_land_area /*acres*/, double land_area_base /*acres*/,
    double csp_pt_sf_land_overhead_factor /*-*/) {       // [acres]
    
    return csp_pt_sf_fixed_land_area + land_area_base * csp_pt_sf_land_overhead_factor;
}

double A_sf_UI(double helio_width /*m*/, double helio_height /*m*/, double dens_mirror /*-*/, int n_hel /*-*/) {  // [m2]
    return helio_width * helio_height * dens_mirror * n_hel;
}

double Helio_area_tot(double A_sf_UI /*m2*/) {     // [m2]
    return A_sf_UI;
}

double Csp_pt_sf_tower_height(double h_tower /*m*/) {        // [m]
    return h_tower;
}

double C_atm_info(const util::matrix_t<ssc_number_t> &helio_positions /*m*/,
    double c_atm_0 /*-*/, double c_atm_1 /*-*/, double c_atm_2 /*-*/, double c_atm_3 /*-*/, double h_tower /*m*/) {  // [%]
    
    double tht2 = h_tower * h_tower;
    std::size_t n_hel = helio_positions.nrows();

    double tot_att = 0.;
    for (std::size_t i = 0; i < n_hel; i++) {
        double x = helio_positions.at(i, 0);
        double y = helio_positions.at(i, 1);
        double r = std::sqrt(x*x + y*y);
        double r2 = r*r;

        double s = std::sqrt(tht2 + r2) * 0.001;    // [km]
        double s2 = s*s;
        double s3 = s2*s;

        tot_att += c_atm_0 + c_atm_1*s + c_atm_2*s2 + c_atm_3*s3;
    }

    return 100. * tot_att / n_hel;
}

double Error_equiv(double helio_optical_error_mrad /*mrad*/) {       // [mrad]
    return std::sqrt(2. * helio_optical_error_mrad * 2. * helio_optical_error_mrad * 2.);
}

int Is_optimize(int override_opt /*-*/) {      // [-]
    if (override_opt == 1) {
        return 1;
    }
    else {
        return 0;
    }
}

double Field_model_type(int is_optimize /*-*/, int override_layout /*-*/) {      // [-]
    if (is_optimize == 1) {
        return 0;
    }
    else if (override_layout) {
        return 1;
    }
    else {
        return 2;
    }
}

double Q_design(double Q_rec_des /*MWt*/) {      // [MWt]
    return Q_rec_des;
}

double Dni_des_calc(double dni_des /*W/m2*/) {       // [W/m2]
    return dni_des;
}

int Opt_algorithm() {        // [-]
    return 1;
}

double Opt_flux_penalty() {  // [-]
    return 0.25;
}



// Originally from 'MSPT Receiver' UI Form
double Csp_pt_rec_cav_lip_height() {     // [m]
    return 1.;
}

double Csp_pt_rec_cav_panel_height() {   // [m]
    return 1.1;
}

double Csp_pt_rec_htf_t_avg(double T_htf_cold_des /*C*/, double T_htf_hot_des /*C*/) {       // [C]
    return (T_htf_cold_des + T_htf_hot_des) / 2.;
}

double Csp_pt_rec_htf_c_avg(double csp_pt_rec_htf_t_avg /*C*/, int rec_htf /*-*/,
    const util::matrix_t<ssc_number_t> &field_fl_props /*-*/) {      // [kJ/kg-K]
    
    HTFProperties htf_properties = GetHtfProperties(rec_htf, field_fl_props);
    return htf_properties.Cp(csp_pt_rec_htf_t_avg + 273.15);
}

double Csp_pt_rec_max_flow_to_rec(double csp_pt_rec_max_oper_frac /*-*/, double Q_rec_des /*MWt*/,
    double csp_pt_rec_htf_c_avg /*kJ/kg-K*/, double T_htf_hot_des /*C*/, double T_htf_cold_des /*C*/) {      // [kg/s]

    return (csp_pt_rec_max_oper_frac * Q_rec_des * 1.e6) /
        (csp_pt_rec_htf_c_avg * 1.e3 * (T_htf_hot_des - T_htf_cold_des));
}

double Csp_pt_rec_cav_ap_height(double rec_d_spec /*m*/, double csp_pt_rec_cav_ap_hw_ratio /*-*/) {      // [m]
    return rec_d_spec * csp_pt_rec_cav_ap_hw_ratio;
}

double Rec_aspect(double D_rec /*m*/, double rec_height /*m*/) {     // [-]
    double aspect;
    if (D_rec != 0.) {
        aspect = rec_height / D_rec;
    }
    else {
        aspect = 1.;
    }

    return aspect;
}

double Piping_length(double h_tower /*m*/, double piping_length_mult /*-*/, double piping_length_const /*m*/) {      // [m]
    return h_tower * piping_length_mult + piping_length_const;
}

double Piping_loss_tot(double piping_length /*m*/, double piping_loss /*Wt/m*/) {        // [kWt]
    return piping_length * piping_loss / 1000.;
}



// Originally from 'MSPT System Control'
double Csp_pt_par_calc_bop(double bop_par /*MWe/MWcap*/, double bop_par_f /*-*/, double bop_par_0 /*-*/,
    double bop_par_1 /*-*/, double bop_par_2 /*-*/, double p_ref /*MWe*/) {      // [MWe]

    return bop_par * bop_par_f * ( bop_par_0 + bop_par_1 + bop_par_2 ) * p_ref;
}

double Csp_pt_par_calc_aux(double aux_par /*MWe/MWcap*/, double aux_par_f /*-*/, double aux_par_0 /*-*/,
    double aux_par_1 /*-*/, double aux_par_2 /*-*/, double p_ref /*MWe*/) {      // [MWe]

    return aux_par * aux_par_f * (aux_par_0 + aux_par_1 + aux_par_2) * p_ref;
}

double Disp_wlim_max(double disp_wlim_maxspec /**/, double constant /*%*/) {        // [MWe]
    return disp_wlim_maxspec * (1. - constant / 100.);
}

util::matrix_t<double> Wlim_series(double disp_wlim_max /*MWe*/) {    // [kWe]
    const int kHoursInYear = 8760;

    double disp_wlim_max_kW = disp_wlim_max * 1000.;
    util::matrix_t<double> wlim_series(1, kHoursInYear, disp_wlim_max_kW);

    return wlim_series;
}





// Originally from 'Tower SolarPilot Capital Costs'
//double Ui_tower_height(TowerTypes tower_type, double height) {
//
//}

double Csp_pt_cost_receiver_area(TowerTypes tower_type /*-*/, double d_rec /*m*/, double rec_height /*m*/,
    int receiver_type /*-*/, double rec_d_spec /*m*/, double csp_pt_rec_cav_ap_height /*m*/) {      // [m2]

    double area = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt || tower_type == TowerTypes::kIscc) {
        switch (receiver_type) {
        case 0:
            area = rec_height * d_rec * M_PI;
            break;
        case 1:
            area = rec_d_spec * csp_pt_rec_cav_ap_height;
            break;
        default:
            throw std::runtime_error("Receiver type not supported.");
        }
    }
    else if (tower_type == TowerTypes::kDirectSteam) {
        area = d_rec * rec_height * M_PI;
    }

    return area;
}

double Csp_pt_cost_storage_mwht(TowerTypes tower_type /*-*/, double p_ref /*MWe*/, double design_eff /*-*/,
    double tshours /*hr*/) {      // [MWht]

    double nameplate = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt) {
        nameplate = p_ref / design_eff * tshours;
    }
    else {
        nameplate = 0.;
    }

    return nameplate;
}

double Csp_pt_cost_power_block_mwe(TowerTypes tower_type /*-*/, double p_ref /*MWe*/, double demand_var /*MWe*/)       // [MWe]
{
    double pb = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt) {
        pb = p_ref;
    }
    else {
        pb = demand_var;
    }

    return pb;
}

void Tower_SolarPilot_Capital_Costs_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    C_mspt_system_costs sys_costs;

    //sys_costs.ms_par.A_sf_refl = as_double("A_sf");
    vt_get_number(vt, "a_sf_ui", &sys_costs.ms_par.A_sf_refl);
    vt_get_number(vt, "site_spec_cost", &sys_costs.ms_par.site_improv_spec_cost);
    vt_get_number(vt, "heliostat_spec_cost", &sys_costs.ms_par.heliostat_spec_cost);
    vt_get_number(vt, "cost_sf_fixed", &sys_costs.ms_par.heliostat_fixed_cost);
    vt_get_number(vt, "h_tower", &sys_costs.ms_par.h_tower);                            // set different for other techs
    vt_get_number(vt, "rec_height", &sys_costs.ms_par.h_rec);
    vt_get_number(vt, "helio_height", &sys_costs.ms_par.h_helio);
    vt_get_number(vt, "tower_fixed_cost", &sys_costs.ms_par.tower_fixed_cost);
    vt_get_number(vt, "tower_exp", &sys_costs.ms_par.tower_cost_scaling_exp);
    vt_get_number(vt, "csp_pt_cost_receiver_area", &sys_costs.ms_par.A_rec);            // calculation specific to each tech
    vt_get_number(vt, "rec_ref_cost", &sys_costs.ms_par.rec_ref_cost);
    vt_get_number(vt, "rec_ref_area", &sys_costs.ms_par.A_rec_ref);
    vt_get_number(vt, "rec_cost_exp", &sys_costs.ms_par.rec_cost_scaling_exp);
    vt_get_number(vt, "csp_pt_cost_storage_mwht", &sys_costs.ms_par.Q_storage);         // calculation specific to each tech
    vt_get_number(vt, "tes_spec_cost", &sys_costs.ms_par.tes_spec_cost);
    vt_get_number(vt, "csp_pt_cost_power_block_mwe", &sys_costs.ms_par.W_dot_design);   // calculation specific to each tech
    vt_get_number(vt, "plant_spec_cost", &sys_costs.ms_par.power_cycle_spec_cost);
    vt_get_number(vt, "bop_spec_cost", &sys_costs.ms_par.bop_spec_cost);
    vt_get_number(vt, "fossil_spec_cost", &sys_costs.ms_par.fossil_backup_spec_cost);
    vt_get_number(vt, "contingency_rate", &sys_costs.ms_par.contingency_rate);
    vt_get_number(vt, "csp_pt_sf_total_land_area", &sys_costs.ms_par.total_land_area);
    vt_get_number(vt, "nameplate", &sys_costs.ms_par.plant_net_capacity);
    vt_get_number(vt, "csp_pt_cost_epc_per_acre", &sys_costs.ms_par.EPC_land_spec_cost);
    vt_get_number(vt, "csp_pt_cost_epc_percent", &sys_costs.ms_par.EPC_land_perc_direct_cost);
    vt_get_number(vt, "csp_pt_cost_epc_per_watt", &sys_costs.ms_par.EPC_land_per_power_cost);
    vt_get_number(vt, "csp_pt_cost_epc_fixed", &sys_costs.ms_par.EPC_land_fixed_cost);
    vt_get_number(vt, "land_spec_cost", &sys_costs.ms_par.total_land_spec_cost);
    vt_get_number(vt, "csp_pt_cost_plm_percent", &sys_costs.ms_par.total_land_perc_direct_cost);
    vt_get_number(vt, "csp_pt_cost_plm_per_watt", &sys_costs.ms_par.total_land_per_power_cost);
    vt_get_number(vt, "csp_pt_cost_plm_fixed", &sys_costs.ms_par.total_land_fixed_cost);
    vt_get_number(vt, "sales_tax_frac", &sys_costs.ms_par.sales_tax_basis);
    vt_get_number(vt, "sales_tax_rate", &sys_costs.ms_par.sales_tax_rate);

    try
    {
        sys_costs.calculate_costs();
    }
    catch (...)
    {
        throw std::runtime_error("MSPT system costs. System cost calculations failed. Check that all inputs are properly defined");
    }

    vt->assign("csp_pt_cost_site_improvements", (ssc_number_t)sys_costs.ms_out.site_improvement_cost);
    vt->assign("csp_pt_cost_heliostats", (ssc_number_t)sys_costs.ms_out.heliostat_cost);
    vt->assign("csp_pt_cost_tower", (ssc_number_t)sys_costs.ms_out.tower_cost);
    vt->assign("csp_pt_cost_receiver", (ssc_number_t)sys_costs.ms_out.receiver_cost);
    vt->assign("csp_pt_cost_storage", (ssc_number_t)sys_costs.ms_out.tes_cost);
    vt->assign("csp_pt_cost_power_block", (ssc_number_t)sys_costs.ms_out.power_cycle_cost);
    vt->assign("csp_pt_cost_bop", (ssc_number_t)sys_costs.ms_out.bop_cost);
    vt->assign("csp_pt_cost_fossil", (ssc_number_t)sys_costs.ms_out.fossil_backup_cost);
    vt->assign("ui_direct_subtotal", (ssc_number_t)sys_costs.ms_out.direct_capital_precontingency_cost);
    vt->assign("csp_pt_cost_contingency", (ssc_number_t)sys_costs.ms_out.contingency_cost);
    vt->assign("total_direct_cost", (ssc_number_t)sys_costs.ms_out.total_direct_cost);
    vt->assign("csp_pt_cost_epc_total", (ssc_number_t)sys_costs.ms_out.epc_and_owner_cost);
    vt->assign("csp_pt_cost_plm_total", (ssc_number_t)sys_costs.ms_out.total_land_cost);
    vt->assign("csp_pt_cost_sales_tax_total", (ssc_number_t)sys_costs.ms_out.sales_tax_cost);
    vt->assign("total_indirect_cost", (ssc_number_t)sys_costs.ms_out.total_indirect_cost);
    vt->assign("total_installed_cost", (ssc_number_t)sys_costs.ms_out.total_installed_cost);
    vt->assign("csp_pt_cost_installed_per_capacity", (ssc_number_t)sys_costs.ms_out.estimated_installed_cost_per_cap);
}
