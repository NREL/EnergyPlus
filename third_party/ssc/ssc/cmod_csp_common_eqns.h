#ifndef _CMOD_CSP_COMMON_EQNS_H_
#define _CMOD_CSP_COMMON_EQNS_H_

#include "sscapi.h"
#include "../shared/lib_util.h"
#include "htf_props.h"
#include "vartab.h"


enum class TowerTypes {
    kMoltenSalt,
    kDirectSteam,
    kIscc,
};


SSCEXPORT ssc_bool_t ssc_data_t_get_number(ssc_data_t p_data, const char* name, ssc_number_t* value);

SSCEXPORT void ssc_data_t_set_number(ssc_data_t p_data, const char* name, ssc_number_t value);

SSCEXPORT ssc_number_t* ssc_data_t_get_array(ssc_data_t p_data, const char* name, int* length);

SSCEXPORT void ssc_data_t_set_array(ssc_data_t p_data, const char* name, ssc_number_t* pvalues, int length);

SSCEXPORT void ssc_data_t_get_matrix(var_table* vt, std::string name, util::matrix_t<double>& matrix);

SSCEXPORT void ssc_data_t_set_matrix(ssc_data_t data, const std::string& name, const var_data& val);


HTFProperties GetHtfProperties(int fluid_number, const util::matrix_t<double> &specified_fluid_properties);       // [-]



// Originally from 'MSPT System Design' UI Form
double Nameplate(double P_ref /*MWe*/, double gross_net_conversion_factor /*-*/);       // [MWe]

double Q_pb_design(double P_ref /*MWe*/, double design_eff /*-*/);      // [MWt]

double Q_rec_des(double solarm /*-*/, double q_pb_design /*MWt*/);      // [MWt]

double Tshours_sf(double tshours /*hr*/, double solarm /*-*/);          // [hr]



// Originally from 'Tower SolarPilot Solar Field' UI Form
double Land_max_calc(double land_max /*-*/, double h_tower /*m*/);      // [m]

int N_hel(const util::matrix_t<ssc_number_t> &helio_positions /*m*/);      // [-]

double Csp_pt_sf_heliostat_area(double helio_height /*m*/, double helio_width /*m*/, double dens_mirror /*-*/);     // [m2]

double Csp_pt_sf_total_reflective_area(int n_hel /*-*/, double csp_pt_sf_heliostat_area /*m2*/);     // [m2]

double Land_min_calc(double land_min /*-*/, double h_tower /*m*/);      // [m]

double Csp_pt_sf_total_land_area(double csp_pt_sf_fixed_land_area /*acres*/, double land_area_base /*acres*/,
    double csp_pt_sf_land_overhead_factor /*-*/);       // [acres]

double A_sf_UI(double helio_width /*m*/, double helio_height /*m*/, double dens_mirror /*-*/, int n_hel /*-*/);  // [m2]

double Helio_area_tot(double A_sf_UI /*m2*/);     // [m2]

double Csp_pt_sf_tower_height(double h_tower /*m*/);        // [m]

double C_atm_info(const util::matrix_t<ssc_number_t> &helio_positions /*m*/,
    double c_atm_0 /*-*/, double c_atm_1 /*-*/, double c_atm_2 /*-*/, double c_atm_3 /*-*/, double h_tower /*m*/);  // [%]

double Error_equiv(double helio_optical_error_mrad /*mrad*/);       // [mrad]

bool Is_optimize(bool override_opt /*-*/);      // [-]

int Field_model_type(bool is_optimize /*-*/, bool override_layout /*-*/, int assigned_field_model_type /*-*/);      // [-]

double Q_design(double Q_rec_des /*MWt*/);      // [MWt]

double Dni_des_calc(double dni_des /*W/m2*/);       // [W/m2]

int Opt_algorithm();        // [-]

double Opt_flux_penalty();  // [-]



// Originally from 'MSPT Receiver' UI Form
double Csp_pt_rec_cav_lip_height();     // [m?]

double Csp_pt_rec_cav_panel_height();   // [m?]

double Csp_pt_rec_max_flow_to_rec(double csp_pt_rec_max_oper_frac /*-*/, double Q_rec_des /*MWt*/,
    double csp_pt_rec_htf_c_avg /*kJ/kg-K*/, double T_htf_hot_des /*C*/, double T_htf_cold_des /*C*/);      // [kg/s]

double Csp_pt_rec_htf_t_avg(double T_htf_cold_des /*C*/, double T_htf_hot_des /*C*/);       // [C]

double Csp_pt_rec_cav_ap_height(double rec_d_spec /*m*/, double csp_pt_rec_cav_ap_hw_ratio /*-*/);      // [m]

double Csp_pt_rec_htf_c_avg(double csp_pt_rec_htf_t_avg /*C*/, int rec_htf /*-*/,
    const util::matrix_t<ssc_number_t> &field_fl_props /*-*/);      // [kJ/kg-K]

double Rec_aspect(double D_rec /*m*/, double rec_height /*m*/);     // [-]

double Piping_length(double h_tower /*m*/, double piping_length_mult /*-*/, double piping_length_const /*m*/);      // [m]

double Piping_loss_tot(double piping_length /*m*/, double piping_loss /*Wt/m*/);        // [kWt]



// Originally from 'MSPT System Control'
double Csp_pt_par_calc_bop(double bop_par /*MWe/MWcap*/, double bop_par_f /*-*/, double bop_par_0 /*-*/,
    double bop_par_1 /*-*/, double bop_par_2 /*-*/, double p_ref /*MWe*/);      // [MWe]

double Csp_pt_par_calc_aux(double aux_par /*MWe/MWcap*/, double aux_par_f /*-*/, double aux_par_0 /*-*/,
    double aux_par_1 /*-*/, double aux_par_2 /*-*/, double p_ref /*MWe*/);      // [MWe]

double Disp_wlim_max(double disp_wlim_maxspec /**/, double constant /*%*/);        // [MWe]

util::matrix_t<double> Wlim_series(double disp_wlim_max /*MWe*/);    // [kWe]



// Originally from 'Tower SolarPilot Capital Costs'
//double Ui_tower_height(TowerTypes tower_type, double height);

double Csp_pt_cost_receiver_area(TowerTypes tower_type /*-*/, double d_rec /*m*/,
    double rec_height = std::numeric_limits<double>::quiet_NaN() /*m*/,
    int receiver_type = std::numeric_limits<int>::quiet_NaN() /*-*/,
    double rec_d_spec = std::numeric_limits<double>::quiet_NaN() /*m*/,
    double csp_pt_rec_cav_ap_height = std::numeric_limits<double>::quiet_NaN() /*m*/);        // [m2]

double Csp_pt_cost_storage_mwht(TowerTypes tower_type /*-*/, double p_ref = std::numeric_limits<double>::quiet_NaN() /*MWe*/,
    double design_eff = std::numeric_limits<double>::quiet_NaN() /*-*/,
    double tshours = std::numeric_limits<double>::quiet_NaN() /*hr*/);      // [MWht]

double Csp_pt_cost_power_block_mwe(TowerTypes tower_type /*-*/, double p_ref = std::numeric_limits<double>::quiet_NaN() /*MWe*/,
    double demand_var = std::numeric_limits<double>::quiet_NaN()) /*MWe*/;      // [MWe]

void Tower_SolarPilot_Capital_Costs_Equations(ssc_data_t data);

#endif
