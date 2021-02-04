#include <gtest/gtest.h>

#define private public              // for setting private data members
#include "lib_csp_trough_test.h"
#include "vs_google_test_explorer_namespace.h"

using namespace csp_trough;

//========Tests===================================================================================
//=== Using factory patterns to create the different physical and non-physical components=========

// Test a standard trough loop at a single point in time
NAMESPACE_TEST(csp_trough, TroughLoop, DefaultTest)
{
    DefaultTroughFactory default_trough_factory = DefaultTroughFactory();
    Location location = default_trough_factory.MakeLocation();
    std::unique_ptr<Trough> trough = default_trough_factory.MakeTrough(location);
    std::unique_ptr<TroughState> trough_state = default_trough_factory.MakeTroughState();
    TroughFactory::SetTroughState(trough.get(), trough_state.get());

    std::unique_ptr<TimeAndWeather> time_and_weather = default_trough_factory.MakeTimeLocationWeather(location);
    FluidInletState fluid_inlet_state = default_trough_factory.MakeInletState();
    double defocus = default_trough_factory.MakeDefocus();
    TroughOutputs trough_outputs;
    TimestepAndTou timestep_and_tou = default_trough_factory.MakeTimestepAndTou();

    trough->on(*time_and_weather, fluid_inlet_state, defocus, trough_outputs, timestep_and_tou);

    EXPECT_NEAR(trough_outputs.m_T_salt_hot, 391.17, 391.17 * kErrorToleranceLo);
    EXPECT_NEAR(trough_outputs.m_m_dot_salt_tot, 6568369, 6568369 * kErrorToleranceLo);
}

// Test a standard trough loop from a homogenous initial condition to steady-state
NAMESPACE_TEST(csp_trough, TroughLoop, SteadyStateTest)
{
    DefaultTroughFactory default_trough_factory = DefaultTroughFactory();
    Location location = default_trough_factory.MakeLocation();
    std::unique_ptr<Trough> trough = default_trough_factory.MakeTrough(location);
    std::unique_ptr<TroughState> trough_state = default_trough_factory.MakeTroughState();
    TroughFactory::SetTroughState(trough.get(), trough_state.get());

    std::unique_ptr<TimeAndWeather> time_and_weather = default_trough_factory.MakeTimeLocationWeather(location);
    FluidInletState fluid_inlet_state = default_trough_factory.MakeInletState();
    double defocus = default_trough_factory.MakeDefocus();
    TroughOutputs trough_outputs;
    TimestepAndTou timestep_and_tou = default_trough_factory.MakeTimestepAndTou();

    trough->m_accept_mode = 1;               // flag so solar zenith from weather is used instead of calc'd
    trough->m_accept_init = false;           // running at steady-state but keeping false to avoid side effects
    trough->m_accept_loc = 1;                // don't just model a single loop
    trough->m_is_using_input_gen = false;    // use parameter values set below instead

    // Design DNI and normal incidence angle
    time_and_weather->m_beam = trough->m_I_bn_des;
    time_and_weather->m_solazi = trough->m_ColAz;
    time_and_weather->m_solzen = trough->m_ColTilt;

    // 5-minute timesteps
    timestep_and_tou.ms_ts.m_step = 5. * 60.;
    timestep_and_tou.ms_ts.m_time = timestep_and_tou.ms_ts.m_time_start + timestep_and_tou.ms_ts.m_step;

    // Initially set all fluid to design temperature
    fluid_inlet_state.m_temp = trough->m_T_loop_in_des - 273.15;
    trough->m_T_sys_c_t_end_converged = trough->m_T_loop_in_des;
    trough->m_T_sys_h_t_end_converged = trough->m_T_loop_in_des;
    trough->m_T_htf_out_t_end_converged.assign(trough->m_nSCA, trough->m_T_loop_in_des);

    // Values for checking whether steady-state
    double ss_diff = std::numeric_limits<double>::quiet_NaN();
    const double tol = 0.05;
    std::vector<double> T_htf_in_t_int_prev = trough->m_T_htf_in_t_int;
    std::vector<double> T_htf_out_t_int_prev = trough->m_T_htf_out_t_int;
    double minutes2SS = 0.;

    do
    {
        trough->on(*time_and_weather, fluid_inlet_state, defocus, trough_outputs, timestep_and_tou);

        // Calculate metric for deciding whether steady-state is reached
        ss_diff = 0.;
        for (int i = 0; i < trough->m_nSCA; i++) {
            ss_diff += fabs(trough->m_T_htf_in_t_int[i] - T_htf_in_t_int_prev[i]) +
                fabs(trough->m_T_htf_out_t_int[i] - T_htf_out_t_int_prev[i]);
        }

        // Set converged values so reset_last_temps() propagates the temps in time
        trough->m_T_sys_c_t_end_converged = trough->m_T_sys_c_t_end;
        trough->m_T_sys_h_t_end_converged = trough->m_T_sys_h_t_end;
        // SCA temperatures - these set the values of m_T_htf_out_t_end_last[i]
        trough->m_T_htf_out_t_end_converged = trough->m_T_htf_out_t_end;

        // Update 'last' values
        T_htf_in_t_int_prev = trough->m_T_htf_in_t_int;
        T_htf_out_t_int_prev = trough->m_T_htf_out_t_int;

        minutes2SS += timestep_and_tou.ms_ts.m_step / 60.;

    } while (ss_diff / 200. > tol);

    EXPECT_NEAR(trough->m_T_sys_h_t_end, 656.1, 656.1 * kErrorToleranceLo);     // final loop outlet temperature
    EXPECT_NEAR(minutes2SS, 35., 35. * kErrorToleranceLo);                      // time to steady-state
}
//========/Tests==================================================================================

//========Factories:==============================================================================
//========TroughFactory (super class)=============================================================
std::unique_ptr<Trough> TroughFactory::MakeTrough(TroughSpecifications* trough_specifications,
    Location location) const
{
    auto trough = std::unique_ptr<Trough>(new Trough);

    trough->m_nSCA = trough_specifications->nSCA;
    trough->m_nHCEt = trough_specifications->nHCEt;
    trough->m_nColt = trough_specifications->nColt;
    trough->m_nHCEVar = trough_specifications->nHCEVar;
    trough->m_nLoops = trough_specifications->nLoops;
    trough->m_FieldConfig = trough_specifications->FieldConfig;
    trough->m_L_power_block_piping = trough_specifications->L_power_block_piping;
    trough->m_include_fixed_power_block_runner = trough_specifications->include_fixed_power_block_runner;
    trough->m_eta_pump = trough_specifications->eta_pump;
    trough->m_Fluid = trough_specifications->Fluid;
    //trough->m_fthrok = trough_specifications->fthrok;
    trough->m_fthrctrl = trough_specifications->fthrctrl;
    trough->m_accept_loc = trough_specifications->accept_loc;
    trough->m_HDR_rough = trough_specifications->HDR_rough;
    trough->m_theta_stow = trough_specifications->theta_stow;
    trough->m_theta_dep = trough_specifications->theta_dep;
    trough->m_Row_Distance = trough_specifications->Row_Distance;

    trough->m_T_loop_in_des = trough_specifications->T_loop_in_des;
    trough->m_T_loop_out_des = trough_specifications->T_loop_out_des;
    trough->m_T_startup = trough_specifications->T_startup;
    trough->m_m_dot_htfmin = trough_specifications->m_dot_htfmin;
    trough->m_m_dot_htfmax = trough_specifications->m_dot_htfmax;
    trough->m_field_fl_props = trough_specifications->field_fl_props;
    trough->m_T_fp = trough_specifications->T_fp;
    trough->m_I_bn_des = trough_specifications->I_bn_des;
    trough->m_V_hdr_cold_max = trough_specifications->V_hdr_cold_max;
    trough->m_V_hdr_cold_min = trough_specifications->V_hdr_cold_min;
    trough->m_V_hdr_hot_max = trough_specifications->V_hdr_hot_max;
    trough->m_V_hdr_hot_min = trough_specifications->V_hdr_hot_min;
    trough->m_V_hdr_max = trough_specifications->V_hdr_max;
    trough->m_V_hdr_min = trough_specifications->V_hdr_min;
    trough->m_Pipe_hl_coef = trough_specifications->Pipe_hl_coef;
    trough->m_SCA_drives_elec = trough_specifications->SCA_drives_elec;
    trough->m_ColTilt = trough_specifications->ColTilt;
    trough->m_ColAz = trough_specifications->ColAz;
    trough->m_wind_stow_speed = trough_specifications->wind_stow_speed;
    trough->m_accept_mode = trough_specifications->accept_mode;
    trough->m_accept_init = trough_specifications->accept_init;
    trough->m_solar_mult = trough_specifications->solar_mult;
    trough->m_mc_bal_hot_per_MW = trough_specifications->mc_bal_hot_per_MW;
    trough->m_mc_bal_cold_per_MW = trough_specifications->mc_bal_cold_per_MW;
    trough->m_mc_bal_sca = trough_specifications->mc_bal_sca;

    trough->m_W_aperture = trough_specifications->W_aperture;
    trough->m_A_aperture = trough_specifications->A_aperture;
    trough->m_TrackingError = trough_specifications->TrackingError;
    trough->m_GeomEffects = trough_specifications->GeomEffects;
    trough->m_Rho_mirror_clean = trough_specifications->Rho_mirror_clean;
    trough->m_Dirt_mirror = trough_specifications->Dirt_mirror;
    trough->m_Error = trough_specifications->Error;
    trough->m_Ave_Focal_Length = trough_specifications->Ave_Focal_Length;
    trough->m_L_SCA = trough_specifications->L_SCA;
    trough->m_L_aperture = trough_specifications->L_aperture;
    trough->m_ColperSCA = trough_specifications->ColperSCA;
    trough->m_Distance_SCA = trough_specifications->Distance_SCA;

    trough->m_IAM_matrix = trough_specifications->IAM_matrix;
    trough->m_HCE_FieldFrac = trough_specifications->HCE_FieldFrac;
    trough->m_D_2 = trough_specifications->D_2;
    trough->m_D_3 = trough_specifications->D_3;
    trough->m_D_4 = trough_specifications->D_4;
    trough->m_D_5 = trough_specifications->D_5;
    trough->m_D_p = trough_specifications->D_p;
    trough->m_Flow_type = trough_specifications->Flow_type;
    trough->m_Rough = trough_specifications->Rough;
    trough->m_alpha_env = trough_specifications->alpha_env;

    trough->m_epsilon_3_11 = trough_specifications->epsilon_3_11;
    trough->m_epsilon_3_12 = trough_specifications->epsilon_3_12;
    trough->m_epsilon_3_13 = trough_specifications->epsilon_3_13;
    trough->m_epsilon_3_14 = trough_specifications->epsilon_3_14;
    trough->m_epsilon_3_21 = trough_specifications->epsilon_3_21;
    trough->m_epsilon_3_22 = trough_specifications->epsilon_3_22;
    trough->m_epsilon_3_23 = trough_specifications->epsilon_3_23;
    trough->m_epsilon_3_24 = trough_specifications->epsilon_3_24;
    trough->m_epsilon_3_31 = trough_specifications->epsilon_3_31;
    trough->m_epsilon_3_32 = trough_specifications->epsilon_3_32;
    trough->m_epsilon_3_33 = trough_specifications->epsilon_3_33;
    trough->m_epsilon_3_34 = trough_specifications->epsilon_3_34;
    trough->m_epsilon_3_41 = trough_specifications->epsilon_3_41;
    trough->m_epsilon_3_42 = trough_specifications->epsilon_3_42;
    trough->m_epsilon_3_43 = trough_specifications->epsilon_3_43;
    trough->m_epsilon_3_44 = trough_specifications->epsilon_3_44;

    trough->m_alpha_abs = trough_specifications->alpha_abs;
    trough->m_Tau_envelope = trough_specifications->Tau_envelope;
    trough->m_EPSILON_4 = trough_specifications->EPSILON_4;
    trough->m_EPSILON_5 = trough_specifications->EPSILON_5;
    trough->m_GlazingIntact = trough_specifications->GlazingIntact;
    trough->m_P_a = trough_specifications->P_a;
    trough->m_AnnulusGas = trough_specifications->AnnulusGas;
    trough->m_AbsorberMaterial = trough_specifications->AbsorberMaterial;
    trough->m_Shadowing = trough_specifications->Shadowing;
    trough->m_Dirt_HCE = trough_specifications->Dirt_HCE;
    trough->m_Design_loss = trough_specifications->Design_loss;
    trough->m_SCAInfoArray = trough_specifications->SCAInfoArray;

    trough->m_calc_design_pipe_vals = trough_specifications->calc_design_pipe_vals;
    trough->m_L_rnr_pb = trough_specifications->L_rnr_pb;
    trough->m_N_max_hdr_diams = trough_specifications->N_max_hdr_diams;
    trough->m_L_rnr_per_xpan = trough_specifications->L_rnr_per_xpan;
    trough->m_L_xpan_hdr = trough_specifications->L_xpan_hdr;
    trough->m_L_xpan_rnr = trough_specifications->L_xpan_rnr;
    trough->m_Min_rnr_xpans = trough_specifications->Min_rnr_xpans;
    trough->m_northsouth_field_sep = trough_specifications->northsouth_field_sep;
    trough->m_N_hdr_per_xpan = trough_specifications->N_hdr_per_xpan;
    trough->m_K_cpnt = trough_specifications->K_cpnt;
    trough->m_D_cpnt = trough_specifications->D_cpnt;
    trough->m_L_cpnt = trough_specifications->L_cpnt;
    trough->m_Type_cpnt = trough_specifications->Type_cpnt;
    trough->m_custom_sf_pipe_sizes = trough_specifications->custom_sf_pipe_sizes;
    trough->m_sf_rnr_diams = trough_specifications->sf_rnr_diams;
    trough->m_sf_rnr_wallthicks = trough_specifications->sf_rnr_wallthicks;
    trough->m_sf_rnr_lengths = trough_specifications->sf_rnr_lengths;
    trough->m_sf_hdr_diams = trough_specifications->sf_hdr_diams;
    trough->m_sf_hdr_wallthicks = trough_specifications->sf_hdr_wallthicks;
    trough->m_sf_hdr_lengths = trough_specifications->sf_hdr_lengths;

    TroughSolvedParams trough_solved_params;
    trough->init(location, trough_solved_params);

    return trough;
}

// Set state by assigning previous (converged) trough HTF temperatures
void TroughFactory::SetTroughState(Trough* trough, TroughState* trough_state)
{
    std::size_t N_scas_trough = trough->m_T_htf_out_t_end_converged.size();
    std::size_t N_scas_state = trough_state->T_out_SCAs_prev.size();
    if (N_scas_trough != N_scas_state) {
        throw "Incorrect trough state array length.";
    }

    trough->m_T_sys_c_t_end_converged = trough_state->T_in_loop_prev;       // this ends up setting m_T_sys_c_t_end_last
    trough->m_T_sys_h_t_end_converged = trough_state->T_out_loop_prev;      // this ends up setting m_T_sys_h_t_end_last

    // SCA temperatures - these end up setting m_T_htf_out_t_end_last[i]
    for (std::vector<int>::size_type i = 0; i != trough_state->T_out_SCAs_prev.size(); i++) {
        trough->m_T_htf_out_t_end_converged[i] = trough_state->T_out_SCAs_prev[i];
    }
}
//========/TroughFactory==========================================================================

//========DefaultTroughFactory (subclass)=========================================================
std::unique_ptr<Trough> DefaultTroughFactory::MakeTrough(Location location) const
{
    std::unique_ptr<TroughSpecifications> trough_specifications = this->MakeSpecifications();
    std::unique_ptr<Trough> trough = TroughFactory::MakeTrough(trough_specifications.get(),
        location);
    return trough;
}

std::unique_ptr<TroughSpecifications> DefaultTroughFactory::MakeSpecifications() const
{
    auto trough_specifications = std::unique_ptr<TroughSpecifications>(new TroughSpecifications);
    trough_specifications->nSCA = 8;
    trough_specifications->nHCEt = 4;
    trough_specifications->nColt = 4;
    trough_specifications->nHCEVar = 4;
    trough_specifications->nLoops = 181;
    trough_specifications->FieldConfig = 2;
    trough_specifications->L_power_block_piping = 50.;
    trough_specifications->include_fixed_power_block_runner = true;
    trough_specifications->eta_pump = 0.85;
    trough_specifications->Fluid = 21;
    trough_specifications->fthrctrl = 2;
    trough_specifications->accept_loc = 1;
    trough_specifications->HDR_rough = 4.57e-5;
    trough_specifications->theta_stow = 170.;
    trough_specifications->theta_dep = 10.;
    trough_specifications->Row_Distance = 15.;

    trough_specifications->T_loop_in_des = 293.;
    trough_specifications->T_loop_out_des = 391.;
    trough_specifications->T_startup = 0.67 * trough_specifications->T_loop_in_des + 0.33 * trough_specifications->T_loop_out_des; //[C]
    trough_specifications->m_dot_htfmin = 1.;
    trough_specifications->m_dot_htfmax = 12.;
    double vals[] = { 0 };
    trough_specifications->field_fl_props.assign(vals, 1, 1);
    trough_specifications->T_fp = 150.;
    trough_specifications->I_bn_des = 950.;
    trough_specifications->V_hdr_cold_max = 3.;
    trough_specifications->V_hdr_cold_min = 2.;
    trough_specifications->V_hdr_hot_max = 3.;
    trough_specifications->V_hdr_hot_min = 2.;
    trough_specifications->V_hdr_max = std::min(trough_specifications->V_hdr_cold_max, trough_specifications->V_hdr_hot_max);
    trough_specifications->V_hdr_min = std::max(trough_specifications->V_hdr_cold_min, trough_specifications->V_hdr_hot_min);
    trough_specifications->Pipe_hl_coef = 0.45;
    trough_specifications->SCA_drives_elec = 125.;
    trough_specifications->ColTilt = 0.;
    trough_specifications->ColAz = 0.;
    trough_specifications->wind_stow_speed = 25.;
    trough_specifications->accept_mode = 0;
    trough_specifications->accept_init = false;
    trough_specifications->solar_mult = 2.;
    trough_specifications->mc_bal_hot_per_MW = 0.2;
    trough_specifications->mc_bal_cold_per_MW = 0.2;
    trough_specifications->mc_bal_sca = 4.5;

    trough_specifications->W_aperture = { 6, 6, 6, 6 };
    trough_specifications->A_aperture = { 656, 656, 656, 656 };
    trough_specifications->TrackingError = { 0.988, 0.988, 0.988, 0.988 };
    trough_specifications->GeomEffects = { 0.952, 0.952, 0.952, 0.952 };
    trough_specifications->Rho_mirror_clean = { 0.93, 0.93, 0.93, 0.93 };
    trough_specifications->Dirt_mirror = { 0.97, 0.97, 0.97, 0.97 };
    trough_specifications->Error = { 1., 1., 1., 1. };
    trough_specifications->Ave_Focal_Length = { 2.15, 2.15, 2.15, 2.15 };
    trough_specifications->L_SCA = { 115., 115., 115., 115. };
    trough_specifications->L_aperture = { 14.375, 14.375, 14.375, 14.375 };
    trough_specifications->ColperSCA = { 8., 8., 8., 8. };
    trough_specifications->Distance_SCA = { 1., 1., 1., 1. };

    double vals2[] = {
        1, 0.0327, -0.1351,
        1, 0.0327, -0.1351,
        1, 0.0327, -0.1351,
        1, 0.0327, -0.1351 };
    trough_specifications->IAM_matrix.assign(vals2, 4, 3);

    double vals3[] = {
        0.985, 0.01, 0.005, 0.,
        1., 0., 0., 0.,
        1., 0., 0., 0.,
        1., 0., 0., 0. };
    trough_specifications->HCE_FieldFrac.assign(vals3, 4, 4);

    double vals4[] = {
        0.076, 0.076, 0.076, 0.076,
        0.076, 0.076, 0.076, 0.076,
        0.076, 0.076, 0.076, 0.076,
        0.076, 0.076, 0.076, 0.076 };
    trough_specifications->D_2.assign(vals4, 4, 4);

    double vals5[] = {
        0.08, 0.08, 0.08, 0.08,
        0.08, 0.08, 0.08, 0.08,
        0.08, 0.08, 0.08, 0.08,
        0.08, 0.08, 0.08, 0.08 };
    trough_specifications->D_3.assign(vals5, 4, 4);

    double vals6[] = {
        0.115, 0.115, 0.115, 0.115,
        0.115, 0.115, 0.115, 0.115,
        0.115, 0.115, 0.115, 0.115,
        0.115, 0.115, 0.115, 0.115 };
    trough_specifications->D_4.assign(vals6, 4, 4);

    double vals7[] = {
        0.12, 0.12, 0.12, 0.12,
        0.12, 0.12, 0.12, 0.12,
        0.12, 0.12, 0.12, 0.12,
        0.12, 0.12, 0.12, 0.12 };
    trough_specifications->D_5.assign(vals7, 4, 4);

    double vals8[] = {
        0., 0., 0., 0.,
        0., 0., 0., 0.,
        0., 0., 0., 0.,
        0., 0., 0., 0. };
    trough_specifications->D_p.assign(vals8, 4, 4);

    double vals9[] = {
        1., 1., 1., 1.,
        1., 1., 1., 1.,
        1., 1., 1., 1.,
        1., 1., 1., 1. };
    trough_specifications->Flow_type.assign(vals9, 4, 4);

    double vals10[] = {
        4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
        4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
        4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
        4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5 };
    trough_specifications->Rough.assign(vals10, 4, 4);

    double vals11[] = {
        0.02, 0.02, 0., 0.,
        0.02, 0.02, 0., 0.,
        0.02, 0.02, 0., 0.,
        0.02, 0.02, 0., 0. };
    trough_specifications->alpha_env.assign(vals11, 4, 4);

    double vals12[] = {
        100., 150., 200., 250., 300., 350., 400., 450., 500.,
        0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
    trough_specifications->epsilon_3_11.assign(vals12, 2, 9);

    double vals13[] = { 0.65 };
    trough_specifications->epsilon_3_12.assign(vals13, 1, 1);

    double vals14[] = { 0.65 };
    trough_specifications->epsilon_3_13.assign(vals14, 1, 1);

    double vals15[] = { 0. };
    trough_specifications->epsilon_3_14.assign(vals15, 0, 0);

    double vals16[] = {
        100., 150., 200., 250., 300., 350., 400., 450., 500.,
        0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
    trough_specifications->epsilon_3_21.assign(vals16, 2, 9);

    double vals17[] = { 0.65 };
    trough_specifications->epsilon_3_22.assign(vals17, 1, 1);

    double vals18[] = { 0.65 };
    trough_specifications->epsilon_3_23.assign(vals18, 1, 1);

    double vals19[] = { 0. };
    trough_specifications->epsilon_3_24.assign(vals19, 1, 1);

    double vals20[] = {
        100., 150., 200., 250., 300., 350., 400., 450., 500.,
        0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
    trough_specifications->epsilon_3_31.assign(vals20, 2, 9);

    double vals21[] = { 0.65 };
    trough_specifications->epsilon_3_32.assign(vals21, 1, 1);

    double vals22[] = { 0.65 };
    trough_specifications->epsilon_3_33.assign(vals22, 1, 1);

    double vals23[] = { 0. };
    trough_specifications->epsilon_3_34.assign(vals23, 1, 1);

    double vals24[] = {
        100., 150., 200., 250., 300., 350., 400., 450., 500.,
        0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
    trough_specifications->epsilon_3_41.assign(vals24, 2, 9);

    double vals25[] = { 0.65 };
    trough_specifications->epsilon_3_42.assign(vals25, 1, 1);

    double vals26[] = { 0.65 };
    trough_specifications->epsilon_3_43.assign(vals26, 1, 1);

    double vals27[] = { 0. };
    trough_specifications->epsilon_3_44.assign(vals27, 1, 1);

    double vals28[] = {
        0.963, 0.963, 0.8, 0.,
        0.963, 0.963, 0.8, 0.,
        0.963, 0.963, 0.8, 0.,
        0.963, 0.963, 0.8, 0. };
    trough_specifications->alpha_abs.assign(vals28, 4, 4);

    double vals29[] = {
        0.964, 0.964, 1., 0.,
        0.964, 0.964, 1., 0.,
        0.964, 0.964, 1., 0.,
        0.964, 0.964, 1., 0. };
    trough_specifications->Tau_envelope.assign(vals29, 4, 4);

    double vals30[] = {
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0. };
    trough_specifications->EPSILON_4.assign(vals30, 4, 4);

    double vals31[] = {
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0.,
        0.86, 0.86, 1., 0. };
    trough_specifications->EPSILON_5.assign(vals31, 4, 4);

    double vals32[] = {
        1., 1., 0., 1.,
        1., 1., 0., 1.,
        1., 1., 0., 1.,
        1., 1., 0., 1. };

    trough_specifications->GlazingIntact_dbl.assign(vals32, 4, 4);
    // convert <double> to <bool>
    int n_gl_row = (int)trough_specifications->GlazingIntact_dbl.nrows();
    int n_gl_col = (int)trough_specifications->GlazingIntact_dbl.ncols();
    trough_specifications->GlazingIntact.resize(n_gl_row, n_gl_col);
    for (int i = 0; i < n_gl_row; i++) {
        for (int j = 0; j < n_gl_col; j++) {
            trough_specifications->GlazingIntact(i, j) = (trough_specifications->GlazingIntact_dbl(i, j) > 0);
        }
    }

    double vals33[] = {
        1.e-4, 750., 750., 0.,
        1.e-4, 750., 750., 0.,
        1.e-4, 750., 750., 0.,
        1.e-4, 750., 750., 0., };
    trough_specifications->P_a.assign(vals33, 4, 4);

    double vals34[] = {
        27., 1., 1., 27.,
        27., 1., 1., 27.,
        27., 1., 1., 27.,
        27., 1., 1., 27., };
    trough_specifications->AnnulusGas.assign(vals34, 4, 4);

    double vals35[] = {
        1., 1., 1., 1.,
        1., 1., 1., 1.,
        1., 1., 1., 1.,
        1., 1., 1., 1., };
    trough_specifications->AbsorberMaterial.assign(vals35, 4, 4);

    double vals36[] = {
        0.935, 0.935, 0.935, 0.963,
        0.935, 0.935, 0.935, 0.963,
        0.935, 0.935, 0.935, 0.963,
        0.935, 0.935, 0.935, 0.963 };
    trough_specifications->Shadowing.assign(vals36, 4, 4);

    double vals37[] = {
        0.98, 0.98, 1., 0.98,
        0.98, 0.98, 1., 0.98,
        0.98, 0.98, 1., 0.98,
        0.98, 0.98, 1., 0.98, };
    trough_specifications->Dirt_HCE.assign(vals37, 4, 4);

    double vals38[] = {
        190., 1270., 1500., 0.,
        190., 1270., 1500., 0.,
        190., 1270., 1500., 0.,
        190., 1270., 1500., 0. };
    trough_specifications->Design_loss.assign(vals38, 4, 4);

    double vals39[] = {
        1., 1.,
        1., 1.,
        1., 1.,
        1., 1.,
        1., 1.,
        1., 1.,
        1., 1.,
        1., 1. };
    trough_specifications->SCAInfoArray.assign(vals39, 8, 2);

    trough_specifications->calc_design_pipe_vals = true;
    trough_specifications->L_rnr_pb = 25.;
    trough_specifications->N_max_hdr_diams = 10.;
    trough_specifications->L_rnr_per_xpan = 70.;
    trough_specifications->L_xpan_hdr = 20.;
    trough_specifications->L_xpan_rnr = 20.;
    trough_specifications->Min_rnr_xpans = 1.;
    trough_specifications->northsouth_field_sep = 20.;
    trough_specifications->N_hdr_per_xpan = 2.;

    double vals40[] = {
        0.9, 0., 0.19, 0., 0.9, -1., -1., -1., -1., -1., -1.,
        0., 0.6, 0.05, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
        0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.15, 0.6, 0.,
        0.9, 0., 0.19, 0., 0.9, -1., -1., -1., -1., -1., -1. };
    trough_specifications->K_cpnt.assign(vals40, 11, 11);

    double vals41[] = {
        0.085, 0.0635, 0.085, 0.0635, 0.085, -1., -1., -1., -1., -1., -1.,
        0.085, 0.085, 0.085, 0.0635, 0.0635, 0.0635,0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
        0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635,0.0635, 0.0635, 0.085, 0.085, 0.085,
        0.085, 0.0635, 0.085, 0.0635, 0.085, -1., -1., -1., -1., -1., -1. };
    trough_specifications->D_cpnt.assign(vals41, 11, 11);

    double vals42[] = {
        0., 0., 0., 0., 0., -1., -1., -1., -1., -1., -1.,
        0., 0., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
        0., 1., 0., 1., 0., 0., 0., 1., 0., 0., 0.,
        0., 0., 0., 0., 0., -1., -1., -1., -1., -1., -1. };
    trough_specifications->L_cpnt.assign(vals42, 11, 11);

    double vals43[] = {
        0., 1., 0., 1., 0., -1., -1., -1., -1., -1., -1.,
        1., 0., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
        0., 2., 0., 2., 0., 1., 0., 2., 0., 0., 1.,
        0., 1., 0., 1., 0., -1., -1., -1., -1., -1., -1. };
    trough_specifications->Type_cpnt.assign(vals43, 11, 11);

    trough_specifications->custom_sf_pipe_sizes = false;

    double vals44[] = { -1. };
    trough_specifications->sf_rnr_diams.assign(vals44, 1, 1);

    double vals45[] = { -1 };
    trough_specifications->sf_rnr_wallthicks.assign(vals45, 1, 1);

    double vals46[] = { -1 };
    trough_specifications->sf_rnr_lengths.assign(vals46, 1, 1);

    double vals47[] = { -1 };
    trough_specifications->sf_hdr_diams.assign(vals47, 1, 1);

    double vals48[] = { -1 };
    trough_specifications->sf_hdr_wallthicks.assign(vals48, 1, 1);

    double vals49[] = { -1 };
    trough_specifications->sf_hdr_lengths.assign(vals49, 1, 1);

    return trough_specifications;
}

std::unique_ptr<TroughState> DefaultTroughFactory::MakeTroughState() const
{
    auto trough_state = std::unique_ptr<TroughState>(new TroughState);
    trough_state->T_in_loop_prev = 574.6;
    trough_state->T_out_loop_prev = 664.5;
    trough_state->T_out_SCAs_prev.push_back(586.5);
    trough_state->T_out_SCAs_prev.push_back(598.4);
    trough_state->T_out_SCAs_prev.push_back(610.0);
    trough_state->T_out_SCAs_prev.push_back(621.4);
    trough_state->T_out_SCAs_prev.push_back(632.5);
    trough_state->T_out_SCAs_prev.push_back(643.4);
    trough_state->T_out_SCAs_prev.push_back(654.1);
    trough_state->T_out_SCAs_prev.push_back(664.5);

    return trough_state;
}

std::unique_ptr<TimeAndWeather> DefaultTroughFactory::MakeTimeLocationWeather(Location location) const
{
    auto time_and_weather = std::unique_ptr<TimeAndWeather>(new TimeAndWeather);
    time_and_weather->m_year = 2009;
    time_and_weather->m_month = 2;
    time_and_weather->m_day = 14;
    time_and_weather->m_hour = 12;
    time_and_weather->m_minute = 0;
    time_and_weather->m_beam = 1016.0;
    time_and_weather->m_tdry = 16.0;
    time_and_weather->m_tdew = -14.0;
    time_and_weather->m_wspd = 1.2;
    time_and_weather->m_pres = 920.0;
    time_and_weather->m_solazi = 167.06;
    time_and_weather->m_solzen = 45.79;
    time_and_weather->m_lat = location.m_latitude;
    time_and_weather->m_lon = location.m_longitude;
    time_and_weather->m_tz = location.m_tz;
    time_and_weather->m_shift = location.m_shift;
    time_and_weather->m_elev = location.m_elev;
    time_and_weather->m_global =    std::numeric_limits<double>::quiet_NaN();
    time_and_weather->m_hor_beam =  std::numeric_limits<double>::quiet_NaN();    // 433.1
    time_and_weather->m_diffuse =   std::numeric_limits<double>::quiet_NaN();    // 282
    time_and_weather->m_twet =      std::numeric_limits<double>::quiet_NaN();
    time_and_weather->m_wdir =      std::numeric_limits<double>::quiet_NaN();    // 88
    time_and_weather->m_rhum =      std::numeric_limits<double>::quiet_NaN();
    time_and_weather->m_snow =      std::numeric_limits<double>::quiet_NaN();
    time_and_weather->m_albedo =    std::numeric_limits<double>::quiet_NaN();    // 0.213
    time_and_weather->m_aod =       std::numeric_limits<double>::quiet_NaN();
    time_and_weather->m_poa =       std::numeric_limits<double>::quiet_NaN();    // 715.1
    time_and_weather->m_time_rise = std::numeric_limits<double>::quiet_NaN();    // 7.486443134
    time_and_weather->m_time_set =  std::numeric_limits<double>::quiet_NaN();    // 17.46109472

    return time_and_weather;
}

TimestepAndTou DefaultTroughFactory::MakeTimestepAndTou() const
{
    TimestepAndTou timestep_and_tou;
    timestep_and_tou.ms_ts.m_time_start = 3844800.;
    timestep_and_tou.ms_ts.m_step = 3600.;
    timestep_and_tou.ms_ts.m_time = timestep_and_tou.ms_ts.m_time_start + timestep_and_tou.ms_ts.m_step;
    timestep_and_tou.m_tou = 1.;
    return timestep_and_tou;
}

FluidInletState DefaultTroughFactory::MakeInletState() const
{
    FluidInletState fluid_inlet_state;
    fluid_inlet_state.m_temp = 296.5;
    fluid_inlet_state.m_pres = std::numeric_limits<double>::quiet_NaN();
    fluid_inlet_state.m_qual = -1.;
    fluid_inlet_state.m_m_dot = std::numeric_limits<double>::quiet_NaN();
    return fluid_inlet_state;
}

double DefaultTroughFactory::MakeDefocus() const
{
    return 1.;
}

Location DefaultTroughFactory::MakeLocation() const
{
    Location location;
    location.m_latitude = 32.13000107;
    location.m_longitude = -110.9400024;
    location.m_tz = -7;
    location.m_shift = -5.940002441;
    location.m_elev = -773;
    return location;
}
//========/DefaultTroughFactory===================================================================
