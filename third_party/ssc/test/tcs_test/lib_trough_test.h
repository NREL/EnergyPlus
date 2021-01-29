#ifndef __LIB_TROUGH_TEST_H__
#define __LIB_TROUGH_TEST_H__

#include <gtest/gtest.h>
#include "csp_solver_trough_collector_receiver.h"

#include "lib_trough_properties.h"

class TroughTest : public TroughProperties
{
protected:
    C_csp_trough_collector_receiver *troughModel;
    C_csp_collector_receiver::S_csp_cr_init_inputs troughInitInputs;            // for init()
    C_csp_collector_receiver::S_csp_cr_solved_params troughSolvedParams;        // for init()

    C_csp_weatherreader::S_outputs weatherValues;
    C_csp_solver_htf_1state htfInletState;
    double defocus;
    C_csp_solver_sim_info troughInfo;
    C_csp_collector_receiver::S_csp_cr_out_solver troughOutputs;

public:
    double m_error_tolerance_lo = 0.001;    // 0.1%
    double m_error_tolerance_hi = 0.01;     // 1.0%

    void SetUp() {
        TroughProperties::SetUp();
        troughModel = new C_csp_trough_collector_receiver();

        troughModel->m_nSCA = nSCA;
        troughModel->m_nHCEt = nHCEt;
        troughModel->m_nColt = nColt;
        troughModel->m_nHCEVar = nHCEVar;
        troughModel->m_nLoops = nLoops;
        troughModel->m_FieldConfig = FieldConfig;
        troughModel->m_L_power_block_piping = L_power_block_piping;
        troughModel->m_include_fixed_power_block_runner = include_fixed_power_block_runner;
        troughModel->m_eta_pump = eta_pump;
        troughModel->m_Fluid = Fluid;
        //troughModel->m_fthrok = fthrok;
        troughModel->m_fthrctrl = fthrctrl;
        troughModel->m_accept_loc = accept_loc;
        troughModel->m_HDR_rough = HDR_rough;
        troughModel->m_theta_stow = theta_stow;
        troughModel->m_theta_dep = theta_dep;
        troughModel->m_Row_Distance = Row_Distance;

        troughModel->m_T_loop_in_des = T_loop_in_des;
        troughModel->m_T_loop_out_des = T_loop_out_des;
        troughModel->m_T_startup = T_startup;
        troughModel->m_m_dot_htfmin = m_dot_htfmin;
        troughModel->m_m_dot_htfmax = m_dot_htfmax;
        troughModel->m_field_fl_props = field_fl_props;
        troughModel->m_T_fp = T_fp;
        troughModel->m_I_bn_des = I_bn_des;
        troughModel->m_V_hdr_cold_max = V_hdr_cold_max;
        troughModel->m_V_hdr_cold_min = V_hdr_cold_min;
        troughModel->m_V_hdr_hot_max = V_hdr_hot_max;
        troughModel->m_V_hdr_hot_min = V_hdr_hot_min;
        troughModel->m_V_hdr_max = V_hdr_max;
        troughModel->m_V_hdr_min = V_hdr_min;
        troughModel->m_Pipe_hl_coef = Pipe_hl_coef;
        troughModel->m_SCA_drives_elec = SCA_drives_elec;
        troughModel->m_ColTilt = ColTilt;
        troughModel->m_ColAz = ColAz;
        troughModel->m_wind_stow_speed = wind_stow_speed;
        troughModel->m_accept_mode = accept_mode;
        troughModel->m_accept_init = accept_init;
        troughModel->m_solar_mult = solar_mult;
        troughModel->m_mc_bal_hot_per_MW = mc_bal_hot_per_MW;
        troughModel->m_mc_bal_cold_per_MW = mc_bal_cold_per_MW;
        troughModel->m_mc_bal_sca = mc_bal_sca;

        troughModel->m_W_aperture = W_aperture;
        troughModel->m_A_aperture = A_aperture;
        troughModel->m_TrackingError = TrackingError;
        troughModel->m_GeomEffects = GeomEffects;
        troughModel->m_Rho_mirror_clean = Rho_mirror_clean;
        troughModel->m_Dirt_mirror = Dirt_mirror;
        troughModel->m_Error = Error;
        troughModel->m_Ave_Focal_Length = Ave_Focal_Length;
        troughModel->m_L_SCA = L_SCA;
        troughModel->m_L_aperture = L_aperture;
        troughModel->m_ColperSCA = ColperSCA;
        troughModel->m_Distance_SCA = Distance_SCA;

        troughModel->m_IAM_matrix = IAM_matrix;
        troughModel->m_HCE_FieldFrac = HCE_FieldFrac;
        troughModel->m_D_2 = D_2;
        troughModel->m_D_3 = D_3;
        troughModel->m_D_4 = D_4;
        troughModel->m_D_5 = D_5;
        troughModel->m_D_p = D_p;
        troughModel->m_Flow_type = Flow_type;
        troughModel->m_Rough = Rough;
        troughModel->m_alpha_env = alpha_env;

        troughModel->m_epsilon_3_11 = epsilon_3_11;
        troughModel->m_epsilon_3_12 = epsilon_3_12;
        troughModel->m_epsilon_3_13 = epsilon_3_13;
        troughModel->m_epsilon_3_14 = epsilon_3_14;
        troughModel->m_epsilon_3_21 = epsilon_3_21;
        troughModel->m_epsilon_3_22 = epsilon_3_22;
        troughModel->m_epsilon_3_23 = epsilon_3_23;
        troughModel->m_epsilon_3_24 = epsilon_3_24;
        troughModel->m_epsilon_3_31 = epsilon_3_31;
        troughModel->m_epsilon_3_32 = epsilon_3_32;
        troughModel->m_epsilon_3_33 = epsilon_3_33;
        troughModel->m_epsilon_3_34 = epsilon_3_34;
        troughModel->m_epsilon_3_41 = epsilon_3_41;
        troughModel->m_epsilon_3_42 = epsilon_3_42;
        troughModel->m_epsilon_3_43 = epsilon_3_43;
        troughModel->m_epsilon_3_44 = epsilon_3_44;

        troughModel->m_alpha_abs = alpha_abs;
        troughModel->m_Tau_envelope = Tau_envelope;
        troughModel->m_EPSILON_4 = EPSILON_4;
        troughModel->m_EPSILON_5 = EPSILON_5;
        troughModel->m_GlazingIntact = GlazingIntact;
        troughModel->m_P_a = P_a;
        troughModel->m_AnnulusGas = AnnulusGas;
        troughModel->m_AbsorberMaterial = AbsorberMaterial;
        troughModel->m_Shadowing = Shadowing;
        troughModel->m_Dirt_HCE = Dirt_HCE;
        troughModel->m_Design_loss = Design_loss;
        troughModel->m_SCAInfoArray = SCAInfoArray;

        troughModel->m_calc_design_pipe_vals = calc_design_pipe_vals;
        troughModel->m_L_rnr_pb = L_rnr_pb;
        troughModel->m_N_max_hdr_diams = N_max_hdr_diams;
        troughModel->m_L_rnr_per_xpan = L_rnr_per_xpan;
        troughModel->m_L_xpan_hdr = L_xpan_hdr;
        troughModel->m_L_xpan_rnr = L_xpan_rnr;
        troughModel->m_Min_rnr_xpans = Min_rnr_xpans;
        troughModel->m_northsouth_field_sep = northsouth_field_sep;
        troughModel->m_N_hdr_per_xpan = N_hdr_per_xpan;
        troughModel->m_K_cpnt = K_cpnt;
        troughModel->m_D_cpnt = D_cpnt;
        troughModel->m_L_cpnt = L_cpnt;
        troughModel->m_Type_cpnt = Type_cpnt;
        troughModel->m_custom_sf_pipe_sizes = custom_sf_pipe_sizes;
        troughModel->m_sf_rnr_diams = sf_rnr_diams;
        troughModel->m_sf_rnr_wallthicks = sf_rnr_wallthicks;
        troughModel->m_sf_rnr_lengths = sf_rnr_lengths;
        troughModel->m_sf_hdr_diams = sf_hdr_diams;
        troughModel->m_sf_hdr_wallthicks = sf_hdr_wallthicks;
        troughModel->m_sf_hdr_lengths = sf_hdr_lengths;

        // init inputs
        troughInitInputs.m_latitude = 32.13000107;
        troughInitInputs.m_longitude = -110.9400024;
        troughInitInputs.m_tz = -7;
        troughInitInputs.m_shift = -5.940002441;
        troughInitInputs.m_elev = -773;

        troughModel->init(troughInitInputs, troughSolvedParams);

        // inputs, constant or unused
        weatherValues.m_lat       = 32.13000107;
        weatherValues.m_lon       = -110.9400024;
        weatherValues.m_tz        = -7;
        weatherValues.m_shift     = -5.940002441;
        weatherValues.m_elev      = 773;
        weatherValues.m_global    = std::numeric_limits<double>::quiet_NaN();
        weatherValues.m_hor_beam  = std::numeric_limits<double>::quiet_NaN();    // 433.1
        weatherValues.m_diffuse   = std::numeric_limits<double>::quiet_NaN();    // 282
        weatherValues.m_twet      = std::numeric_limits<double>::quiet_NaN();
        weatherValues.m_wdir      = std::numeric_limits<double>::quiet_NaN();    // 88
        weatherValues.m_rhum      = std::numeric_limits<double>::quiet_NaN();
        weatherValues.m_snow      = std::numeric_limits<double>::quiet_NaN();
        weatherValues.m_albedo    = std::numeric_limits<double>::quiet_NaN();    // 0.213
        weatherValues.m_aod       = std::numeric_limits<double>::quiet_NaN();
        weatherValues.m_poa       = std::numeric_limits<double>::quiet_NaN();    // 715.1
        weatherValues.m_time_rise = std::numeric_limits<double>::quiet_NaN();    // 7.486443134
        weatherValues.m_time_set  = std::numeric_limits<double>::quiet_NaN();    // 17.46109472

        htfInletState.m_pres      = std::numeric_limits<double>::quiet_NaN();
        htfInletState.m_qual      = -1.;
        htfInletState.m_m_dot     = std::numeric_limits<double>::quiet_NaN();
    }

    void TearDown()
    {
        TroughProperties::TearDown();
        if (troughModel) {
            delete troughModel;
            troughModel = nullptr;
        }
    }

};

#endif
