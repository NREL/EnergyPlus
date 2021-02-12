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

#include "core.h"
#include "common.h"

#include "csp_common.h"

#include <ctime>

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_system[] = {

    /*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Off-design Inputs **
    { SSC_INPUT,  SSC_NUMBER,  "od_rel_tol",           "Baseline off-design relative convergence tolerance exponent (10^-od_rel_tol)", "-", "High temperature recuperator", "Heat Exchanger Design", "?=3","",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "od_T_t_in_mode",       "0: model solves co2/HTF PHX od model to calculate turbine inlet temp, 1: model sets turbine inlet temp to HTF hot temp", "", "", "", "?=0", "", ""},  // default to solving PHX
    { SSC_INPUT,  SSC_NUMBER,  "od_opt_objective",     "0: find P_LP_in to achieve target power, optimize efficiency 1: find P_LP_in to achieve T_HTF_cold, optimize efficiency", "", "", "", "?=0", "", "" },
    { SSC_INPUT,  SSC_MATRIX,  "od_cases",             "Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C,"
                                                       " 3) f_N_rc (=1 use design, =0 optimize, <0, frac_des = abs(input)),"
                                                       " 4) f_N_mc (=1 use design, =0 optimize, <0, frac_des = abs(input)),"
                                                       " 5) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)),"
                                                       " 6) PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases", "", "", "", "", "", "" },
	{ SSC_INPUT,  SSC_ARRAY,   "od_P_mc_in_sweep",     "Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C,"
                                                       " 3) T_mc_in_C, 4) T_pc_in_C,"
                                                       " 5) f_N_rc (=1 use design, <0, frac_des = abs(input),"
                                                       " 6) f_N_mc (=1 use design, <0, frac_des = abs(input),"
                                                       " 7) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)),"
                                                       " 8) PHX_f_dP (=1 use design, <0 = abs(input)", "", "", "", "",  "", "" },
    { SSC_INPUT,  SSC_ARRAY,   "od_T_mc_in_sweep",     "Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C,"
                                                       "3) f_N_rc (=1 use design, <0, frac_des = abs(input),"
                                                       "4) f_N_mc (=1 use design, <0, frac_des = abs(input),"
                                                       "5) f_N_pc (=1 use design, <0, frac_des = abs(input),"
                                                       "6) PHX_f_dP (=1 use design, <0 = abs(input)", "", "", "", "",  "", "" },
    { SSC_INPUT,  SSC_MATRIX,  "od_max_htf_m_dot",     "Columns: T_htf_C, T_amb_C, f_N_rc (=1 use design, <0, frac_des = abs(input), f_N_mc (=1 use design, <0, frac_des = abs(input), PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases", "", "", "", "",  "", "" },
	{ SSC_INPUT,  SSC_MATRIX,  "od_set_control",       "Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C,"
                                                       " 3) P_LP_in_MPa, 4) T_mc_in_C, 5) T_pc_in_C,"
                                                       " 6) f_N_rc (=1 use design, <0, frac_des = abs(input),"
                                                       " 7) f_N_mc (=1 use design, <0, frac_des = abs(input),"
                                                       " 8) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)),"
                                                       " 9) PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases", "", "", "", "", "", "" },
    { SSC_INPUT,  SSC_ARRAY,   "od_generate_udpc",     "True/False, f_N_rc (=1 use design, =0 optimize, <0, frac_des = abs(input), f_N_mc (=1 use design, =0 optimize, <0, frac_des = abs(input), PHX_f_dP (=1 use design, <0 = abs(input)", "", "", "", "",  "", "" },
    { SSC_INPUT,  SSC_NUMBER,  "is_gen_od_polynomials","Generate off-design polynomials for Generic CSP models? 1 = Yes, 0 = No", "", "", "",  "?=0",     "",       "" },

	// ** Off-Design Outputs **
		// Parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_htf_fracs",      "Normalized mass flow rate",                              "",           "Off-Design",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_od",             "Ambient temperatures",                                   "C",          "Off-Design",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_hot_od",         "HTF hot temperatures",                                   "C",          "Off-Design",    "",      "",     "",       "" },
		// Cycle control parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "P_comp_in_od",         "Main compressor inlet pressures",                        "MPa",        "Off-Design Cycle Control",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_phi_od",            "Off-design main compressor flow coefficient [od run][stage]", "",      "Off-Design Cycle Control",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "recomp_frac_od",       "Recompression fractions",                                "",           "Off-Design Cycle Control",    "",      "",     "",       "" },
		// Optimizer outputs
	{ SSC_OUTPUT, SSC_ARRAY,   "sim_time_od",          "Simulation time for off design optimization",            "s",          "Off-Design Optimizer",    "",      "",     "",       "" },
		// System solution
	{ SSC_OUTPUT, SSC_ARRAY,   "eta_thermal_od",       "Off-design cycle thermal efficiency",                    "",           "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_mc_in_od",           "Off-design compressor inlet temperature",                "C",          "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_mc_out_od",          "Off-design high side pressure",                          "MPa",        "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_cold_od",        "Off-design cold return temperature",                     "C",          "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_co2_full_od",    "Off-design mass flow rate through turbine",              "kg/s",       "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "W_dot_net_od",         "Off-design cycle net output (no cooling pars)",          "MWe",        "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "Q_dot_od",             "Off-design thermal input",                               "MWt",        "Off-Design System Solution",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "W_dot_net_less_cooling_od", "Off-design system output subtracting cooling parastics","MWe",    "Off-Design System Solution",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "eta_thermal_net_less_cooling_od","Calculated cycle thermal efficiency using W_dot_net_less_cooling", "-", "Off-Design System Solution","", "",   "",       "" },
    // Compressor
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_T_out_od",          "Off-design main compressor outlet temperature",          "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_W_dot_od",          "Off-design main compressor power",                       "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_m_dot_od",          "Off-design main compressor mass flow",                   "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_rho_in_od",         "Off-design main compressor inlet density",               "kg/m3",      "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_MATRIX,  "mc_psi_od",            "Off-design main compressor ideal head coefficient [od run][stage]","", "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_ideal_spec_work_od","Off-design main compressor ideal specific work",         "kJ/kg",      "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_N_od",              "Off-design main compressor speed",                       "rpm",        "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_N_od_perc",         "Off-design main compressor speed relative to design",    "%",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_eta_od",            "Off-design main compressor overall isentropic efficiency", "",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_tip_ratio_od",      "Off-design main compressor tip speed ratio [od run][stage]", "",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_eta_stages_od",     "Off-design main compressor stages isentropic efficiency [od run][stage]", "", "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_f_bypass_od",       "Off-design main compressor bypass to cooler inlet",      "-",          "",    "",      "",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_T_in_od",           "Off-design recompressor inlet temperature",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_P_in_od",           "Off-design recompressor inlet pressure",                 "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_T_out_od",          "Off-design recompressor outlet temperature",             "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_P_out_od",          "Off-design recompressor outlet pressure",                "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_W_dot_od",          "Off-design recompressor power",                          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_m_dot_od",          "Off-design recompressor mass flow",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_eta_od",            "Off-design recompressor overal isentropic efficiency",   "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_phi_od",            "Off-design recompressor flow coefficients [od run][stage]", "-",	   "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_MATRIX,  "rc_psi_od",            "Off-design recompressor ideal head coefficient [od run][stage]", "-",  "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "rc_N_od",              "Off-design recompressor shaft speed",                    "rpm",		   "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "rc_N_od_perc",         "Off-design recompressor shaft speed relative to design", "%",		   "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_MATRIX,  "rc_tip_ratio_od",      "Off-design recompressor tip speed ratio [od run][stage]","-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_eta_stages_od",     "Off-design recompressor stages isentropic efficiency [od run][stage]", "",    "",    "",      "",     "",       "" },
		// Precompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_T_in_od",           "Off-design precompressor inlet temperature",             "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_P_in_od",           "Off-design precompressor inlet pressure",                "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_W_dot_od",          "Off-design precompressor power",                         "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_m_dot_od",          "Off-design precompressor mass flow",                     "kg/s",       "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "pc_rho_in_od",         "Off-design precompressor inlet density",                 "kg/m3",      "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "pc_ideal_spec_work_od","Off-design precompressor ideal spec work",               "kJ/kg",      "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "pc_eta_od",            "Off-design precompressor overal isentropic efficiency",  "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_phi_od",            "Off-design precompressor flow coefficient [od run][stage]", "-",	   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_N_od",              "Off-design precompressor shaft speed",                   "rpm",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_tip_ratio_od",      "Off-design precompressor tip speed ratio [od run][stage]","-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_eta_stages_od",     "Off-design precompressor stages isentropic efficiency [od run][stage]", "",    "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_f_bypass_od",       "Off-design precompressor bypass to cooler inlet",        "-",          "",    "",      "",     "",       "" },
		// Compressor Totals	
	{ SSC_OUTPUT, SSC_ARRAY,   "c_tot_W_dot_od",       "Compressor total off-design power",                      "MWe",        "",    "",      "",     "",       "" },
		// Turbine																											   
	{ SSC_OUTPUT, SSC_ARRAY,   "t_P_in_od",            "Off-design turbine inlet pressure",                      "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_T_out_od",           "Off-design turbine outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_P_out_od",           "Off-design turbine outlet pressure",                     "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_W_dot_od",           "Off-design turbine power",                               "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_m_dot_od",           "Off-design turbine mass flow rate",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_delta_h_isen_od",    "Off-design turbine isentropic specific work",            "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_rho_in_od",          "Off-design turbine inlet density",                       "kg/m3",      "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_nu_od",              "Off-design turbine velocity ratio",	                     "-",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_N_od",               "Off-design turbine shaft speed",	                     "rpm",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_tip_ratio_od",       "Off-design turbine tip speed ratio",                     "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_eta_od",             "Off-design turbine efficiency",                          "-",          "",    "",      "",     "",       "" },
		// Recuperators
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_HP_T_out_od",      "Off-design low temp recup HP outlet temperature",        "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_LTR_od",           "Off-design low temp recup effectiveness",                "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_LTR_od",         "Off-design low temp recup heat transfer",                "MWt",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_LP_deltaP_od",     "Off-design low temp recup low pressure side pressure drop","-",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_HP_deltaP_od",     "Off-design low temp recup high pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_min_dT_od",        "Off-design low temp recup minimum temperature difference","C",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_LP_T_out_od",      "Off-design high temp recup LP outlet temperature",       "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_HP_T_in_od",       "Off-design high temp recup HP inlet temperature",        "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_HTR_od",           "Off-design high temp recup effectiveness",               "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_HTR_od",         "Off-design high temp recup heat transfer",               "MWt",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_LP_deltaP_od",     "Off-design high temp recup low pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_HP_deltaP_od",     "Off-design high temp recup high pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_min_dT_od",        "Off-design high temp recup minimum temperature difference","C",         "",    "",      "",     "",       "" },
		// PHX 
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_in_od",      "Off-design PHX co2 inlet temperature",                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_co2_PHX_in_od",      "Off-design PHX co2 inlet pressure",                      "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_out_od",     "Off-design PHX co2 outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "deltaT_HTF_PHX_od",    "Off-design HTF temp difference across PHX",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_eff_od",           "Off-design PHX effectiveness",                           "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_co2_deltaP_od",    "Off-design PHX co2 side pressure drop",                  "-",          "",    "",      "",     "",       "" },
		// Low Pressure Cooler
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_cooler_T_in_od",    "Off-design Low pressure cooler inlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_cooler_rho_in_od",  "Off-design Low pressure cooler inlet density",                      "kg/m3",      "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_cooler_in_isen_deltah_to_P_mc_out_od",  "Off-design Low pressure cooler inlet isen enthalpy rise to mc outlet pressure", "kJ/kg", "", "", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_cooler_co2_deltaP_od", "Off-design Off-design low pressure cooler co2 side pressure drop","-",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_cooler_W_dot_fan_od","Off-design Low pressure cooler fan power",                         "MWe",        "",    "",      "",     "",       "" },
		// Intermediate Pressure Cooler
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_cooler_W_dot_fan_od","Off-design Intermediate pressure cooler fan power",                "MWe",        "",    "",      "",     "",       "" },
		// Cooler Totals
    { SSC_OUTPUT, SSC_ARRAY,   "cooler_tot_W_dot_fan_od","Intermediate pressure cooler fan power",               "MWe",        "",    "",      "",     "",       "" },
        // Energy Balance Checks
    { SSC_OUTPUT, SSC_ARRAY,   "diff_m_dot_od",          "Off-design mass flow rate balance",        "-",        "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "diff_E_cycle",           "Off-design cycle energy balance",          "-",        "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "diff_Q_LTR",             "Off-design LTR energy balance",            "-",        "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "diff_Q_HTR",             "Off-design HTR energy balance",            "-",        "",    "",      "",     "",       "" },
		// UDPC Table
    { SSC_OUTPUT, SSC_MATRIX,  "udpc_table",  "Columns (7): HTF Temp [C], HTF ND mass flow [-], Ambient Temp [C], ND Power, ND Heat In, ND Fan Power, ND Water. Rows = runs" "", "", "", "", "", "" },
    { SSC_OUTPUT, SSC_NUMBER,  "udpc_n_T_htf",         "Number of HTF temperature values in udpc parametric",    "",          "",     "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "udpc_n_T_amb",         "Number of ambient temperature values in udpc parametric","",          "",     "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "udpc_n_m_dot_htf",     "Number of HTF mass flow rate values in udpc parameteric","",          "",     "",      "",     "",       "" },
        // Solver Metrics
	{ SSC_OUTPUT, SSC_ARRAY,   "od_code",              "Diagnostic info",                                        "-",          ""     "",      "",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_system : public compute_module
{
public:

	// Off-design parameters
	ssc_number_t *p_m_dot_htf_fracs;
	ssc_number_t *p_T_amb_od;
	ssc_number_t *p_T_htf_hot_od;
	// Optimized control parameters
	ssc_number_t *p_P_comp_in_od;
	ssc_number_t *pm_mc_phi_od;
	
	ssc_number_t *p_recomp_frac_od;
	// Optimizer parameters
	ssc_number_t *p_sim_time_od;
	// Systems
	ssc_number_t *p_eta_thermal_od;
	ssc_number_t *p_T_mc_in_od;
	ssc_number_t *p_P_mc_out_od;
	ssc_number_t *p_T_htf_cold_od;
	ssc_number_t *p_m_dot_co2_full_od;
	ssc_number_t *p_W_dot_net_od;
	ssc_number_t *p_Q_dot_od;
    ssc_number_t* p_eta_thermal_net_less_cooling_od;
	// Compressor
	ssc_number_t *p_mc_T_out_od;
	ssc_number_t *p_mc_W_dot_od;
	ssc_number_t *p_mc_m_dot_od;
	ssc_number_t *p_mc_rho_in_od;
    ssc_number_t *pm_mc_psi_od;
	ssc_number_t *p_mc_ideal_spec_work_od;
	ssc_number_t *p_mc_N_od;
    ssc_number_t *p_mc_N_od_perc;
	ssc_number_t *p_mc_eta_od;
	ssc_number_t *pm_mc_tip_ratio_od;
	ssc_number_t *pm_mc_eta_stages_od;
	ssc_number_t *p_mc_f_bypass_od;
	// Recompressor
	ssc_number_t *p_rc_T_in_od;
	ssc_number_t *p_rc_P_in_od;
	ssc_number_t *p_rc_T_out_od;
	ssc_number_t *p_rc_P_out_od;
	ssc_number_t *p_rc_W_dot_od;
	ssc_number_t *p_rc_m_dot_od;
	ssc_number_t *p_rc_eta_od;
	ssc_number_t *pm_rc_phi_od;
    ssc_number_t *pm_rc_psi_od;
	ssc_number_t *p_rc_N_od;
    ssc_number_t *p_rc_N_od_perc;
	ssc_number_t *pm_rc_tip_ratio_od;
	ssc_number_t *pm_rc_eta_stages_od;
	// Precompressor
	ssc_number_t *p_pc_T_in_od;
	ssc_number_t *p_pc_P_in_od;
	ssc_number_t *p_pc_W_dot_od;
	ssc_number_t *p_pc_m_dot_od;
    ssc_number_t *p_pc_rho_in_od;
    ssc_number_t *p_pc_ideal_spec_work_od;
	ssc_number_t *p_pc_eta_od;
	ssc_number_t *pm_pc_phi_od;
	ssc_number_t *p_pc_N_od;
	ssc_number_t *pm_pc_tip_ratio_od;
	ssc_number_t *pm_pc_eta_stages_od;
	ssc_number_t *p_pc_f_bypass_od;
	// Compressor Totals
	ssc_number_t *p_c_tot_W_dot_od;
	// Turbine
	ssc_number_t *p_t_P_in_od;
	ssc_number_t *p_t_T_out_od;
	ssc_number_t *p_t_P_out_od;
	ssc_number_t *p_t_W_dot_od;
	ssc_number_t *p_t_m_dot_od;
	ssc_number_t* p_t_delta_h_isen_od;
	ssc_number_t* p_t_rho_in_od;
	ssc_number_t *p_t_nu_od;
	ssc_number_t *p_t_N_od;
	ssc_number_t *p_t_tip_ratio_od;
	ssc_number_t *p_t_eta_od;
	// Recuperator
	ssc_number_t *p_LTR_HP_T_out_od;
	ssc_number_t *p_eff_LTR_od;
	ssc_number_t *p_q_dot_LTR_od;
	ssc_number_t *p_LTR_LP_deltaP_od;
	ssc_number_t *p_LTR_HP_deltaP_od;
	ssc_number_t *p_LTR_min_dT_od;
	ssc_number_t *p_HTR_LP_T_out_od;
	ssc_number_t *p_HTR_HP_T_in_od;
	ssc_number_t *p_eff_HTR_od;
	ssc_number_t *p_q_dot_HTR_od;
	ssc_number_t *p_HTR_LP_deltaP_od;
	ssc_number_t *p_HTR_HP_deltaP_od;
	ssc_number_t *p_HTR_min_dT_od;
	// PHX
	ssc_number_t *p_T_co2_PHX_in_od;
	ssc_number_t *p_P_co2_PHX_in_od;
	ssc_number_t *p_T_co2_PHX_out_od;
	ssc_number_t *p_deltaT_HTF_PHX_od;
	ssc_number_t *p_phx_eff_od;
	ssc_number_t *p_phx_co2_deltaP_od;
	// Low Pressure Cooler
	ssc_number_t *p_mc_cooler_T_in_od;
	ssc_number_t *p_mc_cooler_rho_in_od;
	ssc_number_t *p_mc_cooler_in_isen_deltah_to_P_mc_out_od;
	ssc_number_t *p_mc_cooler_co2_deltaP_od;
	ssc_number_t *p_mc_cooler_W_dot_fan_od;
	ssc_number_t* p_W_dot_net_less_cooling_od;
	// Intermediate Pressure Cooler
	ssc_number_t *p_pc_cooler_W_dot_fan_od;
	// Cooler Totals
	ssc_number_t *p_cooler_tot_W_dot_fan_od;
    // Energy Balance Checks
    ssc_number_t *p_diff_m_dot_od;
    ssc_number_t *p_diff_E_cycle;
    ssc_number_t *p_diff_Q_LTR;
    ssc_number_t *p_diff_Q_HTR;
    // UDPC table
    ssc_number_t *pm_udpc_table;
	// Solver Metrics
	ssc_number_t *p_od_code;

	cm_sco2_csp_system()
	{
		add_var_info(vtab_sco2_design);
		
		add_var_info(_cm_vtab_sco2_csp_system);
	}

	void exec() override
	{
		C_sco2_phx_air_cooler c_sco2_cycle;

		int sco2_des_err = sco2_design_cmod_common(this, c_sco2_cycle);
		if (sco2_des_err != 0)
			return;

		double m_dot_htf_design = c_sco2_cycle.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]

		bool is_rc = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_is_rc;

		int n_mc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_n_stages;
		int n_rc_stages = 1;
		if(is_rc)
			n_rc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_n_stages;		//[-]
		
		int cycle_config = c_sco2_cycle.get_design_par()->m_cycle_config;
		int n_pc_stages = 1;
		if(cycle_config == 2)
			n_pc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_n_stages;		//[-]

			   		 
		// Check if 'od_cases' is assigned
		bool is_od_cases_assigned = is_assigned("od_cases");
		bool is_P_mc_in_od_sweep_assigned = is_assigned("od_P_mc_in_sweep");
		bool is_od_T_mc_in_sweep_assigned = is_assigned("od_T_mc_in_sweep");
        bool is_od_max_htf_m_dot_assigned = is_assigned("od_max_htf_m_dot");
		bool is_od_set_control = is_assigned("od_set_control");
        bool is_od_generate_udpc_assigned = is_assigned("od_generate_udpc");
		if (is_od_cases_assigned && is_P_mc_in_od_sweep_assigned)
		{
			log("Both off design cases and main compressor inlet pressure sweep assigned. Only modeling off design cases");
			is_P_mc_in_od_sweep_assigned = false;
		}
		if (is_od_cases_assigned && is_od_T_mc_in_sweep_assigned)
		{
			log("Both off design cases and main compressor inlet temperature sweep assigned. Only modeling off design cases");
			is_od_T_mc_in_sweep_assigned = false;
		}
        if (is_od_cases_assigned && is_od_max_htf_m_dot_assigned)
        {
            log("Both off design cases and 'od_max_htf_m_dot' assigned. Only modeling off design cases");
            is_od_max_htf_m_dot_assigned = false;
        }
		if (is_od_cases_assigned && is_od_set_control)
		{
			log("Both off design cases and od set control assigned. Only modeling off design cases");
			is_od_set_control = false;
		}
        if (is_od_cases_assigned && is_od_generate_udpc_assigned)
        {
            log("Both 'od_cases' and 'od_generate_udpc' were assigned. Only modeling 'od_cases'");
            is_od_generate_udpc_assigned = false;
        }
		if (is_P_mc_in_od_sweep_assigned && is_od_T_mc_in_sweep_assigned)
		{
			log("Both main compressor inlet sweep and is_od_T_mc_in_sweep assigned. Only modeling off design cases");
			is_od_T_mc_in_sweep_assigned = false;
		}
        if (is_P_mc_in_od_sweep_assigned && is_od_max_htf_m_dot_assigned)
        {
            log("Both main compressor inlet sweep and 'od_max_htf_m_dot' assigned. Only modeling off design cases");
            is_od_max_htf_m_dot_assigned = false;
        }
		if (is_P_mc_in_od_sweep_assigned && is_od_set_control)
		{
			log("Both main compressor inlet sweep and od set control assigned. Only modeling off design cases");
			is_od_set_control = false;
		}
        if (is_P_mc_in_od_sweep_assigned && is_od_generate_udpc_assigned)
        {
            log("Both 'od_P_mc_in_sweep' and 'od_generate_udpc' were assigned. Only modeling 'od_P_mc_in_sweep'");
            is_od_generate_udpc_assigned = false;
        }
        if (is_od_T_mc_in_sweep_assigned && is_od_max_htf_m_dot_assigned)
        {
            log("Both is_od_T_mc_in_sweep and od set 'od_max_htf_m_dot' assigned. Only modeling od_T_mc_in_sweep");
            is_od_max_htf_m_dot_assigned = false;
        }
        if (is_od_T_mc_in_sweep_assigned && is_od_set_control)
		{
			log("Both is_od_T_mc_in_sweep and od set control assigned. Only modeling od_T_mc_in_sweep");
			is_od_set_control = false;
		}
		if (is_od_T_mc_in_sweep_assigned && is_od_generate_udpc_assigned)
		{
			log("Both 'od_T_mc_in_sweep' and 'od_generate_udpc' were assigned. Only modeling 'od_T_mc_in_sweep'");
			is_od_generate_udpc_assigned = false;
		}
        if (is_od_max_htf_m_dot_assigned && is_od_set_control)
        {
            log("Both 'od_max_htf_m_dot' and od set control assigned. Only modeling 'od_max_htf_m_dot'");
            is_od_set_control = false;
        }
        if (is_od_max_htf_m_dot_assigned && is_od_generate_udpc_assigned)
        {
            log("Both 'od_max_htf_m_dot' and 'od_generate_udpc' were assigned. Only modeling 'od_max_htf_m_dot'");
            is_od_generate_udpc_assigned = false;
        }
        if (is_od_set_control && is_od_generate_udpc_assigned)
        {
            log("Both 'od_set_control' and 'od_generate_udpc' were assigned. Only modeling 'od_set_control'");
            is_od_generate_udpc_assigned = false;
        }
		if (!is_od_cases_assigned && !is_P_mc_in_od_sweep_assigned && !is_od_set_control && !is_od_generate_udpc_assigned && !is_od_T_mc_in_sweep_assigned && !is_od_max_htf_m_dot_assigned)
		{
			log("No off-design cases or main compressor inlet sweep specified");
			return;
		}
		
		// Set up off-design analysis
		double P_LP_comp_in_des = std::numeric_limits<double>::quiet_NaN();		//[MPa]
		double delta_P = std::numeric_limits<double>::quiet_NaN();

		if (cycle_config == 1)
		{
			P_LP_comp_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0;		//[MPa] convert from kPa
			delta_P = 10.0;
		}
		else
		{
			P_LP_comp_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::PC_IN] / 1000.0;		//[MPa] convert from kPa
			delta_P = 6.0;
		}

        // Get turbine inlet mode
        int T_t_in_mode = as_integer("od_T_t_in_mode");

        // Get some cycle design info
        double T_htf_hot_des = c_sco2_cycle.get_design_par()->m_T_htf_hot_in;      //[K]
        double T_amb_des = c_sco2_cycle.get_design_par()->m_T_amb_des;         //[K]
        double T_t_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::TURB_IN];	//[K] 
        double T_co2_PHX_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT];   //[K]
        double T_htf_PHX_out_des = c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_T_h_out;   //[K]

        // Get off design objective
        int od_strategy_in = as_integer("od_opt_objective");
        C_sco2_phx_air_cooler::E_off_design_strategies od_strategy = (C_sco2_phx_air_cooler::E_off_design_strategies) (od_strategy_in);
        od_strategy = C_sco2_phx_air_cooler::E_TARGET_T_HTF_COLD_POWER_MAX;

		util::matrix_t<double> od_cases;
		if (is_od_cases_assigned)
		{
			util::matrix_t<double> od_cases_local = as_matrix("od_cases");

			// Check if off cases exist and correctly formatted
			int n_od_cols_loc = (int)od_cases_local.ncols();
			int n_od_runs_loc = (int)od_cases_local.nrows();

			if (n_od_cols_loc < 3 && n_od_runs_loc == 1)
			{
				// No off-design cases specified
				log("No off-design cases specified");
				return;
			}
			else if (n_od_cols_loc < 3)
			{
				std::string err_msg = util::format("The matrix of off design cases requires at least 3 columns. The entered matrix has %d columns", n_od_cols_loc);
				throw exec_error("sco2_csp_system", err_msg);
			}
            else if (n_od_cols_loc < 7)
            {
                od_cases.resize_fill(n_od_runs_loc, 7, 1.0);
                for (int i = 0; i < n_od_runs_loc; i++)
                {
                    for (int j = 0; j < n_od_cols_loc; j++)
                    {
                        od_cases(i, j) = od_cases_local(i, j);
                    }
                }
            }
            else
            {
                od_cases = od_cases_local;
            }
		}
		else if(is_P_mc_in_od_sweep_assigned)
		{
			std::vector<double> od_case = as_vector_double("od_P_mc_in_sweep");
			size_t n_od = od_case.size();
			if (n_od < 5)
			{
				std::string err_msg = util::format("The matrix of off design cases requires at least 5 columns. The entered matrix has %d columns", n_od);
				throw exec_error("sco2_csp_system", err_msg);
			}
			
            int n_P_mc_in = 101;

			double P_mc_in_low = P_LP_comp_in_des - delta_P / 2.0;	//[MPa]

			double delta_P_i = delta_P / (n_P_mc_in - 1);	//[MPa]

            // Adding P_LP_in as the final column
			od_cases.resize_fill(n_P_mc_in, 10, 1.0);
            int n_cols_in = std::min((int)n_od, 9);
			for (int i = 0; i < n_P_mc_in; i++)
			{
                for (int j = 0; j < n_cols_in; j++)
                {
                    od_cases(i, j) = od_case[j];
                }

                od_cases(i, 9) = P_mc_in_low + delta_P_i * i;	//[MPa]
			}
		}
		else if (is_od_T_mc_in_sweep_assigned)
		{
			std::vector<double> od_case_local = as_vector_double("od_T_mc_in_sweep");
			size_t n_od = od_case_local.size();
			if (n_od < 3)
			{
				std::string err_msg = util::format("od_T_mc_in_sweep requires at least 3 columns. The entered value has %d columns", n_od);
				throw exec_error("sco2_csp_system", err_msg);
			}

			double range_T_mc_in = 30.0;	//[C]

			double delta_T_mc_in = 0.5;		//[C]

			double T_mc_in_start = od_case_local[2] + 0.5;		//[C]

			double n_T_mc_in = std::floor(range_T_mc_in / delta_T_mc_in + 0.5) + 1;	//[-]

			od_cases.resize_fill(n_T_mc_in, 8, 1.0);
			int n_cols_in = std::min((int)n_od, 7);
			for (int i = 0; i < n_T_mc_in; i++)
			{
				for (int j = 0; j < n_cols_in; j++)
				{
					od_cases(i, j) = od_case_local[j];
				}

				od_cases(i, 7) = (T_mc_in_start + delta_T_mc_in * i)+273.15;		//[K] convert from C
			}
		}
        else if (is_od_max_htf_m_dot_assigned) {

            util::matrix_t<double> od_cases_local = as_matrix("od_max_htf_m_dot");

            // Check if off cases exist and correctly formatted
            int n_od_cols_loc = (int)od_cases_local.ncols();
            int n_od_runs_loc = (int)od_cases_local.nrows();

            if (n_od_cols_loc < 2 && n_od_runs_loc == 1)
            {
                // No off-design cases specified
                log("No od_max_htf_m_dot cases specified");
                return;
            }
            else if (n_od_cols_loc < 2)
            {
                std::string err_msg = util::format("The od_max_htf_m_dot matrix requires at least 2 columns. The entered matrix has %d columns", n_od_cols_loc);
                throw exec_error("sco2_csp_system", err_msg);
            }

            od_cases.resize_fill(n_od_runs_loc, 6, 1.0);

            for (int i = 0; i < n_od_runs_loc; i++) {
                for (int j = 0; j < 1; j++) {
                    od_cases(i, j) = od_cases_local(i, j);
                }
                for (int j = 1; j < n_od_cols_loc; j++) {
                    od_cases(i,j+1) = od_cases_local(i,j);
                }
            }
        }
		else if(is_od_set_control)
		{
            util::matrix_t<double> od_cases_local = as_matrix("od_set_control");

			// Check if off cases exist and correctly formatted
			int n_od_cols_loc = (int)od_cases_local.ncols();
			int n_od_runs_loc = (int)od_cases_local.nrows();

			if (n_od_cols_loc < 6)
			{
				std::string err_msg = util::format("The matrix of od set control requires at least 6 columns. The entered matrix has %d columns", n_od_cols_loc);
				throw exec_error("sco2_csp_system", err_msg);
			}
            else if (n_od_cols_loc < 10)
            {
                od_cases.resize_fill(n_od_runs_loc, 10, 1.0);
                for (int i = 0; i < n_od_runs_loc; i++)
                {
                    for (int j = 0; j < n_od_cols_loc; j++)
                    {
                        od_cases(i, j) = od_cases_local(i, j);
                    }
                }
            }
            else
            {
                od_cases = od_cases_local;
            }
		}
        else if(is_od_generate_udpc_assigned)        // od_generate_udpc
        {
            if (as_integer("od_T_t_in_mode") == 1)
            {
                T_htf_hot_des = T_t_in_des;     //[K]
            }

            // UDPC settings that could potentially be user-defined through SSC INPUTS
                // HTF mass flow
            double m_dot_htf_ND_low = 0.5;      //[-]
            double m_dot_htf_ND_high = 1.05;    //[-]
            int n_m_dot_htf_ND = 12;
                // HTF temperature
            double T_htf_delta_cold = 30.0;     //[K--C]
            double T_htf_delta_hot = 15.0;      //[K--C]
            int n_T_htf_hot = 4;
                // Ambient temperature
            double T_amb_low = 273.15 + 0.0;         //[K]
            double T_amb_high = std::max(273.15 + 45.0, T_amb_des + 5.0);   //[K]
            int n_T_amb = T_amb_high - T_amb_low + 1;

            std::vector<double> udpc_pars = as_vector_double("od_generate_udpc");
            int n_udpc_pars = udpc_pars.size();
            int n_od_cases_mode_pars = std::min(3, n_udpc_pars-1);
            
            assign("udpc_n_m_dot_htf", (ssc_number_t)n_m_dot_htf_ND);
            double m_dot_htf_ND_des = 1.0;      //[-]
            double m_dot_htf_ND_par_start = m_dot_htf_ND_low;       // m_dot_htf_ND_low - 0.05;    //[-]
            double m_dot_htf_ND_par_end = m_dot_htf_ND_high;        // m_dot_htf_ND_high + 0.05;     //[-]
            double delta_m_dot_htf_ND = (m_dot_htf_ND_par_end - m_dot_htf_ND_par_start) / (double)(n_m_dot_htf_ND - 1);
            std::vector<double> m_dot_htf_ND_levels(3);
            m_dot_htf_ND_levels[0] = m_dot_htf_ND_low;
            m_dot_htf_ND_levels[1] = m_dot_htf_ND_des;
            m_dot_htf_ND_levels[2] = m_dot_htf_ND_high;

            double T_htf_low = T_htf_hot_des - T_htf_delta_cold;       //[K]
            double T_htf_high = T_htf_hot_des + T_htf_delta_hot;       //[K]
            assign("udpc_n_T_htf", (ssc_number_t)n_T_htf_hot);
            double T_htf_par_start = T_htf_low;     // T_htf_low - 5.0;     //[K]
            double T_htf_par_end = T_htf_high;      // T_htf_high + 5.0;      //[K]
            double delta_T_htf_hot = (T_htf_par_end - T_htf_par_start) / (double)(n_T_htf_hot - 1);
            std::vector<double> T_htf_levels(3);
            T_htf_levels[0] = T_htf_low;    //[C]
            T_htf_levels[1] = T_htf_hot_des;   //[C]
            T_htf_levels[2] = T_htf_high;  //[C]

            assign("udpc_n_T_amb", (ssc_number_t)n_T_amb);
            double T_amb_par_start = T_amb_low; // 273.15 + 0.0;      //[K]
            double T_amb_par_end = T_amb_high;  // T_amb_high + 1.0;    //[K]
            double delta_T_amb = (T_amb_par_end - T_amb_par_start) / (double)(n_T_amb - 1);  //[K]
            std::vector<double> T_amb_levels(3);
            T_amb_levels[0] = T_amb_low;    //[C]
            T_amb_levels[1] = T_amb_des;	//[C]
            T_amb_levels[2] = T_amb_high;	//[C]

            int n_total_runs = 3 * (n_m_dot_htf_ND + n_T_htf_hot + n_T_amb);

            od_cases.resize_fill(n_total_runs, 7, 1.0);

            // Set constant cycle operating mode parameters from 'od_generate_udpc'
            for (int i = 0; i < n_total_runs; i++)
            {
                for (int j = 0; j < n_od_cases_mode_pars; j++)
                {
                    od_cases(i, 3 + j) = udpc_pars[j + 1];
                }
            }

            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < n_T_htf_hot; j++)
                {
                    double T_htf_hot_j = T_htf_par_start + delta_T_htf_hot * j;       //[K]
                
                    od_cases(i*n_T_htf_hot + j, 0) = T_htf_hot_j - 273.15;      //[C] convert from K -> parametric
                    od_cases(i*n_T_htf_hot + j, 1) = m_dot_htf_ND_levels[i];    //[-] -> levels
                    od_cases(i*n_T_htf_hot + j, 2) = T_amb_des - 273.15;        //[C] convert from K -> constant
                }
            }

            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < n_T_amb; j++)
                {
                    double T_amb_j = T_amb_par_start + delta_T_amb * j;       //[K]

                    od_cases(3*n_T_htf_hot + i*n_T_amb + j, 0) = T_htf_levels[i] - 273.15;   //[C] convert from K -> levels
                    od_cases(3*n_T_htf_hot + i*n_T_amb + j, 1) = m_dot_htf_ND_des;  //[-] -> constant
                    od_cases(3*n_T_htf_hot + i*n_T_amb + j, 2) = T_amb_j - 273.15;  //[C] convert from K -> parametric 
                }
            }

            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < n_m_dot_htf_ND; j++)
                {
                    double m_dot_j = m_dot_htf_ND_par_start + delta_m_dot_htf_ND * j;     //[-]

                    od_cases(3*n_T_htf_hot + 3*n_T_amb + i*n_m_dot_htf_ND + j, 0) = T_htf_hot_des - 273.15; //[C] convert from K -> constant
                    od_cases(3*n_T_htf_hot + 3*n_T_amb + i*n_m_dot_htf_ND + j, 1) = m_dot_j;          //[-] -> parametric
                    od_cases(3*n_T_htf_hot + 3*n_T_amb + i*n_m_dot_htf_ND + j, 2) = T_amb_levels[i] - 273.15;     //[C] convert from K -> levels
                }
            }
        }
        else {
            log("Off-design cases not properly defined");
            return;
        }
		
		int n_od_runs = (int)od_cases.nrows();
		allocate_ssc_outputs(n_od_runs, n_mc_stages, n_rc_stages, n_pc_stages, is_od_generate_udpc_assigned);
		C_sco2_phx_air_cooler::S_od_par s_sco2_od_par;

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

        double od_opt_tol = 1.E-3;
        double od_tol = pow(10, -as_double("od_rel_tol"));

		for(int n_run = 0; n_run < n_od_runs; n_run++)
		{			
			// Try calling off-design model with design parameters
				// Set outputs
			p_T_htf_hot_od[n_run] = (ssc_number_t)od_cases(n_run, 0);			//[C]
			double m_dot_htf_od_ND_in = od_cases(n_run, 1);		//[-]
			p_T_amb_od[n_run] = (ssc_number_t)od_cases(n_run, 2);				//[C]
				// Set input structure
			s_sco2_od_par.m_T_htf_hot = p_T_htf_hot_od[n_run] + 273.15;	    //[K]
			s_sco2_od_par.m_m_dot_htf = m_dot_htf_design*m_dot_htf_od_ND_in;	//[kg/s]
			s_sco2_od_par.m_T_amb = p_T_amb_od[n_run] + 273.15;				//[K]

            s_sco2_od_par.m_T_t_in_mode = T_t_in_mode;  //[-]

			int off_design_code = 0;
			std::clock_t clock_start = std::clock();
			try
			{
				if (is_od_cases_assigned || is_od_generate_udpc_assigned)
				{
                        // RC shaft speed control
                    bool is_rc_N_od_at_design = true;
                    bool is_optimize_rc_N = false;
                    double rc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 3) < 0.0)
                    {
                        is_rc_N_od_at_design = false;
                        rc_N_od_f_des = fabs(od_cases(n_run, 3));
                    }
                    else if (od_cases(n_run, 3) == 0.0)
                    {
                        is_optimize_rc_N = true;
                    }
                        // MC shaft speed control
                    bool is_mc_N_od_at_design = true;
                    bool is_optimize_mc_N = false;
                    double mc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 4) < 0.0)
                    {
                        is_mc_N_od_at_design = false;
                        mc_N_od_f_des = fabs(od_cases(n_run, 4));
                    }
                    else if (od_cases(n_run, 4) == 0.0)
                    {
                        is_optimize_mc_N = true;
                    }
                        // PC shaft speed control
                    bool is_pc_N_od_at_design = true;
                    bool is_optimize_pc_N = false;
                    double pc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 5) < 0.0) {
                        is_pc_N_od_at_design = false;
                        pc_N_od_f_des = fabs(od_cases(n_run, 5));
                    }
                    else if(od_cases(n_run, 5) == 0.0){
                        is_optimize_pc_N = true;
                    }
                        // PHX pressure drop options
                    bool is_PHX_dP_input = false;
                    double PHX_f_dP_od = std::numeric_limits<double>::quiet_NaN();
                    if (od_cases(n_run, 6) < 0.0)
                    {
                        is_PHX_dP_input = true;
                        PHX_f_dP_od = fabs(od_cases(n_run, 6));
                    }

                    double eta_max, f_N_mc_opt, f_N_rc_opt, W_dot_at_eta_max;

                    std::string od_sim_log = util::format("Beginning off design run %d of %d, the "
                                            "HTF temperature is %lg [C] "
                                            "the HTF mass flow rate is %lg [-] "
                                            "the ambient temperature is %lg [C] "
                                            "the relative RC shaft speed is %lg [-] "
                                            "the relative MC shaft speed is %lg [-] "
                                            "the input PHX pressure drop is %lg [-] ",
                                            n_run + 1, n_od_runs,
                                            s_sco2_od_par.m_T_htf_hot - 273.15, m_dot_htf_od_ND_in, s_sco2_od_par.m_T_amb - 273.15,
                                            rc_N_od_f_des, mc_N_od_f_des, PHX_f_dP_od);
                    log(od_sim_log);

                    //if (cycle_config == 1 || cycle_config == 2)
                    //{
                    //    if (is_optimize_mc_N && is_optimize_rc_N)
                    //    {
                    off_design_code = c_sco2_cycle.optimize_N_mc_and_N_rc__max_eta(s_sco2_od_par,
                        is_PHX_dP_input, PHX_f_dP_od,
                        od_strategy,
                        is_optimize_rc_N, is_rc_N_od_at_design, rc_N_od_f_des,
                        is_optimize_mc_N, is_mc_N_od_at_design, mc_N_od_f_des,
                        is_optimize_pc_N, is_pc_N_od_at_design, pc_N_od_f_des,
                        eta_max, f_N_mc_opt, f_N_rc_opt, W_dot_at_eta_max, od_opt_tol, od_tol);
                     /*   }
                        else if(is_optimize_rc_N && is_rc)
                        {
                            off_design_code = c_sco2_cycle.optimize_N_rc__max_eta(s_sco2_od_par,
                                true, 1.0,
                                is_PHX_dP_input, PHX_f_dP_od,
                                od_strategy,
                                eta_max, f_N_rc_opt, W_dot_at_eta_max, 1.0, od_opt_tol, od_tol);
                        }
                        else if(is_optimize_mc_N)
                        {
                            off_design_code = c_sco2_cycle.optimize_N_mc_and_N_rc__max_eta(s_sco2_od_par,
                                is_PHX_dP_input, PHX_f_dP_od,
                                od_strategy, is_optimize_rc_N,
                                eta_max, f_N_mc_opt, f_N_rc_opt, W_dot_at_eta_max, od_opt_tol, od_tol);
                        }
                        else
                        {
                            off_design_code = c_sco2_cycle.off_design__constant_N__T_mc_in_P_LP_in__objective(s_sco2_od_par,
                                is_rc_N_od_at_design, rc_N_od_f_des,
                                is_mc_N_od_at_design, mc_N_od_f_des,
                                is_mc_N_od_at_design, mc_N_od_f_des,
                                is_PHX_dP_input, PHX_f_dP_od,
                                od_strategy, od_opt_tol, od_tol);
                        }
                    }
                    else
                    {
                        off_design_code = c_sco2_cycle.off_design__constant_N__T_mc_in_P_LP_in__objective(s_sco2_od_par,
                            is_rc_N_od_at_design, rc_N_od_f_des,
                            is_mc_N_od_at_design, mc_N_od_f_des,
                            is_pc_N_od_at_design, pc_N_od_f_des,
                            is_PHX_dP_input, PHX_f_dP_od,
                            od_strategy, od_opt_tol, od_tol);
                    }*/
                    
				}
				else if (is_P_mc_in_od_sweep_assigned)
				{
                    double P_mc_in_local = od_cases(n_run, 9);          //[MPa]
                    double T_mc_in_local = od_cases(n_run, 3) + 273.15; //[K] convert from C
                    double T_pc_in_local = od_cases(n_run, 4) + 273.15; //[K] convert from C
                        // RC shaft speed control
                    bool is_rc_N_od_at_design = true;
                    double rc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 5) < 0.0)
                    {
                        is_rc_N_od_at_design = false;
                        rc_N_od_f_des = fabs(od_cases(n_run, 5));
                    }
                        // MC shaft speed control
                    bool is_mc_N_od_at_design = true;
                    double mc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 6) < 0.0)
                    {
                        is_mc_N_od_at_design = false;
                        mc_N_od_f_des = fabs(od_cases(n_run, 6));
                    }
                        // PC shaft speed control
                    bool is_pc_N_od_at_design = true;
                    bool is_optimize_pc_N = false;
                    double pc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 7) < 0.0) {
                        is_pc_N_od_at_design = false;
                        pc_N_od_f_des = fabs(od_cases(n_run, 7));
                    }
                        // PHX pressure drop options
                    bool is_PHX_dP_input = false;
                    double PHX_f_dP_od = std::numeric_limits<double>::quiet_NaN();
                    if (od_cases(n_run, 8) < 0.0)
                    {
                        is_PHX_dP_input = true;
                        PHX_f_dP_od = fabs(od_cases(n_run, 8));
                    }

                    std::string od_sim_log = util::format("Beginning off design run %d of %d, the "
                        "HTF temperature is %lg [C] "
                        "the HTF mass flow rate is %lg [-] "
                        "the ambient temperature is %lg [C] "
                        "the low pressure compressor inlet is %lg [MPa] "
                        "the main compressor inlet temperature is %lg [C] "
                        "the pre-compressor inlet temperature is %lg [C] "
                        "the relative RC shaft speed is %lg [-] "
                        "the relative MC shaft speed is %lg [-] "
                        "the input PHX pressure drop is %lg [-] ",
                        n_run + 1, n_od_runs,
                        s_sco2_od_par.m_T_htf_hot - 273.15, m_dot_htf_od_ND_in, s_sco2_od_par.m_T_amb - 273.15,
                        P_mc_in_local, T_mc_in_local - 273.15, T_pc_in_local - 273.15,
                        rc_N_od_f_des, mc_N_od_f_des, PHX_f_dP_od);
                    log(od_sim_log);

					off_design_code = c_sco2_cycle.off_design_fix_P_mc_in(s_sco2_od_par, P_mc_in_local,
                                                                            T_mc_in_local, T_pc_in_local,
                                                                            is_rc_N_od_at_design, rc_N_od_f_des,
                                                                            is_mc_N_od_at_design, mc_N_od_f_des,
                                                                            is_pc_N_od_at_design, pc_N_od_f_des,
                                                                            is_PHX_dP_input, PHX_f_dP_od,
                                                                            od_strategy, od_opt_tol, od_tol);
				}
				else if (is_od_T_mc_in_sweep_assigned)
				{
                    // RC shaft speed control
					bool is_rc_N_od_at_design = true;
					double rc_N_od_f_des = 1.0;
					if (od_cases(n_run, 3) < 0.0)
					{
						is_rc_N_od_at_design = false;
						rc_N_od_f_des = fabs(od_cases(n_run, 3));
					}
					// MC shaft speed control
					bool is_mc_N_od_at_design = true;
					double mc_N_od_f_des = 1.0;
					if (od_cases(n_run, 4) < 0.0)
					{
						is_mc_N_od_at_design = false;
						mc_N_od_f_des = fabs(od_cases(n_run, 4));
					}
                    // PC shaft speed control
                    bool is_pc_N_od_at_design = true;
                    double pc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 5) < 0.0) {
                        is_pc_N_od_at_design = false;
                        pc_N_od_f_des = fabs(od_cases(n_run, 5));
                    }
					// PHX pressure drop options
					bool is_PHX_dP_input = false;
					double PHX_f_dP_od = std::numeric_limits<double>::quiet_NaN();
					if (od_cases(n_run, 6) < 0.0)
					{
						is_PHX_dP_input = true;
						PHX_f_dP_od = fabs(od_cases(n_run, 6));
					}

					std::string od_sim_log = util::format("Beginning off design run %d of %d, the "
						"Main compressor inlet temp is %lg [C] "
						"HTF temperature is %lg [C] "
						"the HTF mass flow rate is %lg [-] "
						"the ambient temperature is %lg [C] "
						"the relative RC shaft speed is %lg [-] "
						"the relative MC shaft speed is %lg [-] "
                        "the relative PC shaft speed is %lg [-] "
						"the input PHX pressure drop is %lg [-] ",
						n_run + 1, n_od_runs,
						od_cases(n_run,7)-273.15, s_sco2_od_par.m_T_htf_hot - 273.15, m_dot_htf_od_ND_in, s_sco2_od_par.m_T_amb - 273.15,
						rc_N_od_f_des, mc_N_od_f_des, pc_N_od_f_des, PHX_f_dP_od);
					log(od_sim_log);

					off_design_code = c_sco2_cycle.off_design_fix_T_mc_in__P_mc_in_solve_for_target(s_sco2_od_par, od_cases(n_run, 7),
																		is_rc_N_od_at_design, rc_N_od_f_des,
																		is_mc_N_od_at_design, mc_N_od_f_des,
                                                                        is_pc_N_od_at_design, pc_N_od_f_des,
																		is_PHX_dP_input, PHX_f_dP_od,
																		od_strategy, od_opt_tol, od_tol);
				}
                else if (is_od_max_htf_m_dot_assigned) {

                    // RC shaft speed control
                    bool is_rc_N_od_at_design = true;
                    double rc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 3) < 0.0)
                    {
                        is_rc_N_od_at_design = false;
                        rc_N_od_f_des = fabs(od_cases(n_run, 3));
                    }
                    // MC shaft speed control
                    bool is_mc_N_od_at_design = true;
                    double mc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 4) < 0.0)
                    {
                        is_mc_N_od_at_design = false;
                        mc_N_od_f_des = fabs(od_cases(n_run, 4));
                    }
                    // PHX pressure drop options
                    bool is_PHX_dP_input = false;
                    double PHX_f_dP_od = std::numeric_limits<double>::quiet_NaN();
                    if (od_cases(n_run, 5) < 0.0)
                    {
                        is_PHX_dP_input = true;
                        PHX_f_dP_od = fabs(od_cases(n_run, 5));
                    }

                    std::string od_sim_log = util::format("Beginning 'od_max_htf_m_dot' run %d of %d, the "
                        "HTF temperature is %lg [C] "
                        "the ambient temperature is %lg [C] "
                        "the relative RC shaft speed is %lg [-] "
                        "the relative MC shaft speed is %lg [-] "
                        "the input PHX pressure drop is %lg [-] ",
                        n_run + 1, n_od_runs,
                        s_sco2_od_par.m_T_htf_hot - 273.15, s_sco2_od_par.m_T_amb - 273.15,
                        rc_N_od_f_des, mc_N_od_f_des, PHX_f_dP_od);
                    log(od_sim_log);

                    off_design_code = c_sco2_cycle.off_design__constant_N__calc_max_htf_massflow__T_mc_in_P_LP_in__objective(s_sco2_od_par,
                        is_rc_N_od_at_design, rc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_PHX_dP_input, PHX_f_dP_od,
                        od_strategy, od_opt_tol, od_tol);
                }
				else if (is_od_set_control)
				{
					double P_LP_comp_in = od_cases(n_run, 3);
					if (od_cases(n_run, 3) < 0.0)
					{
						P_LP_comp_in = P_LP_comp_in_des / fabs(P_LP_comp_in);
					}
                    double T_mc_in_local = od_cases(n_run, 4) + 273.15; //[K] convert from C
                    double T_pc_in_local = od_cases(n_run, 5) + 273.15; //[K] convert from C
                        // RC shaft speed control
                    bool is_rc_N_od_at_design = true;
                    double rc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 6) < 0.0)
                    {
                        is_rc_N_od_at_design = false;
                        rc_N_od_f_des = fabs(od_cases(n_run, 6));
                    }
                        // MC shaft speed control
                    bool is_mc_N_od_at_design = true;
                    double mc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 7) < 0.0)
                    {
                        is_mc_N_od_at_design = false;
                        mc_N_od_f_des = fabs(od_cases(n_run, 7));
                    }
                        // PC shaft speed control
                    bool is_pc_N_od_at_design = true;
                    bool is_optimize_pc_N = false;
                    double pc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 8) < 0.0) {
                        is_pc_N_od_at_design = false;
                        pc_N_od_f_des = fabs(od_cases(n_run, 8));
                    }
                        // PHX pressure drop options
                    bool is_PHX_dP_input = false;
                    double PHX_f_dP_od = std::numeric_limits<double>::quiet_NaN();
                    if (od_cases(n_run, 9) < 0.0)
                    {
                        is_PHX_dP_input = true;
                        PHX_f_dP_od = fabs(od_cases(n_run, 9));
                    }

					off_design_code = c_sco2_cycle.off_design_fix_P_mc_in(s_sco2_od_par,
                                                                            P_LP_comp_in, T_mc_in_local, T_pc_in_local,
                                                                            is_rc_N_od_at_design, rc_N_od_f_des,
                                                                            is_mc_N_od_at_design, mc_N_od_f_des,
                                                                            is_pc_N_od_at_design, pc_N_od_f_des,
                                                                            is_PHX_dP_input, PHX_f_dP_od,
                                                                            od_strategy, od_opt_tol, od_tol);
				}
			}
			catch( C_csp_exception &csp_exception )
			{
				// Report warning before exiting with error
				while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
				{
					log(out_msg);
				}

				log(csp_exception.m_error_message, SSC_ERROR, -1.0);
				throw exec_error("sco2_csp_system", csp_exception.m_error_message);
			}

			std::clock_t clock_end = std::clock();

			double od_opt_duration = (clock_end - clock_start)/(double) CLOCKS_PER_SEC;		//[s]

			p_od_code[n_run] = (ssc_number_t)off_design_code;
			if(off_design_code == 0 || ((is_P_mc_in_od_sweep_assigned || is_od_set_control) && c_sco2_cycle.get_od_solved()->m_is_converged))
			{	// Off-design call was successful, so write outputs
                p_m_dot_htf_fracs[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_par()->m_m_dot_htf / m_dot_htf_design);   //[-]
					// Control parameters
				p_P_comp_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0);	//[MPa]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_phi[i_s];	//[-]
				p_recomp_frac_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = (ssc_number_t)od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_T_mc_in_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN] - 273.15;	//[C]
				p_P_mc_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT] / 1.E3);	//[MPa]
				p_T_htf_cold_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15);		//[C]
				p_m_dot_co2_full_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net / 1.E3);	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_mc_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_OUT] - 273.15);	//[C]
				p_mc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
				double comp_W_dot_od_sum = p_mc_W_dot_od[n_run];	//[MWe]
				p_mc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_mc);			//[kg/s]
				p_mc_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_dens[C_sco2_cycle_core::MC_IN]);	//[kg/m3]
				p_mc_ideal_spec_work_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_isen_spec_work);	//[kJ/kg]
				p_mc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_N;		//[rpm]
                p_mc_N_od_perc[n_run] = p_mc_N_od[n_run] / as_double("mc_N_des") * 100.0;       //[%]
				p_mc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_eta;	//[-]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
                    pm_mc_psi_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_psi[i_s];   //[-]
                    pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_eta[i_s];	//[-]
				}
				p_mc_f_bypass_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_mc_f_bypass;		//[-]
					// Recompressor
				if (is_rc)
				{
					p_rc_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_T_in - 273.15);	//[C]
					p_rc_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_P_in*1.E-3);		//[MPa] convert from kPa
					p_rc_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_T_out - 273.15);	//[C] convert from K
					p_rc_P_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_P_out*1.E-3);	//[MPa] convert from kPa
					p_rc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_rc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_rc);				//[kg/s]
					p_rc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_eta;		//[-]
                    for (int i_s = 0; i_s < n_rc_stages; i_s++)
                    {
                        pm_rc_phi_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_phi[i_s];	//[-]
                        pm_rc_psi_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_psi[i_s];	//[-]
                    }
					p_rc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_N;			//[rpm]
                    p_rc_N_od_perc[n_run] = p_rc_N_od[n_run] / as_double("rc_N_des") * 100.0;       //[%]
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_eta[i_s];	//[-]
					}
					comp_W_dot_od_sum += p_rc_W_dot_od[n_run];		//[MWe]
				}
				else
				{
					p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_W_dot_od[n_run] = 0.0;
					p_rc_m_dot_od[n_run] = 0.0;
					p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    for (int i_s = 0; i_s < n_rc_stages; i_s++)
                    {
                        pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                        pm_rc_psi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    }
					p_rc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    p_rc_N_od_perc[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					}
				}
					// Precompressor
				if (cycle_config == 2)
				{
					p_pc_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_T_in - 273.15);		//[C]
					p_pc_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_P_in*1.E-3);		//[MPa]
					p_pc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_pc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_pc);		//[kg/s]
                    p_pc_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_dens[C_sco2_cycle_core::PC_IN]);    //[kg/m3]
                    p_pc_ideal_spec_work_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_isen_spec_work);   //[kJ/kg]
                    p_pc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_eta;		//[-]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
						pm_pc_phi_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_phi[i_s];
					p_pc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_N;			//[rpm]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
					{
						pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_eta[i_s];				//[-]
					}
					p_pc_f_bypass_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pc_f_bypass;
					comp_W_dot_od_sum += p_pc_W_dot_od[n_run];	//[MWe]
				}
				else
				{
					p_pc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_W_dot_od[n_run] = 0.0;
					p_pc_m_dot_od[n_run] = 0.0;
                    p_pc_rho_in_od[n_run] = 0.0;
                    p_pc_ideal_spec_work_od[n_run] = 0.0;
					p_pc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
						pm_pc_phi_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
					{
						pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
						pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					}
					p_pc_f_bypass_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();	//[-]
				}
					// Compressor Totals
				p_c_tot_W_dot_od[n_run] = comp_W_dot_od_sum;		//[MWe]
					// Turbine
				p_t_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_IN]*1.E-3);	//[MPa] convert from kPa
				p_t_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::TURB_OUT] - 273.15);	//[C] Convert from K
				p_t_P_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_OUT]*1.E-3);//[MPa] convert form kPa
				p_t_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_W_dot_out*1.E-3);	//[MWe] convert from kWe
				p_t_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t);			//[kg/s]
				p_t_delta_h_isen_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_delta_h_isen);	//[kJ/kg]
				p_t_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_rho_in);	//[kg/m3]
				p_t_nu_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
				p_t_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_eta;	//[-]
					// Recuperator
				p_LTR_HP_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_HP_OUT] - 273.15);	//[C] LTR HP outlet temp, convert from K
				p_eff_LTR_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_eff;	//[-]
				p_q_dot_LTR_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
				double LTR_LP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT] / 
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT];
				p_LTR_LP_deltaP_od[n_run] = (ssc_number_t)LTR_LP_deltaP_od;	//[-]
				double LTR_HP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT] /
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT];
				p_LTR_HP_deltaP_od[n_run] = (ssc_number_t)LTR_HP_deltaP_od;	//[-]
				p_LTR_min_dT_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_min_DT);	//[C]
				
				p_HTR_LP_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::HTR_LP_OUT] - 273.15);	//[C] HTR LP outlet temp, convert from K
				p_HTR_HP_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MIXER_OUT] - 273.15);	//[C] HTR HP inlet temp, convert from K
				p_eff_HTR_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_eff;	//[-]
				p_q_dot_HTR_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
				double HTR_LP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT] /
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_OUT];
				p_HTR_LP_deltaP_od[n_run] = (ssc_number_t)HTR_LP_deltaP_od;
				double HTR_HP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT];
				p_HTR_HP_deltaP_od[n_run] = (ssc_number_t)HTR_HP_deltaP_od;
				p_HTR_min_dT_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_min_DT);	//[C]
					// PHX
				p_T_co2_PHX_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT] - 273.15);	//[C]
				p_P_co2_PHX_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] * 1.E-3);  //[MPa] convert from kPa
				p_T_co2_PHX_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::TURB_IN] - 273.15);		//[C]
				p_deltaT_HTF_PHX_od[n_run] = p_T_htf_hot_od[n_run] - p_T_htf_cold_od[n_run];	//[C]
				p_phx_eff_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_phx_od_solved.m_eff;		//[-]
				double phx_co2_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_IN] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT];
				p_phx_co2_deltaP_od[n_run] = (ssc_number_t)phx_co2_deltaP_od;
					// Low Pressure Cooler
				p_mc_cooler_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT] - 273.15);	//[C] Convert from K
				p_mc_cooler_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_dens[C_sco2_cycle_core::LTR_LP_OUT]);	//[kg/m3]
				double LP_cooler_co2_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];
				p_mc_cooler_co2_deltaP_od[n_run] = (ssc_number_t)LP_cooler_co2_deltaP_od;	//[-]
				p_mc_cooler_W_dot_fan_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
				double cooler_W_dot_total = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
					// Intermediate Pressure Cooler
				if (cycle_config == 2)
				{
					p_pc_cooler_W_dot_fan_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
					cooler_W_dot_total += p_pc_cooler_W_dot_fan_od[n_run];	//[MWe]
				}
				else
				{
					p_pc_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_cooler_tot_W_dot_fan_od[n_run] = cooler_W_dot_total;	//[MWe]

				p_W_dot_net_less_cooling_od[n_run] = p_W_dot_net_od[n_run] - cooler_W_dot_total;	//[MWe]
                p_eta_thermal_net_less_cooling_od[n_run] = p_W_dot_net_less_cooling_od[n_run] / p_Q_dot_od[n_run];  //[-]

				double T_cooler_in_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT];	//[K]
				double P_cooler_in_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];	//[K]
				double P_cooler_out_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT];		//[MPa]
				int isen_enth_check_err = 0;
				double h_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double s_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double rho_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double T_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double h_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double s_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double rho_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double deltah_isen_od = std::numeric_limits<double>::quiet_NaN();

				calculate_turbomachinery_outlet_1(T_cooler_in_od, P_cooler_in_od, P_cooler_out_od, 1.0, true, isen_enth_check_err,
					h_cooler_in_od, s_cooler_in_od, rho_cooler_in_od, T_isen_out_od,
					h_isen_out_od, s_isen_out_od, rho_isen_out_od, deltah_isen_od);

				p_mc_cooler_in_isen_deltah_to_P_mc_out_od[n_run] = (ssc_number_t)-deltah_isen_od;		//[kJ/kg]

                // Columns(11) : 0) HTF Temp[C], 1) HTF ND mass flow[-], 2) Ambient Temp[C], 3) ND Power, 4) ND Heat, 5) ND Fan Power, 6) ND Water
                //               ...... 7) ND PHX deltaT, 8) ND P_co2_PHX_in, 9) ND m_dot_co2_PHX, 10) ND P_co2_turb_in
                if (is_od_generate_udpc_assigned)
                {
                    pm_udpc_table[n_run * 11 + 0] = (ssc_number_t)p_T_htf_hot_od[n_run];      //[C]
                    pm_udpc_table[n_run * 11 + 1] = (ssc_number_t)p_m_dot_htf_fracs[n_run];   //[-]
                    pm_udpc_table[n_run * 11 + 2] = (ssc_number_t)p_T_amb_od[n_run];          //[C]
                    pm_udpc_table[n_run * 11 + 3] = (ssc_number_t)(p_W_dot_net_od[n_run] / (c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_net*1.E-3));  //[-] 
                    pm_udpc_table[n_run * 11 + 4] = (ssc_number_t)(p_Q_dot_od[n_run] / (c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_Q_dot_design*1.E-3));  //[-]
                    pm_udpc_table[n_run * 11 + 5] = (ssc_number_t)(p_cooler_tot_W_dot_fan_od[n_run] / as_double("cooler_tot_W_dot_fan"));   //[-]
                    pm_udpc_table[n_run * 11 + 6] = (ssc_number_t) 0.0;
                    if (T_t_in_mode == 0)    // Model input is HTF hot temperature
                    {
                        pm_udpc_table[n_run * 11 + 7] = (ssc_number_t)((p_deltaT_HTF_PHX_od[n_run])/(T_htf_hot_des - T_htf_PHX_out_des));
                    }
                    else if (T_t_in_mode == 1)  // Model input is CO2 turbine inlet temperature
                    {
                        pm_udpc_table[n_run * 11 + 7] = (ssc_number_t)((p_T_co2_PHX_out_od[n_run]-p_T_co2_PHX_in_od[n_run]) / (T_t_in_des - T_co2_PHX_in_des));
                    }
                    pm_udpc_table[n_run * 11 + 8] = (ssc_number_t)((p_P_co2_PHX_in_od[n_run]) / (c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] * 1.E-3));
                    pm_udpc_table[n_run * 11 + 9] = (ssc_number_t)((p_t_m_dot_od[n_run]) / c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);
                    pm_udpc_table[n_run * 11 + 10] = (ssc_number_t)((p_t_P_in_od[n_run]) / (c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_IN] * 1.E-3));
                }
                // Energy Balance Checks
                double diff_m_dot, diff_E_cycle, diff_Q_LTR, diff_Q_HTR;
                diff_m_dot = diff_E_cycle = diff_Q_LTR = diff_Q_HTR = std::numeric_limits<double>::quiet_NaN();
                c_sco2_cycle.check_od_solution(diff_m_dot, diff_E_cycle, diff_Q_LTR, diff_Q_HTR);
                p_diff_m_dot_od[n_run] = (ssc_number_t) diff_m_dot;
                p_diff_E_cycle[n_run]  = (ssc_number_t) diff_E_cycle;
                p_diff_Q_LTR[n_run]    = (ssc_number_t) diff_Q_LTR;
                p_diff_Q_HTR[n_run]    = (ssc_number_t) diff_Q_HTR;
			}   
			else
			{	// Off-design call failed, write NaN outptus
                p_m_dot_htf_fracs[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Control parameters
				p_P_comp_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for(int i_s = 0; i_s < n_mc_stages; i_s++)
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_recomp_frac_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// System
				p_eta_thermal_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_mc_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_P_mc_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_htf_cold_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_m_dot_co2_full_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_W_dot_net_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_Q_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_eta_thermal_net_less_cooling_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Compressor
				p_mc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_ideal_spec_work_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_mc_N_od_perc[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_mc_f_bypass_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recompressor
				p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                for (int i_s = 0; i_s < n_rc_stages; i_s++)
                {
                    pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_rc_psi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                }
				p_rc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_rc_N_od_perc[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_rc_stages; i_s++)
				{
					pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
					// Precompressor
				p_pc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_pc_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_pc_ideal_spec_work_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_pc_stages; i_s++)
					pm_pc_phi_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_pc_stages; i_s++)
				{
					pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_pc_f_bypass_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Compressor Totals
				p_c_tot_W_dot_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Turbine
				p_t_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_delta_h_isen_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_nu_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_tip_ratio_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recuperator
				p_LTR_HP_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_eff_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_LP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_HP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_min_dT_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();

				p_HTR_LP_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_HP_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_eff_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_LP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_HP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_min_dT_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// PHX
				p_T_co2_PHX_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_P_co2_PHX_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_co2_PHX_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_deltaT_HTF_PHX_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_phx_eff_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_phx_co2_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Low Pressure Cooler
				p_mc_cooler_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_cooler_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_cooler_co2_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Intermediate Pressure Cooler
				p_pc_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Coolerl Totals
				p_cooler_tot_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_W_dot_net_less_cooling_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    // Energy Balance Checks
                p_diff_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_diff_E_cycle[n_run]  = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_diff_Q_LTR[n_run]    = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_diff_Q_HTR[n_run]    = std::numeric_limits<ssc_number_t>::quiet_NaN();

				p_mc_cooler_in_isen_deltah_to_P_mc_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();

                // Columns(7) : HTF Temp[C], HTF ND mass flow[-], Ambient Temp[C], ND Power, ND Heat, ND Fan Power, ND Water
                if (is_od_generate_udpc_assigned)
                {
                    pm_udpc_table[n_run * 11 + 0] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 1] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 2] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 3] = std::numeric_limits<ssc_number_t>::quiet_NaN(); 
                    pm_udpc_table[n_run * 11 + 4] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 5] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 6] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 7] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 8] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 9] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_udpc_table[n_run * 11 + 10] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                }
			}

		}


		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg + "\n");
		}
		
	}

	void allocate_ssc_outputs(int n_od_runs, int n_mc_stages, int n_rc_stages, int n_pc_stages, bool is_udpc_table)
	{
		// Off-design parameters
		p_m_dot_htf_fracs = allocate("m_dot_htf_fracs", n_od_runs);
		p_T_amb_od = allocate("T_amb_od", n_od_runs);
		p_T_htf_hot_od = allocate("T_htf_hot_od", n_od_runs);
		// Optimized control parameters
		p_P_comp_in_od = allocate("P_comp_in_od", n_od_runs);
		pm_mc_phi_od = allocate("mc_phi_od", n_od_runs, n_mc_stages);
		
		p_recomp_frac_od = allocate("recomp_frac_od", n_od_runs);
		// Optimizer parameters
		p_sim_time_od = allocate("sim_time_od", n_od_runs);
		// Systems
		p_eta_thermal_od = allocate("eta_thermal_od", n_od_runs);
		p_T_mc_in_od = allocate("T_mc_in_od", n_od_runs);
		p_P_mc_out_od = allocate("P_mc_out_od", n_od_runs);
		p_T_htf_cold_od = allocate("T_htf_cold_od", n_od_runs);
		p_m_dot_co2_full_od = allocate("m_dot_co2_full_od", n_od_runs);
		p_W_dot_net_od = allocate("W_dot_net_od", n_od_runs);
		p_Q_dot_od = allocate("Q_dot_od", n_od_runs);
        p_eta_thermal_net_less_cooling_od = allocate("eta_thermal_net_less_cooling_od", n_od_runs);
		// Compressor
		p_mc_T_out_od = allocate("mc_T_out_od", n_od_runs);
		p_mc_W_dot_od = allocate("mc_W_dot_od", n_od_runs);
		p_mc_m_dot_od = allocate("mc_m_dot_od", n_od_runs);
		p_mc_rho_in_od = allocate("mc_rho_in_od", n_od_runs);
        pm_mc_psi_od = allocate("mc_psi_od", n_od_runs, n_mc_stages);
		p_mc_ideal_spec_work_od = allocate("mc_ideal_spec_work_od", n_od_runs);
		p_mc_N_od = allocate("mc_N_od", n_od_runs);
        p_mc_N_od_perc = allocate("mc_N_od_perc", n_od_runs);
		p_mc_eta_od = allocate("mc_eta_od", n_od_runs);
		pm_mc_tip_ratio_od = allocate("mc_tip_ratio_od", n_od_runs, n_mc_stages);
		pm_mc_eta_stages_od = allocate("mc_eta_stages_od", n_od_runs, n_mc_stages);
		p_mc_f_bypass_od = allocate("mc_f_bypass_od", n_od_runs);
		// Recompressor
		p_rc_T_in_od = allocate("rc_T_in_od", n_od_runs);
		p_rc_P_in_od = allocate("rc_P_in_od", n_od_runs);
		p_rc_T_out_od = allocate("rc_T_out_od", n_od_runs);
		p_rc_P_out_od = allocate("rc_P_out_od", n_od_runs);
		p_rc_W_dot_od = allocate("rc_W_dot_od", n_od_runs);
		p_rc_m_dot_od = allocate("rc_m_dot_od", n_od_runs);
		p_rc_eta_od = allocate("rc_eta_od", n_od_runs);
		pm_rc_phi_od = allocate("rc_phi_od", n_od_runs, n_rc_stages);
        pm_rc_psi_od = allocate("rc_psi_od", n_od_runs, n_rc_stages);
		p_rc_N_od = allocate("rc_N_od", n_od_runs);
        p_rc_N_od_perc = allocate("rc_N_od_perc", n_od_runs);
		pm_rc_tip_ratio_od = allocate("rc_tip_ratio_od", n_od_runs, n_rc_stages);
		pm_rc_eta_stages_od = allocate("rc_eta_stages_od", n_od_runs, n_rc_stages);
		// Precompressor
		p_pc_T_in_od = allocate("pc_T_in_od", n_od_runs);
		p_pc_P_in_od = allocate("pc_P_in_od", n_od_runs);
		p_pc_W_dot_od = allocate("pc_W_dot_od", n_od_runs);
		p_pc_m_dot_od = allocate("pc_m_dot_od", n_od_runs);
        p_pc_rho_in_od = allocate("pc_rho_in_od", n_od_runs);
        p_pc_ideal_spec_work_od = allocate("pc_ideal_spec_work_od", n_od_runs);
		p_pc_eta_od = allocate("pc_eta_od", n_od_runs);
		pm_pc_phi_od = allocate("pc_phi_od", n_od_runs, n_pc_stages);
		p_pc_N_od = allocate("pc_N_od", n_od_runs);
		pm_pc_tip_ratio_od = allocate("pc_tip_ratio_od", n_od_runs, n_pc_stages);
		pm_pc_eta_stages_od = allocate("pc_eta_stages_od", n_od_runs, n_pc_stages);
		p_pc_f_bypass_od = allocate("pc_f_bypass_od", n_od_runs);
		// Compressor Totals
		p_c_tot_W_dot_od = allocate("c_tot_W_dot_od", n_od_runs);
		// Turbine
		p_t_P_in_od = allocate("t_P_in_od", n_od_runs);
		p_t_T_out_od = allocate("t_T_out_od", n_od_runs);
		p_t_P_out_od = allocate("t_P_out_od", n_od_runs);
		p_t_W_dot_od = allocate("t_W_dot_od", n_od_runs);
		p_t_m_dot_od = allocate("t_m_dot_od", n_od_runs);
		p_t_delta_h_isen_od = allocate("t_delta_h_isen_od", n_od_runs);
		p_t_rho_in_od = allocate("t_rho_in_od", n_od_runs);
		p_t_nu_od = allocate("t_nu_od", n_od_runs);
		p_t_N_od = allocate("t_N_od", n_od_runs);
		p_t_tip_ratio_od = allocate("t_tip_ratio_od", n_od_runs);
		p_t_eta_od = allocate("t_eta_od", n_od_runs);
		// Recuperator
		p_LTR_HP_T_out_od = allocate("LTR_HP_T_out_od", n_od_runs);
		p_eff_LTR_od = allocate("eff_LTR_od", n_od_runs);
		p_q_dot_LTR_od = allocate("q_dot_LTR_od", n_od_runs);
		p_LTR_LP_deltaP_od = allocate("LTR_LP_deltaP_od", n_od_runs);
		p_LTR_HP_deltaP_od = allocate("LTR_HP_deltaP_od", n_od_runs);
		p_LTR_min_dT_od = allocate("LTR_min_dT_od", n_od_runs);

		p_HTR_LP_T_out_od = allocate("HTR_LP_T_out_od", n_od_runs);
		p_HTR_HP_T_in_od = allocate("HTR_HP_T_in_od", n_od_runs);
		p_eff_HTR_od = allocate("eff_HTR_od", n_od_runs);
		p_q_dot_HTR_od = allocate("q_dot_HTR_od", n_od_runs);
		p_HTR_LP_deltaP_od = allocate("HTR_LP_deltaP_od", n_od_runs);
		p_HTR_HP_deltaP_od = allocate("HTR_HP_deltaP_od", n_od_runs);
		p_HTR_min_dT_od = allocate("HTR_min_dT_od", n_od_runs);
		// PHX
		p_T_co2_PHX_in_od = allocate("T_co2_PHX_in_od", n_od_runs);
		p_P_co2_PHX_in_od = allocate("P_co2_PHX_in_od", n_od_runs);
		p_T_co2_PHX_out_od = allocate("T_co2_PHX_out_od", n_od_runs);
		p_deltaT_HTF_PHX_od = allocate("deltaT_HTF_PHX_od", n_od_runs);
		p_phx_eff_od = allocate("phx_eff_od", n_od_runs);
		p_phx_co2_deltaP_od = allocate("phx_co2_deltaP_od", n_od_runs);
		// Low Pressure Cooler
		p_mc_cooler_T_in_od = allocate("mc_cooler_T_in_od", n_od_runs);
		p_mc_cooler_rho_in_od = allocate("mc_cooler_rho_in_od", n_od_runs);
		p_mc_cooler_in_isen_deltah_to_P_mc_out_od = allocate("mc_cooler_in_isen_deltah_to_P_mc_out_od", n_od_runs);
		p_mc_cooler_co2_deltaP_od = allocate("mc_cooler_co2_deltaP_od", n_od_runs);
		p_mc_cooler_W_dot_fan_od = allocate("mc_cooler_W_dot_fan_od", n_od_runs);
		// Intermediate Pressure Cooler
		p_pc_cooler_W_dot_fan_od = allocate("pc_cooler_W_dot_fan_od", n_od_runs);
		// Cooler Totals
		p_cooler_tot_W_dot_fan_od = allocate("cooler_tot_W_dot_fan_od", n_od_runs);
		p_W_dot_net_less_cooling_od = allocate("W_dot_net_less_cooling_od", n_od_runs);
        // Energy Balance Checks
        p_diff_m_dot_od = allocate("diff_m_dot_od", n_od_runs);
        p_diff_E_cycle  = allocate("diff_E_cycle", n_od_runs);
        p_diff_Q_LTR    = allocate("diff_Q_LTR", n_od_runs);
        p_diff_Q_HTR    = allocate("diff_Q_HTR", n_od_runs);
        // UDPC Table
        if (is_udpc_table)
        {
            pm_udpc_table = allocate("udpc_table", n_od_runs, 11);
        }
		// Solver Metrics
		p_od_code = allocate("od_code", n_od_runs);

		return;
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_system, "...", 0)



static var_info _cm_vtab_sco2_comp_curves[] = {
	/*  VARTYPE   DATATYPE         NAME                  LABEL                                     UNITS META GROUP REQUIRED_IF CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "comp_type",     "Integer corresponding to compressor model",        "-",  "",  "",     "*",  "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_comp_in",     "Compressor inlet temperature",                     "C",  "",  "",     "*",  "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_comp_in",     "Compressor inlet pressure",                        "MPa","",  "",     "*",  "",  "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "phi_design",    "Design flow coefficient",                          "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "psi_design",    "Design isentropic head coefficient",               "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_norm_design","Normalized design isentropic efficiency",         "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phi",           "Array of flow coefficients",                       "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phi_ND",        "Array of normalized flow coefficients",            "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "psi",           "Array of isentropic head coefficients at phi",     "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "psi_ND",        "Array of normalized isentropic head coefficients at phi",     "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eta",           "Array of efficiencies at phi",                     "-",  "",  "",     "*",  "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eta_ND",        "Array of normalized efficiencies at phi",          "-",  "",  "",     "*",  "",  "" },

	var_info_invalid };

int sco2_comp_curves_common(compute_module* cm)
{
	int comp_type = cm->as_integer("comp_type");
	double T_comp_in = cm->as_double("T_comp_in") + 273.15;     //[K] convert from C
	double P_comp_in = cm->as_double("P_comp_in") * 1.E3;         //[kPa] convert from MPa

	if (comp_type != C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby)
	{
		throw exec_error("sco2_comp_curves", "Compressor type invalid");
	}

	std::unique_ptr<C_comp__psi_eta_vs_phi> p_c_comp = C_comp__psi_eta_vs_phi::construct_derived_C_comp__psi_eta_vs_phi(comp_type);
	double phi_in = p_c_comp->calc_phi_design(T_comp_in, P_comp_in);
	p_c_comp->set_design_solution(phi_in, T_comp_in, P_comp_in);


	std::vector<double> phi;
	std::vector<double> psi;
	std::vector<double> eta;
	double eta_norm_map = std::numeric_limits<double>::quiet_NaN();
	p_c_comp->report_phi_psi_eta_vectors(phi, psi, eta, eta_norm_map);

	// Get compressor design phi, psi, eta
	double phi_design = std::numeric_limits<double>::quiet_NaN();
	double psi_design = std::numeric_limits<double>::quiet_NaN();
	double eta_norm_comp_design = std::numeric_limits<double>::quiet_NaN();
	if (comp_type == C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby)
	{
		phi_design = p_c_comp->calc_phi_design(T_comp_in, P_comp_in);
		psi_design = p_c_comp->calc_psi_isen_design(T_comp_in, P_comp_in);
		eta_norm_comp_design = 1.0;
	}
	else
	{
		throw exec_error("sco2_comp_curves", "Compressor type invalid");
	}

	double eta_comp_scale = eta_norm_map / eta_norm_comp_design;

	cm->assign("phi_design", (ssc_number_t)phi_design);
	cm->assign("psi_design", (ssc_number_t)psi_design);
	cm->assign("eta_norm_design", (ssc_number_t)eta_comp_scale);

	size_t n_phi = phi.size();

	ssc_number_t* p_phi = cm->allocate("phi", n_phi);
	ssc_number_t* p_phi_nd = cm->allocate("phi_ND", n_phi);
	ssc_number_t* p_psi = cm->allocate("psi", n_phi);
	ssc_number_t* p_psi_nd = cm->allocate("psi_ND", n_phi);
	ssc_number_t* p_eta = cm->allocate("eta", n_phi);
	ssc_number_t* p_eta_nd = cm->allocate("eta_ND", n_phi);

	for (size_t i = 0; i < n_phi; i++)
	{
		p_phi[i] = phi[i];
		p_phi_nd[i] = phi[i] / phi_design;
		p_psi[i] = psi[i];
		p_psi_nd[i] = psi[i] / psi_design;
		p_eta[i] = eta[i];
		p_eta_nd[i] = eta[i] * eta_comp_scale;
	}

	return 0;
}

class cm_sco2_comp_curves : public compute_module
{
public:

	cm_sco2_comp_curves()
	{
		add_var_info(_cm_vtab_sco2_comp_curves);
	}

	void exec() override
	{
		sco2_comp_curves_common(this);
	}

};

DEFINE_MODULE_ENTRY(sco2_comp_curves, "Calls sCO2 auto-design cycle function", 1)
