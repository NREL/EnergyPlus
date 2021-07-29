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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
#include "sam_csp_util.h"
#include "storage_hx.h"
#include "thermocline_tes.h"

#include "CO2_properties.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"

using namespace std;

/* CSP Plant Controller Model
Type 251
Author: Michael J. Wagner
Converted from Fortran to c++ November 2012 by Ty Neises */

enum {
	//Parameters
	P_field_fl,
	P_field_fl_props,
	P_store_fl,
	P_store_fl_props,
	P_tshours,
    P_eta_pump,
    P_hdr_rough,
	P_is_hx,
	P_dt_hot,
	P_dt_cold,
	P_hx_config,
	P_q_max_aux,
	P_lhv_eff,
	P_T_set_aux,	
	P_V_tank_hot_ini,
	P_T_tank_hot_ini,
	P_T_tank_cold_ini,
	P_vol_tank,
	P_h_tank,
	P_h_tank_min,
	P_u_tank,
	P_tank_pairs,
	P_cold_tank_Thtr,
	P_hot_tank_Thtr,	
	P_cold_tank_max_heat,
	P_hot_tank_max_heat,
    P_tanks_in_parallel,
    P_hot_tank_bypass,
    P_T_tank_hot_in_min,
    P_des_pipe_vals,
	P_T_field_in_des,
	P_T_field_out_des,
	P_q_pb_design,
	P_W_pb_design,
	P_cycle_max_frac,
	P_cycle_cutoff_frac,
	P_solarm,
	P_pb_pump_coef,
	P_tes_pump_coef,
    P_V_tes_des,
    P_custom_tes_p_loss,
    P_k_tes_loss_coeffs,
    P_custom_sgs_pipe_sizes,
    P_sgs_diams,
    P_sgs_wallthicks,
    P_sgs_lengths,
    P_dp_sgs,
	P_pb_fixed_par,
	P_bop_array,
	P_aux_array,
	P_T_startup,
	P_fossil_mode,
	P_fthr_ok,
	P_nSCA,
	P_I_bn_des,
	P_fc_on,
	P_q_sby_frac,
	P_t_standby_init,
	P_sf_type,
	P_tes_type,
	P_tslogic_a,
	P_tslogic_b,
	P_tslogic_c,
	P_ffrac,
	P_tc_fill,
	P_tc_void,
	P_t_dis_out_min,
	P_t_ch_out_max,
	P_nodes,
	P_f_tc_cold,
	P_PB_TECH_TYPE,
	
	//Inputs
	I_I_bn,
	I_m_dot_field,
	I_m_dot_htf_ref,
	I_T_field_out,
	I_T_pb_out,
	I_T_amb,
	// I_m_pb_demand,
	// I_q_startup,
	I_dnifc,
	I_TOUPeriod,
    I_T_field_in_at_des,
    I_T_field_out_at_des,
    I_P_field_in_at_des,
    I_defocus,
	I_T_HTF_COLD_DES,

	// I_W_DOT_NET,
	// I_T_MC_IN,
	// I_T_T_IN,
	// I_P_MC_IN,
	// I_P_MC_OUT,
	// I_UA_LT,
	// I_UA_HT,
	// I_RECOMP_FRAC,
	// I_ETA_MC,
	// I_ETA_RC,
	// I_ETA_T,
	// I_N_SUB_HXRS,
	// I_P_HIGH_LIMIT,
	// I_N_turbine,
	// I_DP_LT_C,
	// I_DP_LT_H,
	// I_DP_HT_C,
	// I_DP_HT_H,
	// I_DP_PC_H,
	// I_DP_PHX_C,
	// I_DELTAT_MC,
	// I_DELTAT_T,

	//Outputs
    O_V_sgs,
    O_D_sgs,
    O_wall_thk_sgs,
    O_m_dot_des_sgs,
    O_vel_des_sgs,
    O_t_des_sgs,
    O_p_des_sgs,
    O_p_des_sgs_1,
	O_defocus,
    O_recirc,
	O_standby,   
	O_m_dot_pb,  
	O_T_pb_in,   
	O_T_field_in,
	O_charge_field,   
	O_charge_tank, 
	O_Ts_hot,    
	O_Ts_cold,
	O_T_tank_hot_in,    
	O_T_tank_cold_in,   
	O_vol_tank_hot_fin, 
	O_vol_tank_cold_fin,
	O_T_tank_hot_fin, 
	O_T_tank_cold_fin,
	O_q_par_fp,       
	O_m_dot_aux,      
	O_q_aux_heat,
	O_q_aux_fuel,
	O_vol_tank_total, 
	O_hx_eff,         
	O_mass_tank_hot,  
	O_mass_tank_cold, 
	O_mass_tank_total,
	O_htf_pump_power, 
	O_bop_par,        
	O_fixed_par,      
	O_aux_par,        
	O_q_pb,           
	O_tank_losses,    
	O_q_to_tes,       
	O_mode,
	O_TOU,
	O_T_hot_node,
	O_T_cold_node,
	O_T_max,
	O_f_hot,
	O_f_cold,

	//Include N_max
	N_MAX };
	
tcsvarinfo sam_mw_trough_type251_variables[] = {
	// vartype,        datatype,        index,                name,                  label,                                                    units            meta    group   default_value
	// PARAMETERS																     
	{ TCS_PARAM,    TCS_NUMBER,        P_field_fl,           "field_fluid",          "Material number for the collector field",                 "-",            "",        "",        ""},
	{ TCS_PARAM,    TCS_MATRIX,        P_field_fl_props,     "field_fl_props",       "User defined field fluid property data",                  "-",            "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},			
	{ TCS_PARAM,    TCS_NUMBER,        P_store_fl,           "store_fluid",          "Material number for storage fluid",                       "-",            "",        "",        ""},
	{ TCS_PARAM,    TCS_MATRIX,        P_store_fl_props,     "store_fl_props",       "User defined fluid property data",                        "-",            "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},
	{ TCS_PARAM,    TCS_NUMBER,        P_tshours,            "tshours",              "Equivalent full-load thermal storage hours",              "hr",           "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_eta_pump,           "eta_pump",             "HTF pump efficiency",                                     "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_hdr_rough,          "HDR_rough",            "Header pipe roughness - used as general pipe roughness",  "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_is_hx,              "is_hx",                "1=yes, 0=no"                                              "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_dt_hot,             "dt_hot",               "Hot side HX approach temp",                               "C",            "",        "",        ""},    
    { TCS_PARAM,    TCS_NUMBER,        P_dt_cold,            "dt_cold",              "Cold side HX approach temp",                              "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_hx_config,          "hx_config",            "HX configuration",                                        "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_q_max_aux,          "q_max_aux",            "Max heat rate of auxiliary heater",                       "MWt",          "",        "",        ""},
	{ TCS_PARAM,    TCS_NUMBER,        P_lhv_eff,            "lhv_eff",              "Fuel LHV efficiency (0..1)",                              "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_set_aux,          "T_set_aux",            "Aux heater outlet temp set point",                        "C",            "",        "",        ""},    
    { TCS_PARAM,    TCS_NUMBER,        P_V_tank_hot_ini,     "V_tank_hot_ini",       "Initial hot tank fluid volume",                           "m3",           "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_tank_hot_ini,     "T_tank_hot_ini",       "Initial hot tank fluid temperature",                      "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_tank_cold_ini,    "T_tank_cold_ini",      "Initial cold tank fluid temperature",                     "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_vol_tank,           "vol_tank",             "Total tank volume, including unusable HTF at bottom",     "m3",           "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_h_tank,             "h_tank",               "Total height of tank (height of HTF when tank is full",   "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_h_tank_min,         "h_tank_min",           "Minimum allowable HTF height in storage tank",            "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_u_tank,             "u_tank",               "Loss coefficient from the tank",                          "W/m2-K",       "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_tank_pairs,         "tank_pairs",           "Number of equivalent tank pairs",                         "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_cold_tank_Thtr,     "cold_tank_Thtr",       "Minimum allowable cold tank HTF temp",                    "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_hot_tank_Thtr,      "hot_tank_Thtr",        "Minimum allowable hot tank HTF temp",                     "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_cold_tank_max_heat, "cold_tank_max_heat",   "Rated heater capacity for cold tank heating",             "MW",           "",        "",        ""},
	{ TCS_PARAM,    TCS_NUMBER,        P_hot_tank_max_heat,  "hot_tank_max_heat",    "Rated heater capacity for hot tank heating",              "MW",           "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_tanks_in_parallel,  "tanks_in_parallel",    "Tanks are in parallel, not in series, with solar field",  "-",            "",        "",    "true"},
    { TCS_PARAM,    TCS_NUMBER,        P_hot_tank_bypass,    "has_hot_tank_bypass",  "Bypass valve connects field outlet to cold tank",         "-",            "",        "",   "false"},
    { TCS_PARAM,    TCS_NUMBER,        P_T_tank_hot_in_min,  "T_tank_hot_inlet_min", "Minimum hot tank htf inlet temperature",                  "C",            "",        "",     "400"},
    { TCS_PARAM,    TCS_NUMBER,        P_des_pipe_vals,      "calc_design_pipe_vals", "Calculate pipe temps and pressures at design conditions", "-",           "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_field_in_des,     "T_field_in_des",       "Field design inlet temperature",                          "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_field_out_des,    "T_field_out_des",      "Field design outlet temperature",                         "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_q_pb_design,        "q_pb_design",          "Design heat input to power block",                        "MWt",          "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_W_pb_design,        "W_pb_design",          "Rated plant capacity",                                    "MWe",          "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_cycle_max_frac,     "cycle_max_frac",       "Maximum turbine over design operation fraction",          "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_cycle_cutoff_frac,  "cycle_cutoff_frac",    "Minimum turbine operation fraction before shutdown",      "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_solarm,             "solarm",               "Solar Multiple",                                          "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_pb_pump_coef,       "pb_pump_coef",         "Pumping power to move 1kg of HTF through PB loop",        "kW/(kg/s)",    "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_tes_pump_coef,      "tes_pump_coef",        "Pumping power to move 1kg of HTF through tes loop",       "kW/(kg/s)",    "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_V_tes_des,          "V_tes_des",            "Design-point velocity to size the TES pipe diameters",    "m/s",          "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_custom_tes_p_loss,  "custom_tes_p_loss",    "TES pipe losses are based on custom lengths and coeffs",  "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_k_tes_loss_coeffs,  "k_tes_loss_coeffs",    "Minor loss coeffs for the coll, gen, and bypass loops",   "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_custom_sgs_pipe_sizes, "custom_sgs_pipe_sizes", "Use custom SGS pipe diams, wallthks, and lengths",    "-",            "",        "",   "false"},
    { TCS_PARAM,    TCS_ARRAY,         P_sgs_diams,          "sgs_diams",            "Custom SGS diameters",                                    "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_sgs_wallthicks,     "sgs_wallthicks",       "Custom SGS wall thicknesses",                             "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_sgs_lengths,        "sgs_lengths",          "Custom SGS lengths",                                      "m",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_dp_sgs,             "DP_SGS",               "Pressure drop within the steam generator",                "bar",          "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_pb_fixed_par,       "pb_fixed_par",         "Fraction of rated gross power constantly consumed",       "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_bop_array,          "bop_array",            "Coefficients for balance of plant parasitics calcs",      "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_aux_array,          "aux_array",            "Coefficients for auxiliary heater parasitics calcs",      "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_T_startup,          "T_startup",            "Startup temperature",                                     "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_fossil_mode,        "fossil_mode",          "Fossil backup mode 1=Normal 2=Topping",                   "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_fthr_ok,            "fthr_ok",              "Does the defocus control allow partial defocusing",       "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_nSCA,               "nSCA",                 "Number of SCAs in a single loop",                         "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_I_bn_des,           "I_bn_des",             "Design point irradiation value",                          "W/m2",         "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_fc_on,              "fc_on",                "DNI forecasting enabled",                                 "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_q_sby_frac,         "q_sby_frac",           "Fraction of thermal power required for standby",          "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_t_standby_init,     "t_standby_reset",      "Maximum allowable time for PB standby operation",         "hr",           "",        "",        ""},
	{ TCS_PARAM,    TCS_NUMBER,        P_sf_type,            "sf_type",              "Solar field type, 1 = trough & MSLF, 2 = tower",          "-",            "",        "",        ""},
	{ TCS_PARAM,    TCS_NUMBER,        P_tes_type,           "tes_type",             "1=2-tank, 2=thermocline",                                 "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_tslogic_a,          "tslogic_a",            "Dispatch logic without solar",                            "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_tslogic_b,          "tslogic_b",            "Dispatch logic with solar",                               "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_tslogic_c,          "tslogic_c",            "Dispatch logic for turbine load fraction",                "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_ARRAY,         P_ffrac,              "ffrac",                "Fossil dispatch logic",                                   "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_tc_fill,            "tc_fill",              "Thermocline fill material",                               "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_tc_void,            "tc_void",              "Thermocline void fraction",                               "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_t_dis_out_min,      "t_dis_out_min",        "Min allowable hot side outlet temp during discharge",     "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_t_ch_out_max,       "t_ch_out_max",         "Max allowable cold side outlet temp during charge",       "C",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_nodes,              "nodes",                "Nodes modeled in the flow path",                          "-",            "",        "",        ""},
    { TCS_PARAM,    TCS_NUMBER,        P_f_tc_cold,          "f_tc_cold",            "0=entire tank is hot, 1=entire tank is cold",             "-",            "",        "",        ""},
		// 10.1.14 twn: added for sCO2 cycle logic
	{ TCS_PARAM,    TCS_NUMBER,        P_PB_TECH_TYPE,       "pb_tech_type",         "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)","none","",        "",        "2"},

    // INPUTS
    { TCS_INPUT,    TCS_NUMBER,        I_I_bn,               "I_bn",                 "Direct beam irradiance",                                  "W/m2",         "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_m_dot_field,        "m_dot_field",          "Mass flow rate from the field",                           "kg/hr",        "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_m_dot_htf_ref,      "m_dot_htf_ref",        "Reference HTF flow rate at design conditions",            "kg/hr",        "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_T_field_out,        "T_field_out",          "HTF temperature from the field",                          "C",            "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_T_pb_out,           "T_pb_out",             "Fluid temperature from the power block",                  "C",            "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_T_amb,              "T_amb",                "Ambient temperature",                                     "C",            "",        "",        ""},
    //{ TCS_INPUT,    TCS_NUMBER,        I_m_pb_demand,        "m_pb_demand",          "Demand htf flow from the PB",                             "kg/hr",        "",        "",        ""},
    //{ TCS_INPUT,    TCS_NUMBER,        I_q_startup,          "q_startup",            "Startup energy reported by the collector field",          "MWt-hr",       "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_dnifc,              "dnifc",                "Forecast DNI",                                            "W/m2",         "",        "",        ""},
	{ TCS_INPUT,    TCS_NUMBER,        I_TOUPeriod,          "TOUPeriod",            "The time-of-use period",                                   "",            "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_T_field_in_at_des,  "T_field_in_at_des",    "Field inlet temperature at design conditions",            "C",           "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_T_field_out_at_des, "T_field_out_at_des",   "Field outlet temperature at design conditions",           "C",           "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_P_field_in_at_des,  "P_field_in_at_des",    "Field inlet pressure at design conditions",               "bar",         "",        "",        ""},
    { TCS_INPUT,    TCS_NUMBER,        I_defocus,            "defocus_prev",         "Previous relative defocus",                               "-",           "",        "",        ""},

	// sCO2 cycle design parameters from type 424 - only used if "pb_tech_type" = 424
	{ TCS_OUTPUT, TCS_NUMBER, I_T_HTF_COLD_DES,  "i_T_htf_cold_des",    "Calculated htf cold temperature at design",             "C",     "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_W_DOT_NET,       "i_W_dot_net",         "Target net cycle power",                                "kW",    "",  "",  "0.0" },
	//{ TCS_INPUT, TCS_NUMBER, I_T_MC_IN,         "i_T_mc_in",           "Compressor inlet temperature",                          "K",     "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_T_T_IN,          "i_T_t_in",            "Turbine inlet temperature",                             "K",     "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_P_MC_IN,         "i_P_mc_in",           "Compressor inlet pressure",                             "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_P_MC_OUT,        "i_P_mc_out",          "Compressor outlet pressure",                            "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_UA_LT,           "i_UA_LT",             "UA in LTR",                                             "kW/K",  "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_UA_HT,           "i_UA_HT",             "UA in HTR",                                             "kW/K",  "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_RECOMP_FRAC,     "i_recomp_frac",       "recompresson fraction",                                 "",      "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_ETA_MC,          "i_eta_mc",            "main compressor isentropic efficiency",                 "",      "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_ETA_RC,          "i_eta_rc",            "re-compressor isentropic efficiency",                   "",      "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_ETA_T,           "i_eta_t",             "turbine isentropic efficiency",                         "",      "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_N_SUB_HXRS,      "i_N_sub_hxrs",        "number of sub heat exchangers",                         "",      "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_P_HIGH_LIMIT,    "i_P_high_limit",      "high pressure limit",                                   "MPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_N_turbine,       "i_N_turbine",         "Turbine shaft speed",                                   "rpm",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_LT_C,         "o_DP_LT_c",           "Cold-side pressure drop - LT recup",                    "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_LT_H,         "o_DP_LT_h",           "Hot-side pressure drop - LT recup",                     "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_HT_C,         "o_DP_HT_c",           "Cold-side pressure drop - HT recup",                    "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_HT_H,         "o_DP_HT_h",           "Hot-side pressure drop - HT recup",                     "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_PC_H,         "o_DP_PC_h",           "Hot-side pressure drop - pre-cooler",                   "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DP_PHX_C,        "o_DP_PHX_c",          "Cold-side pressure drop - PHX",                         "kPa",   "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DELTAT_MC,       "o_deltaT_mc",         "Temperature difference btw comp inlet and Tamb",        "K",     "",  "",  "" },
	//{ TCS_INPUT, TCS_NUMBER, I_DELTAT_T,        "o_deltaT_t",          "Temperature difference btw hot HTF and turbine inlet",  "K",     "",  "",  "" },

    // OUTPUTS
    { TCS_OUTPUT,   TCS_NUMBER,        O_V_sgs,              "SGS_vol_tot",          "HTF volume in SGS minus bypass loop",                    "m3",            "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_D_sgs,              "SGS_diams",            "Pipe diameters in SGS",                                  "m",             "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_wall_thk_sgs,       "SGS_wall_thk",         "Pipe wall thickness in SGS",                             "m",             "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_m_dot_des_sgs,      "SGS_m_dot_des",        "Mass flow SGS pipes at design conditions",               "kg/s",          "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_vel_des_sgs,        "SGS_vel_des",          "Velocity in SGS pipes at design conditions",             "m/s",           "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_t_des_sgs,          "SGS_T_des",            "Temperature in SGS pipes at design conditions",          "C",             "",        "",        ""},
    { TCS_OUTPUT,   TCS_ARRAY,         O_p_des_sgs,          "SGS_P_des",            "Pressure in SGS pipes at design conditions",             "bar",           "",        "",        ""},
    { TCS_OUTPUT,   TCS_NUMBER,        O_p_des_sgs_1,        "SGS_P_des_1",          "Pressure in first SGS pipe section at design conditions", "bar",          "",        "",        ""},
    { TCS_OUTPUT,   TCS_NUMBER,        O_defocus,            "defocus",              "Absolute defocus = defocus_abs_prev * defocus_rel",       "-",            "",        "",        ""},
    { TCS_OUTPUT,   TCS_NUMBER,        O_recirc,             "recirculating",        "Field recirculating bypass valve control",                "-",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,	       O_standby,            "standby_control",      "Standby control flag",                                    "-",            "",        "",        ""},
    { TCS_OUTPUT,   TCS_NUMBER,        O_m_dot_pb,           "m_dot_pb",             "Mass flow rate of HTF to PB",                             "kg/hr",        "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_pb_in,            "T_pb_in",              "HTF temperature to power block",                          "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_field_in,         "T_field_in",           "HTF temperature into collector field header",             "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_charge_field,       "m_dot_charge_field",   "Mass flow rate on field side of HX",                      "kg/hr",        "",        "",        ""}, 	
	{ TCS_OUTPUT,   TCS_NUMBER,        O_charge_tank,        "m_dot_discharge_tank", "Mass flow rate on storage side of HX",                    "kg/hr",        "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_Ts_hot,             "Ts_hot",               "Field/pb HTF exiting HX (or hot tank) during discharge",  "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_Ts_cold,            "Ts_cold",              "Field/pb HTF exiting HX (or cold tank) during charge",    "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_tank_hot_in,      "T_tank_hot_in",        "Hot tank HTF inlet temperature",                          "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_tank_cold_in,     "T_tank_cold_in",       "Cold tank HTF inlet temperature",                         "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_vol_tank_hot_fin,   "vol_tank_hot_fin",     "Hot tank HTF volume at end of timestep",                  "m3",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_vol_tank_cold_fin,  "vol_tank_cold_fin",    "Cold tank HTF volume at end of timestep",                 "m3",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_tank_hot_fin,     "T_tank_hot_fin",       "Hot tank HTF temperature at end of timestep",             "K",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_tank_cold_fin,    "T_tank_cold_fin",      "Cold tank HTF temperature at end of timestep",            "K",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_q_par_fp,           "tank_fp_par",          "Total parasitic power required for tank freeze protect.", "MWe",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_m_dot_aux,          "m_dot_aux",            "Auxiliary heater mass flow rate",                         "kg/hr",        "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_q_aux_heat,         "q_aux_heat",           "Thermal energy provided to fluid by aux heater",          "MWt",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_q_aux_fuel,         "q_aux_fuel",           "Heat content of fuel required to provide aux heat",       "MMBTU",        "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_vol_tank_total,     "vol_tank_total",       "Total HTF volume in storage",                             "m3",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_hx_eff,             "hx_eff",               "Heat exchanger effectiveness",                            "-",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_mass_tank_hot,      "mass_tank_hot",        "Mass of total fluid in the hot tank",                     "kg",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_mass_tank_cold,     "mass_tank_cold",       "Mass of total fluid in the cold tank",                    "kg",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_mass_tank_total,    "mass_tank_total",      "Total mass of fluid in tanks",                            "kg",           "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_htf_pump_power,     "htf_pump_power",       "Pumping power for storage, power block loops",            "MWe",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_bop_par,            "bop_par",              "Parasitic power as a function of power block load",       "MWe",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_fixed_par,          "fixed_par",            "Fixed parasitic power losses - every hour of operation",  "MWe",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_aux_par,            "aux_par",              "Parasitic power associated with auxiliary heater",        "MWe",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_q_pb,               "q_pb",                 "Thermal energy to the power block",                       "MWt",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_tank_losses,        "tank_losses",          "Thermal losses from tank",                                "MWt",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_q_to_tes,           "q_to_tes",             "Thermal energy into storage",                             "MWt",          "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_mode,               "mode",                 "Operation mode",                                          "-",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_TOU,                "TOU",                  "Time of use period",                                      "-",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_hot_node,         "T_hot_node",           "Thermocline: Hot node temperature",                       "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_cold_node,        "T_cold_node",          "Thermocline: Cold node temperature",                      "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_T_max,              "T_max",                "Thermocline: Maximum temperature",                        "C",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_f_hot,              "f_hot",                "Thermocline: Hot depth fraction",                         "-",            "",        "",        ""},
	{ TCS_OUTPUT,   TCS_NUMBER,        O_f_cold,             "f_cold",               "Thermocline: Cold depth fraction",                        "-",            "",        "",        ""},
																				     
    { TCS_INVALID,  TCS_INVALID,       N_MAX,                0,                      0,                                                         0,                0,        0,        0 }

};

class sam_mw_trough_type251 : public tcstypeinterface
{
private:	
	HTFProperties field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties store_htfProps;		// Instance of HTFProperties class for storage HTF
	Storage_HX hx_storage;				// Instance of Storage_HX class for heat exchanger between storage and field HTFs
	Thermocline_TES thermocline;

	// Parameters
    const int N_sgs_pipe_sections = 11;
	double tshours;
    double eta_pump;
    double HDR_rough;
	bool is_hx;
	double dt_hot;
	double dt_cold;
	int hx_config;
	double q_max_aux;
	double lhv_eff;
	double T_set_aux;
	int store_fl;
	double vol_tank;
	double h_tank;
	double h_tank_min;
	double u_tank;
	int tank_pairs;
	double cold_tank_Thtr;
	double hot_tank_Thtr;
	double cold_tank_max_heat;
	double hot_tank_max_heat;
	int field_fl;
	double T_field_in_des;
	double T_field_out_des;
	double q_pb_design;
	double W_pb_design;
	double cycle_max_frac;
	double cycle_cutoff_frac;
	double solarm;
	double pb_pump_coef;
	double tes_pump_coef;
    double V_tes_des;
    bool custom_tes_p_loss;
    int l_k_tes_loss_coeffs;
    double * k_tes_loss_coeffs_in;
    util::matrix_t<double> k_tes_loss_coeffs;
    bool custom_sgs_pipe_sizes;
    int l_sgs_diams;
    double * sgs_diams;
    int l_sgs_wallthicks;
    double * sgs_wallthicks;
    int l_sgs_lengths;
    double * sgs_lengths_in;
    double DP_SGS;
    double pb_fixed_par;
	int l_bop_array;		
	double * bop_array;
	int l_aux_array;
	double * aux_array;
	double T_startup;
	int fossil_mode;
	bool fthr_ok;
	int nSCA;
	double I_bn_des;
	bool fc_on;
	double t_standby_reset;
	int sf_type;
	int tes_type;
	int numtou;
	double * tslogic_a;
	double * tslogic_b;
	double * tslogic_c;
	double * ffrac;
    bool tanks_in_parallel;
    bool has_hot_tank_bypass;
    double T_tank_hot_inlet_min;
    bool calc_design_pipe_vals;
    double T_field_in_at_des;
    double T_field_out_at_des;
    double P_field_in_at_des;
	//double * TOU_schedule;
	//int nTOU_schedule;

	//Thermocline Parameters
	int tc_fill;
	double tc_void;
	double t_dis_out_min;
	double t_ch_out_max;
	int nodes;
	double f_tc_cold;

	//Calculated variables
	double ccoef;
	double q_sby_frac;
	double q_sby;
    double m_dot_pb_design;
	double m_dot_pb_max;
	double ms_charge_max;
	double ms_disch_max;
	double V_tank_active;
	double t_standby;
	int tempmode;
    util::matrix_t<double> SGS_v_dot_rel;
    util::matrix_t<double> SGS_diams;
    util::matrix_t<double> SGS_wall_thk;
    util::matrix_t<double> SGS_lengths;
    util::matrix_t<double> SGS_m_dot_des;
    util::matrix_t<double> SGS_vel_des;
    util::matrix_t<double> SGS_T_des;
    util::matrix_t<double> SGS_P_des;

	//"Storage" Variables
	double V_tank_hot_prev;
	double T_tank_hot_prev;
	double V_tank_cold_prev;
	double T_tank_cold_prev;
	int mode_prev_ncall;
	double m_tank_hot_prev;
	double m_tank_cold_prev;
	int pb_on_prev;
    double defocus_rel_prev_ncall;
    double defocus_abs;
	double defocus_prev_ncall;          // absolute defocus previously output for trough model
    double defocus_abs_prev;            // absolute defocus from previous timestep
    bool recirc_prev_ncall;
	double t_standby_prev;

	//"Real-time" variables representing "Storage" variables
	double vol_tank_hot_fin;
	double T_tank_hot_fin;	
	double vol_tank_cold_fin;
	double T_tank_cold_fin;
	double m_tank_hot_fin;
	double m_tank_cold_fin;
	int pb_on;
	double T_pb_in;

	bool hx_err_flag;

	int pb_tech_type;
	bool initialize_sco2;
	// double m_deltaT_mc_in;
	// double m_deltaT_t_in;
	// double q_sco2_max_input;
    double err_prev_call, derr_prev_call;

public:
	
	// enumerations for modes
	enum{
		pb_off_or_standby = 1,
		pb_partial_load,
		excess_energy,
		charging_storage,};

	sam_mw_trough_type251( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		tshours		= std::numeric_limits<double>::quiet_NaN();
        eta_pump	= std::numeric_limits<double>::quiet_NaN();
        HDR_rough	= std::numeric_limits<double>::quiet_NaN();
		is_hx		= false;
		dt_hot		= std::numeric_limits<double>::quiet_NaN();
		dt_cold		= std::numeric_limits<double>::quiet_NaN();
		hx_config	= -1;
		q_max_aux	= std::numeric_limits<double>::quiet_NaN();
		lhv_eff		= std::numeric_limits<double>::quiet_NaN();
		T_set_aux	= std::numeric_limits<double>::quiet_NaN();
		store_fl	= -1;
		vol_tank	= std::numeric_limits<double>::quiet_NaN();
		h_tank		= std::numeric_limits<double>::quiet_NaN();
		h_tank_min	= std::numeric_limits<double>::quiet_NaN();
		u_tank		= std::numeric_limits<double>::quiet_NaN();
		tank_pairs	= -1;
		cold_tank_Thtr	= std::numeric_limits<double>::quiet_NaN();
		hot_tank_Thtr	= std::numeric_limits<double>::quiet_NaN();
		cold_tank_max_heat	= std::numeric_limits<double>::quiet_NaN();
		hot_tank_max_heat = std::numeric_limits<double>::quiet_NaN();
		field_fl		= -1;
		T_field_in_des	= std::numeric_limits<double>::quiet_NaN();
		T_field_out_des = std::numeric_limits<double>::quiet_NaN();
		q_pb_design		= std::numeric_limits<double>::quiet_NaN();
		W_pb_design		= std::numeric_limits<double>::quiet_NaN();
		cycle_max_frac	= std::numeric_limits<double>::quiet_NaN();
		cycle_cutoff_frac = std::numeric_limits<double>::quiet_NaN();
		solarm			= std::numeric_limits<double>::quiet_NaN();
		pb_pump_coef	= std::numeric_limits<double>::quiet_NaN();
		tes_pump_coef	= std::numeric_limits<double>::quiet_NaN();
        V_tes_des       = std::numeric_limits<double>::quiet_NaN();
        custom_tes_p_loss       = false;
        l_k_tes_loss_coeffs     = -1;
        k_tes_loss_coeffs_in    = 0;
        DP_SGS          = std::numeric_limits<double>::quiet_NaN();
		pb_fixed_par	= std::numeric_limits<double>::quiet_NaN();
		l_bop_array		= -1;
		bop_array	= 0;
		l_aux_array = -1;
		aux_array	= 0;
		T_startup	= std::numeric_limits<double>::quiet_NaN();
		fossil_mode = -1;
		fthr_ok		= false;
		nSCA		= -1;
		I_bn_des	= std::numeric_limits<double>::quiet_NaN();
		fc_on		= false;		
		t_standby_reset = std::numeric_limits<double>::quiet_NaN();
		sf_type		= -1;
		tes_type	= -1;
		numtou		= -1;
		tslogic_a	= 0;
		tslogic_b	= 0;
		tslogic_c	= 0;
		ffrac		= 0;
        tanks_in_parallel = false;
        has_hot_tank_bypass = false;
        T_tank_hot_inlet_min = std::numeric_limits<double>::quiet_NaN();
        calc_design_pipe_vals = false;
        T_field_in_at_des = std::numeric_limits<double>::quiet_NaN();
        T_field_out_at_des = std::numeric_limits<double>::quiet_NaN();
        P_field_in_at_des = std::numeric_limits<double>::quiet_NaN();
		//TOU_schedule = NULL;

		//Thermocline Parameters
		tc_fill			= -1;
		tc_void			= std::numeric_limits<double>::quiet_NaN();
		t_dis_out_min	= std::numeric_limits<double>::quiet_NaN();
		t_ch_out_max	= std::numeric_limits<double>::quiet_NaN();
		nodes			= -1;
		f_tc_cold		= std::numeric_limits<double>::quiet_NaN();

		//Calculated variables
		ccoef	= std::numeric_limits<double>::quiet_NaN();
		q_sby	= std::numeric_limits<double>::quiet_NaN();
        m_dot_pb_design = std::numeric_limits<double>::quiet_NaN();
		m_dot_pb_max	= std::numeric_limits<double>::quiet_NaN();
		ms_charge_max	= std::numeric_limits<double>::quiet_NaN();
		ms_disch_max	= std::numeric_limits<double>::quiet_NaN();
		V_tank_active	= std::numeric_limits<double>::quiet_NaN();
		t_standby		= std::numeric_limits<double>::quiet_NaN();

		//"Storage" Variables
		V_tank_hot_prev	= std::numeric_limits<double>::quiet_NaN();
		T_tank_hot_prev	= std::numeric_limits<double>::quiet_NaN();
		V_tank_cold_prev= std::numeric_limits<double>::quiet_NaN();
		T_tank_cold_prev= std::numeric_limits<double>::quiet_NaN();
		mode_prev_ncall		= -1;
		m_tank_hot_prev	= std::numeric_limits<double>::quiet_NaN();
		m_tank_cold_prev= std::numeric_limits<double>::quiet_NaN();
		pb_on_prev		= -1;
        defocus_rel_prev_ncall = std::numeric_limits<double>::quiet_NaN();
        defocus_abs = std::numeric_limits<double>::quiet_NaN();
		defocus_prev_ncall	= std::numeric_limits<double>::quiet_NaN();
        defocus_abs_prev = std::numeric_limits<double>::quiet_NaN();
        recirc_prev_ncall = false;
		t_standby_prev	= std::numeric_limits<double>::quiet_NaN();

		//"Real-time" variables representing "Storage" variables
		pb_on = -1; 
		vol_tank_hot_fin	= std::numeric_limits<double>::quiet_NaN();
		T_tank_hot_fin		= std::numeric_limits<double>::quiet_NaN();
		vol_tank_cold_fin	= std::numeric_limits<double>::quiet_NaN();
		T_tank_cold_fin	= std::numeric_limits<double>::quiet_NaN();
		m_tank_hot_fin	= std::numeric_limits<double>::quiet_NaN();
		m_tank_cold_fin = std::numeric_limits<double>::quiet_NaN();
		T_pb_in = std::numeric_limits<double>::quiet_NaN();

		q_sby_frac = std::numeric_limits<double>::quiet_NaN();

		// m_deltaT_mc_in = std::numeric_limits<double>::quiet_NaN();
		// m_deltaT_t_in = std::numeric_limits<double>::quiet_NaN();
		// q_sco2_max_input = std::numeric_limits<double>::quiet_NaN();
        err_prev_call = std::numeric_limits<double>::quiet_NaN();
        derr_prev_call = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_mw_trough_type251()
	{
	}

	virtual int init()
	{
		//double dt = time_step();
						
		// Declare instance of fluid class for FIELD fluid.
		// Set fluid number and copy over fluid matrix if it makes sense.
		field_fl	= (int) value(P_field_fl);
		if( field_fl != HTFProperties::User_defined )
		{
			if( !field_htfProps.SetFluid( field_fl ) ) // field_fl should match up with the constants
			{
				message(TCS_ERROR, "Field HTF code is not recognized");
				return -1;
			}
		}
		else
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value( P_field_fl_props, &nrows, &ncols );
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r,c) = TCS_MATRIX_INDEX( var( P_field_fl_props ), r, c );

				if ( !field_htfProps.SetUserDefinedFluid( mat ) )
				{
					//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
					message(TCS_ERROR,  field_htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}

		store_fl = (int)value(P_store_fl);

		// Declare instance of fluid class for STORAGE fluid.
		// Set fluid number and copy over fluid matrix if it makes sense.
		if( store_fl != HTFProperties::User_defined )
		{
			if( !store_htfProps.SetFluid(store_fl) ) // store_fl should match up with the constants
			{
				message(TCS_ERROR, "Field HTF code is not recognized");
				return -1;
			}
		}
		else
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value(P_store_fl_props, &nrows, &ncols);
			if( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat(nrows, ncols, 0.0);
				for( int r = 0; r < nrows; r++ )
				for( int c = 0; c < ncols; c++ )
					mat.at(r, c) = TCS_MATRIX_INDEX(var(P_store_fl_props), r, c);

				if( !store_htfProps.SetUserDefinedFluid(mat) )
				{
					message(TCS_ERROR, "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols);
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}

		bool is_hx_calc = true;

		if( store_fl != field_fl )
			is_hx_calc = true;
		else if( field_fl != HTFProperties::User_defined )
			is_hx_calc = false;
		else
		{
			is_hx_calc = !field_htfProps.equals(&store_htfProps);
		}

		is_hx		= (value(P_is_hx) != 0);			//[-]

		if(is_hx != is_hx_calc)
		{
			if( is_hx_calc )
				message(TCS_NOTICE, "Input field and storage fluids are different, but the inputs did not specify a field-to-storage heat exchanger. The system was modeled assuming a heat exchanger.");				
			else
				message(TCS_NOTICE, "Input field and storage fluids are identical, but the inputs specified a field-to-storage heat exchanger. The system was modeled assuming no heat exchanger.");

			is_hx = is_hx_calc;
		}

		tshours		= value(P_tshours);					//[hr]
        eta_pump    = value(eta_pump);                  //[-]
        HDR_rough   = value(P_hdr_rough);               //[m]
		dt_hot		= value(P_dt_hot);					//[K]
		dt_cold		= value(P_dt_cold);					//[K]
		hx_config	= (int) value(P_hx_config);			//[-]
		q_max_aux	= value(P_q_max_aux)*1.E6;			//[W] convert from [MW]	
		lhv_eff     = value(P_lhv_eff);
		T_set_aux	= value(P_T_set_aux)+273.15;		//[K] convert from [C]				
		vol_tank		= value(P_vol_tank);			//[m3] volume of *one temperature*, i.e. vol_tank = total cold storage = total hot storage
		h_tank		= value(P_h_tank);					//[m]
		h_tank_min	= value(P_h_tank_min);				//[m]
		u_tank		= value(P_u_tank);					//[W/m2-K]
		tank_pairs	= (int) value(P_tank_pairs);		//[-]
		cold_tank_Thtr	= value(P_cold_tank_Thtr)+273.15;	//[K] convert from [C]		
		hot_tank_Thtr	= value(P_hot_tank_Thtr)+273.15;	//[K] convert from [C]	
		cold_tank_max_heat	= value(P_cold_tank_max_heat);		//[MW]
		hot_tank_max_heat = value(P_hot_tank_max_heat);			//[MW]
		T_field_in_des	= value(P_T_field_in_des) + 273.15;		//[K] convert from [C]
		T_field_out_des	= value(P_T_field_out_des) + 273.15;	//[K] convert from [C]
		q_pb_design		= value(P_q_pb_design)*1.E6;		//[W] convert from [MW]
		W_pb_design		= value(P_W_pb_design)*1.E6;		//[W] convert from [MW]
		cycle_max_frac	= value(P_cycle_max_frac);			//[-]
		cycle_cutoff_frac	= value(P_cycle_cutoff_frac);	//[-]
		solarm			= value(P_solarm);					//[-]
		pb_pump_coef	= value(P_pb_pump_coef);			//[kW/kg]
		tes_pump_coef	= value(P_tes_pump_coef);			//[kW/kg]
        V_tes_des       = value(P_V_tes_des);               //[m/s]
        custom_tes_p_loss       = (bool) value(P_custom_tes_p_loss);                  //[-]
        k_tes_loss_coeffs_in    = value(P_k_tes_loss_coeffs, &l_k_tes_loss_coeffs);   //[-]
        k_tes_loss_coeffs.assign(k_tes_loss_coeffs_in, l_k_tes_loss_coeffs);

        custom_sgs_pipe_sizes = (bool) value(P_custom_sgs_pipe_sizes);
        sgs_diams = value(P_sgs_diams, &l_sgs_diams);                   //[m]
        sgs_wallthicks = value(P_sgs_wallthicks, &l_sgs_wallthicks);    //[m]
        sgs_lengths_in = value(P_sgs_lengths, &l_sgs_lengths);             //[m]
        SGS_lengths.assign(sgs_lengths_in, l_sgs_lengths);
        DP_SGS = value(P_dp_sgs) * 1.e5;                    // bar to Pa;

		pb_fixed_par	= value(P_pb_fixed_par);			//[-]
	
		bop_array	= value(P_bop_array, &l_bop_array);		
		
		aux_array	= value(P_aux_array, &l_aux_array);
		
		T_startup	= value(P_T_startup)+273.15;		//[K] convert from [C]
		fossil_mode	= (int) value(P_fossil_mode);		//[-]
		fthr_ok		= (value(P_fthr_ok) != 0);			//[-]
		nSCA		= (int) value(P_nSCA);				//[-]
		I_bn_des	= value(P_I_bn_des);				//[W/m2]
		fc_on		= (value(P_fc_on) != 0);			//[-]
		
		q_sby_frac	= value(P_q_sby_frac);				//[-]
		
		t_standby_reset	= value(P_t_standby_init)*3600;	//[s] convert from [hr]
		sf_type		= (int) value(P_sf_type);			//[-] 1 = trough, 2 = tower
		tes_type	= (int) value(P_tes_type);			//[-]
		
		int l_tslogic_a, l_tslogic_b, l_tslogic_c, l_ffrac;
		tslogic_a	= value(P_tslogic_a, &l_tslogic_a);	//[-]
		tslogic_b	= value(P_tslogic_b, &l_tslogic_b);	//[-]
		tslogic_c	= value(P_tslogic_c, &l_tslogic_c);	//[-]
		ffrac		= value(P_ffrac, &l_ffrac);
		if( l_tslogic_a != l_tslogic_b || l_tslogic_a != l_tslogic_c || l_tslogic_a != l_ffrac )
			{
				message( TCS_ERROR, "Time-of-use schedules do not contain the same number of periods" );
				return -1;
			}
		numtou = l_tslogic_a;
        tanks_in_parallel = (bool) value(P_tanks_in_parallel);
        has_hot_tank_bypass = (bool) value(P_hot_tank_bypass);
        T_tank_hot_inlet_min = value(P_T_tank_hot_in_min) + 273.15;
        calc_design_pipe_vals = (bool) value(P_des_pipe_vals);

		//TOU_schedule = value(P_TOU_schedule, &nTOU_schedule);
        err_prev_call = -1.;
        derr_prev_call = -1.;

		//Thermocline Parameters
		tc_fill		= (int) value(P_tc_fill);			//[-]
		tc_void		= value(P_tc_void);					//[-]
		t_dis_out_min	= value(P_t_dis_out_min);		//[C]
		t_ch_out_max	= value(P_t_ch_out_max);		//[C]
		nodes		= (int) value(P_nodes);				//[-]
		f_tc_cold	= value(P_f_tc_cold);				//[-]
		//*******************************************		
		
		/*
		//Calculate standby power
		q_sby = q_sby_frac * q_pb_design;

		//Calculate heat exchanger size based on parameters
		//hx_ua will be used as the heat exchanger size for simulation calculations
		double duty = (solarm - 1.)*q_pb_design;	//[W]

		if(tshours>0.0)
		{
			//if( !hx_storage.hx_size(field_htfProps, store_htfProps, hx_config, duty, dt_hot, dt_cold, T_field_out_des, T_field_in_des) )
			//if( !hx_storage.hx_size(field_htfProps, store_htfProps, hx_config, duty, vol_tank, h_tank, u_tank, tank_pairs, hot_tank_Thtr,
            //    cold_tank_Thtr, tank_max_heat, dt_hot, dt_cold, T_field_out_des, T_field_in_des)  )
			if( !hx_storage.define_storage(field_htfProps, store_htfProps, !is_hx, hx_config, duty, vol_tank, h_tank, u_tank, tank_pairs, hot_tank_Thtr,
                cold_tank_Thtr, tank_max_heat, dt_hot, dt_cold, T_field_out_des, T_field_in_des)  )
			{
				message( "Heat exchanger sizing failed" );
				return -1;
			}
		}

		// Calculate maximum value for power block mass flow
		double c_pb_ref = field_htfProps.Cp( (T_field_in_des + T_field_out_des)/2.0 )*1000.;		//[J/kg-K] Reference power block specific heat 
		m_dot_pb_max = cycle_max_frac * q_pb_design/(c_pb_ref*(T_field_out_des - T_field_in_des));	//[kg/s] Maximum power block mass flow rate
    
		T_pb_in = T_field_out_des;

		// Calculate the maximum charge/discharge rates for storage  MJW 7.13.2010
		// Charge max is either the power block max flow, or the solar multiple-1 times the design PB mass flow rate
		if(is_hx)
		{ms_charge_max = max(m_dot_pb_max, (solarm - 1.)*q_pb_design/(c_pb_ref*(T_field_out_des - T_field_in_des)));}	//[kg/s]		
		else	// MJW 10.18.2010: If we have direct storage, don't limit the maximum storage charge/discharge rate
		{ms_charge_max = 999.*m_dot_pb_max;}
   		// limit discharge to what the power block can handle
		ms_disch_max = m_dot_pb_max;
		// *****************************************************

		//Set initial storage values
		V_tank_hot_prev	= value(P_V_tank_hot_ini);			//[m3]
		T_tank_hot_prev = value(P_T_tank_hot_ini)+273.15;	//[K] convert from [C]
		V_tank_cold_prev= vol_tank - V_tank_hot_prev;		//[m3] Initial cold tank fluid volume
		T_tank_cold_prev= value(P_T_tank_cold_ini)+273.15;	//[K] convert from [C]
		mode_prev_ncall		= 1;								//[-]
		m_tank_hot_prev	= V_tank_hot_prev*store_htfProps.dens(T_tank_hot_prev,1.0);		//[kg]
		m_tank_cold_prev= V_tank_cold_prev*store_htfProps.dens(T_tank_cold_prev,1.0);	//[kg]
		pb_on_prev		= 0;								//[-] power block initially off
		defocus_prev_ncall	= 1.;								//[-] initial defocus
		t_standby_prev	= t_standby_reset;					//[s] 
		// *********************************************************

		V_tank_active	= vol_tank*(1.-2.*h_tank_min/h_tank);	//[m3] Active tank volume.. that is, volume above the minimum fluid level and below the maximum level

		if(tes_type == 2)
		{
			if( !thermocline.Initialize_TC( h_tank, vol_tank/h_tank, tc_fill, u_tank*3.6, u_tank*3.6, u_tank*3.6, tc_void,
				                        1.0, t_dis_out_min, t_ch_out_max, nodes, T_tank_hot_prev - 273.15, T_tank_cold_prev - 273.15,
										f_tc_cold, cold_tank_Thtr - 273.15, tank_max_heat, tank_pairs, store_htfProps ) )
			{
				message( "Thermocline initialization failed" );
				return -1;
			}

		}

		// Set the convergence coefficient
		if(tes_type == 1.)
		{
			ccoef = .9 - .5*(3600.- dt)/3600.;	// 3.5.11
		}
		else
		{
			ccoef = .5;		// 3.5.11	
		}
		
		ccoef = 0.5;

		//Set the heat exchanger error flag
		hx_err_flag = false;
		*/

		pb_tech_type = (int)value(P_PB_TECH_TYPE);

		if( pb_tech_type != 424 )
		{
			int finalize_error_code = finalize_initial_calcs();

			if( finalize_error_code != 0 )
				return -1;

			initialize_sco2 = false;
		}
		else
		{
			initialize_sco2 = true;		// boolean telling code to initialize heat exchangers, etc. during the first timestep call
		}
		

		return 0;
	}

	int finalize_initial_calcs()
	{
		//Calculate standby power
		q_sby = q_sby_frac * q_pb_design;

		//Calculate heat exchanger size based on parameters
		//hx_ua will be used as the heat exchanger size for simulation calculations
		//double duty = max(1.,(solarm - 1.))*q_pb_design;	//[W]
        double duty = q_pb_design * solarm;     //mjw 10/13/14   Allow all energy from the field to go into storage at any time.

		if( tshours>0.0 )
		{
			//if( !hx_storage.hx_size(field_htfProps, store_htfProps, hx_config, duty, dt_hot, dt_cold, T_field_out_des, T_field_in_des) )
			//if( !hx_storage.hx_size(field_htfProps, store_htfProps, hx_config, duty, vol_tank, h_tank, u_tank, tank_pairs, hot_tank_Thtr,
            //    cold_tank_Thtr, tank_max_heat, dt_hot, dt_cold, T_field_out_des, T_field_in_des)  )
			if( !hx_storage.define_storage(field_htfProps, store_htfProps, !is_hx, hx_config, duty, vol_tank, h_tank, u_tank, tank_pairs,
                hot_tank_Thtr, cold_tank_Thtr, cold_tank_max_heat, hot_tank_max_heat, dt_hot, dt_cold, T_field_out_des, T_field_in_des) )
			{
				message(TCS_ERROR, "Heat exchanger sizing failed");
				return -1;
			}
		}

		// Calculate maximum value for power block mass flow
		double c_pb_ref = field_htfProps.Cp((T_field_in_des + T_field_out_des) / 2.0)*1000.;		//[J/kg-K] Reference power block specific heat
        m_dot_pb_design = q_pb_design / (c_pb_ref*(T_field_out_des - T_field_in_des));              //[kg/s] Maximum power block mass flow rate
		m_dot_pb_max = cycle_max_frac * m_dot_pb_design;                                        	//[kg/s] Maximum power block mass flow rate
        
		// Calculate the maximum charge/discharge rates for storage  MJW 7.13.2010
		// Charge max is either the power block max flow, or the solar multiple-1 times the design PB mass flow rate
		if( is_hx )
		{
			ms_charge_max = max(m_dot_pb_max, duty / (c_pb_ref*(T_field_out_des - T_field_in_des)));
		}	//[kg/s]		
		else	// MJW 10.18.2010: If we have direct storage, don't limit the maximum storage charge/discharge rate
		{
			ms_charge_max = 999.*m_dot_pb_max;
		}
		// limit discharge to what the power block can handle
		ms_disch_max = m_dot_pb_max;
		// *****************************************************

		//Set initial storage values
		V_tank_hot_prev = value(P_V_tank_hot_ini);              //[m3]
		T_tank_hot_prev = value(P_T_tank_hot_ini) + 273.15;	    //[K] convert from [C]
		V_tank_cold_prev = vol_tank - V_tank_hot_prev;		    //[m3] Initial cold tank fluid volume
		T_tank_cold_prev = value(P_T_tank_cold_ini) + 273.15;	//[K] convert from [C]
		mode_prev_ncall = 1;								    //[-]
		m_tank_hot_prev = V_tank_hot_prev*store_htfProps.dens(T_tank_hot_prev, 1.0);		//[kg]
		m_tank_cold_prev = V_tank_cold_prev*store_htfProps.dens(T_tank_cold_prev, 1.0);     //[kg]
		pb_on_prev = 0;								            //[-] power block initially off
        defocus_rel_prev_ncall = 1.;                            //[-] initial relative defocus
        defocus_abs = 1.;
		defocus_prev_ncall = 1.;								//[-] initial absolute defocus
        defocus_abs_prev = 1.;                                  //[-] defocus absolute
        recirc_prev_ncall = false;                              //[-] recirculating bypass valve initally closed (no recirc) 
		t_standby_prev = t_standby_reset;					    //[s] 
		//*********************************************************

		V_tank_active = vol_tank*(1. - 2.*h_tank_min / h_tank);	//[m3] Active tank volume.. that is, volume above the minimum fluid level and below the maximum level
        
        tanks_in_parallel ? T_pb_in = T_field_out_des : T_pb_in = T_tank_hot_prev;

        // Size SGS piping and output values
        if (custom_sgs_pipe_sizes) {
            if (l_sgs_diams == N_sgs_pipe_sections && l_sgs_wallthicks == N_sgs_pipe_sections) {
                SGS_diams.assign(sgs_diams, l_sgs_diams);
                SGS_wall_thk.assign(sgs_wallthicks, l_sgs_wallthicks);
            }
            else {
                message(TCS_ERROR, "The number of custom SGS pipe sections is not correct.");
                return -1;
            }
        }
        double rho_avg = field_htfProps.dens((T_field_in_des + T_field_out_des) / 2, 9 / 1.e-5);
        double SGS_vol_tot;
        if ( size_sgs_piping(V_tes_des, SGS_lengths, rho_avg, m_dot_pb_design, solarm, tanks_in_parallel,     // Inputs
            SGS_vol_tot, SGS_v_dot_rel, SGS_diams, SGS_wall_thk, SGS_m_dot_des, SGS_vel_des, custom_sgs_pipe_sizes) ) {        // Outputs
            message(TCS_ERROR, "SGS piping sizing failed.");
            return -1;
        }
        value(O_V_sgs, SGS_vol_tot);
        double *sgs_diams = allocate(O_D_sgs, (int)SGS_diams.ncells());
        std::copy(SGS_diams.data(), SGS_diams.data() + SGS_diams.ncells(), sgs_diams);
        double *sgs_wall_thk = allocate(O_wall_thk_sgs, (int)SGS_wall_thk.ncells());
        std::copy(SGS_wall_thk.data(), SGS_wall_thk.data() + SGS_wall_thk.ncells(), sgs_wall_thk);
        double *sgs_m_dot_des = allocate(O_m_dot_des_sgs, (int)SGS_m_dot_des.ncells());
        std::copy(SGS_m_dot_des.data(), SGS_m_dot_des.data() + SGS_m_dot_des.ncells(), sgs_m_dot_des);
        double *sgs_vel_des = allocate(O_vel_des_sgs, (int)SGS_vel_des.ncells());
        std::copy(SGS_vel_des.data(), SGS_vel_des.data() + SGS_vel_des.ncells(), sgs_vel_des);


		if( tes_type == 2 )
		{
			if( !thermocline.Initialize_TC(h_tank, vol_tank / h_tank, tc_fill, u_tank*3.6, u_tank*3.6, u_tank*3.6, tc_void,
				1.0, t_dis_out_min, t_ch_out_max, nodes, T_tank_hot_prev - 273.15, T_tank_cold_prev - 273.15,
				f_tc_cold, cold_tank_Thtr - 273.15, cold_tank_max_heat, tank_pairs, store_htfProps) )
			{
				message(TCS_ERROR, "Thermocline initialization failed");
				return -1;
			}

		}

		double dt = time_step();

		// Set the convergence coefficient
		if( tes_type == 1. )
		{
			ccoef = .9 - .5*(3600. - dt) / 3600.;	// 3.5.11
		}
		else
		{
			ccoef = .5;		// 3.5.11	
		}

		ccoef = 0.5;

		//Set the heat exchanger error flag
		hx_err_flag = false;

		return 0;	
	}

	virtual int call( double time, double step, int ncall )
	{
		//double dt = time_step();
		
		if( initialize_sco2 )
		{
			if(ncall == 0)		// Inputs aren't updated yet..
			{
				return 0;
			}

			if( pb_tech_type == 424 )
			{
				if( time / step == 1.0 )
				{
					// Get actual receiver inlet temperature at design
					T_field_in_des = value(I_T_HTF_COLD_DES) + 273.15;		//[K] convert from C

					finalize_initial_calcs();
				}
			}
			initialize_sco2 = false;
		}

        if (calc_design_pipe_vals) {
            T_field_in_at_des = value(I_T_field_in_at_des) + 273.15;
            T_field_out_at_des = value(I_T_field_out_at_des) + 273.15;
            P_field_in_at_des = value(I_P_field_in_at_des) * 1.e5;        // bar to Pa

            size_sgs_piping_TandP(T_field_in_at_des, T_field_out_at_des, P_field_in_at_des, DP_SGS,
                SGS_lengths, k_tes_loss_coeffs, HDR_rough, tanks_in_parallel, SGS_diams, SGS_vel_des,
                SGS_T_des, SGS_P_des);                          // Outputs
            
            // Adjust first two pressures after field pumps, because the field inlet pressure used above was
            // not yet corrected for the section in the TES/PB before the hot tank
            double DP_before_hot_tank = SGS_P_des.at(3);        // first section before hot tank
            SGS_P_des.at(1) += DP_before_hot_tank;
            SGS_P_des.at(2) += DP_before_hot_tank;

            double *sgs_t_des = allocate(O_t_des_sgs, (int)SGS_T_des.ncells());
            std::copy(SGS_T_des.data(), SGS_T_des.data() + SGS_T_des.ncells(), sgs_t_des);
            double *sgs_p_des = allocate(O_p_des_sgs, (int)SGS_P_des.ncells());
            std::copy(SGS_P_des.data(), SGS_P_des.data() + SGS_P_des.ncells(), sgs_p_des);

            value(O_p_des_sgs_1, DP_before_hot_tank);           // for adjusting field design pressures

            calc_design_pipe_vals = false;
        }

		double I_bn			= value(I_I_bn);				    // [W/m2]
		double m_dot_field	= value(I_m_dot_field)/3600.;	    // [kg/s] convert from [kg/hr]
		//double m_dot_htf_ref= value(I_m_dot_htf_ref)/3600.;	// [kg/s] convert from [kg/hr]
		double T_field_out	= value(I_T_field_out)+273.15;	    // [K] convert from [C]
		double T_pb_out		= value(I_T_pb_out)+273.15;		    // [K] convert from [C]
		double T_amb		= value(I_T_amb)+273.15;		    // [K] convert from [C]
		//double m_pb_demand	= value(I_m_pb_demand)/3600.;	// [kg/s] convert from [kg/hr]
		//double q_startup	= value(I_q_startup);			    // [MWt-hr]
		int touperiod       = (int)value(I_TOUPeriod) - 1;      // control value between 1 & 9, have to change to 0-8 for array index
		double dnifc;		
		if( fc_on ) 
			dnifc = value(I_dnifc);				// [W/m2]
		else
			dnifc = 0.0;						// [W/m2]
        double defocus_rel_prev_ncall = value(I_defocus);                 // [-] previous relative defocus passed back from type250 to ensure TCS convergence

		// Calculate maximum thermal input for sCO2 cycle
		// if( pb_tech_type == 424 && ncall == 0 )
		// {
		// 	ms_opt_tar_od_par.m_T_mc_in = T_amb + m_deltaT_mc_in;
		// 	ms_opt_tar_od_par.m_T_t_in = T_field_out - m_deltaT_t_in;
		// 
		// 	int max_q_error_code = 0;
		// 	ms_rc_cycle.get_max_output_od(ms_opt_tar_od_par, max_q_error_code);
		// 
		// 	double m_q_max_sf = 0.97;
		// 	if( max_q_error_code != 0 )
		// 	{
		// 		q_sco2_max_input = q_pb_design;
		// 		message("Could not calculate max thermal input for sCO2 cycle: value set to design thermal input");
		// 	}
		// 	else
		// 		q_sco2_max_input = m_q_max_sf*ms_rc_cycle.get_max_target()*1000.0;		// Convert to W
		// }

		// Determine which storage dispatch strategy to use, depending on if any solar resource is available
		double * tselect;
		if(I_bn > 1.E-6) {tselect = tslogic_b;}
		else {tselect = tslogic_a;}
		// *************************************************************************************************
		//int touperiod = CSP::TOU_Reader(TOU_schedule, time, nTOU_schedule);

		if(touperiod < 0){
			message(TCS_ERROR, "The time-of-use (TOU) schedule reader returned with an invalid value of %d.", touperiod);
			return -1;
		}
		double f_storage = tselect[touperiod];				//*** Need to be sure TOU schedule is provided with starting index of 0

		if( ncall == 0 )
            // TODO - test carrying over T_pb_in instead of resetting it to this design condition
            tanks_in_parallel ? T_pb_in = T_field_out_des : T_pb_in = T_tank_hot_prev;

		// Need to fill in iteration controls once inner loops are finished
		
		// Calculate available mass flow and volume from hot and cold tanks
		double rho_tank_hot_avg, T_tank_hot_avg_guess, V_tank_hot_avg, V_tank_hot_avail, m_tank_disch_avail;
		double rho_tank_cold_avg, T_tank_cold_avg_guess, V_tank_cold_avg, V_tank_cold_avail, m_tank_charge_avail;
		T_tank_hot_avg_guess	= T_tank_hot_prev;		//[K] Guess average (w/r/t timestep) hot tank temp - will update after controller calculations
		T_tank_cold_avg_guess	= T_tank_cold_prev;		//[K] Guess average (w/r/t timestep) cold tank temp - will update after controller calculations
		double ms_disch_avail;	                        //[kg/s] current max mass flow of field/pb htf through storage HX during storage discharging
		double ms_charge_avail;                         //[kg/s] current max mass flow of field/pb htf through storage HX during storage charging
		double T_tank_hot_out, eff_disch, q_disch, T_tank_cold_in, Ts_hot;
		double T_tank_cold_out, eff_charge, q_charge, T_tank_hot_in, Ts_cold;
		double c_htf_aux, m_dot_aux_avail;
		double m_avail_tot, q_pb_demand;
		double T_field_in_guess;
		double T_field_in;
		
		//*********************************************************
		//double T_pb_in = T_field_out;	// This is different than TRNSYS because of a mistake in TRNSYS
		//T_pb_in = 664.63;				// So for testing, set to the value TRNSYS uses on the first iteration when time=39
		//*********************************************************

        double c_htf_disch, c_htf_charge;       // spec. heat in J/kg-K of field/pb htf in storage HX when storage discharging and charging, respectively
        double c_htf_field, c_htf_pb;
		double q_pb_demand_guess;
        double qs_disch_avail;                  // current max heat gain in W to field/pb htf during storage discharging
        double qs_charge_avail;                 // current max heat loss in W from field/pb htf during storage charging
		double q_aux_avail,	q_field_avail;
		double q_int, T_int, m_int;
		bool called_TC;
		double ms_disch=0., ms_charge=0.;       // mass flow in kg/s of field/pb htf through storage HX during storage discharging and charging, respectively
		double q_aux, m_dot_aux, q_demand_aux;
		int mode;
		double defocus = defocus_rel_prev_ncall;
        bool recirculating = recirc_prev_ncall;
		int cycle_pl_control, standby_control;
        double m_dot_field_avail;
        double m_dot_pb;
		//int tempmode=0;
		double err, err_prev_iter, derr;
		double defocus_prev_iter, tol;
		int iter, iter_tank;
		bool iterate_mass_temp, iterate_tank_temp;
		double m_tank_disch, m_tank_charge;
        double m_tank_cold_in, m_tank_cold_out, m_tank_hot_in, m_tank_hot_out;
		double T_tank_hot_avg, vol_tank_hot_avg, q_loss_tank_hot, q_htr_tank_hot;
		double T_tank_cold_avg, vol_tank_cold_avg, q_loss_tank_cold, q_htr_tank_cold;
        bool hot_tank_bypassed;

		t_standby = t_standby_prev;	//[s] Initialize t_standby

        bool has_TES = tshours > 1.e-4;

		iter_tank = 1;
		do		// Iteration on average storage tank temperatures
		{
			iter_tank++;
            double m_dot_pb_adj, m_dot_field_adj;

			if(has_TES)
			{
				if(tes_type==1)
				{
					// Hot tank
					rho_tank_hot_avg	= store_htfProps.dens( T_tank_hot_avg_guess, 1.0 );			//[kg/m3] density at average (w/r/t timestep) hot tank temperature
					V_tank_hot_avg		= m_tank_hot_prev/rho_tank_hot_avg;							//[m3] The total fluid volume in the tank based on density at average (w/r/t timestep) hot tank temperature
					V_tank_hot_avail	= max(V_tank_hot_avg - vol_tank*h_tank_min/h_tank, 0.0);	//[m3] Available fluid volume in the tank based on density at average (w/r/t timestep) hot tank temperature
					m_tank_disch_avail	= max(V_tank_hot_avail - V_tank_active*f_storage, 0.0)*rho_tank_hot_avg/step;	//[kg/s] Available discharge mass flow rate

					// Cold tank
					rho_tank_cold_avg	= store_htfProps.dens( T_tank_cold_avg_guess, 1.0 );		//[kg/m3] density at average (w/r/t timestep) cold tank temperature
					V_tank_cold_avg		= m_tank_cold_prev/rho_tank_cold_avg;						//[m3] The total fluid volume in the tank based on density at average (w/r/t timestep) cold tank temperature
					V_tank_cold_avail	= max(V_tank_cold_avg - vol_tank*h_tank_min/h_tank, 0.0);	//[m3] Available fluid volume in the tank based on density at average (w/r/t timestep) cold tank temperature
					m_tank_charge_avail = max(V_tank_cold_avail, 0.0)*rho_tank_cold_avg/step;		//[kg/s] Available charge mass flow rate
				}
				else if(tes_type==2)
				{				
					double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8;
					dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = -999.9;
					thermocline.Solve_TC( T_field_out - 273.15, 0.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 2, 0.0, 0.0, f_storage, step/3600.0,
						                     m_tank_disch_avail, Ts_hot, m_tank_charge_avail, Ts_cold, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8 );
					
					m_tank_disch_avail /= 3600.0;		//[kg/s] Estimated discharge mass flow rate
					Ts_hot += 273.15;					//[K] Estimated discharge temperature
					m_tank_charge_avail /= 3600.0;		//[kg/s] Estimated charge mass flow rate
					Ts_cold += 273.15;					//[K] Estimated charge temperature
				}
				//********************************************************************
		
				if(is_hx)
				{
					if(ncall == 0) hx_err_flag = false;	//reset the error flag.. Don't warn about heat exchanger errors during iteration
					T_tank_hot_out = T_tank_hot_avg_guess;
					if(m_tank_disch_avail > 0.)
					{
						hx_err_flag = ! hx_storage.hx_performance( true, true, T_tank_hot_out, m_tank_disch_avail, T_pb_out,
															eff_disch, T_tank_cold_in, Ts_hot, q_disch, ms_disch_avail);
															
					}
					else	{ms_disch_avail = 0.0; Ts_hot = T_tank_hot_out;}						

					T_tank_cold_out = T_tank_cold_avg_guess;
					if(m_tank_charge_avail > 0.)
					{
						hx_err_flag = ! hx_storage.hx_performance( false, true, T_field_out, m_tank_charge_avail, T_tank_cold_out, 
								eff_charge, Ts_cold, T_tank_hot_in, q_charge, ms_charge_avail);

					}
					else	{ms_charge_avail = 0.0; Ts_cold = T_tank_cold_out;}
				}
				else	// No heat exchanger
				{
					if(tes_type==2)         // Thermocline
					{
						ms_disch_avail = m_tank_disch_avail;		//[kg/s] Available mass flow rate during discharge cycle
						ms_charge_avail = m_tank_charge_avail;		//[kg/s] Available mass flow rate druing charge cycle     
					}		
					else if(tes_type==1)	// 2-tank
					{
						T_tank_hot_out	= T_tank_hot_avg_guess;		//[K]
						T_tank_cold_out = T_tank_cold_avg_guess;	//[K]
						ms_disch_avail	= m_tank_disch_avail;		//[kg/s]
						ms_charge_avail = m_tank_charge_avail;		//[kg/s]
						Ts_hot			= T_tank_hot_out;			//[K]
						Ts_cold			= T_tank_cold_out;			//[K]
					}
				}
				ms_disch_avail	= min( ms_disch_avail, ms_disch_max );
				ms_charge_avail = min( ms_charge_avail, ms_charge_max );
			}
			else	//No storage
			{
                //mjw 10/10/14
                //Adding initializers for non-TES option. Fortran initializes values to 0.0, c++ does not! This caused errors for the no-TES cases.
				ms_disch_avail	= 0.0;
				ms_charge_avail = 0.0;
                rho_tank_hot_avg = 0.;
                V_tank_hot_avg = 0.;
                V_tank_hot_avail = 0.;
                m_tank_disch_avail = 0.;
                rho_tank_cold_avg = 0.;
                V_tank_cold_avg = 0.;
                V_tank_cold_avail = 0.;
                m_tank_charge_avail = 0.;
                T_tank_hot_out = 0.;
                T_tank_cold_out = 0.;
                Ts_hot = 0.;
                Ts_cold = 0.;
                q_htr_tank_hot = 0.;
                q_htr_tank_cold = 0.;
			}
		
			
			// Maximum power cycle output can vary for sco2 cycle depending on ambient conditions
			// so, for calculations determining the available mass flow rate and power cycle demand thermal input,
			// need a new variable
			double q_pb_max_input = 0.0;
			if( pb_tech_type == 424 )
			{
				q_pb_max_input = q_pb_design;
			}
			else
			{
				q_pb_max_input = q_pb_design;
			}

			//Account for any available auxiliary heater flow
			c_htf_aux = field_htfProps.Cp( (T_set_aux + T_pb_out)/2. )*1000.0;		//[J/kg-K] Average specific heat
            // TODO - Seems like you can get rid of this if-else block because if ffrac==0, m_dot_aux_avail=0. (Can ffrac < 0?)
			if(ffrac[touperiod] > 0.0)	//*** Need to be sure TOU schedule is provided with starting index of 0
			{
				m_dot_aux_avail = min( q_max_aux, ffrac[touperiod]*q_pb_max_input )/(c_htf_aux*max( (T_set_aux - T_pb_out), 1.0 ));	
			}
			else	{m_dot_aux_avail = 0.;}

			// Recirculate field if needed
            defocus_abs = defocus_prev_ncall * defocus;     // defocus_abs = defocus_abs_prev * defocus_rel
            if (defocus_abs >= 1 &&
                ((tanks_in_parallel == true && T_field_out <= T_startup) ||
                (tanks_in_parallel == false && T_field_out <= T_tank_hot_inlet_min)))
            {
                recirculating = true;
                m_dot_field_avail = 0.;
            }
            else
            {
                recirculating = false;
                m_dot_field_avail = m_dot_field;
            }

			// Calculate the maximum potentially available mass flow
			m_avail_tot = ms_disch_avail + m_dot_aux_avail + m_dot_field_avail;       // this is also true for tanks in series with the field

			// Calculate the demanded values from the power block
			//*** Need to be sure TOU schedule is provided with starting index of 0
			q_pb_demand = q_pb_max_input*tslogic_c[touperiod];		// [W] Adjust the power block demand energy for the TOU period*/

			/*Calculate the maximum potentially available heat flows.  Flows are relative to the
			power block outlet temperature, except in the case of the storage charge level and
			the field inlet temperature. This is because the field inlet temp is a function of
			the calculated values of the other temperatures and flow rates, and because mass 
			flow in this case is calculated relative to the field-side cold outlet temperature.

			A small degree of iteration is needed to get the field inlet temperature to converge
			in the case where storage is charging and an intermediate heat exchanger is used.
			For cases with iteration, set the initial guess values*/              
            if (!recirculating) {
                if (tanks_in_parallel) {
                    T_field_in_guess = T_pb_out;        // TODO - maybe during charging factor in cold tank temp?
                }
                else
                    T_field_in_guess = T_tank_cold_avg_guess;
                }
			else {
                if (tanks_in_parallel || (!tanks_in_parallel && !has_hot_tank_bypass)) {
                    T_field_in_guess = T_field_out;
                }
                else {
                    T_field_in_guess = T_tank_cold_avg_guess;       // tanks in series and bypass valve is only for hot tank
                }
            }

			//err = 999.; iter = 0; derr=999.; errx = 9999.; defocusX=1.; tol = 0.001
			//50 continue  !iterative loop header
			//iter=iter+1
			err_prev_iter = -9999.; err = -999.; iter = 0; defocus_prev_iter = 1.0; tol = 0.001;
			do
			{
				iter++;
				T_field_in = T_field_in_guess;

				// Calculate available heat flows
                if(has_TES){
                    if (is_hx) {
				        c_htf_disch		= field_htfProps.Cp( (Ts_hot + T_pb_out)/2. )*1000.0;				// field/pb htf in storage HX when storage discharging
				        c_htf_charge	= field_htfProps.Cp( (T_field_out + Ts_cold)/2. )*1000.0;			// field/pb htf in storage HX when storage charging
                    }
                    else {
                        c_htf_disch = field_htfProps.Cp(Ts_hot)*1000.0;
                        c_htf_charge = field_htfProps.Cp(Ts_cold)*1000.0;
                    }
                }
                else{
                    c_htf_disch = 0.;
                    c_htf_charge = 0.;
                }
				c_htf_field		= field_htfProps.Cp( (T_field_out + T_field_in_guess)/2. )*1000.0;	// mjw 1.18.2011
				c_htf_pb		= field_htfProps.Cp( (T_pb_in + T_pb_out)/2. )*1000.0;				// mjw 7.12.2010 Specific heat for the power block

				// MJW 7.12.2010: The mass flow should be adjusted to be over design until the demand mass flow rate is met
				// Calculate the mass flow rate associated with this load, given the applicable HTF temps
		
				// Recalculate the demand mass flow rate based on this new power block flow rate
				if(T_pb_in > T_pb_out + 0.01)
				{
					double m_dot_dum = min(q_pb_demand/max(c_htf_pb*(T_pb_in - T_pb_out),1.e-6),m_dot_pb_max);
					q_pb_demand_guess = m_dot_dum* c_htf_pb*(T_pb_in - T_pb_out);
				}
				else
				{q_pb_demand_guess = q_pb_demand;}

				if(tes_type==2 || ! has_TES)    // TODO - why is the available qs_disch and charge 0 for thermoclines?
				{
					qs_disch_avail	= 0.0;
					qs_charge_avail = 0.0;
				}
				else
				{
					qs_disch_avail = max(ms_disch_avail*c_htf_disch*(Ts_hot - T_pb_out),0.0);        // current max heat gain to field/pb htf during storage discharging
					qs_charge_avail = max(ms_charge_avail*c_htf_charge*(T_field_out - Ts_cold),0.0); // current max heat loss from field/pb htf during storage charging
				}

				q_aux_avail		= max(m_dot_aux_avail*c_htf_aux*(T_set_aux - T_pb_out),0.0); 
				q_field_avail	= max(m_dot_field_avail*c_htf_field*(T_field_out - T_field_in_guess),0.0);		// mjw 1.18.2011

				// Initial values
				q_int	= 0.0;			// Intermediate equivalent power output from the various heat sources
				T_int	= T_pb_out;		// Set intermediate temperature to outlet temperature for now
				m_int	= 0.0;			// Intermediate mass flow from the various heat sources

				// Logic to determine if packed bed model has been called during timestep
				called_TC	= false;

				// Allocate the flows from the various sources
				// Start for the case where the energy available from the field is less than  
				// or equal to the design requirement
				if(q_field_avail <= q_pb_demand_guess)
				{
					//First add the available energy from the field
					q_int = q_field_avail;  
					m_int = m_dot_field_avail;
			
					// If energy is coming from the field, adjust the intermediate temperature
					if(q_int > 0.) {tanks_in_parallel ? T_int = T_field_out : T_int = T_tank_hot_out;}

					// Next, try to add energy from thermocline 
					if( (ms_disch_avail>0.0)&&(tes_type==2))   
					{
						double q_TC_demand = q_pb_demand_guess - q_int;

						double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9;
						dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = -999.9;
						thermocline.Solve_TC( T_field_out - 273.15, 0.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, q_TC_demand, 0.0, f_storage, step/3600.0,
						                        ms_disch_avail, Ts_hot, dummy1, dummy2, qs_disch_avail, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9 );

						ms_disch_avail /= 3600.0;				//[kg/s]
						Ts_hot += 273.15;						//[K]

						called_TC = true;
					}
					// else: 2-tank, so use qs_disch_avail
					if((q_field_avail + qs_disch_avail) < q_pb_demand_guess) 
					{
						// If the total amount of energy is less than the design condition, 
						// add all of the remaining energy from storage
						q_int		= q_int + qs_disch_avail;
						ms_disch	= ms_disch_avail;
						ms_charge	= 0.;
						m_int		= m_dot_field_avail + ms_disch;
						//Adjust the intermediate temperature, if needed
						if(q_int > 0.) T_int = (T_int*m_dot_field_avail + Ts_hot*ms_disch)/(m_int);

						// MJW 11.1.2010: The auxiliary heater can operate in 1 of 2 modes.. 
						// mode 1: Fossil fraction specifies the minimum pb fraction during the period. Fossil provides the balance.
						// mode 2: Fossil tops off the available energy. The total fossil contrib. can't exceed the control fraction.
						if (fossil_mode == 1)
						{
							q_demand_aux = ffrac[touperiod]*q_pb_max_input;     // the overall minimum needed energy input to power block
							if(q_int < q_demand_aux)
							{
								q_aux		= q_demand_aux - q_int;
								q_int		= q_demand_aux;
								m_dot_aux	= q_aux/(c_htf_aux*max((T_set_aux - T_pb_out),1.0));
								m_int		= m_int + m_dot_aux;
								// Adjust the intermediate temperature, if needed
								if(q_int > 0.) T_int = (T_int*(m_dot_field_avail + ms_disch) + T_set_aux*m_dot_aux)/m_int;
							}
							else	// MJW 1.12.2011
							{
								q_aux		= 0.0;
								m_dot_aux	= 0.0;
							}
						}
						else if (fossil_mode == 2)
						{
							// Lastly, add any available auxiliary heat to get to the design point
							if((q_int + q_aux_avail) < q_pb_demand_guess)
							{   // TODO - add a q_aux here and factor out the 'if(q_int > 0.)' lines, changing q_int to q_aux, and put after the 'if(fossil_mode == 1)' block
								q_int		= q_int + q_aux_avail;
								m_dot_aux	= m_dot_aux_avail;
								m_int		= m_int + m_dot_aux;
								// Adjust the intermediate temperature, if needed
								if(q_int > 0.) T_int = (T_int*(m_dot_field_avail + ms_disch) + T_set_aux*m_dot_aux)/m_int;
							}
							else
							{
								// The aux heater was able to produce more energy than what was needed
								q_aux		= q_pb_demand_guess - q_int;
								q_int		= q_pb_demand_guess;
								// Set the required mass flow rate
								m_dot_aux	= q_aux/(c_htf_aux*max((T_set_aux - T_pb_out),1.0));  
								m_int		= m_int + m_dot_aux;
								// Adjust the intermediate temperature
								//T_int = (m_dot_field_avail*T_field_out + ms_disch*Ts_hot + m_dot_aux*T_set_aux)/m_int;
                                if (q_int > 0.) T_int = (T_int*(m_dot_field_avail + ms_disch) + T_set_aux * m_dot_aux) / m_int;
							}
						}				
					}
					else
					{
						// Between the energy available from the field and from storage, the dispatch requirement
						// has been met.  No aux load will be added in this condition              
						q_int = q_pb_demand_guess;
						if(tes_type==2)	ms_disch = ms_disch_avail;		// Thermocline
						else			ms_disch = (q_pb_demand_guess - q_field_avail)/(c_htf_disch * (Ts_hot - T_pb_out));		//2-tank  
						m_int		= m_int + ms_disch;
						ms_charge	= 0.;
						m_dot_aux	= 0.;
						T_int		= (m_dot_field_avail*T_int + ms_disch*Ts_hot)/m_int;
					}

					// Run a check to see if the energy produced is above the cycle cutout fraction
					if(q_int < q_pb_design*cycle_cutoff_frac)	mode = pb_off_or_standby;
					else	mode = pb_partial_load;
				}
				else
				{
					// The flow from the field is sufficient to meet the power block demand. If the system has storage
					// the excess flow will be used to charge it.
			
					// MJW 11.3.2010: Need to specify m_int in cases where we've entered "sticky mode" operation
					m_int	= m_dot_field_avail;
                    tanks_in_parallel ? T_int = T_field_out : T_int = T_tank_hot_out;
			
					if((ms_charge_avail>0.0)&&(tes_type==2))
					{
						//Thermocline
						double q_charge_demand = q_field_avail - q_pb_demand_guess;

						double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9;
						dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = -999.9;

						thermocline.Solve_TC( T_field_out - 273.15, 0.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, 0.0, q_charge_demand, f_storage, step/3600.0,
							                   dummy1, dummy2, ms_charge_avail, Ts_cold, qs_charge_avail, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9 );

						ms_charge_avail /= 3600.0;
						Ts_cold += 273.15;

						called_TC = true;
					}
			
					// Check if the excess flow can be dropped into storage
					if((q_field_avail - q_pb_demand_guess) > qs_charge_avail)	mode = excess_energy;
					else	mode = charging_storage;

					m_dot_aux = 0.0;
				}

				// MJW 11.4.2010
				//if(ncall == 0)	defocus = 1.0;	//cc	
		
				// Check to make sure mode iteration is not happening
				if(ncall > 4)
				{ 
					if(ncall > 8)	mode = mode_prev_ncall;
					if((defocus<1.0)&&(mode_prev_ncall==charging_storage))	mode = charging_storage;	// MJW 11.10.2010 don't let it switch from 4 in this case
				}
		
				// MJW 12.9.2010 -- 
				// enforce special considerations for when the turbine is off and wants to restart
				if((pb_on_prev==0)&&(mode!=pb_off_or_standby))
				{
					// Look ahead for DNI resource to decide whether cycle should start back up
					if((dnifc<I_bn_des*cycle_cutoff_frac)&&(fc_on))	mode = pb_off_or_standby;
					// If it's night time and TES is 0 and power cycle is below cutoff then don't restart the turbine
					if(m_dot_field_avail == 0. && (qs_disch_avail < q_pb_design*cycle_cutoff_frac) )	
                        mode = pb_off_or_standby;
				}

                // MJW 6.18.2018
                // If the power cycle cannot operate due to user-enforced dispatch fraction requirements...
                if (tslogic_c[touperiod] < cycle_cutoff_frac )
                {
                    mode = pb_off_or_standby;
                }
		
				if(mode == pb_off_or_standby)	// mjw 4.21.11 Operation here can either be totally off or running in standby
				{
					//Enter standby mode if the following criteria are met: (1a) Standby time remains and (1b) The available energy from the solar field 
					// ..and/or storage is greater than the standby requirement; AND (2a) The power block was previously running; or (2b) it was previously operating in standby
					// Any of the following criteria will send the power block into normal shutdown (not standby mode)
					bool goto_shutdown = false;

					// *** What if forecasting is not "on"??
					if(dnifc <= 0.001 && fc_on) goto_shutdown = true;			//goto 11		// If solar radiation is not expected (Type 215 only); alternatively, if the sun is below the horizon (Type 15 only)
			
					if(t_standby_prev <= 0.0) goto_shutdown = true;	//goto 11		// If the standby time has been exhausted
					if(qs_disch_avail + q_field_avail < q_sby) goto_shutdown = true;	//goto 11   //!If the total available energy from the field+TES is less than the standby requirement
					if((pb_on_prev != 1.) && (t_standby_prev == t_standby_reset)) goto_shutdown = true;	//goto 11   //!If the power block hasn't been running normally or in standby mode in the previous step
		
					if(!goto_shutdown)
						{
						//**********************************************
						//********** Standby Operation *****************
						//**********************************************			
						if( has_TES )
						{
							if( q_field_avail < q_sby )	// discharge  
							{         
								double q_sby_storage   = max(q_sby - q_field_avail, 0.0);	//[W] Energy required from storage to sustain standby mode
								ms_charge = 0.0;      //[kg/s] Charge mass flow rate
			    
								if(tes_type == 2)	// thermocline code
								{
									double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10;
									dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = dummy10 = -999.9;

									thermocline.Solve_TC( T_field_out - 273.15, 0.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, q_sby_storage, 0.0, f_storage, step/3600.0,
										                   ms_disch, Ts_hot, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10 );
									ms_disch /= 3600.0;
									Ts_hot += 273.15;

									called_TC = true;
								}
								else	// 2-tank
								{ms_disch = q_sby_storage / (c_htf_disch * (Ts_hot - T_pb_out));}        //[kg/s]

                                if (tanks_in_parallel) {
								    T_pb_in = (T_field_out*m_dot_field_avail + Ts_hot*ms_disch)/(m_dot_field_avail + ms_disch);	//[K]
                                }
                                else {
                                    T_pb_in = Ts_hot;
                                }
			    
							}
							else		// (q_field_avail > q_sby), need to charge/defocus
							{
								ms_disch	= 0.0;	//[kg/s] Discharge mass flow rate
                                tanks_in_parallel ? T_pb_in = T_field_out : T_pb_in = T_tank_hot_out; //[K]
								ms_charge	= max(m_dot_field_avail - q_sby/(c_htf_pb*(T_pb_in - T_pb_out)), 0.0);		//[kg/s] Calculate mass flow rate that is not used by PB in
			                
								if(tes_type == 2)	// thermocline code
								{ 									
									double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9;
									dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = -999.9;

									thermocline.Solve_TC( T_field_out - 273.15, ms_charge*3600.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, 0.0, 0.0, f_storage, step/3600.0,
										                     dummy1, dummy2, ms_charge, Ts_cold, qs_charge_avail, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9 );
										       
									ms_charge /= 3600.0;
									Ts_cold += 273.15;
									
									called_TC = true;
								}
								else	// 2-tank
								{
									ms_charge = min(ms_charge_avail, ms_charge);	// limit charge mass flow rate to the max. available
								}
			    
								if(m_dot_field_avail > 0.)
									//!defocus = defocus0*((qs_charge_avail+q_sby)/qfield_avail)**ccoef			//Absolute defocus
								{defocus = pow( ((qs_charge_avail+q_sby)/q_field_avail), ccoef);}    //!TN 4.9.12, relative defocus
								else
									//!defocus = defocus0
								{defocus = 1.0;}	//TN 4.9.12
							}	
						}
						else	//tshours == 0
						{
							defocus		= 1.0;
							m_dot_field_avail = 0.0;
							T_pb_in		= T_field_out;
						}  				
				
						m_dot_pb = 0.0;		//!the power block code will calculate the mass flow for this mode
						// charge storage with any additional field flow, if possible
						// T_pb_in = (T_field_out*m_dot_field_avail + Ts_hot*ms_disch)/(m_dot_field_avail + ms_disch)
				    
						cycle_pl_control	= 2;		// 1=demand, 2=part load
						standby_control		= 2;		// 1=normal operation, 2=standby, 3=shut down
						// Zero out the fossil backup contribution if we're below the turbine minimum
						q_aux = 0.0; m_dot_aux = 0.0;
						t_standby = max(t_standby_prev - step, 0.0);	//[s] 

					}
					else if( goto_shutdown )	//SHUTDOWN
					{
						// normal, non-standby operation
						// If below the cutout fraction, check to see if extra energy can be dumped into storage
						if((m_dot_field_avail>0.0)&&(ms_charge_avail>0.0)&&(tes_type==2))
						{
							// thermocline code
							double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9;
							dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = -999.9;

							thermocline.Solve_TC( T_field_out - 273.15, m_dot_field_avail*3600.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, 0.0, 0.0, f_storage, step/3600.0,
								                    ms_charge_avail, Ts_cold, qs_charge_avail, dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9 );
							ms_charge_avail /= 3600.0;
							Ts_cold += 273.15;

							called_TC = true;
						}
						else if((ms_disch>0.0)&&(tes_type==2))
						{
							called_TC = false;	
						}

						ms_charge	= max( min(ms_charge_avail, m_dot_field_avail), 0.0);
						ms_disch	= 0.0;
						m_dot_pb	= 0.0;
						T_pb_in		= T_pb_out;

						if(has_TES)
						{    
							if(m_dot_field_avail > 0.)
							{
								// Defocus according the the amount of charge accepted as compared to what is produced. 
								// defocus = defocus0*(ms_charge/m_dot_field_avail)**ccoef !MJW 12.9.2010
								defocus = pow( (ms_charge/m_dot_field_avail), ccoef);	// TN 4.9.12
							}
							else
							{
								// defocus = defocus0
								defocus = 1.0;	// TN 4.9.12
							}
						}
						else
						{
							// Here we need to allow the field to run normally and artificially force the power block to dump energy
							defocus		= 1.0;
							m_dot_field_avail = 0.0;
						}
						cycle_pl_control	= 2;	// 1=demand, 2=part load
						standby_control		= 3;	// 1=normal operation, 2=standby, 3=shut down
						// MJW 11.3.2010: Zero out the fossil backup contribution if we're below the turbine minimum
						q_aux = 0.0; m_dot_aux = 0.0;
						t_standby = t_standby_reset;				
					}
				}
				else if( mode == pb_partial_load )
				{
					// Operation below design point. Continue with calculated operation values
					if(fthr_ok)
					{
						// defocus = dmin1(defocus0*((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail,1.d0))**ccoef, 1.d0)  !mjw 1.18.2010
						defocus = pow( ((qs_charge_avail + q_pb_demand_guess)/max(q_field_avail,1.0)), ccoef);		// TN 4.9.12
					}
					else
					{    
						// defocus = dmax1(defocus0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail, 1.d0))**ccoef)/nSCA)
						defocus = ((int)(nSCA*pow( ((qs_charge_avail + q_pb_demand_guess)/max(q_field_avail, 1.0)), ccoef)))/nSCA;	// TN 4.9.12
					}
					m_dot_pb	= m_int;	//[kg/s] The mass flow rate to the power cycle
					T_pb_in		= T_int;	//[K] The HTF temperature to the power block
					cycle_pl_control= 2;	// 1=demand, 2=part load
					standby_control = 1;    // 1=normal operation, 2=standby, 3=shut down
						// twn, 9/17/14: But this overwrites calculated m_dot_aux from above code that determines how much energy is available to power block
					//m_dot_aux = 0.0;		// TFF, June 28, 2013 - could be used below without being initialized if in this mode the first time this is called
				}
				else if( mode == excess_energy )
				{
					// More thermal energy is available than what can be put into storage
					if(tes_type==2)
					{ms_charge = ms_charge_avail;}
					else
					{ms_charge = qs_charge_avail/(c_htf_field*(T_field_out - Ts_cold));}

					// MJW 11.9.2010: adjust defocus calculation relative to the previous. 0.4 coef tested for speed and convergence
					if(fthr_ok)
					{
						// defocus = defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef    !MJW 11.10.2010
						defocus = pow( ((qs_charge_avail + q_pb_demand_guess)/q_field_avail), ccoef);	// TN 4.9.12
					}
					else
					{
						// defocus = defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA  !MJW 12.6.2010
						defocus = ((int)(nSCA*pow( ((qs_charge_avail + q_pb_demand_guess)/max(q_field_avail, 1.0)), ccoef)))/nSCA;	// TN 4.9.12
					}

                    tanks_in_parallel ? T_pb_in = T_field_out : T_pb_in = T_tank_hot_out;
					m_dot_pb	= max(q_pb_demand_guess/(c_htf_pb*(T_pb_in - T_pb_out)), 0.0);		// mjw 1.18.2011
					ms_disch	= 0.;
					cycle_pl_control= 2;	// 1=demand, 2=part load
					standby_control = 1;	// 1=normal operation, 2=standby, 3=shut down
					m_dot_aux	= 0.;
				}
				else if( mode == charging_storage )
				{
					// More energy is available than can be used in the PB, but all of the extra thermal energy can be dropped into storage
					// ..storage charge rate limited to max available charge rate  MJW 7.13.2010
					// ms_charge = dmax1(dmin1(qfield_avail - q_pb_demandX, qs_charge_avail)/(c_htf_fld*dmax1(T_field_out - Ts_cold,1.d0)),0.d0)  !MJW 7.15.2010 1.e-6 -> 1.
					if(tes_type==2)
					{ms_charge = ms_charge_avail;}
					else
					{ms_charge = max(min(q_field_avail - q_pb_demand_guess, qs_charge_avail)/(c_htf_charge*max(T_field_out - Ts_cold, 1.0)), 0.0);}	// MJW 3.6.11 changing c_htf

					if(fthr_ok)
					{
						// defocus = dmax1(defocus0, defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)   !MJW 11.10.2010
						// defocus = dmin1(1.d0, defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)  !mjw 3.7.11
						defocus = pow( ((qs_charge_avail + q_pb_demand_guess)/q_field_avail), ccoef);		// TN 4.9.12
					}
					else
					{
						// defocus = dmax1(defocus0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA)  !MJW 12.6.2010
						// defocus = dmin1(1.d0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA)  !MJW 12.6.2010
						defocus = ((int)(nSCA*pow( ((qs_charge_avail + q_pb_demand_guess)/max(q_field_avail, 1.0)), ccoef)))/nSCA;	// TN 4.9.12
					}

					// m_dot_pb = dmax1(q_pb_demandX/(c_htf_fld*dmax1(T_field_out - T_pb_out,1.d0)),0.d0)      !MJW 7.15.2010 1.e-6 -> 1.
					m_dot_pb	= max(q_pb_demand_guess/(c_htf_pb*max(T_pb_in - T_pb_out, 1.0)) ,0.0);		// MJW 3.6.11 changing c_htf
                    tanks_in_parallel ? T_pb_in = T_field_out : T_pb_in = T_tank_hot_out;
					ms_disch	= 0.0;
					cycle_pl_control= 2;	// 1=demand, 2=part load
					standby_control = 1;	// 1=normal operation, 2=standby, 3=shut down
					m_dot_aux = 0.0;
			
					// Rarely, this mode is selected when the solar field outlet temp. drops. Protect from this
					// by enforcing limits on the temperature that can be accepted by the power block. Limit to 25% of 
					// difference between the design field inlet/outlet temp, and turn off operation. MJW 7.15.2010
					if((T_pb_in < T_field_in_guess + (T_field_out_des-T_field_in_des)*.25)&&(ncall>15))	mode = pb_off_or_standby;
				}
		
				// mode_prev_ncall = mode;		// 10.15.12 TN: Can't this go after the iteration in the main call?

				// Calculate the new field inlet temperature. Stick with the same temperature calculation mode as determined on the first call.
				// MJW 12.9.2010----		
				if((ncall==0)||(mode_prev_ncall != mode))
				{
					if(m_dot_field_avail > 0.)	//mjw 1.12.2011 Be consistent with the criteria above
						tempmode = 1;
					else
						tempmode = 2;
				}
				// If during iteration, the mass flow ends up being zero, switch to temperature mode 2
				if(m_dot_pb + ms_charge == 0.) tempmode = 2;
                if (!tanks_in_parallel) {
                    T_field_in_guess = T_tank_cold_out;
                }
                else if (tempmode == 1) {
                    T_field_in_guess = (m_dot_pb*T_pb_out + ms_charge * Ts_cold) / (m_dot_pb + ms_charge);
                }
				else if(tempmode == 2)
				{
					if( sf_type == 1 )
						T_field_in_guess = min(T_field_out, T_startup);
					else
						T_field_in_guess = T_field_in_des;
						
					//T_field_in_guess = T_field_in_des;
						
					//T_field_in_guess = (m_dot_pb*T_pb_out + ms_charge*Ts_cold)/(m_dot_pb + ms_charge);
				}
				
				// convergence error is based on actual field mass flow rate compared to balance of mass flow rates
				err = sqrt(
						pow( ((ms_charge + m_dot_pb - ms_disch) - m_dot_field_avail)/max(m_dot_field_avail, 1.e-6), 2) + 
						pow( ((T_field_in - T_field_in_guess)/T_field_in), 2));
                if (err_prev_iter != 0) {
                    derr = fabs((err - err_prev_iter) / err_prev_iter);
                }
                else if (err != 0) {
                    derr = fabs((err - err_prev_iter) / err);
                }
                else {
                    derr = fabs(err - err_prev_iter);
                }

				err_prev_iter = err;

				iterate_mass_temp = true;

				// Limit number of iterations
				if(iter > 20)
				{
					// Write the error message in preparation for failure, but don't call the printer yet.. 
					message(TCS_NOTICE, "After %d iterations, the controller convergence error of %lg could not resolve below the tolerance of %lg. Check that results at this timestep are not unreasonably biasing annual metrics.", iter, err, tol);
					//200 format("After ",I2," iterations, the controller convergence error of ",E8.3," could not resolve below the tolerance of ",F6.4)
					//continue on, don't iterate further
					iterate_mass_temp = false;
				}
				else
				{
					// If the defocus control has changed during the calculations, bypass iteration and recalculate field output.
					// Otherwise, the iteration will continue until the max # has been reached.
                    if( abs(defocus - defocus_rel_prev_ncall) > 0.01 )
					{
                        defocus_prev_iter = defocus;    // Does this do anything? iterate_mass_temp = false exits this loop, and defocus_prev_iter is set to 1 just before this loop
						T_field_in = T_field_in_guess;	// MJW 12.8.2010
						iterate_mass_temp = false;
					}
					else
					{
						// We are primarily concerned with whether the solution has converged. 
						// If the change in the error has stabilized, it is safe to move on.
						if(derr < 0.001)
						{
							iterate_mass_temp = false;
						}
						else
						{
							// If the error hasnt stabilized and the absolute error is greater than the tolerance, iterate again
							if(err > tol)
							{
								iterate_mass_temp = true;
							}
							else
							{
								iterate_mass_temp = false;
							}
						}  				
					}
				}

                if (err > tol) {
                    // create adjusted mass flows so they align, and use later to ensure mass balance
                    if (recirculating == false) {
                        if (m_dot_pb <= 0) {    // adjust field
                            m_dot_pb_adj = 0;
                            m_dot_field_adj = ms_charge + m_dot_pb - ms_disch;
                        }
                        else {  // adjust power block
                            m_dot_pb_adj = m_dot_field_avail - ms_charge + ms_disch;
                            m_dot_field_adj = m_dot_field;
                        }
                    }
                    else {      // adjust power block
                        // recirculating == true
                        m_dot_pb_adj = ms_disch;
                        m_dot_field_adj = m_dot_field;
                    }
                }
                else {
                    m_dot_pb_adj = m_dot_pb;
                    m_dot_field_adj = m_dot_field;
                }

			} while (iterate_mass_temp);

			if(is_hx)
			{
				// Calculate flows through the heat exchanger
				// Charging cycle: Collector is hot side - Tanks are cold side
				if(ms_charge > 0.001)
				{
					if( ! hx_storage.hx_performance( true, false, T_field_out, ms_charge, T_tank_cold_out, eff_charge, Ts_cold, T_tank_hot_in, q_charge, m_tank_charge ) )
					{
						message( TCS_ERROR, "heat exchanger performance calculations failed" );
						return -1;
					}
                    m_tank_cold_out = m_tank_hot_in = m_tank_charge;
                    m_tank_cold_in = m_tank_hot_out = 0.;
					eff_disch = -9.99; T_tank_cold_in = T_tank_cold_prev; q_disch = 0; m_tank_disch = 0.;

					// 7/9/14 twn: Calculate the specific heat with the same temps as 'hx_perf' uses so that reported energy in / energy out is consistent
					c_htf_charge = field_htfProps.Cp(0.5*(T_field_out+T_tank_cold_out))*1000.0;
				}
			
				// Discharging cycle: Tanks are hot side - Collector loop is cold side
				else if(ms_disch > 0.001)
				{
					if( ! hx_storage.hx_performance( false, false, T_tank_hot_out, ms_disch, T_pb_out, eff_disch, T_tank_cold_in, Ts_hot, q_disch, m_tank_disch ) )
					{
						message( TCS_ERROR, "heat exchanger performance calculations failed" );
						return -1;
					}
                    m_tank_cold_in = m_tank_hot_out = m_tank_disch;
                    m_tank_cold_out = m_tank_hot_in = 0;
					eff_charge = -9.99; T_tank_hot_in = T_tank_hot_prev; q_charge = 0; m_tank_charge = 0.;

					// 7/9/14 twn: Calculate the specific heat with the same temps as 'hx_reverse' uses so that reported energy in / energy out is consistent
					c_htf_disch = field_htfProps.Cp(0.5*(T_tank_hot_out+T_pb_out))*1000.0;
				}
				else
				{
					// No flow across the storage tank
					Ts_cold		= T_pb_out;
					Ts_hot		= T_field_out;
					T_tank_cold_in	= T_field_in;
					T_tank_hot_in	= T_field_out;
					m_tank_charge = m_tank_disch = 0.;
                    m_tank_cold_in = m_tank_cold_out = m_tank_hot_in = m_tank_hot_out = 0.;
				}
			}
			else
			{
				if(tes_type==2)
				{
					// Set hx_eff in outputs
					//hx_eff = 0.d0

					m_tank_charge = 0.0;
					m_tank_disch = 0.0;
                    m_tank_cold_in = m_tank_cold_out = m_tank_hot_in = m_tank_hot_out = 0.;     // TODO - are these needed?:
					T_tank_cold_in = T_field_in_des;
					T_tank_hot_in = T_field_out_des;
					T_tank_hot_out = T_field_out_des;
					T_tank_cold_out = T_field_in_des;
				}
				else
				{
					//No heat exchanger
                    if (tanks_in_parallel) {
                        if (recirculating) {
                            if (has_hot_tank_bypass) {
                                T_tank_cold_in = (m_dot_field * T_field_out + m_dot_pb * T_pb_out) / (m_dot_field + m_dot_pb);
                                m_tank_cold_in = ms_disch;    // = m_dot_field + m_dot_pb;
                                m_tank_cold_out = ms_charge;   // = m_dot_field;
                                m_tank_hot_in = 0.;
                                m_tank_hot_out = ms_disch;    // = m_dot_pb;
                            }
                            else {  // both tanks bypassed
                                T_tank_cold_in = T_pb_out;
                                m_tank_cold_in = ms_disch;    // = m_dot_pb;
                                m_tank_cold_out = 0.;
                                m_tank_hot_in = 0.;
                                m_tank_hot_out = ms_disch;    // = m_dot_pb;
                            }
                        }
                        else {
                            T_tank_cold_in = T_pb_out;
                            m_tank_cold_in = ms_disch;
                            m_tank_cold_out = ms_charge;
                            m_tank_hot_in = ms_charge;
                            m_tank_hot_out = ms_disch;
                        }
                    }
                    else {  // tanks in series
                        if (recirculating) {
                            m_tank_hot_in = 0.;
                            if (has_hot_tank_bypass) {
                                T_tank_cold_in = (m_dot_field * T_field_out + m_dot_pb * T_pb_out) / (m_dot_field + m_dot_pb);
                                if (std::isnan(T_tank_cold_in)) T_tank_cold_in = T_tank_cold_prev;
                                m_tank_cold_in = m_dot_field_adj + m_dot_pb_adj;   // adjusted to account for convergence errors
                                m_tank_cold_out = m_dot_field_adj;
                            }
                            else {  // both tanks bypassed
                                T_tank_cold_in = T_pb_out;
                                m_tank_cold_in = m_dot_pb_adj;
                                m_tank_cold_out = 0.;
                            }
                        }
                        else {
                            T_tank_cold_in = T_pb_out;
                            m_tank_cold_in = m_dot_pb_adj;
                            m_tank_cold_out = m_dot_field_adj;
                            m_tank_hot_in = m_dot_field_adj;
                        }
                        m_tank_hot_out = m_dot_pb_adj;
                    }

					T_tank_hot_in	= T_field_out;
					Ts_cold		= T_tank_cold_out;
					Ts_hot		= T_tank_hot_out;
					m_tank_charge	= min(ms_charge, ms_charge_avail);
					m_tank_disch	= min(ms_disch, ms_disch_avail);
				}
			}

			double err_tank;
			iterate_tank_temp = false;
			if(has_TES)
			{
				if(tes_type==1)
				{
					// Hot Tank					
					hx_storage.mixed_tank( true, step, m_tank_hot_prev, T_tank_hot_prev, m_tank_hot_in, m_tank_hot_out, 
											T_tank_hot_in, T_amb, T_tank_hot_avg, vol_tank_hot_avg, q_loss_tank_hot,
											T_tank_hot_fin, vol_tank_hot_fin, m_tank_hot_fin, q_htr_tank_hot );
					// Cold Tank					
					hx_storage.mixed_tank( false, step, m_tank_cold_prev, T_tank_cold_prev, m_tank_cold_in, m_tank_cold_out,
											T_tank_cold_in, T_amb, T_tank_cold_avg, vol_tank_cold_avg, q_loss_tank_cold,
											T_tank_cold_fin, vol_tank_cold_fin, m_tank_cold_fin, q_htr_tank_cold );
			
					err_tank = sqrt(
								pow( ((T_tank_hot_avg_guess - T_tank_hot_avg)/max(T_tank_hot_avg, 1.e-6)), 2) +
								pow( ((T_tank_cold_avg_guess - T_tank_cold_avg)/max(T_tank_cold_avg, 1.e-6)), 2) );
					T_tank_hot_avg_guess = T_tank_hot_avg;
					T_tank_cold_avg_guess = T_tank_cold_avg;
					if( err_tank > 1.e-5 && iter_tank < 10 )	iterate_tank_temp = true;
				}
				else
				{
					// Thermocline stuff
				}
			}
			else		// No storage
			{
				// Not sure this is necessary...
				T_tank_hot_avg = vol_tank_hot_avg = q_loss_tank_hot = T_tank_hot_fin = vol_tank_hot_fin = q_htr_tank_hot;
				T_tank_cold_avg = vol_tank_cold_avg = q_loss_tank_cold = T_tank_cold_fin = vol_tank_cold_fin = q_htr_tank_cold;
				// and what about m_tank_hot/cold_fin?
			}

		} while( iterate_tank_temp );

		double T_aux_out, q_aux_delivered, q_aux_fuel;
        err_prev_call = err;
        derr_prev_call = derr;

		if(q_aux_avail > 0.0)
		{
			double cp_aux = field_htfProps.Cp( (T_set_aux + T_pb_out)/2. )*1000.;		//[J/kg-K]
			T_aux_out = min( T_set_aux, (T_pb_out + (q_max_aux)/(m_dot_aux*cp_aux)) );	//[K]
			q_aux_delivered = m_dot_aux*cp_aux*(T_aux_out - T_pb_out)/1.e6;				//[MW]
			q_aux_fuel = q_aux_delivered/lhv_eff*step/1055.06;							//[MMBTU]
		}
		else
		{	
			T_aux_out = q_aux_delivered = q_aux_fuel = 0.0;
		}

		defocus_abs = min(defocus_prev_ncall*defocus, 1.0);    // defocus_abs = defocus_abs_prev * defocus_rel

		// Reset mode and defocus
		mode_prev_ncall = mode;
		defocus_prev_ncall = defocus_abs;                      // absolute defocuses
        recirc_prev_ncall = recirculating;
		
		// Calculate final output values
		// double pb_on;
		if(m_dot_pb > 0.0)
		{pb_on = 1;}
		else {pb_on = 0;}

		// Pumping power
		double htf_pump_power;
        if (custom_tes_p_loss) {
            double rho_sf, rho_pb;
            double DP_col, DP_gen;
            sgs_pressure_drops(m_dot_field, m_dot_pb, SGS_v_dot_rel, T_field_in, T_field_out, T_pb_in, T_pb_out,
                SGS_lengths, SGS_diams, HDR_rough, DP_SGS, k_tes_loss_coeffs, tanks_in_parallel, recirculating, DP_col, DP_gen);
            rho_sf = field_htfProps.dens((T_field_in + T_field_out) / 2., 8e5);
            rho_pb = field_htfProps.dens((T_pb_in + T_pb_out) / 2., 1e5);
            if (is_hx) {
                // TODO - replace tes_pump_coef with a pump efficiency. Maybe utilize unused coefficients specified for the
                //  series configuration, namely the SGS Pump suction header to Individual SGS pump inlet and the additional
                //  two immediately downstream
                htf_pump_power = tes_pump_coef * fabs(m_tank_disch - m_tank_charge) / 1000 +
                    (DP_col * m_dot_field / (rho_sf * eta_pump) + DP_gen * m_dot_pb / (rho_pb * eta_pump)) / 1e6;   //[MW]
            }
            else {
                htf_pump_power = (DP_col * m_dot_field / (rho_sf * eta_pump) + DP_gen * m_dot_pb / (rho_pb * eta_pump)) / 1e6;   //[MW]
            }
        }
        else {    // original methods
            if (is_hx) {
                htf_pump_power = (tes_pump_coef*fabs(m_tank_disch - m_tank_charge) + pb_pump_coef * (fabs(ms_disch - ms_charge) + m_dot_pb)) / 1000.0;	//[MW]
            }
            else {
                if (tanks_in_parallel) {
                    htf_pump_power = pb_pump_coef * (fabs(ms_disch - ms_charge) + m_dot_pb) / 1000.0;	//[MW]
                }
                else {
                    htf_pump_power = pb_pump_coef * m_dot_pb / 1000.0;	//[MW]
                }
            }
        }

        // Variable parasitic power
		double bop_par;
		double q_pb = m_dot_pb*c_htf_pb*(T_pb_in - T_pb_out);		//[MW]
		if(q_pb > 0.0)
		{
			bop_par = W_pb_design*bop_array[0]*bop_array[1]*min( (bop_array[2] + bop_array[3]*(q_pb/q_pb_design) + bop_array[4]*pow( q_pb/q_pb_design, 2 )), cycle_max_frac)/1.e6;	//[MW]
		}
		else	{bop_par = 0.0;}

		// Aux load parasitic power
		double aux_par;
		if(m_dot_aux > 0.0)
		{
			aux_par = W_pb_design*aux_array[0]*aux_array[1]*min( (aux_array[2] + aux_array[3]*(q_aux_delivered/q_pb_design) + aux_array[4]*pow( q_aux_delivered/q_pb_design, 2 )), cycle_max_frac)/1.e6;	//[MW]
		}
		else	{aux_par = 0.0;}

		double q_to_tes;
		if(has_TES)
		{
			if(ms_disch != 0)	q_to_tes = -ms_disch*c_htf_disch*(Ts_hot - T_pb_out)*1.e-6;			//[MW]
			if(ms_charge != 0)	q_to_tes = ms_charge*c_htf_charge*(T_field_out - Ts_cold)*1.e-6;	//[MW]
			if(ms_charge == 0.0 && ms_disch == 0.0)		q_to_tes = 0.0;		//[MW]
		}
		else	q_to_tes = 0.0;

		// If packed bed model has not yet been called, call it under "idle" conditions to find losses and temperature profile at next timestep
		// Would like to move this to final timestep call, but then couldn't report losses and the such to printer
		// New TRNSYS may be able to do this.....
		if( tes_type == 2 && !called_TC )
		{
			double dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12;
			dummy1 = dummy2 = dummy3 = dummy4 = dummy5 = dummy6 = dummy7 = dummy8 = dummy9 = dummy10 = dummy11 = dummy12 = -999.9;

			thermocline.Solve_TC( T_field_out - 273.15, 0.0, T_pb_out - 273.15, 0.0, T_amb - 273.15, 1, 0.0, 0.0, f_storage, step/3600.0, 
				                   dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12 );

			called_TC = true;		//[-] Packed bed model has been called during timestep
		}
		if( tes_type == 2 )
		{
			q_htr_tank_cold = thermocline.GetHeaterLoad_kJ()/(1000.0*step);		//[kJ]*(1/1000)[MJ/kJ]*[1/s] => MWe Parasitic input
			q_htr_tank_hot = 0.0;		//[kJ] Only one tank....
			q_loss_tank_hot = thermocline.GetHeatLosses()/(1000.0*step);		//[kJ]*(1/1000)[MJ/kJ]*[1/s] => MWt Heat losses
			q_loss_tank_cold = 0.0;		//[MWt] Only calculate losses from 1 tank

			T_tank_hot_avg = vol_tank_hot_avg = T_tank_hot_fin = m_tank_hot_fin = 0.0;
			T_tank_cold_avg = vol_tank_cold_avg = T_tank_cold_fin = m_tank_cold_fin = 0.0;
		}

		// Set outputs
		value( O_defocus, defocus_abs );
        value( O_recirc, recirculating );
		value( O_standby, standby_control );
		value( O_m_dot_pb, m_dot_pb*3600.0 );
		value( O_T_pb_in, T_pb_in - 273.15 );
		value( O_T_field_in, T_field_in - 273.15 );
		value( O_charge_field, (ms_disch-ms_charge)*3600.0 );
		value( O_charge_tank, (m_tank_disch-m_tank_charge)*3600.0 );
		value( O_Ts_hot, Ts_hot - 273.15 );
		value( O_Ts_cold, Ts_cold - 273.15 );
		value( O_T_tank_hot_in, T_tank_hot_in - 273.15 );
		value( O_T_tank_cold_in, T_tank_cold_in - 273.15 );
		value( O_vol_tank_hot_fin, vol_tank_hot_fin );
		value( O_vol_tank_cold_fin, vol_tank_cold_fin );
		value( O_T_tank_hot_fin, T_tank_hot_fin - 273.15 );
		value( O_T_tank_cold_fin, T_tank_cold_fin - 273.15 );
		value( O_q_par_fp, q_htr_tank_hot + q_htr_tank_cold);
		value( O_m_dot_aux, (m_dot_aux*3600.0) );
		value( O_q_aux_heat, q_aux_delivered );		//[MW]
		value( O_q_aux_fuel, q_aux_fuel );			//[MMBTU]
		value( O_vol_tank_total, vol_tank_hot_fin + vol_tank_cold_fin );
		if( tes_type == 2 )
			value( O_hx_eff, 0.0 );
		else
			value( O_hx_eff, max(eff_charge, eff_disch) );
		value( O_mass_tank_hot, m_tank_hot_fin );
		value( O_mass_tank_cold, m_tank_cold_fin );
		value( O_mass_tank_total, m_tank_hot_fin + m_tank_cold_fin );
		value( O_htf_pump_power, htf_pump_power );
		value( O_bop_par, bop_par );
		value( O_fixed_par, W_pb_design*pb_fixed_par/1.e6 );
		value( O_aux_par, aux_par );
		value( O_q_pb, q_pb/1.e6 );
		value( O_tank_losses, q_loss_tank_cold+q_loss_tank_hot );
		value( O_q_to_tes, q_to_tes );
		value( O_mode, mode );
		value( O_TOU, touperiod+1 );

		if( tes_type == 2 )
		{
			double T_hot_node, T_cold_node, T_max, f_hot, f_cold;
			T_hot_node = T_cold_node = T_max = f_hot = f_cold = std::numeric_limits<double>::quiet_NaN();

			thermocline.GetFinalOutputs(T_hot_node, T_cold_node, T_max, f_hot, f_cold);

			value(O_T_hot_node, T_hot_node);
			value(O_T_cold_node, T_cold_node);
			value(O_T_max, T_max);
			value(O_f_hot, f_hot);
			value(O_f_cold, f_cold);
		}
		else
		{
			value(O_T_hot_node, 0.0);
			value(O_T_cold_node, 0.0);
			value(O_T_max, 0.0);
			value(O_f_hot, 0.0);
			value(O_f_cold, 0.0);
		}
		return 0;
	}

	virtual int converged( double time )
	{
		// To test main part of code, need to set "stored" variables to constants to match values at TRNSYS timestep = 39
		/*V_tank_hot_prev	= 1313.225;			//[m3]
		T_tank_hot_prev = 638.15;			//[K] convert from [C]
		V_tank_cold_prev= 24977.5598;		//[m3] Initial cold tank fluid volume
		T_tank_cold_prev= 572.323;			//[K] convert from [C]
		mode_prev_ncall	= 2;				//[-]
		m_tank_hot_prev	= 2439348.18;		//[kg]
		m_tank_cold_prev= 47450166.857;		//[kg]
		pb_on_prev		= 1;				//[-] power block initially off
		defocus_prev_ncall	= 1.;			//[-] initial defocus
		t_standby_prev	= 7200;				//[s] 
		*/
		
		// Actual converged code
		V_tank_hot_prev	= vol_tank_hot_fin;	//[m3]
		T_tank_hot_prev = T_tank_hot_fin;	//[K] convert from [C]
		V_tank_cold_prev= vol_tank_cold_fin;//[m3] Initial cold tank fluid volume
		T_tank_cold_prev= T_tank_cold_fin;	//[K] convert from [C]
		m_tank_hot_prev	= m_tank_hot_fin;	//[kg]
		m_tank_cold_prev= m_tank_cold_fin;	//[kg]
		pb_on_prev		= pb_on;			//[-] power block initially off
		t_standby_prev	= t_standby;		//[s]
        defocus_rel_prev_ncall = 1.;        //[-] previous relative defocus input from trough model (type250)
		defocus_prev_ncall = 1.;            //[-] previous absolute defocus output for trough model (type250)
        defocus_abs_prev = defocus_abs;

		//Warn if the heat exchanger performance calculations are having problems at convergence
		if(hx_err_flag) message(TCS_WARNING,  "Heat exchanger performance calculations failed" );

		if(time > 3600.0)
		{
			if( tes_type == 2 )
				thermocline.Converged(time);
		}

		return 0;
	}
	
    int size_sgs_piping(double vel_dsn, util::matrix_t<double> L, double rho_avg, double m_dot_pb, double solarm,
        bool tanks_in_parallel, double &vol_tot, util::matrix_t<double> &v_dot_rel, util::matrix_t<double> &diams,
        util::matrix_t<double> &wall_thk, util::matrix_t<double> &m_dot, util::matrix_t<double> &vel, bool custom_sizes = false)
    {
        const std::size_t bypass_index = 4;
        const std::size_t gen_first_index = 5;      // first generation section index in combined col. gen. loops
        double m_dot_sf;
        double v_dot_sf, v_dot_pb;                  // solar field and power block vol. flow rates
        double v_dot_ref;
        double v_dot;                               // volumetric flow rate
        double Area;
        vol_tot = 0.0;                              // total volume in SGS piping
        std::size_t nPipes = L.ncells();
        v_dot_rel.resize_fill(nPipes, 0.0);         // volumetric flow rate relative to the solar field or power block flow
        m_dot.resize_fill(nPipes, 0.0);
        vel.resize_fill(nPipes, 0.0);
        std::vector<int> sections_no_bypass;
        if (!custom_sizes) {
            diams.resize_fill(nPipes, 0.0);
            wall_thk.resize_fill(nPipes, 0.0);
        }

        m_dot_sf = m_dot_pb * solarm;
        v_dot_sf = m_dot_sf / rho_avg;
        v_dot_pb = m_dot_pb / rho_avg;

        //The volumetric flow rate relative to the solar field for each collection section (v_rel = v_dot / v_dot_pb)
        v_dot_rel.at(0) = 1.0 / 2;                  // 1 - Solar field (SF) pump suction header to individual SF pump inlet
                                                    //     50% -> "/2.0" . The flow rate (i.e., diameter) is sized here for the case when one pump is down.
        v_dot_rel.at(1) = 1.0 / 2;                  // 2 - Individual SF pump discharge to SF pump discharge header
        v_dot_rel.at(2) = 1.0;                      // 3 - SF pump discharge header to collection field section headers (i.e., runners)
        v_dot_rel.at(3) = 1.0;                      // 4 - Collector field section outlet headers (i.e., runners) to expansion vessel (indirect storage) or
                                                    //     hot thermal storage tank (direct storage)
        v_dot_rel.at(4) = 1.0;                      // 5 - Bypass branch - Collector field section outlet headers (i.e., runners) to pump suction header (indirect) or
                                                    //     cold thermal storage tank (direct)

        //The volumetric flow rate relative to the power block for each generation section
        v_dot_rel.at(5) = 1.0 / 2;                  // 2 - SGS pump suction header to individual SGS pump inlet (applicable only for storage in series with SF)
                                                    //     50% -> "/2.0" . The flow rate (i.e., diameter) is sized here for the case when one pump is down.
        v_dot_rel.at(6) = 1.0 / 2;                  // 3 - Individual SGS pump discharge to SGS pump discharge header (only for series storage)
        v_dot_rel.at(7) = 1.0;                      // 4 - SGS pump discharge header to steam generator supply header (only for series storage)

        v_dot_rel.at(8) = 1.0;                      // 5 - Steam generator supply header to inter-steam generator piping
        v_dot_rel.at(9) = 1.0;                      // 6 - Inter-steam generator piping to steam generator outlet header
        v_dot_rel.at(10) = 1.0;                     // 7 - Steam generator outlet header to SF pump suction header (indirect) or cold thermal storage tank (direct)

        if (tanks_in_parallel) {
            sections_no_bypass = { 0, 1, 2, 3, 8, 9, 10 };
        }
        else {  // tanks in series
            sections_no_bypass = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 10 };
        }

        // Collection loop followed by generation loop
        for (std::size_t i = 0; i < nPipes; i++) {
            if (L.at(i) > 0) {
                i < gen_first_index ? v_dot_ref = v_dot_sf : v_dot_ref = v_dot_pb;
                v_dot = v_dot_ref * v_dot_rel.at(i);
                if (!custom_sizes) {
                    diams.at(i) = CSP::pipe_sched(sqrt(4.0*v_dot / (vel_dsn * CSP::pi)));
                    wall_thk.at(i) = CSP::WallThickness(diams.at(i));
                }
                m_dot.at(i) = v_dot * rho_avg;
                Area = CSP::pi * pow(diams.at(i), 2) / 4.;
                vel.at(i) = v_dot / Area;

                // Calculate total volume, excluding bypass branch
                if (std::find(sections_no_bypass.begin(), sections_no_bypass.end(), i) != sections_no_bypass.end()) {
                    vol_tot += Area * L.at(i);
                }
            }
        }

        return 0;
    }

    int size_sgs_piping_TandP(double T_field_in, double T_field_out, double P_field_in, double DP_SGS,
        const util::matrix_t<double> &L, const util::matrix_t<double> &k_tes_loss_coeffs, double pipe_rough,
        bool tanks_in_parallel, const util::matrix_t<double> &diams, const util::matrix_t<double> &vel,
        util::matrix_t<double> &SGS_T_des, util::matrix_t<double> &SGS_P_des)
    {
        std::size_t nPipes = L.ncells();
        SGS_T_des.resize_fill(nPipes, 0.0);
        SGS_P_des.resize_fill(nPipes, 0.0);

        // Calculate Design Temperatures, in C
        SGS_T_des.at(0) = T_field_in - 273.15;
        SGS_T_des.at(1) = T_field_in - 273.15;
        SGS_T_des.at(2) = T_field_in - 273.15;
        SGS_T_des.at(3) = T_field_out - 273.15;
        SGS_T_des.at(4) = T_field_out - 273.15;
        if (tanks_in_parallel) {
            SGS_T_des.at(5) = -1;
            SGS_T_des.at(6) = -1;
            SGS_T_des.at(7) = -1;
        }
        else {
            SGS_T_des.at(5) = T_field_out - 273.15;
            SGS_T_des.at(6) = T_field_out - 273.15;
            SGS_T_des.at(7) = T_field_out - 273.15;
        }
        SGS_T_des.at(8) = T_field_out - 273.15;
        SGS_T_des.at(9) = T_field_in - 273.15;
        SGS_T_des.at(10) = T_field_in - 273.15;


        // Calculate Design Pressures, in Pa
        double P, ff;
        double rho_avg = field_htfProps.dens((T_field_in + T_field_out) / 2, 9 / 1.e-5);
        const double P_hi = 17 / 1.e-5;               // downstream SF pump pressure [Pa]
        const double P_lo = 1 / 1.e-5;                // atmospheric pressure [Pa]

        // P_10
        ff = CSP::FrictionFactor(pipe_rough / diams.at(10), field_htfProps.Re(SGS_T_des.at(10), P_lo, vel.at(10), diams.at(10)));
        SGS_P_des.at(10) = 0 +
            CSP::MajorPressureDrop(vel.at(10), rho_avg, ff, L.at(10), diams.at(10)) +
            CSP::MinorPressureDrop(vel.at(10), rho_avg, k_tes_loss_coeffs.at(10));

        // P_9
        ff = CSP::FrictionFactor(pipe_rough / diams.at(9), field_htfProps.Re(SGS_T_des.at(9), P_lo, vel.at(9), diams.at(9)));
        SGS_P_des.at(9) = SGS_P_des.at(10) +
            CSP::MajorPressureDrop(vel.at(9), rho_avg, ff, L.at(9), diams.at(9)) +
            CSP::MinorPressureDrop(vel.at(9), rho_avg, k_tes_loss_coeffs.at(9));
        
        // P_8
        ff = CSP::FrictionFactor(pipe_rough / diams.at(8), field_htfProps.Re(SGS_T_des.at(8), P_hi, vel.at(8), diams.at(8)));
        SGS_P_des.at(8) = SGS_P_des.at(9) + DP_SGS + 
            CSP::MajorPressureDrop(vel.at(8), rho_avg, ff, L.at(8), diams.at(8)) +
            CSP::MinorPressureDrop(vel.at(8), rho_avg, k_tes_loss_coeffs.at(8));

        if (tanks_in_parallel) {
            SGS_P_des.at(7) = 0;
            SGS_P_des.at(6) = 0;
            SGS_P_des.at(5) = 0;
        }
        else {
            // P_7
            ff = CSP::FrictionFactor(pipe_rough / diams.at(7), field_htfProps.Re(SGS_T_des.at(7), P_hi, vel.at(7), diams.at(7)));
            SGS_P_des.at(7) = SGS_P_des.at(8) +
                CSP::MajorPressureDrop(vel.at(7), rho_avg, ff, L.at(7), diams.at(7)) +
                CSP::MinorPressureDrop(vel.at(7), rho_avg, k_tes_loss_coeffs.at(7));

            // P_6
            ff = CSP::FrictionFactor(pipe_rough / diams.at(6), field_htfProps.Re(SGS_T_des.at(6), P_hi, vel.at(6), diams.at(6)));
            SGS_P_des.at(6) = SGS_P_des.at(7) +
                CSP::MajorPressureDrop(vel.at(6), rho_avg, ff, L.at(6), diams.at(6)) +
                CSP::MinorPressureDrop(vel.at(6), rho_avg, k_tes_loss_coeffs.at(6));

            // P_5
            SGS_P_des.at(5) = 0;
        }

        // P_3
        ff = CSP::FrictionFactor(pipe_rough / diams.at(3), field_htfProps.Re(SGS_T_des.at(3), P_lo, vel.at(3), diams.at(3)));
        SGS_P_des.at(3) = 0 +
            CSP::MajorPressureDrop(vel.at(3), rho_avg, ff, L.at(3), diams.at(3)) +
            CSP::MinorPressureDrop(vel.at(3), rho_avg, k_tes_loss_coeffs.at(3));

        // P_4
        SGS_P_des.at(4) = SGS_P_des.at(3);

        // P_2
        ff = CSP::FrictionFactor(pipe_rough / diams.at(2), field_htfProps.Re(SGS_T_des.at(2), P_hi, vel.at(2), diams.at(2)));
        SGS_P_des.at(2) = P_field_in +
            CSP::MajorPressureDrop(vel.at(2), rho_avg, ff, L.at(2), diams.at(2)) +
            CSP::MinorPressureDrop(vel.at(2), rho_avg, k_tes_loss_coeffs.at(2));

        // P_1
        ff = CSP::FrictionFactor(pipe_rough / diams.at(1), field_htfProps.Re(SGS_T_des.at(1), P_hi, vel.at(1), diams.at(1)));
        SGS_P_des.at(1) = SGS_P_des.at(2) +
            CSP::MajorPressureDrop(vel.at(1), rho_avg, ff, L.at(1), diams.at(1)) +
            CSP::MinorPressureDrop(vel.at(1), rho_avg, k_tes_loss_coeffs.at(1));

        // P_0
        SGS_P_des.at(0) = 0;

        // Convert Pa to bar
        for (int i = 0; i < nPipes; i++) {
            SGS_P_des.at(i) = SGS_P_des.at(i) / 1.e5;
        }

        return 0;
    }

    int sgs_pressure_drops(double m_dot_sf, double m_dot_pb, util::matrix_t<double> v_dot_rel,
        double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out,
        util::matrix_t<double> L, util::matrix_t<double> D, double pipe_rough,
        double DP_SGS_nom, util::matrix_t<double> k_coeffs, bool tanks_in_parallel, bool recirculating,
        double &P_drop_col, double &P_drop_gen)
    {
        const std::size_t num_sections = 11;          // total number of col. + gen. sections
        const std::size_t bypass_section = 4;         // bypass section index
        const std::size_t gen_first_section = 5;      // first generation section index in combined col. gen. loops
        const double P_hi = 17 / 1.e-5;               // downstream SF pump pressure [Pa]
        const double P_lo = 1 / 1.e-5;                // atmospheric pressure [Pa]
        double P, T, rho, v_dot, vel;                 // htf properties
        double Area;                                  // cross-sectional pipe area
        double v_dot_sf, v_dot_pb;                    // solar field and power block vol. flow rates
        double k;                                     // effective minor loss coefficient
        double Re, ff;
        double v_dot_ref;
        double DP_SGS;
        std::vector<double> P_drops(num_sections, 0.0);

        m_dot_pb > 0 ? DP_SGS = DP_SGS_nom : DP_SGS = 0.;
        v_dot_sf = m_dot_sf / field_htfProps.dens((T_sf_in + T_sf_out) / 2, (P_hi + P_lo) / 2);
        v_dot_pb = m_dot_pb / field_htfProps.dens((T_pb_in + T_pb_out) / 2, P_lo);

        for (std::size_t i = 0; i < num_sections; i++) {
            if (L.at(i) > 0 && D.at(i) > 0) {
                (i > 0 && i < 3) ? P = P_hi : P = P_lo;
                if (i < 3) T = T_sf_in;                                                       // 0, 1, 2
                if (i == 3 || i == 4) T = T_sf_out;                                           // 3, 4
                if (i >= gen_first_section && i < gen_first_section + 4) T = T_pb_in;         // 5, 6, 7, 8
                if (i == gen_first_section + 4) T = (T_pb_in + T_pb_out) / 2.;                // 9
                if (i == gen_first_section + 5) T = T_pb_out;                                 // 10
                i < gen_first_section ? v_dot_ref = v_dot_sf : v_dot_ref = v_dot_pb;
                v_dot = v_dot_rel.at(i) * v_dot_ref;
                Area = CSP::pi * pow(D.at(i), 2) / 4.;
                vel = v_dot / Area;
                rho = field_htfProps.dens(T, P);
                Re = field_htfProps.Re(T, P, vel, D.at(i));
                ff = CSP::FrictionFactor(pipe_rough/D.at(i), Re);
                if (i != bypass_section || recirculating) {
                    P_drops.at(i) += CSP::MajorPressureDrop(vel, rho, ff, L.at(i), D.at(i));
                    P_drops.at(i) += CSP::MinorPressureDrop(vel, rho, k_coeffs.at(i));
                }
            }
        }

        P_drop_col = std::accumulate(P_drops.begin(), P_drops.begin() + gen_first_section, 0.0);
        P_drop_gen = DP_SGS + std::accumulate(P_drops.begin() + gen_first_section, P_drops.end(), 0.0);

        return 0;
    }
};

TCS_IMPLEMENT_TYPE( sam_mw_trough_type251, "Indirect HTF Plant Controller", "Ty Neises", 1, sam_mw_trough_type251_variables, NULL, 1 )


	
