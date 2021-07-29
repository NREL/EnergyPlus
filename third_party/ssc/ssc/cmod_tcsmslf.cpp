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
#include "tckernel.h"
#include "common.h"
#include <algorithm>

static var_info _cm_vtab_tcsmslf[] = {
//    VARTYPE           DATATYPE          NAME                 LABEL                                                                                 UNITS            META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                                             "",              "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                                       "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                                          "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                                       "",              "",            "Weather",        "*",                       "",                      "" },
	{ SSC_INPUT, SSC_NUMBER, "system_capacity", "Nameplate capacity", "kW", "", "mslf", "*", "", "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",  "12x24 Time of Use Values for week days",                                              "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",  "12x24 Time of Use Values for week end days",                                          "",             "",             "tou_translator", "*",                       "",                      "" }, 

	// Type 262 (solar field collector) parameters
    { SSC_INPUT,    SSC_NUMBER,         "nMod",                   "Number of collector modules in a loop",                                                 "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "nRecVar",                "Number of receiver variantions",                                                        "",              "",  "controller",            "?=4",      "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "nLoops",                 "Number of loops in the field",                                                          "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "eta_pump",               "HTF pump efficiency",                                                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "HDR_rough",              "Header pipe roughness",                                                                 "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "theta_stow",             "stow angle",                                                                            "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "theta_dep",              "deploy angle",                                                                          "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "FieldConfig",            "Number of subfield headers",                                                            "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_startup",              "Power block startup temperature",                                                       "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "pb_rated_cap",           "Rated plant capacity",                                                                  "MWe",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "m_dot_htfmin",           "Minimum loop HTF flow rate",                                                            "kg/s",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "m_dot_htfmax",           "Maximum loop HTF flow rate",                                                            "kg/s",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_loop_in_des",          "Design loop inlet temperature",                                                         "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_loop_out",             "Target loop outlet temperature",                                                        "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Fluid",                  "Field HTF fluid number",                                                                "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_field_ini",            "Initial field temperature",                                                             "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "field_fl_props",         "Fluid property data",                                                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_fp",                   "Freeze protection temperature (heat trace activation temperature)",                     "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "I_bn_des",               "Solar irradiation at design",                                                           "W/m2",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_hdr_max",              "Maximum HTF velocity in the header at design",                                          "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_hdr_min",              "Minimum HTF velocity in the header at design",                                          "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Pipe_hl_coef",           "Loss coefficient from the header - runner pipe - and non-HCE piping",                   "W/m2-K",        "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "SCA_drives_elec",        "Tracking power in Watts per SCA drive",                                                 "W/module",      "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "fthrok",                 "Flag to allow partial defocusing of the collectors",                                    "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "fthrctrl",               "Defocusing strategy",                                                                   "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "ColAz",                  "Collector azimuth angle",                                                               "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "solar_mult",             "Solar multiple",                                                                        "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_hot",             "The heat capacity of the balance of plant on the hot side",                             "kWht/K-MWt",    "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_cold",            "The heat capacity of the balance of plant on the cold side",                            "kWht/K-MWt",    "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_sca",             "Non-HTF heat capacity associated with each SCA - per meter basis",                      "Wht/K-m",       "",  "controller",            "*",        "",              ""},

	{ SSC_INPUT,    SSC_NUMBER,         "water_per_wash",         "Water usage per wash",                                                                  "L/m2_aper",     "",  "solar_field",    "*",                       "",                      "" },
	{ SSC_INPUT,    SSC_NUMBER,         "washes_per_year",        "Mirror washing frequency",                                                              "none",          "",  "solar_field",    "*",                       "",                      "" },	

    { SSC_INPUT,    SSC_NUMBER,         "opt_model",              "The optical model",                                                                     "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "A_aperture",             "Reflective aperture area of the collector",                                             "m2",            "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "reflectivity",           "Solar-weighted mirror reflectivity value",                                              "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "TrackingError",          "Tracking error derate",                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "GeomEffects",            "Geometry effects derate",                                                               "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Dirt_mirror",            "User-defined dirt on mirror derate",                                                    "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Error",                  "User-defined general optical error derate",                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_mod",                  "The length of the collector module",                                                    "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "IAM_T_coefs",            "Incidence angle modifier coefficients - transversal plane",                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "IAM_L_coefs",            "Incidence angle modifier coefficients - longitudinal plane",                            "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "OpticalTable",           "Values of the optical efficiency table",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "rec_model",              "Receiver model type (1=Polynomial ; 2=Evac tube)",                                      "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_ARRAY,          "HCE_FieldFrac",          "The fraction of the field occupied by this HCE type",                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_abs_in",               "The inner absorber tube diameter",                                                      "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_abs_out",              "The outer absorber tube diameter",                                                      "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_glass_in",             "The inner glass envelope diameter",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_glass_out",            "The outer glass envelope diameter",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_plug",                 "The diameter of the absorber flow plug (optional)",                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Flow_type",              "The flow type through the absorber",                                                    "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Rough",                  "Roughness of the internal surface",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "alpha_env",              "Envelope absorptance",                                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_1",          "Absorber emittance - HCE variation 1",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_2",          "Absorber emittance - HCE variation 2",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_3",          "Absorber emittance - HCE variation 3",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_4",          "Absorber emittance - HCE variation 4",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "alpha_abs",              "Absorber absorptance",                                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Tau_envelope",           "Envelope transmittance",                                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "epsilon_glass",          "Glass envelope emissivity",                                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "GlazingIntactIn",        "The glazing intact flag",                                                               "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "P_a",                    "Annulus gas pressure",                                                                  "torr",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "AnnulusGas",             "Annulus gas type (1=air; 26=Ar; 27=H2)",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "AbsorberMaterial",       "Absorber material type",                                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Shadowing",              "Receiver bellows shadowing loss factor",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "dirt_env",               "Loss due to dirt on the receiver envelope",                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Design_loss",            "Receiver heat loss at design",                                                          "W/m",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_mod_spacing",          "Piping distance between sequential modules in a loop",                                  "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_crossover",            "Length of crossover piping in a loop",                                                  "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "HL_T_coefs",             "HTF temperature-dependent heat loss coefficients",                                      "W/m-K",         "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "HL_w_coefs",             "Wind-speed-dependent heat loss coefficients",                                           "W/m-(m/s)",     "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "DP_nominal",             "Pressure drop across a single collector assembly at design",                            "bar",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "DP_coefs",               "Pressure drop mass flow based part-load curve",                                         "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "rec_htf_vol",            "Volume of HTF in a single collector unit per unit aperture area",                       "L/m2-ap",       "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_amb_sf_des",           "Ambient design-point temperature for the solar field",                                  "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_wind_des",             "Design-point wind velocity",                                                            "m/s",           "",  "controller",            "*",        "",              ""},

    // Type 262 inputs
    { SSC_INPUT,    SSC_NUMBER,         "I_b",                    "Direct normal incident solar irradiation",                                              "kJ/m2-hr",      "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_db",                   "Dry bulb air temperature",                                                              "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_wind",                 "Ambient windspeed",                                                                     "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "P_amb",                  "Ambient pressure",                                                                      "atm",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_dp",                   "The dewpoint temperature",                                                              "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_cold_in",              "HTF return temperature",                                                                "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "defocus",                "Defocus control",                                                                       "",              "",  "controller",            "*",        "",              ""},

    //   controller (type 251) inputs
    //VARTYPE      DATATYPE           NAME                      LABEL                                                                                    UNITS           META  GROUP                   REQUIRED_IF  CONSTRAINTS      UI_HINTS
    { SSC_INPUT,    SSC_NUMBER,         "field_fluid",            "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "store_fl_props",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
	{ SSC_INPUT,    SSC_NUMBER,         "store_fluid",            "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tshours",                "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "is_hx",                  "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "dt_hot",                 "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "dt_cold",                "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "hx_config",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "q_max_aux",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_set_aux",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_tank_hot_ini",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_tank_hot_ini",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_tank_cold_ini",        "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "vol_tank",               "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "h_tank",                 "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "h_tank_min",             "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "u_tank",                 "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tank_pairs",             "Label",                                                                                 "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "cold_tank_Thtr",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "hot_tank_Thtr",          "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tank_max_heat",          "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_field_in_des",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_field_out_des",        "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "q_pb_design",            "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "W_pb_design",            "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "cycle_max_frac",         "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "cycle_cutoff_frac",      "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "solarm",                 "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "pb_pump_coef",           "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tes_pump_coef",          "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "pb_fixed_par",           "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "bop_array",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "aux_array",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tes_temp",               "Label",                                                                                 "",              "",  "controller",           "*",         "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "fossil_mode",            "Label",                                                                                 "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "fthr_ok",                "Label",                                                                                 "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "nSCA",                   "Label",                                                                                 "",              "",  "controller",           "*",         "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "fc_on",                  "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "t_standby_reset",        "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tes_type",               "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "tslogic_a",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "tslogic_b",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "tslogic_c",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "ffrac",                  "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tc_fill",                "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tc_void",                "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "t_dis_out_min",          "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "t_ch_out_max",           "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "nodes",                  "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "f_tc_cold",              "Label",                                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_tes_des",              "Design-point velocity to size the TES pipe diameters",                               "m/s",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "custom_tes_p_loss",      "TES pipe losses are based on custom lengths and coeffs",                               "-",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "k_tes_loss_coeffs",      "Minor loss coeffs for the coll, gen, and bypass loops",                                "-",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "custom_sgs_pipe_sizes",  "Use custom SGS pipe diams, wallthks, and lengths",                                     "-",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "sgs_diams",              "Custom SGS diameters",                                                                 "m",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "sgs_wallthicks",         "Custom SGS wall thicknesses",                                                          "m",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "sgs_lengths",            "Custom SGS lengths",                                                                   "m",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "DP_SGS",                 "Pressure drop within the steam generator",                                           "bar",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "tanks_in_parallel",      "Tanks are in parallel, not in series, with solar field",                               "-",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "has_hot_tank_bypass",    "Bypass valve connects field outlet to cold tank",                                      "-",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_tank_hot_inlet_min",   "Minimum hot tank htf inlet temperature",                                               "C",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "calc_design_pipe_vals",  "Calculate temps and pressures at design conditions for runners and headers",           "-",              "",  "controller",            "*",        "",              ""},


    //VARTYPE      DATATYPE           NAME                      LABEL                                                                          UNITS           META     GROUP                   REQUIRED_IF  CONSTRAINTS      UI_HINTS
		// Power Cycle Inputs
	{ SSC_INPUT,    SSC_NUMBER,         "pc_config",              "0: Steam Rankine (224), 1: user defined",                                   "-",              "",  "powerblock",         "?=0",      "INTEGER",       ""},            	
	{ SSC_INPUT,    SSC_NUMBER,         "P_ref",                  "Label",                                                                     "-",              "",  "powerblock",         "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "eta_ref",                "Cycle thermal efficiency at design point",                                  "-",              "",  "powerblock",         "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "startup_time",           "Time needed for power block startup",                                       "hr",             "",  "powerblock",         "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "startup_frac",           "Fraction of design thermal power needed for startup",                       "none",           "",  "powerblock",         "*",        "",              ""},    
	{ SSC_INPUT,    SSC_NUMBER,         "q_sby_frac",             "Fraction of thermal power required for standby mode",                       "none",           "",  "powerblock",         "*",        "",              ""},
    																																		
		// Steam Rankine Cycle																												
    { SSC_INPUT,    SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                     "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                             "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "P_boil",            "Boiler operating pressure",                                                 "bar",          "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                          "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                        "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                              "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                  "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                      "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                "inHg",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",              "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                     "none",         "constant=[0,0,0,0,0,0,0,0,0]", "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,      "tech_type",         "Turbine inlet pressure control flag (sliding=user, fixed=trough)",          "1/2/3",         "tower/trough/user",           "powerblock",     "pc_config=0",             "",                      "" },
		
	
		// User Defined cycle
	{ SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",  "Percent of user-defined power cycle design gross output consumed by cooling",    "%",	    "",                        "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des", "Mass flow rate of water required at user-defined power cycle design point",   "kg/s",  "",                        "user_defined_PC", "pc_config=1",            "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "ud_ind_od",            "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb", "", "", "user_defined_PC", "pc_config=1",     "",                      "" },


		//  enet calculator																															
    { SSC_INPUT,    SSC_NUMBER,         "eta_lhv",                "Label",                                                                     "-",              "",  "enet",               "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "eta_tes_htr",            "Label",                                                                     "-",              "",  "enet",               "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "fp_mode",                "Label",                                                                     "-",              "",  "enet",               "*",        "",              ""},





																															
																																				
		// System Temperatures...																																	
	{ SSC_INPUT,    SSC_NUMBER,         "T_htf_hot_ref",          "Label",                                                                     "-",              "",  "powerblock",         "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_htf_cold_ref",         "Label",                                                                     "-",              "",  "powerblock",         "*",        "",              ""},
    
	
	
    // OUTPUTS
    // The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

    //weather file reader
    { SSC_OUTPUT,       SSC_ARRAY,       "month",             "Resource Month",                                                  "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Resource Hour of Day",                                            "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Resource Solar Azimuth",                                          "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Resource Solar Zenith",                                           "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Resource Beam normal irradiance",                                 "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Resource Dry bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Resource Wind Speed",                                             "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Resource Wet bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Resource Pressure",                                               "mbar",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tou_value",         "Resource Time-of-use value",                                      "",             "",            "tou",            "*",                       "LENGTH=8760",           "" },

    //VARTYPE      DATATYPE           NAME                      LABEL                                                                                    UNITS           META  GROUP                   REQUIRED_IF  CONSTRAINTS      UI_HINTS
    // Type 262 outputs

    //solar field
    { SSC_OUTPUT,   SSC_ARRAY,          "theta_L",                "Field collector incidence angle - longitudinal",                 "deg",           "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "phi_t",                  "Field collector incidence angle - transversal",                  "deg",           "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "eta_optical",            "Field collector optical efficiency",                             "",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "EqOptEff",               "Field collector and receiver optical efficiency",                "",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "sf_def",                 "Field collector focus fraction",                                 "",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    
    { SSC_OUTPUT,   SSC_ARRAY,          "q_inc_sf_tot",           "Field thermal power incident",                                   "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "q_abs_tot",              "Field thermal power absorbed",                                   "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "q_dump",                 "Field thermal power dumped",                                     "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "q_loss_tot",             "Field thermal power receiver loss",                              "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "Pipe_hl",                "Field thermal power header pipe losses",                         "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,   SSC_ARRAY,          "q_avail",                "Field thermal power produced",                                   "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "q_loss_spec_tot",        "Field thermal power avg. receiver loss",                         "W/m",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    //{ SSC_OUTPUT,   SSC_ARRAY,          "q_field_delivered",      "Total solar field thermal power delivered",                                             "MWt",           "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "eta_thermal",            "Field thermal efficiency",                                        "",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    
    { SSC_OUTPUT,   SSC_ARRAY,          "E_bal_startup",          "Field HTF energy inertial (consumed)",                           "MWht",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
  /*{ SSC_OUTPUT,   SSC_ARRAY,          "E_loop_accum",           "Accumulated internal energy change rate in the loops ONLY",                             "MWht",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "E_hdr_accum",            "Accumulated internal energy change rate in the headers/SGS",                            "MWht",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "E_tot_accum",            "Total accumulated internal energy change rate",                                         "MWht",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "E_field",                "Accumulated internal energy in the entire solar field",                                 "MWht",          "",  "mslf",                  "*",        "LENGTH=8760",   ""},*/
    //{ SSC_OUTPUT,   SSC_ARRAY,          "m_dot_htf_tot",          "The actual flow rate through the field..",                                              "kg/hr",         "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,   SSC_ARRAY,          "m_dot_avail",            "Field HTF mass flow rate total",                                 "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "m_dot_htf2",             "Field HTF mass flow rate loop",                                  "kg/s",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "DP_tot",                 "Field HTF pressure drop total",                                  "bar",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
  	{ SSC_OUTPUT,   SSC_ARRAY,          "T_sys_c",                "Field HTF temperature cold header inlet",                        "C",            "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,   SSC_ARRAY,          "T_sys_h",                "Field HTF temperature hot header outlet",                        "C",            "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "T_field_in",          "Field HTF temperature collector inlet",                          "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,   SSC_ARRAY,          "t_loop_outlet",          "Field HTF temperature loop outlet",                              "C",             "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    
    //{ SSC_OUTPUT,   SSC_ARRAY,          "c_htf_ave",              "Average solar field specific heat",                                                     "J/kg-K",        "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    
    //thermal storage
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_cold",      "TES HTF mass in cold tank",                                      "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_hot",       "TES HTF mass in hot tank",                                       "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_charge_field",  "TES HTF mass flow rate - field side of HX",                      "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,     "m_dot_discharge_tank",  "TES HTF mass flow rate - storage side of HX",                    "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_fin",     "TES HTF temperature in cold tank",                               "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_fin",      "TES HTF temperature in hot tank",                                "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_hot",              "TES HTF temperature HX field side hot",                          "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_cold",             "TES HTF temperature HX field side cold",                         "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_in",       "TES HTF temperature hot tank inlet",                             "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_in",      "TES HTF temperature cold tank inlet",                            "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_cold_fin",   "TES HTF volume in cold tank",                                    "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_hot_fin",    "TES HTF volume in hot tank",                                     "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_total",      "TES HTF volume total",                                           "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_tes",            "TES thermal energy into storage",                                "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",         "TES thermal losses from tank(s)",                                "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    
    //power block
    //{ SSC_OUTPUT,   SSC_ARRAY,          "P_cycle",                "Gross electricity generation (or usage) by the plant",                                  "MW",            "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    //{ SSC_OUTPUT,   SSC_ARRAY,          "W_net",                  "Net electricity generation (or usage) by the plant",                                    "MW",            "",  "mslf",                  "*",        "LENGTH=8760",   ""},
    { SSC_OUTPUT,       SSC_ARRAY,       "eta",               "Cycle efficiency (gross)",                                       "",         "",            "Type224",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_net",             "Cycle electrical power output (net)",                            "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",           "Cycle electrical power output (gross)",                          "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pb",          "Cycle HTF mass flow rate",                                       "kg/hr",        "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_in",           "Cycle HTF temperature in (hot)",                                 "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_out",          "Cycle HTF temperature out (cold)",                               "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_makeup",      "Cycle cooling water mass flow rate - makeup",                    "kg/hr",        "",            "Type224",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pb",              "Cycle thermal power input",                                      "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "P_cond",            "Condenser pressure",                                             "Pa",           "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "f_bays",            "Condenser fraction of operating bays",                           "",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_diams",    "Pipe diameters in SGS",                                          "m",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_wallthk",  "Pipe wall thickness in SGS",                                     "m",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_mdot_dsn", "Mass flow SGS pipes at design conditions",                       "kg/s",         "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_vel_dsn",  "Velocity in SGS pipes at design conditions",                     "m/s",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_T_dsn",    "Temperature in SGS pipes at design conditions",                  "C",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_P_dsn",    "Pressure in SGS pipes at design conditions",                     "bar",          "",            "Type251",        "*",                       "",                      "" },

    //fossil backup
    //{ SSC_OUTPUT,       SSC_ARRAY,       "Fuel_usage",        "Fossil fuel usage (all subsystems)",                             "MMBTU",        "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_aux_fuel",        "Fossil fuel usage (all subsystems)",                             "MMBTU",         "",           "Outputs",               "*",        "LENGTH=8760",   ""},

    //parasitics
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pump",        "Parasitic power solar field HTF pump",                           "MWe",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",    "Parasitic power TES and Cycle HTF pump",                         "MWe",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "track_par_tot",     "Parasitic power field collector drives",                         "MWe",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_par_BOP",         "Parasitic power generation-dependent load",                      "MWe",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "fixed_par",         "Parasitic power fixed load",                                     "MWe",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_par_aux_boiler",  "Parasitic power auxiliary heater operation",                     "MWe",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_cool_par",        "Parasitic power condenser operation",                            "MWe",          "",            "Type224",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_sf_fp",       "Parasitic thermal field freeze protection",                      "MWt",          "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_tes_fp",      "Parasitic thermal TES freeze protection",                        "MWt",          "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    
    // sum calcs

//    { SSC_OUTPUT,   SSC_ARRAY,          "hourly_energy",      "Hourly Energy",                                                  "kWh",          "",            "Calculated",     "*",					    "LENGTH=8760",           ""},

    // monthly values
    { SSC_OUTPUT,   SSC_ARRAY,          "monthly_energy",         "Monthly Energy",                                             "kWh",           "",            "mslf",                  "*",        "LENGTH=12",     ""},
																																					            
    // single values																																            
    { SSC_OUTPUT,   SSC_NUMBER,         "annual_energy",          "Annual Energy",                                              "kWh",           "",            "mslf",                  "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "annual_W_cycle_gross",   "Electrical source - Power cycle gross output",               "kWh",           "",            "mslf",                  "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "conversion_factor",      "Gross to Net Conversion Factor",                             "%",             "",            "Calculated",            "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "capacity_factor",        "Capacity factor",                                            "%",              "",           "",                      "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "kwh_per_kw",             "First year kWh/kW",                                          "kWh/kW",                       "",  "",                      "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "system_heat_rate",       "System heat rate",                                           "MMBtu/MWh",     "",            "",                      "*",        "",              ""},
    { SSC_OUTPUT,   SSC_NUMBER,         "annual_fuel_usage",      "Annual fuel usage",                                          "kWh",           "",            "",                      "*",        "",              ""},
	{ SSC_OUTPUT,   SSC_NUMBER,         "annual_total_water_use", "Total Annual Water Usage: cycle + mirror washing",           "m3",            "",            "PostProcess",    "*",                       "",                      "" },


    var_info_invalid };

class cm_tcsmslf : public tcKernel
{
public:

	cm_tcsmslf(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsmslf );
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);

		// debugging
		//set_store_all_parameters(true);
		//set_store_array_matrix_data(true);
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( )
	{
		bool debug_mode = false; //(__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcs/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
			set_unit_value( weather, "i_hour", "TIME" );
			set_unit_value( weather, "i_month", "month" );
			set_unit_value( weather, "i_day", "day" );
			set_unit_value( weather, "i_global", "GlobalHorizontal" );
			set_unit_value( weather, "i_beam", "DNI" );
			set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
			set_unit_value( weather, "i_tdry", "T_dry" );
			set_unit_value( weather, "i_twet", "T_wet" );
			set_unit_value( weather, "i_tdew", "T_dew" );
			set_unit_value( weather, "i_wspd", "WindSpeed" );
			set_unit_value( weather, "i_wdir", "WindDir" );
			set_unit_value( weather, "i_rhum", "RelHum" );
			set_unit_value( weather, "i_pres", "AtmPres" );
			set_unit_value( weather, "i_snow", "SnowCover" );
			set_unit_value( weather, "i_albedo", "GroundAlbedo" );
			set_unit_value( weather, "i_poa", "POA" );
			set_unit_value( weather, "i_solazi", "Azimuth" );
			set_unit_value( weather, "i_solzen", "Zenith" );
			set_unit_value( weather, "i_lat", "Latitude" );
			set_unit_value( weather, "i_lon", "Longitude" );
			set_unit_value( weather, "i_shift", "Shift" );
		}
		else
		{
			//Set weather parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );
			set_unit_value_ssc_double( weather, "tilt" );
			set_unit_value_ssc_double( weather, "azimuth" );
		}
		
		// Add tou translator
		int	tou_translator = add_unit("tou_translator", "Time of Use Translator");
		//Add the solar field collector unit
		int solarfield = add_unit("sam_mw_lf_type262", "type 262 solarfield");
		//Add controller unit
		int controller = add_unit("sam_mw_trough_type251", "type 251 controller");
		//Add direct powerblock unit
		int powerblock = add_unit("sam_mw_pt_type224", "type 224 powerblock");
		//Add unit to that summarizes energy output
		int enet = add_unit("sam_mw_csp_SumCalcs","type csp_SumCalcs enet calculator");

		// Now set solar field collector unit parameters
		set_unit_value_ssc_double(solarfield, "nMod" ); // nMod);
		set_unit_value_ssc_double(solarfield, "nRecVar" ); // 4);
		set_unit_value_ssc_double(solarfield, "nLoops" ); // nLoops);
		set_unit_value_ssc_double(solarfield, "eta_pump" ); // 0.85);
		set_unit_value_ssc_double(solarfield, "HDR_rough" ); // 4.57E-05);
		set_unit_value_ssc_double(solarfield, "theta_stow" ); // 170);
		set_unit_value_ssc_double(solarfield, "theta_dep" ); // 10);
		set_unit_value_ssc_double(solarfield, "FieldConfig" ); // 2);
		set_unit_value_ssc_double(solarfield, "T_startup" ); // T_startup);
		set_unit_value_ssc_double(solarfield, "pb_rated_cap" ); // E_gross);
		set_unit_value_ssc_double(solarfield, "m_dot_htfmin" ); // m_dot_min);
		set_unit_value_ssc_double(solarfield, "m_dot_htfmax" ); // m_dot_max);
		set_unit_value_ssc_double(solarfield, "T_loop_in_des" ); // T_cold_des);
		set_unit_value_ssc_double(solarfield, "T_loop_out" ); // T_hot_des);
		set_unit_value_ssc_double(solarfield, "Fluid" ); // field_fluid);
		set_unit_value_ssc_double(solarfield, "T_field_ini" ); // T_startup);
		set_unit_value_ssc_matrix(solarfield, "field_fl_props");	// User-defined HTF data
		set_unit_value_ssc_double(solarfield, "T_fp" ); // T_cold_fp);
		set_unit_value_ssc_double(solarfield, "I_bn_des" ); // I_bn_des);
		set_unit_value_ssc_double(solarfield, "V_hdr_max" ); // 3);
		set_unit_value_ssc_double(solarfield, "V_hdr_min" ); // 2);
		set_unit_value_ssc_double(solarfield, "Pipe_hl_coef" ); // 0.45);
		set_unit_value_ssc_double(solarfield, "SCA_drives_elec" ); // 125);
		set_unit_value_ssc_double(solarfield, "fthrok" ); // 1);
		set_unit_value_ssc_double(solarfield, "fthrctrl" ); // 2);
		set_unit_value_ssc_double(solarfield, "ColAz" ); // ColAz);
		set_unit_value_ssc_double(solarfield, "solar_mult" ); // SM);
		set_unit_value_ssc_double(solarfield, "mc_bal_hot" ); // 0.2);
		set_unit_value_ssc_double(solarfield, "mc_bal_cold" ); // 0.2);
		set_unit_value_ssc_double(solarfield, "mc_bal_sca" ); // 4.5);

		set_unit_value_ssc_double(solarfield, "opt_model" ); // opt_model);
		set_unit_value_ssc_double(solarfield, "A_aperture" ); // A_aperture);
		set_unit_value_ssc_double(solarfield, "reflectivity" ); // reflectivity);
		set_unit_value_ssc_double(solarfield, "TrackingError" ); // TrackingError);
		set_unit_value_ssc_double(solarfield, "GeomEffects" ); // GeomEffects);
		set_unit_value_ssc_double(solarfield, "Dirt_mirror" ); // Dirt_mirror);
		set_unit_value_ssc_double(solarfield, "Error" ); // Error);
		set_unit_value_ssc_double(solarfield, "L_mod" ); // L_mod);
		set_unit_value_ssc_array(solarfield, "IAM_T_coefs" ); // [0.9896, 0.044, -0.0721, -0.2327, 0.]);
		set_unit_value_ssc_array(solarfield, "IAM_L_coefs" ); // [1.0031, -0.2259, 0.5368, -1.6434, 0.7222]);
		set_unit_value_ssc_matrix(solarfield, "OpticalTable");

		set_unit_value_ssc_double(solarfield, "rec_model" ); // rec_model);
		set_unit_value_ssc_array(solarfield, "HCE_FieldFrac" ); // HCE_FieldFrac);
		set_unit_value_ssc_array(solarfield, "D_abs_in" ); // [0.066, 0.066, 0.066, 0.066]);
		set_unit_value_ssc_array(solarfield, "D_abs_out" ); // [0.07, 0.07, 0.07, 0.07]);
		set_unit_value_ssc_array(solarfield, "D_glass_in" ); // [0.115, 0.115, 0.115, 0.115]);
		set_unit_value_ssc_array(solarfield, "D_glass_out" ); // [0.12, 0.12, 0.12, 0.12]);
		set_unit_value_ssc_array(solarfield, "D_plug" ); // [0, 0, 0, 0]);
		set_unit_value_ssc_array(solarfield, "Flow_type" ); // [1, 1, 1, 1]);
		set_unit_value_ssc_array(solarfield, "Rough" ); // [4.50E-05, 4.50E-05, 4.50E-05, 4.50E-05]);
		set_unit_value_ssc_array(solarfield, "alpha_env" ); // [0.02, 0.02, 0, 0]);
		set_unit_value_ssc_matrix_transpose(solarfield, "epsilon_abs_1"); // [[100, 150, 200, 250, 300, 350, 400, 450, 500], [0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112]]);
		set_unit_value_ssc_matrix_transpose(solarfield, "epsilon_abs_2"); // [[0], [0.65]]);
		set_unit_value_ssc_matrix_transpose(solarfield, "epsilon_abs_3"); // [[0], [0.65]]);
		set_unit_value_ssc_matrix_transpose(solarfield, "epsilon_abs_4"); // [[0], [0]]);
		set_unit_value_ssc_array(solarfield, "alpha_abs" ); // alpha_abs);
		set_unit_value_ssc_array(solarfield, "Tau_envelope" ); // Tau_envelope);
		set_unit_value_ssc_array(solarfield, "epsilon_glass" ); // [0.86, 0.86, 1, 0]);
		set_unit_value_ssc_array(solarfield, "GlazingIntactIn" ); // [1, 1, 0, 1]);
		set_unit_value_ssc_array(solarfield, "P_a" ); // [0.0001, 750, 750, 0]);
		set_unit_value_ssc_array(solarfield, "AnnulusGas" ); // [27, 1, 1, 27]);
		set_unit_value_ssc_array(solarfield, "AbsorberMaterial" ); // [1, 1, 1, 1]);
		set_unit_value_ssc_array(solarfield, "Shadowing" ); // [0.96, 0.96, 0.96, 0.963]);
		set_unit_value_ssc_array(solarfield, "dirt_env" ); // [0.98, 0.98, 1, 0.98]);
		set_unit_value_ssc_array(solarfield, "Design_loss" ); // Design_loss);
		set_unit_value_ssc_double(solarfield, "L_mod_spacing" ); // 1);
		set_unit_value_ssc_double(solarfield, "L_crossover" ); // 15);
		set_unit_value_ssc_array(solarfield, "HL_T_coefs" ); // HL_T_coefs);
		set_unit_value_ssc_array(solarfield, "HL_w_coefs" ); // HL_w_coefs);
		set_unit_value_ssc_double(solarfield, "DP_nominal" ); // 2.5);
		set_unit_value_ssc_array(solarfield, "DP_coefs" ); // [0., 1., 0., 0.]);
		set_unit_value_ssc_double(solarfield, "rec_htf_vol" ); // 1);
		set_unit_value_ssc_double(solarfield, "T_amb_sf_des" ); // T_amb_des);
		set_unit_value_ssc_double(solarfield, "V_wind_des" ); // V_wind_des);


		//Set the initial values
//		set_unit_value_ssc_double(solarfield, "I_b" ); // 0.);
//		set_unit_value_ssc_double(solarfield, "T_db" ); // 15.);
//		set_unit_value_ssc_double(solarfield, "V_wind" ); // 1.5);
//		set_unit_value_ssc_double(solarfield, "P_amb" ); // 1.);
//		set_unit_value_ssc_double(solarfield, "T_dp" ); // 10.);
		set_unit_value_ssc_double(solarfield, "T_cold_in"); // 293.);
// m_dot_in
		set_unit_value_ssc_double(solarfield, "defocus"); // 1.);
//		set_unit_value_ssc_double(solarfield, "SolarAz"); // 0.);
// SolarZen
// latitude
// longitude
// timezone


		

		//Set controller parameters type 251
		set_unit_value_ssc_double(controller, "field_fluid" ); // field_fluid);
		set_unit_value_ssc_matrix(controller, "field_fl_props" ); // [0]);
		set_unit_value_ssc_double(controller, "store_fluid" ); // tes_fluid);
		set_unit_value_ssc_matrix(controller, "store_fl_props" );		
		set_unit_value_ssc_double(controller, "tshours" ); // TES_hrs);
		set_unit_value_ssc_double(controller, "is_hx" ); // is_hx);
		set_unit_value_ssc_double(controller, "dt_hot" ); // hx_dt_hot);
		set_unit_value_ssc_double(controller, "dt_cold" ); // hx_dt_cold);
		set_unit_value_ssc_double(controller, "hx_config" ); // 2);
		set_unit_value_ssc_double(controller, "q_max_aux" ); // Qsf);
		set_unit_value_ssc_double(controller, "lhv_eff", as_double("eta_lhv"));			// 9.17.14 twn: input lhv here to calculate fuel usage
		set_unit_value_ssc_double(controller, "T_set_aux" ); // T_hot_des);
		set_unit_value_ssc_double(controller, "V_tank_hot_ini" ); // TES_init_vol);
		set_unit_value_ssc_double(controller, "T_tank_hot_ini" ); // T_hot_des);
		set_unit_value_ssc_double(controller, "T_tank_cold_ini" ); // T_cold_des);
		set_unit_value_ssc_double(controller, "vol_tank" ); // TES_tank_vol);
		set_unit_value_ssc_double(controller, "h_tank" ); // 20);
		set_unit_value_ssc_double(controller, "h_tank_min" ); // 1);
		set_unit_value_ssc_double(controller, "u_tank" ); // 0.4);
		set_unit_value_ssc_double(controller, "tank_pairs" ); // 1);
		// cold_tank_Thtr = T_cold_des - 30 = T_loop_in_des - 30
		// set_unit_value_ssc_double(controller, "cold_tank_Thtr"); // T_cold_fp);
		set_unit_value(controller, "cold_tank_Thtr", (as_double("T_loop_in_des") - 30.0) );
		// hot_tank_Thtr = T_hot_des - 100 = T_loop_out - 100
		//set_unit_value_ssc_double(controller, "hot_tank_Thtr"); // T_hot_fp);
		set_unit_value(controller, "hot_tank_Thtr", (as_double("T_loop_out") - 100.0));
		// Q_TES_htr = 1.5*TES_tank_vol/1000.;
		//set_unit_value_ssc_double(controller, "tank_max_heat"); // Q_TES_htr);
		set_unit_value(controller, "cold_tank_max_heat", (1.5 * as_double("vol_Tank") / 1000.0));
		set_unit_value(controller, "hot_tank_max_heat", (1.5 * as_double("vol_Tank") / 1000.0));
		set_unit_value_ssc_double(controller, "T_field_in_des"); // T_cold_des);
		set_unit_value_ssc_double(controller, "T_field_out_des" ); // T_hot_des);
		set_unit_value_ssc_double(controller, "q_pb_design" ); // Q_cycle_des);
		set_unit_value_ssc_double(controller, "W_pb_design" ); // E_gross);
		set_unit_value_ssc_double(controller, "cycle_max_frac" ); // 1.05);
		set_unit_value_ssc_double(controller, "cycle_cutoff_frac" ); // 0.25);
		set_unit_value_ssc_double(controller, "solarm" ); // SM);
		set_unit_value_ssc_double(controller, "pb_pump_coef" ); // 0.55);
		set_unit_value_ssc_double(controller, "tes_pump_coef" ); // 0.15);
		set_unit_value_ssc_double(controller, "pb_fixed_par" ); // 0.0055);
		set_unit_value_ssc_array(controller, "bop_array" ); // [0, 1, 0.483, 0.517, 0]);
		set_unit_value_ssc_array(controller, "aux_array" ); // [0.02273, 1, 0.483, 0.517, 0]);
		// tes_temp = T_sf_ave
		set_unit_value_ssc_double(controller, "T_startup" ); // T_sf_ave);
		//set_unit_value(controller, "T_startup", as_double("tes_temp"));
		set_unit_value_ssc_double(controller, "fossil_mode" ); // 1);
		set_unit_value_ssc_double(controller, "fthr_ok" ); // 1);
		set_unit_value_ssc_double(controller, "nSCA" ); // nMod);
		set_unit_value_ssc_double(controller, "I_bn_des" ); // I_bn_des);
		set_unit_value_ssc_double(controller, "fc_on" ); // 0);
		set_unit_value_ssc_double(controller, "q_sby_frac" ); // 0.2);
		set_unit_value_ssc_double(controller, "t_standby_reset" ); // 2);
		set_unit_value_ssc_double(controller, "sf_type", 1);		// 10.19.15, twn: MSLF will also be 1
		set_unit_value_ssc_double(controller, "tes_type" ); // 1);
		set_unit_value_ssc_array(controller, "tslogic_a" ); // [0, 0, 0, 0, 0, 0, 0, 0, 0]);
		set_unit_value_ssc_array(controller, "tslogic_b" ); // [0, 0, 0, 0, 0, 0, 0, 0, 0]);
		set_unit_value_ssc_array(controller, "tslogic_c" ); // [1.05, 1, 1, 1, 1, 1, 1, 1, 1]);
		set_unit_value_ssc_array(controller, "ffrac" ); // [0, 0, 0, 0, 0, 0, 0, 0, 0]);
		set_unit_value_ssc_double(controller, "tc_fill" ); // 7);
		set_unit_value_ssc_double(controller, "tc_void" ); // 0.25);
		set_unit_value_ssc_double(controller, "t_dis_out_min" ); // 500);
		set_unit_value_ssc_double(controller, "t_ch_out_max" ); // 500);
		set_unit_value_ssc_double(controller, "nodes" ); // 2000);
		set_unit_value_ssc_double(controller, "f_tc_cold" ); // 2);
        set_unit_value_ssc_double(controller, "V_tes_des"); // , 1.85);
        set_unit_value_ssc_double(controller, "custom_tes_p_loss"); // , false);
        set_unit_value_ssc_array(controller, "k_tes_loss_coeffs"); // , []);
        set_unit_value_ssc_double(controller, "custom_sgs_pipe_sizes"); // , []);
        set_unit_value_ssc_array(controller, "sgs_diams"); // , []);
        set_unit_value_ssc_array(controller, "sgs_wallthicks"); // , []);
        set_unit_value_ssc_array(controller, "sgs_lengths"); // , []);
        set_unit_value_ssc_double(controller, "DP_SGS"); // , []);
        set_unit_value_ssc_double(controller, "tanks_in_parallel"); // , 1 = true);
        set_unit_value_ssc_double(controller, "has_hot_tank_bypass"); // , 0 = false);
        set_unit_value_ssc_double(controller, "T_tank_hot_inlet_min"); // , 400);
        set_unit_value_ssc_double(controller, "calc_design_pipe_vals"); // , 1 = true);
        set_unit_value_ssc_double(controller, "eta_pump"); // , 0.85);
        set_unit_value_ssc_double(controller, "HDR_rough"); // , 4.57E-05);


		set_unit_value_ssc_matrix(tou_translator, "weekday_schedule");
		set_unit_value_ssc_matrix(tou_translator, "weekend_schedule");


		set_unit_value(controller, "T_field_out", as_double("T_loop_out"));
		set_unit_value(controller, "T_pb_out", as_double("T_loop_in_des"));
		//set_unit_value_ssc_double(controller, "m_pb_demand", 0.0);


		// Set Common Type 224 Power Cycle Parameters
		set_unit_value_ssc_double(powerblock, "P_ref" );
		set_unit_value_ssc_double(powerblock, "eta_ref" );
		set_unit_value_ssc_double(powerblock, "T_htf_hot_ref" );
		set_unit_value_ssc_double(powerblock, "T_htf_cold_ref" );
		set_unit_value_ssc_double(powerblock, "cycle_max_frac");
		set_unit_value_ssc_double(powerblock, "cycle_cutoff_frac");
		set_unit_value_ssc_double(powerblock, "q_sby_frac");
		set_unit_value_ssc_double(powerblock, "startup_time");
		set_unit_value_ssc_double(powerblock, "startup_frac");
		set_unit_value_ssc_double(powerblock, "pb_pump_coef");
		set_unit_value_ssc_double(powerblock, "HTF", as_double("Fluid"));
		set_unit_value_ssc_matrix(powerblock, "field_fl_props");

		set_unit_value_ssc_double(powerblock, "pc_config");

		int pc_config = as_integer("pc_config");

		if(pc_config == 0)
		{
			set_unit_value_ssc_double(powerblock, "dT_cw_ref");
			set_unit_value_ssc_double(powerblock, "T_amb_des");
		
			set_unit_value_ssc_double(powerblock, "P_boil");
			set_unit_value_ssc_double(powerblock, "CT");

			set_unit_value_ssc_double(powerblock, "tech_type");
			set_unit_value_ssc_double(powerblock, "T_approach");
			set_unit_value_ssc_double(powerblock, "T_ITD_des"); 
			set_unit_value_ssc_double(powerblock, "P_cond_ratio"); 
			set_unit_value_ssc_double(powerblock, "pb_bd_frac"); 
			set_unit_value_ssc_double(powerblock, "P_cond_min"); 
			set_unit_value_ssc_double(powerblock, "n_pl_inc"); 
			set_unit_value_ssc_array(powerblock, "F_wc"); 

		}
		else if(pc_config == 1)
		{
			set_unit_value_ssc_double(powerblock, "ud_f_W_dot_cool_des");
			set_unit_value_ssc_double(powerblock, "ud_m_dot_water_cool_des");

            set_unit_value_ssc_matrix(powerblock, "ud_ind_od");
		}
		else
		{
			log("The 'pc_config' must be either 0 or 1.\n", SSC_WARNING);
			return;
		}




		//Set initial values
		set_unit_value_ssc_double(powerblock, "T_db", 0.0); // 15.);
		set_unit_value_ssc_double(powerblock, "P_amb", 1.0); // 1.);
//		set_unit_value_ssc_double(powerblock, "T_htf_hot" ); // T_hot_des);
//		set_unit_value(controller, "T_htf_hot", as_double("T_loop_out"));
//		set_unit_value_ssc_double(powerblock, "m_dot_htf_init"); // 0.);
		//set_unit_value_ssc_double(powerblock, "standby_control" ); // 0);

		//Connect the enet calculator to the parasitic values
		set_unit_value_ssc_double(enet, "eta_lhv" ); // 0.9);
		set_unit_value_ssc_double(enet, "eta_tes_htr" ); // 0.98);
		set_unit_value_ssc_double(enet, "fp_mode" ); // freeze_prot_mode);


		//Set the connections
		bool bConnected = connect(weather, "beam", solarfield, "I_b",0);
		bConnected &= connect(weather, "tdry", solarfield, "T_db");
		bConnected &= connect(weather, "wspd", solarfield, "V_wind");
		bConnected &= connect(weather, "pres", solarfield, "P_amb");
		bConnected &= connect(weather, "tdew", solarfield, "T_dp");
		bConnected &= connect(weather, "solazi", solarfield, "SolarAz");
		bConnected &= connect(weather, "solzen", solarfield, "SolarZen");
		bConnected &= connect(weather, "lat", solarfield, "latitude");
		bConnected &= connect(weather, "lon", solarfield, "longitude");
		bConnected &= connect(weather, "tz", solarfield, "timezone");
		if (!bConnected)
			throw exec_error("tcsmslf", util::format("there was a problem connecting outputs of weather to inputs of solarfield for the simulation."));





		bConnected &= connect(tou_translator, "tou_value", controller, "TOUPeriod");
		if (!bConnected)
			throw exec_error("tcsmslf", util::format("there was a problem connecting outputs of tou to inputs of controller for the simulation."));

		//Connect weather reader to controller
		bConnected &= connect(weather, "beam", controller, "I_bn");
		bConnected &= connect(weather, "tdry", controller, "T_amb");
		bConnected &= connect(solarfield, "m_dot_field_htf", controller, "m_dot_field");
		bConnected &= connect(powerblock, "m_dot_htf_ref", controller, "m_dot_htf_ref");
		bConnected &= connect(solarfield, "T_sys_h", controller, "T_field_out");
		bConnected &= connect(powerblock, "T_htf_cold", controller, "T_pb_out");

		if (!bConnected)
			throw exec_error("tcsmslf", util::format("there was a problem connecting outputs of some units to inputs of controller for the simulation."));


		bConnected &= connect(controller, "defocus", solarfield, "defocus");
		bConnected &= connect(controller, "T_field_in", solarfield, "T_cold_in");
		if (!bConnected)
			throw exec_error("tcsmslf", util::format("there was a problem connecting outputs of controller to inputs of solarfield for the simulation."));

		//Connect inputs
		bConnected &= connect(weather, "twet", powerblock, "T_wb");
		bConnected &= connect(weather, "tdry", powerblock, "T_db");
		bConnected &= connect(weather, "pres", powerblock, "P_amb");
		bConnected &= connect(weather, "rhum", powerblock, "rh");

		bConnected &= connect(controller, "T_pb_in", powerblock, "T_htf_hot");
//		bConnected &= connect(controller, "T_pb_in", powerblock, "T_htf_hot_ref");
		bConnected &= connect(controller, "m_dot_pb", powerblock, "m_dot_htf");
		bConnected &= connect(controller, "m_dot_pb", powerblock, "demand_var");
		bConnected &= connect(controller, "standby_control", powerblock, "standby_control");
		bConnected &= connect(controller, "TOU", powerblock, "TOU");

		bConnected &= connect(powerblock, "P_cycle", enet, "W_cycle_gross");
		bConnected &= connect(powerblock, "W_cool_par", enet, "W_par_heatrej");
		bConnected &= connect(solarfield, "W_dot_pump", enet, "W_par_sf_pump");
		bConnected &= connect(controller, "htf_pump_power", enet, "W_par_tes_pump");
		bConnected &= connect(controller, "bop_par", enet, "W_par_BOP");
		bConnected &= connect(controller, "fixed_par", enet, "W_par_fixed");
		bConnected &= connect(solarfield, "track_par_tot", enet, "W_par_tracking");
		bConnected &= connect(controller, "aux_par", enet, "W_par_aux_boiler");
		bConnected &= connect(controller, "tank_fp_par", enet, "Q_par_tes_fp");
		bConnected &= connect(solarfield, "E_fp_tot", enet, "Q_par_sf_fp");
		bConnected &= connect(controller, "q_aux_heat", enet, "Q_aux_backup");
	
        bConnected &= connect(solarfield, "defocus_rel", controller, "defocus_prev");   // unique tolerance is just to stand out when debugging
        
        set_unit_value_ssc_double(controller, "defocus_prev", 0.9);  //

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsmslf", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
//		int error = simulate(3600, hours * 3600, 3600, 30);
		int error = simulate(3600.0, hours * 3600.0, 3600.0);
		if (0 > error)
			throw exec_error( "tcsmslf", util::format("there was a problem simulating in the TCS molten salt linear fresnel model. Error %d", error) );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsmslf", util::format("there was a problem returning the results from the simulation.") );
        
        // design parameters
        int nv;
        double *sgs_diams = get_unit_value(controller, "SGS_diams", &nv);
        ssc_number_t *sgs_diams_cm = allocate("pipe_sgs_diams", nv);
        std::copy(sgs_diams, sgs_diams + nv, sgs_diams_cm);
        double *sgs_wallthk = get_unit_value(controller, "SGS_wall_thk", &nv);
        ssc_number_t *sgs_wallthk_cm = allocate("pipe_sgs_wallthk", nv);
        std::copy(sgs_wallthk, sgs_wallthk + nv, sgs_wallthk_cm);
        double *sgs_mdot_dsn = get_unit_value(controller, "SGS_m_dot_des", &nv);
        ssc_number_t *sgs_mdot_dsn_cm = allocate("pipe_sgs_mdot_dsn", nv);
        std::copy(sgs_mdot_dsn, sgs_mdot_dsn + nv, sgs_mdot_dsn_cm);
        double *sgs_vel_dsn = get_unit_value(controller, "SGS_vel_des", &nv);
        ssc_number_t *sgs_vel_dsn_cm = allocate("pipe_sgs_vel_dsn", nv);
        std::copy(sgs_vel_dsn, sgs_vel_dsn + nv, sgs_vel_dsn_cm);
        double *sgs_T_dsn = get_unit_value(controller, "SGS_T_des", &nv);
        ssc_number_t *sgs_T_dsn_cm = allocate("pipe_sgs_T_dsn", nv);
        std::copy(sgs_T_dsn, sgs_T_dsn + nv, sgs_T_dsn_cm);
        double *sgs_P_dsn = get_unit_value(controller, "SGS_P_des", &nv);
        ssc_number_t *sgs_P_dsn_cm = allocate("pipe_sgs_P_dsn", nv);
        std::copy(sgs_P_dsn, sgs_P_dsn + nv, sgs_P_dsn_cm);

		// performance adjustement factors
		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("tcsmslf", "failed to setup adjustment factors: " + haf.error());
		
		size_t count;
		ssc_number_t *p_hourly_energy = allocate("gen", 8760);
		ssc_number_t *timestep_energy_MW = as_array("W_net", &count);			//MW
		char tstr[500];
		std::string out_msg = "hourly energy count %d is incorrect (should be %d)";
		sprintf(tstr, out_msg.c_str(), count, 8760);
		out_msg = tstr;
		if( count != 8760 )
			throw exec_error("tcsmslf", out_msg);

		// Get hourly energy
		for( size_t i = 0; i < count; i++ )
			p_hourly_energy[i] = (ssc_number_t)(timestep_energy_MW[i] * 1000.0);	// convert to kW

		//1.7.15, twn: Need to calculated the conversion factor before the performance adjustments are applied to "hourly energy"
		accumulate_annual("gen", "annual_energy"); // already in kWh
		accumulate_annual("P_cycle", "annual_W_cycle_gross", 1000); // convert from MWh to kWh
		// Calculated outputs
		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		assign("conversion_factor", convfactor);

		// size_t count;
		// ssc_number_t *p_hourly_energy = as_array("hourly_energy", &count);
		// set hourly energy = tcs output Enet
		
		// apply performance adjustments and convert from MWh to kWh 
		for (size_t i = 0; i < count; i++)
		{
			p_hourly_energy[i] = p_hourly_energy[i] * (ssc_number_t)(haf(i));	// already in kWh
//			p_gen[i] = p_hourly_energy[i];
		}


		accumulate_annual("gen", "annual_energy"); // already in kWh
		accumulate_monthly("gen", "monthly_energy"); // already in kWh
		
		// First, sum power cycle water consumption timeseries outputs
		accumulate_annual_for_year("m_dot_makeup", "annual_total_water_use", 1.0 / 1000.0, 1); //[m^3], convert from kg
		// Then, add water usage from mirror cleaning
		ssc_number_t V_water_cycle = as_number("annual_total_water_use");
		double A_aper_tot = get_unit_value_number(solarfield, "A_aper_tot");
		double V_water_mirrors = as_double("water_per_wash") / 1000.0*A_aper_tot*as_double("washes_per_year");
		assign("annual_total_water_use", (ssc_number_t)(V_water_cycle + V_water_mirrors));

		double fuel_usage_mmbtu = 0;
		ssc_number_t *hourly_fuel = as_array("q_aux_fuel", &count);//MWh
		if (count != 8760)
		  {
		    std::stringstream msg;
		    msg << "q_aux_fuel count incorrect (should be 8760): " << count;
		    throw exec_error("tcslinear_fresnel", msg.str());
		  }
		for (size_t i = 0; i < count; i++)
			fuel_usage_mmbtu += hourly_fuel[i];
		assign("system_heat_rate", (ssc_number_t)3.413); // samsim tcstrough_physical
		// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		assign("annual_fuel_usage", var_data((ssc_number_t)(fuel_usage_mmbtu * 293.297)));

		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
			annual_energy += p_hourly_energy[i];
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsmslf, "CSP model using the molten salt linear fresnel TCS types.", 4 )
