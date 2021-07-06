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
#include "interconnect.h"

#include <cmath>
#include <algorithm>
#include <vector>
#include <fstream>

using namespace std;

enum{
	//parameters and inputs
	P_NSCA,
	P_NHCET,
	P_NCOLT,
	P_NHCEVAR,
	P_NLOOPS,
	P_ETA_PUMP,
	P_HDR_ROUGH,
	P_THETA_STOW,
	P_THETA_DEP,
	P_ROW_DISTANCE,
	P_FIELDCONFIG,
    P_T_RECIRC,
	P_PB_RATED_CAP,
	P_M_DOT_HTFMIN,
	P_M_DOT_HTFMAX,
	P_T_LOOP_IN_DES,
	P_T_LOOP_OUT,
	P_FLUID,
	P_T_FIELD_INI,
	P_FIELD_FL_PROPS,
	P_T_FP,
	P_I_BN_DES,
    P_DES_PIPE_VALS,
    P_DP_SGS_1,
	P_V_HDR_COLD_MAX,
	P_V_HDR_COLD_MIN,
    P_V_HDR_HOT_MAX,
    P_V_HDR_HOT_MIN,
    P_NMAX_HDR_DIAMS,
    P_L_RNR_PB,
    P_L_RNR_PER_XPAN,
    P_L_XPAN_HDR,
    P_L_XPAN_RNR,
    P_MIN_RNR_XPANS,
    P_NTHSTH_FIELD_SEP,
    P_NHDR_PER_XPAN,
    P_OFFSET_XPAN_HDR,
	P_PIPE_HL_COEF,
	P_SCA_DRIVES_ELEC,
	P_FTHROK,
	P_FTHRCTRL,
	P_COLTILT,
	P_COLAZ,
	P_ACCEPT_MODE,
	P_ACCEPT_INIT,
	P_ACCEPT_LOC,
	P_USING_INPUT_GEN,
	P_SOLAR_MULT,
	P_MC_BAL_HOT,
	P_MC_BAL_COLD,
	P_MC_BAL_SCA,
    P_V_SGS,

	P_OPTCHARTYPE,
	P_COLLECTORTYPE,
	P_W_APERTURE,
	P_A_APERTURE,
	//P_IAMF0,
	//P_IAMF1,
	//P_IAMF2,
	P_REFLECTIVITY,
	P_TRACKINGERROR,
	P_GEOMEFFECTS,
	P_RHO_MIRROR_CLEAN,
	P_DIRT_MIRROR,
	P_ERROR,
	P_AVE_FOCAL_LENGTH,
	P_L_SCA,
	P_L_APERTURE,
	P_COLPERSCA,
	P_DISTANCE_SCA,

	P_IAM_MATRIX,

	P_HCE_FIELDFRAC,
	P_D_2,
	P_D_3,
	P_D_4,
	P_D_5,
	P_D_P,
	P_FLOW_TYPE,
	P_ROUGH,
	P_ALPHA_ENV,
	P_EPSILON_3_11,
	P_EPSILON_3_12,
	P_EPSILON_3_13,
	P_EPSILON_3_14,
	P_EPSILON_3_21,
	P_EPSILON_3_22,
	P_EPSILON_3_23,
	P_EPSILON_3_24,
	P_EPSILON_3_31,
	P_EPSILON_3_32,
	P_EPSILON_3_33,
	P_EPSILON_3_34,
	P_EPSILON_3_41,
	P_EPSILON_3_42,
	P_EPSILON_3_43,
	P_EPSILON_3_44,
	P_ALPHA_ABS,
	P_TAU_ENVELOPE,
	P_EPSILON_4,
	P_EPSILON_5,
	P_GLAZINGINTACTIN,
	P_P_A,
	P_ANNULUSGAS,
	P_ABSORBERMATERIAL,
	P_SHADOWING,
	P_DIRT_HCE,
	P_DESIGN_LOSS,

	P_SCAINFOARRAY,
	P_SCADEFOCUSARRAY,

    P_K_CPNT,
    P_D_CPNT,
    P_L_CPNT,
    P_TYPE_CPNT,

    P_CUSTOM_SF_PIPE_SIZES,
    P_SF_RNR_DIAMS,
    P_SF_RNR_WALLTHICKS,
    P_SF_RNR_LENGTHS,
    P_SF_HDR_DIAMS,
    P_SF_HDR_WALLTHICKS,
    P_SF_HDR_LENGTHS,

	PO_A_APER_TOT,

	I_I_B,
	I_T_DB,
	I_V_WIND,
	I_P_AMB,
	I_T_DP,
	I_T_COLD_IN,
	I_M_DOT_IN,
	I_DEFOCUS,
	I_SOLARAZ,
	I_LATITUDE,
	I_LONGITUDE,
	I_SHIFT,
    I_RECIRC,

	O_HEADER_DIAMS,
    O_HEADER_WALLTHK,
    O_HEADER_LENGTHS,
    O_HEADER_XPANS,
    O_HEADER_MDOT_DSN,
    O_HEADER_V_DSN,
    O_HEADER_T_DSN,
    O_HEADER_P_DSN,
	O_RUNNER_DIAMS,
    O_RUNNER_WALLTHK,
	O_RUNNER_LENGTHS,
    O_RUNNER_XPANS,
    O_RUNNER_MDOT_DSN,
    O_RUNNER_V_DSN,
    O_RUNNER_T_DSN,
    O_RUNNER_P_DSN,
    O_LOOP_T_DSN,
    O_LOOP_P_DSN,
    O_T_FIELD_IN_AT_DSN,
    O_T_FIELD_OUT_AT_DSN,
    O_P_FIELD_IN_AT_DSN,
	O_T_SYS_H,
	O_M_DOT_AVAIL,
    O_M_DOT_FIELD_HTF,
	O_Q_AVAIL,
	O_DP_TOT,
	O_W_DOT_PUMP,
	O_E_FP_TOT,
	O_QQ,
	O_T_SYS_C,
	O_EQOPTEFF,
	O_SCAS_DEF,
	O_M_DOT_HTF_TOT,
	O_E_BAL_STARTUP,
	O_Q_INC_SF_TOT,
	O_Q_ABS_TOT,
	O_Q_LOSS_TOT,
	O_M_DOT_HTF,
	O_Q_LOSS_SPEC_TOT,
	O_SCA_PAR_TOT,
	O_PIPE_HL,
	O_Q_DUMP,
	O_THETA_AVE,
	O_COSTH_AVE,
	O_IAM_AVE,
	O_ROWSHADOW_AVE,
	O_ENDLOSS_AVE,
	O_DNI_COSTH,
	O_QINC_COSTH,
	O_T_LOOP_OUTLET,
	O_C_HTF_AVE,
	O_Q_FIELD_DELIVERED,
	O_ETA_THERMAL,
	O_E_LOOP_ACCUM,
	O_E_HDR_ACCUM,
	O_E_TOT_ACCUM,
	O_E_FIELD,
	O_T_C_IN_CALC,
    O_DEFOCUS,

	//Include N_max
	N_MAX
};


tcsvarinfo sam_mw_trough_type250_variables[] = {
	// vartype,		      datatype,		            index,				       name,		                                                                             label,          units,           meta,          group,  default_value
	{ TCS_PARAM,          TCS_NUMBER,              P_NSCA,                   "nSCA",                                                               "Number of SCA's in a loop",         "none",             "",             "",            "8" },
	{ TCS_PARAM,          TCS_NUMBER,             P_NHCET,                  "nHCEt",                                                                     "Number of HCE types",         "none",             "",             "",            "4" },
	{ TCS_PARAM,          TCS_NUMBER,             P_NCOLT,                  "nColt",                                                               "Number of collector types",         "none",             "",             "",            "4" },
	{ TCS_PARAM,          TCS_NUMBER,           P_NHCEVAR,                "nHCEVar",                                                         "Number of HCE variants per type",         "none",             "",             "",            "4" },
	{ TCS_PARAM,          TCS_NUMBER,            P_NLOOPS,                 "nLoops",                                                            "Number of loops in the field",         "none",             "",             "",          "230" },
	{ TCS_PARAM,          TCS_NUMBER,          P_ETA_PUMP,               "eta_pump",                                                                     "HTF pump efficiency",         "none",             "",             "",         "0.85" },
	{ TCS_PARAM,          TCS_NUMBER,         P_HDR_ROUGH,              "HDR_rough",                                                                   "Header pipe roughness",            "m",             "",             "",     "4.57E-05" },
	{ TCS_PARAM,          TCS_NUMBER,        P_THETA_STOW,             "theta_stow",                                                                              "stow angle",          "deg",             "",             "",          "170" },
	{ TCS_PARAM,          TCS_NUMBER,         P_THETA_DEP,              "theta_dep",                                                                            "deploy angle",          "deg",             "",             "",           "10" },
	{ TCS_PARAM,          TCS_NUMBER,      P_ROW_DISTANCE,           "Row_Distance",                                         "Spacing between rows (centerline to centerline)",            "m",             "",             "",           "15" },
	{ TCS_PARAM,          TCS_NUMBER,       P_FIELDCONFIG,            "FieldConfig",                                                              "Number of subfield headers",         "none",             "",             "",            "2" },
    { TCS_PARAM,          TCS_NUMBER,          P_T_RECIRC,               "T_recirc",                                      "The temperature which below the field recirculates",            "C",             "",             "",          "300" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PB_RATED_CAP,           "pb_rated_cap",                                                                    "Rated plant capacity",          "MWe",             "",             "",          "111" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMIN,           "m_dot_htfmin",                                                              "Minimum loop HTF flow rate",         "kg/s",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMAX,           "m_dot_htfmax",                                                              "Maximum loop HTF flow rate",         "kg/s",             "",             "",           "12" },
	{ TCS_PARAM,          TCS_NUMBER,     P_T_LOOP_IN_DES,          "T_loop_in_des",                                                           "Design loop inlet temperature",            "C",             "",             "",          "293" },
	{ TCS_PARAM,          TCS_NUMBER,        P_T_LOOP_OUT,             "T_loop_out",                                                          "Target loop outlet temperature",            "C",             "",             "",          "391" },
	{ TCS_PARAM,          TCS_NUMBER,             P_FLUID,                  "Fluid",                                                                  "Field HTF fluid number",         "none",             "",             "",           "21" },
	{ TCS_PARAM,          TCS_NUMBER,       P_T_FIELD_INI,            "T_field_ini",                                                               "Initial field temperature",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_MATRIX,    P_FIELD_FL_PROPS,         "field_fl_props",                                                                     "Fluid property data",         "none","7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",             "",             "" },
	{ TCS_PARAM,          TCS_NUMBER,              P_T_FP,                   "T_fp",                       "Freeze protection temperature (heat trace activation temperature)",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_NUMBER,          P_I_BN_DES,               "I_bn_des",                                                             "Solar irradiation at design",         "W/m2",             "",             "",          "950" },
    { TCS_PARAM,          TCS_NUMBER,     P_DES_PIPE_VALS,  "calc_design_pipe_vals",                                      "Calculate temps and pressures at design conditions",            "-",             "",             "",         "true" },
    { TCS_PARAM,          TCS_NUMBER,          P_DP_SGS_1,               "DP_SGS_1",           "Pressure drop in first section of TES/PB before hot tank at design conditions",          "bar",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,    P_V_HDR_COLD_MAX,         "V_hdr_cold_max",                                      "Maximum HTF velocity in the cold headers at design",          "m/s",             "",             "",            "3" },
	{ TCS_PARAM,          TCS_NUMBER,    P_V_HDR_COLD_MIN,         "V_hdr_cold_min",                                      "Minimum HTF velocity in the cold headers at design",          "m/s",             "",             "",            "2" },
    { TCS_PARAM,          TCS_NUMBER,     P_V_HDR_HOT_MAX,          "V_hdr_hot_max",                                       "Maximum HTF velocity in the hot headers at design",          "m/s",             "",             "",            "3" },
    { TCS_PARAM,          TCS_NUMBER,     P_V_HDR_HOT_MIN,          "V_hdr_hot_min",                                       "Minimum HTF velocity in the hot headers at design",          "m/s",             "",             "",            "2" },
    { TCS_PARAM,          TCS_NUMBER,    P_NMAX_HDR_DIAMS,        "N_max_hdr_diams",                         "Maximum number of diameters in each of the hot and cold headers",         "none",             "",             "",           "10" },
    { TCS_PARAM,          TCS_NUMBER,          P_L_RNR_PB,               "L_rnr_pb",                                                    "Length of runner pipe in power block",            "m",             "",             "",          "25" },
    { TCS_PARAM,          TCS_NUMBER,    P_L_RNR_PER_XPAN,         "L_rnr_per_xpan",                      "Threshold length of straight runner pipe without an expansion loop",            "m",             "",             "",          "70" },
    { TCS_PARAM,          TCS_NUMBER,        P_L_XPAN_HDR,             "L_xpan_hdr",                            "Combined perpendicular lengths of each header expansion loop",            "m",             "",             "",          "20" },
    { TCS_PARAM,          TCS_NUMBER,        P_L_XPAN_RNR,             "L_xpan_rnr",                            "Combined perpendicular lengths of each runner expansion loop",            "m",             "",             "",          "20" },
    { TCS_PARAM,          TCS_NUMBER,     P_MIN_RNR_XPANS,          "Min_rnr_xpans",                    "Minimum number of expansion loops per single-diameter runner section",         "none",             "",             "",           "1" },
    { TCS_PARAM,          TCS_NUMBER,  P_NTHSTH_FIELD_SEP,   "northsouth_field_sep",                         "North/south separation between subfields. 0 = SCAs are touching",            "m",             "",             "",          "20" },
    { TCS_PARAM,          TCS_NUMBER,     P_NHDR_PER_XPAN,         "N_hdr_per_xpan",                                            "Number of collector loops per expansion loop",         "none",             "",             "",           "2" },
    { TCS_PARAM,          TCS_NUMBER,   P_OFFSET_XPAN_HDR,        "offset_xpan_hdr",                 "Location of first header expansion loop. 1 = after first collector loop",         "none",             "",             "",           "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PIPE_HL_COEF,           "Pipe_hl_coef",                       "Loss coefficient from the header, runner pipe, and non-HCE piping",       "W/m2-K",             "",             "",         "0.45" },
	{ TCS_PARAM,          TCS_NUMBER,   P_SCA_DRIVES_ELEC,        "SCA_drives_elec",                                                  "Tracking power, in Watts per SCA drive",        "W/SCA",             "",             "",          "125" },
	{ TCS_PARAM,          TCS_NUMBER,            P_FTHROK,                 "fthrok",                                      "Flag to allow partial defocusing of the collectors",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,          P_FTHRCTRL,               "fthrctrl",                                                                     "Defocusing strategy",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,           P_COLTILT,                "ColTilt",                               "Collector tilt angle (0 is horizontal, 90deg is vertical)",          "deg",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,             P_COLAZ,                  "ColAz",                                                                 "Collector azimuth angle",          "deg",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,       P_ACCEPT_MODE,            "accept_mode",                                                  "Acceptance testing mode? (1=yes, 0=no)",         "none",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,       P_ACCEPT_INIT,            "accept_init",                               "In acceptance testing mode - require steady-state startup",         "none",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,        P_ACCEPT_LOC,             "accept_loc",                  "In acceptance testing mode - temperature sensor location (1=hx,2=loop)",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,   P_USING_INPUT_GEN,        "using_input_gen",             "Are weather inputs from weather file reader (0) or Type250 input generator?",         "none",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,        P_SOLAR_MULT,             "solar_mult",                                                                          "Solar multiple",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,        P_MC_BAL_HOT,             "mc_bal_hot",                               "The heat capacity of the balance of plant on the hot side",   "kWht/K-MWt",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,       P_MC_BAL_COLD,            "mc_bal_cold",                              "The heat capacity of the balance of plant on the cold side",   "kWht/K-MWt",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,        P_MC_BAL_SCA,             "mc_bal_sca",                        "Non-HTF heat capacity associated with each SCA - per meter basis",      "Wht/K-m",             "",             "",          "4.5" },
    { TCS_INPUT,          TCS_NUMBER,             P_V_SGS,                  "v_sgs",                                                     "HTF volume in SGS minus bypass loop",           "m3",             "",             "",           "-1" },

	{ TCS_PARAM,           TCS_ARRAY,       P_OPTCHARTYPE,            "OptCharType",                                                    "The optical characterization method ",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,     P_COLLECTORTYPE,          "CollectorType",                                                "{1=user defined, 2=LS-2, 3=LS-3, 4=IST} ",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,        P_W_APERTURE,             "W_aperture",               "The collector aperture width (Total structural area.. used for shadowing)",            "m",             "",             "",      "5,5,5,5" },
	{ TCS_PARAM,           TCS_ARRAY,        P_A_APERTURE,             "A_aperture",                                               "Reflective aperture area of the collector",           "m2",             "",             "","470.3,470.3,470.3,470.3" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF0,                  "IamF0",                                                  "Incident angle modifier 0th order term",         "none",             "",             "",      "1,1,1,1" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF1,                  "IamF1",                                                  "Incident angle modifier 1st order term",         "none",             "",             "","0.0506,0.0506,0.0506,0.0506" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF2,                  "IamF2",                                                  "Incident angle modifier 2nd order term",         "none",             "",             "","-0.1763,-0.1763,-0.1763,-0.1763" },
	{ TCS_PARAM,           TCS_ARRAY,      P_REFLECTIVITY,           "reflectivity",                                          "Base solar-weighted mirror reflectivity value ",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,     P_TRACKINGERROR,          "TrackingError",                                                      "User-defined tracking error derate",         "none",             "",             "","0.994,0.994,0.994,0.994" },
	{ TCS_PARAM,           TCS_ARRAY,       P_GEOMEFFECTS,            "GeomEffects",                                                    "User-defined geometry effects derate",         "none",             "",             "","0.98,0.98,0.98,0.98" },
	{ TCS_PARAM,           TCS_ARRAY,  P_RHO_MIRROR_CLEAN,       "Rho_mirror_clean",                                                  "User-defined clean mirror reflectivity",         "none",             "",             "","0.935,0.935,0.935,0.935" },
	{ TCS_PARAM,           TCS_ARRAY,       P_DIRT_MIRROR,            "Dirt_mirror",                                                      "User-defined dirt on mirror derate",         "none",             "",             "","0.95,0.95,0.95,0.95" },
	{ TCS_PARAM,           TCS_ARRAY,             P_ERROR,                  "Error",                                              "User-defined general optical error derate ",         "none",             "",             "","0.99,0.99,0.99,0.99" },
	{ TCS_PARAM,           TCS_ARRAY,  P_AVE_FOCAL_LENGTH,       "Ave_Focal_Length",                                              "The average focal length of the collector ",            "m",             "",             "","1.8,1.8,1.8,1.8" },
	{ TCS_PARAM,           TCS_ARRAY,             P_L_SCA,                  "L_SCA",                                                                  "The length of the SCA ",            "m",             "",             "","100,100,100,100" },
	{ TCS_PARAM,           TCS_ARRAY,        P_L_APERTURE,             "L_aperture",                                                  "The length of a single mirror/HCE unit",            "m",             "",             "","8.33333,8.33333,8.33333,8.33333" },
	{ TCS_PARAM,           TCS_ARRAY,         P_COLPERSCA,              "ColperSCA",                                  "The number of individual collector sections in an SCA ",         "none",             "",             "",  "12,12,12,12" },
	{ TCS_PARAM,           TCS_ARRAY,      P_DISTANCE_SCA,           "Distance_SCA",                                             " piping distance between SCA's in the field",            "m",             "",             "",      "1,1,1,1" },

	{ TCS_PARAM,          TCS_MATRIX,        P_IAM_MATRIX,          "IAM_matrix",                                    "Rows = # of collectors, Cols = max # of coefficients",         "none",             "",             "","[[0]]" },

	{ TCS_PARAM,          TCS_MATRIX,     P_HCE_FIELDFRAC,       "HCE_FieldFrac",                                    "The fraction of the field occupied by this HCE type ",         "none",             "",             "","[0.985,0.01,0.005,0][0.985,0.01,0.005,0][0.985,0.01,0.005,0][0.985,0.01,0.005,0]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_D_2,                 "D_2",                                                        "The inner absorber tube diameter",            "m",             "",             "","[0.066,0.066,0.066,0.066][0.066,0.066,0.066,0.066][0.066,0.066,0.066,0.066][0.066,0.066,0.066,0.066]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_D_3,                 "D_3",                                                        "The outer absorber tube diameter",            "m",             "",             "","[0.07,0.07,0.07,0.07][0.07,0.07,0.07,0.07][0.07,0.07,0.07,0.07][0.07,0.07,0.07,0.07]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_D_4,                 "D_4",                                                      "The inner glass envelope diameter ",            "m",             "",             "","[0.115,0.115,0.115,0.115][0.115,0.115,0.115,0.115][0.115,0.115,0.115,0.115][0.115,0.115,0.115,0.115]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_D_5,                 "D_5",                                                      "The outer glass envelope diameter ",            "m",             "",             "","[0.12,0.12,0.12,0.12][0.12,0.12,0.12,0.12][0.12,0.12,0.12,0.12][0.12,0.12,0.12,0.12]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_D_P,                 "D_p",                                      "The diameter of the absorber flow plug (optional) ",            "m",             "",             "","[0,0,0,0][0,0,0,0][0,0,0,0][0,0,0,0]" },
	{ TCS_PARAM,          TCS_MATRIX,         P_FLOW_TYPE,           "Flow_type",                                                      "The flow type through the absorber",         "none",             "",             "","[1,1,1,1][1,1,1,1][1,1,1,1][1,1,1,1]" },
	{ TCS_PARAM,          TCS_MATRIX,             P_ROUGH,               "Rough",                                         "Relative roughness of the internal HCE surface ",            "-",             "",             "","[4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05]" },
	{ TCS_PARAM,          TCS_MATRIX,         P_ALPHA_ENV,           "alpha_env",                                                                   "Envelope absorptance ",         "none",             "",             "","[0.02,0.02,0,0][0.02,0.02,0,0][0.02,0.02,0,0][0.02,0.02,0,0]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_11,        "epsilon_3_11",                                       "Absorber emittance - HCE type 1 - HCE variation 1",         "none",             "",             "","[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_12,        "epsilon_3_12",                                       "Absorber emittance - HCE type 1 - HCE variation 2",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_13,        "epsilon_3_13",                                       "Absorber emittance - HCE type 1 - HCE variation 3",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_14,        "epsilon_3_14",                                       "Absorber emittance - HCE type 1 - HCE variation 4",         "none",             "",             "",          "0,0" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_21,        "epsilon_3_21",                                       "Absorber emittance - HCE type 2 - HCE variation 1",         "none",             "",             "","[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_22,        "epsilon_3_22",                                       "Absorber emittance - HCE type 2 - HCE variation 2",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_23,        "epsilon_3_23",                                       "Absorber emittance - HCE type 2 - HCE variation 3",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_24,        "epsilon_3_24",                                       "Absorber emittance - HCE type 2 - HCE variation 4",         "none",             "",             "",          "0,0" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_31,        "epsilon_3_31",                                       "Absorber emittance - HCE type 3 - HCE variation 1",         "none",             "",             "","[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_32,        "epsilon_3_32",                                       "Absorber emittance - HCE type 3 - HCE variation 2",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_33,        "epsilon_3_33",                                       "Absorber emittance - HCE type 3 - HCE variation 3",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_34,        "epsilon_3_34",                                       "Absorber emittance - HCE type 3 - HCE variation 4",         "none",             "",             "",          "0,0" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_41,        "epsilon_3_41",                                       "Absorber emittance - HCE type 4 - HCE variation 1",         "none",             "",             "","[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_42,        "epsilon_3_42",                                       "Absorber emittance - HCE type 4 - HCE variation 2",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_43,        "epsilon_3_43",                                       "Absorber emittance - HCE type 4 - HCE variation 3",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,      P_EPSILON_3_44,        "epsilon_3_44",                                       "Absorber emittance - HCE type 4 - HCE variation 4",         "none",             "",             "",          "0,0" },
	{ TCS_PARAM,          TCS_MATRIX,         P_ALPHA_ABS,           "alpha_abs",                                                                   "Absorber absorptance ",         "none",             "",             "","[0.96,0.96,0.8,0][0.96,0.96,0.8,0][0.96,0.96,0.8,0][0.96,0.96,0.8,0]" },
	{ TCS_PARAM,          TCS_MATRIX,      P_TAU_ENVELOPE,        "Tau_envelope",                                                                  "Envelope transmittance",         "none",             "",             "","[0.963,0.963,1,0][0.963,0.963,1,0][0.963,0.963,1,0][0.963,0.963,1,0]" },
	{ TCS_PARAM,          TCS_MATRIX,         P_EPSILON_4,           "EPSILON_4",                                              "Inner glass envelope emissivities (Pyrex) ",         "none",             "",             "","[0.86,0.86,1,0][0.86,0.86,1,0][0.86,0.86,1,0][0.86,0.86,1,0]" },
	{ TCS_PARAM,          TCS_MATRIX,         P_EPSILON_5,           "EPSILON_5",                                              "Outer glass envelope emissivities (Pyrex) ",         "none",             "",             "","[0.86,0.86,1,0][0.86,0.86,1,0][0.86,0.86,1,0][0.86,0.86,1,0]" },
	{ TCS_PARAM,          TCS_MATRIX,   P_GLAZINGINTACTIN,     "GlazingIntactIn",                                            "The glazing intact flag {1=true, else=false}",         "none",             "",             "","[1,1,0,1][1,1,0,1][1,1,0,1][1,1,0,1]" },
	{ TCS_PARAM,          TCS_MATRIX,               P_P_A,                 "P_a",                                                                    "Annulus gas pressure",         "torr",             "",             "","[0.0001,750,750,0][0.0001,750,750,0][0.0001,750,750,0][0.0001,750,750,0]" },
	{ TCS_PARAM,          TCS_MATRIX,        P_ANNULUSGAS,          "AnnulusGas",                                                  "Annulus gas type (1=air, 26=Ar, 27=H2)",         "none",             "",             "","[27,1,1,27][27,1,1,27][27,1,1,27][27,1,1,27]" },
	{ TCS_PARAM,          TCS_MATRIX,  P_ABSORBERMATERIAL,    "AbsorberMaterial",                                                                  "Absorber material type",         "none",             "",             "","[1,1,1,1][1,1,1,1][1,1,1,1][1,1,1,1]" },
	{ TCS_PARAM,          TCS_MATRIX,         P_SHADOWING,           "Shadowing",                                                  "Receiver bellows shadowing loss factor",         "none",             "",             "","[0.96,0.96,0.96,0.963][0.96,0.96,0.96,0.963][0.96,0.96,0.96,0.963][0.96,0.96,0.96,0.963]" },
	{ TCS_PARAM,          TCS_MATRIX,          P_DIRT_HCE,            "Dirt_HCE",                                               "Loss due to dirt on the receiver envelope",         "none",             "",             "","[0.98,0.98,1,0.98][0.98,0.98,1,0.98][0.98,0.98,1,0.98][0.98,0.98,1,0.98]" },
	{ TCS_PARAM,          TCS_MATRIX,       P_DESIGN_LOSS,         "Design_loss",                                                            "Receiver heat loss at design",          "W/m",             "",             "","[150,1100,1500,0][150,1100,1500,0][150,1100,1500,0][150,1100,1500,0]" },

	{ TCS_PARAM,          TCS_MATRIX,      P_SCAINFOARRAY,        "SCAInfoArray",                       "(:,0) = HCE type, (:,1)= Collector type for each SCA in the loop ",         "none",             "",             "","[1,1][1,1][1,1][1,1][1,1][1,1][1,1][1,1]" },
	{ TCS_PARAM,           TCS_ARRAY,   P_SCADEFOCUSARRAY,     "SCADefocusArray",                                            "Order in which the SCA's should be defocused",         "none",             "",             "","8,7,6,5,4,3,2,1" },
    
    { TCS_PARAM,          TCS_MATRIX,            P_K_CPNT,              "K_cpnt",                      "Interconnect component minor loss coefficients, row=intc, col=cpnt",         "none",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_MATRIX,            P_D_CPNT,              "D_cpnt",                                    "Interconnect component diameters, row=intc, col=cpnt",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_MATRIX,            P_L_CPNT,              "L_cpnt",                                      "Interconnect component lengths, row=intc, col=cpnt",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_MATRIX,         P_TYPE_CPNT,           "Type_cpnt",                                         "Interconnect component type, row=intc, col=cpnt",         "none",             "",             "",           "-1" },

    { TCS_PARAM,          TCS_NUMBER, P_CUSTOM_SF_PIPE_SIZES, "custom_sf_pipe_sizes",                            "Use custom solar field pipe diams, wallthks, and lengths",         "none",             "",             "",         "false"},
    { TCS_PARAM,          TCS_ARRAY,       P_SF_RNR_DIAMS,        "sf_rnr_diams",                                                                 "Custom runner diameters",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_ARRAY,  P_SF_RNR_WALLTHICKS,   "sf_rnr_wallthicks",                                                          "Custom runner wall thicknesses",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_ARRAY,     P_SF_RNR_LENGTHS,      "sf_rnr_lengths",                                                                   "Custom runner lengths",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_ARRAY,       P_SF_HDR_DIAMS,        "sf_hdr_diams",                                                                 "Custom header diameters",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_ARRAY,  P_SF_HDR_WALLTHICKS,   "sf_hdr_wallthicks",                                                          "Custom header wall thicknesses",            "m",             "",             "",           "-1" },
    { TCS_PARAM,          TCS_ARRAY,     P_SF_HDR_LENGTHS,      "sf_hdr_lengths",                                                                   "Custom header lengths",            "m",             "",             "",           "-1" },

	// Field design calculations
	{ TCS_PARAM,          TCS_NUMBER,     PO_A_APER_TOT,             "A_aper_tot",                                          "Total solar field aperture area",                           "m^2",             "",             "",             "-1.23" },

	{ TCS_INPUT,          TCS_NUMBER,               I_I_B,                    "I_b",                                                "Direct normal incident solar irradiation",        "W/m^2",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DB,                   "T_db",                                                                "Dry bulb air temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,            I_V_WIND,                 "V_wind",                                                                      "Ambient windspeed ",          "m/s",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_P_AMB,                  "P_amb",                                                                        "Ambient pressure",         "mbar",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DP,                   "T_dp",                                                                "The dewpoint temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_T_COLD_IN,              "T_cold_in",                                                                  "HTF return temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_M_DOT_IN,               "m_dot_in",                                                         "HTF mass flow rate at the inlet",        "kg/hr",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_DEFOCUS,                "defocus",                                                                         "Defocus control",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_SOLARAZ,                "SolarAz",                                 "Solar azimuth angle reported by the Type15 weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_LATITUDE,               "latitude",                                                    "Site latitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_LONGITUDE,              "longitude",                                                   "Site longitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_SHIFT,                  "shift",                                         "shift in longitude from local standard meridian",          "deg",             "",             "",             "" },
    { TCS_INPUT,          TCS_NUMBER,            I_RECIRC,          "recirculating",                                                 "Field recirculating (bypass valve open)",         "none",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_ARRAY,      O_HEADER_DIAMS,      "pipe_header_diams",					                                        "Header piping diameter array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,    O_HEADER_WALLTHK,    "pipe_header_wallthk",					                                  "Header piping wall thickness array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,    O_HEADER_LENGTHS,    "pipe_header_lengths",                                                              "Header piping length array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_HEADER_XPANS, "pipe_header_expansions",                                                      "Number of header piping expansions",            "-",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,   O_HEADER_MDOT_DSN,   "pipe_header_mdot_dsn",					                              "Header piping mass flow rate at design",         "kg/s",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_HEADER_V_DSN,    "pipe_header_vel_dsn",					                                    "Header piping velocity at design",          "m/s",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_HEADER_T_DSN,      "pipe_header_T_dsn",					                                 "Header piping temperature at design",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_HEADER_P_DSN,      "pipe_header_P_dsn",					                                    "Header piping pressure at design",          "bar",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_ARRAY,      O_RUNNER_DIAMS,      "pipe_runner_diams",                                                            "Runner piping diameter array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,    O_RUNNER_WALLTHK,    "pipe_runner_wallthk",					                                  "Runner piping wall thickness array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,    O_RUNNER_LENGTHS,    "pipe_runner_lengths",                                                              "Runner piping length array",            "m",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_RUNNER_XPANS, "pipe_runner_expansions",                                                      "Number of runner piping expansions",            "-",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,   O_RUNNER_MDOT_DSN,   "pipe_runner_mdot_dsn",					                              "Runner piping mass flow rate at design",         "kg/s",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_RUNNER_V_DSN,    "pipe_runner_vel_dsn",					                                    "Runner piping velocity at design",          "m/s",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_RUNNER_T_DSN,      "pipe_runner_T_dsn",					                                 "Runner piping temperature at design",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,      O_RUNNER_P_DSN,      "pipe_runner_P_dsn",					                                    "Runner piping pressure at design",          "bar",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,        O_LOOP_T_DSN,        "pipe_loop_T_dsn",					                                 "Runner piping temperature at design",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_ARRAY,        O_LOOP_P_DSN,        "pipe_loop_P_dsn",					                                    "Runner piping pressure at design",          "bar",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER, O_T_FIELD_IN_AT_DSN,     "T_field_in_at_des",	                                 "Field/runner inlet temperature at design conditions",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER, O_T_FIELD_OUT_AT_DSN,   "T_field_out_at_des",		                            "Field/runner outlet temperature at design conditions",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER, O_P_FIELD_IN_AT_DSN,     "P_field_in_at_des",		                                "Field/runner inlet pressure at design conditions",          "bar",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_NUMBER,           O_T_SYS_H,                "T_sys_h",                                                      "Solar field HTF outlet temperature",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_M_DOT_AVAIL,            "m_dot_avail",                                                       "HTF mass flow rate from the field",        "kg/hr",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER,   O_M_DOT_FIELD_HTF,        "m_dot_field_htf",                         "HTF mass flow rate from the field, including when recirculating",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_Q_AVAIL,                "q_avail",                                                     "Thermal power produced by the field",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_DP_TOT,                 "DP_tot",                                                                 "Total HTF pressure drop",          "bar",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_W_DOT_PUMP,             "W_dot_pump",                                                      "Required solar field pumping power",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_E_FP_TOT,               "E_fp_tot",                                                                "Freeze protection energy",           "MW",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,                O_QQ,                     "qq",                                                  "Number of iterations required to solve",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_T_SYS_C,                "T_sys_c",                                                             "Collector inlet temperature",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_EQOPTEFF,               "EqOpteff",                                                 "Collector equivalent optical efficiency",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_SCAS_DEF,               "SCAs_def",                                                           "The fraction of focused SCA's",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_HTF_TOT,          "m_dot_htf_tot",                                                 "The actual flow rate through the field.",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_E_BAL_STARTUP,          "E_bal_startup",                                                                 "Startup energy consumed",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_Q_INC_SF_TOT,           "q_inc_sf_tot",                                                       "Total power incident on the field",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_Q_ABS_TOT,              "q_abs_tot",                                                                   "Total absorbed energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_Q_LOSS_TOT,             "q_loss_tot",                                               "Total receiver thermal and optical losses",          "MWt",             "",             "",             "" },
	// Jan 22, 2014, TFF: renamed "m_dot_htf" (below) to "m_dot_htf2" because it conflicted with an input variable of the same name in "sam_mw_pt_type224.cpp" when retrieving outputs in "cmod_tcstrough_physical.cpp"
	{ TCS_OUTPUT,          TCS_NUMBER,         O_M_DOT_HTF,             "m_dot_htf2",                                                              "Flow rate in a single loop",         "kg/s",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,   O_Q_LOSS_SPEC_TOT,        "q_loss_spec_tot",                        "Field-average receiver thermal losses (convection and radiation)",          "W/m",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_SCA_PAR_TOT,            "SCA_par_tot",                                             "Parasitic electric power consumed by the SC",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_PIPE_HL,                "Pipe_hl",                                     "Pipe heat loss in the hot header and the hot runner",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_Q_DUMP,                 "q_dump",                                                                   "Dumped thermal energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_THETA_AVE,              "Theta_ave",                                                               "Field average theta value",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_COSTH_AVE,              "CosTh_ave",                                                            "Field average costheta value",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_IAM_AVE,                "IAM_ave",                                                  "Field average incidence angle modifier",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_ROWSHADOW_AVE,          "RowShadow_ave",                                                        "Field average row shadowing loss",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_ENDLOSS_AVE,            "EndLoss_ave",                                                                  "Field average end loss",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_DNI_COSTH,              "dni_costh",                                                                             "DNI_x_CosTh",         "W/m2",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_QINC_COSTH,             "qinc_costh",                                                                           "Q_inc_x_CosTh",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_T_LOOP_OUTLET,          "t_loop_outlet",                               "HTF temperature immediately subsequent to the loop outlet",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_C_HTF_AVE,              "c_htf_ave",                                                       "Average solar field specific heat",       "J/kg-K",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER, O_Q_FIELD_DELIVERED,      "q_field_delivered",                                               "Total solar field thermal power delivered",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_ETA_THERMAL,            "eta_thermal",                                          "Solar field thermal efficiency (power out/ANI)",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_E_LOOP_ACCUM,           "E_loop_accum",                               "Accumulated internal energy change rate in the loops ONLY",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_E_HDR_ACCUM,            "E_hdr_accum",                              "Accumulated internal energy change rate in the headers/SGS",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_E_TOT_ACCUM,            "E_tot_accum",                                           "Total accumulated internal energy change rate",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_E_FIELD,                "E_field",                                   "Accumulated internal energy in the entire solar field",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_T_C_IN_CALC,            "T_c_in_calc",                                 "Calculated HTF inlet temp (freeze prot. or stand-alone)",            "C",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER,           O_DEFOCUS,            "defocus_rel",                "Relative defocus for passing back to the controller to force convergence",         "none",             "",             "",             "" },

	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};


class sam_mw_trough_type250 : public tcstypeinterface
{
private:
	HTFProperties htfProps, airProps;
	double pi,Pi,d2r,r2d, g, mtoinch;

	//parameters and inputs
	int nSCA;		//Number of SCA's in a loop
	int nHCEt;		//Number of HCE types
	int nColt;		//Number of collector types
	int nHCEVar;		//Number of HCE variants per type
	int nLoops;		//Number of loops in the field
	double eta_pump;		//HTF pump efficiency
	double HDR_rough;		//Header pipe roughness
	double theta_stow;		//stow angle
	double theta_dep;		//deploy angle
	double Row_Distance;		//Spacing between rows (centerline to centerline)
	int FieldConfig;		//Number of subfield headers
    double T_recirc;        //The temperature which below the field recirculates
	double pb_rated_cap;		//Rated plant capacity
	double m_dot_htfmin;		//Minimum loop HTF flow rate
	double m_dot_htfmax;		//Maximum loop HTF flow rate
	double T_loop_in_des;		//Design loop inlet temperature
	double T_loop_out;		//Target loop outlet temperature
	int Fluid;		//Field HTF fluid number
	double T_field_ini;		//Initial field temperature
    double P_field_in;      //Assumed inlet htf pressure for property lookups
	//double* HTF_data_in;		//Fluid property data
	int nrow_HTF_data,	ncol_HTF_data;
	double T_fp;		//Freeze protection temperature (heat trace activation temperature)
	double I_bn_des;		//Solar irradiation at design
    bool calc_design_pipe_vals; //Calculate temps and pressures at design conditions for runners and headers
    double DP_SGS_1;            //Pressure drop in first section of TES/PB before hot tank at design conditions
    bool SGS_sizing_adjusted;   //Has the field design pressure drop been adjusted for the first section in the TES/PB?
	double V_hdr_cold_max;		//Maximum HTF velocity in the cold headers at design
	double V_hdr_cold_min;		//Minimum HTF velocity in the cold headers at design
    double V_hdr_hot_max;		//Maximum HTF velocity in the hot headers at design
    double V_hdr_hot_min;		//Minimum HTF velocity in the hot headers at design
    int N_max_hdr_diams;        //Maximum number of diameters in each of the hot and cold headers
    double L_rnr_pb;            //Length of runner pipe in power block
    double L_rnr_per_xpan;      //Threshold length of straight runner pipe without an expansion loop
    double L_xpan_hdr;          //Compined perpendicular lengths of each header expansion loop
    double L_xpan_rnr;          //Compined perpendicular lengths of each runner expansion loop
    int Min_rnr_xpans;          //Minimum number of expansion loops per single-diameter runner section
    double northsouth_field_sep; //North/south separation between subfields. 0 = SCAs are touching
    int N_hdr_per_xpan;         //Number of collector loops per expansion loop
    int offset_xpan_hdr;        //Location of first header expansion loop. 1 = after first collector loop
	double Pipe_hl_coef;		//Loss coefficient from the header, runner pipe, and non-HCE piping
	double SCA_drives_elec;		//Tracking power, in Watts per SCA drive
	int fthrok;		//Flag to allow partial defocusing of the collectors
	int fthrctrl;		//Defocusing strategy
	double ColTilt;		//Collector tilt angle (0 is horizontal, 90deg is vertical)
	double ColAz;		//Collector azimuth angle
	
	int accept_mode;		//Acceptance testing mode? (1=yes, 0=no)
	bool accept_init;		//In acceptance testing mode - require steady-state startup
	int accept_loc;			//In acceptance testing mode - temperature sensor location (1=hx,2=loop)
	bool is_using_input_gen;

	double solar_mult;		//Solar multiple
	double mc_bal_hot;		//The heat capacity of the balance of plant on the hot side
	double mc_bal_cold;		//The heat capacity of the balance of plant on the cold side
	double mc_bal_sca;		//Non-HTF heat capacity associated with each SCA - per meter basis
    bool custom_sf_pipe_sizes;  //Use custom solar field pipe diams, wallthks, and lengths

	double* OptCharType;		//The optical characterization method 
	int nval_OptCharType;
	double* CollectorType;		//{1=user defined, 2=LS-2, 3=LS-3, 4=IST} 
	int nval_CollectorType;
	double* W_aperture;		//The collector aperture width (Total structural area.. used for shadowing)
	int nval_W_aperture;
	double* A_aperture;		//Reflective aperture area of the collector
	int nval_A_aperture;
	//double* IamF0;		//Incident angle modifier 0th order term
	//int nval_IamF0;
	//double* IamF1;		//Incident angle modifier 1st order term
	//int nval_IamF1;
	//double* IamF2;		//Incident angle modifier 2nd order term
	//int nval_IamF2;
	double* reflectivity;		//Base solar-weighted mirror reflectivity value 
	int nval_reflectivity;
	double* TrackingError;		//User-defined tracking error derate
	int nval_TrackingError;
	double* GeomEffects;		//User-defined geometry effects derate
	int nval_GeomEffects;
	double* Rho_mirror_clean;		//User-defined clean mirror reflectivity
	int nval_Rho_mirror_clean;
	double* Dirt_mirror;		//User-defined dirt on mirror derate
	int nval_Dirt_mirror;
	double* Error;		//User-defined general optical error derate 
	int nval_Error;
	double* Ave_Focal_Length;		//The average focal length of the collector 
	int nval_Ave_Focal_Length;
	double* L_SCA;		//The length of the SCA 
	int nval_L_SCA;
	double* L_aperture;		//The length of a single mirror/HCE unit
	int nval_L_aperture;
	double* ColperSCA;		//The number of individual collector sections in an SCA 
	int nval_ColperSCA;
	double* Distance_SCA;		// piping distance between SCA's in the field
	int nval_Distance_SCA;

	double* HCE_FieldFrac_in;		//The fraction of the field occupied by this HCE type 
	int nrow_HCE_FieldFrac,	ncol_HCE_FieldFrac;
	double* D_2_in;		//The inner absorber tube diameter
	int nrow_D_2,	ncol_D_2;
	double* D_3_in;		//The outer absorber tube diameter
	int nrow_D_3,	ncol_D_3;
	double* D_4_in;		//The inner glass envelope diameter 
	int nrow_D_4,	ncol_D_4;
	double* D_5_in;		//The outer glass envelope diameter 
	int nrow_D_5,	ncol_D_5;
	double* D_p_in;		//The diameter of the absorber flow plug (optional) 
	int nrow_D_p,	ncol_D_p;
	double* Flow_type_in;		//The flow type through the absorber
	int nrow_Flow_type,	ncol_Flow_type;
	double* Rough_in;		//Roughness of the internal surface 
	int nrow_Rough,	ncol_Rough;
	double* alpha_env_in;		//Envelope absorptance 
	int nrow_alpha_env,	ncol_alpha_env;
	double* epsilon_3_11_in;		//Absorber emittance - HCE type 1 - HCE variation 1
	int nrow_epsilon_3_11,	ncol_epsilon_3_11;
	double* epsilon_3_12_in;		//Absorber emittance - HCE type 1 - HCE variation 2
	int nrow_epsilon_3_12,	ncol_epsilon_3_12;
	double* epsilon_3_13_in;		//Absorber emittance - HCE type 1 - HCE variation 3
	int nrow_epsilon_3_13,	ncol_epsilon_3_13;
	double* epsilon_3_14_in;		//Absorber emittance - HCE type 1 - HCE variation 4
	int nrow_epsilon_3_14,	ncol_epsilon_3_14;
	double* epsilon_3_21_in;		//Absorber emittance - HCE type 2 - HCE variation 1
	int nrow_epsilon_3_21,	ncol_epsilon_3_21;
	double* epsilon_3_22_in;		//Absorber emittance - HCE type 2 - HCE variation 2
	int nrow_epsilon_3_22,	ncol_epsilon_3_22;
	double* epsilon_3_23_in;		//Absorber emittance - HCE type 2 - HCE variation 3
	int nrow_epsilon_3_23,	ncol_epsilon_3_23;
	double* epsilon_3_24_in;		//Absorber emittance - HCE type 2 - HCE variation 4
	int nrow_epsilon_3_24,	ncol_epsilon_3_24;
	double* epsilon_3_31_in;		//Absorber emittance - HCE type 3 - HCE variation 1
	int nrow_epsilon_3_31,	ncol_epsilon_3_31;
	double* epsilon_3_32_in;		//Absorber emittance - HCE type 3 - HCE variation 2
	int nrow_epsilon_3_32,	ncol_epsilon_3_32;
	double* epsilon_3_33_in;		//Absorber emittance - HCE type 3 - HCE variation 3
	int nrow_epsilon_3_33,	ncol_epsilon_3_33;
	double* epsilon_3_34_in;		//Absorber emittance - HCE type 3 - HCE variation 4
	int nrow_epsilon_3_34,	ncol_epsilon_3_34;
	double* epsilon_3_41_in;		//Absorber emittance - HCE type 4 - HCE variation 1
	int nrow_epsilon_3_41,	ncol_epsilon_3_41;
	double* epsilon_3_42_in;		//Absorber emittance - HCE type 4 - HCE variation 2
	int nrow_epsilon_3_42,	ncol_epsilon_3_42;
	double* epsilon_3_43_in;		//Absorber emittance - HCE type 4 - HCE variation 3
	int nrow_epsilon_3_43,	ncol_epsilon_3_43;
	double* epsilon_3_44_in;		//Absorber emittance - HCE type 4 - HCE variation 4
	int nrow_epsilon_3_44,	ncol_epsilon_3_44;
	double* alpha_abs_in;		//Absorber absorptance 
	int nrow_alpha_abs,	ncol_alpha_abs;
	double* Tau_envelope_in;		//Envelope transmittance
	int nrow_Tau_envelope,	ncol_Tau_envelope;
	double* EPSILON_4_in;		//Inner glass envelope emissivities (Pyrex) 
	int nrow_EPSILON_4,	ncol_EPSILON_4;
	double* EPSILON_5_in;		//Outer glass envelope emissivities (Pyrex) 
	int nrow_EPSILON_5,	ncol_EPSILON_5;
	double* GlazingIntactIn_in;		//The glazing intact flag {1=true, else=false}
	int nrow_GlazingIntactIn,	ncol_GlazingIntactIn;
	double* P_a_in;		//Annulus gas pressure
	int nrow_P_a,	ncol_P_a;
	double* AnnulusGas_in;		//Annulus gas type (1=air, 26=Ar, 27=H2)
	int nrow_AnnulusGas,	ncol_AnnulusGas;
	double* AbsorberMaterial_in;		//Absorber material type
	int nrow_AbsorberMaterial,	ncol_AbsorberMaterial;
	double* Shadowing_in;		//Receiver bellows shadowing loss factor
	int nrow_Shadowing,	ncol_Shadowing;
	double* Dirt_HCE_in;		//Loss due to dirt on the receiver envelope
	int nrow_Dirt_HCE,	ncol_Dirt_HCE;
	double* Design_loss_in;		//Receiver heat loss at design
	int nrow_Design_loss,	ncol_Design_loss;

	double* SCAInfoArray_in;		//(:,1) = HCE type, (:,2)= Collector type for each SCA in the loop 
	int nrow_SCAInfoArray,	ncol_SCAInfoArray;
	double* SCADefocusArray;		//Order in which the SCA's should be defocused
	int nval_SCADefocusArray;

    double* K_cpnt_in;         // Interconnect component minor loss coefficients, row=intc, col=component
    int nrow_K_cpnt, ncol_K_cpnt;
    double* D_cpnt_in;         // Interconnect component diameters, row=intc, col=component
    int nrow_D_cpnt, ncol_D_cpnt;
    double* L_cpnt_in;         // Interconnect component lengths, row=intc, col=component
    int nrow_L_cpnt, ncol_L_cpnt;
    double* Type_cpnt_in;        // Interconnect component type, row=intc, col=component
    int nrow_Type_cpnt, ncol_Type_cpnt;
    IntcOutputs inlet_state, crossover_state, outlet_state, intc_state;

    double* sf_rnr_diams;       // Custom runner diameters
    int nval_sf_rnr_diams;
    double* sf_rnr_wallthicks;  // Custom runner wall thicknesses
    int nval_sf_rnr_wallthicks;
    double* sf_rnr_lengths;     // Custom runner lengths
    int nval_sf_rnr_lengths;
    double* sf_hdr_diams;       // Custom header diameters
    int nval_sf_hdr_diams;
    double* sf_hdr_wallthicks;  // Custom header wall thicknesses
    int nval_sf_hdr_wallthicks;
    double* sf_hdr_lengths;     // Custom header lengths
    int nval_sf_hdr_lengths;

	double I_b;		//Direct normal incident solar irradiation
	double T_db;		//Dry bulb air temperature
	double V_wind;		//Ambient windspeed 
	double P_amb;		//Ambient pressure
	double T_dp;		//The dewpoint temperature
	double T_cold_in;		//HTF return temperature
	double m_dot_in;		//HTF mass flow rate at the inlet 
	double defocus;		//Defocus control
    bool recirculating; // Field recirculating (bypass valve open)
	double SolarAz;		//Solar azimuth angle reported by the Type15 weather file
	double latitude;		//Site latitude read from weather file
	double longitude;		//Site longitude read from weather file
	//double timezone;		//Time zone

	double T_sys_h;		//Solar field HTF outlet temperature
	double m_dot_avail;		//HTF mass flow rate from the field
    double m_dot_field_htf;  //HTF mass flow rate from the field, including when recirculating
	double q_avail;		//Thermal power produced by the field
	double DP_tot;		//Total HTF pressure drop
	double W_dot_pump;		//Required solar field pumping power
	double E_fp_tot;		//Freeze protection energy
	int qq;		//Number of iterations required to solve
	double T_sys_c;		//Collector inlet temperature
	double EqOpteff;		//Collector equivalent optical efficiency
	double SCAs_def;		//The fraction of focused SCA's
	double m_dot_htf_tot;		//The actual flow rate through the field..
	double E_bal_startup;		//Startup energy consumed
	double q_inc_sf_tot;		//Total power incident on the field
	double q_abs_tot;		//Total absorbed energy
	double q_loss_tot;		//Total receiver thermal and optical losses
	double m_dot_htf;		//Flow rate in a single loop
	double q_loss_spec_tot;		//Field-average receiver thermal losses (convection and radiation)
	double SCA_par_tot;		//Parasitic electric power consumed by the SC
	double Pipe_hl;		//Pipe heat loss in the hot header and the hot runner
	double q_dump;		//Dumped thermal energy
	double Theta_ave;		//Field average theta value
	double CosTh_ave;		//Field average costheta value
	double IAM_ave;		//Field average incidence angle modifier
	double RowShadow_ave;		//Field average row shadowing loss
	double EndLoss_ave;		//Field average end loss
	double dni_costh;		//DNI_x_CosTh
	double qinc_costh;		//Q_inc_x_CosTh
	double t_loop_outlet;		//HTF temperature immediately subsequent to the loop outlet
	double c_htf_ave;		//Average solar field specific heat
	double q_field_delivered;		//Total solar field thermal power delivered
	double eta_thermal;		//Solar field thermal efficiency (power out/ANI)
	double E_loop_accum;		//Accumulated internal energy change rate in the loops ONLY
	double E_hdr_accum;		//Accumulated internal energy change rate in the headers/SGS
	double E_tot_accum;		//Total accumulated internal energy change rate
	double E_field;		//Accumulated internal energy in the entire solar field


	util::matrix_t<double> HCE_FieldFrac, D_2, D_3, D_4, D_5, D_p, Flow_type, Rough, alpha_env, epsilon_3_11, epsilon_3_12, 
		epsilon_3_13, epsilon_3_14, epsilon_3_21, epsilon_3_22, epsilon_3_23, epsilon_3_24, epsilon_3_31, epsilon_3_32, epsilon_3_33, 
		epsilon_3_34, epsilon_3_41, epsilon_3_42, epsilon_3_43, epsilon_3_44, alpha_abs, Tau_envelope, EPSILON_4, EPSILON_5, 
		GlazingIntactIn, P_a, AnnulusGas, AbsorberMaterial, Shadowing, Dirt_HCE, Design_loss, SCAInfoArray, K_cpnt, D_cpnt, L_cpnt, Type_cpnt,
        rough_cpnt, u_cpnt, mc_cpnt;
	
    vector<interconnect> interconnects;

	util::matrix_t<double> IAM_matrix;
	//int n_c_iam_matrix = 0;
	//int n_r_iam_matrix = 0;
	int n_c_iam_matrix;
	int n_r_iam_matrix;

	//Declare variables that require storage from step to step
	double 
		Ap_tot, //Total aperture area m2
		L_tot,	//Total length of a collector row [m]
		opteff_des,	//Solar field optical efficiency at design
		m_dot_design,	//Solar field mass flow rate at design [kg/s]
		q_design;	//Design thermal power from the solar field [Wt]

	int 
		nfsec,	//Number of field sections
		nhdrsec,	//Number of header sections
		nrunsec;	//Number of unique runner diameters
	//Other matrices
	util::matrix_t<HTFProperties*> AnnulusGasMat;
	util::matrix_t<AbsorberProps*> AbsorberPropMat;
	util::matrix_t<double> L_actSCA, A_cs, D_h, ColOptEff /*nColt, nSCA*/;
	util::matrix_t<bool> GlazingIntact;
	emit_table epsilon_3;
    util::matrix_t<double> D_runner, WallThk_runner, L_runner, m_dot_rnr_dsn, V_rnr_dsn, N_rnr_xpans, DP_rnr, T_rnr, P_rnr,
        D_hdr, WallThk_hdr, L_hdr, m_dot_hdr_dsn, V_hdr_dsn, N_hdr_xpans, DP_hdr, T_hdr, P_hdr,
        DP_intc, P_intc, DP_loop, T_loop, P_loop,
        T_rnr_des_out, P_rnr_des_out, T_hdr_des_out, P_hdr_des_out, T_loop_des_out, P_loop_des_out;

	util::matrix_t<double> 
		T_htf_in, T_htf_out, T_htf_ave, q_loss, q_abs, c_htf, rho_htf, DP_tube, E_abs_field, 
		E_int_loop, E_accum, E_avail, E_abs_max,v_1,q_loss_SCAtot, q_abs_SCAtot, q_SCA, T_htf_in0, T_htf_out0, 
		T_htf_ave0, E_fp, q_1abs_tot, q_1abs, q_i, IAM, EndGain, EndLoss, RowShadow;
	double T_sys_c_last, T_sys_h_last; //stored values for header thermal inertia calculations
	double v_hot, v_cold;	//Header HTF volume
	double defocus_new, defocus_old, ftrack;
	bool 
		no_fp,	//Freeze protection flag
		is_fieldgeom_init;	//Flag to indicate whether the field geometry has been initialized
	double T_cold_in_1, c_hdr_cold, start_time, dt, SolarAlt, costh, theta, shift,
		q_SCA_tot, m_dot_htfX, Header_hl_cold, Header_hl_cold_tot, Runner_hl_cold, Runner_hl_cold_tot, Pipe_hl_cold, T_loop_in,
		T_loop_outX, Runner_hl_hot, Runner_hl_hot_tot, Header_hl_hot, Header_hl_hot_tot, Pipe_hl_hot, Intc_hl, c_hdr_hot, time_hr, dt_hr;
	int day_of_year, SolveMode, dfcount;

	double ncall_track;

	double T_save[5];
	std::vector<double> reguess_args;
	
	double hour, T_sky;

	double m_htf_prop_min;

	bool ss_init_complete;

public:

	sam_mw_trough_type250( tcscontext *cxt, tcstypeinfo *ti ) 
		: tcstypeinterface(cxt, ti)
	{
		//Commonly used values, conversions, etc...
		Pi = acos(-1.);
		pi = Pi;
		r2d = 180./pi;
		d2r = pi/180.;
		g = 9.81;	//gravitation constant
		mtoinch = 39.3700787;	//[m] -> [in]

		//Set all values to NaN or nonsense value to prevent misuse
		nSCA	= -1;
		nHCEt	= -1;
		nColt	= -1;
		nHCEVar	= -1;
		nLoops	= -1;
		eta_pump	= std::numeric_limits<double>::quiet_NaN();
		HDR_rough	= std::numeric_limits<double>::quiet_NaN();
		theta_stow	= std::numeric_limits<double>::quiet_NaN();
		theta_dep	= std::numeric_limits<double>::quiet_NaN();
		Row_Distance	= std::numeric_limits<double>::quiet_NaN();
		FieldConfig	= -1;
        T_recirc        = std::numeric_limits<double>::quiet_NaN();
		pb_rated_cap	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htfmin	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htfmax	= std::numeric_limits<double>::quiet_NaN();
		T_loop_in_des	= std::numeric_limits<double>::quiet_NaN();
		T_loop_out	= std::numeric_limits<double>::quiet_NaN();
		Fluid	= -1;
		T_field_ini	= std::numeric_limits<double>::quiet_NaN();
        P_field_in = std::numeric_limits<double>::quiet_NaN();
		
		nrow_HTF_data = -1, ncol_HTF_data = -1;
		T_fp	= std::numeric_limits<double>::quiet_NaN();
		I_bn_des	= std::numeric_limits<double>::quiet_NaN();
        calc_design_pipe_vals = false;
        DP_SGS_1 = std::numeric_limits<double>::quiet_NaN();
        SGS_sizing_adjusted = false;
		V_hdr_cold_max	= std::numeric_limits<double>::quiet_NaN();
		V_hdr_cold_min	= std::numeric_limits<double>::quiet_NaN();
        V_hdr_hot_max = std::numeric_limits<double>::quiet_NaN();
        V_hdr_hot_min = std::numeric_limits<double>::quiet_NaN();
        N_max_hdr_diams = -1;
        L_rnr_pb = std::numeric_limits<double>::quiet_NaN();
        L_rnr_per_xpan = std::numeric_limits<double>::quiet_NaN();
        L_xpan_hdr = std::numeric_limits<double>::quiet_NaN();
        L_xpan_rnr = std::numeric_limits<double>::quiet_NaN();
        Min_rnr_xpans = -1;
        northsouth_field_sep = std::numeric_limits<double>::quiet_NaN();
        N_hdr_per_xpan = -1;
        offset_xpan_hdr = -1;
		Pipe_hl_coef	= std::numeric_limits<double>::quiet_NaN();
		SCA_drives_elec	= std::numeric_limits<double>::quiet_NaN();
		fthrok	= -1;
		fthrctrl	= -1;
		ColTilt	= std::numeric_limits<double>::quiet_NaN();
		ColAz	= std::numeric_limits<double>::quiet_NaN();
		
		accept_mode	= -1;
		accept_init	= false;
		accept_loc	= -1;
		is_using_input_gen = false;

        calc_design_pipe_vals = true;

		solar_mult	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_hot	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_cold	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_sca	= std::numeric_limits<double>::quiet_NaN();
        custom_sf_pipe_sizes = false;
		OptCharType	= NULL;
		nval_OptCharType = -1;
		CollectorType	= NULL;
		nval_CollectorType = -1;
		W_aperture	= NULL;
		nval_W_aperture = -1;
		A_aperture	= NULL;
		nval_A_aperture = -1;
		//IamF0	= NULL;
		//nval_IamF0 = -1;
		//IamF1	= NULL;
		//nval_IamF1 = -1;
		//IamF2	= NULL;
		//nval_IamF2 = -1;

		n_c_iam_matrix = -1;
		n_r_iam_matrix = -1;

		reflectivity	= NULL;
		nval_reflectivity = -1;
		TrackingError	= NULL;
		nval_TrackingError = -1;
		GeomEffects	= NULL;
		nval_GeomEffects = -1;
		Rho_mirror_clean	= NULL;
		nval_Rho_mirror_clean = -1;
		Dirt_mirror	= NULL;
		nval_Dirt_mirror = -1;
		Error	= NULL;
		nval_Error = -1;
		Ave_Focal_Length	= NULL;
		nval_Ave_Focal_Length = -1;
		L_SCA	= NULL;
		nval_L_SCA = -1;
		L_aperture	= NULL;
		nval_L_aperture = -1;
		ColperSCA	= NULL;
		nval_ColperSCA = -1;
		Distance_SCA	= NULL;
		nval_Distance_SCA = -1;
		HCE_FieldFrac_in	= NULL;
		nrow_HCE_FieldFrac = -1, ncol_HCE_FieldFrac = -1;
		D_2_in	= NULL;
		nrow_D_2 = -1, ncol_D_2 = -1;
		D_3_in	= NULL;
		nrow_D_3 = -1, ncol_D_3 = -1;
		D_4_in	= NULL;
		nrow_D_4 = -1, ncol_D_4 = -1;
		D_5_in	= NULL;
		nrow_D_5 = -1, ncol_D_5 = -1;
		D_p_in	= NULL;
		nrow_D_p = -1, ncol_D_p = -1;
		Flow_type_in	= NULL;
		nrow_Flow_type = -1, ncol_Flow_type = -1;
		Rough_in	= NULL;
		nrow_Rough = -1, ncol_Rough = -1;
		alpha_env_in	= NULL;
		nrow_alpha_env = -1, ncol_alpha_env = -1;
		epsilon_3_11_in	= NULL;
		nrow_epsilon_3_11 = -1, ncol_epsilon_3_11 = -1;
		epsilon_3_12_in	= NULL;
		nrow_epsilon_3_12 = -1, ncol_epsilon_3_12 = -1;
		epsilon_3_13_in	= NULL;
		nrow_epsilon_3_13 = -1, ncol_epsilon_3_13 = -1;
		epsilon_3_14_in	= NULL;
		nrow_epsilon_3_14 = -1, ncol_epsilon_3_14 = -1;
		epsilon_3_21_in	= NULL;
		nrow_epsilon_3_21 = -1, ncol_epsilon_3_21 = -1;
		epsilon_3_22_in	= NULL;
		nrow_epsilon_3_22 = -1, ncol_epsilon_3_22 = -1;
		epsilon_3_23_in	= NULL;
		nrow_epsilon_3_23 = -1, ncol_epsilon_3_23 = -1;
		epsilon_3_24_in	= NULL;
		nrow_epsilon_3_24 = -1, ncol_epsilon_3_24 = -1;
		epsilon_3_31_in	= NULL;
		nrow_epsilon_3_31 = -1, ncol_epsilon_3_31 = -1;
		epsilon_3_32_in	= NULL;
		nrow_epsilon_3_32 = -1, ncol_epsilon_3_32 = -1;
		epsilon_3_33_in	= NULL;
		nrow_epsilon_3_33 = -1, ncol_epsilon_3_33 = -1;
		epsilon_3_34_in	= NULL;
		nrow_epsilon_3_34 = -1, ncol_epsilon_3_34 = -1;
		epsilon_3_41_in	= NULL;
		nrow_epsilon_3_41 = -1, ncol_epsilon_3_41 = -1;
		epsilon_3_42_in	= NULL;
		nrow_epsilon_3_42 = -1, ncol_epsilon_3_42 = -1;
		epsilon_3_43_in	= NULL;
		nrow_epsilon_3_43 = -1, ncol_epsilon_3_43 = -1;
		epsilon_3_44_in	= NULL;
		nrow_epsilon_3_44 = -1, ncol_epsilon_3_44 = -1;
		alpha_abs_in	= NULL;
		nrow_alpha_abs = -1, ncol_alpha_abs = -1;
		Tau_envelope_in	= NULL;
		nrow_Tau_envelope = -1, ncol_Tau_envelope = -1;
		EPSILON_4_in	= NULL;
		nrow_EPSILON_4 = -1, ncol_EPSILON_4 = -1;
		EPSILON_5_in	= NULL;
		nrow_EPSILON_5 = -1, ncol_EPSILON_5 = -1;
		GlazingIntactIn_in	= NULL;
		nrow_GlazingIntactIn = -1, ncol_GlazingIntactIn = -1;
		P_a_in	= NULL;
		nrow_P_a = -1, ncol_P_a = -1;
		AnnulusGas_in	= NULL;
		nrow_AnnulusGas = -1, ncol_AnnulusGas = -1;
		AbsorberMaterial_in	= NULL;
		nrow_AbsorberMaterial = -1, ncol_AbsorberMaterial = -1;
		Shadowing_in	= NULL;
		nrow_Shadowing = -1, ncol_Shadowing = -1;
		Dirt_HCE_in	= NULL;
		nrow_Dirt_HCE = -1, ncol_Dirt_HCE = -1;
		Design_loss_in	= NULL;
		nrow_Design_loss = -1, ncol_Design_loss = -1;
		SCAInfoArray_in	= NULL;
		nrow_SCAInfoArray = -1, ncol_SCAInfoArray = -1;
		SCADefocusArray	= NULL;
		nval_SCADefocusArray = -1;
        K_cpnt_in = NULL;
        nrow_K_cpnt = -1, ncol_K_cpnt = -1;
        D_cpnt_in = NULL;
        nrow_D_cpnt = -1, ncol_D_cpnt = -1;
        L_cpnt_in = NULL;
        nrow_L_cpnt = -1, ncol_L_cpnt = -1;
        Type_cpnt_in = NULL;
        nrow_Type_cpnt = -1, ncol_Type_cpnt = -1;
        sf_rnr_diams = NULL;
        nval_sf_rnr_diams = -1;
        sf_rnr_wallthicks = NULL;
        nval_sf_rnr_wallthicks = -1;
        sf_rnr_lengths = NULL;
        nval_sf_rnr_lengths = -1;
        sf_hdr_diams = NULL;
        nval_sf_hdr_diams = -1;
        sf_hdr_wallthicks = NULL;
        nval_sf_hdr_wallthicks = -1;
        sf_hdr_lengths = NULL;
        nval_sf_hdr_lengths = -1;
		I_b	= std::numeric_limits<double>::quiet_NaN();
		T_db	= std::numeric_limits<double>::quiet_NaN();
		V_wind	= std::numeric_limits<double>::quiet_NaN();
		P_amb	= std::numeric_limits<double>::quiet_NaN();
		T_dp	= std::numeric_limits<double>::quiet_NaN();
		T_cold_in	= std::numeric_limits<double>::quiet_NaN();
		m_dot_in	= std::numeric_limits<double>::quiet_NaN();
		defocus	= std::numeric_limits<double>::quiet_NaN();
        recirculating = false;
		SolarAz	= std::numeric_limits<double>::quiet_NaN();
		latitude = std::numeric_limits<double>::quiet_NaN();
		longitude = std::numeric_limits<double>::quiet_NaN();
		//timezone = std::numeric_limits<double>::quiet_NaN();
		T_sys_h	= std::numeric_limits<double>::quiet_NaN();
		m_dot_avail	= std::numeric_limits<double>::quiet_NaN();
        m_dot_field_htf = std::numeric_limits<double>::quiet_NaN();
		q_avail	= std::numeric_limits<double>::quiet_NaN();
		DP_tot	= std::numeric_limits<double>::quiet_NaN();
		W_dot_pump	= std::numeric_limits<double>::quiet_NaN();
		E_fp_tot	= std::numeric_limits<double>::quiet_NaN();
		qq	= -1;
		T_sys_c	= std::numeric_limits<double>::quiet_NaN();
		EqOpteff	= std::numeric_limits<double>::quiet_NaN();
		SCAs_def	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf_tot	= std::numeric_limits<double>::quiet_NaN();
		E_bal_startup	= std::numeric_limits<double>::quiet_NaN();
		q_inc_sf_tot	= std::numeric_limits<double>::quiet_NaN();
		q_abs_tot	= std::numeric_limits<double>::quiet_NaN();
		q_loss_tot	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf	= std::numeric_limits<double>::quiet_NaN();
		q_loss_spec_tot	= std::numeric_limits<double>::quiet_NaN();
		SCA_par_tot	= std::numeric_limits<double>::quiet_NaN();
		Pipe_hl	= std::numeric_limits<double>::quiet_NaN();
		q_dump	= std::numeric_limits<double>::quiet_NaN();
		Theta_ave	= std::numeric_limits<double>::quiet_NaN();
		CosTh_ave	= std::numeric_limits<double>::quiet_NaN();
		IAM_ave	= std::numeric_limits<double>::quiet_NaN();
		RowShadow_ave	= std::numeric_limits<double>::quiet_NaN();
		EndLoss_ave	= std::numeric_limits<double>::quiet_NaN();
		dni_costh	= std::numeric_limits<double>::quiet_NaN();
		qinc_costh	= std::numeric_limits<double>::quiet_NaN();
		t_loop_outlet	= std::numeric_limits<double>::quiet_NaN();
		c_htf_ave	= std::numeric_limits<double>::quiet_NaN();
		q_field_delivered	= std::numeric_limits<double>::quiet_NaN();
		eta_thermal	= std::numeric_limits<double>::quiet_NaN();
		E_loop_accum	= std::numeric_limits<double>::quiet_NaN();
		E_hdr_accum	= std::numeric_limits<double>::quiet_NaN();
		E_tot_accum	= std::numeric_limits<double>::quiet_NaN();
		E_field	= std::numeric_limits<double>::quiet_NaN();

		hour = T_sky = std::numeric_limits<double>::quiet_NaN();

		for( int i = 0; i < 5; i++ )
			T_save[i] = std::numeric_limits<double>::quiet_NaN();

		reguess_args.resize(3);
		std::fill(reguess_args.begin(), reguess_args.end(), std::numeric_limits<double>::quiet_NaN());

		m_htf_prop_min = std::numeric_limits<double>::quiet_NaN();

		AnnulusGasMat.fill(NULL);
		AbsorberPropMat.fill(NULL);
	}

	virtual ~sam_mw_trough_type250()
	{
		for( int i = 0; i < (int)AbsorberPropMat.nrows(); i ++ )
		{
			for( int j = 0; j < (int)AbsorberPropMat.ncols(); j++ )
			{
				if( AbsorberPropMat(i, j) != NULL )
					delete AbsorberPropMat(i, j);
			}
		}
		
		for( int i = 0; i < (int)AnnulusGasMat.nrows(); i++ )
		{
			for( int j = 0; j < (int)AnnulusGasMat.ncols(); j++ )
			{
				if( AnnulusGasMat(i, j) != NULL )
					delete AnnulusGasMat(i, j);
			}
		}
	}

	virtual int init(){
		/*
		--Initialization call-- 
		
		Do any setup required here.
		Get the values of the inputs and parameters
		*/

		m_htf_prop_min = 275.0;

		dt = time_step();
		start_time = -1; 

		//Initialize air properties -- used in reeiver calcs
		airProps.SetFluid( HTFProperties::Air );

		//Get fluid properties
		Fluid = (int) value(P_FLUID);
		if(Fluid != HTFProperties::User_defined )
		{
			if( !htfProps.SetFluid( Fluid ) )
			{
				message(TCS_ERROR, "Field HTF code is not recognized");
				return -1;
			}
		}
		else if( Fluid == HTFProperties::User_defined )
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value(P_FIELD_FL_PROPS, &nrows, &ncols);
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r, c) = TCS_MATRIX_INDEX(var(P_FIELD_FL_PROPS), r, c);

				if ( !htfProps.SetUserDefinedFluid( mat ) )
				{
					message( TCS_ERROR, htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}
		else
		{
			message(TCS_ERROR, "Field HTF code is not recognized");
			return -1;
		}

		//Get values for any parameters here
		nSCA = (int)value(P_NSCA);		//Number of SCA's in a loop [none]
		nHCEt = (int)value(P_NHCET);		//Number of HCE types [none]
		nColt = (int)value(P_NCOLT);		//Number of collector types [none]
		nHCEVar = (int)value(P_NHCEVAR);		//Number of HCE variants per type [none]
		nLoops = (int)value(P_NLOOPS);		//Number of loops in the field [none]
		eta_pump = value(P_ETA_PUMP);		//HTF pump efficiency [none]
		HDR_rough = value(P_HDR_ROUGH);		//Header pipe roughness [m]
		theta_stow = value(P_THETA_STOW);		//stow angle [deg]
		theta_dep = value(P_THETA_DEP);		//deploy angle [deg]
		Row_Distance = value(P_ROW_DISTANCE);		//Spacing between rows (centerline to centerline) [m]
		FieldConfig = (int)value(P_FIELDCONFIG);		//Number of subfield headers [none]
        T_recirc = value(P_T_RECIRC);               //The temperature which below the field recirculates
		pb_rated_cap = value(P_PB_RATED_CAP);		//Rated plant capacity [MWe]
		m_dot_htfmin = value(P_M_DOT_HTFMIN);		//Minimum loop HTF flow rate [kg/s]
		m_dot_htfmax = value(P_M_DOT_HTFMAX);		//Maximum loop HTF flow rate [kg/s]
		T_loop_in_des = value(P_T_LOOP_IN_DES);		//Design loop inlet temperature [C]
		T_loop_out = value(P_T_LOOP_OUT);		//Target loop outlet temperature [C]
		Fluid = (int)value(P_FLUID);		//Field HTF fluid number [none]
		
		
		
		T_field_ini = value(P_T_FIELD_INI);		//Initial field temperature [C]
        // TODO: Need to replace this with the calculated total pressure drop in the system (+ 1 atm)
        P_field_in = 17 / 1.e-5;                //Assumed inlet htf pressure for property lookups (DP_tot_max = 16 bar + 1 atm) [Pa]
		
		T_field_ini = 0.5*(T_field_ini + T_loop_in_des);
		
		
		T_fp = value(P_T_FP);		//Freeze protection temperature (heat trace activation temperature) [C]
		I_bn_des = value(P_I_BN_DES);		//Solar irradiation at design [W/m2]
        calc_design_pipe_vals = value(P_DES_PIPE_VALS); //Calculate temps and pressures at design conditions for runners and headers
        value(P_DP_SGS_1, -1);                      //Set this to a negative value for signalling that it has not yet been set
		V_hdr_cold_max = value(P_V_HDR_COLD_MAX);	//Maximum HTF velocity in the cold headers at design [m/s]
		V_hdr_cold_min = value(P_V_HDR_COLD_MIN);	//Minimum HTF velocity in the cold headers at design [m/s]
        V_hdr_hot_max = value(P_V_HDR_HOT_MAX);		//Maximum HTF velocity in the hot headers at design [m/s]
        V_hdr_hot_min = value(P_V_HDR_HOT_MIN);		//Minimum HTF velocity in the hot headers at design [m/s]
        N_max_hdr_diams = (int)value(P_NMAX_HDR_DIAMS);  //Maximum number of diameters in each of the hot and cold headers
        L_rnr_pb = value(P_L_RNR_PB);               //Length of runner pipe in power block
        L_rnr_per_xpan = value(P_L_RNR_PER_XPAN);   //Threshold length of straight runner pipe without an expansion loop
        L_xpan_hdr = value(P_L_XPAN_HDR);           //Combined perpendicular lengths of each header expansion loop
        L_xpan_rnr = value(P_L_XPAN_RNR);           //Combined perpendicular lengths of each runner expansion loop
        Min_rnr_xpans = (int)value(P_MIN_RNR_XPANS);     //Minimum number of expansion loops per single-diameter runner section
        northsouth_field_sep = value(P_NTHSTH_FIELD_SEP); //North/south separation between subfields. 0 = SCAs are touching
        N_hdr_per_xpan = (int)value(P_NHDR_PER_XPAN);         //Number of collector loops per expansion loop
        offset_xpan_hdr = (int)value(P_OFFSET_XPAN_HDR);        //Location of first header expansion loop. 1 = after first collector loop
		Pipe_hl_coef = value(P_PIPE_HL_COEF);		//Loss coefficient from the header, runner pipe, and non-HCE piping [W/m2-K]
		SCA_drives_elec = value(P_SCA_DRIVES_ELEC);		//Tracking power, in Watts per SCA drive [W/SCA]
		fthrok = (int)value(P_FTHROK);		//Flag to allow partial defocusing of the collectors [none]
		fthrctrl = (int)value(P_FTHRCTRL);		//Defocusing strategy [none]
		ColTilt = value(P_COLTILT)*d2r;		//Collector tilt angle (0 is horizontal, 90deg is vertical) [deg]
		ColAz = value(P_COLAZ)*d2r;		//Collector azimuth angle [deg]
		
		accept_mode = (int)value(P_ACCEPT_MODE);				// Acceptance testing mode? (1=yes, 0=no) [none]
		accept_init = value(P_ACCEPT_INIT) == 1;				// In acceptance testing mode - require steady-state startup [none]
		accept_loc = (int)value(P_ACCEPT_LOC);					// In acceptance testing mode - temperature sensor location (1=hx,2=loop) [none]
		is_using_input_gen = (value(P_USING_INPUT_GEN)>0);	// Is model getting inputs from input generator (true) or from other components in physical trough SYSTEM model (false)

		solar_mult = value(P_SOLAR_MULT);		//Solar multiple [none]
		mc_bal_hot = value(P_MC_BAL_HOT);		//The heat capacity of the balance of plant on the hot side [kWht/K-MWt]
		mc_bal_cold = value(P_MC_BAL_COLD);		//The heat capacity of the balance of plant on the cold side [kWht/K-MWt]
		mc_bal_sca = value(P_MC_BAL_SCA);		//Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]
        custom_sf_pipe_sizes = (bool)value(P_CUSTOM_SF_PIPE_SIZES);         //Use custom solar field pipe diams, wallthks, and lengths

		OptCharType = value(P_OPTCHARTYPE, &nval_OptCharType);		//The optical characterization method  [none]
		CollectorType = value(P_COLLECTORTYPE, &nval_CollectorType);		//{1=user defined, 2=LS-2, 3=LS-3, 4=IST}  [none]
		W_aperture = value(P_W_APERTURE, &nval_W_aperture);		//The collector aperture width (Total structural area.. used for shadowing) [m]
		A_aperture = value(P_A_APERTURE, &nval_A_aperture);		//Reflective aperture area of the collector [m2]
		//IamF0 = value(P_IAMF0, &nval_IamF0);		//Incident angle modifier 0th order term [none]
		//IamF1 = value(P_IAMF1, &nval_IamF1);		//Incident angle modifier 1st order term [none]
		//IamF2 = value(P_IAMF2, &nval_IamF2);		//Incident angle modifier 2nd order term [none]
		
		// Check IAM matrix against number of collectors: nColt

		n_c_iam_matrix = -1;
		n_r_iam_matrix = -1;
		double *p_iam_matrix = value(P_IAM_MATRIX, &n_r_iam_matrix, &n_c_iam_matrix);
		
		if( n_c_iam_matrix < 3 )
		{
			message(TCS_ERROR, "There must be at least 3 incident angle modifier coefficients");
			return -1;
		}

		if( n_r_iam_matrix < nColt )
		{
			message(TCS_ERROR, "The number of groups of IAM coefficients (%d) is less than the number of collector types in this simulation (%d)", n_r_iam_matrix, nColt);
			return -1;
		}

		// Load p_iam_matrix in matrix_t
		IAM_matrix.resize_fill(n_r_iam_matrix, n_c_iam_matrix, 0.0);
		for( int r = 0; r < n_r_iam_matrix; r++ )
		{
			for( int c = 0; c < n_c_iam_matrix; c++ )
			{
				IAM_matrix(r, c) = TCS_MATRIX_INDEX(var(P_IAM_MATRIX), r, c);
			}
		}

		// Check that for each collector, at least 3 coefficients are != 0.0
		for( int i = 0; i < nColt; i++ )
		{
			for( int j = 0; j < 3; j++ )
			{
				if(IAM_matrix(i,j)==0.0)
				{
					message(TCS_ERROR, "For %d collectors and groups of IAM coefficients, each group of IAM coefficients must begin with at least 3 non-zero values. There are only %d non-zero coefficients for collector %d", nColt, j, i+1);
					return -1;
				}
			}
		}
		// ******************************************************************

		reflectivity = value(P_REFLECTIVITY, &nval_reflectivity);		//Base solar-weighted mirror reflectivity value  [none]
		TrackingError = value(P_TRACKINGERROR, &nval_TrackingError);		//User-defined tracking error derate [none]
		GeomEffects = value(P_GEOMEFFECTS, &nval_GeomEffects);		//User-defined geometry effects derate [none]
		Rho_mirror_clean = value(P_RHO_MIRROR_CLEAN, &nval_Rho_mirror_clean);		//User-defined clean mirror reflectivity [none]
		Dirt_mirror = value(P_DIRT_MIRROR, &nval_Dirt_mirror);		//User-defined dirt on mirror derate [none]
		Error = value(P_ERROR, &nval_Error);		//User-defined general optical error derate  [none]
		Ave_Focal_Length = value(P_AVE_FOCAL_LENGTH, &nval_Ave_Focal_Length);		//The average focal length of the collector  [m]
		L_SCA = value(P_L_SCA, &nval_L_SCA);		//The length of the SCA  [m]
		L_aperture = value(P_L_APERTURE, &nval_L_aperture);		//The length of a single mirror/HCE unit [m]
		ColperSCA = value(P_COLPERSCA, &nval_ColperSCA);		//The number of individual collector sections in an SCA  [none]
		Distance_SCA = value(P_DISTANCE_SCA, &nval_Distance_SCA);		// linear distance between SCA's in the field [m]

		HCE_FieldFrac_in = value(P_HCE_FIELDFRAC, &nrow_HCE_FieldFrac, &ncol_HCE_FieldFrac);		//The fraction of the field occupied by this HCE type  [none]
		D_2_in = value(P_D_2, &nrow_D_2, &ncol_D_2);		//The inner absorber tube diameter [m]
		D_3_in = value(P_D_3, &nrow_D_3, &ncol_D_3);		//The outer absorber tube diameter [m]
		D_4_in = value(P_D_4, &nrow_D_4, &ncol_D_4);		//The inner glass envelope diameter  [m]
		D_5_in = value(P_D_5, &nrow_D_5, &ncol_D_5);		//The outer glass envelope diameter  [m]
		D_p_in = value(P_D_P, &nrow_D_p, &ncol_D_p);		//The diameter of the absorber flow plug (optional)  [m]
		Flow_type_in = value(P_FLOW_TYPE, &nrow_Flow_type, &ncol_Flow_type);		//The flow type through the absorber [none]
		Rough_in = value(P_ROUGH, &nrow_Rough, &ncol_Rough);		//Roughness of the internal surface  [m]
		alpha_env_in = value(P_ALPHA_ENV, &nrow_alpha_env, &ncol_alpha_env);		//Envelope absorptance  [none]
		epsilon_3_11_in = value(P_EPSILON_3_11, &nrow_epsilon_3_11, &ncol_epsilon_3_11);		//Absorber emittance - HCE type 1 - HCE variation 1 [none]
		epsilon_3_12_in = value(P_EPSILON_3_12, &nrow_epsilon_3_12, &ncol_epsilon_3_12);		//Absorber emittance - HCE type 1 - HCE variation 2 [none]
		epsilon_3_13_in = value(P_EPSILON_3_13, &nrow_epsilon_3_13, &ncol_epsilon_3_13);		//Absorber emittance - HCE type 1 - HCE variation 3 [none]
		epsilon_3_14_in = value(P_EPSILON_3_14, &nrow_epsilon_3_14, &ncol_epsilon_3_14);		//Absorber emittance - HCE type 1 - HCE variation 4 [none]
		epsilon_3_21_in = value(P_EPSILON_3_21, &nrow_epsilon_3_21, &ncol_epsilon_3_21);		//Absorber emittance - HCE type 2 - HCE variation 1 [none]
		epsilon_3_22_in = value(P_EPSILON_3_22, &nrow_epsilon_3_22, &ncol_epsilon_3_22);		//Absorber emittance - HCE type 2 - HCE variation 2 [none]
		epsilon_3_23_in = value(P_EPSILON_3_23, &nrow_epsilon_3_23, &ncol_epsilon_3_23);		//Absorber emittance - HCE type 2 - HCE variation 3 [none]
		epsilon_3_24_in = value(P_EPSILON_3_24, &nrow_epsilon_3_24, &ncol_epsilon_3_24);		//Absorber emittance - HCE type 2 - HCE variation 4 [none]
		epsilon_3_31_in = value(P_EPSILON_3_31, &nrow_epsilon_3_31, &ncol_epsilon_3_31);		//Absorber emittance - HCE type 3 - HCE variation 1 [none]
		epsilon_3_32_in = value(P_EPSILON_3_32, &nrow_epsilon_3_32, &ncol_epsilon_3_32);		//Absorber emittance - HCE type 3 - HCE variation 2 [none]
		epsilon_3_33_in = value(P_EPSILON_3_33, &nrow_epsilon_3_33, &ncol_epsilon_3_33);		//Absorber emittance - HCE type 3 - HCE variation 3 [none]
		epsilon_3_34_in = value(P_EPSILON_3_34, &nrow_epsilon_3_34, &ncol_epsilon_3_34);		//Absorber emittance - HCE type 3 - HCE variation 4 [none]
		epsilon_3_41_in = value(P_EPSILON_3_41, &nrow_epsilon_3_41, &ncol_epsilon_3_41);		//Absorber emittance - HCE type 4 - HCE variation 1 [none]
		epsilon_3_42_in = value(P_EPSILON_3_42, &nrow_epsilon_3_42, &ncol_epsilon_3_42);		//Absorber emittance - HCE type 4 - HCE variation 2 [none]
		epsilon_3_43_in = value(P_EPSILON_3_43, &nrow_epsilon_3_43, &ncol_epsilon_3_43);		//Absorber emittance - HCE type 4 - HCE variation 3 [none]
		epsilon_3_44_in = value(P_EPSILON_3_44, &nrow_epsilon_3_44, &ncol_epsilon_3_44);		//Absorber emittance - HCE type 4 - HCE variation 4 [none]
		alpha_abs_in = value(P_ALPHA_ABS, &nrow_alpha_abs, &ncol_alpha_abs);		//Absorber absorptance  [none]
		Tau_envelope_in = value(P_TAU_ENVELOPE, &nrow_Tau_envelope, &ncol_Tau_envelope);		//Envelope transmittance [none]
		EPSILON_4_in = value(P_EPSILON_4, &nrow_EPSILON_4, &ncol_EPSILON_4);		//Inner glass envelope emissivities (Pyrex)  [none]
		EPSILON_5_in = value(P_EPSILON_5, &nrow_EPSILON_5, &ncol_EPSILON_5);		//Outer glass envelope emissivities (Pyrex)  [none]
		GlazingIntactIn_in = value(P_GLAZINGINTACTIN, &nrow_GlazingIntactIn, &ncol_GlazingIntactIn);		//The glazing intact flag {1=true, else=false} [none]
		P_a_in = value(P_P_A, &nrow_P_a, &ncol_P_a);		//Annulus gas pressure [torr]
		AnnulusGas_in = value(P_ANNULUSGAS, &nrow_AnnulusGas, &ncol_AnnulusGas);		//Annulus gas type (1=air, 26=Ar, 27=H2) [none]
		AbsorberMaterial_in = value(P_ABSORBERMATERIAL, &nrow_AbsorberMaterial, &ncol_AbsorberMaterial);		//Absorber material type [none]
		Shadowing_in = value(P_SHADOWING, &nrow_Shadowing, &ncol_Shadowing);		//Receiver bellows shadowing loss factor [none]
		Dirt_HCE_in = value(P_DIRT_HCE, &nrow_Dirt_HCE, &ncol_Dirt_HCE);		//Loss due to dirt on the receiver envelope [none]
		Design_loss_in = value(P_DESIGN_LOSS, &nrow_Design_loss, &ncol_Design_loss);		//Receiver heat loss at design [W/m]

		SCAInfoArray_in = value(P_SCAINFOARRAY, &nrow_SCAInfoArray, &ncol_SCAInfoArray);		//(:,1) = HCE type, (:,2)= Collector type for each SCA in the loop  [none]
		SCADefocusArray = value(P_SCADEFOCUSARRAY, &nval_SCADefocusArray);		//Order in which the SCA's should be defocused [none]

        K_cpnt_in = value(P_K_CPNT, &nrow_K_cpnt, &ncol_K_cpnt);
        D_cpnt_in = value(P_D_CPNT, &nrow_D_cpnt, &ncol_D_cpnt);
        L_cpnt_in = value(P_L_CPNT, &nrow_L_cpnt, &ncol_L_cpnt);
        Type_cpnt_in = value(P_TYPE_CPNT, &nrow_Type_cpnt, &ncol_Type_cpnt);

        sf_rnr_diams = value(P_SF_RNR_DIAMS, &nval_sf_rnr_diams);
        sf_rnr_wallthicks = value(P_SF_RNR_WALLTHICKS, &nval_sf_rnr_wallthicks);
        sf_rnr_lengths = value(P_SF_RNR_LENGTHS, &nval_sf_rnr_lengths);
        sf_hdr_diams = value(P_SF_HDR_DIAMS, &nval_sf_hdr_diams);
        sf_hdr_wallthicks = value(P_SF_HDR_WALLTHICKS, &nval_sf_hdr_wallthicks);
        sf_hdr_lengths = value(P_SF_HDR_LENGTHS, &nval_sf_hdr_lengths);

		//Put all of the matrices into a more handlable format
		HCE_FieldFrac.assign(HCE_FieldFrac_in, nrow_HCE_FieldFrac, ncol_HCE_FieldFrac);
		D_2.assign(D_2_in, nrow_D_2, ncol_D_2);
		D_3.assign(D_3_in, nrow_D_3, ncol_D_3);
		D_4.assign(D_4_in, nrow_D_4, ncol_D_4);
		D_5.assign(D_5_in, nrow_D_5, ncol_D_5);
		D_p.assign(D_p_in, nrow_D_p, ncol_D_p);
		Flow_type.assign(Flow_type_in, nrow_Flow_type, ncol_Flow_type);
		Rough.assign(Rough_in, nrow_Rough, ncol_Rough);
		alpha_env.assign(alpha_env_in, nrow_alpha_env, ncol_alpha_env);
		epsilon_3_11.assign(epsilon_3_11_in, nrow_epsilon_3_11, ncol_epsilon_3_11);
		epsilon_3_12.assign(epsilon_3_12_in, nrow_epsilon_3_12, ncol_epsilon_3_12);
		epsilon_3_13.assign(epsilon_3_13_in, nrow_epsilon_3_13, ncol_epsilon_3_13);
		epsilon_3_14.assign(epsilon_3_14_in, nrow_epsilon_3_14, ncol_epsilon_3_14);
		epsilon_3_21.assign(epsilon_3_21_in, nrow_epsilon_3_21, ncol_epsilon_3_21);
		epsilon_3_22.assign(epsilon_3_22_in, nrow_epsilon_3_22, ncol_epsilon_3_22);
		epsilon_3_23.assign(epsilon_3_23_in, nrow_epsilon_3_23, ncol_epsilon_3_23);
		epsilon_3_24.assign(epsilon_3_24_in, nrow_epsilon_3_24, ncol_epsilon_3_24);
		epsilon_3_31.assign(epsilon_3_31_in, nrow_epsilon_3_31, ncol_epsilon_3_31);
		epsilon_3_32.assign(epsilon_3_32_in, nrow_epsilon_3_32, ncol_epsilon_3_32);
		epsilon_3_33.assign(epsilon_3_33_in, nrow_epsilon_3_33, ncol_epsilon_3_33);
		epsilon_3_34.assign(epsilon_3_34_in, nrow_epsilon_3_34, ncol_epsilon_3_34);
		epsilon_3_41.assign(epsilon_3_41_in, nrow_epsilon_3_41, ncol_epsilon_3_41);
		epsilon_3_42.assign(epsilon_3_42_in, nrow_epsilon_3_42, ncol_epsilon_3_42);
		epsilon_3_43.assign(epsilon_3_43_in, nrow_epsilon_3_43, ncol_epsilon_3_43);
		epsilon_3_44.assign(epsilon_3_44_in, nrow_epsilon_3_44, ncol_epsilon_3_44);
		alpha_abs.assign(alpha_abs_in, nrow_alpha_abs, ncol_alpha_abs);
		Tau_envelope.assign(Tau_envelope_in, nrow_Tau_envelope, ncol_Tau_envelope);
		EPSILON_4.assign(EPSILON_4_in, nrow_EPSILON_4, ncol_EPSILON_4);
		EPSILON_5.assign(EPSILON_5_in, nrow_EPSILON_5, ncol_EPSILON_5);
		GlazingIntactIn.assign(GlazingIntactIn_in, nrow_GlazingIntactIn, ncol_GlazingIntactIn);
		P_a.assign(P_a_in, nrow_P_a, ncol_P_a);
		AnnulusGas.assign(AnnulusGas_in, nrow_AnnulusGas, ncol_AnnulusGas);
		AbsorberMaterial.assign(AbsorberMaterial_in, nrow_AbsorberMaterial, ncol_AbsorberMaterial);
		Shadowing.assign(Shadowing_in, nrow_Shadowing, ncol_Shadowing);
		Dirt_HCE.assign(Dirt_HCE_in, nrow_Dirt_HCE, ncol_Dirt_HCE);
		Design_loss.assign(Design_loss_in, nrow_Design_loss, ncol_Design_loss);
		SCAInfoArray.assign(SCAInfoArray_in, nrow_SCAInfoArray, ncol_SCAInfoArray);
        K_cpnt.assign(K_cpnt_in, nrow_K_cpnt, ncol_K_cpnt);
        D_cpnt.assign(D_cpnt_in, nrow_D_cpnt, ncol_D_cpnt);
        L_cpnt.assign(L_cpnt_in, nrow_L_cpnt, ncol_L_cpnt);
        Type_cpnt.assign(Type_cpnt_in, nrow_Type_cpnt, ncol_Type_cpnt);
        rough_cpnt.resize(nrow_K_cpnt, ncol_K_cpnt);
        u_cpnt.resize_fill(nrow_K_cpnt, ncol_K_cpnt, Pipe_hl_coef);
        mc_cpnt.resize(nrow_K_cpnt, ncol_K_cpnt);
        for (std::size_t i = 0; i < mc_cpnt.ncells(); i++) {
            mc_cpnt[i] = mc_bal_sca * 3.6e3 * L_cpnt[i];
            rough_cpnt[i] = HDR_rough / D_cpnt[i];
        }

        interconnects.reserve(nrow_K_cpnt);  // nrow_K_cpnt = number of interconnects
        for (std::size_t i = 0; i < nrow_K_cpnt; i++) {
            interconnects.push_back(interconnect(&htfProps, K_cpnt.row(i).data(), D_cpnt.row(i).data(), L_cpnt.row(i).data(),
                rough_cpnt.row(i).data(), u_cpnt.row(i).data(), mc_cpnt.row(i).data(), Type_cpnt.row(i).data(), K_cpnt.ncols()));
        }

        //std::ofstream logCustomSFPipes;
        //logCustomSFPipes.open("logCustomSFPipes.txt");
        //logCustomSFPipes << "Rnr_D" << "\t" << "Rnr_WT" << "\t" << "Rnr_L" << "\t" << "HDR_D" << "\t" << "HDR_WT" << "\t" << "HDR_L" << "\n";

        //for (int i = 0; i < 4; i++) {
        //    logCustomSFPipes << sf_rnr_diams[i] << "\t";
        //    logCustomSFPipes << sf_rnr_wallthicks[i] << "\t";
        //    logCustomSFPipes << sf_rnr_lengths[i] << "\t";
        //    logCustomSFPipes << sf_hdr_diams[i] << "\t";
        //    logCustomSFPipes << sf_hdr_wallthicks[i] << "\t";
        //    logCustomSFPipes << sf_hdr_lengths[i] << "\t";

        //    logCustomSFPipes << "\n";
        //    //log.flush();
        //}
        //logCustomSFPipes.close();

		//The glazingintact array should be converted to bools
		GlazingIntact.resize(nrow_GlazingIntactIn, ncol_GlazingIntactIn);
		for(int i=0; i<nrow_GlazingIntactIn; i++){
			for(int j=0; j<ncol_GlazingIntactIn; j++){
				GlazingIntact.at(i,j) = GlazingIntactIn.at(i,j) == 1;
			}
		}

		//Organize the emittance tables here
		epsilon_3.init(4, 4);
		epsilon_3.addTable(&epsilon_3_11);	//HCE #1
		epsilon_3.addTable(&epsilon_3_12);
		epsilon_3.addTable(&epsilon_3_13);
		epsilon_3.addTable(&epsilon_3_14);
		epsilon_3.addTable(&epsilon_3_21);	//HCE #2
		epsilon_3.addTable(&epsilon_3_22);
		epsilon_3.addTable(&epsilon_3_23);
		epsilon_3.addTable(&epsilon_3_24);
		epsilon_3.addTable(&epsilon_3_31);	//HCE #3
		epsilon_3.addTable(&epsilon_3_32);
		epsilon_3.addTable(&epsilon_3_33);
		epsilon_3.addTable(&epsilon_3_34);
		epsilon_3.addTable(&epsilon_3_41);	//HCE #4
		epsilon_3.addTable(&epsilon_3_42);
		epsilon_3.addTable(&epsilon_3_43);
		epsilon_3.addTable(&epsilon_3_44);
		
		//Unit conversions
		theta_stow *= d2r;
		theta_dep *= d2r;
        T_recirc += 273.15;
		T_loop_in_des += 273.15;
		T_loop_out += 273.15;
		T_field_ini += 273.15;
		T_fp += 273.15;
		mc_bal_sca *= 3.6e3;	//[Wht/K-m] -> [J/K-m]
		

		/*--- Do any initialization calculations here ---- */
		//Allocate space for the loop simulation objects
		T_htf_in.resize(nSCA);
		T_htf_out.resize(nSCA); 
		T_htf_ave.resize(nSCA); 
		q_loss.resize(nHCEVar); 
		q_abs.resize(nHCEVar); 
		c_htf.resize(nSCA);
		rho_htf.resize(nSCA);
		DP_tube.resize(nSCA);
		E_abs_field.resize(nSCA); 
		E_int_loop.resize(nSCA); 
		E_accum.resize(nSCA); 
		E_avail.resize(nSCA); 
		E_abs_max.resize(nSCA);
		v_1.resize(nSCA);
		q_loss_SCAtot.resize(nSCA); 
		q_abs_SCAtot.resize(nSCA); 
		q_SCA.resize(nSCA); 
		E_fp.resize_fill(nSCA,0.); 
		q_1abs_tot.resize(nSCA); 
		q_1abs.resize(nHCEVar);
		q_i.resize(nColt);
		IAM.resize(nColt);
		ColOptEff.resize(nColt, nSCA);
		EndGain.resize(nColt, nSCA);
		EndLoss.resize(nColt, nSCA);
		RowShadow.resize(nColt);
		//Allocate space for transient variables
		T_htf_in0.resize(nSCA); 
		T_htf_out0.resize(nSCA); 
		T_htf_ave0.resize(nSCA); 
		
		//Set up annulus gas and absorber property matrices
		AnnulusGasMat.resize(nHCEt, nHCEVar);
		AbsorberPropMat.resize(nHCEt, nHCEVar);
		for(int i=0; i<nHCEt; i++){
			for(int j=0; j<nHCEVar; j++){
				//Set up a matrix of annulus gas properties
				AnnulusGasMat.at(i,j) = new HTFProperties();
				AnnulusGasMat.at(i,j)->SetFluid( (int) AnnulusGas.at(i,j) );
				//Set up a matrix of absorber prop materials
				AbsorberPropMat(i,j) = new AbsorberProps();
				AbsorberPropMat(i,j)->setMaterial( (int) AbsorberMaterial.at(i,j) );
			}
		}

		//Initialize values
		E_fp_tot = 0.;
		defocus_old = 0.;
		is_fieldgeom_init = false;

		Pipe_hl = 0.0;
		E_hdr_accum = 0.0;

		return 0;
	}

	bool init_fieldgeom()
	{

		/* 
		Call this method once when call() is first invoked. The calculations require location information that
		is provided by the weatherreader class and not set until after init() and before the first call().
		*/

		//Calculate the actual length of the SCA's based on the aperture length and the collectors per SCA
		L_actSCA.resize(nColt);
		for(int i=0; i<nColt; i++)
		{
			L_actSCA[i] = L_aperture[i] * ColperSCA[i];
		}

		//Calculate the total field aperture area
		Ap_tot = 0.;
		for(int i=0; i<nSCA; i++)
		{
			int ct = (int)SCAInfoArray.at(i,1);
			Ap_tot += A_aperture[ct-1];
		}		
		
		//Calculate the cross-sectional flow area of the receiver piping
		D_h.resize(nHCEt, nHCEVar);
		A_cs.resize(nHCEt, nHCEVar);
		for( int i = 0; i < nHCEt; i++ )
		{
			for( int j = 0; j < nHCEVar; j++ )
			{
				if( Flow_type.at(i, j) == 2 )
				{
					D_h.at(i, j) = D_2.at(i, j) - D_p.at(i, j);
				}
				else
				{
					D_h.at(i, j) = D_2.at(i, j);
					D_p.at(i, j) = 0.;
				}
				A_cs.at(i, j) = pi * (D_2.at(i, j)*D_2.at(i, j) - D_p.at(i, j)*D_p.at(i, j)) / 4.;  //[m2] The cross-sectional flow area
			}
		}

		L_tot = 0.0;
		for( int i = 0; i < nSCA; i++ )
		{
			int ct = (int)SCAInfoArray.at(i, 1);
			L_tot += L_actSCA[ct - 1];
		}

		if( accept_loc == 1 )
		{
            Ap_tot *= float(nLoops);

			//Calculate header diameters here based on min/max velocities
			//output file with calculated header diameter "header_diam.out"
			nfsec = FieldConfig;  //MJW 1.12.11 allow the user to specify the number of field sections
			//Check to make sure the number of field sections is an even number
			if( nfsec % 2 != 0 )
			{
				message(TCS_ERROR, "Number of field subsections must equal an even number");
				return false;
			}

			/*
			The number of header sections (tee-conns.) per field section is equal to the total number of loops divided
			by the number of distinct headers. Since two loops are connected to the same header section,
			the total number of header sections is then divided by 2.
			*/
			nhdrsec = (int)ceil(float(nLoops) / float(nfsec * 2));

			//We need to determine design information about the field for purposes of header sizing ONLY
			c_htf_ave = htfProps.Cp((T_loop_out + T_loop_in_des) / 2.0)*1000.;    //Specific heat

			//Need to loop through to calculate the weighted average optical efficiency at design
			//Start by initializing sensitive variables
			double x1 = 0.0, x2 = 0.0, loss_tot = 0.0;
			opteff_des = 0.0;
			m_dot_design = 0.0;																		

			for( int i = 0; i < nSCA; i++ )
			{
				int CT = (int)SCAInfoArray.at(i, 1);    //Collector type    
				//Calculate the CosTheta value at summer solstice noon
				//x1 = sqrt(1. - pow(cos((latitude - 23.5/180.*pi)-ColTilt) - cos(ColTilt) * cos((latitude - 23.5/180.*pi)) * (1. - cos(0. -ColAz)), 2)); //costheta
				//Calculate end gain factor
				//x2 = max((Ave_Focal_Length[CT-1]*tan(acos(x1))-Distance_SCA[CT-1]),0.0);  //end gain
				//calculate end loss
				//el_des =  1. - (Ave_Focal_Length[CT-1] * tan(acos(x1)) - (float(nSCA) - 1.) /float(nSCA)* x2) / L_actSCA[CT-1];

				for( int j = 0; j < nHCEVar; j++ )
				{
					int HT = (int)SCAInfoArray.at(i, 0);    //HCE type
					//Calculate optical efficiency approximating use of the first collector only
					opteff_des += Shadowing.at(HT - 1, j)*TrackingError[CT - 1] * GeomEffects[CT - 1] * Rho_mirror_clean[CT - 1] * Dirt_mirror[CT - 1] *
						Dirt_HCE.at(HT - 1, j)*Error[CT - 1] * (L_actSCA[CT - 1] / L_tot)*HCE_FieldFrac.at(HT - 1, j);
					loss_tot += Design_loss.at(HT - 1, j)*L_actSCA[CT - 1] * HCE_FieldFrac.at(HT - 1, j);
				}
			}
			//the estimated mass flow rate at design
			m_dot_design = (Ap_tot*I_bn_des*opteff_des - loss_tot*float(nLoops)) / (c_htf_ave*(T_loop_out - T_loop_in_des));  //tn 4.25.11 using Ap_tot instead of A_loop. Change location of opteff_des
            double m_dot_max = m_dot_htfmax * nLoops;
            double m_dot_min = m_dot_htfmin * nLoops;
            if (m_dot_design > m_dot_max) {
                const char *msg = "The calculated field design mass flow rate of %.2f kg/s is greater than the maximum defined by the max single loop flow rate and number of loops (%.2f kg/s). "
                    "The design mass flow rate is reset to the latter.";
                std::string error_msg = util::format(msg, m_dot_design, m_dot_max);
                message(TCS_NOTICE, error_msg.c_str());
                m_dot_design = m_dot_max;
            }
            else if (m_dot_design < m_dot_min) {
                const char *msg = "The calculated field design mass flow rate of %.2f kg/s is less than the minimum defined by the min single loop flow rate and number of loops (%.2f kg/s). "
                    "The design mass flow rate is reset to the latter.";
                std::string error_msg = util::format(msg, m_dot_design, m_dot_min);
                message(TCS_NOTICE, error_msg.c_str());
                m_dot_design = m_dot_min;
            }

			//mjw 1.16.2011 Design field thermal power 
			q_design = m_dot_design * c_htf_ave * (T_loop_out - T_loop_in_des); //[Wt]
			//mjw 1.16.2011 Convert the thermal inertia terms here
			mc_bal_hot = mc_bal_hot * 3.6 * q_design;    //[J/K]
			mc_bal_cold = mc_bal_cold * 3.6 * q_design;  //[J/K]

			//need to provide fluid density
            double rho_cold = htfProps.dens(T_loop_in_des, P_field_in / 2); //kg/m3
            double rho_hot = htfProps.dens(T_loop_out, P_field_in / 2); //kg/m3
			double rho_ave = htfProps.dens((T_loop_out + T_loop_in_des) / 2.0, P_field_in / 2); //kg/m3
			//Calculate the header design
			nrunsec = (int)floor(float(nfsec) / 4.0) + 1;  //The number of unique runner diameters
			D_runner.resize(2*nrunsec);
            WallThk_runner.resize(2*nrunsec);
			L_runner.resize(2*nrunsec);
            m_dot_rnr_dsn.resize(2*nrunsec);
            V_rnr_dsn.resize(2*nrunsec);
            N_rnr_xpans.resize(2*nrunsec);  //calculated number of expansion loops in the runner section
            DP_rnr.resize(2*nrunsec);
            P_rnr.resize(2*nrunsec);
            T_rnr.resize(2*nrunsec);
			D_hdr.resize(2*nhdrsec);
            WallThk_hdr.resize(2*nhdrsec);
            L_hdr.resize(2*nhdrsec);
            N_hdr_xpans.resize(2*nhdrsec);
            m_dot_hdr_dsn.resize(2*nhdrsec);
            V_hdr_dsn.resize(2*nhdrsec);
            DP_hdr.resize(2*nhdrsec);
            P_hdr.resize(2*nhdrsec);
            T_hdr.resize(2*nhdrsec);
            DP_intc.resize(nSCA + 3);
            P_intc.resize(nSCA + 3);
            DP_loop.resize(2*nSCA + 3);
            P_loop.resize(2*nSCA + 3);
            T_loop.resize(2*nSCA + 3);

            if (custom_sf_pipe_sizes) {
                if (nval_sf_rnr_diams == 2*nrunsec && nval_sf_rnr_wallthicks == 2*nrunsec && nval_sf_rnr_lengths == 2*nrunsec &&
                    nval_sf_hdr_diams == 2*nhdrsec && nval_sf_hdr_wallthicks == 2*nhdrsec && nval_sf_hdr_lengths == 2*nhdrsec) {
                    D_runner.assign(sf_rnr_diams, nval_sf_rnr_diams);
                    WallThk_runner.assign(sf_rnr_wallthicks, nval_sf_rnr_wallthicks);
                    L_runner.assign(sf_rnr_lengths, nval_sf_rnr_lengths);
                    D_hdr.assign(sf_hdr_diams, nval_sf_hdr_diams);
                    WallThk_hdr.assign(sf_hdr_wallthicks, nval_sf_hdr_wallthicks);
                    L_hdr.assign(sf_hdr_lengths, nval_sf_hdr_lengths);
                }
                else {
                    message(TCS_ERROR, "The number of custom solar field pipe sections is not correct.");
                    return -1;
                }
            }

			std::string summary;
            rnr_and_hdr_design(nhdrsec, nfsec, nrunsec, rho_cold, rho_hot, V_hdr_cold_max, V_hdr_cold_min,
                V_hdr_hot_max, V_hdr_hot_min, N_max_hdr_diams, m_dot_design, D_hdr, D_runner,
                m_dot_rnr_dsn, m_dot_hdr_dsn, V_rnr_dsn, V_hdr_dsn, &summary, custom_sf_pipe_sizes);

            if (!custom_sf_pipe_sizes) {
                // Calculate pipe wall thicknesses
                for (int i = 0; i < D_runner.ncells(); i++) {
                    WallThk_runner[i] = CSP::WallThickness(D_runner[i]);
                }
                for (int i = 0; i < D_hdr.ncells(); i++) {
                    WallThk_hdr[i] = CSP::WallThickness(D_hdr[i]);
                }
            }

			//report the header and runner metrics
			double *header_diams = allocate(O_HEADER_DIAMS, (int)D_hdr.ncells());
            double *header_wallthk = allocate(O_HEADER_WALLTHK, (int)WallThk_hdr.ncells());
            double *header_massflow_design = allocate(O_HEADER_MDOT_DSN, (int)m_dot_hdr_dsn.ncells());
            double *header_velocity_design = allocate(O_HEADER_V_DSN, (int)V_hdr_dsn.ncells());
			double *runner_diams = allocate(O_RUNNER_DIAMS, (int)D_runner.ncells());
            double *runner_wallthk = allocate(O_RUNNER_WALLTHK, (int)WallThk_runner.ncells());
            double *runner_massflow_design = allocate(O_RUNNER_MDOT_DSN, (int)m_dot_rnr_dsn.ncells());
            double *runner_velocity_design = allocate(O_RUNNER_V_DSN, (int)V_rnr_dsn.ncells());
            std::copy(D_hdr.data(), D_hdr.data() + D_hdr.ncells(), header_diams);
            std::copy(WallThk_hdr.data(), WallThk_hdr.data() + WallThk_hdr.ncells(), header_wallthk);
            std::copy(m_dot_hdr_dsn.data(), m_dot_hdr_dsn.data() + m_dot_hdr_dsn.ncells(), header_massflow_design);
            std::copy(V_hdr_dsn.data(), V_hdr_dsn.data() + V_hdr_dsn.ncells(), header_velocity_design);
            std::copy(D_runner.data(), D_runner.data() + D_runner.ncells(), runner_diams);
            std::copy(WallThk_runner.data(), WallThk_runner.data() + WallThk_runner.ncells(), runner_wallthk);
            std::copy(m_dot_rnr_dsn.data(), m_dot_rnr_dsn.data() + m_dot_rnr_dsn.ncells(), runner_massflow_design);
            std::copy(V_rnr_dsn.data(), V_rnr_dsn.data() + V_rnr_dsn.ncells(), runner_velocity_design);

			//if(ErrorFound()) return

			// Do one-time calculations for system geometry.
            // Determine header section lengths, including expansion loops
            if (size_hdr_lengths(Row_Distance, nhdrsec, offset_xpan_hdr, N_hdr_per_xpan, L_xpan_hdr, L_hdr, N_hdr_xpans, custom_sf_pipe_sizes)) {
                message(TCS_ERROR, "header length sizing failed");
                return -1;
            }
            
            // Determine runner section lengths, including expansion loops
            if (size_rnr_lengths(nfsec, L_rnr_pb, nrunsec, SCAInfoArray.at(0, 1), northsouth_field_sep,
                L_SCA, Min_rnr_xpans, Distance_SCA, nSCA, L_rnr_per_xpan, L_xpan_rnr, L_runner, N_rnr_xpans, custom_sf_pipe_sizes)) {
                message(TCS_ERROR, "runner length sizing failed");
                return -1;
            }
            
            double v_from_sgs = 0.0; double v_to_sgs = 0.0;
			for( int i = 0; i < nrunsec; i++ )
			{
				v_from_sgs = v_from_sgs + 2.*L_runner[i] * pi*pow(D_runner[i], 2) / 4.;  // volume of the runner going away from sgs
                v_to_sgs = v_to_sgs + 2.*L_runner[2*nrunsec - i - 1] * pi*pow(D_runner[2 * nrunsec - i - 1], 2) / 4.;  // ...and going to the sgs
			}

			//report the header and runner lengths
            double *header_lengths = allocate(O_HEADER_LENGTHS, (int)L_hdr.ncells());
            double *header_xpans = allocate(O_HEADER_XPANS, (int)N_hdr_xpans.ncells());
			double *runner_lengths = allocate(O_RUNNER_LENGTHS, (int)L_runner.ncells());
            double *runner_xpans = allocate(O_RUNNER_XPANS, (int)N_rnr_xpans.ncells());
            std::copy(L_hdr.data(), L_hdr.data() + L_hdr.ncells(), header_lengths);
            std::copy(N_hdr_xpans.data(), N_hdr_xpans.data() + N_hdr_xpans.ncells(), header_xpans);
            std::copy(L_runner.data(), L_runner.data() + L_runner.ncells(), runner_lengths);
            std::copy(N_rnr_xpans.data(), N_rnr_xpans.data() + N_rnr_xpans.ncells(), runner_xpans);

			//-------piping from header into and out of the HCE's
			double v_loop_tot = 0.;
			for( int j = 0; j < nHCEVar; j++ )
			{
				for( int i = 0; i < nSCA; i++ )
				{
					int CT = (int)SCAInfoArray.at(i, 1) - 1;   //Collector type    
					int HT = (int)SCAInfoArray.at(i, 0) - 1;    //HCE type
					//v_loop_bal = v_loop_bal + Distance_SCA(CT)*A_cs(HT,j)*HCE_FieldFrac(HT,j)*float(nLoops)
					v_loop_tot += L_SCA[CT]*A_cs(HT, j)*HCE_FieldFrac(HT, j)*float(nLoops);
				}
			}

			// Add on volume for the interconnects and crossover piping
            for (int i = 0; i < interconnects.size(); i++) {
                if (i == 0 || i == interconnects.size() - 1) {
                    v_loop_tot += interconnects[i].getFluidVolume() / 2. * float(nLoops);  // halve the inlet and outlet piping so to not double count
                }
                else {
                    v_loop_tot += interconnects[i].getFluidVolume() * float(nLoops);
                }
            }

			//-------field header loop
            double v_header_cold = 0.0; double v_header_hot = 0.0;
			for( int i = 0; i < nhdrsec; i++ )
			{
				//Also calculate the hot and cold header volume for later use. 4.25 is for header expansion bends
				v_header_cold += pi * D_hdr[i] * D_hdr[i] / 4. * L_hdr[i] * float(nfsec);
                v_header_hot += pi * D_hdr[i + nhdrsec] * D_hdr[i + nhdrsec] / 4. * L_hdr[i + nhdrsec] * float(nfsec);
			}

			//Calculate the HTF volume associated with pumps and the SGS
            double v_sgs = value(P_V_SGS);

			//Calculate the hot and cold balance-of-plant volumes
			v_hot = v_header_hot + v_to_sgs;
			v_cold = v_header_cold + v_from_sgs;

			//Write the volume totals to the piping diameter file
			summary.append(	"\n----------------------------------------------\n"
							"Plant HTF volume information:\n"
							"----------------------------------------------\n" );
#ifdef _MSC_VER
#define MySnprintf _snprintf
#else
#define MySnprintf snprintf
#endif
#define TSTRLEN 512

			char tstr[TSTRLEN];
			MySnprintf(tstr, TSTRLEN, 
				"Cold header pipe volume:   %10.4le m3\n"
				"Hot header pipe volume:    %10.4le m3\n"
				"Volume per loop:           %10.4le m3\n"
				"Total volume in all loops: %10.4le m3\n"
				"Total solar field volume:  %10.4le m3\n"
				"Pump / SGS system volume:  %10.4le m3\n"
				"---------------------------\n"
				"Total plant HTF volume:    %10.4le m3\n",

				v_cold, v_hot, v_loop_tot / double(nLoops), v_loop_tot, 
				(v_hot + v_cold + v_loop_tot), v_sgs, (v_hot + v_cold + v_loop_tot + v_sgs));

			summary.append(tstr);

			// Can uncomment this when other code is updated to write/display more than ~700 characters
			message(TCS_NOTICE, summary.c_str());

			//if ( FILE *file = fopen( "C:/Users/mwagner/Documents/NREL/SAM/Code conversion/header_diam.out", "w") )
			//{
			//	fprintf(file, summary.c_str());
			//	fclose(file);
			//}

			//Include the pump/SGS volume with the header
			v_hot = v_hot + v_sgs / 2.;
			v_cold = v_cold + v_sgs / 2.;
		}

		/* ----- Set initial storage values ------ */
		T_sys_c_last = T_field_ini;
		T_sys_h_last = T_field_ini;
		//cc--> Note that stored(3) -> Iter is no longer used in the TRNSYS code. It is omitted here.
		for( int i = 0; i < nSCA; i++ )
		{
			T_htf_in0[i] = T_field_ini;
			T_htf_out0[i] = T_field_ini;
			T_htf_ave0[i] = T_field_ini;
		}

		is_fieldgeom_init = true;	//The field geometry has been initialized. Make note.

		if( accept_init )
			ss_init_complete = false;
		else
			ss_init_complete = true;

        // Design Sizing
        if (calc_design_pipe_vals) {
            int accept_mode_orig = accept_mode;
            bool accept_init_orig = accept_init;
            int accept_loc_orig = accept_loc;
            bool is_using_input_gen_orig = is_using_input_gen;

            accept_mode = 1;                    // use acceptance testing code
            accept_init = true;                 // require steady-state
            accept_loc = 1;                     // don't just model a single loop
            is_using_input_gen = false;         // using parameter values set below instead

            value(I_I_B, I_bn_des);		        // Direct normal incident solar irradiation [W/m^2]
            value(I_T_DB, 30);		            // Dry bulb air temperature [C]
            value(I_V_WIND, 5);		            // Ambient windspeed  [m/s]
            //value(I_P_AMB, );		            // Ambient pressure [mbar]   - use first value in weather file
            value(I_T_DP, 30 - 10);		        // The dewpoint temperature [C]
            value(I_T_COLD_IN, T_loop_in_des - 273.15); // HTF return temperature, to the field [C]
            value(I_M_DOT_IN, m_dot_design * 3600);     // HTF mass flow rate at the inlet to the field  [kg/hr]
            value(I_DEFOCUS, 1);		        // Defocus control  [none] (1 = no defocus)
            value(I_RECIRC, 0.);
            value(I_SOLARAZ, ColAz*r2d + 180);	// Solar azimuth angle, 0 = North [deg], before SolarAz is converted so to make them equal
            latitude = value(I_LATITUDE);		// Site latitude read from weather file [deg]
            longitude = value(I_LONGITUDE);		// Site longitude read from weather file [deg]
            shift = value(I_SHIFT);			    // [deg]

            call(14817600, 0, 0);      // 14817600 = noon on the summer solstice

            // Restore parameters
            calc_design_pipe_vals = false;
            start_time = -1;
            accept_mode = accept_mode_orig;
            accept_init = accept_init_orig;
            accept_loc = accept_loc_orig;
            is_using_input_gen = is_using_input_gen_orig;
            T_sys_c_last = T_field_ini;
            T_sys_h_last = T_field_ini;
            T_htf_in0.fill(T_field_ini);
            T_htf_out0.fill(T_field_ini);
            T_htf_ave0.fill(T_field_ini);
        }

		// Write Calculated Design Parameters
		value(PO_A_APER_TOT, Ap_tot);	//[m^2] Total solar field aperture area
        // Update P_field_in value

		return true;
	}

	virtual int call(double time, double step, int ncall){

		//ncall_track = ncall;

		//reset defocus counter
		dfcount = 0;

		//record the start time
		if(start_time < 0)
		{ 
			start_time = current_time(); 
		}

		// ******************************************************************************************************************************
		//               Time-dependent conditions
		// ******************************************************************************************************************************
		I_b = value(I_I_B);		//Direct normal incident solar irradiation [W/m^2]
		T_db = value(I_T_DB);		//Dry bulb air temperature [C]
		V_wind = value(I_V_WIND);		//Ambient windspeed  [m/s]
		P_amb = value(I_P_AMB);		//Ambient pressure [mbar]
		T_dp = value(I_T_DP);		//The dewpoint temperature [C]
		T_cold_in = value(I_T_COLD_IN);		//HTF return temperature [C]
		m_dot_in = value(I_M_DOT_IN);		//HTF mass flow rate at the inlet  [kg/hr]
		defocus_new = value(I_DEFOCUS);		//Defocus control  [none]
        recirculating = value(I_RECIRC);
		SolarAz = value(I_SOLARAZ);		//Solar azimuth angle reported by the Type15 weather file [deg]
		latitude = value(I_LATITUDE);		//Site latitude read from weather file [deg]
		longitude = value(I_LONGITUDE);		//Site longitude read from weather file [deg]
		shift = value(I_SHIFT);			// [deg]

		//Unit conversions
		T_db += 273.15;
		T_dp += 273.15;
		P_amb *= 100.0; //mbar -> Pa
		T_cold_in += 273.15;
		m_dot_in *= 1/3600.;
		SolarAz = (SolarAz - 180.0) * d2r;      // convert from North=0 to South=0
		latitude *= d2r;
		longitude *= d2r;
		shift *= d2r;

		//Initialize the field geometry if it hasn't been already
		if( !is_fieldgeom_init )
		{
			if( !init_fieldgeom() ) 
				return -1;
		}

		//If no change in inputs between calls, then no need to go through calculations.  Sometimes an extra call was run.....
		if (ncall>0) 
		{
    		if( (! no_fp) && (T_cold_in_1 > T_cold_in) )
			{
				E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in);       //[kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
				goto set_outputs_and_return;
			}    
		}
		// **********************************************************************************************************************

		double rho_hdr_cold, rho_hdr_hot;
		double c_hdr_cold_last, m_dot_lower, m_dot_upper;
		bool upflag, lowflag, fp_lowflag, fp_upflag;
		double T_in_lower, y_fp_lower, T_in_upper, y_fp_upper;
		double y_upper, y_lower;
		double upmult, t_tol, err;
		double DP_IOCOP, DP_loop_tot, m_dot_run_in, x3, m_dot_temp;
		double DP_toField, DP_fromField;
		double m_dot_hdr_in, m_dot_hdr, DP_hdr_cold, DP_hdr_hot;
		double E_avail_tot, rho_ave, E_int_sum;
        int loop_i, sca_i, intc_i;
		double q_abs_maxOT;
        q_abs_maxOT = 0;

		if (ncall==0)  //mjw 3.5.11 We only need to calculate these values once per timestep..
		{
			//calculate the hour of the day
			time_hr = time / 3600.;
			dt_hr = dt / 3600.;
			hour = fmod(time_hr, 24.);       //hour of the day (1..24)  //tn 4.25.11 mod returns a natural number. This messes with the ftrack HrA/HrB calculations
			if(T_dp > -300.0)
				T_sky = CSP::skytemp(T_db, T_dp, hour);     //[K] Effective sky temperature 
			else
				T_sky = T_db - 20.0;

			// Always reset the defocus control at the first call of a timestep
			//defocus_new = 1.;
			//defocus_old = 1.;
			//defocus = 1.0;

			//Time calculations
			day_of_year = (int)ceil(time_hr/24.);  //Day of the year
			// Duffie & Beckman 1.5.3b
			double B = (day_of_year-1)*360.0/365.0*pi/180.0;
			// Eqn of time in minutes
			double EOT = 229.2 * (0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B)	- 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
			// Declination in radians (Duffie & Beckman 1.6.1)
			double Dec = 23.45 * sin(360.0*(284.0+day_of_year)/365.0*pi/180.0) * pi/180.0;
			// Solar Noon and time in hours
			double SolarNoon = 12. - ((shift)*180.0/pi) / 15.0 - EOT / 60.0;

			// Deploy & stow times in hours
			// Calculations modified by MJW 11/13/2009 to correct bug
			theta_dep = max(theta_dep,1.e-6);
			double DepHr1 = cos(latitude) / tan(theta_dep);
			double DepHr2 = -tan(Dec) * sin(latitude) / tan(theta_dep);
			double DepHr3 = CSP::sign(tan(pi-theta_dep)) * acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / pi / 15.0;
			double DepTime = SolarNoon + DepHr3;

			theta_stow = max(theta_stow,1.e-6);
			double StwHr1 = cos(latitude) / tan(theta_stow);
			double StwHr2 = -tan(Dec) * sin(latitude) / tan(theta_stow);
			double StwHr3 = CSP::sign(tan(pi-theta_stow))*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0;
			double StwTime = SolarNoon + StwHr3;

			// ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
			double HrA = hour-dt_hr;
			double HrB = hour;

			double  MidTrack;
			// Solar field operates
			if ((HrB > DepTime)  &&  (HrA < StwTime)) 
			{
				// solar field deploys during time period
				if (HrA < DepTime) 
				{
					ftrack = (HrB - DepTime) / dt_hr;
					MidTrack = HrB - ftrack * 0.5 *dt_hr;

				// Solar field stows during time period
				}
				else if (HrB > StwTime) 
				{
					ftrack = (StwTime - HrA) / dt_hr;
					MidTrack = HrA + ftrack * 0.5 *dt_hr;
				// solar field operates during entire period
				} 
				else 
				{
					ftrack = 1.0;
					MidTrack = HrA + 0.5 *dt_hr;
				}
			// solar field doesn't operate
			} 
			else
			{
				ftrack = 0.0;
				MidTrack = HrA + 0.5 *dt_hr;
			}

			double StdTime = MidTrack;
			double SolarTime = StdTime+((shift)*180.0/pi)/15.0+ EOT/60.0;
			// hour angle (arc of sun) in radians
			double omega = (SolarTime - 12.0)*15.0*pi/180.0;

            if (calc_design_pipe_vals) {
                SolarAlt = pi/2. - ColTilt;
                SolarAz = ColAz;
            }
            else {
                // B. Stine equation for Solar Altitude angle in radians
                SolarAlt = asin(sin(Dec)*sin(latitude) + cos(latitude)*cos(Dec)*cos(omega));
                if ((accept_init  &&  time == start_time) || is_using_input_gen)
                {  //MJW 1.14.2011 
                    SolarAz = CSP::sign(omega)*fabs(acos(min(1.0, (cos(pi / 2. - SolarAlt)*sin(latitude) - sin(Dec)) / (sin(pi / 2. - SolarAlt)*cos(latitude)))));
                }
            }
    
			// Calculation of Tracking Angle for Trough. Stine Reference
			double TrackAngle = atan ( cos(SolarAlt) * sin(SolarAz-ColAz) / 
						 (sin(SolarAlt-ColTilt)+sin(ColTilt)*cos(SolarAlt)*(1-cos(SolarAz-ColAz))) );
			// Calculation of solar incidence angle for trough.. Stine reference
			if(ftrack==0.0) 
			{
				costh=1.0;
			} 
			else 
			{
				costh = sqrt(1.0 - pow(cos(SolarAlt-ColTilt) - cos(ColTilt) * cos(SolarAlt) * (1.0 - cos(SolarAz -ColAz)), 2));
			}

			// theta in radians
			theta = acos(costh);
        
			for(int i=0; i<nColt; i++)
			{
				q_i[i] = I_b*A_aperture[i]/L_actSCA[i]; //[W/m] The incoming solar irradiation per aperture length
				//incidence angle modifier (radians)
				//IAM[i] = IamF0[i] + IamF1[i] * theta / costh + IamF2[i] * theta*theta/ costh;
				
				IAM[i] = IAM_matrix(i,0);
				for( int j = 1; j < n_c_iam_matrix; j++ )
					IAM[i] += IAM_matrix(i, j)*pow(theta, j) / costh;
				
				IAM[i] = fmax(0.0, fmin(IAM[i], 1.0));

				//Calculate the Optical efficiency of the collector
				for(int j=0; j<nSCA; j++)
				{
					ColOptEff(i,j) = TrackingError[i] * GeomEffects[i] * Rho_mirror_clean[i] * Dirt_mirror[i] * Error[i] * IAM[i];
				}

				//Account for light reflecting off the collector and missing the receiver, also light from other 
				//collectors hitting a different receiver
				//mjw 4.21.11 - rescope this to be for each specific collector j=1,nSCA
				for(int j=0; j<nSCA; j++)
				{
					if(fabs(SolarAz) <= 90.0) 
					{  //mjw 5.1.11 The sun is in the southern sky (towards equator)
						if(j==0 || j==nSCA-1) 
						{
							EndGain(i,j) =  0.0; //No gain for the first or last collector
						} 
						else 
						{
							EndGain(i,j) = max(Ave_Focal_Length[i] * tan(theta) - Distance_SCA[i], 0.0)/ L_actSCA[i];
						}
					} 
					else 
					{  //mjw 5.1.11 The sun is in the northern sky (away from equator)
						if((j==floor(float(nSCA)/2.)-1) || (j==floor(float(nSCA)/2.))) 
						{
							EndGain(i,j) = 0.0; //No gain for the loops at the ends of the rows
						}
						else 
						{
							EndGain(i,j) = max(Ave_Focal_Length[i] * tan(theta) - Distance_SCA[i], 0.0)/ L_actSCA[i];
						}
					}
					EndLoss(i,j) = 1.0 - Ave_Focal_Length[i] * tan(theta) / L_actSCA[i] + EndGain(i,j);
				}
                
				// Row to Row Shadowing Lossess
				//PH = pi / 2.0 - TrackAngle[i]
				RowShadow[i] = fabs(cos(TrackAngle)) * Row_Distance / W_aperture[i];
				if ( RowShadow[i] < 0.5  ||  SolarAlt < 0. ) 
				{
					RowShadow[i] = 0.;
				} 
				else if (RowShadow[i] > 1.) 
				{
					RowShadow[i] = 1.;
				}
        
				//Finally correct for these losses on the collector optical efficiency value
				for(int j=0; j<nSCA; j++)
				{
					ColOptEff(i,j) = ColOptEff(i,j)*RowShadow[i]*EndLoss(i,j)*ftrack;  //mjw 4.21.11 now a separate value for each SCA
				}
			}

			//Calculate the flux level associated with each SCA
			//but only calculate for the first call of the timestep<----[NO// messes with defocusing control: mjw 11.4.2010]
			Theta_ave = 0.0; CosTh_ave = 0.0; IAM_ave = 0.0; RowShadow_ave = 0.0; EndLoss_ave = 0.0;
			for(int i=0; i<nSCA; i++)
			{
				int CT = (int)SCAInfoArray(i,1)-1;    //Collector type
				q_SCA[i] = q_i[CT]*costh;        //The flux corresponding with the collector type
				//Also use this chance to calculate average optical values
				Theta_ave = Theta_ave + theta*L_actSCA[CT]/L_tot;
				CosTh_ave = CosTh_ave + costh*L_actSCA[CT]/L_tot;
				IAM_ave = IAM_ave + IAM[CT]*L_actSCA[CT]/L_tot;
				RowShadow_ave = RowShadow_ave + RowShadow[CT]*L_actSCA[CT]/L_tot;
				EndLoss_ave = EndLoss_ave + EndLoss(CT,i)*L_actSCA[CT]/L_tot;
			}

			SCAs_def = 1.;

			if( accept_mode )
			{
				if( accept_loc == 1 )
					m_dot_htfX = m_dot_in / float(nLoops);
				else
					m_dot_htfX = m_dot_in;
			}
			else
			{
				if( I_b > 25. )
					m_dot_htfX = max(min(10. / 950.*I_b, m_dot_htfmax), m_dot_htfmin);   //*defocus[kg/s] guess Heat transfer fluid mass flow rate through one loop
				else
					m_dot_htfX = m_dot_htfmin;
			}

		} 
		else  //mjw 3.5.11
		{	
			for(int i=0; i<nSCA; i++)
			{	
				q_SCA[i] = q_i[(int)SCAInfoArray(i,1)-1]*costh;
			}
		}  //mjw 3.5.11 ---------- } of the items that only need to be calculated once per time step
		
		q_SCA_tot = 0.;
		for(int i=0; i<nSCA; i++){ q_SCA_tot += q_SCA[i]; } //W/m

		//9-27-12, TWN: This model uses relative defocus. Changed controller to provide absolute defocus, so now convert to relative here
        // defocus_rel = defocus_abs / defocus_abs_prev
        if (defocus_old == 0) { defocus_old = 1; }
		defocus = defocus_new / defocus_old;
		defocus_old = defocus_new;

		//TWN 6/14/11  if defous is < 1 { calculate defocus from previous call's q_abs and { come back to temperature loop.
		if(defocus<1.) 
		{
			dfcount = dfcount +1;	//cc--> Not sure why this counts.. the dfcount variable is always set to 0 when the simulation is called.. Ask Ty.
			goto post_convergence_flag; // goto 11;
		}

		// 12.29.2014, twn: want this before 'overtemp_iter_flag' so temp doesn't reset when flag is called
		T_cold_in_1 = T_cold_in; //Inlet temperature may change due to new freeze protection model

overtemp_iter_flag: //10 continue     //Return loop for over-temp conditions
		
		//First calculate the cold header temperature, which will serve as the loop inlet temperature
		rho_hdr_cold = htfProps.dens(T_sys_c_last, P_field_in);
		rho_hdr_hot = htfProps.dens(T_sys_h_last, 1.e5);  // use an assumed outlet pressure of 1 atm instead of P_field_in
		c_hdr_cold_last = htfProps.Cp(T_sys_c_last)*1000.0; //mjw 1.6.2011 Adding mc_bal to the cold header inertia

		dfcount ++;   //The defocus iteration counter

		// ******* Initial values *******
		//if(T_loop_in == T_loop_out) T_loop_in = T_loop_out - 1. //Don't allow equal temperatures
		//T_htf_in(1) = T_loop_in

		//mode for solving the field 
		//1=constrain temperature
		//2=defocusing array
		//3=constrain mass flow above min
		SolveMode = 1;

		qq = 0;                  //Set iteration counter

		m_dot_htfX = max(min(m_dot_htfmax, m_dot_htfX), m_dot_htfmin);

		//Set ends of iteration bracket.  Make bracket greater than bracket defined on system limits so a value less than system limit can be tried earlier in iteration
		m_dot_lower = 0.7*m_dot_htfmin;
		m_dot_upper = 1.3*m_dot_htfmax;

		upflag = false;        //Set logic to switch from bisection to false position mode
		lowflag = false;       //Set logic to switch from bisection to false position mode
		fp_lowflag  = false;   //Set logic to switch from bisection to false position mode
		fp_upflag   = false;   //Set logic to switch from bisection to false position mode
		no_fp = true;          //Set logic that specify that SCA loop is not solving for freeze protection temperature
		T_in_lower=std::numeric_limits<double>::quiet_NaN();
		y_fp_lower=std::numeric_limits<double>::quiet_NaN(); 
		T_in_upper=std::numeric_limits<double>::quiet_NaN(); 
		y_fp_upper=std::numeric_limits<double>::quiet_NaN(); 
		y_upper = std::numeric_limits<double>::quiet_NaN();
		y_lower = std::numeric_limits<double>::quiet_NaN();
		upmult=std::numeric_limits<double>::quiet_NaN();		

		t_tol = 1.5e-3;          //Tolerance for T_loop_out

		//Decreasing the tolerance helps get out of repeating defocus iterations
		if(ncall>8)
		{
			t_tol = 1.5e-4;
		}

		err = t_tol * 10.;       //Set beginning error > tolerance
		// ******************************************************************************************************************************
		//                   Iterative section
		// ******************************************************************************************************************************
        while ((fabs(err) > t_tol) && (qq < 30))
        {

            qq++; //Iteration counter
            q_loss_SCAtot.fill(0.);
            q_abs_SCAtot.fill(0.);
            q_1abs_tot.fill(0.);
            E_avail.fill(0.);
            E_accum.fill(0.);
            E_int_loop.fill(0.);
            EqOpteff = 0.0;
            c_htf.fill(0.);
            rho_htf.fill(0.);

            m_dot_htf = m_dot_htfX;

            if (accept_loc == 1)
                m_dot_htf_tot = m_dot_htf * float(nLoops);
            else
                m_dot_htf_tot = m_dot_htf;

            if (accept_loc == 1)
            {
                T_sys_c = (T_sys_c_last - T_cold_in_1)*exp(-(m_dot_htf*float(nLoops)) / (v_cold*rho_hdr_cold + mc_bal_cold / c_hdr_cold_last)*dt) + T_cold_in_1;
                //Consider heat loss from cold piping
                //Runner
                Runner_hl_cold = 0.0;
                Runner_hl_cold_tot = 0.0;
                T_rnr[0] = T_sys_c;             // using whole system thermal inertia (T_sys_c vs. T_cold_in_1) - TODO - adjust this
                c_hdr_cold = htfProps.Cp(T_sys_c)*1000.0;
                for (int i = 0; i < nrunsec; i++)
                {
                    if (i != 0) {
                        T_rnr[i] = T_rnr[i - 1] - Runner_hl_cold / (m_dot_runner(m_dot_htf_tot, nfsec, i - 1)*c_hdr_cold);
                    }
                    Runner_hl_cold = L_runner[i] * pi*D_runner[i] * Pipe_hl_coef*(T_rnr[i] - T_db);  //[W]
                    Runner_hl_cold_tot += Runner_hl_cold;
                }

                //Header
                Header_hl_cold = 0.0;
                Header_hl_cold_tot = 0.0;
                T_hdr[0] = T_rnr[nrunsec - 1] - Runner_hl_cold / (m_dot_runner(m_dot_htf_tot, nfsec, nrunsec - 1)*c_hdr_cold);  // T's for farthest headers
                for (int i = 0; i < nhdrsec; i++)
                {
                    if (i != 0) {
                        T_hdr[i] = T_hdr[i - 1] - Header_hl_cold / (m_dot_header(m_dot_htf_tot, nfsec, nLoops, i - 1)*c_hdr_cold);
                    }
                    Header_hl_cold = Row_Distance * D_hdr[i] * pi*Pipe_hl_coef*(T_hdr[i] - T_db);  //[W]
                    Header_hl_cold_tot += Header_hl_cold;
                }

                T_loop_in = T_hdr[nhdrsec - 1] - Header_hl_cold / (m_dot_header(m_dot_htf_tot, nfsec, nLoops, nhdrsec - 1)*c_hdr_cold);
                T_htf_in[0] = T_loop_in;
            }
            else		// accept_loc == 2, only modeling loop
            {
                T_htf_in[0] = T_cold_in_1;
                T_sys_c = T_htf_in[0];
            }

            //---------------------
            double P_intc_in = P_field_in;
            Intc_hl = 0.;

            T_loop[0] = T_loop_in;
            intc_state = interconnects[0].State(m_dot_htf * 2, T_loop[0], T_db, P_intc_in);
            T_loop[1] = intc_state.temp_out;
            Intc_hl += intc_state.heat_loss;            // W, riser
            intc_state = interconnects[1].State(m_dot_htf, T_loop[1], T_db, intc_state.pressure_out);
            T_htf_in[0] = intc_state.temp_out; 
            Intc_hl += intc_state.heat_loss;            // W, interconnect before first SCA

            for (int i = 0; i < nSCA; i++)
            {
                q_loss.fill(0.);
                q_abs.fill(0.);
                q_1abs.fill(0.);

                int HT = (int)SCAInfoArray(i, 0) - 1;    //HCE type
                int CT = (int)SCAInfoArray(i, 1) - 1;    //Collector type

                for (int j = 0; j < nHCEVar; j++)
                {

                    //Check to see if the field fraction for this HCE is zero.  if so, don't bother calculating for this variation
                    if (HCE_FieldFrac(HT, j) == 0.0) continue;

                    /*if(epsilon_3l(HT,j)>1) {
                        xx(:)=epsilon_3t(HT,j,:)
                        yy(:)=epsilon_3(HT,j,:)
                        //call interp((T_htf_ave[i]-273.15),epsilon_3l(HT,j),xx,yy,eps3sav,eps_3): Now calculate inside of receiver subroutine such that correct temp is used
                    } else {
                        eps_3 = epsilon_3(HT,j,1)
                    }*/	//cc--> Emittance is stored in objects that the EvacReceiver sub. has access to. No need to calculate here
                    double c_htf_j, rho_htf_j;//                                               hn, hv
                    EvacReceiver(T_htf_in[i], m_dot_htf, T_db, T_sky, V_wind, P_amb, q_SCA[i], HT, j, CT, i, false, ncall, time_hr,
                        //outputs
                        q_loss[j], q_abs[j], q_1abs[j], c_htf_j, rho_htf_j);

                    if (q_abs[j] != q_abs[j])	//cc--> Check for NaN
                    {
                        m_dot_htfX = m_dot_htfmax;
                        if (dfcount > 20)
                        {
                            message(TCS_WARNING, "The solution encountered an unresolvable NaN error in the heat loss calculations. Continuing calculations...");
                            return 0;
                        }
                        goto overtemp_iter_flag;
                    }

                    //Keep a running sum of all of the absorbed/lost heat for each SCA in the loop
                    double temp[] = { q_loss[j], q_abs[j], L_actSCA[CT], HCE_FieldFrac(HT,j) };

                    q_abs_SCAtot[i] += q_abs[j] * L_actSCA[CT] * HCE_FieldFrac(HT, j);
                    q_loss_SCAtot[i] += q_loss[j] * L_actSCA[CT] * HCE_FieldFrac(HT, j);
                    q_1abs_tot[i] += q_1abs[j] * HCE_FieldFrac(HT, j);  //losses in W/m from the absorber surface
                    c_htf[i] += c_htf_j * HCE_FieldFrac(HT, j);
                    rho_htf[i] += rho_htf_j * HCE_FieldFrac(HT, j);

                    //keep track of the total equivalent optical efficiency
                    EqOpteff += ColOptEff(CT, i)*Shadowing(HT, j)*Dirt_HCE(HT, j)*alpha_abs(HT, j)*Tau_envelope(HT, j)*(L_actSCA[CT] / L_tot)*HCE_FieldFrac(HT, j);;
                }  //nHCEVar loop

                //Calculate the specific heat for the node
                c_htf[i] *= 1000.0;
                //Calculate the average node outlet temperature, including transient effects
                double m_node = rho_htf[i] * A_cs(HT, 1)*L_actSCA[CT];

                //MJW 12.14.2010 The first term should represent the difference between the previous average temperature and the new 
                //average temperature. Thus, the heat addition in the first term should be divided by 2 rather than include the whole magnitude
                //of the heat addition.
                //mjw & tn 5.1.11: There was an error in the assumption about average and outlet temperature      
                T_htf_out[i] = q_abs_SCAtot[i] / (m_dot_htf*c_htf[i]) + T_htf_in[i] +
                    2.0 * (T_htf_ave0[i] - T_htf_in[i] - q_abs_SCAtot[i] / (2.0 * m_dot_htf * c_htf[i])) *
                    exp(-2. * m_dot_htf * c_htf[i] * dt / (m_node * c_htf[i] + mc_bal_sca * L_actSCA[CT]));
                //Recalculate the average temperature for the SCA
                T_htf_ave[i] = (T_htf_in[i] + T_htf_out[i]) / 2.0;


                //Calculate the actual amount of energy absorbed by the field that doesn't go into changing the SCA's average temperature
                //MJW 1.16.2011 Include the thermal inertia term
                if (!is_using_input_gen)
                {
                    if (q_abs_SCAtot[i] > 0.0)
                    {
                        //E_avail[i] = max(q_abs_SCAtot[i]*dt*3600. - A_cs(HT,1)*L_actSCA[CT]*rho_htf[i]*c_htf[i]*(T_htf_ave[i]- T_htf_ave0[i]),0.0)
                        double x1 = (A_cs(HT, 1)*L_actSCA[CT] * rho_htf[i] * c_htf[i] + L_actSCA[CT] * mc_bal_sca);  //mjw 4.29.11 removed c_htf[i] -> it doesn't make sense on the mc_bal_sca term
                        E_accum[i] = x1 * (T_htf_ave[i] - T_htf_ave0[i]);
                        E_int_loop[i] = x1 * (T_htf_ave[i] - 298.15);  //mjw 1.18.2011 energy relative to ambient 
                        E_avail[i] = max(q_abs_SCAtot[i] * dt - E_accum[i], 0.0);      //[J/s]*[hr]*[s/hr]: [J]

                        //Equation: m_dot_avail*c_htf[i]*(T_hft_out - T_htf_in) = E_avail/(dt*3600)
                        //m_dot_avail = (E_avail[i]/(dt*3600.))/(c_htf[i]*(T_htf_out[i] - T_htf_in[i]))   //[J/s]*[kg-K/J]*[K]: 
                    }
                }
                else
                {
                    //E_avail[i] = max(q_abs_SCAtot[i]*dt*3600. - A_cs(HT,1)*L_actSCA[CT]*rho_htf[i]*c_htf[i]*(T_htf_ave[i]- T_htf_ave0[i]),0.0)
                    double x1 = (A_cs(HT, 1)*L_actSCA[CT] * rho_htf[i] * c_htf[i] + L_actSCA[CT] * mc_bal_sca);  //mjw 4.29.11 removed c_htf[i] -> it doesn't make sense on the mc_bal_sca term
                    E_accum[i] = x1 * (T_htf_ave[i] - T_htf_ave0[i]);
                    E_int_loop[i] = x1 * (T_htf_ave[i] - 298.15);  //mjw 1.18.2011 energy relative to ambient 
                    //E_avail[i] = max(q_abs_SCAtot[i] * dt - E_accum[i], 0.0);      //[J/s]*[hr]*[s/hr]: [J]
                    E_avail[i] = (q_abs_SCAtot[i] * dt - E_accum[i]);      //[J/s]*[hr]*[s/hr]: [J]

                    //Equation: m_dot_avail*c_htf[i]*(T_hft_out - T_htf_in) = E_avail/(dt*3600)
                    //m_dot_avail = (E_avail[i]/(dt*3600.))/(c_htf[i]*(T_htf_out[i] - T_htf_in[i]))   //[J/s]*[kg-K/J]*[K]: 
                }

                //Set the inlet temperature of the next SCA equal to the outlet temperature of the current SCA
                //minus the heat losses in the interconnects
                if (i < nSCA - 1)
                {
                    //Calculate inlet temperature of the next SCA
                    IntcOutputs intc_state = interconnects[i + 2].State(m_dot_htf, T_htf_out[i], T_db, P_intc_in);
                    P_intc_in -= intc_state.pressure_drop;  // pressure drops in HCAs only accounted for later
                    T_htf_in[i + 1] = intc_state.temp_out;
                    Intc_hl += intc_state.heat_loss;            // W
                    //mjw 1.18.2011 Add the internal energy of the crossover piping and interconnects between the current SCA and the next one
                    E_int_loop[i] += intc_state.internal_energy;
                }

            }

            intc_state = interconnects[interconnects.size() - 2].State(m_dot_htf, T_htf_out[nSCA - 1], T_db, P_intc_in);
            T_loop[2*nSCA + 2] = intc_state.temp_out;
            Intc_hl += intc_state.heat_loss;            // W, interconnect after last SCA
            P_intc_in -= intc_state.pressure_drop;  // pressure drops in HCAs only accounted for later

			//Set the loop outlet temperature
            intc_state = interconnects[interconnects.size() - 1].State(m_dot_htf * 2, T_loop[2*nSCA + 2], T_db, P_intc_in);
            T_loop_outX = intc_state.temp_out;
            Intc_hl += intc_state.heat_loss;            // W, downcomer

            //Fill in rest of T_loop using the SCA inlet and outlet temps
            loop_i = 2; sca_i = 0;
            while (loop_i < 2*nSCA + 2) {
                T_loop[loop_i] = T_htf_in[sca_i];
                T_loop[loop_i + 1] = T_htf_out[sca_i];
                loop_i = loop_i + 2; sca_i++;
            }
            
			if( accept_loc == 1 )
			{								
				//Consider heat loss from hot piping
                //Header
                Header_hl_hot = 0.0;
				Header_hl_hot_tot = 0.0;
                T_hdr[nhdrsec] = T_loop_outX;
				c_hdr_hot = htfProps.Cp(T_loop_outX)* 1000.;
				for( int i = nhdrsec; i < 2*nhdrsec; i++ )
				{
                    if (i != nhdrsec) {
                        T_hdr[i] = T_hdr[i - 1] - Header_hl_hot / (m_dot_header(m_dot_htf_tot, nfsec, nLoops, i - 1)*c_hdr_hot);
                    }
                    Header_hl_hot = Row_Distance*D_hdr[i]*pi*Pipe_hl_coef*(T_hdr[i] - T_db);
                    Header_hl_hot_tot += Header_hl_hot;
				}

				//Runner
                Runner_hl_hot = 0.0;
				Runner_hl_hot_tot = 0.0;
                T_rnr[nrunsec] = T_hdr[2*nhdrsec - 1] - Header_hl_hot / (m_dot_header(m_dot_htf_tot, nfsec, nLoops, 2*nhdrsec - 1)*c_hdr_hot);
				for( int i = nrunsec; i < 2*nrunsec; i++ )
				{
                    if (i != nrunsec) {
                        T_rnr[i] = T_rnr[i - 1] - Runner_hl_hot / (m_dot_runner(m_dot_htf_tot, nfsec, i - 1)*c_hdr_hot);
                    }
					Runner_hl_hot = L_runner[i]*pi*D_runner[i]*Pipe_hl_coef*(T_rnr[i] - T_db);  //Wt
                    Runner_hl_hot_tot += Runner_hl_hot;
				}

				Pipe_hl_hot = Header_hl_hot_tot + Runner_hl_hot_tot;


				//Adjust the loop outlet temperature to account for thermal losses incurred in the hot header and the runner pipe
				T_sys_h = T_loop_outX - Pipe_hl_hot / (m_dot_htf_tot*c_hdr_hot);

				//Calculate the system temperature of the hot portion of the collector field. 
				//This will serve as the fluid outlet temperature
				T_sys_h = (T_sys_h_last - T_sys_h)*exp(-m_dot_htf_tot / (v_hot*rho_hdr_hot + mc_bal_hot / c_hdr_hot)*dt) + T_sys_h;
			}
			else
			{
				T_sys_h = T_loop_outX;
			}

			if( !ss_init_complete && accept_init )
			{
				// Check if solution has reached steady state
				double ss_diff = 0.0;
				ss_diff += fabs(T_sys_c - T_sys_c_last) + fabs(T_sys_h_last - T_sys_h);
				for( int i = 0; i < nSCA; i++ )
				{
					ss_diff += fabs(T_htf_in0[i] - T_htf_in[i]) + fabs(T_htf_out0[i] - T_htf_out[i]) + fabs(T_htf_ave0[i] - (T_htf_in[i] + T_htf_out[i]) / 2.0);
				}

				if( ss_diff / 300.0 > 0.001 )	// If not in steady state, updated previous temperatures and re-run energy balances
				{			// Should be similar to Converged call
					
					T_sys_c_last = T_sys_c;		// Get T_sys from the last timestep
					T_sys_h_last = T_sys_h;
					
					for( int i = 0; i<nSCA; i++ )
					{
						T_htf_in0[i] = T_htf_in[i];
						T_htf_out0[i] = T_htf_out[i];
						T_htf_ave0[i] = (T_htf_in0[i] + T_htf_out0[i]) / 2.0;
					}

					rho_hdr_cold = htfProps.dens(T_sys_c_last, P_field_in);
					rho_hdr_hot = htfProps.dens(T_sys_h_last, 1.e5);  // use an assumed outlet pressure of 1 atm instead of P_field_in
					c_hdr_cold_last = htfProps.Cp(T_sys_c_last)*1000.0; //mjw 1.6.2011 Adding mc_bal to the cold header inertia

					//goto overtemp_iter_flag;
					qq = 0;
					continue;
				}
			}

			if( accept_mode )
			{
				goto calc_final_metrics_goto;
			}
    
freeze_prot_flag: //7   continue    
			    
			if(SolveMode==4)
			{
				// Check if HTF inlet temperature is already greater than or equal to the cold header temperature
				if( T_cold_in_1 >= T_sys_c_last )
				{
					goto freeze_prot_ok;
				}
				
				no_fp = false;
				
				// Set HTF inlet temperature to temperature of cold header
				T_cold_in_1 = T_sys_c_last;		//[C]
			}
			else if(SolveMode==14)
			{
				// assumptions...
				// 1) solver won't enter both modes 3 & 4 in the SAME CALL. so, don't need unique flags or to reset flags
								
				t_tol = 1.5e-4;

				no_fp = false;

				// Energy gain in HTF by boosting the inlet temperature
				double E_fp_field = std::numeric_limits<double>::quiet_NaN(); 
				
				if( is_using_input_gen )
					E_fp_field = m_dot_htf_tot*c_hdr_cold*(T_cold_in_1 - T_sys_h)*dt;       //[kg/s]*[J/kg-K]*[K] = [W]*[s] = [J]  Freeze protection energy required
				else
					E_fp_field = m_dot_htf_tot*c_hdr_cold*(T_cold_in_1 - T_cold_in)*dt;       //[kg/s]*[J/kg-K]*[K] = [W]*[s] = [J]  Freeze protection energy required


				// Energy losses in field
				double E_field_loss_tot = 0.;

				for( int i = 0; i<nSCA; i++ )
				{
					E_field_loss_tot += q_loss_SCAtot[i] * 1.e-6;             //*float(nLoops);
				}

				E_field_loss_tot *= 1.e-6 * dt;

				double E_field_pipe_hl = 2*Runner_hl_hot_tot + float(nfsec)*Header_hl_hot_tot + 2*Runner_hl_cold_tot + float(nfsec)*Header_hl_cold_tot + float(nLoops)*Intc_hl;

				E_field_pipe_hl *= dt;		//[J]

				E_field_loss_tot += E_field_pipe_hl;

				err = (E_field_loss_tot - E_fp_field) / E_field_loss_tot;

				if( fabs(err) <= t_tol )
					goto freeze_prot_ok;

				if( (fp_lowflag) && (fp_upflag) )
				{
					if( err > 0.0 )      //Outlet too low, set lower limit of inlet temperature
					{
						T_in_lower = T_cold_in_1;
						y_fp_lower = err;
					}
					else                     //Outlet too high, set upper limit of inlet temperature
					{
						T_in_upper = T_cold_in_1;
						y_fp_upper = err;
					}
					T_cold_in_1 = (y_fp_upper) / (y_fp_upper - y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
				}
				else
				{
					if( is_using_input_gen && T_in_lower != T_in_lower && T_in_upper != T_in_upper )
					{
						T_cold_in_1 = T_fp;
						T_in_lower = T_cold_in_1;
					}
					else
					{
						if( err > 0.0 )      //Outlet too low, set lower limit of inlet temperature
						{
							T_in_lower = T_cold_in_1;
							y_fp_lower = err;
							fp_lowflag = true;
							upmult = 0.50;
						}
						else                    //Outlet too high, set upper limit of inlet temperature
						{
							T_in_upper = T_cold_in_1;
							y_fp_upper = err;
							fp_upflag = true;
							upmult = 0.50;
						}

						if( (fp_lowflag) && (fp_upflag) )    //if results of bracket are defined, use false position
						{
							T_cold_in_1 = (y_fp_upper) / (y_fp_upper - y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
						}
						else                             //if not, recalculate value based on approximate energy balance
						{
							T_cold_in_1 = T_cold_in_1 + 40.0;   //Will always start low, so fp_lowflag = true so until fp_upflag = true { increase inlet temperature
							qq = 0;
						}
					}
				}
			}			
			else if(SolveMode==3)  //Solve to find (increased) inlet temperature that keeps outlet temperature above freeze protection temp
			{    
				t_tol = 1.5e-4;		//12.29.2014, twn: decreases oscillation in freeze protection energy because a smaller deltaT governs it

				if( (no_fp) && ((T_loop_outX > T_fp) || fabs(T_fp - T_loop_outX)/T_fp <= t_tol) )
					goto freeze_prot_ok; //goto 9
                
				no_fp   = false;
        
				err = (T_fp - T_loop_outX) / T_fp;
        
				if(fabs(err) <= t_tol) 
					goto freeze_prot_ok; //goto 9

				if((fp_lowflag) && (fp_upflag))
				{
					if(err > 0.0)      //Outlet too low, set lower limit of inlet temperature
					{
						T_in_lower  = T_cold_in_1;
						y_fp_lower  = err;
					} 
					else                     //Outlet too high, set upper limit of inlet temperature
					{
						T_in_upper  = T_cold_in_1;
						y_fp_upper  = err;
					}
					T_cold_in_1 = (y_fp_upper)/(y_fp_upper-y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
				} 
				else 
				{
					if(is_using_input_gen && T_in_lower != T_in_lower && T_in_upper != T_in_upper)
					{
						T_cold_in_1 = T_fp;
						T_in_lower = T_cold_in_1;
					}					
					else
					{
						if( err > 0.0 )      //Outlet too low, set lower limit of inlet temperature
						{
							T_in_lower = T_cold_in_1;
							y_fp_lower = err;
							fp_lowflag = true;
							upmult = 0.50;
						}
						else                    //Outlet too high, set upper limit of inlet temperature
						{
							T_in_upper = T_cold_in_1;
							y_fp_upper = err;
							fp_upflag = true;
							upmult = 0.50;
						}

						if( (fp_lowflag) && (fp_upflag) )    //if results of bracket are defined, use false position
						{
							T_cold_in_1 = (y_fp_upper) / (y_fp_upper - y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
						}
						else                             //if not, recalculate value based on approximate energy balance
						{
							if(T_cold_in_1 - T_cold_in > 60.0)
							{
								goto freeze_prot_ok;
							}
							T_cold_in_1 = T_cold_in_1 + 10.0;   //Will always start low, so fp_lowflag = true so until fp_upflag = true { increase inlet temperature  
							qq = 0;
						}
					}
				}    
    
			} 
			else    //Solve to find mass flow that results in target outlet temperature
			{     
				err = (T_loop_out - T_loop_outX)/T_loop_out;
        
				if(m_dot_htf==m_dot_htfmin)
				{
					if(T_loop_outX < T_fp + 10.0)         //freeze protection mode
					{
						if( T_loop_outX < T_fp )
						{
							SolveMode = 3;
							goto freeze_prot_flag; //goto 7
						}
						else
						{
							SolveMode = 4;
							goto freeze_prot_flag; 
						}
					}
					else if(err>0.0)
					{
						goto freeze_prot_ok; //goto 9      //M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration    
					}
				}
        
				//if((m_dot_htf==m_dot_htfmin) && (err>0.0))    goto 9      //M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration    

				// ****************************************************   
				// ***** Hybrid False Position Iteration Method********  
				// ****************************************************
				//Determine next guess for mass flow rate.  Want to use false position method, but it requires that the *results* (err in this case) at both ends of the bracket are known.  We have
				//defined a bracket but not the results.  Use the initial mass flow rate guess to get the results at one } of a new bracket.  { calculate a new mass flow rate based on the appoximation 
				//We eventually want a result for undefined result in the bracket.  After 2 iterations using approximation without defining bracket, switch to bisection.
				//Once results for both sides are { defined, "lowflag" and "upflag" will be true, and false position method will be applied.
				if((lowflag) && (upflag))
				{
					if(err > 0.0)
					{
						m_dot_upper = m_dot_htf;
						y_upper     = err;
					}
					else
					{
						m_dot_lower = m_dot_htf;
						y_lower     = err;
					}
					m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				} 
				else 
				{
        			if(err > 0.0)      //Prescribed is greater than calculated, so decrease mass flow, so set upper limit
					{
						m_dot_upper = m_dot_htf;
						y_upper     = err;
						upflag      = true;
						upmult      = 0.50;
					}
					else                    //Presribed is less than calculated, so increase mass flow, so set lower limit
					{
						m_dot_lower = m_dot_htf;
						y_lower     = err;
						lowflag     = true;
						upmult      = 0.50;
					}
            
					if((lowflag) && (upflag))  //if results of bracket are defined, use false position
					{
						m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
					}
					else                            //if not, recalculate value based on approximate energy balance
					{	
						if(qq<3)
						{
							c_htf_ave = htfProps.Cp((T_loop_out+T_loop_in)/2.0)*1000.;    //Specific heat
							double qsum = 0.;
							for(int i=0; i<nSCA; i++){qsum += q_abs_SCAtot[i]; }
							m_dot_htfX = qsum/(c_htf_ave*(T_loop_out - T_loop_in));
							m_dot_htfX = max( m_dot_htfmin, min( m_dot_htfX, m_dot_htfmax));
						}
						else
						{
							m_dot_htfX = 0.5*m_dot_upper + 0.5*m_dot_lower;
						}
					}
				}

				// ***************************************************************************
				// ****** } Hyrbid False Position Iteration Method *************************
				// ***************************************************************************

				if(m_dot_lower > m_dot_htfmax)      //Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
				{
					break;
				}

				if(m_dot_upper < m_dot_htfmin)      //Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
				{
					m_dot_htfX = m_dot_htfmin;
					SolveMode = 3;
				}
    
			}
    
		//    if(ncall > 10){
		//        exit
		//    }
    
		}

freeze_prot_ok:		//9 continue
		
		if( is_using_input_gen && !accept_mode )
		{	
			// 12.29.2014 twn:
			// If the trough is model is called as a stand-alone, but is solving for mass flow rate, then need a method to enter recirculation when outlet temperature and useful energy are too low
			// Choosing here to enter recirculation when the outlet temperature is less than the user specified inlet temperature. Then, if there is no freeze-protection adjustment of the inlet temperature
			// the code should iterate until the inlet and outlet temperatures are roughly equal.
			// Could consider implementing a faster, more robust method than successive substitution to solve for the inlet temperature.
			if( T_sys_h < T_cold_in )
			{
				if( no_fp && fabs(T_cold_in_1 - T_sys_h) / T_sys_h > 0.001 )
				{
					T_cold_in_1 = T_sys_h;
					goto overtemp_iter_flag;
				}
			}
		}

		E_fp_tot = 0.0;
		if(! no_fp)
		{
			if( is_using_input_gen )
				E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_sys_h);       //[kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
			else
				E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in);       //[kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
		}

		if(qq>29)
		{
			message(TCS_ERROR, "Mass Flow Rate Convergence Error");
			return -1;
		}

		m_dot_htfX = m_dot_htf;              //for calls back to the temperature loop during the same run, this ensures that the first mass flow rate used will be the last mass flow rate

		// ******************************************************************
		// Special condition checks
		// ******************************************************************

		//After convergence, check to see if the HTF flow rate is over the maximum level

post_convergence_flag: //11 continue
		if(((m_dot_htf > m_dot_htfmax) && (dfcount<5)) || ((defocus<1.0) && (dfcount==1)))
		{

			//if so, the field defocus control must be engaged. for(int this by calculating the maximum
			//amount of absorbed energy that corresponds to the maximum mass flow rate
    
			//Now determine the number of SCA's that must be defocused to meet this absorption level
			double qsum =0.;
			for(int i=0; i<nSCA; i++){ qsum += q_abs_SCAtot[i]; }
			double q_check = max(qsum,0.0); //limit to positive
			//mjw 11.4.2010: the max Q amount is based on the defocus control    
			if(dfcount==1) q_abs_maxOT = min(q_check*defocus,c_htf_ave*m_dot_htfmax*(T_loop_out - T_loop_in_des));  //mjw 11.30.2010
    
			//Select the method of defocusing used for this system
			if(fthrctrl == 0)
			{
				//Standard defocusing.. 1 SCA at a time completely defocuses
				int j;
				for(j=0; j<nSCA; j++)
				{
					//Incrementally subtract the losses, but limit to positive absorption values to avoid 
					//accounting for losses from previously defocused SCA's
					//q_check = q_check - max(q_abs_SCAtot(SCADefocusArray[j])-q_loss_SCAtot(SCADefocusArray[j]), 0.0)
					//4/9/11, TN: Don't need to subtract losses: see equation for q_check above
					q_check += - max(q_abs_SCAtot[(int)SCADefocusArray[j]-1], 0.0);
            
					if(q_check <= q_abs_maxOT) break;	//[TO DO] check that j doesn't increment 
				}
        
				//Reassign the flux on each SCA
				for(int i=0; i<j; i++)
				{
					q_SCA[(int)SCADefocusArray[i]-1] = 0.0;
				}
			}
			else if(fthrctrl == 1)
			{
				//Partial defocusing in the sequence specified by the SCADefocusArray
				int j;
				for(j=0; j<nSCA; j++)
				{
					//Incrementally subtract the losses, but limit to positive absorption values to avoid 
					//accounting for losses from previously defocused SCA's
					//q_check = q_check - min(max(q_abs_SCAtot(SCADefocusArray[j])-q_loss_SCAtot(SCADefocusArray[j]), 0.0),q_check-q_abs_maxOT)
					//4/9/11, TN: Don't need to subtract losses: see equation for q_check above
					q_check += -max(q_abs_SCAtot[(int)SCADefocusArray[j]-1], 0.0);
					if(q_check <= q_abs_maxOT) 
						break;
				}
        
				//Reassign the flux on each SCA
				for(int i=0; i<j; i++)
				{
					q_SCA[(int)SCADefocusArray[i]-1] = 0.0;
				}
				
				double tsum = 0.;
				for(int k=j+1; k<nSCA; k++){ tsum += q_abs_SCAtot[(int)SCADefocusArray[k]-1]; }
				
				q_SCA[(int)SCADefocusArray[j]-1] *= (q_check-tsum)/q_abs_SCAtot[(int)SCADefocusArray[j]-1];
			}
			else if(fthrctrl == 2)
			{			
				//Partial defocusing of each SCA in the loop simultaneously. Specified defocus order is disregarded.
				for(int k=0; k<nSCA; k++)
				{
					q_SCA[k] *= min(q_abs_maxOT/max(q_check,1.e-6),1.0);  //MJW 7.23.2010::Limit fraction to 1
				}
			}

			if(q_SCA_tot>0.) 
			{
				double tsum = 0.;
				for(int k=0; k<nSCA; k++){ tsum += q_SCA[k]; }
				SCAs_def = min(tsum/q_SCA_tot,1.0);  //MJW 7.23.2010::Limit fraction to 1
			} 
			else 
			{
				SCAs_def = 1.;
			}
    
			SolveMode=2;  //Mode 2 -> defocusing
			//mjw 11.4.2010 added conditional statement
			if( dfcount<5 )
			{
				T_cold_in_1 = T_cold_in;		//12.29.14 twn: Shouldn't need to worry about freeze protection AND defocus, but, if both occur and defocus is adjusted, should try inlet temperature
				goto overtemp_iter_flag; //Return to recalculate with new defocus arrangement
			}
		}
		//Calculate the amount of defocusing
		if(q_SCA_tot>0.) 
		{
			double tsum = 0.;
			for(int k=0; k<nSCA; k++){ tsum += q_SCA[k]; }
			
			SCAs_def = min(tsum/q_SCA_tot,1.0);  //MJW 7.23.2010::Limit fraction to 1
		} 
		else 
		{
			SCAs_def = 1.;
		}

		// ******************************************************************
		// Determine whether freeze protection is needed for the SCA's
		// ******************************************************************
		//E_fp(:) = 0.0
		//for(int i=1,nSCA
		//    if(T_htf_ave[i] < T_fp) {
		//        CT = SCAInfoArray(i,2)    //Collector type    
		//        HT = SCAInfoArray(i,1)    //HCE type
		//        for(int j=1,nHCEVar
		//            //E_fp[i] = E_fp[i] +  ((T_fp - T_htf_ave[i])*A_cs(HT,j)*L_actSCA[CT]*rho_htf[i]*c_htf[i] + q_loss_SCAtot[i])*HCE_FieldFrac(HT,j)*float(nLoops)
		//            E_fp[i] = E_fp[i] +  q_loss_SCAtot[i]*HCE_FieldFrac(HT,j)*float(nLoops)*3600.0*dt      //[J]
		//        }
		//        T_htf_ave[i] = T_fp
		//        T_htf_in[i] = T_fp
		//        T_htf_out[i] = T_fp
		//    } else {
		//        E_fp[i] = 0.0
		//    }
		//}
		//E_fp_tot = sum(E_fp(1:nSCA))
		////Reset the loop outlet temperature if it's different
		//T_loop_outX = T_htf_out(nSCA)

calc_final_metrics_goto:

		// ******************************************************************************************************************************
		// Calculate the pressure drop across the piping system
		// ******************************************************************************************************************************
		//------Inlet and Outlet
        inlet_state = interconnects[0].State(m_dot_htf * 2, T_loop_in, T_db, P_field_in);
        outlet_state = interconnects[interconnects.size() - 1].State(m_dot_htf * 2, T_loop_outX, T_db, 1.e5);  // assumption for press.
        DP_intc[0] = inlet_state.pressure_drop;
        DP_intc[interconnects.size() - 1] = outlet_state.pressure_drop;

		//-------HCE's (no interconnects)
		DP_tube.fill(0.0);
		for(int j=0; j<nHCEVar; j++)
		{
			for(int i=0; i<nSCA; i++)
			{
				int CT = (int)SCAInfoArray(i,1)-1;    //Collector type    
				int HT = (int)SCAInfoArray(i,0)-1;    //HCE type
        
				DP_tube[i] = DP_tube[i] + PressureDrop(m_dot_htf,T_htf_ave[i],P_field_in - i*P_field_in/nSCA,D_h(HT,j),Rough(HT,j)*D_h(HT, j),
							 L_SCA[CT],0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)*HCE_FieldFrac(HT,j);

			}
		}

		//-------Interconnect's (no HCEs, no inlet nor outlet)
        intc_state = interconnects[1].State(m_dot_htf, inlet_state.temp_out, T_db, inlet_state.pressure_out);
        DP_intc[1] = intc_state.pressure_drop;  // just before first SCA
		for(int i=2; i<interconnects.size() - 1; i++)
        {
            intc_state = interconnects[i].State(m_dot_htf, T_htf_out[i - 2], T_db, intc_state.pressure_out - DP_tube[i - 2]);
            DP_intc[i] = intc_state.pressure_drop;
        }

        //-------IOCOP, HCE's and all other Interconnects
        DP_loop[0] = DP_intc[0];  // inlet
        DP_loop[1] = DP_intc[1];  // before first SCA
        loop_i = 2; sca_i = 0; intc_i = 2;
        while(loop_i < nSCA + interconnects.size() - 1) {
            DP_loop[loop_i++] = DP_tube[sca_i++];
            DP_loop[loop_i++] = DP_intc[intc_i++];
        }
        DP_loop[loop_i] = DP_intc[intc_i];  // outlet


		if( accept_loc == 1 )
			m_dot_htf_tot = m_dot_htf*float(nLoops);
		else
			m_dot_htf_tot = m_dot_htf;


		//-------SGS to field section
		//if(FieldConfig==1.) { //"H" type
		//    x1 = 1.  
		//    //MJW changed length calculation 6-8-2010
		//    x2 = (40.+Row_Distance+(L_SCA(SCAInfoArray(1,2))+Distance_SCA(SCAInfoArray(1,2)))*float(nSCA))  //x2 is calculation for piping length
		//    x3 = 1. //number of expansions
		//} else { //"I" type
		//    x1 = 2.
		//    x2 = 50.
		//    x3 = 0.
		//}

		if( accept_loc == 1 )
		{
			m_dot_run_in = std::numeric_limits<double>::quiet_NaN();

			if( nfsec > 2 )  //mjw 5.4.11 Correct the mass flow for situations where nfsec/2==odd
			{
				m_dot_run_in = m_dot_htf_tot / 2.0 * (1. - float(nfsec % 4) / float(nfsec));
			}
			else
			{
				m_dot_run_in = m_dot_htf_tot / 2.0;
			}

            double x3;
            int elbows_per_xpan = 4;
			m_dot_temp = m_dot_run_in;
			DP_toField = 0.0;
			DP_fromField = 0.0;
			for( int i = 0; i<nrunsec; i++ )
			{
                (i < nrunsec - 1 ? x3 = 1.0 : x3 = 0.0);  // contractions/expansions
                DP_rnr[i] = PressureDrop(m_dot_temp, T_loop_in, P_field_in, D_runner[i], HDR_rough,
                    L_runner[i], 0.0, x3, 0.0, 0.0, N_rnr_xpans[i] * elbows_per_xpan, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0);
                DP_rnr[2*nrunsec - i - 1] = PressureDrop(m_dot_temp, T_loop_outX, 1.e5, D_runner[2 * nrunsec - i - 1], HDR_rough,
                    L_runner[2 * nrunsec - i - 1], x3, 0.0, 0.0, 0.0, N_rnr_xpans[2 * nrunsec - i - 1] * elbows_per_xpan, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0);

				if( i>1 )
					m_dot_temp = max(m_dot_temp - 2.*m_dot_htf_tot / float(nfsec), 0.0);
			}

            //Calculate pressure drop in cold header
			m_dot_hdr_in = m_dot_htf_tot / float(nfsec);
			m_dot_hdr = m_dot_hdr_in;
            double x2 = 0.0;
			for( int i = 0; i<nhdrsec; i++ )
			{
				//Determine whether the particular section has a contraction fitting (at the beginning of the section)
				x2 = 0.0;
				if( i>0 )
				{
					if( D_hdr[i] != D_hdr[i - 1] )
						x2 = 1.;
				}

                DP_hdr[i] = PressureDrop(m_dot_hdr, T_loop_in, P_field_in, D_hdr[i], HDR_rough,
                    L_hdr[i], 0.0, x2, 0.0, 0.0, N_hdr_xpans[i] * 4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
				//if(ErrorFound()) return 1
				//Siphon off header mass flow rate at each loop.  Multiply by 2 because there are 2 loops per hdr section
				m_dot_hdr = max(m_dot_hdr - 2.*m_dot_htf, 0.0);
			}

            //Calculate pressure drop in hot header
            m_dot_hdr = 2.*m_dot_htf;
            for (int i = nhdrsec; i<2*nhdrsec; i++)
            {
                //Determine whether the particular section has an expansion fitting (at the beginning of the section)
                x2 = 0.0;
                if (i>nhdrsec)
                {
                    if (D_hdr[i] != D_hdr[i - 1])
                        x2 = 1.;
                }

                DP_hdr[i] = PressureDrop(m_dot_hdr, T_loop_outX, 1.e5, D_hdr[i], HDR_rough,
                    L_hdr[i], x2, 0.0, 0.0, 0.0, N_hdr_xpans[i] * 4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
                //if(ErrorFound()) return 1
                //Add to header mass flow rate at each loop.  Multiply by 2 because there are 2 loops per hdr section
                m_dot_hdr = m_dot_hdr + 2.*m_dot_htf;
            }

		}

        // Aggregate pressures
        DP_IOCOP = DP_intc[0] + DP_intc[(DP_intc.ncells() - 1) / 2] + DP_intc[DP_intc.ncells() - 1];
        DP_loop_tot = accumulate(DP_tube.data(), DP_tube.data() + DP_tube.ncells(), 0.0) +
            accumulate(DP_intc.data(), DP_intc.data() + DP_intc.ncells(), 0.0) -
            DP_IOCOP;
        DP_hdr_cold = accumulate(DP_hdr.data(), DP_hdr.data() + DP_hdr.ncells() / 2, 0.0);
        DP_hdr_hot = accumulate(DP_hdr.data() + DP_hdr.ncells() / 2, DP_hdr.data() + DP_hdr.ncells(), 0.0);
        DP_toField = accumulate(DP_rnr.data(), DP_rnr.data() + DP_rnr.ncells() / 2, 0.0);
        DP_fromField = accumulate(DP_rnr.data() + DP_rnr.ncells() / 2, DP_rnr.data() + DP_rnr.ncells(), 0.0);


        //std::ofstream logDPs;             // moved to top of call()
        //logDPs.open("logDPs.txt");

        //logDPs << "DP_tube" << "\n";
        //for (int i = 0; i < DP_tube.ncells(); i++) {
        //    logDPs << DP_tube.at(i) << "\n";
        //}
        //logDPs << "\n";

        //logDPs << "DP_hdr" << "\n";
        //for (int i = 0; i < DP_hdr.ncells(); i++) {
        //    logDPs << DP_hdr.at(i) << "\n";
        //}
        //logDPs << "\n";

        //logDPs << "DP_rnr" << "\n";
        //for (int i = 0; i < DP_rnr.ncells(); i++) {
        //    logDPs << DP_rnr.at(i) << "\n";
        //}

        ////log.flush();
        //logDPs << "\n";
        //logDPs.close();
        

		if( accept_loc == 1 )
		{
			// The total pressure drop in all of the piping
			DP_tot = (DP_loop_tot + DP_hdr_cold + DP_hdr_hot + DP_fromField + DP_toField + DP_IOCOP);
            
            // Convert pressure drops to gauge pressures
            P_rnr[0] = DP_tot;
            for (int i = 1; i < 2*nrunsec; i++) {
                P_rnr[i] = P_rnr[i - 1] - DP_rnr[i - 1];
                if (i == nrunsec) { P_rnr[i] -= (DP_hdr_cold + DP_loop_tot + DP_IOCOP + DP_hdr_hot); }
            }
            P_hdr[0] = P_rnr[nrunsec - 1] - DP_rnr[nrunsec - 1];    // report pressures for farthest subfield
            for (int i = 1; i < 2*nhdrsec; i++) {
                P_hdr[i] = P_hdr[i - 1] - DP_hdr[i - 1];
                if (i == nhdrsec) { P_hdr[i] -= (DP_loop_tot + DP_IOCOP); }
            }
            P_loop[0] = P_hdr[nhdrsec - 1] - DP_hdr[nhdrsec - 1];   // report pressures for farthest loop
            for (int i = 1; i < nSCA + interconnects.size(); i++) {
                P_loop[i] = P_loop[i - 1] - DP_loop[i - 1];
            }

			// The total pumping power consumption
			W_dot_pump = DP_tot*m_dot_htf_tot / (rho_hdr_cold*eta_pump) / 1000.;  //[kW]

			//The parasitic power consumed by electronics and SCA drives
			if( EqOpteff>0.0 )
			{
				SCA_par_tot = SCA_drives_elec*SCAs_def*float(nSCA*nLoops);
			}
			else
			{
				SCA_par_tot = 0.0;
			}
		}
		else
		{
			// The total pressure drop in all of the piping
			DP_tot = (DP_loop_tot + DP_IOCOP);

            // Convert pressure drops to gauge pressures
            P_loop[0] = DP_tot;
            for (int i = 1; i < nSCA + interconnects.size(); i++) {
                P_loop[i] = P_loop[i - 1] - DP_loop[i - 1];
            }

			// The total pumping power consumption
			W_dot_pump = DP_tot*m_dot_htf / (rho_hdr_cold*eta_pump) / 1000.;  //[kW]

			//The parasitic power consumed by electronics and SCA drives 
			if( EqOpteff>0.0 )
			{
				SCA_par_tot = SCA_drives_elec*SCAs_def*float(nSCA);
			}
			else
			{
				SCA_par_tot = 0.0;
			}
		}


		// ******************************************************************
		// Calculate the system transient temperature for the next time step
		// ******************************************************************
		//First, calculate the amount of energy absorbed during the time step that didn't contribute to 
		//heating up the solar field
		E_avail_tot = 0.;
		E_loop_accum = 0.;
		E_int_sum = 0.;
		for(int i=0; i<nSCA; i++)
		{
			E_avail_tot += E_avail[i];  //[J]
			E_loop_accum += E_accum[i]; //[J]
			E_int_sum += E_int_loop[i]; //[J]
		}

		E_field = E_int_sum;

		// Average properties
		rho_ave = htfProps.dens((T_loop_outX + T_sys_c) / 2.0, P_field_in/2); //kg/m3
		c_htf_ave = htfProps.Cp((T_sys_h + T_cold_in_1) / 2.0)*1000.0;  //MJW 12.7.2010

		if( accept_loc == 1 )		// Consider entire internal energy of entire field, not just the loop
		{
			E_avail_tot *= float(nLoops);
			E_loop_accum *= float(nLoops);
			E_field *= float(nLoops);

			//Calculate the HTF mass in the header, balance of field piping, piping to&from the steam generator (SGS) 
			//The mass of HTF in the system will be calculated based on the design loop inlet temperature
			//v_tot = v_hot + v_cold	//cc--> not used
			c_hdr_cold = htfProps.Cp(T_loop_in)* 1000.;
			c_hdr_hot = htfProps.Cp(T_loop_outX)* 1000.;

			//Half of the plant thermal mass must be heated up to the startup temperature (think hot header, hot half of heat
			//exchangers) and the other half must be heated up to the design loop inlet temperature
			//MJW 12.8.2010 modified startup temp calc to be based on previous system temperature
			//MJW 12.14.2010 Limit to positive to avoid step-to-step oscillation introduced by using previous step. 
			//.. This may cause a minor underestimation of annual energy output (<<.5%).
			E_hdr_accum = (v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - T_sys_h_last) + //Hot half
				max((v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - T_sys_c_last), 0.0);   //cold half
			
			if( !is_using_input_gen )
				E_bal_startup = max(E_hdr_accum, 0.0); //cold half
			else
				E_bal_startup = E_hdr_accum; //cold half

			//mjw 1.17.2011 Calculate the total energy content of the solar field relative to a standard ambient temp. of 25[C]			
			E_field += ((v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - 298.150) +       //hot header and piping
				(v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - 298.150));   //cold header and piping

			//6/14/12, TN: Redefine pipe heat losses with header and runner components to get total system losses
			Pipe_hl_hot = 2*Runner_hl_hot_tot + float(nfsec)*Header_hl_hot_tot;
			Pipe_hl_cold = 2*Runner_hl_cold_tot + float(nfsec)*Header_hl_cold_tot;

			Pipe_hl = Pipe_hl_hot + Pipe_hl_cold + float(nLoops)*Intc_hl;

			if( !is_using_input_gen )
				E_avail_tot = max(E_avail_tot - Pipe_hl*dt, 0.0);    //[J] 11/1/11 TN: Include hot and cold piping losses in available energy calculation
			else
				E_avail_tot = E_avail_tot - Pipe_hl*dt;    //[J] 11/1/11 TN: Include hot and cold piping losses in available energy calculation

			E_avail_tot = max(E_avail_tot - E_bal_startup, 0.0);  //[J]
		}

		// ******************************************************************
		// Calculate final output values
		// ******************************************************************
		DP_tot = DP_tot * 1.e-5; //[bar]
		
		if( !is_using_input_gen )
		{
			//Calculate the thermal power produced by the field
            q_avail = E_avail_tot / (dt)*1.e-6;  //[MW]
			//Calculate the available mass flow of HTF
			m_dot_avail = max(q_avail*1.e6 / (c_htf_ave*(T_sys_h - T_cold_in_1)), 0.0); //[kg/s]
            m_dot_field_htf = m_dot_avail;
            if (recirculating == true)
			{
                q_avail = 0.0;
				m_dot_avail = 0.0;
                m_dot_field_htf = m_dot_htf_tot;
                //m_dot_field_htf = 0;
			}
		}
		else
		{
            recirculating = false;
			q_avail = E_avail_tot / (dt)*1.e-6;  //[MW]
			//Calculate the available mass flow of HTF
			m_dot_avail = max(q_avail*1.e6 / (c_htf_ave*(T_sys_h - T_cold_in_1)), 0.0); //[kg/s]     
		}

		//Dumped energy
		q_dump = Ap_tot*I_b*EqOpteff*(1.-SCAs_def)/1.e6;  //MW

		//Total field performance
		q_field_delivered = m_dot_htf_tot * c_htf_ave * (T_sys_h - T_cold_in_1) / 1.e6; //MJW 1.11.11 [MWt]
		
		if(I_b*CosTh_ave == 0.)	//cc--> Adding case for zero output. Was reporting -Infinity in original version
		{
			eta_thermal = 0.;
		}
		else
		{
			eta_thermal = q_field_delivered / (I_b*CosTh_ave*Ap_tot / 1.e6);  //MJW 1.11.11	
		}
		
set_outputs_and_return:

		//---------Do unit conversions and final calculations-------------
		 
		double T_sys_h_out = T_sys_h - 273.15;			//[C] from K
		double m_dot_avail_out = m_dot_avail*3600.;		//[kg/hr] from kg/s
        double m_dot_field_htf_out = m_dot_field_htf*3600.;  //[kg/hr] from kg/s
		double W_dot_pump_out = W_dot_pump/1000.;		//[MW] from kW
		double E_fp_tot_out = E_fp_tot*1.e-6;			//[MW] from W
		double T_sys_c_out = T_sys_c - 273.15;			//[C] from K
		double EqOpteff_out = EqOpteff*CosTh_ave;
		double m_dot_htf_tot_out = m_dot_htf_tot *3600.;	//[kg/hr] from kg/s
		double E_bal_startup_out = E_bal_startup/(dt*1.e6);	//[MW] from J
		q_inc_sf_tot = Ap_tot*I_b/1.e6;
		q_abs_tot = 0.;
		q_loss_tot = 0.;
		q_loss_spec_tot = 0.;

		for(int i=0; i<nSCA; i++)
		{
			q_abs_tot += q_abs_SCAtot[i] * 1.e-6;               //*float(nLoops);
			q_loss_tot += q_loss_SCAtot[i] * 1.e-6;             //*float(nLoops);
			q_loss_spec_tot += q_1abs_tot[i]/float(nSCA);
		}
		
		if(accept_loc == 1)
		{
			q_abs_tot *= float(nLoops);
			q_loss_tot *= float(nLoops);
		}

		double
			SCA_par_tot_out = SCA_par_tot * 1.e-6,
			Pipe_hl_out = Pipe_hl * 1.e-6,
			Theta_ave_out = Theta_ave/d2r,
			CosTh_ave_out = CosTh_ave;
		
		dni_costh = I_b*CosTh_ave;
		qinc_costh = dni_costh * Ap_tot/1.e6;
		t_loop_outlet = T_loop_outX - 273.15;
		
		double
			E_loop_accum_out = E_loop_accum / 3.6e9,
			E_hdr_accum_out = E_hdr_accum / 3.6e9;
		
		E_tot_accum = E_loop_accum_out + E_hdr_accum_out;
		
		double E_field_out = E_field / 3.6e9;

        if (calc_design_pipe_vals) {
            T_rnr_des_out.resize(2 * nrunsec);
            P_rnr_des_out.resize(2 * nrunsec);
            T_hdr_des_out.resize(2 * nhdrsec);
            P_hdr_des_out.resize(2 * nhdrsec);
            T_loop_des_out.resize(2 * nSCA + 3);
            P_loop_des_out.resize(2 * nSCA + 3);

            for (int i = 0; i < 2 * nrunsec; i++) {
                T_rnr_des_out[i] = T_rnr[i] - 273.15; // K to C
                P_rnr_des_out[i] = P_rnr[i] / 1.e5;   // Pa to bar
            }
            for (int i = 0; i < 2 * nhdrsec; i++) {
                T_hdr_des_out[i] = T_hdr[i] - 273.15;
                P_hdr_des_out[i] = P_hdr[i] / 1.e5;
            }
            for (int i = 0; i < 2 * nSCA + 3; i++) {
                T_loop_des_out[i] = T_loop[i] - 273.15;
                P_loop_des_out[i] = P_loop[i] / 1.e5;
            }

            value(O_T_FIELD_IN_AT_DSN, T_rnr_des_out.at(0));
            value(O_T_FIELD_OUT_AT_DSN, T_rnr_des_out.at(T_rnr_des_out.ncells() - 1));  // need to still account for heat loss in this last pipe
            value(O_P_FIELD_IN_AT_DSN, P_rnr_des_out.at(0));                            // need to still account for pressure loss in this last pipe

            // wait to output arrays until TES/PB sizing has finished so P can be adjusted
        }

        DP_SGS_1 = value(P_DP_SGS_1);
        if (!SGS_sizing_adjusted && DP_SGS_1 >= 0) {
            // Adjust design P values with pressure drop in first section of TES/PB (after controller model has finished sizing)

            for (int i = 0; i < P_rnr_des_out.ncells(); i++) {
                P_rnr_des_out.at(i) += DP_SGS_1;
            }

            for (int i = 0; i < P_hdr_des_out.ncells(); i++) {
                P_hdr_des_out.at(i) += DP_SGS_1;
            }

            for (int i = 0; i < P_loop_des_out.ncells(); i++) {
                P_loop_des_out.at(i) += DP_SGS_1;
            }

            double *runner_temp_design = allocate(O_RUNNER_T_DSN, (int)T_rnr_des_out.ncells());
            std::copy(T_rnr_des_out.data(), T_rnr_des_out.data() + T_rnr_des_out.ncells(), runner_temp_design);
            double *runner_pressure_design = allocate(O_RUNNER_P_DSN, (int)P_rnr_des_out.ncells());
            std::copy(P_rnr_des_out.data(), P_rnr_des_out.data() + P_rnr_des_out.ncells(), runner_pressure_design);
            double *header_temp_design = allocate(O_HEADER_T_DSN, (int)T_hdr_des_out.ncells());
            std::copy(T_hdr_des_out.data(), T_hdr_des_out.data() + T_hdr_des_out.ncells(), header_temp_design);
            double *header_pressure_design = allocate(O_HEADER_P_DSN, (int)P_hdr_des_out.ncells());
            std::copy(P_hdr_des_out.data(), P_hdr_des_out.data() + P_hdr_des_out.ncells(), header_pressure_design);
            double *loop_temp_design = allocate(O_LOOP_T_DSN, (int)T_loop_des_out.ncells());
            std::copy(T_loop_des_out.data(), T_loop_des_out.data() + T_loop_des_out.ncells(), loop_temp_design);
            double *loop_pressure_design = allocate(O_LOOP_P_DSN, (int)P_loop_des_out.ncells());
            std::copy(P_loop_des_out.data(), P_loop_des_out.data() + P_loop_des_out.ncells(), loop_pressure_design);

            SGS_sizing_adjusted = true;
        }
		//------------------------------------------------------------------

		

		//Set outputs
		value(O_T_SYS_H, T_sys_h_out);				//[C] Solar field HTF outlet temperature
		value(O_M_DOT_AVAIL, m_dot_avail_out);		//[kg/hr] HTF mass flow rate from the field
        value(O_M_DOT_FIELD_HTF, m_dot_field_htf_out);  //[kg/hr] HTF mass flow rate from the field, including when recirculating
		value(O_Q_AVAIL, q_avail);					//[MWt] Thermal power produced by the field
		value(O_DP_TOT, DP_tot);					//[bar] Total HTF pressure drop
		value(O_W_DOT_PUMP, W_dot_pump_out);		//[MWe] Required solar field pumping power
		value(O_E_FP_TOT, E_fp_tot_out);			//[MW] Freeze protection energy
		value(O_QQ, qq);							//[none] Number of iterations required to solve
		value(O_T_SYS_C, T_sys_c_out);				//[C] Collector inlet temperature
		value(O_EQOPTEFF, EqOpteff_out);			//[none] Collector equivalent optical efficiency
		value(O_SCAS_DEF, SCAs_def);				//[none] The fraction of focused SCA's
		value(O_M_DOT_HTF_TOT, m_dot_htf_tot_out);	//[kg/hr] The actual flow rate through the field..
		value(O_E_BAL_STARTUP, E_bal_startup_out);	//[MWt] Startup energy consumed
		value(O_Q_INC_SF_TOT, q_inc_sf_tot);		//[MWt] Total power incident on the field
		value(O_Q_ABS_TOT, q_abs_tot);				//[MWt] Total absorbed energy
		value(O_Q_LOSS_TOT, q_loss_tot);			//[MWt] Total receiver thermal and optical losses
		value(O_M_DOT_HTF, m_dot_htf);				//[kg/s] Flow rate in a single loop
		value(O_Q_LOSS_SPEC_TOT, q_loss_spec_tot);	//[W/m] Field-average receiver thermal losses (convection and radiation)
		value(O_SCA_PAR_TOT, SCA_par_tot_out);		//[MWe] Parasitic electric power consumed by the SC
		value(O_PIPE_HL, Pipe_hl_out);				//[MWt] Pipe heat loss in the header and the hot runner
		value(O_Q_DUMP, q_dump);					//[MWt] Dumped thermal energy
		value(O_THETA_AVE, Theta_ave_out);			//[deg] Field average theta value
		value(O_COSTH_AVE, CosTh_ave_out);			//[none] Field average costheta value
		value(O_IAM_AVE, IAM_ave);					//[none] Field average incidence angle modifier
		value(O_ROWSHADOW_AVE, RowShadow_ave);		//[none] Field average row shadowing loss
		value(O_ENDLOSS_AVE, EndLoss_ave);			//[none] Field average end loss
		value(O_DNI_COSTH, dni_costh);				//[W/m2] DNI_x_CosTh
		value(O_QINC_COSTH, qinc_costh);			//[MWt] Q_inc_x_CosTh
		value(O_T_LOOP_OUTLET, t_loop_outlet);		//[C] HTF temperature immediately subsequent to the loop outlet
		value(O_C_HTF_AVE, c_htf_ave);				//[J/kg-K] Average solar field specific heat
		value(O_Q_FIELD_DELIVERED, q_field_delivered);		//[MWt] Total solar field thermal power delivered
		value(O_ETA_THERMAL, eta_thermal);			//[none] Solar field thermal efficiency (power out/ANI)
		value(O_E_LOOP_ACCUM, E_loop_accum_out);	//[MWht] Accumulated internal energy change rate in the loops ONLY
		value(O_E_HDR_ACCUM, E_hdr_accum_out);		//[MWht] Accumulated internal energy change rate in the headers/SGS
		value(O_E_TOT_ACCUM, E_tot_accum);			//[MWht] Total accumulated internal energy change rate
		value(O_E_FIELD, E_field_out);				//[MWht] Accumulated internal energy in the entire solar field
		value(O_T_C_IN_CALC, T_cold_in_1 - 273.15);	//[C] Calculated cold HTF inlet temperature - used in freeze protection and for stand-alone model in recirculation
        value(O_DEFOCUS, defocus);                  //[-] Relative defocus for passing back to the controller to ensure TCS convergence

		return 0;
	}

	virtual int converged(double time){
		/* 
		-- Post-convergence call --

		Update values that should be transferred to the next time step
		*/

		ss_init_complete = true;
        defocus = 1.;

		T_sys_c_last = T_sys_c;   //Get T_sys from the last timestep
		T_sys_h_last = T_sys_h;
		for(int i=0; i<nSCA; i++){
			T_htf_in0[i] = T_htf_in[i];
			T_htf_out0[i] = T_htf_out[i];
			T_htf_ave0[i] = (T_htf_in0[i] + T_htf_out0[i])/2.0;
		}


		return 0;
	}

	// ------------------------------------------ supplemental methods -----------------------------------------------------------

    int size_hdr_lengths(double L_row_sep, int Nhdrsec, int offset_hdr_xpan, int Ncol_loops_per_xpan, double L_hdr_xpan,
        util::matrix_t<double> &L_hdr, util::matrix_t<double> &N_hdr_xpans, bool custom_lengths = false) {
        // Parameters:
        // L_row_sep			distance between SCA rows, centerline to centerline
        // Nhdrsec				number of header sections (tee-conns.) per field section			
        // offset_hdr_xpan		location of first header expansion loop							
        // Ncol_loops_per_xpan	number of collector loops per expansion loop						
        // L_hdr_xpan			combined perpendicular lengths of each header expansion loop
        // custom_lengths       should the lengths be input instead of calculated?

        // Outputs :
        // &L_hdr				length of the header sections
        // &N_hdr_xpans			number of expansion loops in the header section

        if (!custom_lengths) L_hdr.fill(2 * L_row_sep);
        N_hdr_xpans.fill(0);
        for (int i = 0; i < Nhdrsec; i++)
        {
            if ((i - offset_hdr_xpan) % Ncol_loops_per_xpan == 0)
            {
                N_hdr_xpans[i]++;                                 // start with cold loop
                N_hdr_xpans[2 * Nhdrsec - 1 - i]++;               // pair hot loop
                if (!custom_lengths) {
                    L_hdr[i] += L_hdr_xpan;                       // cold loop
                    L_hdr[2 * Nhdrsec - 1 - i] += L_hdr_xpan;     // pair hot loop
                }
            }
        }

        return 0;
    }

    int size_rnr_lengths(int Nfieldsec, double L_rnr_pb, int Nrnrsec, int ColType, double northsouth_field_sep,
        const double *L_SCA, int min_rnr_xpans, const double *L_gap_sca, double Nsca_loop,
        double L_rnr_per_xpan, double L_rnr_xpan, util::matrix_t<double> &L_runner, util::matrix_t<double> &N_rnr_xpans,
        bool custom_lengths = false) {
        // Parameters:
        // Nfieldsec				number of field sections
        // L_rnr_pb				    length of runner piping in and around the power block
        // Nrnrsec					the number of unique runner diameters
        // ColType	                the collector type
        // northsouth_field_sep	    north-south separation between subfields. 0=SCAs are touching
        // L_SCA[]					the length of the SCAs
        // min_rnr_xpans			minimum number of expansion loops per single-diameter runner section
        // L_gap_sca[]			    gap distance between SCAs in the same row
        // Nsca_loop				number of SCAs in a loop
        // L_rnr_per_xpan			threshold length of straight runner pipe without an expansion loop
        // L_rnr_xpan				combined perpendicular lengths of each runner expansion loop
        // custom_lengths           should the lengths be input instead of calculated?

        // Outputs :
        // &L_runner				length of the runner sections
        // &N_rnr_xpans			    number of expansion loops in the runner section
        
        // Assume there are two field subsections per span, then if there's an even number of spans in the field,
        //    we count the first header section as half - length.I.e., if a field looks like this:
        //     (1)        (2)
        //    |||||||   |||||||
        //    ---------------- -
        //    ||||||| : |||||||
        //            :
        //           [P]
        //            :
        //    ||||||| : |||||||
        //    ---------------- -
        //    |||||||   |||||||
        //      (3)        (4)
        // Then the field has 4 subfields and two spans.The runner pipe(:) is half the distance between the two spans.
        // If the number of subfields were 6 (3 spans), the two runner pipe segments would both be equal to the full
        // distance between spans.

        double x1;
        int j;
        double L_runner_linear;

            if (Nfieldsec / 2 % 2 == 1)
            {
                x1 = 2.;     //the first runners are normal
            }
            else
            {
                x1 = 1.;     //the first runners are short
            }

        if (!custom_lengths) {
            L_runner[0] = L_rnr_pb;  // Runner piping in and around the power block
            L_runner[2 * Nrnrsec - 1] = L_rnr_pb;  // assume symmetric runners
        }
        N_rnr_xpans[0] = 0;
        N_rnr_xpans[2 * Nrnrsec - 1] = N_rnr_xpans[0];
        if (Nrnrsec > 1)
        {
            L_runner_linear = 0;  // Runner length minus expansion loops
            for (int i = 1; i < Nrnrsec; i++)
            {
                j = ColType - 1;
                L_runner_linear = x1 * (northsouth_field_sep + (L_SCA[j] + L_gap_sca[j])*float(Nsca_loop) / 2.);  // no expansion loops
                N_rnr_xpans[i] = max(min_rnr_xpans, (int)CSP::nint(L_runner_linear / L_rnr_per_xpan));
                N_rnr_xpans[2 * Nrnrsec - i - 1] = N_rnr_xpans[i];
                if (!custom_lengths) {
                    L_runner[i] = L_runner_linear + L_rnr_xpan * N_rnr_xpans;
                    L_runner[2 * Nrnrsec - i - 1] = L_runner[i];    // assume symmetric runners
                }
                x1 = 2.;   //tn 4.25.11 Default to 2 for subsequent runners
            }
        }

        return 0;
    }

	/*
	This subroutine contains the trough detailed plant model.  The collector field is modeled
	using an iterative solver.
	This code was written for the National Renewable Energy Laboratory
	Copyright 2009-2010
	Author: Mike Wagner

	Subroutine Inputs (and parameters)
	 ----------------------------------------------------------------------------------------------------------------------
	 Nb | Variable             | Description                                             | Input  Units   | Internal Units 
	 ---|----------------------|---------------------------------------------------------|----------------|----------------
	 1  | T_1_in               | Receiver inlet temperature                              |                |
	 2  | m_dot                | Heat transfer fluid mass flow rate                      |                |
	 3  | T_amb                | Ambient dry-bulb temperature                            |                |
	 4  | T_sky                | Sky temperature                                         |                |
	 5  | v_6                  | Ambient wind velocity                                   |                |
	 6  | P_6                  | Ambient atmospheric pressure                            |                |
	 7  | q_i                  | Total incident irradiation on the receiver              |                |
	 8  | A_cs                 | Internal absorber tube cross-sectional area             |                |
	 9  | D_2                  | Internal absorber tube diameter                         |                |
	 10 | D_3                  | External absorber tube diameter                         |                |
	 11 | D_4                  | Internal glass envelope diameter                        |                |
	 12 | D_5                  | External glass envelope diameter                        |                |
	 13 | D_p                  | (optional) Plug diameter                                |                |
	 14 | D_h                  | Absorber tube hydraulic diameter                        |                |
	 15 | eps_mode             | Interpolation mode for the emissivity (1=table,2=fixed) |                |
	 16 | xx                   | Array of temperature values for emissivity table        |                |
	 17 | yy                   | Array of emissivity values for table                    |                |
	 18 | nea                  | Number of entries in the emissivity table               |                |
	 19 | L_actSCA             | Length of the active receiver surface                   |                |
	 20 | single_point         | Logical flag - is the calculation for a single point?   |                |
	 21 | Epsilon_32           | Constant value for emissivity if table isn't used       |                |
	 22 | Epsilon_4            | Envelope inner surface emissivity                       |                |
	 23 | EPSILON_5            | Envelope outer surface emissivity                       |                |
	 24 | Alpha_abs            | Absorber tube absorptance                               |                |
	 25 | alpha_env            | Envelope absorptance                                    |                |
	 26 | ColOptEff            | Collector optical efficiency                            |                |
	 27 | Tau_envelope         | Total envelope transmittance                            |                |
	 28 | P_a                  | Annulus gas pressure                                    | torr           |
	 29 | Flow_type            | Flag indicating the presence of an internal plug        |                |
	 30 | AnnulusGas           | Annulus gas type                                        |                |
	 31 | Fluid                | Heat transfer fluid type                                |                |
	 32 | AbsorberMaterial     | Absorber material type                                  |                |
	 33 | time                 | Simulation time                                         |                |

	Subroutine outputs 
	 ----------------------------------------------------------------------------------------------------------------------
	 Nb | Variable             | Description                                             | Input  Units   | Internal Units 
	 ---|----------------------|---------------------------------------------------------|----------------|----------------
	 1  | q_heatloss           | Total heat loss from the receiver                       | W/m            |
	 2  | q_12conv             | Total heat absorption into the HTF                      | W/m            |
	 3  | q_34tot              | Convective and radiative heat loss                      |                |
	 4  | c_1ave               | Specific heat of the HTF across the receiver            | kJ/kg-K        |
	 5  | rho_1ave             | Density of the HTF across the receiver                  |                |

	 ----------------------------------------------------------------------------------------------------------------------
	Forristall Temperature distribution diagram
	*****************************************************
		Fluid (1) ----------->(2)<--Absorber-->(3)<-- Annulus -->(4)<--- Glass  --->(5)<-- Air (6)/Sky (7)


		T_1 = Bulk heat transfer fluid (HTF) temperature 
		T_2 = Absorber Inside surface temperature
		T_3 = Absorber outside surface temperature 
		T_4 = Glass envelope inside surface temperature
		T_5 = Glass envelope outside surface temperature
		T_6 = Ambient temperature
		T_7 = Effective Sky Temperature

		q_12conv = Convection heat transfer rate per unit length between the HTF and the inside of the receiver tube
		q_23cond = Conduction heat transfer rate per unit length through the absorber
		q_34conv = Convection heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
		q_34rad = Radiation heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
		q_45cond = Conduction heat transfer rate per unit length through the glazing
		q_56conv = Convection heat transfer rate per unit length between the glazing outer surface and the ambient air
		q_57rad = Radiation heat transfer rate per unit length between the glazing outer surface and the sky
	----------------------------------------------------------------------------------------------------------------------
	*/
	/*
	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double T_sky, double v_6, double P_6, double q_i, double A_cs, 
		double D_2, double D_3, double D_4, double D_5, double D_p, double D_h, int eps_mode, double xx[], double yy[], int nea,
		double L_actSCA,double single_point,  double Epsilon_32, double Epsilon_4, 
		double EPSILON_5, double Alpha_abs, double alpha_env, double ColOptEff, double Tau_envelope, double P_a, int Flow_type, 
		int AbsorberMaterial, int annulusGas, bool glazingIntact, int Fluid, int ncall, double time,
		//outputs
		double q_heatloss, double q_12conv, double q_34tot, double c_1ave, double rho_1ave)
		*/
	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double T_sky, double v_6, double P_6, double q_i, 
		int hn /*HCE number [0..3] */, int hv /* HCE variant [0..3] */, int ct /*Collector type*/, int sca_num, bool single_point,  int ncall, double time,
		//outputs
		double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave )
	{

		//cc -- note that collector/hce geometry is part of the parent class. Only the indices specifying the
		//		number of the HCE and collector need to be passed here.

		//---Variable declarations------
		bool reguess;
		double T_2, T_3, T_4, T_5, T_6, T_7,v_1, k_23, q_34conv, q_34rad, h_34conv, h_34rad, q_23cond, 
			k_45, q_45cond, q_56conv, h_56conv, q_57rad, q_3SolAbs, q_5solabs, q_cond_bracket, R_45cond,
			T_2g, cp_1, T3_tol, q5_tol, T1_tol, T2_tol, Diff_T3, diff_q5, T_lower, T_upper, 
			q_5out, T_1_out, diff_T1, T_1_ave, T_1_out1, diff_T2, eps_3, q_in_W, T_upper_max, y_upper,
			y_lower, upmult, q5_tol_1, T3_upper, T3_lower, y_T3_upper, y_T3_lower, abs_diffT3;

		bool UPFLAG, LOWFLAG, T3upflag, T3lowflag, is_e_table;
		int qq, q5_iter, T1_iter, q_conv_iter;

		double T_save_tot, colopteff_tot;
		//cc--> note that xx and yy have size 'nea'
		
		//-------

		bool glazingIntact = GlazingIntact(hn,hv); //.at(hn, hv);

		//---Re-guess criteria:---
		if(time<=2) goto lab_reguess;
		
		if(((int)reguess_args[0] == 1) != GlazingIntact(hn,hv)) goto lab_reguess;	//glazingintact state has changed
		
		if(P_a(hn,hv) != reguess_args[1]) goto lab_reguess;                   //Reguess for different annulus pressure
		
		if(fabs(reguess_args[2]-T_1_in) > 50.) goto lab_reguess;
		
		for(int i=0; i<5; i++){ if(T_save[i] < T_sky - 1.) goto lab_reguess; } 
		
		T_save_tot = 0.;
		for(int i=0; i<5; i++){ T_save_tot += T_save[i];}
		if(T_save_tot != T_save_tot) goto lab_reguess;	//NaN check.. a value is only not equal to itself if it is NaN
		
		reguess = false;
		goto lab_keep_guess;		
lab_reguess:
		reguess = true;
lab_keep_guess:

			
		//------------------------

		if(reguess) {
			if(GlazingIntact(hn,hv)) {
				T_save[0] = T_1_in;
				T_save[1] = T_1_in+2.;
				T_save[2] = T_save[1] + 5.;
				if(P_a(hn,hv) > 1.0){          //Set guess values for different annulus pressures
					T_save[3] = T_save[2] - 0.5*(T_save[2]-T_amb);       //If higher pressure, guess higher T4   
					T_upper_max = T_save[2] - 0.2*(T_save[2]-T_amb);     //Also, high upper limit for T4
				}
				else{
					T_save[3] = T_save[2] - 0.9*(T_save[2]-T_amb);       //If lower pressure, guess lower T4
					T_upper_max = T_save[2] - 0.5*(T_save[2]-T_amb);     //Also, low upper limit for T4
				}                      
				T_save[4] = T_save[3] - 2.;

				reguess_args[1]  = P_a(hn,hv);               //Reset previous pressure
				reguess_args[0] = GlazingIntact(hn,hv) ? 1. : 0.;   //Reset previous glazing logic
				reguess_args[2] = T_1_in;            //Reset previous T_1_in

			}
			else{
				T_save[0] = T_1_in;
				T_save[1] = T_1_in+2.;
				T_save[2] = T_save[1] + 5.;
				T_save[3] = T_amb;
				T_save[4] = T_amb;

				reguess_args[0] = GlazingIntact(hn,hv) ? 1. : 0.;   //Reset previous glazing logic
				reguess_args[1] = T_1_in;            //Reset previous T_1_in

			}
		}

		//Set intial guess values
		T_2 = T_save[1];
		T_3 = T_save[2];
		T_4 = T_save[3];
		T_5 = T_save[4];
		//Set constant temps
		T_6 = T_amb;
		T_7 = T_sky ;         

		qq = 0;                  //Set iteration counter for T3 loop

		T_2g = T_2;              //Initial guess value for T_2 (only used in property lookup)        
		cp_1 = 1950.;            //Initial guess value for cp of WF

		//Tolerances for iteration
		T3_tol = 1.5e-3;           
		q5_tol = 1.0e-3;         //Since iterations are nested inside T3, make tolerances a bit tighter        
		T1_tol = 1.0e-3;        
		T2_tol = 1.0e-3;        

		//Decreasing the tolerance helps get out of repeating defocus iterations
		if(ncall>8) {
			T3_tol = 1.5e-4;        //1.0   
			q5_tol = 1.0e-4;        //max(1.0, 0.001*q_i)
			T1_tol = 1.0e-4;        //1.0
			T2_tol = 1.0e-4;        //1.0
		}

		Diff_T3 = 10.0 + T3_tol;    //Set difference > tolerance

		//Constants
		k_45 = 1.04;                             //[W/m-K]  Conductivity of glass
		R_45cond = log(D_5(hn,hv)/D_4(hn,hv))/(2.*pi*k_45);    //[K-m/W]Equation of thermal resistance for conduction through a cylinder

		colopteff_tot = ColOptEff(ct, sca_num)*Dirt_HCE(hn,hv)*Shadowing(hn,hv);	//The total optical efficiency

		if(GlazingIntact(hn,hv)){   //These calculations (q_3SolAbs,q_5solAbs) are not dependent on temperature, so only need to be computed once per call to subroutine
    
			q_3SolAbs = q_i * colopteff_tot * Tau_envelope.at(hn, hv) * alpha_abs.at(hn, hv);  //[W/m]  
			//We must account for the radiation absorbed as it passes through the envelope
			q_5solabs = q_i * colopteff_tot * alpha_env(hn, hv);   //[W/m]  
		}
		else{
			//Calculate the absorbed energy 
			q_3SolAbs = q_i * colopteff_tot * alpha_abs(hn, hv);  //[W/m]  
			//No envelope
			q_5solabs = 0.0;                            //[W/m]

		}
		
		is_e_table = false;
		if(epsilon_3.getTableSize(hn,hv) < 2){
			eps_3 = epsilon_3.getSingleValue(hn, hv);
		}
		else{
			eps_3 = epsilon_3.interpolate(hn, hv, T_3-273.15);          //Set epsilon value for case that eps_mode = 1.  Will reset inside temp loop if eps_mode > 1.
			is_e_table = true;	//The emissivity is in tabular form
		}

		T3upflag = false;
		T3lowflag = false;

		double T3_adjust = 0.0;
		double T3_prev_qq = 0.0;

		while( ( (fabs(Diff_T3)>T3_tol) && (qq<100) ) || (qq<2)){    //Outer loop: Find T_3 such than energy balance is satisfied
			qq=qq+1; //loop counter

			T3_prev_qq = T_3;

			if(qq>1){
				if((T3upflag)&&(T3lowflag)){
					if(Diff_T3 > 0.){
						T3_upper = T_3;
						y_T3_upper = Diff_T3;
					} 
					else {    
						T3_lower = T_3;
						y_T3_lower = Diff_T3;
					}
					T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper;

				} 
				else {
					if(Diff_T3 > 0.){
						T3_upper = T_3;
						y_T3_upper = Diff_T3;
						T3upflag = true;
					} 
					else {    
						T3_lower = T_3;
						y_T3_lower = Diff_T3;
						T3lowflag = true;
					}
            
					if((T3upflag)&&(T3lowflag)){  
						T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper;
					} 
					else 
					{
						if( Diff_T3 > 0.0 )
							T_3 = T_3 - 50.0;
						else
							T_3 = T_3 + 50.0;
						//T_3 = T_3 - abs_diffT3;         //Note that recalculating T_3 using this exact equation, rather than T_3 = T_3 - frac*diff_T3 was found to solve in fewer iterations
					}
				}
			}
            
			T3_adjust = T_3 - T3_prev_qq;

			//Calculate temperature sensitive emissivity using T_3, if required
			if(is_e_table) eps_3 = epsilon_3.interpolate(hn, hv, (T_3-273.15)); //call interp((T_3-273.15),eps_mode,xx,yy,eps3old,eps_3)

			//Separate GlazingIntact = true and GlazingIntact = false  If true, T4 must be solved, if false then T4 is explicitly known (or doesn't exist, depending on how you want to look at it)
			//Solving for correct T4 as it relates to current T3 value
			if (GlazingIntact(hn,hv)) {
    
				//**********************************************
				//************* SET UP T_4 ITERATION **********************
				//**********************************************
        
				//if(qq==1){               //If first iteration, set T_4 bounds to phyiscal limits defined by T_3 and T_sky
				//	T_lower = T_sky;         //Lowest possible temperature of T_4 is sky temp        
				//	T_upper = max(T_upper_max,T_amb);    //Highest possible temperature is the highest temperature on either side of T_4: either T_3 or ambient
				//	q5_tol_1= 0.001;           //Just get T4 in the ball park.  '20' may not be the optimum value.....
				//} 
				//else {                                            //For additional iterations:
				//	T_lower = T_lower - max(abs_diffT3,0.0);       //If diff_T3 is + then new T3 < old T3 so adjust lower limit
				//	T_upper = T_upper + fabs(min(abs_diffT3,0.0));  //If diff_T3 is (-) then new T3 > old T3 so adjust upper limit
				//	q5_tol_1= q5_tol;        //For remaining T3 iterations, use specified tolerance (note that 2 iterations for T3 are gauranteed)                   
				//}
        
				if( qq == 1 )
				{
					T_lower = T_sky;
					T_upper = max(T_3, T_amb);
				}
				else
				{
					if( T3_adjust > 0.0 )	// new T3 > old T3 so adjust upper limit
					{
						T_upper = min(T_3, T_upper + 1.25*T3_adjust);
						T_lower = T_4;
						T_4 = T_4 + 0.5*T3_adjust;
					}
					else
					{
						T_lower = max(T_sky, T_lower + 1.25*T3_adjust);
						T_upper = T_4;
						T_4 = T_4 + 0.5*T3_adjust;
					}
				}

				q5_tol_1 = q5_tol;

				diff_q5 = q5_tol_1 + 1.0;       //Set diff > tolerance
				q5_iter = 0;                     //Set iteration counter
        
				UPFLAG      = false;           //Set logic to switch from bisection to false position mode
				LOWFLAG     = false;           //Set logic to switch from bisection to false position mode   
				//***********************************************************************************
				//************* Begin Bisection/False Position Iteration method *********************
				//***********************************************************************************
				while( (fabs(diff_q5)>q5_tol_1) && (q5_iter<100) ){       //Determine T_4 such that energy balance from T_3 to surroundings is satisfied
            
					q5_iter = q5_iter + 1;                       //Increase iteration counter

					//The convective heat exchange between the absorber and the envelope
					//      UNITS   ( K , K, torr, Pa , m/s,  K , -, -, W/m, W/m2-K)
					FQ_34CONV(T_3, T_4, P_6, v_6, T_6, hn, hv, q_34conv, h_34conv);
            
					//The radiative heat exchange between the absorber and the envelope
					//    Units         ( K ,  K ,  m ,  m , K  ,    -     ,    -     ,   logical    ,  W/m   , W/m2-K)
					FQ_34RAD(T_3, T_4, T_7, eps_3, hn, hv, q_34rad, h_34rad);
					//The total heat exchange between absorber and envelope
					q_34tot = q_34conv + q_34rad;            //[W/m]
    
					//**********************************************
					//************* Calculate T_5 *************
					//**********************************************
					//The thermal energy flow across 45 is equal to the energy from the absorber plus
					//the thermal energy that is generated by direct heating of the glass envelope
					q_45cond = q_34tot + q_5solabs;          //[W/m]
            
					//Knowing heat flow and properties, T_5 can be calculated
					T_5 = T_4 - q_45cond*R_45cond;           //[K]
   
					//*************************************************************************
					//************* Calculate HT from exterior surface to ambient *************
					//*************************************************************************
					//With T_5 and T_6 (amb T) calculate convective and radiative loss from the glass envelope
					//           units   ( K ,  K ,  torr, m/s, -, -, W/m, W/m2-K)
					FQ_56CONV(T_5, T_6, P_6, v_6, hn, hv, q_56conv, h_56conv); //[W/m]
					q_57rad = EPSILON_5(hn,hv) * 5.67e-8 * (pow(T_5,4) - pow(T_7,4));
					q_5out = q_57rad + q_56conv;     //[W/m]

					//***************************************************************************
					//********** Compare q_5out with q_45 cond***********************************
					//***************************************************************************
					diff_q5 = (q_5out - q_45cond)/fabs(q_45cond);     //[W/m]
            
					//Determine next guess for T_4.  Want to use false position method, but it requires that the *results* at both ends of the bracket are known.  We have
					//defined a bracket but not the results.  Use the guess T_4 to get the results at one end of a new bracket.  Then calculate a new T_4 that is highly weighted 
					//towards the side of the original bracket that the 1st T_4 did not replace.  In most cases, this new T_4 will result in the opposite diff_q5, which 
					//defines both sides of the bracket.  If results for both sides are then defined, "LOWFLAG" and "UPFLAG" will be true, and false position method will be applied.

					if( LOWFLAG && UPFLAG ){          //False position method     
						if(diff_q5>0.0){
							T_upper = T_4;       //If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
							y_upper = diff_q5;   //so set new upper limit to T_4
						} 
						else {                    //If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low
							T_lower = T_4;       //so set new lower limit to T_4
							y_lower = diff_q5;   //also, set result to go along with lower limit
						}    
						T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper;      
            
					} 
					else {                        //For bisection method...
                 
						if(diff_q5>0.0){    //If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
							T_upper = T_4;       //so set new upper limit to T_4
							y_upper = diff_q5;   //also, set result to go along with upper limit
							UPFLAG  = true;    //Upper result is now known
							if(qq==1){
								upmult  = 0.1;       //Just want to get in ballpark for first iteration of receiver
							} 
							else {
								upmult  = 0.1;       //Weight such that next calculated T_4 (if using bisection method) results in negative diff_q5
							}
                    
						} 
						else {                    //If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low          
							T_lower = T_4;       //so set new lower limit to T_4
							y_lower = diff_q5;   //also, set result to go along with lower limit
							LOWFLAG = true;    //Lower result is now known
							if(qq==1){
								upmult  = 0.1;       //Just want to get in ballpark for first iteration of receiver
							} 
							else {
								upmult  = 0.9;       //Weight such that next calculated T_4 (if using bisection method) results in positive diff_q5
							}
                    
						}
                
						if(LOWFLAG && UPFLAG){  //If results of bracket are defined, use false position
							T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper;
						} 
						else {                            //If not, keep bisection
							T_4 = (1.-upmult)*T_lower + upmult*T_upper;
						}

					}  
  
				//*********************************************************************************************
				//********** END Bisection/False Position Iteration Loop on T_4 *******************************
				//*********************************************************************************************       
				}
            
			} 
			else {      //Glazing is not intact

				//Know convection and radiation forcing temps
				//----Having guessed the system temperatures, calculate the thermal losses starting from
				//----the absorber surface (3)
				//The convective heat exchange between the absorber and the envelope
				FQ_34CONV(T_3, T_4, P_6, v_6, T_6, hn, hv, q_34conv, h_34conv);
				//The radiative heat exchange between the absorber and the envelope
				FQ_34RAD(T_3, T_4, T_7, eps_3, hn, hv, q_34rad, h_34rad);
				//The total heat exchange between absorber and envelope
				q_34tot = q_34conv + q_34rad;    //[W/m]
        
			}      //Know heat transfer from outer surface of receiver tube to ambient
    
			//Bracket Losses
			//Bracket conduction losses apply 
			q_cond_bracket = FQ_COND_BRACKET(T_3, T_6, P_6, v_6, hn, hv); //[W/m] 

			q_12conv = q_3SolAbs - (q_34tot+q_cond_bracket);         //[W/m] Energy transfer to/from fluid based on energy balance at T_3
    
			q_in_W  = q_12conv * L_actSCA[ct];                           //Convert [W/m] to [W] for some calculations
    
			if(!single_point) {
				T_1_out = max( T_sky, q_in_W/(m_dot*cp_1) + T_1_in );    //Estimate outlet temperature with previous cp
        
				diff_T1 = T1_tol + 1.0;                                 //Set diff > tolerance
				T1_iter = 0;                                             //Set iteration counter    
        
				while( (fabs(diff_T1)>T1_tol) && (T1_iter<100)){       //Find correct cp& rho and solve for T_1_ave
        
					T1_iter ++;                   //Increase iteration counter
					T_1_ave = (T_1_out + T_1_in) / 2.0;     //Average fluid temperature
					cp_1    = htfProps.Cp(T_1_ave)*1000.;
					T_1_out1= max( T_sky, q_in_W/(m_dot*cp_1) + T_1_in );  //Estimate outlet temperature with previous cp 
					diff_T1 = (T_1_out - T_1_out1)/T_1_out;  //Difference between T_1_out used to calc T_ave, and T_1_out calculated with new cp
					T_1_out = T_1_out1;                      //Calculate new T_1_out
        
				}
			}
			else {
				//If we're only calculating performance for a single point, set the receiver ave/outlet temperature to the inlet.
				T_1_out = T_1_in;
				T_1_ave = T_1_in;
			}
    
			rho_1ave    = htfProps.dens(T_1_ave,P_field_in/2);       //[kg/m^3] Density
			v_1         = m_dot/(rho_1ave*A_cs(hn,hv));             //HTF bulk velocity
    
			q_conv_iter = 0;                 //Set iteration counter
			diff_T2 = 1.0 + T2_tol;         //Set diff > tolerance

			bool T2upflag = false;
			bool T2lowflag = false;

			double y_T2_low = std::numeric_limits<double>::quiet_NaN();
			double y_T2_up = std::numeric_limits<double>::quiet_NaN();

			double T2_low = min(T_1_ave, T_3);
			double T2_up = max(T_1_ave, T_3);

			//Ensure convective calculations are correct (converge on T_2)
			while( (fabs(diff_T2)>T2_tol) && (q_conv_iter<100)){
 
				q_conv_iter ++;       //Increase iteration counter

				T_2 = max(10.0, fT_2(q_12conv, T_1_ave, T_2g, v_1, hn, hv) );	//Calculate T_2 (with previous T_2 as input)
				diff_T2 = (T_2 - T_2g)/T_2;          //T_2 difference

				if(diff_T2 > 0.0)			// Calculated > Guessed, set lower limit and increase guessed
				{
					T2_low = T_2g;
					T2lowflag = true;
					y_T2_low = diff_T2;
					if( T2upflag )
						T_2g = y_T2_up / (y_T2_up - y_T2_low)*(T2_low - T2_up) + T2_up;
					else
						T_2g = T2_up;
				}
				else						// Calculated < Guessed, set upper limit and decrease guessed
				{
					T2_up = T_2g;
					T2upflag = true;
					y_T2_up = diff_T2;
					if( T2lowflag )
						T_2g = y_T2_up / (y_T2_up - y_T2_low)*(T2_low - T2_up) + T2_up;
					else
						T_2g = T2_low;
				}
 
				if( (T2_up - T2_low) / T2_low < T2_tol / 10.0 )
					break;

			}
    
			//The conductive heat transfer equals the convective heat transfer (energy balance)
			q_23cond = q_12conv;         //[W/m]
    
			//Calculate tube conductivity 
			k_23 = FK_23(T_2, T_3, hn, hv);       //[W/m-K]  

			//Update the absorber surface temperature (T_3) according to new heat transfer rate
			abs_diffT3 = T_3 - (T_2 + q_23cond*log(D_3(hn,hv)/D_2(hn,hv))/(2.*pi*k_23));
			Diff_T3 = abs_diffT3/T_3;
   
    
		}

		//Warning of convergence failure
		//if(qq>99) {                           //End simulation if loop does not converge
		//    call messages(-1,"Trough Energy Balance Convergence Error 1",'WARNING',INFO(1),INFO(2))
		//    return
		//}
		//
		//if(T1_iter>99) {
		//    call messages(-1,"Trough Energy Balance Convergence Error 2",'WARNING',INFO(1),INFO(2))
		//    return
		//}
		//
		//if(q_conv_iter>99) {
		//    call messages(-1,"Trough Energy Balance Convergence Error 3",'WARNING',INFO(1),INFO(2))
		//    return
		//}
		//
		//if(q5_iter>99) {
		//    call messages(-1,"Trough Energy Balance Convergence Error 4",'WARNING',INFO(1),INFO(2))
		//    return
		//}
  
		//Calculate specific heat in kJ/kg
		c_1ave = cp_1/1000.;
 
        // 10.6.2016 twn: q_5solabs is already reported as an optical loss, so don't report as a thermal loss...
		//q_heatloss = q_34tot + q_cond_bracket + q_5solabs;   //[W/m]
        q_heatloss = q_34tot + q_cond_bracket;   //[W/m]

		//Save temperatures
		T_save[1] = T_2;
		T_save[2] = T_3; 
		T_save[3] = T_4;
		T_save[4] = T_5;

	};


	/*
	#################################################################################################################
	#################################################################################################################
	#################################################################################################################


	"******************************************************************************************************************************
 		FUNCTION Fq_12conv :  Convective heat transfer rate from the HTF to the inside of the receiver tube
	******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
						 note:  This function was programmed and tested against the EES original.
								Small variations in output are due to slightly different fluid 
								properties used in the two models.

		Newton's Law of Cooling.

			q' = h * D_i *  PI * (T_m - T_s)

			h = Nu_Di * k / D_i

		Where

			q' = convection heat transfer rate per unit length [W/m]
			h = convection heat transfer coefficient [W/m^2-k]
			D_i = inside diameter of absorber pipe [m]
			T_m = mean (bulk) temperature of HTF [C]
			T_s = inside surface temperature of absorber pipe [C]
			Nu_Di = Nusselt number based on inside diameter
			k = conduction heat transfer coefficient of HTF [W/m-K]

		The Nusselt number is estimated with the correlation developed by Gnielinski. 

 			Nu# = (f / 8) * (Re_Di - 1000) * Pr / (1 + 12.7 * (f / 8)^(1/2) * (Pr^(2/3) -1))  * (Pr / Pr_w)^0.11
			f = (1.82 * log10(Re_Di) - 1.64)^(-2)
			Re_Di = Rho * v_m * Di / u
			Pr  = Cp * u / k

		Where

			Nu# = Nusselt number
			Re_Di = Reynolds number for internal pipe flow
			Pr = Prandtl number
			Pr_w = Prandtl number evaluated at the wall temperature
			u = fluid absolute viscosity [kg/m-s]
			Di = inside diameter [m]
			Cp = fluid specific heat [J/kg-K]
			k = fluid thermal conductivity [W/m-K]
			Rho = fluid density [kg/m^3]
			v_m = mean fluid velocity [m/s]

	The above correlation is valid for 0.5 < Pr < 2000 and 2300< Re_Di < 5 * 10^6 and can be used for both uniform heat flux and uniform wall temperature cases. With the exception of Pr_w, all properties are evaluated at the mean fluid temperature.

	If Re_D <= 2300 and the choice was made from the diagram window  to use the laminar flow model, one of  the following correlations is used.

			for inner tube flow (uniform flux condition)
			Nu# = 4.36

			for inner annulus flow (uniform flux condition -- estimated from table for Nu# with heat fluxes at both surfaces)
			D_p/D_2	Nu#
			0		4.364
			0.05		4.792
			0.10		4.834
			0.20		4.833
			0.40		4.979
			0.60		5.099
			0.80		5.24
			1.00		5.385		


	For the "SNL test platform" case the inside diameter in the above correlations is replaced with the following hydraulic diameter definition.

			D_h = 4 * A_c / P = D_ao - D_ai

		Where

			D_h = hydraulic diameter [m]
			A_c = flow cross sectional area [m^2]
			P = wetted perimeter [m]
			D_ai = inner annulus diameter [m]
			D_ao = outer annulus diameter [m]

	(Sources: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 489-491, 502-503. Gnielinski, V., "New Equations for Heat and Mass Transfer in Turbulent Pipe and Channel Flow," International Chemical Engineering, Vol. 16, No. 2, April 1976.)
	*/

	double fT_2(double q_12conv, double T_1, double T_2g, double v_1, int hn, int hv){
		//		convection 1->2,  HTF temp, guess T2,  fluid velocity, HCE #, HCE variant
		//     Input units (  K   ,  K ,  real,  m/s, - , -)
	
		double Cp_1, Cp_2, f, h_1, k_1, k_2, mu_1, mu_2, Nu_D2, Pr_1, Pr_2, Re_D2, rho_1, DRatio;
		bool includelaminar = true;	//cc -- this is always set to TRUE in TRNSYS
	
		T_2g = max(T_2g, m_htf_prop_min);

		// Thermophysical properties for HTF 
		mu_1 = htfProps.visc(T_1);  //[kg/m-s]
		mu_2 = htfProps.visc(T_2g);  //[kg/m-s]
		Cp_1 = htfProps.Cp(T_1)*1000.;  //[J/kg-K]
		Cp_2 = htfProps.Cp(T_2g)*1000.;  //[J/kg-K]
		k_1 = max(htfProps.cond(T_1),P_field_in/2);  //[W/m-K]
		k_2 = max(htfProps.cond(T_2g),P_field_in/2);  //[W/m-K]
		rho_1 = htfProps.dens(T_1, P_field_in/2);  //[kg/m^3]

		Pr_2 = (Cp_2 * mu_2) / k_2;
		Pr_1 = (Cp_1 * mu_1) / k_1;

		if(v_1 > 0.1) {

			Re_D2 = (rho_1 * D_h(hn,hv) * v_1) / (mu_1);

			// Nusselt Number for laminar flow case if option to include laminar flow model is chosen 
			if (( includelaminar == true) && (Re_D2 <= 2300.)) {
				if(Flow_type(hn,hv) == 2.0) {
					DRatio = D_p(hn,hv)/D_2(hn,hv);
					//Estimate for uniform heat flux case (poly. regression based on lookup table in Forristall EES model)
					//---Note that this regression is based on an 8-point table, and is highly non-practical outside of DRatio bounds
					//---0 and 1
					if(DRatio > 1.) {
						Nu_D2 = 5.385;
					}
					else if(DRatio < 0.) {
						Nu_D2 = 4.364;
					}
					else{
						Nu_D2 = 41.402*pow(DRatio,5) - 109.702*pow(DRatio,4) + 104.570*pow(DRatio,3) - 42.979*pow(DRatio,2) + 7.686*DRatio + 4.411;
					}
				}
				else{
					Nu_D2 = 4.36;				//uniform heat flux
				}
			}
			else{
				// Warning statements if turbulent/transitional flow Nusselt Number correlation is used out of recommended range 
		//		if (Pr_1 <= 0.5) or (2000 <= Pr_1) { CALL WARNING('The result may not be accurate, since 0.5 < Pr_1 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_1 = XXXA1', Pr_1)
		//		if (Pr_2 <= 0.5) or (2000 <= Pr_2) { CALL WARNING('The result may not be accurate, since 0.5 < Pr_2 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_2 = XXXA1', Pr_2)
		//		If ( Re_D2 <= (2300) ) or (5*10**6 <= Re_D2 ) Then CALL WARNING('The result may not be accurate, since 2300 < Re_D2 < (5 * 10**6) does not hold. See PROCEDURE Pq_12conv. Re_D2 = XXXA1', Re_D2)

				// Turbulent/transitional flow Nusselt Number correlation (modified Gnielinski correlation) 	
				f = pow(1.82 * log10(Re_D2) - 1.64,-2);
				Nu_D2 = (f / 8.) * (Re_D2 - 1000.) * Pr_1 / (1. + 12.7 * sqrt(f / 8.) * (pow(Pr_1,0.6667) -1.)) * pow(Pr_1 / Pr_2, 0.11);
			}

			h_1 = Nu_D2 * k_1 / D_h(hn,hv);  //[W/m**2-K]
			return T_1 + q_12conv/(h_1*D_2(hn,hv)*pi);
			//q_12conv = h_1 * D_2 * PI  * (T_2 - T_1ave)  //[W/m]
		}
		else{
			h_1 = 0.0001;
			return T_1;
		}

	};



	/******************************************************************************************************************************
 		FUNCTION fq_34conv :	Convective heat transfer rate between the absorber outer surface and the glazing inner surface
	******************************************************************************************************************************"
	  NOTE: Temperatures input in terms of degrees K

	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009

	{ Four cases:

		1. Vacuum in annulus: free-molecular heat transfer model for an annulus.
		2. Low or lost vacuum: natural convection heat transfer model for an annulus.
		3. No glazing, no wind: natural convection heat transfer model for a horizontal cylinder.
		4. No glazing, with wind: forced convection heat transfer model for a horizontal cylinder.


	Case 1:
	
		Free-molecular heat transfer for an annular space between horizontal cylinders.

			q' = D_i * PI * h * (T_i - T_o)		
			h = k_gas / (D_i / 2 * ln(D_o / D_i) + b * Lambda * (D_i / D_o + 1))
			b = (2 - a) / a * (9 * Gamma - 5) / (2 * (Gamma + 1))
			Lambda = 2.331 * 10^(-20) * T_avg / (P * Delta^2)

		Where

			q' = convection heat transfer rate per unit length [W/m]
			D_i = outer absorber diameter [m]
			D_o = inner glazing diameter [m]
			h = convection heat transfer coefficient for annulus gas [W/m^2-K]
			T_i = outer absorber surface temperature [C]
			T_o = inner glazing surface temperature [C]
			k_gas = thermal conductivity of the annulus fluid at standard temperature and pressure [W/m^2-K]
			b = interaction coefficient [dimensionless]
			Lambda = mean-free-path between collisions of a molecule [cm]
			a = accommodation coefficient [dimensionless]
			Gamma = ratio of specific heats for the annulus fluid [dimensionless]
			T_avg = average temperature of the annulus fluid [K]
			P = pressure of the annulus gas [mm of Hg]
			Delta = molecular diameter of the annulus gas [cm]

		The above correlation is valid for Ra_Do < (D_o / (D_o -D_i))^4, but may over estimate q' slightly for large vacuums.

	(Source: Ratzel, A., Hickox, C., Gartling, D., "Techniques for Reducing Thermal Conduction and Natural Convection Heat Losses 
	 in Annular Receiver Geometries," Journal of Heat Transfer, Vol. 101, No. 1, February 1979; pp. 108-113)


	Case 2:
	
		Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders.

			q' = 2.425 * k * (T_i - T_o) / (1 + (D_i / D_o)^(3/5))^(5/4) * (Pr * Ra_Di / (0.861 + Pr))^(1/4)
			Pr = NU / Alpha
			Ra_Di = g * Beta * (T_i - T_o) * (D_i)^3 / (Alpha * NU)
			Beta = 1 / T_avg		"Ideal Gas"

		Where

			k = conduction heat transfer coefficient for the annulus gas [W/m-K]
			Pr = Prandtl number 
			NU = kinematic viscosity [m^2/s]
			Alpha = thermal diffusivity [m^2/s]
			Ra_Di = Rayleigh number based on the annulus inner diameter
			g = local acceleration due to gravity [m/s^2]
			Beta = volumetric thermal expansion coefficient [1/K]
			Rho_o = annulus gas density at the outer surface [kg/m^3]
			Rho_i = annulus gas density at the inner surface [kg/m^3]
			T_avg = average temperature, (T_i + T_o) / 2 [K]

		Above correlation is valid for Ra_Do > (D_o / (D_o -D_i))^4. All physical properties are evaluated at the average temperature, (T_i + T_o)/2.

	(Source: Bejan, A., Convection Heat Transfer, Second Edition; John Wiley & Son's, New York, 1995, pp. 257-259.)


	Case 3:

		Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder.

			Nu_bar = (0.60 + (0.387 * Ra_D^(1/6)) / (1 + (0.559 / Pr)^(9/16))^(8/27) )^2
			Ra_D = g * Beta * (T_s - T_inf) * D^3 / (Alpha * NU)
			Beta =  1 / T_f	"Ideal Gas"
			Alpha = k / (Cp * Rho)
			Pr = NU / Alpha

			h = Nu_bar * k / D

			q' = h * PI * D * (T_s - T_inf)

		Where

			Nu_bar = average Nusselt number
			Ra_D = Rayleigh number based on diameter
			Rho = fluid density  [kg/m^3]
			Cp = specific heat at constant pressure [kJ / kg-K]
			T_inf = fluid temperature in the free stream [C]
			T_s = surface temperature [C]
			T_f = film temperature, (T_s + T_inf) / 2 [K]
			T_inf = ambient air temperature [C]

		Above correlation is valid for  10^(-5) < Ra_D < 10^12. All physical properties are evaluated at the film temperature, (T_s + T_inf) / 2.

	(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 550-552.)


	Case 4:

		Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder.

 			Nu_bar = C * Re_D^m * Pr^n * (Pr / Pr_s)^(1/4)

			Re_D		C			m
			1-40		0.75			0.4
			40-1000		0.51			0.5
			1e3- 2e5	0.26			0.6
			2e5-1e6	0.076			0.7

			n = 0.37, Pr <=10
			n = 0.36, Pr >10

 			Re_D =  U_inf * D / NU
			Pr  = NU / Alpha
			Alpha = k / (Cp * Rho)

			Q =  h * D * PI * (T_s - T_inf) * L

		Where,

			Re_D = Reynolds number evaluated at the diameter
			Cp = specific heat at constant pressure of air [W/m-K]
			Rho = density of air [kg/m^3]
			C, m, n = constants

		Above correlation is valid for  0.7 < Pr < 500, and 1 < Re_D < 10^6. All physical properties evaluated 
	   at the free stream temperature, T_inf,  except Pr_s.

	(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and 
	 Sons, New York, 1981, p. 413.)
	}*/
	//subroutine FQ_34CONV(T_3,T_4, D_3, D_4, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34)
	void FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hn, int hv, double &q_34conv, double &h_34){
	   //      UNITS   ( K , K ,  Pa , m/s,  K , -, -, W/m, W/m2-K)
	
	double a, Alpha_34, b, Beta_34, C, C1, Cp_34, Cv_34, Delta, Gamma, k_34, Lambda, 
			  m, mu_34, n, nu_34, P, Pr_34, P_A1, Ra_D3, Ra_D4, rho_34, T_34, T_36, 
			  grav, Nu_bar, rho_3, rho_6, mu_36, rho_36, cp_36,
			  k_36, nu_36, alpha_36, beta_36, Pr_36, h_36, mu_3, mu_6, k_3, k_6, cp_3, Cp_6, nu_6, nu_3,
			  Alpha_3, alpha_6, Re_D3, Pr_3, Pr_6, Natq_34conv, Kineticq_34conv;
	
	grav = 9.81; //m/s2  gravitation constant

	P_A1 = P_a(hn,hv) * 133.322368;  //convert("torr", "Pa")  //[Pa]

		T_34 = (T_3 + T_4) / 2.;  //[C]
		T_36 = (T_3 + T_6) / 2.;  //[C]

		if(!GlazingIntact(hn,hv) ) {
		
			// Thermophysical Properties for air 
			rho_3 = airProps.dens(T_3, P_6);  //[kg/m**3], air is fluid 1.
			rho_6 = airProps.dens(T_6, P_6);  //[kg/m**3], air is fluid 1.

			if (v_6 <= 0.1) {
				mu_36 = airProps.visc(T_36);  //[N-s/m**2], AIR
				rho_36 = airProps.dens(T_36, P_6);  //[kg/m**3], AIR
				cp_36 = airProps.Cp(T_36)*1000.;  //[J/kg-K], AIR
				k_36 = airProps.cond(T_36);  //[W/m-K], AIR
				nu_36 = mu_36 / rho_36;  //[m**2/s] kinematic viscosity, AIR
				alpha_36 = k_36 / (cp_36 * rho_36);  //[m**2/s], thermal diffusivity, AIR
				beta_36 =  1.0 / T_36;  //[1/K]
				Ra_D3 = grav * beta_36 * fabs(T_3 - T_6) * pow(D_3(hn,hv),3) / (alpha_36 * nu_36);

				// Warning Statement if following Nusselt Number correlation is used out of recommended range //
				//If ((Ra_D3 <= 1.e-5) || (Ra_D3 >= 1.e12)) continue
					//CALL WARNING('The result may not be accurate, since 10**(-5) < Ra_D3 < 10**12 does not hold. See Function fq_34conv. Ra_D3 = XXXA1', Ra_D3)

				// Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder //
				Pr_36 = nu_36 / alpha_36;
				Nu_bar = pow(0.60 + (0.387 * pow(Ra_D3,0.1667)) / pow(1. + pow(0.559 / Pr_36,0.5625), 0.2963) , 2);
				h_36 = Nu_bar * k_36 / D_3(hn,hv);  //[W/m**2-K]//
				q_34conv = h_36 * pi * D_3(hn,hv) * (T_3 - T_6);  //[W/m]//
				h_34 = h_36;  //Set output coefficient
			}
			else {

				// Thermophysical Properties for air 
				mu_3 = airProps.visc(T_3);  //[N-s/m**2]
				mu_6 = airProps.visc(T_6);  //[N-s/m**2]
				k_3 = airProps.cond(T_3);  //[W/m-K]
				k_6 = airProps.cond(T_6);  //[W/m-K]
				cp_3 = airProps.Cp(T_3)*1000.;  //[J/kg-K]
				Cp_6 = airProps.Cp(T_6)*1000.;  //[J/kg-K]
				nu_6 = mu_6 / rho_6;  //[m**2/s]
				nu_3 = mu_3 / rho_3;  //[m**2/s]
				Alpha_3 = k_3 / (cp_3 * rho_3);  //[m**2/s]
				alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s]
				Re_D3 = v_6 * D_3(hn,hv) / nu_6;
				Pr_3 = nu_3 / Alpha_3;
				Pr_6 = nu_6 / alpha_6;

				// Warning Statements if following Nusselt Number correlation is used out of range //
				//if (Re_D3 <= 1) or (Re_D3 >= 10**6) { CALL WARNING('The result may not be accurate, since 1 < Re_D3 < 10**6 does not hold. See Function fq_34conv. Re_D3 = XXXA1', Re_D3)
				//If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_34conv. Pr_6 = XXXA1', Pr_6)

				// Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) //
				if (Pr_6 <= 10) {
					n = 0.37;
				}
				else{
					n = 0.36;
				}

				if (Re_D3 < 40) {
					C = 0.75;
					m = 0.4;
				}
				else{

					if ((40 <= Re_D3) && (Re_D3 < 1000.)){
						C = 0.51;
						m = 0.5;
					}
					else{
						if ((1.e3 <= Re_D3) && (Re_D3 < 2.e5)) {
							C = 0.26;
							m = 0.6;
						}
						else{
							if ((2.e5 <= Re_D3) && (Re_D3 < 1.e6)) {
								C = 0.076;
								m = 0.7;
							}
						}
					}
				}

				// Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
				Nu_bar = C * pow(Re_D3, m)  * pow(Pr_6, n) * pow(Pr_6 / Pr_3, 0.25);
				h_36 = Nu_bar  *  k_6  /  D_3(hn,hv);  //[W/m**2-K]
				q_34conv =  h_36  *  D_3(hn,hv)  *  pi  *  (T_3 - T_6);  //[W/m]	
				h_34 = h_36;  //set output coefficient
			}
		}
		else {

			// Thermophysical Properties for gas in annulus space 
			mu_34 = AnnulusGasMat.at(hn,hv)->visc(T_34);  //[kg/m-s] 
			Cp_34 = AnnulusGasMat.at(hn,hv)->Cp(T_34)*1000.;  //[J/kg-K]
			Cv_34 = AnnulusGasMat.at(hn,hv)->Cv(T_34)*1000.;  //[J/kg-K]
			rho_34 = AnnulusGasMat.at(hn,hv)->dens(T_34, P_A1);  //[kg/m**3]
			k_34 = AnnulusGasMat.at(hn,hv)->cond(T_34);  //[W/m-K]

			// Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders 
			Alpha_34 = k_34 /(Cp_34 * rho_34);  //[m**2/s]//
			nu_34 = mu_34 / rho_34;  //[m**2/s]//
			Beta_34 = 1. / max(T_34,1.0);  //[1/K]//
			Ra_D3 = grav * Beta_34 * fabs(T_3 - T_4) * pow(D_3(hn,hv),3) / (Alpha_34 * nu_34);
			Ra_D4 = grav * Beta_34 * fabs(T_3 - T_4) * pow(D_4(hn,hv),3) / (Alpha_34 * nu_34);
			Pr_34 = nu_34 / Alpha_34;
			Natq_34conv = 2.425 * k_34 * (T_3 - T_4) / pow(1 + pow(D_3(hn,hv)/ D_4(hn,hv), 0.6), 1.25) * pow(Pr_34 * Ra_D3 / (0.861 + Pr_34),0.25);  //[W/m]//	
			P = P_a(hn,hv);  //[mmHg] (note that 1 torr = 1 mmHg by definition)
			C1 = 2.331e-20;  //[mmHg-cm**3/K]//

			// Free-molecular heat transfer for an annular space between horizontal cylinders 
			if (AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Air) { //AIR
				Delta = 3.53e-8;  //[cm]
			}

			if (AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Hydrogen_ideal){ //H2
				Delta = 2.4e-8;  //[cm]
			}

			if (AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Argon_ideal){  //Argon
				Delta = 3.8e-8;  //[cm]
			}

			Lambda = C1 * T_34 / (P * Delta*Delta);  //[cm]
			Gamma = Cp_34 / Cv_34;
			a = 1.;
			b = (2. - a) / a * (9. * Gamma - 5.) / (2. * (Gamma + 1.));
			h_34 = k_34 / (D_3(hn,hv) / 2. * log(D_4(hn,hv) / D_3(hn,hv)) + b * Lambda /100.* (D_3(hn,hv) / D_4(hn,hv) + 1.));  //[W/m**2-K]
			Kineticq_34conv  = D_3(hn,hv) * pi * h_34 * (T_3 - T_4);  //[W/m]

			// Following compares free-molecular heat transfer with natural convection heat transfer and uses the largest value for heat transfer in annulus 
			if (Kineticq_34conv > Natq_34conv) {
				q_34conv = Kineticq_34conv;  //[W/m]
			}
			else{
				q_34conv = Natq_34conv;  //[W/m]
				h_34 = q_34conv/(D_3(hn,hv)*pi*(T_3-T_4));  //Recalculate the convection coefficient for natural convection
			}
		}

	 };
	 
	 


	/******************************************************************************************************************************
 		FUNCTION fq_34rad :	Radiation heat transfer rate between the absorber surface and glazing inner surface
	******************************************************************************************************************************"
	  NOTE: Temperatures input in terms of degrees K

	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version

	{ 	Radiation heat transfer for a two-surface enclosure.

			Two cases, one if the glazing envelope is intact and one if the glazing is missing or damaged.

			Case 1: Long (infinite) concentric cylinders.

				q' = sigma * PI * D_1 * (T_1^4 - T_2^4) / (1 / EPSILON_1 + (1 - EPSILON_2) / EPSILON_2 * (D_1 / D_2))

				Where,

					q' = radiation heat transfer per unit length [W/m]
					sigma = Stephan-Boltzmann constant [W/m^2-K^4]
					T_1 = absorber outer surface temperature [K]
					T_2 = glazing inner surface temperature [K]
					D_1 = outer absorber diameter [m]
					D_2 = inner glazing diameter [m]
					EPSILON_1 = emissivity of inner surface
					EPSILON_2 = emissivity of outer surface

			Case 2: Small convex object in a large cavity.

				q' = sigma * PI * D_1 * EPSILON_1 * (T_1^4 - T_2^4)
	}*/

	void FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_3_v, int hn, int hv, double &q_34rad, double &h_34){
		//units		(K, K, K, -, -, -, W/m, W/m2-K)
	double sigma=5.67e-8, T_ave;
	T_ave = (T_3 + T_4)/2.;
		if (! GlazingIntact.at(hn,hv)) {
			q_34rad = epsilon_3_v * pi * D_3(hn,hv)  * sigma * (pow(T_3, 4) - pow(T_7, 4));  //[W/m]
			h_34 = q_34rad/(pi*D_3(hn,hv)*(T_3 - T_7));
		}
		else {
			h_34 = sigma*(T_3*T_3 + T_4*T_4)*(T_3 + T_4)/ (1.0 / epsilon_3_v + D_3(hn,hv) / D_4(hn,hv) * ( 1.0 / EPSILON_4(hn,hv) - 1.0)) ;
			q_34rad = pi* D_3(hn,hv) * h_34 * (T_3 - T_4);
		}

	}


	/******************************************************************************************************************************
 		FUNCTION fq_56conv :	Convective heat transfer rate between the glazing outer surface and the ambient air
	******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version

	{ 	h6	Heat Transfer Coefficient

		If no wind, then the Churchill and Chu correlation is used. If wind, then the Zhukauskas's correlation is used. These correlations are described above for q_34conv.
	}*/

	void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6)
	//           units   ( K ,  K , torr, m/s,  W/m    , W/m2-K)
	{
	double alpha_5, alpha_6, C, Cp_5, Cp_56, Cp_6, k_5, k_56, k_6, m, mu_5, mu_56, mu_6, n, Nus_6,
			  nu_5, nu_6, Pr_5, Pr_6, Re_D5, rho_5, rho_56, rho_6, T_56, Nu_bar,
			  nu_56, alpha_56, beta_56, Ra_D5, Pr_56;

		T_56 = (T_5 + T_6)/2.0;  //[K]

		// Thermophysical Properties for air 
		mu_5 = airProps.visc(T_5);  //[kg/m-s]
		mu_6 = airProps.visc(T_6);  //[kg/m-s]
		mu_56 = airProps.visc(T_56);  //[kg/m-s]
		k_5 = airProps.cond(T_5);  //[W/m-K]
		k_6 = airProps.cond(T_6);  //[W/m-K]
		k_56 = airProps.cond(T_56);  //[W/m-K]
		Cp_5 = airProps.Cp(T_5)*1000.;  //[J/kg-K]
		Cp_6 = airProps.Cp(T_6)*1000.;  //[J/kg-K]
		Cp_56 = airProps.Cp(T_56)*1000.;  //[J/kg-K]
		rho_5 = airProps.dens(T_5, P_6);  //[kg/m^3]
		rho_6 = airProps.dens(T_6, P_6);  //[kg/m^3]
		rho_56 = airProps.dens(T_56, P_6);  //[kg/m^3]

		// if the glass envelope is missing then the convection heat transfer from the glass 
		//envelope is forced to zero by T_5 = T_6 
		if (! GlazingIntact(hn,hv)) {
			q_56conv = (T_5 - T_6);  //[W/m]
		}
		else{
			if (v_6 <= 0.1) {

				// Coefficients for Churchill and Chu natural convection correlation //
				nu_56 = mu_56 / rho_56;  //[m^2/s]
				alpha_56 = k_56 / (Cp_56 * rho_56 );  //[m^2/s]
				beta_56 =  1.0 / T_56;  //[1/K]
				Ra_D5 = g *beta_56 * fabs(T_5 - T_6) * pow(D_5(hn,hv),3) / (alpha_56 * nu_56);

				// Warning Statement if following Nusselt Number correlation is used out of range //
				//If (Ra_D5 <= 10**(-5)) or (Ra_D5 >= 10**12) Then CALL WARNING('The result may not be accurate, 
				//since 10**(-5) < Ra_D5 < 10**12 does not hold. See Function fq_56conv. Ra_D5 = XXXA1', Ra_D5)

				// Churchill and Chu correlation for natural convection for a horizontal cylinder //
				Pr_56 = nu_56 / alpha_56;
				Nu_bar = pow(0.60 + (0.387 * pow(Ra_D5, 0.1667)) / pow(1.0 + pow(0.559 / Pr_56, 0.5625), 0.2963) , 2);
				h_6 = Nu_bar * k_56 / D_5(hn,hv);  //[W/m**2-K]
				q_56conv = h_6 * pi * D_5(hn,hv) * (T_5 - T_6);  //[W/m]
			}
			else {

				// Coefficients for Zhukauskas's correlation //
				alpha_5 = k_5 / (Cp_5 * rho_5);  //[m**2/s]
				alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s]
				nu_5 = mu_5 / rho_5;  //[m**2/s]
				nu_6 = mu_6 / rho_6;  //[m**2/s]
				Pr_5 = nu_5 / alpha_5;
				Pr_6 = nu_6 / alpha_6;
				Re_D5 = v_6 * D_5(hn,hv) * rho_6 / mu_6;

				// Warning Statement if following Nusselt Number correlation is used out of range //
	//			if (Pr_6 <= 0.7) or (Pr_6 >= 500) { CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_56conv. Pr_6 = XXXA1', Pr_6)
	//			If (Re_D5 <= 1) or (Re_D5 >= 10**6) Then CALL WARNING('The result may not be accurate, since 1 < Re_D5 < 10**6 does not hold. See Function fq_56conv. Re_D5 = XXXA1 ', Re_D5)

				// Zhukauskas's correlation for forced convection over a long horizontal cylinder //
				if (Pr_6 <= 10) {
					n = 0.37;
				}
				else{
					n = 0.36;
				}

				if (Re_D5 < 40.0) {
					C = 0.75;
					m = 0.4	;
				}
				else{
					if ((40.0 <= Re_D5) && (Re_D5 < 1.e3)) {
						C = 0.51;
						m = 0.5;
					}
					else{
						if ((1.e3 <= Re_D5) && (Re_D5 < 2.e5)) {
							C = 0.26;
							m = 0.6;
						}
						else{
							if ((2.e5 <= Re_D5) && (Re_D5 < 1.e6)) {
								C = 0.076;
								m = 0.7;
							}
						}
					}
				}

				Nus_6 = C * pow(Re_D5,m) *  pow(Pr_6,n)  * pow(Pr_6/Pr_5, 0.25);
				h_6 = Nus_6 * k_6 / D_5(hn,hv);  //[W/m**2-K]
				q_56conv = h_6 * pi * D_5(hn,hv) * (T_5 - T_6);  //[W/m]
			}
		}
	}



	
	/******************************************************************************************************************************
		FUNCTION fq_cond_bracket:	Heat loss estimate through HCE support bracket
	 ******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version
	*/
	double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv){
		//           units                    ( K ,  K , bar, m/s)
	
		double P_brac, D_brac, A_CS_brac, k_brac, T_base, T_brac, T_brac6, mu_brac6, rho_brac6, 
			  Cp_brac6, k_brac6, nu_brac6, Alpha_brac6, Beta_brac6, Ra_Dbrac, Pr_brac6, Nu_bar, h_brac6,
			  mu_brac, mu_6, rho_6, rho_brac, k_6, Cp_brac, nu_6, Cp_6, Nu_brac, Alpha_brac,
			  Re_Dbrac, Pr_brac, Pr_6, n, C, m, L_HCE, alpha_6;


		// effective bracket perimeter for convection heat transfer
		P_brac = 0.2032;  //[m]

		// effective bracket diameter (2 x 1in) 
		D_brac = 0.0508;  //[m]

		// minimum bracket cross-sectional area for conduction heat transfer
		A_CS_brac = 0.00016129;  //[m**2]

		// conduction coefficient for carbon steel at 600 K
		k_brac = 48.0;  //[W/m-K]

		// effective bracket base temperature
		T_base = T_3 - 10.0;  //[C]

		// estimate average bracket temperature 
		T_brac = (T_base + T_6) / 2.0;  //[C]  //NOTE: MJW modified from /3 to /2.. believed to be an error

		// estimate film temperature for support bracket 
		T_brac6 = (T_brac + T_6) /2.0;  //[C]

		// convection coefficient with and without wind
		if (v_6 <= 0.1) {
		
			mu_brac6 = airProps.visc(T_brac6);  //[N-s/m**2]
			rho_brac6 = airProps.dens(T_brac6, P_6);  //[kg/m**3]
			Cp_brac6 = airProps.Cp(T_brac6)*1000.;  //[J/kg-K]
			k_brac6 = airProps.cond(T_brac6);  //[W/m-K]
			nu_brac6 = mu_brac6 / rho_brac6;  //[m**2/s]
			Alpha_brac6 = k_brac6 / (Cp_brac6 * rho_brac6);  //[m**2/s]
			Beta_brac6 =  1.0 / T_brac6;  //[1/K]
			Ra_Dbrac = g * Beta_brac6 * fabs(T_brac - T_6) * D_brac*D_brac*D_brac / (Alpha_brac6 * nu_brac6);

			// Warning Statement if following Nusselt Number correlation is used out of recommended range 
			//If ((Ra_Dbrac <= 1.e-5)) || (Ra_Dbrac >= 1.e12) Then CALL WARNING('The result may not be accurate, 
			//since 10**(-5) < Ra_Dbrac < 10**12 does not hold. See Function fq_cond_bracket. Ra_Dbrac = XXXA1', Ra_Dbrac)

			// Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder 
			Pr_brac6 = nu_brac6 / Alpha_brac6;
			Nu_bar = pow(0.60 + (0.387 * pow(Ra_Dbrac, 0.1667)) / pow(1.0 + pow(0.559 / Pr_brac6, 0.5625), 0.2963) , 2);
			h_brac6 = Nu_bar * k_brac6 / D_brac;  //[W/m**2-K]
		}
		else{
		
			// Thermophysical Properties for air 
			mu_brac = airProps.visc(T_brac);  //[N-s/m**2]
			mu_6 = airProps.visc(T_6);  //[N-s/m**2]
			rho_6 = airProps.dens(T_6, P_6);  //[kg/m**3]
			rho_brac = airProps.dens(T_brac, P_6);  //[kg/m**3]
			k_brac = airProps.cond(T_brac);  //[W/m-K]
			k_6 = airProps.cond(T_6);  //[W/m-K]
			k_brac6 = airProps.cond(T_brac6);  //[W/m-K]
			Cp_brac = airProps.Cp(T_brac)*1000.;  //[J/kg-K]
			Cp_6 = airProps.Cp(T_6)*1000.;  //[J/kg-K]
			nu_6 = mu_6 / rho_6;  //[m**2/s]
			Nu_brac = mu_brac / rho_brac;  //[m**2/s]

			Alpha_brac = k_brac / (Cp_brac * rho_brac);  //[m**2/s]	1.21.14 twn: Fixed unit conversion error
			alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s] 1.21.14 twn: Fixed unit conversion error
			Re_Dbrac = v_6 * D_brac / nu_6;
			Pr_brac = Nu_brac / Alpha_brac;
			Pr_6 = nu_6 / alpha_6;

			// Warning Statements if following Nusselt Correlation is used out of range 
	//		if (Re_Dbrac <= 1) or (Re_Dbrac >= 10**6) { CALL WARNING('The result may not be accurate, since 1 < Re_Dbrac < 10**6 does not hold. See Function fq_cond_bracket. Re_Dbrac = XXXA1', Re_Dbrac)
	//		If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_cond_bracket. Pr_6 = XXXA1', Pr_6)

			// Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) 
			if (Pr_6 <= 10.) {
				n = 0.37;
			} 
			else {
				n = 0.36;
			}

			if (Re_Dbrac < 40.) {
				C = 0.75;
				m = 0.4	;
			} 
			else {

				if ((40. <= Re_Dbrac) && (Re_Dbrac< 1.e3)) {
					C = 0.51;
					m = 0.5;
				} 
				else {
					if ((1.e3 <= Re_Dbrac) && (Re_Dbrac < 2.e5)) {
						C = 0.26;
						m = 0.6;
					} 
					else {
						if ((2.e5 <= Re_Dbrac) && (Re_Dbrac < 1.e6)) {
							C = 0.076;
							m = 0.7;
						}
					}
				}
			}

			// Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
			Nu_bar = C * pow(Re_Dbrac,m)  * pow(Pr_6,n) * pow(Pr_6 / Pr_brac,0.25);
			h_brac6 = Nu_bar  *  k_brac6  /  D_brac;  //[W/m**2-K]
	
		}

		// estimated conduction heat loss through HCE support brackets / HCE length 
		L_HCE = 4.06;  //[m]
		return sqrt(h_brac6 * P_brac * k_brac * A_CS_brac) * (T_base - T_6)/L_HCE;  //[W/m]

	}

	

	/******************************************************************************************************************************
		Subroutine pOpticalEfficiency:  Optical Efficiencies based on HCE type
	******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version
	
	subroutine OpticalEfficiency(OptCharType,CollectorType, reflectivity, K, Shadowing, &
								 TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror,Dirt_HCE, Error, ColOptEff)
	
	implicit none

	integer,intent(in):: OptCharType, CollectorType
	real(8),intent(inout):: reflectivity, shadowing, trackingError, GeomEffects, Rho_mirror_clean, &
							Dirt_mirror, Dirt_HCE, Error
	real(8),intent(out):: ColOptEff
	real(8):: K


	//Various methods for characterizing the optical performance of collectors are anticipated.  Among these will be
	//the traditional SAM method of specifying the various "derate" factors or selecting these from a library.  The 
	//other methods are likely going to involve calculation of an intercept factor based on input of surface character-
	//istics. 

	select case(OptCharType)
	case(1)  //The traditional method of entering derate factors that roll up into a single value:
		If (CollectorType == 1) then   // 'User-Defined' 
			Shadowing = min(1.0,Shadowing)
			TrackingError = min(1.0,TrackingError)
			GeomEffects = min(1.0,GeomEffects)
			Rho_mirror_clean = min(1.0,Rho_mirror_clean)
			Dirt_mirror = min(1.0,Dirt_mirror)
			Dirt_HCE = min(1.0,Dirt_HCE)
			Error = min(1.0,Error)
		}

		If (CollectorType == 2) then    //'LS-2'
			Shadowing = 0.974
			TrackingError = 0.994
			GeomEffects = 0.98
			Rho_mirror_clean = 0.935
			Dirt_mirror = min(1.0,reflectivity/Rho_mirror_clean)
			Dirt_HCE = min(1.0,(1.0+ Dirt_mirror)/2.0)
			Error = 0.96
		}

 		If ((CollectorType == 3) || (CollectorType == 4)) then    //'LS-3' or 'IST'
			Shadowing = 0.974
			TrackingError = 0.994
			GeomEffects = 0.98
			Rho_mirror_clean = 0.935
			Dirt_mirror = min(1.0,reflectivity/Rho_mirror_clean)
			Dirt_HCE = min(1.0,(1.0+ Dirt_mirror)/2.0)
			Error = 0.96
		}
	
		ColOptEff = Shadowing * TrackingError * GeomEffects * Rho_mirror_clean * Dirt_mirror * Dirt_HCE * Error * K

	case(2:)
		continue //reserve space for additional characterization methods here...
	end select


	end subroutine
	*/

	

	/******************************************************************************************************************************
		FUNCTION fk_23:	Absorber conductance
	******************************************************************************************************************************"
	{ Based on linear fit of data from "Alloy Digest, Sourcebook, Stainless Steels"; ASM International, 2000.}
	*/

	double FK_23(double T_2, double T_3, int hn, int hv)
	{
		double T_23;

		//Absorber materials:
		// (1)   304L
		// (2)   216L
		// (3)   321H
		// (4)   B42 Copper Pipe

		T_23 = (T_2 + T_3) / 2. - 273.15;  //[C]
		return AbsorberPropMat(hn,hv)->cond(T_23);
		
	}

	/***************************************************************************************************
	*********************************************************************
	* PipeFlow_turbulent:                                               *
	* This procedure calculates the average Nusselt number and friction *
	* factor for turbulent flow in a pipe given Reynolds number (Re),   *
	* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
	* the relative roughness}                                           *
	*********************************************************************
	*/ /*
	//subroutine PipeFlow(Re, Pr, LoverD,relRough, Nusselt, f)
	void PipeFlow(double Re, double Pr, double LoverD, double relRough, double &Nusselt, double &f){
	

		double f_fd,Nusselt_L, Gz, Gm, Nusselt_T, Nusselt_H,fR,X;

		//Correlation for laminar flow.. Note that no transitional effects are considered
		if (Re < 2300.) {
			//This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe 
			//..given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD) 
			//..and the relative roughness}
			Gz=Re*Pr/LoverD;
			X=LoverD/Re;
			fR=3.44/sqrt(X)+(1.25/(4*X)+16-3.44/sqrt(X))/(1+0.00021*pow(X,-2));
			f=4.*fR/Re;
			//{f$='Shah' {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts", 
			//..Academic Press, 1978 ,Eqn 192, p98}}
			Gm=pow(Gz,1./3.);
			Nusselt_T=3.66+((0.049+0.02/Pr)*pow(Gz,1.12))/(1+0.065*pow(Gz,0.7));
			Nusselt_H=4.36+((0.1156 +0.08569 /pow(Pr,0.4))*Gz)/(1+0.1158*pow(Gz,0.6));
			//{Nusselt$='Nellis and Klein fit to Hornbeck'  {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
			//..Academic Press, 1978 ,Tables  20 and 22}}
			Nusselt = Nusselt_T;  //Constant temperature Nu is better approximation
		}
		else { //Correlation for turbulent flow
 			f_fd = pow(0.79*log(Re)-1.64, -2); //Petukhov, B.S., in Advances in Heat Transfer, Vol. 6, Irvine and Hartnett, Academic Press, 1970
			Nusselt_L= ((f_fd/8.)*(Re-1000)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976

			if (relRough > 1e-5) {

			  //f=8.*((8./Re)**12+((2.457*log(1./((7./Re)**0.9+0.27*(RelRough))))**16+(37530./Re)**16)**(-1.5))**(1./12.)
			  //mjw 8.30.2010 :: not used  
        
			  f_fd=pow(-2.*log10(2*relRough/7.4-5.02*log10(2*relRough/7.4+13/Re)/Re), -2);
	
			  Nusselt_L= ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976}
			}
			f=f_fd*(1.+pow(1./LoverD, 0.7)); //account for developing flow
			Nusselt= Nusselt_L*(1.+pow(1./LoverD, 0.7));  //account for developing flow
		}
  
	}  */

	/*
	***************************************************************************************************
	 Trough system piping loss model
	***************************************************************************************************

	 This piping loss model is derived from the pressure drop calculations presented in the 
	 following document:

	   Parabolic Trough Solar System Piping Model
	   Final Report May 13, 2002 � December 31, 2004

	   B. Kelly
	   Nexant, Inc. San Francisco, California

	   D. Kearney
	   Kearney & Associates
	   Vashon, Washington

	   Subcontract Report
	   NREL/SR-550-40165
	   July 2006

	 ----------------------------
	 Note on use of this function
	 ----------------------------
	 The function returns the pressure drop across a given length of pipe, and also accounts for 
	 a variety of possible pressure-loss components. This function should be called multiple times -
	 once for each section under consideration.  For example, separate calls should be made for the
	 HCE pressure drop, the pressure drop in each section of the header in which flow/geometrical 
	 conditions vary, the section of pipe leading to the header, and so on.

	 ----------------------------
	 Inputs
	 ----------------------------
	 No | Name         | Description                           | Units     |  Type
	===================================================================================
	  1 | m_dot        | Mass flow rate of the fluid           | kg/s      | float
	  2 | T            | Fluid temperature                     | K         | float
	  3 | P            | Fluid pressure                        | Pa        | float
	  4 | D            | Diameter of the contact surface       | m         | float
	  5 | Rough        | Pipe roughness                        | m         | float
      6 | L_pipe       | Length of pipe for pressure drop      | m         | float
      7 | Nexp         | Number of expansions                  | none      | float
      8 | Ncon         | Number of contractions                | none      | float
	  9 | Nels         | Number of standard elbows             | none      | float
	 10 | Nelm         | Number of medium elbows               | none      | float
	 11 | Nell         | Number of long elbows                 | none      | float
	 12 | Ngav         | Number of gate valves                 | none      | float
	 13 | Nglv         | Number of globe valves                | none      | float
	 14 | Nchv         | Number of check valves                | none      | float
	 15 | Nlw          | Number of loop weldolets              | none      | float
	 16 | Nlcv         | Number of loop control valves         | none      | float
	 17 | Nbja         | Number of ball joint assemblies       | none      | float
	===================================================================================
	 ----------------------------
	 Outputs
	 ----------------------------
	 1. PressureDrop  (Pa)
	*/
	double PressureDrop(double m_dot, double T, double P, double D, double Rough, double L_pipe, 
		double Nexp, double Ncon, double Nels, double Nelm, double Nell, double Ngav, double Nglv, 
		double Nchv, double Nlw, double Nlcv, double Nbja){
                                       
		double rho, v_dot, mu, nu, u_fluid, Re, f, DP_pipe, DP_exp,DP_con,DP_els,DP_elm,DP_ell,DP_gav,
				  DP_glv,DP_chv,DP_lw,DP_lcv,DP_bja, HL_pm;
	
		//Calculate fluid properties and characteristics
		rho = htfProps.dens(T,P);
		mu = htfProps.visc(T);
		nu = mu/rho;
		v_dot = m_dot/rho;   //fluid volumetric flow rate
		u_fluid = v_dot/(pi*(D/2.)*(D/2.));  //Fluid mean velocity

		//Dimensionless numbers
		Re = u_fluid*D/nu;
		//if(Re<2300.) then
		//    f = 64./max(Re,1.0)
		//else
		f = FricFactor(Rough/D,Re);
		if( f == 0 ) return std::numeric_limits<double>::quiet_NaN();
		//}

		//Calculation of pressure loss from pipe length
		HL_pm = f*u_fluid*u_fluid/(2.*D*g);
		DP_pipe = HL_pm*rho*g*L_pipe;

		//Calculation of pressure loss from Fittings
		DP_exp = 0.25*rho*u_fluid*u_fluid*Nexp;
		DP_con = 0.25*rho*u_fluid*u_fluid*Ncon;
		DP_els = 0.9 * D / f * HL_pm * rho * g * Nels;
		DP_elm = 0.75 * D / f * HL_pm * rho * g * Nelm;
		DP_ell = 0.6 * D / f * HL_pm * rho * g * Nell;
		DP_gav = 0.19 * D / f * HL_pm * rho * g * Ngav;
		DP_glv = 10.0 * D / f * HL_pm * rho * g * Nglv;
		DP_chv = 2.5 * D / f * HL_pm * rho * g * Nchv;
		DP_lw = 1.8 * D / f * HL_pm * rho * g * Nlw;
		DP_lcv = 10.0 * D / f * HL_pm * rho * g * Nlcv;
		DP_bja = 8.69 * D / f * HL_pm * rho * g * Nbja;

		return DP_pipe + DP_exp + DP_con + DP_els + DP_elm + DP_ell + DP_gav + DP_glv + DP_chv + DP_lw + DP_lcv + DP_bja;

	}

	
	/**************************************************************************************************
	 Friction factor (taken from Piping loss model)
	***************************************************************************************************
	 Uses an iterative method to solve the implicit friction factor function.
	 For more on this method, refer to Fox, et al., 2006 Introduction to Fluid Mechanics.			 */

	double FricFactor(double Rough, double Reynold){

		double Test, TestOld, X, Xold, Slope;
		double Acc = .01; //0.0001
		int NumTries;

		if(Reynold < 2750.) {
			return 64./max(Reynold,1.0);
		}

		X = 33.33333;  //1. / 0.03
		TestOld = X + 2. * log10(Rough / 3.7 + 2.51 * X / Reynold);
		Xold = X;
		X = 28.5714;  //1. / (0.03 + 0.005)
		NumTries = 0;

		while(NumTries < 21){
			NumTries ++;
			Test = X + 2 * log10(Rough / 3.7 + 2.51 * X / Reynold);
			if (fabs(Test - TestOld) <= Acc) {
				return 1. / (X * X);
			}

			Slope = (Test - TestOld) / (X - Xold);
			Xold = X;
			TestOld = Test;
			X = max((Slope * X - Test) / Slope,1.e-5);
		}
		
		//call Messages(-1," Could not find friction factor solution",'Warning',0,250) 
		return 0;
	}

    // Returns runner mass flow for a given runner index
    double m_dot_runner(double m_dot_field, int nfieldsec, int irnr) {
        int nrnrsec = (int)floor(float(nfieldsec) / 4.0) + 1;

        if (irnr < 0 || irnr > 2*nrnrsec - 1) { throw std::invalid_argument("Invalid runner index"); }
        
        int irnr_onedir;
        double m_dot_rnr;
        double m_dot_rnr_0;
        double m_dot_rnr_1;

        // convert index to a mass flow equivalent cold runner index
        if (irnr > nrnrsec - 1) {
            irnr_onedir = 2 * nrnrsec - irnr - 1;
        }
        else {
            irnr_onedir = irnr;
        }

        m_dot_rnr_0 = m_dot_field / 2.;
        m_dot_rnr_1 = m_dot_rnr_0 * (1. - float(nfieldsec % 4) / float(nfieldsec));

        switch (irnr_onedir) {
            case 0:
                m_dot_rnr = m_dot_rnr_0;
            case 1:
                m_dot_rnr = m_dot_rnr_1;
            default:
                m_dot_rnr = m_dot_rnr_1 - (irnr_onedir - 1)*m_dot_field / float(nfsec) * 2;
        }

        return max(m_dot_rnr, 0.0);
    }

    // Returns header mass flow for a given header index
    double m_dot_header(double m_dot_field, int nfieldsec, int nLoopsField, int ihdr) {
        int nhdrsec = (int)ceil(float(nLoopsField) / float(nfieldsec * 2));  // in the cold or hot headers

        if (ihdr < 0 || ihdr > 2*nhdrsec - 1) { throw std::invalid_argument("Invalid header index"); }
        
        int ihdr_onedir;

        // convert index to a mass flow equivalent cold header index
        if (ihdr > nhdrsec - 1) {
            ihdr_onedir = 2*nhdrsec - ihdr - 1;
        }
        else {
            ihdr_onedir = ihdr;
        }

        double m_dot_oneloop = m_dot_field / float(nLoopsField);
        return m_dot_field / float(nfieldsec) - ihdr_onedir * 2 * m_dot_oneloop;
    }
	/**************************************************************************************************
	---------------------------------------------------------------------------------
	--Inputs
	   * nhsec - [-] number of header sections
	   * nfsec - [-] number of field section
	   * nrunsec- [-] number of unique runner diameter sections
	   * rho   - [kg/m3] Fluid density
	   * V_cold_max - [m/s] Maximum cold fluid velocity at design
	   * V_cold_min - [m/s] Minimum cold fluid velocity at design
       * V_hot_max -  [m/s] Maximum hot fluid velocity at design
       * V_hot_min -  [m/s] Minimum hot fluid velocity at design
       * N_max_hdr_diams - [-] Maximum number of diameters in each hot/cold header
	   * m_dot - [kg/s] Mass flow rate at design
	--Outputs
	   * D_hdr - [m] An ARRAY containing the header diameter for each loop section
       * m_dot_hdr - [kg/s] Mass flow rate in each header section at design
       * V_hdr - [m/s] Velocity in each header section at design
	   * D_runner - [m] An ARRAY containing the diameter of the runner pipe sections
       * m_dot_rnr - [kg/s] Mass flow rate in each runner section at design
       * V_rnr - [m/s] Velocity in each runner section at design
	   * summary - Address of string variable on which summary contents will be written.
       * custom_diams - [-] Should the diameters be input instead of calculated? 
	---------------------------------------------------------------------------------			*/

	void rnr_and_hdr_design(unsigned nhsec, int nfsec, unsigned nrunsec, double rho_cold, double rho_hot, double V_cold_max, double V_cold_min,
        double V_hot_max, double V_hot_min, int N_max_hdr_diams, double m_dot, util::matrix_t<double> &D_hdr, util::matrix_t<double> &D_runner,
        util::matrix_t<double> &m_dot_rnr, util::matrix_t<double> &m_dot_hdr, util::matrix_t<double> &V_rnr, util::matrix_t<double> &V_hdr,
        std::string *summary = NULL, bool custom_diams = false){
	
		//resize the header matrices if they are incorrect
		//real(8),intent(out):: D_hdr(nhsec), D_runner(nrunsec)
        if (!custom_diams) {
		    if(D_hdr.ncells() != 2*nhsec) D_hdr.resize(2*nhsec);
		    if(D_runner.ncells() != 2*nrunsec) D_runner.resize(2*nrunsec);
        }
        if(m_dot_hdr.ncells() != 2*nhsec) m_dot_hdr.resize(2*nhsec);
        if(V_hdr.ncells() != 2*nhsec) V_hdr.resize(2*nhsec);
        if(m_dot_rnr.ncells() != 2*nrunsec) m_dot_rnr.resize(2*nrunsec);
        if(V_rnr.ncells() != 2*nrunsec) V_rnr.resize(2*nrunsec);

		//----
		int nend, nd;
		unsigned nst;
		double m_dot_hdrs, m_dot_2loops;
        double V_cold_avg = (V_cold_max + V_cold_min) / 2.;
        double V_hot_avg = (V_hot_max + V_hot_min) / 2.;
		
		//Mass flow into 1 header
		m_dot_hdrs = m_dot/float(nfsec);
		//Mass flow into the 2 loops attached to a single header section
		m_dot_2loops = m_dot_hdrs/float(nhsec);

		//Runner diameters
		//runner pipe needs some length to go from the power block to the headers
		m_dot_rnr[0] = m_dot/2.;   //mass flow through half-length runners is always half of total
        m_dot_rnr[2 * nrunsec - 1] = m_dot_rnr[0];
        if (!custom_diams) {
		    D_runner.at(0) = CSP::pipe_sched(sqrt(4.*m_dot_rnr[0]/(rho_cold*V_cold_max*pi)));
            D_runner.at(2 * nrunsec - 1) = CSP::pipe_sched(sqrt(4.*m_dot_rnr[2 * nrunsec - 1] / (rho_hot*V_hot_avg*CSP::pi)));
        }
        V_rnr.at(0) = 4.*m_dot_rnr[0] / (rho_cold*pow(D_runner.at(0), 2)*pi);
        V_rnr.at(2 * nrunsec - 1) = 4.*m_dot_rnr[2 * nrunsec - 1] / (rho_hot*pow(D_runner.at(2 * nrunsec - 1), 2)*CSP::pi);
		for (unsigned i=1; i<nrunsec; i++){
            if (i == 1) {
		        m_dot_rnr[i] = m_dot_rnr[i-1]*(1.-float(nfsec%4)/float(nfsec));  //Adjust mass flow for first full-length runners when nfsec/2==odd
            }
            else {
				m_dot_rnr[i] = max(m_dot_rnr[i-1] - m_dot_hdrs*2, 0.0);
            }
            m_dot_rnr[2 * nrunsec - i - 1] = m_dot_rnr[i];
            if (!custom_diams) {
			    D_runner[i] = CSP::pipe_sched(sqrt(4.*m_dot_rnr[i]/(rho_cold*V_cold_max*pi)));
                D_runner[2 * nrunsec - i - 1] = CSP::pipe_sched(sqrt(4.*m_dot_rnr[2 * nrunsec - i - 1] / (rho_hot*V_hot_avg*CSP::pi)));
            }
            V_rnr.at(i) = 4.*m_dot_rnr[i] / (rho_cold*pow(D_runner.at(i), 2)*pi);
            V_rnr.at(2 * nrunsec - i - 1) = 4.*m_dot_rnr[2 * nrunsec - i - 1] / (rho_hot*pow(D_runner.at(2 * nrunsec - i - 1), 2)*CSP::pi);
		}

		//Calculate each section in the cold header
        double m_dot_enter = 0;
        double V_enter = 0;  // for cold header, V_enter is the velocity in the pipe
        double D_hdr_next = 0; double V_enter_next = 0;
        double D_hdr_next2 = 0; double V_enter_next2 = 0;
        nd = 0;
        if (custom_diams) {
            for (std::size_t i = 0; i < nhsec; i++) {
                if (i == 0) {
                    m_dot_enter = m_dot_hdrs;
                }
                else {
                    m_dot_enter -= m_dot_2loops;
                }
                V_enter = 4.*m_dot_enter / (rho_cold*pi*D_hdr[i] * D_hdr[i]);
                m_dot_hdr[i] = m_dot_enter;
                V_hdr[i] = V_enter;
            }
        }
        else {
            for (std::size_t i = 0; i < nhsec; i++) {
                if (i == 0) {
                    m_dot_enter = m_dot_hdrs;
                    // Size cold header diameter using V_max to allow for mass loss into loops
                    // Select actual pipe that is larger (param=true) than ideal pipe b/c if smaller it will definitely exceed V_max
                    D_hdr[i] = CSP::pipe_sched(sqrt(4.*m_dot_enter / (rho_cold*V_hdr_cold_max*pi)), true);
                    V_enter = 4.*m_dot_enter / (rho_cold*pi*D_hdr[i] * D_hdr[i]);
                    if (V_enter < V_hdr_cold_min) {  // if the entering velocity will be below the minimum (it won't exceed V_max)
                        D_hdr_next = CSP::pipe_sched(sqrt(4.*m_dot_enter / (rho_cold*V_hdr_cold_max*pi)), false);   // size smaller this time, will definitely exceed V_max
                        V_enter_next = 4.*m_dot_enter / (rho_cold*pi*D_hdr_next*D_hdr_next);
                        // Choose the smaller diameter (faster V) if it's closer to being in range
                        if (V_enter_next - V_hdr_cold_max <= V_hdr_cold_min - V_enter) {  // '<=' is so the smaller (faster) pipe is preferred in a tie
                            D_hdr[i] = D_hdr_next;
                        }
                    }
                    nd++;
                }
                else if (nd < N_max_hdr_diams) {
                    m_dot_enter -= m_dot_2loops;
                    V_enter = 4.*m_dot_enter / (rho_cold*pi*D_hdr[i - 1] * D_hdr[i - 1]);  // assuming no diameter change
                    if (V_enter < V_hdr_cold_min) {   // if the entering velocity will be below the minimum if there is no diameter change
                        D_hdr_next = CSP::pipe_sched(sqrt(4.*m_dot_enter / (rho_cold*V_hdr_cold_max*pi)), true);  // size larger than optimal so it won't exceed V_max
                        V_enter_next = 4.*m_dot_enter / (rho_cold*pi*D_hdr_next*D_hdr_next);
                        if (V_enter_next < V_hdr_cold_min) {  // if the velocity is still below V_min (it won't exceed V_max)
                            // try smaller than the optimal this time and choose the one with the velocity closest to being in range
                            D_hdr_next2 = CSP::pipe_sched(sqrt(4.*m_dot_enter / (rho_cold*V_hdr_cold_max*pi)), false);  // size smaller this time (will exceed V_max)
                            V_enter_next2 = 4.*m_dot_enter / (rho_cold*pi*D_hdr_next2*D_hdr_next2);
                            if (V_hdr_cold_min - V_enter_next < V_enter_next2 - V_hdr_cold_max) {   // '<' is so the smaller (faster) pipe is preferred in a tie
                                D_hdr[i] = D_hdr_next;
                            }
                            else {
                                D_hdr[i] = D_hdr_next2;
                            }
                        }
                        else {
                            D_hdr[i] = D_hdr_next;
                        }
                        if ((D_hdr[i - 1] - D_hdr[i]) > 0.001) { nd++; }
                    }
                    else {
                        D_hdr[i] = D_hdr[i - 1];
                    }
                }
                else {
                    m_dot_enter -= m_dot_2loops;
                    D_hdr[i] = D_hdr[i - 1];        // no diameter change allowed
                }
                m_dot_hdr[i] = m_dot_enter;
                V_hdr[i] = 4.*m_dot_hdr[i] / (rho_cold*CSP::pi*D_hdr[i] * D_hdr[i]);
            }
        }


        //Calculate each section in the hot header
        double m_dot_leave = 0;
        double V_leave = 0;  // for hot header, V_leave is the velocity in the pipe
        D_hdr_next = 0; double V_leave_next = 0;
        D_hdr_next2 = 0; double V_leave_next2 = 0;
        nd = 0;
        if (custom_diams) {
            for (std::size_t i = nhsec; i < 2 * nhsec; i++) {
                m_dot_leave += m_dot_2loops;
                V_leave = 4.*m_dot_leave / (rho_hot*pi*D_hdr[i] * D_hdr[i]);
                m_dot_hdr[i] = m_dot_leave;
                V_hdr[i] = V_leave;
            }
        }
        else {
            for (std::size_t i = nhsec; i < 2*nhsec; i++) {
                if (i == nhsec) {
                    m_dot_leave = m_dot_2loops;
                    // Size hot header diameter using V_min to allow for mass addition from downstream loops
                    // Select actual pipe that is smaller than ideal pipe b/c if sizing larger it will definitely deceed V_min
                    D_hdr[i] = CSP::pipe_sched(sqrt(4.*m_dot_leave / (rho_hot*V_hdr_hot_min*pi)), false);
                    V_leave = 4.*m_dot_leave / (rho_hot*pi*D_hdr[i] * D_hdr[i]);
                    if (V_leave > V_hdr_hot_max) {   // if the leaving velocity will be above the maximum (it won't deceed V_min)
                        D_hdr_next = CSP::pipe_sched(sqrt(4.*m_dot_leave / (rho_hot*V_hdr_hot_min*pi)), true);   // size larger this time, will definitely be below V_min
                        V_leave_next = 4.*m_dot_leave / (rho_hot*pi*D_hdr_next*D_hdr_next);
                            // Choose the larger diameter (slower V) if it's closer to being in range
                            if (V_hdr_hot_min - V_leave_next < V_leave - V_hdr_hot_max) {  // '<' is so the smaller (cheaper) pipe is preferred in a tie
                                D_hdr[i] = D_hdr_next;
                            }
                    }
                    nd++;
                }
                else if (nd < N_max_hdr_diams) {
                    m_dot_leave += m_dot_2loops;
                    V_leave = 4.*m_dot_leave / (rho_hot*pi*D_hdr[i - 1] * D_hdr[i - 1]);  // assuming no diameter change
                    if (V_leave > V_hdr_hot_max) {   // if the leaving velocity will be above the maximum if there is no diameter change
                        D_hdr_next = CSP::pipe_sched(sqrt(4.*m_dot_leave / (rho_hot*V_hdr_hot_min*pi)), false);  // size smaller than optimal so it won't deceed V_min
                        V_leave_next = 4.*m_dot_leave / (rho_hot*pi*D_hdr_next*D_hdr_next);
                        if (V_leave_next > V_hdr_hot_max) {  // if the velocity is still above V_max (it won't be below V_min)
                            // try larger than the optimal this time and choose the one with the velocity closest to being in range
                            D_hdr_next2 = CSP::pipe_sched(sqrt(4.*m_dot_leave / (rho_hot*V_hdr_hot_min*pi)), true);  // size larger this time (will be below V_min)
                            V_leave_next2 = 4.*m_dot_leave / (rho_hot*pi*D_hdr_next2*D_hdr_next2);
                            if (V_leave_next - V_hdr_hot_max <= V_hdr_hot_min - V_leave_next2) {   // '<=' is so the smaller (cheaper) pipe is preferred in a tie
                                D_hdr[i] = D_hdr_next;
                            }
                            else {
                                D_hdr[i] = D_hdr_next2;
                            }
                        }
                        else {
                            D_hdr[i] = D_hdr_next;
                        }
                        if ((D_hdr[i] - D_hdr[i - 1]) > 0.001) { nd++; }
                    }
                    else {
                        D_hdr[i] = D_hdr[i - 1];
                    }
                }
                else {
                    m_dot_leave += m_dot_2loops;
                    D_hdr[i] = D_hdr[i - 1];        // no diameter change allowed
                }
                m_dot_hdr[i] = m_dot_leave;
                V_hdr[i] = 4.*m_dot_hdr[i] / (rho_hot*CSP::pi*D_hdr[i] * D_hdr[i]);
            }
        }
		
		//Print the results to a string
		if(summary != NULL){
			summary->clear();
			char tstr[TSTRLEN];
			//Write runner diam
			MySnprintf(tstr, TSTRLEN, 
				"Piping geometry file\n\nMaximum fluid velocity: %.2lf\nMinimum fluid velocity: %.2lf\n\n", 
				max(V_cold_max, V_hot_max), min(V_cold_min, V_hot_min));
			summary->append(tstr);

			for (unsigned i=0; i<2*nrunsec; i++){
				MySnprintf(tstr, TSTRLEN, "To section %d header pipe diameter: %.4lf m (%.2lf in)\n",i+1, D_runner[i], D_runner[i]*mtoinch);
				summary->append(tstr);
			}
			//Write header diams
			summary->append( "Loop No. | Diameter [m] | Diameter [in] | Diam. ID\n--------------------------------------------------\n" );

			nd=1;
			for (unsigned i=0; i<2*nhsec; i++){
				if(i>1) {
					if(D_hdr[i] != D_hdr.at(i-1)) nd=nd+1;
				}
				MySnprintf(tstr, TSTRLEN, "  %4d   |    %6.4lf    |    %6.4lf     | %3d\n", i+1, D_hdr[i], D_hdr[i]*mtoinch, nd);
				summary->append(tstr);
			}
			//110 format(2X,I4,3X,"|",4X,F6.4,4X,"|",4X,F6.3,5X,"|",1X,I3)
		}

	}
	

};

TCS_IMPLEMENT_TYPE( sam_mw_trough_type250, "Physical trough solar field model", "Mike Wagner", 1, sam_mw_trough_type250_variables, NULL, 1 );
