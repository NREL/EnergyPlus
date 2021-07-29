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

#include <cmath>

using namespace std;

enum{
	//parameters and inputs
	P_NMOD,
	P_NRECVAR,
	P_NLOOPS,
	P_ETA_PUMP,
	P_HDR_ROUGH,
	P_THETA_STOW,
	P_THETA_DEP,
	P_FIELDCONFIG,
	P_T_STARTUP,
	P_PB_RATED_CAP,
	P_M_DOT_HTFMIN,
	P_M_DOT_HTFMAX,
	P_T_LOOP_IN_DES,
	P_T_LOOP_OUT,
	P_FLUID,
	P_T_FIELD_INI,
	P_HTF_DATA,
	P_T_FP,
	P_I_BN_DES,
	P_V_HDR_MAX,
	P_V_HDR_MIN,
	P_PIPE_HL_COEF,
	P_SCA_DRIVES_ELEC,
	P_FTHROK,
	P_FTHRCTRL,
	P_COLAZ,
	P_SOLAR_MULT,
	P_MC_BAL_HOT,
	P_MC_BAL_COLD,
	P_MC_BAL_SCA,

	P_OPT_MODEL,
	P_A_APERTURE,
	P_REFLECTIVITY,
	P_TRACKINGERROR,
	P_GEOMEFFECTS,
	P_DIRT_MIRROR,
	P_ERROR,
	P_L_MOD,
	P_IAM_T_COEFS,
	P_IAM_L_COEFS,
	P_OPTICALTABLE,

	P_REC_MODEL,
	P_HCE_FIELDFRAC,
	P_D_ABS_IN,
	P_D_ABS_OUT,
	P_D_GLASS_IN,
	P_D_GLASS_OUT,
	P_D_PLUG,
	P_FLOW_TYPE,
	P_ROUGH,
	P_ALPHA_ENV,
	P_EPSILON_ABS_1,
	P_EPSILON_ABS_2,
	P_EPSILON_ABS_3,
	P_EPSILON_ABS_4,
	P_ALPHA_ABS,
	P_TAU_ENVELOPE,
	P_EPSILON_GLASS,
	P_GLAZINGINTACTIN,
	P_P_A,
	P_ANNULUSGAS,
	P_ABSORBERMATERIAL,
	P_SHADOWING,
	P_DIRT_ENV,
	P_DESIGN_LOSS,
	P_L_MOD_SPACING,
	P_L_CROSSOVER,
	P_HL_T_COEFS,
	P_HL_W_COEFS,
	P_DP_NOMINAL,
	P_DP_COEFS,
	P_REC_HTF_VOL,
	P_T_AMB_SF_DES,
	P_V_WIND_DES,
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
	I_SOLARZEN,
	I_LATITUDE,
	I_LONGITUDE,
	I_TIMEZONE,

	O_T_SYS_H,
	O_M_DOT_AVAIL,
    O_M_DOT_FIELD_HTF,
	O_Q_AVAIL,
	O_DP_TOT,
	O_W_DOT_PUMP,
	O_E_FP_TOT,
	O_T_SYS_C,
	O_ETA_OPTICAL,
	O_EQOPTEFF,
	O_SF_DEF,
	O_M_DOT_HTF_TOT,
	O_E_BAL_STARTUP,
	O_Q_INC_SF_TOT,
	O_Q_ABS_TOT,
	O_Q_LOSS_TOT,
	O_M_DOT_HTF,
	O_Q_LOSS_SPEC_TOT,
	O_TRACK_PAR_TOT,
	O_PIPE_HL,
	O_Q_DUMP,
	O_PHI_T,
	O_THETA_L,
	O_T_LOOP_OUTLET,
	O_C_HTF_AVE,
	O_Q_FIELD_DELIVERED,
	O_ETA_THERMAL,
	O_E_LOOP_ACCUM,
	O_E_HDR_ACCUM,
	O_E_TOT_ACCUM,
	O_E_FIELD,
	O_PIPING_SUMMARY,
    O_DEFOCUS,

	//Include N_max
	N_MAX
};


tcsvarinfo sam_mw_lf_type262_variables[] = {
	// vartype,		      datatype,		            index,				       name,		                                                                             label,          units,           meta,          group,  default_value
	{ TCS_PARAM,          TCS_NUMBER,              P_NMOD,                   "nMod",                                                   "Number of collector modules in a loop",         "none",             "",             "",           "16" },
	{ TCS_PARAM,          TCS_NUMBER,           P_NRECVAR,                "nRecVar",                                                          "Number of receiver variantions",         "none",             "",             "",            "4" },
	{ TCS_PARAM,          TCS_NUMBER,            P_NLOOPS,                 "nLoops",                                                            "Number of loops in the field",         "none",             "",             "",          "105" },
	{ TCS_PARAM,          TCS_NUMBER,          P_ETA_PUMP,               "eta_pump",                                                                     "HTF pump efficiency",         "none",             "",             "",         "0.85" },
	{ TCS_PARAM,          TCS_NUMBER,         P_HDR_ROUGH,              "HDR_rough",                                                                   "Header pipe roughness",            "m",             "",             "",     "4.57E-05" },
	{ TCS_PARAM,          TCS_NUMBER,        P_THETA_STOW,             "theta_stow",                                                                              "stow angle",          "deg",             "",             "",          "170" },
	{ TCS_PARAM,          TCS_NUMBER,         P_THETA_DEP,              "theta_dep",                                                                            "deploy angle",          "deg",             "",             "",           "10" },
	{ TCS_PARAM,          TCS_NUMBER,       P_FIELDCONFIG,            "FieldConfig",                                                              "Number of subfield headers",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,         P_T_STARTUP,              "T_startup",        "The required temperature of the system before the power block can be switched on",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PB_RATED_CAP,           "pb_rated_cap",                                                                    "Rated plant capacity",          "MWe",             "",             "",          "111" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMIN,           "m_dot_htfmin",                                                              "Minimum loop HTF flow rate",         "kg/s",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMAX,           "m_dot_htfmax",                                                              "Maximum loop HTF flow rate",         "kg/s",             "",             "",           "12" },
	{ TCS_PARAM,          TCS_NUMBER,     P_T_LOOP_IN_DES,          "T_loop_in_des",                                                           "Design loop inlet temperature",            "C",             "",             "",          "293" },
	{ TCS_PARAM,          TCS_NUMBER,        P_T_LOOP_OUT,             "T_loop_out",                                                          "Target loop outlet temperature",            "C",             "",             "",          "500" },
	{ TCS_PARAM,          TCS_NUMBER,             P_FLUID,                  "Fluid",                                                                  "Field HTF fluid number",         "none",             "",             "",           "17" },
	{ TCS_PARAM,          TCS_NUMBER,       P_T_FIELD_INI,            "T_field_ini",                                                               "Initial field temperature",            "C",             "",             "",          "300" },
	{ TCS_PARAM,          TCS_MATRIX,          P_HTF_DATA,         "field_fl_props",                                                                     "Fluid property data",         "none","7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",             "",             "" },
	{ TCS_PARAM,          TCS_NUMBER,              P_T_FP,                   "T_fp",                       "Freeze protection temperature (heat trace activation temperature)",            "C",             "",             "",          "260" },
	{ TCS_PARAM,          TCS_NUMBER,          P_I_BN_DES,               "I_bn_des",                                                             "Solar irradiation at design",         "W/m2",             "",             "",          "950" },
	{ TCS_PARAM,          TCS_NUMBER,         P_V_HDR_MAX,              "V_hdr_max",                                            "Maximum HTF velocity in the header at design",          "m/s",             "",             "",            "3" },
	{ TCS_PARAM,          TCS_NUMBER,         P_V_HDR_MIN,              "V_hdr_min",                                            "Minimum HTF velocity in the header at design",          "m/s",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PIPE_HL_COEF,           "Pipe_hl_coef",                       "Loss coefficient from the header, runner pipe, and non-HCE piping",       "W/m2-K",             "",             "",         "0.45" },
	{ TCS_PARAM,          TCS_NUMBER,   P_SCA_DRIVES_ELEC,        "SCA_drives_elec",                                                  "Tracking power, in Watts per SCA drive",     "W/module",             "",             "",          "125" },
	{ TCS_PARAM,          TCS_NUMBER,            P_FTHROK,                 "fthrok",                                      "Flag to allow partial defocusing of the collectors",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,          P_FTHRCTRL,               "fthrctrl",                                                                     "Defocusing strategy",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,             P_COLAZ,                  "ColAz",                                                                 "Collector azimuth angle",          "deg",             "",             "",            "0" },
	{ TCS_PARAM,          TCS_NUMBER,        P_SOLAR_MULT,             "solar_mult",                                                                          "Solar multiple",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,        P_MC_BAL_HOT,             "mc_bal_hot",                               "The heat capacity of the balance of plant on the hot side",   "kWht/K-MWt",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,       P_MC_BAL_COLD,            "mc_bal_cold",                              "The heat capacity of the balance of plant on the cold side",   "kWht/K-MWt",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,        P_MC_BAL_SCA,             "mc_bal_sca",                        "Non-HTF heat capacity associated with each SCA - per meter basis",      "Wht/K-m",             "",             "",          "4.5" },

	{ TCS_PARAM,          TCS_NUMBER,         P_OPT_MODEL,              "opt_model",      "The optical model (1=Solar position ; 2=Collector incidence table ; 3 = IAM polys)",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,        P_A_APERTURE,             "A_aperture",                                               "Reflective aperture area of the collector",           "m2",             "",             "",        "470.3" },
	{ TCS_PARAM,          TCS_NUMBER,      P_REFLECTIVITY,           "reflectivity",                                               "Solar-weighted mirror reflectivity value ",         "none",             "",             "",        "0.935" },
	{ TCS_PARAM,          TCS_NUMBER,     P_TRACKINGERROR,          "TrackingError",                                                                   "Tracking error derate",         "none",             "",             "",        "0.994" },
	{ TCS_PARAM,          TCS_NUMBER,       P_GEOMEFFECTS,            "GeomEffects",                                                                 "Geometry effects derate",         "none",             "",             "",         "0.98" },
	{ TCS_PARAM,          TCS_NUMBER,       P_DIRT_MIRROR,            "Dirt_mirror",                                                      "User-defined dirt on mirror derate",         "none",             "",             "",         "0.95" },
	{ TCS_PARAM,          TCS_NUMBER,             P_ERROR,                  "Error",                                              "User-defined general optical error derate ",         "none",             "",             "",         "0.99" },
	{ TCS_PARAM,          TCS_NUMBER,             P_L_MOD,                  "L_mod",                                                      "The length of the collector module",            "m",             "",             "",         "44.8" },
	{ TCS_PARAM,           TCS_ARRAY,       P_IAM_T_COEFS,            "IAM_T_coefs",                               "Incidence angle modifier coefficients - transversal plane",         "none",             "",             "","0.9896,0.044,-0.0721,-0.2327,0." },
	{ TCS_PARAM,           TCS_ARRAY,       P_IAM_L_COEFS,            "IAM_L_coefs",                              "Incidence angle modifier coefficients - longitudinal plane",         "none",             "",             "","1.0031,-0.2259,0.5368,-1.6434,0.7222" },
	{ TCS_PARAM,          TCS_MATRIX,      P_OPTICALTABLE,           "OpticalTable",                                                  "Values of the optical efficiency table",         "none",             "",             "",             "" },

	{ TCS_PARAM,          TCS_NUMBER,         P_REC_MODEL,              "rec_model",                                        "Receiver model type (1=Polynomial ; 2=Evac tube)",         "none",             "",             "",            "2" },
	{ TCS_PARAM,           TCS_ARRAY,     P_HCE_FIELDFRAC,          "HCE_FieldFrac",                                    "The fraction of the field occupied by this HCE type ",         "none",             "",             "","0.985,0.01,0.005,0" },
	{ TCS_PARAM,           TCS_ARRAY,          P_D_ABS_IN,               "D_abs_in",                                                        "The inner absorber tube diameter",            "m",             "",             "","0.066,0.066,0.066,0.066" },
	{ TCS_PARAM,           TCS_ARRAY,         P_D_ABS_OUT,              "D_abs_out",                                                        "The outer absorber tube diameter",            "m",             "",             "","0.07,0.07,0.07,0.07" },
	{ TCS_PARAM,           TCS_ARRAY,        P_D_GLASS_IN,             "D_glass_in",                                                      "The inner glass envelope diameter ",            "m",             "",             "","0.115,0.115,0.115,0.115" },
	{ TCS_PARAM,           TCS_ARRAY,       P_D_GLASS_OUT,            "D_glass_out",                                                      "The outer glass envelope diameter ",            "m",             "",             "","0.12,0.12,0.12,0.12" },
	{ TCS_PARAM,           TCS_ARRAY,            P_D_PLUG,                 "D_plug",                                      "The diameter of the absorber flow plug (optional) ",            "m",             "",             "",      "0,0,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,         P_FLOW_TYPE,              "Flow_type",                                                      "The flow type through the absorber",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,             P_ROUGH,                  "Rough",                                                      "Roughness of the internal surface ",            "m",             "",             "","4.50E-05,4.50E-05,4.50E-05,4.50E-05" },
	{ TCS_PARAM,           TCS_ARRAY,         P_ALPHA_ENV,              "alpha_env",                                                                   "Envelope absorptance ",         "none",             "",             "","0.02,0.02,0,0" },
	{ TCS_PARAM,          TCS_MATRIX,     P_EPSILON_ABS_1,          "epsilon_abs_1",                                                    "Absorber emittance - HCE variation 1",         "none",             "",             "","[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ TCS_PARAM,          TCS_MATRIX,     P_EPSILON_ABS_2,          "epsilon_abs_2",                                                    "Absorber emittance - HCE variation 2",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,     P_EPSILON_ABS_3,          "epsilon_abs_3",                                                    "Absorber emittance - HCE variation 3",         "none",             "",             "",        "0,.65" },
	{ TCS_PARAM,          TCS_MATRIX,     P_EPSILON_ABS_4,          "epsilon_abs_4",                                                    "Absorber emittance - HCE variation 4",         "none",             "",             "",          "0,0" },
	{ TCS_PARAM,           TCS_ARRAY,         P_ALPHA_ABS,              "alpha_abs",                                                                   "Absorber absorptance ",         "none",             "",             "","0.96,0.96,0.8,0" },
	{ TCS_PARAM,           TCS_ARRAY,      P_TAU_ENVELOPE,           "Tau_envelope",                                                                  "Envelope transmittance",         "none",             "",             "","0.963,0.963,1,0" },
	{ TCS_PARAM,           TCS_ARRAY,     P_EPSILON_GLASS,          "epsilon_glass",                                                               "Glass envelope emissivity",         "none",             "",             "","0.86,0.86,1,0" },
	{ TCS_PARAM,           TCS_ARRAY,   P_GLAZINGINTACTIN,        "GlazingIntactIn",                                            "The glazing intact flag {1=true, else=false}",         "none",             "",             "",      "1,1,0,1" },
	{ TCS_PARAM,           TCS_ARRAY,               P_P_A,                    "P_a",                                                                    "Annulus gas pressure",         "torr",             "",             "","0.0001,750,750,0" },
	{ TCS_PARAM,           TCS_ARRAY,        P_ANNULUSGAS,             "AnnulusGas",                                                  "Annulus gas type (1=air, 26=Ar, 27=H2)",         "none",             "",             "",    "27,1,1,27" },
	{ TCS_PARAM,           TCS_ARRAY,  P_ABSORBERMATERIAL,       "AbsorberMaterial",                                                                  "Absorber material type",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,         P_SHADOWING,              "Shadowing",                                                  "Receiver bellows shadowing loss factor",         "none",             "",             "","0.96,0.96,0.96,0.963" },
	{ TCS_PARAM,           TCS_ARRAY,          P_DIRT_ENV,               "dirt_env",                                               "Loss due to dirt on the receiver envelope",         "none",             "",             "","0.98,0.98,1,0.98" },
	{ TCS_PARAM,           TCS_ARRAY,       P_DESIGN_LOSS,            "Design_loss",                                                            "Receiver heat loss at design",          "W/m",             "",             "","150,1100,1500,0" },
	{ TCS_PARAM,          TCS_NUMBER,     P_L_MOD_SPACING,          "L_mod_spacing",                                    "Piping distance between sequential modules in a loop",            "m",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,       P_L_CROSSOVER,            "L_crossover",                                                    "Length of crossover piping in a loop",            "m",             "",             "",           "15" },
	{ TCS_PARAM,           TCS_ARRAY,        P_HL_T_COEFS,             "HL_T_coefs",                                        "HTF temperature-dependent heat loss coefficients",        "W/m-K",             "",             "","0.,0.672,0.002556,0.,0." },
	{ TCS_PARAM,           TCS_ARRAY,        P_HL_W_COEFS,             "HL_w_coefs",                                             "Wind-speed-dependent heat loss coefficients",    "W/m-(m/s)",             "",             "","1.,0.,0.,0.,0." },
	{ TCS_PARAM,          TCS_NUMBER,        P_DP_NOMINAL,             "DP_nominal",                              "Pressure drop across a single collector assembly at design",          "bar",             "",             "",          "2.5" },
	{ TCS_PARAM,           TCS_ARRAY,          P_DP_COEFS,               "DP_coefs",                                           "Pressure drop mass flow based part-load curve",         "none",             "",             "",  "0.,1.,0.,0." },
	{ TCS_PARAM,          TCS_NUMBER,       P_REC_HTF_VOL,            "rec_htf_vol",                         "Volume of HTF in a single collector unit per unit aperture area",      "L/m2-ap",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_AMB_SF_DES,           "T_amb_sf_des",                                    "Ambient design-point temperature for the solar field",            "C",             "",             "",           "25" },
	{ TCS_PARAM,          TCS_NUMBER,        P_V_WIND_DES,             "V_wind_des",                                                              "Design-point wind velocity",          "m/s",             "",             "",          "3.5" },

	// Field design calculations
	{ TCS_PARAM,          TCS_NUMBER,       PO_A_APER_TOT,             "A_aper_tot",                                                         "Total solar field aperture area",          "m^2",             "",             "",        "-1.23" },


	{ TCS_INPUT,          TCS_NUMBER,               I_I_B,                    "I_b",                                                "Direct normal incident solar irradiation",     "kJ/m2-hr",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DB,                   "T_db",                                                                "Dry bulb air temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,            I_V_WIND,                 "V_wind",                                                                      "Ambient windspeed ",          "m/s",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_P_AMB,                  "P_amb",                                                                        "Ambient pressure",          "atm",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DP,                   "T_dp",                                                                "The dewpoint temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_T_COLD_IN,              "T_cold_in",                                                                  "HTF return temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_M_DOT_IN,               "m_dot_in",                                                        "HTF mass flow rate at the inlet ",        "kg/hr",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_DEFOCUS,                "defocus",                                                                        "Defocus control ",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_SOLARAZ,                "SolarAz",                                                                    "Solar azimuth angle ",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_SOLARZEN,               "SolarZen",                                                                      "Solar zenith angle",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_LATITUDE,               "latitude",                                                    "Site latitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_LONGITUDE,              "longitude",                                                   "Site longitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_TIMEZONE,               "timezone",                                                                               "Time zone",           "hr",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_NUMBER,           O_T_SYS_H,                "T_sys_h",                                                      "Solar field HTF outlet temperature",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_M_DOT_AVAIL,            "m_dot_avail",                                                       "HTF mass flow rate from the field",        "kg/hr",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER,   O_M_DOT_FIELD_HTF,        "m_dot_field_htf",                         "HTF mass flow rate from the field, including when recirculating",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_Q_AVAIL,                "q_avail",                                                     "Thermal power produced by the field",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_DP_TOT,                 "DP_tot",                                                                 "Total HTF pressure drop",          "bar",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_W_DOT_PUMP,             "W_dot_pump",                                                      "Required solar field pumping power",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_E_FP_TOT,               "E_fp_tot",                                                                "Freeze protection energy",           "MW",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_T_SYS_C,                "T_sys_c",                                                             "Collector inlet temperature",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_ETA_OPTICAL,            "eta_optical",                                                      "Collector total optical efficiency",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_EQOPTEFF,               "EqOptEff",                "Total solar field optical efficiency - including receiver optical losses",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_SF_DEF,                 "sf_def",                                         "The fraction of the solar field that's on focus",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_HTF_TOT,          "m_dot_htf_tot",                                                "The actual flow rate through the field..",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_E_BAL_STARTUP,          "E_bal_startup",                                                                 "Startup energy consumed",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_Q_INC_SF_TOT,           "q_inc_sf_tot",                                                       "Total power incident on the field",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_Q_ABS_TOT,              "q_abs_tot",                                                                   "Total absorbed energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_Q_LOSS_TOT,             "q_loss_tot",                                               "Total receiver thermal and optical losses",          "MWt",             "",             "",             "" },
	// Feb 27, 2015, twn: renamed "m_dot_htf" (below) to "m_dot_htf2" because it conflicted with an input variable of the same name in "sam_mw_pt_type224.cpp" when retrieving outputs in "cmod_tcsmslf.cpp"
	{ TCS_OUTPUT,          TCS_NUMBER,         O_M_DOT_HTF,             "m_dot_htf2",                                                              "Flow rate in a single loop",         "kg/s",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,   O_Q_LOSS_SPEC_TOT,        "q_loss_spec_tot",                        "Field-average receiver thermal losses (convection and radiation)",          "W/m",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_TRACK_PAR_TOT,          "track_par_tot",                                "Parasitic electric power consumed by the tracking drives",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_PIPE_HL,                "Pipe_hl",                                     "Pipe heat loss in the hot header and the hot runner",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_Q_DUMP,                 "q_dump",                                                                   "Dumped thermal energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,             O_PHI_T,                  "phi_t",                                "Solar incidence angle in the collector transversal plane",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_THETA_L,                "theta_L",                               "Solar incidence angle in the collector longitudinal plane",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_T_LOOP_OUTLET,          "t_loop_outlet",                               "HTF temperature immediately subsequent to the loop outlet",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_C_HTF_AVE,              "c_htf_ave",                                                       "Average solar field specific heat",       "J/kg-K",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER, O_Q_FIELD_DELIVERED,      "q_field_delivered",                                               "Total solar field thermal power delivered",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_ETA_THERMAL,            "eta_thermal",                                          "Solar field thermal efficiency (power out/ANI)",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_E_LOOP_ACCUM,           "E_loop_accum",                               "Accumulated internal energy change rate in the loops ONLY",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_E_HDR_ACCUM,            "E_hdr_accum",                              "Accumulated internal energy change rate in the headers/SGS",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_E_TOT_ACCUM,            "E_tot_accum",                                           "Total accumulated internal energy change rate",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_E_FIELD,                "E_field",                                   "Accumulated internal energy in the entire solar field",         "MWht",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_STRING,    O_PIPING_SUMMARY,         "piping_summary",                                    "String containing description of field piping design",         "none",             "",             "",             "" },
    { TCS_OUTPUT,          TCS_NUMBER,           O_DEFOCUS,            "defocus_rel",                "Relative defocus for passing back to the controller to force convergence",         "none",             "",             "",             "" },

	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};



class sam_mw_lf_type262 : public tcstypeinterface
{
private:
	HTFProperties htfProps, airProps;
	OpticalDataTable optical_table;
	double pi,Pi,d2r,r2d, g, mtoinch;

	//parameters and inputs
	int nMod;		//Number of collector modules in a loop
	int nRecVar;		//Number of receiver variantions
	int nLoops;		//Number of loops in the field
	double eta_pump;		//HTF pump efficiency
	double HDR_rough;		//Header pipe roughness
	double theta_stow;		//stow angle
	double theta_dep;		//deploy angle
	int FieldConfig;		//Number of subfield headers
	double T_startup;		//The required temperature of the system before the power block can be switched on
	double pb_rated_cap;		//Rated plant capacity
	double m_dot_htfmin;		//Minimum loop HTF flow rate
	double m_dot_htfmax;		//Maximum loop HTF flow rate
	double T_loop_in_des;		//Design loop inlet temperature
	double T_loop_out;		//Target loop outlet temperature
	int Fluid;		//Field HTF fluid number
	double T_field_ini;		//Initial field temperature
	double* HTF_data_in;		//Fluid property data
	int nrow_HTF_data,	ncol_HTF_data;
	double T_fp;		//Freeze protection temperature (heat trace activation temperature)
	double I_bn_des;		//Solar irradiation at design
	double V_hdr_max;		//Maximum HTF velocity in the header at design
	double V_hdr_min;		//Minimum HTF velocity in the header at design
	double Pipe_hl_coef;		//Loss coefficient from the header, runner pipe, and non-HCE piping
	double SCA_drives_elec;		//Tracking power, in Watts per SCA drive
	int fthrok;		//Flag to allow partial defocusing of the collectors
	int fthrctrl;		//Defocusing strategy
	double ColAz;		//[rad] Collector azimuth angle - converted from [deg] in init()
	double solar_mult;		//Solar multiple
	double mc_bal_hot;		//The heat capacity of the balance of plant on the hot side
	double mc_bal_cold;		//The heat capacity of the balance of plant on the cold side
	double mc_bal_sca;		//Non-HTF heat capacity associated with each SCA - per meter basis

	int opt_model;		//The optical model (1=Solar position ; 2=Collector incidence table ; 3 = IAM polys)
	double A_aperture;		//Reflective aperture area of the collector
	double reflectivity;		//Solar-weighted mirror reflectivity value 
	double TrackingError;		//Tracking error derate
	double GeomEffects;		//Geometry effects derate
	double Dirt_mirror;		//User-defined dirt on mirror derate
	double Error;		//User-defined general optical error derate 
	double L_mod;		//The length of the collector module
	double* IAM_T_coefs;		//Incidence angle modifier coefficients - transversal plane
	int nval_IAM_T_coefs;
	double* IAM_L_coefs;		//Incidence angle modifier coefficients - longitudinal plane
	int nval_IAM_L_coefs;
	double* OpticalTable_in;		//Values of the optical efficiency table
	int nrow_OpticalTable,	ncol_OpticalTable;

	int rec_model;		//Receiver model type (1=Polynomial ; 2=Evac tube)
	double* HCE_FieldFrac;		//The fraction of the field occupied by this HCE type 
	int nval_HCE_FieldFrac;
	double* D_abs_in;		//The inner absorber tube diameter
	int nval_D_abs_in;
	double* D_abs_out;		//The outer absorber tube diameter
	int nval_D_abs_out;
	double* D_glass_in;		//The inner glass envelope diameter 
	int nval_D_glass_in;
	double* D_glass_out;		//The outer glass envelope diameter 
	int nval_D_glass_out;
	double* D_plug;		//The diameter of the absorber flow plug (optional) 
	int nval_D_plug;
	double* Flow_type;		//The flow type through the absorber
	int nval_Flow_type;
	double* Rough;		//Roughness of the internal surface 
	int nval_Rough;
	double* alpha_env;		//Envelope absorptance 
	int nval_alpha_env;
	double* epsilon_abs_1_in;		//Absorber emittance - HCE variation 1
	int nrow_epsilon_abs_1,	ncol_epsilon_abs_1;
	double* epsilon_abs_2_in;		//Absorber emittance - HCE variation 2
	int nrow_epsilon_abs_2,	ncol_epsilon_abs_2;
	double* epsilon_abs_3_in;		//Absorber emittance - HCE variation 3
	int nrow_epsilon_abs_3,	ncol_epsilon_abs_3;
	double* epsilon_abs_4_in;		//Absorber emittance - HCE variation 4
	int nrow_epsilon_abs_4,	ncol_epsilon_abs_4;
	double* alpha_abs;		//Absorber absorptance 
	int nval_alpha_abs;
	double* Tau_envelope;		//Envelope transmittance
	int nval_Tau_envelope;
	double* epsilon_glass;		//Glass envelope emissivity
	int nval_epsilon_glass;
	double* GlazingIntactIn;		//The glazing intact flag {1=true, else=false}
	int nval_GlazingIntactIn;
	double* P_a;		//Annulus gas pressure
	int nval_P_a;
	double* AnnulusGas;		//Annulus gas type (1=air, 26=Ar, 27=H2)
	int nval_AnnulusGas;
	double* AbsorberMaterial;		//Absorber material type
	int nval_AbsorberMaterial;
	double* Shadowing;		//Receiver bellows shadowing loss factor
	int nval_Shadowing;
	double* dirt_env;		//Loss due to dirt on the receiver envelope
	int nval_dirt_env;
	double* Design_loss;		//Receiver heat loss at design
	int nval_Design_loss;
	double L_mod_spacing;		//Piping distance between sequential modules in a loop
	double L_crossover;		//Length of crossover piping in a loop
	double* HL_T_coefs;		//HTF temperature-dependent heat loss coefficients
	int nval_HL_T_coefs;
	double* HL_w_coefs;		//Wind-speed-dependent heat loss coefficients
	int nval_HL_w_coefs;
	double DP_nominal;		//Pressure drop across a single collector assembly at design
	double* DP_coefs;		//Pressure drop mass flow based part-load curve
	int nval_DP_coefs;
	double rec_htf_vol;		//Volume of HTF in a single collector unit per unit aperture area
	double T_amb_sf_des;		//Ambient design-point temperature for the solar field
	double V_wind_des;		//Design-point wind velocity

	double I_b;		//Direct normal incident solar irradiation
	double T_db;		//Dry bulb air temperature
	double V_wind;		//Ambient windspeed 
	double P_amb;		//Ambient pressure
	double T_dp;		//The dewpoint temperature
	double T_cold_in;		//HTF return temperature
	double m_dot_in;		//HTF mass flow rate at the inlet 
	double defocus;		//Defocus control 
	double SolarAz;		//Solar azimuth angle 
	double SolarZen;		//Solar zenith angle
	double latitude;		//Site latitude read from weather file
	double longitude;		//Site longitude read from weather file
	double timezone;		//Time zone

	double T_sys_h;		//Solar field HTF outlet temperature
	double m_dot_avail;		//HTF mass flow rate from the field
    double m_dot_field_htf;  //HTF mass flow rate from the field, including when recirculating
	double q_avail;		//Thermal power produced by the field
	double DP_tot;		//Total HTF pressure drop
	double W_dot_pump;		//Required solar field pumping power
	double E_fp_tot;		//Freeze protection energy
	double T_sys_c;		//Collector inlet temperature
	double eta_optical;		//Collector total optical efficiency
	double EqOptEff;		//Total solar field optical efficiency - including receiver optical losses
	double sf_def;		//The fraction of the solar field that's on focus
	double m_dot_htf_tot;		//The actual flow rate through the field..
	double E_bal_startup;		//Startup energy consumed
	double q_inc_sf_tot;		//Total power incident on the field
	double q_abs_tot;		//Total absorbed energy
	double q_loss_tot;		//Total receiver thermal and optical losses
	double m_dot_htf;		//Flow rate in a single loop
	double q_loss_spec_tot;		//Field-average receiver thermal losses (convection and radiation)
	double track_par_tot;		//Parasitic electric power consumed by the tracking drives
	double Pipe_hl;		//Pipe heat loss in the hot header and the hot runner
	double q_dump;		//Dumped thermal energy
	double phi_t;		//Solar incidence angle in the collector transversal plane
	double theta_L;		//Solar incidence angle in the collector longitudinal plane
	double t_loop_outlet;		//HTF temperature immediately subsequent to the loop outlet
	double c_htf_ave;		//Average solar field specific heat
	double q_field_delivered;		//Total solar field thermal power delivered
	double eta_thermal;		//Solar field thermal efficiency (power out/ANI)
	double E_loop_accum;		//Accumulated internal energy change rate in the loops ONLY
	double E_hdr_accum;		//Accumulated internal energy change rate in the headers/SGS
	double E_tot_accum;		//Total accumulated internal energy change rate
	double E_field;		//Accumulated internal energy in the entire solar field
	string piping_summary;		//String containing description of field piping design

	util::matrix_t<double> HTF_data, OpticalTable, epsilon_abs_1, epsilon_abs_2, epsilon_abs_3, epsilon_abs_4;

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
		nrunsec,	//Number of unique runner diameters
		qq;	//Number of solution iterations
	//Other matrices
	util::matrix_t<HTFProperties*> AnnulusGasMat;
	util::matrix_t<AbsorberProps*> AbsorberPropMat;
	util::matrix_t<double> A_cs, D_h, ColOptEff /*nMod*/;
	util::matrix_t<bool> GlazingIntact;
	emit_table epsilon_abs;
	util::matrix_t<double> D_runner, L_runner, D_hdr;

	util::matrix_t<double> 
		T_htf_in, //size nMod
		T_htf_out, //size nMod
		T_htf_ave, //size nMod
		q_loss, //size nHCEvar
		q_abs, //size nRecVar
		c_htf, //size  nMod
		rho_htf, //size nMod
		DP_tube, //size nMod
		E_abs_field,  //size 
		E_int_loop,  //size 
		E_accum,  //size 
		E_avail,  //size 
		E_abs_max, //size 
		v_1, //size nMod
		q_loss_SCAtot, //size nMod
		q_abs_SCAtot, //size nMod
		T_htf_in0, //size nMod
		T_htf_out0, //size nMod
		T_htf_ave0, //size nMod
		E_fp,  //size nMod
		q_1abs_tot,  //size nMod
		q_1abs,  //size nRecVar
		q_SCA, //size nMod
		SCADefocusArray; //size nMod
	double T_sys_c_last, T_sys_h_last; //stored values for header thermal inertia calculations
	double N_run_mult;	
	double v_hot, v_cold;	//Header HTF volume
	double defocus_new, defocus_old, ftrack, q_i;
	bool 
		no_fp,	//Freeze protection flag
		is_fieldgeom_init;	//Flag to indicate whether the field geometry has been initialized
	double T_cold_in_1, c_hdr_cold, start_time, dt, shift,
		q_SCA_tot, m_dot_htfX, Header_hl_cold, Runner_hl_cold, Pipe_hl_cold, T_loop_in,
		T_loop_outX, Runner_hl_hot, Header_hl_hot, Pipe_hl_hot, c_hdr_hot, time_hr, dt_hr, 
		eta_opt_fixed, A_loop;
	int day_of_year, SolveMode, dfcount;
	std::vector<double> mv_HCEguessargs;

	double m_htf_prop_min;

public:

	sam_mw_lf_type262( tcscontext *cxt, tcstypeinfo *ti ) 
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
		nMod	= -1;
		nRecVar	= -1;
		nLoops	= -1;
		eta_pump	= std::numeric_limits<double>::quiet_NaN();
		HDR_rough	= std::numeric_limits<double>::quiet_NaN();
		theta_stow	= std::numeric_limits<double>::quiet_NaN();
		theta_dep	= std::numeric_limits<double>::quiet_NaN();
		FieldConfig	= -1;
		T_startup	= std::numeric_limits<double>::quiet_NaN();
		pb_rated_cap	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htfmin	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htfmax	= std::numeric_limits<double>::quiet_NaN();
		T_loop_in_des	= std::numeric_limits<double>::quiet_NaN();
		T_loop_out	= std::numeric_limits<double>::quiet_NaN();
		Fluid	= -1;
		T_field_ini	= std::numeric_limits<double>::quiet_NaN();
		HTF_data_in	= NULL;
		nrow_HTF_data = -1, ncol_HTF_data = -1;
		T_fp	= std::numeric_limits<double>::quiet_NaN();
		I_bn_des	= std::numeric_limits<double>::quiet_NaN();
		V_hdr_max	= std::numeric_limits<double>::quiet_NaN();
		V_hdr_min	= std::numeric_limits<double>::quiet_NaN();
		Pipe_hl_coef	= std::numeric_limits<double>::quiet_NaN();
		SCA_drives_elec	= std::numeric_limits<double>::quiet_NaN();
		fthrok	= -1;
		fthrctrl	= -1;
		ColAz	= std::numeric_limits<double>::quiet_NaN();
		solar_mult	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_hot	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_cold	= std::numeric_limits<double>::quiet_NaN();
		mc_bal_sca	= std::numeric_limits<double>::quiet_NaN();
		opt_model	= -1;
		A_aperture	= std::numeric_limits<double>::quiet_NaN();
		reflectivity	= std::numeric_limits<double>::quiet_NaN();
		TrackingError	= std::numeric_limits<double>::quiet_NaN();
		GeomEffects	= std::numeric_limits<double>::quiet_NaN();
		Dirt_mirror	= std::numeric_limits<double>::quiet_NaN();
		Error	= std::numeric_limits<double>::quiet_NaN();
		L_mod	= std::numeric_limits<double>::quiet_NaN();
		IAM_T_coefs	= NULL;
		nval_IAM_T_coefs = -1;
		IAM_L_coefs	= NULL;
		nval_IAM_L_coefs = -1;
		OpticalTable_in	= NULL;
		nrow_OpticalTable = -1, ncol_OpticalTable = -1;
		rec_model	= -1;
		HCE_FieldFrac	= NULL;
		nval_HCE_FieldFrac = -1;
		D_abs_in	= NULL;
		nval_D_abs_in = -1;
		D_abs_out	= NULL;
		nval_D_abs_out = -1;
		D_glass_in	= NULL;
		nval_D_glass_in = -1;
		D_glass_out	= NULL;
		nval_D_glass_out = -1;
		D_plug	= NULL;
		nval_D_plug = -1;
		Flow_type	= NULL;
		nval_Flow_type = -1;
		Rough	= NULL;
		nval_Rough = -1;
		alpha_env	= NULL;
		nval_alpha_env = -1;
		epsilon_abs_1_in	= NULL;
		nrow_epsilon_abs_1 = -1, ncol_epsilon_abs_1 = -1;
		epsilon_abs_2_in	= NULL;
		nrow_epsilon_abs_2 = -1, ncol_epsilon_abs_2 = -1;
		epsilon_abs_3_in	= NULL;
		nrow_epsilon_abs_3 = -1, ncol_epsilon_abs_3 = -1;
		epsilon_abs_4_in	= NULL;
		nrow_epsilon_abs_4 = -1, ncol_epsilon_abs_4 = -1;
		alpha_abs	= NULL;
		nval_alpha_abs = -1;
		Tau_envelope	= NULL;
		nval_Tau_envelope = -1;
		epsilon_glass	= NULL;
		nval_epsilon_glass = -1;
		GlazingIntactIn	= NULL;
		nval_GlazingIntactIn = -1;
		P_a	= NULL;
		nval_P_a = -1;
		AnnulusGas	= NULL;
		nval_AnnulusGas = -1;
		AbsorberMaterial	= NULL;
		nval_AbsorberMaterial = -1;
		Shadowing	= NULL;
		nval_Shadowing = -1;
		dirt_env	= NULL;
		nval_dirt_env = -1;
		Design_loss	= NULL;
		nval_Design_loss = -1;
		L_mod_spacing	= std::numeric_limits<double>::quiet_NaN();
		L_crossover	= std::numeric_limits<double>::quiet_NaN();
		HL_T_coefs	= NULL;
		nval_HL_T_coefs = -1;
		HL_w_coefs	= NULL;
		nval_HL_w_coefs = -1;
		DP_nominal	= std::numeric_limits<double>::quiet_NaN();
		DP_coefs	= NULL;
		nval_DP_coefs = -1;
		rec_htf_vol	= std::numeric_limits<double>::quiet_NaN();
		T_amb_sf_des	= std::numeric_limits<double>::quiet_NaN();
		V_wind_des	= std::numeric_limits<double>::quiet_NaN();
		I_b	= std::numeric_limits<double>::quiet_NaN();
		T_db	= std::numeric_limits<double>::quiet_NaN();
		V_wind	= std::numeric_limits<double>::quiet_NaN();
		P_amb	= std::numeric_limits<double>::quiet_NaN();
		T_dp	= std::numeric_limits<double>::quiet_NaN();
		T_cold_in	= std::numeric_limits<double>::quiet_NaN();
		m_dot_in	= std::numeric_limits<double>::quiet_NaN();
		defocus	= std::numeric_limits<double>::quiet_NaN();
		SolarAz	= std::numeric_limits<double>::quiet_NaN();
		SolarZen	= std::numeric_limits<double>::quiet_NaN();
		latitude	= std::numeric_limits<double>::quiet_NaN();
		longitude	= std::numeric_limits<double>::quiet_NaN();
		timezone	= std::numeric_limits<double>::quiet_NaN();
		T_sys_h	= std::numeric_limits<double>::quiet_NaN();
		m_dot_avail	= std::numeric_limits<double>::quiet_NaN();
        m_dot_field_htf = std::numeric_limits<double>::quiet_NaN();
		q_avail	= std::numeric_limits<double>::quiet_NaN();
		DP_tot	= std::numeric_limits<double>::quiet_NaN();
		W_dot_pump	= std::numeric_limits<double>::quiet_NaN();
		E_fp_tot	= std::numeric_limits<double>::quiet_NaN();
		T_sys_c	= std::numeric_limits<double>::quiet_NaN();
		eta_optical	= std::numeric_limits<double>::quiet_NaN();
		EqOptEff	= std::numeric_limits<double>::quiet_NaN();
		sf_def	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf_tot	= std::numeric_limits<double>::quiet_NaN();
		E_bal_startup	= std::numeric_limits<double>::quiet_NaN();
		q_inc_sf_tot	= std::numeric_limits<double>::quiet_NaN();
		q_abs_tot	= std::numeric_limits<double>::quiet_NaN();
		q_loss_tot	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf	= std::numeric_limits<double>::quiet_NaN();
		q_loss_spec_tot	= std::numeric_limits<double>::quiet_NaN();
		track_par_tot	= std::numeric_limits<double>::quiet_NaN();
		Pipe_hl	= std::numeric_limits<double>::quiet_NaN();
		q_dump	= std::numeric_limits<double>::quiet_NaN();
		phi_t	= std::numeric_limits<double>::quiet_NaN();
		theta_L	= std::numeric_limits<double>::quiet_NaN();
		t_loop_outlet	= std::numeric_limits<double>::quiet_NaN();
		c_htf_ave	= std::numeric_limits<double>::quiet_NaN();
		q_field_delivered	= std::numeric_limits<double>::quiet_NaN();
		eta_thermal	= std::numeric_limits<double>::quiet_NaN();
		E_loop_accum	= std::numeric_limits<double>::quiet_NaN();
		E_hdr_accum	= std::numeric_limits<double>::quiet_NaN();
		E_tot_accum	= std::numeric_limits<double>::quiet_NaN();
		E_field	= std::numeric_limits<double>::quiet_NaN();
		piping_summary	= "";

		m_htf_prop_min = std::numeric_limits<double>::quiet_NaN();

		mv_HCEguessargs.resize(3);
		std::fill(mv_HCEguessargs.begin(), mv_HCEguessargs.end(), std::numeric_limits<double>::quiet_NaN());

	}

	virtual ~sam_mw_lf_type262(){
		/* Clean up on simulation terminate 
		for(int i=0; i<nHCEt; i++){
			for(int j=0; j<nRecVar; j++){
				delete AbsorberPropMat(i,j);
				delete AnnulusGasMat(i,j);
			}
		}*/
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
			double *fl_mat = value(P_HTF_DATA, &nrows, &ncols);
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r, c) = TCS_MATRIX_INDEX(var(P_HTF_DATA), r, c);

				if ( !htfProps.SetUserDefinedFluid( mat ) )
				{
					message( TCS_ERROR, htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}
		else
		{
			message(TCS_ERROR, "Field HTF code is not recognized");
			return -1;
		}

		//Get values for any parameters here
		nMod = (int)value(P_NMOD);		//Number of collector modules in a loop [none]
		nRecVar = (int)value(P_NRECVAR);		//Number of receiver variantions [none]
		nLoops = (int)value(P_NLOOPS);		//Number of loops in the field [none]
		eta_pump = value(P_ETA_PUMP);		//HTF pump efficiency [none]
		HDR_rough = value(P_HDR_ROUGH);		//Header pipe roughness [m]
		theta_stow = value(P_THETA_STOW);		//stow angle [deg]
		theta_dep = value(P_THETA_DEP);		//deploy angle [deg]
		FieldConfig = (int)value(P_FIELDCONFIG);		//Number of subfield headers [none]
		T_startup = value(P_T_STARTUP);		//The required temperature of the system before the power block can be switched on [C]
		pb_rated_cap = value(P_PB_RATED_CAP);		//Rated plant capacity [MWe]
		m_dot_htfmin = value(P_M_DOT_HTFMIN);		//Minimum loop HTF flow rate [kg/s]
		m_dot_htfmax = value(P_M_DOT_HTFMAX);		//Maximum loop HTF flow rate [kg/s]
		T_loop_in_des = value(P_T_LOOP_IN_DES);		//Design loop inlet temperature [C]
		T_loop_out = value(P_T_LOOP_OUT);		//Target loop outlet temperature [C]
		Fluid = (int)value(P_FLUID);		//Field HTF fluid number [none]
		T_field_ini = value(P_T_FIELD_INI);		//Initial field temperature [C]
		HTF_data_in = value(P_HTF_DATA, &nrow_HTF_data, &ncol_HTF_data);		//Fluid property data [none]
		T_fp = value(P_T_FP);		//Freeze protection temperature (heat trace activation temperature) [C]
		I_bn_des = value(P_I_BN_DES);		//Solar irradiation at design [W/m2]
		V_hdr_max = value(P_V_HDR_MAX);		//Maximum HTF velocity in the header at design [m/s]
		V_hdr_min = value(P_V_HDR_MIN);		//Minimum HTF velocity in the header at design [m/s]
		Pipe_hl_coef = value(P_PIPE_HL_COEF);		//Loss coefficient from the header, runner pipe, and non-HCE piping [W/m2-K]
		SCA_drives_elec = value(P_SCA_DRIVES_ELEC);		//Tracking power, in Watts per SCA drive [W/module]
		fthrok = (int)value(P_FTHROK);		//Flag to allow partial defocusing of the collectors [none]
		fthrctrl = (int)value(P_FTHRCTRL);		//Defocusing strategy [none]
		ColAz = value(P_COLAZ)*CSP::pi*180.0;	//[rad] Collector azimuth angle, convert from [deg]
		solar_mult = value(P_SOLAR_MULT);		//Solar multiple [none]
		mc_bal_hot = value(P_MC_BAL_HOT);		//The heat capacity of the balance of plant on the hot side [kWht/K-MWt]
		mc_bal_cold = value(P_MC_BAL_COLD);		//The heat capacity of the balance of plant on the cold side [kWht/K-MWt]
		mc_bal_sca = value(P_MC_BAL_SCA);		//Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]

		opt_model = (int)value(P_OPT_MODEL);		//The optical model (1=Solar position ; 2=Collector incidence table ; 3 = IAM polys) [none]
		A_aperture = value(P_A_APERTURE);		//Reflective aperture area of the collector [m2]
		reflectivity = value(P_REFLECTIVITY);		//Solar-weighted mirror reflectivity value  [none]
		TrackingError = value(P_TRACKINGERROR);		//Tracking error derate [none]
		GeomEffects = value(P_GEOMEFFECTS);		//Geometry effects derate [none]
		Dirt_mirror = value(P_DIRT_MIRROR);		//User-defined dirt on mirror derate [none]
		Error = value(P_ERROR);		//User-defined general optical error derate  [none]
		L_mod = value(P_L_MOD);		//The length of the collector module [m]
		IAM_T_coefs = value(P_IAM_T_COEFS, &nval_IAM_T_coefs);		//Incidence angle modifier coefficients - transversal plane [none]
		IAM_L_coefs = value(P_IAM_L_COEFS, &nval_IAM_L_coefs);		//Incidence angle modifier coefficients - longitudinal plane [none]
		OpticalTable_in = value(P_OPTICALTABLE, &nrow_OpticalTable, &ncol_OpticalTable);		//Values of the optical efficiency table [none]

		rec_model = (int)value(P_REC_MODEL);		//Receiver model type (1=Polynomial ; 2=Evac tube) [none]
		HCE_FieldFrac = value(P_HCE_FIELDFRAC, &nval_HCE_FieldFrac);		//The fraction of the field occupied by this HCE type  [none]
		D_abs_in = value(P_D_ABS_IN, &nval_D_abs_in);		//The inner absorber tube diameter [m]
		D_abs_out = value(P_D_ABS_OUT, &nval_D_abs_out);		//The outer absorber tube diameter [m]
		D_glass_in = value(P_D_GLASS_IN, &nval_D_glass_in);		//The inner glass envelope diameter  [m]
		D_glass_out = value(P_D_GLASS_OUT, &nval_D_glass_out);		//The outer glass envelope diameter  [m]
		D_plug = value(P_D_PLUG, &nval_D_plug);		//The diameter of the absorber flow plug (optional)  [m]
		Flow_type = value(P_FLOW_TYPE, &nval_Flow_type);		//The flow type through the absorber [none]
		Rough = value(P_ROUGH, &nval_Rough);		//Roughness of the internal surface  [m]
		alpha_env = value(P_ALPHA_ENV, &nval_alpha_env);		//Envelope absorptance  [none]
		epsilon_abs_1_in = value(P_EPSILON_ABS_1, &nrow_epsilon_abs_1, &ncol_epsilon_abs_1);		//Absorber emittance - HCE variation 1 [none]
		epsilon_abs_2_in = value(P_EPSILON_ABS_2, &nrow_epsilon_abs_2, &ncol_epsilon_abs_2);		//Absorber emittance - HCE variation 2 [none]
		epsilon_abs_3_in = value(P_EPSILON_ABS_3, &nrow_epsilon_abs_3, &ncol_epsilon_abs_3);		//Absorber emittance - HCE variation 3 [none]
		epsilon_abs_4_in = value(P_EPSILON_ABS_4, &nrow_epsilon_abs_4, &ncol_epsilon_abs_4);		//Absorber emittance - HCE variation 4 [none]
		alpha_abs = value(P_ALPHA_ABS, &nval_alpha_abs);		//Absorber absorptance  [none]
		Tau_envelope = value(P_TAU_ENVELOPE, &nval_Tau_envelope);		//Envelope transmittance [none]
		epsilon_glass = value(P_EPSILON_GLASS, &nval_epsilon_glass);		//Glass envelope emissivity [none]
		GlazingIntactIn = value(P_GLAZINGINTACTIN, &nval_GlazingIntactIn);		//The glazing intact flag {1=true, else=false} [none]
		P_a = value(P_P_A, &nval_P_a);		//Annulus gas pressure [torr]
		AnnulusGas = value(P_ANNULUSGAS, &nval_AnnulusGas);		//Annulus gas type (1=air, 26=Ar, 27=H2) [none]
		AbsorberMaterial = value(P_ABSORBERMATERIAL, &nval_AbsorberMaterial);		//Absorber material type [none]
		Shadowing = value(P_SHADOWING, &nval_Shadowing);		//Receiver bellows shadowing loss factor [none]
		dirt_env = value(P_DIRT_ENV, &nval_dirt_env);		//Loss due to dirt on the receiver envelope [none]
		Design_loss = value(P_DESIGN_LOSS, &nval_Design_loss);		//Receiver heat loss at design [W/m]
		L_mod_spacing = value(P_L_MOD_SPACING);		//Piping distance between sequential modules in a loop [m]
		L_crossover = value(P_L_CROSSOVER);		//Length of crossover piping in a loop [m]
		HL_T_coefs = value(P_HL_T_COEFS, &nval_HL_T_coefs);		//HTF temperature-dependent heat loss coefficients [W/m-K]
		HL_w_coefs = value(P_HL_W_COEFS, &nval_HL_w_coefs);		//Wind-speed-dependent heat loss coefficients [W/m-(m/s)]
		DP_nominal = value(P_DP_NOMINAL);		//Pressure drop across a single collector assembly at design [bar]
		DP_coefs = value(P_DP_COEFS, &nval_DP_coefs);		//Pressure drop mass flow based part-load curve [none]
		rec_htf_vol = value(P_REC_HTF_VOL);		//Volume of HTF in a single collector unit per unit aperture area [L/m2-ap]
		T_amb_sf_des = value(P_T_AMB_SF_DES);		//Ambient design-point temperature for the solar field [C]
		V_wind_des = value(P_V_WIND_DES);		//Design-point wind velocity [m/s]


		//Put all of the matrices into a more handlable format
		HTF_data.assign(HTF_data_in, nrow_HTF_data, ncol_HTF_data);
		OpticalTable.assign(OpticalTable_in, nrow_OpticalTable, ncol_OpticalTable);
		epsilon_abs_1.assign(epsilon_abs_1_in, nrow_epsilon_abs_1, ncol_epsilon_abs_1);
		epsilon_abs_2.assign(epsilon_abs_2_in, nrow_epsilon_abs_2, ncol_epsilon_abs_2);
		epsilon_abs_3.assign(epsilon_abs_3_in, nrow_epsilon_abs_3, ncol_epsilon_abs_3);
		epsilon_abs_4.assign(epsilon_abs_4_in, nrow_epsilon_abs_4, ncol_epsilon_abs_4);


		/* 
		Set up the optical table object..

		The input should be defined as follows:
		- Data of size nx, ny
		- OpticalTable of size (nx+1)*(ny+1)
		- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
		- First value of remaining ny rows are y-axis values, not data
		- Data is contained in cells i,j : where i>1, j>1
		*/
		double *xax = new double[ncol_OpticalTable-1];
		double *yax = new double[nrow_OpticalTable-1];
		double *data = new double[(ncol_OpticalTable -1) * (nrow_OpticalTable -1)];

		//get the xaxis data values
		for(int i=1; i<ncol_OpticalTable; i++){
			xax[i-1] = OpticalTable.at(0, i)*d2r;
		}
		//get the yaxis data values
		for(int j=1; j<nrow_OpticalTable; j++){
			yax[j-1] = OpticalTable.at(j, 0)*d2r;
		}
		//Get the data values
		for(int j=1; j<nrow_OpticalTable; j++){
			for(int i=1; i<ncol_OpticalTable; i++){
				data[ i-1 + (ncol_OpticalTable-1)*(j-1) ] = OpticalTable.at(j, i);
			}
		}


		//for(int j=0; j<nrow_OpticalTable; j++){
		//	if(j>0) yax[j-1] = OpticalTable.at(j,0)*d2r;
		//	for(int i=0; i<ncol_OpticalTable; i++){
		//		
		//		if(j==0){	//x axis header row
		//			if(i>0) xax[i-1] = OpticalTable.at(0, i)*d2r;
		//		}
		//		else{		//Data rows
		//			if(i>0) data[(j-1)*ncol_OpticalTable + (i-1)] = OpticalTable.at(j, i);
		//		}
		//	}
		//}
		//
		optical_table.AddXAxis(xax, ncol_OpticalTable-1);
		optical_table.AddYAxis(yax, nrow_OpticalTable-1);
		optical_table.AddData(data);
		delete [] xax;
		delete [] yax;
		delete [] data;

		//The glazingintact array should be converted to bools
		GlazingIntact.resize(nval_GlazingIntactIn);
		for(int i=0; i<nval_GlazingIntactIn; i++){
			GlazingIntact.at(i) = GlazingIntactIn[i] == 1;
		}

		//Organize the emittance tables here
		epsilon_abs.init(4);
		epsilon_abs.addTable(&epsilon_abs_1);	//HCE #1
		epsilon_abs.addTable(&epsilon_abs_2);
		epsilon_abs.addTable(&epsilon_abs_3);
		epsilon_abs.addTable(&epsilon_abs_4);
		
		//Unit conversions
		theta_stow *= d2r;
		theta_dep *= d2r;
		T_startup += 273.15;
		T_loop_in_des += 273.15;
		T_loop_out += 273.15;
		T_field_ini += 273.15;
		T_fp += 273.15;
		T_amb_sf_des += 273.15;
		mc_bal_sca *= 3.6e3;	//[Wht/K-m] -> [J/K-m]
		

		/*--- Do any initialization calculations here ---- */
		//Allocate space for the loop simulation objects
		T_htf_in.resize(nMod);
		T_htf_out.resize(nMod); 
		T_htf_ave.resize(nMod); 
		q_loss.resize(nRecVar); 
		q_abs.resize(nRecVar); 
		c_htf.resize(nMod);
		rho_htf.resize(nMod);
		DP_tube.resize(nMod);
		E_abs_field.resize(nMod); 
		E_int_loop.resize(nMod); 
		E_accum.resize(nMod); 
		E_avail.resize(nMod); 
		E_abs_max.resize(nMod);
		v_1.resize(nMod);
		q_loss_SCAtot.resize(nMod); 
		q_abs_SCAtot.resize(nMod); 
		q_SCA.resize(nMod); 
		E_fp.resize_fill(nMod,0.); 
		q_1abs_tot.resize(nMod); 
		q_1abs.resize(nRecVar);
		ColOptEff.resize(nMod);

		//Allocate space for transient variables
		T_htf_in0.resize(nMod); 
		T_htf_out0.resize(nMod); 
		T_htf_ave0.resize(nMod); 
		
		//Set up annulus gas and absorber property matrices
		AnnulusGasMat.resize(nRecVar);
		AbsorberPropMat.resize(nRecVar);
		for(int j=0; j<nRecVar; j++){
			//Set up a matrix of annulus gas properties
			AnnulusGasMat.at(j) = new HTFProperties();
			AnnulusGasMat.at(j)->SetFluid( (int) AnnulusGas[j] );
			//Set up a matrix of absorber prop materials
			AbsorberPropMat.at(j) = new AbsorberProps();
			AbsorberPropMat.at(j)->setMaterial( (int) AbsorberMaterial[j] );
		}
		
		//Initialize values
		E_fp_tot = 0.;
		defocus_old = 0.;
		is_fieldgeom_init = false;

		//Set the defocus order to always be last->first in the loop
		SCADefocusArray.resize(nMod);
		for(int i=0; i<nMod; i++){
			SCADefocusArray[i] = nMod-i;
		}
		
		return 0;
	}

	bool init_fieldgeom(){

		/* 
		Call this method once when call() is first invoked. The calculations require location information that
		is provided by the weatherreader class and not set until after init() and before the first call().
		*/

		//Calculate the total field aperture area
		A_loop = (float)nMod * A_aperture;
		Ap_tot = (float)nLoops*A_loop;
		

		if(rec_model == 2){		//Evacuated tube receiver model
			//Calculate the cross-sectional flow area of the receiver piping
			D_h.resize(nRecVar);
			A_cs.resize(nRecVar);
			for(int i=0; i<nRecVar; i++){
			
				if((int)Flow_type[i] == 2){
					D_h.at(i) = D_abs_in[i] - D_plug[i];
				}
				else{
					D_h.at(i) = D_abs_in[i];
					D_plug[i] = 0.;
				}
				A_cs.at(i) =  pi * (D_abs_in[i]*D_abs_in[i] - D_plug[i]*D_plug[i]) / 4.;  //[m2] The cross-sectional flow area
			}
		}

		//Calculate header diameters here based on min/max velocities
		//output file with calculated header diameter "header_diam.out"
		nfsec = FieldConfig;  //MJW 1.12.11 allow the user to specify the number of field sections
		//Check to make sure the number of field sections is an even number
		if( nfsec%2 != 0){
			message(TCS_ERROR, "Number of field subsections must equal an even number");
			return false;
		}

		/*
		The number of header sections per field section is equal to the total number of loops divided
		by the number of distinct headers. Since two loops are connected to the same header section,
		the total number of header sections is then divided by 2.
		*/
		nhdrsec = (int)ceil(float(nLoops)/float(nfsec*2));

		//Allocate space for the D_hdr array
		D_hdr.resize_fill(nhdrsec, 0.);
		
		//We need to determine design information about the field for purposes of header sizing ONLY
		c_htf_ave = htfProps.Cp((T_loop_out+T_loop_in_des)/2.0)*1000.;    //Specific heat
		
		//Start by initializing sensitive variables
		double x1=0.0, loss_tot=0.0;
		opteff_des=0.0; 
		m_dot_design=0.0; 
		L_tot=(float)nMod*L_mod;
		
		//Determine the optical efficiency at design
		eta_opt_fixed = TrackingError*GeomEffects*reflectivity*Dirt_mirror*Error;
		//design point solar elevation
		double elev_des = asin(sin(0.4092793)*sin(latitude)+cos(latitude)*cos(0.4092793));
		//translate the solar angles into incidence angles
		double phi_t, theta_L, iam_t, iam_l;
		CSP::theta_trans(0., pi/2.-elev_des, ColAz, phi_t, theta_L);	//phi_t and theta_L are the translated angles (transverse and longitudinal)
		switch(opt_model)
		{
		case 1:		//Solar position table
			opteff_des = eta_opt_fixed*optical_table.interpolate(0., Pi/2.-elev_des);
			break;
		case 2:		//Collector incidence table
			opteff_des = eta_opt_fixed*optical_table.interpolate(0., theta_L);
			break;
		case 3:		//IAM polynomials
			iam_t = 0.;
			iam_l = 0.;
			for(int i=0; i<nval_IAM_L_coefs; i++)
				iam_l += IAM_L_coefs[i]*pow(theta_L, i);
			for(int i=0; i<nval_IAM_T_coefs; i++)
				iam_t += IAM_T_coefs[i]*pow(phi_t, i);
			opteff_des = eta_opt_fixed * iam_t * iam_l;
			break;
		default:
			message(TCS_ERROR, "The selected optical model (%d) does not exist. Options are 1=Solar position table : 2=Collector incidence table : 3= IAM polynomials",opt_model);
			return false;
		}

		//Determine the heat loss efficiency at design
		double dT_loc, c_hl, dTSCA, c_hl_w, hceopt;		
		switch(rec_model)
		{
		case 1:		//Polynomial model
			//evaluate the wind speed polynomial
			c_hl_w = 0.;
			for(int j=0; j<nval_HL_w_coefs; j++){
				c_hl_w += HL_w_coefs[j]*pow(V_wind_des, j);
			}

			//Assume a linear temperature rise across the field
			c_hl = 0.;
			dTSCA = (T_loop_out - T_loop_in_des)/(float)(nMod+1);
			for(int j=0; j<nMod; j++){
				dT_loc = T_loop_in_des + dTSCA*(0.5 + (float)j) - T_amb_sf_des;
				//evaluate the temperature polynomial
				for(int k=0; k<nval_HL_T_coefs; k++){
					c_hl += HL_T_coefs[k]*pow(dT_loc, k)*L_mod;		//Total receiver thermal loss [W/m] for a single loop
				}	
			}
			//Calculate the total thermal loss, including temperature and wind loss effects, for the entire loop
			loss_tot = c_hl_w * c_hl;

			break;
		case 2:		//Evacuated tube receiver model
			loss_tot = 0.;
			for(int j=0; j<nRecVar; j++)
				loss_tot += (float)nMod*L_mod*HCE_FieldFrac[j]*Design_loss[j];
			//correct for receiver optical losses
			hceopt = 0.;
			for(int i=0; i<nRecVar; i++){
				hceopt += alpha_abs[i]*Tau_envelope[i]*HCE_FieldFrac[i];
			}
			opteff_des *= hceopt;
			break;
		default:
			message(TCS_ERROR, "The selected thermal model (%d) does not exist. Options are 1=Regression model : 2=Evacuated tube receiver model",rec_model);
			return false;
		}


		//the estimated mass flow rate at design
		m_dot_design = (Ap_tot*I_bn_des*opteff_des - loss_tot*float(nLoops))/(c_htf_ave*(T_loop_out - T_loop_in_des));  //tn 4.25.11 using Ap_tot instead of A_loop. Change location of opteff_des
		//mjw 1.16.2011 Design field thermal power 
		q_design = m_dot_design * c_htf_ave * (T_loop_out - T_loop_in_des); //[Wt]
		//mjw 1.16.2011 Convert the thermal inertia terms here
		mc_bal_hot = mc_bal_hot * 3.6 * q_design;    //[J/K]
		mc_bal_cold = mc_bal_cold * 3.6 * q_design;  //[J/K]

	    //need to provide fluid density
		double rho_ave = htfProps.dens((T_loop_out+T_loop_in_des)/2.0,0.0); //kg/m3
		//Calculate the header design
		nrunsec = (int)floor(float(nfsec)/4.0)+1;  //The number of unique runner diameters
		D_runner.resize(nrunsec);
		L_runner.resize(nrunsec);
		D_hdr.resize(nhdrsec);
		
		header_design(nhdrsec, nfsec, nrunsec, rho_ave, V_hdr_max, V_hdr_min, m_dot_design, D_hdr, D_runner, &piping_summary);
		//if(ErrorFound()) return

		/* ----- Set initial storage values ------ */
		T_sys_c_last = T_field_ini;
		T_sys_h_last = T_field_ini;
		//cc--> Note that stored(3) -> Iter is no longer used in the TRNSYS code. It is omitted here.
		for(int i=0; i<nMod; i++){
			T_htf_in0[i] = T_field_ini;
			T_htf_out0[i] = T_field_ini;
			T_htf_ave0[i] = T_field_ini;
		}
		
		/*
		Do one-time calculations for system geometry. Calculate all HTF volume, set runner piping length
		Assume there are two field subsections per span, then if there's an even number of spans in the field, 
		we count the first header section as half-length. I.e., if a field looks like this:
		   (1)        (2)
		 |||||||   |||||||
		 -----------------
		 ||||||| : |||||||
				 :
				[P]
				 :
		 ||||||| : |||||||
		 -----------------
		 |||||||   |||||||
		   (3)        (4)
		Then the field has 4 subfields and two spans. The runner pipe (:) is half the distance between the two spans. 
		If the number of subfields were 6 (3 spans), the two runner pipe segments would both be equal to the full
		distance between spans.
		*/
		if(nfsec/2 % 2==1) {
			x1 = 2.;     //the first runners are normal
		}
		else{
			x1 = 1.;     //the first runners are short
		}
		L_runner[0] = 50.;  //Assume 50 [m] of runner piping in and around the power block before it heads out to the field in the main runners
		if(nrunsec > 1) {
			for(int i=1; i<nrunsec; i++){
				L_runner[i] = x1 * (2*L_crossover + (L_mod + L_mod_spacing)*float(nMod)/2.);
				x1 = 2.;   //tn 4.25.11 Default to 2 for subsequent runners
			}
		}
		double v_tofrom_sgs = 0.0;
		for(int i=0; i<nrunsec; i++){
			v_tofrom_sgs = v_tofrom_sgs + 2.*L_runner[i]*pi*pow(D_runner[i],2)/4.;  //This is the volume of the runner in 1 direction.
		}
    
		//6/14/12, TN: Multiplier for runner heat loss. In main section of code, are only calculating loss for one path.
		//Since there will be two symmetric paths (when nrunsec > 1), need to calculate multiplier for heat loss, considering
		//that the first 50 meters of runner is assumed shared.
		double lsum=0.;
		for(int i=0; i<nrunsec; i++){ lsum += L_runner[i]; }
		N_run_mult = 1.0 + (1.0 - 50.0/lsum);
    
		//Calculate the total HTF volume per loop based on user input. Select method based on heat loss model
		double v_loop_tot = 0.;
		switch(rec_model)
		{		
		case 1:		//Polynomial model
			v_loop_tot = A_loop * rec_htf_vol/1000.*(float)nLoops; //[m3]
			break;
		case 2:
			//-------piping from header into and out of the HCE's
			for(int j=0; j< nRecVar; j++){
				for(int i=0; i<nMod; i++){
					v_loop_tot += (L_mod + L_mod_spacing)*A_cs.at(j)*HCE_FieldFrac[j]*(float)nLoops;
				}
			}
			//mjw 1.13.2011 Add on volume for the crossover piping 
			//v_loop_tot = v_loop_tot + L_crossover*A_cs(SCAInfoArray(nMod/2,1),1)*float(nLoops)
			v_loop_tot += L_crossover*A_cs.at(0)*(float)nLoops;      //TN 6/20: need to solve for nMod = 1
			break;
		default:
			message(TCS_ERROR, "The selected thermal model (%d) does not exist. Options are 1=Regression model : 2=Evacuated tube receiver model",rec_model);
			return false;
		}
    
    
		//-------field header loop
		double v_header = 0.0;
		for(int i=0; i<nhdrsec; i++){
			//Also calculate the hot and cold header volume for later use. 4.25 is for header expansion bends
			v_header += D_hdr[i]*D_hdr[i]/4.*pi*(L_crossover+4.275)*float(nfsec)*2.0;  //tn 4.25.11 The header distance should be multiplied by 2 row spacings
		}
		//Add on inlet/outlet from the header to the loop. Assume header to loop inlet ~= 10 [m] (Kelley/Kearney)
		if(rec_model == 2) v_header = v_header + 20.*A_cs.at(0)*float(nLoops);
    
		//Calculate the HTF volume associated with pumps and the SGS
		double v_sgs = Pump_SGS(rho_ave,m_dot_design,solar_mult);
    
		//Calculate the hot and cold balance-of-plant volumes
		v_hot = v_header + v_tofrom_sgs;
		v_cold = v_hot;
    
		//Write the volume totals to the piping diameter file
		piping_summary.append(
		"\n----------------------------------------------\n"
		"Plant HTF volume information:\n"
		"----------------------------------------------\n");
		char tstr[500];
		string fmt = "Cold header pipe volume:   %10.4e m3\n"
				   "Hot header pipe volume:    %10.4e m3\n"
				   "Volume per loop:           %10.4e m3\n"
				   "Total volume in all loops: %10.4e m3\n"
				   "Total solar field volume:  %10.4e m3\n"
				   "Pump / SGS system volume:  %10.4e m3\n"
				   "---------------------------\n"
				   "Total plant HTF volume:    %10.4e m3\n";
		sprintf(tstr, fmt.c_str(), v_cold, v_hot, v_loop_tot/float(nLoops), v_loop_tot, (v_hot*2. + v_loop_tot), v_sgs, (v_hot*2. + v_loop_tot + v_sgs));
		piping_summary.append(tstr);
				
		//Include the pump/SGS volume with the header
		v_hot = v_hot + v_sgs/2.;
		v_cold = v_cold + v_sgs/2.;

		is_fieldgeom_init = true;	//The field geometry has been initialized. Make note.

		// Write Calculate Design Parameters
		value(PO_A_APER_TOT, Ap_tot);		//[m]

		return true;
	}

	virtual int call(double time, double /*step*/, int ncall){
		/* 
		-- Standard timestep call --
		
		*get inputs
		*do calculations
		*set outputs

		*/
		//reset defocus counter
		dfcount = 0;

		//record the start time
		if(start_time < 0){ start_time = current_time(); }

		//******************************************************************************************************************************
		//               Time-dependent conditions
		//******************************************************************************************************************************
		I_b = value(I_I_B);		//Direct normal incident solar irradiation [kJ/m2-hr]
		T_db = value(I_T_DB);		//Dry bulb air temperature [C]
		V_wind = value(I_V_WIND);		//Ambient windspeed  [m/s]
		P_amb = value(I_P_AMB);		//Ambient pressure [atm]
		T_dp = value(I_T_DP);		//The dewpoint temperature [C]
		T_cold_in = value(I_T_COLD_IN);		//HTF return temperature [C]
		m_dot_in = value(I_M_DOT_IN);		//HTF mass flow rate at the inlet  [kg/hr]
		defocus_new = value(I_DEFOCUS);		//Defocus control  [none]
		SolarAz = value(I_SOLARAZ);		//Solar azimuth angle  [deg]
		SolarZen = value(I_SOLARZEN);		//Solar zenith angle [deg]
		latitude = value(I_LATITUDE);		//Site latitude read from weather file [deg]
		longitude = value(I_LONGITUDE);		//Site longitude read from weather file [deg]
		timezone = value(I_TIMEZONE);		//Time zone [hr]

		shift = (longitude - timezone*15.)*d2r;

		//Unit conversions
		//I_b *= 1./3.6;
		T_db += 273.15;
		T_dp += 273.15;
		P_amb *= 101.325; //mbar -> Pa
		T_cold_in += 273.15;
		m_dot_in *= 1/3600.;
		SolarAz *= d2r;		//Note that the angle provided by the weather reader is 0..360 [deg] with 180 deg = South. Convert to 0=South, -90 East, 0/180=North.
		SolarAz += -pi;
		SolarZen *= d2r;
		latitude *= d2r;
		longitude *= d2r;

		//Initialize the field geometry if it hasn't been already
		if(! is_fieldgeom_init) 
			if(! init_fieldgeom()) return -1;

		// ******* Read in stored variables every timestep*******
		double hour, T_sky, rho_hdr_cold, rho_hdr_hot;
		double c_hdr_cold_last, m_dot_lower, m_dot_upper;
		bool upflag, lowflag, fp_lowflag, fp_upflag;
		double T_in_lower, y_fp_lower, T_in_upper, y_fp_upper;
		double y_upper, y_lower;
		double upmult, t_tol, err;
		double DP_IOCOP, DP_loop, m_dot_run_in, x3, m_dot_temp;
		double DP_toField, DP_fromField;
		double m_dot_header_in, m_dot_header, DP_hdr_cold, DP_hdr_hot;
		double E_avail_tot, rho_ave, E_int_sum;
		double q_abs_maxOT=0.;
		double dp_loop_tot = 0.;
		
		//If no change in inputs between calls, then no need to go through calculations.  Sometimes an extra call was run.....
		if (ncall>0) {
    		if( (! no_fp) && (T_cold_in_1 > T_cold_in) ){
				E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in);       //[kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
				goto set_outputs_and_return;
			}
    
		}
		//**********************************************************************************************************************

		



		//9-27-12, TWN: This model uses relative defocus. Changed controller to provide absolute defocus, so now convert to relative here
		defocus = defocus_new / defocus_old;
		defocus_old = defocus_new;

		if(ncall == 0){        //Always reset the defocus control at the begining of the timestep
			defocus_new = 1.;
			defocus_old = 1.;
			defocus = 1.0;
		}
		//calculate the hour of the day
		time_hr = time/3600.;
		dt_hr = dt/3600.;
		hour = fmod(time_hr,24.);       //hour of the day (1..24)  //tn 4.25.11 mod returns a natural number. This messes with the ftrack HrA/HrB calculations
		T_sky = CSP::skytemp(T_db,T_dp,hour);     //[K] Effective sky temperature 
		
		// *******Recalculate values that are time-dependent***********

		//First calculate the cold header temperature, which will serve as the loop inlet temperature
		rho_hdr_cold = htfProps.dens(T_sys_c_last, 1.);
		rho_hdr_hot = htfProps.dens(T_sys_h_last, 1.);
		c_hdr_cold_last = htfProps.Cp(T_sys_c_last)*1000.0; //mjw 1.6.2011 Adding mc_bal to the cold header inertia

		if (ncall==0) { //mjw 3.5.11 We only need to calculate these values once per timestep..
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

			//double  MidTrack;
			// Solar field operates
			if ((HrB > DepTime)  &&  (HrA < StwTime)) {
				// solar field deploys during time period
				if (HrA < DepTime) {
					ftrack = (HrB - DepTime) / dt_hr;
					//MidTrack = HrB - ftrack * 0.5 *dt_hr;

				// Solar field stows during time period
				}
				else if (HrB > StwTime) {
					ftrack = (StwTime - HrA) / dt_hr;
					//MidTrack = HrA + ftrack * 0.5 *dt_hr;
				// solar field operates during entire period
				} else {
					ftrack = 1.0;
					//MidTrack = HrA + 0.5 *dt_hr;
				}
			// solar field doesn't operate
			} else {
				ftrack = 0.0;
				//MidTrack = HrA + 0.5 *dt_hr;
			}

			//double StdTime = MidTrack;
			//double SolarTime = StdTime+((shift)*180.0/pi)/15.0+ EOT/60.0;
			// hour angle (arc of sun) in radians
			//double omega = (SolarTime - 12.0)*15.0*pi/180.0;
			// B. Stine equation for Solar Altitude angle in radians
			//SolarAlt = asin(sin(Dec)*sin(latitude)+cos(latitude)*cos(Dec)*cos(omega));
			
			if (SolarZen < pi/2.){
				//Convert the solar angles to collector incidence angles
				CSP::theta_trans(SolarAz, SolarZen, ColAz, phi_t, theta_L);
        
				switch(opt_model)
				{
				case 1:		//sun position
					//user provides an optical table as a function of solar position
					eta_optical = eta_opt_fixed*max( optical_table.interpolate(SolarAz, min(SolarZen, pi/2.)), 0.0);  
					break;
				case 2:		//incidence angle table
					//user provides an optical table as a function of collector incidence angles
					eta_optical = eta_opt_fixed*max( optical_table.interpolate(phi_t, max(theta_L, 0.0)), 0.0);
					break;
				case 3:		//incidence angle modifier polys
					//Otherwise, calculate the collector incidence angles for the IAM equations
						eta_optical = eta_opt_fixed * 
							CSP::poly_eval(phi_t, IAM_T_coefs, nval_IAM_T_coefs) * 
							CSP::poly_eval(theta_L, IAM_L_coefs, nval_IAM_L_coefs);
					break;
				default:
					//error
					message(TCS_ERROR, "No corresponding optical model. Error in solar angle calculation.");
					return -1;
				}
                				
				eta_optical *= ftrack;
			}
			else{
				eta_optical = 0.0;
				phi_t = pi/2.;
				theta_L = 0.0;
			}
			
			
			q_i = I_b*A_aperture/L_mod; //[W/m] The incoming solar irradiation per aperture length

			//Optical efficiency and incident power values for each SCA
			for(int j=0; j<nMod; j++){
				ColOptEff.at(j) = eta_optical;  
				q_SCA[j] = q_i;        //[W/m] The flux on the collector 
			}
			
		} else { 
			
			for(int i=0; i<nMod; i++) q_SCA[i] = q_i;        //The flux on the collector 
			
		}  //mjw 3.5.11 ---------- } of the items that only need to be calculated once per time step
		q_SCA_tot = 0.;
		for(int i=0; i<nMod; i++) q_SCA_tot += q_SCA[i];	//W/m
		
		//mode for solving the field 
		//1=constrain temperature
		//2=defocusing array
		//3=constrain mass flow above min
		SolveMode = 1;
		//MJW 12.14.2010 Only calculate the estimate on the first call of the timestep
		if(ncall==0) {
			sf_def = 1.;
			if(I_b > 25.) {
				//m_dot_htfX = max(min(10./950.*I_b,m_dot_htfmax),m_dot_htfmin);   //*defocus[kg/s] guess Heat transfer fluid mass flow rate through one loop
				m_dot_htfX = max(min( m_dot_design/(float)nLoops*I_b/I_bn_des, m_dot_htfmax), m_dot_htfmin);
			} else {
				m_dot_htfX = m_dot_htfmin;
			}
		} else {
			m_dot_htfX = max(min(m_dot_htfmax, m_dot_htfX),m_dot_htfmin);
		}

		//TWN 6/14/11  if defous is < 1 { calculate defocus from previous call's q_abs and { come back to temperature loop.
		if(defocus<1.) {
			//dfcount = dfcount +1;	//cc--> Not sure why this counts.. the dfcount variable is always set to 0 when the simulation is called.. Ask Ty.
			goto post_convergence_flag; // goto 11;
		}

overtemp_iter_flag: //10 continue     //Return loop for over-temp conditions
		dfcount ++;   //The defocus iteration counter. Starts at 1

		// ******* Initial values *******
		qq = 0;                  //Set iteration counter

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
		T_cold_in_1 = T_cold_in; //Inlet temperature may change due to new freeze protection model

		t_tol = 1.5e-3;          //Tolerance for T_loop_out

		//Decreasing the tolerance helps get out of repeating defocus iterations
		if(ncall>8){
			t_tol = 1.5e-4;
		}

		err = t_tol * 10.;       //Set beginning error > tolerance
		// ******************************************************************************************************************************
		//                   Iterative section
		// ******************************************************************************************************************************
		while ((fabs(err) > t_tol) && (qq < 30)){
		
			qq++; //Iteration counter
			q_loss_SCAtot.fill(0.); 
			q_abs_SCAtot.fill(0.);
			q_1abs_tot.fill(0.);
			E_avail.fill(0.);
			E_accum.fill(0.);
			E_int_loop.fill(0.);
			EqOptEff = 0.0;
			c_htf.fill(0.);
			rho_htf.fill(0.);
   
			m_dot_htf = m_dot_htfX;
			    
			m_dot_htf_tot = m_dot_htf*float(nLoops);
    
			T_sys_c  = (T_sys_c_last - T_cold_in_1)*exp(-(m_dot_htf*float(nLoops))/(v_cold*rho_hdr_cold+mc_bal_cold/c_hdr_cold_last)*dt) + T_cold_in_1;
			c_hdr_cold = htfProps.Cp(T_sys_c)*1000.0; //mjw 1.6.2011 Adding mc_bal to the cold header inertia
			//Consider heat loss from cold piping
			//Pipe_hl_cold = 0.0
			Header_hl_cold = 0.0;
			Runner_hl_cold = 0.0;
			//Header
			for(int i=0; i<nhdrsec; i++){
				Header_hl_cold = Header_hl_cold + L_crossover*D_hdr[i]*pi*Pipe_hl_coef*(T_sys_c - T_db);  //[W]
			}
			//Runner
			for(int i=0; i<nrunsec; i++){
				Runner_hl_cold = Runner_hl_cold + L_runner[i]*pi*D_runner[i]*Pipe_hl_coef*(T_sys_c - T_db);  //[W]
			}
			Pipe_hl_cold = Header_hl_cold + Runner_hl_cold;
        
			T_loop_in   = T_sys_c - Pipe_hl_cold/(m_dot_htf*float(nLoops)*c_hdr_cold);
			T_htf_in[0] = T_loop_in;
			
			//---------------------
			for(int i=0; i<nMod; i++){
				q_loss.fill(0.);
				q_abs.fill(0.);
				q_1abs.fill(0.);

				double dT_loc, m_node, T_node_ave, errhl;
				switch(rec_model)
				{
				case 2: //Evacuated receiver model
					for(int j=0; j<nRecVar; j++){
					            
						//Check to see if the field fraction for this HCE is zero.  if so, don't bother calculating for this variation
						if(HCE_FieldFrac[j]==0.0) continue;
            
						double c_htf_j, rho_htf_j;
						EvacReceiver(T_htf_in[i], m_dot_htf, T_db, T_sky, V_wind, P_amb, q_SCA, j, i, false,  ncall, time_hr,
									 //outputs
									 q_loss[j], q_abs[j], q_1abs[j], c_htf_j, rho_htf_j, mv_HCEguessargs);
					
						if(q_abs[j] != q_abs[j]) 
						{	//cc--> Check for NaN
							m_dot_htfX = m_dot_htfmax;
							if(dfcount > 20) {
								message(TCS_WARNING, "The solution encountered an unresolvable NaN error in the heat loss calculations. Retrying solar field calculations...");
								return 0;
							}
							goto overtemp_iter_flag;
						}

						//Keep a running sum of all of the absorbed/lost heat for each SCA in the loop
						q_abs_SCAtot[i] += q_abs[j]*L_mod*HCE_FieldFrac[j];
						q_loss_SCAtot[i] += q_loss[j]*L_mod*HCE_FieldFrac[j];
						q_1abs_tot[i] += q_1abs[j]*HCE_FieldFrac[j];  //losses in W/m from the absorber surface
						c_htf[i] += c_htf_j*HCE_FieldFrac[j];
						rho_htf[i] += rho_htf_j*HCE_FieldFrac[j];

						//keep track of the total equivalent optical efficiency
						EqOptEff += ColOptEff.at(i)*Shadowing[j]*dirt_env[j]*alpha_abs[j]*Tau_envelope[j]*(L_mod/L_tot)*HCE_FieldFrac[j];;
					}  //nRecVar loop

					//Calculate the specific heat for the node
					c_htf[i] *= 1000.0;
					//Calculate the mass of HTF associated with this node
					m_node = rho_htf[i]*A_cs.at(0)*L_mod;
					
					//MJW 12.14.2010 The first term should represent the difference between the previous average temperature and the new 
					//average temperature. Thus, the heat addition in the first term should be divided by 2 rather than include the whole magnitude
					//of the heat addition.
					//mjw & tn 5.1.11: There was an error in the assumption about average and outlet temperature      
					T_htf_out[i] = q_abs_SCAtot[i]/(m_dot_htf*c_htf[i]) + T_htf_in[i] + 
								   2.0 * (T_htf_ave0[i] - T_htf_in[i] - q_abs_SCAtot[i]/(2.0 * m_dot_htf * c_htf[i])) * 
								   exp(-2. * m_dot_htf * c_htf[i] * dt / (m_node * c_htf[i] + mc_bal_sca * L_mod));
					//Recalculate the average temperature for the SCA
					T_htf_ave[i] = (T_htf_in[i] + T_htf_out[i])/2.0;
			

					break;

				case 1:	//Polynomial model
					//If the average temperature hasn't been calculated yet, use the initial value
					if(ncall == 0){
						T_node_ave = T_htf_ave0[i];
					}
					else{
						T_node_ave = T_htf_ave[i];
					}
					//iterate to improve heat loss estimate
					errhl = 999.;
					while(fabs(errhl) > .1){
						dT_loc = T_node_ave - T_db;
						q_loss_SCAtot[i] = L_mod * CSP::poly_eval(dT_loc, HL_T_coefs, nval_HL_T_coefs) * CSP::poly_eval(V_wind, HL_w_coefs, nval_HL_w_coefs);	//W loss
						q_abs_SCAtot[i] = q_SCA[i] * L_mod * ColOptEff.at(i) - q_loss_SCAtot[i];
						c_htf[i] = htfProps.Cp(T_node_ave)*1000.;	//specific heat [J/kg-K]
						//Calculate the mass of HTF associated with this node
						rho_htf[i] = htfProps.dens(T_node_ave, 1.0);
						m_node = rec_htf_vol/1000.*A_aperture*rho_htf[i];	// [L/m2-ap]*[1 m3/1000 L]*[m2-ap]*[kg/m3] --> kg
						//update estimate of average temperature
						T_htf_out[i] = q_abs_SCAtot[i]/(m_dot_htf*c_htf[i]) + T_htf_in[i] + 
									   2.0 * (T_htf_ave0[i] - T_htf_in[i] - q_abs_SCAtot[i]/(2.0 * m_dot_htf * c_htf[i])) * 
									   exp(-2. * m_dot_htf * c_htf[i] * dt / (m_node * c_htf[i] + mc_bal_sca * L_mod));
						//Recalculate the average temperature for the SCA
						T_htf_ave[i] = (T_htf_in[i] + T_htf_out[i])/2.0;
						
						errhl = T_node_ave - T_htf_ave[i];	//iterate until the node temperature does not vary significantly
						
						T_node_ave = T_htf_ave[i];
					}
					
					q_1abs_tot[i] = q_loss_SCAtot[i]/L_mod;
					EqOptEff = eta_optical;	//Use the optical efficiency as it is for this option
					
					break;
				default:
					message(TCS_ERROR, "The selected thermal model (%d) does not exist. Options are 1=Regression model : 2=Evacuated tube receiver model",rec_model);
					return -1;
				}

				
				       
				//Calculate the actual amount of energy absorbed by the field that doesn't go into changing the SCA's average temperature
				//Include the thermal inertia term
				if(q_abs_SCAtot[i] > 0.0) {
					
					double x1 = (m_node*c_htf[i] + L_mod*mc_bal_sca);  
					E_accum[i] = x1*(T_htf_ave[i]- T_htf_ave0[i]);
					E_int_loop[i] = x1*(T_htf_ave[i] - 298.15);  //energy relative to ambient 
					E_avail[i] = max(q_abs_SCAtot[i]*dt - E_accum[i],0.0);      //[J/s]*[hr]*[s/hr]: [J]
            
				}

        
				//Set the inlet temperature of the next SCA equal to the outlet temperature of the current SCA
				//minus the heat losses in intermediate piping
				if(i < nMod-1) {
					//Determine the length between SCA's to use.  if halfway down the loop, use the row distance.
					double L_int;
					if(i==nMod/2-1) { 
						L_int = 2.+ L_crossover;
					} else {
						L_int = L_mod_spacing;
					}
            
					//Calculate inlet temperature of the next SCA
					T_htf_in[i+1] = T_htf_out[i] - Pipe_hl_coef*D_abs_out[0]*pi*L_int*(T_htf_out[i] - T_db)/(m_dot_htf*c_htf[i]);
					//Add the internal energy of the crossover piping
					E_int_loop[i] = E_int_loop[i] + L_int*(pow(D_abs_out[0],2)/4.*pi + mc_bal_sca/c_htf[i])*(T_htf_out[i] - 298.150);
				}

			}

			//Set the loop outlet temperature
			T_loop_outX = T_htf_out[nMod-1];

			//Calculation for heat losses from hot header and runner pipe
			Runner_hl_hot = 0.0;    //initialize
			Header_hl_hot = 0.0;   //initialize
			for(int i=0; i<nhdrsec; i++){
				Header_hl_hot = Header_hl_hot + L_crossover*D_hdr[i]*pi*Pipe_hl_coef*(T_loop_outX - T_db);
			}

			//Add the runner length
			for(int i=0; i<nrunsec; i++){
				Runner_hl_hot = Runner_hl_hot + L_runner[i]*pi*D_runner[i]*Pipe_hl_coef*(T_loop_outX - T_db);  //Wt
			}
			Pipe_hl_hot = Header_hl_hot + Runner_hl_hot;

			c_hdr_hot = htfProps.Cp(T_loop_outX)* 1000.;

			//Adjust the loop outlet temperature to account for thermal losses incurred in the hot header and the runner pipe
			T_sys_h = T_loop_outX - Pipe_hl_hot/(m_dot_htf_tot*c_hdr_hot);

			//Calculate the system temperature of the hot portion of the collector field. 
			//This will serve as the fluid outlet temperature
			T_sys_h = (T_sys_h_last - T_sys_h)*exp(-m_dot_htf_tot/(v_hot*rho_hdr_hot+mc_bal_hot/c_hdr_hot)*dt) + T_sys_h;

			    
freeze_prot_flag: //resume on freeze protection iteration
			    
			if(SolveMode==3) {  //Solve to find (increased) inlet temperature that keeps outlet temperature above freeze protection temp
    
				if((no_fp) && (T_sys_h > T_fp)) goto freeze_prot_ok; //goto 9
                
				no_fp   = false;
        
				err = (T_fp - T_sys_h)/T_fp;
        
				if(fabs(err) <= t_tol) goto freeze_prot_ok; //goto 9

				if((fp_lowflag) && (fp_upflag)){
					if(err > 0.0){      //Outlet too low, set lower limit of inlet temperature
						T_in_lower  = T_cold_in_1;
						y_fp_lower  = err;
					} else {                    //Outlet too high, set upper limit of inlet temperature
						T_in_upper  = T_cold_in_1;
						y_fp_upper  = err;
					}
						T_cold_in_1 = (y_fp_upper)/(y_fp_upper-y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
				} else {
        
					if(err > 0.0){      //Outlet too low, set lower limit of inlet temperature
						T_in_lower  = T_cold_in_1;
						y_fp_lower  = err;
						fp_lowflag  = true;
						upmult      = 0.50;
					} else {                    //Outlet too high, set upper limit of inlet temperature
						T_in_upper  = T_cold_in_1;
						y_fp_upper  = err;
						fp_upflag   = true;
						upmult      = 0.50;
					}
            
					if((fp_lowflag) && (fp_upflag)){    //if results of bracket are defined, use false position
						T_cold_in_1 = (y_fp_upper)/(y_fp_upper-y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper;
                
					} else {                            //if not, recalculate value based on approximate energy balance
						T_cold_in_1 = T_cold_in_1 + 40.0;   //Will always start low, so fp_lowflag = true so until fp_upflag = true { increase inlet temperature
                
					}
				}    
    
			} else {    //Solve to find mass flow that results in target outlet temperature
                 
				err = (T_loop_out - T_loop_outX)/T_loop_out;
        
				if(m_dot_htf==m_dot_htfmin){
					if(T_loop_outX < T_fp){         //freeze protection mode
						SolveMode = 3;       
						goto freeze_prot_flag; 
					}
					else if(err>0.0){
						goto freeze_prot_ok; //M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration    
					}
				}
        
				/* 
				  ****************************************************   
				  ***** Hybrid False Position Iteration Method********  
				  ****************************************************
				  Determine next guess for mass flow rate.  Want to use false position method, but it requires that the *results* (err in this case) at both ends of the bracket are known.  We have
				  defined a bracket but not the results.  Use the initial mass flow rate guess to get the results at one } of a new bracket.  { calculate a new mass flow rate based on the appoximation 
				  We eventually want a result for undefined result in the bracket.  After 2 iterations using approximation without defining bracket, switch to bisection.
				  Once results for both sides are { defined, "lowflag" and "upflag" will be true, and false position method will be applied.
				*/
				if((lowflag) && (upflag)){
					if(err > 0.0){
						m_dot_upper = m_dot_htf;
						y_upper     = err;
					} else {
						m_dot_lower = m_dot_htf;
						y_lower     = err;
					}
						m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				} else {
        
					if(err > 0.0){      //Prescribed is greater than calculated, so decrease mass flow, so set upper limit
						m_dot_upper = m_dot_htf;
						y_upper     = err;
						upflag      = true;
						upmult      = 0.50;
					} else {                    //Presribed is less than calculated, so increase mass flow, so set lower limit
						m_dot_lower = m_dot_htf;
						y_lower     = err;
						lowflag     = true;
						upmult      = 0.50;
					}
            
					if((lowflag) && (upflag)){  //if results of bracket are defined, use false position
						m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
					} else {                            //if not, recalculate value based on approximate energy balance
						if(qq<3){
							c_htf_ave = htfProps.Cp((T_loop_out+T_loop_in)/2.0)*1000.;    //Specific heat
							double qsum = 0.;
							for(int i=0; i<nMod; i++){qsum += q_abs_SCAtot[i]; }
							m_dot_htfX = qsum/(c_htf_ave*(T_loop_out - T_loop_in));
							m_dot_htfX = max( m_dot_htfmin, min( m_dot_htfX, m_dot_htfmax));							
						} else {
							m_dot_htfX = 0.5*m_dot_upper + 0.5*m_dot_lower;
						}
					}
				}

				// ***************************************************************************
				// ****** } Hyrbid False Position Iteration Method *************************
				// ***************************************************************************

				if(m_dot_lower > m_dot_htfmax){      //Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
					break;
				}

				if(m_dot_upper < m_dot_htfmin){      //Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
					m_dot_htfX = m_dot_htfmin;
					SolveMode = 3;
				}
    
			}
   
		}

freeze_prot_ok:		//continue on ok freeze protection check
		
		E_fp_tot = 0.0;
		if(! no_fp){
			E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in);       //[kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
		}

		if(qq>29) {
			message(TCS_ERROR, "Mass Flow Rate Convergence Error");
			return -1;
		}

		m_dot_htfX = m_dot_htf;              //for calls back to the temperature loop during the same run, this ensures that the first mass flow rate used will be the last mass flow rate

		// ******************************************************************
		// Special condition checks
		// ******************************************************************

		//After convergence, check to see if the HTF flow rate is over the maximum level

post_convergence_flag: //11 continue
		if(((m_dot_htf > m_dot_htfmax) && (dfcount<5)) || ((defocus<1.0) && (dfcount==1))){

			//if so, the field defocus control must be engaged. for(int this by calculating the maximum
			//amount of absorbed energy that corresponds to the maximum mass flow rate
    
			//Now determine the number of SCA's that must be defocused to meet this absorption level
			double qsum =0.;
			for(int i=0; i<nMod; i++){ qsum += q_abs_SCAtot[i]; }
			double q_check = max(qsum,0.0); //limit to positive
			//mjw 11.4.2010: the max Q amount is based on the defocus control    
			if(dfcount==1) q_abs_maxOT = min(q_check*defocus,c_htf_ave*m_dot_htfmax*(T_loop_out - T_loop_in_des));  //mjw 11.30.2010
    
			//Select the method of defocusing used for this system
			if(fthrctrl == 0){
				//Standard defocusing.. 1 SCA at a time completely defocuses
				int j;
				for(j=0; j<nMod; j++){
					//Incrementally subtract the losses, but limit to positive absorption values to avoid 
					//accounting for losses from previously defocused SCA's
					//q_check = q_check - max(q_abs_SCAtot(SCADefocusArray[j])-q_loss_SCAtot(SCADefocusArray[j]), 0.0)
					//4/9/11, TN: Don't need to subtract losses: see equation for q_check above
					q_check += - max(q_abs_SCAtot[(int)SCADefocusArray[j]-1], 0.0);
            
					if(q_check <= q_abs_maxOT) break;	//[TO DO] check that j doesn't increment 
				}
        
				//Reassign the flux on each SCA
				for(int i=0; i<j; i++){
					q_SCA[(int)SCADefocusArray[i]-1] = 0.0;
				}
			}
			else if(fthrctrl == 1){
				//Partial defocusing in the sequence specified by the SCADefocusArray
				int j;
				for(j=0; j<nMod; j++){
					//Incrementally subtract the losses, but limit to positive absorption values to avoid 
					//accounting for losses from previously defocused SCA's
					//q_check = q_check - min(max(q_abs_SCAtot(SCADefocusArray[j])-q_loss_SCAtot(SCADefocusArray[j]), 0.0),q_check-q_abs_maxOT)
					//4/9/11, TN: Don't need to subtract losses: see equation for q_check above
					q_check += -max(q_abs_SCAtot[(int)SCADefocusArray[j]-1], 0.0);
					if(q_check <= q_abs_maxOT) break;
				}
        
				//Reassign the flux on each SCA
				for(int i=0; i<j; i++){
					q_SCA[(int)SCADefocusArray[i]-1] = 0.0;
				}
				double tsum = 0.;
				for(int k=j+1; k<nMod; k++){ tsum += q_abs_SCAtot[(int)SCADefocusArray[k]-1]; }
				q_SCA[(int)SCADefocusArray[j]-1] *= (q_check-tsum)/q_abs_SCAtot[(int)SCADefocusArray[j]-1];
			}
			else if(fthrctrl == 2){
			
				//Partial defocusing of each SCA in the loop simultaneously. Specified defocus order is disregarded.
				for(int k=0; k<nMod; k++){
					q_SCA[k] *= min(q_abs_maxOT/max(q_check,1.e-6),1.0);  //MJW 7.23.2010::Limit fraction to 1
				}
			}

			if(q_SCA_tot>0.) {
				double tsum = 0.;
				for(int k=0; k<nMod; k++){ tsum += q_SCA[k]; }
				sf_def = min(tsum/q_SCA_tot,1.0);  //MJW 7.23.2010::Limit fraction to 1
			} else {
				sf_def = 1.;
			}
    
			SolveMode=2;  //Mode 2 -> defocusing
			//mjw 11.4.2010 added conditional statement
			if(dfcount<5) goto overtemp_iter_flag; //Return to recalculate with new defocus arrangement
		}
		//Calculate the amount of defocusing
		if(q_SCA_tot>0.) {
			double tsum = 0.;
			for(int k=0; k<nMod; k++){ tsum += q_SCA[k]; }
			sf_def = min(tsum/q_SCA_tot,1.0);  //MJW 7.23.2010::Limit fraction to 1
		} else {
			sf_def = 1.;
		}

		// ******************************************************************************************************************************
		// Calculate the pressure drop across the piping system
		// ******************************************************************************************************************************

		//handle loop pressure drop based on the heat loss model selection
		switch(rec_model)
		{
		case 1:		//Polynomial heat loss model
			/*
			This option doesn't require specific knowledge about absorber geometry, so we cannot calculate
			pressure drop from first principles. Instead use coefficients and scaling polynomials provided
			by the user.
			*/

			dp_loop_tot = (float)nMod*DP_nominal*CSP::poly_eval( m_dot_htf/m_dot_design, DP_coefs, nval_DP_coefs );

			break;
		case 2:		//Evacuated tube receiver model
			//------Inlet, Outlet, and COP
			DP_IOCOP = PressureDrop(m_dot_htf,(T_loop_in + T_loop_outX)/2.0,1.0,D_h.at(0),
											HDR_rough,(40.+L_crossover),0.0,0.0,2.0,0.0,0.0,2.0,0.0,0.0,2.0,1.0,0.0);
							//if(ErrorFound()) return 1
			//-------HCE's
			DP_tube.fill(0.);
			for(int j=0; j<nRecVar; j++){
				for(int i=0; i<nMod; i++){
				
					//Account for extra fittings on the first HCE
					double x1, x2;
					if(i==0) { 
						x1 = 10.0;
						x2 = 3.0;
					} else {
						x1 = 0.0;
						x2 = 1.0;
					}
					DP_tube[i] += PressureDrop(m_dot_htf,T_htf_ave[i],1.0,D_h.at(j),(Rough[j]*D_h.at(j)),
								 (L_mod+L_mod_spacing),0.0,0.0,x1,0.0,0.0,0.0,0.0,0.0,0.0,0.0,x2)*HCE_FieldFrac[j];
					//if(ErrorFound()) return 1
				}
			}
			//The pressure drop only across the loop
			DP_loop = 0.;
			for(int j=0; j<nMod; j++) 
				DP_loop += DP_tube[j];

			dp_loop_tot = DP_IOCOP + DP_loop;

			break;

		default:
			//error
			message(TCS_ERROR, "No such heat loss model. Error in loop pressure drop calculations.");
			return -1;
		}

		//-------SGS to field section
		m_dot_htf_tot = m_dot_htf*float(nLoops);
		m_dot_run_in = std::numeric_limits<double>::quiet_NaN();
		if(nfsec>2) {  //mjw 5.4.11 Correct the mass flow for situations where nfsec/2==odd
			m_dot_run_in = m_dot_htf_tot/2.0 * (1. - float(nfsec%4)/float(nfsec));
		} else {
			m_dot_run_in = m_dot_htf_tot/2.0; 
		}
		x3 = float(nrunsec) - 1.0;  //Number of contractions/expansions
		m_dot_temp = m_dot_run_in;
		DP_toField = 0.0;
		DP_fromField = 0.0;
		for(int i=0; i<nrunsec; i++){
			DP_toField += PressureDrop(m_dot_temp,T_loop_in,1.0,D_runner[i],HDR_rough,L_runner[i],0.0,x3,0.0,0.0,
							  max(float(CSP::nint(L_runner[i]/70.))*4.,8.),1.0,0.0,1.0,0.0,0.0,0.0);    //Correct for less than all mass flow passing through each section
			//if(ErrorFound()) return 1                  
			//-------SGS from field section
			DP_fromField += PressureDrop(m_dot_temp,T_loop_outX,1.0,D_runner[i],HDR_rough,L_runner[i],x3,0.0,0.0,0.0,
								 max(float(CSP::nint(L_runner[i]/70.))*4.,8.),1.0,0.0,0.0,0.0,0.0,0.0);   //Correct for less than all mass flow passing through each section
			//if(ErrorFound()) return 1
			if(i>1) m_dot_temp = max(m_dot_temp - 2.*m_dot_htf_tot/float(nfsec),0.0);
		}
		
		m_dot_header_in = m_dot_htf_tot/float(nfsec);
		m_dot_header = m_dot_header_in;
		DP_hdr_cold = 0.0;
		DP_hdr_hot = 0.0;
		for(int i=0; i<nhdrsec; i++){
			//Determine whether the particular section has an expansion valve
			double x2=0.0;
			if(i>0) {
				if(D_hdr[i] != D_hdr[i-1]) x2=1.;
			}
    
			//Calculate pressure drop in cold header and hot header sections.. both use similar information
			DP_hdr_cold = DP_hdr_cold + PressureDrop(m_dot_header,T_loop_in,1.0,D_hdr[i],HDR_rough,
							(L_crossover+4.275)*2.,0.0,x2,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0); //*m_dot_header/m_dot_header_in  //mjw/tn 1.25.12 already account for m_dot_header in function call //mjw 5.11.11 scale by mass flow passing though
			//if(ErrorFound()) return 1
			DP_hdr_hot =  DP_hdr_hot + PressureDrop(m_dot_header,T_loop_outX,1.0,D_hdr[i],HDR_rough,
							(L_crossover+4.275)*2.,x2,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0); //*m_dot_header/m_dot_header_in  //mjw 5.11.11
			//if(ErrorFound()) return 1
			//Siphon off header mass flow rate at each loop.  Multiply by 2 because there are 2 loops per hdr section
			m_dot_header = max(m_dot_header - 2.*m_dot_htf, 0.0);
			
		}


		//The total pressure drop in all of the piping
		DP_tot = dp_loop_tot + DP_hdr_cold + DP_hdr_hot + DP_fromField + DP_toField;

		//The total pumping power consumption
		W_dot_pump = DP_tot*m_dot_htf_tot/(rho_hdr_cold*eta_pump)/ 1000.;  //[kW]

		//The parasitic power consumed by electronics and SCA drives
		if(EqOptEff>0.0) {
			track_par_tot = SCA_drives_elec*sf_def*float(nMod*nLoops);
		} else {
			track_par_tot = 0.0;
		}


		// ******************************************************************************************************************************
		 


		// ******************************************************************
		// Calculate the system transient temperature for the next time step
		// ******************************************************************
		//First, calculate the amount of energy absorbed during the time step that didn't contribute to 
		//heating up the solar field
		E_avail_tot = 0.;
		E_loop_accum = 0.;
		for(int i=0; i<nMod; i++){
			E_avail_tot += E_avail[i];  //[J]
			E_loop_accum += E_accum[i]; //[J]
		}
		E_avail_tot *= float(nLoops) - E_fp_tot;
		E_loop_accum *= float(nLoops);
		//Calculate the HTF mass in the header, balance of field piping, piping to&from the steam generator (SGS)
		//The mass of HTF in the system will be calculated based on the design loop inlet temperature
		c_hdr_cold = htfProps.Cp(T_loop_in)* 1000.;
		c_hdr_hot = htfProps.Cp(T_loop_outX)* 1000.;

		//Half of the plant thermal mass must be heated up to the startup temperature (think hot header, hot half of heat
		//exchangers) and the other half must be heated up to the design loop inlet temperature
		//MJW 12.8.2010 modified startup temp calc to be based on previous system temperature
		//MJW 12.14.2010 Limit to positive to avoid step-to-step oscillation introduced by using previous step. 
		//.. This may cause a minor underestimation of annual energy output (<<.5%).
		E_hdr_accum = (v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - T_sys_h_last) + //Hot half
					  (v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - T_sys_c_last);   //cold half
		E_bal_startup = max(E_hdr_accum,0.0); //cold half

		//Calculate the total energy content of the solar field relative to a standard ambient temp. of 25[C]
		rho_ave = htfProps.dens((T_loop_outX+T_sys_c)/2.0, 0.0); //kg/m3
		c_htf_ave = htfProps.Cp( (T_sys_h + T_cold_in_1)/2.0)*1000.0;  //MJW 12.7.2010
		
		E_int_sum = 0.;
		for(int i=0; i<nMod; i++){E_int_sum += E_int_loop[i]; }
		
		E_field = ((v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - 298.150) +       //hot header and piping
				   (v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - 298.150) +   //cold header and piping
				   E_int_sum*float(nLoops));  //Field loops

		//Redefine pipe heat losses with header and runner components to get total system losses
		Pipe_hl_hot = N_run_mult*Runner_hl_hot + float(nfsec)*Header_hl_hot;
		Pipe_hl_cold = N_run_mult*Runner_hl_cold + float(nfsec)*Header_hl_cold;

		Pipe_hl = Pipe_hl_hot + Pipe_hl_cold;

		E_avail_tot = max(E_avail_tot - Pipe_hl*dt, 0.0);    //[J] 11/1/11 TN: Include hot and cold piping losses in available energy calculation

		E_avail_tot = max(E_avail_tot - E_bal_startup, 0.0);  //[J]

		// ******************************************************************
		// Calculate final output values
		// ******************************************************************
		DP_tot = DP_tot * 1.e-5; //[bar]
		//Calculate the thermal power produced by the field
		q_avail = E_avail_tot/(dt)*1.e-6;  //[MW]
		//Calculate the available mass flow of HTF
		m_dot_avail = max(q_avail*1.e6/(c_htf_ave*(T_sys_h - T_cold_in_1)),0.0); //[kg/s]
        m_dot_field_htf = m_dot_avail;
		if(T_sys_h < T_startup) {  //MJW 12.14.2010 Limit field production to above startup temps. Otherwise we get strange results during startup. Does this affect turbine startup?
			q_avail = 0.0;
			m_dot_avail = 0.0;
            m_dot_field_htf = m_dot_htf_tot;
		}

		//Dumped energy
		q_dump = Ap_tot*I_b*EqOptEff*(1.-defocus_new)/1.e6;  //MW

		//Total field performance
		q_field_delivered = m_dot_htf_tot * c_htf_ave * (T_sys_h - T_cold_in_1) / 1.e6; //MJW 1.11.11 [MWt]
		if(I_b == 0.){	//Adding case for zero output. Was reporting -Infinity in Fortran version
			eta_thermal = 0.;
		}
		else{
			eta_thermal = q_field_delivered / (I_b*Ap_tot / 1.e6);  //Total solar field thermal efficiency - by definition
		}
					
set_outputs_and_return:

		//---------Do unit conversions and final calculations-------------
		 
		double T_sys_h_out = T_sys_h - 273.15;
		double m_dot_avail_out = m_dot_avail*3600.;
        double m_dot_field_htf_out = m_dot_field_htf * 3600.;  //[kg/hr] from kg/s
		double W_dot_pump_out = W_dot_pump/1000.;
		double E_fp_tot_out = E_fp_tot*1.e-6;
		double T_sys_c_out = T_sys_c - 273.15;
		double EqOpteff_out = EqOptEff*ftrack;
		double m_dot_htf_tot_out = m_dot_htf_tot *3600.;
		double E_bal_startup_out = E_bal_startup/(dt*1.e6);
		q_inc_sf_tot = Ap_tot*I_b/1.e6;
		q_abs_tot = 0.;
		q_loss_tot = 0.;
		q_loss_spec_tot = 0.;
		for(int i=0; i<nMod; i++){
			q_abs_tot += q_abs_SCAtot[i]*1.e-6*float(nLoops);
			q_loss_tot += q_loss_SCAtot[i]*1.e-6*float(nLoops);
			q_loss_spec_tot += q_1abs_tot[i]/float(nMod);
		}
		double
			track_par_tot_out = track_par_tot * 1.e-6,
			Pipe_hl_out = Pipe_hl * 1.e-6;
		t_loop_outlet = T_loop_outX - 273.15;
		double 
			phi_t_out = phi_t*r2d,
			theta_L_out = theta_L*r2d;
		double
			E_loop_accum_out = E_loop_accum * 3.6e-9,
			E_hdr_accum_out = E_hdr_accum * 3.6e-9;
		E_tot_accum = E_loop_accum_out + E_hdr_accum_out;
		double
			E_field_out = E_field*3.6e-9;
		//------------------------------------------------------------------

		//Set outputs
		value(O_T_SYS_H, T_sys_h_out);		//[C] Solar field HTF outlet temperature
		value(O_M_DOT_AVAIL, m_dot_avail_out);		//[kg/hr] HTF mass flow rate from the field
        value(O_M_DOT_FIELD_HTF, m_dot_field_htf_out);  //[kg/hr] HTF mass flow rate from the field, including when recirculating
		value(O_Q_AVAIL, q_avail);		//[MWt] Thermal power produced by the field
		value(O_DP_TOT, DP_tot);		//[bar] Total HTF pressure drop
		value(O_W_DOT_PUMP, W_dot_pump_out);		//[MWe] Required solar field pumping power
		value(O_E_FP_TOT, E_fp_tot_out);		//[J] Freeze protection energy
		value(O_T_SYS_C, T_sys_c_out);		//[C] Collector inlet temperature
		value(O_ETA_OPTICAL, eta_optical);		//[none] Collector total optical efficiency
		value(O_EQOPTEFF, EqOpteff_out);		//[none] Total solar field optical efficiency - including receiver optical losses
		value(O_SF_DEF, sf_def);		//[none] The fraction of the solar field that's on focus
		value(O_M_DOT_HTF_TOT, m_dot_htf_tot_out);		//[kg/hr] The actual flow rate through the field..
		value(O_E_BAL_STARTUP, E_bal_startup_out);		//[MWt] Startup energy consumed
		value(O_Q_INC_SF_TOT, q_inc_sf_tot);		//[MWt] Total power incident on the field
		value(O_Q_ABS_TOT, q_abs_tot);		//[MWt] Total absorbed energy
		value(O_Q_LOSS_TOT, q_loss_tot);		//[MWt] Total receiver thermal and optical losses
		value(O_M_DOT_HTF, m_dot_htf);		//[kg/s] Flow rate in a single loop
		value(O_Q_LOSS_SPEC_TOT, q_loss_spec_tot);		//[W/m] Field-average receiver thermal losses (convection and radiation)
		value(O_TRACK_PAR_TOT, track_par_tot_out);		//[MWe] Parasitic electric power consumed by the tracking drives
		value(O_PIPE_HL, Pipe_hl_out);		//[MWt] Pipe heat loss in the hot header and the hot runner
		value(O_Q_DUMP, q_dump);		//[MWt] Dumped thermal energy
		value(O_PHI_T, phi_t_out);		//[deg] Solar incidence angle in the collector transversal plane
		value(O_THETA_L, theta_L_out);		//[deg] Solar incidence angle in the collector longitudinal plane
		value(O_T_LOOP_OUTLET, t_loop_outlet);		//[C] HTF temperature immediately subsequent to the loop outlet
		value(O_C_HTF_AVE, c_htf_ave);		//[J/kg-K] Average solar field specific heat
		value(O_Q_FIELD_DELIVERED, q_field_delivered);		//[MWt] Total solar field thermal power delivered
		value(O_ETA_THERMAL, eta_thermal);		//[none] Solar field thermal efficiency (power out/ANI)
		value(O_E_LOOP_ACCUM, E_loop_accum_out);		//[MWht] Accumulated internal energy change rate in the loops ONLY
		value(O_E_HDR_ACCUM, E_hdr_accum_out);		//[MWht] Accumulated internal energy change rate in the headers/SGS
		value(O_E_TOT_ACCUM, E_tot_accum);		//[MWht] Total accumulated internal energy change rate
		value(O_E_FIELD, E_field_out);		//[MWht] Accumulated internal energy in the entire solar field
		value_str(O_PIPING_SUMMARY, piping_summary);	//String containing description of field piping design
        value(O_DEFOCUS, defocus);          //[-] Relative defocus for passing back to the controller to ensure TCS convergence

		return 0;
	}

	virtual int converged(double /*time*/){
		/* 
		-- Post-convergence call --

		Update values that should be transferred to the next time step
		*/

		T_sys_c_last = T_sys_c;   //Get T_sys from the last timestep
		T_sys_h_last = T_sys_h;
		for(int i=0; i<nMod; i++){
			T_htf_in0[i] = T_htf_in[i];
			T_htf_out0[i] = T_htf_out[i];
			T_htf_ave0[i] = (T_htf_in0[i] + T_htf_out0[i])/2.0;
		}


		return 0;
	}

	// ------------------------------------------ supplemental methods -----------------------------------------------------------


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
	 9  | D_abs_in                  | Internal absorber tube diameter                         |                |
	 10 | D_abs_out                  | External absorber tube diameter                         |                |
	 11 | D_glass_in                  | Internal glass envelope diameter                        |                |
	 12 | D_glass_out                  | External glass envelope diameter                        |                |
	 13 | D_plug                  | (optional) Plug diameter                                |                |
	 14 | D_h                  | Absorber tube hydraulic diameter                        |                |
	 15 | eps_mode             | Interpolation mode for the emissivity (1=table,2=fixed) |                |
	 16 | xx                   | Array of temperature values for emissivity table        |                |
	 17 | yy                   | Array of emissivity values for table                    |                |
	 18 | nea                  | Number of entries in the emissivity table               |                |
	 19 | L_mod             | Length of the active receiver surface                   |                |
	 20 | single_point         | Logical flag - is the calculation for a single point?   |                |
	 21 | Epsilon_32           | Constant value for emissivity if table isn't used       |                |
	 22 | Epsilon_4            | Envelope inner surface emissivity                       |                |
	 23 | epsilon_glass            | Envelope outer surface emissivity                       |                |
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
		double D_abs_in, double D_abs_out, double D_glass_in, double D_glass_out, double D_plug, double D_h, int eps_mode, double xx[], double yy[], int nea,
		double L_mod,double single_point,  double Epsilon_32, double Epsilon_4, 
		double epsilon_glass, double Alpha_abs, double alpha_env, double ColOptEff, double Tau_envelope, double P_a, int Flow_type, 
		int AbsorberMaterial, int annulusGas, bool glazingIntact, int Fluid, int ncall, double time,
		//outputs
		double q_heatloss, double q_12conv, double q_34tot, double c_1ave, double rho_1ave)
		*/
	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double T_sky, double v_6, double P_6, double q_i, 
		int hv /* HCE variant [0..3] */, int sca_num, bool single_point,  int ncall, double time,
		//outputs
		double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave, std::vector<double> & v_reguess_args)
	{

		//cc -- note that collector/hce geometry is part of the parent class. Only the indices specifying the
		//		number of the HCE and collector need to be passed here.

		//---Variable declarations------
		bool reguess;
		double T_2, T_3, T_4, T_5, T_6, T_7,v_1, k_23, q_34conv, q_34rad, h_34conv, h_34rad, q_23cond, 
			k_45, q_45cond, q_56conv, h_56conv, q_57rad, q_3SolAbs, q_5solabs, q_cond_bracket, R_45cond,
			T_save[5], T_2g, cp_1, T3_tol, q5_tol, T1_tol, T2_tol, Diff_T3, diff_q5, T_lower, T_upper, 
			q_5out, T_1_out, diff_T1, T_1_ave, T_1_out1, diff_T2, eps_3, q_in_W, T_upper_max, y_upper,
			y_lower, upmult, q5_tol_1, T3_upper, T3_lower, y_T3_upper, y_T3_lower, abs_diffT3;

		bool UPFLAG, LOWFLAG, T3upflag, T3lowflag, is_e_table;
		int qq, q5_iter, T1_iter, q_conv_iter;

		double T_save_tot, colopteff_tot;
		//cc--> note that xx and yy have size 'nea'
		
		//---Re-guess criteria:---
		if(time<=2) goto lab_reguess;
		
		if(((int)v_reguess_args[0] == 1) != GlazingIntact.at(hv)) goto lab_reguess;	//glazingintact state has changed
		
		if(P_a[hv] != v_reguess_args[1]) goto lab_reguess;                   //Reguess for different annulus pressure
		
		if(fabs(v_reguess_args[2]-T_1_in) > 50.) goto lab_reguess;
		
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
			if(GlazingIntact.at(hv)) {
				T_save[0] = T_1_in;
				T_save[1] = T_1_in+2.;
				T_save[2] = T_save[1] + 5.;
				if(P_a[hv] > 1.0){          //Set guess values for different annulus pressures
					T_save[3] = T_save[2] - 0.5*(T_save[2]-T_amb);       //If higher pressure, guess higher T4   
					T_upper_max = T_save[2] - 0.2*(T_save[2]-T_amb);     //Also, high upper limit for T4
				}
				else{
					T_save[3] = T_save[2] - 0.9*(T_save[2]-T_amb);       //If lower pressure, guess lower T4
					T_upper_max = T_save[2] - 0.5*(T_save[2]-T_amb);     //Also, low upper limit for T4
				}                      
				T_save[4] = T_save[3] - 2.;

				v_reguess_args[1]  = P_a[hv];               //Reset previous pressure
				v_reguess_args[0] = GlazingIntact.at(hv) ? 1. : 0.;   //Reset previous glazing logic
				v_reguess_args[2] = T_1_in;            //Reset previous T_1_in

			}
			else{
				T_save[0] = T_1_in;
				T_save[1] = T_1_in+2.;
				T_save[2] = T_save[1] + 5.;
				T_save[3] = T_amb;
				T_save[4] = T_amb;

				v_reguess_args[0] = GlazingIntact.at(hv) ? 1. : 0.;   //Reset previous glazing logic
				v_reguess_args[1] = T_1_in;            //Reset previous T_1_in

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
		R_45cond = log(D_glass_out[hv]/D_glass_in[hv])/(2.*pi*k_45);    //[K-m/W]Equation of thermal resistance for conduction through a cylinder

		colopteff_tot = ColOptEff.at(sca_num)*dirt_env[hv]*Shadowing[hv];	//The total optical efficiency

		if(GlazingIntact.at(hv)){   //These calculations (q_3SolAbs,q_5solAbs) are not dependent on temperature, so only need to be computed once per call to subroutine
    
			q_3SolAbs = q_i * colopteff_tot * Tau_envelope[hv] * alpha_abs[hv];  //[W/m]  
			//We must account for the radiation absorbed as it passes through the envelope
			q_5solabs = q_i * colopteff_tot * alpha_env[hv];   //[W/m]  
		}
		else{
			//Calculate the absorbed energy 
			q_3SolAbs = q_i * colopteff_tot * alpha_abs[hv];  //[W/m]  
			//No envelope
			q_5solabs = 0.0;                            //[W/m]

		}
		
		is_e_table = false;
		if(epsilon_abs.getTableSize(hv) < 2){
			eps_3 = epsilon_abs.getSingleValue(hv);
		}
		else{
			eps_3 = epsilon_abs.interpolate(hv, T_3-273.15);          //Set epsilon value for case that eps_mode = 1.  Will reset inside temp loop if eps_mode > 1.
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
						if( Diff_T3 > 0. )
							T_3 = T_3 - 50.0;
						else
							T_3 = T_3 + 50.0;
						//T_3 = max(T_7, T_3 - abs_diffT3);         //Note that recalculating T_3 using this exact equation, rather than T_3 = T_3 - frac*diff_T3 was found to solve in fewer iterations
					}
				}
			}
            
			T3_adjust = T_3 - T3_prev_qq;

			//Calculate temperature sensitive emissivity using T_3, if required
			if(is_e_table) eps_3 = epsilon_abs.interpolate(hv, (T_3-273.15)); //call interp((T_3-273.15),eps_mode,xx,yy,eps3old,eps_3)

			//Separate GlazingIntact = true and GlazingIntact = false  If true, T4 must be solved, if false then T4 is explicitly known (or doesn't exist, depending on how you want to look at it)
			//Solving for correct T4 as it relates to current T3 value
			if (GlazingIntact.at(hv)) {
    
				//**********************************************
				//************* SET UP T_4 ITERATION **********************
				//**********************************************
        
				// if(qq==1){               //If first iteration, set T_4 bounds to phyiscal limits defined by T_3 and T_sky
				// 	T_lower = T_sky;         //Lowest possible temperature of T_4 is sky temp        
				// 	T_upper = max(T_upper_max,T_amb);    //Highest possible temperature is the highest temperature on either side of T_4: either T_3 or ambient
				// 	q5_tol_1= 0.001;           //Just get T4 in the ball park.  '20' may not be the optimum value.....
				// } 
				// else {                                            //For additional iterations:
				// 	T_lower = T_lower - max(abs_diffT3,0.0);       //If diff_T3 is + then new T3 < old T3 so adjust lower limit
				// 	T_upper = T_upper + fabs(min(abs_diffT3,0.0));  //If diff_T3 is (-) then new T3 > old T3 so adjust upper limit
				// 	q5_tol_1= q5_tol;        //For remaining T3 iterations, use specified tolerance (note that 2 iterations for T3 are gauranteed)                   
				// }
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
					else	// T3_adjust negative
					{
						T_lower = max(T_sky, T_lower + 1.25*T3_adjust);
						T_upper = T_4;
						T_4 = T_4 + 0.5*T3_adjust;
					}
				}
				q5_tol_1 = q5_tol;

				//if( T_4 > T_upper || T_4 < T_lower )
				//	T_4 = 0.5*(T_upper + T_lower);
        
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
					FQ_34CONV(T_3, T_4, P_6, v_6, T_6, hv, q_34conv, h_34conv);
            
					//The radiative heat exchange between the absorber and the envelope
					//    Units         ( K ,  K ,  m ,  m , K  ,    -     ,    -     ,   logical    ,  W/m   , W/m2-K)
					FQ_34RAD(T_3, T_4, T_7, eps_3, hv, q_34rad, h_34rad);
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
					FQ_56CONV(T_5, T_6, P_6, v_6, hv, q_56conv, h_56conv); //[W/m]
					q_57rad = epsilon_glass[hv] * 5.67e-8 * (pow(T_5,4) - pow(T_7,4));
					q_5out = q_57rad + q_56conv;     //[W/m]

					//***************************************************************************
					//********** Compare q_5out with q_45 cond***********************************
					//***************************************************************************
					diff_q5 = (q_5out - q_45cond)/q_45cond;     //[W/m]
            
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
				FQ_34CONV(T_3, T_4, P_6, v_6, T_6, hv, q_34conv, h_34conv);
				//The radiative heat exchange between the absorber and the envelope
				FQ_34RAD(T_3, T_4, T_7, eps_3, hv, q_34rad, h_34rad);
				//The total heat exchange between absorber and envelope
				q_34tot = q_34conv + q_34rad;    //[W/m]
        
			}      //Know heat transfer from outer surface of receiver tube to ambient
    
			//Bracket Losses
			//Bracket conduction losses apply 
			q_cond_bracket = FQ_COND_BRACKET(T_3, T_6, P_6, v_6); //[W/m] 

			q_12conv = q_3SolAbs - (q_34tot+q_cond_bracket);         //[W/m] Energy transfer to/from fluid based on energy balance at T_3
    
			q_in_W  = q_12conv * L_mod;                           //Convert [W/m] to [W] for some calculations
    
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
    
			rho_1ave    = htfProps.dens(T_1_ave,0.0);       //[kg/m^3] Density
			v_1         = m_dot/(rho_1ave*A_cs.at(hv));             //HTF bulk velocity
    
			q_conv_iter = 0;                 //Set iteration counter
			diff_T2 = 1.0 + T2_tol;         //Set diff > tolerance
    
			bool T2upflag = false;
			bool T2lowflag = false;

			double y_T2_low = std::numeric_limits<double>::quiet_NaN();
			double y_T2_up = std::numeric_limits<double>::quiet_NaN();

			double T2_low = min(T_1_ave,T_3);
			double T2_up = max(T_1_ave, T_3);

			//Ensure convective calculations are correct (converge on T_2)
			while( (fabs(diff_T2)>T2_tol) && (q_conv_iter<100)){
 
				q_conv_iter ++;       //Increase iteration counter

				T_2 = fT_2(q_12conv, T_1_ave, T_2g, v_1, hv);	//Calculate T_2 (with previous T_2 as input)
				diff_T2 = (T_2 - T_2g)/T_2;          //T_2 difference
				
				if(diff_T2 > 0.0)		// Calculated > Guessed, set lower limit and increase guessed
				{
					T2_low = T_2g;
					T2lowflag = true;
					y_T2_low = diff_T2;
					if( T2upflag )
						T_2g = y_T2_up / (y_T2_up - y_T2_low)*(T2_low - T2_up) + T2_up;
					else
						T_2g = T2_up;
				}
				else					// Calculated < Guessed, set upper limit and decrease guessed
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
				
				//T_2g = T_2 - 0.5*(T_2-T_2g);         //Reset T_2
        
				// if(qq<2){        //For first T3 iteration, do not iterate on T_2 (again, this control is based on observation of solve time and may not be optimal for all simulations)
				// 	break;
				// }
 
			}
    
			//The conductive heat transfer equals the convective heat transfer (energy balance)
			q_23cond = q_12conv;         //[W/m]
    
			//Calculate tube conductivity 
			k_23 = FK_23(T_2, T_3, hv);       //[W/m-K]  

			//Update the absorber surface temperature (T_3) according to new heat transfer rate
			abs_diffT3 = T_3 - (T_2 + q_23cond*log(D_abs_out[hv]/D_abs_in[hv])/(2.*pi*k_23));
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
 
		q_heatloss = q_34tot + q_cond_bracket + q_5solabs;   //[W/m]

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
			D_plug/D_abs_in	Nu#
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

	double fT_2(double q_12conv, double T_1, double T_2g, double v_1, int hv){
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
		k_1 = max(htfProps.cond(T_1),1.e-4);  //[W/m-K]
		k_2 = max(htfProps.cond(T_2g),1.e-4);  //[W/m-K]
		rho_1 = htfProps.dens(T_1, 0.0);  //[kg/m^3]

		Pr_2 = (Cp_2 * mu_2) / k_2;
		Pr_1 = (Cp_1 * mu_1) / k_1;

		if(v_1 > 0.1) {

			Re_D2 = (rho_1 * D_h.at(hv) * v_1) / (mu_1);

			// Nusselt Number for laminar flow case if option to include laminar flow model is chosen 
			if (( includelaminar == true) && (Re_D2 <= 2300.)) {
				if(Flow_type[hv] == 2.0) {
					DRatio = D_plug[hv]/D_abs_in[hv];
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

			h_1 = Nu_D2 * k_1 / D_h.at(hv);  //[W/m**2-K]
			return T_1 + q_12conv/(h_1*D_abs_in[hv]*pi);
			//q_12conv = h_1 * D_abs_in * PI  * (T_2 - T_1ave)  //[W/m]
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
	//subroutine FQ_34CONV(T_3,T_4, D_abs_out, D_glass_in, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34)
	void FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hv, double &q_34conv, double &h_34){
	   //      UNITS   ( K , K ,  Pa , m/s,  K , -, -, W/m, W/m2-K)
	
	double a, Alpha_34, b, Beta_34, C, C1, Cp_34, Cv_34, Delta, Gamma, k_34, Lambda, 
			  m, mu_34, n, nu_34, P, Pr_34, P_A1, Ra_D3, Ra_D4, rho_34, T_34, T_36, 
			  grav, Nu_bar, rho_3, rho_6, mu_36, rho_36, cp_36,
			  k_36, nu_36, alpha_36, beta_36, Pr_36, h_36, mu_3, mu_6, k_3, k_6, cp_3, Cp_6, nu_6, nu_3,
			  Alpha_3, alpha_6, Re_D3, Pr_3, Pr_6, Natq_34conv, Kineticq_34conv;
	
	grav = 9.81; //m/s2  gravitation constant

	P_A1 = P_a[hv] * 133.322368;  //convert("torr", "Pa")  //[Pa]

		T_34 = (T_3 + T_4) / 2.;  //[C]
		T_36 = (T_3 + T_6) / 2.;  //[C]

		if(!GlazingIntact.at(hv) ) {
		
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
				Ra_D3 = grav * beta_36 * fabs(T_3 - T_6) * pow(D_abs_out[hv],3) / (alpha_36 * nu_36);

				// Warning Statement if following Nusselt Number correlation is used out of recommended range //
				//If ((Ra_D3 <= 1.e-5) || (Ra_D3 >= 1.e12)) continue
					//CALL WARNING('The result may not be accurate, since 10**(-5) < Ra_D3 < 10**12 does not hold. See Function fq_34conv. Ra_D3 = XXXA1', Ra_D3)

				// Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder //
				Pr_36 = nu_36 / alpha_36;
				Nu_bar = pow(0.60 + (0.387 * pow(Ra_D3,0.1667)) / pow(1. + pow(0.559 / Pr_36,0.5625), 0.2963) , 2);
				h_36 = Nu_bar * k_36 / D_abs_out[hv];  //[W/m**2-K]//
				q_34conv = h_36 * pi * D_abs_out[hv] * (T_3 - T_6);  //[W/m]//
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
				Re_D3 = v_6 * D_abs_out[hv] / nu_6;
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
				h_36 = Nu_bar  *  k_6  /  D_abs_out[hv];  //[W/m**2-K]
				q_34conv =  h_36  *  D_abs_out[hv]  *  pi  *  (T_3 - T_6);  //[W/m]	
				h_34 = h_36;  //set output coefficient
			}
		}
		else {

			// Thermophysical Properties for gas in annulus space 
			mu_34 = AnnulusGasMat.at(hv)->visc(T_34);  //[kg/m-s] 
			Cp_34 = AnnulusGasMat.at(hv)->Cp(T_34)*1000.;  //[J/kg-K]
			Cv_34 = AnnulusGasMat.at(hv)->Cv(T_34)*1000.;  //[J/kg-K]
			rho_34 = AnnulusGasMat.at(hv)->dens(T_34, P_A1);  //[kg/m**3]
			k_34 = AnnulusGasMat.at(hv)->cond(T_34);  //[W/m-K]

			// Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders 
			Alpha_34 = k_34 /(Cp_34 * rho_34);  //[m**2/s]//
			nu_34 = mu_34 / rho_34;  //[m**2/s]//
			Beta_34 = 1. / max(T_34,1.0);  //[1/K]//
			Ra_D3 = grav * Beta_34 * fabs(T_3 - T_4) * pow(D_abs_out[hv],3) / (Alpha_34 * nu_34);
			Ra_D4 = grav * Beta_34 * fabs(T_3 - T_4) * pow(D_glass_in[hv],3) / (Alpha_34 * nu_34);
			Pr_34 = nu_34 / Alpha_34;
			Natq_34conv = 2.425 * k_34 * (T_3 - T_4) / pow(1 + pow(D_abs_out[hv]/ D_glass_in[hv], 0.6), 1.25) * pow(Pr_34 * Ra_D3 / (0.861 + Pr_34),0.25);  //[W/m]//	
			P = P_a[hv];  //[mmHg] (note that 1 torr = 1 mmHg by definition)
			C1 = 2.331e-20;  //[mmHg-cm**3/K]//

			// Free-molecular heat transfer for an annular space between horizontal cylinders 
			if (AnnulusGasMat.at(hv)->GetFluid() == HTFProperties::Air) { //AIR
				Delta = 3.53e-8;  //[cm]
			}

			if (AnnulusGasMat.at(hv)->GetFluid() == HTFProperties::Hydrogen_ideal){ //H2
				Delta = 2.4e-8;  //[cm]
			}

			if (AnnulusGasMat.at(hv)->GetFluid() == HTFProperties::Argon_ideal){  //Argon
				Delta = 3.8e-8;  //[cm]
			}

			Lambda = C1 * T_34 / (P * Delta*Delta);  //[cm]
			Gamma = Cp_34 / Cv_34;
			a = 1.;
			b = (2. - a) / a * (9. * Gamma - 5.) / (2. * (Gamma + 1.));
			h_34 = k_34 / (D_abs_out[hv] / 2. * log(D_glass_in[hv] / D_abs_out[hv]) + b * Lambda /100.* (D_abs_out[hv] / D_glass_in[hv] + 1.));  //[W/m**2-K]
			Kineticq_34conv  = D_abs_out[hv] * pi * h_34 * (T_3 - T_4);  //[W/m]

			// Following compares free-molecular heat transfer with natural convection heat transfer and uses the largest value for heat transfer in annulus 
			if (Kineticq_34conv > Natq_34conv) {
				q_34conv = Kineticq_34conv;  //[W/m]
			}
			else{
				q_34conv = Natq_34conv;  //[W/m]
				h_34 = q_34conv/(D_abs_out[hv]*pi*(T_3-T_4));  //Recalculate the convection coefficient for natural convection
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

				q' = sigma * PI * D_1 * (T_1^4 - T_2^4) / (1 / EPSILON_1 + (1 - EPSILON_2) / EPSILON_2 * (D_1 / D_abs_in))

				Where,

					q' = radiation heat transfer per unit length [W/m]
					sigma = Stephan-Boltzmann constant [W/m^2-K^4]
					T_1 = absorber outer surface temperature [K]
					T_2 = glazing inner surface temperature [K]
					D_1 = outer absorber diameter [m]
					D_abs_in = inner glazing diameter [m]
					EPSILON_1 = emissivity of inner surface
					EPSILON_2 = emissivity of outer surface

			Case 2: Small convex object in a large cavity.

				q' = sigma * PI * D_1 * EPSILON_1 * (T_1^4 - T_2^4)
	}*/

	void FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_abs_v, int hv, double &q_34rad, double &h_34){
		//units		(K, K, K, -, -, -, W/m, W/m2-K)
	double sigma=5.67e-8, T_ave;
	T_ave = (T_3 + T_4)/2.;
		if (! GlazingIntact.at(hv)) {
			q_34rad = epsilon_abs_v * pi * D_abs_out[hv]  * sigma * (pow(T_3, 4) - pow(T_7, 4));  //[W/m]
			h_34 = q_34rad/(pi*D_abs_out[hv]*(T_3 - T_7));
		}
		else {
			h_34 = sigma*(T_3*T_3 + T_4*T_4)*(T_3 + T_4)/ (1.0 / epsilon_abs_v + D_abs_out[hv] / D_glass_in[hv] * ( 1.0 / epsilon_glass[hv] - 1.0)) ;
			q_34rad = pi* D_abs_out[hv] * h_34 * (T_3 - T_4);
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

	void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hv, double &q_56conv, double &h_6)
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
		if (! GlazingIntact.at(hv)) {
			q_56conv = (T_5 - T_6);  //[W/m]
		}
		else{
			if (v_6 <= 0.1) {

				// Coefficients for Churchill and Chu natural convection correlation //
				nu_56 = mu_56 / rho_56;  //[m^2/s]
				alpha_56 = k_56 / (Cp_56 * rho_56 );  //[m^2/s]
				beta_56 =  1.0 / T_56;  //[1/K]
				Ra_D5 = g *beta_56 * fabs(T_5 - T_6) * pow(D_glass_out[hv],3) / (alpha_56 * nu_56);

				// Warning Statement if following Nusselt Number correlation is used out of range //
				//If (Ra_D5 <= 10**(-5)) or (Ra_D5 >= 10**12) Then CALL WARNING('The result may not be accurate, 
				//since 10**(-5) < Ra_D5 < 10**12 does not hold. See Function fq_56conv. Ra_D5 = XXXA1', Ra_D5)

				// Churchill and Chu correlation for natural convection for a horizontal cylinder //
				Pr_56 = nu_56 / alpha_56;
				Nu_bar = pow(0.60 + (0.387 * pow(Ra_D5, 0.1667)) / pow(1.0 + pow(0.559 / Pr_56, 0.5625), 0.2963) , 2);
				h_6 = Nu_bar * k_56 / D_glass_out[hv];  //[W/m**2-K]
				q_56conv = h_6 * pi * D_glass_out[hv] * (T_5 - T_6);  //[W/m]
			}
			else {

				// Coefficients for Zhukauskas's correlation //
				alpha_5 = k_5 / (Cp_5 * rho_5);  //[m**2/s]
				alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s]
				nu_5 = mu_5 / rho_5;  //[m**2/s]
				nu_6 = mu_6 / rho_6;  //[m**2/s]
				Pr_5 = nu_5 / alpha_5;
				Pr_6 = nu_6 / alpha_6;
				Re_D5 = v_6 * D_glass_out[hv] * rho_6 / mu_6;

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
				h_6 = Nus_6 * k_6 / D_glass_out[hv];  //[W/m**2-K]
				q_56conv = h_6 * pi * D_glass_out[hv] * (T_5 - T_6);  //[W/m]
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
	double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6){
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

			Alpha_brac = k_brac / (Cp_brac * rho_brac * 1000.0);  //[m**2/s]
			alpha_6 = k_6 / (Cp_6 * rho_6 * 1000.0);  //[m**2/s]
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
								 TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror,dirt_env, Error, ColOptEff)
	
	implicit none

	integer,intent(in):: OptCharType, CollectorType
	real(8),intent(inout):: reflectivity, shadowing, trackingError, GeomEffects, Rho_mirror_clean, &
							Dirt_mirror, dirt_env, Error
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
			dirt_env = min(1.0,dirt_env)
			Error = min(1.0,Error)
		}

		If (CollectorType == 2) then    //'LS-2'
			Shadowing = 0.974
			TrackingError = 0.994
			GeomEffects = 0.98
			Rho_mirror_clean = 0.935
			Dirt_mirror = min(1.0,reflectivity/Rho_mirror_clean)
			dirt_env = min(1.0,(1.0+ Dirt_mirror)/2.0)
			Error = 0.96
		}

 		If ((CollectorType == 3) || (CollectorType == 4)) then    //'LS-3' or 'IST'
			Shadowing = 0.974
			TrackingError = 0.994
			GeomEffects = 0.98
			Rho_mirror_clean = 0.935
			Dirt_mirror = min(1.0,reflectivity/Rho_mirror_clean)
			dirt_env = min(1.0,(1.0+ Dirt_mirror)/2.0)
			Error = 0.96
		}
	
		ColOptEff = Shadowing * TrackingError * GeomEffects * Rho_mirror_clean * Dirt_mirror * dirt_env * Error * K

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

	double FK_23(double T_2, double T_3, int hv)
	{
		double T_23;

		//Absorber materials:
		// (1)   304L
		// (2)   216L
		// (3)   321H
		// (4)   B42 Copper Pipe

		T_23 = (T_2 + T_3) / 2. - 273.15;  //[C]
		return AbsorberPropMat.at(hv)->cond(T_23);
		
	}

	/***************************************************************************************************
	*********************************************************************
	* PipeFlow_turbulent:                                               *
	* This procedure calculates the average Nusselt number and friction *
	* factor for turbulent flow in a pipe given Reynolds number (Re),   *
	* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
	* the relative roughness}                                           *
	*********************************************************************
	*/
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
  
	};

	/*
	***************************************************************************************************
	 Trough system piping loss model
	***************************************************************************************************

	 This piping loss model is derived from the pressure drop calculations presented in the 
	 following document:

	   Parabolic Trough Solar System Piping Model

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
	  1 | Fluid        | Number associated with fluid type     | none      | float
	  2 | m_dot        | Mass flow rate of the fluid           | kg/s      | float
	  3 | T            | Fluid temperature                     | K         | float
	  4 | P            | Fluid pressure                        | Pa        | float
	  5 | D            | Diameter of the contact surface       | m         | float
	  6 | Rough        | Pipe roughness                        | m         | float
	  7 | L_pipe       | Length of pipe for pressure drop      | m         | float
	  8 | Nexp         | Number of expansions                  | none      | float
	  9 | Ncon         | Number of contractions                | none      | float
	 10 | Nels         | Number of standard elbows             | none      | float
	 11 | Nelm         | Number of medium elbows               | none      | float
	 12 | Nell         | Number of long elbows                 | none      | float
	 13 | Ngav         | Number of gate valves                 | none      | float
	 14 | Nglv         | Number of globe valves                | none      | float
	 15 | Nchv         | Number of check valves                | none      | float
	 16 | Nlw          | Number of loop weldolets              | none      | float
	 17 | Nlcv         | Number of loop control valves         | none      | float
	 18 | Nbja         | Number of ball joint assemblies       | none      | float
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

	
	/**************************************************************************************************
	---------------------------------------------------------------------------------
	--Inputs
	   * nhsec - [-] number of header sections
	   * nfsec - [-] number of field section
	   * nrunsec- [-] number of unique runner diameter sections
	   * rho   - [kg/m3] Fluid density
	   * V_max - [m/s] Maximum fluid velocity at design
	   * V_min - [m/s] Minimum fluid velocity at design
	   * m_dot - [kg/s] Mass flow rate at design
	--Outputs
	   * D_hdr - [m] An ARRAY containing the header diameter for each loop section
	   * D_runner - [m] An ARRAY containing the diameter of the runner pipe sections
	   * summary - Address of string variable on which summary contents will be written.
	---------------------------------------------------------------------------------			*/

	void header_design(int nhsec, int nfsec, int nrunsec, double rho, double V_max, double V_min, double m_dot, 
		util::matrix_t<double> &D_hdr, util::matrix_t<double> &D_runner, std::string *summary = NULL){
	
		//resize the header matrices if they are incorrect
		//real(8),intent(out):: D_hdr(nhsec), D_runner(nrunsec)
		if((int)D_hdr.ncells() != nhsec) D_hdr.resize(nhsec);
		if((int)D_runner.ncells() != nrunsec) D_runner.resize(nrunsec);

		//----
		int nst,nend, nd;
		double m_dot_max, m_dot_min, m_dot_ts, m_dot_hdr, m_dot_2loops, m_dot_temp;
		
		for(int i=0; i<nhsec; i++){ D_hdr[i] = 0.; }

		//mass flow to section is always half of total
		m_dot_ts = m_dot/2.;
		//Mass flow into 1 header
		m_dot_hdr = 2.*m_dot_ts/(float(nfsec));
		//Mass flow into the 2 loops attached to a single header section
		m_dot_2loops = m_dot_hdr/float(nhsec);

		//Runner diameters
		//runner pipe needs some length to go from the power block to the headers
		D_runner.at(0) = pipe_sched(sqrt(4.*m_dot_ts/(rho*V_max*pi)));
		//other runner diameters
		m_dot_temp = m_dot_ts*(1.-float(nfsec%4)/float(nfsec));  //mjw 5.4.11 Fix mass flow rate for nfsec/2==odd 
		if(nrunsec>1) {
			for(int i=1; i<nrunsec; i++){
				D_runner[i] = pipe_sched(sqrt(4.*m_dot_temp/(rho*V_max*pi)));
				m_dot_temp = max(m_dot_temp - m_dot_hdr*2, 0.0);
			}
		}

		//Calculate each section in the header
		nst=0; nend = 0; nd = 0;
		m_dot_max = m_dot_hdr;
		for(int i=0; i<nhsec; i++){
			if((i==nst)&&(nd <= 10)) {
				//If we've reached the point where a diameter adjustment must be made...
				//Also, limit the number of diameter reductions to 10
        
				nd++; //keep track of the total number of diameter sections
				//Calculate header diameter based on max velocity
				D_hdr[i]=pipe_sched(sqrt(4.*m_dot_max/(rho*V_max*pi)));
				//Determine the mass flow corresponding to the minimum velocity at design
				m_dot_min = rho*V_min*pi*D_hdr[i]*D_hdr[i]/4.;
				//Determine the loop after which the current diameter calculation will no longer apply
				nend = (int)floor((m_dot_hdr-m_dot_min)/(m_dot_2loops));  //tn 4.12.11 ceiling->floor
				//The starting loop for the next diameter section starts after the calculated ending loop
				nst = nend;
				//Adjust the maximum required flow rate for the next diameter section based on the previous 
				//section's outlet conditions
				m_dot_max = max(m_dot_hdr - m_dot_2loops*float(nend),0.0);
			}
			else{
				//If we haven't yet reached the point where the minimum flow condition is acheived, just
				//set the header diameter for this loop to be equal to the last diameter
				D_hdr[i] = D_hdr.at(i-1);
			}
		}
		
		//Print the results to a string
		if(summary != NULL){
			summary->clear();
			char tstr[200];
			//Write runner diam
			sprintf(tstr, "Piping geometry file\n\nMaximum fluid velocity: %.2f\nMinimum fluid velocity: %.2f\n\n", V_max, V_min);
			summary->append(tstr);

			for(int i=0; i<nrunsec; i++){
				sprintf(tstr, "To section %d header pipe diameter: %.4f m (%.2f in)\n",i+1, D_runner[i], D_runner[i]*mtoinch);
				summary->append(tstr);
			}
			//Write header diams
			sprintf(tstr, "Loop No. | Diameter [m] | Diameter [in] | Diam. ID\n--------------------------------------------------\n");
			summary->append(tstr);

			nd=1;
			for(int i=0; i<nhsec; i++){
				if(i>1) {
					if(D_hdr[i] != D_hdr.at(i-1)) nd=nd+1;
				}
				sprintf(tstr, "  %4d   |    %6.4f    |    %6.4f     | %3d\n", i+1, D_hdr[i], D_hdr[i]*mtoinch, nd);
				summary->append(tstr);
			}
			//110 format(2X,I4,3X,"|",4X,F6.4,4X,"|",4X,F6.3,5X,"|",1X,I3)
		}

	}
	
	/***************************************************************************************************
	This function takes a piping diameter "De" [m] and locates the appropriate pipe schedule
	from a list of common pipe sizes. The function always returns the pipe schedule equal to or
	immediately larger than the ideal diameter De.
	The pipe sizes are selected based on the assumption of a maximum hoop stress of 105 MPa and a total
	solar field pressure drop of 20 Bar. The sizes correspond to the pipe schedule with a wall thickness
	sufficient to match these conditions. For very large pipe diameters (above 42in), no suitable schedule 
	was found, so the largest available schedule is applied.
	Data and stress calculations were obtained from Kelly & Kearney piping model, rev. 1/2011.
	*/

	double pipe_sched(double De) {

		int np=25;
		
		//D_inch = (/2.50, 3.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0, &
		//           28.0, 30.0, 32.0, 34.0, 36.0, 42.0, 48.0, 54.0, 60.0, 66.0, 72.0/)

		double D_m[] = {0.06880860, 0.08468360, 0.1082040, 0.16146780, 0.2063750, 0.260350, 0.311150, 0.33975040, 
						0.39055040, 0.438150, 0.488950, 0.53340, 0.58420, 0.6350, 0.679450, 0.730250, 0.781050, 
						0.82864960, 0.87630, 1.02870, 1.16840, 1.32080, 1.47320, 1.62560, 1.7780};

		//Select the smallest pipe schedule above the diameter provided
		for(int i=0; i<np; i++){		
			if(D_m[i] >= De) return D_m[i];
		}
		//Nothing was found, so return an error
		message(TCS_WARNING, "No suitable pipe schedule found for this plant design. Looking for a schedule above %.2f in. "
			"Maximum schedule is %.2f in. Using the exact pipe diameter instead.", De*mtoinch, D_m[np-1]*mtoinch);
		return std::numeric_limits<double>::quiet_NaN();
	}

	//***************************************************************************************************
	double Pump_SGS(double rho, double m_dotsf, double sm){
	
		int nl = 8;
		double v_dotpb, v_dotsf, m_dotpb, vel_max;
		double
			*V_dot = new double[nl],
			*D = new double[nl],
			*V = new double[nl];

		//Line no.	
		//1	Expansion vessel or thermal storage tank to pump suction header
		//2	Individual pump suction line, from suction header to pump inlet
		//3	Individual pump discharge line, from pump discharge to discharge header
		//4	Pump discharge header
		//5	Collector field outlet header to expansion vessel or thermal storage tank
		//6	Steam generator supply header
		//7	Inter steam generator piping
		//8	Steam generator exit header to expansion vessel or thermal storage
		//Assume standard lengths for each line [m] (Kelly & Kearney)
		//Assume 3 pumps at 50% each. #3) 3*30. 
		double L_line[] = {0.0, 0.0, 90.0, 100.0, 120.0, 80.0, 120.0, 80.0};

		//Assume a maximum HTF velocity of 1.85 m/s (based on average from Kelly & Kearney model
		vel_max = 1.85;

		//design-point vol. flow rate m3/s
		m_dotpb = m_dotsf/sm;
		v_dotpb = m_dotpb/rho;
		v_dotsf = m_dotsf/rho;

		//Set the volumetric flow rate for each line.
		V_dot[0] = v_dotsf;
		V_dot[1] = v_dotsf/2.0;
		V_dot[2] = V_dot[1];
		V_dot[3] = v_dotsf;
		V_dot[4] = V_dot[3];
		V_dot[5] = v_dotpb;
		V_dot[6] = V_dot[5];
		V_dot[7] = V_dot[5];
		
		//for each line..
		double psum=0.;
		for(int i=0; i<nl; i++){
			//Calculate the pipe diameter
			D[i] = pipe_sched(sqrt(4.0*V_dot[i]/(vel_max*pi)));
			//Calculate the total volume
			V[i] = pow(D[i],2)/4.*pi*L_line[i];
			psum += V[i];
		}

		delete [] V_dot;
		delete [] D;
		delete [] V;

		return psum;

	}

	//***************************************************************************************************


};

TCS_IMPLEMENT_TYPE( sam_mw_lf_type262, "Molten salt Linear Fresnel solar field model", "Mike Wagner", 1, sam_mw_lf_type262_variables, NULL, 1 );
