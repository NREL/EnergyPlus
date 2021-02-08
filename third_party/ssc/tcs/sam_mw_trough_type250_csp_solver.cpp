#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
#include "sam_csp_util.h"

#include <cmath>

#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_util.h"
#include "csp_solver_core.h"

using namespace std;

enum{
	// Site parameters
	P_LATITUDE,
	P_LONGITUDE,
	P_SHIFT,

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
	P_T_STARTUP,
	P_PB_RATED_CAP,
	P_M_DOT_HTFMIN,
	P_M_DOT_HTFMAX,
	P_T_LOOP_IN_DES,
	P_T_LOOP_OUT,
	P_FLUID,
	//P_T_FIELD_INI,
	P_FIELD_FL_PROPS,
	P_T_FP,
	P_I_BN_DES,
	P_V_HDR_MAX,
	P_V_HDR_MIN,
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

	//P_OPTCHARTYPE,
	//P_COLLECTORTYPE,
	P_W_APERTURE,
	P_A_APERTURE,
	//P_IAMF0,
	//P_IAMF1,
	//P_IAMF2,
	//P_REFLECTIVITY,
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

	O_T_SYS_H,
	O_M_DOT_AVAIL,
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

	//Include N_max
	N_MAX
};


tcsvarinfo sam_mw_trough_type250_variables[] = {
	// vartype,		      datatype,		            index,				       name,		                                                                             label,          units,           meta,          group,  default_value
	{ TCS_INPUT,          TCS_NUMBER,          P_LATITUDE,               "latitude",                                                    "Site latitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         P_LONGITUDE,              "longitude",                                                   "Site longitude read from weather file",          "deg",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             P_SHIFT,                  "shift",                                         "shift in longitude from local standard meridian",          "deg",             "",             "",             "" },

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
	{ TCS_PARAM,          TCS_NUMBER,         P_T_STARTUP,              "T_startup",        "The required temperature of the system before the power block can be switched on",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PB_RATED_CAP,           "pb_rated_cap",                                                                    "Rated plant capacity",          "MWe",             "",             "",          "111" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMIN,           "m_dot_htfmin",                                                              "Minimum loop HTF flow rate",         "kg/s",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_M_DOT_HTFMAX,           "m_dot_htfmax",                                                              "Maximum loop HTF flow rate",         "kg/s",             "",             "",           "12" },
	{ TCS_PARAM,          TCS_NUMBER,     P_T_LOOP_IN_DES,          "T_loop_in_des",                                                           "Design loop inlet temperature",            "C",             "",             "",          "293" },
	{ TCS_PARAM,          TCS_NUMBER,        P_T_LOOP_OUT,             "T_loop_out",                                                          "Target loop outlet temperature",            "C",             "",             "",          "391" },
	{ TCS_PARAM,          TCS_NUMBER,             P_FLUID,                  "Fluid",                                                                  "Field HTF fluid number",         "none",             "",             "",           "21" },
	//{ TCS_PARAM,          TCS_NUMBER,       P_T_FIELD_INI,            "T_field_ini",                                                               "Initial field temperature",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_MATRIX,    P_FIELD_FL_PROPS,         "field_fl_props",                                                                     "Fluid property data",         "none","7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",             "",             "" },
	{ TCS_PARAM,          TCS_NUMBER,              P_T_FP,                   "T_fp",                       "Freeze protection temperature (heat trace activation temperature)",            "C",             "",             "",          "150" },
	{ TCS_PARAM,          TCS_NUMBER,          P_I_BN_DES,               "I_bn_des",                                                             "Solar irradiation at design",         "W/m2",             "",             "",          "950" },
	{ TCS_PARAM,          TCS_NUMBER,         P_V_HDR_MAX,              "V_hdr_max",                                            "Maximum HTF velocity in the header at design",          "m/s",             "",             "",            "3" },
	{ TCS_PARAM,          TCS_NUMBER,         P_V_HDR_MIN,              "V_hdr_min",                                            "Minimum HTF velocity in the header at design",          "m/s",             "",             "",            "2" },
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

	//{ TCS_PARAM,           TCS_ARRAY,       P_OPTCHARTYPE,            "OptCharType",                                                    "The optical characterization method ",         "none",             "",             "",      "1,1,1,1" },
	//{ TCS_PARAM,           TCS_ARRAY,     P_COLLECTORTYPE,          "CollectorType",                                                "{1=user defined, 2=LS-2, 3=LS-3, 4=IST} ",         "none",             "",             "",      "1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,        P_W_APERTURE,             "W_aperture",               "The collector aperture width (Total structural area.. used for shadowing)",            "m",             "",             "",      "5,5,5,5" },
	{ TCS_PARAM,           TCS_ARRAY,        P_A_APERTURE,             "A_aperture",                                               "Reflective aperture area of the collector",           "m2",             "",             "","470.3,470.3,470.3,470.3" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF0,                  "IamF0",                                                  "Incident angle modifier 0th order term",         "none",             "",             "",      "1,1,1,1" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF1,                  "IamF1",                                                  "Incident angle modifier 1st order term",         "none",             "",             "","0.0506,0.0506,0.0506,0.0506" },
	//{ TCS_PARAM,           TCS_ARRAY,             P_IAMF2,                  "IamF2",                                                  "Incident angle modifier 2nd order term",         "none",             "",             "","-0.1763,-0.1763,-0.1763,-0.1763" },
	//{ TCS_PARAM,           TCS_ARRAY,      P_REFLECTIVITY,           "reflectivity",                                          "Base solar-weighted mirror reflectivity value ",         "none",             "",             "",      "1,1,1,1" },
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
	{ TCS_PARAM,          TCS_MATRIX,             P_ROUGH,               "Rough",                                                      "Roughness of the internal surface ",            "m",             "",             "","[4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05][4.50E-05,4.50E-05,4.50E-05,4.50E-05]" },
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

	{ TCS_PARAM,          TCS_MATRIX,      P_SCAINFOARRAY,        "SCAInfoArray",                       "(:,1) = HCE type, (:,2)= Collector type for each SCA in the loop ",         "none",             "",             "","[1,1][1,1][1,1][1,1][1,1][1,1][1,1][1,1]" },
	{ TCS_PARAM,           TCS_ARRAY,   P_SCADEFOCUSARRAY,        "SCADefocusArray",                                            "Order in which the SCA's should be defocused",         "none",             "",             "","8,7,6,5,4,3,2,1" },

	// Field design calculations
	{ TCS_PARAM,          TCS_NUMBER,     PO_A_APER_TOT,             "A_aper_tot",                                          "Total solar field aperture area",                           "m^2",             "",             "",             "-1.23" },

	{ TCS_INPUT,          TCS_NUMBER,               I_I_B,                    "I_b",                                                "Direct normal incident solar irradiation",        "W/m^2",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DB,                   "T_db",                                                                "Dry bulb air temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,            I_V_WIND,                 "V_wind",                                                                      "Ambient windspeed ",          "m/s",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_P_AMB,                  "P_amb",                                                                        "Ambient pressure",         "mbar",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DP,                   "T_dp",                                                                "The dewpoint temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_T_COLD_IN,              "T_cold_in",                                                                  "HTF return temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,          I_M_DOT_IN,               "m_dot_in",                                                        "HTF mass flow rate at the inlet ",        "kg/hr",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_DEFOCUS,                "defocus",                                                                        "Defocus control ",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,           I_SOLARAZ,                "SolarAz",                                 "Solar azimuth angle reported by the Type15 weather file",          "deg",             "",             "",             "" },
	
	{ TCS_OUTPUT,          TCS_NUMBER,           O_T_SYS_H,                "T_sys_h",                                                      "Solar field HTF outlet temperature",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_M_DOT_AVAIL,            "m_dot_avail",                                                       "HTF mass flow rate from the field",        "kg/hr",             "",             "",             "" },
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

	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};


class sam_mw_trough_type250 : public tcstypeinterface
{
private:
	C_csp_trough_collector_receiver ms_trough;
	C_csp_weatherreader::S_outputs ms_weather;
	C_csp_solver_htf_1state ms_htf_state_in;
	C_csp_collector_receiver::S_csp_cr_inputs ms_inputs;
	C_csp_collector_receiver::S_csp_cr_out_solver ms_out_solver;
	//C_csp_collector_receiver::S_csp_cr_out_report ms_out_report;
	C_csp_solver_sim_info ms_sim_info;

public:

	sam_mw_trough_type250( tcscontext *cxt, tcstypeinfo *ti ) 
		: tcstypeinterface(cxt, ti)
	{
	}

	virtual ~sam_mw_trough_type250()
	{
	}

	void set_matrix_t(size_t idx, util::matrix_t<double> &class_matrix)
	{
		int n_rows = -1;
		int n_cols = -1;
		double *data = value(idx, &n_rows, &n_cols);
		class_matrix.resize(n_rows, n_cols);
		for( int r = 0; r < n_rows; r++ )
			for( int c = 0; c < n_cols; c++ )
				class_matrix(r, c) = TCS_MATRIX_INDEX(var(idx), r, c);
	}

	void set_matrix_t(size_t idx, util::matrix_t<int> &class_matrix)
	{
		int n_rows = -1;
		int n_cols = -1;
		double *data = value(idx, &n_rows, &n_cols);
		class_matrix.resize(n_rows, n_cols);
		for( int r = 0; r < n_rows; r++ )
			for( int c = 0; c < n_cols; c++ )
				class_matrix(r, c) = (int) TCS_MATRIX_INDEX(var(idx), r, c);
	}

	virtual int init(){
		/*
		--Initialization call-- 
		
		Do any setup required here.
		Get the values of the inputs and parameters
		*/

		// Set fluid properties
		ms_trough.m_Fluid = (int)value(P_FLUID);		//[-]
		int n_rows = 0;
		int n_cols = 0;
		double *field_fl_props = value(P_FIELD_FL_PROPS, &n_rows, &n_cols);
		ms_trough.m_field_fl_props.resize(n_rows, n_cols);
		for( int r = 0; r < n_rows; r++ )
			for( int c = 0; c < n_cols; c++ )
				ms_trough.m_field_fl_props(r, c) = TCS_MATRIX_INDEX(var(P_FIELD_FL_PROPS), r, c);
		// **********************************************************

		ms_trough.m_nSCA = (int)value(P_NSCA);			//Number of SCA's in a loop [none]
		ms_trough.m_nHCEt = (int)value(P_NHCET);		//Number of HCE types [none]
		ms_trough.m_nColt = (int)value(P_NCOLT);		//Number of collector types [none]
		ms_trough.m_nHCEVar = (int)value(P_NHCEVAR);	//Number of HCE variants per type [none]
		ms_trough.m_nLoops = (int)value(P_NLOOPS);		//Number of loops in the field [none]
		ms_trough.m_eta_pump = value(P_ETA_PUMP);		//HTF pump efficiency [none]
		ms_trough.m_HDR_rough = value(P_HDR_ROUGH);		//Header pipe roughness [m]
		ms_trough.m_theta_stow = value(P_THETA_STOW);		//stow angle [deg]
		ms_trough.m_theta_dep = value(P_THETA_DEP);			//deploy angle [deg]
		ms_trough.m_Row_Distance = value(P_ROW_DISTANCE);	//Spacing between rows (centerline to centerline) [m]
		ms_trough.m_FieldConfig = (int)value(P_FIELDCONFIG);		//Number of subfield headers [none]
		ms_trough.m_T_startup = value(P_T_STARTUP);			//The required temperature of the system before the power block can be switched on [C]
		ms_trough.m_m_dot_htfmin = value(P_M_DOT_HTFMIN);	//Minimum loop HTF flow rate [kg/s]
		ms_trough.m_m_dot_htfmax = value(P_M_DOT_HTFMAX);	//Maximum loop HTF flow rate [kg/s]
		ms_trough.m_T_loop_in_des = value(P_T_LOOP_IN_DES);	//Design loop inlet temperature [C]
		ms_trough.m_T_loop_out_des = value(P_T_LOOP_OUT);		//Target loop outlet temperature [C]
		ms_trough.m_Fluid = (int)value(P_FLUID);			//Field HTF fluid number [none]
		
		ms_trough.m_T_fp = value(P_T_FP);					//Freeze protection temperature (heat trace activation temperature) [C]
		ms_trough.m_I_bn_des = value(P_I_BN_DES);			//Solar irradiation at design [W/m2]
		ms_trough.m_V_hdr_max = value(P_V_HDR_MAX);			//Maximum HTF velocity in the header at design [m/s]
		ms_trough.m_V_hdr_min = value(P_V_HDR_MIN);			//Minimum HTF velocity in the header at design [m/s]
		ms_trough.m_Pipe_hl_coef = value(P_PIPE_HL_COEF);	//Loss coefficient from the header, runner pipe, and non-HCE piping [W/m2-K]
		ms_trough.m_SCA_drives_elec = value(P_SCA_DRIVES_ELEC);		//Tracking power, in Watts per SCA drive [W/SCA]
		ms_trough.m_fthrok = (int)value(P_FTHROK);			//Flag to allow partial defocusing of the collectors [none]
		ms_trough.m_fthrctrl = (int)value(P_FTHRCTRL);		//Defocusing strategy [none]
		ms_trough.m_ColTilt = value(P_COLTILT);				//Collector tilt angle (0 is horizontal, 90deg is vertical) [deg]
		ms_trough.m_ColAz = value(P_COLAZ);					//Collector azimuth angle [deg]
		
		ms_trough.m_accept_mode = (int)value(P_ACCEPT_MODE);				// Acceptance testing mode? (1=yes, 0=no) [none]
		ms_trough.m_accept_init = value(P_ACCEPT_INIT) == 1;				// In acceptance testing mode - require steady-state startup [none]
		ms_trough.m_accept_loc = (int)value(P_ACCEPT_LOC);					// In acceptance testing mode - temperature sensor location (1=hx,2=loop) [none]
		ms_trough.m_is_using_input_gen = (bool)value(P_USING_INPUT_GEN);	// Is model getting inputs from input generator (true) or from other components in physical trough SYSTEM model (false)
		
		ms_trough.m_solar_mult = value(P_SOLAR_MULT);		//Solar multiple [none]
		ms_trough.m_mc_bal_hot_per_MW = value(P_MC_BAL_HOT);		//The heat capacity of the balance of plant on the hot side [kWht/K-MWt]
		ms_trough.m_mc_bal_cold_per_MW = value(P_MC_BAL_COLD);		//The heat capacity of the balance of plant on the cold side [kWht/K-MWt]
		ms_trough.m_mc_bal_sca = value(P_MC_BAL_SCA);		//Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]

		// Setup member vectors
		//int n_OptCharType = -1;
		//double *OptCharType = value(P_OPTCHARTYPE, &n_OptCharType);		//The optical characterization method  [none]
		//ms_trough.m_OptCharType.resize(n_OptCharType);
		//for( int i = 0; i < n_OptCharType; i++ )
		//	ms_trough.m_OptCharType[i] = (int)OptCharType[i];
		
		//int n_CollectorType = -1;
		//double *CollectorType = value(P_COLLECTORTYPE, &n_CollectorType);		//{1=user defined, 2=LS-2, 3=LS-3, 4=IST}  [none]
		//ms_trough.m_CollectorType.resize(n_CollectorType);
		//for( int i = 0; i < n_CollectorType; i++ )
		//	ms_trough.m_CollectorType[i] = (int)CollectorType[i];

		int n_W_aper = -1;
		double *W_aper = value(P_W_APERTURE, &n_W_aper);		//The collector aperture width (Total structural area.. used for shadowing) [m]
		ms_trough.m_W_aperture.resize(n_W_aper);
		for( int i = 0; i < n_W_aper; i++ )
			ms_trough.m_W_aperture[i] = W_aper[i];

		int n_A_aper = -1;
		double *A_aper = value(P_A_APERTURE, &n_A_aper);		//Reflective aperture area of the collector [m2]
		ms_trough.m_A_aperture.resize(n_A_aper);
		for( int i = 0; i < n_A_aper; i++ )
			ms_trough.m_A_aperture[i] = A_aper[i];

		int n_c_iam_matrix = -1;
		int n_r_iam_matrix = -1;
		double *p_iam_matrix = value(P_IAM_MATRIX, &n_r_iam_matrix, &n_c_iam_matrix);
		ms_trough.m_IAM_matrix.resize(n_r_iam_matrix, n_c_iam_matrix);
		for( int r = 0; r < n_r_iam_matrix; r++ )
			for( int c = 0; c < n_c_iam_matrix; c++ )
				ms_trough.m_IAM_matrix(r,c) = TCS_MATRIX_INDEX(var(P_IAM_MATRIX), r, c);

		//int n_refl = -1;
		//double *refl = value(P_REFLECTIVITY, &n_refl);		//Base solar-weighted mirror reflectivity value  [none]
		//ms_trough.m_reflectivity.resize(n_refl);
		//for( int i = 0; i < n_refl; i++ )
		//	ms_trough.m_reflectivity[i] = refl[i];

		int n_track = -1;
		double *track = value(P_TRACKINGERROR, &n_track);		//User-defined tracking error derate [none]
		ms_trough.m_TrackingError.resize(n_track);
		for( int i = 0; i < n_track; i++ )
			ms_trough.m_TrackingError[i] = track[i];

		int n_geom = -1;
		double *geom = value(P_GEOMEFFECTS, &n_geom);		//User-defined geometry effects derate [none]
		ms_trough.m_GeomEffects.resize(n_geom);
		for( int i = 0; i < n_geom; i++ )
			ms_trough.m_GeomEffects[i] = geom[i];

		int n_rho_m = -1;
		double *rho_m = value(P_RHO_MIRROR_CLEAN, &n_rho_m);		//User-defined clean mirror reflectivity [none]
		ms_trough.m_Rho_mirror_clean.resize(n_rho_m);
		for( int i = 0; i < n_rho_m; i++ )
			ms_trough.m_Rho_mirror_clean[i] = rho_m[i];

		int n_dirt_m = -1;
		double *dirt_m = value(P_DIRT_MIRROR, &n_dirt_m);		//User-defined dirt on mirror derate [none]
		ms_trough.m_Dirt_mirror.resize(n_dirt_m);
		for( int i = 0; i < n_dirt_m; i++ )
			ms_trough.m_Dirt_mirror[i] = dirt_m[i];

		int n_error = -1;
		double *error = value(P_ERROR, &n_error);		//User-defined general optical error derate  [none]
		ms_trough.m_Error.resize(n_error);
		for( int i = 0; i < n_error; i++ )
			ms_trough.m_Error[i] = error[i];

		int n_focal = -1;
		double *focal = value(P_AVE_FOCAL_LENGTH, &n_focal);		//The average focal length of the collector  [m]
		ms_trough.m_Ave_Focal_Length.resize(n_focal);
		for( int i = 0; i < n_focal; i++ )
			ms_trough.m_Ave_Focal_Length[i] = focal[i];

		int n_L_SCA = -1;
		double *L_SCA = value(P_L_SCA, &n_L_SCA);		//The length of the SCA  [m]
		ms_trough.m_L_SCA.resize(n_L_SCA);
		for( int i = 0; i < n_L_SCA; i++ )
			ms_trough.m_L_SCA[i] = L_SCA[i];

		int n_L_aper = -1;
		double *L_aper = value(P_L_APERTURE, &n_L_aper);		//The length of a single mirror/HCE unit [m]
		ms_trough.m_L_aperture.resize(n_L_aper);
		for( int i = 0; i < n_L_aper; i++ )
			ms_trough.m_L_aperture[i] = L_aper[i];

		int n_colper = -1;
		double *colper = value(P_COLPERSCA, &n_colper);		//The number of individual collector sections in an SCA  [none]
		ms_trough.m_ColperSCA.resize(n_colper);
		for( int i = 0; i < n_colper; i++ )
			ms_trough.m_ColperSCA[i] = colper[i];

		int n_dist = -1;
		double *dist = value(P_DISTANCE_SCA, &n_dist);		// piping distance between SCA's in the field [m]
		ms_trough.m_Distance_SCA.resize(n_dist);
		for( int i = 0; i < n_dist; i++ )
			ms_trough.m_Distance_SCA[i] = dist[i];

		set_matrix_t(P_HCE_FIELDFRAC, ms_trough.m_HCE_FieldFrac);
		set_matrix_t(P_D_2, ms_trough.m_D_2);
		set_matrix_t(P_D_3, ms_trough.m_D_3);
		set_matrix_t(P_D_4, ms_trough.m_D_4);
		set_matrix_t(P_D_5, ms_trough.m_D_5);
		set_matrix_t(P_D_P, ms_trough.m_D_p);
		set_matrix_t(P_FLOW_TYPE, ms_trough.m_Flow_type);
		set_matrix_t(P_ROUGH, ms_trough.m_Rough);
		set_matrix_t(P_ALPHA_ENV, ms_trough.m_alpha_env);
		set_matrix_t(P_EPSILON_3_11, ms_trough.m_epsilon_3_11);
		set_matrix_t(P_EPSILON_3_12, ms_trough.m_epsilon_3_12);
		set_matrix_t(P_EPSILON_3_13, ms_trough.m_epsilon_3_13);
		set_matrix_t(P_EPSILON_3_14, ms_trough.m_epsilon_3_14);
		set_matrix_t(P_EPSILON_3_21, ms_trough.m_epsilon_3_21);
		set_matrix_t(P_EPSILON_3_22, ms_trough.m_epsilon_3_22);
		set_matrix_t(P_EPSILON_3_23, ms_trough.m_epsilon_3_23);
		set_matrix_t(P_EPSILON_3_24, ms_trough.m_epsilon_3_24);
		set_matrix_t(P_EPSILON_3_31, ms_trough.m_epsilon_3_31);
		set_matrix_t(P_EPSILON_3_32, ms_trough.m_epsilon_3_32);
		set_matrix_t(P_EPSILON_3_33, ms_trough.m_epsilon_3_33);
		set_matrix_t(P_EPSILON_3_34, ms_trough.m_epsilon_3_34);
		set_matrix_t(P_EPSILON_3_41, ms_trough.m_epsilon_3_41);
		set_matrix_t(P_EPSILON_3_42, ms_trough.m_epsilon_3_42);
		set_matrix_t(P_EPSILON_3_43, ms_trough.m_epsilon_3_43);
		set_matrix_t(P_EPSILON_3_44, ms_trough.m_epsilon_3_44);
		
		set_matrix_t(P_ALPHA_ABS, ms_trough.m_alpha_abs);
		set_matrix_t(P_TAU_ENVELOPE, ms_trough.m_Tau_envelope);
		set_matrix_t(P_EPSILON_4, ms_trough.m_EPSILON_4);
		set_matrix_t(P_EPSILON_5, ms_trough.m_EPSILON_5);
		set_matrix_t(P_GLAZINGINTACTIN, ms_trough.m_GlazingIntact);
		set_matrix_t(P_P_A, ms_trough.m_P_a);
		set_matrix_t(P_ANNULUSGAS, ms_trough.m_AnnulusGas);
		set_matrix_t(P_ABSORBERMATERIAL, ms_trough.m_AbsorberMaterial);
		set_matrix_t(P_SHADOWING, ms_trough.m_Shadowing);
		set_matrix_t(P_DIRT_HCE, ms_trough.m_Dirt_HCE);
		set_matrix_t(P_DESIGN_LOSS, ms_trough.m_Design_loss);
		set_matrix_t(P_SCAINFOARRAY, ms_trough.m_SCAInfoArray);
		
		int n_defocus = -1;
		double *defocus = value(P_SCADEFOCUSARRAY, &n_defocus);
		ms_trough.m_SCADefocusArray.resize(n_defocus);
		for( int i = 0; i < n_defocus; i++ )
			ms_trough.m_SCADefocusArray[i] = defocus[i];

		C_csp_collector_receiver::S_csp_cr_solved_params solved_params;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
			init_inputs.m_latitude = value(P_LATITUDE);		//[deg] Site latitude read from weather file
			init_inputs.m_longitude = value(P_LONGITUDE);	//[deg] Site longitude read from weather file
			init_inputs.m_shift = value(P_SHIFT);			//[deg]
			ms_trough.init(init_inputs, solved_params);
		}

		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( ms_trough.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( ms_trough.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		// Write Calculated Design Parameters
		value(PO_A_APER_TOT, solved_params.m_A_aper_total);	//[m^2] Total solar field aperture area

		return 0;
	}

	virtual int call(double time, double step, int ncall)
	{

		// ******************************************************************************************************************************
		//               Time-dependent conditions
		// ******************************************************************************************************************************
		ms_weather.m_beam = value(I_I_B);		//[W/m^2] Direct normal incident solar irradiation
		ms_weather.m_tdry = value(I_T_DB);		//[C] Dry bulb air temperature
		ms_weather.m_wspd = value(I_V_WIND);	//[m/s] Ambient windspeed
		ms_weather.m_pres = value(I_P_AMB);		//[mbar] Ambient pressure
		ms_weather.m_tdew = value(I_T_DP);		//[C] The dewpoint temperature
		ms_htf_state_in.m_temp = value(I_T_COLD_IN);			//[C] HTF return temperature
		ms_htf_state_in.m_m_dot = value(I_M_DOT_IN)/3600.0;		//[kg/s] HTF mass flow rate at the inlet, convert from kg/hr
		ms_inputs.m_field_control = value(I_DEFOCUS);			//[-] Defocus control
		ms_weather.m_solazi = value(I_SOLARAZ);	//[deg] Solar azimuth angle reported by the Type15 weather file

		ms_sim_info.ms_ts.m_time = time;
		ms_sim_info.ms_ts.m_step = step;

		ms_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::ON;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			ms_trough.call(ms_weather,
				ms_htf_state_in,
				ms_inputs,
				ms_out_solver,
				//ms_out_report,
				ms_sim_info);
		}
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( ms_trough.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// Set outputs
		value(O_T_SYS_H, ms_out_solver.m_T_salt_hot);			//[C] Solar field HTF outlet temperature
		value(O_M_DOT_AVAIL, ms_out_solver.m_m_dot_salt_tot);	//[kg/hr] HTF mass flow rate from the field
		value(O_Q_AVAIL, 0.0);					//[MWt] Thermal power produced by the field
		value(O_DP_TOT, 0.0);					//[bar] Total HTF pressure drop
		value(O_W_DOT_PUMP, ms_out_solver.m_W_dot_htf_pump);		//[MWe] Required solar field pumping power
		value(O_E_FP_TOT, ms_out_solver.m_E_fp_total);				//[MW] Freeze protection energy
		value(O_QQ, 0.0);							//[none] Number of iterations required to solve
		value(O_T_SYS_C, 0.0);				//[C] Collector inlet temperature
		value(O_EQOPTEFF, 0.0);			//[none] Collector equivalent optical efficiency
		value(O_SCAS_DEF, 0.0);				//[none] The fraction of focused SCA's
		value(O_M_DOT_HTF_TOT, 0.0);	//[kg/hr] The actual flow rate through the field..
		value(O_E_BAL_STARTUP, 0.0);	//[MWt] Startup energy consumed
		value(O_Q_INC_SF_TOT, 0.0);		//[MWt] Total power incident on the field
		value(O_Q_ABS_TOT, 0.0);				//[MWt] Total absorbed energy
		value(O_Q_LOSS_TOT, 0.0);			//[MWt] Total receiver thermal and optical losses
		value(O_M_DOT_HTF, 0.0);				//[kg/s] Flow rate in a single loop
		value(O_Q_LOSS_SPEC_TOT, 0.0);	//[W/m] Field-average receiver thermal losses (convection and radiation)
		value(O_SCA_PAR_TOT, ms_out_solver.m_W_dot_col_tracking);		//[MWe] Parasitic electric power consumed by the SC
		value(O_PIPE_HL, 0.0);				//[MWt] Pipe heat loss in the header and the hot runner
		value(O_Q_DUMP, 0.0);					//[MWt] Dumped thermal energy
		value(O_THETA_AVE, 0.0);			//[deg] Field average theta value
		value(O_COSTH_AVE, 0.0);			//[none] Field average costheta value
		value(O_IAM_AVE, 0.0);					//[none] Field average incidence angle modifier
		value(O_ROWSHADOW_AVE, 0.0);		//[none] Field average row shadowing loss
		value(O_ENDLOSS_AVE, 0.0);			//[none] Field average end loss
		value(O_DNI_COSTH, 0.0);				//[W/m2] DNI_x_CosTh
		value(O_QINC_COSTH, 0.0);			//[MWt] Q_inc_x_CosTh
		value(O_T_LOOP_OUTLET, 0.0);		//[C] HTF temperature immediately subsequent to the loop outlet
		value(O_C_HTF_AVE, 0.0);				//[J/kg-K] Average solar field specific heat
		value(O_Q_FIELD_DELIVERED, 0.0);		//[MWt] Total solar field thermal power delivered
		value(O_ETA_THERMAL, 0.0);			//[none] Solar field thermal efficiency (power out/ANI)
		value(O_E_LOOP_ACCUM, 0.0);	//[MWht] Accumulated internal energy change rate in the loops ONLY
		value(O_E_HDR_ACCUM, 0.0);		//[MWht] Accumulated internal energy change rate in the headers/SGS
		value(O_E_TOT_ACCUM, 0.0);			//[MWht] Total accumulated internal energy change rate
		value(O_E_FIELD, 0.0);				//[MWht] Accumulated internal energy in the entire solar field
		value(O_T_C_IN_CALC, 0.0);	//[C] Calculated cold HTF inlet temperature - used in freeze protection and for stand-alone model in recirculation

		return 0;
	}

	virtual int converged(double time)
	{
		int out_type = -1;
		std::string out_msg = "";

		try
		{
			ms_trough.converged();
		}

		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( ms_trough.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( ms_trough.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_trough_type250, "Physical trough solar field model", "Mike Wagner", 1, sam_mw_trough_type250_variables, NULL, 1 );
