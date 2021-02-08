#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "sam_csp_util.h"
//#include "waterprop.h"
#include "water_properties.h"

//using namespace std;
#include "sam_csp_util.h"

#include <cmath>

#include "csp_solver_lf_dsg_collector_receiver.h"
#include "csp_solver_util.h"
#include "csp_solver_core.h"

enum{	//Parameters  
		P_TESHOURS,         
		P_Q_MAX_AUX,        
		P_LHV_EFF,          
		P_T_SET_AUX,        
		P_T_FIELD_IN_DES,   
		P_T_FIELD_OUT_DES,  
		P_X_B_DES,          
		P_P_TURB_DES,       
		P_FP_HDR_C,         
		P_FP_SF_BOIL,       
		P_FP_BOIL_TO_SH,    
		P_FP_SF_SH,         
		P_FP_HDR_H,         
		P_Q_PB_DES,         
		P_W_PB_DES,         
		P_CYCLE_MAX_FRAC,   
		P_CYCLE_CUTOFF_FRAC,
		P_T_SBY,            
		P_Q_SBY_FRAC,       
		P_SOLARM,	        
		P_PB_PUMP_COEF,
		P_PB_FIXED_PAR,
		P_BOP_ARRAY,        
		P_AUX_ARRAY,        
		P_T_STARTUP,
		P_FOSSIL_MODE,     
		P_I_BN_DES,        
		P_IS_SH,           
		P_IS_ONCETHRU,     
		P_IS_MULTGEOM,     
		P_NMODBOIL,        
		P_NMODSH,          
		P_NLOOPS,          
		P_ETA_PUMP,        
		P_LATITUDE,        
		P_THETA_STOW,      
		P_THETA_DEP,       
		P_M_DOT_MIN,       
		P_T_FIELD_INI,     
		P_T_FP,            
		P_PIPE_HL_COEF,    
		P_SCA_DRIVES_ELEC, 
		P_COLAZ,			
		P_E_STARTUP,       
		P_T_AMB_DES_SF,    
		P_V_WIND_MAX,      
		P_FFRAC,           
		P_A_APERTURE,      
		P_L_COL,           
		P_OPTCHARTYPE,     
		P_IAM_T,           
		P_IAM_L,           
		P_TRACKINGERROR,   
		P_GEOMEFFECTS,     
		P_RHO_MIRROR_CLEAN,
		P_DIRT_MIRROR,     
		P_ERROR,           
		P_HLCHARTYPE,      
		P_HL_DT,           
		P_HL_W,            
		P_D_2,			   
		P_D_3,             
		P_D_4,             
		P_D_5,             
		P_D_p,             
		P_ROUGH,           
		P_FLOW_TYPE,       
		P_ABSORBER_MAT, 
		P_HCE_FIELDFRAC,  
		P_ALPHA_ABS,      
		P_B_EPS_HCE1, 
		P_B_EPS_HCE2, 
		P_B_EPS_HCE3, 
		P_B_EPS_HCE4, 
		P_SH_EPS_HCE1,
		P_SH_EPS_HCE2,
		P_SH_EPS_HCE3,
		P_SH_EPS_HCE4,
		P_ALPHA_ENV,      
		P_EPSILON_4,      
		P_TAU_ENVELOPE,   
		P_GLAZINGINTACTIN,
		P_ANNULUSGAS,     
		P_P_A,            
		P_DESIGN_LOSS,    
		P_SHADOWING,      
		P_DIRT_HCE, 
		P_B_OPTICALTABLE, 
		P_SH_OPTICALTABLE,
		PO_A_APER_TOT,

		//Inputs         
		I_DNIFC,        
		I_I_BN,			
		I_T_DB,			
		I_T_DP,			
		I_P_AMB,		
		I_V_WIND,		
		I_M_DOT_HTF_REF,
		I_M_PB_DEMAND,	
		I_SHIFT,		
		I_SOLARAZ,	
		I_SOLARZEN,
		I_T_PB_OUT,		
		I_TOUPERIOD,	

		//Outputs                       
		O_CYCLE_PL_CONTROL,  
		O_DP_TOT,			  
		O_DP_HDR_C,		  
		O_DP_SF_BOIL,		  
		O_DP_BOIL_TO_SH,	  
		O_DP_SF_SH,		  
		O_DP_HDR_H,		  
		O_E_BAL_STARTUP,	  
		O_E_FIELD,			  
		O_E_FP_TOT,		  
		O_ETA_OPT_AVE,		  
		O_ETA_THERMAL,		  
		O_ETA_SF,            
		O_DEFOCUS,	          
		O_M_DOT_AUX,		  
		O_M_DOT_FIELD,		  
		O_M_DOT_B_TOT,		  
		O_M_DOT,			  
		O_M_DOT_TO_PB,		  
		O_P_TURB_IN,		  
		O_Q_LOSS_PIPING,	  
		O_Q_AUX_FLUID,		  
		O_Q_AUX_FUEL,		  
		O_Q_DUMP,			  
		O_Q_FIELD_DELIVERED, 
		O_Q_INC_TOT,		  
		O_Q_LOSS_REC,		  
		O_Q_LOSS_SF,		  
		O_Q_TO_PB,			  
		O_SOLARALT,		  
		O_SOLARAZ,			  
		O_PHI_T,			  
		O_THETA_L,			  
		O_STANDBY_CONTROL,	  
		O_T_FIELD_IN,		  
		O_T_FIELD_OUT,		  
		O_T_LOOP_OUT,		  
		O_T_PB_IN,			  
		O_W_DOT_AUX,		  
		O_W_DOT_BOP,		  
		O_W_DOT_COL,		  
		O_W_DOT_FIXED,		  
		O_W_DOT_PUMP,	
		O_W_DOT_PAR_TOT,
		O_P_SF_IN,

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_lf_type261_steam_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_TESHOURS,         "tes_hours",              "Equivalent full-load thermal storage hours",                           "hr",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_Q_MAX_AUX,        "q_max_aux",              "Maximum heat rate of the auxiliary heater",                            "MW",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_LHV_EFF,          "LHV_eff",				  "Fuel LHV efficiency (0..1)",                                        	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_T_SET_AUX,        "T_set_aux",			  "Aux heater outlet temperature set point",                           	  "C",   "", "", ""},       
	{TCS_PARAM, TCS_NUMBER, P_T_FIELD_IN_DES,   "T_field_in_des",		  "Field design inlet temperature",                                    	  "C",   "", "", ""},       
	{TCS_PARAM, TCS_NUMBER, P_T_FIELD_OUT_DES,  "T_field_out_des",		  "Field loop outlet design temperature",                              	  "C",   "", "", ""},       
	{TCS_PARAM, TCS_NUMBER, P_X_B_DES,          "x_b_des",				  "Design point boiler outlet steam quality",                          	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_P_TURB_DES,       "P_turb_des",			  "Design-point turbine inlet pressure",                               	  "bar",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_FP_HDR_C,         "fP_hdr_c",				  "Average design-point cold header pressure drop fraction",           	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_FP_SF_BOIL,       "fP_sf_boil",			  "Design-point pressure drop across the solar field boiler fraction", 	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_FP_BOIL_TO_SH,    "fP_boil_to_sh",		  "Design-point pressure drop between the boiler and superheater frac",	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_FP_SF_SH,         "fP_sf_sh",				  "Design-point pressure drop across the solar field superheater frac",	  "none",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FP_HDR_H,         "fP_hdr_h",				  "Average design-point hot header pressure drop fraction",            	  "none",   "", "", ""},    
	{TCS_PARAM, TCS_NUMBER, P_Q_PB_DES,         "q_pb_des",				  "Design heat input to the power block",                              	  "MW",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_W_PB_DES,         "W_pb_des",				  "Rated plant capacity",                                              	  "MW",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER,	P_CYCLE_MAX_FRAC,   "cycle_max_fraction",	  "Maximum turbine over design operation fraction",                    	  "none",   "", "", ""},    
    {TCS_PARAM, TCS_NUMBER,	P_CYCLE_CUTOFF_FRAC,"cycle_cutoff_frac",      "Minimum turbine operation fraction before shutdown",                   "none",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_T_SBY,            "t_sby",				  "Low resource standby period",                                          "hr",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_Q_SBY_FRAC,       "q_sby_frac",			  "Fraction of thermal power required for standby",                       "none",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_SOLARM,	        "solarm",				  "Solar multiple",                                                       "none",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_PB_PUMP_COEF,     "PB_pump_coef",			  "Pumping power required to move 1kg of HTF through power block flow",   "kW/kg",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PB_FIXED_PAR,     "PB_fixed_par",           "fraction of rated gross power consumed at all hours of the year",      "none",    "", "", ""},
	{TCS_PARAM, TCS_ARRAY,  P_BOP_ARRAY,        "bop_array",              "BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2",                  "-",            "",        "",        ""},
	{TCS_PARAM, TCS_ARRAY,  P_AUX_ARRAY,        "aux_array",              "Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2",                  "-",            "",        "",        ""},
	{TCS_PARAM, TCS_NUMBER, P_T_STARTUP,		"T_startup",              "Startup temperature (same as field startup)",                          "C",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FOSSIL_MODE,      "fossil_mode",            "Operation mode for the fossil backup {1=Normal,2=supp,3=toppin}",          "none",   "", "", ""},   
	{TCS_PARAM, TCS_NUMBER, P_I_BN_DES,         "I_bn_des",				  "Design point irradiation value",                                    		  "W/m2",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_IS_SH,            "is_sh",				  "Does the solar field include a superheating section",               		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_IS_ONCETHRU,      "is_oncethru",			  "Flag indicating whether flow is once through with superheat",       		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_IS_MULTGEOM,      "is_multgeom",			  "Does the superheater have a different geometry from the boiler {1=yes}",	  "none",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_NMODBOIL,         "nModBoil",				  "Number of modules in the boiler section",                           		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_NMODSH,           "nModSH",				  "Number of modules in the superheater section",                      		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_NLOOPS,           "nLoops",				  "Number of loops",                                                   		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_ETA_PUMP,         "eta_pump",				  "Feedwater pump efficiency",                                         		  "none",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_LATITUDE,         "latitude",				  "Site latitude read from weather file",                              		  "deg",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_THETA_STOW,       "theta_stow",			  "stow angle",                                                        		  "deg",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_THETA_DEP,        "theta_dep",			  "deploy angle",                                                      		  "deg",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_M_DOT_MIN,        "m_dot_min",			  "Minimum loop flow rate",                                            		  "kg/s",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_T_FIELD_INI,      "T_field_ini",			  "Initial field temperature",                                         		  "C",   "", "", ""},        
	{TCS_PARAM, TCS_NUMBER, P_T_FP,             "T_fp",					  "Freeze protection temperature (heat trace activation temperature)", 		  "C",   "", "", ""},        
	{TCS_PARAM, TCS_NUMBER, P_PIPE_HL_COEF,     "Pipe_hl_coef",			  "Loss coefficient from the header.. runner pipe.. and non-HCE pipin",		  "W/m2-K",   "", "", ""},  
	{TCS_PARAM, TCS_NUMBER, P_SCA_DRIVES_ELEC,  "SCA_drives_elec",		  "Tracking power.. in Watts per SCA drive",                           		  "W/m2",   "", "", ""},     
	{TCS_PARAM, TCS_NUMBER, P_COLAZ,			"ColAz",				  "Collector azimuth angle",                                           		  "deg",   "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_E_STARTUP,        "e_startup",              "Thermal inertia contribution per sq meter of solar field",                     "kJ/K-m2",  "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_AMB_DES_SF,     "T_amb_des_sf",		      "Design-point ambient temperature",                            				  "C",  "", "", ""},      
	{TCS_PARAM, TCS_NUMBER, P_V_WIND_MAX,       "V_wind_max",			  "Maximum allowable wind velocity before safety stow",          				  "m/s",  "", "", ""},    
	{TCS_PARAM, TCS_ARRAY,  P_FFRAC,            "ffrac",				  "Fossil dispatch logic - TOU periods",                                       	  "none",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_A_APERTURE,       "A_aperture",			  "(boiler, SH) Reflective aperture area of the collector module",            	  "m^2",  "", "", ""},    
	{TCS_PARAM, TCS_MATRIX, P_L_COL,            "L_col",				  "(boiler, SH) Active length of the superheater section collector module",   	  "m",  "", "", ""},      
	{TCS_PARAM, TCS_MATRIX, P_OPTCHARTYPE,      "OptCharType",			  "(boiler, SH) The optical characterization method",                         	  "none",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_IAM_T,            "IAM_T",                  "(boiler, SH) Transverse Incident angle modifiers (0,1,2,3,4 order terms)",     "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_IAM_L,            "IAM_L",                  "(boiler, SH) Longitudinal Incident angle modifiers (0,1,2,3,4 order terms)",   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_TRACKINGERROR,    "TrackingError",          "(boiler, SH) User-defined tracking error derate",                                          "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_GEOMEFFECTS,      "GeomEffects",			  "(boiler, SH) User-defined geometry effects derate",   									  "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_RHO_MIRROR_CLEAN, "rho_mirror_clean",		  "(boiler, SH) User-defined clean mirror reflectivity", 									  "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_DIRT_MIRROR,      "dirt_mirror",			  "(boiler, SH) User-defined dirt on mirror derate",     									  "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_ERROR,            "error",                  "(boiler, SH) User-defined general optical error derate",                                   "none",  "", "", ""},       
	{TCS_PARAM, TCS_MATRIX, P_HLCHARTYPE,       "HLCharType",			  "(boiler, SH) Flag indicating the heat loss model type {1=poly.; 2=Forristall}",            "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_HL_DT,            "HL_dT",                  "(boiler, SH) Heat loss coefficient - HTF temperature (0,1,2,3,4 order terms)",             "W/m-K^order",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_HL_W,             "HL_W",                   "(boiler, SH) Heat loss coef adj wind velocity (0,1,2,3,4 order terms)",					  "1/(m/s)^order",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_D_2,			    "D_2",                    "(boiler, SH) The inner absorber tube diameter",                                                         "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_D_3,              "D_3",					  "(boiler, SH) The outer absorber tube diameter",                 										  "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_D_4,              "D_4",					  "(boiler, SH) The inner glass envelope diameter",                										  "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_D_5,              "D_5",					  "(boiler, SH) The outer glass envelope diameter",                										  "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_D_p,              "D_p",					  "(boiler, SH) The diameter of the absorber flow plug (optional)",										  "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_ROUGH,            "Rough",				  "(boiler, SH) Roughness of the internal surface",                										  "m",  "", "", ""},   
	{TCS_PARAM, TCS_MATRIX, P_FLOW_TYPE,        "Flow_type",			  "(boiler, SH) The flow type through the absorber",               										  "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_ABSORBER_MAT,     "AbsorberMaterial",		  "(boiler, SH) Absorber material type",                           										  "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_HCE_FIELDFRAC,    "HCE_FieldFrac",          "(boiler, SH) The fraction of the field occupied by this HCE type (4: # field fracs)",       "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_ALPHA_ABS,        "alpha_abs",			  "(boiler, SH) Absorber absorptance (4: # field fracs)",                               	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_B_EPS_HCE1,       "b_eps_HCE1",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_B_EPS_HCE2,       "b_eps_HCE2",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_B_EPS_HCE3,       "b_eps_HCE3",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_B_EPS_HCE4,       "b_eps_HCE4",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SH_EPS_HCE1,      "sh_eps_HCE1",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SH_EPS_HCE2,      "sh_eps_HCE2",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SH_EPS_HCE3,      "sh_eps_HCE3",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SH_EPS_HCE4,      "sh_eps_HCE4",			  "(temperature) Absorber emittance (eps)",                                 				   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_ALPHA_ENV,        "alpha_env",			  "(boiler, SH) Envelope absorptance (4: # field fracs)",                               	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_EPSILON_4,        "EPSILON_4",			  "(boiler, SH) Inner glass envelope emissivities (Pyrex) (4: # field fracs)",          	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_TAU_ENVELOPE,     "Tau_envelope",			  "(boiler, SH) Envelope transmittance (4: # field fracs)",                             	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_GLAZINGINTACTIN,  "GlazingIntactIn",		  "(boiler, SH) The glazing intact flag {true=0; false=1} (4: # field fracs)",          	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_ANNULUSGAS,       "AnnulusGas",			  "(boiler, SH) Annulus gas type {1=air; 26=Ar; 27=H2} (4: # field fracs)",             	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_P_A,              "P_a",					  "(boiler, SH) Annulus gas pressure (4: # field fracs)",                               	   "torr",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_DESIGN_LOSS,      "Design_loss",			  "(boiler, SH) Receiver heat loss at design (4: # field fracs)",                       	   "W/m",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SHADOWING,        "Shadowing",			  "(boiler, SH) Receiver bellows shadowing loss factor (4: # field fracs)",             	   "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_DIRT_HCE,         "Dirt_HCE",				  "(boiler, SH) Loss due to dirt on the receiver envelope (4: # field fracs)",          	   "none",  "", "", ""},
    {TCS_PARAM, TCS_MATRIX, P_B_OPTICALTABLE,   "b_OpticalTable",         "Values of the optical efficiency table",                                                    "none",  "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_SH_OPTICALTABLE,  "sh_OpticalTable",        "Values of the optical efficiency table",                                                    "none",  "", "", ""},

	// Field design calculations
	{TCS_PARAM, TCS_NUMBER, PO_A_APER_TOT,      "A_aper_tot",             "Total solar field aperture area",                                                           "m^2",   "", "", "-1.23" },

	// INPUTS   
	{TCS_INPUT, TCS_NUMBER, I_DNIFC,            "dnifc",                  "Forecast DNI",                                                      "W/m2",   "", "", ""},      
	{TCS_INPUT, TCS_NUMBER, I_I_BN,				"I_bn",                   "Beam normal radiation (input kJ/m2-hr)",                            "W/m2",   "", "", ""}, 
	{TCS_INPUT, TCS_NUMBER, I_T_DB,				"T_db",                   "Dry bulb air temperature",                                          "C",      "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_T_DP,				"T_dp",                   "The dewpoint temperature",                                          "C",      "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_P_AMB,			"P_amb",                  "Ambient pressure",                                                  "atm",    "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_V_WIND,			"V_wind",                 "Ambient windspeed",                                                 "m/s",    "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_M_DOT_HTF_REF,	"m_dot_htf_ref",          "Reference HTF flow rate at design conditions",                      "kg/hr",  "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_M_PB_DEMAND,		"m_pb_demand",            "Demand htf flow from the power block",                              "kg/hr",  "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_SHIFT,			"shift",                  "Shift in longitude from local standard meridian",                   "deg",    "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_SOLARAZ,			"SolarAz",                "Solar azimuth angle",                                               "deg",    "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_SOLARZEN,         "SolarZen",               "Solar zenith angle",                                                "deg",    "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_PB_OUT,			"T_pb_out",               "Fluid temperature from the power block",                            "C",      "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_TOUPERIOD,		"TOUPeriod",              "Time of use period",                                                "none",   "", "", ""},     

	// OUTPUTS 
	{TCS_OUTPUT, TCS_NUMBER, O_CYCLE_PL_CONTROL,   "cycle_pl_control",      "Part-load control flag - used by Type224",                             "none",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_TOT,			   "dP_tot",                "Total HTF pressure drop",                                           	  "bar",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_HDR_C,		   "dP_hdr_c",              "Average cold header pressure drop",                                 	  "bar",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_SF_BOIL,		   "dP_sf_boil",            "Pressure drop across the solar field boiler",                       	  "bar",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_BOIL_TO_SH,	   "dP_boil_to_SH",         "Pressure drop between the boiler and superheater",                  	  "bar",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_SF_SH,		   "dP_sf_sh",              "Pressure drop across the solar field superheater",                  	  "bar",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DP_HDR_H,		   "dP_hdr_h",              "Average hot header pressure drop",                                  	  "bar",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_E_BAL_STARTUP,	   "E_bal_startup",         "Startup energy consumed",                                           	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_E_FIELD,			   "E_field",               "Accumulated internal energy in the entire solar field",             	  "MW-hr",	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_E_FP_TOT,		   "E_fp_tot",              "Freeze protection energy",                                          	  "J",    	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_OPT_AVE,		   "eta_opt_ave",           "collector equivalent optical efficiency",                           	  "none",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_THERMAL,		   "eta_thermal",           "Solar field thermal efficiency (power out/ANI)",                    	  "none", 	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_SF,             "eta_sf",                "Total solar field collection efficiency",                           	  "none", 	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_DEFOCUS,	           "defocus",  		   	    "The fraction of focused aperture area in the solar field",          	  "none", 	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_AUX,		   "m_dot_aux",             "Auxiliary heater mass flow rate",                                   	  "kg/hr",	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_FIELD,		   "m_dot_field",           "Flow rate from the field",                                          	  "kg/hr",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_B_TOT,		   "m_dot_b_tot",           "Flow rate within the boiler section",                               	  "kg/hr",	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT,			   "m_dot",                 "Flow rate in a single loop",                                        	  "kg/s", 	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_TO_PB,		   "m_dot_to_pb",           "Flow rate delivered to the power block",                            	  "kg/hr",	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_TURB_IN,		   "P_turb_in",             "Pressure at the turbine inlet",                                     	  "bar",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_LOSS_PIPING,	   "q_loss_piping",         "Pipe heat loss in the hot header and the hot runner",               	  "MW",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_AUX_FLUID,		   "q_aux_fluid",           "Thermal energy provided to the fluid passing through the aux heater",    "MW",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_AUX_FUEL,		   "q_aux_fuel",            "Heat content of fuel required to provide aux firing",               	  "MMBTU",	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_DUMP,			   "q_dump",                "Dumped thermal energy",                                             	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_FIELD_DELIVERED,  "q_field_delivered",     "Total solar field thermal power delivered",                         	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_INC_TOT,		   "q_inc_tot",             "Total power incident on the field",                                 	  "MW",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_LOSS_REC,		   "q_loss_rec",            "Total Receiver thermal losses",                                     	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_LOSS_SF,		   "q_loss_sf",             "Total solar field thermal losses",                                  	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_TO_PB,			   "q_to_pb",               "Thermal energy to the power block",                                 	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_SOLARALT,		   "SolarAlt",              "Solar altitude used in optical calculations",                       	  "deg",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_SOLARAZ,			   "SolarAz",               "Solar azimuth used in optical calculations",                        	  "deg",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_PHI_T,			   "phi_t",                 "Transversal solar incidence angle",                                 	  "deg",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_THETA_L,			   "theta_L",               "Longitudinal solar incidence angle",                                	  "deg",  	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_STANDBY_CONTROL,	   "standby_control",       "Standby control flag - used by Type224",                            	  "none", 	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_FIELD_IN,		   "T_field_in",            "HTF temperature into the collector field header",                   	  "C",    	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_FIELD_OUT,		   "T_field_out",           "HTF Temperature from the field",                                    	  "C",       "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_LOOP_OUT,		   "T_loop_out",            "Loop outlet temperature",                                           	  "C",    	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_PB_IN,			   "T_pb_in",               "HTF Temperature to the power block",                                	  "C",    	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_AUX,		   "W_dot_aux",             "Parasitic power associated with operation of the aux boiler",       	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_BOP,		   "W_dot_bop",             "parasitic power as a function of power block load",                 	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_COL,		   "W_dot_col",             "Parasitic electric power consumed by the collectors",               	  "MW",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_FIXED,		   "W_dot_fixed",           "Fixed parasitic power losses.. for every hour of operation",        	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PUMP,		   "W_dot_pump",       	    "Required solar field pumping power",                                	  "MW",   	 "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PAR_TOT,      "W_dot_par_tot",         "Total parasitics",                                                       "MW",      "", "", ""},																																					 																																							
	{TCS_OUTPUT, TCS_NUMBER, O_P_SF_IN,            "P_sf_in",               "Solar field inlet pressure",                                             "bar",     "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	

class sam_mw_lf_type261_steam : public tcstypeinterface
{
private:
	C_csp_lf_dsg_collector_receiver dsg_lf;
	C_csp_weatherreader::S_outputs dsg_weather;
	C_csp_solver_htf_1state dsg_htf_state_in;
	C_csp_collector_receiver::S_csp_cr_inputs dsg_inputs;
	C_csp_collector_receiver::S_csp_cr_out_solver dsg_out_solver;
	C_csp_solver_sim_info dsg_sim_info;

public:
	sam_mw_lf_type261_steam( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		

	}

	virtual ~sam_mw_lf_type261_steam()
	{
		//try
		//{
		//	// Set up matrix_t of pointers to HTFproperties class
		//	for( int i = 0; i < m_n_rows_matrix; i++ )
		//	{
		//		for( int j = 0; j < 4; j++ )
		//		{
		//			delete m_AnnulusGas.at(i,j);
		//		}
		//	}
		//}
		//catch(...){};

		//try
		//{
		//	for( int i = 0; i < m_n_rows_matrix; i++ )
		//	{
		//		delete m_AbsorberMaterial.at(i,0);
		//	}
		//}
		//catch(...){};
	}

	double turb_pres_frac( double m_dot_nd, int fmode, double ffrac, double fP_min )
	{
		/*Take a mass flow fraction, fossil backup fraction, fossil fill mode, and minimum turbine fraction
		and calculate the corresponding fraction of the design point pressure at which the turbine 
		will operate*/
		switch( fmode )
		{
		case 1:			// Backup minimum level - parallel
			return max( fP_min, max( m_dot_nd, ffrac ) );
		case 2:			// Supplemental Operation - parallel
			return max( fP_min, max( m_dot_nd, min(1.0, m_dot_nd + ffrac)) );
		case 3:			// Temperature topping mode - series
			return max( fP_min, m_dot_nd );
		}
	}

	void set_matrix_t(size_t idx, util::matrix_t<double> &class_matrix)
	{
		int n_rows = -1;
		int n_cols = -1;
		double *data = value(idx, &n_rows, &n_cols);
		class_matrix.resize(n_rows, n_cols);
		for (int r = 0; r < n_rows; r++)
			for (int c = 0; c < n_cols; c++)
				class_matrix(r, c) = TCS_MATRIX_INDEX(var(idx), r, c);
	}

	void set_matrix_t(size_t idx, util::matrix_t<int> &class_matrix)
	{
		int n_rows = -1;
		int n_cols = -1;
		double *data = value(idx, &n_rows, &n_cols);
		class_matrix.resize(n_rows, n_cols);
		for (int r = 0; r < n_rows; r++)
			for (int c = 0; c < n_cols; c++)
				class_matrix(r, c) = (int)TCS_MATRIX_INDEX(var(idx), r, c);
	}

	void set_matrix_t(size_t idx, util::matrix_t<bool> &class_matrix)
	{
		int n_rows = -1;
		int n_cols = -1;
		double *data = value(idx, &n_rows, &n_cols);
		class_matrix.resize(n_rows, n_cols);
		for (int r = 0; r < n_rows; r++)
			for (int c = 0; c < n_cols; c++)
				class_matrix(r, c) = (bool)TCS_MATRIX_INDEX(var(idx), r, c);
	}

	virtual int init()
	{

		////Starting new code
		//C_csp_lf_dsg_collector_receiver c_lf_dsg;

		// Now set solar field collector unit parameters
		dsg_lf.m_q_max_aux = value(P_Q_MAX_AUX)*1.0E3; // q_max_aux );
		dsg_lf.m_LHV_eff = value(P_Q_MAX_AUX); // LHV_eff );
		dsg_lf.m_T_set_aux = value(P_T_SET_AUX) + 273.15;
		dsg_lf.m_T_field_in_des = value(P_T_FIELD_IN_DES) + 273.15;
		dsg_lf.m_T_field_out_des = value(P_T_FIELD_OUT_DES) + 273.15;
		dsg_lf.m_x_b_des = value(P_X_B_DES); // x_b_des );
		dsg_lf.m_P_turb_des = value(P_P_TURB_DES); // P_turb_des );
		dsg_lf.m_fP_hdr_c = value(P_FP_HDR_C); // fP_hdr_c );
		dsg_lf.m_fP_sf_boil = value(P_FP_SF_BOIL); // fP_sf_boil );
		dsg_lf.m_fP_boil_to_sh = value(P_FP_BOIL_TO_SH); // fP_boil_to_SH );
		dsg_lf.m_fP_sf_sh = value(P_FP_SF_SH); // fP_sf_sh );
		dsg_lf.m_fP_hdr_h = value(P_FP_HDR_H); // fP_hdr_h );
		dsg_lf.m_q_pb_des = value(P_Q_PB_DES)*1000.0; // Q_ref ); // = P_ref/eta_ref;
		dsg_lf.m_W_pb_des = value(P_W_PB_DES)*1000.0;
		
		
		dsg_lf.m_m_dot_max_frac = 3.0;		//[kg/s] Let cycle fractions control mass flow rate limits
		dsg_lf.m_m_dot_min_frac = 0.0;		//[kg/s] Let cycle fractions control mass flow rate limits


		dsg_lf.m_cycle_max_fraction = value(P_CYCLE_MAX_FRAC); // cycle_max_fraction );
		dsg_lf.m_cycle_cutoff_frac = value(P_CYCLE_CUTOFF_FRAC); // cycle_cutoff_frac );
		
		
		dsg_lf.m_t_sby_des = value(P_T_SBY); // t_sby );
		dsg_lf.m_q_sby_frac = value(P_Q_SBY_FRAC); // q_sby_frac );
		dsg_lf.m_PB_fixed_par = value(P_PB_FIXED_PAR); // PB_fixed_par );	dsg_lf.q_max_aux = value("T_startup", value("T_hot"));
		dsg_lf.m_fossil_mode = (int)value(P_FOSSIL_MODE); // fossil_mode );
		dsg_lf.m_I_bn_des = value(P_I_BN_DES); // I_bn_des );
		dsg_lf.m_is_oncethru = (bool)value(P_IS_ONCETHRU); // is_oncethru ); ? bool
		dsg_lf.m_is_sh_target = true;
		dsg_lf.m_is_multgeom = (bool)value(P_IS_MULTGEOM); // is_multgeom ); 
		dsg_lf.m_nModBoil = (int)value(P_NMODBOIL); // nModBoil );
		dsg_lf.m_nModSH = (int)value(P_NMODSH); // nModSH );
		dsg_lf.m_nLoops = (int)value(P_NLOOPS); // nLoops );
		dsg_lf.m_eta_pump = value(P_ETA_PUMP); // eta_pump );
		dsg_lf.m_latitude = value(P_LATITUDE)*0.0174533; // latitude );
		dsg_lf.m_theta_stow = value(P_THETA_STOW)*0.0174533; // theta_stow );
		dsg_lf.m_theta_dep = value(P_THETA_DEP)*0.0174533; // theta_dep );
		dsg_lf.m_T_field_ini = value(P_T_FIELD_INI) + 275.15;
		dsg_lf.m_T_fp = value(P_T_FP) + 273.15; // T_fp );
		dsg_lf.m_Pipe_hl_coef = value(P_PIPE_HL_COEF); // Pipe_hl_coef );
		dsg_lf.m_SCA_drives_elec = value(P_SCA_DRIVES_ELEC); // SCA_drives_elec );
		dsg_lf.m_ColAz = value(P_COLAZ)*0.0174533; // ColAz );
		dsg_lf.m_e_startup = value(P_E_STARTUP); // e_startup );
		dsg_lf.m_T_amb_des_sf = value(P_T_AMB_DES_SF) + 273.15; // T_amb_des_sf );
		dsg_lf.m_V_wind_max = value(P_V_WIND_MAX); // V_wind_max );		

		int nval_bop_array = -1;
		double *bop_array = value(P_BOP_ARRAY, &nval_bop_array);		//The collector aperture width (Total structural area.. used for shadowing) [m]
		dsg_lf.m_bop_array.resize(nval_bop_array);
		for (int i = 0; i < nval_bop_array; i++)
			dsg_lf.m_bop_array[i] = bop_array[i];

		int nval_aux_array = -1;
		double *aux_array = value(P_AUX_ARRAY, &nval_aux_array);
		dsg_lf.m_aux_array.resize(nval_aux_array);
		for (int i = 0; i < nval_aux_array; i++)
			dsg_lf.m_aux_array[i] = (double)aux_array[i];

		int nval_ffrac = -1;
		double *ffrac = value(P_FFRAC, &nval_ffrac);
		dsg_lf.m_ffrac.resize(nval_ffrac);
		for (int i = 0; i < nval_ffrac; i++)
			dsg_lf.m_ffrac[i] = (double)ffrac[i];

		set_matrix_t(P_A_APERTURE, dsg_lf.m_A_aperture); //A_aper);
		set_matrix_t(P_L_COL, dsg_lf.m_L_col); //L_col);
		set_matrix_t(P_OPTCHARTYPE, dsg_lf.m_OptCharType); //OptCharType);
		set_matrix_t(P_IAM_T, dsg_lf.m_IAM_T); //IAM_T);
		set_matrix_t(P_IAM_L, dsg_lf.m_IAM_L); //IAM_L);
		set_matrix_t(P_TRACKINGERROR, dsg_lf.m_TrackingError); //TrackingError);
		set_matrix_t(P_GEOMEFFECTS, dsg_lf.m_GeomEffects); //GeomEffects);
		set_matrix_t(P_RHO_MIRROR_CLEAN, dsg_lf.m_rho_mirror_clean); //rho_mirror_clean);
		set_matrix_t(P_DIRT_MIRROR, dsg_lf.m_dirt_mirror); //dirt_mirror);
		set_matrix_t(P_ERROR, dsg_lf.m_error);//error);
		set_matrix_t(P_HLCHARTYPE, dsg_lf.m_HLCharType);//HLCharType);
		set_matrix_t(P_HL_DT, dsg_lf.m_HL_dT); //HL_dT);
		set_matrix_t(P_HL_W, dsg_lf.m_HL_W); //HL_W);
		set_matrix_t(P_D_2, dsg_lf.m_D_2);
		set_matrix_t(P_D_3, dsg_lf.m_D_3);
		set_matrix_t(P_D_4, dsg_lf.m_D_4);
		set_matrix_t(P_D_5, dsg_lf.m_D_5);
		set_matrix_t(P_D_p, dsg_lf.m_D_p);
		set_matrix_t(P_ROUGH, dsg_lf.m_Rough); //Rough);
		set_matrix_t(P_FLOW_TYPE, dsg_lf.m_Flow_type); //Flow_type);
		set_matrix_t(P_ABSORBER_MAT, dsg_lf.m_AbsorberMaterial_in); // double check ooooooo
		//dsg_lf.m_AbsorberMaterial_in = as_matrix("AbsorberMaterial");
		set_matrix_t(P_HCE_FIELDFRAC, dsg_lf.m_HCE_FieldFrac);//HCE_FieldFrac);
		set_matrix_t(P_ALPHA_ABS, dsg_lf.m_alpha_abs); //alpha_abs);
		set_matrix_t(P_B_EPS_HCE1, dsg_lf.m_b_eps_HCE1); //b_eps_HCE1);
		set_matrix_t(P_B_EPS_HCE2, dsg_lf.m_b_eps_HCE2); //b_eps_HCE2);
		set_matrix_t(P_B_EPS_HCE3, dsg_lf.m_b_eps_HCE3); //b_eps_HCE3);
		set_matrix_t(P_B_EPS_HCE4, dsg_lf.m_b_eps_HCE4); //b_eps_HCE4);
		if (dsg_lf.m_is_multgeom != 0)
		{
			set_matrix_t(P_SH_EPS_HCE1, dsg_lf.m_sh_eps_HCE1); //s_eps_HCE1);
			set_matrix_t(P_SH_EPS_HCE2, dsg_lf.m_sh_eps_HCE2); //s_eps_HCE2);
			set_matrix_t(P_SH_EPS_HCE3, dsg_lf.m_sh_eps_HCE3); //s_eps_HCE3);
			set_matrix_t(P_SH_EPS_HCE4, dsg_lf.m_sh_eps_HCE4); //s_eps_HCE4);
		}

		set_matrix_t(P_ALPHA_ENV, dsg_lf.m_alpha_env); //alpha_env); [-] Envelope absorptance
		set_matrix_t(P_EPSILON_4, dsg_lf.m_EPSILON_4); //EPSILON_4); [-] Inner glass envelope emissivities (Pyrex)
		set_matrix_t(P_TAU_ENVELOPE, dsg_lf.m_Tau_envelope); //Tau_envelope); [-] Envelope transmittance
		set_matrix_t(P_GLAZINGINTACTIN, dsg_lf.m_GlazingIntactIn); //GlazingIntactIn); [-] Is the glazing intact?
		set_matrix_t(P_ANNULUSGAS, dsg_lf.m_AnnulusGas_in); //AnnulusGas); double check ooooooooooooooo
		set_matrix_t(P_P_A, dsg_lf.m_P_a); //P_a); [torr] Annulus gas pressure 
		set_matrix_t(P_DESIGN_LOSS, dsg_lf.m_Design_loss); //Design_loss); [W/m] Receiver heat loss at design
		set_matrix_t(P_SHADOWING, dsg_lf.m_Shadowing); //Shadowing); [-] Receiver bellows shadowing loss factor
		set_matrix_t(P_DIRT_HCE, dsg_lf.m_Dirt_HCE); //Dirt_HCE); [-] Loss due to dirt on the receiver envelope
		set_matrix_t(P_B_OPTICALTABLE, dsg_lf.m_b_OpticalTable); // opt_data); [-] Boiler Optical Table double check oooooooooooo
		set_matrix_t(P_SH_OPTICALTABLE, dsg_lf.m_sh_OpticalTable); // opt_data); double check oooooooooooo

		//updated parameters for saturated steam generation
		//dsg_lf.m_is_multgeom = false; // single geometry  is used
		//dsg_lf.m_is_sh = false; // no superheater

		C_csp_collector_receiver::S_csp_cr_solved_params solved_params;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
			//init_inputs.m_latitude = value(P_LATITUDE);		//[deg] Site latitude read from weather file
			//init_inputs.m_longitude = value(P_LONGITUDE);	//[deg] Site longitude read from weather file
			//init_inputs.m_shift = value(P_SHIFT);			//[deg]
			dsg_lf.init(init_inputs, solved_params);
		}

		catch (C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while (dsg_lf.mc_csp_messages.get_message(&out_type, &out_msg))
			{
				if (out_type == C_csp_messages::NOTICE)
					message(TCS_NOTICE, out_msg.c_str());
				else if (out_type == C_csp_messages::WARNING)
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while (dsg_lf.mc_csp_messages.get_message(&out_type, &out_msg))
		{
			if (out_type == C_csp_messages::NOTICE)
				message(TCS_NOTICE, out_msg.c_str());
			else if (out_type == C_csp_messages::WARNING)
				message(TCS_WARNING, out_msg.c_str());
		}

		// Write Calculated Design Parameters
		value(PO_A_APER_TOT, solved_params.m_A_aper_total);	//[m^2] Total solar field aperture area

		return 0;
	} // init

	virtual int call( double time, double step, int ncall )
	{		
		
		dsg_weather.m_beam = value(I_I_BN);		//[W/m^2] Direct normal incident solar irradiation
		dsg_weather.m_tdry = value(I_T_DB);		//[C] Dry bulb air temperature
		dsg_weather.m_wspd = value(I_V_WIND);	//[m/s] Ambient windspeed
		dsg_weather.m_pres = value(I_P_AMB);		//[mbar] Ambient pressure
		dsg_weather.m_tdew = value(I_T_DP);		//[C] The dewpoint temperature
		dsg_htf_state_in.m_temp = value(I_T_PB_OUT);			//[C] HTF return temperature
		//dsg_htf_state_in.m_m_dot = value(I_M_DOT_IN) / 3600.0;		//[kg/s] HTF mass flow rate at the inlet, convert from kg/hr
		//dsg_inputs.m_field_control = value(I_DEFOCUS);			//[-] Defocus control
		dsg_weather.m_solazi = value(I_SOLARAZ);	//[deg] Solar azimuth angle reported by the Type15 weather file
		dsg_weather.m_shift = value(I_SHIFT);
		dsg_weather.m_solzen = value(I_SOLARZEN);
		dsg_sim_info.m_tou = (int)value(I_TOUPERIOD); 

		dsg_sim_info.ms_ts.m_time = time;
		dsg_sim_info.ms_ts.m_step = step;

		dsg_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::ON;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			dsg_lf.call(dsg_weather,
				dsg_htf_state_in,
				dsg_inputs,
				dsg_out_solver,
				dsg_sim_info);
		}
		catch (C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while (dsg_lf.mc_csp_messages.get_message(&out_type, &out_msg))
			{
				if (out_type == C_csp_messages::NOTICE)
					message(TCS_NOTICE, out_msg.c_str());
				else if (out_type == C_csp_messages::WARNING)
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}
	
		// Part-load control is always 2
		double cycle_pl_control = 2.0;

		// Set Outputs

		value(O_CYCLE_PL_CONTROL, cycle_pl_control);					//[none] Part-load control flag - used by Type224
		value(O_T_FIELD_OUT, dsg_out_solver.m_T_salt_hot);				//[C] HTF Temperature from the field
		value(O_M_DOT_TO_PB, dsg_out_solver.m_m_dot_salt_tot);			//[kg/hr] Flow rate delivered to the power block         
		value(O_STANDBY_CONTROL, dsg_out_solver.m_standby_control);		//[none] Standby control flag - used by Type224
		value(O_DP_SF_SH, dsg_out_solver.m_dP_sf_sh);					//[bar] Pressure drop across the solar field superheater
		value(O_W_DOT_PAR_TOT, dsg_out_solver.m_W_dot_col_tracking + 
									dsg_out_solver.m_W_dot_htf_pump);	//[MW] Total parasitic power losses

		value(O_DP_TOT, 0.0);							  //[bar] Total HTF pressure drop
		value(O_DP_HDR_C, 0.0);						  //[bar] Average cold header pressure drop
		value(O_DP_SF_BOIL, 0.0);					  //[bar] Pressure drop across the solar field boiler
		value(O_DP_BOIL_TO_SH, 0.0);			  //[bar] Pressure drop between the boiler and superheater
		value(O_DP_HDR_H, 0.0);						  //[bar] Average hot header pressure drop
		value(O_E_BAL_STARTUP, 0.0);			  //[MW] Startup energy consumed
		value(O_E_FIELD, 0.0);						  //[MW-hr] Accumulated internal energy in the entire solar field
		value(O_E_FP_TOT, 0.0);							  //[J] Freeze protection energy
		value(O_ETA_OPT_AVE, 0.0);				  //[none] Collector equivalent optical efficiency
		value(O_ETA_THERMAL, 0.0);				  //[none] Solar field thermal efficiency (power out/ANI)
		value(O_ETA_SF, 0.0);    						  //[none] Total solar field collection efficiency
		value(O_DEFOCUS, 0.0);     					  //[none] The fraction of focused aperture area in the solar field
		value(O_M_DOT_AUX, 0.0);				  //[kg/s] --> [kg/hr] Auxiliary heater mass flow rate
		value(O_M_DOT_FIELD, 0.0);			  //[kg/s] --> [kg/hr] Flow rate from the field
		value(O_M_DOT_B_TOT, 0.0);			  //[kg/s] --> [kg/hr] Flow rate within the boiler section
		value(O_M_DOT, 0.0);							  //[kg/s] Flow rate in a single loop
		value(O_P_TURB_IN, 0.0);			               //[bar] Pressure at the turbine inlet		
		value(O_Q_LOSS_PIPING, 0.0);				   //[MW] Pipe heat loss in the hot header and the hot runner
		value(O_Q_AUX_FLUID, 0.0);					   //[MW] Thermal energy provided to the fluid passing through the aux heater
		value(O_Q_AUX_FUEL, 0.0);				   //[W] --> [MMBTU] Heat content of fuel required to provide aux firing
		value(O_Q_DUMP, 0.0);								   //[MW] Dumped thermal energy
		value(O_Q_FIELD_DELIVERED, 0.0);	   //[kW] --> [MW] Total solar field thermal power delivered
		value(O_Q_INC_TOT, 0.0);						   //[MW] Total power incident on the field
		value(O_Q_LOSS_REC, 0.0);						   //[MW] Total Receiver thermal losses
		value(O_Q_LOSS_SF, 0.0);						   //[MW] Total solar field thermal losses
		value(O_Q_TO_PB, 0.0);						   //[kW] --> [MW] Thermal energy to the power block
		value(O_SOLARALT, 0.0);					   //[rad] --> [deg] Solar altitude used in optical calculations
		value(O_SOLARAZ, 0.0);					   //[rad] --> [deg] Solar azimuth used in optical calculations
		value(O_PHI_T, 0.0);						   //[rad] --> [deg] Transversal solar incidence angle
		value(O_THETA_L, 0.0);					   //[rad] --> [deg] Longitudinal solar incidence angle
		value(O_T_FIELD_IN, 0.0);						   //[C] HTF temperature into the collector field header
		value(O_T_LOOP_OUT, 0.0);						   //[C] Loop outlet temperature
		value(O_T_PB_IN, 0.0);							   //[C] HTF Temperature to the power block
		value(O_W_DOT_AUX, 0.0);						   //[MW] Parasitic power associated with operation of the aux boiler
		value(O_W_DOT_BOP, 0.0);						   //[MW] parasitic power as a function of power block load
		value(O_W_DOT_COL, 0.0);						   //[MW] Parasitic electric power consumed by the collectors
		value(O_W_DOT_FIXED, 0.0);					   //[MW] Fixed parasitic power losses.. for every hour of operation
		value(O_W_DOT_PUMP, 0.0);						   //[MW] Required solar field pumping power
		value(O_P_SF_IN, 0.0);						//[bar] Solar field inlet pressure


		////value( O_CYCLE_PL_CONTROL, cycle_pl_control );        //[none] Part-load control flag - used by Type224
		//value( O_DP_TOT, dP_tot );							  //[bar] Total HTF pressure drop
		//value( O_DP_HDR_C, dP_hdr_c );						  //[bar] Average cold header pressure drop
		//value( O_DP_SF_BOIL, dP_sf_boil );					  //[bar] Pressure drop across the solar field boiler
		//value( O_DP_BOIL_TO_SH, dP_boil_to_SH );			  //[bar] Pressure drop between the boiler and superheater
		////value( O_DP_SF_SH, dP_sf_sh );						  //[bar] Pressure drop across the solar field superheater
		//value( O_DP_HDR_H, dP_hdr_h );						  //[bar] Average hot header pressure drop
		//value( O_E_BAL_STARTUP, E_bal_startup );			  //[MW] Startup energy consumed
		//value( O_E_FIELD, E_field );						  //[MW-hr] Accumulated internal energy in the entire solar field
		//value( O_E_FP_TOT, 0.0 );							  //[J] Freeze protection energy
		//value( O_ETA_OPT_AVE, eta_opt_ave );				  //[none] Collector equivalent optical efficiency
		//value( O_ETA_THERMAL, eta_thermal );				  //[none] Solar field thermal efficiency (power out/ANI)
		//value( O_ETA_SF, eta_sf );    						  //[none] Total solar field collection efficiency
		//value( O_DEFOCUS, m_defocus );     					  //[none] The fraction of focused aperture area in the solar field
		//value( O_M_DOT_AUX, m_dot_aux*3600 );				  //[kg/s] --> [kg/hr] Auxiliary heater mass flow rate
		//value( O_M_DOT_FIELD, m_dot_field*3600 );			  //[kg/s] --> [kg/hr] Flow rate from the field
		//value( O_M_DOT_B_TOT, m_dot_b_tot*3600 );			  //[kg/s] --> [kg/hr] Flow rate within the boiler section
		//value( O_M_DOT,	m_dot );							  //[kg/s] Flow rate in a single loop
		////value( O_M_DOT_TO_PB, m_dot_to_pb*3600 );			  //[kg/s] --> [kg/hr] Flow rate delivered to the power block         
		//value( O_P_TURB_IN, P_turb_in );			               //[bar] Pressure at the turbine inlet		
		//value( O_Q_LOSS_PIPING, q_loss_piping );				   //[MW] Pipe heat loss in the hot header and the hot runner
		//value( O_Q_AUX_FLUID, q_aux*0.001 );					   //[MW] Thermal energy provided to the fluid passing through the aux heater
		//value( O_Q_AUX_FUEL, q_aux_fuel*3.412E-3 );				   //[W] --> [MMBTU] Heat content of fuel required to provide aux firing
		//value( O_Q_DUMP, q_dump );								   //[MW] Dumped thermal energy
		//value( O_Q_FIELD_DELIVERED, q_field_delivered*0.001 );	   //[kW] --> [MW] Total solar field thermal power delivered
		//value( O_Q_INC_TOT, q_inc_tot );						   //[MW] Total power incident on the field
		//value( O_Q_LOSS_REC, q_loss_rec );						   //[MW] Total Receiver thermal losses
		//value( O_Q_LOSS_SF, q_loss_sf );						   //[MW] Total solar field thermal losses
		//value( O_Q_TO_PB, q_to_pb*0.001 );						   //[kW] --> [MW] Thermal energy to the power block
		//value( O_SOLARALT, solaralt*57.2958 );					   //[rad] --> [deg] Solar altitude used in optical calculations
		//value( O_SOLARAZ, SolarAz*57.2958 );					   //[rad] --> [deg] Solar azimuth used in optical calculations
		//value( O_PHI_T, phi_t*57.2958 );						   //[rad] --> [deg] Transversal solar incidence angle
		//value( O_THETA_L, theta_L*57.2958 );					   //[rad] --> [deg] Longitudinal solar incidence angle
		////value( O_STANDBY_CONTROL, standby_control );			   //[none] Standby control flag - used by Type224
		//value( O_T_FIELD_IN, m_T_field_in );						   //[C] HTF temperature into the collector field header
		////value(O_T_FIELD_OUT, m_T_field_out);					   //[C] HTF Temperature from the field
		//value(O_T_LOOP_OUT, T_loop_out);						   //[C] Loop outlet temperature
		//value( O_T_PB_IN, T_pb_in );							   //[C] HTF Temperature to the power block
		//value( O_W_DOT_AUX,	W_dot_aux );						   //[MW] Parasitic power associated with operation of the aux boiler
		//value( O_W_DOT_BOP, W_dot_bop );						   //[MW] parasitic power as a function of power block load
		//value( O_W_DOT_COL,	W_dot_col );						   //[MW] Parasitic electric power consumed by the collectors
		//value( O_W_DOT_FIXED, W_dot_fixed );					   //[MW] Fixed parasitic power losses.. for every hour of operation
		//value( O_W_DOT_PUMP, W_dot_pump );						   //[MW] Required solar field pumping power
		////value( O_W_DOT_PAR_TOT, W_dot_aux+W_dot_bop+W_dot_col+W_dot_fixed+W_dot_pump );	//[MW] Total parasitic power losses
		//value( O_P_SF_IN, P_turb_in + dP_tot );						//[bar] Solar field inlet pressure

		return 0;
	}

	virtual int converged(double time)
	{
		int out_type = -1;
		std::string out_msg = "";

		try
		{
			dsg_lf.converged();
		}

		catch (C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while (dsg_lf.mc_csp_messages.get_message(&out_type, &out_msg))
			{
				if (out_type == C_csp_messages::NOTICE)
					message(TCS_NOTICE, out_msg.c_str());
				else if (out_type == C_csp_messages::WARNING)
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while (dsg_lf.mc_csp_messages.get_message(&out_type, &out_msg))
		{
			if (out_type == C_csp_messages::NOTICE)
				message(TCS_NOTICE, out_msg.c_str());
			else if (out_type == C_csp_messages::WARNING)
				message(TCS_WARNING, out_msg.c_str());
		}

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_lf_type261_steam, "Linear Fresnel Steam Receiver", "Ty Neises", 1, sam_mw_lf_type261_steam_variables, NULL, 1 )

