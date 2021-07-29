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
#include "sam_csp_util.h"
//#include "waterprop.h"
#include "water_properties.h"

using namespace std;

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
	// Class Instances
	emit_table eps_abs;
	OpticalDataTable b_optical_table;
	OpticalDataTable sh_optical_table;
	TwoOptTables optical_tables;
	P_max_check check_pressure;
	enth_lim check_h;
	water_state wp;
	Evacuated_Receiver evac_tube_model;
	HTFProperties htfProps;

	// Parameters
	double m_tes_hours;         
	double m_q_max_aux;         
	double m_LHV_eff;			
	double m_T_set_aux;		
	double m_T_field_in_des;	
	double m_T_field_out_des;	
	double m_x_b_des;			
	double m_P_turb_des;		
	double m_fP_hdr_c;			
	double m_fP_sf_boil;		
	double m_fP_boil_to_sh;	
	double m_fP_sf_sh;			
	double m_fP_hdr_h;			
	double m_q_pb_des;			
	double m_W_pb_des;			
	double m_cycle_max_fraction;
	double m_cycle_cutoff_frac; 
	double m_t_sby_des;			
	double m_q_sby_frac;		
	double m_solarm;			
	double m_PB_pump_coef;	
	double m_PB_fixed_par;
	double * m_bop_array;         
	   int m_l_bop_array;
	double * m_aux_array; 
	   int m_l_aux_array;
	double m_T_startup;         

	int m_fossil_mode;      
	double m_I_bn_des;			
	bool m_is_sh;			
	double m_is_oncethru;		
	bool m_is_multgeom;		
	int m_nModBoil;			
	int m_nModSH;			
	int m_nLoops;			
	double m_eta_pump;			
	double m_latitude;			
	double m_theta_stow;		
	double m_theta_dep;		
	double m_m_dot_min;		
	double m_T_field_ini;		
	double m_T_fp;				
	double m_Pipe_hl_coef;		
	double m_SCA_drives_elec;	
	double m_ColAz;			
	double m_e_startup;        
	double m_T_amb_des_sf;	
	double m_V_wind_max;		
	double * m_ffrac;			
	int    m_l_ffrac;

	util::matrix_t<double> m_A_aperture;	
	util::matrix_t<double> m_L_col;			
	util::matrix_t<double> m_OptCharType;		
	util::matrix_t<double> m_IAM_T;            
	util::matrix_t<double> m_IAM_L;            
	util::matrix_t<double> m_TrackingError;    
	util::matrix_t<double> m_GeomEffects;		
	util::matrix_t<double> m_rho_mirror_clean;	
	util::matrix_t<double> m_dirt_mirror;		
	util::matrix_t<double> m_error;            
	util::matrix_t<double> m_HLCharType;		
	util::matrix_t<double> m_HL_dT;            
	util::matrix_t<double> m_HL_W;             
	util::matrix_t<double> m_D_2;              
	util::matrix_t<double> m_D_3;				
	util::matrix_t<double> m_D_4;				
	util::matrix_t<double> m_D_5;				
	util::matrix_t<double> m_D_p;				
	util::matrix_t<double> m_Rough;			
	util::matrix_t<double> m_Flow_type;		
	util::matrix_t<AbsorberProps*> m_AbsorberMaterial;	
	util::matrix_t<double> m_HCE_FieldFrac;    
	util::matrix_t<double> m_alpha_abs;		

	util::matrix_t<double> m_alpha_env;		
	util::matrix_t<double> m_EPSILON_4;		
	util::matrix_t<double> m_Tau_envelope;		
	util::matrix_t<bool> m_GlazingIntactIn;	
	util::matrix_t<HTFProperties*> m_AnnulusGas;		
	util::matrix_t<double> m_P_a;				
	util::matrix_t<double> m_Design_loss;		
	util::matrix_t<double> m_Shadowing;		
	util::matrix_t<double> m_Dirt_HCE;
	util::matrix_t<double> m_b_OpticalTable;
	util::matrix_t<double> m_sh_OpticalTable;

	// Stored Variables
	util::matrix_t<double> m_T_ave, m_T_ave0, m_h_ave, m_h_ave0;

	// Constants
	double m_P_max;
	double m_fP_turb_min;
	double m_d2r;
	
	// Calculated
	util::matrix_t<double> m_D_h;
	util::matrix_t<double> m_A_cs;
	util::matrix_t<double> m_EPSILON_5;
	util::matrix_t<double> m_q_inc, m_q_loss, m_q_abs, m_h_in, m_h_out, m_x, m_q_rec;
	util::matrix_t<double> m_eta_opt_fixed, m_opteff_des;
	double m_fP_sf_tot;
	int m_n_rows_matrix;
	int m_nModTot;
	double m_Ap_tot;
	double m_m_dot_des;    
	double m_q_rec_tot_des;
	double m_m_dot_max;
	double m_m_dot_pb_des;
	double m_e_trans;
	double m_m_dot_b_max;
	double m_m_dot_b_des;

	// Stored
	util::matrix_t<double> m_T_ave_prev;
	double m_defocus_prev;
	double m_t_sby_prev;
	double m_t_sby;
	bool   m_is_pb_on_prev;
	bool   m_is_pb_on;
	double m_T_sys_prev;
	double m_T_field_in;
	double m_T_field_out;

	// Calculated - ncall = 0
	util::matrix_t<double> m_eta_optical;
	double m_defocus;
	bool m_is_def;	
	double m_err_def;	
	double m_tol_def;	
	double m_rc;
	double phi_t;
	double theta_L;
	double m_ftrack;

public:
	sam_mw_lf_type261_steam( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		// Parameters
		m_tes_hours = std::numeric_limits<double>::quiet_NaN();         
		m_q_max_aux = std::numeric_limits<double>::quiet_NaN();         
		m_LHV_eff = std::numeric_limits<double>::quiet_NaN();			
		m_T_set_aux = std::numeric_limits<double>::quiet_NaN();		
		
		m_T_field_in_des = std::numeric_limits<double>::quiet_NaN();	
		m_T_field_out_des = std::numeric_limits<double>::quiet_NaN();	
		m_x_b_des = std::numeric_limits<double>::quiet_NaN();			
		m_P_turb_des = std::numeric_limits<double>::quiet_NaN();		
		m_fP_hdr_c = std::numeric_limits<double>::quiet_NaN();			
		m_fP_sf_boil = std::numeric_limits<double>::quiet_NaN();		
		m_fP_boil_to_sh = std::numeric_limits<double>::quiet_NaN();	
		m_fP_sf_sh = std::numeric_limits<double>::quiet_NaN();			
		m_fP_hdr_h = std::numeric_limits<double>::quiet_NaN();			
		m_q_pb_des = std::numeric_limits<double>::quiet_NaN();			
		m_W_pb_des = std::numeric_limits<double>::quiet_NaN();			
		m_cycle_max_fraction = std::numeric_limits<double>::quiet_NaN();
		m_cycle_cutoff_frac = std::numeric_limits<double>::quiet_NaN(); 
		m_t_sby_des = std::numeric_limits<double>::quiet_NaN();			
		m_q_sby_frac = std::numeric_limits<double>::quiet_NaN();		
		m_solarm = std::numeric_limits<double>::quiet_NaN();			
		m_PB_pump_coef = std::numeric_limits<double>::quiet_NaN();	
		m_PB_fixed_par = std::numeric_limits<double>::quiet_NaN();	
		m_bop_array = 0;       
		m_l_bop_array = -1;
		m_aux_array = 0; 
		m_l_aux_array = -1;
		m_T_startup = std::numeric_limits<double>::quiet_NaN();         

		m_fossil_mode = -1;    
		m_I_bn_des = std::numeric_limits<double>::quiet_NaN();		
		m_is_sh = false;			
		m_is_oncethru = std::numeric_limits<double>::quiet_NaN();		
		m_is_multgeom = false;		
		m_nModBoil = -1;		
		m_nModSH = -1;			
		m_nLoops = -1;			
		m_eta_pump = std::numeric_limits<double>::quiet_NaN();		
		m_latitude = std::numeric_limits<double>::quiet_NaN();		
		m_theta_stow = std::numeric_limits<double>::quiet_NaN();		
		m_theta_dep = std::numeric_limits<double>::quiet_NaN();		
		m_m_dot_min = std::numeric_limits<double>::quiet_NaN();		
		m_T_field_ini = std::numeric_limits<double>::quiet_NaN();		
		m_T_fp = std::numeric_limits<double>::quiet_NaN();			
		m_Pipe_hl_coef = std::numeric_limits<double>::quiet_NaN();	
		m_SCA_drives_elec = std::numeric_limits<double>::quiet_NaN();
		m_ColAz = std::numeric_limits<double>::quiet_NaN();			
		m_e_startup = std::numeric_limits<double>::quiet_NaN();      
		m_T_amb_des_sf = std::numeric_limits<double>::quiet_NaN();	
		m_V_wind_max = std::numeric_limits<double>::quiet_NaN();		
		m_ffrac = 0;
		m_l_ffrac = -1;

		// Calculated
		m_fP_sf_tot = std::numeric_limits<double>::quiet_NaN();
		m_nModTot = -1;
		m_Ap_tot = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_des     = std::numeric_limits<double>::quiet_NaN();
		m_q_rec_tot_des = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_max     = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_pb_des  = std::numeric_limits<double>::quiet_NaN();
		m_e_trans       = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_b_max   = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_b_des   = std::numeric_limits<double>::quiet_NaN();


		// Constants
		m_P_max = std::numeric_limits<double>::quiet_NaN();
		m_fP_turb_min = std::numeric_limits<double>::quiet_NaN();
		m_d2r = CSP::pi/180.0;

		// Stored
		m_defocus_prev = std::numeric_limits<double>::quiet_NaN();
		m_t_sby_prev = std::numeric_limits<double>::quiet_NaN();
		m_t_sby = std::numeric_limits<double>::quiet_NaN();
		m_is_pb_on_prev = false;
		m_is_pb_on = false;
		m_T_sys_prev = std::numeric_limits<double>::quiet_NaN();
		m_T_field_in = std::numeric_limits<double>::quiet_NaN();
		m_T_field_out = std::numeric_limits<double>::quiet_NaN();

		// Calculated ncall = 0
		m_defocus = std::numeric_limits<double>::quiet_NaN();
		m_is_def = true;
		m_err_def = std::numeric_limits<double>::quiet_NaN();
		m_tol_def = std::numeric_limits<double>::quiet_NaN();
		m_rc = std::numeric_limits<double>::quiet_NaN();
		phi_t = std::numeric_limits<double>::quiet_NaN();
		theta_L = std::numeric_limits<double>::quiet_NaN();
		m_ftrack = std::numeric_limits<double>::quiet_NaN();

	}

	virtual ~sam_mw_lf_type261_steam()
	{
		try
		{
			// Set up matrix_t of pointers to HTFproperties class
			for( int i = 0; i < m_n_rows_matrix; i++ )
			{
				for( int j = 0; j < 4; j++ )
				{
					delete m_AnnulusGas.at(i,j);
				}
			}
		}
		catch(...){};

		try
		{
			for( int i = 0; i < m_n_rows_matrix; i++ )
			{
				delete m_AbsorberMaterial.at(i,0);
			}
		}
		catch(...){};
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
		default:
			return 0.0;
		}
	}

	virtual int init()
	{

		m_P_max = 190.0;		// [bar]
		
		// Read in parameters
		m_tes_hours = value( P_TESHOURS );                          //[hr]Equivalent full-load thermal storage hours
		m_q_max_aux = value( P_Q_MAX_AUX ) *1.E3;                   //[kW] Maximum heat rate of the auxiliary heater
		m_LHV_eff = value( P_LHV_EFF ); 			                  //[none] Fuel LHV efficiency (0..1)
		m_T_set_aux = value( P_T_SET_AUX )+273.15;		          //[K] Aux heater outlet temperature set point, convert from C
		m_T_field_in_des = value( P_T_FIELD_IN_DES )+273.15;	      //[K] Field design inlet temperature, convert from C 
		m_T_field_out_des = value( P_T_FIELD_OUT_DES )+273.15;	  //[K] Field loop outlet design temperature, convert from C
		m_x_b_des = value( P_X_B_DES );			                  //[none] Design point boiler outlet steam quality
		m_P_turb_des = value( P_P_TURB_DES );		                  //[bar] Design-point turbine inlet pressure
		m_fP_hdr_c = value( P_FP_HDR_C );			                  //[none] Average design-point cold header pressure drop fraction
		m_fP_sf_boil = value( P_FP_SF_BOIL );		                  //[none] Design-point pressure drop across the solar field boiler fraction
		m_fP_boil_to_sh = value( P_FP_BOIL_TO_SH );	              //[none] Design-point pressure drop between the boiler and superheater frac
		m_fP_sf_sh = value( P_FP_SF_SH ); 			              //[none] Design-point pressure drop across the solar field superheater fraction
		m_fP_hdr_h = value( P_FP_HDR_H );			                  //[none] Average design-point hot header pressure drop fraction
		m_q_pb_des = value( P_Q_PB_DES )*1000.0;			          //[kW] Design heat input to the power block, convert from MW
		m_W_pb_des = value( P_W_PB_DES )*1000.0;			          //[kW] Rated plant capacity, convert from MW
		m_cycle_max_fraction = value( P_CYCLE_MAX_FRAC );           //[none] Maximum turbine over design operation fraction
		m_cycle_cutoff_frac = value( P_CYCLE_CUTOFF_FRAC );         //[none] Minimum turbine operation fraction before shutdown
		m_t_sby_des = value( P_T_SBY );			                      //[hr] Low resource standby period
		m_q_sby_frac = value( P_Q_SBY_FRAC );		                  //[none] Fraction of thermal power required for standby
		m_solarm = value( P_SOLARM );			                      //[none] Solar multiple
		m_PB_pump_coef = value( P_PB_PUMP_COEF );		              //[kW/kg] Pumping power required to move 1kg of HTF through power block flow loop
		m_PB_fixed_par = value( P_PB_FIXED_PAR );                   //[none] fraction of rated gross power consumed at all hours of the year

		m_bop_array = value( P_BOP_ARRAY, &m_l_bop_array );			  //[MW/MWcap] Balance of plant parasitic (parasitic, multiplier, constant, linear, quadratic)
		m_aux_array = value( P_AUX_ARRAY, &m_l_aux_array );		      //[Mw/MWcap] Aux heater/boiler parasitic (parasitic, multiplier, constant, linear, quadratic)

		m_T_startup = value( P_T_STARTUP ) + 273.15;				  //[C] Startup temperature (same as field startup)
		m_fossil_mode = (int) value( P_FOSSIL_MODE );				// [none] Operation mode for the fossil backup {1=Normal.. 2=supp.. 3=topping}
		m_I_bn_des = value( P_I_BN_DES );							// [W/m2] Design point irradiation value
		m_is_sh = ( value( P_IS_SH ) > 0);							// [-] Does the solar field include a superheating section
		m_is_oncethru = ( value( P_IS_ONCETHRU ) > 0);				// [-] Flag indicating whether flow is once through with superheat
		m_is_multgeom = ( value( P_IS_MULTGEOM ) > 0);				// [-] Does the superheater have a different geometry from the boiler {1=yes}?
		m_nModBoil = (int) value( P_NMODBOIL );						// [none] Number of modules in the boiler section
		m_nModSH = (int) value( P_NMODSH );							// [none] Number of modules in the superheater section
		m_nLoops = (int) value( P_NLOOPS );							// [none] Number of loops 
		m_eta_pump = value( P_ETA_PUMP );							// [none] Feedwater pump efficiency
		m_latitude = value( P_LATITUDE )*0.0174533;					// [rad] Site latitude read from weather file, convert from [deg]
		m_theta_stow = value( P_THETA_STOW )*0.0174533;				// [rad] stow angle, convert from [deg]
		m_theta_dep = value( P_THETA_DEP )*0.0174533;				// [rad] deploy angle, convert from [deg]
		m_m_dot_min = value( P_M_DOT_MIN );							// [kg/s] Minimum loop flow rate
		m_T_field_ini = value( P_T_FIELD_INI )+273.15;				// [K] Initial field temperature, convert from [C]
		m_T_fp = value( P_T_FP )+273.15;							// [K] Freeze protection temperature (heat trace activation temperature), convert from [C]
		m_Pipe_hl_coef = value( P_PIPE_HL_COEF );					// [W/m2-K] Loss coefficient from the header.. runner pipe.. and non-HCE piping
		m_SCA_drives_elec = value( P_SCA_DRIVES_ELEC );				// [W/m2] Tracking power.. in Watts per SCA drive
		m_ColAz = value( P_COLAZ )*0.0174533;						// [rad] Collector azimuth angle, convert from [deg]
		m_e_startup = value( P_E_STARTUP );							// [kJ/K-m2] Thermal inertia contribution per sq meter of solar field
		m_T_amb_des_sf = value( P_T_AMB_DES_SF )+273.15;			// [K] Design-point ambient temperature, convert from [C]
		m_V_wind_max = value( P_V_WIND_MAX );						// [m/s] Maximum allowable wind velocity before safety stow

		m_ffrac = value( P_FFRAC, &m_l_ffrac );

		m_n_rows_matrix = 1;
		if( m_is_multgeom )
			m_n_rows_matrix = 2;

		// *****************************************
		// Size matrix_t that are used in main call
		// *****************************************
		m_eta_optical.resize(m_n_rows_matrix,1);
		// ****************************************

		int n_rows = 0, n_cols = 0;
		double *p_matrix_t = value( P_A_APERTURE, &n_rows, &n_cols );
		m_A_aperture.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_A_aperture.at(r,c) = TCS_MATRIX_INDEX( var( P_A_APERTURE ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Aperature area matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_L_COL, &n_rows, &n_cols );
		m_L_col.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_L_col.at(r,c) = TCS_MATRIX_INDEX( var( P_L_COL ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Collector length matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_OPTCHARTYPE, &n_rows, &n_cols );
		m_OptCharType.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_OptCharType.at(r,c) = TCS_MATRIX_INDEX( var( P_OPTCHARTYPE ), r, c );
		}
		else
		{
			message(TCS_ERROR,  "Optical char. method matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_IAM_T, &n_rows, &n_cols );
		m_IAM_T.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 5 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_IAM_T.at(r,c) = TCS_MATRIX_INDEX( var( P_IAM_T ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Transverse IAM matrix should have %d rows (b,SH) and 5 columns (0 - 4th order terms) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_IAM_L, &n_rows, &n_cols );
		m_IAM_L.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 5 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_IAM_L.at(r,c) = TCS_MATRIX_INDEX( var( P_IAM_L ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Longitudinal IAM matrix should have %d rows (b,SH) and 5 columns (0 - 4th order terms) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_TRACKINGERROR, &n_rows, &n_cols );
		m_TrackingError.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_TrackingError.at(r,c) = TCS_MATRIX_INDEX( var( P_TRACKINGERROR ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Tracking error derate matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_GEOMEFFECTS, &n_rows, &n_cols );
		m_GeomEffects.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_GeomEffects.at(r,c) = TCS_MATRIX_INDEX( var( P_GEOMEFFECTS ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Geometry effects derate matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_RHO_MIRROR_CLEAN, &n_rows, &n_cols);
		m_rho_mirror_clean.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_rho_mirror_clean.at(r,c) = TCS_MATRIX_INDEX( var( P_RHO_MIRROR_CLEAN ), r, c);
		}
		else
		{
			message( TCS_ERROR, "Clean mirror reflectivity matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_DIRT_MIRROR, &n_rows, &n_cols );
		m_dirt_mirror.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_dirt_mirror.at(r,c) = TCS_MATRIX_INDEX( var( P_DIRT_MIRROR ), r, c );
		}
		else
		{
			message(TCS_ERROR,  "Dirt on mirror derate matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_ERROR, &n_rows, &n_cols );
		m_error.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_error.at(r,c) = TCS_MATRIX_INDEX( var( P_ERROR ), r, c );
		}
		else
		{
			message( TCS_ERROR, "General optical error derate matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_HLCHARTYPE, &n_rows, &n_cols );
		m_HLCharType.resize( n_rows, n_cols);
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_HLCharType.at(r,c) = TCS_MATRIX_INDEX( var( P_HLCHARTYPE ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Heat loss model type matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_HL_DT, &n_rows, &n_cols );
		m_HL_dT.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 5 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_HL_dT.at(r,c) = TCS_MATRIX_INDEX( var( P_HL_DT ), r, c );
		}
		else
		{
			message( TCS_ERROR, "HTF temperature heat loss coef. matrix should have %d rows (b,SH) and 5 columns (0-4th order terms) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_HL_W, &n_rows, &n_cols );
		m_HL_W.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 5 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_HL_W.at(r,c) = TCS_MATRIX_INDEX( var( P_HL_W ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Wind speed heat loss coef. matrix should have %d rows (b,SH) and 5 columns (0-4th order terms) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_D_2, &n_rows, &n_cols );
		m_D_2.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_D_2.at(r,c) = TCS_MATRIX_INDEX( var( P_D_2 ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Inner absorber tube diameter matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_D_3, &n_rows, &n_cols );
		m_D_3.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_D_3.at(r,c) = TCS_MATRIX_INDEX( var( P_D_3 ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Outer absorber tube diameter matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_D_4, &n_rows, &n_cols );
		m_D_4.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_D_4.at(r,c) = TCS_MATRIX_INDEX( var( P_D_4 ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Inner glass envelope diameter matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_D_5, &n_rows, &n_cols );
		m_D_5.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_D_5.at(r,c) = TCS_MATRIX_INDEX( var( P_D_5 ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Outer glass envelope diameter matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_D_p, &n_rows, &n_cols );
		m_D_p.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_D_p.at(r,c) = TCS_MATRIX_INDEX( var( P_D_p ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Absorber flow plug diameter matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_ROUGH, &n_rows, &n_cols );
		m_Rough.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
		{
			for( int r = 0; r < n_rows; r++ )
				for( int c = 0; c < n_cols; c++ )
					m_Rough.at(r,c) = TCS_MATRIX_INDEX( var( P_ROUGH ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Internal surface roughness matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_FLOW_TYPE, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
			m_Flow_type.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message(TCS_ERROR,  "Absorber flow-type matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		m_D_h.resize(m_n_rows_matrix,1);
		m_A_cs.resize(m_n_rows_matrix,1);
		for( int i = 0; i < m_n_rows_matrix; i++ )
		{
			if( m_Flow_type.at(i,0) == 2.0 )
				m_D_h.at(i,0) = m_D_2.at(i,0) - m_D_p.at(i,0);		// [m] The hydraulic diameter for plug flow
			else
			{
				m_D_h.at(i,0) = m_D_2.at(i,0);						// [m] The hydraulic diameter for tube flow
				m_D_p.at(i,0) = 0.0;							
			}
			m_A_cs.at(i,0) = CSP::pi*(m_D_2.at(i,0)*m_D_2.at(i,0) - m_D_p.at(i,0)*m_D_p.at(i,0))/4.0;	// [m^2] The cross-sectional flow area
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_ABSORBER_MAT, &n_rows, &n_cols );
		util::matrix_t<double> AbsorberMaterial;
		m_AbsorberMaterial.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 1 )
			AbsorberMaterial.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Absorber material type matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		for( int i = 0; i < m_n_rows_matrix; i++ )
		{
			m_AbsorberMaterial.at(i,0) = new AbsorberProps;
			m_AbsorberMaterial.at(i, 0)->setMaterial((int)AbsorberMaterial.at(i, 0));
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_HCE_FIELDFRAC, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_HCE_FieldFrac.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE field fraction matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		n_rows = n_cols = 0;
		p_matrix_t = value( P_ALPHA_ABS, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_alpha_abs.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE absorber absorptance matrix should have %d rows (b,SH) and 1 columns - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		util::matrix_t<double> b_eps_HCE1;
		n_rows = n_cols = 0;
		p_matrix_t = value( P_B_EPS_HCE1, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
		//if( p_matrix_t != 0 && n_rows > 0 && n_cols == 2 )
			b_eps_HCE1.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Boiler epsilon HCE2 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
			return -1;
		}

		util::matrix_t<double> b_eps_HCE2;
		n_rows = n_cols = 0;
		p_matrix_t = value( P_B_EPS_HCE2, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
			b_eps_HCE2.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Boiler epsilon HCE2 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
			return -1;
		}

		util::matrix_t<double> b_eps_HCE3;
		n_rows = n_cols = 0;
		p_matrix_t = value( P_B_EPS_HCE3, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
			b_eps_HCE3.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Boiler epsilon HCE3 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
			return -1;
		}

		util::matrix_t<double> b_eps_HCE4;
		n_rows = n_cols = 0;
		p_matrix_t = value( P_B_EPS_HCE4, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
			b_eps_HCE4.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Boiler epsilon HCE4 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
			return -1;
		}														

		if( m_is_multgeom )
		{
			util::matrix_t<double> sh_eps_HCE1;
			n_rows = n_cols = 0;
			p_matrix_t = value( P_SH_EPS_HCE1, &n_rows, &n_cols );
			if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
				sh_eps_HCE1.assign( p_matrix_t, n_rows, n_cols );
			else
			{
				message( TCS_ERROR, "Superheater epsilon HCE1 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
				return -1;
			}

			util::matrix_t<double> sh_eps_HCE2;
			n_rows = n_cols = 0;
			p_matrix_t = value( P_SH_EPS_HCE2, &n_rows, &n_cols );
			if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
				sh_eps_HCE2.assign( p_matrix_t, n_rows, n_cols );
			else
			{
				message( TCS_ERROR, "Superheater epsilon HCE2 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
				return -1;
			}

			util::matrix_t<double> sh_eps_HCE3;
			n_rows = n_cols = 0;
			p_matrix_t = value( P_SH_EPS_HCE3, &n_rows, &n_cols );
			if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
				sh_eps_HCE3.assign( p_matrix_t, n_rows, n_cols );
			else
			{
				message( TCS_ERROR, "Superheater epsilon HCE3 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
				return -1;
			}

			util::matrix_t<double> sh_eps_HCE4;
			n_rows = n_cols = 0;
			p_matrix_t = value( P_SH_EPS_HCE4, &n_rows, &n_cols );
			if( p_matrix_t != 0 && n_rows == 2 && n_cols > 0 )
				sh_eps_HCE4.assign( p_matrix_t, n_rows, n_cols );
			else
			{
				message( TCS_ERROR, "Superheater epsilon HCE4 matrix should have 2 rows (Temp,eps) and at least 1 column - the input matrix has %d rows and %d columns", n_rows, n_cols );
				return -1;
			}

			//[-] Organize the boiler/superheater emittance tables here
			eps_abs.init( m_n_rows_matrix, 4 );
			eps_abs.addTable(&b_eps_HCE1);
			eps_abs.addTable(&b_eps_HCE2);
			eps_abs.addTable(&b_eps_HCE3);
			eps_abs.addTable(&b_eps_HCE4);
			eps_abs.addTable(&sh_eps_HCE1);
			eps_abs.addTable(&sh_eps_HCE2);
			eps_abs.addTable(&sh_eps_HCE3);
			eps_abs.addTable(&sh_eps_HCE4);
		}
		else
		{
			// Organize the boiler emittance tables here
			eps_abs.init( m_n_rows_matrix, 4);
			eps_abs.addTable(&b_eps_HCE1);
			eps_abs.addTable(&b_eps_HCE2);
			eps_abs.addTable(&b_eps_HCE3);
			eps_abs.addTable(&b_eps_HCE4);
		}

		//[-] Envelope absorptance
		n_rows = n_cols = 0;
		p_matrix_t = value( P_ALPHA_ENV, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_alpha_env.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE envelope absorptance matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Inner glass envelope emissivities (Pyrex)
		n_rows = n_cols = 0;
		p_matrix_t = value( P_EPSILON_4, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_EPSILON_4.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE inner glass envelope absorptance matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Outer glass envelope emissivities (Pyrex)
		m_EPSILON_5 = m_EPSILON_4;

		//[-] Envelope transmittance
		n_rows = n_cols = 0;
		p_matrix_t = value( P_TAU_ENVELOPE, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_Tau_envelope.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE envelope transmittance matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Is the glazing intact?
		util::matrix_t<double> glaz_intact;
		n_rows = n_cols = 0;
		p_matrix_t = value( P_GLAZINGINTACTIN, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			glaz_intact.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE glazing intact matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}
		m_GlazingIntactIn.resize( n_rows, n_cols );
		for( int i = 0; i < n_rows; i++ )
			for( int j = 0; j < n_cols; j++ )
				m_GlazingIntactIn.at(i,j) =  (glaz_intact.at(i,j) > 0);

		//[-] Annulus gas type (1 = air; 26 = Ar; 27 = H2 )
		n_rows = n_cols = 0;
		p_matrix_t = value( P_ANNULUSGAS, &n_rows, &n_cols );
		util::matrix_t<double> AnnulusGas;
		m_AnnulusGas.resize( n_rows, n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			AnnulusGas.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE annulus gas type matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		// Set up matrix_t of pointers to HTFproperties class
		for( int i = 0; i < m_n_rows_matrix; i++ )
		{
			for( int j = 0; j < 4; j++ )
			{
				m_AnnulusGas.at(i,j) = new HTFProperties;
				m_AnnulusGas.at(i, j)->SetFluid((int)AnnulusGas.at(i, j));
			}
		}


		//[torr] Annulus gas pressure
		n_rows = n_cols = 0;
		p_matrix_t = value( P_P_A, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_P_a.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE annulus gas pressure matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[W/m] Receiver heat loss at design
		n_rows = n_cols = 0;
		p_matrix_t = value( P_DESIGN_LOSS, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_Design_loss.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "Receiver heat loss at design matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Receiver bellows shadowing loss factor
		n_rows = n_cols = 0;
		p_matrix_t = value( P_SHADOWING, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_Shadowing.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE bellows shadowing loss factor matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Loss due to dirt on the receiver envelope
		n_rows = n_cols = 0;
		p_matrix_t = value( P_DIRT_HCE, &n_rows, &n_cols );
		if( p_matrix_t != 0 && n_rows == m_n_rows_matrix && n_cols == 4 )
			m_Dirt_HCE.assign( p_matrix_t, n_rows, n_cols );
		else
		{
			message( TCS_ERROR, "HCE dirt on receiver loss factor matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns", m_n_rows_matrix, n_rows, n_cols );
			return -1;
		}

		//[-] Boiler Optical Table
		n_rows = n_cols = 0;
		p_matrix_t = value( P_B_OPTICALTABLE, &n_rows, &n_cols );
		m_b_OpticalTable.assign( p_matrix_t, n_rows, n_cols );
	
		// Set up the optical table object..

		/* The input should be defined as follows:
		- Data of size nx, ny
		- OpticalTable of size (nx+1)*(ny+1)
		- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
		- First value of remaining ny rows are y-axis values, not data
		- Data is contained in cells i,j : where i>1, j>1
		*/
		double *xax = new double[n_cols-1];
		double *yax = new double[n_rows-1];
		double *data = new double[(n_cols -1) * (n_rows -1)];

		//get the xaxis data values
		for(int i=0; i < n_cols-1; i++)
			xax[i] = m_b_OpticalTable.at(0, i)*m_d2r;

		//get the yaxis data values
		for(int j=1; j < n_rows; j++)
			yax[j-1] = m_b_OpticalTable.at(j, 0)*m_d2r;

		//get the data values
		for(int j=1; j < n_rows; j++)
		{
			for(int i=1; i < n_cols; i++)
				data[ i-1 + (n_cols-1)*(j-1) ] = m_b_OpticalTable.at(j, i);
		}

		/*
		//get the xaxis data values
		for(int i=1; i < n_cols; i++){
			xax[i-1] = m_b_OpticalTable.at(0, i)*m_d2r;
		}
		//get the yaxis data values
		for(int j=1; j < n_rows; j++){
			yax[j-1] = m_b_OpticalTable.at(j, 0)*m_d2r;
		}
		//Get the data values
		for(int j=1; j < n_rows; j++){
			for(int i=1; i < n_cols; i++){
				data[ i-1 + (n_cols-1)*(j-1) ] = m_b_OpticalTable.at(j, i);
			}
		}*/

		b_optical_table.AddXAxis(xax, n_cols-1);
		b_optical_table.AddYAxis(yax, n_rows-1);
		b_optical_table.AddData(data);
		delete [] xax;
		delete [] yax;
		delete [] data;
		optical_tables.Set_Table( &b_optical_table, 0 );

		// *************************
		//[-] Superheater Optical Table
		// *************************
		if( m_is_multgeom )
		{
			n_rows = n_cols = 0;
			p_matrix_t = value( P_SH_OPTICALTABLE, &n_rows, &n_cols );
			m_sh_OpticalTable.assign( p_matrix_t, n_rows, n_cols );
		
			// Set up the optical table object..
		
			/* The input should be defined as follows:
			- Data of size nx, ny
			- OpticalTable of size (nx+1)*(ny+1)
			- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
			- First value of remaining ny rows are y-axis values, not data
			- Data is contained in cells i,j : where i>1, j>1
			*/
			double *xax1 = new double[n_cols-1];
			double *yax1 = new double[n_rows-1];
			double *data1 = new double[(n_cols -1) * (n_rows -1)];
		
			//get the xaxis data values
			for(int i=0; i < n_cols-1; i++){
				xax1[i] = m_sh_OpticalTable.at(0, i)*m_d2r;
			}
			//get the yaxis data values
			for(int j=1; j < n_rows; j++){
				yax1[j-1] = m_sh_OpticalTable.at(j, 0)*m_d2r;
			}
			//Get the data values
			for(int j=1; j < n_rows; j++){
				for(int i=1; i < n_cols; i++){
					data1[ i-1 + (n_cols-1)*(j-1) ] = m_sh_OpticalTable.at(j, i);
				}
			}
			sh_optical_table.AddXAxis(xax1, n_cols-1);
			sh_optical_table.AddYAxis(yax1, n_rows-1);
			sh_optical_table.AddData(data1);
			delete [] xax1;
			delete [] yax1;
			delete [] data1;
			optical_tables.Set_Table( &sh_optical_table, 1 );
		}
		// ****************************************************************************
		// Don't write SH row in matrix_t if superheater if not different than boiler?
		// ****************************************************************************

		// Determine whether an incompatible set of inputs has been provided for the boiler quality. For recirc systems, the
		// boiler quality must fall in the range (0..1]. Limit to a minimum of .01 for convergence sake.
		if( !m_is_oncethru )
		{
			if( m_x_b_des < 0.01 || m_x_b_des > 1.0 )
			{
				message( TCS_ERROR, "For recirculated boiler systems, the specified boiler outlet quality %.3f must be greater than 0.01 and less than or equal to 1.0", m_x_b_des );
				return -1;
			}
		}

		// Initialize any member matrix_t values
		if( !m_is_sh )
			m_nModSH = 0;

		m_nModTot = m_nModBoil + m_nModSH;
		m_q_inc.resize( m_nModTot, 1 );
		m_q_loss.resize( m_nModTot, 1 );
		m_q_abs.resize( m_nModTot, 1 );
		m_T_ave.resize( m_nModTot, 1 );
		m_T_ave0.resize( m_nModTot, 1 );
		m_h_ave.resize( m_nModTot, 1 );
		m_h_ave0.resize( m_nModTot, 1 );
		m_h_in.resize( m_nModTot, 1 );
		m_h_out.resize( m_nModTot, 1 );
		m_x.resize( m_nModTot, 1 );
		m_q_rec.resize( m_nModTot, 1 );

		m_q_inc.fill( 0.0 );
		m_q_loss.fill( 0.0 );
		m_q_abs.fill( 0.0 );
		m_T_ave.fill( 0.0 );
		m_T_ave0.fill( 0.0 );
		m_h_ave.fill( 0.0 );
		m_h_ave0.fill( 0.0 );
		m_h_in.fill( 0.0 );
		m_h_out.fill( 0.0 );
		m_x.fill( 0.0 );
		m_q_rec.fill( 0.0 );

		// Set any constants
		m_fP_turb_min = 0.5;								//[-] Minimum fractional operating pressure at the turbine inlet
		
		// Convert theta_stow
		m_theta_stow = CSP::pi - m_theta_stow;

		// Design point values
		double omega	= 0.0;						// solar noon
		double dec		= 23.45*CSP::pi/180.0;		// declination at summer solstice

		// Solar altitude at noon on the summer solstice
		double SolarAlt = asin( sin(dec)*sin(m_latitude) + cos(m_latitude)*cos(dec)*cos(omega) );

		// Convert the solar angles to collector incidence angles
		double phi_t, theta_L;
		CSP::theta_trans( 0.0, (CSP::pi/2.0 - SolarAlt), m_ColAz, phi_t, theta_L );
		
		// Calculate the design-point efficiency
		m_eta_opt_fixed.resize(m_n_rows_matrix,1);
		for( int i = 0; i < m_n_rows_matrix; i++ )
			m_eta_opt_fixed.at(i,0) = m_TrackingError.at(i,0) * m_GeomEffects.at(i,0) * m_rho_mirror_clean.at(i,0) * m_dirt_mirror.at(i,0) * m_error.at(i,0);

		m_opteff_des.resize(m_n_rows_matrix,1);
		
		// Boiler receiver optical efficiency
		if( m_OptCharType.at(0,0) == 1 )
		{
			// User provides an optical table as a function of solar position
			m_opteff_des.at(0,0) = m_eta_opt_fixed.at(0,0)*max( b_optical_table.interpolate( 0.0, CSP::pi/2.0 - SolarAlt ), 0.0 );	
		}
		else if( m_OptCharType.at(0,0) == 2 )
		{
			// Collector incidence table
			m_opteff_des.at(0,0) = m_eta_opt_fixed.at(0,0)*max( b_optical_table.interpolate( 0.0, theta_L ), 0.0 );
		}
		else if( m_OptCharType.at(0,0) == 3 )
		{
			// IAM polynomials
			double iam_t = 0.0;
			double iam_l = 0.0;
			for( size_t i = 0; i < m_IAM_L.ncols(); i++ )
				iam_l += m_IAM_L.at(0,i)*pow(theta_L,i);
			for( size_t i = 0; i < m_IAM_T.ncols(); i++ )
				iam_t += m_IAM_T.at(0,i)*pow(phi_t,i);
			m_opteff_des.at(0,0) = m_eta_opt_fixed.at(0,0) * iam_t * iam_l;
		}
		else
		{
			message( TCS_ERROR, "The selected boiler optical model (%d) does not exist. Options are 1=Solar position table : 2=Collector incidence table : 3= IAM polynomials", m_OptCharType.at(0,0));
			return -1;
		}
		
		if( m_is_multgeom )
		{
			// Superheater receiver optical efficiency
			if( m_OptCharType.at(1,0) == 1 )
			{
				// User provides an optical table as a function of solar position
				m_opteff_des.at(1,0) = m_eta_opt_fixed.at(1,0)*max( sh_optical_table.interpolate( 0.0, CSP::pi/2.0 - SolarAlt ), 0.0 );
			}
			else if( m_OptCharType.at(1,0) == 2 )
			{
				// Collector incidence table
				m_opteff_des.at(1,0) = m_eta_opt_fixed.at(1,0)*max( sh_optical_table.interpolate( 0.0, theta_L ), 0.0 );
			}
			else if( m_OptCharType.at(1,0) == 3 )
			{
				// IAM polynomials
				double iam_t = 0.0;
				double iam_l = 0.0;
				for( size_t i = 0; i < m_IAM_L.ncols(); i++ )
					iam_l += m_IAM_L.at(1,i)*pow(theta_L,i);
				for( size_t i = 0; i < m_IAM_T.ncols(); i++ )
					iam_t += m_IAM_T.at(1,i)*pow(phi_t,i);
				m_opteff_des.at(1,0) = m_eta_opt_fixed.at(1,0) * iam_t * iam_l;
			}
			else
			{
				message(TCS_ERROR,  "The selected superheater optical model (%d) does not exist. Options are 1=Solar position table : 2=Collector incidence table : 3= IAM polynomials", m_OptCharType.at(1,0));
				return -1;
			}
		}
    
		// Calculate the design-point incident energy on each module for a single loop
		for( int i = 0; i < m_nModTot; i++ )
		{
			int gset = 0;
			if( (i>=m_nModBoil) && (m_is_multgeom) )
				gset = 1;
			else
				gset = 0;
			m_q_inc.at(i,0) = m_I_bn_des * m_A_aperture.at(gset,0) / 1000.0;	// [kW]
			m_q_rec.at(i,0) = m_q_inc.at(i,0) * m_opteff_des.at(gset,0);
		}

		// Calculate the total reference pressure drop across the field
		if( !m_is_sh )
		{
			m_fP_boil_to_sh = 0.0;
			m_fP_sf_sh = 0.0;
		}
		m_fP_sf_tot = m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h;

		if( m_P_turb_des*(1.0+m_fP_sf_tot) > 220.6 )
		{
			message(TCS_ERROR, "The design-point pressure at the inlet of the solar field exceeds the critical pressure (220.6 bar). Review your settings for turbine inlet pressure and solar field pressure drop to maintain reasonable design pressure conditions");
			return -1;
		}

		check_pressure.set_P_max( m_P_max );
		double h_sh_out_des = 0.0;
		double h_pb_out_des = 0.0;

		// Estimate the design-point thermal losses
		// *********** "standard" boiler + optional superheater design *******
		if( !m_is_oncethru )		// Analyze the conventional boiler only/boiler+superheat options
		{
			// Calculate boiler inlet/outlet enthalpies
			water_PQ( check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, m_x_b_des, &wp );
			double h_b_out_des = wp.enth;		//[kJ/kg]
			// Power block outlet/field inlet enthalpy
			water_TP( m_T_field_in_des, check_pressure.P_check( m_P_turb_des*(1.0 + m_fP_sf_tot - m_fP_hdr_c) )*100.0, &wp );
			h_pb_out_des = wp.enth;		//[kJ/kg]
			// Determine the mixed boiler inlet enthalpy
			double h_b_in_des = h_pb_out_des*m_x_b_des + h_b_out_des*(1.0 - m_x_b_des);
			double dh_b_des = (h_b_out_des - h_b_in_des)/(double)m_nModBoil;
			for( int i = 0; i < m_nModBoil; i++ )	
			{
				// Calculate the local pressure in the boiler. Assume a linear pressure drop across each section
				double P_loc = m_P_turb_des*(1.0 + m_fP_sf_tot-m_fP_sf_boil*(1.0 - (double)(i)/(double)m_nModBoil));
				// Get the temperature and quality at each state in the boiler
				water_PH( check_pressure.P_check( P_loc )*100.0, (h_b_in_des + dh_b_des*(double)(i+1) - dh_b_des/2.0), &wp );
				m_T_ave.at(i,0) = wp.temp;		//[K]
				m_x.at(i,0) = wp.qual;

				// Calculate the heat loss at each temperature
				if( m_HLCharType.at(0,0) == 1 )
				{
					// Estimate based on the polynomial adjustments provided by the user
					double dT_loc = m_T_ave.at(i,0) - m_T_amb_des_sf;
					double c_hl = m_HL_dT.at(0,0) + m_HL_dT.at(0,1)*dT_loc + m_HL_dT.at(0,2)*pow(dT_loc,2) + m_HL_dT.at(0,3)*pow(dT_loc,3) + m_HL_dT.at(0,4)*pow(dT_loc,4);		//[W/m] Don't adjust for wind speed here
					m_q_loss.at(i,0) = c_hl*m_L_col.at(0,0)/1000.0;		//[kW] Total thermal loss from this collector
				}
				else if( m_HLCharType.at(0,0) == 2 )
				{
					// Estimate based on user-supplied guesses for thermal losses
					m_q_loss.at(i,0) = 0.0;
					for( int j = 0; j < 4; j++ )
						m_q_loss.at(i,0) += m_Design_loss.at(0,j)*m_HCE_FieldFrac.at(0,j)*m_L_col.at(0,0)/1000.0;		// [kW]
				}
				m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);		// [kW]
			}

			if( m_is_sh )
			{
				// Decide which geometry set to use
				int gset = 0;
				if( m_is_multgeom ) gset = 1;

				// Calculate superheater inlet/outlet enthalpies
				water_PQ( check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h+m_fP_sf_sh))*100.0, 1.0, &wp);
				double h_sh_in_des = wp.enth;
				water_TP( (m_T_field_out_des), check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h))*100.0, &wp );
				h_sh_out_des = wp.enth;
				double dh_sh_des = (h_sh_out_des - h_sh_in_des)/(double)m_nModSH;
				for( int ii = 0; ii < m_nModSH; ii++ )
				{
					int i = ii + m_nModBoil;
					// Calculate the local pressure in the superheater. Assume a linear pressure drop
					double P_loc = m_P_turb_des*(1.0 + m_fP_hdr_h+m_fP_sf_sh*(1.0 - (double)(ii)/(double)m_nModSH));
					water_PH( check_pressure.P_check( P_loc )*100.0, (h_sh_in_des + dh_sh_des*(double)(ii+1) - dh_sh_des/2.0), &wp );
					m_T_ave(i,0) = wp.temp;		// Convert to K

					// Calculate the heat loss at each temperature
					if( m_HLCharType.at(gset,0) == 1 )
					{
						// Polynomials
						double dT_loc = m_T_ave(i,0) - m_T_amb_des_sf;
						double c_hl = m_HL_dT.at(gset,0) + m_HL_dT.at(gset,1)*dT_loc + m_HL_dT.at(gset,2)*pow(dT_loc,2) + m_HL_dT.at(gset,3)*pow(dT_loc,3) + m_HL_dT.at(gset,4)*pow(dT_loc,4);	//[w/m] Don't adjust for wind speed here
						m_q_loss.at(i,0) = c_hl*m_L_col.at(gset,0)/1000.0;		//[kW] Total collector thermal loss
					}
					else if( m_HLCharType.at(gset,0) == 2 )
					{
						// User supplied guesses for thermal losses - Forristall model
						m_q_loss.at(i,0) = 0.0;
						for( int j = 0; j < 4; j++ )
							m_q_loss.at(i,0) += m_Design_loss.at(gset,j)*m_HCE_FieldFrac.at(gset,j)*m_L_col.at(gset,0)/1000.0;
					}
					m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);
				}
			}
		}
		else	// Analyze the once-through boiler+superheater options
		{
			// Calculate the total enthalpy rise across the loop
			water_TP( (m_T_field_in_des), check_pressure.P_check( m_P_turb_des*(1.0+m_fP_sf_tot))*100.0, &wp );
			h_pb_out_des = wp.enth;
			water_TP( (m_T_field_out_des), check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h))*100.0, &wp );
			h_sh_out_des = wp.enth;
			// Enthalpy rise across each collector module
			double dh_ot_des = (h_sh_out_des - h_pb_out_des)/(double)m_nModTot;		//[kJ/kg]
			for( int i = 0; i < m_nModTot; i++ )
			{
				// Decide which geometry set to use
				int gset = 0;
				if( i >= m_nModBoil && m_is_multgeom )	gset = 1;

				// Calculate the local pressure in the loop, assume a linear pressure drop
				double P_loc = m_P_turb_des*(1.0 + (m_fP_sf_boil + m_fP_sf_sh)*(1.0 - (double)(i)/(double)m_nModTot) + m_fP_hdr_h);
				// Get the temperature/quality at each state in the loop
				water_PH( check_pressure.P_check( P_loc )*100.0, (h_pb_out_des + dh_ot_des*(double)(i+1) - dh_ot_des/2.0), &wp );
				m_T_ave.at(i,0) = wp.temp;
				m_x.at(i,0) = wp.qual;

				// Calculate the heat loss at each temperature
				if( m_HLCharType.at(gset,0) == 1 )
				{
					// Polynomials
					double dT_loc = m_T_ave.at(i,0) - m_T_amb_des_sf;
					double c_hl = m_HL_dT.at(gset,0) + m_HL_dT.at(gset,1)*dT_loc + m_HL_dT.at(gset,2)*pow(dT_loc,2) + m_HL_dT.at(gset,3)*pow(dT_loc,3) + m_HL_dT.at(gset,4)*pow(dT_loc,4);	//[w/m] Don't adjust for wind speed here
					m_q_loss.at(i,0) = c_hl*m_L_col.at(gset,0)/1000.0;		//[kW] total collector thermal loss
				}
				else if( m_HLCharType.at(gset,0) == 2 )
				{
					// User supplied guesses for thermal losses - Forristall model
					m_q_loss.at(i,0) = 0.0;
					for( int j = 0; j < 4; j++ )
						m_q_loss.at(i,0) += m_Design_loss.at(gset,j)*m_HCE_FieldFrac.at(gset,j)*m_L_col.at(gset,0)/1000.0;
				}
				m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);		// [kW] Total absorbed energy in the collector
			}
		}

		// Calculate total solar field aperture area
		m_Ap_tot = 0.0;
		if( m_is_multgeom )
			m_Ap_tot = (m_A_aperture.at(0,0)*m_nModBoil + m_A_aperture.at(1,0)*m_nModSH)*m_nLoops;
		else
			m_Ap_tot = m_A_aperture.at(0,0)*(double)(m_nModTot*m_nLoops);

		// Estimate piping thermal loss
		double q_loss_piping = m_Ap_tot*m_Pipe_hl_coef/1000.0*( (m_T_field_in_des + m_T_field_out_des)/2.0 - m_T_amb_des_sf );		// hl coef is [W/m2-K], use average field temp as driving difference

		// ************* Design solar field thermal power output **************
		double q_inc_tot_loop, q_rec_tot_loop, q_loss_tot_loop, q_abs_tot_loop;
		q_inc_tot_loop = q_rec_tot_loop = q_loss_tot_loop = q_abs_tot_loop = 0.0;
		for( int i = 0; i < m_nModTot; i++ )
		{
			q_inc_tot_loop += m_q_inc.at(i,0);
			q_rec_tot_loop += m_q_rec.at(i,0);
			q_loss_tot_loop += m_q_loss.at(i,0);
		}
		//double q_inc_tot_des = q_inc_tot_loop*(double)m_nLoops;
		m_q_rec_tot_des = q_rec_tot_loop*(double)m_nLoops;
		double q_loss_tot_des = q_loss_tot_loop*(double)m_nLoops + q_loss_piping;
		double q_abs_tot_des = m_q_rec_tot_des - q_loss_tot_des;

		double eta_therm_sf_des = 1.0 - q_loss_tot_des/m_q_rec_tot_des;		// Design solar field efficiency

		double eta_tot_sf_des = 0.0;
		if( m_is_multgeom )
			eta_tot_sf_des = (m_opteff_des.at(0,0)*m_nModBoil*m_A_aperture.at(0,0) + m_opteff_des.at(1,0)*m_nModSH*m_A_aperture.at(1,0))/(m_nModBoil*m_A_aperture.at(0,0)+m_nModSH*m_A_aperture.at(1,0))*eta_therm_sf_des; // Design solar field total efficiency
		else
			eta_tot_sf_des = m_opteff_des.at(0,0)*eta_therm_sf_des;

		// Calculate the design-point mass flow rate leaving the solar field
		if( h_sh_out_des == 0.0 || h_pb_out_des == 0.0 )
		{
			message( TCS_WARNING, "At design, either the superheater outlet enthlalpy (%.1f) or the solarfield inlet density (%f) is not set", h_sh_out_des, h_pb_out_des );
		}

		m_m_dot_des = q_abs_tot_des/(h_sh_out_des - h_pb_out_des);

		// Calculate the design-point mass flow rate in the boiler only (include recirculation mass)
		m_m_dot_b_des = 0.0;
		if( m_x_b_des == 0.0 )
			m_m_dot_b_des = m_m_dot_des;
		else
			m_m_dot_b_des = m_m_dot_des/m_x_b_des;

		// Calculate maximum flow rate to the power block
		m_m_dot_pb_des = m_q_pb_des/(h_sh_out_des - h_pb_out_des);
		double m_dot_pb_max = m_m_dot_pb_des * m_cycle_max_fraction;
		m_m_dot_max = m_dot_pb_max/(double)m_nLoops;
		m_m_dot_b_max = m_m_dot_max/m_x_b_des;

		// Convert the thermal inertia term here
		m_e_trans = m_e_startup * m_Ap_tot/(double)(m_nModTot*m_nLoops);		//[kJ/m2-K] -> [kJ/K] Average transient energy per collector

		// Check to see if the design provided by the user is reasonable. If not, return a warning message
		double T_burn = 0.0;
		if( !m_is_oncethru )
		{
			water_TP( m_T_field_in_des, check_pressure.P_check( m_P_turb_des*(1.0+m_fP_sf_tot))*100.0, &wp );	// solar field inlet
			double dvar1 = wp.enth;
			water_PQ( check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, m_x_b_des, &wp );	// boiler outlet
			double dvar2 = wp.enth;
			water_PQ( check_pressure.P_check( m_P_turb_des*(1.0+m_fP_hdr_h+m_fP_sf_sh))*100.0, 1.0, &wp );		// superheater inlet
			double dvar7 = wp.enth;
			double dvar3 = (dvar2 - dvar1)/(double)m_nModBoil;		// The enthalpy rise per boiler module

			// Calculate the expected enthalpy rise in the superheater based on ratios of expected energy absorptoin between the boiler and superheater
			double dvar10 = 0.0;
			if( m_is_multgeom )
			{
				double q_loss_b_sum = 0.0;
				double q_loss_sh_sum = 0.0;
				for( int i = 0; i < m_nModBoil; i++ )
					q_loss_b_sum += m_q_loss.at(i,0);
				for( int i = m_nModBoil; i < m_nModTot; i++ )
					q_loss_sh_sum += m_q_loss.at(i,0);

				// = (Expected performance of the superheater modules) / (Expected performance of the boiler modules)
				dvar10 = (m_opteff_des.at(1,0)*m_A_aperture.at(1,0)*m_I_bn_des/1000.0 - q_loss_sh_sum/(double)m_nModSH) /
							(m_opteff_des.at(0,0)*m_A_aperture.at(0,0)*m_I_bn_des/1000.0 - q_loss_b_sum/(double)m_nModBoil);
			}
			else
			{
				// No separate geometry, so approximate the superheater and boiler as the same
				dvar10 = 1.0;
			}
			// Project this to the superheater modules
			double dvar4 = dvar7 + dvar3*m_nModSH*dvar10;		// Estimated superheater outlet enthalpy
			// Check the temperature
			water_PH( m_P_turb_des*(1.0 - m_fP_boil_to_sh)*100.0, dvar4, &wp );
			double dvar5 = wp.temp;						// convert to K
			double dvar6 = dvar5 - m_T_field_out_des;			// Difference in temperature between estimated outlet temperature and user-spec
			// What are the superheater design conditions?
			water_TP( m_T_field_out_des, check_pressure.P_check( m_P_turb_des*(1.0*m_fP_hdr_h) )*100.0, &wp );	// Superheater outlet
			double dvar8 = wp.enth;
			double dvar9 = (dvar8 - dvar7)/(dvar2 - dvar1)*m_nModBoil;

			if( fabs(dvar6) > 25.0 )
			{
				message( TCS_WARNING, "The field you selected with %d boiler modules and %d superheater modules results in a projected superheater outlet temperature"
					" of %.1f [C] which is %.1f [C] away from the design-point value of %.1f [C]. Based on the specified collector geometry, the ideal fractional"
					" number of superheater modules is approximately %.2f. Consider adjusting the design-point steam settings, the module geometry, and/or"
					" the module distribution to better match the desired steam conditions", m_nModBoil, m_nModSH, dvar5-273.15, dvar6, m_T_field_out_des-273.15, dvar9 );					
			}
			/*if( dvar6 < -25.0 )
			{
				message( TCS_WARNING,  "The field you selected with %d boiler modules and %d superheater modules results in a projected superheater outlet temperature"
					" of %d [C] which is %d [C] away from the design-point value of %d [C]. Based on the specified collector geometry, the ideal fractional"
					" number of superheater modules is approximately %d. Consider adjusting the design-point steam settings, the module geometry, and/or"
					" the module distribution to better match the desired steam conditions", m_nModBoil, m_nModSH, dvar5-273.15, -dvar6, m_T_field_out_des-273.15, dvar9 );								
			}*/
			T_burn = max( dvar5, m_T_field_out_des ) - 273.15;
		}
		else
		{
			// The value of the design-point temperature is used in the limiting function for enthlapy. Set the value to 
			// the field design outlet temperature plus a margin
			T_burn = m_T_field_out_des - 273.15;
		}

		// Calculate the minimum allowable enthalpy before freezing
		water_TP( 5.0 + 273.15, m_P_turb_des*m_cycle_cutoff_frac*100.0, &wp );
		double h_freeze = wp.enth;
		// Calculate the maximum allowable enthalpy before convergence error
		water_TP( min( T_burn + 150.0 + 273.15, 1000.0 ), m_P_max*100.0, &wp );
		double h_burn = wp.enth;
		// Set up the enthalpy limit function
		check_h.set_enth_limits( h_freeze, h_burn );

		m_T_ave_prev.resize( m_nModTot, 1);
		m_T_ave_prev.fill( m_T_field_ini );

		m_defocus_prev = 1.0;
		m_t_sby_prev = m_t_sby_des;
		m_t_sby = m_t_sby_des;
		m_is_pb_on_prev = false;
		m_T_sys_prev = m_T_field_ini;

		// Initialize evacuated tube model if used
		for( int i = 0; i < m_n_rows_matrix; i++ )
		{
			if( m_HLCharType.at(i,0) == 2 )
			{
				htfProps.SetFluid( 21 );
				evac_tube_model.Initialize_Receiver( m_GlazingIntactIn, m_P_a, m_D_5, m_D_4, m_D_3, m_D_2, m_D_p, m_opteff_des, m_Dirt_HCE, m_Shadowing, m_Tau_envelope, m_alpha_abs,
					                                      m_alpha_env, &eps_abs, m_AnnulusGas, m_AbsorberMaterial, m_EPSILON_4, m_EPSILON_5, m_L_col, &htfProps, m_A_cs, m_D_h, m_Flow_type );
			}
		}

		// Write Calculated Design Parameters
		value(PO_A_APER_TOT, m_Ap_tot);		//[m]

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{		
		
		//double dnifc = value( I_DNIFC );					//[W/m2] Forecast DNI
		double I_bn = value( I_I_BN );						//[W/m2] Current DNI
		double T_db = value( I_T_DB )+273.15;				//[K] Dry bulb temp, convert from C
		double T_dp = value( I_T_DP )+273.15;				//[K] Dewpoint temp, convert from C
		double P_amb = value( I_P_AMB )*100.0;				//[Pa] Ambient pressure, convert from mbar
		double V_wind = value( I_V_WIND );					//[m/s] Ambient windspeed
		//double m_dot_htf_ref = value( I_M_DOT_HTF_REF )/3600.0;	//[kg/s] Reference HTF flow rate at design conditions, convert from kg/hr
		//double m_pb_demand = value( I_M_PB_DEMAND )/3600.0;		//[kg/s] Demand HTF flow from the power block, convert from kg/hr
		double shift = value( I_SHIFT )*0.0174533;			//[deg] Shift in longitude from local standard meridian
		double SolarAz = value( I_SOLARAZ );				//[deg] Solar azimuth angle
		double SolarZen = value(I_SOLARZEN)*0.0174533;;		//Solar zenith angle [deg]
		double T_pb_out = value( I_T_PB_OUT )+273.15;		//[K] Fluid temperature from the power block, convert from C
	//  int tou_period = (int) value( I_TOUPERIOD );		//[-] Time-of-use period
		int tou_period = (int) value( I_TOUPERIOD ) - 1;	// control value between 1 & 9, have to change to 0-8 for array index

		SolarAz = (SolarAz - 180.0) * 0.0174533;			//[rad] Convert to TRNSYS convention, radians

		double hour = (double) ((int)(time/3600.0)%24);							
		

		/****** Read in stored variables every timestep*******
		call getStorageVars(stored,nS,info)
		do i=1,nModTot
			T_ave0(i) = stored(i)
		enddo
		defocus0 = stored(nModTot + 1)
		t_sby0 = stored(nModTot + 2)
		is_pb_on0 = .false.
		if(stored(nModTot+3)==1) is_pb_on0 = .true.
		T_sys0 = stored(nModTot + 4)
		!-------------------------------- */

		//************************************************
		// If Debugging, Set Stored Variables Here
		//************************************************
		/*m_T_ave_prev.at(0,0) = 537.009;
		m_T_ave_prev.at(1,0) = 558.912;
		m_T_ave_prev.at(2,0) = 574.929;
		m_T_ave_prev.at(3,0) = 575.174;
		m_T_ave_prev.at(4,0) = 575.419;
		m_T_ave_prev.at(5,0) = 575.663;
		m_T_ave_prev.at(6,0) = 575.907;
		m_T_ave_prev.at(7,0) = 576.15;
		m_T_ave_prev.at(8,0) = 576.392;
		m_T_ave_prev.at(9,0) = 576.633;
		m_T_ave_prev.at(10,0) = 576.874;
		m_T_ave_prev.at(11,0) = 577.115;
		m_T_ave_prev.at(12,0) = 587.15;
		m_T_ave_prev.at(13,0) = 622.209;
		m_T_ave_prev.at(14,0) = 666.725;
		m_T_ave_prev.at(15,0) = 716.48;
		m_defocus_prev = 1.0;
		m_t_sby_prev = 2.0;
		m_is_pb_on_prev = false;
		m_T_sys_prev = 525.458;

		hour = 12.0; */
		//*************************************************
		//*************************************************

		double T_sky = CSP::skytemp( T_db, T_dp, hour );		

		// Calculations for values once per timestep
		if( ncall == 0 )
		{
			// Optical calculations
			// Time calculations
			int day_of_year = (int) (time/3600.0) / 24 + 1;					//[-] Day of year
			// Duffie and Beckman 1.5.3b
			double B = (int) (day_of_year-1)*2.0*CSP::pi/365.0;	
			// Eqn of time in minutes
			double EOT = 229.2*(0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B) - 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
			// Declination in radians (Duffie and Beckman 1.6.1)
			double Dec = 23.45 * sin(360.0*(284.0+(double)day_of_year)/365.0*CSP::pi/180.0) * CSP::pi/180.0;
			// Solar Noon and time in hours
			double SolarNoon = 12.0 - ((shift)*180.0/CSP::pi)/15.0 - EOT/60.0;

			// Deploy & stow times in hours
			// Calculations modified by MJW 11/30/2009 to correct bug
			double theta_dep = max( m_theta_dep, 1.E-6 );
			double DepHr1 = cos(m_latitude)/tan(theta_dep);
			double DepHr2 = -tan(Dec)*sin(m_latitude)/tan(theta_dep);			
			double DepHr3 = CSP::sign(tan(CSP::pi-theta_dep)) * acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
			double DepTime = SolarNoon + DepHr3;

			double theta_stow = max(m_theta_stow,1.e-6);
			double StwHr1 = cos(m_latitude) / tan(theta_stow);
			double StwHr2 = -tan(Dec) * sin(m_latitude) / tan(theta_stow);
			double StwHr3 = CSP::sign(tan(CSP::pi-theta_stow))*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
			double StwTime = SolarNoon + StwHr3;

			// ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
			double HrA = hour-(step/3600.0);
			double HrB = hour;
			
			double MidTrack = std::numeric_limits<double>::quiet_NaN();         
			// Solar field operates
			if ((HrB > DepTime)  &&  (HrA < StwTime)) 
			{
				// solar field deploys during time period
				if (HrA < DepTime) 
				{
					m_ftrack = (HrB - DepTime) / (step/3600.0);
					MidTrack = HrB - m_ftrack * 0.5 * (step/3600.0);				
				}
				// Solar field stows during time period
				else if (HrB > StwTime) 
				{
					m_ftrack = (StwTime - HrA) / (step/3600.0);
					MidTrack = HrA + m_ftrack * 0.5 * (step/3600.0);				
				} 
				// solar field operates during entire period
				else 
				{
					m_ftrack = 1.0;
					MidTrack = HrA + 0.5 * (step/3600.0);
				}
			// solar field doesn't operate
			} else 
			{
				m_ftrack = 0.0;
				MidTrack = HrA + 0.5 * (step/3600.0);
			}

			// Maximum wind speed value
			if( V_wind >= m_V_wind_max )
				m_ftrack = 0.0;

			double StdTime = MidTrack;
			double SolarTime = StdTime + ((shift)*180/CSP::pi)/15.0 + EOT/60.0;
			// hour angle (arc of sun) in radians
			double omega = (SolarTime-12.0)*15.0*CSP::pi/180.0;
			// B. Stine equation for Solar Altitude angle in radians
			double SolarAlt = asin( sin( Dec ) * sin( m_latitude ) + cos(m_latitude)*cos(Dec)*cos(omega) );	
			SolarZen = CSP::pi/2 - SolarAlt;
				
			if (SolarZen < CSP::pi/2.0)
			{				
				//Convert the solar angles to collector incidence angles
				CSP::theta_trans(SolarAz, SolarZen, m_ColAz, phi_t, theta_L);
			
				for( int i = 0; i < m_n_rows_matrix; i++ )
				{
					double Iam_T, Iam_L;
					switch((int)m_OptCharType.at(i,0))
					{
					case 1:		//sun position
						//user provides an optical table as a function of solar position
						//m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( b_optical_table.interpolate(SolarAz, min(SolarZen, CSP::pi/2.)), 0.0);
						m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( optical_tables.interpolate(SolarAz, min(SolarZen, CSP::pi/2.0), i), 0.0 );
						break;
					case 2:		//incidence angle table
						//user provides an optical table as a function of collector incidence angles
						//m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( b_optical_table.interpolate(phi_t, max(theta_L, 0.0)), 0.0);
						m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( optical_tables.interpolate(phi_t, max(theta_L, 0.0), i), 0.0);
						break;
					case 3:		//incidence angle modifier polys
						//Otherwise, calculate the collector incidence angles for the IAM equations
						Iam_T = m_IAM_T.at(i,0) + m_IAM_T.at(i,1)*phi_t + m_IAM_T.at(i,2)*pow(phi_t,2) + m_IAM_T.at(i,3)*pow(phi_t,3) + m_IAM_T.at(i,4)*pow(phi_t,4);
						Iam_L = m_IAM_L.at(i,0) + m_IAM_L.at(i,1)*theta_L + m_IAM_L.at(i,2)*pow(theta_L,2) + m_IAM_L.at(i,3)*pow(theta_L,3) + m_IAM_L.at(i,4)*pow(theta_L,4);
						m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0) * Iam_T * Iam_L;
						break;
					default:
						//error
						message(TCS_ERROR, "No corresponding optical model. Error in solar angle calculation.");
						return -1;
					}
					m_eta_optical.at(i,0) *= m_ftrack;
				}			    								
			}
			else
			{
				m_eta_optical.fill(0.0);
				phi_t = CSP::pi/2.;
				theta_L = 0.0;
			}

			// Set initial defocus
			m_defocus	= 1.0;
			m_defocus_prev = m_defocus;
			m_is_def	= false;
			m_err_def	= 0.0;
			m_tol_def	= 0.0001;
			m_rc		= 0.7;						//Relaxation coefficient
			// Set the power block return temperature to a reasonable value since the power block has not yet been called
			T_pb_out = m_T_field_in_des;	//[K]

			// Reset the pressure check function
			check_pressure.report_and_reset();

			evac_tube_model.Update_Timestep_Properties( m_eta_optical );
		}

		//************************************************************
		// Determine the solar field thermal performance
		//************************************************************
		int iter_def = 0;
		bool defocus_on = false;


		double m_dot = std::numeric_limits<double>::quiet_NaN();
		double P_turb_in = std::numeric_limits<double>::quiet_NaN();
		double dP_basis = std::numeric_limits<double>::quiet_NaN();
		double h_pb_out = std::numeric_limits<double>::quiet_NaN();
		double h_b_in = std::numeric_limits<double>::quiet_NaN();
		double eta_opt_ave = 0.0;
		double m_dot_b_tot = 0.0;

		do		// Begin defocus loop
		{
			defocus_on = false;
			double defocus_lim = max( min( m_defocus, 1.0 ), 0.0 );
			eta_opt_ave = 0.0;
			int gset = 0;
			double q_rec_loop = 0.0;
			double q_inc_loop = 0.0;
			for( int i = 0; i < m_nModTot; i++ )
			{
				if( i >= m_nModBoil && m_is_multgeom )
					gset = 1;
				// Calculate the incident energy on each module
				m_q_inc.at(i,0) = I_bn*m_A_aperture.at(gset,0)/1000.0;		//[kW]
				// Calculate the energy on the receiver
				m_q_rec.at(i,0) = m_q_inc.at(i,0)*m_eta_optical.at(gset,0)*defocus_lim;
				// Average optical efficiency
				eta_opt_ave += m_eta_optical.at(gset,0)*m_A_aperture.at(gset,0)*m_nLoops/m_Ap_tot;
				q_rec_loop += m_q_rec.at(i,0);
				q_inc_loop += m_q_inc.at(i,0);
			}
			
			// For nighttime conditions, calculate the solar field inlet temperature
			if( q_rec_loop == 0.0 && m_ffrac[tou_period] < m_cycle_cutoff_frac )
				T_pb_out = max( 313.15, m_T_sys_prev );		// Need to limit the inlet temp so that the minimum temp in the loops stays above freezing

			// Total reference pressure drop across the field: 9/16/13: tn: already calculated in initial call

			// Guess the mass flow rate in a loop for iteration. Ratio of incident heat/abs heat at design differs by nLoops..
			double m_dot_guess = max( min( q_rec_loop/m_q_rec_tot_des * m_m_dot_des, m_m_dot_max ), m_m_dot_min );

			// Guess the turbine pressure.. turbine inlet pressure is highly insensitive to condenser pressure, so
			// simplify the expression to eliminate condenser pressure
			double P_turb_in_guess = turb_pres_frac(m_dot_guess*(double)m_nLoops/m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des;
			double dP_basis_guess = m_dot_guess*(double)m_nLoops/m_m_dot_des*m_P_turb_des;						 

			if( m_is_oncethru || m_ftrack <= 0.0 )		// Run in once-through mode at night since distinct boiler/superheater models are not useful
			{
				// Guess the loop inlet/outlet enthalpies
				water_TP( T_pb_out, check_pressure.P_check( P_turb_in_guess+dP_basis_guess*(m_fP_sf_tot-m_fP_hdr_c))*100.0, &wp );
				double h_b_in_guess = wp.enth;		//[kJ/kg]
				//double h_pb_out_guess = h_b_in_guess;	//[kJ/kg]
				water_TP( m_T_field_out_des, check_pressure.P_check( P_turb_in_guess+dP_basis_guess*m_fP_hdr_h)*100.0, &wp );
				double h_sh_out_guess = wp.enth;		//[kJ/kg]
				
				// Set the loop inlet enthalpy
				m_h_in.at(0,0) = h_b_in_guess;	//[kJ/kg]

				// Set up iteration brackets for false-position method
				double m_dot_lower = 0.7*m_m_dot_min;
				double m_dot_upper = 1.3*m_m_dot_max;
				// Set logic to switch from bisection to false position mode
				bool upflag = false;
				bool lowflag = false;
				// Set upper and lower err results
				double y_upper = std::numeric_limits<double>::quiet_NaN();
				double y_lower = std::numeric_limits<double>::quiet_NaN(); 

				// Set the iteration tolerance (MJW found that it doesn't help to adjust the tolerance based on iteration number)
				double tol = 1.E-4;

				// Do a rough guess of the receiver enthlapy for the whole loop
				double dh_guess = (h_sh_out_guess - h_b_in_guess)/(double)m_nModTot;
				for( int i = 0; i < m_nModTot; i++ )
					m_h_ave.at(i,0) = h_b_in_guess + dh_guess*(double)(i+1) - dh_guess/2.0;

				double err = 10.0*tol;
				int iter = 0;

				// Main iteration loop
				while( fabs(err) > tol && iter < 50 )
				{
					iter++;
					m_dot = m_dot_guess;

					// Update the turbine pressure and enthalpies
					P_turb_in = check_pressure.P_check( turb_pres_frac(m_dot*(double)m_nLoops/m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des);
					dP_basis = m_dot*(double)m_nLoops/m_m_dot_des*m_P_turb_des;

					// Guess the loop inlet/outlet enthalpies
					water_TP(T_pb_out, check_pressure.P_check(P_turb_in+dP_basis*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
					h_b_in = wp.enth;
					h_pb_out = h_b_in;
					water_TP(m_T_field_out_des, check_pressure.P_check(P_turb_in+dP_basis*m_fP_hdr_h)*100.0, &wp);
					double h_sh_out = wp.enth;

					// Set the loop inlet enthalpy
					m_h_in.at(0,0) = h_b_in;

					// Initialize
					m_q_loss.fill(0.0);
					m_q_abs.fill(0.0);

					for( int i = 0; i < m_nModTot; i++)
					{
						// Which geometry set?
						gset = 0;
						if( i >= m_nModBoil && m_is_multgeom )
							gset = 1;

						// Calculate thermal losses based on temperature guess values
						// Calculate the local pressure in the superheter. Assume a linear pressure drop across each section
						double P_loc = check_pressure.P_check( P_turb_in + dP_basis * (m_fP_hdr_h + (m_fP_sf_sh + m_fP_boil_to_sh + m_fP_sf_boil)*(1.0 - (double)i/(double)m_nModTot)));

						// Get the temperature at each state point in the loop
						water_PH( P_loc*100.0, m_h_ave.at(i,0), &wp );
						m_T_ave.at(i,0) = wp.temp;

						// Calculate the heat loss at each temperature
						if( m_HLCharType.at(gset,0) == 1 )
						{
							// Estimate based on the polynomial adjustments provided by the user
							double dT_loc = m_T_ave.at(i,0) - T_db;
							double c_hl = m_HL_dT.at(gset,0) + m_HL_dT.at(gset,1)*dT_loc + m_HL_dT.at(gset,2)*pow(dT_loc,2) + m_HL_dT.at(gset,3)*pow(dT_loc,3) + m_HL_dT.at(gset,4)*pow(dT_loc,4);	//[W/m] Effect from dT
							if( m_HL_W.at(gset,0) != 0 || m_HL_W.at(gset,1) != 0 || m_HL_W.at(gset,2) != 0 || m_HL_W.at(gset,3) != 0 || m_HL_W.at(gset,0) != 0 )
								c_hl *= m_HL_W.at(gset,0) + m_HL_W.at(gset,1)*V_wind + m_HL_W.at(gset,2)*pow(V_wind,2) + m_HL_W.at(gset,3)*pow(V_wind,3) + m_HL_W.at(gset,4)*pow(V_wind,4);
							m_q_loss.at(i,0) = c_hl*m_L_col.at(gset,0)/1000.0;			//[kW] Total thermal loss from this collector
							m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);	//[kW] Total absorbed energy in this collector
						}
						else if( m_HLCharType.at(gset,0) == 2 )
						{
							// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)

							m_q_loss.at(i,0) = 0.0;
							m_q_abs.at(i,0) = 0.0;

							for( int j = 0; j < 4; j++ )
							{
								// Only calculate if the HCE fraction is non-zero
								if( m_HCE_FieldFrac.at(gset,j) <= 0.0 )
									continue;

								/*Call the receiver performance model - single point mode
								!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
								!only use for the fluid type in the single point model is calculating the convective heat transfer
								!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
								!steam that substituting the HTF here introduces negligible error.*/

								// For LF, HT = CT && sca_num = 0
								double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
								q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
								evac_tube_model.EvacReceiver( m_T_ave.at(i,0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc.at(i,0)/m_L_col.at(gset,0)*1000.0, gset, j, gset, 0, true, ncall,
								                             time/3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3 );

								if( q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs )
								{
									q_rec_loss = 0.0;
									q_rec_abs = 0.0;
								}

								m_q_loss.at(i,0) += q_rec_loss*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]
								m_q_abs.at(i,0) += q_rec_abs*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]

							}

						}

						// Set the inlet enthalpy equal to the outlet of the previous node
						if( i > 0 )
							m_h_in.at(i,0) = m_h_out.at(i-1,0);

						// Calculate the collector outlet enthalpy
						double tol_t = 0.001;
						double err_t = 10.0*tol_t;
						int iter_t = 0;
						while( err_t > tol_t && iter_t < 50 )
						{
							iter_t++;
							// Calculate the average enthalpy value in the collector module
							m_h_out.at(i,0) = check_h.check( m_h_in.at(i,0) + m_q_abs.at(i,0)/m_dot_guess - (m_T_ave.at(i,0)-m_T_ave_prev.at(i,0))*m_e_trans/step );
							// Update guesses for h_ave and T_ave
							double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i,0))/2.0;
							// Update the average temperature for the heat loss calculation
							water_PH( P_loc*100.0, h_aveg, &wp );
							m_T_ave.at(i,0) = wp.temp;
							err_t = fabs( (m_h_ave.at(i,0) - h_aveg)/m_h_ave.at(i,0) );
							m_h_ave.at(i,0) = h_aveg;
						}
						if( i < m_nModTot - 1 )
							m_h_ave.at(i+1,0) = check_h.check( m_h_in.at(i,0) + (m_h_out.at(i,0) - m_h_in.at(i,0))*1.5 );
					
					}   // End step through receivers in flow path

					err = (h_sh_out - m_h_out.at(m_nModTot-1,0))/h_sh_out;

					if( m_dot == m_m_dot_min && err > 0.0 )			// m_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
						break;

					//***************************************************
					// *** Hybrid False Position Iteration Method *******
					//***************************************************
					if( lowflag && upflag )
					{
						if( err > 0.0 )
						{
							m_dot_upper = m_dot;
							y_upper = err;
						}
						else
						{
							m_dot_lower = m_dot;
							y_lower = err;
						}
						m_dot_guess = y_upper/(y_upper-y_lower)*(m_dot_lower-m_dot_upper) + m_dot_upper;
					}
					else
					{
						if( err > 0.0 )			// Prescribed is greater than calculated, so decrease mass flow and set upper limit
						{
							m_dot_upper = m_dot;
							y_upper = err;
							upflag = true;
						}
						else					// Prescribed is less than calculated, so increase mass flow and set lower limit
						{
							m_dot_lower = m_dot;
							y_lower = err;
							lowflag = true;
						}

						if( lowflag && upflag )		// If results of bracket are defined, use false position
							m_dot_guess = y_upper/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
						else						// If not, recalculate value based on approximate energy balance
						{
							if( iter < 3 )
							{
								double q_abs_sum = 0.0;
								for( int ii = 0; ii < m_nModTot; ii++ )
									q_abs_sum += m_q_abs.at(ii,0);
								m_dot_guess = q_abs_sum/(h_sh_out - h_b_in);
								m_dot_guess = max( m_m_dot_min*0.5, min( m_dot_guess, m_m_dot_max*1.5 ) );
							}
							else
								m_dot_guess = 0.5*m_dot_upper + 0.5*m_dot_lower;
						}
					}

					//**************************************************************************
					// **** End Hyrbid False Position Iteration Method *****************
					//**************************************************************************
					if( m_dot_lower >= m_m_dot_max )	// Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
					{
						m_is_def = true;
						break;
					}
					if( m_dot_upper <= m_m_dot_min )   // Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
						m_dot_guess = m_m_dot_min;
						
				}		// End main iteration loop

				// Defocus calculations
				m_err_def = (m_dot_guess - m_m_dot_max)/m_m_dot_max;
				if( !m_is_def && m_err_def > 0.0 )
					m_is_def = true;
				if( m_is_def )
				{
					// Calculate new defocus
					m_defocus = min( 1.0, m_defocus_prev*pow( 1.0/(m_err_def + 1.0), m_rc ) );
					if( fabs(m_err_def) > m_tol_def )
					{
						m_defocus_prev = m_defocus;
						iter_def++;
						if( iter_def < 11 )
						{
							defocus_on = true;
							continue;
						}
					}
				}

				// The boiler mass flow rate is equal to the flow throughout the loop
				m_dot_b_tot = m_dot*(double)m_nLoops;

			}		// End once-through model
			else	// Standard boiler + optional superheater design
			{
				// Boiler
				// Guess the field inlet enthalpy
				water_TP( T_pb_out, check_pressure.P_check( P_turb_in_guess+dP_basis_guess*(m_fP_sf_tot-m_fP_hdr_c) )*100.0, &wp );
				double h_pb_out_guess = wp.enth;		//[kJ/kg]

				// Boiler outlet conditions
				water_PQ( check_pressure.P_check( P_turb_in_guess+dP_basis_guess*(m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, m_x_b_des, &wp );
				double h_b_out_guess = wp.enth;		//[kJ/kg]
				water_PQ( check_pressure.P_check( P_turb_in_guess+dP_basis_guess*(m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, 0.0, &wp );
				double h_b_recirc_guess = wp.enth;	//[kJ/kg]

				// Determine the mixed inlet enthalpy
				double h_b_in_guess = h_pb_out_guess*m_x_b_des + h_b_recirc_guess*(1.0 - m_x_b_des);

				// Set the loop inlet enthalpy
				m_h_in(0,0) = h_b_in_guess;		//[kJ/kg]

				double m_dot_b_guess = m_dot_guess/m_x_b_des;
				// Set up iteration brackets for false-position method
				double m_dot_lower = 0.7*m_m_dot_min/m_x_b_des;
				double m_dot_upper = 1.3*m_m_dot_b_max;

				bool upflag = false;		// Set logic to switch from bisection to false position mode
				bool lowflag = false;		// Set logic to swtich from bisection to false position mode

				double y_upper = std::numeric_limits<double>::quiet_NaN();
				double y_lower = std::numeric_limits<double>::quiet_NaN();

				// Set iteration tolerance
				double tol = 1.E-4;

				// Do a rough guess of the receiver enthalpy for the boiler
				double dh_b_guess = (h_b_out_guess - h_b_in_guess)/(double) m_nModBoil;
				for( int i = 0; i < m_nModBoil; i++ )
					m_h_ave.at(i,0) = h_b_in_guess + dh_b_guess*(double)(i+1) - dh_b_guess/2.0;

				double err = 10.0*tol;
				int iter = 0;
								
				double m_dot_b = std::numeric_limits<double>::quiet_NaN();

				while( fabs(err) > tol && iter < 50 )
				{
					iter++;
					m_dot_b = m_dot_b_guess;
					//Initialize
					m_q_loss.fill(0.0);
					m_q_abs.fill(0.0);

					// Update inlet enthalpy conditions and turbine pressure
					P_turb_in = check_pressure.P_check( turb_pres_frac(m_dot_b*m_x_b_des*(double)m_nLoops/m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des );
					dP_basis = m_dot_b*(double)m_nLoops/m_m_dot_b_des*m_P_turb_des;
					
					// Field inlet enthalpy
					water_TP( T_pb_out, check_pressure.P_check( P_turb_in+dP_basis*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp );
					h_pb_out = wp.enth;

					// Update the boiler outlet conditions
					water_PQ( check_pressure.P_check( P_turb_in+dP_basis*(m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, m_x_b_des, &wp );	// 2-phase outlet enthalpy
					double h_b_out = wp.enth;
					water_PQ( check_pressure.P_check( P_turb_in+dP_basis*(m_fP_hdr_h+m_fP_sf_sh+m_fP_boil_to_sh))*100.0, 0.0, &wp );		// Recirculation enthalpy
					double h_b_recirc = wp.enth;

					// Determin the mixed inlet enthalpy
					h_b_in = h_pb_out*m_x_b_des + h_b_recirc*(1.0 - m_x_b_des);

					// Set the loop inlet enthalpy
					m_h_in(0,0) = h_b_in;

					for( int i = 0; i < m_nModBoil; i++ )
					{
						// Calculate thermal losses based on temperature guess values
						// Calculate the local pressure in the boiler. Assume a linear pressure drop across each section
						double P_loc = check_pressure.P_check( P_turb_in + dP_basis*(m_fP_sf_tot - m_fP_sf_boil*(1.0 - (double)i/(double)m_nModBoil) ) );

						// Get the temperature at each state in the boiler
						water_PH( P_loc*100.0, m_h_ave.at(i,0), &wp );
						m_T_ave.at(i,0) = wp.temp;

						gset = 0;
						// Calculate the heat loss at each temperature
						if( m_HLCharType.at(gset,0) == 1 )
						{
							// Estimate based on the polynomial adjustments provided by the user
							double dT_loc = m_T_ave.at(i,0) - T_db;
							double c_hl = m_HL_dT.at(gset,0) + m_HL_dT.at(gset,1)*dT_loc + m_HL_dT.at(gset,2)*pow(dT_loc,2) + m_HL_dT.at(gset,3)*pow(dT_loc,3) + m_HL_dT.at(gset,4)*pow(dT_loc,4);	//[W/m] Effect from dT
							if( m_HL_W.at(gset,0) != 0 || m_HL_W.at(gset,1) != 0 || m_HL_W.at(gset,2) != 0 || m_HL_W.at(gset,3) != 0 || m_HL_W.at(gset,0) != 0 )
								c_hl *= m_HL_W.at(gset,0) + m_HL_W.at(gset,1)*V_wind + m_HL_W.at(gset,2)*pow(V_wind,2) + m_HL_W.at(gset,3)*pow(V_wind,3) + m_HL_W.at(gset,4)*pow(V_wind,4);
							m_q_loss.at(i,0) = c_hl*m_L_col.at(gset,0)/1000.0;			//[kW] Total thermal loss from this collector
							m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);	//[kW] Total absorbed energy in this collector
						}
						else if( m_HLCharType.at(gset,0) == 2 )
						{
							// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)

							m_q_loss.at(i,0) = 0.0;
							m_q_abs.at(i,0) = 0.0;
							
							for( int j = 0; j < 4; j++ )
							{
								// Only calculate if the HCE fraction is non-zero
								if( m_HCE_FieldFrac.at(gset,j) <= 0.0 )
									continue;
							
								/*Call the receiver performance model - single point mode
								!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
								!only use for the fluid type in the single point model is calculating the convective heat transfer
								!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
								!steam that substituting the HTF here introduces negligible error.*/
							
								// For LF, HT = CT && sca_num = 0
								double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
								q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
								evac_tube_model.EvacReceiver( m_T_ave.at(i,0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc.at(i,0)/m_L_col.at(gset,0)*1000.0, gset, j, gset, 0, true, ncall,
								                             time/3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3 );
							
								if( q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs )
								{
									q_rec_loss = 0.0;
									q_rec_abs = 0.0;
								}
							
								m_q_loss.at(i,0) += q_rec_loss*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]
								m_q_abs.at(i,0) += q_rec_abs*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]
							
							}																																			
							
						}

						// Set the inlet enthalpy equal to the outlet of the previous node
						if(i > 0)
							m_h_in(i,0) = m_h_out(i-1,0);

						// Calculate the collector outlet enthalpy 
						double tol_t = 0.001;
						double err_t = 10.0*tol_t;
						int iter_t = 0;

						while( err_t > tol_t && iter_t < 50 )
						{
							iter_t++;
							// Calculate the average enthalpy value in the collector module
							m_h_out.at(i,0) = check_h.check( m_h_in.at(i,0) + m_q_abs.at(i,0)/m_dot_b_guess - (m_T_ave.at(i,0) - m_T_ave_prev.at(i,0))*m_e_trans/step );
							// Update guesses for h_ave and T_ave
							double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i,0))/2.0;
							// Update the average temperature for the heat loss calculation
							water_PH( P_loc*100.0, h_aveg, &wp );
							m_T_ave.at(i,0) = wp.temp;
							err_t = fabs( (m_h_ave.at(i,0) - h_aveg)/m_h_ave.at(i,0) );
							m_h_ave.at(i,0) = h_aveg;
						}

						// Predict the next outlet enthalpy
						if( i < m_nModTot - 1 )
							m_h_ave.at(i+1,0) = check_h.check( m_h_in.at(i,0) + (m_h_out.at(i,0)-m_h_in.at(i,0))*1.5 );

					}	// End step through boiler receiver modules

					err = (h_b_out - m_h_out.at(m_nModBoil-1,0))/h_b_out;

					if( m_dot_b == m_m_dot_min/m_x_b_des && err > 0.0 )		// M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
						break;

					// **************************************************
					// **** Hybrid False Position Iteration Method ******
					// **************************************************
					if( lowflag && upflag )
					{
						if( err > 0.0 )
						{
							m_dot_upper = m_dot_b;
							y_upper = err;
						}
						else
						{
							m_dot_lower = m_dot_b;
							y_lower = err;
						}
						m_dot_b_guess = y_upper/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
					}
					else
					{
						if(err > 0.0)			// Prescribed is greater than calculated, so decrease mass flow and set upper limit
						{
							m_dot_upper = m_dot_b;
							y_upper = err;
							upflag = true;
						}
						else
						{
							m_dot_lower = m_dot_b;
							y_lower = err;
							lowflag = true;
						}

						if( lowflag && upflag )		// If results of bracket are defined, use false position
							m_dot_b_guess = y_upper/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
						else		// If not, recalculate value based on approximate energy balance
						{
							if( iter < 3 )
							{
								double q_abs_sum = 0.0;
								for( int ii = 0; ii < m_nModBoil; ii++ )
									q_abs_sum += m_q_abs.at(ii,0);
								m_dot_b_guess = q_abs_sum / (h_b_out - h_b_in);
								m_dot_b_guess = max( m_m_dot_min/m_x_b_des*0.5, min( m_dot_b_guess, m_m_dot_b_max*1.5 ) );
							}
							else
								m_dot_b_guess = 0.5*m_dot_upper + 0.5*m_dot_lower;
						}
					}

					// ************************************************************************
					// *** End Hybrid False Position Iteration Method ***********
					// ************************************************************************
					if( m_dot_lower >= m_m_dot_b_max )		// Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
					{
						m_is_def = true;
						break;
					}

					if( m_dot_upper <= m_m_dot_min/m_x_b_des )		// Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
						m_dot_b_guess = m_m_dot_min/m_x_b_des;

				}   // End main BOILER iteration loop

				// Defocus calculations
				m_err_def = (m_dot_b_guess - m_m_dot_b_max)/m_m_dot_b_max;
				if( !m_is_def && m_err_def > 0.0 )
					m_is_def = true;
				if( m_is_def )
				{
					// Calculate new defocus
					m_defocus = min( 1.0, m_defocus_prev*pow( 1.0/(m_err_def+1.0), m_rc ));
					if( fabs(m_err_def) > m_tol_def )
					{
						m_defocus_prev = m_defocus;
						iter_def++;
						if( iter_def < 11 )
						{
							defocus_on = true;
							continue;
						}
					}
				}

				// Superheater
				if( m_is_sh )
				{
					// Choose which geometry set to use
					gset = 0;
					if( m_is_multgeom )
						gset = 1;

					// Calculate superheater inlet enthalpy
					water_PQ( check_pressure.P_check( P_turb_in+dP_basis*(m_fP_hdr_h+m_fP_sf_sh) )*100.0, 1.0, &wp );
					double h_sh_in = wp.enth;		//[kJ/kg]
					// The superheater outlet enthalpy is constrained according to the steam mass flow produced in the boiler
					water_TP( m_T_field_out_des, check_pressure.P_check( P_turb_in+dP_basis*m_fP_hdr_h )*100.0, &wp );
					double h_sh_out = wp.enth;		//[kJ/kg]

					// Set the loop inlet enthalpy
					m_h_in.at( m_nModBoil, 0 ) = h_sh_in;

					// Do a rough guess of the receiver enthalpy for the boiler
					double dh_sh = (h_sh_out - h_sh_in)/(double)m_nModSH;
					for( int ii = 0; ii < m_nModSH; ii++ )
					{
						int i = ii + m_nModBoil;
						m_h_ave.at(i,0) = h_sh_in + dh_sh*(double)(ii+1) - dh_sh/2.0;
					}

					m_dot = m_dot_b*m_x_b_des;
					double T_sh_guess = m_T_field_out_des;		// Guess the superheater outlet temp

					// Iterative loop to get convergence in heat loss
					double tol_sh = 0.01;
					double err_sh = 10.0*tol_sh;
					int iter_sh = 0;

					while( fabs(err_sh) > tol_sh && iter_sh < 5)
					{
						iter_sh++;

						// Initialize
						for( int i = m_nModBoil; i < m_nModTot; i++ )
						{
							m_q_loss.at(i,0) = 0.0;
							m_q_abs.at(i,0) = 0.0;
						}

						for( int ii = 0; ii < m_nModSH; ii++ )
						{
							int i = ii + m_nModBoil;
							
							// Calculate thermal losses based on temperature guess values
							// Calculate the local pressure in the superheater. Assume a linear pressure drop across each section
							double P_loc = check_pressure.P_check( P_turb_in + dP_basis*(m_fP_hdr_h+m_fP_sf_sh*(1.0 - (double)ii/(double)m_nModSH)) );

							// Get the temperature at each state in the boiler
							water_PH( P_loc*100.0, m_h_ave.at(i,0), &wp );
							m_T_ave.at(i,0) = wp.temp;

							// Calculate the heat loss at each temperature
							if( m_HLCharType.at(gset,0) == 1 )
							{
								// Estimate based on the polynomial adjustments provided by the user
								double dT_loc = m_T_ave.at(i,0) - T_db;
								double c_hl = m_HL_dT.at(gset,0) + m_HL_dT.at(gset,1)*dT_loc + m_HL_dT.at(gset,2)*pow(dT_loc,2) + m_HL_dT.at(gset,3)*pow(dT_loc,3) + m_HL_dT.at(gset,4)*pow(dT_loc,4);	//[W/m] Effect from dT
								if( m_HL_W.at(gset,0) != 0 || m_HL_W.at(gset,1) != 0 || m_HL_W.at(gset,2) != 0 || m_HL_W.at(gset,3) != 0 || m_HL_W.at(gset,0) != 0 )
									c_hl *= m_HL_W.at(gset,0) + m_HL_W.at(gset,1)*V_wind + m_HL_W.at(gset,2)*pow(V_wind,2) + m_HL_W.at(gset,3)*pow(V_wind,3) + m_HL_W.at(gset,4)*pow(V_wind,4);
								m_q_loss.at(i,0) = c_hl*m_L_col.at(gset,0)/1000.0;			//[kW] Total thermal loss from this collector
								m_q_abs.at(i,0) = m_q_rec.at(i,0) - m_q_loss.at(i,0);	//[kW] Total absorbed energy in this collector
							}
							else if( m_HLCharType.at(gset,0) == 2 )
							{
								// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)
								
								m_q_loss.at(i,0) = 0.0;
                    			m_q_abs.at(i,0) = 0.0;
								
								for( int j = 0; j < 4; j++ )
								{
                        			// Only calculate if the HCE fraction is non-zero
									if( m_HCE_FieldFrac.at(gset,j) <= 0.0 )
										continue;
								
									/*Call the receiver performance model - single point mode
									!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
									!only use for the fluid type in the single point model is calculating the convective heat transfer
									!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
                    				!steam that substituting the HTF here introduces negligible error.*/
								
									// For LF, HT = CT && sca_num = 0
									double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
									q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
									evac_tube_model.EvacReceiver( m_T_ave.at(i,0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc.at(i,0)/m_L_col.at(gset,0)*1000.0, gset, j, gset, 0, true, ncall,
									                             time/3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3 );
								
									if( q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs )
									{
										q_rec_loss = 0.0;
										q_rec_abs = 0.0;
									}
								
									m_q_loss.at(i,0) += q_rec_loss*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]
									m_q_abs.at(i,0) += q_rec_abs*m_L_col.at(gset,0)*m_HCE_FieldFrac.at(gset,j)/1000.0;		//[kW]
								
                        		}																																

							}

							// Set the inlet enthalpy equal to the outlet of the previous mode
							if( ii > 0 )
								m_h_in.at(i,0) = m_h_out.at(i-1,0);

							// Calculate the collector outlet enthalpy
							double tol_t = 0.001;
							double err_t = 10.0*tol_t;
							int iter_t = 0;

							while( err_t > tol_t && iter_t < 50 )
							{
								// Calculate the average enthalpy value in the collector module
								m_h_out.at(i,0) = check_h.check( m_h_in.at(i,0) + m_q_abs.at(i,0)/m_dot - (m_T_ave.at(i,0) - m_T_ave_prev.at(i,0))*m_e_trans/step );
								// Update guesses for h_ave and T_ave
								double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i,0))/2.0;
								// Update the average temperature for the heat loss calculation
								water_PH( P_loc*100.0, h_aveg, &wp );
								m_T_ave.at(i,0) = wp.temp;
								err_t = fabs( (m_h_ave.at(i,0) - h_aveg)/m_h_ave.at(i,0) );
								m_h_ave.at(i,0) = h_aveg;
							}

						}	// Step through superheater receivers

						err_sh = (T_sh_guess - m_T_ave.at( m_nModTot - 1, 0 ))/T_sh_guess;
						T_sh_guess = m_T_ave.at( m_nModTot - 1, 0 );

					}	// Main superheater iteration

				}		// End superheater
				else
				{
					//double m_dot_field = m_dot_b*m_x_b_des*(double)m_nLoops;	//[kg/s] The total field mass flow rate is just the saturated steam coming from the boiler section
				}

				m_dot_b_tot = m_dot_b * (double)m_nLoops;
			
			}	// End "standard" boiler + sh configuration	

		} while(defocus_on);

		// **** Calculate final solar field values ******
		// Total effective solar field mass flow rate
		double m_dot_field = m_dot*(double)m_nLoops;		//[kg/s]

		// Look up temperatures
		water_PH( check_pressure.P_check( P_turb_in + dP_basis*(m_fP_sf_tot-m_fP_hdr_c))*100.0, m_h_in.at(0,0), &wp );
		m_T_field_in = wp.temp - 273.15;		// [C]
		water_PH( check_pressure.P_check( P_turb_in + dP_basis*m_fP_hdr_h)*100.0, m_h_out.at(m_nModTot-1,0), &wp );
		double T_loop_out = wp.temp - 273.15;		// [C]

		// Piping thermal loss
		double q_loss_piping = m_Ap_tot * m_Pipe_hl_coef/1000.0 * ((m_T_field_in + T_loop_out)/2.0 - (T_db-273.15));	// hl coef is [W/m2-K], use average field temp as driving difference

		// Given the piping heat/pressure loss, calculate the temperature at the inlet to the power block
		double h_to_pb = 0.0;
		if( m_dot > 0.0 )
			h_to_pb = m_h_out.at( m_nModTot-1, 0 ) - q_loss_piping/m_dot_field;
		else
			h_to_pb = m_h_out.at( m_nModTot-1, 0 );

		water_PH( P_turb_in*100.0, h_to_pb, &wp );
		m_T_field_out = wp.temp - 273.15;		// [C]

		// Energies
		double q_inc_tot, q_rec_tot, q_abs_tot, q_loss_rec;
		q_inc_tot = q_rec_tot = q_abs_tot = q_loss_rec = 0.0;
		for( int i = 0; i < m_nModTot; i++ )
		{
			q_inc_tot += m_q_inc.at(i,0);
			q_rec_tot += m_q_rec.at(i,0);
			q_abs_tot += m_q_abs.at(i,0);
			q_loss_rec += m_q_loss.at(i,0);
		}
		q_inc_tot *= (double)m_nLoops/1000.0;
		q_rec_tot *= (double)m_nLoops/1000.0;
		q_abs_tot *= (double)m_nLoops/1000.0;
		q_loss_rec *= (double)m_nLoops/1000.0;
		q_loss_piping = q_loss_piping/1000.0;			//[MW] Thermal losses from the receiver
		double q_loss_sf = q_loss_rec + q_loss_piping;	//[MW] Total solar field losses, receiver + piping loss
		double q_field_delivered = m_dot_field*max( h_to_pb - h_pb_out, 0.0 );	//[kW] Energy balance indicating total energy delivered from the solar field
		double h_field_out = h_to_pb;					// h_field_out is corrected later if fossil energy is supplied in topping mode
		
		// 11.2.15 twn: corrected q_dump equation
		double q_dump = (1.0 - m_defocus)*q_inc_tot;	//[MW] Total amount of energy dumped by collector defocusing
			// this also works, assuming defocus is last multiplier before q_rec_tot:
		//double q_dump = q_inc_tot*(1.0/m_defocus - 1.0);


		double eta_thermal = 0.0;
		if( q_rec_tot > 0.0 )
			eta_thermal = 1.0 - min( max( q_loss_sf/q_rec_tot, 0.0 ), 1.0 );	//[-] thermal efficiency after reflection

		// Calculate solar field pressure drops all [bar]
		double dP_tot = dP_basis*m_fP_sf_tot;
		double dP_hdr_c = dP_basis*m_fP_hdr_c;
		double dP_sf_boil = dP_basis*m_fP_sf_boil;
		double dP_boil_to_SH = dP_basis*m_fP_boil_to_sh;
		double dP_sf_sh = dP_basis*m_fP_sf_sh;
		double dP_hdr_h = dP_basis*m_fP_hdr_h;
		
		// Calculate the total change in energy state
		double E_bal_startup = 0.0;
		double E_field = 0.0;
		for( int i = 0; i < m_nModTot; i++ )
		{
			E_bal_startup += (m_T_ave.at(i,0) - m_T_ave_prev.at(i,0))*m_e_trans/step*(double)m_nLoops/1000.0;	//[MW]
			E_field += (m_T_ave.at(i,0) - m_T_field_ini)*m_e_trans/step*(double)m_nLoops/1000.0;				//[MW]
		}


		//***********************************************************************
		//*** Control logic *******************
		//***********************************************************************
		// Check how much power is available from the aux backup
		double q_aux_avail = min( m_q_pb_des*m_ffrac[tou_period], m_q_max_aux );

		/*Fossil mode 3 allows fossil as supplemental temperature boosting. This requires flow from the field.
		it is also possible that the total of the energy from the field plus the topping energy does not 
		produce enough to meet the cycle cutoff fraction, so calculate here how much energy would be contributed
		from the fossil contribution */
		if( m_fossil_mode == 3 )
		{
			if( q_field_delivered <= 0.0 )
				q_aux_avail = 0.0;
			else
			{
				double h_target = 0.0;
				if( m_is_sh)
				{
					water_TP( m_T_field_out_des, P_turb_in*100.0, &wp );
					h_target = wp.enth;
				}
				else
				{
					water_PQ( P_turb_in*100.0, m_x_b_des, &wp );
					h_target = wp.enth;
				}
				// Thermal requirement for the aux heater
				q_aux_avail = max( 0.0, (h_target - h_field_out)*m_dot_field );
			}
		}

		// Calculate the total available energy for the power cycle as the sum of the energy from the solar field and the aux backup
		double q_avail_tot = q_aux_avail + max( 0.0, q_field_delivered );

		double T_pb_in = 0.0;
		double q_aux_fuel = 0.0;
		double m_dot_to_pb = 0.0;
		double q_to_pb = 0.0;
		int standby_control = 0;
		double q_aux = 0.0;
		double m_dot_aux = 0.0;
		// Do we have enough to run the power cycle?
		if( q_avail_tot >= m_q_pb_des*m_cycle_cutoff_frac )
		{
			if( q_aux_avail > 0.0 )
			{
				double h_target = 0.0;																
				double h_to_pb = 0.0;
				// Calculate the contribution from the aux backup
				switch( m_fossil_mode )
				{
				case 1:			// backup minimum level - parallel				
					if(m_is_sh)
					{
						water_TP( m_T_field_out_des, P_turb_in*100.0, &wp );
						h_target = wp.enth;
					}
					else
					{
						water_PQ( P_turb_in*100.0, m_x_b_des, &wp );
						h_target = wp.enth;
					}
					q_aux = max( 0.0, q_aux_avail - q_field_delivered );
					m_dot_aux = q_aux/(h_target - h_pb_out);
										
					if( q_field_delivered > 0.0 )
					{
						m_dot_to_pb = m_dot_aux + m_dot_field;
						h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux)/m_dot_to_pb;
					}
					else
					{
						m_dot_to_pb = m_dot_aux;
						h_to_pb = h_target;
					}
					water_PH( P_turb_in*100.0, h_to_pb, &wp );
					q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out);
					T_pb_in = wp.temp - 273.15;
					break;
				case 2:			// supplemental parallel
					if( m_is_sh )
					{
						water_TP( m_T_field_out_des, P_turb_in*100.0, &wp );
						h_target = wp.enth;
					}
					else
					{
						water_PQ( P_turb_in*100.0, m_x_b_des, &wp );
						h_target = wp.enth;
					}
					q_aux = min( m_q_pb_des - q_field_delivered, q_aux_avail );
					// For parallel operation, the result is a weighted mix of the field output and the boiler
					m_dot_aux = q_aux/(h_target - h_pb_out);
					m_dot_to_pb = 0.0;
					if(	q_field_delivered > 0.0 )
					{
						m_dot_to_pb = m_dot_aux + m_dot_field;
						h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux)/m_dot_to_pb;
					}
					else
					{
						m_dot_to_pb = m_dot_aux;
						h_to_pb = h_target;
					}
					water_PH( P_turb_in*100.0, h_to_pb, &wp );
					q_to_pb = m_dot_to_pb * (h_to_pb - h_b_in);
					T_pb_in = wp.temp - 273.15;
					break;
				case 3:		// Supplemental parallel
					// The auxiliary heater is used to bring the steam from the solar field up to the design-point temperature
					// for the power block. The fossil use corresponds to the operation level of the solar field
					 if( m_is_sh )
					 {
						 water_TP( m_T_field_out_des, P_turb_in*100.0, &wp );
						 h_target = wp.enth;
					 }
					 else
					 {
						 water_PQ( P_turb_in*100.0, m_x_b_des, &wp );
						 h_target = wp.enth;
					 }
					 // The flow rate through the aux heater is the same as through the field
					 m_dot_aux = m_dot_field;
					 // Thermal requirement for the aux heater
					 q_aux = min( max( 0.0, h_target - h_field_out )*m_dot_aux, q_aux_avail );
					 // Calcualte the enthalpy into the power block
					 h_to_pb = h_field_out + q_aux/m_dot_aux;
					 m_dot_to_pb = m_dot_field;
					 q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out);
					 water_PH( P_turb_in*100.0, h_to_pb, &wp );
					 T_pb_in = wp.temp - 273.15;
					break;
				}  // end switch
				q_aux_fuel = q_aux/m_LHV_eff;
			}   // end "if q_aux_avail > 0.0 
			else
			{
				// No aux backup, just the solar field
				m_dot_to_pb = m_dot_field;
				T_pb_in = m_T_field_out;
				q_to_pb = q_field_delivered;
			}
			// 3.2.17 twn: recalculated field outlet temperature so need to reset here
			m_T_field_out = T_pb_in;
			standby_control = 1;	// We're operating the power block normally	
			m_is_pb_on = true;
		}
		else
		{
			// There isn't enough energy to run the power block

			// Do we have enough to do standby?
			if( q_avail_tot > m_q_pb_des*m_q_sby_frac && m_t_sby_prev > 0.0 && m_is_pb_on_prev )
			{
				standby_control = 2;	// Operate in standby mode
				m_t_sby = max( 0.0, m_t_sby_prev-step/3600.0 );		
				q_aux = max( m_q_sby_frac*m_q_pb_des - q_field_delivered, 0.0 );
				m_dot_aux = 0.0;		// It's not meaningful to report the aux mass flow rate
				q_aux_fuel = q_aux/m_LHV_eff;
				q_to_pb = m_q_sby_frac*m_q_pb_des;
				T_pb_in = m_T_field_in_des;
				m_is_pb_on = true;
			}
			else
			{
				standby_control = 3;		// Turn off power block
				m_t_sby = m_t_sby_des;		// Reset standby time
				q_aux = 0.0;
				m_dot_aux = 0.0;
				q_field_delivered = 0.0;
				q_aux_fuel = 0.0;
				q_to_pb = 0.0;
				T_pb_in = m_T_field_out;
				m_is_pb_on = false;
			}
			m_dot_to_pb = 0.0;
		}

		// *** Calculate final plant values ****
		// Parasitic losses associated with the operation of the aux boiler
		double W_dot_aux = 0.0;
		if( q_aux > 0.0 )
			W_dot_aux = m_W_pb_des/1000.0 * m_aux_array[0] * m_aux_array[1] * (m_aux_array[2] + m_aux_array[3]*(q_aux/m_q_pb_des) + m_aux_array[4]*pow( q_aux/m_q_pb_des, 2) );

		// Balance of plant parasitics as a function of power block load
		double W_dot_bop = 0.0;
		if( q_to_pb > 0.0 )
			W_dot_bop = m_W_pb_des/1000.0 * m_bop_array[0] * m_bop_array[1] * (m_bop_array[2] + m_bop_array[3]*(q_to_pb/m_q_pb_des) + m_bop_array[4]*pow( q_to_pb/m_q_pb_des, 2 ) );

		// Parasitic electric power consumed by the collectors
		double W_dot_col = 0.0;
		if( CSP::pi/2.0 - SolarZen > 0.0 )
			W_dot_col = m_ftrack*m_SCA_drives_elec*m_Ap_tot/1.E6;	

		double W_dot_fixed = m_PB_fixed_par*m_W_pb_des/1000.0;	// Fixed parasitic power losses... for every hour of operation

		// If the power block isn't running, there's only a small pressure differential across the field
		if( q_to_pb <= 0.0 )
		{
			P_turb_in = m_P_turb_des * turb_pres_frac( m_cycle_cutoff_frac, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min );
			// m_cycle_cutoff_frac  mjw 11.15.11 The cycle cutoff restriction occasionally allows low pressure and steaming in the feedwater line. Avoid this.
			dP_hdr_c = dP_basis * m_fP_hdr_c;
			dP_sf_boil = dP_basis * m_fP_sf_boil;
			dP_boil_to_SH = dP_basis * m_fP_boil_to_sh;
			dP_sf_sh = dP_basis * m_fP_sf_sh;
			dP_hdr_h = dP_basis * m_fP_hdr_h;
			dP_tot = dP_basis * (m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h);
		}

		// Feedwater pump parasitic
		water_PQ( check_pressure.P_check( P_turb_in+dP_basis*(m_fP_hdr_c+m_fP_sf_boil+m_fP_boil_to_sh+m_fP_sf_sh+m_fP_hdr_h))*100.0, 0.0, &wp );
		double T_low_limit = wp.temp;
		water_TP( min(T_pb_out, T_low_limit), check_pressure.P_check( P_turb_in+dP_basis*(m_fP_hdr_c+m_fP_sf_boil+m_fP_boil_to_sh+m_fP_sf_sh+m_fP_hdr_h))*100.0, &wp );
		double rho_fw = wp.dens;

		double W_dot_pump = 0.0;
		if( m_is_oncethru )
			W_dot_pump = m_dot_field*dP_tot/rho_fw/m_eta_pump*0.1;
		else
			W_dot_pump = (m_dot_field*(m_fP_hdr_c+m_fP_boil_to_sh+m_fP_sf_sh+m_fP_hdr_h)+m_dot_field/m_x_b_des*m_fP_sf_boil)*dP_basis/rho_fw/m_eta_pump*0.1;	//[MW] P_turb in in bar

		// Solar field efficiency
		double eta_sf = eta_opt_ave * eta_thermal;

		// Limit the reported values of solar azimuth/elevation
		double solaralt = max( 0.0, CSP::pi/2 - SolarZen );
		if( SolarZen >= CSP::pi/2 )
			SolarAz = 0.0;

		// Part-load control is always 2
		double cycle_pl_control = 2.0;

		// Set Outputs
		
		value( O_CYCLE_PL_CONTROL, cycle_pl_control );        //[none] Part-load control flag - used by Type224
		value( O_DP_TOT, dP_tot );							  //[bar] Total HTF pressure drop
		value( O_DP_HDR_C, dP_hdr_c );						  //[bar] Average cold header pressure drop
		value( O_DP_SF_BOIL, dP_sf_boil );					  //[bar] Pressure drop across the solar field boiler
		value( O_DP_BOIL_TO_SH, dP_boil_to_SH );			  //[bar] Pressure drop between the boiler and superheater
		value( O_DP_SF_SH, dP_sf_sh );						  //[bar] Pressure drop across the solar field superheater
		value( O_DP_HDR_H, dP_hdr_h );						  //[bar] Average hot header pressure drop
		value( O_E_BAL_STARTUP, E_bal_startup );			  //[MW] Startup energy consumed
		value( O_E_FIELD, E_field );						  //[MW-hr] Accumulated internal energy in the entire solar field
		value( O_E_FP_TOT, 0.0 );							  //[J] Freeze protection energy
		value( O_ETA_OPT_AVE, eta_opt_ave );				  //[none] Collector equivalent optical efficiency
		value( O_ETA_THERMAL, eta_thermal );				  //[none] Solar field thermal efficiency (power out/ANI)
		value( O_ETA_SF, eta_sf );    						  //[none] Total solar field collection efficiency
		value( O_DEFOCUS, m_defocus );     					  //[none] The fraction of focused aperture area in the solar field
		value( O_M_DOT_AUX, m_dot_aux*3600 );				  //[kg/s] --> [kg/hr] Auxiliary heater mass flow rate
		value( O_M_DOT_FIELD, m_dot_field*3600 );			  //[kg/s] --> [kg/hr] Flow rate from the field
		value( O_M_DOT_B_TOT, m_dot_b_tot*3600 );			  //[kg/s] --> [kg/hr] Flow rate within the boiler section
		value( O_M_DOT,	m_dot );							  //[kg/s] Flow rate in a single loop
		value( O_M_DOT_TO_PB, m_dot_to_pb*3600 );			  //[kg/s] --> [kg/hr] Flow rate delivered to the power block         
		value( O_P_TURB_IN, P_turb_in );			               //[bar] Pressure at the turbine inlet		
		value( O_Q_LOSS_PIPING, q_loss_piping );				   //[MW] Pipe heat loss in the hot header and the hot runner
		value( O_Q_AUX_FLUID, q_aux*0.001 );					   //[MW] Thermal energy provided to the fluid passing through the aux heater
		value( O_Q_AUX_FUEL, q_aux_fuel*3.412E-3 );				   //[W] --> [MMBTU] Heat content of fuel required to provide aux firing
		value( O_Q_DUMP, q_dump );								   //[MW] Dumped thermal energy
		value( O_Q_FIELD_DELIVERED, q_field_delivered*0.001 );	   //[kW] --> [MW] Total solar field thermal power delivered
		value( O_Q_INC_TOT, q_inc_tot );						   //[MW] Total power incident on the field
		value( O_Q_LOSS_REC, q_loss_rec );						   //[MW] Total Receiver thermal losses
		value( O_Q_LOSS_SF, q_loss_sf );						   //[MW] Total solar field thermal losses
		value( O_Q_TO_PB, q_to_pb*0.001 );						   //[kW] --> [MW] Thermal energy to the power block
		value( O_SOLARALT, solaralt*57.2958 );					   //[rad] --> [deg] Solar altitude used in optical calculations
		value( O_SOLARAZ, SolarAz*57.2958 );					   //[rad] --> [deg] Solar azimuth used in optical calculations
		value( O_PHI_T, phi_t*57.2958 );						   //[rad] --> [deg] Transversal solar incidence angle
		value( O_THETA_L, theta_L*57.2958 );					   //[rad] --> [deg] Longitudinal solar incidence angle
		value( O_STANDBY_CONTROL, standby_control );			   //[none] Standby control flag - used by Type224
		value( O_T_FIELD_IN, m_T_field_in );						   //[C] HTF temperature into the collector field header
		value( O_T_FIELD_OUT, m_T_field_out );					   //[C] HTF Temperature from the field
		value( O_T_LOOP_OUT, T_loop_out );						   //[C] Loop outlet temperature
		value( O_T_PB_IN, T_pb_in );							   //[C] HTF Temperature to the power block
		value( O_W_DOT_AUX,	W_dot_aux );						   //[MW] Parasitic power associated with operation of the aux boiler
		value( O_W_DOT_BOP, W_dot_bop );						   //[MW] parasitic power as a function of power block load
		value( O_W_DOT_COL,	W_dot_col );						   //[MW] Parasitic electric power consumed by the collectors
		value( O_W_DOT_FIXED, W_dot_fixed );					   //[MW] Fixed parasitic power losses.. for every hour of operation
		value( O_W_DOT_PUMP, W_dot_pump );						   //[MW] Required solar field pumping power
		value( O_W_DOT_PAR_TOT, W_dot_aux+W_dot_bop+W_dot_col+W_dot_fixed+W_dot_pump );	//[MW] Total parasitic power losses
		value( O_P_SF_IN, P_turb_in + dP_tot );						//[bar] Solar field inlet pressure

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		for( int i = 0; i < m_nModTot; i++ )
			m_T_ave_prev.at(i,0) = m_T_ave.at(i,0);

		m_t_sby_prev = m_t_sby;

		m_is_pb_on_prev = m_is_pb_on;

		m_T_sys_prev = min( m_T_field_in, m_T_field_out ) + 273.15;	//[K] Last field outlet temperature

		check_pressure.report_and_reset();
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_lf_type261_steam, "Linear Fresnel Steam Receiver", "Ty Neises", 1, sam_mw_lf_type261_steam_variables, NULL, 1 )

