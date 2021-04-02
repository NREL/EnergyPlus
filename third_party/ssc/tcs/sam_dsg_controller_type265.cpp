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

#include "direct_steam_receivers.h"

//#include "waterprop.h"
#include "water_properties.h"

enum {
	P_fossil_mode,
	P_q_pb_design,    
	P_q_aux_max,      
	P_lhv_eff,        
	P_h_tower,        
	P_n_panels,   
	P_flowtype,
	P_d_rec,          
	P_q_rec_des,      
	P_f_rec_min,      
	P_rec_qf_delay,   
	P_rec_su_delay,   
	P_f_pb_cutoff,    
	P_f_pb_sb,        
	P_t_standby_ini,  
	P_x_b_target,     
	P_eta_rec_pump,   
	P_P_hp_in_des,      
	P_P_hp_out_des,     
	P_f_mdotrh_des,   
	P_p_cycle_design, 
	P_ct,             
	P_T_amb_des,          
	P_dT_cw_ref,      
	P_T_approach,     
	P_T_ITD_des,   
	P_hl_ffact,   
	P_h_boiler,   
	P_d_t_boiler, 
	P_th_t_boiler,
	P_emis_boiler,
	P_abs_boiler, 
	P_mat_boiler,  
	P_h_sh,       
	P_d_sh,       
	P_th_sh,      
	P_emis_sh,    
	P_abs_sh,  
	P_mat_sh,  
	P_T_sh_out_des,
	P_h_rh,    
	P_d_rh,    
	P_th_rh,   
	P_emis_rh, 
	P_abs_rh,  
	P_mat_rh,  
	P_T_rh_out_des,
	P_cycle_max_frac,
	P_A_sf,  
	P_ffrac,
//		P_fluxmap_angles,
//		P_fluxmap,       
	P_n_flux_x,
	P_n_flux_y,
	//P_TOU_schedule,

	I_azimuth,
	I_zenith,     
	I_I_bn,       
	I_T_amb,      
	I_v_wind_10,  
	I_P_atm,             
	I_T_dp,       
	I_field_eff,  
	I_P_b_in,     
	I_f_mdotrh,   
	I_P_hp_out,   
	I_T_hp_out,   
	I_T_rh_target,
	I_T_fw,         
	I_P_cond,     
	I_TOUPeriod,
	I_flux_map,

	O_T_fw,
	O_T_b_in,      
	O_T_boil,      
	O_P_b_in,      
	O_P_b_out,     
	O_P_drop_b,    
	O_m_dot_b,          
	O_eta_b,       
	O_q_b_conv,    
	O_q_b_rad,     
	O_q_b_abs,     
	O_T_max_b_surf,
	O_m_dot_sh,    
	O_P_sh_out,    
	O_dP_sh,       
	O_eta_sh, 
	O_q_sh_conv,   
	O_q_sh_rad,    
	O_q_sh_abs,    
	O_T_max_sh_surf,
	O_v_sh_max,    
	O_f_mdot_rh,   
	O_P_rh_in,     
	O_T_rh_in,     
	O_P_rh_out,    
	O_T_rh_out,    
	O_dP_rh,       
	O_eta_rh,      
	O_T_max_rh_surf,
	O_v_rh_max,    
	O_q_rh_conv,   
	O_q_rh_rad,    
	O_q_rh_abs,  
	O_q_inc_full,  
	O_q_inc_actual,
	O_defocus,     
	O_field_eta_adj,
	O_q_abs_rec,   
	O_q_conv_rec,  
	O_q_rad_rec,   
	O_q_abs_less_rad,
	O_q_therm_in_rec,
	O_eta_rec,
	O_W_dot_boost, 
	O_m_dot_aux,   
	O_q_aux,       
	O_q_aux_fuel,  
	O_standby_control,
	O_f_timestep,  
	O_m_dot_toPB,  

	N_MAX };

tcsvarinfo sam_dsg_controller_type265_variables[] = {
	{ TCS_PARAM,  TCS_NUMBER, P_fossil_mode,    "fossil_mode",            "Fossil model",                               "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_q_pb_design,    "q_pb_design",			  "Heat rate into powerblock at design",        "MW",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_q_aux_max,      "q_aux_max",              "Maximum heat rate of auxiliary heater",      "MW",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_lhv_eff,        "lhv_eff",                "Aux Heater lower heating value efficiency",  "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_h_tower,        "h_tower",                "Tower Height",                               "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_n_panels,       "n_panels",               "Number of panels",                           "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_flowtype,       "flowtype",               "Code for flow pattern through rec.",         "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_d_rec,          "d_rec",                  "Diameter of Receiver",                       "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_q_rec_des,      "q_rec_des",              "Design-point thermal power",                 "MW",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_f_rec_min,      "f_rec_min",              "Minimum receiver absorbed power fraction",   "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_rec_qf_delay,   "rec_qf_delay",           "Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour", "-", "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_rec_su_delay,   "rec_su_delay",			  "Receiver start-up delay time",               "hr",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_f_pb_cutoff,    "f_pb_cutoff",            "Cycle cut-off fraction",                     "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_f_pb_sb,        "f_pb_sb",                "Cycle minimum standby fraction",             "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_t_standby_ini,  "t_standby_ini",          "Power block standby time",                   "hr",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_x_b_target,     "x_b_target",             "Target boiler outlet quality",               "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_eta_rec_pump,   "eta_rec_pump",           "Feedwater pump efficiency",                  "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_P_hp_in_des,    "P_hp_in_des",            "Design HP Turbine Inlet Pressure",           "bar", "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_P_hp_out_des,   "P_hp_out_des",           "Design HP Turbine Outlet Pressure",          "bar", "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_f_mdotrh_des,   "f_mdotrh_des",           "Design reheat mass flow rate fraction",      "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_p_cycle_design, "p_cycle_design",         "Design Cycle Power",                         "MW",  "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_ct,             "ct",                     "Cooling Type",                               "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_T_amb_des,      "T_amb_des",              "Design ambient temperature (power cycle)",   "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_dT_cw_ref,      "dT_cw_ref",              "Reference condenser water dT",               "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_T_approach,     "T_approach",             "Approach temperature for wet cooling",       "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_T_ITD_des,      "T_ITD_des",              "Approach temperature for dry cooling",       "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_hl_ffact,       "hl_ffact",               "Heat Loss Fudge FACTor",                     "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_h_boiler,       "h_boiler",               "Height of boiler",                           "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_d_t_boiler,     "d_t_boiler",             "O.D. of boiler tubes",                       "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_th_t_boiler,    "th_t_boiler",            "Thickness of boiler tubes",                  "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_emis_boiler,    "emis_boiler",            "Emissivity of boiler tubes",                 "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_abs_boiler,     "abs_boiler",             "Absorptance of boiler tubes",                "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_mat_boiler,     "mat_boiler",             "Numerical code for tube material",           "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_h_sh,           "h_sh",                   "Height of superheater",                      "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_d_sh,           "d_sh",                   "O.D. of superheater tubes",                  "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_th_sh,          "th_sh",                  "Thickness of superheater tubes",             "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_emis_sh,        "emis_sh",                "Emissivity of superheater tubes",            "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_abs_sh,         "abs_sh",                 "Absorptance of superheater tubes",           "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_mat_sh,         "mat_sh",                 "Numerical code for superheater material",    "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_T_sh_out_des,   "T_sh_out_des",           "Target superheater outlet temperature",      "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_h_rh,           "h_rh",                   "Height of reheater",                         "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_d_rh,           "d_rh",                   "O.D. of reheater tubes",                     "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_th_rh,          "th_rh",                  "Thickness of reheater tubes",                "m",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_emis_rh,        "emis_rh",                "Emissivity of reheater tubes",               "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_abs_rh,         "abs_rh",                 "Absorptance of reheater tubes",              "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_mat_rh,         "mat_rh",                 "Numerical code for reheater material",       "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_T_rh_out_des,   "T_rh_out_des",           "Target reheater outlet temperature",         "C",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_cycle_max_frac, "cycle_max_frac",         "Cycle maximum overdesign fraction",          "-",   "", "", "" },
	{ TCS_PARAM,  TCS_NUMBER, P_A_sf,           "A_sf",                   "Solar field area",                           "m^2", "", "", "" },
	{ TCS_PARAM,  TCS_ARRAY,  P_ffrac,          "ffrac",                  "Fossil dispatch logic",                      "-",   "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_n_flux_x, "n_flux_x", "Receiver flux map resolution - X", "-", "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_n_flux_y, "n_flux_y", "Receiver flux map resolution - Y", "-", "", "", "" },
	//{TCS_PARAM, TCS_MATRIX, P_fluxmap_angles,   "fluxmap_angles",   "Matrix containing zenith and azimuth angles for flux maps",                "-",        "2 columns - azimuth angle, zenith angle. number of rows must equal number of flux maps provided", "", "" },
	//{TCS_PARAM, TCS_MATRIX, P_fluxmap,          "fluxmap",          "Matrix containing flux map for various solar positions",                   "-",        "", "", "" },


	{ TCS_INPUT,  TCS_NUMBER, I_azimuth,     "azimuth",                "Solar azimuth",                   "deg",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_zenith,      "zenith",                 "Solar zenith",                    "deg",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_I_bn,        "DNI",                    "Direct normal irradiance",        "W/m^2", "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T_amb,       "T_amb",                  "Ambient dry bulb temperature",    "C",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_v_wind_10,   "v_wind_10",              "Wind speed at 10 m",              "m/s",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_P_atm,       "P_atm",                  "Ambient Pressure",                "atm",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T_dp,        "T_dp",                   "Dew point temperature",           "C",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_field_eff,   "field_eff",              "Heliostat field efficiency",      "-",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_P_b_in,      "P_b_in",                 "Boiler inlet pressure",           "bar",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_f_mdotrh,    "f_mdot_rh",              "Reheat mass flow rate fraction",  "-",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_P_hp_out,    "P_hp_out",               "HP turbine outlet pressure",      "bar",   "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T_hp_out,    "T_hp_out",               "HP turbine outlet temperature",   "C",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T_rh_target, "T_rh_target",            "Target reheater outlet temp.",    "C",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T_fw,        "T_fw",                   "Feedwater outlet temperature",    "C",     "", "", "" },
	{ TCS_INPUT,  TCS_NUMBER, I_P_cond,      "P_cond",                 "Condenser pressure",              "Pa",    "", "", "" },	
	{ TCS_INPUT,  TCS_NUMBER, I_TOUPeriod,   "TOUPeriod",              "The time-of-use period",          "",      "", "", "" },
	{ TCS_INPUT, TCS_MATRIX, I_flux_map, "flux_map", "Receiver flux map", "-", "", "", "" },



	{ TCS_OUTPUT, TCS_NUMBER, O_T_fw,        "T_fw",				   "Feedwater outlet temp",                  "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_b_in,      "T_b_in",                 "Boiler Inlet Temperature",               "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_boil,      "T_boiling",              "Boiler temp (= recirc temp, drum temp)", "C",     "", "", "" }, 
	{ TCS_OUTPUT, TCS_NUMBER, O_P_b_in,      "P_b_in",                 "Boiler inlet pressure",                  "kPa",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_b_out,     "P_b_out",                "Boiler outlet pressure",                 "kPa",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_drop_b,    "P_drop_b",               "Boiler pressure drop",                   "Pa",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_b,     "m_dot_b",                "Mass flow rate through boiler",          "kg/hr", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_eta_b,       "eta_b",                  "Boiler thermal efficiency",              "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_b_conv,    "q_b_conv",               "Boiler convective losses",               "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_b_rad,     "q_b_rad",                "Boiler radiative losses",                "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_b_abs,     "q_b_abs",                "Boiler absorbed power",                  "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_max_b_surf,"T_max_b_surf",           "Max boiler surface temperature",         "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_sh,    "m_dot_sh",               "Mass flow rate through superheater",     "kg/hr", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_sh_out,    "P_sh_out",               "Superheater outlet pressure",            "kPa",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_dP_sh,       "dP_sh",                  "Superheater pressure drop",              "Pa",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_eta_sh,      "eta_sh",                 "Superheater thermal efficiency",         "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_sh_conv,   "q_sh_conv",              "Superheater convective losses",          "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_sh_rad,    "q_sh_rad",               "Superheater radiative losses",           "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_sh_abs,    "q_sh_abs",               "Superheater absorbed power",             "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_max_sh_surf, "T_max_sh_surf",        "Max superheater surface temperature",    "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_v_sh_max,    "v_sh_max",               "Superheater exit velocity",              "m/s",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_f_mdot_rh,   "f_mdot_rh",              "Reheater mass flow rate fraction",       "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_rh_in,     "P_rh_in",                "Reheater inlet pressure",                "kPa",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_rh_in,     "T_rh_in",                "Reheater inlet temperature",             "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_rh_out,    "P_rh_out",               "Reheater outlet pressure",               "kPa",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_rh_out,    "T_rh_out",               "Reheater outlet temperature",            "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_dP_rh,       "dP_rh",                  "Pressure drop through reheater",         "Pa",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_eta_rh,      "eta_rh",                 "Thermal efficiency of reheater",         "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_max_rh_surf, "T_max_rh_surf",        "Max reheater surface temperature",       "C",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_v_rh_max,    "v_rh_max",               "Exit velocity through reheater",         "m/s",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_rh_conv,   "q_rh_conv",              "Reheater convective losses",             "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_rh_rad,    "q_rh_rad",               "Reheater radiative losses",              "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_rh_abs,    "q_rh_abs",               "Reheater absorbed power",                "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_inc_full,  "q_inc_full",             "Incident radiation before defocus",      "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_inc_actual,"q_inc_actual",           "Incident radiation after defocus",       "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_defocus,     "defocus",                "Defocus fraction",                       "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_field_eta_adj,"field_eta_adj",         "Adjusted field efficiency",              "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_abs_rec,   "q_abs_rec",              "Total receiver absorbed power",          "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_conv_rec,  "q_conv_rec",             "Total receiver convective losses",       "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_rad_rec,   "q_rad_rec",              "Total receiver radiative losses",        "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_abs_less_rad, "q_abs_less_rad",      "Absorbed power less rad losses",         "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_therm_in_rec,"q_therm_in_rec",       "Rate of energy into steam/water",        "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_eta_rec,     "eta_rec",                "Receiver thermal efficiency",            "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_W_dot_boost, "W_dot_boost",            "Feedwater booster pump power",           "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_aux,   "m_dot_aux",              "Auxiliary mass flow rate",               "kg/hr", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_aux,       "q_aux",                  "Auxiliary heat rate delivered to cycle", "MW",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_q_aux_fuel,  "q_aux_fuel",             "Fuel energy rate to aux heater",         "MMBTU", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_standby_control, "standby_control",    "1: PB on, 2: Standby, 3: Off",           "-",     "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_f_timestep,  "f_timestep",             "Fraction of timestep turbine can operate", "-",   "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_toPB,  "m_dot_toPB",             "Mass flow rate to powerblock (rec + aux)", "kg/hr", "", "", "" },


	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

class sam_dsg_controller_type265 : public tcstypeinterface
{
private:
	//Code Constants
	double m_tol_T_rh;
	double m_tol_T_sh_base;
	double m_tol_T_sh_high;
	double m_bracket_tol;

	//Class Instances
	C_DSG_macro_receiver dsg_rec;
	C_DSG_Boiler boiler;
	C_DSG_Boiler superheater;
	C_DSG_Boiler reheater;

	// Parameters
	int    m_fossil_mode;  
	double m_q_pb_design;	
	double m_q_aux_max;    
	double m_lhv_eff;      
	double m_h_tower;           
	double m_q_rec_des;    
	double m_f_rec_min;    
	double m_rec_qf_delay; 
	double m_rec_su_delay;	
	double m_f_pb_cutoff;  
	double m_f_pb_sb;      
	double m_t_standby_ini;
	double m_x_b_target;   
	double m_eta_rec_pump; 

	double m_P_hp_in_des;     
	double m_P_hp_out_des;    
	double m_f_mdotrh_des;  
	double m_p_cycle_design;
	double m_ct;            
	double m_T_amb_des;         
	double m_dT_cw_ref;     
	double m_T_approach;    
	double m_T_ITD_des;       

	/*double m_h_boiler;      	
	double m_d_t_boiler;    
	double m_th_t_boiler;   
	double m_emis_boiler;   
	double m_abs_boiler;    
	double m_mat_boiler;    
	double m_th_fin;        
	double m_l_fin;         
	double m_emis_fin;      
	double m_abs_fin;       
	double m_mat_fin;  */     
	
	double m_h_sh;          
	double m_d_sh;          
	double m_th_sh;         
	double m_emis_sh;       
	double m_abs_sh;        
	double m_mat_sh;        
	double m_T_sh_out_des;      
	double m_h_rh;          
	double m_d_rh;          
	double m_th_rh;         
	double m_emis_rh;       
	double m_abs_rh;        
	double m_mat_rh;        
	double m_T_rh_out_des;   
	double m_cycle_max_frac;
	double m_A_sf;
	double * m_ffrac;
	int m_numtou;
	//double * m_tou_schedule;
	//int m_l_tou_schedule;
	int m_n_flux_x;
	int m_n_flux_y;
	double *m_i_flux_map;

	//util::matrix_t<double> fluxmap_angles;	// matrix for fluxmap solar positions
	//util::matrix_t<double> fluxmap;         // matrix for flux values
	//int m_num_sol_pos;

	// Calculated
	double m_q_rec_min;
	double m_q_pb_min;
	double m_q_sb_min;
	double m_eta_des;
	double m_P_b_in_min;
	double m_P_hp_out_min;

	int m_high_pres_count;
	bool m_eta_lin_approx;
	bool m_q_low_set;
	bool m_q_high_set;

	double m_azimuth;
	double m_zenith;
	double m_I_bn;
	double m_T_amb;
	double m_P_atm;
	double m_hour;
	double m_T_dp;
	int    m_touperiod;
	double m_field_eff;
	double m_h_total;
	double m_v_wind;
	double m_T_sky;

	double m_q_aux;
	double m_m_dot_aux;
	double m_defocus;
	bool m_df_flag;

	//util::matrix_t<double> m_solarflux;		// matrix for solar flux values at timestep
	util::matrix_t<double> m_flux_in;

	
	util::matrix_t<double> m_q_inc_base;
	util::matrix_t<double> m_q_inc;
	util::matrix_t<double> m_q_inc_rh;
	util::matrix_t<double> m_q_inc_b;
	util::matrix_t<double> m_q_inc_sh;

	string m_msg;

	bool m_success;
	double m_q_total;
	double m_A_panel;
	double m_q_total_df;

	double m_eta_b_high, m_eta_b_low, m_eta_sh_high, m_eta_sh_low, m_eta_rh_high, m_eta_rh_low, m_q_total_high, m_q_total_low;
	double m_eta_b_ref, m_eta_sh_ref, m_eta_rh_ref;
	double m_mguessmult;

	double m_q_b_des_sp, m_q_sh_des_sp, m_q_rh_des_sp;
	double m_m_dot_ref;
	double m_m_dot_des;
	double m_m_dot_ND;

	double m_Psat_des;
	double m_deltaT_fw_des, m_h_sh_in_ref, m_T_boil_pred;
	double m_s_sh_out_ref, m_h_lp_isen_ref, m_h_rh_in_ref, m_h_sh_out_ref, m_rho_hp_out, m_dp_rh_up, m_P_rh_in;
	double m_h_rh_out_ref, m_h_fw;
	double m_q_pb_max;
	double m_m_dot_guess, m_f_rh, m_f_b, m_P_sh_out_min, m_P_rh_out_min;
	int m_b_EB_count;
	double m_h_hp_out;
	double m_h_boiler;

	// Stored
	double m_E_su_rec_prev;	
	double m_t_su_rec_prev;	
	double m_t_sb_pb_prev;	
	double m_E_su_rec;
	double m_t_su_rec;
	double m_t_sb_pb;

	double m_diff_m_dot_old_ncall;
	double m_diff_m_dot_out_ncall;
	double m_m_dot_prev_ncall;
	double m_dp_b_prev_ncall;
	double m_dp_sh_prev_ncall;
	double m_dp_rh_prev_ncall;
	double f_timestep_prev_ncall;

	int m_standby_control;

public:
	sam_dsg_controller_type265( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		//Code Constants
			//Solving (absolute) tolerances for reheater and superheater exit temperatures
		m_tol_T_rh = 0.005;			//[-]
		m_tol_T_sh_base = 0.005;	//[-]
		m_tol_T_sh_high = 0.03;		//[-]
		m_bracket_tol = 0.001;			//[-] Boiler fraction bracket

		m_fossil_mode = -1; 
		m_q_pb_design = std::numeric_limits<double>::quiet_NaN(); 	
		m_q_aux_max = std::numeric_limits<double>::quiet_NaN();     
		m_lhv_eff = std::numeric_limits<double>::quiet_NaN();
		m_h_tower = std::numeric_limits<double>::quiet_NaN();
		m_q_rec_des = std::numeric_limits<double>::quiet_NaN();
		m_f_rec_min = std::numeric_limits<double>::quiet_NaN();
		m_rec_qf_delay = std::numeric_limits<double>::quiet_NaN();
		m_rec_su_delay = std::numeric_limits<double>::quiet_NaN();
		m_f_pb_cutoff = std::numeric_limits<double>::quiet_NaN();
		m_f_pb_sb = std::numeric_limits<double>::quiet_NaN();
		m_t_standby_ini = std::numeric_limits<double>::quiet_NaN();
		m_x_b_target = std::numeric_limits<double>::quiet_NaN();
		m_eta_rec_pump = std::numeric_limits<double>::quiet_NaN();
		
		m_P_hp_in_des = std::numeric_limits<double>::quiet_NaN();
		m_P_hp_out_des = std::numeric_limits<double>::quiet_NaN();
		m_f_mdotrh_des = std::numeric_limits<double>::quiet_NaN();
		m_p_cycle_design = std::numeric_limits<double>::quiet_NaN();
		m_ct = std::numeric_limits<double>::quiet_NaN();
		m_T_amb_des = std::numeric_limits<double>::quiet_NaN();
		m_dT_cw_ref = std::numeric_limits<double>::quiet_NaN();
		m_T_approach = std::numeric_limits<double>::quiet_NaN();
		m_T_ITD_des = std::numeric_limits<double>::quiet_NaN();

		m_h_sh = std::numeric_limits<double>::quiet_NaN();          
		m_d_sh = std::numeric_limits<double>::quiet_NaN();          
		m_th_sh = std::numeric_limits<double>::quiet_NaN();         
		m_emis_sh = std::numeric_limits<double>::quiet_NaN();       
		m_abs_sh = std::numeric_limits<double>::quiet_NaN();        
		m_mat_sh = std::numeric_limits<double>::quiet_NaN();        
		m_T_sh_out_des = std::numeric_limits<double>::quiet_NaN();  
		m_h_rh = std::numeric_limits<double>::quiet_NaN();          
		m_d_rh = std::numeric_limits<double>::quiet_NaN();          
		m_th_rh = std::numeric_limits<double>::quiet_NaN();         
		m_emis_rh = std::numeric_limits<double>::quiet_NaN();       
		m_abs_rh = std::numeric_limits<double>::quiet_NaN();        
		m_mat_rh = std::numeric_limits<double>::quiet_NaN();        
		m_T_rh_out_des = std::numeric_limits<double>::quiet_NaN();  
		m_cycle_max_frac = std::numeric_limits<double>::quiet_NaN();
		m_A_sf = std::numeric_limits<double>::quiet_NaN();

		m_ffrac = 0;
		m_numtou = -1;
		//m_tou_schedule = 0;
		//m_l_tou_schedule = -1;
		
		/*fluxmap_angles  = 0.0;
		fluxmap         = 0.0;
		m_num_sol_pos   = -1;*/

		// Calculated
		m_q_rec_min = std::numeric_limits<double>::quiet_NaN();
		m_q_pb_min = std::numeric_limits<double>::quiet_NaN();
		m_q_sb_min = std::numeric_limits<double>::quiet_NaN();
		m_eta_des = std::numeric_limits<double>::quiet_NaN();
		m_P_b_in_min = std::numeric_limits<double>::quiet_NaN();
		m_P_hp_out_min = std::numeric_limits<double>::quiet_NaN();

		m_high_pres_count = -1;
		m_eta_lin_approx = false;
		m_q_low_set = false;
		m_q_high_set = false;

		m_azimuth = std::numeric_limits<double>::quiet_NaN();
		m_zenith = std::numeric_limits<double>::quiet_NaN();
		m_I_bn = std::numeric_limits<double>::quiet_NaN();
		m_T_amb = std::numeric_limits<double>::quiet_NaN();
		m_P_atm = std::numeric_limits<double>::quiet_NaN();
		m_hour = std::numeric_limits<double>::quiet_NaN();
		m_T_dp = std::numeric_limits<double>::quiet_NaN();
		m_touperiod = -1;
		m_field_eff = std::numeric_limits<double>::quiet_NaN();
		m_h_total = std::numeric_limits<double>::quiet_NaN();
		m_v_wind = std::numeric_limits<double>::quiet_NaN();
		m_T_sky = std::numeric_limits<double>::quiet_NaN();

		m_q_aux = m_m_dot_aux = m_defocus = std::numeric_limits<double>::quiet_NaN();
		m_df_flag = false;
		/*m_solarflux.resize(12, 1);
		m_solarflux.fill(0.0);*/
		m_success = false;				
		m_q_total = std::numeric_limits<double>::quiet_NaN();	
		m_A_panel = std::numeric_limits<double>::quiet_NaN();
		m_q_total_df = std::numeric_limits<double>::quiet_NaN();
		m_eta_b_high = std::numeric_limits<double>::quiet_NaN(); 
		m_eta_b_low = std::numeric_limits<double>::quiet_NaN();
		m_eta_sh_high = std::numeric_limits<double>::quiet_NaN();
		m_eta_sh_low = std::numeric_limits<double>::quiet_NaN();
		m_eta_rh_high = std::numeric_limits<double>::quiet_NaN();
		m_eta_rh_low = std::numeric_limits<double>::quiet_NaN();
		m_q_total_high = std::numeric_limits<double>::quiet_NaN();
		m_q_total_low = std::numeric_limits<double>::quiet_NaN();
		m_eta_b_ref = std::numeric_limits<double>::quiet_NaN();
		m_eta_sh_ref = std::numeric_limits<double>::quiet_NaN();
		m_eta_rh_ref = std::numeric_limits<double>::quiet_NaN();
		m_mguessmult = std::numeric_limits<double>::quiet_NaN();

		m_q_b_des_sp = std::numeric_limits<double>::quiet_NaN();
		m_q_sh_des_sp = std::numeric_limits<double>::quiet_NaN();
		m_q_rh_des_sp = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ref = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_des = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ND = std::numeric_limits<double>::quiet_NaN();

		m_Psat_des = std::numeric_limits<double>::quiet_NaN();
		m_deltaT_fw_des = m_h_sh_in_ref = m_T_boil_pred = std::numeric_limits<double>::quiet_NaN();
		m_s_sh_out_ref = m_h_lp_isen_ref = m_h_rh_in_ref = m_h_sh_out_ref = m_rho_hp_out = m_dp_rh_up = m_P_rh_in = std::numeric_limits<double>::quiet_NaN();
		m_h_rh_out_ref = m_h_fw = std::numeric_limits<double>::quiet_NaN();
		m_q_pb_max = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_guess = m_f_rh = m_f_b = m_P_sh_out_min = m_P_rh_out_min = std::numeric_limits<double>::quiet_NaN();
		m_b_EB_count = -1;
		m_h_hp_out = std::numeric_limits<double>::quiet_NaN();
		m_h_boiler = std::numeric_limits<double>::quiet_NaN();

		// Stored	
		m_E_su_rec_prev = std::numeric_limits<double>::quiet_NaN();	
		m_t_su_rec_prev = std::numeric_limits<double>::quiet_NaN();
		m_t_sb_pb_prev = std::numeric_limits<double>::quiet_NaN();
		m_E_su_rec = std::numeric_limits<double>::quiet_NaN();
		m_t_su_rec = std::numeric_limits<double>::quiet_NaN();
		m_t_sb_pb = std::numeric_limits<double>::quiet_NaN();
		
		m_diff_m_dot_old_ncall = std::numeric_limits<double>::quiet_NaN();
		m_diff_m_dot_out_ncall = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_prev_ncall = std::numeric_limits<double>::quiet_NaN();
		m_dp_b_prev_ncall = std::numeric_limits<double>::quiet_NaN();
		m_dp_sh_prev_ncall = std::numeric_limits<double>::quiet_NaN();
		m_dp_rh_prev_ncall = std::numeric_limits<double>::quiet_NaN();
		f_timestep_prev_ncall = std::numeric_limits<double>::quiet_NaN();

		m_standby_control = -1;

		m_n_flux_x = 0;
		m_n_flux_y = 0;

	}

	virtual ~sam_dsg_controller_type265()  {}

	virtual int init()
	{
		/*
		util::matrix_t<double> test(3);
		test[0] = 0.0;
		test[1] = 1.0;
		test[2] = 2.0;
		*/

    // use_refprop = .false.				// Can't call refprop with current setup
		m_fossil_mode = (int)value( P_fossil_mode );			//[-] The fossil fill operation strategy mode
		m_q_pb_design = value( P_q_pb_design )*1.0E6;	//[W] Heat rate into powerblock at design
		m_q_aux_max = value( P_q_aux_max )*1.0E6;		//[W] Maximum heat rate of aux heater    
		m_lhv_eff = value( P_lhv_eff );					//[-] Aux Heater lower heating value efficiency
		
		m_h_tower = value( P_h_tower );					//[m] Tower height   
		int n_panels = (int)value(P_n_panels);				//[-] Number of vertical panels on receiver
		int flowtype = (int)value(P_flowtype);				//[-] Flow pattern

		m_n_flux_x = (int)value(P_n_flux_x);
		m_n_flux_y = (int)value(P_n_flux_y);


		/*
		m_q_inc_base.resize(n_panels, 1);
		m_q_inc_base.fill(0.0);
		m_q_inc.resize(n_panels, 1);
		m_q_inc.fill(0.0);
		m_q_inc_rh.resize(n_panels, 1);
		m_q_inc_rh.fill(0.0);
		m_q_inc_b.resize(n_panels, 1);
		m_q_inc_sh.resize(n_panels, 1);
		*/
		m_q_inc_base.resize(n_panels);
		m_q_inc_base.fill(0.0);
		m_q_inc.resize(n_panels);
		m_q_inc.fill(0.0);
		m_q_inc_rh.resize(n_panels);
		m_q_inc_rh.fill(0.0);
		m_q_inc_b.resize(n_panels);
		m_q_inc_sh.resize(n_panels);

		double d_rec = value(P_d_rec);						//[m] Diameter of receiver - used in external convection correlation 
		double per_rec = CSP::pi * d_rec;					//[m] Perimeter of receiver
		
		m_q_rec_des = value( P_q_rec_des )*1.0E6;		//[W] Design-point thermal power 
		m_f_rec_min = value( P_f_rec_min );				//[-] Minimum receiver absorbed power fraction
		m_q_rec_min = m_q_rec_des * m_f_rec_min;		//[W] Minimum receiver absorbed power
		
		m_rec_qf_delay = value( P_rec_qf_delay );		//[-] Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour
		m_rec_su_delay = value( P_rec_su_delay );		//[hr] Receiver start-up delay time
		m_f_pb_cutoff = value( P_f_pb_cutoff );			//[-] Cycle cut-off fraction
		m_q_pb_min = m_q_pb_design * m_f_pb_cutoff;		//[W] Cycle cut-off heat rate
		
		m_f_pb_sb = value( P_f_pb_sb );					//[-] Cycle minimum standby fraction
		m_q_sb_min = m_q_pb_design * m_f_pb_sb;			//[W] Cycle minimum standby heat rate

		m_t_standby_ini = value( P_t_standby_ini );		//[hr] Time that power block can reamin in standby
		m_x_b_target = value( P_x_b_target );			//[-] Set target outlet quality of boiler
		m_eta_rec_pump = value( P_eta_rec_pump );		//[-] Efficiency of feedwater pump(s)  

		m_P_hp_in_des = value( P_P_hp_in_des )*1.0E2;		//[kPa] Design High Pressure Turbine Inlet Pressure (convert from bar)
		m_P_hp_out_des = value( P_P_hp_out_des )*1.0E2;		//[kPa] Design High Pressure Turbine Outlet Pressure (convert from bar)

		if( m_P_hp_in_des > 18000.0 || m_P_hp_out_des > 18000.0 )
		{
			message( TCS_ERROR, "The design cycle pressure(s) are greater than the 180 bar limit" );
			return -1;
		}

		m_f_mdotrh_des = value( P_f_mdotrh_des );			//[-] Design reheat mass flow rate fraction

		if(m_f_mdotrh_des > 1.0)
		{
			message(TCS_ERROR, "The design reheat mass flow rate fraction, %lg, must be less than or equal to 1.0", m_f_mdotrh_des);
			return -1;
		}

		if(m_f_mdotrh_des < 0.5)
		{
			message(TCS_ERROR, "For this model, the design reheat mass flow rate fraction, %lg, must be greater than or equal to 0.5", m_f_mdotrh_des);
		}

		m_p_cycle_design = value( P_p_cycle_design )*1.E3;	//[kW] Design cycle power (convert from MW)
		m_eta_des = m_p_cycle_design * 1000.0 / m_q_pb_design;	//[-] Design cycle efficiency

		m_ct = value( P_ct );							//[-] Cooling Type
		m_T_amb_des = value( P_T_amb_des ) + 273.15;	//[K] Design ambient temperature for design power cycle values, convert from C
		m_dT_cw_ref = value( P_dT_cw_ref );				//[C/K] Reference condenser cooling water inlet/outlet T diff
		m_T_approach = value(P_T_approach );			//[C/K] Cooling tower approach
		m_T_ITD_des = value( P_T_ITD_des );				//[C/K] ITD at design for dry systems

		m_P_b_in_min = 0.5 * m_P_hp_in_des;			//[kPa] Corresponds to limit in Type 234
		m_P_hp_out_min = 0.5* m_P_hp_out_des;		//[kPa] Corresponds to limit in Type 234

		double hl_ffact = value( P_hl_ffact );	//[-] Heat Loss Fudge FACTor
		
		m_h_boiler = value( P_h_boiler );				//[m] Height of boiler    
		double d_t_boiler = value( P_d_t_boiler );		//[m] Diameter of boiler tubes
		double th_t_boiler = value( P_th_t_boiler );	//[m] Thickness of boiler tubes
		double emis_boiler = value( P_emis_boiler );	//[-] Emissivity of boiler tubes
		//double abs_boiler = value( P_abs_boiler );		//[-] Absorptivity of boiler tubes
		double mat_boiler = value( P_mat_boiler );		//[-] Material of boiler tubes
		
		// 8.6.15, twn: Fin calculations are not enabled in the boiler performance model
		//                so don't pass into the TCS type as inputs
		//               ****  'l_fin' MUST be = 0 *****
		double th_fin = 0.0;			//value( P_th_fin );				//[m] Thickness of fin 
		double l_fin = 0.0;				//value( P_l_fin );				//[m] Length of fin (distance between boiler tubes)
		double emis_fin = 0.0;			//value( P_emis_fin );			//[-] Emissivity of fin
		//double abs_fin = 0.0;			//value( P_abs_fin );			//[-] Absorptivity of fin
		double mat_fin = 0.0;			//value( P_mat_fin );			//[-] Numerical code for fin material (2:Stainless_AISI316, 28: T-91 Steel)
		// ***********************************************************
		
		//double A_cs_b = CSP::pi*0.25*pow(d_t_boiler,2);	//[m^2] Cross-sectional area of boiler tube

		if( !dsg_rec.Initialize_Receiver( n_panels, d_rec, per_rec, hl_ffact, flowtype, false, 0, 0.0 ))
		{
			message(TCS_ERROR,  "Receiver initialization failed" );
			return -1;
		}

		if( !boiler.Initialize_Boiler( dsg_rec, m_h_boiler, d_t_boiler, th_t_boiler, emis_boiler, mat_boiler, 0.0,
			th_fin, l_fin, emis_fin, mat_fin, false ) )
		{
			message( TCS_ERROR, "Boiler initialization failed" );
			return -1;
		}

		// Superheater
		m_T_sh_out_des = value( P_T_sh_out_des ) + 273.15;	//[K] Target outlet temperature of superheater
		m_h_sh = value( P_h_sh );					//[m] Height of superheater
		m_d_sh = value( P_d_sh );					//[m] Diameter of superheater tube
		m_th_sh = value( P_th_sh );					//[m] Thickness of superheater tube
		m_emis_sh = value( P_emis_sh );				//[-] Emissivity of superheater tube
		m_abs_sh = value( P_abs_sh );				//[-] Absorptivity of superheater tube
		m_mat_sh = value( P_mat_sh );				//[-] Material of superheater tube
		//double A_cs_sh = CSP::pi*0.25*pow(m_th_sh, 2);	//[m^2] Cross-sectional area of superheating tube
		double h_sh_max = 4658519.0;		//[J/kg] Corresponds to maximum possible temperature of lookup tables so steam code doesn't bug out
   
		if( !superheater.Initialize_Boiler( dsg_rec, m_h_sh, m_d_sh, m_th_sh, m_emis_sh, m_mat_sh, h_sh_max, 0.0, 0.0, 0.0, 0.0, false ) )
		{
			message( TCS_ERROR, "Superheater initialization failed" );
			return -1;
		}

		//Reheater
		m_T_rh_out_des = value( P_T_rh_out_des ) + 273.15;	//[K] Target outlet temperature of reheater
		m_h_rh = value( P_h_rh );					//[m] Height of reheater
		m_d_rh = value( P_d_rh );					//[m] Diameter of reheater tube
		m_th_rh = value( P_th_rh );					//[m] Thickness of reheater tube
		m_emis_rh = value( P_emis_rh );				//[-] Emissivity of reheater tube
		m_abs_rh = value( P_abs_rh );				//[-] Absorptivity of reheater tube
		m_mat_rh = value( P_mat_rh );				//[-] Material of reheater tube
		//double A_cs_rh = CSP::pi*0.25*pow(m_th_rh, 2);	//[m^2] Cross-sectional area of reheater tube
		double h_rh_max = 4658519.0;				//[J/kg]

		if( !reheater.Initialize_Boiler( dsg_rec, m_h_rh, m_d_rh, m_th_rh, m_emis_rh, m_mat_rh, h_rh_max, 0.0, 0.0, 0.0, 0.0, false ) )
		{
			message( TCS_ERROR, "Reheater initialization failed" );
			return -1;
		}


		m_h_total = m_h_boiler + m_h_sh + m_h_rh;		//[m] Combined height of receiver panels
		//double area_rec = m_h_total * per_rec;			//[m^2] Total surface area of receiver
		m_A_panel = m_h_total * per_rec / (double) n_panels;	//[m^2] Surface area of one panel - combined receiver height
     
		m_cycle_max_frac = value( P_cycle_max_frac );	//[-] Maximum cycle overdesign fraction
		m_A_sf = value( P_A_sf );						//[m^2]

		m_q_pb_max = m_cycle_max_frac * m_q_pb_design;	//[W] Maximum heat rate to power block.  Defocus if > 

		m_ffrac = value(P_ffrac, &m_numtou);			//[-] Array of fossil fractions, index is time-of-use period
		//m_tou_schedule = value(P_TOU_schedule, &m_l_tou_schedule);	//[-] Array of time-of-use periods, index is hour of year

		water_state wp;
		
		water_TP( m_T_sh_out_des, m_P_hp_in_des, &wp ); 
		double h_hp_in_des = wp.enth;	double s_hp_in_des = wp.entr; double rho_hp_in_des = wp.dens;	//Design high pressure turbine inlet enthalpy(kJ/kg), entropy(kJ/kg-K), and density (kg/m^3)
		
		water_PS( m_P_hp_out_des, s_hp_in_des, &wp );
		double h_hp_out_isen = wp.enth;		//[kJ/kg] Design reheat extraction enthalpy assuming isentropic expansion
		double h_hp_out_des = h_hp_in_des - (h_hp_in_des - h_hp_out_isen)*0.88;	//[kJ/kg] Design reheat inlet enthlapy (isentropic efficiency = 88%)

		water_PH( m_P_hp_out_des, h_hp_out_des, &wp );
		//double T_rh_in_des = wp.temp;		//[K] Design reheat inlet temperature

		water_TP( m_T_rh_out_des, m_P_hp_out_des, &wp );
		double h_rh_out_des = wp.enth; double s_rh_out_des = wp.entr; double rho_lp_in_des = wp.dens;	//Reheater outlet enthalpy(kJ/kg), entropy(kJ/kg-K), and density [kg/m^3]

		//Design Condenser Pressure [kPa]
		if(m_ct==1)
		{
			water_TQ( m_dT_cw_ref + 3.0 + m_T_approach + m_T_amb_des, 0.0, &wp );
		}
		else if(m_ct==2 || m_ct==3)
		{
			water_TQ( m_T_ITD_des + m_T_amb_des, 0.0, &wp );
		}
		m_Psat_des = wp.pres;

		water_PS( m_Psat_des, s_rh_out_des, &wp );
		double h_lp_out_isen = wp.enth;		//[kJ/kg] Design low pressure outlet enthalpy assuming isentropic expansion
		double h_lp_out_des = h_rh_out_des - (h_rh_out_des - h_lp_out_isen)*0.88;	//[kJ/kg] Design low pressure outlet enthalpy 

		water_PQ( m_P_hp_in_des, 1.0, &wp );
		double h_sh_in_des = wp.enth; double T_boil_des = wp.temp;	//[kJ/kg] Design SH inlet enthalpy; [C] Design SH inlet temperature

		//Calculate design mass flow rate based on design setpoints
		//Governing equation: P_cycle = m_dot*(h_hp_in_des - h_hp_out_des) + m_dot*m_f_mdotrh_des*(h_rh_out_des - h_lp_out_des)
		m_m_dot_des = m_p_cycle_design/( (h_hp_in_des - h_hp_out_des) + m_f_mdotrh_des*(h_rh_out_des - h_lp_out_des) );	//[kW/(kJ/kg)] = kg/s
		
		//Now find feedwater outlet enthlapy using design cycle efficiency
		double q_sh_des = (h_hp_in_des - h_sh_in_des)*m_m_dot_des;	//[kW] Design rate of energy input to superheater
		double q_rh_des = (h_rh_out_des - h_hp_out_des)*m_m_dot_des*m_f_mdotrh_des;	//[kW] Design rate of energy input to reheater

		double q_b_des = m_q_pb_design/1.E3 - q_sh_des - q_rh_des;	//[kW] Design rate of energy input to boiler

		//Governing equation: q_b_des = (h_sh_in_des - h_fw_out_des)*m_dot_des
		double h_fw_out_des = h_sh_in_des - q_b_des/m_m_dot_des;
		water_PH( m_P_hp_in_des, h_fw_out_des, &wp );
		double T_fw_out_des = wp.temp; double rho_fw_out_des = wp.dens;	//[K] Design feedwater outlet temp, [kg/m^3] Design feedwater outlet density

		//double m_dot_tube_b = (m_m_dot_des/m_x_b_target)/boiler.Get_n_flowpaths()/(per_rec/(double)dsg_rec.Get_n_panels_rec()/d_t_boiler);	//[kg/s]
		//double m_dot_tube_sh = (m_m_dot_des/superheater.Get_n_flowpaths())/(per_rec/(double)dsg_rec.Get_n_panels_rec()/m_d_sh);				//[kg/s]
		//double m_dot_tube_rh = (m_m_dot_des/reheater.Get_n_flowpaths())/(per_rec/(double)dsg_rec.Get_n_panels_rec()/m_d_rh);					//[kg/s]

		//Helpful metrics - not used elsewhere in code
		//double v_boiler_in = (m_dot_tube_b/m_x_b_target)/(rho_fw_out_des*A_cs_b);	//[m/s] Design fluid entrance velocity in boiler tube
		//double v_sh_out = m_dot_tube_sh / (rho_hp_in_des*A_cs_sh);					//[m/s] Design fluid exit velocity in superheater tube
		//double v_rh_out = m_dot_tube_rh / (rho_lp_in_des*A_cs_rh);					//[m/s] Design fluid exit velocity in reheater tube

		//Specific heat rate of receivers
		m_q_sh_des_sp = h_hp_in_des - h_sh_in_des;		//[kJ/kg]
		m_q_rh_des_sp = h_rh_out_des - h_hp_out_des;	//[kJ/kg]
		m_q_b_des_sp = h_sh_in_des - h_fw_out_des;		//[kJ/kg]

		m_deltaT_fw_des = T_boil_des - T_fw_out_des;		//[K] Design difference between boiling temperature and feedwater outlet temperature
		m_Psat_des *= 1.E3;		//[Pa] convert from kPa for calcs later in code

		m_high_pres_count = 0;
		m_eta_lin_approx = true;
		m_q_low_set = false;
		m_q_high_set = false;

		// Set Stored     
		m_E_su_rec_prev = m_q_rec_des * m_rec_qf_delay;	//[W-hr]  Set startup energy requirement for mode = 0	
		m_t_su_rec_prev = m_rec_su_delay;				//[hr] Set startup time requirement for mode = 0				
		m_t_sb_pb_prev = m_t_standby_ini;				//[hr] Powerblock standby time remaining					                      

		//**************************************************************************
		// Set up matrix for solar positions of flux map. 2 rows: azimuth, zenith -- a column for each solar position
		/*
		int angle_rows = 0, angle_cols = 0;
		double *p_angle = value( P_fluxmap_angles, &angle_rows, &angle_cols );
		fluxmap_angles.resize( angle_rows, angle_cols );
		if( p_angle != 0 && angle_rows == 2 && angle_cols > 3 )
		{
			for( int r = 0; r < angle_rows; r++ )
				for( int c = 0; c < angle_cols; c++ )
					fluxmap_angles.at(r,c) = TCS_MATRIX_INDEX( var( P_fluxmap_angles ), r, c );
		}
		else
		{
			message( "Flux map solar position input is incorrect P_fluxmap_angles: %d x %d", angle_rows, angle_cols );
			return -1;
		}
		m_num_sol_pos = angle_cols;
		*/
		// ****************************************************************************
		

		// ********************************************************************************
		// Set up matrix for solar flux. number of rows = num_sol_pos = number of solar positions, number of columns - 120 (10 height nodes x 12 circumferential nodes)
		/*
		int flux_rows = 0, flux_cols = 0;
		double *p_flux = value( P_fluxmap, &flux_rows, &flux_cols );
		if( flux_rows != m_num_sol_pos )
		{
			message( "Number of flux maps is not equal to number of solar positions" );
			return -1;
		}
		fluxmap.resize( flux_rows, flux_cols );
		if( p_flux != 0 && flux_cols == 12 )
		{
			for( int r = 0; r < flux_rows; r++ )
				for( int c = 0; c < flux_cols; c++ )
					fluxmap.at(r,c) = TCS_MATRIX_INDEX( var( P_fluxmap ), r, c );
		}
		else
		{
			message( "Flux map input is incorrect" );
			return -1;
		}
		*/
		// ********************************************************************************
		//allocate the input array for the flux map
		m_i_flux_map = allocate(I_flux_map, m_n_flux_y, m_n_flux_x);

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		if( (m_standby_control > 1) && (ncall > 0) )
		{
			return 0;	// What outputs does this give??
		}

		/*// If debugging just this type for a specific set of inputs (one timestep), set stored variables here
		// ****************************
		m_E_su_rec_prev = 0.0;
		m_t_su_rec_prev = 0.0;
		m_t_sb_pb_prev = 2.0;
		m_eta_lin_approx = false;

		m_eta_b_high = 0.92079;
		m_eta_b_low = 0.8474;
		m_q_total_high = 232006614;
		m_q_total_low = 114349354;
		m_eta_sh_high = 0.7970;
		m_eta_sh_low = 0.5992;
		m_eta_rh_high = 0.6816;
		m_eta_rh_low = 0.4433;
		// ****************************/

		// Time-dependent Calculations
		double P_b_in = value( I_P_b_in )*1.E2;		//[kPa] Boiler inlete pressure (convert from bar) -> HP turbine inlet + DP_SH + DP_B (combined in type 234)
		double f_mdotrh = value( I_f_mdotrh );		//[-] Reheat mass flow fraction
		double P_hp_out = value( I_P_hp_out )*1.E2; //[kPa] Reheat inlet pressure (convert from bar)
		double T_hp_out = value( I_T_hp_out )+273.15;		//[K] Reheater inlet temp
		double T_rh_target = value( I_T_rh_target )+273.15;	//[K] Reheater outlet temp
		double T_fw = value( I_T_fw )+273.15;		//[K] Feedwater outlet temp
		double P_cond = value( I_P_cond );			//[Pa] Condenser pressure
		m_touperiod = (int)value(I_TOUPeriod) - 1; // control value between 1 & 9, have to change to 0-8 for array index

		// Need to set this here because if using fossil fuel in low irradiance cases, it will skip part of code that sets T_rh_in
		double T_rh_in = T_hp_out;

		// May be using aux with receiver off, so need logic to skip receiver calcs 
		bool skip_rec_calcs = false;
		if( !m_success && ncall > 0 )
			skip_rec_calcs = true;
		// IF( (success==0) .and.(info(7)>0) ) GOTO 375    

		double m_dot_sh = std::numeric_limits<double>::quiet_NaN();
		double m_dot_rh = std::numeric_limits<double>::quiet_NaN();
		double dp_sh = std::numeric_limits<double>::quiet_NaN();
		double dp_rh = std::numeric_limits<double>::quiet_NaN();
		double q_therm_in_b = std::numeric_limits<double>::quiet_NaN();
		double q_therm_in_sh = std::numeric_limits<double>::quiet_NaN();
		double q_therm_in_rh = std::numeric_limits<double>::quiet_NaN();
		double q_therm_in_rec = std::numeric_limits<double>::quiet_NaN();
		double deltaP1 = std::numeric_limits<double>::quiet_NaN();
		double W_dot_fw = std::numeric_limits<double>::quiet_NaN();
		double W_dot_boost = std::numeric_limits<double>::quiet_NaN();
		double eta_rh = std::numeric_limits<double>::quiet_NaN();
		double eta_sh = std::numeric_limits<double>::quiet_NaN();
		double eta_b = std::numeric_limits<double>::quiet_NaN();
		double h_fw_Jkg = std::numeric_limits<double>::quiet_NaN();
		double P_sh_in = std::numeric_limits<double>::quiet_NaN();
		double P_hp_in = std::numeric_limits<double>::quiet_NaN();
		double P_lp_in = std::numeric_limits<double>::quiet_NaN();
		double q_boiler_abs = std::numeric_limits<double>::quiet_NaN();
		double q_sh_abs = std::numeric_limits<double>::quiet_NaN();
		double q_rh_abs = std::numeric_limits<double>::quiet_NaN();
		double rho_fw = std::numeric_limits<double>::quiet_NaN();
		double T_in = std::numeric_limits<double>::quiet_NaN();
		double P_b_out = std::numeric_limits<double>::quiet_NaN();
		double T_boil = std::numeric_limits<double>::quiet_NaN();
		double h_hp_in = std::numeric_limits<double>::quiet_NaN();
		double h_lp_in = std::numeric_limits<double>::quiet_NaN();
		double h_rh_in = std::numeric_limits<double>::quiet_NaN();
		double dp_b = 0.0;
		double P_sh_out = 0.0;
		double P_rh_out = 0.0;

		//get the flux map
		int n_flux_y, n_flux_x;
		m_i_flux_map = value(I_flux_map, &n_flux_y, &n_flux_x);

		if (n_flux_y > 1){
			message(TCS_WARNING, "The Direct Steam External Receiver (Type265) model does not currently support 2-dimensional "
				"flux maps. The flux profile in the vertical dimension will be averaged. NY=%d", n_flux_y);
		}
		m_flux_in.resize(n_flux_x);



		water_state wp;

		while( !skip_rec_calcs )	// 'while' loop around receiver calcs allows code to break out if certain flags are tripped. Only go through once, so reset 'skip_rec_calcs' upon entry
		{
			skip_rec_calcs = true;
			bool df_upflag = false;
			bool df_lowflag = false;			

			// These values only need to be calculated once every timestep
			if(ncall==0)
			{
				m_azimuth = value( I_azimuth );		//[deg] Solar azimuth angle
				m_zenith = value( I_zenith );		//[deg] Solar zenith angle
				m_I_bn = value( I_I_bn );			//[W/m^2] DNI
				m_T_amb = value( I_T_amb )+273.15;  //[K] Ambient Temperature
				double v_wind_10 = value( I_v_wind_10 ); //[m/s] Wind speed (at 10m from ground)
				m_P_atm = value( I_P_atm )*100.0;	//[Pa] Ambient pressure (convert from mbar)
				m_hour = time/3600.0;
				//m_hour = 12.0;
				m_T_dp = value( I_T_dp )+273.15;    //[K] Dew point temperature
				//m_touperiod = CSP::TOU_Reader( m_tou_schedule, time, m_l_tou_schedule );	//[-] Time of use period
				//m_touperiod = 1;
				m_field_eff = value( I_field_eff ); //[-] field efficiency

				// Calculate sky temp [K]
				m_T_sky = CSP::skytemp( m_T_amb, m_T_dp, m_hour );
				// Correct the windspeed to receiver height using the logarithmic profile law (HOMER documentation, Wind Sheer Inputs section)
				m_v_wind = log((m_h_tower+(m_h_total/2.0)/2.0)/0.003)/log(10.0/0.003)*v_wind_10;
			
				m_E_su_rec = m_E_su_rec_prev;		//[W-hr] Energy required for startup
				m_t_su_rec = m_t_su_rec_prev;		//[hr] Time required for startup
				m_t_sb_pb  = m_t_sb_pb_prev;		//[hr] Remaining standby operation time

				// Reset Values
				m_q_aux = 0.0;						//[W]
				m_m_dot_aux = 0.0;					//[kg/s]
			
				// Defocus: this needs to be before low flow checks!!
				m_defocus = 1.0;					//[-] Defocus control
				m_df_flag = false;					//[-] Defocus flag: true = defocus < 1

				// Calculate Flux
				//if (m_I_bn > 150.0)
				if (m_I_bn > 150.0)
				{
					for (int j = 0; j<n_flux_x; j++){
						m_flux_in.at(j) = 0.;
						for (int i = 0; i<n_flux_y; i++){
							m_flux_in.at(j) += m_i_flux_map[j*n_flux_y + i]
								* m_I_bn*m_field_eff*m_A_sf / 1000. / (m_h_total*dsg_rec.Get_per_rec() /(double)n_flux_x);	//[kW/m^2]	12 or n_flux_x?								
						}
					}
				}
				else
				{
					m_flux_in.fill(0.0);
					m_success = false;
					m_q_total = 0.0;
					m_msg = "type 265: fail at m_I_bn=" + util::to_string(m_I_bn) + " <= 1.0";
					break;
				}

				double n_flux_x_d = (double)m_n_flux_x;

				/*
				if( m_I_bn > 150.0 )
				{
					double hold = 1.e6; int p1;
					for( int i = 0; i < m_num_sol_pos; i++ )
					{
						double azi_i = fluxmap_angles.at(0,i);
						double zen_i = fluxmap_angles.at(1,i);
						double xdist = sqrt( pow( azi_i - m_azimuth, 2 ) + pow( zen_i - m_zenith, 2 ) );
						if( xdist <= hold )
						{
							hold = xdist;
							p1 = i;
						}					
					}
					for( int i = 0; i < 12; i++ )
						m_solarflux.at(i,0) = fluxmap.at( p1, i )*m_I_bn*m_field_eff*m_A_sf/1000./(m_h_total*dsg_rec.Get_per_rec()/12.0);	//[kW/m^2]									
				}
				else
				{
					m_success = false;
					m_q_total = 0.0;
					break;
					// GOTO 375
				}
				*/
				// Translate to the number of panels, so each panel has its own linearly interpolated flux value
				// Originally from Type 222
				/*
				double n_panels = (double) dsg_rec.Get_n_panels_rec();
				for( int i = 0; i < n_panels; i++ )
				{
					double ppos = (12.0/n_panels*i+6.0/n_panels);
					double flo = floor( ppos );
					double ceiling = ceil( ppos );
					double ind = (ppos - flo)/max((ceiling - flo),1.e-6);
					if( ceiling > 11 )	ceiling = 0;
					m_q_inc_base.at(i,0) = (ind*(m_solarflux.at(ceiling,0)-m_solarflux.at(flo,0))+m_solarflux.at(flo,0))*1000.0;	//[W/m^2] Average area-specific power for each node
				}
				
				double n_panels = (double)dsg_rec.Get_n_panels_rec();
				for (int i = 0; i < n_panels; i++)
				{
					double ppos = (n_flux_x_d / n_panels*i + 6.0 / n_panels);
					double flo = floor(ppos);
					double ceiling = ceil(ppos);
					double ind = (ppos - flo) / max((ceiling - flo), 1.e-6);
					if (ceiling > n_flux_x_d)	ceiling = 0;
					m_q_inc_base.at(i) = (ind*(m_flux_in.at(ceiling) - m_flux_in.at(flo)) + m_flux_in.at(flo))*1000.0;	//[W/m^2] Average area-specific power for each node
				}
				*/
				
				double n_panels = (double)dsg_rec.Get_n_panels_rec();
				
				if (n_panels >= m_n_flux_x)
				{
					// Translate to the number of panels, so each panel has its own linearly interpolated flux value
					for (int i = 0; i < n_panels; i++)
					{
						double ppos = (n_flux_x_d / (double)n_panels*i + n_flux_x_d*0.5 / (double)n_panels);
						int flo = (int)floor(ppos);
						int ceiling = (int)ceil(ppos);
						double ind = (int)((ppos - flo) / max((double)(ceiling - flo), 1.e-6));
						if (ceiling > m_n_flux_x - 1) ceiling = 0;

						double psp_field = (ind*(m_flux_in.at(ceiling) - m_flux_in.at(flo)) + m_flux_in.at(flo));		//[kW/m^2] Average area-specific power for each node
//						m_q_inc_base.at(i) = m_A_panel*psp_field;	//[kW] The power incident on each node   ?? correct area?
						m_q_inc_base.at(i) = psp_field * 1000; // [W/m^2]
					}

					
				}
				else
				{
					double back_mult = 1.0; double front_mult = 0.0;
					int index_start = -1; int index_stop = -1;
					//double q_flux_sum = 0.0;
					bool is_div = false;
					if (m_n_flux_x%(int)n_panels == 0)
						is_div = true;

					for (int i = 0; i < n_panels; i++)
					{
						front_mult = 1.0 - back_mult;
						index_start = index_stop;

						if (is_div)
							index_stop = (int)(m_n_flux_x / n_panels*(i + 1) - 1);
						else
							index_stop = (int)ceil(((double)(m_n_flux_x / n_panels)*(i + 1))) - 1;

						if (is_div)
							back_mult = 1.0;
						else
							back_mult = (double)(m_n_flux_x / n_panels)*(i + 1) - (int)((double)(m_n_flux_x / n_panels)*(i + 1));

						double sum_fracs = 0.0; double sum_flux = 0.0;
						for (int j = index_start; j <= index_stop; j++)
						{
							if (j == index_start)
							{
								sum_fracs += front_mult;
								if (j == -1)
									sum_flux += front_mult*m_flux_in.at(m_n_flux_x - 1);
								else
									sum_flux += front_mult*m_flux_in.at(j);
							}
							else if (j == index_stop)
							{
								sum_fracs += back_mult;
								if (j == 12)
									sum_flux += back_mult*m_flux_in.at(0);
								else
									sum_flux += back_mult*m_flux_in.at(j);
							}
							else
							{
								sum_fracs += 1.0;
								sum_flux += m_flux_in.at(j);
							}
						}
//						m_q_inc_base.at(i) = sum_flux*m_A_panel / sum_fracs;
						m_q_inc_base.at(i) = sum_flux*1000 / sum_fracs; // [W/m^2]
					}
				}

				

			/////////////


				double sum_q_inc = 0.0;
				for (int i = 0; i < n_panels; i++)
					sum_q_inc += m_q_inc_base.at(i);// , 0);
				m_q_total = sum_q_inc*m_A_panel;	//[W] Available 'incident' thermal power

				// If available thermal power is less than specified receiver minimum, then receiver is shut down, go to post-receiver calcs
				if(m_q_total < m_q_rec_min)
				{
					m_success = false;
					break;
					// GOTO 375
				}
				m_q_total_df = m_q_total;
				m_mguessmult = 1.0;

				// Predict efficiencies		
				if( m_eta_lin_approx )
				{
					m_eta_b_ref	= 0.86;	//[-] Guess boiler efficiency
					m_eta_sh_ref = 0.78; //[-] Guess superheater efficiency
					m_eta_rh_ref = 0.55; //[-] Guess reheater efficiency
				}
				else
				{
					m_eta_b_ref	= min( 0.99, (m_eta_b_high - m_eta_b_low)/(m_q_total_high - m_q_total_low)*(m_q_total - m_q_total_low) + m_eta_b_low );
					m_eta_sh_ref = min( 0.99, (m_eta_sh_high - m_eta_sh_low)/(m_q_total_high - m_q_total_low)*(m_q_total - m_q_total_low) + m_eta_sh_low );
					m_eta_rh_ref = min( 0.99, (m_eta_rh_high - m_eta_rh_low)/(m_q_total_high - m_q_total_low)*(m_q_total - m_q_total_low) + m_eta_rh_low );
				}

				// Predict mass flow rates using design conditions
				m_m_dot_ref = (m_q_total/1.E3)/(m_q_b_des_sp/m_eta_b_ref + m_q_sh_des_sp/m_eta_sh_ref + m_q_rh_des_sp*m_f_mdotrh_des/m_eta_rh_ref);	//[kg/s] Estimate mass flow rate using design conditions

				// Use max turbine fraction and a multiplier to limit possible values in over-design conditions
				m_m_dot_ND = min( m_cycle_max_frac, m_m_dot_ref/m_m_dot_des );	//[-] Nondimensional mass flow rate

				P_hp_out	= pow( (pow(m_Psat_des,2)+pow(m_m_dot_ND,2)*(pow(m_P_hp_out_des*1.E3,2)-pow(m_Psat_des,2))), 0.5 );
				P_b_in		= pow( (pow(P_hp_out,2)+pow(m_m_dot_ND,2)*(pow(m_P_hp_in_des*1.E3,2)-pow(m_P_hp_out_des*1.E3,2))), 0.5 );
				P_hp_out	/= 1.E3;
				P_b_in		/= 1.E3;

				//***************************************************************************************
				//*** If inputs from PB model were 0, so we need supply useful values to get started ****
				//***************************************************************************************
				// Ensure the boiler and reheat inlet pressures are within the bounds of the steam property code (and are not 0)
				P_b_in	= min(19.E3, max(m_P_b_in_min, P_b_in));
				P_hp_out	= min(19.E3, max(m_P_hp_out_min, P_hp_out));

				// If reheater target temperature is = 0
				if( T_rh_target == 273.15 )		T_rh_target = m_T_rh_out_des;	//[K]

				// If mass flow rate fraction is >1 or <0
				if( f_mdotrh == 0.0 )			f_mdotrh = m_f_mdotrh_des;		//[-]
				f_mdotrh = min(1.0, f_mdotrh);									//[-]

				// Always recalculate the feedwater temperature
				water_PQ(P_b_in, 1.0, &wp);
				m_h_sh_in_ref = wp.enth;					//[kJ/kg]
				m_T_boil_pred = wp.temp - 273.15;					//[C]
				T_fw = m_T_boil_pred - m_deltaT_fw_des + 273.15;	//[C]

				// Calculate reheat inlet temperature
				water_TP( m_T_sh_out_des, P_b_in, &wp );
				m_h_sh_out_ref = wp.enth; m_s_sh_out_ref = wp.entr;			//Predict high pressure turbine inlet enthalpy[kJ/kg] and entropy[kJ/kg-K]
				water_PS( P_hp_out, m_s_sh_out_ref, &wp );			//[kJ/kg] Predict isentropic outlet enthalpy at tower base
				m_h_lp_isen_ref = wp.enth;
				m_h_rh_in_ref = m_h_sh_out_ref - (m_h_sh_out_ref - m_h_lp_isen_ref)*0.88;	//[kJ/kg] Predict outlet enthalpy at tower base

				water_PH( P_hp_out, m_h_rh_in_ref, &wp );
				m_rho_hp_out = wp.dens;									//[kg/m^3] Predict density at tower base
				m_dp_rh_up = m_rho_hp_out * 9.81 * m_h_tower;			//[Pa] Pressure loss due to elevation rise
				m_P_rh_in = P_hp_out - m_dp_rh_up/1.E3;				//[kPa] Reheater inlet pressure at receiver

				water_PH( m_P_rh_in, m_h_rh_in_ref, &wp );				//[C] Predict reheat inlet temperature
				T_rh_in = wp.temp;								//[K] Convert from C

				water_TP( m_T_rh_out_des, m_P_rh_in, &wp );	
				m_h_rh_out_ref = wp.enth;									//[kJ/kg] Reheat outlet enthalpy
				water_TP( T_fw, P_b_in, &wp );				
				m_h_fw = wp.enth;											//[kJ/kg] Feedwater enthalpy

				//bool m_df_pred_ct = true;			//[-] Reset defocus prediction iteration counter

				double q_b_pred, q_sh_pred, q_rh_pred, q_rec_pred_tot; 		
				q_b_pred = std::numeric_limits<double>::quiet_NaN();
				q_sh_pred = std::numeric_limits<double>::quiet_NaN();
				q_rh_pred = std::numeric_limits<double>::quiet_NaN();
				q_rec_pred_tot = std::numeric_limits<double>::quiet_NaN();

				for( int i = 0; i < 2; i++ )
				{
					double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/m_eta_b_ref;			//[J/kg] Predict specific energy input to boiler
					double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/m_eta_sh_ref;	//[J/kg] Predict specific energy input to superheater
					double q_rh_pred_sp = (m_h_rh_out_ref - m_h_rh_in_ref)*1000.0/m_eta_rh_ref;	//[J/kg] Predict specific energy input to reheater

					m_m_dot_ref = m_q_total_df / (q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdotrh );	//[kg/s] Predict cycle mass flow rate 

					q_b_pred = q_b_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to boiler
					q_sh_pred = q_sh_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to superheater
					q_rh_pred = q_rh_pred_sp * m_m_dot_ref * f_mdotrh;	//[W] Predicted rate of energy transferred to reheater

					q_rec_pred_tot = q_b_pred + q_sh_pred + q_rh_pred;	//[W] Total predicted rate of energy transferred to receiver

					double q_therm_in_diff = (q_rec_pred_tot - m_q_pb_max)/m_q_pb_max;	//[-] Difference between predicted receiver thermal input and max pb thermal input

					bool break_loop = true;
					if( q_therm_in_diff > 0.1 && i == 0 )
					{
						m_defocus = min( 1.0, m_defocus*m_q_pb_max/q_rec_pred_tot );
						m_q_total_df = m_q_total * m_defocus;
						break_loop = false;
						if(!m_eta_lin_approx)
						{
							m_eta_b_ref = (m_eta_b_high-m_eta_b_low)/(m_q_total_high-m_q_total_low)*(m_q_total_df-m_q_total_low)+m_eta_b_low;
							m_eta_sh_ref = (m_eta_sh_high-m_eta_sh_low)/(m_q_total_high-m_q_total_low)*(m_q_total_df-m_q_total_low)+m_eta_sh_low;
							m_eta_rh_ref = (m_eta_rh_high-m_eta_rh_low)/(m_q_total_high-m_q_total_low)*(m_q_total_df-m_q_total_low)+m_eta_rh_low;
						}
						if(m_defocus<0.7)	m_mguessmult = 1.4;
					}
					else
					{
						break_loop = true;
					}
					if( break_loop )	break;
				}	
				m_m_dot_guess = m_m_dot_ref / m_x_b_target;				//[kg/s] Mass flow rate through the boiler is related to target outlet quality
				m_f_rh = q_rh_pred / (q_b_pred+q_sh_pred+q_rh_pred);	//[-] Predicted reheater fraction
				m_f_b = q_b_pred / (q_b_pred+q_sh_pred);				//[-] Predicted boiler fraction
			
				P_cond = m_Psat_des;					//[Pa] Use reference condenser pressure at first timestep
				// Mass flow rate dependent
				m_P_sh_out_min = max(1.E5, P_cond);		//[Pa] Specify minimum allowable outlet pressure of superheater
				m_P_rh_out_min = max(1.E5, P_cond);		//[Pa] Specify minimum allowable outlet pressure of reheater

				m_b_EB_count = 0;
			}	// end if(info(7) == 0) call

			if(ncall > 0)		//4/4/13, twn: haven't debugged this
			{
				P_b_in = min( 19.E3, P_b_in );		//[kPa]
				water_TP( m_T_sh_out_des, P_b_in, &wp );
				m_h_sh_out_ref = wp.enth;		//[kJ/kg] Predict superheater outlet enthalpy

				// Calculate temperature and pressure at reheater inlet
				water_TP( T_hp_out, P_hp_out, &wp );
				m_rho_hp_out = wp.dens; m_h_hp_out = wp.enth;	//[kg/m^3] density and [kJ/kg] enthalpy at HP outlet
				// By convention here, is m_h_hp_out = m_h_rh_in_ref ?
				m_dp_rh_up = m_rho_hp_out*9.81*m_h_tower;	//[Pa] Pressure loss due to elevation rise
				m_P_rh_in = P_hp_out - m_dp_rh_up/1000.0;	//[kPa] Reheater inlet pressure
				water_PH( m_P_rh_in, m_h_hp_out, &wp );
				T_rh_in = wp.temp;					//[K] Inlet temperature to reheater - convert from C
				// **************************************************************************************************

				m_h_rh_in_ref = m_h_hp_out;							//[kJ/kg] Predict reheater inlet enthlapy
				water_TP( T_rh_target, m_P_rh_in, &wp );	
				m_h_rh_out_ref = wp.enth;								//[kJ/kg] Predict reheat outlet enthalpy
			}

			int rh_count=0, sh_count=0, boiler_count = 0;
			int df_count = -1;		//[-] Defocus counter
			bool defocus_mode = false;

			eta_b = m_eta_b_ref;	eta_sh = m_eta_sh_ref; eta_rh = m_eta_rh_ref;	 // new for tcs -> initialize here - should overwrite before use
			int iter_T_rh = -1;				//[-] Number of iterations on reheater fraction
			//double diff_T_rh = std::numeric_limits<double>::quiet_NaN();				//[-] Difference between target and calculated reheat temperature		
			double df_upper, y_df_upper, df_lower, y_df_lower;
			df_upper = std::numeric_limits<double>::quiet_NaN();
			y_df_upper = std::numeric_limits<double>::quiet_NaN();
			df_lower = std::numeric_limits<double>::quiet_NaN();
			y_df_lower = std::numeric_limits<double>::quiet_NaN();
			//bool break_rec_calcs = false;
			bool break_def_calcs = false;

			do
			{
				defocus_mode = false;
				df_count++;							//[-] Increase defocus iteration counter
				for( int i = 0; i < dsg_rec.Get_n_panels_rec(); i++ )
					m_q_inc.at(i) = m_defocus*m_q_inc_base.at(i);		//[W/m^2] Defocused incident radiation on receiver
				m_q_total_df = m_defocus*m_q_total; //[W] Defocused total power on receiver
				
				// Need to recalculate mass flow rate and flux fraction guesses when defocus changes
				if( df_count > 0 && !m_df_flag )
				{
					//IF(df_count > 10) GOTO 1529 -> tcs note: this bypasses receiver calcs -> looks like previous results are kept, rather than the receiver turned off?
					if(df_count > 10) 
					{
						break_def_calcs = true;
						break;
					}
					double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
					double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
					double q_rh_pred_sp = (m_h_rh_out_ref - m_h_rh_in_ref)*1000.0/eta_rh;	//[J/kg] Predict specific energy input to reheater
					m_m_dot_ref = m_q_total_df/(q_b_pred_sp+q_sh_pred_sp+q_rh_pred_sp);		//[kg/s] Predict cycle mass flow rate
					m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s] Mass flow rate through boiler
					double q_b_pred = q_b_pred_sp * m_m_dot_ref;							//[W] Predicted rate of energy transferred to boiler
					double q_sh_pred = q_sh_pred_sp * m_m_dot_ref;							//[W] Predicted rate of energy transferred to superheater
					double q_rh_pred = q_rh_pred_sp * m_m_dot_ref;							//[W] Predicted rate of energy transferred to reheater
					m_f_rh = q_rh_pred/(q_b_pred+q_sh_pred+q_rh_pred);						//[-] Predicted reheater fraction
					m_f_b = q_b_pred/(q_b_pred+q_sh_pred);									//[-] Predicted boiler fraction
				}

				// Reset differences and iteration counter for next nested loop
				double diff_frh_b = 999.0;		//[-] Difference between upper and lower bounds of reheater fraction iteration
				double diff_T_rh = 999.0;		//[-] Difference between target and calculated reheat temperature
				iter_T_rh = 0;				//[-] Number of iterations on reheater fraction

				// Reset logic flags for reheater fraction iteration
				bool rh_low_guess = false;		//[-] Has a lower bound on the rh fraction been established
				bool rh_low_flag = false;		//[-] Has the model converged at the lower reheat fraction bound
				bool rh_up_guess = false;		//[-] Has an upper bound on the rh fraction been established
				bool rh_up_flag = false;		//[-] Has the model converged at the upper reheat fraction bound

				// Reset value of flags for reheater flux fraction iteration
				double f_rh_upper = 1.0;
				double f_rh_lower = 0.0;
				int rh_br_upper = 0;
				int rh_br_lower = 0;
				m_success = true;
				bool check_hxs = false;
				bool high_tol = true;			// True = use high tolerance to quickly get ballpark results. False = use tight tolerance for final answer
				
				int fb_stuck, rh_exit;
				fb_stuck = -1; rh_exit = -1;
				double y_rh_upper, y_rh_lower;
				y_rh_upper = y_rh_lower = std::numeric_limits<double>::quiet_NaN();
			
				//*********** Reheater Loop ***************
				//Iterate on reheater flux fraction WHILE:
				// - - - Difference between calculated and target reheater temperature is > tolerance
				// - - - or the Superheater model is still being solved with a larger tolerance (because reheater fraction bracket is not set)
				// - - - - - - - - AND - - - - - - - - 
				// - - - Number of reheater iterations is "small"  

				bool break_to_rh_iter = false;
				bool break_rec_calcs = false;
				// 93
				while( (fabs(diff_T_rh) > m_tol_T_rh || (high_tol) ) && (iter_T_rh < 20) )
				{
					iter_T_rh++;							//[-] Increase iteration counter
					break_to_rh_iter = false;				//[-] Flag to restart this loop
					diff_frh_b = f_rh_upper - f_rh_lower;	//[-] Calculate difference between brackets

					//***************************************************************************************************
					//***** Convergence logic to zero in on correct value of fraction of incident radiation to reheater 
					//************************************************************************************************** 
					if(iter_T_rh > 1)
					{
						if( fb_stuck == 0 && rh_exit == 0 )		// If reheater results are available, then we can try false position mode
						{					
							double f_rh_adjust = std::numeric_limits<double>::quiet_NaN();
							//Use relaxed tolerance until reheater solves once.  This will give updated an updated reheater efficiency and outlet enthalpy.
							//Use these updated values to predict reheat fraction and boiler fraction
							if(high_tol)
							{
								high_tol = false;
								double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
								double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
								double q_rh_pred_sp = (m_h_rh_out_ref - m_h_rh_in_ref)*1000.0/eta_rh;	//[J/kg] Predict specific energy input to reheater
								m_m_dot_ref = m_q_total_df/(q_b_pred_sp+q_sh_pred_sp+q_rh_pred_sp*f_mdotrh);	//[kg/s] Predict cycle mass flow rate
								m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s] Guess mass flow rate through boiler

								double q_b_pred = q_b_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to boiler
								double q_sh_pred = q_sh_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to superheater
								double q_rh_pred = q_rh_pred_sp * m_m_dot_ref * f_mdotrh;	//[W] Predicted rate of energy transferred to reheater
							
								f_rh_adjust = q_rh_pred/(q_b_pred+q_sh_pred+q_rh_pred);		//[-] Predicted reheater fraction
								m_f_b = q_b_pred / (q_b_pred + q_sh_pred);					//[-] Predicted boiler fraction
							}
							else if(rh_up_flag && rh_low_flag)
							{
								if(diff_T_rh < 0.0)
								{
									rh_br_upper = 3;
									f_rh_upper = m_f_rh;
									y_rh_upper = diff_T_rh;
								}
								else
								{
									rh_br_lower = 4;
									f_rh_lower = m_f_rh;
									y_rh_lower = diff_T_rh;
								}
								m_f_rh = (y_rh_upper)/(y_rh_upper-y_rh_lower)*(f_rh_lower-f_rh_upper) + f_rh_upper;	//[-] False position method
							}
							else
							{
								if(diff_T_rh < 0.0)			// Target is less than calculated, need to decrease flux on reheater/increase flux on superheater/boiler
								{
									rh_br_upper = 3;
									f_rh_upper = m_f_rh;		//[-] so set upper bound on reheater fraction
									y_rh_upper = diff_T_rh; //[K] Set upper result to use in false interpolation
									rh_up_flag = true;		//[-] set flag showing temperature difference has been found (rather than the modeling failing to solve)
								
									double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
									double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
									double q_rh_pred_sp = (m_h_rh_out_ref - m_h_rh_in_ref)*1000.0/eta_rh;	//[J/kg] Predict specific energy input to reheater
									m_m_dot_ref = m_q_total_df/(q_b_pred_sp+q_sh_pred_sp+q_rh_pred_sp*f_mdotrh);	//[kg/s] Predict cycle mass flow rate
									m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s] Guess mass flow rate through boiler
								
									double q_b_pred = q_b_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to boiler
									double q_sh_pred = q_sh_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to superheater
									double q_rh_pred = q_rh_pred_sp * m_m_dot_ref * f_mdotrh;	//[W] Predicted rate of energy transferred to reheater
								
									f_rh_adjust = q_rh_pred/(q_b_pred+q_sh_pred+q_rh_pred);		//[-] Predicted reheater fraction
									m_f_b = q_b_pred / (q_b_pred + q_sh_pred);					//[-] Predicted boiler fraction

										// Sometimes this predictive method gets "stuck", so need more brute-force approach
									if( m_f_rh - f_rh_adjust < 0.0005 )	f_rh_adjust = m_f_rh - 0.001;
								}
								else	// Target is greater than calculated, need to increase flux on reheater/ decrease flux on boiler/superheater
								{
									rh_br_lower = 4;
									f_rh_lower = m_f_rh;
									y_rh_lower = diff_T_rh;
									rh_low_flag = true;

									double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
									double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
									double q_rh_pred_sp = (m_h_rh_out_ref - m_h_rh_in_ref)*1000.0/eta_rh;	//[J/kg] Predict specific energy input to reheater
									m_m_dot_ref = m_q_total_df/(q_b_pred_sp+q_sh_pred_sp+q_rh_pred_sp*f_mdotrh);	//[kg/s] Predict cycle mass flow rate
									m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s] Guess mass flow rate through boiler
								
									double q_b_pred = q_b_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to boiler
									double q_sh_pred = q_sh_pred_sp * m_m_dot_ref;				//[W] Predicted rate of energy transferred to superheater
									double q_rh_pred = q_rh_pred_sp * m_m_dot_ref * f_mdotrh;	//[W] Predicted rate of energy transferred to reheater
								
									f_rh_adjust = q_rh_pred/(q_b_pred+q_sh_pred+q_rh_pred);		//[-] Predicted reheater fraction
									m_f_b = q_b_pred / (q_b_pred + q_sh_pred);					//[-] Predicted boiler fraction
								
										// Sometimes this predictive method gets "stuck", so need more brute-force approach
									if( f_rh_adjust - m_f_rh < 0.0005 )	f_rh_adjust = m_f_rh + 0.001;
								}

								if( rh_up_flag && rh_low_flag )		// If current call set the final result, use false position
									m_f_rh = (y_rh_upper)/(y_rh_upper-y_rh_lower)*(f_rh_lower-f_rh_upper) + f_rh_upper;		//[-] False position method
								else
								{
									m_f_rh = f_rh_adjust;
								}
							}
						}
						else if( fb_stuck == 1 || rh_exit == 1 || rh_exit == 2 )	
						{
							if(fb_stuck==1)		rh_br_lower = 1;
							if(rh_exit==1)		rh_br_lower = 2;
							if(rh_exit==2)		rh_br_lower = 3;
							rh_exit = 0;
							f_rh_lower = m_f_rh;	//so increase reheater flux
							rh_low_guess = true;
							rh_low_flag = false;
							if(rh_up_flag||rh_up_guess)		// If upper bound is known, use bisection
								m_f_rh	= 0.5*f_rh_lower + 0.5*f_rh_upper;
							else							// Otherwise, adjust with set multiplier 
								m_f_rh	= 1.25*m_f_rh;
						}
						else if( fb_stuck == 2 || rh_exit == 3 )
						{
							if( fb_stuck == 2 )
								rh_br_upper = 1;
							else if(rh_exit==3)
								rh_br_upper = 2;

							rh_exit = 0;
							f_rh_upper = m_f_rh;	// Decrease reheater flux
							rh_up_guess = true;
							rh_up_flag = false;
							if(rh_low_flag||rh_low_guess)
								m_f_rh = 0.5*f_rh_lower + 0.5*f_rh_upper;
							else
								m_f_rh = 0.75*m_f_rh;
						}
					}	// End convergence logic
				
					if( fabs(diff_frh_b)<0.0051 || iter_T_rh== 20 )
					{
						if( f_rh_upper < 0.01 && rh_br_lower == 0 )		rh_br_lower = 5;

						/*!Set lower limit on reheat fraction
						!rh_br_lower==1: Boiler fraction is stuck: boiler/superheat needs less available flux
						!rh_br_lower==2: High mass flow through RH led to pressure drop exit
						!rh_br_lower==3: Flux is too low for RH model to solve
						!rh_br_lower==4: RH Target temperature is higher than calculated temperature
						!rh_br_lower==5: No lower limit established
					
						!Set upper limit on reheat fraction
						!rh_br_upper==1: Boiler fractionis stuck: boiler/superheat needs more available flux
						!rh_br_upper==2: The RH model coudl not solve due to low mass flow rate
						!rh_br_upper==3: RH Target temperature is lower than calculated temperature
					
						!***** NEW 8/26/11********************
						!Pairing should not occur:
						!IF( ((rh_br_lower==1).and.(rh_br_upper==1)) .or. ((rh_br_lower==3).and.(rh_br_upper==3)) .or. &
						!    ((rh_br_lower==4).and.(rh_br_upper==3)) .or. ((rh_br_lower==5).and.(rh_br_upper==2,3)) */
						// Low flux: exit receiver modeling and determine if aux heat is used to power cycle
						if( (rh_br_lower==2 && rh_br_upper==1) ||
							(rh_br_lower==3 && rh_br_upper==1) || (rh_br_lower==3 && rh_br_upper==2) || (rh_br_lower==3 && rh_br_upper==3) ||
							(rh_br_lower==4 && rh_br_upper==1) || (rh_br_lower==4 && rh_br_upper==2) ||
							(rh_br_lower==5 && rh_br_upper==1) )
						{
							m_success = false;
							break_rec_calcs = true;
							break;
							// Exit !?!
						}

						// Defocus
						else if( (rh_br_lower==1 && rh_br_upper==2) || (rh_br_lower==1 && rh_br_upper==3) ||
								 (rh_br_lower==2 && rh_br_upper==2) || (rh_br_lower==2 && rh_br_upper==3) )
						{
							m_df_flag = true;	// Defocus is required
							m_defocus = max( 0.5*m_defocus, m_defocus - 0.1 );
							// Since defocus is required due to huge pressure drops, the system will not be operating efficiently.  Therefore,
							// defocus in large increments until receiver solves.  Don't attempt to then increase defocus to optimize.

							// Exit !?!?
							break_rec_calcs = true;
							break;
						}

						// Need to continue iterating, may have to slightly adjust limits due to relaxed sh exit tolerance at early f_rh iterations
						else if( rh_br_lower==4 && rh_br_upper==3 && fabs(diff_frh_b) < 0.0005 )
						{
							if(diff_T_rh < 0.0)
							{
								m_f_rh = m_f_rh - 0.01;
								rh_low_flag = false;
								f_rh_lower = m_f_rh;
							}
							else
							{
								m_f_rh = m_f_rh + 0.01;
								rh_up_flag = false;
								f_rh_upper = m_f_rh;
							}
						}
					}

					if(m_f_rh > 1.0)
					{
						f_rh_lower = 1.0;
						m_f_rh = 1.0;
					}

					rh_exit = 0;	// Reset reheater exit flag

					// ***************************************************************************************
					// ******** End of reheater convergence logic ********************************************
					// ***************************************************************************************
					double sum_q_inc_rh = 0.0;
					for( int i = 0; i < dsg_rec.Get_n_panels_rec(); i++ )
					{
						m_q_inc_rh.at(i) = m_f_rh * m_q_inc.at(i) * m_h_total / m_h_rh;	//[W/m^2] Incident radiation on reheater
						sum_q_inc_rh += m_q_inc_rh.at(i);
					}
					double q_inc_b_sh = m_q_total_df - sum_q_inc_rh*(m_h_rh/m_h_total)*m_A_panel;
					diff_T_rh = 999.9;		//[K]

					// Reset iteration variables for inner loop
					double diff_T_sh = 999.9;			//[K] Difference between target and calculated superheater outlet temperature
					double diff_f_bracket = 999.9;		//[K] Difference between upper and lower bounds for boiler fraction iteration
					int iter_T_sh = 0;

					// Set limits on boiler fraction
					double f_upper = 1.0;
					double f_lower = 0.0;

					// Reset logic flags for boiler fraction iteration
					bool upflag = false;			// Has an upper result (temp difference) been established?
					bool lowflag = false;			// Has a lower result (temp difference) been established?
					bool upguess = false;			// Has an upper bound (fraction) been established?
					bool lowguess = false;			// Has a lower bound (fraction) been established?
					int br_lower = 0; 
					int br_upper = 0;
					fb_stuck = 0;

					double tol_T_sh;
					if(high_tol)
						tol_T_sh = m_tol_T_sh_high;
					else
						tol_T_sh = m_tol_T_sh_base;

					int sh_exit = -1; 
					int boiler_exit = -1;
					double y_upper, y_lower, f_adjust;		
					y_upper = std::numeric_limits<double>::quiet_NaN();
					y_lower = std::numeric_limits<double>::quiet_NaN();
					f_adjust = std::numeric_limits<double>::quiet_NaN();
					bool checkflux;
					// *********************************************************************************************
					// ***** Loop to determine the fraction of remaing flux on boiler, given a set reheater fraction
					// *********************************************************************************************

					// 184 -> Fortran GOTO loop
					while( fabs(diff_T_sh)>tol_T_sh && iter_T_sh<20 )
					{
						iter_T_sh++;							//[-] Add to iteration counter
						diff_f_bracket = f_upper - f_lower;		//[-] Difference between upper and lower bracket guesses

						// ***********************************************************************************************************************************************
						// ***** Convergence logic to zero in on correct value of fraction of incident radiation to boiler given a total available to boiler and superheater
						// ***********************************************************************************************************************************************
						if( iter_T_sh > 1 )		// After first simulation of boiler and superheater, can iterate on boiler fraction
						{
							if( sh_exit==0 && boiler_exit==0 )		//[-] If boiler and superheaterd solved, then begin/continue false interpolation on boiler fraction
							{
								if(upflag && lowflag)			//[-] If results (superheater outlet temp) for both end of bracket, use false interpolation
								{
									if(diff_T_sh > 0.0)			//[-] Target is greater than calculated, need to decrease mass flow/increase flux on SH, so reduce flux on boiler, set upper limit
									{
										br_upper = 3;
										f_upper = m_f_b;
										y_upper = diff_T_sh;
									}
									else						//[-] Target is less than calculated, need to increase mass flow/decrease flux on SH, so increase flux on boiler, set lower limit
									{
										br_lower = 3;
										f_lower = m_f_b;
										y_lower = diff_T_sh;
									}
									m_f_b = (y_upper)/(y_upper-y_lower)*(f_lower-f_upper) + f_upper;	//[-] False interpolation method
								}
								else
								{
									if(diff_T_sh > 0.0)			//[-] Target is greater than calculated, need to decrease mass flow/increase flux on SH, so reduce flux on boiler, set upper limit
									{
										br_upper = 3;
										f_upper = m_f_b;
										y_upper = diff_T_sh;
										upflag = true;
										double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
										double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
										m_m_dot_ref = q_inc_b_sh/(q_b_pred_sp+q_sh_pred_sp);					//[kg/s] Predict mass flow rate
										m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s]
										f_adjust = q_b_pred_sp*m_m_dot_ref/q_inc_b_sh;							//[-] New boiler fraction
										//Know that boiler fraction needs to be decreased, so make sure that happens
										if( m_f_b - f_adjust < 0.0005 )		f_adjust = m_f_b - 0.001;
										//Also, if we know lower bound, then make sure we stay within 10/16 (?)
										if( f_adjust < f_lower )			f_adjust = 0.8*f_lower + 0.2*f_upper;
									}
									else
									{
										br_lower = 3;
										f_lower = m_f_b;
										y_lower = diff_T_sh;
										lowflag = true;
										double q_b_pred_sp = (m_h_sh_in_ref - m_h_fw)*1000.0/eta_b;				//[J/kg] Predict specific energy input to boiler
										double q_sh_pred_sp = (m_h_sh_out_ref - m_h_sh_in_ref)*1000.0/eta_sh;	//[J/kg] Predict specific energy input to superheater
										m_m_dot_ref = q_inc_b_sh/(q_b_pred_sp+q_sh_pred_sp);					//[kg/s] Predict mass flow rate
										m_m_dot_guess = m_m_dot_ref/m_x_b_target;								//[kg/s]
										f_adjust = q_b_pred_sp*m_m_dot_ref/q_inc_b_sh;							//[-] New boiler fraction
										// Know that boiler fraction needs to be increased, so make sure that happens
										if( f_adjust - m_f_b < 0.0005 )		f_adjust = m_f_b + 0.001;
										// Also, if we know upper bound, then make sure we stay within 10/16 (!?)
										if( f_adjust > f_upper )			f_adjust = 0.8*f_upper + 0.2*f_lower;
									}
									if( upflag && lowflag )
										m_f_b = y_upper/(y_upper-y_lower)*(f_lower-f_upper)+f_upper;	//[-] False position method
									else
										m_f_b = f_adjust;
								}
							}
							else if( boiler_exit==2 || sh_exit==3 )		//Boiler model did not solve: requires higher flux to solve OR superheater did not solve, needs less flux/mass flow
							{
								if(boiler_exit==2)
									br_lower = 1;
								else
									br_lower = 2;
								f_lower = m_f_b;
								lowguess = true;
								if(upflag||upguess)
									m_f_b = 0.5*f_upper + 0.5*f_lower;
								else
									m_f_b = m_f_b + 0.05;
							}
							else if( sh_exit==1 || sh_exit==2 || boiler_exit==3 )	//Superheater model did not solve or boiler could not due to high flux: requires that boiler has less flux (sh_exit = 1 or 2)
							{
								if(sh_exit==1)		br_upper = 1;
								if(sh_exit==2)		br_upper = 2;
								if(boiler_exit==3)	br_upper = 5;

								f_upper = m_f_b;
								upguess = true;
								if( lowguess || lowflag )
									m_f_b = 0.5*f_upper + 0.5*f_lower;
								else
									m_f_b = m_f_b - 0.05;		// Decrease boiler fraction: if flag is tripped, then logically upflag should be false
							}
						}

						if( m_f_b > 1.0 )	m_f_b = 1.0;

						boiler_exit = sh_exit = 0;

						// End of boiler convergence
						if( fabs(diff_f_bracket)<m_bracket_tol && iter_T_sh<20 )
						{
							if( f_lower>0.99 && br_upper==0 )	br_upper = 4;

							/******* NEW **************************************
							!**** Set lower limit on boiler fraction *********
							!br_lower==1: Boiler exit = 2 -> Boiler cannot solve due to low flux
							!br_lower==2: SH exit = 3     -> Superheater requires an increase in mass flow to solve
							!br_lower==3: The calculated SH outlet temperature is higher than the target temperature
						
							!**** Set upper limit on boiler fraction **********
							!br_upper==1: SH exit = 1     -> Superheater requires a decrease in mass flow to solve
							!br_upper==2: SH exit = 2     -> Superheater cannot solve due to low flux
							!br_upper==3: The calculated SH outlet temperature is lower than the target temperature
							!br_upper==4: No upper limit has been set
							!br_upper==5: Boiler exit = 3 -> Boiler cannot solve due to high flux */

							if( (br_lower==1 && br_upper==2) || (br_lower==1 && br_upper==3) || (br_lower==1 && br_upper==4) ||
								(br_lower==2 && br_upper==2) || (br_lower==2 && br_upper==3) || (br_lower==2 && br_upper==4) )	
							{
								fb_stuck = 2;
								break_to_rh_iter = true;
								break;
								// GOTO 93
								// fix this
							}
							else if( (br_lower==2 && br_upper==1) || (br_lower==2 && br_upper==5) ||
									 (br_lower==3 && br_upper==1) || (br_lower==3 && br_upper==5) )
							{
								fb_stuck = 1;
								break_to_rh_iter = true;
								break;
								// GOTO 93
								// fix this
							}
							else if( (br_lower==1 && br_upper==1) || (br_lower==1 && br_upper==5) ||
									 (br_lower==3 && br_upper==2) || (br_lower==3 && br_upper==4) )
							{
								// continue
							}
							else if( br_lower==3 && br_upper==3 )
							{
								break;
								// GOTO 1075
							}						
						}
					
						for( int i = 0; i < dsg_rec.Get_n_panels_rec(); i++ )
						{
							m_q_inc_b.at(i) = m_f_b*(1.0-m_f_rh)*m_q_inc.at(i)*m_h_total/m_h_boiler;
							m_q_inc_sh.at(i) = (1.0 - m_f_b)*(1.0 - m_f_rh)*m_q_inc.at(i)*m_h_total/m_h_sh;
						}
					
						double m_dot_lower = 0.75*m_m_dot_guess/m_mguessmult;
						double m_dot_upper = 1.35*m_m_dot_guess*m_mguessmult;
					
						if(check_hxs)	// increased flux is required to run boiler, so want to make sure any flux distribution minimally works for each HX
						{
							checkflux = true;
							boiler_exit = 0;
							// First, check boiler
							double dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9;
							boiler.Solve_Boiler( m_T_amb, m_T_sky, m_v_wind, m_P_atm, T_fw, P_b_in, m_x_b_target, m_m_dot_guess, m_dot_lower, 
												m_dot_upper, checkflux, m_q_inc_b, boiler_exit, dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9 );
							if( boiler_exit == 2 )
							{
								continue;
								// GOTO 184
							}

							// If code reaches this point, check superheater
							sh_exit = 0;
							water_TQ( m_T_boil_pred, 0.0, &wp );
							double h_sh_in_dummy = wp.enth;
							double P_sh_in_dummy = wp.pres;

							superheater.Solve_Superheater( m_T_amb, m_T_sky, m_v_wind, m_P_atm, P_sh_in_dummy, 1.0, h_sh_in_dummy, 1.0, checkflux, m_q_inc_sh,
															sh_exit, 1.0, dum1, dum2, dum3, dum4, dum5 );

							if( sh_exit ==2 )
							{
								continue;
								// GOTO 184
							}

							rh_exit = 0;
							water_TP( T_hp_out, m_P_rh_in, &wp );
							double h_rh_in_dummy = wp.enth;

							reheater.Solve_Superheater( m_T_amb, m_T_sky, m_v_wind, m_P_atm, m_P_rh_in, 1.0, h_rh_in_dummy, 1.0, checkflux, m_q_inc_rh,
															rh_exit, 1.0, dum1, dum2, dum3, dum4, dum5 );
						
							if( rh_exit == 2 )
							{
								break_to_rh_iter = true;
								break;
								// GOTO 93
							}
						}	// End if 'checkflux'

						checkflux = false;
						boiler_exit = 0;
						boiler_count++;

						boiler.Solve_Boiler( m_T_amb, m_T_sky, m_v_wind, m_P_atm, T_fw, P_b_in, m_x_b_target, m_m_dot_guess, m_dot_lower, m_dot_upper, checkflux, m_q_inc_b, boiler_exit,
											eta_b, T_boil, m_dot_sh, m_h_fw, P_b_out, m_h_sh_in_ref, rho_fw, q_boiler_abs, T_in);

						if( boiler_exit > 0 )
						{
							if( boiler_exit == 1 )
							{
								m_success = false;
								break_rec_calcs = true;
								break_def_calcs = true;
								break;
								// GOTO 375
							}
							if( boiler_exit == 2 )
							{
								check_hxs = true;
								continue;
								// GOTO 184
							}
							if( boiler_exit == 3 )
							{
								continue;
								// GOTO 184
							}
						}

						m_m_dot_guess = m_dot_sh / m_x_b_target;		//[kg/s]
						dp_b = (P_b_in - P_b_out)*1.E3;		//[Pa] Pressure drop through boiler

						sh_count++;

						double T_sh_in = T_boil;		//[K] Inlet temperature to superheater is boiling temperature from boiler
						water_TQ( T_sh_in, 1.0, &wp );
						P_sh_in = wp.pres;			//[kPa] Inlet pressure to superheater
						double h_sh_in = wp.enth;
					
						double rho_sh_out, h_sh_out;
						superheater.Solve_Superheater( m_T_amb, m_T_sky, m_v_wind, m_P_atm, P_sh_in, m_dot_sh, h_sh_in, m_P_sh_out_min, checkflux, m_q_inc_sh, sh_exit, m_T_sh_out_des,
														P_sh_out, eta_sh, rho_sh_out, h_sh_out, q_sh_abs);

						if( sh_exit > 0 )
						{
							continue;
							// GOTO 184		! If SH did not solve, don't need following calcs
						}

						water_TP( m_T_sh_out_des, P_sh_out, &wp );
						m_h_sh_out_ref = wp.enth;		//[kJ/kg] Update reference outlet enthalpy used in setting flux and mass flow guess rates - this is why target and not actual SH outlet temp is used

						double dp_sh_down = rho_sh_out*CSP::grav*m_h_tower;		//[Pa] Pressure due to tower elevations

						P_hp_in = P_sh_out + dp_sh_down/1.E3;			//[kPa] Pressure at HP inlet turbine (bottom of tower)
						water_PH( P_hp_in, h_sh_out, &wp );
						double T_sh_out = wp.temp;			//[K] Outlet temperature at bottom of tower assuming adiabatic piping
						h_hp_in = h_sh_out;							//[kJ/kg]

						diff_T_sh = (m_T_sh_out_des - T_sh_out) / m_T_sh_out_des;	//[K]

					}	// End iteration on boiler mass flow rate and superheater outlet temperature

					if( iter_T_sh==20 && fabs(diff_T_sh)>tol_T_sh )
					{
						m_success = false;
						// message: "The receiver model did not converge at this timestep (SH)"
						break_rec_calcs = true;
						break_def_calcs = true;
						// GOTO 375
					}

					if( break_rec_calcs )
						break;

					if( break_to_rh_iter )
						continue;
					
					// 1075 continue

					m_dot_rh = f_mdotrh * m_dot_sh;	//[kg/s] Calculate mass flow rate through reheater based on specified fraction

					rh_exit = 0;
					rh_count++;
					double rho_rh_out, h_rh_out;
					rho_rh_out = h_rh_out = std::numeric_limits<double>::quiet_NaN();
					

					water_TP( T_rh_in, m_P_rh_in, &wp );
					h_rh_in = wp.enth;

					reheater.Solve_Superheater( m_T_amb, m_T_sky, m_v_wind, m_P_atm, m_P_rh_in, m_dot_rh, h_rh_in, m_P_rh_out_min, checkflux, m_q_inc_rh, rh_exit, m_T_rh_out_des, 
												P_rh_out, eta_rh, rho_rh_out, h_rh_out, q_rh_abs );

					if( rh_exit > 0 )
					{
						continue;
						// GOTO 93
					}

					water_TP( m_T_rh_out_des, P_rh_out, &wp );
					m_h_rh_out_ref = wp.enth;				//[kJ/kg]

					double dp_rh_down = rho_rh_out*CSP::grav*m_h_tower;		//[Pa] Pressure due to tower elevation

					P_lp_in = P_rh_out + dp_rh_down/1.e3;			//[kPa] Pressure at LP turbine outlet (bottom of tower)
					water_PH( P_lp_in, h_rh_out, &wp );
					double T_rh_out = wp.temp;			//[K] Outlet temperature at bottom of tower assuming adiabatic piping
					h_lp_in = h_rh_out;						//[kJ/kg]

					diff_T_rh = (m_T_rh_out_des - T_rh_out)/m_T_rh_out_des;	//[K]

					// Logic that allows code to solve on first iteration if SH and RH are both winthin final tolerance
					if( high_tol && fabs(diff_T_sh)<m_tol_T_sh_base && fabs(diff_T_rh)>m_tol_T_rh )
						high_tol = false;

				}	// end iteration on flux and outlet temperature of reheater

				if( break_def_calcs )
					break;

				if( iter_T_rh==20 && fabs(diff_T_rh)>m_tol_T_rh )
				{
					m_success = false;
					// Message: "The receiver model did not converge at this timestep (RH)"
				}

				m_m_dot_ND = m_dot_sh/m_m_dot_des;							//[-]

				if( (m_m_dot_ND - m_cycle_max_frac)/m_cycle_max_frac > 0.005 || ( (m_m_dot_ND-m_cycle_max_frac)/m_cycle_max_frac < -0.005 && m_defocus<1.0 && !m_df_flag ) )
				{
					if( df_upflag && df_lowflag )
					{
						if( (m_m_dot_ND - m_cycle_max_frac) > 0.0 )
						{
							df_upper = m_defocus;
							y_df_upper = m_m_dot_ND - m_cycle_max_frac;
						}
						else
						{
							df_lower = m_defocus;
							y_df_lower = m_m_dot_ND - m_cycle_max_frac;
						}
						m_defocus = y_df_upper/(y_df_upper - y_df_lower)*(df_lower - df_upper) + df_upper;
					}
					else
					{
						if( (m_m_dot_ND - m_cycle_max_frac)>0.0 )
						{
							df_upflag = true;
							df_upper = m_defocus;
							y_df_upper = m_m_dot_ND - m_cycle_max_frac;
						}
						else
						{
							df_lowflag = true;
							df_lower = m_defocus;
							y_df_lower = m_m_dot_ND - m_cycle_max_frac;
						}
						if( df_upflag && df_lowflag )
							m_defocus = y_df_upper/(y_df_upper-y_df_lower)*(df_lower-df_upper) + df_upper;
						else
							m_defocus = min( 1.0, m_defocus*(m_cycle_max_frac/m_m_dot_ND) );
					}
					defocus_mode = true;
				}
			} while( defocus_mode );		// End iteration on field defocus
			if( break_def_calcs )
				break;				// Don't have to include this, but would be useful if more code is added between here and next };
		};

		// 375 -> TRNSYS GOTO number	
		h_fw_Jkg = m_h_fw * 1000.0;				//[J/kg] Feedwater enthalpy
		if( !m_success )
		{
			m_dot_sh = 0.0;
			m_dot_rh = 0.0;
			m_E_su_rec = m_q_rec_des * m_rec_qf_delay;
			m_t_su_rec = m_rec_su_delay;
			dp_sh = 0.0;
			dp_rh = 0.0;
			q_therm_in_b = 0.0;
			q_therm_in_sh = 0.0;
			q_therm_in_rh = 0.0;
			q_therm_in_rec = 0.0;
			P_cond = 0.0;
			deltaP1 = 10.E6;		// Set high so that difference in pressure trends towards 0
			W_dot_fw = 0.0;
			W_dot_boost = 0.0;
		}
		else
		{
			if( m_eta_lin_approx && m_defocus>0.999 )
			{
				if( m_q_total < (0.75*m_q_rec_min + 0.25*m_q_pb_design)  &&  !m_q_low_set )
				{
					m_q_total_low = m_q_total;
					m_eta_rh_low = eta_rh;
					m_eta_sh_low = eta_sh;
					m_eta_b_low = eta_b;
					m_q_low_set = true;
				}
				if( m_q_total > 0.85*m_q_pb_design && !m_q_high_set )
				{
					m_q_total_high = m_q_total;
					m_eta_rh_high = eta_rh;
					m_eta_sh_high = eta_sh;
					m_eta_b_high = eta_b;
					m_q_high_set = true;
				}
				if( m_q_low_set && m_q_high_set )
					m_eta_lin_approx = false;					
			}
			
			dp_sh = (P_sh_in - P_hp_in)*1000.0;		//[Pa] Pressure drop through superheater
			dp_rh = (P_hp_out - P_lp_in)*1000.0;	//[Pa] Pressure drop through reheater
			q_therm_in_b = q_boiler_abs;			//[W] Rate of energy transferred to boiler
			q_therm_in_sh = q_sh_abs;				//[W] Rate of energy transferred to superheater
			q_therm_in_rh = q_rh_abs;				//[W] Rate of energy transferred to reheater
			q_therm_in_rec = q_therm_in_b + q_therm_in_sh + q_therm_in_rh;	//[W] Rate of energy transferred to total receiver
			deltaP1 = rho_fw*CSP::grav*m_h_tower;	//[Pa] Pressure drop due to pumping feedwater up tower
			W_dot_fw = (deltaP1 + max(0.0,dp_sh))*m_dot_sh/rho_fw;		//[W] Power required to pump feedwater up tower AND increase pressure from HP turbine inlet to steam drum pressure

			water_TQ( T_boil, 0, &wp );
			double rho_x0 = wp.dens;
			double W_dot_sd = max(0.0, dp_b)*(m_dot_sh/m_x_b_target)/rho_x0;		//[W] Power required to pump boiler flow from steam drum pressure to boiler inlet pressure

			W_dot_boost = (W_dot_fw + W_dot_sd)/m_eta_rec_pump;						//[W] Total pumping power required (that is not already accounted for in regression model)					
		}

		//*******************************
		// Auxiliary Heating Contribution
		//*******************************
		// Is aux heating available, and if so, which mode?
		// 1529    TRNSYS GOTO Number
		m_m_dot_aux = 0.0;			// **** Why are these member data?
		m_q_aux = 0.0;
		double q_aux_fuel = 0.0;
		double f_timestep = 1.0;

		// Mass flow rate controls
		if( m_ffrac[m_touperiod] > 0.01 )
		{
			//If receiver solves, then define fossil cycle from receiver model
			if( m_success )
			{
				h_hp_in = h_hp_in*1000.0;
				h_lp_in = h_lp_in*1000.0;
				water_TP( T_rh_in, m_P_rh_in, &wp );
				h_rh_in = wp.enth*1000.0;
			}
			else	// If receiver does not solve, use type inputs to define a fossil cycle
			{
				water_TP( m_T_sh_out_des, P_b_in, &wp );
				h_hp_in = wp.enth*1000.0;
				water_TP( T_fw, m_P_b_in_min, &wp );
				h_fw_Jkg = wp.enth*1000.0; rho_fw = wp.dens;
				water_TP( m_T_rh_out_des, P_hp_out, &wp );
				h_lp_in = wp.enth*1000.0;
				water_TP( T_rh_in, P_hp_out, &wp );
				h_rh_in = wp.enth*1000.0;
			}

			if( m_fossil_mode == 1 )
			{
				// If fossil fraction is less than minimum turbine fraction (poor specification in fossil mode 1, but it could happen...)
				// or
				// If the heat rate corresponding to the fossil fraction is less than the heat rate absorbed by the receiver
				if( m_ffrac[m_touperiod]<m_f_pb_cutoff || m_ffrac[m_touperiod]*m_m_dot_des < m_dot_sh )
					m_m_dot_aux = 0.0;			// [kg/s]
				else
					m_m_dot_aux = m_ffrac[m_touperiod]*m_m_dot_des - m_dot_sh;
			}
			else if( m_fossil_mode == 2 )
			{
				m_m_dot_aux = m_ffrac[m_touperiod]*m_m_dot_des;
				if( (m_m_dot_aux + m_dot_sh)<(m_f_pb_cutoff*m_m_dot_des) )
					m_m_dot_aux = 0.0;
				else if( ((m_m_dot_aux + m_dot_sh)/m_m_dot_des - m_cycle_max_frac)/m_cycle_max_frac > 0.005 )
					m_m_dot_aux = max(0.0, m_cycle_max_frac*m_m_dot_des - m_dot_sh);
			}

			m_q_aux = m_m_dot_aux*((h_hp_in - h_fw_Jkg) + f_mdotrh*(h_lp_in - h_rh_in));	//[W]

			q_aux_fuel = m_q_aux / m_lhv_eff * step * (1.0/3600.0) * 3.41214116E-6;			//[MMBTU] Fuel energy usage (convert W-hr to MMBTU)
			double W_dot_aux = max(0.0, dp_sh)*m_m_dot_aux/rho_fw;							//[W] Power required to increase parameters from HP turbine inlet to steam drum pressure
			W_dot_boost = W_dot_boost + W_dot_aux;											//[W] Final pumping power must include aus pump power			
		}
		
		//double q_aux_rec = m_q_aux + q_therm_in_rec;		//[W] Total (auxiliary + incident absorbed) energy to power block
		double m_dot_toPB = m_m_dot_aux + m_dot_sh;			//[kg/s] Total mass flow through HP turbine is sum of aux and sh mass flow because they operate in parallel
		//double E_aux = m_q_aux * step / 3600.;				//[W-hr] Energy added by aux heater in current timestep

		//double t_standby;
		// Mass flow rate based controls
		if( m_dot_toPB > m_f_pb_cutoff*m_m_dot_des )
		{
			m_standby_control = 1;			//[-] Power cycle is in normal operation
			m_t_sb_pb = m_t_standby_ini;	//[hr] If cycle is in normal operation, then full standby time is available
		}
		else if( m_dot_toPB > m_f_pb_sb*m_m_dot_des && m_t_sb_pb_prev - step/3600.0 > 0.0 )
		{
			m_standby_control = 2;			//[-] Power cycle is in standby operation
			m_t_sb_pb = m_t_sb_pb_prev - step/3600.0;	//[hr] Subtract timestep from available standby time
		}
		else
		{
			m_standby_control = 3;			//[-] Power cycle is off
			m_t_sb_pb = 0.0;				//[hr] No standby time remaining
		}


		double q_startup;
		// Has receiver completed startup?
		if( (m_E_su_rec_prev > 0.0 || m_t_su_rec_prev > 0.0) && m_success )
		{
			q_startup = min( m_E_su_rec_prev, q_therm_in_rec*step/3600.0 );			//[W-hr] Startup energy during current timestep
			m_E_su_rec = max( 0.0, m_E_su_rec_prev - q_therm_in_rec*step/3600.0 );	//[W-hr] Subtract energy added in current timestep to find remaining required rec. startup energy
			m_t_su_rec = max( 0.0, m_t_su_rec_prev - step/3600.0 );					//[hr] Calculate remaining rec. startup time
			
			if( m_E_su_rec + m_t_su_rec > 0.0 )
				f_timestep = 0.0;			//[-] Receiver has not completed start-up at end of timestep
			else
				f_timestep = max( 0.0, min( 1.0 - m_t_su_rec_prev/(step/3600.0), 1.0 - m_E_su_rec_prev/(q_therm_in_rec)*step/3600.0 ) );	//[-] Receiver has completed startup during timestep

			if( m_m_dot_aux > 0.0 )
			{
				f_timestep = m_dot_sh/m_dot_toPB*f_timestep + m_m_dot_aux/m_dot_toPB;	//[-] Adjust f_timestep to allow aux mass flow rate to be used for power if receiver is starting up
				f_timestep = min( 1.0, f_timestep );
			}
		}

		// If mass flow rate and pressure drops do not change within tolerance (0.005) then we can assume that
		// return conditions from the power block will be the same, so end loop (rather than rely on TRNSYS internal tolerance)
		// Specify relative tolerance for pressure drops based on turbine pressures, since that is the useful value in the power block model
		m_diff_m_dot_old_ncall = m_diff_m_dot_out_ncall;
		m_diff_m_dot_out_ncall = m_dot_toPB - m_m_dot_prev_ncall;

		double diff_dp_b = (dp_b - m_dp_b_prev_ncall)/(P_b_in/1.E3);
		double diff_dp_sh = (dp_sh - m_dp_sh_prev_ncall)/(P_b_in/1.E3);
		double diff_dp_rh = (dp_rh - m_dp_rh_prev_ncall)/(m_P_rh_in/1.E3);

		if( ncall > 2 )
		{
			if( m_diff_m_dot_old_ncall > m_diff_m_dot_out_ncall )
			{
				if( fabs(m_diff_m_dot_old_ncall + m_diff_m_dot_out_ncall) < 0.01 )
				{
					m_dot_toPB = 0.5*m_m_dot_prev_ncall + 0.5*m_dot_toPB;
					m_diff_m_dot_out_ncall = m_dot_toPB - m_m_dot_prev_ncall;
				}
			}
			else if( m_diff_m_dot_out_ncall > m_diff_m_dot_old_ncall )
			{
				if( fabs(m_diff_m_dot_out_ncall + m_diff_m_dot_old_ncall) < 0.01 )
				{
					m_dot_toPB = 0.5*m_m_dot_prev_ncall + 0.5*m_dot_toPB;
					m_diff_m_dot_out_ncall = m_dot_toPB - m_m_dot_prev_ncall;
				}
			}
		}

		if( ncall > 0 && fabs(m_diff_m_dot_out_ncall/m_dot_toPB) < 0.005 && fabs(diff_dp_b) < 0.005 && fabs(diff_dp_sh) < 0.005 && fabs(diff_dp_rh) < 0.005 )
		{
			m_dot_toPB = m_m_dot_prev_ncall;
			f_timestep = f_timestep_prev_ncall;
			dp_b = m_dp_b_prev_ncall;
			dp_sh = m_dp_sh_prev_ncall;
			dp_rh = m_dp_rh_prev_ncall;
		}

		m_m_dot_prev_ncall = m_dot_toPB;
		f_timestep_prev_ncall = f_timestep;
		m_dp_b_prev_ncall = dp_b;
		m_dp_sh_prev_ncall = dp_sh;
		m_dp_rh_prev_ncall = dp_rh;

		bool PB_on = true;
		if( !m_success && m_dot_toPB == 0.0 )
			PB_on = false;

		// Set Outputs
			// Boiler
		double b_m_dot, b_T_max, b_q_out, b_q_in, b_q_conv, b_q_rad, b_q_abs;
		boiler.Get_Other_Boiler_Outputs( b_m_dot, b_T_max, b_q_out, b_q_in, b_q_conv, b_q_rad, b_q_abs );
		
		double sh_q_conv, sh_q_rad, sh_q_abs, sh_T_surf_max, sh_v_exit, sh_q_in;
		if (m_success)
		{
			value( O_T_b_in, (T_in - 273.15) );		//[C] Boiler Inlet Temperature
			value( O_T_boil, (T_boil - 273.15) );	//[C] Boiler Temperature (= recirc temp, steam drum temp)			
			value( O_P_b_out, P_b_out );			//[kPa] Boiler Outlet Pressure
			value( O_P_drop_b, dp_b );				//[Pa] Pressure drop through boiler
			value( O_m_dot_b, b_m_dot*3600.0 );		//[kg/hr] Mass flow rate through boiler
			value( O_eta_b, eta_b );				//[-] Boiler thermal efficiency
			value( O_q_b_conv, b_q_conv );			//[MW] Boiler convective losses
			value( O_q_b_rad, b_q_rad );			//[MW] Boiler radiative losses
			value( O_q_b_abs, b_q_abs );			//[MW] Thermal power absorbed by boiler (before thermal losses)
			value( O_T_max_b_surf, (b_T_max - 273.15) );		//[C] Maximum boiler tube surface temperature
				// Superheater
			value( O_m_dot_sh, m_dot_sh*3600.0 );	//[kg/hr] Mass flow rate through superheater
			value( O_P_sh_out, P_sh_out );			//[kPa] Outlet pressure of superheater
			value( O_dP_sh, dp_sh );				//[Pa] Superheater pressure drop
			value( O_eta_sh, eta_sh );				//[-] Thermal efficiency of superheater			
			superheater.Get_Other_Superheater_Outputs( sh_q_conv, sh_q_rad, sh_q_abs, sh_T_surf_max, sh_v_exit, sh_q_in );
			value( O_q_sh_conv, sh_q_conv );		//[MW] Superheater Convective losses
			value( O_q_sh_rad, sh_q_rad );			//[MW] Superheater Radiative losses
			value( O_q_sh_abs, sh_q_abs );			//[MW] Thermal power absorbed by superheater (before thermal losses)
			value( O_T_max_sh_surf, (sh_T_surf_max - 273.15) );	//[C] Maximum superheater surface temperature
			value( O_v_sh_max, sh_v_exit );					//[m/s] Superheater exit velocity

				// Reheater
			value(O_P_rh_out, P_rh_out);		//[kPa] Reheater outlet pressure			
			value(O_dP_rh, dp_rh);				//[Pa] Reheater pressure drop
			value(O_eta_rh, eta_rh);			//[-] Reheater thermal efficiency
			double rh_q_conv, rh_q_rad, rh_q_abs, rh_T_surf, rh_v_exit, rh_q_in;
			reheater.Get_Other_Superheater_Outputs(rh_q_conv, rh_q_rad, rh_q_abs, rh_T_surf, rh_v_exit, rh_q_in);
			value(O_T_max_rh_surf, (rh_T_surf - 273.15));	//[C] Maximum reheater surface temperature
			value(O_v_rh_max, rh_v_exit);		//[m/s] Reheater exit velocity
			value(O_q_rh_conv, rh_q_conv);		//[MW] Convective losses
			value(O_q_rh_rad, rh_q_rad);		//[MW] Radiative losses
			value(O_q_rh_abs, rh_q_abs);		//[MW] Thermal power absorbed by reheater (before thermal losses)
			// Combined
			double EnergyInComb = b_q_in + sh_q_in + rh_q_in;
			double field_eff_adj = m_field_eff * m_defocus;
			double q_abs_rec = b_q_abs + sh_q_abs + rh_q_abs;		//[MW] Thermal power absorbed by receiver
			double q_conv_rec = b_q_conv + sh_q_conv + rh_q_conv;	//[MW] Receiver convective losses
			double q_rad_rec = b_q_rad + sh_q_rad + rh_q_rad;		//[MW] Receiver radiative losses
			double eta_therm_rec = q_therm_in_rec / (max(0.001, EnergyInComb));

			value(O_q_inc_full, m_q_total / 1.E6);		//[MW] Total incident radiation on receiver before defocus
			value(O_q_inc_actual, EnergyInComb / 1.E6);	//[MW] Total energy incident on receiver
			value(O_defocus, m_defocus);				//[-] Defocus fraction
			value(O_field_eta_adj, field_eff_adj);		//[-] Adjusted field efficiency
			value(O_q_abs_rec, q_abs_rec);				//[MW] Thermal power absorbed by receiver
			value(O_q_conv_rec, q_conv_rec);			//[MW] Receiver convective losses
			value(O_q_rad_rec, q_rad_rec);				//[MW] Receiver radiative losses
			value(O_q_abs_less_rad, q_abs_rec - q_rad_rec);		//[MW] Thermal power absorbed less radiation losses
			value(O_q_therm_in_rec, q_therm_in_rec / 1.E6);		//[MW] Thermal power absorbed by steam in receiver
			value(O_eta_rec, eta_therm_rec);					//[-] Receiver thermal efficiency
		}
		else
		{
			// Boiler
			value(O_T_b_in, 0.0);
			value(O_T_boil, 0.0);
			value(O_P_b_out, 0.0);
			value(O_P_drop_b, 0.0);
			value(O_m_dot_b, 0.0);
			value(O_eta_b, 0.0);
			value(O_q_b_conv, 0.0);
			value(O_q_b_rad, 0.0);
			value(O_q_b_abs, 0.0);
			value(O_T_max_b_surf, 0.0);
			// Superheater
			value(O_m_dot_sh, 0.0);
			value(O_P_sh_out, 0.0);
			value(O_dP_sh, 0.0);
			value(O_eta_sh, 0.0);
			value(O_q_sh_conv, 0.0);
			value(O_q_sh_rad, 0.0);
			value(O_q_sh_abs, 0.0);
			value(O_T_max_sh_surf, 0.0);
			value(O_v_sh_max, 0.0);
			// Reheater
			value(O_P_rh_out, 0.0);
			value(O_dP_rh, 0.0);
			value(O_eta_rh, 0.0);
			value(O_T_max_rh_surf, 0.0);
			value(O_v_rh_max, 0.0);
			value(O_q_rh_conv, 0.0);
			value(O_q_rh_rad, 0.0);
			value(O_q_rh_abs, 0.0);

			value(O_q_inc_full, 0.0);
			value(O_q_inc_actual, 0.0);
			value(O_defocus, 0.0);
			value(O_field_eta_adj, 0.0);
			value(O_q_abs_rec, 0.0);
			value(O_q_conv_rec, 0.0);
			value(O_q_rad_rec, 0.0);
			value(O_q_abs_less_rad, 0.0);
			value(O_q_therm_in_rec, 0.0);
			value(O_eta_rec, 0.0);

		}

		if( PB_on )
		{
				// Powerblock specific
			value(O_T_fw, (T_fw - 273.15));			//[C] Feedwater Outlet Temperature
			value(O_P_b_in, P_b_in);				//[kPa] Boiler Inlet Pressure
			value(O_f_mdot_rh, f_mdotrh);			//[-] Reheater mass flow rate fraction
			value(O_P_rh_in, m_P_rh_in);			//[kPa] Reheater inlet pressure
			value(O_T_rh_in, (T_rh_in - 273.15));	//[C] Reheater inlet temperature
			value(O_T_rh_out, (T_rh_target - 273.15));	//[C] Reheater outlet temperature
		}
		else
		{
				// Powerblock specific
			value(O_T_fw, 0.0);
			value(O_P_b_in, 0.0);
			value(O_f_mdot_rh, 0.0);
			value(O_P_rh_in, 0.0);
			value(O_T_rh_in, 0.0);
			value(O_T_rh_out, 0.0);			
		}

		value( O_W_dot_boost, W_dot_boost/1.E6 );				//[MW] Feedwater booster pump power
			// Auxiliary
		value( O_m_dot_aux, m_m_dot_aux*3600.0 );				//[kg/hr] Auxiliary mass flow rate
		value( O_q_aux, m_q_aux/1.E6 );							//[MW] Auxiliary heat rate delivered to cycle
		value( O_q_aux_fuel, q_aux_fuel );						//[MMBTU] Fuel energy rate delivered to aux heater
			// Controls
		value( O_standby_control, m_standby_control );			//[-] 1: Turbine can operate, 2: Turbine can be in standby, 3: Turbine is off
		value( O_f_timestep, f_timestep );						//[-] Fraction of timestep turbine can operate due to receiver start-up
		value( O_m_dot_toPB, m_dot_toPB*3600.0 );				//[kg/hr] Mass flow rate to power block (m_dot_sh + m_dot_aux)

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		m_E_su_rec_prev = m_E_su_rec;	//[W-hr] Startup energy remaining
		m_t_su_rec_prev = m_t_su_rec;	//[hr] Startup time remaining
		m_t_sb_pb_prev = m_t_sb_pb;		//[hr] Remaining standby operation time

		if( m_df_flag )
		{
			// call messages(-1,"The outlet pressure of either the superheater or reheater was less than the condenser pressure, requiring defocus",'WARNING',INFO(1),INFO(2))
		}

		if( value( I_P_b_in ) )
			m_high_pres_count++;


		// if( time == last_step && m_high_pres_count > 0 )
		// {
		//		call messages(-1,"The boiler inlet pressure is greater than 190 bar (steam property limit) during some timesteps.  It is limited to 190 in the boiler model which may affect the accuracy of some calculations", 'WARNING', INFO(1), INFO(2))
		// }

		return 0;
	}
};

TCS_IMPLEMENT_TYPE( sam_dsg_controller_type265, "Direct steam receiver controller", "Ty Neises", 1, sam_dsg_controller_type265_variables, NULL, 1 )







