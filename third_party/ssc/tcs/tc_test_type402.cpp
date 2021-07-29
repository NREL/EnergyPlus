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
#include <vector>
#include "thermocline_tes.h"

using namespace std;

enum{	//Parameters
	P_h,
	P_A,
	P_fill_mat,
	P_U,
	P_U_top,
	P_U_bot,
	P_f_void,
	P_capfac,
	P_Thmin,
	P_Tcmax,
	P_nodes,
	P_T_hot_ini,
	P_T_cold_ini,
	P_TC_break,
	P_T_htr_set,
	P_max_htr_q,
	P_n_pairs,
	P_fluid_mat,

	//Inputs
	I_T_hot_in,
	I_flow_h,
	I_T_cold_in,
	I_flow_c,
	I_T_env,
	I_solve_mode,
	I_Q_dis_target,
	I_Q_cha_target,
	I_f_storage,
	I_delta_time,
	
	//Outputs
	O_m_dot_dis_avail,
	O_T_dis_avail,
	O_m_dot_cha_avail,
	O_T_cha_avail,
	O_Q_dot_out,
	O_Q_dot_losses,
	O_T_hot_bed,
	O_T_cold_bed,
	O_T_max_bed,
	O_f_hot,
	O_f_cold,
	O_Q_htr,
	O_T_TC_start,

	//N_MAX
	N_MAX
};

tcsvarinfo tc_test_type402_variables[] = {
	//PARAMETERS
	{ TCS_PARAM, TCS_NUMBER, P_h,          "h",          "Height of the rock bed storage tank",                               "m",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_A,          "A",          "Cross-sectional area of storage tank",                              "m2",         "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_fill_mat,   "fill_mat",   "Filler material integer - see code",                                "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_U,          "U",          "Tank loss coefficient",                                             "kJ/hr-m2-K", "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_U_top,      "U_top",      "Top surface loss coefficient",                                      "kJ/hr-m2-K", "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_U_bot,      "U_bot",      "Bottom surface loss coefficient",                                   "kJ/hr-m2-k", "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_f_void,     "f_void",     "Rock bed void fraction",                                            "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_capfac,     "capfac",     "Bottom thermal mass capacitance factor multiplier",                 "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_Thmin,      "Thmin",      "Min allowable hot side outlet temp during discharage",              "C",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_Tcmax,      "Tcmax",      "Max allowable cold side outlet temp during charge",                 "C",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_nodes,      "nodes",      "Number of nodes in thermocline model",                              "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_hot_ini,  "T_hot_ini",  "Initial thermocline hot temperature",                               "C",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_cold_ini, "T_cold_ini", "Initial thermocline cold temperature",                              "C",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_TC_break,   "TC_break",   "Fraction into tank for initial TC break (0: all hot, 1: all cold)", "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_htr_set,  "T_htr_set",  "Min tank temp before aux heater starts",                            "C",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_max_htr_q,  "max_htr_q",  "Capacity of tank heater",                                           "MW",         "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_n_pairs,    "n_pairs",    "Number of equivalent tank pairs",                                   "-",          "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_fluid_mat,  "fluid_mat",  "Fluid material integer - see code",                                 "-",          "", "", "" },

	//INPUTS
	{ TCS_INPUT, TCS_NUMBER, I_T_hot_in,      "T_hot_in",       "Charging (hot) temp into top of tank",                              "C",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_flow_h,        "flow_h",         "Charging (hot) mass flow rate into top of tank",                    "kg/hr",      "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_cold_in,     "T_cold_in",      "Discharging (cold) temp into bottom of tank",                       "C",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_flow_c,        "flow_c",         "Discharging (cold) mass flow rate into bottom of tank",             "kg/hr",      "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_env,         "T_env",          "Ambient (environment) temperature",                                 "C",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_solve_mode,    "solve_mode",     "Solve thermocline (1) or check availability (2)",                   "-",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_Q_dis_target,  "Q_dis_target",   "Discharge rate required by controller",                             "W",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_Q_cha_target,  "Q_cha_target",   "Charge rate required by controller",                                "W",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_f_storage,     "f_storage",      "Storage dispatch fraction",                                         "-",          "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_delta_time,    "delta_time",     "Duration of steady state timestep",                                 "hr",         "", "", "" },

	//OUTPUTS
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_dis_avail,  "m_dot_dis_avail",    "Mass flow rate available for discharge",                    "kg/hr",      "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_dis_avail,      "T_dis_avail",        "Discharge (hot) outlet temperature (time averaged)",        "C",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_m_dot_cha_avail,  "m_dot_cha_avail",    "Mass flow rate available for charging",                     "kg/hr",      "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_cha_avail,      "T_cha_avail",        "Charge (cold) outlet temperature (time averaged)",          "C",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_dot_out,        "Q_dot_out",          "Thermal power (always +)",                                  "W",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_dot_losses,     "Q_dot_losses",       "Energy lost to environment",                                "kJ",         "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_hot_bed,        "T_hot_bed",          "Final temp at hot (top) node",                              "C",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_cold_bed,       "T_cold_bed",         "Final temp at cold (bottom) node",                          "C",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_max_bed,        "T_max_bed",          "Maximum temp in tank",                                      "C",          "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_f_hot,            "f_hot",              "Fraction of depth at which hot temperature decreases below minimum hot temperature limit", "-", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_f_cold,           "f_cold",             "Fraction of depth at which cold temperature increases above maximum cold temperature limit", "-", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_htr,            "Q_dot_htr",          "Total energy required by heater to maintain min temp in tank", "kJ",      "", "", "" },
	{ TCS_OUTPUT, TCS_ARRAY,  O_T_TC_start,       "T_TC_start",         "Temperature profile of thermocline at start of timestep",   "C",          "", "", "" },

	//N_MAX
	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};

class tc_test_type402 : public tcstypeinterface
{
private:
	//Parameters
	double n_zen;

	Thermocline_TES   thermocline;
	HTFProperties     htfProps;		// Instance of HTFProperties class for field HTF

public:
	tc_test_type402(tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface(cst, ti)
	{
		n_zen = std::numeric_limits<double>::quiet_NaN();		
	}

	virtual ~tc_test_type402()
	{
	}

	virtual int init()
	{	
		double h =             value(P_h);         
		double A =         	   value(P_A);         
		int fill_mat =  	   (int)value(P_fill_mat);  
		double U =         	   value(P_U);         
		double U_top =     	   value(P_U_top);     
		double U_bot =     	   value(P_U_bot);     
		double f_void =    	   value(P_f_void);    
		double capfac =    	   value(P_capfac);    
		double Thmin =     	   value(P_Thmin);     
		double Tcmax =     	   value(P_Tcmax);     
		double nodes =     	   value(P_nodes);     
		double T_hot_ini = 	   value(P_T_hot_ini); 
		double T_cold_ini =	   value(P_T_cold_ini);
		double TC_break =  	   value(P_TC_break);  
		double T_htr_set = 	   value(P_T_htr_set); 
		double max_htr_q = 	   value(P_max_htr_q); 
		double n_pairs =   	   value(P_n_pairs);   
		int fluid_mat = 	   (int) value(P_fluid_mat); 


		htfProps.SetFluid(fluid_mat);
		
		thermocline.Initialize_TC(h,A,fill_mat,U,U_top,U_bot,f_void,capfac,Thmin,Tcmax,(int)nodes,T_hot_ini,T_cold_ini,
			TC_break,T_htr_set,max_htr_q,(int)n_pairs,htfProps);

		return 0;
	}

	virtual int call(double /*time*/, double /*step*/, int /*ncall*/)
	{
		double T_hot_in =          value(I_T_hot_in);    
		double flow_h =      	   value(I_flow_h);      
		double T_cold_in =   	   value(I_T_cold_in);   
		double flow_c =      	   value(I_flow_c);      
		double T_env =       	   value(I_T_env);       
		double solve_mode =  	   value(I_solve_mode);  
		double Q_dis_target =	   value(I_Q_dis_target);
		double Q_cha_target =	   value(I_Q_cha_target);
		double f_storage =   	   value(I_f_storage);   
		double delta_time =  	   value(I_delta_time);  

		double m_dot_dis_avail =   std::numeric_limits<double>::quiet_NaN();
		double T_dis_avail =       std::numeric_limits<double>::quiet_NaN();
		double m_dot_cha_avail =   std::numeric_limits<double>::quiet_NaN();
		double T_cha_avail =       std::numeric_limits<double>::quiet_NaN();
		double Q_dot_out =         std::numeric_limits<double>::quiet_NaN();
		double Q_dot_losses =      std::numeric_limits<double>::quiet_NaN();
		double T_hot_bed =         std::numeric_limits<double>::quiet_NaN();
		double T_cold_bed =        std::numeric_limits<double>::quiet_NaN();
		double T_max_bed =         std::numeric_limits<double>::quiet_NaN();
		double f_hot =             std::numeric_limits<double>::quiet_NaN();
		double f_cold =            std::numeric_limits<double>::quiet_NaN();
		double Q_dot_htr =         std::numeric_limits<double>::quiet_NaN();

		thermocline.Solve_TC(T_hot_in, flow_h, T_cold_in, flow_c, T_env, (int)solve_mode, Q_dis_target, Q_cha_target, f_storage,
			delta_time, m_dot_dis_avail, T_dis_avail, m_dot_cha_avail, T_cha_avail, Q_dot_out, Q_dot_losses, T_hot_bed,
			T_cold_bed, T_max_bed, f_hot, f_cold, Q_dot_htr);

		value(O_m_dot_dis_avail, m_dot_dis_avail);
		value(O_T_dis_avail,     T_dis_avail);    
		value(O_m_dot_cha_avail, m_dot_cha_avail);
		value(O_T_cha_avail,     T_cha_avail);    
		value(O_Q_dot_out,       Q_dot_out);      
		value(O_Q_dot_losses,    Q_dot_losses);   
		value(O_T_hot_bed,       T_hot_bed);      
		value(O_T_cold_bed,      T_cold_bed);     
		value(O_T_max_bed,       T_max_bed);      
		value(O_f_hot,           f_hot);          
		value(O_f_cold,          f_cold);         
		value(O_Q_htr,           Q_dot_htr);      		

		return 0;
	}

	virtual int converged(double time)
	{
		// Reset 'prev' arrays for next timestep
		thermocline.Converged(time);

		return 0;
	}

};

TCS_IMPLEMENT_TYPE(tc_test_type402, "Test type for thermocline storage", "Ty Neises", 1, tc_test_type402_variables, NULL, 1)

