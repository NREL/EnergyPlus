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

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"

#include <vector>

// This compute module finds the optimal cycle efficiency given design point conditions
//    the user must provide the recuperator UA

static var_info _cm_vtab_sco2_design_cycle[] = {
/*  VARTYPE   DATATYPE         NAME                  LABEL                                                UNITS     META        GROUP                      REQUIRED_IF          CONSTRAINTS   UI_HINTS*/
{ SSC_INPUT,  SSC_NUMBER,     "I_W_dot_net_des",     "Design cycle power output",                         "MW",     "",         "sCO2 power cycle",         "*",                "",           "" },		
{ SSC_INPUT,  SSC_NUMBER,     "I_T_mc_in_des",       "Main compressor inlet temp at design",              "C",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_T_t_in_des",        "Turbine inlet temp at design",                      "C",      "",         "sCO2 power cycle",         "*",                "",           "" },								
{ SSC_INPUT,  SSC_NUMBER,     "I_N_t_des",           "Design turbine speed, negative links to comp.",     "rpm",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_mc",            "Design main compressor isentropic efficiency",      "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_rc",            "Design re-compressor isentropic efficiency",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_t",             "Design turbine isentropic efficiency",              "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_tol",               "Convergence tolerance for performance calcs",       "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_opt_tol",           "Convergence tolerance - optimization calcs",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_UA_total_des",      "Total UA allocatable to recuperators",              "kW/K",   "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_P_high_limit",      "High pressure limit in cycle",                      "MPa",    "",         "sCO2 power cycle",         "*",                "",           "" },

{ SSC_OUTPUT, SSC_NUMBER,     "O_LT_frac_des",       "Optimized design point UA distribution",            "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_P_mc_out_des",      "Optimized design point high side pressure",         "MPa",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_PR_mc_des",         "Optimized Pressure Ratio across main compressor",   "",       "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_recomp_frac_des",   "Optimized recompression fraction",                  "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_eta_thermal_des",   "Design cycle thermal efficiency",                   "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_N_mc_des",          "Design point compressor shaft speed",               "rpm",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_m_dot_PHX",         "Mass flow rate through primary HX",                 "kg/s",   "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_ARRAY,      "O_T_array_des",       "Cycle temp state points at design",                 "K",      "",         "sCO2 power cycle",         "*",                "",           "" },


var_info_invalid };

class cm_sco2_design_cycle : public compute_module
{
public:

	cm_sco2_design_cycle()
	{
		add_var_info(_cm_vtab_sco2_design_cycle);
	}

	void exec() override
	{		
		double W_dot_net_des = as_double("I_W_dot_net_des")*1.E3;	//[MW] convert from [kW]
		double T_mc_in_des = as_double("I_T_mc_in_des")+273.15;		//[K] convert from [C]
		double T_t_in_des = as_double("I_T_t_in_des")+273.15;		//[K] convert from [C]
		double N_t_des = as_double("I_N_t_des");					//[rpm], if negative, then link to compressor
		double eta_mc = as_double("I_eta_mc");						//[-]
		double eta_rc = as_double("I_eta_rc");						//[-]
		double eta_t = as_double("I_eta_t");						//[-]
		double tol = as_double("I_tol");							//[-]
		double opt_tol = as_double("I_opt_tol");					//[-]
		double UA_total_des = as_double("I_UA_total_des");			//[kW/K]
		double P_high_limit = as_double("I_P_high_limit")*1.E3;		//[kPa] convert from MPa

		// Define hardcoded sco2 design point parameters
		std::vector<double> DP_LT(2);
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		DP_LT[0] = 0;
		DP_LT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_HT(2);
		DP_HT[0] = 0;
		DP_HT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PC(2);
		DP_PC[0] = 0;
		DP_PC[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PHX(2);
		DP_PHX[0] = 0;
		DP_PHX[1] = 0;
		int N_sub_hxrs = 10;

		C_sco2_cycle_core::S_auto_opt_design_parameters ms_rc_autodes_par;

		ms_rc_autodes_par.m_DP_HTR = DP_HT;
		ms_rc_autodes_par.m_DP_LTR = DP_LT;
		ms_rc_autodes_par.m_DP_PC_main = DP_PC;
		ms_rc_autodes_par.m_DP_PHX = DP_PHX;

		ms_rc_autodes_par.m_eta_mc = eta_mc;
		ms_rc_autodes_par.m_eta_rc = eta_rc;
		ms_rc_autodes_par.m_eta_t = eta_t;

        ms_rc_autodes_par.m_LTR_N_sub_hxrs = N_sub_hxrs;
        ms_rc_autodes_par.m_HTR_N_sub_hxrs = N_sub_hxrs;
		ms_rc_autodes_par.m_N_turbine = N_t_des;

		ms_rc_autodes_par.m_des_opt_tol = opt_tol;
		ms_rc_autodes_par.m_P_high_limit = P_high_limit;
		ms_rc_autodes_par.m_des_tol = tol;
		ms_rc_autodes_par.m_T_mc_in = T_mc_in_des;
		ms_rc_autodes_par.m_T_t_in = T_t_in_des;
		ms_rc_autodes_par.m_UA_rec_total = UA_total_des;
		ms_rc_autodes_par.m_W_dot_net = W_dot_net_des;

		C_RecompCycle ms_rc_cycle;
		int auto_opt_error_code = 0;
		auto_opt_error_code = ms_rc_cycle.auto_opt_design(ms_rc_autodes_par);
		if( auto_opt_error_code != 0 )
		{
			char tstr[300];
			sprintf(tstr, "Cycle auto-optimization failed with Error Message = %d.", auto_opt_error_code);

			string error_msg = "";

			error_msg.append(tstr);

			throw exec_error("sco2 design cycle", error_msg);
		}
		
		double LT_frac = ms_rc_cycle.get_design_solved()->m_UA_LTR / (ms_rc_cycle.get_design_solved()->m_UA_LTR + ms_rc_cycle.get_design_solved()->m_UA_HTR);

		assign("O_LT_frac_des", var_data((ssc_number_t) LT_frac));		//[-]
		assign("O_P_mc_out_des", var_data((ssc_number_t) (ms_rc_cycle.get_design_solved()->m_pres[2-1]/1000.0)));	//[MPa] convert from kPa
		assign("O_PR_mc_des", var_data((ssc_number_t) (ms_rc_cycle.get_design_solved()->m_pres[2-1]/ms_rc_cycle.get_design_solved()->m_pres[1-1])));	//[-]
		assign("O_recomp_frac_des", var_data((ssc_number_t) ms_rc_cycle.get_design_solved()->m_recomp_frac));	//[-]
		assign("O_eta_thermal_des", var_data((ssc_number_t) ms_rc_cycle.get_design_solved()->m_eta_thermal));	//[-]
		assign("O_N_mc_des", var_data((ssc_number_t) ms_rc_cycle.get_design_solved()->ms_mc_ms_des_solved.m_N_design));		//[-]
		assign("O_m_dot_PHX", var_data((ssc_number_t) ms_rc_cycle.get_design_solved()->ms_t_des_solved.m_N_design));
		
		// Assign temperatures to array
		const std::vector<double> T_vector = ms_rc_cycle.get_design_solved()->m_temp;
		size_t l_T_array = T_vector.size();
		ssc_number_t * T_array = new ssc_number_t[l_T_array];
		
		for( size_t i = 0; i < l_T_array; i++ )
			T_array[i] = (ssc_number_t)(T_vector[i]);

		// Assign temp array to var_data
		assign("O_T_array_des", var_data(T_array, l_T_array));
		delete [] T_array;

		return;
	}

};

DEFINE_MODULE_ENTRY(sco2_design_cycle, "Calls sCO2 auto-design cycle function", 1)
