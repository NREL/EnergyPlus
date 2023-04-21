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

#include "sco2_pc_csp_int.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"
#include "sco2_partialcooling_cycle.h"

#include "csp_solver_util.h"
#include "CO2_properties.h"
#include <cmath>
#include <string>

#include "nlopt.hpp"

#include "fmin.h"

C_sco2_phx_air_cooler::C_sco2_phx_air_cooler()
{
	// Get CO2 critical temperature
	CO2_info co2_fluid_info;
	get_CO2_info(&co2_fluid_info);
	m_T_co2_crit = co2_fluid_info.T_critical;		//[K]
	m_P_co2_crit = co2_fluid_info.P_critical;		//[kPa]

    // Defaul critical temperature limit
    m_is_T_crit_limit = true;

	mf_callback_update = 0;		// NULL
	mp_mf_update = 0;			// NULL
}

void C_sco2_phx_air_cooler::design(S_des_par des_par)
{
	ms_des_par = des_par;

	design_core();
}

void C_sco2_phx_air_cooler::C_P_LP_in_iter_tracker::reset_vectors()
{
    mv_P_LP_in.resize(0);
    mv_W_dot_net.resize(0);
    mv_P_mc_out.resize(0);
    mv_od_error_code.resize(0);
    mv_is_converged.resize(0);
}

void C_sco2_phx_air_cooler::C_P_LP_in_iter_tracker::push_back_vectors(double P_LP_in /*kpa*/, double W_dot_net /*kWe*/, double P_mc_out /*kPa*/,
    int od_error_code, bool is_converged)
{
    mv_P_LP_in.push_back(P_LP_in);      //[kPa]
    mv_W_dot_net.push_back(W_dot_net);  //[kWe]
    mv_P_mc_out.push_back(P_mc_out);    //[kPa]
    mv_od_error_code.push_back(od_error_code);  //[-]
    mv_is_converged.push_back(is_converged);    //[-]
}

void C_sco2_phx_air_cooler::design_core()
{
	// using -> C_RecompCycle::S_auto_opt_design_hit_eta_parameters
	std::string error_msg;
	int auto_err_code = 0;
	std::string s_cycle_config = "";

	if (ms_des_par.m_cycle_config == 2)
	{
		mpc_sco2_cycle = &mc_partialcooling_cycle;
		s_cycle_config = "partial cooling";
	}
	else
	{
		mpc_sco2_cycle = &mc_rc_cycle;
		s_cycle_config = "recompression";
	}

	// Set min temp
	m_T_mc_in_min = mpc_sco2_cycle->get_design_limits().m_T_mc_in_min;		//[K]
	
	if (ms_des_par.m_design_method == 1)
	{
		// Design the cycle to hit a specified efficiency
		// Define sCO2 cycle design parameter structure
		ms_cycle_des_par.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		ms_cycle_des_par.m_eta_thermal = ms_des_par.m_eta_thermal;	//[-]
		ms_cycle_des_par.m_T_mc_in = ms_des_par.m_T_amb_des + ms_des_par.m_dt_mc_approach;	//[K]
		if (ms_cycle_des_par.m_T_mc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The input design main compressor inlet temperature is %lg [C]."
				" The sCO2 cycle design code reset it to the minimum allowable design main compressor inlet temperature: %lg [C].",
				ms_cycle_des_par.m_T_mc_in - 273.15,
				m_T_mc_in_min - 273.15);
		}
		ms_cycle_des_par.m_T_pc_in = ms_cycle_des_par.m_T_mc_in;		//[K]
		ms_cycle_des_par.m_T_t_in = ms_des_par.m_T_htf_hot_in - ms_des_par.m_phx_dt_hot_approach;	//[K]
		ms_cycle_des_par.m_DP_LT = ms_des_par.m_DP_LT;
		ms_cycle_des_par.m_DP_HT = ms_des_par.m_DP_HT;
		ms_cycle_des_par.m_DP_PC_pre = ms_des_par.m_DP_PC;
		ms_cycle_des_par.m_DP_PC_main = ms_des_par.m_DP_PC;
		ms_cycle_des_par.m_DP_PHX = ms_des_par.m_DP_PHX;
            // LTR thermal design
        ms_cycle_des_par.m_LTR_target_code = ms_des_par.m_LTR_target_code;  //[-]
        ms_cycle_des_par.m_LTR_UA = ms_des_par.m_LTR_UA;                    //[kW/K]
        ms_cycle_des_par.m_LTR_min_dT = ms_des_par.m_LTR_min_dT;            //[K]
        ms_cycle_des_par.m_LTR_eff_target = ms_des_par.m_LTR_eff_target;    //[-]
		ms_cycle_des_par.m_LTR_eff_max = ms_des_par.m_LTR_eff_max;       //[-]
        ms_cycle_des_par.m_LTR_N_sub_hxrs = ms_des_par.m_LTR_N_sub_hxrs;    //[-]
        ms_cycle_des_par.m_LTR_od_UA_target_type = ms_des_par.m_LTR_od_UA_target_type;
            // HTR thermal design
        ms_cycle_des_par.m_HTR_target_code = ms_des_par.m_HTR_target_code;  //[-]
        ms_cycle_des_par.m_HTR_UA = ms_des_par.m_HTR_UA;                    //[kW/K]
        ms_cycle_des_par.m_HTR_min_dT = ms_des_par.m_HTR_min_dT;            //[K]
        ms_cycle_des_par.m_HTR_eff_target = ms_des_par.m_HTR_eff_target;    //[-]
        ms_cycle_des_par.m_HTR_eff_max = ms_des_par.m_HTR_eff_max;       //[-]
        ms_cycle_des_par.m_HTR_N_sub_hxrs = ms_des_par.m_HTR_N_sub_hxrs;    //[-]
        ms_cycle_des_par.m_HTR_od_UA_target_type = ms_des_par.m_HTR_od_UA_target_type;
            //
		ms_cycle_des_par.m_eta_mc = ms_des_par.m_eta_mc;
        ms_cycle_des_par.m_mc_comp_model_code = ms_des_par.m_mc_comp_type;
		ms_cycle_des_par.m_eta_rc = ms_des_par.m_eta_rc;
		ms_cycle_des_par.m_eta_pc = ms_des_par.m_eta_pc;
		ms_cycle_des_par.m_eta_t = ms_des_par.m_eta_t;
		ms_cycle_des_par.m_P_high_limit = ms_des_par.m_P_high_limit;
		ms_cycle_des_par.m_des_tol = ms_des_par.m_des_tol;
		ms_cycle_des_par.m_des_opt_tol = ms_des_par.m_des_opt_tol;
		ms_cycle_des_par.m_N_turbine = ms_des_par.m_N_turbine;
		ms_cycle_des_par.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;

		ms_cycle_des_par.m_is_des_air_cooler = ms_des_par.m_is_des_air_cooler;		//[-]
		ms_cycle_des_par.m_frac_fan_power = ms_des_par.m_frac_fan_power;			//[-]
		ms_cycle_des_par.m_deltaP_cooler_frac = ms_des_par.m_deltaP_cooler_frac;	//[-]
		ms_cycle_des_par.m_T_amb_des = ms_des_par.m_T_amb_des;						//[K]
		ms_cycle_des_par.m_elevation = ms_des_par.m_elevation;						//[m]
        ms_cycle_des_par.m_eta_fan = ms_des_par.m_eta_fan;                          //[-]
        ms_cycle_des_par.m_N_nodes_pass = ms_des_par.m_N_nodes_pass;                //[-]

		ms_cycle_des_par.m_des_objective_type = ms_des_par.m_des_objective_type;		//[-]
		ms_cycle_des_par.m_min_phx_deltaT = ms_des_par.m_min_phx_deltaT;				//[C]

		ms_cycle_des_par.m_fixed_P_mc_out = ms_des_par.m_fixed_P_mc_out;	//[-]

		ms_cycle_des_par.m_PR_HP_to_LP_guess = ms_des_par.m_PR_HP_to_LP_guess;      //[-]
		ms_cycle_des_par.m_fixed_PR_HP_to_LP = ms_des_par.m_fixed_PR_HP_to_LP;      //[-]

        ms_cycle_des_par.m_f_PR_HP_to_IP_guess = ms_des_par.m_f_PR_HP_to_IP_guess;      //[-]
        ms_cycle_des_par.m_fixed_f_PR_HP_to_IP = ms_des_par.m_fixed_f_PR_HP_to_IP;      //[-]

		ms_cycle_des_par.mf_callback_log = mf_callback_update;
		ms_cycle_des_par.mp_mf_active = mp_mf_update;

		auto_err_code = mpc_sco2_cycle->auto_opt_design_hit_eta(ms_cycle_des_par, error_msg);
	}
	else if (ms_des_par.m_design_method == 2 || ms_des_par.m_design_method == 3)
	{
		if (ms_des_par.m_design_method == 2)
		{
			if (ms_des_par.m_UA_recup_tot_des < 0.0)
			{
				std::string ex_msg = "The " + s_cycle_config + " cycle and CSP integration design, design method 2, conductance must be > 0";
				throw(C_csp_exception(ex_msg.c_str()));
			}
		}
		
		C_sco2_cycle_core::S_auto_opt_design_parameters des_params;
		des_params.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		des_params.m_T_mc_in = ms_des_par.m_T_amb_des + ms_des_par.m_dt_mc_approach;	//[K]
		if (ms_cycle_des_par.m_T_mc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The input design main compressor inlet temperature is %lg [C]."
				" The sCO2 cycle design code reset it to the minimum allowable design main compressor inlet temperature: %lg [C].",
				ms_cycle_des_par.m_T_mc_in - 273.15,
				m_T_mc_in_min - 273.15);
		}
		des_params.m_T_pc_in = des_params.m_T_mc_in;		//[K]
		des_params.m_T_t_in = ms_des_par.m_T_htf_hot_in - ms_des_par.m_phx_dt_hot_approach;	//[K]
		des_params.m_DP_LTR = ms_des_par.m_DP_LT;
		des_params.m_DP_HTR = ms_des_par.m_DP_HT;
		des_params.m_DP_PC_pre = ms_des_par.m_DP_PC;
		des_params.m_DP_PC_main = ms_des_par.m_DP_PC;
		des_params.m_DP_PHX = ms_des_par.m_DP_PHX;
		des_params.m_UA_rec_total = ms_des_par.m_UA_recup_tot_des;	//[kW/K]
            // LTR thermal design
        des_params.m_LTR_target_code = ms_des_par.m_LTR_target_code;  //[-]
        des_params.m_LTR_UA = ms_des_par.m_LTR_UA;                      //[kW/K]
        des_params.m_LTR_min_dT = ms_des_par.m_LTR_min_dT;            //[K]
        des_params.m_LTR_eff_target = ms_des_par.m_LTR_eff_target;    //[-]
        des_params.m_LTR_eff_max = ms_des_par.m_LTR_eff_max;       //[-]
        des_params.m_LTR_N_sub_hxrs = ms_des_par.m_LTR_N_sub_hxrs;  //[-]
        des_params.m_LTR_od_UA_target_type = ms_des_par.m_LTR_od_UA_target_type;
            // HTR thermal design
        des_params.m_HTR_target_code = ms_des_par.m_HTR_target_code;    //[-]
        des_params.m_HTR_UA = ms_des_par.m_HTR_UA;                  //[kW/K]
        des_params.m_HTR_min_dT = ms_des_par.m_HTR_min_dT;          //[K]
        des_params.m_HTR_eff_target = ms_des_par.m_HTR_eff_target;  //[-]
		des_params.m_HTR_eff_max = ms_des_par.m_HTR_eff_max;		//[-]
        des_params.m_HTR_N_sub_hxrs = ms_des_par.m_HTR_N_sub_hxrs;  //[-]
        des_params.m_HTR_od_UA_target_type = ms_des_par.m_HTR_od_UA_target_type;
            //
		des_params.m_eta_mc = ms_des_par.m_eta_mc;
        des_params.m_mc_comp_model_code = ms_des_par.m_mc_comp_type;
		des_params.m_eta_rc = ms_des_par.m_eta_rc;
		des_params.m_eta_pc = ms_des_par.m_eta_pc;
		des_params.m_eta_t = ms_des_par.m_eta_t;
		des_params.m_P_high_limit = ms_des_par.m_P_high_limit;
		des_params.m_des_tol = ms_des_par.m_des_tol;
		des_params.m_des_opt_tol = ms_des_par.m_des_opt_tol;
		des_params.m_N_turbine = ms_des_par.m_N_turbine;

		des_params.m_is_des_air_cooler = ms_des_par.m_is_des_air_cooler;	//[-]
		des_params.m_frac_fan_power = ms_des_par.m_frac_fan_power;			//[-]
		des_params.m_deltaP_cooler_frac = ms_des_par.m_deltaP_cooler_frac;	//[-]
		des_params.m_T_amb_des = ms_des_par.m_T_amb_des;					//[K]
		des_params.m_elevation = ms_des_par.m_elevation;					//[m]
        des_params.m_eta_fan = ms_des_par.m_eta_fan;                        //[-]
        des_params.m_N_nodes_pass = ms_des_par.m_N_nodes_pass;              //[-]

		des_params.m_des_objective_type = ms_des_par.m_des_objective_type;		//[-]
		des_params.m_min_phx_deltaT = ms_des_par.m_min_phx_deltaT;				//[C]

		des_params.m_fixed_P_mc_out = ms_des_par.m_fixed_P_mc_out;	//[-]

		des_params.m_PR_HP_to_LP_guess = ms_des_par.m_PR_HP_to_LP_guess;    //[-]
		des_params.m_fixed_PR_HP_to_LP = ms_des_par.m_fixed_PR_HP_to_LP;    //[-]

        des_params.m_f_PR_HP_to_IP_guess = ms_des_par.m_f_PR_HP_to_IP_guess;    //[-]
        des_params.m_fixed_f_PR_HP_to_IP = ms_des_par.m_fixed_f_PR_HP_to_IP;    //[-]

		des_params.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;

		auto_err_code = mpc_sco2_cycle->auto_opt_design(des_params);
	}
	else
	{
		std::string ex_msg = "The " + s_cycle_config + " cycle and CSP integration design, design method can only be:"
			" 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design";
		throw(C_csp_exception(ex_msg.c_str()));
	}

	if (auto_err_code != 0)
	{
		throw(C_csp_exception(error_msg.c_str()));
	}

	if (error_msg.empty())
	{
		mc_messages.add_notice("The " + s_cycle_config + " cycle design optimization was successful");
	}
	else
	{
		string out_msg = "The sCO2 " + s_cycle_config + " cycle design optimization solved with the following warning(s):\n" + error_msg;
		mc_messages.add_notice(out_msg);
	}

	ms_des_solved.ms_rc_cycle_solved = *mpc_sco2_cycle->get_design_solved();

	// Initialize the PHX
    mc_phx.initialize(ms_des_par.m_hot_fl_code, ms_des_par.mc_hot_fl_props, ms_des_par.m_phx_N_sub_hx, ms_des_par.m_phx_od_UA_target_type);

	// Design the PHX
	double q_dot_des_phx = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;
	//ms_phx_des_par.m_Q_dot_design = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;		//[kWt]
	ms_phx_des_par.m_T_h_in = ms_des_par.m_T_htf_hot_in;	//[K] HTF hot inlet temperature 
		// Okay, but CO2-HTF HX is assumed here. How does "structure inheritance" work?
	ms_phx_des_par.m_P_h_in = 1.0;							// Assuming HTF is incompressible...
	ms_phx_des_par.m_P_h_out = 1.0;						// Assuming HTF is incompressible...
		// .................................................................................
	ms_phx_des_par.m_T_c_in = ms_des_solved.ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_in = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_out = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_IN];		//[K]
	ms_phx_des_par.m_m_dot_cold_des = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;	//[kg/s]
		// Calculating the HTF mass flow rate in 'design_and_calc_m_dot_htf'
	ms_phx_des_par.m_m_dot_hot_des = std::numeric_limits<double>::quiet_NaN();
		// Set maximum effectiveness
	ms_phx_des_par.m_eff_max = 1.0;
	
	mc_phx.design_and_calc_m_dot_htf(ms_phx_des_par, q_dot_des_phx, ms_des_par.m_phx_dt_cold_approach, ms_des_solved.ms_phx_des_solved);

	//*************************************************************************************
	//*************************************************************************************

	return;
}



int C_sco2_phx_air_cooler::off_design_fix_P_mc_in(S_od_par od_par,
                                double P_mc_in /*MPa*/, double T_mc_in /*K*/, double T_pc_in /*K*/,
                                bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/, 
                                bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
                                bool is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
                                bool is_PHX_dP_input, double PHX_f_dP /*-*/,
                                C_sco2_phx_air_cooler::E_off_design_strategies off_design_strategy,
                                double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    // This function sets T_mc_in and T_pc_in... May want to at these instead as arguments
    setup_off_design_info(od_par);
    ms_cycle_od_par.m_T_mc_in = T_mc_in;        //[K]
    ms_cycle_od_par.m_T_pc_in = T_pc_in;        //[K]
	
	// Now, call off-design with the input compressor inlet pressure		
	ms_cycle_od_par.m_P_LP_comp_in = P_mc_in*1.E3;	//[kPa] convert from MPa

    if (get_design_par()->m_cycle_config == 1)
    {
        if (ms_cycle_od_par.m_T_mc_in < N_co2_props::T_crit)
        {
            if (ms_cycle_od_par.m_P_LP_comp_in < N_co2_props::P_crit)
            {
                CO2_state co2_props;
                // Then calculate the compressor inlet pressure that achieves this density at the off-design ambient temperature
                CO2_TQ(ms_cycle_od_par.m_T_mc_in, 0.0, &co2_props);
                double Psat = co2_props.pres;       //[kPa]

                if (ms_cycle_od_par.m_P_LP_comp_in < Psat)
                {
                    ms_cycle_od_par.m_P_LP_comp_in = std::min(ms_cycle_od_par.m_P_LP_comp_in, 0.995*Psat);
                }
                else
                {
                    ms_cycle_od_par.m_P_LP_comp_in = std::max(ms_cycle_od_par.m_P_LP_comp_in, 1.005*Psat);
                }
            }
            else
            {
                ms_cycle_od_par.m_P_LP_comp_in = std::max(ms_cycle_od_par.m_P_LP_comp_in, 1.01*N_co2_props::P_crit);
            }
        }
    }

    // Input RC shaft speed controls
    ms_cycle_od_par.m_is_rc_N_od_at_design = is_rc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_rc_N_od_f_des = rc_N_od_f_des;                //[-]

    // Input MC shaft speed controls
    ms_cycle_od_par.m_is_mc_N_od_at_design = is_mc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_mc_N_od_f_des = mc_N_od_f_des;                //[-]

    // Input PC shaft speed controls
    ms_cycle_od_par.m_is_pc_N_od_at_design = is_pc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_pc_N_od_f_des = pc_N_od_f_des;                //[-]

    // PHX pressure drop options
    ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
    ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

	double eta_od_solved = std::numeric_limits<double>::quiet_NaN();
	int od_core_error_code = off_design_core(eta_od_solved, od_tol);
	
	if (ms_od_solved.m_is_converged)
	{
		double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
		
		if (std::isfinite(mpc_sco2_cycle->get_design_solved()->ms_pc_air_cooler.m_UA_total))
		{
			int air_cooler_err_code = mpc_sco2_cycle->solve_OD_all_coolers_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan);

			if (air_cooler_err_code != 0)
			{
				W_dot_fan = std::numeric_limits<double>::quiet_NaN();
				//throw(C_csp_exception("Off design air cooler model failed"));
			}
		}
	}

	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return od_core_error_code;
}

void C_sco2_phx_air_cooler::setup_off_design_info(C_sco2_phx_air_cooler::S_od_par od_par)
{
	ms_od_par = od_par;

	ms_cycle_od_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]

    if (m_is_T_crit_limit)
    {
        if (ms_cycle_od_par.m_T_mc_in < m_T_mc_in_min)
        {
            std::string msg = util::format("The off-design main compressor inlet temperature is %lg [C]."
                " The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
                ms_cycle_od_par.m_T_mc_in - 273.15,
                m_T_mc_in_min - 273.15);
            ms_cycle_od_par.m_T_mc_in = m_T_mc_in_min;
        }

        if (ms_des_par.m_cycle_config == 2)
        {
            ms_cycle_od_par.m_T_pc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;	//[K]
            if (ms_cycle_od_par.m_T_pc_in < m_T_mc_in_min)
            {
                std::string msg = util::format("The off-design main compressor in let temperture is %lg [C]."
                    " The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
                    ms_cycle_od_par.m_T_pc_in - 273.15,
                    m_T_mc_in_min - 273.15);
                ms_cycle_od_par.m_T_pc_in = m_T_mc_in_min;
            }
        }
    }

	// Begin with no compressor bypass
	ms_cycle_od_par.m_f_mc_pc_bypass = 0.0;

	ms_cycle_od_par.m_tol = ms_des_par.m_des_tol;						//[-]

	// Defined downstream
	ms_cycle_od_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]			
	ms_cycle_od_par.m_P_LP_comp_in = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	
    // Define turbine inlet mode
    ms_cycle_od_par.m_T_t_in_mode = ms_od_par.m_T_t_in_mode;    //[-]

	// Define ms_phx_od_par
	ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
	ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
	ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]

	// Defined downstream
	ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
	ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
}

int C_sco2_phx_air_cooler::optimize_N_mc_and_N_rc__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    C_sco2_phx_air_cooler::E_off_design_strategies off_design_strategy,
    bool is_optimize_N_rc /*-*/, bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_optimize_N_mc /*-*/, bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    bool is_optimize_N_pc /*-*/, bool is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    double & eta_max /*-*/, double & f_N_mc_opt_out /*-*/,
    double & f_N_rc_opt_out /*-*/, double & W_dot_at_eta_max /*kWe*/,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    if (off_design_strategy == E_off_design_strategies::E_TARGET_T_HTF_COLD_POWER_MAX){

        if (!is_optimize_N_rc && !is_optimize_N_mc && !is_optimize_N_pc) {

            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in__tracker;
            off_design__calc_T_pc_in__target_T_htf_cold__max_power(od_par,
                is_rc_N_od_at_design, rc_N_od_f_des,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_pc_N_od_at_design, pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                od_opt_tol_in, od_tol,
                v_T_mc_in__tracker);

            return 0;
        }

        std::vector<double> x;
        std::vector<double> lb;
        std::vector<double> ub;
        std::vector<double> scale;

        if (ms_des_par.m_cycle_config == 2) {
            // Main compressor
            if (is_optimize_N_mc) {
                x.push_back(1.0);
                lb.push_back(0.5);
                ub.push_back(1.0);
                scale.push_back(-0.1);
            }

            // Recompressor
            if (is_optimize_N_rc) {
                x.push_back(1.0);
                lb.push_back(0.5);
                ub.push_back(1.0);
                scale.push_back(-0.1);
            }

            // Precompressor
            if (is_optimize_N_pc) {
                x.push_back(1.0);
                lb.push_back(0.5);
                ub.push_back(1.0);
                scale.push_back(-0.1);
            }
        }
        else {
            // Main compressor
            if (is_optimize_N_mc) {
                x.push_back(1.0);
                lb.push_back(0.5);
                ub.push_back(1.0);
                scale.push_back(-0.1);
            }

            // Recompressor
            if (is_optimize_N_rc && get_design_solved()->ms_rc_cycle_solved.m_is_rc) {
                x.push_back(1.0);
                lb.push_back(0.5);
                ub.push_back(1.0);
                scale.push_back(-0.1);
            }
        }
       
        nlopt::opt opt_N__max_power(nlopt::LN_SBPLX, x.size());
        opt_N__max_power.set_lower_bounds(lb);
        opt_N__max_power.set_upper_bounds(ub);
        opt_N__max_power.set_initial_step(scale);
        opt_N__max_power.set_xtol_abs(0.05);
        opt_N__max_power.set_ftol_rel(0.0005);

        C_sco2_phx_air_cooler::C_to_N_mc_rc_opt inputs(this,
            is_optimize_N_mc, is_mc_N_od_at_design, mc_N_od_f_des,
            is_optimize_N_rc, is_rc_N_od_at_design, rc_N_od_f_des,
            is_optimize_N_pc, is_pc_N_od_at_design, pc_N_od_f_des,
            od_par,
            is_PHX_dP_input, PHX_f_dP,
            off_design_strategy,
            od_opt_tol_in, od_tol);

        try
        {
            opt_N__max_power.set_max_objective(nlopt_cb_opt_N_mc_rc, &inputs);
            double max_obj = std::numeric_limits<double>::quiet_NaN();
            nlopt::result  result_opt_N = opt_N__max_power.optimize(x, max_obj);
        }
        catch (const std::exception&)
        {

        }
        catch (C_csp_exception&)
        {
            return -1;
        }
        catch (...)
        {
            return -1;
        }

        // Get local 'x' and translate to shaft speed guess
        double is_rc_N_od_at_design_local = is_rc_N_od_at_design;
        double is_mc_N_od_at_design_local = is_mc_N_od_at_design;
        double is_pc_N_od_at_design_local = is_pc_N_od_at_design;
        double opt_rc_N_od_f_des = 1.0;
        double opt_mc_N_od_f_des = 1.0;
        double opt_pc_N_od_f_des = 1.0;
        if (get_design_par()->m_cycle_config == 2) {
            int i = 0;
            if (is_optimize_N_mc) {
                is_mc_N_od_at_design_local = false;
                opt_mc_N_od_f_des = x[i];
                i++;
            }
            if (is_optimize_N_rc) {
                is_rc_N_od_at_design_local = false;
                opt_rc_N_od_f_des = x[i];
                i++;
            }
            if (is_optimize_N_pc) {
                is_pc_N_od_at_design_local = false;
                opt_pc_N_od_f_des = x[i];
            }
        }
        else {
            int i = 0;
            if (is_optimize_N_mc) {
                is_mc_N_od_at_design_local = false;
                opt_mc_N_od_f_des = x[i];
                i++;
            }
            if (is_optimize_N_rc && get_design_solved()->ms_rc_cycle_solved.m_is_rc) {
                is_rc_N_od_at_design_local = false;
                opt_rc_N_od_f_des = x[i];
            }
        }

        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in__tracker;
        off_design__calc_T_pc_in__target_T_htf_cold__max_power(od_par,
                is_rc_N_od_at_design_local, opt_rc_N_od_f_des,
                is_mc_N_od_at_design_local, opt_mc_N_od_f_des,
                is_pc_N_od_at_design_local, opt_pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                od_opt_tol_in, od_tol,
                v_T_mc_in__tracker);

        return 0;

    }
    else {

        double scope_step = 0.04;   //[-]
        double opt_step = 0.01;     //[-]

        // Guess f_n_mc
        // solve (via optimizing n_rc), get eta
        int err_mc_od = 0;
        double f_N_mc = 1.0;

        double eta_rc_max, f_N_rc_opt_local, W_dot_at_rc_opt_max;
        eta_rc_max = f_N_rc_opt_local = W_dot_at_rc_opt_max = std::numeric_limits<double>::quiet_NaN();
        if (is_optimize_N_rc)
        {
            err_mc_od = optimize_N_rc__max_eta(od_par,
                false, f_N_mc,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                eta_rc_max, f_N_rc_opt_local, W_dot_at_rc_opt_max,
                -1.0, od_opt_tol_in, od_tol);
        }
        else
        {
            try
            {
                err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                    false, 1.0,
                    false, f_N_mc,
                    false, 1.0,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    od_opt_tol_in, od_tol);
            }
            catch (C_csp_exception& csp_exception)
            {
                return -1;
            }
        }

        if (err_mc_od != 0)
        {
            return err_mc_od;
        }

        double eta_f_N_mc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
        double eta_baseline = eta_f_N_mc;   //[-]

        double f_N_rc_opt, P_LP_in_opt, T_mc_in_opt, T_pc_in_opt;
        f_N_rc_opt = P_LP_in_opt = T_mc_in_opt = T_pc_in_opt = std::numeric_limits<double>::quiet_NaN();
        f_N_rc_opt = f_N_rc_opt_local;    //[-]
        P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
        T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
        T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]

        // Guess new f_n_mc = f_n_mc + scope_step
        // solve, get eta
        double f_N_mc_2 = f_N_mc + scope_step;      //[-]
        double f_N_rc_opt_local_2 = std::numeric_limits<double>::quiet_NaN();

        if (is_optimize_N_rc)
        {
            err_mc_od = optimize_N_rc__max_eta(od_par,
                false, f_N_mc_2,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                eta_rc_max, f_N_rc_opt_local_2, W_dot_at_rc_opt_max,
                f_N_rc_opt_local, od_opt_tol_in, od_tol);
        }
        else
        {
            try
            {
                err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                    false, 1.0,
                    false, f_N_mc_2,
                    false, 1.0,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    od_opt_tol_in, od_tol);
            }
            catch (C_csp_exception& csp_exception)
            {
                err_mc_od = -1;
            }
        }

        if (err_mc_od != 0)
        {
            f_N_mc_2 = f_N_mc - scope_step;
            f_N_rc_opt_local_2 = std::numeric_limits<double>::quiet_NaN();

            if (is_optimize_N_rc)
            {
                err_mc_od = optimize_N_rc__max_eta(od_par,
                    false, f_N_mc_2,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    eta_rc_max, f_N_rc_opt_local_2, W_dot_at_rc_opt_max,
                    f_N_rc_opt_local, od_opt_tol_in, od_tol);
            }
            else
            {
                try
                {
                    err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                        false, 1.0,
                        false, f_N_mc_2,
                        false, 1.0,
                        is_PHX_dP_input, PHX_f_dP,
                        off_design_strategy,
                        od_opt_tol_in, od_tol);
                }
                catch (C_csp_exception& csp_exception)
                {
                    err_mc_od = -1;
                }
            }

            if (err_mc_od != 0)
            {
                return err_mc_od;
            }
        }

        double eta_f_N_mc_2 = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

        // Based on slope, guess new f_n_mc that will result in a new max eta
        // ---- solve
        // ---- repeat until new eta < max
        double f_N_mc_opt, eta_low1, f_N_mc_low1, eta_low2, f_N_mc_low2, slope_sign;
        eta_max = f_N_mc_opt = eta_low1 = f_N_mc_low1 = eta_low2 = f_N_mc_low2 = slope_sign = std::numeric_limits<double>::quiet_NaN();

        if (eta_f_N_mc_2 > eta_f_N_mc)
        {
            eta_max = eta_f_N_mc_2;     //[-]
            f_N_mc_opt = f_N_mc_2;      //[-]
            f_N_rc_opt = f_N_rc_opt_local_2;    //[-]
            P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
            T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
            T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]

            eta_low1 = eta_f_N_mc;      //[-]
            f_N_mc_low1 = f_N_mc;       //[-]

            slope_sign = (f_N_mc_2 - f_N_mc) / fabs(f_N_mc_2 - f_N_mc); //[-]
        }
        else
        {
            eta_max = eta_f_N_mc;       //[-]
            f_N_mc_opt = f_N_mc;        //[-]        

            eta_low1 = eta_f_N_mc_2;    //[-]
            f_N_mc_low1 = f_N_mc_2;     //[-]

            slope_sign = (f_N_mc - f_N_mc_2) / fabs(f_N_mc - f_N_mc_2); //[-]
        }

        while (true)
        {
            f_N_mc = f_N_mc_opt + slope_sign * scope_step;      //[-]

            if (is_optimize_N_rc)
            {
                err_mc_od = optimize_N_rc__max_eta(od_par,
                    false, f_N_mc,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    eta_rc_max, f_N_rc_opt_local, W_dot_at_rc_opt_max,
                    f_N_rc_opt, od_opt_tol_in, od_tol);
            }
            else
            {
                try
                {
                    err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                        false, 1.0,
                        false, f_N_mc,
                        false, 1.0,
                        is_PHX_dP_input, PHX_f_dP,
                        off_design_strategy,
                        od_opt_tol_in, od_tol);
                }
                catch (C_csp_exception& csp_exception)
                {
                    err_mc_od = -1;
                }
            }

            eta_f_N_mc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

            if (err_mc_od != 0)
            {
                eta_low2 = std::numeric_limits<double>::quiet_NaN();   //[-]
                f_N_mc_low2 = f_N_mc;      //[-]
                break;
            }
            else if (eta_f_N_mc < eta_max || ms_od_solved.ms_rc_cycle_od_solved.m_mc_f_bypass > 0.0001)
            {
                eta_low2 = eta_f_N_mc;     //[-]
                f_N_mc_low2 = f_N_mc;      //[-]
                break;
            }
            else
            {
                eta_low1 = eta_max;        //[-]
                f_N_mc_low1 = f_N_mc_opt;  //[-]
                eta_max = eta_f_N_mc;      //[-]
                f_N_mc_opt = f_N_mc;       //[-]

                f_N_rc_opt = f_N_rc_opt_local;    //[-]
                P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
                T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
                T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]
            }
        }

        // Now have an eta_max with lower eta values on either side
        // --- try f_n_rc_at_eta_max + opt_step towards side with higher eta
        // ------- if results in new eta_max, keep stepping until eta decreases
        // ------- else step other way
        // ------- if neither direction results in larger eta_max, then we're already there

        if (!std::isfinite(eta_low2) || eta_low2 > eta_low1)
        {   // Move in f_N_mc_low2 direction
            slope_sign = (f_N_mc_low2 - f_N_mc_opt) / fabs(f_N_mc_low2 - f_N_mc_opt);
        }
        else
        {   // Move in f_N_mc_low1 direction
            slope_sign = (f_N_mc_low1 - f_N_mc_opt) / fabs(f_N_mc_low1 - f_N_mc_opt);
        }

        f_N_mc = f_N_mc_opt + slope_sign * opt_step;        //[-]

        if (is_optimize_N_rc)
        {
            err_mc_od = optimize_N_rc__max_eta(od_par,
                false, f_N_mc,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                eta_rc_max, f_N_rc_opt_local, W_dot_at_rc_opt_max,
                f_N_rc_opt, od_opt_tol_in, od_tol);
        }
        else
        {
            try
            {
                err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                    false, 1.0,
                    false, f_N_mc,
                    false, 1.0,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    od_opt_tol_in, od_tol);
            }
            catch (C_csp_exception& csp_exception)
            {
                err_mc_od = -1;
            }
        }

        eta_f_N_mc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

        bool is_fine_step_opt = true;
        if (err_mc_od != 0 || eta_f_N_mc < eta_max)
        {   // Try flipping sign
            slope_sign = -slope_sign;
        }
        else if (ms_od_solved.ms_rc_cycle_od_solved.m_mc_f_bypass > 0.0001) // eta_f_N_mc > eta_max but there's MC bypass
        {
            is_fine_step_opt = false;
        }
        else
        {
            eta_max = eta_f_N_mc;       //[-]
            f_N_mc_opt = f_N_mc;        //[-]

            f_N_rc_opt = f_N_rc_opt_local;    //[-]
            P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
            T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
            T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];
        }

        while (is_fine_step_opt)
        {
            f_N_mc = f_N_mc_opt + slope_sign * opt_step;    //[-]

            if (fabs(f_N_mc - f_N_mc_low1) < opt_step || fabs(f_N_mc - f_N_mc_low2) < opt_step)
            {
                break;
            }

            if (is_optimize_N_rc)
            {
                err_mc_od = optimize_N_rc__max_eta(od_par,
                    false, f_N_mc,
                    is_PHX_dP_input, PHX_f_dP,
                    off_design_strategy,
                    eta_rc_max, f_N_rc_opt_local, W_dot_at_rc_opt_max,
                    f_N_rc_opt, od_opt_tol_in, od_tol);
            }
            else
            {
                try
                {
                    err_mc_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                        false, 1.0,
                        false, f_N_mc,
                        false, 1.0,
                        is_PHX_dP_input, PHX_f_dP,
                        off_design_strategy,
                        od_opt_tol_in, od_tol);
                }
                catch (C_csp_exception& csp_exception)
                {
                    err_mc_od = -1;
                }
            }

            eta_f_N_mc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

            if (err_mc_od != 0 || eta_f_N_mc < eta_max || ms_od_solved.ms_rc_cycle_od_solved.m_mc_f_bypass > 0.0001)
            {
                break;
            }
            else
            {
                eta_max = eta_f_N_mc;       //[-]
                f_N_mc_opt = f_N_mc;        //[-]

                f_N_rc_opt = f_N_rc_opt_local;    //[-]
                P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
                T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
                T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];
            }
        }

        // Now should have eta_max at f_N_mc_opt, f_N_rc_opt, P_LP_in_opt, T_mc_in_opt, T_pc_in_opt
        // Need to solve cycle off-design with saved optimal inputs
        ms_cycle_od_par.m_is_mc_N_od_at_design = false;
        ms_cycle_od_par.m_mc_N_od_f_des = f_N_mc_opt;       //[-]

        ms_cycle_od_par.m_is_rc_N_od_at_design = !is_optimize_N_rc;     //[-]
        ms_cycle_od_par.m_rc_N_od_f_des = f_N_rc_opt;       //[-]

        ms_cycle_od_par.m_P_LP_comp_in = P_LP_in_opt;		//[kPa]
        ms_cycle_od_par.m_T_mc_in = T_mc_in_opt;			//[K]
        ms_cycle_od_par.m_T_pc_in = T_pc_in_opt;			//[K]

        ms_cycle_od_par.m_f_mc_pc_bypass = 0.0;             //[-]

        // PHX pressure drop options
        ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
        ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

        double f_od_obj = std::numeric_limits<double>::quiet_NaN();
        int od_opt_err_code = off_design_core(f_od_obj, od_tol);

        if (od_opt_err_code != 0)
        {
            throw(C_csp_exception("optimize_N_mc__max_eta::optimize_off_design at maximize efficiency parameters failed"));
        }

        double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
        int air_cooler_err_code = mpc_sco2_cycle->solve_OD_all_coolers_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan);

        if (air_cooler_err_code != 0)
        {
            throw(C_csp_exception("optimize_N_mc__max_eta::calculate_off_design_fan_power at maximize efficiency parameters failed"));
        }

        ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
        ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

        f_N_mc_opt_out = f_N_mc_opt;
        f_N_rc_opt_out = f_N_rc_opt;
        eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
        W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

        return err_mc_od;
    }
}

int C_sco2_phx_air_cooler::optimize_N_rc__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    C_sco2_phx_air_cooler::E_off_design_strategies off_design_strategy,
    double & eta_max /*-*/, double & f_N_rc_opt_out /*-*/, double & W_dot_at_eta_max /*kWe*/,
    double f_N_rc_guess,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    int err_od = 0;

    if (!get_design_solved()->ms_rc_cycle_solved.m_is_rc)
    {
        try
        {
            err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                true, 0.0,
                false, 1.0,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                od_opt_tol_in, od_tol);
        }
        catch (C_csp_exception &csp_exception)
        {
            return -1;
        }
        if (err_od != 0)
        {
            return err_od;
        }

        f_N_rc_opt_out = std::numeric_limits<double>::quiet_NaN();      //[-]
        eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;     //[-]
        W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

        return 0;
    }
    
    double scope_step = 0.04;
    double opt_step = 0.01;
    
    // Guess f_n_rc
    // solve, get eta    
    double f_N_rc = 1.0;

    if (f_N_rc_guess > 0.0)
    {
        f_N_rc = f_N_rc_guess;
    }
    else if (!is_mc_N_od_at_design)
    {   // If fractional MC shaft speed input is != 1, then want to start with RC with the same fractional speed
        f_N_rc = mc_N_od_f_des;     //[-]
    }

    try
    {
        err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
            false, f_N_rc,
            is_mc_N_od_at_design, mc_N_od_f_des,
            false, 1.0,
            is_PHX_dP_input, PHX_f_dP,
            off_design_strategy,
            od_opt_tol_in, od_tol);
    }
    catch (C_csp_exception &csp_exception)
    {
        return -1;
    }
    if (err_od != 0)
    {
        return err_od;
    }

    double eta_f_N_rc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
    double eta_baseline = eta_f_N_rc;   //[-]

    double P_LP_in_opt, T_mc_in_opt, T_pc_in_opt;
    P_LP_in_opt = T_mc_in_opt = T_pc_in_opt = std::numeric_limits<double>::quiet_NaN();
    P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
    T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
    T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]

    // Guess new f_n_rc = f_n_rc + scope_step
    // solve, get eta
    double f_N_rc_2 = f_N_rc + scope_step;      //[-]

    try
    {
        err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
            false, f_N_rc_2,
            is_mc_N_od_at_design, mc_N_od_f_des,
            false, 1.0,
            is_PHX_dP_input, PHX_f_dP,
            off_design_strategy,
            od_opt_tol_in, od_tol);
    }
    catch (C_csp_exception &csp_exception)
    {
        return -1;
    }
    if (err_od != 0)
    {
        return err_od;
    }

    double eta_f_N_rc_2 = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

    // Based on slope, guess new f_n_rc that will result in new max eta
    // ---- solve
    // ---- repeat until new eta < max
    //double sign_slope = ((eta_f_N_rc_2 - eta_f_N_rc) / fabs(eta_f_N_rc_2 - eta_f_N_rc)) / 
    //                        ((f_N_rc_2 - f_N_rc) / fabs(f_N_rc_2 - f_N_rc));
    double f_N_rc_opt, eta_low1, f_N_rc_low1, eta_low2, f_N_rc_low2, slope_sign;
    eta_max = f_N_rc_opt = eta_low1 = f_N_rc_low1 = eta_low2 = f_N_rc_low2 = slope_sign = std::numeric_limits<double>::quiet_NaN();    
    
    if (eta_f_N_rc_2 > eta_f_N_rc)
    {
        eta_max = eta_f_N_rc_2;     //[-]
        f_N_rc_opt = f_N_rc_2;      //[-]
        P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
        T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
        T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]
        
        eta_low1 = eta_f_N_rc;      //[-]
        f_N_rc_low1 = f_N_rc;       //[-]

        slope_sign = (f_N_rc_2 - f_N_rc) / fabs(f_N_rc_2 - f_N_rc);  //[-]
    }
    else if (eta_f_N_rc > eta_f_N_rc_2)
    {
        eta_max = eta_f_N_rc;       //[-]
        f_N_rc_opt = f_N_rc;        //[-]
        
        eta_low1 = eta_f_N_rc_2;    //[-]
        f_N_rc_low1 = f_N_rc_2;     //[-]

        slope_sign = (f_N_rc - f_N_rc_2) / fabs(f_N_rc - f_N_rc_2);  //[-]
    }
    else
    {
        f_N_rc_opt_out = f_N_rc;
        eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
        W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

        return 0;
    }

    while (true)
    {
        f_N_rc = f_N_rc_opt + slope_sign * scope_step;      //[-]

        try
        {
            err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                false, f_N_rc,
                is_mc_N_od_at_design, mc_N_od_f_des,
                false, 1.0,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                od_opt_tol_in, od_tol);
        }
        catch (C_csp_exception &csp_exception)
        {
            err_od = -2;
        }
        if (err_od != 0)
        {
            err_od = -1;
        }

        eta_f_N_rc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

        if (err_od != 0)
        {
            eta_low2 = std::numeric_limits<double>::quiet_NaN();  //[-]
            f_N_rc_low2 = f_N_rc;   //[-]
            break;
        }
        else if (eta_f_N_rc < eta_max)
        {
            eta_low2 = eta_f_N_rc;  //[-]
            f_N_rc_low2 = f_N_rc;   //[-]
            break;
        }
        else if (eta_max < eta_f_N_rc)
        {
            eta_low1 = eta_max;         //[-]
            f_N_rc_low1 = f_N_rc_opt;   //[-]
            eta_max = eta_f_N_rc;       //[-]
            f_N_rc_opt = f_N_rc;        //[-]
            P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
            T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
            T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]
        }
        else
        {
            f_N_rc_opt_out = f_N_rc;
            eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
            W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

            return 0;
        }
    }
    
    // Now have an eta_max with lower eta values on either side
    // --- try f_n_rc_at_eta_max + opt_step towards side with higher eta
    // ------- if results in new eta_max, keep stepping until eta decreases
    // ------- else step other way
    // ------- if neither direction results in larger eta_max, then we're already there
    if (!std::isfinite(eta_low2) || eta_low2 > eta_low1)
    {
        slope_sign = (f_N_rc_low2 - f_N_rc_opt) / fabs(f_N_rc_low2 - f_N_rc_opt);
    }
    else
    {
        slope_sign = (f_N_rc_low1 - f_N_rc_opt) / fabs(f_N_rc_low1 - f_N_rc_opt);
    }

    f_N_rc = f_N_rc_opt + slope_sign * opt_step;      //[-]

    try
    {
        err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
            false, f_N_rc,
            is_mc_N_od_at_design, mc_N_od_f_des,
            false, 1.0,
            is_PHX_dP_input, PHX_f_dP,
            off_design_strategy,
            od_opt_tol_in, od_tol);
    }
    catch (C_csp_exception &csp_exception)
    {
        err_od = -2;
    }
    if (err_od != 0)
    {
        err_od = -1;
    }

    eta_f_N_rc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

    if (err_od != 0 || eta_f_N_rc < eta_max)
    {   // Try flipping sign
        slope_sign = -slope_sign;
    }
    else if (eta_max < eta_f_N_rc)
    {
        eta_max = eta_f_N_rc;
        f_N_rc_opt = f_N_rc;
        P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
        T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
        T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]
    }
    else
    {
        f_N_rc_opt_out = f_N_rc;
        eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
        W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

        return 0;
    }

    while (true)
    {
        f_N_rc = f_N_rc_opt + slope_sign * opt_step;      //[-]

        if (fabs(f_N_rc - f_N_rc_low1) < opt_step || fabs(f_N_rc - f_N_rc_low2) < opt_step)
        {
            break;
        }

        try
        {
            err_od = off_design__constant_N__T_mc_in_P_LP_in__objective(od_par,
                false, f_N_rc,
                is_mc_N_od_at_design, mc_N_od_f_des,
                false, 1.0,
                is_PHX_dP_input, PHX_f_dP,
                off_design_strategy,
                od_opt_tol_in, od_tol);
        }
        catch (C_csp_exception &csp_exception)
        {
            err_od = -2;
        }
        if (err_od != 0)
        {
            err_od = -1;
        }

        eta_f_N_rc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]

        if (err_od != 0 || eta_f_N_rc < eta_max)
        {
            break;
        }
        else if(eta_max < eta_f_N_rc)
        {
            eta_max = eta_f_N_rc;       //[-]
            f_N_rc_opt = f_N_rc;        //[-]
            P_LP_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
            T_mc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];  //[K]
            T_pc_in_opt = ms_od_solved.ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];  //[K]
        }
        else
        {
            f_N_rc_opt_out = f_N_rc;
            eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
            W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

            return 0;
        }
    }

    // Now should have eta_max at f_N_rc_max, P_LP_in_opt, T_mc_in_opt, T_pc_in_opt
    // Need to solve cycle off-design with saved optimal inputs
    ms_cycle_od_par.m_is_rc_N_od_at_design = false;     //[-]
    ms_cycle_od_par.m_rc_N_od_f_des = f_N_rc_opt;       //[-]
    ms_cycle_od_par.m_P_LP_comp_in = P_LP_in_opt;		//[kPa]
    ms_cycle_od_par.m_T_mc_in = T_mc_in_opt;			//[K]
    ms_cycle_od_par.m_T_pc_in = T_pc_in_opt;			//[K]

    ms_cycle_od_par.m_f_mc_pc_bypass = 0.0;             //[-]

    // Input MC shaft speed controls
    ms_cycle_od_par.m_is_mc_N_od_at_design = is_mc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_mc_N_od_f_des = mc_N_od_f_des;                //[-]

    // PHX pressure drop options
    ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
    ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

    double f_od_obj = std::numeric_limits<double>::quiet_NaN();
    int od_opt_err_code = off_design_core(f_od_obj, od_tol);

    if (od_opt_err_code != 0)
    {
        throw(C_csp_exception("optimize_N_rc__max_eta::optimize_off_design at maximize efficiency parameters failed"));
    }

    double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
    int air_cooler_err_code = mpc_sco2_cycle->solve_OD_all_coolers_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan);

    if (air_cooler_err_code != 0)
    {
        throw(C_csp_exception("optimize_N_rc__max_eta::calculate_off_design_fan_power at maximize efficiency parameters failed"));
    }

    ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
    ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

    f_N_rc_opt_out = f_N_rc_opt;
    eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;   //[-]
    W_dot_at_eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_W_dot_net;  //[kWe]

    return 0;
}

int C_sco2_phx_air_cooler::solve_T_pc_in_for_cooler_constraints(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    double W_dot_pc_cooler_fan_target /*MWe*/,
    double T_pc_in_min /*K*/, C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_T_mc_in_call_tracker,
    double od_opt_tol /*-*/, double od_tol /*-*/)
{
    double T_pc_in_guess = ms_cycle_od_par.m_T_pc_in;     //[K]

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_P_LP_in__tracker;
    int T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
        T_pc_in_guess,
        is_rc_N_od_at_design, rc_N_od_f_des,
        is_mc_N_od_at_design, mc_N_od_f_des,
        is_pc_N_od_at_design, pc_N_od_f_des,
        is_PHX_dP_input, PHX_f_dP,
        v_P_LP_in__tracker,
        od_opt_tol, od_tol);
    v_T_mc_in_call_tracker.push_back(*(v_P_LP_in__tracker.end() - 1));

    if (T_mc_in_err_code != 0) {
        throw(C_csp_exception("solve_T_pc_in_for_cooler_constraints at pc guess inlet temperature failed"));
    }
    
    double W_dot_pc_cooler_fan_OD = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]

    // If pc inlet temp was at minimum possible value and fan speed is less than target, then get out
    if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && W_dot_pc_cooler_fan_OD <= W_dot_pc_cooler_fan_target) {
        return 0;
    }

    // Next, find another pc inlet temp that
    C_monotonic_eq_solver::S_xy_pair xy_1;

    // If previous guess resulted in a fan power less than target, guess a colder value
    if (W_dot_pc_cooler_fan_OD < W_dot_pc_cooler_fan_target) {

        while (W_dot_pc_cooler_fan_OD < W_dot_pc_cooler_fan_target) {

            xy_1.x = ms_cycle_od_par.m_T_pc_in;     //[K]
            xy_1.y = W_dot_pc_cooler_fan_OD;        //[MWe]

            T_pc_in_guess = std::max(xy_1.x - 1.0, T_pc_in_min);    //[K]

            ms_cycle_od_par.m_T_pc_in = T_pc_in_guess;  //[K]

            T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                T_pc_in_guess,
                is_rc_N_od_at_design, rc_N_od_f_des,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_pc_N_od_at_design, pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                v_P_LP_in__tracker,
                od_opt_tol, od_tol);
            W_dot_pc_cooler_fan_OD = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]
            v_T_mc_in_call_tracker.push_back(*(v_P_LP_in__tracker.end() - 1));

            if (T_mc_in_err_code != 0) {
                // if code reports error, we have a solution at the previous guess value that meets fan power constraints
                // so, revert back to previous guess, which we can say is T_comp_min
                ms_cycle_od_par.m_T_pc_in = xy_1.x;     //[K]

                // then solve downstream code and get out
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in_guess,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker,
                    od_opt_tol, od_tol);
                W_dot_pc_cooler_fan_OD = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]
                v_T_mc_in_call_tracker.push_back(*(v_P_LP_in__tracker.end() - 1));

                return 0;
            }

            // If we've decreased the pc inlet temp to the minimum, then get out
            if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min) {
                return 0;
            }
        }
    }
    else {
        // If previous guess resulted in a fan power greater than target, guess a warmer value
        xy_1.x = ms_cycle_od_par.m_T_pc_in;     //[K]
        xy_1.y = W_dot_pc_cooler_fan_OD;        //[MWe]

        T_pc_in_guess = xy_1.x + 1.0;       //[K]

        ms_cycle_od_par.m_T_pc_in = T_pc_in_guess;  //[K]

        T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
            T_pc_in_guess,
            is_rc_N_od_at_design, rc_N_od_f_des,
            is_mc_N_od_at_design, mc_N_od_f_des,
            is_pc_N_od_at_design, pc_N_od_f_des,
            is_PHX_dP_input, PHX_f_dP,
            v_P_LP_in__tracker,
            od_opt_tol, od_tol);
        W_dot_pc_cooler_fan_OD = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]
        v_T_mc_in_call_tracker.push_back(*(v_P_LP_in__tracker.end() - 1));

        // If error, then have a problem because we don't have a solution that meets fan power constraint
        if (T_mc_in_err_code != 0) {
            throw(C_csp_exception("solve_T_pc_in_for_cooler_constraints failed to find a pc inlet temp that solved while meeting pc air cooler constraints"));
        }
    }

    // Check if most recent call to downstream code was at T_pc_min and fan power was less than constraint
    if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && W_dot_pc_cooler_fan_OD < W_dot_pc_cooler_fan_target) {
        return 0;
    }

    // At this point, should have a 2nd set of T_pc_in and W_dot_pc_fan_OD
    C_monotonic_eq_solver::S_xy_pair xy_2;
    xy_2.x = ms_cycle_od_par.m_T_pc_in;         //[K]
    xy_2.y = W_dot_pc_cooler_fan_OD;            //[MWe]

    // Solve for the pc inlet temperature that results in the target pc fan target
    C_MEQ_T_pc_in__W_dot_fan__T_mc_in_opt c_W_dot_pc_fan(this, od_par,
                                    is_rc_N_od_at_design, rc_N_od_f_des,
                                    is_mc_N_od_at_design, mc_N_od_f_des,
                                    is_pc_N_od_at_design, pc_N_od_f_des,
                                    is_PHX_dP_input, PHX_f_dP,
                                    od_opt_tol, od_tol,
                                    &v_T_mc_in_call_tracker);

    C_monotonic_eq_solver c_W_dot_pc_fan_solver(c_W_dot_pc_fan);

    c_W_dot_pc_fan_solver.settings(od_tol, 50, T_pc_in_min, ms_od_par.m_T_amb + 45.0, true);

    double T_pc_in_solved = std::numeric_limits<double>::quiet_NaN();
    double W_dot_pc_fan_tol_solved = std::numeric_limits<double>::quiet_NaN();
    int W_dot_fan_iter = -1;

    int W_dot_fan_error_code = c_W_dot_pc_fan_solver.solve(xy_1, xy_2, W_dot_pc_cooler_fan_target, T_pc_in_solved, W_dot_pc_fan_tol_solved, W_dot_fan_iter);

    if (W_dot_fan_error_code != C_monotonic_eq_solver::CONVERGED) {

        if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan < W_dot_pc_cooler_fan_target) {
            return 0;
        }

        // Not actually checking convergence tolerance...
        if (W_dot_fan_error_code < C_monotonic_eq_solver::CONVERGED) {
            throw(C_csp_exception("solve_T_pc_in_for_cooler_constraints MEQ on T_pc_in returned an error"));
        }
    }

    return 0;
}

void C_sco2_phx_air_cooler::solve_T_mc_in_for_cooler_constraint(double W_dot_mc_cooler_fan_target /*MWe*/,
                            double T_comp_in_min /*K*/, C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
                            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_call_tracker,
                            double od_tol /*-*/)
{
    int opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_call_tracker, od_tol);

    if (opt_P_LP_err != 0)
    {
        throw(C_csp_exception("Off-design at main compressor guess inlet temperature failed"));
    }

    // First, check the fan power
    double W_dot_fan_local = std::numeric_limits<double>::quiet_NaN();    //[MWe]
    double P_cooler_out = std::numeric_limits<double>::quiet_NaN();
    if (mpc_sco2_cycle->solve_OD_mc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
    {
        throw(C_csp_exception("Off design main compressor air cooler model failed"));
    }

    if (ms_cycle_od_par.m_T_mc_in == T_comp_in_min && W_dot_fan_local < W_dot_mc_cooler_fan_target)
    {
        return;
    }

    C_monotonic_eq_solver::S_xy_pair xy_1;

    double T_mc_in_guess = std::numeric_limits<double>::quiet_NaN();
    // Guess another inlet temperature
    if (W_dot_fan_local < W_dot_mc_cooler_fan_target)
    {   // Need more fan power, guess closer to ambient temperature
        while (W_dot_fan_local < W_dot_mc_cooler_fan_target)
        {
            xy_1.x = ms_cycle_od_par.m_T_mc_in;     //[K]
            xy_1.y = W_dot_fan_local;               //[MWe]

            T_mc_in_guess = std::max(xy_1.x - 1.0, T_comp_in_min);     //[K]

            ms_cycle_od_par.m_T_mc_in = T_mc_in_guess;  //[K]

            opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_call_tracker, od_tol);

            if (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO || opt_P_LP_err != 0)
            {   // revert back to previous guess and set T_comp_min
                ms_cycle_od_par.m_T_mc_in = xy_1.x;  //[K]

                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_call_tracker, od_tol);

                T_comp_in_min = xy_1.x;     //[K]
            }

            /*if (opt_P_LP_err != 0)
            {
                throw(C_csp_exception("Off-design at main compressor guess inlet temperature failed"));
            }*/

            if (mpc_sco2_cycle->solve_OD_mc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
            {
                throw(C_csp_exception("Off design main compressor air cooler model failed"));
            }

            if (ms_cycle_od_par.m_T_mc_in == T_comp_in_min)
            {
                break;
            }
        }
    }
    else
    {   // Need less fan power, guess hotter than 1st guess
        xy_1.x = ms_cycle_od_par.m_T_mc_in;     //[K]
        xy_1.y = W_dot_fan_local;               //[MWe]

        T_mc_in_guess = xy_1.x + 1.0;       //[K]

        ms_cycle_od_par.m_T_mc_in = T_mc_in_guess;  //[K]
        //ms_cycle_od_par.m_T_pc_in = T_mc_in_guess;  //[K]

        opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_call_tracker, od_tol);

        if (opt_P_LP_err != 0)
        {
            throw(C_csp_exception("Off-design at main compressor guess inlet temperature failed"));
        }

        if (mpc_sco2_cycle->solve_OD_mc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
        {
            throw(C_csp_exception("Off design main compressor air cooler model failed"));
        }

        if (ms_cycle_od_par.m_T_mc_in == T_comp_in_min)
        {
            return;
        }
    }

    if (ms_cycle_od_par.m_T_mc_in == T_comp_in_min && W_dot_fan_local < W_dot_mc_cooler_fan_target)
    {
        return;
    }

    C_monotonic_eq_solver::S_xy_pair xy_2;
    xy_2.x = ms_cycle_od_par.m_T_mc_in;     //[K]
    xy_2.y = W_dot_fan_local;               //[MWe]

    // Solve for inlet temperature that results in target fan power
    C_MEQ_T_mc_in__W_dot_fan c_W_dot_fan_eq(this, od_opt_objective, &v_call_tracker, od_tol);
    C_monotonic_eq_solver c_W_dot_fan_solver(c_W_dot_fan_eq);

    c_W_dot_fan_solver.settings(od_tol*10, 50, T_comp_in_min, ms_od_par.m_T_amb + 45, true);

    double T_LP_in_solved = std::numeric_limits<double>::quiet_NaN();
    double W_dot_fan_tol_solved = std::numeric_limits<double>::quiet_NaN();
    int W_dot_fan_iter = -1;
    int W_dot_fan_err_code = c_W_dot_fan_solver.solve(xy_1, xy_2, W_dot_mc_cooler_fan_target, T_LP_in_solved, W_dot_fan_tol_solved, W_dot_fan_iter);

    if (W_dot_fan_err_code != C_monotonic_eq_solver::CONVERGED)
    {
        if (ms_cycle_od_par.m_T_mc_in == T_comp_in_min && mpc_sco2_cycle->get_od_solved()->ms_mc_air_cooler_od_solved.m_W_dot_fan < W_dot_mc_cooler_fan_target)
        {
            return;
        }
        /*if (W_dot_fan_err_code > C_monotonic_eq_solver::CONVERGED && fabs(W_dot_fan_tol_solved) < 0.1)
        {
            return;
        }*/
        if (W_dot_fan_err_code < C_monotonic_eq_solver::CONVERGED)
        {
            throw(C_csp_exception("Iteration on main compressor inlet temp to achieve target fan power failed"));
        }
    }

}

void C_sco2_phx_air_cooler::solve_nested_T_pc_in__T_mc_in_for_cooler_constrains(double W_dot_pc_cooler_fan_target /*MWe*/,
    double W_dot_mc_cooler_fan_target_in /*MWe*/,
    double T_comp_in_min_in /*K*/,
    C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_call_tracker,
    double od_tol /*-*/)
{
    double T_pc_in_min = T_comp_in_min_in;  //[K]
    
    // First, generate solution at T_pc_in_min
    try
    {
        solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target_in,
            T_comp_in_min_in, od_opt_objective, v_call_tracker, od_tol);
    }
    catch (C_csp_exception)
    {
        throw(C_csp_exception("solve_nested_T_pc_in__T_mc_in_for_cooler_constrains failed"));
    }
    
    // Then check the fan power at first guess
    double W_dot_fan_local = std::numeric_limits<double>::quiet_NaN();    //[MWe]
    double P_cooler_out = std::numeric_limits<double>::quiet_NaN();     //[kPa]
    if (mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
    {
        throw(C_csp_exception("Off design air cooler model failed"));
    }

    if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && W_dot_fan_local < W_dot_pc_cooler_fan_target)
    {
        return;
    }

    C_monotonic_eq_solver::S_xy_pair xy_1;

    double T_pc_in_guess = std::numeric_limits<double>::quiet_NaN();
    // Guess another inlet temperature
    if (W_dot_fan_local < W_dot_pc_cooler_fan_target)
    {   // Need more fan power, guess closer to ambient temperature
        while (W_dot_fan_local < W_dot_pc_cooler_fan_target)
        {
            xy_1.x = ms_cycle_od_par.m_T_pc_in;     //[K]
            xy_1.y = W_dot_fan_local;               //[MWe]

            T_pc_in_guess = std::max(xy_1.x - 1.0, T_pc_in_min);     //[K]

            ms_cycle_od_par.m_T_pc_in = T_pc_in_guess;  //[K]

            // Don't really have a way for nested T_mc_in iteration to
            // relay that there's a problem with T_pc_in...
            // e.g. opt_P_LP_err == E_TIP_RATIO in the nested code...
            try
            {
                solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target_in,
                    T_comp_in_min_in, od_opt_objective, v_call_tracker, od_tol);
            }
            catch (C_csp_exception)
            {
                throw(C_csp_exception("solve_nested_T_pc_in__T_mc_in_for_cooler_constrains failed"));
            }

            //if (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
            //{   // revert back to previous guess and set T_comp_min
            //    ms_cycle_od_par.m_T_pc_in = xy_1.x;  //[K]

            //    if (is_modified_P_mc_in_solver)
            //    {
            //        opt_P_LP_err = solve_P_LP_in__target_W_dot();
            //    }
            //    else
            //    {
            //        opt_P_LP_err = opt_P_LP_comp_in__fixed_N_turbo();
            //    }

            //    T_pc_in_min = xy_1.x;     //[K]
            //}

            /*if (opt_P_LP_err != 0)
            {
                throw(C_csp_exception("Off-design at inlet temperature failed"));
            }*/

            if (mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
            {
                throw(C_csp_exception("Off design PC air cooler model failed"));
            }

            if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min)
            {
                break;
            }
        }
    }
    else
    {   // Need less fan power, guess hotter than 1st guess
        xy_1.x = ms_cycle_od_par.m_T_pc_in;     //[K]
        xy_1.y = W_dot_fan_local;               //[MWe]

        T_pc_in_guess = xy_1.x + 1.0;       //[K]

        ms_cycle_od_par.m_T_pc_in = T_pc_in_guess;  //[K]

        // Don't really have a way for nested T_mc_in iteration to
        // relay that there's a problem T_pc_in...
        // e.g. opt_P_LP_err check
        try
        {
            solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target_in,
                T_comp_in_min_in, od_opt_objective, v_call_tracker, od_tol);
        }        
        catch (C_csp_exception)
        {
            throw(C_csp_exception("solve_nested_T_pc_in__T_mc_in_for_cooler_constrains failed"));
        }

        //if (opt_P_LP_err != 0)
        //{
        //    throw(C_csp_exception("Off-design at inlet temperature failed"));
        //}

        if (mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan_local, P_cooler_out) != 0)
        {
            throw(C_csp_exception("Off design PC air cooler model failed"));
        }

        if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min)
        {
            return;
        }
    }

    if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && W_dot_fan_local < W_dot_pc_cooler_fan_target)
    {
        return;
    }

    C_monotonic_eq_solver::S_xy_pair xy_2;
    xy_2.x = ms_cycle_od_par.m_T_pc_in;     //[K]
    xy_2.y = W_dot_fan_local;               //[MWe]

    // Solve for inlet temperature that results in target fan power
    C_MEQ_T_pc_in__W_dot_fan c_W_dot_pc_fan_eq(this, W_dot_mc_cooler_fan_target_in,
                            T_comp_in_min_in, od_opt_objective, &v_call_tracker, od_tol);
    C_monotonic_eq_solver c_W_dot_fan_solver(c_W_dot_pc_fan_eq);

    c_W_dot_fan_solver.settings(0.01, 50, T_pc_in_min, ms_od_par.m_T_amb + 45, true);

    double T_LP_in_solved = std::numeric_limits<double>::quiet_NaN();
    double W_dot_fan_tol_solved = std::numeric_limits<double>::quiet_NaN();
    int W_dot_fan_iter = -1;
    int W_dot_fan_err_code = c_W_dot_fan_solver.solve(xy_1, xy_2, W_dot_pc_cooler_fan_target, T_LP_in_solved, W_dot_fan_tol_solved, W_dot_fan_iter);

    if (W_dot_fan_err_code != C_monotonic_eq_solver::CONVERGED)
    {
        if (ms_cycle_od_par.m_T_pc_in == T_pc_in_min && mpc_sco2_cycle->get_od_solved()->ms_mc_air_cooler_od_solved.m_W_dot_fan < W_dot_pc_cooler_fan_target)
        {
            return;
        }
        /*if (W_dot_fan_err_code > C_monotonic_eq_solver::CONVERGED && fabs(W_dot_fan_tol_solved) < 0.1)
        {
            return;
        }
        throw(C_csp_exception("Iteration on compressor inlet temp to achieve target fan power failed"));*/
        if (W_dot_fan_err_code < C_monotonic_eq_solver::CONVERGED)
        {
            throw(C_csp_exception("Iteration on main compressor inlet temp to achieve target fan power failed"));
        }
    }

    return;
}

int C_sco2_phx_air_cooler::C_MEQ_T_pc_in__W_dot_fan::operator()(double T_pc_in /*K*/, double *W_dot_fan /*MWe*/)
{
    mpc_sco2_ac->ms_cycle_od_par.m_T_pc_in = T_pc_in;   //[K]

    try
    {
        mpc_sco2_ac->solve_T_mc_in_for_cooler_constraint(m_W_dot_mc_cooler_fan_target,
            m_T_mc_in_min, m_od_opt_objective, *m_call_tracker, m_od_tol);
    }
    catch (C_csp_exception)
    {
        *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }

    *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
    double P_cooler_out = std::numeric_limits<double>::quiet_NaN();
    if (mpc_sco2_ac->mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(mpc_sco2_ac->ms_od_par.m_T_amb, m_od_tol, *W_dot_fan, P_cooler_out) != 0)
    {
        *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    return 0;
}

int C_sco2_phx_air_cooler::C_MEQ_T_mc_in__W_dot_fan::operator()(double T_mc_in /*K*/, double *W_dot_fan /*MWe*/)
{
    mpc_sco2_ac->ms_cycle_od_par.m_T_mc_in = T_mc_in;  //[K]

    int opt_P_LP_err = mpc_sco2_ac->solve_P_LP_in__objective(m_od_opt_objective, *m_call_tracker, m_od_tol);

    if (opt_P_LP_err != 0)
    {
        *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }

    *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
    double P_cooler_out = std::numeric_limits<double>::quiet_NaN();
    if (mpc_sco2_ac->mpc_sco2_cycle->solve_OD_mc_cooler_fan_power(mpc_sco2_ac->ms_od_par.m_T_amb, m_od_tol, *W_dot_fan, P_cooler_out) != 0)
    {
        *W_dot_fan = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    return 0;
}

int C_sco2_phx_air_cooler::C_MEQ_T_pc_in__W_dot_fan__T_mc_in_opt::operator()(double T_pc_in /*K*/, double* W_dot_pc_fan /*MWe*/)
{
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_P_LP_in__tracker;

    int T_mc_in_err_code = mpc_sco2_ac->off_design__calc_T_mc_in__target_T_htf_cold__max_power(m_od_par,
        T_pc_in,
        m_is_rc_N_od_at_design, m_rc_N_od_f_des,
        m_is_mc_N_od_at_design, m_mc_N_od_f_des,
        m_is_pc_N_od_at_design, m_pc_N_od_f_des,
        m_is_PHX_dP_input, m_PHX_f_dP,
        v_P_LP_in__tracker,
        m_od_opt_tol, m_od_tol);
    mv_T_mc_in_call_tracker->push_back(*(v_P_LP_in__tracker.end() - 1));

    *W_dot_pc_fan = mpc_sco2_ac->mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]

    return T_mc_in_err_code;
}

int C_sco2_phx_air_cooler::off_design_fix_T_mc_in__P_mc_in_solve_for_target(C_sco2_phx_air_cooler::S_od_par od_par,
    double T_mc_in_input /*K*/,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    // This sets: T_mc_in, T_pc_in, etc.
    setup_off_design_info(od_par);

    // Input RC shaft speed controls
    ms_cycle_od_par.m_is_rc_N_od_at_design = is_rc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_rc_N_od_f_des = rc_N_od_f_des;                //[-]

    // Input MC shaft speed controls
    ms_cycle_od_par.m_is_mc_N_od_at_design = is_mc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_mc_N_od_f_des = mc_N_od_f_des;                //[-]

    // PHX pressure drop options
    ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
    ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

    int cycle_config = get_design_par()->m_cycle_config;    //[-]

    double T_comp_in_min_amb = ms_od_par.m_T_amb + 0.5;  //[K]

    double T_mc_in = std::max(T_mc_in_input, T_comp_in_min_amb);    //[K]

    if (m_is_T_crit_limit)
    {
        T_mc_in = std::max(m_T_mc_in_min, T_mc_in);    //[K]
    }

    ms_cycle_od_par.m_T_mc_in = T_mc_in;    //[K]
    ms_cycle_od_par.m_T_pc_in = T_mc_in;    //[K]

    bool is_modified_P_mc_in_solver = true;

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in__tracker;

    int opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);

    if (opt_P_LP_err != 0 && opt_P_LP_err != -31 && opt_P_LP_err != C_sco2_phx_air_cooler::E_TIP_RATIO)
    {
        return opt_P_LP_err;
    }

    if (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
    {   // Incrementally increase compressor inlet temperature to try to avoid violating tip speed constraint
        return opt_P_LP_err;
    }

    if (opt_P_LP_err == -31)
    {
        while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
        {
            while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
            {
                ms_cycle_od_par.m_f_mc_pc_bypass += 0.01;

                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);

                if (opt_P_LP_err != 0 && opt_P_LP_err != -31)
                {
                    return opt_P_LP_err;
                }
            }
            if (opt_P_LP_err != 0)
                return opt_P_LP_err;
        }
    }
    
    return 0;
}

int C_sco2_phx_air_cooler::off_design__calc_T_pc_in__target_T_htf_cold__max_power(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    double od_opt_tol_in /*-*/, double od_tol /*-*/,
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_T_mc_in__tracker)
{
    E_off_design_strategies od_opt_objective = E_TARGET_T_HTF_COLD_POWER_MAX;

    // This sets: T_mc_in, T_pc_in, etc.
    setup_off_design_info(od_par);

    double T_pc_in = ms_cycle_od_par.m_T_pc_in;

    //std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in__tracker;
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_P_LP_in__tracker_in;
    int T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
        T_pc_in,
        is_rc_N_od_at_design, rc_N_od_f_des,
        is_mc_N_od_at_design, mc_N_od_f_des,
        is_pc_N_od_at_design, pc_N_od_f_des,
        is_PHX_dP_input, PHX_f_dP,
        v_P_LP_in__tracker_in,
        od_opt_tol_in, od_tol);
    v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

    if (T_mc_in_err_code != 0) {
        throw(C_csp_exception("off_design__calc_T_pc_in__target_T_htf_cold__max_power failed 1"));
    }

    if (ms_des_par.m_cycle_config != 2)
    {
        return T_mc_in_err_code;
    }

    double T_comp_in_min = ms_od_par.m_T_amb + 0.5;  //[K]

    if (m_is_T_crit_limit)
    {
        T_comp_in_min = std::max(m_T_mc_in_min, T_comp_in_min);    //[K]
    }

    double W_dot_pc_cooler_fan_des = get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_W_dot_fan;	    //[MWe]

    double W_dot_pc_cooler_fan_OD = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]

    T_mc_in_err_code = solve_T_pc_in_for_cooler_constraints(od_par,
                        is_rc_N_od_at_design, rc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_pc_N_od_at_design, pc_N_od_f_des,
                        is_PHX_dP_input, PHX_f_dP,
                        W_dot_pc_cooler_fan_des, T_comp_in_min,
                        od_opt_objective, v_T_mc_in__tracker,
                        od_opt_tol_in, od_tol);

    if (T_mc_in_err_code != 0) {
        throw(C_csp_exception("off_design__calc_T_pc_in__target_T_htf_cold__max_power failed 2"));
    }

    // v_T_mc_in__tracker should be populated from 1st call and 'solve_T_pc_in_for_cooler_constraints'
    // Should have cycle solution that
    // 1) Does not report an error code
    // 2) Has both pc and mc fan power = respective design powers
    // 2.a) In this nest, have a T_pc_in_min

    // the fan power iteration potentially 'wastes' some iterations that could be used to check T_HTF_cold and max power
    // so there will be ways to streamline this code for that case


    // First, check all the solutions from the fan power iterations
    // Copy tracker to new vector and sort ascending by T_pc_in
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in_tracker__T_pc_in_sorted = v_T_mc_in__tracker;
    std::sort(v_T_mc_in_tracker__T_pc_in_sorted.begin(), v_T_mc_in_tracker__T_pc_in_sorted.end(), SortByTpcin);

    // Erase any duplicates. Ideally don't want duplicates, but that's dependent on 'solve_T_pc_in_for_cooler_constraints' being smarter
    std::vector<std::vector<S_solve_P_LP_in__tracker>::iterator> v_T_pc_in_duplicates;
    if (v_T_mc_in_tracker__T_pc_in_sorted.size() > 1) {
        for (std::vector<S_solve_P_LP_in__tracker>::iterator it = v_T_mc_in_tracker__T_pc_in_sorted.begin() + 1; it < v_T_mc_in_tracker__T_pc_in_sorted.end(); it++) {
            if ((*(it - 1)).m_T_pc_in == (*it).m_T_pc_in) {
                v_T_pc_in_duplicates.push_back(it);
            }
        }
    }
    for (size_t i = 0; i < v_T_pc_in_duplicates.size(); i++) {
        v_T_mc_in_tracker__T_pc_in_sorted.erase(v_T_pc_in_duplicates[v_T_pc_in_duplicates.size() - 1 - i]);
    }

    // Find iterator in sorted vector that represents the T_pc_in corresponding to the max fan power
    double T_pc_in_cooler_target = get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN];     //[K]
    std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_cooler_target = v_T_mc_in_tracker__T_pc_in_sorted.begin();

    while (it_sorted_cooler_target != v_T_mc_in_tracker__T_pc_in_sorted.end()) {
        if ((*it_sorted_cooler_target).m_T_pc_in == T_pc_in_cooler_target) {
            break;
        }
        it_sorted_cooler_target++;
    }
    double i_rel_diff_T_htf_cold_at_cooler_target = (*it_sorted_cooler_target).m_rel_diff_T_htf_cold;   //[K]

    // Erase values at T_pc_in < T_pc_in_cooler_target
    v_T_mc_in_tracker__T_pc_in_sorted.erase(v_T_mc_in_tracker__T_pc_in_sorted.begin(), it_sorted_cooler_target);
    // And reset iterator to beginning
    it_sorted_cooler_target = v_T_mc_in_tracker__T_pc_in_sorted.begin();

    // Check whether HTF cold temperature is within tolerance of target
    double tol_W_dot_pc_fan = od_tol * 2.0;     //[-]
    bool is_T_htf_equal_target = std::fabs(i_rel_diff_T_htf_cold_at_cooler_target) < tol_W_dot_pc_fan;

    // Start search at max fan power T_pc_in, which is cold limit
    std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_W_dot_net_less_cooling_min__T_cool = it_sorted_cooler_target;

    // If at the cooler fan power target the HTF temperature is not equal to the target
    // ... then only option is to try a warmer pc inlet temperature and check whether calculated value is getting closer
    while (!is_T_htf_equal_target) {

        // Is there another T_pc_in in the solution vector where T_htf_cold is equal to the target?
        std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_T_htf_cold_target = it_sorted_cooler_target;
        while (it_sorted_T_htf_cold_target < v_T_mc_in_tracker__T_pc_in_sorted.end()) {
            if (std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold) < tol_W_dot_pc_fan &&
                (*it_sorted_T_htf_cold_target).m_error_code == 0) {
                break;
            }
            it_sorted_T_htf_cold_target++;
        }

        // If a T_pc_in in the solution vector has T_htf_cold equal to target, get out of while loop
        // Will try below to maximize cycle power output
        if (it_sorted_T_htf_cold_target < v_T_mc_in_tracker__T_pc_in_sorted.end()) {
            it_sorted_W_dot_net_less_cooling_min__T_cool = it_sorted_cooler_target;
            break;
        }

        // If no solution in vector has T_htf_cold equal to target, check for min difference
        double T_htf_cold_diff_abs_min = std::fabs(i_rel_diff_T_htf_cold_at_cooler_target);
        it_sorted_T_htf_cold_target = it_sorted_cooler_target;

        std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_rel_diff_T_htf_min = it_sorted_cooler_target;
        while (it_sorted_T_htf_cold_target < v_T_mc_in_tracker__T_pc_in_sorted.end()) {

            if (std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold) < T_htf_cold_diff_abs_min
                && (*it_sorted_T_htf_cold_target).m_error_code == 0) {

                T_htf_cold_diff_abs_min = std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold);
                it_sorted_rel_diff_T_htf_min = it_sorted_T_htf_cold_target;
            }
            it_sorted_T_htf_cold_target++;
        }

        // Min HTF difference occured at warmest temperature, so keep trying warmer temps
        if (it_sorted_rel_diff_T_htf_min == v_T_mc_in_tracker__T_pc_in_sorted.end() - 1) {
            while (true) {
                T_pc_in = (*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_T_pc_in + 0.5;     //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));
                v_T_mc_in_tracker__T_pc_in_sorted.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                if ((*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_error_code != 0 ||
                    std::fabs((*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) > T_htf_cold_diff_abs_min + std::fmax(0.01, tol_W_dot_pc_fan)) {
                    // If most recent T_pc_in guess resulted in error code or a greater absolute difference in T_htf_cold target, then revert and get out
                    T_pc_in -= 0.5;
                    v_P_LP_in__tracker_in.resize(0);
                    T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                        T_pc_in,
                        is_rc_N_od_at_design, rc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_pc_N_od_at_design, pc_N_od_f_des,
                        is_PHX_dP_input, PHX_f_dP,
                        v_P_LP_in__tracker_in,
                        od_opt_tol_in, od_tol);

                    v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                    return T_mc_in_err_code;
                }
                else if(std::fabs((*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) > T_htf_cold_diff_abs_min)
                {   // Otherwise, set new minimum HTF cold temperature difference
                    T_htf_cold_diff_abs_min = std::fabs((*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold);    //[-]
                }

                // Check if HTF cold temp is within tolerance of target. If so, get out.
                if (std::fabs((*(v_T_mc_in_tracker__T_pc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) < tol_W_dot_pc_fan) {
                    it_sorted_W_dot_net_less_cooling_min__T_cool = v_T_mc_in_tracker__T_pc_in_sorted.end() - 1;
                    is_T_htf_equal_target = true;
                    break;
                }
            }
        }
        // Otherwise, if min HTF difference occured at most recent off designs solution, get out
        else if ((*it_sorted_rel_diff_T_htf_min).m_T_pc_in == get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::PC_IN]) {
            return 0;
        }
        // Otherwise, resolve cycle at T_pc_in corresponding to min HTF cold difference
        else {
            T_pc_in = (*it_sorted_rel_diff_T_htf_min).m_T_pc_in;        //[K]
            v_P_LP_in__tracker_in.resize(0);
            T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                T_pc_in,
                is_rc_N_od_at_design, rc_N_od_f_des,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_pc_N_od_at_design, pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                v_P_LP_in__tracker_in,
                od_opt_tol_in, od_tol);

            v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

            return T_mc_in_err_code;
        }
    }

    // T_htf_cold at it_sorted_W_dot_net_less_cooling_min__T_cool is within tolerance of target
    // Fan power is at or less than limit
    // no error code

    // Step through stored solutions, try to bracket max net power
    S_solve_P_LP_in__tracker odsol_T_cool = *it_sorted_W_dot_net_less_cooling_min__T_cool;
    S_solve_P_LP_in__tracker odsol_T_warm = *it_sorted_W_dot_net_less_cooling_min__T_cool;
    S_solve_P_LP_in__tracker odsol_max = *it_sorted_W_dot_net_less_cooling_min__T_cool;

    bool found_constraint_violation = false;
    S_solve_P_LP_in__tracker odsol_constraint_violation;

    if (v_T_mc_in_tracker__T_pc_in_sorted.size() > 1) {

        for (std::vector<S_solve_P_LP_in__tracker>::iterator it = it_sorted_W_dot_net_less_cooling_min__T_cool + 1; it < v_T_mc_in_tracker__T_pc_in_sorted.end(); it++) {

            if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_pc_fan) {
                found_constraint_violation = true;
                odsol_constraint_violation = *it;
                break;
            }
            if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                odsol_T_cool = odsol_max;
                odsol_max = *it;
                odsol_T_warm = odsol_max;
            }
            else {
                odsol_T_warm = *it;
                break;
            }
        }
    }

    // Stored solutions do not differentiate between T_pc_in at min T_pc_in and T_pc_in at max net power (so far) and warmest T_pc_in tried
    // So need to figure out why
    if (odsol_T_cool.m_T_pc_in == odsol_max.m_T_pc_in && odsol_max.m_T_pc_in == odsol_T_warm.m_T_pc_in) {
        // 1) if no constraint found, then need to try warmer T_pc_in values
        if (!found_constraint_violation) {
            while (true) {
                T_pc_in = odsol_max.m_T_pc_in + 1.0;        //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker_in.end() - 1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_pc_fan) {
                    // If solution violates constraints, get out
                    found_constraint_violation = true;
                    odsol_constraint_violation = *it;
                    break;
                }
                if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else
                {
                    odsol_T_warm = *it;
                    break;
                }
                if (ms_cycle_od_par.m_T_pc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::PC_IN] + 30.0) {
                    return T_mc_in_err_code;
                }
            }
        }
        // If constraint violation during sorting or the last if(!found_constraint_violation)
        if (found_constraint_violation) {
            // Check distance between T_pc_in at constraint violation and T_pc_in at 'T_warm' point
            // If some distance, check point in between for a feasible solution
            while (odsol_constraint_violation.m_T_pc_in - odsol_T_warm.m_T_pc_in > 0.1) {

                T_pc_in = 0.5 * (odsol_constraint_violation.m_T_pc_in + odsol_T_warm.m_T_pc_in);        //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker_in.end() - 1;

                // Track whether most recent solution violates constraints, results in a new max power, or is less than previous max power
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_pc_fan) {
                    odsol_constraint_violation = *it;
                }
                else if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else
                {
                    odsol_T_warm = *it;
                    break;
                }
            }

            // if no solution at a T_pc_in > T_pc_in_at_max_power, then solve and get out
            if (odsol_max.m_T_pc_in == odsol_T_warm.m_T_pc_in) {
                // and if T_pc_in at odsol_max isn't the most recent solution
                if (ms_cycle_od_par.m_T_pc_in != odsol_max.m_T_pc_in) {
                    T_pc_in = odsol_T_warm.m_T_pc_in;   //[K]
                    v_P_LP_in__tracker_in.resize(0);
                    T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                        T_pc_in,
                        is_rc_N_od_at_design, rc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_pc_N_od_at_design, pc_N_od_f_des,
                        is_PHX_dP_input, PHX_f_dP,
                        v_P_LP_in__tracker_in,
                        od_opt_tol_in, od_tol);

                    v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));
                    return T_mc_in_err_code;
                }
                else {
                    return 0;
                }
            }
        }
    }

    if (odsol_T_cool.m_T_pc_in != odsol_max.m_T_pc_in && odsol_max.m_T_pc_in == odsol_T_warm.m_T_pc_in) {

        // Maximum net power greater than power at T_mc_in_cooler and occurs at warmest T_mc_in in tracker
        // If no constraint found in prior calcs, then need to try warmer T_pc_in values
        if (!found_constraint_violation) {
            while (true) {
                T_pc_in = odsol_max.m_T_pc_in + 1.0;  //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker_in.end() - 1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_pc_fan) {
                    found_constraint_violation = true;
                    odsol_constraint_violation = *it;
                    break;
                }
                if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else {
                    odsol_T_warm = *it;
                    break;
                }
                if (ms_cycle_od_par.m_T_pc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::PC_IN] + 30.0) {
                    return T_mc_in_err_code;
                }
            }
        }

        // If constraint violation during sorting or the last if(!found_constraint_violation)
        if (found_constraint_violation) {
            // Check distance between T_pc_in at constraint violation and T_pc_in at 'T_warm' point
            // If some distance, check point in between for a feasible solution
            while (odsol_constraint_violation.m_T_pc_in - odsol_T_warm.m_T_pc_in > 0.1) {

                T_pc_in = 0.5 * (odsol_constraint_violation.m_T_pc_in + odsol_T_warm.m_T_pc_in);        //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker_in.end() - 1;

                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_pc_fan) {
                    odsol_constraint_violation = *it;
                }
                else if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else {
                    odsol_T_warm = *it;
                    break;
                }
            }

            // if no solution at a T_pc_in > T_pc_in_at_max_power, then solve and get out
            if (odsol_max.m_T_pc_in == odsol_T_warm.m_T_pc_in) {
                // and if T_pc_in at odsol_max isn't the most recent solution
                if (ms_cycle_od_par.m_T_pc_in != odsol_max.m_T_pc_in) {
                    T_pc_in = odsol_T_warm.m_T_pc_in;   //[K]
                    v_P_LP_in__tracker_in.resize(0);
                    T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                        T_pc_in,
                        is_rc_N_od_at_design, rc_N_od_f_des,
                        is_mc_N_od_at_design, mc_N_od_f_des,
                        is_pc_N_od_at_design, pc_N_od_f_des,
                        is_PHX_dP_input, PHX_f_dP,
                        v_P_LP_in__tracker_in,
                        od_opt_tol_in, od_tol);

                    v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));

                    return T_mc_in_err_code;
                }
                else {
                    return 0;
                }
            }
        }

    }

    double T_pc_in_opt_tol = 0.1;   //[K]

    std::vector<S_solve_P_LP_in__tracker> v_T_mc_in__opt__call_tracker;
    // Use 'cool', 'max', 'warm' T_pc_in brackets to guide T_pc_in optimization
    if ((odsol_T_cool.m_T_pc_in == odsol_max.m_T_pc_in && odsol_max.m_T_pc_in != odsol_T_warm.m_T_pc_in) ||
        (odsol_T_cool.m_T_pc_in != odsol_max.m_T_pc_in && odsol_max.m_T_pc_in != odsol_T_warm.m_T_pc_in)) {

        if (odsol_T_warm.m_T_pc_in - odsol_T_cool.m_T_pc_in > T_pc_in_opt_tol) {

            C_sco2_phx_air_cooler::S_to_off_design__calc_T_mc_in s_opt_inputs;
            s_opt_inputs.mpc_sco2_phx_air_cooler = this;
            s_opt_inputs.od_par = od_par;
            s_opt_inputs.is_rc_N_od_at_design = is_rc_N_od_at_design;
            s_opt_inputs.rc_N_od_f_des = rc_N_od_f_des;
            s_opt_inputs.is_mc_N_od_at_design = is_mc_N_od_at_design;
            s_opt_inputs.pc_N_od_f_des = pc_N_od_f_des;
            s_opt_inputs.is_pc_N_od_at_design = is_pc_N_od_at_design;
            s_opt_inputs.mc_N_od_f_des = mc_N_od_f_des;
            s_opt_inputs.is_PHX_dP_input = is_PHX_dP_input;
            s_opt_inputs.PHX_f_dP = PHX_f_dP;
            s_opt_inputs.od_opt_tol = od_opt_tol_in;
            s_opt_inputs.od_tol = od_tol;
            s_opt_inputs.pv_T_mc_in_call_tracker = &v_T_mc_in__opt__call_tracker;

            std::vector<double> x(1);
            x[0] = odsol_max.m_T_pc_in;     //[K]
            std::vector<double> lb(1);
            lb[0] = odsol_T_cool.m_T_pc_in; //[K]
            std::vector<double> ub(1);
            ub[0] = odsol_T_warm.m_T_pc_in + 3.0; //[K]
            std::vector<double> scale(1);
            scale[0] = 0.75 * (odsol_T_cool.m_T_pc_in - odsol_T_warm.m_T_pc_in);    //[K]
            scale[0] = 0.9 * (odsol_T_warm.m_T_pc_in - x[0]);

            nlopt::opt opt_N__max_power(nlopt::LN_SBPLX, 1);
            opt_N__max_power.set_lower_bounds(lb);
            opt_N__max_power.set_upper_bounds(ub);
            opt_N__max_power.set_initial_step(scale);
            opt_N__max_power.set_xtol_abs(T_pc_in_opt_tol);
            opt_N__max_power.set_ftol_rel(0.0005);

            try
            {
                opt_N__max_power.set_max_objective(nlopt_opt_T_pc_in__max_net_power_less_cooling, &s_opt_inputs);
                double max_obj = std::numeric_limits<double>::quiet_NaN();
                nlopt::result  result_opt_N = opt_N__max_power.optimize(x, max_obj);

                for (std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>::iterator it = s_opt_inputs.pv_T_mc_in_call_tracker->begin();
                    it < s_opt_inputs.pv_T_mc_in_call_tracker->end(); it++) {
                    v_T_mc_in__tracker.push_back(*it);
                }

                double T_pc_in_opt = x[0];      //[K]

                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in_opt,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));
                return T_mc_in_err_code;

            }
            catch (const std::exception&)
            {

            }
            catch (C_csp_exception&)
            {
                return -1;
            }
            catch (...)
            {
                return -1;
            }
        }
        else {
            if (ms_cycle_od_par.m_T_pc_in != odsol_max.m_T_pc_in) {
                T_pc_in = odsol_T_warm.m_T_pc_in;   //[K]
                v_P_LP_in__tracker_in.resize(0);
                T_mc_in_err_code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                    T_pc_in,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_pc_N_od_at_design, pc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    v_P_LP_in__tracker_in,
                    od_opt_tol_in, od_tol);

                v_T_mc_in__tracker.push_back(*(v_P_LP_in__tracker_in.end() - 1));
                return T_mc_in_err_code;
            }
            else {
                return 0;
            }
        }
    }

    return 0;
}


int C_sco2_phx_air_cooler::off_design__calc_T_mc_in__target_T_htf_cold__max_power(C_sco2_phx_air_cooler::S_od_par od_par,
    double T_pc_in /*K*/,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_P_LP_in__tracker,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    E_off_design_strategies od_opt_objective = E_TARGET_T_HTF_COLD_POWER_MAX;

    // This sets: T_mc_in, T_pc_in, etc.
    setup_off_design_info(od_par);
    ms_cycle_od_par.m_T_pc_in = T_pc_in;        //[K]

    // Input RC shaft speed controls
    ms_cycle_od_par.m_is_rc_N_od_at_design = is_rc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_rc_N_od_f_des = rc_N_od_f_des;                //[-]

    // Input MC shaft speed controls
    ms_cycle_od_par.m_is_mc_N_od_at_design = is_mc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_mc_N_od_f_des = mc_N_od_f_des;                //[-]

    // Input PC shaft speed controls
    ms_cycle_od_par.m_is_pc_N_od_at_design = is_pc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_pc_N_od_f_des = pc_N_od_f_des;                //[-]

    // PHX pressure drop options
    ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
    ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

    int cycle_config = get_design_par()->m_cycle_config;    //[-]

    double T_comp_in_min = ms_od_par.m_T_amb + 0.5;  //[K]

    if (m_is_T_crit_limit)
    {
        T_comp_in_min = std::max(m_T_mc_in_min, T_comp_in_min);    //[K]
    }

    double W_dot_mc_cooler_fan_des = get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_W_dot_fan;	    //[MWe]

    int opt_P_LP_err = 0;

    opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);

    if (opt_P_LP_err != 0 && opt_P_LP_err != -31 && opt_P_LP_err != C_sco2_phx_air_cooler::E_TIP_RATIO)
    {
        throw(C_csp_exception("solve_P_LP_in__objective failed 1"));
    }

    if (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
    {   // Incrementally increase compressor inlet temperature to try to avoid violating tip speed constraint
        while (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
        {
            // Increase compressor inlet temperatures by constant interval
            ms_cycle_od_par.m_T_mc_in += 0.5;	//[K]

            if (ms_cycle_od_par.m_T_mc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] + 10.0)
            {
                break;
            }

            opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
        }
        if (opt_P_LP_err != 0)
        {	// 
            throw(C_csp_exception("off design optimization, fixed shaft speed config, failed with tip speed constraint"));
        }
        T_comp_in_min = ms_cycle_od_par.m_T_mc_in;     //[K]
    }

    double bypass_step = 0.01;
    if (ms_des_par.m_cycle_config == 2) {
        bypass_step = 0.05;
    }
    if (opt_P_LP_err == -31)
    {
        while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
        {
            while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
            {
                ms_cycle_od_par.m_f_mc_pc_bypass += bypass_step;

                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);

                if (opt_P_LP_err != 0 && opt_P_LP_err != -31)
                {
                    throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
                }
            }
            if (opt_P_LP_err != 0)
                throw(C_csp_exception("off design optimization, fixed shaft speed config, failed"));

            try
            {
                solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_des, T_comp_in_min,
                    od_opt_objective, v_P_LP_in__tracker, od_tol);
            }
            catch (C_csp_exception)
            {
                return -1;
            }
        }
        if (opt_P_LP_err != 0)
        {
            throw(C_csp_exception("off design iteration on compressor bypass failed"));
        }
    }
    else
    {
        try
        {
            solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_des, T_comp_in_min,
                od_opt_objective, v_P_LP_in__tracker, od_tol);
        }
        catch (C_csp_exception)
        {
            return -1;
        }
    }


    // Should have cycle solution that
    // 1) Does not report an error code
    // 2) Has fan power = max fan power
    // 2.a) So we have a T_mc_in_min

    // the fan power iteration potentially 'wastes' some iterations that could be used to check T_HTF_cold and max power
    // so there will be ways to streamline this code for that case

    // We can check all the solutions from the fan power (or error code) iterations

    // Copy tracker to new vector and sort ascending by T_mc_in
    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in_tracker_T_mc_in_sorted = v_P_LP_in__tracker;
    std::sort(P_LP_in_tracker_T_mc_in_sorted.begin(), P_LP_in_tracker_T_mc_in_sorted.end(), SortByTmcin);

    // Erase any duplicates. Ideally don't want duplicates, but that's dependent on code above being smarter
    std::vector<std::vector<S_solve_P_LP_in__tracker>::iterator> T_mc_in_duplicates;
    if (P_LP_in_tracker_T_mc_in_sorted.size() > 1) {
        for (std::vector<S_solve_P_LP_in__tracker>::iterator it = P_LP_in_tracker_T_mc_in_sorted.begin()+1; it < P_LP_in_tracker_T_mc_in_sorted.end(); it++){ 
            if((*(it-1)).m_T_mc_in == (*it).m_T_mc_in){ 
                T_mc_in_duplicates.push_back(it);
            }
        }
    }
    for (size_t i = 0; i < T_mc_in_duplicates.size(); i++) {
        P_LP_in_tracker_T_mc_in_sorted.erase(T_mc_in_duplicates[T_mc_in_duplicates.size() - 1 - i]);
    }

    // Find iterator in sorted vector that represents the T_mc_in corresponding to the max fan power
    double T_mc_in_cooler_target = get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN];     //[K]
    std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_cooler_target = P_LP_in_tracker_T_mc_in_sorted.begin();

    while (it_sorted_cooler_target != P_LP_in_tracker_T_mc_in_sorted.end()) {
        if ((*it_sorted_cooler_target).m_T_mc_in == T_mc_in_cooler_target) {
            break;
        }
        it_sorted_cooler_target++;
    }
    double i_rel_diff_T_htf_cold_at_cooler_target = (*it_sorted_cooler_target).m_rel_diff_T_htf_cold;   //[K]

    // Erase values at T_mc_in < T_mc_in_cooler_target
    P_LP_in_tracker_T_mc_in_sorted.erase(P_LP_in_tracker_T_mc_in_sorted.begin(), it_sorted_cooler_target);
    // And reset iterator to beginning
    it_sorted_cooler_target = P_LP_in_tracker_T_mc_in_sorted.begin();

    // Check whether HTF cold temperature is within tolerance of target
    double tol_W_dot_fan = od_tol * 2.0;        // 0.002;
    bool is_T_htf_equal_target = std::fabs(i_rel_diff_T_htf_cold_at_cooler_target) < tol_W_dot_fan;

    // Start searching at max fan power T_mc_in, which is cold limit
    std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_W_dot_net_less_cooling_min__T_cool = it_sorted_cooler_target;

    // If at the cooler fan power target the HTF temperature is not equal to the target
    // ... then only option is to try a warmer mc inlet temperature and check whether calculated value is getting closer
    while (!is_T_htf_equal_target){

        // Is there another T_mc_in in the solution vector where T htf cold is equal to target?
        std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_T_htf_cold_target = it_sorted_cooler_target;
        while (it_sorted_T_htf_cold_target < P_LP_in_tracker_T_mc_in_sorted.end()) {
            if (std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold) < tol_W_dot_fan && (*it_sorted_T_htf_cold_target).m_error_code == 0) {
                break;
            }
            it_sorted_T_htf_cold_target++;
        }

        // If a T_mc_in in the solution vector has T htf cold equal to target, get out of while loop
        // Can try below to maximize cycle power output
        if (it_sorted_T_htf_cold_target < P_LP_in_tracker_T_mc_in_sorted.end()) {
            it_sorted_W_dot_net_less_cooling_min__T_cool = it_sorted_T_htf_cold_target;
            break;
        }

        // If no solution in vector has T htf cold equal to target, check for min difference
        double T_htf_cold_diff_abs_min = std::fabs(i_rel_diff_T_htf_cold_at_cooler_target);
        it_sorted_T_htf_cold_target = it_sorted_cooler_target;

        std::vector<S_solve_P_LP_in__tracker>::iterator it_sorted_rel_diff_T_htf_min = it_sorted_cooler_target;
        while (it_sorted_T_htf_cold_target < P_LP_in_tracker_T_mc_in_sorted.end()) {

            if (std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold) < T_htf_cold_diff_abs_min
                    && (*it_sorted_T_htf_cold_target).m_error_code == 0) {

                T_htf_cold_diff_abs_min = std::fabs((*it_sorted_T_htf_cold_target).m_rel_diff_T_htf_cold);
                it_sorted_rel_diff_T_htf_min = it_sorted_T_htf_cold_target;
            }
            it_sorted_T_htf_cold_target++;
        }

        // Min HTF difference occured at warmest temperature, so keep trying warmer temps
        if (it_sorted_rel_diff_T_htf_min == P_LP_in_tracker_T_mc_in_sorted.end() - 1) {
            while (true) {
                ms_cycle_od_par.m_T_mc_in = (*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_T_mc_in + 0.5;  //[K]
                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in_tracker_T_mc_in_sorted, od_tol);
                if ((*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_error_code != 0 ||
                    std::fabs((*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) > T_htf_cold_diff_abs_min + std::fmax(0.01, tol_W_dot_fan)) {
                    ms_cycle_od_par.m_T_mc_in -= 0.5;
                    return solve_P_LP_in__objective(od_opt_objective, P_LP_in_tracker_T_mc_in_sorted, od_tol);
                }
                else if (std::fabs((*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) < T_htf_cold_diff_abs_min) {
                    T_htf_cold_diff_abs_min = std::fabs((*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold);
                }
                if (std::fabs((*(P_LP_in_tracker_T_mc_in_sorted.end() - 1)).m_rel_diff_T_htf_cold) < tol_W_dot_fan) {
                    it_sorted_W_dot_net_less_cooling_min__T_cool = P_LP_in_tracker_T_mc_in_sorted.end() - 1;
                    is_T_htf_equal_target = true;
                    break;
                }
            }
        }
        // If the min HTF difference was not at the warmest inlet temp tried but at the *last inlet temp tried, then get out
        else if ((*it_sorted_rel_diff_T_htf_min).m_T_mc_in = get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::MC_IN]){ 
            return 0;
        }
        // Else, rerun simulation with inlet temp corresponding to min HTF temperature difference and get out
        else{
            ms_cycle_od_par.m_T_mc_in = (*it_sorted_rel_diff_T_htf_min).m_T_mc_in;  //[K]
            return solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
        }

    }

    // T_htf_cold at *cooler fan target*?? is equal to target
    // Fan power is at limit
    // no error code

    S_solve_P_LP_in__tracker odsol_T_cool = *it_sorted_W_dot_net_less_cooling_min__T_cool;
    S_solve_P_LP_in__tracker odsol_T_warm = *it_sorted_W_dot_net_less_cooling_min__T_cool;
    S_solve_P_LP_in__tracker odsol_max = *it_sorted_W_dot_net_less_cooling_min__T_cool;

    bool found_constraint_violation = false;
    S_solve_P_LP_in__tracker odsol_constraint_violation;

    if (P_LP_in_tracker_T_mc_in_sorted.size() > 1) {
        // Start checking warmer T_mc_in
        // Expect fan power to stay within limit, but no guarantee that T_htf_cold will remain equal to target or error code will be 0
        for (std::vector<S_solve_P_LP_in__tracker>::iterator it = it_sorted_W_dot_net_less_cooling_min__T_cool + 1; it < P_LP_in_tracker_T_mc_in_sorted.end(); it++) {
            if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_fan)
            {
                found_constraint_violation = true;
                odsol_constraint_violation = *it;
                break;
            }
            if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {     // (*it_sorted_W_dot_net_less_cooling_max).m_W_dot_net_less_cooling) {
                odsol_T_cool = odsol_max;
                odsol_max = *it;
                odsol_T_warm = odsol_max;
            }
            else
            {
                odsol_T_warm = *it;
                break;
            }
        }
    }

    // So we have three structure and potentially a structure containing the first (coolest) constraint violation (either T_HTF or error_code)
    if(odsol_T_cool.m_T_mc_in == odsol_max.m_T_mc_in && odsol_max.m_T_mc_in == odsol_T_warm.m_T_mc_in){
        // Everything at T_mc_in
        // Was T_mc_in at max power the warmest T_mc_in in the tracker?
        if (!found_constraint_violation) {
            while (true) {
                ms_cycle_od_par.m_T_mc_in = odsol_max.m_T_mc_in + 1.0;  //[K]
                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker.end() - 1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_fan){
                    found_constraint_violation = true;
                    odsol_constraint_violation = *it;
                    break;
                }
                if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {     
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else{
                    odsol_T_warm = *it;
                    break;
                }
                if (ms_cycle_od_par.m_T_mc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] + 30.0){
                    return opt_P_LP_err;
                }
            }
        }

        // Was there a warmer point that violated constraints?
        if (found_constraint_violation) {
            // Check distance between T_mc_in at constraint violation and T_mc_in at 'T_warm' point
            while (odsol_constraint_violation.m_T_mc_in - odsol_T_warm.m_T_mc_in > 0.1) {
                ms_cycle_od_par.m_T_mc_in = 0.5*(odsol_constraint_violation.m_T_mc_in + odsol_T_warm.m_T_mc_in);  //[K]
                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker.end() - 1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_fan){
                    odsol_constraint_violation = *it;
                }
                else if((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else
                {
                    odsol_T_warm = *it;
                    break;
                }
            }

            if (odsol_max.m_T_mc_in == odsol_T_warm.m_T_mc_in) {
                ms_cycle_od_par.m_T_mc_in = odsol_T_warm.m_T_mc_in;  //[K]
                return solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
            }
        }

        // Or was T_mc_in the warmest point we tried?
        // If so, it may be possible to find the max and __T_warm point and then move to a following 'if/then'
    }
    
    if (odsol_T_cool.m_T_mc_in != odsol_max.m_T_mc_in && odsol_max.m_T_mc_in == odsol_T_warm.m_T_mc_in) {
        // Maximum net power greater than power at T_mc_in_cooler and occurs at warmest T_mc_in in tracker
        // Was T_mc_in at max power the warmest T_mc_in in the tracker?
        if (!found_constraint_violation) {
            while(true) {
                ms_cycle_od_par.m_T_mc_in = odsol_max.m_T_mc_in + 1.0;  //[K]
                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker.end()-1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_fan){
                    found_constraint_violation = true;
                    odsol_constraint_violation = *it;
                    break;
                }
                if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling){     // (*it_sorted_W_dot_net_less_cooling_max).m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else{
                    odsol_T_warm = *it;
                    break;
                }
                if (ms_cycle_od_par.m_T_mc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] + 30.0){
                    return opt_P_LP_err;
                }
            } 
        }

        // Maximum net power greater than power at T_mc_in_cooler and occurs at warmest T_mc_in in tracker
        // Was there a warmer point that violated constraints?
        if (found_constraint_violation) {
            // Check distance between T_mc_in at constraint violation and T_mc_in at 'T_warm' point
            while (odsol_constraint_violation.m_T_mc_in - odsol_T_warm.m_T_mc_in > 0.1) {
                ms_cycle_od_par.m_T_mc_in = 0.5 * (odsol_constraint_violation.m_T_mc_in + odsol_T_warm.m_T_mc_in);  //[K]
                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
                std::vector<S_solve_P_LP_in__tracker>::iterator it = v_P_LP_in__tracker.end() - 1;
                if ((*it).m_error_code != 0 || fabs((*it).m_rel_diff_T_htf_cold) > tol_W_dot_fan) {
                    odsol_constraint_violation = *it;
                }
                else if ((*it).m_W_dot_net_less_cooling >= odsol_max.m_W_dot_net_less_cooling) {
                    odsol_T_cool = odsol_max;
                    odsol_max = *it;
                    odsol_T_warm = odsol_max;
                }
                else
                {
                    odsol_T_warm = *it;
                    break;
                }
            }

            if (odsol_max.m_T_mc_in == odsol_T_warm.m_T_mc_in) {
                ms_cycle_od_par.m_T_mc_in = odsol_T_warm.m_T_mc_in;  //[K]
                return solve_P_LP_in__objective(od_opt_objective, v_P_LP_in__tracker, od_tol);
            }
        }
    }

    if ( (odsol_T_cool.m_T_mc_in == odsol_max.m_T_mc_in && odsol_max.m_T_mc_in != odsol_T_warm.m_T_mc_in) ||
        (odsol_T_cool.m_T_mc_in != odsol_max.m_T_mc_in && odsol_max.m_T_mc_in != odsol_T_warm.m_T_mc_in) ) {
        // Have bounded a maximum net power less cooling between two lower values, iterate/solve for max
        C_sco2_phx_air_cooler::S_to_W_net_less_cooling_max fmin_inputs;
        fmin_inputs.mpc_sco2_phx_air_cooler = this;
        fmin_inputs.m_call_tracker = &v_P_LP_in__tracker;
        fmin_inputs.m_od_opt_obj = od_opt_objective;
        fmin_inputs.od_tol = od_tol;

        double T_mc_in_warm__margin = 3;

        double T_mc_in_opt = fminbr(odsol_T_cool.m_T_mc_in, odsol_T_warm.m_T_mc_in + T_mc_in_warm__margin,
                            &fmin_opt_T_mc_in__max_net_power_less_cooling, &fmin_inputs, 0.1);
    }
    else{
        throw(C_csp_exception("off_design__target_T_htf_cold__max_power optimization failed"));
    }

    return 0;
}

int C_sco2_phx_air_cooler::off_design__constant_N__calc_max_htf_massflow__T_mc_in_P_LP_in__objective(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    E_off_design_strategies od_opt_objective,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    if (od_opt_objective == E_TARGET_T_HTF_COLD_POWER_MAX)
    {
        double m_dot_htf_guess = 1.0;   //[-]
        double T_htf_cold_tol = od_tol * 2.0;       // 0.002; //[-]

        od_par.m_m_dot_htf = m_dot_htf_guess * ms_phx_des_par.m_m_dot_hot_des;    //[kg/s]

        // For input htf mass flow rate, solve cycle for T htf cold target constraint and max power target
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in__tracker;
        int code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
            std::numeric_limits<double>::quiet_NaN(),
            is_rc_N_od_at_design, rc_N_od_f_des,
            is_mc_N_od_at_design, mc_N_od_f_des,
            is_pc_N_od_at_design, pc_N_od_f_des,
            is_PHX_dP_input, PHX_f_dP,
            P_LP_in__tracker,
            od_opt_tol_in, od_tol);

        double T_htf_cold_reldiff = (mc_phx.ms_od_solved.m_T_h_out - ms_des_solved.ms_phx_des_solved.m_T_h_out) / ms_des_solved.ms_phx_des_solved.m_T_h_out;

        double m_dot_htf_target_bound = std::numeric_limits<double>::quiet_NaN();
        double m_dot_htf_miss_bound = std::numeric_limits<double>::quiet_NaN();
        double step_sign = std::numeric_limits<double>::quiet_NaN();
        if (std::fabs(T_htf_cold_reldiff) < T_htf_cold_tol) {
            m_dot_htf_target_bound = m_dot_htf_guess;
            step_sign = 1.0;
        }
        else {
            m_dot_htf_miss_bound = m_dot_htf_guess;
            step_sign = -1.0;
        }

        double m_dot_htf_step = 0.08;

        while (true) {

            m_dot_htf_guess += step_sign * m_dot_htf_step;

            od_par.m_m_dot_htf = m_dot_htf_guess * ms_phx_des_par.m_m_dot_hot_des;    //[kg/s]

            // For input htf mass flow rate, solve cycle for T htf cold target constraint and max power target
            P_LP_in__tracker.resize(0);
            int code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                std::numeric_limits<double>::quiet_NaN(),
                is_rc_N_od_at_design, rc_N_od_f_des,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_pc_N_od_at_design, pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                P_LP_in__tracker,
                od_opt_tol_in, od_tol);

            T_htf_cold_reldiff = (mc_phx.ms_od_solved.m_T_h_out - ms_des_solved.ms_phx_des_solved.m_T_h_out) / ms_des_solved.ms_phx_des_solved.m_T_h_out;

            if (std::fabs(T_htf_cold_reldiff) < T_htf_cold_tol) {
                m_dot_htf_target_bound = m_dot_htf_guess;
                if (step_sign == -1.0) {
                    break;
                }
            }
            else {
                m_dot_htf_miss_bound = m_dot_htf_guess;
                if (step_sign == 1.0) {
                    break;
                }
            }
        }

        while (m_dot_htf_miss_bound - m_dot_htf_target_bound > 0.011) {

            m_dot_htf_guess = 0.5 * (m_dot_htf_miss_bound + m_dot_htf_target_bound);

            od_par.m_m_dot_htf = m_dot_htf_guess * ms_phx_des_par.m_m_dot_hot_des;    //[kg/s]

            // For input htf mass flow rate, solve cycle for T htf cold target constraint and max power target
            P_LP_in__tracker.resize(0);
            int code = off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
                std::numeric_limits<double>::quiet_NaN(),
                is_rc_N_od_at_design, rc_N_od_f_des,
                is_mc_N_od_at_design, mc_N_od_f_des,
                is_pc_N_od_at_design, pc_N_od_f_des,
                is_PHX_dP_input, PHX_f_dP,
                P_LP_in__tracker,
                od_opt_tol_in, od_tol);

            T_htf_cold_reldiff = (mc_phx.ms_od_solved.m_T_h_out - ms_des_solved.ms_phx_des_solved.m_T_h_out) / ms_des_solved.ms_phx_des_solved.m_T_h_out;

            if (std::fabs(T_htf_cold_reldiff) < T_htf_cold_tol) {
                m_dot_htf_target_bound = m_dot_htf_guess;
            }
            else {
                m_dot_htf_miss_bound = m_dot_htf_guess;
            }
        }

        od_par.m_m_dot_htf = m_dot_htf_target_bound * ms_phx_des_par.m_m_dot_hot_des;    //[kg/s]

        // For input htf mass flow rate, solve cycle for T htf cold target constraint and max power target
        P_LP_in__tracker.resize(0);
        return off_design__calc_T_mc_in__target_T_htf_cold__max_power(od_par,
            std::numeric_limits<double>::quiet_NaN(),
            is_rc_N_od_at_design, rc_N_od_f_des,
            is_mc_N_od_at_design, mc_N_od_f_des,
            is_pc_N_od_at_design, pc_N_od_f_des,
            is_PHX_dP_input, PHX_f_dP,
            P_LP_in__tracker,
            od_opt_tol_in, od_tol);
    }
    else
    {
        throw(C_csp_exception("off_design__constant_N__calc_max_htf_massflow__T_mc_in_P_LP_in__objective only recognized"
        " for E_TARGET_T_HTF_COLD_POWER_MAX off design strategy"));
    }
}

int C_sco2_phx_air_cooler::off_design__constant_N__T_mc_in_P_LP_in__objective(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
    double od_opt_tol_in /*-*/, double od_tol /*-*/)
{
    if (od_opt_objective == E_TARGET_POWER_ETA_MAX)
    {
        return off_design__target_power__max_eta(od_par,
                    is_rc_N_od_at_design, rc_N_od_f_des,
                    is_mc_N_od_at_design, mc_N_od_f_des,
                    is_PHX_dP_input, PHX_f_dP,
                    od_opt_tol_in, od_tol);
    }
    else if (od_opt_objective == E_TARGET_T_HTF_COLD_POWER_MAX)
    {
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in__tracker;

        return off_design__calc_T_pc_in__target_T_htf_cold__max_power(od_par,
            is_rc_N_od_at_design, rc_N_od_f_des,
            is_mc_N_od_at_design, mc_N_od_f_des,
            is_pc_N_od_at_design, pc_N_od_f_des,
            is_PHX_dP_input, PHX_f_dP,
            od_opt_tol_in, od_tol,
            v_T_mc_in__tracker);
    }
    else
    {
        throw(C_csp_exception("Off design cycle operation strategy not recognized"));
    }
}

int C_sco2_phx_air_cooler::off_design__target_power__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
    bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
    bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
    bool is_PHX_dP_input, double PHX_f_dP /*-*/,
    double od_opt_tol_in, double od_tol /*-*/)
{
    E_off_design_strategies od_opt_objective = E_TARGET_POWER_ETA_MAX;

    // This sets: T_mc_in, T_pc_in, etc.
	setup_off_design_info(od_par);

    // Input RC shaft speed controls
    ms_cycle_od_par.m_is_rc_N_od_at_design = is_rc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_rc_N_od_f_des = rc_N_od_f_des;                //[-]

    // Input MC shaft speed controls
    ms_cycle_od_par.m_is_mc_N_od_at_design = is_mc_N_od_at_design;  //[-]
    ms_cycle_od_par.m_mc_N_od_f_des = mc_N_od_f_des;                //[-]

    // PHX pressure drop options
    ms_cycle_od_par.m_is_PHX_dP_input = is_PHX_dP_input;    //[-]
    ms_cycle_od_par.m_PHX_f_dP = PHX_f_dP;                  //[-]

    int cycle_config = get_design_par()->m_cycle_config;    //[-]

    bool is_mc_cooler_fan_limit = true;

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in__tracker;

    double T_comp_in_min = ms_od_par.m_T_amb + 0.5;  //[K]
        
    if (m_is_T_crit_limit)
    {
        T_comp_in_min = std::max(m_T_mc_in_min, T_comp_in_min);    //[K]
    }

    double W_dot_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * ms_des_par.m_W_dot_net;	//[kWe]

    double W_dot_mc_cooler_fan_des = get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_W_dot_fan;	    //[MWe]

    double W_dot_pc_cooler_fan_des = 0.0;
    if (cycle_config == 2)
    {
        W_dot_pc_cooler_fan_des = get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_W_dot_fan;	    //[MWe]
    }

    double W_dot_mc_cooler_fan_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * W_dot_mc_cooler_fan_des; //[MWe]
    double W_dot_pc_cooler_fan_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * W_dot_pc_cooler_fan_des; //[MWe]

    bool is_modified_P_mc_in_solver = true;

    int opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);

    if (opt_P_LP_err != 0 && opt_P_LP_err != -31 && opt_P_LP_err != C_sco2_phx_air_cooler::E_TIP_RATIO)
	{
		throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
	}

    if (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
    {   // Incrementally increase compressor inlet temperature to try to avoid violating tip speed constraint
        while (opt_P_LP_err == C_sco2_phx_air_cooler::E_TIP_RATIO)
        {
            // Increase compressor inlet temperatures by constant interval
            ms_cycle_od_par.m_T_mc_in += 0.5;	//[K]
            ms_cycle_od_par.m_T_pc_in += 0.5;	//[K]

            if (ms_cycle_od_par.m_T_mc_in > get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] + 10.0)
            {
                break;
            }

            opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);
        }
        if (opt_P_LP_err != 0)
        {	// 
            throw(C_csp_exception("off design optimization, fixed shaft speed config, failed with tip speed constraint"));
        }
        T_comp_in_min = ms_cycle_od_par.m_T_mc_in;     //[K]
    }

    if (opt_P_LP_err == -31)
    {
        while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
        {
            while (opt_P_LP_err != 0 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
            {
                ms_cycle_od_par.m_f_mc_pc_bypass += 0.01;

                opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);

                if (opt_P_LP_err != 0 && opt_P_LP_err != -31)
                {
                    throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
                }
            }
            if (opt_P_LP_err != 0)
                throw(C_csp_exception("off design optimization, fixed shaft speed config, failed"));

            if (is_mc_cooler_fan_limit && cycle_config == 2)
            {
                try
                {
                    solve_nested_T_pc_in__T_mc_in_for_cooler_constrains(W_dot_pc_cooler_fan_target,
                        W_dot_mc_cooler_fan_target, T_comp_in_min, od_opt_objective, P_LP_in__tracker, od_tol);
                }
                catch (C_csp_exception)
                {
                    opt_P_LP_err = -1;
                }
            }
            else if (is_mc_cooler_fan_limit)
            {
                try
                {
                    solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target, T_comp_in_min, od_opt_objective, P_LP_in__tracker, od_tol);
                }
                catch (C_csp_exception)
                {
                    return -1;
                }
            }
        }
        if (opt_P_LP_err != 0)
        {
            throw(C_csp_exception("off design iteration on compressor bypass failed"));
        }

    }
	else
	{
        bool is_iterate_for_power_and_eta = true;

        if (is_mc_cooler_fan_limit && cycle_config == 2)
        {
            solve_nested_T_pc_in__T_mc_in_for_cooler_constrains(W_dot_pc_cooler_fan_target,
                            W_dot_mc_cooler_fan_target, T_comp_in_min, od_opt_objective, P_LP_in__tracker, od_tol);
        }
        else if (is_mc_cooler_fan_limit)
        {
            try
            {
                solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target, T_comp_in_min, od_opt_objective, P_LP_in__tracker, od_tol);
            }
            catch (C_csp_exception)
            {
                return -1;
            }
        } 
            			
        if (is_iterate_for_power_and_eta)
        {
            // For the E_FIXED_MC_FIXED_RC_FIXED_T configuration, at off design ambient temperatures colder than design
            // ... the cycle may not be able to reach the target power and/or the cycle efficiency may improve if compressor inlet temperatures increase
            // Increase compressor inlet temperatures
            // Track power output between steps
            // If target power is achieved, store max efficiency case. Otherwise, track maximum power case
            // Limit to target power?

            double T_pc_in_opt_global = ms_cycle_od_par.m_T_pc_in;     //[K]

            double W_dot_opt_global = std::min(W_dot_target, mpc_sco2_cycle->get_od_solved()->m_W_dot_net);	//[kWe]

            double eta_max_global = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

            double P_LP_in_opt_global = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

            double T_mc_in_opt_global = ms_cycle_od_par.m_T_mc_in;		//[K]

            try
            {
                check_increasing_T_mc_in(W_dot_target, W_dot_pc_cooler_fan_target*1.001,
                    is_modified_P_mc_in_solver,
                    od_opt_objective,
                    W_dot_opt_global, eta_max_global,
                    P_LP_in_opt_global, T_mc_in_opt_global, od_tol);
            }
            catch (C_csp_exception)
            {
                return -1;
            }

            while (cycle_config == 2)
            {
                ms_cycle_od_par.m_T_pc_in += 0.5;   //[K]

                // find ms_cycle_od_par.m_T_mc_in that satisfies cooler requirement
                try
                {
                    solve_T_mc_in_for_cooler_constraint(W_dot_mc_cooler_fan_target, T_comp_in_min, od_opt_objective, P_LP_in__tracker, od_tol);
                }
                catch (C_csp_exception)
                {
                    break;
                }

                double W_dot_opt_mc = std::numeric_limits<double>::quiet_NaN(); 	//[kWe]

                double eta_max_mc = std::numeric_limits<double>::quiet_NaN();	    //[-]

                double P_LP_in_opt_mc = std::numeric_limits<double>::quiet_NaN();	//[kPa]

                double T_mc_in_opt_mc = std::numeric_limits<double>::quiet_NaN();	//[K]

                try
                {
                    opt_P_LP_err = check_increasing_T_mc_in(W_dot_target, W_dot_pc_cooler_fan_target*1.001,
                        is_modified_P_mc_in_solver,
                        od_opt_objective,
                        W_dot_opt_mc, eta_max_mc,
                        P_LP_in_opt_mc, T_mc_in_opt_mc, od_tol);
                }
                catch (C_csp_exception)
                {
                    break;
                }
                    

                if (opt_P_LP_err != 0)
                {	// If off-design breaks, we've solved at colder temperatures, so don't crash the entire simulation
                    // just exit loop that increases temperature
                    // This can happen especially w/ the partial cooling cycle when bypass is not required initially, but becomes necessary in this loop
                    break;
                }
                    
                // If current inlet temperatures generate more power than optimal, reset optimal
                // This relies on 'opt_P_LP_comp_in__fixed_N_turbo()' not return a power that is over-target...
                // '0.002' seems reasonable based on parametric runs but may be suboptimal
                if ((W_dot_opt_mc - W_dot_opt_global) / W_dot_opt_global > 0.002)
                {
                    eta_max_global = eta_max_mc;	//[-]
                    
                    P_LP_in_opt_global = P_LP_in_opt_mc;		//[kPa]
                    
                    T_mc_in_opt_global = T_mc_in_opt_mc;		//[K]
                    
                    T_pc_in_opt_global = ms_cycle_od_par.m_T_pc_in;     //[K]
                    
                    W_dot_opt_global = W_dot_opt_mc;		//[kWe]
                }
                    
                // Otherwise, if the current inlet temperature generates very close to the current optimal power
                // then check if the efficiency is higher. If so, save efficiency
                else if ((W_dot_opt_mc - W_dot_opt_global) / W_dot_opt_global > -0.005)
                {
                    if (eta_max_mc > eta_max_global)
                    {
                        eta_max_global = eta_max_mc;
                    
                        P_LP_in_opt_global = P_LP_in_opt_mc;		//[kPa]
                    
                        T_mc_in_opt_global = T_mc_in_opt_mc;		//[K]
                    
                        T_pc_in_opt_global = ms_cycle_od_par.m_T_pc_in;     //[K]
                    
                        if (W_dot_opt_mc > W_dot_opt_global)
                            W_dot_opt_global = W_dot_opt_mc;		//[kWe]
                    }
                    else if (eta_max_global - eta_max_mc > 0.0015)
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }

            }

            // Now solve cycle off-design with saved optimal inputs
            ms_cycle_od_par.m_P_LP_comp_in = P_LP_in_opt_global;    //[kPa]
            ms_cycle_od_par.m_T_mc_in = T_mc_in_opt_global;			//[K]
            ms_cycle_od_par.m_T_pc_in = T_pc_in_opt_global;			//[K]

            double f_od_obj = std::numeric_limits<double>::quiet_NaN();
            int od_opt_err_code = off_design_core(f_od_obj, od_tol);

            if (od_opt_err_code != 0)
            {
                throw(C_csp_exception("C_sco2_phx_air_cooler::optimize_off_design to maximize efficiency failed"));
            }
        }
	}		

	double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
	int air_cooler_err_code = mpc_sco2_cycle->solve_OD_all_coolers_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_fan);

	if (air_cooler_err_code != 0)
	{
		throw(C_csp_exception("Off design air cooler model failed"));
	}

	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return 0;
}

int C_sco2_phx_air_cooler::check_increasing_T_mc_in(double W_dot_target /*kWe*/, double W_dot_fan_limit /*MWe*/,
    bool is_modified_P_mc_in_solver,
    C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
    double & W_dot_opt /*kWe*/, double & eta_max_at_W_dot_opt /*-*/,
    double & P_LP_in_opt /*kPa*/, double & T_mc_in_opt /*K*/,
    double od_tol /*-*/)
{
    // For the E_FIXED_MC_FIXED_RC_FIXED_T configuration, at off design ambient temperatures colder than design
    // ... the cycle may not be able to reach the target power and/or the cycle efficiency may improve if compressor inlet temperatures increase
    // Increase compressor inlet temperatures
    // Track power output between steps
    // If target power is achieved, store max efficiency case. Otherwise, track maximum power case
    // Limit to target power?

    int cycle_config = get_design_par()->m_cycle_config;    //[-]

    W_dot_opt = std::min(W_dot_target, mpc_sco2_cycle->get_od_solved()->m_W_dot_net);	//[kWe]

    eta_max_at_W_dot_opt = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

    P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

    T_mc_in_opt = ms_cycle_od_par.m_T_mc_in;		//[K]

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in__tracker;

    //double T_pc_in_opt = ms_cycle_od_par.m_T_pc_in;     //[K]

    while (true)
    {
        // Increase compressor inlet temperatures by constant interval
        ms_cycle_od_par.m_T_mc_in += 0.5;	//[K]
        //ms_cycle_od_par.m_T_pc_in += 0.5;	//[K]

        int opt_P_LP_err = solve_P_LP_in__objective(od_opt_objective, P_LP_in__tracker, od_tol);

        if (opt_P_LP_err != 0)
        {	// If off-design breaks, we've solved at colder temperatures, so don't crash the entire simulation
            // just exit loop that increases temperature
            // This can happen especially w/ the partial cooling cycle when bypass is not required initially, but becomes necessary in this loop
            return opt_P_LP_err;
        }

        if (cycle_config == 2)
        {
            double W_dot_pc_cooler_fan = std::numeric_limits<double>::quiet_NaN();
            double P_pc_cooler_out = std::numeric_limits<double>::quiet_NaN();
            mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_pc_cooler_fan, P_pc_cooler_out);
            if (W_dot_pc_cooler_fan > W_dot_fan_limit)
            {
                return 0;
            }
        }

        // Get most recent outputs
        double W_dot_calc = std::min(W_dot_target, mpc_sco2_cycle->get_od_solved()->m_W_dot_net);	//[kPWe]
        double eta_calc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

        // If current inlet temperatures generate more power than optimal, reset optimal
        // This relies on 'opt_P_LP_comp_in__fixed_N_turbo()' not return a power that is over-target...
        // '0.002' seems reasonable based on parametric runs but may be suboptimal
        if ((W_dot_calc - W_dot_opt) / W_dot_opt > 0.002)
        {
            eta_max_at_W_dot_opt = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

            P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

            T_mc_in_opt = ms_cycle_od_par.m_T_mc_in;		//[K]

            //T_pc_in_opt = ms_cycle_od_par.m_T_pc_in;     //[K]

            W_dot_opt = W_dot_calc;		//[kWe]
        }

        // Otherwise, if the current inlet temperature generates very close to the current optimal power
        // then check if the efficiency is higher. If so, save efficiency
        else if ((W_dot_calc - W_dot_opt) / W_dot_opt > -0.005)
        {
            if (eta_calc > eta_max_at_W_dot_opt)
            {
                eta_max_at_W_dot_opt = eta_calc;

                P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

                T_mc_in_opt = ms_cycle_od_par.m_T_mc_in;		//[K]

                //T_pc_in_opt = ms_cycle_od_par.m_T_pc_in;     //[K]

                if (W_dot_calc > W_dot_opt)
                    W_dot_opt = W_dot_calc;		//[kWe]
            }
            else if (eta_max_at_W_dot_opt - eta_calc > 0.0015)
            {
                return 0;
            }
        }
        else
        {
            return 0;
        }
    }

    return 0;
}

//int C_sco2_phx_air_cooler::solve_P_LP_in__objective(E_off_design_strategies od_opt_objective)
//{
//    if (od_opt_objective == E_TARGET_POWER_ETA_MAX)
//    {
//        return solve_P_LP_in__target_W_dot();
//    }
//    else if (od_opt_objective == E_TARGET_T_HTF_COLD_POWER_MAX)
//    {
//        return solve_P_LP_in__target_T_htf_cold();
//    }
//    else
//    {
//        throw(C_csp_exception("Off design cycle operation strategy not recognized"));
//    }
//}

int C_sco2_phx_air_cooler::solve_P_LP_in__objective(E_off_design_strategies od_opt_objective,
                                            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> & v_call_tracker,
                                            double od_tol /*-*/)
{
    int ret_code = -1;

    if (od_opt_objective == E_TARGET_POWER_ETA_MAX)
    {
        ret_code = solve_P_LP_in__target_W_dot(od_tol);
    }
    else if (od_opt_objective == E_TARGET_T_HTF_COLD_POWER_MAX)
    {
        ret_code = solve_P_LP_in__target_T_htf_cold(od_tol);
    }
    else
    {
        throw(C_csp_exception("Off design cycle operation strategy not recognized"));
    }

    S_solve_P_LP_in__tracker local_tracker_values;

    local_tracker_values.m_error_code = ret_code;
    if (ret_code != 0) {
        v_call_tracker.push_back(local_tracker_values);
        return ret_code;
    }

    local_tracker_values.m_T_mc_in = mpc_sco2_cycle->get_od_solved()->m_temp[C_sco2_cycle_core::MC_IN]; //[K]
    local_tracker_values.m_T_pc_in = mpc_sco2_cycle->get_od_solved()->m_temp[C_sco2_cycle_core::PC_IN]; //[K]
    local_tracker_values.m_W_dot_fan_mc_cooler = mpc_sco2_cycle->get_od_solved()->ms_mc_air_cooler_od_solved.m_W_dot_fan;   //[MWe]
    local_tracker_values.m_W_dot_fan_pc_cooler = mpc_sco2_cycle->get_od_solved()->ms_pc_air_cooler_od_solved.m_W_dot_fan;   //[MWe]
    local_tracker_values.m_rel_diff_T_htf_cold = (mc_phx.ms_od_solved.m_T_h_out - ms_des_solved.ms_phx_des_solved.m_T_h_out)/ ms_des_solved.ms_phx_des_solved.m_T_h_out;  //[-]
    double W_dot_fan_total = local_tracker_values.m_W_dot_fan_mc_cooler;
    if (ms_des_par.m_cycle_config == 2) {
        W_dot_fan_total += local_tracker_values.m_W_dot_fan_pc_cooler;      //[MWe]
    }
    +local_tracker_values.m_W_dot_fan_pc_cooler; //[MWe]
    double W_dot_net = mpc_sco2_cycle->get_od_solved()->m_W_dot_net * 1.E-3;   //[MWe]
    local_tracker_values.m_W_dot_net_less_cooling = W_dot_net - W_dot_fan_total;    //[MWe]

    if (od_opt_objective == C_sco2_phx_air_cooler::E_TARGET_T_HTF_COLD_POWER_MAX) {
        if (local_tracker_values.m_error_code != 0) {
            local_tracker_values.m_objective = 0.0;
        }
        else{
            double T_htf_cold_exp_penalty = std::max(0.0, std::fabs(local_tracker_values.m_rel_diff_T_htf_cold) - 0.002);
            local_tracker_values.m_objective = local_tracker_values.m_W_dot_net_less_cooling * std::exp(-T_htf_cold_exp_penalty * 100.0);
        }
    }

    v_call_tracker.push_back(local_tracker_values);

    return ret_code;
}

int C_sco2_phx_air_cooler::solve_P_LP_in__target_T_htf_cold(double od_tol /*-*/)
{
    ms_cycle_od_par.m_count_off_design_core = 0;

    double tol_margin = 1.0;

    // Prior to calling, need to set :
    //	*ms_od_par, ms_rc_cycle_od_phi_par, ms_phx_od_par, ms_od_op_inputs(will set P_mc_in here and f_recomp downstream)

    double T_htf_cold_target = get_design_solved()->ms_phx_des_solved.m_T_h_out;    //[K]

    // Set up monotonic equation solver to find the compressor inlet pressure that results in the target HTF cold outlet
    C_MEQ__P_LP_in__T_htf_cold_target c_P_LP_in_eq(this, od_tol);
    C_monotonic_eq_solver c_P_LP_in_solver(c_P_LP_in_eq);

    double P_lower_limit_global = 1000;    //[kPa]
    double P_upper_limit_global = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT];   //[kPa]
    c_P_LP_in_solver.settings(od_tol, 50, P_lower_limit_global, P_upper_limit_global, true);

    // Get density at design point
    double mc_dens_in_des = std::numeric_limits<double>::quiet_NaN();

    if (ms_des_par.m_cycle_config == 1)
        mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::MC_IN];		//[kg/m^3]
    else
        mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::PC_IN];		//[kg/m^3]

    CO2_state co2_props;
    // Then calculate the compressor inlet pressure that achieves this density at the off-design ambient temperature
    CO2_TD(ms_cycle_od_par.m_T_mc_in, mc_dens_in_des, &co2_props);
    double mc_pres_dens_des_od = co2_props.pres;	//[kPa]
    double P_LP_in_guess = mc_pres_dens_des_od;	    //[kPa]

    // Try guess value and check that OD model *converged* (can have constraint limits)
    mc_P_LP_in_iter_tracker.reset_vectors();
    C_monotonic_eq_solver::S_xy_pair xy_1;

    double T_htf_cold__at_P_guess = std::numeric_limits<double>::quiet_NaN();
    int T_htf_cold_err_code = c_P_LP_in_solver.test_member_function(P_LP_in_guess, &T_htf_cold__at_P_guess);

    // If first guess didn't solve, 
    while (T_htf_cold_err_code != 0 && P_LP_in_guess > P_lower_limit_global)
    {
        P_LP_in_guess -= 500;  //[kpa]
        T_htf_cold_err_code = c_P_LP_in_solver.test_member_function(P_LP_in_guess, &T_htf_cold__at_P_guess);
    }

    if (T_htf_cold_err_code != 0)
    {
        return -31;
    }

    xy_1.x = P_LP_in_guess; //[kPa]
    xy_1.y = T_htf_cold__at_P_guess; //[kWe]

    // Try another guess value close to the first, and check slope of T_htf_cold vs P_LP_in
    // Expect T_htf_cold to decrease as pressure increases
    C_monotonic_eq_solver::S_xy_pair xy_2;
    if (T_htf_cold__at_P_guess > T_htf_cold_target)
    {
        xy_2.x = 1.05 * P_LP_in_guess;

        if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
        {
            xy_2.x = 0.95 * P_LP_in_guess;

            if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
            {
                xy_2.x = 1.01 * P_LP_in_guess;

                if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                {
                    xy_2.x = 0.99 * P_LP_in_guess;
                    if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                    {
                        return -31;
                    }
                }
            }
        }
    }
    else
    {
        xy_2.x = 0.95 * P_LP_in_guess;

        if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
        {
            xy_2.x = 1.05 * P_LP_in_guess;

            if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
            {
                xy_2.x = 1.01 * P_LP_in_guess;

                if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                {
                    xy_2.x = 0.99 * P_LP_in_guess;

                    if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                    {
                        return -31;
                    }
                }
            }
        }
    }

    double T_htf_cold_vs_P_LP_slope = (xy_1.y - xy_2.y) / (xy_1.x - xy_2.x);    //[K/kPa]

    // Expecting slope to be negative, so if it's positive, try searching across pressures to find negative slope
    if (T_htf_cold_vs_P_LP_slope > 0.0)
    {
        double T_htf_cold_low = std::min(xy_1.y, xy_2.y);   //[K]
        double T_htf_cold_i = T_htf_cold_low;               //[K]

        double P_at_T_htf_cold_low = std::min(xy_1.x, xy_2.x);  //[kPa]
        double P_i = P_at_T_htf_cold_low;       //[kPa]

        // Need an outside loop in case err code is != before we find negative slope
        while (true)
        {
            // Step down pressures until find negative slope or error
            while (T_htf_cold_err_code == 0 && P_i > P_lower_limit_global && T_htf_cold_i <= T_htf_cold_low)
            {
                T_htf_cold_low = T_htf_cold_i;
                P_at_T_htf_cold_low = P_i;
                P_i -= 500;     //[kPa]
                T_htf_cold_err_code = c_P_LP_in_solver.test_member_function(P_i, &T_htf_cold_i);
            }

            // If no error, then get out
            if (T_htf_cold_err_code == 0)
            {
                break;
            }
            else
            {   // Otherwise, step down in pressure until no error and repeat process
                while (T_htf_cold_err_code != 0 && P_i > P_lower_limit_global)
                {
                    P_i -= 500;
                    T_htf_cold_err_code = c_P_LP_in_solver.test_member_function(P_i, &T_htf_cold_i);
                }
                if (T_htf_cold_err_code != 0)
                {   // Can't find negative slope, try converging with positive slope
                    T_htf_cold_i = T_htf_cold_low = std::numeric_limits<double>::quiet_NaN();
                    break;
                }
                else
                {
                    T_htf_cold_low = T_htf_cold_i;
                }
            }
        }

        // If we found negative slope, set xy structures
        if (T_htf_cold_err_code == 0 && T_htf_cold_i < T_htf_cold_low)
        {
            xy_1.x = P_i;
            xy_1.y = T_htf_cold_i;

            xy_2.x = P_at_T_htf_cold_low;
            xy_2.y = T_htf_cold_low;
        }
    }

    // Try to find pressure resulting in target cold temp UNLESS temp is greater than target and already over pressure
    if (!(xy_2.y > T_htf_cold_target && ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_OVER_PRESSURE))
    {
        double P_LP_in_T_htf_cold_target, tol_T_htf_cold_target;
        P_LP_in_T_htf_cold_target = tol_T_htf_cold_target = std::numeric_limits<double>::quiet_NaN();
        int T_htf_cold_iter = 0;
        T_htf_cold_err_code = c_P_LP_in_solver.solve(xy_1, xy_2, T_htf_cold_target, P_LP_in_T_htf_cold_target, tol_T_htf_cold_target, T_htf_cold_iter);

        if (T_htf_cold_err_code != C_monotonic_eq_solver::CONVERGED && fabs(tol_T_htf_cold_target) > od_tol*tol_margin)    // && tol_T_htf_cold_target > 1.E-3)
        {
            return -31;
        }
    }

    double P_mc_out_target = (1.0 - od_tol) * ms_des_par.m_P_high_limit;

    // If solution at target power is over pressure, then adjust to meet pressure constraint
    if (ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_OVER_PRESSURE)
    {
        C_MEQ__P_LP_in__P_mc_out_target c_P_LP__P_mc_out_eq(this, od_tol);
        C_monotonic_eq_solver c_P_LP__P_mc_out_solver(c_P_LP__P_mc_out_eq);

        c_P_LP__P_mc_out_solver.settings(od_tol, 50, P_lower_limit_global, P_upper_limit_global, true);

        double P_LP_in_P_mc_out_target, tol_P_mc_out;
        P_LP_in_P_mc_out_target = tol_P_mc_out = std::numeric_limits<double>::quiet_NaN();
        int P_mc_out_iter = 0;
        int P_mc_out_err_code = c_P_LP__P_mc_out_solver.solve(mc_P_LP_in_iter_tracker.mv_P_LP_in, mc_P_LP_in_iter_tracker.mv_P_mc_out, P_mc_out_target, P_LP_in_P_mc_out_target, tol_P_mc_out, P_mc_out_iter);

        if (P_mc_out_err_code != C_monotonic_eq_solver::CONVERGED && fabs(tol_P_mc_out) > od_tol*tol_margin) // && tol_P_mc_out > 1.E-3)
        {
            return -31;
        }
    }

    // The last solution should have the maximum possible pressure...
    // If the cold htf return temperature is now somehow less than target, fix max inlet pressure and iterate again
    double f_obj_local = std::numeric_limits<double>::quiet_NaN();
    while ((T_htf_cold_target - mc_phx.ms_od_solved.m_T_h_out) / T_htf_cold_target > od_tol)
    {
        ms_cycle_od_par.m_P_LP_comp_in *= 0.99;
        off_design_core(f_obj_local, od_tol);
    }

    // If a compressor is violating tip speed constraint at this pressure, then there is no solution
    // to both satisfy tip speed constraint and stay within power target
    if (ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_TIP_RATIO)
    {
        return C_sco2_phx_air_cooler::E_TIP_RATIO;
    }

    // Check whether cycle is still solving with an error code
    while (ms_od_solved.m_od_error_code != 0)
    {
        // Iterating blind here - we don't have a target metric
        // But we know we want the highest pressure that doesn't have an error code
        double P_LP_in_upper_local = std::numeric_limits<double>::quiet_NaN();
        if (ms_des_par.m_cycle_config == 1)
        {
            P_LP_in_upper_local = mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
        }
        else
        {
            P_LP_in_upper_local = mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::PC_IN];  //[kPa]
        }

        C_MEQ__P_LP_in__max_no_err_code c_P_LP__no_err_eq(this, od_tol);
        C_monotonic_eq_solver c_P_LP__no_err_solver(c_P_LP__no_err_eq);

        // sort the P_LP_in values solved so far
        std::vector<double> v_P_LP_in_sorted = mc_P_LP_in_iter_tracker.mv_P_LP_in;
        std::sort(v_P_LP_in_sorted.begin(), v_P_LP_in_sorted.end());

        // find index in sorted vector of P_LP_in_upper_local
        std::vector<double>::iterator it_sorted_P_LP_in_upper_local = std::find(v_P_LP_in_sorted.begin(), v_P_LP_in_sorted.end(), P_LP_in_upper_local);

        // Now, stepping down in pressure, want to find index in sorted where od_error_code is 0
        bool is_P_no_error_found = false;
        int i_base_no_error = -1;
        if (it_sorted_P_LP_in_upper_local != v_P_LP_in_sorted.begin()) {
            std::vector<double>::iterator it_sorted_find = it_sorted_P_LP_in_upper_local - 1;
            double P_LP_in_find_no_error = std::numeric_limits<double>::quiet_NaN();
            while (it_sorted_find != v_P_LP_in_sorted.begin()) {
                P_LP_in_find_no_error = *it_sorted_find;
                std::vector<double>::iterator it_base_find = std::find(mc_P_LP_in_iter_tracker.mv_P_LP_in.begin(), mc_P_LP_in_iter_tracker.mv_P_LP_in.end(), P_LP_in_find_no_error);
                i_base_no_error = it_base_find - mc_P_LP_in_iter_tracker.mv_P_LP_in.begin();
                int local_od_error_code = mc_P_LP_in_iter_tracker.mv_od_error_code[i_base_no_error];
                if (local_od_error_code == 0)
                {
                    is_P_no_error_found = true;
                    break;
                }
                it_sorted_find--;
            }
        }

        double y_P_mc_out_lower = std::numeric_limits<double>::quiet_NaN();
        double P_LP_in_guess_lower = std::numeric_limits<double>::quiet_NaN();
        if (is_P_no_error_found) {

            P_LP_in_guess_lower = mc_P_LP_in_iter_tracker.mv_P_LP_in[i_base_no_error];
            y_P_mc_out_lower = mc_P_LP_in_iter_tracker.mv_P_mc_out[i_base_no_error];
            double P_LP_in_upper_local = mc_P_LP_in_iter_tracker.mv_P_LP_in[i_base_no_error + 1];

        }
        else {
            double P_step = 250;    //[kPa]

            P_LP_in_guess_lower = P_LP_in_upper_local - P_step;  //[kPa]

            int no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);

            while (!std::isfinite(y_P_mc_out_lower) && P_LP_in_guess_lower > P_lower_limit_global)
            {
                P_LP_in_guess_lower -= 500;  //[kpa]
                no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);
            }

            if (!std::isfinite(y_P_mc_out_lower))
            {
                return -31;
            }
        }

        double P_LP_in_guess_upper = P_LP_in_guess_lower * (1.0 + od_tol);      //[kPa]

        double y_P_mc_out_upper = std::numeric_limits<double>::quiet_NaN();

        int no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_upper, &y_P_mc_out_upper);

        if (!std::isfinite(y_P_mc_out_upper) || y_P_mc_out_lower > y_P_mc_out_upper)
        {
            // If an inlet pressure that is very close to the first pressure fails, then choose first pressure
            // Need to call model again to use the correct pressure.
            no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);
            break;
        }

        xy_1.x = P_LP_in_guess_lower;
        xy_1.y = y_P_mc_out_lower;

        //C_monotonic_eq_solver::S_xy_pair xy_2;
        xy_2.x = P_LP_in_guess_upper;
        xy_2.y = y_P_mc_out_upper;

        c_P_LP__no_err_solver.settings(od_tol, 50, P_LP_in_guess_lower * 0.999, P_LP_in_upper_local, true);

        double P_LP_in_no_err, tol_no_err;
        P_LP_in_no_err = tol_no_err = std::numeric_limits<double>::quiet_NaN();
        int no_err_iter = 0;
        no_err_err_code = c_P_LP__no_err_solver.solve(xy_1, xy_2, P_mc_out_target, P_LP_in_no_err, tol_no_err, no_err_iter);

        // Expect not to be able to converge, instead want to see SLOPE_POS_NO_POS_ERR
        //    indicating solver is at inlet pressure where errors begin
        if (no_err_err_code != C_monotonic_eq_solver::CONVERGED && no_err_err_code != C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR && fabs(tol_no_err) > 1.E-3*tol_margin)
        {
            return -31;
        }

        // Should have a valid cycle solution, but check here
        if (!std::isfinite(mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN]))
        {
            return -31;
        }
    }

    ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
    ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

    return 0;
}

int C_sco2_phx_air_cooler::solve_P_LP_in__target_W_dot(double od_tol /*-*/)
{
    ms_cycle_od_par.m_count_off_design_core = 0;

    // Prior to calling, need to set :
    //	*ms_od_par, ms_rc_cycle_od_phi_par, ms_phx_od_par, ms_od_op_inputs(will set P_mc_in here and f_recomp downstream)

    double W_dot_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * ms_des_par.m_W_dot_net;	//[kWe]

    // Set up monotonic equation solver to find the compressor inlet pressure that results in the target power output
    C_MEQ__P_LP_in__W_dot_target c_P_LP_in_eq(this, od_tol);
    C_monotonic_eq_solver c_P_LP_in_solver(c_P_LP_in_eq);

    double P_lower_limit_global = 1000;    //[kPa]
    double P_upper_limit_global = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT];   //[kPa]
    c_P_LP_in_solver.settings(od_tol, 50, P_lower_limit_global, P_upper_limit_global, true);

    // Get density at design point
    double mc_dens_in_des = std::numeric_limits<double>::quiet_NaN();

    if (ms_des_par.m_cycle_config == 1)
        mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::MC_IN];		//[kg/m^3]
    else
        mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::PC_IN];		//[kg/m^3]

    CO2_state co2_props;
    // Then calculate the compressor inlet pressure that achieves this density at the off-design ambient temperature
    CO2_TD(ms_cycle_od_par.m_T_mc_in, mc_dens_in_des, &co2_props);
    double mc_pres_dens_des_od = co2_props.pres;	//[kPa]
    double P_LP_in_guess = mc_pres_dens_des_od;	    //[kPa]

    // Try guess value and check that OD model *converged* (can have constraint limits)
    mc_P_LP_in_iter_tracker.reset_vectors();
    C_monotonic_eq_solver::S_xy_pair xy_1;

    double y_W_dot_guess = std::numeric_limits<double>::quiet_NaN();
    int W_dot_err_code = c_P_LP_in_solver.test_member_function(P_LP_in_guess, &y_W_dot_guess);

    while (W_dot_err_code != 0 && P_LP_in_guess > P_lower_limit_global)
    {
        P_LP_in_guess -= 500;  //[kpa]
        W_dot_err_code = c_P_LP_in_solver.test_member_function(P_LP_in_guess, &y_W_dot_guess);
    }

    if (W_dot_err_code != 0)
    {
        return -31;
    }

    xy_1.x = P_LP_in_guess; //[kPa]
    xy_1.y = y_W_dot_guess; //[kWe]

    // Try another guess value close to the first, and check slope of W_dot vs P_LP_in
    C_monotonic_eq_solver::S_xy_pair xy_2;
    if (y_W_dot_guess < W_dot_target)
    {
        xy_2.x = 1.05*P_LP_in_guess;

        if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
        {
            xy_2.x = 0.95*P_LP_in_guess;

            if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
            {
                xy_2.x = 1.01*P_LP_in_guess;

                if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                {
                    xy_2.x = 0.99*P_LP_in_guess;
                    if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                    {
                        return -31;
                    }
                }
            }
        }
    }
    else
    {
        xy_2.x = 0.95*P_LP_in_guess;

        if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
        {
            xy_2.x = 1.05*P_LP_in_guess;

            if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
            {
                xy_2.x = 1.01*P_LP_in_guess;

                if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                {
                    xy_2.x = 0.99*P_LP_in_guess;

                    if (c_P_LP_in_solver.test_member_function(xy_2.x, &xy_2.y) != 0)
                    {
                        return -31;
                    }
                }
            }
        }
    }    

    if (W_dot_err_code != 0) 
    {
        if (!(xy_1.y < W_dot_target && ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_OVER_PRESSURE))
        {            
            double P_LP_in_W_dot_target, tol_W_dot;
            P_LP_in_W_dot_target = tol_W_dot = std::numeric_limits<double>::quiet_NaN();
            int W_dot_iter = 0;
            W_dot_err_code = c_P_LP_in_solver.solve(xy_1, P_LP_in_guess - 1000.0, W_dot_target, P_LP_in_W_dot_target, tol_W_dot, W_dot_iter);

            if (W_dot_err_code != C_monotonic_eq_solver::CONVERGED && tol_W_dot > 1.E-3)
            {
                return -31;
            }
        }
    }
    else
    {
        double W_vs_P_LP_slope = (xy_1.y - xy_2.y) / (xy_1.x - xy_2.x);

        // Expecting slope to be positive, so if it's not, need to try to find positive slope
        if (W_vs_P_LP_slope < 0.0)
        {
            double W_dot_high = std::max(xy_1.y, xy_2.y);
            double W_dot_i = W_dot_high;

            double P_at_W_dot_high = std::min(xy_1.x, xy_2.x);
            double P_i = P_at_W_dot_high;

            while (true)
            {
                // Try decreasing pressure until power also decreases
                while (W_dot_err_code == 0 && P_i > P_lower_limit_global && W_dot_i >= W_dot_high)
                {
                    W_dot_high = W_dot_i;
                    P_at_W_dot_high = P_i;
                    P_i -= 500;  //[kpa]
                    W_dot_err_code = c_P_LP_in_solver.test_member_function(P_i, &W_dot_i);
                }

                if (W_dot_err_code == 0)
                {
                    break;
                }
                else
                {
                    while (W_dot_err_code != 0 && P_i > P_lower_limit_global)
                    {
                        P_i -= 500;
                        W_dot_err_code = c_P_LP_in_solver.test_member_function(P_i, &y_W_dot_guess);
                    }
                    if (W_dot_err_code != 0)
                    {
                        W_dot_i = W_dot_high = std::numeric_limits<double>::quiet_NaN();
                        break;
                    }
                    else
                    {
                        W_dot_i = W_dot_high = mpc_sco2_cycle->get_od_solved()->m_W_dot_net;
                    }
                }
            }

            if (W_dot_err_code == 0 && W_dot_i < W_dot_high)
            {
                xy_1.x = P_i;
                xy_1.y = W_dot_i;

                xy_2.x = P_at_W_dot_high;
                xy_2.y = W_dot_high;
            }
        }

        // Try to find pressure resulting in target power UNLESS power is less than target and already over pressure
        if (!(xy_2.y < W_dot_target && ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_OVER_PRESSURE))
        {
            double P_LP_in_W_dot_target, tol_W_dot;
            P_LP_in_W_dot_target = tol_W_dot = std::numeric_limits<double>::quiet_NaN();
            int W_dot_iter = 0;
            W_dot_err_code = c_P_LP_in_solver.solve(xy_1, xy_2, W_dot_target, P_LP_in_W_dot_target, tol_W_dot, W_dot_iter);

            if (W_dot_err_code != C_monotonic_eq_solver::CONVERGED && tol_W_dot > 1.E-3)
            {
                return -31;
            }
        }
    }

    double P_mc_out_target = (1.0 - od_tol)*ms_des_par.m_P_high_limit;

    // If solution at target power is over pressure, then adjust to meet pressure constraint
    if (ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_OVER_PRESSURE)
    {
        C_MEQ__P_LP_in__P_mc_out_target c_P_LP__P_mc_out_eq(this, od_tol);
        C_monotonic_eq_solver c_P_LP__P_mc_out_solver(c_P_LP__P_mc_out_eq);

        c_P_LP__P_mc_out_solver.settings(od_tol, 50, P_lower_limit_global, P_upper_limit_global, true);

        double P_LP_in_P_mc_out_target, tol_P_mc_out;
        P_LP_in_P_mc_out_target = tol_P_mc_out = std::numeric_limits<double>::quiet_NaN();
        int P_mc_out_iter = 0;
        int P_mc_out_err_code = c_P_LP__P_mc_out_solver.solve(mc_P_LP_in_iter_tracker.mv_P_LP_in, mc_P_LP_in_iter_tracker.mv_P_mc_out, P_mc_out_target, P_LP_in_P_mc_out_target, tol_P_mc_out, P_mc_out_iter);

        if (P_mc_out_err_code != C_monotonic_eq_solver::CONVERGED && tol_P_mc_out > 1.E-3)
        {
            return -31;
        }
    }

    // The last solution should have the maximum possible pressure...
    // If net power is now somehow over the limit, fix max inlet pressure and iterate again
    double f_obj_local = std::numeric_limits<double>::quiet_NaN();
    while ((mpc_sco2_cycle->get_od_solved()->m_W_dot_net - W_dot_target) / W_dot_target > 0.001)
    {
        ms_cycle_od_par.m_P_LP_comp_in *= 0.99;
        off_design_core(f_obj_local, od_tol);
    }

    // If a compressor is violating tip speed constraint at this pressure, then there is no solution
    // to both satisfy tip speed constraint and stay within power target
    if (ms_od_solved.m_od_error_code == C_sco2_phx_air_cooler::E_TIP_RATIO)
    {
        return C_sco2_phx_air_cooler::E_TIP_RATIO;
    }

    // We can use these iterations to inform this next iteration

    // Check whether cycle is still solving with an error code
    while (ms_od_solved.m_od_error_code != 0)
    {
        // Iterating blind here - we don't have a target metric
        // But we know we want the highest pressure that doesn't have an error code
        double P_LP_in_upper_local = std::numeric_limits<double>::quiet_NaN();
        if (ms_des_par.m_cycle_config == 1)
        {
            P_LP_in_upper_local = mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN];  //[kPa]
        }
        else
        {
            P_LP_in_upper_local = mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::PC_IN];  //[kPa]
        }

        double P_step = 250;    //[kPa]

        double P_LP_in_guess_lower = P_LP_in_upper_local - P_step;  //[kPa]

        C_MEQ__P_LP_in__max_no_err_code c_P_LP__no_err_eq(this, od_tol);
        C_monotonic_eq_solver c_P_LP__no_err_solver(c_P_LP__no_err_eq);

        double y_P_mc_out_lower = std::numeric_limits<double>::quiet_NaN();
            
        int no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);

        while ( !std::isfinite(y_P_mc_out_lower) && P_LP_in_guess_lower > P_lower_limit_global)
        {
            P_LP_in_guess_lower -= 500;  //[kpa]
            no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);
        }

        if (!std::isfinite(y_P_mc_out_lower))
        {
            return -31;
        }

        double P_LP_in_guess_upper = P_LP_in_guess_lower * 1.001;    //[kPa]

        double y_P_mc_out_upper = std::numeric_limits<double>::quiet_NaN();

        no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_upper, &y_P_mc_out_upper);

        if (!std::isfinite(y_P_mc_out_upper))
        {
            // If an inlet pressure that is very close to the first pressure fails, then choose first pressure
            // Need to call model again to use the correct pressure.
            no_err_err_code = c_P_LP__no_err_solver.test_member_function(P_LP_in_guess_lower, &y_P_mc_out_lower);
            break;
        }

        xy_1.x = P_LP_in_guess_lower;
        xy_1.y = y_P_mc_out_lower;

        C_monotonic_eq_solver::S_xy_pair xy_2;
        xy_2.x = P_LP_in_guess_upper;
        xy_2.y = y_P_mc_out_upper;

        c_P_LP__no_err_solver.settings(od_tol, 50, P_LP_in_guess_lower*0.999, P_LP_in_upper_local, true);

        double P_LP_in_no_err, tol_no_err;
        P_LP_in_no_err = tol_no_err = std::numeric_limits<double>::quiet_NaN();
        int no_err_iter = 0;
        no_err_err_code = c_P_LP__no_err_solver.solve(xy_1, xy_2, P_mc_out_target, P_LP_in_no_err, tol_no_err, no_err_iter);

        // Expect not to be able to converge, instead want to see SLOPE_POS_NO_POS_ERR
        //    indicating solver is at inlet pressure where errors begin
        if (no_err_err_code != C_monotonic_eq_solver::CONVERGED && no_err_err_code != C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR && tol_no_err > 1.E-3)
        {
            return -31;
        }

        // Should have a valid cycle solution, but check here
        if (!std::isfinite(mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN]))
        {
            return -31;
        }
    }

    ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
    ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

    return 0;
}

void C_sco2_phx_air_cooler::check_od_solution(double & diff_m_dot, double & diff_E_cycle,
    double & diff_Q_LTR, double & diff_Q_HTR)
{
    mpc_sco2_cycle->check_od_solution(diff_m_dot, diff_E_cycle, diff_Q_LTR, diff_Q_HTR);
}

double C_sco2_phx_air_cooler::adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/)
{	
	double P_mc_in_restricted = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	CO2_state co2_props;
	// Is T_co2 < the critical temperature
	if (T_co2 < m_T_co2_crit)
	{
		CO2_TQ(T_co2, 0.0, &co2_props);
		P_mc_in_restricted = co2_props.pres;	//[kPa]
	}
	else if (T_co2 < 1.001*m_T_co2_crit)
	{	// Else, T_co2 > the critical temperature
		P_mc_in_restricted = m_P_co2_crit;		//[kPa]
	}
	else
	{
		return P_mc_in;	//[kPa]
	}
	
	if (P_mc_in >= P_mc_in_restricted)
	{
		double P_upper = 1.01*P_mc_in_restricted;
		if (P_mc_in < P_upper)
		{
			double P_mid = 1.005*P_mc_in_restricted;
			return P_upper - (P_upper - P_mc_in)/(P_upper - P_mc_in_restricted)*(P_upper-P_mid);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}
	else
	{
		double P_lower = 0.99*P_mc_in_restricted;
		if (P_mc_in > P_lower)
		{
			return P_lower + (P_mc_in - P_lower)/(P_mc_in_restricted - P_lower)*(P_mc_in-P_lower);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}

}

int C_sco2_phx_air_cooler::off_design_core(double & eta_solved, double od_tol /*-*/)
{
    ms_cycle_od_par.m_count_off_design_core++;

    ms_cycle_od_par.m_P_LP_comp_in = adjust_P_mc_in_away_2phase(ms_cycle_od_par.m_T_mc_in, ms_cycle_od_par.m_P_LP_comp_in);

    int T_t_in_mode = ms_cycle_od_par.m_T_t_in_mode;        //[-]

	// Apply 1 var solver to find the turbine inlet temperature that results in a "converged" PHX
	C_mono_eq_T_t_in c_phx_cycle(this, T_t_in_mode, od_tol);
	C_monotonic_eq_solver c_phx_cycle_solver(c_phx_cycle);

    bool is_hx_deltaP_converge = true;
    size_t iter_deltaP = 0;

    double diff_P_LTR_HP_out_calc_less_guess = 0.0;      //[kPa]
    double diff_P_LTR_HP_out_rel = 0.0;                  //[-]

    double diff_P_HTR_HP_out_calc_less_guess = 0.0;     //[kPa]
    double diff_P_HTR_HP_out_rel = 0.0;                 //[-]

    double diff_P_PHX_out_calc_less_guess = 0.0;        //[kPa]
    double diff_P_PHX_out_rel = 0.0;                    //[-]

    double diff_P_HTR_LP_out_calc_less_guess = 0.0;     //[kPa]
    double diff_P_HTR_LP_out_rel = 0.0;                 //[-]

    double diff_P_LTR_LP_out_calc_less_guess = 0.0;     //[kPa]
    double diff_P_LTR_LP_out_rel = 0.0;                 //[-]

    double diff_P_mc_cooler_out_calc_less_guess = 0.0;  //[kPa]
    double diff_P_mc_cooler_out_rel = 0.0;              //[kPa]

    double diff_P_pc_cooler_out_calc_less_guess = 0.0;  //[kPa]
    double diff_P_pc_cooler_out_rel = 0.0;              //[kPa]

    do
    {
        if (iter_deltaP < 11)
        {
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_LTR_HP_out_calc_less_guess = diff_P_LTR_HP_out_calc_less_guess;    //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_HTR_HP_out_calc_less_guess = diff_P_HTR_HP_out_calc_less_guess;    //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_PHX_out_calc_less_guess = diff_P_PHX_out_calc_less_guess;          //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_HTR_LP_out_calc_less_guess = diff_P_HTR_LP_out_calc_less_guess;    //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_LTR_LP_out_calc_less_guess = diff_P_LTR_LP_out_calc_less_guess;    //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_mc_cooler_out_calc_less_guess = diff_P_mc_cooler_out_calc_less_guess;  //[kPa]
            mpc_sco2_cycle->ms_od_deltaP.m_od_diff_P_pc_cooler_out_calc_less_guess = diff_P_pc_cooler_out_calc_less_guess;  //[kPa]
        }
        else
        {
            C_sco2_cycle_core::S_od_deltaP s_od_deltaP_temp;
            mpc_sco2_cycle->ms_od_deltaP = s_od_deltaP_temp;
            is_hx_deltaP_converge = false;
        }

        if (T_t_in_mode == C_sco2_cycle_core::E_SET_T_T_IN)
        {
            double diff_T_t_in_local = std::numeric_limits<double>::quiet_NaN();

            int test_code = 0;
            try
            {
                test_code = c_phx_cycle_solver.test_member_function(ms_phx_od_par.m_T_h_in, &diff_T_t_in_local);        //[K] Use hot HTF temp as turbine inlet temperature
            }
            catch (C_csp_exception)
            {
                eta_solved = 0.0;
                ms_od_solved.m_od_error_code = -1;
                ms_od_solved.m_is_converged = false;
                return ms_od_solved.m_od_error_code;
            }
            if (test_code != 0)
            {
                ms_od_solved.m_od_error_code = test_code;
                ms_od_solved.m_is_converged = false;
                return test_code;
            }
        }
        else if (T_t_in_mode == C_sco2_cycle_core::E_SOLVE_PHX)
        {
            // Set upper and lower bounds
            double T_t_upper = ms_phx_od_par.m_T_h_in;		//[K] Upper CO2 limit is HTF hot temperature
            double T_t_lower = 373.15;						//[K] Lower CO2 limit is something fairly low, I guess

            // Generate guess values
            double T_t_guess_upper = ms_phx_od_par.m_T_h_in - ms_des_par.m_phx_dt_hot_approach;	//[K] One reasonable guess might be to apply the design approach
            double T_t_guess_lower = T_t_guess_upper - 20.0;		//[K] This might be another reasonable guess...

            // Set solver settings
            // Because this application of solver is trying to get outlet to match guess, need to calculate error in function
            // So it's already relative, and solver is looking at an absolute value
            c_phx_cycle_solver.settings(ms_des_par.m_des_tol / 10.0, 50, T_t_lower, T_t_upper, false);

            // Now, solve for the turbine inlet temperature
            double T_t_solved, tol_solved;
            T_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
            int iter_solved = -1;

            int phx_cycle_code = 0;
            try
            {
                phx_cycle_code = c_phx_cycle_solver.solve(T_t_guess_lower, T_t_guess_upper, 0.0, T_t_solved, tol_solved, iter_solved);
            }
            catch (C_csp_exception)
            {
                eta_solved = 0.0;
                ms_od_solved.m_od_error_code = -1;
                ms_od_solved.m_is_converged = false;
                return ms_od_solved.m_od_error_code;
            }

            if (phx_cycle_code != C_monotonic_eq_solver::CONVERGED)
            {
                int n_call_history = (int)c_phx_cycle_solver.get_solver_call_history()->size();

                eta_solved = 0.0;

                int nested_error_code = (*(c_phx_cycle_solver.get_solver_call_history()))[n_call_history - 1].err_code;

                if (nested_error_code == 0)
                {
                    nested_error_code = phx_cycle_code;
                }

                ms_od_solved.m_od_error_code = nested_error_code;
                ms_od_solved.m_is_converged = false;
                return nested_error_code;
            }
        }

        double P_LTR_HP_out_calc = mpc_sco2_cycle->get_od_solved()->ms_LT_recup_od_solved.m_P_c_out;  //[kPa]
        diff_P_LTR_HP_out_calc_less_guess = P_LTR_HP_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::LTR_HP_OUT]; //[kPa]
        diff_P_LTR_HP_out_rel = diff_P_LTR_HP_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::LTR_HP_OUT]; //[-]

        double P_HTR_HP_out_calc = mpc_sco2_cycle->get_od_solved()->ms_HT_recup_od_solved.m_P_c_out;    //[kPa]
        diff_P_HTR_HP_out_calc_less_guess = P_HTR_HP_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_HP_OUT]; //[kPa]
        diff_P_HTR_HP_out_rel = diff_P_HTR_HP_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_HP_OUT]; //[-]

        double P_PHX_out_calc = mc_phx.ms_od_solved.m_P_c_out;      //[kPa]
        diff_P_PHX_out_calc_less_guess = P_PHX_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::TURB_IN];  //[kPa]
        diff_P_PHX_out_rel = diff_P_PHX_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::TURB_IN];  //[-]

        double P_HTR_LP_out_calc = mpc_sco2_cycle->get_od_solved()->ms_HT_recup_od_solved.m_P_h_out;    //[kPa]
        diff_P_HTR_LP_out_calc_less_guess = P_HTR_LP_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_LP_OUT];   //[kPa]
        diff_P_HTR_LP_out_rel = diff_P_HTR_LP_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_LP_OUT];   //[-]

        double P_LTR_LP_out_calc = mpc_sco2_cycle->get_od_solved()->ms_LT_recup_od_solved.m_P_h_out;    //[kPa]
        diff_P_LTR_LP_out_calc_less_guess = P_LTR_LP_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::LTR_LP_OUT];
        diff_P_LTR_LP_out_rel = diff_P_LTR_LP_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::LTR_LP_OUT];

        double W_dot_mc_cooler_fan_local, P_mc_cooler_out_calc;
        W_dot_mc_cooler_fan_local = P_mc_cooler_out_calc = std::numeric_limits<double>::quiet_NaN();
        mpc_sco2_cycle->solve_OD_mc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_mc_cooler_fan_local, P_mc_cooler_out_calc);
        diff_P_mc_cooler_out_calc_less_guess = P_mc_cooler_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN];
        diff_P_mc_cooler_out_rel = diff_P_mc_cooler_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_IN];
        
        if (ms_des_par.m_cycle_config == 2)
        {
            double W_dot_pc_cooler_fan_local, P_pc_cooler_out_calc;
            W_dot_pc_cooler_fan_local = P_pc_cooler_out_calc = std::numeric_limits<double>::quiet_NaN();
            mpc_sco2_cycle->solve_OD_pc_cooler_fan_power(ms_od_par.m_T_amb, od_tol, W_dot_pc_cooler_fan_local, P_pc_cooler_out_calc);
            diff_P_pc_cooler_out_calc_less_guess = P_pc_cooler_out_calc - mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::PC_IN];
            diff_P_pc_cooler_out_rel = diff_P_pc_cooler_out_calc_less_guess / mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::PC_IN];
        }

        iter_deltaP++;

    } while (is_hx_deltaP_converge && 
        ( fabs(diff_P_LTR_HP_out_rel) > od_tol
            || fabs(diff_P_HTR_HP_out_rel) > od_tol
            || fabs(diff_P_PHX_out_rel) > od_tol
            || fabs(diff_P_HTR_LP_out_rel) > od_tol
            || fabs(diff_P_LTR_LP_out_rel) > od_tol
            || fabs(diff_P_mc_cooler_out_rel) > od_tol
            || fabs(diff_P_pc_cooler_out_rel) > od_tol) );

    ms_od_solved.m_is_converged = true;

	// Now, need to filter results that exceed temperature/pressure/other limitations
	// 1) Don't let the turbine inlet temperature exceed the design inlet temperature
	double over_T_t_in = 0.0;
	// double over_T_t_in = max(0.0, T_t_solved - .get_design_solved()->m_temp[6-1]);

	// 2) Don't let the upper pressure in the system exceed the specified max (typically also = design point P_high)
	double over_P_high = max(0.0, (mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT] - 0.9999*ms_des_par.m_P_high_limit) / 1.E3);

	// 3) Check compressor(s) tip ratio?
	double mc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_mc_ms_od_solved.m_tip_ratio_max;
	// Recompressor has multiple stages, it's reporting the fastest tip speed
	double rc_w_tip_ratio = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		rc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_rc_ms_od_solved.m_tip_ratio_max;
	}
	double pc_w_tip_ratio = 0.0;
	if (ms_des_par.m_cycle_config == 2)
	{
		pc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_pc_ms_od_solved.m_tip_ratio_max;
	}
	double comp_tip_ratio = max(pc_w_tip_ratio, max(mc_w_tip_ratio, rc_w_tip_ratio));
	double over_tip_ratio = max(0.0, 10.0*(comp_tip_ratio - 0.999));

	// 4) Check for compressor(s) surge?
	// Main compressor
	double mc_phi = mpc_sco2_cycle->get_od_solved()->ms_mc_ms_od_solved.m_phi_min;
	double over_surge_mc = max(0.0, (mpc_sco2_cycle->get_design_solved()->ms_mc_ms_des_solved.m_phi_surge - mc_phi) / mpc_sco2_cycle->get_design_solved()->ms_mc_ms_des_solved.m_phi_surge*100.0);
	// Recompressor
	double over_surge_rc = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		double rc_phi_min = mpc_sco2_cycle->get_od_solved()->ms_rc_ms_od_solved.m_phi_min;
		over_surge_rc = max(0.0, (mpc_sco2_cycle->get_design_solved()->ms_rc_ms_des_solved.m_phi_surge - rc_phi_min) / mpc_sco2_cycle->get_design_solved()->ms_rc_ms_des_solved.m_phi_surge*100.0);
	}
	// Pre-compressor
	double over_surge_pc = 0.0;
	if (ms_des_par.m_cycle_config == 2)
	{
		double pc_phi_min = mpc_sco2_cycle->get_od_solved()->ms_pc_ms_od_solved.m_phi_min;
		over_surge_pc = max(0.0, (mpc_sco2_cycle->get_design_solved()->ms_pc_ms_des_solved.m_phi_surge - pc_phi_min) / mpc_sco2_cycle->get_design_solved()->ms_pc_ms_des_solved.m_phi_surge*100.0);
	}

	// Want thermal efficiency gradient, not step change, as turbine inlet temperature exceeds design
	// ... to help the solver
	double eta_T_t_in_scale = exp(-over_T_t_in);
	double eta_P_high_scale = exp(-over_P_high);
	double eta_tip_ratio_scale = exp(-over_tip_ratio);
	double eta_surge_mc_scale = exp(-over_surge_mc);
	double eta_surge_rc_scale = exp(-over_surge_rc);
	double eta_surge_pc_scale = exp(-over_surge_pc);

	int od_solve_code = 0;

	// If a problem with the solved operation
	//  then overwrite integer that code returns to calling program
	if(over_T_t_in != 0.0)
		od_solve_code = E_TURBINE_INLET_OVER_TEMP;
	else if(mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT] > ms_des_par.m_P_high_limit )
		od_solve_code = E_OVER_PRESSURE;
	else if (over_tip_ratio != 0.0)
		od_solve_code = E_TIP_RATIO;
	else if(over_surge_mc != 0.0)
		od_solve_code = E_MC_SURGE;
	else if(over_surge_rc != 0.0)
		od_solve_code = E_RC_SURGE;
	else if(over_surge_pc != 0.0)
		od_solve_code = E_PC_SURGE;

	ms_od_solved.m_od_error_code = od_solve_code;

	// Want to make an efficiency value available to the optimization although it may be decreased by system operation constraints
	if( !(od_solve_code == 0 || od_solve_code == E_TURBINE_INLET_OVER_TEMP || od_solve_code == E_OVER_PRESSURE ||
		od_solve_code == E_TIP_RATIO || od_solve_code == E_MC_SURGE || od_solve_code == E_RC_SURGE || od_solve_code == E_PC_SURGE) )
		return 0;


	return od_solve_code;
}

int C_sco2_phx_air_cooler::C_mono_eq_T_t_in::operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/)
{
	// Using:
	//	-mc_rc_cycle
	//	-ms_rc_cycle_od_par
	//	-ms_phx_od_par

	// 1) Update Turbine Inlet Temperature in sco2 cycle off design parameter structure
	mpc_sco2_rc->ms_cycle_od_par.m_T_t_in = T_t_in;

	// 2) Solve the off-design cycle model with off design parameter structure
	int rc_od_error_code = 0;
	
	try
	{
		rc_od_error_code = mpc_sco2_rc->mpc_sco2_cycle->off_design_fix_shaft_speeds(mpc_sco2_rc->ms_cycle_od_par, m_od_tol);
	}
	catch ( C_csp_exception )
	{
		// reset 'diff_T_t_in' to NaN
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	// If off-design cycle model did not solve, return to solver
	if( rc_od_error_code != 0 )
	{
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		return rc_od_error_code;
	}

    double T_co2_phx_out = std::numeric_limits<double>::quiet_NaN();
    if (m_T_t_in_mode == C_sco2_cycle_core::E_SOLVE_PHX)
    {
        // Solve PHX heat exchanger performance using CO2 and HTF *inlet* conditions
        mpc_sco2_rc->ms_phx_od_par.m_T_c_in = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_temp[C_sco2_cycle_core::HTR_HP_OUT];	//[K]
        mpc_sco2_rc->ms_phx_od_par.m_P_c_in = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_HP_OUT];	//[kPa]
        mpc_sco2_rc->ms_phx_od_par.m_m_dot_c = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_m_dot_t;		//[kg/s]
        double P_c_out = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::TURB_IN];		//[kPa]
        double q_dot, T_htf_cold;
        q_dot = T_htf_cold = std::numeric_limits<double>::quiet_NaN();

        // Solves HX performance. 
        // If successful, this call updates 'ms_od_solved'
        try
        {
            mpc_sco2_rc->mc_phx.off_design_solution_fixed_dP(mpc_sco2_rc->ms_phx_od_par.m_T_c_in, mpc_sco2_rc->ms_phx_od_par.m_P_c_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_c, P_c_out,
                mpc_sco2_rc->ms_phx_od_par.m_T_h_in, mpc_sco2_rc->ms_phx_od_par.m_P_h_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_h, mpc_sco2_rc->ms_phx_od_par.m_P_h_in,
                m_od_tol,
                q_dot, T_co2_phx_out, T_htf_cold);
        }
        catch (C_csp_exception)
        {
            // reset 'diff_T_t_in' to NaN
            *diff_T_t_in = std::numeric_limits<double>::quiet_NaN();

            return -1;
        }
    }
    else if (m_T_t_in_mode == C_sco2_cycle_core::E_SET_T_T_IN)
    {
        mpc_sco2_rc->ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();
        mpc_sco2_rc->ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();
        mpc_sco2_rc->ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();

        T_co2_phx_out = mpc_sco2_rc->ms_cycle_od_par.m_T_t_in;      //[K]
    }
	
	*diff_T_t_in = (T_co2_phx_out - T_t_in) / T_t_in;       //[-]
	return 0;
}

int C_sco2_phx_air_cooler::C_MEQ__P_LP_in__T_htf_cold_target::operator()(double P_LP_in /*kPa*/, double *T_htf_cold /*K*/)
{
    mpc_sco2_cycle->ms_cycle_od_par.m_P_LP_comp_in = P_LP_in;	//[kPa]	

    double f_obj_max = std::numeric_limits<double>::quiet_NaN();

    try
    {
        mpc_sco2_cycle->off_design_core(f_obj_max, m_od_tol);
    }
    catch (C_csp_exception&)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *T_htf_cold = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }
    catch (...)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *T_htf_cold = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    if (!mpc_sco2_cycle->ms_od_solved.m_is_converged)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *T_htf_cold = std::numeric_limits<double>::quiet_NaN();
        return -3;
    }

    *T_htf_cold = mpc_sco2_cycle->mc_phx.ms_od_solved.m_T_h_out;      //[K]

    mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, *T_htf_cold, mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT],
        mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

    return 0;
}

int C_sco2_phx_air_cooler::C_MEQ__P_LP_in__W_dot_target::operator()(double P_LP_in /*kPa*/, double *W_dot /*kWe*/)
{
    mpc_sco2_cycle->ms_cycle_od_par.m_P_LP_comp_in = P_LP_in;	//[kPa]	

    double f_obj_max = std::numeric_limits<double>::quiet_NaN();

    try
    {
        mpc_sco2_cycle->off_design_core(f_obj_max, m_od_tol);
    }
    catch (C_csp_exception &)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);
            
        *W_dot = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }
    catch (...)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *W_dot = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    if (!mpc_sco2_cycle->ms_od_solved.m_is_converged)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *W_dot = std::numeric_limits<double>::quiet_NaN();
        return -3;
    }    

    *W_dot = mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_W_dot_net;  //[kWe]

    mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, *W_dot, mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT],
        mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

    return 0;
}

int C_sco2_phx_air_cooler::C_MEQ__P_LP_in__P_mc_out_target::operator()(double P_LP_in /*kPa*/, double *P_mc_out /*kPa*/)
{
    mpc_sco2_cycle->ms_cycle_od_par.m_P_LP_comp_in = P_LP_in;	//[kPa]	

    double f_obj_max = std::numeric_limits<double>::quiet_NaN();

    try
    {
        mpc_sco2_cycle->off_design_core(f_obj_max, m_od_tol);
    }
    catch (C_csp_exception &)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }
    catch (...)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    if (!mpc_sco2_cycle->ms_od_solved.m_is_converged)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -3;
    }

    *P_mc_out = mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT];     //[kPa]

    mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_W_dot_net, *P_mc_out,
        mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

    return 0;
}

int C_sco2_phx_air_cooler::C_MEQ__P_LP_in__max_no_err_code::operator()(double P_LP_in /*kPa*/, double *P_mc_out /*kPa*/)
{
    mpc_sco2_cycle->ms_cycle_od_par.m_P_LP_comp_in = P_LP_in;	//[kPa]	

    double f_obj_max = std::numeric_limits<double>::quiet_NaN();

    try
    {
        mpc_sco2_cycle->off_design_core(f_obj_max, m_od_tol);
    }
    catch (C_csp_exception &)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -1;
    }
    catch (...)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    if (!mpc_sco2_cycle->ms_od_solved.m_is_converged || mpc_sco2_cycle->ms_od_solved.m_od_error_code != 0)
    {
        mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
            mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

        *P_mc_out = std::numeric_limits<double>::quiet_NaN();
        return -3;
    }

    *P_mc_out = mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT];     //[kPa]

    mpc_sco2_cycle->mc_P_LP_in_iter_tracker.push_back_vectors(P_LP_in, mpc_sco2_cycle->mpc_sco2_cycle->get_od_solved()->m_W_dot_net, *P_mc_out,
        mpc_sco2_cycle->ms_od_solved.m_od_error_code, mpc_sco2_cycle->ms_od_solved.m_is_converged);

    return 0;
}

int C_sco2_phx_air_cooler::C_sco2_csp_od::operator()(S_f_inputs inputs, S_f_outputs & outputs)
{
	S_od_par sco2_od_par;
	sco2_od_par.m_T_htf_hot = inputs.m_T_htf_hot + 273.15;	//[K] convert from C
	sco2_od_par.m_m_dot_htf = mpc_sco2_rc->get_phx_des_par()->m_m_dot_hot_des*inputs.m_m_dot_htf_ND;	//[kg/s] scale from [-]
	sco2_od_par.m_T_amb = inputs.m_T_amb + 273.15;			//[K] convert from C
    sco2_od_par.m_T_t_in_mode = C_sco2_cycle_core::E_SOLVE_PHX; //[-]

	C_sco2_phx_air_cooler::E_off_design_strategies od_strategy = C_sco2_phx_air_cooler::E_TARGET_POWER_ETA_MAX;

	int off_design_code = -1;	//[-]

	try
	{
		off_design_code = mpc_sco2_rc->off_design__constant_N__T_mc_in_P_LP_in__objective(sco2_od_par,
                                                    true, 1.0,
                                                    true, 1.0,
                                                    true, 1.0,
                                                    false, std::numeric_limits<double>::quiet_NaN(),
                                                    od_strategy, m_od_opt_tol, m_od_tol);
	}
	catch (C_csp_exception &)
	{
		return -1;
	}
	// Cycle off-design may want to operate below this value, so ND value could be < 1 everywhere
	double W_dot_gross_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net;	//[kWe]
	double Q_dot_in_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net
								/ mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_eta_thermal;	//[kWt]
	double W_dot_cooler_tot_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_cooler_tot;	//[kWe]

	outputs.m_W_dot_gross_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net
								/ W_dot_gross_design;

	outputs.m_Q_dot_in_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_Q_dot
								/ Q_dot_in_design;

    double W_dot_mc_cooler = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_air_cooler_od_solved.m_W_dot_fan;    //[MWe]
    double W_dot_pc_cooler = 0.0;
    if (mpc_sco2_rc->ms_des_par.m_cycle_config == 2) {
        W_dot_pc_cooler = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_air_cooler_od_solved.m_W_dot_fan;       //[MWe]
    }

	outputs.m_W_dot_cooling_ND = (W_dot_mc_cooler + W_dot_pc_cooler)
								/ W_dot_cooler_tot_design;	
	
	//outputs.m_W_dot_cooling_ND = outputs.m_W_dot_gross_ND;

	outputs.m_m_dot_water_ND = 1.0;	

	return off_design_code;
}

int C_sco2_phx_air_cooler::generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
	double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
	double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
	util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind,
    double od_opt_tol /*-*/, double od_tol /*-*/)
{
	C_sco2_csp_od c_sco2_csp(this, od_opt_tol, od_tol);
	C_ud_pc_table_generator c_sco2_ud_pc(c_sco2_csp);

	c_sco2_ud_pc.mf_callback = mf_callback_update;
	c_sco2_ud_pc.mp_mf_active = mp_mf_update;

	double T_htf_ref = ms_des_par.m_T_htf_hot_in - 273.15;	//[C] convert from K
	double T_amb_ref = ms_des_par.m_T_amb_des - 273.15;		//[C] convert from K
	double m_dot_htf_ND_ref = 1.0;							//[-]

	int ud_pc_error_code = c_sco2_ud_pc.generate_tables(T_htf_ref, T_htf_low, T_htf_high, n_T_htf,
								T_amb_ref, T_amb_low, T_amb_high, n_T_amb,
								m_dot_htf_ND_ref, m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND,
								T_htf_ind, T_amb_ind, m_dot_htf_ND_ind);

	return ud_pc_error_code;
}

const C_sco2_phx_air_cooler::S_des_par * C_sco2_phx_air_cooler::get_design_par()
{
	return &ms_des_par;
}

const C_sco2_phx_air_cooler::S_des_solved * C_sco2_phx_air_cooler::get_design_solved()
{
	return &ms_des_solved;
}

const C_HX_counterflow_CRM::S_des_calc_UA_par * C_sco2_phx_air_cooler::get_phx_des_par()
{
	return &ms_phx_des_par;
}

const C_sco2_phx_air_cooler::S_od_solved * C_sco2_phx_air_cooler::get_od_solved()
{
	return &ms_od_solved;
}

const C_sco2_phx_air_cooler::S_od_par* C_sco2_phx_air_cooler::get_od_par()
{
    return &ms_od_par;
}

double C_sco2_phx_air_cooler::opt_P_LP_in__fixed_N_turbo__return_f_obj(double P_mc_in /*kPa*/, double od_tol /*-*/)
{
	m_nlopt_iter++;
	
	ms_cycle_od_par.m_P_LP_comp_in = P_mc_in;	//[kPa]	

	double f_obj_max = std::numeric_limits<double>::quiet_NaN();

	try
	{
		off_design_core(f_obj_max, od_tol);
	}
	catch (C_csp_exception &)
	{
		return 0.0;
	}
	catch (...)
	{
		return 0.0;
	}

	if( !std::isfinite(f_obj_max) )
	{
		f_obj_max = 0.0;
	}

	return f_obj_max;
}

double fmin_opt_T_mc_in__max_net_power_less_cooling(double x, void* data)
{
    C_sco2_phx_air_cooler::S_to_W_net_less_cooling_max* frame = static_cast<C_sco2_phx_air_cooler::S_to_W_net_less_cooling_max*>(data);

    frame->mpc_sco2_phx_air_cooler->set_ms_od_par_T_mc_in(x);
    frame->mpc_sco2_phx_air_cooler->solve_P_LP_in__objective(frame->m_od_opt_obj, *frame->m_call_tracker, frame->od_tol);

    return -(*(frame->m_call_tracker->end() - 1)).m_objective;
}

double nlopt_opt_T_pc_in__max_net_power_less_cooling(const std::vector<double>& x, std::vector<double>& grad, void* data)
{
    C_sco2_phx_air_cooler::S_to_off_design__calc_T_mc_in* frame = static_cast<C_sco2_phx_air_cooler::S_to_off_design__calc_T_mc_in*>(data);

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> P_LP_in__tracker;
    try
    {
        frame->mpc_sco2_phx_air_cooler->off_design__calc_T_mc_in__target_T_htf_cold__max_power(frame->od_par,
            x[0],
            frame->is_rc_N_od_at_design, frame->rc_N_od_f_des,
            frame->is_mc_N_od_at_design, frame->mc_N_od_f_des,
            frame->is_pc_N_od_at_design, frame->pc_N_od_f_des,
            frame->is_PHX_dP_input, frame->PHX_f_dP,
            P_LP_in__tracker,
            frame->od_opt_tol, frame->od_tol);
    }
    catch (C_csp_exception&)
    {
        return 0.0;
    }
    catch (...)
    {
        return 0.0;
    }

    frame->pv_T_mc_in_call_tracker->push_back(*(P_LP_in__tracker.end() - 1));
    return (*(P_LP_in__tracker.end() - 1)).m_objective;
}

double nlopt_cb_opt_N_mc_rc(const std::vector<double>& x, std::vector<double>& grad, void* data)
{
    C_sco2_phx_air_cooler::C_to_N_mc_rc_opt* frame = static_cast<C_sco2_phx_air_cooler::C_to_N_mc_rc_opt*>(data);

    // Get local 'x' and translate to shaft speed guess
    double is_rc_N_od_at_design = frame->m_is_rc_N_od_at_design;
    double is_mc_N_od_at_design = frame->m_is_mc_N_od_at_design;
    double is_pc_N_od_at_design = frame->m_is_pc_N_od_at_design;
    double rc_N_od_f_des = 1.0;
    double mc_N_od_f_des = 1.0;
    double pc_N_od_f_des = 1.0;
    if (frame->mpc_sco2_phx_air_cooler->get_design_par()->m_cycle_config == 2) {
        int i = 0;
        if (frame->m_is_N_mc_opt) {
            is_mc_N_od_at_design = false;
            mc_N_od_f_des = x[i];
            i++;
        }
        if (frame->m_is_N_rc_opt) {
            is_rc_N_od_at_design = false;
            rc_N_od_f_des = x[i];
            i++;
        }
        if (frame->m_is_N_pc_opt) {
            is_pc_N_od_at_design = false;
            pc_N_od_f_des = x[i];
        }
    }
    else {
        int i = 0;
        if (frame->m_is_N_mc_opt) {
            is_mc_N_od_at_design = false;
            mc_N_od_f_des = x[i];
            i++;
        }
        if (frame->m_is_N_rc_opt && frame->mpc_sco2_phx_air_cooler->get_design_solved()->ms_rc_cycle_solved.m_is_rc) {
            is_rc_N_od_at_design = false;
            rc_N_od_f_des = x[i];
        }
    }

    std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> v_T_mc_in__tracker;
    try
    {
        frame->mpc_sco2_phx_air_cooler->off_design__calc_T_pc_in__target_T_htf_cold__max_power(frame->m_od_par,
            is_rc_N_od_at_design, rc_N_od_f_des,
            is_mc_N_od_at_design, mc_N_od_f_des,
            is_pc_N_od_at_design, pc_N_od_f_des,
            frame->m_is_PHX_dP_input, frame->m_PHX_f_dP,
            frame->m_od_opt_tol, frame->m_od_tol,
            v_T_mc_in__tracker);
    }
    catch (C_csp_exception&)
    {
        return 0.0;
    }
    catch (...)
    {
        return 0.0;
    }    

    return (*(v_T_mc_in__tracker.end() - 1)).m_objective;
}

bool SortByTmcin(const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& lhs,
    const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& rhs)
{
    return lhs.m_T_mc_in < rhs.m_T_mc_in;
}

bool SortByTpcin(const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& lhs,
    const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& rhs)
{
    return lhs.m_T_pc_in < rhs.m_T_pc_in;
}

