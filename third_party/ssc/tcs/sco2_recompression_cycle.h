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

#ifndef __SCO2_PC_CORE_
#define __SCO2_PC_CORE_

#include "sco2_cycle_components.h"
#include "sco2_cycle_templates.h"

#include <limits>
#include <vector>
#include <algorithm>
#include <string>
#include <math.h>
#include "CO2_properties.h"

#include "heat_exchangers.h"

#include "numeric_solvers.h"

using namespace std;

class C_RecompCycle : public C_sco2_cycle_core
{
public:

	//struct S_design_limits
	//{
	//	double m_UA_net_power_ratio_max;		//[-/K]
	//	double m_UA_net_power_ratio_min;		//[-/K]

	//	double m_T_mc_in_min;					//[K]

	//	S_design_limits()
	//	{
	//		m_UA_net_power_ratio_max = m_UA_net_power_ratio_min = std::numeric_limits<double>::quiet_NaN();
	//	}
	//};

	struct S_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		double m_P_mc_in;					//[kPa] Compressor inlet pressure
		double m_P_mc_out;					//[kPa] Compressor outlet pressure
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		    // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
		double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        int m_LTR_N_sub_hxrs;               //[-] Number of sub-hxs to use in hx model
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
        // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        int m_HTR_N_sub_hxrs;               //[-] Number of sub-hxs to use in hx model
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
        //
        double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
        int m_mc_comp_model_code;           //[-] Main compressor model - see sco2_cycle_components.h 
        double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
        int m_rc_comp_model_code;           //[-] Recompressor model - see sco2_cycle_components.h 
        double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		//int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_des_tol;						//[-] Convergence tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		// Air cooler parameters
		bool m_is_des_air_cooler;		//[-] False will skip physical air cooler design. UA will not be available for cost models.
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;	//[-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
		double m_T_amb_des;				//[K] Design point ambient temperature
		double m_elevation;				//[m] Elevation (used to calculate ambient pressure)
        double m_eta_fan;               //[-] Fan isentropic efficiency
        int m_N_nodes_pass;             //[-] Number of nodes per pass

		int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;		//[C]

		S_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_P_mc_in = m_P_mc_out = 
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max = 
                m_recomp_frac = 
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_des_tol = m_N_turbine =
				m_frac_fan_power = m_deltaP_cooler_frac =
                m_eta_fan = m_T_amb_des = m_elevation = std::numeric_limits<double>::quiet_NaN();
			m_LTR_N_sub_hxrs = m_HTR_N_sub_hxrs = -1;

            // Compressor model codes
            m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;


            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_compA__P85_T37;
            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_compA__P85_T32;
            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_compA__P80_T37;
            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_compA__P80_T32;
            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_compA__interpolate;



            //m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;


            m_rc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;


            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			// Air cooler default
			m_is_des_air_cooler = true;
            m_N_nodes_pass = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_opt_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		    // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
        double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        int m_LTR_N_sub_hxrs;               //[-] Number of sub-hxs used to model hx
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
            // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        int m_HTR_N_sub_hxrs;               //[-] Number of sub-hxs used to model hx
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
            //
        double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
        int m_mc_comp_model_code;           //[-] Recompressor model - see sco2_cycle_components.h 
        double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_des_tol;					//[-] Convergence tolerance
		double m_des_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		
		// Air cooler parameters
		bool m_is_des_air_cooler;		//[-] False will skip physical air cooler design. UA will not be available for cost models.
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;	//[-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
		double m_T_amb_des;				//[K] Design point ambient temperature
		double m_elevation;				//[m] Elevation (used to calculate ambient pressure)
        double m_eta_fan;               //[-] Fan isentropic efficiency
        int m_N_nodes_pass;             //[-] Number of nodes per pass

		int m_des_objective_type;			//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;			//[C]

		double m_P_mc_out_guess;			//[kPa] Initial guess for main compressor outlet pressure
		bool m_fixed_P_mc_out;				//[-] if true, P_mc_out is fixed at P_mc_out_guess
		
		double m_PR_HP_to_LP_guess;         //[-] Initial guess for ratio of P_mc_out to P_LP_in
		bool m_fixed_PR_HP_to_LP;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		double m_recomp_frac_guess;			//[-] Initial guess for design-point recompression fraction
		bool m_fixed_recomp_frac;			//[-] if true, recomp_frac is fixed at recomp_frac_guess

		double m_LT_frac_guess;				//[-] Initial guess for fraction of UA_rec_total that is in the low-temperature recuperator
		bool m_fixed_LT_frac;				//[-] if true, LT_frac is fixed at LT_frac_guess

		

		S_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_UA_rec_total = 
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max = 
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_des_tol = m_des_opt_tol = m_N_turbine =
				m_frac_fan_power = m_deltaP_cooler_frac = m_T_amb_des = m_elevation =
				m_P_mc_out_guess = m_PR_HP_to_LP_guess = m_recomp_frac_guess = m_LT_frac_guess =
                m_eta_fan = std::numeric_limits<double>::quiet_NaN();

			m_LTR_N_sub_hxrs = m_HTR_N_sub_hxrs = -1;

            m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;

            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

			// Air cooler default
			m_is_des_air_cooler = true;
            m_N_nodes_pass = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_od_turbo_bal_csp_par
	{
		double m_P_mc_in;	//[kPa] Main compressor inlet pressure
		double m_f_recomp;	//[-] Recompression fraction
		double m_T_mc_in;	//[K] Main compressor inlet temperature
		double m_T_t_in;	//[K] Turbine inlet temperature
		double m_phi_mc;	//[-] Main compressor flow coefficient

		double m_co2_to_htf_m_dot_ratio_des;	//[-] m_dot_co2 / m_dot_htf at design
		double m_m_dot_htf;						//[kg/s] (off design) HTF mass flow rate 

		S_od_turbo_bal_csp_par()
		{
			m_P_mc_in = m_f_recomp = m_T_mc_in = m_T_t_in = m_phi_mc =
				m_co2_to_htf_m_dot_ratio_des = m_m_dot_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_turbo_bal_csp_solved
	{
		S_od_turbo_bal_csp_par ms_par;

		bool m_is_feasible;			//[-] Did a set of parameters result in a solution that satisfied constraints

		double m_W_dot_net;			//[kWe] W_t - W_mc - W_rc
		double m_W_dot_net_adj;		//[kWe] Adjusted with derates for constraints
		double m_P_high;			//[kPa] Upper pressure in cycle (not considering pressure drops)
		double m_m_dot_total;		//[kg/s] CO2 mass flow rate through turbine & PHX

		double m_N_mc;				//[rpm] Main compressor speed required to hit phi_des
		double m_w_tip_ratio_mc;	//[-] Main compressor tip speed over speed of sound
		double m_eta_mc;			//[-] Main compressor isentropic efficiency
	
		double m_N_rc;				//[rpm] Recompressor speed required to supply m_dot_rc
		double m_phi_rc_1;			//[-] Recompressor flow coefficient, stage 1
		double m_phi_rc_2;			//[-] Recompressor flow coefficient, stage 2
		double m_w_tip_ratio_rc;	//[-] Recompressor tip seed over speed of sound (max of 2 stages)
		double m_eta_rc;			//[-] Recompressor isentropic efficiency

		double m_eta_t;				//[-] Turbine isentropic efficiency

		S_od_turbo_bal_csp_solved()
		{
			m_is_feasible = false;
			
			m_W_dot_net = m_W_dot_net_adj = m_P_high = m_m_dot_total =
				m_N_mc = m_w_tip_ratio_mc = m_eta_mc = 
				m_N_rc = m_phi_rc_1 = m_phi_rc_2 = m_w_tip_ratio_rc = m_eta_rc =
				m_eta_t = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		double m_tol;			//[-] Convergence tolerance

		S_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in = m_recomp_frac = m_N_mc = m_N_t = m_tol = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_opt_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		bool m_is_max_W_dot;	//[-] Value to maximize: true = W_dot, false = eta

		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers

		double m_P_mc_in_guess;	//[kPa] Initial guess for P_mc_in when iterating to hit target
		bool m_fixed_P_mc_in;	//[-] if true, P_mc_in is fixed at P_mc_in_guess

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		S_opt_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in_guess = m_recomp_frac_guess = m_N_mc_guess =
				m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_fixed_P_mc_in = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = false;
		}
	};

	struct S_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance
		
		double m_target;		//[kW] type of target: 1) W_dot 2) Q_dot_PHX
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		double m_lowest_pressure;	//[-] lowest pressure to check
		double m_highest_pressure;	//[-] highest pressure to check
		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_recomp_frac = m_N_mc = m_N_t = m_tol =
				m_target = m_lowest_pressure = m_highest_pressure = std::numeric_limits<double>::quiet_NaN();

			m_is_target_Q = true;

			m_N_sub_hxrs = -1;

			m_use_default_res = true;
		}
	};

	struct S_opt_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		double m_target;		//[kW] target value
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		int m_N_sub_hxrs;				//[-] Number of sub heat exchangers
		double m_lowest_pressure;		//[-] lowest pressure to check
		double m_highest_pressure;		//[-] highest pressure to check

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_opt_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_target = m_lowest_pressure = m_highest_pressure = m_recomp_frac_guess =
				m_N_mc_guess = m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;
			
			m_is_target_Q = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = true;

			m_use_default_res = true;
		}
	};

	struct S_PHX_od_parameters
	{
		double m_m_dot_htf_des;		//[kg/s] Design point htf mass flow rate
		
		double m_T_htf_hot;			//[K] Current htf inlet temperature
		double m_m_dot_htf;			//[kg/s] Current htf mass flow rate
		double m_T_htf_cold;		//[K] Target htf cold return temp

		double m_UA_PHX_des;		//[kW/K] Design point PHX conductance

		double m_cp_htf;			//[kW/K] Constant HTF specific heat

		S_PHX_od_parameters()
		{
			m_m_dot_htf_des = m_T_htf_hot = m_m_dot_htf = m_T_htf_cold = m_UA_PHX_des = m_cp_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
		// Component classes
	C_turbine m_t;
	C_comp_multi_stage m_mc_ms;
	C_comp_multi_stage m_rc_ms;
	C_HeatExchanger m_PHX, m_PC;
	
    C_HX_co2_to_co2_CRM mc_LT_recup;
    C_HX_co2_to_co2_CRM mc_HT_recup;

	C_CO2_to_air_cooler mc_air_cooler;
	
		// Input/Ouput structures for class methods
	//S_design_limits ms_des_limits;
	S_design_parameters ms_des_par;
	S_opt_design_parameters ms_opt_des_par;
	
	//S_od_turbo_bal_par ms_od_turbo_bal_par;
	S_od_turbo_bal_csp_par ms_od_turbo_bal_csp_par;
	S_od_turbo_bal_csp_solved ms_od_turbo_bal_csp_solved;
	//S_od_parameters ms_od_par;
	S_opt_od_parameters ms_opt_od_par;
	S_target_od_parameters ms_tar_od_par;
	S_opt_target_od_parameters ms_opt_tar_od_par;
	S_PHX_od_parameters ms_phx_od_par;

		// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_calc_last;
	double m_W_dot_net_last;
	double m_m_dot_mc, m_m_dot_rc, m_m_dot_t;
	double m_Q_dot_PHX, m_Q_dot_bypass, m_eta_bypass;
	double m_W_dot_mc, m_W_dot_rc, m_W_dot_t, m_W_dot_mc_bypass;
	double m_objective_metric_last;
	
		// Structures and data for optimization
	S_design_parameters ms_des_par_optimal;
	double m_objective_metric_opt;

		// Structures and data for auto-optimization
	double m_objective_metric_auto_opt;	
	S_design_parameters ms_des_par_auto_opt;

		// Results from last off-design solution
	std::vector<double> m_temp_od, m_pres_od, m_enth_od, m_entr_od, m_dens_od;					// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_od;
	double m_W_dot_net_od;
	double m_Q_dot_PHX_od;
    double m_Q_dot_mc_cooler_od;    //[MWt]

		// Structures and data for off-design optimization
	S_od_parameters ms_od_par_optimal;
	double m_W_dot_net_max;

		// Structures and data for optimal target off-design
	S_od_parameters ms_od_par_tar_optimal;
	double m_eta_best;
	double m_biggest_target;

		// New opt
	bool m_found_opt;
	double m_eta_phx_max;
	double m_UA_diff_eta_max;
	double m_over_deltaP_eta_max;

	void design_core(int & error_code);	

	void design_core_standard(int & error_code);
	
	//void design_core_bypass(int & error_code);

	//void design_core_bypass150C(int & error_code);

	//void design_core_HTR_hs(int & error_code);

	void opt_design_core(int & error_code);

	void auto_opt_design_core(int & error_code);

	void finalize_design(int & error_code);	

	//void off_design_core(int & error_code);

	//void off_design_phi_core(int & error_code);

	void off_design_fix_shaft_speeds_core(int & error_code, double od_tol /*-*/);
	
	//void optimal_off_design_core(int & error_code);

	//void target_off_design_core(int & error_code);	

	//void clear_ms_od_solved();

public:

	C_RecompCycle()
	{
		m_temp_last.resize(END_SCO2_STATES);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

		m_eta_thermal_calc_last = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_PHX = m_Q_dot_bypass = m_eta_bypass = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_mc = m_W_dot_rc = m_W_dot_t = m_W_dot_mc_bypass = std::numeric_limits<double>::quiet_NaN();
		m_objective_metric_last = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_last = std::numeric_limits<double>::quiet_NaN();

		m_objective_metric_opt = std::numeric_limits<double>::quiet_NaN();
		m_objective_metric_auto_opt = std::numeric_limits<double>::quiet_NaN();

		m_temp_od = m_pres_od = m_enth_od = m_entr_od = m_dens_od = m_temp_last;

		m_eta_thermal_od = m_W_dot_net_od = m_Q_dot_PHX_od = m_Q_dot_mc_cooler_od = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_max = m_eta_best = m_biggest_target = std::numeric_limits<double>::quiet_NaN();

		m_found_opt = false;

		m_eta_phx_max = m_over_deltaP_eta_max = m_UA_diff_eta_max = std::numeric_limits<double>::quiet_NaN();

		// Set design limits!!!!
		//ms_des_limits.m_UA_net_power_ratio_max = 2.0;		//[-/K]
		//ms_des_limits.m_UA_net_power_ratio_min = 1.E-5;		//[-/K]
		//
		//// Set minimum main compressor inlet temperature
		//CO2_info s_co2_info;
		//
		//get_CO2_info(&s_co2_info);
		//
		//ms_des_limits.m_T_mc_in_min = ceil(s_co2_info.T_critical);		//[K]
	}

	CO2_state mc_co2_props;

	~C_RecompCycle(){}

	void design(S_design_parameters & des_par_in, int & error_code);

	void opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code);

	//void od_turbo_bal_csp(const S_od_turbo_bal_csp_par & par_in);

	//double od_turbo_bal_csp_Wnet(const std::vector<double> &x);
	
	void reset_ms_od_turbo_bal_csp_solved();

	//void optimize_od_turbo_balance_csp(S_od_turbo_bal_csp_par in_params, std::vector<double> &opt_params);

	int auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in);

	int auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, string & error_msg);

	//void off_design(S_od_parameters & od_par_in, int & error_code);

	//void off_design_phi(S_od_phi_par & od_phi_par_in, int & error_code);

	int off_design_fix_shaft_speeds(S_od_par & od_phi_par_in, double od_tol /*-*/);

	virtual int solve_OD_all_coolers_fan_power(double T_amb /*K*/, double od_tol /*-*/, double & W_dot_fan /*MWe*/);

    virtual int solve_OD_mc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/, double & W_dot_mc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/);

    virtual int solve_OD_pc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/, double & W_dot_pc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/);

	//void optimal_off_design(S_opt_od_parameters & opt_od_par_in, int & error_code);
	
	//void get_max_output_od(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	//void target_off_design(S_target_od_parameters & tar_od_par_in, int & error_code);

	//void optimal_target_off_design(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	//void optimal_target_off_design_no_check(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	//void opt_od_eta_for_hx(S_od_parameters & od_par_in, S_PHX_od_parameters phx_od_par_in, int & error_code);

	double get_od_temp(int n_state_point);

	double get_od_pres(int n_state_point);
	
    virtual void check_od_solution(double & diff_m_dot, double & diff_E_cycle,
        double & diff_Q_LTR, double & diff_Q_HTR);

	void set_od_temp(int n_state_point, double temp_K);

	void set_od_pres(int n_state_point, double pres_kPa);

	void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, double tol /*-*/, int & error_code, double & T_out);

	void estimate_od_turbo_operation(double T_mc_in /*K*/, double P_mc_in /*kPa*/, double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
							int & mc_error_code, double & mc_w_tip_ratio /*-*/, double & P_mc_out /*kPa*/,
							int & rc_error_code, double & rc_w_tip_ratio /*-*/, double & rc_phi /*-*/,
							bool is_update_ms_od_solved = false);

	const C_comp_multi_stage::S_od_solved * get_rc_od_solved()
	{
		return m_rc_ms.get_od_solved();
	}

	const S_od_turbo_bal_csp_solved *get_od_turbo_bal_csp_solved()
	{
		return &ms_od_turbo_bal_csp_solved;
	}

	double get_max_target()
	{
		return m_biggest_target;
	}

	/*const S_design_limits & get_design_limits()
	{
		return ms_des_limits;
	}*/

	class C_mono_eq_x_f_recomp_y_N_rc : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_T_t_in;		//[K] Turbine inlet temperature

        double m_f_mc_bypass;   //[-] Fraction of main compressor bypassed to cooler
        double m_od_tol;

	public:
		
		double m_m_dot_t;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_mc;		//[kg/s]
        double m_m_dot_LTR_HP;  //[kg/s]

		C_mono_eq_x_f_recomp_y_N_rc(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, 
            double P_mc_in /*kPa*/, double T_t_in /*K*/,
            double f_mc_bypass, double od_tol)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_T_mc_in = T_mc_in;		//[K]
			m_P_mc_in = P_mc_in;		//[kPa]
			m_T_t_in = T_t_in;			//[K]
            m_f_mc_bypass = f_mc_bypass;    //[-]
            m_od_tol = od_tol;          //[-]

			m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_m_dot_LTR_HP = std::numeric_limits<double>::quiet_NaN();
		}

		virtual int operator()(double f_recomp /*-*/, double *N_rc /*rpm*/);

		CO2_state mc_co2_props;
	};

	class C_mono_eq_turbo_N_fixed_m_dot : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_f_recomp;		//[-] Recompression fraction
		double m_T_t_in;		//[K] Turbine inlet temperature

        double m_f_mc_bypass;   //[-] Fraction of main compressor bypassed to cooler

		bool m_is_update_ms_od_solved;	//[-] Bool to update member structure ms_od_solved
		// that is typically updated after entire cycle off-design solution 

	public:
		C_mono_eq_turbo_N_fixed_m_dot(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, double P_mc_in /*kPa*/,
			double f_recomp /*-*/, double T_t_in /*K*/, 
            double f_mc_bypass /*-*/,
            bool is_update_ms_od_solved = false)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_T_mc_in = T_mc_in;			//[K]
			m_P_mc_in = P_mc_in;			//[kPa]
			m_f_recomp = f_recomp;			//[-]
			m_T_t_in = T_t_in;				//[K]

            m_f_mc_bypass = f_mc_bypass;    //[-]

			m_is_update_ms_od_solved = is_update_ms_od_solved;

            m_m_dot_mc = m_m_dot_LTR_HP = std::numeric_limits<double>::quiet_NaN();
		}

        double m_m_dot_mc;      //[kg/s]
        double m_m_dot_LTR_HP;  //[kg/s]

		virtual int operator()(double m_dot_t /*kg/s*/, double *diff_m_dot_t /*-*/);

		CO2_state mc_co2_props;	
	};

	//class C_mono_eq_turbo_m_dot : public C_monotonic_equation
	//{
	//private:
	//	C_RecompCycle *mpc_rc_cycle;
	//
	//	double m_T_mc_in;		//[K] Compressor inlet temperature
	//	double m_P_mc_in;		//[kPa] Compressor inlet pressure
	//	double m_f_recomp;		//[-] Recompression fraction
	//	double m_T_t_in;		//[K] Turbine inlet temperature
	//	double m_phi_mc;		//[-] Compressor flow coefficient
	//
	//	bool m_is_update_ms_od_solved;	//[-] Bool to update member structure ms_od_solved
	//									// that is typically updated after entire cycle off-design solution 
	//
	//public:
	//	C_mono_eq_turbo_m_dot(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, double P_mc_in /*kPa*/,
	//								double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
	//								bool is_update_ms_od_solved = false)
	//	{
	//		mpc_rc_cycle = pc_rc_cycle;
	//		m_T_mc_in = T_mc_in;		//[K]
	//		m_P_mc_in = P_mc_in;		//[kPa]
	//		m_f_recomp = f_recomp;		//[-]
	//		m_T_t_in = T_t_in;			//[K]
	//		m_phi_mc = phi_mc;			//[-]
	//
	//		m_is_update_ms_od_solved = is_update_ms_od_solved;
	//	}
	//
	//	virtual int operator()(double m_dot_t /*kg/s*/, double *diff_m_dot_t /*-*/);
	//
	//	CO2_state mc_co2_props;
	//};

	class C_mono_eq_LTR_od : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;
        double m_od_tol;

	public:
		C_mono_eq_LTR_od(C_RecompCycle *pc_rc_cycle, double m_dot_rc,
            double m_dot_LTR_HP, double m_dot_t, double od_tol /*-*/)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_m_dot_rc = m_dot_rc;
            m_m_dot_LTR_HP = m_dot_LTR_HP;  //[kg/s]
			m_m_dot_t = m_dot_t;
            m_od_tol = od_tol;
		}	
		
		// These values are calculated in the operator() method and need to be extracted form this class
		//     after convergence
		double m_Q_dot_LTR;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_m_dot_rc, m_m_dot_LTR_HP, m_m_dot_t;

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};

	class C_mono_eq_LTR_des : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_LTR_des(C_RecompCycle *pc_rc_cycle, double w_mc, double w_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_w_mc = w_mc;
			m_w_t = w_t;
		}
	
		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_w_rc, m_m_dot_t, m_m_dot_rc, m_m_dot_mc, m_Q_dot_LT;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_w_mc, m_w_t;

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};

	class C_mono_eq_HTR_od : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;
        double m_od_tol;

	public:
		C_mono_eq_HTR_od(C_RecompCycle *pc_rc_cycle, double m_dot_rc,
            double m_dot_LTR_HP, double m_dot_t, double od_tol)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_m_dot_rc = m_dot_rc;
            m_m_dot_LTR_HP = m_dot_LTR_HP;  //[kg/s]
			m_m_dot_t = m_dot_t;
            m_od_tol = od_tol;
		}
	
		// These values are passed in as arguments to Constructor call and should not be reset
		double m_m_dot_rc, m_m_dot_LTR_HP, m_m_dot_t;

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_Q_dot_LTR, m_Q_dot_HTR;

		virtual int operator()(double T_HTR_LP_out_guess /*K*/, double *diff_T_HTR_LP_out /*K*/);
	};

	class C_mono_eq_HTR_des : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_HTR_des(C_RecompCycle *pc_rc_cycle, double w_mc, double w_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_w_mc = w_mc;
			m_w_t = w_t;
		}

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_w_rc, m_m_dot_t, m_m_dot_rc, m_m_dot_mc, m_Q_dot_LT, m_Q_dot_HT;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_w_mc, m_w_t;

		virtual int operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/);	
	};

	class C_MEQ_sco2_design_hit_eta__UA_total : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;
		std::string msg_log;
		std::string msg_progress;

	public:
		C_MEQ_sco2_design_hit_eta__UA_total(C_RecompCycle *pc_rc_cycle)
		{
			mpc_rc_cycle = pc_rc_cycle;

			msg_log = "Log message ";
			msg_progress = "Designing cycle...";
		}

		virtual int operator()(double UA_recup_total /*kW/K*/, double *eta /*-*/);
	};

	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_cycle_return_objective_metric(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta_fixed_P_high(double P_high_opt /*kPa*/);

	// Called by 'nlopt_cb_opt_od', so needs to be public
	//double off_design_point_value(const std::vector<double> &x);

	// Called by 'nlopt...', so needs to be public
	//double eta_at_target(const std::vector<double> &x);
	
	// Called by 'nlopt...', so needs to be public
	//double opt_od_eta(const std::vector<double> &x);
};

double nlopt_cb_opt_des(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_cb_opt_des_fixed_P_high(double P_high /*kPa*/, void *data);

//double nlopt_cb_opt_od(const std::vector<double> &x, std::vector<double> &grad, void *data);

//double nlopt_cb_eta_at_target(const std::vector<double> &x, std::vector<double> &grad, void *data);

//double nlopt_cb_opt_od_eta(const std::vector<double> &x, std::vector<double> &grad, void *data);

double P_pseudocritical_1(double T_K);


//double nlopt_callback_tub_bal_opt(const std::vector<double> &x, std::vector<double> &grad, void *data);


bool find_polynomial_coefs(const std::vector<double> x_data, const std::vector<double> y_data, int n_coefs, std::vector<double> & coefs_out, double & r_squared);

class C_poly_curve_r_squared
{
private:
	std::vector<double> m_x;
	std::vector<double> m_y;
	int m_n_points;
	double m_y_bar;
	double m_SS_tot;

public:
	C_poly_curve_r_squared()
	{
		m_x.resize(0);
		m_y.resize(0);
		m_n_points = -1;
		m_y_bar = std::numeric_limits<double>::quiet_NaN();
		m_SS_tot = std::numeric_limits<double>::quiet_NaN();
	}

	bool init(const std::vector<double> x_data, const std::vector<double> y_data);	


	// Called by 'nlopt...', so needs to be public
	double calc_r_squared(const std::vector<double> coefs);

};

double nlopt_callback_poly_coefs(const std::vector<double> &x, std::vector<double> &grad, void *data);

#endif
