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

#ifndef __SCO2_PARTIAL_COOLING_
#define __SCO2_PARTIAL_COOLING_

#include "sco2_cycle_components.h"
#include "sco2_cycle_templates.h"

#include "heat_exchangers.h"
#include "CO2_properties.h"

#include <string>
#include <vector>
#include <math.h>
#include <limits>

class C_PartialCooling_Cycle : public C_sco2_cycle_core
{
public:

	struct S_design_limits
	{
		double m_UA_net_power_ratio_max;		//[-/K]
		double m_UA_net_power_ratio_min;		//[-/K]

		double m_T_mc_in_min;					//[K]

		S_design_limits()
		{
			m_UA_net_power_ratio_max = m_UA_net_power_ratio_min = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_des_params
	{
		double m_W_dot_net;					//[kWe] Target net cycle power
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		double m_P_pc_in;					//[kPa] Pre-compressor inlet pressure
		double m_P_mc_in;					//[kPa] Compressor inlet pressure
		double m_P_mc_out;					//[kPa] Compressor outlet pressure
		std::vector<double> m_DP_LTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_LP;     //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_IP;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
            // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
        double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        int m_LTR_N_sub_hxrs;               //[-] Number of sub-hxs in hx model
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
            // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        int m_HTR_N_sub_hxrs;               //[-] Number of sub-hxs in hx model
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
            //
        double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
        int m_mc_comp_model_code;           //[-] Main compressor model - see sco2_cycle_components.h 
        double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
        int m_rc_comp_model_code;           //[-] Recompressor model - see sco2_cycle_components.h 
		double m_eta_pc;					//[-] design-point efficiency of the pre-compressor; 
        int m_pc_comp_model_code;           //[-] Precompressor model - see sco2_cycle_components.h 
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_des_tol;					//[-] Convergence tolerance
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

		S_des_params()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in = 
				m_P_pc_in = m_P_mc_in = m_P_mc_out = 
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_max = m_LTR_eff_target =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_max = m_HTR_eff_target =
                m_recomp_frac =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit = m_des_tol = m_N_turbine =
				m_frac_fan_power = m_deltaP_cooler_frac = m_T_amb_des = m_elevation = m_eta_fan = std::numeric_limits<double>::quiet_NaN();
			m_LTR_N_sub_hxrs = m_HTR_N_sub_hxrs = -1;

			// Air cooler default
			m_is_des_air_cooler = true;
            m_N_nodes_pass = -1;

            // Compressor model codes
            m_mc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;
            m_rc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;
            m_pc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;

            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_LP.resize(2);
			std::fill(m_DP_PC_LP.begin(), m_DP_PC_LP.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_IP.resize(2);
			std::fill(m_DP_PC_IP.begin(), m_DP_PC_IP.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_opt_des_params
	{
		double m_W_dot_net;					//[kWe] Target net cycle power
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_LP;     //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_IP;     //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
            // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
        double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        int m_LTR_N_sub_hxrs;            //[-] Number of sub-hxs used to model hx
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
            // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        int m_HTR_N_sub_hxrs;            //[-] Number of sub-hxs used to model hx
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
            //
        double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the pre-compressor; 
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		//int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_des_tol;					//[-] Convergence tolerance
		double m_des_opt_tol;				//[-] Optimization tolerance
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

		double m_P_mc_out_guess;		//[kPa] Initial guess for main compressor outlet pressure
		bool m_fixed_P_mc_out;			//[-] If true, P_mc_out is fixed at P_mc_out_guess

		double m_PR_total_guess;		//[-] Initial guess for ratio of P_mc_out / P_pc_in
		bool m_fixed_PR_total;			//[-] if true, ratio of P_mc_out to P_pc_in is fixed at PR_guess

		double m_f_PR_mc_guess;			//[-] Initial guess: fraction of total PR that is P_mc_out / P_mc_in
		bool m_fixed_f_PR_mc;			//[-] if true, fixed at f_PR_mc_guess

		double m_recomp_frac_guess;		//[-] Initial guess: recompression fraction
		bool m_fixed_recomp_frac;		//[-] if true, fixed at m_recomp_frac_guess

		double m_LTR_frac_guess;		//[-] Initial guess for fraction of UA_rec_total that is allocated to LTR
		bool m_fixed_LTR_frac;			//[-] if true, fixed at m_LTR_frac_guess

		S_opt_des_params()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in =
				m_UA_rec_total = 
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit =
                m_des_tol = m_des_opt_tol = m_N_turbine = 
				m_frac_fan_power = m_deltaP_cooler_frac = m_T_amb_des = m_elevation =
				m_P_mc_out_guess = m_PR_total_guess = m_f_PR_mc_guess = 
				m_recomp_frac_guess = m_LTR_frac_guess = m_eta_fan = std::numeric_limits<double>::quiet_NaN();
			m_LTR_N_sub_hxrs = m_HTR_N_sub_hxrs = -1;

			// Air cooler default
			m_is_des_air_cooler = true;
            m_N_nodes_pass = -1;

            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_LP.resize(2);
			std::fill(m_DP_PC_LP.begin(), m_DP_PC_LP.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_IP.resize(2);
			std::fill(m_DP_PC_IP.begin(), m_DP_PC_IP.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

private:

	// Cycle component classes
	C_turbine mc_t;
	C_comp_multi_stage mc_mc, mc_rc, mc_pc;
    C_HX_co2_to_co2_CRM mc_LTR;
    C_HX_co2_to_co2_CRM mc_HTR;
	C_HeatExchanger mc_PHX, mc_cooler_pc, mc_cooler_mc;	

	C_CO2_to_air_cooler mc_pc_air_cooler;   // formerly LP
	C_CO2_to_air_cooler mc_mc_air_cooler;   // formerly IP

	S_des_params ms_des_par;
	S_opt_des_params ms_opt_des_par;

	CO2_state mc_co2_props;

	// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_m_dot_mc, m_m_dot_pc, m_m_dot_rc, m_m_dot_t;	//[kg/s]
	double m_W_dot_mc, m_W_dot_pc, m_W_dot_rc, m_W_dot_t;	//[kWe]
	double m_eta_thermal_calc_last;	//[-]
	double m_W_dot_net_last;	//[kWe]
	double m_energy_bal_last;	//[-]
	double m_objective_metric_last;	//[??]

	// Structures and data for optimization
	S_des_params ms_des_par_optimal;
	double m_objective_metric_opt;

		// Structures and data for auto-optimization
	double m_objective_metric_auto_opt;
	S_des_params ms_des_par_auto_opt;

	// Results from last off-design solution
	std::vector<double> mv_temp_od, mv_pres_od, mv_enth_od, mv_entr_od, mv_dens_od;
	double m_eta_thermal_od;
	double m_W_dot_net_od;
	double m_Q_dot_PHX_od;
    double m_Q_dot_mc_cooler_od;
    double m_Q_dot_pc_cooler_od;

	int design_core();

	int auto_opt_design_core();

	int finalize_design();

	int opt_design_core();

	int off_design_fix_shaft_speeds_core(double od_tol /*-*/);

public:

	C_PartialCooling_Cycle()
	{
		m_temp_last.resize(END_SCO2_STATES);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());

		m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

		m_m_dot_mc = m_m_dot_pc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_mc = m_W_dot_pc = m_W_dot_rc = m_W_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_eta_thermal_calc_last = m_W_dot_net_last = m_energy_bal_last =
		m_objective_metric_last = m_objective_metric_opt = m_objective_metric_auto_opt = std::numeric_limits<double>::quiet_NaN();

		mv_temp_od = mv_pres_od = mv_enth_od = mv_entr_od = mv_dens_od = m_temp_last;
		m_eta_thermal_od = m_W_dot_net_od = m_Q_dot_PHX_od =
        m_Q_dot_mc_cooler_od = m_Q_dot_pc_cooler_od = std::numeric_limits<double>::quiet_NaN();
	}

	class C_MEQ__f_recomp__y_N_rc : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle * mpc_pc_cycle;

		double m_T_pc_in;		//[K] Pre-compressor inlet temperature
		double m_P_pc_in;		//[kPa] Pre-compressor inlet pressure
		double m_T_mc_in;		//[K] Main compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		double m_f_mc_pc_bypass;	//[-] Fraction of main and pre compressors bypassed to respective coolers

        double m_od_tol;        //[-] Convergence tolerance

        double m_N_rc_od_target;    //[rpm]

	public:

		double m_m_dot_t;		//[kg/s]
		double m_m_dot_pc;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_mc;		//[kg/s]
		double m_m_dot_LTR_HP;	//[kg/s]

		C_MEQ__f_recomp__y_N_rc(C_PartialCooling_Cycle *pc_pc_cycle,
			double T_pc_in /*K*/, double P_pc_in /*kPa*/, 
			double T_mc_in /*K*/, double T_t_in /*K*/,
			double f_mc_pc_bypass /*-*/, double od_tol /*-*/,
            double N_rc_od_target /*rpm*/)
		{
			mpc_pc_cycle = pc_pc_cycle;
			m_T_pc_in = T_pc_in;		//[K]
			m_P_pc_in = P_pc_in;		//[kPa]
			m_T_mc_in = T_mc_in;		//[K]
			m_T_t_in = T_t_in;			//[K]
			m_f_mc_pc_bypass = f_mc_pc_bypass;	//[-]
            m_od_tol = od_tol;
            m_N_rc_od_target = N_rc_od_target;  //[rpm]
			
			m_m_dot_t = m_m_dot_pc = m_m_dot_rc = 
				m_m_dot_mc = m_m_dot_LTR_HP = std::numeric_limits<double>::quiet_NaN();
		}

		virtual int operator()(double f_recomp /*-*/, double *diff_N_rc /*-*/);
	};

	class C_MEQ__t_m_dot__bal_turbomachinery : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle * mpc_pc_cycle;

		double m_T_pc_in;		//[K] Pre-compressor inlet temperature
		double m_P_pc_in;		//[kPa] Pre-compressor inlet pressure
		double m_T_mc_in;		//[K] Main compressor inlet temperature
		double m_f_recomp;		//[-] Recompression fraction
		double m_T_t_in;		//[K] Turbine inlet temperature

		double m_f_mc_pc_bypass;	//[-] Fraction of main and pre compressors bypassed to respective coolers

	public:
		C_MEQ__t_m_dot__bal_turbomachinery(C_PartialCooling_Cycle *pc_pc_cycle,
			double T_pc_in /*K*/, double P_pc_in /*kPa*/, double T_mc_in /*K*/,
			double f_recomp /*-*/, double T_t_in /*K*/,
			double f_mc_pc_bypass /*-*/)
		{
			mpc_pc_cycle = pc_pc_cycle;
			m_T_pc_in = T_pc_in;		//[K]
			m_P_pc_in = P_pc_in;		//[kPa]
			m_T_mc_in = T_mc_in;		//[K]
			m_f_recomp = f_recomp;		//[-]
			m_T_t_in = T_t_in;			//[K]
			m_f_mc_pc_bypass = f_mc_pc_bypass;	//[-]

			m_m_dot_mc = m_m_dot_pc = m_m_dot_LTR_HP = std::numeric_limits<double>::quiet_NaN();
		}

		double m_m_dot_mc;		//[kg/s]
		double m_m_dot_pc;		//[kg/s]
		double m_m_dot_LTR_HP;	//[kg/s]

		virtual int operator()(double m_dot_t /*kg/s*/, double *diff_m_dot_t /*-*/);
	};

	class C_MEQ_recup_od : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle * mpc_pc_cycle;

		double m_m_dot_LTR_HP;		//[kg/s]
		double m_m_dot_t;			//[kg/s]
		double m_m_dot_rc;			//[kg/s]

        double m_od_tol;

	public:
		C_MEQ_recup_od(C_PartialCooling_Cycle *pc_pc_cycle,
			double m_dot_LTR_HP /*kg/s*/,
			double m_dot_t /*kg/s*/,
			double m_dot_rc /*kg/s*/,
            double od_tol /*-*/)
		{
			mpc_pc_cycle = pc_pc_cycle;

			m_m_dot_LTR_HP = m_dot_LTR_HP;
			m_m_dot_t = m_dot_t;
			m_m_dot_rc = m_dot_rc;

            m_od_tol = od_tol;
		}

		virtual int operator()(double T_HTR_LP_out_guess /*K*/, double *diff_T_HTR_LP_out /*K*/);
	};

	class C_MEQ_HTR_des : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle *mpc_pc_cycle;

	public:
		C_MEQ_HTR_des(C_PartialCooling_Cycle *pc_pc_cycle)
		{
			mpc_pc_cycle = pc_pc_cycle;
			m_Q_dot_LTR = m_Q_dot_HTR = std::numeric_limits<double>::quiet_NaN();
		}

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_Q_dot_LTR, m_Q_dot_HTR;	//[kWt]

		virtual int operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/);
	};

	class C_MEQ_LTR_des : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle *mpc_pc_cycle;

	public:
		C_MEQ_LTR_des(C_PartialCooling_Cycle *pc_pc_cycle)
		{
			mpc_pc_cycle = pc_pc_cycle;
			m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();
		}

		double m_Q_dot_LTR;		//[kWt]

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};

	class C_MEQ_sco2_design_hit_eta__UA_total : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle * mpc_pc_cycle;
		std::string msg_log;
		std::string msg_progress;

	public:
		C_MEQ_sco2_design_hit_eta__UA_total(C_PartialCooling_Cycle *pc_pc_cycle)
		{
			mpc_pc_cycle = pc_pc_cycle;

			msg_log = "Log message ";
			msg_progress = "Designing cycle...";
		}

		virtual int operator()(double UA_recup_total /*kW/K*/, double *eta /*-*/);
	};

	int design(S_des_params & des_par_in);

	int opt_design(S_opt_des_params & opt_des_par_in);

	int auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in);
	
	int auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, std::string & error_msg);

	int off_design_fix_shaft_speeds(S_od_par & od_phi_par_in, double od_tol);

    virtual void check_od_solution(double & diff_m_dot, double & diff_E_cycle,
        double & diff_Q_LTR, double & diff_Q_HTR);

	virtual int solve_OD_all_coolers_fan_power(double T_amb /*K*/, double od_tol /*-*/, double & W_dot_fan /*MWe*/);

    virtual int solve_OD_mc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/,
                                    double & W_dot_mc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/);

    virtual int solve_OD_pc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/,
                                    double & W_dot_pc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/);

	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_cycle_return_objective_metric(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta_fixed_P_high(double P_high_opt /*kPa*/);

	const C_comp_multi_stage::S_od_solved * get_rc_od_solved()
	{
		return mc_rc.get_od_solved();
	}

};

double nlopt_cb_opt_partialcooling_des(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_cb_opt_partialcooling_des_fixed_P_high(double P_high /*kPa*/, void *data);

#endif
