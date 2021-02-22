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

#ifndef __SCO2_PC_CSP_INT_
#define __SCO2_PC_CSP_INT_

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"
#include "sco2_partialcooling_cycle.h"
#include "sco2_cycle_templates.h"

#include "heat_exchangers.h"
#include "csp_solver_util.h"

#include "numeric_solvers.h"

#include "ud_power_cycle.h"

#include <iosfwd>

class C_sco2_phx_air_cooler
{
public:

	struct S_des_par
	{
		// System Design
		int m_hot_fl_code;				//[-] Integer coding the HTF type
		util::matrix_t<double> mc_hot_fl_props;	//[-] Custom HTF properties (if applicable)
		double m_T_htf_hot_in;			//[K] Design-point hot inlet temperature
		double m_phx_dt_hot_approach;	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		double m_T_amb_des;				//[K] Ambient temperature
		double m_dt_mc_approach;		//[K] Temperature difference between main compressor inlet and ambient air
		double m_elevation;				//[m] Site elevation
		double m_W_dot_net;				//[kW] Target net cycle power
		int m_design_method;			//[-] 1 = Specify efficiency, 2 = Specify total recup UA
		double m_eta_thermal;			//[-] Cycle thermal efficiency
		double m_UA_recup_tot_des;		//[kW/K] Total recuperator conductance
		int m_cycle_config;				//[-] 2 = partial cooling, [else] = recompression
	
		// Cycle design parameters
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
        int m_LTR_N_sub_hxrs;               //[-]
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
            // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        int m_HTR_N_sub_hxrs;               //[-]
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
            //
        double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
        int m_mc_comp_type;                 //[-] Main compressor type 1: SNL 2: CompA
        double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the precompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_des_tol;				    //[-] Design point convergence tolerance
		double m_des_opt_tol;				//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		double m_is_recomp_ok;				//[-] 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
	
		int m_des_objective_type;			//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;			//[C]
	
		bool m_fixed_P_mc_out;			//[-] if true, P_mc_out is fixed at 'm_P_high_limit'
		
		double m_PR_HP_to_LP_guess;     //[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_HP_to_LP;       //[-] if true, ratio of P_mc_out to P_LP_in is fixed at m_PR_HP_to_LP_guess

        double m_f_PR_HP_to_IP_guess;   //[-] Initial guess fraction of HP-to-LP deltaP for HP-to-IP (partial cooling cycle)
        bool m_fixed_f_PR_HP_to_IP;     //[-] if true, use guess        

		// PHX design parameters
		// This is a PHX rather than system parameter because we don't know T_CO2_in until cycle model is solved
		double m_phx_dt_cold_approach;	//[K/C] Temperature difference between cold HTF and PHX CO2 inlet
        int m_phx_N_sub_hx;             //[-]
        NS_HX_counterflow_eqs::E_UA_target_type m_phx_od_UA_target_type;

		// Air cooler parameters
		bool m_is_des_air_cooler;		//[-] False will skip physical air cooler design. UA will not be available for cost models.
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;    // [-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
        double m_eta_fan;               //[-] Fan isentropic efficiency
        int m_N_nodes_pass;             //[-] Number of nodes per pass

		S_des_par()
		{
			m_hot_fl_code = m_design_method = m_LTR_N_sub_hxrs = m_LTR_N_sub_hxrs = m_phx_N_sub_hx = -1;
	
			// Default cycle config to recompression
			m_cycle_config = 1;	      

            // Default to SNL compressor
            m_mc_comp_type = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;

			// Air cooler default
			m_is_des_air_cooler = true;
            m_N_nodes_pass = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]
	
            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_phx_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

			m_T_htf_hot_in = m_phx_dt_hot_approach = m_T_amb_des = m_dt_mc_approach =
				m_elevation = m_W_dot_net = m_eta_thermal = 
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max =
	
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t =
				m_P_high_limit = m_des_tol = m_des_opt_tol = m_N_turbine =
                m_is_recomp_ok = 
	
				m_PR_HP_to_LP_guess = m_f_PR_HP_to_IP_guess =
	
				m_phx_dt_cold_approach = m_frac_fan_power = m_deltaP_cooler_frac = m_eta_fan =
				std::numeric_limits<double>::quiet_NaN();
	
            m_fixed_P_mc_out = false;       //[-] If false, then should default to optimizing this parameter
            m_fixed_PR_HP_to_LP = false;    //[-] If false, then should default to optimizing this parameter
            m_fixed_f_PR_HP_to_IP = false;  //[-] If false, then should default to optimizing this parameter

		}
	};

	struct S_des_solved
	{
		C_HX_counterflow_CRM::S_des_solved ms_phx_des_solved;
		C_sco2_cycle_core::S_design_solved ms_rc_cycle_solved;
	};

	struct S_od_par
	{
		// From CSP System
		double m_T_htf_hot;		//[K] Hot HTF temperature from the receiver or storage
		double m_m_dot_htf;		//[kg/s] HTF mass flow rate 
	
		// Ambient Conditions
		double m_T_amb;			//[K] Ambient temperature
	
        // Turbine inlet mode
        int m_T_t_in_mode;

		S_od_par()
		{
			m_T_htf_hot = m_m_dot_htf = m_T_amb = std::numeric_limits<double>::quiet_NaN();

            m_T_t_in_mode = C_sco2_cycle_core::E_SOLVE_PHX;  //[-] Default to using PHX and HTF temp and mass flow rate
		}
	};

	struct S_od_solved
	{
		C_sco2_cycle_core::S_od_solved ms_rc_cycle_od_solved;
		C_HX_counterflow_CRM::S_od_solved ms_phx_od_solved;
		int m_od_error_code;
		bool m_is_converged;
	
		S_od_solved()
		{
			m_od_error_code = 0;
			m_is_converged = false;
		}
	};

    struct S_solve_P_LP_in__tracker
    {
        double m_T_mc_in;       //[K]
        double m_T_pc_in;       //[K]
        int m_error_code;         //[-]
        double m_W_dot_fan_mc_cooler; //[MWe]
        double m_W_dot_fan_pc_cooler; //[MWe]
        double m_rel_diff_T_htf_cold;    //[-]
        double m_W_dot_net_less_cooling;   //[MWe]

        double m_objective;       //[-]

        S_solve_P_LP_in__tracker()
        {
            m_T_mc_in = m_T_pc_in =
                m_W_dot_fan_mc_cooler = m_W_dot_fan_pc_cooler =
                m_rel_diff_T_htf_cold = m_objective = std::numeric_limits<double>::quiet_NaN();

            m_error_code = -1;
        }
    };

    class C_P_LP_in_iter_tracker
    {
    public:

        std::vector<double> mv_P_LP_in;   //[kPa]
        std::vector<double> mv_W_dot_net; //[kWe]
        std::vector<double> mv_P_mc_out;  //[MPa]

        std::vector<int> mv_od_error_code;
        std::vector<bool> mv_is_converged;

        C_P_LP_in_iter_tracker() {}

        void reset_vectors();

        void push_back_vectors(double P_LP_in /*kpa*/, double W_dot_net /*kWe*/, double P_mc_out /*kPa*/,
                            int od_error_code, bool is_converged);
    };

	enum E_off_design_strategies
	{
		E_TARGET_POWER_ETA_MAX,
        E_TARGET_T_HTF_COLD_POWER_MAX
	};

	enum E_system_op_constraints
	{
		E_TURBINE_INLET_OVER_TEMP = -15,
		E_OVER_PRESSURE,
		E_TIP_RATIO,
		E_MC_SURGE,
		E_RC_SURGE,
		E_PC_SURGE
	};

    struct S_to_W_net_less_cooling_max
    {
        C_sco2_phx_air_cooler* mpc_sco2_phx_air_cooler;
        C_sco2_phx_air_cooler::E_off_design_strategies m_od_opt_obj;
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* m_call_tracker;
        double od_tol;

        S_to_W_net_less_cooling_max()
        {
            od_tol = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_to_off_design__calc_T_mc_in
    {
        C_sco2_phx_air_cooler* mpc_sco2_phx_air_cooler;
        C_sco2_phx_air_cooler::S_od_par od_par;
        bool is_rc_N_od_at_design;
        double rc_N_od_f_des;
        bool is_mc_N_od_at_design;
        double mc_N_od_f_des;
        bool is_pc_N_od_at_design;
        double pc_N_od_f_des;
        bool is_PHX_dP_input;
        double PHX_f_dP;
        double od_opt_tol;
        double od_tol;
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* pv_T_mc_in_call_tracker;

        S_to_off_design__calc_T_mc_in()
        {
            rc_N_od_f_des = mc_N_od_f_des = pc_N_od_f_des = PHX_f_dP = od_opt_tol = od_tol = std::numeric_limits<double>::quiet_NaN();
        }
    };

    class C_to_N_mc_rc_opt
    {
    public:

        C_sco2_phx_air_cooler* mpc_sco2_phx_air_cooler;
        bool m_is_N_mc_opt;
        bool m_is_mc_N_od_at_design;
        double m_mc_N_od_f_des;
        bool m_is_N_rc_opt;
        bool m_is_rc_N_od_at_design;
        double m_rc_N_od_f_des;
        bool m_is_N_pc_opt;
        bool m_is_pc_N_od_at_design;
        double m_pc_N_od_f_des;
        C_sco2_phx_air_cooler::S_od_par m_od_par;
        bool m_is_PHX_dP_input;
        double m_PHX_f_dP;
        C_sco2_phx_air_cooler::E_off_design_strategies m_od_opt_objective;
        double m_od_opt_tol;
        double m_od_tol;

        C_to_N_mc_rc_opt(C_sco2_phx_air_cooler* pc_sco2_phx_air_cooler,
            bool is_N_mc_opt, bool is_mc_N_od_at_design, double mc_N_od_f_des,
            bool is_N_rc_opt, bool is_rc_N_od_at_design, double rc_N_od_f_des,
            bool is_N_pc_opt, bool is_pc_N_od_at_design, double pc_N_od_f_des,
            C_sco2_phx_air_cooler::S_od_par od_par,
            bool is_PHX_dP_input, double PHX_f_dP,
            C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objectives,
            double od_opt_tol, double od_tol)
        {
            mpc_sco2_phx_air_cooler = pc_sco2_phx_air_cooler;
            m_is_N_mc_opt = is_N_mc_opt;
            m_is_mc_N_od_at_design = is_mc_N_od_at_design;
            m_mc_N_od_f_des = mc_N_od_f_des;
            m_is_N_rc_opt = is_N_rc_opt;
            m_is_rc_N_od_at_design = is_rc_N_od_at_design;
            m_rc_N_od_f_des = rc_N_od_f_des;
            m_is_N_pc_opt = is_N_pc_opt;
            m_is_pc_N_od_at_design = is_pc_N_od_at_design;
            m_pc_N_od_f_des = pc_N_od_f_des;
            m_od_par = od_par;
            m_is_PHX_dP_input = is_PHX_dP_input;
            m_PHX_f_dP = PHX_f_dP;
            m_od_opt_objective = od_opt_objectives;
            m_od_opt_tol = od_opt_tol;
            m_od_tol = od_tol;
        }
    };

	// Callback function with progress bar
	bool(*mf_callback_update)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);
	void *mp_mf_update;

	C_csp_messages mc_messages;

private:
	
	C_sco2_cycle_core * mpc_sco2_cycle;

	C_RecompCycle mc_rc_cycle;
	C_HX_co2_to_htf mc_phx;
	C_PartialCooling_Cycle mc_partialcooling_cycle;

	S_des_par ms_des_par;
	C_sco2_cycle_core::S_auto_opt_design_hit_eta_parameters ms_cycle_des_par;
	C_HX_counterflow_CRM::S_des_calc_UA_par ms_phx_des_par;
		
	S_des_solved ms_des_solved;

	S_od_par ms_od_par;
	C_sco2_cycle_core::S_od_par ms_cycle_od_par;
	C_HX_counterflow_CRM::S_od_par ms_phx_od_par;

	S_od_solved ms_od_solved;
	
    C_P_LP_in_iter_tracker mc_P_LP_in_iter_tracker;

    //double m_od_tol__csp_int;

    // Cycle control parameter
    bool m_is_T_crit_limit;

    int m_nlopt_iter;		//[-]

	double m_T_mc_in_min;		//[K]
	double m_T_co2_crit;		//[K]
	double m_P_co2_crit;		//[kPa]

	void design_core();

	double adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/);

	void setup_off_design_info(C_sco2_phx_air_cooler::S_od_par od_par);

public:	

	C_sco2_phx_air_cooler();

	~C_sco2_phx_air_cooler(){};

    void set_ms_od_par_T_mc_in(double x) { ms_cycle_od_par.m_T_mc_in = x; }

    class C_MEQ_T_mc_in__W_dot_fan : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_ac;
        E_off_design_strategies m_od_opt_objective;
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> *m_call_tracker;
        double m_od_tol;

    public:
        C_MEQ_T_mc_in__W_dot_fan(C_sco2_phx_air_cooler *pc_sco2_ac,
            E_off_design_strategies od_opt_objective,
            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker> * call_tracker,
            double od_tol)
        {
            mpc_sco2_ac = pc_sco2_ac;
            m_od_opt_objective = od_opt_objective;
            m_call_tracker = call_tracker;
            m_od_tol = od_tol;
        }

        virtual int operator()(double T_mc_in /*K*/, double *W_dot_fan /*MWe*/);
    };

    class C_MEQ_T_pc_in__W_dot_fan__T_mc_in_opt : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler* mpc_sco2_ac;
        C_sco2_phx_air_cooler::S_od_par m_od_par;
        bool m_is_rc_N_od_at_design;
        double m_rc_N_od_f_des;         //[-]
        bool m_is_mc_N_od_at_design;
        double m_mc_N_od_f_des;         //[-]
        bool m_is_pc_N_od_at_design;
        double m_pc_N_od_f_des;         //[-]
        bool m_is_PHX_dP_input;
        double m_PHX_f_dP;                //[-]
        double m_od_opt_tol;            //[-]
        double m_od_tol;                  //[-]
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* mv_T_mc_in_call_tracker;

    public:
        C_MEQ_T_pc_in__W_dot_fan__T_mc_in_opt(C_sco2_phx_air_cooler* pc_sco2_ac,
            C_sco2_phx_air_cooler::S_od_par od_par,
            bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
            bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
            double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
            bool is_PHX_dP_input, double PHX_f_dP /*-*/,
            double od_opt_tol /*-*/, double od_tol /*-*/,
            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* v_T_mc_in_call_tracker)
        {
            mpc_sco2_ac = pc_sco2_ac;
            m_od_par = od_par;
            m_is_rc_N_od_at_design = is_rc_N_od_at_design;
            m_rc_N_od_f_des = rc_N_od_f_des;
            m_is_mc_N_od_at_design = is_mc_N_od_at_design;
            m_mc_N_od_f_des = mc_N_od_f_des;
            m_is_pc_N_od_at_design = is_pc_N_od_at_design;
            m_pc_N_od_f_des = pc_N_od_f_des;
            m_is_PHX_dP_input = is_PHX_dP_input;
            m_PHX_f_dP = PHX_f_dP;
            m_od_opt_tol = od_opt_tol;
            m_od_tol = od_tol;
            mv_T_mc_in_call_tracker = v_T_mc_in_call_tracker;
        }

        virtual int operator()(double T_pc_in /*K*/, double* W_dot_pc_fan /*MWe*/);
    };

    class C_MEQ_T_pc_in__W_dot_fan : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_ac;
        double m_W_dot_mc_cooler_fan_target; //[MWe]
        double m_T_mc_in_min;   //[K]
        E_off_design_strategies m_od_opt_objective;
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* m_call_tracker;
        double m_od_tol;

    public:
        C_MEQ_T_pc_in__W_dot_fan(C_sco2_phx_air_cooler *pc_sco2_ac,
            double W_dot_mc_cooler_fan_target /*MWe*/,
            double T_mc_in_min /*K*/,
            E_off_design_strategies od_opt_objective,
            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>* call_tracker,
            double od_tol /*-*/)
        {
            mpc_sco2_ac = pc_sco2_ac;
            m_W_dot_mc_cooler_fan_target = W_dot_mc_cooler_fan_target;    //[MWe]
            m_T_mc_in_min = T_mc_in_min;    //[K]
            m_od_opt_objective = od_opt_objective;
            m_call_tracker = call_tracker;
            m_od_tol = od_tol;
        }

        virtual int operator()(double T_pc_in /*K*/, double *W_dot_fan /*MWe*/);
    };

	class C_mono_eq_T_t_in : public C_monotonic_equation
	{
	private: 
		C_sco2_phx_air_cooler *mpc_sco2_rc;
        int m_T_t_in_mode;
        double m_od_tol;

	public:
		C_mono_eq_T_t_in(C_sco2_phx_air_cooler *pc_sco2_rc, int T_t_in_mode,
                        double od_tol)
		{
			mpc_sco2_rc = pc_sco2_rc;
            m_od_tol = od_tol;

            m_T_t_in_mode = T_t_in_mode;
		}
	
		virtual int operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/);
	};

    class C_MEQ__P_LP_in__T_htf_cold_target : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_cycle;
        double m_od_tol;

    public:
        C_MEQ__P_LP_in__T_htf_cold_target(C_sco2_phx_air_cooler* pc_sco2_cycle,
                                    double od_tol /*-*/)
        {
            mpc_sco2_cycle = pc_sco2_cycle;
            m_od_tol = od_tol;
        }

        virtual int operator()(double P_LP_in /*kPa*/, double* T_htf_cold /*K*/);
    };

    class C_MEQ__P_LP_in__W_dot_target : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_cycle;
        double m_od_tol;

    public:
        C_MEQ__P_LP_in__W_dot_target(C_sco2_phx_air_cooler *pc_sco2_cycle,
                                    double od_tol /*-*/)
        {
            mpc_sco2_cycle = pc_sco2_cycle;
            m_od_tol = od_tol;
        }

        virtual int operator()(double P_LP_in /*kPa*/, double *W_dot /*kWe*/);
    };

    class C_MEQ__P_LP_in__P_mc_out_target : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_cycle;
        double m_od_tol;

    public:
        C_MEQ__P_LP_in__P_mc_out_target(C_sco2_phx_air_cooler *pc_sco2_cycle,
                                        double od_tol /*-*/)
        {
            mpc_sco2_cycle = pc_sco2_cycle;
            m_od_tol = od_tol;
        }

        virtual int operator()(double P_LP_in /*kPa*/, double *P_mc_out /*kPa*/);
    };

    class C_MEQ__P_LP_in__max_no_err_code : public C_monotonic_equation
    {
    private:
        C_sco2_phx_air_cooler *mpc_sco2_cycle;
        double m_od_tol;

    public:
        C_MEQ__P_LP_in__max_no_err_code(C_sco2_phx_air_cooler *pc_sco2_cycle,
                                        double od_tol /*-*/)
        {
            mpc_sco2_cycle = pc_sco2_cycle;
            m_od_tol = od_tol;
        }

        virtual int operator()(double P_LP_in /*kPa*/, double *P_mc_out /*kPa*/);
    };

	class C_sco2_csp_od : public C_od_pc_function
	{
	private:
		C_sco2_phx_air_cooler *mpc_sco2_rc;
        double m_od_opt_tol;
        double m_od_tol;

	public:
		C_sco2_csp_od(C_sco2_phx_air_cooler *pc_sco2_rc,
            double od_opt_tol /*-*/,
            double od_tol /*-*/)
		{
			mpc_sco2_rc = pc_sco2_rc;
            m_od_opt_tol = od_opt_tol;
            m_od_tol = od_tol;
		}
	
		virtual int operator()(S_f_inputs inputs, S_f_outputs & outputs);
	};

	int generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind,
        double od_opt_tol /*-*/, double od_tol /*-*/);

	void design(S_des_par des_par);

    int off_design__constant_N__calc_max_htf_massflow__T_mc_in_P_LP_in__objective(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        E_off_design_strategies od_opt_objective,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

	int off_design__constant_N__T_mc_in_P_LP_in__objective(C_sco2_phx_air_cooler::S_od_par od_par, 
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        E_off_design_strategies od_opt_objective,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

    //int off_design__constant_N__calc_max_mass_flow__objective

    int off_design__target_power__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        double od_opt_tol_in, double od_tol /*-*/);

    int off_design__calc_T_pc_in__target_T_htf_cold__max_power(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        double od_opt_tol_in /*-*/, double od_tol /*-*/,
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_T_mc_in__tracker);

    int off_design__calc_T_mc_in__target_T_htf_cold__max_power(C_sco2_phx_air_cooler::S_od_par od_par,
        double T_pc_in /*K*/,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_P_LP_in__tracker,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

	int off_design_fix_T_mc_in__P_mc_in_solve_for_target(C_sco2_phx_air_cooler::S_od_par od_par,
		double T_mc_in /*K*/,
		bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
		bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
		bool is_PHX_dP_input, double PHX_f_dP /*-*/,
		C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

    void solve_T_mc_in_for_cooler_constraint(double W_dot_mc_cooler_fan_target /*MWe*/,
            double T_comp_in_min /*K*/, C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
            std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_call_tracker,
            double od_tol /*-*/);

    int solve_T_pc_in_for_cooler_constraints(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        double is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        double W_dot_pc_cooler_fan_target /*MWe*/,
        double T_pc_in_min /*K*/, C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_T_mc_in_call_tracker,
        double od_opt_tol /*-*/, double od_tol /*-*/);

    int check_increasing_T_mc_in(double W_dot_target /*kWe*/, double W_dot_fan_limit /*MWe*/,
        bool is_modified_P_mc_in_solver,
        C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
        double & W_dot_opt /*kWe*/, double & eta_max_at_W_dot_opt /*-*/,
        double & P_LP_in_opt /*kPa*/, double & T_mc_in_opt /*K*/,
        double od_tol /*-*/);

    void solve_nested_T_pc_in__T_mc_in_for_cooler_constrains(double W_dot_pc_cooler_fan_target /*MWe*/,
        double W_dot_mc_cooler_fan_target /*MWe*/,
        double T_comp_in_min /*K*/,
        C_sco2_phx_air_cooler::E_off_design_strategies od_opt_objective,
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_call_tracker,
        double od_tol /*-*/);

    int optimize_N_mc_and_N_rc__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        E_off_design_strategies off_design_strategy,
        bool is_optimize_N_rc /*-*/, bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_optimize_N_mc /*-*/, bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        bool is_optimize_N_pc /*-*/, bool is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        double & eta_max /*-*/, double & f_N_mc_opt /*-*/, 
        double & f_N_rc_opt /*-*/, double & W_dot_at_eta_max /*kWe*/,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

    int optimize_N_rc__max_eta(C_sco2_phx_air_cooler::S_od_par od_par,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        E_off_design_strategies od_opt_objective,
        double & eta_max /*-*/, double & f_N_rc_opt, double & W_dot_at_eta_max /*kWe*/,
        double f_N_rc_guess,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);

	int off_design_fix_P_mc_in(S_od_par od_par,
        double P_mc_in /*MPa*/, double T_mc_in /*K*/, double T_pc_in /*K*/,
        bool is_rc_N_od_at_design, double rc_N_od_f_des /*-*/,
        bool is_mc_N_od_at_design, double mc_N_od_f_des /*-*/,
        bool is_pc_N_od_at_design, double pc_N_od_f_des /*-*/,
        bool is_PHX_dP_input, double PHX_f_dP /*-*/,
        E_off_design_strategies off_design_strategy,
        double od_opt_tol_in /*-*/, double od_tol /*-*/);
	
    int solve_P_LP_in__target_W_dot(double od_tol /*-*/);

    int solve_P_LP_in__target_T_htf_cold(double od_tol /*-*/);

    //int solve_P_LP_in__objective(E_off_design_strategies od_opt_objective);

    int solve_P_LP_in__objective(E_off_design_strategies od_opt_objective,
        std::vector<C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker>& v_call_tracker,
        double od_tol /*-*/);
    
    void check_od_solution(double & diff_m_dot, double & diff_E_cycle,
        double & diff_Q_LTR, double & diff_Q_HTR);

	int off_design_core(double & eta_solved, double od_tol /*-*/);

	double get_T_mc_in_min()
	{
		return m_T_mc_in_min;		//[K]
	}

	// Methods to private access member data
	const S_des_par * get_design_par();

	const S_des_solved * get_design_solved();

	const C_HX_counterflow_CRM::S_des_calc_UA_par * get_phx_des_par();

	const S_od_solved * get_od_solved();

    const S_od_par* get_od_par();

	double opt_P_LP_in__fixed_N_turbo__return_f_obj(double P_mc_in /*kPa*/, double od_tol /*-*/);

};

// Optimization method callbacks
double fmin_opt_T_mc_in__max_net_power_less_cooling(double x, void* data);

double nlopt_opt_T_pc_in__max_net_power_less_cooling(const std::vector<double>& x, std::vector<double>& grad, void* data);

double nlopt_cb_opt_N_mc_rc(const std::vector<double>& x, std::vector<double>& grad, void* data);

bool SortByTmcin(const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& lhs,
    const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& rhs);

bool SortByTpcin(const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& lhs,
    const C_sco2_phx_air_cooler::S_solve_P_LP_in__tracker& rhs);

#endif
