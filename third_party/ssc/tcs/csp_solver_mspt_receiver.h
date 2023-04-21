/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __csp_solver_mspt_receiver_
#define __csp_solver_mspt_receiver_

#include "ngcc_powerblock.h"
#include "csp_solver_pt_receiver.h"
#include "csp_solver_util.h"

class C_mspt_receiver : public C_pt_receiver
{
// The transient receiver, including legacy steady-state receiver code for either non-transient startup or non-transient operation

private:
    ngcc_power_cycle cycle_calcs;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	double m_A_rec_proj;
	double m_A_node;
	
	double m_Q_dot_piping_loss;		//[Wt] = Constant thermal losses from piping to env. = (THT*length_mult + length_add) * piping_loss_coef

	int m_itermode;
	double m_od_control;
	double m_eta_field_iter_prev;	//[-] Efficiency from heliostat on last iteration. Maybe change if CR gets defocus signal from controller
	double m_tol_od;

	/* declare storage variables here */
	double m_E_su;
	double m_E_su_prev;
	double m_t_su;
	double m_t_su_prev;

	util::matrix_t<int> m_flow_pattern;
	int m_n_lines;

	util::matrix_t<double> m_flux_in;

	util::matrix_t<double> m_q_dot_inc;

	util::matrix_t<double> m_T_s;
	util::matrix_t<double> m_T_panel_out;
	util::matrix_t<double> m_T_panel_in;
	util::matrix_t<double> m_T_panel_ave;
	util::matrix_t<double> m_q_dot_conv;
	util::matrix_t<double> m_q_dot_rad;
	util::matrix_t<double> m_q_dot_loss;
	util::matrix_t<double> m_q_dot_abs;

	double m_m_mixed;
	double m_LoverD;
	double m_RelRough;

	// ISCC-specific
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low;
	double m_P_amb_high;
	double m_q_iscc_max;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

	struct s_steady_state_soln
	{
		C_csp_collector_receiver::E_csp_cr_modes mode;					// Operating mode
		bool rec_is_off;
		int itermode;

		double hour;				// Hour of the year 
		double T_amb;				// Dry bulb temperature (K)
		double T_dp;				// Dewpoint temperature (K)
		double v_wind_10;			// Wind speed at 10m (m/s)
		double p_amb;				// Ambient pressure (Pa)

		double dni;					// DNI for this solution
		double field_eff;			// Field efficiency for this solution

		double od_control;          // Defocus control

		double m_dot_salt;			// Salt mass flow per path (kg/s)
		double m_dot_salt_tot;      // Total salt mass flow (kg/s)
		double T_salt_cold_in;		// Cold salt inlet temperature (K)
		double T_salt_hot;			// Receiver outlet T including piping loss (K)
		double T_salt_hot_rec;      // Receiver outlet T before piping loss (K)
		double T_salt_props;		// Temperature at which salt properties are evaluated

		double u_salt;				// Salt velocity (m/s)
		double f;					// Friction factor

		double Q_inc_sum;			// Total absorbed solar energy (W)
		double Q_conv_sum;			// Total convection loss (W)
		double Q_rad_sum;			// Total radiation loss (W)
		double Q_abs_sum;			// Total energy transferred to HTF, not including piping loss (W)
		double Q_dot_piping_loss;   // Piping loss (W)
		double Q_inc_min;			// Minimum absorbed solar energy on any panel (W)
		double Q_thermal;			// Thermal power delivered to fluid (less piping loss) (W)

		double eta_therm;			// Receiver thermal efficiency (energy to HTF not including piping loss / Absorbed solar energy)

		util::matrix_t<double> T_s;			// Average external tube T (K)
		util::matrix_t<double> T_panel_out; // Panel HTF outlet T (K)
		util::matrix_t<double> T_panel_in;	// Panel HTF inlet T (K)
		util::matrix_t<double> T_panel_ave; // Panel average HTF T (k)

		util::matrix_t<double> q_dot_inc;  // Panel absorbed solar energy (W)
		util::matrix_t<double> q_dot_conv; // Panel convection loss (W)
		util::matrix_t<double> q_dot_rad;  // Panel radiation loss (W)
		util::matrix_t<double> q_dot_loss; // Panel convection + radiation loss (W)
		util::matrix_t<double> q_dot_abs;  // Panel energy to HTF (W)

		s_steady_state_soln()
		{
			clear();
		}

		void clear()
		{
			hour = T_amb = T_dp = v_wind_10 = p_amb = std::numeric_limits<double>::quiet_NaN();
			dni = od_control = field_eff = m_dot_salt = m_dot_salt_tot = T_salt_cold_in = T_salt_hot = T_salt_hot_rec = T_salt_props = std::numeric_limits<double>::quiet_NaN();
			u_salt = f = Q_inc_sum = Q_conv_sum = Q_rad_sum = Q_abs_sum = Q_dot_piping_loss = Q_inc_min = Q_thermal = eta_therm = std::numeric_limits<double>::quiet_NaN();
            mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
            itermode = -1;
			rec_is_off = true;
		}
	};

	s_steady_state_soln m_mflow_soln_prev;  // Steady state solution using actual DNI from the last call to the model
	s_steady_state_soln m_mflow_soln_csky_prev;  // Steady state solution using clear-sky DNI from the last call to the model

	//-------------------
	//Transient model parameters
	int m_startup_mode;
	int m_startup_mode_initial;
	int m_n_call_fill;
	int m_n_call_fill_initial;
	double m_id_riser;				//[m]
	double m_od_riser;				//[m]
	double m_id_downc;				//[m]
	double m_od_downc;				//[m]
	double m_Rtot_riser;		//[K*m/W]
	double m_Rtot_downc;		//[K*m/W]
	double m_total_startup_time; // [s]
	double m_total_startup_time_initial; //[s]
	double m_minimum_startup_time; //s
	double m_total_ramping_time; //s
	double m_total_ramping_time_initial; //s
	double m_total_fill_time;
	double m_total_fill_time_initial;
	double m_total_preheat_time;
	double m_total_preheat_time_initial;
	int m_crossover_index;


	int m_n_elem;
	int m_nz_tot;
	std::vector<double> m_tm;		 //[J/K/m]
    std::vector<double> m_tm_solid;	//[J/K/m]
    std::vector<double> m_od;		 //[m]
    std::vector<double> m_id;		 //[m]
	util::matrix_t<int> m_flowelem_type;
	util::matrix_t<double> m_tinit;
	util::matrix_t<double> m_tinit_wall;

	struct transient_inputs
	{
		size_t nelem;
		size_t nztot;
		size_t npath;
		double inlet_temp;
		util::matrix_t<double> lam1, lam2, cval, aval, tinit, tinit_wall, Rtube;
        std::vector<double> length, zpts;
        std::vector<int> nz, startpt;

		transient_inputs()
		{
			nelem = nztot = npath = 0;
			inlet_temp = std::numeric_limits<double>::quiet_NaN();
		}

	} trans_inputs;

	struct transient_outputs
	{
		double timeavg_tout;					// Time-averaged downcomer outlet T [K]
		double tout;							// Downcomer outlet T at the end of the time step [K] 
		double max_tout;						// Max downcomer outlet T during the time step [K]
		double min_tout;						// Min downcomer outlet T during the time step [K]
		double max_rec_tout;					// Max receiver outlet T during the time step [K]
		double timeavg_conv_loss;				// Time-averaged convection loss from the receiver panels [W]
		double timeavg_rad_loss;				// Time-averaged radiation loss
		double timeavg_piping_loss;				// Time-averaged thermal loss from piping [W]
		double timeavg_qthermal;				// Average thermal power sent to power cycle or storage during the time step [W]
		double timeavg_qnet;					// Average net thermal power absorbed by the receiver during the time step [W]
		double timeavg_qheattrace;				// Average heat trace thermal input during the time step [W]
		double timeavg_eta_therm;				// Time-averaged thermal efficiency of the receiver 
		double time_min_tout;					// Time at which minimum downcomer outlet T occurs
		double tube_temp_inlet;					// Receiver inlet tube wall temperature at the end of the the time step [K]
		double tube_temp_outlet;				// Receiver outlet tube wall temperature at the end of the the time step [K]

		util::matrix_t<double> t_profile;		// Axial temperature profile at the end of the time step[K]
		util::matrix_t<double> t_profile_wall;	// Axial wall temperature profile at the end of the time step[K]
		util::matrix_t<double> timeavg_temp;	// Time-average outlet temperature of each flow element [K]

		transient_outputs()
		{
			timeavg_tout = tout = max_tout = min_tout = max_rec_tout = timeavg_conv_loss = timeavg_rad_loss = timeavg_piping_loss = std::numeric_limits<double>::quiet_NaN();
			timeavg_qthermal = timeavg_qnet = timeavg_qheattrace  = timeavg_eta_therm = time_min_tout = tube_temp_inlet = tube_temp_outlet = std::numeric_limits<double>::quiet_NaN();
		}

	} trans_outputs;

	struct parameter_eval_inputs
	{
		double T_amb, T_sky, pres, wspd, c_htf, rho_htf, mu_htf, k_htf, Pr_htf, mflow_tot, finitial, ffinal, ramptime;
		std::vector<double> tm;
		util::matrix_t<double> Tfeval, Tseval, qinc, qheattrace;
		
		parameter_eval_inputs()
		{
			T_amb = T_sky = pres = wspd = c_htf = rho_htf = mu_htf = k_htf = Pr_htf = mflow_tot = finitial = ffinal = ramptime = std::numeric_limits<double>::quiet_NaN();
		}

	} param_inputs;



	void initialize_transient_parameters();
	bool use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev);
	util::matrix_t<double> calculate_flux_profiles(double dni, double field_eff, double od_control, const util::matrix_t<double> *flux_map_input);
	void calculate_steady_state_soln(s_steady_state_soln &soln, double tol, int max_iter = 50);
	void solve_for_mass_flow(s_steady_state_soln &soln);
	void solve_for_mass_flow_and_defocus(s_steady_state_soln &soln, double m_dot_htf_max, const util::matrix_t<double> *flux_map_input);
	void solve_for_defocus_given_flow(s_steady_state_soln &soln, const util::matrix_t<double> *flux_map_input);



	double calc_external_convection_coeff(double T_amb, double P_amb, double wspd, double Twall);
	void calc_thermal_loss(double Ts, double T_amb, double T_sky, double P_amb, double wspd, double &hext, double &qconv, double &qrad);
	void calc_surface_temperature(double Tf, double qabs, double Rtube, double OD, double T_amb, double T_sky, double P_amb, double wspd, double &Tsguess);
	void calc_header_size(double pdrop, double mdot, double rhof, double muf, double Lh, double &id_calc, double &th_calc, double &od_calc);
	double interpolate(double x, const std::vector<double> &xarray, const std::vector<double> &yarray, int klow, int khigh);
	double integrate(double xlow, double xhigh, const std::vector<double> &xarray, const std::vector<double> &yarray, int klow, int khigh);
	void cubic_splines(const std::vector<double> &xarray, const std::vector<double> &yarray, util::matrix_t<double> &splines);
	double calc_single_pt(double tpt, double zpt, int flowid, int pathid, const transient_inputs &tinputs);
	double calc_timeavg_exit_temp(double tstep, int flowid, int pathid, const transient_inputs &tinputs);

	
	void calc_ss_profile(const transient_inputs &tinputs, util::matrix_t<double> &tprofile, util::matrix_t<double> &tprofile_wall);
	void calc_axial_profile( double tpt, const transient_inputs &tinputs, util::matrix_t<double> &tprofile);
	void calc_extreme_outlet_values(double tstep, int flowid, const transient_inputs &tinputs, util::matrix_t<double> &textreme, util::matrix_t<double> &tpt);	
	void initialize_transient_param_inputs(const s_steady_state_soln &soln, parameter_eval_inputs &pinputs);
	void update_pde_parameters(bool use_initial_t, parameter_eval_inputs &pinputs, transient_inputs &tinputs);
	void solve_transient_model(double tstep, double allowable_Trise, parameter_eval_inputs &pinputs, transient_inputs &tinputs, transient_outputs &toutputs);
	void solve_transient_startup_model(parameter_eval_inputs &pinputs, transient_inputs &tinputs, int startup_mode, double target_temperature, double min_time, double max_time, transient_outputs &toutputs, double &startup_time, double &energy, double &parasitic);
	void set_heattrace_power(bool is_maintain_T, double Ttarget, double time, parameter_eval_inputs &pinputs, transient_inputs &tinputs);

	enum startup_modes
	{
		HEAT_TRACE = 0,		// No flux on receiver, riser/downcomer heated with heat tracing
		PREHEAT,			// Low flux on receiver, no HTF flow
		PREHEAT_HOLD,		// Preheat temperature requirement has been met, but not time requirement.  Low flux on receiver, no HTF flow
		FILL,				// User-defined time delay for filling the receiver/piping
		CIRCULATE,			// Full available power on receiver (with optional ramp rate), HTF mass flow rate selected to hit target hot at steady state
		HOLD				// Models predict that startup has been completed, but minimum startup time has not yet been reached.  Fluid continues to circulate through the receiver.  
	};


public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Data
	int m_n_panels;					//[-]
	double m_d_rec;					//[m]
	double m_h_rec;					//[m]
	double m_od_tube;				//[mm], convert to [m] in init()
	double m_th_tube;				//[mm], convert to [m] in init()
	double m_hl_ffact;				//[-]
	double m_A_sf;					//[m2]

	// 8.10.2015 twn: add tower piping thermal losses to receiver performance
	double m_pipe_loss_per_m;		//[Wt/m]
	double m_pipe_length_add;		//[m]
	double m_pipe_length_mult;		//[-]

	// 7.13.17 twn: keep this public for now so iscc can calculate
	double m_m_dot_htf_max;			//[kg/s]


	int m_n_flux_x;
	int m_n_flux_y;

	// Transient model 
	bool m_is_transient;			// Use transient model?
	bool m_is_startup_transient;	// Use transient startup model?
	double m_rec_tm_mult;			//[-], receiver thermal mass multiplier
	double m_u_riser;				//[m/s], 
	double m_th_riser;				//[mm], convert to [m] in init()
	double m_th_downc;				//[mm], convert to [m] in init()
	double m_piping_loss_coeff;		//[W/m2/K]
	double m_riser_tm_mult;			//[-], riser thermal mass multiplier
	double m_downc_tm_mult;			//[-], downcomer thermal mass multiplier
	double m_heat_trace_power;		//[kW/m], convert to [W/m] in init()
	double m_tube_flux_preheat;		//[kW/m2]
	double m_min_preheat_time;      //[hr], convert to [s] in init(), Minimum time required in preheat startup stage
	double m_fill_time;				//[hr], convert to [s] in init(), Time requirement to fill receiver
	double m_flux_ramp_time;		//[hr], convert to [s] in init()
	double m_preheat_target;		//[C], convert to [k] in init()
	double m_startup_target_delta;	//[C], (target temperature at end of startup) - (steady state temperature at current conditions )
	double m_initial_temperature;	//[C], convert to [K] in init()

	bool m_is_startup_from_solved_profile;  // Begin receiver startup from solved temperature profiles?
	bool m_is_enforce_min_startup;		// Always enforce minimum startup time?  If false, minimum startup time is ignored when receiver starts above preheat temperature

	// 4.17.15 twn: former TCS inputs, moved to member data because are constant throughout simulation
	double m_T_salt_hot_target;			//[C], convert to K in init() call
	double m_hel_stow_deploy;			//[-]

	// Added for csp_solver/tcs wrappers:
	int m_field_fl;
	util::matrix_t<double> m_field_fl_props;	
	int m_mat_tube;
	int m_flow_type;
    int m_crossover_shift;

	// ISCC specific
	bool m_is_iscc;
	int m_cycle_config;
	
	// Flow control
	double m_csky_frac;

	S_outputs outputs;

	// Methods
	C_mspt_receiver();

	~C_mspt_receiver(){};

	virtual void init();

	virtual void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_pt_receiver::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

    void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc,
        double &WdotPump_calc);

    virtual double get_pumping_parasitic_coef();

	void est_startup_time_energy(double fract, double &est_time, double &est_energy);

	double est_heattrace_energy();

    virtual double get_startup_time();

    virtual double get_startup_energy();

    virtual double area_proj();
};

#endif // __csp_solver_mspt_receiver_222_
