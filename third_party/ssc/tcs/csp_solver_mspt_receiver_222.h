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

#ifndef __csp_solver_mspt_receiver_222_
#define __csp_solver_mspt_receiver_222_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "ngcc_powerblock.h"
#include "csp_solver_core.h"
#include "csp_solver_pt_receiver.h"

class C_mspt_receiver_222 : public C_pt_receiver
{
// The steady-state receiver (as opposed to the transient, for example)

private:
	ngcc_power_cycle cycle_calcs;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	double m_A_rec_proj;
	double m_A_node;
	
	double m_Q_dot_piping_loss;		//[Wt] = Constant thermal losses from piping to env. = (THT*length_mult + length_add) * piping_loss_coef
	double m_piping_loss_coeff;		//[W/m2/K]

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

	double m_Rtot_riser;
	double m_Rtot_downc;

	struct s_steady_state_soln
	{
		C_csp_collector_receiver::E_csp_cr_modes mode;
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

	bool use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev);
	util::matrix_t<double> calculate_flux_profiles(double dni, double field_eff, double od_control, const util::matrix_t<double> *flux_map_input);
	void calculate_steady_state_soln(s_steady_state_soln &soln, double tol, int max_iter = 50);
	void solve_for_mass_flow(s_steady_state_soln &soln);
	void solve_for_mass_flow_and_defocus(s_steady_state_soln &soln, double m_dot_htf_max, const util::matrix_t<double> *flux_map_input);
	void solve_for_defocus_given_flow(s_steady_state_soln &soln, const util::matrix_t<double> *flux_map_input);

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
	C_mspt_receiver_222();

	~C_mspt_receiver_222(){};

	virtual void init();

	virtual void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_pt_receiver::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

    void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc);

    virtual double get_pumping_parasitic_coef();

    virtual double area_proj();

};

#endif // __csp_solver_mspt_receiver_222_
