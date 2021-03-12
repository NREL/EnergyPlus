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

#ifndef __csp_solver_lf_dsg_collector_receiver_
#define __csp_solver_lf_dsg_collector_receiver_

#include "csp_solver_core.h"
#include "htf_props.h"
#include "tcstype.h"

#include "lib_weatherfile.h"
#include <cmath>
#include "sam_csp_util.h"
#include "water_properties.h"

#include "numeric_solvers.h"

#include <iosfwd>

class C_csp_lf_dsg_collector_receiver : public C_csp_collector_receiver
{

public:

	enum
	{
		E_THETA_TRAVERSE,		//[deg]
		E_THETA_LONGITUDINAL,	//[deg]
		E_ETA_OPTICAL,			//[-]
		E_DEFOCUS,				//[-]

		E_Q_DOT_INC_SF_TOT,			//[MWt]
		E_Q_DOT_REC_INC,			//[MWt]
		E_Q_DOT_REC_THERMAL_LOSS,	//[MWt]
		E_Q_DOT_REC_ABS,			//[MWt]
		E_Q_DOT_PIPING_LOSS,		//[MWt]
		E_E_DOT_INTERNAL_ENERGY,	//[MWt]
		E_Q_DOT_OUT,				//[MWt]
		E_Q_DOT_FREEZE_PROT,		//[MWt]

		E_M_DOT_LOOP,		//[kg/s]
		E_M_DOT_FIELD,		//[kg/s]
		E_T_FIELD_COLD_IN,	//[C]
		E_T_REC_COLD_IN,	//[C]
		E_T_REC_HOT_OUT,	//[C]
		E_X_REC_HOT_OUT,	//[-]
		E_T_FIELD_HOT_OUT,	//[C]
		E_X_FIELD_HOT_OUT,	//[-]
		E_PRESSURE_DROP,	//[bar]

		E_W_DOT_SCA_TRACK,	//[MWe]
		E_W_DOT_PUMP		//[MWe]
	};

	C_csp_reported_outputs mc_reported_outputs;

private:

	// *******************************************
	// Hardcoded member data
		// Timestep management
	double m_step_recirc;		//[s] 
		// Conversion constants
	double m_d2r;				//[deg/rad] Degree-to-radian conversion
	double m_r2d;				//[rad/deg] Radian-to-degree conversion
	double m_mtoinch;			//[m/in] meter-to-inch conversion
		// DSG Model Constants
	double m_P_max;				//[bar]
	double m_fP_turb_min;		//[-] Minimum fractional operating pressure (of design) at the turbine inlet
		// Water props limits
	double m_wp_max_temp;		//[K]
	double m_wp_min_temp;		//[K]
	double m_wp_T_crit;			//[K]
	double m_wp_max_pres;		//[kPa]
	double m_wp_min_pres;		//[kPa]
	// *******************************************
	// *******************************************

	// *******************************************
	// Design Calculations
		// Geometry and Layout
	util::matrix_t<double> m_D_h;		//[m]
	util::matrix_t<double> m_A_cs;		//[m2]
	util::matrix_t<double> m_EPSILON_5;	//[-]
	util::matrix_t<double> m_eta_opt_fixed;
	util::matrix_t<double> m_opteff_des;
	double m_C_thermal;			//[kJ/K] Thermal capacity per receiver
	double m_fP_sf_tot;			//[-] Total fractional pressure drop across the solar field
	int m_n_rows_matrix;		//[-] 1 if Single Geom, 2 if Multigeom
	int m_nModTot;				//[-] nBoiler + nSH
	bool m_is_sh;				//[-]
	double m_Ap_tot;			//[m2] Total solar field aperture area	
	double m_Ap_loop;			//[m2] Loop solar field aperture area
	double m_opt_eta_des;		//[-] Design point optical efficiency (theta = 0) from the solar field
		// Energy and mass balance calcs
	double m_q_dot_abs_tot_des;	//[kWt] SYSTEM total thermal power absorbed by steam at design
    double m_q_dot_loss_tot_des;//[kWt]
    double m_m_dot_min;			//[kg/s] LOOP min mass flow rate - max of field & PC bases
	double m_m_dot_max;			//[kg/s] LOOP max mass flow rate - min of field & PC bases
	double m_m_dot_b_max;		//[kg/s] LOOP max mass flow rate through boiler (m_m_dot_max/x_b_des)
	double m_m_dot_b_des;		//[kg/s] SYSTEM mass flow rate at power cycle design
	double m_m_dot_pb_des;		//[kg/s] SYSTEM mass flow rate at power cycle design
	double m_m_dot_des;			//[kg/s] SYSTEM design point mass flow rate - field design basis
	double m_m_dot_loop_des;	//[kg/s] LOOP design point mass flow rate

	double m_W_dot_sca_tracking_nom;	//[MWe] Tracking parasitics when trough is on sun
	// *******************************************
	// *******************************************

	// *******************************************
	// Timestep Calculations
		// Control & operation
	int m_operating_mode_converged;
	int m_operating_mode;
	int m_ncall;
		// *********************************************
		// CSP Solver Temperature Tracking
		// *********************************************
			// Temperatures from the most recent converged() operation
				// SUB TIMESTEP outputs
	C_csp_solver_steam_state mc_sys_cold_out_t_end_converged;			// End-of-previously-converged-timestep outlet condition from cold system/header/field
	std::vector<C_csp_solver_steam_state> mc_sca_out_t_end_converged;	// End-of-previously-converged-timestep outlet condition from each SCA in loop
	C_csp_solver_steam_state mc_sys_hot_out_t_end_converged;			// End-of-previously-converged-timestep outlet condition hot cold system/header/field
			// Temperature from the most recent timestep (in the event that a method solves multiple, shorter timesteps
	C_csp_solver_steam_state mc_sys_cold_out_t_end_last;			// End-of-previous-timestep outlet condition from cold system/header/field
	std::vector<C_csp_solver_steam_state> mc_sca_out_t_end_last;	// End-of-previous-timestep outlet condition from each SCA in loop
	C_csp_solver_steam_state mc_sys_hot_out_t_end_last;				// End-of-previous-timestep outlet condition hot cold system/header/field

			// Latest temperature solved in loop_energy_balance
	C_csp_solver_steam_state mc_sys_cold_in_t_int;		// Time-integrated inlet condition to cold system/header/field
	C_csp_solver_steam_state mc_sys_cold_out_t_end;		// End-of-timestep outlet condition from cold system/header/field
	C_csp_solver_steam_state mc_sys_cold_out_t_int;		// Time-integrated outlet condition from cold system/header/field
	
	std::vector<C_csp_solver_steam_state> mc_sca_in_t_int;     // Time-integrated inlet condition to each SCA in loop
	std::vector<C_csp_solver_steam_state> mc_sca_out_t_end;	   // End-of-timestep outlet condition from each SCA in loop
	std::vector<C_csp_solver_steam_state> mc_sca_out_t_int;	   // Time-integrated outlet condition from each SCA in loop
	
	C_csp_solver_steam_state mc_sys_hot_in_t_int;		// Time-integrated inlet condition to hot system/header/field
	C_csp_solver_steam_state mc_sys_hot_out_t_end;		// End-of-timestep outlet condition hot cold system/header/field
	C_csp_solver_steam_state mc_sys_hot_out_t_int;		// Time-integrated outlet condition hot cold system/header/field

	double m_q_dot_sca_loss_summed_subts;		//[MWt] SYSTEM SCA heat loss
	double m_q_dot_sca_abs_summed_subts;		//[MWt] SYSTEM SCA absorbed thermal power (into HTF stream & material)
	double m_q_dot_HR_cold_loss_subts;			//[MWt] SYSTEM Cold header heat loss
	double m_q_dot_HR_hot_loss_subts;			//[MWt] SYSTEM Hot header heat loss
	double m_E_dot_sca_summed_subts;			//[MWt] SYSTEM SCA internal energy change over time

	double m_q_dot_to_sink_subts;			//[MWt] SYSTEM thermal power to sink (or artificially added to system in recirculation...)
	
			// Full timestep outputs
	double m_h_sys_c_in_t_int_fullts;		//[kJ/kg] Temperature (bulk) of cold runners & headers integrated over subtimesteps
	double m_P_sys_c_in_t_int_fullts;		//[bar] Pressure integrated over subtimesteps
	double m_h_c_rec_in_t_int_fullts;		//[kJ/kg] Time-integrated-average inlet HTF temperature to FIRST sca
	double m_P_c_rec_in_t_int_fullts;		//[bar]
	double m_h_h_rec_out_t_int_fullts;		//[kJ/kg] Time-integrated-average outlet HTF temperature from LAST sca
	double m_P_h_rec_out_t_int_fullts;		//[bar]
	double m_h_sys_h_out_t_int_fullts;		//[kJ/kg] Temperature (bulk) of hot runners & headers at timestep-integrated-average
	double m_P_sys_h_out_t_int_fullts;		//[bar]


	double m_q_dot_sca_loss_summed_fullts;		//[MWt] SYSTEM SCA heat loss 
	double m_q_dot_sca_abs_summed_fullts;		//[MWt] SYSTEM SCA absorbed thermal power (into HTF stream & material)
	double m_q_dot_HR_cold_loss_fullts;			//[MWt] SYSTEM Cold header heat loss
	double m_q_dot_HR_hot_loss_fullts;			//[MWt] SYSTEM Hot header heat loss
	double m_E_dot_sca_summed_fullts;			//[MWt] SYSTEM SCA internal energy change over time

	double m_q_dot_to_sink_fullts;			//[MWt] SYSTEM thermal power to sink (or artificially added to system in recirculation...)			
	double m_q_dot_freeze_protection;		//[MWt] SYSTEM thermal freeze protection
	
	double m_m_dot_loop;				//[kg/s] LOOP mass flow rate

	double m_W_dot_sca_tracking;		//[MWe]
	double m_W_dot_pump;				//[MWe] FIELD pumping power

		// *********************************************
		// TCS Shell Stuff State-Point Tracking
		// Hot System/Field Headers Outlet
	C_csp_solver_steam_state mc_sys_h_out;
	std::vector<double> m_T_ave_prev;
	util::matrix_t<double> m_T_ave;
	util::matrix_t<double> m_h_in;
	util::matrix_t<double> m_h_out;
		// *********************************************
		// *********************************************
	// ****************************************************************
		// ****************************************************************
		// Sun Position
	double m_phi_t;			//[rad] Traverse incident angle
	double m_theta_L;		//[rad] Longitudinal incident angle
		// Optical calcs
	double m_ftrack;		//[-] Fraction of timestep that solar field is tracking
	double m_eta_opt;		//[-] Field optical efficiency before defocus

	double m_control_defocus;	//[-] Defocus signal from control model
	double m_component_defocus;	//[-] Defocus signal from this component (max mass flow rate reached and still over target...)

		// Energy Balance
			//  Single Values
	double m_q_dot_inc_sf_tot;	//[MWt] Total incident radiation on the solar field

	std::vector<double> m_q_inc;			//[kWt] Incident beam radiation for each receiver in loop
	std::vector<double> m_q_inc_control_df;	//[kWt] Incident beam radiation for each receiver in loop with CONTROL defocus
	util::matrix_t<double> m_eta_optical;	//[-] Optical efficiency for each collector geometry
	std::vector<double> m_q_rec;			//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*
	std::vector<double> m_q_rec_control_df;	//[kWt] Incident thermal power on receiver after *optical* and CONTROL defocus
	std::vector<double> m_q_loss;		//[kWt] Thermal loss for each receiver in loop
	std::vector<double> m_q_abs;		//[kWt] Thermal power absorbed by steam in each receiver
	
	double m_Q_field_losses_total;		//[MJ] scas + xover + hot_HR + cold_HR
	double m_q_rec_loop;		//[kWt] LOOP total thermal power on receiver after *optical* losses and *defocus*
	double m_q_inc_loop;		//[kWt] LOOP total incident beam radiation

	// *********************************************
	// Required for backwards compatability with TCS - call & init & converged only!
	// *********************************************
	double m_defocus_prev;	//[-]
	double m_t_sby_prev;	//[-]
	double m_t_sby;			//[-]
	bool m_is_pb_on_prev;	//[-]
	bool m_is_pb_on;		//[-]
	double m_T_sys_prev;	//[K]
	double m_defocus;		//[-]
	bool m_is_def;			//[-]
	double m_err_def;		//[-]
	double m_tol_def;		//[-]
	double m_rc;			//[-]
	// *********************************************
	// *********************************************

	int freeze_protection(const C_csp_weatherreader::S_outputs &weather, double P_field_out /*bar*/, 
		double T_cold_in /*K*/, double m_dot_loop /*kg/s*/, double h_sca_out_target /*kJ/kg*/, 
		const C_csp_solver_sim_info &sim_info, double & Q_fp /*MJ*/);

	double od_pressure(double m_dot_loop /*kg/s*/);

	void apply_component_defocus(double defocus /*-*/);

	void set_output_values();

	int n_integration_steps;

public:

	// Class to save messages for up stream classes
	//C_csp_messages mc_csp_messages;

	//parameters and inputs
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
	double m_q_max_aux;			//[kWt] 
	double m_LHV_eff;			//[-]
	double m_T_set_aux;			//[K]
	double m_T_field_in_des;	//[K]
	double m_T_field_out_des;	//[K]
	double m_x_b_des;			//[-]
	double m_P_turb_des;		//[bar]
	double m_fP_hdr_c;			//[-]
	double m_fP_sf_boil;		//[-]
	double m_fP_boil_to_sh;		//[-]
	double m_fP_sf_sh;			//[-]
	double m_fP_hdr_h;			//[-]
	double m_q_pb_des;			//[kWt]
	double m_W_pb_des;			//[kWe]
	double m_cycle_cutoff_frac;		//[-]
	double m_cycle_max_fraction;	//[-]
	double m_m_dot_min_frac;	//[-]
	double m_m_dot_max_frac;	//[-]
	double m_t_sby_des;			//[hr]
	double m_q_sby_frac;		//[-]
	double m_PB_fixed_par;		//[-]
	std::vector<double> m_bop_array;
	std::vector<double> m_aux_array;
	double m_T_startup;			//[K]

	int m_fossil_mode;			//[-]
	double m_I_bn_des;			//[W/m2]
	bool m_is_oncethru;			//[-]
	
	// True = target outlet state is superheat and uses(m_T_field_out_des), False = target outlet state is 2-phase and uses (m_x_b_des)
	bool m_is_sh_target;		//[-] 
	
	bool m_is_multgeom;			//[-]
	int m_nModBoil;				//[-]
	int m_nModSH;				//[-]
	int m_nLoops;				//[-]
	double m_eta_pump;			//[-]
	double m_latitude;			//[rad]
	double m_theta_stow;		//[rad]
	double m_theta_dep;			//[rad]
	double m_T_field_ini;		//[K]
	double m_T_fp;				//[K]
	double m_Pipe_hl_coef;		//[W/m2-K]
	double m_SCA_drives_elec;	//[W/SCA]
	double m_ColAz;				//[rad]
	double m_e_startup;			//[kJ/K-m2], Thermal inertia contribution per sq meter of solar field
	double m_T_amb_des_sf;		//[K]
	double m_V_wind_max;		//[m/s]
	std::vector<double> m_ffrac;

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
	util::matrix_t<double> m_AbsorberMaterial_in;
	util::matrix_t<double> m_b_eps_HCE1;
	util::matrix_t<double> m_b_eps_HCE2;
	util::matrix_t<double> m_b_eps_HCE3;
	util::matrix_t<double> m_b_eps_HCE4;
	util::matrix_t<double> m_sh_eps_HCE1;
	util::matrix_t<double> m_sh_eps_HCE2;
	util::matrix_t<double> m_sh_eps_HCE3;
	util::matrix_t<double> m_sh_eps_HCE4;
	util::matrix_t<double> m_HCE_FieldFrac;
	util::matrix_t<double> m_alpha_abs;
	util::matrix_t<double> m_alpha_env;
	util::matrix_t<double> m_EPSILON_4;
	util::matrix_t<double> m_Tau_envelope;
	util::matrix_t<bool> m_GlazingIntactIn;
	util::matrix_t<double> m_AnnulusGas_in;
	util::matrix_t<double> m_P_a;
	util::matrix_t<double> m_Design_loss;
	util::matrix_t<double> m_Shadowing;
	util::matrix_t<double> m_Dirt_HCE;
	util::matrix_t<double> m_b_OpticalTable;
	util::matrix_t<double> m_sh_OpticalTable;	

	// **************************************************************************
	// **************************************************************************
	// **************************************************************************

	C_csp_lf_dsg_collector_receiver();

	~C_csp_lf_dsg_collector_receiver(){};

		// Trunk version
	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
		C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual int get_operating_state();

	virtual double get_startup_time();
	virtual double get_startup_energy(); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt
	virtual double get_tracking_power();		//MWe
	virtual double get_col_startup_power();		//MWe-hr

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);
	
	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	double turb_pres_frac(double m_dot_nd, int fmode, double ffrac, double fP_min);

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	class E_piping_config
	{
	public:
		enum
		{
			FIELD = 1,
			LOOP
		};
	};
	class E_loop_energy_balance_exit
	{
	public:
		enum
		{
			SOLVED,
			NaN
		};
	};

	void loop_optical_eta(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_sim_info &sim_info);

	void loop_optical_eta_off();

	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	void transient_energy_bal_numeric_int_ave(double h_in /*kJ/kg*/, double P_in /*kPa*/,
		double q_dot_abs /*kWt*/, double m_dot /*kg/s*/, 
		double T_out_t_end_prev /*K*/,
		double C_thermal /*kJ/K*/, double step /*s*/, 
		double & h_out_t_end /*kJ/K*/, double & h_out_t_int /*kJ/K*/);

	void transient_energy_bal_numeric_int(double h_in /*kJ/kg*/, double P_in /*kPa*/,
			double q_dot_abs /*kWt*/, double m_dot /*kg/s*/, 
			double T_out_t_end_prev /*K*/,
			double C_thermal /*kJ/K*/, double step /*s*/, 
			double & h_out_t_end_prev /*kJ/K*/, double & h_out_t_end /*kJ/K*/, double & T_out_t_end /*K*/);

	int once_thru_loop_energy_balance_T_t_int(const C_csp_weatherreader::S_outputs &weather,
		double T_cold_in /*K*/, double P_field_out /*bar*/, double m_dot_loop /*kg/s*/, double h_sca_out_target /*kJ/kg*/,
		const C_csp_solver_sim_info &sim_info);

	void update_last_temps();

	void reset_last_temps();

	class C_mono_eq_transient_energy_bal : public C_monotonic_equation
	{
	private:
		water_state mc_wp;

		double m_h_in;		//[kJ/kg]
		double m_P_in;		//[kPa]
		double m_q_dot_abs;	//[kWt]
		double m_m_dot;		//[kg/s]
		double m_T_out_t_end_prev;	//[K]
		double m_h_out_t_end_prev;	//[kJ/kg]
		double m_C_thermal;	//[kJ/K]
		double m_step;		//[s]

	public:
		C_mono_eq_transient_energy_bal(double h_in /*kJ/kg*/, double P_in /*kPa*/,
			double q_dot_abs /*kWt*/, double m_dot /*kg/s*/, double T_out_t_end_prev /*K*/, 
			double h_out_t_end_prev /*kJ/kg*/, double C_thermal /*kJ/K*/, double step /*s*/)
		{
			m_h_in = h_in; m_P_in = P_in; m_q_dot_abs = q_dot_abs; m_m_dot = m_dot;
				m_T_out_t_end_prev = T_out_t_end_prev; 
				m_h_out_t_end_prev = h_out_t_end_prev;
				m_C_thermal = C_thermal; m_step = step;

			m_T_out_t_end = std::numeric_limits<double>::quiet_NaN();
		}

		double m_T_out_t_end;	//[K]

		virtual int operator()(double h_out_t_end /*K*/, double *diff_T_out_t_end /*-*/);
	};

	class C_mono_eq_freeze_prot_E_bal : public C_monotonic_equation
	{	// The solver chooses a cold inlet temperature and sends it to the operator. The operator
		//		calls the loop energy balance at the recirculation mass flow rate
		//		and returns the total field heat loss. The solver finds the T_cold_in such that E_fp = E_losses
	private:
		C_csp_lf_dsg_collector_receiver *mpc_dsg_lf;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_P_field_out;	//[bar]
		double m_m_dot_loop;	//[kg/s]
		double m_h_sca_out_target;	//[kJ/kg]
		C_csp_solver_sim_info ms_sim_info;

	public:

		double m_Q_fp;		//[MJ]

		C_mono_eq_freeze_prot_E_bal(C_csp_lf_dsg_collector_receiver *pc_dsg_lf, const C_csp_weatherreader::S_outputs &weather,
						double P_field_out /*bar*/, double m_dot_loop /*kg/s*/, 
						double h_sca_out_target /*kJ/kg*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_dsg_lf = pc_dsg_lf;
			ms_weather = weather;
			m_P_field_out = P_field_out;	//[bar]
			m_m_dot_loop = m_dot_loop;		//[kg/s]
			m_h_sca_out_target = h_sca_out_target;	//[kJ/kg]
			ms_sim_info = sim_info;

			m_Q_fp = std::numeric_limits<double>::quiet_NaN();	//[MJ]
		}
	
		virtual int operator()(double T_cold_in /*K*/, double *E_loss_balance /*-*/);	
	};

	class C_mono_eq_h_loop_out_target : public C_monotonic_equation
	{
	private:
		C_csp_lf_dsg_collector_receiver *mpc_dsg_lf;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_T_cold_in;		//[K]
		C_csp_solver_sim_info ms_sim_info;

	public:
		C_mono_eq_h_loop_out_target(C_csp_lf_dsg_collector_receiver *pc_dsg_lf, const C_csp_weatherreader::S_outputs &weather,
			double T_cold_in /*K*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_dsg_lf = pc_dsg_lf;
			ms_weather = weather;
			m_T_cold_in = T_cold_in;		//[K]
			ms_sim_info = sim_info;

			m_P_field_out = std::numeric_limits<double>::quiet_NaN();
			m_h_sca_out_target = std::numeric_limits<double>::quiet_NaN();
		}

		double m_P_field_out;		//[bar]
		double m_h_sca_out_target;	//[kJ/kg]
	
		virtual int operator()(double m_dot_loop /*kg/s*/, double *diff_h_loop_out /*kJ/kg*/);
	};

	class C_mono_eq_defocus : public C_monotonic_equation
	{
	private:
		C_csp_lf_dsg_collector_receiver *mpc_dsg_lf;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_T_cold_in;		//[K]
		double m_P_field_out;	//[bar]
		double m_m_dot_loop;	//[kg/s]
		double m_h_sca_out_target;	//[kJ/kg]
		C_csp_solver_sim_info ms_sim_info;

	public:
		C_mono_eq_defocus(C_csp_lf_dsg_collector_receiver *pc_dsg_lf, const C_csp_weatherreader::S_outputs &weather,
			double T_cold_in /*K*/, double P_field_out /*bar*/, double m_dot_loop /*kg/s*/,
			double h_sca_out_target /*kJ/kg*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_dsg_lf = pc_dsg_lf;
			ms_weather = weather;
			m_T_cold_in = T_cold_in;		//[K]
			m_P_field_out = P_field_out;	//[bar]
			m_m_dot_loop = m_dot_loop;		//[kg/s]
			m_h_sca_out_target = h_sca_out_target;	//[kJ/kg]
			ms_sim_info = sim_info;
		}
	
		virtual int operator()(double defocus /*-*/, double *diff_h_loop_out /*kJ/kg*/);
	};

};


#endif
