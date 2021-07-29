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

#include "csp_solver_mspt_receiver_222.h"
#include "csp_solver_core.h"

#include "Ambient.h"
#include "definitions.h"

C_mspt_receiver_222::C_mspt_receiver_222()
{
	m_n_panels = -1;

	m_d_rec = std::numeric_limits<double>::quiet_NaN();
	m_h_rec = std::numeric_limits<double>::quiet_NaN();
	m_od_tube = std::numeric_limits<double>::quiet_NaN();
	m_th_tube = std::numeric_limits<double>::quiet_NaN();
	m_hl_ffact = std::numeric_limits<double>::quiet_NaN();
	m_A_sf = std::numeric_limits<double>::quiet_NaN();

	m_pipe_loss_per_m = std::numeric_limits<double>::quiet_NaN();
	m_pipe_length_add = std::numeric_limits<double>::quiet_NaN();
	m_pipe_length_mult = std::numeric_limits<double>::quiet_NaN();

	m_id_tube = std::numeric_limits<double>::quiet_NaN();
	m_A_tube = std::numeric_limits<double>::quiet_NaN();
	m_n_t = -1;
	m_n_flux_x = 0;
	m_n_flux_y = 0;

	m_T_salt_hot_target = std::numeric_limits<double>::quiet_NaN();
	m_eta_pump = std::numeric_limits<double>::quiet_NaN();
	m_night_recirc = -1;
	m_hel_stow_deploy = std::numeric_limits<double>::quiet_NaN();

		// Added for csp_solver/tcs wrapper
	m_field_fl = -1;
	m_mat_tube = -1;
	m_flow_type = -1;
    m_crossover_shift = 0;

	m_A_rec_proj = std::numeric_limits<double>::quiet_NaN();
	m_A_node = std::numeric_limits<double>::quiet_NaN();

	m_Q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
	m_piping_loss_coeff = std::numeric_limits<double>::quiet_NaN();
	m_m_dot_htf_max = std::numeric_limits<double>::quiet_NaN();

	m_itermode = -1;
	m_od_control = std::numeric_limits<double>::quiet_NaN();
	m_eta_field_iter_prev = std::numeric_limits<double>::quiet_NaN();
	m_tol_od = std::numeric_limits<double>::quiet_NaN();
	m_q_dot_inc_min = std::numeric_limits<double>::quiet_NaN();

	m_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
    m_mode_prev = C_csp_collector_receiver::E_csp_cr_modes::OFF;
	m_E_su = std::numeric_limits<double>::quiet_NaN();
	m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
	m_t_su = std::numeric_limits<double>::quiet_NaN();
	m_t_su_prev = std::numeric_limits<double>::quiet_NaN();

	m_flow_pattern = 0;
	m_n_lines = -1;

	m_m_mixed = std::numeric_limits<double>::quiet_NaN();
	m_LoverD = std::numeric_limits<double>::quiet_NaN();
	m_RelRough = std::numeric_limits<double>::quiet_NaN();

	m_is_iscc = false;
	m_cycle_config = 1;

	m_T_amb_low = std::numeric_limits<double>::quiet_NaN();
	m_T_amb_high = std::numeric_limits<double>::quiet_NaN();
	m_P_amb_low = std::numeric_limits<double>::quiet_NaN();
	m_P_amb_high = std::numeric_limits<double>::quiet_NaN();

	m_q_iscc_max = std::numeric_limits<double>::quiet_NaN();

	m_csky_frac = std::numeric_limits<double>::quiet_NaN();

	m_ncall = -1;
}

void C_mspt_receiver_222::init()
{
	ambient_air.SetFluid(ambient_air.Air);

	// Declare instance of fluid class for FIELD fluid
	if( m_field_fl != HTFProperties::User_defined && m_field_fl < HTFProperties::End_Library_Fluids )
	{
		if( !field_htfProps.SetFluid( m_field_fl ) )
		{
			throw(C_csp_exception("Receiver HTF code is not recognized", "MSPT receiver"));
		}
	}
	else if( m_field_fl == HTFProperties::User_defined )
	{
		// Check that 'm_field_fl_props' is allocated and correct dimensions
		int n_rows = (int)m_field_fl_props.nrows();
		int n_cols = (int)m_field_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !field_htfProps.SetUserDefinedFluid(m_field_fl_props) )
			{
				error_msg = util::format(field_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "MSPT receiver"));
			}
		}
		else
		{
			error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "MSPT receiver"));
		}
	}
	else
	{
		throw(C_csp_exception("Receiver HTF code is not recognized", "MSPT receiver"));
	}

	
	// Declare instance of htf class for receiver tube material
	if( m_mat_tube == HTFProperties::Stainless_AISI316 || m_mat_tube == HTFProperties::T91_Steel ||
        m_mat_tube == HTFProperties::N06230 || m_mat_tube == HTFProperties::N07740)
	{
		if( !tube_material.SetFluid(m_mat_tube) )
		{
			throw(C_csp_exception("Tube material code not recognized", "MSPT receiver"));
		}
	}
	else if( m_mat_tube == HTFProperties::User_defined )
	{
		throw(C_csp_exception("Receiver material currently does not accept user defined properties", "MSPT receiver"));
	}
	else
	{
		error_msg = util::format("Receiver material code, %d, is not recognized", m_mat_tube);
		throw(C_csp_exception(error_msg, "MSPT receiver"));
	}

	// Unit Conversions
	m_od_tube /= 1.E3;			//[m] Convert from input in [mm]
	m_th_tube /= 1.E3;			//[m] Convert from input in [mm]
	m_T_htf_hot_des += 273.15;	//[K] Convert from input in [C]
	m_T_htf_cold_des += 273.15;	//[K] Convert from input in [C]
	m_q_rec_des *= 1.E6;		//[W] Convert from input in [MW]

	m_id_tube = m_od_tube - 2 * m_th_tube;			//[m] Inner diameter of receiver tube
	m_A_tube = CSP::pi*m_od_tube / 2.0*m_h_rec;	//[m^2] Outer surface area of each tube
	m_n_t = (int)(CSP::pi*m_d_rec / (m_od_tube*m_n_panels));	// The number of tubes per panel, as a function of the number of panels and the desired diameter of the receiver
	
	int n_tubes = m_n_t * m_n_panels;				//[-] Number of tubes in the system
	m_A_rec_proj = m_od_tube*m_h_rec*n_tubes;		//[m^2] The projected area of the tubes on a plane parallel to the center lines of the tubes
	m_A_node = CSP::pi*m_d_rec / m_n_panels*m_h_rec; //[m^2] The area associated with each node

	m_mode = C_csp_collector_receiver::OFF;					//[-] 0 = requires startup, 1 = starting up, 2 = running
	m_itermode = 1;			//[-] 1: Solve for design temp, 2: solve to match mass flow restriction
	m_od_control = 1.0;			//[-] Additional defocusing for over-design conditions
	m_tol_od = 0.001;		//[-] Tolerance for over-design iteration

	double c_htf_des = field_htfProps.Cp((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0)*1000.0;		//[J/kg-K] Specific heat at design conditions
	m_m_dot_htf_des = m_q_rec_des / (c_htf_des*(m_T_htf_hot_des - m_T_htf_cold_des));					//[kg/s]
	double eta_therm_des = 0.9;
	m_q_dot_inc_min = m_q_rec_des * m_f_rec_min / eta_therm_des;	//[W] Minimum receiver thermal power

	if (m_m_dot_htf_max_frac != m_m_dot_htf_max_frac)
	{
		// if max frac not set, then max mass flow (absolute) needs to be defined
		if (m_m_dot_htf_max != m_m_dot_htf_max)
		{
			throw(C_csp_exception("maximum rec htf mass flow rate not defined", "MSPT receiver"));
		}
		m_m_dot_htf_max /= 3600.0;	//[kg/s] Convert from input in [kg/hr]
	}
	m_m_dot_htf_max = m_m_dot_htf_max_frac * m_m_dot_htf_des;	//[kg/s]

	m_mode_prev = m_mode;
	m_E_su_prev = m_q_rec_des * m_rec_qf_delay;	//[W-hr] Startup energy
	m_t_su_prev = m_rec_su_delay;				//[hr] Startup time requirement
	m_eta_field_iter_prev = 1.0;				//[-] Set to largest possible value

	m_T_salt_hot_target += 273.15;			//[K] convert from C
	
	// 8.10.2015 twn: Calculate constant thermal losses to the environment
	if(m_pipe_loss_per_m > 0.0 && m_pipe_length_mult > 0.0)
		m_Q_dot_piping_loss = m_pipe_loss_per_m*(m_h_tower*m_pipe_length_mult + m_pipe_length_add);		//[Wt]
	else
		m_Q_dot_piping_loss = 0.0;


	// *******************************************************************
	// *******************************************************************
	//      Allocate the input array for the flux map!?!?!??! (line 418 type222)
	// *******************************************************************
	// *******************************************************************

	std::string flow_msg;
	if( !CSP::flow_patterns(m_n_panels, m_crossover_shift, m_flow_type, m_n_lines, m_flow_pattern, &flow_msg) )
	{
		throw(C_csp_exception(flow_msg, "MSPT receiver initialization"));
	}

	m_q_dot_inc.resize(m_n_panels);
	m_q_dot_inc.fill(0.0);


	m_T_s.resize(m_n_panels);
	m_T_s.fill(0.0);

	m_T_panel_out.resize(m_n_panels);
	m_T_panel_out.fill(0.0);

	m_T_panel_in.resize(m_n_panels);
	m_T_panel_in.fill(0.0);

	m_T_panel_ave.resize(m_n_panels);
	m_T_panel_ave.fill(0.0);

	m_q_dot_conv.resize(m_n_panels);
	m_q_dot_conv.fill(0.0);

	m_q_dot_rad.resize(m_n_panels);
	m_q_dot_rad.fill(0.0);

	m_q_dot_loss.resize(m_n_panels);
	m_q_dot_loss.fill(0.0);

	m_q_dot_abs.resize(m_n_panels);
	m_q_dot_abs.fill(0.0);

	m_m_mixed = 3.2;	//[-] Exponential for calculating mixed convection

	m_LoverD = m_h_rec / m_id_tube;
	m_RelRough = (4.5e-5) / m_id_tube;	//[-] Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm

	if(m_is_iscc)
	{
		// Set cycle configuration in class
		cycle_calcs.set_cycle_config(m_cycle_config);

		// Get table limits
		cycle_calcs.get_table_range(m_T_amb_low, m_T_amb_high, m_P_amb_low, m_P_amb_high);
	}

	m_ncall = -1;

	m_Rtot_riser = 0.0;
	m_Rtot_downc = 0.0;

	return;
}

void C_mspt_receiver_222::call(const C_csp_weatherreader::S_outputs &weather, 
	const C_csp_solver_htf_1state &htf_state_in,
	const C_mspt_receiver_222::S_inputs &inputs,
	const C_csp_solver_sim_info &sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;
	
	// Get inputs
	double field_eff = inputs.m_field_eff;					//[-]
	const util::matrix_t<double> *flux_map_input = inputs.m_flux_map_input;
		// When this function is called from TCS solver, input_operation_mode should always be == 2
	C_csp_collector_receiver::E_csp_cr_modes input_operation_mode = inputs.m_input_operation_mode;

	if(input_operation_mode < C_csp_collector_receiver::OFF || input_operation_mode > C_csp_collector_receiver::STEADY_STATE)
	{
		error_msg = util::format("Input operation mode must be either [0,1,2], but value is %d", input_operation_mode);
		throw(C_csp_exception(error_msg, "MSPT receiver timestep performance call"));
	}

	// Get sim info 
	double step = sim_info.ms_ts.m_step;			//[s]
	double time = sim_info.ms_ts.m_time;	//[s]

	// Get applicable htf state info
	double T_salt_cold_in = htf_state_in.m_temp;		//[C]

	// Complete necessary conversions/calculations of input variables
	T_salt_cold_in += 273.15;				//[K] Cold salt inlet temp, convert from C
	double P_amb = weather.m_pres*100.0;	//[Pa] Ambient pressure, convert from mbar
	double hour = time / 3600.0;			//[hr] Hour of the year
	double T_dp = weather.m_tdew + 273.15;	//[K] Dewpoint temperature, convert from C
	double T_amb = weather.m_tdry + 273.15;	//[K] Dry bulb temperature, convert from C
	// **************************************************************************************

	// Read in remaining weather inputs from weather output structure
	double zenith = weather.m_solzen;
	double azimuth = weather.m_solazi;
	double v_wind_10 = weather.m_wspd;
	double I_bn = weather.m_beam;


	int n_flux_y = (int)flux_map_input->nrows();
	if(n_flux_y > 1)
	{
		error_msg = util::format("The Molten Salt External Receiver (Type222) model does not currently support 2-dimensional "
			"flux maps. The flux profile in the vertical dimension will be averaged. NY=%d", n_flux_y);
		csp_messages.add_message(C_csp_messages::WARNING, error_msg);
	}
	int n_flux_x = (int)flux_map_input->ncols();
	m_flux_in.resize(n_flux_x);

	double T_sky = CSP::skytemp(T_amb, T_dp, hour);

	// Set current timestep stored values to NaN so we know that code solved for them
	m_mode = C_csp_collector_receiver::OFF;
	m_E_su = std::numeric_limits<double>::quiet_NaN();
	m_t_su = std::numeric_limits<double>::quiet_NaN();

	m_itermode = 1;

	double v_wind = log((m_h_tower + m_h_rec / 2) / 0.003) / log(10.0 / 0.003)*v_wind_10;

	double c_p_coolant, rho_coolant, f, u_coolant, q_conv_sum, q_rad_sum, q_dot_inc_sum, q_dot_piping_loss, q_dot_inc_min_panel;
	c_p_coolant = rho_coolant = f = u_coolant = q_conv_sum = q_rad_sum = q_dot_inc_sum = q_dot_piping_loss = q_dot_inc_min_panel = std::numeric_limits<double>::quiet_NaN();
	double eta_therm, m_dot_salt_tot, T_salt_hot, m_dot_salt_tot_ss, T_salt_hot_rec;
	eta_therm = m_dot_salt_tot = T_salt_hot = m_dot_salt_tot_ss = T_salt_hot_rec = std::numeric_limits<double>::quiet_NaN();
	double clearsky = std::numeric_limits<double>::quiet_NaN();
	
	bool rec_is_off = false;
	bool rec_is_defocusing = false;
	double field_eff_adj = 0.0;

	// ************* Outputs for ISCC model ****************
	double q_thermal_ss = 0.0;
	double f_rec_timestep = 1.0;
	// *****************************************************

	// Do an initial check to make sure the solar position called is valid
	// If it's not, return the output equal to zeros. Also check to make sure
	// the solar flux is at a certain level, otherwise the correlations aren't valid
	if( input_operation_mode == C_csp_collector_receiver::OFF )
	{
		rec_is_off = true;
	}

	if( zenith>(90.0 - m_hel_stow_deploy) || I_bn <= 1.E-6 || (zenith == 0.0 && azimuth == 180.0) )
	{
		if( m_night_recirc == 1 )
		{
			I_bn = 0.0;
		}
		else
		{
			m_mode = C_csp_collector_receiver::OFF;
			rec_is_off = true;
		}
	}

	double T_coolant_prop = (m_T_salt_hot_target + T_salt_cold_in) / 2.0;		//[K] The temperature at which the coolant properties are evaluated. Validated as constant (mjw)
	c_p_coolant = field_htfProps.Cp(T_coolant_prop)*1000.0;						//[J/kg-K] Specific heat of the coolant

	double m_dot_htf_max = m_m_dot_htf_max;
	if( m_is_iscc )
	{
		if( m_ncall == 0 )
		{
			double T_amb_C = fmax(m_P_amb_low, fmin(m_T_amb_high, T_amb - 273.15));
			double P_amb_bar = fmax(m_P_amb_low, fmin(m_P_amb_high, P_amb / 1.E5));
			m_q_iscc_max = cycle_calcs.get_ngcc_data(0.0, T_amb_C, P_amb_bar, ngcc_power_cycle::E_solar_heat_max)*1.E6;	// W-th, convert from MWth
		}

		double m_dot_iscc_max = m_q_iscc_max / (c_p_coolant*(m_T_salt_hot_target - T_salt_cold_in));		// [kg/s]
		m_dot_htf_max = fmin(m_m_dot_htf_max, m_dot_iscc_max);
	}

	if (field_eff < m_eta_field_iter_prev && m_od_control < 1.0)
	{	// Suggests controller applied defocus, so reset *controller* defocus
		m_od_control = fmin(m_od_control + (1.0 - field_eff / m_eta_field_iter_prev), 1.0);
	}

	
	// Initialize steady state solutions with current weather, DNI, field efficiency, and inlet conditions
	s_steady_state_soln soln, soln_actual, soln_clearsky;
	soln.hour = time / 3600.0;
	soln.T_amb = weather.m_tdry + 273.15;
	soln.T_dp = weather.m_tdew + 273.15;
	soln.v_wind_10 = weather.m_wspd;
	soln.p_amb = weather.m_pres * 100.0;

	soln.dni = I_bn;
	soln.field_eff = field_eff;
	soln.T_salt_cold_in = T_salt_cold_in;	
	soln.od_control = m_od_control;         // Initial defocus control (may be adjusted during the solution)
    soln.mode = input_operation_mode;
    soln.itermode = m_itermode;
	soln.rec_is_off = rec_is_off;

	clearsky = get_clearsky(weather, hour);  
	double clearsky_adj = std::fmax(clearsky, weather.m_beam);   // Set clear-sky DNI to actual DNI if actual value is higher

	
	if (rec_is_off)
		soln.q_dot_inc.resize_fill(m_n_panels, 0.0);

	else
	{

		//--- Solve for mass flow at actual and/or clear-sky DNI extremes
		if (m_csky_frac <= 0.9999 || fabs(I_bn - clearsky_adj) < 0.001)  // Solve for mass flow at actual DNI?
		{
			soln_actual = soln;  // Sets initial solution properties (inlet T, initial defocus control, etc.)
			soln_actual.dni = I_bn;
			
			if (use_previous_solution(soln_actual, m_mflow_soln_prev))  // Same conditions were solved in the previous call to this method
				soln_actual = m_mflow_soln_prev;
			else
				solve_for_mass_flow_and_defocus(soln_actual, m_dot_htf_max, flux_map_input);

			m_mflow_soln_prev = soln_actual;
		}

		if (m_csky_frac >= 0.0001) // Solve for mass flow at clear-sky DNI?
		{
			if (fabs(I_bn - clearsky_adj) < 0.001)
				soln_clearsky = soln_actual;
			else
			{
				soln_clearsky = soln;
				soln_clearsky.dni = clearsky_adj;

				if (use_previous_solution(soln_clearsky, m_mflow_soln_csky_prev))  // Same conditions were solved in the previous call to this method
					soln_clearsky = m_mflow_soln_csky_prev;
				else
					solve_for_mass_flow_and_defocus(soln_clearsky, m_dot_htf_max, flux_map_input);
				
				m_mflow_soln_csky_prev = soln_clearsky;
			}
		}

		//--- Set mass flow and calculate final solution
		if (fabs(I_bn - clearsky_adj) < 0.001 || m_csky_frac < 0.0001)  // Flow control based on actual DNI
			soln = soln_actual;
		
		else if (soln_clearsky.rec_is_off)    // Receiver can't operate at this time point 
		{
			soln.rec_is_off = true;
			soln.q_dot_inc = soln_clearsky.q_dot_inc;			
		}

		else if (m_csky_frac > 0.9999)   // Flow control based only on clear-sky DNI
		{
			soln.m_dot_salt = soln_clearsky.m_dot_salt;
			soln.rec_is_off = soln_clearsky.rec_is_off;
			soln.od_control = soln_clearsky.od_control;
			soln.q_dot_inc = calculate_flux_profiles(I_bn, field_eff, soln_clearsky.od_control, flux_map_input);  // Absorbed flux profiles at actual DNI and clear-sky defocus
			calculate_steady_state_soln(soln, 0.00025);  // Solve energy balances at clearsky mass flow rate and actual DNI conditions
		}
		
		else  // Receiver can operate and flow control based on a weighted average of clear-sky and actual DNI
		{

			if (soln_actual.rec_is_off)  // Receiver is off in actual DNI solution -> Set mass flow to the minimum value
			{
				soln_actual.m_dot_salt = m_f_rec_min * m_m_dot_htf_max;  
				soln_actual.od_control = 1.0;
			}

			soln.rec_is_off = false;
			soln.m_dot_salt = (1.0 - m_csky_frac) * soln_actual.m_dot_salt + m_csky_frac * soln_clearsky.m_dot_salt;  // weighted average of clear-sky and actual DNI

			if (soln_clearsky.od_control >= 0.9999)  // No defocus in either clear-sky or actual DNI solutions
			{
				soln.od_control = soln_clearsky.od_control;
				soln.q_dot_inc = soln_actual.q_dot_inc;
				calculate_steady_state_soln(soln, 0.00025); // Solve energy balances at this mass flow rate and actual DNI conditions
			}
			else  
			{
				soln.od_control = (1.0 - m_csky_frac) * soln_actual.od_control + m_csky_frac * soln_clearsky.od_control;
				solve_for_defocus_given_flow(soln, flux_map_input);     // Solve for defocus to achieve as close as possible to target outlet T with this mass flow and actual DNI conditions
			}

		}
	}

	// Set variables for use in the rest of the solution
	rec_is_off = soln.rec_is_off;
	m_mode = soln.mode;
	m_itermode = soln.itermode;
	m_od_control = soln.od_control;
	field_eff_adj = field_eff * soln.od_control;

	m_dot_salt_tot = soln.m_dot_salt_tot;
	T_salt_hot = soln.T_salt_hot;	
	T_salt_hot_rec = soln.T_salt_hot_rec;
	eta_therm = soln.eta_therm;

	u_coolant = soln.u_salt;
	f = soln.f;
	T_coolant_prop = (T_salt_hot + T_salt_cold_in) / 2.0;
	c_p_coolant = field_htfProps.Cp(T_coolant_prop)*1000.0;
	rho_coolant = field_htfProps.dens(T_coolant_prop, 1.0);

	q_conv_sum = soln.Q_conv_sum;
	q_rad_sum = soln.Q_rad_sum;
	q_dot_piping_loss = soln.Q_dot_piping_loss;
	q_dot_inc_sum = soln.Q_inc_sum;
	q_dot_inc_min_panel = soln.Q_inc_min;

	m_T_s = soln.T_s;
	m_T_panel_in = soln.T_panel_in;
	m_T_panel_out = soln.T_panel_out;
	m_T_panel_ave = soln.T_panel_ave;

	m_q_dot_conv = soln.q_dot_conv;
	m_q_dot_rad = soln.q_dot_rad;
	m_q_dot_loss = soln.q_dot_conv + soln.q_dot_rad;
	m_q_dot_abs = soln.q_dot_abs;
	m_q_dot_inc = soln.q_dot_inc;

	// Calculate total absorbed solar energy and minimum absorbed per panel if needed
	if (soln.Q_inc_sum != soln.Q_inc_sum)  
	{
		q_dot_inc_sum = 0.0;
		q_dot_inc_min_panel = m_q_dot_inc.at(0);
		for (int i = 0; i < m_n_panels; i++)
		{
			q_dot_inc_sum += m_q_dot_inc.at(i);
			q_dot_inc_min_panel = fmin(q_dot_inc_min_panel, m_q_dot_inc.at(i));
		}
	}

	double q_thermal_steadystate = soln.Q_thermal;
	double q_thermal_csky = 0.0;
	if (m_csky_frac > 0.0001)
		q_thermal_csky = soln_clearsky.Q_thermal;  // Steady state thermal power with clearsky DNI





	double DELTAP, Pres_D, W_dot_pump, q_thermal, q_startup;
	DELTAP = Pres_D = W_dot_pump = q_thermal = q_startup = std::numeric_limits<double>::quiet_NaN();

	q_startup = 0.0;

	double time_required_su = step/3600.0;

	if( !rec_is_off )
	{
		m_dot_salt_tot_ss = m_dot_salt_tot;

		switch( input_operation_mode )
		{
		case C_csp_collector_receiver::STARTUP:
			{
				double time_require_su_energy = m_E_su_prev / (m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in));	//[hr]
				double time_require_su_ramping = m_t_su_prev;

				double time_required_max = fmax(time_require_su_energy, time_require_su_ramping);	//[hr]

				double time_step_hrs = step / 3600.0;		//[hr]

				if( time_required_max  > time_step_hrs )		// Can't completely startup receiver in maximum allowable timestep
				{											// Need to advance timestep and try again
					time_required_su = time_step_hrs;		
					m_mode = C_csp_collector_receiver::STARTUP;
					q_startup = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*step / 3600.0;
				}
				else
				{
					time_required_su = time_required_max;		//[hr]
					m_mode = C_csp_collector_receiver::ON;

					double q_startup_energy_req = m_E_su_prev;	//[W-hr]
					double q_startup_ramping_req = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*m_t_su_prev;	//[W-hr]
					q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);
				}

				m_E_su = fmax(0.0, m_E_su_prev - m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*step / 3600.0);
				m_t_su = fmax(0.0, m_t_su_prev - step / 3600.0);
			}

			rec_is_off = true;

			break;

		case C_csp_collector_receiver::ON:
			
			if( m_E_su_prev > 0.0 || m_t_su_prev > 0.0 )
			{
				
				m_E_su = fmax(0.0, m_E_su_prev - m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*step / 3600.0);	//[W-hr]
				m_t_su = fmax(0.0, m_t_su_prev - step / 3600.0);	//[hr]

				if( m_E_su + m_t_su > 0.0 )
				{
					m_mode = C_csp_collector_receiver::STARTUP;		// If either are greater than 0, we're staring up but not finished
					
					// 4.28.15 twn: Startup energy also needs to consider energy consumed during time requirement, if that is greater than energy requirement
						//q_startup = (m_E_su_prev - m_E_su) / (step / 3600.0)*1.E-6;
					q_startup = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*step / 3600.0;

					rec_is_off = true;
					f_rec_timestep = 0.0;
				}
				else
				{
					m_mode = C_csp_collector_receiver::ON;

					double q_startup_energy_req = m_E_su_prev;	//[W-hr]
					double q_startup_ramping_req = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*m_t_su;	//[W-hr]
					q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);

					// Adjust the available mass flow to reflect startup
					m_dot_salt_tot = fmin((1.0 - m_t_su_prev / (step / 3600.0))*m_dot_salt_tot, m_dot_salt_tot - m_E_su_prev / ((step / 3600.0)*c_p_coolant*(T_salt_hot - T_salt_cold_in)));
					f_rec_timestep = fmax(0.0, fmin(1.0 - m_t_su_prev / (step / 3600.0), 1.0 - m_E_su_prev / (m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in))));
				}
					//4.28.15 twn: Startup energy needs to consider
				//q_startup = (m_E_su_prev - m_E_su) / (step / 3600.0)*1.E-6;
			}
			else
			{
				m_E_su = m_E_su_prev;
				m_t_su = m_t_su_prev;
				m_mode = C_csp_collector_receiver::ON;
				q_startup = 0.0;

				q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in);

				//if( q_thermal < m_q_rec_min )
				if(q_dot_inc_sum < m_q_dot_inc_min)
				{
					// If output here is less than specified allowed minimum, then need to shut off receiver
					m_mode = C_csp_collector_receiver::OFF;

					// Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
					W_dot_pump = 0.0;
					// Pressure drops
					DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;
				}
			}
			break;

		case C_csp_collector_receiver::STEADY_STATE:

			m_mode = C_csp_collector_receiver::STEADY_STATE;
			f_rec_timestep = 1.0;

			break;
		
		}	// End switch() on input_operation_mode

		// Pressure drop calculations
        calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump);

		q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in);
		q_thermal_ss = m_dot_salt_tot_ss*c_p_coolant*(T_salt_hot - T_salt_cold_in);

		// After convergence, determine whether the mass flow rate falls below the lower limit
		if(q_dot_inc_sum < m_q_dot_inc_min)
		{
			// GOTO 900
			// Steady State always reports q_thermal (even when much less than min) because model is letting receiver begin startup with this energy
			// Should be a way to communicate to controller that q_thermal is less than q_min without losing this functionality
			if(m_mode != C_csp_collector_receiver::STEADY_STATE || m_mode_prev == C_csp_collector_receiver::ON)
				rec_is_off = true;
		}
	}
	else
	{	// If receiver was off BEFORE startup deductions
		m_mode = C_csp_collector_receiver::OFF;

		// Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
		W_dot_pump = 0.0;
		// Pressure drops
		DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;
	}

	if( rec_is_off )
	{
		// 900 continue	// Receiver isn't producing usable energy
		m_dot_salt_tot = 0.0; eta_therm = 0.0; /*W_dot_pump = 0.0;*/
		q_conv_sum = 0.0; q_rad_sum = 0.0; m_T_s.fill(0.0); q_thermal = 0.0;
		// Set the receiver outlet temperature equal to the inlet design temperature
		T_salt_hot = m_T_htf_cold_des;
		q_dot_inc_sum = 0.0;
		// Pressure drops
		/*DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;*/
		// Set receiver startup energy to 0
		// q_startup = 0.0;
		// ISCC outputs
		m_dot_salt_tot_ss = 0.0; f_rec_timestep = 0.0; q_thermal_ss = 0.0;
		q_thermal_csky = q_thermal_steadystate = 0.0;
		

		// Reset m_od_control
		m_od_control = 1.0;		//[-]
	}

	outputs.m_m_dot_salt_tot = m_dot_salt_tot*3600.0;		//[kg/hr] convert from kg/s
	outputs.m_eta_therm = eta_therm;							//[-] RECEIVER thermal efficiency (includes radiation and convective losses. reflection losses are contained in receiver flux model)
	outputs.m_W_dot_pump = W_dot_pump / 1.E6;				//[MW] convert from W
	outputs.m_q_conv_sum = q_conv_sum / 1.E6;				//[MW] convert from W
	outputs.m_q_rad_sum = q_rad_sum / 1.E6;					//[MW] convert from W
	outputs.m_Q_thermal = q_thermal / 1.E6;					//[MW] convert from W
	outputs.m_T_salt_hot = T_salt_hot - 273.15;				//[C] convert from K
	outputs.m_field_eff_adj = field_eff_adj;					//[-]
	outputs.m_component_defocus = m_od_control;				//[-]
	outputs.m_q_dot_rec_inc = q_dot_inc_sum / 1.E6;			//[MW] convert from W
	outputs.m_q_startup = q_startup/1.E6;					//[MW-hr] convert from W-hr
	outputs.m_dP_receiver = DELTAP*m_n_panels / m_n_lines / 1.E5;	//[bar] receiver pressure drop, convert from Pa
	outputs.m_dP_total = Pres_D*10.0;						//[bar] total pressure drop, convert from MPa
	outputs.m_vel_htf = u_coolant;							//[m/s]
	outputs.m_T_salt_cold = T_salt_cold_in - 273.15;			//[C] convert from K
	outputs.m_m_dot_ss = m_dot_salt_tot_ss*3600.0;			//[kg/hr] convert from kg/s
	outputs.m_q_dot_ss = q_thermal_ss / 1.E6;				//[MW] convert from W
	outputs.m_f_timestep = f_rec_timestep;					//[-]
	outputs.m_time_required_su = time_required_su*3600.0;	//[s], convert from hr in code
	if(q_thermal > 0.0)
		outputs.m_q_dot_piping_loss = q_dot_piping_loss/1.E6;	//[MWt]
	else
		outputs.m_q_dot_piping_loss = 0.0;		//[MWt]
    outputs.m_q_heattrace = 0.0;

	outputs.m_clearsky = clearsky;  // W/m2
	outputs.m_Q_thermal_csky_ss = q_thermal_csky / 1.e6; //[MWt]
	outputs.m_Q_thermal_ss = q_thermal_steadystate / 1.e6; //[MWt]

    ms_outputs = outputs;

	m_eta_field_iter_prev = field_eff;	//[-]
}

void C_mspt_receiver_222::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_solver_sim_info &sim_info)
{
	// Don't currently need *any* of these inputs, but if we add recirculation or thermal capacitance it would be helpful to have in place
	m_mode = C_csp_collector_receiver::OFF;

	// Assuming no night recirculation, so... these should be zero
	outputs.m_m_dot_salt_tot = 0.0;		//[kg/hr] convert from kg/s
	outputs.m_eta_therm = 0.0;			//[-] RECEIVER thermal efficiency (includes radiation and convective losses. reflection losses are contained in receiver flux model)
	outputs.m_W_dot_pump = 0.0;			//[MW] convert from W
	outputs.m_q_conv_sum = 0.0;			//[MW] convert from W
	outputs.m_q_rad_sum = 0.0;			//[MW] convert from W
	outputs.m_Q_thermal = 0.0;			//[MW] convert from W
	outputs.m_T_salt_hot = 0.0;			//[C] convert from K
	outputs.m_field_eff_adj = 0.0;		//[-]
	outputs.m_component_defocus = 1.0;	//[-]
	outputs.m_q_dot_rec_inc = 0.0;		//[MW] convert from kW
	outputs.m_q_startup = 0.0;			//[MW-hr] convert from W-hr
	outputs.m_dP_receiver = 0.0;			//[bar] receiver pressure drop, convert from Pa
	outputs.m_dP_total = 0.0;			//[bar] total pressure drop, convert from MPa
	outputs.m_vel_htf = 0.0;				//[m/s]
	outputs.m_T_salt_cold = 0.0;			//[C] convert from K
	outputs.m_m_dot_ss = 0.0;			//[kg/hr] convert from kg/s
	outputs.m_q_dot_ss = 0.0;			//[MW] convert from W
	outputs.m_f_timestep = 0.0;			//[-]
	outputs.m_time_required_su = sim_info.ms_ts.m_step;	//[s], convert from hr in code
	outputs.m_q_dot_piping_loss = 0.0;	//[MWt]
    outputs.m_q_heattrace = 0.0;
	
	outputs.m_clearsky = get_clearsky(weather, sim_info.ms_ts.m_time / 3600.);  // clear-sky DNI (set to actual DNI if actual DNI is higher than computed clear-sky value)
	outputs.m_Q_thermal_csky_ss = 0.0; //[MWt]
	outputs.m_Q_thermal_ss = 0.0; //[MWt]

    ms_outputs = outputs;
	
	return;
}

void C_mspt_receiver_222::converged()
{
	// Check HTF props?
	//!MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
	//call check_htf(Coolant,T_salt_hot)
	//call check_htf(Coolant,T_salt_cold)

	if( m_mode == C_csp_collector_receiver::STEADY_STATE )
	{
		throw(C_csp_exception("Receiver should only be run at STEADY STATE mode for estimating output. It must be run at a different mode before exiting a timestep",
			"MSPT receiver converged method"));
	}

	if( m_mode == C_csp_collector_receiver::OFF )
	{
		m_E_su = m_q_rec_des * m_rec_qf_delay;
		m_t_su = m_rec_su_delay;
	}

	m_mode_prev = m_mode;
	m_E_su_prev = m_E_su;
	m_t_su_prev = m_t_su;

	m_itermode = 1;
	m_od_control = 1.0;
	m_eta_field_iter_prev = 1.0;		//[-]

	m_ncall = -1;

    ms_outputs = outputs;
}


bool C_mspt_receiver_222::use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev)
{
	// Are these conditions identical to those used in the last solution?
	if (!soln_prev.rec_is_off && 
		soln.dni == soln_prev.dni &&
		soln.T_salt_cold_in == soln_prev.T_salt_cold_in &&
		soln.field_eff == soln_prev.field_eff &&
		soln.od_control == soln_prev.od_control &&
		soln.T_amb == soln_prev.T_amb && 
		soln.T_dp == soln_prev.T_dp &&
		soln.v_wind_10 == soln_prev.v_wind_10 &&
		soln.p_amb == soln_prev.p_amb)
	{
		return true;
	}
	else
		return false;
}


// Calculate flux profiles (interpolated to receiver panels at specified DNI)
util::matrix_t<double> C_mspt_receiver_222::calculate_flux_profiles(double dni, double field_eff, double od_control, const util::matrix_t<double> *flux_map_input)
{
	util::matrix_t<double> q_dot_inc, flux;
	q_dot_inc.resize_fill(m_n_panels, 0.0);

	double field_eff_adj = field_eff * od_control;

	// Set flux at flux map resolution
	int n_flux_y = (int)flux_map_input->nrows();
	int n_flux_x = (int)flux_map_input->ncols();
	flux.resize_fill(n_flux_x, 0.0);

	if (dni > 1.0)
	{
		for (int j = 0; j<n_flux_x; j++)
		{
			flux.at(j) = 0.;
			for (int i = 0; i<n_flux_y; i++)
			{
				flux.at(j) += (*flux_map_input)(i, j) * dni * field_eff_adj*m_A_sf / 1000. / (CSP::pi*m_h_rec*m_d_rec / (double)n_flux_x);	//[kW/m^2];
			}
		}
	}
	else
	{
		flux.fill(0.0);
	}

	double n_flux_x_d = (double)m_n_flux_x;
	double n_panels_d = (double)m_n_panels;

	// Translate flux to panels
	if (m_n_panels >= m_n_flux_x)
	{
		// Translate to the number of panels, so each panel has its own linearly interpolated flux value
		for (int i = 0; i < m_n_panels; i++)
		{
			double ppos = (n_flux_x_d / n_panels_d * i + n_flux_x_d * 0.5 / n_panels_d);
			int flo = (int)floor(ppos);
			int ceiling = (int)ceil(ppos);
			double ind = (int)((ppos - flo) / fmax((double)ceiling - (double)flo, 1.e-6));
			if (ceiling > m_n_flux_x - 1) ceiling = 0;

			double psp_field = (ind*(flux.at(ceiling) - flux.at(flo)) + flux.at(flo));		//[kW/m^2] Average area-specific power for each node			
			q_dot_inc.at(i) = m_A_node * psp_field*1000;									//[W] The power incident on each node

		}
	}
	else
	{
		/*
		The number of panels is always even, therefore the receiver panels are symmetric about the N-S plane.

		The number of flux points may be even or odd. The distribution is assumed to be symmetric
		about North, therefore:
		(a) A distribution with an odd number of points includes a center point (n_flux_x - 1)/2+1
		whose normal faces exactly north
		(b) A distribution with an even number of points includes 2 points n_flux_x/2, n_flux_x/2+1
		which straddle the North vector.
		In either scenario, two points straddle the South vector and no scenario allows a point to fall
		directly on the South vector. Hence, the first and last flux points fall completely on the first
		and last panel, respectively.
		*/

		double leftovers = 0.;
		int index_start = 0; int index_stop = 0;
		double q_flux_sum = 0.0;

		double panel_step = n_flux_x_d / n_panels_d;   //how many flux points are stepped over by each panel?

		for (size_t i = 0; i<m_n_panels; i++)
		{
			double panel_pos = panel_step * (i + 1);   //Where does the current panel end in the flux array?

			index_start = (int)floor(panel_step*i);
			index_stop = (int)floor(panel_pos);

			q_flux_sum = 0.;

			for (int j = index_start; j<index_stop + 1; j++)
			{
				if (j == m_n_flux_x)
				{
					if (leftovers > 0.)
					{
						csp_messages.add_message(C_csp_messages::WARNING, "An error occurred during interpolation of the receiver flux map. The results may be inaccurate! Contact SAM support to resolve this issue.");
					}

					break;
				}
				if (j == 0)
				{
					q_flux_sum = flux.at(j);
					leftovers = 0.;
				}
				else if (j == index_start)
				{
					q_flux_sum += leftovers;
					leftovers = 0.;
				}
				else if (j == index_stop)
				{
					double stop_mult = (panel_pos - floor(panel_pos));
					q_flux_sum += stop_mult * flux.at(j);
					leftovers = (1 - stop_mult)*flux.at(j);
				}
				else
				{
					q_flux_sum += flux[j];
				}
			}
			q_dot_inc.at(i) = q_flux_sum * m_A_node / n_flux_x_d * n_panels_d*1000;
		}

	}

	return q_dot_inc;
}

// Calculate steady state temperature and heat loss profiles for a given mass flow and incident flux
void C_mspt_receiver_222::calculate_steady_state_soln(s_steady_state_soln &soln, double tol, int max_iter)
{

	double P_amb = soln.p_amb;	
	double hour = soln.hour;			
	double T_dp = soln.T_dp;	
	double T_amb = soln.T_amb;	
	double v_wind_10 = soln.v_wind_10;
	double T_sky = CSP::skytemp(T_amb, T_dp, hour);
	double v_wind = log((m_h_tower + m_h_rec / 2) / 0.003) / log(10.0 / 0.003)*v_wind_10;

	util::matrix_t<double> T_s_guess(m_n_panels);
	util::matrix_t<double> T_panel_out_guess(m_n_panels);
	util::matrix_t<double> T_panel_in_guess(m_n_panels);
	util::matrix_t<double> T_film(m_n_panels);

	bool soln_exists = (soln.T_salt_hot == soln.T_salt_hot);

	soln.m_dot_salt_tot = soln.m_dot_salt * m_n_lines;

	// Set initial guess
	double T_salt_hot_guess;
	if (soln_exists)    // Use existing solution as the inital guess
	{
		T_salt_hot_guess = soln.T_salt_hot;    // Initial guess for outlet T
		T_s_guess = soln.T_s;
		T_panel_out_guess = soln.T_panel_out;
		T_panel_in_guess = soln.T_panel_in;
	}
	else // Initialize solution from scratch
	{
		T_salt_hot_guess = m_T_salt_hot_target;    // Initial guess for outlet T

		soln.T_s.resize(m_n_panels);
		soln.T_panel_out.resize(m_n_panels);
		soln.T_panel_in.resize(m_n_panels);
		soln.q_dot_conv.resize(m_n_panels);
		soln.q_dot_rad.resize(m_n_panels);
		soln.q_dot_loss.resize(m_n_panels);
		soln.q_dot_abs.resize(m_n_panels);
		soln.T_panel_ave.resize(m_n_panels);

		if (m_night_recirc == 1)
		{
			T_s_guess.fill(m_T_salt_hot_target);										//[K] Guess the temperature for the surface nodes
			T_panel_out_guess.fill((m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0);	//[K] Guess values for the fluid temp coming out of the control volume
			T_panel_in_guess.fill((m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0);	//[K] Guess values for the fluid temp coming into the control volume
		}
		else
		{
			T_s_guess.fill(m_T_salt_hot_target);			//[K] Guess the temperature for the surface nodes
			T_panel_out_guess.fill(soln.T_salt_cold_in);	//[K] Guess values for the fluid temp coming out of the control volume
			T_panel_in_guess.fill(soln.T_salt_cold_in);		//[K] Guess values for the fluid temp coming into the control volume
		}
	}


	// Temperature solution iterations
	for (int q = 0; q < max_iter; q++)
	{
		double T_coolant_prop;
		if (soln.T_salt_props == soln.T_salt_props)   // Temperature for property evaluation exists (calling from within loop over salt mass flow rate)
			T_coolant_prop = soln.T_salt_props;
		else    
			T_coolant_prop = (T_salt_hot_guess + soln.T_salt_cold_in) / 2.0;
		double c_p_coolant = field_htfProps.Cp(T_coolant_prop)*1000.;	

		for (int i = 0; i < m_n_panels; i++)
		{
			soln.T_s.at(i) = T_s_guess.at(i);
			soln.T_panel_out.at(i) = T_panel_out_guess.at(i);
			soln.T_panel_in.at(i) = T_panel_in_guess.at(i);
			soln.T_panel_ave.at(i) = (soln.T_panel_in.at(i) + soln.T_panel_out.at(i)) / 2.0;		//[K] The average coolant temperature in each control volume
			T_film.at(i) = (soln.T_s.at(i) + T_amb) / 2.0;											//[K] Film temperature
		}

		// Calculate the average surface temperature
		double T_s_sum = 0.0;
		for (int i = 0; i < m_n_panels; i++)
			T_s_sum += soln.T_s.at(i);
		double T_film_ave = (T_amb + T_salt_hot_guess) / 2.0;


		// Convective coefficient for external forced convection using Siebers & Kraabel
		double k_film = ambient_air.cond(T_film_ave);				//[W/m-K] The conductivity of the ambient air
		double mu_film = ambient_air.visc(T_film_ave);				//[kg/m-s] Dynamic viscosity of the ambient air
		double rho_film = ambient_air.dens(T_film_ave, P_amb);		//[kg/m^3] Density of the ambient air
		double c_p_film = ambient_air.Cp(T_film_ave);				//[kJ/kg-K] Specific heat of the ambient air
		double Re_for = rho_film * v_wind*m_d_rec / mu_film;		//[-] Reynolds number
		double ksD = (m_od_tube / 2.0) / m_d_rec;					//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
		double Nusselt_for = CSP::Nusselt_FC(ksD, Re_for);			//[-] S&K
		double h_for = Nusselt_for * k_film / m_d_rec * m_hl_ffact;	//[W/m^2-K] Forced convection heat transfer coefficient

		// Convection coefficient for external natural convection using Siebers & Kraabel
		// Note: This relationship applies when the surrounding properties are evaluated at ambient conditions [S&K]
		double beta = 1.0 / T_amb;													//[1/K] Volumetric expansion coefficient
		double nu_amb = ambient_air.visc(T_amb) / ambient_air.dens(T_amb, P_amb);	//[m^2/s] Kinematic viscosity		

		for (size_t j = 0; j < m_n_lines; j++)   
		{
			for (size_t  i = 0; i < m_n_panels / m_n_lines; i++)
			{
				int i_fp = m_flow_pattern.at(j, i);

				// Natural convection
				double Gr_nat = fmax(0.0, CSP::grav*beta*(soln.T_s.at(i_fp) - T_amb)*pow(m_h_rec, 3) / pow(nu_amb, 2));	//[-] Grashof Number at ambient conditions
				double Nusselt_nat = 0.098*pow(Gr_nat, (1.0 / 3.0))*pow(soln.T_s.at(i_fp) / T_amb, -0.14);				//[-] Nusselt number
				double h_nat = Nusselt_nat * ambient_air.cond(T_amb) / m_h_rec * m_hl_ffact;							//[W/m^-K] Natural convection coefficient

				// Mixed convection
				double h_mixed = pow((pow(h_for, m_m_mixed) + pow(h_nat, m_m_mixed)), 1.0 / m_m_mixed)*4.0;		//(4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)
				soln.q_dot_conv.at(i_fp) = h_mixed * m_A_node*(soln.T_s.at(i_fp) - T_film.at(i_fp));			//[W] Convection losses per node

				// Radiation from the receiver - Calculate the radiation node by node
				soln.q_dot_rad.at(i_fp) = 0.5*CSP::sigma*m_epsilon*m_A_node*(2.0*pow(soln.T_s.at(i_fp), 4) - pow(T_amb, 4) - pow(T_sky, 4))*m_hl_ffact;	//[W] Total radiation losses per node
				soln.q_dot_loss.at(i_fp) = soln.q_dot_rad.at(i_fp) + soln.q_dot_conv.at(i_fp);			//[W] Total overall losses per node
				soln.q_dot_abs.at(i_fp) = soln.q_dot_inc.at(i_fp) - soln.q_dot_loss.at(i_fp);			//[W] Absorbed flux at each node

				// Calculate the temperature drop across the receiver tube wall... assume a cylindrical thermal resistance
				double T_wall = (soln.T_s.at(i_fp) + soln.T_panel_ave.at(i_fp)) / 2.0;				//[K] The temperature at which the conductivity of the wall is evaluated
				double k_tube = tube_material.cond(T_wall);											//[W/m-K] The conductivity of the wall
				double R_tube_wall = m_th_tube / (k_tube*m_h_rec*m_d_rec*pow(CSP::pi, 2) / 2.0 / (double)m_n_panels);	//[K/W] The thermal resistance of the wall

				// Calculations for the inside of the tube						
				double mu_coolant = field_htfProps.visc(T_coolant_prop);							//[kg/m-s] Absolute viscosity of the coolant
				double k_coolant = field_htfProps.cond(T_coolant_prop);								//[W/m-K] Conductivity of the coolant
				double rho_coolant = field_htfProps.dens(T_coolant_prop, 1.0);						//[kg/m^3] Density of the coolant
				double u_coolant = soln.m_dot_salt / (m_n_t*rho_coolant*pow((m_id_tube / 2.0), 2)*CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
				double Re_inner = rho_coolant * u_coolant*m_id_tube / mu_coolant;					//[-] Reynolds number of internal flow
				double Pr_inner = c_p_coolant * mu_coolant / k_coolant;								//[-] Prandtl number of internal flow

				double Nusselt_t, f;
				CSP::PipeFlow(Re_inner, Pr_inner, m_LoverD, m_RelRough, Nusselt_t, f);
				if (Nusselt_t <= 0.0)
				{
					soln.mode = C_csp_collector_receiver::OFF;	
					break;
				}
				double h_inner = Nusselt_t * k_coolant / m_id_tube;								//[W/m^2-K] Convective coefficient between the inner tube wall and the coolant
				double R_conv_inner = 1.0 / (h_inner*CSP::pi*m_id_tube / 2.0*m_h_rec*m_n_t);	//[K/W] Thermal resistance associated with this value

				soln.u_salt = u_coolant;
				soln.f = f;

				// Update panel inlet/outlet temperature guess
				if (i > 0)
				{
					int i_prev = m_flow_pattern.at(j, i - 1);   // Previous panel in flow order
					T_panel_in_guess.at(i_fp) = T_panel_out_guess.at(i_prev);
				}
				else
					T_panel_in_guess.at(i_fp) = soln.T_salt_cold_in;
				

				T_panel_out_guess.at(i_fp) = T_panel_in_guess.at(i_fp) + soln.q_dot_abs.at(i_fp) / (soln.m_dot_salt*c_p_coolant);		//[K] Energy balance for each node		
				double Tavg = (T_panel_out_guess.at(i_fp) + T_panel_in_guess.at(i_fp)) / 2.0;											//[K] Panel average temperature
				T_s_guess.at(i_fp) = Tavg + soln.q_dot_abs.at(i_fp)*(R_conv_inner + R_tube_wall);										//[K] Surface temperature based on the absorbed heat
				if (T_s_guess.at(i_fp) < 1.0)
				{
					soln.mode = C_csp_collector_receiver::OFF;
				}

			}  // End loop over panels per flow path
		}  // End loop over flow paths
		

		if (soln.mode == C_csp_collector_receiver::OFF)
			break;

		// Calculate average receiver outlet temperature
		int klast = m_n_panels / m_n_lines - 1;
		double T_salt_hot_guess_sum = 0.0;
		for (int j = 0; j < m_n_lines; j++)
			T_salt_hot_guess_sum += T_panel_out_guess.at(m_flow_pattern.at(j, klast));		//[K] Update the calculated hot salt outlet temp
		soln.T_salt_hot = T_salt_hot_guess_sum / (double)m_n_lines;


		// Calculate outlet temperature after piping losses
		soln.Q_dot_piping_loss = 0.0;
		if (m_Q_dot_piping_loss > 0.0)
		{
			double m_dot_salt_tot_temp = soln.m_dot_salt * m_n_lines;		//[kg/s]

			if (m_piping_loss_coeff != m_piping_loss_coeff)   // Calculate piping loss from constant loss per m (m_Q_dot_piping_loss)
				soln.Q_dot_piping_loss = m_Q_dot_piping_loss;
			else
			{
				double riser_loss = 2.0*CSP::pi * (soln.T_salt_cold_in - T_amb) / m_Rtot_riser; //[W/m]
				double downc_loss = 2.0*CSP::pi * (soln.T_salt_hot - T_amb) / m_Rtot_downc; //[W/m]
				soln.Q_dot_piping_loss = 0.5*(riser_loss + downc_loss) * (m_h_tower*m_pipe_length_mult + m_pipe_length_add); // Total piping thermal loss [W]
			}
			double delta_T_piping = soln.Q_dot_piping_loss / (m_dot_salt_tot_temp*c_p_coolant);	//[K]
			soln.T_salt_hot_rec = soln.T_salt_hot;
			soln.T_salt_hot -= delta_T_piping;	//[K]
		}
		
		
		// Check convergence
		double err = (soln.T_salt_hot - T_salt_hot_guess) / T_salt_hot_guess;
		T_salt_hot_guess = soln.T_salt_hot;
		if (fabs(err) < tol && q>0)
			break;

	} // End iterations

	if (soln.T_salt_hot < soln.T_salt_cold_in)
		soln.mode = C_csp_collector_receiver::OFF;


	// Save overall energy loss
	soln.Q_inc_sum = 0.0;
	soln.Q_conv_sum = 0.0;
	soln.Q_rad_sum = 0.0;
	soln.Q_abs_sum = 0.0;
	soln.Q_inc_min = soln.q_dot_inc.at(0);
	for (int i = 0; i < m_n_panels; i++)
	{
		soln.Q_inc_sum += soln.q_dot_inc.at(i);
		soln.Q_conv_sum += soln.q_dot_conv.at(i);
		soln.Q_rad_sum += soln.q_dot_rad.at(i);
		soln.Q_abs_sum += soln.q_dot_abs.at(i);
		soln.Q_inc_min = fmin(soln.Q_inc_min, soln.q_dot_inc.at(i));
	}
	soln.Q_thermal = soln.Q_abs_sum - soln.Q_dot_piping_loss;

	if (soln.Q_inc_sum > 0.0)
		soln.eta_therm = soln.Q_abs_sum / soln.Q_inc_sum;
	else
		soln.eta_therm = 0.0;

	soln.rec_is_off = false;
	if (soln.mode == C_csp_collector_receiver::OFF)
		soln.rec_is_off = true;

	// Save final temperature profile solution
	if (!soln.rec_is_off)
	{
		soln.T_s = T_s_guess;
		soln.T_panel_out = T_panel_out_guess;
		soln.T_panel_in = T_panel_in_guess;
		for (int i = 0; i < m_n_panels; i++)
			soln.T_panel_ave.at(i) = (soln.T_panel_in.at(i) + soln.T_panel_out.at(i)) / 2.0;
	}

	return;

}

// Calculate mass flow rate needed to achieve target outlet temperature (m_T_salt_hot_target) given incident flux profiles
void C_mspt_receiver_222::solve_for_mass_flow(s_steady_state_soln &soln)
{

	bool soln_exists = (soln.m_dot_salt == soln.m_dot_salt);

	soln.T_salt_props = (m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0;		//[K] The temperature at which the coolant properties are evaluated. Validated as constant (mjw)
	double c_p_coolant = field_htfProps.Cp(soln.T_salt_props)*1000.0;				//[J/kg-K] Specific heat of the coolant

	double m_dot_salt_guess;
	if (soln_exists)  // Use existing solution as intial guess
	{
		m_dot_salt_guess = soln.m_dot_salt;
	}
	else  // Set inital guess for mass flow solution
	{

		double q_dot_inc_sum = 0.0;
		for (int i = 0; i < m_n_panels; i++)
			q_dot_inc_sum += soln.q_dot_inc.at(i);		//[kW] Total power absorbed by receiver

		double c_guess = field_htfProps.Cp((m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0)*1000.;	//[kJ/kg-K] Estimate the specific heat of the fluid in receiver

		if (soln.dni > 1.E-6)
		{
			double q_guess = 0.85*q_dot_inc_sum;		//[kW] Estimate the thermal power produced by the receiver			
			m_dot_salt_guess = q_guess / (c_guess*(m_T_salt_hot_target - soln.T_salt_cold_in)*m_n_lines);	//[kg/s] Mass flow rate for each flow path			
		}
		else	// The tower recirculates at night (based on earlier conditions)
		{
			// Enter recirculation mode, where inlet/outlet temps switch
			double T_salt_hot = m_T_salt_hot_target;
			m_T_salt_hot_target = soln.T_salt_cold_in;
			soln.T_salt_cold_in = T_salt_hot;
			m_dot_salt_guess = -3500.0 / (c_guess*(m_T_salt_hot_target - soln.T_salt_cold_in) / 2.0);

		}
	}


	// Set soluion tolerance
	double T_salt_hot_guess = 9999.9;		//[K] Initial guess value for error calculation
	double err = -999.9;					//[-] Relative outlet temperature error
	double tol = std::numeric_limits<double>::quiet_NaN();
	if (m_night_recirc == 1)
		tol = 0.0057;
	else
		tol = 0.00025;

	int qq_max = 50;
	int qq = 0;

	bool converged = false;
	while (!converged)
	{
		qq++;

		// if the problem fails to converge after 50 iterations, then the power is likely negligible and the zero set can be returned
		if (qq > qq_max)
		{
			soln.mode = C_csp_collector_receiver::OFF;  // Set the startup mode
			soln.rec_is_off = true;
			break;
		}

		soln.m_dot_salt = m_dot_salt_guess;
		double tolT = tol;
		calculate_steady_state_soln(soln, tolT, 50);   // Solve steady state thermal model		
		err = (soln.T_salt_hot - m_T_salt_hot_target) / m_T_salt_hot_target;
		
		if (soln.rec_is_off)  // SS solution was unsuccessful or resulted in an infeasible exit temperature -> remove outlet T for solution to start next iteration from the default intial guess
			soln.T_salt_hot = std::numeric_limits<double>::quiet_NaN();

		if (fabs(err) > tol)
		{
			m_dot_salt_guess = (soln.Q_abs_sum - soln.Q_dot_piping_loss) / (m_n_lines * c_p_coolant * (m_T_salt_hot_target - soln.T_salt_cold_in));			//[kg/s]

			if (m_dot_salt_guess < 1.E-5)
			{
				soln.mode = C_csp_collector_receiver::OFF;
				soln.rec_is_off = true;
				break;
			}
		}
		else if (err > 0.0)  // Solution has converged but outlet T is above target.  CSP solver seems to perform better with slighly under-design temperature than with slighly over-design temperatures. 
			m_dot_salt_guess *= (soln.T_salt_hot - soln.T_salt_cold_in) / ((1.0 - 0.5*tol) * m_T_salt_hot_target - soln.T_salt_cold_in);
		else
			converged = true;

	}

	soln.m_dot_salt_tot = soln.m_dot_salt * m_n_lines;

	return;
}

// Calculate mass flow rate and defocus needed to achieve target outlet temperature given DNI
void C_mspt_receiver_222::solve_for_mass_flow_and_defocus(s_steady_state_soln &soln, double m_dot_htf_max, const util::matrix_t<double> *flux_map_input)
{

	bool rec_is_defocusing = true;
	double err_od = 999.0;

	while (rec_is_defocusing)
	{
		if (soln.rec_is_off)
			break;

		soln.q_dot_inc = calculate_flux_profiles(soln.dni, soln.field_eff, soln.od_control, flux_map_input);  // Calculate flux profiles
		solve_for_mass_flow(soln);	// Iterative calculation of mass flow to produce target outlet temperature

		if (soln.rec_is_off)
			break;

		double m_dot_salt_tot = soln.m_dot_salt * m_n_lines;
		double m_dot_tube = soln.m_dot_salt / (double)m_n_t;		//[kg/s] The mass flow through each individual tube

		// Limit the HTF mass flow rate to the maximum, if needed
		rec_is_defocusing = false;
		if ((m_dot_salt_tot > m_dot_htf_max) || soln.itermode == 2)
		{
			double err_od = (m_dot_salt_tot - m_dot_htf_max) / m_dot_htf_max;
			if (err_od < m_tol_od)
			{
				soln.itermode = 1;
				soln.od_control = 1.0;
				rec_is_defocusing = false;
			}
			else
			{
				soln.od_control = soln.od_control * pow((m_dot_htf_max / m_dot_salt_tot), 0.8);	//[-] Adjust the over-design defocus control by modifying the current value
				soln.itermode = 2;
				rec_is_defocusing = true;
			}
		}

	}

	return;
}

// Calculate defocus needed to maintain outlet temperature under the target value given DNI and mass flow
void C_mspt_receiver_222::solve_for_defocus_given_flow(s_steady_state_soln &soln, const util::matrix_t<double> *flux_map_input)
{ 
	
	double Tprev, od, odprev, odlow, odhigh;
	double tolT = 0.00025;
	double urf = 0.8;

	Tprev = odprev = odlow = std::numeric_limits<double>::quiet_NaN();
	od = soln.od_control;
	odhigh = 1.0;

	od = odprev * (m_T_salt_hot_target - soln.T_salt_cold_in) / (Tprev - soln.T_salt_cold_in);

	int q = 0;
	while (q<50)
	{
		soln.od_control = od;
		if (odprev != odprev)
			soln.q_dot_inc = calculate_flux_profiles(soln.dni, soln.field_eff, soln.od_control, flux_map_input);
		else
			soln.q_dot_inc = soln.q_dot_inc * soln.od_control / odprev; // Calculate flux profiles (note flux is directly proportional to defocus control)
		
		calculate_steady_state_soln(soln, tolT);     // Solve steady state thermal model 

		if (soln.od_control > 0.9999 && soln.T_salt_hot < m_T_salt_hot_target)  // Impossible for solution to achieve temperature target
			break;
		else if ((fabs(soln.T_salt_hot - m_T_salt_hot_target) / m_T_salt_hot_target) < tolT)
			break;
		else
		{
			
			if (soln.rec_is_off)
			{
				odlow = soln.od_control;
				od = odlow + 0.5*(odhigh - odlow);
			}
			else if (odprev != odprev)
			{
				od = odprev * (m_T_salt_hot_target - soln.T_salt_cold_in) / (Tprev - soln.T_salt_cold_in);
			}
			else
			{
				double delta_od = (soln.T_salt_hot - m_T_salt_hot_target) / ((soln.T_salt_hot - Tprev) / (soln.od_control - odprev));
				double od = soln.od_control - urf * delta_od;
				if (od < odlow || od > odhigh)
				{
					if (odlow == odlow)
						od = odlow + 0.5*(odhigh - odlow);
					else
						od = od * 0.95*odhigh;
				}
			}

			odprev = soln.od_control;
			Tprev = soln.T_salt_hot;
		}
		q++;
	}

	return;
}






void C_mspt_receiver_222::calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc)
{

    // Pressure drop calculations
	double mpertube = mdot / ((double)m_n_lines * (double)m_n_t);
	double u_coolant = mpertube / (rho_f * m_id_tube * m_id_tube * 0.25 * CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes

	double L_e_45 = 16.0;						// The equivalent length produced by the 45 degree bends in the tubes - Into to Fluid Mechanics, Fox et al.
	double L_e_90 = 30.0;						// The equivalent length produced by the 90 degree bends in the tubes
	double DELTAP_tube = rho_f*(ffact*m_h_rec / m_id_tube*pow(u_coolant, 2) / 2.0);	//[Pa] Pressure drop across the tube, straight length
	double DELTAP_45 = rho_f*(ffact*L_e_45*pow(u_coolant, 2) / 2.0);					//[Pa] Pressure drop across 45 degree bends
	double DELTAP_90 = rho_f*(ffact*L_e_90*pow(u_coolant, 2) / 2.0);					//[Pa] Pressure drop across 90 degree bends
	double DELTAP = DELTAP_tube + 2 * DELTAP_45 + 4 * DELTAP_90;						//[Pa] Total pressure drop across the tube with (4) 90 degree bends, (2) 45 degree bends
	double DELTAP_h_tower = rho_f*m_h_tower*CSP::grav;						//[Pa] The pressure drop from pumping up to the receiver
	double DELTAP_net = DELTAP*m_n_panels / (double)m_n_lines + DELTAP_h_tower;		//[Pa] The new pressure drop across the receiver panels
	PresDrop_calc = DELTAP_net*1.E-6;			//[MPa]
	double est_load = fmax(0.25, mdot / m_m_dot_htf_des) * 100;		//[%] Relative pump load. Limit to 25%
	double eta_pump_adj = m_eta_pump*(-2.8825E-9*pow(est_load, 4) + 6.0231E-7*pow(est_load, 3) - 1.3867E-4*pow(est_load, 2) + 2.0683E-2*est_load);	//[-] Adjusted pump efficiency
	WdotPump_calc = DELTAP_net*mdot / rho_f / eta_pump_adj;

}

double C_mspt_receiver_222::get_pumping_parasitic_coef()
{
    double Tavg = (m_T_htf_cold_des + m_T_htf_hot_des) / 2.;

    double mu_coolant = field_htfProps.visc(Tavg);				//[kg/m-s] Absolute viscosity of the coolant
    double k_coolant = field_htfProps.cond(Tavg);				//[W/m-K] Conductivity of the coolant
    double rho_coolant = field_htfProps.dens(Tavg, 1.0);        //[kg/m^3] Density of the coolant
    double c_p_coolant = field_htfProps.Cp(Tavg)*1e3;           //[J/kg-K] Specific heat

    double m_dot_salt = m_q_rec_des / (c_p_coolant * (m_T_htf_hot_des - m_T_htf_cold_des));

    double n_t = (int)(CSP::pi*m_d_rec / (m_od_tube*m_n_panels));   // The number of tubes per panel, as a function of the number of panels and the desired diameter of the receiver
    double id_tube = m_od_tube - 2 * m_th_tube;                 //[m] Inner diameter of receiver tube


    double u_coolant = m_dot_salt / (n_t*rho_coolant*pow((id_tube / 2.0), 2)*CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
    double Re_inner = rho_coolant * u_coolant*id_tube / mu_coolant;				        //[-] Reynolds number of internal flow
    double Pr_inner = c_p_coolant * mu_coolant / k_coolant;						        //[-] Prandtl number of internal flow
    double Nusselt_t, f;
    double LoverD = m_h_rec / id_tube;
    double RelRough = (4.5e-5) / id_tube;   //[-] Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm
    CSP::PipeFlow(Re_inner, Pr_inner, LoverD, RelRough, Nusselt_t, f);

    double deltap, wdot;
    calc_pump_performance(rho_coolant, m_dot_salt, f, deltap, wdot);

    return wdot / m_q_rec_des;
}

double C_mspt_receiver_222::area_proj()
{
    return CSP::pi * m_d_rec * m_h_rec; //[m^2] projected or aperture area of the receiver
}
