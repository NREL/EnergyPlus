/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
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
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHERm
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "csp_solver_pt_receiver.h"
#include "csp_solver_mspt_receiver.h"
#include "csp_solver_core.h"
#include "sam_csp_util.h"

#include "Ambient.h"
#include "definitions.h"

C_mspt_receiver::C_mspt_receiver()
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
	m_m_dot_htf_max = std::numeric_limits<double>::quiet_NaN();

	m_itermode = -1;
	m_od_control = std::numeric_limits<double>::quiet_NaN();
	m_eta_field_iter_prev = std::numeric_limits<double>::quiet_NaN();
	m_tol_od = std::numeric_limits<double>::quiet_NaN();
	m_q_dot_inc_min = std::numeric_limits<double>::quiet_NaN();

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
	
	m_ncall = -1;

	//Transient model parameters
	m_is_transient = 0;
	m_is_startup_transient = 0;
	m_rec_tm_mult = std::numeric_limits<double>::quiet_NaN();
	m_u_riser = std::numeric_limits<double>::quiet_NaN();
	m_th_riser = std::numeric_limits<double>::quiet_NaN();
	m_th_downc = std::numeric_limits<double>::quiet_NaN();
	m_piping_loss_coeff = std::numeric_limits<double>::quiet_NaN();
	m_riser_tm_mult = std::numeric_limits<double>::quiet_NaN();
	m_downc_tm_mult = std::numeric_limits<double>::quiet_NaN();
	m_id_riser = std::numeric_limits<double>::quiet_NaN();
	m_od_riser = std::numeric_limits<double>::quiet_NaN();
	m_id_downc = std::numeric_limits<double>::quiet_NaN();
	m_od_downc = std::numeric_limits<double>::quiet_NaN();
	m_Rtot_riser = std::numeric_limits<double>::quiet_NaN();
	m_Rtot_downc = std::numeric_limits<double>::quiet_NaN();
	m_tube_flux_preheat = std::numeric_limits<double>::quiet_NaN();
	m_fill_time = std::numeric_limits<double>::quiet_NaN();
	m_flux_ramp_time = std::numeric_limits<double>::quiet_NaN();
	m_heat_trace_power = std::numeric_limits<double>::quiet_NaN();
	m_preheat_target = std::numeric_limits<double>::quiet_NaN();
	m_startup_target_delta = std::numeric_limits<double>::quiet_NaN();

	m_is_startup_from_solved_profile = 0;
	m_is_enforce_min_startup = 1;

	m_n_elem = 0;
	m_nz_tot = 0;
	m_startup_mode = -1;
	m_startup_mode_initial = -1;
	m_n_call_fill = -1;
	m_n_call_fill_initial = -1;
	m_total_startup_time = std::numeric_limits<double>::quiet_NaN();
	m_total_startup_time_initial = std::numeric_limits<double>::quiet_NaN();
	m_minimum_startup_time = std::numeric_limits<double>::quiet_NaN();
	m_total_ramping_time_initial = std::numeric_limits<double>::quiet_NaN();
	m_total_ramping_time = std::numeric_limits<double>::quiet_NaN();
	m_total_preheat_time_initial = std::numeric_limits<double>::quiet_NaN();
	m_total_preheat_time = std::numeric_limits<double>::quiet_NaN();
	m_total_fill_time_initial = std::numeric_limits<double>::quiet_NaN();
	m_total_fill_time = std::numeric_limits<double>::quiet_NaN();
	m_crossover_index = -1;

	m_csky_frac = std::numeric_limits<double>::quiet_NaN();

}

// Identical to C_mspt_receiver_222::init() except for last line (initialize_transient_parameters())
void C_mspt_receiver::init()
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

	initialize_transient_parameters();
	
	return;
}

void C_mspt_receiver::initialize_transient_parameters()
{
	//************** Transient model parameters  **************************
	m_flux_ramp_time *= 3600.0;  //[s], convert from input in [hr]
	m_fill_time *= 3600.0;  //[s], convert from input in [hr]
	m_min_preheat_time *= 3600.;  //[s], convert from input in [hr]

	m_th_riser /= 1.E3;				//[m], Riser wall thickness, convert from input in [mm]
	m_th_downc = m_th_riser;		//[m], Downcomer wall thickness, convert from input in [mm]
	m_heat_trace_power *= 1.e3;		//[W/m-length], Heat trace power for riser and downcomer during startup, convert from input in [kW/m]
	m_initial_temperature += 273.15;  // Initial temperature at start of simulation [K], convert from input in [C]
	m_preheat_target += 273.15;		// Preheat target temperature [K], convert from input in [C]

	// HTF properties
	double rho_htf_inlet = field_htfProps.dens(m_T_htf_cold_des, 1.0);
	double rho_htf_des = field_htfProps.dens((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0, 1.0);
	double mu_htf_des = field_htfProps.visc((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0);
	double rho_tube_des = tube_material.dens((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0, 1.0);
	double c_tube_des = tube_material.Cp((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0)*1000.0;

	// Riser/downcomer sizing, thermal mass, and constant thermal resistance 
	double c_htf_des = field_htfProps.Cp((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0)*1000.0;		//[J/kg-K] Specific heat at design conditions

	m_id_riser = pow(4.0*m_m_dot_htf_des / rho_htf_inlet / CSP::pi / m_u_riser, 0.5);	// Riser ID [m]
	m_id_downc = m_id_riser;
	m_od_riser = m_id_riser + 2.0 * m_th_riser;				// Riser OD [m]
	m_od_downc = m_id_downc + 2.0 * m_th_downc;				// Downcomer OD [m]
	double tm_riser = m_riser_tm_mult * (0.25*CSP::pi*pow(m_id_riser, 2)*rho_htf_des*c_htf_des + 0.25*CSP::pi*(pow(m_od_riser, 2) - pow(m_id_riser, 2))*rho_tube_des*c_tube_des);	// Thermal mass of riser [J/m/K]
	double tm_downc = m_downc_tm_mult * (0.25*CSP::pi*pow(m_id_downc, 2)*rho_htf_des*c_htf_des + 0.25*CSP::pi*(pow(m_od_downc, 2) - pow(m_id_downc, 2))*rho_tube_des*c_tube_des);	// Thermal mass of downcomer [J/m/K]
	double tm_riser_solid = tm_riser - 0.25*CSP::pi*pow(m_id_riser, 2)*rho_htf_des*c_htf_des;		// Thermal mass of riser tube [J/m/K]
	double tm_downc_solid = tm_downc - 0.25*CSP::pi*pow(m_id_downc, 2)*rho_htf_des*c_htf_des;		// Thermal mass of downcomer tube [J/m/K]	


	// Riser/downcomer thermal loss coefficients
	m_piping_loss_coeff = m_pipe_loss_per_m / (0.5*CSP::pi * (m_id_downc*(m_T_htf_hot_des - 298.) + m_id_riser * (m_T_htf_cold_des - 298.))); // Piping wetted loss coefficient (W/m2/K) to reproduce user-specified total piping loss (W/m)	
	m_piping_loss_coeff = fmax(1.e-4, m_piping_loss_coeff);
	m_Rtot_riser = 1.0 / (m_piping_loss_coeff * 0.5 * m_id_riser);  // Riser total thermal resistance between fluid and ambient [K*m/W]
	m_Rtot_downc = 1.0 / (m_piping_loss_coeff * 0.5 * m_id_downc);  // Downcomer total thermal resistance between fluid and ambient [K*m/W]

	
																	
	// Header sizing 
	double dp_header_fract = 0.1;								// Fraction of panel pressure drop allowable in header
	double L_header = 2.0*(CSP::pi*m_d_rec / m_n_panels);		// Header length [m] = 2 x panel width
	double m_m_dot_head = m_m_dot_htf_des / m_n_lines;			// Mass flow rate through header [kg/s]
	double  ftube_des, Nutube_des, m_id_header, m_th_header, m_od_header;
	ftube_des = Nutube_des = m_id_header = m_th_header = m_od_header = std::numeric_limits<double>::quiet_NaN();
	double m_per_tube_des = m_m_dot_htf_des / ((double)m_n_lines * (double)m_n_t);									// [kg/s] Mass flow per tube at design point
	double utube_des = m_per_tube_des / (rho_htf_des* m_id_tube * m_id_tube * 0.25 * CSP::pi);						//[m/s] Average velocity of the coolant through the receiver tubes
	double Retube_des = rho_htf_des * utube_des*m_id_tube / mu_htf_des;												//[-] Reynolds number of internal flow for receiver tubes
	CSP::PipeFlow(Retube_des, 4.0, m_LoverD, m_RelRough, Nutube_des, ftube_des);									// Calculate friction factor for receiver tube
	double dp_tube = 0.5*rho_htf_des*ftube_des*pow(utube_des, 2) * (m_h_rec / m_id_tube + 2 * 16.0 + 4 * 30.0);		//[Pa] Tube pressure drop including (2) 45deg. bends and (4) 90deg. bends at design point mass flow
	double dp_header = dp_header_fract * dp_tube;																	// Allowable header pressure drop [Pa]
	calc_header_size(dp_header, m_m_dot_head, rho_htf_des, mu_htf_des, L_header, m_id_header, m_th_header, m_od_header);	// Calculate header size
	double tm_header_tot = L_header * (0.25*CSP::pi*pow(m_id_header, 2)*rho_htf_des*c_htf_des + 0.25*CSP::pi*(pow(m_od_header, 2) - pow(m_id_header, 2))*rho_tube_des*c_tube_des); // Total header thermal mass [J/K]

	 // Crossover header sizing
	double tm_header_cross, tm_header_cross_solid, od_header_cross, id_header_cross;
	tm_header_cross = tm_header_cross_solid = od_header_cross = id_header_cross = 0;
	if (m_flow_type == 1 || m_flow_type == 2) 
	{
		double th_header_cross = std::numeric_limits<double>::quiet_NaN();
		calc_header_size(dp_header, m_m_dot_head, rho_htf_des, mu_htf_des, m_d_rec, id_header_cross, th_header_cross, od_header_cross);	// Calculate header size
		tm_header_cross = 0.25*CSP::pi*pow(id_header_cross, 2)*rho_htf_des*c_htf_des + 0.25*CSP::pi*(pow(od_header_cross, 2) - pow(id_header_cross, 2))*rho_tube_des*c_tube_des;	// Thermal mass of crossover header tube wall and fluid [J/m/K]
		tm_header_cross_solid = tm_header_cross - 0.25*CSP::pi*pow(id_header_cross, 2)*rho_htf_des*c_htf_des;	// Thermal mass of crossover header tube wall [W/m/K]
	}

	// Receiver tube thermal mass (including inter-panel headers)  [J/m/K]
	double tm_tube = m_rec_tm_mult * (0.25*CSP::pi*pow(m_id_tube, 2)*rho_htf_des*c_htf_des + 0.25*CSP::pi*(pow(m_od_tube, 2) - pow(m_id_tube, 2))*rho_tube_des*c_tube_des + tm_header_tot / m_h_rec / (double)m_n_t);	// Thermal mass of receiver tube and fluid including the inter-panel header [J/m/K]
	double tm_tube_solid = tm_tube - 0.25*CSP::pi*pow(m_id_tube, 2)*rho_htf_des*c_htf_des - (0.25*CSP::pi*pow(m_id_header, 2)*rho_htf_des*c_htf_des)*L_header / m_h_rec / (double)m_n_t;		// Thermal mass of receiver tube including inter-panel header [J/m/K]

	// Set up PDE parameters : dT/dt + lam1*dT/dz + lam2*T = C
	m_n_elem = m_n_panels / m_n_lines + 2;		// Number of flow elements in each flow path: Inter-panel headers are lumped with panels for simplicity
	int nz_panel = 4;						    // Number of axial evaluation points per panel or crossover header
	int nz_tower = 8;							// Number of axial evaluation points per riser or downcomer
	m_nz_tot = nz_panel * m_n_panels / m_n_lines + 2 * nz_tower;	// Total number of axial evaluation points in one flow path
	int crossposition = 0;
	if (m_flow_type == 1 || m_flow_type == 2)	// Flow path contains a crossover header
	{
		m_n_elem = m_n_elem + 1;
		m_nz_tot = m_nz_tot + nz_panel;
		double npq = (double)m_n_panels / 4.;
		int nq1;
		if (m_n_panels % 4 != 0)
			nq1 = (int)floor(npq) + 1;
		else
			nq1 = (int)floor(npq + 1.e-6);
		crossposition = nq1 + 1;		// Location of crossover header in array of all flow elements
	}

	trans_inputs.nelem = m_n_elem;
	trans_inputs.nztot = m_nz_tot;
	trans_inputs.npath = m_n_lines;
	trans_inputs.length.resize(m_n_elem, m_h_rec);	// Total length of each flow element [m]
	trans_inputs.nz.resize(m_n_elem, nz_panel);		// # of axial evaluation points per flow element
	trans_inputs.zpts.resize(m_nz_tot);				// Axial point positions (z = 0 at inlet of each flow element)
	trans_inputs.startpt.resize(m_n_elem);			// Index of first axial position associated with each flow element

	trans_inputs.lam1.resize_fill(m_n_elem, m_n_lines, 0.0);	// Parameter 1 (lam1) [m/s] 
	trans_inputs.lam2.resize_fill(m_n_elem, m_n_lines, 0.0);	// Parameter 2 (lam2) [1/s]
	trans_inputs.cval.resize_fill(m_n_elem, m_n_lines, 0.0);	// Parameter 3 (C) [K/s]
	trans_inputs.aval.resize_fill(m_n_elem, m_n_lines, 0.0);	// Parameter 4 (a) [K/s^2]
	trans_inputs.Rtube.resize_fill(m_n_elem, m_n_lines, 0.0);
	trans_inputs.tinit.resize(m_nz_tot, m_n_lines);
	trans_inputs.tinit_wall.resize(m_nz_tot, m_n_lines);

	m_tinit.resize_fill(m_nz_tot, m_n_lines, m_initial_temperature);  // Initial temperature profile [K]
	m_tinit_wall.resize_fill(m_nz_tot, m_n_lines, m_initial_temperature);  // Initial wall temperature profile [K]

	m_flowelem_type.resize(m_n_elem, m_n_lines);	// Identifier for each flow element in flow path order: positive integer = receiver panel number, -1 = riser, -2 = downcomer, -3 = crossover header
	m_flowelem_type.fill(0);
	m_tm.resize(m_n_elem, tm_tube);
	m_tm_solid.resize(m_n_elem, tm_tube_solid);
	m_od.resize(m_n_elem, m_od_tube);
	m_id.resize(m_n_elem, m_id_tube);

	// Fill in tube panel positions in flow order
	int k = 0;
	for (int j = 0; j < m_n_panels / m_n_lines; j++)
	{
		k = j + 1;
		if ((m_flow_type == 1 || m_flow_type == 2) && k >= crossposition)	// Panel resides after a crossover header
			k = k + 1;
		for (int i = 0; i < m_n_lines; i++)
			m_flowelem_type.at(k, i) = m_flow_pattern.at(i, j);
	}

	// Fill in riser/downcomer/crossover header parameters
	trans_inputs.length.at(0) = trans_inputs.length.at((size_t)m_n_elem - 1) = 0.5*(m_h_tower*m_pipe_length_mult + m_pipe_length_add);
	trans_inputs.nz.at(0) = trans_inputs.nz.at((size_t)m_n_elem - 1) = nz_tower;
	m_tm.at(0) = tm_riser;
	m_tm_solid.at(0) = tm_riser_solid;
	m_tm.at((size_t)m_n_elem - 1) = tm_downc;
	m_tm_solid.at((size_t)m_n_elem - 1) = tm_downc_solid;
	m_od.at(0) = m_od_riser;
	m_od.at((size_t)m_n_elem - 1) = m_od_downc;
	m_id.at(0) = m_id_riser;
	m_id.at((size_t)m_n_elem - 1) = m_id_downc;
	if (m_flow_type == 1 || m_flow_type == 2)
	{
		trans_inputs.length.at(crossposition) = m_d_rec;
		m_tm.at(crossposition) = tm_header_cross;
		m_tm_solid.at(crossposition) = tm_header_cross_solid;
		m_od.at(crossposition) = od_header_cross;
		m_id.at(crossposition) = id_header_cross;
		m_crossover_index = crossposition;
	}
	for (int i = 0; i < m_n_lines; i++)
	{
		m_flowelem_type.at(0, i) = -1;
		m_flowelem_type.at((size_t)m_n_elem - 1, i) = -2;
		if (m_flow_type == 1 || m_flow_type == 2)
			m_flowelem_type.at(crossposition, i) = -3;
	}

	// Set local axial positions
	double dz;
	size_t s = 0;
	for (size_t j = 0; j < m_n_elem; j++)
	{
		trans_inputs.startpt.at(j) = s;
		int nspace = trans_inputs.nz.at(j)-1;
		dz = trans_inputs.length.at(j) / (double)nspace;	// Spacing between axial points
		for (size_t i = 0; i < trans_inputs.nz.at(j); i++)		// Loop over axial positions
			trans_inputs.zpts.at(s + i) = dz * i;
		s = s + trans_inputs.nz.at(j);;
	}

	trans_outputs.timeavg_tout = trans_outputs.timeavg_conv_loss = trans_outputs.timeavg_rad_loss = trans_outputs.timeavg_piping_loss = trans_outputs.timeavg_qthermal = trans_outputs.timeavg_qnet = trans_outputs.timeavg_eta_therm = trans_outputs.time_min_tout = ::numeric_limits<double>::quiet_NaN();
	trans_outputs.max_tout = trans_outputs.min_tout = trans_outputs.max_rec_tout = std::numeric_limits<double>::quiet_NaN();
	trans_outputs.timeavg_temp.resize_fill(m_n_elem, m_n_lines, 0.0);

	trans_outputs.t_profile.resize_fill(m_nz_tot, m_n_lines, m_initial_temperature);
	trans_outputs.t_profile_wall.resize_fill(m_nz_tot, m_n_lines, m_initial_temperature);
	trans_outputs.tube_temp_inlet = trans_outputs.tube_temp_outlet = m_initial_temperature;

	param_inputs.T_amb = param_inputs.T_sky = param_inputs.c_htf = param_inputs.rho_htf = param_inputs.mu_htf = param_inputs.k_htf = param_inputs.Pr_htf = std::numeric_limits<double>::quiet_NaN();
	param_inputs.Tfeval.resize_fill(m_n_elem, m_n_lines, 0.0); param_inputs.Tseval.resize_fill(m_n_elem, m_n_lines, 0.0); param_inputs.qinc.resize_fill(m_n_elem, m_n_lines, 0.0);
	param_inputs.qheattrace.resize_fill(m_n_elem, 0.0);
	return; 
}




void C_mspt_receiver::call(const C_csp_weatherreader::S_outputs &weather, 
	const C_csp_solver_htf_1state &htf_state_in,
	const C_mspt_receiver::S_inputs &inputs,
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
    m_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
	m_E_su = std::numeric_limits<double>::quiet_NaN();
	m_t_su = std::numeric_limits<double>::quiet_NaN();

	m_itermode = 1;

	double v_wind = log((m_h_tower + m_h_rec / 2) / 0.003) / log(10.0 / 0.003)*v_wind_10;

	double c_p_coolant, rho_coolant, f, u_coolant, q_conv_sum, q_rad_sum, q_dot_inc_sum, q_dot_inc_min_panel, q_dot_piping_loss;
	c_p_coolant = rho_coolant = f = u_coolant = q_conv_sum = q_rad_sum = q_dot_inc_sum = q_dot_inc_min_panel = std::numeric_limits<double>::quiet_NaN();
	double eta_therm, m_dot_salt_tot, T_salt_hot, m_dot_salt_tot_ss, T_salt_hot_rec;
	eta_therm = m_dot_salt_tot = T_salt_hot = m_dot_salt_tot_ss = T_salt_hot_rec = std::numeric_limits<double>::quiet_NaN();
	double clearsky = std::numeric_limits<double>::quiet_NaN();

	bool rec_is_off = false;
	bool rec_is_defocusing = false;
	double field_eff_adj = 0.0;

	double panel_req_preheat = m_tube_flux_preheat * m_od_tube * m_h_rec * m_n_t*1000;					// Panel absorbed solar energy required to meet preheat flux requirement (W)
	double total_req_preheat = (m_tube_flux_preheat * m_od_tube * m_h_rec * m_n_t) * m_n_panels*1000;	// Total absorbed solar energy on all panels (W) required to meet preheat flux requirement
	bool startup_low_flux = false;

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


	s_steady_state_soln soln, soln_actual, soln_clearsky;
	soln.hour = time / 3600.0;
	soln.T_amb = weather.m_tdry + 273.15;
	soln.T_dp = weather.m_tdew + 273.15;
	soln.v_wind_10 = weather.m_wspd;
	soln.p_amb = weather.m_pres * 100.0;

	soln.dni = I_bn;
	soln.field_eff = field_eff;
	soln.T_salt_cold_in = T_salt_cold_in;	// Cold salt inlet temperature (K)
	soln.od_control = m_od_control;         // Initial defocus control (will be adjusted during the solution)
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
			}
		}

		//--- Save steady state solution for mass flow for next iteration
		m_mflow_soln_prev = soln_actual;
		m_mflow_soln_csky_prev = soln_clearsky;


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

	// Adjust receiver state if SS mass flow calculation did not converge, but incident flux is sufficient to begin startup
	if (m_is_startup_transient)
	{
		if (m_mode_prev == C_csp_collector_receiver::OFF || m_mode_prev == C_csp_collector_receiver::STARTUP)	// Receiver was either off or starting up in previous time step
		{
			if (rec_is_off && q_dot_inc_sum >= total_req_preheat && q_dot_inc_min_panel >= panel_req_preheat)	// Total absorbed solar flux and minimum panel absorbed solar flux are sufficient to begin preheating
			{
				rec_is_off = false;
				startup_low_flux = true;
			}
		}
	}

	// Calculate solution parameters needed for transient model
	if (!rec_is_off && (m_is_transient || m_is_startup_transient) && (input_operation_mode != C_csp_collector_receiver::STEADY_STATE))
	{
		trans_inputs.inlet_temp = T_salt_cold_in;
		trans_inputs.tinit = m_tinit;
		trans_inputs.tinit_wall = m_tinit_wall;
		initialize_transient_param_inputs(soln, param_inputs);
	}

	double q_heat_trace_energy = 0.0;	//[J]
	double q_startup_energy = 0.0;		//[J]

	q_startup = 0.0;

	double time_required_su = step/3600.0;

	if( !rec_is_off )
	{
		m_dot_salt_tot_ss = m_dot_salt_tot;
		double m_dot_rec_des = m_q_rec_des / (c_p_coolant*(m_T_htf_hot_des - m_T_htf_cold_des)); // Design point receiver mass flow rate (kg/s)

		switch( input_operation_mode )
		{
		case C_csp_collector_receiver::STARTUP:

			// Startup model based on fixed time and energy requirements
			if (!m_is_startup_transient)
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

				rec_is_off = true;
				calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump);
				if (m_is_transient && m_mode == C_csp_collector_receiver::ON)		// Define temperature profile after startup if transient receiver model will be solved
				{
					param_inputs.tm = m_tm;	// Select combined fluid/solid thermal mass values
					param_inputs.mflow_tot = m_dot_salt_tot;
					param_inputs.finitial = 1.0;
					param_inputs.ffinal = 1.0;
					param_inputs.ramptime = 0.0;
					update_pde_parameters(false, param_inputs, trans_inputs);
					calc_ss_profile(trans_inputs, trans_outputs.t_profile, trans_outputs.t_profile_wall);
				}
			}


			// Startup model based on transient response of receiver temperature
			if (m_is_startup_transient)
			{
				double time_remaining = step;			// Remaining time in current time step [s]
				double min_circulate_time = 0.0;

				if (q_dot_inc_sum >= total_req_preheat && q_dot_inc_min_panel >= panel_req_preheat) // Available flux is sufficient for preheating	
				{
					double heat_trace_target = m_T_htf_cold_des;	// Target riser/downcomer temperature at end of heat_trace startup stage [K]
					double preheat_target = m_preheat_target;		// Target tube temperature at end of preheat startup stage [K]
					double circulation_target = T_salt_hot + m_startup_target_delta;	// Target HTF outlet temperature at end of circulation startup stage [K]
					double m_dot_salt_startup = 0.0;				// Mass flow rate during startup 
					double Tmin_rec, Tmin_piping;
					Tmin_rec = Tmin_piping = std::numeric_limits<double>::quiet_NaN();

					m_mode = C_csp_collector_receiver::STARTUP;

					if (m_startup_mode_initial == -1)			// Startup didn't begin in a previous time step
					{
						m_startup_mode = HEAT_TRACE;
						m_total_startup_time_initial = 0.0;
						m_total_ramping_time_initial = 0.0;
						m_total_preheat_time_initial = 0.0;
						m_total_fill_time_initial = 0.0;
						m_n_call_fill_initial = -1;
						m_minimum_startup_time = m_rec_su_delay*3600.0;
						if (!m_is_startup_from_solved_profile)   // Start from ambient temperature
						{
							trans_inputs.tinit.fill(T_amb);
							trans_inputs.tinit_wall.fill(T_amb);
						}
					}
					else
						m_startup_mode = m_startup_mode_initial;

					// Calculate current minimum receiver and piping temperatrues
					Tmin_rec = 5000.0;
					Tmin_piping = 5000.0;
					for (int i = 0; i < m_n_lines; i++)
					{
						for (int j = 0; j < m_n_elem; j++)
						{
							size_t p1 = trans_inputs.startpt.at(j);
							double Twall_min = fmin(trans_inputs.tinit.at(p1, i), trans_inputs.tinit.at(p1 + trans_inputs.nz.at(j)-1, i));
							if (m_flowelem_type.at(j, i) >= 0)  //Receiver
								Tmin_rec = fmin(Tmin_rec, Twall_min);
							if (m_flowelem_type.at(j, i) == -1 || m_flowelem_type.at(j, i) == -2) // Riser or downcomer
								Tmin_piping = fmin(Tmin_piping, Twall_min);
						}
					}

					if (m_startup_mode_initial == -1 && !m_is_enforce_min_startup && Tmin_rec > preheat_target)
						m_minimum_startup_time = 0.0;  // Minimum time not enforced for this start when receiver begins above preheat temperature

					m_total_startup_time = m_total_startup_time_initial;		// Total startup time completed in previous time steps
					m_total_ramping_time = m_total_ramping_time_initial;
					m_total_preheat_time = m_total_preheat_time_initial;
					m_total_fill_time = m_total_fill_time_initial;
					m_n_call_fill = m_n_call_fill_initial;
					util::matrix_t<double> tinit_start = trans_inputs.tinit;    // Initial temperature profile at the start of the overall time step [K]	
					util::matrix_t<double> tinit_wall_start = trans_inputs.tinit_wall;
					util::matrix_t<double> q_inc_panel_full = param_inputs.qinc;  // Absorbed solar energy on panel (W) during operation

					double time_flow = 0.0;
					
					while (time_remaining > 0.1 && m_mode == C_csp_collector_receiver::STARTUP)		// Receiver is starting up and there is time remaining in the current time step
					{
						trans_inputs.lam1.fill(0.0);  trans_inputs.lam2.fill(0.0);  trans_inputs.cval.fill(0.0);  // Reinitialize PDE solution parameters
						param_inputs.qinc.fill(0.0);  param_inputs.qheattrace.fill(0.0);
						param_inputs.finitial = 1.0;
						param_inputs.ffinal = 1.0;
						param_inputs.ramptime = 0.0;

						double time, energy, parasitic;
						time = energy = parasitic = std::numeric_limits<double>::quiet_NaN();

						switch (m_startup_mode)
						{
						case HEAT_TRACE:
						{
							if (Tmin_piping > heat_trace_target)  // Riser and downcomer already above heat trace target temperature
								m_startup_mode = PREHEAT;
							else
							{
								param_inputs.qinc.fill(0.0);
								param_inputs.tm = m_tm_solid;
								param_inputs.mflow_tot = 0.0;
								solve_transient_startup_model(param_inputs, trans_inputs, HEAT_TRACE, heat_trace_target, 0.0, time_remaining, trans_outputs, time, energy, parasitic);
								q_heat_trace_energy += parasitic;
								//q_startup_energy += energy;   
								m_total_startup_time += time;		
								time_remaining -= time;	
								m_startup_mode = HEAT_TRACE;
								if (time_remaining > 0)						// Heat tracing startup stage is completed before the end of the time step --> move to preheat stage
								{
									m_startup_mode = PREHEAT;
									trans_inputs.tinit = trans_outputs.t_profile;
									trans_inputs.tinit_wall = trans_outputs.t_profile_wall;
								}
									
							}
						}
						break;

						case PREHEAT:
						{
							if (Tmin_rec > preheat_target)  // Skip preheat stage-> entire receiver is already above preheat target temperature
								m_startup_mode = FILL;
							else
							{
								param_inputs.mflow_tot = 0.0;
								param_inputs.tm = m_tm_solid;
								param_inputs.qinc.fill(m_tube_flux_preheat*1000. * m_od_tube * m_h_rec); // Absorbed solar energy on tube (W) for preheat stage
								solve_transient_startup_model(param_inputs, trans_inputs, PREHEAT, preheat_target, 0.0, time_remaining, trans_outputs, time, energy, parasitic);
								q_startup_energy += energy;
								q_heat_trace_energy += parasitic;
								m_total_preheat_time += time;
								m_total_startup_time += time;
								time_remaining -= time;		
								m_startup_mode = PREHEAT;
								if (time_remaining > 0)			
								{
									if (m_total_preheat_time < m_min_preheat_time)  // Receiver has reached preheat target temperature, but minimum preheat time requirement has not been met
										m_startup_mode = PREHEAT_HOLD;
									else
										m_startup_mode = FILL;
									trans_inputs.tinit = trans_outputs.t_profile;
									trans_inputs.tinit_wall = trans_outputs.t_profile_wall;

									
								}
							}
						}
						break;

						case PREHEAT_HOLD:
						{
							// Keep minimum flux on receiver necessary to maintain temperature on the receiver until required preheat time has elapsed
							param_inputs.mflow_tot = 0.0;
							param_inputs.tm = m_tm_solid;
							double time_preheat = fmax(0.01, fmin(time_remaining, m_min_preheat_time - m_total_preheat_time));
							solve_transient_startup_model(param_inputs, trans_inputs, PREHEAT_HOLD, preheat_target, 0.0, time_preheat, trans_outputs, time, energy, parasitic);
							q_startup_energy += energy;
							q_heat_trace_energy += parasitic;
							m_total_preheat_time += time;
							m_total_startup_time += time;
							time_remaining -= time;
							m_startup_mode = PREHEAT_HOLD;
							if (time_remaining > 0)
							{
								m_startup_mode = FILL;
								trans_inputs.tinit = trans_outputs.t_profile;
								trans_inputs.tinit_wall = trans_outputs.t_profile_wall;
							}

						}

						case FILL:
						{
							m_n_call_fill++;

							// Update initial temperature profile after preheating
							if (m_n_call_fill == 0) // First time during the current startup that the fluid is in the receiver --> "tinit" still contains solid temperature profile, initial fluid temperature = cold inlet temperature
							{
								for (size_t j = 0; j < m_n_elem; j++)
								{
									double tinit_avg = (m_tm_solid.at(j) / m_tm.at(j)) * trans_inputs.tinit.at(trans_inputs.startpt.at(j), 0) + (1.0 - m_tm_solid.at(j) / m_tm.at(j))* T_salt_cold_in;	// Mass-weighted average of solid and fluid temperatures in element j
									for (size_t i = 0; i < m_n_lines; i++)
									{
										for (size_t k = 0; k < trans_inputs.nz.at(j); k++)
											trans_inputs.tinit.at((size_t)trans_inputs.startpt.at(j) + k, i) = tinit_avg;
									}
								}
							}
							trans_inputs.tinit_wall = trans_inputs.tinit;

							param_inputs.mflow_tot = 0.0;
							param_inputs.tm = m_tm;
							double fill_time = fmax(0.01, fmin(time_remaining, m_fill_time - m_total_fill_time));
							solve_transient_startup_model(param_inputs, trans_inputs, FILL, 0.0, 0.0, fill_time, trans_outputs, time, energy, parasitic);
							q_startup_energy += energy;
							q_heat_trace_energy += parasitic;
							m_total_fill_time += time;
							m_total_startup_time += time;
							time_remaining -= time;

							m_startup_mode = FILL;
							if (time_remaining > 0)
							{
								m_startup_mode = CIRCULATE;
								trans_inputs.tinit = trans_outputs.t_profile;
								trans_inputs.tinit_wall = trans_outputs.t_profile_wall;
							}

						}
						break;

						case CIRCULATE:
						{
							m_dot_salt_startup = fmax(m_dot_salt_tot, m_f_rec_min *m_dot_rec_des);	// HTF mass flow rate during startup (kg/s)	
							param_inputs.mflow_tot = m_dot_salt_startup;
							param_inputs.tm = m_tm;
							param_inputs.qinc = q_inc_panel_full;		

							double ramp_time = 0.0;
							if (m_flux_ramp_time > 0.0)
							{
								ramp_time = fmin(time_remaining, m_flux_ramp_time - m_total_ramping_time);
								param_inputs.ramptime = ramp_time;
								param_inputs.finitial = fmin(1.0, m_total_ramping_time / m_flux_ramp_time);   // Fraction of full flux at start of the time step
								param_inputs.ffinal = fmin(1.0, (m_total_ramping_time + param_inputs.ramptime) / m_flux_ramp_time); // Fraction of full flux at the end of the time step
							}

							solve_transient_startup_model(param_inputs, trans_inputs, CIRCULATE, circulation_target, min_circulate_time, time_remaining, trans_outputs, time, energy, parasitic);
							q_startup_energy += energy;
							q_heat_trace_energy += parasitic;
							m_total_startup_time += time;
							m_total_ramping_time += ramp_time;
							time_flow += time;
							time_remaining -= time;
							if (time_remaining <= 1e-6 || trans_outputs.tout<circulation_target)  // Target outlet temperature not achieved within the current time step
								m_startup_mode = CIRCULATE;
							else
							{
								if (m_total_startup_time < m_minimum_startup_time)		// Receiver has reached temperature target, but minimum startup time requirement has not been met
								{
									m_startup_mode = HOLD;
									trans_inputs.tinit = trans_outputs.t_profile;
									trans_inputs.tinit_wall = trans_outputs.t_profile_wall;
								}
								else			// Receiver is finished starting up and the minimum startup time requirement has been met
								{
									m_mode = C_csp_collector_receiver::ON;
									m_startup_mode = -1;
								}
							}
						}
						break;

						case HOLD:
						{
							double required_hold_time = m_minimum_startup_time - m_total_startup_time;	// Time remaining (s) to satisfy the minimum required startup time
							double time_hold = fmax(0.01, fmin(required_hold_time, time_remaining));
							param_inputs.tm = m_tm;		
							param_inputs.qinc = q_inc_panel_full;
							m_dot_salt_startup = fmax(m_dot_salt_tot, m_f_rec_min*m_dot_rec_des);
							param_inputs.mflow_tot = m_dot_salt_startup;
							if (time_hold >= time_remaining)		// Startup time requirement will not be fulfilled in the current time step
							{
								solve_transient_startup_model(param_inputs, trans_inputs, HOLD, 0.0, 0.0, time_remaining, trans_outputs, time, energy, parasitic);
								q_startup_energy += energy;
								q_heat_trace_energy += parasitic;
								m_total_startup_time += time;
								time_flow += time;
								time_remaining -= time;	
								m_startup_mode = HOLD;
							}
							else  // Startup time requirement will be fulfilled in the current time step --> call in "circulate" mode to ensure that temperature requirement will also be met
							{
								m_startup_mode = CIRCULATE;
								min_circulate_time = time_hold;
							}
						}
						break;
						}
					}

					q_startup = q_startup_energy / 3600.0;					// Startup energy (W-hr) --> Doesn't include heat trace energy
					time_required_su = (step - time_remaining) / 3600.0;	// Total amount of time in current time step needed for startup [hr]
					trans_inputs.tinit = tinit_start;						// Revert back to initial temperature profile at the start of the full time step
					trans_inputs.tinit_wall = tinit_wall_start;
					m_dot_salt_tot = m_dot_salt_startup;					// Mass flow rate (kg/s)
					W_dot_pump = 0.0;
					if (time_flow > 0)						// HTF is flowing for at least part of the startup time
					{
						// Re-calculate pressure drop based on flow rate during startup
						double mu_coolant = field_htfProps.visc(T_coolant_prop);		//[kg/m-s] Absolute viscosity of the coolant
						double k_coolant = field_htfProps.cond(T_coolant_prop);			//[W/m-K] Conductivity of the coolant
						rho_coolant = field_htfProps.dens(T_coolant_prop, 1.0);			//[kg/m^3] Density of the coolant
						double fstartup, Nusselt_t;
						u_coolant = m_dot_salt_tot / (m_n_t*rho_coolant*pow((m_id_tube / 2.0), 2)*CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
						double Re_inner = rho_coolant*u_coolant*m_id_tube / mu_coolant;				//[-] Reynolds number of internal flow
						double Pr_inner = c_p_coolant*mu_coolant / k_coolant;						//[-] Prandtl number of internal flow
						CSP::PipeFlow(Re_inner, Pr_inner, m_LoverD, m_RelRough, Nusselt_t, fstartup);
						calc_pump_performance(rho_coolant, m_dot_salt_tot, fstartup, Pres_D, W_dot_pump);
						W_dot_pump = W_dot_pump*(time_flow) / (time_required_su*3600.0);	 // Average pump work over the startup time
					}

				}
				else			// Not enough power available to begin (or continue) startup
				{
					q_startup = 0.0;
					m_mode = C_csp_collector_receiver::OFF;
				}
				rec_is_off = true;
			}

			break;

		case C_csp_collector_receiver::ON:

			// Steady state receiver model
			if (!m_is_transient)
			{
				if (m_E_su_prev > 0.0 || m_t_su_prev > 0.0)
				{
					m_E_su = fmax(0.0, m_E_su_prev - m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in)*step / 3600.0);	//[W-hr]
					m_t_su = fmax(0.0, m_t_su_prev - step / 3600.0);	//[hr]

					if (m_E_su + m_t_su > 0.0)
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

					if (q_dot_inc_sum < m_q_dot_inc_min)
					{
						// If output here is less than specified allowed minimum, then need to shut off receiver
						m_mode = C_csp_collector_receiver::OFF;

						// Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
						W_dot_pump = 0.0;
						// Pressure drops
						DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;
					}
				}
				q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in);
				q_thermal_ss = m_dot_salt_tot_ss*c_p_coolant*(T_salt_hot - T_salt_cold_in);
				calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump);

				if (m_mode == C_csp_collector_receiver::ON && m_is_startup_from_solved_profile)  // Calculate temperature profile
				{
					param_inputs.tm = m_tm;	
					param_inputs.mflow_tot = m_dot_salt_tot;
					update_pde_parameters(false, param_inputs, trans_inputs);
					calc_ss_profile(trans_inputs, trans_outputs.t_profile, trans_outputs.t_profile_wall);
				}

			}

			// Transient receiver model
			if (m_is_transient)
			{
				q_startup = 0.0;
				m_mode = C_csp_collector_receiver::ON;
				q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in);			// Steady state thermal power (W)
				q_thermal_ss = m_dot_salt_tot_ss*c_p_coolant*(T_salt_hot - T_salt_cold_in);
				calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump);

				if (q_dot_inc_sum < m_q_dot_inc_min)				// Receiver is not allowed to operate
				{
					m_mode = C_csp_collector_receiver::OFF;
					W_dot_pump = 0.0;
					DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;
				}
				else
				{
					param_inputs.tm = m_tm;	// Set thermal mass values with both fluid and solid
					param_inputs.mflow_tot = m_dot_salt_tot;
					param_inputs.finitial = 1.0;
					param_inputs.ffinal = 1.0;
					param_inputs.ramptime = 0.0;
					solve_transient_model(step, 100.0, param_inputs, trans_inputs, trans_outputs);
					trans_outputs.timeavg_eta_therm = 1.0 - (trans_outputs.timeavg_conv_loss + trans_outputs.timeavg_rad_loss) / (q_dot_inc_sum);	//[-] Time-averaged recevier thermal efficiency during the time step
				}
			}

			if (q_dot_inc_sum < m_q_dot_inc_min)
				rec_is_off = true;

			break;

		case C_csp_collector_receiver::STEADY_STATE:

			m_mode = C_csp_collector_receiver::STEADY_STATE;
			f_rec_timestep = 1.0;
			calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump);
			q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot - T_salt_cold_in);
			q_thermal_ss = m_dot_salt_tot_ss*c_p_coolant*(T_salt_hot - T_salt_cold_in);

			if (m_is_startup_transient && startup_low_flux)    // Incident flux is high enough for startup but not for steady state operation. Report nonzero q_thermal to allow startup
				q_thermal = q_dot_inc_sum;
			else
			{
				if (q_dot_inc_sum < m_q_dot_inc_min && m_mode_prev == C_csp_collector_receiver::ON)
					rec_is_off = true;
			}

			break;
		
		}	// End switch() on input_operation_mode

	}
	else
	{	// If receiver was off BEFORE startup deductions
		m_mode = C_csp_collector_receiver::OFF;

		// Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
		W_dot_pump = 0.0;
		// Pressure drops
		DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;

		m_startup_mode_initial = -1;
		m_startup_mode = -1;
	}

	if( rec_is_off )
	{
		// 900 continue	// Receiver isn't producing usable energy
		m_dot_salt_tot = 0.0; eta_therm = 0.0; /*W_dot_pump = 0.0;*/
		q_conv_sum = 0.0; q_rad_sum = 0.0; m_T_s.fill(0.0); q_thermal = 0.0;
		// Set the receiver outlet temperature equal to the inlet design temperature
		T_salt_hot = m_T_htf_cold_des;
		T_salt_hot_rec = m_T_htf_cold_des;
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

		if (m_is_transient || m_is_startup_transient)
		{
			trans_outputs.timeavg_conv_loss = 0.0; trans_outputs.timeavg_rad_loss = 0.0; trans_outputs.timeavg_piping_loss = 0.0; trans_outputs.timeavg_qthermal = 0.0;
			trans_outputs.timeavg_tout = m_T_htf_cold_des;
			trans_outputs.tout = m_T_htf_cold_des;
			trans_outputs.max_tout = m_T_htf_cold_des;
			trans_outputs.min_tout = m_T_htf_cold_des;
			trans_outputs.max_rec_tout = m_T_htf_cold_des;
		}
	}

	// Steady state outputs
	outputs.m_m_dot_salt_tot = m_dot_salt_tot*3600.0;		//[kg/hr] convert from kg/s
	outputs.m_eta_therm = eta_therm;							//[-] RECEIVER thermal efficiency (includes radiation and convective losses. reflection losses are contained in receiver flux model)
	outputs.m_W_dot_pump = W_dot_pump / 1.E6;				//[MW] convert from W
	outputs.m_q_conv_sum = q_conv_sum / 1.E6;				//[MW] convert from W
	outputs.m_q_rad_sum = q_rad_sum / 1.E6;					//[MW] convert from W
	outputs.m_Q_thermal = q_thermal / 1.E6;					//[MW] convert from W
	outputs.m_T_salt_hot = T_salt_hot - 273.15;		//[C] convert from K
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


	outputs.m_inst_T_salt_hot = T_salt_hot - 273.15;
	outputs.m_max_T_salt_hot = T_salt_hot - 273.15;
	outputs.m_min_T_salt_hot = T_salt_hot - 273.15;
	outputs.m_max_rec_tout = T_salt_hot_rec - 273.15;
	outputs.m_q_heattrace = 0.0;
	outputs.m_Twall_inlet = 0.0;
	outputs.m_Twall_outlet = 0.0;
	outputs.m_Triser = 0.0;
	outputs.m_Tdownc = 0.0;

	// Transient model outputs
	if ((m_is_transient && input_operation_mode == C_csp_collector_receiver::ON) || (m_is_startup_transient && input_operation_mode == C_csp_collector_receiver::STARTUP))		// Transient model is solved
	{
		if (q_dot_inc_sum == 0.0)
			outputs.m_eta_therm = 0.0;
		else
			outputs.m_eta_therm = trans_outputs.timeavg_eta_therm;		//[-] Time-averaged recevier thermal efficiency during the time step

		outputs.m_q_conv_sum = trans_outputs.timeavg_conv_loss / 1.e6;	//[MWt] Time-averaged receiver convective heat loss
		outputs.m_q_rad_sum = trans_outputs.timeavg_rad_loss / 1.e6;		//[MWt] Time-averaged receiver radiative heat loss
		outputs.m_Q_thermal = trans_outputs.timeavg_qthermal / 1.e6;		//[MWt] Time-averaged thermal power delivered to TES/PC
		outputs.m_q_dot_piping_loss = trans_outputs.timeavg_piping_loss / 1.e6;			//[MWt] Time-averaged piping loss
		outputs.m_q_heattrace = q_heat_trace_energy / 1.e6 / 3600.0;		//[MWt-hr] Power required for heat tracing during the time step
		outputs.m_T_salt_hot = trans_outputs.timeavg_tout - 273.15;		//[C] Time-averaged downcomer outlet T during the time step
		outputs.m_inst_T_salt_hot = trans_outputs.tout - 273.15;			//[C] Instantaneous salt outlet T at the end of the time step
		outputs.m_max_T_salt_hot = trans_outputs.max_tout - 273.15;		//[C] Maximum salt outlet T during the time step
		outputs.m_min_T_salt_hot = trans_outputs.min_tout - 273.15;		//[C] Minimum salt outlet T during the time step
		outputs.m_max_rec_tout = trans_outputs.max_rec_tout - 273.15;	//[C] Maximum salt T (receiver outlet) during the time step
		outputs.m_Twall_inlet = trans_outputs.tube_temp_inlet - 273.15;	//[C] Average receiver wall temperature at inlet
		outputs.m_Twall_outlet = trans_outputs.tube_temp_outlet - 273.15;  //[C] Average receiver wall temperature at receiver outlet
		outputs.m_Triser = trans_outputs.t_profile.at(0, 0) - 273.15;	//[C] Average riser wall temperature at inlet
		outputs.m_Tdownc = trans_outputs.t_profile.at((size_t)m_nz_tot-1, 0) - 273.15;  //[C] Average downcomer wall temperature at outlet

	}

	outputs.m_clearsky = clearsky;  // W/m2
	outputs.m_Q_thermal_csky_ss = q_thermal_csky / 1.e6; //[MWt]
	outputs.m_Q_thermal_ss = q_thermal_steadystate / 1.e6; //[MWt]

    ms_outputs = outputs;

	m_eta_field_iter_prev = field_eff;	//[-]
}


void C_mspt_receiver::off(const C_csp_weatherreader::S_outputs &weather,
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
	
	outputs.m_inst_T_salt_hot = 0.0;
	outputs.m_max_T_salt_hot = 0.0;
	outputs.m_min_T_salt_hot = 0.0;
	outputs.m_max_rec_tout = 0.0;
	outputs.m_q_heattrace = 0.0;
	outputs.m_Twall_inlet = 0.0;
	outputs.m_Twall_outlet = 0.0;
	outputs.m_Triser = 0.0;
	outputs.m_Tdownc = 0.0;

	m_startup_mode = -1;
	
	// Solve transient model for receiver wall temperatures
	if (m_is_startup_from_solved_profile)
	{
		double step = sim_info.ms_ts.m_step;
		double hour = sim_info.ms_ts.m_time / 3600.0;
		param_inputs.T_amb = weather.m_tdry + 273.15;
		param_inputs.T_sky = CSP::skytemp(weather.m_tdry + 273.15, weather.m_tdew + 273.15, hour);
		param_inputs.qinc.fill(0.0);
		param_inputs.tm = m_tm_solid;
		param_inputs.mflow_tot = 0.0;
		param_inputs.wspd = weather.m_wspd;
		param_inputs.pres = weather.m_pres*100.;
		
		trans_inputs.tinit = m_tinit_wall;
		trans_inputs.tinit_wall = m_tinit_wall;
		trans_inputs.inlet_temp = 0.0;
		solve_transient_model(step, 50.0, param_inputs, trans_inputs, trans_outputs);
		outputs.m_Twall_inlet = trans_outputs.tube_temp_inlet - 273.15;
		outputs.m_Twall_outlet = trans_outputs.tube_temp_outlet - 273.15;
		outputs.m_Triser = trans_outputs.t_profile.at(0, 0)-273.15;
		outputs.m_Tdownc = trans_outputs.t_profile.at(trans_inputs.startpt.at((size_t)m_n_elem - 1), 0) - 273.15;

		// Overwrite solution for crossover header assuming same cooling rate as receiver
		for (size_t j = 0; j < m_n_elem; j++)
		{
			if (m_flowelem_type.at(j, 0) == -3)  
			{
				for (size_t i = 0; i < m_n_lines; i++)
				{
					trans_outputs.timeavg_temp.at(j, i) = trans_outputs.timeavg_temp.at(j-1, i);
					size_t krec = trans_inputs.startpt.at(j-1);
					for (size_t q = 0; q < trans_inputs.nz.at(j); q++)
					{
						int k = trans_inputs.startpt.at(j) + q;
						trans_outputs.t_profile.at(k, i) = trans_outputs.t_profile.at(krec, i);
						trans_outputs.t_profile_wall.at(k, i) = trans_outputs.t_profile.at(k, i);
					}
				}
			}
		}
		
	}

	outputs.m_clearsky = get_clearsky(weather, sim_info.ms_ts.m_time / 3600.);  // clear-sky DNI (set to actual DNI if actual DNI is higher than computed clear-sky value)
	outputs.m_Q_thermal_csky_ss = 0.0; //[MWt]
	outputs.m_Q_thermal_ss = 0.0; //[MWt]

    ms_outputs = outputs;

	return;
}

void C_mspt_receiver::converged()
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

	m_startup_mode_initial = m_startup_mode;
	m_n_call_fill_initial = m_n_call_fill;
	m_total_startup_time_initial = m_total_startup_time;
	m_total_ramping_time_initial = m_total_ramping_time;
	m_total_preheat_time_initial = m_total_preheat_time;
	m_total_fill_time_initial = m_total_fill_time;

	m_tinit = trans_outputs.t_profile;
	m_tinit_wall = trans_outputs.t_profile_wall;

    ms_outputs = outputs;
}

bool C_mspt_receiver::use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev)
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


// Calculate flux profiles (interpolated to receiver panels at specified DNI) -> Same as C_mspt_receiver_222::calculate_flux_profiles
util::matrix_t<double> C_mspt_receiver::calculate_flux_profiles(double dni, double field_eff, double od_control, const util::matrix_t<double> *flux_map_input)
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
			q_dot_inc.at(i) = q_flux_sum * m_A_node / n_flux_x_d * n_panels_d *1000.;
		}

	}

	return q_dot_inc;
}

// Calculate steady state temperature and heat loss profiles for a given mass flow and incident flux -> Same as C_mspt_receiver_222::calculate_steady_state_soln
void C_mspt_receiver::calculate_steady_state_soln(s_steady_state_soln &soln, double tol, int max_iter)
{
	double P_amb = soln.p_amb;
	double hour = soln.hour;
	double T_dp = soln.T_dp;
	double T_amb = soln.T_amb;
	double v_wind_10 = soln.v_wind_10;
	double T_sky = CSP::skytemp(T_amb, T_dp, hour);
	double v_wind = log((m_h_tower + m_h_rec / 2) / 0.003) / log(10.0 / 0.003) * v_wind_10;

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
		double c_p_coolant = field_htfProps.Cp(T_coolant_prop) * 1000.;

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
		double Re_for = rho_film * v_wind * m_d_rec / mu_film;		//[-] Reynolds number
		double ksD = (m_od_tube / 2.0) / m_d_rec;					//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
		double Nusselt_for = CSP::Nusselt_FC(ksD, Re_for);			//[-] S&K
		double h_for = Nusselt_for * k_film / m_d_rec * m_hl_ffact;	//[W/m^2-K] Forced convection heat transfer coefficient

		// Convection coefficient for external natural convection using Siebers & Kraabel
		// Note: This relationship applies when the surrounding properties are evaluated at ambient conditions [S&K]
		double beta = 1.0 / T_amb;													//[1/K] Volumetric expansion coefficient
		double nu_amb = ambient_air.visc(T_amb) / ambient_air.dens(T_amb, P_amb);	//[m^2/s] Kinematic viscosity		

		for (size_t j = 0; j < m_n_lines; j++)
		{
			for (size_t i = 0; i < m_n_panels / m_n_lines; i++)
			{
				int i_fp = m_flow_pattern.at(j, i);

				// Natural convection
				double Gr_nat = fmax(0.0, CSP::grav * beta * (soln.T_s.at(i_fp) - T_amb) * pow(m_h_rec, 3) / pow(nu_amb, 2));	//[-] Grashof Number at ambient conditions
				double Nusselt_nat = 0.098 * pow(Gr_nat, (1.0 / 3.0)) * pow(soln.T_s.at(i_fp) / T_amb, -0.14);				//[-] Nusselt number
				double h_nat = Nusselt_nat * ambient_air.cond(T_amb) / m_h_rec * m_hl_ffact;							//[W/m^-K] Natural convection coefficient

				// Mixed convection
				double h_mixed = pow((pow(h_for, m_m_mixed) + pow(h_nat, m_m_mixed)), 1.0 / m_m_mixed) * 4.0;		//(4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)
				soln.q_dot_conv.at(i_fp) = h_mixed * m_A_node * (soln.T_s.at(i_fp) - T_film.at(i_fp));			//[W] Convection losses per node

				// Radiation from the receiver - Calculate the radiation node by node
				soln.q_dot_rad.at(i_fp) = 0.5 * CSP::sigma * m_epsilon * m_A_node * (2.0 * pow(soln.T_s.at(i_fp), 4) - pow(T_amb, 4) - pow(T_sky, 4)) * m_hl_ffact;	//[W] Total radiation losses per node
				soln.q_dot_loss.at(i_fp) = soln.q_dot_rad.at(i_fp) + soln.q_dot_conv.at(i_fp);			//[W] Total overall losses per node
				soln.q_dot_abs.at(i_fp) = soln.q_dot_inc.at(i_fp) - soln.q_dot_loss.at(i_fp);			//[W] Absorbed flux at each node

				// Calculate the temperature drop across the receiver tube wall... assume a cylindrical thermal resistance
				double T_wall = (soln.T_s.at(i_fp) + soln.T_panel_ave.at(i_fp)) / 2.0;				//[K] The temperature at which the conductivity of the wall is evaluated
				double k_tube = tube_material.cond(T_wall);											//[W/m-K] The conductivity of the wall
				double R_tube_wall = m_th_tube / (k_tube * m_h_rec * m_d_rec * pow(CSP::pi, 2) / 2.0 / (double)m_n_panels);	//[K/W] The thermal resistance of the wall

				// Calculations for the inside of the tube						
				double mu_coolant = field_htfProps.visc(T_coolant_prop);							//[kg/m-s] Absolute viscosity of the coolant
				double k_coolant = field_htfProps.cond(T_coolant_prop);								//[W/m-K] Conductivity of the coolant
				double rho_coolant = field_htfProps.dens(T_coolant_prop, 1.0);						//[kg/m^3] Density of the coolant
				double u_coolant = soln.m_dot_salt / (m_n_t * rho_coolant * pow((m_id_tube / 2.0), 2) * CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
				double Re_inner = rho_coolant * u_coolant * m_id_tube / mu_coolant;					//[-] Reynolds number of internal flow
				double Pr_inner = c_p_coolant * mu_coolant / k_coolant;								//[-] Prandtl number of internal flow

				double Nusselt_t, f;
				CSP::PipeFlow(Re_inner, Pr_inner, m_LoverD, m_RelRough, Nusselt_t, f);
				if (Nusselt_t <= 0.0)
				{
					soln.mode = C_csp_collector_receiver::OFF;
					break;
				}
				double h_inner = Nusselt_t * k_coolant / m_id_tube;								//[W/m^2-K] Convective coefficient between the inner tube wall and the coolant
				double R_conv_inner = 1.0 / (h_inner * CSP::pi * m_id_tube / 2.0 * m_h_rec * m_n_t);	//[K/W] Thermal resistance associated with this value

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


				T_panel_out_guess.at(i_fp) = T_panel_in_guess.at(i_fp) + soln.q_dot_abs.at(i_fp) / (soln.m_dot_salt * c_p_coolant);		//[K] Energy balance for each node		
				double Tavg = (T_panel_out_guess.at(i_fp) + T_panel_in_guess.at(i_fp)) / 2.0;											//[K] Panel average temperature
				T_s_guess.at(i_fp) = Tavg + soln.q_dot_abs.at(i_fp) * (R_conv_inner + R_tube_wall);										//[K] Surface temperature based on the absorbed heat
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
				double riser_loss = 2.0 * CSP::pi * (soln.T_salt_cold_in - T_amb) / m_Rtot_riser; //[W/m]
				double downc_loss = 2.0 * CSP::pi * (soln.T_salt_hot - T_amb) / m_Rtot_downc; //[W/m]
				soln.Q_dot_piping_loss = 0.5 * (riser_loss + downc_loss) * (m_h_tower * m_pipe_length_mult + m_pipe_length_add); // Total piping thermal loss [W]
			}
			double delta_T_piping = soln.Q_dot_piping_loss / (m_dot_salt_tot_temp * c_p_coolant);	//[K]
			soln.T_salt_hot_rec = soln.T_salt_hot;
			soln.T_salt_hot -= delta_T_piping;	//[K]
		}


		// Check convergence
		double err = (soln.T_salt_hot - T_salt_hot_guess) / T_salt_hot_guess;
		T_salt_hot_guess = soln.T_salt_hot;
		if (fabs(err) < tol && q > 0)
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

// Calculate mass flow rate needed to achieve target outlet temperature (m_T_salt_hot_target) given incident flux profiles -> Same as C_mspt_receiver_222::solve_for_mass_flow
void C_mspt_receiver::solve_for_mass_flow(s_steady_state_soln &soln)
{
	
	bool soln_exists = (soln.m_dot_salt == soln.m_dot_salt);

	soln.T_salt_props = (m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0;		//[K] The temperature at which the coolant properties are evaluated. Validated as constant (mjw)
	double c_p_coolant = field_htfProps.Cp(soln.T_salt_props) * 1000.0;				//[J/kg-K] Specific heat of the coolant

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

		double c_guess = field_htfProps.Cp((m_T_salt_hot_target + soln.T_salt_cold_in) / 2.0) * 1000.;	//[kJ/kg-K] Estimate the specific heat of the fluid in receiver

		if (soln.dni > 1.E-6)
		{
			double q_guess = 0.85 * q_dot_inc_sum;		//[kW] Estimate the thermal power produced by the receiver			
			m_dot_salt_guess = q_guess / (c_guess * (m_T_salt_hot_target - soln.T_salt_cold_in) * m_n_lines);	//[kg/s] Mass flow rate for each flow path			
		}
		else	// The tower recirculates at night (based on earlier conditions)
		{
			// Enter recirculation mode, where inlet/outlet temps switch
			double T_salt_hot = m_T_salt_hot_target;
			m_T_salt_hot_target = soln.T_salt_cold_in;
			soln.T_salt_cold_in = T_salt_hot;
			m_dot_salt_guess = -3500.0 / (c_guess * (m_T_salt_hot_target - soln.T_salt_cold_in) / 2.0);

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
			m_dot_salt_guess *= (soln.T_salt_hot - soln.T_salt_cold_in) / ((1.0 - 0.5 * tol) * m_T_salt_hot_target - soln.T_salt_cold_in);
		else
			converged = true;

	}

	soln.m_dot_salt_tot = soln.m_dot_salt * m_n_lines;

	return;
}

// Calculate mass flow rate and defocus needed to achieve target outlet temperature given DNI -> Same as C_mspt_receiver_222::solve_for_mass_flow_and_defocus
void C_mspt_receiver::solve_for_mass_flow_and_defocus(s_steady_state_soln &soln, double m_dot_htf_max, const util::matrix_t<double> *flux_map_input)
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

// Calculate defocus needed to maintain outlet temperature under the target value given DNI and mass flow -> Same as C_mspt_receiver_222::solve_for_defocus_given_flow
void C_mspt_receiver::solve_for_defocus_given_flow(s_steady_state_soln &soln, const util::matrix_t<double> *flux_map_input)
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




void C_mspt_receiver::calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc)
{

    // Pressure drop calculations
	double mpertube = mdot / ((double)m_n_lines * (double)m_n_t);
	double u_coolant = mpertube / (rho_f* m_id_tube * m_id_tube * 0.25 * CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes

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

double C_mspt_receiver::get_pumping_parasitic_coef()
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

double C_mspt_receiver::area_proj()
{
    return CSP::pi * m_d_rec * m_h_rec; //[m^2] projected or aperture area of the receiver
}

double C_mspt_receiver::calc_external_convection_coeff(double T_amb, double P_amb, double wspd, double Twall)
{

	double v_wind_10 = wspd;
	double v_wind = log((m_h_tower + m_h_rec / 2) / 0.003) / log(10.0 / 0.003)*v_wind_10;
	double T_film_ave = 0.5*(T_amb + Twall);
	double Re_for = ambient_air.dens(T_film_ave, P_amb)*v_wind*m_d_rec / ambient_air.visc(T_film_ave);			//[-] Reynolds number for external forced convection
	double ksD = (m_od_tube / 2.0) / m_d_rec;									//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
	double Nufor = CSP::Nusselt_FC(ksD, Re_for);								//[-] S&K
	double hfor = Nufor*ambient_air.cond(T_film_ave) / m_d_rec;					//[W/m^2-K] Forced convection heat transfer coefficient

	double nu_amb = ambient_air.visc(T_amb) / ambient_air.dens(T_amb, P_amb);						//[m^2/s] Kinematic viscosity of ambient air
	double Gr = fmax(0.0, CSP::grav*(Twall-T_amb)*pow(m_h_rec, 3) / pow(nu_amb, 2) / T_amb);		//[-] Grashof Number at ambient conditions
	double Nunat = 0.098*pow(Gr, (1.0 / 3.0))*pow(Twall / T_amb, -0.14);							// [-] Nusselt number
	double hnat = Nunat*ambient_air.cond(T_amb) / m_h_rec;									// [W/m^-K] Natural convection coefficient
	double hmix = pow((pow(hfor, m_m_mixed) + pow(hnat, m_m_mixed)), 1.0 / m_m_mixed)*4.0;	// (4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)

	return hmix;
}



void C_mspt_receiver::calc_thermal_loss(double Ts, double T_amb, double T_sky, double P_amb, double wspd, double &hext, double &qconv, double &qrad)
{
	double vf = (2.0 / CSP::pi);				// View factor between tube surface area and ambient
	double Tfilm = 0.5*(Ts + T_amb);
	hext = calc_external_convection_coeff(T_amb, P_amb, wspd, Ts) * (2.0 / CSP::pi);   // 2/pi scales for loss from tube SA instead of flat SA
	qconv = hext * (Ts - Tfilm)*m_hl_ffact;   // Note use of film T here mimics steady state convection loss calcs, a correction factor is incorporated in hext
	qrad = vf * CSP::sigma*m_epsilon*(pow(Ts, 4) - 0.5*pow(T_amb, 4) - 0.5*pow(T_sky, 4))*m_hl_ffact;
	return;
}

void C_mspt_receiver::calc_surface_temperature(double Tf, double qabs, double Rtube, double OD, double T_amb, double T_sky, double P_amb, double wspd, double &Tsguess)
{
	double hext, qconv, qrad;
	double f, dfdT, Tsguessnew;
	double vf = (2.0 / CSP::pi);  // View factor between tube and ambient
	int qq = 0;
	double delT = 100.0;
	while (delT > 1.0 && qq < 20)
	{
		calc_thermal_loss(Tsguess, T_amb, T_sky, P_amb, wspd, hext, qconv, qrad);
		f = Tsguess - Tf - (qabs - qconv - qrad)*0.5*OD*Rtube;
		dfdT = 1.0 + 0.5*OD*Rtube* (hext + 4.0 * vf * m_epsilon*CSP::sigma*pow(Tsguess, 3));
		Tsguessnew = Tsguess - f / dfdT;
		delT = abs(Tsguessnew - Tsguess);
		Tsguess = Tsguessnew;
		qq++;
	}
}


void C_mspt_receiver::calc_header_size(double pdrop, double mdot, double rhof, double muf, double Lh, double &id_calc, double &th_calc, double &od_calc)
{
	// Calculate minimum header size to meet pressure drop requirement
	double id_min, Re_h;
	double fh = 0.015;		// Initial guess for friction factor in header
	double Nucalc = 0.0;
	double id_min_prev = 0.0;
	for (int i = 0; i < 10; i++){
		id_min = pow(8.0*fh*mdot*mdot*Lh / rhof / pow(CSP::pi, 2) / pdrop, 0.2);		// Minimum Header ID [m] to meet pressure drop requirement
		Re_h = 4.0*mdot / CSP::pi / muf / id_min;										// Reynolds number for header
		CSP::PipeFlow(Re_h, 4.0, Lh / id_min, 4.5e-5 / id_min, Nucalc, fh);				// Update header friction factor
		if (fabs(id_calc - id_min_prev) <= 0.001)
			break;
		else
			id_min_prev = id_min;
	}

	// Find schedule 40 pipe size with ID >= calculated minimum header size 
	double wall, id;
	double odin[26] = { 0.405, 0.54, 0.675, 0.84, 1.05, 1.315, 1.66, 1.9, 2.375, 2.875, 3.5, 4.0, 4.5, 5.563, 6.625, 8.625, 10.75, 12.75, 14.0, 16.0, 18.0, 20.0, 24.0, 32.0, 34.0, 36.0 };		//Pipe OD [in]
	double wallin[26] = { 0.068, 0.088, 0.091, 0.109, 0.113, 0.133, 0.14, 0.145, 0.154, 0.203, 0.216, 0.226, 0.237, 0.258, 0.28, 0.322, 0.365, 0.406, 0.437, 0.5, 0.562, 0.593, 0.687, 0.688, 0.688, 0.75 }; //Schedule 40 pipe wall thickness [in]
	int i = 0;
	while (id_min / 0.0254 > odin[i] - 2 * wallin[i] && i <= 25){
		i++;
	}
	if (i <= 25){
		wall = wallin[i] * 0.0254; id = odin[i] * 0.0254 - 2 * wall;
	}
	else{
		id = id_min; wall = wallin[25] * 0.0254;
	}
	id_calc = id;
	th_calc = wall;
	od_calc = id + 2 * wall;
}

double C_mspt_receiver::interpolate(double x, const std::vector<double> &xarray, const std::vector<double> &yarray, int klow, int khigh)
{
	// Linear interpolation between discrete tabulated points
	// x = value of independent variable
	// xarray = independent variable points (assumed to be increasing between klow and khigh), 
	// yarray = dependent variable points
	// klow, khigh = lowest,highest indicies of interest 

	int jl = klow, ju = khigh;
	int jm;
	while (ju - jl > 1)
	{
		jm = (ju + jl) / 2;	//middle index of the range
		if (x < xarray.at(jm)) ju = jm;
		else jl = jm;
	}
	double yinterp = yarray.at(jl) + (yarray.at(ju) - yarray.at(jl)) / (xarray.at(ju) - xarray.at(jl)) * (x - xarray.at(jl));
	return yinterp;
}

double C_mspt_receiver::integrate(double xlow, double xhigh, const std::vector<double> &xarray, const std::vector<double> &yarray, int klow, int khigh)
{

	// Numerical integral between upper and lower bounds xlow and xhigh
	// xarray = independent variable points (assumed to be increasing between klow and khigh), 
	// yarray = dependent variable points
	// klow, khigh = lowest,highest indicies of interest 

	size_t i = klow; size_t j = (size_t)khigh - 1;
	while (i < khigh && xarray.at(i) < xlow)		// i = first point > lower integration bound
		i++;
	while (j >= klow && xarray.at(i) > xhigh)		// j = last point < upper integration bound
		j--;

	// Interpolate to find values at lower and upper integration bounds
	double y1 = yarray.at(i);
	if (i>klow)   y1 = yarray.at(i) + (yarray.at(i) - yarray.at(i - 1)) / (xarray.at(i) - xarray.at(i - 1)) * (xlow - xarray.at(i));
	double y2 = yarray.at(j);
	if (j<khigh)   y2 = yarray.at(j) + (yarray.at(j) - yarray.at(j + 1)) / (xarray.at(j) - xarray.at(j + 1)) * (xhigh - xarray.at(j));

	double inteval = 0.0;
	for (size_t k = i; k < j; k++)		// Intergral between tabulated points entirely included in the integration range
		inteval = inteval + (xarray.at(k + 1) - xarray.at(k)) * 0.5 * (yarray.at(k) + yarray.at(k + 1));
	inteval = inteval + (xarray.at(i) - xlow) * 0.5 * (y1 + yarray.at(i));
	if (j >= i)
		inteval = inteval + (xhigh - xarray.at(j)) * 0.5 * (yarray.at(j) + y2);

	return inteval;
}

void C_mspt_receiver::cubic_splines(const std::vector<double> &xarray, const std::vector<double> &yarray, util::matrix_t<double> &splines)
{
	// Fit cubic splines to data points in xarray, yarray
	size_t n = xarray.size()-1;
	splines.resize_fill(n, 5, 0.0);

	vector<double> a(n + 1, 0.0);
	vector<double> b(n, 0.0);
	vector<double> d(n, 0.0);
	vector<double> h(n, 0.0);
	vector<double> alpha(n, 0.0);
	vector<double> c(n + 1, 0.0);
	vector<double> l(n + 1, 0.0);
	vector<double> mu(n + 1, 0.0);
	vector<double> z(n + 1, 0.0);

	a = yarray;
	l.at(0) = 1.0;
	mu.at(0) = 0.0;
	z.at(0) = 0.0;
	for (size_t i = 0; i < n; i++)
	{
		h.at(i) = xarray.at(i + 1) - xarray.at(i);
		if (i > 0)
		{
			alpha.at(i) = (3.0 / h.at(i)) * (a.at(i + 1) - a.at(i)) - (3.0 / h.at(i - 1))*(a.at(i) - a.at(i - 1));
			l.at(i) = 2.0*(xarray.at(i + 1) - xarray.at(i-1)) - h.at(i - 1)*mu.at(i - 1);
			mu.at(i) = h.at(i) / l.at(i);
			z.at(i) = (alpha.at(i) - h.at(i - 1)*z.at(i - 1)) / l.at(i);
		}
	}

	l.at(n) = 1.0;
	z.at(n) = 0.0;
	c.at(n) = 0.0;
	for (int i = n - 1; i >= 0; i--)
	{
		size_t j = (size_t)i;
		c.at(j) = z.at(j) - mu.at(j)*c.at(j + 1);
		b.at(j) = (a.at(j + 1) - a.at(j)) / h.at(j) - h.at(j)*(c.at(j + 1) + 2.0*c.at(j)) / 3.0;
		d.at(j) = (c.at(j + 1) - c.at(j)) / 3.0 / h.at(j);
	}

	for (size_t i = 0; i < n; i++)
	{
		splines.at(i, 0) = a.at(i);
		splines.at(i, 1) = b.at(i);
		splines.at(i, 2) = c.at(i);
		splines.at(i, 3) = d.at(i);
		splines.at(i, 4) = xarray.at(i);
	}
}

double C_mspt_receiver::calc_timeavg_exit_temp(double tstep, int flowid, int pathid, const transient_inputs &tinputs){

	/*=====================================================================================
	Calculate time-averaged outlet temperature for each flow element (receiver panel, downcomer, riser, etc)
	Temperature is described by PDE: dT/dt + lam1*dT/dz + lam2*T = c with constant parameters lam1, lam2, c

	tstep = time step (s)
	flowid = integer flow element (riser, receiver panel, downcomer, etc.) position in the flow path
	pathid = integer flow path index

	tinputs:
	inlet_temp = cold fluid inlet temperature (K)
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C+a*t
	lam2(j,i) = PDE parameter for element j in flow path i
	cval(j,i) = PDE parameter for element j in flow path i
	aval(m,i) = PDE parameter for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]
	timeavg(j,i) = time averaged fluid outlet temperature for element j in flow path i
	=======================================================================================*/

	double Tavg = std::numeric_limits<double>::quiet_NaN();
	size_t p = flowid;
	size_t nelem = tinputs.nelem;
	double Tfin = tinputs.inlet_temp;

	vector<double> lam1, lam2, cval, aval, len, gam, Tinit;
	lam1.resize(nelem); lam2.resize(nelem); cval.resize(nelem); aval.resize(nelem); len.resize(nelem); gam.resize(nelem), Tinit.resize(tinputs.nztot);
	for (size_t i = 0; i < nelem; i++)
	{
		lam1.at(i) = tinputs.lam1.at(i, pathid);
		lam2.at(i) = tinputs.lam2.at(i, pathid);
		cval.at(i) = tinputs.cval.at(i, pathid);
		aval.at(i) = tinputs.aval.at(i, pathid);
		len.at(i) = tinputs.length.at(i);
		gam.at(i) = lam2.at(i) / lam1.at(i);
	}
	for (size_t i = 0; i < tinputs.nztot; i++)
		Tinit.at(i) = tinputs.tinit.at(i, pathid);

	double T1 = Tinit.at((size_t)tinputs.startpt.at(p) + (size_t)tinputs.nz.at(p) - 1);  // Initial T at outlet
	if (tstep < 1.e-3)		// Numerical limit for small time steps: time-average outlet temperature = initial outlet temperature
		Tavg = T1;
	else
	{
		if (lam1.at(p) == 0.0) // No mass flow rate 
		{
			if (lam2.at(p) != 0.0)
				Tavg = (1.0 / lam2.at(p)) * (cval.at(p) - aval.at(p) / lam2.at(p) + aval.at(p)*tstep / 2.0) + (1.0 - exp(-lam2.at(p)*tstep)) / lam2.at(p) / tstep * (T1 - cval.at(p) / lam2.at(p) + aval.at(p) / lam2.at(p) / lam2.at(p));
			else
				Tavg = T1 + cval.at(p) *tstep / 2.0 + aval.at(p) * tstep*tstep / 6.0;
		}
		else
		{
			util::matrix_t<double> sum1, mult;
			vector<double> Tint;
			sum1.resize_fill(p+1, p+1, 0.0);
			mult.resize_fill(p+1, p+1, 1.0);
			Tint.resize(tinputs.nztot);
			for (size_t i = 0; i <= p; i++)
			{
				mult.at(i, i) = exp(-gam.at(i) * len.at(i));
				sum1.at(i, i) = len.at(i) / lam1.at(i);
				for (size_t j = i + 1; j <= p; j++)
				{
					mult.at(i, j) = mult.at(i, j - 1) * exp(-gam.at(j) * len.at(j) );
					sum1.at(i, j) = sum1.at(i, j - 1) + len.at(j) / lam1.at(j);
				}

				size_t j = tinputs.startpt.at(i);
				for (size_t k = 0; k < tinputs.nz.at(i); k++)
					Tint.at(j + k) = Tinit.at(j + k) * exp(gam.at(i) * tinputs.zpts.at(j + k));
			}


			// Find largest integer for which tcritq > tstep
			int q = p;
			double tcritq;
			while (q > -1)
			{
				tcritq = sum1.at(q, p);
				if (tcritq > tstep)
					break;
				q--;
			}

			// Sum terms over q+1 : p
			int qplus1 = q + 1;
			double sum = 0.0;
			for (size_t j = qplus1; j <= p; j++)
			{
				double multval = 1.0;
				if (j + 1 <= p)
					multval = mult.at(j + 1, p);

				double intTj = integrate(0.0, len.at(j), tinputs.zpts, Tint, tinputs.startpt.at(j), tinputs.startpt.at(j) + tinputs.nz.at(j) - 1);		// Integral of initial T*exp(lam2/lam1*z) over full axial coordinate of element j
				double term1, term2, term3;
				term1 = 1. / lam1.at(j) * exp(-gam.at(j)*len.at(j)) * intTj;
				if (lam2.at(j) != 0.0)
				{
					term2 = (cval.at(j) / lam2.at(j) - aval.at(j) / lam2.at(j) / lam2.at(j)) * (len.at(j) / lam1.at(j) - (1.0 - exp(-gam.at(j)*len.at(j))) / lam2.at(j)) + 
						    aval.at(j) / 2.0 / lam2.at(j) * pow((len.at(j) / lam1.at(j)), 2);

					term3 = (tstep - sum1(j, p)) * ((cval.at(j) / lam2.at(j) - aval.at(j) / lam2.at(j) / lam2.at(j))*(1.0 - exp(-gam.at(j)*len.at(j))) + 
													(aval.at(j)*len.at(j) / lam1.at(j) / lam2.at(j)) + (aval.at(j) / 2.0 / lam2.at(j))*(1.0 - exp(-gam.at(j)*len.at(j)))*(tstep - sum1(j, p)));
				}
				else
				{
					term2 = pow((len.at(j) / lam1.at(j)), 2)  * ((cval.at(j) / 2.0) + (aval.at(j) / 6.0) * (len.at(j) / lam1.at(j)));
					term3 = (tstep - sum1(j, p)) * (cval.at(j) + (aval.at(j) / 2.0) * (len.at(j) / lam1.at(j)) + (aval.at(j) / 2.0)* (tstep - sum1(j, p)));
				}
				sum += multval * (term1 + term2 + term3);
			}

			double multval = 1.0;
			if (qplus1 <= p)
				multval = mult.at(qplus1, p);

			double M = std::numeric_limits<double>::quiet_NaN();
			if (q == -1)
				M = Tfin * (tstep-sum1(0, p));
			else
			{
				double tsub = tstep;
				double term1;
				if (qplus1 <= p)
					tsub -= sum1(qplus1, p);

				if (lam2.at(q) != 0.0)
					term1 = (cval.at(q) / lam2.at(q) - aval.at(q) / lam2.at(q) / lam2.at(q)) * (tsub - (1.0 / lam2.at(q))*(1.0 - exp(-lam2.at(q)*tsub))) + aval.at(q) / 2.0 / lam2.at(q) * tsub*tsub;
				else
					term1 = tsub*tsub * (cval.at(q) / 2.0 + aval.at(q) / 6.0 * tsub);

				double intTq = integrate(len.at(q) - lam1.at(q)*tsub, len.at(q), tinputs.zpts, Tint, tinputs.startpt.at(q), tinputs.startpt.at(q) + tinputs.nz.at(q) - 1);		// Integral of initial T*exp(lam2/lam1*z) over partial axial coordinate of element q
				M = term1 + (1.0 / lam1.at(q)) * exp(-gam.at(q)*len.at(q)) * intTq;
			}
			Tavg = (sum + multval*M) / tstep;  // Time-average exit temperature
		}
	}
	return Tavg;
}

double C_mspt_receiver::calc_single_pt(double tpt, double zpt, int flowid, int pathid, const transient_inputs &tinputs)
{
	/*=====================================================================================
	Calculate HTF temperature at a given time and axial position
	Temperature is described by PDE: dT/dt + lam1*dT/dz + lam2*T = c + a*t with constant parameters lam1, lam2, c, a

	tpt = time (s)
	zpt = local flow element axial position (m): z = 0 at the flow element inlet
	flowid = integer flow element (riser, receiver panel, downcomer, etc.) position in the flow path
	pathid = integer flow path index

	tinputs:
	inlet_temp = cold fluid inlet temperature (K)
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C+a*t
	lam2(j,i) = PDE parameter for element j in flow path i
	cval(j,i) = PDE parameter for element j in flow path i
	aval(m,i) = PDE parameter for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]
	timeavg(j,i) = time averaged fluid outlet temperature for element j in flow path i
	=======================================================================================*/

	int k, q;
	double Tpt, Tval, nk, tk, tj, Ap, Aj, Dk, mult, mult2, sum;
	size_t p = flowid;
	size_t nelem = tinputs.nelem;
	double Tfin = tinputs.inlet_temp;

	vector<double> lam1, lam2, cval, aval, len, gam, Tinit;
	lam1.resize(nelem); lam2.resize(nelem); cval.resize(nelem); aval.resize(nelem); len.resize(nelem); gam.resize(nelem), Tinit.resize(tinputs.nztot);
	for (size_t i = 0; i < nelem; i++)
	{
		lam1.at(i) = tinputs.lam1.at(i, pathid);
		lam2.at(i) = tinputs.lam2.at(i, pathid);
		cval.at(i) = tinputs.cval.at(i, pathid);
		aval.at(i) = tinputs.aval.at(i, pathid);
		len.at(i) = tinputs.length.at(i);
		gam.at(i) = lam2.at(i) / lam1.at(i);
	}
	for (size_t i = 0; i < tinputs.nztot; i++)
		Tinit.at(i) = tinputs.tinit.at(i, pathid);


	double np = zpt - lam1.at(p) * tpt;
	if (lam1.at(p) == 0.0 || np >= 0)
	{
		Tval = interpolate(np, tinputs.zpts, Tinit, tinputs.startpt.at(p), tinputs.startpt.at(p) + tinputs.nz.at(p) - 1);		// Intial temperature evaluated at point np
		if (lam2.at(p) != 0)
			Tpt = (cval.at(p) / lam2.at(p) - aval.at(p) / lam2.at(p) / lam2.at(p))*(1.0 - exp(-lam2.at(p)*tpt)) + aval.at(p)*tpt / lam2.at(p) + Tval*exp(-lam2.at(p)*tpt);
		else
			Tpt = Tval + cval.at(p)*tpt + 0.5*aval.at(p)*tpt*tpt;
	}

	else			
	{
		if (lam2.at(p) != 0)
			Ap = (cval.at(p) / lam2.at(p) - (aval.at(p) / lam2.at(p) / lam2.at(p))*(1. - lam2.at(p)*tpt)) * (1. - exp(-gam.at(p)*zpt)) + aval.at(p)*zpt / lam1.at(p) / lam2.at(p) * exp(-gam.at(p)*zpt);
		else
			Ap = (zpt / lam1.at(p)) * (cval.at(p) + aval.at(p)*tpt) - (aval.at(p) / 2.0) * pow(zpt / lam1.at(p), 2);

		k = p;
		nk = -1;
		tk = 0.0;
		while (nk < 0 && k > 0)
		{
			k -= 1;
			if (k == p-1)
				tk = tpt - zpt / lam1.at(p);
			else
			{
				int kplus1 = k + 1;
				tk -= len.at(kplus1) / lam1.at(kplus1);
			}
			nk = len.at(k) - lam1.at(k)*tk;
		}

		if (nk >= 0)
			q = k + 1;
		else
			q = 0;

		int j = p - 1;
		sum = 0.0;
		mult2 = 1.0;
		while (j >= q)
		{
			int jplus1 = j + 1;
			if (j < p-1)
				mult2 *= exp(-gam.at(jplus1)*len.at(jplus1));
			
			tj = 0.0;
			if (j == p - 1)
				tj = tpt - zpt / lam1.at(p);
			else
				tj -= len.at(jplus1) / lam1.at(jplus1);

			if (lam2.at(j) != 0)
				Aj = (cval.at(j) / lam2.at(j) - (aval.at(j) / lam2.at(j) / lam2.at(j))*(1. - lam2.at(j)*tj)) * (1. - exp(-gam.at(j)*len.at(j))) + aval.at(j)*len.at(j) / lam1.at(j) / lam2.at(j) * exp(-gam.at(j)*len.at(j));
			else
				Aj = (len.at(j) / lam1.at(j)) * (cval.at(j) + aval.at(j)*tj) - (aval.at(j) / 2.0) * pow(len.at(j) / lam1.at(j), 2);
			sum += Aj * mult2;
			j--;
		}

		mult = mult2;
		if (q <= p-1)
			mult *= exp(-gam.at(q)*len.at(q));

		if (nk >= 0)
		{
			Tval = interpolate(nk, tinputs.zpts, Tinit, tinputs.startpt.at(k), tinputs.startpt.at(k) + tinputs.nz.at(k) - 1);		// Intial temperature of element k evaluated at point z = nk
			if (lam2.at(k) != 0)
				Dk = (cval.at(k) / lam2.at(k) - aval.at(k) / lam2.at(k) / lam2.at(k))*(1 - exp(-lam2.at(k) * tk)) + aval.at(k)*tk / lam2.at(k) + Tval * exp(-lam2.at(k) * tk);
			else
				Dk = cval.at(k)*tk + 0.5*aval.at(k)*tk*tk + Tval;
			Tpt = Ap + exp(-gam.at(p)*zpt) * (Dk*mult + sum);
		}
		else
			Tpt = Ap + exp(-gam.at(p)*zpt) * (mult*Tfin + sum);
		
	}
	return Tpt;
}

void C_mspt_receiver::calc_axial_profile(double tpt, const transient_inputs &tinputs, util::matrix_t<double> &tprofile){

	/*=====================================================================================
	Calculate axial temperature profile in each flow path at a given time point
	Temperature is described by PDE: dT/dt + lam1*dT/dz + lam2*T = c with constant parameters lam1, lam2, c

	
	tpt = time (s)
	tinputs:
	inlet_temp = cold fluid inlet temperature (K)
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C+a*t
	lam2(j,i) = PDE parameter for element j in flow path i
	cval(j,i) = PDE parameter for element j in flow path i
	aval(m,i) = PDE parameter for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]
	timeavg(j,i) = time averaged fluid outlet temperature for element j in flow path i

	tprofile(j,i) = HTF temperature at axial position j in flow path i
	=======================================================================================*/

	size_t nelem = tinputs.nelem;
	double Tfin = tinputs.inlet_temp;

	//-- ODE Solution with no mass flow rate
	if (tinputs.lam1.at(0, 0) == 0.0) 
	{
		double c, a, lam2, Tinit;
		for (size_t pathid = 0; pathid < tinputs.npath; pathid++)  // Loop over flow paths 
		{
			for (size_t j = 0; j < tinputs.nelem; j++)				// Loop through elements on flow path
			{
				for (size_t i = 0; i < tinputs.nz.at(j); i++)		// Loop through axial positions on flow element j (except for first point)
				{
					size_t k = tinputs.startpt.at(j);				// Index of first axial position in flow element j
					Tinit = tinputs.tinit.at(k + i, pathid);
					c = tinputs.cval.at(j, pathid);
					a = tinputs.aval.at(j, pathid);
					lam2 = tinputs.lam2.at(j, pathid);
					if (lam2 != 0)
						tprofile.at(k + i, pathid) = (c / lam2 + a / lam2 / lam2)*(1.0 - exp(-lam2*tpt)) + a*tpt / lam2 + Tinit*exp(-lam2*tpt);
					else
						tprofile.at(k + i, pathid) = Tinit + c*tpt + 0.5*a*tpt*tpt;
				}
			}
		}
	}
	

	//---PDE solution with nonzero mass flow rate
	else
	{
		double Aconst, Apos, Tval;
		util::matrix_t<double> sumAconst, sumApos, mult;

		for (size_t pathid = 0; pathid < tinputs.npath; pathid++)    // Flow paths
		{
			vector<double> lam1, lam2, cval, aval, len, gam, Tinit;
			lam1.resize(nelem); lam2.resize(nelem); cval.resize(nelem); aval.resize(nelem); len.resize(nelem); gam.resize(nelem), Tinit.resize(tinputs.nztot);
			for (size_t i = 0; i < nelem; i++)
			{
				lam1.at(i) = tinputs.lam1.at(i, pathid);
				lam2.at(i) = tinputs.lam2.at(i, pathid);
				cval.at(i) = tinputs.cval.at(i, pathid);
				aval.at(i) = tinputs.aval.at(i, pathid);
				len.at(i) = tinputs.length.at(i);
				gam.at(i) = lam2.at(i) / lam1.at(i);
			}
			for (size_t i = 0; i < tinputs.nztot; i++)
				Tinit.at(i) = tinputs.tinit.at(i, pathid);

			// Calculate repetitive terms
			sumAconst.resize_fill(nelem, nelem, 0.0);
			sumApos.resize_fill(nelem, nelem, 0.0);
			mult.resize_fill(nelem, nelem, 1.0);
			for (size_t i = 0; i < nelem; i++)
			{
				mult.at(i, i) = exp(-gam.at(i)*len.at(i));
				for (size_t j = i + 1; j < nelem; j++)
					mult.at(i, j) = mult.at(i, j - 1) * exp(-gam.at(j)*len.at(j));
			}

			for (int i = nelem - 1; i >= 0; i--)
			{
				double sum = 0.0;
				for (int j = i; j >= 0; j--)
				{

					if (lam2.at(j) != 0)
					{
						Aconst = (cval.at(j) / lam2.at(j) - aval.at(j) / lam2.at(j)*(1.0 / lam2.at(j) - tpt + sum)) * (1.0 - exp(-gam.at(j)*len.at(j))) + aval.at(j)*len.at(j) / lam1.at(j) / lam2.at(j)*exp(-gam.at(j)*len.at(j));
						Apos = -aval.at(j) / lam2.at(j) * (1.0 - exp(-gam.at(j)*len.at(j)));
					}
					else
					{
						Aconst = len.at(j) / lam1.at(j) * (cval.at(j) + aval.at(j)*tpt - aval.at(j)*sum) - aval.at(j)*len.at(j)*len.at(j) / lam1.at(j) / lam1.at(j);
						Apos = -aval.at(j)*len.at(j) / lam1.at(j);
					}

					if (j == i)
					{
						sumAconst.at(j, i) = Aconst;
						sumApos.at(j, i) = Apos;
					}
					else
					{
						int jplus1 = j + 1;
						sumAconst.at(j, i) = sumAconst.at(jplus1, i) + Aconst*mult.at(jplus1, i);
						sumApos.at(j, i) = sumApos.at(jplus1, i) + Apos*mult.at(jplus1, i);
					}

					sum += len.at(j) / lam1.at(j);
				}
			}

			// Calculate temperature profiles
			size_t j, k, nz, q;
			double zp, np, nk, tk, Dk, sum, Ap;
			for (size_t p = 0; p < nelem; p++)
			{
				nz = tinputs.nz.at(p);
				j = tinputs.startpt.at(p);				// Index of first axial position in flow element p

				if (p == 0)
					tprofile.at(j, pathid) = Tfin;
				else
					tprofile.at(j, pathid) = tprofile.at(j - 1, pathid);

				for (size_t i = 1; i < nz; i++)
				{
					zp = tinputs.zpts.at(j + i);
					np = zp - lam1.at(p)*tpt;
					if (np >= 0)
					{
						k = p;
						nk = np;
						tk = tpt;
						Tval = interpolate(nk, tinputs.zpts, Tinit, j, j + nz - 1);		// Intial temperature of element p evaluated at point z = nk
						if (lam2.at(k) != 0)
							Dk = (cval.at(k) / lam2.at(k) - aval.at(k) / (lam2.at(k) *lam2.at(k))) * (1 - exp(-lam2.at(k) * tk)) + aval.at(k)*tk / lam2.at(k) + Tval * exp(-lam2.at(k) * tk);
						else
							Dk = cval.at(k)*tk + 0.5*aval.at(k)*tk*tk + Tval;
						tprofile.at(j + i, pathid) = Dk;
					}
					else
					{
						k = p;
						nk = -1;
						tk = tpt - zp / lam1.at(p);
						while (nk < 0 && k > 0)
						{
							k = k - 1;
							if (k<p-1)
								tk -= len.at(k + 1) / lam1.at(k + 1);
							nk = len.at(k) - lam1.at(k)*tk;
						}


						q = 0;
						if (nk >= 0)
							q = k + 1;
						sum = 0.0;
						if (p > 0)
							sum = sumAconst.at(q, p - 1) + zp / lam1.at(p) * sumApos.at(q, p - 1);


						if (lam2.at(p) != 0)
							Ap = (cval.at(p) / lam2.at(p) - aval.at(p) / (lam2.at(p)*lam2.at(p))*(1. - lam2.at(p)*tpt)) * (1 - exp(-gam.at(p)*zp)) + aval.at(p)*zp / lam1.at(p) / lam2.at(p) * exp(-gam.at(p)*zp);
						else
							Ap = (zp / lam1.at(p)) * (cval.at(p) + aval.at(p)*tpt) - aval.at(p)*zp*zp / 2.0 / (lam1.at(p) *lam1.at(p));


						if (nk >= 0)
						{
							Tval = interpolate(nk, tinputs.zpts, Tinit, tinputs.startpt.at(k), tinputs.startpt.at(k) + tinputs.nz.at(k) - 1);		// Intial temperature of element k evaluated at point z = nk
							if (lam2.at(k) != 0)
								Dk = (cval.at(k) / lam2.at(k) - aval.at(k) / (lam2.at(k) *lam2.at(k))) * (1 - exp(-lam2.at(k) * tk)) + aval.at(k)*tk / lam2.at(k) + Tval * exp(-lam2.at(k) * tk);
							else
								Dk = cval.at(k)*tk + 0.5*aval.at(k)*tk*tk + Tval;
							tprofile.at(j + i, pathid) = Ap + exp(-gam.at(p)*zp) * (Dk*mult.at(k + 1, p - 1) + sum);
						}
						else
						{
							double m = 1.0;
							if (p > 0)
								m = mult.at(0, p - 1);
							tprofile.at(j + i, pathid) = Ap + exp(-gam.at(p)*zp) * (Tfin * m + sum);
						}
					}
				}
			}
		}

		// Average downcomer T profile if more than one flow path exists
		if (tinputs.npath > 1)
		{
			size_t j = tinputs.startpt.at(nelem - 1);  // First downcomer axial position point
			for (size_t i = 0; i < tinputs.nz.at(nelem - 1); i++)
			{
				tprofile.at(j + i, 0) = 0.5*tprofile.at(j + i, 0) + 0.5*tprofile.at(j + i, 1);
				tprofile.at(j + i, 1) = tprofile.at(j + i, 0);
			}
		}
	}
}

void C_mspt_receiver::calc_extreme_outlet_values(double tstep, int flowid, const transient_inputs &tinputs, util::matrix_t<double> &textreme, util::matrix_t<double> &tpt)
{
	/*=====================================================================================
	Calculate the minimum and maximum outlet temperatures which occur at any point in time within the designated time step
	Temperature is described by PDE: dT/dt + lam1*dT/dz + lam2*T = c + a*t with constant parameters lam1, lam2, c, a

	tstep = time step (s)
	flowid = integer flow element (riser, receiver panel, downcomer, etc.) position in the flow path

	tinputs:
	inlet_temp = cold fluid inlet temperature (K)
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C
	lam2(j,i) = PDE parameter for element j in flow path i
	cval(j,i) = PDE parameter for element j in flow path i
	aval(j,i) = PDE parameter for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]

	textreme = extreme temperature points for each flow path
	tpt = time at which extreme temperature occurs for each flow path

	Note: Calculated values for single downcomer with multiple flow paths are only strictly accurate if parameter lam1 is the same for corresponding elements in each flow path
	=======================================================================================*/
	size_t p = flowid;
	double zp = tinputs.length.at(flowid);

	int nlines = m_n_lines;
	bool combine = false;
	if (flowid == m_n_elem - 1 && m_n_lines > 1) // Downcomer with more than one flow path
	{
		nlines = 1;
		combine = true;
	}

	util::matrix_t<double> lam1, lam2, cval, aval, gam, Tinit;
	lam1.resize(tinputs.nelem, m_n_lines); lam2.resize(tinputs.nelem, m_n_lines); cval.resize(tinputs.nelem, m_n_lines); aval.resize(tinputs.nelem, m_n_lines); gam.resize(tinputs.nelem, m_n_lines); Tinit.resize(tinputs.nztot, m_n_lines);
	for (size_t j = 0; j < m_n_lines; j++)
	{
		for (int i = 0; i < tinputs.nelem; i++)
		{
			lam1.at(i, j) = tinputs.lam1.at(i, j);
			lam2.at(i, j) = tinputs.lam2.at(i, j);
			cval.at(i, j) = tinputs.cval.at(i, j);
			aval.at(i, j) = tinputs.aval.at(i, j);
			gam.at(i, j) = lam2.at(i, j) / lam1.at(i, j);
		}
		for (size_t i = 0; i < tinputs.nztot; i++)
			Tinit.at(i, j) = tinputs.tinit.at(i, j);
	}


	//--- Set initial min/max values to values at beginning or end of time step
	textreme.resize_fill(2, nlines, 0.0);
	tpt.resize_fill(2, nlines, 0.0);
	int s = tinputs.startpt.at(flowid) + tinputs.nz.at(flowid) - 1;	// Axial point index outlet from flow element 
	double Tcalc[2];
	for (size_t i = 0; i < nlines; i++)
	{
		textreme.at(0, i) = textreme.at(1, i) = Tinit.at(s, i);
		if (!combine)
			Tcalc[i] = calc_single_pt(tstep, tinputs.length.at(flowid), flowid, i, tinputs);
		else
			Tcalc[0] = 0.5*(calc_single_pt(tstep, tinputs.length.at(flowid), flowid, 0, tinputs) + calc_single_pt(tstep, tinputs.length.at(flowid), flowid, 1, tinputs));
	}
	for (size_t i = 0; i < nlines; i++)
	{
		if (Tcalc[i] < textreme.at(0, i))
		{
			tpt.at(0, i) = tstep;
			textreme.at(0, i) = Tcalc[i];
		}
		if (Tcalc[i] > textreme.at(1, i))
		{
			tpt.at(1, i) = tstep;
			textreme.at(1, i) = Tcalc[i];
		}
	}

	//--- Find local extreme values
	if (lam1.at(0, 0) != 0)		// lam1 > 0 
	{

		//--- Calculate useful terms
		util::matrix_t<double> sumval;
		util::matrix_t<double> multval;
		sumval.resize_fill(p+1, m_n_lines, 0.0);
		multval.resize_fill(p+1, m_n_lines, 1.0);
		for (size_t m = 0; m < m_n_lines; m++)
		{
			double term1;
			if (lam2.at(p, m) != 0)
				term1 = (aval.at(p, m) / lam2.at(p, m)) * (1.0 - exp(-gam.at(p, m)*zp));
			else
				term1 = aval.at(p, m)*zp / lam1.at(p, m);

			for (size_t j = 0; j < p; j++)
			{
				sumval.at(j, m) = term1;
				multval.at(j, m) *= exp(-gam.at(p, m)*zp);
				for (size_t k = j + 1; k < p; k++)
				{
					multval.at(j, m) *= exp(-gam.at(k, m)* tinputs.length.at(k));
					double mult2 = exp(-gam.at(p, m)*zp);
					for (int l = k + 1; l < p; l++)
						mult2 *= exp(-gam.at(l, m)* tinputs.length.at(l));

					if (lam2.at(k, m) != 0)
						sumval.at(j, m) += aval.at(k, m) / lam2.at(k, m) * (1.0 - exp(-gam.at(k, m) *tinputs.length.at(k))) * mult2;
					else
						sumval.at(j, m) += aval.at(k, m)*tinputs.length.at(k) / lam1.at(k, m) * mult2;
				}
				sumval.at(j,m) *= (-1.0 / lam1.at(j, m));
			}
		}

		for (size_t m = 0; m < nlines; m++)
		{
			double currentsign, sign, Tval, dTval, d2Tval, n, dz, f, df, nnew, ndiff, term2, term3, len, zint;
			currentsign = 0;
			for (int j = p; j >= 0; j--)
			{
				size_t k1 = tinputs.startpt.at(j);

				//--- Fit cubic splines for evaluation of derivatives of initial condition
				vector<double> x(tinputs.nz.at(j));
				vector<double> y(tinputs.nz.at(j));
				util::matrix_t<double> splines, splines2;
				for (size_t i = 0; i < tinputs.nz.at(j); i++)
				{
					x.at(i) = tinputs.zpts.at(k1 + i);
					y.at(i) = Tinit.at(k1 + i, m);
				}
				cubic_splines(x, y, splines);

				if (combine && j < p)
				{
					for (size_t i = 0; i < tinputs.nz.at(j); i++)
						y.at(i) = Tinit.at(k1 + i, m + 1);
					cubic_splines(x, y, splines2);
				}

				//--- Find time points where dT/dt = 0
				len = tinputs.length.at(j);
				zint = tinputs.zpts.at(k1 + 1) - tinputs.zpts.at(k1);
				for (int i = tinputs.nz.at(j) - 1; i >= 0; i--)
				{
					n = tinputs.zpts.at(k1 + i);
					bool stop = false;
					bool found = false;
					int q = 0;
					sign = 0;
					while (q < 50 && !stop)
					{
						q++;
						s = (int) fmin( (int)(n / zint), tinputs.nz.at(j)-2);
						dz = n - splines.at(s, 4);
						Tval = splines.at(s, 0) + splines.at(s, 1)*dz + splines.at(s, 2)*dz*dz + splines.at(s, 3)*dz*dz*dz;
						dTval = splines.at(s, 1) + 2.0 * splines.at(s, 2)*dz + 3.0 * splines.at(s, 3)*dz*dz;
						d2Tval = 2.0 * splines.at(s, 2) + 6.0 * splines.at(s, 3)*dz;

						term2 = cval.at(j, m) / lam1.at(j, m) - gam.at(j, m) * Tval - dTval;
						term3 = (gam.at(j, m) * cval.at(j, m) / lam1.at(j, m) - aval.at(j, m) / lam1.at(j, m) / lam1.at(j, m) - pow(gam.at(j, m), 2)*Tval - 2.0*gam.at(j, m) * dTval - d2Tval);
						if (j == p)
						{
							f = -exp(-gam.at(j, m) *(zp - n))*term2;
							if (lam2.at(j, m) != 0)
								f -= aval.at(j, m) / lam2.at(j, m) / lam1.at(j, m) * (1.0 - exp(-gam.at(j, m) * (zp - n)));
							else
								f -= aval.at(j, m) / lam1.at(j, m) * (zp - n);
							df = -exp(-gam.at(p, m)*(zp - n)) *term3;
						}
						else
						{
							f = sumval.at(j, m) - multval.at(j, m) * exp(-gam.at(j, m) * (len - n)) * term2;
							if (lam2.at(j, m) != 0)
								f -= multval.at(j, m) * (aval.at(j, m) / lam2.at(j, m) / lam1.at(j, m)) * (1.0 - exp(-gam.at(j, m) * (len - n)));
							else
								f -= multval.at(j, m) * aval.at(j, m) / lam1.at(j, m) / lam1.at(j, m) * (len - n);
							df = -multval.at(j, m) * exp(-gam.at(j, m)* (len - n)) *term3;


							if (combine)
							{
								Tval = splines2.at(s, 0) + splines2.at(s, 1)*dz + splines2.at(s, 2)*dz*dz + splines2.at(s, 3)*dz*dz*dz;
								dTval = splines2.at(s, 1) + 2.0 * splines2.at(s, 2)*dz + 3.0 * splines2.at(s, 3)*dz*dz;
								d2Tval = 2.0 * splines2.at(s, 2) + 6.0 * splines2.at(s, 3)*dz;
								term2 = cval.at(j, m + 1) / lam1.at(j, m + 1) - gam.at(j, m + 1) * Tval - dTval;
								term3 = (gam.at(j, m + 1) * cval.at(j, m + 1) / lam1.at(j, m + 1) - aval.at(j, m + 1) / lam1.at(j, m + 1) / lam1.at(j, m + 1) - pow(gam.at(j, m + 1), 2)*Tval - 2.0*gam.at(j, m + 1) * dTval - d2Tval);
								double f2 = sumval.at(j, m+1) - multval.at(j, m+1) * exp(-gam.at(j, m+1) * (len - n)) * term2;
								double df2 = -multval.at(j, m + 1) * exp(-gam.at(j, m + 1)* (len - n)) *term3;
								f = 0.5*(f + f2);
								if (lam2.at(j, m + 1) != 0)
									f -= 0.5* (multval.at(j, m + 1) * (aval.at(j, m + 1) / lam2.at(j, m + 1) / lam1.at(j, m + 1)) * (1.0 - exp(-gam.at(j, m + 1) * (len - n))));
								else
									f -= 0.5*(multval.at(j, m + 1) * aval.at(j, m + 1) / lam1.at(j, m + 1) / lam1.at(j, m + 1) * (len - n));
								df = 0.5*(df + df2);
							}
						}

						if (q == 1)
						{
							sign = (double)(f > 0) - (double)(f < 0);
							if (sign == currentsign)
								stop = true;

							if (sign != currentsign && i == tinputs.nz.at(j) - 1)
							{
								stop = true;
								found = true;
							}
						}

						if (!stop)
						{
							nnew = n - 0.8 * f / df;
							nnew = fmin(nnew, len);
							nnew = fmax(nnew, 0.0);
							ndiff = abs(nnew - n);
							n = nnew;
							if (abs(ndiff / len) < 1.e-3 && abs(f) < 1.e-4)  // Converged
							{
								stop = true;
								found = true;
							}
						}
					}

					currentsign = sign;

					if (found)
					{
						double t, Textreme;

						t = (len - n) / lam1.at(j, m);
						for (int k = j + 1; k < p + 1; k++)
							t += tinputs.length.at(k) / lam1.at(k, m);

						if (t <= tstep)
						{
							Textreme = calc_single_pt(t, zp, flowid, m, tinputs);
							if (combine && j < p)
								Textreme = 0.5*Textreme + 0.5*calc_single_pt(t, zp, flowid, m + 1, tinputs);
							if (Textreme < textreme.at(0, m))
							{
								tpt.at(0, m) = t;
								textreme.at(0, m) = Textreme;
							}
							if (Textreme > textreme.at(1, m))
							{
								tpt.at(1, m) = t;
								textreme.at(1, m) = Textreme;
							}
						}
					}
				}
			}
		}
	}
}

void C_mspt_receiver::calc_ss_profile(const transient_inputs &tinputs, util::matrix_t<double> &tprofile, util::matrix_t<double> &tprofile_wall)
{
	/*=====================================================================================
	Calculate axial temperature profile at steady state (only valid if flux ramp rate is zero)
	Temperature is described by PDE: dT/dt + lam1*dT/dz + lam2*T = c with constant parameters lam1, lam2, c

	tinputs:
	inlet_temp = current cold fluid inlet temperature (K)
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C
	lam2(j,i) = PDE parameter for element j in flow path i
	cval(j,i) = PDE parameter for element j in flow path i
	Rtube(j,i) = Total thermal resistance for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]
	timeavg(j,i) = time averaged fluid outlet temperature for element j in flow path i

	tprofile(j,i) = steady state HTF temperature at axial position j in flow path i
	tprofile_wall(j,i) = steady state wall temperature profile at axial position j in flow path i
	=======================================================================================*/

	size_t i, j, k, pathid;
	double z, term1;



	if (tinputs.lam1.at(0, 0) == 0.0)		// No mass flow rate 
	{
		for (pathid = 0; pathid < tinputs.npath; pathid++)    // Loop over flow paths 
		{
			for (j = 0; j < tinputs.nelem; j++)						// Loop through elements on flow path
			{
				k = tinputs.startpt.at(j);
				if (j>0)
					tprofile.at(k, pathid) = tprofile.at(k - 1, pathid);	// First point of current element = last point of previous element

				for (i = 1; i < tinputs.nz.at(j); i++)						// Loop through axial positions on flow element j (except for first point)
				{
					if (tinputs.lam2.at(j, pathid) != 0)
						tprofile.at(k + i, pathid) = tinputs.cval.at(j, pathid) / tinputs.lam2.at(j, pathid);
					else					// Note: No steady state temperature exists if lam2 = 0
						tprofile.at(k + i, pathid) = 1.0e6;
				}
			}
		}
	}

	else				// lam1 != 0
	{

		for (pathid = 0; pathid < tinputs.npath; pathid++)    
		{
			tprofile.at(0, pathid) = tinputs.inlet_temp;
			for (j = 0; j < tinputs.nelem; j++)						
			{
				k = tinputs.startpt.at(j);
				if (j>0)
					tprofile.at(k, pathid) = tprofile.at(k - 1, pathid);		// First point of current element = last point of previous element

				for (i = 1; i < tinputs.nz.at(j); i++)						
				{
					z = tinputs.zpts.at(k + i);			// Local axial position on element j
					if (tinputs.lam2.at(j, pathid) != 0)
						term1 = tinputs.cval.at(j, pathid) / tinputs.lam2.at(j, pathid) * (1.0 - exp(-tinputs.lam2.at(j, pathid) / tinputs.lam1.at(j, pathid) * z));
					else
						term1 = tinputs.cval.at(j, pathid) / tinputs.lam1.at(j, pathid) * z;
					tprofile.at(k + i, pathid) = term1 + tprofile.at(k, pathid) * exp(-tinputs.lam2.at(j, pathid) / tinputs.lam1.at(j, pathid) * z);
				}
			}
		}

		// Average downcomer T profile calculated from each flow path 
		if (tinputs.npath > 1)
		{
			j = tinputs.startpt.at(tinputs.nelem - 1);  // First axial position point
			for (i = 0; i < tinputs.nz.at(tinputs.nelem - 1); i++)		
			{
				tprofile.at(j + i, 0) = 0.5*tprofile.at(j + i, 0) + 0.5*tprofile.at(j + i, 1);
				tprofile.at(j + i, 1) = tprofile.at(j + i, 0);
			}
		}
	}

	// Calculate wall temperature profile
	for (i = 0; i < m_n_lines; i++)			// Loop through flow paths
	{
		k = 0;
		for (j = 0; j < m_n_elem; j++)		// Loop through flow elements
		{
			for (size_t q = 0; q < tinputs.nz.at(j); q++)
			{
				double Tf = tprofile.at(k, i);
				double qnet = (tinputs.cval.at(j, i) - tinputs.lam2.at(j, i)*Tf) * m_tm.at(j);
				tprofile_wall.at(k, i) = Tf;
				if (m_flowelem_type.at(j, i) >= 0)			// Receiver panel
					tprofile_wall.at(k, i) += qnet / CSP::pi * tinputs.Rtube.at(j, i);
				k += 1;
			}
		}
	}
}



void C_mspt_receiver::initialize_transient_param_inputs(const s_steady_state_soln &soln, parameter_eval_inputs &pinputs)
{
	// Initialize values of transient model parameter inputs (pinputs) from steady state solution, current weather, and current time

	double P_amb = soln.p_amb;	//[Pa] Ambient pressure
	double hour = soln.hour;	//[hr] Hour of the year
	double T_dp = soln.T_dp;	//[K] Dewpoint temperature
	double T_amb = soln.T_amb;	//[K] Dry bulb temperature
	double v_wind_10 = soln.v_wind_10;
	double T_sky = CSP::skytemp(T_amb, T_dp, hour);

	double T_coolant_prop = (soln.T_salt_hot + soln.T_salt_cold_in) / 2.0;
	pinputs.mflow_tot = soln.m_dot_salt_tot;
	pinputs.c_htf = field_htfProps.Cp(T_coolant_prop)*1000.0;		// HTF specific heat at average temperature [J/kg-K] 
	pinputs.rho_htf = field_htfProps.dens(T_coolant_prop, 1.0);	// HTF density at average temperature [kg/m3]
	pinputs.mu_htf = field_htfProps.visc(T_coolant_prop);			// HTF viscosity at average temperature [kg/m/s]
	pinputs.k_htf = field_htfProps.cond(T_coolant_prop);			// HTF conductivity at average temperature [W/m/K]
	pinputs.Pr_htf = param_inputs.c_htf*pinputs.mu_htf / pinputs.k_htf;
	
	pinputs.T_amb = T_amb;
	pinputs.T_sky = T_sky;
	pinputs.wspd = v_wind_10;
	pinputs.pres = P_amb;

	// Set panel incident solar energy and fill in initial guesses for property evaluation temperatures with steady state values
	pinputs.qinc.fill(0.0);				// Solar energy incident on one tube (W)
	pinputs.qheattrace.fill(0.0);
	for (size_t i = 0; i < m_n_lines; i++)
	{
		size_t jdc = (size_t)m_n_elem - 1; 
		pinputs.Tfeval.at(0, i) = soln.T_salt_cold_in;	  //Riser
		pinputs.Tseval.at(0, i) = soln.T_salt_cold_in;
		pinputs.Tfeval.at(jdc, i) = soln.T_salt_hot;	  //Downcomer
		pinputs.Tseval.at(jdc, i) = soln.T_salt_hot;
		for (size_t j = 1; j < jdc; j++)
		{
			if (m_flowelem_type.at(j, i) >= 0)		// Receiver panel
			{
				pinputs.qinc.at(j, i) = soln.q_dot_inc.at(m_flowelem_type.at(j, i)) / double(m_n_t);
				pinputs.Tfeval.at(j, i) = soln.T_panel_ave.at(m_flowelem_type.at(j, i));
				pinputs.Tseval.at(j, i) = soln.T_s.at(m_flowelem_type.at(j, i));
			}
			if (m_flowelem_type.at(j, i) == -3)		// Crossover header
			{
				pinputs.Tfeval.at(j, i) = pinputs.Tfeval.at(j - 1, i);
				pinputs.Tseval.at(j, i) = pinputs.Tfeval.at(j, i);
			}
		}
	}
}

void C_mspt_receiver::update_pde_parameters(bool use_initial_t, parameter_eval_inputs &pinputs, transient_inputs &tinputs)
{

	//Update PDE parameters and tube wall thermal resistance based on property evaluation temperatures: pinputs.Tfeval and pinputs.Tseval

	/*=============================================================================
	
	use_initial_t = use the temperature profile specified in tinputs.tinit and tinputs.tinit_wall to evaluate pde parameters, overwriting any current inputs in pinputs.Tfeval and pinputs.Tsevel

	pinputs.
	mflow_tot = total HTF mass flow through the receiver (kg/s)
	T_amb = ambient temperature (K)
	T_sky = sky temperature (K)
	c_htf = HTF specific heat evaluated at the avg. of the current inlet and the design point outlet temperatures (J/kg/K)
	rho_htf = HTF density (kg/m3)
	mu_htf = HTF viscosity (kg/m/s)
	k_htf = HTF thermal conductivity (W/m/K)
	Pr_htf = HTF Prandtl number
	finitial = fraction of full flux at start of period
	ffinal = fraction of full flux at end of period
	ramptime = time period during which the flux ramp is applied [s]

	tm(j) = thermal mass of element j (assumed to be the same in each flow path) [J/m/K]
	qinc(j,i) = incident solar energy per tube for element j in path i [W]
	Tfeval(j,i) = fluid temperature for element j in flow path i [K]
	Tseval(j,i) = solid temperature for element j in flow path i [K]

	tinputs.
	npath = number of flow paths
	nelem = total number of flow elements in one flow path (riser and downcomer are included in each flow path)
	nztot = total number of axial evaluation points in one flow path
	length(j) = total length of flow element j (assumed to be the same in each flow path) [m]
	nz(j) = number of axial points for element j (assumed to be the same in each flow path)
	zpts(j) = position of axial point j (0 at inlet of each flow element) [m]
	startpt(j) = index of first point in element j (assumed to be the same in each flow path)
	lam1(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C
	lam2(j,i) = PDE parameter for element j in flow path i  --> dT/dt + lam1*dT/dz + lam2*T = C
	cval(j,i) = PDE parameter for element j in flow path i --> dT/dt + lam1*dT/dz + lam2*T = C
	Rtube = tube thermal resistance for element j in flow path i
	tinit(j,i) = fluid temperature at axial point j in flow path i [K]

	================================================================================*/

	double mmult, Reelem, Nuelem, felem, hinner, k_tube, Rwall, Rconv;
	double Pr_htf = pinputs.c_htf*pinputs.mu_htf / pinputs.k_htf;
	size_t i, j;

	tinputs.lam1.fill(0.0);
	tinputs.lam2.fill(0.0);
	tinputs.cval.fill(0.0);
	tinputs.aval.fill(0.0);
	tinputs.Rtube.fill(0.0);

	for (i = 0; i < m_n_lines; i++)
	{
		for (j = 0; j < m_n_elem; j++)			// Flow path elements in flow order
		{
			if (use_initial_t)   // Overwrite Tfeval and Tseval using values in tinputs.tinit at element midpoint
			{
				int k = tinputs.startpt.at(j) + (int)floor(tinputs.nz.at(j) / 2);
				pinputs.Tfeval.at(j, i) = tinputs.tinit.at(k, i);	// Initial temperature at midpoint of element j
				pinputs.Tseval.at(j, i) = tinputs.tinit_wall.at(k, i);	// Initial wall temperature at midpoint of element j
			}

			k_tube = tube_material.cond((pinputs.Tseval.at(j, i) + pinputs.Tfeval.at(j, i)) / 2.0);		//[W/m-K] Thermal conductivity of the tube wall
			Rwall = log(m_od.at(j) / m_id.at(j)) / k_tube;  
			Rconv = 0.0;

			if (pinputs.mflow_tot > 0.0)
			{
				mmult = 1.0 / (double)m_n_lines / (double)m_n_t;		// Flow rate multiplier for individual tube
				if (m_flowelem_type.at(j, i) == -1 || m_flowelem_type.at(j, i) == -2)	// Riser or downcomer
					mmult = 1.0;
				if (m_flowelem_type.at(j, i) == -3)		// Crossover header
					mmult = 1.0 / (double)m_n_lines;

				Reelem = 4 * mmult*pinputs.mflow_tot / (CSP::pi * m_id.at(j)*pinputs.mu_htf);			// Flow Reynolds number in element j
				CSP::PipeFlow(Reelem, Pr_htf, tinputs.length.at(j) / m_id.at(j), (4.5e-5) / m_id.at(j), Nuelem, felem);
				hinner = Nuelem*pinputs.k_htf / m_id.at(j);										// Tube internal heat transfer coefficient [W/m2/K]
				tinputs.lam1.at(j, i) = mmult*pinputs.mflow_tot*pinputs.c_htf / pinputs.tm.at(j);
				Rconv = 1.0 / (0.5*hinner* m_id.at(j));  // Convective resistance between fluid and internal tube wall
			}

			if (m_flowelem_type.at(j, i) >= 0)				// Receiver panel
			{
				double sa = CSP::pi*0.5*m_od.at(j);			// Front tube surface area per m (m2/m)
				double vf = (2.0 / CSP::pi);				// View factor from tube front surface area to ambient
				double hmix = calc_external_convection_coeff(pinputs.T_amb, pinputs.pres, pinputs.wspd, pinputs.Tseval.at(j, i)) * (2.0 / CSP::pi);  // External convection coeff (2/pi scales for loss from tube front SA)
				double Tlin = pinputs.Tseval.at(j, i);															// Linearization temperature for radiative loss [K]
				double heff =  m_hl_ffact * (0.5*hmix + 4.0*vf*CSP::sigma*m_epsilon*pow(Tlin, 3));				// Effective heat transfer coefficient [W/m2/K]
				double qabstube = pinputs.qinc.at(j, i) / (sa*tinputs.length.at(j));							// Solar flux absorbed by one tube [W/m2 tube front SA] 
				if (pinputs.mflow_tot > 0.0)
					tinputs.Rtube.at(j, i) = Rwall + Rconv; // Total thermal resistance between fluid and front external tube wall	

				double den = (1.0 + 0.5*m_od.at(j)*tinputs.Rtube.at(j, i)*heff) * pinputs.tm.at(j);
				tinputs.lam2.at(j, i) = sa*heff / den;
				tinputs.cval.at(j, i) = sa * (pinputs.finitial*qabstube + 0.5*hmix*m_hl_ffact*pinputs.T_amb + vf*m_hl_ffact*m_epsilon*CSP::sigma*(3.0*pow(Tlin, 4) + 0.5*pow(pinputs.T_amb, 4) + 0.5*pow(pinputs.T_sky, 4))) / den;
				if (pinputs.ramptime > 0)
				{
					double ramp_rate = qabstube * (pinputs.ffinal - pinputs.finitial) / pinputs.ramptime;  // Flux ramp rate (W/m2/s)
					tinputs.aval.at(j, i) = sa*ramp_rate / den;
				}
			}

			if (m_flowelem_type.at(j, i) == -1 || m_flowelem_type.at(j, i) == -2)		// Riser or downcomer
			{
				double Rtot;
				if (m_flowelem_type.at(j, i) == -1)
					Rtot = m_Rtot_riser;   // Thermal resistance between fluid and external tube wall (assumed constant from wetted loss coefficient)
				else
					Rtot = m_Rtot_downc;
				tinputs.lam2.at(j, i) = 2.0*CSP::pi / Rtot / pinputs.tm.at(j);
				tinputs.cval.at(j, i) = (pinputs.qheattrace.at(j) + 2.0*CSP::pi * pinputs.T_amb / Rtot) / pinputs.tm.at(j);
			}

			if (m_flowelem_type.at(j, i) == -3)  // Crossover header
			{
				tinputs.lam2.at(j, i) = 0.0;
				tinputs.cval.at(j, i) = pinputs.qheattrace.at(j) / pinputs.tm.at(j);
			}
		}
	}

}

void C_mspt_receiver::solve_transient_model(double tstep,
	double allowable_Trise,
	parameter_eval_inputs &pinputs,
	transient_inputs &tinputs,
	transient_outputs &toutputs)
{
	/* ===============================================================
	Solve transient receiver model:
	1) Set property evaluation temperatures to values at initial condition
	2) Calculate time averaged outlet temperature from each flow element
	3) Update linearization and property evaluation temperatures based on time-averaged flow element temperature
	4) Update PDE coefficients
	5) Repeat 1-3 until time-averaged temperatures converge
	6) Calculate the maximum temperature change during the time step
	7) Subdivide time step if maximum temperature changes exceeds the specified limiting value and repeat 1-6

	tstep = full time step (s)
	allowable_Trise = maximum allowable change in temperature during the sub-divided time step (for accuracy of linearized radiative losses and analytical formulation)

	pinputs = physical properties and conditions required to evaluate PDE parameters
	tinputs = transient model input parameters
	toutputs = transient model output parameters

	================================================================================*/


	toutputs.timeavg_tout = 0.0;			// Time-averaged downcomer outlet T [K]
	toutputs.tout = 0.0;					// Downcomer outlet T at the end of the time step [K]
	toutputs.max_tout = 0.0;				// Max downcomer outlet T during the time step [K]
	toutputs.min_tout = 5000.0;				// Min downcomer outlet T during the time step [K]
	toutputs.max_rec_tout = 0.0;			// Max receiver outlet T during the time step [K]
	toutputs.timeavg_conv_loss = 0.0;		// Time-averaged convection loss from the receiver panels [W]
	toutputs.timeavg_rad_loss = 0.0;		// Time-averaged radiation loss
	toutputs.timeavg_piping_loss = 0.0;		// Time-averaged thermal loss from piping [W]
	toutputs.timeavg_qthermal = 0.0;		// Average thermal power sent to power cycle or storage during the time step [W]
	toutputs.timeavg_qnet = 0.0;			// Average net thermal power absorbed by the receiver during the time step [W]
	toutputs.timeavg_qheattrace = 0.0;		// Average heat trace thermal input during the time step [W]
	toutputs.t_profile.fill(0.0);			// Axial temperature profile at the end of the time step
	toutputs.t_profile_wall.fill(0.0);		// Axial temperature profile at the end of the time step
	toutputs.timeavg_temp.fill(0.0);		// Time-averaged exit temperature from each flow element [K]
	toutputs.time_min_tout = 0.0;			// Time (relative to beginning of time step) at which minimum downcomer outlet T occurs (s)
	toutputs.tube_temp_inlet = 0.0;
	toutputs.tube_temp_outlet = 0.0;


	double max_Trise;
	double allowable_min_step = 60.0;		// Minimum allowable time step (s) for solving transient model (transient model time step is only reduced if allowable_Trise is exceeded)
	double transmodel_step = tstep;			// Initial time step for solution of transient model (s)
	double solved_time = 0.0;
	int qsub = 0;							// Iterations to adjust intermediate transient model time steps
	int qmax = 50;							// Max iterations for adjustment of PDE parameters based on iterative solution of time-averaged tempeatures
	util::matrix_t<double>tinit_start = tinputs.tinit;			// Save initial condition at start of full time step 

	// Update PDE parameters using initial temperature solution
	update_pde_parameters(true, pinputs, tinputs);	

	// Solve transient model
	while (solved_time < tstep)	       // Iterations for subdivision of full time step if temperature changes are too large for accuracy of the linearized approximation for radiative loss
	{
		max_Trise = 0.0;

		// Calculate time-averaged temperature and iterate to adjust linearization temperature and properties
		double maxTdiff = 1000.0;
		double Tconverge = 10.0;		// Convergence criteria (K) for change in property evaluation and linearization temperatures between iterations
		double panel_loss_sum, piping_loss_sum, rad_loss_sum, conv_loss_sum, qnet_sum, qheattrace_sum;
		panel_loss_sum = piping_loss_sum = rad_loss_sum = conv_loss_sum = qnet_sum = qheattrace_sum = 0.0;
		int q = 0;
		while (maxTdiff > Tconverge && q < qmax)
		{
			maxTdiff = panel_loss_sum = piping_loss_sum = rad_loss_sum = conv_loss_sum = qnet_sum = qheattrace_sum = 0.0;

			// Update PDE parameters with specified Tfeval and Tseval	
			if (q>0)
				update_pde_parameters(false, pinputs, tinputs);			


			// Calculate time-averaged outlet temperatures
			for (size_t i = 0; i < m_n_lines; i++)
			{
				for (size_t j = 0; j < m_n_elem; j++)
					toutputs.timeavg_temp.at(j, i) = calc_timeavg_exit_temp(transmodel_step, j, i, tinputs);
			}
			if (m_n_lines > 1)  // Average values for downcomer over flow paths
			{
				size_t j = (size_t)m_n_elem - 1;
				toutputs.timeavg_temp.at(j, 0) = 0.5*(toutputs.timeavg_temp.at(j, 0) + toutputs.timeavg_temp.at(j, 1));
				toutputs.timeavg_temp.at(j, 1) = toutputs.timeavg_temp.at(j, 0);
			}


			// Calculate time-averaged wall temperatures, convection / radiation losses, heat-trace thermal input
			util::matrix_t<double> Tfavg, Tsavg;
			Tfavg.resize_fill(m_n_elem, m_n_lines, 0.0);
			Tsavg.resize_fill(m_n_elem, m_n_lines, 0.0);
			for (size_t i = 0; i < m_n_lines; i++)
			{
				for (size_t j = 0; j < m_n_elem; j++)
				{
					// Calculate average element temperature
					Tfavg.at(j, i) = toutputs.timeavg_temp.at(j, i);  // Set avg fluid T to time-averaged outlet value
					if (pinputs.mflow_tot > 0.0)
					{
						if (j > 0)
							Tfavg.at(j, i) += toutputs.timeavg_temp.at(j - 1, i); // Inlet 
						else
							Tfavg.at(j, i) += tinputs.inlet_temp;
						
						Tfavg.at(j, i) = Tfavg.at(j, i) / 2.0;
					}
					Tsavg.at(j, i) = Tfavg.at(j, i); 

					// Receiver panels
					if (m_flowelem_type.at(j, i) >= 0)			
					{
						double sa = 0.5*CSP::pi*m_od.at(j)*tinputs.length.at(j);  // Front surface area per tube (m2)
						double qabs_avg = 0.5*(pinputs.finitial + pinputs.ffinal) * (pinputs.qinc.at(j, i) / sa);  // Average absorbed flux per tube [W/m2] front tube surface area
						double hext, qconv, qrad;
						double Tsguess = Tfavg.at(j, i);
						if (pinputs.mflow_tot > 0.0)
						{ 
							Tsguess = Tfavg.at(j, i) + ((tinputs.cval.at(j, i) - tinputs.lam2.at(j, i)*Tfavg.at(j, i)) * m_tm.at(j)) / CSP::pi * tinputs.Rtube.at(j, i);
							calc_surface_temperature(Tfavg.at(j, i), qabs_avg, tinputs.Rtube.at(j, i), m_od.at(j), pinputs.T_amb, pinputs.T_sky, pinputs.pres, pinputs.wspd, Tsguess);
						}
						calc_thermal_loss(Tsguess, pinputs.T_amb, pinputs.T_sky, pinputs.pres, pinputs.wspd, hext, qconv, qrad);

						// Total radiative and convective loss [W]
						rad_loss_sum += qrad * sa * m_n_t;	
						conv_loss_sum += qconv * sa * m_n_t;
						panel_loss_sum += (qconv + qrad) * sa * m_n_t;
						qnet_sum += (qabs_avg - qrad - qconv) * sa * m_n_t;  
						Tsavg.at(j,i) = Tsguess;  
					}

					// Cross-over header
					if (m_flowelem_type.at(j, i) == -3)
					{
						qheattrace_sum += pinputs.qheattrace.at(j) * tinputs.length.at(j);
					}

					// Riser
					if (m_flowelem_type.at(j, i) == -1 && i == 0)
					{
						double heatloss = (2.0*CSP::pi * (Tfavg.at(j, i) - pinputs.T_amb) / m_Rtot_riser) * tinputs.length.at(j); //[W]
						piping_loss_sum += heatloss;
						qnet_sum -= heatloss;
						qheattrace_sum += pinputs.qheattrace.at(j) * tinputs.length.at(j);
					}

					// Downcomer
					if (m_flowelem_type.at(j, i) == -2 && i == 0)
					{
						double heatloss = (2.0*CSP::pi * (Tfavg.at(j, i) - pinputs.T_amb) / m_Rtot_downc) * tinputs.length.at(j); //[W]
						piping_loss_sum += heatloss;
						qnet_sum -= heatloss;
						qheattrace_sum += pinputs.qheattrace.at(j) * tinputs.length.at(j); // [W]
					}

				}

			}

			// Update property evaluation and linearization temperatures
			for (size_t i = 0; i < m_n_lines; i++)
			{
				for (size_t j = 0; j < m_n_elem; j++)
				{
					maxTdiff = fmax(maxTdiff, fmax(fabs(Tsavg.at(j, i) - pinputs.Tseval.at(j, i)), fabs(Tfavg.at(j, i) - pinputs.Tfeval.at(j, i))));
				}
			}
			pinputs.Tfeval = Tfavg;
			pinputs.Tseval = Tsavg;
			q++;
		}

		// Calculate axial profile at end of time step
		calc_axial_profile(transmodel_step, tinputs, toutputs.t_profile);		// Calculate full axial temperature profile at the end of the time step

		// Estimate the maximum temperature variation during the time step 
		for (size_t i = 0; i < m_n_lines; i++)
		{
			for (size_t j = 0; j < m_nz_tot; j++)
				max_Trise = fmax(max_Trise, fabs(toutputs.t_profile.at(j, i) - tinputs.tinit.at(j, i)));		// Difference between final and initial temperature at axial position j
		}
		util::matrix_t<double> textreme_d, tpt_d, textreme_r, tpt_r;
		calc_extreme_outlet_values(transmodel_step, m_n_elem - 1, tinputs, textreme_d, tpt_d);   //Extreme downcomer outlet T
		calc_extreme_outlet_values(transmodel_step, m_n_elem - 2, tinputs, textreme_r, tpt_r);   //Extreme receiver outlet T
		max_Trise = fmax(max_Trise, fmax(textreme_d.at(1, 0) - textreme_d.at(0, 0), textreme_r.at(1, 0) - textreme_r.at(0, 0)));
		if (m_n_lines >1)
			max_Trise = fmax(max_Trise, textreme_r.at(1, 1) - textreme_r.at(0, 1));

		// Calculate output values or subdivide time step
		if (max_Trise < allowable_Trise || transmodel_step <= allowable_min_step)				// Maximum temperature variation is acceptable or the time step is the minimum allowable value
		{
			toutputs.timeavg_qnet = toutputs.timeavg_qnet + qnet_sum*(transmodel_step / tstep);
			toutputs.timeavg_qheattrace = toutputs.timeavg_qheattrace + qheattrace_sum * (transmodel_step / tstep);
			toutputs.timeavg_rad_loss = toutputs.timeavg_rad_loss + rad_loss_sum*(transmodel_step / tstep);
			toutputs.timeavg_conv_loss = toutputs.timeavg_conv_loss + conv_loss_sum*(transmodel_step / tstep);
			toutputs.timeavg_piping_loss = toutputs.timeavg_piping_loss + piping_loss_sum*(transmodel_step / tstep);
			toutputs.timeavg_tout = toutputs.timeavg_tout + toutputs.timeavg_temp.at((size_t)m_n_elem - 1, 0)*(transmodel_step / tstep);
			
			toutputs.max_tout = fmax(toutputs.max_tout, textreme_d.at(1, 0));
			if (textreme_d.at(0, 0) < toutputs.min_tout)
			{
				toutputs.min_tout = textreme_d.at(1, 0);
				toutputs.time_min_tout = solved_time + tpt_d.at(1, 0);	// Time relative to beginning of full time step
			}
			toutputs.max_rec_tout = fmax(toutputs.max_rec_tout, textreme_r.at(1, 0));
			if (m_n_lines > 1)
				toutputs.max_rec_tout = fmax(toutputs.max_rec_tout, textreme_r.at(1, 1));
			

			solved_time = solved_time + transmodel_step;
			if (tstep - solved_time > 0.01)					// Full time step not finished
			{
				transmodel_step = tstep - solved_time;		// Set transient model time step = remaining fraction of the full model time step
				tinputs.tinit = toutputs.t_profile;			// Set new initial temperature profile to profile at the end of the last successful transient model time step
			}
		}
		else		// Maximum temperature variation over the transient model time step is not acceptable --> Decrease transient model time step and try again
			transmodel_step = transmodel_step / 2.0;
		qsub++;
	}
	toutputs.tout = toutputs.t_profile.at((size_t)m_nz_tot - 1, 0);															// Downcomer outlet T at the end of the time step
	toutputs.timeavg_qthermal = pinputs.mflow_tot * pinputs.c_htf * (toutputs.timeavg_tout - tinputs.inlet_temp);	// Time-averaged thermal power leaving the receiver during the time step [W]	
	tinputs.tinit = tinit_start;		// Revert initial temperature profile back to profile at the start of the full time step (in case the model is called more than once during this time step)


	// Calculate receiver wall temperature profile at the end of the time step
	size_t krecout = size_t(tinputs.startpt.back()) - 1;  // Index of receiver outlet
	for (size_t i = 0; i < m_n_lines; i++)			// Loop through flow paths
	{
		size_t k = 0;
		for (size_t j = 0; j < m_n_elem; j++)		// Loop through flow elements
		{
			for (size_t q = 0; q < tinputs.nz.at(j); q++)
			{
				double Tf = toutputs.t_profile.at(k, i);
				double qnet = (tinputs.cval.at(j, i) + tinputs.aval.at(j,i)*tstep - tinputs.lam2.at(j, i)*Tf) * m_tm.at(j);
				toutputs.t_profile_wall.at(k, i) = Tf;
				if (m_flowelem_type.at(j, i) >= 0)			// Receiver panel
					toutputs.t_profile_wall.at(k, i) += qnet / CSP::pi * tinputs.Rtube.at(j, i);
				k += 1;
			}
		}
		toutputs.tube_temp_inlet += toutputs.t_profile_wall.at(tinputs.startpt.at(1), i) / double(m_n_lines);
		toutputs.tube_temp_outlet += toutputs.t_profile_wall.at(krecout, i) / double(m_n_lines);
	}
}


void C_mspt_receiver::solve_transient_startup_model(parameter_eval_inputs &pinputs,
	transient_inputs &tinputs,
	int startup_mode,
	double target_temperature,
	double min_time,
	double max_time,
	transient_outputs &toutputs,
	double &startup_time, 
	double &energy,
	double &parasitic)
{

	switch (startup_mode)
	{
	case HEAT_TRACE:
	{
		double heat_trace_time = 0.0;
		double heat_trace_energy = 0.0;

		// Calculate time for all heated elements to reach the target temperature
		double max_time_required = 0.0;
		std::vector<int> elems = { 0, m_n_elem - 1 };
		std::vector<double> time = { 0, 0 };
		if (m_flow_type == 1 || m_flow_type == 2)
		{
			elems.push_back(m_crossover_index);
			time.push_back(0);
		}

		pinputs.qheattrace.fill(m_heat_trace_power);  
		update_pde_parameters(true, pinputs, tinputs);
		for (size_t k = 0; k < elems.size(); k++)
		{
			int j = elems.at(k);
			double T0 = tinputs.tinit.at(tinputs.startpt.at(j), 0);  // Initial temperature
			if (tinputs.lam2.at(j, 0) == 0.0)	
				time.at(k) = (target_temperature - T0) / tinputs.cval.at(j, 0);
			else
				time.at(k) = 1.0 / tinputs.lam2.at(j, 0) * log((T0 - tinputs.cval.at(j, 0) / tinputs.lam2.at(j, 0)) / (target_temperature - tinputs.cval.at(j, 0) / tinputs.lam2.at(j, 0)));
			time.at(k) = fmax(0.0, time.at(k));
			time.at(k) = fmin(max_time, time.at(k));
			max_time_required = fmax(max_time_required, time.at(k));
		}

		// Adjust heat trace power and solve transient model
		set_heattrace_power(false, target_temperature, max_time_required, pinputs, tinputs); // Adjust heat trace power so that all elements reach the target temperature at the same time
		solve_transient_model(max_time_required, 150.0, pinputs, tinputs, toutputs);
		energy = toutputs.timeavg_qnet * max_time_required;			  // Total net energy added to receiver subsystem [J]
		parasitic = toutputs.timeavg_qheattrace * max_time_required; // Heat trace parasitic power [W]
		startup_time = max_time_required;
	}
	break;


	case PREHEAT:
	{
		double preheat_time = 0.0;
		double preheat_energy = 0.0;

		// Recalculate receiver min wall T after heat trace
		double Tmin_rec = 5000.0;
		for (size_t i = 0; i < m_n_lines; i++)
		{
			for (size_t j = 0; j < m_n_elem; j++)
			{
				size_t p1 = tinputs.startpt.at(j);
				if (m_flowelem_type.at(j, i) >= 0)
				{
					double Twall_min = fmin(tinputs.tinit.at(p1, i), tinputs.tinit.at(p1 + tinputs.nz.at(j) - 1, i));
					Tmin_rec = fmin(Tmin_rec, Twall_min);
					pinputs.Tseval.at(j, i) = 0.5*(tinputs.tinit.at(p1, i) + target_temperature);
				}
				else
					pinputs.Tseval.at(j, i) = tinputs.tinit.at(p1, i);
			}
		}
		pinputs.Tfeval = pinputs.Tseval;
		
		// Calculate required preheating time
		update_pde_parameters(false, pinputs, tinputs);	
		if (tinputs.lam2.at(1, 0) == 0.0)		// No heat losses
			preheat_time = (target_temperature - Tmin_rec) / tinputs.cval.at(1, 0);
		else
		{
			double preheat_ss_temp = tinputs.cval.at(1, 0) / tinputs.lam2.at(1, 0);		// Steady state temperature during preheating
			if (preheat_ss_temp > target_temperature)										
				preheat_time = 1.0 / tinputs.lam2.at(1, 0) * log((Tmin_rec - tinputs.cval.at(1, 0) / tinputs.lam2.at(1, 0)) / (target_temperature - tinputs.cval.at(1, 0) / tinputs.lam2.at(1, 0)));
			else				// Steady state temperature is below preheat target
				preheat_time = max_time;
		}
		preheat_time = fmin(preheat_time, max_time);

		// Solve transient model
		set_heattrace_power(true, 0.0, 0.0, pinputs, tinputs); // Set power to heat-trace elements to keep riser/downcomer/cross-over header at constant T during preheat
		solve_transient_model(preheat_time, 150.0, pinputs, tinputs, toutputs);  
		energy = toutputs.timeavg_qnet * preheat_time;
		parasitic = toutputs.timeavg_qheattrace * preheat_time;
		startup_time = preheat_time;

	}
	break;

	case PREHEAT_HOLD:
	{		
		
		double hext, qconv, qrad;
		double Ts = tinputs.tinit.at(tinputs.startpt.at(1), 0);  // All panels are at same temperature
		calc_thermal_loss(Ts, pinputs.T_amb, pinputs.T_sky, pinputs.pres, pinputs.wspd, hext, qconv, qrad);
		double sa = 0.5*CSP::pi*m_od.at(1)*tinputs.length.at(1);  // Front surface area per tube (m2)
		pinputs.qinc.fill((qconv + qrad)*sa);  // Incident flux per tube [W] needed to maintain current temperature
		update_pde_parameters(false, pinputs, tinputs);
		set_heattrace_power(true, 0.0, 0.0, pinputs, tinputs);   // Set power to heat-trace elements to keep riser/downcomer/cross-over header at constant T 
		solve_transient_model(max_time, 150.0, pinputs, tinputs, toutputs);  // Solve for temperature profiles
		energy = toutputs.timeavg_qnet * max_time;   // Energy [J] used for startup during this step
		parasitic = toutputs.timeavg_qheattrace * max_time;
		startup_time = max_time;
	}
	break;

	case FILL:  
	{	
		double hext, qconv, qrad;
		double Ts = tinputs.tinit.at(tinputs.startpt.at(1), 0);  // All panels are at same temperature
		calc_thermal_loss(Ts, pinputs.T_amb, pinputs.T_sky, pinputs.pres, pinputs.wspd, hext, qconv, qrad);
		double sa = 0.5*CSP::pi*m_od.at(1)*tinputs.length.at(1);  // Front surface area per tube (m2)
		pinputs.qinc.fill((qconv + qrad)*sa);  // Incident flux per tube [W] needed to maintain current temperature
		update_pde_parameters(false, pinputs, tinputs);
		set_heattrace_power(true, 0.0, 0.0, pinputs, tinputs);  // Set power to heat-trace elements to keep riser/downcomer/cross-over header at constant T 
		solve_transient_model(max_time, 150.0, pinputs, tinputs, toutputs);  // Solve for temperature profiles
		energy = toutputs.timeavg_qnet * max_time;   // Energy [J] used for startup during this step
		parasitic = toutputs.timeavg_qheattrace * max_time;
		startup_time = max_time;
	}
	break;

	case CIRCULATE:
	{
		double circulate_time = 0.0;
		double circulate_energy = 0.0;
		double max_Trise = 150.0;
		double total_time = 0.0;

		if (pinputs.ramptime >= max_time)    // Flux ramp is applied during the full time period remaining in the time step -> Startup won't be completed in this time step
		{
			solve_transient_model(max_time, max_Trise, pinputs, tinputs, toutputs);		
			circulate_energy += toutputs.timeavg_qnet * max_time; 
			energy = circulate_energy;
			parasitic = 0.0;
			startup_time = max_time;
		}

		else
		{
			if (pinputs.finitial < pinputs.ffinal)  // Flux ramp is applied for only part of the time step
			{
				solve_transient_model(pinputs.ramptime, max_Trise, pinputs, tinputs, toutputs);		// Solve transient model during portion of time step with flux ramp
				circulate_energy += toutputs.timeavg_qnet * pinputs.ramptime; // Energy [J] used for startup during the flux ramp
				total_time = pinputs.ramptime;
				trans_inputs.tinit = trans_outputs.t_profile;
				trans_inputs.tinit_wall = trans_outputs.t_profile_wall;
				max_time -= pinputs.ramptime;
				pinputs.finitial = 1.0;
				pinputs.ffinal = 1.0;
				pinputs.ramptime = 0.0;
			}

			// Flux ramp is finished -> find time at which outlet temperature reaches target value
			double lowerbound = 0.0;
			if (min_time > 0.01)
				lowerbound = fmin(min_time, max_time);		// Redefine lower bound if previously defined from "Hold" mode			

			if (tinputs.tinit.at((size_t)m_nz_tot - 1, 0) >= target_temperature && lowerbound == 0.0) // Outlet temperature is already above target
				circulate_time = fmin(1.0, max_time);
			else
			{
				double upperbound = fmax(max_time, lowerbound);
				circulate_time = upperbound;
				solve_transient_model(circulate_time, max_Trise, pinputs, tinputs, toutputs);

				if (toutputs.tout < target_temperature)	// Target outlet temperature not achieved within the current time step
					circulate_time = max_time;
				else
				{
					if (toutputs.min_tout < target_temperature)		// Outlet temperature drops below the target during the time step --> set lower bound to time when minimum outlet T occurs
						lowerbound = fmax(lowerbound, toutputs.time_min_tout);

					// Estimate expected time to reach steady state
					update_pde_parameters(true, pinputs, tinputs);
					std::vector<double> est_time_ss(m_n_elem, 0.0);
					double time_ss;
					for (size_t i = 0; i < m_n_lines; i++)
					{
						std::vector<double> est_time_ss_path(m_n_elem, 0.0);
						for (size_t j = 0; j < m_n_elem; j++)
						{
							est_time_ss_path.at(j) = tinputs.length.at(j) / tinputs.lam1.at(j, i) + ((j == 0) ? 0 : est_time_ss_path.at(j - 1));
							est_time_ss.at(j) = fmax(est_time_ss.at(j), est_time_ss_path.at(j));
						}

					}

					// Find time at which outlet temperature reaches target value 
					if (tinputs.tinit.at(0, 0) - tinputs.inlet_temp < m_startup_target_delta) // Can't get to target T until inlet T step change reaches downcomer outlet
						circulate_time = est_time_ss.back();
					else
					{
						double t, tprev, tsolve, f, fprev, fsolve;
						t = tprev = f = fprev = std::numeric_limits<double>::quiet_NaN();
						double ttol = 0.05;  // Need < 0.1s for interaction with solver
						double ftol = 0.01;
						int q = 0;
						while (q < 100)
						{
							if (q == 0 && toutputs.tout - target_temperature < 0.5) // Solution at max time step is close (controller iterations for timestep use +0.05s buffer)
							{
								t = upperbound;
								f = toutputs.tout - target_temperature;
								tsolve = t - 0.1;
							}
							else if (q == 0)  // No good first guess, use time required for receiver outlet (before downcomer) to reach steady state
								tsolve = est_time_ss.at((size_t)m_n_elem - 2);
							else if (fprev != fprev)
								tsolve = fsolve > 0 ? est_time_ss.at((size_t)m_n_elem - 3) : tsolve + 10;
							else if (f < 0 && fprev < 0 && fabs(f - fprev) < 0.1)  // Below target, but converging slowly
								tsolve = fabs(f) < 0.01 ? tsolve + 0.05 : tsolve + 0.5;
							else if (f > 0 && fprev > 0 && fabs(f - fprev) < 0.2) // Above target, likely near steady state solution
								tsolve = 0.5 * (upperbound + lowerbound);
							else
								tsolve = t - f * (t - tprev) / (f - fprev) + 0.001; // Add small increase to end up on high side of target when nearly converged

							if (tsolve <= lowerbound || tsolve >= upperbound)
								tsolve = 0.5 * (upperbound + lowerbound);

							solve_transient_model(tsolve, max_Trise, pinputs, tinputs, toutputs);
							fsolve = toutputs.tout - target_temperature;
							if (fsolve < 0.0)
								lowerbound = tsolve;
							else
								upperbound = tsolve;

							if (fsolve > 0.0 && (fsolve < ftol || (upperbound - lowerbound) < ttol))
								break;

							tprev = t;
							fprev = f;
							t = tsolve;
							f = fsolve;
							q++;
						}
						circulate_time = tsolve;
					}

					if (circulate_time == max_time)
						circulate_time -= 0.1;   // Target temperature achieved, report time < max_time to move to next stage
				}
			}

			if (circulate_time > 0.0)
				circulate_energy += toutputs.timeavg_qnet * circulate_time; // Energy [J] used for startup during the time step
			total_time += circulate_time;
			energy = circulate_energy;
			parasitic = 0.0;
			startup_time = total_time;
		}
	}
	break;

	case HOLD:
	{
		solve_transient_model(max_time, 150.0, pinputs, tinputs, toutputs);  // Solve for temperature profiles
		energy = toutputs.timeavg_qnet * max_time;   // Energy [J] used for startup during this step
		parasitic = 0.0;
		startup_time = max_time;
	}
	break;

	}

}


void C_mspt_receiver::set_heattrace_power(bool is_maintain_T, double Ttarget, double time, parameter_eval_inputs &pinputs, transient_inputs &tinputs)
{
	// Set power to heat-trace for each heated element (riser, downcomer, cross-over header) to reach the target temperature at the time, or to maintain the current temperature
	pinputs.qheattrace.fill(0.0);

	std::vector<int> elems = { 0, m_n_elem - 1 };
	if (m_flow_type == 1 || m_flow_type == 2)
		elems.push_back(m_crossover_index);
	
	for (int k = 0; k < elems.size(); k++)
	{
		int j = elems.at(k);
		double T0 = tinputs.tinit.at(tinputs.startpt.at(j), 0);  

		if (is_maintain_T)  // Set heat trace power to maintain the current temperature
		{
			if (tinputs.lam2.at(j, 0) > 0.0)
				pinputs.qheattrace.at(j) = (tinputs.lam2.at(j, 0)*pinputs.tm.at(j))*(T0 - pinputs.T_amb);
		}
		else  // Set heat trace power to reach the target temperature at a given time
		{
			if (tinputs.lam2.at(j, 0) == 0.0)
				pinputs.qheattrace.at(j) = (pinputs.tm.at(j) / time) * (Ttarget - T0);
			else
				pinputs.qheattrace.at(j) = (tinputs.lam2.at(j, 0)*pinputs.tm.at(j)) * ((Ttarget - pinputs.T_amb) - (T0 - pinputs.T_amb)*exp(-tinputs.lam2.at(j, 0)*time)) / (1. - exp(-tinputs.lam2.at(j, 0)*time));
		}
		pinputs.qheattrace.at(j) = fmax(pinputs.qheattrace.at(j), 0.0);
	}

	return;
}


void C_mspt_receiver::est_startup_time_energy(double fract, double &est_time, double &est_energy)
{
	// fract = Fraction of design point thermal power 

	double start_time = 0.0;
	double start_energy = 0.0;

	double hext = 10.;
	double Tamb = 20. + 273.15;

	// Heat tracing (without losses)
	double time_heattrace = (m_T_htf_cold_des - Tamb) * m_tm_solid.at(0) / m_heat_trace_power;
	start_time += time_heattrace;

	//Preheating 	
	double Tavg = 0.5*(m_T_htf_cold_des + Tamb);
	double qpreheat = (m_od_tube*m_tube_flux_preheat*1000.0);    // Solar power absorbed (W/m) during preheat
	double qloss = (0.5*CSP::pi*m_od_tube) * (hext * (Tavg - Tamb) + (2.0/CSP::pi)*CSP::sigma*m_epsilon*(pow(Tavg, 4) - pow(Tamb, 4)));
	double time_preheat = (m_preheat_target - Tamb)*m_tm_solid.at(1) / (qpreheat - qloss);
	double energy_preheat = time_preheat * ((qpreheat - qloss)*m_h_rec*m_n_t*m_n_panels)* 1.e-6 / 3600.0;  //Energy needed to reach target T (MWht)
	time_preheat = fmax(time_preheat, m_min_preheat_time);
	start_time += time_preheat;
	start_energy += energy_preheat;

	// Fill
	start_time += m_fill_time;

	// Circulation

	C_csp_weatherreader::S_outputs weather;
	weather.m_pres = 101325./100; //mbar
	weather.m_tdew = 2.0; //C
	weather.m_tdry = Tamb-273.15; //C
	weather.m_wspd = 5.0; //m/s

	s_steady_state_soln soln;
	soln.hour = 182.0 * 24.0 + 8.0;
	soln.T_amb = Tamb;
	soln.T_dp = 2.0+273.15;
	soln.v_wind_10 = 5.0;
	soln.p_amb = 101325.;
	soln.T_salt_cold_in = m_T_htf_cold_des;
	double qinc_approx = fract * m_q_rec_des / 0.92 / double(m_n_panels); // Approximate average absorbed flux per panel [W]
	soln.q_dot_inc.resize_fill(m_n_panels, qinc_approx);



	soln.dni = 500.;   // Not used, just need >0
	solve_for_mass_flow(soln);
	initialize_transient_param_inputs(soln, param_inputs);
	param_inputs.tm = m_tm;
	param_inputs.ramptime = m_flux_ramp_time;
	param_inputs.finitial = 0.0;
	param_inputs.ffinal = 1.0;
	if (m_flux_ramp_time == 0.0)
		param_inputs.finitial = 1.0;

	trans_inputs.inlet_temp = m_T_htf_cold_des;
	trans_inputs.tinit.fill(m_T_htf_cold_des);
	trans_inputs.tinit_wall.fill(m_T_htf_cold_des);
	double time_circulate, circulate_energy, parasitic;
	solve_transient_startup_model(param_inputs, trans_inputs, CIRCULATE, m_T_htf_hot_des + m_startup_target_delta, 0.0, 1.e6, trans_outputs, time_circulate, circulate_energy, parasitic);
	if (time_circulate == 1.e6)  // Transient model solution did not converge, use simple approximation of startup time instead
	{
		double tube_lam1 = (param_inputs.mflow_tot / m_n_lines / m_n_t)*param_inputs.c_htf / m_tm.at(1);
		double downc_lam1 = param_inputs.mflow_tot*param_inputs.c_htf / m_tm.back();
		time_circulate = (m_n_panels / m_n_lines)*m_h_rec / tube_lam1 + 0.5*(m_h_tower*m_pipe_length_mult + m_pipe_length_add) / downc_lam1;
		time_circulate += m_flux_ramp_time * 3600;
	}

	start_time += time_circulate;
	start_time = fmax(start_time, m_rec_su_delay*3600.0);
	start_energy += circulate_energy * 1.e-6 / 3600.0;

	est_time = start_time;
	est_energy = start_energy;
}


double C_mspt_receiver::est_heattrace_energy()
{
	double Tamb = 290.0;				// Typical ambient temperature (K) during startup
	if (m_is_startup_transient)			// Heat trace parasitic is only included if the transient startup model is activated
	{
		double riser_tm = m_tm_solid.at(0) * trans_inputs.length.at(0);						// Riser tube thermal mass (J/K)
		double downc_tm = m_tm_solid.back()* trans_inputs.length.back();	// Downcomer tube thermal mass (J/K)
		double heattrace_energy = (riser_tm + downc_tm) * (m_T_htf_cold_des - Tamb);	// Energy (J) needed to raise riser and downcomer from ambient T to target T
		return heattrace_energy*1e-6 / 3600.;											// MW-hr
	}
	else
		return 0.0;

}

double C_mspt_receiver::get_startup_time()
{
	double startup_time = std::numeric_limits<double>::quiet_NaN();
	if (!m_is_startup_transient)
		startup_time = m_rec_su_delay * 3600.; 
	else
	{
		double startup_energy = std::numeric_limits<double>::quiet_NaN();
		double fract = 0.4;  // Approximate mass flow / design point mass flow during startup
		est_startup_time_energy(fract, startup_time, startup_energy);
	}
    return startup_time;    // [s]
}

double C_mspt_receiver::get_startup_energy()
{
	double startup_energy = std::numeric_limits<double>::quiet_NaN();
	if (!m_is_startup_transient)
		startup_energy = m_rec_qf_delay * m_q_rec_des * 1.e-6;
	else
	{
		double startup_time = std::numeric_limits<double>::quiet_NaN();
		double fract = 0.4;  // Approximate mass flow / design point mass flow during startup
		est_startup_time_energy(fract, startup_time, startup_energy);	
	}
    return startup_energy;  // [MWh]
}
