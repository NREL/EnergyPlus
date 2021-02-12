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

#include "csp_solver_lf_dsg_collector_receiver.h"

#include "water_properties.h"

using namespace std;

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_csp_lf_dsg_collector_receiver::E_THETA_TRAVERSE, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_lf_dsg_collector_receiver::E_THETA_LONGITUDINAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_lf_dsg_collector_receiver::E_ETA_OPTICAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_lf_dsg_collector_receiver::E_DEFOCUS, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_INC_SF_TOT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_REC_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_REC_THERMAL_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_REC_ABS, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_PIPING_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_E_DOT_INTERNAL_ENERGY, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},				//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_Q_DOT_FREEZE_PROT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt]

	{C_csp_lf_dsg_collector_receiver::E_M_DOT_LOOP, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_M_DOT_FIELD, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[MWt]
	{C_csp_lf_dsg_collector_receiver::E_T_FIELD_COLD_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},	    //[C]
	{C_csp_lf_dsg_collector_receiver::E_T_REC_COLD_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},	    //[C]
	{C_csp_lf_dsg_collector_receiver::E_T_REC_HOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},	    //[C]
	{C_csp_lf_dsg_collector_receiver::E_X_REC_HOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[-]
	{C_csp_lf_dsg_collector_receiver::E_T_FIELD_HOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},     //[C]
	{C_csp_lf_dsg_collector_receiver::E_X_FIELD_HOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[-]
	{C_csp_lf_dsg_collector_receiver::E_PRESSURE_DROP, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[bar]

	{C_csp_lf_dsg_collector_receiver::E_W_DOT_SCA_TRACK, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWe]
	{C_csp_lf_dsg_collector_receiver::E_W_DOT_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[MWe]

	csp_info_invalid
};

C_csp_lf_dsg_collector_receiver::C_csp_lf_dsg_collector_receiver()
{
	n_integration_steps = 10;

	mc_reported_outputs.construct(S_output_info);

	// *******************************************
	// Hardcoded member data
	m_is_sensible_htf = false;
		// Set maximum timestep from parent class member data
	m_max_step = 60.0*60.0;			//[s]: [m] * [s/m] :: this is declared in parent CR class
	m_step_recirc = 10.0*60.0;		//[s]
		// Conversion constants: Commonly used values, conversions, etc...
	m_r2d = 180. / CSP::pi;
	m_d2r = CSP::pi / 180.;
	m_mtoinch = 39.3700787;	//[m] -> [in]
		// DSG Model Constants
	m_P_max = std::numeric_limits<double>::quiet_NaN();			//[bar]
	m_fP_turb_min = std::numeric_limits<double>::quiet_NaN();	//[-]
		// Water props limits
	m_wp_max_temp = std::numeric_limits<double>::quiet_NaN();	//[K]
	m_wp_min_temp = std::numeric_limits<double>::quiet_NaN();	//[K]
	m_wp_T_crit = std::numeric_limits<double>::quiet_NaN();		//[K]
	m_wp_max_pres = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	m_wp_min_pres = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	// *******************************************
	// *******************************************

	// *******************************************
	// Design Calculations
		// Geometry and Layout
	m_C_thermal = std::numeric_limits<double>::quiet_NaN();		//[kJ/K]
	m_fP_sf_tot = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_n_rows_matrix = -1;		//[-]
	m_nModTot = -1;				//[-]
	m_is_sh = false;			//[-]
	m_Ap_tot = std::numeric_limits<double>::quiet_NaN();		//[m2]
	m_Ap_loop = std::numeric_limits<double>::quiet_NaN();		//[m2]
	m_opt_eta_des = std::numeric_limits<double>::quiet_NaN();	//[-]
		// Energy and mass balance calcs
	m_q_dot_abs_tot_des = std::numeric_limits<double>::quiet_NaN();	//[kWt]
    m_q_dot_loss_tot_des = std::numeric_limits<double>::quiet_NaN();//[kWt]
	m_m_dot_min = std::numeric_limits<double>::quiet_NaN();			//[kg/s]
	m_m_dot_max = std::numeric_limits<double>::quiet_NaN();			//[kg/s]
	m_m_dot_b_max = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
	m_m_dot_b_des = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
	m_m_dot_pb_des = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
	m_m_dot_des = std::numeric_limits<double>::quiet_NaN();			//[kg/s]

	m_W_dot_sca_tracking_nom = std::numeric_limits<double>::quiet_NaN();	//[MWe]
	// *******************************************
	// *******************************************

	// ***********************************************************
	// Timestep Calculations
		// Control & operation
	m_operating_mode_converged = -1;	//[-]
	m_operating_mode = -1;				//[-]
	m_ncall = -1;						//[-]
		// CSP Solver Temperature Tracking
			// SUB TIMESTEP outputs
	m_q_dot_sca_loss_summed_subts = std::numeric_limits<double>::quiet_NaN();	//[MWt]
	m_q_dot_sca_abs_summed_subts = std::numeric_limits<double>::quiet_NaN();	//[MWt]
	m_q_dot_HR_cold_loss_subts = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_q_dot_HR_hot_loss_subts = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_E_dot_sca_summed_subts = std::numeric_limits<double>::quiet_NaN();		//[MWt]

	m_q_dot_to_sink_subts = std::numeric_limits<double>::quiet_NaN();		//[MWt]

			// FULL TIMESTEP outputs
	m_h_sys_c_in_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg]
	m_P_sys_c_in_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[bar]
	m_h_c_rec_in_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg]
	m_P_c_rec_in_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[bar]
	m_h_h_rec_out_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg]
	m_P_h_rec_out_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[bar]
	m_h_sys_h_out_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg]
	m_P_sys_h_out_t_int_fullts = std::numeric_limits<double>::quiet_NaN();	//[bar]

	m_q_dot_sca_loss_summed_fullts = std::numeric_limits<double>::quiet_NaN();	//[MWt]
	m_q_dot_sca_abs_summed_fullts = std::numeric_limits<double>::quiet_NaN();	//[MWt]
	m_q_dot_HR_cold_loss_fullts = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_q_dot_HR_hot_loss_fullts = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_E_dot_sca_summed_fullts = std::numeric_limits<double>::quiet_NaN();		//[MWt]

	m_q_dot_to_sink_fullts = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_q_dot_freeze_protection = std::numeric_limits<double>::quiet_NaN();	//[MWt]

	m_m_dot_loop = std::numeric_limits<double>::quiet_NaN();		//[MWt]
	m_m_dot_loop_des = std::numeric_limits<double>::quiet_NaN();	//[MWt]

	m_W_dot_sca_tracking = std::numeric_limits<double>::quiet_NaN();	//[MWe]
	m_W_dot_pump = std::numeric_limits<double>::quiet_NaN();			//[MWe]

		// Sun Position
	m_phi_t = std::numeric_limits<double>::quiet_NaN();		//[rad]
	m_theta_L = std::numeric_limits<double>::quiet_NaN();	//[rad]
		// Optical calcs
	m_ftrack = std::numeric_limits<double>::quiet_NaN();	//[-]
	m_eta_opt = std::numeric_limits<double>::quiet_NaN();	//[-]

	m_control_defocus = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_component_defocus = std::numeric_limits<double>::quiet_NaN();		//[-]

	m_q_dot_inc_sf_tot = std::numeric_limits<double>::quiet_NaN();		//[MWt]

		// Energy Balance
	m_Q_field_losses_total = std::numeric_limits<double>::quiet_NaN();		//[MJ]
	m_q_rec_loop = std::numeric_limits<double>::quiet_NaN();				//[kWt]
	m_q_inc_loop = std::numeric_limits<double>::quiet_NaN();				//[kWt]
	// *************************************************************
	// *************************************************************

	// *********************************************
	// Required for backwards compatability with TCS - call & init & converged only!
	// *********************************************
	m_defocus_prev = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_t_sby_prev = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_t_sby = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_is_pb_on_prev = false;										//[-]
	m_is_pb_on = false;												//[-]
	m_T_sys_prev = std::numeric_limits<double>::quiet_NaN();		//[K]
	m_defocus = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_is_def = true;												//[-]
	m_err_def = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_tol_def = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_rc = std::numeric_limits<double>::quiet_NaN();				//[-]
	// *********************************************
	// *********************************************



	// *******************************************
	// *******************************************
	// Design Parameters - public member data (user specified)
	m_q_max_aux = std::numeric_limits<double>::quiet_NaN();			//[kWt]
	m_LHV_eff = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_T_set_aux = std::numeric_limits<double>::quiet_NaN();			//[K]
	m_T_field_in_des = std::numeric_limits<double>::quiet_NaN();		//[K]
	m_T_field_out_des = std::numeric_limits<double>::quiet_NaN();		//[K]
	m_x_b_des = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_P_turb_des = std::numeric_limits<double>::quiet_NaN();			//[bar]
	m_fP_hdr_c = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_fP_sf_boil = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_fP_boil_to_sh = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_fP_sf_sh = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_fP_hdr_h = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_q_pb_des = std::numeric_limits<double>::quiet_NaN();				//[kWt]
	m_W_pb_des = std::numeric_limits<double>::quiet_NaN();				//[kWe]
	m_cycle_max_fraction = std::numeric_limits<double>::quiet_NaN();	//[-]
	m_cycle_cutoff_frac = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_m_dot_min_frac = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_m_dot_max_frac = std::numeric_limits<double>::quiet_NaN();		//[-]
	m_t_sby_des = std::numeric_limits<double>::quiet_NaN();				//[hr]
	m_q_sby_frac = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_PB_fixed_par = std::numeric_limits<double>::quiet_NaN();			//[-]
	m_T_startup = std::numeric_limits<double>::quiet_NaN();				//[K]

	m_fossil_mode = -1;													//[-]
	m_I_bn_des = std::numeric_limits<double>::quiet_NaN();				//[W/m2]
	m_is_oncethru = true;												//[-]
	m_is_sh_target = true;												//[-]
	m_is_multgeom = false;												//[-]
	m_nModBoil = -1;													//[-]
	m_nModSH = -1;														//[-]
	m_nLoops = -1;														//[-]
	m_eta_pump = std::numeric_limits<double>::quiet_NaN();				//[-]
	m_latitude = std::numeric_limits<double>::quiet_NaN();				//[rad]
	m_theta_stow = std::numeric_limits<double>::quiet_NaN();			//[rad]
	m_theta_dep = std::numeric_limits<double>::quiet_NaN();				//[rad]
	m_T_field_ini = std::numeric_limits<double>::quiet_NaN();			//[K]
	m_T_fp = std::numeric_limits<double>::quiet_NaN();					//[K]
	m_Pipe_hl_coef = std::numeric_limits<double>::quiet_NaN();			//[W/m2-K]
	m_SCA_drives_elec = std::numeric_limits<double>::quiet_NaN();		//[W/SCA]
	m_ColAz = std::numeric_limits<double>::quiet_NaN();					//[rad]
	m_e_startup = std::numeric_limits<double>::quiet_NaN();				//[kJ/K-m2], Thermal inertia contribution per sq meter of solar field
	m_T_amb_des_sf = std::numeric_limits<double>::quiet_NaN();			//[K]
	m_V_wind_max = std::numeric_limits<double>::quiet_NaN();			//[m/s]
	// ****************************************************************************
	// ****************************************************************************

}

void C_csp_lf_dsg_collector_receiver::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
	C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	// Save init_inputs to member data
	m_latitude = init_inputs.m_latitude;	//[deg]
	m_latitude *= m_d2r;					//[rad] convert from [deg]

	// Harcoded Parameter:
	m_P_max = 190.0;					//[bar]


	// Get limits from water properties code
	water_info wp_info;
	get_water_info( &wp_info );
	m_wp_max_temp = wp_info.temp_upper_limit;	//[K]
	m_wp_min_temp = wp_info.temp_lower_limit;	//[K]
	m_wp_T_crit = wp_info.T_critical;			//[K]
	m_wp_max_pres = wp_info.pres_upper_limit;	//[kPa]
	m_wp_min_pres = wp_info.pres_lower_limit;	//[kPa]

	// Compare the startup to final temperature
	int wp_code = water_PQ(m_P_turb_des*100.0, m_x_b_des, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init", "Design point water_PQ failed", wp_code));
	}
	double T_boil_des = wp.temp;	//[K]
	double T_startup_max = T_boil_des - 5.0;	//[K]

	if( !m_is_sh_target )
	{
		m_T_field_out_des = T_boil_des;		//[K]
		m_T_startup = T_startup_max;		//[K]

		if( m_T_startup > T_startup_max )
		{
			std::string msg = util::format("The specified configuration returns 2-phase steam"
								"The model requires the specified startup temperature, %lg, [C] to be at least"
								"5 [C] less than the boiling temperature, %lg, [C] at the design pressure"
								"The startup temperature was reset to %lg [C].", m_T_startup, T_boil_des, T_startup_max);
			mc_csp_messages.add_message(C_csp_messages::NOTICE,msg);
			m_T_startup = T_startup_max;
		}
	}
	else
	{
		m_T_startup = m_T_field_out_des;	//[K]
	}


	//[-] Outer glass envelope emissivities (Pyrex)
	m_EPSILON_5 = m_EPSILON_4;

	//m_AbsorberMaterial
	int n_rows_abs = (int)m_AbsorberMaterial_in.nrows();
	int n_cols_abs = (int)m_AbsorberMaterial_in.ncols();

	int n_rows_matrix_lk_in = 2;
	if( !(n_rows_abs == n_rows_matrix_lk_in && n_cols_abs == 1) )
	{
		std::string err_msg = util::format("Absorber material type matrix should have %d rows"
			"(b,SH) and 1 columns - the input matrix has %d rows and %d columns", n_rows_matrix_lk_in, n_rows_abs, n_cols_abs);

		throw(C_csp_exception(err_msg, "LF DSG init()"));
	}

	util::matrix_t<AbsorberProps*> m_AbsorberMaterial;
	util::matrix_t<HTFProperties*> m_AnnulusGas;

	int ii = 3;
	m_AbsorberMaterial.resize(n_rows_abs, n_cols_abs);
	for( int i = 0; i < n_rows_matrix_lk_in; i++ )
	{
		m_AbsorberMaterial.at(i, 0) = new AbsorberProps;
		m_AbsorberMaterial.at(i, 0)->setMaterial((int)m_AbsorberMaterial_in.at(i, 0));
	}

	//m_AnnulusGas [-] Annulus gas type (1 = air; 26 = Ar; 27 = H2 )
	n_rows_abs = (int)m_AnnulusGas_in.nrows();
	n_cols_abs = (int)m_AnnulusGas_in.ncols();
	if( !(n_rows_abs == n_rows_matrix_lk_in && n_cols_abs == 4) )
	{
		std::string err_msg = util::format("HCE annulus gas type matrix should have %d rows (b,SH) and 4 columns (HCE options) - the input matrix has %d rows and %d columns",
								n_rows_matrix_lk_in, n_rows_abs, n_cols_abs);

		throw(C_csp_exception(err_msg, "LF DSG init()"));
	}

	// Set up matrix_t of pointers to HTFproperties class
	m_AnnulusGas.resize(n_rows_abs, n_cols_abs);
	for( int i = 0; i < n_rows_matrix_lk_in; i++ )
	{
		for (int j = 0; j < 4; j++)
		{
			m_AnnulusGas.at(i, j) = new HTFProperties;
			m_AnnulusGas.at(i, j)->SetFluid((int)m_AnnulusGas_in.at(i, j));
		}
	}

	// set up m_D_h and m_A_cs
	m_D_h.resize(n_rows_matrix_lk_in, 1);
	m_A_cs.resize(n_rows_matrix_lk_in, 1);
	for( int i = 0; i < n_rows_matrix_lk_in; i++ )
	{
		if (m_Flow_type.at(i, 0) == 2.0)
			m_D_h.at(i, 0) = m_D_2.at(i, 0) - m_D_p.at(i, 0);		// [m] The hydraulic diameter for plug flow
		else
		{
			m_D_h.at(i, 0) = m_D_2.at(i, 0);						// [m] The hydraulic diameter for tube flow
			m_D_p.at(i, 0) = 0.0;
		}
		m_A_cs.at(i, 0) = CSP::pi*(m_D_2.at(i, 0)*m_D_2.at(i, 0) - m_D_p.at(i, 0)*m_D_p.at(i, 0)) / 4.0;	// [m^2] The cross-sectional flow area
	}


	if (m_is_multgeom)
	{
		//[-] Organize the boiler/superheater emittance tables here
		eps_abs.init(n_rows_matrix_lk_in, 4);
		eps_abs.addTable(&m_b_eps_HCE1);
		eps_abs.addTable(&m_b_eps_HCE2);
		eps_abs.addTable(&m_b_eps_HCE3);
		eps_abs.addTable(&m_b_eps_HCE4);
		eps_abs.addTable(&m_sh_eps_HCE1);
		eps_abs.addTable(&m_sh_eps_HCE2);
		eps_abs.addTable(&m_sh_eps_HCE3);
		eps_abs.addTable(&m_sh_eps_HCE4);
	}
	else
	{
		// Organize the boiler emittance tables here
		eps_abs.init(n_rows_matrix_lk_in, 4);
		eps_abs.addTable(&m_b_eps_HCE1);
		eps_abs.addTable(&m_b_eps_HCE2);
		eps_abs.addTable(&m_b_eps_HCE3);
		eps_abs.addTable(&m_b_eps_HCE4);
	}

	//[-] Boiler Optical Table
	n_rows_abs = (int)m_b_OpticalTable.nrows();
	n_cols_abs = (int)m_b_OpticalTable.ncols();

	// Set up the optical table object..

	/* The input should be defined as follows:
	- Data of size nx, ny
	- OpticalTable of size (nx+1)*(ny+1)
	- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
	- First value of remaining ny rows are y-axis values, not data
	- Data is contained in cells i,j : where i>1, j>1
	*/
	double *xax = new double[n_cols_abs - 1];
	double *yax = new double[n_rows_abs - 1];
	double *data = new double[(n_cols_abs - 1) * (n_rows_abs - 1)];

	//get the xaxis data values
	for (int i = 0; i < n_cols_abs - 1; i++)
		xax[i] = m_b_OpticalTable.at(0, i)*m_d2r;

	//get the yaxis data values
	for (int j = 1; j < n_rows_abs; j++)
		yax[j - 1] = m_b_OpticalTable.at(j, 0)*m_d2r;

	//get the data values
	for (int j = 1; j < n_rows_abs; j++)
	{
		for (int i = 1; i < n_cols_abs; i++)
			data[i - 1 + (n_cols_abs - 1)*(j - 1)] = m_b_OpticalTable.at(j, i);
	}

	// debug, check the b_optical_table
	b_optical_table.AddXAxis(xax, n_cols_abs - 1);
	b_optical_table.AddYAxis(yax, n_rows_abs - 1);
	b_optical_table.AddData(data);
	delete [] xax;
	delete [] yax;
	delete [] data;
	optical_tables.Set_Table(&b_optical_table, 0);

	// *************************
	//[-] Superheater Optical Table
	// *************************
	//0608
	if (m_is_multgeom)
	{
		int n_rows = (int)m_sh_OpticalTable.nrows();
		int n_cols = (int)m_sh_OpticalTable.ncols();

		// Set up the optical table object..

		/* The input should be defined as follows:
		- Data of size nx, ny
		- OpticalTable of size (nx+1)*(ny+1)
		- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
		- First value of remaining ny rows are y-axis values, not data
		- Data is contained in cells i,j : where i>1, j>1
		*/
		double *xax1 = new double[n_cols - 1];
		double *yax1 = new double[n_rows - 1];
		double *data1 = new double[(n_cols - 1) * (n_rows - 1)];

		//get the xaxis data values
		for (int i = 0; i < n_cols - 1; i++){
			xax1[i] = m_sh_OpticalTable.at(0, i)*m_d2r;
		}
		//get the yaxis data values
		for (int j = 1; j < n_rows; j++){
			yax1[j - 1] = m_sh_OpticalTable.at(j, 0)*m_d2r;
		}
		//Get the data values
		for (int j = 1; j < n_rows; j++){
			for (int i = 1; i < n_cols; i++){
				data1[i - 1 + (n_cols - 1)*(j - 1)] = m_sh_OpticalTable.at(j, i);
			}
		}
		sh_optical_table.AddXAxis(xax1, n_cols - 1);
		sh_optical_table.AddYAxis(yax1, n_rows - 1);
		sh_optical_table.AddData(data1);
		delete [] xax1;
		delete [] yax1;
		delete []data1;
		optical_tables.Set_Table(&sh_optical_table, 1);
	}
	//0608
	// ****************************************************************************
	// Don't write SH row in matrix_t if superheater if not different than boiler?
	// ****************************************************************************

	// Determine whether an incompatible set of inputs has been provided for the boiler quality. For recirc systems, the
	// boiler quality must fall in the range (0..1]. Limit to a minimum of .01 for convergence sake.
	if (!m_is_oncethru)
	{
		if (m_x_b_des < 0.01 || m_x_b_des > 1.0)
		{
			std::string err_msg = util::format("For recirculated boiler systems, the specified boiler outlet quality %.3f must be greater than 0.01 and less than or equal to 1.0", m_x_b_des);
			throw(C_csp_exception(err_msg, "LF DSG init()"));
		}
	}

	// Initialize any member matrix_t values
	if( m_nModSH > 0 )
		m_is_sh = true;
	else
		m_is_sh = false;

	m_n_rows_matrix = 1;
	if( m_is_multgeom )
		m_n_rows_matrix = 2;

	// Optical efficiency is only a function of collector geometry, so not unique for each collector
	// So vector's length is = m_n_rows_matrix
	m_eta_optical.resize(m_n_rows_matrix);

	m_nModTot = m_nModBoil + m_nModSH;
	m_q_inc.resize(m_nModTot);				//[kWt]
	m_q_inc_control_df.resize(m_nModTot);	//[kWt]
	m_q_loss.resize(m_nModTot);
	m_q_abs.resize(m_nModTot);
	m_T_ave.resize(m_nModTot, 1);
	m_h_in.resize(m_nModTot, 1);
	m_h_out.resize(m_nModTot, 1);
	m_q_rec.resize(m_nModTot);
	m_q_rec_control_df.resize(m_nModTot);

	mc_sca_out_t_end_converged.resize(m_nModTot);
	mc_sca_out_t_end_last.resize(m_nModTot);
	mc_sca_in_t_int.resize(m_nModTot);
	mc_sca_out_t_end.resize(m_nModTot);
	mc_sca_out_t_int.resize(m_nModTot);

	m_q_inc.assign(m_q_inc.size(),0.0);		//[kWt]
	m_q_inc_control_df.assign(m_q_inc_control_df.size(),0.0);	//[kWt]
	m_q_loss.assign(m_q_loss.size(),0.0);	//[kWt]
	m_q_abs.assign(m_q_abs.size(), 0.0);	//[kWt] Thermal power absorbed by steam in each receiver
	m_T_ave.fill(0.0);
	m_h_in.fill(0.0);
	m_h_out.fill(0.0);
	m_q_rec.assign(m_q_rec.size(), 0.0);	//[kWt]
	m_q_rec_control_df.assign(m_q_rec_control_df.size(), 0.0);	//[kWt]

	// Set any constants
	m_fP_turb_min = 0.5;								//[-] Minimum fractional operating pressure at the turbine inlet

	// Convert theta_stow
	m_theta_stow = CSP::pi - m_theta_stow;

	// Design point values
	double omega = 0.0;						// solar noon
	double dec = 23.45*CSP::pi / 180.0;		// declination at summer solstice

	// Solar altitude at noon on the summer solstice
	double SolarAlt = asin(sin(dec)*sin(m_latitude) + cos(m_latitude)*cos(dec)*cos(omega));

	// Convert the solar angles to collector incidence angles
	double phi_t, theta_L;
	CSP::theta_trans(0.0, (CSP::pi / 2.0 - SolarAlt), m_ColAz, phi_t, theta_L);

	// Calculate the design-point efficiency
	m_eta_opt_fixed.resize(m_n_rows_matrix, 1);
	for (int i = 0; i < m_n_rows_matrix; i++)
		m_eta_opt_fixed.at(i, 0) = m_TrackingError.at(i, 0) * m_GeomEffects.at(i, 0) * m_rho_mirror_clean.at(i, 0) * m_dirt_mirror.at(i, 0) * m_error.at(i, 0);

	m_opteff_des.resize(m_n_rows_matrix, 1);

	// Boiler receiver optical efficiency
	if (m_OptCharType.at(0, 0) == 1)
	{
		// User provides an optical table as a function of solar position
		m_opteff_des.at(0, 0) = m_eta_opt_fixed.at(0, 0)*max(b_optical_table.interpolate(0.0, CSP::pi / 2.0 - SolarAlt), 0.0);
	}
	else if (m_OptCharType.at(0, 0) == 2)
	{
		// Collector incidence table
		m_opteff_des.at(0, 0) = m_eta_opt_fixed.at(0, 0)*max(b_optical_table.interpolate(0.0, theta_L), 0.0);
	}
	else if (m_OptCharType.at(0, 0) == 3)
	{
		// IAM polynomials
		double iam_t = 0.0;
		double iam_l = 0.0;
		for (size_t i = 0; i < m_IAM_L.ncols(); i++)
			iam_l += m_IAM_L.at(0, i)*pow(theta_L, i);
		for (size_t i = 0; i < m_IAM_T.ncols(); i++)
			iam_t += m_IAM_T.at(0, i)*pow(phi_t, i);
		m_opteff_des.at(0, 0) = m_eta_opt_fixed.at(0, 0) * iam_t * iam_l;
	}
	else
	{

		std::string err_msg = util::format("The selected boiler optical model (%d) does not exist. Options are 1=Solar position table : 2=Collector incidence table : 3= IAM polynomials", m_OptCharType.at(0, 0));
		throw(C_csp_exception(err_msg, "LF DSG init()"));
	}

	if (m_is_multgeom)
	{
		// Superheater receiver optical efficiency
		if (m_OptCharType.at(1, 0) == 1)
		{
			// User provides an optical table as a function of solar position
			m_opteff_des.at(1, 0) = m_eta_opt_fixed.at(1, 0)*max(sh_optical_table.interpolate(0.0, CSP::pi / 2.0 - SolarAlt), 0.0);
		}
		else if (m_OptCharType.at(1, 0) == 2)
		{
			// Collector incidence table
			m_opteff_des.at(1, 0) = m_eta_opt_fixed.at(1, 0)*max(sh_optical_table.interpolate(0.0, theta_L), 0.0);
		}
		else if (m_OptCharType.at(1, 0) == 3)
		{
			// IAM polynomials
			double iam_t = 0.0;
			double iam_l = 0.0;
			for (size_t i = 0; i < m_IAM_L.ncols(); i++)
				iam_l += m_IAM_L.at(1, i)*pow(theta_L, i);
			for (size_t i = 0; i < m_IAM_T.ncols(); i++)
				iam_t += m_IAM_T.at(1, i)*pow(phi_t, i);
			m_opteff_des.at(1, 0) = m_eta_opt_fixed.at(1, 0) * iam_t * iam_l;
		}
		else
		{
			std::string err_msg = util::format("The selected superheater optical model (%d) does not exist. Options are 1=Solar position table : 2=Collector incidence table : 3= IAM polynomials", m_OptCharType.at(1, 0));
			throw(C_csp_exception(err_msg, "LF DSG init()"));
		}
	}

	// Calculate the design-point incident energy on each module for a single loop
	for (int i = 0; i < m_nModTot; i++)
	{
		int gset = 0;
		if ((i >= m_nModBoil) && (m_is_multgeom))
			gset = 1;
		else
			gset = 0;
		m_q_inc[i] = m_I_bn_des * m_A_aperture.at(gset, 0) / 1000.0;	//[kWt]
		m_q_rec[i] = m_q_inc[i] * m_opteff_des.at(gset, 0);				//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*
	}

	// Calculate the total reference pressure drop across the field
	if (!m_is_sh)
	{
		m_fP_boil_to_sh = 0.0;
		m_fP_sf_sh = 0.0;
	}
	m_fP_sf_tot = m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h;	//[-]

	if (m_P_turb_des*(1.0 + m_fP_sf_tot) > 220.6)
	{
		std::string err_msg = util::format("The design-point pressure at the inlet of the solar field exceeds the critical pressure (220.6 bar). Review your settings for turbine inlet pressure and solar field pressure drop to maintain reasonable design pressure conditions");
		throw(C_csp_exception(err_msg, "LF DSG init()"));
	}

	check_pressure.set_P_max(m_P_max);
	double h_field_in = 0.0;	//[kJ/kg]
	double h_field_out = 0.0;	//[kJ/kg]

	// Estimate the design-point thermal losses
	// *********** "standard" boiler + optional superheater design *******
	if (!m_is_oncethru)		// Analyze the conventional boiler only/boiler+superheat options
	{
		// Calculate boiler inlet/outlet enthalpies
		water_PQ(check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, m_x_b_des, &wp);
		double h_b_out_des = wp.enth;		//[kJ/kg]

		// Power block outlet/field inlet enthalpy
		int wp_code = water_TP(m_T_field_in_des, check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point state point calcs failed", "water_TP error", wp_code));
		}
		if( wp.qual > 0.0 )
		{
			throw(C_csp_exception("The design inlet to the once thru loop at the design field inlet pressure this is not good"));
		}
		h_field_in = wp.enth;		//[kJ/kg]

		// Determine the mixed boiler inlet enthalpy
		double h_b_in_des = h_field_in*m_x_b_des + h_b_out_des*(1.0 - m_x_b_des);
		double dh_b_des = (h_b_out_des - h_b_in_des) / (double)m_nModBoil;
		for (int i = 0; i < m_nModBoil; i++)
		{
			// Calculate the local pressure in the boiler. Assume a linear pressure drop across each section
			double P_loc = m_P_turb_des*(1.0 + m_fP_sf_tot - m_fP_sf_boil*(1.0 - (double)(i) / (double)m_nModBoil));
			// Get the temperature and quality at each state in the boiler
			water_PH(check_pressure.P_check(P_loc)*100.0, (h_b_in_des + dh_b_des*(double)(i + 1) - dh_b_des / 2.0), &wp);
			m_T_ave.at(i, 0) = wp.temp;		//[K]

			// Calculate the heat loss at each temperature
			if (m_HLCharType.at(0, 0) == 1)
			{
				// Estimate based on the polynomial adjustments provided by the user
				double dT_loc = m_T_ave.at(i, 0) - m_T_amb_des_sf;
				double c_hl = m_HL_dT.at(0, 0) + m_HL_dT.at(0, 1)*dT_loc + m_HL_dT.at(0, 2)*pow(dT_loc, 2) + m_HL_dT.at(0, 3)*pow(dT_loc, 3) + m_HL_dT.at(0, 4)*pow(dT_loc, 4);		//[W/m] Don't adjust for wind speed here
				m_q_loss[i] = c_hl*m_L_col.at(0, 0) / 1000.0;		//[kWt] Total thermal loss from this collector
			}
			else if (m_HLCharType.at(0, 0) == 2)
			{
				// Estimate based on user-supplied guesses for thermal losses
				m_q_loss[i] = 0.0;
				for (int j = 0; j < 4; j++)
					m_q_loss[i] += m_Design_loss.at(0, j)*m_HCE_FieldFrac.at(0, j)*m_L_col.at(0, 0) / 1000.0;		// [kWt] Thermal loss for each receiver in loop
			}
			m_q_abs[i] = m_q_rec[i] - m_q_loss[i];		//[kWt] Thermal power absorbed by steam in each receiver
		}

		if (m_is_sh)
		{
			// Decide which geometry set to use
			int gset = 0;
			if (m_is_multgeom) gset = 1;

			// Calculate superheater inlet/outlet enthalpies
			water_PQ(check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h + m_fP_sf_sh))*100.0, 1.0, &wp);
			double h_sh_in_des = wp.enth;

			water_TP((m_T_field_out_des), check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h))*100.0, &wp);
			h_field_out = wp.enth;

			double dh_sh_des = (h_field_out - h_sh_in_des) / (double)m_nModSH;
			for (int ii = 0; ii < m_nModSH; ii++)
			{
				int i = ii + m_nModBoil;
				// Calculate the local pressure in the superheater. Assume a linear pressure drop
				double P_loc = m_P_turb_des*(1.0 + m_fP_hdr_h + m_fP_sf_sh*(1.0 - (double)(ii) / (double)m_nModSH));
				water_PH(check_pressure.P_check(P_loc)*100.0, (h_sh_in_des + dh_sh_des*(double)(ii + 1) - dh_sh_des / 2.0), &wp);
				m_T_ave(i, 0) = wp.temp;		// Convert to K

				// Calculate the heat loss at each temperature
				if (m_HLCharType.at(gset, 0) == 1)
				{
					// Polynomials
					double dT_loc = m_T_ave(i, 0) - m_T_amb_des_sf;
					double c_hl = m_HL_dT.at(gset, 0) + m_HL_dT.at(gset, 1)*dT_loc + m_HL_dT.at(gset, 2)*pow(dT_loc, 2) + m_HL_dT.at(gset, 3)*pow(dT_loc, 3) + m_HL_dT.at(gset, 4)*pow(dT_loc, 4);	//[w/m] Don't adjust for wind speed here
					m_q_loss[i] = c_hl*m_L_col.at(gset, 0) / 1000.0;		//[kWt] Thermal loss for each receiver in loop
				}
				else if (m_HLCharType.at(gset, 0) == 2)
				{
					// User supplied guesses for thermal losses - Forristall model
					m_q_loss[i] = 0.0;
					for (int j = 0; j < 4; j++)
						m_q_loss[i] += m_Design_loss.at(gset, j)*m_HCE_FieldFrac.at(gset, j)*m_L_col.at(gset, 0) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
				}
				m_q_abs[i] = m_q_rec[i] - m_q_loss[i];			//[kWt] Thermal power absorbed by steam in each receiver
			}
		}
	}
	else	// Analyze the once-through boiler+superheater options
	{
		// Calculate the total enthalpy rise across the loop
		int wp_code = water_TP((m_T_field_in_des), check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_sf_tot))*100.0, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point inlet state point calcs failed", "water_TP error", wp_code));
		}
		if( wp.qual > 0.0 )
		{
			throw(C_csp_exception("The design inlet to the once thru loop at the design field inlet pressure this is not good"));
		}
		h_field_in = wp.enth;	//[kJ/kg]

		if( m_is_sh_target )
		{
			wp_code = water_TP(m_T_field_out_des, check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h))*100.0, &wp);
			if( wp_code != 0 )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_TP error", wp_code));
			}
			h_field_out = wp.enth;	//[kJ/kg]
		}
		else
		{
			wp_code = water_PQ(m_P_turb_des*(1.0+m_fP_hdr_h)*100.0, m_x_b_des, &wp);
			if( wp_code != 0 )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_PQ error", wp_code));
			}
			h_field_out = wp.enth;	//[kJ/kg]
		}

		// Enthalpy rise across each collector module
		double dh_ot_des = (h_field_out - h_field_in) / (double)m_nModTot;		//[kJ/kg]
		for (int i = 0; i < m_nModTot; i++)
		{
			// Decide which geometry set to use
			int gset = 0;
			if (i >= m_nModBoil && m_is_multgeom)	gset = 1;

			// Calculate the local pressure in the loop, assume a linear pressure drop
			double P_loc = m_P_turb_des*(1.0 + (m_fP_sf_boil + m_fP_sf_sh)*(1.0 - (double)(i) / (double)m_nModTot) + m_fP_hdr_h);
			// Get the temperature/quality at each state in the loop
			water_PH(check_pressure.P_check(P_loc)*100.0, (h_field_in + dh_ot_des*(double)(i + 1) - dh_ot_des / 2.0), &wp);
			m_T_ave.at(i, 0) = wp.temp;

			// Calculate the heat loss at each temperature
			if (m_HLCharType.at(gset, 0) == 1)
			{
				// Polynomials
				double dT_loc = m_T_ave.at(i, 0) - m_T_amb_des_sf;
				double c_hl = m_HL_dT.at(gset, 0) + m_HL_dT.at(gset, 1)*dT_loc + m_HL_dT.at(gset, 2)*pow(dT_loc, 2) + m_HL_dT.at(gset, 3)*pow(dT_loc, 3) + m_HL_dT.at(gset, 4)*pow(dT_loc, 4);	//[w/m] Don't adjust for wind speed here
				m_q_loss[i] = c_hl*m_L_col.at(gset, 0) / 1000.0;		//[kW] total collector thermal loss
			}
			else if (m_HLCharType.at(gset, 0) == 2)
			{
				// User supplied guesses for thermal losses - Forristall model
				m_q_loss[i] = 0.0;
				for (int j = 0; j < 4; j++)
					m_q_loss[i] += m_Design_loss.at(gset, j)*m_HCE_FieldFrac.at(gset, j)*m_L_col.at(gset, 0) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
			}
			m_q_abs[i] = m_q_rec[i] - m_q_loss[i];			//[kWt] Thermal power absorbed by steam in each receiver
		}
	}

	// Calculate total solar field aperture area
	m_Ap_tot = 0.0;		//[m2]
	m_Ap_loop = 0.0;	//[m2]
	if (m_is_multgeom)
	{
		m_Ap_loop = (m_A_aperture.at(0, 0)*m_nModBoil + m_A_aperture.at(1, 0)*m_nModSH);	//[m2]
	}
	else
		m_Ap_loop = m_A_aperture.at(0, 0)*(double)(m_nModTot);		//[m2]

	m_Ap_tot = m_Ap_loop * m_nLoops;


	// Estimate piping thermal loss
	double q_loss_piping = m_Ap_tot*m_Pipe_hl_coef / 1000.0*((m_T_field_in_des + m_T_field_out_des) / 2.0 - m_T_amb_des_sf);		// hl coef is [W/m2-K], use average field temp as driving difference

	// ************* Design solar field thermal power output **************
	double q_inc_tot_loop, q_rec_tot_loop, q_loss_tot_loop, q_abs_tot_loop;
	q_inc_tot_loop = q_rec_tot_loop = q_loss_tot_loop = q_abs_tot_loop = 0.0;
	for (int i = 0; i < m_nModTot; i++)
	{
		q_inc_tot_loop += m_q_inc[i];			//[kWt] LOOP total incident beam radiation
		q_rec_tot_loop += m_q_rec[i];			//[kWt] LOOP total incident thermal power after *optical* losses and *defocus*
		q_loss_tot_loop += m_q_loss[i];			//[kWt] LOOP total thermal losses
	}
	double q_inc_tot_des = q_inc_tot_loop*(double)m_nLoops;
	double m_q_rec_tot_des = q_rec_tot_loop*(double)m_nLoops;			//[kWt] SYSTEM total design incident thermal power after *optical* losses
	m_q_dot_loss_tot_des = q_loss_tot_loop*(double)m_nLoops + q_loss_piping;
	m_q_dot_abs_tot_des = m_q_rec_tot_des - m_q_dot_loss_tot_des;			//[kWt]

	double eta_therm_sf_des = 1.0 - m_q_dot_loss_tot_des / m_q_rec_tot_des;		//[-] Design solar field efficiency

	double eta_tot_sf_des = 0.0;
	m_opt_eta_des = 0.0;
	if (m_is_multgeom)
	{
		m_opt_eta_des = (m_opteff_des.at(0, 0)*m_nModBoil*m_A_aperture.at(0, 0) + m_opteff_des.at(1, 0)*m_nModSH*m_A_aperture.at(1, 0)) / (m_nModBoil*m_A_aperture.at(0, 0) + m_nModSH*m_A_aperture.at(1, 0));
		eta_tot_sf_des = m_opt_eta_des*eta_therm_sf_des; // Design solar field total efficiency
	}
	else
	{
		m_opt_eta_des = m_opteff_des.at(0, 0);
		eta_tot_sf_des = m_opt_eta_des*eta_therm_sf_des;
	}

	// Calculate the design-point mass flow rate leaving the solar field
	if(h_field_out == 0.0 || h_field_in == 0.0)
	{
		std::string err_msg = util::format("At design, either the superheater outlet enthlalpy (%.1f) or the solarfield inlet density (%f) is not set", h_field_out, h_field_in);
		throw(C_csp_exception(err_msg, "LF DSG init()"));
	}

	m_m_dot_des = m_q_dot_abs_tot_des / (h_field_out - h_field_in);	//[kg/s] SYSTEM design point mass flow rate - field design basis
	m_m_dot_loop_des = m_m_dot_des / (double)m_nLoops;				//[kg/s] LOOP design point mass flow rate

	// Calculate the Solar Field design-point mass flow rate in the boiler only (include recirculation mass)
	m_m_dot_b_des = 0.0;
	if( m_x_b_des == 0.0 || !m_is_sh_target )
		m_m_dot_b_des = m_m_dot_des;
	else
		m_m_dot_b_des = m_m_dot_des / m_x_b_des;

	// ***************************************************
	// Ty: modified code for IPH additional to PowerGen baseline
	// ***************************************************
	// Calculate maximum flow rate to the power block
	m_m_dot_pb_des = m_q_pb_des / (h_field_out - h_field_in);	//[kg/s] SYSTEM mass flow rate at power cycle design
	double m_dot_pb_max = m_m_dot_pb_des * m_cycle_max_fraction;	//[kg/s] SYSTEM max mass flow rate - power cycle design basis
	double m_dot_sf_max = m_m_dot_max_frac * m_m_dot_des;			//[kg/s] SYSTEM max mass flow rate - field design basis

	m_m_dot_max = min(m_dot_sf_max, m_dot_pb_max) / (double)m_nLoops;			//[kg/s] LOOP max mass flow rate - min of field & PC bases

	if( m_x_b_des == 0.0 || !m_is_sh_target )
		m_m_dot_b_max = m_m_dot_max;											//[kg/s] LOOP max mass flow rate through boiler
	else
		m_m_dot_b_max = m_m_dot_max / m_x_b_des;								//[kg/s] LOOP max mass flow rate through boiler

	// 9.26.16 twn: Added m_m_dot_max_frac and m_m_dot_min_frac as Design Parameters
	double m_m_dot_min_sf = m_m_dot_min_frac * m_m_dot_des;			//[kg/s] SYSTEM min mass flow rate - solar field basis
	double m_m_dot_min_pc = m_cycle_cutoff_frac * m_m_dot_pb_des;	//[kg/s] SYSTEM min mass flow rate - PC basis
	m_m_dot_min = max(m_m_dot_min_sf, m_m_dot_min_pc) / (double)m_nLoops;	//[kg/s] LOOP min mass flow rate - max of field & PC bases
	// *****************************************************
	// *****************************************************

	// Convert the thermal inertia term here
	m_C_thermal = m_e_startup * m_Ap_tot / (double)(m_nModTot*m_nLoops);		//[kJ/m2-K] -> [kJ/K] Average transient energy per collector

	// Check to see if the design provided by the user is reasonable. If not, return a warning message
	double T_burn = 0.0;
	if (!m_is_oncethru)
	{
		water_TP(m_T_field_in_des, check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_sf_tot))*100.0, &wp);	// solar field inlet
		double dvar1 = wp.enth;
		water_PQ(check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, m_x_b_des, &wp);	// boiler outlet
		double dvar2 = wp.enth;
		water_PQ(check_pressure.P_check(m_P_turb_des*(1.0 + m_fP_hdr_h + m_fP_sf_sh))*100.0, 1.0, &wp);		// superheater inlet
		double dvar7 = wp.enth;
		double dvar3 = (dvar2 - dvar1) / (double)m_nModBoil;		// The enthalpy rise per boiler module

		// Calculate the expected enthalpy rise in the superheater based on ratios of expected energy absorptoin between the boiler and superheater
		double dvar10 = 0.0;
		if (m_is_multgeom)
		{
			double q_loss_b_sum = 0.0;
			double q_loss_sh_sum = 0.0;
			for (int i = 0; i < m_nModBoil; i++)
				q_loss_b_sum += m_q_loss[i];		//[kWt] SubLOOP total thermal losses in boiler
			for (int i = m_nModBoil; i < m_nModTot; i++)
				q_loss_sh_sum += m_q_loss[i];		//[kWt] SubLOOP total thermal losses in SH

			// = (Expected performance of the superheater modules) / (Expected performance of the boiler modules)
			dvar10 = (m_opteff_des.at(1, 0)*m_A_aperture.at(1, 0)*m_I_bn_des / 1000.0 - q_loss_sh_sum / (double)m_nModSH) /
				(m_opteff_des.at(0, 0)*m_A_aperture.at(0, 0)*m_I_bn_des / 1000.0 - q_loss_b_sum / (double)m_nModBoil);
		}
		else
		{
			// No separate geometry, so approximate the superheater and boiler as the same
			dvar10 = 1.0;
		}
		// Project this to the superheater modules
		double dvar4 = dvar7 + dvar3*m_nModSH*dvar10;		// Estimated superheater outlet enthalpy
		// Check the temperature
		water_PH(m_P_turb_des*(1.0 - m_fP_boil_to_sh)*100.0, dvar4, &wp);
		double dvar5 = wp.temp;						// convert to K
		double dvar6 = dvar5 - m_T_field_out_des;			// Difference in temperature between estimated outlet temperature and user-spec
		// What are the superheater design conditions?
		water_TP(m_T_field_out_des, check_pressure.P_check(m_P_turb_des*(1.0*m_fP_hdr_h))*100.0, &wp);	// Superheater outlet
		double dvar8 = wp.enth;
		double dvar9 = (dvar8 - dvar7) / (dvar2 - dvar1)*m_nModBoil;

		if (fabs(dvar6) > 25.0)
		{

			std::string err_msg = util::format("The field you selected with %d boiler modules and %d superheater modules results in a projected superheater outlet temperature"
				" of %.1f [C] which is %.1f [C] away from the design-point value of %.1f [C]. Based on the specified collector geometry, the ideal fractional"
				" number of superheater modules is approximately %.2f. Consider adjusting the design-point steam settings, the module geometry, and/or"
				" the module distribution to better match the desired steam conditions", m_nModBoil, m_nModSH, dvar5 - 273.15, dvar6, m_T_field_out_des - 273.15, dvar9);
			throw(C_csp_exception(err_msg, "LF DSG init()"));
		}
		/*if( dvar6 < -25.0 )
		{
		message( TCS_WARNING,  "The field you selected with %d boiler modules and %d superheater modules results in a projected superheater outlet temperature"
		" of %d [C] which is %d [C] away from the design-point value of %d [C]. Based on the specified collector geometry, the ideal fractional"
		" number of superheater modules is approximately %d. Consider adjusting the design-point steam settings, the module geometry, and/or"
		" the module distribution to better match the desired steam conditions", m_nModBoil, m_nModSH, dvar5-273.15, -dvar6, m_T_field_out_des-273.15, dvar9 );
		}*/
		T_burn = max(dvar5, m_T_field_out_des) - 273.15;
	}
	else
	{
		// The value of the design-point temperature is used in the limiting function for enthlapy. Set the value to
		// the field design outlet temperature plus a margin
		T_burn = m_T_field_out_des - 273.15;
	}

	// Calculate the minimum allowable enthalpy before freezing
	double pres_test_frac = 0.9;
	if( m_fossil_mode != 4 )
		pres_test_frac = m_cycle_cutoff_frac;

	water_TP(5.0 + 273.15, m_P_turb_des*pres_test_frac*100.0, &wp);
	double h_freeze = wp.enth;
	// Calculate the maximum allowable enthalpy before convergence error
	water_TP(min(T_burn + 150.0 + 273.15, 1000.0), m_P_max*100.0, &wp);
	double h_burn = wp.enth;
	// Set up the enthalpy limit function
	check_h.set_enth_limits(h_freeze, h_burn);

	m_T_ave_prev.resize(m_nModTot, 1);
	m_T_ave_prev.assign(m_T_ave_prev.size(), m_T_field_ini);

	mc_sys_cold_out_t_end_converged.m_temp = m_T_field_ini;		//[K]
	mc_sys_hot_out_t_end_converged.m_temp = m_T_field_ini;		//[K]
	for(int i = 0; i < m_nModTot; i++)
	{
		mc_sca_out_t_end_converged[i].m_temp = m_T_field_ini;	//[K]
	}

	m_defocus_prev = 1.0;
	m_t_sby_prev = m_t_sby_des;
	m_t_sby = m_t_sby_des;
	m_is_pb_on_prev = false;
	m_T_sys_prev = m_T_field_ini;

	// Initialize evacuated tube model if used
	for (int i = 0; i < m_n_rows_matrix; i++)
	{
		if (m_HLCharType.at(i, 0) == 2)
		{
			htfProps.SetFluid(21);
			evac_tube_model.Initialize_Receiver(m_GlazingIntactIn, m_P_a, m_D_5, m_D_4, m_D_3, m_D_2, m_D_p, m_opteff_des, m_Dirt_HCE, m_Shadowing, m_Tau_envelope, m_alpha_abs,
				m_alpha_env, &eps_abs, m_AnnulusGas, m_AbsorberMaterial, m_EPSILON_4, m_EPSILON_5, m_L_col, &htfProps, m_A_cs, m_D_h, m_Flow_type);
		}
	}

	m_ncall = -1;

	// Calculate the tracking parasitics for when trough is on sun
	m_W_dot_sca_tracking_nom = m_SCA_drives_elec*m_Ap_tot/1.E6;		//[MWe]

	// Set solved parameters
	solved_params.m_T_htf_cold_des = m_T_field_in_des;	//[K] Design point inlet temperature
	solved_params.m_P_cold_des = m_P_turb_des*100.0;	//[kPa] Design point *field outlet* pressure
	solved_params.m_x_cold_des = -1;					//[-] Design point inlet quality
	solved_params.m_q_dot_rec_des = m_q_dot_abs_tot_des / 1.E3;	//[MWt] Absorbed thermal power delivered to HTF at design
	solved_params.m_A_aper_total = m_Ap_tot;			//[m2]

	// Set previous operating mode
	m_operating_mode_converged = C_csp_collector_receiver::OFF;					//[-] 0 = requires startup, 1 = starting up, 2 = running

	return;

} // init

int C_csp_lf_dsg_collector_receiver::get_operating_state()
{
	return m_operating_mode_converged;
}


double C_csp_lf_dsg_collector_receiver::get_startup_time()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_lf_dsg_collector_receiver::get_startup_energy()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_lf_dsg_collector_receiver::get_pumping_parasitic_coef()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_lf_dsg_collector_receiver::get_min_power_delivery()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_lf_dsg_collector_receiver::get_tracking_power()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::get_tracking_power() is not complete"));
	return std::numeric_limits<double>::quiet_NaN(); //MWe
}

double C_csp_lf_dsg_collector_receiver::get_col_startup_power()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::get_col_startup_power() is not complete"));
	return std::numeric_limits<double>::quiet_NaN(); //MWe-hr
}

int C_csp_lf_dsg_collector_receiver::C_mono_eq_freeze_prot_E_bal::operator()(double T_cold_in /*K*/, double *E_loss_balance /*-*/)
{
	// Call energy balance with updated timestep and temperature info
	int exit_code = mpc_dsg_lf->once_thru_loop_energy_balance_T_t_int(ms_weather, T_cold_in, m_P_field_out,
							m_m_dot_loop, m_h_sca_out_target, ms_sim_info);

	if( exit_code != E_loop_energy_balance_exit::SOLVED )
	{
		*E_loss_balance = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	// Calculate energy added to water
	m_Q_fp = m_m_dot_loop*(double)mpc_dsg_lf->m_nLoops*(mpc_dsg_lf->mc_sys_cold_in_t_int.m_enth-mpc_dsg_lf->mc_sys_hot_out_t_int.m_enth)/1.E3*ms_sim_info.ms_ts.m_step;	//[MJ]

	// Set the normalized difference between the Field Energy Loss and Freeze Protection Energy
    double Q_field_loss_rel = std::max(0.01 * mpc_dsg_lf->m_q_dot_loss_tot_des / 1.E3 * ms_sim_info.ms_ts.m_step, mpc_dsg_lf->m_Q_field_losses_total);
    *E_loss_balance = (m_Q_fp - mpc_dsg_lf->m_Q_field_losses_total) / Q_field_loss_rel;     // mpc_dsg_lf->m_Q_field_losses_total;		//[-]

    return 0;
}

double C_csp_lf_dsg_collector_receiver::od_pressure(double m_dot_loop /*kg/s*/)
{
	return check_pressure.P_check(turb_pres_frac(m_dot_loop*(double)m_nLoops / m_m_dot_des, m_fossil_mode, 0.0, m_fP_turb_min)*m_P_turb_des);
}

int C_csp_lf_dsg_collector_receiver::C_mono_eq_h_loop_out_target::operator()(double m_dot_loop /*kg/s*/, double *diff_h_loop_out /*kJ/kg*/)
{
	// Need to recalculate pressure, and therefore target enthalpy
		// Calculate the field outlet pressure as a function of mass flow rate [bar]
	m_P_field_out = mpc_dsg_lf->od_pressure(m_dot_loop);	//[bar]
	// Calculate the target outlet enthalpy
	int wp_code = 0;
	m_h_sca_out_target = std::numeric_limits<double>::quiet_NaN();
	if( mpc_dsg_lf->m_is_sh_target )
	{
		wp_code = water_TP(mpc_dsg_lf->m_T_field_out_des, m_P_field_out*100.0, &mpc_dsg_lf->wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_TP error", wp_code));
		}
		m_h_sca_out_target = mpc_dsg_lf->wp.enth;	//[kJ/kg]
	}
	else
	{
		wp_code = water_PQ(m_P_field_out*100.0, mpc_dsg_lf->m_x_b_des, &mpc_dsg_lf->wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_PQ error", wp_code));
		}
		m_h_sca_out_target = mpc_dsg_lf->wp.enth;	//[kJ/kg]
	}

	int exit_code = mpc_dsg_lf->once_thru_loop_energy_balance_T_t_int(ms_weather, m_T_cold_in, m_P_field_out,
									m_dot_loop, m_h_sca_out_target, ms_sim_info);

	if( exit_code != E_loop_energy_balance_exit::SOLVED )
	{
		*diff_h_loop_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	// Set the relative difference between calculated and target sca outlet enthalpies
	*diff_h_loop_out = (mpc_dsg_lf->mc_sca_out_t_end[mpc_dsg_lf->m_nModTot-1].m_enth - m_h_sca_out_target)/m_h_sca_out_target;	//[-]

	return 0;
}

int C_csp_lf_dsg_collector_receiver::freeze_protection(const C_csp_weatherreader::S_outputs &weather, double P_field_out /*bar*/,
	double T_cold_in /*K*/, double m_dot_loop /*kg/s*/, double h_sca_out_target /*kJ/kg*/,
	const C_csp_solver_sim_info &sim_info_temp, double & Q_fp /*MJ*/)
{
	C_mono_eq_freeze_prot_E_bal c_freeze_protection_eq(this, weather, P_field_out, m_dot_loop, h_sca_out_target, sim_info_temp);
	C_monotonic_eq_solver c_fp_solver(c_freeze_protection_eq);

	// Set upper and lower bounds on independent variable: T_cold_in
	double T_cold_in_lower = T_cold_in;		//[K]
	int wp_code = water_PQ(P_field_out*100.0, 0.5, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::freeze protection find Boiling Temperature", "water_PQ error", wp_code));
	}
	double T_cold_in_upper = wp.temp - 1.0;		//[K]

	// Set two initial guess values
	double q_dot_field_losses_tot = m_Q_field_losses_total / sim_info_temp.ms_ts.m_step*1.E3;			//[kWt]
	double h_guess_lower = h_sca_out_target + q_dot_field_losses_tot / ((double)m_nLoops*m_dot_loop);	//[kJ/kg]
	wp_code = water_PH(P_field_out*100.0, h_guess_lower, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::freeze protection initial guess", "water_PH error", wp_code));
	}
	double T_guess_lower = wp.temp;		//[K]
	double T_guess_upper = T_guess_lower + 10.0;	//[K]

	// Set solver settings - monotonic equation is calculating an eror, so want to get to 0
	c_fp_solver.settings(0.01, 30, T_cold_in_lower, T_cold_in_upper, false);

	int iter_solved = -1;
	double tol_solved = std::numeric_limits<double>::quiet_NaN();

	int fp_code = 0;
	double T_cold_in_solved = std::numeric_limits<double>::quiet_NaN();

	try
	{
		fp_code = c_fp_solver.solve(T_guess_lower, T_guess_upper, 0.0, T_cold_in_solved, tol_solved, iter_solved);
	}
	catch( C_csp_exception & )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::off - freeze protection failed"));
	}

    if (!(fp_code >= C_monotonic_eq_solver::CONVERGED && std::fabs(tol_solved) < 10.0))
    {
        throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::off - freeze protection failed to converge"));
    }

	Q_fp = c_freeze_protection_eq.m_Q_fp;		//[MJ]

	return 0;
}

void C_csp_lf_dsg_collector_receiver::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// Always reset last temps
	reset_last_temps();

	// Get optical properties
		// Should reflect that the collector is not tracking and probably (but not necessarily) DNI = 0
	loop_optical_eta_off();

	// Set mass flow rat to minimum allowable
	double m_dot_loop = m_m_dot_min;	//[kg/s]

	// Not varying mass flow rate, so calculate the field outlet pressure [bar]
	double P_field_out = check_pressure.P_check(turb_pres_frac(m_dot_loop*(double)m_nLoops / m_m_dot_des, m_fossil_mode, 0.0, m_fP_turb_min)*m_P_turb_des);

	// Set duration for recirculation timestep
	if(m_step_recirc != m_step_recirc)
		m_step_recirc = 10.0*60.0;		//[s]

	// Calculate number of steps required given timestep from solver and recirculation step
	int n_steps_recirc = (int)std::ceil(sim_info.ms_ts.m_step / m_step_recirc);	//[-]

	// Define a copy of the sim_info structure
	double time_start = sim_info.ms_ts.m_time - sim_info.ms_ts.m_step;	//[s] Time at start of step
	double step_local = sim_info.ms_ts.m_step / (double)n_steps_recirc;	//[s] Recirculation time step

	// Create local sim_info structure to handle recirculation timesteps
	C_csp_solver_sim_info sim_info_temp = sim_info;
	sim_info_temp.ms_ts.m_step = step_local;		//[s]

	double Q_fp_sum = 0.0;						//[MJ]

	// Zero full timestep outputs
	m_h_sys_c_in_t_int_fullts = m_P_sys_c_in_t_int_fullts =
		m_h_c_rec_in_t_int_fullts = m_P_c_rec_in_t_int_fullts =
		m_h_h_rec_out_t_int_fullts = m_P_h_rec_out_t_int_fullts =
		m_h_sys_h_out_t_int_fullts = m_P_sys_h_out_t_int_fullts = 0.0;

	m_q_dot_sca_loss_summed_fullts = m_q_dot_sca_abs_summed_fullts =
		m_q_dot_HR_cold_loss_fullts = m_q_dot_HR_hot_loss_fullts =
		m_E_dot_sca_summed_fullts =
		m_q_dot_to_sink_fullts =
		0.0;

	for(int i = 0; i < n_steps_recirc; i++)
	{
		sim_info_temp.ms_ts.m_time = time_start + step_local*(i+1);	//[s]

		// Could iterate here for each step such that T_cold_in = mc_sys_hot_out_t_int.m_temp
		//    This would significantly slow the code

			// Set inlet temperature to previous timestep outlet temperature
			double T_cold_in = mc_sys_hot_out_t_end_last.m_temp;	//[K]
            double T_cold_in_base = mc_sys_hot_out_t_end_last.m_temp;	//[K]

			// Recirculating, so the target enthalpy is roughly the outlet enthalpy
            int wp_code = 0;
            do {
                water_TP(T_cold_in, P_field_out * 100.0, &wp);
                if (wp_code != 0)
                {
                    throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::off", "water_TP error", wp_code));
                }
                if (wp.qual > 0.0){
                    T_cold_in -= 1.0;
                }
                else{
                    break;
                }
            } while (T_cold_in > T_cold_in_base - 5.0);

			if( wp.qual > 0.0 )
			{
				throw(C_csp_exception("The inlet to the once thru loop off mode, pre-pump, is 2-phase, this is not good"));
			}
			double h_target = wp.enth;	//[kJ/kg]


			// Call energy balance with updated timestep and temperature info
			once_thru_loop_energy_balance_T_t_int(weather, T_cold_in, P_field_out, m_dot_loop, h_target, sim_info_temp);

		// This iteration would end here, and step forward

		// Check if freeze protection is required
		if( mc_sca_out_t_int[m_nModTot-1].m_temp < m_T_fp + 10.0 )
		{
			if(m_Q_field_losses_total > 0.0)
			{
				double Q_fp_i = std::numeric_limits<double>::quiet_NaN();
				int fp_code = freeze_protection(weather, P_field_out, T_cold_in, m_dot_loop, h_target, sim_info_temp, Q_fp_i);

				Q_fp_sum += Q_fp_i;		//[MJ]
			}
		}

		// Add subtimestep calcs
		m_h_sys_c_in_t_int_fullts += mc_sys_cold_in_t_int.m_enth;		//[kJ/kg]
		m_P_sys_c_in_t_int_fullts += mc_sys_cold_in_t_int.m_pres;		//[bar]

		m_h_c_rec_in_t_int_fullts += mc_sca_in_t_int[0].m_enth;			//[kJ/kg]
		m_P_c_rec_in_t_int_fullts += mc_sca_in_t_int[0].m_pres;			//[bar]

		m_h_h_rec_out_t_int_fullts += mc_sca_out_t_int[m_nModTot-1].m_enth;	//[kJ/kg]
		m_P_h_rec_out_t_int_fullts += mc_sca_out_t_int[m_nModTot-1].m_pres;	//[bar]

		m_h_sys_h_out_t_int_fullts += mc_sys_hot_out_t_int.m_enth;		//[kJ/kg]
		m_P_sys_h_out_t_int_fullts += mc_sys_hot_out_t_int.m_pres;		//[bar]

		m_q_dot_sca_loss_summed_fullts += m_q_dot_sca_loss_summed_subts;	//[MWt]
		m_q_dot_sca_abs_summed_fullts += m_q_dot_sca_abs_summed_subts;		//[MWt]
		m_q_dot_HR_cold_loss_fullts += m_q_dot_HR_cold_loss_subts;			//[MWt]
		m_q_dot_HR_hot_loss_fullts += m_q_dot_HR_hot_loss_subts;			//[MWt]
		m_E_dot_sca_summed_fullts += m_E_dot_sca_summed_subts;				//[MWt]

		m_q_dot_to_sink_fullts += m_q_dot_to_sink_subts;		//[MWt]

		update_last_temps();
	}

	// Calculate average value over all subtimesteps
	double nd_steps_recirc = (double)n_steps_recirc;

	m_h_sys_c_in_t_int_fullts /= nd_steps_recirc;		//[kJ/kg]
	m_P_sys_c_in_t_int_fullts /= nd_steps_recirc;		//[bar]

	m_h_c_rec_in_t_int_fullts /= nd_steps_recirc;		//[kJ/kg]
	m_P_c_rec_in_t_int_fullts /= nd_steps_recirc;		//[bar]

	m_h_h_rec_out_t_int_fullts /= nd_steps_recirc;		//[kJ/kg]
	m_P_h_rec_out_t_int_fullts /= nd_steps_recirc;		//[bar]

	m_h_sys_h_out_t_int_fullts /= nd_steps_recirc;		//[kJ/kg]
	m_P_sys_h_out_t_int_fullts /= nd_steps_recirc;		//[bar]

	m_q_dot_sca_loss_summed_fullts /= nd_steps_recirc;		//[MWt]
	m_q_dot_sca_abs_summed_fullts /= nd_steps_recirc;		//[MWt]
	m_q_dot_HR_cold_loss_fullts /= nd_steps_recirc;			//[MWt]
	m_q_dot_HR_hot_loss_fullts /= nd_steps_recirc;			//[MWt]
	m_E_dot_sca_summed_fullts /= nd_steps_recirc;			//[MWt]

	m_q_dot_to_sink_fullts /= nd_steps_recirc;		//[MWt]

	//double E_bal_check = m_q_dot_sca_abs_summed_fullts - m_q_dot_HR_cold_loss_fullts - m_q_dot_HR_hot_loss_fullts -
	//					m_q_dot_to_sink_fullts - m_E_dot_sca_summed_fullts;

	m_q_dot_freeze_protection = Q_fp_sum / sim_info.ms_ts.m_step;	//[MWt]

	// Find average enthalpy over recirculation timesteps
	int wp_code = water_PH(P_field_out*100.0, m_h_sys_h_out_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::off::recirculation", "water_PH error", wp_code));
	}
	double T_sys_hot_out_t_int_ts_ave = wp.temp;	//[K]

	// Set outputs
	cr_out_solver.m_q_startup = 0.0;								//[MWt-hr] Receiver thermal output used to warm up the receiver
	cr_out_solver.m_time_required_su = sim_info.ms_ts.m_step;		//[s] Time required for receiver to startup - at least the entire timestep because it's off
	cr_out_solver.m_m_dot_salt_tot = m_dot_loop*3600.0*(double)m_nLoops;	//[kg/hr] SYSTEM mass flow rate
	cr_out_solver.m_q_thermal = 0.0;								//[MWt] No available receiver thermal output
	cr_out_solver.m_T_salt_hot = T_sys_hot_out_t_int_ts_ave - 273.15;	//[C] Average timestep field outlet temperature
	cr_out_solver.m_component_defocus = 1.0;		//[-]

	cr_out_solver.m_E_fp_total = m_q_dot_freeze_protection;		//[MWt]
	cr_out_solver.m_W_dot_col_tracking = m_W_dot_sca_tracking;	//[MWe]
	cr_out_solver.m_W_dot_htf_pump = m_W_dot_pump;				//[MWe]

	cr_out_solver.m_standby_control = -1;
	cr_out_solver.m_dP_sf_sh = 0.0;
	cr_out_solver.m_h_htf_hot = m_h_sys_h_out_t_int_fullts;		//[kJ/kg]
	cr_out_solver.m_xb_htf_hot = wp.qual;						//[-]
    cr_out_solver.m_P_htf_hot = m_P_sys_h_out_t_int_fullts * 100.0; //[kPa] convert from bar

	m_operating_mode = C_csp_collector_receiver::OFF;		//[-]

	set_output_values();

	return;
}


void C_csp_lf_dsg_collector_receiver::startup(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// Always reset last temps
	reset_last_temps();

	// Get optical properties
	loop_optical_eta(weather, sim_info);

	// Set mass flow rate to what I imagine might be an appropriate value
	double m_dot_loop = m_m_dot_min;	//[kg/s]
	if( weather.m_beam * m_eta_opt > 30.0 && mc_sca_out_t_end_converged[m_nModTot-1].m_temp > (0.5*m_T_fp + 0.5*m_T_startup) )
	{
		double m_dot_ss = (weather.m_beam * m_eta_opt) /
			(m_I_bn_des * m_opt_eta_des) * m_m_dot_loop_des;
		m_dot_loop = min( m_m_dot_max, max(m_m_dot_min, 0.8*m_dot_ss + 0.2*m_m_dot_min) );	//[kg/s]
	}

	// Not varying mass flow rate, so calculate the field outlet pressure [bar]
	double P_field_out = check_pressure.P_check(turb_pres_frac(m_dot_loop*(double)m_nLoops / m_m_dot_des, m_fossil_mode, 0.0, m_fP_turb_min)*m_P_turb_des);

	// Set duration for recirculation timestep
	if( m_step_recirc != m_step_recirc )
		m_step_recirc = 10.0*60.0;		//[s]

	// Calculate number of steps required given timestep from solver and recirculation step
	int n_steps_recirc = (int)std::ceil(sim_info.ms_ts.m_step / m_step_recirc);	//[-]

	// Define a copy of the sim_info structure
	double time_start = sim_info.ms_ts.m_time - sim_info.ms_ts.m_step;	//[s] Time at start of step
    double time_end = sim_info.ms_ts.m_time;    //[s]

	// Create local sim_info structure to handle recirculation timesteps
	C_csp_solver_sim_info sim_info_temp = sim_info;

	bool is_T_startup_achieved = false;

	// This code finds the first "Recirculation Step" when the outlet temperature is greater than the Startup Temperature
	double time_required_su = sim_info.ms_ts.m_step;		//[s]

	double Q_fp_sum = 0.0;						//[MJ]

	// Zero full timestep outputs
	m_h_sys_c_in_t_int_fullts = m_P_sys_c_in_t_int_fullts =
		m_h_c_rec_in_t_int_fullts = m_P_c_rec_in_t_int_fullts =
		m_h_h_rec_out_t_int_fullts = m_P_h_rec_out_t_int_fullts =
		m_h_sys_h_out_t_int_fullts = m_P_sys_h_out_t_int_fullts = 0.0;

	m_q_dot_sca_loss_summed_fullts = m_q_dot_sca_abs_summed_fullts =
		m_q_dot_HR_cold_loss_fullts = m_q_dot_HR_hot_loss_fullts =
		m_E_dot_sca_summed_fullts =
		m_q_dot_to_sink_fullts =
		0.0;

    sim_info_temp.ms_ts.m_time = time_start;    //[s]
    while (sim_info_temp.ms_ts.m_time < time_end)
    {
        sim_info_temp.ms_ts.m_time_start = sim_info_temp.ms_ts.m_time;      //[s]
        sim_info_temp.ms_ts.m_time = std::min(sim_info_temp.ms_ts.m_time_start + m_step_recirc, time_end);  //[s]
        sim_info_temp.ms_ts.m_step = sim_info_temp.ms_ts.m_time - sim_info_temp.ms_ts.m_time_start;     //[s]

		// Could iterate here for each step such that T_cold_in = mc_sys_hot_out_t_int.m_temp
		//    This would significantly slow the code

			// Set inlet temperature to previous timestep outlet temperature
			double T_cold_in = mc_sys_hot_out_t_end_last.m_temp;	//[K]

            double T_cold_in_base = mc_sys_hot_out_t_end_last.m_temp;	//[K]

            // Recirculating, so the target enthalpy is roughly the outlet enthalpy
            int wp_code = 0;
            do {
                water_TP(T_cold_in, P_field_out * 100.0, &wp);
                if (wp_code != 0)
                {
                    throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::off", "water_TP error", wp_code));
                }
                if (wp.qual > 0.0) {
                    T_cold_in -= 1.0;
                }
                else {
                    break;
                }
            } while (T_cold_in > T_cold_in_base - 5.0);

            if (wp.qual > 0.0)
            {
                throw(C_csp_exception("The inlet to the once thru loop off mode, pre-pump, is 2-phase, this is not good"));
            }
            double h_target = wp.enth;	//[kJ/kg]

			// Call energy balance with updated timestep and temperature info
			once_thru_loop_energy_balance_T_t_int(weather, T_cold_in, P_field_out, m_dot_loop, h_target, sim_info_temp);

		// This iteration would end here, and step forward

		// Check if freeze protection is required
		if( mc_sca_out_t_int[m_nModTot - 1].m_temp < m_T_fp + 10.0 )
		{
			if(m_Q_field_losses_total > 0.0)
			{
				double Q_fp_i = std::numeric_limits<double>::quiet_NaN();
				int fp_code = freeze_protection(weather, P_field_out, T_cold_in, m_dot_loop, h_target, sim_info_temp, Q_fp_i);

				Q_fp_sum += Q_fp_i;		//[MJ]
			}
		}

		// Add subtimestep calcs
		m_h_sys_c_in_t_int_fullts += mc_sys_cold_in_t_int.m_enth*sim_info_temp.ms_ts.m_step;		//[kJ/kg]
		m_P_sys_c_in_t_int_fullts += mc_sys_cold_in_t_int.m_pres*sim_info_temp.ms_ts.m_step;		//[bar]

		m_h_c_rec_in_t_int_fullts += mc_sca_in_t_int[0].m_enth*sim_info_temp.ms_ts.m_step;			//[kJ/kg]
		m_P_c_rec_in_t_int_fullts += mc_sca_in_t_int[0].m_pres*sim_info_temp.ms_ts.m_step;			//[bar]

		m_h_h_rec_out_t_int_fullts += mc_sca_out_t_int[m_nModTot - 1].m_enth*sim_info_temp.ms_ts.m_step;	//[kJ/kg]
		m_P_h_rec_out_t_int_fullts += mc_sca_out_t_int[m_nModTot - 1].m_pres*sim_info_temp.ms_ts.m_step;	//[bar]

		m_h_sys_h_out_t_int_fullts += mc_sys_hot_out_t_int.m_enth*sim_info_temp.ms_ts.m_step;		//[kJ/kg]
		m_P_sys_h_out_t_int_fullts += mc_sys_hot_out_t_int.m_pres*sim_info_temp.ms_ts.m_step;		//[bar]

		m_q_dot_sca_loss_summed_fullts += m_q_dot_sca_loss_summed_subts*sim_info_temp.ms_ts.m_step;	//[MWt]
		m_q_dot_sca_abs_summed_fullts += m_q_dot_sca_abs_summed_subts*sim_info_temp.ms_ts.m_step;		//[MWt]
		m_q_dot_HR_cold_loss_fullts += m_q_dot_HR_cold_loss_subts*sim_info_temp.ms_ts.m_step;			//[MWt]
		m_q_dot_HR_hot_loss_fullts += m_q_dot_HR_hot_loss_subts*sim_info_temp.ms_ts.m_step;			//[MWt]
		m_E_dot_sca_summed_fullts += m_E_dot_sca_summed_subts*sim_info_temp.ms_ts.m_step;				//[MWt]

		m_q_dot_to_sink_fullts += m_q_dot_to_sink_subts*sim_info_temp.ms_ts.m_step;		//[MWt]

		// If the *outlet temperature at the end of the timestep* is greater than the startup temperature
		if( mc_sys_hot_out_t_end.m_temp > m_T_startup )
		{
			time_required_su = sim_info_temp.ms_ts.m_time - time_start;		//[s]
			m_operating_mode = C_csp_collector_receiver::ON;
			is_T_startup_achieved = true;
			break;
		}

		update_last_temps();
	}

    // Check if startup is achieved in current controller/kernel timestep
    if (!is_T_startup_achieved)
    {
        time_required_su = sim_info.ms_ts.m_step;		//[s]
        m_operating_mode = C_csp_collector_receiver::STARTUP;	//[-]
    }

	m_h_sys_c_in_t_int_fullts /= time_required_su;		//[kJ/kg]
	m_P_sys_c_in_t_int_fullts /= time_required_su;		//[bar]

	m_h_c_rec_in_t_int_fullts /= time_required_su;		//[kJ/kg]
	m_P_c_rec_in_t_int_fullts /= time_required_su;		//[bar]

	m_h_h_rec_out_t_int_fullts /= time_required_su;		//[kJ/kg]
	m_P_h_rec_out_t_int_fullts /= time_required_su;		//[bar]

	m_h_sys_h_out_t_int_fullts /= time_required_su;		//[kJ/kg]
	m_P_sys_h_out_t_int_fullts /= time_required_su;		//[bar]

	m_q_dot_sca_loss_summed_fullts /= time_required_su;		//[MWt]
	m_q_dot_sca_abs_summed_fullts /= time_required_su;		//[MWt]
	m_q_dot_HR_cold_loss_fullts /= time_required_su;			//[MWt]
	m_q_dot_HR_hot_loss_fullts /= time_required_su;			//[MWt]
	m_E_dot_sca_summed_fullts /= time_required_su;			//[MWt]

	m_q_dot_to_sink_fullts /= time_required_su;

	//double E_bal_check = m_q_dot_sca_abs_summed_fullts - m_q_dot_HR_cold_loss_fullts - m_q_dot_HR_hot_loss_fullts -
	//	m_q_dot_to_sink_fullts - m_E_dot_sca_summed_fullts;

	m_q_dot_freeze_protection = Q_fp_sum / time_required_su;	//[MWt]

	int wp_code = water_PH(P_field_out*100.0, m_h_sys_h_out_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::startup::recirculation", "water_PH error", wp_code));
	}
	double T_sys_hot_out_t_int_ts_ave = wp.temp;	//[K]

		// For now, set startup > 0.0 so that the controller knows that startup was successful
	cr_out_solver.m_q_startup = 1.0;			//[MWt-hr] Receiver thermal output used to warm up the receiver

	cr_out_solver.m_time_required_su = time_required_su;	//[s]
		// For now, controller doesn't use mass flow rate
	cr_out_solver.m_m_dot_salt_tot = m_dot_loop*(double)m_nLoops*3600.0;	//[kg/hr]
		// No thermal output if receiver is in startup
	cr_out_solver.m_q_thermal = 0.0;
	cr_out_solver.m_T_salt_hot = T_sys_hot_out_t_int_ts_ave - 273.15;		//[C]
	cr_out_solver.m_component_defocus = 1.0;

		// Shouldn't need freeze protection if in startup, but may want a check on this
	cr_out_solver.m_E_fp_total = m_q_dot_freeze_protection;		//[MWt]
	cr_out_solver.m_W_dot_col_tracking = m_W_dot_sca_tracking;	//[MWe]
	cr_out_solver.m_W_dot_htf_pump = m_W_dot_pump;				//[MWe]

	cr_out_solver.m_standby_control = -1;
	cr_out_solver.m_dP_sf_sh = 0.0;
	cr_out_solver.m_h_htf_hot = m_h_sys_h_out_t_int_fullts;		//[kJ/kg]
	cr_out_solver.m_xb_htf_hot = wp.qual;						//[-]
    cr_out_solver.m_P_htf_hot = m_P_sys_h_out_t_int_fullts * 100.0; //[kPa] convert from bar

	set_output_values();

	return;
}

void C_csp_lf_dsg_collector_receiver::apply_component_defocus(double defocus /*-*/)
{
	// Store component defocus
	m_component_defocus = defocus;

	// Calculate the design-point incident energy on each module for a single loop
	for( int i = 0; i < m_nModTot; i++ )
	{
		m_q_inc[i] = defocus * m_q_inc[i];					//[kWt] Incident thermal power after defocus but before optical losses
		m_q_rec[i] = defocus * m_q_rec_control_df[i];		//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*
	}
}

int C_csp_lf_dsg_collector_receiver::C_mono_eq_defocus::operator()(double defocus /*-*/, double *diff_h_loop_out /*kJ/kg*/)
{
	// Apply the component defocus to calculate a new m_q_rec
	mpc_dsg_lf->apply_component_defocus(defocus);

	// Solve the once through loop energy balance
	int exit_code = mpc_dsg_lf->once_thru_loop_energy_balance_T_t_int(ms_weather, m_T_cold_in, m_P_field_out,
										m_m_dot_loop, m_h_sca_out_target, ms_sim_info);

	if( exit_code != E_loop_energy_balance_exit::SOLVED )
	{
		*diff_h_loop_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	// Set the relative difference between calculated and target sca outlet enthalpies
	*diff_h_loop_out = (mpc_dsg_lf->mc_sca_out_t_end[mpc_dsg_lf->m_nModTot - 1].m_enth - m_h_sca_out_target) / m_h_sca_out_target;	//[-]

	return 0;
}

void C_csp_lf_dsg_collector_receiver::on(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	double field_control,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// Always reset last temps
	reset_last_temps();

	// Get optical performance (no defocus applied yet...)
	// This calculates member data m_q_inc[] with NO defocus applied
	loop_optical_eta(weather, sim_info);

	// After loop optical efficiency, reset defocus variables
	m_control_defocus = field_control;

	// If Control Defocus: field_control < 1, then apply it here
	if( field_control < 1.0 )
	{
		// Calculate the design-point incident energy on each module for a single loop
		for( int i = 0; i < m_nModTot; i++ )
		{
			int gset = 0;
			if( (i >= m_nModBoil) && (m_is_multgeom) )
				gset = 1;
			else
				gset = 0;

			m_q_inc_control_df[i] = field_control * m_q_inc[i];
			m_q_rec_control_df[i] = m_q_inc_control_df[i] * m_opteff_des.at(gset, 0);	//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*
		}

		m_q_inc = m_q_inc_control_df;
		m_q_rec = m_q_rec_control_df;	//[kWt]
	}
	else if(field_control == 1.0)
	{
		// If no CONTROL defocus, then baseline against the vector return by 'loop_optical_eta'
		m_q_inc_control_df = m_q_inc;		//[kWt]
		m_q_rec_control_df = m_q_rec;		//[kWt]
	}
	else
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector::on(...) received a CONTROL defocus > 1.0, "
			"and that is not ok!"));
	}

	// Solve the loop energy balance at the minimum mass flow rate
		// Set mass flow rate to the minimum allowable
	double m_dot_loop = m_m_dot_min;		//[kg/s]
		// Calculate the field outlet pressure as a function of mass flow rate [bar]
	double P_field_out = check_pressure.P_check(turb_pres_frac(m_dot_loop*(double)m_nLoops / m_m_dot_des, m_fossil_mode, 0.0, m_fP_turb_min)*m_P_turb_des);
		// Calculate the target outlet enthalpy
	int wp_code = 0;
	double h_sca_out_target = std::numeric_limits<double>::quiet_NaN();
	if( m_is_sh_target )
	{
		wp_code = water_TP(m_T_field_out_des, P_field_out*100.0, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_TP error", wp_code));
		}
		h_sca_out_target = wp.enth;	//[kJ/kg]
	}
	else
	{
		wp_code = water_PQ(P_field_out*100.0, m_x_b_des, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_PQ error", wp_code));
		}
		h_sca_out_target = wp.enth;	//[kJ/kg]
	}

		// Get inlet condition from input argument
	double T_cold_in = htf_state_in.m_temp + 273.15;	//[K]
		// Call energy balance with updated info
	int balance_code = once_thru_loop_energy_balance_T_t_int(weather, T_cold_in, P_field_out, m_dot_loop, h_sca_out_target, sim_info);

	bool on_success = true;

	if( balance_code != E_loop_energy_balance_exit::SOLVED )
	{
		on_success = false;
	}

	// If the outlet enthalpy (of final SCA in loop!) is greater than the target (considering some convergence tolerance)
		// then adjust mass flow rate and see what happens
	if( (mc_sca_out_t_end[m_nModTot-1].m_enth - h_sca_out_target)/h_sca_out_target > 0.001 && on_success )
	{
		// Next, try the maximum mass flow rate
		m_dot_loop = m_m_dot_max;		//[kg/s]

		// Need to recalculate pressure, and therefore target enthalpy
			// Calculate the field outlet pressure as a function of mass flow rate [bar]
		P_field_out = check_pressure.P_check(turb_pres_frac(m_dot_loop*(double)m_nLoops / m_m_dot_des, m_fossil_mode, 0.0, m_fP_turb_min)*m_P_turb_des);
		// Calculate the target outlet enthalpy
		wp_code = 0;
		h_sca_out_target = std::numeric_limits<double>::quiet_NaN();
		if( m_is_sh_target )
		{
			wp_code = water_TP(m_T_field_out_des, P_field_out*100.0, &wp);
			if( wp_code != 0 )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_TP error", wp_code));
			}
			h_sca_out_target = wp.enth;	//[kJ/kg]
		}
		else
		{
			wp_code = water_PQ(P_field_out*100.0, m_x_b_des, &wp);
			if( wp_code != 0 )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::init design point outlet state point calcs failed", "water_PQ error", wp_code));
			}
			h_sca_out_target = wp.enth;	//[kJ/kg]
		}

		// Call energy balance with updated info
		int balance_code = once_thru_loop_energy_balance_T_t_int(weather, T_cold_in, P_field_out, m_dot_loop, h_sca_out_target, sim_info);

		if( balance_code != E_loop_energy_balance_exit::SOLVED )
		{
			on_success = false;
		}

		// Is the outlet enthalpy (of the final SCA in the loop!) greater than the target (considering some convergence tolerance)?
			// then need to defocus
		if( (mc_sca_out_t_end[m_nModTot - 1].m_enth - h_sca_out_target) / h_sca_out_target > 0.001 && on_success )
		{
			C_mono_eq_defocus c_defocus_eq(this, weather, T_cold_in, P_field_out, m_dot_loop, h_sca_out_target, sim_info);
			C_monotonic_eq_solver c_defocus_solver(c_defocus_eq);

			// Set upper and lower bounds
			double defocus_upper = 1.0;		//[-]
			double defocus_lower = 0.0;		//[-]

			// Generate guess values
			double defocus_guess_upper = min(1.0, (h_sca_out_target - mc_sys_cold_in_t_int.m_enth)/(mc_sca_out_t_end[m_nModTot - 1].m_enth - mc_sys_cold_in_t_int.m_enth));
			double defocus_guess_lower = 0.9*defocus_guess_upper;

			// Set solver settings
			c_defocus_solver.settings(0.001, 100, defocus_lower, defocus_upper, false);

			int defocus_iter = -1;
			double defocus_tol_solved = std::numeric_limits<double>::quiet_NaN();

			int defocus_code = 0;
			double defocus_solved = 1.0;
			try
			{
				defocus_code = c_defocus_solver.solve(defocus_guess_lower, defocus_guess_upper, 0.0,
													defocus_solved, defocus_tol_solved, defocus_iter);
			}
			catch( C_csp_exception &  )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector::on(...) COMPONENT defocus method reached exception."));
				on_success = false;
			}

			if( defocus_code != C_monotonic_eq_solver::CONVERGED )
			{
				if( defocus_tol_solved == defocus_tol_solved && defocus_tol_solved < 0.3 )
				{
					double blah = 1.23;
				}
				else
				{
					throw(C_csp_exception("C_csp_lf_dsg_collector::on(...) COMPONENT defocus failed to converge"));
				}
			}
		}
		else if(on_success)
		{
			// The mass flow rate resulting in the target outlet enthalpy is somewhere between the min and max. Iterate!

			// Mass flow rate is changing, so solver calculates and stores:
			//   * P_field_out
			//   * h_sca_out_target

			C_mono_eq_h_loop_out_target c_h_out_target(this, weather, T_cold_in, sim_info);
			C_monotonic_eq_solver c_h_out_target_solver(c_h_out_target);

			// Set upper and lower bounds
			double m_dot_upper = m_m_dot_max;		//[kg/s]
			double m_dot_lower = m_m_dot_min;		//[kg/s]

			// Set guess values... can be smarter about this, maybe
			double m_dot_guess_upper = 0.75*m_dot_upper + 0.25*m_dot_lower;	//[kg/s]
			double m_dot_guess_lower = 0.25*m_dot_upper + 0.75*m_dot_lower;	//[kg/s]

			// Set solver settings
			c_h_out_target_solver.settings(0.001, 30, m_dot_lower, m_dot_upper, false);

			int iter_solved = -1;
			double tol_solved = std::numeric_limits<double>::quiet_NaN();

			int m_dot_code = 0;
			try
			{
				m_dot_code = c_h_out_target_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
													m_dot_loop, tol_solved, iter_solved);
			}
			catch( C_csp_exception & )
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::on(...) mass flow rate iteration failed."));
				on_success = false;
			}

			if( m_dot_code != C_monotonic_eq_solver::CONVERGED )
			{
                if (m_dot_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.3)
                {
                    std::string error_msg = util::format("At time = %lg the iteration to find the steam mass flow rate resulting in the target outlet enthalpy only reached a convergence "
                        "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                        sim_info.ms_ts.m_time / 3600.0, tol_solved);
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                }
				else
				{
					throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::on(...) mass flow rate iteration did not"
						"within a relative tolerance of 0.1."));
				}
				//on_success = false;
			}

			// Get pressure and target enthalpy calculated in Solver Class
			h_sca_out_target = c_h_out_target.m_h_sca_out_target;	//[kJ/kg]
			P_field_out = c_h_out_target.m_P_field_out;				//[bar]

		}	// end iteration on mass flow rate to hit target enthalpy

	}
	// Else, we can run at min mass flow without overheating the loop, so we're finished here

	if( on_success )
	{
		// Call final metrics method?
		// (i.e. pressure drops, parasitics...)
		m_h_sys_c_in_t_int_fullts = mc_sys_cold_in_t_int.m_enth;		//[kJ/kg]
		m_P_sys_c_in_t_int_fullts = mc_sys_cold_in_t_int.m_pres;		//[bar]

		m_h_c_rec_in_t_int_fullts = mc_sca_in_t_int[0].m_enth;			//[kJ/kg]
		m_P_c_rec_in_t_int_fullts = mc_sca_in_t_int[0].m_pres;			//[bar]

		m_h_h_rec_out_t_int_fullts = mc_sca_out_t_int[m_nModTot - 1].m_enth;	//[kJ/kg]
		m_P_h_rec_out_t_int_fullts = mc_sca_out_t_int[m_nModTot - 1].m_pres;	//[bar]

		m_h_sys_h_out_t_int_fullts = mc_sys_hot_out_t_int.m_enth;		//[kJ/kg]
		m_P_sys_h_out_t_int_fullts = mc_sys_hot_out_t_int.m_pres;		//[bar]

		m_q_dot_sca_loss_summed_fullts = m_q_dot_sca_loss_summed_subts;		//[MWt]
		m_q_dot_sca_abs_summed_fullts = m_q_dot_sca_abs_summed_subts;		//[MWt]
		m_q_dot_HR_cold_loss_fullts = m_q_dot_HR_cold_loss_subts;			//[MWt]
		m_q_dot_HR_hot_loss_fullts = m_q_dot_HR_hot_loss_subts;				//[MWt]
		m_E_dot_sca_summed_fullts = m_E_dot_sca_summed_subts;				//[MWt]

		m_q_dot_to_sink_fullts = m_q_dot_to_sink_subts;		//[MWt]

		//double E_bal_check = m_q_dot_sca_abs_summed_fullts - m_q_dot_HR_cold_loss_fullts - m_q_dot_HR_hot_loss_fullts -
		//	m_q_dot_to_sink_fullts - m_E_dot_sca_summed_fullts;

		m_q_dot_freeze_protection = 0.0;		//[MWt]

		// Set solver outputs & return
			// Receiver is already on, so the controller is not looking for this value
		cr_out_solver.m_q_startup = 0.0;		//[MWt-hr]
			// Receiver is already on, so the controller is not looking for the required startup time
		cr_out_solver.m_time_required_su = 0.0;	//[s]
			// The controller requires the total mass flow rate from the CR
		cr_out_solver.m_m_dot_salt_tot = m_dot_loop*(double)m_nLoops*3600.0;	//[kg/hr]
			// Thermal power absorbed by steam/water
		cr_out_solver.m_q_thermal = m_dot_loop*(double)m_nLoops*(mc_sys_hot_out_t_int.m_enth - mc_sys_cold_in_t_int.m_enth) / 1.E3;	//[MWt]
			// Outlet temperature (set quality below)
		cr_out_solver.m_T_salt_hot = mc_sys_hot_out_t_int.m_temp - 273.15;
		cr_out_solver.m_component_defocus = m_component_defocus;

		// For now, set parasitic outputs to 0
		cr_out_solver.m_E_fp_total = 0.0;			//[MW]
		cr_out_solver.m_W_dot_col_tracking = m_W_dot_sca_tracking;	//[MWe]
		cr_out_solver.m_W_dot_htf_pump = m_W_dot_pump;				//[MWe]

		cr_out_solver.m_standby_control = -1;		//[-]
		cr_out_solver.m_dP_sf_sh = 0.0;				//[bar]
		cr_out_solver.m_h_htf_hot = mc_sys_hot_out_t_int.m_enth;		//[kJ/kg]
		cr_out_solver.m_xb_htf_hot = mc_sys_hot_out_t_int.m_x;			//[-]
		cr_out_solver.m_P_htf_hot = mc_sys_hot_out_t_int.m_pres*100.0;	//[kPa], convert from bar
	}
	else
	{
		m_h_sys_c_in_t_int_fullts = m_P_sys_c_in_t_int_fullts =
			m_h_c_rec_in_t_int_fullts = m_P_c_rec_in_t_int_fullts =
			m_h_h_rec_out_t_int_fullts = m_P_h_rec_out_t_int_fullts =
			m_h_sys_h_out_t_int_fullts = m_P_sys_h_out_t_int_fullts = 0.0;

		m_q_dot_sca_loss_summed_fullts = m_q_dot_sca_abs_summed_fullts =
			m_q_dot_HR_cold_loss_fullts = m_q_dot_HR_hot_loss_subts =
			m_E_dot_sca_summed_fullts =
			m_q_dot_to_sink_fullts =
			m_q_dot_freeze_protection = 0.0;

		// Solution failed, so tell controller/solver
		cr_out_solver.m_q_startup = 0.0;			//[MWt-hr]
		cr_out_solver.m_time_required_su = 0.0;		//[s]
		cr_out_solver.m_m_dot_salt_tot = 0.0;		//[kg/hr]
		cr_out_solver.m_q_thermal = 0.0;			//[MWt]
		cr_out_solver.m_T_salt_hot = 0.0;			//[C]
		cr_out_solver.m_component_defocus = 1.0;	//[-]

		cr_out_solver.m_E_fp_total = 0.0;			//[MW]
		cr_out_solver.m_W_dot_col_tracking = 0.0;	//[MWe]
		cr_out_solver.m_W_dot_htf_pump = 0.0;		//[MWe]

		cr_out_solver.m_standby_control = -1;		//[-]
		cr_out_solver.m_dP_sf_sh = 0.0;				//[bar]
		cr_out_solver.m_h_htf_hot = 0.0;			//[kJ/kg]
		cr_out_solver.m_xb_htf_hot = 0.0;			//[-]
		cr_out_solver.m_P_htf_hot = 0.0;			//[kPa]
	}

	set_output_values();

	return;
}

void C_csp_lf_dsg_collector_receiver::estimates(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_est_out &est_out,
	const C_csp_solver_sim_info &sim_info)
{
	if( m_operating_mode == C_csp_collector_receiver::ON )
	{
		C_csp_collector_receiver::S_csp_cr_out_solver cr_out_solver;

		on(weather, htf_state_in, 1.0, cr_out_solver, sim_info);

		est_out.m_q_dot_avail = cr_out_solver.m_q_thermal;		//[MWt]
		est_out.m_m_dot_avail = cr_out_solver.m_m_dot_salt_tot;	//[kg/hr]
		est_out.m_T_htf_hot = cr_out_solver.m_T_salt_hot;		//[C]
		est_out.m_q_startup_avail = 0.0;		//[MWt]
	}
	else
	{
		if( weather.m_beam > 1.0 )
		{
			est_out.m_q_startup_avail = 1.0;	//[MWt] Recirculating, so going into startup isn't sig. different than OFF
		}
		else
		{
			est_out.m_q_startup_avail = 0.0;
		}
		est_out.m_q_dot_avail = 0.0;
		est_out.m_m_dot_avail = 0.0;
		est_out.m_T_htf_hot = 0.0;
	}

	return;
}

double C_csp_lf_dsg_collector_receiver::turb_pres_frac(double m_dot_nd, int fmode, double ffrac, double fP_min)
{
	/*Take a mass flow fraction, fossil backup fraction, fossil fill mode, and minimum turbine fraction
	and calculate the corresponding fraction of the design point pressure at which the turbine
	will operate*/
	switch (fmode)
	{
	case 1:			// Backup minimum level - parallel
		return max(fP_min, max(m_dot_nd, ffrac));
	case 2:			// Supplemental Operation - parallel
		return max(fP_min, max(m_dot_nd, min(1.0, m_dot_nd + ffrac)));
	case 3:			// Temperature topping mode - series
		return max(fP_min, m_dot_nd);
	case 4:
		return 1.0;	// IPH case where heat sink is not utilizing pressure for expansion
	default:
		return 0;
	}
}

void C_csp_lf_dsg_collector_receiver::converged()
{
	m_ncall = -1;	//[-]
	// Always reset the m_defocus control at the first call of a timestep
	m_defocus = 1.0;		//[-]

	// Reset the optical efficiency member data
	loop_optical_eta_off();

	for (int i = 0; i < m_nModTot; i++)
		m_T_ave_prev[i] = m_T_ave.at(i, 0);


	// Update '_last' temperatures from temperature solved in loop_energy_balance
	mc_sys_cold_out_t_end_last.m_enth = mc_sys_cold_out_t_end_converged.m_enth = mc_sys_cold_out_t_end.m_enth; //[kJ/kg]
	mc_sys_cold_out_t_end_last.m_pres = mc_sys_cold_out_t_end_converged.m_pres = mc_sys_cold_out_t_end.m_pres; //[bar]
	mc_sys_cold_out_t_end_last.m_temp = mc_sys_cold_out_t_end_converged.m_temp = mc_sys_cold_out_t_end.m_temp; //[K]
	mc_sys_cold_out_t_end_last.m_x    = mc_sys_cold_out_t_end_converged.m_x	= mc_sys_cold_out_t_end.m_x;	   //[-]

	mc_sys_hot_out_t_end_last.m_enth = mc_sys_hot_out_t_end_converged.m_enth = mc_sys_hot_out_t_end.m_enth;		//[kJ/kg]
	mc_sys_hot_out_t_end_last.m_pres = mc_sys_hot_out_t_end_converged.m_pres = mc_sys_hot_out_t_end.m_pres;     //[bar]
	mc_sys_hot_out_t_end_last.m_temp = mc_sys_hot_out_t_end_converged.m_temp = mc_sys_hot_out_t_end.m_temp;     //[K]
	mc_sys_hot_out_t_end_last.m_x    = mc_sys_hot_out_t_end_converged.m_x = mc_sys_hot_out_t_end.m_x;			//[-]

	for( int i = 0; i < m_nModTot; i++ )
	{
		mc_sca_out_t_end_last[i].m_enth = mc_sca_out_t_end_converged[i].m_enth = mc_sca_out_t_end[i].m_enth;	//[kJ/kg]
		mc_sca_out_t_end_last[i].m_pres = mc_sca_out_t_end_converged[i].m_pres = mc_sca_out_t_end[i].m_pres;    //[bar]
		mc_sca_out_t_end_last[i].m_temp = mc_sca_out_t_end_converged[i].m_temp = mc_sca_out_t_end[i].m_temp;    //[K]
		mc_sca_out_t_end_last[i].m_x    = mc_sca_out_t_end_converged[i].m_x = mc_sca_out_t_end[i].m_x;		    //[-]
	}

	// Check that, if trough is ON, if outlet temperature at the end of the timestep is colder than the Startup Temperature
	if( m_operating_mode == ON && mc_sys_hot_out_t_end.m_temp < m_T_startup )
	{
		m_operating_mode = OFF;
	}

	if( m_operating_mode == C_csp_collector_receiver::STEADY_STATE )
	{
		throw(C_csp_exception("Receiver should only be run at STEADY STATE mode for estimating output. It must be run at a different mode before exiting a timestep",
			"Trough converged method"));
	}

	m_operating_mode_converged = m_operating_mode;	//[-]

	mc_reported_outputs.set_timestep_outputs();

	m_W_dot_sca_tracking = 0.0;		//[MWe]

	return;
}

void C_csp_lf_dsg_collector_receiver::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

double C_csp_lf_dsg_collector_receiver::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim)
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_lf_dsg_collector_receiver::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/)
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_lf_dsg_collector_receiver::get_collector_area()
{
	throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::write_output_intervals() is not complete"));


	return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_lf_dsg_collector_receiver::loop_optical_eta(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_sim_info &sim_info)
{
	// First, clear all the values calculated below
	loop_optical_eta_off();

	double I_bn = weather.m_beam;					//[W/m2] Current DNI
	double T_db = weather.m_tdry + 273.15;			//[K] Dry bulb temp, convert from C
	double T_dp = weather.m_tdew + 273.15;			//[K] Dewpoint temp, convert from C
	double P_amb = weather.m_pres*100.0;			//[Pa] Ambient pressure, convert from mbar
	double V_wind = weather.m_wspd;					//[m/s] Ambient windspeed

	double shift = weather.m_shift*0.0174533;				//[rad] Shift in longitude from local standard meridian
	double SolarAz = (weather.m_solazi - 180.0)*0.0174533;	//[rad] Solar azimuth angle, convert to [rad] below
	double SolarZen = weather.m_solzen*0.0174533;;			//[rad] Solar zenith angle

	double time = sim_info.ms_ts.m_time;					//[s]
	double m_dt = sim_info.ms_ts.m_step;					//[s]
	double hour = (double)((int)(time / 3600.0) % 24);

	// Optical calculations
	// Time calculations
	int day_of_year = (int)(time / 3600.0) / 24 + 1;					//[-] Day of year
	// Duffie and Beckman 1.5.3b
	double B = (int)(day_of_year - 1)*2.0*CSP::pi / 365.0;
	// Eqn of time in minutes
	double EOT = 229.2*(0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B) - 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
	// Declination in radians (Duffie and Beckman 1.6.1)
	double Dec = 23.45 * sin(360.0*(284.0 + (double)day_of_year) / 365.0*CSP::pi / 180.0) * CSP::pi / 180.0;	//[rad]
	// Solar Noon and time in hours
	double SolarNoon = 12.0 - ((shift)*180.0 / CSP::pi) / 15.0 - EOT / 60.0;

	// Deploy & stow times in hours
	// Calculations modified by MJW 11/30/2009 to correct bug
	double theta_dep = max(m_theta_dep, 1.E-6);
	double DepHr1 = cos(m_latitude) / tan(theta_dep);
	double DepHr2 = -tan(Dec)*sin(m_latitude) / tan(theta_dep);
	double DepHr3 = CSP::sign(tan(CSP::pi - theta_dep)) * acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1 - DepHr2*DepHr2 + 1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
	double DepTime = SolarNoon + DepHr3;

	double theta_stow = max(m_theta_stow, 1.e-6);
	double StwHr1 = cos(m_latitude) / tan(theta_stow);
	double StwHr2 = -tan(Dec) * sin(m_latitude) / tan(theta_stow);
	double StwHr3 = CSP::sign(tan(CSP::pi - theta_stow))*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1 - StwHr2*StwHr2 + 1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
	double StwTime = SolarNoon + StwHr3;

	// ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
	//double HrA = hour - (step / 3600.0);
	double HrA = hour - (m_dt / 3600.0);
	double HrB = hour;

	double MidTrack = std::numeric_limits<double>::quiet_NaN();
	// Solar field operates
	if ((HrB > DepTime) && (HrA < StwTime))
	{
		// solar field deploys during time period
		if (HrA < DepTime)
		{
			m_ftrack = (HrB - DepTime) / (m_dt / 3600.0);
			MidTrack = HrB - m_ftrack * 0.5 * (m_dt / 3600.0);
		}
		// Solar field stows during time period
		else if (HrB > StwTime)
		{
			m_ftrack = (StwTime - HrA) / (m_dt / 3600.0);
			MidTrack = HrA + m_ftrack * 0.5 * (m_dt / 3600.0);
		}
		// solar field operates during entire period
		else
		{
			m_ftrack = 1.0;
			MidTrack = HrA + 0.5 * (m_dt / 3600.0);
		}
		// solar field doesn't operate
	}
	else
	{
		m_ftrack = 0.0;
		MidTrack = HrA + 0.5 * (m_dt / 3600.0);
	}

	// Maximum wind speed value
	if (V_wind >= m_V_wind_max)
		m_ftrack = 0.0;

	double StdTime = MidTrack;
	double SolarTime = StdTime + ((shift)* 180 / CSP::pi) / 15.0 + EOT / 60.0;
	// hour angle (arc of sun) in radians
	double omega = (SolarTime - 12.0)*15.0*CSP::pi / 180.0;		//[rad]

	if (SolarZen < CSP::pi / 2.0)
	{
		//[rad] Convert the solar angles to collector incidence angles
		CSP::theta_trans(SolarAz, SolarZen, m_ColAz, m_phi_t, m_theta_L);

		for (int i = 0; i < m_n_rows_matrix; i++)
		{
			double Iam_T, Iam_L;
			switch ((int)m_OptCharType.at(i, 0))
			{
			case 1:		//sun position
				//user provides an optical table as a function of solar position
				//m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( b_optical_table.interpolate(SolarAz, min(SolarZen, CSP::pi/2.)), 0.0);
				m_eta_optical[i] = m_eta_opt_fixed.at(i, 0)*max(optical_tables.interpolate(SolarAz, min(SolarZen, CSP::pi / 2.0), i), 0.0);
				break;
			case 2:		//incidence angle table
				//user provides an optical table as a function of collector incidence angles
				//m_eta_optical.at(i,0) = m_eta_opt_fixed.at(i,0)*max( b_optical_table.interpolate(phi_t, max(theta_L, 0.0)), 0.0);
				m_eta_optical[i] = m_eta_opt_fixed.at(i, 0)*max(optical_tables.interpolate(m_phi_t, max(m_theta_L, 0.0), i), 0.0);
				break;
			case 3:		//incidence angle modifier polys
				//Otherwise, calculate the collector incidence angles for the IAM equations
				Iam_T = m_IAM_T.at(i, 0) + m_IAM_T.at(i, 1)*m_phi_t + m_IAM_T.at(i, 2)*pow(m_phi_t, 2) + m_IAM_T.at(i, 3)*pow(m_phi_t, 3) + m_IAM_T.at(i, 4)*pow(m_phi_t, 4);
				Iam_L = m_IAM_L.at(i, 0) + m_IAM_L.at(i, 1)*m_theta_L + m_IAM_L.at(i, 2)*pow(m_theta_L, 2) + m_IAM_L.at(i, 3)*pow(m_theta_L, 3) + m_IAM_L.at(i, 4)*pow(m_theta_L, 4);
				m_eta_optical[i] = m_eta_opt_fixed.at(i, 0) * Iam_T * Iam_L;
				break;
			default:
				//error
				std::string err_msg = util::format("No corresponding optical model. Error in solar angle calculation.");
				throw(C_csp_exception(err_msg, "LF DSG call()"));
				/*message(TCS_ERROR, "No corresponding optical model. Error in solar angle calculation.");
				return -1;*/
			}
			m_eta_optical[i] *= m_ftrack;
		}
	}
	else
	{
		m_eta_optical.fill(0.0);	//[-]
		m_phi_t = CSP::pi / 2.;
		m_theta_L = 0.0;
	}

	evac_tube_model.Update_Timestep_Properties(m_eta_optical);

	m_eta_opt = 0.0;
	for( int i = 0; i < m_nModTot; i++ )
	{
		int gset = 0;
		if( i >= m_nModBoil && m_is_multgeom )
			gset = 1;
		// Calculate the incident energy on each module
		m_q_inc[i] = I_bn*m_A_aperture.at(gset, 0) / 1000.0;		//[kWt] Incident beam radiation for each receiver in loop
		// Calculate the energy on the receiver
		m_q_rec[i] = m_q_inc[i] * m_eta_optical[gset];				//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*

		m_eta_opt += m_eta_optical[gset]*m_A_aperture.at(gset,0);		//[-*m2]
	}
	m_eta_opt /= (m_Ap_loop);

	// Assume that whenever field is in STARTUP OR ON, we're using the nominal tracking load
	// This is because it takes power to move into and out of defocus, and we'd probably
	//    just add complexity without any accuracy by trying to capture that
	m_W_dot_sca_tracking = m_W_dot_sca_tracking_nom*m_ftrack;	//[MWe]

	m_control_defocus = m_component_defocus = 1.0;		//[-]

	m_q_dot_inc_sf_tot = m_Ap_tot*weather.m_beam/1.E6;	//[MWt]
}


void C_csp_lf_dsg_collector_receiver::loop_optical_eta_off()
{
	// If solar collectors are not absorbing any sunlight (night or 100% defocus), then set member data as necessary
	m_q_inc.assign(m_q_inc.size(), 0.0);	//[kWt]
	m_eta_optical.fill(0.0);	//[-] Optical efficiency for each collector geometry
	m_q_rec.assign(m_q_rec.size(), 0.0);	//[kWt]

	m_phi_t = 0.0;		//[rad]
	m_theta_L = 0.0;	//[rad]
	m_ftrack = 0.0;		//[-]
	m_eta_opt = 0.0;	//[-]
	m_control_defocus = m_component_defocus = 0.0;	//[-]

	m_q_dot_inc_sf_tot = 0.0;	//[MWt]

	m_W_dot_sca_tracking = 0.0;	//[MWe]

	return;
}

void C_csp_lf_dsg_collector_receiver::update_last_temps()
{
	// Update '_last' temperatures from temperature solved in loop_energy_balance
	mc_sys_cold_out_t_end_last.m_enth = mc_sys_cold_out_t_end.m_enth;	//[kJ/kg]
	mc_sys_cold_out_t_end_last.m_pres = mc_sys_cold_out_t_end.m_pres;	//[bar]
	mc_sys_cold_out_t_end_last.m_temp = mc_sys_cold_out_t_end.m_temp;	//[K]
	mc_sys_cold_out_t_end_last.m_x = mc_sys_cold_out_t_end.m_x;			//[-]

	mc_sys_hot_out_t_end_last.m_enth = mc_sys_hot_out_t_end.m_enth;		//[kJ/kg]
	mc_sys_hot_out_t_end_last.m_pres = mc_sys_hot_out_t_end.m_pres;		//[bar]
	mc_sys_hot_out_t_end_last.m_temp = mc_sys_hot_out_t_end.m_temp;		//[K]
	mc_sys_hot_out_t_end_last.m_x = mc_sys_hot_out_t_end.m_x;			//[-]

	for(int i = 0; i < m_nModTot; i++)
	{
		mc_sca_out_t_end_last[i].m_enth = mc_sca_out_t_end[i].m_enth;	//[kJ/kg]
		mc_sca_out_t_end_last[i].m_pres = mc_sca_out_t_end[i].m_pres;	//[bar]
		mc_sca_out_t_end_last[i].m_temp = mc_sca_out_t_end[i].m_temp;	//[K]
		mc_sca_out_t_end_last[i].m_x = mc_sca_out_t_end[i].m_x;			//[-]
	}

	return;
}

void C_csp_lf_dsg_collector_receiver::reset_last_temps()
{
	// Update '_last' temperatures from temperature solved in loop_energy_balance
	mc_sys_cold_out_t_end_last.m_enth = mc_sys_cold_out_t_end_converged.m_enth;	//[kJ/kg]
	mc_sys_cold_out_t_end_last.m_pres = mc_sys_cold_out_t_end_converged.m_pres;	//[bar]
	mc_sys_cold_out_t_end_last.m_temp = mc_sys_cold_out_t_end_converged.m_temp;	//[K]
	mc_sys_cold_out_t_end_last.m_x = mc_sys_cold_out_t_end_converged.m_x;		//[-]

	mc_sys_hot_out_t_end_last.m_enth = mc_sys_hot_out_t_end_converged.m_enth;	//[kJ/kg]
	mc_sys_hot_out_t_end_last.m_pres = mc_sys_hot_out_t_end_converged.m_pres;	//[bar]
	mc_sys_hot_out_t_end_last.m_temp = mc_sys_hot_out_t_end_converged.m_temp;	//[K]
	mc_sys_hot_out_t_end_last.m_x = mc_sys_hot_out_t_end_converged.m_x;			//[-]

	for( int i = 0; i < m_nModTot; i++ )
	{
		mc_sca_out_t_end_last[i].m_enth = mc_sca_out_t_end_converged[i].m_enth;	//[kJ/kg]
		mc_sca_out_t_end_last[i].m_pres = mc_sca_out_t_end_converged[i].m_pres;	//[bar]
		mc_sca_out_t_end_last[i].m_temp = mc_sca_out_t_end_converged[i].m_temp;	//[K]
		mc_sca_out_t_end_last[i].m_x = mc_sca_out_t_end_converged[i].m_x;		//[-]
	}

	return;
}

int C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int(const C_csp_weatherreader::S_outputs &weather,
	double T_cold_in /*K*/, double P_field_out /*bar*/, double m_dot_loop_in /*kg/s*/, double h_sca_out_target /*kJ/kg*/,
	const C_csp_solver_sim_info &sim_info)
{
	// assumes the following calculations/metrics are up-to-date:
	// * m_q_inc[]
	// * m_q_rec[]

	m_m_dot_loop = m_dot_loop_in;

	double T_db = weather.m_tdry + 273.15;		//[K] Dry bulb temperature, convert from C
	double V_wind = weather.m_wspd;				//[m/s] Ambient windspeed

	double hour = (double)((int)(sim_info.ms_ts.m_time/3600.0) % 24);
	double T_sky = CSP::skytemp(T_db, weather.m_tdew+273.15, hour);		//[K] Sky temperature

	// Basis pressure used to calculate off-design pressure drop through field
	double dP_basis = m_m_dot_loop*(double)m_nLoops/m_m_dot_des*m_P_turb_des;	//[bar]

	// Calculate pressure at inlet of solar field
	bool m_is_model_headers = true;

	double P_system_in = std::numeric_limits<double>::quiet_NaN();	//[bar]
	if( m_is_model_headers )
	{
		P_system_in = P_field_out + dP_basis*(m_fP_sf_tot);		//[bar]
	}
	else
	{
		P_system_in = P_field_out + dP_basis*(m_fP_sf_tot - m_fP_hdr_c);		//[bar]
	}

	// Need to provide pumping power to get from Field Outlet to Field Inlet
		// Calculate pump inlet enthalpy
	int wp_code = water_TP(T_cold_in, P_field_out*100.0, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int pump inlet", "water_TP error", wp_code));
	}
	if( wp.qual > 0.0 )
	{
		throw(C_csp_exception("The inlet to the once thru loop, pre-pump, is 2-phase, this is not good"));
	}
	double s_pump_in = wp.entr;		//[kJ/kg-K]
	double h_pump_in = wp.enth;		//[kJ/kg]

		// Calculate isentropic pump outlet enthalpy
	wp_code = water_PS(P_system_in*100.0, s_pump_in, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int pump isentropic outlet", "water_PS error", wp_code));
	}
	double h_pump_out_isen = wp.enth;	//[kJ/kg]

		// Calculate actual pump outlet enthalpy
	double eta_isen = m_eta_pump;		//[-]
	double h_pump_out = (h_pump_out_isen - h_pump_in)/eta_isen + h_pump_in;	//[kJ/kg]

		// Calculate pump outlet state
	wp_code = water_PH(P_system_in*100.0, h_pump_out, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int pump outlet", "water_PH error", wp_code));
	}
	if( wp.qual > 0.0 )
	{
		throw(C_csp_exception("The inlet to the once thru loop, post-pump, is 2-phase, this is not good"));
	}

		// Calculate pumping power
	m_W_dot_pump = m_m_dot_loop*(double)m_nLoops*(h_pump_out - h_pump_in)/1.E3;		//[MWe]

	// Set system/header/field inlet state
	mc_sys_cold_in_t_int.m_pres = P_system_in;	//[bar]
	mc_sys_cold_in_t_int.m_enth = h_pump_out;	//[kJ/kg]
	mc_sys_cold_in_t_int.m_temp = wp.temp;		//[K]
	mc_sys_cold_in_t_int.m_x = -1;				//[-]

	double q_dot_loss_HR_cold = 0.0;		//[kWt]

	if( m_is_model_headers )
	{
		// Apply header/runner transients here
			// Current none...
		mc_sys_cold_out_t_int.m_pres = mc_sys_cold_out_t_end.m_pres = P_field_out + dP_basis*(m_fP_sf_tot - m_fP_hdr_c);	//[bar]
		mc_sys_cold_out_t_int.m_enth = mc_sys_cold_out_t_end.m_enth = mc_sys_cold_in_t_int.m_enth;		//[kJ/kg]

		wp_code = water_PH(mc_sys_cold_out_t_int.m_pres*100.0, mc_sys_cold_out_t_int.m_enth, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int cold system/header/field outlet",
				"water_PH error", wp_code));
		}

		mc_sys_cold_out_t_int.m_temp = mc_sys_cold_out_t_end.m_temp = wp.temp;		//[K]
		mc_sys_cold_out_t_int.m_x = mc_sys_cold_out_t_end.m_x = wp.qual;			//[-]

			// This is a bit different than the trough version because we don't know piping dimensions
			//   and are instead using aperture area.
			// Older version applied this loss after the loop calculations converged
			//  So let's apply half (of aperture area) at cold inlet and half at hot outlet
		q_dot_loss_HR_cold = m_Ap_tot/2.0 * m_Pipe_hl_coef / 1000.0 * (mc_sys_cold_out_t_int.m_temp - T_db);		//[kWt]

		mc_sca_in_t_int[0].m_pres = mc_sys_cold_out_t_int.m_pres;		//[bar]
		mc_sca_in_t_int[0].m_enth = mc_sys_cold_out_t_int.m_enth - q_dot_loss_HR_cold/(m_m_dot_loop*double(m_nLoops));		//[kJ/kg]

		wp_code = water_PH(mc_sca_in_t_int[0].m_pres*100.0, mc_sca_in_t_int[0].m_enth, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int 1st sca inlet",
				"water_PH error", wp_code));
		}

		mc_sca_in_t_int[0].m_temp = wp.temp;	//[K]
		mc_sca_in_t_int[0].m_x = wp.qual;		//[-]
	}
	else
	{
		// Set all system/header/field inlet and outlet states and mc_sca_in_t_int[0]
		mc_sca_in_t_int[0].m_pres = mc_sys_cold_out_t_int.m_pres = mc_sys_cold_out_t_end.m_pres = mc_sys_cold_in_t_int.m_pres;	//[bar]
		mc_sca_in_t_int[0].m_enth = mc_sys_cold_out_t_int.m_enth = mc_sys_cold_out_t_end.m_enth = mc_sys_cold_in_t_int.m_enth;	//[kJ/kg]
		mc_sca_in_t_int[0].m_temp = mc_sys_cold_out_t_int.m_temp = mc_sys_cold_out_t_end.m_temp = mc_sys_cold_in_t_int.m_temp;	//[K]
		mc_sca_in_t_int[0].m_x = mc_sys_cold_out_t_int.m_x = mc_sys_cold_out_t_end.m_x = mc_sys_cold_in_t_int.m_x;				//[-]
	}

	// Initialize thermal loss and total thermal power absorbed vectors
	m_q_loss.assign(m_q_loss.size(), 0.0);		//[kWt] Thermal loss for each receiver in loop
	m_q_abs.assign(m_q_abs.size(), 0.0);		//[kWt] Thermal power absorbed by steam in each receiver

	// Guess the enthalpy at the next SCA
	double dh_per_sca = (h_sca_out_target - mc_sca_in_t_int[0].m_enth)/(double)m_nModTot;	//[kJ/kg]

	std::vector<double> E_sca;	//[kJ]
	E_sca.resize(m_nModTot);	//[kJ]

	// Begin loop through SCAs
	for(int i = 0; i < m_nModTot; i++)
	{
		// Update sca inlet enthalpy
		if( i > 0 )
			mc_sca_in_t_int[i] = mc_sca_out_t_int[i-1];		//[kJ/kg]

		// Which geometry set?
		int geom_type = 0;
		if( i >= m_nModBoil && m_is_multgeom )
			geom_type = 1;

		mc_sca_in_t_int[i].m_pres =
			P_field_out + dP_basis*(m_fP_hdr_h+(m_fP_sf_sh+m_fP_boil_to_sh+m_fP_sf_boil)*(1.0-(double)i/(double)m_nModTot));	//[bar]

		double h_ave_i = mc_sca_in_t_int[i].m_enth + dh_per_sca*0.5;	//[kJ/kg]

		// Get the temperature at each state point in the loop
		wp_code = water_PH(mc_sca_in_t_int[i].m_pres*100.0, h_ave_i, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int ith sca inlet",
				"water_PH error", wp_code));
		}

		double T_ave_i = wp.temp;	//[K]

		// Estimate the heat loss for each receiver using the average temperature
		if( m_HLCharType.at(geom_type, 0) == 1 )
		{
			// Estimate based on the user-specified polynomial heat loss adjustments
			double dT_loc = T_ave_i - T_db;		//[K/C]
			double c_hl = m_HL_dT.at(geom_type, 0) + m_HL_dT.at(geom_type, 1)*dT_loc + m_HL_dT.at(geom_type, 2)*pow(dT_loc, 2) +
							m_HL_dT.at(geom_type, 3)*pow(dT_loc, 3) + m_HL_dT.at(geom_type, 4)*pow(dT_loc, 4);	//[W/m] Effect from temperature difference between fluid and dry bulb

			if( m_HL_W.at(geom_type, 0) != 0 || m_HL_W.at(geom_type, 1) != 0 || m_HL_W.at(geom_type, 2) != 0 || m_HL_W.at(geom_type, 3) != 0 || m_HL_W.at(geom_type, 0) != 0 )
			{
				c_hl *= m_HL_W.at(geom_type, 0) + m_HL_W.at(geom_type, 1)*V_wind + m_HL_W.at(geom_type, 2)*pow(V_wind, 2) +
							m_HL_W.at(geom_type, 3)*pow(V_wind, 3) + m_HL_W.at(geom_type, 4)*pow(V_wind, 4);
			}

			m_q_loss[i] = c_hl*m_L_col.at(geom_type, 0) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
			m_q_abs[i] = m_q_rec[i] - m_q_loss[i];					//[kWt] Thermal power absorbed by steam in each receiver
		}
		else if( m_HLCharType.at(geom_type, 0) == 2 )
		{
			// Calculate thermal loss from Forristall receiver model
			m_q_loss[i] = 0.0;		//[kWt] Thermal loss for each receiver in loop
			m_q_abs[i] = 0.0;		//[kWt] Thermal power absorbed by steam in each receiver

			for( int j = 0; j < 4; j++ )
			{
				// Only calculate if the HCE fraction is non-zero
				if( m_HCE_FieldFrac.at(geom_type, j) <= 0.0 )
					continue;

				/*Call the receiver performance model - single point mode
				!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
				!only use for the fluid type in the single point model is calculating the convective heat transfer
				!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and
				!steam that substituting the HTF here introduces negligible error.*/

				// For LF, HT = CT && sca_num = 0
				double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
				q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
				evac_tube_model.EvacReceiver(T_ave_i, 10.0, T_db, T_sky, V_wind, weather.m_pres*100.0, m_q_inc[i] / m_L_col.at(geom_type, 0)*1000.0,
					geom_type, j, geom_type, 0, true, m_ncall, sim_info.ms_ts.m_time/3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3);

				if( q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs )
				{
					q_rec_loss = 0.0;
					q_rec_abs = 0.0;
				}

				m_q_loss[i] += q_rec_loss*m_L_col.at(geom_type, 0)*m_HCE_FieldFrac.at(geom_type, j) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
				m_q_abs[i] += q_rec_abs*m_L_col.at(geom_type, 0)*m_HCE_FieldFrac.at(geom_type, j) / 1000.0;		//[kWt] Thermal power absorbed by steam in each receiver
			}
		}
		else
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int: Geometry type not recognized!"));
		}

		mc_sca_out_t_int[i].m_pres = mc_sca_out_t_end[i].m_pres = mc_sca_in_t_int[i].m_pres;	//[bar]

		transient_energy_bal_numeric_int_ave(mc_sca_in_t_int[i].m_enth, mc_sca_out_t_int[i].m_pres*100.0, m_q_abs[i], m_m_dot_loop,
			mc_sca_out_t_end_last[i].m_temp, m_C_thermal, sim_info.ms_ts.m_step, mc_sca_out_t_end[i].m_enth, mc_sca_out_t_int[i].m_enth);

		wp_code = water_PH(mc_sca_out_t_end[i].m_pres*100.0, mc_sca_out_t_end[i].m_enth, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int ith sca t_end",
				"water_PH error", wp_code));
		}
		mc_sca_out_t_end[i].m_temp = wp.temp;		//[K]
		mc_sca_out_t_end[i].m_x = wp.qual;			//[-]

		wp_code = water_PH(mc_sca_out_t_int[i].m_pres*100.0, mc_sca_out_t_int[i].m_enth, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int ith sca t_end",
				"water_PH error", wp_code));
		}
		mc_sca_out_t_int[i].m_temp = wp.temp;		//[K]
		mc_sca_out_t_int[i].m_x = wp.qual;			//[-]

		E_sca[i] = m_C_thermal*(mc_sca_out_t_end[i].m_temp - mc_sca_out_t_end_last[i].m_temp);	//[kJ]

		//double time_here = sim_info.ms_ts.m_time;
		//
		//double q_bal_abs = m_q_abs[i];		//[kW]
		//double q_bal_htf = m_m_dot_loop*(mc_sca_out_t_int[i].m_enth - mc_sca_in_t_int[i].m_enth);	//[kW]
		//double q_bal_int_energy = E_sca[i] / sim_info.ms_ts.m_step;		//[kW]
		//
		//double q_max_abs = max( abs(q_bal_int_energy), max( abs(q_bal_abs), abs(q_bal_htf) ) );
		//
		//double E_bal_sca = q_bal_abs - q_bal_htf - q_bal_int_energy;
		//
		//double E_bal_sca_abs = fabs(E_bal_sca);
		//
		//double blahhh = 1.2345;
	}

	mc_sys_hot_in_t_int.m_enth = mc_sca_out_t_int[m_nModTot-1].m_enth;	//[kJ/kg]
	mc_sys_hot_in_t_int.m_pres = mc_sca_out_t_int[m_nModTot-1].m_pres;	//[bar]
	mc_sys_hot_in_t_int.m_temp = mc_sca_out_t_int[m_nModTot-1].m_temp;	//[K]
	mc_sys_hot_in_t_int.m_x = mc_sca_out_t_int[m_nModTot-1].m_x;		//[-]

	double q_dot_loss_HR_hot = 0.0;			//[kWt]

	if( m_is_model_headers )
	{
		q_dot_loss_HR_hot = m_Ap_tot / 2.0 * m_Pipe_hl_coef / 1000.0 * (mc_sys_hot_in_t_int.m_temp - T_db);		//[kWt]

		// Calculate outlet state
		mc_sys_hot_out_t_int.m_pres = mc_sys_hot_out_t_end.m_pres = P_field_out;	//[bar]
		mc_sys_hot_out_t_int.m_enth = mc_sys_hot_out_t_end.m_enth = mc_sys_hot_in_t_int.m_enth - q_dot_loss_HR_hot/(m_m_dot_loop*double(m_nLoops));

		wp_code = water_PH(mc_sys_hot_out_t_int.m_pres*100.0, mc_sys_hot_out_t_int.m_enth, &wp);
		if( wp_code != 0 )
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::once_thru_loop_energy_balance_T_t_int hot header",
				"water_PH error", wp_code));
		}
		mc_sys_hot_out_t_int.m_temp = mc_sys_hot_out_t_end.m_temp = wp.temp;	//[K]
		mc_sys_hot_out_t_int.m_x = mc_sys_hot_out_t_end.m_x = wp.qual;			//[-]
	}
	else
	{
		// Set all system/header/field inlet and outlet states and mc_sca_in_t_int[0]
		mc_sys_hot_out_t_int.m_enth = mc_sys_hot_out_t_end.m_enth = mc_sys_hot_in_t_int.m_enth;		//[kJ/kg]
		mc_sys_hot_out_t_int.m_pres = mc_sys_hot_out_t_end.m_pres = mc_sys_hot_in_t_int.m_pres;		//[bar]
		mc_sys_hot_out_t_int.m_temp = mc_sys_hot_out_t_end.m_temp = mc_sys_hot_in_t_int.m_temp;		//[K]
		mc_sys_hot_out_t_int.m_x = mc_sys_hot_out_t_end.m_x = mc_sys_hot_in_t_int.m_x;				//[-]
	}

	// Calculate sub-timestep reporting energy (rate) balance metrics
		// Loop metrics
	m_q_dot_sca_loss_summed_subts = 0.0;	//[MWt]
	m_q_dot_sca_abs_summed_subts = 0.0;		//[MWt]
	m_E_dot_sca_summed_subts = 0.0;			//[MWt]

	for(int i = 0; i < m_nModTot; i++)
	{
		m_q_dot_sca_loss_summed_subts += m_q_loss[i];	//[kWt] -> convert to MWt and multiply by nLoops below
		m_q_dot_sca_abs_summed_subts += m_q_abs[i];		//[kWt] -> convert to MWt and multiply by nLoops below
		m_E_dot_sca_summed_subts += E_sca[i];			//[kJ] -> convert to MWt and multiply by nLoops below
	}
	m_q_dot_sca_loss_summed_subts *= 1.E-3 * m_nLoops;	//[MWt] SYSTEM
	m_q_dot_sca_abs_summed_subts *= 1.E-3 * m_nLoops;	//[MWt] SYSTEM
	m_E_dot_sca_summed_subts *= 1.E-3 * (m_nLoops / sim_info.ms_ts.m_step);	//[MWt] SYSTEM

		// Header-runner metrics
	m_q_dot_HR_cold_loss_subts = q_dot_loss_HR_cold*1.E-3;		//[MWt]
	m_q_dot_HR_hot_loss_subts = q_dot_loss_HR_hot*1.E-3;		//[MWt]

		// Q_dot out of system
	m_q_dot_to_sink_subts = m_m_dot_loop*double(m_nLoops)*(mc_sys_hot_out_t_int.m_enth - mc_sys_cold_in_t_int.m_enth)*1.E-3;	//[MWt]

	//double E_bal = m_q_dot_sca_abs_summed_subts - m_q_dot_HR_cold_loss_subts - m_q_dot_HR_hot_loss_subts -
	//				m_q_dot_to_sink_subts - m_E_dot_sca_summed_subts;

	// *********************************************************
	// Calculate total losses and energy balances
	// *********************************************************
	double Q_abs_sca_summed = 0.0;			//[MJ]
	for( int i = 0; i < m_nModTot; i++ )
	{
		Q_abs_sca_summed += m_q_abs[i];		//[kW] -> convert to MJ and multiply by nLoops below
	}
	Q_abs_sca_summed *= sim_info.ms_ts.m_step*m_nLoops/1.E3;		//[MJ]

	double Q_loss_HR_cold = q_dot_loss_HR_cold*sim_info.ms_ts.m_step/1.E3;			//[MJ]
	double Q_loss_HR_hot = q_dot_loss_HR_hot*sim_info.ms_ts.m_step/1.E3;			//[MJ]

	m_Q_field_losses_total = Q_loss_HR_cold + Q_loss_HR_hot - Q_abs_sca_summed;		//[MJ]

	return E_loop_energy_balance_exit::SOLVED;
}

void C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int_ave(double h_in /*kJ/kg*/, double P_in /*kPa*/,
	double q_dot_abs /*kWt*/, double m_dot /*kg/s*/,
	double T_out_t_end_prev /*K*/,
	double C_thermal /*kJ/K*/, double step /*s*/,
	double & h_out_t_end /*kJ/K*/, double & h_out_t_int /*kJ/K*/)
{
	int n_steps = n_integration_steps;	//[-]
	double step_subts = step / (double)n_steps;	//[s]

	double h_out_t_int_sum = 0.0;		//[kJ/K]
	double h_out_t_end_local = 0.0;		//[kJ/K]
	double T_out_t_end_prev_local = T_out_t_end_prev;		//[K]
	double h_out_t_start_local = 0.0;
	double T_out_t_end_local = 0.0;
	for(int i = 0; i < n_steps; i++)
	{
		try
		{
			transient_energy_bal_numeric_int(h_in, P_in, q_dot_abs, m_dot, T_out_t_end_prev_local, C_thermal,
				step_subts, h_out_t_start_local, h_out_t_end_local, T_out_t_end_local);
		}
		catch (C_csp_exception &csp_except)
		{
			int error_code = csp_except.m_error_code;

			if (error_code != 5)
			{
				throw(C_csp_exception(csp_except));
			}

			int n_steps_lx = 20;
			double steps_subts_local = step_subts / (double)n_steps;

			double h_out_t_int_sum_lx = 0.0;
			double h_out_t_end_lx = 0.0;
			double h_out_t_start_lx = 0.0;
			double T_out_t_end_lx = 0.0;

			for (int j = 0; j < n_steps_lx; j++)
			{

				transient_energy_bal_numeric_int(h_in, P_in, q_dot_abs, m_dot, T_out_t_end_prev_local, C_thermal,
					steps_subts_local, h_out_t_start_lx, h_out_t_end_lx, T_out_t_end_lx);

				h_out_t_int_sum_lx += 0.5*(h_out_t_start_lx + h_out_t_end_lx);
				T_out_t_end_prev_local = T_out_t_end_lx;
			}

			h_out_t_end_local = h_out_t_end_lx;
			double h_out_t_int_lx = h_out_t_int_sum_lx / (double)n_steps;
			h_out_t_start_local = 2.0*h_out_t_int_lx - h_out_t_end_local;
			T_out_t_end_local = T_out_t_end_lx;
		}

		h_out_t_int_sum += 0.5*(h_out_t_start_local + h_out_t_end_local);	//[kJ/K]
		T_out_t_end_prev_local = T_out_t_end_local;		//[K]

	}

	h_out_t_int = h_out_t_int_sum / (double)n_steps;	//[kJ/K]
	h_out_t_end = h_out_t_end_local;					//[kJ/K]

	return;
}

void C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int(double h_in /*kJ/kg*/, double P_in /*kPa*/,
	double q_dot_abs /*kWt*/, double m_dot /*kg/s*/,
	double T_out_t_end_prev /*K*/,
	double C_thermal /*kJ/K*/, double step /*s*/,
	double & h_out_t_end_prev /*kJ/K*/, double & h_out_t_end /*kJ/K*/, double & T_out_t_end /*K*/)
{
	// Check whether 'T_out_t_end_prev' corresponds to boiling temperature of 'P_in'
	int water_prop_error = water_PQ(P_in, 0.0, &wp);
	if(water_prop_error != 0)
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
			"water_TP error trying to find boiling temperature", water_prop_error));
	}
	double T_x0_at_P_in = wp.temp;		//[K]

	double deltaT_tol = 0.001*T_x0_at_P_in;	//[K]
	double deltaT = T_out_t_end_prev - T_x0_at_P_in;	//[K]

	if (fabs(deltaT) >= deltaT_tol)
	{
		water_prop_error = water_TP(T_out_t_end_prev, P_in, &wp);
		if (water_prop_error != 0)
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
				"water_TP error at T_out_t_end_prev and P_in", water_prop_error));
		}
		h_out_t_end_prev = wp.enth;		//[kJ/kg]
	}
	else
	{
		double f_deltaT = fabs(deltaT) / deltaT_tol;	//[-]

		if (T_out_t_end_prev > T_x0_at_P_in)
		{
			water_prop_error = water_TQ(T_out_t_end_prev + deltaT, 1.0, &wp);
			if (water_prop_error != 0)
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
					"water_TQ T_out_t_end_prev q = 0", water_prop_error));
			}
			double h_1phase = wp.enth;	//[kJ/kg]

			h_out_t_end_prev = (1.0 - f_deltaT)*h_in + f_deltaT * h_1phase;		//[kJ/kg]
		}
		else
		{
			water_prop_error = water_TQ(T_out_t_end_prev + deltaT, 0.0, &wp);
			if (water_prop_error != 0)
			{
				throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
					"water_TQ T_out_t_end_prev q = 0", water_prop_error));
			}
			double h_1phase = wp.enth;	//[kJ/kg]

			h_out_t_end_prev = (1.0 - f_deltaT)*h_in + f_deltaT * h_1phase;	//[kJ/kg]
		}
	}

	// Guess2: outlet enthalpy is from a steady state calculation
	double h_out_t_end_guess2 = h_in + q_dot_abs/m_dot;		//[kJ/kg]

	// Check that this guess is not too close to Guess1
	double diff_guess = (h_out_t_end_guess2 - h_out_t_end_prev) / h_out_t_end_prev;
	if( abs(diff_guess) < 0.01 )
	{
		if(diff_guess > 0.0)
		{
			h_out_t_end_guess2 = 1.05*h_out_t_end_prev;
		}
		else
		{
			h_out_t_end_guess2 = 0.95*h_out_t_end_prev;
		}
	}

	// Apply 1 var solver to find the mass flow rate that achieves the target outlet temperature
	C_mono_eq_transient_energy_bal c_transient_energy_bal(h_in, P_in, q_dot_abs, m_dot, T_out_t_end_prev, h_out_t_end_prev, C_thermal, step);
	C_monotonic_eq_solver c_h_out_t_end_solver(c_transient_energy_bal);

	// Get minimum enthalpy at this pressure
	water_prop_error = water_TP(m_wp_min_temp*1.01, P_in, &wp);
	if(water_prop_error != 0)
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
			"water_TP error trying to find min enthalpy", water_prop_error));
	}
	double h_out_t_end_lower = wp.enth;		//[kJ/kg]

	// Get maximum enthalpy at this pressure
	water_prop_error = water_TP(m_wp_max_temp*0.99, P_in, &wp);
	if(water_prop_error != 0)
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int",
			"water_TP error trying to find MAX enthalpy", water_prop_error));
	}
	double h_out_t_end_upper = wp.enth;		//[kJ/kg]

	// Set solver settings
		// Absolute error
	c_h_out_t_end_solver.settings(0.00001, 100, h_out_t_end_lower, h_out_t_end_upper, false);

	int iter_solved = -1;
	double tol_solved = std::numeric_limits<double>::quiet_NaN();

	int h_out_t_end_code = 0;

	h_out_t_end = std::numeric_limits<double>::quiet_NaN();

	try
	{
		h_out_t_end_code = c_h_out_t_end_solver.solve(h_out_t_end_prev, h_out_t_end_guess2, 0.0, h_out_t_end, tol_solved, iter_solved);
	}
	catch( C_csp_exception &  )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int monotonic solver failed"));
	}

	if( h_out_t_end_code != C_monotonic_eq_solver::CONVERGED )
	{
		if( tol_solved == tol_solved && tol_solved < 0.1 )
		{
			double blah = 1.23;
		}
		else
		{
			throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::transient_energy_bal_numeric_int monotonic solver failed to reach convergence","",5));
		}
	}

	T_out_t_end = c_transient_energy_bal.m_T_out_t_end;		//[K]
}

int C_csp_lf_dsg_collector_receiver::C_mono_eq_transient_energy_bal::operator()(double h_out_t_end /*K*/, double *diff_T_out_t_end /*-*/)
{
	int water_prop_error = water_PH(m_P_in, h_out_t_end, &mc_wp);
	if( water_prop_error != 0 )
	{
		*diff_T_out_t_end = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}
	m_T_out_t_end = mc_wp.temp;	//[K]

	double dTdt_prev = (m_q_dot_abs + m_m_dot*(m_h_in - m_h_out_t_end_prev));
	double dTdt_next = (m_q_dot_abs + m_m_dot*(m_h_in - h_out_t_end));

	double T_out_t_end_calc = m_T_out_t_end_prev + 0.5*m_step/m_C_thermal*(dTdt_prev + dTdt_next);	//[K]

	*diff_T_out_t_end = (m_T_out_t_end - T_out_t_end_calc) / m_T_out_t_end_prev;		//[-]

	return 0;
}


void C_csp_lf_dsg_collector_receiver::set_output_values()
{
	mc_reported_outputs.value(E_THETA_TRAVERSE, m_phi_t*180.0/CSP::pi);			//[deg], convert from rad
	mc_reported_outputs.value(E_THETA_LONGITUDINAL, m_theta_L*180.0/CSP::pi);	//[deg], convert from rad
	mc_reported_outputs.value(E_ETA_OPTICAL, m_eta_opt);						//[-]
	mc_reported_outputs.value(E_DEFOCUS, m_control_defocus*m_component_defocus);	//[-]

	mc_reported_outputs.value(E_Q_DOT_INC_SF_TOT, m_q_dot_inc_sf_tot);			//[MWt]
	mc_reported_outputs.value(E_Q_DOT_REC_INC, m_q_dot_sca_abs_summed_fullts + m_q_dot_sca_loss_summed_fullts);	//[MWt]
	mc_reported_outputs.value(E_Q_DOT_REC_THERMAL_LOSS, m_q_dot_sca_loss_summed_fullts);	//[MWt]
	mc_reported_outputs.value(E_Q_DOT_REC_ABS, m_q_dot_sca_abs_summed_fullts);	//[MWt]
	mc_reported_outputs.value(E_Q_DOT_PIPING_LOSS, m_q_dot_HR_cold_loss_fullts + m_q_dot_HR_hot_loss_fullts);	//[MWt]
	mc_reported_outputs.value(E_E_DOT_INTERNAL_ENERGY, m_E_dot_sca_summed_fullts);	//[MWt]
	mc_reported_outputs.value(E_Q_DOT_OUT, m_q_dot_to_sink_fullts);				//[MWt]
	mc_reported_outputs.value(E_Q_DOT_FREEZE_PROT, m_q_dot_freeze_protection);	//[MWt]

	mc_reported_outputs.value(E_M_DOT_LOOP, m_m_dot_loop);				//[kg/s]
	mc_reported_outputs.value(E_M_DOT_FIELD, m_m_dot_loop*m_nLoops);	//[kg/s]

	// Calculate output statepoints
	int wp_code = water_PH(m_P_sys_c_in_t_int_fullts*100.0, m_h_sys_c_in_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::set_output_values Field Cold In state point calcs failed", "water_PH error", wp_code));
	}
	mc_reported_outputs.value(E_T_FIELD_COLD_IN, wp.temp-273.15);		//[C]

	wp_code = water_PH(m_P_c_rec_in_t_int_fullts*100.0, m_h_c_rec_in_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::set_output_values Rec Cold In state point calcs failed", "water_PH error", wp_code));
	}
	mc_reported_outputs.value(E_T_REC_COLD_IN, wp.temp-273.15);			//[C]

	wp_code = water_PH(m_P_h_rec_out_t_int_fullts*100.0, m_h_h_rec_out_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::set_output_values Rec Hot Out state point calcs failed", "water_PH error", wp_code));
	}
	mc_reported_outputs.value(E_T_REC_HOT_OUT, wp.temp-273.15);			//[C]
	double x_out = wp.qual;
	if( x_out < 0.0 )
		x_out = -1.0;
	if( x_out > 1.0 )
		x_out = 10.0;
	mc_reported_outputs.value(E_X_REC_HOT_OUT, x_out);		//[-]

	wp_code = water_PH(m_P_sys_h_out_t_int_fullts*100.0, m_h_sys_h_out_t_int_fullts, &wp);
	if( wp_code != 0 )
	{
		throw(C_csp_exception("C_csp_lf_dsg_collector_receiver::set_output_values Field Hot Out state point calcs failed", "water_PH error", wp_code));
	}
	mc_reported_outputs.value(E_T_FIELD_HOT_OUT, wp.temp-273.15);		//[C]
	x_out = wp.qual;
	if( x_out < 0.0 )
		x_out = -1.0;
	if( x_out > 1.0 )
		x_out = 10.0;
	mc_reported_outputs.value(E_X_FIELD_HOT_OUT, x_out);	//[-]

	// Total pressure drop
	mc_reported_outputs.value(E_PRESSURE_DROP, m_P_sys_c_in_t_int_fullts - m_P_sys_h_out_t_int_fullts);	//[bar]

	mc_reported_outputs.value(E_W_DOT_SCA_TRACK, m_W_dot_sca_tracking);		//[MWe]
	mc_reported_outputs.value(E_W_DOT_PUMP, m_W_dot_pump);					//[MWe]
}

void C_csp_lf_dsg_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{

	// start optical function
	double I_bn = weather.m_beam;						//[W/m2] Current DNI
	double T_db = weather.m_tdry + 273.15;				//[K] Dry bulb temp, convert from C
	double T_dp = weather.m_tdew + 273.15;				//[K] Dewpoint temp, convert from C
	double P_amb = weather.m_pres*100.0;				//[Pa] Ambient pressure, convert from mbar
	double V_wind = weather.m_wspd;						//[m/s] Ambient windspeed
	double shift = weather.m_shift*0.0174533;			//[deg] Shift in longitude from local standard meridian
	double SolarAz = weather.m_solazi;					//[deg] Solar azimuth angle
	double SolarZen = weather.m_solzen*0.0174533;;		//Solar zenith angle [deg]
	double T_pb_out = htf_state_in.m_temp + 273.15;		//[K] Fluid temperature from the power block, convert from C
	int tou_period = sim_info.m_tou - 1;				//[-] control value between 1 & 9, have to change to 0-8 for array index

	SolarAz = (SolarAz - 180.0) * 0.0174533;			//[rad] Convert to TRNSYS convention, radians

	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;

	double time = sim_info.ms_ts.m_time;
	double m_dt = sim_info.ms_ts.m_step;
	double hour = (double)((int)(time / 3600.0) % 24);

	double T_sky = CSP::skytemp(T_db, T_dp, hour);		//[K]

	// Calculations for values once per timestep
	if (m_ncall == 0)
	{
		if(I_bn > 0.0)
			loop_optical_eta(weather, sim_info);
		else
		{
			loop_optical_eta_off();
		}

		// Set initial defocus
		m_defocus = 1.0;
		m_defocus_prev = m_defocus;
		m_is_def = false;
		m_err_def = 0.0;
		m_tol_def = 0.0001;
		m_rc = 0.7;						//Relaxation coefficient
		// Set the power block return temperature to a reasonable value since the power block has not yet been called
		T_pb_out = m_T_field_in_des;	//[K]

		// Reset the pressure check function
		check_pressure.report_and_reset();

		evac_tube_model.Update_Timestep_Properties(m_eta_optical);
	}

	// end optical
	////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////




	//************************************************************
	// Determine the solar field thermal performance
	//************************************************************
	int iter_def = 0;
	bool defocus_on = false;


	double m_dot = std::numeric_limits<double>::quiet_NaN();
	double P_turb_in = std::numeric_limits<double>::quiet_NaN();
	double dP_basis = std::numeric_limits<double>::quiet_NaN();
	double h_pb_out = std::numeric_limits<double>::quiet_NaN();
	double h_b_in = std::numeric_limits<double>::quiet_NaN();
	double eta_opt_ave = 0.0;
	double m_dot_b_tot = 0.0;

	do		// Begin defocus loop
	{
		defocus_on = false;
		double defocus_lim = max(min(m_defocus, 1.0), 0.0);
		eta_opt_ave = 0.0;
		int gset = 0;
		double q_rec_loop = 0.0;
		double q_inc_loop = 0.0;
		for (int i = 0; i < m_nModTot; i++)
		{
			if (i >= m_nModBoil && m_is_multgeom)
				gset = 1;
			// Calculate the incident energy on each module
			m_q_inc[i] = I_bn*m_A_aperture.at(gset, 0) / 1000.0;		//[kWt] Incident beam radiation for each receiver in loop
			// Calculate the energy on the receiver
			m_q_rec[i] = m_q_inc[i] * m_eta_optical[gset]*defocus_lim;	//[kWt] Incident thermal power on receiver after *optical* losses and *defocus*
			// Average optical efficiency
			eta_opt_ave += m_eta_optical[gset]*m_A_aperture.at(gset, 0)*m_nLoops / m_Ap_tot;
			q_rec_loop += m_q_rec[i];			//[kWt] LOOP total thermal power on receiver after *optical* losses and *defocus*
			q_inc_loop += m_q_inc[i];					//[kW] LOOP total incident beam radiation
		}

		// For nighttime conditions, calculate the solar field inlet temperature
		if (q_rec_loop == 0.0 && m_ffrac[tou_period] < m_cycle_cutoff_frac)
			T_pb_out = max(313.15, m_T_sys_prev);		// Need to limit the inlet temp so that the minimum temp in the loops stays above freezing

		// Total reference pressure drop across the field: 9/16/13: tn: already calculated in initial call

		// Guess the mass flow rate in a loop for iteration. Ratio of incident heat/abs heat at design differs by nLoops..
		double m_dot_guess = max(min(q_rec_loop / m_q_dot_abs_tot_des * m_m_dot_des, m_m_dot_max), m_m_dot_min);

		// Guess the turbine pressure.. turbine inlet pressure is highly insensitive to condenser pressure, so
		// simplify the expression to eliminate condenser pressure
		double P_turb_in_guess = turb_pres_frac(m_dot_guess*(double)m_nLoops / m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des;
		double dP_basis_guess = m_dot_guess*(double)m_nLoops / m_m_dot_des*m_P_turb_des;

		if (m_is_oncethru || m_ftrack <= 0.0)		// Run in once-through mode at night since distinct boiler/superheater models are not useful
		{
			// Guess the loop inlet/outlet enthalpies
			water_TP(T_pb_out, check_pressure.P_check(P_turb_in_guess + dP_basis_guess*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
			double h_b_in_guess = wp.enth;		//[kJ/kg]
			double h_pb_out_guess = h_b_in_guess;	//[kJ/kg]
			water_TP(m_T_field_out_des, check_pressure.P_check(P_turb_in_guess + dP_basis_guess*m_fP_hdr_h)*100.0, &wp);
			double h_sh_out_guess = wp.enth;		//[kJ/kg]

			// Set the loop inlet enthalpy
			m_h_in.at(0, 0) = h_b_in_guess;	//[kJ/kg]

			// Set up iteration brackets for false-position method
			double m_dot_lower = 0.7*m_m_dot_min;
			double m_dot_upper = 1.3*m_m_dot_max;
			// Set logic to switch from bisection to false position mode
			bool upflag = false;
			bool lowflag = false;
			// Set upper and lower err results
			double y_upper = std::numeric_limits<double>::quiet_NaN();
			double y_lower = std::numeric_limits<double>::quiet_NaN();

			// Set the iteration tolerance (MJW found that it doesn't help to adjust the tolerance based on iteration number)
			double tol = 1.E-4;

			// Do a rough guess of the receiver enthlapy for the whole loop
			double dh_guess = (h_sh_out_guess - h_b_in_guess) / (double)m_nModTot;
			std::vector<double> h_ave;
			h_ave.resize(m_nModTot, std::numeric_limits<double>::quiet_NaN());
			for (int i = 0; i < m_nModTot; i++)
				h_ave[i] = h_b_in_guess + dh_guess*(double)(i + 1) - dh_guess / 2.0;

			double err = 10.0*tol;
			int iter = 0;

			// Main iteration loop
			while (fabs(err) > tol && iter < 50)
			{
				iter++;
				m_dot = m_dot_guess;

				// Update the turbine pressure and enthalpies
				P_turb_in = check_pressure.P_check(turb_pres_frac(m_dot*(double)m_nLoops / m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des);
				dP_basis = m_dot*(double)m_nLoops / m_m_dot_des*m_P_turb_des;



				// *****************************************
				// Test 'once_thru_loop_energy_balance_T_t_in
				// *****************************************
				//once_thru_loop_energy_balance_T_t_int(weather, T_pb_out, P_turb_in, m_dot, h_sh_out_guess, sim_info);




				// Guess the loop inlet/outlet enthalpies
				water_TP(T_pb_out, check_pressure.P_check(P_turb_in + dP_basis*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
				h_b_in = wp.enth;
				h_pb_out = h_b_in;
				water_TP(m_T_field_out_des, check_pressure.P_check(P_turb_in + dP_basis*m_fP_hdr_h)*100.0, &wp);
				double h_sh_out_target = wp.enth;

				// Set the loop inlet enthalpy
				m_h_in.at(0, 0) = h_b_in;

				// Initialize
				m_q_loss.assign(m_q_loss.size(), 0.0);	//[kWt] Thermal loss for each receiver in loop
				m_q_abs.assign(m_q_abs.size(), 0.0);	//[kWt] Thermal power absorbed by steam in each receiver

				for (int i = 0; i < m_nModTot; i++)
				{
					// Which geometry set?
					gset = 0;
					if (i >= m_nModBoil && m_is_multgeom)
						gset = 1;

					// Calculate thermal losses based on temperature guess values
					// Calculate the local pressure in the superheter. Assume a linear pressure drop across each section
					double P_loc = check_pressure.P_check(P_turb_in + dP_basis * (m_fP_hdr_h + (m_fP_sf_sh + m_fP_boil_to_sh + m_fP_sf_boil)*(1.0 - (double)i / (double)m_nModTot)));

					// Get the temperature at each state point in the loop
					water_PH(P_loc*100.0, h_ave[i], &wp);
					m_T_ave.at(i, 0) = wp.temp;

					// Calculate the heat loss at each temperature
					if (m_HLCharType.at(gset, 0) == 1)
					{
						// Estimate based on the polynomial adjustments provided by the user
						double dT_loc = m_T_ave.at(i, 0) - T_db;
						double c_hl = m_HL_dT.at(gset, 0) + m_HL_dT.at(gset, 1)*dT_loc + m_HL_dT.at(gset, 2)*pow(dT_loc, 2) + m_HL_dT.at(gset, 3)*pow(dT_loc, 3) + m_HL_dT.at(gset, 4)*pow(dT_loc, 4);	//[W/m] Effect from dT
						if (m_HL_W.at(gset, 0) != 0 || m_HL_W.at(gset, 1) != 0 || m_HL_W.at(gset, 2) != 0 || m_HL_W.at(gset, 3) != 0 || m_HL_W.at(gset, 0) != 0)
							c_hl *= m_HL_W.at(gset, 0) + m_HL_W.at(gset, 1)*V_wind + m_HL_W.at(gset, 2)*pow(V_wind, 2) + m_HL_W.at(gset, 3)*pow(V_wind, 3) + m_HL_W.at(gset, 4)*pow(V_wind, 4);
						m_q_loss[i] = c_hl*m_L_col.at(gset, 0) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
						m_q_abs[i] = m_q_rec[i] - m_q_loss[i];				//[kWt] Thermal power absorbed by steam in each receiver
					}
					else if (m_HLCharType.at(gset, 0) == 2)
					{
						// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)

						m_q_loss[i] = 0.0;		//[kWt] Thermal loss for each receiver in loop
						m_q_abs[i] = 0.0;		//[kWt] Thermal power absorbed by steam in each receiver

						for (int j = 0; j < 4; j++)
						{
							// Only calculate if the HCE fraction is non-zero
							if (m_HCE_FieldFrac.at(gset, j) <= 0.0)
								continue;

							/*Call the receiver performance model - single point mode
							!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
							!only use for the fluid type in the single point model is calculating the convective heat transfer
							!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and
							!steam that substituting the HTF here introduces negligible error.*/

							// For LF, HT = CT && sca_num = 0
							double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
							q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
							evac_tube_model.EvacReceiver(m_T_ave.at(i, 0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc[i] / m_L_col.at(gset, 0)*1000.0, gset, j, gset, 0, true, m_ncall,
								time / 3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3);

							if (q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs)
							{
								q_rec_loss = 0.0;
								q_rec_abs = 0.0;
							}

							m_q_loss[i] += q_rec_loss*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kWt] Thermal loss for each receiver in loop
							m_q_abs[i] += q_rec_abs*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kWt] Thermal power absorbed by steam in each receiver

						}

					}

					// Set the inlet enthalpy equal to the outlet of the previous node
					if (i > 0)
						m_h_in.at(i, 0) = m_h_out.at(i - 1, 0);

					// Calculate the collector outlet enthalpy
					double tol_t = 0.001;
					double err_t = 10.0*tol_t;
					int iter_t = 0;
					while (err_t > tol_t && iter_t < 50)
					{
						iter_t++;
						// Calculate the average enthalpy value in the collector module
						m_h_out.at(i,0) = check_h.check(m_h_in.at(i,0) + m_q_abs[i]/m_dot_guess - (m_T_ave.at(i,0)-m_T_ave_prev[i])*m_C_thermal/(m_dt*m_dot_guess));	//[kJ/kg]

						// Update guesses for h_ave and T_ave
						double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i, 0)) / 2.0;
						// Update the average temperature for the heat loss calculation
						water_PH(P_loc*100.0, h_aveg, &wp);
						m_T_ave.at(i, 0) = wp.temp;
						err_t = fabs((h_ave[i] - h_aveg) / h_ave[i]);
						h_ave[i] = h_aveg;
					}

					// Test transient energy balance convergence
					// double h_out_comp = std::numeric_limits<double>::quiet_NaN();
					// transient_energy_bal_numeric_int(m_h_in.at(i, 0), P_loc*100.0, m_q_abs[i], m_dot_guess, m_T_ave_prev[i], m_C_thermal, m_dt, h_out_comp);

					if (i < m_nModTot - 1)
						h_ave[i + 1] = check_h.check(m_h_in.at(i, 0) + (m_h_out.at(i,0) - m_h_in.at(i, 0))*1.5);

				}   // End step through receivers in flow path

				err = (h_sh_out_target - m_h_out.at(m_nModTot - 1, 0)) / h_sh_out_target;

				if (m_dot == m_m_dot_min && err > 0.0)			// m_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
					break;

				//***************************************************
				// *** Hybrid False Position Iteration Method *******
				//***************************************************
				if (lowflag && upflag)
				{
					if (err > 0.0)
					{
						m_dot_upper = m_dot;
						y_upper = err;
					}
					else
					{
						m_dot_lower = m_dot;
						y_lower = err;
					}
					m_dot_guess = y_upper / (y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				}
				else
				{
					if (err > 0.0)			// Prescribed is greater than calculated, so decrease mass flow and set upper limit
					{
						m_dot_upper = m_dot;
						y_upper = err;
						upflag = true;
					}
					else					// Prescribed is less than calculated, so increase mass flow and set lower limit
					{
						m_dot_lower = m_dot;
						y_lower = err;
						lowflag = true;
					}

					if (lowflag && upflag)		// If results of bracket are defined, use false position
						m_dot_guess = y_upper / (y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
					else						// If not, recalculate value based on approximate energy balance
					{
						if (iter < 3)
						{
							double q_abs_sum = 0.0;
							for (int ii = 0; ii < m_nModTot; ii++)
								q_abs_sum += m_q_abs[ii];				//[kWt] LOOP total Thermal power absorbed by steam in each receiver
							m_dot_guess = q_abs_sum / (h_sh_out_target - h_b_in);
							m_dot_guess = max(m_m_dot_min*0.5, min(m_dot_guess, m_m_dot_max*1.5));
						}
						else
							m_dot_guess = 0.5*m_dot_upper + 0.5*m_dot_lower;
					}
				}

				//**************************************************************************
				// **** End Hyrbid False Position Iteration Method *****************
				//**************************************************************************
				if (m_dot_lower >= m_m_dot_max)	// Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
				{
					m_is_def = true;
					break;
				}
				if (m_dot_upper <= m_m_dot_min)   // Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
					m_dot_guess = m_m_dot_min;

			}		// End main iteration loop

			// Defocus calculations
			m_err_def = (m_dot_guess - m_m_dot_max) / m_m_dot_max;
			if (!m_is_def && m_err_def > 0.0)
				m_is_def = true;
			if (m_is_def)
			{
				// Calculate new defocus
				m_defocus = min(1.0, m_defocus_prev*pow(1.0 / (m_err_def + 1.0), m_rc));
				if (fabs(m_err_def) > m_tol_def)
				{
					m_defocus_prev = m_defocus;
					iter_def++;
					if (iter_def < 11)
					{
						defocus_on = true;
						continue;
					}
				}
			}

			// The boiler mass flow rate is equal to the flow throughout the loop
			m_dot_b_tot = m_dot*(double)m_nLoops;

		}		// End once-through model
		else	// Standard boiler + optional superheater design
		{
			// Boiler
			// Guess the field inlet enthalpy
			water_TP(T_pb_out, check_pressure.P_check(P_turb_in_guess + dP_basis_guess*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
			double h_pb_out_guess = wp.enth;		//[kJ/kg]

			// Boiler outlet conditions
			water_PQ(check_pressure.P_check(P_turb_in_guess + dP_basis_guess*(m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, m_x_b_des, &wp);
			double h_b_out_guess = wp.enth;		//[kJ/kg]
			water_PQ(check_pressure.P_check(P_turb_in_guess + dP_basis_guess*(m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, 0.0, &wp);
			double h_b_recirc_guess = wp.enth;	//[kJ/kg]

			// Determine the mixed inlet enthalpy
			double h_b_in_guess = h_pb_out_guess*m_x_b_des + h_b_recirc_guess*(1.0 - m_x_b_des);

			// Set the loop inlet enthalpy
			m_h_in(0, 0) = h_b_in_guess;		//[kJ/kg]

			double m_dot_b_guess = m_dot_guess / m_x_b_des;
			// Set up iteration brackets for false-position method
			double m_dot_lower = 0.7*m_m_dot_min / m_x_b_des;
			double m_dot_upper = 1.3*m_m_dot_b_max;

			bool upflag = false;		// Set logic to switch from bisection to false position mode
			bool lowflag = false;		// Set logic to swtich from bisection to false position mode

			double y_upper = std::numeric_limits<double>::quiet_NaN();
			double y_lower = std::numeric_limits<double>::quiet_NaN();

			// Set iteration tolerance
			double tol = 1.E-4;

			// Do a rough guess of the receiver enthalpy for the boiler
			double dh_b_guess = (h_b_out_guess - h_b_in_guess) / (double)m_nModBoil;
			std::vector<double> h_ave;
			h_ave.resize(m_nModTot, std::numeric_limits<double>::quiet_NaN());
			for (int i = 0; i < m_nModBoil; i++)
				h_ave[i] = h_b_in_guess + dh_b_guess*(double)(i + 1) - dh_b_guess / 2.0;

			double err = 10.0*tol;
			int iter = 0;

			double m_dot_b = std::numeric_limits<double>::quiet_NaN();

			while (fabs(err) > tol && iter < 50)
			{
				iter++;
				m_dot_b = m_dot_b_guess;
				//Initialize
				m_q_loss.assign(m_q_loss.size(), 0.0);		//[kWt] Thermal loss for each receiver in loop
				m_q_abs.assign(m_q_abs.size(), 0.0);		//[kWt] Thermal power absorbed by steam in each receiver

				// Update inlet enthalpy conditions and turbine pressure
				P_turb_in = check_pressure.P_check(turb_pres_frac(m_dot_b*m_x_b_des*(double)m_nLoops / m_m_dot_pb_des, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min)*m_P_turb_des);
				dP_basis = m_dot_b*(double)m_nLoops / m_m_dot_b_des*m_P_turb_des;

				// Field inlet enthalpy
				water_TP(T_pb_out, check_pressure.P_check(P_turb_in + dP_basis*(m_fP_sf_tot - m_fP_hdr_c))*100.0, &wp);
				h_pb_out = wp.enth;

				// Update the boiler outlet conditions
				water_PQ(check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, m_x_b_des, &wp);	// 2-phase outlet enthalpy
				double h_b_out = wp.enth;
				water_PQ(check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_h + m_fP_sf_sh + m_fP_boil_to_sh))*100.0, 0.0, &wp);		// Recirculation enthalpy
				double h_b_recirc = wp.enth;

				// Determin the mixed inlet enthalpy
				h_b_in = h_pb_out*m_x_b_des + h_b_recirc*(1.0 - m_x_b_des);

				// Set the loop inlet enthalpy
				m_h_in(0, 0) = h_b_in;

				for (int i = 0; i < m_nModBoil; i++)
				{
					// Calculate thermal losses based on temperature guess values
					// Calculate the local pressure in the boiler. Assume a linear pressure drop across each section
					double P_loc = check_pressure.P_check(P_turb_in + dP_basis*(m_fP_sf_tot - m_fP_sf_boil*(1.0 - (double)i / (double)m_nModBoil)));

					// Get the temperature at each state in the boiler
					water_PH(P_loc*100.0, h_ave[i], &wp);
					m_T_ave.at(i, 0) = wp.temp;

					gset = 0;
					// Calculate the heat loss at each temperature
					if (m_HLCharType.at(gset, 0) == 1)
					{
						// Estimate based on the polynomial adjustments provided by the user
						double dT_loc = m_T_ave.at(i, 0) - T_db;
						double c_hl = m_HL_dT.at(gset, 0) + m_HL_dT.at(gset, 1)*dT_loc + m_HL_dT.at(gset, 2)*pow(dT_loc, 2) + m_HL_dT.at(gset, 3)*pow(dT_loc, 3) + m_HL_dT.at(gset, 4)*pow(dT_loc, 4);	//[W/m] Effect from dT
						if (m_HL_W.at(gset, 0) != 0 || m_HL_W.at(gset, 1) != 0 || m_HL_W.at(gset, 2) != 0 || m_HL_W.at(gset, 3) != 0 || m_HL_W.at(gset, 0) != 0)
							c_hl *= m_HL_W.at(gset, 0) + m_HL_W.at(gset, 1)*V_wind + m_HL_W.at(gset, 2)*pow(V_wind, 2) + m_HL_W.at(gset, 3)*pow(V_wind, 3) + m_HL_W.at(gset, 4)*pow(V_wind, 4);
						m_q_loss[i] = c_hl*m_L_col.at(gset, 0) / 1000.0;	//[kWt] Thermal loss for each receiver in loop
						m_q_abs[i] = m_q_rec[i] - m_q_loss[i];				//[kWt] Thermal power absorbed by steam in each receiver
					}
					else if (m_HLCharType.at(gset, 0) == 2)
					{
						// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)

						m_q_loss[i] = 0.0;			//[kWt] Thermal loss for each receiver in loop
						m_q_abs[i] = 0.0;			//[kWt] Thermal power absorbed by steam in each receiver

						for (int j = 0; j < 4; j++)
						{
							// Only calculate if the HCE fraction is non-zero
							if (m_HCE_FieldFrac.at(gset, j) <= 0.0)
								continue;

							/*Call the receiver performance model - single point mode
							!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
							!only use for the fluid type in the single point model is calculating the convective heat transfer
							!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and
							!steam that substituting the HTF here introduces negligible error.*/

							// For LF, HT = CT && sca_num = 0
							double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
							q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
							evac_tube_model.EvacReceiver(m_T_ave.at(i, 0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc[i] / m_L_col.at(gset, 0)*1000.0, gset, j, gset, 0, true, m_ncall,
								time / 3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3);

							if (q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs)
							{
								q_rec_loss = 0.0;
								q_rec_abs = 0.0;
							}

							m_q_loss[i] += q_rec_loss*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kW]
							m_q_abs[i] += q_rec_abs*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kWt] Thermal power absorbed by steam in each receiver

						}

					}

					// Set the inlet enthalpy equal to the outlet of the previous node
					if (i > 0)
						m_h_in(i, 0) = m_h_out(i - 1, 0);

					// Calculate the collector outlet enthalpy
					double tol_t = 0.001;
					double err_t = 10.0*tol_t;
					int iter_t = 0;

					while (err_t > tol_t && iter_t < 50)
					{
						iter_t++;
						// Calculate the average enthalpy value in the collector module
						m_h_out.at(i,0) = check_h.check(m_h_in.at(i, 0) + m_q_abs[i] / m_dot_b_guess - (m_T_ave.at(i, 0) - m_T_ave_prev[i])*m_C_thermal/(m_dt*m_dot_b_guess));
						// Update guesses for h_ave and T_ave
						double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i, 0)) / 2.0;
						// Update the average temperature for the heat loss calculation
						water_PH(P_loc*100.0, h_aveg, &wp);
						m_T_ave.at(i, 0) = wp.temp;
						err_t = fabs((h_ave[i] - h_aveg) / h_ave[i]);
						h_ave[i] = h_aveg;
					}

					// Predict the next outlet enthalpy
					if (i < m_nModTot - 1)
						h_ave[i+1] = check_h.check(m_h_in.at(i, 0) + (m_h_out.at(i, 0) - m_h_in.at(i, 0))*1.5);

				}	// End step through boiler receiver modules

				err = (h_b_out - m_h_out.at(m_nModBoil - 1, 0)) / h_b_out;

				if (m_dot_b == m_m_dot_min / m_x_b_des && err > 0.0)		// M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
					break;

				// **************************************************
				// **** Hybrid False Position Iteration Method ******
				// **************************************************
				if (lowflag && upflag)
				{
					if (err > 0.0)
					{
						m_dot_upper = m_dot_b;
						y_upper = err;
					}
					else
					{
						m_dot_lower = m_dot_b;
						y_lower = err;
					}
					m_dot_b_guess = y_upper / (y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				}
				else
				{
					if (err > 0.0)			// Prescribed is greater than calculated, so decrease mass flow and set upper limit
					{
						m_dot_upper = m_dot_b;
						y_upper = err;
						upflag = true;
					}
					else
					{
						m_dot_lower = m_dot_b;
						y_lower = err;
						lowflag = true;
					}

					if (lowflag && upflag)		// If results of bracket are defined, use false position
						m_dot_b_guess = y_upper / (y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
					else		// If not, recalculate value based on approximate energy balance
					{
						if (iter < 3)
						{
							double q_abs_sum = 0.0;
							for (int ii = 0; ii < m_nModBoil; ii++)
								q_abs_sum += m_q_abs[ii];				//[kWt] LOOP total Thermal power absorbed by steam in each receiver
							m_dot_b_guess = q_abs_sum / (h_b_out - h_b_in);
							m_dot_b_guess = max(m_m_dot_min / m_x_b_des*0.5, min(m_dot_b_guess, m_m_dot_b_max*1.5));
						}
						else
							m_dot_b_guess = 0.5*m_dot_upper + 0.5*m_dot_lower;
					}
				}

				// ************************************************************************
				// *** End Hybrid False Position Iteration Method ***********
				// ************************************************************************
				if (m_dot_lower >= m_m_dot_b_max)		// Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
				{
					m_is_def = true;
					break;
				}

				if (m_dot_upper <= m_m_dot_min / m_x_b_des)		// Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
					m_dot_b_guess = m_m_dot_min / m_x_b_des;

			}   // End main BOILER iteration loop

			// Defocus calculations
			m_err_def = (m_dot_b_guess - m_m_dot_b_max) / m_m_dot_b_max;
			if (!m_is_def && m_err_def > 0.0)
				m_is_def = true;
			if (m_is_def)
			{
				// Calculate new defocus
				m_defocus = min(1.0, m_defocus_prev*pow(1.0 / (m_err_def + 1.0), m_rc));
				if (fabs(m_err_def) > m_tol_def)
				{
					m_defocus_prev = m_defocus;
					iter_def++;
					if (iter_def < 11)
					{
						defocus_on = true;
						continue;
					}
				}
			}

			// Superheater
			if (m_is_sh)
			{
				// Choose which geometry set to use
				gset = 0;
				if (m_is_multgeom)
					gset = 1;

				// Calculate superheater inlet enthalpy
				water_PQ(check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_h + m_fP_sf_sh))*100.0, 1.0, &wp);
				double h_sh_in = wp.enth;		//[kJ/kg]
				// The superheater outlet enthalpy is constrained according to the steam mass flow produced in the boiler
				water_TP(m_T_field_out_des, check_pressure.P_check(P_turb_in + dP_basis*m_fP_hdr_h)*100.0, &wp);
				double h_sh_out = wp.enth;		//[kJ/kg]

				// Set the loop inlet enthalpy
				m_h_in.at(m_nModBoil, 0) = h_sh_in;

				// Do a rough guess of the receiver enthalpy for the boiler
				double dh_sh = (h_sh_out - h_sh_in) / (double)m_nModSH;
				for (int ii = 0; ii < m_nModSH; ii++)
				{
					int i = ii + m_nModBoil;
					h_ave[i] = h_sh_in + dh_sh*(double)(ii + 1) - dh_sh / 2.0;
				}

				m_dot = m_dot_b*m_x_b_des;
				double T_sh_guess = m_T_field_out_des;		// Guess the superheater outlet temp

				// Iterative loop to get convergence in heat loss
				double tol_sh = 0.01;
				double err_sh = 10.0*tol_sh;
				int iter_sh = 0;

				while (fabs(err_sh) > tol_sh && iter_sh < 5)
				{
					iter_sh++;

					// Initialize
					for (int i = m_nModBoil; i < m_nModTot; i++)
					{
						m_q_loss[i] = 0.0;			//[kWt] Thermal loss for each receiver in loop
						m_q_abs[i] = 0.0;			//[kWt] Thermal power absorbed by steam in each receiver
					}

					for (int ii = 0; ii < m_nModSH; ii++)
					{
						int i = ii + m_nModBoil;

						// Calculate thermal losses based on temperature guess values
						// Calculate the local pressure in the superheater. Assume a linear pressure drop across each section
						double P_loc = check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_h + m_fP_sf_sh*(1.0 - (double)ii / (double)m_nModSH)));

						// Get the temperature at each state in the boiler
						water_PH(P_loc*100.0, h_ave[i], &wp);
						m_T_ave.at(i, 0) = wp.temp;

						// Calculate the heat loss at each temperature
						if (m_HLCharType.at(gset, 0) == 1)
						{
							// Estimate based on the polynomial adjustments provided by the user
							double dT_loc = m_T_ave.at(i, 0) - T_db;
							double c_hl = m_HL_dT.at(gset, 0) + m_HL_dT.at(gset, 1)*dT_loc + m_HL_dT.at(gset, 2)*pow(dT_loc, 2) + m_HL_dT.at(gset, 3)*pow(dT_loc, 3) + m_HL_dT.at(gset, 4)*pow(dT_loc, 4);	//[W/m] Effect from dT
							if (m_HL_W.at(gset, 0) != 0 || m_HL_W.at(gset, 1) != 0 || m_HL_W.at(gset, 2) != 0 || m_HL_W.at(gset, 3) != 0 || m_HL_W.at(gset, 0) != 0)
								c_hl *= m_HL_W.at(gset, 0) + m_HL_W.at(gset, 1)*V_wind + m_HL_W.at(gset, 2)*pow(V_wind, 2) + m_HL_W.at(gset, 3)*pow(V_wind, 3) + m_HL_W.at(gset, 4)*pow(V_wind, 4);
							m_q_loss[i] = c_hl*m_L_col.at(gset, 0) / 1000.0;		//[kWt] Thermal loss for each receiver in loop
							m_q_abs[i] = m_q_rec[i] - m_q_loss[i];		//[kWt] Thermal power absorbed by steam in each receiver
						}
						else if (m_HLCharType.at(gset, 0) == 2)
						{
							// Calculate thermal loss from Forristall receiver model (algorithm is found in Type 250)

							m_q_loss[i] = 0.0;			//[kWt] Thermal loss for each receiver in loop
							m_q_abs[i] = 0.0;			//[kWt] Thermal power absorbed by steam in each receiver

							for (int j = 0; j < 4; j++)
							{
								// Only calculate if the HCE fraction is non-zero
								if (m_HCE_FieldFrac.at(gset, j) <= 0.0)
									continue;

								/*Call the receiver performance model - single point mode
								!This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The
								!only use for the fluid type in the single point model is calculating the convective heat transfer
								!coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and
								!steam that substituting the HTF here introduces negligible error.*/

								// For LF, HT = CT && sca_num = 0
								double q_rec_loss, q_rec_abs, dum1, dum2, dum3;
								q_rec_loss = q_rec_abs = dum1 = dum2 = dum3 = std::numeric_limits<double>::quiet_NaN();
								evac_tube_model.EvacReceiver(m_T_ave.at(i, 0), 10.0, T_db, T_sky, V_wind, P_amb, defocus_lim*m_q_inc[i] / m_L_col.at(gset, 0)*1000.0, gset, j, gset, 0, true, m_ncall,
									time / 3600.0, q_rec_loss, q_rec_abs, dum1, dum2, dum3);

								if (q_rec_loss != q_rec_loss || q_rec_abs != q_rec_abs)
								{
									q_rec_loss = 0.0;
									q_rec_abs = 0.0;
								}

								m_q_loss[i] += q_rec_loss*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kWt] Thermal loss for each receiver in loop
								m_q_abs[i] += q_rec_abs*m_L_col.at(gset, 0)*m_HCE_FieldFrac.at(gset, j) / 1000.0;		//[kWt] Thermal power absorbed by steam in each receiver

							}

						}

						// Set the inlet enthalpy equal to the outlet of the previous mode
						if (ii > 0)
							m_h_in.at(i, 0) = m_h_out.at(i - 1, 0);

						// Calculate the collector outlet enthalpy
						double tol_t = 0.001;
						double err_t = 10.0*tol_t;
						int iter_t = 0;

						while (err_t > tol_t && iter_t < 50)
						{
							// Calculate the average enthalpy value in the collector module
							m_h_out.at(i,0) = check_h.check(m_h_in.at(i, 0) + m_q_abs[i] / m_dot - (m_T_ave.at(i, 0) - m_T_ave_prev[i])*m_C_thermal/(m_dt*m_dot));
							// Update guesses for h_ave and T_ave
							double h_aveg = (m_h_out.at(i,0) + m_h_in.at(i, 0)) / 2.0;
							// Update the average temperature for the heat loss calculation
							water_PH(P_loc*100.0, h_aveg, &wp);
							m_T_ave.at(i, 0) = wp.temp;
							err_t = fabs((h_ave[i] - h_aveg) / h_ave[i]);
							h_ave[i] = h_aveg;
						}

					}	// Step through superheater receivers

					err_sh = (T_sh_guess - m_T_ave.at(m_nModTot - 1, 0)) / T_sh_guess;
					T_sh_guess = m_T_ave.at(m_nModTot - 1, 0);

				}	// Main superheater iteration

			}		// End superheater
			else
			{
				double m_dot_field = m_dot_b*m_x_b_des*(double)m_nLoops;	//[kg/s] The total field mass flow rate is just the saturated steam coming from the boiler section
			}

			m_dot_b_tot = m_dot_b * (double)m_nLoops;

		}	// End "standard" boiler + sh configuration

	} while (defocus_on);

	// **** Calculate final solar field values ******
	// Total effective solar field mass flow rate
	double m_dot_field = m_dot*(double)m_nLoops;		//[kg/s]

	// Look up temperatures
	water_PH(check_pressure.P_check(P_turb_in + dP_basis*(m_fP_sf_tot - m_fP_hdr_c))*100.0, m_h_in.at(0, 0), &wp);
	double T_field_in = wp.temp - 273.15;		// [C]
	water_PH(check_pressure.P_check(P_turb_in + dP_basis*m_fP_hdr_h)*100.0, m_h_out.at(m_nModTot - 1, 0), &wp);
	double T_loop_out = wp.temp - 273.15;		// [C]

	// Piping thermal loss
	double q_loss_piping = m_Ap_tot * m_Pipe_hl_coef / 1000.0 * ((T_field_in + T_loop_out) / 2.0 - (T_db - 273.15));	// hl coef is [W/m2-K], use average field temp as driving difference

	// Given the piping heat/pressure loss, calculate the temperature at the inlet to the power block
	double h_to_pb = 0.0;
	if (m_dot > 0.0)
		h_to_pb = m_h_out.at(m_nModTot - 1, 0) - q_loss_piping / m_dot_field;
	else
		h_to_pb = m_h_out.at(m_nModTot - 1, 0);

	water_PH(P_turb_in*100.0, h_to_pb, &wp);
	mc_sys_h_out.m_temp = wp.temp;		//[K]

	// Energies
	double q_inc_tot, q_rec_tot, q_abs_tot, q_loss_rec;
	q_inc_tot = q_rec_tot = q_abs_tot = q_loss_rec = 0.0;
	for (int i = 0; i < m_nModTot; i++)
	{
		q_inc_tot += m_q_inc[i];			//[kWt] LOOP total incident beam radiation
		q_rec_tot += m_q_rec[i];			//[kWt] LOOP total incident thermal power on receiver after *optical* losses and *defocus*
		q_abs_tot += m_q_abs[i];			//[kWt] LOOP total Thermal power absorbed by steam in each receiver
		q_loss_rec += m_q_loss[i];			//[kWt] LOOP total thermal losses
	}
	q_inc_tot *= (double)m_nLoops / 1000.0;
	q_rec_tot *= (double)m_nLoops / 1000.0;
	q_abs_tot *= (double)m_nLoops / 1000.0;
	q_loss_rec *= (double)m_nLoops / 1000.0;
	q_loss_piping = q_loss_piping / 1000.0;			//[MW] Thermal losses from the receiver
	double q_loss_sf = q_loss_rec + q_loss_piping;	//[MW] Total solar field losses, receiver + piping loss
	double q_field_delivered = m_dot_field*max(h_to_pb - h_pb_out, 0.0);	//[kW] Energy balance indicating total energy delivered from the solar field
	double h_field_out = h_to_pb;					// h_field_out is corrected later if fossil energy is supplied in topping mode

	// 11.2.15 twn: corrected q_dump equation
	double q_dump = (1.0 - m_defocus)*q_inc_tot;	//[MW] Total amount of energy dumped by collector defocusing
	// this also works, assuming defocus is last multiplier before q_rec_tot:
	//double q_dump = q_inc_tot*(1.0/m_defocus - 1.0);


	double eta_thermal = 0.0;
	if (q_rec_tot > 0.0)
		eta_thermal = 1.0 - min(max(q_loss_sf / q_rec_tot, 0.0), 1.0);	//[-] thermal efficiency after reflection

	// Calculate solar field pressure drops all [bar]
	double dP_tot = dP_basis*m_fP_sf_tot;
	double dP_hdr_c = dP_basis*m_fP_hdr_c;
	double dP_sf_boil = dP_basis*m_fP_sf_boil;
	double dP_boil_to_SH = dP_basis*m_fP_boil_to_sh;
	double dP_sf_sh = dP_basis*m_fP_sf_sh;
	double dP_hdr_h = dP_basis*m_fP_hdr_h;

	// Calculate the total change in energy state
	double E_bal_startup = 0.0;
	double E_field = 0.0;
	for (int i = 0; i < m_nModTot; i++)
	{
		E_bal_startup += (m_T_ave.at(i, 0) - m_T_ave_prev[i] )*m_C_thermal / m_dt*(double)m_nLoops / 1000.0;	//[MW]
		E_field += (m_T_ave.at(i, 0) - m_T_field_ini)*m_C_thermal / m_dt*(double)m_nLoops / 1000.0;				//[MW]
	}


	//***********************************************************************
	//*** Control logic *******************
	//***********************************************************************
	// Check how much power is available from the aux backup
	double q_aux_avail = min(m_q_pb_des*m_ffrac[tou_period], m_q_max_aux);

	/*Fossil mode 3 allows fossil as supplemental temperature boosting. This requires flow from the field.
	it is also possible that the total of the energy from the field plus the topping energy does not
	produce enough to meet the cycle cutoff fraction, so calculate here how much energy would be contributed
	from the fossil contribution */
	if (m_fossil_mode == 3)
	{
		if (q_field_delivered <= 0.0)
			q_aux_avail = 0.0;
		else
		{
			double h_target = 0.0;
			if (m_is_sh)
			{
				water_TP(m_T_field_out_des, P_turb_in*100.0, &wp);
				h_target = wp.enth;
			}
			else
			{
				water_PQ(P_turb_in*100.0, m_x_b_des, &wp);
				h_target = wp.enth;
			}
			// Thermal requirement for the aux heater
			q_aux_avail = max(0.0, (h_target - h_field_out)*m_dot_field);
		}
	}

	// Calculate the total available energy for the power cycle as the sum of the energy from the solar field and the aux backup
	double q_avail_tot = q_aux_avail + max(0.0, q_field_delivered);

	double T_pb_in = 0.0;
	double q_aux_fuel = 0.0;
	double m_dot_to_pb = 0.0;	//[kg/s]
	double q_to_pb = 0.0;
	int standby_control = 0;
	double q_aux = 0.0;
	double m_dot_aux = 0.0;
	// Do we have enough to run the power cycle?
	if (q_avail_tot >= m_q_pb_des*m_cycle_cutoff_frac)
	{
		if (q_aux_avail > 0.0)
		{
			double h_target = 0.0;
			double h_to_pb = 0.0;
			// Calculate the contribution from the aux backup
			switch (m_fossil_mode)
			{
			case 1:			// backup minimum level - parallel
				if (m_is_sh)
				{
					water_TP(m_T_field_out_des, P_turb_in*100.0, &wp);
					h_target = wp.enth;
				}
				else
				{
					water_PQ(P_turb_in*100.0, m_x_b_des, &wp);
					h_target = wp.enth;
				}
				q_aux = max(0.0, q_aux_avail - q_field_delivered);
				m_dot_aux = q_aux / (h_target - h_pb_out);

				if (q_field_delivered > 0.0)
				{
					m_dot_to_pb = m_dot_aux + m_dot_field;
					h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux) / m_dot_to_pb;
				}
				else
				{
					m_dot_to_pb = m_dot_aux;
					h_to_pb = h_target;
				}
				water_PH(P_turb_in*100.0, h_to_pb, &wp);
				q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out);
				break;
			case 2:			// supplemental parallel
				if (m_is_sh)
				{
					water_TP(m_T_field_out_des, P_turb_in*100.0, &wp);
					h_target = wp.enth;
				}
				else
				{
					water_PQ(P_turb_in*100.0, m_x_b_des, &wp);
					h_target = wp.enth;
				}
				q_aux = min(m_q_pb_des - q_field_delivered, q_aux_avail);
				// For parallel operation, the result is a weighted mix of the field output and the boiler
				m_dot_aux = q_aux / (h_target - h_pb_out);
				m_dot_to_pb = 0.0;
				if (q_field_delivered > 0.0)
				{
					m_dot_to_pb = m_dot_aux + m_dot_field;
					h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux) / m_dot_to_pb;
				}
				else
				{
					m_dot_to_pb = m_dot_aux;
					h_to_pb = h_target;
				}
				water_PH(P_turb_in*100.0, h_to_pb, &wp);
				q_to_pb = m_dot_to_pb * (h_to_pb - h_b_in);
				break;
			case 3:		// Supplemental parallel
				// The auxiliary heater is used to bring the steam from the solar field up to the design-point temperature
				// for the power block. The fossil use corresponds to the operation level of the solar field
				if (m_is_sh)
				{
					water_TP(m_T_field_out_des, P_turb_in*100.0, &wp);
					h_target = wp.enth;
				}
				else
				{
					water_PQ(P_turb_in*100.0, m_x_b_des, &wp);
					h_target = wp.enth;
				}
				// The flow rate through the aux heater is the same as through the field
				m_dot_aux = m_dot_field;
				// Thermal requirement for the aux heater
				q_aux = min(max(0.0, h_target - h_field_out)*m_dot_aux, q_aux_avail);
				// Calcualte the enthalpy into the power block
				h_to_pb = h_field_out + q_aux / m_dot_aux;
				m_dot_to_pb = m_dot_field;
				q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out);
				water_PH(P_turb_in*100.0, h_to_pb, &wp);
				T_pb_in = wp.temp - 273.15;
				break;
			}  // end switch
			q_aux_fuel = q_aux / m_LHV_eff;
		}   // end "if q_aux_avail > 0.0
		else
		{
			// No aux backup, just the solar field
			m_dot_to_pb = m_dot_field;
			T_pb_in = mc_sys_h_out.m_temp - 273.15;		//[C]
			q_to_pb = q_field_delivered;
		}
		standby_control = 1;	// We're operating the power block normally
		m_is_pb_on = true;
	}
	else
	{
		// There isn't enough energy to run the power block

		// Do we have enough to do standby?
		if (q_avail_tot > m_q_pb_des*m_q_sby_frac && m_t_sby_prev > 0.0 && m_is_pb_on_prev)
		{
			standby_control = 2;	// Operate in standby mode
			m_t_sby = max(0.0, m_t_sby_prev - m_dt / 3600.0);
			q_aux = max(m_q_sby_frac*m_q_pb_des - q_field_delivered, 0.0);
			m_dot_aux = 0.0;		// It's not meaningful to report the aux mass flow rate
			q_aux_fuel = q_aux / m_LHV_eff;
			q_to_pb = m_q_sby_frac*m_q_pb_des;
			T_pb_in = m_T_field_in_des;
			m_is_pb_on = true;
		}
		else
		{
			standby_control = 3;		// Turn off power block
			m_t_sby = m_t_sby_des;		// Reset standby time
			q_aux = 0.0;
			m_dot_aux = 0.0;
			q_field_delivered = 0.0;
			q_aux_fuel = 0.0;
			q_to_pb = 0.0;
			T_pb_in = mc_sys_h_out.m_temp - 273.15;		//[C]
			m_is_pb_on = false;
		}
		m_dot_to_pb = 0.0;
	}

	// *** Calculate final plant values ****
	// Parasitic losses associated with the operation of the aux boiler
	double W_dot_aux = 0.0;
	if (q_aux > 0.0)
		W_dot_aux = m_W_pb_des / 1000.0 * m_aux_array[0] * m_aux_array[1] * (m_aux_array[2] + m_aux_array[3] * (q_aux / m_q_pb_des) + m_aux_array[4] * pow(q_aux / m_q_pb_des, 2));	//[MWe]

	// Balance of plant parasitics as a function of power block load
	double W_dot_bop = 0.0;
	if (q_to_pb > 0.0)
		W_dot_bop = m_W_pb_des / 1000.0 * m_bop_array[0] * m_bop_array[1] * (m_bop_array[2] + m_bop_array[3] * (q_to_pb / m_q_pb_des) + m_bop_array[4] * pow(q_to_pb / m_q_pb_des, 2));	//[MWe]

	// Parasitic electric power consumed by the collectors
	double W_dot_col = 0.0;
	if (CSP::pi / 2.0 - SolarZen > 0.0)
		W_dot_col = m_ftrack*m_SCA_drives_elec*m_Ap_tot / 1.E6;	//[MWe]

	double W_dot_fixed = m_PB_fixed_par*m_W_pb_des / 1000.0;	//[MWe] Fixed parasitic power losses... for every hour of operation

	// If the power block isn't running, there's only a small pressure differential across the field
	if (q_to_pb <= 0.0)
	{
		P_turb_in = m_P_turb_des * turb_pres_frac(m_cycle_cutoff_frac, m_fossil_mode, m_ffrac[tou_period], m_fP_turb_min);
		// m_cycle_cutoff_frac  mjw 11.15.11 The cycle cutoff restriction occasionally allows low pressure and steaming in the feedwater line. Avoid this.
		dP_hdr_c = dP_basis * m_fP_hdr_c;
		dP_sf_boil = dP_basis * m_fP_sf_boil;
		dP_boil_to_SH = dP_basis * m_fP_boil_to_sh;
		dP_sf_sh = dP_basis * m_fP_sf_sh;
		dP_hdr_h = dP_basis * m_fP_hdr_h;
		dP_tot = dP_basis * (m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h);
	}

	// Feedwater pump parasitic
	water_PQ(check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h))*100.0, 0.0, &wp);
	double T_low_limit = wp.temp;
	water_TP(min(T_pb_out, T_low_limit), check_pressure.P_check(P_turb_in + dP_basis*(m_fP_hdr_c + m_fP_sf_boil + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h))*100.0, &wp);
	double rho_fw = wp.dens;

	double W_dot_pump = 0.0;
	if (m_is_oncethru)
		W_dot_pump = m_dot_field*dP_tot / rho_fw / m_eta_pump*0.1;
	else
		W_dot_pump = (m_dot_field*(m_fP_hdr_c + m_fP_boil_to_sh + m_fP_sf_sh + m_fP_hdr_h) + m_dot_field / m_x_b_des*m_fP_sf_boil)*dP_basis / rho_fw / m_eta_pump*0.1;	//[MWe] P_turb in in bar

	// Solar field efficiency
	double eta_sf = eta_opt_ave * eta_thermal;

	// Limit the reported values of solar azimuth/elevation
	double solaralt = max(0.0, CSP::pi / 2 - SolarZen);
	if (SolarZen >= CSP::pi / 2)
		SolarAz = 0.0;

	// Set Outputs
	//reference
	//bConnected &= connect(type261_solarfield, "T_field_out", type234_powerblock, "T_hot");		//[C] connect to LF solarfield
	//bConnected &= connect(type261_solarfield, "m_dot_to_pb", type234_powerblock, "m_dot_st");		//[kg/hr] connect to LF solarfield
	//bConnected &= connect(type261_solarfield, "standby_control", type234_powerblock, "standby_control");
	//bConnected &= connect(type261_solarfield, "dP_sf_sh", type234_powerblock, "dp_sh");			//[Pa] Pressure drop in sh
	//bConnected &= connect(type261_solarfield, "W_dot_par_tot", type261_summarizer, "W_par_sf_tot");
	//reference

	cr_out_solver.m_T_salt_hot = mc_sys_h_out.m_temp - 273.15;	//[C] not really "salt", but ok
	cr_out_solver.m_m_dot_salt_tot = m_dot_to_pb*3600.0;		//[kg/hr] not really "salt", but ok
	cr_out_solver.m_standby_control = standby_control;			//[-]
	cr_out_solver.m_dP_sf_sh = dP_sf_sh;						//[bar]

	cr_out_solver.m_W_dot_col_tracking = W_dot_col;				//[MWe]
	cr_out_solver.m_W_dot_htf_pump = W_dot_aux + W_dot_bop + W_dot_fixed + W_dot_pump;	//[MWe]
	//cr_out_solver.m_W_dot_par_tot = W_dot_aux + W_dot_bop + W_dot_col + W_dot_fixed + W_dot_pump;	//[MWe]
	//value(O_CYCLE_PL_CONTROL, cycle_pl_control);        //[none] Part-load control flag - used by Type224
	//value(O_DP_TOT, dP_tot);							  //[bar] Total HTF pressure drop
	//value(O_DP_HDR_C, dP_hdr_c);						  //[bar] Average cold header pressure drop
	//value(O_DP_SF_BOIL, dP_sf_boil);					  //[bar] Pressure drop across the solar field boiler
	//value(O_DP_BOIL_TO_SH, dP_boil_to_SH);			  //[bar] Pressure drop between the boiler and superheater
	//value(O_DP_SF_SH, dP_sf_sh);						  //[bar] Pressure drop across the solar field superheater
	//value(O_DP_HDR_H, dP_hdr_h);						  //[bar] Average hot header pressure drop
	//value(O_E_BAL_STARTUP, E_bal_startup);			  //[MW] Startup energy consumed
	//value(O_E_FIELD, E_field);						  //[MW-hr] Accumulated internal energy in the entire solar field
	//value(O_E_FP_TOT, 0.0);							  //[J] Freeze protection energy
	//value(O_ETA_OPT_AVE, eta_opt_ave);				  //[none] Collector equivalent optical efficiency
	//value(O_ETA_THERMAL, eta_thermal);				  //[none] Solar field thermal efficiency (power out/ANI)
	//value(O_ETA_SF, eta_sf);    						  //[none] Total solar field collection efficiency
	//value(O_DEFOCUS, m_defocus);     					  //[none] The fraction of focused aperture area in the solar field
	//value(O_M_DOT_AUX, m_dot_aux * 3600);				  //[kg/s] --> [kg/hr] Auxiliary heater mass flow rate
	//value(O_M_DOT_FIELD, m_dot_field * 3600);			  //[kg/s] --> [kg/hr] Flow rate from the field
	//value(O_M_DOT_B_TOT, m_dot_b_tot * 3600);			  //[kg/s] --> [kg/hr] Flow rate within the boiler section
	//value(O_M_DOT, m_dot);							  //[kg/s] Flow rate in a single loop
	//value(O_M_DOT_TO_PB, m_dot_to_pb * 3600);			  //[kg/s] --> [kg/hr] Flow rate delivered to the power block
	//value(O_P_TURB_IN, P_turb_in);			               //[bar] Pressure at the turbine inlet
	//value(O_Q_LOSS_PIPING, q_loss_piping);				   //[MW] Pipe heat loss in the hot header and the hot runner
	//value(O_Q_AUX_FLUID, q_aux*0.001);					   //[MW] Thermal energy provided to the fluid passing through the aux heater
	//value(O_Q_AUX_FUEL, q_aux_fuel*3.412E-3);				   //[W] --> [MMBTU] Heat content of fuel required to provide aux firing
	//value(O_Q_DUMP, q_dump);								   //[MW] Dumped thermal energy
	//value(O_Q_FIELD_DELIVERED, q_field_delivered*0.001);	   //[kW] --> [MW] Total solar field thermal power delivered
	//value(O_Q_INC_TOT, q_inc_tot);						   //[MW] Total power incident on the field
	//value(O_Q_LOSS_REC, q_loss_rec);						   //[MW] Total Receiver thermal losses
	//value(O_Q_LOSS_SF, q_loss_sf);						   //[MW] Total solar field thermal losses
	//value(O_Q_TO_PB, q_to_pb*0.001);						   //[kW] --> [MW] Thermal energy to the power block
	//value(O_SOLARALT, solaralt*57.2958);					   //[rad] --> [deg] Solar altitude used in optical calculations
	//value(O_SOLARAZ, SolarAz*57.2958);					   //[rad] --> [deg] Solar azimuth used in optical calculations
	//value(O_PHI_T, phi_t*57.2958);						   //[rad] --> [deg] Transversal solar incidence angle
	//value(O_THETA_L, theta_L*57.2958);					   //[rad] --> [deg] Longitudinal solar incidence angle
	//value(O_STANDBY_CONTROL, standby_control);			   //[none] Standby control flag - used by Type224
	//value(O_T_FIELD_IN, m_T_field_in);						   //[C] HTF temperature into the collector field header
	//value(O_T_FIELD_OUT, m_T_field_out);					   //[C] HTF Temperature from the field
	//value(O_T_LOOP_OUT, T_loop_out);						   //[C] Loop outlet temperature
	//value(O_T_PB_IN, T_pb_in);							   //[C] HTF Temperature to the power block
	//value(O_W_DOT_AUX, W_dot_aux);						   //[MW] Parasitic power associated with operation of the aux boiler
	//value(O_W_DOT_BOP, W_dot_bop);						   //[MW] parasitic power as a function of power block load
	//value(O_W_DOT_COL, W_dot_col);						   //[MW] Parasitic electric power consumed by the collectors
	//value(O_W_DOT_FIXED, W_dot_fixed);					   //[MW] Fixed parasitic power losses.. for every hour of operation
	//value(O_W_DOT_PUMP, W_dot_pump);						   //[MW] Required solar field pumping power
	//value(O_W_DOT_PAR_TOT, W_dot_aux + W_dot_bop + W_dot_col + W_dot_fixed + W_dot_pump);	//[MW] Total parasitic power losses
	//value(O_P_SF_IN, P_turb_in + dP_tot);						//[bar] Solar field inlet pressure

	return;


}
