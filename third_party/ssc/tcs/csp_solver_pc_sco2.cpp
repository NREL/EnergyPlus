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

#include "csp_solver_pc_sco2.h"
#include "csp_solver_core.h"

#include "htf_props.h"

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_pc_sco2::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_Q_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_M_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_Q_DOT_STARTUP, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_W_DOT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_T_HTF_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_T_HTF_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_sco2::E_M_DOT_WATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	csp_info_invalid
};

C_pc_sco2::C_pc_sco2()
{
	m_q_dot_design = m_q_dot_standby = m_q_dot_max = m_q_dot_min = m_startup_energy_required = 
		m_W_dot_des = m_T_htf_cold_des = m_m_dot_htf_des =
		m_startup_time_remain_prev = m_startup_energy_remain_prev =
		m_startup_time_remain_calc = m_startup_energy_remain_calc = std::numeric_limits<double>::quiet_NaN();

	m_standby_control_prev = m_standby_control_calc = -1;

	mc_reported_outputs.construct(S_output_info);
}

void C_pc_sco2::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	// Call the sCO2 Recompression Cycle class to design the cycle
	mc_sco2_recomp.design(ms_params.ms_mc_sco2_recomp_params);
	
	// Setup HTF class
	if( ms_params.ms_mc_sco2_recomp_params.m_hot_fl_code != HTFProperties::User_defined && ms_params.ms_mc_sco2_recomp_params.m_hot_fl_code < HTFProperties::End_Library_Fluids )
	{
		if( !mc_pc_htfProps.SetFluid(ms_params.ms_mc_sco2_recomp_params.m_hot_fl_code) )
		{
			throw(C_csp_exception("Power cycle HTF code is not recognized", "sCO2 Power Cycle Initialization"));
		}
	}
	else if( ms_params.ms_mc_sco2_recomp_params.m_hot_fl_code == HTFProperties::User_defined )
	{
		// Check that 'm_field_fl_props' is allocated and correct dimensions
		int n_rows = (int)ms_params.ms_mc_sco2_recomp_params.mc_hot_fl_props.nrows();
		int n_cols = (int)ms_params.ms_mc_sco2_recomp_params.mc_hot_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_pc_htfProps.SetUserDefinedFluid(ms_params.ms_mc_sco2_recomp_params.mc_hot_fl_props) )
			{
				std::string error_msg = util::format(mc_pc_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "sCO2 Power Cycle Initialization"));
			}
		}
		else
		{
			std::string error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "sCO2 Power Cycle Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Power cycle HTF code is not recognized", "sCO2 Power Cycle Initialization"));
	}

	// Set solved paramaters and calculate timestep dependent information
	solved_params.m_W_dot_des = mc_sco2_recomp.get_design_solved()->ms_rc_cycle_solved.m_W_dot_net / 1.E3;	//[MWe] convert from kWe
	m_W_dot_des = solved_params.m_W_dot_des;		//[MWe] Net power from cycle NOT counting cooling parasitics
	solved_params.m_eta_des = mc_sco2_recomp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal;			//[-]
	m_q_dot_design = solved_params.m_W_dot_des / solved_params.m_eta_des;			//[MWt]
	solved_params.m_q_dot_des = m_q_dot_design;										//[MWt]
	
	// Calculate the startup energy needed
	m_startup_energy_required = ms_params.m_startup_frac*solved_params.m_q_dot_des*1.E3;			//[kWt-hr]	
	solved_params.m_q_startup = m_startup_energy_required/1.E3;										//[MWt-hr]
	solved_params.m_max_frac = ms_params.m_cycle_max_frac;					//[-]
	solved_params.m_cutoff_frac = ms_params.m_cycle_cutoff_frac;			//[-]
	solved_params.m_sb_frac = ms_params.m_q_sby_frac;						//[-]
	solved_params.m_T_htf_hot_ref = ms_params.ms_mc_sco2_recomp_params.m_T_htf_hot_in - 273.15;	//[C]
	solved_params.m_m_dot_design = mc_sco2_recomp.get_phx_des_par()->m_m_dot_hot_des*3600.0;	//[kg/hr]
	solved_params.m_m_dot_min = solved_params.m_m_dot_design*solved_params.m_cutoff_frac;	//[kg/hr]
	solved_params.m_m_dot_max = solved_params.m_m_dot_design*solved_params.m_max_frac;		//[kg/hr]
	m_m_dot_htf_des = solved_params.m_m_dot_design;		//[kg/hr]

	// Calculate the standby thermal power requirement
	m_q_dot_standby = ms_params.m_q_sby_frac * m_q_dot_design;		//[MWt]
	// and max/min thermal power to cycle
	m_q_dot_max = ms_params.m_cycle_max_frac * m_q_dot_design;		//[MWt]
	m_q_dot_min = ms_params.m_cycle_cutoff_frac * m_q_dot_design;	//[MWt]
	// and calculated cold HTF return temperature
	m_T_htf_cold_des = mc_sco2_recomp.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]

	// Finally, set member model-timestep-tracking variables
	m_standby_control_prev = OFF;			// Assume power cycle is off when simulation begins
	m_startup_energy_remain_prev = m_startup_energy_required;		//[kWt-hr]
	m_startup_time_remain_prev = ms_params.m_startup_time;			//[hr]

}

int C_pc_sco2::get_operating_state()
{
	if( ms_params.m_startup_frac == 0.0 && ms_params.m_startup_time == 0.0 )
	{
		return C_csp_power_cycle::ON;
	}

	return m_standby_control_prev;
}

double C_pc_sco2::get_cold_startup_time()
{
	return ms_params.m_startup_time;	//[hr]
}

double C_pc_sco2::get_warm_startup_time()
{
	//startup time from warm state. No differentiation between cold/hot yet.
	return ms_params.m_startup_time;	//[hr]
}

double C_pc_sco2::get_hot_startup_time()
{
	//startup time from warm state. No differentiation between cold/warm yet.
	return ms_params.m_startup_time;	//[hr]
}

double C_pc_sco2::get_standby_energy_requirement()
{
	return m_q_dot_standby;		//[MWt]
}

double C_pc_sco2::get_cold_startup_energy()
{
	return m_startup_energy_required/1.E3;	//[MWt-hr]
}

double C_pc_sco2::get_warm_startup_energy()
{
	return m_startup_energy_required / 1.E3;	//[MWt-hr]
}

double C_pc_sco2::get_hot_startup_energy()
{
	return m_startup_energy_required / 1.E3;	//[MWt-hr]
}

double C_pc_sco2::get_max_thermal_power()
{
	return m_q_dot_max;		//[MWt]
}

double C_pc_sco2::get_min_thermal_power()
{
	return m_q_dot_min;		//[MWt]
}

void C_pc_sco2::get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max)
{
	throw(C_csp_exception("C_pc_sco2::get_max_power_output_operation_constraints() is not complete"));

	return;	//[-]
}

double C_pc_sco2::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser)
{
	throw(C_csp_exception("C_pc_sco2::get_efficiency_at_TPH() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_sco2::get_efficiency_at_load(double load_frac, double *w_dot_condenser)
{
	throw(C_csp_exception("C_pc_sco2::get_efficiency_at_load() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

// This can vary between timesteps for Type224, depending on remaining startup energy and time
double C_pc_sco2::get_max_q_pc_startup()
{
	if( m_startup_time_remain_prev > 0.0 )
		return fmin(m_q_dot_max,
		m_startup_energy_remain_prev / 1.E3 / m_startup_time_remain_prev);		//[MWt]
	else if( m_startup_energy_remain_prev > 0.0 )
	{
		return m_q_dot_max;    //[MWt]
	}
	else
	{
		return 0.0;
	}
}

double C_pc_sco2::get_htf_pumping_parasitic_coef()
{
	return ms_params.m_htf_pump_coef* (m_m_dot_htf_des / 3600.) / (m_q_dot_design*1000.0);	// kWe/kWt
}

void C_pc_sco2::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs &inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	//C_csp_power_cycle::S_csp_pc_out_report &out_report,
	const C_csp_solver_sim_info &sim_info)
{
	// Get sim info
	double step_sec = sim_info.ms_ts.m_step;		//[s]
	
	// Check and convert inputs
	double T_htf_hot = htf_state_in.m_temp+273.15;	//[K], convert from C 
	double m_dot_htf = inputs.m_m_dot;				//[kg/hr]
	
	int standby_control = inputs.m_standby_control;			//[-] 1: On, 2: Standby, 3: Off
	m_standby_control_calc = standby_control;

	double P_cycle, eta, T_htf_cold, m_dot_demand, W_cool_par;
	P_cycle = eta = T_htf_cold = m_dot_demand = W_cool_par = std::numeric_limits<double>::quiet_NaN();

	double time_required_su = 0.0;
	double q_startup = 0.0;

	double q_dot_htf = std::numeric_limits<double>::quiet_NaN();	//[MWt]
	
	bool was_method_successful = true;

	switch( standby_control )
	{
	case STARTUP:
		{
			double c_htf = mc_pc_htfProps.Cp((T_htf_hot + m_T_htf_cold_des) / 2.0);		//[kJ/kg-K]

			double time_required_su_energy = m_startup_energy_remain_prev / (m_dot_htf*c_htf*(T_htf_hot - m_T_htf_cold_des) / 3600);	//[hr]
			double time_required_su_ramping = m_startup_time_remain_prev;	//[hr]

			double time_required_max = fmax(time_required_su_energy, time_required_su_ramping);

			double time_step_hrs = step_sec / 3600.0;	//[hr]


			if( time_required_max > time_step_hrs )
			{
				time_required_su = time_step_hrs;		//[hr]
				m_standby_control_calc = STARTUP;	//[-] Power cycle requires additional startup next timestep
				q_startup = m_dot_htf*c_htf*(T_htf_hot - m_T_htf_cold_des)*time_step_hrs / 3600.0;	//[kW-hr]
			}
			else
			{
				time_required_su = time_required_max;	//[hr]
				m_standby_control_calc = ON;	//[-] Power cycle has started up, next time step it will be ON

				double q_startup_energy_req = m_startup_energy_remain_prev;	//[kWt-hr]
				double q_startup_ramping_req = m_dot_htf*c_htf*(T_htf_hot - m_T_htf_cold_des)*m_startup_time_remain_prev / 3600.0;	//[kWt-hr]
				q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);	//[kWt-hr]

				// ******************

			}

			m_startup_time_remain_calc = fmax(m_startup_time_remain_prev - time_required_su, 0.0);	//[hr]
			m_startup_energy_remain_calc = fmax(m_startup_energy_remain_prev - q_startup, 0.0);		//[kWt-hr]
		}

		q_dot_htf = q_startup / 1000.0 / (time_required_su);	//[kWt-hr] * [MW/kW] * [1/hr] = [MWt]

		// *****
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = m_T_htf_cold_des;		//[K]
		// *****
		m_dot_demand = 0.0;
		W_cool_par = 0.0;

		was_method_successful = true;

		break;

	case ON:
		{
			C_sco2_phx_air_cooler::S_od_par sco2_rc_od_par;
			sco2_rc_od_par.m_T_htf_hot = T_htf_hot;				//[K]
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf/3600.0;		//[kg/s]
			sco2_rc_od_par.m_T_amb = weather.m_tdry+273.15;		//[K]
            sco2_rc_od_par.m_T_t_in_mode = C_sco2_cycle_core::E_SOLVE_PHX;  //[-]

			C_sco2_phx_air_cooler::E_off_design_strategies od_strategy = C_sco2_phx_air_cooler::E_TARGET_POWER_ETA_MAX;

			int off_design_code = 0;
			try
			{
				off_design_code = mc_sco2_recomp.off_design__constant_N__T_mc_in_P_LP_in__objective(sco2_rc_od_par,
                                                            true, 1.0,
                                                            true, 1.0,
                                                            true, 1.0,
                                                            false, std::numeric_limits<double>::quiet_NaN(),
                                                            od_strategy, 1.E-3, 1.E-3);
			}
			catch( C_csp_exception &csp_exception )
			{
				throw(C_csp_exception(csp_exception.m_error_message, "sCO2 power cycle"));		
			}

			// Was power cycle simulations successful?
			if(off_design_code == 0)
			{
				P_cycle = mc_sco2_recomp.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net;	//[kWe]
				eta = mc_sco2_recomp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				T_htf_cold = mc_sco2_recomp.get_od_solved()->ms_phx_od_solved.m_T_h_out;		//[K]
				q_dot_htf = P_cycle/eta/1.E3;		//[MWt]
			
				W_cool_par = 0.0;

				m_dot_demand = 0.0;

				was_method_successful = true;
			}
			else
			{
				P_cycle = 0.0;
				eta = 0.0;
				T_htf_cold = m_T_htf_cold_des;		//[K]
				q_dot_htf = 0.0;

				W_cool_par = 0.0;

				m_dot_demand = 0.0;

				was_method_successful = false;
			}
		}

		break;

	case STANDBY:
		{
			double c_htf = mc_pc_htfProps.Cp((T_htf_hot + m_T_htf_cold_des) / 2.0);		//[kJ/kg-K]

			// Calculate the actual q_sby_needed from the reference flows
			double q_sby_needed = m_q_dot_standby;		//[MWt]

			// now calculate the mass flow rate knowing the inlet temperature of the salt,
			// ..and holding the outlet temperature at the reference outlet temperature
			double m_dot_sby = q_sby_needed / (c_htf * (T_htf_hot - m_T_htf_cold_des))*3600.0;

			// Set other output values
			P_cycle = 0.0;
			eta = 0.0;
			T_htf_cold = m_T_htf_cold_des;
			m_dot_demand = m_dot_sby;
			W_cool_par = 0.0;

			q_dot_htf = m_dot_htf / 3600.0*c_htf*(T_htf_hot - T_htf_cold) / 1000.0;		//[MWt]

			was_method_successful = true;
		}

		break;

	case OFF:

		// Set other output values
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = m_T_htf_cold_des;
		m_dot_demand = 0.0;
		W_cool_par = 0.0;

		q_dot_htf = 0.0;

		// Cycle is off, so reset startup parameters!
		m_startup_time_remain_calc = ms_params.m_startup_time;			//[hr]
		m_startup_energy_remain_calc = m_startup_energy_required;		//[kWt-hr]

		was_method_successful = true;

		break;

	case STARTUP_CONTROLLED:
		// Thermal input can be controlled (e.g. TES mass flow rate is adjustable, rather than direct connection
		//     to the receiver), so find the mass flow rate that results in the required energy input can be achieved
		//     simultaneously with the required startup time. If the timestep is less than the required startup time
		//     scale the mass flow rate appropriately

		double c_htf = mc_pc_htfProps.Cp((T_htf_hot + m_T_htf_cold_des) / 2.0);		//[kJ/kg-K]

		// Maximum thermal power to power cycle based on design conditions:
		double q_dot_to_pc_max = m_q_dot_max*1.E3;		//[kWt]

		double time_required_su_energy = m_startup_energy_remain_prev / q_dot_to_pc_max;		//[hr]
		double time_required_su_ramping = m_startup_time_remain_prev;		//[hr]

		if( time_required_su_energy > time_required_su_ramping )	// Meeting energy requirements (at design thermal input) will require more time than time requirements
		{
			// Can the power cycle startup within the timestep?
			if( time_required_su_energy > step_sec / 3600.0 )	// No: the power cycle startup will require another timestep
			{
				time_required_su = step_sec / 3600.0;	//[hr]
				m_standby_control_calc = STARTUP;		//[-] Power cycle requires additional startup next timestep

			}
			else	// Yes: the power cycle will complete startup within this timestep
			{
				time_required_su = time_required_su_energy;	//[hr]
				m_standby_control_calc = ON;				//[-] Power cycle has started up, next time step it will be ON
			}
		}
		else		// Meeting time requirements will require more time than energy requirements (at design thermal input)
		{
			// Can the power cycle startup within the timestep?
			if( time_required_su_ramping > step_sec / 3600.0 )	// No: the power cycle startup will require another timestep
			{
				time_required_su = step_sec / 3600.0;			//[hr]
				m_standby_control_calc = STARTUP;		//[-] Power cycle requires additional startup next timestep
			}
			else	// Yes: the power cycle will complete startup within this timestep
			{
				time_required_su = time_required_su_ramping;	//[hr]
				m_standby_control_calc = ON;					//[-] Power cycle has started up, next time step it will be ON
			}
		}
		q_startup = q_dot_to_pc_max*time_required_su;	//[kWt-hr]

		double m_dot_htf_required = (q_startup / time_required_su) / (c_htf*(T_htf_hot - m_T_htf_cold_des));	//[kg/s]

		m_startup_time_remain_calc = fmax(m_startup_time_remain_prev - time_required_su, 0.0);	//[hr]
		m_startup_energy_remain_calc = fmax(m_startup_energy_remain_prev - q_startup, 0.0);		//[kWt-hr]	


		// Set other output values
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = m_T_htf_cold_des;
		m_dot_htf = m_dot_htf_required*3600.0;		//[kg/hr], convert from kg/s
		//m_dot_demand = m_dot_htf_required*3600.0;		//[kg/hr], convert from kg/s
		W_cool_par = 0.0;

		q_dot_htf = m_dot_htf_required*c_htf*(T_htf_hot - m_T_htf_cold_des) / 1000.0;	//[MWt]

		was_method_successful = true;

		break;

	}	// end switch() on standby control

	// Set outputs
	out_solver.m_P_cycle = P_cycle / 1000.0;			//[MWe] Cycle power output, convert from kWe
	mc_reported_outputs.value(E_ETA_THERMAL,eta);		//[-] Cycle thermal efficiency
	out_solver.m_T_htf_cold = T_htf_cold-273.15;		//[C] HTF outlet temperature
	mc_reported_outputs.value(E_M_DOT_WATER, 0.0);		//[kg/hr] Cooling water makeup flow rate, convert from kg/s
	//out_report.m_m_dot_demand = m_dot_demand;			//[kg/hr] HTF required flow rate to meet power load
	out_solver.m_m_dot_htf = m_dot_htf;					//[kg/hr] Actual HTF flow rate passing through the power cycle
	//out_report.m_m_dot_htf_ref = m_m_dot_htf_des;		//[kg/hr] Calculated reference HTF flow rate at design
	out_solver.m_W_cool_par = W_cool_par;				//[MWe] Cooling system parasitic load
	//out_report.m_P_ref = m_W_dot_des;					//[MWe] Reference power level output at design not counting cooling parasitics
	//out_report.m_f_hrsys = 0.0;							//[-] Fraction of operating heat rejection system
	//out_report.m_P_cond = 0.0;							//[Pa] Condenser pressure

	//outputs.m_q_startup = q_startup / 1.E3;					//[MWt-hr] Startup energy
	double q_dot_startup = 0.0;
	if( q_startup > 0.0 )
		q_dot_startup = q_startup / 1.E3 / time_required_su;	//[MWt] Startup thermal power
	else
		q_dot_startup = 0.0;

	out_solver.m_time_required_su = time_required_su*3600.0;	//[s]
	out_solver.m_q_dot_htf = q_dot_htf;						//[MWt] Thermal power from HTF (= thermal power into cycle)
	out_solver.m_W_dot_htf_pump = ms_params.m_htf_pump_coef*(m_dot_htf / 3.6E6);	//[MW] HTF pumping power, convert from [kW/kg/s]*[kg/hr]    

	out_solver.m_was_method_successful = was_method_successful;	//[-]
}

void C_pc_sco2::converged()
{
	m_standby_control_prev = m_standby_control_calc;
	m_startup_time_remain_prev = m_startup_time_remain_calc;
	m_startup_energy_remain_prev = m_startup_energy_remain_calc;

	mc_reported_outputs.set_timestep_outputs();
}

void C_pc_sco2::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_pc_sco2::assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}
