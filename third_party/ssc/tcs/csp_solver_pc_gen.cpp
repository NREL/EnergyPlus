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

#include <algorithm>
#include <math.h>
#include "csp_solver_pc_gen.h"
#include "csp_solver_core.h"

#include "lib_util.h"

static C_csp_reported_outputs::S_output_info S_output_info[] = 
{
	{C_pc_gen::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	csp_info_invalid
};

C_pc_gen::C_pc_gen()
{
	// *************************************************************
	// Define temperature and cp values so code interfaces with solver
	// *************************************************************
	m_T_htf_cold_fixed = 300.0 + 273.15;	//[K]
	m_T_htf_hot_fixed = 500.0 + 273.15;		//[K]
	m_cp_htf_fixed = 2.0;					//[kJ/kg-K]
	// *************************************************************

	m_q_startup_remain = m_q_startup_used =
		m_q_des = m_qttmin = m_qttmax = std::numeric_limits<double>::quiet_NaN();

	m_pc_mode_prev = m_pc_mode = -1;

	mc_reported_outputs.construct(S_output_info);
}

void C_pc_gen::get_fixed_properties(double &T_htf_cold_fixed /*K*/, double &T_htf_hot_fixed /*K*/, double &cp_htf_fixed /*K*/)
{
	T_htf_cold_fixed = m_T_htf_cold_fixed;	//[K]
	T_htf_hot_fixed = m_T_htf_hot_fixed;	//[K]
	cp_htf_fixed = m_cp_htf_fixed;			//[kJ/kg-K]
}

void C_pc_gen::check_double_params_are_set()
{
	if( !check_double(ms_params.m_W_dot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_W_dot_des"));
	}
	if( !check_double(ms_params.m_eta_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_eta_des"));
	}
	if( !check_double(ms_params.m_f_wmax) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_f_wmax"));
	}
	if( !check_double(ms_params.m_f_wmin) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_f_wmin"));
	}
	if( !check_double(ms_params.m_f_startup) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_f_startup"));
	}
	if( !check_double(ms_params.m_T_pc_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_T_pc_des"));
	}
}

void C_pc_gen::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	// Check that ms_params are set
	check_double_params_are_set();

	// Sanity-check other parameters here:
	if(ms_params.m_PC_T_corr < 1 || ms_params.m_PC_T_corr > 2)
	{
		std::string msg = util::format("The power cycle temperature correction mode must be "
			"1 (Wet Bulb) or 2 (Dry Bulb). The input value was %d, so it was reset to 2.", ms_params.m_PC_T_corr);
		mc_csp_messages.add_notice(msg);
		ms_params.m_PC_T_corr = 2;
	}
	if( ms_params.mv_etaQ_coefs.size() < 1 )
	{
		throw(C_csp_exception("C_csp_gen_pc::init", 
			"The model requires at least one part-load power cycle efficiency coefficient (mv_etaQ_coefs)"));
	}
	if( ms_params.mv_etaT_coefs.size() < 1 )
	{
		throw(C_csp_exception("C_csp_gen_pc::init",
			"The model requires at least one temperature correction power cycle efficiency coefficient (mv_etaT_coefs)"));
	}

	// Unit conversion
	ms_params.m_T_pc_des += 273.15;		//[K], convert from C

	// Initial power cycle calculations
		// Design thermal input
	m_q_des = ms_params.m_W_dot_des / ms_params.m_eta_des;		//[MWt]
		// Min/max thermal inputs
	m_qttmin = m_q_des*ms_params.m_f_wmin;		//[MWt]
	m_qttmax = m_q_des*ms_params.m_f_wmax;		//[MWt]
		// Set initial values for power cycle mode and startup requirements
	m_q_startup_remain = m_q_des*ms_params.m_f_startup;	//[MWt-hr]
	m_pc_mode_prev = 0;

	// ************************
	// Set solved parameters
	solved_params.m_W_dot_des = ms_params.m_W_dot_des;	//[MWe]
	solved_params.m_eta_des = ms_params.m_eta_des;		//[-]
	solved_params.m_q_dot_des = m_q_des;				//[MWt]
	solved_params.m_q_startup = m_q_startup_remain;		//[MWt-hr]
	solved_params.m_max_frac = ms_params.m_f_wmax;		//[-]
	solved_params.m_cutoff_frac = ms_params.m_f_wmin;	//[-]
	solved_params.m_sb_frac = 0.0;						//[-]
	solved_params.m_T_htf_hot_ref = m_T_htf_hot_fixed-273.15;	//[C]
	solved_params.m_m_dot_design = m_q_des*1.E3/(m_cp_htf_fixed*(m_T_htf_hot_fixed - m_T_htf_cold_fixed))*3600.0;	//[kg/hr]
	solved_params.m_m_dot_min = solved_params.m_m_dot_design*solved_params.m_cutoff_frac;	//[kg/hr]
	solved_params.m_m_dot_max = solved_params.m_m_dot_design*solved_params.m_max_frac;		//[kg/hr]
}

int C_pc_gen::get_operating_state()
{
	return m_pc_mode_prev;		//[-]
}

double C_pc_gen::get_cold_startup_time()
{
	throw(C_csp_exception("C_csp_gen_pc::get_cold_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_gen::get_warm_startup_time()
{
	throw(C_csp_exception("C_csp_gen_pc::get_warm_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_gen::get_hot_startup_time()
{
	throw(C_csp_exception("C_csp_gen_pc::get_hot_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_gen::get_standby_energy_requirement()
{
	throw(C_csp_exception("C_csp_gen_pc::get_standby_energy_requirement() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWt]
}
double C_pc_gen::get_cold_startup_energy()
{
	throw(C_csp_exception("C_csp_gen_pc::get_cold_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_gen::get_warm_startup_energy()
{
	throw(C_csp_exception("C_csp_gen_pc::get_warm_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_gen::get_hot_startup_energy()
{
	throw(C_csp_exception("C_csp_gen_pc::get_hot_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_gen::get_max_thermal_power()
{
	throw(C_csp_exception("C_csp_gen_pc::get_max_thermal_power() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MW]
} 
double C_pc_gen::get_min_thermal_power()
{
	throw(C_csp_exception("C_csp_gen_pc::get_min_thermal_power() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MW]
}
void C_pc_gen::get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max)
{
	throw(C_csp_exception("C_csp_gen_pc::get_max_power_output_operation_constraints() is not complete"));

	return;	//[-]
}
double C_pc_gen::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser)
{
	throw(C_csp_exception("C_csp_gen_pc::get_efficiency_at_TPH() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_gen::get_efficiency_at_load(double load_frac, double *w_dot_condenser)
{
	throw(C_csp_exception("C_csp_gen_pc::get_efficiency_at_load() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

// This can vary between timesteps for Type224, depending on remaining startup energy and time
double C_pc_gen::get_max_q_pc_startup()
{
	throw(C_csp_exception("C_csp_gen_pc::get_max_q_pc_startup() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWt]
}

double C_pc_gen::get_htf_pumping_parasitic_coef()
{
	throw(C_csp_exception("C_pc_gen::get_htf_pumping_parasitic_coef() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWt]	// kWe/kWt
}


void C_pc_gen::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs &inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	//C_csp_power_cycle::S_csp_pc_out_report &out_report,
	const C_csp_solver_sim_info &sim_info)
{
	double twb = weather.m_twet+273.15;		//[K] Wet-bulb temperature, convert from C
	double tdb = weather.m_tdry+273.15;		//[K] Dry-bulb temperature, convert from C

	// But, need to put this in context of csp-solver (m_dot, T_hot, cp, T_cold_fixed...)
	double T_hot = htf_state_in.m_temp+273.15;		//[K] hot inlet temp
	double m_dot = inputs.m_m_dot/3600.0;			//[kg/s] mass flow rate
	double q_to_pb = m_dot*m_cp_htf_fixed*(T_hot - m_T_htf_cold_fixed)*1.E-3;	//[MWt]
	// ***********************************************************************************

	double qnorm = q_to_pb / m_q_des;		//[-] The normalized thermal energy flow
	double tnorm = std::numeric_limits<double>::quiet_NaN();
	if( ms_params.m_PC_T_corr == 1 )		//[-] Select the dry or wet bulb temperature as the driving difference
		tnorm = twb - ms_params.m_T_pc_des;
	else
		tnorm = tdb - ms_params.m_T_pc_des;

	// Calculate the load-based and temperature-based efficiency correction factors
	double f_effpc_qtpb = 0.0;
	double f_effpc_tamb = 0.0;
	for( size_t i = 0; i < ms_params.mv_etaQ_coefs.size(); i++ )
		f_effpc_qtpb += ms_params.mv_etaQ_coefs[i] * pow(qnorm, i);
	for( size_t i = 0; i < ms_params.mv_etaT_coefs.size(); i++ )
		f_effpc_tamb += ms_params.mv_etaT_coefs[i] * pow(tnorm, i);
	
	double eta_cycle = ms_params.m_eta_des * (f_effpc_qtpb + f_effpc_tamb);  //[-] Adjusted power conversion efficiency

	if( q_to_pb <= 0. )
	{ 
		eta_cycle = 0.0;	//[-] Set conversion efficiency to zero when the power block isn't operating
	}

	// Calculate the gross power
	double w_gr = q_to_pb * eta_cycle;		//[MWe]

	// Set output structures
	out_solver.m_time_required_su = 0.0;			//[s]
	out_solver.m_P_cycle = w_gr;					//[MWe]
	out_solver.m_T_htf_cold = m_T_htf_cold_fixed;	//[K]
	out_solver.m_q_dot_htf = q_to_pb;				//[MWt]
	out_solver.m_m_dot_htf = m_dot*3600.0;			//[kg/hr]
	
	out_solver.m_W_dot_htf_pump = 0.0;		//[MWe]
	out_solver.m_W_cool_par = 0.0;			//[MWe]

	mc_reported_outputs.value(E_ETA_THERMAL, eta_cycle);	//[-]

	//out_report.m_eta = eta_cycle;		//[-]
	//out_report.m_m_dot_makeup = 0.0;	//[kg/hr]
	//out_report.m_m_dot_demand = 0.0;	//[kg/hr]
	//out_report.m_m_dot_htf_ref = 0.0;	//[kg/hr]
	//out_report.m_P_ref = 0.0;			//[MWe]
	//out_report.m_f_hrsys = 0.0;			//[-]
	//out_report.m_P_cond = 0.0;			//[-]
	//out_report.m_q_startup = 0.0;		//[MWt-hr]

}

void C_pc_gen::converged()
{
	mc_reported_outputs.set_timestep_outputs();

	throw(C_csp_exception("C_csp_gen_pc::converged() is not complete"));
}

void C_pc_gen::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_pc_gen::assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}
