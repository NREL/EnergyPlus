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

#include "csp_solver_pc_steam_heat_sink.h"
#include "csp_solver_core.h"

#include "lib_util.h"

#include "water_properties.h"

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_pc_steam_heat_sink::E_Q_DOT_HEAT_SINK, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_steam_heat_sink::E_W_DOT_PUMPING, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	csp_info_invalid
};

C_pc_steam_heat_sink::C_pc_steam_heat_sink()
{
	mc_reported_outputs.construct(S_output_info);

	m_max_frac = 100.0;

	m_is_sensible_htf = false;	//[-] STEAM
}

void C_pc_steam_heat_sink::check_double_params_are_set()
{
	if( !check_double(ms_params.m_x_hot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_x_hot_des"));
	}
	if( !check_double(ms_params.m_T_hot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_T_hot_des"));
	}
	if( !check_double(ms_params.m_P_hot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_P_hot_des"));
	}
	if( !check_double(ms_params.m_T_cold_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_T_cold_des"));
	}
	if( !check_double(ms_params.m_dP_frac_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_dP_frac_des"));
	}
	if( !check_double(ms_params.m_q_dot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_q_dot_des"));
	}
	if( !check_double(ms_params.m_m_dot_max_frac) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_m_dot_max_frac"));
	}
	if( !check_double(ms_params.m_pump_eta_isen) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method: ", "m_pump_eta_isen"));
	}
}

void C_pc_steam_heat_sink::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	check_double_params_are_set();

	// Get enthalpy at design inlet and outlet
		// Hot inlet
	int prop_error_code = -1;
	if( ms_params.m_x_hot_des < 0.0 || ms_params.m_x_hot_des > 1.0 )
	{
		prop_error_code = water_TP(ms_params.m_T_hot_des + 273.15, ms_params.m_P_hot_des, &mc_water_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_pc_steam_heat_sink::init(...) Design hot state point property calcs failed"));
		}
	}
	else
	{
		prop_error_code = water_PQ(ms_params.m_P_hot_des, ms_params.m_x_hot_des, &mc_water_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_pc_steam_heat_sink::init(...) Design hot state point property calcs failed"));
		}
	}
	double h_hot = mc_water_props.enth;		//[kJ/kg]
		// Cold outlet
	double P_cold_des = (1.0 - ms_params.m_dP_frac_des)*ms_params.m_P_hot_des;	//[kPa]
	prop_error_code = water_TP(ms_params.m_T_cold_des+273.15, P_cold_des, &mc_water_props);
	if( prop_error_code != 0 )
	{
		throw(C_csp_exception("C_pc_steam_heat_sink::init(...) Design cold state point property calcs failed"));
	}
	double h_cold = mc_water_props.enth;	//[kJ/kg]

	double m_dot_steam_des = ms_params.m_q_dot_des*1.E3 / (h_hot - h_cold);		//[kg/s]

	// Set 'solved_params' structure
	solved_params.m_W_dot_des = 0.0;		//[MWe] Assuming heat sink is not generating electricity FOR THIS MODEL
	solved_params.m_eta_des = 0.0;			//[-] Same
	solved_params.m_q_dot_des = ms_params.m_q_dot_des;	//[MWt]
	solved_params.m_q_startup = 0.0;		//[MWt-hr] Assuming heat sink does not require any startup energy
	
	// This *should be* really large so heat sink can handle any collector - receiver output
		// But want to set at Compute Module level so it's consistent with value going to CR model
	solved_params.m_max_frac = ms_params.m_m_dot_max_frac;	//[-] 
	
	solved_params.m_cutoff_frac = 0.0;		//[-] Similarly, don't put a floor on the thermal power input
	solved_params.m_sb_frac = 0.0;			//[-] So, don't need standby
	solved_params.m_T_htf_hot_ref = ms_params.m_T_hot_des;	//[C]
	solved_params.m_m_dot_design = m_dot_steam_des*3600.0;	//[kg/hr]
	solved_params.m_m_dot_min = solved_params.m_m_dot_design*solved_params.m_cutoff_frac;	//[kg/hr]
	solved_params.m_m_dot_max = solved_params.m_m_dot_design*solved_params.m_max_frac;		//[kg/hr]

	solved_params.m_P_hot_des = ms_params.m_P_hot_des;		//[kPa]
	solved_params.m_x_hot_des = ms_params.m_x_hot_des;		//[-]

}

int C_pc_steam_heat_sink::get_operating_state()
{
	// Assume heat sink is always able to accept thermal power from solar field/TES
	return C_csp_power_cycle::ON;
}

double C_pc_steam_heat_sink::get_cold_startup_time()
{
	return 0.0;
}

double C_pc_steam_heat_sink::get_warm_startup_time()
{
	return 0.0;
}

double C_pc_steam_heat_sink::get_hot_startup_time()
{
	return 0.0;
}

double C_pc_steam_heat_sink::get_standby_energy_requirement()
{
	return 0.0;	//[MWt]
}

double C_pc_steam_heat_sink::get_cold_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_steam_heat_sink::get_warm_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_steam_heat_sink::get_hot_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_steam_heat_sink::get_max_thermal_power()
{
	return m_max_frac * ms_params.m_q_dot_des;	//[MWt]
}

double C_pc_steam_heat_sink::get_min_thermal_power()
{
	return 0.0;		//[MWt]
}

double C_pc_steam_heat_sink::get_htf_pumping_parasitic_coef()
{
	return 0.0;	// kWe/kWt
}

void C_pc_steam_heat_sink::get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max)
{
	m_dot_HTF_ND_max = m_max_frac;		//[-]
	W_dot_ND_max = m_dot_HTF_ND_max;	//[-]
	
	return;	//[-]
}

double C_pc_steam_heat_sink::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser)
{
	throw(C_csp_exception("C_pc_steam_heat_sink::get_efficiency_at_TPH() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_pc_steam_heat_sink::get_efficiency_at_load(double load_frac, double *w_dot_condenser)
{
	throw(C_csp_exception("C_pc_steam_heat_sink::get_efficiency_at_load() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_pc_steam_heat_sink::get_max_q_pc_startup()
{
	return 0.0;		//[MWt]
}

void C_pc_steam_heat_sink::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs &inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	//C_csp_power_cycle::S_csp_pc_out_report &out_report,
	const C_csp_solver_sim_info &sim_info)
{
	double T_steam_hot = htf_state_in.m_temp + 273.15;	//[K], convert from C
	double P_steam_hot = htf_state_in.m_pres;	//[kPa]
	double x_steam_hot = htf_state_in.m_qual;	//[-]
	double m_dot_steam = inputs.m_m_dot/3600.0;	//[kg/s], convert from kg/hr

	// Hot state point
	int prop_error_code = -1;
	if( x_steam_hot < 0.0 || x_steam_hot > 1.0 )
	{
		prop_error_code = water_TP(T_steam_hot, P_steam_hot, &mc_water_props);
		if( prop_error_code != 0 )
		{
			std::string msg = util::format("Hot inlet water/steam properties failed at T = %lg [K] and P = %lg [kPa]", T_steam_hot, P_steam_hot);
			throw(C_csp_exception("C_pc_steam_heat_sink::call(...)", msg));
		}
	}
	else
	{
		prop_error_code = water_PQ(P_steam_hot, x_steam_hot, &mc_water_props);
		if( prop_error_code != 0 )
		{
			std::string msg = util::format("Hot inlet water/steam properties failed at P = %lg [K] and x = %lg [-]", P_steam_hot, x_steam_hot);
			throw(C_csp_exception("C_pc_steam_heat_sink::call(...)", msg));
		}
	}
	double h_steam_hot = mc_water_props.enth;	//[kJ/kg]

	// For now, let's assume the Heat Sink can always return the HTF at the design cold temperature
		// Cold outlet state point
	double P_steam_cold = (1.0 - ms_params.m_dP_frac_des)*ms_params.m_P_hot_des;	//[kPa]
	double T_steam_cold = ms_params.m_T_cold_des + 273.15;							//[K]
	prop_error_code = water_TP(T_steam_cold, P_steam_cold, &mc_water_props);
	if( prop_error_code != 0 )
	{
		throw(C_csp_exception("C_pc_steam_heat_sink::call(...) Cold outlet water/steam property calcs failed"));
	}
	double h_steam_cold = mc_water_props.enth;	//[kJ/kg]
	double s_steam_cold = mc_water_props.entr;	//[kJ/kg-K]

	double q_dot_steam = m_dot_steam*(h_steam_hot - h_steam_cold)/1.E3;		//[MWt]

	// Calculate isentropic compression back to Hot Inlet Pressure
	prop_error_code = water_PS(P_steam_hot, s_steam_cold, &mc_water_props);
	if( prop_error_code != 0 )
	{
		throw(C_csp_exception("C_pc_steam_heat_sink::call(...) Isentropic compression calcs failed"));
	}
	double h_steam_cold_comp_isen = mc_water_props.enth;

	// eta_isen = (h_out_isen - h_in) / (h_out - h_in)
	double h_steam_cold_comp = (h_steam_cold_comp_isen - h_steam_cold)/ms_params.m_pump_eta_isen + h_steam_cold;	//[kJ/kg]

	out_solver.m_P_cycle = 0.0;							//[MWe] No electricity generation
	out_solver.m_T_htf_cold = T_steam_cold - 273.15;	//[C] convert from K
	out_solver.m_m_dot_htf = m_dot_steam*3600.0;		//[kg/hr] Return inlet mass flow rate
	out_solver.m_W_cool_par = 0.0;			//[MWe] No cooling load
	
	out_solver.m_time_required_su = 0.0;		//[s] No startup requirements, for now
	out_solver.m_q_dot_htf = q_dot_steam;	//[MWt] Thermal power form HTF
	out_solver.m_W_dot_htf_pump = m_dot_steam*(h_steam_cold_comp - h_steam_cold)/1.E3;	//[MWe]
	
	out_solver.m_was_method_successful = true;

	mc_reported_outputs.value(E_Q_DOT_HEAT_SINK, q_dot_steam);					//[MWt]
	mc_reported_outputs.value(E_W_DOT_PUMPING, out_solver.m_W_dot_htf_pump);	//[MWe]

	return;
}

void C_pc_steam_heat_sink::converged()
{
	// Nothing, so far, in model is time dependent

	// But need to set final timestep outputs to reported outputs class
	mc_reported_outputs.set_timestep_outputs();

	return;
}

void C_pc_steam_heat_sink::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_pc_steam_heat_sink::assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}