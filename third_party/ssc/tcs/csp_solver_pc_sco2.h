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

#ifndef __csp_solver_pc_sco2_
#define __csp_solver_pc_sco2_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

#include "sco2_pc_csp_int.h"

class C_pc_sco2 : public C_csp_power_cycle
{

private:

	C_sco2_phx_air_cooler mc_sco2_recomp;

	HTFProperties mc_pc_htfProps;

	double m_q_dot_design;					//[MWt]
	double m_q_dot_standby;					//[MWt]
	double m_q_dot_max;						//[MWt]
	double m_q_dot_min;						//[MWt]
	double m_startup_energy_required;		//[kW-hr]

	double m_W_dot_des;						//[MWe]
	double m_T_htf_cold_des;				//[K]
	double m_m_dot_htf_des;					//[kg/hr]

	int m_standby_control_prev;				//[-]
	double m_startup_time_remain_prev;		//[hr]
	double m_startup_energy_remain_prev;	//[kW-hr]

	int m_standby_control_calc;				//[-]
	double m_startup_time_remain_calc;		//[hr]
	double m_startup_energy_remain_calc;	//[kW-hr]

public:

	enum
	{
		E_ETA_THERMAL,		//[-] Cycle thermal efficiency (gross)
		E_Q_DOT_HTF,		//[MWt] Cycle thermal power input
		E_M_DOT_HTF,		//[kg/hr] Cycle HTF mass flow rate
		E_Q_DOT_STARTUP,	//[MWt] Cycle startup thermal power
		E_W_DOT,			//[MWe] Cycle electricity output (gross)
		E_T_HTF_IN,			//[C] Cycle HTF inlet temperature
		E_T_HTF_OUT,		//[C] Cycle HTF outlet temperature
		E_M_DOT_WATER,		//[kg/hr] Cycle water consumption: makeup + cooling	
	};
	
	C_csp_reported_outputs mc_reported_outputs;

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_des_par
	{
		C_sco2_phx_air_cooler::S_des_par ms_mc_sco2_recomp_params;

		double m_cycle_max_frac;	//[-] Maximum turbine over-design operation fraction
		double m_cycle_cutoff_frac;	//[-] Minimum turbine operation fraction
		double m_q_sby_frac;		//[-] fraction of thermal power required for standby mode
		double m_startup_time;		//[hr] time needed for power block startup
		double m_startup_frac;		//[-] fraction of design thermal power needed for startup
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

		S_des_par()
		{
			m_cycle_max_frac = m_cycle_cutoff_frac = m_q_sby_frac = m_startup_time = m_startup_frac = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_des_par ms_params;

	C_pc_sco2();

	~C_pc_sco2(){};

	virtual void init(C_csp_power_cycle::S_solved_params &solved_params);

	virtual int get_operating_state();

	virtual double get_cold_startup_time();
	virtual double get_warm_startup_time();
	virtual double get_hot_startup_time();
	virtual double get_standby_energy_requirement();    //[MW]
	virtual double get_cold_startup_energy();    //[MWh]
	virtual double get_warm_startup_energy();    //[MWh]
	virtual double get_hot_startup_energy();    //[MWh]
	virtual double get_max_thermal_power();     //MW
	virtual double get_min_thermal_power();     //MW
	virtual void get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max);	//[-] Normalized over design power
	virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser=0);
	virtual double get_efficiency_at_load(double load_frac, double *w_dot_condenser=0);
	virtual double get_htf_pumping_parasitic_coef();		//[kWe/kWt]

	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup();		//[MWt]

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_1state &htf_state_in,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
		//C_csp_power_cycle::S_csp_pc_out_report &out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual void assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array);
};


#endif	//__csp_solver_pc_sco2_