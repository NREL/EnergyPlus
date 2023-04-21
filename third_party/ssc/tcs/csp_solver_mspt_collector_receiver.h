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

#ifndef __csp_solver_mspt_collector_receiver_
#define __csp_solver_mspt_collector_receiver_

#include "csp_solver_core.h"
#include "csp_solver_pt_sf_perf_interp.h"
#include "csp_solver_pt_receiver.h"



class C_csp_mspt_collector_receiver : public C_csp_collector_receiver
{

private:
	C_pt_sf_perf_interp &mc_pt_heliostatfield;
	C_pt_receiver &mc_pt_receiver;

public:
	
	enum
	{
		E_FIELD_Q_DOT_INC,		//[MWt] Field incident thermal power
		E_FIELD_ETA_OPT,		//[-] Optical efficiency including receiver refl
		E_FIELD_ADJUST,			//[-] Field adjustment factor
		
		E_Q_DOT_INC,			//[MWt] Receiver incident thermal power
		E_ETA_THERMAL,			//[-] Receiver thermal efficiency
		E_Q_DOT_THERMAL,		//[MWt] Field incident thermal power
		E_M_DOT_HTF,			//[kg/hr] Receiver mass flow rate
		E_Q_DOT_STARTUP,		//[MWt] Receiver startup thermal power consumed
		E_T_HTF_IN,				//[C] Receiver HTF inlet temperature
		E_T_HTF_OUT,			//[C] Receiver HTF outlet temperature
		E_Q_DOT_PIPE_LOSS,		//[MWt] Tower piping losses
        E_Q_DOT_LOSS,           //[MWt] Receiver convection and radiation losses
		E_P_HEATTRACE,			//[MWe] Receiver heat trace parasitic
		E_T_HTF_OUT_END,		//[C] Instantaneous receiver HTF outlet temperature at the end of the time step
		E_T_HTF_OUT_MAX,		//[C] Receiver maximum HTF outlet temperature at any point during time step
		E_T_HTF_PANEL_OUT_MAX,	//[C] Receiver panel maximum HTF outlet temperature at any point during time step
		E_T_WALL_INLET,			//[C] Receiver inlet wall temperature at end of time step
		E_T_WALL_OUTLET,		//[C] Receiver inlet wall temperature at end of time step
		E_T_RISER,				//[C] Riser temperature at the end of the time step
		E_T_DOWNC,				//[C] Downcomer temperature at the end of the time step
		E_CLEARSKY,				//[W/m2] Clear-sky DNI 
		E_Q_DOT_THERMAL_CSKY_SS, //[MWt] Thermal power from receiver under steady-state clear-sky conditions
		E_Q_DOT_THERMAL_SS		//[MWt] Thermal power from receiver under steady-state conditions
	};
	
	C_csp_reported_outputs mc_reported_outputs;
	
	C_csp_mspt_collector_receiver(C_pt_sf_perf_interp & pt_heliostatfield,
		C_pt_receiver & pt_receiver);

	~C_csp_mspt_collector_receiver();

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
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

    virtual double calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim );
  
    virtual double calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/ );

    virtual double get_collector_area();


	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

};








#endif //__csp_solver_mspt_collector_receiver_