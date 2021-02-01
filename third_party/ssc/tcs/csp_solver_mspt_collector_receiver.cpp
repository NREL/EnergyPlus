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

#include "csp_solver_mspt_collector_receiver.h"
#include "sam_csp_util.h"
#include <algorithm>

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_csp_mspt_collector_receiver::E_FIELD_Q_DOT_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_FIELD_ETA_OPT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_FIELD_ADJUST, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	{C_csp_mspt_collector_receiver::E_Q_DOT_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_Q_DOT_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_M_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_Q_DOT_STARTUP, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_T_HTF_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_T_HTF_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_Q_DOT_PIPE_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_mspt_collector_receiver::E_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    // from transient model:
	{ C_csp_mspt_collector_receiver::E_P_HEATTRACE, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	{ C_csp_mspt_collector_receiver::E_T_HTF_OUT_END, C_csp_reported_outputs::TS_LAST },
	{ C_csp_mspt_collector_receiver::E_T_HTF_OUT_MAX, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	{ C_csp_mspt_collector_receiver::E_T_HTF_PANEL_OUT_MAX, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	{ C_csp_mspt_collector_receiver::E_T_WALL_INLET, C_csp_reported_outputs::TS_LAST },
	{ C_csp_mspt_collector_receiver::E_T_WALL_OUTLET, C_csp_reported_outputs::TS_LAST },
	{ C_csp_mspt_collector_receiver::E_T_RISER, C_csp_reported_outputs::TS_LAST },
	{ C_csp_mspt_collector_receiver::E_T_DOWNC, C_csp_reported_outputs::TS_LAST },

	{ C_csp_mspt_collector_receiver::E_CLEARSKY, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	{ C_csp_mspt_collector_receiver::E_Q_DOT_THERMAL_CSKY_SS, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	{ C_csp_mspt_collector_receiver::E_Q_DOT_THERMAL_SS, C_csp_reported_outputs::TS_WEIGHTED_AVE },
	csp_info_invalid	
};

C_csp_mspt_collector_receiver::C_csp_mspt_collector_receiver(C_pt_sf_perf_interp & pt_heliostatfield,
	C_pt_receiver & pt_receiver):
	mc_pt_heliostatfield(pt_heliostatfield),
	mc_pt_receiver(pt_receiver)
{
	mc_reported_outputs.construct(S_output_info);
}

C_csp_mspt_collector_receiver::~C_csp_mspt_collector_receiver()
{}

void C_csp_mspt_collector_receiver::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
				C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	mc_pt_heliostatfield.init();
	mc_pt_receiver.init();

	solved_params.m_T_htf_cold_des = mc_pt_receiver.m_T_htf_cold_des;       //[K]
	solved_params.m_q_dot_rec_des = mc_pt_receiver.m_q_rec_des / 1.E6;		//[MW]
	solved_params.m_A_aper_total = mc_pt_heliostatfield.ms_params.m_A_sf;	//[m^2]

	return;
}

int C_csp_mspt_collector_receiver::get_operating_state()
{
	return mc_pt_receiver.get_operating_state();
}

double C_csp_mspt_collector_receiver::get_startup_time()
{
    return mc_pt_receiver.get_startup_time();   //[s]
}

double C_csp_mspt_collector_receiver::get_startup_energy()
{
    return mc_pt_receiver.get_startup_energy(); //[MWh]
}

double C_csp_mspt_collector_receiver::get_pumping_parasitic_coef()  //MWe/MWt
{
    return mc_pt_receiver.get_pumping_parasitic_coef();
}

double C_csp_mspt_collector_receiver::get_min_power_delivery()    //MWt
{
    return mc_pt_receiver.m_f_rec_min * mc_pt_receiver.m_q_rec_des*1.e-6;
}


double C_csp_mspt_collector_receiver::get_tracking_power()
{
	return mc_pt_heliostatfield.ms_params.m_p_track * mc_pt_heliostatfield.ms_params.m_N_hel*1.e-3;	//MWe
}

double C_csp_mspt_collector_receiver::get_col_startup_power()
{
	return mc_pt_heliostatfield.ms_params.m_p_start * mc_pt_heliostatfield.ms_params.m_N_hel *1.e-3;	//MWe-hr
}


void C_csp_mspt_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	// What about catching errors here?
	
	// First call heliostat field class: 'csp_solver_pt_heliostat'
	// Then use its outputs as inputs to receiver class: 'csp_solver_mspt_receiver_222'

	// Set heliostat field call() parameters and solve
	double heliostat_field_control = inputs.m_field_control;
	mc_pt_heliostatfield.call(weather, heliostat_field_control, sim_info);

	// Get heliostat field outputs and set corresponding receiver inputs
	C_pt_receiver::S_inputs receiver_inputs;
	receiver_inputs.m_field_eff = mc_pt_heliostatfield.ms_outputs.m_eta_field;
	receiver_inputs.m_input_operation_mode = inputs.m_input_operation_mode;
	receiver_inputs.m_flux_map_input = &mc_pt_heliostatfield.ms_outputs.m_flux_map_out;
	mc_pt_receiver.call(weather, htf_state_in, receiver_inputs, sim_info);
		
	// Set collector/receiver parent class outputs and return
	//cr_out_report.m_eta_field = mc_pt_heliostatfield.ms_outputs.m_eta_field;				//[-]
    //cr_out_report.m_sf_adjust_out = mc_pt_heliostatfield.ms_outputs.m_sf_adjust_out;
	//cr_out_report.m_q_dot_field_inc = mc_pt_heliostatfield.ms_outputs.m_q_dot_field_inc;	//[MWt]

	//cr_out_report.m_q_dot_rec_inc = mc_pt_receiver.ms_outputs.m_q_dot_rec_inc;		//[MWt]
	//cr_out_report.m_eta_thermal = mc_pt_receiver.ms_outputs.m_eta_therm;				//[-]
	cr_out_solver.m_q_thermal = mc_pt_receiver.ms_outputs.m_Q_thermal;				//[MW]
	cr_out_solver.m_q_startup = mc_pt_receiver.ms_outputs.m_q_startup;				//[MWt-hr]
	//cr_out_report.m_q_dot_piping_loss = mc_pt_receiver.ms_outputs.m_q_dot_piping_loss;	//[MWt]
	cr_out_solver.m_m_dot_salt_tot = mc_pt_receiver.ms_outputs.m_m_dot_salt_tot;		//[kg/hr]
	cr_out_solver.m_T_salt_hot = mc_pt_receiver.ms_outputs.m_T_salt_hot;				//[C]
	
	cr_out_solver.m_component_defocus = mc_pt_receiver.ms_outputs.m_component_defocus;	//[-]
	
	cr_out_solver.m_W_dot_htf_pump = mc_pt_receiver.ms_outputs.m_W_dot_pump;			//[MWe]
	cr_out_solver.m_W_dot_col_tracking = mc_pt_heliostatfield.ms_outputs.m_pparasi;		//[MWe]

	cr_out_solver.m_time_required_su = mc_pt_receiver.ms_outputs.m_time_required_su;	//[s]
	cr_out_solver.m_q_rec_heattrace = mc_pt_receiver.ms_outputs.m_q_heattrace / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0);		//[MWt])


	mc_reported_outputs.value(E_FIELD_Q_DOT_INC, mc_pt_heliostatfield.ms_outputs.m_q_dot_field_inc);	//[MWt]
	mc_reported_outputs.value(E_FIELD_ETA_OPT, mc_pt_heliostatfield.ms_outputs.m_eta_field);			//[-]
	mc_reported_outputs.value(E_FIELD_ADJUST, mc_pt_heliostatfield.ms_outputs.m_sf_adjust_out);			//[-]

	mc_reported_outputs.value(E_Q_DOT_INC, mc_pt_receiver.ms_outputs.m_q_dot_rec_inc);	//[MWt]
	mc_reported_outputs.value(E_ETA_THERMAL, mc_pt_receiver.ms_outputs.m_eta_therm);		//[-]
	mc_reported_outputs.value(E_Q_DOT_THERMAL, mc_pt_receiver.ms_outputs.m_Q_thermal);	//[MWt]
	mc_reported_outputs.value(E_M_DOT_HTF, mc_pt_receiver.ms_outputs.m_m_dot_salt_tot);	//[kg/hr]
		// If startup, then timestep may have changed (why not report this from 222 in MWt?)
	mc_reported_outputs.value(E_Q_DOT_STARTUP, mc_pt_receiver.ms_outputs.m_q_startup / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0));		//[MWt])
	mc_reported_outputs.value(E_T_HTF_IN, htf_state_in.m_temp);									//[C]
	mc_reported_outputs.value(E_T_HTF_OUT, mc_pt_receiver.ms_outputs.m_T_salt_hot);		//[C]
	mc_reported_outputs.value(E_Q_DOT_PIPE_LOSS, mc_pt_receiver.ms_outputs.m_q_dot_piping_loss);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_LOSS, mc_pt_receiver.ms_outputs.m_q_rad_sum + mc_pt_receiver.ms_outputs.m_q_conv_sum ); //MWt
    // from transient model:
	mc_reported_outputs.value(E_P_HEATTRACE, mc_pt_receiver.ms_outputs.m_q_heattrace / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0));		//[MWt])
	mc_reported_outputs.value(E_T_HTF_OUT_END, mc_pt_receiver.ms_outputs.m_inst_T_salt_hot);	//[C]
	mc_reported_outputs.value(E_T_HTF_OUT_MAX, mc_pt_receiver.ms_outputs.m_max_T_salt_hot);	//[C]
	mc_reported_outputs.value(E_T_HTF_PANEL_OUT_MAX, mc_pt_receiver.ms_outputs.m_max_rec_tout);	//[C]
	
	mc_reported_outputs.value(E_T_WALL_INLET, mc_pt_receiver.ms_outputs.m_Twall_inlet);	//[C]
	mc_reported_outputs.value(E_T_WALL_OUTLET, mc_pt_receiver.ms_outputs.m_Twall_outlet);	//[C]
	mc_reported_outputs.value(E_T_RISER, mc_pt_receiver.ms_outputs.m_Triser);	//[C]
	mc_reported_outputs.value(E_T_DOWNC, mc_pt_receiver.ms_outputs.m_Tdownc);	//[C]

	mc_reported_outputs.value(E_CLEARSKY, mc_pt_receiver.ms_outputs.m_clearsky);
	mc_reported_outputs.value(E_Q_DOT_THERMAL_CSKY_SS, mc_pt_receiver.ms_outputs.m_Q_thermal_csky_ss); //[MWt]
	mc_reported_outputs.value(E_Q_DOT_THERMAL_SS, mc_pt_receiver.ms_outputs.m_Q_thermal_ss); //[MWt]
}

void C_csp_mspt_collector_receiver::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	// First call heliostat field class
	// In OFF call, looking specifically for weather STOW parasitics apply
	mc_pt_heliostatfield.off(sim_info);

	// Set collector/receiver parent class outputs from field model
	//cr_out_report.m_eta_field = mc_pt_heliostatfield.ms_outputs.m_eta_field;				//[-]
    //cr_out_report.m_sf_adjust_out = mc_pt_heliostatfield.ms_outputs.m_sf_adjust_out;
	//cr_out_report.m_q_dot_field_inc = mc_pt_heliostatfield.ms_outputs.m_q_dot_field_inc;	//[MWt]
	cr_out_solver.m_W_dot_col_tracking = mc_pt_heliostatfield.ms_outputs.m_pparasi;			//[MWe]

	// Now, call the tower-receiver model
	mc_pt_receiver.off(weather, htf_state_in, sim_info);

	// Set collector/receiver parent class outputs from field model
	//cr_out_report.m_q_dot_rec_inc = mc_pt_receiver.ms_outputs.m_q_dot_rec_inc;		 //[MWt]
	//cr_out_report.m_eta_thermal = mc_pt_receiver.ms_outputs.m_eta_therm;				 //[-]
	cr_out_solver.m_q_thermal = mc_pt_receiver.ms_outputs.m_Q_thermal;				 //[MW]
	cr_out_solver.m_q_startup = mc_pt_receiver.ms_outputs.m_q_startup;				 //[MWt-hr]
	//cr_out_report.m_q_dot_piping_loss = mc_pt_receiver.ms_outputs.m_q_dot_piping_loss; //[MWt]
	cr_out_solver.m_m_dot_salt_tot = mc_pt_receiver.ms_outputs.m_m_dot_salt_tot;		 //[kg/hr]
	cr_out_solver.m_T_salt_hot = mc_pt_receiver.ms_outputs.m_T_salt_hot;				 //[C]
	cr_out_solver.m_component_defocus = 1.0;	//[-]
	cr_out_solver.m_W_dot_htf_pump = mc_pt_receiver.ms_outputs.m_W_dot_pump;			 //[MWe]
		// Not sure that we want 'startup time required' calculated in 'off' call
	cr_out_solver.m_time_required_su = mc_pt_receiver.ms_outputs.m_time_required_su;	 //[s]
	cr_out_solver.m_q_rec_heattrace = mc_pt_receiver.ms_outputs.m_q_heattrace / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0);		//[MWt])

	mc_reported_outputs.value(E_FIELD_Q_DOT_INC, mc_pt_heliostatfield.ms_outputs.m_q_dot_field_inc);	//[MWt]
	mc_reported_outputs.value(E_FIELD_ETA_OPT, mc_pt_heliostatfield.ms_outputs.m_eta_field);			//[-]
	mc_reported_outputs.value(E_FIELD_ADJUST, mc_pt_heliostatfield.ms_outputs.m_sf_adjust_out);			//[-]

	mc_reported_outputs.value(E_Q_DOT_INC, mc_pt_receiver.ms_outputs.m_q_dot_rec_inc);	//[MWt]
	mc_reported_outputs.value(E_ETA_THERMAL, mc_pt_receiver.ms_outputs.m_eta_therm);		//[-]
	mc_reported_outputs.value(E_Q_DOT_THERMAL, mc_pt_receiver.ms_outputs.m_Q_thermal);	//[MWt]
	mc_reported_outputs.value(E_M_DOT_HTF, mc_pt_receiver.ms_outputs.m_m_dot_salt_tot);	//[kg/hr]
		// Should not be startup energy in OFF, but timestep may be subhourly/nonuniform (why not report this from 222 in MWt?)
	mc_reported_outputs.value(E_Q_DOT_STARTUP, mc_pt_receiver.ms_outputs.m_q_startup / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0));		//[MWt])
	mc_reported_outputs.value(E_T_HTF_IN, htf_state_in.m_temp);									//[C]
	mc_reported_outputs.value(E_T_HTF_OUT, mc_pt_receiver.ms_outputs.m_T_salt_hot);		//[C]
	mc_reported_outputs.value(E_Q_DOT_PIPE_LOSS, mc_pt_receiver.ms_outputs.m_q_dot_piping_loss);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_LOSS, mc_pt_receiver.ms_outputs.m_q_rad_sum + mc_pt_receiver.ms_outputs.m_q_conv_sum ); //MWt
    // from transient model:
	mc_reported_outputs.value(E_P_HEATTRACE, mc_pt_receiver.ms_outputs.m_q_heattrace / (mc_pt_receiver.ms_outputs.m_time_required_su / 3600.0));		//[MWt])
	mc_reported_outputs.value(E_T_HTF_OUT_END, mc_pt_receiver.ms_outputs.m_inst_T_salt_hot);	//[C]
	mc_reported_outputs.value(E_T_HTF_OUT_MAX, mc_pt_receiver.ms_outputs.m_max_T_salt_hot);	//[C]
	mc_reported_outputs.value(E_T_HTF_PANEL_OUT_MAX, mc_pt_receiver.ms_outputs.m_max_rec_tout);	//[C]

	mc_reported_outputs.value(E_T_WALL_INLET, mc_pt_receiver.ms_outputs.m_Twall_inlet);	//[C]
	mc_reported_outputs.value(E_T_WALL_OUTLET, mc_pt_receiver.ms_outputs.m_Twall_outlet);	//[C]
	mc_reported_outputs.value(E_T_RISER, mc_pt_receiver.ms_outputs.m_Triser);	//[C]
	mc_reported_outputs.value(E_T_DOWNC, mc_pt_receiver.ms_outputs.m_Tdownc);	//[C]

	mc_reported_outputs.value(E_CLEARSKY, mc_pt_receiver.ms_outputs.m_clearsky);
	mc_reported_outputs.value(E_Q_DOT_THERMAL_CSKY_SS, mc_pt_receiver.ms_outputs.m_Q_thermal_csky_ss); //[MWt]
	mc_reported_outputs.value(E_Q_DOT_THERMAL_SS, mc_pt_receiver.ms_outputs.m_Q_thermal_ss); //[MWt]

	return;
}

void C_csp_mspt_collector_receiver::startup(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define startup(...) shell that calls call() with operation mode defined.
	// Should eventually develop a startup method for the collector receiver

	// Set heliostat field call() parameters and solve
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::STARTUP;
	inputs.m_field_control = 1.0;

	call(weather, htf_state_in, inputs, cr_out_solver, sim_info);
}

void C_csp_mspt_collector_receiver::on(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	double field_control,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define on(...) shell that calls call() with operation mode defined.
	// Should eventually develop an 'on' method for the MSPT

	// Define 'C_csp_cr_inputs' for call(...)
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::ON;
	inputs.m_field_control = field_control;

	call(weather, htf_state_in, inputs, cr_out_solver, sim_info);
}

void C_csp_mspt_collector_receiver::estimates(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_est_out &est_out,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define estimates(...) shell that calls call() with operation mode defined.
	// Should eventually develop an estimate(...) method for the MSPT
	
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::STEADY_STATE;
	inputs.m_field_control = 1.0;

	C_csp_collector_receiver::S_csp_cr_out_solver cr_out_solver;

	call(weather, htf_state_in, inputs, cr_out_solver, sim_info);

	int mode = get_operating_state();

	if( mode == C_csp_collector_receiver::ON )
	{
		est_out.m_q_dot_avail = cr_out_solver.m_q_thermal;			//[MWt]
		est_out.m_m_dot_avail = cr_out_solver.m_m_dot_salt_tot;		//[kg/hr]
		est_out.m_T_htf_hot = cr_out_solver.m_T_salt_hot;			//[C]
		est_out.m_q_startup_avail = 0.0;
	}
	else
	{
		est_out.m_q_startup_avail = cr_out_solver.m_q_thermal;		//[MWt]
		est_out.m_q_dot_avail = 0.0;
		est_out.m_m_dot_avail = 0.0;
		est_out.m_T_htf_hot = 0.0;
	}
}

double C_csp_mspt_collector_receiver::calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim )
{
    /*
    Evaluate optical efficiency. This is a required function for the parent class, 
    but doesn't do much other than simply call the optical efficiency model in this case.
    */

    mc_pt_heliostatfield.call(weather, 1., sim );

    return mc_pt_heliostatfield.ms_outputs.m_eta_field;
}

double C_csp_mspt_collector_receiver::get_collector_area()
{
    //C_pt_heliostatfield::S_params *p = &mc_pt_heliostatfield.ms_params;

    //return p->m_dens_mirror * p->m_helio_height * p->m_helio_width * (double)p->m_helio_positions.nrows();

    //return mc_pt_receiver.m_A_sf;
    return mc_pt_heliostatfield.ms_params.m_A_sf;
}

double C_csp_mspt_collector_receiver::calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_inc )
{
    /* 
    A very approximate thermal efficiency used for quick optimization performance projections
    */

    double T_eff = (mc_pt_receiver.m_T_htf_cold_des + mc_pt_receiver.m_T_htf_hot_des)*.55;

    double T_amb = weather.m_tdry + 273.15;
    double T_eff4 = T_eff * T_eff;
    T_eff4 *= T_eff4;
    double T_amb4 = T_amb * T_amb;
    T_amb4 *= T_amb4;

    double Arec = mc_pt_receiver.area_proj();

    double q_rad = 5.67e-8*mc_pt_receiver.m_epsilon * Arec * (T_eff4 - T_amb4) * 1.e-6;   //MWt

    double v = weather.m_wspd;
    double v2 = v*v;
    double v3 = v2*v;

    double q_conv = q_rad/2. * (-0.001129*v3 + 0.031229*v2 - 0.01822*v +0.962476);  //convection is about half radiation, scale by wind speed. surrogate regression from molten salt run.

    return max(1. - (q_rad + q_conv)/q_inc, 0.);

}


void C_csp_mspt_collector_receiver::converged()
{
	mc_pt_heliostatfield.converged();
	mc_pt_receiver.converged();

	// Hardcode to test...
	//mc_reported_outputs.set_timestep_output(E_Q_DOT_THERMAL, mc_pt_receiver.ms_outputs.m_Q_thermal);	//[MWt]
	mc_reported_outputs.set_timestep_outputs();
}

void C_csp_mspt_collector_receiver::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}
