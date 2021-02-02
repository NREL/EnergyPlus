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

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "lib_util.h"
#include "csp_dispatch.h"

#include <algorithm>

#include <sstream>

std::string C_csp_solver::tech_operating_modes_str[] =
{
    "ENTRY_MODE",  // = 0

    "CR_OFF__PC_OFF__TES_OFF__AUX_OFF",
    "CR_SU__PC_OFF__TES_OFF__AUX_OFF",
    "CR_ON__PC_SU__TES_OFF__AUX_OFF",
    "CR_ON__PC_SB__TES_OFF__AUX_OFF",

    "CR_ON__PC_RM_HI__TES_OFF__AUX_OFF",
    "CR_ON__PC_RM_LO__TES_OFF__AUX_OFF",

    "CR_DF__PC_MAX__TES_OFF__AUX_OFF",

    "CR_OFF__PC_SU__TES_DC__AUX_OFF",
    "CR_ON__PC_OFF__TES_CH__AUX_OFF",

    "SKIP_10",

    "CR_ON__PC_TARGET__TES_CH__AUX_OFF",
    "CR_ON__PC_TARGET__TES_DC__AUX_OFF",

    "CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF",

    "CR_DF__PC_OFF__TES_FULL__AUX_OFF",

    "CR_OFF__PC_SB__TES_DC__AUX_OFF",
    "CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF",
    "CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF",

    "CR_ON__PC_SB__TES_CH__AUX_OFF",
    "CR_SU__PC_MIN__TES_EMPTY__AUX_OFF",

    "SKIP_20",

    "CR_SU__PC_SB__TES_DC__AUX_OFF",
    "CR_ON__PC_SB__TES_DC__AUX_OFF",
    "CR_OFF__PC_TARGET__TES_DC__AUX_OFF",
    "CR_SU__PC_TARGET__TES_DC__AUX_OFF",
    "CR_ON__PC_RM_HI__TES_FULL__AUX_OFF",

    "CR_ON__PC_MIN__TES_EMPTY__AUX_OFF",

    "CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF",

    "CR_DF__PC_MAX__TES_FULL__AUX_OFF",

    "CR_ON__PC_SB__TES_FULL__AUX_OFF",

    "SKIP_30",

    "CR_SU__PC_SU__TES_DC__AUX_OFF",

    "CR_ON__PC_SU__TES_CH__AUX_OFF",

    "CR_DF__PC_SU__TES_FULL__AUX_OFF",

    "CR_DF__PC_SU__TES_OFF__AUX_OFF"
};

void C_timestep_fixed::init(double time_start /*s*/, double step /*s*/)
{
	ms_timestep.m_time_start = time_start;	//[s]
	ms_timestep.m_step = step;				//[s]
	ms_timestep.m_time = ms_timestep.m_time_start + ms_timestep.m_step;	//[s]
}

double C_timestep_fixed::get_end_time()
{
	return ms_timestep.m_time;	//[s]
}

void C_timestep_fixed::step_forward()
{
	ms_timestep.m_time_start = ms_timestep.m_time;	//[s] Set start time to previous end time
	ms_timestep.m_time += ms_timestep.m_step;		//[s] Step forward to step new end time
}

double C_timestep_fixed::get_step()
{
	return ms_timestep.m_step;
}

void C_csp_solver::C_csp_solver_kernel::init(C_csp_solver::S_sim_setup & sim_setup, double wf_step /*s*/, double baseline_step /*s*/, C_csp_messages & csp_messages)
{
	ms_sim_setup = sim_setup;
	
	// Compare steps: if necessary, set baseline to = weather file
	if(baseline_step > wf_step)
	{
		std::string msg = util::format("The input Baseline Simulation Timestep (%lg [s]) must be less than or equal to " 
								"the Weatherfile Timestep (%lg [s]). It was reset to the Weatherfile Timestep", baseline_step, wf_step);
		csp_messages.add_message(C_csp_messages::WARNING, msg);
		baseline_step = wf_step;
	}
	else if( (int)wf_step % (int)baseline_step != 0)
	{
		double wf_over_bl = wf_step / baseline_step;
		double wf_over_bl_new = ceil(wf_over_bl);
		double baseline_step_new = wf_step / wf_over_bl_new;

		std::string msg = util::format("The Weatherfile Timestep (%lg [s]) must be divisible by the "
								"input Baseline Simulation Timestep (%lg [s]). It was reset to %lg [s].", wf_step, baseline_step, baseline_step_new);

		csp_messages.add_message(C_csp_messages::WARNING, msg);
		baseline_step = baseline_step_new;
	}

	// Define start and steps for weatherfile, baseline, and sim_info timesteps
	double wf_time_start = ms_sim_setup.m_sim_time_start;			//[s]
	double baseline_time_start = ms_sim_setup.m_sim_time_start;		//[s]

	// Initialize the private C_timestep_fixed classes
	mc_ts_weatherfile.init(wf_time_start, wf_step);
	mc_ts_sim_baseline.init(baseline_time_start, baseline_step);

	// Set up 'mc_sim_info'
	mc_sim_info.ms_ts.m_time_start = ms_sim_setup.m_sim_time_start;	//[s]
	mc_sim_info.ms_ts.m_step = baseline_step;						//[s]
	mc_sim_info.ms_ts.m_time = mc_sim_info.ms_ts.m_time_start + mc_sim_info.ms_ts.m_step;	//[s]
}

double C_csp_solver::C_csp_solver_kernel::get_wf_end_time()
{
	return mc_ts_weatherfile.get_end_time();
}

double C_csp_solver::C_csp_solver_kernel::get_baseline_end_time()
{
	return mc_ts_sim_baseline.get_end_time();
}

void C_csp_solver::C_csp_solver_kernel::wf_step_forward()
{
	mc_ts_weatherfile.step_forward();
}

const C_csp_solver::S_sim_setup * C_csp_solver::C_csp_solver_kernel::get_sim_setup()
{
	return &ms_sim_setup;
}

double C_csp_solver::C_csp_solver_kernel::get_wf_step()
{
	return mc_ts_weatherfile.get_step();
}

double C_csp_solver::C_csp_solver_kernel::get_baseline_step()
{
	return mc_ts_sim_baseline.get_step();
}

void C_csp_solver::C_csp_solver_kernel::baseline_step_forward()
{
	mc_ts_sim_baseline.step_forward();
}

static C_csp_reported_outputs::S_output_info S_solver_output_info[] =
{
	// Ouputs that are NOT reported as weighted averages
		// Simulation
	{C_csp_solver::C_solver_outputs::TIME_FINAL, C_csp_reported_outputs::TS_LAST},	//[hr]
		// Weather Reader
	{ C_csp_solver::C_solver_outputs::MONTH, C_csp_reported_outputs::TS_1ST},		//[-] Month of year
	{ C_csp_solver::C_solver_outputs::HOUR_DAY, C_csp_reported_outputs::TS_1ST},		//[hr] hour of day
		// Controller, TES, & Dispatch
	{C_csp_solver::C_solver_outputs::ERR_M_DOT, C_csp_reported_outputs::TS_1ST},		//[-] Relative mass conservation error
	{C_csp_solver::C_solver_outputs::ERR_Q_DOT, C_csp_reported_outputs::TS_1ST},		//[-] Relative energy conservation error
	{C_csp_solver::C_solver_outputs::N_OP_MODES, C_csp_reported_outputs::TS_1ST},	//[-] Number of subtimesteps in reporting timestep
	{C_csp_solver::C_solver_outputs::OP_MODE_1, C_csp_reported_outputs::TS_1ST},     //[-] Operating mode in first subtimestep
	{C_csp_solver::C_solver_outputs::OP_MODE_2, C_csp_reported_outputs::TS_1ST},		//[-] Operating mode in second subtimestep
	{C_csp_solver::C_solver_outputs::OP_MODE_3, C_csp_reported_outputs::TS_1ST},		//[-] Operating mode in third subtimestep
	{C_csp_solver::C_solver_outputs::TOU_PERIOD, C_csp_reported_outputs::TS_1ST},                  //[-] CSP operating TOU period
	{C_csp_solver::C_solver_outputs::PRICING_MULT, C_csp_reported_outputs::TS_1ST},				  //[-] PPA price multiplier
	{C_csp_solver::C_solver_outputs::PC_Q_DOT_SB, C_csp_reported_outputs::TS_1ST},				  //[MWt] PC required standby thermal power
	{C_csp_solver::C_solver_outputs::PC_Q_DOT_MIN, C_csp_reported_outputs::TS_1ST},				  //[MWt] PC required min thermal power
	{C_csp_solver::C_solver_outputs::PC_Q_DOT_TARGET, C_csp_reported_outputs::TS_1ST},			  //[MWt] PC target thermal power
	{C_csp_solver::C_solver_outputs::PC_Q_DOT_MAX, C_csp_reported_outputs::TS_1ST},				  //[MWt] PC allowable max thermal power
	{C_csp_solver::C_solver_outputs::CTRL_IS_REC_SU, C_csp_reported_outputs::TS_1ST},			  //[-] Control decision: is receiver startup allowed?
	{C_csp_solver::C_solver_outputs::CTRL_IS_PC_SU, C_csp_reported_outputs::TS_1ST},				  //[-] Control decision: is power cycle startup allowed?
	{C_csp_solver::C_solver_outputs::CTRL_IS_PC_SB, C_csp_reported_outputs::TS_1ST},				  //[-] Control decision: is power cycle standby allowed?
	{C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_SU, C_csp_reported_outputs::TS_1ST},			  //[MWt] Estimate receiver startup thermal power
	{C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_ON, C_csp_reported_outputs::TS_1ST},			  //[MWt] Estimate receiver thermal power to HTF
	{C_csp_solver::C_solver_outputs::EST_Q_DOT_DC, C_csp_reported_outputs::TS_1ST},				  //[MWt] Estimate max TES dc thermal power
	{C_csp_solver::C_solver_outputs::EST_Q_DOT_CH, C_csp_reported_outputs::TS_1ST},				  //[MWt] Estimate max TES ch thermal power
	{C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_A, C_csp_reported_outputs::TS_1ST},		  //[-] First 3 operating modes tried
	{C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_B, C_csp_reported_outputs::TS_1ST},		  //[-] Next 3 operating modes tried
	{C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_C, C_csp_reported_outputs::TS_1ST},		  //[-] Final 3 operating modes tried
	{C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_STATE, C_csp_reported_outputs::TS_1ST},		  //[-] The status of the dispatch optimization solver
	{C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_ITER, C_csp_reported_outputs::TS_1ST},		  //[-] Number of iterations before completing dispatch optimization
	{C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ, C_csp_reported_outputs::TS_1ST},		  //[?] Objective function value achieved by the dispatch optimization solver
	{C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ_RELAX, C_csp_reported_outputs::TS_1ST},	  //[?] Objective function value for the relaxed continuous problem 
	{C_csp_solver::C_solver_outputs::DISPATCH_QSF_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWt] Expected total solar field energy generation in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_QSFPROD_EXPECT, C_csp_reported_outputs::TS_1ST},	  //[MWt] Expected useful solar field energy generation in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_QSFSU_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWt] Solar field startup energy in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_TES_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWht] Thermal energy storage charge state in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_PCEFF_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[-] Expected power cycle efficiency adjustment in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_SFEFF_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[-] Expected solar field thermal efficiency adjustment in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_QPBSU_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWt] Power cycle startup energy consumption in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_WPB_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWe] Power cycle electricity production in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_REV_EXPECT, C_csp_reported_outputs::TS_1ST},		  //[MWe*fact] Power cycle electricity production times revenue factor in dispatch model
	{C_csp_solver::C_solver_outputs::DISPATCH_PRES_NCONSTR, C_csp_reported_outputs::TS_1ST},		  //[-] Number of constraint relationships in dispatch model formulation
	{C_csp_solver::C_solver_outputs::DISPATCH_PRES_NVAR, C_csp_reported_outputs::TS_1ST},		  //[-] Number of variables in dispatch model formulation
	{C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_TIME, C_csp_reported_outputs::TS_1ST},		  //[sec]   Time required to solve the dispatch model at each instance

	// **************************************************************
	//      Outputs that are reported as weighted averages if 
	//       multiple csp-timesteps for one reporting timestep
	// **************************************************************

	{C_csp_solver::C_solver_outputs::SOLZEN, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[deg] Solar zenith angle
	{C_csp_solver::C_solver_outputs::SOLAZ, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[deg] Solar azimuth angle
	{C_csp_solver::C_solver_outputs::BEAM, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[W/m^2] Resource beam normal irradiance
	{C_csp_solver::C_solver_outputs::TDRY, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[C] Dry bulb temperature
	{C_csp_solver::C_solver_outputs::TWET, C_csp_reported_outputs::TS_WEIGHTED_AVE},			//[C] Wet bulb temperature
	{C_csp_solver::C_solver_outputs::RH, C_csp_reported_outputs::TS_WEIGHTED_AVE},				//[-] Relative humidity
	{C_csp_solver::C_solver_outputs::WSPD, C_csp_reported_outputs::TS_WEIGHTED_AVE},           //[m/s] Wind speed
	{C_csp_solver::C_solver_outputs::PRES, C_csp_reported_outputs::TS_WEIGHTED_AVE}, 			//[mbar] Atmospheric pressure
		
		// Controller and TES
	{C_csp_solver::C_solver_outputs::CR_DEFOCUS, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[-] Field optical focus fraction
	{C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWt] TES discharge thermal power
	{C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWt] TES charge thermal power
	{C_csp_solver::C_solver_outputs::TES_E_CH_STATE, C_csp_reported_outputs::TS_LAST},			//[MWht] TES charge state at the end of the time step
	
	{C_csp_solver::C_solver_outputs::M_DOT_CR_TO_TES_HOT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[kg/s]
	{C_csp_solver::C_solver_outputs::M_DOT_TES_HOT_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[kg/s]
	{C_csp_solver::C_solver_outputs::M_DOT_PC_TO_TES_COLD, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[kg/s]
	{C_csp_solver::C_solver_outputs::M_DOT_TES_COLD_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[kg/s]
	{C_csp_solver::C_solver_outputs::M_DOT_FIELD_TO_CYCLE, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[kg/s]
	{C_csp_solver::C_solver_outputs::M_DOT_CYCLE_TO_FIELD, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[kg/s]
	
	{C_csp_solver::C_solver_outputs::COL_W_DOT_TRACK, C_csp_reported_outputs::TS_WEIGHTED_AVE},	  //[MWe] Parasitic collector tracking, startup, stow power consumption
	{C_csp_solver::C_solver_outputs::CR_W_DOT_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWe] Parasitic tower HTF pump power
	{C_csp_solver::C_solver_outputs::SYS_W_DOT_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWe] Parasitic PC and TES HTF pump power
	{C_csp_solver::C_solver_outputs::PC_W_DOT_COOLING, C_csp_reported_outputs::TS_WEIGHTED_AVE},	  //[MWe] Parasitic condenser operation power
	{C_csp_solver::C_solver_outputs::SYS_W_DOT_FIXED, C_csp_reported_outputs::TS_WEIGHTED_AVE},	  //[MWe] Parasitic fixed power consumption
	{C_csp_solver::C_solver_outputs::SYS_W_DOT_BOP, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWe] Parasitic BOP power consumption
	{C_csp_solver::C_solver_outputs::W_DOT_NET, C_csp_reported_outputs::TS_WEIGHTED_AVE},			  //[MWe] System total electric power to grid

	csp_info_invalid
};

C_csp_solver::C_csp_solver(C_csp_weatherreader &weather,
	C_csp_collector_receiver &collector_receiver,
	C_csp_power_cycle &power_cycle,
	C_csp_tes &tes,
	C_csp_tou &tou,
	S_csp_system_params &system,
	bool(*pf_callback)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type),
	void *p_cmod_active) :
	mc_weather(weather), 
	mc_collector_receiver(collector_receiver), 
	mc_power_cycle(power_cycle),
	mc_tes(tes),
	mc_tou(tou),
	ms_system_params(system)
{

	// Hierarchy logic
	reset_hierarchy_logic();

	// Inititalize non-reference member data
	m_T_htf_cold_des = m_P_cold_des = m_x_cold_des =
		m_q_dot_rec_des = m_A_aperture =
		m_cycle_W_dot_des = m_cycle_eta_des = m_cycle_q_dot_des = m_cycle_max_frac = m_cycle_cutoff_frac =
		m_cycle_sb_frac_des = m_cycle_T_htf_hot_des =
		m_cycle_P_hot_des = m_cycle_x_hot_des = 
		m_m_dot_pc_des = m_m_dot_pc_min =
        m_m_dot_pc_max = m_m_dot_pc_max_startup = m_T_htf_pc_cold_est = std::numeric_limits<double>::quiet_NaN();

    m_is_cr_config_recirc = true;

	// Reporting and Output Tracking
	mc_reported_outputs.construct(S_solver_output_info);

	m_i_reporting = -1;
	//m_sim_time_start = 
	//m_sim_time_end = 
	//m_sim_step_size_baseline =
	m_report_time_start = m_report_time_end = m_report_step = std::numeric_limits<double>::quiet_NaN();

	m_step_tolerance = 10.0;		//[s] For adjustable timesteps, if within 10 seconds, assume it equals baseline timestep

	m_op_mode_tracking.resize(0);

	error_msg = "";

	mv_time_local.reserve(10);

	mpf_callback = pf_callback;
	mp_cmod_active = p_cmod_active;

	// Solved Controller Variables
	m_defocus = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_pc_max = std::numeric_limits<double>::quiet_NaN();  //[MWt]
}

void C_csp_solver::send_callback(double percent)
{
	if (mpf_callback && mp_cmod_active)
	{
		int out_type = 1;
		std::string out_msg = "";
		std::string prg_msg = "Simulation Progress";

		while (mc_csp_messages.get_message(&out_type, &out_msg))
		{
			mpf_callback(out_msg, prg_msg, mp_cmod_active, percent, out_type);
		}

		out_msg = "";
		bool cmod_ret = mpf_callback(out_msg, prg_msg, mp_cmod_active, percent, out_type);

		if (!cmod_ret)
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_csp_solver";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}
}

void C_csp_solver::reset_hierarchy_logic()
{
	m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = true;
	m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = true;
	m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = true;

	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = true;
	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = true;

	m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = true;

	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = true;
	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = true;

	m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = true;
	m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = true;

	m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = true;

	m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = true;
	m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail = true;
	m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = true;
	
	m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = true;
	m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = true;
	m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = true;
	m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = true;

	m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = true;
	m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = true;
	m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = true;

	m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = true;

	m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = true;

	m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = true;

	m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = true;

	m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = true;

	m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = true;

	m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = true;

	m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = true;
} 

void C_csp_solver::turn_off_plant()
{
	m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = false;
	m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
	m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = false;
	m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;
	m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = false;
	m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;

	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = false;
	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;

	m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;

	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = false;
	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = false;

	m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = false;
	m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

	m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = false;

	m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = false;
	m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
	m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

	m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = false;
	m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
	m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;
	m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = false;

	m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = false;
	m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;
	m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = false;

	m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;

	m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

	m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;

	m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = false;

	m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = false;

	m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = false;

	m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;

	m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
}

double C_csp_solver::get_cr_aperture_area()
{
	return m_A_aperture;	//[m2]
}

void C_csp_solver::init()
{
	// First, initialize each component and update solver-level membe data as necessary
		// Weather reader
	mc_weather.init();
		// Collector-receiver
	C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
	init_inputs.m_latitude = mc_weather.ms_solved_params.m_lat;		//[deg]
	init_inputs.m_longitude = mc_weather.ms_solved_params.m_lon;	//[deg]
    init_inputs.m_tz = mc_weather.ms_solved_params.m_tz;	    	//[hr]
	init_inputs.m_shift = mc_weather.ms_solved_params.m_shift;		//[deg]
    init_inputs.m_elev = mc_weather.ms_solved_params.m_elev;		//[m]
	C_csp_collector_receiver::S_csp_cr_solved_params cr_solved_params;
	
	mc_collector_receiver.init(init_inputs, cr_solved_params);
	mc_csp_messages.transfer_messages(mc_collector_receiver.mc_csp_messages);
	
	m_T_htf_cold_des = cr_solved_params.m_T_htf_cold_des;		//[K]
	m_P_cold_des = cr_solved_params.m_P_cold_des;				//[kPa]
	m_x_cold_des = cr_solved_params.m_x_cold_des;				//[-]
	m_q_dot_rec_des = cr_solved_params.m_q_dot_rec_des;			//[MW]
	m_A_aperture = cr_solved_params.m_A_aper_total;				//[m2]
		// Power cycle
	C_csp_power_cycle::S_solved_params pc_solved_params;
	mc_power_cycle.init(pc_solved_params);
	m_cycle_W_dot_des = pc_solved_params.m_W_dot_des;					//[MW]
	m_cycle_eta_des = pc_solved_params.m_eta_des;						//[-]
	m_cycle_q_dot_des = pc_solved_params.m_q_dot_des;					//[MW]
	m_cycle_max_frac = pc_solved_params.m_max_frac;						//[-]
	m_cycle_cutoff_frac = pc_solved_params.m_cutoff_frac;				//[-]
	m_cycle_sb_frac_des = pc_solved_params.m_sb_frac;					//[-]
	m_cycle_T_htf_hot_des = pc_solved_params.m_T_htf_hot_ref + 273.15;	//[K] convert from C
	m_m_dot_pc_des = pc_solved_params.m_m_dot_design;					//[kg/hr]
				
	m_m_dot_pc_min = 0.0 * pc_solved_params.m_m_dot_min;		//[kg/hr]
	m_m_dot_pc_max_startup = pc_solved_params.m_m_dot_max;		//[kg/hr]				
	
	m_cycle_P_hot_des = pc_solved_params.m_P_hot_des;					//[kPa]
	m_cycle_x_hot_des = pc_solved_params.m_x_hot_des;					//[-]
		// TES
    C_csp_tes::S_csp_tes_init_inputs tes_init_inputs;
    tes_init_inputs.T_to_cr_at_des = cr_solved_params.m_T_htf_cold_des;
    tes_init_inputs.T_from_cr_at_des = cr_solved_params.m_T_htf_hot_des;
    tes_init_inputs.P_to_cr_at_des = cr_solved_params.m_dP_sf;
	mc_tes.init(tes_init_inputs);
		// TOU
    mc_tou.mc_dispatch_params.m_isleapyear = mc_weather.ms_solved_params.m_leapyear;
	mc_tou.init();
	mc_tou.init_parent();
		// Thermal Storage
	m_is_tes = mc_tes.does_tes_exist();



    m_is_cr_config_recirc = true;

    // Value helps solver get out of T_field_htf_cold iteration when weird conditions cause the solution to be a very cold value
    // Should update with technology-specific htf freeze protection values
    m_T_field_cold_limit = -100.0;      //[C]
    m_T_field_in_hot_limit = (0.9*m_cycle_T_htf_hot_des + 0.1*m_T_htf_cold_des) - 273.15;   //[C]


	if( mc_collector_receiver.m_is_sensible_htf != mc_power_cycle.m_is_sensible_htf )
	{
		throw(C_csp_exception("The collector-receiver and power cycle models have incompatible HTF - direct/indirect assumptions", "CSP Solver"));
	}

    /* 
    If no TES exists, initialize values to zero. They won't be touched again
    */

	if(!m_is_tes)
	{	// Set constant values for tes HTF states

		mc_tes_outputs.m_q_heater = 0.0;		//[MW]
		mc_tes_outputs.m_q_dot_dc_to_htf = 0.0;	//[MW]
		mc_tes_outputs.m_q_dot_ch_from_htf = 0.0;	//[MW]
		
		mc_tes_outputs.m_m_dot_cr_to_tes_hot = 0.0;		//[kg/s]
		mc_tes_outputs.m_m_dot_tes_hot_out = 0.0;		//[kg/s]
		mc_tes_outputs.m_m_dot_pc_to_tes_cold = 0.0;	//[kg/s]
		mc_tes_outputs.m_m_dot_tes_cold_out = 0.0;		//[kg/s]
		mc_tes_outputs.m_m_dot_field_to_cycle = 0.0;	//[kg/s]
		mc_tes_outputs.m_m_dot_cycle_to_field = 0.0;	//[kg/s]

		mc_tes_outputs.m_m_dot_cold_tank_to_hot_tank = 0.0;
	}
}

int C_csp_solver::steps_per_hour()
{
	// Get number of records in weather file
	int n_wf_records = (int)mc_weather.m_weather_data_provider->nrecords();
	int step_per_hour = n_wf_records / 8760;
	return step_per_hour;
}

void C_csp_solver::Ssimulate(C_csp_solver::S_sim_setup & sim_setup)
{
	// Get number of records in weather file
	int n_wf_records = (int)mc_weather.m_weather_data_provider->nrecords();
	int step_per_hour = n_wf_records / 8760;

	double wf_step = 3600.0 / step_per_hour;	//[s] Weather file time step - would like to check this against weather file, some day
	
    m_is_first_timestep = true;
	double baseline_step = wf_step;		//[s] Baseline timestep of the simulation - this should probably be technology/model specific
	// Check the collector-receiver model for a maximum step
	if(mc_collector_receiver.m_max_step > 0.0)
	{
		baseline_step = max(m_step_tolerance, min(baseline_step, mc_collector_receiver.m_max_step));
	}
	
	mc_kernel.init(sim_setup, wf_step, baseline_step, mc_csp_messages);

    //instantiate dispatch optimization object
    csp_dispatch_opt dispatch;
    //load parameters used by dispatch algorithm
    //-------------------------------    
    
	if( mc_tou.mc_dispatch_params.m_dispatch_optimize )
	{
		dispatch.copy_weather_data(mc_weather);
		dispatch.params.col_rec = &mc_collector_receiver;
		dispatch.params.mpc_pc = &mc_power_cycle;
		dispatch.params.siminfo = &mc_kernel.mc_sim_info;
		dispatch.params.messages = &mc_csp_messages;

		dispatch.params.dt = 1./(double)mc_tou.mc_dispatch_params.m_disp_steps_per_hour;  //hr
		dispatch.params.dt_pb_startup_cold = mc_power_cycle.get_cold_startup_time();
		dispatch.params.dt_pb_startup_hot = mc_power_cycle.get_hot_startup_time();
		dispatch.params.q_pb_standby = mc_power_cycle.get_standby_energy_requirement()*1000.;
		dispatch.params.e_pb_startup_cold = mc_power_cycle.get_cold_startup_energy()*1000.;
		dispatch.params.e_pb_startup_hot = mc_power_cycle.get_hot_startup_energy()*1000.;

		dispatch.params.dt_rec_startup = mc_collector_receiver.get_startup_time() / 3600.;
		dispatch.params.e_rec_startup = mc_collector_receiver.get_startup_energy() * 1000;
		dispatch.params.q_rec_min = mc_collector_receiver.get_min_power_delivery()*1000.;
		dispatch.params.w_rec_pump = mc_collector_receiver.get_pumping_parasitic_coef();


		dispatch.params.e_tes_init = mc_tes.get_initial_charge_energy() * 1000;
		dispatch.params.e_tes_min = mc_tes.get_min_charge_energy() * 1000;
		dispatch.params.e_tes_max = mc_tes.get_max_charge_energy() * 1000;
		dispatch.params.tes_degrade_rate = mc_tes.get_degradation_rate();

		dispatch.params.q_pb_max = mc_power_cycle.get_max_thermal_power() * 1000;
		dispatch.params.q_pb_min = mc_power_cycle.get_min_thermal_power() * 1000;
		dispatch.params.q_pb_des = m_cycle_q_dot_des*1000.;
		dispatch.params.eta_cycle_ref = mc_power_cycle.get_efficiency_at_load(1.);

        dispatch.params.disp_time_weighting = mc_tou.mc_dispatch_params.m_disp_time_weighting;
		dispatch.params.rsu_cost = mc_tou.mc_dispatch_params.m_rsu_cost;
		dispatch.params.csu_cost = mc_tou.mc_dispatch_params.m_csu_cost;
		dispatch.params.pen_delta_w = mc_tou.mc_dispatch_params.m_pen_delta_w;
        dispatch.params.disp_inventory_incentive = mc_tou.mc_dispatch_params.m_disp_inventory_incentive;
		dispatch.params.q_rec_standby = mc_tou.mc_dispatch_params.m_q_rec_standby;
		
		dispatch.params.w_rec_ht = mc_tou.mc_dispatch_params.m_w_rec_ht;
		dispatch.params.w_track = mc_collector_receiver.get_tracking_power()*1000.0;	//kWe
		dispatch.params.w_stow = mc_collector_receiver.get_col_startup_power()*1000.0;	//kWe-hr
		dispatch.params.w_cycle_pump = mc_power_cycle.get_htf_pumping_parasitic_coef();// kWe/kWt
		dispatch.params.w_cycle_standby = dispatch.params.q_pb_standby*dispatch.params.w_cycle_pump; //kWe

		//Cycle efficiency
		dispatch.params.eff_table_load.clear();
		//add zero point
		dispatch.params.eff_table_load.add_point(0., 0.);    //this is required to allow the model to converge

		int neff = 2;   //mjw: if using something other than 2, the linear approximation assumption and associated code in csp_dispatch.cpp/calculate_parameters() needs to be reformulated.
		for(int i=0; i<neff; i++)
		{
			double x = dispatch.params.q_pb_min + (dispatch.params.q_pb_max - dispatch.params.q_pb_min)/(double)(neff - 1)*i;
			double xf = x * 1.e-3/m_cycle_q_dot_des;  //MW

			double eta;
        
			//eta = 0.86 + xf * 0.28 - xf*xf * 0.14;  //Equation from curve fit of power tower steam rankine Type 224
			//eta *= m_cycle_eta_des;
			eta = mc_power_cycle.get_efficiency_at_load(xf);

			dispatch.params.eff_table_load.add_point(x, eta);
		}

		//cycle efficiency vs temperature
		dispatch.params.eff_table_Tdb.clear();
        dispatch.params.wcondcoef_table_Tdb.clear();
		int neffT = 40;

		for(int i=0; i<neffT; i++)
		{
			double T = -10. + 60./(double)(neffT - 1) * i;
            double wcond;
			double eta = mc_power_cycle.get_efficiency_at_TPH(T, 1., 30., &wcond) / m_cycle_eta_des;  

			dispatch.params.eff_table_Tdb.add_point(T, eta);
            dispatch.params.wcondcoef_table_Tdb.add_point(T, wcond/m_cycle_W_dot_des); //fraction of rated gross gen
		}

	}
    

        //solver parameters
    dispatch.solver_params.max_bb_iter = mc_tou.mc_dispatch_params.m_max_iterations;
    dispatch.solver_params.mip_gap = mc_tou.mc_dispatch_params.m_mip_gap;
    dispatch.solver_params.solution_timeout = mc_tou.mc_dispatch_params.m_solver_timeout;
    dispatch.solver_params.bb_type = mc_tou.mc_dispatch_params.m_bb_type;
    dispatch.solver_params.disp_reporting = mc_tou.mc_dispatch_params.m_disp_reporting;
    dispatch.solver_params.scaling_type = mc_tou.mc_dispatch_params.m_scaling_type;
    dispatch.solver_params.presolve_type = mc_tou.mc_dispatch_params.m_presolve_type;
    dispatch.solver_params.is_write_ampl_dat = mc_tou.mc_dispatch_params.m_is_write_ampl_dat;
    dispatch.solver_params.is_ampl_engine = mc_tou.mc_dispatch_params.m_is_ampl_engine;
    dispatch.solver_params.ampl_data_dir = mc_tou.mc_dispatch_params.m_ampl_data_dir;
    dispatch.solver_params.ampl_exec_call = mc_tou.mc_dispatch_params.m_ampl_exec_call;
    //-------------------------------

        
	int cr_operating_state = C_csp_collector_receiver::OFF;
	int pc_operating_state = C_csp_power_cycle::OFF;

	
	double tol_mode_switching = 0.10;		// Give buffer to account for uncertainty in estimates


	// Reset vector that tracks operating modes
	m_op_mode_tracking.resize(0);

	// Reset Controller Variables to Defaults
	m_defocus = 1.0;		//[-]  

	m_i_reporting = 0;
	m_report_time_start = mc_kernel.get_sim_setup()->m_sim_time_start;			//[s]
	m_report_step = sim_setup.m_report_step;		//[s]
	m_report_time_end = m_report_time_start + m_report_step;	//[s]

	double progress_msg_interval_frac = 0.02;
	double progress_msg_frac_current = progress_msg_interval_frac;
	double V_hot_tank_frac_initial;

    /* 
    ************************** MAIN TIME-SERIES LOOP **************************
    */

    double disp_time_last = -9999.;

    //values to report later on the dispatch algorithm
    double disp_qsf_expect = 0.;
    double disp_qsfprod_expect = 0.;
    double disp_qsfsu_expect = 0.;
    double disp_tes_expect = 0.;
    double disp_etasf_expect = 0.;
    double disp_etapb_expect = 0.;
    double disp_qpbsu_expect = 0.;
    double disp_wpb_expect = 0.;
    double disp_rev_expect = 0.;
    //field efficiency learning parameters
    double disp_qsf_last = 0.;
    double disp_qsf_effadj = 1.;
    double disp_effadj_weight = 0.;
    //int disp_effadj_count = 0;

	// Block dispatch saved variables
	bool is_q_dot_pc_target_overwrite = false;

	//mf_callback(m_cdata, 0.0, 0, 0.0);

    double start_time = mc_kernel.get_sim_setup()->m_sim_time_start;
    if( start_time != 0. )
        mc_csp_messages.add_message(C_csp_messages::WARNING, util::format("Start time: %f", start_time) );

    double end_time = mc_kernel.get_sim_setup()->m_sim_time_end;
    if(end_time != 8760*3600.)
        mc_csp_messages.add_message(C_csp_messages::WARNING, util::format("End time: %f", end_time) );

    int operating_mode = ENTRY_MODE;
    std::string operating_mode_str = tech_operating_modes_str[operating_mode];
    std::string operating_mode_str_prev = "";
    std::string op_mode_str = "";
    std::string op_mode_str_prev = "";

	while( mc_kernel.mc_sim_info.ms_ts.m_time <= mc_kernel.get_sim_setup()->m_sim_time_end )
	{
		// Report simulation progress
		double calc_frac_current = (mc_kernel.mc_sim_info.ms_ts.m_time - mc_kernel.get_sim_setup()->m_sim_time_start) / (mc_kernel.get_sim_setup()->m_sim_time_end - mc_kernel.get_sim_setup()->m_sim_time_start);
		if( calc_frac_current > progress_msg_frac_current )
		{
			send_callback( (float)calc_frac_current*100.f );

			progress_msg_frac_current += progress_msg_interval_frac;
		}
		
		// Get tou for timestep
		mc_tou.call(mc_kernel.mc_sim_info.ms_ts.m_time, mc_tou_outputs);
		size_t tou_period = mc_tou_outputs.m_csp_op_tou;	//[-]
		double f_turbine_tou = mc_tou_outputs.m_f_turbine;	//[-]
		double pricing_mult = mc_tou_outputs.m_price_mult;	//[-]

		// Get collector/receiver & power cycle operating states at start of time step (last time step)
		cr_operating_state = mc_collector_receiver.get_operating_state();
		if( cr_operating_state < C_csp_collector_receiver::OFF ||
			cr_operating_state > C_csp_collector_receiver::ON )
		{
			std::string msg = util::format("The collector-receiver operating state at time %lg [hr] is %d. Recognized"
				" values are from %d to %d\n", mc_kernel.mc_sim_info.ms_ts.m_step/ 3600.0, cr_operating_state, C_csp_collector_receiver::OFF, C_csp_collector_receiver::ON);
			throw(C_csp_exception(msg,"CSP Solver Core"));
		}
		pc_operating_state = mc_power_cycle.get_operating_state();
        if (m_is_first_timestep && f_turbine_tou <= 0.) pc_operating_state = C_csp_power_cycle::OFF;

		// Calculate maximum thermal power to power cycle for startup. This will be zero if power cycle is on.
		double q_dot_pc_su_max = mc_power_cycle.get_max_q_pc_startup();		//[MWt]

		// Get weather at this timestep. Should only be called once per timestep. (Except converged() function)
		mc_weather.timestep_call(mc_kernel.mc_sim_info);

		// Get volume of hot hot tank, for debugging
		V_hot_tank_frac_initial = mc_tes.get_hot_tank_vol_frac();

		// Get or set decision variables
		bool is_rec_su_allowed = true;
		bool is_pc_su_allowed = true;
		bool is_pc_sb_allowed = true;
		mc_kernel.mc_sim_info.m_tou = 1;	    //[base 1] used ONLY by power cycle model for hybrid cooling - may also want to move this to controller

		// Get standby fraction and min operating fraction
			// Could eventually be a method in PC class...
		double cycle_sb_frac = m_cycle_sb_frac_des;				//[-]
			
			// *** If standby not allowed, then reset q_pc_sb = q_pc_min ?? *** 
                //or is this too confusing and not helpful enough?
		double q_pc_sb = cycle_sb_frac * m_cycle_q_dot_des;		//[MW]
		double q_pc_min = m_cycle_cutoff_frac * m_cycle_q_dot_des;	//[MW]
		m_q_dot_pc_max = m_cycle_max_frac * m_cycle_q_dot_des;		//[MWt]
		double q_pc_target = m_q_dot_pc_max;							//[MW]

		q_pc_target = f_turbine_tou * m_cycle_q_dot_des;	//[MW]

        if (mc_tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max)
        {
            m_q_dot_pc_max = q_pc_target;     //[MW]
        }


		double m_dot_htf_ND_max = std::numeric_limits<double>::quiet_NaN();
		double W_dot_ND_max = std::numeric_limits<double>::quiet_NaN();
		mc_power_cycle.get_max_power_output_operation_constraints(mc_weather.ms_outputs.m_tdry, m_dot_htf_ND_max, W_dot_ND_max);
		m_m_dot_pc_max = m_dot_htf_ND_max * m_m_dot_pc_des;



		// Need to call power cycle at ambient temperature to get a guess of HTF return temperature
		// If the return temperature is hotter than design, then the mass flow from the receiver will be
		// bigger than expected
		mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15; //[C]
		mc_pc_htf_state_in.m_pres = m_cycle_P_hot_des;	//[kPa]
		mc_pc_htf_state_in.m_qual = m_cycle_x_hot_des;	//[-]
		mc_pc_inputs.m_m_dot = (std::min)(m_m_dot_pc_max, m_m_dot_pc_des);				//[kg/hr] no mass flow rate to power cycle
		// Inputs
		mc_pc_inputs.m_standby_control = C_csp_power_cycle::ON;
		//mc_pc_inputs.m_tou = tou_timestep;
		// Performance Call
		mc_power_cycle.call(mc_weather.ms_outputs,
			mc_pc_htf_state_in,
			mc_pc_inputs,
			mc_pc_out_solver,
			mc_kernel.mc_sim_info);
		
		
		m_T_htf_pc_cold_est = mc_pc_out_solver.m_T_htf_cold;	//[C]
		// Solve collector/receiver at steady state with design inputs and weather to estimate output
		mc_cr_htf_state_in.m_temp = m_T_htf_pc_cold_est;	//[C]
		C_csp_collector_receiver::S_csp_cr_est_out est_out;
		mc_collector_receiver.estimates(mc_weather.ms_outputs,
			mc_cr_htf_state_in,
			est_out,
			mc_kernel.mc_sim_info);
		double q_dot_cr_startup = est_out.m_q_startup_avail;
		double q_dot_cr_on = est_out.m_q_dot_avail;
		double m_dot_cr_on = est_out.m_m_dot_avail;		//[kg/hr]
		double T_htf_hot_cr_on = est_out.m_T_htf_hot;	//[C]
		if (cr_operating_state != C_csp_collector_receiver::ON)
			T_htf_hot_cr_on = m_cycle_T_htf_hot_des - 273.15;	//[C]

		// Get TES operating state info at end of last time step
		double q_dot_tes_dc, q_dot_tes_ch;      //[MWt]
		q_dot_tes_dc = q_dot_tes_ch = std::numeric_limits<double>::quiet_NaN();
		double m_dot_tes_dc_est, m_dot_tes_ch_est;
		if (m_is_tes)
		{
			//predict estimated amount of charge/discharge available
			double T_hot_field_dc_est;	//[K]
			T_hot_field_dc_est = std::numeric_limits<double>::quiet_NaN();
			mc_tes.discharge_avail_est(m_T_htf_pc_cold_est + 273.15, mc_kernel.mc_sim_info.ms_ts.m_step, q_dot_tes_dc, m_dot_tes_dc_est, T_hot_field_dc_est);
			m_dot_tes_dc_est *= 3600.0;	//[kg/hr] convert from kg/s

			double T_cold_field_ch_est;	//[K]
			T_cold_field_ch_est = std::numeric_limits<double>::quiet_NaN();
			mc_tes.charge_avail_est(T_htf_hot_cr_on + 273.15, mc_kernel.mc_sim_info.ms_ts.m_step, q_dot_tes_ch, m_dot_tes_ch_est, T_cold_field_ch_est);
			m_dot_tes_ch_est *= 3600.0;	//[kg/hr] convert from kg/s
		}
		else
		{
			q_dot_tes_dc = q_dot_tes_ch = 0.0;
			m_dot_tes_dc_est = m_dot_tes_ch_est = 0.0;
		}

        // Check that there is enough discharge energy to operate cycle for a 'reasonable' fraction of the timestep
        double t_q_dot_min = fmax(0.05*mc_kernel.mc_sim_info.ms_ts.m_step, m_step_tolerance);   //[s]
        if (q_dot_tes_dc * mc_kernel.mc_sim_info.ms_ts.m_step < m_cycle_q_dot_des * t_q_dot_min)
        {
            q_dot_tes_dc = 0.0;     //[s
        }



		// Can add the following code to simulate with no storage charge/discharge, but IDLE calcs
		//q_dot_tes_dc = q_dot_tes_ch = 0.0;

		// Optional rules for TOD Block Plant Control
		if( mc_tou.mc_dispatch_params.m_is_block_dispatch )
		{

			// Rule 1: if the sun sets (or does not rise) in __ [hours], then do not allow power cycle standby
				//double standby_time_buffer = 2.0;
			if( mc_tou.mc_dispatch_params.m_use_rule_1 &&
				(mc_weather.ms_outputs.m_hour + mc_tou.mc_dispatch_params.m_standby_off_buffer <= mc_weather.ms_outputs.m_time_rise ||
				mc_weather.ms_outputs.m_hour + mc_tou.mc_dispatch_params.m_standby_off_buffer >= mc_weather.ms_outputs.m_time_set))
			{
				is_pc_sb_allowed = false;
			}

			// Rule 2:
			if( mc_tou.mc_dispatch_params.m_use_rule_2 &&
				((q_pc_target < q_pc_min && q_dot_tes_ch < m_q_dot_rec_des*mc_tou.mc_dispatch_params.m_q_dot_rec_des_mult) ||
				is_q_dot_pc_target_overwrite) )
			{
				// If overwrite was previously true, but now power cycle is off, set to false
				if( is_q_dot_pc_target_overwrite && 
				(pc_operating_state == C_csp_power_cycle::OFF || q_pc_target >= q_pc_min) )
				{
					is_q_dot_pc_target_overwrite = false;
				}
				else
				{
					is_q_dot_pc_target_overwrite = true;
				}

				if( is_q_dot_pc_target_overwrite )
				{
					q_pc_target = mc_tou.mc_dispatch_params.m_f_q_dot_pc_overwrite*m_cycle_q_dot_des;
				}
			}
		}



		// After rules, reset booleans if necessary
		if( q_pc_target < q_pc_min || q_pc_target <= 0. )
		{
			is_pc_su_allowed = false;
			is_pc_sb_allowed = false;
			q_pc_target = 0.0;
		}



        bool opt_complete = false;

        //Run dispatch optimization?
        if(mc_tou.mc_dispatch_params.m_dispatch_optimize)
        {

            //time to reoptimize
            int opt_horizon = mc_tou.mc_dispatch_params.m_optimize_horizon;

            double hour_now = mc_kernel.mc_sim_info.ms_ts.m_time/3600.;

            //reoptimize when the time is equal to multiples of the first time step
			if( (int)mc_kernel.mc_sim_info.ms_ts.m_time % (int)(3600.*mc_tou.mc_dispatch_params.m_optimize_frequency) == baseline_step
				&& disp_time_last != mc_kernel.mc_sim_info.ms_ts.m_time
                )
            {
                //if this is the last day of the year, update the optimization horizon to be no more than the last 24 hours. 
				
                if( hour_now >= (8760 - opt_horizon) )
                    opt_horizon = (int)min((double)opt_horizon, (double)(8761-hour_now));

                //message
                stringstream ss;
                ss << "Optimizing thermal energy dispatch profile for time window " 
					<< (int)(mc_kernel.mc_sim_info.ms_ts.m_time / 3600.) << " - "
					<< (int)(mc_kernel.mc_sim_info.ms_ts.m_time / 3600.) + mc_tou.mc_dispatch_params.m_optimize_frequency;
                
                mc_csp_messages.add_message(C_csp_messages::NOTICE, ss.str());

				send_callback((float)calc_frac_current*100.f);

                ss.flush();

                //get the new price signal
                dispatch.price_signal.clear();
                dispatch.price_signal.resize(opt_horizon*mc_tou.mc_dispatch_params.m_disp_steps_per_hour, 1.);

                for(int t=0; t<opt_horizon*mc_tou.mc_dispatch_params.m_disp_steps_per_hour; t++)
                {
					mc_tou.call(mc_kernel.mc_sim_info.ms_ts.m_time + t * 3600./(double)mc_tou.mc_dispatch_params.m_disp_steps_per_hour, mc_tou_outputs);
		            dispatch.price_signal.at(t) = mc_tou_outputs.m_price_mult;
                }

				// get the new electricity generation limits
				dispatch.w_lim.clear();
				dispatch.w_lim.resize(opt_horizon*mc_tou.mc_dispatch_params.m_disp_steps_per_hour, 1.e99);
				int hour_start = (int)(ceil (mc_kernel.mc_sim_info.ms_ts.m_time / 3600. - 1.e-6)) - 1;
				for (int t = 0; t<opt_horizon; t++)
				{
					for (int d = 0; d < mc_tou.mc_dispatch_params.m_disp_steps_per_hour; d++)
						dispatch.w_lim.at(t*mc_tou.mc_dispatch_params.m_disp_steps_per_hour+d) = mc_tou.mc_dispatch_params.m_w_lim_full.at(hour_start + t);
				}


                //note the states of the power cycle and receiver
                dispatch.params.is_pb_operating0 = mc_power_cycle.get_operating_state() == 1;
                dispatch.params.is_pb_standby0 = mc_power_cycle.get_operating_state() == 2;
                dispatch.params.is_rec_operating0 = mc_collector_receiver.get_operating_state() == C_csp_collector_receiver::ON;
                dispatch.params.q_pb0 = mc_pc_out_solver.m_q_dot_htf * 1000.;

                if(dispatch.params.q_pb0 != dispatch.params.q_pb0 )
                    dispatch.params.q_pb0 = 0.;
            
                //time
                dispatch.params.info_time = mc_kernel.mc_sim_info.ms_ts.m_time; //s

                //Note the state of the thermal energy storage system
                double q_disch, m_dot_disch, T_tes_return;
				mc_tes.discharge_avail_est(m_T_htf_cold_des, mc_kernel.mc_sim_info.ms_ts.m_step, q_disch, m_dot_disch, T_tes_return);
				dispatch.params.e_tes_init = q_disch * 1000. * mc_kernel.mc_sim_info.ms_ts.m_step / 3600. + dispatch.params.e_tes_min;        //kWh
		        if(dispatch.params.e_tes_init < dispatch.params.e_tes_min )
                    dispatch.params.e_tes_init = dispatch.params.e_tes_min;
                if(dispatch.params.e_tes_init > dispatch.params.e_tes_max )
                    dispatch.params.e_tes_init = dispatch.params.e_tes_max;

                //predict performance for the time horizon
                if( 
                    dispatch.predict_performance((int)
                            (mc_kernel.mc_sim_info.ms_ts.m_time/ baseline_step - 1), 
                            (int)(opt_horizon * mc_tou.mc_dispatch_params.m_disp_steps_per_hour), 
                            (int)((3600./baseline_step)/mc_tou.mc_dispatch_params.m_disp_steps_per_hour)
                            ) 
                    )
                {
                    
                    //call the optimize method
                    opt_complete = dispatch.m_last_opt_successful = 
                        dispatch.optimize();
                    
                    if(dispatch.solver_params.disp_reporting && (! dispatch.solver_params.log_message.empty()) )
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, dispatch.solver_params.log_message.c_str() );
                    
					//mc_csp_messages.add_message(C_csp_messages::NOTICE, dispatch.solver_params.log_message.c_str());

                    dispatch.m_current_read_step = 0;   //reset
                }

                //call again to go back to original state
                mc_tou.call(mc_kernel.mc_sim_info.ms_ts.m_time, mc_tou_outputs);

            }

            //running from the optimized profile 
            if(
                dispatch.m_last_opt_successful 
                && dispatch.m_current_read_step < (int)dispatch.outputs.q_pb_target.size()
                )
            {

                //update the learned field efficiency adjustment factor
                if(disp_qsf_last > 0.)
                {
                    double qopt_last = dispatch.outputs.q_sf_expected.at( dispatch.m_current_read_step )*1.e-3;     //mw

                    double etanew = disp_qsf_last / qopt_last;

                    disp_effadj_weight += disp_qsf_last;
                    //disp_effadj_count ++;

                    //double wfact = disp_effadj_weight / (double)disp_effadj_count;
                    
                    disp_qsf_effadj =+ (1. - etanew)/(min(disp_effadj_weight/disp_qsf_last, 5.));
                }

                //read in other values

                //calculate the current read step, account for number of dispatch steps per hour and the simulation time step
                dispatch.m_current_read_step = (int)(mc_kernel.mc_sim_info.ms_ts.m_time * mc_tou.mc_dispatch_params.m_disp_steps_per_hour / 3600. - .001) 
                    % (mc_tou.mc_dispatch_params.m_optimize_frequency * mc_tou.mc_dispatch_params.m_disp_steps_per_hour ); 

                is_rec_su_allowed = dispatch.outputs.rec_operation.at( dispatch.m_current_read_step );
                is_pc_sb_allowed = dispatch.outputs.pb_standby.at( dispatch.m_current_read_step );
                is_pc_su_allowed = dispatch.outputs.pb_operation.at( dispatch.m_current_read_step ) || is_pc_sb_allowed;

                q_pc_target = (dispatch.outputs.q_pb_target.at( dispatch.m_current_read_step ) 
                    + dispatch.outputs.q_pb_startup.at( dispatch.m_current_read_step ) )
                    / 1000. ;

                //quality checks
				/*
                if(!is_pc_sb_allowed && (q_pc_target + 1.e-5 < q_pc_min))
                    is_pc_su_allowed = false;
                if(is_pc_sb_allowed)
                    q_pc_target = dispatch.params.q_pb_standby*1.e-3; 
				*/


				if (q_pc_target + 1.e-5 < q_pc_min)
				{
					is_pc_su_allowed = false;
					//is_pc_sb_allowed = false;
					q_pc_target = 0.0;
				}
                
				// Calculate approximate upper limit for power cycle thermal input at current electricity generation limit
				if (dispatch.w_lim.at(dispatch.m_current_read_step) < 1.e-6)
                    m_q_dot_pc_max = 0.0;
				else
				{
					double wcond;
					double eta_corr = mc_power_cycle.get_efficiency_at_TPH(mc_weather.ms_outputs.m_tdry, 1., 30., &wcond) / m_cycle_eta_des; 
					double eta_calc = dispatch.params.eta_cycle_ref * eta_corr;
					double eta_diff = 1.;
					int i = 0;
					while (eta_diff > 0.001 && i<20)
					{
						double q_pc_est = dispatch.w_lim.at(dispatch.m_current_read_step)*1.e-3 / eta_calc;			// Estimated power cycle thermal input at w_lim
						double eta_new = mc_power_cycle.get_efficiency_at_load(q_pc_est / m_cycle_q_dot_des) * eta_corr;		// Calculated power cycle efficiency
						eta_diff = fabs(eta_calc - eta_new);
						eta_calc = eta_new;
						i++;
					}
					m_q_dot_pc_max = fmin(m_q_dot_pc_max, dispatch.w_lim.at(dispatch.m_current_read_step)*1.e-3 / eta_calc); // Restrict max pc thermal input to *approximate* current allowable value (doesn't yet account for parasitics)
					m_q_dot_pc_max = fmax(m_q_dot_pc_max, q_pc_target);													// calculated q_pc_target accounts for parasitics --> can be higher than approximate limit 
				}

                //q_pc_sb = dispatch.outputs.q_pb_standby.at( dispatch.m_current_read_step ) / 1000. ;

                //disp_etapb_expect = dispatch.outputs.eta_pb_expected.at( dispatch.m_current_read_step ) 
                //                    /** m_cycle_eta_des*/ * ( dispatch.outputs.pb_operation.at( dispatch.m_current_read_step ) ? 1. : 0. );
                disp_etasf_expect = dispatch.outputs.eta_sf_expected.at( dispatch.m_current_read_step );
                disp_qsf_expect = dispatch.outputs.q_sfavail_expected.at( dispatch.m_current_read_step )*1.e-3;
                disp_qsfprod_expect = dispatch.outputs.q_sf_expected.at( dispatch.m_current_read_step )*1.e-3;
                disp_qsfsu_expect = dispatch.outputs.q_rec_startup.at( dispatch.m_current_read_step )*1.e-3;
                disp_tes_expect = dispatch.outputs.tes_charge_expected.at( dispatch.m_current_read_step )*1.e-3;
                disp_qpbsu_expect = dispatch.outputs.q_pb_startup.at( dispatch.m_current_read_step )*1.e-3;
                //disp_wpb_expect = dispatch.outputs.q_pb_target.at(dispatch.m_current_read_step ) * disp_etapb_expect *1.e-3;  
                disp_wpb_expect = dispatch.outputs.w_pb_target.at( dispatch.m_current_read_step )*1.e-3;
                disp_rev_expect = disp_wpb_expect * dispatch.price_signal.at( dispatch.m_current_read_step );
                disp_etapb_expect = disp_wpb_expect / max(1.e-6, dispatch.outputs.q_pb_target.at( dispatch.m_current_read_step ))* 1.e3 
                                        * ( dispatch.outputs.pb_operation.at( dispatch.m_current_read_step ) ? 1. : 0. );

                //if( is_sim_timestep_complete ) // disp_time_last != mc_kernel.mc_sim_info.ms_ts.ms_ts.m_time)
                //    dispatch.m_current_read_step++;

                if(dispatch.m_current_read_step > mc_tou.mc_dispatch_params.m_optimize_frequency * mc_tou.mc_dispatch_params.m_disp_steps_per_hour)
                    throw C_csp_exception("Counter synchronization error in dispatch optimization routine.", "dispatch");
            }
            
            disp_time_last = mc_kernel.mc_sim_info.ms_ts.m_time;
                        
        }

        /* 
        ------------ Controller/Solver iteration loop -------------
        */

		bool are_models_converged = false;
		reset_hierarchy_logic();
		// Reset operating mode tracker		
		m_op_mode_tracking.resize(0);
					
		// Check if CR startup should be solved before entering hierarchy
		double q_dot_tes_dc_t_CR_su = 0.0;
		double m_dot_tes_dc_t_CR_su = 0.0;
		if( (cr_operating_state == C_csp_collector_receiver::OFF || cr_operating_state == C_csp_collector_receiver::STARTUP) &&
			q_dot_cr_startup > 0.0 &&
			is_rec_su_allowed && 
			m_is_tes )
		{
			// Set startup conditions
			mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

			mc_collector_receiver.startup(mc_weather.ms_outputs,
				mc_cr_htf_state_in,
				mc_cr_out_solver,
				mc_kernel.mc_sim_info);

			// Check that startup happened
			// Because for all modes w/ startup, the startup occurs under the same conditions for any given timestep
			// then if startup fails here, it won't succeed downstream
			// so set is_rec_su_allowed = false
			if( mc_cr_out_solver.m_q_startup == 0.0 || mc_cr_out_solver.m_time_required_su != mc_cr_out_solver.m_time_required_su )
			{	// Collector/receiver can't produce useful energy
				
				is_rec_su_allowed = false;
			}
			else
			{
				double t_CR_su = mc_cr_out_solver.m_time_required_su;		//[s] Receiver model returns MIN(time required to completely startup, full timestep duration)

				// Use minimum of CR startup timestep and initial simulation timestep
				t_CR_su = fmin(t_CR_su, mc_kernel.mc_sim_info.ms_ts.m_step);			//[s]

				// Predict estimated amount of discharage available with new timestep
				if( m_is_tes )
				{
					double T_hot_field_dc_est;	//[kg/s, K]
					T_hot_field_dc_est = std::numeric_limits<double>::quiet_NaN();
					mc_tes.discharge_avail_est(m_T_htf_cold_des, t_CR_su, q_dot_tes_dc_t_CR_su, m_dot_tes_dc_t_CR_su, T_hot_field_dc_est);
					m_dot_tes_dc_t_CR_su *= 3600.0;		//[kg/hr] convert from kg/s
				}
				else
				{
					q_dot_tes_dc_t_CR_su = 0.0;
					m_dot_tes_dc_t_CR_su = 0.0;
				}
			} 
		}


		// Check if receiver can be defocused enough to stay under cycle+TES max thermal power and mass flow (this will usually be the case unless using clear-sky control or constrained cycle thermal input)
		if (cr_operating_state == C_csp_collector_receiver::ON && q_dot_cr_on>0.0 && is_rec_su_allowed && m_is_tes)
		{
			double qpcmax = m_q_dot_pc_max;
			if (pc_operating_state == C_csp_power_cycle::OFF || C_csp_power_cycle::STARTUP)
				qpcmax = q_dot_pc_su_max;

			double qmax = (m_q_dot_pc_max + q_dot_tes_ch) / (1.0 - tol_mode_switching);
			double mmax = (m_m_dot_pc_max + m_dot_tes_ch_est) / (1.0 - tol_mode_switching);
			if (q_dot_cr_on > qmax || m_dot_cr_on > mmax)
			{
				double df = fmin(qmax / q_dot_cr_on, mmax / m_dot_cr_on);
				mc_collector_receiver.on(mc_weather.ms_outputs, mc_cr_htf_state_in, df, mc_cr_out_solver, mc_kernel.mc_sim_info);
				if (mc_cr_out_solver.m_q_thermal == 0.0)  // Receiver solution wasn't successful 
					is_rec_su_allowed = false;
			}

		}


		while(!are_models_converged)		// Solve for correct operating mode and performance in following loop:
		{
			// Reset timestep info for iterations on the operating mode...
			mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.get_baseline_end_time();
			mc_kernel.mc_sim_info.ms_ts.m_step = mc_kernel.mc_sim_info.ms_ts.m_time - mc_kernel.mc_sim_info.ms_ts.m_time_start;

			if( (cr_operating_state == C_csp_collector_receiver::OFF || cr_operating_state == C_csp_collector_receiver::STARTUP)
				&& (pc_operating_state == C_csp_power_cycle::OFF || pc_operating_state == C_csp_power_cycle::STARTUP) )
			{	// At start of this timestep, are power cycle AND collector/receiver off?

				if( q_dot_cr_startup > 0.0 && is_rec_su_allowed &&
					m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail )
				{	// Receiver startup is allowed and possible (will generate net energy)

					if( q_dot_tes_dc > 0.0 && is_pc_su_allowed &&
						m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail )
					{
						operating_mode = CR_SU__PC_SU__TES_DC__AUX_OFF;
					}
					else
					{
						operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
				else
				{
					if( q_dot_tes_dc > 0.0 && is_pc_su_allowed &&
						m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail )
					{
						operating_mode = CR_OFF__PC_SU__TES_DC__AUX_OFF;
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
			}	// End logic for CR_state == OFF or STARTUP    AND     PC_state == OFF or STARTUP

			else if( cr_operating_state == C_csp_collector_receiver::ON &&
				(pc_operating_state == C_csp_power_cycle::OFF || pc_operating_state == C_csp_power_cycle::STARTUP) )
			{
				if( q_dot_cr_on > 0.0 && is_rec_su_allowed )
				{	// Receiver is allowed to remain on, and it can produce useful energy. Now, need to find a home for it

					if( is_pc_su_allowed &&
						m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail )	// Can receiver output go to power cycle?
					{
						if( q_dot_tes_ch > 0.0 )
						{
							if( ( (q_dot_cr_on - q_dot_tes_ch)*(1.0+tol_mode_switching) > q_dot_pc_su_max 
								|| (m_dot_cr_on - m_dot_tes_ch_est)*(1.0+tol_mode_switching) > m_m_dot_pc_max ) && 
								m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail )
							{
								operating_mode = CR_DF__PC_SU__TES_FULL__AUX_OFF;								
							}
							else if( ( q_dot_cr_on*(1.0+tol_mode_switching) > q_dot_pc_su_max 
								|| m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_max ) &&
								m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail )
							{
								operating_mode = CR_ON__PC_SU__TES_CH__AUX_OFF;								
							}
							else
							{
								operating_mode = CR_ON__PC_SU__TES_OFF__AUX_OFF;
							}
						}
						else
						{
							if( (q_dot_cr_on*(1.0+tol_mode_switching) > q_dot_pc_su_max ||
								m_dot_cr_on*(1.0+tol_mode_switching) > m_m_dot_pc_max)
								&& m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail )
							{
								operating_mode = CR_DF__PC_SU__TES_OFF__AUX_OFF;								
							}
							else
							{
								operating_mode = CR_ON__PC_SU__TES_OFF__AUX_OFF;
							}							
						}						
					}
					else if( q_dot_tes_ch > 0.0 )
					{
						if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
							m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
						{
							operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
						}
						else if(m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail)
						{
							operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF;														
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
				else if( q_dot_tes_dc > 0.0 && is_pc_su_allowed &&
					m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail )
				{	// Can power cycle startup using TES?

					operating_mode = CR_OFF__PC_SU__TES_DC__AUX_OFF;
				}
				else
				{
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}
			}

			else if( (cr_operating_state == C_csp_collector_receiver::OFF || cr_operating_state == C_csp_collector_receiver::STARTUP) &&
				(pc_operating_state == C_csp_power_cycle::ON || pc_operating_state == C_csp_power_cycle::STANDBY) )
			{
				if( q_dot_cr_startup > 0.0 && is_rec_su_allowed )
				{	// Receiver startup is allowed and possible (will generate net energy) - determine if power cycle can remain on

					if( is_pc_su_allowed || is_pc_sb_allowed )
					{					
						if( ( (q_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > q_pc_target
							&& m_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > m_m_dot_pc_min)
							|| m_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > m_m_dot_pc_max )
							&& is_pc_su_allowed &&
							m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to matching target, the controller tries that mode

							operating_mode = CR_SU__PC_TARGET__TES_DC__AUX_OFF;
						}
						else if( q_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > q_pc_min 
							&& m_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > m_m_dot_pc_min
							&& is_pc_su_allowed &&
							m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

							operating_mode = CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF;
						}
						else if( q_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > q_pc_sb 
							&& m_dot_tes_dc_t_CR_su*(1.0 + tol_mode_switching) > m_m_dot_pc_min
							&& is_pc_sb_allowed &&
							m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

							operating_mode = CR_SU__PC_SB__TES_DC__AUX_OFF;
						}
						else if( q_dot_tes_dc_t_CR_su > 0.0 && is_pc_su_allowed &&
							m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail )
						{
							operating_mode = CR_SU__PC_MIN__TES_EMPTY__AUX_OFF;														
						}
						else if( m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail )
						{
							operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}	// End 'is_pc_su_allowed' logic
					else
					{	// power cycle startup/operation not allowed
						
						if( m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail )
						{
							operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}
				}
				else	// Receiver remains OFF - determine if power cycle can remain on
				{
					if( is_pc_su_allowed || is_pc_sb_allowed )
					{
					
						if( ( (q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_target
							&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min)
							|| m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_max )
							&& is_pc_su_allowed &&
							m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to matching target, the controller tries that mode

							operating_mode = CR_OFF__PC_TARGET__TES_DC__AUX_OFF;
						}
						else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_min 
								&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min
								&& is_pc_su_allowed &&
								m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

							operating_mode = CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF;						
						}
						else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_sb
								&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min
								&& is_pc_sb_allowed && 
								m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail )
						{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

							operating_mode = CR_OFF__PC_SB__TES_DC__AUX_OFF;						
						}
						else if( q_dot_tes_dc > 0.0 && is_pc_su_allowed &&
								m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail )
						{
							operating_mode = CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF;												
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}	// end logic on 'is_pc_su_allowed'
					else
					{

						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
			}

			else if( cr_operating_state == C_csp_collector_receiver::ON &&
				(pc_operating_state == C_csp_power_cycle::ON || pc_operating_state == C_csp_power_cycle::STANDBY) )
			{
				if( q_dot_cr_on > 0.0 && is_rec_su_allowed )
				{	// Receiver operation is allowed and possible - find a home for output

					if( is_pc_su_allowed || is_pc_sb_allowed )
					{
						if( (q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_target || m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_max) && 
							is_pc_su_allowed &&
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE && m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE )
						{	// The power cycle cannot accept the entire receiver output
							// Tolerance is applied so that if CR is *close* to reaching the PC target, the controller tries modes that fill TES

							// Can storage be charged?
							if( q_dot_tes_ch > 0.0 )
							{
								// 1) Try to fill storage while hitting power cycle target
								if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < q_pc_target 
									&& (m_dot_cr_on - m_dot_tes_ch_est)*(1.0 - tol_mode_switching) < m_m_dot_pc_max &&
									m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE )
								{	// Storage can accept the remaining receiver output
									// Tolerance is applied so that if CR + TES is *close* to reaching PC target, the controller tries that mode

									operating_mode = CR_ON__PC_TARGET__TES_CH__AUX_OFF;
								}

								// 2) Try operating power cycle at maximum capacity
								// Assume we want to completely fill storage, so the power cycle operation should float to meet that condition
								else if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < m_q_dot_pc_max
									&& (m_dot_cr_on - m_dot_tes_ch_est)*(1.0 - tol_mode_switching) < m_m_dot_pc_max &&
									m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail )
								{	// Storage and the power cycle operating between target and max can accept the remaining receiver output
									// Tolerance is applied so that if CR + TES is *close* to reaching PC  max, the controller tries that mode

									operating_mode = CR_ON__PC_RM_HI__TES_FULL__AUX_OFF;
								}

								// 3) Try defocusing the CR and operating the power cycle at maximum capacity
								else if( m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail )
								{
									
									operating_mode = CR_DF__PC_MAX__TES_FULL__AUX_OFF;
								}
								else
								{
									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;;
								}
							}	// End if(q_dot_tes_ch > 0.0) logic

							else
							{	// No storage available for dispatch

								// 1) Try operating power cycle at maximum capacity
								if( (q_dot_cr_on*(1.0 - tol_mode_switching) < m_q_dot_pc_max && m_dot_cr_on*(1.0 - tol_mode_switching)) &&
									m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE )
								{	// Tolerance is applied so that if CR + TES is *close* to reaching PC  max, the controller tries that mode

									operating_mode = CR_ON__PC_RM_HI__TES_OFF__AUX_OFF;
								}
								else if( m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail )
								{
									operating_mode = CR_DF__PC_MAX__TES_OFF__AUX_OFF;
								}
								else
								{
									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								}
							}	// End else 'no storage available for dispatch'
						}
						else
						{	// Power cycle is asking for more output than the receiver can supply

							if( q_dot_tes_dc > 0.0 )
							{	// Storage dispatch is available

								if( ( ( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_target 
									&& (m_dot_cr_on + m_dot_tes_dc_est)*(1.0 + tol_mode_switching) > m_m_dot_pc_min  )
									|| (m_dot_cr_on + m_dot_tes_dc_est)*(1.0 + tol_mode_switching) > m_m_dot_pc_max )
									&& is_pc_su_allowed &&
									m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail )
								{	// Storage can provide enough dispatch to reach power cycle target
									// Tolerance is applied so that if CR + TES is *close* to reaching PC target, the controller tries that mode

									operating_mode = CR_ON__PC_TARGET__TES_DC__AUX_OFF;									
								}
								else if( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_min 
									&& is_pc_su_allowed 
									&& (m_dot_cr_on + m_dot_tes_dc_est)*(1.0 + tol_mode_switching) > m_m_dot_pc_min &&
									m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail )
								{	// Storage can provide enough dispatch to at least meet power cycle minimum operation fraction
									// Run at highest possible PC fraction by dispatch all remaining storage
									// Tolerance is applied so that if CR + TES is *close* to reaching PC min, the controller tries that mode

									operating_mode = CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF;									
								}
								else if( q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_sb 
									&& m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_min
									&& is_pc_sb_allowed &&
									m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail && m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail )
								{
									if( q_dot_tes_ch > 0.0 )
									{
										if( ( (q_dot_cr_on - q_dot_tes_ch)*(1.0 + tol_mode_switching) > q_pc_sb 
											|| (m_dot_cr_on - m_dot_tes_ch_est)*(1.0 + tol_mode_switching) > m_m_dot_pc_min ) &&
											m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail )
										{	// Tolerance is applied so that if CR output is *close* to operating at standby AND completely filling storage, controller tries that mode

											operating_mode = CR_ON__PC_SB__TES_FULL__AUX_OFF;
										}
										else
										{
											operating_mode = CR_ON__PC_SB__TES_CH__AUX_OFF;
										}
									}
									else
									{
										// This could *technically* use defocus, but can argue the energy is just being thrown away in power cycle anyway
										operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
									}
								}
								else if( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_sb 
									&& (m_dot_cr_on + m_dot_tes_dc_est)*(1.0 + tol_mode_switching) > m_m_dot_pc_min
									&& is_pc_sb_allowed &&
									m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail )
								{
									operating_mode = CR_ON__PC_SB__TES_DC__AUX_OFF;
								}
								else if( is_pc_su_allowed &&
										m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail )
								{
									operating_mode = CR_ON__PC_MIN__TES_EMPTY__AUX_OFF;
								}
								else if (q_dot_tes_ch > 0.0)
								{
									if (q_dot_cr_on * (1.0 - tol_mode_switching) < q_dot_tes_ch &&
										m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail)
									{	// Tolerance is applied so that if CR is *close* to being less than a full TES charge, the controller tries normal operation (no defocus)


										operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
									}
									else if (m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail)
									{	// The CR output will overcharge storage, so it needs to defocus.
										// However, because the CR output is already part-load, it may be close to shutting down before defocus...

										operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF;
									}
									else
									{
										operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
									}
								}
								else
								{	// No home for receiver output, and not enough thermal power for power cycle

									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								}
							}
							else
							{	// Storage dispatch is not available

								// Can the power cycle operate at or above the minimum operation fraction?
								if( ( (q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_min 
									&& m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_min)
									|| m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_max)
									&& is_pc_su_allowed &&
									m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail )
								{	// Tolerance is applied so that if CR is *close* to reaching PC min, the controller tries that mode

									operating_mode = CR_ON__PC_RM_LO__TES_OFF__AUX_OFF;
								}
								else if( is_pc_sb_allowed 
									&& q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_sb 
									&& m_dot_cr_on*(1.0 + tol_mode_switching) > m_m_dot_pc_min &&
									m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail && m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail )
								{	// Receiver can likely operate in standby
									// Tolerance is applied so that if CR is *close* to reaching PC standby, the controller tries that mode

									if( q_dot_tes_ch > 0.0 )
									{
										if( ( (q_dot_cr_on - q_dot_tes_ch)*(1.0+tol_mode_switching) > q_pc_sb
											|| (m_dot_cr_on - m_dot_tes_ch_est)*(1.0+tol_mode_switching) > m_m_dot_pc_min ) &&
											m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail )
										{	// Tolerance is applied so that if CR output is *close* to operating at standby AND completely filling storage, controller tries that mode

											operating_mode = CR_ON__PC_SB__TES_FULL__AUX_OFF;
										}
										else
										{
											operating_mode = CR_ON__PC_SB__TES_CH__AUX_OFF;
										}
									}
									else
									{
										// This could *technically* use defocus, but can argue the energy is just being thrown away in power cycle anyway
										operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
									}
								}
								else if( q_dot_tes_ch > 0.0 )
								{	// Charge storage with receiver output

									if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
										m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
									{	// Tolerance is applied so that if CR is *close* to being less than a full TES charge, the controller tries normal operation (no defocus)


										operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
									}
									else if( m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail )
									{	// The CR output will overcharge storage, so it needs to defocus.
										// However, because the CR output is already part-load, it may be close to shutting down before defocus...

										operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF;
									}
									else
									{
										operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
									}
								}
								else
								{	// No home for receiver output, and not enough thermal power for power cycle

									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								}
							}	// End logic else 'storage dispatch not available'
						}	// End logic else 'power cycle requires more q_dot than receiver can supply'				
					}	// End logic if(is_rec_su_allowed)
					else
					{	// Power cycle startup is not allowed - see if receiver output can go to storage

						if( q_dot_tes_ch > 0.0 )
						{
							if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
								m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
							{
								operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
							}
							else if( m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail )
							{
								operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF;
							}
							else
							{
								operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
							}
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}

					}	// End logic else 'pc su is NOT allowed'		
				}	// End logic if(q_dot_cr_output > 0.0 && is_rec_su_allowed)

				else	// Receiver is off - determine if power cycle can remain on
				{
					if( is_pc_su_allowed || is_pc_sb_allowed )
					{
						if( q_dot_tes_dc > 0.0 )
						{	// Storage dispatch is available

							if( ( (q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_target
								&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min)
								|| m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_max )
								&& is_pc_su_allowed &&
								m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail )
							{	// Storage can provide enough dispatch to reach power cycle target
								// Tolerance is applied so that if TES is *close* to reaching PC target, the controller tries that mode

								operating_mode = CR_OFF__PC_TARGET__TES_DC__AUX_OFF;
							}
							else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_min 
									&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min
									&& is_pc_su_allowed &&
									m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail )
							{	// Storage can provide enough dispatch to at least meet power cycle minimum operation fraction
								// Run at highest possible PC fraction by dispatching all remaining storage
								// Tolerance is applied so that if CR + TES is *close* to reaching PC min, the controller tries that mode

								operating_mode = CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF;
							}
							else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_sb 
									&& m_dot_tes_dc_est*(1.0 + tol_mode_switching) > m_m_dot_pc_min
									&& is_pc_sb_allowed &&
									m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail )
							{	// Tolerance is applied so that if CR + TES is *close* to reaching standby, the controller tries that mode
								
								operating_mode = CR_OFF__PC_SB__TES_DC__AUX_OFF;
							}
							else if( is_pc_su_allowed && 
									m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail )
							{	// If not enough thermal power to stay in standby, then run at min PC load until TES is fully discharged

								operating_mode = CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF;
							}
							else
							{
								operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
							}
						}	// End logic for if( q_dot_tes_dc > 0.0 )
						else
						{	// Storage dispatch is not available

							// No thermal power available to power cycle
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}	// End logic if( is_pc_su_allowed )
					else
					{	// If neither receiver nor power cycle operation is allowed, then shut everything off

						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}	// End logic for else 'receiver not on'

			}
			// End operating state mode for CR ON, PC ON/STANDBY
			

			// Store operating mode
			m_op_mode_tracking.push_back(operating_mode);
            operating_mode_str = tech_operating_modes_str[operating_mode];

            op_mode_str = "";
            
            switch( operating_mode )
			{
            case CR_DF__PC_MAX__TES_OFF__AUX_OFF:
            {
                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__TO_PC__PC_MAX;
                C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = true;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_DF__PC_MAX__TES_OFF__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Set member defocus
                m_defocus = defocus_solved;

                are_models_converged = true;
            }
            break;
            
            case CR_DF__PC_SU__TES_OFF__AUX_OFF:
			{
				// The PC is operating at its maximum operating thermal power or HTF mass flow rate
				// TES cannot be charged
				// Defocus CR to hit PC constraints

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

				if (!mc_collector_receiver.m_is_sensible_htf)
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__TO_PC__ITER_M_DOT_SU;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = true;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_DF__PC_SU__TES_OFF__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Set member defocus
                m_defocus = defocus_solved;

                are_models_converged = true;

                break;
			}

            case CR_ON__PC_RM_LO__TES_OFF__AUX_OFF:
            {
                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_ON__PC_RM_LO__TES_OFF__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if (mc_cr_out_solver.m_q_thermal < q_pc_min)
                {
                    m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
            break;

			case CR_ON__PC_RM_HI__TES_OFF__AUX_OFF:
            {

                // Collector/Receiver in ON, and only place for HTF to go is power cycle.
                // Therefore, power cycle must operate at Resource Match and use w/e is provided
                // (in cases with storage or field defocus, power cycle will try to hit an exact thermal input)
                // 'Failure Modes'
                // 1) Receiver provides too much power
                //		* Go to defocus
                // 2) Receiver cannot maintain minimum operation fraction
                //		* Go to power cycle standby or shutdown

                // Set Solved Controller Variables Here (that won't be reset in this operating mode)

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_ON__PC_RM_HI__TES_OFF__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
					m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = false;
                    are_models_converged = false;
                    break;
                }

                if (mc_cr_out_solver.m_q_thermal > m_q_dot_pc_max || mc_cr_out_solver.m_m_dot_salt_tot > m_m_dot_pc_max)
                {
                    m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = false;
                    are_models_converged = false;
                    break;
                }
                else if (mc_cr_out_solver.m_q_thermal < q_pc_target)
                {
                    m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }	// end case{} to allow compilation with local (w/r/t case) variables

            break;


            case CR_ON__PC_SB__TES_OFF__AUX_OFF:
            {

                // Collector/receiver is ON
                // Power cycle is running in standby
                // During standby, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
                // so shouldn't need to iterate between CR and PC
                // Assume power cycle can remain in standby the entirety of the timestep

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();

                op_mode_str = "CR_ON__PC_SB__TES_OFF__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Check that cr and pc mass flow rates balance
                if (fabs(mc_cr_out_solver.m_m_dot_salt_tot - mc_pc_out_solver.m_m_dot_htf) / m_m_dot_pc_des > 1.E-4)
                {
                    m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is greater than target
                if ((mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) > 1.E-3)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a PC thermal power %lg [MWt]"
                        " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                        " larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is less than target
                if ((mc_pc_out_solver.m_q_dot_htf - q_pc_sb) / q_pc_sb < -1.E-3)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a PC thermal power %lg [MWt]"
                        " less than the minimum PC thermal power %lg [MWt].",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_min);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if (mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                        " less than the minimum PC HTF mass flow rate %lg [kg/s].",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
				break;


			case CR_ON__PC_SU__TES_OFF__AUX_OFF:
            {
                // Collector/receiver is ON
                // Startup power cycle
                // Reseting timestep during iteration, so need to be careful at this level before returning to controller

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                double q_dot_pc_target = std::numeric_limits<double>::quiet_NaN();
                op_mode_str = "CR_ON__PC_SU__TES_OFF__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_target, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Compare q_dot_to_pc to q_dot_pc_su_max
                if (mc_cr_out_solver.m_q_thermal > q_dot_pc_su_max || mc_cr_out_solver.m_m_dot_salt_tot > m_m_dot_pc_max_startup)
                {
                    if (mc_cr_out_solver.m_q_thermal > q_dot_pc_su_max)
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_SU__TES_OFF__AUX_OFF method converged to a power cycle"
                            " thermal input, %lg [MWt], greater than the target %lg [MWt].",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_cr_out_solver.m_q_thermal, q_dot_pc_su_max);
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_SU__TES_OFF__AUX_OFF method converged to a power cycle"
                            " mass flow rate input, %lg [kg/s], greater than the maximum allowable %lg [kg/s].",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_cr_out_solver.m_m_dot_salt_tot / 3600.0, m_m_dot_pc_max_startup / 3600.0);
                    }
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }

				break;

			case CR_SU__PC_OFF__TES_OFF__AUX_OFF:
            {
                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]
                
                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::OFF;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__0;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                op_mode_str = "CR_SU__PC_OFF__TES_OFF__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                are_models_converged = true;
            }
				break;

			case CR_OFF__PC_OFF__TES_OFF__AUX_OFF:
            {
                int cr_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::OFF;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__0;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                op_mode_str = "CR_OFF__PC_OFF__TES_OFF__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    throw(C_csp_exception(util::format("At time = %lg, CR_OFF__PC_OFF__TES_OFF__AUX_OFF failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
				break;		// exit switch() after CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

			case CR_OFF__PC_SU__TES_DC__AUX_OFF:
			{
				// Use thermal storage to startup power cycle
				// This solver iterates to find the thermal storage outlet temperature to the power cycle
				//    and the power cycle demand mass flow rate that reach system equilibrium

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                op_mode_str = "CR_OFF__PC_SU__TES_DC__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
			}
				break;

			case CR_ON__PC_OFF__TES_CH__AUX_OFF:
            {
                // Method to solve operating mode where the CR is on (under some fixed operating conditions, i.e. defocus)
                // and charging TES. No PC operating or AUX, so the output of the CR connects directly to TES

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::OFF;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__0;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                op_mode_str = "CR_ON__PC_OFF__TES_CH__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
				break;

			case CR_ON__PC_TARGET__TES_CH__AUX_OFF:
            {
                // CR is on (no defocus)
                // PC is on and hitting specified target
                // TES is charging

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                double q_dot_pc_fixed = q_pc_target;
                op_mode_str = "CR_ON__PC_TARGET__TES_CH__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;		//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;		//[kg/hr]

                if (fabs(q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < 1.E-3)
                {	// If successfully solved for target thermal power, check that mass flow is above minimum
                    if ((m_dot_pc_solved - m_m_dot_pc_min) / fmax(0.01, m_m_dot_pc_min) < -1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s solved with a PC HTF mass flow rate %lg [kg/s]"
                            " smaller than the minimum %lg [kg/s]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    if ((m_dot_pc_solved - m_m_dot_pc_max) / m_m_dot_pc_max < -1.E-3)
                    {
                        if (operating_mode == CR_ON__PC_TARGET__TES_CH__AUX_OFF)
                        {
                            // Can send more mass flow to PC from TES
                            m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = false;
                            are_models_converged = false;
                            break;
                        }
                    }
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
            break;

			case CR_ON__PC_SB__TES_CH__AUX_OFF:
			{
				// CR is on (no defocus)
				// PC is on and hitting specified target
				// TES is charging

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_sb;
                
                op_mode_str = "CR_ON__PC_SB__TES_CH__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;		//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;		//[kg/hr]

                if (fabs(q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < 1.E-3)
                {	// If successfully solved for target thermal power, check that mass flow is above minimum
                    if ((m_dot_pc_solved - m_m_dot_pc_min) / fmax(0.01, m_m_dot_pc_min) < -1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s solved with a PC HTF mass flow rate %lg [kg/s]"
                            " smaller than the minimum %lg [kg/s]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;

			}
				break;

			case CR_ON__PC_TARGET__TES_DC__AUX_OFF:
			{
				// The collector receiver is on and returning hot HTF to the PC
				// TES is discharging hot HTF that is mixed with the CR HTF
				// to operating the PC at its target value

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                double q_dot_pc_fixed = q_pc_target;
                op_mode_str = "CR_ON__PC_TARGET__TES_DC__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check bounds on solved thermal power and mass flow rate
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
                            " greater than the maximum %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
                            " greater than the target %lg [MWt], but less than the maximum %lg [MWt].",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_dot_pc_fixed, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                if (m_dot_pc_solved < m_m_dot_pc_min)
                {	// If we're already hitting the minimum mass flow rate, then trying next operating mode won't help
                    error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC HTF mass flow rate %lg [kg/s]"
                        " less than the minimum %lg [kg/s]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3
                    && (m_dot_pc_solved - m_m_dot_pc_max) / m_m_dot_pc_max < -1.E-3)
                {
                    m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if (m_dot_pc_solved > m_m_dot_pc_max)
                {	// Shouldn't happen but can try next operating mode
                    m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
			}	// end outer bracket for case CR_ON__PC_OFF__TES_CH__AUX_OFF
				
				break;	// break case CR_ON__PC_OFF__TES_CH__AUX_OFF

			case CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF:
			{
				// The collector receiver is on and return hot HTF to the Pc
				// TES is discharging hot HTF that is then mixed with the CR HTF
				// The power cycle operates between its minimum operation fraction and target operation

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                op_mode_str = "CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// *********************************
				// Check if solved thermal power is greater than target
				if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_target) / q_pc_target > 1.E-3 )
				{
					if ( (mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3 )
					{
						error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
							" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

						mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

						turn_off_plant();
                        reset_time(t_ts_initial);
						are_models_converged = false;
						break;
					}
					else
					{
						error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
							" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, m_q_dot_pc_max);
						
						mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
					}
				}

				if ( (mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_max) / m_m_dot_pc_max > 1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					turn_off_plant();
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// *********************************
				// Check PC q_dot is >= MIN!!!!!!!!

				if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_min) / q_pc_min < -1.E-3 )
				{
					m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}
				if ( (mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_min) / m_m_dot_pc_min < -1.E-3 )
				{
					m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// Set member defocus
				m_defocus = defocus_solved;

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;

			}
				break;	// break case CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF
			

			case CR_DF__PC_OFF__TES_FULL__AUX_OFF:
			{
				// Running the CR at full power results in too much thermal power to TES
				// Power cycle operation is either not allowed or not possible under the timestep conditions

				// Assuming here that partial defocus is allowed, so should always be able to reach full power to PC
				
				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::OFF;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__TES_FULL__0;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = true;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_DF__PC_OFF__TES_FULL__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Set member defocus
                m_defocus = defocus_solved;

                are_models_converged = true;
			}
				break;	// break case CR_DF__PC_OFF__TES_FULL__AUX_OFF

			case CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF:
            {
                //The collector receiver is off
                //The power cycle runs at its minimum operating fraction until storage is depleted
                //A new, shorter timestep is calculated here

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_Q_DOT_PC;
                bool is_defocus = false;
                double q_dot_pc_target = q_pc_min;      //[MWt]
                op_mode_str = "CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_target, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is greater than target
                if (mc_pc_out_solver.m_q_dot_htf > q_pc_target)
                {
                    if (mc_pc_out_solver.m_q_dot_htf > m_q_dot_pc_max)
                    {
                        error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
                            " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        reset_time(t_ts_initial);
                        are_models_converged = false;
                        break;
                    }
                }

                if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
                {
                    error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                        " larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    reset_time(t_ts_initial);
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
				break;	// break case CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF


			case CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF:
            {
                // The collector-receiver is off
                // The power cycle runs somewhere between its minimum operating fraction and target operation, with thermal input from TES, which is depleted at the end of the timestep

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;
                op_mode_str = "CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if (mc_pc_out_solver.m_q_dot_htf < q_pc_min || mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
                {
                    m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is greater than target
                if (mc_pc_out_solver.m_q_dot_htf > q_pc_target)
                {
                    if (mc_pc_out_solver.m_q_dot_htf > m_q_dot_pc_max)
                    {
                        error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
                                " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                                mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
                                " larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
                                mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }

                if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
                {
                    error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                            " larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
            break;

			case CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF:
			{
				// The collector-receiver is off
				// The power cycle runs somewhere between its minimum operating fraction and target operation, with thermal input from TES, which is depleted at the end of the timestep

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                double q_dot_pc_target = std::numeric_limits<double>::quiet_NaN();      //[MWt]
                op_mode_str = "CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_target, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				if (mc_pc_out_solver.m_q_dot_htf < q_pc_min || mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
				{
					m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is greater than target
				if (mc_pc_out_solver.m_q_dot_htf > q_pc_target)
				{
					if (mc_pc_out_solver.m_q_dot_htf > m_q_dot_pc_max)
					{
						if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);
						}
						else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);
						}

						mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

						turn_off_plant();
                        reset_time(t_ts_initial);
						are_models_converged = false;
						break;
					}
					else
					{
						if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, m_q_dot_pc_max);
						}
						else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, m_q_dot_pc_max);
						}
						mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
					}
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
					{
						error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
							" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf/3600.0, m_m_dot_pc_max/3600.0);
					}
					else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
					{
						error_msg = util::format("At time = %lg CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
							" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf/3600.0, m_m_dot_pc_max/3600.0);
					}

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					turn_off_plant();
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// Set member defocus
				m_defocus = defocus_solved;

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;
			}
				break;	// break case CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF

			case CR_SU__PC_MIN__TES_EMPTY__AUX_OFF:
			{
				// The collector-receiver is in startup
				// The power cycle runs at its minimum fraction until storage is depleted
				// A new, shorter timestep is calculated here

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_Q_DOT_PC;
                bool is_defocus = false;
                double q_dot_pc_target = q_pc_min;      //[MWt]
                op_mode_str = "CR_SU__PC_MIN__TES_EMPTY__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_target, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Check if solved thermal power is greater than target
				if ( (mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) > 1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					turn_off_plant();
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					turn_off_plant();
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is less than target
				if ( (mc_pc_out_solver.m_q_dot_htf-q_pc_min) / q_pc_min < -1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" less than the minimum PC thermal power %lg [MWt].",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_min);

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" less than the minimum PC HTF mass flow rate %lg [kg/s].",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

					m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
                    reset_time(t_ts_initial);
					are_models_converged = false;
					break;
				}

				// Set member defocus
				m_defocus = defocus_solved;

				are_models_converged = true;
			}
				break;


			case CR_ON__PC_SB__TES_DC__AUX_OFF:
			{
				// The collector receiver is on and returning hot HTF to the PC
				// TES is discharging hot HTF that is mixed with the CR HTF
				// to operate the PC at standby

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_sb;        //[MWt]

                op_mode_str = "CR_ON__PC_SB__TES_DC__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check bounds on solved thermal power and mass flow rate
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
                            " greater than the maximum %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
                            " greater than the target %lg [MWt]",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_dot_pc_fixed);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                if (m_dot_pc_solved < m_m_dot_pc_min)
                {	// If we're already hitting the minimum mass flow rate, then trying next operating mode won't help
                    error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC HTF mass flow rate %lg [kg/s]"
                        " less than the minimum %lg [kg/s]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if (m_dot_pc_solved > m_m_dot_pc_max)
                {	// Shouldn't happen but can try next operating mode
                    m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                // If convergence was successful, finalize this timestep and get out
                // Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
                are_models_converged = true;

            }	// end 'CR_ON__PC_SB__TES_DC__AUX_OFF'
				
				break;


			case CR_OFF__PC_TARGET__TES_DC__AUX_OFF:
            {
                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }
                
                int cr_mode = C_csp_collector_receiver::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_target;        //[MWt]
                op_mode_str = "CR_OFF__PC_TARGET__TES_DC__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check if solved thermal power is greater than target
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, q_dot_pc_fixed, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    if (m_dot_pc_solved < m_m_dot_pc_max)
                    {	// TES cannot provide enough thermal power - step down to next operating mode
                        
                        m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                        are_models_converged = false;
                        break;
                    }
                    // Notes:
                    //else
                    //{	// PC maximum mass flow is constraining the thermal power that TES can send the PC. Changing modes wont' help
                    //
                    //}
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
            }
            break;

			case CR_SU__PC_TARGET__TES_DC__AUX_OFF:
            {
                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }
                
                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]
                
                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_target;        //[MWt]
                op_mode_str = "CR_SU__PC_TARGET__TES_DC__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check if solved thermal power is greater than target
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        reset_time(t_ts_initial);
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, q_dot_pc_fixed, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    if (m_dot_pc_solved < m_m_dot_pc_max)
                    {	// TES cannot provide enough thermal power - step down to next operating mode
                        m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;

                        reset_time(t_ts_initial);
                        are_models_converged = false;
                        break;
                    }
                    // Notes:
                    //else
                    //{	// PC maximum mass flow is constraining the thermal power that TES can send the PC. Changing modes wont' help
                    //
                    //}
                }

				// Set member defocus
				m_defocus = defocus_solved;

                // If convergence was successful, finalize this timestep and get out
                // Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
                are_models_converged = true;
            }
            break;

			case CR_OFF__PC_SB__TES_DC__AUX_OFF:
            {
                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::OFF;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_sb;        //[MWt]
                
                op_mode_str = "CR_OFF__PC_SB__TES_DC__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check if solved thermal power is greater than target
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, q_dot_pc_fixed, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    if (m_dot_pc_solved < m_m_dot_pc_max)
                    {	// TES cannot provide enough thermal power - step down to next operating mode
                        m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = false;

                        are_models_converged = false;
                        break;
                    }
                    // Notes:
                    //else
                    //{	// PC maximum mass flow is constraining the thermal power that TES can send the PC. Changing modes wont' help
                    //
                    //}
                }

				// Set member defocus
				m_defocus = defocus_solved;

                // If convergence was successful, finalize this timestep and get out
                // Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
                are_models_converged = true;
            }
                break;

			case CR_SU__PC_SB__TES_DC__AUX_OFF:
			{
				// The collector receiver is off
				// The power cycle run at the target thermal input level
				// The TES supplies the thermal power to the power cycle
				
				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;

                double q_dot_pc_fixed = q_pc_sb;        //[MWt]

                op_mode_str = "CR_SU__PC_SB__TES_DC__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
                double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

                // Check if solved thermal power is greater than target
                if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
                {
                    if ((q_dot_pc_solved - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, m_q_dot_pc_max);
                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                        turn_off_plant();
                        reset_time(t_ts_initial);
                        are_models_converged = false;
                        break;
                    }
                    else
                    {
                        error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
                            " larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
                            mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_pc_solved, q_dot_pc_fixed, m_q_dot_pc_max);

                        mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
                    }
                }
                else if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
                {
                    if (m_dot_pc_solved < m_m_dot_pc_max)
                    {	// TES cannot provide enough thermal power - step down to next operating mode
                        m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;

                        reset_time(t_ts_initial);
                        are_models_converged = false;
                        break;
                    }
                    // Notes:
                    //else
                    //{	// PC maximum mass flow is constraining the thermal power that TES can send the PC. Changing modes wont' help
                    //
                    //}
                }

				// Set member defocus
				m_defocus = defocus_solved;

                // If convergence was successful, finalize this timestep and get out
                // Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
                are_models_converged = true;

			}	// end 'CR_OFF__PC_TARGET__TES_DC__AUX_OFF'
				break;


			case CR_ON__PC_RM_HI__TES_FULL__AUX_OFF:
			{
				// The collector receiver is on and delivering hot HTF to the TES and PC
				// The PC is operating between its target and maximum thermal power
				// The TES is fully charging over the timestep

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_LESS_TES_FULL;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_ON__PC_RM_HI__TES_FULL__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				if ((mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
				{
					m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}
				else if( mc_pc_out_solver.m_q_dot_htf < q_pc_target )
				{
					error_msg = util::format("At time = %lg %s method converged to a power cycle"
						" thermal input less than the target.",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
				}

				// Set member defocus
				m_defocus = defocus_solved;

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;

			}	// end 'CR_ON__PC_RM_HI__TES_FULL__AUX_OFF
				break;

			case CR_ON__PC_MIN__TES_EMPTY__AUX_OFF:
			{
				// The collector-receiver is on and returning hot HTF to the PC
				// The PC is operating at its minimum fraction
				// The CR + TES output over the initial controller timestep is less than the PC min fraction
				// so the controller calculates a new timestep such that the TES completely discharges and 
				//  ... the CR + TES output is = the PC min fraction

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                // Get initial timestep
                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_PLUS_TES_EMPTY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_Q_DOT_PC;
                bool is_defocus = false;
                
                double q_dot_pc_target = q_pc_min;      //[MWt]
                
                op_mode_str = "CR_ON__PC_MIN__TES_EMPTY__AUX_OFF";
                
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_target, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is greater than target
                if ((mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) > 1.E-3)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
                        " larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, m_q_dot_pc_max);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    reset_time(t_ts_initial);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                        " larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    reset_time(t_ts_initial);

                    turn_off_plant();
                    are_models_converged = false;
                    break;
                }

                // Check if solved thermal power is less than target
                if ((mc_pc_out_solver.m_q_dot_htf - q_pc_min) / q_pc_min < -1.E-3)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
                        " less than the minimum PC thermal power %lg [MWt]. Controller moved to next operating mode.",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_min);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;

                    reset_time(t_ts_initial);

                    are_models_converged = false;
                    break;
                }

                if ((mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_min) / m_m_dot_pc_min < -1.E-4)
                {
                    error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
                        " less than the minimum PC HTF mass flow rate %lg [kg/s]. Controller moved to next operating mode.",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;

                    reset_time(t_ts_initial);

                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;

                break;

			}	// end 'CR_ON__PC_MIN__TES_EMPTY__AUX_OFF
				break;

			case CR_DF__PC_MAX__TES_FULL__AUX_OFF:
            {
                // The PC is operating at its maximum operating fraction
                // TES is fully charged at the end of the timestep
                // The CR is still delivering too much mass flow rate and needs to be defocused

                if (!mc_collector_receiver.m_is_sensible_htf)
                {
                    std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
                    throw(C_csp_exception(err_msg, "CSP Solver"));
                }

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::ON;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__PC_MAX_PLUS_TES_FULL__PC_MAX;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = true;

                double q_dot_pc_fixed = q_pc_target;        //[MWt]
                op_mode_str = "CR_DF__PC_MAX__TES_FULL__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;
                    is_rec_su_allowed = false;
                    are_models_converged = false;
                    break;
                }

                // Set member defocus
                m_defocus = defocus_solved;

                are_models_converged = true;

            }	// end 'CR_DF__PC_MAX__TES_FULL__AUX_OFF'
                break;


			case CR_DF__PC_SU__TES_FULL__AUX_OFF:
			{				
                // The PC is operating at its maximum operating fraction
				// TES is fully charged at the end of the timestep
				// The CR is still delivering too much mass flow rate and needs to be defocused
				
				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__TO_PC_PLUS_TES_FULL__ITER_M_DOT_SU;
                C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = true;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_DF__PC_SU__TES_FULL__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                // Set member defocus
                m_defocus = defocus_solved;

                are_models_converged = true;

            }	// end 'CR_DF__PC_MAX__TES_FULL__AUX_OFF'
				break;

			case CR_ON__PC_SB__TES_FULL__AUX_OFF:
			{
				// The collector receiver is on and delivering hot HTF to the TES and PC
				// The PC is operating between its target and maximum thermal power
				// The TES is fully charging over the timestep

				if (!mc_collector_receiver.m_is_sensible_htf)
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STANDBY;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_LESS_TES_FULL;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FIXED;
                bool is_defocus = false;

                double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();        //[MWt]
                op_mode_str = "CR_ON__PC_STANDBY__TES_DC__AUX_OFF";

                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    q_dot_pc_fixed, is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

                if ((mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
                {
                    m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }
                else if (mc_pc_out_solver.m_q_dot_htf < q_pc_sb)
                {
                    m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                // If convergence was successful, finalize this timestep and get out
                // Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
                are_models_converged = true;

            }	// end 'CR_ON__PC_SB__TES_FULL__AUX_OFF'
				break;

			case CR_SU__PC_SU__TES_DC__AUX_OFF:
			{
				// Collector-receiver is starting up
				// Power cycle is starting up, with thermal power from TES
				// Code calculates the shortest timestep of: (CR SU, PC SU, initial timestep)
				//      and then recalculates the other component startup as necessary

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::STARTUP;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_DC_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                
                op_mode_str = "CR_SU__PC_SU__TES_DC__AUX_OFF";
                
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
			}
				break;


			case CR_ON__PC_SU__TES_CH__AUX_OFF:
			{
				// CR in on
				// PC is starting up with its maximum thermal power for startup
				//      and is returning the startup time required
				// Excess CR output is charging TES

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

                double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

                int cr_mode = C_csp_collector_receiver::ON;
                C_csp_power_cycle::E_csp_power_cycle_modes pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode = C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_CH_ONLY;
				C_MEQ__timestep::E_timestep_target_modes step_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                bool is_defocus = false;
                
                op_mode_str = "CR_ON__PC_SU__TES_CH__AUX_OFF";
                double defocus_solved = std::numeric_limits<double>::quiet_NaN();

                int mode_code = solve_operating_mode(cr_mode, pc_mode, solver_mode, step_target_mode,
                    std::numeric_limits<double>::quiet_NaN(), is_defocus, op_mode_str, defocus_solved);

                if (mode_code != 0)
                {
                    m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = false;
                    are_models_converged = false;
                    break;
                }

				// Set member defocus
				m_defocus = defocus_solved;

                are_models_converged = true;
			}
				break;

			default: 
				throw(C_csp_exception("Operation mode not recognized",""));

			}	// End switch() on receiver operating modes
		
		}	
        
        /* 
        ------------ End loop to find correct operating mode and system performance --------
        */


		// Calculate system-level parasitics: can happen after controller/solver converges
		double W_dot_fixed = ms_system_params.m_pb_fixed_par*m_cycle_W_dot_des;			//[MWe]

		double W_dot_ratio = mc_pc_out_solver.m_P_cycle / fmax(0.001, m_cycle_W_dot_des);		//[-]

		double W_dot_bop = m_cycle_W_dot_des*ms_system_params.m_bop_par*ms_system_params.m_bop_par_f *
			(ms_system_params.m_bop_par_0 + ms_system_params.m_bop_par_1*W_dot_ratio + ms_system_params.m_bop_par_2*pow(W_dot_ratio,2));
			// [MWe]

        double W_dot_tes_pump;
        if (m_is_tes) {
            W_dot_tes_pump = mc_tes.pumping_power(mc_cr_out_solver.m_m_dot_salt_tot / 3600., mc_pc_out_solver.m_m_dot_htf / 3600., fabs(mc_tes_outputs.m_m_dot_cold_tank_to_hot_tank),
                mc_cr_htf_state_in.m_temp + 273.15, mc_cr_out_solver.m_T_salt_hot + 273.15,
                mc_pc_htf_state_in.m_temp + 273.15, mc_pc_out_solver.m_T_htf_cold + 273.15,
                mc_cr_out_solver.m_is_recirculating);
        }
        else {
            W_dot_tes_pump = 0.;
        }
        if (W_dot_tes_pump < 0 || W_dot_tes_pump != W_dot_tes_pump){
            error_msg = "TES pumping power failed";
            throw(C_csp_exception(error_msg, "System-level parasitics"));
        }

		double W_dot_net = mc_pc_out_solver.m_P_cycle - 
			mc_cr_out_solver.m_W_dot_col_tracking -
			mc_cr_out_solver.m_W_dot_htf_pump - 
			(mc_pc_out_solver.m_W_dot_htf_pump + W_dot_tes_pump) -
			mc_cr_out_solver.m_q_rec_heattrace -
			mc_pc_out_solver.m_W_cool_par -
			mc_tes_outputs.m_q_heater - 
			W_dot_fixed -
			W_dot_bop;	//[MWe]


        // Timestep solved: run post-processing, converged()		
		mc_collector_receiver.converged();
		mc_power_cycle.converged();
		mc_tes.converged();
		
        //update the tracked field generation
        disp_qsf_last = mc_cr_out_solver.m_q_startup > 0. ? mc_cr_out_solver.m_q_thermal : 0.;    //only count if not starting up

        //Update the estimated thermal energy storage charge state
        double e_tes_disch = 0.;
		double mhot_avail = 0.;
		double mcold_avail = 0.;
        if(m_is_tes)
        {
            double mdot_disch, Tdisch;
			mc_tes.discharge_avail_est(m_T_htf_cold_des, mc_kernel.mc_sim_info.ms_ts.m_step, e_tes_disch, mdot_disch, Tdisch);

            e_tes_disch *= mc_kernel.mc_sim_info.ms_ts.m_step / 3600.;  //MWh
			mhot_avail = mdot_disch * mc_kernel.mc_sim_info.ms_ts.m_step;  //kg

			double e_tes_ch, mdot_ch, Tch;
			mc_tes.charge_avail_est(m_cycle_T_htf_hot_des, mc_kernel.mc_sim_info.ms_ts.m_step, e_tes_ch, mdot_ch, Tch);
			mcold_avail = mdot_ch * mc_kernel.mc_sim_info.ms_ts.m_step;  //kg
        }

		// Save timestep outputs
		// This is after timestep convergence, so be sure convergence() methods don't unexpectedly change outputs
		
			// Simulation outputs
		mv_time_local.push_back(mc_kernel.mc_sim_info.ms_ts.m_time);
		mc_reported_outputs.value(C_solver_outputs::TIME_FINAL, mc_kernel.mc_sim_info.ms_ts.m_time/3600.0);
		mc_reported_outputs.value(C_solver_outputs::MONTH, mc_weather.ms_outputs.m_month);	//[-]
		mc_reported_outputs.value(C_solver_outputs::HOUR_DAY, (int)(m_report_time_end/3600) % 24);	//[hr]


		int n_sub_ts = (int)mv_time_local.size();
		mc_reported_outputs.overwrite_vector_to_constant(C_solver_outputs::N_OP_MODES, n_sub_ts);	//[-]
		if( n_sub_ts == 1 )
		{
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_1, operating_mode);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_2, 0.0);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_3, 0.0);
		}
		else if( n_sub_ts == 2 )
		{
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_1, 0.0);
			mc_reported_outputs.overwrite_vector_to_constant(C_solver_outputs::OP_MODE_2, operating_mode);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_3, 0.0);
		}
		else if( n_sub_ts == 3 )
		{
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_1, 0.0);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_2, 0.0);
			mc_reported_outputs.overwrite_vector_to_constant(C_solver_outputs::OP_MODE_3, operating_mode);
		}
		else
		{
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_1, 0.0);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_2, 0.0);
			mc_reported_outputs.value(C_solver_outputs::OP_MODE_3, 0.0);
		}
		

		mc_reported_outputs.value(C_solver_outputs::TOU_PERIOD, (double)tou_period);        //[-]       
		mc_reported_outputs.value(C_solver_outputs::PRICING_MULT, pricing_mult);	//[-] 
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_SB, q_pc_sb);          //[MW]     
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_MIN, q_pc_min);        //[MW]    
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_TARGET, q_pc_target);  //[MW]
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_MAX, m_q_dot_pc_max);         //[MW]    
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_REC_SU, is_rec_su_allowed);     //[-] 
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_PC_SU, is_pc_su_allowed);       //[-] 
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_PC_SB, is_pc_sb_allowed);       //[-]  
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CR_SU, is_pc_sb_allowed);     //[-]
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CR_ON, q_dot_cr_on);          //[MWt]
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_DC, q_dot_tes_dc);            //[MWt]    
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CH, q_dot_tes_ch);            //[MWt]    

		double m_dot_bal_hot = (mc_cr_out_solver.m_m_dot_salt_tot +
							mc_tes_outputs.m_m_dot_tes_hot_out*3600.0 -
							mc_pc_inputs.m_m_dot -
							mc_tes_outputs.m_m_dot_cr_to_tes_hot*3600.0) / m_m_dot_pc_des;		//[-]

		double m_dot_bal_cold = (mc_pc_inputs.m_m_dot +
							mc_tes_outputs.m_m_dot_tes_cold_out*3600.0 -
							mc_cr_out_solver.m_m_dot_salt_tot - 
							mc_tes_outputs.m_m_dot_pc_to_tes_cold*3600.0) / m_m_dot_pc_des;	//[-]

		double m_dot_bal_max = max(fabs(m_dot_bal_hot), fabs(m_dot_bal_cold));

		double q_dot_bal = (mc_cr_out_solver.m_q_thermal +
							mc_tes_outputs.m_q_dot_dc_to_htf -
							mc_pc_out_solver.m_q_dot_htf -
							mc_tes_outputs.m_q_dot_ch_from_htf) / m_cycle_q_dot_des;	//[-]

		mc_reported_outputs.value(C_solver_outputs::ERR_M_DOT, m_dot_bal_max);
		mc_reported_outputs.value(C_solver_outputs::ERR_Q_DOT, q_dot_bal);

		mc_reported_outputs.value(C_solver_outputs::SOLZEN, mc_weather.ms_outputs.m_solzen);	//[deg] Solar zenith
		mc_reported_outputs.value(C_solver_outputs::SOLAZ, mc_weather.ms_outputs.m_solazi);		//[deg] Solar azimuth
		mc_reported_outputs.value(C_solver_outputs::BEAM, mc_weather.ms_outputs.m_beam);		//[W/m2] DNI
		mc_reported_outputs.value(C_solver_outputs::TDRY, mc_weather.ms_outputs.m_tdry);		//[C] Dry bulb temperature
		mc_reported_outputs.value(C_solver_outputs::TWET, mc_weather.ms_outputs.m_twet);		//[C] Wet bulb temperature
		mc_reported_outputs.value(C_solver_outputs::RH, mc_weather.ms_outputs.m_rhum);			//[-] Relative humidity
		mc_reported_outputs.value(C_solver_outputs::WSPD, mc_weather.ms_outputs.m_wspd);		//[m/s]
		mc_reported_outputs.value(C_solver_outputs::PRES, mc_weather.ms_outputs.m_pres);		//[mbar]
		
		mc_reported_outputs.value(C_solver_outputs::CR_DEFOCUS, m_defocus);						//[-] Controller defocus
			// Thermal energy storage outputs
		mc_reported_outputs.value(C_solver_outputs::TES_Q_DOT_DC, mc_tes_outputs.m_q_dot_dc_to_htf);    //[MWt] TES discharge thermal power   
		mc_reported_outputs.value(C_solver_outputs::TES_Q_DOT_CH, mc_tes_outputs.m_q_dot_ch_from_htf);  //[MWt] TES charge thermal power    
		mc_reported_outputs.value(C_solver_outputs::TES_E_CH_STATE, e_tes_disch);                       //[MWht] TES charge state 

		mc_reported_outputs.value(C_solver_outputs::M_DOT_CR_TO_TES_HOT, mc_tes_outputs.m_m_dot_cr_to_tes_hot);		//[kg/s]
		mc_reported_outputs.value(C_solver_outputs::M_DOT_TES_HOT_OUT, mc_tes_outputs.m_m_dot_tes_hot_out);			//[kg/s]
		mc_reported_outputs.value(C_solver_outputs::M_DOT_PC_TO_TES_COLD, mc_tes_outputs.m_m_dot_pc_to_tes_cold);	//[kg/s]
		mc_reported_outputs.value(C_solver_outputs::M_DOT_TES_COLD_OUT, mc_tes_outputs.m_m_dot_tes_cold_out);		//[kg/s]
		mc_reported_outputs.value(C_solver_outputs::M_DOT_FIELD_TO_CYCLE, mc_tes_outputs.m_m_dot_field_to_cycle);	//[kg/s]
		mc_reported_outputs.value(C_solver_outputs::M_DOT_CYCLE_TO_FIELD, mc_tes_outputs.m_m_dot_cycle_to_field);	//[kg/s]


			// Parasitics outputs
		mc_reported_outputs.value(C_solver_outputs::COL_W_DOT_TRACK, mc_cr_out_solver.m_W_dot_col_tracking);    //[MWe] Collector tracking, startup, stow power consumption 
		mc_reported_outputs.value(C_solver_outputs::CR_W_DOT_PUMP, mc_cr_out_solver.m_W_dot_htf_pump);          //[MWe] Receiver/tower HTF pumping power   
		mc_reported_outputs.value(C_solver_outputs::SYS_W_DOT_PUMP, (mc_pc_out_solver.m_W_dot_htf_pump + W_dot_tes_pump ));    //[MWe] TES & PC HTF pumping power (Receiver - PC side HTF)  
		mc_reported_outputs.value(C_solver_outputs::PC_W_DOT_COOLING, mc_pc_out_solver.m_W_cool_par);           //[MWe] Power cycle cooling power consumption (fan, pumps, etc.)
		mc_reported_outputs.value(C_solver_outputs::SYS_W_DOT_FIXED, W_dot_fixed);								//[MWe] Fixed electric parasitic power load 
		mc_reported_outputs.value(C_solver_outputs::SYS_W_DOT_BOP, W_dot_bop);									//[MWe] Balance-of-plant electric parasitic power load   
		mc_reported_outputs.value(C_solver_outputs::W_DOT_NET, W_dot_net);								//[MWe] Total electric power output to grid        
		
            //Dispatch optimization outputs
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SOLVE_STATE, dispatch.outputs.solve_state);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SOLVE_ITER, dispatch.outputs.solve_iter);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SOLVE_OBJ, dispatch.outputs.objective);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SOLVE_OBJ_RELAX, dispatch.outputs.objective_relaxed);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_QSF_EXPECT, disp_qsf_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_QSFPROD_EXPECT, disp_qsfprod_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_QSFSU_EXPECT, disp_qsfsu_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_TES_EXPECT, disp_tes_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_PCEFF_EXPECT, disp_etapb_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SFEFF_EXPECT, disp_etasf_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_QPBSU_EXPECT, disp_qpbsu_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_WPB_EXPECT, disp_wpb_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_REV_EXPECT, disp_rev_expect);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_PRES_NCONSTR, dispatch.outputs.presolve_nconstr);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_PRES_NVAR, dispatch.outputs.presolve_nvar);
		mc_reported_outputs.value(C_solver_outputs::DISPATCH_SOLVE_TIME, dispatch.outputs.solve_time);

		// Report series of operating modes attempted during the timestep as a 'double' using 0s to separate the enumerations 
		// ... (10 is set as a dummy enumeration so it won't show up as a potential operating mode)
		int n_op_modes = (int)m_op_mode_tracking.size();
		double op_mode_key = 0.0;
		for( int i = 0; i < fmin(3,n_op_modes); i++ )
		{
			double op_mode_step = m_op_mode_tracking[i];

			if( op_mode_step < 10.0 )
			{
				op_mode_key = 100.0*op_mode_key + 10.0*op_mode_step;
			}
			else
			{
				op_mode_key = 100.0*op_mode_key + op_mode_step;
			}
		}
		mc_reported_outputs.value(C_solver_outputs::CTRL_OP_MODE_SEQ_A, op_mode_key);

		op_mode_key = 0.0;
		for( int i = 3; i < fmin(6,n_op_modes); i++ )
		{
			double op_mode_step = m_op_mode_tracking[i];

			if( op_mode_step < 10.0 )
			{
				op_mode_key = 100.0*op_mode_key + 10.0*op_mode_step;
			}
			else
			{
				op_mode_key = 100.0*op_mode_key + op_mode_step;
			}
		}
		mc_reported_outputs.value(C_solver_outputs::CTRL_OP_MODE_SEQ_B, op_mode_key);

		op_mode_key = 0.0;
		for( int i = 6; i < n_op_modes; i++ )
		{
			double op_mode_step = m_op_mode_tracking[i];

			if( op_mode_step < 10.0 )
			{
				op_mode_key = 100.0*op_mode_key + 10.0*op_mode_step;
			}
			else
			{
				op_mode_key = 100.0*op_mode_key + op_mode_step;
			}
		}
		mc_reported_outputs.value(C_solver_outputs::CTRL_OP_MODE_SEQ_C, op_mode_key);

        operating_mode_str_prev = operating_mode_str;
        op_mode_str_prev = op_mode_str;

		mc_reported_outputs.set_timestep_outputs();




		// ****************************************************
		//          End saving timestep outputs
		// ****************************************************

		// Now check if internal csp timestep matches reporting timestep
		do
		{			
			if(mc_kernel.mc_sim_info.ms_ts.m_time >= m_report_time_end)
			{
				mc_collector_receiver.write_output_intervals(m_report_time_start, mv_time_local, m_report_time_end);
				mc_power_cycle.write_output_intervals(m_report_time_start, mv_time_local, m_report_time_end);
				mc_tes.write_output_intervals(m_report_time_start, mv_time_local, m_report_time_end);

				// Overwrite TIME_FINAL
				mc_reported_outputs.overwrite_most_recent_timestep(C_solver_outputs::TIME_FINAL, m_report_time_end / 3600.0);	//[hr]
				mc_reported_outputs.send_to_reporting_ts_array(m_report_time_start, mv_time_local, m_report_time_end);

				// Check if the most recent csp solver timestep aligns with the end of the reporting timestep
				bool delete_last_step = false;
				int pop_back_start = 1;

				int n_time_local = (int)mv_time_local.size();
				if( mv_time_local[n_time_local - 1] == m_report_time_end )
				{
					delete_last_step = true;
					pop_back_start = 0;
				}

				// If more than 1 element in temp vectors, only keep most recent value
				if( n_time_local > 1 || delete_last_step )
				{
					if( !delete_last_step )
					{
						mv_time_local[0] = mv_time_local[n_time_local - 1];
					}

					for( int i = pop_back_start; i < n_time_local; i++ )
					{
						mv_time_local.pop_back();
					}
				}

				int n_time_local_refresh = (int)mv_time_local.size();
				if(n_time_local_refresh > 0)
				{
					mc_reported_outputs.value(C_solver_outputs::N_OP_MODES, 1);	//[-]
					mc_reported_outputs.value(C_solver_outputs::OP_MODE_1, operating_mode);
					mc_reported_outputs.value(C_solver_outputs::OP_MODE_2, 0.0);
					mc_reported_outputs.value(C_solver_outputs::OP_MODE_3, 0.0);
				}

				// Advance time_reporting_hr index
				m_i_reporting++;
				m_report_time_start = m_report_time_end;	//[s]
				m_report_time_end += m_report_step;	//[s]			
			}
			else
			{
				break;
			}

		} while(true);

		
		// Don't converge weather file if working with partial timesteps
		if( mc_kernel.mc_sim_info.ms_ts.m_time < mc_kernel.get_baseline_end_time() )
		{
			mc_kernel.mc_sim_info.ms_ts.m_time_start = mc_kernel.mc_sim_info.ms_ts.m_time;	//[s]
			mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.get_baseline_end_time();			//[s]
			mc_kernel.mc_sim_info.ms_ts.m_step = mc_kernel.mc_sim_info.ms_ts.m_time - mc_kernel.mc_sim_info.ms_ts.m_time_start;	//[s]
		}
		else if( mc_kernel.mc_sim_info.ms_ts.m_time == mc_kernel.get_baseline_end_time() )
		{
			if( mc_kernel.get_baseline_end_time() == mc_kernel.get_wf_end_time () )
			{
				mc_weather.converged();

				mc_kernel.wf_step_forward();
			}
			else if( mc_kernel.get_baseline_end_time() > mc_kernel.get_wf_end_time() )
			{
				throw(C_csp_exception("Baseline end time is larger than the weather file end time. This shouldn't happen"));
			}
					
			mc_kernel.baseline_step_forward();

			mc_kernel.mc_sim_info.ms_ts.m_time_start = mc_kernel.mc_sim_info.ms_ts.m_time;	//[s]
			mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.get_baseline_end_time();			//[s]
			mc_kernel.mc_sim_info.ms_ts.m_step = mc_kernel.mc_sim_info.ms_ts.m_time - mc_kernel.mc_sim_info.ms_ts.m_time_start;	//[s]
		}
		else
		{
			throw(C_csp_exception("Kernel end time is larger than the baseline end time. This shouldn't happen"));
		}
		
        m_is_first_timestep = false;
	}	// End timestep loop

}	// End simulate() method


void C_csp_tou::init_parent()
{
	// Check that dispatch logic is reasonable
	if( !(mc_dispatch_params.m_dispatch_optimize || mc_dispatch_params.m_is_block_dispatch) )
	{
		throw(C_csp_exception("Must select a plant control strategy", "TOU initialization"));
	}

	if( mc_dispatch_params.m_dispatch_optimize && mc_dispatch_params.m_is_block_dispatch )
	{
		throw(C_csp_exception("Both plant control strategies were selected. Please select one.", "TOU initialization"));
	}

	if( mc_dispatch_params.m_is_block_dispatch )
	{
		if( mc_dispatch_params.m_use_rule_1 )
		{
			if( mc_dispatch_params.m_standby_off_buffer < 0.0 )
			{
				throw(C_csp_exception("Block Dispatch Rule 1 was selected, but the time entered was invalid."
					" Please select a time >= 0", "TOU initialization"));
			}
		}

		if( mc_dispatch_params.m_use_rule_2 )
		{
			if( mc_dispatch_params.m_f_q_dot_pc_overwrite <= 0.0 || 
				mc_dispatch_params.m_q_dot_rec_des_mult <= 0.0 )
			{
				throw(C_csp_exception("Block Dispatch Rule 2 was selected, but the parameters entered were invalid."
					" Both values must be greater than 0", "TOU initialization"));
			}
		}
	}
}
