#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "lib_util.h"
#include "csp_dispatch.h"

#include <algorithm>

#include <sstream>

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
	{C_csp_solver::C_solver_outputs::TES_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},       //[MWt] TES thermal losses
	{C_csp_solver::C_solver_outputs::TES_W_DOT_HEATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},	  //[MWe] TES freeze protection power
	{C_csp_solver::C_solver_outputs::TES_T_HOT, C_csp_reported_outputs::TS_WEIGHTED_AVE},			  //[C] TES final hot tank temperature
	{C_csp_solver::C_solver_outputs::TES_T_COLD, C_csp_reported_outputs::TS_WEIGHTED_AVE},			  //[C] TES final cold tank temperature
	{C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWt] TES discharge thermal power
	{C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWt] TES charge thermal power
	{C_csp_solver::C_solver_outputs::TES_E_CH_STATE, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWht] TES charge state at the end of the time step
	{C_csp_solver::C_solver_outputs::TES_M_DOT_DC, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWt] TES discharge mass flow rate
	{C_csp_solver::C_solver_outputs::TES_M_DOT_CH, C_csp_reported_outputs::TS_WEIGHTED_AVE},		  //[MWt] TES charge mass flow rate
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
	S_csp_system_params &system) : 
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
		m_m_dot_pc_des = m_m_dot_pc_min = m_m_dot_pc_max = m_T_htf_pc_cold_est = std::numeric_limits<double>::quiet_NaN();

	// Reporting and Output Tracking
	mc_reported_outputs.construct(S_solver_output_info);

	m_i_reporting = -1;
	//m_sim_time_start = 
	//m_sim_time_end = 
	//m_sim_step_size_baseline =
	m_report_time_start = m_report_time_end = m_report_step = std::numeric_limits<double>::quiet_NaN();

	m_op_mode_tracking.resize(0);

	error_msg = "";

	mv_time_local.reserve(10);

	// Solved Controller Variables
	m_defocus = std::numeric_limits<double>::quiet_NaN();
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
	init_inputs.m_shift = mc_weather.ms_solved_params.m_shift;		//[deg]
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
	m_m_dot_pc_max = pc_solved_params.m_m_dot_max;		//[kg/hr]				
	
	m_cycle_P_hot_des = pc_solved_params.m_P_hot_des;					//[kPa]
	m_cycle_x_hot_des = pc_solved_params.m_x_hot_des;					//[-]
		// TES
	mc_tes.init();
		// TOU
    mc_tou.mc_dispatch_params.m_isleapyear = mc_weather.ms_solved_params.m_leapyear;
	mc_tou.init();
	mc_tou.init_parent();
		// Thermal Storage
	m_is_tes = mc_tes.does_tes_exist();

	if( mc_collector_receiver.m_is_sensible_htf != mc_power_cycle.m_is_sensible_htf )
	{
		throw(C_csp_exception("The collector-receiver and power cycle models have incompatible HTF - direct/indirect assumptions", "CSP Solver"));
	}

    /* 
    If no TES exists, initialize values to zero. They won't be touched again
    */

	if(!m_is_tes)
	{	// Set constant values for tes HTF states
	
		mc_tes_ch_htf_state.m_m_dot = 0.0;		//[kg/hr]
		mc_tes_ch_htf_state.m_temp_in = 0.0;	//[C]
		mc_tes_ch_htf_state.m_temp_out =0.0;	//[C]

		mc_tes_dc_htf_state.m_m_dot = 0.0;		//[kg/hr]
		mc_tes_dc_htf_state.m_temp_in = 0.0;	//[C]
		mc_tes_dc_htf_state.m_temp_out = 0.0;	//[C]

		mc_tes_outputs.m_q_heater = 0.0;		//[MW]
		mc_tes_outputs.m_W_dot_rhtf_pump = 0.0;	//[MWe]
		mc_tes_outputs.m_q_dot_loss = 0.0;		//[MW]
		mc_tes_outputs.m_q_dot_dc_to_htf = 0.0;	//[MW]
		mc_tes_outputs.m_q_dot_ch_from_htf = 0.0;	//[MW]
		mc_tes_outputs.m_T_hot_ave = 0.0;		//[K]
		mc_tes_outputs.m_T_cold_ave = 0.0;		//[K]
		mc_tes_outputs.m_T_hot_final = 0.0;		//[K]
		mc_tes_outputs.m_T_cold_final = 0.0;	//[K]
	}
}

int C_csp_solver::steps_per_hour()
{
	// Get number of records in weather file
	int n_wf_records = mc_weather.get_n_records();
	int step_per_hour = n_wf_records / 8760;
	return step_per_hour;
}

void C_csp_solver::Ssimulate(C_csp_solver::S_sim_setup & sim_setup, 
								bool(*mf_callback)(void *data, double percent, C_csp_messages *csp_messages, float time_sec), 
								void *m_cdata)
{
	// Get number of records in weather file
	int n_wf_records = mc_weather.get_n_records();
	int step_per_hour = n_wf_records / 8760;

	double wf_step = 3600.0 / step_per_hour;	//[s] Weather file time step - would like to check this against weather file, some day
	
	double step_tolerance = 10.0;		//[s] For adjustable timesteps, if within 10 seconds, assume it equals baseline timestep
	double baseline_step = wf_step;		//[s] Baseline timestep of the simulation - this should probably be technology/model specific
	// Check the collector-receiver model for a maximum step
	if(mc_collector_receiver.m_max_step > 0.0)
	{
		baseline_step = max(step_tolerance, min(baseline_step, mc_collector_receiver.m_max_step));
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

		int neff = 2;
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
    int disp_effadj_count = 0;

	// Block dispatch saved variables
	bool is_q_dot_pc_target_overwrite = false;

	mf_callback(m_cdata, 0.0, 0, 0.0);

    mc_csp_messages.add_message(C_csp_messages::WARNING, util::format("End time: %f", mc_kernel.get_sim_setup()->m_sim_time_end) );

	while( mc_kernel.mc_sim_info.ms_ts.m_time <= mc_kernel.get_sim_setup()->m_sim_time_end )
	{
		// Report simulation progress
		double calc_frac_current = (mc_kernel.mc_sim_info.ms_ts.m_time - mc_kernel.get_sim_setup()->m_sim_time_start) / (mc_kernel.get_sim_setup()->m_sim_time_end - mc_kernel.get_sim_setup()->m_sim_time_start);
		if( calc_frac_current > progress_msg_frac_current )
		{
			if(! 
                mf_callback(m_cdata, (float) calc_frac_current*100.f, &mc_csp_messages, mc_kernel.mc_sim_info.ms_ts.m_time)
                )
                return;     //user cancelled the simulation

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

		// Calculate maximum thermal power to power cycle for startup. This will be zero if power cycle is on.
		double q_dot_pc_su_max = mc_power_cycle.get_max_q_pc_startup();		//[MWt]

		// Get weather at this timestep. Should only be called once per timestep. (Except converged() function)
		mc_weather.timestep_call(mc_kernel.mc_sim_info);

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
		double q_pc_max = m_cycle_max_frac * m_cycle_q_dot_des;		//[MW]
		double q_pc_target = q_pc_max;							//[MW]

		q_pc_target = f_turbine_tou * m_cycle_q_dot_des;	//[MW]

		// Need to call power cycle at ambient temperature to get a guess of HTF return temperature
		// If the return temperature is hotter than design, then the mass flow from the receiver will be
		// bigger than expected
		mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15; //[C]
		mc_pc_htf_state_in.m_pres = m_cycle_P_hot_des;	//[kPa]
		mc_pc_htf_state_in.m_qual = m_cycle_x_hot_des;	//[-]
		mc_pc_inputs.m_m_dot = m_m_dot_pc_des;						//[kg/hr] no mass flow rate to power cycle
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
		double q_dot_tes_dc, q_dot_tes_ch;
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
		if( q_pc_target < q_pc_min )
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
                    opt_horizon = min((double)opt_horizon, (double)(8761-hour_now));

                //message
                stringstream ss;
                ss << "Optimizing thermal energy dispatch profile for time window " 
					<< (int)(mc_kernel.mc_sim_info.ms_ts.m_time / 3600.) << " - "
					<< (int)(mc_kernel.mc_sim_info.ms_ts.m_time / 3600.) + mc_tou.mc_dispatch_params.m_optimize_frequency;
                
                mc_csp_messages.add_message(C_csp_messages::NOTICE, ss.str());
			    if(! 
					mf_callback(m_cdata, (float)calc_frac_current*100.f, &mc_csp_messages, mc_kernel.mc_sim_info.ms_ts.m_time)
                    ) 
                    return;

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
                    dispatch.predict_performance(
                            mc_kernel.mc_sim_info.ms_ts.m_time/ baseline_step - 1, 
                            opt_horizon * mc_tou.mc_dispatch_params.m_disp_steps_per_hour, 
                            (int)(3600./baseline_step)/mc_tou.mc_dispatch_params.m_disp_steps_per_hour
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
                && dispatch.m_current_read_step < dispatch.outputs.q_pb_target.size()
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
				double eta_diff = 1.;
				double eta_calc = dispatch.params.eta_cycle_ref;
				int i = 0;
				if (dispatch.w_lim.at(dispatch.m_current_read_step) < 1.e-6)
					q_pc_max = 0.0;
				else
				{
					while (eta_diff > 0.001 && i<20)
					{
						double q_pc_est = dispatch.w_lim.at(dispatch.m_current_read_step)*1.e-3 / eta_calc;			// Estimated power cycle thermal input at w_lim
						double eta_new = mc_power_cycle.get_efficiency_at_load(q_pc_est / m_cycle_q_dot_des);		// Calculated power cycle efficiency
						eta_diff = fabs(eta_calc - eta_new);
						eta_calc = eta_new;
						i++;
					}
					q_pc_max = fmin(q_pc_max, dispatch.w_lim.at(dispatch.m_current_read_step)*1.e-3 / eta_calc); // Restrict max pc thermal input to *approximate* current allowable value (doesn't yet account for parasitics)
					q_pc_max = fmax(q_pc_max, q_pc_target);													// calculated q_pc_target accounts for parasitics --> can be higher than approximate limit 
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

		int operating_mode = ENTRY_MODE;
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
				else if( q_dot_tes_dc && is_pc_su_allowed &&
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

							// Is storage available to discharge to power cycle?
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
								else if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < q_pc_max 
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
								if( (q_dot_cr_on*(1.0 - tol_mode_switching) < q_pc_max && m_dot_cr_on*(1.0 - tol_mode_switching)) &&
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
								else
								{
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


			switch( operating_mode )
			{
			case CR_DF__PC_SU__TES_OFF__AUX_OFF:
			case CR_DF__PC_MAX__TES_OFF__AUX_OFF:
			{
				// The PC is operating at its maximum operating thermal power or HTF mass flow rate
				// TES cannot be charged
				// Defocus CR to hit PC constraints

				if (!mc_collector_receiver.m_is_sensible_htf)
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				std::string op_mode_str = "";
				int pc_mode = -1;
				if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
				{
					op_mode_str = "CR_DF__PC_SU__TES_OFF__AUX_OFF";
					pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				}
				else
				{
					op_mode_str = "CR_DF__PC_MAX__TES_OFF__AUX_OFF";
					pc_mode = C_csp_power_cycle::ON;
				}

				// First, check if at defocus = 1 whether the PC mass flow rate is less than maximum
				//    when storage is fully charged				
				C_MEQ_cr_on__pc_m_dot_max__tes_off__defocus c_df_m_dot(this, pc_mode);
				C_monotonic_eq_solver c_df_m_dot_solver(c_df_m_dot);
				
				double defocus_guess = 1.0;
				double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
				int m_dot_df_code = c_df_m_dot_solver.test_member_function(defocus_guess, &m_dot_bal);
				if (m_dot_df_code != 0)
				{
					error_msg = util::format("At time = %lg the controller chose %s operating mode, but the collector/receiver "
						"and power cycle did not converge on a cold HTF temp at defocus = 1. Controller will shut-down CR and PC",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
					{
						m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
					}
					else
					{
						// Next operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
					}
					
					are_models_converged = false;
					break;
				}

				if (m_dot_bal > 0.0)
				{	// At no defocus, PC mass flow rate is over upper limit
					// So, need to find defocus that results in mass flow <= max
						// Set first solution from defocus = 1.0 above
					C_monotonic_eq_solver::S_xy_pair xy1;
					xy1.x = 1.0;
					xy1.y = m_dot_bal;

					// Guess another guess value
					C_monotonic_eq_solver::S_xy_pair xy2;
					xy2.x = xy1.x * (1.0 / (1.0 + m_dot_bal));

					m_dot_df_code = c_df_m_dot_solver.test_member_function(xy2.x, &m_dot_bal);
					if (m_dot_df_code != 0)
					{
						error_msg = util::format("At time = %lg the controller chose %s operating mode, but the collector/receiver "
							"and power cycle did not converge on a cold HTF temp at defocus guess = %lg. Controller will shut-down CR and PC",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), xy2.x);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
						{
							m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
						}
						else
						{
							// Next operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
							m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
						}
						are_models_converged = false;
						break;
					}

					xy2.y = m_dot_bal;

					// Set up solver for defocus
					c_df_m_dot_solver.settings(1.E-3, 50, 0.0, 1.0, false);

					// Now solve for the required defocus
					double defocus_solved, tol_solved;
					defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int defocus_code = 0;
					try
					{
						defocus_code = c_df_m_dot_solver.solve(xy1, xy2, -1.E-3, defocus_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception(util::format("At time = %lg, %s failed to find a solution"
							" to achieve a PC HTF mass flow less than the maximum", mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str()), ""));
					}
					
					if (defocus_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (defocus_code > C_monotonic_eq_solver::CONVERGED && abs(tol_solved) < 0.1)
						{
							std::string msg = util::format("At time = %lg %s "
								"iteration to find a defocus resulting in the maximum power cycle mass flow rate only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
						}
						else
						{
							// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
							error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
								"failed to find a solution to achieve a PC HTF mass flow rate less than maximum. Controller will shut-down CR and PC",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

							if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
							{
								m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
							}
							else
							{
								// Next operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
							}

							are_models_converged = false;

							break;
						}
					}

					defocus_guess = defocus_solved;
				}

				// Now get the thermal power from the CR
				// Note that power cycle solved with max mass flow rate regardless of what CR sent, so can't use that q_dot
				// If it's greater, then we know upper limit on defocus and need to iterate AGAIN
				double q_dot_pc_defocus = mc_cr_out_solver.m_q_thermal;		//[MWt]

				if ((q_dot_pc_defocus - q_pc_max) / q_pc_max > 1.E-3)
				{
					C_MEQ_cr_on__pc_q_dot_max__tes_off__defocus c_eq(this, pc_mode, q_pc_max);
					C_monotonic_eq_solver c_solver(c_eq);

					// Set up solver
					c_solver.settings(1.E-3, 50, 0.0, defocus_guess, true);

					// Solve for defocus
					double defocus_guess_high = (std::min)(0.99*defocus_guess, defocus_guess * (q_pc_max / q_dot_pc_defocus));
					double defocus_guess_low = defocus_guess_high * 0.9;

					double defocus_solved, tol_solved;
					defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int solver_code = 0;
					try
					{
						solver_code = c_solver.solve(defocus_guess_low, defocus_guess_high, q_pc_max, defocus_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception(util::format("At time = %lg, %s failed to find a solution"
							" to achieve a PC thermal power less than the maximum", mc_kernel.mc_sim_info.ms_ts.m_time, op_mode_str.c_str()), ""));
					}

					if (solver_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (solver_code > C_monotonic_eq_solver::CONVERGED && abs(tol_solved) < 0.1)
						{
							std::string msg = util::format("At time = %lg %s "
								"iteration to find a defocus resulting in the maximum power cycle heat input only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
						}
						else
						{
							// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
							error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
								"failed to achieve a PC thermal powre less than the maximum. Controller will shut-down CR and PC",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

							if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
							{
								m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;
							}
							else
							{
								// Next operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
							}

							are_models_converged = false;

							break;
						}
					}

					defocus_guess = defocus_solved;
				}

				if (defocus_guess == 1.0)
				{
					if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
					{	
						// Still haven't converged solution for defocus = 1.0, so essentially call CR_ON__PC_SU__TES_OFF

						// CR: ON
						mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

						mc_collector_receiver.on(mc_weather.ms_outputs,
							mc_cr_htf_state_in,
							m_defocus,
							mc_cr_out_solver,
							mc_kernel.mc_sim_info);

						if (mc_cr_out_solver.m_q_thermal == 0.0)
						{	// Collector/receiver can't produce useful energy

							m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail = false;

							are_models_converged = false;
							break;
						}

						// If receiver IS producing energy, try starting up power cycle
						// Power Cycle: STARTUP
						mc_pc_htf_state_in.m_temp = mc_cr_out_solver.m_T_salt_hot;		//[C]
						mc_pc_inputs.m_m_dot = mc_cr_out_solver.m_m_dot_salt_tot;		//[kg/hr] no mass flow rate to power cycle
						// Inputs
						mc_pc_inputs.m_standby_control = C_csp_power_cycle::STARTUP;
						// Performance Call
						mc_power_cycle.call(mc_weather.ms_outputs,
							mc_pc_htf_state_in,
							mc_pc_inputs,
							mc_pc_out_solver,
							mc_kernel.mc_sim_info);

					}
					else
					{
						// Still haven't converged solution for defocus = 1.0, so essentially call CR_ON__PC_RM_HI__TES_OFF here

						C_mono_eq_cr_to_pc_to_cr c_eq(this, pc_mode, m_P_cold_des, -1, defocus_guess);
						C_monotonic_eq_solver c_solver(c_eq);

						c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

						double T_htf_cold_guess_colder = m_T_htf_pc_cold_est - 10.0;		//[C]]
						double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

						double T_htf_cold_solved, tol_solved;
						T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
						int iter_solved = -1;

						int solver_code = 0;
						try
						{
							solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
						}
						catch (C_csp_exception)
						{
							throw(C_csp_exception(util::format("At time = %lg, %s failed to find a solution"
								" at defocus = 1", mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str()), ""));
						}

						if (solver_code != C_monotonic_eq_solver::CONVERGED)
						{
							if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
							{
								std::string msg = util::format("At time = %lg %s "
									"iteration to find a defocus resulting in the maximum power cycle heat input only reached a convergence "
									"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
									mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
								mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
							}
							else
							{
								m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail = false;
								are_models_converged = false;
								break;
							}
						}						
					}					
				}

				if (operating_mode == CR_DF__PC_SU__TES_OFF__AUX_OFF)
				{
					double step_pc_su = mc_pc_out_solver.m_time_required_su;	//[s]
					if (step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
					{
						mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_su;
						mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_su;
					}
				}

				// Solve for idle storage
				if (m_is_tes)
				{
					mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);

					// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

					// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
					mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
				}

				// Set member defocus
				m_defocus = defocus_guess;

				are_models_converged = true;

				break;
			}

			case CR_ON__PC_RM_HI__TES_OFF__AUX_OFF:
			case CR_ON__PC_RM_LO__TES_OFF__AUX_OFF:
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
				m_defocus = 1.0;

				double tol_C = 2.0;
				double tol = tol_C / m_T_htf_cold_des;

				double relaxed_tol_multiplier = 5.0;
				double relaxed_tol = relaxed_tol_multiplier*tol;

				// Call CR-PC_CR Solver
				int exit_mode = -1;
				double exit_tolerance = std::numeric_limits<double>::quiet_NaN();
				double field_control = 1.0;
				solver_cr_to_pc_to_cr(C_csp_power_cycle::ON, field_control, tol, exit_mode, exit_tolerance);

				// If CR and PC models solved and produced power, but did not converge within tolerance,
				// check whether achieved convergence is "good enough" to report and continue
				if( exit_mode == POOR_CONVERGENCE )
				{
					if( fabs(exit_tolerance) > relaxed_tol )
					{	// Did not converge within Relaxed Tolerance, shut off CR and PC						

						// update 'exit_mode'
						exit_mode = C_csp_solver::CSP_NO_SOLUTION;
					}
					else
					{	// Convergence within Relaxed Tolerance, *Report message* but assume timestep solved in this mode

						error_msg = util::format("At time = %lg the collector/receiver and power cycle solution only reached a convergence"
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time/ 3600.0, exit_tolerance);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						// update 'exit_mode' for following logic branches
						exit_mode = CSP_CONVERGED;
					}
				}

				if( exit_mode == C_csp_solver::CSP_NO_SOLUTION )
				{	// Either CR & PC did not solve/produce power, or did not solve within Relaxed Tolerance: shut off CR and PC

					if( operating_mode == CR_ON__PC_RM_LO__TES_OFF__AUX_OFF )
					{
						m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
						are_models_converged = false;
					}
					else if( operating_mode == CR_ON__PC_RM_HI__TES_OFF__AUX_OFF )
					{
						m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;
						are_models_converged = false;
					}
					else
					{
						throw(C_csp_exception("Operating mode not recognized", "CSP Solver"));
					}

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;

					break;		// exits switch(operating mode)
				}

				else if (exit_mode == CSP_CONVERGED)
				{
					// If the CR and PC models converged, check whether the power cycle thermal input is within bounds

					if( operating_mode == CR_ON__PC_RM_LO__TES_OFF__AUX_OFF )
					{	// In this mode, the power cycle thermal input needs to be greater than the minimum power cycle fraction

						if( mc_cr_out_solver.m_q_thermal < q_pc_min )
						{
							m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
							are_models_converged = false;
							break;						
						}

					}
					else if( operating_mode == CR_ON__PC_RM_HI__TES_OFF__AUX_OFF )
					{	// In this mode, the power cycle thermal input needs to be greater than the target cycle fraction
						// ... and less than the maximum cycle fraction

						if( mc_cr_out_solver.m_q_thermal > q_pc_max || mc_cr_out_solver.m_m_dot_salt_tot > m_m_dot_pc_max )
						{
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = false;
							are_models_converged = false;
							break;
						}
						else if( mc_cr_out_solver.m_q_thermal < q_pc_target )
						{
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;
							are_models_converged = false;
							break;
						}

					}
					else
					{
						throw(C_csp_exception("Operating mode not recognized", "CSP Solver"));
					}


					if(m_is_tes)
					{
						mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
					
					
						// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
						mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
						mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
						mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

						// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
						mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
						mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
						mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					}

					
					are_models_converged = true;
					break;

				}
				else
				{
					throw(C_csp_exception("Solver tried mode 'CR_ON__PC_RM__TES_OFF__AUX_OFF' and did not receive exit instructions", "CSP Solver"));
				}


			}	// end case{} to allow compilation with local (w/r/t case) variables

				break;


			case CR_ON__PC_SB__TES_OFF__AUX_OFF:

				// Collector/receiver is ON
				// Power cycle is running in standby
				// During standby, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
				// so shouldn't need to iterate between CR and PC
				// Assume power cycle can remain in standby the entirety of the timestep

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// First, solve the CR. Again, we're assuming HTF inlet temperature is always = m_T_htf_cold_des
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

				mc_collector_receiver.on(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					m_defocus,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				if( mc_cr_out_solver.m_q_thermal < q_pc_sb )
				{	// Collector/receiver can't produce useful energy
					
					m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// If receiver is indeed producing power, then try power cycle at standby
					// Power cycle: STANDBY
				mc_pc_htf_state_in.m_temp = mc_cr_out_solver.m_T_salt_hot;		//[C]
				mc_pc_inputs.m_m_dot = mc_cr_out_solver.m_m_dot_salt_tot;	//[kg/hr] no mass flow rate to power cycle
					// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::STANDBY;
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				// Check if solved thermal power is greater than target
				if ((mc_pc_out_solver.m_q_dot_htf - q_pc_max) > 1.E-3)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

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

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_OFF__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" less than the minimum PC HTF mass flow rate %lg [kg/s].",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				if( m_is_tes )
				{
					mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);

					// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

					// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
					mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
				}

				are_models_converged = true;

				break;


			case CR_ON__PC_SU__TES_OFF__AUX_OFF:

				// Collector/receiver is ON
				// Startup power cycle
				// During startup, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
				// so shouldn't need to iterate between collector/receiver and power cycle
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// CR: ON
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

				mc_collector_receiver.on(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					m_defocus,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				if( mc_cr_out_solver.m_q_thermal == 0.0 )
				{	// Collector/receiver can't produce useful energy
					
					m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// If receiver IS producing energy, try starting up power cycle
				// Power Cycle: STARTUP
				mc_pc_htf_state_in.m_temp = mc_cr_out_solver.m_T_salt_hot;		//[C]
				mc_pc_inputs.m_m_dot = mc_cr_out_solver.m_m_dot_salt_tot;		//[kg/hr] no mass flow rate to power cycle
					// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::STARTUP;
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				// Check for new timestep
				if( mc_pc_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance )
				{
					// Reset sim_info values
					mc_kernel.mc_sim_info.ms_ts.m_step = mc_pc_out_solver.m_time_required_su;						//[s]
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_pc_out_solver.m_time_required_su;		//[s]
				}

				if( m_is_tes )
				{
					mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);


					// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

					// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
					mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
				}

				// Compare q_dot_to_pc to q_dot_pc_su_max
				if (mc_cr_out_solver.m_q_thermal > q_dot_pc_su_max || mc_cr_out_solver.m_m_dot_salt_tot > m_m_dot_pc_max)
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
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_cr_out_solver.m_m_dot_salt_tot/3600.0, m_m_dot_pc_max/3600.0);
					}
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
				}

				are_models_converged = true;

				break;

			case CR_SU__PC_OFF__TES_OFF__AUX_OFF:

				// Run the collector/receiver under startup mode
				// **************
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				
				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_htf_state_in.m_pres = m_P_cold_des;					//[kPa]
				mc_cr_htf_state_in.m_qual = m_x_cold_des;					//[-]

				mc_collector_receiver.startup(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				// Check that startup happened
				if( mc_cr_out_solver.m_q_startup == 0.0 )
				{	// Collector/receiver can't produce useful energy
					
					m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// Check for new timestep
				if( mc_cr_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance )
				{
					// Reset sim_info values
					mc_kernel.mc_sim_info.ms_ts.m_step = mc_cr_out_solver.m_time_required_su;						//[s]
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_cr_out_solver.m_time_required_su;		//[s]
				}

				// Power Cycle: OFF
				mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15;	//[C]
				mc_pc_htf_state_in.m_pres = m_cycle_P_hot_des;		//[kPa]
				mc_pc_htf_state_in.m_qual = m_cycle_x_hot_des;		//[-]
				mc_pc_inputs.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
					// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::OFF;
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				if( m_is_tes )
				{
					mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);


					// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

					// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
					mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
				}

				are_models_converged = true;

				break;

			case CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

				// Solve all models as 'off' or 'idle'
				// Collector/receiver
				
				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_htf_state_in.m_pres = m_P_cold_des;					//[kPa]
				mc_cr_htf_state_in.m_qual = m_x_cold_des;					//[-]

				mc_collector_receiver.off(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				// Power Cycle: OFF
					// HTF State
				mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15;	//[C]
				mc_pc_htf_state_in.m_pres = m_cycle_P_hot_des;		//[kPa]
				mc_pc_htf_state_in.m_qual = m_cycle_x_hot_des;		//[-]
				mc_pc_inputs.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
					// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::OFF;
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				if( m_is_tes )
				{
					mc_tes.idle(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);


					// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
					mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

					// If not actually discharging (i.e. mass flow rate = 0.0), what should the temperatures be?
					mc_tes_dc_htf_state.m_m_dot = 0.0;										//[kg/hr]
					mc_tes_dc_htf_state.m_temp_in = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K
					mc_tes_dc_htf_state.m_temp_out = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
				}

				are_models_converged = true;

				break;		// exit switch() after CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

			case CR_OFF__PC_SU__TES_DC__AUX_OFF:
			{
				// Use thermal storage to startup power cycle
				// This solver iterates to find the thermal storage outlet temperature to the power cycle
				//    and the power cycle demand mass flow rate that reach system equilibrium

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				double step_tol = step_tolerance;		//[s]
				double step_pc_su = std::numeric_limits<double>::quiet_NaN();

				int exit_mode = CSP_CONVERGED;
				double T_pc_in_exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				solver_pc_su_controlled__tes_dc(step_tol, 
					step_pc_su,
					exit_mode, T_pc_in_exit_tolerance);

				// Check exit mode
				if(exit_mode != CSP_CONVERGED)
				{
					are_models_converged = false;
					m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = false;
					break;
				}

				// Check reported timestep against initial timestep
				if(step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{
					mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_su;
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_su;
				}

				// Now run CR at 'OFF'
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				
				mc_collector_receiver.off(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				are_models_converged = true; 
			}

				break;

			case CR_ON__PC_OFF__TES_CH__AUX_OFF:
			{
				// Method to solve operating mode where the CR is on (under some fixed operating conditions, i.e. defocus)
				// and charging TES. No PC operating or AUX, so the output of the CR connects directly to TES

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				C_MEQ_cr_on__pc_off__tes_ch__T_htf_cold c_eq(this, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				double T_htf_cold_guess_colder = m_T_htf_pc_cold_est;				//[C]
				double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

				double T_htf_cold_solved, tol_solved;
				T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;
				try
				{
					solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception("CR_ON__PC_OFF__TES_CH__AUX_OFF received exception from mono equation solver"));
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
					{
						error_msg = util::format("At time = %lg the CR_ON__PC_OFF__TES_CH__AUX_OFF iteration to find the cold HTF temperature connecting the TES and receiver only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}
				}

				// If CR ON, TES CH solved, then solve powerblock OFF and get out
				// Power Cycle: OFF
					// HTF State
				mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15;	//[C]
					// Inputs
				mc_pc_inputs.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::OFF;
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				are_models_converged = true;

			}	// End brace after code for this operating mode - brace required to avoid compiler error for local variables

				break;

			case CR_ON__PC_TARGET__TES_CH__AUX_OFF:
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

				int power_cycle_mode = -1;
				double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();	//[MWt]

				std::string op_mode_str = "";
				if (operating_mode == CR_ON__PC_TARGET__TES_CH__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::ON;
					q_dot_pc_fixed = q_pc_target;
					op_mode_str = "CR_ON__PC_TARGET__TES_CH__AUX_OFF";
				}
				else if (operating_mode == CR_ON__PC_SB__TES_CH__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::STANDBY;
					q_dot_pc_fixed = q_pc_sb;
					op_mode_str = "CR_ON__PC_SB__TES_CH__AUX_OFF";
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				C_mono_eq_cr_on_pc_target_tes_ch__T_cold c_eq(this, power_cycle_mode, q_dot_pc_fixed, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold HTF temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est - 10.0; 			//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int T_cold_code = 0;
				try
				{
					T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, %s failed", mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str()), ""));
				}

				if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg %s iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						if (operating_mode == CR_ON__PC_TARGET__TES_CH__AUX_OFF)
						{
							m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = false;
						}
						else if (operating_mode == CR_ON__PC_SB__TES_CH__AUX_OFF)
						{
							m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = false;
						}
						are_models_converged = false;
						break;
					}
				}

				double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;		//[MWt]
				double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;		//[kg/hr]

				if (fabs(q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < 1.E-3)
				{	// If successfully solved for target thermal power, check that mass flow is above minimum
					if ( (m_dot_pc_solved - m_m_dot_pc_min) / fmax(0.01, m_m_dot_pc_min) < -1.E-3 )
					{
						error_msg = util::format("At time = %lg %s solved with a PC HTF mass flow rate %lg [kg/s]"
							" smaller than the minimum %lg [kg/s]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), m_dot_pc_solved/3600.0, m_m_dot_pc_min/3600.0);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
				}
				else if ( (q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3 )
				{
					if ( (m_dot_pc_solved - m_m_dot_pc_max) / m_m_dot_pc_max < -1.E-3 )
					{	
						if (operating_mode == CR_ON__PC_TARGET__TES_CH__AUX_OFF)
						{
							// Can send more mass flow to PC from TES
							m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = false;
							are_models_converged = false;
							break;
						}
					}
					if (operating_mode == CR_ON__PC_SB__TES_CH__AUX_OFF)
					{
						// If can't hit standby power then this mode won't work regardless of mass flow constraints
						m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
					//else
					//{	// Mass flow rate to cycle is maxed out, so end here
					//
					//}
				}

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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Define arguments to solver method
				double q_dot_pc_fixed = q_pc_target;		//[MWt]
				int power_cycle_mode = C_csp_power_cycle::ON;
				
				C_mono_eq_cr_on_pc_target_tes_dc c_eq(this, power_cycle_mode, q_dot_pc_fixed, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold HTF temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est;		//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;	//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int T_cold_code = 0;
				try
				{
					T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, CR_ON__PC_TARGET__TES_DC__AUX_OFF failed", mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0), ""));
				}

				if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						std::string msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC failed",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);

						m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
				double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

				// Check bounds on solved thermal power and mass flow rate
				if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
				{
					if ((q_dot_pc_solved - q_pc_max) / q_pc_max > 1.E-3)
					{
						error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
							" greater than the maximum %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_pc_max);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
					else
					{
						error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
							" greater than the target %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_dot_pc_fixed);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
				}
				if (m_dot_pc_solved < m_m_dot_pc_min)
				{	// If we're already hitting the minimum mass flow rate, then trying next operating mode won't help
					error_msg = util::format("At time = %lg CR_ON__PC_TARGET__TES_DC__AUX_OFF solved with a PC HTF mass flow rate %lg [kg/s]"
						" less than the minimum %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				if ( (q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3
					&& (m_dot_pc_solved - m_m_dot_pc_max) / m_m_dot_pc_max < -1.E-3 )
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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				C_mono_eq_cr_on_pc_match_tes_empty c_eq(this, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				double T_htf_cold_guess_colder = m_T_htf_pc_cold_est;				//[C]
				double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

				double T_htf_cold_solved, tol_solved;
				T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;
				try
				{
					solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception("CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF received exception from mono equation solver"));
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
					{
						error_msg = util::format("At time = %lg the iteration to find the cold HTF temperature connecting the power cycle and receiver only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				
				// *********************************
				// Check if solved thermal power is greater than target

				if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_target) / q_pc_target > 1.E-3 )
				{
					if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_max) / q_pc_max > 1.E-3 )
					{
						error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
							" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);

						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
					else
					{
						error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
							" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, q_pc_max);
						
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
				}

				if ( (mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_max) / m_m_dot_pc_max > 1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				// *********************************
				// Check PC q_dot is >= MIN!!!!!!!!

				if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_min) / q_pc_min < -1.E-3 )
				{
					m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}
				if ( (mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_min) / m_m_dot_pc_min < -1.E-3 )
				{
					m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}
				
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

				C_MEQ_cr_df__pc_off__tes_full__defocus c_eq(this);
				C_monotonic_eq_solver c_solver(c_eq);

				double defocus_guess = 1.0;
				double diff_m_dot = std::numeric_limits<double>::quiet_NaN();
				int df_code = c_solver.test_member_function(defocus_guess, &diff_m_dot);
				if (df_code != 0)
				{	// Can't solve models with defocus = 1, so get out

					error_msg = util::format("At time = %lg the controller chose CR_DF__PC_OFF__TES_FULL__AUX_OFF, but the collector/receiver "
						"did not produce power with the design inlet temperature. Controller will shut-down CR and PC",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					are_models_converged = false;

					m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = false;

					break;
				}

				if (diff_m_dot > 0.0)
				{
					// At no defocus, mass flow rate to TES is greater than full charge
					// So need to find a defocus that results in full charge

					// Get another guess value
					C_monotonic_eq_solver::S_xy_pair xy2;
					double defocus_guess_2 = defocus_guess * (1.0 / (1.0 + diff_m_dot));

					// Set up solver for defocus
					c_solver.settings(1.E-3, 50, 0.0, 1.0, false);

					// Now solve for the required defocus
					double defocus_solved, tol_solved;
					defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int defocus_code = 0;
					try
					{
						defocus_code = c_solver.solve(defocus_guess, defocus_guess_2, -1.E-3, defocus_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception(util::format("At time = %lg, CR_DF__PC_OFF__TES_FULL__AUX_OFF failed to find a solution", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
					}

					if (defocus_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (defocus_code > C_monotonic_eq_solver::CONVERGED && abs(tol_solved) < 0.1)
						{
							std::string msg = util::format("At time = %lg CR_DF__PC_OFF__TES_FULL__AUX_OFF "
								"iteration to find a defocus resulting in fully charged TES only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
						}
						else
						{
							// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
							error_msg = util::format("At time = %lg the controller chose CR_DF__PC_OFF__TES_FULL__AUX_OFF operating mode, but the code"
								"failed to solve. Controller will shut-down CR and PC",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

							m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = false;

							are_models_converged = false;

							break;
						}
					}

					defocus_guess = defocus_solved;
				}
				else
				{
					// Haven't actually converged solution yet, so need to basically call CR_ON__PC_OFF__TES_CH
					C_MEQ_cr_on__pc_off__tes_ch__T_htf_cold c_eq(this, m_defocus);
					C_monotonic_eq_solver c_solver(c_eq);

					c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

					double T_htf_cold_guess_colder = m_T_htf_pc_cold_est;				//[C]
					double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

					double T_htf_cold_solved, tol_solved;
					T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int solver_code = 0;
					try
					{
						solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception("CR_ON__PC_OFF__TES_FULL__AUX_OFF received exception from mono equation solver"));
					}

					if (solver_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
						{
							error_msg = util::format("At time = %lg the CR_ON__PC_OFF__TES_FULL__AUX_OFF iteration to find the cold HTF temperature connecting the TES and receiver only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
						}
						else
						{
							m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail = false;

							are_models_converged = false;
							break;
						}
					}
				}

				// Set member data defocus
				m_defocus = defocus_guess;

				// Need to call power cycle at 'OFF'
					// HTF State
				mc_pc_htf_state_in.m_temp = m_cycle_T_htf_hot_des - 273.15;	//[C]
					// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::OFF;
				mc_pc_inputs.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
					// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state_in,
					mc_pc_inputs,
					mc_pc_out_solver,
					mc_kernel.mc_sim_info);

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;
			}
				break;	// break case CR_DF__PC_OFF__TES_FULL__AUX_OFF

			case CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF:
			{
				// The collector receiver is off
				// The power cycle runs at its minimum operating fraction until storage is depleted
				// A new, shorter timestep is calculated here
				
				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				double tol_C = 1.0;								//[C]
				double tol = tol_C / m_cycle_T_htf_hot_des;		//[-]

				double relaxed_tol_mult = 5.0;				//[-]
				double relaxed_tol = relaxed_tol_mult*tol;	//[-]

				double q_dot_pc_fixed = q_pc_min;			//[MWt]

				double time_tes_dc, T_tes_in_exit_tolerance, q_pc_exit_tolerance;
				time_tes_dc = T_tes_in_exit_tolerance = q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				int T_tes_in_exit_mode, q_pc_exit_mode;

				solver_pc_fixed__tes_empty(q_dot_pc_fixed,
					tol,
					time_tes_dc,
					T_tes_in_exit_mode, T_tes_in_exit_tolerance,
					q_pc_exit_mode, q_pc_exit_tolerance);

				if (time_tes_dc > mc_kernel.mc_sim_info.ms_ts.m_step)
				{
					error_msg = util::format("At time = %lg CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF method calculated a timestep"
						"that was longer than the baseline timestep. Controller moved to the next timestep in the"
						"controller hierarchy",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);

					m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is greater than target
				if (mc_pc_out_solver.m_q_dot_htf > q_pc_target)
				{
					if (mc_pc_out_solver.m_q_dot_htf > q_pc_max)
					{
						error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
							" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);

						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				// Update mc_sim_info
				mc_kernel.mc_sim_info.ms_ts.m_step = time_tes_dc;
				mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + time_tes_dc;

				// Now run CR at 'OFF'
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

				mc_collector_receiver.off(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;
			}
				break;	// break case CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF


			case CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF:
			case CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF:
			{
				// The collector-receiver is off
				// The power cycle runs somewhere between its minimum operating fraction and target operation, with thermal input from TES, which is depleted at the end of the timestep

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// First, solve the CR
				if( operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF )
				{
					// Now run CR at 'OFF'
					mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
					
					mc_collector_receiver.off(mc_weather.ms_outputs,
						mc_cr_htf_state_in,
						mc_cr_out_solver,
						mc_kernel.mc_sim_info);

				}
				else if( operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF )
				{
					// Run CR at 'Start Up'
					mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

					mc_collector_receiver.startup(mc_weather.ms_outputs,
						mc_cr_htf_state_in,
						mc_cr_out_solver,
						mc_kernel.mc_sim_info);

					// Check that startup happened
					if( mc_cr_out_solver.m_q_startup == 0.0 )
					{	// Collector/receiver can't produce useful energy

						m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}

					// Check for new timestep
					if( mc_cr_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance )
					{
						// Reset sim_info values
						mc_kernel.mc_sim_info.ms_ts.m_step = mc_cr_out_solver.m_time_required_su;						//[s]
						mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_cr_out_solver.m_time_required_su;		//[s]
					}
				}

				// Set up solver to converge the cold HTF temperature between TES and PC
				C_mono_eq_pc_match_tes_empty c_eq(this);
				C_monotonic_eq_solver c_solver(c_eq);

				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				double T_htf_cold_guess_hotter = m_T_htf_pc_cold_est;		//[C]
				double T_htf_cold_guess_colder = T_htf_cold_guess_hotter - 10.0;	//[C]

				double T_htf_cold_solved, tol_solved;
				T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;
				try
				{
					solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_hotter,
						0.0, T_htf_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
					{
						throw(C_csp_exception("CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF solver to converge the HTF cold temperature returned an unexpected exemption"));
					}
					else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
					{
						throw(C_csp_exception("CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF solver to converge the HTF cold temperature returned an unexpected exemption"));
					}
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
					{
						error_msg = util::format("At time = %lg the iteration to find the hot HTF temperature connecting the power cycle startup and tes discharge only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
							m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
							m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}
				}

				if (mc_pc_out_solver.m_q_dot_htf < q_pc_min || mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
				{
					if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
						m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;
					else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
						m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is greater than target
				if (mc_pc_out_solver.m_q_dot_htf > q_pc_target)
				{
					if (mc_pc_out_solver.m_q_dot_htf > q_pc_max)
					{
						if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);
						}
						else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);
						}

						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
					else
					{
						if (operating_mode == CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, q_pc_max);
						}
						else if (operating_mode == CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF)
						{
							error_msg = util::format("At time = %lg CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
								" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_target, q_pc_max);
						}
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
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

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// First, startup the collector-receiver and get the time required
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

				mc_collector_receiver.startup(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				// Check that startup happened
				if( mc_cr_out_solver.m_q_startup == 0.0 )
				{	// Collector/receiver can't produce useful energy

					m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// Reset timestep based on receiver startup
				if (mc_cr_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{
					// Reset sim_info values
					mc_kernel.mc_sim_info.ms_ts.m_step = mc_cr_out_solver.m_time_required_su;						//[s]
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_cr_out_solver.m_time_required_su;		//[s]
				}


				// Next, calculate the required TES empty time
				C_mono_eq_pc_target_tes_empty__T_cold c_eq(this, q_pc_min);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold HTF temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est;			//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int T_cold_code = 0;
				try
				{
					T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, C_csp_solver::CR_SU__PC_MIN__TES_EMPTY failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
				}

				if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg C_csp_solver::CR_SU__PC_MIN__TES_EMPTY iteration "
							"to find the cold HTF temperature to balance energy between TES and PC target only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						std::string msg = util::format("At time = %lg C_csp_solver::CR_SU__PC_MIN__TES_EMPTY iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC failed",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
						
						m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				double step_pc_empty = c_eq.m_step;		//[s]

				// ******************************************************************
				// Compare the CR and PC startup times
				if (step_pc_empty < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{	// If the time required for CR startup is longer than the time to empty the PC
					//       then rerun CR_SU with the PC empty timestep (and CR_SU will continue in the next timestep)

					// Update simulation time info
					mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_empty;							//[s]
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_empty;			//[s]

					// Rerun CR_SU
					mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

					mc_collector_receiver.startup(mc_weather.ms_outputs,
						mc_cr_htf_state_in,
						mc_cr_out_solver,
						mc_kernel.mc_sim_info);

					// Check that startup happened
					if (mc_cr_out_solver.m_q_startup == 0.0)
					{	// Collector/receiver can't produce useful energy

						m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}
				}

				// Check if solved thermal power is greater than target
				if ( (mc_pc_out_solver.m_q_dot_htf - q_pc_max) > 1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is less than target
				if ( (mc_pc_out_solver.m_q_dot_htf-q_pc_min) / q_pc_min < -1.E-3 )
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" less than the minimum PC thermal power %lg [MWt].",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_min);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf < m_m_dot_pc_min)
				{
					error_msg = util::format("At time = %lg CR_SU__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" less than the minimum PC HTF mass flow rate %lg [kg/s].",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				are_models_converged = true;
			}
				break;


			case CR_ON__PC_SB__TES_DC__AUX_OFF:
			{
				std::string msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF "
					"was called. This isn't a common operating mode and should be stepped through to confirm it's working.",
					mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
				mc_csp_messages.add_message(C_csp_messages::WARNING, msg);

				// The collector receiver is on and returning hot HTF to the PC
				// TES is discharging hot HTF that is mixed with the CR HTF
				// to operate the PC at standby

				if( !mc_collector_receiver.m_is_sensible_htf )
				{
					std::string err_msg = util::format("Operating mode, %d, is not configured for DSG mode", operating_mode);
					throw(C_csp_exception(err_msg, "CSP Solver"));
				}

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Define arguments to solver method
				double q_dot_pc_fixed = q_pc_sb;		//[MWt]
				int power_cycle_mode = C_csp_power_cycle::STANDBY;

				C_mono_eq_cr_on_pc_target_tes_dc c_eq(this, power_cycle_mode, q_dot_pc_fixed, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold HTF temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est;		//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;	//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int T_cold_code = 0;
				try
				{
					T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, CR_ON__PC_SB__TES_DC__AUX_OFF failed", mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0), ""));
				}

				if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						std::string msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF iteration "
							"to find the cold HTF temperature to balance energy between the CR, TES, and PC failed",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);

						m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				double q_dot_pc_solved = mc_pc_out_solver.m_q_dot_htf;	//[MWt]
				double m_dot_pc_solved = mc_pc_out_solver.m_m_dot_htf;	//[kg/hr]

				// Check bounds on solved thermal power and mass flow rate
				if ((q_dot_pc_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
				{
					if ((q_dot_pc_solved - q_pc_max) / q_pc_max > 1.E-3)
					{
						error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
							" greater than the maximum %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_pc_max);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
					else
					{
						error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC thermal power %lg [MWt]"
							" greater than the target %lg [MWt]",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, q_dot_pc_solved, q_dot_pc_fixed);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
				}
				if (m_dot_pc_solved < m_m_dot_pc_min)
				{	// If we're already hitting the minimum mass flow rate, then trying next operating mode won't help
					error_msg = util::format("At time = %lg CR_ON__PC_SB__TES_DC__AUX_OFF solved with a PC HTF mass flow rate %lg [kg/s]"
						" less than the minimum %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, m_dot_pc_solved / 3600.0, m_m_dot_pc_min / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

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

				// If convergence was successful, finalize this timestep and get out
				// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;
			
			}	// end 'CR_ON__PC_SB__TES_DC__AUX_OFF'
				
				break;


			case CR_OFF__PC_TARGET__TES_DC__AUX_OFF:
			case CR_SU__PC_TARGET__TES_DC__AUX_OFF:
			case CR_OFF__PC_SB__TES_DC__AUX_OFF:
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

				int power_cycle_mode = -1;
				double q_dot_pc_fixed = std::numeric_limits<double>::quiet_NaN();	//[MWt]

				std::string op_mode_str = "";
				if (operating_mode == CR_OFF__PC_TARGET__TES_DC__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::ON; 
					q_dot_pc_fixed = q_pc_target;
					op_mode_str = "CR_OFF__PC_TARGET__TES_DC__AUX_OFF";
				}
				else if (operating_mode == CR_SU__PC_TARGET__TES_DC__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::ON; 
					q_dot_pc_fixed = q_pc_target;
					op_mode_str = "CR_SU__PC_TARGET__TES_DC__AUX_OFF";
				}
				else if (operating_mode == CR_OFF__PC_SB__TES_DC__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::STANDBY;
					q_dot_pc_fixed = q_pc_sb;
					op_mode_str = "CR_OFF__PC_SB__TES_DC__AUX_OFF";
				}
				else if (operating_mode == CR_SU__PC_SB__TES_DC__AUX_OFF)
				{
					power_cycle_mode = C_csp_power_cycle::STANDBY;
					q_dot_pc_fixed = q_pc_sb;
					op_mode_str = "CR_SU__PC_SB__TES_DC__AUX_OFF";
				}
				
				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// First, solve the CR
				if( operating_mode == CR_OFF__PC_TARGET__TES_DC__AUX_OFF
					|| operating_mode == CR_OFF__PC_SB__TES_DC__AUX_OFF )
				{
					// Now run CR at 'OFF'
					mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
					
					mc_collector_receiver.off(mc_weather.ms_outputs,
						mc_cr_htf_state_in,
						mc_cr_out_solver,
						mc_kernel.mc_sim_info);

				}
				else if( operating_mode == CR_SU__PC_TARGET__TES_DC__AUX_OFF
					|| operating_mode == CR_SU__PC_SB__TES_DC__AUX_OFF )
				{
					// Run CR at 'Start Up'
					mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

					mc_collector_receiver.startup(mc_weather.ms_outputs,
						mc_cr_htf_state_in,
						mc_cr_out_solver,
						mc_kernel.mc_sim_info);

					// Check that startup happened
					if( mc_cr_out_solver.m_q_startup == 0.0 )
					{	// Collector/receiver can't produce useful energy

						if (operating_mode == CR_SU__PC_TARGET__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_SB__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}

					// Check for new timestep
					if( mc_cr_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance )
					{
						// Reset sim_info values
						mc_kernel.mc_sim_info.ms_ts.m_step = mc_cr_out_solver.m_time_required_su;						//[s]
						mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_cr_out_solver.m_time_required_su;		//[s]
					}
				}

				C_mono_eq_pc_target_tes_dc__T_cold c_eq(this, power_cycle_mode, q_dot_pc_fixed);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, 0, std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold temperature
				double T_cold_guess_low = m_T_htf_cold_des - 273.15;	//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int T_cold_code = 0;
				try
				{
					T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, %s failed", mc_kernel.mc_sim_info.ms_ts.m_time, op_mode_str.c_str()), ""));
				}

				if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg %s"
							"iteration to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						if (operating_mode == CR_OFF__PC_TARGET__TES_DC__AUX_OFF)
							m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_TARGET__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_OFF__PC_SB__TES_DC__AUX_OFF)
							m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_SB__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}
				}

				double q_dot_solved = c_eq.m_q_dot_calc;	//[MWt]
				double m_dot_solved = c_eq.m_m_dot_calc;	//[kg/hr]
				
				// Check if solved thermal power is greater than target
				if ((q_dot_solved - q_dot_pc_fixed) / q_dot_pc_fixed > 1.E-3)
				{
					if ((q_dot_solved - q_pc_max) / q_pc_max > 1.E-3)
					{
						error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
							" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_solved, q_pc_max);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						turn_off_plant();
						are_models_converged = false;
						break;
					}
					else
					{
						error_msg = util::format("At time = %lg %s converged to a PC thermal power %lg [MWt]"
							" larger than the target PC thermal power %lg [MWt] but less than the maximum thermal power %lg [MWt]",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), q_dot_solved, q_dot_pc_fixed, q_pc_max);

						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
				}
				else if ((q_dot_solved - q_dot_pc_fixed) / q_dot_pc_fixed < -1.E-3)
				{
					if (m_dot_solved < m_m_dot_pc_max)
					{	// TES cannot provide enough thermal power - step down to next operating mode
						if (operating_mode == CR_OFF__PC_TARGET__TES_DC__AUX_OFF)
							m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_TARGET__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_OFF__PC_SB__TES_DC__AUX_OFF)
							m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail = false;
						else if (operating_mode == CR_SU__PC_SB__TES_DC__AUX_OFF)
							m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail = false;

						are_models_converged = false;
						break;
					}
					// Notes:
					//else
					//{	// PC maximum mass flow is constraining the thermal power that TES can send the PC. Changing modes wont' help
					//
					//}
				}

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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;
				int power_cycle_mode = C_csp_power_cycle::ON;
				
				C_mono_eq_cr_on__pc_match_m_dot_ceil__tes_full c_eq(this, power_cycle_mode, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est;	//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;
				try
				{
					solver_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, C_csp_solver::CR_ON__PC_RM_HI__TES_FULL failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg C_csp_solver::CR_ON__PC_RM_HI__TES_FULL failed "
							"iteration to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				// Calculate and report mass flow rate balance
				double m_dot_rec = mc_cr_out_solver.m_m_dot_salt_tot;	//[kg/hr]
				double m_dot_pc = mc_pc_out_solver.m_m_dot_htf;			//[kg/hr]
				double m_dot_tes = mc_tes_ch_htf_state.m_m_dot;			//[kg/hr]

				double m_dot_bal = (m_dot_rec - (m_dot_pc + m_dot_tes)) / m_dot_rec;		//[-]

				if (m_dot_bal > 0.0 || (mc_pc_out_solver.m_q_dot_htf - q_pc_max) / q_pc_max > 1.E-3)
				{
					m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}
				else if( mc_pc_out_solver.m_q_dot_htf < q_pc_target )
				{
					error_msg = util::format("At time = %lg CR_ON__PC_RM_HI__TES_FULL__AUX_OFF method converged to a power cycle"
						" thermal input less than the target.",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);
				}

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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				C_MEQ_cr_on__pc_target__tes_empty__T_htf_cold c_eq(this, m_defocus, q_pc_min);
				C_monotonic_eq_solver c_solver(c_eq);

				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				double T_htf_cold_guess_colder = m_T_htf_pc_cold_est;				//[C]
				double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

				double T_htf_cold_solved, tol_solved;
				T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;

				try
				{
					solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception("CR_ON__PC_MIN__TES_EMPTY__AUX_OFF received exception from mono equation solver"));
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
					{
						error_msg = util::format("At time = %lg the CR_ON__PC_MIN__TES_EMPTY__AUX_OFF iteration to find the cold HTF temperature connecting the power cycle, receiver, & TES only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				double time_tes_dc = c_eq.m_step;		//[s]

				if (time_tes_dc > mc_kernel.mc_sim_info.ms_ts.m_step)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF method calculated a timestep"
						"that was longer than the baseline timestep. Controller moved to the next timestep in the"
						"controller hierarchy",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);

					m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				// Check if solved thermal power is greater than target
				if ((mc_pc_out_solver.m_q_dot_htf - q_pc_max) > 1.E-3)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a PC thermal power %lg [MWt]"
						" larger than the maximum PC thermal power %lg [MWt]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_q_dot_htf, q_pc_max);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					turn_off_plant();
					are_models_converged = false;
					break;
				}

				if (mc_pc_out_solver.m_m_dot_htf > m_m_dot_pc_max)
				{
					error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" larger than the maximum PC mass flow rate %lg [kg/s]. Controller shut off plant",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_max / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

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

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				if ( (mc_pc_out_solver.m_m_dot_htf - m_m_dot_pc_min) / m_m_dot_pc_min < -1.E-4 )
				{
					error_msg = util::format("At time = %lg CR_ON__PC_MIN__TES_EMPTY__AUX_OFF converged to a HTF mass flow rate %lg [kg/s]"
						" less than the minimum PC HTF mass flow rate %lg [kg/s]. Controller moved to next operating mode.",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, mc_pc_out_solver.m_m_dot_htf / 3600.0, m_m_dot_pc_min / 3600.0);

					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				// Update mc_sim_info
				mc_kernel.mc_sim_info.ms_ts.m_step = time_tes_dc;
				mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + time_tes_dc;

				are_models_converged = true;
				
			}	// end 'CR_ON__PC_MIN__TES_EMPTY__AUX_OFF
				break;

			case CR_DF__PC_MAX__TES_FULL__AUX_OFF:
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

				std::string op_mode_str = "";
				int pc_mode = -1;
				if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
				{
					op_mode_str = "CR_DF__PC_SU__TES_FULL__AUX_OFF";
					pc_mode = C_csp_power_cycle::STARTUP_CONTROLLED;
				}
				else
				{
					op_mode_str = "CR_DF__PC_MAX__TES_FULL__AUX_OFF";
					pc_mode = C_csp_power_cycle::ON;
				}

				// First, check if at defocus = 1 whether the PC mass flow rate is less than maximum
				//   when storage is fully charged
				C_mono_eq_cr_on__pc_m_dot_max__tes_full_defocus c_df_m_dot(this, pc_mode);
				C_monotonic_eq_solver c_df_m_dot_solver(c_df_m_dot);

				double defocus_guess = 1.0;
				double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
				int m_dot_df_code = c_df_m_dot_solver.test_member_function(defocus_guess, &m_dot_bal);
				if (m_dot_df_code != 0)
				{
					// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
					error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
						" failed to solve at defocus = 1. Controller will shut-down CR and PC",
						mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
					{
						m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
					}
					else
					{
						m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;
					}

					are_models_converged = false;

					break;
				}

				if (m_dot_bal > 0.0)
				{	// At no defocus, PC mass flow rate is exceeding limits
					// So, need to find defocus that results in mass flow <= max
					C_monotonic_eq_solver::S_xy_pair xy1;
					xy1.x = 1.0;
					xy1.y = m_dot_bal;

					// Guess another guess value
					C_monotonic_eq_solver::S_xy_pair xy2;
					xy2.x = xy1.x * (1.0 / (1.0 + m_dot_bal));
					
					m_dot_df_code = c_df_m_dot_solver.test_member_function(xy2.x, &m_dot_bal);
					if (m_dot_df_code != 0)
					{
						// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
						error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
							"failed to solve at defocus = %lg. Controller will shut-down CR and PC",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), xy2.x);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
						{
							m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
						}
						else
						{
							m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;
						}

						are_models_converged = false;

						break;
					}

					xy2.y = m_dot_bal;

					// Set up solver for defocus
					c_df_m_dot_solver.settings(1.E-3, 50, 0.0, 1.0, false);

					// Now solve for the required defocus
					double defocus_solved, tol_solved;
					defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int defocus_code = 0;
					try
					{
						defocus_code = c_df_m_dot_solver.solve(xy1, xy2, -1.E-3, defocus_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception(util::format("At time = %lg, %s failed to find a solution"
							" to achieve a PC HTF mass flow less than the maximum", mc_kernel.mc_sim_info.ms_ts.m_time, op_mode_str.c_str()), ""));
					}

					if (defocus_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (defocus_code > C_monotonic_eq_solver::CONVERGED && abs(tol_solved) < 0.1)
						{
							std::string msg = util::format("At time = %lg %s "
								"iteration to find a defocus resulting in the maximum power cycle mass flow rate only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
						}
						else
						{
							// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
							error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
								"failed to solve. Controller will shut-down CR and PC",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

							if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
							{
								m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
							}
							else
							{
								m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;
							}

							are_models_converged = false;

							break;
						}
					}

					defocus_guess = defocus_solved;
				}

				// Now get the thermal power from the CR and TES
				// Note that power cycle solved with max mass flow rate regardless of what CR sent, so can't use that q_dot
				// If it's greater, then we know upper limit on defocus and need to iterate AGAIN
				double q_dot_pc_defocus = mc_cr_out_solver.m_q_thermal - mc_tes_outputs.m_q_dot_ch_from_htf;		//[MWt]

				if ((q_dot_pc_defocus - q_pc_max) / q_pc_max > 1.E-3)
				{
					if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
					{
						throw(C_csp_exception(util::format("At time = %lg, %s should not need to defocus to meet thermal power requirements"
							" because STARTUP_CONTROLLED should choose a mass flow rate that satisfies thermal power limits", mc_kernel.mc_sim_info.ms_ts.m_time, op_mode_str.c_str()), ""));
					}

					C_mono_eq_cr_on__pc_target__tes_full__defocus c_eq(this, pc_mode, q_pc_max);
					C_monotonic_eq_solver c_solver(c_eq);

					// Set up solver
					c_solver.settings(1.E-3, 50, 0.0, defocus_guess, true);

					// Solve for defocus
					double defocus_guess_high = (std::min)(0.99*defocus_guess, defocus_guess * (q_pc_max / q_dot_pc_defocus));
					double defocus_guess_low = defocus_guess_high * 0.9;

					double defocus_solved, tol_solved;
					defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					int iter_solved = -1;

					int solver_code = 0;
					try
					{
						solver_code = c_solver.solve(defocus_guess_low, defocus_guess_high, q_pc_max, defocus_solved, tol_solved, iter_solved);
					}
					catch (C_csp_exception)
					{
						throw(C_csp_exception(util::format("At time = %lg, CR_DF__PC_MAX__TES_FULL__AUX_OFF failed to find a solution"
							" to achieve a PC thermal power less than the maximum", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
					}

					if (solver_code != C_monotonic_eq_solver::CONVERGED)
					{
						if (solver_code > C_monotonic_eq_solver::CONVERGED && abs(tol_solved) < 0.1)
						{
							std::string msg = util::format("At time = %lg CR_DF__PC_MAX__TES_FULL__AUX_OFF "
								"iteration to find a defocus resulting in the maximum power cycle heat input only reached a convergence "
								"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
							mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
						}
						else
						{
							// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
							error_msg = util::format("At time = %lg the controller chose CR_DF__PC_MAX__TES_FULL__AUX_OFF operating mode, but the code"
								"failed to solve. Controller will shut-down CR and PC",
								mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
							mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

							m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;

							are_models_converged = false;

							break;
						}
					}

					defocus_guess = defocus_solved;
				}
				else if (defocus_guess == 1.0)
				{
					if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
					{
						// Haven't actually converged solution yet, so need to basically call CR_ON__PC_SU__TES_CH
						C_mono_eq_cr_on_pc_su_tes_ch c_eq(this);
						C_monotonic_eq_solver c_solver(c_eq);

						// Get first htf cold temp guess
						double T_htf_cold_guess = m_T_htf_pc_cold_est;	//[C]

						// Use this to test code calculating new htf cold temperature
						// Specifically checking that there's enough mass flow to startup PC AND send > 0 to TES
						double diff_T_htf_cold_temp = std::numeric_limits<double>::quiet_NaN();
						int T_htf_cold_code = c_solver.test_member_function(T_htf_cold_guess, &diff_T_htf_cold_temp);
						if (T_htf_cold_code != 0)
						{	// If failed, go to next mode (CR_ON__PC_SU__TES_OFF)
							m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
							are_models_converged = false;
							break;
						}

						C_monotonic_eq_solver::S_xy_pair xy_pair_1;
						xy_pair_1.x = T_htf_cold_guess;		//[C]
						xy_pair_1.y = diff_T_htf_cold_temp;	//[-]

						// Now guess another HTF temperature
						T_htf_cold_guess += 10.0;			//[C]
						T_htf_cold_code = c_solver.test_member_function(T_htf_cold_guess, &diff_T_htf_cold_temp);
						if (T_htf_cold_code != 0)
						{	// If failed, go to next mode (CR_ON__PC_SU__TES_OFF)
							m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
							are_models_converged = false;
							break;
						}

						C_monotonic_eq_solver::S_xy_pair xy_pair_2;
						xy_pair_2.x = T_htf_cold_guess;		//[C]
						xy_pair_2.y = diff_T_htf_cold_temp;	//[-]

						c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

						// Solve for T_htf_cold
						double T_htf_cold_solved, tol_solved;
						T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
						int iter_solved = -1;

						T_htf_cold_code = 0;
						try
						{
							T_htf_cold_code = c_solver.solve(xy_pair_1, xy_pair_2, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
						}
						catch (C_csp_exception)
						{
							throw(C_csp_exception("CR_DF_PC_SU__TES_FULL__AUX_OFF solver to converge the HTF cold temperature returned an unexpected exemption"));
						}

						if (T_htf_cold_code != C_monotonic_eq_solver::CONVERGED)
						{
							if (T_htf_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
							{
								error_msg = util::format("At time = %lg the iteration to find the cold HTF temperature connecting the receiver, power cycle startup, and tes charge only reached a convergence "
									"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
									mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
								mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
							}
							else
							{
								m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail = false;
								are_models_converged = false;
								break;
							}
						}
					}
					else
					{
						// Haven't actually converged solution yet, so need to basically call CR_ON__PC_RM_HI__TES_FULL
						C_mono_eq_cr_on__pc_match_m_dot_ceil__tes_full c_eq(this, pc_mode, defocus_guess);
						C_monotonic_eq_solver c_solver(c_eq);

						// Set up solver
						c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

						// Solve for cold temperature
						double T_cold_guess_low = m_T_htf_pc_cold_est;	//[C]
						double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

						double T_cold_solved, tol_solved;
						T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
						int iter_solved = -1;

						int solver_code = 0;
						try
						{
							solver_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
						}
						catch (C_csp_exception)
						{
							throw(C_csp_exception(util::format("At time = %lg, C_csp_solver::CR_DF_1__PC_RM_HI__TES_FULL failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
						}

						if (solver_code != C_monotonic_eq_solver::CONVERGED)
						{
							if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
							{
								std::string msg = util::format("At time = %lg C_csp_solver::CR_DF_1__PC_RM_HI__TES_FULL failed "
									"iteration to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
									"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
									mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
								mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
							}
							else
							{
								m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail = false;
								are_models_converged = false;
								break;
							}
						}
					}
				}

				if (operating_mode == CR_DF__PC_SU__TES_FULL__AUX_OFF)
				{
					double step_pc_su = mc_pc_out_solver.m_time_required_su;	//[s]
					if (step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
					{
						mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_su;
						mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_su;
					}
				}

				// Set member defocus
				m_defocus = defocus_guess;

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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;
				int power_cycle_mode = C_csp_power_cycle::STANDBY;

				C_mono_eq_cr_on__pc_match_m_dot_ceil__tes_full c_eq(this, power_cycle_mode, m_defocus);
				C_monotonic_eq_solver c_solver(c_eq);

				// Set up solver
				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for cold temperature
				double T_cold_guess_low = m_T_htf_pc_cold_est;	//[C]
				double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

				double T_cold_solved, tol_solved;
				T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				int solver_code = 0;
				try
				{
					solver_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception(util::format("At time = %lg, CR_ON__PC_SB__TES_FULL__AUX_OFF failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
				}

				if (solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
					{
						std::string msg = util::format("At time = %lg CR_ON__PC_SB__TES_FULL__AUX_OFF failed "
							"iteration to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
					}
					else
					{
						m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				// Calculate and report mass flow rate balance
				double m_dot_rec = mc_cr_out_solver.m_m_dot_salt_tot;	//[kg/hr]
				double m_dot_pc = mc_pc_out_solver.m_m_dot_htf;			//[kg/hr]
				double m_dot_tes = mc_tes_ch_htf_state.m_m_dot;			//[kg/hr]

				double m_dot_bal = (m_dot_rec - (m_dot_pc + m_dot_tes)) / m_dot_rec;		//[-]

				if (m_dot_bal > 0.0 || (mc_pc_out_solver.m_q_dot_htf - q_pc_max) / q_pc_max > 1.E-3)
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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// First, startup the collector-receiver and get the time required
				mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

				mc_collector_receiver.startup(mc_weather.ms_outputs,
					mc_cr_htf_state_in,
					mc_cr_out_solver,
					mc_kernel.mc_sim_info);

				// Check that startup happened
				if( mc_cr_out_solver.m_q_startup == 0.0 )
				{	// Collector/receiver can't produce useful energy
					m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// Reset timestep based on receiver startup
				if (mc_cr_out_solver.m_time_required_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{
					// Reset sim_info values
					mc_kernel.mc_sim_info.ms_ts.m_step = mc_cr_out_solver.m_time_required_su;						//[s]
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + mc_cr_out_solver.m_time_required_su;		//[s]
				}

				// *****************************************************************
				// Next, calculate the required power cycle startup time
				double step_tol = step_tolerance;		//[s]
				double step_pc_su = std::numeric_limits<double>::quiet_NaN();

				int exit_mode = CSP_CONVERGED;
				double T_pc_in_exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				solver_pc_su_controlled__tes_dc(step_tol,
					step_pc_su,
					exit_mode, T_pc_in_exit_tolerance);

				// Check exit mode
				if( exit_mode != CSP_CONVERGED )
				{
					are_models_converged = false;
					m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = false;
					break;
				}

				// ******************************************************************
				// Compare the CR and PC startup times
				if (step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{	// If the time required for CR startup is longer than the time to startup the PC
					//       then rerun CR_SU with the PC startup timestep (and CR_SU will continue in the next timestep)

					// Check if shortest timestep is close to end of initial timestep
					if(step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
					{
						// Update simulation time info
						mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_su;							//[s]
						mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_su;			//[s]

						// Rerun CR_SU
						mc_cr_htf_state_in.m_temp = m_T_htf_cold_des - 273.15;		//[C], convert from [K]

						mc_collector_receiver.startup(mc_weather.ms_outputs,
							mc_cr_htf_state_in,
							mc_cr_out_solver,
							mc_kernel.mc_sim_info);

						// Check that startup happened
						if( mc_cr_out_solver.m_q_startup == 0.0 )
						{	// Collector/receiver can't produce useful energy

							m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail = false;

							are_models_converged = false;
							break;
						}		
					}		
				}
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

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;
				double step_pc_su = std::numeric_limits<double>::quiet_NaN();

				C_mono_eq_cr_on_pc_su_tes_ch c_eq(this);
				C_monotonic_eq_solver c_solver(c_eq);

				// Get first htf cold temp guess
				double T_htf_cold_guess = m_T_htf_pc_cold_est;	//[C]

				// Use this to test code calculating new htf cold temperature
				// Specifically checking that there's enough mass flow to startup PC AND send > 0 to TES
				double diff_T_htf_cold_temp = std::numeric_limits<double>::quiet_NaN();
				int T_htf_cold_code = c_solver.test_member_function(T_htf_cold_guess, &diff_T_htf_cold_temp);
				if (T_htf_cold_code != 0)
				{	// If failed, go to next mode (CR_ON__PC_SU__TES_OFF)
					m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				C_monotonic_eq_solver::S_xy_pair xy_pair_1;
				xy_pair_1.x = T_htf_cold_guess;		//[C]
				xy_pair_1.y = diff_T_htf_cold_temp;	//[-]

				// Now guess another HTF temperature
				T_htf_cold_guess += 10.0;			//[C]
				T_htf_cold_code = c_solver.test_member_function(T_htf_cold_guess, &diff_T_htf_cold_temp);
				if (T_htf_cold_code != 0)
				{	// If failed, go to next mode (CR_ON__PC_SU__TES_OFF)
					m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = false;
					are_models_converged = false;
					break;
				}

				C_monotonic_eq_solver::S_xy_pair xy_pair_2;
				xy_pair_2.x = T_htf_cold_guess;		//[C]
				xy_pair_2.y = diff_T_htf_cold_temp;	//[-]

				c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

				// Solve for T_htf_cold
				double T_htf_cold_solved, tol_solved;
				T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
				int iter_solved = -1;

				T_htf_cold_code = 0;
				try
				{
					T_htf_cold_code = c_solver.solve(xy_pair_1, xy_pair_2, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception("CR_ON__PC_SU__TES_CH__AUX_OFF solver to converge the HTF cold temperature returned an unexpected exemption"));
				}

				if (T_htf_cold_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_htf_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
					{
						error_msg = util::format("At time = %lg the iteration to find the cold HTF temperature connecting the receiver, power cycle startup, and tes charge only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail = false;
						are_models_converged = false;
						break;
					}
				}

				// Check reported timestep against initial timesteps
				step_pc_su = c_eq.m_step_pc_su;		//[s]
				if (step_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tolerance)
				{
					mc_kernel.mc_sim_info.ms_ts.m_step = step_pc_su;
					mc_kernel.mc_sim_info.ms_ts.m_time = mc_kernel.mc_sim_info.ms_ts.m_time_start + step_pc_su;
				}

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

		double W_dot_net = mc_pc_out_solver.m_P_cycle - 
			mc_cr_out_solver.m_W_dot_col_tracking -
			mc_cr_out_solver.m_W_dot_htf_pump - 
			(mc_pc_out_solver.m_W_dot_htf_pump + mc_tes_outputs.m_W_dot_rhtf_pump) -
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
        if(m_is_tes)
        {
            double mdot_disch, Tdisch;
			mc_tes.discharge_avail_est(m_T_htf_cold_des, mc_kernel.mc_sim_info.ms_ts.m_step, e_tes_disch, mdot_disch, Tdisch);

            e_tes_disch *= mc_kernel.mc_sim_info.ms_ts.m_step / 3600.;  //MWh
        }

		double step_hr = mc_kernel.mc_sim_info.ms_ts.m_step / 3600.0;
		// Save timestep outputs
		// This is after timestep convergence, so be sure convergence() methods don't unexpectedly change outputs
		
			// Simulation outputs
		mv_time_local.push_back(mc_kernel.mc_sim_info.ms_ts.m_time);
		mc_reported_outputs.value(C_solver_outputs::TIME_FINAL, mc_kernel.mc_sim_info.ms_ts.m_time/3600.0);
		mc_reported_outputs.value(C_solver_outputs::MONTH, mc_weather.ms_outputs.m_month);	//[-]
		mc_reported_outputs.value(C_solver_outputs::HOUR_DAY, (int)(m_report_time_end/3600) % 24);	//[hr]


		int n_sub_ts = mv_time_local.size();
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
		

		mc_reported_outputs.value(C_solver_outputs::TOU_PERIOD, tou_period);        //[-]       
		mc_reported_outputs.value(C_solver_outputs::PRICING_MULT, pricing_mult);	//[-] 
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_SB, q_pc_sb);          //[MW]     
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_MIN, q_pc_min);        //[MW]    
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_TARGET, q_pc_target);  //[MW]
		mc_reported_outputs.value(C_solver_outputs::PC_Q_DOT_MAX, q_pc_max);         //[MW]    
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_REC_SU, is_rec_su_allowed);     //[-] 
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_PC_SU, is_pc_su_allowed);       //[-] 
		mc_reported_outputs.value(C_solver_outputs::CTRL_IS_PC_SB, is_pc_sb_allowed);       //[-]  
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CR_SU, is_pc_sb_allowed);     //[-]
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CR_ON, q_dot_cr_on);          //[MWt]
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_DC, q_dot_tes_dc);            //[MWt]    
		mc_reported_outputs.value(C_solver_outputs::EST_Q_DOT_CH, q_dot_tes_ch);            //[MWt]    

		double m_dot_bal = (mc_cr_out_solver.m_m_dot_salt_tot +
							mc_tes_dc_htf_state.m_m_dot -
							mc_pc_inputs.m_m_dot -
							mc_tes_ch_htf_state.m_m_dot) / m_m_dot_pc_des;		//[-]


		double q_dot_bal = (mc_cr_out_solver.m_q_thermal +
							mc_tes_outputs.m_q_dot_dc_to_htf -
							mc_pc_out_solver.m_q_dot_htf -
							mc_tes_outputs.m_q_dot_ch_from_htf) / m_cycle_q_dot_des;	//[-]

		mc_reported_outputs.value(C_solver_outputs::ERR_M_DOT, m_dot_bal);
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
		mc_reported_outputs.value(C_solver_outputs::TES_Q_DOT_LOSS, mc_tes_outputs.m_q_dot_loss);       //[MWt] TES thermal power losses to environment  
		mc_reported_outputs.value(C_solver_outputs::TES_W_DOT_HEATER, mc_tes_outputs.m_q_heater);       //[MWe] Energy into TES from heaters (hot+cold) to maintain tank temperatures
		mc_reported_outputs.value(C_solver_outputs::TES_T_HOT, mc_tes_outputs.m_T_hot_final - 273.15);    //[C] TES hot temperature at end of timestep      
		mc_reported_outputs.value(C_solver_outputs::TES_T_COLD, mc_tes_outputs.m_T_cold_final - 273.15);  //[C] TES cold temperature at end of timestep      
		mc_reported_outputs.value(C_solver_outputs::TES_Q_DOT_DC, mc_tes_outputs.m_q_dot_dc_to_htf);    //[MWt] TES discharge thermal power   
		mc_reported_outputs.value(C_solver_outputs::TES_Q_DOT_CH, mc_tes_outputs.m_q_dot_ch_from_htf);  //[MWt] TES charge thermal power    
		mc_reported_outputs.value(C_solver_outputs::TES_E_CH_STATE, e_tes_disch);                       //[MWht] TES charge state 
		mc_reported_outputs.value(C_solver_outputs::TES_M_DOT_DC, mc_tes_dc_htf_state.m_m_dot);         //[kg/hr] TES mass flow rate discharge   
		mc_reported_outputs.value(C_solver_outputs::TES_M_DOT_CH, mc_tes_ch_htf_state.m_m_dot);         //[kg/hr] TES mass flow rate charge   
			// Parasitics outputs
		mc_reported_outputs.value(C_solver_outputs::COL_W_DOT_TRACK, mc_cr_out_solver.m_W_dot_col_tracking);    //[MWe] Collector tracking, startup, stow power consumption 
		mc_reported_outputs.value(C_solver_outputs::CR_W_DOT_PUMP, mc_cr_out_solver.m_W_dot_htf_pump);          //[MWe] Receiver/tower HTF pumping power   
		mc_reported_outputs.value(C_solver_outputs::SYS_W_DOT_PUMP, (mc_pc_out_solver.m_W_dot_htf_pump + mc_tes_outputs.m_W_dot_rhtf_pump));    //[MWe] TES & PC HTF pumping power (Receiver - PC side HTF)  
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
		int n_op_modes = m_op_mode_tracking.size();
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

				// Overwrite TIME_FINAL
				mc_reported_outputs.overwrite_most_recent_timestep(C_solver_outputs::TIME_FINAL, m_report_time_end / 3600.0);	//[hr]
				mc_reported_outputs.send_to_reporting_ts_array(m_report_time_start, mv_time_local, m_report_time_end);

				// Check if the most recent csp solver timestep aligns with the end of the reporting timestep
				bool delete_last_step = false;
				int pop_back_start = 1;

				int n_time_local = mv_time_local.size();
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

				int n_time_local_refresh = mv_time_local.size();
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

			double blah = 1.23;
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

			double blah = 1.23;
		}
		else
		{
			throw(C_csp_exception("Kernel end time is larger than the baseline end time. This shouldn't happen"));
		}
		
	}	// End timestep loop

}	// End simulate() method


void C_csp_solver::solver_pc_su_controlled__tes_dc(double step_tol /*s*/,
	double &time_pc_su /*s*/, 
	int &exit_mode, double &T_pc_in_exit_tolerance)
{
	// Method to solve scenario where the CR is not interacting with PC +/- TES
	// and the PC is starting up using thermal power from TES

	// Outputs:
	// time_pc_su  [s] time required for power cycle startup
	// exit_mode   [-] E_solver_outcomes
	// T_pc_in_exit_tolerance [-] relative converged tolerance
	
	C_mono_eq_pc_su_cont_tes_dc c_eq(this);
	C_monotonic_eq_solver c_solver(c_eq);

	c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

	double T_htf_hot_guess_hotter = mc_tes.get_hot_temp() - 273.15;	//[C]
	double T_htf_hot_guess_colder = T_htf_hot_guess_hotter - 10.0;	//[C]

	double T_htf_hot_solved, tol_solved;
	T_htf_hot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(T_htf_hot_guess_colder, T_htf_hot_guess_hotter,
								0.0, T_htf_hot_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("solver_pc_su_controlled__tes_dc received an exception from the monotonic equation solver"));
	}

	time_pc_su = c_eq.m_time_pc_su;			//[s]
	T_pc_in_exit_tolerance = tol_solved;	//[-]

	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			error_msg = util::format("At time = %lg the iteration to find the hot HTF temperature connecting the power cycle startup and tes discharge only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
			mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			exit_mode = C_csp_solver::CSP_NO_SOLUTION;
		}
	}
	else
	{
		exit_mode = C_csp_solver::CSP_CONVERGED;
	}

	if (exit_mode == C_csp_solver::CSP_NO_SOLUTION)
	{	// Try fully discharging TES and beginning PC startup
		// Check that power cycle hasn't completely started up, as that suggests an error above (in this mode)

		time_pc_su = mc_kernel.mc_sim_info.ms_ts.m_step;		//[s]

		// Get mass flow rate and temperature at a full discharge
		double m_dot_pc = std::numeric_limits<double>::quiet_NaN();
		double T_pc_in_calc = std::numeric_limits<double>::quiet_NaN();
		mc_tes.discharge_full(mc_kernel.mc_sim_info.ms_ts.m_step, mc_weather.ms_outputs.m_tdry + 273.15, m_T_htf_cold_des, T_pc_in_calc, m_dot_pc, mc_tes_outputs);

		// If not actually charging (i.e. mass flow rate = 0.0), what should the temperatures be?
		mc_tes_ch_htf_state.m_m_dot = 0.0;										//[kg/hr]
		mc_tes_ch_htf_state.m_temp_in = mc_tes_outputs.m_T_hot_ave - 273.15;	//[C] convert from K
		mc_tes_ch_htf_state.m_temp_out = mc_tes_outputs.m_T_cold_ave - 273.15;	//[C] convert from K

		// Set discharge htf state
		mc_tes_dc_htf_state.m_m_dot = m_dot_pc*3600.0;							//[kg/hr]
		mc_tes_dc_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;				//[C] convert from K
		mc_tes_dc_htf_state.m_temp_out = T_pc_in_calc - 273.15;					//[C] convert from K

		// Power Cycle: STARTUP
		mc_pc_htf_state_in.m_temp = T_pc_in_calc - 273.15;			//[C]
		mc_pc_inputs.m_m_dot = m_dot_pc*3600.0;						//[kg/hr] no mass flow rate to power cycle
			// Inputs
		mc_pc_inputs.m_standby_control = C_csp_power_cycle::STARTUP;
			// Performance Call
		mc_power_cycle.call(mc_weather.ms_outputs,
			mc_pc_htf_state_in,
			mc_pc_inputs,
			mc_pc_out_solver,
			mc_kernel.mc_sim_info);

		// Would be nice to have some check to know whether startup solved appropriately...
		// Check for new timestep
		time_pc_su = mc_pc_out_solver.m_time_required_su;		//[s] power cycle model returns MIN(time required to completely startup, full timestep duration)
		if (time_pc_su < mc_kernel.mc_sim_info.ms_ts.m_step - step_tol)
		{	// We fully discharged the TES, so need full timestep here
			exit_mode = C_csp_solver::CSP_NO_SOLUTION;

			T_pc_in_exit_tolerance = std::numeric_limits<double>::quiet_NaN();
		}
		else
		{
			exit_mode = C_csp_solver::CSP_CONVERGED;

			time_pc_su = mc_kernel.mc_sim_info.ms_ts.m_step;
			T_pc_in_exit_tolerance = 0.0;
		}
	}

	return;
}

int C_csp_solver::solver_cr_on__pc_match__tes_full(int pc_mode, double defocus_in)
{
	C_mono_eq_cr_on__pc_match__tes_full c_eq(this, pc_mode, defocus_in);
	C_monotonic_eq_solver c_solver(c_eq);

	// Set up solver
	c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

	// Solve for cold temperature
	double T_cold_guess_low = m_T_htf_pc_cold_est;	//[C]
	double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

	double T_cold_solved, tol_solved;
	T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int T_cold_code = 0;
	try
	{
		T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception(util::format("At time = %lg, C_csp_solver::solver_cr_on__pc_match__tes_full failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
	}

	if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
		{
			std::string msg = util::format("At time = %lg C_csp_solver::solver_cr_on__pc_match__tes_full iteration "
				"failed to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
			mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
		}
		else
		{
			return -1;
		}
	}

	return 0;
}

void C_csp_solver::solver_cr_on__pc_float__tes_full(int power_cycle_mode,
	double field_control_in,
	double tol,
	int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance)
{
	// The collector-receiver is on and delivering hot HTF to the PC and TES
	// The PC accepts floating input
	// The TES is fully charging over the timestep

	throw(C_csp_exception("Retiring solver_cr_on__pc_float__tes_full for mass flow constraint updates..", ""));
}

void C_csp_solver::solver_pc_on_fixed__tes_dc(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode,
	double tol,
	int &T_cold_exit_mode, double &T_cold_exit_tolerance,
	int &q_pc_exit_mode, double &q_pc_exit_tolerance,
	double &q_dot_solved /*MWt*/, double &m_dot_solved /*kg/hr*/)
{
	throw(C_csp_exception("Retiring solver__pc_on_fixed__tes_dc for mass flow constraint updates..", ""));
}

void C_csp_solver::solver_pc_fixed__tes_empty(double q_dot_pc_fixed /*MWt*/,
	double tol,
	double & time_tes_dc,
	int &T_tes_in_exit_mode, double &T_tes_in_exit_tolerance,
	int &q_pc_exit_mode, double &q_pc_exit_tolerance)
{

	// CR is either off or in startup. It's flow (if applicable) is not connected to TES or PC
	// The power cycle is controlled for a fixed q_dot_in
	// TES discharges until it is depleted
	// A new, shorter timestep is calculated if solver is successful

	C_mono_eq_pc_target_tes_empty__T_cold c_eq(this, q_dot_pc_fixed);
	C_monotonic_eq_solver c_solver(c_eq);

	// Set up solver
	c_solver.settings(1.E-3, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

	// Solve for cold HTF temperature
	double T_cold_guess_low = m_T_htf_pc_cold_est;			//[C]
	double T_cold_guess_high = T_cold_guess_low + 10.0;		//[C]

	double T_cold_solved, tol_solved;
	T_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int T_cold_code = 0;
	try
	{
		T_cold_code = c_solver.solve(T_cold_guess_low, T_cold_guess_high, 0.0, T_cold_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception(util::format("At time = %lg, C_csp_solver::solver_pc_fixed__tes_empty failed", mc_kernel.mc_sim_info.ms_ts.m_time), ""));
	}

	if (T_cold_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (T_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
		{
			std::string msg = util::format("At time = %lg C_csp_solver::solver_pc_fixed__tes_empty iteration "
				"to find the cold HTF temperature to balance energy between TES and PC target only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
			mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
		}
		else
		{
			std::string msg = util::format("At time = %lg C_csp_solver::solver_cr_on__pc_fixed__tes_empty iteration "
				"to find the cold HTF temperature to balance energy between the CR, TES, and PC failed",
				mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0);
			throw(C_csp_exception(msg, ""));
		}
	}

	time_tes_dc = c_eq.m_step;	//[s]

	return;
}

void C_csp_solver::solver_cr_on__pc_fixed__tes_dc(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode,
	double field_control_in,
	double tol,
	int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance,
	int &q_pc_exit_mode, double &q_pc_exit_tolerance)
{
	throw(C_csp_exception("Retiring solver_cr_on__pc_fixed__tes_dc for mass flow constraint updates..", ""));	
}

void C_csp_solver::solver_cr_on__pc_fixed__tes_ch(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode,
	double field_control_in, 
	double tol, 
	int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance,
	int &q_pc_exit_mode, double &q_pc_exit_tolerance)
{
	// CR in on
	// PC is controlled for a fixed q_dot_in
	// excess CR output is charging TES
	
	throw(C_csp_exception("Retiring solver_cr_on__pc_fixed__tes_ch for mass flow constraint updates..", ""));
}

void C_csp_solver::solver_cr_to_pc_to_cr(int pc_mode, double field_control_in, double tol, int &exit_mode, double &exit_tolerance)
{
	// Method to solve scenario where the CR is on (under some fixed operating conditions, i.e. defocus)
	// and the PC is on. No TES or AUX, so the output of the CR connects directly to the PC

	// Ouputs:
	// int exit_mode: E_solver_outcomes 
	
	C_mono_eq_cr_to_pc_to_cr c_eq(this, pc_mode, m_P_cold_des, -1, field_control_in);
	C_monotonic_eq_solver c_solver(c_eq);

	c_solver.settings(tol, 50, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), false);

	double T_htf_cold_guess_colder = m_T_htf_cold_des - 273.15;			//[C], convert from [K]
	double T_htf_cold_guess_warmer = T_htf_cold_guess_colder + 10.0;	//[C]

	double T_htf_cold_solved, tol_solved;
	T_htf_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(T_htf_cold_guess_colder, T_htf_cold_guess_warmer, 0.0, T_htf_cold_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("solver_cr_to_pc_to_cr received exception from mono equation solver"));
	}

	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			error_msg = util::format("At time = %lg the iteration to find the cold HTF temperature connecting the power cycle and receiver only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
			mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			exit_mode = C_csp_solver::CSP_NO_SOLUTION;
			return;
		}		
	}

	exit_mode = C_csp_solver::CSP_CONVERGED;
	exit_tolerance = tol_solved;

	return;
}

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
