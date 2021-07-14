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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
//#include <vector>
#include "CO2_properties.h"

#include "heat_exchangers.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"

#include "nlopt.hpp"
#include "nlopt_callbacks.h"

using namespace std;

enum{	//Parameters
	P_W_dot_net_des,
	P_eta_c,
	P_eta_t,
	P_P_high_limit,
	P_DELTAT_PHX,

	P_DELTAT_ACC,
	P_T_AMB_DES,
	P_FAN_POWER_PERC,
	P_PLANT_ELEVATION,

	P_T_htf_hot,
	P_T_htf_cold_est,
	P_eta_des,
	P_rec_fl,
	P_rec_fl_props,

	P_STARTUP_TIME,
	P_STARTUP_FRAC,
	P_Q_SBY_FRAC,
	P_cycle_cutoff_frac,

	//Inputs
	I_T_HTF_HOT,
	I_M_DOT_HTF,
	I_STANDBY_CONTROL,
	I_T_DB,
	I_P_AMB,

	//Outputs
	O_ETA_CYCLE_DES,
	O_P_LOW_DES,
	O_P_HIGH_DES,
	O_F_RECOMP_DES,
	O_UA_RECUP_DES,
	O_UA_PHX_DES,
	O_T_COOLER_IN_DES,
	O_COOLER_VOLUME,

	O_P_CYCLE,
	O_ETA,
	O_T_HTF_COLD,
	O_M_DOT_MAKEUP,
	O_M_DOT_DEMAND,
	O_M_DOT_HTF_REF,
	O_W_COOL_PAR,
	O_F_BAYS,
	O_P_COND,

	O_Q_STARTUP,

	O_T_HTF_COLD_DES,

	O_T_TURBINE_IN,
	O_P_MC_IN,
	O_P_MC_OUT,
	O_F_RECOMP,
	O_N_MC,

	// O_W_DOT_NET,
	// O_T_MC_IN,
	// O_T_T_IN,
	// O_P_MC_IN,
	// O_P_MC_OUT,
	// O_UA_LT,
	// O_UA_HT,
	// O_RECOMP_FRAC,
	// O_ETA_MC,
	// O_ETA_RC,
	// O_ETA_T,
	// O_N_SUB_HXRS,
	// O_P_HIGH_LIMIT,
	// O_N_turbine,
	// O_DP_LT_C,
	// O_DP_LT_H,
	// O_DP_HT_C,
	// O_DP_HT_H,
	// O_DP_PC_H,
	// O_DP_PHX_C,
	// O_DELTAT_MC,
	// O_DELTAT_T,

	//N_MAX
	N_MAX
};

tcsvarinfo sam_sco2_recomp_type424_variables[] = {
	//PARAMETERS
		// Cycle Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_W_dot_net_des,  "W_dot_net_des",   "Design cycle power output",                      "MW",   "", "", "" },	
	{ TCS_PARAM, TCS_NUMBER, P_eta_c,          "eta_c",           "Design compressor(s) isentropic efficiency",     "-",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_t,          "eta_t",           "Design turbine isentropic efficiency",           "-",    "", "", "" },	
	{ TCS_PARAM, TCS_NUMBER, P_P_high_limit,   "P_high_limit",    "High pressure limit in cycle",                   "MPa",  "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_DELTAT_PHX,     "deltaT_PHX",      "Temp diff btw hot HTF and turbine inlet",        "C",    "", "", "" },
		// Air-cooler Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_DELTAT_ACC,     "deltaT_ACC",      "Temp diff btw ambient air and compressor inlet", "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_AMB_DES,      "T_amb_des",       "Design: Ambient temperature for air cooler",     "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_FAN_POWER_PERC, "fan_power_perc",  "Percent of net cycle power used for fan",        "%",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_PLANT_ELEVATION,"plant_elevation", "Plant Elevation",                                "m",    "", "", "" },
		// Solar Receiver Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_hot,      "T_htf_hot_des",   "Tower design outlet temp",                       "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_cold_est, "T_htf_cold_est",  "Estimated tower design inlet temp",              "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_des,        "eta_des",         "Power cycle thermal efficiency",                 "",     "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_fl,         "rec_htf",         "The name of the HTF used in the receiver",       "",     "", "", "" },
	{ TCS_PARAM, TCS_MATRIX, P_rec_fl_props,   "field_fl_props",  "User defined rec fluid property data",           "-", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "" },
		// Cycle Controller Parameters
	{ TCS_PARAM, TCS_NUMBER, P_STARTUP_TIME,   "startup_time",    "Time needed for power block startup",                   "hr",   "", "", "0.5" },
	{ TCS_PARAM, TCS_NUMBER, P_STARTUP_FRAC,   "startup_frac",    "Fraction of design thermal power needed for startup",   "none", "", "", "0.2" },
	{ TCS_PARAM, TCS_NUMBER, P_Q_SBY_FRAC,     "q_sby_frac",      "Fraction of thermal power required for standby mode",   "none", "", "", "0.2" },
	{ TCS_PARAM, TCS_NUMBER, P_cycle_cutoff_frac, "cycle_cutoff_frac", "Minimum turbine operation fraction before shutdown","-",   "", "", "" },

	//INPUTS
	{ TCS_INPUT, TCS_NUMBER, I_T_HTF_HOT,      "T_htf_hot",       "Hot HTF inlet temperature, from storage tank",   "C",    "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_M_DOT_HTF,      "m_dot_htf",       "HTF mass flow rate",                             "kg/hr","", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_STANDBY_CONTROL,"standby_control", "Control signal indicating standby mode",         "none", "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_DB,           "T_db",            "Ambient dry bulb temperature",                   "C",    "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_P_AMB,          "P_amb",           "Ambient pressure",                               "mbar", "", "", "" },

	//OUTPUTS
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_CYCLE_DES,   "eta_cycle_des",       "Design: Power cycle efficiency",           "%",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_LOW_DES,       "P_low_des",           "Design: Compressor inlet pressure",        "kPa",  "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_HIGH_DES,      "P_high_des",          "Design: Comp. outlet pressure",            "kPa",  "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_RECOMP_DES,    "f_recomp_des",        "Design: Recompression fraction",           "-",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_RECUP_DES,    "UA_recup_des",        "Design: Recuperator conductance UA",       "kW/K", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_PHX_DES,      "UA_PHX_des",          "Design: PHX conductance (UA)",             "kW/K", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_COOLER_IN_DES, "T_cooler_in_des",     "Design: Cooler CO2 inlet temp",            "C",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_COOLER_VOLUME,   "cooler_volume",       "Estimated required cooler material vol.",  "m^3",  "", "", "" },

	//OUTPUTS TO MATCH SSC REQUIRED OUTPUTS FOR MOLTEN-SALT POWER TOWER
	{ TCS_OUTPUT, TCS_NUMBER, O_P_CYCLE,         "P_cycle",             "Cycle power output",                                    "MWe",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA,             "eta",                 "Cycle thermal efficiency",                              "none",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_HTF_COLD,      "T_htf_cold",          "Heat transfer fluid outlet temperature ",               "C",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_MAKEUP,    "m_dot_makeup",        "Cooling water makeup flow rate",                        "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_DEMAND,    "m_dot_demand",        "HTF required flow rate to meet power load",             "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_HTF_REF,   "m_dot_htf_ref",       "Calculated reference HTF flow rate at design",          "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_W_COOL_PAR,      "W_cool_par",          "Cooling system parasitic load",                         "MWe",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_BAYS,          "f_bays",              "Fraction of operating heat rejection bays",             "none",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_COND,          "P_cond",              "Condenser pressure",                                    "Pa",    "",  "",  "" },
	
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_STARTUP,       "q_pc_startup",        "Power cycle startup energy",                            "MWt-hr","",  "",  "" },

	//OUTPUT SENT TO CONTROLLER TO GIVE NEW RECEIVER INLET TEMPERATURE AT DESIGN
	{ TCS_OUTPUT, TCS_NUMBER, O_T_HTF_COLD_DES,  "o_T_htf_cold_des",    "Calculated htf cold temperature at design",             "C",     "",  "",  "" },

	// Other interesting outputs: they are only used as outputs: need to reconsider if used by other types in performance code
	{ TCS_OUTPUT, TCS_NUMBER, O_T_TURBINE_IN,    "T_turbine_in",        "Turbine inlet temperature",                             "C",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_IN,         "P_mc_in",             "Main compressor inlet pressure",                        "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_OUT,        "P_mc_out",            "Main compressor outlet pressure",                       "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_RECOMP,        "f_recomp",            "Recompression fraction",                                "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_N_MC,            "N_MC",                "Main comp. shaft speed",                                "rpm",   "",  "",  "" },

	// Cycle Design Parameters to pass to controller
	//{ TCS_OUTPUT, TCS_NUMBER, O_W_DOT_NET,       "o_W_dot_net",         "Target net cycle power",                                "kW",    "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_T_MC_IN,         "o_T_mc_in",           "Compressor inlet temperature",                          "K",     "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_T_T_IN,          "o_T_t_in",            "Turbine inlet temperature",                             "K",     "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_IN,         "o_P_mc_in",           "Compressor inlet pressure",                             "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_OUT,        "o_P_mc_out",          "Compressor outlet pressure",                            "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_UA_LT,           "o_UA_LT",             "UA in LTR",                                             "kW/K",  "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_UA_HT,           "o_UA_HT",             "UA in HTR",                                             "kW/K",  "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_RECOMP_FRAC,     "o_recomp_frac",       "recompresson fraction",                                 "",      "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_ETA_MC,          "o_eta_mc",            "main compressor isentropic efficiency",                 "",      "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_ETA_RC,          "o_eta_rc",            "re-compressor isentropic efficiency",                   "",      "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_ETA_T,           "o_eta_t",             "turbine isentropic efficiency",                         "",      "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_N_SUB_HXRS,      "o_N_sub_hxrs",        "number of sub heat exchangers",                         "",      "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_P_HIGH_LIMIT,    "o_P_high_limit",      "high pressure limit",                                   "MPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_N_turbine,       "o_N_turbine",         "Turbine shaft speed",                                   "rpm",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_LT_C,         "o_DP_LT_c",           "Cold-side pressure drop - LT recup",                    "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_LT_H,         "o_DP_LT_h",           "Hot-side pressure drop - LT recup",                     "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_HT_C,         "o_DP_HT_c",           "Cold-side pressure drop - HT recup",                    "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_HT_H,         "o_DP_HT_h",           "Hot-side pressure drop - HT recup",                     "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_PC_H,         "o_DP_PC_h",           "Hot-side pressure drop - pre-cooler",                   "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DP_PHX_C,        "o_DP_PHX_c",          "Cold-side pressure drop - PHX",                         "kPa",   "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DELTAT_MC,       "o_deltaT_mc",         "Temperature difference btw comp inlet and Tamb",        "K",     "",  "",  "" },
	//{ TCS_OUTPUT, TCS_NUMBER, O_DELTAT_T,        "o_deltaT_t",          "Temperature difference btw hot HTF and turbine inlet",  "K",     "",  "",  "" },

	/*
	double m_W_dot_net;					//[kW] Target net cycle power
	double m_T_mc_in;					//[K] Compressor inlet temperature
	double m_T_t_in;					//[K] Turbine inlet temperature
	double m_P_mc_in;					//[kPa] Compressor inlet pressure
	double m_P_mc_out;					//[kPa] Compressor outlet pressure
	std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	double m_UA_LT;						//[kW/K] UA in LTR
	double m_UA_HT;						//[kW/K] UA in HTR
	double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
	double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
	double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
	double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
	int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
	double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
	double m_tol;						//[-] Convergence tolerance
	double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	*/


	//N_MAX
	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};


class sam_sco2_recomp_type424 : public tcstypeinterface
{
private:
	// Classes and Structures
		// Old sco2 cycle code
	// cycle_design_parameters rc_des_par;
	// RecompCycle * rc_cycle;
		// New sco2 cycle code
	C_RecompCycle ms_rc_cycle;
	C_sco2_cycle_core::S_auto_opt_design_hit_eta_parameters ms_rc_autodes_hit_eta_par;
	//C_RecompCycle::S_auto_opt_design_parameters ms_rc_autodes_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_opt_od_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_max_opt_od_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_des_opt_od_par;
	C_RecompCycle::S_od_parameters ms_rc_od_par;
	C_RecompCycle::S_PHX_od_parameters ms_phx_od_par;

	HTFProperties rec_htfProps;		// Instance of HTFProperties class for field HTF
	CO2_state co2_props;
	C_CO2_to_air_cooler ACC;
	int co2_error;
	
	// Cycle Design Parameters
	double m_W_dot_net_des;            // "Design cycle power output",                      "MW",  
	double m_T_mc_in_des;			   // "Main compressor inlet temp at design",           "C",   
	double m_T_t_in_des;			   // "Turbine inlet temp at design",                   "C",   
	double m_N_t_des;				   // "Design turbine speed, negative links to comp.",  "rpm", 
	double m_eta_c;					   // "Design compressor(s) isentropic efficiency",     "-",   
	double m_eta_t;					   // "Design turbine isentropic efficiency",           "-",   	
	double m_P_high_limit;			   // "High pressure limit in cycle",                   "MPa", 

	// Hardcoded Cycle Design Parameters
	double m_tol;                      // "Convergence tolerance for performance calcs", "-", "",
	double m_opt_tol;				   // "Convergence tolerance - optimization calcs", "-", "", 
	vector<double> m_DP_LT;            // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_HT;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PC;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PHX;		   // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	int m_N_sub_hxrs;                  // [-] Number of sections to model in heat exchangers
	double m_deltaP_cooler_frac;       // [-] Fraction of high side pressure that is allowed as pressure drop to design the ACC
	double m_q_max_sf;

	// Calculated Cycle Design Parameters
	double m_UA_total_des;			   // "Total UA allocatable to recuperators",           "kW/K",
	//double m_T_PHX_in;                 // [K] CO2 cold inlet to Primary Heat Exchanger
	double m_delta_T_acc;			   // [K/C] Temperature difference between compressor inlet temp and ambient
	double m_delta_T_t;				   // [K/C] Temperature difference between htf hot side and turbine inlet
	double m_m_dot_des;			       // [kg/s] CO2 mass flow rat thru cycle at design
	double m_cp_rec;                   // [kJ/kg-K] Specific heat of htf
	double m_UA_PHX_des;		       // [kW/K] PHX Conductance
	double m_T_htf_cold_sby;		   // [K] HTF cold return temperature for standby mode
	double m_m_dot_htf_sby;			   // [kg/s] 
	double m_W_dot_fan_des;            // [kW]
	double m_eta_thermal_des;		   // [-] Power cycle design thermal efficiency
	double m_T_htf_cold_des;           // [K]

	// Solar Receiver Design Parameters
	double m_T_htf_hot;              // [K] Tower design outlet temperature
	//double m_T_htf_cold;             // [K] Tower design inlet temperature	
	double m_Q_dot_rec_des;			// [MWt] Receiver design thermal input

	// Calculated Receiver Design Parameters
	double m_dot_rec_des;				// [kg/s] Receiver design point mass flow rate

	// Cycle Control Parameters
	double m_startup_time;			//[hr] Time needed for power block startup
	double m_startup_frac;			//[-] Fraction of design thermal power needed for startup
	double m_startup_energy;		//[kWt] Startup energy
	double m_q_sby_frac;			//[-] Fraction of thermal power required for standby mode

	// Stored Variables
	int    m_standby_control_prev;
	int    m_standby_control;
	double m_time_su_prev;
	double m_time_su;
	double m_E_su_prev;
	double m_E_su;
	int    m_error_message_code;

	bool   m_is_first_t_call;

	double m_q_dot_cycle_max;
	double m_P_mc_in_q_max;

	double m_ncall;

public:
	sam_sco2_recomp_type424(tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface(cst, ti)
	{
		// Pointers
		// rc_cycle = NULL;

		// Parameters
		m_W_dot_net_des = std::numeric_limits<double>::quiet_NaN();
		m_T_mc_in_des = std::numeric_limits<double>::quiet_NaN();
		m_T_t_in_des = std::numeric_limits<double>::quiet_NaN();
		m_N_t_des = std::numeric_limits<double>::quiet_NaN();
		m_eta_c = std::numeric_limits<double>::quiet_NaN();
		m_eta_t = std::numeric_limits<double>::quiet_NaN();		
		m_P_high_limit = std::numeric_limits<double>::quiet_NaN();

		// Hardcoded values
		m_tol = 1.E-3;
		m_opt_tol = m_tol;
		m_DP_LT.resize(2);
		fill(m_DP_LT.begin(), m_DP_LT.end(), 0.0);
		m_DP_HT.resize(2);
		fill(m_DP_HT.begin(), m_DP_HT.end(), 0.0);
		m_DP_PC.resize(2);
		fill(m_DP_PC.begin(), m_DP_PC.end(), 0.0);
		m_DP_PHX.resize(2);
		fill(m_DP_PHX.begin(), m_DP_PHX.end(), 0.0);
		m_N_sub_hxrs = 10;
		m_deltaP_cooler_frac = 0.002;
		m_N_t_des = 3600.0;
		m_q_max_sf = 0.97;			// Safety factor for optimizer: can safely get with m_q_max_sf * q_max_calculated

		// Calculated Cycle Design Parameters
		m_UA_total_des = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_acc = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_t = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_des = std::numeric_limits<double>::quiet_NaN();
		m_cp_rec = std::numeric_limits<double>::quiet_NaN();
		m_T_htf_cold_sby = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_htf_sby = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_fan_des = std::numeric_limits<double>::quiet_NaN();
		m_eta_thermal_des = std::numeric_limits<double>::quiet_NaN();
		m_T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();

		// Solar Receiver Design Parameters
		m_T_htf_hot = std::numeric_limits<double>::quiet_NaN();
		//m_T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_rec_des = std::numeric_limits<double>::quiet_NaN();

		// Calculated Receiver Design Parameters
		m_dot_rec_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_PHX_des = std::numeric_limits<double>::quiet_NaN();

		// Cycle Control Parameters
		m_startup_time = numeric_limits<double>::quiet_NaN();
		m_startup_frac = numeric_limits<double>::quiet_NaN();
		m_startup_energy = numeric_limits<double>::quiet_NaN();
		m_q_sby_frac = numeric_limits<double>::quiet_NaN();

		// Stored Variables
		m_standby_control_prev = -1;
		m_standby_control = -1;
		m_time_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_time_su = std::numeric_limits<double>::quiet_NaN();
		m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_E_su = std::numeric_limits<double>::quiet_NaN();

		m_is_first_t_call = true;

		m_q_dot_cycle_max = std::numeric_limits<double>::quiet_NaN();
		m_P_mc_in_q_max = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_sco2_recomp_type424(){}

	virtual int init()
	{
		// Get Parameters
		m_W_dot_net_des = value(P_W_dot_net_des)*1000.0;		//[kW] Design cycle power outpt 
		m_eta_c = value(P_eta_c);								// "Design compressor(s) isentropic efficiency",     "-",    
		m_eta_t = value(P_eta_t);								// "Design turbine isentropic efficiency",           "-",    		
		m_P_high_limit = value(P_P_high_limit)*1.E3;			//[kPa] High pressure limit in cycle, convert from MPa
		m_delta_T_t = value(P_DELTAT_PHX);						//[C] temperature difference between hot htf and sco2 turbine inlet
		
		// Air-cooler specific parameters
		m_delta_T_acc = value(P_DELTAT_ACC);					//[C] temperature difference between ambient air and compressor inlet
		double T_amb_cycle_des = value(P_T_AMB_DES) + 273.15;		//[K] Ambient temperature at power cycle design, convert from C
		double fan_power_frac = value(P_FAN_POWER_PERC) / 100.0;			//[-] Fraction of cycle net power output used by cooler air fan, convert from %
		
		// Solar Receiver Parameters
		// Receiver inlet/outlet temps and thermal input
		m_T_htf_hot = value(P_T_htf_hot) + 273.15;		//[K] Tower outlet temp at design, convert from C
		m_eta_thermal_des = value(P_eta_des);
		
		// ***********************************************************
		// ***********************************************************
		// ***********************************************************

		m_Q_dot_rec_des = m_W_dot_net_des / m_eta_thermal_des;		//[kWt] Receiver thermal input at design

		// Calculate other cycle design parameters based on User Parameters
		m_T_mc_in_des = T_amb_cycle_des + m_delta_T_acc;	//[K] Compressor inlet temperature
		m_T_t_in_des = m_T_htf_hot - m_delta_T_t;			//[K] Turbine inlet temperature		

		//double P_amb_cycle_des = 101325.0*pow(1 - 2.25577E-5*value(P_PLANT_ELEVATION), 5.25588);	//[Pa] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html	

		// ******************************************************************************************
		// ******************************************************************************************
		// Define error and warning message strings
		std::string error_msg;
		int auto_err_code = 0;
		
		// Setup recompressoin cycle autodes parameter structure
		ms_rc_autodes_hit_eta_par.m_W_dot_net = m_W_dot_net_des;		//[kW]
		ms_rc_autodes_hit_eta_par.m_eta_thermal = m_eta_thermal_des;	//[-]
		ms_rc_autodes_hit_eta_par.m_T_mc_in = m_T_mc_in_des;			//[K]
		ms_rc_autodes_hit_eta_par.m_T_t_in = m_T_t_in_des;				//[K]
		ms_rc_autodes_hit_eta_par.m_DP_LT = m_DP_LT;
		ms_rc_autodes_hit_eta_par.m_DP_HT = m_DP_HT;
		ms_rc_autodes_hit_eta_par.m_DP_PC_main = m_DP_PC;
		ms_rc_autodes_hit_eta_par.m_DP_PHX = m_DP_PHX;
		ms_rc_autodes_hit_eta_par.m_eta_mc = m_eta_c;
		ms_rc_autodes_hit_eta_par.m_eta_rc = m_eta_c;
		ms_rc_autodes_hit_eta_par.m_eta_t = m_eta_t;
        ms_rc_autodes_hit_eta_par.m_LTR_N_sub_hxrs = m_N_sub_hxrs;      //[-]
        ms_rc_autodes_hit_eta_par.m_HTR_N_sub_hxrs = m_N_sub_hxrs;      //[-]
		ms_rc_autodes_hit_eta_par.m_P_high_limit = m_P_high_limit;		//[kPa]
		ms_rc_autodes_hit_eta_par.m_des_tol = m_tol;
		ms_rc_autodes_hit_eta_par.m_des_opt_tol = m_opt_tol;
		ms_rc_autodes_hit_eta_par.m_N_turbine = m_N_t_des;
		ms_rc_autodes_hit_eta_par.m_is_recomp_ok = 1;

		auto_err_code = ms_rc_cycle.auto_opt_design_hit_eta(ms_rc_autodes_hit_eta_par, error_msg);

		if(auto_err_code != 0)
		{
			message(TCS_ERROR, error_msg.c_str());
			return -1;
		}

		if( error_msg.empty() )
		{
			message(TCS_NOTICE, "sCO2 cycle design optimization was successful");
		}
		else
		{
			string out_msg = "The sCO2 cycle design optimization solved with the following warning(s):\n" + error_msg;
			message(TCS_NOTICE, out_msg.c_str());
		}

		// Check air-cooler parameters are within limits
			// Ambient temperature must be cooler than compressor inlet temperature
		if(T_amb_cycle_des > m_T_mc_in_des - 2.0)
		{
			message(TCS_ERROR, "The ambient temperature used for the air cooler design, %lg [C], must be 2 [C] less than the specified compressor inlet temperature %lg [C]", T_amb_cycle_des-273.15, m_T_mc_in_des-273.15);
			return -1;
		}
			// Also need some "reasonable" lower limit on the ambient temperature
		if(T_amb_cycle_des < 273.15)
		{
			message(TCS_WARNING,"The ambient temperature used for the air cooler design, %lg [C], was reset to 0 [C] to improve solution stability", T_amb_cycle_des-273.15);
			T_amb_cycle_des = 273.15;
		}
			// Set an upper limit on the fraction of cycle net power allocated to the fan on the air cooler
		double fan_power_frac_max = 0.1;
		if(fan_power_frac > fan_power_frac_max)
		{
			message(TCS_ERROR, "The fraction of cycle net power used by the cooling fan, %lg, is greater than the internal maximum %lg", fan_power_frac, fan_power_frac_max);
			return -1;
		}
		double fan_power_frac_min = 0.001;
		if(fan_power_frac < fan_power_frac_min)
		{
			message(TCS_ERROR, "The fraction of cycle net power used by the cooling fan, %lg, is less than the internal minimum %lg", fan_power_frac, fan_power_frac_min);
			return -1;
		}

		// Declare instance of fluid class for FIELD fluid.
		int rec_fl = (int)value(P_rec_fl);
		if( rec_fl != HTFProperties::User_defined && rec_fl < HTFProperties::End_Library_Fluids )
		{
			if( !rec_htfProps.SetFluid(rec_fl) )	// field_fl should match up with the constants
			{
				message(TCS_ERROR, "Receiver HTF code is not recognized");
				return -1;
			}
		}
		else if( rec_fl == HTFProperties::User_defined )
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value(P_rec_fl_props, &nrows, &ncols);
			if( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat(nrows, ncols, 0.0);
				for( int r = 0; r < nrows; r++ )
				for( int c = 0; c < ncols; c++ )
					mat.at(r, c) = TCS_MATRIX_INDEX(var(P_rec_fl_props), r, c);

				if( !rec_htfProps.SetUserDefinedFluid(mat) )
				{
					//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
					message(TCS_ERROR, rec_htfProps.UserFluidErrMessage(), nrows, ncols);
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}
		else
		{
			message(TCS_ERROR, "Receiver HTF code is not recognized");
			return -1;
		}

		// Design metrics
		double design_eta = ms_rc_cycle.get_design_solved()->m_eta_thermal;
		//double W_dot_net_des_calc = ms_rc_cycle.get_design_solved()->m_W_dot_net;		// Can compare to target net output to check convergence
		m_m_dot_des = ms_rc_cycle.get_design_solved()->m_m_dot_t;			//[kg/s]
		//double P_PHX_out = ms_rc_cycle.get_design_solved()->m_pres[6-1];		//[kPa]
		double P_PHX_in = ms_rc_cycle.get_design_solved()->m_pres[5 - 1];			//[kPa]

		double T_htf_cold_est = value(P_T_htf_cold_est) + 273.15;
		// Calculate HTF cold temperature

		double T_PHX_co2_in = ms_rc_cycle.get_design_solved()->m_temp[5 - 1];
		double T_htf_cold = T_PHX_co2_in + m_delta_T_t;

		// Can give PHX inlet --or-- HX UA, but there is going to be a conflict between
		// 1) Assumed thermal input to cycle
		// -- and --
		// 2) Calculated thermal input to cycle: W_dot_net/eta_thermal
		// 
		// So, solar multiple in UI will also depend on estimating the cycle thermal efficiency
		// Should also revisit UA guess assumptions, but probably still OK

		// ***************************************************************
		// Calculate Design UA of PHX
		// Assumes properties behave well here, which is reasonable given distance from critical point
		// Also assumes that CR = 1
		// ***************************************************************
		// Receiver/hot side
		double T_rec_ave = 0.5*(T_htf_cold + m_T_htf_hot);		//[K]
		m_cp_rec = rec_htfProps.Cp(T_rec_ave);					//[kJ/kg-K]
		m_dot_rec_des = m_Q_dot_rec_des / (m_cp_rec*(m_T_htf_hot - T_htf_cold));	//[kg/s]

		// Cycle/cold side
		double T_PHX_co2_ave = 0.5*(m_T_t_in_des + T_PHX_co2_in);		//[K]
		co2_error = CO2_TP(T_PHX_co2_ave, P_PHX_in, &co2_props);
		//double cp_PHX_co2 = m_Q_dot_rec_des / (m_m_dot_des*(m_T_t_in_des - T_PHX_co2_in));

		// Because C_dot_c = C_dot_h, q_dot_max = 
		double q_dot_max = m_dot_rec_des*m_cp_rec*(m_T_htf_hot - T_PHX_co2_in);		//[kW]

		// Effectiveness & NTU
		double eff_des = m_Q_dot_rec_des / q_dot_max;
		double NTU = eff_des / (1.0 - eff_des);

		// UA
		m_UA_PHX_des = NTU * m_dot_rec_des * m_cp_rec;

		m_T_htf_cold_des = T_htf_cold;		//[K]

		message(TCS_WARNING, "The calculated cold HTF temperature is %lg [C]. The estimated cold HTF temperature is %lg [C]. This difference may affect the receiver design and hours of thermal storage. Try adjusting the receiver inlet temperature or design cycle efficiency",
			T_htf_cold - 273.15, T_htf_cold_est - 273.15);


		value(O_T_HTF_COLD_DES, m_T_htf_cold_des - 273.15);

		// *****************************************************************
		// Call Air Cooled Condenser
		// *****************************************************************		
		C_CO2_to_air_cooler::S_des_par_ind acc_des_par_ind;
		acc_des_par_ind.m_T_amb_des = T_amb_cycle_des;			//[K]
		acc_des_par_ind.m_elev = value(P_PLANT_ELEVATION);			//[Pa]
		//acc_des_par_ind.m_frac_fan_power = fan_power_frac;		//[-]
		//acc_des_par_ind.m_W_dot_fan_des = fan_power_frac*m_W_dot_net_des/1000.0;	//[MW] Cooler air fan power at design
		C_CO2_to_air_cooler::S_des_par_cycle_dep acc_des_par_cycle_dep;
		acc_des_par_cycle_dep.m_T_hot_in_des = ms_rc_cycle.get_design_solved()->m_temp[9 - 1];	//[K]
		acc_des_par_cycle_dep.m_P_hot_in_des = ms_rc_cycle.get_design_solved()->m_pres[9 - 1];	//[kPa]
		acc_des_par_cycle_dep.m_m_dot_total = ms_rc_cycle.get_design_solved()->m_m_dot_mc;		//[kg/s]
		acc_des_par_cycle_dep.m_delta_P_des = m_deltaP_cooler_frac*ms_rc_cycle.get_design_solved()->m_pres[2 - 1];		//[kPa]
		acc_des_par_cycle_dep.m_T_hot_out_des = m_T_mc_in_des;	//[K]
		acc_des_par_cycle_dep.m_W_dot_fan_des = fan_power_frac*m_W_dot_fan_des/1000.0;	//[MW] Cooler air fan power at design
		ACC.design_hx(acc_des_par_ind, acc_des_par_cycle_dep, 1.E-3);

		// Get air-cooler design parameters
		// compact_hx::S_hx_design_solved s_hx_design_solved;
		
		// Write outputs
		value(O_ETA_CYCLE_DES, design_eta);
		value(O_P_LOW_DES, ms_rc_cycle.get_design_solved()->m_pres[1-1]);
		value(O_P_HIGH_DES, ms_rc_cycle.get_design_solved()->m_pres[2-1]);
		value(O_F_RECOMP_DES, ms_rc_cycle.get_design_solved()->m_recomp_frac);
		value(O_UA_RECUP_DES, m_UA_total_des);
		value(O_UA_PHX_DES, m_UA_PHX_des);
		value(O_T_COOLER_IN_DES, acc_des_par_cycle_dep.m_T_hot_in_des - 273.15);	//[C]
		value(O_COOLER_VOLUME, ACC.get_total_hx_volume());

		m_startup_time = value(P_STARTUP_TIME);		//[hr] Time needed for power block startup
		m_startup_frac = value(P_STARTUP_FRAC);		//[-] Fraction of design thermal power needed for startup
		m_q_sby_frac	= value( P_Q_SBY_FRAC );	//[-] Fraction of thermal power required for standby mode	
		double cutoff_frac = value(P_cycle_cutoff_frac);	//[-] Minimum fraction of thermal power at which power cycle operates in part load (can still go standby)

		// Calculate the startup energy needed
		m_startup_energy = m_startup_frac*m_W_dot_net_des / design_eta;		//[kWt - hr]

		// Initialize stored variables
		m_standby_control_prev = 3;
		m_time_su_prev = m_startup_time;
		m_E_su_prev = m_startup_energy;

		m_time_su = m_time_su_prev;
		m_E_su = m_E_su_prev;
		m_standby_control = m_standby_control_prev;

		m_error_message_code = 0;

		int q_sby_error_code = 0;

		ms_rc_od_par.m_T_mc_in = m_T_mc_in_des;
		ms_rc_od_par.m_T_t_in = m_T_t_in_des;
		ms_rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->ms_t_des_solved.m_N_design;
		ms_rc_od_par.m_tol = ms_rc_autodes_hit_eta_par.m_des_tol;

		ms_phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
		ms_phx_od_par.m_T_htf_hot = m_T_htf_hot;
		ms_phx_od_par.m_m_dot_htf = m_dot_rec_des*cutoff_frac;
		ms_phx_od_par.m_T_htf_cold = m_T_htf_cold_des;
		ms_phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
		ms_phx_od_par.m_cp_htf = m_cp_rec;		

		if( q_sby_error_code != 0 )
		{
			message(TCS_ERROR, "The power cycle model crashes at the specified cutoff fraction, %lg. Try increasing this value", cutoff_frac);
			return -1;
		}

		double m_T_PHX_in_sby = ms_rc_cycle.get_od_solved()->m_temp[5 - 1];

		m_T_htf_cold_sby = m_T_PHX_in_sby + 5.0;		// Estimate htf return temp w/o heat exchanger
		//double m_m_dot_htf_sby = m_Q_dot_rec_des*m_q_sby_frac/(m_cp_rec*(m_T_htf_hot - m_T_htf_cold_sby));

		// Set outputs that will be constant for this type (required because can share a cmod with molten salt tower)
		value(O_M_DOT_MAKEUP, 0.0);
		value(O_M_DOT_DEMAND, 0.0);							// This output is an input for the controller, but the controller doesn't use it...
		value(O_M_DOT_HTF_REF, m_dot_rec_des*3600.0);		// Not actually the RECEIVER des. should be HTF
		value(O_F_BAYS, 0.0);
		value(O_P_COND, 0.0);								// Probably do want to report some cycle info that is different from steam

		return 0;
	}

	virtual int call(double /*time*/, double step, int ncall)
	{
		double T_htf_hot = value(I_T_HTF_HOT) + 273.15;			//[K] Hot HTF temp from the receiver, convert from C
		double m_dot_htf = value(I_M_DOT_HTF)/3600.0;			//[kg/s] Mass flow rate of htf from receiver, convert from kg/s
		m_standby_control = (int)value(I_STANDBY_CONTROL);		//[-] Standby control from the controller
		double T_db = value(I_T_DB) + 273.15;					//[K] Dry bulb temperature, convert from C
		double P_amb = value(I_P_AMB)*100.0;					//[mbar] Ambient air pressure

		//**************************************************
		// Test by setting important inputs to design values
		//**************************************************
		// T_htf_hot = m_T_htf_hot;
		// m_dot_htf = m_dot_rec_des*1.15;
		// m_standby_control = 1;
		// T_db = value(P_T_AMB_DES) + 273.15;
		// P_amb = 101325.0;
		//**************************************************
		//**************************************************
		//**************************************************

		// track ncall so it's available in 'convergence' call
		m_ncall = ncall;

		// Reset error message code
		m_error_message_code = 0;

		double W_dot_net = std::numeric_limits<double>::quiet_NaN();
		double eta_thermal = std::numeric_limits<double>::quiet_NaN();
		double T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		double W_dot_par = std::numeric_limits<double>::quiet_NaN();

		double Q_dot_PHX = std::numeric_limits<double>::quiet_NaN();
		double C_dot_htf = std::numeric_limits<double>::quiet_NaN();

		double T_turbine_in, P_main_comp_in, P_main_comp_out, frac_recomp, N_mc_od;
		T_turbine_in = P_main_comp_in = P_main_comp_out = frac_recomp = N_mc_od = std::numeric_limits<double>::quiet_NaN();

		double q_startup = 0.0;

		switch( m_standby_control )
		{
		case 1:
		{
			C_dot_htf = m_dot_htf * m_cp_rec;

			// Assume compressor inlet temperature is always design point delta T above ambient: (T_amb_des - T_comp_in)
			// Floor is ~ critical temp + 1 = 32 C
			double T_mc_in = max(ms_rc_cycle.get_design_limits().m_T_mc_in_min, T_db + m_delta_T_acc);

			// Assume turbine inlet temperature is always design point delta T below receiver hot side
			double T_t_in = T_htf_hot - m_delta_T_t;

			ms_rc_od_par.m_T_mc_in = T_mc_in;
			ms_rc_od_par.m_T_t_in = T_t_in;
			ms_rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->ms_t_des_solved.m_N_design;
			ms_rc_od_par.m_tol = ms_rc_autodes_hit_eta_par.m_des_tol;
			
			ms_phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
			ms_phx_od_par.m_T_htf_hot = T_htf_hot;
			ms_phx_od_par.m_m_dot_htf = m_dot_htf;
			ms_phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
			ms_phx_od_par.m_cp_htf = m_cp_rec;

			int hx_od_error = 0;
			//ms_rc_cycle.opt_od_eta_for_hx(ms_rc_od_par, ms_phx_od_par, hx_od_error);

			if( hx_od_error != 0 )
			{
				if( hx_od_error != 1 )
				{
					m_error_message_code = 1;		// Off-design model not solving
					break;
				}
				else
				{
					m_error_message_code = 2;
				}
			}

			W_dot_net = ms_rc_cycle.get_od_solved()->m_W_dot_net;
			Q_dot_PHX = ms_rc_cycle.get_od_solved()->m_Q_dot;
			T_htf_cold = T_htf_hot - Q_dot_PHX/C_dot_htf;

			T_turbine_in = ms_rc_cycle.get_od_solved()->m_temp[6-1];
			P_main_comp_in = ms_rc_cycle.get_od_solved()->m_pres[1-1];
			P_main_comp_out = ms_rc_cycle.get_od_solved()->m_pres[2-1];
			frac_recomp = ms_rc_cycle.get_od_solved()->m_recomp_frac;
			N_mc_od = ms_rc_cycle.get_od_solved()->ms_mc_ms_od_solved.m_N;		//[rpm]

			T_htf_cold = ms_phx_od_par.m_T_htf_cold;

			eta_thermal = ms_rc_cycle.get_od_solved()->m_eta_thermal;
			
			// Call off-design air-cooler model
			int acc_error_code = 0;
			//ACC.off_design_hx(T_db, P_amb, ms_rc_cycle.get_od_solved()->m_temp[9 - 1], ms_rc_cycle.get_od_solved()->m_pres[9 - 1],
			//								ms_rc_cycle.get_od_solved()->m_m_dot_mc, T_mc_in, W_dot_par, acc_error_code);

			if(acc_error_code == 1)
			{
				W_dot_par = (m_dot_htf / m_dot_rec_des)*m_W_dot_fan_des;
				message(TCS_NOTICE, "Off-design air cooler model did not solve. Fan power was set to the design value scaled by the timestep/design HTF mass flow rate");
			}
			if(acc_error_code == 2)
			{
				message(TCS_NOTICE, "Off-design air cooler model did not converge within its numerical tolerance");
			}

			break;
		}
		// End "Normal Operation" power cycle mode

		case 2:			// Standby mode
			W_dot_net = 0.0;
			// T_htf_cold = m_T_htf_cold_sby;

			T_htf_cold = ms_phx_od_par.m_T_htf_cold;

			T_turbine_in = 0.0;
			P_main_comp_in = 0.0;
			P_main_comp_out = 0.0;
			frac_recomp = 0.0;
			N_mc_od = 0.0;

			eta_thermal = 0.0;
			W_dot_par = 0.0;

			break;

		case 3:
		default:			// OFF
			W_dot_net = 0.0;
			// T_htf_cold = m_T_htf_cold_des;

			// This value needs to stay at its operational value or the first timestep with DNI has problems with a guess value so low (in type 222)...
			T_htf_cold = ms_phx_od_par.m_T_htf_cold;

			T_turbine_in = 0.0 + 273.15;	//[K] Output expected in K and converted to C
			P_main_comp_in = 0.0;
			P_main_comp_out = 0.0;
			frac_recomp = 0.0;
			N_mc_od = 0.0;

			eta_thermal = 0.0;
			W_dot_par = 0.0;

		}		// End switch for different PC operating modes

		// Cycle off-design model not solving, but we need to return some results to controller
		if(m_error_message_code == 1)
		{
			W_dot_net = m_W_dot_net_des*(m_dot_htf / m_dot_rec_des);
			eta_thermal = ms_rc_cycle.get_design_solved()->m_eta_thermal;
			double Q_dot_PHX_guess = W_dot_net / eta_thermal;
			T_htf_cold = T_htf_hot - Q_dot_PHX_guess / C_dot_htf;

			// For now, set these to 0 because they are only used as outputs: need to reconsider if used by other types in performance code
			T_turbine_in = 0.0;
			P_main_comp_in = 0.0;
			P_main_comp_out = 0.0;
			frac_recomp = 0.0;

			T_htf_cold = ms_phx_od_par.m_T_htf_cold;

			W_dot_par = (m_dot_htf / m_dot_rec_des)*m_W_dot_fan_des;
		}

		double W_dot_net_output = W_dot_net;

		if(W_dot_net > 0.0)
		{
			if( (m_standby_control_prev==3 && m_standby_control == 1) || (m_E_su_prev + m_time_su_prev > 0.0) )
			{
				m_time_su = max(m_time_su_prev-step/3600.0, 0.0);
				if(m_E_su_prev < Q_dot_PHX*step/3600.0)		// units?  kWt-hr < kW * s * hr/s
				{
					m_E_su = 0.0;
					if(min(1.0,m_time_su_prev/(step/3600.0)) > m_E_su_prev/(Q_dot_PHX*step/3600.0))
					{
						W_dot_net_output = W_dot_net_output*(1.0 - min(1.0, m_time_su_prev / (step/3600.0)));
					}
					else
					{
						W_dot_net_output = (Q_dot_PHX*step/3600.0 - m_E_su_prev)/(step/3600.0) * eta_thermal;
					}
				}
				else
				{
					m_E_su = m_E_su_prev - Q_dot_PHX*step / 3600.0;					
				}						
			}
			q_startup = m_E_su_prev - m_E_su;
		}

		// Set Outputs
		value(O_P_CYCLE, W_dot_net_output/1.E3);	//[MWe] 
		value(O_ETA, eta_thermal);					//[-]	
		value(O_T_HTF_COLD, T_htf_cold - 273.15);	//[C]	
		value(O_W_COOL_PAR, W_dot_par);				//[MWe]	

		value(O_T_TURBINE_IN, T_turbine_in - 273.15);	//[C]
		value(O_P_MC_IN, P_main_comp_in);				//[kPa]
		value(O_P_MC_OUT, P_main_comp_out);             //[kPa]
		value(O_F_RECOMP, frac_recomp);                 //[-]
		value(O_N_MC, N_mc_od);							//[rpm]
		
		value(O_Q_STARTUP, q_startup / 1.E3);		//[MWt-hr]

		return 0;										 
	}													 
														 
	virtual int converged(double /*time*/)					 
	{
		if(m_standby_control == 3)
		{
			m_E_su_prev = m_startup_energy;
			m_time_su_prev = m_startup_time;
		}
		else
		{
			m_E_su_prev = m_E_su;
			m_time_su_prev = m_time_su;
		}

		m_standby_control_prev = m_standby_control;

		if(m_error_message_code == 1)
		{
			message(TCS_NOTICE, "The off-design power cylce model did not solve. Performance values for this timestep are design point values scaled by HTF mass flow rate");
		}

		if(m_error_message_code == 2)
		{
			message(TCS_NOTICE, "The off-design power cycle model solved, but the the PHX performance did not converge. The results at this timestep may be non-physical");
		}

		m_error_message_code = 0;

		m_is_first_t_call = true;

		return 0;
	}

};

TCS_IMPLEMENT_TYPE(sam_sco2_recomp_type424, "Integrated sco2 powerblock", "Ty Neises", 1, sam_sco2_recomp_type424_variables, NULL, 1)

