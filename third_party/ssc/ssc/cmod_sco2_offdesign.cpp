#include "core.h"
#include "sco2_power_cycle.h"
#include <vector>

static var_info _cm_vtab_sco2_offdesign[] = {
/*  VARTYPE   DATATYPE         NAME                  LABEL                                                UNITS     META        GROUP                      REQUIRED_IF          CONSTRAINTS   UI_HINTS*/
	// **************** Design Parameters *****************************
{ SSC_INPUT,  SSC_NUMBER,     "I_W_dot_net_des",     "Design cycle power output",                         "MWe",    "",         "sCO2 power cycle",         "*",                "",           "" },		
{ SSC_INPUT,  SSC_NUMBER,     "I_T_mc_in_des",       "Main compressor inlet temp at design",              "C",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_T_t_in_des",        "Turbine inlet temp at design",                      "C",      "",         "sCO2 power cycle",         "*",                "",           "" },								
{ SSC_INPUT,  SSC_NUMBER,     "I_N_t_des",           "Design turbine speed, negative links to comp.",     "rpm",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_c",             "Design compressor(s) isentropic efficiency",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_t",             "Design turbine isentropic efficiency",              "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_tol",               "Convergence tolerance for performance calcs",       "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_opt_tol",           "Convergence tolerance - optimization calcs",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_UA_total_des",      "Total UA allocatable to recuperators",              "kW/K",   "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_P_high_des",        "Design compressor outlet pressure",                 "MPa",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "I_PR_mc_des",         "Design Pressure Ratio across main comp.",           "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "I_LT_frac_des",       "Design UA distribution",                            "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "I_recomp_frac_des",   "Design recompression fraction",                     "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
	// ***************** Off Design Inputs and Parameters *************************
{ SSC_OUTPUT, SSC_NUMBER,     "I_T_mc_in",           "Compressor inlet temperature",                      "C",      "",         "sCO2 power cycle",         "*",                "",           "" },    
{ SSC_OUTPUT, SSC_NUMBER,     "I_T_t_in",            "Turbine inlet temperature",                         "C",		"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "I_W_dot_net_target",  "Target net output target",                          "MWe",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "I_optimize_N_t",      "Bool: '1 = true' or '0 = false' ",                  "-",		"",         "sCO2 power cycle",         "*",                "",           "" },

	// ***************** Reference Design Output **************************************************
{ SSC_OUTPUT, SSC_NUMBER,     "O_eta_thermal_des",   "Design cycle thermal efficiency",                   "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
	// ***************** Off-design Outputs *******************************************************
{ SSC_OUTPUT, SSC_NUMBER,     "O_eta_thermal",       "Off-design thermal efficiency",                     "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_W_dot_net",         "Actual off-design net power output",                "MWe",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_P_mc_out",          "Main compressor outlet pressure",                   "MPa",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_P_mc_in",           "Main compressor inlet pressure",                    "MPa",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_recomp_frac",       "Recompression fraction",                            "-",		"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_N_mc",              "Main compressor shaft speed",                       "rpm",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_N_t",               "Turbine shaft speed",                               "rpm",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_N_rc",              "Recompressor shaft speed",                          "rpm",	"",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_m_dot_PHX",         "Mass flow rate through primary HX",                 "kg/s",   "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_ARRAY,      "O_T_array",			 "Cycle temp state points at design",                 "K",      "",         "sCO2 power cycle",         "*",                "",           "" },

var_info_invalid };


class cm_sco2_offdesign : public compute_module
{
public:

	cm_sco2_offdesign()
	{
		add_var_info(_cm_vtab_sco2_offdesign);
	}

	void exec() override
	{
		cycle_design_parameters rc_des_par;

		// Only 1 compressor map currently available, so hardcode these
		rc_des_par.m_mc_type = 1;
		rc_des_par.m_rc_type = 1;

		// Read in design parameters
		//rc_des_par.m_W_dot_net = 10.0 * 1000.0;				//[kW]
		//rc_des_par.m_T_mc_in = 32.0 + 273.15;				//[K]
		//rc_des_par.m_T_t_in = 550.0 + 273.15;				//[K]
		rc_des_par.m_W_dot_net = as_double("I_W_dot_net_des")*1000.0;		//[kW] Design cycle power outpt
		rc_des_par.m_T_mc_in = as_double("I_T_mc_in_des") + 273.15;			//[K] Compressor inlet temp at design, convert from C
		rc_des_par.m_T_t_in = as_double("I_T_t_in_des") + 273.15;			//[K] Turbine inlet temp at design, convert from C

		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		// Hardcode for now
		rc_des_par.m_DP_LT[0] = 0.0;
		rc_des_par.m_DP_LT[1] = 0.0;
		rc_des_par.m_DP_HT[0] = 0.0;
		rc_des_par.m_DP_HT[1] = 0.0;
		rc_des_par.m_DP_PC[0] = 0.0;
		rc_des_par.m_DP_PC[1] = 0.0;
		rc_des_par.m_DP_PHX[0] = 0.0;
		rc_des_par.m_DP_PHX[1] = 0.0;

		// Read in turbine speed
		//rc_des_par.m_N_t = -1.0;
		rc_des_par.m_N_t = as_double("I_N_t_des");			//[rpm] Turbine speed, Negative number links to compressor speed

		// Read in turbomachinery isentropic efficiencies
		//rc_des_par.m_eta_mc = 0.89;
		//rc_des_par.m_eta_rc = 0.89;
		//rc_des_par.m_eta_t = 0.93;
		double eta_comps = as_double("I_eta_c");
		rc_des_par.m_eta_mc = eta_comps;
		rc_des_par.m_eta_rc = eta_comps;
		rc_des_par.m_eta_t = as_double("I_eta_t");

		// Hardcode for now
		rc_des_par.m_N_sub_hxrs = 20;

		// Read in tolerances
		rc_des_par.m_tol = 1.E-6;						//[-]
		rc_des_par.m_opt_tol = 1.E-6;					//[-]
		rc_des_par.m_tol = as_double("I_tol");			//[-] Convergence tolerance for performance calcs
		rc_des_par.m_opt_tol = as_double("I_opt_tol");	//[-] Convergence tolerance for optimization calcs

		// Set UA total, distribution and boolean
		rc_des_par.m_fixed_LT_frac = true;
		rc_des_par.m_UA_rec_total = as_double("I_UA_total_des");		//[kW/K] Total UA allocatable to recuperators
		rc_des_par.m_LT_frac = as_double("I_LT_frac_des");

		// Set mc outlet pressure and boolean to fixed
		rc_des_par.m_fixed_P_mc_out = true;
		rc_des_par.m_P_mc_out = as_double("I_P_high_des")*1000.0;

		// Set pressure ratio and boolean to fixed
		rc_des_par.m_fixed_PR_mc = true;
		rc_des_par.m_PR_mc = as_double("I_PR_mc_des");
		rc_des_par.m_P_high_limit = as_double("I_P_high_des")*1000.0;

		// Set recompression fraction
		rc_des_par.m_fixed_recomp_frac = true;
		rc_des_par.m_recomp_frac = as_double("I_recomp_frac_des");

		// ***************************************************************
		// Run design point model to set component parameters
		RecompCycle rc_cycle(rc_des_par);
		bool design_cycle_success = rc_cycle.design_no_opt();
		// ***************************************************************
		assign("O_eta_thermal_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_eta_thermal));

		// Set structure for off-design inputs
		cycle_opt_off_des_inputs rc_opt_off_des_in;
		rc_opt_off_des_in.m_T_mc_in = as_double("I_T_mc_in")+273.15;
		rc_opt_off_des_in.m_T_t_in = as_double("I_T_t_in") + 273.15;
		rc_opt_off_des_in.m_W_dot_net_target = as_double("I_W_dot_net_target")*1000.0;
		rc_opt_off_des_in.m_N_sub_hxrs = rc_cycle.get_cycle_design_parameters()->m_N_sub_hxrs;
		// Optimize recompression fraction?
		if( rc_cycle.get_cycle_design_parameters()->m_recomp_frac == 0.0 )
		{
			rc_opt_off_des_in.m_fixed_recomp_frac = true;		// If no recompressor then no need to vary recompression fraction
			rc_opt_off_des_in.m_recomp_frac = rc_cycle.get_cycle_design_parameters()->m_recomp_frac;
		}
		else
		{
			rc_opt_off_des_in.m_fixed_recomp_frac = false;
			rc_opt_off_des_in.m_recomp_frac_guess = rc_cycle.get_cycle_design_parameters()->m_recomp_frac;
		}
		// Optimize main compressor speed?
		rc_opt_off_des_in.m_fixed_N_mc = false;
		rc_opt_off_des_in.m_N_mc_guess = rc_cycle.get_cycle_design_metrics()->m_N_mc;
		// Optimize turbine speed?
		rc_opt_off_des_in.m_fixed_N_t = !(bool)(as_double("I_optimize_N_t"));
		if( rc_opt_off_des_in.m_fixed_N_t )
			rc_opt_off_des_in.m_N_t = rc_cycle.get_cycle_design_parameters()->m_N_t;
		else
			rc_opt_off_des_in.m_N_t_guess = rc_cycle.get_cycle_design_parameters()->m_N_t;
		// Tolerances
		rc_opt_off_des_in.m_tol = rc_cycle.get_cycle_design_parameters()->m_tol;
		rc_opt_off_des_in.m_opt_tol = rc_cycle.get_cycle_design_parameters()->m_opt_tol;
		bool od_opt_cycle_success = rc_cycle.optimal_off_design(rc_opt_off_des_in);
		
		const std::vector<double> P_vector = rc_cycle.get_off_design_outputs()->m_P;


		assign("O_eta_thermal", var_data((ssc_number_t)rc_cycle.get_off_design_outputs()->m_eta_thermal));
		assign("O_W_dot_net", var_data((ssc_number_t)rc_cycle.get_off_design_outputs()->m_W_dot_net/1000.0));
		assign("O_P_mc_out", var_data((ssc_number_t)P_vector[2 - 1]/1000.0));
		assign("O_P_mc_in", var_data((ssc_number_t)rc_cycle.get_off_design_inputs()->m_P_mc_in/1000.0));
		assign("O_recomp_frac", var_data((ssc_number_t)rc_cycle.get_off_design_inputs()->m_S.m_recomp_frac));
		assign("O_N_mc", var_data((ssc_number_t)rc_cycle.get_off_design_inputs()->m_S.m_N_mc));
		assign("O_N_t", var_data((ssc_number_t)rc_cycle.get_off_design_inputs()->m_S.m_N_t));
		assign("O_N_rc", var_data((ssc_number_t)rc_cycle.get_off_design_outputs()->m_N_rc));
		assign("O_m_dot_PHX", var_data((ssc_number_t)rc_cycle.get_off_design_outputs()->m_m_dot_PHX));


		// Assign temperatures to array
		const std::vector<double> T_vector = rc_cycle.get_off_design_outputs()->m_T;
		int l_T_array = T_vector.size();
		ssc_number_t * T_array = new ssc_number_t[l_T_array];

		for( int i = 0; i < l_T_array; i++ )
			T_array[i] = (ssc_number_t)(T_vector[i]);

		// Assign pressure and temp arrays to var_data
		assign("O_T_array", var_data(T_array, l_T_array));
		//assign("O_P_array_des", var_data(P_array, l_P_array));

		// Delete dynamically created arrays
		delete[] T_array;


		// Example of throwing an error
		//if( eta > 0.4 )
		//	throw general_error("invalid cycle efficiency: too high!");

		

		// Assign temperatures to array
		//const std::vector<double> T_vector = rc_cycle.get_cycle_design_metrics()->m_T;
		//int l_T_array = T_vector.size();
		//ssc_number_t * T_array = new ssc_number_t[l_T_array];

		//for( int i = 0; i < l_T_array; i++ )
		//	T_array[i] = (ssc_number_t)(T_vector[i]);

		//// Assign pressure to array
		//int l_P_array = rc_cycle.get_cycle_design_metrics()->m_P.size();
		//ssc_number_t * P_array = new ssc_number_t[l_P_array];

		//for( int i = 0; i < l_P_array; i++ )
		//	P_array[i] = (ssc_number_t)(rc_cycle.get_cycle_design_metrics()->m_P[i]);

		//// Assign pressure and temp arrays to var_data
		//assign("O_T_array", var_data(T_array, l_T_array));
		//assign("O_P_array", var_data(P_array, l_P_array));

		//// Delete dynamically created arrays
		//delete[] T_array;
		//delete[] P_array;

		//cycle_design_parameters rc_des_par_test;

		//// Only 1 compressor map currently available, so hardcode these
		//rc_des_par_test.m_mc_type = 1;
		//rc_des_par_test.m_rc_type = 1;

		//// Read in design parameters
		//rc_des_par_test.m_W_dot_net = as_double("I_W_dot_net_des")*1000.0;		//[kW] Design cycle power outpt
		//rc_des_par_test.m_T_mc_in = as_double("I_T_mc_in_des") + 273.15;			//[K] Compressor inlet temp at design, convert from C
		//rc_des_par_test.m_T_t_in = as_double("I_T_t_in_des") + 273.15;			//[K] Turbine inlet temp at design, convert from C

		////(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		//// Hardcode for now
		//rc_des_par_test.m_DP_LT[0] = 0.0;
		//rc_des_par_test.m_DP_LT[1] = 0.0;
		//rc_des_par_test.m_DP_HT[0] = 0.0;
		//rc_des_par_test.m_DP_HT[1] = 0.0;
		//rc_des_par_test.m_DP_PC[0] = 0.0;
		//rc_des_par_test.m_DP_PC[1] = 0.0;
		//rc_des_par_test.m_DP_PHX[0] = 0.0;
		//rc_des_par_test.m_DP_PHX[1] = 0.0;

		//// Read in turbine speed
		////rc_des_par.m_N_t = -1.0;
		//rc_des_par_test.m_N_t = as_double("I_N_t_des");			//[rpm] Turbine speed, Negative number links to compressor speed

		//// Hardcode for now
		//rc_des_par_test.m_eta_mc = 0.89;
		//rc_des_par_test.m_eta_rc = 0.89;
		//rc_des_par_test.m_eta_t = 0.9;
		//rc_des_par_test.m_N_sub_hxrs = 20;

		//// Read in tolerances
		//rc_des_par_test.m_tol = 1.E-6;						//[-]
		//rc_des_par_test.m_opt_tol = 1.E-6;					//[-]
		//rc_des_par_test.m_tol = as_double("I_tol");			//[-] Convergence tolerance for performance calcs
		//rc_des_par_test.m_opt_tol = as_double("I_opt_tol");	//[-] Convergence tolerance for optimization calcs


		//// Using auto-optimization, so only need to define total allocatable UA, optimization code will define LT_frac
		////double UA_LT = 500.0;
		////double UA_HT = 500.0;
		////rc_des_par.m_UA_rec_total = UA_LT + UA_HT;
		//rc_des_par_test.m_LT_frac = rc_cycle.get_cycle_design_parameters()->m_LT_frac;
		//rc_des_par_test.m_fixed_LT_frac = true;
		//rc_des_par_test.m_UA_rec_total = as_double("I_UA_total_des");		//[kW/K] Total UA allocatable to recuperators

		//// Using auto-optimization, so only need to define the high side pressur limit, optimization code will define P_high and P_ratio
		////double P_mc_in = 7.69*1000.0;
		////double P_mc_out = 20.0*1000.0;
		//rc_des_par_test.m_fixed_P_mc_out = true;
		//rc_des_par_test.m_P_mc_out = rc_cycle.get_cycle_design_parameters()->m_P_mc_out;
		////rc_des_par.m_P_mc_out_guess = rc_des_par.m_P_mc_out;
		////rc_des_par.m_P_high_limit = 25.0*1000.0;
		//rc_des_par_test.m_fixed_PR_mc = true;
		//rc_des_par_test.m_PR_mc = rc_cycle.get_cycle_design_parameters()->m_PR_mc;
		////rc_des_par.m_PR_mc_guess = rc_des_par.m_PR_mc;
		//rc_des_par_test.m_P_high_limit = as_double("I_P_high_limit")*1000.0;

		//// Using auto-optimization, optimization code will define recompression fraction
		//rc_des_par_test.m_fixed_recomp_frac = true;
		//rc_des_par_test.m_recomp_frac = rc_cycle.get_cycle_design_parameters()->m_recomp_frac;
		//// rc_des_par.m_recomp_frac_guess = 0.5;

		//RecompCycle rc_cycle_test(rc_des_par_test);

		//rc_cycle_test.design_no_opt();

		//return;
	}

};

DEFINE_MODULE_ENTRY(sco2_offdesign, "Calls sCO2 off design performance model given cycle design parameters", 1)
