#ifndef __SCO2_PC_
#define __SCO2_PC_

#include <algorithm>
#include <vector>
#include <limits>
#include "co2_compressor_library.h"
//#include "co2props.h"
//#include "co2props_nn.h"
#include "CO2_properties.h"
#include <math.h>

class TrackErrors   
{
private:
	std::vector<int> stored_codes;

public:
	~TrackErrors(){};

	TrackErrors(){};

	void SetError(int error_code)
	{
		stored_codes.push_back(error_code);
		return;
	}

	void ReportErrors()
	{
		/*
		// Report something!
		for( std::vector<int>::iterator it = stored_codes.begin();
			it != stored_codes.end();
			++it )
		{
			int val = *it;
		}
		
		for( size_t i = 0; i < stored_codes.size(); i++ )
		{
			int val = stored_codes[i];
		}
		*/


		return;
	}


};

struct cycle_design_parameters
{
	int m_mc_type; 
	int m_rc_type;

	double m_W_dot_net;		//[kW]
	double m_T_mc_in;			//[K]
	double m_T_t_in;			//[K]
	std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	double m_N_t;						//[rpm] turbine shaft speed: required for calculating physical turbomachinery parameters
	double m_eta_mc;					//[-] Isentropic efficiency of main compressor
	double m_eta_rc;					//[-] Isentropic efficiency of recompessor
	double m_eta_t;					//[-] Isentropic efficiency of turbine
	int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers used to discretize recuperators
	double m_tol;						//[-] Relative convergence tolerance for iteration loops
	double m_opt_tol;					//[-] Tolerance for optimization convergence

	bool m_fixed_LT_frac;				// Is UA distribution fixed or available for optimization?
	double m_UA_rec_total;			//[kW/K] Total available recuperator conductance (UA): REQUIRED
	double m_LT_frac;					//[-] Fraction of UA to LT recup - only used if fixed_LT_frac = true
	double m_LT_frac_guess;			//[-] Initial value for fraction of UA to LT recup - only used if fixed_LT_frac = false

	bool m_fixed_P_mc_out;			// Is P_mc_out fixed or available for optimization?
	double m_P_mc_out;				//[kPa] Compressor outlet pressure - only used if fixed_P_mc_out = true
	double m_P_high_limit;			//[kPa] Maximum allowable high-side pressure - only used if fixed_P_mc_out = false
	double m_P_mc_out_guess;			//[kPa] Initial value for mc outlet pressure - only used if fixed_P_mc_out = false

	bool m_fixed_PR_HP_to_LP;				// Is the Pressure Ratio over the main compressor fixed or available for optimization
	double m_PR_mc;					//[-] Pressure Ratio - only used if fixed_PR_mc = true
	double m_PR_HP_to_LP_guess;				//[-] Initial value for Pressure Ratio - only used if fixed_PR_mc = false

	bool m_fixed_recomp_frac;			// Is the recompressor fraction fixed or available for optimization?
	double m_recomp_frac;				//[-] Recompression fraction - only used if fixed_recomp_frac = true
	double m_recomp_frac_guess;		//[-] Initial value for Recompression fraction - only used if fixed_recomp_frac = false

	cycle_design_parameters()
	{
		m_mc_type = m_rc_type = m_N_sub_hxrs - 1;
		
		m_fixed_LT_frac = m_fixed_P_mc_out = m_fixed_PR_HP_to_LP = m_fixed_recomp_frac = false;
		
		m_W_dot_net = m_T_mc_in = m_T_t_in = m_N_t = m_eta_mc = m_eta_rc = m_eta_t = m_tol = m_opt_tol = m_UA_rec_total = m_LT_frac = m_LT_frac_guess = m_P_mc_out = 
			m_P_high_limit = m_P_mc_out_guess = m_PR_mc = m_PR_HP_to_LP_guess = m_recomp_frac = m_recomp_frac_guess = std::numeric_limits<double>::quiet_NaN();
		
		m_DP_LT.resize(2);
		std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
		m_DP_HT.resize(2);
		std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
		m_DP_PC.resize(2);
		std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
		m_DP_PHX.resize(2);
		std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
	}
};

struct S_cycle_design_metrics
{
	double m_eta_thermal;		//[-] Cycle thermal efficiency
	double m_W_dot_net;			//[-] Power output
	double m_min_DT_LT;			//[-] Minimum temperature difference in LT recup
	double m_min_DT_HT;			//[-] Minimum temperature difference in HT recup
	double m_N_mc;				//[rpm] Main compressor speed
	double m_m_dot_PHX;			//[kg/s] Mass flow rate through primary heat exchanger
	double m_m_dot_PC;			//[kg/s] Mass flow rate through pre-cooler

	std::vector<double> m_T;	//[K] Temperature at cycle state points
	std::vector<double> m_P;	//[kPa] Pressure at cycle state points

	S_cycle_design_metrics()
	{
		m_eta_thermal = m_W_dot_net = m_min_DT_LT = m_min_DT_HT = m_m_dot_PHX = m_m_dot_PC = std::numeric_limits<double>::quiet_NaN();
		// Size vectors when they are assigned to values from the RecompCycle class?
	}
};

struct cycle_opt_off_des_inputs
{
	double m_T_mc_in;		//[K] Main compressor inlet temperature
	double m_T_t_in;		//[K] Turbine inlet temperature
	double m_W_dot_net_target;	//[kW] Target net power output or rate of heat addition to the cycle
	int m_N_sub_hxrs;		//[-] Number of sub heat exchangers

	bool m_fixed_recomp_frac;		// Is recomp fraction fixed or available for optimization
	double m_recomp_frac;			//[-] Recompression fraction: only used if fraction is fixed
	double m_recomp_frac_guess;		//[-] Guess for recompression fraction: only used if fraction is available for optimization

	bool m_fixed_N_mc;			//[-] Is the main compressor speed fixed fixed or available for optimization
	double m_N_mc;				//[rpm] Main compressor speed: only used if it is fixed [use negative value to use design point speed]
	double m_N_mc_guess;		//[rpm] Guess for main compressor speed: only used if it is available for optimization

	bool m_fixed_N_t;			//[-] Is the turbine speed fixed or available for optimization
	double m_N_t;				//[rpm] Turbine speed [use negative value to couple to main compressor]
	double m_N_t_guess;			//[rpm] Guess for turbine speed: only used if it is available for optimization

	//double m_P_mc_in;		//[kPa] Main compressor inlet pressure
			
	double m_tol;			//[-] Cycle Convergence Tolerance
	double m_opt_tol;		//[-] Optimization convergence

	cycle_opt_off_des_inputs()
	{
		m_T_mc_in = m_T_t_in = m_W_dot_net_target = m_recomp_frac = m_recomp_frac_guess = m_N_mc = m_N_mc_guess = m_N_t =
			m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();
		m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = false;
	}
};

struct cycle_off_des_inputs
{
	cycle_opt_off_des_inputs m_S;	// Structure for inputs to Cycle Optimum Off-Design

	double m_P_mc_in;

	cycle_off_des_inputs()
	{
		m_P_mc_in = std::numeric_limits<double>::quiet_NaN();
	}	
};

struct S_off_design_performance
{
	double m_eta_thermal;		//[-] Cycle thermal efficiency
	double m_W_dot_net;			//[kW] Power output
	double m_q_dot_in;			//[kW] Thermal input to cycle
	double m_min_DT_LT;			//[-] Minimum temperature difference in LT recup
	double m_min_DT_HT;			//[-] Minimum temperature difference in HT recup
	double m_N_rc;				//[-] Shaft speed - recompressor
	double m_m_dot_PHX;			//[kg/s] Mass flow rate through primary heat exchanger
	double m_m_dot_PC;			//[kg/s] Mass flow rate through pre-cooler

	std::vector<double> m_T;	//[K] Temperature at cycle state points
	std::vector<double> m_P;	//[kPa] Pressure at cycle state points

	S_off_design_performance()
	{
		m_eta_thermal = m_W_dot_net = m_q_dot_in = m_min_DT_LT = m_min_DT_HT = m_N_rc = m_m_dot_PHX = m_m_dot_PC = std::numeric_limits<double>::quiet_NaN();
		// Size vectors when they are assigned to values from the RecompCycle class?
	}

};

struct compressor_design_parameters
{
	int m_type;
	double m_w_design;				//[kJ/kg] Specific work required by compressor [negative value]
	double m_eta_design;				//[-] Isentropic efficiency
	double m_m_dot_design;			//[kg/s] Mass flow rate through the compressor
	double m_rho_in_design;			//[kg/m^3] Inlet fluid density
	double m_recomp_frac_design;		//[-] Recompression fraction

	compressor_design_parameters()
	{
		m_type = -1;
		m_w_design = m_eta_design = m_m_dot_design = m_rho_in_design = m_recomp_frac_design = std::numeric_limits<double>::quiet_NaN();
	}
};

class compressor
{
private:

	// Parameter Structure
	compressor_design_parameters m_comp_des_par;

	// Calculated performance values at design
	double m_N_design;			  // design-point shaft speed (rpm)
	double m_D_rotor;				  // rotor diameter (m)
	double m_phi_design;			  // design-point flow coefficient for Sandia compressor (corresponds roughly to max eta)
	double m_phi_min;				  // surge limit for SNL compressor
	double m_phi_max;				  // x-intercept for SNL compressor

	// Off-design performnace - save as member data?
	bool m_surge;                   // true if the compressor is in the surge region
	double m_N;					  // shaft speed (rpm)
	double m_eta;					  // efficiency (-)
	//double w_tip_ratio;			  // ratio of the local (comp outlet) speed of sound to the tip speed
	double m_w;					  // specific work required by the compressor (kJ/kg) [negative value]
	double m_m_dot;				  // mass flow rate through the compressor (kg/s)
	//double phi_star;			  // dimensionless modified flow coefficient (-)

public:
	~compressor(){};

	compressor()
	{
		m_N_design = m_D_rotor = m_phi_design = m_phi_min = m_phi_max = std::numeric_limits<double>::quiet_NaN();
		// Set structure members to NaN?
	}

	bool initialize(const compressor_design_parameters & comp_des_par_in )
	{
		m_comp_des_par = comp_des_par_in;

		get_compressor_parameters(m_comp_des_par.m_type, m_phi_design, m_phi_min, m_phi_max);		// Get compressor-specific parameters

		if( m_comp_des_par.m_recomp_frac_design > 1.E-12 )
		{
			double psi_design = std::numeric_limits<double>::quiet_NaN();
			psi_design = compressor_psi_polynomial_fit(m_comp_des_par.m_type, m_phi_design);		// Get modified ideal head coefficient at design

			double U_tip = sqrt(1000.0*(-(m_comp_des_par.m_w_design*m_comp_des_par.m_eta_design)) / psi_design);		// Rearranging definition of head coefficient

			m_D_rotor = sqrt(m_comp_des_par.m_m_dot_design / (m_phi_design*m_comp_des_par.m_rho_in_design*U_tip));				// Rearranging definition of flow coefficient

			m_N_design = (U_tip*2.0 / m_D_rotor)*9.549296590;					// Shaft speed in rpm

			if( m_N_design != m_N_design )
				return false;
		}
		else
		{
			m_D_rotor = m_N_design = 0.0;
		}
		return true;
	}

	double calculate_m_dot_max( double rho_in, double N_mc_local )
	{
		return m_phi_max*rho_in*pow(m_D_rotor,2)*m_D_rotor*0.5*N_mc_local*0.10471975512;		// Highest possible mass flow rate in main compressor (pressure rise would be zero)		
	}

	double get_N_design()
	{
		return m_N_design;
	}

	double get_N_off_design()
	{
		return m_N;
	}

	double get_w()
	{
		return m_w;
	}

	void solve_compressor(double T_in, double P_in, double m_dot, double N_in, int & error_code, double & T_out, double & P_out);

};

struct turbine_design_parameters
{
	double m_w_design;			//[kJ/kg] Specific work generated by the turbine [positive value]
	double m_eta_design;			//[-] Isentropic efficiency of turbine
	double m_m_dot_design;		//[kg/s] Mass flow rate through turbine
	double m_N_design;			//[rpm] Turbine shaft speed
	double m_rho_out_design;		//[kg/m^3] Fluid outlet density

	turbine_design_parameters()
	{
		m_w_design = m_eta_design = m_m_dot_design = m_N_design = m_rho_out_design = std::numeric_limits<double>::quiet_NaN();
	}
};

class turbine
{
private:
	// Parameter Structure
	turbine_design_parameters m_turb_des_par;

	// Calculated performance values at design

	// Member design data
	double m_D_rotor;                  // rotor diameter (m)
	double m_A_nozzle;                 // effective nozzle area (m2)

	// Off-design performnace - save as member data?
	double m_N;                        // shaft speed (rpm)
	double m_eta;                      // efficiency (-)
	double m_w;                        // specific work generated by the turbine (kJ/kg) [positive value]
	double m_m_dot;                    // mass flow rate through the turbine (kg/s)
	//double nu;                       // ratio of tip speed to spouting velocity (-)
	//double w_tip_ratio;              // ratio of the local (turbine inlet) speed of sound to the tip speed

public:
	~turbine(){};

	turbine()
	{
		m_D_rotor = m_A_nozzle = std::numeric_limits<double>::quiet_NaN();
	}

	bool initialize(const turbine_design_parameters & turb_des_par_in )
	{
		m_turb_des_par = turb_des_par_in;

		double nu = 0.707;					// Design-point ratio of tip speed to spouting velocity

		double C_s = sqrt(2.0*(m_turb_des_par.m_w_design / m_turb_des_par.m_eta_design)*1000.0);			// [m/s] Spouting velocity
		double U_tip = nu * C_s;											// Rearrange definition of nu
		m_D_rotor = U_tip / (0.5*m_turb_des_par.m_N_design*0.104719755);			// [m] Turbine diameter
		m_A_nozzle = m_turb_des_par.m_m_dot_design / (C_s * m_turb_des_par.m_rho_out_design);		// [m2] Turbine effective nozzle area in m2 (uses density at turbine outlet)

		if( m_A_nozzle != m_A_nozzle || m_D_rotor != m_D_rotor )
			return false;

		return true;
	}

	void solve_turbine(double T_in, double P_in, double P_out, double N_in, int & error_code, double & T_out, double & m_dot);

	double get_w()
	{
		return m_w;
	}

};

struct HX_design_parameters
{
	int m_N_sub;							//[-] Number of sub-heat exchangers used in the model
	std::vector<double> m_m_dot_design;	//[kg/s] Design-point mass flow rates of the two streams
	std::vector<double> m_DP_design;		//[kPa] Design-point pressure drops across the heat exchanger
	double m_UA_design;					//[kW/K] Design-point conductance
	double m_Q_dot_design;				//[kW] Design-point heat transfer
	double m_min_DT;					//[K] Minimum temperature difference in heat exchanger

	HX_design_parameters()
	{
		m_N_sub = -1;
		
		m_m_dot_design.resize(2);
		std::fill(m_m_dot_design.begin(), m_m_dot_design.end(), std::numeric_limits<double>::quiet_NaN());
		m_DP_design.resize(2);
		std::fill(m_DP_design.begin(), m_DP_design.end(), std::numeric_limits<double>::quiet_NaN());

		m_UA_design = m_min_DT = m_Q_dot_design = std::numeric_limits<double>::quiet_NaN();
	}
};

struct HX_off_design_outputs
{
	double m_UA_calc;
	std::vector<double> m_DP_calc;
	std::vector<double> m_m_dot_calc;
	double m_Q_dot_calc;
	double m_eff;
	double m_min_DT;

	HX_off_design_outputs()
	{
		m_m_dot_calc.resize(2);
		std::fill(m_m_dot_calc.begin(), m_m_dot_calc.end(), std::numeric_limits<double>::quiet_NaN());
		m_DP_calc.resize(2);
		std::fill(m_DP_calc.begin(), m_DP_calc.end(), std::numeric_limits<double>::quiet_NaN());

		m_UA_calc = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_calc = std::numeric_limits<double>::quiet_NaN();
		m_eff = std::numeric_limits<double>::quiet_NaN();
		m_min_DT = std::numeric_limits<double>::quiet_NaN();
	}
};

class HeatExchanger
{
private:
	// Parameter Structure
	HX_design_parameters m_HX_des_par;
	HX_off_design_outputs m_HX_od_out;

	// Useful design data?
	//double min_DT_design;			// minimum temperature difference in hxr (K)
	//double eff_design;				// heat exchanger effectiveness, defined as Q_dot_act / Q_dot_hot_stream_ideal (NOT Q_dot_act / Q_dot_max)
	//double C_dot_cold_design;		// cold stream capacitance rate (kW/K), defined as m_dot_cold * (delta_h_cold / delta_T_cold)
	//double C_dot_hot_design;		// hot stream capacitance rate (kW/K), defined as m_dot_hot * (delta_h_hot / delta_T_hot)

public:
	~HeatExchanger(){};

	HeatExchanger(){};

	void initialize(const HX_design_parameters & HX_des_par_in)
	{
		m_HX_des_par = HX_des_par_in;
		return;
	}

	const HX_off_design_outputs * get_od_outputs()
	{
		return &m_HX_od_out;
	}

	void set_od_outputs(const HX_off_design_outputs & HX_outputs_in)
	{
		m_HX_od_out = HX_outputs_in;
		return;
	}

	const HX_design_parameters * get_design_parameters()
	{
		return &m_HX_des_par;
	}

	// Check to see if HeatExchanger is initialized before these can be called?
	double hxr_DP(int stream, double m_dot, bool scale_DP);

	double hxr_UA(double m_dot_0, double m_dot_1, bool scale_UA);

};

class RecompCycle
{
private:
	// member classes
	compressor m_mc, m_rc;                                                // compressor and recompressor
	turbine m_t;													      // turbine
	HeatExchanger m_LT, m_HT, m_PHX, m_PC;								      // heat exchangers
	TrackErrors m_errors;

	// member structure
	cycle_design_parameters m_cycle_des_par;
	cycle_opt_off_des_inputs m_cycle_opt_off_des_in;
	cycle_off_des_inputs m_cycle_off_des_in;
	S_cycle_design_metrics m_cycle_des_metrics;
	S_off_design_performance m_cycle_od_performance;

	// member data: useful results from latest solution of 'design' method
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_W_dot_net_last;
	double m_eta_thermal_last;

	// member design data
	std::vector<double> m_temp_des, m_pres_des, m_enth_des, m_entr_des, m_dens_des;	// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_W_dot_net_des;
	double m_eta_thermal_des;

	// member auto design data
	double m_eta_thermal_autodes;
	double m_PR_mc_autodes;
	double m_recomp_frac_autodes;
	double m_LT_frac_autodes;
	double m_P_high_autodes;

	// member off-design data from latest solution of 'off_design' method
	std::vector<double> m_temp_od_last, m_pres_od_last, m_enth_od_last, m_entr_od_last, m_dens_od_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_W_dot_net_od_last;
	double m_q_dot_in_od_last;
	double m_eta_thermal_od_last;

	// member off-design data
	std::vector<double> m_temp_od, m_pres_od, m_enth_od, m_entr_od, m_dens_od;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_od;												  // thermal efficiency of the cycle (-)
	double m_W_dot_net_od;												  // net power output of the cycle (kW)
	double m_q_dot_in_od;												// thermal input to cycle

	bool design();

	bool off_design_target_power();

	void set_des_data()
	{
		m_temp_des = m_temp_last; m_pres_des = m_pres_last; m_enth_des = m_enth_last; m_entr_des = m_entr_last; m_dens_des = m_dens_last;
		m_W_dot_net_des = m_W_dot_net_last;
		m_eta_thermal_des = m_eta_thermal_last;

		m_cycle_des_metrics.m_eta_thermal = m_eta_thermal_des;
		m_cycle_des_metrics.m_W_dot_net = m_W_dot_net_des;
		m_cycle_des_metrics.m_min_DT_LT = m_LT.get_design_parameters()->m_min_DT;
		m_cycle_des_metrics.m_min_DT_HT = m_HT.get_design_parameters()->m_min_DT;
		m_cycle_des_metrics.m_N_mc = m_mc.get_N_design();
		m_cycle_des_metrics.m_T = m_temp_des;
		m_cycle_des_metrics.m_P = m_pres_des;
		m_cycle_des_metrics.m_m_dot_PHX = m_PHX.get_design_parameters()->m_m_dot_design[0];
		m_cycle_des_metrics.m_m_dot_PC = m_PC.get_design_parameters()->m_m_dot_design[1];
	}
	
	void set_autodes_opts()
	{
		m_eta_thermal_autodes = m_eta_thermal_des;
		m_PR_mc_autodes = m_cycle_des_par.m_PR_mc;
		m_recomp_frac_autodes = m_cycle_des_par.m_recomp_frac;
		m_LT_frac_autodes = m_cycle_des_par.m_LT_frac;
		m_P_high_autodes = m_cycle_des_par.m_P_mc_out;
	};

	void set_od_data()
	{
		m_temp_od = m_temp_od_last; m_pres_od = m_pres_od_last; m_enth_od = m_enth_od_last; m_entr_od = m_entr_od_last; m_dens_od = m_dens_od_last;
		m_W_dot_net_od = m_W_dot_net_od_last;
		m_eta_thermal_od = m_eta_thermal_od_last;

		// Set structure member data
		m_cycle_od_performance.m_eta_thermal = m_eta_thermal_od;
		m_cycle_od_performance.m_W_dot_net = m_W_dot_net_od;
		m_cycle_od_performance.m_q_dot_in = m_q_dot_in_od;
		m_cycle_od_performance.m_min_DT_HT = m_HT.get_od_outputs()->m_min_DT;
		m_cycle_od_performance.m_min_DT_LT = m_LT.get_od_outputs()->m_min_DT;
		m_cycle_od_performance.m_N_rc = m_rc.get_N_off_design();
		m_cycle_od_performance.m_m_dot_PHX = m_PHX.get_od_outputs()->m_m_dot_calc[0];
		m_cycle_od_performance.m_m_dot_PC = m_PC.get_od_outputs()->m_m_dot_calc[1];

		m_cycle_od_performance.m_T = m_temp_od;
		m_cycle_od_performance.m_P = m_pres_od;
	}

	void clear_member_data()
	{
		m_temp_last.resize(10);
		m_pres_last.resize(10);
		m_enth_last.resize(10);
		m_entr_last.resize(10);
		m_dens_last.resize(10);
		m_temp_des.resize(10);
		m_pres_des.resize(10);
		m_enth_des.resize(10);
		m_entr_des.resize(10);
		m_dens_des.resize(10);
		m_temp_od_last.resize(10);
		m_pres_od_last.resize(10);
		m_enth_od_last.resize(10);
		m_entr_od_last.resize(10);
		m_dens_od_last.resize(10);
		m_temp_od.resize(10);
		m_pres_od.resize(10);
		m_enth_od.resize(10);
		m_entr_od.resize(10);
		m_dens_od.resize(10);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_pres_last.begin(), m_pres_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_enth_last.begin(), m_enth_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_entr_last.begin(), m_entr_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_dens_last.begin(), m_dens_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_temp_des.begin(), m_temp_des.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_pres_des.begin(), m_pres_des.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_enth_des.begin(), m_enth_des.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_entr_des.begin(), m_entr_des.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_dens_des.begin(), m_dens_des.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_temp_od_last.begin(), m_temp_od_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_pres_od_last.begin(), m_pres_od_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_enth_od_last.begin(), m_enth_od_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_entr_od_last.begin(), m_entr_od_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_dens_od_last.begin(), m_dens_od_last.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_temp_od.begin(), m_temp_od.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_pres_od.begin(), m_pres_od.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_enth_od.begin(), m_enth_od.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_entr_od.begin(), m_entr_od.end(), std::numeric_limits<double>::quiet_NaN());
		std::fill(m_dens_od.begin(), m_dens_od.end(), std::numeric_limits<double>::quiet_NaN());
		m_W_dot_net_last = m_eta_thermal_last =
			m_W_dot_net_des = m_eta_thermal_des =
			m_W_dot_net_od_last = m_q_dot_in_od_last = m_eta_thermal_od_last =
			m_W_dot_net_od = m_q_dot_in_od = m_eta_thermal_od = std::numeric_limits<double>::quiet_NaN();

		m_eta_thermal_autodes = -HUGE_VAL;
		m_PR_mc_autodes = m_recomp_frac_autodes = m_LT_frac_autodes = m_P_high_autodes = std::numeric_limits<double>::quiet_NaN();
	}

public:
	~RecompCycle(){};

	RecompCycle(const cycle_design_parameters & cycle_des_par_in)
	{
		m_cycle_des_par = cycle_des_par_in;
		clear_member_data();
	}

	void set_design_parameters(const cycle_design_parameters & cycle_des_par_in)
	{
		m_cycle_des_par = cycle_des_par_in;
		clear_member_data();
	}

	const cycle_design_parameters * get_cycle_design_parameters()
	{
		return &m_cycle_des_par;
	}

	const S_cycle_design_metrics * get_cycle_design_metrics()
	{
		return &m_cycle_des_metrics;
	}

	const S_off_design_performance * get_off_design_outputs()
	{
		return &m_cycle_od_performance;
	}

	const cycle_off_des_inputs * get_off_design_inputs()
	{
		return &m_cycle_off_des_in;
	}

	bool optimal_design();

	bool auto_optimal_design();

	bool design_no_opt();

	double design_point_eta(const std::vector<double> &x);

	double opt_eta(double P_high_opt);

	bool optimal_off_design(const cycle_opt_off_des_inputs & cycle_opt_off_des_in_in);

	double off_design_target_power_function(const std::vector<double> &x);

	bool off_design(const cycle_off_des_inputs & cycle_off_des_in_in);

};

void calculate_turbomachinery_outlet(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/, 
	                                 double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/ );

void calculate_hxr_UA(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
	                  int & error_code, double & UA, double & min_DT);

double P_pseudocritical(double T_K);

double fmin_callback_opt_eta(double x, void *data);

double nlopt_callback_opt_des(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_callback_opt_off_des(const std::vector<double> &x, std::vector<double> &grad, void *data);

#endif




