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

#ifndef __csp_solver_pc_Rankine_indirect_224_
#define __csp_solver_pc_Rankine_indirect_224_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

#include "lib_util.h"
#include "htf_props.h"

#include "ud_power_cycle.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_radiator.h"
#include "csp_solver_stratified_tes.h"

#include <set>
#include <algorithm>

class C_pc_Rankine_indirect_224 : public C_csp_power_cycle
{
private:
	bool m_is_initialized;

	double m_F_wcMax;
	double m_F_wcMin;
	double m_delta_h_steam;
	double m_startup_energy_required;
	double m_eta_adj;

	double m_m_dot_design;				//[kg/hr]
	double m_m_dot_max;					//[kg/hr]
	double m_m_dot_min;					//[kg/hr]
	double m_q_dot_design;				//[MWt]
	double m_cp_htf_design;				//[kJ/kg-K]

	C_csp_power_cycle::E_csp_power_cycle_modes m_operating_mode_prev;
	double m_startup_time_remain_prev;		//[hr]
	double m_startup_energy_remain_prev;	//[kW-hr]

    C_csp_power_cycle::E_csp_power_cycle_modes m_operating_mode_calc;
	double m_startup_time_remain_calc;
	double m_startup_energy_remain_calc;

	util::matrix_t<double> m_db;

	HTFProperties mc_pc_htfProps;

	// member string for exception messages
	std::string m_error_msg;

	// member class for User Defined Power Cycle
	C_ud_power_cycle mc_user_defined_pc;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

	double GetFieldToTurbineTemperatureDropC() { return 25.0; }

	//void RankineCycle(/*double time,*/double P_ref, double eta_ref, double T_htf_hot_ref, double T_htf_cold_ref, double T_db, double T_wb,
	//	double P_amb, double dT_cw_ref, /*double HTF,*/ double c_p_w, double T_htf_hot, double m_dot_htf, int /*double*/ mode,
	//	double demand_var, double P_boil, /*double tech_type,*/ double T_amb_des, double T_approach, double F_wc, double F_wcmin,
	//	double F_wcmax, double T_ITD_des, double P_cond_ratio, /*double CT,*/ double P_cond_min, /*double n_pl_inc,*/
	//	/*double& fcall, */ double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref,
	//	double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond);

    void RankineCycle(double T_db, double T_wb,
		double P_amb, double T_htf_hot, double m_dot_htf, int mode,
		double demand_var, double P_boil, double F_wc, double F_wcmin, double F_wcmax, double T_cold, double dT_cw,
        //outputs
        double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref,
		double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond, double &T_cond_out);

	double Interpolate(int YT, int XT, double X, double Z = std::numeric_limits<double>::quiet_NaN());

	// Isopentane
	double T_sat4(double P/*Bar*/) 
	{
		return 284.482349 + 20.8848464*P - 1.5898147*P*P + 0.0655241456*P*P*P - 0.0010168822*P*P*P*P; /*return value in Kelvin*/
	}
   
public:
	
    enum
    {
        E_COL_T_HTF,
        E_COL_M_DOT,
        E_COL_T_AMB,
        E_COL_W_CYL,
        E_COL_Q_CYL,
        E_COL_W_COOL,
        E_COL_M_H2O
    };

    enum
	{
		E_ETA_THERMAL,		//[-] Cycle thermal efficiency (gross)
		E_Q_DOT_HTF,		//[MWt] Cycle thermal power input
		E_M_DOT_HTF,		//[kg/hr] Cycle HTF mass flow rate
		E_Q_DOT_STARTUP,	//[MWt] Cycle startup thermal power
		E_W_DOT,			//[MWe] Cycle electricity output (gross)
		E_T_HTF_IN,			//[C] Cycle HTF inlet temperature
		E_T_HTF_OUT,		//[C] Cycle HTF outlet temperature
		E_T_COND_OUT,		//[C] Cycle condenser water outlet temperature
		E_T_COLD,			//[C] Cold storage temperature
		E_M_COLD,			//[C] Cold storage mass
		E_M_WARM,			//[C] Cold storage warm (return) tank mass
		E_T_WARM,			//[C] Cold storage warm (return) tank temperature
		E_T_RADOUT,			//[C] Radiator outlet temperature
		E_M_DOT_WATER,		//[kg/hr] Cycle water consumption: makeup + cooling	
		E_P_COND,			//[Pa] Cycle condensing pressure
		E_RADCOOL_CNTRL,	//Code showing the status of radiative cooling with cold storage

		// Variables added for backwards compatability with TCS
		E_M_DOT_HTF_REF		//[kg/hr] HTF mass flow rate at design
	};

	C_csp_reported_outputs mc_reported_outputs;

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;
	
	// Instantiate two fully mixed tanks class for cold storage AND three node model
	C_csp_cold_tes mc_two_tank_ctes;
	C_csp_cold_tes::S_csp_cold_tes_outputs mc_two_tank_ctes_outputs;	// for outputs
	C_csp_stratified_tes mc_stratified_ctes;
	C_csp_stratified_tes::S_csp_strat_tes_outputs mc_stratified_ctes_outputs;

	double m_dot_cold_avail;
	double m_dot_warm_avail;
	double m_dot_condenser;
	double T_warm_prev_K;
	double T_cold_prev_K;
	double T_cold_prev;
	double dT_cw_design;
	double T_s_measured;
	double T_s_corr;
	double T_s_K;
	int idx_time;

	// Instantiate radiator model
	C_csp_radiator mc_radiator;
	double m_dot_radfield;
	double m_dot_radact;
	double W_radpumptest;
	struct S_params
	{
			// Parameters with common SSCINPUT name for both Rankine Cycle and User Defined Cycle
		double m_P_ref;				//[MW] design electric power output, converted to kW in init()
		double m_eta_ref;			//[%] design conversion efficiency
		double m_T_htf_hot_ref;		//[C] design HTF inlet temperature
		double m_T_htf_cold_ref;	//[C] design HTF output temperature
		double m_cycle_max_frac;	//[-] Maximum turbine over-design operation fraction
		double m_cycle_cutoff_frac;	//[-] Minimum turbine operation fraction
		double m_q_sby_frac;		//[-] fraction of thermal power required for standby mode
		double m_startup_time;		//[hr] time needed for power block startup
		double m_startup_frac;		//[-] fraction of design thermal power needed for startup
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle
		int m_pc_fl;				//[-] integer flag identifying Heat Transfer Fluid (HTF) in power block {1-27}
		util::matrix_t<double> m_pc_fl_props;
        double DP_SGS;              //[bar] pressure drop within the steam generator system


		// Steam Rankine or User-Defined
		bool m_is_user_defined_pc;				//[-] True: user-defined power cycle, False: Built-in Rankine Cycle model

			// Parameters that have different SSCINPUT names for Rankine Cycle and User Defined Cycle
		double m_dT_cw_ref;			//[C] design temp difference between cooling water inlet/outlet
		double m_T_amb_des;			//[C] design ambient temperature
		double m_P_boil;			//[bar] boiler operating pressure
		int m_CT;					//[-] integer flag for cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
		int m_tech_type;			//[-] Flag indicating which coef. set to use. (1=tower,2=trough,3=user) 
		double m_T_approach;		//[C] cooling tower approach temp
		double m_T_ITD_des;			//[C] design ITD for dry system
		double m_P_cond_ratio;		//[-] condenser pressure ratio
		double m_pb_bd_frac;		//[-] blowdown steam fraction
		double m_P_cond_min;		//[inHG] minimum condenser pressure, converted to Pa in code
		int m_n_pl_inc;				//[-] Number of part-load increments for the heat rejection system
		
		
		std::vector<double> m_F_wc;		//[-] hybrid cooling dispatch fractions 1 thru 9 (array index 0-8)	
		
		// Parameters for user-defined power cycle
			// Lookup table with dependent variables corresponding to parametric on independent variable T_htf_hot [C] (first column)
		util::matrix_t<double> mc_T_htf_ind;	// At m_dot_htf levels (-, 0, +)
		double m_T_htf_low;			//[C] Low level of T_htf corresponding to ambient temperature parametric (also must be included within range of independent T_HTF values)
		double m_T_htf_high;		//[C] High level of T_htf corresponding to ambient temperature parametric (also must be included within range of independent T_HTF values)
			// Lookup table with dependent variables corresponding to parametric on independent variable T_amb [C] (first column)
		util::matrix_t<double> mc_T_amb_ind;	// At T_htf levels (-, 0, +)
		double m_T_amb_low;			//[C] Low level of T_amb corresponding to m_dot_HTF parametric (also must be included within range of independent T_amb values)
		double m_T_amb_high;		//[C] High level of T_amb corresponding to m_dot_HTF parametric (also must be included within range of independent T_amb values)
			// Lookup table with dependent variables corresponding to parametric on independent variable m_dot_htf [ND] (first column)
		util::matrix_t<double> mc_m_dot_htf_ind;	// At T_amb levels (-, 0, +)
		double m_m_dot_htf_low;		//[-] Low level of m_dot_htf corresponding to T_HTF parametric (also must be included within range of independent T_htf values)
		double m_m_dot_htf_high;	//[-] High level of m_dot_htf corresponding to T_HTF parametric (also must be included within range of independent T_htf_values)
            // Lookup table that is the combination of the three above T_htf_hot, T_amb, and m_dot_htf tables (this is the newer table format)
        util::matrix_t<double> mc_combined_ind;

		double m_W_dot_cooling_des;		//[MW] Cooling parasitic at design conditions
		double m_m_dot_water_des;		//[kg/s] Power cycle water use at design conditions

		S_params()
		{
			m_P_ref = m_eta_ref = m_T_htf_hot_ref = m_T_htf_cold_ref = m_dT_cw_ref = m_T_amb_des =
				m_q_sby_frac = m_P_boil = m_startup_time = m_startup_frac = m_T_approach = m_T_ITD_des =
				m_P_cond_ratio = m_pb_bd_frac = m_P_cond_min = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();

			m_pc_fl = m_CT = m_tech_type = m_n_pl_inc = -1;

			// Initialize parameters for user-defined power cycle
			m_is_user_defined_pc = false;
			
			m_T_htf_low = m_T_htf_high =
				m_T_amb_low = m_T_amb_high =
				m_m_dot_htf_low = m_m_dot_htf_high =				
				m_W_dot_cooling_des = m_m_dot_water_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_params ms_params;

	C_pc_Rankine_indirect_224();

	~C_pc_Rankine_indirect_224(){};

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
	virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser = 0);
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

void get_var_setup(std::vector<double> & vec_unique, std::vector<double> & var_vec,
    double & var_des, double & var_low, double & var_high);

bool is_level_in_par(std::vector<std::vector<double>> test_combs,
    std::vector<std::vector<double>> full_table);

int split_ind_tbl(util::matrix_t<double> &combined, util::matrix_t<double> &T_htf_ind,
    util::matrix_t<double> &m_dot_ind, util::matrix_t<double> &T_amb_ind);

int split_ind_tbl(util::matrix_t<double> &combined, util::matrix_t<double> &T_htf_ind,
    util::matrix_t<double> &m_dot_ind, util::matrix_t<double> &T_amb_ind,
    int & n_T_htf_pars, int & n_T_amb_pars, int & n_m_dot_pars,
    double & m_dot_low, double & m_dot_des, double & m_dot_high,
    double & T_htf_low, double & T_htf_des, double & T_htf_high,
    double & T_amb_low, double & T_amb_des, double & T_amb_high);

int combine_ind_tbl(util::matrix_t<double>& combined, util::matrix_t<double>& T_htf_ind,
	util::matrix_t<double>& m_dot_ind, util::matrix_t<double>& T_amb_ind,
	double m_dot_low, double m_dot_des, double m_dot_high,
	double T_htf_low, double T_htf_des, double T_htf_high,
	double T_amb_low, double T_amb_des, double T_amb_high);


#endif //__csp_solver_pc_Rankine_indirect_224_
