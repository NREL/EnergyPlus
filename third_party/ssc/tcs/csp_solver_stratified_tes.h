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

#ifndef __csp_solver_stratified_tes_
#define __csp_solver_stratified_tes_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"
#include "csp_solver_two_tank_tes.h"

class C_storage_node
{
private:
	HTFProperties mc_htf;

	double m_V_total;			//[m^3] Total volume for *one temperature* tank
	double m_V_active;			//[m^3] active volume of *one temperature* tank (either cold or hot)
	double m_V_inactive;		//[m^3] INactive volume of *one temperature* tank (either cold or hot)
	double m_UA;				//[W/K] Tank loss conductance

	double m_T_htr;				//[K] Tank heater set point
	double m_max_q_htr;			//[MWt] Max tank heater capacity

								// Stored values from end of previous timestep
	double m_V_prev;		//[m^3] Volume of storage fluid in tank
	double m_T_prev;		//[K] Temperature of storage fluid in tank
	double m_m_prev;		//[kg] Mass of storage fluid in tank

							// Calculated values for current timestep
	double m_V_calc;		//[m^3] Volume of storage fluid in tank
	double m_T_calc;		//[K] Temperature of storage fluid in tank
	double m_m_calc;		//[kg] Mass of storage fluid in tank

	public:

	C_storage_node();

	double calc_mass_at_prev();

	double get_m_T_prev();

	double get_m_T_calc();

	double get_m_m_calc();

	void init(HTFProperties htf_class_in, double V_tank_one_temp, double h_tank, bool lid, double u_tank,
		double tank_pairs, double T_htr, double max_q_htr, double V_ini, double T_ini);

	double m_dot_available(double f_unavail, double timestep);

	void energy_balance(double timestep /*s*/, double m_dot_in, double m_dot_out, double T_in /*K*/, double T_amb /*K*/,
		double &T_ave /*K*/, double &q_heater /*MW*/, double &q_dot_loss /*MW*/);

	void energy_balance_constant_mass(double timestep /*s*/, double m_dot_in, double T_in /*K*/, double T_amb /*K*/,
		double &T_ave /*K*/, double &q_heater /*MW*/, double &q_dot_loss /*MW*/);

	void converged();
};


class C_csp_stratified_tes //Class for cold storage based on two tank tes ARD
{
private:

	HTFProperties mc_field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties mc_store_htfProps;		// Instance of HTFProperties class for storage HTF

	C_hx_cold_tes mc_hx;

	//Storage_HX mc_hx_storage;				// Instance of Storage_HX class for heat exchanger between storage and field HTFs

	C_storage_node mc_node_one;				// Instance of storage node class for the top node
	C_storage_node mc_node_two;				 
	C_storage_node mc_node_three;			
	C_storage_node mc_node_four;			
	C_storage_node mc_node_five;			// Upto six nodes allowed
	C_storage_node mc_node_n;				// Instance of storage node class for the bottom node
											// member string for exception messages
	std::string error_msg;

	// Timestep data
	double m_m_dot_tes_dc_max;
	double m_m_dot_tes_ch_max;

	// Member data
	bool m_is_tes;
	double m_vol_tank;			//[m3] volume of *one temperature*, i.e. vol_tank = total cold storage = total hot storage
	double m_V_tank_active;		//[m^3] available volume (considering h_min) of *one temperature*
	double m_q_pb_design;		//[Wt] thermal power to power cycle at design
	double m_V_tank_hot_ini;	//[m^3] Initial volume in hot storage tank

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_csp_strat_tes_outputs
	{
		double m_q_heater;			//[MWe]  Heating power required to keep tanks at a minimum temperature
		double m_m_dot;             //[kg/s] Hot tank mass flow rate, valid for direct and indirect systems
		double m_W_dot_rhtf_pump;	//[MWe]  Pumping power, just for tank-to-tank in indirect storage
		double m_q_dot_loss;		//[MWt]  Storage thermal losses
		double m_q_dot_dc_to_htf;	//[MWt]  Thermal power to the HTF from storage
		double m_q_dot_ch_from_htf;	//[MWt]  Thermal power from the HTF to storage
		double m_T_hot_ave;		    //[K]    Average hot tank temperature over timestep
		double m_T_cold_ave;	    //[K]    Average cold tank temperature over timestep
		double m_T_hot_final;	    //[K]    Hot tank temperature at end of timestep
		double m_T_cold_final;	    //[K]    Cold tank temperature at end of timestep

		S_csp_strat_tes_outputs()
		{
			m_q_heater = m_m_dot = m_W_dot_rhtf_pump = m_q_dot_loss = m_q_dot_dc_to_htf = m_q_dot_ch_from_htf =
				m_T_hot_ave = m_T_cold_ave = m_T_hot_final = m_T_cold_final = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_params
	{
		int m_field_fl;
		util::matrix_t<double> m_field_fl_props;

		int m_tes_fl;
		util::matrix_t<double> m_tes_fl_props;

		bool m_is_hx;

		double m_W_dot_pc_design;   //[MWe] Design point gross power cycle output
		double m_eta_pc_factor;     //[-] Factor accounting for Design point power cycle thermal efficiency
		double m_solarm;			//[-] solar multiple
		double m_ts_hours;			//[hr] hours of storage at design power cycle operation		
		double m_h_tank;			//[m] tank height
		double m_u_tank;			//[W/m^2-K]
		int m_tank_pairs;			//[-]
		double m_hot_tank_Thtr;		//[C] convert to K in init()
		double m_hot_tank_max_heat;	//[MW]
		double m_cold_tank_Thtr;	//[C] convert to K in init()
		double m_cold_tank_max_heat;//[MW]
		double m_dt_hot;			//[C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
		double m_T_field_in_des;	//[C] convert to K in init()
		double m_T_field_out_des;	//[C] convert to K in init()
		double m_T_tank_hot_ini;	//[C] Initial temperature in hot storage tank
		double m_T_tank_cold_ini;	//[C] Initial temperature in cold storage cold
		double m_h_tank_min;		//[m] Minimum allowable HTF height in storage tank
		double m_f_V_hot_ini;       //[%] Initial fraction of available volume that is hot

		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

		double dT_cw_rad;			//[degrees] Temperature change in cooling water for cold storage cooling.
		double m_dot_cw_rad;		//[kg/sec]	Mass flow of cooling water for cold storage cooling at design.
		int m_ctes_type;			//2= two tank (this model) 3=three node (other model)
		double m_dot_cw_cold;		//[kg/sec]	Mass flow of storage water between cold storage and radiative field HX.
		double m_lat;			//Latitude [degrees]
		S_params()
		{
			m_field_fl = m_tes_fl = m_tank_pairs = -1;
			m_is_hx = true;

			m_ts_hours = 0.0;		//[hr] Default to 0 so that if storage isn't defined, simulation won't crash

			m_W_dot_pc_design = m_eta_pc_factor = m_solarm = m_h_tank = m_u_tank = m_hot_tank_Thtr = m_hot_tank_max_heat = m_cold_tank_Thtr =
				m_cold_tank_max_heat = m_dt_hot = m_T_field_in_des = m_T_field_out_des = m_T_tank_hot_ini =
				m_T_tank_cold_ini = m_h_tank_min = m_f_V_hot_ini = m_htf_pump_coef = dT_cw_rad = m_dot_cw_rad = m_dot_cw_cold=m_lat= std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_params ms_params;

	C_csp_stratified_tes();

	~C_csp_stratified_tes() {};

	void init(const C_csp_tes::S_csp_tes_init_inputs init_inputs);

	bool does_tes_exist();

	double get_hot_temp();

	double get_cold_temp();

	double get_hot_mass();

	double get_cold_mass();

	double get_hot_mass_prev();

	double get_cold_mass_prev();

	double get_physical_volume(); //m^3

	double get_hot_massflow_avail(double step_s); //kg/sec

	double get_cold_massflow_avail(double step_s); //kg/sec

	double get_initial_charge_energy(); //MWh

	double get_min_charge_energy(); //MWh

	double get_max_charge_energy(); //MWh

	double get_degradation_rate();  // s^-1

	void discharge_avail_est(double T_cold_K, double step_s, double &q_dot_dc_est, double &m_dot_field_est, double &T_hot_field_est);

	void charge_avail_est(double T_hot_K, double step_s, double &q_dot_ch_est, double &m_dot_field_est, double &T_cold_field_est);

	// Calculate pumping power...???
	bool discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
		double T_htf_cold_in, double & T_htf_hot_out /*K*/, S_csp_strat_tes_outputs &outputs);

    void discharge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_cold_in, 
		double & T_htf_hot_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_strat_tes_outputs &outputs);

	bool charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
		double T_htf_hot_in, double & T_htf_cold_out /*K*/, S_csp_strat_tes_outputs &outputs);

	bool charge_discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_hot_in /*kg/s*/, 
		double T_hot_in, double m_dot_cold_in /*kg/s*/, double T_cold_in, S_csp_strat_tes_outputs &outputs);

	bool recirculation(double timestep /*s*/, double T_amb /*K*/, double m_dot_cold_in /*kg/s*/, 
		double T_cold_in /*K*/, S_csp_strat_tes_outputs &outputs);

	bool stratified_tanks(double timestep /*s*/, double T_amb /*K*/, double m_dot_cond /*kg/s*/, 
		double T_cond_out /*K*/, double m_dot_rad /*kg/s*/, double T_rad_out /*K*/, S_csp_strat_tes_outputs &outputs);

	void charge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_hot_in /*K*/, 
		double & T_htf_cold_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_strat_tes_outputs &outputs);

	void idle(double timestep, double T_amb, S_csp_strat_tes_outputs &outputs);

	void converged();

    int pressure_drops(double m_dot_sf, double m_dot_pb,
        double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
        double &P_drop_col, double &P_drop_gen);

    double pumping_power(double m_dot_sf, double m_dot_pb, double m_dot_tank,
        double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating);
};




#endif   //__csp_solver_stratified_tes_
