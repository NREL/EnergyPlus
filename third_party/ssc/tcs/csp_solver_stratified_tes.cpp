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

#include "csp_solver_stratified_tes.h"
#include "csp_solver_util.h"


C_storage_node::C_storage_node()
{
	m_V_prev = m_T_prev = m_m_prev =

		m_V_total = m_V_active = m_V_inactive = m_UA =

		m_T_htr = m_max_q_htr = std::numeric_limits<double>::quiet_NaN();
}

void C_storage_node::init(HTFProperties htf_class_in, double V_tank_one_temp, double h_tank, bool lid, double u_tank,
	double tank_pairs, double T_htr, double max_q_htr, double V_ini, double T_ini)
{
	mc_htf = htf_class_in;

	m_V_total = V_tank_one_temp;						//[m^3]

	double A_cs = m_V_total / (h_tank*tank_pairs);		//[m^2] Cross-sectional area of a single tank

	double diameter = pow(A_cs / CSP::pi, 0.5)*2.0;		//[m] Diameter of a single tank

	if (lid)
	{// Calculate tank conductance if including top area in losses (top node of stratified tank.)
		m_UA = u_tank * (A_cs + CSP::pi*diameter*h_tank)*tank_pairs;	//[W/K]
	}
	if (!lid)
	{// Calculate tank conductance if only including sides of node
		m_UA = u_tank * (CSP::pi*diameter*h_tank)*tank_pairs;			//[W/K]

	}
	m_T_htr = T_htr;
	m_max_q_htr = max_q_htr;

	m_V_prev = V_ini;
	m_T_prev = T_ini;
	m_m_prev = calc_mass_at_prev();
}

double C_storage_node::calc_mass_at_prev()
{
	return m_V_prev * mc_htf.dens(m_T_prev, 1.0);	//[kg] 
}

double C_storage_node::get_m_T_prev()
{
	return m_T_prev;		//[K]
}

double C_storage_node::get_m_T_calc()
{
	return m_T_calc;
}

double C_storage_node::get_m_m_calc() //ARD new getter for current mass 
{
	return m_m_calc;
}

double C_storage_node::m_dot_available(double f_unavail, double timestep)
{
	double rho = mc_htf.dens(m_T_prev, 1.0);		//[kg/m^3]
	double V = m_m_prev / rho;						//[m^3] Volume available in tank (one temperature)
	double V_avail = fmax(V - m_V_inactive, 0.0);	//[m^3] Volume that is active - need to maintain minimum height (corresponding m_V_inactive)

													// "Unavailable" fraction now applied to one temperature tank volume, not total tank volume
	double m_dot_avail = fmax(V_avail - m_V_active * f_unavail, 0.0)*rho / timestep;		//[kg/s] Max mass flow rate available

	return m_dot_avail;		//[kg/s]
}

void C_storage_node::converged()
{
	// Reset 'previous' timestep values to 'calculated' values
	m_V_prev = m_V_calc;		//[m^3]
	m_T_prev = m_T_calc;		//[K]
	m_m_prev = m_m_calc;		//[kg]
}

void C_storage_node::energy_balance(double timestep /*s*/, double m_dot_in, double m_dot_out, double T_in /*K*/, double T_amb /*K*/,
	double &T_ave /*K*/, double & q_heater /*MW*/, double & q_dot_loss /*MW*/)
{
	// Get properties from tank state at the end of last time step
	double rho = mc_htf.dens(m_T_prev, 1.0);	//[kg/m^3]
	double cp = mc_htf.Cp(m_T_prev)*1000.0;		//[J/kg-K] spec heat, convert from kJ/kg-K

												// Calculate ending volume levels
	m_m_calc = fmax(0.001, m_m_prev + timestep * (m_dot_in - m_dot_out));	//[kg] Available mass at the end of this timestep, limit to nonzero positive number
	m_V_calc = m_m_calc / rho;					//[m^3] Available volume at end of timestep (using initial temperature...)		

	if ((m_dot_in - m_dot_out) != 0.0)
	{
		double a_coef = m_dot_in * T_in + m_UA / cp * T_amb;
		double b_coef = m_dot_in + m_UA / cp;
		double c_coef = (m_dot_in - m_dot_out);

		m_T_calc = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef);
		T_ave = a_coef / b_coef + m_m_prev * (m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow((timestep*c_coef / m_m_prev + 1.0), 1.0 - b_coef / c_coef) - 1.0);
		q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]

		if (m_T_calc < m_T_htr)
		{
			q_heater = b_coef * ((m_T_htr - m_T_prev * pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef)) /
				(-pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef) + 1)) - a_coef;

			q_heater = q_heater * cp;

			q_heater /= 1.E6;
		}
		else
		{
			q_heater = 0.0;
			return;
		}

		if (q_heater > m_max_q_htr)
		{
			q_heater = m_max_q_htr;
		}

		a_coef += q_heater * 1.E6 / cp;

		m_T_calc = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef);
		T_ave = a_coef / b_coef + m_m_prev * (m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow((timestep*c_coef / m_m_prev + 1.0), 1.0 - b_coef / c_coef) - 1.0);
		q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]

	}
	else	// No mass flow rate, tank is idle
	{
		double b_coef = m_UA / (cp*m_m_prev);
		double c_coef = m_UA / (cp*m_m_prev) * T_amb;

		m_T_calc = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef * timestep);
		T_ave = c_coef / b_coef - (m_T_prev - c_coef / b_coef) / (b_coef*timestep)*(exp(-b_coef * timestep) - 1.0);
		q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;

		if (m_T_calc < m_T_htr)
		{
			q_heater = (b_coef*(m_T_htr - m_T_prev * exp(-b_coef * timestep)) / (-exp(-b_coef * timestep) + 1.0) - c_coef)*cp*m_m_prev;
			q_heater /= 1.E6;	//[MW]
		}
		else
		{
			q_heater = 0.0;
			return;
		}

		if (q_heater > m_max_q_htr)
		{
			q_heater = m_max_q_htr;
		}

		c_coef += q_heater * 1.E6 / (cp*m_m_prev);

		m_T_calc = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef * timestep);
		T_ave = c_coef / b_coef - (m_T_prev - c_coef / b_coef) / (b_coef*timestep)*(exp(-b_coef * timestep) - 1.0);
		q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]
	}
}

void C_storage_node::energy_balance_constant_mass(double timestep /*s*/, double m_dot_in, double T_in /*K*/, double T_amb /*K*/,
	double &T_ave /*K*/, double & q_heater /*MW*/, double & q_dot_loss /*MW*/)
{
	// Get properties from tank state at the end of last time step
	double rho = mc_htf.dens(m_T_prev, 1.0);	//[kg/m^3]
	double cp = mc_htf.Cp(m_T_prev)*1000.0;		//[J/kg-K] spec heat, convert from kJ/kg-K

												// Calculate ending volume levels
	m_m_calc = m_m_prev;						//[kg] Available mass at the end of this timestep, same as previous
	m_V_calc = m_m_calc / rho;					//[m^3] Available volume at end of timestep (using initial temperature...)		

	//Analytical method to calculate final temperature at end of timestep
	double a_coef = m_dot_in / m_m_calc + m_UA / (m_m_calc*cp);
	double b_coef = m_dot_in / m_m_calc * T_in + m_UA / (m_m_calc*cp)*T_amb;

	m_T_calc = b_coef / a_coef - (b_coef / a_coef - m_T_prev)*exp(-a_coef * timestep);
	T_ave = b_coef / a_coef - (b_coef / a_coef - m_T_prev)*exp(-a_coef * timestep / 2); //estimate of average

	q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]
																					
	q_heater = 0.0;								//Assume no heater.
	return;
	
}


C_csp_stratified_tes::C_csp_stratified_tes()
{
	m_vol_tank = m_V_tank_active = m_q_pb_design = m_V_tank_hot_ini = std::numeric_limits<double>::quiet_NaN();

	m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();
}

void C_csp_stratified_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
	if (!(ms_params.m_ts_hours > 0.0))
	{
		m_is_tes = false;
		return;		// No storage!
	}

	m_is_tes = true;

	// Declare instance of fluid class for FIELD fluid
	// Set fluid number and copy over fluid matrix if it makes sense
	if (ms_params.m_field_fl != HTFProperties::User_defined && ms_params.m_field_fl < HTFProperties::End_Library_Fluids)
	{
		if (!mc_field_htfProps.SetFluid(ms_params.m_field_fl))
		{
			throw(C_csp_exception("Field HTF code is not recognized", "Two Tank TES Initialization"));
		}
	}
	else if (ms_params.m_field_fl == HTFProperties::User_defined)
	{
		int n_rows = (int)ms_params.m_field_fl_props.nrows();
		int n_cols = (int)ms_params.m_field_fl_props.ncols();
		if (n_rows > 2 && n_cols == 7)
		{
			if (!mc_field_htfProps.SetUserDefinedFluid(ms_params.m_field_fl_props))
			{
				error_msg = util::format(mc_field_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
			}
		}
		else
		{
			error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Field HTF code is not recognized", "Two Tank TES Initialization"));
	}


	// Declare instance of fluid class for STORAGE fluid.
	// Set fluid number and copy over fluid matrix if it makes sense.
	if (ms_params.m_tes_fl != HTFProperties::User_defined && ms_params.m_tes_fl < HTFProperties::End_Library_Fluids)
	{
		if (!mc_store_htfProps.SetFluid(ms_params.m_tes_fl))
		{
			throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
		}
	}
	else if (ms_params.m_tes_fl == HTFProperties::User_defined)
	{
		int n_rows = (int)ms_params.m_tes_fl_props.nrows();
		int n_cols = (int)ms_params.m_tes_fl_props.ncols();
		if (n_rows > 2 && n_cols == 7)
		{
			if (!mc_store_htfProps.SetUserDefinedFluid(ms_params.m_tes_fl_props))
			{
				error_msg = util::format(mc_store_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
			}
		}
		else
		{
			error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
	}

	bool is_hx_calc = true;

	if (ms_params.m_tes_fl != ms_params.m_field_fl)
		is_hx_calc = true;
	else if (ms_params.m_field_fl != HTFProperties::User_defined)
		is_hx_calc = false;
	else
	{
		is_hx_calc = !mc_field_htfProps.equals(&mc_store_htfProps);
	}

	if (ms_params.m_is_hx != is_hx_calc)
	{
		if (is_hx_calc)
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are different, but the inputs did not specify a field-to-storage heat exchanger. The system was modeled assuming a heat exchanger.");
		else
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are identical, but the inputs specified a field-to-storage heat exchanger. The system was modeled assuming no heat exchanger.");

		ms_params.m_is_hx = is_hx_calc;
	}

	// Calculate thermal power to PC at design
	m_q_pb_design = ms_params.m_W_dot_pc_design / ms_params.m_eta_pc_factor*1.E6;	//[Wt] - using pc efficiency factor for cold storage ARD

																					// Convert parameter units
	ms_params.m_hot_tank_Thtr += 273.15;		//[K] convert from C
	ms_params.m_cold_tank_Thtr += 273.15;		//[K] convert from C
	ms_params.m_T_field_in_des += 273.15;		//[K] convert from C
	ms_params.m_T_field_out_des += 273.15;		//[K] convert from C
	ms_params.m_T_tank_hot_ini += 273.15;		//[K] convert from C
	ms_params.m_T_tank_cold_ini += 273.15;		//[K] convert from C


	double Q_tes_des = m_q_pb_design / 1.E6 * ms_params.m_ts_hours;		//[MWt-hr] TES thermal capacity at design

	double d_tank_temp = std::numeric_limits<double>::quiet_NaN();
	double q_dot_loss_temp = std::numeric_limits<double>::quiet_NaN();
	two_tank_tes_sizing(mc_store_htfProps, Q_tes_des, ms_params.m_T_field_out_des, ms_params.m_T_field_in_des,
		ms_params.m_h_tank_min, ms_params.m_h_tank, ms_params.m_tank_pairs, ms_params.m_u_tank,
		m_V_tank_active, m_vol_tank, d_tank_temp, q_dot_loss_temp);

	// 5.13.15, twn: also be sure that hx is sized such that it can supply full load to power cycle, in cases of low solar multiples
	double duty = m_q_pb_design * fmax(1.0, ms_params.m_solarm);		//[W] Allow all energy from the field to go into storage at any time

	if (ms_params.m_ts_hours > 0.0)
	{
		mc_hx.init(mc_field_htfProps, mc_store_htfProps, duty, ms_params.m_dt_hot, ms_params.m_T_field_out_des, ms_params.m_T_field_in_des);
	}

	// Do we need to define minimum and maximum thermal powers to/from storage?
	// The 'duty' definition should allow the tanks to accept whatever the field and/or power cycle can provide...

	// Calculate initial storage values
	int n_nodes = ms_params.m_ctes_type;			//local variable for number of nodes
	double V_node_ini = m_V_tank_active / n_nodes;			//[m^3] Each node has equal volume
	

	double T_hot_ini = ms_params.m_T_tank_hot_ini;		//[K]
	double T_cold_ini = ms_params.m_T_tank_cold_ini;	//[K]
	double dT_node_ini = (T_hot_ini - T_cold_ini);	//[K] spacing in temperature to initialize
													// Initialize nodes. For these tanks disregard active versus inactive volume. Use active volume.
	double h_node = ms_params.m_h_tank / n_nodes;	//Height of each section of tank equal divided equally

	//Cold node (bottom)
	mc_node_n.init(mc_store_htfProps, V_node_ini, h_node, false,
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
		V_node_ini, T_cold_ini);
	switch (n_nodes)
	{
	case 6:
		mc_node_five.init(mc_store_htfProps, V_node_ini, h_node, false,
			ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
			V_node_ini, T_cold_ini+(n_nodes-5.0)/(n_nodes-1.0)*dT_node_ini); //Assume equal spacing between initial temperatures
	case 5:
		mc_node_four.init(mc_store_htfProps, V_node_ini, h_node, false,
			ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
			V_node_ini, T_cold_ini + (n_nodes - 4.0) / (n_nodes - 1.0)*dT_node_ini);
	case 4:
		mc_node_three.init(mc_store_htfProps, V_node_ini, h_node, false,
			ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
			V_node_ini, T_cold_ini + (n_nodes - 3.0) / (n_nodes - 1.0)*dT_node_ini);
	case 3:
		mc_node_two.init(mc_store_htfProps, V_node_ini, h_node, false,
			ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
			V_node_ini, T_cold_ini + (n_nodes - 2.0) / (n_nodes - 1.0)*dT_node_ini);
	
	}
	// Hot node (top)
	mc_node_one.init(mc_store_htfProps, V_node_ini, h_node, true,
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_hot_tank_Thtr, ms_params.m_hot_tank_max_heat,
		V_node_ini, T_hot_ini);
}

bool C_csp_stratified_tes::does_tes_exist()
{
	return m_is_tes;
}

double C_csp_stratified_tes::get_hot_temp()
{
	return mc_node_one.get_m_T_prev();	//[K]
}

double C_csp_stratified_tes::get_cold_temp()
{
	return mc_node_n.get_m_T_prev();	//[K]
}


double C_csp_stratified_tes::get_hot_mass()
{
	return mc_node_one.get_m_m_calc();	// [kg]
}

double C_csp_stratified_tes::get_cold_mass()
{
	return mc_node_n.get_m_m_calc();	//[kg]
}

double C_csp_stratified_tes::get_hot_mass_prev()
{
	return mc_node_one.calc_mass_at_prev();	// [kg]
}

double C_csp_stratified_tes::get_cold_mass_prev()
{
	return mc_node_n.calc_mass_at_prev();	//[kg]
}

double C_csp_stratified_tes::get_physical_volume()

{
	return m_vol_tank;				//[m^3]
}

double C_csp_stratified_tes::get_hot_massflow_avail(double step_s) //[kg/sec]
{
	return mc_node_one.m_dot_available(0, step_s);
}

double C_csp_stratified_tes::get_cold_massflow_avail(double step_s) //[kg/sec]
{
	return mc_node_n.m_dot_available(0, step_s);
}


double C_csp_stratified_tes::get_initial_charge_energy()
{
	//MWh
	return m_q_pb_design * ms_params.m_ts_hours * m_V_tank_hot_ini / m_vol_tank * 1.e-6;
}

double C_csp_stratified_tes::get_min_charge_energy()
{
	//MWh
	return 0.; //ms_params.m_q_pb_design * ms_params.m_ts_hours * ms_params.m_h_tank_min / ms_params.m_h_tank*1.e-6;
}

double C_csp_stratified_tes::get_max_charge_energy()
{
	//MWh
	//double cp = mc_store_htfProps.Cp(ms_params.m_T_field_out_des);		//[kJ/kg-K] spec heat at average temperature during discharge from hot to cold
	//   double rho = mc_store_htfProps.dens(ms_params.m_T_field_out_des, 1.);

	//   double fadj = (1. - ms_params.m_h_tank_min / ms_params.m_h_tank);

	//   double vol_avail = m_vol_tank * ms_params.m_tank_pairs * fadj;

	//   double e_max = vol_avail * rho * cp * (ms_params.m_T_field_out_des - ms_params.m_T_field_in_des) / 3.6e6;   //MW-hr

	//   return e_max;
	return m_q_pb_design * ms_params.m_ts_hours / 1.e6;
}

double C_csp_stratified_tes::get_degradation_rate()
{
	//calculates an approximate "average" tank heat loss rate based on some assumptions. Good for simple optimization performance projections.
	double d_tank = sqrt(m_vol_tank / ((double)ms_params.m_tank_pairs * ms_params.m_h_tank * 3.14159));
	double e_loss = ms_params.m_u_tank * 3.14159 * ms_params.m_tank_pairs * d_tank * (ms_params.m_T_field_in_des + ms_params.m_T_field_out_des - 576.3)*1.e-6;  //MJ/s  -- assumes full area for loss, Tamb = 15C
	return e_loss / (m_q_pb_design * ms_params.m_ts_hours * 3600.); //s^-1  -- fraction of heat loss per second based on full charge
}

void C_csp_stratified_tes::discharge_avail_est(double T_cold_K, double step_s, double &q_dot_dc_est, double &m_dot_field_est, double &T_hot_field_est)
{
	double f_storage = 0.0;		// for now, hardcode such that storage always completely discharges

	double m_dot_tank_disch_avail = mc_node_one.m_dot_available(f_storage, step_s);	//[kg/s]

	double T_hot_ini = mc_node_one.get_m_T_prev();		//[K]

	if (ms_params.m_is_hx)
	{
		double eff, T_cold_tes;
		eff = T_cold_tes = std::numeric_limits<double>::quiet_NaN();
		mc_hx.hx_discharge_mdot_tes(T_hot_ini, m_dot_tank_disch_avail, T_cold_K, eff, T_cold_tes, T_hot_field_est, q_dot_dc_est, m_dot_field_est);

		// If above method fails, it will throw an exception, so if we don't want to break here, need to catch and handle it
	}
	else
	{
		double cp_T_avg = mc_store_htfProps.Cp(0.5*(T_cold_K + T_hot_ini));		//[kJ/kg-K] spec heat at average temperature during discharge from hot to cold

		q_dot_dc_est = m_dot_tank_disch_avail * cp_T_avg * (T_hot_ini - T_cold_K)*1.E-3;	//[MW]

		m_dot_field_est = m_dot_tank_disch_avail;

		T_hot_field_est = T_hot_ini;
	}

	m_m_dot_tes_dc_max = m_dot_tank_disch_avail * step_s;		//[kg/s]
}

void C_csp_stratified_tes::charge_avail_est(double T_hot_K, double step_s, double &q_dot_ch_est, double &m_dot_field_est, double &T_cold_field_est)
{
	double f_ch_storage = 0.0;	// for now, hardcode such that storage always completely charges

	double m_dot_tank_charge_avail = mc_node_three.m_dot_available(f_ch_storage, step_s);	//[kg/s]

	double T_cold_ini = mc_node_three.get_m_T_prev();	//[K]

	if (ms_params.m_is_hx)
	{
		double eff, T_hot_tes;
		eff = T_hot_tes = std::numeric_limits<double>::quiet_NaN();
		mc_hx.hx_charge_mdot_tes(T_cold_ini, m_dot_tank_charge_avail, T_hot_K, eff, T_hot_tes, T_cold_field_est, q_dot_ch_est, m_dot_field_est);

		// If above method fails, it will throw an exception, so if we don't want to break here, need to catch and handle it
	}
	else
	{
		double cp_T_avg = mc_store_htfProps.Cp(0.5*(T_cold_ini + T_hot_K));	//[kJ/kg-K] spec heat at average temperature during charging from cold to hot

		q_dot_ch_est = m_dot_tank_charge_avail * cp_T_avg * (T_hot_K - T_cold_ini) *1.E-3;	//[MW]

		m_dot_field_est = m_dot_tank_charge_avail;

		T_cold_field_est = T_cold_ini;
	}

	m_m_dot_tes_ch_max = m_dot_tank_charge_avail * step_s;		//[kg/s]
}

void C_csp_stratified_tes::discharge_full(double timestep /*s*/, double T_amb /*K*/, 
	double T_htf_cold_in /*K*/, double & T_htf_hot_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_strat_tes_outputs &outputs)
{
	// This method calculates the hot discharge temperature on the HX side (if applicable) during FULL DISCHARGE. If no heat exchanger (direct storage),
	//    the discharge temperature is equal to the average (timestep) hot tank outlet temperature

	// Inputs are:
	// 2) inlet temperature on the HX side (if applicable). If no heat exchanger, the inlet temperature is the temperature
	//	   of HTF directly entering the cold tank. 

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_cold_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		m_dot_htf_out = m_m_dot_tes_dc_max / timestep;		//[kg/s]

															// Call energy balance on hot tank discharge to get average outlet temperature over timestep
		mc_node_one.energy_balance(timestep, 0.0, m_dot_htf_out, 0.0, T_amb, T_htf_hot_out, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to track tank mass and temperature
		mc_node_three.energy_balance(timestep, m_dot_htf_out, 0.0, T_htf_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;
    outputs.m_m_dot = m_dot_htf_out;
	outputs.m_W_dot_rhtf_pump = m_dot_htf_out * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;

	outputs.m_T_hot_ave = T_htf_hot_out;
	outputs.m_T_cold_ave = T_cold_ave;
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_cold_in + T_htf_hot_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_dc_to_htf = m_dot_htf_out * cp_htf_ave*(T_htf_hot_out - T_htf_cold_in) / 1000.0;		//[MWt]
	outputs.m_q_dot_ch_from_htf = 0.0;							//[MWt]

}

bool C_csp_stratified_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_cold_in /*K*/, double & T_htf_hot_out /*K*/, S_csp_strat_tes_outputs &outputs)
{
	// This method calculates the hot discharge temperature on the HX side (if applicable). If no heat exchanger (direct storage),
	// the discharge temperature is equal to the average (timestep) hot tank outlet temperature.

	// Inputs are:
	// 1) Required hot side mass flow rate on the HX side (if applicable). If no heat exchanger, then the mass flow rate
	//     is equal to the hot tank exit mass flow rate (and cold tank fill mass flow rate)
	// 2) inlet temperature on the HX side (if applicable). If no heat exchanger, the inlet temperature is the temperature
	//	   of HTF directly entering the cold tank. 

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_cold_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_htf_in > m_m_dot_tes_dc_max / timestep)
		{
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on hot tank discharge to get average outlet temperature over timestep
		mc_node_one.energy_balance(timestep, 0.0, m_dot_htf_in, 0.0, T_amb, T_htf_hot_out, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to track tank mass and temperature
		mc_node_three.energy_balance(timestep, m_dot_htf_in, 0.0, T_htf_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;			//[MWt]
    outputs.m_m_dot = m_dot_htf_in;
	outputs.m_W_dot_rhtf_pump = m_dot_htf_in * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]

	outputs.m_T_hot_ave = T_htf_hot_out;						//[K]
	outputs.m_T_cold_ave = T_cold_ave;							//[K]
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_cold_in + T_htf_hot_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_dc_to_htf = m_dot_htf_in * cp_htf_ave*(T_htf_hot_out - T_htf_cold_in) / 1000.0;		//[MWt]
	outputs.m_q_dot_ch_from_htf = 0.0;		//[MWt]

	return true;
}

bool C_csp_stratified_tes::charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_hot_in /*K*/, double & T_htf_cold_out /*K*/, S_csp_strat_tes_outputs &outputs)
{
	// This method calculates the cold charge return temperature on the HX side (if applicable). If no heat exchanger (direct storage),
	// the return charge temperature is equal to the average (timestep) cold tank outlet temperature.

	// The method returns FALSE if the input mass flow rate 'm_dot_htf_in' * timestep is greater than the allowable charge 

	// Inputs are:
	// 1) Required cold side mass flow rate on the HX side (if applicable). If no heat exchanger, then the mass flow rate
	//     is equal to the cold tank exit mass flow rate (and hot tank fill mass flow rate)
	// 2) Inlet temperature on the HX side (if applicable). If no heat exchanger, the inlet temperature is the temperature
	//     of HTF directly entering the hot tank

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_hot_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_htf_in > m_m_dot_tes_ch_max / timestep)
		{
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_node_three.energy_balance(timestep, 0.0, m_dot_htf_in, 0.0, T_amb, T_htf_cold_out, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature
		mc_node_one.energy_balance(timestep, m_dot_htf_in, 0.0, T_htf_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;			//[MW] Storage thermal losses
    outputs.m_m_dot = m_dot_htf_in;
	outputs.m_W_dot_rhtf_pump = m_dot_htf_in * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MW] Heating power required to keep tanks at a minimum temperature


	outputs.m_T_hot_ave = T_hot_ave;							//[K] Average hot tank temperature over timestep
	outputs.m_T_cold_ave = T_htf_cold_out;						//[K] Average cold tank temperature over timestep
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_hot_in + T_htf_cold_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_htf_in * cp_htf_ave*(T_htf_hot_in - T_htf_cold_out) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}





bool C_csp_stratified_tes::charge_discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_hot_in /*kg/s*/, 
	double T_hot_in /*K*/, double m_dot_cold_in /*kg/s*/, double T_cold_in /*K*/, S_csp_strat_tes_outputs &outputs)
{
	// ARD This is for simultaneous charge and discharge. If no heat exchanger (direct storage),
	// the return charge temperature is equal to the average (timestep) cold tank outlet temperature.

	// The method returns FALSE if the input mass flow rate 'm_dot_htf_in' * timestep is greater than the allowable charge 

	// Inputs are:
	// 1) (Assumes no heat exchanger) The cold tank exit mass flow rate (and hot tank fill mass flow rate)
	// 2) The temperature of HTF directly entering the hot tank.
	// 3) The hot tank exit mass flow rate (and cold tank fill mass flow rate)
	// 4) The temperature of the HTF directly entering the cold tank.

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_hot_ave, T_cold_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave = T_cold_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_hot_in > m_m_dot_tes_ch_max / timestep)
		{
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_node_three.energy_balance(timestep, m_dot_cold_in, m_dot_hot_in, T_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature
		mc_node_one.energy_balance(timestep, m_dot_hot_in, m_dot_cold_in, T_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;			//[MW] Storage thermal losses
    outputs.m_m_dot = m_dot_hot_in;
	outputs.m_W_dot_rhtf_pump = m_dot_hot_in * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MW] Heating power required to keep tanks at a minimum temperature


	outputs.m_T_hot_ave = T_hot_ave;							//[K] Average hot tank temperature over timestep
	outputs.m_T_cold_ave = T_cold_ave;						//[K] Average cold tank temperature over timestep
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_hot_in + T_cold_ave);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_hot_in * cp_htf_ave*(T_hot_in - T_cold_ave) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}

bool C_csp_stratified_tes::recirculation(double timestep /*s*/, double T_amb /*K*/, double m_dot_cold_in /*kg/s*/, 
	double T_cold_in /*K*/, S_csp_strat_tes_outputs &outputs)
{
	// This method calculates the average (timestep) cold tank outlet temperature when recirculating cold fluid for further cooling.
	// This warm tank is idle and its state is also determined.

	// The method returns FALSE if the input mass flow rate 'm_dot_htf_in' * timestep is greater than the allowable charge 

	// Inputs are:
	// 1) The cold tank exit mass flow rate
	// 2) The inlet temperature of HTF directly entering the cold tank

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_hot_ave, T_cold_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave = T_cold_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_cold_in > m_m_dot_tes_ch_max / timestep)	//Is this necessary for recirculation mode? ARD
		{
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_node_three.energy_balance(timestep, m_dot_cold_in, m_dot_cold_in, T_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature while idle
		mc_node_one.energy_balance(timestep, 0.0, 0.0, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;			//[MW] Storage thermal losses
    outputs.m_m_dot = m_dot_cold_in;
	outputs.m_W_dot_rhtf_pump = m_dot_cold_in * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MW] Heating power required to keep tanks at a minimum temperature


	outputs.m_T_hot_ave = T_hot_ave;							//[K] Average hot tank temperature over timestep
	outputs.m_T_cold_ave = T_cold_ave;							//[K] Average cold tank temperature over timestep
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_cold_in + T_cold_ave);			//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_cold_in * cp_htf_ave*(T_cold_in - T_cold_ave) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}

bool C_csp_stratified_tes::stratified_tanks(double timestep /*s*/, double T_amb /*K*/, double m_dot_cond /*kg/s*/, 
	double T_cond_out /*K*/, double m_dot_rad /*kg/s*/, double T_rad_out /*K*/, S_csp_strat_tes_outputs &outputs)
{
	// ARD This is completing the energy balance on a stratified tank. Uses nodal model in Duffie & Beckman. 3-6 nodes accomodated by this code.

	// Inputs are:
	// 1) The mass flow rate through condenser
	// 2) The temperature of HTF directly entering from the condenser to top node
	// 3) The mass flow rate through radiator field/HX 
	// 4) The temperature of the HTF directly entering from the radiator to bottom node

// Determine mass flow rates for each node and mass-averaged inlet temperature for each node
	int n_nodes = ms_params.m_ctes_type;	//Number of nodes specified in input (3-5)
	int n_last = n_nodes - 1;				//Zero based index of last node
	double T_node_prev[6] = {};			//Temperatures of each node at previous converged timestep. Initialize to zero.
	T_node_prev[n_last] = mc_node_n.get_m_T_prev();//Bottom node
	switch (n_nodes)							//Use switch to determine how many other nodes to access
	{
	case 6:
		T_node_prev[4] = mc_node_five.get_m_T_prev();
	case 5:
		T_node_prev[3] = mc_node_four.get_m_T_prev();
	case 4:
		T_node_prev[2] = mc_node_three.get_m_T_prev();
	case 3:
		T_node_prev[1] = mc_node_two.get_m_T_prev();
	}
	T_node_prev[0] = mc_node_one.get_m_T_prev();	//Top node

	int F_C_node[6] = {};		//Condenser control function determining which node condenser return water goes to
	int F_C_down[6] = {};
	int F_R_node[6] = {};		//Radiator control function determining which node radiator return water goes to
	int F_R_up[6] = {};
	double m_dot_in_node[6] = {};//Mass flow rate into & out of each node
	double T_in_node[6] = {};	//Mass averaged inlet water temperature
	double T_node_ave[6] = {};
	double q_heater[6] = {};
	double q_dot_loss[6] = {};

	//Set control function for condenser return flow
	if (T_cond_out > T_node_prev[0])
	{
		F_C_node[0] = 1;
	}

	for (int n = 1; n != n_last; ++n)
	{
		if ( (T_node_prev[n-1] >= T_cond_out) && (T_cond_out > T_node_prev[n]) )
		{
			F_C_node[n] = 1;
		}
	}
	if (T_node_prev[n_last-1] >= T_cond_out)
	{
		F_C_node[n_last] = 1; 
	}

	//Set control function for radiator return flow
	if (T_rad_out > T_node_prev[0])
	{
		F_R_node[0] = 1; 
	}
	for (int n = 1; n != n_last; ++n)
	{
		if ((T_node_prev[n-1] >= T_rad_out) && (T_rad_out > T_node_prev[n]))
		{
			F_R_node[n] = 1;
		}
	}
	if (T_node_prev[n_last-1] >= T_rad_out)
	{
		F_R_node[n_last] = 1; 
	}

	//Set mass flow rates for each node

	for (int j = 1; j != n_last + 1; ++j)	//Loop through all nodes below top node
	{
		F_R_up[0] = F_R_up[0] + F_R_node[j];
	}
	m_dot_in_node[0] = F_C_node[0] * m_dot_cond + F_C_down[0] * m_dot_cond + F_R_node[0] * m_dot_rad + F_R_up[0] * m_dot_rad; // Top node mass flow rate in
	T_in_node[0] = (F_C_node[0] * m_dot_cond*T_cond_out  + F_R_node[0] * m_dot_rad*T_rad_out + F_R_up[0] * m_dot_rad*T_node_prev[1]) / (0.001 + m_dot_in_node[0]); //Top node mass-averaged temperature in

	for (int n = 1; n != n_last; ++n) //Loop through all nodes except top and bottom
	{
		for (int i = 0; i != n; ++i)			//Loop through all nodes above
		{
			F_C_down[n] = F_C_down[n] + F_C_node[i];
		}
		for (int j = (n+1); j != n_last+1; ++j)	//Loop through all nodes below
		{
			F_R_up[n] = F_R_up[n] + F_R_node[j];
		}
		m_dot_in_node[n] = F_C_node[n] * m_dot_cond + F_C_down[n] * m_dot_cond + F_R_node[n] * m_dot_rad + F_R_up[n] * m_dot_rad;
		T_in_node[n] = (F_C_node[n] * m_dot_cond*T_cond_out + F_C_down[n] * m_dot_cond*T_node_prev[n - 1] + F_R_node[n] * m_dot_rad*T_rad_out + F_R_up[n] * m_dot_rad*T_node_prev[n + 1]) / (0.001 + m_dot_in_node[n]);
		
	}

	for (int i = 0; i != n_last; ++i)			//Loop through all nodes above bottom node
	{
		F_C_down[n_last] = F_C_down[n_last] + F_C_node[i];
	}
	m_dot_in_node[n_last] = F_C_node[n_last] * m_dot_cond + F_C_down[n_last] * m_dot_cond + F_R_node[n_last] * m_dot_rad; //Bottom node
	T_in_node[n_last] = (F_C_node[n_last] * m_dot_cond*T_cond_out + F_C_down[n_last] * m_dot_cond*T_node_prev[n_last - 1] + F_R_node[n_last] * m_dot_rad*T_rad_out) / (0.001 + m_dot_in_node[n_last]);
	

	// Call energy balance on top node
	mc_node_n.energy_balance_constant_mass(timestep, m_dot_in_node[n_last], T_in_node[n_last], T_amb, T_node_ave[n_last], q_heater[n_last], q_dot_loss[n_last]);
	switch (n_nodes)
	{
	case 6:
		mc_node_five.energy_balance_constant_mass(timestep, m_dot_in_node[4], T_in_node[4], T_amb, T_node_ave[4], q_heater[4], q_dot_loss[4]);
	case 5:
		mc_node_four.energy_balance_constant_mass(timestep, m_dot_in_node[3], T_in_node[3], T_amb, T_node_ave[3], q_heater[3], q_dot_loss[3]);
	case 4:
		mc_node_three.energy_balance_constant_mass(timestep, m_dot_in_node[2], T_in_node[2], T_amb, T_node_ave[2], q_heater[2], q_dot_loss[2]);
	case 3:
		mc_node_two.energy_balance_constant_mass(timestep, m_dot_in_node[1], T_in_node[1], T_amb, T_node_ave[1], q_heater[1], q_dot_loss[1]);
	}
	mc_node_one.energy_balance_constant_mass(timestep, m_dot_in_node[0], T_in_node[0], T_amb, T_node_ave[0], q_heater[0], q_dot_loss[0]);

		
		

	outputs.m_q_heater = q_heater[0] +q_heater[1] + q_heater[2] + q_heater[4]+q_heater[5];			//[MW] Storage thermal losses
	//outputs.m_W_dot_rhtf_pump = m_dot_cond * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss[0] + q_dot_loss[1]+q_dot_loss[2]+q_dot_loss[3]+q_dot_loss[4]+q_dot_loss[5];	//[MW] Heating power required to keep tanks at a minimum temperature


	outputs.m_T_hot_ave = T_node_ave[0];						//[K] Average hot tank temperature over timestep
	outputs.m_T_cold_ave = T_node_ave[n_last];					//[K] Average cold tank temperature over timestep
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_node_n.get_m_T_calc();			//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF - CHECK THESE FOR COLD STORAGE?
	//double T_htf_ave = 0.5*(T_cond_out + T_node_ave[2]);		//[K]
	//double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	//outputs.m_q_dot_ch_from_htf = m_dot_cond * cp_htf_ave*(T_cond_out - T_node_ave[2]) / 1000.0;		//[MWt]
	//outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}


void C_csp_stratified_tes::charge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_hot_in /*K*/, 
	double & T_htf_cold_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_strat_tes_outputs &outputs)
{
	// This method calculates the cold charge return temperature and mass flow rate on the HX side (if applicable) during FULL CHARGE. If no heat exchanger (direct storage),
	//    the charge return temperature is equal to the average (timestep) cold tank outlet temperature

	// Inputs are:
	// 1) inlet temperature on the HX side (if applicable). If no heat exchanger, the inlet temperature is the temperature
	//	   of HTF directly entering the hot tank. 

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_hot_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		m_dot_htf_out = m_m_dot_tes_ch_max / timestep;		//[kg/s]

															// Call energy balance on hot tank charge to track tank mass and temperature
		mc_node_one.energy_balance(timestep, m_dot_htf_out, 0.0, T_htf_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to calculate cold HTF return temperature
		mc_node_three.energy_balance(timestep, 0.0, m_dot_htf_out, 0.0, T_amb, T_htf_cold_out, q_heater_cold, q_dot_loss_cold);
	}

	else
	{	// Iterate between field htf - hx - and storage	

	}

	outputs.m_q_heater = q_heater_cold + q_heater_hot;
    outputs.m_m_dot = m_dot_htf_out;
	outputs.m_W_dot_rhtf_pump = m_dot_htf_out * ms_params.m_htf_pump_coef / 1.E3;	//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;

	outputs.m_T_hot_ave = T_hot_ave;
	outputs.m_T_cold_ave = T_htf_cold_out;
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_node_three.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_hot_in + T_htf_cold_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_htf_out * cp_htf_ave*(T_htf_hot_in - T_htf_cold_out) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

}




void C_csp_stratified_tes::idle(double timestep, double T_amb, S_csp_strat_tes_outputs &outputs)
{
	int n_nodes = ms_params.m_ctes_type;
	int n_last = n_nodes - 1;
	double T_node_ave[6] = {};
	double q_heater[6] = {};
	double q_dot_loss[6] = {};

	// Call energy balance on top node
	mc_node_n.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[n_last], q_heater[n_last], q_dot_loss[n_last]);
	switch (n_nodes)
	{
	case 6:
		mc_node_five.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[4], q_heater[4], q_dot_loss[4]);
	case 5:
		mc_node_four.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[3], q_heater[3], q_dot_loss[3]);
	case 4:
		mc_node_three.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[2], q_heater[2], q_dot_loss[2]);
	case 3:
		mc_node_two.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[1], q_heater[1], q_dot_loss[1]);
	}
	mc_node_one.energy_balance_constant_mass(timestep, 0, 0, T_amb, T_node_ave[0], q_heater[0], q_dot_loss[0]);

	
	
	outputs.m_q_heater = q_heater[0] + q_heater[1] + q_heater[2] + q_heater[4] + q_heater[5];			//[MW] Storage thermal losses
    outputs.m_m_dot = 0.;
	//outputs.m_W_dot_rhtf_pump = 0;																		//[MWe] Pumping power for Receiver HTF, convert from kW/kg/s*kg/s
	outputs.m_q_dot_loss = q_dot_loss[0] + q_dot_loss[1] + q_dot_loss[2] + q_dot_loss[3] + q_dot_loss[4] + q_dot_loss[5];	//[MW] Heating power required to keep tanks at a minimum temperature

	outputs.m_T_hot_ave = T_node_ave[0];						//[K]
	outputs.m_T_cold_ave = T_node_ave[n_last];					//[K]
	outputs.m_T_hot_final = mc_node_one.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_node_n.get_m_T_calc();			//[K]

	outputs.m_q_dot_ch_from_htf = 0.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;		//[MWt]
}

void C_csp_stratified_tes::converged()
{
	mc_node_n.converged();
	switch (ms_params.m_ctes_type)
	{
	case 6:
		mc_node_five.converged();
	case 5:
		mc_node_four.converged();
	case 4:
		mc_node_three.converged();
	case 3:
		mc_node_two.converged();
	}
	mc_node_one.converged();


	// The max charge and discharge flow rates should be set at the beginning of each timestep
	//   during the q_dot_xx_avail_est calls
	m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();
}

int C_csp_stratified_tes::pressure_drops(double m_dot_sf, double m_dot_pb,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
    double &P_drop_col, double &P_drop_gen)
{
    P_drop_col = 0.;
    P_drop_gen = 0.;

    return 0;
}

double C_csp_stratified_tes::pumping_power(double m_dot_sf, double m_dot_pb, double m_dot_tank,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating)
{
    return m_dot_tank * this->ms_params.m_htf_pump_coef / 1.E3;
}
