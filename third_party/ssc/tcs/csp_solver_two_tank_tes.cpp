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

#include "csp_solver_two_tank_tes.h"
#include "csp_solver_util.h"

C_hx_two_tank_tes::C_hx_two_tank_tes()
{
	m_m_dot_des_ave = m_eff_des = m_UA_des = std::numeric_limits<double>::quiet_NaN();
}

void C_hx_two_tank_tes::init(const HTFProperties &fluid_field, const HTFProperties &fluid_store, double q_transfer_des /*W*/,
	double dt_des, double T_h_in_des /*K*/, double T_h_out_des /*K*/)
{
	// Counter flow heat exchanger

    mc_field_htfProps = fluid_field;
    mc_store_htfProps = fluid_store;

		// Design should provide field/pc side design temperatures
	double T_ave = (T_h_in_des + T_h_out_des) / 2.0;		//[K] Average hot side temperature
	double c_h = mc_field_htfProps.Cp(T_ave)*1000.0;		//[J/kg-K] Spec heat of hot side fluid at hot side average temperature, convert from [kJ/kg-K]
	double c_c = mc_store_htfProps.Cp(T_ave)*1000.0;		//[J/kg-K] Spec heat of cold side fluid at hot side average temperature (estimate, but should be close)
		// HX inlet and outlet temperatures
	double T_c_out = T_h_in_des - dt_des;
	double T_c_in = T_h_out_des - dt_des;
		// Mass flow rates
	double m_dot_h = q_transfer_des/(c_h*(T_h_in_des - T_h_out_des));	//[kg/s]
	double m_dot_c = q_transfer_des/(c_c*(T_c_out - T_c_in));			//[kg/s]
	m_m_dot_des_ave = 0.5*(m_dot_h + m_dot_c);		//[kg/s]
		// Capacitance rates
	double c_dot_h = m_dot_h * c_h;				//[W/K]
	double c_dot_c = m_dot_c * c_c;				//[W/K]
	double c_dot_max = fmax(c_dot_h, c_dot_c);	//[W/K]
	double c_dot_min = fmin(c_dot_h, c_dot_c);	//[W/K]
	double cr = c_dot_min / c_dot_max;			//[-]
		// Maximum possible energy flow rate
	double q_max = c_dot_min * (T_h_in_des - T_c_in);	//[W]
		// Effectiveness
	m_eff_des = q_transfer_des / q_max;

	// Check for realistic conditions
	if(cr > 1.0 || cr < 0.0)
	{
		throw(C_csp_exception("Heat exchanger design calculations failed", ""));
	}

	double NTU = std::numeric_limits<double>::quiet_NaN();
	if(cr < 1.0)
		NTU = log((1. - m_eff_des*cr) / (1. - m_eff_des)) / (1. - cr);
	else
		NTU = m_eff_des / (1. - m_eff_des);

	m_UA_des = NTU * c_dot_min;		//[W/K]
}

void C_hx_two_tank_tes::solve(double T_f_htf_hx_in /*K*/, double m_dot_f_htf /*kg/s*/,
		double T_s_htf_hx_in /*K*/, double m_dot_s_htf /*kg/s*/,
		double & T_f_htf_hx_out /*K*/, double & T_s_htf_hx_out /*K*/,
		double & eff /*-*/, double & q_dot_hx /*MWt*/)
{
	if (m_dot_f_htf == 0.0 || m_dot_s_htf == 0.0)
	{
		T_f_htf_hx_out = T_f_htf_hx_in;
		T_s_htf_hx_out = T_s_htf_hx_in;

		eff = 0.0;
		q_dot_hx = 0.0;

		return;
	}

	// Scale UA
	double m_dot_od = 0.5 * (m_dot_f_htf + m_dot_s_htf);		//[kg/s]
	double UA = m_UA_des * pow(m_dot_od / m_m_dot_des_ave, 0.8);

	double T_ave = (T_f_htf_hx_in + T_s_htf_hx_in) / 2.0;		//[K]

	double cp_f = mc_field_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
	double c_dot_f = cp_f * m_dot_f_htf;
	double cp_s = mc_store_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
	double c_dot_s = cp_s * m_dot_s_htf;

	double c_dot_min = std::min(c_dot_f, c_dot_s);

	double CR = std::min(c_dot_f, c_dot_s) / std::max(c_dot_s, c_dot_f);

	double NTU = UA / c_dot_min;

	// calculate effectiveness
	if (CR > 0.999)
	{
		eff = NTU / (1.0 + NTU);
	}
	else
	{
		eff = (1.0 - std::exp(-NTU*(1.0-CR)))/(1.0 - CR*std::exp(-NTU*(1.0-CR)));	//[-]
	}

	if (std::isnan(eff) || eff <= 0.0 || eff > 1.0)
	{
		T_f_htf_hx_out = T_s_htf_hx_out = 
			eff = q_dot_hx = std::numeric_limits<double>::quiet_NaN();
		throw(C_csp_exception("Off design heat exchanger failed", ""));
	}

	double T_hot_in = std::max(T_f_htf_hx_in, T_s_htf_hx_in);	//[K]
	double T_cold_in = std::min(T_f_htf_hx_in, T_s_htf_hx_in);	//[K]

	// Calculate heat transfer in HX
	double q_dot_max = c_dot_min * (T_hot_in - T_cold_in);
	q_dot_hx = eff * q_dot_max;

	if (T_s_htf_hx_in > T_f_htf_hx_in)
	{
		T_s_htf_hx_out = T_s_htf_hx_in - q_dot_hx / c_dot_s;
		T_f_htf_hx_out = T_f_htf_hx_in + q_dot_hx / c_dot_f;
	}
	else
	{
		T_s_htf_hx_out = T_s_htf_hx_in + q_dot_hx / c_dot_s;
		T_f_htf_hx_out = T_f_htf_hx_in - q_dot_hx / c_dot_f;
	}

	q_dot_hx *= 1.E-6;		//[MWt] convert from W
}

C_hx_cold_tes::C_hx_cold_tes()
{
	m_m_dot_des_ave = m_eff_des = m_UA_des = std::numeric_limits<double>::quiet_NaN();
}

void C_hx_cold_tes::init(const HTFProperties& fluid_field, const HTFProperties& fluid_store, double q_transfer_des /*W*/,
	double dt_des, double T_h_in_des /*K*/, double T_h_out_des /*K*/)
{
	// Counter flow heat exchanger

	mc_field_htfProps = fluid_field;
	mc_store_htfProps = fluid_store;

	// Design should provide field/pc side design temperatures
	double T_ave = (T_h_in_des + T_h_out_des) / 2.0;		//[K] Average hot side temperature
	double c_h = mc_field_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K] Spec heat of hot side fluid at hot side average temperature, convert from [kJ/kg-K]
	double c_c = mc_store_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K] Spec heat of cold side fluid at hot side average temperature (estimate, but should be close)
		// HX inlet and outlet temperatures
	double T_c_out = T_h_in_des - dt_des;
	double T_c_in = T_h_out_des - dt_des;
	// Mass flow rates
	double m_dot_h = q_transfer_des / (c_h * (T_h_in_des - T_h_out_des));	//[kg/s]
	double m_dot_c = q_transfer_des / (c_c * (T_c_out - T_c_in));			//[kg/s]
	m_m_dot_des_ave = 0.5 * (m_dot_h + m_dot_c);		//[kg/s]
		// Capacitance rates
	double c_dot_h = m_dot_h * c_h;				//[W/K]
	double c_dot_c = m_dot_c * c_c;				//[W/K]
	double c_dot_max = fmax(c_dot_h, c_dot_c);	//[W/K]
	double c_dot_min = fmin(c_dot_h, c_dot_c);	//[W/K]
	double cr = c_dot_min / c_dot_max;			//[-]
		// Maximum possible energy flow rate
	double q_max = c_dot_min * (T_h_in_des - T_c_in);	//[W]
		// Effectiveness
	m_eff_des = q_transfer_des / q_max;

	// Check for realistic conditions
	if (cr > 1.0 || cr < 0.0)
	{
		throw(C_csp_exception("Heat exchanger design calculations failed", ""));
	}

	double NTU = std::numeric_limits<double>::quiet_NaN();
	if (cr < 1.0)
		NTU = log((1. - m_eff_des * cr) / (1. - m_eff_des)) / (1. - cr);
	else
		NTU = m_eff_des / (1. - m_eff_des);

	m_UA_des = NTU * c_dot_min;		//[W/K]

	m_T_hot_field_prev = m_T_hot_field_calc = T_h_in_des;
	m_T_cold_field_prev = m_T_cold_field_calc = T_h_out_des;
	m_m_dot_field_prev = m_m_dot_field_calc = m_dot_h;
	m_T_hot_tes_prev = m_T_hot_tes_calc = T_c_out;
	m_T_cold_tes_prev = m_T_cold_tes_calc = T_c_in;
	m_m_dot_tes_prev = m_m_dot_tes_calc = m_dot_c;
}

void C_hx_cold_tes::hx_charge_mdot_tes(double T_cold_tes, double m_dot_tes, double T_hot_field,
	double& eff, double& T_hot_tes, double& T_cold_field, double& q_trans, double& m_dot_field)
{
	hx_performance(false, true, T_hot_field, m_dot_tes, T_cold_tes,
		eff, T_cold_field, T_hot_tes, q_trans, m_dot_field);
}

void C_hx_cold_tes::hx_discharge_mdot_tes(double T_hot_tes, double m_dot_tes, double T_cold_field,
	double& eff, double& T_cold_tes, double& T_hot_field, double& q_trans, double& m_dot_field)
{
	hx_performance(true, true, T_hot_tes, m_dot_tes, T_cold_field,
		eff, T_cold_tes, T_hot_field, q_trans, m_dot_field);
}

void C_hx_cold_tes::hx_charge_mdot_field(double T_hot_field, double m_dot_field, double T_cold_tes,
	double& eff, double& T_cold_field, double& T_hot_tes, double& q_trans, double& m_dot_tes)
{
	hx_performance(true, false, T_hot_field, m_dot_field, T_cold_tes,
		eff, T_cold_field, T_hot_tes, q_trans, m_dot_tes);
}

void C_hx_cold_tes::hx_discharge_mdot_field(double T_cold_field, double m_dot_field, double T_hot_tes,
	double& eff, double& T_hot_field, double& T_cold_tes, double& q_trans, double& m_dot_tes)
{
	hx_performance(false, false, T_hot_tes, m_dot_field, T_cold_field,
		eff, T_cold_tes, T_hot_field, q_trans, m_dot_tes);
}

void C_hx_cold_tes::hx_performance(bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in,
	double& eff, double& T_hot_out, double& T_cold_out, double& q_trans, double& m_dot_solved)
{
	// UA fixed

	// Subroutine for storage heat exchanger performance
	// Pass a flag to determine whether mass flow rate is cold side or hot side. Return mass flow rate will be the other
	// Also pass a flag to determine whether storage or field side mass flow rate is known. Return mass flow rate will be the other
	// Inputs: hot side mass flow rate [kg/s], hot side inlet temp [K], cold side inlet temp [K]
	// Outputs: HX effectiveness [-], hot side outlet temp [K], cold side outlet temp [K], 
	//				Heat transfer between fluids [MWt], cold side mass flow rate [kg/s]

	if (m_dot_known < 0) {
		eff = T_hot_out = T_cold_out = q_trans = m_dot_solved = std::numeric_limits<double>::quiet_NaN();
		throw(C_csp_exception("HX provided a negative mass flow", ""));
	}
	else if (m_dot_known == 0) {
		eff = 0.;
		T_hot_out = T_hot_in;
		T_cold_out = T_cold_in;
		q_trans = 0.;
		m_dot_solved = 0.;
		return;
	}

	double m_dot_hot, m_dot_cold, c_hot, c_cold, c_dot;
	bool m_field_is_hot;     // Is the field side the 'hot' side (e.g., during storage charging)?

	double T_ave = (T_hot_in + T_cold_in) / 2.0;		//[K]

	if (is_hot_side_mdot)		// know hot side mass flow rate - assuming always know storage side and solving for field
	{
		if (is_storage_side)
		{
			m_field_is_hot = false;
			c_cold = mc_field_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
			c_hot = mc_store_htfProps.Cp(T_ave) * 1000.0;			//[J/kg-K]
		}
		else
		{
			m_field_is_hot = true;
			c_hot = mc_field_htfProps.Cp(T_ave) * 1000.0;			//[J/kg-K]
			c_cold = mc_store_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
		}

		m_dot_hot = m_dot_known;
		// Calculate flow capacitance of hot stream
		double c_dot_hot = m_dot_hot * c_hot;			//[W/K]
		c_dot = c_dot_hot;
		// Choose a cold stream mass flow rate that results in c_dot_h = c_dot_c
		m_dot_cold = c_dot_hot / c_cold;
		m_dot_solved = m_dot_cold;
	}
	else
	{
		if (is_storage_side)
		{
			m_field_is_hot = true;
			c_hot = mc_field_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
			c_cold = mc_store_htfProps.Cp(T_ave) * 1000.0;	//[J/kg-K]
		}
		else
		{
			m_field_is_hot = false;
			c_cold = mc_field_htfProps.Cp(T_ave) * 1000.0;	//[J/kg-K]
			c_hot = mc_store_htfProps.Cp(T_ave) * 1000.0;		//[J/kg-K]
		}

		m_dot_cold = m_dot_known;
		// Calculate flow capacitance of cold stream
		double c_dot_cold = m_dot_cold * c_cold;
		c_dot = c_dot_cold;
		// Choose a cold stream mass flow rate that results in c_dot_c = c_dot_h
		m_dot_hot = c_dot_cold / c_hot;
		m_dot_solved = m_dot_hot;
	}

	// Scale UA
	double m_dot_od = 0.5 * (m_dot_cold + m_dot_hot);
	double UA = m_UA_des * pow(m_dot_od / m_m_dot_des_ave, 0.8);

	// Calculate effectiveness
	double NTU = UA / c_dot;
	eff = NTU / (1.0 + NTU);

	if (std::isnan(eff) || eff <= 0.0 || eff > 1.0)
	{
		eff = T_hot_out = T_cold_out = q_trans = m_dot_solved =
			m_T_hot_field_prev = m_T_cold_field_prev = m_T_hot_tes_prev = m_T_cold_tes_prev =
			m_m_dot_field_prev = m_m_dot_tes_prev = std::numeric_limits<double>::quiet_NaN();
		throw(C_csp_exception("Off design heat exchanger failed", ""));
	}

	// Calculate heat transfer in HX
	double q_dot_max = c_dot * (T_hot_in - T_cold_in);
	q_trans = eff * q_dot_max;

	T_hot_out = T_hot_in - q_trans / c_dot;
	T_cold_out = T_cold_in + q_trans / c_dot;

	q_trans *= 1.E-6;		//[MWt]

	// Set member variables
	if (m_field_is_hot == true) {
		m_T_hot_field_prev = T_hot_in;
		m_T_cold_field_prev = T_hot_out;
		m_T_hot_tes_prev = T_cold_out;
		m_T_cold_tes_prev = T_cold_in;
	}
	else {
		m_T_hot_field_prev = T_cold_out;
		m_T_cold_field_prev = T_cold_in;
		m_T_hot_tes_prev = T_hot_in;
		m_T_cold_tes_prev = T_hot_out;
	}

	if (is_storage_side) {
		m_m_dot_field_prev = m_dot_solved;
		m_m_dot_tes_prev = m_dot_known;
	}
	else {
		m_m_dot_field_prev = m_dot_known;
		m_m_dot_tes_prev = m_dot_solved;
	}
}

C_storage_tank::C_storage_tank()
{
	m_V_prev = m_T_prev = m_m_prev =
		
	m_V_total = m_V_active = m_V_inactive = m_UA = 
	
	m_T_htr = m_max_q_htr =
	m_T_design = m_mass_total = m_mass_inactive = m_mass_active = std::numeric_limits<double>::quiet_NaN();
}

void C_storage_tank::init(HTFProperties htf_class_in, double V_tank /*m3*/,
	double h_tank /*m*/, double h_min /*m*/, double u_tank /*W/m2-K*/,
	double tank_pairs /*-*/, double T_htr /*K*/, double max_q_htr /*MWt*/,
	double V_ini /*m3*/, double T_ini /*K*/,
	double T_design /*K*/)
{
	mc_htf = htf_class_in;

	double rho_des = mc_htf.dens(T_design, 1.0);		//[kg/m^3] Density at average temperature

	m_V_total = V_tank;				//[m^3]

	m_mass_total = m_V_total * rho_des;			//[kg]

	m_V_inactive = m_V_total*h_min / h_tank;	//[m^3]

	m_mass_inactive = m_V_inactive * rho_des;	//[kg]

	m_V_active = m_V_total - m_V_inactive;		//[m^3]

	m_mass_active = m_mass_total - m_mass_inactive;	//[kg]

	double A_cs = m_V_total / (h_tank*tank_pairs);		//[m^2] Cross-sectional area of a single tank

	double diameter = pow(A_cs / CSP::pi, 0.5)*2.0;		//[m] Diameter of a single tank

	// Calculate tank conductance
	m_UA = u_tank*(A_cs + CSP::pi*diameter*h_tank)*tank_pairs;	//[W/K]

	m_T_htr = T_htr;
	m_max_q_htr = max_q_htr;

	m_V_prev = V_ini;
	m_T_prev = T_ini;
	m_m_prev = calc_mass_at_prev();
}

double C_storage_tank::calc_mass_at_prev()
{
	return m_V_prev*mc_htf.dens(m_T_prev, 1.0);	//[kg] 
}

double C_storage_tank::calc_cp_at_prev()
{
    return mc_htf.Cp(m_T_prev)*1000.0;      // [J/kg-K]
}

double C_storage_tank::calc_enth_at_prev()
{
    return mc_htf.enth(m_T_prev);      // [J/kg-K]
}

double C_storage_tank::get_m_UA()
{
    return m_UA;            //[W/K]
}

double C_storage_tank::get_m_T_prev()
{
	return m_T_prev;		//[K]
}

double C_storage_tank::get_m_T_calc()
{
	return m_T_calc;
}

double C_storage_tank::get_m_m_calc() //ARD new getter for current mass 
{
	return m_m_calc;
}

double C_storage_tank::get_vol_frac()
{
	return (m_V_prev - m_V_inactive) / m_V_active;
}

double C_storage_tank::m_dot_available(double f_unavail, double timestep)
{
	//double rho = mc_htf.dens(m_T_prev, 1.0);		//[kg/m^3]
	//double V = m_m_prev / rho;						//[m^3] Volume available in tank (one temperature)
	//double V_avail = fmax(V - m_V_inactive, 0.0);	//[m^3] Volume that is active - need to maintain minimum height (corresponding m_V_inactive)

	double mass_avail = std::max(m_m_prev - m_mass_inactive, 0.0);		//[kg]
	double m_dot_avail = std::max(mass_avail - m_mass_active * f_unavail, 0.0) / timestep;	//[kg/s]

	// "Unavailable" fraction now applied to one temperature tank volume, not total tank volume
	//double m_dot_avail = fmax(V_avail - m_V_active*f_unavail, 0.0)*rho / timestep;		//[kg/s] Max mass flow rate available

	return m_dot_avail;		//[kg/s]
}

void C_storage_tank::converged()
{
	// Reset 'previous' timestep values to 'calculated' values
	m_V_prev = m_V_calc;		//[m^3]
	m_T_prev = m_T_calc;		//[K]
	m_m_prev = m_m_calc;		//[kg]
}

void C_storage_tank::energy_balance(double timestep /*s*/, double m_dot_in, double m_dot_out, double T_in /*K*/, double T_amb /*K*/, 
	double &T_ave /*K*/, double & q_heater /*MW*/, double & q_dot_loss /*MW*/)
{
	// Get properties from tank state at the end of last time step
	double rho = mc_htf.dens(m_T_prev, 1.0);	//[kg/m^3]
	double cp = mc_htf.Cp(m_T_prev)*1000.0;		//[J/kg-K] spec heat, convert from kJ/kg-K

	// Calculate ending volume levels
	m_m_calc = m_m_prev + timestep*(m_dot_in - m_dot_out);	//[kg] Available mass at the end of this timestep
    double m_min, m_dot_out_adj;
    bool tank_is_empty = false;
    m_min = 0.001;                              //[kg] minimum tank mass for use in the calculations
    if (m_m_calc < m_min) {
        m_m_calc = m_min;
        tank_is_empty = true;
        m_dot_out_adj = m_dot_in - (m_min - m_m_prev) / timestep;
    }
    else {
        m_dot_out_adj = m_dot_out;
    }
	m_V_calc = m_m_calc / rho;					//[m^3] Available volume at end of timestep (using initial temperature...)

    // Check for continual empty tank
    if (m_m_prev <= 1e-4 && tank_is_empty == true) {
        if (m_dot_in > 0) {
            m_T_calc = T_ave = T_in;
        }
        else {
            m_T_calc = T_ave = m_T_prev;
        }
        q_dot_loss = m_V_calc = m_m_calc = q_heater = 0.;
        return;
    }

    double diff_m_dot = m_dot_in - m_dot_out_adj;   //[kg/s]
    if (diff_m_dot >= 0.0)
    {
        diff_m_dot = std::max(diff_m_dot, 1.E-5);
    }
    else
    {
        diff_m_dot = std::min(diff_m_dot, -1.E-5);
    }

	if( diff_m_dot != 0.0 )
	{
		double a_coef = m_dot_in*T_in + m_UA / cp*T_amb;
		double b_coef = m_dot_in + m_UA / cp;
        double c_coef = diff_m_dot;

		m_T_calc = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow( max( (timestep*c_coef / m_m_prev + 1), 0.0), -b_coef / c_coef);
		T_ave = a_coef / b_coef + m_m_prev*(m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow( max( (timestep*c_coef / m_m_prev + 1.0), 0.0), 1.0 -b_coef/c_coef) - 1.0);
		if (timestep < 1.e-6)
			T_ave = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow( max( (timestep*c_coef / m_m_prev + 1.0), 0.0), -b_coef / c_coef);	// Limiting expression for small time step	
		q_dot_loss = m_UA*(T_ave - T_amb)/1.E6;		//[MW]

		if( m_T_calc < m_T_htr )
		{
				q_heater = b_coef*((m_T_htr - m_T_prev*pow( max( (timestep*c_coef / m_m_prev + 1), 0.0), -b_coef / c_coef)) /
					(-pow( max( (timestep*c_coef / m_m_prev + 1), 0.0), -b_coef / c_coef) + 1)) - a_coef;

				q_heater = q_heater*cp;

				q_heater /= 1.E6;
		}
		else
		{
			q_heater = 0.0;
			return;
		}

		if( q_heater > m_max_q_htr )
		{
			q_heater = m_max_q_htr;
		}

		a_coef += q_heater*1.E6 / cp;

		m_T_calc = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow( max( (timestep*c_coef / m_m_prev + 1), 0.0), -b_coef / c_coef);
		T_ave = a_coef / b_coef + m_m_prev*(m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow( max( (timestep*c_coef / m_m_prev + 1.0), 0.0), 1.0 -b_coef/c_coef) - 1.0);
		if (timestep < 1.e-6)
			T_ave = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow( max( (timestep*c_coef / m_m_prev + 1.0), 0.0), -b_coef / c_coef);  // Limiting expression for small time step	
		q_dot_loss = m_UA*(T_ave - T_amb)/1.E6;		//[MW]

	}
	else	// No mass flow rate, tank is idle
	{
		double b_coef = m_UA / (cp*m_m_prev);
		double c_coef = m_UA / (cp*m_m_prev) * T_amb;

		m_T_calc = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef*timestep);
		T_ave = c_coef/b_coef - (m_T_prev - c_coef/b_coef)/(b_coef*timestep)*(exp(-b_coef*timestep)-1.0);
		if (timestep < 1.e-6)
			T_ave = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef*timestep);  // Limiting expression for small time step	
		q_dot_loss = m_UA*(T_ave - T_amb)/1.E6;

		if( m_T_calc < m_T_htr )
		{
			q_heater = (b_coef*(m_T_htr - m_T_prev*exp(-b_coef*timestep)) / (-exp(-b_coef*timestep) + 1.0) - c_coef)*cp*m_m_prev;
			q_heater /= 1.E6;	//[MW]
		}
		else
		{
			q_heater = 0.0;
			return;
		}

		if( q_heater > m_max_q_htr )
		{
			q_heater = m_max_q_htr;
		}

		c_coef += q_heater*1.E6 / (cp*m_m_prev);

		m_T_calc = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef*timestep);
		T_ave = c_coef / b_coef - (m_T_prev - c_coef / b_coef) / (b_coef*timestep)*(exp(-b_coef*timestep) - 1.0);
		if (timestep < 1.e-6)
			T_ave = c_coef / b_coef + (m_T_prev - c_coef / b_coef)*exp(-b_coef*timestep);  // Limiting expression for small time step	
		q_dot_loss = m_UA*(T_ave - T_amb)/1.E6;		//[MW]
	}

    if (tank_is_empty) {
        // set to actual values
        m_V_calc = 0.;
        m_m_calc = 0.;
    }
}

void C_storage_tank::energy_balance_constant_mass(double timestep /*s*/, double m_dot_in, double T_in /*K*/, double T_amb /*K*/,
	double &T_ave /*K*/, double & q_heater /*MW*/, double & q_dot_loss /*MW*/)
{
	// Get properties from tank state at the end of last time step
	double rho = mc_htf.dens(m_T_prev, 1.0);	//[kg/m^3]
	double cp = mc_htf.Cp(m_T_prev)*1000.0;		//[J/kg-K] spec heat, convert from kJ/kg-K

												// Calculate ending volume levels
	m_m_calc = m_m_prev;						//[kg] Available mass at the end of this timestep, same as previous
	m_V_calc = m_m_calc / rho;					//[m^3] Available volume at end of timestep (using initial temperature...)		


	//Analytical method
		double a_coef = m_dot_in/m_m_calc + m_UA / (m_m_calc*cp);
		double b_coef = m_dot_in / m_m_calc*T_in + m_UA / (m_m_calc*cp)*T_amb;

		m_T_calc = b_coef / a_coef - (b_coef/a_coef-m_T_prev)*exp(-a_coef*timestep);
		T_ave = b_coef / a_coef - (b_coef / a_coef - m_T_prev)*exp(-a_coef*timestep/2); //estimate of average

		//T_ave = a_coef / b_coef + m_m_prev * (m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow((timestep*c_coef / m_m_prev + 1.0), 1.0 - b_coef / c_coef) - 1.0);
		//q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]

		

		
		//Euler numerical method
		//m_T_calc = m_T_prev + m_dot_in / m_m_calc * (T_in - m_T_prev)*timestep + m_UA / (m_m_calc*cp)*(T_amb - m_T_prev)*timestep;
		//T_ave = m_T_prev + m_dot_in / m_m_calc * (T_in - m_T_prev)*timestep/2 + m_UA / (m_m_calc*cp)*(T_amb - m_T_prev)*timestep/2; //estimate for average
		
		/*if (m_T_calc < m_T_htr)
		{
			q_heater = b_coef * ((m_T_htr - m_T_prev * pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef)) /
				(-pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef) + 1)) - a_coef;

			q_heater = q_heater * cp;

			q_heater /= 1.E6;
		}
		else
		{*/
			q_heater = 0.0;
			return;
		//}

		/*if (q_heater > m_max_q_htr)
		{
			q_heater = m_max_q_htr;
		}

		a_coef += q_heater * 1.E6 / cp;

		m_T_calc = a_coef / b_coef + (m_T_prev - a_coef / b_coef)*pow((timestep*c_coef / m_m_prev + 1), -b_coef / c_coef);
		T_ave = a_coef / b_coef + m_m_prev * (m_T_prev - a_coef / b_coef) / ((c_coef - b_coef)*timestep)*(pow((timestep*c_coef / m_m_prev + 1.0), 1.0 - b_coef / c_coef) - 1.0);
		q_dot_loss = m_UA * (T_ave - T_amb) / 1.E6;		//[MW]*/

	
	/*{
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
	}*/
}

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_csp_two_tank_tes::E_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
	{C_csp_two_tank_tes::E_W_DOT_HEATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWe] TES freeze protection power
	{C_csp_two_tank_tes::E_TES_T_HOT, C_csp_reported_outputs::TS_LAST},					//[C] TES final hot tank temperature
	{C_csp_two_tank_tes::E_TES_T_COLD, C_csp_reported_outputs::TS_LAST},				//[C] TES cold temperature at end of timestep      
	{C_csp_two_tank_tes::E_M_DOT_TANK_TO_TANK, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
	{C_csp_two_tank_tes::E_MASS_COLD_TANK, C_csp_reported_outputs::TS_LAST},			//[kg] Mass in cold tank at end of timestep		
	{C_csp_two_tank_tes::E_MASS_HOT_TANK, C_csp_reported_outputs::TS_LAST},				//[kg] Mass in hot tank at end of timestep

	csp_info_invalid
};

C_csp_two_tank_tes::C_csp_two_tank_tes()
{
	m_vol_tank = m_V_tank_active = m_q_pb_design = 
		m_V_tank_hot_ini = m_cp_field_avg = m_m_dot_tes_des_over_m_dot_field_des = std::numeric_limits<double>::quiet_NaN();

	// m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();

	mc_reported_outputs.construct(S_output_info);
}

void C_csp_two_tank_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
	if( !(ms_params.m_ts_hours > 0.0) )
	{
		m_is_tes = false;
		return;		// No storage!
	}

	m_is_tes = true;

	// Declare instance of fluid class for FIELD fluid
	// Set fluid number and copy over fluid matrix if it makes sense
	if( ms_params.m_field_fl != HTFProperties::User_defined && ms_params.m_field_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_field_htfProps.SetFluid(ms_params.m_field_fl) )
		{
			throw(C_csp_exception("Field HTF code is not recognized", "Two Tank TES Initialization"));
		}
	}
	else if( ms_params.m_field_fl == HTFProperties::User_defined )
	{
		int n_rows = (int)ms_params.m_field_fl_props.nrows();
		int n_cols = (int)ms_params.m_field_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_field_htfProps.SetUserDefinedFluid(ms_params.m_field_fl_props) )
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
	if( ms_params.m_tes_fl != HTFProperties::User_defined && ms_params.m_tes_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_store_htfProps.SetFluid(ms_params.m_tes_fl) )
		{
			throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
		}	
	}
	else if( ms_params.m_tes_fl == HTFProperties::User_defined )
	{
		int n_rows = (int)ms_params.m_tes_fl_props.nrows();
		int n_cols = (int)ms_params.m_tes_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_store_htfProps.SetUserDefinedFluid(ms_params.m_tes_fl_props) )
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

	if( ms_params.m_tes_fl != ms_params.m_field_fl )
		is_hx_calc = true;
	else if( ms_params.m_field_fl != HTFProperties::User_defined )
		is_hx_calc = false;
	else
	{
		is_hx_calc = !mc_field_htfProps.equals(&mc_store_htfProps);
	}

	if( ms_params.m_is_hx != is_hx_calc )
	{
		if( is_hx_calc )
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are different, but the inputs did not specify a field-to-storage heat exchanger. The system was modeled assuming a heat exchanger.");
		else
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are identical, but the inputs specified a field-to-storage heat exchanger. The system was modeled assuming no heat exchanger.");

		ms_params.m_is_hx = is_hx_calc;
	}

    if (ms_params.m_is_hx && !ms_params.tanks_in_parallel)
    {
        mc_csp_messages.add_message(C_csp_messages::NOTICE, "The inputs specified serial TES operation, but the field and storage fluids are different"
            " The simulation model parallel TES operation.");
        ms_params.tanks_in_parallel = true;
    }

	// Calculate thermal power to PC at design
	m_q_pb_design = ms_params.m_W_dot_pc_design/ms_params.m_eta_pc*1.E6;	//[Wt]

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

	if( ms_params.m_ts_hours > 0.0 )
	{
		mc_hx.init(mc_field_htfProps, mc_store_htfProps, duty, ms_params.m_dt_hot, ms_params.m_T_field_out_des, ms_params.m_T_field_in_des);
	}

	// Do we need to define minimum and maximum thermal powers to/from storage?
	// The 'duty' definition should allow the tanks to accept whatever the field and/or power cycle can provide...

	// Calculate initial storage values
	double V_inactive = m_vol_tank - m_V_tank_active;
	double V_hot_ini = ms_params.m_f_V_hot_ini*0.01*m_V_tank_active + V_inactive;			//[m^3]
	double V_cold_ini = (1.0 - ms_params.m_f_V_hot_ini*0.01)*m_V_tank_active + V_inactive;	//[m^3]

	double T_hot_ini = ms_params.m_T_tank_hot_ini;		//[K]
	double T_cold_ini = ms_params.m_T_tank_cold_ini;	//[K]

		// Initialize cold and hot tanks
			// Hot tank
	mc_hot_tank.init(mc_store_htfProps, m_vol_tank, ms_params.m_h_tank, ms_params.m_h_tank_min, 
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_hot_tank_Thtr, ms_params.m_hot_tank_max_heat,
		V_hot_ini, T_hot_ini, ms_params.m_T_field_out_des);
			// Cold tank
	mc_cold_tank.init(mc_store_htfProps, m_vol_tank, ms_params.m_h_tank, ms_params.m_h_tank_min, 
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
		V_cold_ini, T_cold_ini, ms_params.m_T_field_in_des);

    // Size TES piping and output values
    if (ms_params.tes_lengths.ncells() < 11) {
        // set defaults
        double vals1[11] = { 0., 90., 100., 120., 0., 0., 0., 0., 80., 120., 80. };
        ms_params.tes_lengths.assign(vals1, 11);
    }

    if (ms_params.custom_tes_pipe_sizes &&
        (ms_params.tes_diams.ncells() != N_tes_pipe_sections ||
            ms_params.tes_wallthicks.ncells() != N_tes_pipe_sections)) {
        error_msg = "The number of custom TES pipe sections is not correct.";
        throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
    }
    double rho_avg = mc_field_htfProps.dens((ms_params.m_T_field_in_des + ms_params.m_T_field_out_des) / 2, 9 / 1.e-5);
    m_cp_field_avg = mc_field_htfProps.Cp((ms_params.m_T_field_in_des + ms_params.m_T_field_out_des) / 2);
	double cp_tes_avg = mc_store_htfProps.Cp((ms_params.m_T_field_in_des + ms_params.m_T_field_out_des) / 2);
	m_m_dot_tes_des_over_m_dot_field_des = m_cp_field_avg / cp_tes_avg;		//[-] assume hx cr = 1
    double m_dot_pb_design = ms_params.m_W_dot_pc_design * 1.e3 /   // convert MWe to kWe for cp [kJ/kg-K]
        (ms_params.m_eta_pc * m_cp_field_avg * (ms_params.m_T_field_out_des - ms_params.m_T_field_in_des));
    if (size_tes_piping(ms_params.V_tes_des, ms_params.tes_lengths, rho_avg,
        m_dot_pb_design, ms_params.m_solarm, ms_params.tanks_in_parallel,     // Inputs
        this->pipe_vol_tot, this->pipe_v_dot_rel, this->pipe_diams,
        this->pipe_wall_thk, this->pipe_m_dot_des, this->pipe_vel_des,        // Outputs
        ms_params.custom_tes_pipe_sizes)) {

        error_msg = "TES piping sizing failed";
        throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
    }
    this->pipe_lengths = ms_params.tes_lengths;

    if (ms_params.calc_design_pipe_vals) {
        if (size_tes_piping_TandP(mc_field_htfProps, init_inputs.T_to_cr_at_des, init_inputs.T_from_cr_at_des,
            init_inputs.P_to_cr_at_des * 1.e5, ms_params.DP_SGS * 1.e5, // bar to Pa
            ms_params.tes_lengths, ms_params.k_tes_loss_coeffs, ms_params.pipe_rough, ms_params.tanks_in_parallel,
            this->pipe_diams, this->pipe_vel_des,
            this->pipe_T_des, this->pipe_P_des,
            this->P_in_des)) {        // Outputs

            error_msg = "TES piping design temperature and pressure calculation failed";
            throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
        }
        // Adjust first two pressures after field pumps, because the field inlet pressure used above was
        // not yet corrected for the section in the TES/PB before the hot tank
        double DP_before_hot_tank = this->pipe_P_des.at(3);        // first section before hot tank
        this->pipe_P_des.at(1) += DP_before_hot_tank;
        this->pipe_P_des.at(2) += DP_before_hot_tank;

        //value(O_p_des_sgs_1, DP_before_hot_tank);           // for adjusting field design pressures
    }
}

bool C_csp_two_tank_tes::does_tes_exist()
{
	return m_is_tes;
}

double C_csp_two_tank_tes::get_hot_temp()
{
	return mc_hot_tank.get_m_T_prev();	//[K]
}

double C_csp_two_tank_tes::get_cold_temp()
{
	return mc_cold_tank.get_m_T_prev();	//[K]
}

double C_csp_two_tank_tes::get_hot_tank_vol_frac()
{
	return mc_hot_tank.get_vol_frac();
}



double C_csp_two_tank_tes::get_initial_charge_energy() 
{
    //MWh
	return m_q_pb_design * ms_params.m_ts_hours * m_V_tank_hot_ini / m_vol_tank *1.e-6;
}

double C_csp_two_tank_tes::get_min_charge_energy() 
{
    //MWh
    return 0.; //ms_params.m_q_pb_design * ms_params.m_ts_hours * ms_params.m_h_tank_min / ms_params.m_h_tank*1.e-6;
}

double C_csp_two_tank_tes::get_max_charge_energy() 
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

double C_csp_two_tank_tes::get_degradation_rate()  
{
    //calculates an approximate "average" tank heat loss rate based on some assumptions. Good for simple optimization performance projections.
    double d_tank = sqrt( m_vol_tank / ( (double)ms_params.m_tank_pairs * ms_params.m_h_tank * 3.14159) );
    double e_loss = ms_params.m_u_tank * 3.14159 * ms_params.m_tank_pairs * d_tank * ( ms_params.m_T_field_in_des + ms_params.m_T_field_out_des - 576.3 )*1.e-6;  //MJ/s  -- assumes full area for loss, Tamb = 15C
	return e_loss / (m_q_pb_design * ms_params.m_ts_hours * 3600.); //s^-1  -- fraction of heat loss per second based on full charge
}

double C_csp_two_tank_tes::get_tes_m_dot(double m_dot_field /*kg/s*/)
{
	return m_dot_field * m_m_dot_tes_des_over_m_dot_field_des;
}

double C_csp_two_tank_tes::get_field_m_dot(double m_dot_tes /*kg/s*/)
{
	return m_dot_tes / m_m_dot_tes_des_over_m_dot_field_des;
}

void C_csp_two_tank_tes::discharge_avail_est(double T_cold_K, double step_s, 
	double &q_dot_dc_est /*MWt*/, double &m_dot_field_est /*kg/s*/, double &T_hot_field_est /*K*/) 
{
	double f_storage = 0.0;		// for now, hardcode such that storage always completely discharges

	double m_dot_tank_disch_avail = mc_hot_tank.m_dot_available(f_storage, step_s);	//[kg/s]

    if (m_dot_tank_disch_avail == 0) {
        q_dot_dc_est = 0.;
        m_dot_field_est = 0.;
        T_hot_field_est = std::numeric_limits<double>::quiet_NaN();
        return;
    }

	double T_hot_ini = mc_hot_tank.get_m_T_prev();		//[K]

	if(ms_params.m_is_hx)
	{
		m_dot_field_est = get_field_m_dot(m_dot_tank_disch_avail);	//[kg/s]

		double T_cold_tes, eff;
		T_cold_tes = eff = std::numeric_limits<double>::quiet_NaN();
		mc_hx.solve(T_cold_K, m_dot_field_est,
			T_hot_ini, m_dot_tank_disch_avail,
			T_hot_field_est, T_cold_tes, eff, q_dot_dc_est);

		// If above method fails, it will throw an exception, so if we don't want to break here, need to catch and handle it
	}
	else
	{
		double cp_T_avg = mc_store_htfProps.Cp(0.5*(T_cold_K + T_hot_ini));		//[kJ/kg-K] spec heat at average temperature during discharge from hot to cold
		
		q_dot_dc_est = m_dot_tank_disch_avail * cp_T_avg * (T_hot_ini - T_cold_K)*1.E-3;	//[MW]

		m_dot_field_est = m_dot_tank_disch_avail;

		T_hot_field_est = T_hot_ini;
	}
}

void C_csp_two_tank_tes::charge_avail_est(double T_hot_K, double step_s, 
	double &q_dot_ch_est /*MWt*/, double &m_dot_field_est /*kg/s*/, double &T_cold_field_est /*K*/)
{
	double f_ch_storage = 0.0;	// for now, hardcode such that storage always completely charges

	double m_dot_tank_charge_avail = mc_cold_tank.m_dot_available(f_ch_storage, step_s);	//[kg/s]

	double T_cold_ini = mc_cold_tank.get_m_T_prev();	//[K]

	if(ms_params.m_is_hx)
	{
		m_dot_field_est = get_field_m_dot(m_dot_tank_charge_avail);		//[kg/s]

		double eff, T_hot_tes;
		eff = T_hot_tes = std::numeric_limits<double>::quiet_NaN();
		mc_hx.solve(T_hot_K, m_dot_field_est,
			T_cold_ini, m_dot_tank_charge_avail,
			T_cold_field_est, T_hot_tes, eff, q_dot_ch_est);

		// If above method fails, it will throw an exception, so if we don't want to break here, need to catch and handle it
	}
	else
	{
		double cp_T_avg = mc_store_htfProps.Cp(0.5*(T_cold_ini + T_hot_K));	//[kJ/kg-K] spec heat at average temperature during charging from cold to hot

		q_dot_ch_est = m_dot_tank_charge_avail * cp_T_avg * (T_hot_K - T_cold_ini) *1.E-3;	//[MW]

		m_dot_field_est = m_dot_tank_charge_avail;	//[kg/s]

		T_cold_field_est = T_cold_ini;				//[K]
	}
}

int C_csp_two_tank_tes::solve_tes_off_design(double timestep /*s*/, double  T_amb /*K*/, 
    double m_dot_field /*kg/s*/, double m_dot_cycle /*kg/s*/,
    double T_field_htf_out_hot /*K*/, double T_cycle_htf_out_cold /*K*/,
    double & T_cycle_htf_in_hot /*K*/, double & T_field_htf_in_cold /*K*/,
    C_csp_tes::S_csp_tes_outputs & s_outputs)		//, C_csp_solver_htf_state & s_tes_ch_htf, C_csp_solver_htf_state & s_tes_dc_htf)
{
	s_outputs = S_csp_tes_outputs();

	double m_dot_cr_to_tes_hot, m_dot_tes_hot_out, m_dot_pc_to_tes_cold, m_dot_tes_cold_out;
	m_dot_cr_to_tes_hot = m_dot_tes_hot_out = m_dot_pc_to_tes_cold = m_dot_tes_cold_out = std::numeric_limits<double>::quiet_NaN();
	double m_dot_field_to_cycle, m_dot_cycle_to_field;
	m_dot_field_to_cycle = m_dot_cycle_to_field = std::numeric_limits<double>::quiet_NaN();

	if (ms_params.tanks_in_parallel)
	{
		if (m_dot_field >= m_dot_cycle)
		{
			m_dot_cr_to_tes_hot = m_dot_field - m_dot_cycle;		//[kg/s]
			m_dot_tes_hot_out = 0.0;							//[kg/s]
			m_dot_pc_to_tes_cold = 0.0;							//[kg/s]
			m_dot_tes_cold_out = m_dot_cr_to_tes_hot;			//[kg/s]
			m_dot_field_to_cycle = m_dot_cycle;					//[kg/s]
			m_dot_cycle_to_field = m_dot_cycle;					//[kg/s]
		}
		else
		{
			m_dot_cr_to_tes_hot = 0.0;							//[kg/s]
			m_dot_tes_hot_out = m_dot_cycle - m_dot_field;		//[kg/s]
			m_dot_pc_to_tes_cold = m_dot_tes_hot_out;			//[kg/s]
			m_dot_tes_cold_out = 0.0;							//[kg/s]
			m_dot_field_to_cycle = m_dot_field;					//[kg/s]
			m_dot_cycle_to_field = m_dot_field;					//[kg/s]
		}
	}
	else
	{
		if (ms_params.m_is_hx)
		{
			throw(C_csp_exception("Serial operation of C_csp_two_tank_tes not available if there is a storage HX"));
		}

		m_dot_cr_to_tes_hot = m_dot_field;		//[kg/s]
		m_dot_tes_hot_out = m_dot_cycle;		//[kg/s]
		m_dot_pc_to_tes_cold = m_dot_cycle;		//[kg/s]
		m_dot_tes_cold_out = m_dot_field;		//[kg/s]
		m_dot_field_to_cycle = 0.0;				//[kg/s]
		m_dot_cycle_to_field = 0.0;				//[kg/s]
	}


	double q_dot_heater = std::numeric_limits<double>::quiet_NaN();			//[MWe]  Heating power required to keep tanks at a minimum temperature
	double m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();	//[kg/s] Hot tank mass flow rate, valid for direct and indirect systems
	double W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();		//[MWe]  Pumping power, just for tank-to-tank in indirect storage
	double q_dot_loss = std::numeric_limits<double>::quiet_NaN();			//[MWt]  Storage thermal losses
	double q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();		//[MWt]  Thermal power to the HTF from storage
	double q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();	//[MWt]  Thermal power from the HTF to storage
	double T_hot_ave = std::numeric_limits<double>::quiet_NaN();		    //[K]    Average hot tank temperature over timestep
	double T_cold_ave = std::numeric_limits<double>::quiet_NaN();			//[K]    Average cold tank temperature over timestep
	double T_hot_final = std::numeric_limits<double>::quiet_NaN();			//[K]    Hot tank temperature at end of timestep
	double T_cold_final = std::numeric_limits<double>::quiet_NaN();			//[K]    Cold tank temperature at end of timestep


	if (ms_params.tanks_in_parallel)	
    {

		if (m_dot_field >= m_dot_cycle) // Charging
        {
            T_cycle_htf_in_hot = T_field_htf_out_hot;       //[K]
            double m_dot_tes_ch = m_dot_field - m_dot_cycle;    //[kg/s]
            double T_htf_tes_cold = std::numeric_limits<double>::quiet_NaN();   //[K]
            bool ch_solved = charge(timestep,
                T_amb,
                m_dot_tes_ch,
                T_field_htf_out_hot,
                T_htf_tes_cold,
                q_dot_heater, m_dot_cold_tank_to_hot_tank, W_dot_rhtf_pump,
				q_dot_loss, q_dot_dc_to_htf, q_dot_ch_from_htf,
				T_hot_ave, T_cold_ave, T_hot_final, T_cold_final);

            // Check if TES.charge method solved
            if (!ch_solved)
            {
                return -3;
            }

            // Enthalpy balance to calculate T_htf_cold to CR
            if (m_dot_field == 0.0)
            {
                T_field_htf_in_cold = T_htf_tes_cold;       //[K]
            }
            else
            {
                T_field_htf_in_cold = (m_dot_tes_ch*T_htf_tes_cold + m_dot_cycle * T_cycle_htf_out_cold) / m_dot_field;       //[K]
            }            
        }
        else    // Discharging
        {
            T_field_htf_in_cold = T_cycle_htf_out_cold;     //[K]
            double m_dot_tes_dc = m_dot_cycle - m_dot_field;    //[kg/s]
            double T_htf_tes_hot = std::numeric_limits<double>::quiet_NaN();
            bool is_tes_success = discharge(timestep,
                T_amb,
                m_dot_tes_dc,
                T_cycle_htf_out_cold,
                T_htf_tes_hot,
				q_dot_heater, m_dot_cold_tank_to_hot_tank, W_dot_rhtf_pump,
				q_dot_loss, q_dot_dc_to_htf, q_dot_ch_from_htf,
				T_hot_ave, T_cold_ave, T_hot_final, T_cold_final);

			m_dot_cold_tank_to_hot_tank *= -1.0;

            // Check if discharge method solved
            if (!is_tes_success)
            {
                return -4;
            }

            T_cycle_htf_in_hot = (m_dot_tes_dc*T_htf_tes_hot + m_dot_field * T_field_htf_out_hot) / m_dot_cycle;   //[K]
        }
    }
    else // Serial tank operation
    {
        if (ms_params.m_is_hx)
        {
            throw(C_csp_exception("C_csp_two_tank_tes::discharge_decoupled not available if there is a storage HX"));
        }

        // Inputs are:
        // 1) Mass flow rate of HTF through the field
        // 2) Mass flow rate of HTF 
        // 3) Temperature of HTF leaving field and entering hot tank
        // 4) Temperature of HTF leaving the power cycle and entering the cold tank

		double q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_field_est;
		q_dot_ch_est = m_dot_tes_ch_max = T_cold_to_field_est = std::numeric_limits<double>::quiet_NaN();
		charge_avail_est(T_field_htf_out_hot, timestep, q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_field_est);

        if (m_dot_field > m_dot_cycle && std::max(1.E-4,(m_dot_field - m_dot_cycle)) > 1.0001*std::max(1.E-4,m_dot_tes_ch_max))
        {
            q_dot_heater = std::numeric_limits<double>::quiet_NaN();
			m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();
            W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
            q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
            q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
            T_hot_ave = std::numeric_limits<double>::quiet_NaN();
            T_cold_ave = std::numeric_limits<double>::quiet_NaN();
            T_hot_final = std::numeric_limits<double>::quiet_NaN();
            T_cold_final = std::numeric_limits<double>::quiet_NaN();

            return -1;
        }

		double q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est;
		q_dot_dc_est = m_dot_tes_dc_max = T_hot_to_pc_est = std::numeric_limits<double>::quiet_NaN();
		discharge_avail_est(T_cycle_htf_out_cold, timestep, q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est);

        if (m_dot_cycle > m_dot_field && std::max(1.E-4,(m_dot_cycle - m_dot_field)) > 1.0001*std::max(1.E-4,m_dot_tes_dc_max))
        {
            q_dot_heater = std::numeric_limits<double>::quiet_NaN();
			m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();
            W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
            q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
            q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
            T_hot_ave = std::numeric_limits<double>::quiet_NaN();
            T_cold_ave = std::numeric_limits<double>::quiet_NaN();
            T_hot_final = std::numeric_limits<double>::quiet_NaN();
            T_cold_final = std::numeric_limits<double>::quiet_NaN();

            return -2;
        }

		// serial operation constrained to direct configuration, so HTF leaving TES must pass through another plant component
		m_dot_cold_tank_to_hot_tank = 0.0;	//[kg/s]

        double q_heater_hot, q_dot_loss_hot, q_heater_cold, q_dot_loss_cold;
        q_heater_hot = q_dot_loss_hot = q_heater_cold = q_dot_loss_cold = std::numeric_limits<double>::quiet_NaN();

        // Call energy balance on hot tank discharge to get average outlet temperature over timestep
        mc_hot_tank.energy_balance(timestep, m_dot_field, m_dot_cycle, T_field_htf_out_hot, T_amb,
            T_cycle_htf_in_hot, q_heater_hot, q_dot_loss_hot);

        // Call energy balance on cold tank charge to track tank mass and temperature
        mc_cold_tank.energy_balance(timestep, m_dot_cycle, m_dot_field, T_cycle_htf_out_cold, T_amb,
            T_field_htf_in_cold, q_heater_cold, q_dot_loss_cold);

        // Set output structure
        q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]
        double m_dot_tes_abs_net = fabs(m_dot_cycle - m_dot_field);          //[kg/s]

        W_dot_rhtf_pump = 0;                              //[MWe] Tank-to-tank pumping power

        q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
        q_dot_ch_from_htf = 0.0;		                    //[MWt]
        T_hot_ave = T_cycle_htf_in_hot;						//[K]
        T_cold_ave = T_field_htf_in_cold;						//[K]
        T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
        T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

        // Net TES discharge
		double q_dot_tes_net_discharge = m_cp_field_avg * (m_dot_tes_hot_out * T_hot_ave + m_dot_tes_cold_out * T_cold_ave -
			m_dot_cr_to_tes_hot * T_field_htf_out_hot - m_dot_pc_to_tes_cold * T_cycle_htf_out_cold) / 1000.0;		//[MWt]

        if (m_dot_cycle >= m_dot_field)
        {
            //double T_htf_ave = 0.5*(T_cycle_htf_in_hot + T_cycle_htf_out_cold);	//[K]
            //double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		    //[kJ/kg-K]
            //q_dot_dc_to_htf = (m_dot_cycle - m_dot_field) * cp_htf_ave*(T_cycle_htf_in_hot - T_cycle_htf_out_cold) / 1000.0;		//[MWt]
            q_dot_ch_from_htf = 0.0;

			q_dot_dc_to_htf = q_dot_tes_net_discharge;	//[MWt]
        }
        else
        {
            //double T_htf_ave = 0.5*(T_field_htf_out_hot + T_field_htf_in_cold);	//[K]
            //double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		    //[kJ/kg-K]
            q_dot_dc_to_htf = 0.0;
            //q_dot_ch_from_htf = (m_dot_field - m_dot_cycle) * cp_htf_ave*(T_field_htf_out_hot - T_field_htf_in_cold) / 1000.0;		//[MWt]

			q_dot_ch_from_htf = -q_dot_tes_net_discharge;	//[MWt]
        }

    }

	s_outputs.m_q_heater = q_dot_heater;
	s_outputs.m_q_dot_dc_to_htf = q_dot_dc_to_htf;
	s_outputs.m_q_dot_ch_from_htf = q_dot_ch_from_htf;

	s_outputs.m_m_dot_cr_to_tes_hot = m_dot_cr_to_tes_hot;		//[kg/s]
	s_outputs.m_m_dot_tes_hot_out = m_dot_tes_hot_out;			//[kg/s]
	s_outputs.m_m_dot_pc_to_tes_cold = m_dot_pc_to_tes_cold;	//[kg/s]
	s_outputs.m_m_dot_tes_cold_out = m_dot_tes_cold_out;		//[kg/s]
	s_outputs.m_m_dot_field_to_cycle = m_dot_field_to_cycle;	//[kg/s]
	s_outputs.m_m_dot_cycle_to_field = m_dot_cycle_to_field;	//[kg/s]

	s_outputs.m_m_dot_cold_tank_to_hot_tank = m_dot_cold_tank_to_hot_tank;

	mc_reported_outputs.value(E_Q_DOT_LOSS, q_dot_loss);		//[MWt]
	mc_reported_outputs.value(E_W_DOT_HEATER, q_dot_heater);		//[MWt]
	mc_reported_outputs.value(E_TES_T_HOT, T_hot_final - 273.15);	//[C]
	mc_reported_outputs.value(E_TES_T_COLD, T_cold_final - 273.15);	//[C]
	mc_reported_outputs.value(E_M_DOT_TANK_TO_TANK, m_dot_cold_tank_to_hot_tank);	//[kg/s]
	mc_reported_outputs.value(E_MASS_COLD_TANK, mc_cold_tank.get_m_m_calc());		//[kg]
	mc_reported_outputs.value(E_MASS_HOT_TANK, mc_hot_tank.get_m_m_calc());			//[kg]

    return 0;
}

bool C_csp_two_tank_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_cold_in /*K*/, double & T_htf_hot_out /*K*/, 
	double & q_dot_heater /*MWe*/, double & m_dot_tank_to_tank /*kg/s*/, double & W_dot_rhtf_pump /*MWe*/,
	double & q_dot_loss /*MWt*/, double & q_dot_dc_to_htf /*MWt*/, double & q_dot_ch_from_htf /*MWt*/,
	double & T_hot_ave /*K*/, double & T_cold_ave /*K*/, double & T_hot_final /*K*/, double & T_cold_final /*K*/)
{
	// This method calculates the timestep-average hot discharge temperature of the TES system. This is out of the field side of the heat exchanger (HX), opposite the tank (or 'TES') side,
	// or if no HX (direct storage), this is equal to the hot tank outlet temperature.

    // The method returns FALSE if the system output (same as input) mass flow rate is greater than that available

	// Inputs are:
	// 1) Mass flow rate of HTF into TES system (equal to that exiting the system)
	// 2) Temperature of HTF into TES system. If no heat exchanger, this temperature
	//	   is of the HTF directly entering the cold tank

	double q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est;
	q_dot_dc_est = m_dot_tes_dc_max = T_hot_to_pc_est = std::numeric_limits<double>::quiet_NaN();
	discharge_avail_est(T_htf_cold_in, timestep, q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est);

    if (m_dot_htf_in > 1.0001*m_dot_tes_dc_max && m_dot_htf_in > 1.E-6)      // mass flow in = mass flow out
    {
        q_dot_heater = std::numeric_limits<double>::quiet_NaN();
        m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();
        W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
        q_dot_loss = std::numeric_limits<double>::quiet_NaN();
        q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
        q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
        T_hot_ave = std::numeric_limits<double>::quiet_NaN();
        T_cold_ave = std::numeric_limits<double>::quiet_NaN();
        T_hot_final = std::numeric_limits<double>::quiet_NaN();
        T_cold_final = std::numeric_limits<double>::quiet_NaN();

        return false;
    }

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, m_dot_field, 
		T_field_cold_in, T_field_hot_out, m_dot_tank, T_cold_tank_in;
    q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = T_hot_ave = 
		m_dot_field = T_field_cold_in = T_field_hot_out = m_dot_tank = T_cold_tank_in = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if(!ms_params.m_is_hx)
	{
        m_dot_field = m_dot_tank = m_dot_htf_in;
        T_field_cold_in = T_cold_tank_in = T_htf_cold_in;

		// Call energy balance on hot tank discharge to get average outlet temperature over timestep
		mc_hot_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to track tank mass and temperature
		mc_cold_tank.energy_balance(timestep, m_dot_tank, 0.0, T_cold_tank_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
	}
	else 
	{	
		T_field_cold_in = T_htf_cold_in;	//[K]
		
		m_dot_field = m_dot_htf_in;	//[kg/s]
		m_dot_tank = get_tes_m_dot(m_dot_field);
		
		mc_hot_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

		double eff_hx, q_dot_hx;
		eff_hx = q_dot_hx = std::numeric_limits<double>::quiet_NaN();
		mc_hx.solve(T_htf_cold_in, m_dot_field,
					T_hot_ave, m_dot_tank,
					T_field_hot_out, T_cold_tank_in,
					eff_hx, q_dot_hx);

		mc_cold_tank.energy_balance(timestep, m_dot_tank, 0.0, T_cold_tank_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
		
		//// Iterate between field htf - hx - and storage
  //      m_dot_field = m_dot_htf_in;
  //      T_field_cold_in = T_htf_cold_in;
  //      C_MEQ_indirect_tes_discharge c_tes_disch(this, timestep, T_amb, T_field_cold_in, m_dot_field);
  //      C_monotonic_eq_solver c_tes_disch_solver(c_tes_disch);

  //      // Set up solver for tank mass flow
  //      double m_dot_tank_lower = 0;
  //      double m_dot_tank_upper = mc_hot_tank.m_dot_available(0.0, timestep);
  //      c_tes_disch_solver.settings(1.E-3, 50, m_dot_tank_lower, m_dot_tank_upper, false);

  //      // Guess tank mass flows
  //      double T_tank_guess_1, m_dot_tank_guess_1, T_tank_guess_2, m_dot_tank_guess_2;
  //      T_tank_guess_1 = m_dot_tank_guess_1 = T_tank_guess_2 = m_dot_tank_guess_2 = std::numeric_limits<double>::quiet_NaN();
  //      double eff_guess, T_hot_field_guess, T_cold_tes_guess, q_trans_guess;
  //      eff_guess = T_hot_field_guess = T_cold_tes_guess = q_trans_guess = std::numeric_limits<double>::quiet_NaN();

  //      T_tank_guess_1 = mc_hot_tank.get_m_T_prev();                                // the highest possible tank temp and thus highest resulting mass flow
  //      mc_hx.hx_discharge_mdot_fieldabc(T_field_cold_in, m_dot_field, T_tank_guess_1,
  //          eff_guess, T_hot_field_guess, T_cold_tes_guess, q_trans_guess, m_dot_tank_guess_1);

  //      double T_s, mCp, Q_u, L_s, UA;
  //      T_s = mc_hot_tank.get_m_T_prev();
  //      mCp = mc_hot_tank.calc_mass_at_prev() * mc_hot_tank.calc_cp_at_prev();  // [J/K]
  //      Q_u = 0.;   // [W]
  //      //L_s = mc_hot_tank.m_dot_available(0.0, timestep) * mc_hot_tank.calc_enth_at_prev();       // maybe use this instead?
  //      L_s = m_dot_tank_guess_1 * mc_hot_tank.calc_enth_at_prev();  // [W]  using highest mass flow, calculated from the first guess
  //      UA = mc_hot_tank.get_m_UA();
  //      T_tank_guess_2 = T_s + timestep / mCp * (Q_u - L_s - UA * (T_s - T_amb));   // tank energy balance, giving the lowest estimated tank temp due to L_s choice
  //      mc_hx.hx_discharge_mdot_fieldabc(T_field_cold_in, m_dot_field, T_tank_guess_2,
  //          eff_guess, T_hot_field_guess, T_cold_tes_guess, q_trans_guess, m_dot_tank_guess_2);

  //      // Constrain guesses to within limits
  //      m_dot_tank_guess_1 = fmax(m_dot_tank_lower, fmin(m_dot_tank_guess_1, m_dot_tank_upper));
  //      m_dot_tank_guess_2 = fmax(m_dot_tank_lower, fmin(m_dot_tank_guess_2, m_dot_tank_upper));

  //      bool m_dot_solved = false;
  //      if (m_dot_tank_guess_1 == m_dot_tank_guess_2) {
  //          // First try if guess solves
  //          double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
  //          int m_dot_bal_code = c_tes_disch_solver.test_member_function(m_dot_tank_guess_1, &m_dot_bal);

  //          if (m_dot_bal < 1.E-3) {
  //              m_dot_tank = m_dot_tank_guess_1;
  //              m_dot_solved = true;
  //          }
  //          else {
  //              // Adjust guesses so they're different
  //              if (m_dot_tank_guess_1 == m_dot_tank_upper) {
  //                  m_dot_tank_guess_2 -= 0.01*(m_dot_tank_upper - m_dot_tank_lower);
  //              }
  //              else {
  //                  m_dot_tank_guess_1 = fmin(m_dot_tank_upper, m_dot_tank_guess_1 + 0.01*(m_dot_tank_upper - m_dot_tank_lower));
  //              }
  //          }
  //      }
  //      else if (m_dot_tank_guess_1 == 0 || m_dot_tank_guess_2 == 0) {
  //          // Try a 0 guess
  //          double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
  //          int m_dot_bal_code = c_tes_disch_solver.test_member_function(0., &m_dot_bal);

  //          if (m_dot_bal < 1.E-3) {
  //              m_dot_tank = 0.;
  //              m_dot_solved = true;
  //          }
  //          else {
  //              // Adjust 0 guess to avoid divide by 0 errors
  //              m_dot_tank_guess_2 = fmax(m_dot_tank_guess_1, m_dot_tank_guess_2);
  //              m_dot_tank_guess_1 = 1.e-3;
  //          }
  //      }

  //      if (!m_dot_solved) {
  //          // Solve for required tank mass flow
  //          double tol_solved;
  //          tol_solved = std::numeric_limits<double>::quiet_NaN();
  //          int iter_solved = -1;
		//	int m_dot_bal_solver_code = 0;

  //          try
  //          {
  //              m_dot_bal_solver_code = c_tes_disch_solver.solve(m_dot_tank_guess_1, m_dot_tank_guess_2, -1.E-3, m_dot_tank, tol_solved, iter_solved);
  //          }
  //          catch (C_csp_exception)
  //          {
  //              throw(C_csp_exception("Failed to find a solution for the hot tank mass flow"));
  //          }

		//	if (m_dot_bal_solver_code != 3)
		//	{
		//		double abc = 1.23;
		//	}

  //          if (std::isnan(m_dot_tank)) {
  //              throw(C_csp_exception("Failed to converge on a valid tank mass flow."));
  //          }
  //      }
		//else
		//{
		//	double abcde = 1.23;
		//}

  //      // The other needed outputs are not all saved in member variables so recalculate here
  //      mc_hot_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

  //      double eff, q_trans;
  //      eff = q_trans = std::numeric_limits<double>::quiet_NaN();
  //      mc_hx.hx_discharge_mdot_fieldabc(T_field_cold_in, m_dot_field, T_hot_ave,
  //          eff, T_field_hot_out, T_cold_tank_in, q_trans, m_dot_tank);

  //      mc_cold_tank.energy_balance(timestep, m_dot_tank, 0.0, T_cold_tank_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
	}

	q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]
   
	if (!ms_params.m_is_hx) 
	{
		m_dot_tank_to_tank = 0.0;
		W_dot_rhtf_pump = 0;                          //[MWe] Just tank-to-tank pumping power
        T_htf_hot_out = T_hot_ave;
    }
    else 
	{
		m_dot_tank_to_tank = m_dot_tank;		//[kg/s]
        W_dot_rhtf_pump = m_dot_tank * ms_params.m_tes_pump_coef / 1.E3;     //[MWe] Just tank-to-tank pumping power, convert from kW/kg/s*kg/s
        T_htf_hot_out = T_field_hot_out;
    }
	
	q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
	q_dot_ch_from_htf = 0.0;		                    //[MWt]
	T_hot_ave = T_hot_ave;						    //[K]
	T_cold_ave = T_cold_ave;							//[K]
	T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

	// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_cold_in + T_htf_hot_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	q_dot_dc_to_htf = m_dot_htf_in*cp_htf_ave*(T_htf_hot_out - T_htf_cold_in)/1000.0;		//[MWt]

	return true;
}

bool C_csp_two_tank_tes::charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_hot_in /*K*/, double & T_htf_cold_out /*K*/, 
	double & q_dot_heater /*MWe*/, double & m_dot_tank_to_tank /*kg/s*/, double & W_dot_rhtf_pump /*MWe*/,
	double & q_dot_loss /*MWt*/, double & q_dot_dc_to_htf /*MWt*/, double & q_dot_ch_from_htf /*MWt*/,
	double & T_hot_ave /*K*/, double & T_cold_ave /*K*/, double & T_hot_final /*K*/, double & T_cold_final /*K*/)
{
	// This method calculates the timestep-average cold charge return temperature of the TES system. This is out of the field side of the heat exchanger (HX), opposite the tank (or 'TES') side,
    // or if no HX (direct storage), this is equal to the cold tank outlet temperature.
	
	// The method returns FALSE if the system input mass flow rate is greater than the allowable charge 

	// Inputs are:
	// 1) Mass flow rate of HTF into TES system (equal to that exiting the system)
	// 2) Temperature of HTF into TES system. If no heat exchanger, this temperature
	//	   is of the HTF directly entering the hot tank

	double q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_field_est;
	q_dot_ch_est = m_dot_tes_ch_max = T_cold_to_field_est = std::numeric_limits<double>::quiet_NaN();
	charge_avail_est(T_htf_hot_in, timestep, q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_field_est);

    if (m_dot_htf_in > 1.0001*m_dot_tes_ch_max && m_dot_htf_in > 1.E-6)
    {
        q_dot_heater = std::numeric_limits<double>::quiet_NaN();
        m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();
        W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
        q_dot_loss = std::numeric_limits<double>::quiet_NaN();
        q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
        q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
        T_hot_ave = std::numeric_limits<double>::quiet_NaN();
        T_cold_ave = std::numeric_limits<double>::quiet_NaN();
        T_hot_final = std::numeric_limits<double>::quiet_NaN();
        T_cold_final = std::numeric_limits<double>::quiet_NaN();

        return false;
    }

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, m_dot_field, 
		T_field_hot_in, T_field_cold_out, m_dot_tank, T_hot_tank_in;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = T_hot_ave = m_dot_field = 
		T_field_hot_in = T_field_cold_out = m_dot_tank = T_hot_tank_in = std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if( !ms_params.m_is_hx )
	{
        m_dot_field = m_dot_tank = m_dot_htf_in;
        T_field_hot_in = T_hot_tank_in = T_htf_hot_in;

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_cold_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature
		mc_hot_tank.energy_balance(timestep, m_dot_tank, 0.0, T_hot_tank_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
	}
	else
	{	
		T_field_hot_in = T_htf_hot_in;				//[K]

		m_dot_field = m_dot_htf_in;
		m_dot_tank = get_tes_m_dot(m_dot_field);		//[kg/s]

		mc_cold_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		double eff_hx, q_dot_hx;
		eff_hx = q_dot_hx = std::numeric_limits<double>::quiet_NaN();
		mc_hx.solve(T_field_hot_in, m_dot_field,
					T_cold_ave, m_dot_tank,
					T_field_cold_out, T_hot_tank_in,
					eff_hx, q_dot_hx);

		mc_hot_tank.energy_balance(timestep, m_dot_tank, 0.0, T_hot_tank_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

  //      m_dot_field = m_dot_htf_in;
  //      T_field_hot_in = T_htf_hot_in;
  //      C_MEQ_indirect_tes_charge c_tes_chrg(this, timestep, T_amb, T_field_hot_in, m_dot_field);
  //      C_monotonic_eq_solver c_tes_chrg_solver(c_tes_chrg);

  //      // Set up solver for tank mass flow
  //      double m_dot_tank_lower = 0;
  //      double m_dot_tank_upper = mc_cold_tank.m_dot_available(0.0, timestep);
  //      c_tes_chrg_solver.settings(1.E-3, 50, m_dot_tank_lower, m_dot_tank_upper, false);

  //      // Guess tank mass flows
  //      double T_tank_guess_1, m_dot_tank_guess_1, T_tank_guess_2, m_dot_tank_guess_2;
  //      T_tank_guess_1 = m_dot_tank_guess_1 = T_tank_guess_2 = m_dot_tank_guess_2 = std::numeric_limits<double>::quiet_NaN();
  //      double eff_guess, T_cold_field_guess, T_hot_tes_guess, q_trans_guess;
  //      eff_guess = T_cold_field_guess = T_hot_tes_guess = q_trans_guess = std::numeric_limits<double>::quiet_NaN();

  //      T_tank_guess_1 = mc_cold_tank.get_m_T_prev();                                // the highest possible tank temp and thus highest resulting mass flow
  //      mc_hx.hx_charge_mdot_fieldabc(T_field_hot_in, m_dot_field, T_tank_guess_1,
  //          eff_guess, T_cold_field_guess, T_hot_tes_guess, q_trans_guess, m_dot_tank_guess_1);

  //      double T_s, mCp, Q_u, L_s, UA;
  //      T_s = mc_cold_tank.get_m_T_prev();
  //      mCp = mc_cold_tank.calc_mass_at_prev() * mc_cold_tank.calc_cp_at_prev();  // [J/K]
  //      Q_u = 0.;   // [W]
  //      //L_s = mc_cold_tank.m_dot_available(0.0, timestep) * mc_cold_tank.calc_enth_at_prev();       // maybe use this instead?
  //      L_s = m_dot_tank_guess_1 * mc_cold_tank.calc_enth_at_prev();  // [W]  using highest mass flow, calculated from the first guess
  //      UA = mc_cold_tank.get_m_UA();
  //      T_tank_guess_2 = T_s + timestep / mCp * (Q_u - L_s - UA * (T_s - T_amb));   // tank energy balance, giving the lowest estimated tank temp due to L_s choice
  //      mc_hx.hx_charge_mdot_fieldabc(T_field_hot_in, m_dot_field, T_tank_guess_2,
  //          eff_guess, T_cold_field_guess, T_hot_tes_guess, q_trans_guess, m_dot_tank_guess_2);

  //      // Constrain guesses to within limits
  //      m_dot_tank_guess_1 = fmax(m_dot_tank_lower, fmin(m_dot_tank_guess_1, m_dot_tank_upper));
  //      m_dot_tank_guess_2 = fmax(m_dot_tank_lower, fmin(m_dot_tank_guess_2, m_dot_tank_upper));

  //      bool m_dot_solved = false;
  //      if (m_dot_tank_guess_1 == m_dot_tank_guess_2) {
  //          // First try if guess solves
  //          double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
  //          int m_dot_bal_code = c_tes_chrg_solver.test_member_function(m_dot_tank_guess_1, &m_dot_bal);

  //          if (m_dot_bal < 1.E-3) {
  //              m_dot_tank = m_dot_tank_guess_1;
  //              m_dot_solved = true;
  //          }
  //          else {
  //              // Adjust guesses so they're different
  //              if (m_dot_tank_guess_1 == m_dot_tank_upper) {
  //                  m_dot_tank_guess_2 -= 0.01*(m_dot_tank_upper - m_dot_tank_lower);
  //              }
  //              else {
  //                  m_dot_tank_guess_1 = fmin(m_dot_tank_upper, m_dot_tank_guess_1 + 0.01*(m_dot_tank_upper - m_dot_tank_lower));
  //              }
  //          }
  //      }
  //      else if (m_dot_tank_guess_1 == 0 || m_dot_tank_guess_2 == 0) {
  //          // Try a 0 guess
  //          double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
  //          int m_dot_bal_code = c_tes_chrg_solver.test_member_function(0., &m_dot_bal);

  //          if (m_dot_bal < 1.E-3) {
  //              m_dot_tank = 0.;
  //              m_dot_solved = true;
  //          }
  //          else {
  //              // Adjust 0 guess to avoid divide by 0 errors
  //              m_dot_tank_guess_2 = fmax(m_dot_tank_guess_1, m_dot_tank_guess_2);
  //              m_dot_tank_guess_1 = 1.e-3;
  //          }
  //      }

  //      if (!m_dot_solved) {
  //          // Solve for required tank mass flow
  //          double tol_solved;
  //          tol_solved = std::numeric_limits<double>::quiet_NaN();
  //          int iter_solved = -1;
		//	int m_dot_bal_solver_code = 0;

  //          try
  //          {
  //              m_dot_bal_solver_code = c_tes_chrg_solver.solve(m_dot_tank_guess_1, m_dot_tank_guess_2, -1.E-3, m_dot_tank, tol_solved, iter_solved);
  //          }
  //          catch (C_csp_exception)
  //          {
  //              throw(C_csp_exception("Failed to find a solution for the cold tank mass flow"));
  //          }

		//	if (m_dot_bal_solver_code != 3)
		//	{
		//		double abc = 1.23;
		//	}

  //          if (std::isnan(m_dot_tank)) {
  //              throw(C_csp_exception("Failed to converge on a valid tank mass flow."));
  //          }
  //      }
		//else
		//{
		//	double abcde = 1.23;
		//}

  //      // The other needed outputs are not all saved in member variables so recalculate here
  //      mc_cold_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

  //      double eff, q_trans;
  //      eff = q_trans = std::numeric_limits<double>::quiet_NaN();
  //      mc_hx.hx_charge_mdot_fieldabc(T_field_hot_in, m_dot_field, T_cold_ave,
  //          eff, T_field_cold_out, T_hot_tank_in, q_trans, m_dot_tank);

  //      mc_hot_tank.energy_balance(timestep, m_dot_tank, 0.0, T_hot_tank_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

	}

	q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]
    
	if (!ms_params.m_is_hx) 
	{
		m_dot_tank_to_tank = 0.0;
		W_dot_rhtf_pump = 0;                          //[MWe] Just tank-to-tank pumping power
        T_htf_cold_out = T_cold_ave;
    }
    else 
	{
		m_dot_tank_to_tank = m_dot_tank;
		W_dot_rhtf_pump = m_dot_tank * ms_params.m_tes_pump_coef / 1.E3;     //[MWe] Just tank-to-tank pumping power, convert from kW/kg/s*kg/s
        T_htf_cold_out = T_field_cold_out;
    }
    q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
	q_dot_dc_to_htf = 0.0;							//[MWt]
	T_hot_ave = T_hot_ave;							//[K]
	T_cold_ave = T_cold_ave;  						//[K]
	T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

	// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_hot_in + T_htf_cold_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	q_dot_ch_from_htf = m_dot_htf_in*cp_htf_ave*(T_htf_hot_in - T_htf_cold_out)/1000.0;		//[MWt]

	return true;
}

void C_csp_two_tank_tes::converged()
{
	mc_cold_tank.converged();
	mc_hot_tank.converged();
    //mc_hx.converged();
	
	mc_reported_outputs.set_timestep_outputs();

	// The max charge and discharge flow rates should be set at the beginning of each timestep
	//   during the q_dot_xx_avail_est calls
	// m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();
}

void C_csp_two_tank_tes::write_output_intervals(double report_time_start,
	const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_csp_two_tank_tes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}

int C_csp_two_tank_tes::pressure_drops(double m_dot_sf, double m_dot_pb,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
    double &P_drop_col, double &P_drop_gen)
{
    const std::size_t num_sections = 11;          // total number of col. + gen. sections
    const std::size_t bypass_section = 4;         // bypass section index
    const std::size_t gen_first_section = 5;      // first generation section index in combined col. gen. loops
    const double P_hi = 17 / 1.e-5;               // downstream SF pump pressure [Pa]
    const double P_lo = 1 / 1.e-5;                // atmospheric pressure [Pa]
    double P, T, rho, v_dot, vel;                 // htf properties
    double Area;                                  // cross-sectional pipe area
    double v_dot_sf, v_dot_pb;                    // solar field and power block vol. flow rates
    double k;                                     // effective minor loss coefficient
    double Re, ff;
    double v_dot_ref;
    double DP_SGS;
    std::vector<double> P_drops(num_sections, 0.0);

    util::matrix_t<double> L = this->ms_params.tes_lengths;
    util::matrix_t<double> D = this->pipe_diams;
    util::matrix_t<double> k_coeffs = this->ms_params.k_tes_loss_coeffs;
    util::matrix_t<double> v_dot_rel = this->pipe_v_dot_rel;
    m_dot_pb > 0 ? DP_SGS = this->ms_params.DP_SGS * 1.e5 : DP_SGS = 0.;
    v_dot_sf = m_dot_sf / this->mc_field_htfProps.dens((T_sf_in + T_sf_out) / 2, (P_hi + P_lo) / 2);
    v_dot_pb = m_dot_pb / this->mc_field_htfProps.dens((T_pb_in + T_pb_out) / 2, P_lo);

    for (std::size_t i = 0; i < num_sections; i++) {
        if (L.at(i) > 0 && D.at(i) > 0) {
            (i > 0 && i < 3) ? P = P_hi : P = P_lo;
            if (i < 3) T = T_sf_in;                                                       // 0, 1, 2
            if (i == 3 || i == 4) T = T_sf_out;                                           // 3, 4
            if (i >= gen_first_section && i < gen_first_section + 4) T = T_pb_in;         // 5, 6, 7, 8
            if (i == gen_first_section + 4) T = (T_pb_in + T_pb_out) / 2.;                // 9
            if (i == gen_first_section + 5) T = T_pb_out;                                 // 10
            i < gen_first_section ? v_dot_ref = v_dot_sf : v_dot_ref = v_dot_pb;
            v_dot = v_dot_rel.at(i) * v_dot_ref;
            Area = CSP::pi * pow(D.at(i), 2) / 4.;
            vel = v_dot / Area;
            rho = this->mc_field_htfProps.dens(T, P);
            Re = this->mc_field_htfProps.Re(T, P, vel, D.at(i));
            ff = CSP::FrictionFactor(this->ms_params.pipe_rough / D.at(i), Re);
            if (i != bypass_section || recirculating) {
                P_drops.at(i) += CSP::MajorPressureDrop(vel, rho, ff, L.at(i), D.at(i));
                P_drops.at(i) += CSP::MinorPressureDrop(vel, rho, k_coeffs.at(i));
            }
        }
    }

    P_drop_col = std::accumulate(P_drops.begin(), P_drops.begin() + gen_first_section, 0.0);
    P_drop_gen = DP_SGS + std::accumulate(P_drops.begin() + gen_first_section, P_drops.end(), 0.0);

    return 0;
}

double C_csp_two_tank_tes::pumping_power(double m_dot_sf, double m_dot_pb, double m_dot_tank,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating)
{
    double htf_pump_power = 0.;
    double rho_sf, rho_pb;
    double DP_col, DP_gen;
    double tes_pump_coef = this->ms_params.m_tes_pump_coef;
    double pb_pump_coef = this->ms_params.m_htf_pump_coef;
    double eta_pump = this->ms_params.eta_pump;
    
    if (this->ms_params.custom_tes_p_loss) {
        double rho_sf, rho_pb;
        double DP_col, DP_gen;
        this->pressure_drops(m_dot_sf, m_dot_pb, T_sf_in, T_sf_out, T_pb_in, T_pb_out, recirculating,
            DP_col, DP_gen);
        rho_sf = this->mc_field_htfProps.dens((T_sf_in + T_sf_out) / 2., 8e5);
        rho_pb = this->mc_field_htfProps.dens((T_pb_in + T_pb_out) / 2., 1e5);
        if (this->ms_params.m_is_hx) {
            // TODO - replace tes_pump_coef with a pump efficiency. Maybe utilize unused coefficients specified for the
            //  series configuration, namely the SGS Pump suction header to Individual SGS pump inlet and the additional
            //  two immediately downstream
            htf_pump_power = tes_pump_coef * m_dot_tank / 1000 +
                (DP_col * m_dot_sf / (rho_sf * eta_pump) + DP_gen * m_dot_pb / (rho_pb * eta_pump)) / 1e6;   //[MW]
        }
        else {
            htf_pump_power = (DP_col * m_dot_sf / (rho_sf * eta_pump) + DP_gen * m_dot_pb / (rho_pb * eta_pump)) / 1e6;   //[MW]
        }
    }
    else {    // original methods
        if (this->ms_params.m_is_hx) 
		{
            // Also going to be tanks_in_parallel = true if there's a hx between field and TES HTF
			htf_pump_power = (tes_pump_coef * m_dot_tank + tes_pump_coef * fabs(m_dot_pb - m_dot_sf)) / 1000.0;		//[MWe]
			//htf_pump_power = (tes_pump_coef*m_dot_tank + pb_pump_coef * (fabs(m_dot_pb - m_dot_sf) + m_dot_pb)) / 1000.0;	//[MW]
        }
        else 
		{
			htf_pump_power = 0.0;	//[MWe]
			//if (this->ms_params.tanks_in_parallel) {
            //    htf_pump_power = pb_pump_coef * (fabs(m_dot_pb - m_dot_sf) + m_dot_pb) / 1000.0;	//[MW]
            //}
            //else {
            //    htf_pump_power = pb_pump_coef * m_dot_pb / 1000.0;	//[MW]
            //}
        }
    }

    return htf_pump_power;
}

void two_tank_tes_sizing(HTFProperties &tes_htf_props, double Q_tes_des /*MWt-hr*/, double T_tes_hot /*K*/,
	double T_tes_cold /*K*/, double h_min /*m*/, double h_tank /*m*/, int tank_pairs /*-*/, double u_tank /*W/m^2-K*/,
	double & vol_one_temp_avail /*m3*/, double & vol_one_temp_total /*m3*/, double & d_tank /*m*/,
	double & q_dot_loss_des /*MWt*/)
{
	double T_tes_ave = 0.5*(T_tes_hot + T_tes_cold);		//[K]
	
	double rho_ave = tes_htf_props.dens(T_tes_ave, 1.0);		//[kg/m^3] Density at average temperature
	double cp_ave = tes_htf_props.Cp(T_tes_ave);				//[kJ/kg-K] Specific heat at average temperature

	// Volume required to supply design hours of thermal energy storage
		//[m^3] = [MJ/s-hr] * [sec]/[hr] = [MJ] / (kg/m^3 * MJ/kg-K * K 
	vol_one_temp_avail = Q_tes_des*3600.0 / (rho_ave * cp_ave / 1000.0 * (T_tes_hot - T_tes_cold));

	// Additional volume necessary due to minimum tank limits
	vol_one_temp_total = vol_one_temp_avail / (1.0 - h_min / h_tank);	//[m^3]

	double A_cs = vol_one_temp_total / (h_tank*tank_pairs);		//[m^2] Cross-sectional area of a single tank

	d_tank = pow(A_cs / CSP::pi, 0.5)*2.0;			//[m] Diameter of a single tank

	double UA_tank = u_tank*(A_cs + CSP::pi*d_tank*h_tank)*tank_pairs;		//[W/K]

	q_dot_loss_des = UA_tank*(T_tes_ave - 15.0)*1.E-6;	//[MWt]
		
}

int size_tes_piping(double vel_dsn, util::matrix_t<double> L, double rho_avg, double m_dot_pb, double solarm,
    bool tanks_in_parallel, double &vol_tot, util::matrix_t<double> &v_dot_rel, util::matrix_t<double> &diams,
    util::matrix_t<double> &wall_thk, util::matrix_t<double> &m_dot, util::matrix_t<double> &vel, bool custom_sizes)
{
    const std::size_t bypass_index = 4;
    const std::size_t gen_first_index = 5;      // first generation section index in combined col. gen. loops
    double m_dot_sf;
    double v_dot_sf, v_dot_pb;                  // solar field and power block vol. flow rates
    double v_dot_ref;
    double v_dot;                               // volumetric flow rate
    double Area;
    vol_tot = 0.0;                              // total volume in SGS piping
    std::size_t nPipes = L.ncells();
    v_dot_rel.resize_fill(nPipes, 0.0);         // volumetric flow rate relative to the solar field or power block flow
    m_dot.resize_fill(nPipes, 0.0);
    vel.resize_fill(nPipes, 0.0);
    std::vector<int> sections_no_bypass;
    if (!custom_sizes) {
        diams.resize_fill(nPipes, 0.0);
        wall_thk.resize_fill(nPipes, 0.0);
    }

    m_dot_sf = m_dot_pb * solarm;
    v_dot_sf = m_dot_sf / rho_avg;
    v_dot_pb = m_dot_pb / rho_avg;

    //The volumetric flow rate relative to the solar field for each collection section (v_rel = v_dot / v_dot_pb)
    v_dot_rel.at(0) = 1.0 / 2;                  // 1 - Solar field (SF) pump suction header to individual SF pump inlet
                                                //     50% -> "/2.0" . The flow rate (i.e., diameter) is sized here for the case when one pump is down.
    v_dot_rel.at(1) = 1.0 / 2;                  // 2 - Individual SF pump discharge to SF pump discharge header
    v_dot_rel.at(2) = 1.0;                      // 3 - SF pump discharge header to collection field section headers (i.e., runners)
    v_dot_rel.at(3) = 1.0;                      // 4 - Collector field section outlet headers (i.e., runners) to expansion vessel (indirect storage) or
                                                //     hot thermal storage tank (direct storage)
    v_dot_rel.at(4) = 1.0;                      // 5 - Bypass branch - Collector field section outlet headers (i.e., runners) to pump suction header (indirect) or
                                                //     cold thermal storage tank (direct)

    //The volumetric flow rate relative to the power block for each generation section
    v_dot_rel.at(5) = 1.0 / 2;                  // 2 - SGS pump suction header to individual SGS pump inlet (applicable only for storage in series with SF)
                                                //     50% -> "/2.0" . The flow rate (i.e., diameter) is sized here for the case when one pump is down.
    v_dot_rel.at(6) = 1.0 / 2;                  // 3 - Individual SGS pump discharge to SGS pump discharge header (only for series storage)
    v_dot_rel.at(7) = 1.0;                      // 4 - SGS pump discharge header to steam generator supply header (only for series storage)

    v_dot_rel.at(8) = 1.0;                      // 5 - Steam generator supply header to inter-steam generator piping
    v_dot_rel.at(9) = 1.0;                      // 6 - Inter-steam generator piping to steam generator outlet header
    v_dot_rel.at(10) = 1.0;                     // 7 - Steam generator outlet header to SF pump suction header (indirect) or cold thermal storage tank (direct)

    if (tanks_in_parallel) {
        sections_no_bypass = { 0, 1, 2, 3, 8, 9, 10 };
    }
    else {  // tanks in series
        sections_no_bypass = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 10 };
    }

    // Collection loop followed by generation loop
    for (std::size_t i = 0; i < nPipes; i++) {
        if (L.at(i) > 0) {
            i < gen_first_index ? v_dot_ref = v_dot_sf : v_dot_ref = v_dot_pb;
            v_dot = v_dot_ref * v_dot_rel.at(i);
            if (!custom_sizes) {
                diams.at(i) = CSP::pipe_sched(sqrt(4.0*v_dot / (vel_dsn * CSP::pi)));
                wall_thk.at(i) = CSP::WallThickness(diams.at(i));
            }
            m_dot.at(i) = v_dot * rho_avg;
            Area = CSP::pi * pow(diams.at(i), 2) / 4.;
            vel.at(i) = v_dot / Area;

            // Calculate total volume, excluding bypass branch
            if (std::find(sections_no_bypass.begin(), sections_no_bypass.end(), i) != sections_no_bypass.end()) {
                vol_tot += Area * L.at(i);
            }
        }
    }

    return 0;
}

int size_tes_piping_TandP(HTFProperties &field_htf_props, double T_field_in, double T_field_out, double P_field_in, double DP_SGS,
    const util::matrix_t<double> &L, const util::matrix_t<double> &k_tes_loss_coeffs, double pipe_rough,
    bool tanks_in_parallel, const util::matrix_t<double> &diams, const util::matrix_t<double> &vel,
    util::matrix_t<double> &TES_T_des, util::matrix_t<double> &TES_P_des, double &TES_P_in)
{
    std::size_t nPipes = L.ncells();
    TES_T_des.resize_fill(nPipes, 0.0);
    TES_P_des.resize_fill(nPipes, 0.0);

    // Calculate Design Temperatures, in C
    TES_T_des.at(0) = T_field_in - 273.15;
    TES_T_des.at(1) = T_field_in - 273.15;
    TES_T_des.at(2) = T_field_in - 273.15;
    TES_T_des.at(3) = T_field_out - 273.15;
    TES_T_des.at(4) = T_field_out - 273.15;
    if (tanks_in_parallel) {
        TES_T_des.at(5) = 0;
        TES_T_des.at(6) = 0;
        TES_T_des.at(7) = 0;
    }
    else {
        TES_T_des.at(5) = T_field_out - 273.15;
        TES_T_des.at(6) = T_field_out - 273.15;
        TES_T_des.at(7) = T_field_out - 273.15;
    }
    TES_T_des.at(8) = T_field_out - 273.15;
    TES_T_des.at(9) = T_field_in - 273.15;
    TES_T_des.at(10) = T_field_in - 273.15;


    // Calculate Design Pressures, in Pa
    double ff;
    double rho_avg = field_htf_props.dens((T_field_in + T_field_out) / 2, 9 / 1.e-5);
    const double P_hi = 17 / 1.e-5;               // downstream SF pump pressure [Pa]
    const double P_lo = 1 / 1.e-5;                // atmospheric pressure [Pa]

    // P_10
    ff = CSP::FrictionFactor(pipe_rough / diams.at(10), field_htf_props.Re(TES_T_des.at(10), P_lo, vel.at(10), diams.at(10)));
    TES_P_des.at(10) = 0 +
        CSP::MajorPressureDrop(vel.at(10), rho_avg, ff, L.at(10), diams.at(10)) +
        CSP::MinorPressureDrop(vel.at(10), rho_avg, k_tes_loss_coeffs.at(10));

    // P_9
    ff = CSP::FrictionFactor(pipe_rough / diams.at(9), field_htf_props.Re(TES_T_des.at(9), P_lo, vel.at(9), diams.at(9)));
    TES_P_des.at(9) = TES_P_des.at(10) +
        CSP::MajorPressureDrop(vel.at(9), rho_avg, ff, L.at(9), diams.at(9)) +
        CSP::MinorPressureDrop(vel.at(9), rho_avg, k_tes_loss_coeffs.at(9));

    // P_8
    ff = CSP::FrictionFactor(pipe_rough / diams.at(8), field_htf_props.Re(TES_T_des.at(8), P_hi, vel.at(8), diams.at(8)));
    TES_P_des.at(8) = TES_P_des.at(9) + DP_SGS +
        CSP::MajorPressureDrop(vel.at(8), rho_avg, ff, L.at(8), diams.at(8)) +
        CSP::MinorPressureDrop(vel.at(8), rho_avg, k_tes_loss_coeffs.at(8));

    if (tanks_in_parallel) {
        TES_P_des.at(7) = 0;
        TES_P_des.at(6) = 0;
        TES_P_des.at(5) = 0;
    }
    else {
        // P_7
        ff = CSP::FrictionFactor(pipe_rough / diams.at(7), field_htf_props.Re(TES_T_des.at(7), P_hi, vel.at(7), diams.at(7)));
        TES_P_des.at(7) = TES_P_des.at(8) +
            CSP::MajorPressureDrop(vel.at(7), rho_avg, ff, L.at(7), diams.at(7)) +
            CSP::MinorPressureDrop(vel.at(7), rho_avg, k_tes_loss_coeffs.at(7));

        // P_6
        ff = CSP::FrictionFactor(pipe_rough / diams.at(6), field_htf_props.Re(TES_T_des.at(6), P_hi, vel.at(6), diams.at(6)));
        TES_P_des.at(6) = TES_P_des.at(7) +
            CSP::MajorPressureDrop(vel.at(6), rho_avg, ff, L.at(6), diams.at(6)) +
            CSP::MinorPressureDrop(vel.at(6), rho_avg, k_tes_loss_coeffs.at(6));

        // P_5
        TES_P_des.at(5) = 0;
    }

    // P_3
    ff = CSP::FrictionFactor(pipe_rough / diams.at(3), field_htf_props.Re(TES_T_des.at(3), P_lo, vel.at(3), diams.at(3)));
    TES_P_des.at(3) = 0 +
        CSP::MajorPressureDrop(vel.at(3), rho_avg, ff, L.at(3), diams.at(3)) +
        CSP::MinorPressureDrop(vel.at(3), rho_avg, k_tes_loss_coeffs.at(3));

    // P_4
    TES_P_des.at(4) = TES_P_des.at(3);

    // P_2
    ff = CSP::FrictionFactor(pipe_rough / diams.at(2), field_htf_props.Re(TES_T_des.at(2), P_hi, vel.at(2), diams.at(2)));
    TES_P_des.at(2) = P_field_in +
        CSP::MajorPressureDrop(vel.at(2), rho_avg, ff, L.at(2), diams.at(2)) +
        CSP::MinorPressureDrop(vel.at(2), rho_avg, k_tes_loss_coeffs.at(2));

    // P_1
    ff = CSP::FrictionFactor(pipe_rough / diams.at(1), field_htf_props.Re(TES_T_des.at(1), P_hi, vel.at(1), diams.at(1)));
    TES_P_des.at(1) = TES_P_des.at(2) +
        CSP::MajorPressureDrop(vel.at(1), rho_avg, ff, L.at(1), diams.at(1)) +
        CSP::MinorPressureDrop(vel.at(1), rho_avg, k_tes_loss_coeffs.at(1));

    // P_0
    TES_P_des.at(0) = 0;

    // Convert Pa to bar
    for (int i = 0; i < nPipes; i++) {
        TES_P_des.at(i) = TES_P_des.at(i) / 1.e5;
    }
    TES_P_in = TES_P_des.at(3);     // pressure at the inlet to the TES, at the field side

    return 0;
}

C_csp_cold_tes::C_csp_cold_tes()
{
	m_vol_tank = m_V_tank_active = m_q_pb_design = m_V_tank_hot_ini = std::numeric_limits<double>::quiet_NaN();

	m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();
}

void C_csp_cold_tes::init(const C_csp_cold_tes::S_csp_cold_tes_init_inputs init_inputs)
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
	double V_inactive = m_vol_tank - m_V_tank_active;
	double V_hot_ini = ms_params.m_f_V_hot_ini*0.01*m_V_tank_active + V_inactive;			//[m^3]
	double V_cold_ini = (1.0 - ms_params.m_f_V_hot_ini*0.01)*m_V_tank_active + V_inactive;	//[m^3]

	double T_hot_ini = ms_params.m_T_tank_hot_ini;		//[K]
	double T_cold_ini = ms_params.m_T_tank_cold_ini;	//[K]

														// Initialize cold and hot tanks
														// Hot tank
	mc_hot_tank.init(mc_store_htfProps, m_vol_tank, ms_params.m_h_tank, ms_params.m_h_tank_min,
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_hot_tank_Thtr, ms_params.m_hot_tank_max_heat,
		V_hot_ini, T_hot_ini, ms_params.m_T_field_out_des);
	// Cold tank
	mc_cold_tank.init(mc_store_htfProps, m_vol_tank, ms_params.m_h_tank, ms_params.m_h_tank_min,
		ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_cold_tank_Thtr, ms_params.m_cold_tank_max_heat,
		V_cold_ini, T_cold_ini, ms_params.m_T_field_in_des);

}

bool C_csp_cold_tes::does_tes_exist()
{
	return m_is_tes;
}

double C_csp_cold_tes::get_hot_temp()
{
	return mc_hot_tank.get_m_T_prev();	//[K]
}

double C_csp_cold_tes::get_cold_temp()
{
	return mc_cold_tank.get_m_T_prev();	//[K]
}


double C_csp_cold_tes::get_hot_mass()
{
	return mc_hot_tank.get_m_m_calc();	// [kg]
}

double C_csp_cold_tes::get_cold_mass()
{
	return mc_cold_tank.get_m_m_calc();	//[kg]
}

double C_csp_cold_tes::get_hot_mass_prev()
{
	return mc_hot_tank.calc_mass_at_prev();	// [kg]
}

double C_csp_cold_tes::get_cold_mass_prev()
{
	return mc_cold_tank.calc_mass_at_prev();	//[kg]
}

double C_csp_cold_tes::get_physical_volume()

{
	return m_vol_tank;				//[m^3]
}

double C_csp_cold_tes::get_hot_massflow_avail(double step_s) //[kg/sec]
{
	return mc_hot_tank.m_dot_available(0, step_s);  
}

double C_csp_cold_tes::get_cold_massflow_avail(double step_s) //[kg/sec]
{
	return mc_cold_tank.m_dot_available(0, step_s); 
}


double C_csp_cold_tes::get_initial_charge_energy()
{
	//MWh
	return m_q_pb_design * ms_params.m_ts_hours * m_V_tank_hot_ini / m_vol_tank * 1.e-6;
}

double C_csp_cold_tes::get_min_charge_energy()
{
	//MWh
	return 0.; //ms_params.m_q_pb_design * ms_params.m_ts_hours * ms_params.m_h_tank_min / ms_params.m_h_tank*1.e-6;
}

double C_csp_cold_tes::get_max_charge_energy()
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

double C_csp_cold_tes::get_degradation_rate()
{
	//calculates an approximate "average" tank heat loss rate based on some assumptions. Good for simple optimization performance projections.
	double d_tank = sqrt(m_vol_tank / ((double)ms_params.m_tank_pairs * ms_params.m_h_tank * 3.14159));
	double e_loss = ms_params.m_u_tank * 3.14159 * ms_params.m_tank_pairs * d_tank * (ms_params.m_T_field_in_des + ms_params.m_T_field_out_des - 576.3)*1.e-6;  //MJ/s  -- assumes full area for loss, Tamb = 15C
	return e_loss / (m_q_pb_design * ms_params.m_ts_hours * 3600.); //s^-1  -- fraction of heat loss per second based on full charge
}

void C_csp_cold_tes::discharge_avail_est(double T_cold_K, double step_s, double &q_dot_dc_est, double &m_dot_field_est, double &T_hot_field_est)
{
	double f_storage = 0.0;		// for now, hardcode such that storage always completely discharges

	double m_dot_tank_disch_avail = mc_hot_tank.m_dot_available(f_storage, step_s);	//[kg/s]

	double T_hot_ini = mc_hot_tank.get_m_T_prev();		//[K]

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

void C_csp_cold_tes::charge_avail_est(double T_hot_K, double step_s, double &q_dot_ch_est, double &m_dot_field_est, double &T_cold_field_est)
{
	double f_ch_storage = 0.0;	// for now, hardcode such that storage always completely charges

	double m_dot_tank_charge_avail = mc_cold_tank.m_dot_available(f_ch_storage, step_s);	//[kg/s]

	double T_cold_ini = mc_cold_tank.get_m_T_prev();	//[K]

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

void C_csp_cold_tes::discharge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_cold_in /*K*/, 
	double & T_htf_hot_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_cold_tes_outputs &outputs)
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
		mc_hot_tank.energy_balance(timestep, 0.0, m_dot_htf_out, 0.0, T_amb, T_htf_hot_out, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to track tank mass and temperature
		mc_cold_tank.energy_balance(timestep, m_dot_htf_out, 0.0, T_htf_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_cold_in + T_htf_hot_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_dc_to_htf = m_dot_htf_out * cp_htf_ave*(T_htf_hot_out - T_htf_cold_in) / 1000.0;		//[MWt]
	outputs.m_q_dot_ch_from_htf = 0.0;							//[MWt]

}

bool C_csp_cold_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_cold_in /*K*/, double & T_htf_hot_out /*K*/, S_csp_cold_tes_outputs &outputs)
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
		mc_hot_tank.energy_balance(timestep, 0.0, m_dot_htf_in, 0.0, T_amb, T_htf_hot_out, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to track tank mass and temperature
		mc_cold_tank.energy_balance(timestep, m_dot_htf_in, 0.0, T_htf_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);
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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_cold_in + T_htf_hot_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_dc_to_htf = m_dot_htf_in * cp_htf_ave*(T_htf_hot_out - T_htf_cold_in) / 1000.0;		//[MWt]
	outputs.m_q_dot_ch_from_htf = 0.0;		//[MWt]

	return true;
}

bool C_csp_cold_tes::charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, 
	double T_htf_hot_in /*K*/, double & T_htf_cold_out /*K*/, S_csp_cold_tes_outputs &outputs)
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
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_cold_tank.energy_balance(timestep, 0.0, m_dot_htf_in, 0.0, T_amb, T_htf_cold_out, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature
		mc_hot_tank.energy_balance(timestep, m_dot_htf_in, 0.0, T_htf_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_hot_in + T_htf_cold_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_htf_in * cp_htf_ave*(T_htf_hot_in - T_htf_cold_out) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}

bool C_csp_cold_tes::charge_discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_hot_in /*kg/s*/, 
	double T_hot_in /*K*/, double m_dot_cold_in /*kg/s*/, double T_cold_in /*K*/, S_csp_cold_tes_outputs &outputs)
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
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave = T_cold_ave=std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_hot_in > m_m_dot_tes_ch_max / timestep)
		{
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_cold_tank.energy_balance(timestep, m_dot_cold_in, m_dot_hot_in, T_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature
		mc_hot_tank.energy_balance(timestep, m_dot_hot_in, m_dot_cold_in, T_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);
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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_hot_in + T_cold_ave);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_hot_in * cp_htf_ave*(T_hot_in - T_cold_ave) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}

bool C_csp_cold_tes::recirculation(double timestep /*s*/, double T_amb /*K*/, double m_dot_cold_in /*kg/s*/, 
	double T_cold_in /*K*/, S_csp_cold_tes_outputs &outputs)
{
	// This method calculates the average (timestep) cold tank outlet temperature when recirculating cold fluid for further cooling.
	// This warm tank is idle and its state is also determined.

	// The method returns FALSE if the input mass flow rate 'm_dot_htf_in' * timestep is greater than the allowable charge 

	// Inputs are:
	// 1) The cold tank exit mass flow rate
	// 2) The inlet temperature of HTF directly entering the cold tank

	double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, T_hot_ave, T_cold_ave;
	q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_hot_ave =T_cold_ave= std::numeric_limits<double>::quiet_NaN();

	// If no heat exchanger, no iteration is required between the heat exchanger and storage tank models
	if (!ms_params.m_is_hx)
	{
		if (m_dot_cold_in > m_m_dot_tes_ch_max / timestep)	//Is this necessary for recirculation mode? ARD
		{
			outputs.m_q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            outputs.m_m_dot = std::numeric_limits<double>::quiet_NaN();
			outputs.m_q_heater = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_ave = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_hot_final = std::numeric_limits<double>::quiet_NaN();
			outputs.m_T_cold_final = std::numeric_limits<double>::quiet_NaN();

			return false;
		}

		// Call energy balance on cold tank discharge to get average outlet temperature over timestep
		mc_cold_tank.energy_balance_constant_mass(timestep, m_dot_cold_in, T_cold_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

		// Call energy balance on hot tank charge to track tank mass and temperature while idle
		mc_hot_tank.energy_balance(timestep, 0.0, 0.0, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K] Hot temperature at end of timestep
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K] Cold temperature at end of timestep


																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_cold_in + T_cold_ave);			//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_cold_in * cp_htf_ave*(T_cold_in - T_cold_ave) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

	return true;

}


void C_csp_cold_tes::charge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_hot_in /*K*/, 
	double & T_htf_cold_out /*K*/, double & m_dot_htf_out /*kg/s*/, S_csp_cold_tes_outputs &outputs)
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
		mc_hot_tank.energy_balance(timestep, m_dot_htf_out, 0.0, T_htf_hot_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

		// Call energy balance on cold tank charge to calculate cold HTF return temperature
		mc_cold_tank.energy_balance(timestep, 0.0, m_dot_htf_out, 0.0, T_amb, T_htf_cold_out, q_heater_cold, q_dot_loss_cold);
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
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

																// Calculate thermal power to HTF
	double T_htf_ave = 0.5*(T_htf_hot_in + T_htf_cold_out);		//[K]
	double cp_htf_ave = mc_field_htfProps.Cp(T_htf_ave);		//[kJ/kg-K]
	outputs.m_q_dot_ch_from_htf = m_dot_htf_out * cp_htf_ave*(T_htf_hot_in - T_htf_cold_out) / 1000.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;							//[MWt]

}

void C_csp_cold_tes::idle(double timestep, double T_amb, S_csp_cold_tes_outputs &outputs)
{
	double T_hot_ave, q_hot_heater, q_dot_hot_loss;
	T_hot_ave = q_hot_heater = q_dot_hot_loss = std::numeric_limits<double>::quiet_NaN();

	mc_hot_tank.energy_balance(timestep, 0.0, 0.0, 0.0, T_amb, T_hot_ave, q_hot_heater, q_dot_hot_loss);

	double T_cold_ave, q_cold_heater, q_dot_cold_loss;
	T_cold_ave = q_cold_heater = q_dot_cold_loss = std::numeric_limits<double>::quiet_NaN();

	mc_cold_tank.energy_balance(timestep, 0.0, 0.0, 0.0, T_amb, T_cold_ave, q_cold_heater, q_dot_cold_loss);

	outputs.m_q_heater = q_cold_heater + q_hot_heater;			//[MJ]
    outputs.m_m_dot = 0.;
	outputs.m_W_dot_rhtf_pump = 0.0;							//[MWe]
	outputs.m_q_dot_loss = q_dot_cold_loss + q_dot_hot_loss;	//[MW]

	outputs.m_T_hot_ave = T_hot_ave;							//[K]
	outputs.m_T_cold_ave = T_cold_ave;							//[K]
	outputs.m_T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
	outputs.m_T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

	outputs.m_q_dot_ch_from_htf = 0.0;		//[MWt]
	outputs.m_q_dot_dc_to_htf = 0.0;		//[MWt]
}

void C_csp_cold_tes::converged()
{
	mc_cold_tank.converged();
	mc_hot_tank.converged();

	// The max charge and discharge flow rates should be set at the beginning of each timestep
	//   during the q_dot_xx_avail_est calls
	m_m_dot_tes_dc_max = m_m_dot_tes_ch_max = std::numeric_limits<double>::quiet_NaN();
}

int C_csp_cold_tes::pressure_drops(double m_dot_sf, double m_dot_pb,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
    double &P_drop_col, double &P_drop_gen)
{
    P_drop_col = 0.;
    P_drop_gen = 0.;

    return 0;
}

double C_csp_cold_tes::pumping_power(double m_dot_sf, double m_dot_pb, double m_dot_tank,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating)
{
    return m_dot_tank * this->ms_params.m_htf_pump_coef / 1.E3;
}

