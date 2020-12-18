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

#include "heat_exchangers.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"
#include <algorithm>
#include "numeric_solvers.h"

double NS_HX_counterflow_eqs::calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
    int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
    double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/)
{
    double h_h_out, T_h_out, h_c_out, T_c_out, T_h_in, T_c_in;
    return calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        h_h_in, P_h_in, P_h_out, m_dot_h,
        h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out, T_h_out, 
        h_c_out, T_c_out, 
        T_h_in, T_c_in);
}

double NS_HX_counterflow_eqs::calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
	double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/,
    double & h_h_out /*kJ/kg*/, double & T_h_out /*K*/,
    double & h_c_out /*kJ/kg*/, double & T_c_out /*K*/,
    double & T_h_in /*K*/, double & T_c_in /*K*/)
{
	int prop_error_code = 0;

	// Calculate the hot and cold temperatures at the node
	T_h_in = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_PH(P_h_in, h_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side CO2 inlet enthalpy calculations failed", 12));
		}
		T_h_in = ms_co2_props.temp;		//[K]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_PH(P_h_in, h_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Hot side water/steam inlet enthalpy calculations failed", 12));
		}
		T_h_in = ms_water_props.temp;	//[K]
	}
	else
	{
		T_h_in = hot_htf_class.temp_lookup(h_h_in); //[K]
	}

	T_c_in = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_PH(P_c_in, h_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side inlet enthalpy calculations failed", 13));
		}
		T_c_in = ms_co2_props.temp;		//[K]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_PH(P_c_in, h_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Cold side water/steam inlet enthalpy calculations failed", 12));
		}
		T_c_in = ms_water_props.temp;	//[K]
	}
	else
	{
		T_c_in = cold_htf_class.temp_lookup(h_c_in);	//[K]
	}

	// **********************************************************************************
	// **********************************************************************************
	double Q_dot_cold_max = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_c_out, &ms_co2_props);
		if (prop_error_code == 205)
		{
			prop_error_code = CO2_TQ(T_h_in, 0.0, &ms_co2_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		double h_c_out_max = max(ms_co2_props.enth, h_c_in);	//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);	//[kWt]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_c_out, &ms_water_props);
		if (prop_error_code == 205)
		{
			prop_error_code = water_TQ(T_h_in, 0.0, &ms_water_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Cold side water/steam enthalpy calcs at max eff failed", 12));
		}
		double h_c_out_max = max(ms_water_props.enth, h_c_in);	//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);
	}
	else
	{
		double h_c_out_max = cold_htf_class.enth_lookup(T_h_in);//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);	//[kWt]
	}

	double Q_dot_hot_max = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_h_out, &ms_co2_props);
		if (prop_error_code == 205)
		{
			prop_error_code = CO2_TQ(T_c_in, 1.0, &ms_co2_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		double h_h_out_min = min(h_h_in, ms_co2_props.enth);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);	//[kWt]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_h_out, &ms_water_props);
		if (prop_error_code == 205)
		{
			prop_error_code = water_TQ(T_c_in, 1.0, &ms_water_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Hot side water/stream inlet enthalpy calcs at max eff failed", 12));
		}
		double h_h_out_min = min(h_h_in, ms_water_props.enth);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);		//[kWt]
	}
	else
	{
		double h_h_out_min = hot_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);		//[kWt]
	}

    double q_dot_max = min(Q_dot_hot_max, Q_dot_cold_max);  //[kWt]
    h_h_out = h_h_in - q_dot_max / m_dot_h;
    h_c_out = h_c_in + q_dot_max / m_dot_c;

    // Hot outlet temperature
    T_h_out = std::numeric_limits<double>::quiet_NaN();
    if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
    {
        CO2_state ms_co2_props;
        prop_error_code = CO2_PH(P_h_out, h_h_out, &ms_co2_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
                "Hot CO2 outlet temp from PH calcs failed", 12));
        }
        T_h_out = ms_co2_props.temp;        //[K]
    }
    else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
    {
        water_state ms_water_props;
        prop_error_code = water_PH(P_h_out, h_h_out, &ms_water_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
                "Hot side water/stream outlet temp from PH calcs failed", 12));
        }
        T_h_out = ms_water_props.temp;      //[K]
    }
    else
    {
        T_h_out = hot_htf_class.temp_lookup(h_h_out);    //[K]
    }

    // Cold outlet temperature
    T_c_out = std::numeric_limits<double>::quiet_NaN();
    if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
    {
        CO2_state ms_co2_props;
        prop_error_code = CO2_PH(P_c_out, h_c_out, &ms_co2_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
                "Cold CO2 outlet temp from PH calcs failed", 12));
        }
        T_c_out = ms_co2_props.temp;        //[K]
    }
    else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
    {
        water_state ms_water_props;
        prop_error_code = water_PH(P_c_out, h_c_out, &ms_water_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
                "Cold side water/steam outlet temp from PH calcs failed", 12));
        }
        T_c_out = ms_water_props.temp;      //[K]
    }
    else
    {
        T_c_out = cold_htf_class.temp_lookup(h_c_out);  //[K]
    }

	return q_dot_max;
}

double NS_HX_counterflow_eqs::calc_max_q_dot(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	double T_h_in, double P_h_in, double P_h_out, double m_dot_h,
	double T_c_in, double P_c_in, double P_c_out, double m_dot_c,
    double & h_h_out /*kJ/kg*/, double & T_h_out /*K*/,
    double & h_c_out /*kJ/kg*/, double & T_c_out /*K*/)
{
	int prop_error_code = 0;

	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_c_in = ms_co2_props.enth;
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side water/steam inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_c_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_c_in = cold_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
	}

	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_h_in = ms_co2_props.enth;
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side water/steam inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_h_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_h_in = hot_htf_class.enth_lookup(T_h_in);	//[kJ/kg]
	}

    double T_h_in_calc, T_c_in_calc;
    T_h_in_calc = T_c_in_calc = std::numeric_limits<double>::quiet_NaN();
	return NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
											cold_fl_code, cold_htf_class,
											h_h_in, P_h_in, P_h_out, m_dot_h,
											h_c_in, P_c_in, P_c_out, m_dot_c,
                                            h_h_out, T_h_out,
                                            h_c_out, T_c_out,
                                            T_h_in_calc, T_c_in_calc);
}

double NS_HX_counterflow_eqs::hx_fl__calc_h__TP(int fl_code, HTFProperties & fl_htf_class, 
                                                double T_K /*K*/, double P_kpa /*kPa*/)
{
    C_hx_fl__TP__core c_TP(fl_code, &fl_htf_class, T_K, P_kpa, false);
    return c_TP.m_h;        //[kJ/kg]
}

NS_HX_counterflow_eqs::C_hx_fl__TP__core::C_hx_fl__TP__core(int fl_code, HTFProperties * fl_htf_class,
    double T_K /*K*/, double P_kpa /*kPa*/, bool is_calc_cond_visc)
{
    if (fl_code == NS_HX_counterflow_eqs::CO2)
    {
        CO2_state ms_co2_props;
        int prop_error_code = CO2_TP(T_K, P_kpa, &ms_co2_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::design",
                "Cold side inlet enthalpy calculations failed", 12));
        }
        m_h = ms_co2_props.enth;        //[K]
        m_rho = ms_co2_props.dens;      //[kg/m3]
        m_cp = ms_co2_props.cp;         //[kJ/kg-K]
        if (is_calc_cond_visc)
        {
            m_k = CO2_cond(ms_co2_props.dens, ms_co2_props.temp);     //[W/m-K]
            m_mu = CO2_visc(ms_co2_props.dens, ms_co2_props.temp);    //[uPa-s]
        }
    }
    else if (fl_code == NS_HX_counterflow_eqs::WATER)
    {
        water_state ms_water_props;
        int prop_error_code = water_TP(T_K, P_kpa, &ms_water_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                "Cold side inlet enthalpy calculations failed", 12));
        }
        m_h = ms_water_props.enth;       //[K]
        m_rho = ms_water_props.dens;    //[kg/m3]
        m_cp = ms_water_props.cp;       //[kJ/kg-K]
        if (is_calc_cond_visc)
        {
            m_k = water_cond(ms_water_props.dens, ms_water_props.temp);   //[W/m-K]
            m_mu = water_visc(ms_water_props.dens, ms_water_props.temp);  //[uPa-s]
        }
    }
    else
    {
        m_h = fl_htf_class->enth_lookup(T_K);           //[kJ/kg]

        m_rho = fl_htf_class->dens(T_K, P_kpa*1.E3);    //[kg/m3]
        m_cp = fl_htf_class->Cp(T_K);
        if (is_calc_cond_visc)
        {
            m_k = fl_htf_class->cond(T_K);   //[W/m-K]
            m_mu = fl_htf_class->visc(T_K)*1.E6;  //[uPa-s]
        }
    }

    if (!is_calc_cond_visc)
    {
        m_k = m_mu = std::numeric_limits<double>::quiet_NaN();
    }
}

NS_HX_counterflow_eqs::C_hx_fl__Ph__core::C_hx_fl__Ph__core(int fl_code, HTFProperties * fl_htf_class,
    double P_kpa /*kPa*/, double h_kjkg /*kJ/kg*/, bool is_calc_cond_visc)
{
    if (fl_code == NS_HX_counterflow_eqs::CO2)
    {
        CO2_state ms_co2_props;
        int prop_error_code = CO2_PH(P_kpa, h_kjkg, &ms_co2_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::design",
                "Cold side inlet enthalpy calculations failed", 12));
        }
        m_T = ms_co2_props.temp;        //[K]
        m_rho = ms_co2_props.dens;      //[kg/m3]
        m_cp = ms_co2_props.cp;         //[kJ/kg-K]
        if (is_calc_cond_visc)
        {
            m_k = CO2_cond(ms_co2_props.dens, ms_co2_props.temp);     //[W/m-K]
            m_mu = CO2_visc(ms_co2_props.dens, ms_co2_props.temp);    //[uPa-s]
        }
    }
    else if (fl_code == NS_HX_counterflow_eqs::WATER)
    {
        water_state ms_water_props;
        int prop_error_code = water_PH(P_kpa, h_kjkg, &ms_water_props);
        if (prop_error_code != 0)
        {
            throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                "Cold side inlet enthalpy calculations failed", 12));
        }
        m_T = ms_water_props.temp;       //[K]
        m_rho = ms_water_props.dens;    //[kg/m3]
        m_cp = ms_water_props.cp;       //[kJ/kg-K]
        if (is_calc_cond_visc)
        {
            m_k = water_cond(ms_water_props.dens, ms_water_props.temp);   //[W/m-K]
            m_mu = water_visc(ms_water_props.dens, ms_water_props.temp);  //[uPa-s]
        }
    }
    else
    {
        m_T = fl_htf_class->temp_lookup(h_kjkg);	    //[K]
        m_rho = fl_htf_class->dens(m_T, P_kpa*1.E3);    //[kg/m3]
        m_cp = fl_htf_class->Cp(m_T);
        if (is_calc_cond_visc)
        {
            m_k = fl_htf_class->cond(m_T);   //[W/m-K]
            m_mu = fl_htf_class->visc(m_T)*1.E6;  //[uPa-s]
        }
    }

    
    if(!is_calc_cond_visc)
    {
        m_k = m_mu = std::numeric_limits<double>::quiet_NaN();
    }
}

double NS_HX_counterflow_eqs::hx_fl__calc_T__Ph(int fl_code, HTFProperties & fl_htf_class,
    double P_kpa /*kPa*/, double h_kjkg /*kJ/kg*/)
{
    C_hx_fl__Ph__core c_ph(fl_code, &fl_htf_class, P_kpa, h_kjkg, false);
    return c_ph.m_T;
}

void NS_HX_counterflow_eqs::calc_req_UA(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	int N_sub_hx /*-*/,
	double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    v_s_node_info.resize(0);

    // Check inputs
	if (q_dot < 0.0)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Input heat transfer rate is less than 0.0. It must be >= 0.0", 4));
	}
	if (m_dot_c < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The cold mass flow rate must be a positive value"));
	}
	if (m_dot_h < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The hot mass flow rate must be a positive value"));
	}
	if (T_h_in < T_c_in)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Inlet hot temperature is colder than the cold inlet temperature", 5));
	}
	if (P_h_in < P_h_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Hot side outlet pressure is greater than hot side inlet pressure", 6));
	}
	if (P_c_in < P_c_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Cold side outlet pressure is greater than cold side inlet pressure", 7));
	}
	if (q_dot <= 1.E-14)	// very low Q_dot; assume it is zero
	{
		// Set outputs, and S_des_solved members		
		UA = 0.0;					//[kW/K]
		NTU = 0.0;					//[-]
		q_dot_calc = 0.0;			//[kW]
		min_DT = T_h_in - T_c_in;	//[K]
		eff = 0.0;			//[-]
		T_h_out = T_h_in;	//[K]
		T_c_out = T_c_in;	//[K]

		return;
	}

	// Calculate inlet enthalpies from known state points
	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	int prop_error_code = 0;

    h_c_in = hx_fl__calc_h__TP(cold_fl_code, cold_htf_class, T_c_in, P_c_in);
    h_h_in = hx_fl__calc_h__TP(hot_fl_code, hot_htf_class, T_h_in, P_h_in);

	// Know h_h_in and h_c_in, so can call 'calc_req_UA_enth' here
	double h_c_out = std::numeric_limits<double>::quiet_NaN();
	double h_h_out = std::numeric_limits<double>::quiet_NaN();
	NS_HX_counterflow_eqs::calc_req_UA_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		N_sub_hx,
		q_dot, m_dot_c, m_dot_h, 
		h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out, 
		h_h_out, T_h_out, h_c_out, T_c_out,
		UA, min_DT, eff, NTU, q_dot_calc,
        v_s_node_info);

	return;
}

void NS_HX_counterflow_eqs::calc_req_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
    int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    int N_sub_hx /*-*/,
    double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
    double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
    double & h_h_out /*kJ/kg*/, double & T_h_out /*K*/, double & h_c_out /*kJ/kg*/, double & T_c_out /*K*/,
    double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & q_dot_calc /*kWt*/,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    v_s_node_info.resize(N_sub_hx);

    // Check inputs
    if (q_dot < 0.0)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "Input heat transfer rate is less than 0.0. It must be >= 0.0", 4));
    }
    if (m_dot_c < 1.E-14)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "The cold mass flow rate must be a positive value"));
    }
    if (m_dot_h < 1.E-14)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "The hot mass flow rate must be a positive value"));
    }
    if (P_h_in < P_h_out)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "Hot side outlet pressure is greater than hot side inlet pressure", 6));
    }
    if (P_c_in < P_c_out)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "Cold side outlet pressure is greater than cold side inlet pressure", 7));
    }

    // Calculate outlet stream states
    CO2_state ms_co2_props;
    water_state ms_water_props;
    int prop_error_code = 0;

    h_h_out = h_h_in - q_dot / m_dot_h;		//[kJ/kg]
    h_c_out = h_c_in + q_dot / m_dot_c;		//[kJ/kg]	

    int N_nodes = N_sub_hx + 1;
    double h_h_prev = 0.0;
    double T_h_prev = 0.0;
    double P_h_prev = 0.0;
    double h_c_prev = 0.0;
    double T_c_prev = 0.0;
    double P_c_prev = 0.0;
    double T_c_in = std::numeric_limits<double>::quiet_NaN();	//[K]
    double T_h_in = std::numeric_limits<double>::quiet_NaN();	//[K]

    bool is_temp_violation = false;

    // Loop through the sub-heat exchangers
    UA = 0.0;
    min_DT = T_h_in;
    for (int i = 0; i < N_nodes; i++)
    {
        // Assume pressure varies linearly through heat exchanger
        double P_c = P_c_out + i * (P_c_in - P_c_out) / (double)(N_nodes - 1);
        double P_h = P_h_in - i * (P_h_in - P_h_out) / (double)(N_nodes - 1);

        // Calculate the entahlpy at the node
        double h_c = h_c_out + i * (h_c_in - h_c_out) / (double)(N_nodes - 1);
        double h_h = h_h_in - i * (h_h_in - h_h_out) / (double)(N_nodes - 1);

        // ****************************************************
        // Calculate the hot and cold temperatures at the node
        double T_h = std::numeric_limits<double>::quiet_NaN();
        if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
        {
            prop_error_code = CO2_PH(P_h, h_h, &ms_co2_props);
            if (prop_error_code != 0)
            {
                throw(C_csp_exception("C_HX_counterflow::design",
                    "Cold side inlet enthalpy calculations failed", 12));
            }
            T_h = ms_co2_props.temp;		//[K]
        }
        else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
        {
            prop_error_code = water_PH(P_h, h_h, &ms_water_props);
            if (prop_error_code != 0)
            {
                throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                    "Cold side inlet enthalpy calculations failed", 12));
            }
            T_h = ms_water_props.temp;		//[K]
        }
        else
        {
            T_h = hot_htf_class.temp_lookup(h_h);	//[K]
        }

        double T_c = std::numeric_limits<double>::quiet_NaN();
        if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
        {
            prop_error_code = CO2_PH(P_c, h_c, &ms_co2_props);
            if (prop_error_code != 0)
            {
                throw(C_csp_exception("C_HX_counterflow::design",
                    "Cold side inlet enthalpy calculations failed", 13));
            }
            T_c = ms_co2_props.temp;	//[K]
        }
        else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
        {
            prop_error_code = water_PH(P_c, h_c, &ms_water_props);
            if (prop_error_code != 0)
            {
                throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                    "Cold side water/steam inlet enthalpy calculations failed", 13));
            }
            T_c = ms_water_props.temp;  //[K]
        }
        else
        {
            T_c = cold_htf_class.temp_lookup(h_c);	//[K]
        }

        if (i == 0)
        {
            T_h_in = T_h;
            T_c_out = T_c;
        }
        if (i == N_nodes - 1)
        {
            T_h_out = T_h;
            T_c_in = T_c;
        }

        // ****************************************************
        // ****************************************************
        // ****************************************************

        // Check that 2nd law is not violated
        if (T_c >= T_h)
        {
            is_temp_violation = true;
        }

        // Track the minimum temperature difference in the heat exchanger
        min_DT = fmin(min_DT, T_h - T_c);

        // Perform effectiveness-NTU and UA calculations 
        if (i > 0)
        {
            bool is_h_2phase = false;
            bool is_c_2phase = false;

            double h_h_avg = 0.5*(h_h_prev + h_h);
            double P_h_avg = 0.5*(P_h_prev + P_h);

            double cp_h_avg = std::numeric_limits<double>::quiet_NaN();
            v_s_node_info[i - 1].s_fl_hot.m_dot = m_dot_h;      //[kg/s]
            if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
            {
                prop_error_code = CO2_PH(P_h_avg, h_h_avg, &ms_co2_props);
                if (prop_error_code != 0)
                {
                    throw(C_csp_exception("C_HX_counterflow::design",
                        "Cold side inlet enthalpy calculations failed", 12));
                }
                cp_h_avg = ms_co2_props.cp;       //[kJ/kg-K]

                v_s_node_info[i - 1].s_fl_hot.cp = ms_co2_props.cp;   //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_hot.rho = ms_co2_props.dens;    //[kg/m3]
                v_s_node_info[i - 1].s_fl_hot.k = CO2_cond(ms_co2_props.dens, ms_co2_props.temp);     //[W/m-K]
                v_s_node_info[i - 1].s_fl_hot.mu = CO2_visc(ms_co2_props.dens, ms_co2_props.temp);    //[uPa-s]
            }
            else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
            {
                prop_error_code = water_PH(P_h_avg, h_h_avg, &ms_water_props);
                if (prop_error_code != 0)
                {
                    throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                        "Cold side inlet enthalpy calculations failed", 12));
                }
                cp_h_avg = ms_water_props.cp;       //[kJ/kg-K]

                v_s_node_info[i - 1].s_fl_hot.cp = ms_water_props.cp;   //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_hot.rho = ms_water_props.dens;    //[kg/m3]
                v_s_node_info[i - 1].s_fl_hot.k = water_cond(ms_water_props.dens, ms_water_props.temp);   //[W/m-K]
                v_s_node_info[i - 1].s_fl_hot.mu = water_visc(ms_water_props.dens, ms_water_props.temp);  //[uPa-s]
            }
            else
            {
                double T_h_avg = hot_htf_class.temp_lookup(h_h_avg);	//[K]
                cp_h_avg = hot_htf_class.Cp(T_h_avg);   //[kJ/kg-K]

                v_s_node_info[i - 1].s_fl_hot.cp = cp_h_avg; //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_hot.rho = hot_htf_class.dens(T_h_avg, P_h_avg); //[kg/m3]
                v_s_node_info[i - 1].s_fl_hot.k = hot_htf_class.cond(T_h_avg);   //[W/m-K]
                v_s_node_info[i - 1].s_fl_hot.mu = hot_htf_class.visc(T_h_avg)*1.E6;  //[uPa-s]
            }
            // Check for 2 phase conditions
            if (!(std::isfinite(cp_h_avg) && cp_h_avg > 0.0))
            {
                is_h_2phase = true;
            }

            double h_c_avg = 0.5*(h_c_prev + h_c);
            double P_c_avg = 0.5*(P_c_prev + P_c);

            double cp_c_avg = std::numeric_limits<double>::quiet_NaN();
            v_s_node_info[i - 1].s_fl_cold.m_dot = m_dot_c;     //[kg/s]
            if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
            {
                prop_error_code = CO2_PH(P_c_avg, h_c_avg, &ms_co2_props);
                if (prop_error_code != 0)
                {
                    throw(C_csp_exception("C_HX_counterflow::design",
                        "Cold side inlet enthalpy calculations failed", 13));
                }
                cp_c_avg = ms_co2_props.cp;     //[kJ/kg-K]

                v_s_node_info[i - 1].s_fl_cold.cp = ms_co2_props.cp;   //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_cold.rho = ms_co2_props.dens;    //[kg/m3]
                v_s_node_info[i - 1].s_fl_cold.k = CO2_cond(ms_co2_props.dens, ms_co2_props.temp);     //[W/m-K]
                v_s_node_info[i - 1].s_fl_cold.mu = CO2_visc(ms_co2_props.dens, ms_co2_props.temp);    //[uPa-s]
            }
            else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
            {
                prop_error_code = water_PH(P_c_avg, h_c_avg, &ms_water_props);
                if (prop_error_code != 0)
                {
                    throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
                        "Cold side water/steam inlet enthalpy calculations failed", 13));
                }
                cp_c_avg = ms_water_props.cp;   //[kJ/kg-K]

                v_s_node_info[i - 1].s_fl_cold.cp = ms_water_props.cp;   //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_cold.rho = ms_water_props.dens;    //[kg/m3]
                v_s_node_info[i - 1].s_fl_cold.k = water_cond(ms_water_props.dens, ms_water_props.temp);   //[W/m-K]
                v_s_node_info[i - 1].s_fl_cold.mu = water_visc(ms_water_props.dens, ms_water_props.temp);  //[uPa-s]
            }
            else
            {
                double T_c_avg = cold_htf_class.temp_lookup(h_c_avg);	//[K]
                cp_c_avg = cold_htf_class.Cp(T_c_avg);  //[K]

                v_s_node_info[i - 1].s_fl_cold.cp = cp_c_avg; //[kJ/kg-K]
                v_s_node_info[i - 1].s_fl_cold.rho = hot_htf_class.dens(T_c_avg, P_c_avg); //[kg/m3]
                v_s_node_info[i - 1].s_fl_cold.k = hot_htf_class.cond(T_c_avg);   //[W/m-K]
                v_s_node_info[i - 1].s_fl_cold.mu = hot_htf_class.visc(T_c_avg)*1.E6;  //[uPa-s]
            }
            // Check for 2 phase conditions
            if (!(std::isfinite(cp_c_avg) && cp_c_avg > 0.0))
            {
                is_c_2phase = true;
            }


            double C_dot_min, C_R;
            C_dot_min = C_R = std::numeric_limits<double>::quiet_NaN();

            if (is_h_2phase && !is_c_2phase)
            {
                C_dot_min = m_dot_c * cp_c_avg;			// [kW/K] cold stream capacitance rate
                C_R = 0.0;
            }
            else if (!is_c_2phase && is_h_2phase)
            {
                C_dot_min = m_dot_h * cp_h_avg;			// [kW/K] hot stream capacitance rate
                C_R = 0.0;
            }
            else if (is_c_2phase && is_h_2phase)
            {
                C_dot_min = q_dot / (double)N_sub_hx * 1.E10 * (T_h_prev - T_c);
                C_R = 1.0;
            }
            else
            {
                double C_dot_h = m_dot_h * cp_h_avg;			// [kW/K] hot stream capacitance rate
                double C_dot_c = m_dot_c * cp_c_avg;			// [kW/K] cold stream capacitance rate
                C_dot_min = fmin(C_dot_h, C_dot_c);			// [kW/K] Minimum capacitance stream
                double C_dot_max = fmax(C_dot_h, C_dot_c);	// [kW/K] Maximum capacitance stream
                C_R = C_dot_min / C_dot_max;				// [-] Capacitance ratio of sub-heat exchanger
            }

            double eff = min(0.99999, (q_dot / (double)N_sub_hx) / (C_dot_min*(T_h_prev - T_c)));	// [-] Effectiveness of each sub-heat exchanger
            double NTU = 0.0;
            if (C_R != 1.0)
                NTU = log((1.0 - eff * C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
            else
                NTU = eff / (1.0 - eff);
            double UA_local = NTU * C_dot_min;      //[kW/K]
            UA += UA_local;					//[kW/K] Sum UAs for each hx section

            v_s_node_info[i - 1].UA = UA_local;       //[kW/K]
        }
        h_h_prev = h_h;
        T_h_prev = T_h;
        P_h_prev = P_h;
        h_c_prev = h_c;
        T_c_prev = T_c;
        P_c_prev = P_c;
    }

    if (is_temp_violation)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "Cold temperature is hotter than hot temperature.", 11));
    }

    // Check for NaNs in UA
    if (UA != UA)
    {
        throw(C_csp_exception("C_HX_counterflow::design",
            "NaN found for total heat exchanger UA", 14));
    }

    q_dot_calc = q_dot;

    // **************************************************************
    // Calculate the HX effectiveness

    double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in_q_max, T_c_in_q_max;
    h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = T_h_in_q_max = T_c_in_q_max = std::numeric_limits<double>::quiet_NaN();
    double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        h_h_in, P_h_in, P_h_out, m_dot_h,
        h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out_q_max, T_h_out_q_max,
        h_c_out_q_max, T_c_out_q_max,
        T_h_in_q_max, T_c_in_q_max);

    eff = q_dot / q_dot_max;

    bool is_h_2phase = false;
    if (fabs(T_h_in - T_h_out) < 0.001)
    {
        is_h_2phase = true;
    }
    bool is_c_2phase = false;
    if (fabs(T_c_out - T_c_in) < 0.001)
    {
        is_c_2phase = true;
    }

    double C_dot_min, C_R;
    C_dot_min = C_R = std::numeric_limits<double>::quiet_NaN();

    if (is_h_2phase && !is_c_2phase)
    {
        C_dot_min = m_dot_c * (h_c_out - h_c_in) / (T_c_out - T_c_in);			// [kW/K] cold stream capacitance rate
        C_R = 0.0;
    }
    else if (!is_c_2phase && is_h_2phase)
    {
        C_dot_min = m_dot_h * (h_h_in - h_h_out) / (T_h_in - T_h_out);			// [kW/K] hot stream capacitance rate
        C_R = 0.0;
    }
    else if (is_c_2phase && is_h_2phase)
    {
        C_dot_min = q_dot / (double)N_sub_hx * 1.E10 * (T_h_in - T_c_in);
        C_R = 1.0;
    }
    else
    {
        double C_dot_h = m_dot_h * (h_h_in - h_h_out) / (T_h_in - T_h_out);			// [kW/K] hot stream capacitance rate
        double C_dot_c = m_dot_c * (h_c_out - h_c_in) / (T_c_out - T_c_in);			// [kW/K] cold stream capacitance rate
        C_dot_min = fmin(C_dot_h, C_dot_c);						// [kW/K] Minimum capacitance stream
        double C_dot_max = fmax(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
        C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
    }

    if (C_R != 1.0)
        NTU = log((1.0 - eff * C_R) / std::max(1.E-6, (1.0 - eff))) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
    else
        NTU = eff / std::max(1.E-6, (1.0 - eff));

    if (NTU < 0.0 || !std::isfinite(NTU))
    {
        NTU = 0.0;
    }

    return;
}

double NS_HX_counterflow_eqs::UA_CRM(int hot_fl_code /*-*/, HTFProperties * hot_htf_class,
    int cold_fl_code /*-*/, HTFProperties * cold_htf_class, 
    const S_hx_node_info & s_node_info_des,
    double P_hot_in /*kPa*/, double P_hot_out /*kPa*/,
    double h_hot_in /*kPa*/, double h_hot_out /*kPa*/,
    double m_dot_hot /*kg/s*/,
    double P_cold_in /*kPa*/, double P_cold_out /*kPa*/,
    double h_cold_in /*kPa*/, double h_cold_out /*kPa*/,
    double m_dot_cold /*kg/s*/)
{
    double UA_des = s_node_info_des.UA;        // ms_des_solved.m_UA_design;  //[kW/K]
    double hA_ratio = 1.0;      //[-]
    double hA_hot_des = UA_des * (1.0 + hA_ratio);    //[kW/K]
    double hA_cold_des = hA_hot_des / hA_ratio;     //[kW/K]
    
    double P_hot_avg = 0.5*(P_hot_in + P_hot_out);  //[kPa]
    double h_hot_avg = 0.5*(h_hot_in + h_hot_out);  //[kJ/kg]

    NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_hot_avg_props(hot_fl_code, hot_htf_class, P_hot_avg, h_hot_avg, true);

    double hA_hot_scale = (c_hot_avg_props.m_k / s_node_info_des.s_fl_hot.k) *
        std::pow((m_dot_hot / c_hot_avg_props.m_mu) / (s_node_info_des.s_fl_hot.m_dot / s_node_info_des.s_fl_hot.mu), 0.8)*
        std::pow((c_hot_avg_props.m_mu*c_hot_avg_props.m_cp / c_hot_avg_props.m_k) /
        (s_node_info_des.s_fl_hot.mu*s_node_info_des.s_fl_hot.cp/ s_node_info_des.s_fl_hot.k), 0.3);
    double hA_hot = hA_hot_des * hA_hot_scale;

    double P_cold_avg = 0.5*(P_cold_in + P_cold_out);   //[kPa]
    double h_cold_avg = 0.5*(h_cold_in + h_cold_out);   //[kPa]

    NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_cold_avg_props(cold_fl_code, cold_htf_class, P_cold_avg, h_cold_avg, true);

    double hA_cold_scale = (c_cold_avg_props.m_k / s_node_info_des.s_fl_cold.k) *
        std::pow((m_dot_cold / c_cold_avg_props.m_mu) / (s_node_info_des.s_fl_cold.m_dot / s_node_info_des.s_fl_cold.mu), 0.8)*
        std::pow((c_cold_avg_props.m_mu*c_cold_avg_props.m_cp / c_cold_avg_props.m_k) /
        (s_node_info_des.s_fl_cold.mu*s_node_info_des.s_fl_cold.cp / s_node_info_des.s_fl_cold.k), 0.4);
    double hA_cold = hA_cold_des * hA_cold_scale;

    double UA = 1.0 / (1.0 / hA_hot + 1.0 / hA_cold);   //[kW/K]

    return UA;
}

int NS_HX_counterflow_eqs::C_MEQ__q_dot__target_UA__c_in_h_out__enth::operator()(double q_dot /*kWt*/, double *diff_UA_calc /*-*/)
{
    double q_dot_calc = std::numeric_limits<double>::quiet_NaN();
    mv_s_node_info_calc.resize(0);

    m_h_hot_in = m_h_hot_out + q_dot / m_m_dot_hot;     //[kJ/kg]
    m_h_cold_out = m_h_cold_in + q_dot / m_m_dot_cold;  //[kJ/kg]

    double m_dot_cold_des = ms_node_info_des.s_fl_cold.m_dot;   //[kg/s]
    double m_dot_hot_des = ms_node_info_des.s_fl_hot.m_dot;     //[kg/s]
    double UA_design = ms_node_info_des.UA;     //[kW/K]
    
    double UA_target = std::max(1.E-6, UA_design * UA_scale_vs_m_dot(m_m_dot_cold / m_dot_cold_des, m_m_dot_hot / m_dot_hot_des));

    UA_target = UA_CRM(m_hot_fl_code, &mc_hot_htf_class, m_cold_fl_code, &mc_cold_htf_class,
        ms_node_info_des, m_P_hot_in, m_P_hot_out, m_h_hot_in, m_h_hot_out, m_m_dot_hot,
        m_P_cold_in, m_P_cold_out, m_h_cold_in, m_h_cold_out, m_m_dot_cold);

    double h_hot_out, T_hot_out;
    h_hot_out = T_hot_out = std::numeric_limits<double>::quiet_NaN();

    try
    {
        NS_HX_counterflow_eqs::calc_req_UA_enth(m_hot_fl_code, mc_hot_htf_class,
            m_cold_fl_code, mc_cold_htf_class,
            m_N_sub_hx,
            q_dot, m_m_dot_cold, m_m_dot_hot,
            m_h_cold_in, m_h_hot_in, m_P_cold_in, m_P_cold_out, m_P_hot_in, m_P_hot_out,
            h_hot_out, T_hot_out, m_h_cold_out, m_T_cold_out,
            m_UA_calc, m_min_DT, m_eff, m_NTU, q_dot_calc,
            mv_s_node_info_calc);
    }
    catch (C_csp_exception &csp_except)
    {
        *diff_UA_calc = std::numeric_limits<double>::quiet_NaN();

        return -1;
    }

     *diff_UA_calc = (m_UA_calc - UA_target) / UA_target;      //[kW/K]

    return 0;
}

int NS_HX_counterflow_eqs::C_MEQ__q_dot__UA_target__enth::operator()(double q_dot /*kWt*/, double *diff_UA /*-*/)
{
	double q_dot_calc = std::numeric_limits<double>::quiet_NaN();
    mv_s_node_info.resize(0);
    
	try
	{
		NS_HX_counterflow_eqs::calc_req_UA_enth(m_hot_fl_code, mc_hot_htf_class,
			m_cold_fl_code, mc_cold_htf_class,
			m_N_sub_hx,
			q_dot, m_m_dot_c, m_m_dot_h, 
			m_h_c_in, m_h_h_in, m_P_c_in, m_P_c_out, m_P_h_in, m_P_h_out, 
			m_h_h_out, m_T_h_out, m_h_c_out, m_T_c_out,
			m_UA_calc, m_min_DT, m_eff, m_NTU, q_dot_calc,
            mv_s_node_info);
	}
	catch (C_csp_exception &csp_except)
	{
		// Reset solved OD parameters to NaN
		m_T_c_out = m_T_h_out = std::numeric_limits<double>::quiet_NaN();

		// reset 'UA_calc' to NaN
		*diff_UA = std::numeric_limits<double>::quiet_NaN();		//[kW/K]

		return -1;
	}

    if (m_UA_target_type == NS_HX_counterflow_eqs::E_calc_UA)
    {
        m_UA_target = NS_HX_counterflow_eqs::UA_CRM(m_hot_fl_code, &mc_hot_htf_class,
            m_cold_fl_code, &mc_cold_htf_class,
            *mps_node_info_des,
            m_P_h_in, m_P_h_out,
            m_h_h_in, m_h_h_out,
            m_m_dot_h,
            m_P_c_in, m_P_c_out,
            m_h_c_in, m_h_c_out,
            m_m_dot_c);
    }

    *diff_UA = (m_UA_calc - m_UA_target) / m_UA_target;     //[-]

	return 0;
}

int NS_HX_counterflow_eqs::C_MEQ__min_dT__q_dot::operator()(double q_dot /*kWt*/, double *min_dT /*C*/)
{
    double q_dot_calc = std::numeric_limits<double>::quiet_NaN();
    mv_s_node_info.resize(0);

    try
    {
        NS_HX_counterflow_eqs::calc_req_UA_enth(m_hot_fl_code, mc_hot_htf_class,
            m_cold_fl_code, mc_cold_htf_class,
            m_N_sub_hx,
            q_dot, m_m_dot_c, m_m_dot_h,
            m_h_c_in, m_h_h_in, m_P_c_in, m_P_c_out, m_P_h_in, m_P_h_out,
            m_h_h_out, m_T_h_out, m_h_c_out, m_T_c_out,
            m_UA_calc, m_min_DT, m_eff, m_NTU, q_dot_calc,
            mv_s_node_info);
    }
    catch (C_csp_exception &csp_except)
    {
        if (csp_except.m_error_code != 11)
        {
            // Reset solved OD parameters to NaN
            m_T_c_out = m_T_h_out = std::numeric_limits<double>::quiet_NaN();

            // reset 'UA_calc' to NaN
            *min_dT = std::numeric_limits<double>::quiet_NaN();		//[kW/K]

            return -1;
        }
    }

    *min_dT = m_min_DT;		//[kW/K]

    return 0;
}

void NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
    int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    int N_sub_hx /*-*/,
    double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
    double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
    double eff_target /*-*/,
    double & T_c_out  /*K*/, double & h_c_out /*kJ/kg*/,
    double & T_h_out /*K*/, double & h_h_out /*kJ/kg*/,
    double & q_dot /*kWt*/, double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    // Check for feasible target effectiveness
    if (eff_target > 1.0 || eff_target < 0.0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(...) was sent infeasible effectiveness target"));
    }
    
    // Calculate maximum possible heat transfer assuming 0 approach temperature
    double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in, T_c_in;
    h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = std::numeric_limits<double>::quiet_NaN();
    double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        h_h_in, P_h_in, P_h_out, m_dot_h,
        h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out_q_max, T_h_out_q_max,
        h_c_out_q_max, T_c_out_q_max,
        T_h_in, T_c_in);

    // What does 'calc_max_q_dot_enth' return if cold stream is hotter than hot stream?
    if (q_dot_max < 0.0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(...) was sent infeasible hx design conditions"));
    }
    else if (q_dot_max == 0.0)
    {
        T_c_out = T_c_out_q_max;    //[K]
        h_c_out = h_c_out_q_max;    //[kJ/kg]
        T_h_out = T_h_out_q_max;    //[K]
        h_h_out = h_h_out_q_max;    //[kJ/kg]
        q_dot = 0.0;        //[kWt]
        eff_calc = 0.0;     //[-]
        min_DT = T_h_out - T_c_out; //[K]
        NTU = 0.0;          //[-]
        UA_calc = 0.0;      //[kW/K]

        return;
    }

    // Apply max effectiveness value (to account for cross flow in headers and/or axial conduction) to get q_dot upper limit
    double q_dot_eff_target = eff_target * q_dot_max;     //[kWt]

    // Set up class to find q_dot that achieves dT
    NS_HX_counterflow_eqs::C_MEQ__min_dT__q_dot hx_min_dt_eq(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        N_sub_hx,
        P_c_out, P_h_out,
        h_c_in, P_c_in, m_dot_c,
        h_h_in, P_h_in, m_dot_h);
    C_monotonic_eq_solver hx_min_dt_solver(hx_min_dt_eq);
    // ***********************************************************
    
    // Find min dT at q_dot_upper
    double min_dT_eff_target = std::numeric_limits<double>::quiet_NaN(); //[K]
    int min_dT_test_code = hx_min_dt_solver.test_member_function(q_dot_eff_target, &min_dT_eff_target);
    if (min_dT_test_code != 0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(...) failed at q_dot_upper"));
    }
    // ********************************************************************************************

    // If minimum temperature at target effectiveness solution, then that's solution, so get out
    if (min_dT_eff_target > 0.0)
    {
        T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
        h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
        T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
        h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

        q_dot = q_dot_eff_target;           //[kWt]
        eff_calc = hx_min_dt_eq.m_eff;		//[-]
        min_DT = hx_min_dt_eq.m_min_DT;		//[K]
        NTU = hx_min_dt_eq.m_NTU;			//[-]
        UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

        v_s_node_info = hx_min_dt_eq.mv_s_node_info;

        return;
    }

    // Otherwise, need to iterate to find q_dot such that the minimum temperature difference is ~= 0.1
    // Find min dT at another q_dot
    double q_dot_guess = 0.95*q_dot_eff_target;      //[kWt]
    double min_dT_q_dot_guess = std::numeric_limits<double>::quiet_NaN();   //[K]
    min_dT_test_code = hx_min_dt_solver.test_member_function(q_dot_guess, &min_dT_q_dot_guess);
    if (min_dT_test_code != 0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(...) failed at q_dot_guess"));
    }
    // ********************************************************************************************

    // Complete solver settings
    double tol = 0.1;           //[K] convergence tolerance
    double min_dT_target = tol; //[K] 0 + tol so worst case convergence is still > 0 
    double q_dot_lower = 1.E-10;	//[kWt]

    // If min dT is within tolerance of tareget, then get out
    if (fabs(min_dT_q_dot_guess - min_dT_target) < tol)
    {
        T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
        h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
        T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
        h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

        q_dot = q_dot_guess;                //[kWt]
        eff_calc = hx_min_dt_eq.m_eff;		//[-]
        min_DT = hx_min_dt_eq.m_min_DT;		//[K]
        NTU = hx_min_dt_eq.m_NTU;			//[-]
        UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

        v_s_node_info = hx_min_dt_eq.mv_s_node_info;

        return;
    }

    // Set solver settings - don't normalize error
    hx_min_dt_solver.settings(tol, 1000, q_dot_lower, q_dot_eff_target, false);

    // Set up solver to find q_dot that results in target min dT
    C_monotonic_eq_solver::S_xy_pair xy1;
    xy1.x = q_dot_eff_target;        //[kWt]
    xy1.y = min_dT_eff_target;   //[K]

    C_monotonic_eq_solver::S_xy_pair xy2;
    xy2.x = q_dot_guess;        //[kWt]
    xy2.y = min_dT_q_dot_guess; //[K]

    // Solve
    double tol_solved, q_dot_solved;
    q_dot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
    int iter_solved = -1;

    int hx_min_dT_solver_code = hx_min_dt_solver.solve(xy1, xy2, min_dT_target,
        q_dot_solved, tol_solved, iter_solved);

    if (hx_min_dT_solver_code != C_monotonic_eq_solver::CONVERGED)
    {
        if (!(hx_min_dT_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 1.0))
        {
            throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(...) failed to converge"));
        }
    }

    T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
    h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
    T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
    h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

    q_dot = q_dot_solved;               //[kWt]
    eff_calc = hx_min_dt_eq.m_eff;		//[-]
    min_DT = hx_min_dt_eq.m_min_DT;		//[K]
    NTU = hx_min_dt_eq.m_NTU;			//[-]
    UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

    v_s_node_info = hx_min_dt_eq.mv_s_node_info;

    return;
}


void NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
    int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    int N_sub_hx /*-*/,
    double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
    double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
    double min_dT_target /*C*/, double eff_limit /*-*/,
    double & T_c_out  /*K*/, double & h_c_out /*kJ/kg*/,
    double & T_h_out /*K*/, double & h_h_out /*kJ/kg*/,
    double & q_dot /*kWt*/, double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc /*kW/K*/,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    // Calculate maximum possible heat transfer assuming 0 approach temperature
    double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in, T_c_in;
    h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = std::numeric_limits<double>::quiet_NaN();
    double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        h_h_in, P_h_in, P_h_out, m_dot_h,
        h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out_q_max, T_h_out_q_max,
        h_c_out_q_max, T_c_out_q_max,
        T_h_in, T_c_in);

    // What does 'calc_max_q_dot_enth' return if cold stream is hotter than hot stream?
    if (q_dot_max < 0.0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(...) was sent infeasible hx design conditions"));
    }
    else if (q_dot_max == 0.0)
    {
        T_c_out = T_c_out_q_max;    //[K]
        h_c_out = h_c_out_q_max;    //[kJ/kg]
        T_h_out = T_h_out_q_max;    //[K]
        h_h_out = h_h_out_q_max;    //[kJ/kg]
        q_dot = 0.0;        //[kWt]
        eff_calc = 0.0;     //[-]
        min_DT = T_h_out - T_c_out; //[K]
        NTU = 0.0;          //[-]
        UA_calc = 0.0;      //[kW/K]

        return;
    }

    // Is the min dT possible given the inlet conditions?
    if ((T_h_in - T_c_in) < min_dT_target)
    {
        T_c_out = T_c_in;    //[K]
        h_c_out = h_c_in;    //[kJ/kg]
        T_h_out = T_h_in;    //[K]
        h_h_out = h_h_in;    //[kJ/kg]
        q_dot = 0.0;        //[kWt]
        eff_calc = 0.0;     //[-]
        min_DT = T_h_out - T_c_out; //[K]
        NTU = 0.0;          //[-]
        UA_calc = 0.0;      //[kW/K]

        return;
    }

    // Apply max effectiveness value (to account for cross flow in headers and/or axial conduction) to get q_dot upper limit
    double q_dot_upper = eff_limit * q_dot_max;     //[kWt]

    // Set up class to find q_dot that achieves dT
    NS_HX_counterflow_eqs::C_MEQ__min_dT__q_dot hx_min_dt_eq(hot_fl_code, hot_htf_class,
        cold_fl_code, cold_htf_class,
        N_sub_hx,
        P_c_out, P_h_out,
        h_c_in, P_c_in, m_dot_c,
        h_h_in, P_h_in, m_dot_h);
    C_monotonic_eq_solver hx_min_dt_solver(hx_min_dt_eq);
    // ***********************************************************

    // Complete solver settings
    double tol = 0.1;       //[K] convergence tolerance
    double q_dot_lower = 1.E-10;	//[kWt]

    // Set solver settings - don't normalize error
    hx_min_dt_solver.settings(tol, 1000, q_dot_lower, q_dot_upper, false);

    // Find min dT at q_dot_upper
    double min_dT_eff_ideal = std::numeric_limits<double>::quiet_NaN(); //[K]
    int min_dT_test_code = hx_min_dt_solver.test_member_function(q_dot_upper, &min_dT_eff_ideal);
    if (min_dT_test_code != 0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(...) failed at q_dot_upper"));
    }
    // ********************************************************************************************

    // If min dT is within tolerance of target, then get out
    // Also, the max q dot will have the smallest dT, so if smallest dT larger than target then also get out
    if (fabs(min_dT_eff_ideal - min_dT_target) < tol || min_dT_eff_ideal - min_dT_target > tol)
    {
        T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
        h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
        T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
        h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

        q_dot = q_dot_upper;                //[kWt]
        eff_calc = hx_min_dt_eq.m_eff;		//[-]
        min_DT = hx_min_dt_eq.m_min_DT;		//[K]
        NTU = hx_min_dt_eq.m_NTU;			//[-]
        UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

        v_s_node_info = hx_min_dt_eq.mv_s_node_info;

        return;
    }

    // Find min dT at another q_dot
    double q_dot_guess = 0.95*q_dot_upper;      //[kWt]
    double min_dT_q_dot_guess = std::numeric_limits<double>::quiet_NaN();   //[K]
    min_dT_test_code = hx_min_dt_solver.test_member_function(q_dot_guess, &min_dT_q_dot_guess);
    if (min_dT_test_code != 0)
    {
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(...) failed at q_dot_guess"));
    }
    // ********************************************************************************************

    // If min dT is within tolerance of tareget, then get out
    if (fabs(min_dT_q_dot_guess - min_dT_target) < tol)
    {
        T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
        h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
        T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
        h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

        q_dot = q_dot_guess;                //[kWt]
        eff_calc = hx_min_dt_eq.m_eff;		//[-]
        min_DT = hx_min_dt_eq.m_min_DT;		//[K]
        NTU = hx_min_dt_eq.m_NTU;			//[-]
        UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

        v_s_node_info = hx_min_dt_eq.mv_s_node_info;

        return;
    }

    // Set up solver to find q_dot that results in target min dT
    C_monotonic_eq_solver::S_xy_pair xy1;
    xy1.x = q_dot_upper;        //[kWt]
    xy1.y = min_dT_eff_ideal;   //[K]

    C_monotonic_eq_solver::S_xy_pair xy2;
    xy2.x = q_dot_guess;        //[kWt]
    xy2.y = min_dT_q_dot_guess; //[K]

    // Solve
    double tol_solved, q_dot_solved;
    q_dot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
    int iter_solved = -1;

    int hx_min_dT_solver_code = hx_min_dt_solver.solve(xy1, xy2, min_dT_target,
        q_dot_solved, tol_solved, iter_solved);

    if (hx_min_dT_solver_code != C_monotonic_eq_solver::CONVERGED)
    {
        if (!(hx_min_dT_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 1.0))
        {
            throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(...) failed to converge"));
        }
    }

    T_c_out = hx_min_dt_eq.m_T_c_out;	//[K]
    h_c_out = hx_min_dt_eq.m_h_c_out;	//[kJ/kg]
    T_h_out = hx_min_dt_eq.m_T_h_out;	//[K]
    h_h_out = hx_min_dt_eq.m_h_h_out;	//[kJ/kg]

    q_dot = q_dot_solved;               //[kWt]
    eff_calc = hx_min_dt_eq.m_eff;		//[-]
    min_DT = hx_min_dt_eq.m_min_DT;		//[K]
    NTU = hx_min_dt_eq.m_NTU;			//[-]
    UA_calc = hx_min_dt_eq.m_UA_calc;   //[kW/K]

    v_s_node_info = hx_min_dt_eq.mv_s_node_info;
}

void NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    S_hx_node_info & s_node_info_des,
	int N_sub_hx /*-*/, NS_HX_counterflow_eqs::E_UA_target_type UA_target_type,
	double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
    double tol /*-*/,
	double & T_c_out  /*K*/, double & h_c_out /*kJ/kg*/,
	double & T_h_out /*K*/, double & h_h_out /*kJ/kg*/,
	double & q_dot /*kWt*/, double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    v_s_node_info.resize(0);

    if (UA_target < 1.E-10)
	{
		q_dot = 0.0;

		double q_dot_calc = std::numeric_limits<double>::quiet_NaN();

		try
		{
			NS_HX_counterflow_eqs::calc_req_UA_enth(hot_fl_code, hot_htf_class,
				cold_fl_code, cold_htf_class,
				N_sub_hx,
				q_dot, m_dot_c, m_dot_h,
				h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
				h_h_out, T_h_out, h_c_out, T_c_out,
				UA_calc, min_DT, eff_calc, NTU, q_dot_calc,
                v_s_node_info);
		}
		catch (C_csp_exception &)
		{
			throw(C_csp_exception("C_HX_counterflow::hx_solution(...) failed with UA_target < 1.E-10"));
		}

		q_dot = q_dot_calc;			//[kWt]

		return;
	}

    double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in_q_max, T_c_in_q_max;
    h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = std::numeric_limits<double>::quiet_NaN();
	double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		h_h_in, P_h_in, P_h_out, m_dot_h,
		h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out_q_max, T_h_out_q_max,
        h_c_out_q_max, T_c_out_q_max,
        T_h_in_q_max, T_c_in_q_max);

	double q_dot_upper = eff_limit*q_dot_max;

	// Use design point effectiveness to generate 2 guess values
	double q_dot_mult = max(0.99, min(0.95, eff_limit) / eff_limit);
	if ( std::isfinite(eff_guess) )
	{
		q_dot_mult = max(0.99, min(0.1, eff_guess));
	}

	double q_dot_guess_upper = q_dot_mult*q_dot_upper;
	double q_dot_guess_lower = 0.85*q_dot_guess_upper;

	// Complete solver settings
	double q_dot_lower = 1.E-10;		//[kWt]

	NS_HX_counterflow_eqs::C_MEQ__q_dot__UA_target__enth od_hx_eq(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
        &s_node_info_des,
		N_sub_hx,
        UA_target_type, UA_target,
		P_c_out, P_h_out,
		h_c_in, P_c_in, m_dot_c,
		h_h_in, P_h_in, m_dot_h);
	C_monotonic_eq_solver od_hx_solver(od_hx_eq);

	// First, test at q_dot_upper
	double diff_UA_max_eff = std::numeric_limits<double>::quiet_NaN();
	int test_code = od_hx_solver.test_member_function(q_dot_upper, &diff_UA_max_eff);

	double q_dot_solved = std::numeric_limits<double>::quiet_NaN();
	if (test_code != 0 || diff_UA_max_eff > 0.0) // UA_max_eff > UA_target)
	{
		// Set solver settings
		od_hx_solver.settings(tol, 1000, q_dot_lower, q_dot_upper, false);

		// Solve
		double tol_solved;
		q_dot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;

		int od_hx_code = od_hx_solver.solve(q_dot_guess_lower, q_dot_guess_upper, 0.0,
			q_dot_solved, tol_solved, iter_solved);

		// UA vs. q_dot is very nonlinear, with very large increases of UA as q_dot approaches q_dot_max
		// As such, may not reach convergence on UA while the uncertainty on q_dot is very small, which should be ok
		if (od_hx_code < C_monotonic_eq_solver::CONVERGED || 
			(fabs(tol_solved) > 0.1 && 
			!(od_hx_code == C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_BOTH_ERRS)) )
		{
			throw(C_csp_exception("Off-design heat exchanger method failed"));
		}

		//if (!(od_hx_code == C_monotonic_eq_solver::CONVERGED || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_BOTH_ERRS))
		//{
		//	throw(C_csp_exception("Off-design heat exchanger method failed"));
		//}
	}
	else if (test_code == 0 && diff_UA_max_eff <= 0.0)  // UA_max_eff <= UA_target)
	{
		// At maximum allowable heat transfer, the calculated UA is less than target
		q_dot_solved = q_dot_upper;
	}

	T_c_out = od_hx_eq.m_T_c_out;	//[K]
	h_c_out = od_hx_eq.m_h_c_out;	//[kJ/kg]
	T_h_out = od_hx_eq.m_T_h_out;	//[K]
	h_h_out = od_hx_eq.m_h_h_out;	//[kJ/kg]

	q_dot = q_dot_solved;
	eff_calc = od_hx_eq.m_eff;		//[-]
	min_DT = od_hx_eq.m_min_DT;		//[K]
	NTU = od_hx_eq.m_NTU;			//[-]
	UA_calc = od_hx_eq.m_UA_calc;

    v_s_node_info = od_hx_eq.mv_s_node_info;
}

void NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(int hx_target_code /*-*/, 
    int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
    S_hx_node_info & s_node_info_des,
	int N_sub_hx /*-*/, NS_HX_counterflow_eqs::E_UA_target_type UA_target_type,
	double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double UA_target /*kW/K*/, double min_dT_target /*K*/, double eff_target /*-*/,
    double eff_limit /*-*/, double eff_guess /*-*/,
    double tol /*-*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/,
    double & h_c_in /*kJ/kg*/, double & h_c_out /*kJ/kg*/,
    double & h_h_in /*kJ/kg*/, double & h_h_out /*kJ/kg*/,
	double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc,
    std::vector<S_hx_node_info> & v_s_node_info)
{
    // Calculate inlet enthalpies from known state points
    h_c_in = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(cold_fl_code, cold_htf_class, T_c_in, P_c_in);
    h_h_in = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(hot_fl_code, hot_htf_class, T_h_in, P_h_in);

    // Need to check if hot stream is actually hotter than the cold stream
	// If not, just return the input temperatures for each stream
	// Should maybe improve code to handle this case (i.e. check which input is hotter)...
	if (T_h_in - T_c_in < 0.01  ||
        (hx_target_code == NS_HX_counterflow_eqs::TARGET_UA && (UA_target <= 0.0 || !std::isfinite(UA_target) ) ) ||
        (hx_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && !std::isfinite(min_dT_target) ) ||
        (hx_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && (eff_target <= 0.0 || !std::isfinite(eff_target) ) ) )
	{
		q_dot = 0.0;
		T_c_out = T_c_in;
		T_h_out = T_h_in;
        h_c_out = h_c_in;
        h_h_out = h_h_in;
		eff_calc = 0.0;
		min_DT = std::abs(T_h_in - T_c_in);
		NTU = 0.0;
		UA_calc = UA_target;
		return;
	}

	h_c_out = std::numeric_limits<double>::quiet_NaN();
	h_h_out = std::numeric_limits<double>::quiet_NaN();

    switch (hx_target_code)
    {
    case NS_HX_counterflow_eqs::TARGET_UA:
    case NS_HX_counterflow_eqs::OPTIMIZE_UA:
        
        NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA_enth(hot_fl_code, hot_htf_class,
            cold_fl_code, cold_htf_class,
            s_node_info_des,
            N_sub_hx, UA_target_type,
            h_c_in, P_c_in, m_dot_c, P_c_out,
            h_h_in, P_h_in, m_dot_h, P_h_out,
            UA_target, eff_limit, eff_guess,
            tol,
            T_c_out, h_c_out,
            T_h_out, h_h_out,
            q_dot, eff_calc, min_DT, NTU, UA_calc,
            v_s_node_info);

        break;

    case NS_HX_counterflow_eqs::TARGET_MIN_DT:

        NS_HX_counterflow_eqs::solve_q_dot__fixed_min_dT__enth(hot_fl_code, hot_htf_class,
            cold_fl_code, cold_htf_class,
            N_sub_hx,
            h_c_in, P_c_in, m_dot_c, P_c_out,
            h_h_in, P_h_in, m_dot_h, P_h_out,
            min_dT_target, eff_limit,
            T_c_out, h_c_out,
            T_h_out, h_h_out,
            q_dot, eff_calc, min_DT, NTU, UA_calc,
            v_s_node_info);

        break;

    case NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS:

        NS_HX_counterflow_eqs::solve_q_dot__fixed_eff__enth(hot_fl_code, hot_htf_class,
            cold_fl_code, cold_htf_class,
            N_sub_hx,
            h_c_in, P_c_in, m_dot_c, P_c_out,
            h_h_in, P_h_in, m_dot_h, P_h_out,
            eff_target,
            T_c_out, h_c_out,
            T_h_out, h_h_out,
            q_dot, eff_calc, min_DT, NTU, UA_calc,
            v_s_node_info);

        break;

    default:
        throw(C_csp_exception("NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA",
            "hx target code not recognized", 10));
    }

	return;
}

C_HX_counterflow_CRM::C_HX_counterflow_CRM()
{
    m_is_HX_initialized = false;
    m_is_HX_designed = false;

    m_is_single_node_des_set = false;

    m_cost_model = C_od_thermal_solution_type::E_DEFAULT;
    m_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
}

void C_HX_counterflow_CRM::initialize(const S_init_par & init_par_in)
{
    // Set member structure
    ms_init_par = init_par_in;

    // Set up HTFProperties for the hot fluid
    if (ms_init_par.m_hot_fl != NS_HX_counterflow_eqs::CO2 && ms_init_par.m_hot_fl != NS_HX_counterflow_eqs::WATER)
    {
        if (ms_init_par.m_hot_fl != HTFProperties::User_defined && ms_init_par.m_hot_fl < HTFProperties::End_Library_Fluids)
        {
            if (!mc_hot_fl.SetFluid(ms_init_par.m_hot_fl, true))
            {
                throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
            }
        }
        else if (ms_init_par.m_hot_fl == HTFProperties::User_defined)
        {
            int n_rows = (int)ms_init_par.mc_hot_fl_props.nrows();
            int n_cols = (int)ms_init_par.mc_hot_fl_props.ncols();
            if (n_rows > 2 && n_cols == 7)
            {
                if (!mc_hot_fl.SetUserDefinedFluid(ms_init_par.mc_hot_fl_props, true))
                {
                    std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
                    throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
                }
            }
            else
            {
                std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
            }
        }
        else
        {
            throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
        }
    }

    // Set up HTFProperties for the cold fluid
    if (ms_init_par.m_cold_fl != NS_HX_counterflow_eqs::CO2 && ms_init_par.m_cold_fl != NS_HX_counterflow_eqs::WATER)
    {
        if (ms_init_par.m_cold_fl != HTFProperties::User_defined && ms_init_par.m_cold_fl < HTFProperties::End_Library_Fluids)
        {
            if (!mc_cold_fl.SetFluid(ms_init_par.m_cold_fl, true))
            {
                throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
            }
        }
        else if (ms_init_par.m_cold_fl == HTFProperties::User_defined)
        {
            int n_rows = (int)ms_init_par.mc_cold_fl_props.nrows();
            int n_cols = (int)ms_init_par.mc_hot_fl_props.ncols();
            if (n_rows > 2 && n_cols == 7)
            {
                if (!mc_cold_fl.SetUserDefinedFluid(ms_init_par.mc_cold_fl_props, true))
                {
                    std::string error_msg = util::format(mc_cold_fl.UserFluidErrMessage(), n_rows, n_cols);
                    throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
                }
            }
            else
            {
                std::string error_msg = util::format("The user defined cold fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
            }
        }
        else
        {
            throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
        }
    }

    // Class is initialized
    m_is_HX_initialized = true;

    return;
}

void C_HX_counterflow_CRM::calc_req_UA(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
    double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
    double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/,
    std::vector<NS_HX_counterflow_eqs::S_hx_node_info> & v_s_node_info)
{
    NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl,
        ms_init_par.m_cold_fl, mc_cold_fl,
        ms_init_par.m_N_sub_hx,
        q_dot, m_dot_c, m_dot_h,
        T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
        UA, min_DT, eff, NTU, T_h_out, T_c_out, q_dot_calc,
        v_s_node_info);
}

void C_HX_counterflow_CRM::calc_req_UA_enth(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
    double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
    double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & h_h_out /*K*/, double & h_c_out /*K*/, double & q_dot_calc /*kWt*/,
    std::vector<NS_HX_counterflow_eqs::S_hx_node_info> & v_s_node_info)
{
    double T_h_out = std::numeric_limits<double>::quiet_NaN();
    double T_c_out = std::numeric_limits<double>::quiet_NaN();
    NS_HX_counterflow_eqs::calc_req_UA_enth(ms_init_par.m_hot_fl, mc_hot_fl,
        ms_init_par.m_cold_fl, mc_cold_fl,
        ms_init_par.m_N_sub_hx,
        q_dot, m_dot_c, m_dot_h,
        h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
        h_h_out, T_h_out, h_c_out, T_c_out,
        UA, min_DT, eff, NTU, q_dot_calc,
        v_s_node_info);
}

double C_HX_counterflow_CRM::calc_max_q_dot_enth(double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
    double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/)
{
    double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in_q_max, T_c_in_q_max;
    h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = std::numeric_limits<double>::quiet_NaN();
    return NS_HX_counterflow_eqs::calc_max_q_dot_enth(ms_init_par.m_hot_fl, mc_hot_fl,
        ms_init_par.m_cold_fl, mc_cold_fl,
        h_h_in, P_h_in, P_h_out, m_dot_h,
        h_c_in, P_c_in, P_c_out, m_dot_c,
        h_h_out_q_max, T_h_out_q_max,
        h_c_out_q_max, T_c_out_q_max,
        T_h_in_q_max, T_c_in_q_max);
}

double C_HX_counterflow_CRM::calculate_cost(double UA /*kWt/K*/,
    double T_hot_in /*K*/, double P_hot_in /*kPa*/, double m_dot_hot /*kg/s*/,
    double T_cold_in /*K*/, double P_cold_in /*kPa*/, double m_dot_cold /*kg/s*/)
{
    switch (m_cost_model)
    {
    case C_HX_counterflow_CRM::E_CARLSON_17_RECUP:
        return 1.25*1.E-3*UA;		//[M$] needs UA in kWt/K
    case C_HX_counterflow_CRM::E_CARLSON_17_PHX:
        return 3.5*1.E-3*UA;		//[M$] needs UA in kWt/K
    default:
        return std::numeric_limits<double>::quiet_NaN();
    }
}

void C_HX_counterflow_CRM::design_calc_UA(C_HX_counterflow_CRM::S_des_calc_UA_par des_par,
	double q_dot_design /*kWt*/, C_HX_counterflow_CRM::S_des_solved &des_solved)
{
    /*Designs heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
	Note: the heat transfer rate must be positive.*/
	ms_des_calc_UA_par = des_par;
	ms_des_solved.m_DP_cold_des = des_par.m_P_c_in - des_par.m_P_c_out;		//[kPa]
	ms_des_solved.m_DP_hot_des = des_par.m_P_h_in - des_par.m_P_h_out;		//[kPa]

	// Trying to solve design point, so set boolean to false until method solves successfully
	m_is_HX_designed = false;

	// Check that design parameters are set
	if( !m_is_HX_initialized )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Design parameters are not initialized!"));
	}

	double UA_calc, min_DT_calc, eff_calc, NTU_calc, T_h_out_calc, T_c_out_calc, q_dot_calc;
	UA_calc = min_DT_calc = eff_calc = NTU_calc = T_h_out_calc = T_c_out_calc = q_dot_calc = std::numeric_limits<double>::quiet_NaN();
	
	NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		q_dot_design, ms_des_calc_UA_par.m_m_dot_cold_des, ms_des_calc_UA_par.m_m_dot_hot_des,
		ms_des_calc_UA_par.m_T_c_in, ms_des_calc_UA_par.m_T_h_in, ms_des_calc_UA_par.m_P_c_in, ms_des_calc_UA_par.m_P_c_out, ms_des_calc_UA_par.m_P_h_in, ms_des_calc_UA_par.m_P_h_out,
		UA_calc, min_DT_calc, eff_calc, NTU_calc, T_h_out_calc, T_c_out_calc, q_dot_calc,
        mv_s_node_info_des);

	// Check that calculated effectiveness is less than limit
	if (eff_calc > ms_des_calc_UA_par.m_eff_max)
	{
		std::string msg = util::format("Calculated design effectiveness, %lg [-] is greater than the specified maximum effectiveness, %lg [-].", eff_calc, ms_des_calc_UA_par.m_eff_max);

		throw(C_csp_exception("C_HX_counterflow::design",
			"Calculated design effectiveness, %lg [-] is greater than the specified maximum effectiveness, %lg [-]."));
	}

    ms_des_solved.m_UA_allocated = 0.0;     //[kW/K]
    ms_des_solved.m_UA_calc_at_eff_max = UA_calc;   //[kW/K]

	ms_des_solved.m_Q_dot_design = q_dot_design;	//[kWt]
	ms_des_solved.m_UA_design = UA_calc;		    //[kWt/K]
	ms_des_solved.m_min_DT_design = min_DT_calc;
	ms_des_solved.m_eff_design = eff_calc;
	ms_des_solved.m_NTU_design = NTU_calc;
	ms_des_solved.m_T_h_out = T_h_out_calc;
	ms_des_solved.m_T_c_out = T_c_out_calc;

	ms_des_solved.m_cost = calculate_cost(ms_des_solved.m_UA_design,
		ms_des_calc_UA_par.m_T_h_in, ms_des_calc_UA_par.m_P_h_in, ms_des_calc_UA_par.m_m_dot_hot_des,
		ms_des_calc_UA_par.m_T_c_in, ms_des_calc_UA_par.m_P_c_in, ms_des_calc_UA_par.m_m_dot_cold_des);

	// Specify that method solved successfully
	m_is_HX_designed = true;

	des_solved = ms_des_solved;

	return;
}

void C_HX_co2_to_htf::design_and_calc_m_dot_htf(C_HX_counterflow_CRM::S_des_calc_UA_par &des_par, 
			double q_dot_design /*kWt*/, double dt_cold_approach /*C/K*/, C_HX_counterflow_CRM::S_des_solved &des_solved)
{
	double T_htf_cold = des_par.m_T_c_in + dt_cold_approach;	//[C]

	double h_h_in = mc_hot_fl.enth_lookup(des_par.m_T_h_in);	//[kJ/kg]
	double h_c_in = mc_hot_fl.enth_lookup(T_htf_cold);			//[kJ/kg]

	des_par.m_m_dot_hot_des = q_dot_design/(h_h_in - h_c_in);

	design_calc_UA(des_par, q_dot_design, des_solved);
}

void C_HX_counterflow_CRM::design_for_target__calc_outlet(int hx_target_code /*-*/,
    double UA_target /*kW/K*/, double min_dT_target /*K*/, double eff_target /*-*/,
    double eff_max /*-*/, double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
    double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
    double tol /*-*/,
    double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)
{
    ms_des_solved.m_UA_allocated = ms_des_solved.m_UA_calc_at_eff_max =
        ms_des_solved.m_Q_dot_design = ms_des_solved.m_UA_design = ms_des_solved.m_min_DT_design = ms_des_solved.m_eff_design = ms_des_solved.m_NTU_design =
        ms_des_solved.m_T_h_out = ms_des_solved.m_T_c_out = ms_des_solved.m_DP_cold_des = ms_des_solved.m_DP_hot_des = std::numeric_limits<double>::quiet_NaN();

    double h_c_in, h_c_out, h_h_in, h_h_out, eff_calc, min_DT, NTU, UA_calc;
    h_c_in = h_c_out = h_h_in = h_h_out = eff_calc = min_DT = NTU = UA_calc = std::numeric_limits<double>::quiet_NaN();

    NS_HX_counterflow_eqs::S_hx_node_info s_node_info_des_pass_thru;
    NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(hx_target_code,
        ms_init_par.m_hot_fl, mc_hot_fl,
        ms_init_par.m_cold_fl, mc_cold_fl,
        s_node_info_des_pass_thru,
        ms_init_par.m_N_sub_hx, NS_HX_counterflow_eqs::E_constant_UA,
        T_c_in, P_c_in, m_dot_c, P_c_out,
        T_h_in, P_h_in, m_dot_h, P_h_out,
        UA_target, min_dT_target, eff_target,
        eff_max, ms_des_solved.m_eff_design,
        tol,
        q_dot, T_c_out, T_h_out,
        h_c_in, h_c_out,
        h_h_in, h_h_out,
        eff_calc, min_DT, NTU, UA_calc,
        mv_s_node_info_des);

    ms_des_calc_UA_par.m_T_h_in = T_h_in;			//[K]
    ms_des_calc_UA_par.m_P_h_in = P_h_in;			//[kPa]
    ms_des_calc_UA_par.m_P_h_out = P_h_out;			//[kPa]
    ms_des_calc_UA_par.m_m_dot_hot_des = m_dot_h;	//[kg/s]
    ms_des_calc_UA_par.m_T_c_in = T_c_in;			//[K]
    ms_des_calc_UA_par.m_P_c_in = P_c_in;			//[kPa]
    ms_des_calc_UA_par.m_P_c_out = P_c_out;			//[kPa]
    ms_des_calc_UA_par.m_m_dot_cold_des = m_dot_c;	//[kg/s]
    ms_des_calc_UA_par.m_eff_max = eff_max;		    //[-]

    ms_des_solved.m_Q_dot_design = q_dot;			//[kWt]
    ms_des_solved.m_UA_calc_at_eff_max = UA_calc;	//[kW/K]

    if (hx_target_code == NS_HX_counterflow_eqs::TARGET_UA || hx_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA)
    {
        ms_des_solved.m_UA_allocated = UA_target;	//[kW/K] assigned UA - not necessarily = calculated
        ms_des_solved.m_UA_design = UA_target;      //[kW/K] designing with "extra" UA - will use this value to scale at off-design so could still be useful
    }
    else
    {
        ms_des_solved.m_UA_allocated = 0.0;
        ms_des_solved.m_UA_design = UA_calc;        //[kW/K] design with calculated UA
    }

    ms_des_solved.m_min_DT_design = min_DT;			//[K]
    ms_des_solved.m_eff_design = eff_calc;			//[-]
    ms_des_solved.m_NTU_design = NTU;				//[-]
    ms_des_solved.m_T_h_out = T_h_out;				//[K]
    ms_des_solved.m_T_c_out = T_c_out;				//[K]
    ms_des_solved.m_DP_cold_des = P_c_in - P_c_out;		//[kPa]
    ms_des_solved.m_DP_hot_des = P_h_in - P_h_out;		//[kPa]

    ms_des_solved.m_cost = calculate_cost(ms_des_solved.m_UA_design,
        T_h_in, P_h_in, m_dot_h,
        T_c_in, P_c_in, m_dot_c);
}

void C_HX_counterflow_CRM::C_MEQ__hx_total_q_dot::init_calc_member_vars()
{
    m_h_hot_out = m_h_cold_out = m_UA_tot_calc = m_min_dT = std::numeric_limits<double>::quiet_NaN();
}

int C_HX_counterflow_CRM::C_MEQ__hx_total_q_dot::operator()(double q_dot_hx /*kWt*/, double *diff_q_dot /*-*/)
{
    init_calc_member_vars();
    mpc_hx->mv_s_node_info_od.resize(mpc_hx->ms_init_par.m_N_sub_hx);

    // Calculate total HX hot outlet from q_dot_hx guess
    m_h_hot_out = m_h_hot_in - q_dot_hx / m_m_dot_hot;     //[kJ/kg]
    m_h_cold_out = m_h_cold_in + q_dot_hx / m_m_dot_cold;  //[kJ/kg]

    int N_nodes = mpc_hx->ms_init_par.m_N_sub_hx + 1;

    double h_hot_in_i = std::numeric_limits<double>::quiet_NaN();
    double P_hot_in_i = std::numeric_limits<double>::quiet_NaN();
    double h_cold_out_i = std::numeric_limits<double>::quiet_NaN();
    double P_cold_out_i = std::numeric_limits<double>::quiet_NaN();

    double P_cold_in_i = std::numeric_limits<double>::quiet_NaN();
    double P_hot_out_i = std::numeric_limits<double>::quiet_NaN();
    double h_cold_in_i = std::numeric_limits<double>::quiet_NaN();
    double h_hot_out_i = std::numeric_limits<double>::quiet_NaN();

    double UA_calc_i, min_DT_i, eff_calc_i, NTU_i;
    UA_calc_i = min_DT_i = eff_calc_i = NTU_i = std::numeric_limits<double>::quiet_NaN();

    m_UA_tot_calc = 0.0;
    m_min_dT = 1.E6;

    std::vector<NS_HX_counterflow_eqs::S_hx_node_info> v_s_node_info_i;
    v_s_node_info_i.resize(1);

    double q_dot_subhx_uniform = q_dot_hx / (double)mpc_hx->ms_init_par.m_N_sub_hx;

    // Know cold in and hot out
    // Want to work from cold end of hx to hot end, so need to reverse index convention in calc_req_UA_enth
    for (int i = N_nodes - 1; i > -1; i--)
    {
        if (i == N_nodes - 1)
        {
            h_hot_in_i = m_h_hot_out;
            P_hot_in_i = m_P_hot_out;
            h_cold_out_i = m_h_cold_in;
            P_cold_out_i = m_P_cold_in;
        }
        else
        {
            // Solve pressure here, for now
            P_hot_in_i = m_P_hot_in - i * (m_P_hot_in - m_P_hot_out) / (double)(N_nodes - 1);
            P_cold_out_i = m_P_cold_out + i * (m_P_cold_in - m_P_cold_out) / (double)(N_nodes - 1);

            // Solve for hot in and cold out
            if (mpc_hx->mv_s_node_info_des[i].UA < 1.E-4)
            {
                // Not "enough" conductance, so assume no heat transfer in this node
                double q_dot_guess_i = 0.0;
                double q_dot_calc_i = std::numeric_limits<double>::quiet_NaN();
                
                // Set hot side inlet to outlet, because know heat transfer = 0
                h_hot_in_i = h_hot_out_i;   //[kJ/kg]

                double h_hot_out_calc, T_h_out_calc, T_c_out_calc;
                h_hot_out_calc = T_h_out_calc = T_c_out_calc = std::numeric_limits<double>::quiet_NaN();
                try
                {
                    NS_HX_counterflow_eqs::calc_req_UA_enth(mpc_hx->ms_init_par.m_hot_fl, mpc_hx->mc_hot_fl,
                        mpc_hx->ms_init_par.m_cold_fl, mpc_hx->mc_cold_fl,
                        N_sub_hx_i,
                        q_dot_guess_i, m_m_dot_cold, m_m_dot_hot,
                        h_cold_in_i, h_hot_in_i, P_cold_in_i, P_cold_out_i, P_hot_in_i, P_hot_out_i,
                        /*calculated*/
                        h_hot_out_calc, T_h_out_calc, h_cold_out_i, T_c_out_calc,
                        UA_calc_i, min_DT_i, eff_calc_i, NTU_i, q_dot_calc_i,
                        v_s_node_info_i);
                }
                catch (C_csp_exception &)
                {
                    throw(C_csp_exception("C_HX_counterflow::hx_solution(...) failed with UA_target < 1.E-10"));
                }

                q_dot_guess_i = q_dot_calc_i;			//[kWt]
            }
            else
            {
                // Solve for heat transfer that satifies HX conductance calculation

                // Complete solver settings
                double q_dot_lower = 1.E-10;		//[kWt]

                NS_HX_counterflow_eqs::C_MEQ__q_dot__target_UA__c_in_h_out__enth od_hx_eq(mpc_hx->ms_init_par.m_hot_fl, mpc_hx->mc_hot_fl,
                    mpc_hx->ms_init_par.m_cold_fl, mpc_hx->mc_cold_fl,
                    mpc_hx->mv_s_node_info_des[i],
                    P_cold_out_i, P_hot_out_i,
                    h_cold_in_i, P_cold_in_i, m_m_dot_cold,
                    h_hot_out_i, P_hot_in_i, m_m_dot_hot);

                C_monotonic_eq_solver od_hx_solver(od_hx_eq);

                od_hx_solver.settings(m_tol, 50, q_dot_lower, std::numeric_limits<double>::quiet_NaN(), false);

                C_monotonic_eq_solver::S_xy_pair xy_q_1;
                xy_q_1.x = q_dot_subhx_uniform*0.1;

                int UA_err_1 = od_hx_solver.test_member_function(xy_q_1.x, &xy_q_1.y);

                C_monotonic_eq_solver::S_xy_pair xy_q_2;
                
                int UA_err_2 = UA_err_1;
                double slope_UA_v_q = 0.0;

                do
                {
                    if (xy_q_1.x < 0.001)
                    {
                        *diff_q_dot = std::numeric_limits<double>::quiet_NaN();
                        return -3;
                    }

                    UA_err_2 = UA_err_1;
                    xy_q_2 = xy_q_1;

                    xy_q_1.x = xy_q_2.x * 0.1;
                    UA_err_1 = od_hx_solver.test_member_function(xy_q_1.x, &xy_q_1.y);

                    slope_UA_v_q = (xy_q_2.y - xy_q_1.y) / (xy_q_2.x - xy_q_1.x);

                } while (UA_err_1 != 0 || UA_err_2 != 0 || slope_UA_v_q <= 0.0 || xy_q_1.y > 0.0);

                double tol_solved, q_dot_solved;
                tol_solved = q_dot_solved = std::numeric_limits<double>::quiet_NaN();
                int iter_solved = -1;

                int od_hx_code = 0;

                try
                {
                    //od_hx_code = od_hx_solver.solve(q_dot_subhx_uniform, 1.1*q_dot_subhx_uniform, 0.0,
                    //    q_dot_solved, tol_solved, iter_solved);
                    od_hx_code = od_hx_solver.solve(xy_q_1, xy_q_2, 0.0,
                        q_dot_solved, tol_solved, iter_solved);
                }
                catch (C_csp_exception)
                {
                    *diff_q_dot = std::numeric_limits<double>::quiet_NaN();
                    return -1;
                }

                if (od_hx_code != C_monotonic_eq_solver::CONVERGED)
                {
                    if (!(od_hx_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.01))
                    {
                        *diff_q_dot = std::numeric_limits<double>::quiet_NaN();
                        return -2;
                    }
                }

                h_hot_in_i = od_hx_eq.m_h_hot_in;       //[kJ/kg]
                h_cold_out_i = od_hx_eq.m_h_cold_out;   //[kJ/kg]
                UA_calc_i = od_hx_eq.m_UA_calc;         //[kW/K]
                min_DT_i = od_hx_eq.m_min_DT;           //[K]
            }
            mpc_hx->mv_s_node_info_od[i] = v_s_node_info_i[0];
            m_UA_tot_calc += UA_calc_i;     //[kW/K]
            m_min_dT = fmin(min_DT_i, m_min_dT);    //[K]
        }

        // Reset cold in to cold out and hot out to hot in
        h_hot_out_i = h_hot_in_i;
        P_hot_out_i = P_hot_in_i;
        h_cold_in_i = h_cold_out_i;
        P_cold_in_i = P_cold_out_i;
    }

    // Calculate q_dot from node UA solution
    double q_dot_calc = m_m_dot_cold * (h_cold_out_i - m_h_cold_in);    //[kWt]

    *diff_q_dot = (q_dot_calc - q_dot_hx) / q_dot_hx;   //[kWt]

    return 0;
}

void C_HX_counterflow_CRM::off_design_solution_fixed_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
    double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
    double od_tol /*-*/,
    double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)
{
    if (m_od_solution_type == C_HX_counterflow_CRM::C_od_thermal_solution_type::E_CRM_UA_PER_NODE)
    {
        double h_cold_in = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_cold_fl, mc_cold_fl, T_c_in, P_c_in); //[kJ/kg]
        double h_hot_in = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_hot_fl, mc_hot_fl, T_h_in, P_h_in);    //[kJ/kg]

        double h_h_out_q_max, T_h_out_q_max, h_c_out_q_max, T_c_out_q_max, T_h_in_q_max, T_c_in_q_max;
        h_h_out_q_max = T_h_out_q_max = h_c_out_q_max = T_c_out_q_max = T_h_in_q_max = T_c_in_q_max = std::numeric_limits<double>::quiet_NaN();
        double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(ms_init_par.m_hot_fl, mc_hot_fl,
            ms_init_par.m_cold_fl, mc_cold_fl,
            h_hot_in, P_h_in, P_h_out, m_dot_h,
            h_cold_in, P_c_in, P_c_out, m_dot_c,
            h_h_out_q_max, T_h_out_q_max,
            h_c_out_q_max, T_c_out_q_max,
            T_h_in_q_max, T_c_in_q_max);

        if (q_dot_max > 0.0)
        {
            double T_h_out_local, T_c_out_local, UA_local, min_DT_local, eff_local, NTU_local, q_dot_calc_local;
            T_h_out_local = T_c_out_local = UA_local = min_DT_local = eff_local = NTU_local = q_dot_calc_local = std::numeric_limits<double>::quiet_NaN();
            std::vector<NS_HX_counterflow_eqs::S_hx_node_info> v_s_node_info_local;

            double q_dot_max_local = 0.9999*q_dot_max;
            bool is_feasible_q_dot = false;
            double q_dot_max_pinch = q_dot_max;

            while (!is_feasible_q_dot)
            {
                int err_code = 0;
                try
                {
                    NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl, ms_init_par.m_cold_fl, mc_cold_fl,
                        ms_init_par.m_N_sub_hx,
                        q_dot_max_local, m_dot_c, m_dot_h,
                        T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
                        UA_local, min_DT_local, eff_local, NTU_local,
                        T_h_out_local, T_c_out_local, q_dot_calc_local,
                        v_s_node_info_local);
                }
                catch (C_csp_exception & csp_except)
                {
                    err_code = csp_except.m_error_code;
                    q_dot_max_pinch = q_dot_max_local;
                    q_dot_max_local *= 0.995;
                }

                if (err_code == 0)
                {
                    is_feasible_q_dot = true;
                }
            }
            
            C_MEQ__hx_total_q_dot c_q_dot_eq(this, m_dot_c, m_dot_h,
                h_cold_in, h_hot_in,
                P_c_in, P_c_out,
                P_h_in, P_h_out, od_tol);

            C_monotonic_eq_solver c_q_dot_solver(c_q_dot_eq);

            double diff_q_dot_upper = std::numeric_limits<double>::quiet_NaN();
            int q_dot_upper_test_code = c_q_dot_solver.test_member_function(q_dot_calc_local, &diff_q_dot_upper);

            if (q_dot_upper_test_code != 0 || diff_q_dot_upper < 0.0)
            {
                C_monotonic_eq_solver::S_xy_pair xy1;
                C_monotonic_eq_solver::S_xy_pair xy2;
                double q_dot_guess_upper = q_dot_calc_local;

                if (q_dot_upper_test_code == 0)
                {
                    //if (q_dot_upper_test_code != 0)
                    //{
                    //    while (q_dot_upper_test_code != 0)
                    //    {
                    //        // Use design point effectiveness to generate 2 guess values
                    //        //double q_dot_mult = max(0.99, min(0.95, eff_max_limit) / eff_max_limit);
                    //        double q_dot_mult = 0.995;
                    //        if (std::isfinite(ms_des_solved.m_eff_design))
                    //        {
                    //            q_dot_mult = max(0.99, min(0.1, ms_des_solved.m_eff_design));
                    //        }

                    //        q_dot_guess_upper *= q_dot_mult;

                    //        q_dot_upper_test_code = c_q_dot_solver.test_member_function(q_dot_guess_upper, &diff_q_dot_upper);
                    //    }
                    //}

                    xy1.x = q_dot_guess_upper;
                    xy1.y = diff_q_dot_upper;

                    xy2.x = 0.95*q_dot_guess_upper;

                    q_dot_upper_test_code = c_q_dot_solver.test_member_function(xy2.x, &xy2.y);

                    while (q_dot_upper_test_code != 0)
                    {
                        xy2.x *= 1.005;

                        if (xy2.x > xy1.x)
                        {
                            break;
                        }

                        q_dot_upper_test_code = c_q_dot_solver.test_member_function(xy2.x, &xy2.y);
                    }
                }

                if (q_dot_upper_test_code == 0)
                {
                    c_q_dot_solver.settings(od_tol, 50, 0.0, q_dot_max_pinch, false);

                    double tol_solved, q_dot_solved;
                    tol_solved = q_dot_solved = std::numeric_limits<double>::quiet_NaN();
                    int iter_solved = -1;

                    int q_dot_hx_code = 0;

                    try
                    {
                        q_dot_hx_code = c_q_dot_solver.solve(xy1, xy2, 0.0,
                            q_dot_solved, tol_solved, iter_solved);
                    }
                    catch (C_csp_exception)
                    {
                        throw(C_csp_exception("C_HX_counterflow_CRM::off_design_solution threw exception trying to solve for the off-design"
                            "heat transfer for off design hx conductance"));
                    }

                    if (q_dot_hx_code != C_monotonic_eq_solver::CONVERGED)
                    {
                        if (!(q_dot_hx_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1))
                        {
                            throw(C_csp_exception("C_HX_counterflow_CRM::off_design_solution did not solve for the off-design"
                                "heat transfer for off design hx conductance within the specified tolerance"));
                        }
                    }

                    q_dot = q_dot_solved;
                }
                else
                {
                    q_dot_upper_test_code = c_q_dot_solver.test_member_function(q_dot_guess_upper, &diff_q_dot_upper);
                    q_dot = q_dot_guess_upper;
                }
            }
            else
            {
                // Off-design HX wants a q_dot resulting in an effectiveness greater than limit
                // What to do about this?        
                q_dot = q_dot_calc_local;
                //T_h_out = T_h_out_q_max;
                //T_c_out = T_c_out_q_max;
            }

            double eff_max_limit = fmin(0.99999, ms_des_calc_UA_par.m_eff_max);  //[-]
            double q_dot_upper_eff = eff_max_limit * q_dot_max;     //[kWt]

            if (q_dot_upper_eff < q_dot)
            {
                q_dot = q_dot_upper_eff;
                
                try
                {
                    NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl, ms_init_par.m_cold_fl, mc_cold_fl,
                        ms_init_par.m_N_sub_hx,
                        q_dot, m_dot_c, m_dot_h,
                        T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
                        UA_local, min_DT_local, eff_local, NTU_local,
                        T_h_out_local, T_c_out_local, q_dot_calc_local,
                        v_s_node_info_local);
                }
                catch (C_csp_exception & csp_except)
                {
                    throw(C_csp_exception("C_HX_counterflow_CRM::off_design_solution failed to solve 'calc_req_UA'"
                        " at effectiveness-limited heat transfer."));
                }

                T_h_out = T_h_out_local;
                T_c_out = T_c_out_local;

                ms_od_solved.m_min_DT = min_DT_local;
                ms_od_solved.m_UA_total = UA_local;
            }
            else
            {
                double h_hot_out = c_q_dot_eq.m_h_hot_out;      //[kJ/kg]
                double h_cold_old = c_q_dot_eq.m_h_cold_out;    //[kJ/kg]

                T_h_out = NS_HX_counterflow_eqs::hx_fl__calc_T__Ph(ms_init_par.m_hot_fl, mc_hot_fl, P_h_out, h_hot_out);
                T_c_out = NS_HX_counterflow_eqs::hx_fl__calc_T__Ph(ms_init_par.m_cold_fl, mc_cold_fl, P_c_out, h_cold_old);

                ms_od_solved.m_min_DT = c_q_dot_eq.m_min_dT;    //[K]
                ms_od_solved.m_UA_total = c_q_dot_eq.m_UA_tot_calc;     //[kW/K]
            }
        }
        else
        {
            // HX q_dot = 0.0
                // What to do about this?        
            q_dot = q_dot_max;
            T_h_out = T_h_out_q_max;
            T_c_out = T_c_out_q_max;
            ms_od_solved.m_min_DT = 0.0;
            ms_od_solved.m_UA_total = 0.0;
        }

        ms_od_solved.m_eff = q_dot / q_dot_max; //[-]
        //ms_od_solved.m_NTU = NTU;			//[-]
        ms_od_solved.m_P_c_out = P_c_out;	//[kPa]
        ms_od_solved.m_P_h_out = P_h_out;	//[kPa]
        ms_od_solved.m_q_dot = q_dot;		//[kWt]
        ms_od_solved.m_T_c_out = T_c_out;	//[K]
        ms_od_solved.m_T_h_out = T_h_out;	//[K]

        ms_od_solved.m_deltaP_cold = P_c_in - P_c_out;      //[kPa]
        ms_od_solved.m_deltaP_hot = P_h_in - P_h_out;       //[kPa]
    }
    else
    {
        // Set o.d. UA as solver target
        double UA_target = od_UA(m_dot_c, m_dot_h);	//[kW/K]
        double eff_target = ms_des_calc_UA_par.m_eff_max;


        ms_od_solved.m_q_dot = ms_od_solved.m_T_c_out = ms_od_solved.m_P_c_out =
            ms_od_solved.m_T_h_out = ms_od_solved.m_P_h_out = ms_od_solved.m_UA_total =
            ms_od_solved.m_min_DT = ms_od_solved.m_eff = ms_od_solved.m_NTU = std::numeric_limits<double>::quiet_NaN();

        double eff_calc, min_DT, NTU, UA_calc;
        eff_calc = min_DT = NTU = UA_calc = std::numeric_limits<double>::quiet_NaN();

        // Off-design should always use UA as the performance target
        // So set min_dT_target and eff_target parameters = nan
        int hx_target_code = NS_HX_counterflow_eqs::TARGET_UA;

        if (!m_is_single_node_des_set)
        {
            ms_node_info_des.UA = ms_des_solved.m_UA_design;

            // Hot fluid
            double P_h_in_des = ms_des_calc_UA_par.m_P_h_in;        //[kPa]
            double T_h_in_des = ms_des_calc_UA_par.m_T_h_in;        //[K]
            double h_h_in_des = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_hot_fl, mc_hot_fl, T_h_in_des, P_h_in_des);  //[kJ/kg]

            double P_h_out_des = ms_des_calc_UA_par.m_P_h_out;      //[kPa]
            double T_h_out_des = ms_des_solved.m_T_h_out;           //[K]
            double h_h_out_des = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_hot_fl, mc_hot_fl, T_h_out_des, P_h_out_des);   //[kJ/kg]

            double P_h_avg_des = 0.5*(P_h_in_des + P_h_out_des);
            double h_h_avg_des = 0.5*(h_h_in_des + h_h_out_des);

            NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_Ph_hot(ms_init_par.m_hot_fl, &mc_hot_fl, P_h_avg_des, h_h_avg_des, true);
            ms_node_info_des.s_fl_hot.k = c_Ph_hot.m_k;
            ms_node_info_des.s_fl_hot.rho = c_Ph_hot.m_rho;
            ms_node_info_des.s_fl_hot.mu = c_Ph_hot.m_mu;
            ms_node_info_des.s_fl_hot.cp = c_Ph_hot.m_cp;
            ms_node_info_des.s_fl_hot.m_dot = ms_des_calc_UA_par.m_m_dot_hot_des;       //[kg/s]

            // Cold fluid
            double P_c_in_des = ms_des_calc_UA_par.m_P_c_in;        //[kPa]
            double T_c_in_des = ms_des_calc_UA_par.m_T_c_in;        //[K]
            double h_c_in_des = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_cold_fl, mc_cold_fl, T_c_in_des, P_c_in_des);

            double P_c_out_des = ms_des_calc_UA_par.m_P_c_out;      //[kPa]
            double T_c_out_des = ms_des_solved.m_T_c_out;           //[K]
            double h_c_out_des = NS_HX_counterflow_eqs::hx_fl__calc_h__TP(ms_init_par.m_cold_fl, mc_cold_fl, T_c_out_des, P_c_out_des);

            double P_c_avg_des = 0.5*(P_c_in_des + P_c_out_des);
            double h_c_avg_des = 0.5*(h_c_in_des + h_c_out_des);

            NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_Ph_cold(ms_init_par.m_cold_fl, &mc_cold_fl, P_c_avg_des, h_c_avg_des, true);
            ms_node_info_des.s_fl_cold.k = c_Ph_cold.m_k;
            ms_node_info_des.s_fl_cold.rho = c_Ph_cold.m_rho;
            ms_node_info_des.s_fl_cold.mu = c_Ph_cold.m_mu;
            ms_node_info_des.s_fl_cold.cp = c_Ph_cold.m_cp;
            ms_node_info_des.s_fl_cold.m_dot = ms_des_calc_UA_par.m_m_dot_cold_des;

            m_is_single_node_des_set = true;
        }

        double h_c_in, h_c_out, h_h_in, h_h_out;
        h_c_in = h_c_out = h_h_in = h_h_out = std::numeric_limits<double>::quiet_NaN();

        double P_c_out_guess = P_c_out;
        double P_c_out_calc = P_c_out_guess;

        double P_h_out_guess = P_h_out;
        double P_h_out_calc = P_h_out_guess;

        bool is_hx_deltaP_converge = true;

        double diff_P_c_out = std::numeric_limits<double>::quiet_NaN();
        double diff_P_h_out = std::numeric_limits<double>::quiet_NaN();

        size_t iter_deltaP = 0;

        do
        {
            if (P_c_out_calc != P_c_out_guess)
            {
                P_c_out_guess = 0.9 * P_c_out_calc + 0.1 * P_c_out_guess;
            }
            
            if (P_h_out_calc != P_h_out_guess)
            {
                P_h_out_guess = 0.9 * P_h_out_calc + 0.1 * P_h_out_guess;
            }

            if (iter_deltaP > 10)
            {
                P_c_out_guess = P_c_in - (ms_des_calc_UA_par.m_P_c_in - ms_des_calc_UA_par.m_P_c_out);
                P_h_out_guess = P_h_in - (ms_des_calc_UA_par.m_P_h_in - ms_des_calc_UA_par.m_P_h_out);
                is_hx_deltaP_converge = false;
            }

            NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(hx_target_code,
                ms_init_par.m_hot_fl, mc_hot_fl,
                ms_init_par.m_cold_fl, mc_cold_fl,
                ms_node_info_des,
                ms_init_par.m_N_sub_hx, m_od_UA_target_type,
                T_c_in, P_c_in, m_dot_c, P_c_out_guess,
                T_h_in, P_h_in, m_dot_h, P_h_out_guess,
                UA_target, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
                eff_target, ms_des_solved.m_eff_design,
                od_tol,
                q_dot, T_c_out, T_h_out,
                h_c_in, h_c_out,
                h_h_in, h_h_out,
                eff_calc, min_DT, NTU, UA_calc,
                mv_s_node_info_des);

            double P_c_avg = 0.5*(P_c_in + P_c_out_guess);
            double h_c_avg = 0.5*(h_c_in + h_c_out);
            NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_c_fl_props(ms_init_par.m_cold_fl, &mc_cold_fl, P_c_avg, h_c_avg, false);
            double rho_c_avg = c_c_fl_props.m_rho;

            double P_h_avg = 0.5*(P_h_in + P_h_out_guess);
            double h_h_avg = 0.5*(h_h_in + h_h_out);
            NS_HX_counterflow_eqs::C_hx_fl__Ph__core c_h_fl_props(ms_init_par.m_hot_fl, &mc_hot_fl, P_h_avg, h_h_avg, false);
            double rho_h_avg = c_h_fl_props.m_rho;

            P_c_out_calc = P_c_in - (ms_des_calc_UA_par.m_P_c_in - ms_des_calc_UA_par.m_P_c_out) *
                (std::pow(m_dot_c, 2) / rho_c_avg) /
                (std::pow(ms_node_info_des.s_fl_cold.m_dot, 2) / ms_node_info_des.s_fl_cold.rho);

            P_h_out_calc = P_h_in - (ms_des_calc_UA_par.m_P_h_in - ms_des_calc_UA_par.m_P_h_out) *
                (std::pow(m_dot_h, 2) / rho_h_avg) /
                (std::pow(ms_node_info_des.s_fl_hot.m_dot, 2) / ms_node_info_des.s_fl_hot.rho);

            diff_P_c_out = (P_c_out_calc - P_c_out_guess) / P_c_out_guess;  //[-]
            diff_P_h_out = (P_h_out_calc - P_h_out_guess) / P_h_out_guess;  //[-]

            iter_deltaP++;

        } while (is_hx_deltaP_converge && (fabs(diff_P_c_out) > od_tol || fabs(diff_P_h_out) > od_tol));

        ms_od_solved.m_eff = eff_calc;			//[-]
        ms_od_solved.m_min_DT = min_DT;		//[K]
        ms_od_solved.m_NTU = NTU;			//[-]
        ms_od_solved.m_P_c_out = P_c_out_guess;	//[kPa]
        ms_od_solved.m_P_h_out = P_h_out_guess;	//[kPa]
        ms_od_solved.m_q_dot = q_dot;		//[kWt]
        ms_od_solved.m_T_c_out = T_c_out;	//[K]
        ms_od_solved.m_T_h_out = T_h_out;	//[K]
        ms_od_solved.m_UA_total = UA_calc;	//[kW/K]

        ms_od_solved.m_deltaP_cold = P_c_in - ms_od_solved.m_P_c_out;      //[kPa]
        ms_od_solved.m_deltaP_hot = P_h_in - ms_od_solved.m_P_h_out;       //[kPa]
    }
}

void C_HX_counterflow_CRM::off_design_solution_calc_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
    double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
    double od_tol /*-*/,
    double & q_dot /*kWt*/, double & T_c_out /*K*/, double & P_c_out /*kPa*/, double & T_h_out /*K*/, double & P_h_out /*kPa*/)
{
    // Calculate pressure drops
    P_c_out = P_c_in - ms_des_solved.m_DP_cold_des*od_delta_p_cold_frac(m_dot_c);
    P_h_out = P_h_in - ms_des_solved.m_DP_hot_des*od_delta_p_hot_frac(m_dot_h);

    off_design_solution_fixed_dP(T_c_in, P_c_in, m_dot_c, P_c_out, T_h_in, P_h_in, m_dot_h, P_h_out, od_tol, q_dot, T_c_out, T_h_out);
}

double C_HX_counterflow_CRM::od_delta_p_cold_frac(double m_dot_c /*kg/s*/)
{
    return pow(m_dot_c / ms_des_calc_UA_par.m_m_dot_cold_des, 1.75);
}

double C_HX_counterflow_CRM::od_delta_p_cold(double m_dot_c /*kg/s*/)
{
    return ms_des_solved.m_DP_cold_des*od_delta_p_cold_frac(m_dot_c);
}

double C_HX_counterflow_CRM::od_delta_p_hot_frac(double m_dot_h /*kg/s*/)
{
    return pow(m_dot_h / ms_des_calc_UA_par.m_m_dot_hot_des, 1.75);
}

double C_HX_counterflow_CRM::od_delta_p_hot(double m_dot_h /*kg/s*/)
{
    return ms_des_solved.m_DP_hot_des*od_delta_p_hot_frac(m_dot_h);
}

double C_HX_counterflow_CRM::od_UA(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/)
{
    return ms_des_solved.m_UA_design*od_UA_frac(m_dot_c, m_dot_h);	//[kW/K]
}

double C_HX_counterflow_CRM::od_UA_frac(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/)
{
    double m_dot_ratio = 0.5*(m_dot_c / ms_des_calc_UA_par.m_m_dot_cold_des + m_dot_h / ms_des_calc_UA_par.m_m_dot_hot_des);
    return pow(m_dot_ratio, 0.8);
}

double NS_HX_counterflow_eqs::UA_scale_vs_m_dot(double m_dot_cold_over_des /*-*/, double m_dot_hot_over_des /*-*/)
{
    double m_dot_ratio = 0.5*(m_dot_cold_over_des + m_dot_hot_over_des);
    return pow(m_dot_ratio, 0.8);
}

void C_HX_co2_to_co2_CRM::initialize(int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type)
{
    // Set design parameters member structure
    ms_init_par.m_N_sub_hx = N_sub_hx;
    ms_init_par.m_cold_fl = NS_HX_counterflow_eqs::CO2;
    ms_init_par.m_hot_fl = NS_HX_counterflow_eqs::CO2;
    m_is_HX_initialized = true;

    m_od_UA_target_type = od_UA_target_type;
}

void C_HX_co2_to_htf::initialize(int hot_fl, util::matrix_t<double> hot_fl_props, int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type)
{
	// Hard-code some of the design parameters
    ms_init_par.m_N_sub_hx = N_sub_hx;  //[-]
	ms_init_par.m_cold_fl = NS_HX_counterflow_eqs::CO2;

	// Read-in hot side HTF props
	ms_init_par.m_hot_fl = hot_fl;
	ms_init_par.mc_hot_fl_props = hot_fl_props;

    m_od_UA_target_type = od_UA_target_type;

	// Set up HTFProperties for the hot fluid
	if( ms_init_par.m_hot_fl != HTFProperties::User_defined && ms_init_par.m_hot_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_hot_fl.SetFluid(ms_init_par.m_hot_fl, true) )
		{
			throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
		}
	}
	else if( ms_init_par.m_hot_fl == HTFProperties::User_defined )
	{
		int n_rows = (int)ms_init_par.mc_hot_fl_props.nrows();
		int n_cols = (int)ms_init_par.mc_hot_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_hot_fl.SetUserDefinedFluid(ms_init_par.mc_hot_fl_props, true) )
			{
				std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
			}
		}
		else
		{
			std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
	}

	// Class is initialized
	m_is_HX_initialized = true;

}

void C_HX_co2_to_htf::initialize(int hot_fl, int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type)
{
	util::matrix_t<double> null_fluid_props;

	initialize(hot_fl, null_fluid_props, N_sub_hx, od_UA_target_type);
}

bool N_compact_hx::get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
	double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
	double & s_h, double & s_v, double & fin_V_per_m)
{

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		d_out = 0.0102;		//[m] Outer tube diameter
		fin_pitch = 315;	//[1/m] Number of fins per meter of tube
		D_h = 0.003632;		//[m] Hydraulic diameter of air side
		fin_thk = 0.0003302;//[m] Fin thickness
		sigma = 0.534;		//[-] Ratio of free-flow to frontal area
		alpha = 587;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.913;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.022;		//[m] Distance between tubes in air flow direction
		s_v = 0.0254;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = (s_h*s_v - 0.25*CSP::pi*pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	case fc_tubes_sCF_88_10Jb:
		d_out = 0.02601;		//[m] Outer tube diameter
		fin_pitch = 346;	//[1/m] Number of fins per meter of tube
		D_h = 0.01321;		//[m] Hydraulic diameter of air side
		fin_thk = 0.000305;//[m] Fin thickness
		sigma = 0.642;		//[-] Ratio of free-flow to frontal area
		alpha = 191;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.825;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.0524;		//[m] Distance between tubes in air flow direction
		s_v = 0.07818;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = 0.25*CSP::pi*(pow(0.04412, 2) - pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	default:
		return false;
	}

};

bool N_compact_hx::get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H)
{
	double Re_mm = fmax(0.001, Re*1e-3);

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		f = 0.02949346*pow(Re_mm, -0.208110211);
		j_H = 0.0105331507*pow(Re_mm, -0.400092073);
		return true;

	case fc_tubes_sCF_88_10Jb:
		f = 0.0606753986*pow(Re_mm, -0.256298233);
		j_H = 0.0148711552*pow(Re_mm, -0.382144871);
		return true;

	default:
		return false;
	}

};

C_CO2_to_air_cooler::C_CO2_to_air_cooler()
{
	m_th = m_eta_fan = m_roughness =
		m_A_cs = m_relRough = 
		m_L_path = m_A_surf_total = m_V_footprint =
		m_V_material_tubes = m_V_material_fins =
		m_m_dot_air_des = m_Q_dot_des = m_P_hot_out_des =
		m_fin_pitch = m_D_h = m_fin_thk = m_sigma = m_alpha = m_A_fin_to_surf = m_s_h = m_s_v = m_fin_V_per_m = numeric_limits<double>::quiet_NaN();

	m_N_nodes = m_enum_compact_hx_config = -1;

	m_T_co2_hot_max = 700.0 + 273.15;	//[K]

	mc_air.SetFluid(mc_air.Air);

	m_cost_model = C_CO2_to_air_cooler::E_CARLSON_17;		//[-]
}

bool C_CO2_to_air_cooler::design_hx(S_des_par_ind des_par_ind, S_des_par_cycle_dep des_par_cycle_dep, double tol /*-*/)
{
	// Set member structures
	ms_des_par_ind = des_par_ind;
	ms_des_par_cycle_dep = des_par_cycle_dep;

	// Check pressure drop is "reasonable"
	if (ms_des_par_cycle_dep.m_delta_P_des / ms_des_par_cycle_dep.m_P_hot_in_des < 0.001)
	{
		ms_des_par_cycle_dep.m_delta_P_des = 0.001 * ms_des_par_cycle_dep.m_P_hot_in_des;	//[kPa]
	}

	// Calculate ambient pressure
	ms_hx_des_sol.m_P_amb_des = air_pressure(ms_des_par_ind.m_elev);	//[Pa]
	
	//m_enum_compact_hx_config = fc_tubes_s80_38T;
	m_enum_compact_hx_config = N_compact_hx::fc_tubes_sCF_88_10Jb;

	// Get HX Geometry
	N_compact_hx::get_compact_hx_geom(m_enum_compact_hx_config, ms_hx_des_sol.m_d_out, m_fin_pitch, m_D_h, m_fin_thk,
		m_sigma, m_alpha, m_A_fin_to_surf, m_s_h, m_s_v, m_fin_V_per_m);

	// Thickness should really be tied to HX config
	//m_th = 0.001;		//fc_tubes_s80-38T
	m_th = 0.0024;		//fc_tubes_sCF-88-10Jb

	// Get Remaining Design Info: hardcode for now, but eventually will be inputs
	// Air-Cooler Specs
	ms_hx_des_sol.m_N_passes = 3;	    
    m_N_nodes = ms_des_par_ind.m_N_nodes_pass;      //[-]
    m_eta_fan = ms_des_par_ind.m_eta_fan;           //[-]
	ms_hx_des_sol.m_d_in = ms_hx_des_sol.m_d_out - 2.0*m_th;
	m_roughness = 4.5E-5;					//[m] absolute roughness of material
	m_A_cs = 0.25*CSP::pi*pow(ms_hx_des_sol.m_d_in, 2);	//[m2] flow cross-section area
	m_relRough = m_roughness / ms_hx_des_sol.m_d_in;		//[-] Relative Roughness

	m_P_hot_out_des = ms_des_par_cycle_dep.m_P_hot_in_des - ms_des_par_cycle_dep.m_delta_P_des;		//[kPa]
	double P_hot_ave = 0.5*(m_P_hot_out_des + ms_des_par_cycle_dep.m_P_hot_in_des);
	//double P_hot_ave = ms_des_par_cycle_dep.m_P_hot_in_des;			//[kPa]
	// Set up 'matrix_t's for temperature and pressure
	// Using index 1 for m_N_nodes, so 0 index remains undefined
	// Also, each node requires inlet&outlet temp, so in total, m_N_nodes + 2 required
	//mm_T_co2.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());
	//mm_P_co2.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());
	//mm_T_air.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());

	// index that gives outlet temperatur and pressure: depends on whether odd or even loops
	m_final_outlet_index = ((ms_hx_des_sol.m_N_passes + 2) % 2)*m_N_nodes + 1;

	double mu_air = std::numeric_limits<double>::quiet_NaN();
	double v_air = std::numeric_limits<double>::quiet_NaN();
	double cp_air = std::numeric_limits<double>::quiet_NaN();
	double k_air = std::numeric_limits<double>::quiet_NaN();
	double Pr_air = std::numeric_limits<double>::quiet_NaN();

	calc_air_props(ms_des_par_ind.m_T_amb_des, ms_hx_des_sol.m_P_amb_des,
		mu_air, v_air, cp_air, k_air, Pr_air);

	// Calculate the required heat rejection
	int co2_prop_err = CO2_TP(ms_des_par_cycle_dep.m_T_hot_in_des, ms_des_par_cycle_dep.m_P_hot_in_des, &mc_co2_props);
    if (co2_prop_err != 0)
    {
        throw(C_csp_exception("Air cooler design routine failed to calculate CO2 props at inlet"));
    }
	double h_in_des = mc_co2_props.enth*1000.0;					//[J/kg]
    co2_prop_err = CO2_TP(ms_des_par_cycle_dep.m_T_hot_out_des, m_P_hot_out_des, &mc_co2_props);
    if (co2_prop_err != 0)
    {
        throw(C_csp_exception("Air cooler design routine failed to calculate CO2 props at outlet"));
    }
	double h_out_des = mc_co2_props.enth*1000.0;				//[J/kg]

	if (ms_des_par_cycle_dep.m_m_dot_total > 0.0)
	{
		m_Q_dot_des = ms_des_par_cycle_dep.m_m_dot_total*(h_in_des - h_out_des);	//[Wt]
	}
	else if (ms_des_par_cycle_dep.m_Q_dot_des > 0.0)
	{
		m_Q_dot_des = ms_des_par_cycle_dep.m_Q_dot_des*1.E6;		//[Wt] convert from MWt
		ms_des_par_cycle_dep.m_m_dot_total = m_Q_dot_des / (h_in_des - h_out_des);	//[kg/s]
	}
	else
	{
		throw(C_csp_exception("Air cooler design parameters need to specify either m_Q_dot_des or m_m_dot_total as positive numbers"));
	}
	
	// double deltaT_hot = ms_des_par_cycle_dep.m_T_hot_in_des - ms_des_par_cycle_dep.m_T_hot_out_des;	//[K,C] Hot side temperature difference

	ms_hx_des_sol.m_Depth = m_s_h * ms_hx_des_sol.m_N_passes;	//[m] Dimension parallel to air flow

	// 1) Guess dimension perpendicular to air AND hot fluid flow
	// (basically the number of parallel flow paths)

	// ********************************************************************************
	// ** Set up guesses and control for bisection and false-position **
	// ** Try to get better guess by estimating length required to hit pressure drop **
	// ********************************************************************************
	double T_co2_deltaP_eval = 0.75*ms_des_par_cycle_dep.m_T_hot_in_des + 0.25*ms_des_par_cycle_dep.m_T_hot_out_des;
	co2_prop_err = CO2_TP(T_co2_deltaP_eval, ms_des_par_cycle_dep.m_P_hot_in_des, &mc_co2_props);
    if (co2_prop_err != 0)
    {
        throw(C_csp_exception("Air cooler design routine failed to calculate CO2 props at weighted temp and hot pressure"));
    }
	double visc_dyn_co2_g = CO2_visc(mc_co2_props.dens, mc_co2_props.temp)*1.E-6;

	// Just try hitting a "reasonable" Reynolds number?
	// This sets the mass flow rate in the tube, which then sets the number of required tubes
	//  to contain the defined mass flow rate
	//double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);
	double Re_g = 5.E6;		//[-] A "reasonable" Reynolds number
	double m_dot_tube_g1 = Re_g*m_A_cs*visc_dyn_co2_g / ms_hx_des_sol.m_d_in;	//[kg/s] Mass flow rate to achieve Reynolds number

	double N_par_g = ms_des_par_cycle_dep.m_m_dot_total / m_dot_tube_g1;	//[-] Number of parallel flow paths required to contain all mass flow

	double W_par_guess = N_par_g * m_s_v;		//[m] Dimension perpendicular to air AND hot fluid flow... parallel paths dimension

	//double tol = 1.E-3;		//[-] Relative tolerance for convergence

	C_MEQ_target_T_hot__width_parallel c_eq(this,
									mu_air, v_air,
									cp_air, Pr_air,
									T_co2_deltaP_eval, P_hot_ave,
									tol, tol);

	C_monotonic_eq_solver c_solver(c_eq);

	C_monotonic_eq_solver::S_xy_pair xy1;
	double T_hot_in_calc = std::numeric_limits<double>::quiet_NaN();
	
	int solver_code = -1;
	int i_W_par = -1;

	while(solver_code != 0)
	{
		i_W_par++;

		if (i_W_par > 0)
			W_par_guess *= 1.5;

		if(i_W_par > 10)
			throw(C_csp_exception("Air cooler iteration on the parallel width received exception from mono equation solver"));

		solver_code = c_solver.test_member_function(W_par_guess, &T_hot_in_calc);		
	}

	xy1.x = W_par_guess;	//[m]
	xy1.y = T_hot_in_calc;	//[K]

	double W_par_mult = 2.0;
	if (T_hot_in_calc > ms_des_par_cycle_dep.m_T_hot_in_des)
	{
		W_par_mult = 0.5;
	}

	C_monotonic_eq_solver::S_xy_pair xy2;
	double T_hot_in_calc_2 = std::numeric_limits<double>::quiet_NaN();
	double W_par_guess_2 = W_par_guess * W_par_mult;
	solver_code = -1;
	i_W_par = -1;

	while (solver_code != 0 || fabs(T_hot_in_calc_2 - T_hot_in_calc) / T_hot_in_calc < 0.01)
	{
		i_W_par++;

		if (i_W_par > 0)
		{
			W_par_guess_2 *= W_par_mult;
		}

		if (i_W_par > 10)
			throw(C_csp_exception("Air cooler iteration on the parallel width received exception from mono equation solver"));

		solver_code = c_solver.test_member_function(W_par_guess_2, &T_hot_in_calc_2);
	}

	xy2.x = W_par_guess_2;		//[m]
	xy2.y = T_hot_in_calc_2;	//[K]

	c_solver.settings(tol, 50, 0.01, std::numeric_limits<double>::quiet_NaN(), true);

	double W_par_solved, tol_solved;
	W_par_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	solver_code = 0;
	try
	{
		solver_code = c_solver.solve(xy1, xy2, ms_des_par_cycle_dep.m_T_hot_in_des,
			W_par_solved, tol_solved, iter_solved);
			
			//c_solver.solve(W_par_guess, W_par_guess*2.0, ms_des_par_cycle_dep.m_T_hot_in_des,
			//W_par_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on the parallel width received exception from mono equation solver"));
	}
	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on the parallel width only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				tol_solved);
			mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			throw(C_csp_exception("Air cooler iteration on the parallel width did not converge"));
		}
	}

	// Final reporting metrics
	ms_hx_des_sol.m_W_par = W_par_solved;			//[m] Dimension perpendicular to loop/air flow direction
	ms_hx_des_sol.m_N_par = c_eq.m_N_par;			//[-] Number of parallel flow paths
	ms_hx_des_sol.m_L_tube = c_eq.m_L_tube;			//[m] Tube length
	m_L_path = ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_N_passes;	//[m] Total flow path length
	ms_hx_des_sol.m_N_tubes = c_eq.m_N_tubes;		//[-] Number of tubes
	m_V_footprint = c_eq.m_V_total;		//[m^3] Total HX "footprint" Volume
	m_A_surf_total = m_V_footprint*m_alpha;			//[-] Air-side surface area of node
	ms_hx_des_sol.m_UA_total = m_A_surf_total*c_eq.m_h_conv_air;	//[W/K] Total HX Conductance
	m_m_dot_air_des = c_eq.m_m_dot_air_total;		//[kg/s] Total air mass flow rate
	m_A_surf_node = c_eq.m_A_surf_node;		//[m^2] Air-side surface area of node

	double L_tube_total = ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_N_tubes;		//[m] Cumulative length of tubes
	m_V_material_tubes = 0.25*CSP::pi*(pow(ms_hx_des_sol.m_d_out, 2) - pow(ms_hx_des_sol.m_d_in, 2))*L_tube_total;	//[m3] Total material required for tubing
	m_V_material_fins = m_fin_V_per_m*L_tube_total;		//[m3] Total material required for fins
	ms_hx_des_sol.m_V_material_total = m_V_material_tubes + m_V_material_fins;	//[m3] Total material in HX
	ms_hx_des_sol.m_V_total = c_eq.m_V_total;		//[m^3]

	ms_hx_des_sol.m_L_node = ms_hx_des_sol.m_L_tube / (double)m_N_nodes;	//[m] Length of one node
	ms_hx_des_sol.m_V_node = ms_hx_des_sol.m_L_node*m_s_v*m_s_h;	//[m^3] Volume of one node

	ms_hx_des_sol.m_m_dot_co2 = ms_des_par_cycle_dep.m_m_dot_total;		//[kg/s] Total CO2 flow rate
	ms_hx_des_sol.m_T_in_co2 = ms_des_par_cycle_dep.m_T_hot_in_des;		//[K] Hot CO2 inlet temperature
	ms_hx_des_sol.m_P_in_co2 = ms_des_par_cycle_dep.m_P_hot_in_des;		//[kPa] Hot CO2 inlet pressure
	ms_hx_des_sol.m_T_out_co2 = ms_des_par_cycle_dep.m_T_hot_out_des;	//[K] Cold CO2 outlet temperature
	ms_hx_des_sol.m_P_out_co2 = m_P_hot_out_des;			//[K] Cold CO2 outlet pressure
	ms_hx_des_sol.m_q_dot = m_Q_dot_des;					//[Wt] Heat exchanger duty

	ms_hx_des_sol.m_W_dot_fan = ms_des_par_cycle_dep.m_W_dot_fan_des;	//[MWe]

	ms_hx_des_sol.m_cost = calculate_cost(ms_hx_des_sol.m_UA_total*1.E-3, ms_hx_des_sol.m_V_total,
		ms_hx_des_sol.m_T_in_co2, ms_hx_des_sol.m_P_in_co2, ms_hx_des_sol.m_m_dot_co2);		//[M$]

	return true;
};

int C_MEQ_target_W_dot_fan__m_dot_air::operator()(double m_dot_air /*kg/s*/, double *W_dot_fan /*MWe*/)
{
	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();

	double G_air = m_dot_air / (m_sigma*m_L_tube*m_W_par);
	double Re_air = G_air*m_D_h / m_mu_air;
	
	double f_air, j_H_air;
	f_air = numeric_limits<double>::quiet_NaN();
	j_H_air = numeric_limits<double>::quiet_NaN();

	if (!N_compact_hx::get_compact_hx_f_j(m_comp_hx_config, Re_air, f_air, j_H_air))
		return -1;

	double deltaP_air = pow(G_air, 2.0)*m_v_air*0.5*f_air*m_alpha*m_V_total / (m_sigma*m_L_tube*m_W_par);
	m_h_conv_air = j_H_air*G_air*m_cp_air / pow(m_Pr_air, (2.0 / 3.0));	//[W/m^2-K]

	double V_dot_air_total = m_dot_air*m_v_air;
	*W_dot_fan = deltaP_air*V_dot_air_total / m_eta_fan / 1.E6;

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_node_energy_balance__h_co2_out::operator()(double h_co2_hot_in /*kJ/kg*/, double *diff_h_co2_hot /*-*/)
{
    m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();    //[W]
    m_T_co2_hot_in = std::numeric_limits<double>::quiet_NaN();  //[K]

    // First check that CO2 cold temperature exists and is hotter than cold air temperature
    if (!std::isfinite(m_T_co2_cold_out) || m_T_co2_cold_out < m_T_air_cold_in)
    {
        return -1;
    }

    // CO2 properties at hot side
    int co2_prop_error = CO2_PH(m_P_co2_hot_in, h_co2_hot_in, mpc_co2_props);
    if (co2_prop_error != 0)
    {
        return -3;
    }
    m_T_co2_hot_in = mpc_co2_props->temp;      //[K]

    // CO2 properties at node average
    double h_co2_ave = 0.5*(h_co2_hot_in + m_h_co2_cold_out);   //[kJ/kg]
    co2_prop_error = CO2_PH(m_P_co2_node_ave, h_co2_ave, mpc_co2_props);
    if (co2_prop_error != 0)
    {
        return -2;
    }
    double cp_co2_ave = mpc_co2_props->cp*1000.0;       //[J/kg-K]

    double C_dot_min, C_dot_max, CR;
    C_dot_min = C_dot_max = CR = std::numeric_limits<double>::quiet_NaN();
    // Check if 2-phase
    if (!(std::isfinite(cp_co2_ave) && cp_co2_ave > 0.0))
    {
        C_dot_min = m_C_dot_air;        //[W/K]
        CR = 0.0;                       //[-]
    }
    else
    {
        double C_dot_co2 = cp_co2_ave * m_m_dot_co2_tube;			//[W/K]
        C_dot_min = fmin(m_C_dot_air, C_dot_co2);		//[W/K]
        C_dot_max = fmax(m_C_dot_air, C_dot_co2);		//[W/K]
        CR = C_dot_min / C_dot_max;			            //[-]
    }

    double Q_dot_max = C_dot_min * (m_T_co2_hot_in - m_T_air_cold_in);	//[W]
    double NTU = m_UA_node / C_dot_min;

    double epsilon = std::numeric_limits<double>::quiet_NaN();
    if (CR > 0.0)
    {
        epsilon = 1 - exp(pow(NTU, 0.22) / CR * (exp(-CR * pow(NTU, 0.78)) - 1));   //[-]
    }
    else
    {
        epsilon = 1.0 - exp(-NTU);
    }

    m_Q_dot_node = epsilon * Q_dot_max * 1.E-3;     //[kW]

    //double q_dot_co2_guessed = m_m_dot_co2_tube * (h_co2_hot_in - m_h_co2_cold_out);       //[kW]

    //*diff_q_dot = (q_dot_co2_guessed - m_Q_dot_node) / std::max(q_dot_co2_guessed, 1.E-10); //[-]

    double h_co2_cold_calc = h_co2_hot_in - m_Q_dot_node / m_m_dot_co2_tube;    //[kJ/kg]

    *diff_h_co2_hot = (h_co2_cold_calc - m_h_co2_cold_out) / m_h_co2_cold_out;      //[-]

    return 0;
}


int C_CO2_to_air_cooler::C_MEQ_node_energy_balance__T_co2_out::operator()(double T_co2_hot_in /*K*/, double *diff_T_co2_cold /*-*/)
{
	if (m_T_co2_cold_out <= m_T_air_cold_in)
	{
		return -1;
	}
	
	m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();

	double T_co2_ave = 0.5*(T_co2_hot_in + m_T_co2_cold_out);		//[K]

	int co2_prop_error = CO2_TP(T_co2_ave, m_P_co2_ave, mpc_co2_props);
	if (co2_prop_error != 0)
	{
		return -2;
	}
	double cp_co2_ave = mpc_co2_props->cp*1000.0;		//[J/kg-K]

	// Capacitance rates
	double C_dot_co2 = cp_co2_ave*m_m_dot_co2_tube;			//[W/K]
	double C_dot_min = fmin(m_C_dot_air, C_dot_co2);		//[W/K]
	double C_dot_max = fmax(m_C_dot_air, C_dot_co2);		//[W/K]
	double CR = C_dot_min / C_dot_max;			//[-]
	double Q_dot_max = C_dot_min*(T_co2_hot_in - m_T_air_cold_in);	//[W]
	double NTU = m_UA_node / C_dot_min;
	// Unmixed cross-flow
	double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
	m_Q_dot_node = epsilon*Q_dot_max;
	
	double T_co2_cold_calc = T_co2_hot_in - m_Q_dot_node / C_dot_co2;
	
	*diff_T_co2_cold = (T_co2_cold_calc - m_T_co2_cold_out) / m_T_co2_cold_out;		//[-]

	return 0;
}

//int C_CO2_to_air_cooler::C_MEQ_target_CO2_dP__L_tube_pass::operator()(double L_tube /*m*/, double *delta_P_co2 /*kPa*/)
//{
//	double L_total = L_tube*mpc_ac->ms_hx_des_sol.m_N_passes;	//[m] Total length of flow path including loops
//	double L_node = L_tube/mpc_ac->m_N_nodes;	//[m] Length of one node
//	double V_node = L_node*mpc_ac->m_s_v*mpc_ac->m_s_h;	//[m^3] Volume of one node
//	m_V_total = L_tube*mpc_ac->ms_hx_des_sol.m_Depth*m_W_par;		//[m^3] Total HX footprint volume
//
//	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();		//[W/m2-K]
//	m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();	//[kg/s]
//	m_A_surf_node = std::numeric_limits<double>::quiet_NaN();		//[m2]
//
//	// Iterate to find air mass flow rate resulting in target fan power
//	C_MEQ_target_W_dot_fan__m_dot_air c_m_dot_air_eq(L_tube, m_W_par, m_V_total,
//		m_mu_air, m_v_air, m_cp_air, m_Pr_air,
//		mpc_ac->m_sigma, mpc_ac->m_D_h,
//		mpc_ac->m_enum_compact_hx_config,
//		mpc_ac->m_alpha, mpc_ac->m_eta_fan);
//	C_monotonic_eq_solver c_m_dot_air_solver(c_m_dot_air_eq);
//
//	double tol_m_dot = m_tol_upper / 2.0;					//[-] Relative tolerance for convergence
//	c_m_dot_air_solver.settings(tol_m_dot, 50, 1.E-10, std::numeric_limits<double>::quiet_NaN(), true);
//
//	double m_dot_air_guess = mpc_ac->m_Q_dot_des / (5.0*m_cp_air);	//[kg/s] Guess assuming 5k temp rise
//	double m_dot_air_guess2 = 1.05*m_dot_air_guess;		//[kg/s] Another guess...
//
//	double m_dot_air_solved, m_dot_air_tol_solved;
//	m_dot_air_solved = m_dot_air_tol_solved = std::numeric_limits<double>::quiet_NaN();
//	int m_dot_air_iter_solved = -1;
//
//	int m_dot_air_solver_code = 0;
//	try
//	{
//		m_dot_air_solver_code = c_m_dot_air_solver.solve(m_dot_air_guess, m_dot_air_guess2,
//			mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des, m_dot_air_solved, m_dot_air_tol_solved, m_dot_air_iter_solved);
//	}
//	catch (C_csp_exception)
//	{
//		throw(C_csp_exception("Air cooler iteration on air mass flow rate received exception from mono equation solver"));
//	}
//	if (m_dot_air_solver_code != C_monotonic_eq_solver::CONVERGED)
//	{
//		if (m_dot_air_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(m_dot_air_tol_solved) <= 0.1)
//		{
//			std::string error_msg = util::format("Air cooler iteration on air mass flow rate only reached a convergence "
//				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
//				m_dot_air_tol_solved);
//			mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
//		}
//		else
//		{
//			return -1;
//			//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
//		}
//	}
//
//	m_m_dot_air_total = m_dot_air_solved;			//[kg/s]
//	m_h_conv_air = c_m_dot_air_eq.m_h_conv_air;		//[W/m2-K]
//
//	m_A_surf_node = V_node*mpc_ac->m_alpha;			//[m2] Air-side surface area of node
//	double UA_node = m_A_surf_node*m_h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance
//
//	// Set known inlet conditions: iteration thru # of loops needs previous loop info
//	mpc_ac->mm_T_co2(1, 0) = mpc_ac->ms_des_par_cycle_dep.m_T_hot_out_des;
//	mpc_ac->mm_P_co2(1, 0) = mpc_ac->ms_des_par_cycle_dep.m_P_hot_in_des - mpc_ac->ms_des_par_cycle_dep.m_delta_P_des;	//[kPa]
//	for (int i = 1; i < mpc_ac->m_N_nodes + 2; i++)
//		mpc_ac->mm_T_air(i, 0) = mpc_ac->ms_des_par_ind.m_T_amb_des;
//
//	// Assuming constant air props, so can set those
//	double m_dot_air_tube = m_m_dot_air_total / (m_N_par*mpc_ac->m_N_nodes);
//	double C_dot_air = m_cp_air*m_dot_air_tube;	//[W/K]
//
//	for (int j = 1; j < mpc_ac->ms_hx_des_sol.m_N_passes + 1; j++)
//	{
//		// Set up constants and multipliers to switch direction of flow
//		double mult_const = (j + 1) % 2;
//		double constant = mpc_ac->m_N_nodes + 2;
//		double mult_index = 1.0 - 2.0*mult_const;
//		double out_const = mult_index;
//
//		// Set inlet temperatures & pressures of current row
//		double mult_inlet = (j + 1) % 2;
//		double const_inlet = mpc_ac->m_N_nodes;
//		double inlet = mult_inlet*const_inlet + 1;
//
//		// Set loop inlet conditions
//		mpc_ac->mm_T_co2((int)inlet, j) = mpc_ac->mm_T_co2((int)inlet, j - 1);		//[K]
//		mpc_ac->mm_P_co2((int)inlet, j) = mpc_ac->mm_P_co2((int)inlet, j - 1);		//[kPa]
//
//		//double deltaT_prev = numeric_limits<double>::quiet_NaN();
//
//		for (int i = 1; i < mpc_ac->m_N_nodes + 1; i++)
//		{
//			double in = mult_const*constant + mult_index*i;
//			double out = in + out_const;
//			double air_in = fmin(in, out);
//
//			// Get CO2 and Air inlet temperatures to node
//			double T_co2_cold_local = mpc_ac->mm_T_co2((size_t)in, j);			//[K]
//			double T_air_cold_in_local = mpc_ac->mm_T_air((size_t)air_in, j - 1);	//[K]
//
//			// Set max allowable CO2 temp here, for now
//			double T_co2_hot_max = 700.0 + 273.15;		//[K]
//
//			C_MEQ_node_energy_balance__T_co2_out c_node_bal_eq(&mpc_ac->mc_co2_props,
//				T_co2_cold_local, m_P_hot_ave,
//				m_m_dot_tube,
//				T_air_cold_in_local, C_dot_air,
//				UA_node);
//			C_monotonic_eq_solver c_node_bal_solver(c_node_bal_eq);
//
//			double tol_T_in = tol_m_dot / 5.0;		//[-] Relative tolerance for convergence
//			c_node_bal_solver.settings(tol_T_in, 50, T_co2_cold_local, T_co2_hot_max, false);
//
//			double diff_T_co2_cold_calc = std::numeric_limits<double>::quiet_NaN();
//			int diff_T_co2_cold_calc_code = c_node_bal_solver.test_member_function(T_co2_hot_max, &diff_T_co2_cold_calc);
//			if (diff_T_co2_cold_calc_code != 0)
//			{
//				throw(C_csp_exception("Air cooler UA calculation failed at maximum CO2 temperature"));
//			}
//
//			double T_co2_hot_solved = std::numeric_limits<double>::quiet_NaN();
//			if (diff_T_co2_cold_calc >= 0.0)
//			{	// Use monotonic equation solver to fine T_co2_hot
//				double T_co2_hot_local_guess = T_co2_cold_local + 0.02;			//[K]
//				double T_co2_hot_local_guess_2 = T_co2_hot_local_guess + 1.5;	//[K]
//
//				double T_co2_hot_tol_solved;
//				T_co2_hot_tol_solved = std::numeric_limits<double>::quiet_NaN();
//				int T_co2_hot_iter_solved = -1;
//
//				int T_co2_hot_solver_code = 0;
//				try
//				{
//					T_co2_hot_solver_code = c_node_bal_solver.solve(T_co2_hot_local_guess, T_co2_hot_local_guess_2, 0.0, T_co2_hot_solved, T_co2_hot_tol_solved, T_co2_hot_iter_solved);
//				}
//				catch (C_csp_exception)
//				{
//					throw(C_csp_exception("Air cooler calculation to find T_co2_hot that matched node UA returned an exception"));
//				}
//
//				if (T_co2_hot_solver_code != C_monotonic_eq_solver::CONVERGED)
//				{
//					if (T_co2_hot_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(T_co2_hot_tol_solved) <= 0.1)
//					{
//						std::string error_msg = util::format("Air cooler iteration on nodal hot co2 temperature rate only reached a convergence "
//							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
//							T_co2_hot_tol_solved);
//						mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
//					}
//					else
//					{
//						return -2;
//						//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
//					}
//				}
//			}
//			else
//			{	// CO2 is unrealistically hot. stop here before property routine gets weird
//				//    (although, generally, returning a unique value is useful...)
//				//    (which is why we let it get this hot instead of say using the target outlet temp)
//				T_co2_hot_solved = T_co2_hot_max;
//			}
//
//			double Q_dot_node = c_node_bal_eq.m_Q_dot_node;		//[W]
//			mpc_ac->mm_T_co2((size_t)out, j) = T_co2_hot_solved;		//[K]
//			mpc_ac->mm_T_air((size_t)air_in, j) = mpc_ac->mm_T_air((size_t)air_in, j - 1) + Q_dot_node / C_dot_air;	//[K]
//
//			// Add pressure drop calcs (co2_props is up-to-date)
//			// ** Could also move this to a function if also called to guess length
//			double visc_dyn_co2 = CO2_visc(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp)*1.E-6;	//[Pa-s] convert from (uPa-s)
//			double Re_co2 = m_m_dot_tube*mpc_ac->ms_hx_des_sol.m_d_in / (mpc_ac->m_A_cs*visc_dyn_co2);		//[-]
//
//			double rho_co2 = mpc_ac->mc_co2_props.dens;				//[kg/s]
//			double visc_kin_co2 = visc_dyn_co2 / rho_co2;	//[m2/s]
//			double cond_co2 = CO2_cond(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp);	//[W/m-K]
//			double specheat_co2 = mpc_ac->mc_co2_props.cp*1000.0;		//[J/kg-K] convert from kJ/kg-K
//			double alpha_co2 = cond_co2 / (specheat_co2*rho_co2);	//[m2/s]
//			double Pr_co2 = visc_kin_co2 / alpha_co2;		//[-]
//
//			double Nusselt_co2 = -999.9;	//[-]
//			double f_co2 = -999.9;			//[-]
//
//			// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
//			// CSP::PipeFlow(Re_co2, Pr_co2, 1000.0, m_relRough, Nusselt_co2, f_co2);
//			CSP::PipeFlow(Re_co2, Pr_co2, L_node / mpc_ac->ms_hx_des_sol.m_d_in, mpc_ac->m_relRough, Nusselt_co2, f_co2);
//
//			double u_m = m_m_dot_tube / (rho_co2*mpc_ac->m_A_cs);		//[m/s]
//			mpc_ac->mm_P_co2((size_t)out, j) = mpc_ac->mm_P_co2((size_t)in, j) + f_co2*L_node*rho_co2*pow(u_m, 2) / (2.0*mpc_ac->ms_hx_des_sol.m_d_in) / 1000.0;	//[kPa]
//
//			double deltaP_node = mpc_ac->mm_P_co2((size_t)out, j) - mpc_ac->mm_P_co2((size_t)in, j);	//[kPa]
//
//			mpc_ac->mm_P_co2((size_t)out, j) = fmin(25000.0, fmax(1000.0, mpc_ac->mm_P_co2((size_t)out, j)));
//
//		}	// End iteration through nodes in flow path		
//
//	}	// End iteration through loop in flow path
//
//	*delta_P_co2 = mpc_ac->mm_P_co2(mpc_ac->m_final_outlet_index, mpc_ac->ms_hx_des_sol.m_N_passes) - mpc_ac->m_P_hot_out_des;	//[kPa]
//
//	return 0;
//}

int C_CO2_to_air_cooler::C_MEQ_target_CO2_dP__L_tube_pass::operator()(double L_tube /*m*/, double *delta_P_co2 /*kPa*/)
{
	double L_node = L_tube / (double)mpc_ac->m_N_nodes;	//[m] Length of one node
	double V_node = L_node*mpc_ac->m_s_v*mpc_ac->m_s_h;	//[m^3] Volume of one node
	m_V_total = L_tube*mpc_ac->ms_hx_des_sol.m_Depth*m_W_par;		//[m^3] Total HX footprint volume

	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();		//[W/m2-K]
	m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();	//[kg/s]
	m_A_surf_node = std::numeric_limits<double>::quiet_NaN();		//[m2]
	
	// Iterate to find air mass flow rate resulting in target fan power
	C_MEQ_target_W_dot_fan__m_dot_air c_m_dot_air_eq(L_tube, m_W_par, m_V_total,
		m_mu_air, m_v_air, m_cp_air, m_Pr_air,
		mpc_ac->m_sigma, mpc_ac->m_D_h,
		mpc_ac->m_enum_compact_hx_config,
		mpc_ac->m_alpha, mpc_ac->m_eta_fan);
	C_monotonic_eq_solver c_m_dot_air_solver(c_m_dot_air_eq);

	double tol_m_dot = m_tol_upper / 2.0;					//[-] Relative tolerance for convergence
	c_m_dot_air_solver.settings(tol_m_dot, 50, 1.E-10, std::numeric_limits<double>::quiet_NaN(), true);

	double m_dot_air_guess = mpc_ac->m_Q_dot_des / (5.0*m_cp_air);	//[kg/s] Guess assuming 5k temp rise
    double W_dot_fan_test = std::numeric_limits<double>::quiet_NaN();   //[MWe]

    c_m_dot_air_solver.test_member_function(m_dot_air_guess, &W_dot_fan_test);
    if (!std::isfinite(W_dot_fan_test))
    {
        return -1;
    }

    // Want to make a rough bracket above and below target
    C_monotonic_eq_solver::S_xy_pair xy_1;
    int i_m_dot_guess = -1;         //[-]
    if (W_dot_fan_test < mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des)
    {
        // Increase mass flow rate to increase fan power
        while (W_dot_fan_test < mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des)
        {
            xy_1.x = m_dot_air_guess;   //[kg/s]
            xy_1.y = W_dot_fan_test;    //[MWe]

            i_m_dot_guess++;
            
            m_dot_air_guess *= 2.0;

            if (i_m_dot_guess > 20)
            {
                return -2;
            }

            c_m_dot_air_solver.test_member_function(m_dot_air_guess, &W_dot_fan_test);

            if (!std::isfinite(W_dot_fan_test))
            {
                return -3;
            }
        }
    }
    else
    {
        // Decrease mass flow rate to decrease fan power
        while (W_dot_fan_test > mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des)
        {
            xy_1.x = m_dot_air_guess;   //[kg/s]
            xy_1.y = W_dot_fan_test;    //[MWe]
            
            i_m_dot_guess++;

            m_dot_air_guess *= 0.5;

            if (i_m_dot_guess > 20)
            {
                return -2;
            }

            c_m_dot_air_solver.test_member_function(m_dot_air_guess, &W_dot_fan_test);

            if (!std::isfinite(W_dot_fan_test))
            {
                return -3;
            }
        }
    }

    C_monotonic_eq_solver::S_xy_pair xy_2;
    xy_2.x = m_dot_air_guess;       //[kg/s]
    xy_2.y = W_dot_fan_test;        //[MWe]

	double m_dot_air_solved, m_dot_air_tol_solved;
	m_dot_air_solved = m_dot_air_tol_solved = std::numeric_limits<double>::quiet_NaN();
	int m_dot_air_iter_solved = -1;

	int m_dot_air_solver_code = 0;
	try
	{
		m_dot_air_solver_code = c_m_dot_air_solver.solve(xy_1, xy_2,
			mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des, m_dot_air_solved, m_dot_air_tol_solved, m_dot_air_iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on air mass flow rate received exception from mono equation solver"));
	}
	if (m_dot_air_solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (m_dot_air_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(m_dot_air_tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on air mass flow rate only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				m_dot_air_tol_solved);
			mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			return -1;
			//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
		}
	}

	m_m_dot_air_total = m_dot_air_solved;			//[kg/s]
	m_h_conv_air = c_m_dot_air_eq.m_h_conv_air;		//[W/m2-K]

	m_A_surf_node = V_node*mpc_ac->m_alpha;			//[m2] Air-side surface area of node

	double tol_T_in = tol_m_dot / 5.0;		//[-] Relative tolerance

	double T_co2_in_calc = std::numeric_limits<double>::quiet_NaN();
    double q_dot_tube_calc = std::numeric_limits<double>::quiet_NaN();       //[kWt]
    double P_co2_out = mpc_ac->ms_des_par_cycle_dep.m_P_hot_in_des - mpc_ac->ms_des_par_cycle_dep.m_delta_P_des;
	// Solve air cooler performance with known geometry and inputs
	int air_cooler_code = co2_outlet_given_geom_and_air_m_dot(mpc_ac->ms_des_par_cycle_dep.m_T_hot_out_des, m_m_dot_tube,
		P_co2_out, mpc_ac->ms_des_par_cycle_dep.m_P_hot_in_des,
		mpc_ac->ms_des_par_ind.m_T_amb_des,
		tol_T_in, m_tol_pressure,
		&mpc_ac->mc_messages, &mpc_ac->mc_co2_props,
		mpc_ac->ms_hx_des_sol.m_d_in, mpc_ac->m_A_cs, mpc_ac->m_relRough,
		L_node, V_node, mpc_ac->m_N_nodes, 
		m_N_par, mpc_ac->ms_hx_des_sol.m_N_passes,
		mpc_ac->m_alpha, m_cp_air, 
		m_m_dot_air_total, m_h_conv_air,
		*delta_P_co2, T_co2_in_calc, q_dot_tube_calc);

	m_T_co2_in_calc = T_co2_in_calc;

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_target_T_hot__width_parallel::operator()(double W_par /*m*/, double *T_co2_hot /*K*/)
{
	m_L_tube = std::numeric_limits<double>::quiet_NaN();		//[m]
	m_V_total = std::numeric_limits<double>::quiet_NaN();		//[m3]
	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();	//[W/m2-K]
	m_A_surf_node = std::numeric_limits<double>::quiet_NaN();	//[m2]
	
	// Divide by the distance between tubes to get number of parallel units
	// This is number of tube connected to the inlet headers
	// Can be any positive rational number so a continuous solution space is available
	m_N_par = W_par / mpc_ac->m_s_v;
	m_N_tubes = m_N_par*mpc_ac->ms_hx_des_sol.m_N_passes;
	/// Can now calculate the mass flow rate per tube
	double m_dot_tube = mpc_ac->ms_des_par_cycle_dep.m_m_dot_total / m_N_par;

	// 2) Guess the length of the hot side tube for one pass/loop
	// ********************************************************************************
	// ** Try to estimate length required to hit pressure drop **
	// ********************************************************************************
    int co2_prop_err = CO2_TP(m_T_co2_deltaP_eval, m_P_hot_ave, &mpc_ac->mc_co2_props);
    if (co2_prop_err != 0)
    {
        *T_co2_hot = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }
	double visc_dyn_co2_g = CO2_visc(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp)*1.E-6;
	double Re_co2_g = m_dot_tube*mpc_ac->ms_hx_des_sol.m_d_in / (mpc_ac->m_A_cs*visc_dyn_co2_g);

	double rho_co2_g = mpc_ac->mc_co2_props.dens;
	double visc_kin_co2_g = visc_dyn_co2_g / rho_co2_g;
	double cond_co2_g = CO2_cond(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp);
	double specheat_co2_g = mpc_ac->mc_co2_props.cp*1000.0;
	double alpha_co2_g = cond_co2_g / (specheat_co2_g*rho_co2_g);
	double Pr_co2_g = visc_kin_co2_g / alpha_co2_g;

	double Nusselt_co2_g = -999.9;
	double f_co2_g = -999.9;

	double tol_L_tube = m_tol / 2.0;

	// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
	CSP::PipeFlow(Re_co2_g, Pr_co2_g, 1000.0, mpc_ac->m_relRough, Nusselt_co2_g, f_co2_g);

	double u_m = m_dot_tube / (rho_co2_g*mpc_ac->m_A_cs);
	//m_delta_P_des*1000.0 = f_co2_g*L_node*rho_co2_g*pow(u_m,2)/(2.0*m_d_in)
	double L_tube_guess = mpc_ac->ms_des_par_cycle_dep.m_delta_P_des*1000.0*(2.0*mpc_ac->ms_hx_des_sol.m_d_in) / (f_co2_g*rho_co2_g*pow(u_m, 2)) / mpc_ac->ms_hx_des_sol.m_N_passes;

	C_MEQ_target_CO2_dP__L_tube_pass c_eq(mpc_ac,
		W_par, m_N_par,
		//m_P_hot_ave, 
        m_dot_tube,
		m_mu_air, m_v_air,
		m_cp_air, m_Pr_air,
		tol_L_tube, m_tol_pressure);

	C_monotonic_eq_solver c_solver(c_eq);

	c_solver.settings(tol_L_tube, 50, 0.001, std::numeric_limits<double>::quiet_NaN(), true);

	double L_tube_solved, tol_solved;
	L_tube_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(L_tube_guess, L_tube_guess*1.1, mpc_ac->ms_des_par_cycle_dep.m_delta_P_des,
			L_tube_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on tube length received exception from mono equation solver"));
	}
	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on tube length only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				tol_solved);
			mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			return -1;
			//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
		}
	}

	m_L_tube = L_tube_solved;		//[m]
	m_V_total = c_eq.m_V_total;		//[m3]
	m_h_conv_air = c_eq.m_h_conv_air;	//[W/m2-K]
	m_m_dot_air_total = c_eq.m_m_dot_air_total;	//[kg/s]
	m_A_surf_node = c_eq.m_A_surf_node;	//[m2]

	//*T_co2_hot = mpc_ac->mm_T_co2(mpc_ac->m_final_outlet_index, mpc_ac->ms_hx_des_sol.m_N_passes);		//[K]
	*T_co2_hot = c_eq.m_T_co2_in_calc;	//[K]

	return 0;
}

double C_CO2_to_air_cooler::calculate_cost(double UA /*kWt/K*/, double V_material /*m^3*/,
	double T_hot_in /*K*/, double P_hot_in /*kPa*/, double m_dot_hot /*kg/s*/)
{
	switch (m_cost_model)
	{
	case C_CO2_to_air_cooler::E_CARLSON_17:
		return 2.3*1.E-3*UA;		//[M$] needs UA in kWt/K
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

void C_CO2_to_air_cooler::calc_air_props(double T_amb /*K*/, double P_amb /*Pa*/,
	double & mu_air /*kg/m-s*/, double & v_air /*m3/kg*/, double & cp_air /*J/kg-K*/,
	double & k_air /*W/m-K*/, double & Pr_air)
{
	mu_air = mc_air.visc(T_amb);				//[kg/m-s] dynamic viscosity
	v_air = 1.0 / mc_air.dens(T_amb, P_amb);	//[m3/kg] specific volume
	cp_air = mc_air.Cp(T_amb)*1000.0;			//[J/kg-K] specific heat convert from kJ/kg-K
	k_air = mc_air.cond(T_amb);					//[W/m-K] conductivity
	Pr_air = (cp_air*mu_air / k_air);			//[-] Prandtl number
}

int co2_outlet_given_geom_and_air_m_dot(double T_co2_cold_out /*K*/, double m_dot_co2_tube /*kg/s*/,
    double P_cold_out /*kPa*/, double P_hot_in /*kPa*/,
    double T_amb /*K*/,
    double tol_h_in /*-*/, double tol_pressure /*-*/,
    C_csp_messages *mc_messages, CO2_state *co2_props,
    double d_in_tube /*m*/, double A_cs_tube /*m2*/, double relrough /*-*/,
    double L_node /*m*/, double V_node /*m3*/, int N_nodes /*-*/,
    double N_par /*-*/, int N_passes /*-*/,
    double alpha /*1/m*/, double cp_air /*J/kg-K*/,
    double m_dot_air_total /*kg/s*/, double h_conv_air /*W/m2-K*/,
    double & delta_P_co2_calc /*kPa*/, double & T_co2_in_calc /*K*/,
    double & q_dot_tube /*kWt*/)
{
    double T_co2_hot_max = 700.0 + 273.15;      //[K]
    double P_co2_ave = 0.5*(P_cold_out + P_hot_in); //[kPa]

    int prop_err_code = CO2_TP(T_co2_hot_max, P_co2_ave, co2_props);
    if (prop_err_code != 0)
    {
        return -1;
    }
    double h_co2_hot_max = co2_props->enth;     //[kJ/kg]

    prop_err_code = CO2_TP(T_co2_cold_out, P_cold_out, co2_props);
    if (prop_err_code != 0)
    {
        return -2;
    }
    double h_co2_cold_out = co2_props->enth;    //[kJ/kg]

    double A_surf_node = V_node * alpha;			//[m2] Air-side surface area of node
    double UA_node = A_surf_node * h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

    // Set known inlet conditions: iteration thru # of loops needs previous loop info
    util::matrix_t<double> mt_h_co2(N_nodes + 2, N_passes + 1, std::numeric_limits<double>::quiet_NaN());	//[K]
    util::matrix_t<double> mt_T_co2(N_nodes + 2, N_passes + 1, std::numeric_limits<double>::quiet_NaN());	//[K]
    util::matrix_t<double> mt_P_co2(N_nodes + 2, N_passes + 1, std::numeric_limits<double>::quiet_NaN());	//[kPa]
    util::matrix_t<double> mt_T_air(N_nodes + 2, N_passes + 1, std::numeric_limits<double>::quiet_NaN());	//[K]

    mt_h_co2(1, 0) = h_co2_cold_out;        //[kJ/kg]
    mt_T_co2(1, 0) = T_co2_cold_out;		//[K]
    mt_P_co2(1, 0) = P_cold_out;	//[kPa]
    for (int i = 1; i < N_nodes + 2; i++)
        mt_T_air(i, 0) = T_amb;		//[K]

    // Assuming constant air props, so can set those
    double m_dot_air_tube = m_dot_air_total / (N_par*N_nodes);
    double C_dot_air = cp_air * m_dot_air_tube;	//[W/K]

    for (int j = 1; j < N_passes + 1; j++)
    {
        // Set up constants and multipliers to switch direction of flow
        int mult_const = (j + 1) % 2;
        int constant = N_nodes + 2;
        int mult_index = 1 - 2 * mult_const;
        int out_const = mult_index;

        // Set inlet temperatures & pressures of current row
        int mult_inlet = (j + 1) % 2;
        int const_inlet = N_nodes;
        int inlet = mult_inlet * const_inlet + 1;

        // Set loop inlet conditions
        mt_h_co2((int)inlet, j) = mt_h_co2((int)inlet, j - 1);      //[kJ/kg]
        mt_T_co2((int)inlet, j) = mt_T_co2((int)inlet, j - 1);		//[K]
        mt_P_co2((int)inlet, j) = mt_P_co2((int)inlet, j - 1);		//[kPa]

        for (int i = 1; i < N_nodes + 1; i++)
        {
            int in = mult_const * constant + mult_index * i;
            int out = in + out_const;
            int air_in = fmin(in, out);

            // Get CO2 and Air inlet temperatures to node
            double i_h_co2_cold = mt_h_co2((size_t)in, j);			//[K]
            double T_air_cold_in_local = mt_T_air((size_t)air_in, j - 1);	//[K]

            double P_co2_out_guess = mt_P_co2((size_t)in, j);   //[kPa]
            double P_co2_out_calc = P_co2_out_guess;            //[kPa]

            bool is_iter_deltaP = true;
            size_t iter_deltaP = 0;

            double diff_P_co2_out = 0.0;

            do
            {
                if (iter_deltaP < 11)
                {
                    P_co2_out_guess = 0.9*P_co2_out_calc + 0.1*P_co2_out_guess;
                }
                else
                {
                    P_co2_out_guess = mt_P_co2((size_t)in, j);
                    is_iter_deltaP = false;
                }

                C_CO2_to_air_cooler::C_MEQ_node_energy_balance__h_co2_out c_node_e_bal_eq(co2_props,
                    i_h_co2_cold, 
                    mt_P_co2((size_t)in, j), P_co2_out_guess,
                    m_dot_co2_tube,
                    T_air_cold_in_local, C_dot_air,
                    UA_node);
                C_monotonic_eq_solver c_node_e_bal_solver(c_node_e_bal_eq);

                c_node_e_bal_solver.settings(tol_h_in, 50, i_h_co2_cold, h_co2_hot_max, false);

                double diff_h_co2_cold_calc = std::numeric_limits<double>::quiet_NaN();

                int diff_h_co2_cold_calc_code = c_node_e_bal_solver.test_member_function(h_co2_hot_max, &diff_h_co2_cold_calc);
                if (diff_h_co2_cold_calc_code != 0)
                {
                    throw(C_csp_exception("Air cooler UA calculation failed at maximum CO2 enthalpy"));
                }

                double h_co2_hot_solved = std::numeric_limits<double>::quiet_NaN();

                if (diff_h_co2_cold_calc >= 0.0)
                {
                    // Use monotonic equation solver to find T_co2_hot
                    double h_co2_hot_guess = i_h_co2_cold + 0.02;       //[K]
                    double h_co2_hot_guess2 = i_h_co2_cold + 1.5;       //[K]

                    double h_co2_hot_tol_solved = std::numeric_limits<double>::quiet_NaN();
                    int h_co2_hot_iter = -1;
                    int h_co2_hot_solve_code = 0;

                    try
                    {
                        h_co2_hot_solve_code = c_node_e_bal_solver.solve(h_co2_hot_guess, h_co2_hot_guess2, 0.0, h_co2_hot_solved, h_co2_hot_tol_solved, h_co2_hot_iter);
                    }
                    catch (C_csp_exception)
                    {
                        throw(C_csp_exception("Air cooler calculation to find h_co2_hot to solve node energy balance returned an exception"));
                    }

                    if (h_co2_hot_solve_code != C_monotonic_eq_solver::CONVERGED)
                    {
                        if (h_co2_hot_solve_code > C_monotonic_eq_solver::CONVERGED && fabs(h_co2_hot_tol_solved) <= 0.1)
                        {
                            std::string error_msg = util::format("Air cooler iteration on nodal energy balance only reached a convergence "
                                "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                                h_co2_hot_tol_solved);
                            mc_messages->add_message(C_csp_messages::WARNING, error_msg);
                        }
                        else
                        {
                            return -3;
                        }
                    }
                }
                else
                {
                    // CO2 is unrealistically hot. stop here before property routine gets weird
                    //    (although, generally, returning a unique value is useful...)
                    //    (which is why we let it get this hot instead of say using the target outlet temp)
                    h_co2_hot_solved = h_co2_hot_max;       //[kJ/kg]                
                }
            
                double Q_dot_node = c_node_e_bal_eq.m_Q_dot_node;   //[W]
                mt_h_co2((size_t)out, j) = h_co2_hot_solved;        //[kJ/kg]
                mt_T_co2((size_t)out, j) = c_node_e_bal_eq.m_T_co2_hot_in;     //[K]

                mt_T_air((size_t)air_in, j) = mt_T_air((size_t)air_in, j - 1) + Q_dot_node / C_dot_air;	//[K]

                // Add pressure drop calcs (co2_props is up-to-date for h_ave, P_ave)
                // ** Could also move this to a function if also called to guess length
                double visc_dyn_co2 = CO2_visc(co2_props->dens, co2_props->temp)*1.E-6;	//[Pa-s] convert from (uPa-s)
                double Re_co2 = m_dot_co2_tube * d_in_tube / (A_cs_tube*visc_dyn_co2);		//[-]

                double rho_co2 = co2_props->dens;				//[kg/s]
                double visc_kin_co2 = visc_dyn_co2 / rho_co2;	//[m2/s]
                double cond_co2 = CO2_cond(co2_props->dens, co2_props->temp);	//[W/m-K]
                double specheat_co2 = co2_props->cp*1000.0;		//[J/kg-K] convert from kJ/kg-K
                double alpha_co2 = cond_co2 / (specheat_co2*rho_co2);	//[m2/s]
                double Pr_co2 = visc_kin_co2 / alpha_co2;		//[-]

                double Nusselt_co2 = -999.9;	//[-]
                double f_co2 = -999.9;			//[-]

                // Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
                // CSP::PipeFlow(Re_co2, Pr_co2, 1000.0, m_relRough, Nusselt_co2, f_co2);
                CSP::PipeFlow(Re_co2, Pr_co2, L_node / d_in_tube, relrough, Nusselt_co2, f_co2);

                double u_m = m_dot_co2_tube / (rho_co2*A_cs_tube);		//[m/s]

                mt_P_co2((size_t)out, j) = mt_P_co2((size_t)in, j) + f_co2 * L_node*rho_co2*pow(u_m, 2) / (2.0*d_in_tube) / 1000.0;	//[kPa]

                double deltaP_node = mt_P_co2((size_t)out, j) - mt_P_co2((size_t)in, j);	//[kPa]

                mt_P_co2((size_t)out, j) = fmin(25000.0, fmax(1000.0, mt_P_co2((size_t)out, j)));   //[kPa]

                P_co2_out_calc = mt_P_co2((size_t)out, j);  //[kPa]

                diff_P_co2_out = P_co2_out_calc / P_co2_out_guess - 1.0;    //[-]
                iter_deltaP++;

            // Make sure this iterates at least once on pressure so that pressure moves through HX even if each node has small deltaP
            } while (is_iter_deltaP && (fabs(diff_P_co2_out) > tol_pressure || iter_deltaP < 2));
        }
    }

    // index that gives outlet temperatur and pressure: depends on whether odd or even loops
    int final_outlet_index = ((N_passes + 2) % 2)*N_nodes + 1;

    delta_P_co2_calc = mt_P_co2(final_outlet_index, N_passes) - mt_P_co2(1, 0);	//[kPa]
    T_co2_in_calc = mt_T_co2(final_outlet_index, N_passes);						//[K]
    double h_co2_in_calc = mt_h_co2(final_outlet_index, N_passes);
    q_dot_tube = m_dot_co2_tube * (h_co2_in_calc - h_co2_cold_out);     //[kWt]

    return 0;
}

int C_CO2_to_air_cooler::off_design_given_fan_power(double T_amb /*K*/, double T_hot_in /*K*/, double P_hot_in /*kPa*/,
    double m_dot_hot /*kg/s*/, double W_dot_fan_target /*MWe*/, double tol_od /*-*/, double tol_pressure /*-*/,
    double & T_co2_out /*K*/, double & P_co2_out /*kPa*/)
{
    C_MEQ_od__T_co2_out__fan_power c_T_co2_out_od(this, m_dot_hot, T_hot_in, P_hot_in, T_amb, tol_od, tol_pressure);
    
    C_monotonic_eq_solver c_T_co2_out_od_solver(c_T_co2_out_od);

    // Lower limit is ambient air temperature
    double T_co2_cold_lower = T_amb + 0.01;     //[K]
    // Upper limit is T_hot_in
    double T_co2_cold_upper = T_hot_in - 0.01;  //[K]

    c_T_co2_out_od_solver.settings(tol_od, 50, T_co2_cold_lower, T_co2_cold_upper, true);

    double T_co2_cold_guess = T_amb + (ms_hx_des_sol.m_T_out_co2 - ms_des_par_ind.m_T_amb_des);    //[K]
    double W_dot_fan_calc_guess = std::numeric_limits<double>::quiet_NaN();
    int err_code = c_T_co2_out_od_solver.test_member_function(T_co2_cold_guess, &W_dot_fan_calc_guess);
    if (err_code != 0)
        return -1;

    // Want to find a T_co2_cold where fan power is more than target but not by more then 2x
    C_monotonic_eq_solver::S_xy_pair xy1;
    C_monotonic_eq_solver::S_xy_pair xy2;
    if (W_dot_fan_calc_guess < W_dot_fan_target)
    {
        while (W_dot_fan_calc_guess < W_dot_fan_target)
        {
            if (T_co2_cold_guess == T_co2_cold_lower)
                return -2;
            
            xy1.x = T_co2_cold_guess;
            xy1.y = W_dot_fan_calc_guess;
            T_co2_cold_guess = fmax(T_co2_cold_lower, T_co2_cold_guess - 1.0);
            
            err_code = c_T_co2_out_od_solver.test_member_function(T_co2_cold_guess, &W_dot_fan_calc_guess);
            if (err_code != 0)
                return -3;
        }
        xy2.x = T_co2_cold_guess;
        xy2.y = W_dot_fan_calc_guess;
    }
    else if (W_dot_fan_calc_guess / W_dot_fan_target > 2.0)
    {
        while (W_dot_fan_calc_guess / W_dot_fan_target > 2.0)
        {
            if (T_co2_cold_guess == T_co2_cold_upper)
                return -4;

            xy1.x = T_co2_cold_guess;
            xy2.y = W_dot_fan_calc_guess;
            T_co2_cold_guess = fmin(T_co2_cold_upper, T_co2_cold_guess + 1.0);

            err_code = c_T_co2_out_od_solver.test_member_function(T_co2_cold_guess, &W_dot_fan_calc_guess);
            if (err_code != 0)
                return -5;
        }
        xy2.x = T_co2_cold_guess;
        xy2.y = W_dot_fan_calc_guess;
    }
    else
    {   // Calculated fan power is more than target but less than 2x target
        xy1.x = T_co2_cold_guess;
        xy2.y = W_dot_fan_calc_guess;
        T_co2_cold_guess = fmin(T_co2_cold_upper, T_co2_cold_guess + 1.0);

        err_code = c_T_co2_out_od_solver.test_member_function(T_co2_cold_guess, &W_dot_fan_calc_guess);
        if (err_code != 0)
            return -6;
    
        xy2.x = T_co2_cold_guess;
        xy2.y = W_dot_fan_calc_guess;
    }

    // Now solve for the outlet temperature corresponding to the target fan power
    double T_co2_out_solved, tol_solved;
    T_co2_out_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
    int iter_solved = -1;

    int T_co2_out_code = 0;
    try
    {
        T_co2_out_code = c_T_co2_out_od_solver.solve(xy1, xy2,
            W_dot_fan_target, T_co2_out_solved, tol_solved, iter_solved);
    }
    catch (C_csp_exception)
    {
        return -1;
    }

    if (T_co2_out_code != C_monotonic_eq_solver::CONVERGED)
    {
        if (!(T_co2_out_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1))
        {
            return -2;
        }
    }

    return 0;
}

int C_CO2_to_air_cooler::off_design_given_T_out(double T_amb /*K*/, double T_hot_in /*K*/, double P_hot_in /*kPa*/,
	double m_dot_hot /*kg/s*/, double T_hot_out /*K*/, double tol_m_dot /*-*/, double tol_m_pressure /*-*/,
    double & W_dot_fan /*MWe*/, double & P_hot_out /*kPa*/)
{
	// Want to iterate over *air* mass flow rate until T_co2_out is = T_hot_out
	if (T_hot_out <= T_amb)
	{
		return -1;
	}

	double P_amb = air_pressure(ms_des_par_ind.m_elev);

	// Assume air props don't change significantly in air cooler
	double mu_air = std::numeric_limits<double>::quiet_NaN();      //[kg/m-s] dynamic viscosity
	double v_air = std::numeric_limits<double>::quiet_NaN();	   //[m3/kg] specific volume
	double cp_air = std::numeric_limits<double>::quiet_NaN();	   //[J/kg-K] specific heat convert from kJ/kg-K
	double k_air = std::numeric_limits<double>::quiet_NaN();	   //[W/m-K] conductivity
	double Pr_air = std::numeric_limits<double>::quiet_NaN();	   //[-] Prandtl number

	calc_air_props(T_amb, P_amb,
		mu_air, v_air, cp_air, k_air, Pr_air);

	// Set up solver to find the air mass flow rate that achieves the target hot outlet temperature
	double deltaP_co2_od = ms_des_par_cycle_dep.m_delta_P_des;		//[kPa]
	double m_dot_hot_tube = m_dot_hot / ms_hx_des_sol.m_N_par;		//[kg/s]
	//double tol_m_dot = 1.E-4;		//[-]

	C_MEQ_od_air_mdot__T_co2_out c_m_dot_od(this, m_dot_hot_tube, T_hot_out,
		P_hot_in, T_amb, tol_m_dot, tol_m_pressure,
		mu_air, v_air, cp_air, k_air, Pr_air);

	C_monotonic_eq_solver c_m_dot_od_solver(c_m_dot_od);

	// Set lower bound
	double m_dot_air_lower = 1.E-10;		//[kg/s]
	double m_dot_air_upper = std::numeric_limits<double>::quiet_NaN();

	// Generate guess values
	double m_dot_air_guess1 = m_dot_hot/ms_des_par_cycle_dep.m_m_dot_total*m_m_dot_air_des;		//[kg/s]
	
	double T_hot_in_calc1 = 1000.0;
	int i_m_dot_guess = -1;

    // As air mass flow rate increases, T_hot_in_calc increases
	while (T_hot_in_calc1 >= 965.0 || !std::isfinite(T_hot_in_calc1))
	{
		i_m_dot_guess++;

		if (i_m_dot_guess > 0)
			m_dot_air_guess1 *= 0.75;

		if (i_m_dot_guess > 10)
			return -2;

		int solver_code = c_m_dot_od_solver.test_member_function(m_dot_air_guess1, &T_hot_in_calc1);
	}
	
	double m_dot_air_guess2 = 0.7*m_dot_air_guess1;	//[kg/s]
    double T_hot_in_calc2 = std::numeric_limits<double>::quiet_NaN();
    int solver_code = c_m_dot_od_solver.test_member_function(m_dot_air_guess2, &T_hot_in_calc2);

    while (T_hot_in_calc2 == T_hot_in_calc1 || !std::isfinite(T_hot_in_calc2))
    {
        m_dot_air_guess2 *= 1.25;

        int solver_code = c_m_dot_od_solver.test_member_function(m_dot_air_guess2, &T_hot_in_calc2);
    }

	c_m_dot_od_solver.settings(tol_m_dot, 50, m_dot_air_lower, m_dot_air_upper, true);

	// Now solve for air mass flow rate
	double m_dot_air_solved, tol_solved;
	m_dot_air_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_code = 0;
	try
	{
		m_dot_code = c_m_dot_od_solver.solve(m_dot_air_guess1, m_dot_air_guess2, T_hot_in, m_dot_air_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		return -1;
	}

	if (m_dot_code != C_monotonic_eq_solver::CONVERGED)
	{
		if( !(m_dot_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1) )
		{
			return -1;
		}
	}

    ms_od_solved.m_W_dot_fan = c_m_dot_od.m_W_dot_fan;	//[MWe]
    ms_od_solved.m_P_co2_cold = c_m_dot_od.m_P_co2_out;  //[kPa]
    ms_od_solved.m_T_co2_cold = T_hot_out;  //[K]
    ms_od_solved.m_q_dot = c_m_dot_od.m_q_dot_tube * ms_hx_des_sol.m_N_par * 1.E-3; //[MWt]

    W_dot_fan = c_m_dot_od.m_W_dot_fan;		//[MWe]
    P_hot_out = c_m_dot_od.m_P_co2_out;     //[kPa]

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_od__T_co2_out__fan_power::operator()(double T_co2_out /*K*/, double *W_dot_fan_calc /*MWe*/)
{
    m_P_co2_out_calc = std::numeric_limits<double>::quiet_NaN();

    int err_code = mpc_ac->off_design_given_T_out(m_T_amb, m_T_hot_in, m_P_hot_in, m_m_dot_co2_total, T_co2_out, m_tol_op, m_tol_pressure, *W_dot_fan_calc, m_P_co2_out_calc);

    return err_code;
}

int C_CO2_to_air_cooler::C_MEQ_od_air_mdot__T_co2_out::operator()(double m_dot_air /*kg/s*/, double *T_hot_out_calc /*K*/)
{
    m_W_dot_fan = m_P_co2_out = m_q_dot_tube = std::numeric_limits<double>::quiet_NaN();

    // Iterate to find air mass flow rate resulting in target fan power
	C_MEQ_target_W_dot_fan__m_dot_air c_m_dot_air_eq(mpc_ac->ms_hx_des_sol.m_L_tube, mpc_ac->ms_hx_des_sol.m_W_par, mpc_ac->ms_hx_des_sol.m_V_total,
		m_mu_air, m_v_air, m_cp_air, m_Pr_air,
		mpc_ac->m_sigma, mpc_ac->m_D_h,
		mpc_ac->m_enum_compact_hx_config,
		mpc_ac->m_alpha, mpc_ac->m_eta_fan);

	m_W_dot_fan = std::numeric_limits<double>::quiet_NaN();		//[MWe]
	int fan_error = c_m_dot_air_eq(m_dot_air, &m_W_dot_fan);
	if (fan_error != 0)
	{
		return -1;
	}

	double h_conv_air = c_m_dot_air_eq.m_h_conv_air;		//[W/m2-K]

    double P_hot_out_guess = m_P_hot_in - mpc_ac->ms_des_par_cycle_dep.m_delta_P_des;
    double P_hot_out_calc = P_hot_out_guess;
    double diff_P_hot_out = 0.0;

    int air_cooler_code = 0;
    size_t iter_deltaP = 0;

    bool is_deltaP_iter = true;

    do
    {
        if (iter_deltaP < 11)
        {
            P_hot_out_guess = 0.9*P_hot_out_calc + 0.1*P_hot_out_guess;
        }
        else
        {
            P_hot_out_guess = m_P_hot_in - mpc_ac->ms_des_par_cycle_dep.m_delta_P_des;
            is_deltaP_iter = false;
        }

        double deltaP_co2_in = m_P_hot_in - P_hot_out_guess;
        double deltaP_co2_calc = std::numeric_limits<double>::quiet_NaN();

        double P_eval = 0.5*(P_hot_out_guess + m_P_hot_in);

	    // Solve air cooler performance with known geometry and inputs
        m_q_dot_tube = std::numeric_limits<double>::quiet_NaN();
	    air_cooler_code = co2_outlet_given_geom_and_air_m_dot(m_T_hot_out, m_m_dot_hot_tube,
		    P_hot_out_guess, m_P_hot_in,
		    m_T_amb,
		    m_tol_op/2.0, m_tol_pressure,
		    &mpc_ac->mc_messages, &mpc_ac->mc_co2_props,
		    mpc_ac->ms_hx_des_sol.m_d_in, mpc_ac->m_A_cs, mpc_ac->m_relRough,
		    mpc_ac->ms_hx_des_sol.m_L_node, mpc_ac->ms_hx_des_sol.m_V_node, mpc_ac->m_N_nodes,
		    mpc_ac->ms_hx_des_sol.m_N_par, mpc_ac->ms_hx_des_sol.m_N_passes,
		    mpc_ac->m_alpha, m_cp_air,
		    m_dot_air, h_conv_air,
		    deltaP_co2_calc, *T_hot_out_calc, m_q_dot_tube);

        P_hot_out_calc = m_P_hot_in - deltaP_co2_calc;

        diff_P_hot_out = (P_hot_out_calc - P_hot_out_guess) / P_hot_out_guess;

        iter_deltaP++;

    } while (is_deltaP_iter && fabs(diff_P_hot_out) > m_tol_pressure);

    m_P_co2_out = P_hot_out_guess;

	return air_cooler_code;
}

double C_CO2_to_air_cooler::air_pressure(double elevation /*m*/)
{
	// http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html	
	return 101325.0*pow(1 - 2.25577E-5*elevation, 5.25588);	//[Pa] 
}
