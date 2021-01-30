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

#include "core.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"

#include "sco2_pc_csp_int.h"

#include "heat_exchangers.h"
#include "numeric_solvers.h"

// This compute module finds the optimal cycle design that meets the user-input design point cycle efficiency
//    and calculates the required recuperator UA

static var_info _cm_vtab_sco2_design_point[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                  UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",   "Design cycle power output",                              "MW",         "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_c",           "Design compressor(s) isentropic efficiency",             "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",    "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_PHX",      "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_ACC",      "Temp diff btw ambient air and compressor inlet",         "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",       "Design: Ambient temperature for air cooler",             "C",          "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",   "Tower design outlet temp",                               "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_des",         "Power cycle thermal efficiency",                         "",           "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "run_off_des_study", "1 = yes, 0/other = no",                                "",           "",    "",      "*",      "",               "" },
	{ SSC_INPUT,  SSC_ARRAY,   "part_load_fracs", "Array of part load q_dot_in fractions for off-design parametric", "",  "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_INPUT,  SSC_ARRAY,   "T_amb_array",     "Array of ambient temperatures for off-design parametric","C",          "",    "",      "run_off_des_study=1", "",  "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc","Calculated cycle thermal efficiency",                    "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_total",        "Total recuperator UA",                                   "kW/K",       "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",     "Recompression fraction",                                 "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",       "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",      "Compressor outlet pressure",                             "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold",      "Calculated cold HTF temp",                               "C",          "",    "",      "*",     "",                "" },

	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_fracs_out", "Array of part load fractions that SOLVED at off design", "-",      "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_eta",   "Matrix of power cycle efficiency results for q_dot_in part load", "-", "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_coefs", "Part load polynomial coefficients",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "part_load_r_squared", "Part load curve fit R squared",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_array_out", "Array of ambient temps that SOLVED at off design",       "C",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_eta",       "Matrix of ambient temps and power cycle efficiency",     "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_coefs",     "Part load polynomial coefficients",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_amb_r_squared", "T amb curve fit R squared",                              "-",          "",    "",      "run_off_des_study=1", "",  "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_design_point : public compute_module
{
public:

	cm_sco2_design_point()
	{
		add_var_info(_cm_vtab_sco2_design_point);
	}

	void exec() override
	{
		// Test CO2 air cooler class
		C_CO2_to_air_cooler c_ac;
		C_CO2_to_air_cooler::S_des_par_ind s_des_weather;
		s_des_weather.m_T_amb_des = 30.0 + 273.15;		//[K]
		s_des_weather.m_elev = 300.0;					//[m]
        s_des_weather.m_eta_fan = 0.5;
        s_des_weather.m_N_nodes_pass = 10;

		C_CO2_to_air_cooler::S_des_par_cycle_dep s_des_cycle;
		s_des_cycle.m_m_dot_total = 0.0;		//[kg/s] Use q_dot to design
		s_des_cycle.m_Q_dot_des = 10.0;			//[MWt]
		s_des_cycle.m_T_hot_in_des = 100.0 + 273.15;	//[K]
		s_des_cycle.m_P_hot_in_des = 8. * 1.E3;	//[kPa]
		s_des_cycle.m_delta_P_des = s_des_cycle.m_P_hot_in_des*0.005;	//[kPa]
		s_des_cycle.m_T_hot_out_des = 40.0 + 273.15;	//[K]
		s_des_cycle.m_W_dot_fan_des = 10 * 0.02;	//[MWe]

		c_ac.design_hx(s_des_weather, s_des_cycle, 1.E-3);

		double T_amb_od = c_ac.get_des_par_ind()->m_T_amb_des;		//[K]
		double P_amb_od = c_ac.get_design_solved()->m_P_amb_des;	//[Pa]
		double T_hot_in = c_ac.get_des_par_cycle_dep()->m_T_hot_in_des;		//[K]
		double P_hot_in = c_ac.get_des_par_cycle_dep()->m_P_hot_in_des;		//[kPa]
		double m_dot_hot = c_ac.get_des_par_cycle_dep()->m_m_dot_total;		//[kg/s]
		double T_hot_out = c_ac.get_des_par_cycle_dep()->m_T_hot_out_des;
		double W_dot_fan = std::numeric_limits<double>::quiet_NaN();	//[MWe]
        double P_hot_out = std::numeric_limits<double>::quiet_NaN();    //[kPa]
		int ac_od_code = -1;

        ac_od_code = c_ac.off_design_given_T_out(T_amb_od, T_hot_in, P_hot_in, m_dot_hot, T_hot_out, 1.E-4, 1.E-3, W_dot_fan, P_hot_out);

		// Test out multi-stage compressor model
		CO2_state co2_props;
		double P_in = 8000.0;		//[kPa]
		double T_in = 35 + 273.15;	//[K]
		int prop_err_code = CO2_TP(T_in, P_in, &co2_props);
		if (prop_err_code != 0)
		{
			return;
		}
		double h_in = co2_props.enth;
		double s_in = co2_props.entr;

		double P_out = 25000.0;		//[kPa]
		double s_out_isen = s_in;	//[kJ/kg-K]
		prop_err_code = CO2_PS(P_out, s_out_isen, &co2_props);
		if (prop_err_code != 0)
		{
			return;
		}
		double h_out_isen = co2_props.enth;

		double eta_isen = 0.9;
		double h_out = h_in + (h_out_isen - h_in) / eta_isen;
		prop_err_code = CO2_PH(P_out, h_out, &co2_props);
		if (prop_err_code != 0)
		{
			return;
		}
		double T_out = co2_props.temp;	//[K]

		/*C_compressor c_comp_old;
		C_compressor::S_design_parameters s_des_comp_old;
		s_des_comp_old.m_T_in = T_in;
		s_des_comp_old.m_P_in = P_in;
		s_des_comp_old.m_D_in = D_in;
		s_des_comp_old.m_h_in = h_in;
		s_des_comp_old.m_s_in = s_in;

		s_des_comp_old.m_T_out = T_out;
		s_des_comp_old.m_P_out = P_out;
		s_des_comp_old.m_h_out = h_out;
		s_des_comp_old.m_D_out = D_out;*/

		double m_dot_mc = 3000.0 / (h_out - h_in);	//[kg/s] mass flow for 3 MWe compressor
		//s_des_comp_old.m_m_dot = m_dot_mc;

		//c_comp_old.compressor_sizing(s_des_comp_old, comp_old_err_code);

		//double diameter_old = c_comp_old.get_design_solved()->m_D_rotor;	//[m]
		//double N_old = c_comp_old.get_design_solved()->m_N_design;			//[rpm]
		//double tip_ratio_old = c_comp_old.get_design_solved()->m_w_tip_ratio;	//[-]


		//C_recompressor::S_design_parameters s_rc_des_old;

		double T_rc_in = 100.0 + 273.15;	//[K]
		double P_rc_in = P_in;				//[kPa]
		
		CO2_TP(T_rc_in, P_rc_in, &co2_props);
		double s_rc_in = co2_props.entr;
		double h_rc_in = co2_props.enth;
		//s_rc_des_old.m_P_in = P_rc_in;
		//s_rc_des_old.m_D_in = co2_props.dens;
		//s_rc_des_old.m_h_in = co2_props.enth;
		//s_rc_des_old.m_s_in = co2_props.entr;

		double P_rc_out = P_out;			//[kPa]
		CO2_PS(P_rc_out, s_rc_in, &co2_props);
		double h_rc_out_isen = co2_props.enth;	//[kJ/kg]

		double h_rc_out = h_rc_in + (h_rc_out_isen - h_rc_in) / eta_isen;
		CO2_PH(P_rc_out, h_rc_out, &co2_props);
		//s_rc_des_old.m_T_out = co2_props.temp;
		//s_rc_des_old.m_P_out = co2_props.pres;
		//s_rc_des_old.m_h_out = co2_props.enth;
		//s_rc_des_old.m_D_out = co2_props.dens;
		double T_rc_out = co2_props.temp;		//[K]

		double m_dot_rc = m_dot_mc / (0.8) * 0.2;
		//s_rc_des_old.m_m_dot = m_dot_rc;

		//C_recompressor c_rc_old;
		//int rc_err_code = 0;
		//c_rc_old.recompressor_sizing(s_rc_des_old, rc_err_code);




		C_comp_multi_stage c_rc_ms;
		c_rc_ms.design_given_outlet_state(C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby, T_rc_in, P_rc_in, m_dot_rc, T_rc_out, P_rc_out, 1.E-3);



		double P_rc_in_od = 1.15*P_rc_in;
		double T_rc_in_od = T_rc_in + 10.0;
		double m_dot_rc_od = 0.90*m_dot_rc;

		int rc_od_err_code = 0;
		//c_rc_old.off_design_recompressor(T_rc_in_od, P_rc_in_od, m_dot_rc_od, P_rc_out, rc_od_err_code, T_rc_out_od);


		double T_rc_out_od_ms = std::numeric_limits<double>::quiet_NaN();
		c_rc_ms.off_design_given_P_out(T_rc_in_od, P_rc_in_od, m_dot_rc_od, P_rc_out, 1.E-3, rc_od_err_code, T_rc_out_od_ms);




		C_comp_multi_stage c_comp_ms;
		c_comp_ms.design_given_outlet_state(C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby, T_in, P_in, m_dot_mc, T_out, P_out, 1.E-3);

		double P_in_od = 1.15*P_in;
		double T_in_od = T_in + 5.0;
		double m_dot_od = 0.90*m_dot_mc;

		//c_comp_old.od_comp_at_N_des(T_in_od, P_in_od, m_dot_od, comp_old_err_code, T_out_od_old, P_out_od_old);
		//double P_out_od_old = std::numeric_limits<double>::quiet_NaN();
		//double T_out_od_old = std::numeric_limits<double>::quiet_NaN();

		double P_out_od_new = std::numeric_limits<double>::quiet_NaN();
		double T_out_od_new = std::numeric_limits<double>::quiet_NaN();

		int comp_new_err_code = 0;
		c_comp_ms.off_design_at_N_des(T_in_od, P_in_od, m_dot_od, comp_new_err_code, T_out_od_new, P_out_od_new);



		// Hot sCO2 to water heat exchanger

		double W_dot_net = 10.0*1.E3;	//[KWe]
		double eta = 0.492;				//[-]
		double q_dot_reject = W_dot_net / eta - W_dot_net;	//[kWt]

		double P_co2 = 7.661*1.E3;		//[kPa]
		double T_co2_hot = 45.0;		//[C]
		double T_co2_cold = 32.0;		//[C]
		double m_dot_co2_full_q_dot = 55.8065;	//[kg/s]
		bool is_partial_med = true;

		double T_water_cold = 17.0;		//[C] Groundwater temperature
		double T_water_hot = 39.0;		//[C] Groundwater outlet
		double x_water = -1;			//[-]

		//CO2_state co2_props;
		prop_err_code = CO2_TP(T_co2_hot+273.15, P_co2, &co2_props);
		if (prop_err_code != 0)
		{
			log("CO2 hot props failed", SSC_ERROR, -1.0);
			return;
		}
		double h_co2_hot = co2_props.enth;	//[kJ/kg]

		prop_err_code = CO2_TP(T_co2_cold + 273.15, P_co2, &co2_props);
		if (prop_err_code != 0)
		{
			log("CO2 cold props failed", SSC_ERROR, -1.0);
			return;
		}
		double h_co2_cold = co2_props.enth;	//[kJ/kg]
		
		double m_dot_co2 = std::numeric_limits<double>::quiet_NaN();
		if (!is_partial_med)
		{
			m_dot_co2 = (q_dot_reject) / (h_co2_hot - h_co2_cold);
		}
		else
		{
			m_dot_co2 = m_dot_co2_full_q_dot;
			q_dot_reject = m_dot_co2*(h_co2_hot - h_co2_cold);
		}		

		water_state water_props;
		double P_water = std::numeric_limits<double>::quiet_NaN();
		if (x_water > 0.0)
		{			
			prop_err_code = water_TQ(T_water_hot + 273.15, x_water, &water_props);
			if (prop_err_code != 0)
			{
				log("Water hot props failed at inlet", SSC_ERROR, -1.0);

				return;
			}
			P_water = water_props.pres;	//[kPa]
		}
		else
		{
			P_water = 101.0;			//[kPa]

			prop_err_code = water_TP(T_water_hot + 273.15, P_water, &water_props);
			if (prop_err_code != 0)
			{
				log("Water hot props failed at inlet", SSC_ERROR, -1.0);

				return;
			}
		}		
		double h_water_hot = water_props.enth;	//[kJ/kg]

		prop_err_code = water_TP(T_water_cold + 273.15, P_water, &water_props);
		if (prop_err_code != 0)
		{
			log("Water hot props failed at inlet", SSC_ERROR, -1.0);

			return;
		}
		double h_water_cold = water_props.enth;	//[kJ/kg]

		double m_dot_water = (q_dot_reject) / (h_water_hot - h_water_cold);

		// Test C_HX_counterflow model as a sCO2-water heat exchanger
		C_HX_counterflow_CRM mc_sco2_water_hx;
		C_HX_counterflow_CRM::S_init_par ms_hx_init;
		ms_hx_init.m_N_sub_hx = 20;
		ms_hx_init.m_hot_fl = NS_HX_counterflow_eqs::CO2;
		ms_hx_init.m_cold_fl = NS_HX_counterflow_eqs::WATER;
		// Initialize
		mc_sco2_water_hx.initialize(ms_hx_init);

        std::vector<NS_HX_counterflow_eqs::S_hx_node_info> v_s_node_info;

		double UA_cooler, min_DT_cooler, eff_cooler, NTU_cooler, h_co2_cold_calc, h_water_hot_calc, q_dot_reject_calc;
		try
		{
		mc_sco2_water_hx.calc_req_UA_enth(q_dot_reject, m_dot_water, m_dot_co2,
			h_water_cold, h_co2_hot, P_water, P_water, P_co2, P_co2,
			UA_cooler, min_DT_cooler, eff_cooler, NTU_cooler, h_co2_cold_calc, h_water_hot_calc, q_dot_reject_calc, v_s_node_info);
		}
		catch (C_csp_exception &csp_except)
		{
			//throw exec_error("sco2-water hx", "failed");
		}


		// Get user-defined parameters
		double W_dot_net_des = as_double("W_dot_net_des")*1.E3;
		double eta_c = as_double("eta_c");
		double eta_t = as_double("eta_t");
		double P_high_limit = as_double("P_high_limit")*1.E3;		//[kPa], convert from MPa
		double delta_T_t = as_double("deltaT_PHX");

		double delta_T_acc = as_double("deltaT_ACC");
		double T_amb_cycle_des = as_double("T_amb_des") + 273.15;

		double T_htf_hot = as_double("T_htf_hot_des") + 273.15;
		double eta_thermal_des = as_double("eta_des");

		// Define hardcoded sco2 design point parameters
		std::vector<double> DP_LT(2);
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		DP_LT[0] = 0;
		DP_LT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_HT(2);
		DP_HT[0] = 0;
		DP_HT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PC(2);
		DP_PC[0] = 0;
		DP_PC[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PHX(2);
		DP_PHX[0] = 0;
		DP_PHX[1] = 0;
		int N_sub_hxrs = 10;
		double N_t_des = 3600.0;
		double tol = 1.E-3;
		double opt_tol = 1.E-3;

		// Test C_HX_counterflow model as a sCO2-water heat exchanger
		//C_HX_counterflow mc_sco2_water_hx;
		//C_HX_counterflow::S_init_par ms_hx_init;
		//ms_hx_init.m_N_sub_hx = 20;
		//ms_hx_init.m_hot_fl = NS_HX_counterflow_eqs::CO2;
		//ms_hx_init.m_cold_fl = NS_HX_counterflow_eqs::WATER;
		//// Initialize
		//mc_sco2_water_hx.initialize(ms_hx_init);

		std::vector<double> v_P_water_in;		//[kPa]
		std::vector<double> v_T_sco2_cold;		//[C]

		double T_water_amb = 20.0 + 273.15;		//[C]

		double P_water_in = 50.0;				//[kPa]
		int iter_P_water_in = 0;

		while (P_water_in < 110.0)
		{
			P_water_in = 10.0 + 50.0*iter_P_water_in;

			double delta_T_pc = 5.0;				//[C]
			int iter_deltaT_pc = 0;

			while (true)
			{
				delta_T_pc = 5.0 + 1.0*iter_deltaT_pc;

				// For now, pick outlet conditions
				// CO2
				// in
				double P_co2_in = 9.4E3;		//[kPa]
				double T_co2_in = 128.1 + 273.15;	//[K]
				
				prop_err_code = CO2_TP(T_co2_in, P_co2_in, &co2_props);
				if (prop_err_code != 0)
				{
					log("CO2 props failed at inlet", SSC_ERROR, -1.0);

					return;
				}
				double h_co2_in = co2_props.enth;	//[kJ/kg]
				// out
				double P_co2_out = P_co2_in;		//[MPa]
				double T_co2_out = T_water_amb + delta_T_pc;	//[K]
				prop_err_code = CO2_TP(T_co2_out, P_co2_out, &co2_props);
				if (prop_err_code != 0)
				{
					log("CO2 props failed at outlet", SSC_ERROR, -1.0);

					return;
				}
				double h_co2_out = co2_props.enth;	//[kJ/kg]
				double q_dot_hx = 10.E3;		//[kWt]
				double m_dot_co2 = q_dot_hx / (h_co2_in - h_co2_out);
				// Water
				// in
				double T_water_in = T_water_amb;	//[K]
				
				prop_err_code = water_TP(T_water_in, P_water_in, &water_props);
				if (prop_err_code != 0)
				{
					log("Water props failed at inlet", SSC_ERROR, -1.0);

					return;
				}
				double h_water_in = water_props.enth;	//[kJ/kg]
				// out
				double P_water_out = P_water_in;		//[kPa]
				double x_water_out = 1.0;				//[-]
				prop_err_code = water_PQ(P_water_out, x_water_out, &water_props);
				if (prop_err_code != 0)
				{
					log("Water props failed at outlet", SSC_ERROR, -1.0);

					return;
				}
				double h_water_out = water_props.enth;	//[kJ/kg]
				double m_dot_water = q_dot_hx / (h_water_out - h_water_in);

				double UA_calc, min_DT_calc, eff_calc, NTU_calc, T_co2_out_calc, T_water_out_calc, q_dot_calc;
				UA_calc = min_DT_calc = eff_calc = NTU_calc = T_co2_out_calc = T_water_out_calc = q_dot_calc = std::numeric_limits<double>::quiet_NaN();
				try
				{
					mc_sco2_water_hx.calc_req_UA(q_dot_hx, m_dot_water, m_dot_co2,
						T_water_in, T_co2_in, P_water_in, P_water_out, P_co2_in, P_co2_out,
						UA_calc, min_DT_calc, eff_calc, NTU_calc, T_co2_out_calc, T_water_out_calc, q_dot_calc, v_s_node_info);
				}
				catch (C_csp_exception &csp_except)
				{
					iter_deltaT_pc++;
					continue;
				}

				break;
			}
			iter_P_water_in++;

		}

//		// Test C_HX_counterflow model as a sCO2 recuperator
//		C_HX_counterflow mc_sco2_recup;
//		C_HX_counterflow::S_init_par ms_recup_init;
//		ms_recup_init.m_N_sub_hx = 10;
//		ms_recup_init.m_hot_fl = C_HX_counterflow::CO2;
//		ms_recup_init.m_cold_fl = C_HX_counterflow::CO2;
//
//		// Initialize recuperator
//		mc_sco2_recup.initialize(ms_recup_init);
//		C_HX_counterflow::S_des_par recup_par;
//		recup_par.m_Q_dot_design = 53131.3;
//		recup_par.m_m_dot_cold_des = 213.59;
//		recup_par.m_m_dot_hot_des = 305.13;
//		recup_par.m_T_c_in = 331.78;
//		recup_par.m_T_h_in = 633.65;
//		recup_par.m_P_c_in = 12639.0;
//		recup_par.m_P_c_out = 12639.0;
//		recup_par.m_P_h_in = 9694.0;
//		recup_par.m_P_h_out = 9694.0;
//		C_HX_counterflow::S_des_solved recup_des_solved;
//		mc_sco2_recup.design_calc_UA(recup_par, recup_des_solved);
//
//		C_HX_co2_to_co2 c_co2_to_co2;
//		c_co2_to_co2.initialize(10);
//
//		C_HX_counterflow::S_des_solved co2_to_co2_des_solved;
//		c_co2_to_co2.design_calc_UA(recup_par, co2_to_co2_des_solved);
//
//		//*************************************************************
//		//*************************************************************
//		// Set up PHX model from Type 424
//		//*************************************************************
//		// First, need to know something about fluids
//		HTFProperties mc_htf;
//		mc_htf.SetFluid(HTFProperties::Salt_60_NaNO3_40_KNO3,true);
//		CO2_state mc_co2_props;
//
//		double T_t_in_des = T_htf_hot - delta_T_t;	//[K]
//		// For now, estimate T_PHX_co2_in - will get from design point cycle optimization in future
//		double T_PHX_co2_in = T_t_in_des - 200.0;	//[K]
//		double T_htf_cold = T_PHX_co2_in + delta_T_t;	//[K]
//
//		// Receiver/hot side
//		double h_htf_hot = mc_htf.enth_lookup(T_htf_hot);	//[kJ/kg]
//		double h_htf_cold = mc_htf.enth_lookup(T_htf_cold);	//[kJ/kg]
//		double eta_thermal = 0.5;		//[-] target power cycle efficiency
//		double q_dot_htf = (W_dot_net_des / eta_thermal);	//[kWt] target thermal power to power cycle
//		double m_dot_htf_des = q_dot_htf / (h_htf_hot - h_htf_cold);
//
//		// Because C_dot_c = C_dot_h, q_dot_max = 
//		double h_htf_cold_min = mc_htf.enth_lookup(T_PHX_co2_in);
//		double q_dot_htf_max = m_dot_htf_des*(h_htf_hot - h_htf_cold_min);		//[kWt]
//
//		// Effectiveness & NTU
//		double eff_des = q_dot_htf / q_dot_htf_max;
//		double NTU = eff_des / (1.0 - eff_des);
//
//		// UA estimate with bulk calculation
//		double UA_PHX_des = NTU*m_dot_htf_des*(h_htf_hot-h_htf_cold)/(T_htf_hot-T_htf_cold);
//
//		// Calculate CO2 mass flow rate for HX model
//		double P_CO2 = 20000.0;										//[kPa]
//		int co2_error = CO2_TP(T_t_in_des, P_CO2, &mc_co2_props);
//		double h_out = mc_co2_props.enth;
//		co2_error = CO2_TP(T_PHX_co2_in, P_CO2, &mc_co2_props);
//		double h_in = mc_co2_props.enth;
//		double m_dot_CO2 = (W_dot_net_des / eta_thermal) / (h_out - h_in);	//[kg/s]
//
//		C_HX_co2_to_htf mc_phx;
//		mc_phx.initialize(HTFProperties::Salt_60_NaNO3_40_KNO3);
//		double UA_PHX_calc = std::numeric_limits<double>::quiet_NaN();
//		double min_DT_calc = std::numeric_limits<double>::quiet_NaN();
//
//		// ****************************************************************************
//		// ****************************************************************************
//		// ****************************************************************************
//		C_HX_counterflow::S_des_par ms_phx_des_par;
//		ms_phx_des_par.m_Q_dot_design = q_dot_htf;
//		ms_phx_des_par.m_T_h_in = T_htf_hot;
//		ms_phx_des_par.m_P_h_in = 1.0;
//		ms_phx_des_par.m_P_h_out = 1.0;
//		ms_phx_des_par.m_m_dot_hot_des = m_dot_htf_des;
//		ms_phx_des_par.m_T_c_in = T_PHX_co2_in;
//		ms_phx_des_par.m_P_c_in = P_CO2;
//		ms_phx_des_par.m_P_c_out = P_CO2;
//		ms_phx_des_par.m_m_dot_cold_des = m_dot_CO2;
//
//		C_HX_counterflow::S_des_solved phx_des_solved;
//		
//		mc_phx.design_calc_UA(ms_phx_des_par, phx_des_solved);
//
//		double q_dot_od, T_c_out_od, T_h_out_od;
//		q_dot_od = T_c_out_od = T_h_out_od = std::numeric_limits<double>::quiet_NaN();
//
//		// Need to check what is happening when one mass flow rate decreases
//		// Solver *should* return a notice...
//
///*		mc_phx.od_performance(T_PHX_co2_in, P_CO2, 0.5*m_dot_CO2,
//			T_htf_hot, 1.0, m_dot_htf_des,
//			q_dot_od, T_c_out_od, T_h_out_od);
//*/
//		mc_phx.off_design_solution(T_PHX_co2_in, P_CO2, 0.5*m_dot_CO2, P_CO2,
//			T_htf_hot, 1.0, m_dot_htf_des, 1.0,
//			q_dot_od, T_c_out_od, T_h_out_od);

		//od_performance(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
		//	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
		//	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)

		//double q_dot_od, T_PHX_co2_out_od, T_htf_cold_od;
		//mc_phx.od_performance(T_PHX_co2_in, m_dot_CO2, T_htf_hot, m_dot_htf_des,
		//	q_dot_od, T_PHX_co2_out_od, T_htf_cold_od);

		// ****************************************************************************
		// ****************************************************************************
		// Test out monotonic function solver
		// ****************************************************************************

		/*HX_object da_solver;
		double result;
		int int_success = da_solver.myfunction(result);*/

		C_import_mono_eq ty_mono_eq(&test_mono_function);
		
		C_monotonic_eq_solver eq_solv(ty_mono_eq);

		double x_low = std::numeric_limits<double>::quiet_NaN();
		double x_high = std::numeric_limits<double>::quiet_NaN();
		int iter_limit = 50;
		eq_solv.settings(0.001, iter_limit, x_low, x_high, true);

		double x_solved, tol_solved;
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;
		double x_guess_1 = -100.0;
		double x_guess_2 = -99.0;
		double y_target = -2.0;
		eq_solv.solve(x_guess_1, x_guess_2, y_target, x_solved, tol_solved, iter_solved);

		// Initialize cycle here, so can use 'get_design_limits()'
			// Also define error and warning message strings
		std::string error_msg;
		int error_code = 0;
		C_RecompCycle rc_cycle;

		int run_off_des_study = as_integer("run_off_des_study");

		if( run_off_des_study == 1 && eta_thermal_des < 0.0 )
		{
			// Find optimal design at maximum allowable recuperator UA
			// This will result in the maximum allowable cycle efficiency
			// Subtract abs(eta_thermal_des) from max allowable efficiency to get "reasonable" efficiency to use in subsequent analysis

			C_RecompCycle::S_auto_opt_design_parameters rc_params_max_eta;

			rc_params_max_eta.m_W_dot_net = W_dot_net_des;					//[kW]
			rc_params_max_eta.m_T_mc_in = T_amb_cycle_des + delta_T_acc;	//[K]
			rc_params_max_eta.m_T_t_in = T_htf_hot - delta_T_t;				//[K]
			rc_params_max_eta.m_DP_LTR = DP_LT;
			rc_params_max_eta.m_DP_HTR = DP_HT;
			rc_params_max_eta.m_DP_PC_main = DP_PC;
			rc_params_max_eta.m_DP_PHX = DP_PHX;

			// Get max recuperator UA!!!
			rc_params_max_eta.m_UA_rec_total = rc_cycle.get_design_limits().m_UA_net_power_ratio_max*rc_params_max_eta.m_W_dot_net;		//[kW/K]

			rc_params_max_eta.m_eta_mc = eta_c;
			rc_params_max_eta.m_eta_rc = eta_c;
			rc_params_max_eta.m_eta_t = eta_t;
            rc_params_max_eta.m_LTR_N_sub_hxrs = N_sub_hxrs;
            rc_params_max_eta.m_HTR_N_sub_hxrs = N_sub_hxrs;
			rc_params_max_eta.m_P_high_limit = P_high_limit;
			rc_params_max_eta.m_des_tol = tol;
			rc_params_max_eta.m_des_opt_tol = opt_tol;
			rc_params_max_eta.m_N_turbine = N_t_des;

			error_code = rc_cycle.auto_opt_design(rc_params_max_eta);

			if(error_code != 0)
			{
				throw exec_error("sCO2 maximum efficiency calculations failed","");
			}

			// Get solved maximum cycle efficiency and subtract delta_eta to get "reasonable" efficiency
			eta_thermal_des = rc_cycle.get_design_solved()->m_eta_thermal - fabs(eta_thermal_des);		//[-]
		}		

		C_RecompCycle::S_auto_opt_design_hit_eta_parameters rc_params;
		rc_params.m_W_dot_net = W_dot_net_des;					//[kW]
		rc_params.m_eta_thermal = eta_thermal_des;
		rc_params.m_T_mc_in = T_amb_cycle_des + delta_T_acc;	//[K]
		rc_params.m_T_t_in = T_htf_hot - delta_T_t;				//[K]
		rc_params.m_DP_LT = DP_LT;
		rc_params.m_DP_HT = DP_HT;
		rc_params.m_DP_PC_main = DP_PC;
		rc_params.m_DP_PHX = DP_PHX;
		rc_params.m_eta_mc = eta_c;
		rc_params.m_eta_rc = eta_c;
		rc_params.m_eta_t = eta_t;
        rc_params.m_LTR_N_sub_hxrs = N_sub_hxrs;
        rc_params.m_HTR_N_sub_hxrs = N_sub_hxrs;
		rc_params.m_P_high_limit = P_high_limit;
		rc_params.m_des_tol = tol;
		rc_params.m_des_opt_tol = opt_tol;
		rc_params.m_N_turbine = N_t_des;
		
		C_sco2_phx_air_cooler::S_des_par sco2_rc_des_par;
		double elevation = 300.0;		//[m] Elevation
			// System design parameters
		sco2_rc_des_par.m_hot_fl_code = HTFProperties::Salt_60_NaNO3_40_KNO3;
		sco2_rc_des_par.m_T_htf_hot_in = T_htf_hot;
		sco2_rc_des_par.m_phx_dt_hot_approach = delta_T_t;
		sco2_rc_des_par.m_T_amb_des = T_amb_cycle_des;
		sco2_rc_des_par.m_dt_mc_approach = delta_T_acc;
		sco2_rc_des_par.m_elevation = elevation;
		sco2_rc_des_par.m_W_dot_net = W_dot_net_des;
		sco2_rc_des_par.m_eta_thermal = eta_thermal_des;
		sco2_rc_des_par.m_is_recomp_ok = 1;
			// Cycle design parameters
		sco2_rc_des_par.m_DP_LT = DP_LT;
		sco2_rc_des_par.m_DP_HT = DP_HT;
		sco2_rc_des_par.m_DP_PC = DP_PC;
		sco2_rc_des_par.m_DP_PHX = DP_PHX;
		sco2_rc_des_par.m_eta_mc = eta_c;
		sco2_rc_des_par.m_eta_rc = eta_c;
		sco2_rc_des_par.m_eta_t = eta_t;
        sco2_rc_des_par.m_LTR_N_sub_hxrs = N_sub_hxrs;
        sco2_rc_des_par.m_HTR_N_sub_hxrs = N_sub_hxrs;
		sco2_rc_des_par.m_P_high_limit = P_high_limit;
		sco2_rc_des_par.m_des_tol = tol;
		sco2_rc_des_par.m_des_opt_tol = opt_tol;
		sco2_rc_des_par.m_N_turbine = N_t_des;
			// PHX design parameters
		sco2_rc_des_par.m_phx_dt_cold_approach = delta_T_t;
        sco2_rc_des_par.m_phx_N_sub_hx = 10;
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = 0.01;
		sco2_rc_des_par.m_deltaP_cooler_frac = 0.002;

		// So, there are some useful outputs we probably want here...
		C_sco2_phx_air_cooler sco2_recomp_csp;
		sco2_recomp_csp.design(sco2_rc_des_par);
		double m_dot_htf = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		
		// Try calling off-design model with design parameters
		C_sco2_phx_air_cooler::S_od_par sco2_rc_od_par;
		sco2_rc_od_par.m_T_htf_hot = sco2_rc_des_par.m_T_htf_hot_in;
		sco2_rc_od_par.m_m_dot_htf = m_dot_htf;
		sco2_rc_od_par.m_T_amb = T_amb_cycle_des;

		error_code = rc_cycle.auto_opt_design_hit_eta(rc_params, error_msg);

		if(error_code != 0)
		{
			throw exec_error("sco2 design point calcs", error_msg);
		}

		double eta_thermal_calc = rc_cycle.get_design_solved()->m_eta_thermal;
		double UA_total = rc_cycle.get_design_solved()->m_UA_HTR + rc_cycle.get_design_solved()->m_UA_LTR;
		double recomp_frac = rc_cycle.get_design_solved()->m_recomp_frac;
		double P_comp_in = rc_cycle.get_design_solved()->m_pres[0] / 1.E3;
		double P_comp_out = rc_cycle.get_design_solved()->m_pres[1] / 1.E3;
		double T_htf_cold = rc_cycle.get_design_solved()->m_temp[5 - 1] + delta_T_t - 273.15;	//[C]

		// Assign SSC outputs
		assign("eta_thermal_calc", (ssc_number_t)eta_thermal_calc);
		assign("UA_total", (ssc_number_t)UA_total);
		assign("recomp_frac", (ssc_number_t)recomp_frac);
		assign("P_comp_in", (ssc_number_t)P_comp_in);
		assign("P_comp_out", (ssc_number_t)P_comp_out);
		assign("T_htf_cold", (ssc_number_t)T_htf_cold);

		if (error_msg == "")
			log("Design point optimization was successful!");
		else
		{
			log("The sCO2 design point optimization solved with the following warning(s):\n" + error_msg);
		}

		//int run_off_des_study = as_integer("run_off_des_study");

		//if( run_off_des_study != 1)
		//{
		//	return;
		//}
		//else	// Run off-design parametrics
		//{
		//	C_RecompCycle::S_opt_target_od_parameters opt_target_od_params;
		//	
		//	// opt_target_od_params.m_T_mc_in      ** Set compressor inlet temperature below **
		//	opt_target_od_params.m_T_t_in = rc_params.m_T_t_in;			//[K]
		//	
		//	// opt_target_od_params.m_target       ** Set target q_dot below **
		//	opt_target_od_params.m_is_target_Q = true;

		//	opt_target_od_params.m_N_sub_hxrs = rc_params.m_N_sub_hxrs;
		//	opt_target_od_params.m_lowest_pressure = 1000.0;
		//	opt_target_od_params.m_highest_pressure = 17000.0;

		//	opt_target_od_params.m_recomp_frac_guess = rc_cycle.get_design_solved()->m_recomp_frac;
		//	opt_target_od_params.m_fixed_recomp_frac = false;

		//	opt_target_od_params.m_N_mc_guess = rc_cycle.get_design_solved()->ms_mc_ms_des_solved.m_N_design;
		//	opt_target_od_params.m_fixed_N_mc = false;

		//	opt_target_od_params.m_N_t_guess = rc_cycle.get_design_solved()->ms_t_des_solved.m_N_design;
		//	opt_target_od_params.m_fixed_N_t = true;

		//	opt_target_od_params.m_tol = rc_params.m_tol;
		//	opt_target_od_params.m_opt_tol = rc_params.m_opt_tol;

		//	opt_target_od_params.m_use_default_res = false;

		//	double q_dot_in_des = rc_cycle.get_design_solved()->m_W_dot_net / rc_cycle.get_design_solved()->m_eta_thermal;	//[kWt]

		//	size_t n_f_pl = 0;
		//	ssc_number_t * f_pl = as_array("part_load_fracs", &n_f_pl);

		//	int n_solved = 0;

		//	std::vector<double> part_load_fracs_out(0);
		//	std::vector<double> part_load_eta(0);

		//	if(n_f_pl > 0)	// At least one part-load simulation is required
		//	{
		//		opt_target_od_params.m_T_mc_in = rc_params.m_T_mc_in;		//[K]

		//		for( size_t i = 0; i < n_f_pl; i++ )
		//		{
		//			opt_target_od_params.m_target = (double)f_pl[i] * q_dot_in_des;				//[kWt]
		//			log(util::format("Off design simulation at part load = %lg", f_pl[i]));
		//			int od_error_code = 0;
		//			rc_cycle.optimal_target_off_design(opt_target_od_params, od_error_code);
		//			if(od_error_code == 0)
		//			{
		//				part_load_fracs_out.push_back((double)f_pl[i]);
		//				part_load_eta.push_back(rc_cycle.get_od_solved()->m_eta_thermal/eta_thermal_calc);
		//			}
		//		}

		//		n_solved = (int)part_load_fracs_out.size();
		//	}
		//				
		//	ssc_number_t * f_pl_out = allocate("part_load_fracs_out", n_solved);
		//	ssc_number_t * eta_f_pl = allocate("part_load_eta", n_solved);

		//	for( int i = 0; i < n_solved; i++ )
		//	{
		//		f_pl_out[i] = (ssc_number_t)part_load_fracs_out[i];
		//		eta_f_pl[i] = (ssc_number_t)part_load_eta[i];
		//	}

		//	// Find and write polynomial coefficients for part load
		//	std::vector<double> pl_coefs;
		//	double pl_r_squared = std::numeric_limits<double>::quiet_NaN();
		//	bool pl_success = find_polynomial_coefs(part_load_fracs_out, part_load_eta, 5, pl_coefs, pl_r_squared);
		//	assign("Part_load_r_squared", (ssc_number_t)pl_r_squared);

		//	ssc_number_t * p_pl_coefs = allocate("part_load_coefs", 5);
		//	if(pl_success)
		//	{
		//		for( int i = 0; i < 5; i++ )
		//			p_pl_coefs[i] = (ssc_number_t)pl_coefs[i];
		//	}
		//	else
		//	{
		//		log("Part load coefficient calcuations failed");
		//		for( int i = 0; i < 5; i++ )
		//			p_pl_coefs[i] = 0.0;
		//	}
		//	// ********************************************

		//				
		//	size_t n_T_amb_od = 0;
		//	ssc_number_t * T_amb_od = as_array("T_amb_array", &n_T_amb_od);

		//	n_solved = 0;

		//	std::vector<double> T_amb_out(0);
		//	std::vector<double> T_amb_eta(0);

		//	if(n_T_amb_od > 0)	// At least one off-design ambient temperature simulation is required
		//	{
		//		opt_target_od_params.m_target = q_dot_in_des;

		//		for( size_t i = 0; i < n_T_amb_od; i++ )
		//		{
		//			opt_target_od_params.m_T_mc_in = max(rc_cycle.get_design_limits().m_T_mc_in_min, (double)T_amb_od[i] + delta_T_acc + 273.15);	//[K] convert from C and add air cooler deltaT
		//			log(util::format("Off design simulation at ambient temperature = %lg", T_amb_od[i]));
		//			log(util::format("Corresponding compressor inlet temperature = %lg", opt_target_od_params.m_T_mc_in - 273.15));
		//			int od_error_code = 0;
		//			rc_cycle.optimal_target_off_design(opt_target_od_params, od_error_code);
		//			if(od_error_code == 0)
		//			{
		//				T_amb_out.push_back((double)T_amb_od[i]);
		//				T_amb_eta.push_back(rc_cycle.get_od_solved()->m_eta_thermal / eta_thermal_calc);
		//			}
		//		}

		//		n_solved = (int)T_amb_out.size();
		//	}						

		//	ssc_number_t * T_amb_od_out = allocate("T_amb_array_out", n_solved);
		//	ssc_number_t * eta_T_amb = allocate("T_amb_eta", n_solved);

		//	for( int i = 0; i < n_solved; i++ )
		//	{
		//		T_amb_od_out[i] = (ssc_number_t)T_amb_out[i];
		//		eta_T_amb[i] = (ssc_number_t)T_amb_eta[i];
		//	}


		//	// Find polynomial coefficients for part load
		//	std::vector<double> T_amb_coefs;
		//	std::vector<double> T_amb_od_less_des;
		//	double T_amb_r_squared = std::numeric_limits<double>::quiet_NaN();

		//	T_amb_od_less_des.resize(n_solved);
		//	for( int i = 0; i < n_solved; i++ )
		//	{
		//		T_amb_od_less_des[i] = T_amb_od[i] - (T_amb_cycle_des - 273.15);
		//	}
		//	bool T_amb_success = find_polynomial_coefs(T_amb_od_less_des, T_amb_eta, 5, T_amb_coefs, T_amb_r_squared);
		//	assign("T_amb_r_squared", (ssc_number_t)T_amb_r_squared);

		//	ssc_number_t * p_T_amb_coefs = allocate("T_amb_coefs", 5);
		//	if( T_amb_success )
		//	{
		//		for( int i = 0; i < 5; i++ )
		//			p_T_amb_coefs[i] = (ssc_number_t)T_amb_coefs[i];
		//	}
		//	else
		//	{
		//		log("Ambient temperature coefficient calcuations failed");
		//		for( int i = 0; i < 5; i++ )
		//			p_T_amb_coefs[i] = 0.0;
		//	}

		//	// ******************************************************
		//}

	}


};

int test_mono_function(double x, double *y)
{
	*y = -(x*x);

	return 0;
}

DEFINE_MODULE_ENTRY(sco2_design_point, "Returns optimized sco2 cycle parameters given inputs", 0)
