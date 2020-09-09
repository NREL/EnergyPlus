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

#include "heat_exchangers.h"

// This compute module finds the optimal cycle design that meets the user-input design point cycle efficiency
//    and calculates the required recuperator UA

static var_info _cm_vtab_sco2_air_cooler[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                      UNITS  META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb",             "Ambient temperature at design",              "C",    "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "q_dot_reject",		"Heat rejected from CO2 stream",			  "MWt",  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "T_co2_hot_in",		"Hot temperature of CO2 at inlet to cooler",  "C",	  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "P_co2_hot_in",		"Pressure of CO2 at inlet to cooler",		  "MPa",  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP",			"Pressure drop of CO2 through cooler",		  "MPa",  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "T_co2_cold_out",	"Cold temperature of CO2 at cooler exit",	  "C",	  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_fan",			"Air fan power",							  "MWe",  "",  "", "*", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",	"Site elevation",							  "m",	  "",  "", "*", "", ""},
	
	{ SSC_OUTPUT, SSC_NUMBER,  "d_tube_out",        "CO2 tube outer diameter",                    "cm",   "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "d_tube_in",         "CO2 tube inner diameter",                    "cm",   "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "depth_footprint",   "Dimension of total air cooler in loop/air flow direction",  "m",  "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "width_footprint",   "Dimension of total air cooler of parallel loops",           "m",  "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "parallel_paths",    "Number of parallel flow paths",              "-",    "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "number_of_tubes",   "Number of tubes (one pass)",                 "-",    "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "length",            "Length of tube (one pass)",                  "m",    "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "n_passes_series",   "Number of serial tubes in flow path",        "-",    "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_total",          "Total air-side conductance",                 "kW/K", "",  "", "*", "", ""},
	{ SSC_OUTPUT, SSC_NUMBER,  "m_V_hx_material",   "Total hx material volume - no headers",      "m^3",  "",  "", "*", "", ""},
	

	var_info_invalid };

class cm_sco2_air_cooler : public compute_module
{
public:

	cm_sco2_air_cooler()
	{
		add_var_info(_cm_vtab_sco2_air_cooler);
	}
	
	void exec() override
	{
		C_CO2_to_air_cooler::S_des_par_cycle_dep s_air_cooler_des_par_cycle;
		C_CO2_to_air_cooler::S_des_par_ind s_air_cooler_des_par_ambient;

		s_air_cooler_des_par_ambient.m_T_amb_des = as_double("T_amb") + 273.15;		//[K]
		s_air_cooler_des_par_ambient.m_elev = as_double("site_elevation");			//[m]

		s_air_cooler_des_par_cycle.m_Q_dot_des = as_double("q_dot_reject");			//[MWt]
		s_air_cooler_des_par_cycle.m_T_hot_in_des = as_double("T_co2_hot_in") + 273.15;		//[K] convert from C
		s_air_cooler_des_par_cycle.m_P_hot_in_des = as_double("P_co2_hot_in")*1.E3;			//[MPa] convert from MPa
		s_air_cooler_des_par_cycle.m_T_hot_out_des = as_double("T_co2_cold_out") + 273.15;	//[K] convert from C
		s_air_cooler_des_par_cycle.m_delta_P_des = as_double("deltaP")*1.E3;				//[MPa] convert from MPa
		s_air_cooler_des_par_cycle.m_W_dot_fan_des = as_double("W_dot_fan");		//[MWe]

		C_CO2_to_air_cooler c_air_cooler;

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		try
		{
			c_air_cooler.design_hx(s_air_cooler_des_par_ambient, s_air_cooler_des_par_cycle);
		}
		catch (C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while (c_air_cooler.mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (c_air_cooler.mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg + "\n");
		}

		// Write outputs
		const C_CO2_to_air_cooler::S_des_solved *p_hx_des_sol;
		p_hx_des_sol = c_air_cooler.get_design_solved();

		assign("d_tube_out", (ssc_number_t)(p_hx_des_sol->m_d_out*1.E2));		//[cm] convert from m
		assign("d_tube_in", (ssc_number_t)(p_hx_des_sol->m_d_in*1.E2));			//[cm] convert from m
		assign("depth_footprint", (ssc_number_t)p_hx_des_sol->m_Depth);		//[m]
		assign("width_footprint", (ssc_number_t)p_hx_des_sol->m_W_par);		//[m]
		assign("parallel_paths", (ssc_number_t)p_hx_des_sol->m_N_par);		//[-]
		assign("number_of_tubes", (ssc_number_t)p_hx_des_sol->m_N_tubes);		//[-]
		assign("length", (ssc_number_t)p_hx_des_sol->m_L_tube);				//[m]
		assign("n_passes_series", (ssc_number_t)p_hx_des_sol->m_N_passes);	//[-]
		assign("UA_total", (ssc_number_t)(p_hx_des_sol->m_UA_total / 1.E3));		//[kW/K]
		assign("m_V_hx_material", (ssc_number_t)p_hx_des_sol->m_V_material_total);	//[m^3]
	}


};


DEFINE_MODULE_ENTRY(sco2_air_cooler, "Returns air cooler dimensions given fluid and location design points", 0)
