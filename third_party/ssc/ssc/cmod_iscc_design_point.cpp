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

#include "ngcc_powerblock.h"
#include "water_properties.h"
#include "htf_props.h"

static var_info _cm_vtab_iscc_design_point[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "ngcc_model",         "1: NREL, 2: GE",                                 "",        "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "q_pb_design",        "Design point power block thermal power",         "MWt",     "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "pinch_point_cold",   "Cold side pinch point",                          "C",       "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "pinch_point_hot",    "Hot side pinch point",                           "C",       "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "elev",               "Plant elevation",                                "m",       "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HTF_code",           "HTF fluid code",                                 "-",       "",    "",      "*",     "",                ""  },
	{ SSC_INPUT,  SSC_MATRIX,  "field_fl_props",     "User defined field fluid property data",         "-",       "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },


	{ SSC_OUTPUT, SSC_NUMBER,  "W_dot_fossil",       "Electric output with no solar contribution",     "MWe",     "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_st_inject",        "Steam injection temp into HRSG",                 "C",       "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_solar_max",        "Max. solar thermal input at design",             "MWt",     "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold",         "HTF return temp from HRSG",                      "C",	      "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "W_dot_solar",        "Solar contribution to hybrid output",            "MWe",     "",    "",      "*",     "",                "" },


	var_info_invalid };

class cm_iscc_design_point : public compute_module
{
public:

	cm_iscc_design_point()
	{
		add_var_info(_cm_vtab_iscc_design_point);
	}

	void exec() override
	{
		HTFProperties htfProps;			// Instance of HTFProperties class for receiver/HX htf
		
		// Initialize heat transfer fluid
		int field_fl = as_integer("HTF_code");
		if( field_fl != HTFProperties::User_defined )
		{
			htfProps.SetFluid(field_fl); // field_fl should match up with the constants
		}
		else
		{
			size_t nrows = 0, ncols = 0;
			ssc_number_t *htf_mat = as_matrix("field_fl_props", &nrows, &ncols);
			if( htf_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<ssc_number_t> mat;
				mat.assign(htf_mat, nrows, ncols);

				util::matrix_t<double> mat_double(nrows, ncols);
				for( size_t i = 0; i < nrows; i++ )
				{
					for( size_t j = 0; j < ncols; j++ )
					{
						mat_double(i, j) = (double)mat(i, j);
					}
				}
				if( !htfProps.SetUserDefinedFluid(mat_double) )
				{
					throw exec_error("tcsmolten_salt", util::format("The user-defined HTF did not read correctly"));
					//message(TCS_ERROR, htfProps.UserFluidErrMessage(), nrows, ncols);
					//return -1;
				}
			}
			else
			{
				throw exec_error("tcsmolten_salt", util::format("The user-defined HTF did not load correctly"));
				//message(TCS_ERROR, "The htf properties matrix must have more than 2 rows and exactly 7 columns - the input matrix has %d rows and %d columns", nrows, ncols);
				//return -1;
			}
		}

		ngcc_power_cycle cycle_calcs;

		int cycle_config = as_integer("ngcc_model");
		cycle_calcs.set_cycle_config(cycle_config);

		// Get table limits
		double T_amb_low, T_amb_high, P_amb_low, P_amb_high;
		T_amb_low = T_amb_high = P_amb_low = P_amb_high = std::numeric_limits<double>::quiet_NaN();
		cycle_calcs.get_table_range(T_amb_low, T_amb_high, P_amb_low, P_amb_high);

		// Cycle design-point conditions
		double q_pb_des = as_double("q_pb_design");   // [MWt]
		double T_amb_des = 20.0;						// [C]
		double plant_elevation = as_double("elev");		// [m]
		double P_amb_des = 101325.0*pow(1 - 2.25577E-5*plant_elevation, 5.25588) / 1.E5;	//[bar] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html						

		// Check ambient pressure
		if( P_amb_des < P_amb_low )
		{
			P_amb_des = P_amb_low;
			//message(TCS_ERROR, "The design ambient pressure, %d, [bar] is lower than the lowest value of ambient pressure, %d [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_low);
			//return -1;
		}
		if( P_amb_des > P_amb_high )
		{
			P_amb_des = P_amb_high;			
			//message(TCS_ERROR, "The design ambient pressure, %d, [bar] is greater than the largest value of ambient pressure, %d [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_high);
			//return -1;
		}

		// Check design solar thermal input
		double q_pb_max = cycle_calcs.get_ngcc_data(0.0, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_heat_max);				//[MWt]
		if( q_pb_des > q_pb_max )
		{
			q_pb_des = q_pb_max;
			
			//message(TCS_ERROR, "The design solar thermal input, %d MWt, is greater than the ngcc can accept, %d MWt at the design ambient pressure, %d bar, and designt ambient temperature"
			//	"20 C. The HTF-steam HX was sized using the maximum solar thermal input.", m_q_sf_des, q_dot_sf_max, m_P_amb_des);
			//m_q_sf_des = q_dot_sf_max;
		}
		
		// ********************************************************************************************************
		// Get Steam Pressure, Extraction, Injection, and mass flow rate at design solar input from Regression Model
		//double m_dot_st_des2 = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_steam_mass);			// [kg/s]
		double P_st_extract = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_extraction_p)*100.0;	// [kPa] convert from [bar]
		double P_st_inject = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_injection_p)*100.0;	// [kPa] convert from [bar]
		double T_st_extract = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_extraction_t);		// [C]
		double T_st_inject = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_injection_t);			// [C]
		double W_dot_fossil = cycle_calcs.get_ngcc_data(0.0, T_amb_des, P_amb_des, ngcc_power_cycle::E_plant_power_net);
		double W_dot_hybrid = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_plant_power_net);
		double W_dot_solar = W_dot_hybrid - W_dot_fossil;
		// ********************************************************************************************************
		
		water_state wp;

		water_TP(T_st_extract + 273.15, P_st_extract, &wp);
		double h_st_extract = wp.enth;			// [kJ/kg]
		water_TP(T_st_inject + 273.15, P_st_inject, &wp);
		double h_st_inject = wp.enth;			// [kJ/kg]
		// double h_st_extract = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_extraction_h);		// [kJ/kg]
		// double h_st_inject = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_injection_h);			// [kJ/kg]
		double m_dot_st_des = q_pb_des*1000.0 / (h_st_inject - h_st_extract);

		// Calculate evaporator and superheater duty
		water_PQ(P_st_extract, 0.0, &wp);						// Steam props at design pressure and quality = 0
		double h_x0 = wp.enth;									//[kJ/kg] Steam enthalpy at evaporator inlet

		water_PQ(P_st_extract, 1.0, &wp);						// Steam props at design pressure and quality = 1
		double T_sat = wp.temp - 273.15;						// [C] Saturation temperature
		double h_x1 = wp.enth;									// [kJ/kg] Steam enthalpy at evaporator exit

		water_TP(T_st_inject + 273.15, P_st_inject, &wp);		// Steam props at superheater exit
		double h_sh_out = wp.enth;								//[kJ/kg] Steam enthalpy at sh exit

		water_TP(T_st_extract + 273.15, P_st_extract, &wp);		// Steam props at economizer inlet
		double h_econo_in = wp.enth;							//[kJ/kg] Steam enthalpy at econo inlet

		double q_dot_econo = m_dot_st_des*(h_x0 - h_econo_in);		//[kW] design point duty of economizer
		double q_dot_evap = m_dot_st_des*(h_x1 - h_x0);				//[kW] design point duty of evaporator
		double q_dot_sh_des = m_dot_st_des*(h_sh_out - h_x1);		//[kW] design point duty of superheater
		double q_dot_evap_and_sh = q_dot_evap + q_dot_sh_des;		//[kW]

		double T_pinch_point = as_double("pinch_point_cold");
		double T_ms_evap_out = T_sat + T_pinch_point;			//[C] Molten Salt evaporator outlet temperature

		double T_approach = as_double("pinch_point_hot");
		double T_ms_sh_in = T_st_inject + T_approach;

		double cp_ms = htfProps.Cp((T_ms_evap_out + T_ms_sh_in) / 2.0);				//[kJ/kg-K] Specific heat of molten salt
		double m_dot_ms_des = q_dot_evap_and_sh / (cp_ms*(T_ms_sh_in - T_ms_evap_out));	//[kg/s] Mass flow rate of molten salt

		double T_ms_econo_out = T_ms_evap_out - q_dot_econo / (m_dot_ms_des*cp_ms);		//[C] Temperature of molten salt at outlet of economizer

		// Return outputs
		assign("W_dot_fossil", (ssc_number_t)W_dot_fossil);
		assign("T_st_inject", (ssc_number_t)T_st_inject);
		assign("q_solar_max", (ssc_number_t)q_pb_max);
		assign("T_htf_cold", (ssc_number_t)T_ms_econo_out);
		assign("W_dot_solar", (ssc_number_t)W_dot_solar);
	}


};

DEFINE_MODULE_ENTRY(iscc_design_point, "Calculates design point inject, extraction, fossil output", 0)