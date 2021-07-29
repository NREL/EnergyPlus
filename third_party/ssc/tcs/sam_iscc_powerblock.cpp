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

#define _TCSTYPEINTERFACE_
#include "ngcc_powerblock.h"
#include "tcstype.h"
#include "htf_props.h"
#include "sam_csp_util.h"

//#include "waterprop.h"
#include "water_properties.h"

using namespace std;

enum{	//Parameters
		P_HTF,
		P_USER_HTF_PROPS,
		P_Q_SF_DES,
		P_PLANT_ELEVATION,
		P_CYCLE_CONFIG,
		P_HOT_SIDE_DELTA_T,
		P_PINCH_POINT,

		//Inputs
		I_T_AMB,
		I_P_AMB,
		I_M_DOT_MS,
		I_Q_DOT_REC_SS,
		I_T_REC_IN, 
		I_T_REC_OUT,

		//Outputs
		O_T_HTF_COLD,
		O_T_HTF_HOT, 
		O_W_DOT_PC_FOSSIL,
		O_W_DOT_PC_HYBRID,   
		O_T_ST_COLD,
		O_T_ST_HOT, 
		O_P_ST_COLD,
		O_P_ST_HOT, 
		O_ETA_SOLAR_PC,
		O_Q_DOT_MAX,
		O_FUEL_USE,
		O_Q_DOT_FUEL,
		O_M_DOT_STEAM,

		//N_MAX
		N_MAX};

tcsvarinfo sam_iscc_powerblock_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_HTF,             "HTF_code",          "HTF fluid code",	                                    "-",     "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_USER_HTF_PROPS,  "field_fl_props",    "User defined field fluid property data",               "-",     "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},			
	{TCS_PARAM, TCS_NUMBER, P_Q_SF_DES,        "Q_sf_des",          "Design point solar field thermal output",              "MW",    "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PLANT_ELEVATION, "plant_elevation",   "Plant Elevation",                                      "m",     "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_CYCLE_CONFIG,    "cycle_config",      "Cycle configuration code, 1 = HP evap injection",      "-",     "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_HOT_SIDE_DELTA_T,"hot_side_delta_t",  "Hot side temperature HX temperature difference",       "C",     "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PINCH_POINT,     "pinch_point",       "Cold side HX pinch point",                             "C",     "", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,           "T_amb",             "Ambient temperature",                                  "C",     "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_P_AMB,           "P_amb",             "Ambient pressure",                                     "mbar",  "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_M_DOT_MS,        "m_dot_ms_ss",       "Molten salt mass flow rate from rec. - no startup derate", "kg/hr", "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_Q_DOT_REC_SS,    "q_dot_rec_ss",      "Receiver thermal output - no startup derate",          "MWt",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_REC_IN,        "T_rec_in",          "Receiver inlet temperature",                           "C",     "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_REC_OUT,       "T_rec_out",         "Receiver outlet temperature",                          "C",     "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_T_HTF_COLD,    "T_htf_cold",       "Outlet molten salt temp - inlet rec. temp",           "C",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_HTF_HOT,     "T_htf_hot",        "Inlet molten salt temp - outlet rec. temp",           "C",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PC_FOSSIL,"W_dot_pc_fossil", "POWER CYCLE output - no solar thermal input",         "MWe",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PC_HYBRID,"W_dot_pc_hybrid", "POWER CYCLE output at timestep with solar",           "MWe",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_ST_COLD,     "T_st_cold",        "Steam extraction temp TO molten salt HX",             "C",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_ST_HOT,      "T_st_hot",         "Steam injection temp TO ngcc",                        "C",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_ST_COLD,     "P_st_cold",        "Steam extraction pressure TO molten salt HX",         "bar",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_ST_HOT,      "P_st_hot",         "Steam extraction pressure TO ngcc",                   "bar",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_SOLAR_PC,  "eta_solar_pc",     "Solar use efficiency - no solar parasitics",          "-",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_DOT_MAX,     "Q_dot_max",        "Maximum allowable thermal power to power cycle",      "MWt",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_FUEL_USE,      "fuel_use",         "Total fossil fuel used during timestep",              "MMBTU", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_DOT_FUEL,    "q_dot_fuel",       "Fuel thermal power into gas turbines",                "kW",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_STEAM,   "m_dot_steam",      "Solar steam mass flow rate",                          "kg/hr", "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_iscc_powerblock : public tcstypeinterface
{
private:

	water_state wp;
	HTFProperties htfProps;			// Instance of HTFProperties class for receiver/HX htf
	ngcc_power_cycle cycle_calcs;

	double m_q_sf_des;
	double m_T_amb_des;
	double m_P_amb_des;

	double m_m_dot_st_des;
	double m_m_dot_ms_des;
	double m_UA_econo_des;
	double m_UA_sh_des;
	double m_UA_evap_des;
	double m_T_approach;
	double m_cp_ms;

	double m_T_ms_sh_in_des;    
	double m_T_ms_econo_out_des;
	double m_P_st_extract;
	double m_P_st_inject;
	double m_T_st_extract;
	double m_T_st_inject;

	double m_W_dot_pc_fossil;
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low; 
	double m_P_amb_high;
	double m_q_dot_rec_max;
	double m_plant_fuel_mass;
	double m_q_dot_fuel;

	// Better convergence
	bool m_T_lowflag_ncall;
	bool m_T_upflag_ncall;
	double m_T_low_ncall;
	double m_T_up_ncall;


public:
	sam_iscc_powerblock( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_q_sf_des = std::numeric_limits<double>::quiet_NaN();
		m_T_amb_des = std::numeric_limits<double>::quiet_NaN();
		m_P_amb_des = std::numeric_limits<double>::quiet_NaN();

		m_m_dot_st_des = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ms_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_econo_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_sh_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_evap_des = std::numeric_limits<double>::quiet_NaN();
		m_T_approach = std::numeric_limits<double>::quiet_NaN();
		m_cp_ms = std::numeric_limits<double>::quiet_NaN();

		m_T_ms_sh_in_des     = std::numeric_limits<double>::quiet_NaN();
		m_T_ms_econo_out_des = std::numeric_limits<double>::quiet_NaN();
		m_P_st_extract	     = std::numeric_limits<double>::quiet_NaN();
		m_P_st_inject	     = std::numeric_limits<double>::quiet_NaN();
		m_T_st_extract	     = std::numeric_limits<double>::quiet_NaN();
		m_T_st_inject	     = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_pc_fossil = std::numeric_limits<double>::quiet_NaN();
		m_T_amb_low = std::numeric_limits<double>::quiet_NaN();
		m_T_amb_high = std::numeric_limits<double>::quiet_NaN();
		m_P_amb_low = std::numeric_limits<double>::quiet_NaN();
		m_P_amb_high = std::numeric_limits<double>::quiet_NaN();

		m_T_lowflag_ncall = false;
		m_T_upflag_ncall = false;
		m_T_low_ncall = std::numeric_limits<double>::quiet_NaN();
		m_T_up_ncall = std::numeric_limits<double>::quiet_NaN();

		m_q_dot_rec_max = std::numeric_limits<double>::quiet_NaN();
		m_plant_fuel_mass = std::numeric_limits<double>::quiet_NaN();
		m_q_dot_fuel = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_iscc_powerblock()
	{
	}

	virtual int init()
	{
		// Initialize heat transfer fluid
		int field_fl	= (int) value(P_HTF);
		if( field_fl != HTFProperties::User_defined )
		{
			if( !htfProps.SetFluid( field_fl ) ) // field_fl should match up with the constants
			{
				message(TCS_ERROR, "Receiver HTF code is not recognized");
				return -1;
			}
		}
		else if( field_fl == HTFProperties::User_defined )
		{
			int nrows = 0, ncols = 0;
			double *htf_mat = value( P_USER_HTF_PROPS, &nrows, &ncols );
			if( htf_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat;
				mat.assign( htf_mat, nrows, ncols );
				if( !htfProps.SetUserDefinedFluid( mat ) )
				{
					message(TCS_ERROR,  htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR,  "The htf properties matrix must have more than 2 rows and exactly 7 columns - the input matrix has %d rows and %d columns", nrows, ncols );
				return -1;
			}
		}
		else
		{
			message(TCS_ERROR, "Receiver HTF code is not recognized");
			return -1;
		}

		// Set cycle configuration in class
		int cycle_config = (int)value(P_CYCLE_CONFIG);
		cycle_calcs.set_cycle_config(cycle_config);

		// Get table limits
		cycle_calcs.get_table_range(m_T_amb_low, m_T_amb_high, m_P_amb_low, m_P_amb_high);

		// Cycle design-point conditions
		m_q_sf_des = value( P_Q_SF_DES );		// [MWt]
		m_T_amb_des = 20.0;						// [C]
		m_P_amb_des = 101325.0*pow( 1 - 2.25577E-5*value(P_PLANT_ELEVATION), 5.25588 )/1.E5;	//[bar] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html						

		// Check ambient pressure
		if( m_P_amb_des < m_P_amb_low )
		{
			message(TCS_ERROR, "The design ambient pressure, %lg, [bar] is lower than the lowest value of ambient pressure, %lg [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_low);
			return -1;
		}
		if( m_P_amb_des > m_P_amb_high )
		{
			message(TCS_ERROR, "The design ambient pressure, %lg, [bar] is greater than the largest value of ambient pressure, %lg [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_high);
			return -1;
		}

		// Check design solar thermal input
		double q_dot_sf_max = cycle_calcs.get_ngcc_data(0.0, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_heat_max);				//[MWt]
		if( m_q_sf_des > q_dot_sf_max )
		{		
			message(TCS_ERROR, "The design solar thermal input, %lg MWt, is greater than the ngcc can accept, %lg MWt at the design ambient pressure, %lg bar, and designt ambient temperature"
				    "20 C. The HTF-steam HX was sized using the maximum solar thermal input.", m_q_sf_des, q_dot_sf_max, m_P_amb_des);
			m_q_sf_des = q_dot_sf_max;
		}

		// ********************************************************************************************************
		// Get Steam Pressure, Extraction, Injection, and mass flow rate at design solar input from Regression Model
		// m_m_dot_st_des = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_steam_mass );			// [kg/s]
		double P_st_extract = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_extraction_p )*100.0;	// [kPa] convert from [bar]
		double P_st_inject = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_injection_p )*100.0;	// [kPa] convert from [bar]
		double T_st_extract = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_extraction_t );		// [C]
		double T_st_inject = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_injection_t );			// [C]
		
		water_TP(T_st_extract + 273.15, P_st_extract, &wp);
		double h_st_extract = wp.enth;			// [kJ/kg]
		water_TP(T_st_inject + 273.15, P_st_inject, &wp);
		double h_st_inject = wp.enth;			// [kJ/kg]
		// double h_st_extract = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_extraction_h );		// [kJ/kg]
		// double h_st_inject = cycle_calcs.get_ngcc_data( m_q_sf_des, m_T_amb_des, m_P_amb_des, ngcc_power_cycle::E_solar_injection_h );			// [kJ/kg]
		m_m_dot_st_des = m_q_sf_des*1000.0 / (h_st_inject - h_st_extract);
		// ********************************************************************************************************

		m_P_st_extract	= P_st_extract;
		m_P_st_inject	= P_st_inject;
		m_T_st_extract	= T_st_extract;
		m_T_st_inject	= T_st_inject;

		// ********************************************************************************************************
		// For now, use estimates...
		// ********************************************************************************************************
		//double P_st_des = 10000.0;			//[kPa] Extraction/Injection at "full solar input"
		//water_PQ( P_st_des, 0.0, &wp );		// Steam props at design pressure and quality = 0
		//double T_st_des = wp.T;			//[C] Saturation temperature
		//double T_iscc_extraction = T_st_des - 15.0;		//[C] Extraction temperature FROM power cycle
		//double T_iscc_injection = T_st_des + 20.0;		//[C] Injection temperature TO power cycle
		//m_m_dot_st_des = 200.0;						//[kg/s] Steam mass flow rate extracted TO solar HX
		// ********************************************************************************************************

		// Calculate evaporator and superheater duty
		water_PQ( P_st_extract, 0.0, &wp );					// Steam props at design pressure and quality = 0
		double h_x0 = wp.enth;									//[kJ/kg] Steam enthalpy at evaporator inlet
		double cp_x0 = wp.cp;								//[kJ/kg-K] Thermal capacitance at evap inlet
		water_PQ( P_st_extract, 1.0, &wp );					// Steam props at design pressure and quality = 1
		double T_sat = wp.temp - 273.15;								// [C] Saturation temperature
		double h_x1 = wp.enth;									// [kJ/kg] Steam enthalpy at evaporator exit
		double cp_x1 = wp.cp;								//[kJ/kg-K] Thermal capacitance at evap outlet
		water_TP( T_st_inject + 273.15, P_st_inject, &wp );			// Steam props at superheater exit
		double h_sh_out = wp.enth;								//[kJ/kg] Steam enthalpy at sh exit
		double cp_sh_out =wp.cp;							//[kJ/kg-k] Thermal capacitance at sh exit
		water_TP(T_st_extract + 273.15, P_st_extract, &wp);		// Steam props at economizer inlet
		double h_econo_in = wp.enth;							//[kJ/kg] Steam enthalpy at econo inlet
		double cp_econo_in = wp.cp;							//[kJ/kg-K] Thermal capacitance at econo inlet
		double q_dot_econo = m_m_dot_st_des*(h_x0 - h_econo_in);				//[kW] design point duty of economizer
		double cp_st_econo = (cp_x0 + cp_econo_in)/2.0;							//[kJ/kg-K] Average thermal capacitance of steam in economizer
		double q_dot_evap = m_m_dot_st_des*(h_x1 - h_x0);					//[kW] design point duty of evaporator
		double q_dot_sh_des = m_m_dot_st_des*(h_sh_out - h_x1);					//[kW] design point duty of superheater
		double cp_st_sh = (cp_sh_out + cp_x1)/2.0;									//[kJ/kg-K] Average thermal capacitance of steam in superheater
		double q_dot_evap_and_sh = q_dot_evap + q_dot_sh_des;	//[kW] 
		// *********************************************************************************************************

		// Calculate MS temperatures
		double T_pinch_point = value(P_PINCH_POINT);			//[C] Get pinch point at design in evaporator
		//double T_pinch_point = 10.0;							//[C] Set pinch point at design in evaporator
		double T_ms_evap_out = T_sat + T_pinch_point;			//[C] Molten Salt evaporator outlet temperature
		m_T_approach = value(P_HOT_SIDE_DELTA_T);				//[C] Get molten salt approach temperature to superheater
		//m_T_approach = 30.0;									//[C] Set molten salt approach temperature to superheater 
		double T_ms_sh_in = T_st_inject + m_T_approach;			//[C] Molten salt superheater inlet temperature = receiver outlet temperature + approach temperature
			// m_dot_ms * cp_ms * delta_T_ms = m_dot_steam * delta_h_steam
		m_cp_ms = htfProps.Cp( (T_ms_evap_out + T_ms_sh_in)/2.0 );				//[kJ/kg-K] Specific heat of molten salt
		m_m_dot_ms_des = q_dot_evap_and_sh/(m_cp_ms*(T_ms_sh_in - T_ms_evap_out));	//[kg/s] Mass flow rate of molten salt
			// Economizer
			// m_dot_ms * cp_ms * (T_ms_evap_out - T_ms_econo_out) = q_dot_econo
		double T_ms_econo_out = T_ms_evap_out - q_dot_econo/(m_m_dot_ms_des*m_cp_ms);		//[C] Temperature of molten salt at outlet of economizer
			// Evaporator
			// m_dot_ms * cp_ms * (T_ms_evap_in - T_ms_evap_out) = q_dot_evap
		double T_ms_evap_in = q_dot_evap/(m_m_dot_ms_des*m_cp_ms) + T_ms_evap_out;			//[C] Temperature of molten salt inlet of evaporator

		// Calculate heat exchanger UA
		double C_dot_ms = m_m_dot_ms_des * m_cp_ms;											//[kW/K] Capacitance rate of molten salt in economizer
			// Economizer		
		double C_dot_st = m_m_dot_st_des * cp_st_econo;								//[kW/K] Capacitance rate of steam in economizer
		double C_dot_min = min( C_dot_ms, C_dot_st );								//[kJ/kg-K] Minimum capacitance rate of economizer
		double C_dot_max = max( C_dot_ms, C_dot_st );								//[kJ/kg-K] Maximum capacitance rate of economizer
		double q_dot_max = C_dot_min * (T_ms_evap_out - T_st_extract);			//[kW] Maximum possible heat transfer in economizer
		double epsilon_econo = q_dot_econo/q_dot_max;								//[-] Effectiveness of economizer
		double CR = C_dot_min/C_dot_max;											//[-] Capacitance ratio of econo.
		double NTU = log( (epsilon_econo - 1.0)/(epsilon_econo*CR - 1.0) )/(CR - 1.0);	//[-] NTU
		m_UA_econo_des = NTU * C_dot_min;											//[kW/K] Conductance
			// Superheater															
		double C_dot_st_sh_des = m_m_dot_st_des * cp_st_sh;										//[kW/K] Capacitance rate of steam in superheater
		double C_dot_min_sh_des = min( C_dot_ms, C_dot_st_sh_des );										//[kJ/kg-K] Minimum capacitance rate of superheater
		double C_dot_max_sh_des = max( C_dot_ms, C_dot_st_sh_des );										//[kJ/kg-K] Maximum capacitance rate of superheater
		double q_dot_max_sh_des = C_dot_min_sh_des * (T_ms_sh_in - T_sat);							//[kW] Maximum possible heat transfer in superheater
		double epsilon_sh_des = q_dot_sh_des/q_dot_max_sh_des;										//[-] Effectiveness of superheater
		double CR_sh_des = C_dot_min_sh_des/C_dot_max_sh_des;													//[-] Capacitance ratio of superheater
		double NTU_sh_des = log( (epsilon_sh_des - 1.0)/(epsilon_sh_des*CR_sh_des - 1.0) )/(CR_sh_des - 1.0);			//[-] NTU
		m_UA_sh_des = NTU_sh_des * C_dot_min_sh_des;										//[kW/K] Conductance of superheater
			// Evaporator
		C_dot_min = C_dot_ms;														//[kJ/kg-K] Minimum capacitance rate of evap
		q_dot_max = C_dot_min * (T_ms_evap_in - T_sat);							//[kW] Max possible heat transfer in evap
		double epsilon_evap = q_dot_evap/q_dot_max;									//[-] Effectiveness of evaporator
		NTU = -log(1 - epsilon_evap);												//[-] NTU
		m_UA_evap_des = NTU * C_dot_min;											//[kW/K] Conductance of evaporator

		m_T_ms_sh_in_des     = T_ms_sh_in;
		m_T_ms_econo_out_des = T_ms_econo_out;		

		return 0;
	}

	virtual int call( double /*time*/, double step, int ncall )
	{	
		// 1) Get inputs from receiver and weather reader
		double T_amb = value( I_T_AMB );					//[C] Ambient temperature
		double P_amb = value( I_P_AMB )/1000.0;				//[bar] Ambient pressure, convert from [mbar]
			// This input (m_dot_ms_rec, isn't used anywhere: is it necessary??)
		//double m_dot_ms_rec = value( I_M_DOT_MS )/3600.0;	//[kg/s] Molten salt mass flow rate from receiver, convert from [kg/hr]
		double q_dot_rec = value( I_Q_DOT_REC_SS )*1000.0;		//[kWt] Receiver thermal output, convert from [MWt]
		double T_rec_in_prev = value( I_T_REC_IN );			//[C] Receiver inlet molten salt temperature - used to solve previous call to tower model
		double T_rec_out = value( I_T_REC_OUT );		    //[C] Receiver outlet molten salt temperature - used to solve previous call to tower model

		//T_amb = max( m_T_amb_low, min( m_T_amb_high, T_amb ) );
		if( P_amb < m_P_amb_low || P_amb > m_P_amb_high )
		{
			message(TCS_NOTICE, "The design ambient pressure, %lg, is outside of the bounds"
				    "for ambient pressure (%lg, %lg) [bar] in the cycle performance lookup table and has been set to the appropriate bound"
					"for this timestep", m_P_amb_des, m_P_amb_low, m_P_amb_high);
			P_amb = max(m_P_amb_low, min(m_P_amb_high, P_amb));
		}
		

		// Get Basline output - only depends on weather
		if( ncall == 0 )
		{
			m_W_dot_pc_fossil = cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_plant_power_net );
			value( O_W_DOT_PC_FOSSIL, m_W_dot_pc_fossil );
			m_q_dot_rec_max = cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_solar_heat_max )*1000.0;	//[kWt] Convert from MWt
			value(O_Q_DOT_MAX, m_q_dot_rec_max/1.E3);
			double m_dot = cycle_calcs.get_ngcc_data(0.0, T_amb, P_amb, ngcc_power_cycle::E_plant_fuel_mass);				//[kg/s] Fuel mass flow rate
			m_plant_fuel_mass = m_dot*0.045556*step;	//[MMBTU] Fuel use over time period (LHV = 48065 kJ/kg per IPSEpro model)
			m_q_dot_fuel = m_dot * 48065;				//[kW] Fuel thermal power into system
			value(O_FUEL_USE, m_plant_fuel_mass);
			value(O_Q_DOT_FUEL, m_q_dot_fuel);
		}

		if( q_dot_rec == 0 )
		{
			// No solar load - so no need to iterate - get out and calculate ngcc performance at fossil-only conditions 

			value( O_T_HTF_COLD, T_rec_in_prev );
			value( O_T_HTF_HOT, T_rec_out );	
			value( O_W_DOT_PC_HYBRID, m_W_dot_pc_fossil );
			value( O_T_ST_COLD, cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_solar_extraction_t ) );
			value( O_T_ST_HOT, cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_solar_injection_t ) );
			value( O_P_ST_COLD, cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_solar_extraction_p ) );
			value( O_P_ST_HOT, cycle_calcs.get_ngcc_data( 0.0, T_amb, P_amb, ngcc_power_cycle::E_solar_injection_p ) );
			value( O_ETA_SOLAR_PC, 0.0 );
			value( O_M_DOT_STEAM );

			return 0;
		}
		else if( q_dot_rec > m_q_dot_rec_max )
		{
			message(TCS_NOTICE, "Solar thermal input from the receiver, %lg MWt, is greater than the allowable maximum, %lg MWt", q_dot_rec / 1.E3, m_q_dot_rec_max / 1.E3);
			q_dot_rec = m_q_dot_rec_max;						
		}


		// 2) Use regression curves to find steam state points in power cycle
				// Either iterate on steam mass flow rate until q_dot_steam = q_dot_ms
				// Or use 2nd version of regression data that is a function of q_dot_st instead of m_dot_st
		//double m_dot_st1 = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_steam_mass )*2.0;			// [kg/s]
		double P_st_extract = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_extraction_p )*100.0;	// [kPa] convert from [bar]
		double P_st_inject = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_injection_p )*100.0;		// [kPa] convert from [bar]
		double T_st_extract = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_extraction_t );			// [C]
		double T_st_inject = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_injection_t );			// [C]
		
		// double h_st_extract = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_extraction_h );			// [kJ/kg]
		// double h_st_inject = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_solar_injection_h );			// [kJ/kg]
		water_TP(T_st_extract + 273.15, P_st_extract, &wp);
		double h_st_extract = wp.enth;			// [kJ/kg]
		water_TP(T_st_inject + 273.15, P_st_inject, &wp);
		double h_st_inject = wp.enth;			// [kJ/kg]
		double m_dot_st = q_dot_rec / (h_st_inject - h_st_extract);
		
		// 3) Calculate remaining steam cycle state points
		double P_st_evap_in = P_st_extract;				//[kPa] Inlet pressure to evaporator
		double P_st_sh_in = P_st_extract;				//[kPa] Inlet pressure to superheater	
			// Superheater
		water_TP(T_st_inject + 273.15, P_st_inject, &wp);		// Water props at sh outlet
		double h_st_sh_out = wp.enth;						//[kJ/kg] Enthalpy at superheater outlet
		double cp_st_sh_out = wp.cp;					//[kJ/kg-K] Specific heat at superheater outlet
		water_PQ( P_st_sh_in, 1.0, &wp );				// Water props at sh inlet
		double h_st_sh_in = wp.enth;						//[kJ/kg] Enthalpy at superheater inlet
		double cp_st_sh_in = wp.cp;						//[kJ/kg-K] Specific heat at superheater inlet
		double T_st_sh_in = wp.temp - 273.15;						//[C] Temperature at superheater inlet
		double cp_st_sh = (cp_st_sh_in + cp_st_sh_out)/2.0;	//[kJ/kg-K] Average specific heat in superheater
		double C_dot_st_sh = cp_st_sh * m_dot_st;			//[kW/K] Capacitance rate of steam in superheater
		double q_dot_sh = m_dot_st*(h_st_sh_out - h_st_sh_in);			//[kW] Superheater heater duty
			// Economizer
		water_PQ( P_st_evap_in, 0.0, &wp );				// Water props at evap inlet
		double h_st_econo_out = wp.enth;					//[kJ/kg] Enthalpy at evaporator inlet
		double cp_st_econo_out = wp.cp;					//[kJ/kg-K]
		water_TP(T_st_extract + 273.15, P_st_extract, &wp);	// Water props at econo inlet
		double h_st_econo_in = wp.enth;					//[kJ/kg]
		double cp_st_econo_in = wp.cp;					//[kJ/kg-K]
		double cp_st_econo = (cp_st_econo_in+cp_st_econo_out)/2.0;	//[kJ/kg-K]
		double C_dot_st_econo = cp_st_econo*m_dot_st;				//[kW/K]
		double q_dot_econo = m_dot_st*(h_st_econo_out-h_st_econo_in);	//[kW]
			// Evaporator
		double q_dot_evap = m_dot_st*(h_st_sh_in - h_st_econo_out);		//[kW] Evaporator duty

		// **************************************
		// Guess T_ms_out
		// **************************************
		double T_ms_out_guess = T_st_sh_in;

		//bool T_lowflag = true;
		double T_lower = T_st_extract;				//[C]
		bool T_upflag = true;
		double T_upper = T_st_inject;				//[C]
		
		bool UAdiff_T_lowflag = false;
		double UAdiff_T_lower = std::numeric_limits<double>::quiet_NaN();	//[C]
		bool UAdiff_T_upflag = false;
		double UAdiff_T_upper = std::numeric_limits<double>::quiet_NaN();	//[C]
		
		bool pinch_point_break = false;
		
		int iter_UA = 0;
		double diff_UA = -999.9;	//[-]

		//double m_dot_ms = std::numeric_limits<double>::quiet_NaN();	//[C]
		
		// ***************************************
		// Calculate m_dot_ms
		// ***************************************
		while( fabs(diff_UA) > 0.0001 && iter_UA < 50 )
		{
			iter_UA++;

			if( iter_UA > 1 )
			{
				if( UAdiff_T_lowflag && UAdiff_T_upflag )
				{
					if( diff_UA > 0.0 )
					{
						T_lower = T_ms_out_guess;
						UAdiff_T_lower = diff_UA;
					}
					else
					{
						T_upper = T_ms_out_guess;
						UAdiff_T_upper = diff_UA;
					}
					T_ms_out_guess = UAdiff_T_upper/(UAdiff_T_upper-UAdiff_T_lower)*(T_lower-T_upper) + T_upper;		//[C] False position method
				}
				else if( pinch_point_break )
				{
					pinch_point_break = false;
					T_lower = T_ms_out_guess;
					T_ms_out_guess = 0.5*T_lower + 0.5*T_upper;		//[C] Biscetion method
				}
				else
				{
					if( diff_UA > 0.0 )					// Not enough UA for temperature difference -> increase inlet temperature
					{
						T_lower = T_ms_out_guess;
						UAdiff_T_lower = diff_UA;
						UAdiff_T_lowflag = true;
					}
					else								// Too much UA for temperature difference -> decrease inlet temperature
					{
						T_upper = T_ms_out_guess;
						T_upflag = true;
						UAdiff_T_upper = diff_UA;
						UAdiff_T_upflag = true;
					}
					if( UAdiff_T_lowflag && UAdiff_T_upflag )
						T_ms_out_guess = UAdiff_T_upper/(UAdiff_T_upper-UAdiff_T_lower)*(T_lower-T_upper) + T_upper;		//[C] False position method
					else
						T_ms_out_guess = 0.5*T_lower + 0.5*T_upper;		//[C] Biscetion method
				}
			}

			double m_dot_ms = q_dot_rec/((T_rec_out - T_ms_out_guess)*m_cp_ms);

			// *********************************************
			// *********** Calculate Off-design UAs
			// *********************************************
				// UA = UA_ref*( (m1^0.8 * m2^0.8)/(m1_ref^0.8 * m2_ref^0.8) )*( (m1_ref^0.8+m2_ref^0.8)/(m1^0.8+m2^0.8) )
			double UA_mult = ( (pow(m_dot_ms,0.8)*pow(m_dot_st,0.8))/(pow(m_m_dot_ms_des,0.8)*pow(m_m_dot_st_des,0.8)) )*( (pow(m_m_dot_ms_des,0.8)+pow(m_m_dot_st_des,0.8))/(pow(m_dot_ms,0.8)+pow(m_dot_st,0.8)));
			double UA_econo_phys = m_UA_econo_des*UA_mult;
			double UA_evap_phys = m_UA_evap_des*UA_mult;
			double UA_sh_phys = m_UA_sh_des*UA_mult;
			double UA_total_phys = UA_econo_phys + UA_evap_phys + UA_sh_phys;
			
			// *****************************************************
			// Calculate capacitance rates and HX duties
			// *****************************************************
			double C_dot_ms = m_dot_ms * m_cp_ms;							//[kW/K] Capacitance rate of molten salt
				// Superheater
			double C_dot_min_sh = min( C_dot_ms, C_dot_st_sh );				//[kW/K] Minimum capacitance rate in superheater
			double C_dot_max_sh = max( C_dot_ms, C_dot_st_sh );				//[kW/K] Maximum capacitance rate in superheater
			double CR_sh = C_dot_min_sh / C_dot_max_sh;						//[-] Capacitance ratio of superheater			
				// Economizer
			double C_dot_min_econo = min( C_dot_ms, C_dot_st_econo );		//[kW/K]
			double C_dot_max_econo = max( C_dot_ms, C_dot_st_econo );		//[kW/K]
			double CR_econo = C_dot_min_econo/C_dot_max_econo;				//[-]											

			// 7) Solve performance for HX train (assuming constant MS specific heat for all timesteps)	
				// Superheater performance
			double T_ms_sh_out = T_rec_out - q_dot_sh/(m_dot_ms*m_cp_ms);			//[C] Outlet temperature of superheater
			double q_dot_max_sh = C_dot_min_sh*(T_rec_out - T_st_sh_in);			//[kW] Maximum possible heat transfer in superheater
			double epsilon_sh = q_dot_sh/q_dot_max_sh;								//[-] Superheater effectiveness
			double NTU_sh = log( (epsilon_sh - 1.0)/(epsilon_sh*CR_sh - 1.0) )/(CR_sh - 1.0);		//[-] NTU
			double UA_sh_guess = NTU_sh * C_dot_min_sh;								//[kW/K] Conductance of superheater
				// Evaporator performance
			double T_ms_evap_out = T_ms_sh_out - q_dot_evap/(m_dot_ms*m_cp_ms);		//[C]
			if( T_ms_evap_out < T_st_sh_in )
			{
				T_lower = T_rec_out;			//[C] Set lower limit on ms inlet temp
				UAdiff_T_lower = false;				//[-] Don't have UA error for this
				pinch_point_break = true;
				continue;
			}
			double q_dot_max_evap = C_dot_ms*(T_ms_sh_out - T_st_sh_in);				//[kW] Maximum possible heat transfer in evap
			double epsilon_evap = q_dot_evap/q_dot_max_evap;							//[-] Effectiveness of evaporator
			double NTU_evap = -log(1 - epsilon_evap);									//[-] NTU of evaporator
			double UA_evap_guess = NTU_evap * C_dot_ms;									//[kW/K] Conductance of evaporator
				// Economizer performance
			//double T_ms_econo_out = T_ms_evap_out - q_dot_econo/(m_dot_ms*m_cp_ms);		//[C]
			double q_dot_max_econo = C_dot_min_econo*(T_ms_evap_out - T_st_extract);	//[kW]
			double epsilon_econo = q_dot_econo/q_dot_max_econo;							//[-]
			double NTU_econo = log( (epsilon_econo - 1.0)/(epsilon_econo*CR_econo - 1.0) )/(CR_econo - 1.0);		//[-] NTU
			double UA_econo_guess = NTU_econo * C_dot_min_econo;						//[kW/K]																																	
			
			double UA_total_guess = UA_sh_guess + UA_evap_guess + UA_econo_guess;		//[kW/K]
			diff_UA = (UA_total_guess - UA_total_phys)/UA_total_phys;			//[-]
		}

		if( T_ms_out_guess < T_rec_in_prev )
		{
			m_T_upflag_ncall = true;
			m_T_up_ncall = T_rec_in_prev; 
		}
		else
		{
			m_T_lowflag_ncall = true;
			m_T_low_ncall = T_rec_in_prev;
		}

		// MS inlet temp to receiver is only input to receiver model changing, so if successive substitution is not working,
		// bound temperature and use bisection method to converge
		if( ncall > 8 && m_T_upflag_ncall && m_T_lowflag_ncall )
			T_ms_out_guess = 0.5*m_T_up_ncall + 0.5*m_T_low_ncall;

		value( O_T_HTF_COLD, T_ms_out_guess );
		value( O_T_HTF_HOT, T_rec_out );

		double W_dot_pc_hybrid = cycle_calcs.get_ngcc_data( q_dot_rec/1000.0, T_amb, P_amb, ngcc_power_cycle::E_plant_power_net );

		value( O_W_DOT_PC_HYBRID, W_dot_pc_hybrid );

		value( O_T_ST_COLD, T_st_extract );
		value( O_T_ST_HOT, T_st_inject  );
		value( O_P_ST_COLD, P_st_extract  );
		value( O_P_ST_HOT, P_st_inject  );

		value( O_ETA_SOLAR_PC, (W_dot_pc_hybrid - m_W_dot_pc_fossil)/(q_dot_rec/1000.0) );

		value( O_M_DOT_STEAM, m_dot_st*3600.0 );

		return 0;

	}

	virtual int converged( double /*time*/ )
	{
		m_T_lowflag_ncall = false;
		m_T_upflag_ncall = false;

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_iscc_powerblock, "ISCC Powerblock ", "Ty Neises", 1, sam_iscc_powerblock_variables, NULL, 1 )

