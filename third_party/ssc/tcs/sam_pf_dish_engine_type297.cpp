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
#include "tcstype.h"
#include "sam_csp_util.h"

using namespace std;

enum{	//Parameters
		P_MANUFACTURER,
		P_T_HEATER_HEAD_HIGH,
		P_T_HEATER_HEAD_LOW, 
		P_BEALE_CONST_COEF,  
		P_BEALE_FIRST_COEF,  
		P_BEALE_SQUARE_COEF, 
		P_BEALE_THIRD_COEF,  
		P_BEALE_FOURTH_COEF, 
		P_PRESSURE_COEF,     
		P_PRESSURE_FIRST,    
		P_ENGINE_SPEED,      
		P_V_DISPLACED,       

		//Inputs         
		I_P_SE,                 
		I_T_AMB,                
		I_N_COLS,               
		I_T_COMPRESSION,                              
		I_T_HEATER_HEAD_OPERATE,         
		I_P_IN_COLLECTOR,       

		//Outputs                
		O_P_OUT_SE,          
		O_P_SE_LOSSES,       
		O_ETA_SE,                     
		O_T_HEATER_HEAD_LOW, 
		O_T_HEATER_HEAD_HIGH,
		O_V_DISPLACED,       
		O_FREQUENCY,         
		O_ENGINE_PRESSURE,   
		O_ETA_GROSS,         

		//N_MAX
		N_MAX};

tcsvarinfo sam_pf_dish_engine_type297_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_MANUFACTURER,          "manufacturer",       "Manufacturer (fixed as 5)",                    "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_HEATER_HEAD_HIGH,    "T_heater_head_high", "Heater Head Set Temperature",                  "K",   "", "", ""},   
	{TCS_PARAM, TCS_NUMBER, P_T_HEATER_HEAD_LOW,     "T_heater_head_low",  "Header Head Lowest Temperature",               "K",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BEALE_CONST_COEF,      "Beale_const_coef",   "Beale Constant Coefficient",                   "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BEALE_FIRST_COEF,      "Beale_first_coef",   "Beale first-order coefficient",                "1/W",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BEALE_SQUARE_COEF,     "Beale_square_coef",  "Beale second-order coefficient",               "1/W^2",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BEALE_THIRD_COEF,      "Beale_third_coef",   "Beale third-order coefficient",                "1/W^3",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BEALE_FOURTH_COEF,     "Beale_fourth_coef",  "Beale fourth-order coefficient",               "1/W^4",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PRESSURE_COEF,         "Pressure_coef",	   "Pressure constant coefficient",                "MPa",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PRESSURE_FIRST,        "Pressure_first",	   "Pressure first-order coefficient",             "MPa/W",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_ENGINE_SPEED,          "engine_speed",	   "Engine operating speed",                       "rpm",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_V_DISPLACED,           "V_displaced",		   "Displaced engine volume",                      "m3",   "", "", ""},

	// INPUTS
	{TCS_INPUT, TCS_NUMBER, I_P_SE,                  "P_SE",                      "Receiver output power",                        "kW", "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,                 "T_amb",					  "Ambient temperature in Kelvin",                "K", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_N_COLS,                "N_cols",					  "Number of collectors",                         "-", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_T_COMPRESSION,         "T_compression",			  "Receiver efficiency",                          "C", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_T_HEATER_HEAD_OPERATE, "T_heater_head_operate",	  "Receiver head operating temperature",          "K", "", "", ""},      
	{TCS_INPUT, TCS_NUMBER, I_P_IN_COLLECTOR,        "P_in_collector",			  "Power incident on the collector",              "kW", "", "", ""},    

	// OUTPUTS 
	{TCS_OUTPUT, TCS_NUMBER, O_P_OUT_SE,             "P_out_SE",                  "Stirling engine gross output",     "kW", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_SE_LOSSES,          "P_SE_losses",				  "Stirling engine losses",           "kW", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_SE,               "eta_SE",					  "Stirling engine efficiency",       "-", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_HEATER_HEAD_LOW,    "T_heater_head_low",		  "Header Head Lowest Temperature",   "K", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_HEATER_HEAD_HIGH,   "T_heater_head_high",		  "Heater Head Set Temperature",      "K", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_V_DISPLACED,          "V_displaced",				  "Displaced engine volume",          "cm^3", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_FREQUENCY,            "frequency",				  "Engine frequency (= RPM/60s)",     "1/s", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ENGINE_PRESSURE,      "engine_pressure",			  "Engine pressure",                  "Pa", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_GROSS,            "eta_gross",				  "Gross efficiency of the system",   "-", "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_pf_dish_engine_type297 : public tcstypeinterface
{
private:
	// Class Instances

	//Parameters
	int m_manufacturer;                 
	double m_T_heater_head_high;           
	double m_T_heater_head_low;            
	double m_Beale_const_coef;             
	double m_Beale_first_coef;             
	double m_Beale_square_coef;            
	double m_Beale_third_coef;             
	double m_Beale_fourth_coef;            
	double m_Pressure_coef;                
	double m_Pressure_first;               
	double m_engine_speed;                 
	double m_V_displaced;

	//Stored Variables

	// Calculated

public:
	sam_pf_dish_engine_type297( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_manufacturer = -1;      
		m_T_heater_head_high = std::numeric_limits<double>::quiet_NaN();
		m_T_heater_head_low = std::numeric_limits<double>::quiet_NaN(); 
		m_Beale_const_coef = std::numeric_limits<double>::quiet_NaN();  
		m_Beale_first_coef = std::numeric_limits<double>::quiet_NaN();  
		m_Beale_square_coef = std::numeric_limits<double>::quiet_NaN(); 
		m_Beale_third_coef = std::numeric_limits<double>::quiet_NaN();  
		m_Beale_fourth_coef = std::numeric_limits<double>::quiet_NaN(); 
		m_Pressure_coef = std::numeric_limits<double>::quiet_NaN();     
		m_Pressure_first = std::numeric_limits<double>::quiet_NaN();    
		m_engine_speed = std::numeric_limits<double>::quiet_NaN();      
		m_V_displaced = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_pf_dish_engine_type297()
	{
	}

	virtual int init()
	{
		m_manufacturer = (int) value( P_MANUFACTURER );

		switch( m_manufacturer )
		{
		case 1:							// SES System = 1
			m_T_heater_head_high = 993.0;                   
			m_T_heater_head_low = 973.0;           
			m_Beale_const_coef = 0.04247;            
			m_Beale_first_coef = 0.00001682;          
			m_Beale_square_coef = -5.105E-10;        
			m_Beale_third_coef = 7.0726E-15;                   
			m_Beale_fourth_coef = -3.586E-20;        
			m_Pressure_coef = 0.658769;       
			m_Pressure_first = 0.000234963;             
			m_engine_speed = 1800.0;         
			m_V_displaced = 380*0.000001;
			break;

		case 2:							// WGA System = 2
			m_T_heater_head_high=903;	
			m_T_heater_head_low=903;	
			m_Beale_const_coef= 0.0850686;      //   !0.103371  !-0.00182451  
			m_Beale_first_coef= 0.0000194116;   //     !0.0000184703     !0.0000260289
			m_Beale_square_coef=-3.18449E-10;   //    !-3.07798e-10    !-4.68164E-10 
			m_Pressure_coef= -0.736342;         // ! -0.412058  !-0.0200284
			m_Pressure_first= 0.00036416;       //   !0.000359699  !0.000352522
			m_engine_speed = 1800;              //  !rpm
			m_V_displaced = 160 * 0.000001;     //     !convert(cm^3, m^3)
			m_Beale_third_coef=0;
			m_Beale_fourth_coef=0;    
			break;

		case 3:							// SBP System = 3
			m_T_heater_head_high=903;	
			m_T_heater_head_low=903;	
			m_Beale_const_coef= -0.00182451;
			m_Beale_first_coef=0.0000260289;
			m_Beale_square_coef=-4.68164E-10;
			m_Pressure_coef=-0.0200284;
			m_Pressure_first=0.000352522;
			m_engine_speed = 1800;                    //  !rpm  
			//actually 1500rpm but would need new curve...data allows this curve to be accurate
			//heat xfer will be worse based on 1500rpm
			m_V_displaced = 160 * 0.000001;    // !convert(cm^3, m^3)
			m_Beale_third_coef=0;
			m_Beale_fourth_coef=0;      
			break;

		case 4:							// SAIC System = 4
			m_T_heater_head_high=993;	
			m_T_heater_head_low=973;
			m_Beale_const_coef=-0.016;
			m_Beale_first_coef=0.000015;
			m_Beale_square_coef=-3.50E-10;
			m_Beale_third_coef=3.85E-15;
			m_Beale_fourth_coef=-1.6E-20;
			m_Pressure_coef=0.0000347944;
			m_Pressure_first=5.26329E-9;
			m_engine_speed = 2200;                 //  !rpm
			// m_V_displaced = 480 * 0.000001;        //  This varies for SAIC.....see below
			break;

		case 5:
			m_T_heater_head_high = value( P_T_HEATER_HEAD_HIGH );
			m_T_heater_head_low = value( P_T_HEATER_HEAD_LOW );
			m_Beale_const_coef = value( P_BEALE_CONST_COEF );
			m_Beale_first_coef = value( P_BEALE_FIRST_COEF );
			m_Beale_square_coef = value( P_BEALE_SQUARE_COEF );
			m_Beale_third_coef = value( P_BEALE_THIRD_COEF );
			m_Beale_fourth_coef = value( P_BEALE_FOURTH_COEF );
			m_Pressure_coef = value( P_PRESSURE_COEF );
			m_Pressure_first = value( P_PRESSURE_FIRST );
			m_engine_speed = value( P_ENGINE_SPEED );
			m_V_displaced = value( P_V_DISPLACED );
			break;

		default:
			message(TCS_ERROR,  "Manufacturer integer needs to be from 1 to 5" );
			return -1;
		}

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{		
		double P_SE = value( I_P_SE );
		//double T_amb = value( I_T_AMB ) + 273.15;
		//double Number_of_Collectors = value( I_N_COLS );
		double T_compression = value( I_T_COMPRESSION );
		double T_heater_head_operate = value( I_T_HEATER_HEAD_OPERATE );
		double P_in_collector = value( I_P_IN_COLLECTOR );

		/* ===============================================
		!Curve fit of engine performance using Beale-Max method [Watts]
		!X-axis is input power to Stirling engine in Watts
		!Y-axis is the Beale number divided by 1-sqrt(TC/TE)*/
		
		double frequency = max(0.001, m_engine_speed / 60.0);				// Hz
		
		double Beale_max_fit = m_Beale_const_coef + m_Beale_first_coef*P_SE*1000 + 
		              (m_Beale_square_coef*pow(P_SE*1000.0,2.0)) + 
		              (m_Beale_third_coef*pow(P_SE*1000.0,3.0)) + 
		              (m_Beale_fourth_coef*pow(P_SE*1000.0,4.0));  
		
		// Curve fit of the engine pressure vs. power in to the engine [MPa]
		double engine_pressure_fit;
		if(m_manufacturer == 4)				// SAIC varies the engine volume not pressure
		{
			engine_pressure_fit = 12.0;		// MPa
			m_V_displaced = max(0.00001, m_Pressure_coef+m_Pressure_first*P_SE*1000.0);  //!pressure_coef actually is volume
		}
		else								// all other systems vary the pressure not volume
			engine_pressure_fit = max(0.001, m_Pressure_coef + m_Pressure_first*P_SE*1000);
		
		//output gross power from engine
		//double P_SE_out = (Beale_max_fit*(engine_pressure_fit*pow(10,6) * m_V_displaced*frequency)*(1.0-pow(T_compression/T_heater_head_operate,0.5)))/1000.0;

		
		//output gross power from engine
		double P_SE_out = (Beale_max_fit*(engine_pressure_fit*1.0e6 
			              * m_V_displaced*frequency)*(1.0-pow(T_compression/T_heater_head_operate,0.5)))/1000.0;
				
		
		// =================================================
		// Gross output power from the Stirling engine
		// if the input power is less than 25W, then the output power will be 0
		if(P_SE >= 0.025)
		{
			if (P_SE_out >= 0)
			{
				if (P_SE_out < P_SE)    
				{
					value( O_P_OUT_SE, P_SE_out );
					value( O_P_SE_LOSSES, P_SE - P_SE_out );
				}
				else
				{
					value( O_P_OUT_SE, 0.0 );
					value( O_P_SE_LOSSES, 0.001 );		//1 Watt so parasitic code is ok
				}			
			}
			else
			{
				value( O_P_OUT_SE, 0.0 );
				value( O_P_SE_LOSSES, 0.001 );			//1 Watt so parasitic code is ok
			}
		}
		else
		{
			value( O_P_OUT_SE, 0.0 );
			value( O_P_SE_LOSSES, 0.001 );				//1 Watt so parasitic code is ok
		}

		value( O_ETA_SE, value(O_P_OUT_SE)/(value(O_P_OUT_SE)+value(O_P_SE_LOSSES)+0.000000001) );
		
		value( O_T_HEATER_HEAD_HIGH, m_T_heater_head_high );
		value( O_T_HEATER_HEAD_LOW, m_T_heater_head_low );
		value( O_V_DISPLACED, m_V_displaced );
		value( O_FREQUENCY, frequency );
		value( O_ENGINE_PRESSURE, engine_pressure_fit*1.0e6);
		value( O_ETA_GROSS, value(O_P_OUT_SE)/(P_in_collector+0.00000001) );

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_pf_dish_engine_type297, "Collector Dish", "Ty Neises", 1, sam_pf_dish_engine_type297_variables, NULL, 1 )

