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
		P_COOLING_TOWER_ON,      
		P_TOWER_MODE,            
		P_D_PIPE_TOWER,          
		P_TOWER_M_DOT_WATER,     
		P_TOWER_M_DOT_WATER_TEST,
		P_TOWER_PIPE_MATERIAL,   
		P_ETA_TOWER_PUMP,        
		P_FAN_CONTROL_SIGNAL,    
		P_EPSILON_TOWER_TEST,    
		P_SYSTEM_AVAILABILITY,   
		P_PUMP_SPEED,            
		P_FAN_SPEED1,            
		P_FAN_SPEED2,            
		P_FAN_SPEED3,            
		P_T_COOL_SPEED2,         
		P_T_COOL_SPEED3,         
		P_EPSILON_COOLER_TEST,   
		P_EPSILON_RADIATOR_TEST,   
		P_COOLING_FLUID,      
		P_MANUFACTURER,       
		P_P_CONTROLS,         
		P_TEST_P_PUMP,        
		P_TEST_PUMP_SPEED,    
		P_TEST_COOLING_FLUID, 
		P_TEST_T_FLUID,       
		P_TEST_V_DOT_FLUID,   
		P_TEST_P_FAN,         
		P_TEST_FAN_SPEED,      
		P_TEST_FAN_RHO_AIR,   
		P_TEST_FAN_CFM,       
		P_B_RADIATOR,         
		P_B_COOLER,

		//Inputs           
        I_GROSS_POWER,           
		I_T_AMB,                 
		I_N_COLS,                              
		I_DNI,                   
		I_T_HEATER_HEAD_LOW,     
		I_V_SWEPT,               
		I_FREQUENCY,             
		I_ENGINE_PRESSURE,       
		I_I_CUT_IN,              
		I_Q_REJECT,              
		I_TOWER_WATER_OUTLET_TEMP,
		I_P_AMB_PA,              
		I_NS_DISH_SEPARATION,    
		I_EW_DISH_SEPARATION,    
		I_P_TOWER_FAN,           
		I_POWER_IN_COLLECTOR,    

		//Outputs                  
		O_NET_POWER,            
		O_P_PARASITIC,          
		O_T_COMPRESSION,        
		O_P_FAN,                
		O_P_PUMP,               
		O_TOWER_WATER_INLET_TEMP,
		O_M_DOT_WATER,          
		O_FAN_CONTROL_SIGNAL,   
		O_P_PARASITIC_TOWER,    
		O_T_TOWER_IN,           
		O_T_TOWER_OUT,          
		O_ETA_NET,              

		//N_MAX
		N_MAX};

tcsvarinfo sam_pf_dish_parasitics_type298_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_COOLING_TOWER_ON,      "cooling_tower_on",          "Option to use a cooling tower (set to 0=off)",                      "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TOWER_MODE,            "tower_mode",				  "Cooling tower type (natural or forced draft)",                      "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_D_PIPE_TOWER,          "d_pipe_tower",			  "Runner pipe diameter to the cooling tower (set to 0.4m)",           "m", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TOWER_M_DOT_WATER,     "tower_m_dot_water",		  "Tower cooling water flow rate (set to 134,000 kg/hr)",              "kg/s", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TOWER_M_DOT_WATER_TEST,"tower_m_dot_water_test",	  "Test value for the cooling water flow rate (set to 134,000 kg/hr)", "kg/s", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TOWER_PIPE_MATERIAL,   "tower_pipe_material",		  "Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel)", "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_ETA_TOWER_PUMP,        "eta_tower_pump",			  "Tower pump efficiency (set to 0.6)",                                "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FAN_CONTROL_SIGNAL,    "fan_control_signal",		  "Fan control signal (set to 1, not used in this model)",             "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_EPSILON_TOWER_TEST,    "epsilon_power_test",		  "Test value for cooling tower effectiveness (set to 0.7)",           "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_SYSTEM_AVAILABILITY,   "system_availability",		  "System availability (set to 1.0)",                                  "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PUMP_SPEED,            "pump_speed",				  "Reference Condition Pump Speed",                                    "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FAN_SPEED1,            "fan_speed1",				  "Cooling system fan speed 1",                                        "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FAN_SPEED2,            "fan_speed2",				  "Cooling system fan speed 2",                                        "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_FAN_SPEED3,            "fan_speed3",				  "Cooling system fan speed 3",                                        "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_COOL_SPEED2,         "T_cool_speed2",			  "Cooling Fluid Temp. For Fan Speed 2 Cut-In",                        "C", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_COOL_SPEED3,         "T_cool_speed3",			  "Cooling Fluid Temp. For Fan Speed 3 Cut-In",                        "C", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_EPSILON_COOLER_TEST,   "epsilon_cooler_test",		  "Cooler effectiveness",                                              "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_EPSILON_RADIATOR_TEST, "epsilon_radiator_test",	  "Radiator effectiveness",                                            "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_COOLING_FLUID,         "cooling_fluid",          "Reference Condition Cooling Fluid: 1=Water,2=V50%EG,3=V25%EG,4=V40%PG,5=V25%PG", "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_MANUFACTURER,          "manufacturer",              "Manufacturer (fixed as 5=other)",                           "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_P_CONTROLS,            "P_controls",				  "Control System Parasitic Power, Avg.",                      "W", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_P_PUMP,           "test_P_pump",				  "Reference Condition Pump Parasitic Power",                  "W", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_PUMP_SPEED,       "test_pump_speed",			  "Reference Condition Pump Speed",                            "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_COOLING_FLUID,    "test_cooling_fluid",		  "Reference Condition Cooling Fluid",                         "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_T_FLUID,          "test_T_fluid",			  "Reference Condition Cooling Fluid Temperature",             "K", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_V_DOT_FLUID,      "test_V_dot_fluid",		  "Reference Condition Cooling Fluid Volumetric Flow Rate",    "gpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_P_FAN,            "test_P_fan",				  "Reference Condition Cooling System Fan Power",              "W", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_FAN_SPEED,        "test_fan_speed",			  "Reference Condition Cooling System Fan Speed",              "rpm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_FAN_RHO_AIR,      "test_fan_rho_air",		  "Reference condition fan air density",                       "kg/m^3", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_FAN_CFM,          "test_fan_cfm",			  "Reference condition van volumentric flow rate",             "cfm", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_B_RADIATOR,            "b_radiator",				  "b_radiator parameter",                                      "-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_B_COOLER,              "b_cooler",				  "b_cooler parameter",                                        "-", "", "", ""},

	// INPUTS  
	{TCS_INPUT, TCS_NUMBER, I_GROSS_POWER,           "gross_power",               "Stirling engine gross output",                              "kW", "", "", ""},                       
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,                 "T_amb",					  "Ambient temperature in Kelvin",                     		   "K", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_N_COLS,                "N_cols",					  "Number of collectors",                              		   "none", "", "", ""},  
	{TCS_INPUT, TCS_NUMBER, I_DNI,                   "DNI",						  "Direct normal radiation (not interpolated)",        		   "W/m2", "", "", ""},  
	{TCS_INPUT, TCS_NUMBER, I_T_HEATER_HEAD_LOW,     "T_heater_head_low",		  "Header Head Lowest Temperature",                    		   "K", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_V_SWEPT,               "V_swept",					  "Displaced engine volume",                           		   "cm3", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_FREQUENCY,             "frequency",				  "Engine frequency (= RPM/60s)",                      		   "1/s", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_ENGINE_PRESSURE,       "engine_pressure",			  "Engine pressure",                                   		   "Pa", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_I_CUT_IN,              "I_cut_in",				  "Cut in DNI value used in the simulation",           		   "W/m2", "", "", ""},  
	{TCS_INPUT, TCS_NUMBER, I_Q_REJECT,              "Q_reject",				  "Stirling engine losses",                            		   "W", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_TOWER_WATER_OUTLET_TEMP, "Tower_water_outlet_temp", "Tower water outlet temperature (set to 20)",        		   "C", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_P_AMB_PA,              "P_amb_Pa",				  "Atmospheric pressure",                              		   "Pa", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_NS_DISH_SEPARATION,    "ns_dish_separation",		  "North-South dish separation used in the simulation",		   "m", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_EW_DISH_SEPARATION,    "ew_dish_separation",		  "East-West dish separation used in the simulation",  		   "m", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_P_TOWER_FAN,           "P_tower_fan",				  "Tower fan power (set to 0)",                        		   "kJ/hr", "", "", ""}, 
	{TCS_INPUT, TCS_NUMBER, I_POWER_IN_COLLECTOR,    "power_in_collector",		  "Power incident on the collector",                   		   "kW", "", "", ""},     

	// OUTPUTS 
	{TCS_OUTPUT, TCS_NUMBER, O_NET_POWER,            "net_power",                 "Net system output power",                                          "kW",   "", "", ""},    
	{TCS_OUTPUT, TCS_NUMBER, O_P_PARASITIC,          "P_parasitic",				  "Total parasitic power load",                                       "W",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_T_COMPRESSION,        "T_compression",			  "Cold sink temperature / compression temperature",                  "K",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_P_FAN,                "P_fan",					  "System fan power",                                                 "W",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_P_PUMP,               "P_pump",					  "Pumping parasitic power",                                          "W",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_TOWER_WATER_INLET_TEMP, "Tower_water_inlet_temp",  "Cooling water temperature into the cooling system",                "C",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_WATER,          "Tower_m_dot_water",		  "Cooling water mass flow rate in the cooling tower",                "kg/hr",   "", "", ""}, 
	{TCS_OUTPUT, TCS_NUMBER, O_FAN_CONTROL_SIGNAL,   "fan_control_signal",		  "Fan control signal (set to 1, not used in this model)",            "-",   "", "", ""},  
	{TCS_OUTPUT, TCS_NUMBER, O_P_PARASITIC_TOWER,    "P_parasitic_tower",		  "Parasitic load associated with the heat rejection system",         "W",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_T_TOWER_IN,           "T_tower_in",				  "Cooling fluid temperature out of the cooling and into the tower",  "C",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_T_TOWER_OUT,          "T_tower_out",				  "Cooling fluid temperature into the cooler and out of the tower",   "C",   "", "", ""},     
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_NET,              "eta_net",					  "Net system efficiency",                                            "-",   "", "", ""},  

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_pf_dish_parasitics_type298 : public tcstypeinterface
{
private:
	// Class Instances

	//Parameters
	double m_cooling_tower_on;        
	double m_tower_mode;				
	double m_d_pipe_tower;			
	double m_tower_m_dot_water;		
	double m_tower_m_dot_water_test;	
	double m_tower_pipe_material;		
	double m_eta_tower_pump;			
	double m_fan_control_sigmal;		
	double m_epsilon_tower_test;		
	double m_system_availability;		
	double m_pump_speed;				
	double m_fan_speed1;				
	double m_fan_speed2;				
	double m_fan_speed3;				
	double m_T_cool_speed2;			
	double m_T_cool_speed3;			
	double m_epsilon_cooler_test;		
	double m_epsilon_radiator_test;	
	double m_cooling_fluid;          
	int m_manufacturer;            
	double m_P_controls;				
	double m_test_P_pump;				
	double m_test_pump_speed;			
	int m_test_cooling_fluid;		
	double m_test_T_fluid;			
	double m_test_V_dot_fluid;		
	double m_test_P_fan;				
	double m_test_fan_speed;			
	double m_test_fan_diameter;		
	double m_test_fan_rho_air;		
	double m_test_fan_cfm;			
	double m_b_radiator;				
	double m_b_cooler;	
	double m_test_pump_d_impeller;
	double m_pump_d_impeller;

	//Stored Variables

	// Calculated

public:
	sam_pf_dish_parasitics_type298( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_cooling_tower_on = std::numeric_limits<double>::quiet_NaN();      
		m_tower_mode = std::numeric_limits<double>::quiet_NaN();			
		m_d_pipe_tower = std::numeric_limits<double>::quiet_NaN();			
		m_tower_m_dot_water = std::numeric_limits<double>::quiet_NaN();		
		m_tower_m_dot_water_test = std::numeric_limits<double>::quiet_NaN();
		m_tower_pipe_material = std::numeric_limits<double>::quiet_NaN();	
		m_eta_tower_pump = std::numeric_limits<double>::quiet_NaN();		
		m_fan_control_sigmal = std::numeric_limits<double>::quiet_NaN();	
		m_epsilon_tower_test = std::numeric_limits<double>::quiet_NaN();	
		m_system_availability = std::numeric_limits<double>::quiet_NaN();	
		m_pump_speed = std::numeric_limits<double>::quiet_NaN();			
		m_fan_speed1 = std::numeric_limits<double>::quiet_NaN();			
		m_fan_speed2 = std::numeric_limits<double>::quiet_NaN();			
		m_fan_speed3 = std::numeric_limits<double>::quiet_NaN();			
		m_T_cool_speed2 = std::numeric_limits<double>::quiet_NaN();			
		m_T_cool_speed3 = std::numeric_limits<double>::quiet_NaN();			
		m_epsilon_cooler_test = std::numeric_limits<double>::quiet_NaN();	
		m_epsilon_radiator_test = std::numeric_limits<double>::quiet_NaN();	
		m_cooling_fluid = std::numeric_limits<double>::quiet_NaN();         
		m_manufacturer = -1;            
		m_P_controls = std::numeric_limits<double>::quiet_NaN();			
		m_test_P_pump = std::numeric_limits<double>::quiet_NaN();			
		m_test_pump_speed = std::numeric_limits<double>::quiet_NaN();		
		m_test_cooling_fluid = -1;	
		m_test_T_fluid = std::numeric_limits<double>::quiet_NaN();			
		m_test_V_dot_fluid = std::numeric_limits<double>::quiet_NaN();		
		m_test_P_fan = std::numeric_limits<double>::quiet_NaN();			
		m_test_fan_speed = std::numeric_limits<double>::quiet_NaN();		
		m_test_fan_diameter = std::numeric_limits<double>::quiet_NaN();		
		m_test_fan_rho_air = std::numeric_limits<double>::quiet_NaN();		
		m_test_fan_cfm = std::numeric_limits<double>::quiet_NaN();			
		m_b_radiator = std::numeric_limits<double>::quiet_NaN();			
		m_b_cooler = std::numeric_limits<double>::quiet_NaN();	
		m_test_pump_d_impeller = std::numeric_limits<double>::quiet_NaN();
		m_pump_d_impeller = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_pf_dish_parasitics_type298()
	{
	}

	virtual int init()
	{
		m_cooling_tower_on = value( P_COOLING_TOWER_ON );    
		m_tower_mode = value( P_TOWER_MODE );
		m_d_pipe_tower = value( P_D_PIPE_TOWER ); 			
		m_tower_m_dot_water = value( P_TOWER_M_DOT_WATER ); 		
		m_tower_m_dot_water_test = value( P_TOWER_M_DOT_WATER_TEST );
		m_tower_pipe_material = value( P_TOWER_PIPE_MATERIAL );	
		m_eta_tower_pump = value( P_ETA_TOWER_PUMP );
		m_fan_control_sigmal = value( P_FAN_CONTROL_SIGNAL );
		m_epsilon_tower_test = value( P_EPSILON_TOWER_TEST );
		m_system_availability = value( P_SYSTEM_AVAILABILITY );
		m_pump_speed = value( P_PUMP_SPEED );			
		m_fan_speed1 = value( P_FAN_SPEED1 );			
		m_fan_speed2 = value( P_FAN_SPEED2 );
		m_fan_speed3 = value( P_FAN_SPEED3 );			
		m_T_cool_speed2 = value( P_T_COOL_SPEED2 ); 			
		m_T_cool_speed3 = value( P_T_COOL_SPEED3 );			
		m_epsilon_cooler_test = value( P_EPSILON_COOLER_TEST );	
		m_epsilon_radiator_test = value( P_EPSILON_RADIATOR_TEST );
		m_cooling_fluid = value( P_COOLING_FLUID );         
		m_manufacturer = (int) value( P_MANUFACTURER );           		

		m_tower_m_dot_water = m_tower_m_dot_water / 3600;			// convert to kg/s
		m_tower_m_dot_water_test = m_tower_m_dot_water_test / 3600;	// convert to kg/s

		switch( m_manufacturer )
		{
		case 1:							// SES System = 1
			m_P_controls=150;
			m_test_P_pump=100;
			m_test_pump_speed=1800;		// rpm
			m_test_cooling_fluid = 2;
			m_test_T_fluid=288;			// K
			m_test_V_dot_fluid= 9.0*0.003785/60.0;	// convert 9 gpm --> m^3/s
			m_test_P_fan=1000.0;		// W
			m_test_fan_speed=890;		// rpm
			m_test_fan_rho_air=1.2;
			m_test_fan_cfm=6000;
			m_b_radiator = 0.7;			// used for UA_act/UA_test = [V_dot_act/V_dot_test]^b
			m_b_cooler = 0.7;			// used for UA_act/UA_test = [V_dot_act/V_dot_test]^b
			break;

		case 2:							// WGA System = 2
			m_P_controls= 100;
			m_test_P_pump= 75;
			m_test_pump_speed=1800;	// rpm
			m_test_cooling_fluid = 2;
			m_test_T_fluid=288;				// K
			m_test_V_dot_fluid= 7.5*0.003785/60;	// convert 7.5 gpm --> m^3/s
			m_test_P_fan=410;
			m_test_fan_speed=890;
			m_test_fan_rho_air=1.2;
			m_test_fan_cfm=4000;
			m_b_radiator = 0.7;
			m_b_cooler = 0.7;
			break;

		case 3:							// SBP System = 3
			m_P_controls= 175;
			m_test_P_pump= 100;
			m_test_pump_speed=1800;		// rpm
			m_test_cooling_fluid = 1;
			m_test_T_fluid=288;			// K
			m_test_V_dot_fluid= 7.5*0.003785/60.0;
			m_test_P_fan=510;
			m_test_fan_speed=890;
			m_test_fan_rho_air=1.2;
			m_test_fan_cfm=4500;
			m_b_radiator = 0.7;
			m_b_cooler = 0.7;
			break;

		case 4:							// SAIC System = 4
			m_P_controls= 300;
			m_test_P_pump= 200;
			m_test_pump_speed=1800;		// rpm
			m_test_cooling_fluid = 2;
			m_test_T_fluid=288;			// K
			m_test_V_dot_fluid= 12*0.003785/60.0;
			m_test_P_fan=2500;
			m_test_fan_speed=850;
			m_test_fan_rho_air=1.2;
			m_test_fan_cfm=10000;
			m_b_radiator = 0.7;
			m_b_cooler = 0.7;
			break;

		case 5:
			m_P_controls = value( P_P_CONTROLS );
			m_test_P_pump = value( P_TEST_P_PUMP );
			m_test_pump_speed = value( P_TEST_PUMP_SPEED ); 	
			m_test_cooling_fluid = (int)value( P_TEST_COOLING_FLUID );
			m_test_T_fluid = value( P_TEST_T_FLUID ); 			
			m_test_V_dot_fluid = value( P_TEST_V_DOT_FLUID )*0.003785/60.0; 	
			m_test_P_fan = value( P_TEST_P_FAN );			
			m_test_fan_speed = value( P_TEST_FAN_SPEED ); 					
			m_test_fan_rho_air = value( P_TEST_FAN_RHO_AIR ); 	
			m_test_fan_cfm = value( P_TEST_FAN_CFM );			
			m_b_radiator = value( P_B_RADIATOR ); 			
			m_b_cooler = value( P_B_COOLER ); 				
			break;

		default:
			message( TCS_ERROR, "Manufacturer integer needs to be from 1 to 5" );
			return -1;
		}
		
		m_test_fan_diameter = 0.63;			// Does not change...arbitrary value chosen
		m_test_pump_d_impeller = 0.15;		// Does not change...arbitrary value chosen
		m_pump_d_impeller = 0.15;					// m  pump impeller diameter....won't change so need any value for pump performane eq's

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{		
		double gross_power = value( I_GROSS_POWER );                
		double T_amb = value( I_T_AMB ) + 273.15;     
		double Number_of_Collectors = value( I_N_COLS );              
		double DNI  = value( I_DNI );             
		double T_heater_head_low = value( I_T_HEATER_HEAD_LOW );                
		double V_swept = value( I_V_SWEPT );
		double frequency = value( I_FREQUENCY );              
		double engine_pressure = value( I_ENGINE_PRESSURE );          
		double I_cut_in  = value( I_I_CUT_IN );
		double Q_reject  = value( I_Q_REJECT );          
		double Tower_water_outlet_temp  = value( I_TOWER_WATER_OUTLET_TEMP );        
		double P_amb_Pa  = value( I_P_AMB_PA )*100.0; 
		double NS_dish_separation  = value( I_NS_DISH_SEPARATION );             
		//double EW_dish_separation  = value( I_EW_DISH_SEPARATION );
		double P_tower_fan  = value( I_P_TOWER_FAN );
		double P_in_collector  = value( I_POWER_IN_COLLECTOR );                  

		//double k_air = 0.00169319 + 0.0000794814*T_amb;
		//double beta_air = 0.00949962 - 0.0000297215*T_amb + 3.06353*10E-08*pow(T_amb,2);
		//double mu_air = 0.00000499562 + 4.50917E-08*T_amb;
		double M_air = 28.97;		// [kg/kmol]  molar mass of air
		double R_bar = 8314;		// [J/kmol-K]  gas constant
		double R_air = R_bar / M_air;
		double rho_air = P_amb_Pa/(R_air*T_amb);  // ideal gas law

		P_tower_fan = P_tower_fan *1000 / 3600;			// convert kJ/hr to J/s
		double P_SE_losses = Q_reject;
		double Q_losses = 1000*P_SE_losses;				// [W] Heat rejected to the cooling system
		double Q_reject_total = Number_of_Collectors*Q_reject*1000.0;	// W/kW		
		
		//double T_amb_C = T_amb;		
		//double P_amb_atm = P_amb_Pa * 0.000009869;

		// Determine properties of the cooling fluid during test conditions
		
		double T_K2 = m_test_T_fluid;
		double T_K1 = m_test_T_fluid;

		double mu_water, mu_fluid_test, rho_cool_fluid_test, cp_fluid_test;
		if( m_test_cooling_fluid == 1 )				// Water
		{
			mu_water = 3.50542 - 0.0539638*T_K2 + 0.000333345*pow(T_K2,2) - 0.0000010319*pow(T_K2,3) + 1.59983E-09*pow(T_K2,4) - 9.93386E-13*pow(T_K2,5);
			mu_fluid_test = 3.50542 - 0.0539638*T_K2 + 0.000333345*pow(T_K2,2) - 0.0000010319*pow(T_K2,3) + 1.59983E-09*pow(T_K2,4) - 9.93386E-13*pow(T_K2,5);
			rho_cool_fluid_test = 692.604 + 2.2832*T_K2 - 0.00423412*pow(T_K2,2);
			cp_fluid_test = 2.14384E+06 - 34048.9*T_K2 + 216.467*pow(T_K2,2) - 0.687249*pow(T_K2,3) + 0.00108959*pow(T_K2,4) - 6.90127E-07*pow(T_K2,5);
		}
		else if( m_test_cooling_fluid == 2 )		// 50% Ethylene Glycol
		{
			mu_fluid_test = 18.3853 - 0.238994*T_K1 + 0.00116489*pow(T_K1,2) - 0.00000252199*pow(T_K1,3) + 2.04562E-09*pow(T_K1,4);
			rho_cool_fluid_test = 1026.4 + 0.80163*T_K1 - 0.00227397*pow(T_K1,2);
			cp_fluid_test = 1899.32 + 4.19104*T_K1 + 0.00194702*pow(T_K1,2);
		}
		else if( m_test_cooling_fluid == 3 )	// !25% Ethylene Glycol
		{
			mu_fluid_test = 5.33548 - 0.0686842*T_K1 + 0.000331949*pow(T_K1,2) - 7.13336E-07*pow(T_K1,3) + 5.74824E-10*pow(T_K1,4);
			rho_cool_fluid_test = 823.536 + 1.76857*T_K1 - 0.00360812*pow(T_K1,2);
			cp_fluid_test = 3056.35 + 3.04401*T_K1 - 0.00126969*pow(T_K1,2);
		}
		else if( m_test_cooling_fluid == 4 )	// 50% Propylene Glycol
		{
			mu_fluid_test = 29.2733 - 0.367285*T_K1 + 0.00172909*pow(T_K1,2) - 0.00000361866*pow(T_K1,3) + 2.83986E-09*pow(T_K1,4);
			rho_cool_fluid_test = 1032.38 + 0.658226*T_K1 - 0.00217079*pow(T_K1,2);
			cp_fluid_test = 3501.54 - 2.12091*T_K1 + 0.00807326*pow(T_K1,2);
		}
		else if( m_test_cooling_fluid == 5 )	// 25% Propylene Glycol
		{
			mu_fluid_test = 8.22884 - 0.104386*T_K1 + 0.000496931*pow(T_K1,2) - 0.00000105157*pow(T_K1,3) + 8.34276E-10*pow(T_K1,4);
			rho_cool_fluid_test = 814.76 + 1.75047*T_K1 - 0.00358803*pow(T_K1,2);
			cp_fluid_test = 10775.6 - 62.9957*T_K1 + 0.190454*pow(T_K1,2) - 0.000186685*pow(T_K1,3);
		}
		else	// 50% ethylene glycol
		{
			mu_fluid_test = 18.3853 - 0.238994*T_K1 + 0.00116489*pow(T_K1,2) - 0.00000252199*pow(T_K1,3) + 2.04562E-09*pow(T_K1,4);
			rho_cool_fluid_test = 1026.4 + 0.80163*T_K1 - 0.00227397*pow(T_K1,2);
			cp_fluid_test = 1899.32 + 4.19104*T_K1 + 0.00194702*pow(T_K1,2);
		}


		// fluid properties of cooling fluid
		double T_tolerance = 1;			//[K] Difference between residual & cool fluid must less than the tolerance
		                 
		double d_T = 1.0;				//[K] Increasing by larger value to be in middle of average cooling fluid temp
		double T_difference = 200.0;	//[K] Initialize
		double T_cool_in = 300.0;		//[K] initialize
		double T_cool_speed2 = m_T_cool_speed2+273.15;			// convert C to K
		double T_cool_speed3 = m_T_cool_speed3+273.15;			// convert C to K
		
		double mu_cool_fluid, rho_tower, T1, fan_speed_use, T_compression, Tower_water_inlet_temp, P_pump, P_fan;
		double T_cool_out = -987.6;
		for( double T_res = 260; T_res <= 600; T_res += d_T )
		{
			if( T_difference >= T_tolerance )
			{
				// ========================
				// Determine the fan speed based on direct normal insolation
				double fan_speed;
				if(T_cool_in >= T_cool_speed2) 
				{
					if(T_cool_in < T_cool_speed3)
						fan_speed = m_fan_speed2;
					else 
						fan_speed = m_fan_speed3;
				}
				else
					fan_speed =  m_fan_speed1;
					
				// Set fan speed to zero if DNI < I_cut_in
				if(DNI < I_cut_in)
					fan_speed =  0.0;

				/*System does not converge with a fan speed below 50RPM so modify code
				Output power is reduced in output below for fans speeds below 50RPM
				to simulate natural convection of radiator*/

				fan_speed_use = fan_speed;
				if(fan_speed < 50.0)    
					fan_speed = 50.0;

				double rho_fluid, cp_fluid;
				mu_water = 3.50542 - 0.0539638*T_res + 0.000333345*pow(T_res,2) - 0.0000010319*pow(T_res,3) + 1.59983E-09*pow(T_res,4) - 9.93386E-13*pow(T_res,5);
				if( m_cooling_fluid == 1 )				// Water
				{
					mu_cool_fluid = 3.50542 - 0.0539638*T_res + 0.000333345*pow(T_res,2) - 0.0000010319*pow(T_res,3) + 1.59983E-09*pow(T_res,4) - 9.93386E-13*pow(T_res,5);
					rho_fluid = 692.604 + 2.2832*T_res - 0.00423412*pow(T_res,2);
					cp_fluid = 2.14384E+06 - 34048.9*T_res + 216.467*pow(T_res,2) - 0.687249*pow(T_res,3) + 0.00108959*pow(T_res,4) - 6.90127E-07*pow(T_res,5);
				}
				else if( m_cooling_fluid == 2 )		// 50% Ethylene Glycol
				{
					mu_cool_fluid = 18.3853 - 0.238994*T_res + 0.00116489*pow(T_res,2) - 0.00000252199*pow(T_res,3) + 2.04562E-09*pow(T_res,4);
					rho_fluid = 1026.4 + 0.80163*T_res - 0.00227397*pow(T_res,2);
					cp_fluid = 1899.32 + 4.19104*T_res + 0.00194702*pow(T_res,2);
				}
				else if( m_cooling_fluid == 3 )	// !25% Ethylene Glycol
				{
					mu_cool_fluid = 5.33548 - 0.0686842*T_res + 0.000331949*pow(T_res,2) - 7.13336E-07*pow(T_res,3) + 5.74824E-10*pow(T_res,4);
					rho_fluid = 823.536 + 1.76857*T_res - 0.00360812*pow(T_res,2);
					cp_fluid = 3056.35 + 3.04401*T_res - 0.00126969*pow(T_res,2);
				}
				else if( m_cooling_fluid == 4 )	// 50% Propylene Glycol
				{
					mu_cool_fluid = 29.2733 - 0.367285*T_res + 0.00172909*pow(T_res,2) - 0.00000361866*pow(T_res,3) + 2.83986E-09*pow(T_res,4);
					rho_fluid = 1032.38 + 0.658226*T_res - 0.00217079*pow(T_res,2);
					cp_fluid = 3501.54 - 2.12091*T_res + 0.00807326*pow(T_res,2);
				}
				else if( m_cooling_fluid == 5 )	// 25% Propylene Glycol
				{
					mu_cool_fluid = 8.22884 - 0.104386*T_res + 0.000496931*pow(T_res,2) - 0.00000105157*pow(T_res,3) + 8.34276E-10*pow(T_res,4);
					rho_fluid = 814.76 + 1.75047*T_res - 0.00358803*pow(T_res,2);
					cp_fluid = 10775.6 - 62.9957*T_res + 0.190454*pow(T_res,2) - 0.000186685*pow(T_res,3);
				}
				else	// 50% ethylene glycol
				{
					mu_cool_fluid = 18.3853 - 0.238994*T_res + 0.00116489*pow(T_res,2) - 0.00000252199*pow(T_res,3) + 2.04562E-09*pow(T_res,4);
					rho_fluid = 1026.4 + 0.80163*T_res - 0.00227397*pow(T_res,2);
					cp_fluid = 1899.32 + 4.19104*T_res + 0.00194702*pow(T_res,2);
				}

				// ================================================================
				// Use fan laws to determine how fan parasitic power changes determine new air mass flow rate
				// C_W --> fan power coefficient
				double test_fan_speed_rad = m_test_fan_speed*2*3.14159/60.0;
				double fan_speed_rad = fan_speed*2.0*3.14159/60.0;
					
				double C_W = m_test_P_fan / (pow(test_fan_speed_rad,3)*pow(m_test_fan_diameter,5)*m_test_fan_rho_air+1.E-8);

				// find fan power based on operating speed & air density
				P_fan = C_W*pow(fan_speed_rad,3)*pow(m_test_fan_diameter,5)*rho_air;
					
				// determine capacity coef C_V
				double V_dot_air_test = m_test_fan_cfm*pow(0.3048,3)/60;			// m^3/s !volume flow rate of air
				double C_V = V_dot_air_test/(test_fan_speed_rad*pow(m_test_fan_diameter,3)+1.E-8);
				// solve for new air volume flow rate
				double V_dot_air=C_V*fan_speed_rad*pow(m_test_fan_diameter,3);
				// determine new mass flow rate of air
				double m_dot_air = V_dot_air*rho_air;

				// =========================================================================
				// Determine pump (one-dish) power using dimentionless pump laws & viscosity conversion
				double test_pump_speed_rad = m_test_pump_speed*2*3.14159/60.0;
				double pump_speed_rad = m_pump_speed*2*3.14159/60;					// rad/s
					
				double C_W_pump = m_test_P_pump/(pow(test_pump_speed_rad,3)*pow(m_test_pump_d_impeller,5)*rho_cool_fluid_test+1.E-8);

				// find pump power based on operating speed & cooling fluid density
				P_pump = C_W_pump*pow(pump_speed_rad,3)*pow(m_test_pump_d_impeller,5)*rho_fluid;
					
				// determine capacity coef C_V_pump
				double C_V_pump = m_test_V_dot_fluid/(test_pump_speed_rad*pow(m_test_pump_d_impeller,3)+1.E-8);
					
				// solve for new cooling fluid volume flow rate
				double V_dot_cool_fluid = C_V_pump*pump_speed_rad*pow( m_test_pump_d_impeller,3 );
					
				// determine new mass flow rate of cooling fluid
				double m_dot_cool_fluid = V_dot_cool_fluid*rho_fluid;

				// ==========================================================================
				// capacitance, mass, and volumetric flow rates of H2, cooling fluid, & air
				double engine_pressure_test = 15000000;				// Pa
				double frequency_test = 30.0;						// 1/s
				double T_H2_ave_test = 600.0;						// K
				double Cp_H2_TEST = 14500.0;
					
				// mass & capacitance rate of H2 in the engine
				double R_gas = 8314.0;						// J/kmol-K
				double M_H2 = 2.0160;						// molar mass of H2
				double T_H2_ave = (T_heater_head_low + T_amb)/2.0;
				double V_total = 2.5 * V_swept;
				double mass_H2  = engine_pressure * V_total*M_H2 /(R_gas*(T_H2_ave)+1E-8);
				double mass_H2_test = engine_pressure_test * V_total*M_H2 / (R_gas*(T_H2_ave_test)+1E-8);
				double rho_H2 = engine_pressure *M_H2 /(R_gas*(T_H2_ave)+1E-8);
				double rho_H2_test = engine_pressure_test /(R_gas*(T_H2_ave_test)+1E-8);					// at full load
				double m_dot_H2_test = mass_H2_test * 2 * frequency_test;	
				double m_dot_H2 = mass_H2 * 2 * frequency;	  
				double Cp_H2 = 14500;
				double C_dot_H2 = m_dot_H2 * Cp_H2;
				double V_dot_H2 = m_dot_H2 / (rho_H2+1E-8);
				double V_dot_H2_test = m_dot_H2_test / (rho_H2_test+1E-8);
				double C_dot_H2_test = m_dot_H2_test * Cp_H2_TEST;
					
				// capacitance rate of cooling fluid through radiator
				double C_dot_cool_fluid = m_dot_cool_fluid * cp_fluid;
				double m_dot_cool_fluid_test = m_test_V_dot_fluid * rho_cool_fluid_test;	
				double C_dot_cool_fluid_test = m_dot_cool_fluid_test * cp_fluid_test;
					
				// capacitance rate of air through radiator
				double cp_air_test = 1005;							// mostly constant Cp for air
				double m_dot_air_test = V_dot_air_test * m_test_fan_rho_air;	// mass flow rate of air 
				double C_dot_air_test = m_dot_air_test * cp_air_test;
				double cp_air = 1005;
				double C_dot_air = m_dot_air * cp_air;
					
				// Min and Max capacitance & volumetric flow rates					
				double C_dot_min_test_rad = min(C_dot_air_test,C_dot_cool_fluid_test);
				double C_dot_max_test_rad = max(C_dot_air_test,C_dot_cool_fluid_test);
				double C_dot_min_rad = min(C_dot_air,C_dot_cool_fluid);
				double C_dot_max_rad = max(C_dot_air,C_dot_cool_fluid);
				double V_dot_min_rad = min(V_dot_air,V_dot_cool_fluid);
				double V_dot_min_test_rad = min(V_dot_air_test,m_test_V_dot_fluid);
				double C_dot_min_test_cooler = min(C_dot_H2_test,C_dot_cool_fluid_test);
				double C_dot_max_test_cooler = max(C_dot_H2_test,C_dot_cool_fluid_test);
				double C_dot_min_cooler = min(C_dot_H2,C_dot_cool_fluid);
				double C_dot_max_cooler = max(C_dot_H2,C_dot_cool_fluid);
				double V_dot_min_cooler = min(V_dot_H2,V_dot_cool_fluid);
				double V_dot_min_test_cooler = min(V_dot_H2_test,m_test_V_dot_fluid);

				// ==========================================================================
				// Determine effectiveness of the cooler and radiator for operating conditions
					
				// ========================================================================
				// Radiator effectiveness-NTU for determining radiator effectiveness with changing fan speed
				// =========================================================================
					
				// Cross-flow NTU-effectiveness correlation
				double Cr_radiator_TEST = C_dot_min_test_rad / C_dot_max_test_rad;
				double NTU_radiator_TEST = -log(1+(1/Cr_radiator_TEST)*(log(1-m_epsilon_radiator_test*Cr_radiator_TEST)));		// solve for test NTU
					
				// determine overall heat xfer coef UA at test conditions
				double UA_radiator_TEST = NTU_radiator_TEST * C_dot_min_test_rad;  
					
				// determine overall heat xfer coef UA at operating conditions
				double UA_radiator = UA_radiator_TEST * pow( (V_dot_min_rad/V_dot_min_test_rad), m_b_radiator);
					
				// determine new NTU value based on new C_dot of air
				double NTU_radiator = UA_radiator / (C_dot_min_rad+1E-8);
					
				// solve for new radiator effectiveness                           
				double Cr_radiator = C_dot_min_rad / C_dot_max_rad;
				double EPSILON_radiator = (1/(Cr_radiator+1E-8))*(1-exp(-Cr_radiator*(1-exp(-NTU_radiator))));
					
				// ============================================================
				// Solve for cooling tower water temp into the tower
				// ============================================================
				Tower_water_inlet_temp = Tower_water_outlet_temp + Q_reject_total/(cp_fluid * m_tower_m_dot_water+1E-8);
				// average temp of cooling tower fluid
				double Tower_water_ave=(Tower_water_inlet_temp+Tower_water_outlet_temp)/2.0;

				// ==========================================================================
				// Cooling Tower loop HX effectiveness-NTU for effectiveness with changing pump/tower flow
				// ==========================================================================
				T1 = Tower_water_outlet_temp+273.15;
				rho_tower = 589.132 + 2.98577*T1 - 0.00542465*pow(T1,2);
					
				// determine new NTU value based on new C_dot of the engine
				//double NTU_tower = UA_tower / (C_dot_min_tower+1E-8);
					
				// solve for new cooler effectiveness (counter-flow correlation)
				//double Cr_tower = C_dot_min_tower / C_dot_max_tower;
				//double epsilon_tower = (1-exp(-NTU_tower*(1-Cr_tower))) / (1-Cr_tower*exp(-NTU_tower*(1-Cr_tower)));

				// ==========================================================================
				// Heater effectiveness-NTU for determining cooler effectiveness with changing mass in engine
				// ==========================================================================
					
				// Counter-flow Concentric-Tube NTU-effectiveness correlation
				double Cr_cooler_test = C_dot_min_test_cooler / C_dot_max_test_cooler;
				double NTU_cooler_test = 1/(Cr_cooler_test-1.0)*log((m_epsilon_cooler_test-1.0)/(m_epsilon_cooler_test*Cr_cooler_test-1.0));
					
				// determine overall heat xfer coef UA at test conditions
				double UA_cooler_test = NTU_cooler_test * C_dot_min_test_cooler;  
					
				// determine overall heat xfer coef UA at operating conditions
				double UA_cooler = UA_cooler_test * pow( (V_dot_min_cooler/(V_dot_min_test_cooler+1E-8)), m_b_cooler);
					
				// determine new NTU value based on new C_dot of the engine
				double NTU_cooler = UA_cooler / (C_dot_min_cooler+1E-8);
					
				// solve for new cooler effectiveness (counter-flow correlation)
				double Cr_cooler = C_dot_min_cooler / C_dot_max_cooler;
				double epsilon_cooler = (1-exp(-NTU_cooler*(1-Cr_cooler))) / (1-Cr_cooler*exp(-NTU_cooler*(1-Cr_cooler)));
										
				// =================================================================
				// Determine compression space temperature
				// double T_cool_in;
				if( m_cooling_tower_on == 0 )		// No Tower
				{
					// solve for cooling fluid temp out of cooler and into radiator
					T_cool_out = Q_losses/(EPSILON_radiator*C_dot_min_rad+1E-8)+T_amb;
					// solve for cooling fluid temp in to cooler and out of radiator
					T_cool_in= -Q_losses/(C_dot_cool_fluid+1E-8)+T_cool_out;			// Energy balance
				}
				else								// Cooling Tower used
				{
					// solve for cooling fluid temp out of cooler and into tower loop HX
					T_cool_out = Q_losses/(EPSILON_radiator*C_dot_min_rad+1E-8)+(Tower_water_ave +273.15);
					// solve for cooling fluid temp in to cooler and out of tower loop HX
					T_cool_in= -Q_losses/(C_dot_cool_fluid+1E-8)+T_cool_out;			// energy balance
				}
					
				//double T_cool_water_ave =  (T_cool_out + T_cool_in) / 2.0;
				C_dot_min_cooler = min(C_dot_H2, C_dot_cool_fluid);
					
				// solve for temp of hydrogen into the cooler
				double T_H2_in = Q_losses/(epsilon_cooler*C_dot_min_cooler)+T_cool_in;
					
				// solve for temp of compression space
				double T_H2_out=T_H2_in-epsilon_cooler*C_dot_min_cooler/(C_dot_H2+1E-8)*(T_H2_in - T_cool_in);
					
				// solve for cold junction temp = compression space
				T_compression = T_H2_out;
			}
			else
				break;

			T_difference = fabs(T_res - T_cool_out);
		}

		// =================================================================
		// correct pump power based on viscosity of fluid
		double mu_div_mu_water = mu_cool_fluid / (mu_water+1E-8);
		double pump_multiplier=1.01178 - 0.0117778*mu_div_mu_water;
		
		P_pump = P_pump / (pump_multiplier+1E-8);
		
		// =================================================================
		// Set pump and control power to 0 when DNI < 1
		double P_controls;
		if(DNI < 1.0)
		{
			P_pump = 0.0;
			P_controls = 1.0;
		}
		else
			P_controls = m_P_controls;
		
		// =================================================================
		// Set individual system fan power to zero when cooling tower on
		if(m_cooling_tower_on != 0.0)
			P_fan = 0.0;
		
		// =================================================================
		// Set cooling tower fan power to zero when DNI is low
		if(DNI < 300.0) 
			P_tower_fan = 0.0;
		
		// =========================================================================
		// Determine pump power for the cooling tower to distribute water to all dishes
		double pipe_length = NS_dish_separation * Number_of_Collectors;
		double A_pipe = 3.14159*pow((m_d_pipe_tower/2),2);
		double vel_tower = m_tower_m_dot_water / (rho_tower * A_pipe+1E-8);
		double mu_tower = 0.299062 - 0.00283786*T1 + 0.0000090396*pow(T1,2) - 9.64494E-09*pow(T1,3);
		double Re_tower = rho_tower * vel_tower * m_d_pipe_tower / (mu_tower+1E-8);
		
		double epsilon_wall = std::numeric_limits<double>::quiet_NaN();
		if( m_tower_pipe_material == 1 )		// Plastic
			epsilon_wall = 0.0000015;
		else if( m_tower_pipe_material == 2)	// cast iron
			epsilon_wall = 0.00026;
		else if( m_tower_pipe_material == 3)	// riveted steel
			epsilon_wall = 0.003;

		// Haaland eq......p366 White 2003
		double friction_factor = 1/pow( (-1.8*log10(6.9/(Re_tower+1E-8)+pow( epsilon_wall/(3.7*m_d_pipe_tower), 1.11))), 2);
		double head_friction = friction_factor*pipe_length*pow(vel_tower,2)/(m_d_pipe_tower*2*9.8);
		double K_total = 0.001/39.37;					// meters   p.387 White 2003 estimate for minor losses
		double head_minor = K_total*(pow(vel_tower,2)/(2*9.8));
		double head_total = head_friction + head_minor;
		double V_dot_tower = m_tower_m_dot_water / (rho_tower+1E-8);
		double P_tower_pump = rho_tower*9.8*V_dot_tower*(head_total)/(m_eta_tower_pump+1E-8);
		
		if(m_cooling_tower_on == 0)
		{
			P_tower_pump = 0.0;
			P_tower_fan = 0.0;
		}
		
		// =================================================================
		// Determine total parasitic power
		
		// Stirling dish system with a fan, pump and radiator
		double P_parasitic = P_fan + P_pump + P_controls;
		
		// cooling tower with pump and fan
		double P_parasitic_tower;
		if(m_tower_mode == 1)
			P_parasitic_tower = P_tower_pump;			// natural convection
		else
			P_parasitic_tower = P_tower_pump + P_tower_fan;	// forced draft

		double net_power_out = gross_power - (P_parasitic/1000.0);
		
		double SUM = Number_of_Collectors*net_power_out - P_parasitic_tower/1000.0;
		if( m_tower_mode == 1 )
		{
			if( SUM <= 0 )
			{
				net_power_out = 0.0;
				P_parasitic_tower = 0.0;
			}
		}

		if(net_power_out >= 0)
		{
			if( fan_speed_use >= 50) 
				value( O_NET_POWER, net_power_out*m_system_availability );
			else 
				// approx natural convection at very low fan speeds
				value( O_NET_POWER, net_power_out*m_system_availability*pow( ((fan_speed_use+0.5)/50), 0.5) );
		}
		else
			value( O_NET_POWER, 0.0 );
			
		value( O_P_PARASITIC, P_parasitic );

		value( O_T_COMPRESSION, T_compression );

		value( O_P_FAN, P_fan );

		value( O_P_PUMP, P_pump );

		value( O_TOWER_WATER_INLET_TEMP, Tower_water_inlet_temp );

		value( O_M_DOT_WATER, m_tower_m_dot_water*3600.0 );			// convert back to kg/hr

		value( O_FAN_CONTROL_SIGNAL, m_fan_control_sigmal );

		value( O_P_PARASITIC_TOWER, P_parasitic_tower );

		value( O_T_TOWER_OUT, T_cool_out - 273.15 );

		value( O_T_TOWER_IN, T_cool_in - 273.15 );

		value( O_ETA_NET, value( O_NET_POWER ) / (P_in_collector+0.00000001) );

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_pf_dish_parasitics_type298, "Collector Dish", "Ty Neises", 1, sam_pf_dish_parasitics_type298_variables, NULL, 1 )

