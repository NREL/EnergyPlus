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

// Dish Stirling model
#include "core.h"
#include "tckernel.h"
// for adjustment factors
#include "common.h"

static var_info _cm_vtab_tcsdish[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                      LABEL                                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",               "local weather file path",                                                        "",             "",             "Weather",       "*",                       "LOCAL_FILE",            "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "track_mode",              "Tracking mode",                                                                  "",             "",             "Weather",       "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "tilt",                    "Tilt angle of surface/axis",                                                     "",             "",             "Weather",       "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                 "Azimuth angle of surface/axis",                                                  "",             "",             "Weather",       "*",                       "",                      "" },

	{ SSC_INPUT, SSC_NUMBER, "system_capacity", "Nameplate capacity", "kW", "", "dish", "*", "", "" },


	// sam_pf_dish_collector_type295_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "d_ap",                    "Dish aperture diameter",                                                         "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rho",                     "Mirror surface reflectivity",                                                    "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_ns",                    "Number of collectors North-South",                                               "-",            "",             "type295",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_ew",                    "Number of collectors East-West",                                                 "-",            "",             "type295",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "ns_dish_sep",             "Collector separation North-South",                                               "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ew_dish_sep",             "Collector separation East-West",                                                 "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "slope_ns",                "North-South ground slope",                                                       "%",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "slope_ew",                "East-West ground slope",                                                         "%",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "w_slot_gap",              "Slot gap width",                                                                 "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_slot_gap",              "Slot gap height",                                                                "m",            "",             "type295",       "*",                       "",                      "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Dish manufacturer (fixed as 5 = other)",                                         "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "wind_stow_speed",         "Wind stow speed",                                                                "m/s",          "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_proj",                  "Projected mirror area",                                                          "m^2",          "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "Insolation cut in value",                                                        "W/m^2",        "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_ap_test",               "Receiver aperture diameter during test",                                         "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_if",                 "Test intercept factor",                                                          "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_L_focal",            "Focal length of mirror system",                                                  "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_total",                 "Total Area",                                                                     "m^2",          "",             "type295",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "I_beam",                  "Direct normal radiation",                                                        "kJ/hr-m2",     "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Dry bulb temperature",                                                           "C",            "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "wind_speed",              "Wind velocity",                                                                  "m/s",          "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "zenith",                  "Solar zenith angle",                                                             "deg",          "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_atm",                   "Atmospheric pressure",                                                           "Pa",           "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                 "Solar azimuth angle",                                                            "deg",          "",             "type295",       "*",                       "",                      "" },

	// sam_pf_dish_receiver_type296_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "rec_type",                "Receiver type (always = 1)",                                                     "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "transmittance_cover",     "Transmittance cover (always = 1)",                                               "-",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (always=5)",                                                        "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "alpha_absorber",          "Absorber absorptance",                                                           "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_absorber",              "Absorber surface area",                                                          "m^2",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "alpha_wall",              "Cavity absorptance",                                                             "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_wall",                  "Cavity surface area",                                                            "m^2",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_insulation",            "Insulation thickness",                                                           "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "k_insulation",            "Insulation thermal conductivity",                                                "W/m-K",        "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_cav",                   "Internal diameter of cavity perp to aperture",                                   "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cav",                   "Internal cavity pressure with aperture covered",                                 "kPa",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_cav",                   "Internal depth of cavity perp to aperture",                                      "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DELTA_T_DIR",             "Delta temperature for DIR receiver",                                             "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DELTA_T_REFLUX",          "Delta temp for REFLUX receiver (always = 40)",                                   "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_heater_head_high",      "Heater Head Set Temperature",                                                    "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type296",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "Power_in_rec",            "Power entering the receiver from the collector",                                 "kW",           "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_atm",                   "Atmospheric pressure",                                                           "Pa",           "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "wind_speed",              "Wind velocity",                                                                  "m/s",          "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "sun_angle",               "Solar altitude angle",                                                           "deg",          "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "n_collectors",            "Total number of collectors (Num N-S x Num E-W)",                                 "-",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "DNI",                     "Direct normal radiation",                                                        "W/m^2",        "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "The cut-in DNI value used in the simulation",                                    "W/m^2",        "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "d_ap",                    "The aperture diameter used in the simulation",                                   "m",            "",             "type296",       "*",                       "",                      "" },

	// sam_pf_dish_engine_type297_variables
	//PARAMETERS
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (fixed as 5)",                                                      "-",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_high",      "Heater Head Set Temperature",                                                    "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_const_coef",        "Beale Constant Coefficient",                                                     "-",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_first_coef",        "Beale first-order coefficient",                                                  "1/W",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_square_coef",       "Beale second-order coefficient",                                                 "1/W^2",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_third_coef",        "Beale third-order coefficient",                                                  "1/W^3",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_fourth_coef",       "Beale fourth-order coefficient",                                                 "1/W^4",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pressure_coef",           "Pressure constant coefficient",                                                  "MPa",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pressure_first",          "Pressure first-order coefficient",                                               "MPa/W",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "engine_speed",            "Engine operating speed",                                                         "rpm",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_displaced",             "Displaced engine volume",                                                        "m3",           "",             "type297",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "P_SE",                    "Receiver output power",                                                          "kW",           "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "N_cols",                  "Number of collectors",                                                           "-",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_compression_in",        "Receiver efficiency",                                                            "C",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_operate",   "Receiver head operating temperature",                                            "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_in_collector",          "Power incident on the collector",                                                "kW",           "",             "type297",       "*",                       "",                      "" },

	//sam_pf_dish_parasitics_type298_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "cooling_tower_on",        "Option to use a cooling tower (set to 0=off)",                                   "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_mode",              "Cooling tower type (natural or forced draft)",                                   "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_pipe_tower",            "Runner pipe diameter to the cooling tower (set to 0.4m)",                        "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_m_dot_water",       "Tower cooling water flow rate (set to 134,000 kg/hr)",                           "kg/s",         "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_m_dot_water_test",  "Test value for the cooling water flow rate (set to 134,000 kg/hr)",              "kg/s",         "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_pipe_material",     "Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel)",              "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_tower_pump",          "Tower pump efficiency (set to 0.6)",                                             "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_control_signal",      "Fan control signal (set to 1, not used in this model)",                          "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_power_test",      "Test value for cooling tower effectiveness (set to 0.7)",                        "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "system_availability",     "System availability (set to 1.0)",                                               "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pump_speed",              "Reference Condition Pump Speed",                                                 "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed1",              "Cooling system fan speed 1",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed2",              "Cooling system fan speed 2",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed3",              "Cooling system fan speed 3",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cool_speed2",           "Cooling Fluid Temp. For Fan Speed 2 Cut-In",                                     "C",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cool_speed3",           "Cooling Fluid Temp. For Fan Speed 3 Cut-In",                                     "C",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_cooler_test",     "Cooler effectiveness",                                                           "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_radiator_test",   "Radiator effectiveness",                                                         "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cooling_fluid",           "Reference Condition Cooling Fluid: 1=Water,2=V50%EG,3=V25%EG,4=V40%PG,5=V25%PG", "-",            "",             "type298",       "*",                       "INTEGER",               "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (fixed as 5=other)",                                                "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_controls",              "Control System Parasitic Power, Avg.",                                           "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_P_pump",             "Reference Condition Pump Parasitic Power",                                       "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_pump_speed",         "Reference Condition Pump Speed",                                                 "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_cooling_fluid",      "Reference Condition Cooling Fluid",                                              "-",            "",             "type298",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_T_fluid",            "Reference Condition Cooling Fluid Temperature",                                  "K",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_V_dot_fluid",        "Reference Condition Cooling Fluid Volumetric Flow Rate",                         "gpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_P_fan",              "Reference Condition Cooling System Fan Power",                                   "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_speed",          "Reference Condition Cooling System Fan Speed",                                   "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_rho_air",        "Reference condition fan air density",                                            "kg/m^3",       "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_cfm",            "Reference condition van volumentric flow rate",                                  "cfm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "b_radiator",              "b_radiator parameter",                                                           "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "b_cooler",                "b_cooler parameter",                                                             "-",            "",             "type298",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "gross_power",             "Stirling engine gross output",                                                   "kW",           "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "N_cols",                  "Number of collectors",                                                           "none",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "DNI",                     "Direct normal radiation (not interpolated)",                                     "W/m2",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "V_swept",                 "Displaced engine volume",                                                        "cm3",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "frequency",               "Engine frequency (= RPM/60s)",                                                   "1/s",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "engine_pressure",         "Engine pressure",                                                                "Pa",           "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "Cut in DNI value used in the simulation",                                        "W/m2",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "Q_reject",                "Stirling engine losses",                                                         "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Tower_water_outlet_temp", "Tower water outlet temperature (set to 20)",                                       "C",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_amb_Pa",                "Atmospheric pressure",                                                           "Pa",           "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ns_dish_separation",      "North-South dish separation used in the simulation",                             "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ew_dish_separation",      "East-West dish separation used in the simulation",                               "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_tower_fan",             "Tower fan power (set to 0)",                                                     "kJ/hr",        "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "power_in_collector",      "Power incident on the collector",                                                "kW",           "",             "type298",       "*",                       "",                      "" },


// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep

//   VARTYPE            DATATYPE          NAME                       LABEL                                                                UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS

	// output per collector
	{ SSC_OUTPUT,       SSC_ARRAY,       "month",             "Resource Month",                                                  "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Resource Hour of Day",                                            "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Resource Solar Azimuth",                                          "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Resource Solar Zenith",                                           "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Resource Beam normal irradiance",                                 "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Resource Dry bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Resource Wet bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Resource Wind Speed",                                             "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Resource Pressure",                                               "mbar",         "",            "weather",        "*",                       "LENGTH=8760",           "" },

	
	{ SSC_OUTPUT,       SSC_ARRAY,       "Phi_shade",                "Collector shading efficiency",                                    "",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Collector_Losses",         "Collector loss total",                                            "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_collector",            "Collector efficiency",                                              "",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Power_in_collector",       "Collector thermal power incident",                                "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Power_out_col",            "Collector thermal power produced",                                "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Power_in_rec",             "Receiver thermal power input",                                    "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_out_rec",                "Receiver thermal power output",                                   "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_rec_losses",             "Receiver thermal power loss",                                     "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_rec",                  "Receiver efficiency",                                               "",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_heater_head_operate",    "Receiver temperature - head operating",                            "K",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	
    { SSC_OUTPUT,       SSC_ARRAY,       "net_power",                "Engine power output (net)",                                       "kWe",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_out_SE",                 "Engine power output (gross)",                                     "kWe",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_SE_losses",              "Engine power loss",                                               "kWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_SE",                   "Engine efficiency",                                               "",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "engine_pressure",          "Engine pressure",                                                   "Pa",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_compression",            "Engine compression temperature",                                  "K",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tower_out",              "Cooling fluid temperature - cooler in/tower out",                 "C",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tower_in",               "Cooling fluid temperature - cooler out/tower in",                 "C",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	
    
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_parasitic",              "Parasitic power",                                         "We",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },

	// totaled for all collectors
    { SSC_OUTPUT,       SSC_ARRAY,       "eta_net",                  "System total: Net efficiency",                                    "",            "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_Power_in_collector","System total: Collector thermal power incident",                  "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_Power_out_col",     "System total: Collector thermal power produced",                  "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_Collector_Losses",  "System total: Collector loss total",                              "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_Power_in_rec",      "System total: Receiver thermal power input",                      "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_Q_rec_losses",      "System total: Receiver thermal loss",                             "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_P_out_rec",         "System total: Receiver thermal power output",                     "MWt",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_P_out_SE",          "System total: Engine power output (gross)",                                      "MWe",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_P_parasitic",       "System total: Parasitic power",                                        "MWe",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },
	
//    { SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",            "Hourly Energy",                                                     "kWe",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },

	// annual outputs
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",            "Annual Energy",                                                     "kWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_Power_in_collector","Power incident on the collector",                                   "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_Power_out_col",     "Total power from the collector dish",                               "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_Power_in_rec",      "Power entering the receiver from the collector",                    "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_P_out_rec",         "Receiver output power",                                             "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_P_out_SE",          "Stirling engine gross output",                                      "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_Collector_Losses",  "Total collector losses (Incident - P_out)",                         "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_P_parasitic",       "Total parasitic power load",                                        "MWh",           "",             "Outputs",        "*",                       "",                    "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_Q_rec_losses",      "Receiver thermal losses",                                           "MWh",           "",             "Outputs",        "*",                       "",                    "" },

	// monthly outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",            "Monthly Energy",                                                   "kWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_Power_in_collector","Power incident on the collector",                                  "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_Power_out_col",     "Total power from the collector dish",                              "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_Power_in_rec",      "Power entering the receiver from the collector",                   "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_P_out_rec",         "Receiver output power",                                            "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_P_out_SE",          "Stirling engine gross output",                                     "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_Collector_Losses",  "Total collector losses (Incident - P_out)",                        "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_P_parasitic",       "Total parasitic power load",                                       "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_Q_rec_losses",      "Receiver thermal losses",                                          "MWh",           "",             "Outputs",        "*",                       "LENGTH=12",           "" },

	{ SSC_OUTPUT, SSC_NUMBER, "conversion_factor", "Gross to Net Conversion Factor", "%", "", "Calculated", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "First year kWh/kW", "kWh/kW", "", "", "*", "", "" },


	var_info_invalid };



class cm_tcsdish : public tcKernel
{
public:

	cm_tcsdish(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsdish );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{
		//if ( 0 >= load_library("typelib") ) throw exec_error( "tcsdish", util::format("could not load the tcs type library.") );

		bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		//Add units
		int type295_collector = add_unit("sam_pf_dish_collector_type295");
		int type296_receiver = add_unit("sam_pf_dish_receiver_type296");
		int type297_dishengine = add_unit("sam_pf_dish_engine_type297");
		int type298_dishparasitics = add_unit("sam_pf_dish_parasitics_type298");

		// set weather reader parameters/inputs
		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcsdata/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
			set_unit_value( weather, "i_hour", "TIME" );
			set_unit_value( weather, "i_month", "month" );
			set_unit_value( weather, "i_day", "day" );
			set_unit_value( weather, "i_global", "GlobalHorizontal" );
			set_unit_value( weather, "i_beam", "DNI" );
			set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
			set_unit_value( weather, "i_tdry", "T_dry" );
			set_unit_value( weather, "i_twet", "T_wet" );
			set_unit_value( weather, "i_tdew", "T_dew" );
			set_unit_value( weather, "i_wspd", "WindSpeed" );
			set_unit_value( weather, "i_wdir", "WindDir" );
			set_unit_value( weather, "i_rhum", "RelHum" );
			set_unit_value( weather, "i_pres", "AtmPres" );
			set_unit_value( weather, "i_snow", "SnowCover" );
			set_unit_value( weather, "i_albedo", "GroundAlbedo" );
			set_unit_value( weather, "i_poa", "POA" );
			set_unit_value( weather, "i_solazi", "Azimuth" );
			set_unit_value( weather, "i_solzen", "Zenith" );
			set_unit_value( weather, "i_lat", "Latitude" );
			set_unit_value( weather, "i_lon", "Longitude" );
			set_unit_value( weather, "i_shift", "Shift" );
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode", 1 );    //, 1 );
			set_unit_value_ssc_double( weather, "tilt", 0.0 );          //, 0 );
			set_unit_value_ssc_double( weather, "azimuth", 180.0 );       //, 0 );
		}

		// Set Collector Parameters
		set_unit_value_ssc_double( type295_collector, "d_ap"); //  d_ap );
		set_unit_value_ssc_double( type295_collector, "rho"); //  reflectivity );
		set_unit_value_ssc_double( type295_collector, "n_ns"); //  NNS );
		set_unit_value_ssc_double( type295_collector, "n_ew"); //  NEW );
		set_unit_value_ssc_double( type295_collector, "ns_dish_sep"); //  NS_dish_sep );
		set_unit_value_ssc_double( type295_collector, "ew_dish_sep"); //  EW_dish_sep );
		set_unit_value_ssc_double( type295_collector, "slope_ns"); //  slope_NS );
		set_unit_value_ssc_double( type295_collector, "slope_ew"); //  slope_EW );
		set_unit_value_ssc_double( type295_collector, "w_slot_gap"); //  width_slot_gap );
		set_unit_value_ssc_double( type295_collector, "h_slot_gap"); //  height_slot_gap );
		set_unit_value_ssc_double( type295_collector, "manufacturer", 5 );
		set_unit_value_ssc_double( type295_collector, "wind_stow_speed"); //  wind_stow_speed );
		set_unit_value_ssc_double( type295_collector, "A_proj"); //  proj_area );
		set_unit_value_ssc_double( type295_collector, "I_cut_in"); //  i_cut_in );
		set_unit_value_ssc_double( type295_collector, "d_ap_test"); //  d_ap_TEST );
		set_unit_value_ssc_double( type295_collector, "test_if"); //  test_intercept_f );
		set_unit_value_ssc_double( type295_collector, "test_L_focal"); //  test_focal_length );
		set_unit_value_ssc_double( type295_collector, "A_total"); //  total_area );

		// Connect Collector Parameters
		bool bConnected = connect( weather, "beam", type295_collector, "I_beam" );
		bConnected &= connect( weather, "tdry", type295_collector, "T_amb" );
		bConnected &= connect( weather, "wspd", type295_collector, "wind_speed" );
		bConnected &= connect( weather, "solzen", type295_collector, "zenith" );
		bConnected &= connect( weather, "pres", type295_collector, "P_atm" );
		bConnected &= connect( weather, "solazi", type295_collector, "azimuth" );

		// Set Receiver Parameters
		set_unit_value_ssc_double( type296_receiver, "rec_type"); //  rec_type );
		set_unit_value_ssc_double( type296_receiver, "transmittance_cover"); //  trans_cover );
		set_unit_value_ssc_double( type296_receiver, "manufacturer", 5 );
		set_unit_value_ssc_double( type296_receiver, "alpha_absorber"); //  alpha_absorb );
		set_unit_value_ssc_double( type296_receiver, "A_absorber"); //  A_absorb );
		set_unit_value_ssc_double( type296_receiver, "alpha_wall"); //  alpha_wall );
		set_unit_value_ssc_double( type296_receiver, "A_wall"); //  A_wall );
		set_unit_value_ssc_double( type296_receiver, "L_insulation"); //  L_insul );
		set_unit_value_ssc_double( type296_receiver, "k_insulation"); //  k_insul );
		set_unit_value_ssc_double( type296_receiver, "d_cav"); //  d_cav );
		set_unit_value_ssc_double( type296_receiver, "P_cav"); //  p_cav );
		set_unit_value_ssc_double( type296_receiver, "L_cav"); //  l_cav );
		set_unit_value_ssc_double( type296_receiver, "DELTA_T_DIR"); //  delta_T_DIR );
		set_unit_value_ssc_double( type296_receiver, "DELTA_T_REFLUX"); //  delta_T_reflux );
		set_unit_value_ssc_double( type296_receiver, "T_heater_head_high"); //  T_heater_head_high );
		set_unit_value_ssc_double( type296_receiver, "T_heater_head_low"); //  T_heater_head_low );

		// Connect Receiver Inputs
		bConnected &= connect( type295_collector, "Power_in_rec", type296_receiver, "Power_in_rec" );
		bConnected &= connect( weather, "tdry", type296_receiver, "T_amb" );
		bConnected &= connect( weather, "pres", type296_receiver, "P_atm" );
		bConnected &= connect( weather, "wspd", type296_receiver, "wind_speed" );
		bConnected &= connect( weather, "solzen", type296_receiver, "sun_angle" );
		bConnected &= connect( type295_collector, "Number_of_collectors", type296_receiver, "n_collectors" );
		bConnected &= connect( weather, "beam", type296_receiver, "DNI" );
		bConnected &= connect( type295_collector, "I_cut_in", type296_receiver, "I_cut_in" );
		bConnected &= connect( type295_collector, "d_ap_out", type296_receiver, "d_ap" );

		// Set Engine Parameters
		set_unit_value_ssc_double( type297_dishengine, "manufacturer",  5 );
		set_unit_value_ssc_double( type297_dishengine, "T_heater_head_high"); //  T_heater_head_high );
		set_unit_value_ssc_double( type297_dishengine, "T_heater_head_low"); //  T_heater_head_low );
		set_unit_value_ssc_double( type297_dishengine, "Beale_const_coef"); //  Beale_const_coef );
		set_unit_value_ssc_double( type297_dishengine, "Beale_first_coef"); //  Beale_first_coef );
		set_unit_value_ssc_double( type297_dishengine, "Beale_square_coef"); //  Beale_square_coef );
		set_unit_value_ssc_double( type297_dishengine, "Beale_third_coef"); //  Beale_third_coef );
		set_unit_value_ssc_double( type297_dishengine, "Beale_fourth_coef"); //  Beale_fourth_coef );
		set_unit_value_ssc_double( type297_dishengine, "Pressure_coef"); //  Pressure_coef );
		set_unit_value_ssc_double( type297_dishengine, "Pressure_first"); //  Pressure_first );
		set_unit_value_ssc_double( type297_dishengine, "engine_speed"); //  engine_speed );
		set_unit_value_ssc_double( type297_dishengine, "V_displaced"); //  V_displaced );
		// initial values
		set_unit_value_ssc_double( type297_dishengine, "T_compression", as_double("T_compression_in") ); //  273.15 );

		// Connect Engine Parameters
		bConnected &= connect( type296_receiver, "P_out_rec", type297_dishengine, "P_SE" );
		bConnected &= connect( weather, "tdry", type297_dishengine, "T_amb" );
		bConnected &= connect( type295_collector, "Number_of_collectors", type297_dishengine, "N_cols" );
		bConnected &= connect( type298_dishparasitics, "T_compression", type297_dishengine, "T_compression" );
		bConnected &= connect( type296_receiver, "T_heater_head_operate", type297_dishengine, "T_heater_head_operate" );
		bConnected &= connect( type295_collector, "Power_in_collector", type297_dishengine, "P_in_collector" );

		// Set Parasitic Parameters
		set_unit_value_ssc_double( type298_dishparasitics, "cooling_tower_on"); //  0 );
		set_unit_value_ssc_double( type298_dishparasitics, "tower_mode"); //  1 );
		set_unit_value_ssc_double( type298_dishparasitics, "d_pipe_tower"); //  0.4 );
		set_unit_value_ssc_double( type298_dishparasitics, "tower_m_dot_water"); //  134000 );
		set_unit_value_ssc_double( type298_dishparasitics, "tower_m_dot_water_test"); //  134000 );
		set_unit_value_ssc_double( type298_dishparasitics, "tower_pipe_material"); //  1 );
		set_unit_value_ssc_double( type298_dishparasitics, "eta_tower_pump"); //  0.6 );
		set_unit_value_ssc_double( type298_dishparasitics, "fan_control_signal"); //  1 );
		set_unit_value_ssc_double( type298_dishparasitics, "epsilon_power_test"); //  0.7 );
		set_unit_value_ssc_double( type298_dishparasitics, "system_availability"); //  SYS_AVAIL );
		set_unit_value_ssc_double( type298_dishparasitics, "pump_speed"); //  PUMPSPEED );
		set_unit_value_ssc_double( type298_dishparasitics, "fan_speed1"); //  FAN_SPD1 );
		set_unit_value_ssc_double( type298_dishparasitics, "fan_speed2"); //  FAN_SPD2 );
		set_unit_value_ssc_double( type298_dishparasitics, "fan_speed3"); //  FAN_SPD3 );
		set_unit_value_ssc_double( type298_dishparasitics, "T_cool_speed2"); //  T_cool_speed2 );
		set_unit_value_ssc_double( type298_dishparasitics, "T_cool_speed3"); //  T_cool_speed3 );
		set_unit_value_ssc_double( type298_dishparasitics, "epsilon_cooler_test"); //  EPS_COOL );
		set_unit_value_ssc_double( type298_dishparasitics, "epsilon_radiator_test"); //  EPS_RADTR );
		set_unit_value_ssc_double( type298_dishparasitics, "cooling_fluid"); //  coolfluid );
		set_unit_value_ssc_double( type298_dishparasitics, "manufacturer",  5 );
		set_unit_value_ssc_double( type298_dishparasitics, "P_controls"); //  P_controls );
		set_unit_value_ssc_double( type298_dishparasitics, "test_P_pump"); //  TEST_P_pump );
		set_unit_value_ssc_double( type298_dishparasitics, "test_pump_speed"); //  TEST_pump_speed );
		set_unit_value_ssc_double( type298_dishparasitics, "test_cooling_fluid"); //  TEST_coolfluid );
		set_unit_value_ssc_double( type298_dishparasitics, "test_T_fluid"); //  TEST_T_fluid );
		set_unit_value_ssc_double( type298_dishparasitics, "test_V_dot_fluid"); //  TEST_V_dot_Fluid );
		set_unit_value_ssc_double( type298_dishparasitics, "test_P_fan"); //  TEST_P_fan );
		set_unit_value_ssc_double( type298_dishparasitics, "test_fan_speed"); //  TEST_fan_speed );
		set_unit_value_ssc_double( type298_dishparasitics, "test_fan_rho_air"); //  TEST_fan_rho_air );
		set_unit_value_ssc_double( type298_dishparasitics, "test_fan_cfm"); //  TEST_fan_CFM );
		set_unit_value_ssc_double( type298_dishparasitics, "b_radiator"); //  b_radiator );
		set_unit_value_ssc_double( type298_dishparasitics, "b_cooler"); //  b_cooler );
		// initial values
		set_unit_value_ssc_double( type298_dishparasitics, "I_cut_in"); //  i_cut_in );
		set_unit_value_ssc_double( type298_dishparasitics, "Tower_water_outlet_temp"); //  0.0 );
		set_unit_value_ssc_double( type298_dishparasitics, "ns_dish_separation"); //  NS_dish_sep );
		set_unit_value_ssc_double( type298_dishparasitics, "ew_dish_separation"); //  NS_dish_sep );
		set_unit_value_ssc_double( type298_dishparasitics, "P_tower_fan"); //  0.0 );

		// Connect Parasitics Inputs
		bConnected &= connect( type297_dishengine, "P_out_SE", type298_dishparasitics, "gross_power" );
		bConnected &= connect( weather, "tdry", type298_dishparasitics, "T_amb" );
		bConnected &= connect( type295_collector, "Number_of_collectors", type298_dishparasitics, "N_cols" );
		bConnected &= connect( weather, "beam", type298_dishparasitics, "DNI" );
		bConnected &= connect( type297_dishengine, "T_heater_head_low", type298_dishparasitics, "T_heater_head_low" );
		bConnected &= connect( type297_dishengine, "V_displaced", type298_dishparasitics, "V_swept" );
		bConnected &= connect( type297_dishengine, "frequency", type298_dishparasitics, "frequency" );
		bConnected &= connect( type297_dishengine, "engine_pressure", type298_dishparasitics, "engine_pressure" );
		bConnected &= connect( type297_dishengine, "P_SE_losses", type298_dishparasitics, "Q_reject" );
		bConnected &= connect( weather, "pres", type298_dishparasitics, "P_amb_Pa" );
		bConnected &= connect( type295_collector, "Power_in_collector", type298_dishparasitics, "power_in_collector" );

		// Example for changing an input variable name in the SSC interface
		// set_unit_value( u3, "m_dot_htf", as_double("m_dot_htf_init") );


		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsdish", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600.0, hours*3600.0, 3600) )
			throw exec_error( "tcsdish", util::format("there was a problem simulating in tcsdish.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsdish", util::format("there was a problem returning the results from the simulation.") );

		//1.7.15, twn: Need to calculated the conversion factor before the performance adjustments are applied to "hourly energy"
		accumulate_annual("net_power", "annual_energy");					// MWh
		accumulate_annual("P_SE_losses", "annual_Power_in_collector");		// MWh USING DUMMY VARIABLE HERE: IS OVERWRITTEN BELOW
		// Calculated outputs
		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_Power_in_collector");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		assign("conversion_factor", convfactor);

		// annual accumulations
		size_t count = 0;
		ssc_number_t *enet = as_array("net_power", &count);
		ssc_number_t *p1 = as_array("Power_in_collector", &count);
		ssc_number_t *p2 = as_array("Power_out_col", &count);
		ssc_number_t *p3 = as_array("Power_in_rec", &count);
		ssc_number_t *p4 = as_array("P_out_rec", &count);
		ssc_number_t *p5 = as_array("P_out_SE", &count);
		ssc_number_t *p6 = as_array("Collector_Losses", &count);
		ssc_number_t *p7 = as_array("P_parasitic", &count); // in Watts
		ssc_number_t *p8 = as_array("Q_rec_losses", &count);
		if (!enet || !p1 || !p2 || !p3 || !p4 || !p5 || !p6 || !p7 || !p8 || count != 8760)
			throw exec_error("tcsdish", "Failed to retrieve hourly data");

		ssc_number_t collectors = as_number("n_ns") * as_number("n_ew");
		ssc_number_t converter = (ssc_number_t)(collectors * 0.001); // convert from kWh per collector to MWh for the field

		ssc_number_t *hourly = allocate("gen", count);
		ssc_number_t *po1 = allocate("hourly_Power_in_collector", count);
		ssc_number_t *po2 = allocate("hourly_Power_out_col", count);
		ssc_number_t *po3 = allocate("hourly_Power_in_rec", count);
		ssc_number_t *po4 = allocate("hourly_P_out_rec", count);
		ssc_number_t *po5 = allocate("hourly_P_out_SE", count);
		ssc_number_t *po6 = allocate("hourly_Collector_Losses", count);
		ssc_number_t *po7 = allocate("hourly_P_parasitic", count);
		ssc_number_t *po8 = allocate("hourly_Q_rec_losses", count);


		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("dish", "failed to setup adjustment factors: " + haf.error());


		for (size_t i = 0; i < count; i++) {
			hourly[i] = enet[i] * collectors*haf(i);
			po1[i] = p1[i] * converter;
			po2[i] = p2[i] * converter;
			po3[i] = p3[i] * converter;
			po4[i] = p4[i] * converter;
			po5[i] = p5[i] * converter;
			po6[i] = p6[i] * converter;
			po7[i] = p7[i] * converter/1000; // Watts to MWatts
			po8[i] = p8[i] * converter;
		}
		accumulate_annual("gen",             "annual_energy");
		accumulate_annual("hourly_Power_in_collector", "annual_Power_in_collector");
		accumulate_annual("hourly_Power_out_col",      "annual_Power_out_col");
		accumulate_annual("hourly_Power_in_rec",       "annual_Power_in_rec");
		accumulate_annual("hourly_P_out_rec",          "annual_P_out_rec");
		accumulate_annual("hourly_P_out_SE",           "annual_P_out_SE");
		accumulate_annual("hourly_Collector_Losses",   "annual_Collector_Losses");
		accumulate_annual("hourly_P_parasitic",        "annual_P_parasitic");
		accumulate_annual("hourly_Q_rec_losses",       "annual_Q_rec_losses");

		// monthly accumulations
		accumulate_monthly("gen",             "monthly_energy");
		accumulate_monthly("hourly_Power_in_collector", "monthly_Power_in_collector");
		accumulate_monthly("hourly_Power_out_col",      "monthly_Power_out_col");
		accumulate_monthly("hourly_Power_in_rec",       "monthly_Power_in_rec");
		accumulate_monthly("hourly_P_out_rec",          "monthly_P_out_rec");
		accumulate_monthly("hourly_P_out_SE",           "monthly_P_out_SE");
		accumulate_monthly("hourly_Collector_Losses",   "monthly_Collector_Losses");
		accumulate_monthly("hourly_P_parasitic",        "monthly_P_parasitic");
		accumulate_monthly("hourly_Q_rec_losses",       "monthly_Q_rec_losses");

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
		{
			annual_energy += hourly[i];
		}
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

	}

};

DEFINE_TCS_MODULE_ENTRY( tcsdish, "Dish Stirling model using the TCS types.", 4 )