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
#include "lib_weatherfile.h"
#include "lib_physics.h"
#include "lib_geothermal.h"
// for adjustment factors
#include "common.h"



static var_info _cm_vtab_geothermal[] = {
//   VARTYPE           DATATYPE         NAME                                   LABEL                                           UNITS             META            GROUP              REQUIRED_IF                 CONSTRAINTS      UI_HINTS
																		       																														             
    // control input													       																														             
    { SSC_INPUT,        SSC_NUMBER,      "ui_calculations_only",               "If = 1, only run UI calculations",             "",               "",             "GeoHourly",        "*",                        "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",          "Geothermal lifetime simulation",              "0/1",     "0=SingleYearRepeated,1=RunEveryYear",     "GeoHourly",             "?=0",           "BOOLEAN",                        "" },


    // climate and resource inputs		 								       											   				     
    { SSC_INPUT,        SSC_STRING,      "file_name",                          "local weather file path",                      "",               "",             "GeoHourly",        "ui_calculations_only=0",   "LOCAL_FILE",      "" },
    { SSC_INPUT,        SSC_NUMBER,      "resource_potential",                 "Resource Potential",                           "MW",             "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "resource_type",                      "Type of Resource",                             "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "resource_temp",                      "Resource Temperature",                         "C",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "resource_depth",                     "Resource Depth",                               "m",              "",             "GeoHourly",        "*",                        "",                "" },
										
    // Other inputs						 								       											   				     
    { SSC_INPUT,        SSC_NUMBER,      "geothermal_analysis_period",         "Analysis Lifetime",                            "years",          "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "model_choice",                       "Which model to run (0,1,2)",                   "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
																		       											   				     														             
    // geothermal plant and equipment									       											   				     														             
    { SSC_INPUT,        SSC_NUMBER,      "specified_pump_work_amount",         "Pump work specified by user",                  "MW",             "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "nameplate",                          "Desired plant output",                         "kW",             "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "analysis_type",                      "Analysis Type",                                "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "num_wells",                          "Number of Wells",                              "",               "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "num_wells_getem",                    "Number of Wells GETEM calc'd",                 "",               "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "conversion_type",                    "Conversion Type",                              "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "plant_efficiency_input",             "Plant efficiency",                             "",               "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "conversion_subtype",                 "Conversion Subtype",                           "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "decline_type",                       "Temp decline Type",                            "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "temp_decline_rate",                  "Temperature decline rate",                     "%/yr",           "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "temp_decline_max",                   "Maximum temperature decline",                  "C",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "wet_bulb_temp",                      "Wet Bulb Temperature",                         "C",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "ambient_pressure",                   "Ambient pressure",                             "psi",            "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "well_flow_rate",                     "Production flow rate per well",                "kg/s",           "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "pump_efficiency",                    "Pump efficiency",                              "%",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "delta_pressure_equip",               "Delta pressure across surface equipment",      "psi",            "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "excess_pressure_pump",               "Excess pressure @ pump suction",               "psi",            "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "well_diameter",                      "Production well diameter",                     "in",             "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "casing_size",                        "Production pump casing size",                  "in",             "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "inj_well_diam",                      "Injection well diameter",                      "in",             "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "design_temp",                        "Power block design temperature",               "C",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "specify_pump_work",                  "Did user specify pump work?",                  "0 or 1",         "",             "GeoHourly",        "*",                        "INTEGER",         "" },
										 																				   				     														             
    // detailed geothermal inputs		 																				   				     														             
    { SSC_INPUT,        SSC_NUMBER,      "rock_thermal_conductivity",          "Rock thermal conductivity",                    "J/m-day-C",      "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "rock_specific_heat",                 "Rock specific heat",                           "J/kg-C",         "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "rock_density",                       "Rock density",                                 "kg/m^3",         "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "reservoir_pressure_change_type",     "Reservoir pressure change type",               "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "reservoir_pressure_change",          "Pressure change",                              "psi-h/1000lb",   "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "reservoir_width",                    "Reservoir width",                              "m",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "reservoir_height",                   "Reservoir height",                             "m",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "reservoir_permeability",             "Reservoir Permeability",                       "darcys",         "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "inj_prod_well_distance",             "Distance from injection to production wells",  "m",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "subsurface_water_loss",              "Subsurface water loss",                        "%",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "fracture_aperature",                 "Fracture aperature",                           "m",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "fracture_width",                     "Fracture width",                               "m",              "",             "GeoHourly",        "*",                        "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "num_fractures",                      "Number of fractures",                          "",               "",             "GeoHourly",        "*",                        "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "fracture_angle",                     "Fracture angle",                               "deg",            "",             "GeoHourly",        "*",                        "",                "" },

    // power block inputs (these could change on an hourly basis, but don't here)

	//   VARTYPE           DATATYPE         NAME                                   LABEL                                              UNITS      META            GROUP               REQUIRED_IF                  CONSTRAINTS      UI_HINTS
																																											     				                
    // power block parameters needed but not on power block SAM input page																									     				                
  //{ SSC_INPUT,        SSC_NUMBER,      "tech_type",                          "Technology type ID",                                  "(1-4)",   "",             "GeoHourly",        "*",                         "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",                     "Outlet design temp",                                  "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",                      "Inlet design temp",                                   "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "HTF",                                "Heat trans fluid type ID",                            "(1-27)",  "",             "GeoHourly",        "ui_calculations_only=0",    "INTEGER",         "" },
																															          										     
    // power block input parameters																							          										     
  //{ SSC_INPUT,        SSC_NUMBER,      "P_ref",                              "Design Output",                                       "MW",      "",             "GeoHourly",        "*",                         "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",                             "Design Boiler Pressure",                              "bar",     "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",                            "Desgin conversion efficiency",                        "%",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",                         "% thermal power for standby mode",                    "%",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",                       "% thermal power for startup",                         "%",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",                       "Hours to start power block",                          "hours",   "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",                         "Blowdown steam fraction",                             "%",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",                          "Design ambient temperature",                          "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                                 "Condenser type (Wet, Dry,Hybrid)",                    "(1-3)",   "",             "GeoHourly",        "ui_calculations_only=0",    "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",                          "Design condenser cooling water inlet/outlet T diff",  "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",                         "Approach Temperature",                                "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",                          "Design ITD for dry system",                           "C",       "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",                       "Condenser pressure ratio",                            "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",                         "Minimum condenser pressure",                          "in Hg",   "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hr_pl_nlev",                         "# part-load increments",                              "(0-9)",   "",             "GeoHourly",        "ui_calculations_only=0",    "INTEGER",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl1",                            "HC Control 1",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl2",                            "HC Control 2",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl3",                            "HC Control 3",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl4",                            "HC Control 4",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl5",                            "HC Control 5",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl6",                            "HC Control 6",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl7",                            "HC Control 7",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl8",                            "HC Control 8",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "hc_ctl9",                            "HC Control 9",                                        "",        "",             "GeoHourly",        "ui_calculations_only=0",    "",                "" },
    // dispatch
    { SSC_INPUT,        SSC_STRING,      "hybrid_dispatch_schedule",           "Daily dispatch schedule",                             "",        "",             "GeoHourly",        "ui_calculations_only=0",    "TOUSCHED",        "" },

	// OUTPUTS
	// VARTYPE           DATATYPE         NAME                                   LABEL                                               UNITS      META            GROUP             REQUIRED_IF                    CONSTRAINTS      UI_HINTS
																																																	             
	// This first batch of outputs is for calculating UI values																																		             
    { SSC_OUTPUT,       SSC_NUMBER,      "num_wells_getem_output",             "Number of wells calculated by GETEM",                 "",        "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "plant_brine_eff",                    "Plant Brine Efficiency",                              "",        "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "gross_output",                       "Gross output from GETEM",                             "",        "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "pump_depth_ft",                      "Pump depth calculated by GETEM",                      "ft",      "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "pump_hp",                            "Pump hp calculated by GETEM",                         "hp",      "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "reservoir_pressure",                 "Reservoir pres calculated by GETEM",                  "",        "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "reservoir_avg_temp",                 "Avg reservoir temp calculated by GETEM",              "C",       "",             "GeoHourly",        "ui_calculations_only=1",   "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "bottom_hole_pressure",               "Bottom hole pres calculated by GETEM",                "",        "",             "GeoHourly",        "ui_calculations_only=1",    "",                "" },
																																																	             
    // pump work is an output for both the UI call and the model run																																             
    { SSC_OUTPUT,       SSC_NUMBER,      "pump_work",                          "Pump work calculated by GETEM",                       "MW",      "",             "GeoHourly",        "*",                        "",                "" },
																																																	             
    // The array outputs are only meaningful when the model is run (not UI calculations)																											             
    // User can specify whether the analysis should be done hourly or monthly.  With monthly analysis, there are only monthly results.																             
    // With hourly analysis, there are still monthly results, but there are hourly (over the whole lifetime of the project) results as well.														             
//	{ SSC_OUTPUT, SSC_ARRAY, "annual_replacements", "Resource replacement? (1=yes)", "kWhac", "", "GeoHourly", "ui_calculations_only=0", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "gen",                                "System power generated",                              "kW",                      "GeoHourly",        "",                         "",                "",                               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "system_lifetime_recapitalize",       "Resource replacement? (1=yes)", "", "", "GeoHourly", "ui_calculations_only=0", "", "" },

    { SSC_OUTPUT,       SSC_ARRAY,      "monthly_resource_temperature",       "Monthly avg resource temperature",                    "C",       "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "monthly_power",                      "Monthly power",                                       "kW",      "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "monthly_energy",                     "Monthly energy before performance adjustments",       "kWh",     "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
																																													   			                 
    { SSC_OUTPUT,       SSC_ARRAY,      "timestep_resource_temperature",      "Resource temperature in each time step",              "C",       "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "timestep_test_values",               "Test output values in each time step",                "",        "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
																																													   			                 
    { SSC_OUTPUT,       SSC_ARRAY,      "timestep_pressure",                  "Atmospheric pressure in each time step",              "atm",     "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "timestep_dry_bulb",                  "Dry bulb temperature in each time step",              "C",       "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "timestep_wet_bulb",                  "Wet bulb temperature in each time step",              "C",       "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
																																																	             
    { SSC_OUTPUT,       SSC_NUMBER,     "lifetime_output",                    "Lifetime Output",                                     "kWh",     "",             "GeoHourly",        "ui_calculations_only=0",   "",                "" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"first_year_output",				  "First Year Output",									 "kWh",		"",				 "GeoHourly",		 "ui_calculations_only=0",	 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"annual_energy",					  "Annual Energy",										"kWh",		"",				"GeoHourly",		 "ui_calculations_only=0",	"",					"" },


	{ SSC_OUTPUT,		SSC_NUMBER,		"capacity_factor",					  "Capacity factor",									"",			"",					 "",					"*",				"",					"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "kwh_per_kw",						  "First year kWh/kW",									 "",		"",					 "",					 "*",				"",					"" },
	
		//Outputs Used in cmod_geothermal_costs:
	{ SSC_OUTPUT,       SSC_NUMBER,		"eff_secondlaw",                        "Second Law Efficiency",							 "C",		"",             "GeoHourly",				 "",                 "",                "" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"qRejectTotal",							"Total Heat Rejection",								 "btu/h",	"",				"GeoHourly",				 "",				 "",				""},
	{ SSC_OUTPUT,		SSC_NUMBER,		"qCondenser",							"Condenser Heat Rejected",							 "btu/h",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"hp_flash_pressure",					"HP Flash Pressure",								 "psia",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"lp_flash_pressure",					"LP Flash Pressure",								 "psia",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"v_stage_1",							"Vacumm Pump Stage 1",								 "kW",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"v_stage_2",							"Vacumm Pump Stage 2",								 "kW",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"v_stage_3",							"Vacumm Pump Stage 3",								 "kW",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"GF_flowrate",							"GF Flow Rate",										 "lb/h",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"qRejectByStage_1",						"Heat Rejected by NCG Condenser Stage 1",			"BTU/h",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"qRejectByStage_2",						"Heat Rejected by NCG Condenser Stage 2",			"BTU/h",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"qRejectByStage_3",						"Heat Rejected by NCG Condenser Stage 3",			"BTU/h",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"ncg_condensate_pump",					"Condensate Pump Work",								"kW",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"cw_pump_work",							"CW Pump Work",										"kW",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"pressure_ratio_1",						"Suction Steam Ratio 1",							"",			"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"pressure_ratio_2",						"Suction Steam Ratio 2",							"",			"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"pressure_ratio_3",						"Suction Steam Ratio 3",							"",			"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"condensate_pump_power",				"hp",												"",			"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"cwflow",								"Cooling Water Flow",								"lb/h",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"cw_pump_head",							"Cooling Water Pump Head",							"lb/h",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"spec_vol",								"HP Specific Volume",								"cft/lb",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"spec_vol_lp",							"LP Specific Volume",								"cft/lb",	"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"x_hp",									"HP Mass Fraction",									"%",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"x_lp",									"LP Mass Fraction",									"%",		"",				"GeoHourly",				 "",				 "",				"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"flash_count",							"Flash Count",										"(1 -2)",	"",				"GeoHourly",				 "",				 "",				"" },
var_info_invalid };


static bool my_update_function( float percent, void *data )
{
	if ( data != 0 ) 
		return ((compute_module*)data)->update("working...", percent);
	else 
		return true;
}

class cm_geothermal : public compute_module
{
private:
public:
	
	cm_geothermal()
	{
		add_var_info( _cm_vtab_geothermal );
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
//		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{
		int iControl = as_integer("ui_calculations_only");		 // 0=run full model, 1=just do UI calculations

		// set the geothermal model inputs -------------------------------------
		SGeothermal_Inputs geo_inputs;
		geo_inputs.md_RatioInjectionToProduction = 0.5; // THIS SHOULD BE AN INPUT. ALTHOUGH IT'S FROM THE COST PAGE, IT'S USED IN NON-COST EQUATION
		geo_inputs.md_DesiredSalesCapacityKW = as_double("nameplate");
		geo_inputs.md_NumberOfWells = as_double("num_wells");
		if ( as_integer("analysis_type") == 0)
			geo_inputs.me_cb = POWER_SALES;
		else
			geo_inputs.me_cb = NUMBER_OF_WELLS;

		if ( as_integer("conversion_type") == 0)
			geo_inputs.me_ct = BINARY;
		else if ( as_integer("conversion_type") == 1)
			geo_inputs.me_ct = FLASH;

		switch ( as_integer("conversion_subtype") )
		{
			case 0:	geo_inputs.me_ft = SINGLE_FLASH_NO_TEMP_CONSTRAINT; break;
			case 1:	geo_inputs.me_ft = SINGLE_FLASH_WITH_TEMP_CONSTRAINT; break;
			case 2:	geo_inputs.me_ft = DUAL_FLASH_NO_TEMP_CONSTRAINT; break;
			case 3:	geo_inputs.me_ft = DUAL_FLASH_WITH_TEMP_CONSTRAINT; break;			
		}
		geo_inputs.md_PlantEfficiency = as_double("plant_efficiency_input")/100;

		// temperature decline
		if ( as_integer("decline_type") == 0 )
			geo_inputs.me_tdm = ENTER_RATE;
		else if ( as_integer("decline_type") == 1 )
			geo_inputs.me_tdm = CALCULATE_RATE;
		geo_inputs.md_TemperatureDeclineRate = as_double("temp_decline_rate")/100;
		geo_inputs.md_MaxTempDeclineC = as_double("temp_decline_max");

		// flash inputs
		geo_inputs.md_TemperatureWetBulbC = as_double("wet_bulb_temp");
		geo_inputs.md_PressureAmbientPSI = as_double("ambient_pressure" );

		//pumping parameters
		geo_inputs.md_ProductionFlowRateKgPerS = as_double("well_flow_rate");
		geo_inputs.md_GFPumpEfficiency = as_double("pump_efficiency")/100;
		geo_inputs.md_PressureChangeAcrossSurfaceEquipmentPSI = as_double("delta_pressure_equip");
		geo_inputs.md_ExcessPressureBar = physics::PsiToBar( as_double("excess_pressure_pump") );
		geo_inputs.md_DiameterProductionWellInches = as_double("well_diameter");
		geo_inputs.md_DiameterPumpCasingInches = as_double("casing_size");
		geo_inputs.md_DiameterInjectionWellInches = as_double("inj_well_diam");
		geo_inputs.mb_CalculatePumpWork = ( 1 != as_integer("specify_pump_work") );
		geo_inputs.md_UserSpecifiedPumpWorkKW = as_double("specified_pump_work_amount") * 1000; // entered in MW

		//resource characterization
		if ( as_integer("resource_type") == 0 )
			geo_inputs.me_rt =  HYDROTHERMAL;
		else if ( as_integer("resource_type") == 1 )
			geo_inputs.me_rt = EGS;
		geo_inputs.md_ResourceDepthM = as_double("resource_depth");
		geo_inputs.md_TemperatureResourceC = as_double("resource_temp");
		geo_inputs.me_dc = TEMPERATURE;
		geo_inputs.md_TemperaturePlantDesignC = as_double("design_temp");
		
		

		//reservoir properties
		geo_inputs.md_TemperatureEGSAmbientC = 15.0;
		geo_inputs.md_EGSThermalConductivity = as_double("rock_thermal_conductivity");
		geo_inputs.md_EGSSpecificHeatConstant = as_double("rock_specific_heat");
		geo_inputs.md_EGSRockDensity = as_double("rock_density");
		switch(as_integer("reservoir_pressure_change_type"))
		{
			case 0: geo_inputs.me_pc = ENTER_PC; break;				// pressure change entered by user
			case 1: geo_inputs.me_pc = SIMPLE_FRACTURE; break;		// use fracture flow (EGS only)
			case 2: geo_inputs.me_pc = K_AREA; break;				// permeability * area
		}
		geo_inputs.md_ReservoirDeltaPressure = as_double("reservoir_pressure_change");
		geo_inputs.md_ReservoirWidthM = as_double("reservoir_width");
		geo_inputs.md_ReservoirHeightM = as_double("reservoir_height");
		geo_inputs.md_ReservoirPermeability = as_double("reservoir_permeability");
		geo_inputs.md_DistanceBetweenProductionInjectionWellsM = as_double("inj_prod_well_distance");
		geo_inputs.md_WaterLossPercent = as_double("subsurface_water_loss")/100;
		geo_inputs.md_EGSFractureAperature = as_double("fracture_aperature");
		geo_inputs.md_EGSNumberOfFractures = as_double("num_fractures");
		geo_inputs.md_EGSFractureWidthM = as_double("fracture_width");
		geo_inputs.md_EGSFractureAngle = as_double("fracture_angle");

		// calculate output array sizes
		geo_inputs.mi_ModelChoice = as_integer("model_choice");		 // 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
		
		// set geothermal inputs RE how analysis is done and for how long
		geo_inputs.mi_ProjectLifeYears = 1;
		if (as_boolean("system_use_lifetime_output")) {
			geo_inputs.mi_ProjectLifeYears = as_integer("geothermal_analysis_period");
		}
		if ( geo_inputs.mi_ProjectLifeYears == 0)
			throw general_error("invalid analysis period specified in the geothermal hourly model");

		// Create output object
		SGeothermal_Outputs geo_outputs;

		// --------------------------------------------------------------------------------------------------------------------------
		std::string err_msg;
		
		// in all cases, save the UI values in the outputs
		// needed to have the gross power plant size for capacity factor
		// calculation
		if (FillOutputsForUI(err_msg, geo_inputs, geo_outputs) != 0)
			throw general_error("input error: " + err_msg + ".");

		if (iControl == 1) {
			
		// just doing calculations for the UI, not running the model

			// assign values for UI results
			assign("num_wells_getem_output", var_data((ssc_number_t)geo_outputs.md_NumberOfWells));
			assign("plant_brine_eff", var_data((ssc_number_t)geo_outputs.md_PlantBrineEffectiveness));
			assign("gross_output", var_data((ssc_number_t)geo_outputs.md_GrossPlantOutputMW));

			assign("pump_depth_ft", var_data((ssc_number_t)geo_outputs.md_PumpDepthFt));
			assign("pump_hp", var_data((ssc_number_t)geo_outputs.md_PumpHorsePower));

			assign("reservoir_pressure", var_data((ssc_number_t)geo_outputs.md_PressureChangeAcrossReservoir));
			assign("reservoir_avg_temp", var_data((ssc_number_t)physics::FarenheitToCelcius(geo_outputs.md_AverageReservoirTemperatureF)));
			assign("bottom_hole_pressure", var_data((ssc_number_t)geo_outputs.md_BottomHolePressure));
			
			assign("capacity_factor", var_data((ssc_number_t)(0)));
			assign("kwh_per_kw", var_data((ssc_number_t)0));
			//assign("reservoir_avg_temp", var_data(ssc_number_t)physics::FarenheitToCelsius(geo_outputs.md_AverageReservoirTemperatureF));
			//double nameplate = geo_outputs.md_GrossPlantOutputMW * 1000; // in kW
		}
		else {
			// running the model, we need to specify other inputs
			geo_inputs.md_PotentialResourceMW = as_double("resource_potential");

			// we need to create the SPowerBlockInputs & SPowerBlockParameters and set the inputs

			// Set the power block input values that won't change hourly in geothermal model
			SPowerBlockInputs pbInputs;
			pbInputs.mode = 2;
			if (as_integer("analysis_type") == 0) // used number of wells as calculated by GETEM
				pbInputs.m_dot_htf = as_double("well_flow_rate")* 3600.0 * as_double("num_wells_getem"); // (kg/sec) * (sec/hour) * (# wells) = total flow (kg/hour)
			else // use number of wells input by user
				pbInputs.m_dot_htf = as_double("well_flow_rate")* 3600.0 * as_double("num_wells"); // (kg/sec) * (sec/hour) * (# wells) = total flow (kg/hour)
			pbInputs.demand_var = pbInputs.m_dot_htf;
			pbInputs.standby_control = 1;
			pbInputs.rel_humidity = 0.7;
			// pbInputs.f_restart = 1.0;		// twn 12.6.12 - f_restart no longer used in Type 224

			// hybrid dispatch schedule, which will set the value for pbInputs.TOU
			const char *sched = as_string("hybrid_dispatch_schedule");
			int tou[8760];
			if (!util::translate_schedule(tou, sched, sched, 0, 8))
				throw general_error("could not translate schedule for time-of-use rate");

			// weather file and time-of-use data
			geo_inputs.mc_WeatherFileName = as_string("file_name");
			geo_inputs.mia_tou = tou;


			// Set power block parameters (parameters don't change hourly)
			SPowerBlockParameters pbp;

			// power block parameters NOT on the SAM power block input page
			// tech_type is a flag for which coefficient set to use in the power block (1=tower,2=trough,3=Sliding pressure power cycle formulation, 4=geothermal)
			pbp.tech_type = 4; //as_integer("tech_type");
			// the geothermal model is only valid with tech type = 4.  if it's set to any other value, use it as a flag here
			// bool bFlag = (as_integer("tech_type") == 4);
			pbp.T_htf_cold_ref = as_double("T_htf_cold_ref");	// design outlet fluid temp
			pbp.T_htf_hot_ref = as_double("T_htf_hot_ref");		// design inlet fluid temp
			pbp.HTF = as_integer("HTF");						// heat transfer fluid type - set in interface, but no user input

			// power block parameters that ARE on the SAM power block input page
			pbp.P_ref = as_double("nameplate") / 1000; // P_ref wants MW, 'nameplate' in kW
			pbp.P_boil = as_double("P_boil");
			pbp.eta_ref = as_double("eta_ref");
			pbp.q_sby_frac = as_double("q_sby_frac");
			pbp.startup_frac = as_double("startup_frac");
			pbp.startup_time = as_double("startup_time");
			pbp.pb_bd_frac = as_double("pb_bd_frac");
			pbp.T_amb_des = as_double("T_amb_des");
			pbp.CT = as_integer("CT");
			pbp.dT_cw_ref = as_double("dT_cw_ref");
			pbp.T_approach = as_double("T_approach");
			pbp.T_ITD_des = as_double("T_ITD_des");
			pbp.P_cond_ratio = as_double("P_cond_ratio");
			pbp.P_cond_min = as_double("P_cond_min");
			pbp.n_pl_inc = as_integer("hr_pl_nlev");
			pbp.F_wc[0] = as_double("hc_ctl1");
			pbp.F_wc[1] = as_double("hc_ctl2");
			pbp.F_wc[2] = as_double("hc_ctl3");
			pbp.F_wc[3] = as_double("hc_ctl4");
			pbp.F_wc[4] = as_double("hc_ctl5");
			pbp.F_wc[5] = as_double("hc_ctl6");
			pbp.F_wc[6] = as_double("hc_ctl7");
			pbp.F_wc[7] = as_double("hc_ctl8");
			pbp.F_wc[8] = as_double("hc_ctl9");

			// since we're going to run the model, we have to allocate the arrays

			// allocate lifetime annual arrays (one element per year, over lifetime of project)
//			geo_outputs.maf_ReplacementsByYear = allocate("annual_replacements", geo_inputs.mi_ProjectLifeYears);
			geo_outputs.maf_ReplacementsByYear = allocate("system_lifetime_recapitalize", geo_inputs.mi_ProjectLifeYears);
			//ssc_number_t *annual_replacements = allocate( "annual_replacements", geo_inputs.mi_ProjectLifeYears);

			// allocate lifetime monthly arrays (one element per month, over lifetime of project)
			geo_outputs.maf_monthly_resource_temp = allocate("monthly_resource_temperature", 12 * geo_inputs.mi_ProjectLifeYears);
			geo_outputs.maf_monthly_power = allocate("monthly_power", 12 * geo_inputs.mi_ProjectLifeYears);
			geo_outputs.maf_monthly_energy = allocate("monthly_energy", 12 * geo_inputs.mi_ProjectLifeYears);

			// allocate lifetime timestep arrays (one element per timestep, over lifetime of project)
			// if this is a monthly analysis, these are redundant with monthly arrays that track same outputs
			geo_inputs.mi_MakeupCalculationsPerYear = (geo_inputs.mi_ModelChoice == 2) ? 8760 : 12;
			geo_inputs.mi_TotalMakeupCalculations = geo_inputs.mi_ProjectLifeYears * geo_inputs.mi_MakeupCalculationsPerYear;

			geo_outputs.maf_timestep_resource_temp = allocate("timestep_resource_temperature", geo_inputs.mi_TotalMakeupCalculations);
			geo_outputs.maf_timestep_power = allocate("timestep_power", geo_inputs.mi_TotalMakeupCalculations);
			geo_outputs.maf_timestep_test_values = allocate("timestep_test_values", geo_inputs.mi_TotalMakeupCalculations);

			geo_outputs.maf_timestep_pressure = allocate("timestep_pressure", geo_inputs.mi_TotalMakeupCalculations);
			geo_outputs.maf_timestep_dry_bulb = allocate("timestep_dry_bulb", geo_inputs.mi_TotalMakeupCalculations);
			geo_outputs.maf_timestep_wet_bulb = allocate("timestep_wet_bulb", geo_inputs.mi_TotalMakeupCalculations);

			size_t n_rec = 8760;
			if (as_boolean("system_use_lifetime_output")){
				n_rec *= geo_inputs.mi_ProjectLifeYears;
			}
	
			geo_outputs.maf_hourly_power = allocate("tmp", n_rec);
			ssc_number_t * p_gen = allocate("gen", n_rec);

			// TODO - implement performance factors 
			adjustment_factors haf(this, "adjust");
			if (!haf.setup())
				throw exec_error("geothermal", "failed to setup adjustment factors: " + haf.error());


			// running
			if (RunGeothermalAnalysis(my_update_function, this, err_msg, pbp, pbInputs, geo_inputs, geo_outputs) != 0)
				throw exec_error("geothermal", "error from geothermal hourly model: " + err_msg + ".");


			//Assigning 2nd Law efficiency:
			//double eff_secondlaw = var_data(static_cast<ssc_number_t>(geo_outputs.eff_secondlaw));
			double eff_secondlaw = geo_outputs.eff_secondlaw;
			assign("eff_secondlaw", (ssc_number_t)eff_secondlaw);

			//Assigning Rejected Total Heat from Flash Plant:
			double qRejectTotal = geo_outputs.qRejectedTotal;	//total heat rejected 
			assign("qRejectTotal", (ssc_number_t)qRejectTotal);

			//Assign qCondenser (Flash Plant Type):
			double qCondenser = geo_outputs.condenser_q;
			assign("qCondenser", (ssc_number_t)qCondenser);

			//Assign HP & LP Flash Pressures: 
			double hp_flash_pressure = geo_outputs.md_PressureHPFlashPSI;
			assign("hp_flash_pressure", (ssc_number_t)hp_flash_pressure);
			double lp_flash_pressure = geo_outputs.md_PressureLPFlashPSI;
			assign("lp_flash_pressure", (ssc_number_t)lp_flash_pressure);

			//Assign all 3 stages of vacuum pump powers:
			double v_stage_1 = geo_outputs.v_stage_1;
			assign("v_stage_1", (ssc_number_t)v_stage_1);
			double v_stage_2 = geo_outputs.v_stage_2;
			assign("v_stage_2", (ssc_number_t)v_stage_2);
			double v_stage_3 = geo_outputs.v_stage_3;
			assign("v_stage_3", (ssc_number_t)v_stage_3);

			//Assign total GF Flow Rate: 
			double GF_flowrate = geo_outputs.GF_flowrate;
			assign("GF_flowrate", (ssc_number_t)GF_flowrate);

			//Assign NCG Condenser Heat Rejecting Stages:
			double qRejectByStage_1 = geo_outputs.qRejectByStage_1;
			assign("qRejectByStage_1", (ssc_number_t)qRejectByStage_1);
			double qRejectByStage_2 = geo_outputs.qRejectByStage_2;
			assign("qRejectByStage_2", (ssc_number_t)qRejectByStage_2);
			double qRejectByStage_3 = geo_outputs.qRejectByStage_3;
			assign("qRejectByStage_3", (ssc_number_t)qRejectByStage_3);

			//Assign NCG Condensate Pump Work & CW Pump Work Value for Calculating NCG Pump Cost: 
			double ncg_condensate_pump = geo_outputs.ncg_condensate_pump;
			assign("ncg_condensate_pump", (ssc_number_t)ncg_condensate_pump);
			double cw_pump_work = geo_outputs.cw_pump_work;
			assign("cw_pump_work", (ssc_number_t)cw_pump_work);

			//Assign steam suction ratio value for NCG Ejector Cost Calculation
			double pressure_ratio_1 = geo_outputs.pressure_ratio_1;
			assign("pressure_ratio_1", (ssc_number_t)pressure_ratio_1);
			double pressure_ratio_2 = geo_outputs.pressure_ratio_2;
			assign("pressure_ratio_2", (ssc_number_t)pressure_ratio_2);
			double pressure_ratio_3 = geo_outputs.pressure_ratio_3;
			assign("pressure_ratio_3", (ssc_number_t)pressure_ratio_3);

			//Assigning Value of Condensate Pump for Pump Cost Calculation:
			double condensate_pump_power = geo_outputs.condensate_pump_power;
			assign("condensate_pump_power", (ssc_number_t)condensate_pump_power);

			//Assign CW Flow and Head for Pump Cost Caclulation:
			double cwflow = geo_outputs.cwflow;
			assign("cwflow", (ssc_number_t)cwflow);
			double cw_pump_head = geo_outputs.cw_pump_head;
			assign("cw_pump_head", (ssc_number_t)cw_pump_head);

			//Assign Specific Volume and Mass Fraction (x) for Flash Vessel Calculations:
			double spec_vol = geo_outputs.spec_vol;	//HP Specific Volume
			assign("spec_vol", (ssc_number_t)spec_vol);
			double x_hp = geo_outputs.getX_hp;
			assign("x_hp", (ssc_number_t)x_hp);
			double spec_vol_lp = geo_outputs.spec_vol_lp;	//LP Specific Volume
			assign("spec_vol_lp", (ssc_number_t)spec_vol_lp);
			double x_lp = geo_outputs.getX_lp;
			assign("x_lp", (ssc_number_t)x_lp);


			

			//Assign Flash Count: 
			double flash_count = geo_outputs.flash_count;
			assign("flash_count", (ssc_number_t)flash_count);

			// Summary calculations
			ssc_number_t total_energy = 0;
			for (size_t i = 0; i < 12 * geo_inputs.mi_ProjectLifeYears; ++i) {
				total_energy += geo_outputs.maf_monthly_energy[i];
			}
			assign("lifetime_output", var_data(total_energy));

			total_energy = 0;
			for (size_t i = 0; i < 12; ++i) {
				total_energy += geo_outputs.maf_monthly_energy[i];
			}
			assign("first_year_output", var_data(total_energy));	

			// metric outputs moved to technology
			double capacity_fac;	//geothermal plant capacity factor
			double kWhperkW = 0.0;
			double nameplate = geo_inputs.md_DesiredSalesCapacityKW; // Was md_GrossPlantOutputMW*1000 -> now it is md_DesiredSalesCapacityKW
			double annual_energy = 0.0;

			//Loop calculates total energy generation over entire project lifetime (in kWh) 
			// Why?  Is the annual energy changing from year to year?
			for (size_t i = 0; i <n_rec ; i++)	{
				annual_energy += geo_outputs.maf_hourly_power[i];
				p_gen[i] = geo_outputs.maf_hourly_power[i];
			}

			if (nameplate > 0) kWhperkW = annual_energy / nameplate;
			capacity_fac = total_energy / nameplate;
			if (geo_inputs.mi_ProjectLifeYears > 0) kWhperkW = kWhperkW / geo_inputs.mi_ProjectLifeYears;

			assign("gross_output", var_data((ssc_number_t)geo_outputs.md_GrossPlantOutputMW));
			assign("capacity_factor", var_data((ssc_number_t)(capacity_fac / 87.6)));		//Divided by 8760 and then multiplied by 100 (or divide by 87.6) to return CF as a %
			assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
			// 5/28/15 average provided for FCR market
			assign("annual_energy", var_data((ssc_number_t)(annual_energy / geo_inputs.mi_ProjectLifeYears)));
		}

		// this assignment happens in UI calculations and model run
		assign("pump_work", var_data((ssc_number_t) geo_outputs.md_PumpWorkKW/1000 ) ); // kW must be converted to MW

	}

};

DEFINE_MODULE_ENTRY( geothermal, "Geothermal monthly and hourly models using general power block code from TRNSYS Type 224 code by M.Wagner, and some GETEM model code.", 3 );

// above is equivalent to:
//static compute_module *_create_geothermal () { return new cm_geothermal; }
//module_entry_info cm_entry_geothermal = { "geothermal", "Geothermal monthly...", 3, _create_geothermal }; 
