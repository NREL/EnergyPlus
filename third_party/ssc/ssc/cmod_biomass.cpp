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
// for adjustment factors
#include "common.h"


static var_info _cm_vtab_biomass[] = {
//	  VARTYPE          DATATYPE          NAME                                          LABEL                                            UNITS            META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
    { SSC_INPUT,       SSC_STRING,       "file_name",                                 "Local weather file path",                        "",              "",      "biopower",          "*",               "LOCAL_FILE",          "" },
																								
	{ SSC_INPUT, SSC_NUMBER, "system_capacity", "Nameplate capacity", "kW", "", "biopower", "*", "", "" },


	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total",                    "Total fuel resource (dt/yr)",                    "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_biomass",            "Total biomass resource (dt/yr)",                 "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_moisture",           "Overall Moisture Content (dry %)",               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_coal",               "Total coal resource (dt/yr)",                    "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_lhv",                "Dry feedstock LHV (Btu/lb)",                     "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_hhv",                "Dry feedstock HHV (Btu/lb)",                     "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_c",                  "Mass fraction carbon",                           "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_biomass_c",          "Biomass fraction carbon",                        "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.total_h",                  "Mass fraction hydrogen",                         "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.bagasse_frac",             "Bagasse feedstock fraction",                     "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.barley_frac",              "Barley feedstock fraction",                      "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.stover_frac",              "Stover feedstock fraction",                      "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.rice_frac",                "Rice straw feedstock fraction",                  "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.wheat_frac",               "Wheat straw feedstock fraction",                 "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.forest_frac",              "Forest residue feedstock fraction",              "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.mill_frac",                "Mill residue feedstock fraction",                "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.mill_c",                   "Carbon fraction in mill residue",                "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.urban_frac",               "Urban wood residue feedstock fraction",          "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.urban_c",                  "Carbon fraction in urban residue",               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.woody_frac",               "Woody energy crop feedstock fraction",           "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.woody_c",                  "Carbon fraction in woody energy crop",           "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.herb_frac",                "Herbaceous energy crop feedstock fraction",      "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.herb_c",                   "Carbon fraction in herbaceous energy crop",      "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.additional_opt",           "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_resource",      "Opt feedstock 1 (dt/yr)",                        "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_resource",      "Opt feedstock 2 (dt/yr)",                        "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_c",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_c",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_h",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_h",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_hhv",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_hhv",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_frac",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_frac",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.bit_frac",                 "Bituminos coal feedstock fraction",              "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.subbit_frac",              "Sub-bituminous coal feedstock fraction",         "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.lig_frac",                 "Lignite coal feedstock fraction",                "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.bagasse_moisture",         "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.barley_moisture",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.stover_moisture",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.rice_moisture",            "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.wheat_moisture",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.forest_moisture",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.mill_moisture",            "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.urban_moisture",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.woody_moisture",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.herb_moisture",            "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock1_moisture",      "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.feedstock2_moisture",      "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.bit_moisture",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.subbit_moisture",          "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.lig_moisture",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstock.collection_radius",        "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//Emissions Comparison																																 
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.avoided_cred",             "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.collection_fuel",          "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_fuel",           "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_legs",           "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_predist",        "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_long",           "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_longmiles",      "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.transport_longopt",        "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.pre_chipopt",              "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.pre_grindopt",             "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.pre_pelletopt",            "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.emissions.grid_intensity",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	// Biopower Plant Specifications																													 
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.drying_method",                "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.drying_spec",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.combustor_type",               "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.air_feed",              "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.flue_temp",             "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.steam_enthalpy",        "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.num",                   "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.cap_per_boiler",        "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.nameplate",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.rated_eff",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.min_load",                     "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.max_over_design",              "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.over_design",           "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.cycle_design_temp",            "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.pl_eff_f0",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.pl_eff_f1",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.pl_eff_f2",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.pl_eff_f3",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.pl_eff_f4",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f0",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f1",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f2",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f3",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f4",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_corr_mode",               "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.temp_eff_f4",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.par_percent",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.tou_option",                   "",                                               "",              "",      "biopower",          "*",               "INTEGER",             "" },
	{ SSC_INPUT,       SSC_ARRAY,        "biopwr.plant.disp.power",                   "",                                               "",              "",      "biopower",          "*",               "LENGTH=9",            "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp1.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp2.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp3.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp4.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp5.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp6.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp7.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp8.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	//{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.disp9.power",                  "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.ramp_rate",                    "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_STRING,       "biopwr.plant.tou_grid",                     "",                                               "",              "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.plant.boiler.steam_pressure",        "",                                               "",              "",      "biopower",          "*",               "",                    "" },

	/*
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.biomass_fuel_used",     "Annual biomass used",                           "dry tons/year", "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.biomass_fuel_cost",     "Annual biomass fuel cost",                      "$",             "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.biomass_fuel_cost_esc", "Annual biomass fuel cost escalation",           "%",             "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.coal_fuel_used",        "Annual coal used",                              "dry tons/year", "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.coal_fuel_cost",        "Annual coal fuel cost",                         "$",             "",      "biopower",          "*",               "",                    "" },
	{ SSC_INPUT,       SSC_NUMBER,       "biopwr.feedstockcost.coal_fuel_cost_esc",    "Annual coal fuel cost escalation",              "%",             "",      "biopower",          "*",               "",                    "" },
	*/


//    OUTPUTS ----------------------------------------------------------------------------								      														   
//	  VARTYPE           DATATYPE         NAME                          LABEL                                                   UNITS            META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
//	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",              "Hourly Energy",                                        "kWh",            "",      "biomass",           "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_q_to_pb",             "Q To Power Block",                                     "kW",            "",      "biomass",           "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_boiler_eff",          "Boiler Efficiency",                                    "",              "",      "biomass",           "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_pbeta",               "Power Block Efficiency",                               "",              "",      "biomass",           "*",               "LENGTH=8760",         "" },

	// monthly
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",             "Monthly Energy",                                       "kWh",           "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_to_pb",            "Q To Power Block",                                     "kWh",           "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_pb_eta",             "Power Block Effiency",                                 "%",             "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_boiler_eff",         "Total Boiler Efficiency - HHV (%)",                    "%",             "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_moist",              "Monthly biomass moisture fraction (dry)",              "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_lhv_heatrate",       "Net Monthly Heat Rate (MMBtu/MWh)",                    "MMBtu/MWh",     "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_hhv_heatrate",       "Gross Monthly Heat Rate (MMBtu/MWh)",                  "MMBtu/MWh",     "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_bagasse_emc",        "Monthly bagasse EMC (dry)",                            "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_barley_emc",         "Monthly barley EMC (dry)",                             "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_stover_emc",         "Monthly stover EMC (dry)",                             "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_rice_emc",           "Monthly rice straw EMC (dry)",                         "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_wheat_emc",          "Monthly wheat straw EMC (dry)",                        "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_forest_emc",         "Monthly forest EMC (dry)",                             "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_mill_emc",           "Monthly mill waste EMC (dry)",                         "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_urban_emc",          "Monthly urban wood waste EMC (dry)",                   "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_woody_emc",          "Monthly woody crop EMC (dry)",                         "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_herb_emc",           "Monthly herbaceous crop EMC (dry)",                    "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_temp_c",             "Temperature",                                          "",              "",      "biomass",           "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_rh",                 "Relative humidity",                                    "",              "",      "biomass",           "*",               "LENGTH=12",           "" },


	// single values
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                        "Annual Energy",                              "kWh",           "",      "biomass",           "*",               "",                    "" },
	/*
	{ SSC_OUTPUT,       SSC_NUMBER,      "om_opt_fuel_1_usage",       "Annual biomass used",                        "dry tons/year", "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "om_opt_fuel_1_cost",                "Annual biomass fuel cost",                   "$",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "om_opt_fuel_1_cost_escal",     "Annual biomass fuel cost escalation",        "%",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "om_opt_fuel_2_usage",       "Annual coal used",                           "dry tons/year", "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "om_opt_fuel_2_cost",                "Annual coal fuel cost",                      "$",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "om_opt_fuel_2_cost_escal",     "Annual coal fuel cost escalation",           "%",             "",      "biomass",           "*",               "",                    "" },
	*/

	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.e_net",                  "Gross Annual Energy",                        "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.biomass",                "Annual biomass usage",                       "dry tons/yr",   "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.coal",                   "Annual coal usage",                          "dry tons/yr",   "",      "biomass",           "*",               "",                    "" },
	//{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.biomass_output",         "Annual output from biomass (kWh)",           "kWh",           "",      "biomass",           "*",               "",                    "" },
	//{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.coal_output",            "Annual output from coal (kWh)",              "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",             "Annual Fuel Usage",                          "kWht",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_watter_usage",            "Annual Water Usage",                         "m3",            "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.ash",                    "Ash produced",                               "tons/yr",       "",      "biomass",           "*",               "",                    "" },

	//System Use
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.capfactor",                     "Annual Capacity Factor (%)",                 "%",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.hhv_heatrate",                  "Gross Heat Rate (MMBtu/MWh)",                "MMBtu/MWh",     "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.lhv_heatrate",                  "Net Heat Rate (MMBtu/MWh)",                  "MMBtu/MWh",     "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",                     "Heat Rate Conversion Factor (MMBTUs/MWhe)",  "MMBTUs/MWhe",   "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.hhv_thermeff",                  "Thermal efficiency, HHV (%)",                "%",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.lhv_thermeff",                  "Thermal efficiency, LHV (%)",                "%",             "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.total_moisture",                "Overall Moisture Content (dry %)",           "%",             "",      "biomass",           "*",               "",                    "" },

	//Emissions Calculations
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.growth",              "Biomass Collection",                         "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.avoided",             "Biomass Avoided Use",                        "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.transport",           "Biomass Transport",                          "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.preprocessing",       "Biomass Preprocessing",                      "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.drying",              "Biomass Drying",                             "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.combustion",          "Combustion",                                 "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.uptake",              "Biomass CO2 Uptake",                         "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.total_sum",           "Biomass Life Cycle CO2",                     "kWh",           "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.diesel",              "Life Cycle Diesel use",                      "Btu/kWh",       "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.biodiesel",           "Life Cycle Biodiesel use",                   "Btu/kWh",       "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.bunker",              "Life Cycle Bunker fuel use",                 "Btu/kWh",       "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.oil",                 "Life Cycle Oil use",                         "Btu/kWh",       "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.naturalgas",          "Life Cycle Natural gas use",                 "Btu/kWh",       "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.nitrogen",            "Life Cycle Nitrogen fertilizer use",         "lb N/kWh",      "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.potassium",           "Life Cycle Potassium fertilizer use",        "lb P2O5/kWh",   "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.phosphorus",          "Life Cycle Phosphorus fertilizer use",       "lb K2O/kWh",    "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.lime",                "Life Cycle Lime fertilizer use",             "lb Lime/kWh",   "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.emissions.ems_per_lb",          "Life Cycle g CO2eq released/lb dry biomass", "",              "",      "biomass",           "*",               "",                    "" },

	//loss diagram outputs
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_fuel_kwh",   "Energy lost in fuel out of boiler",             "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_unburn_kwh", "Energy lost in unburned fuel",                  "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_manu_kwh",   "Energy loss included in manufacturer's margin", "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_rad_kwh",    "Energy loss due to boiler radiation",           "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_dry_kwh",    "Energy lost in hot flue gas",                   "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_wet_kwh",    "Energy lost to moisture in air",                "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.pb_eta_kwh",             "Energy lost in steam turbine and generator",    "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.par_loss_kwh",           "Energy consumed within plant - parasitic load", "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_total_kwh",  "Energy lost in boiler - total",                 "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_output",          "Boiler output",                                 "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.turbine_output",         "Turbine output",                                "kWh",        "",      "biomass",           "*",               "",                    "" },


	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_fuel",       "Energy lost in fuel out of boiler",             "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_unburn",     "Energy lost in unburned fuel",                  "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_manu",       "Energy loss included in manufacturer's margin", "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_rad",        "Energy loss due to boiler radiation",           "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_dry",        "Energy lost in hot flue gas",                   "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_wet",        "Energy lost to moisture in air",                "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.pb_eta",                 "Energy lost in steam turbine and generator",    "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.par_loss",               "Energy consumed within plant - parasitic load", "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.boiler_loss_total",      "Energy lost in boiler - total",                 "%",          "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.qtoboil_tot",            "Q to Boiler",                                   "kWh",        "",      "biomass",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system.annual.qtopb_tot",              "Q to Power Block",                              "kWh",        "",      "biomass",           "*",               "",                    "" },

	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "First year kWh/kW", "kWh/kW", "", "", "*", "", "" },



var_info_invalid };

class cm_biomass : public compute_module
{
private:
public:
	cm_biomass()
	{
		add_var_info( _cm_vtab_biomass );
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{
		static int nday[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };


		//finding tons (wet and dry) of each type of biomass :: to be used in avoided emissions calculations
		//going to generalize feedstock 1 and 2 as "bagasse". units of ton/year
		double dry[10];
		double wet[10];
		double harv_moist[15][12];
		dry[0] = (as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.bagasse_frac")) +
			(as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.feedstock1_frac")) +
			(as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.feedstock2_frac"));
		dry[1] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.barley_frac");
		dry[2] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.stover_frac");
		dry[3] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.rice_frac");
		dry[4] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.wheat_frac");
		dry[5] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.forest_frac");
		dry[6] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.mill_frac");
		dry[7] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.urban_frac");
		dry[8] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.woody_frac");
		dry[9] = as_double("biopwr.feedstock.total") * as_double("biopwr.feedstock.herb_frac");
		wet[0] = dry[0] * (1.0 + (as_double("biopwr.feedstock.bagasse_moisture") / 100.0));
		wet[1] = dry[1] * (1.0 + (as_double("biopwr.feedstock.barley_moisture") / 100.0));
		wet[2] = dry[2] * (1.0 + (as_double("biopwr.feedstock.stover_moisture") / 100.0));
		wet[3] = dry[3] * (1.0 + (as_double("biopwr.feedstock.rice_moisture") / 100.0));
		wet[4] = dry[4] * (1.0 + (as_double("biopwr.feedstock.wheat_moisture") / 100.0));
		wet[5] = dry[5] * (1.0 + (as_double("biopwr.feedstock.forest_moisture") / 100.0));
		wet[6] = dry[6] * (1.0 + (as_double("biopwr.feedstock.mill_moisture") / 100.0));
		wet[7] = dry[7] * (1.0 + (as_double("biopwr.feedstock.urban_moisture") / 100.0));
		wet[8] = dry[8] * (1.0 + (as_double("biopwr.feedstock.woody_moisture") / 100.0));
		wet[9] = dry[9] * (1.0 + (as_double("biopwr.feedstock.herb_moisture") / 100.0));
		double wet_total = 0;
		double dry_total = 0;
		for (int i = 0; i<10; i++){ wet_total += wet[i]; dry_total += dry[i]; }
		double collection_radius = as_double("biopwr.feedstock.collection_radius");
		int transport_fuel = as_integer("biopwr.emissions.transport_fuel"); /*0 if diesel, 1 if biodiesel*/
		int transport_legs = as_integer("biopwr.emissions.transport_legs"); /*0 if 1-stage, 1 if 2-stage*/
		double transport_predist = as_double("biopwr.emissions.transport_predist"); /*distance to preprocessing*/
//		int transport_long = as_integer("biopwr.emissions.transport_long"); /*0 if no long distance option, 1 if yes*/
		double transport_longmiles = as_double("biopwr.emissions.transport_longmiles"); /*max distance for truck transport*/
		int transport_longopt = as_integer("biopwr.emissions.transport_longopt"); /*0 if rail, 1 if barge*/
		int pre_chipopt = as_integer("biopwr.emissions.pre_chipopt"); /*0 if no, 1 if yes*/
		int pre_grindopt = as_integer("biopwr.emissions.pre_grindopt"); /*0 if no, 1 if yes*/
		int pre_pelletopt = as_integer("biopwr.emissions.pre_pelletopt"); /*0 if no, 1 if yes*/
		double frac_c = as_double("biopwr.feedstock.total_c");
		double biomass_frac_c = as_double("biopwr.feedstock.total_biomass_c");
		double waste_c[2];
		waste_c[0] = as_double("biopwr.feedstock.mill_c");
		waste_c[1] = as_double("biopwr.feedstock.urban_c");
		double grid_intensity = as_double("biopwr.emissions.grid_intensity"); /*grid intensity in g CO2 eq/kWh*/

		//ABMA radiation table later used to calculate the radiation losses in the boiler using double linear interpolation
		double rad_table[18][6] =
		{
			{ 1.60, 2.00, 2.67, 3.20, 4.00, 8.00 },
			{ 1.05, 1.31, 1.75, 2.10, 2.62, 5.25 },
			{ 0.84, 1.05, 1.40, 1.68, 2.10, 4.20 },
			{ 0.73, 0.91, 1.22, 1.46, 1.82, 3.65 },
			{ 0.66, 0.82, 1.10, 1.32, 1.65, 3.30 },
			{ 0.62, 0.78, 1.03, 1.24, 1.55, 3.10 },
			{ 0.59, 0.74, 0.98, 1.18, 1.48, 2.95 },
			{ 0.56, 0.70, 0.93, 1.12, 1.40, 2.80 },
			{ 0.54, 0.68, 0.90, 1.08, 1.35, 2.70 },
			{ 0.52, 0.65, 0.87, 1.04, 1.30, 2.60 },
			{ 0.48, 0.60, 0.80, 0.96, 1.20, 2.40 },
			{ 0.45, 0.56, 0.75, 0.90, 1.12, 2.25 },
			{ 0.43, 0.54, 0.72, 0.86, 1.08, 2.15 },
			{ 0.40, 0.50, 0.67, 0.80, 1.00, 2.00 },
			{ 0.38, 0.48, 0.63, 0.76, 0.95, 1.90 },
			{ 0.30, 0.40, 0.50, 0.60, 0.80, 1.50 },
			{ 0.24, 0.29, 0.39, 0.45, 0.58, 1.20 },
			{ 0.18, 0.25, 0.35, 0.38, 0.50, 0.92 }
		};
		//labels for the rows and columns for the radiation table above
		double rad_col[18] = { 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 120.0, 140.0, 160.0, 180.0, 200.0, 400.0, 1000.0, 2000.0 }; /*max output, MBtu/hr*/
		double rad_row[6] = { 100.0, 80.0, 60.0, 50.0, 40.0, 20.0 };												/*percent of max*/

		//giving internal labels to UI variables 
		double lhv = as_double("biopwr.feedstock.total_lhv");
		double hhv = as_double("biopwr.feedstock.total_hhv");
		//wxLogStatus("hhv is " + wxString::Format("%lf", hhv));
		//wxLogStatus("total is " + wxString::Format("%lf", dry_total));
		double hydrogen = as_double("biopwr.feedstock.total_h");
		double boiler_capacity = as_double("biopwr.plant.boiler.cap_per_boiler");
		double steam_enth = as_double("biopwr.plant.boiler.steam_enthalpy");
		double boiler_num = as_double("biopwr.plant.boiler.num");
		double total = as_double("biopwr.feedstock.total");
		double total_biomass = as_double("biopwr.feedstock.total_biomass");
		double total_coal = as_double("biopwr.feedstock.total_coal");
		ssc_number_t max_turb = as_double("biopwr.plant.max_over_design");
		ssc_number_t max_boil = as_double("biopwr.plant.boiler.over_design") / 100.0 + 1.0;
		double steam_pressure = as_double("biopwr.plant.boiler.steam_pressure");
		int tou_opt = as_integer("biopwr.plant.tou_option");
		int tou[8760];
		for (int i = 0; i<8760; i++) tou[i] = 0;
		double ramp_rate = as_double("biopwr.plant.ramp_rate") / 100.0;/*ramp rate in frac/hour*/
		ssc_number_t *disp = 0;
		size_t disp_count = 0;
		if (tou_opt == 1)
		{
			disp = as_array("biopwr.plant.disp.power", &disp_count);
			for (size_t i = 0; i<disp_count; i++)
			{
				if (disp[i]>max_turb)
				{
					throw exec_error("biopower", util::format("fractional generation capacity exceeds turbine design for Period %d", i+1));
					return;
				}
				if (disp[i]>max_boil)
				{
					throw exec_error("biopower", util::format("fractional generation capacity exceeds boiler design for Period %d", i+1));
					return;
				}
			}

			char *sched = (char*)as_string("biopwr.plant.tou_grid");
			if (!util::translate_schedule(tou, sched, sched, 0, 8))
				throw exec_error("biopower", "could not translate schedule for time-of-use rate");

		}

		int dry_opt = as_integer("biopwr.plant.drying_method");
		int burn_opt = as_integer("biopwr.plant.combustor_type");
		double dry_spec = as_double("biopwr.plant.drying_spec") / 100.0;
		double Wdesign = as_double("biopwr.plant.nameplate");
		double rated_eff = as_double("biopwr.plant.rated_eff");
		double min_load = as_double("biopwr.plant.min_load");
		double max_over = as_double("biopwr.plant.max_over_design");
		double design_temp = (as_double("biopwr.plant.cycle_design_temp") - 32.0) * 5.0 / 9.0;
		int temp_mode = as_integer("biopwr.plant.temp_corr_mode");
		double flue_temp = as_double("biopwr.plant.boiler.flue_temp");
		double par = as_double("biopwr.plant.par_percent") / 100.0;
		double air_feed = as_double("biopwr.plant.boiler.air_feed"); /*per B&W 21-5, this is reported as lb dry gas/10000 Btu*/
		double total_heat_in = (total * 2000.0 * hhv) / 1000000.0 / 8760.0; /*in MBtu/hr*/
		double fw_enth_out = (((0.00003958 * flue_temp) + 0.4329) * flue_temp) + 1062.2;

		//Errors
		int add_opt = as_integer("biopwr.feedstock.additional_opt");
		if (add_opt == 1)
		{
			for (int i = 0; i<2; i++)
			{
				double resource = as_double(util::format("biopwr.feedstock.feedstock%d_resource", i + 1));
				if (resource > 0)
				{
					double hv = as_double(util::format("biopwr.feedstock.feedstock%d_hhv", i + 1));
					double hc = as_double(util::format("biopwr.feedstock.feedstock%d_c", i + 1));
					double hh = as_double(util::format("biopwr.feedstock.feedstock%d_h", i + 1));
					if (hv == 0)
					{
						throw exec_error("biopower", util::format("did not fully specify additional Feedstock %d, add HHV", i + 1));
						return;
					}
					if (hc == 0)
					{
						throw exec_error("biopower", util::format("did not fully specify additional Feedstock %d, add carbon content", i + 1));
						return;
					}
					if (hh == 0)
					{
						throw exec_error("biopower", util::format("Did not fully specify additional Feedstock %d, add hydrogen content", i + 1));
						return;
					}
				}
			}
		}

		weatherfile wFile(as_string("file_name"));
		if (!wFile.ok()) throw exec_error("biopower", wFile.message());
		if( wFile.has_message() ) log( wFile.message(), SSC_WARNING);

		//const char *wf = SVal("climate.location");
		//if (!wf)
		//{
		//	Messages.Add("Failed to access weather file.");
		//	return SAMSIM_ERR;
		//}
		//if (ValueLookupError)
		//{
		//	Messages.Add("Discovered internal variable lookup error.");
		//	return SAMSIM_ERR;
		//}

		double etaQ[5];
		double etaT[5];
		double frac[15]; /*this vector contains the fraction of each feedstock, 0=bagasse, etc. as follows*/
		frac[0] = as_double("biopwr.feedstock.bagasse_frac");
		frac[1] = as_double("biopwr.feedstock.barley_frac");
		frac[2] = as_double("biopwr.feedstock.stover_frac");
		frac[3] = as_double("biopwr.feedstock.rice_frac");
		frac[4] = as_double("biopwr.feedstock.wheat_frac");
		frac[5] = as_double("biopwr.feedstock.forest_frac");
		frac[6] = as_double("biopwr.feedstock.mill_frac");
		frac[7] = as_double("biopwr.feedstock.urban_frac");
		frac[8] = as_double("biopwr.feedstock.woody_frac");
		frac[9] = as_double("biopwr.feedstock.herb_frac");
		frac[10] = as_double("biopwr.feedstock.feedstock1_frac");
		frac[11] = as_double("biopwr.feedstock.feedstock2_frac");
		frac[12] = as_double("biopwr.feedstock.bit_frac");
		frac[13] = as_double("biopwr.feedstock.subbit_frac");
		frac[14] = as_double("biopwr.feedstock.lig_frac");

		double moisture[15]; /*this vector contains the fraction of normal moisture that was input into SAM, and will adjust moisture contents accordingly*/
		moisture[0] = as_double("biopwr.feedstock.bagasse_moisture") / 100.0;
		moisture[1] = as_double("biopwr.feedstock.barley_moisture") / 100.0;
		moisture[2] = as_double("biopwr.feedstock.stover_moisture") / 100.0;
		moisture[3] = as_double("biopwr.feedstock.rice_moisture") / 100.0;
		moisture[4] = as_double("biopwr.feedstock.wheat_moisture") / 100.0;
		moisture[5] = as_double("biopwr.feedstock.forest_moisture") / 100.0;
		moisture[6] = as_double("biopwr.feedstock.mill_moisture") / 100.0;
		moisture[7] = as_double("biopwr.feedstock.urban_moisture") / 100.0;
		moisture[8] = as_double("biopwr.feedstock.woody_moisture") / 100.0;
		moisture[9] = as_double("biopwr.feedstock.herb_moisture") / 100.0;
		moisture[10] = as_double("biopwr.feedstock.feedstock1_moisture") / 100.0;
		moisture[11] = as_double("biopwr.feedstock.feedstock2_moisture") / 100.0;
		moisture[12] = as_double("biopwr.feedstock.bit_moisture") / 100.0;
		moisture[13] = as_double("biopwr.feedstock.subbit_moisture") / 100.0;
		moisture[14] = as_double("biopwr.feedstock.lig_moisture") / 100.0;

		for (int i = 0; i<15; i++) /*adjusting moisture content based on input*/
		{
			for (int j = 0; j<12; j++)
			{
				harv_moist[i][j] = moisture[i];
			}
		}


		for (int i = 0; i<5; i++)
		{
			etaQ[i] = as_double(util::format("biopwr.plant.pl_eff_f%d", i));
			etaT[i] = as_double(util::format("biopwr.plant.temp_eff_f%d", i));
		}

		//Warnings

		////Getting weather file
		//FILE *fp = 0;
		//WFHeader info;
		//wxString file = wxString(wf);
		//int type = GetWeatherFileType(file);
		//bool ok = true;
		//if (type == WF_TM2)
		//	ok = ParseTM2Header(file, info, &fp);
		//else if (type == WF_TM3)
		//	ok = ParseTM3Header(file, info, &fp);
		//else if (type == WF_EPW)
		//	ok = ParseEPWHeader(file, info, &fp);
		////weather file error
		//if (!ok || !fp)
		//{
		//	Messages.Add(wxString::Format("could not parse weather file header, type = %d, wf=%s", type, wf));
		//	if (fp) fclose(fp);
		//	return SAMSIM_ERR;
		//}
		//declaring new emissions variables
		int coll_fuel = as_integer("biopwr.emissions.collection_fuel");
		int av_cred = as_integer("biopwr.emissions.avoided_cred");

		//declaring new internal variables
		double annual_output = 0;
		double air_moist = 0;
		double pp_water = 0;
		ssc_number_t * temp_c = allocate("monthly_temp_c", 12);
		double temp_f[12];
		double temp_k[12];
		ssc_number_t * rh = allocate("monthly_rh", 12);
		double w_per_hv[12]; /*following B&W 21-5, this is the H20 content per 10,000 btu, HHV basis*/
		double fuel_eff_loss[12]; /*efficiency loss due to liquid water in fuel and water formed from H in biomass (uses hhv)*/
		ssc_number_t *boiler_eff = allocate("monthly_boiler_eff", 12);
		ssc_number_t *moist = allocate("monthly_moist", 12);
		double moist_b[12]; /*moisture before drying*/
		double emc[12];
		double capfactor[12];
		ssc_number_t * heatrate_hhv = allocate("monthly_hhv_heatrate", 12);
		ssc_number_t * heatrate_lhv = allocate("monthly_lhv_heatrate", 12);
		ssc_number_t * bagasse_emc = allocate("monthly_bagasse_emc", 12);
		ssc_number_t * barley_emc = allocate("monthly_barley_emc", 12);
		ssc_number_t * stover_emc = allocate("monthly_stover_emc", 12);
		ssc_number_t * rice_emc = allocate("monthly_rice_emc", 12);
		ssc_number_t * wheat_emc = allocate("monthly_wheat_emc", 12);
		ssc_number_t * forest_emc = allocate("monthly_forest_emc", 12);
		ssc_number_t * mill_emc = allocate("monthly_mill_emc", 12);
		ssc_number_t * urban_emc = allocate("monthly_urban_emc", 12);
		ssc_number_t * woody_emc = allocate("monthly_woody_emc", 12);
		ssc_number_t * herb_emc = allocate("monthly_herb_emc", 12);
		std::vector<double> _twet(8760, 0.0);
		std::vector<double> _twetc(8760, 0.0);
		std::vector<double> _tdry(8760, 0.0);
		std::vector<double> _rh(8760, 0.0);
		std::vector<double> _it(8760, 0.0);
		std::vector<double> _wet_air_eff_loss(8760, 0.0);
		std::vector<double> _dry_eff_loss(8760, 0.0);
		ssc_number_t *_boiler_eff = allocate("hourly_boiler_eff", 8760);


		ssc_number_t * _etaa = allocate("monthly_pb_eta", 12);

		_twet[1] = 1000; /*this is slightly "kludgy", but will be used to check if _twet was populated by the weather files or not. This will remain until I think of something more sophisticated.*/
		for (int i = 0; i<12; i++)
		{
			temp_c[i] = 0;
			rh[i] = 0;
			moist[i] = 0;
			moist_b[i] = 0;
			emc[i] = 0;
			capfactor[i] = 0;
			heatrate_hhv[i] = 0;
			heatrate_lhv[i] = 0;
			_etaa[i] = 0;
			bagasse_emc[i] = 0;
			barley_emc[i] = 0;
			stover_emc[i] = 0;
			rice_emc[i] = 0;
			wheat_emc[i] = 0;
			forest_emc[i] = 0;
			mill_emc[i] = 0;
			urban_emc[i] = 0;
			woody_emc[i] = 0;
			herb_emc[i] = 0;
			boiler_eff[i] = 0;
		}
		//double tous[8760];
		std::vector<double> tous(8760, 0.0);
		double total_wair_eff_loss = 0;
		double total_dry_eff_loss = 0;

		weather_record wf;
		int istep = 0, nstep = (int)wFile.nrecords();
		while (wFile.read( &wf ) && istep < 8760)
		{
			// send progress update notification to any callback
			if (istep % (nstep / 20) == 0)
				update( "", 100.0f * ((float)istep) / ((float)nstep), (float)istep );

			//int year, month, day, hour;
			//double gh, dn, df, wind, tdry, twet, relhum, pres, wdir, snow, alb;
			//if (!ReadWeatherFileLine(fp, type, year, month, day, hour, gh, dn, df, wind, tdry, twet, relhum, pres, wdir, snow, alb))
			//{
			//	fclose(fp);
			//	Messages.Add("failed to read weather file data line");
			//	return SAMSIM_ERR;
			//}
			if (std::isnan(wf.rhum))
			{
				throw exec_error("biopower", "weather file does not contain relative humidity data required to calculate air moisture");
				return;
			}
			int iMonth = util::month_of((double)istep) - 1;
			temp_c[iMonth] += (ssc_number_t)(wf.tdry / (nday[iMonth] * 24.0));		/*calculating avg monthly temp & rh*/
			rh[iMonth] += (ssc_number_t)(wf.rhum / (nday[iMonth] * 24.0) / 100.0);

			_twet[istep] = wf.twet;
			_tdry[istep] = wf.tdry;
			_rh[istep] = wf.rhum / 100.0;
			pp_water = _rh[istep] * (exp(77.3450 + (0.0057 * (wf.tdry + 273.15)) - (7235 / (wf.tdry + 273.15))) / pow((wf.tdry + 273.15), 8.2)) * 0.000145037738;
			air_moist = 0.622 * pp_water / ((wf.pres * 0.0145037738) - pp_water);
			_wet_air_eff_loss[istep] = (air_feed * air_moist * 0.0045 * (flue_temp - ((wf.tdry * 9.0 / 5.0) + 32))) / 100.0;
			_dry_eff_loss[istep] = (0.0024 * air_feed * (flue_temp - ((wf.tdry * 9.0 / 5.0) + 32))) / 100.0;
			if (tou_opt == 1 && disp_count)
			{
				tous[istep] = disp[tou[istep]];
			}

			istep++;
		}

		//if _twet was not populated by weather file, _twet will be calculated
		//all commented calculations in this sections may be implemented, but right now the guess is the best estimate of twet
		if (temp_mode == 1 && _twet[1] == 1000)
		{
			_twet[1] = 0;
			/*double guess, maxit, xacc;
			maxit = 1000;
			xacc = 0.00001;*/
			for (int i = 0; i<8760; i++)
			{
				/*bool success = false;*/
				double dp_est = _tdry[i] - ((1 - _rh[i]) / 0.05);
				double guess = _tdry[i] - ((_tdry[i] - dp_est) / 3.0);
				_twetc[i] = guess;
				/*_guess[i] = guess;*/
				/*	double esata = exp(1.8096 + ((17.27*_tdry[i])/(237.3+_tdry[i])));
				double cc = (_rh[i]*esata)+(998.3*0.000799*_tdry[i]);
				for (int j=0;j<maxit;j++)
				{
				double f1 = (1.8096+((17.27*guess)/(237.3+guess)));
				double fx = exp(f1) + (998.3*0.000799*guess)-cc;
				double fdx = (exp(f1)*(17.27*237.3)/((237.3+guess)*(237.3+guess)))+(998.3*0.000799);
				double newguess = guess - (fx/fdx);*/
				/*_guess[i] = guess;
				double in_app = 0.0001;
				double es = 6.1121 * exp((18.678 - (_tdry[i] / 234.5))*(_tdry[i] / (257.15 + _tdry[i])));
				bool success = false;
				for (int iter=0;iter<maxit;iter++)
				{*/
				//Failed Method 3
				/*double ew = 6.1121 * exp((18.678 - (guess / 234.5))*(guess / (257.15 + guess)));
				double ee = ew - ((_press[i]) * (_tdry[i] - guess) * 0.00066 * (1 + (0.00115 * guess)));
				double fx = (ee/es) - _rh[i];
				double ew1 = 6.1121 * exp((18.678 - ((guess+in_app) / 234.5))*((guess+in_app) / (257.15 + (guess+in_app))));
				double ee1 = ew1 - ((_press[i]) * (_tdry[i] - (guess+in_app)) * 0.00066 * (1 + (0.00115 * (guess+in_app))));
				double fx1 = (ee1/es) - _rh[i];
				double ew2 = 6.1121 * exp((18.678 - ((guess-in_app) / 234.5))*((guess-in_app) / (257.15 + (guess-in_app))));
				double ee2 = ew2 - ((_press[i]) * (_tdry[i] - (guess-in_app)) * 0.00066 * (1 + (0.00115 * (guess-in_app))));
				double fx2 = (ee2/es) - _rh[i];
				double fprimex = (fx1-fx2)/(in_app*2);*/
				//Failed Method 1
				//double fprimex = (1.0/es) *  (((6.1121 * ((-0.00426439 * guess * guess) - (2.19309 * guess) + 4802.86) * exp((guess*(18.678-0.00426439*guess))/(guess+257.14)))/((guess+257.14)*(guess+257.14))) +  (((_press[i]) * 0.00066) * ((-0.00115 * _tdry[i]) + (0.0023 * guess) + 1)));
				//double es = 6.112 * exp((17.67 * _tdry[i])/(_tdry[i] + 243.5));
				//double ew = 6.112 * exp((17.67 * guess)/(guess + 243.5));
				//double ee = ew - ((_press[i]) * (_tdry[i] - guess) * 0.00066 * (1 + (0.00115 * guess)));
				//double fx =  ( ee / es) - _rh[i];
				//double fprimex =  ((26297.76624 * exp((17.67 * guess)/(guess + 243.5)) / ((guess + 243.5)*(guess + 243.5))) + (((_press[i]) * 0.00066) * ((-0.00115 * _tdry[i]) + (0.0023 * guess) + 1))) / es;
				//Failed Method 2
				//double eswb = exp(((16.78 * guess) - 116.9)/(guess + 237.3));
				//double aa = 0.00066 * (1.0 + (0.00115 * guess));
				//double esdb = exp(((16.78 * _tdry[i]) - 116.9)/(_tdry[i] + 237.3));
				//double ed = eswb - (aa * _pressp[i] * (_tdry[i] - guess));
				//double fx = (1.0 / _rh[i] * ed / esdb) - 1;
				//double fprimex = 1 / _rh[i] / esdb * ((eswb * 4098.79 /((guess + 237.3) * (guess + 237.3))) + ((0.00066 * _pressp[i])*(1 + (2 * 0.00115 * guess) - (0.00115 * _tdry[i]))));
				/*newguess = guess - (fx / fprimex);*/
				/*if (fabs(newguess - guess) >= xacc)
				{
				guess = newguess;
				}
				else
				{
				success = true;
				_it[i] = j;
				break;
				}
				}
				if (success == true)
				{
				_twetc[i] = guess-(1-_rh[i]);*/
				//_cdiff[i] = (_twetc[i] - _twet[i])/_twetc[i]*100.0;
				//_gdiff[i] = (_guess[i] - _twet[i])/_guess[i]*100.0;
				/*}*/
			}
		}

		//calculating dry losses in boiler efficiency
		for (int i = 0; i<12; i++)
		{
			temp_f[i] = (temp_c[i] * 9.0 / 5.0) + 32;
			temp_k[i] = temp_c[i] + 273.15;
		}

		//calculating unburned carbon efficiency losses and manufacturer's margin
		double manu_eff_loss = 0.040;
		double unburn_eff_loss = 0.0;
		if (burn_opt == 0)
		{
			unburn_eff_loss = 0.035; /*this accounts for unburned carbon in stoker boilers*/
		}
		else if (burn_opt == 1)
		{
			unburn_eff_loss = 0.0025; /*this accounts for unburned carbon in FB boilers*/
		}
		else if (burn_opt == 2)
		{
			unburn_eff_loss = 0.030; /*this accounts for unburned carbon in cyclone boilers*/
		}
		double _moist_b = 0.0;
		double _moist = 0.0;
		//calculating moisture losses in boiler efficiency
		double total_fuel_eff_loss = 0;
		for (int i = 0; i<12; i++)
		{
			if (dry_opt == 0)
			{
				for (int j = 0; j<15; j++) /*fed as received monthly moisture content*/
				{
					moist[i] += (ssc_number_t)(frac[j] * harv_moist[j][i]);
				}
			}
			else if (dry_opt == 1) /*dry to EMC on a monthly basis */
			{
				for (int j = 0; j<15; j++)
				{
					if (j == 0 && frac[j] != 0)
					{
						double const_w = -57.7 - (0.1982 * temp_k[i]) + (22.305 * sqrt(temp_k[i]));
						double const_k = -2778.14 - (2042.09 * (temp_k[i])) + (5238.88 * sqrt(temp_k[i]));
						double const_k1 = -70.42 - (13.68 * (temp_k[i])) + (180.22 * sqrt(temp_k[i]));
						double const_k2 = 194.01 + (0.62 * (temp_k[i])) + (51.48 * sqrt(temp_k[i]));
						double bag = (((1800.0 / const_w) * (((const_k * rh[i] * 100.0) / (1 - (const_k * rh[i] * 100.0))) + (((const_k1 * const_k * rh[i] * 100.0) + (2 * const_k1 * const_k2 * const_k * const_k * rh[i] * 100.0 * rh[i] * 100.0)) / (1 + (const_k1 * const_k * rh[i] * 100.0) + (const_k1 * const_k2 * const_k * const_k * rh[i] * 100.0 * rh[i] * 100.0))))) / 100.0);
						emc[i] += (frac[j] * bag);
						bagasse_emc[i] = (ssc_number_t)bag;
						moist[i] += (ssc_number_t)(frac[j] * harv_moist[j][i]);
					}
					else if (j == 1 && frac[j] != 0)
					{
						double chu_a = -475.12;
						double chu_b = -0.14843;
						double chu_c = 71.996;
						double bar = (1.0 / chu_b) * std::log(((temp_c[i] + chu_c) / chu_a) * std::log(rh[i])) / 100.0;
						emc[i] += frac[j] * bar;
						barley_emc[i] = (ssc_number_t)bar;
						moist[i] += (ssc_number_t)(frac[j] * harv_moist[j][i]);
					}
					else if (j == 2 && frac[j] != 0)
					{
						double const_a = 10.9137;
						double const_b = -0.0746;
						double const_c = 1.0 / 2.4116;
						double stov = (((const_a + (const_b * temp_c[i])) * pow((rh[i] / (1.0 - rh[i])), const_c)) / 100.0);
						emc[i] += (frac[j] * stov);
						stover_emc[i] = (ssc_number_t)stov;
						moist[i] += (ssc_number_t)(frac[j] * harv_moist[j][i]);
					}
					else if (j == 3 && frac[j] != 0)
					{
						double const_wr = 330 + (0.452 * temp_f[i]) + (0.00415 * temp_f[i] * temp_f[i]);
						double const_kr = 0.791 + (0.000463 * temp_f[i]) - (0.000000844 * temp_f[i] * temp_f[i]);
						double const_k1r = 6.34 + (0.000775 * temp_f[i]) - (0.0000935 * temp_f[i] * temp_f[i]);
						double const_k2r = 1.09 + (0.0284 * temp_f[i]) - (0.0000904 * temp_f[i] * temp_f[i]);
						double ric = ((1800 / const_wr) * (((const_kr * rh[i]) / (1 - (const_kr * rh[i]))) + (((const_k1r * const_kr * rh[i]) + (2 * const_k1r * const_k2r * const_kr * const_kr * rh[i] * rh[i])) / (1 + (const_k1r * const_kr * rh[i]) + (const_k1r * const_k2r * const_kr * const_kr * rh[i] * rh[i])))) / 100.0);
						emc[i] += (frac[j] * ric * 1.1);
						rice_emc[i] = (ssc_number_t)(ric * 1.1);
						moist[i] += (ssc_number_t)(frac[j] * harv_moist[j][i]);
					}
					else if ((j == 4 || j == 5 || j == 6 || j == 7 || j == 8 || j == 9) && (frac[j] != 0))
					{
						double const_w = 330 + (0.452 * temp_f[i]) + (0.00415 * temp_f[i] * temp_f[i]);
						double const_k = 0.791 + (0.000463 * temp_f[i]) - (0.000000844 * temp_f[i] * temp_f[i]);
						double const_k1 = 6.34 + (0.000775 * temp_f[i]) - (0.0000935 * temp_f[i] * temp_f[i]);
						double const_k2 = 1.09 + (0.0284 * temp_f[i]) - (0.0000904 * temp_f[i] * temp_f[i]);
						ssc_number_t these = (ssc_number_t)((1800 / const_w) * (((const_k * rh[i]) / (1 - (const_k * rh[i]))) + (((const_k1 * const_k * rh[i]) + (2 * const_k1 * const_k2 * const_k * const_k * rh[i] * rh[i])) / (1 + (const_k1 * const_k * rh[i]) + (const_k1 * const_k2 * const_k * const_k * rh[i] * rh[i])))) / 100.0);
						emc[i] += (frac[j] * these);
						if (j == 4) { wheat_emc[i] = these; }
						if (j == 5) { forest_emc[i] = these; }
						if (j == 6) { mill_emc[i] = these; }
						if (j == 7) { urban_emc[i] = these; }
						if (j == 8) { woody_emc[i] = these; }
						if (j == 9) { herb_emc[i] = these; }
						moist[i] += (ssc_number_t)((frac[j] * harv_moist[j][i]));
					}
					else if ((j == 10 || j == 11 || j == 12 || j == 13 || j == 14) && (frac[j] != 0))
					{
						moist[i] += (ssc_number_t)((frac[j] * harv_moist[j][i]));
					}
				}
			}
			else if (dry_opt == 2)
			{
				for (int j = 0; j<15; j++) /*dry to moisture spec*/
				{
					moist_b[i] += ((frac[j] * harv_moist[j][i]));

				}
				moist[i] = (ssc_number_t)dry_spec;
				if (moist_b[i]<moist[i])
				{
					moist[i] = (ssc_number_t)moist_b[i];
					//AddWarning("specified dried moisture content was higher than actual biomass moisture content");
				}
				_moist_b += moist_b[i] / 12.0;
				_moist = dry_spec;
			}
			if (emc[i] != 0 && emc[i]<moist[i])
			{
				moist[i] = (ssc_number_t)emc[i];
			}
			//if (moist[i] < 0.1) {moist[i] = 0.1;} /*in case 10 is unrealistically low moisture content*/
			w_per_hv[i] = (moist[i] / hhv * 10000.0) + (hydrogen / 2.0 * 8.94 / hhv * 10000.0); /*calculates amt of water per 10,000 btu/fuel (hhv basis), per B&W 21-5*/
			fuel_eff_loss[i] = w_per_hv[i] * (fw_enth_out - temp_f[i] + 32) / 100.0 / 100.0;
		}

		//calculating radiation boiler efficiency losses - double linear interpolation 
		double boiler_output = boiler_capacity * steam_enth / 1000000.0;
		double boiler_percent = boiler_output / (total_heat_in / boiler_num) * 100.0;

		int count1 = 0;
		int count2 = 0;
		for (int j = 0; j<18; j++)
		{
			if (rad_col[j] <= boiler_output)
			{
				count1++;
			}
			else
			{
				break;
			}
		}
		for (int i = 0; i<6; i++)
		{
			if (rad_row[i] >= boiler_percent)
			{
				count2++;
			}
			else
			{
				break;
			}
		}
		double result = 0;
		if (count1 != 0 && boiler_percent != 100)
		{
			double spread1 = fabs(rad_col[count1 - 1] - rad_col[count1]);
			double spread2 = fabs(rad_row[count2 - 1] - rad_row[count2]);
			double frac1 = fmod(boiler_output, spread1) / spread1;
			double frac2 = fmod(boiler_percent, spread2) / spread2;
			double val1 = rad_table[count1 - 1][count2 - 1];
			double val2 = rad_table[count1 - 1][count2];
			double val3 = rad_table[count1][count2 - 1];
			double val4 = rad_table[count1][count2];
			double endpoint1 = (frac1 * (val3 - val1)) + val1;
			double endpoint2 = (frac1 * (val4 - val2)) + val2;
			result = (frac2 * (endpoint2 - endpoint1)) + endpoint1;
		}
		else if (count1 == 0 && boiler_percent != 100)
		{
			double spread2 = fabs(rad_row[count2 - 1] - rad_row[count2]);
			double frac2 = fmod(boiler_percent, spread2) / spread2;
			double val3 = rad_table[count1][count2 - 1];
			double val4 = rad_table[count1][count2];
			result = (frac2 * (val4 - val3)) + val3;
		}
		else if (count1 != 0 && boiler_percent == 100)
		{
			double spread1 = fabs(rad_col[count1 - 1] - rad_col[count1]);
			double frac1 = fmod(boiler_output, spread1) / spread1;
			double val2 = rad_table[count1 - 1][count2 - 1];
			double val4 = rad_table[count1][count2 - 1];
			result = (frac1 * (val4 - val2)) + val4;
		}
		else if (count1 == 0 && boiler_percent == 100)
		{
			result = rad_table[count1][count2];
		}

		double rad_eff_loss = fabs(result / 100.0);

		ssc_number_t *_enet = allocate("gen", 8760);
//		ssc_number_t *_gen = allocate("gen", 8760);
		ssc_number_t *_qtpb = allocate("hourly_q_to_pb", 8760);
		std::vector<double> _tnorm(8760, 0.0);
		std::vector<double> _gross(8760, 0.0);
		ssc_number_t *_pbeta = allocate("hourly_pbeta", 8760);

		double annual_heatrate_hhv = 0;
		double annual_heatrate_lhv = 0;
		double annual_biomass = 0;
		double annual_coal = 0;
		double annual_capfactor = 0;
		double total_boiler_eff = 0;
		double total_etaa = 0;
		total_dry_eff_loss = 0;
		total_wair_eff_loss = 0;
		total_fuel_eff_loss = 0;
		total_boiler_eff = 0;
		//Power block calculations
		for (int i = 0; i<12; i++)
		{

		}
		double Qtotal = total * 2000.0 * hhv;
		double par_loss = 0; //in kWh
		double Qtoboil_tot = 0;
		double Qtopb_tot = 0;
		double press_adj = (steam_pressure - 900.0) / 14.2857143*0.1 / 100.0; /*rule of thumb that says for every 14.2847 increase in psi of steam pressure, efficiency increases 0.1%*/
		rated_eff += press_adj;


		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("biopower", "failed to setup adjustment factors: " + haf.error());


		for (int i = 0; i<8760; i++)
		{
			int iMonth = util::month_of(i) - 1;
			_boiler_eff[i] = (ssc_number_t)((1 - fuel_eff_loss[iMonth] - _dry_eff_loss[i] - _wet_air_eff_loss[i] - unburn_eff_loss - manu_eff_loss - rad_eff_loss) *100.0);

			total_dry_eff_loss += _dry_eff_loss[i] / 8760.0; //for loss diagram
			total_wair_eff_loss += _wet_air_eff_loss[i] / 8760.0; //for loss diagram
			total_fuel_eff_loss += fuel_eff_loss[iMonth] / 8760.0; //for loss diagram
			total_boiler_eff += _boiler_eff[i] / 8760.0; //for loss diagram

			boiler_eff[iMonth] += (ssc_number_t)(_boiler_eff[i] / (nday[iMonth] * 24.0));
			double Qmonth = total * nday[iMonth] / 365.0 * 2000.0 * hhv;
			double Qtoboil = Qmonth / (nday[iMonth] * 24.0);
			Qtoboil_tot += (Qtoboil * (1 / 3412.14163));
			if (tou_opt == 1)
			{
				if (ramp_rate == 0)
				{
					Qtoboil *= tous[i];
					Qtotal -= Qtoboil;
				}
				else if (ramp_rate != 0 && i > 0 && i < 8760 && tous[i] >= tous[i - 1])
				{
					tous[i] = (tous[i - 1] + ramp_rate<tous[i]) ? tous[i - 1] + ramp_rate : tous[i];
					Qtoboil *= tous[i];
					Qtotal -= Qtoboil;
				}
				else if (ramp_rate != 0 && i > 0 && i < 8760 && tous[i] <= tous[i - 1])
				{
					tous[i] = (tous[i - 1] - ramp_rate>tous[i]) ? tous[i - 1] - ramp_rate : tous[i];
					Qtoboil *= tous[i];
					Qtotal -= Qtoboil;
				}
				else
				{
					Qtoboil *= tous[i];
					Qtotal -= Qtoboil;
				}
				if (Qtotal <= 0)
				{
					throw exec_error("biopower", "the fractional operation specifications were too high! Biomass was over-utilized");
					return;
				}
			}
			else
			{
				Qtotal -= Qtoboil;
			}
			double Qtopb = (_boiler_eff[i] / 100.0) * Qtoboil * (1 / 3412.14163); /*conversion to kW*/
			Qtopb_tot += (Qtopb);
			double Qdesign = Wdesign / rated_eff;

			double Qnorm = Qtopb / Qdesign;
			double Tnorm = (temp_mode == 1) ? _twet[i] - design_temp : _tdry[i] - design_temp;

			double eta_q = etaQ[0] + etaQ[1] * Qnorm + etaQ[2] * Qnorm*Qnorm + etaQ[3] * Qnorm*Qnorm*Qnorm + etaQ[4] * Qnorm*Qnorm*Qnorm*Qnorm;
			double eta_t = etaT[0] + etaT[1] * Tnorm + etaT[2] * Tnorm*Tnorm + etaT[3] * Tnorm*Tnorm*Tnorm + etaT[4] * Tnorm*Tnorm*Tnorm*Tnorm;
			double eta_adj = eta_q * eta_t * rated_eff;

			double Wgr = Qtopb * eta_adj;

			if (Wgr < min_load*Wdesign) Wgr = 0;
			if (Wgr > max_over*Wdesign) Wgr = Wdesign;

			double Wnet = Wgr - (par*Wgr);
			par_loss += (Wgr - Wnet);
			double heatrat_hhv = Qtoboil / Wnet / 1000.0;
			double heatrat_lhv = (Qtoboil / hhv*lhv) / Wnet / 1000.0;
			double capfact = Wnet / Wdesign;

			_gross[i] = Wgr;
			_pbeta[i] = (ssc_number_t)(eta_adj*100.0);
			_etaa[iMonth] += (ssc_number_t)(eta_adj*100.0 / (nday[iMonth] * 24.0));
			total_etaa += eta_adj / 8760.0; //for loss diagram
			_qtpb[i] = (ssc_number_t)Qtopb;
			_enet[i] = (ssc_number_t)(Wnet*haf(i));
//			_gen[i] = _enet[i];
			_tnorm[i] = Tnorm;
			capfactor[iMonth] += capfact / (nday[iMonth] * 24.0);
			heatrate_hhv[iMonth] += (ssc_number_t)(heatrat_hhv / (nday[iMonth] * 24.0));
			heatrate_lhv[iMonth] += (ssc_number_t)(heatrat_lhv / (nday[iMonth] * 24.0));

			annual_output += Wnet;
			annual_heatrate_hhv += heatrat_hhv / 8760.0;
			annual_heatrate_lhv += heatrat_lhv / 8760.0;
			annual_capfactor += capfact * 100.0 / 8760.0;
		}
		double leftover = 0;
		if (Qtotal != 0)
		{
			leftover = Qtotal / (total * 2000.0 * hhv);
		}
		annual_biomass = total_biomass * (1 - leftover);
		annual_coal = total_coal * (1 - leftover);
		double total_feedstock = annual_biomass + annual_coal;


		double thermeff_hhv = 3.41213163 / annual_heatrate_hhv * 100.0;
		double thermeff_lhv = 3.41213163 / annual_heatrate_lhv * 100.0;

		//populating array for the ash content
		double _ash[19];
		_ash[0] = 0.038; /*bagasse*/
		_ash[1] = 0.049; /*barley*/
		_ash[2] = 0.051; /*stover*/
		_ash[3] = 0.134; /*rice*/
		_ash[4] = 0.050; /*wheat*/
		_ash[5] = 0.0087; /*forest*/
		_ash[6] = 0.0148; /*mill*/
		_ash[7] = 0.0125; /*urban*/
		_ash[8] = 0.0125; /*woody*/
		_ash[9] = 0.038; /*herby*/
		_ash[10] = 0.05; /*feedstock1, estimate*/
		_ash[11] = 0.05; /*feedstock2, estimate*/
		_ash[12] = 0.062; /*bituminous*/
		_ash[13] = 0.091; /*sub-bituminous*/
		_ash[14] = 0.177; /*lignite*/
		double ash = 0;
		for (int i = 0; i<15; i++)
		{
			ash += frac[i] * _ash[i];
		}
		double tpy_ash = total_feedstock * ash;
//		double tpy_nitrogen = (((frac[5] + frac[6] + frac[7] + frac[8] + frac[9])*2.6290) + ((frac[0] + frac[1] + frac[2] + frac[3] + frac[4])*2.501) + (((frac[10] + frac[11])*4.7587) + (frac[12] * 3.8076))) / 2000.0 * annual_output / 1000.0; /*from eGrid*/
//		double tpy_sulfur = (((frac[5] + frac[6] + frac[7] + frac[8] + frac[9])*0.1) + ((frac[0] + frac[1] + frac[2] + frac[3] + frac[4])*0.3) + (((frac[10] + frac[11])*5.8540) + (frac[12] * 10.3309))) / 2000.0 * annual_output / 1000.0; /*from eGrid*/
//		double tpy_gwp = 0 + (total_coal*((frac[10] * 0.767*(1 - 0.062)*3.7) + (frac[11] * 0.608*(1 - 0.091)*3.7) + (frac[12] * 0.498*(1 - 0.177)*3.7))); /*accounts for carbon released from any coal feedstocks*/

		//Emissions calculations - block 1 (Biomass growth/avoided residue and harvest/collection), for details see Excel file SAM Equations_12_19_11
		double npci = 1628.491234; /*g CO2/lb nitrogen used*/
		double ppci = 488.0545632; /*g CO2/lb phosphorus used*/
		double kpci = 314.800651; /*g CO2/lb potassium used*/
		double lpci = 288.0804725; /*g CO2/lb lime used*/
		double cfci = (coll_fuel == 1) ? 0.025216 : 0.094744; /*diesel fuel carbon intensity (collection) units of g CO2/Btu fuel*/
		double avem = (av_cred == 0) ? 0.0 : 1.0;
		double bgarhc[10][11]; /*columns here match the units in the excel file*/
		double n_app[10] = { 0.0, 13.6, 18.0, 18.0, 13.6, 0.0, 0.0, 0.0, 11.57, 21.91 };
		double p_app[10] = { 0.0, 1.38, 1.98, 1.98, 1.38, 0.0, 0.0, 0.0, 0.69, 2.11 };
		double k_app[10] = { 0.0, 25.2, 30.0, 30.0, 25.2, 0.0, 0.0, 0.0, 3.44, 25.80 };
		double l_app[10] = { 0.0, 0.0, 0.0, 0.0, 0.0, 74.08, 0.0, 0.0, 18.58, 31.72 };
		double coll_yield[10] = { 0.0, 68466.9 / (865.41 / 2000.0), 68466.9 / (2935.3 / 2000.0), 68466.9 / (2935.3 / 2000.0), 68466.9 / (865.41 / 2000.0), 99727.7 / (15167.0 / 2000.0), 0.0, 0.0, 805492.8 / (12490.5 / 2000.0), 690422.4 / (10547.7 / 2000.0) };
		for (int i = 0; i<10; i++)
		{
			bgarhc[i][0] = dry[i] / 0.95 * n_app[i];
			bgarhc[i][1] = dry[i] / 0.95 * p_app[i];
			bgarhc[i][2] = dry[i] / 0.95 * k_app[i];
			bgarhc[i][3] = dry[i] / 0.95 * l_app[i];
			bgarhc[i][4] = dry[i] / 0.95 * coll_yield[i];
			bgarhc[i][5] = bgarhc[i][0] * npci;
			bgarhc[i][6] = bgarhc[i][1] * ppci;
			bgarhc[i][7] = bgarhc[i][2] * kpci;
			bgarhc[i][8] = bgarhc[i][3] * lpci;
			bgarhc[i][9] = bgarhc[i][4] * cfci;
			if (i == 6 || i == 7)
			{
				bgarhc[i][9] = 6.58*2000.0;
				bgarhc[i][10] = -(((9.6343873 / 100.0)*(waste_c[i - 6])*((16.0 / 12.0*25.0) - (44.0 / 12.0)))*1000.0*0.45359) * 2000.0 * wet[i] * avem;
			}
		}
		double final[12] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }; /*as seen in the spreadsheet*/
		for (int j = 0; j<10; j++)
		{
			for (int i = 5; i<10; i++)
			{
				final[0] += bgarhc[j][i] / annual_output;
			}
		}

		//avoided emissions
		final[10] = (bgarhc[6][10] + bgarhc[7][10]) / annual_output;
		final[9] = (bgarhc[6][9] + bgarhc[7][9]) / annual_output;
		double swit = 0;
		double swit2 = 0;
		double swit3 = 0;
		for (int j = 0; j<4; j++)
		{
			if (j == 0){ swit = 839.15; swit2 = 0.446; swit3 = 19644.34; }
			else if (j == 1){ swit = 2197.47; swit2 = 0.531; swit3 = 3653.64; }
			else if (j == 2){ swit = 1813.564; swit2 = 0.758; swit3 = 1900.96; }
			else if (j == 3){ swit = 1411.813; swit2 = 0.819; swit3 = 2023.57; }
			for (int i = 0; i<10; i++)
			{
				final[1] += bgarhc[i][j] * swit / annual_output;
				final[4] += bgarhc[i][j] * swit2 / annual_output;
				final[5] += bgarhc[i][j] * swit3 / annual_output;
			}
		}
		int hold = coll_fuel + 2;
		for (int j = 0; j<10; j++)
		{
			final[hold] += bgarhc[j][4] / annual_output;
			final[6] += bgarhc[j][0] / annual_output;
			final[7] += bgarhc[j][1] / annual_output;
			final[8] += bgarhc[j][2] / annual_output;
			final[11] += bgarhc[j][8] / annual_output;
		}
		//Emissions calculations block 2: Transportation
		double dcfci = (transport_fuel == 1) ? 0.02522*947.8171203*(34.0 / 0.264172052) : 0.09474*947.8171203*(34.0 / 0.264172052); /*diesel vehicle fuel carbon intensity gCO2/gal gas eq*/
		double trans_diesel = 0; /*this represents either diesel or biodiesel, whichever was selected*/
		//truck leg 1
		if (transport_legs == 0) { transport_predist = 0; }
		double leg1 = wet_total / 25.0 * transport_predist * 0.8 / 4.75 * dcfci / annual_output;
		trans_diesel += leg1 / dcfci * annual_output;
		//truck leg 2
		double leg2 = wet_total / 25.0 * (transport_longmiles - transport_predist) * 0.8 / 4.75 * dcfci / annual_output;
		trans_diesel += leg2 / dcfci * annual_output;
		//rail leg
		double leg3 = (transport_longopt == 0) ? wet_total * (collection_radius - transport_longmiles) * 0.8 / 330.0 * dcfci / annual_output : 0.0;
		trans_diesel += leg3 / dcfci * annual_output;
		//barge leg
		double leg4 = (transport_longopt == 1) ? wet_total * (collection_radius - transport_longmiles) * 0.8 / 550.0 * 12686.73697 / annual_output : 0.0;
		double trans_bunker = leg4 / 12686.73697 * annual_output;

		double testing_leg = leg1 + leg2 + leg3 + leg4;
		double testing_dies = trans_diesel / 0.264172052 * 34.0 * 947.8171203 / annual_output;
		double testing_bunker = trans_bunker / 0.264172052 * 34.0 * 947.8171203 / annual_output;

		//Emissions calculations block 3: Preprocessing
		double preprocessing_diesel = pre_chipopt * (0.02387 + 0.01982) * dry_total * 1000000.0 / annual_output;
		double preprocessing_kwh = ((pre_grindopt * (dry_total * 0.2137 * 1000000.0 / 3412.14)) + (pre_pelletopt * (dry_total * 0.2835 * 1000000.0 / 3412.14))); /*kWh / year*/
		double preprocessing_ems = (preprocessing_kwh * grid_intensity / annual_output) + (preprocessing_diesel * 0.09474); /*g CO2/kWh*/
//		double preprocessing_ele = preprocessing_kwh / annual_output; /*kWh used/kWh out*/

		//Emissions calculations block 4: Drying and storage
		double drying_kwh = 0; /*kwh used/kwh produced*/
		double drying_ems = 0; /*g CO2/kwh */
		double drying_natgas = 0; /*Btu/kWh*/
		if (dry_opt == 0){ drying_kwh = 0.299 * dry_total / annual_output; drying_ems = drying_kwh * grid_intensity; }
		if (dry_opt == 1 || dry_opt == 2){ drying_kwh = 0.97 * dry_total / annual_output; drying_ems = drying_kwh * grid_intensity; }
		if (dry_opt == 2){
			drying_ems += (dry_total * (_moist_b - _moist) * 2000.0 * 1931.0 * 0.065 / annual_output);
			drying_natgas += dry_total * (_moist_b - _moist) * 2000.0 * 1931.0 / annual_output;
		}

		//Emissions calculations block 5: combustion
		double comb_ems = frac_c * total * (44.0 / 12.0) * 907184.74 / annual_output; /*g CO2/kwh */
//		double comb_ash = ash * total; /*tons/year*/
		double comb_ems_neg = -(biomass_frac_c * dry_total * (44.0 / 12.0) * 907184.74 / annual_output); /*negative g CO2/kwh */

		double final_emissions[7];
		final_emissions[0] = final[0];
		final_emissions[1] = testing_leg;
		final_emissions[2] = preprocessing_ems;
		final_emissions[3] = drying_ems;
		final_emissions[4] = comb_ems;
		final_emissions[5] = comb_ems_neg;
		final_emissions[6] = final[10];
		for (int i = 0; i<6; i++) { final_emissions[6] += final_emissions[i]; }
		double ems_per_lb = final_emissions[6] * annual_output / dry_total / 2000.0;

		//Diesel consumption
		double diesel_use = 0;
		double biodiesel_use = 0;
		if (transport_fuel == 0)
		{
			diesel_use = testing_dies + final[2] + preprocessing_diesel;
		}
		if (transport_fuel == 1)
		{
			biodiesel_use = testing_dies + final[3];
		}


		//Natural gas consumption
		double naturalgas = final[5] + drying_natgas;

		//water usage (as a metric)
		// gal / hr = 300 gal/1000 kWh * kWh / yr * 1 MW / 1000 kW * 1 yr / 8760 hr  
		double water = (300.0*annual_output / 1000.0 / 8760.0); /*gallons/hr*/
		// assuming that water in gal and 1 gal of water = 0.0037854118 m3 www.convertunits.com/from/US+gallon/to/meters%5E3
		// and 8760 hr per year gives and annual water usage of 
		water *= 0.0037854118 * 8760.0; // for output in m3/yr

		//fclose(fp);

		double fuel_usage = total * 2000 * hhv / 3412.14163;

		// create outputs ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

		//convert internal variables into external variables

		// Single value outputs
		assign("system.annual.boiler_loss_fuel_kwh", var_data((ssc_number_t)(total_fuel_eff_loss*fuel_usage)));

		//new variables for loss diagram
		assign("system.annual.boiler_loss_unburn_kwh", var_data((ssc_number_t)(unburn_eff_loss*fuel_usage)));
		assign("system.annual.boiler_loss_manu_kwh", var_data((ssc_number_t)(manu_eff_loss*fuel_usage)));
		assign("system.annual.boiler_loss_rad_kwh", var_data((ssc_number_t)(rad_eff_loss*fuel_usage)));
		assign("system.annual.boiler_loss_dry_kwh", var_data((ssc_number_t)(total_dry_eff_loss*fuel_usage)));
		assign("system.annual.boiler_loss_wet_kwh", var_data((ssc_number_t)(total_wair_eff_loss*fuel_usage)));


		double boiler_output_total = (1.0 - (total_fuel_eff_loss + unburn_eff_loss + manu_eff_loss + rad_eff_loss + total_dry_eff_loss + total_wair_eff_loss))* fuel_usage;

		assign("system.annual.boiler_loss_total_kwh", var_data((ssc_number_t)((1.0 - rated_eff)*boiler_output_total)));
		assign("system.annual.pb_eta_kwh", var_data((ssc_number_t)((rated_eff - total_etaa)*boiler_output_total)));

		double turbine_output = boiler_output_total * (1.0 - ((1.0 - rated_eff) + (rated_eff - total_etaa)));

		assign("system.annual.par_loss_kwh", var_data((ssc_number_t)(par*turbine_output)));

		assign("system.annual.boiler_output", var_data((ssc_number_t)(boiler_output_total)));
		assign("system.annual.turbine_output", var_data((ssc_number_t)(turbine_output)));

		assign("system.annual.boiler_loss_fuel", var_data((ssc_number_t)total_fuel_eff_loss));
		assign("system.annual.boiler_loss_unburn", var_data((ssc_number_t)unburn_eff_loss));
		assign("system.annual.boiler_loss_manu", var_data((ssc_number_t)manu_eff_loss));
		assign("system.annual.boiler_loss_rad", var_data((ssc_number_t)rad_eff_loss));
		assign("system.annual.boiler_loss_dry", var_data((ssc_number_t)total_dry_eff_loss));
		assign("system.annual.boiler_loss_wet", var_data((ssc_number_t)total_wair_eff_loss));
		assign("system.annual.boiler_loss_total", var_data((ssc_number_t)total_boiler_eff));
		assign("system.annual.pb_eta", var_data((ssc_number_t)total_etaa));
		assign("system.annual.par_loss", var_data((ssc_number_t)par));
		assign("system.annual.qtoboil_tot", var_data((ssc_number_t)Qtoboil_tot));
		assign("system.annual.qtopb_tot", var_data((ssc_number_t)Qtopb_tot));

		//hourly
		// release
		//samsim_set_da((long)this, "system.hourly.q_to_pb", _qtpb.data(), 8760);
		//samsim_set_da((long)this, "system.hourly.e_net", _enet.data(), 8760);
		//samsim_set_da((long)this, "system.hourly.boiler_eff", _boiler_eff.data(), 8760);
		//samsim_set_da((long)this, "system.hourly.pbeta", _pbeta.data(), 8760);

		//monthly
		accumulate_monthly("gen", "monthly_energy");
		accumulate_monthly("hourly_q_to_pb", "monthly_q_to_pb");
		//samsim_set_da((long)this, "system.monthly.pb_eta", _etaa, 12);
		//samsim_set_da((long)this, "system.monthly.boiler_eff", boiler_eff, 12);
		//samsim_set_da((long)this, "system.monthly.moist", moist, 12);
		//samsim_set_da((long)this, "system.monthly.hhv_heatrate", heatrate_hhv, 12);
		//samsim_set_da((long)this, "system.monthly.lhv_heatrate", heatrate_lhv, 12);
		//samsim_set_da((long)this, "system.monthly.bagasse_emc", bagasse_emc, 12);
		//samsim_set_da((long)this, "system.monthly.barley_emc", barley_emc, 12);
		//samsim_set_da((long)this, "system.monthly.stover_emc", stover_emc, 12);
		//samsim_set_da((long)this, "system.monthly.rice_emc", rice_emc, 12);
		//samsim_set_da((long)this, "system.monthly.wheat_emc", wheat_emc, 12);
		//samsim_set_da((long)this, "system.monthly.forest_emc", forest_emc, 12);
		//samsim_set_da((long)this, "system.monthly.mill_emc", mill_emc, 12);
		//samsim_set_da((long)this, "system.monthly.urban_emc", urban_emc, 12);
		//samsim_set_da((long)this, "system.monthly.woody_emc", woody_emc, 12);
		//samsim_set_da((long)this, "system.monthly.herb_emc", herb_emc, 12);
		//samsim_set_da((long)this, "system.monthly.rh", rh, 12);
		//samsim_set_da((long)this, "system.monthly.temp_c", temp_c, 12);


		accumulate_annual("gen", "annual_energy");


		//annual
		assign("system.annual.ash", var_data((ssc_number_t)tpy_ash));
		assign("system.annual.e_net", var_data((ssc_number_t)annual_output));
		assign("system.annual.biomass", var_data((ssc_number_t)annual_biomass));
		assign("annual_fuel_usage", var_data((ssc_number_t)fuel_usage)); // output in kWh
		assign("annual_watter_usage", var_data((ssc_number_t)water));
		assign("system.annual.coal", var_data((ssc_number_t)annual_coal));

		//emissions
		assign("system.emissions.growth", var_data((ssc_number_t)final_emissions[0]));
		assign("system.emissions.avoided", var_data((ssc_number_t)final[10]));
		assign("system.emissions.transport", var_data((ssc_number_t)final_emissions[1]));
		assign("system.emissions.preprocessing", var_data((ssc_number_t)final_emissions[2]));
		assign("system.emissions.drying", var_data((ssc_number_t)final_emissions[3]));
		assign("system.emissions.combustion", var_data((ssc_number_t)final_emissions[4]));
		assign("system.emissions.uptake", var_data((ssc_number_t)final_emissions[5]));
		assign("system.emissions.total_sum", var_data((ssc_number_t)final_emissions[6]));
		assign("system.emissions.diesel", var_data((ssc_number_t)diesel_use));
		assign("system.emissions.biodiesel", var_data((ssc_number_t)biodiesel_use));
		assign("system.emissions.bunker", var_data((ssc_number_t)testing_bunker));
		assign("system.emissions.oil", var_data((ssc_number_t)final[1]));
		assign("system.emissions.naturalgas", var_data((ssc_number_t)naturalgas));
		assign("system.emissions.nitrogen", var_data((ssc_number_t)final[6]));
		assign("system.emissions.potassium", var_data((ssc_number_t)final[8]));
		assign("system.emissions.phosphorus", var_data((ssc_number_t)final[7]));
		assign("system.emissions.lime", var_data((ssc_number_t)final[11]));
		assign("system.emissions.ems_per_lb", var_data((ssc_number_t)ems_per_lb));

		//system use
		assign("system.capfactor", var_data((ssc_number_t)annual_capfactor));
		assign("system.use_lifetime_output", var_data((ssc_number_t)0));
		assign("system.use_recapitalization", var_data((ssc_number_t)0));
		assign("system.hhv_heatrate", var_data((ssc_number_t)annual_heatrate_hhv));
		assign("system.total_moisture", var_data((ssc_number_t)as_double("biopwr.feedstock.total_moisture")));

		/*
		double opt_fuel_1_used = as_double("biopwr.feedstockcost.biomass_fuel_used");
		assign("om_opt_fuel_1_usage", var_data((ssc_number_t)opt_fuel_1_used));
		double opt_fuel_1_cost = as_double("biopwr.feedstockcost.biomass_fuel_cost");
		ssc_number_t * da1 = allocate("om_opt_fuel_1_cost", 1);
		da1[0] = opt_fuel_1_cost;
		double opt_fuel_1_cost_esc = as_double("biopwr.feedstockcost.biomass_fuel_cost_esc") / 100.0;
		assign("om_opt_fuel_1_cost_escal", var_data((ssc_number_t)opt_fuel_1_cost_esc));//
		double opt_fuel_2_used = as_double("biopwr.feedstockcost.coal_fuel_used");
		assign("om_opt_fuel_2_usage", var_data((ssc_number_t)opt_fuel_2_used));
		double opt_fuel_2_cost = as_double("biopwr.feedstockcost.coal_fuel_cost");
		ssc_number_t * da2 = allocate("om_opt_fuel_2_cost", 1);
		da2[0] = opt_fuel_2_cost;
		double opt_fuel_2_cost_esc = as_double("biopwr.feedstockcost.coal_fuel_cost_esc") / 100.0;
		assign("om_opt_fuel_2_cost_escal", var_data((ssc_number_t)opt_fuel_2_cost_esc));//
		*/

		// modify heat rate to use here (MMBTU/MWhe)
		assign("system_heat_rate", var_data((ssc_number_t)3.4123)); // thermal to electric conversion
		assign("system.lhv_heatrate", var_data((ssc_number_t)annual_heatrate_lhv));
		assign("system.hhv_thermeff", var_data((ssc_number_t)thermeff_hhv));
		assign("system.lhv_thermeff", var_data((ssc_number_t)thermeff_lhv));

		ssc_number_t * da = allocate("om_fuel_cost", 1);
		da[0] = 0.0;

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
			annual_energy += _enet[i];
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));


	} // exec
};

DEFINE_MODULE_ENTRY( biomass, "Utility scale wind farm model (adapted from TRNSYS code by P.Quinlan and openWind software by AWS Truepower)", 2 );

