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
#include "common.h"

enum MHK_DEVICE_TYPES { GENERIC, RM3, RM5, RM6, RM1 };
enum MHK_TECHNOLOGY_TYPE { WAVE, TIDAL };



static var_info _cm_vtab_mhk_costs[] = {
	/*   VARTYPE			DATATYPE			NAME									   LABEL                                                   UNITS			META            GROUP              REQUIRED_IF            CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,			SSC_NUMBER,			"device_rated_power",						"Rated capacity of device",								"kW",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"system_capacity",							"System Nameplate Capacity",							"kW",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"devices_per_row",							"Number of wave devices per row in array",				"",				"",								"MHKCosts",         "*",                    "INTEGER",			    	"" },
//	{ SSC_INPUT,			SSC_NUMBER,			"device_type",								"Device Type",											"0/1/2/3/4",		"0=Generic,1=RM3,2=RM5,3=RM6,4=RM1",	"MHKCosts",			"?=0",					"MIN=0,MAX=4",				"" },
	{ SSC_INPUT,			SSC_NUMBER,			"marine_energy_tech",						"Marine energy technology",								"0/1",			"0=Wave,1=Tidal",				"MHKCosts",			"*",					"MIN=0,MAX=1",				"" },
	{ SSC_INPUT,			SSC_NUMBER,			"library_or_input_wec",						"Wave library or user input",								"",			"0=Library,1=User",				"MHKCosts",			"marine_energy_tech=0",					"",				"" },
	{ SSC_INPUT,			SSC_STRING,			"lib_wave_device",							"Wave library name",								"",			"",				"MHKCosts",			"marine_energy_tech=0",					"",				"" },

	{ SSC_INPUT,			SSC_NUMBER,			"inter_array_cable_length",					"Inter-array cable length",								"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"riser_cable_length",						"Riser cable length",									"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"export_cable_length",						"Export cable length",									"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },

	// User input for CapEx dependent costs
		{ SSC_INPUT,			SSC_NUMBER,			"structural_assembly_cost_method",								"Structural assembly cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"structural_assembly_cost_input",								"Structural assembly cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		//{ SSC_INPUT,			SSC_NUMBER,			"structural_assembly_cost_total",								"Structural assembly itemized cost total",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"power_takeoff_system_cost_method",								"Power take-off system cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"power_takeoff_system_cost_input",								"Power take-off system cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		//{ SSC_INPUT,			SSC_NUMBER,			"power_takeoff_system_cost_total",								"Power take-off system cost itemized cost total",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"mooring_found_substruc_cost_method",								"Mooring, foundation, and substructure cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"mooring_found_substruc_cost_input",								"Mooring, foundation, and substructure cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		//{ SSC_INPUT,			SSC_NUMBER,			"mooring_found_substruc_cost_total",								"Mooring, foundation, and substructure itemized cost total",											"$",		"",	"MHKCosts",			"*",					"",				"" },

// User input BOS values
		{ SSC_INPUT,			SSC_NUMBER,			"development_cost_method",								"Development cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Enter in itemized costs",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"development_cost_input",								"Development cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"eng_and_mgmt_cost_method",								"Engineering and management cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Enter in itemized costs",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"eng_and_mgmt_cost_input",								"Engineering and management cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"assembly_and_install_cost_method",								"Assembly and installation cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"assembly_and_install_cost_input",								"Assembly and installation cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"other_infrastructure_cost_method",								"Other infrastructure cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"other_infrastructure_cost_input",								"Other infrastructure cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },

		{ SSC_INPUT,			SSC_NUMBER,			"array_cable_system_cost_method",								"Array cable system cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"array_cable_system_cost_input",								"Array cable system cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"export_cable_system_cost_method",								"Export cable system cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"export_cable_system_cost_input",								"Export cable system cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"onshore_substation_cost_method",								"Onshore substation cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"onshore_substation_cost_input",								"Onshore substation cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"offshore_substation_cost_method",								"Offshore substation cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"offshore_substation_cost_input",								"Offshore substation cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"other_elec_infra_cost_method",								"Other electrical infrastructure cost method",											"0/1/2",		"0=Enter in $/kW,1=Enter in $,2=Use modeled value",	"MHKCosts",			"*",					"MIN=0,MAX=3",				"" },
		{ SSC_INPUT,			SSC_NUMBER,			"other_elec_infra_cost_input",								"Other electrical infrastructure cost",											"$",		"",	"MHKCosts",			"*",					"",				"" },

	//CapEx costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"structural_assembly_cost_modeled",			"Modeled structural assembly cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"power_takeoff_system_cost_modeled",		"Modeled power take-off cost",							"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"mooring_found_substruc_cost_modeled",		"Modeled mooring, foundation, and substructure cost",	"$",			"",								"MHKCosts",			"",						"",							"" },
	
	//Balance of system costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"development_cost_modeled",					"Modeled development cost",								"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"eng_and_mgmt_cost_modeled",				"Modeled engineering and management cost",				"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"plant_commissioning_cost_modeled",			"Modeled plant commissioning cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"site_access_port_staging_cost_modeled",	"Modeled site access, port, and staging cost",			"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"assembly_and_install_cost_modeled",		"Modeled assembly and installation cost",				"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"other_infrastructure_cost_modeled",		"Modeled other infrastructure cost",					"$",			"",								"MHKCosts",			"",						"",							"" },
	
	//Electrical infrastructure costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"array_cable_system_cost_modeled",			"Modeled array cable system cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"export_cable_system_cost_modeled",			"Modeled export cable system cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"onshore_substation_cost_modeled",			"Modeled onshore substation cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"offshore_substation_cost_modeled",			"Modeled offshore substation cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"other_elec_infra_cost_modeled",			"Modeled other electrical infrastructure cost",			"$",			"",								"MHKCosts",			"",						"",							"" },

	//Financial costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"project_contingency",						"Modeled project contingency cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"insurance_during_construction",			"Modeled cost of insurance during construction",		"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"reserve_accounts",							"Modeled reserve account costs",						"$",			"",								"MHKCosts",			"",						"",							"" },

	//O and M costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"operations_cost",							"Operations cost",										"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"maintenance_cost",							"Maintenance cost",										"$",			"",								"MHKCosts",			"",						"",							"" },
	

	var_info_invalid };



class cm_mhk_costs : public compute_module
{
public:
	cm_mhk_costs()
	{
		add_var_info(_cm_vtab_mhk_costs);
	}

	void exec()
	{
		//get inputs to compute module
		double device_rating = as_double("device_rated_power"); // kW
		double system_capacity_kW = as_double("system_capacity"); // kW
		double system_capacity_MW = system_capacity_kW / 1000.0; // MW
//		int device_type = as_integer("device_type");
		int technology = as_integer("marine_energy_tech");
		int devices_per_row = as_integer("devices_per_row");
		double interarray_length = as_double("inter_array_cable_length");
		double riser_length = as_double("riser_cable_length");
		double export_length = as_double("export_cable_length");


		int device_type = 4;
		if (technology == WAVE)
		{
			if (as_integer("library_or_input_wec") == 1)
				device_type = 0;
			else
			{
				std::string wave_device = as_string("lib_wave_device");
				if (wave_device == "RM3")
					device_type = 1;
				else if (wave_device == "RM5")
					device_type = 2;
				else if (wave_device == "RM6")
					device_type = 3;
				else
					device_type = 0;
			}
		}

		//define intermediate variables to store calculated outputs
		double structural_assembly, power_takeoff, mooring_found_substruc;
		double development, eng_and_mgmt, plant_commissioning, site_access_port_staging, assembly_and_install, other_infrastructure;
		double array_cable_system, export_cable_system, onshore_substation, offshore_substation, other_elec_infra;
		double project_contingency, insurance_during_construction, reserve_accounts;
		double operations_cost, maintenance_cost;
		
		//Most CapEx costs depend on technology
		if (technology == TIDAL)
		{ // device = RM1
			structural_assembly = 284245.0 * system_capacity_MW + 785137.0;
			power_takeoff = 1527017.0 * system_capacity_MW +  505548.0;
			mooring_found_substruc = 437091.0 * system_capacity_MW + 433518.0;
			//BOS costs SAM Cost Model v8.xlsx
			development = 3197591.76 * pow(system_capacity_MW, 0.49);
			eng_and_mgmt = 850744.0 * pow(system_capacity_MW, 0.565);
		}
		else // wave
		{
			if (device_type == RM3)
			{
				structural_assembly = 6854912.0 * system_capacity_MW + 2629191.0;
				power_takeoff = 2081129.0 * pow(system_capacity_MW, 0.91);
				mooring_found_substruc = 1836365.0 * system_capacity_MW + 29672.0;
				//BOS costs SAM Cost Model v8.xlsx
				development = 3197591.76 * pow(system_capacity_MW, 0.49);
				eng_and_mgmt = 850744.0 * pow(system_capacity_MW, 0.5649);
			}

			else if (device_type == RM5)
			{
				structural_assembly = 6848402.0 * system_capacity_MW + 3315338.0;
				power_takeoff = 1600927.0 * pow(system_capacity_MW, 0.91);
				mooring_found_substruc = 2158462.0 * system_capacity_MW + 1048932.0;
				//BOS costs SAM Cost Model v8.xlsx
				development = 3197591.76 * pow(system_capacity_MW, 0.49);
				eng_and_mgmt = 850744.0 * pow(system_capacity_MW, 0.5649);
			}

			else if (device_type == RM6)
			{
				structural_assembly = 13320092.0 * system_capacity_MW + 6681164.0;
				power_takeoff = 3796551.0 * pow(system_capacity_MW, 0.78);
				mooring_found_substruc = 2030816.0 * system_capacity_MW + 478400.0;
				//BOS costs SAM Cost Model v8.xlsx
				development = 3197591.76 * pow(system_capacity_MW, 0.49);
				eng_and_mgmt = 850744.0 * pow(system_capacity_MW, 0.565);
			}

			else //generic model applies to everything else
			{
				structural_assembly = 6854912.0 * system_capacity_MW + 2629191.0;
				power_takeoff = 1179579.0 * system_capacity_MW + 2495107.0;
				mooring_found_substruc = 1178598.0 * system_capacity_MW + 1602348.0;
				//BOS costs SAM Cost Model v8.xlsx
				development = 3197591.0 * pow(system_capacity_MW, 0.49);
				eng_and_mgmt = 850744.0 * pow(system_capacity_MW, 0.565);
			}
		}

		// REmaining BOS costs that are not CapEx dependent and not technology dependent
		assembly_and_install = 2805302.0 * pow(system_capacity_MW, 0.66);
		other_infrastructure = 0;

		//electrical infrastructure costs
		array_cable_system = (4.40 * (device_rating * devices_per_row / 1000.0) + 162.81) * interarray_length 
			+ (4.40 * (device_rating / 1000.0) + 162.81) * riser_length;
		export_cable_system = (4.40 * system_capacity_MW + 162.81) * export_length;
		onshore_substation = 75000.0 * system_capacity_MW;
		offshore_substation = 100000.0 * system_capacity_MW;
		other_elec_infra = 47966.16 * system_capacity_MW + 665841.0;

		// operations cost
		operations_cost = 31250.0 * system_capacity_MW + 879282.0;

		// maintenance cost
		maintenance_cost = 116803.0 * system_capacity_MW + 317719.0;

		//at this point, we need to assign the "independent" modeled outputs- 
		//i.e., we want the modeled value to be reported prior to overwriting with a user input
		//for all variables that are NOT dependent on CapEx
		assign("structural_assembly_cost_modeled", var_data(static_cast<ssc_number_t>(structural_assembly)));
		assign("power_takeoff_system_cost_modeled", var_data(static_cast<ssc_number_t>(power_takeoff)));
		assign("mooring_found_substruc_cost_modeled", var_data(static_cast<ssc_number_t>(mooring_found_substruc)));
		assign("development_cost_modeled", var_data(static_cast<ssc_number_t>(development)));
		assign("eng_and_mgmt_cost_modeled", var_data(static_cast<ssc_number_t>(eng_and_mgmt)));
		assign("assembly_and_install_cost_modeled", var_data(static_cast<ssc_number_t>(assembly_and_install)));
		assign("other_infrastructure_cost_modeled", var_data(static_cast<ssc_number_t>(other_infrastructure)));
		assign("array_cable_system_cost_modeled", var_data(static_cast<ssc_number_t>(array_cable_system)));
		assign("export_cable_system_cost_modeled", var_data(static_cast<ssc_number_t>(export_cable_system)));
		assign("onshore_substation_cost_modeled", var_data(static_cast<ssc_number_t>(onshore_substation)));
		assign("offshore_substation_cost_modeled", var_data(static_cast<ssc_number_t>(offshore_substation)));
		assign("other_elec_infra_cost_modeled", var_data(static_cast<ssc_number_t>(other_elec_infra)));
		assign("operations_cost", var_data(static_cast<ssc_number_t>(operations_cost)));
		assign("maintenance_cost", var_data(static_cast<ssc_number_t>(maintenance_cost)));

		// there are five cost values that are a percentage of total CapEx
		// we want those modeled values to reflect user-input values that are overwriting the modeled values
		// therefore, here, we replace modeled values calculated above with those input by the user, if that's selected in the UI
		int structural_assembly_cost_method = as_integer("structural_assembly_cost_method");
		int power_takeoff_system_cost_method = as_integer("power_takeoff_system_cost_method");
		int mooring_found_substruc_cost_method = as_integer("mooring_found_substruc_cost_method");

		int development_cost_method = as_integer("development_cost_method");
		int eng_and_mgmt_cost_method = as_integer("eng_and_mgmt_cost_method");
		int assembly_and_install_cost_method = as_integer("assembly_and_install_cost_method");
		int other_infrastructure_cost_method = as_integer("other_infrastructure_cost_method");

		int array_cable_system_cost_method = as_integer("array_cable_system_cost_method");
		int export_cable_system_cost_method = as_integer("export_cable_system_cost_method");
		int onshore_substation_cost_method = as_integer("onshore_substation_cost_method");
		int offshore_substation_cost_method = as_integer("offshore_substation_cost_method");
		int other_elec_infra_cost_method = as_integer("other_elec_infra_cost_method");

		// check for user entered values
		if (structural_assembly_cost_method == 0)
			structural_assembly = as_double("structural_assembly_cost_input") * system_capacity_kW;
		else if (structural_assembly_cost_method == 1)
			structural_assembly = as_double("structural_assembly_cost_input");
		//else if (structural_assembly_cost_method == 3)
			//structural_assembly = as_double("structural_assembly_cost_total");
		if (power_takeoff_system_cost_method == 0)
			power_takeoff = as_double("power_takeoff_system_cost_input") * system_capacity_kW;
		else if (power_takeoff_system_cost_method == 1)
			power_takeoff = as_double("power_takeoff_system_cost_input");
		//else if (power_takeoff_system_cost_method == 3)
			//power_takeoff = as_double("power_takeoff_system_cost_total");
		if (mooring_found_substruc_cost_method == 0)
			mooring_found_substruc = as_double("mooring_found_substruc_cost_input") * system_capacity_kW;
		else if (mooring_found_substruc_cost_method == 1)
			mooring_found_substruc = as_double("mooring_found_substruc_cost_input");
		//else if (mooring_found_substruc_cost_method == 3)
			//mooring_found_substruc = as_double("mooring_found_substruc_cost_total");

		if (development_cost_method == 0)
			development = as_double("development_cost_input") * system_capacity_kW;
		else if (development_cost_method == 1)
			development = as_double("development_cost_input");
		if (eng_and_mgmt_cost_method == 0)
			eng_and_mgmt = as_double("eng_and_mgmt_cost_input") * system_capacity_kW;
		else if (eng_and_mgmt_cost_method == 1)
			eng_and_mgmt = as_double("eng_and_mgmt_cost_input");
		if (assembly_and_install_cost_method == 0)
			assembly_and_install = as_double("assembly_and_install_cost_input") * system_capacity_kW;
		else if (assembly_and_install_cost_method == 1)
			assembly_and_install = as_double("assembly_and_install_cost_input");
		if (other_infrastructure_cost_method == 0)
			other_infrastructure = as_double("other_infrastructure_cost_input") * system_capacity_kW;
		else if (other_infrastructure_cost_method == 1)
			other_infrastructure = as_double("other_infrastructure_cost_input");

		if (array_cable_system_cost_method == 0)
			array_cable_system = as_double("array_cable_system_cost_input") * system_capacity_kW;
		else if (array_cable_system_cost_method == 1)
			array_cable_system = as_double("array_cable_system_cost_input");
		if (export_cable_system_cost_method == 0)
			export_cable_system = as_double("export_cable_system_cost_input") * system_capacity_kW;
		else if (export_cable_system_cost_method == 1)
			export_cable_system = as_double("export_cable_system_cost_input");
		if (onshore_substation_cost_method == 0)
			onshore_substation = as_double("onshore_substation_cost_input") * system_capacity_kW;
		else if (onshore_substation_cost_method == 1)
			onshore_substation = as_double("onshore_substation_cost_input");
		if (offshore_substation_cost_method == 0)
			offshore_substation = as_double("offshore_substation_cost_input") * system_capacity_kW;
		else if (offshore_substation_cost_method == 1)
			offshore_substation = as_double("offshore_substation_cost_input");
		if (other_elec_infra_cost_method == 0)
			other_elec_infra = as_double("other_elec_infra_cost_input") * system_capacity_kW;
		else if (other_elec_infra_cost_method == 1)
			other_elec_infra = as_double("other_elec_infra_cost_input");


		// Now, we calculated the CapEx using whatever combination of modeled values and user-entered values
		// that we have at this point.
		// CapEx is defined to include all device costs and BOS costs that are not CapEx dependent
		double capex = structural_assembly + power_takeoff + mooring_found_substruc
			+ development + eng_and_mgmt + assembly_and_install + other_infrastructure
			+ array_cable_system + export_cable_system + onshore_substation + offshore_substation + other_elec_infra;

		// Calculate the CapEx dependent BOS costs
		plant_commissioning = 0.016 * capex;
		site_access_port_staging = 0.011 * capex;

		// Calculate the CapEx-dependent financial costs
		project_contingency = 0.05 * capex;
		insurance_during_construction = 0.01 * capex;
		reserve_accounts = 0.03 * capex;

		// Assign the CapEx-dependent outputs
		assign("plant_commissioning_cost_modeled", var_data(static_cast<ssc_number_t>(plant_commissioning)));
		assign("site_access_port_staging_cost_modeled", var_data(static_cast<ssc_number_t>(site_access_port_staging)));
		assign("project_contingency", var_data(static_cast<ssc_number_t>(project_contingency)));
		assign("insurance_during_construction", var_data(static_cast<ssc_number_t>(insurance_during_construction)));
		assign("reserve_accounts", var_data(static_cast<ssc_number_t>(reserve_accounts)));

		

		

		

	}

};

DEFINE_MODULE_ENTRY(mhk_costs, "Calculates various cost categories for Marine Energy arrays for different device types.", 3);
