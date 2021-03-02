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

static var_info _cm_vtab_windcsm[] = {
/*   VARTYPE           DATATYPE         NAME                                LABEL                                UNITS     META           GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/			
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_class",					 "Turbine class",                     "",      "",            "wind_csm",      "?=0",                    "INTEGER,MIN=0,MAX=3",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_user_exponent",			 "Turbine user exponent",             "",      "",            "wind_csm",      "?=2.5",                  "",                               "" },
	{ SSC_INPUT,		SSC_NUMBER,      "turbine_carbon_blades",            "Turbine carbon blades",             "0/1",   "",            "wind_csm",      "?=0",                    "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,		SSC_NUMBER,      "turbine_rotor_diameter",           "Turbine rotor diameter",            "m",     "",            "wind_csm",      "*",                      "",                               "" },

	{ SSC_INPUT,		SSC_NUMBER,      "machine_rating",                   "Machine rating",                    "kW",    "",            "wind_csm",      "*",                      "",                               "" },

	{ SSC_INPUT,		SSC_NUMBER,      "rotor_torque",                     "Rotor torque",                      "Nm",    "",            "wind_csm",      "*",                      "",                               "" },

	{ SSC_INPUT,		SSC_NUMBER,      "onboard_crane",                    "Onboard crane",                     "0/1",   "",            "wind_csm",      "?=0",                    "INTEGER,MIN=0,MAX=1",            "" },

	{ SSC_INPUT,		SSC_NUMBER,      "hub_height",                       "Hub height",                        "m",     "",            "wind_csm",      "*",                      "",                               "" },

	{ SSC_INPUT,		SSC_NUMBER,      "num_blades",                       "Number of blades",                  "",      "",            "wind_csm",      "?=3",                    "INTEGER,MIN=1",                  "" },
	
	{ SSC_INPUT,		SSC_NUMBER,      "num_bearings",                     "Number of main bearings",           "",      "",            "wind_csm",      "?=2",                    "INTEGER,MIN=1",                  "" },

	// Outputs intermediate percentages and cost breakdown and total cost
	{ SSC_OUTPUT,       SSC_NUMBER,      "rotor_mass",                       "Rotor mass",                        "kg",    "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "rotor_cost",                       "Rotor cost",                        "$",     "",            "wind_csm",      "*",                       "",                              "" },
	// rotor breakdown
	{ SSC_OUTPUT,       SSC_NUMBER,      "blade_cost",                       "Rotor cost",                        "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "hub_cost",                         "Hub cost",                          "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "pitch_cost",                       "Pitch cost",                        "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "spinner_cost",                     "Spinner cost",                      "$",     "",            "wind_csm",      "*",                       "",                              "" },

	
	{ SSC_OUTPUT,       SSC_NUMBER,      "drivetrain_mass",                  "Drivetrain mass",                   "kg",    "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "drivetrain_cost",                  "Drivetrain cost",                   "$",     "",            "wind_csm",      "*",                       "",                              "" },
	// Drive train or Nacelle breakdown
	{ SSC_OUTPUT,       SSC_NUMBER,      "low_speed_side_cost",              "Low speed side cost",               "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "main_bearings_cost",               "Main bearings cost",                "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "gearbox_cost",                     "Gearbox cost",                      "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "high_speed_side_cost",             "High speed side cost",              "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "generator_cost",                   "Generator cost",                    "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "bedplate_cost",                    "Bedplate cost",                     "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "yaw_system_cost",                  "Yaw system cost",                   "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "variable_speed_electronics_cost",  "Variable speed electronics cost",   "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "hvac_cost",                        "HVAC cost",                         "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "electrical_connections_cost",      "Electrical connections cost",       "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "controls_cost",                    "Controls cost",                     "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "mainframe_cost",                   "Mainframe cost",                    "$",     "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "transformer_cost",                 "Transformer cost",                  "$",     "",            "wind_csm",      "*",                       "",                              "" },



	{ SSC_OUTPUT,       SSC_NUMBER,      "tower_mass",                       "Tower mass",                        "kg",    "",            "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tower_cost",                       "Tower cost",                        "$",     "",            "wind_csm",      "*",                       "",                              "" },

	// overall cost
	{ SSC_OUTPUT,       SSC_NUMBER,      "turbine_cost",                     "Turbine cost",                      "$",     "",            "wind_csm",      "*",                       "",                              "" },


var_info_invalid };

// $/kg costs from WISDEM https://github.com/WISDEM/Turbine_CostsSE


class cm_windcsm : public compute_module
{
public:
	cm_windcsm()
	{
		add_var_info(_cm_vtab_windcsm);
	}



	void exec( )
	{
		// mass values from nrel_csm_tcc_2015.py
		// Cost values from turbine_costsse_2015.py


		// get values
		double blades = (double)as_number("num_blades");
		double turbine_user_exponent = (double)as_number("turbine_user_exponent");
		double turbine_rotor_diameter = (double)as_number("turbine_rotor_diameter");
		bool turbine_carbon_blades = as_integer("turbine_carbon_blades")==1;
		int turbine_class = as_integer("turbine_class");
		double exponent = 0;
		switch (turbine_class)
		{
		case 0:
			exponent = turbine_user_exponent;
			break;
		case 1:
		{
			if (turbine_carbon_blades)
				exponent = 2.47;
			else
				exponent = 2.54;
		}
			break;
		case 2:
		case 3:
		{
			if (turbine_carbon_blades)
				exponent = 2.44;
			else
				exponent = 2.50;
		}
		break;
		default:
			exponent = 2.5;
		}
		double blade_mass = 0.5 * pow((0.5*turbine_rotor_diameter), exponent);
		double blade_mass_cost_coeff = 14.6; // line 27
		//double blade_mass_cost_coeff_2015 = 13.08; // line 152
		double blade_cost_2015 = blade_mass_cost_coeff * blade_mass;

		// hub mass
		double hub_mass = 2.3 * blade_mass + 1320.0;
		double hub_mass_cost_coeff = 3.9; // line 45
		//double hub_mass_cost_coeff_2015 = 3.8; // line 154
		double hub_cost_2015 = hub_mass_cost_coeff * hub_mass;

		// pitch mass
		double pitch_bearing_mass = 0.1295 * blade_mass * blades + 491.31;
		double pitch_mass = pitch_bearing_mass * (1.0 + 0.3280) + 555.0;
		double pitch_mass_cost_coeff = 22.1; // line 63
		//double pitch_mass_cost_coeff_2015 = 22.91; // line 156
		double pitch_cost_2015 = pitch_mass_cost_coeff * pitch_mass;

		// spinner mass
		double spinner_mass = 15.5 * turbine_rotor_diameter - 980.0;
		double spinner_mass_cost_coeff = 11.1; // line 81
		//double spinner_mass_cost_coeff_2015 = 15.59; // line 158
		double spinner_cost_2015 = spinner_mass_cost_coeff * spinner_mass;


		double rotor_mass = blade_mass + hub_mass + pitch_mass + spinner_mass;

		// cost adders
		double hub_assembly_cost_multiplier = 0.0;
		double hub_overhead_cost_multiplier = 0.0;
		double hub_profit_multiplier = 0.0;
		double hub_transport_multiplier = 0.0;

		double parts_cost = hub_cost_2015 + pitch_cost_2015 + spinner_cost_2015;
		double hub_system_cost_adder_2015 = (1.0 + hub_transport_multiplier + hub_profit_multiplier)
			* ((1.0 + hub_overhead_cost_multiplier + hub_assembly_cost_multiplier) * parts_cost);

		int num_blades = as_integer("num_blades");
		double rotor_cost_adder_2015 = blade_cost_2015 * (double)num_blades + hub_system_cost_adder_2015;



		///////////////////////////////////////////////////////////////////////////////////////
		// Drivetrain - 13 subcomponents

		double machine_rating = as_double("machine_rating"); //kW

		double low_speed_shaft_mass = 13.0 * pow((blade_mass *  machine_rating / 1000.0), 0.65) + 775.0;
		double low_speed_shaft_mass_cost_coeff = 11.9;; // line 211
		double low_speed_shaft_cost = low_speed_shaft_mass_cost_coeff * low_speed_shaft_mass;

		double bearings_mass = 0.0001 * pow(turbine_rotor_diameter, 3.5);
		int num_bearings = as_integer("num_bearings");
		double bearings_mass_cost_coeff = 4.5; // line 230
		double bearings_cost = (double)num_bearings * bearings_mass_cost_coeff * bearings_mass;

		double rotor_torque = as_double("rotor_torque");

		double gearbox_mass = 113.0 * pow(rotor_torque / 1000.0, 0.71);
		double gearbox_mass_cost_coeff = 12.9; // line 248
		double gearbox_cost = gearbox_mass_cost_coeff * gearbox_mass;

		double high_speed_side_mass = 0.19894 * machine_rating;
		double high_speed_side_mass_cost_coeff = 6.8; // line 266
		double high_speed_side_cost = high_speed_side_mass_cost_coeff * high_speed_side_mass;

		double generator_mass = 2300.0 * machine_rating / 1000.0 + 3400.0;
		double generator_mass_cost_coeff = 12.4; // line 284
		double generator_cost = generator_mass_cost_coeff * generator_mass;

		double bedplate_mass = pow(turbine_rotor_diameter, 2.2);
		double bedplate_mass_cost_coeff = 2.9; // line 302
		double bedplate_cost = bedplate_mass_cost_coeff * bedplate_mass;

		double yaw_system_mass = 1.5 * (0.0009 * pow(turbine_rotor_diameter, 3.314));
		double yaw_system_mass_cost_coeff = 8.3; // line 320
		double yaw_system_cost = yaw_system_mass_cost_coeff * yaw_system_mass;

// Variable speed - no mass calculations but have costs
		double variable_speed_elec_mass = 0; //???
		double variable_speed_elec_mass_cost_coeff = 18.8; // line 338
		double variable_speed_elec_cost = variable_speed_elec_mass_cost_coeff * variable_speed_elec_mass;

		double hydraulic_cooling_mass = 0.08 * machine_rating;
		double hydraulic_cooling_mass_cost_coeff = 124.0; // line 356
		double hydraulic_cooling_cost = hydraulic_cooling_mass_cost_coeff * hydraulic_cooling_mass;

		double nacelle_cover_mass = 1.2817 * machine_rating + 428.19;
		double nacelle_cover_mass_cost_coeff = 5.7; // line 374
		double nacelle_cover_cost = nacelle_cover_mass_cost_coeff * nacelle_cover_mass;

// No mass calculations for electornic connections and controls - based on turbine rating only.
		double elec_connec_machine_rating_mass_cost_coeff = 41.85; // line 392
		double elec_connec_machine_rating_cost = elec_connec_machine_rating_mass_cost_coeff * machine_rating;

		double controls_machine_rating_mass_cost_coeff = 21.15; // line 409
		double controls_machine_rating_cost = controls_machine_rating_mass_cost_coeff * machine_rating;


		double nacelle_platforms_mass = 0.125 * bedplate_mass;
		double nacelle_platforms_mass_cost_coeff = 17.1; // line 427
		double nacelle_platforms_cost = nacelle_platforms_mass_cost_coeff * nacelle_platforms_mass;

		bool onboard_crane = as_integer("onboard_crane") == 1;
		double crane_mass = 0.0, crane_cost = 0.0;
		if (onboard_crane)
		{
			crane_mass = 3000.0; //kg line 259 nrel_csm_tcc_2015.py
			crane_cost = 12000.0; // line 429 nrel_csm_costsse_2015.py
			nacelle_platforms_cost = nacelle_platforms_mass_cost_coeff * (nacelle_platforms_mass - crane_mass);
		}

		// bedplate cost is input and crane cost is hard coded at line 429
		// Note that bedplate cost only used in base hardware cost which is commented out 
		// turbine_costsse_2015.py line 450.
		//double base_hardware_cost_coeff = 0.7;
		//double base_hardware_cost = base_hardware_cost_coeff * bedplate_cost;

		double mainframe_cost = nacelle_platforms_cost + crane_cost;

		double other_mass = nacelle_platforms_mass + crane_mass;

		double transformer_mass = 1915.0 * machine_rating / 1000.0 + 1910.0;
		double transformer_mass_cost_coeff = 18.8; // line 462
		double transformer_cost = transformer_mass_cost_coeff * transformer_mass;

		double drivetrain_mass = low_speed_shaft_mass + bearings_mass + gearbox_mass + high_speed_side_mass
			+ generator_mass + bedplate_mass + yaw_system_mass + hydraulic_cooling_mass
			+ nacelle_cover_mass + other_mass + transformer_mass;

		// inputs?
		double nacelle_assembly_cost_multiplier = 0.0;
		double nacelle_overhead_cost_multiplier = 0.0;
		double nacelle_profit_multiplier = 0.0;
		double nacelle_transport_multiplier = 0.0;

		parts_cost = low_speed_shaft_cost + bearings_cost + gearbox_cost + high_speed_side_cost
			+ generator_cost + bedplate_cost + yaw_system_cost + variable_speed_elec_cost
			+ hydraulic_cooling_cost + nacelle_cover_cost + elec_connec_machine_rating_cost 
			+ controls_machine_rating_cost + mainframe_cost + transformer_cost;

		double nacelle_system_cost_adder_2015 = (1.0 + nacelle_transport_multiplier
			+ nacelle_profit_multiplier) * ((1.0 + nacelle_overhead_cost_multiplier
				+ nacelle_assembly_cost_multiplier) * parts_cost);


		//////////////////////////////////////////////////////////////////////////////////////
		// tower mass
		double hub_height = as_double("hub_height");

		double tower_mass = 19.828 * pow(hub_height, 2.0282);
		double tower_mass_cost_coeff = 2.9; // line 660
		double tower_cost = tower_mass_cost_coeff * tower_mass;

		double tower_assembly_cost_multiplier = 0.0;
		double tower_overhead_cost_multiplier = 0.0;
		double tower_profit_multiplier = 0.0;
		double tower_transport_multiplier = 0.0;

		double tower_cost_adder_2015 = (1.0 + tower_transport_multiplier + tower_profit_multiplier)
			* ((1.0 + tower_overhead_cost_multiplier + tower_assembly_cost_multiplier) * tower_cost);


		double turbine_assembly_cost_multiplier = 0.0;
		double turbine_overhead_cost_multiplier = 0.0;
		double turbine_profit_multiplier = 0.0;
		double turbine_transport_multiplier = 0.0;

		parts_cost = rotor_cost_adder_2015 + nacelle_system_cost_adder_2015 + tower_cost_adder_2015;

		double turbine_cost_adder_2015 = (1.0 + turbine_transport_multiplier + turbine_profit_multiplier)
			* ((1.0 + turbine_overhead_cost_multiplier + turbine_assembly_cost_multiplier) * parts_cost);


		// assign outputs
		// Outputs intermediate percentages and cost breakdown and total cost
		assign("rotor_mass", var_data(ssc_number_t(rotor_mass)));
		assign("rotor_cost", var_data(ssc_number_t(rotor_cost_adder_2015)));
			// rotor breakdown
		assign("blade_cost", var_data(ssc_number_t(blade_cost_2015)));
		assign("hub_cost", var_data(ssc_number_t(hub_cost_2015)));
		assign("pitch_cost", var_data(ssc_number_t(pitch_cost_2015)));
		assign("spinner_cost", var_data(ssc_number_t(spinner_cost_2015)));

		assign("drivetrain_mass", var_data(ssc_number_t(drivetrain_mass)));
		// cost
		assign("drivetrain_cost", var_data(ssc_number_t(nacelle_system_cost_adder_2015)));
			// Drive train or Nacelle breakdown
		assign("low_speed_side_cost", var_data(ssc_number_t(low_speed_shaft_cost)));
		assign("main_bearings_cost", var_data(ssc_number_t(bearings_cost)));
		assign("gearbox_cost", var_data(ssc_number_t(gearbox_cost)));
		assign("high_speed_side_cost", var_data(ssc_number_t(high_speed_side_cost)));
		assign("generator_cost", var_data(ssc_number_t(generator_cost)));
		assign("bedplate_cost", var_data(ssc_number_t(bedplate_cost)));
		assign("yaw_system_cost", var_data(ssc_number_t(yaw_system_cost)));
		assign("variable_speed_electronics_cost", var_data(ssc_number_t(variable_speed_elec_cost)));
		assign("hvac_cost", var_data(ssc_number_t(hydraulic_cooling_cost)));
		assign("electrical_connections_cost", var_data(ssc_number_t(elec_connec_machine_rating_cost)));
		assign("controls_cost", var_data(ssc_number_t(controls_machine_rating_cost)));
		assign("mainframe_cost", var_data(ssc_number_t(mainframe_cost)));
		assign("transformer_cost", var_data(ssc_number_t(transformer_cost)));


		assign("tower_mass", var_data(ssc_number_t(tower_mass)));
		assign("tower_cost", var_data(ssc_number_t(tower_cost_adder_2015)));

			// overall cost
		assign("turbine_cost", var_data(ssc_number_t(turbine_cost_adder_2015)));

	}
};

DEFINE_MODULE_ENTRY( windcsm, "WISDEM turbine cost model", 1 )
