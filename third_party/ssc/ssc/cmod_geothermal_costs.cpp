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
#include <stdio.h>
#include <math.h>
#include "lib_geothermal.h"
#include "common.h"


static var_info _cm_vtab_geothermal_costs[] = {
	/*   VARTYPE			DATATYPE         NAME                              LABEL                                                       UNITS		META                      GROUP                   REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

		{ SSC_INPUT,        SSC_NUMBER,     "conversion_type",					"Conversion Type",											"",			"",						"GeoHourly",			    "*",                        "INTEGER",					    "" },
		// Binary Plant Type Inputs:		
		{ SSC_INPUT,		SSC_NUMBER,     "gross_output",						"Gross output from GETEM",									"kW",		"",						"GeoHourly",				"*",						"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"design_temp",						"Power block design temperature",							"C",        "",						"GeoHourly",				"*",						"",								"" },
		{ SSC_INPUT,        SSC_NUMBER,     "eff_secondlaw",					"Second Law Efficiency",									"%",		"",						"GeoHourly",				"*",						"",								"" },
		// Flash Plant Type Inputs:
		{ SSC_INPUT,		SSC_NUMBER,		"qRejectTotal",						"Total Rejected Heat",										"btu/h",			"",				"GeoHourly",				"conversion_type=1",		"",								""},
		{ SSC_INPUT,		SSC_NUMBER,		"qCondenser",						"Condenser Heat Rejected",									"btu/h",	"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"v_stage_1",						"Vacumm Pump Stage 1",										"kW",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"v_stage_2",						"Vacumm Pump Stage 2",										"kW",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"v_stage_3",						"Vacumm Pump Stage 3",										"kW",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"GF_flowrate",						"GF Flow Rate",												"lb/h",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"qRejectByStage_1",					"Heat Rejected by NCG Condenser Stage 1",					"BTU/hr",	"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"qRejectByStage_2",					"Heat Rejected by NCG Condenser Stage 2",					"BTU/hr",	"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"qRejectByStage_3",					"Heat Rejected by NCG Condenser Stage 3",					"BTU/hr",	"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"ncg_condensate_pump",				"Condensate Pump Work",										"kW",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"cw_pump_work",						"CW Pump Work",												"kW",		"",						"GeoHourly",				"conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"pressure_ratio_1",					"Suction Steam Ratio 1",									"",			"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"pressure_ratio_2",					"Suction Steam Ratio 2",									"",			"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"pressure_ratio_3",					"Suction Steam Ratio 3",									"",			"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"condensate_pump_power",			"hp",														"",			"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"cwflow",							"Cooling Water Flow",										"lb/h",		"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"cw_pump_head",						"Cooling Water Pump Head",									"lb/h",		"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"spec_vol",							"Specific Volume",											"cft/lb",	"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"spec_vol_lp",						"LP Specific Volume",										"cft/lb",	"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"x_hp",								"HP Mass Fraction",											"%",		"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"x_lp",								"LP Mass Fraction",											"%",		"",						"GeoHourly",				 "conversion_type=1",		 "",							"" },
		{ SSC_INPUT,		SSC_NUMBER,		"hp_flash_pressure",				"HP Flash Pressure",										"psia",		"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"lp_flash_pressure",				"LP Flash Pressure",										"psia",		"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		{ SSC_INPUT,		SSC_NUMBER,		"flash_count",						"Flash Count",												"(1 -2)",	"",						"GeoHourly",				 "conversion_type=1",		"",								"" },
		// Outputs	

		{ SSC_OUTPUT,       SSC_NUMBER,     "baseline_cost",					"Baseline Cost",											"$/kW",		"",                     "GeoHourly",				"?",                         "",                            "" },
		var_info_invalid };



class cm_geothermal_costs : public compute_module
{
private:

	//Inputs for Binary Type Plant (Note: Some variables might be common to both plant types - Binary and Flash)
	std::vector<double> hx_ppi{ 0.890669720,0.919862622,0.938752147,0.957069262,0.963938180,0.972524327,0.983400114,1.000000000,0.998855180,1.066399542,1.226674299,1.333142530,1.377790498,1.438465942,1.414997138,1.423583286,1.464224385,1.513451631,1.535203205,1.555237550,1.604464797,1.643961076,1.657698912, 0.000000000 };		//HX Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> steel_ppi{ 1.128834356,1.102541630,1.108676599,1.073619632,0.999123576,1.021910605,0.961437336,1.000000000,1.064855390,1.423312883,1.499561788,1.634531113,1.762489045,2.159509202,1.612620508,1.958808063,2.219106047,2.109553024,1.984224365,2.034180543,1.714285714,1.638913234,1.858019281,0.000000000 };	//Steel Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> process_equip_ppi{ 0.884044412,0.907406542,0.926150373,0.942470679,0.956807105,0.967657025,0.985381166,1.000000000,1.014742613,1.077732637,1.155135271,1.222672817,1.304587248,1.382668341,1.403383400,1.411178107,1.455344223,1.509492085,1.533628545,1.638936512,1.656479161,1.653172080,1.679672296,0.000000000 }; //Process Equipment Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> engineering_ppi{ 0.779879622,0.810834050,0.859415305,0.888650043,0.913585555,0.954428203,0.975924334,1.000000000,1.048581255,1.081685297,1.102751505,1.136285469,1.210232158,1.275150473,1.329750645,1.392089424,1.362424764,1.365004299,1.388650043,1.433791917,1.486242476,1.503869304,1.558039553,0.000000000 }; // Engineering Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> pump_ppi{ 0.853394181,0.872219053,0.899600685,0.924130063,0.936679977,0.950370793,0.976041072,1.000000000,1.010838562,1.039931546,1.093553908,1.142042213,1.213348545,1.278379920,1.314318311,1.324586423,1.324586423,1.349115801,1.339418140,1.366799772,1.391899601,1.411294923,1.438106104,0.000000000 }; //Pump Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> turbine_ppi{ 0.882850242,0.896135266,0.917874396,0.934782609,0.960144928,0.969202899,0.980072464,1.000000000,1.013285024,1.018719807,1.017512077,1.050120773,1.106884058,1.245169082,1.350241546,1.340579710,1.359903382,1.349637681,1.376811594,1.411835749,1.399154589,1.403046162,1.346947738,1.327974034 }; //Turbine-Generator Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> construction_ppi{ 0.790555556,0.816666667,0.842222222,0.872777778,0.909444444,0.933333333,0.957777778,1.000000000,1.039444444,1.067222222,1.088888889,1.129444444,1.170000000,1.221666667,1.277777778,1.320000000,1.357777778,1.361666667,1.374444444,1.426666667,1.475000000,1.528333333,1.594444444,0.000000000 };
	double user_adjust = 1;
	double size_ratio;
	//double scaling_factor ;	//for the GF HX
	double ref_plant_size = 10000;	//kW
	double hx_cost_adjust = 1;
	double hx_cost;
	double sf_hx;
	double hx_gf_c1;
	double hx_gf_c2;
	double hx_gf_c;	//ref gf hx cost
	double sales_tax = 0.05; //Always 5% ? 
	double freight = 0.05;	//Also always 5% ?
	//freight hydrothermal_binary = 0.05;
	//freight hydrothermal_flash = 
	double total_material_cost_multiplier = 1.7;
	double steel = 0.22;
	double current_cost_ref_hx;

	double sf_condenser;
	double condenser_cost;
	double wf_pump_cost;
	double turbine_cost;

	double corrected_equip_cost;

	//Defining variables used in Direct Construction Cost Multiplier:
	double dc_cost_multiplier;	//Direct Construction Cost Multiplier  [Defined]
	double corrected_total_material_mult;
	double corrected_construct_malts;
	double plant_size_adjustment;
	double direct_installation_multiplier;
	double escalation_equip_cost;	//Escalation in Equipment Cost
	double const_matls_rentals = 0.25;
	double multiplier_input_year;
	double corrected_labor;
	double labor_cost_multiplier = 0.27;
	double labor_fringe_benefits = 0.45;

	//Coefficients for GF HX cost calculation:
	double sf_0 = 1.01216;
	double sf_1 = -0.000760473;
	double sf_2 = 2.0145e-6;
	double sf_3 = 0;
	double hx_c10 = 5.95;
	double hx_c11 = 2163827753;
	double hx_c12 = -3.810541361;
	double hx_c20 = -22.09917;
	double hx_c21 = 0.4275955;
	double hx_c22 = -0.002356472;
	double hx_c23 = 4.244622e-06;

	//Coefficients for Air Cooled Condenser (ACC) Cost Calculations:
	double acc_c0 = 47;
	double acc_c1 = 11568490;
	double acc_c2 = -2.350919;
	double acc_c10 = 15.52712;
	double acc_c11 = 0.005950211;
	double acc_c12 = -0.001200635;
	double acc_c13 = 0.000005657483;
	double acc_c20 = 3.582461;
	double acc_c21 = -0.05107826;
	double acc_c22 = 0.000277465;
	double acc_c23 = -2.549391e-07;
	double acc_0;
	double acc_1;
	double acc_2;
	double accc_0;
	double accc_1;
	double accc_2;
	double acc_c;	//ref acc cost 
	double current_cost_ref_acc;

	//Coefficients for WF Pump Cost Calculation:
	double sf_wf;
	double wf_sf_c0 = -0.777900000001;
	double wf_sf_c1 = 0.029802;
	double wf_sf_c2 = -0.00019008;
	double wf_sf_c3 = 3.872e-07;
	double wf_c10 = 32.0607143;
	double wf_c11 = -0.2537857;
	double wf_c12 = 0.0006714;
	double wf_c20 = -0.3329714;
	double wf_c21 = 0.0559291;
	double wf_c22 = -0.0001977;
	double pcc_1;
	double pcc_2;
	double pcc_c;	//ref wf pump cost
	double current_cost_ref_pcc;

	//Coefficients for Turbine Cost Calculation: 
	double turbine_sf_c0 = 0.6642;
	double turbine_sf_c1 = 0.0003589091;
	double turbine_sf_c2 = -2.0218e-06;
	double sf_turbine;
	double turbine_c0 = 0.79664761905;
	double turbine_c1 = -0.00977366138;
	double turbine_c2 = 0.00004244825;
	double turbine_c3 = -5.321e-08;
	double turbine_c10 = -24.62889285714;
	double turbine_c11 = 0.49768131746;
	double turbine_c12 = -0.00296254476;
	double turbine_c13 = 5.52551e-06;
	double ppc_0;
	double ppc_1;
	double turbine_c;	//Reference Turbine Cost
	double generator_c; //Reference Generator Cost
	double max_turbine_size = 15000 * 0.7457; //From GETEM
	double parasitic;
	double tg_size;
	double tg_sets;
	double ref_turbine_cost;
	double tg_cost;
	double current_cost_ref_tg;

	double plant_equip_cost, baseline_cost;



	//Inputs for Flash Plant Type:
	int tg_sets_num = 1; //Number of T-G Sets
	double cooling_tower_cost;
	double condenser_cost_flash;
	double lmtd;
	double condenser_pinch_pt = 7.50; //As seen in GETEM, this value is always 7.50
	double dtCooling_water = 25.0;
	double condenser_u = 350.00; //As seen in GETEM, this value is always 350.00
	double area;
	//double qCondenser;
	double hp_total_cost;
	//double hp_flash_pressure;
	double a_cross_section;
	//double flash_vessels_cost = 552981.85;
	double vacuum_pump_1;	//stage 1 cost of vacuum pump
	double vacuum_pump_2;	//stage 2 cost of vacuum pump
	double vacuum_pump_3;	//stage 3 cost of vacuum pump
	double vacuum_pump;		//vacuum pump cost (sum of all 3 stages)
	int U = 350;				//Heat Transfer Coefficient
	double cond_area_1;
	double cond_area_2;
	double cond_area_3;
	double condenser_ncg;
	double ncg_pump_work;
	double ncg_water_pump;
	double pump_ncg;
	double ejector_ncg;
	double cw_pump_power;
	double condensate;
	double condensate_pump;
	double cooling_water;
	double pump_cost;
	double h2s_level = 20; //ppm
	double h2s_flow;
	double h2s_cost;
	double flash_vessel_cost;
	double equip_cost_flash;
	double ncg_cost;
	double current_tg_cost;
	double current_vessel_cost;
	double current_tower_cost;
	double current_condenser_cost;
	double current_pump_cost;
	double current_ncg_cost;
	double current_h2s_cost;

	//HP Flash Vessel Cost Inputs:
	double current_cost_flash;
	double m_stm, m_stm_lp;
	double hp_steam_flow;
	double max_drop_size = 200; //(microns)
	double v_terminal;
	double num_vessels; //number of hp flash vessels
	double area_xsection_hp;
	double hp_flash_volume;
	double A, D, H, A_lp, D_lp, H_lp;
	double hp_flash_cost;

	//LP Flash Vessel Cost Calculation:
	double num_vessels_lp, a_xsection_lp, v_terminal_lp, lp_steam_flow, lp_flash_volume, lp_flash_cost,
		direct_multiplier_2002, tax, labor_multiplier, construction_multiplier, freight_flash, material_multiplier, escalation_ppi,
		direct_plant_cost, condenser_heat_rejected, vStage_3;



public:
	//double hx_cost();

	cm_geothermal_costs() {

		add_var_info(_cm_vtab_geothermal_costs);
	}


	void exec() override
	{
		SGeothermal_Inputs geo_inputs;
		int conversion_type = as_integer("conversion_type");

		if (conversion_type == 0) {
			//geo_inputs.me_ct = BINARY;

			// for inputs, to get use as_integer, as_double, etc, e.g.
			//double unit_plant = as_double("nameplate");		//Gross plant size
			double design_temp = as_double("design_temp");
			double eff = as_double("eff_secondlaw");	// w-h/lb
			double unit_plant = as_double("gross_output");

			//Geofluid Heat Exchangers Equipment Cost Calculations:				
			size_ratio = unit_plant / ref_plant_size;
			sf_hx = (sf_3 * pow(design_temp, 3)) + (sf_2 * pow(design_temp, 2)) + (sf_1 * design_temp) + sf_0;
			hx_gf_c1 = hx_c10 + (hx_c11 * pow(design_temp, hx_c12));
			hx_gf_c2 = hx_c20 + (hx_c21 * design_temp) + (hx_c22 * pow(design_temp, 2)) + (hx_c23 * pow(design_temp, 3));
			hx_gf_c = hx_gf_c1 * exp(hx_gf_c2 * eff);
			current_cost_ref_hx = hx_gf_c * hx_ppi[20];

			hx_cost = user_adjust * pow(size_ratio, sf_hx)*((ref_plant_size*hx_gf_c*hx_ppi[20]) / unit_plant);


			//Air Cooled Condenser Cost Calculations:
			sf_condenser = 1;
			accc_0 = (acc_c1 * pow(design_temp, acc_c2)) + acc_c0;
			accc_1 = exp((acc_c13*pow(design_temp, 3)) + (acc_c12*pow(design_temp, 2)) + (acc_c11*design_temp) + acc_c10);
			accc_2 = exp((acc_c23*pow(design_temp, 3)) + (acc_c22*pow(design_temp, 2)) + (acc_c21*design_temp) + acc_c20);
			acc_c = accc_1 * pow(eff, accc_2) + accc_0;
			//acc_c = 183.65;
			current_cost_ref_acc = acc_c * hx_ppi[20];
			condenser_cost = user_adjust * pow(size_ratio, sf_condenser)*((ref_plant_size * acc_c * hx_ppi[20]) / unit_plant);



			//Working Fluid Pumps Cost Calculation:
			sf_wf = (wf_sf_c3*pow(design_temp, 3)) + (wf_sf_c2*pow(design_temp, 2)) + (wf_sf_c1*design_temp) + wf_sf_c0;
			pcc_1 = (wf_c12 * pow(design_temp, 2)) + (wf_c11 * design_temp) + wf_c10;
			pcc_2 = (wf_c22 * pow(design_temp, 2)) + (wf_c21 * design_temp) + wf_c20;
			pcc_c = pcc_1 * exp(pcc_2*eff);
			current_cost_ref_pcc = pcc_c * pump_ppi[20];
			wf_pump_cost = user_adjust * pow(size_ratio, sf_wf)*((ref_plant_size * pcc_c * pump_ppi[20]) / unit_plant);


			//Turbine-Generator (TG) Cost Calculation:
			if (unit_plant < ref_plant_size)
				sf_turbine = (turbine_sf_c2*pow(design_temp, 2)) + (turbine_sf_c1*design_temp) + turbine_sf_c0;
			else
				sf_turbine = 1;

			ppc_0 = turbine_c0 + (turbine_c1*design_temp) + (turbine_c2*pow(design_temp, 2)) + (turbine_c3*pow(design_temp, 3));
			ppc_1 = turbine_c10 + (turbine_c11*design_temp) + (turbine_c12*pow(design_temp, 2)) + (turbine_c13*pow(design_temp, 3));
			parasitic = (ppc_0 * exp(ppc_1 * eff))*ref_plant_size;
			tg_size = ref_plant_size + parasitic;
			tg_sets = tg_size / max_turbine_size;

			if (tg_sets > 1)
				turbine_c = 7400 * pow(max_turbine_size, 0.6);		//Total $ of reference turbine
			else
				turbine_c = 7400 * pow(tg_size, 0.6);				//Total $ of reference turbine

			if (tg_sets < 1)
				ref_turbine_cost = turbine_c / ref_plant_size;		//Cost of turbine per kW ($/kW)
			else
				ref_turbine_cost = (turbine_c * tg_sets) / ref_plant_size;

			generator_c = ((1800 * pow(tg_size, 0.67))) / ref_plant_size;
			tg_cost = ref_turbine_cost + generator_c;
			current_cost_ref_tg = tg_cost * turbine_ppi[20];
			turbine_cost = user_adjust * pow(size_ratio, sf_turbine)*((ref_plant_size * tg_cost * turbine_ppi[20]) / unit_plant);


			//Calculating Direct Construction Cost Multiplier:
			escalation_equip_cost = (current_cost_ref_acc + current_cost_ref_hx + current_cost_ref_pcc + current_cost_ref_tg) / (hx_gf_c + acc_c + pcc_c + tg_cost);
			corrected_labor = ((labor_cost_multiplier*engineering_ppi[20]) / escalation_equip_cost)*(1 + labor_fringe_benefits);
			corrected_construct_malts = (const_matls_rentals * process_equip_ppi[20]) / escalation_equip_cost;
			corrected_total_material_mult = ((steel*steel_ppi[20]) + ((total_material_cost_multiplier - 1 - steel)*process_equip_ppi[20]))*(1 / escalation_equip_cost) + 1;
			multiplier_input_year = corrected_total_material_mult + corrected_labor + corrected_construct_malts;
			plant_size_adjustment = 1.02875*pow((unit_plant / 1000), -0.01226);
			direct_installation_multiplier = plant_size_adjustment * multiplier_input_year;
			dc_cost_multiplier = (sales_tax + freight)*((corrected_total_material_mult + corrected_construct_malts)*plant_size_adjustment) + direct_installation_multiplier;


			//Total Plant Cost: 
			plant_equip_cost = hx_cost + condenser_cost + wf_pump_cost + turbine_cost;
			corrected_equip_cost = dc_cost_multiplier * plant_equip_cost;


			// for outputs, to assign, use:
			//assign("dc_cost_multiplier", var_data(static_cast<ssc_number_t>(dc_cost_multiplier)));
			assign("baseline_cost", var_data(static_cast<ssc_number_t>(corrected_equip_cost)));
		}

		else if (conversion_type == 1) {
			//geo_inputs.me_ct = FLASH;
			double unit_plant = as_double("gross_output");

			double qRejectTotal = as_double("qRejectTotal") / 1000000;		// Converting from btu/h to MMBTU/h
			double q_Condenser = as_double("qCondenser") / 1000000;			// Converting from btu/h to MMBTU/h




			//double hp_flash_pressure = as_double("hp_flash_pressure");
			double v_stage_1 = as_double("v_stage_1");
			double v_stage_2 = as_double("v_stage_2");
			double v_stage_3 = as_double("v_stage_3");
			double GF_flowrate = as_double("GF_flowrate");
			double qRejectByStage_1 = as_double("qRejectByStage_1");
			double qRejectByStage_2 = as_double("qRejectByStage_2");
			double qRejectByStage_3 = as_double("qRejectByStage_3");
			double ncg_condensate_pump = as_double("ncg_condensate_pump");
			double cw_pump_work = as_double("cw_pump_work");
			double pressure_ratio_1 = 1 / as_double("pressure_ratio_1");
			double pressure_ratio_2 = 1 / as_double("pressure_ratio_2");
			//double pressure_ratio_3 = 1 / as_double("pressure_ratio_3");
			int ncg_level = 2000;	//units: ppm
			double ncg_flow = GF_flowrate * ncg_level / 1000000; //units: lb/h
			double cwflow = as_double("cwflow");
			//double total_head = as_double("total_head");
			double condensate_pump_power = as_double("condensate_pump_power");
			double cw_pump_head = as_double("cw_pump_head");
			double spec_vol = as_double("spec_vol");
			double spec_vol_lp = as_double("spec_vol_lp");
			double x_hp = as_double("x_hp");		// %
			double x_lp = as_double("x_lp");	// %
			double hp_flash_pressure = as_double("hp_flash_pressure");
			double lp_flash_pressure = as_double("lp_flash_pressure");
			double flash_count = as_double("flash_count");
			double design_temp = as_double("design_temp");

			//T-G Cost:
			tg_cost = (tg_sets_num * (2830 * (pow((unit_plant / tg_sets_num), 0.745)))) + (3685 * (pow((unit_plant / tg_sets_num), 0.617)));	//Reference Equipment Cost
			current_tg_cost = tg_cost * turbine_ppi[20];


			//Cooling Tower Cost:
			condenser_heat_rejected = GF_flowrate * qRejectTotal / 1000;
			cooling_tower_cost = 7200 * (pow(condenser_heat_rejected, 0.8));		//Reference Equipment Cost
			current_tower_cost = cooling_tower_cost * process_equip_ppi[20];

			//Condenser Cost: 
			lmtd = (condenser_pinch_pt - (condenser_pinch_pt + dtCooling_water)) / (std::log(condenser_pinch_pt / (condenser_pinch_pt + dtCooling_water)));
			area = (q_Condenser*GF_flowrate / 1000) * 1000000 / (lmtd*condenser_u);
			condenser_cost_flash = 102 * pow(area, 0.85);		//Reference Equipment Cost
			current_condenser_cost = condenser_cost_flash * hx_ppi[20];

			//Flash Vessel Calculation:
			//HP Flash Cost Calculation:
			m_stm = x_hp * 1000; // (lb / h)
			hp_steam_flow = ((GF_flowrate / 1000)* m_stm) * spec_vol / 60;	// units: cfm
			v_terminal = (-0.0009414 * pow(max_drop_size, 2) * std::log(hp_flash_pressure)) + (0.01096 * pow(max_drop_size, 2));
			area_xsection_hp = hp_steam_flow / v_terminal;
			num_vessels = ceil(area_xsection_hp / 300);
			A = area_xsection_hp / num_vessels;
			D = pow((A * 4 / M_PI), 0.5);
			H = D * 3;
			hp_flash_volume = A * H * 7.4805;
			hp_flash_cost = num_vessels * ((hp_flash_pressure < 75) ? 166.5 * pow(hp_flash_volume, 0.625) : 110 * pow(hp_flash_volume, 0.68));

			//LP Flash Cost Calculation: 
			m_stm_lp = (flash_count == 2) ? (x_lp * 1000 * (1 - x_hp)) : 0;
			lp_steam_flow = ((GF_flowrate / 1000)*m_stm_lp) * spec_vol_lp / 60;	// (lb/h)
			v_terminal_lp = (flash_count == 1) ? 0 : ((-0.0009414 * pow(max_drop_size, 2) * std::log(lp_flash_pressure)) + (0.01096 * pow(max_drop_size, 2)));
			a_xsection_lp = (flash_count == 1) ? 0 : (lp_steam_flow / v_terminal_lp);
			num_vessels_lp = ceil(a_xsection_lp / 300);
			A_lp = (flash_count == 1) ? 0 : (a_xsection_lp / num_vessels_lp);
			D_lp = pow((A_lp * 4 / M_PI), 0.5);
			H_lp = D_lp * 3;
			lp_flash_volume = A_lp * H_lp * 7.4805;
			lp_flash_cost = (flash_count == 1) ? 0 : (num_vessels_lp * ((lp_flash_pressure < 75) ? (166.5 * pow(lp_flash_volume, 0.625)) : (110 * pow(lp_flash_volume, 0.68))));

			//Total Flash Vessel Cost:
			flash_vessel_cost = hp_flash_cost + lp_flash_cost;		//Reference Equipment Cost
			current_vessel_cost = flash_vessel_cost * process_equip_ppi[20];


			//NCG Removal System Cost: //Reference Equipment Cost
			//Vacuum Pump Cost breakdown:
			vacuum_pump_1 = (v_stage_1 < 5000) ? 70000 * pow(v_stage_1, 0.34) : 7400 * pow(v_stage_1, 0.6);
			vacuum_pump_2 = (v_stage_2 < 5000) ? 70000 * pow(v_stage_2, 0.34) : 7400 * pow(v_stage_2, 0.6);
			vStage_3 = v_stage_3 * (GF_flowrate / 1000);
			vacuum_pump_3 = (vStage_3 < 5000) ? 70000 * pow(vStage_3, 0.34) : 7400 * pow(vStage_3, 0.6);
			vacuum_pump = vacuum_pump_1 + vacuum_pump_2 + vacuum_pump_3;

			//(NCG) Condensers Cost Breakdown:	//Reference Equipment Cost
			cond_area_1 = (GF_flowrate / 1000) * (qRejectByStage_1 / (lmtd * 0.9 * U));
			cond_area_2 = (GF_flowrate / 1000) * (qRejectByStage_2 / (lmtd * 0.9 * U));
			cond_area_3 = (GF_flowrate / 1000) * (qRejectByStage_3 / (lmtd * 0.9 * U));
			condenser_ncg = 322 * (pow(cond_area_1, 0.72) + pow(cond_area_2, 0.72) + pow(cond_area_3, 0.72));

			//(NCG) Pumps Cost Calculation:	//Reference Equipment Cost
			ncg_pump_work = (ncg_condensate_pump * GF_flowrate / 1000) / 0.7457;
			ncg_water_pump = (cw_pump_work * GF_flowrate / 1000) / 0.7457;
			pump_ncg = 2.35 * 1185 * (pow(ncg_pump_work, 0.767) + pow(ncg_water_pump, 0.767));

			//(NCG) Ejector Cost Calculation: (Note: According to lib_geothermal.cpp, ncg removal type is alwasy JET)	//Reference Equipment Cost
			ejector_ncg = (76 * pow(pressure_ratio_1, (-0.45)) + 43 * pow(pressure_ratio_2, (-0.63))) * ncg_flow;

			//NCG total cost:	
			ncg_cost = vacuum_pump + condenser_ncg + pump_ncg + ejector_ncg;		//Reference Equipment Cost
			current_ncg_cost = ncg_cost * process_equip_ppi[20];

			//Pump Cost Calculation:
			condensate_pump = (GF_flowrate / 1000) * (condensate_pump_power * 1.34102);		//condensate_pump_power * 1.34102 is conversion from kW to hp
			condensate = 2.35 * 1185 * pow(condensate_pump, 0.767);
			cw_pump_power = (GF_flowrate / 1000) * ((((cwflow / 60)*(cw_pump_head)) / 33000) / 0.7);
			cooling_water = 2.35 * 1185 * pow(cw_pump_power, 0.767);
			pump_cost = condensate + cooling_water;		//Reference Equipment Cost
			current_pump_cost = pump_cost * pump_ppi[20];


			//H2S Removal System Cost Calculation:
			h2s_flow = h2s_level * GF_flowrate / 1000000;
			h2s_cost = 115000 * pow(h2s_flow, 0.58);		//Reference Equipment Cost
			current_h2s_cost = h2s_cost * process_equip_ppi[20];

			//Total Equipment Cost: 
			equip_cost_flash = tg_cost + cooling_tower_cost + condenser_cost_flash + flash_vessel_cost + ncg_cost + pump_cost + h2s_cost;
			current_cost_flash = current_tg_cost + current_tower_cost + current_condenser_cost + current_vessel_cost + current_ncg_cost + current_pump_cost + current_h2s_cost;
			escalation_ppi = current_cost_flash / equip_cost_flash;

			//Calculating Direct Construction Cost Multiplier:
			material_multiplier = 1 + ((8.65* pow(design_temp, -0.297)) - 1) * (process_equip_ppi[20] / escalation_ppi);
			labor_multiplier = ((42.65 * pow(design_temp, -0.923)) * 1.45) * construction_ppi[20] / escalation_ppi;
			construction_multiplier = (16.177*pow(design_temp, -0.827)) * process_equip_ppi[20] / escalation_ppi;
			direct_multiplier_2002 = material_multiplier + labor_multiplier + construction_multiplier;
			tax = (material_multiplier + construction_multiplier) * sales_tax;
			freight_flash = (material_multiplier + construction_multiplier) * freight;
			dc_cost_multiplier = direct_multiplier_2002 + tax + freight_flash;


			//Direct Plant Construction Cost: 
			direct_plant_cost = current_cost_flash * dc_cost_multiplier;
			baseline_cost = direct_plant_cost / unit_plant;		// ($/kW)

			assign("baseline_cost", var_data(static_cast<ssc_number_t>(baseline_cost)));

		}
	};

};

DEFINE_MODULE_ENTRY(geothermal_costs, "Geothermal monthly and hourly models using general power block code from TRNSYS Type 224 code by M.Wagner, and some GETEM model code.", 3);