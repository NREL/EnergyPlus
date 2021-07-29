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

static var_info _cm_vtab_windbos[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
																	      								                             
	// Inputs														      								                             
	{ SSC_INPUT,        SSC_NUMBER,      "machine_rating",                "Machine Rating",                                          "kW",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotor_diameter",                "Rotor Diameter",                                          "m",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hub_height",                    "Hub Height",                                              "m",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "number_of_turbines",            "Number of Turbines",                                      "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "interconnect_voltage",          "Interconnect Voltage",                                    "kV",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "distance_to_interconnect",      "Distance to Interconnect",                                "miles",  "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_terrain",                  "Site Terrain",                                            "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_layout",                "Turbine Layout",                                          "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "soil_condition",                "Soil Condition",                                          "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
		
	// Values that can be calculated in UI, or can be entered directly by the user
	{ SSC_INPUT,        SSC_NUMBER,      "construction_time",             "Construction Time",                                       "months", "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_building_size",              "O&M Building Size",                                       "ft^2",   "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "quantity_test_met_towers",      "Quantity of Temporary Meteorological Towers for Testing", "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "quantity_permanent_met_towers", "Quantity of Permanent Meteorological Towers for Testing", "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weather_delay_days",            "Wind / Weather delay days",                               "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "crane_breakdowns",              "Crane breakdowns",                                        "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "access_road_entrances",         "Access road entrances",                                   "",       "",                      "wind_bos",      "*",                       "",                              "" },

	// inputs from cost model outputs
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_capital_cost",          "Turbine Capital Cost",                                    "$/kW",   "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_top_mass",                "Tower Top Mass",                                          "Tonnes", "",                      "wind_bos",      "*",                       "",                              "" },

	// advanced user BOS inputs
	{ SSC_INPUT,        SSC_NUMBER,      "delivery_assist_required",      "Delivery Assist Required",                                "y/n",    "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pad_mount_transformer_required","Pad mount Transformer required",                          "y/n",    "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "new_switchyard_required",       "New Switchyard Required",                                 "y/n",    "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rock_trenching_required",       "Rock trenching required",                                 "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mv_thermal_backfill",           "MV thermal backfill",                                     "mi",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mv_overhead_collector",         "MV overhead collector",                                   "mi",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "performance_bond",              "Performance bond",                                        "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency",                   "Contingency",                                             "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "warranty_management",           "Warranty management",                                     "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_and_use_tax",             "Sales and Use Tax",                                       "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "overhead",                      "Overhead",                                                "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "profit_margin",                 "Profit Margin",                                           "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "development_fee",               "Development Fee",                                         "$M",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_transportation",        "Turbine Transportation",                                  "mi",     "",                      "wind_bos",      "*",                       "",                              "" },

//	{ SSC_OUTPUT,       SSC_ARRAY,       "e_net",                         "AC Generation",                                           "kWh",    "",                      "wind_bos",      "*",                       "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "project_total_budgeted_cost",   "Project Total Budgeted Cost",                             "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "transportation_cost",           "Transportation Cost",                                     "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "insurance_cost",                "Insurance Cost",                                          "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "engineering_cost",              "Engineering Cost",                                        "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "power_performance_cost",        "Power Performance Cost",                                  "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "site_compound_security_cost",   "Site Compound & Security Cost",                           "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "building_cost",                 "Building Cost",                                           "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "transmission_cost",             "Transmission Cost",                                       "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "markup_cost",                   "Markup Cost",                                             "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "development_cost",              "Development Cost",                                        "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "access_roads_cost",             "Access Roads Cost",                                       "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "foundation_cost",               "Foundation Cost",                                         "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "erection_cost",                 "Turbine Erection Cost",                                   "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "electrical_materials_cost",     "MV Electrical Materials Cost",                            "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "electrical_installation_cost",  "MV Electrical Installation Cost",                         "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "substation_cost",               "Substation Cost",                                         "$s",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "project_mgmt_cost",             "Project Management Cost",                                 "$s",     "",                      "wind_bos",      "*",                       "",                              "" },

var_info_invalid };

class cm_windbos : public compute_module
{
public:
	cm_windbos()
	{
		add_var_info(_cm_vtab_windbos);
	}

	//
	//  LandBOS.h
	//  LandBOS
	//
	//  Created by Andrew Ning on 3/12/14.
	//  Copyright (c) 2014 NREL. All rights reserved.
	//
	// **********************************************************************
	// Code from LandBOS.h and LandBOS.c starts here, ends at 'exec' function
	// **********************************************************************
	typedef enum { FLAT_TO_ROLLING, RIDGE_TOP, MOUNTAINOUS } SiteTerrain;
	typedef enum { SIMPLE, COMPLEX } TurbineLayout;
	typedef enum { STANDARD, BOUYANT } SoilCondition;

	int round_bos(double number)
	{
		return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
	}

	// wind farm size in MW
	double farmSize(double rating, int nTurb){
		return rating * nTurb / 1000.0;
	}

	// Construction Time (months)
	//int defaultConstructionTime(int nTurb){
	//	return round_bos(0.0001*nTurb*nTurb + 0.0963*nTurb + 2.7432);
	//}

	// Access road entrances
	//int defaultAccessRoadEntrances(int nTurb){
	//	return fmax(1, round_bos(nTurb / 20.0));
	//}

	// O&M Building Size (ft2)
	//double defaultBuildingSize(double farmSize){
	//	double buildingSize;
	//	// O&M Building Size (ft2)
	//	if (farmSize < 200){
	//		buildingSize = 3000;
	//	}
	//	else if (farmSize < 500){
	//		buildingSize = 5000;
	//	}
	//	else if (farmSize < 800){
	//		buildingSize = 7000;
	//	}
	//	else if (farmSize < 1000){
	//		buildingSize = 9000;
	//	}
	//	else{
	//		buildingSize = 12000;
	//	}
	//	return buildingSize;
	//}

	// Quantity of Temporary Meteorological Towers for Testing
	//double defaultTempMetTowers(double farmSize){
	//	return round_bos(farmSize / 75.0);
	//}

	// Quantity of Permanent Meteorological Towers for Testing
	//double defaultPermanentMetTowers(double farmSize){
	//	int permanent;
	//	if (farmSize < 100){
	//		permanent = 1;
	//	}
	//	else if (farmSize < 200){
	//		permanent = 2;
	//	}
	//	else{
	//		permanent = (int)(farmSize / 100.0);
	//	}
	//	return permanent;
	//}

	// Wind/Weather delay days
	//int defaultWeatherDelayDays(int nTurb){
	//	return round_bos(nTurb / 5.0);
	//}

	// Crane breakdowns
	//int defaultCraneBreakdowns(int nTurb){
	//	return round_bos(nTurb / 20.0);
	//}

	double transportationCost(double tcc, double rating, int nTurb,	double hubHt, double transportDist){
		double cost = tcc * rating * nTurb;
		if (rating < 2500 && hubHt < 100) {
			cost += 1349 * pow(transportDist, 0.746) * nTurb;
		}
		else {
			cost += 1867 * pow(transportDist, 0.726) * nTurb;
		}
		assign("transportation_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double engineeringCost(int nTurb, double farmSize)
	{
		double cost = 7188.5 * nTurb;
		cost += round_bos(3.4893*std::log(nTurb) - 7.3049) * 16800;
		double multiplier = 2.0;
		if (farmSize < 200) multiplier = 1.0;
		cost += multiplier * 161675;
		cost += 4000;
		assign("engineering_cost", var_data((ssc_number_t)cost));
		return cost;
	}

	double powerPerformanceCost(double hubHt, double permanent,	double temporary){
		double multiplier1 = 290000;
		double multiplier2 = 116800;
		if (hubHt < 90) {
			multiplier1 = 232600;
			multiplier2 = 92600;
		}
		double cost = 200000 + permanent*multiplier1 + temporary*multiplier2;
		assign("power_performance_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double accessRoadsCost(SiteTerrain terrain, TurbineLayout layout, int nTurb, double diameter, int constructionTime,	int accessRoadEntrances){
		double factor1 = 0.0;
		double factor2 = 0.0;

		if (layout == SIMPLE){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 49962.5;
				factor2 = 24.8;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 59822.0;
				factor2 = 26.8;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 66324.0;
				factor2 = 26.8;
			}

		}
		else if (layout == COMPLEX){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 62653.6;
				factor2 = 30.9;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 74213.3;
				factor2 = 33.0;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 82901.1;
				factor2 = 33.0;
			}
		}
		double cost = (nTurb*factor1 + nTurb*diameter*factor2
			+ constructionTime * 55500
			+ accessRoadEntrances * 3800)*1.05;

		assign("access_roads_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double siteCompoundCost(int accessRoadEntrances, int constructionTime, double farmSize)
	{
		double cost = 9825.0*accessRoadEntrances + 29850.0*constructionTime;
		double multiplier;
		if (farmSize > 100){
			multiplier = 10.0;
		}
		else if (farmSize > 30){
			multiplier = 5.0;
		}
		else{
			multiplier = 3.0;
		}
		cost += multiplier * 30000;
		if (farmSize > 30){
			cost += 90000;
		}
		cost += farmSize * 60 + 62400;
		assign("site_compound_security_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double buildingCost(double buildingSize){
		double cost = buildingSize * 125 + 176125;
		assign("building_cost", var_data((ssc_number_t)cost));
		return cost;
	}

	double foundationCost(double rating, double diameter, double topMass, double hubHt, SoilCondition soil, int nTurb)
	{
		double cost = rating*diameter*topMass / 1000.0
			+ 163421.5*pow(nTurb, -0.1458) + (hubHt - 80) * 500;

		if (soil == BOUYANT){
			cost += 20000;
		}
		cost *= nTurb;
		assign("foundation_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double erectionCost(double rating, double hubHt, int nTurb, int weatherDelayDays, int craneBreakdowns, int deliveryAssistRequired)
	{
		double cost = (37 * rating + 27000 * pow(nTurb, -0.42145) + (hubHt - 80) * 500)*nTurb;
		if (deliveryAssistRequired){
			cost += 60000 * nTurb;
		}
		cost += 20000 * weatherDelayDays + 35000 * craneBreakdowns + 181 * nTurb + 1834;
		assign("erection_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double electricalMaterialsCost(SiteTerrain terrain, TurbineLayout layout, double farmSize, double diameter, int nTurb, int padMountTransformer,	double thermalBackfill)
	{
		double factor1 = 0.0;
		double factor2 = 0.0;
		double factor3 = 0.0;

		if (layout == SIMPLE){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 66733.4;
				factor2 = 27088.4;
				factor3 = 545.4;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 67519.4;
				factor2 = 27874.4;
				factor3 = 590.8;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 68305.4;
				factor2 = 28660.4;
				factor3 = 590.8;
			}

		}
		else if (layout == COMPLEX){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 67519.4;
				factor2 = 27874.4;
				factor3 = 681.7;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 68305.4;
				factor2 = 28660.4;
				factor3 = 727.2;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 69484.4;
				factor2 = 29839.4;
				factor3 = 727.2;
			}
		}

		double cost;
		if (padMountTransformer){
			cost = nTurb*factor1;
		}
		else{
			cost = nTurb*factor2;
		}
		cost += floor(farmSize / 25.0) * 35375 + floor(farmSize / 100.0) * 50000
			+ diameter*nTurb*factor3 + thermalBackfill * 5 + 41945;

		assign("electrical_materials_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double electricalInstallationCost(SiteTerrain terrain, TurbineLayout layout, double farmSize, double diameter, int nTurb, double rockTrenchingLength, double overheadCollector)
	{
		double factor1 = 0.0;
		double factor2 = 0.0;
		double factor3 = 0.0;

		if (layout == SIMPLE){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 7059.3;
				factor2 = 352.4;
				factor3 = 297.0;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 7683.5;
				factor2 = 564.3;
				factor3 = 483.0;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 8305.0;
				factor2 = 682.6;
				factor3 = 579.0;
			}

		}
		else if (layout == COMPLEX){
			if (terrain == FLAT_TO_ROLLING){
				factor1 = 7683.5;
				factor2 = 564.9;
				factor3 = 446.0;
			}
			else if (terrain == RIDGE_TOP){
				factor1 = 8305.0;
				factor2 = 866.8;
				factor3 = 713.0;
			}
			else if (terrain == MOUNTAINOUS){
				factor1 = 9240.0;
				factor2 = 972.8;
				factor3 = 792.0;
			}
		}
		double cost = (int)(farmSize / 25.0) * 14985;
		if (farmSize > 200){
			cost += 300000;
		}
		else{
			cost += 155000;
		}
		cost += nTurb*(factor1 + diameter*(factor2 + factor3*rockTrenchingLength / 100.0))
			+ overheadCollector * 200000 + 10000;

		assign("electrical_installation_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double substationCost(double voltage, double farmSize)
	{
		double cost = 11652 * (voltage + farmSize) + 11795 * pow(farmSize, 0.3549) + 1526800;
		assign("substation_cost", var_data((ssc_number_t)cost));
		return cost;
	}

	double transmissionCost(double voltage, double distInter, int newSwitchyardRequired)
	{
		double cost = (1176 * voltage + 218257)*pow(distInter, 0.8937);
		if (newSwitchyardRequired){
			cost += 18115 * voltage + 165944;
		}
		assign("transmission_cost", var_data((ssc_number_t)cost));
		return cost;
	}

	double projectMgmtCost(int constructionTime)
	{
		double cost;
		if (constructionTime < 28){
			cost = (53.333*constructionTime*constructionTime - 3442 * constructionTime
				+ 209542)*(constructionTime + 2);
		}
		else{
			cost = (constructionTime + 2) * 155000;
		}
		assign("project_mgmt_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double developmentCost(double developmentFee)
	{
		double cost = developmentFee * 1000000;
		assign("development_cost", var_data((ssc_number_t)cost));
		return cost;
	}


	double insuranceMultiplierAndCost(double cost, double tcc, double farmSize, double foundationCost, int performanceBond)
	{
		double ins;
		double pb_rate = 0;
		if (performanceBond)
			pb_rate = 10.0;
			
		ins = cost / 1000 * (3.5 + 0.7 + 0.4 + 1.0 + pb_rate) //rates are per $1000
			+ (tcc * farmSize) * (0.7 + 0.4 + 1.0 + pb_rate) //tcc in $/kW times farmSize in MW is equal to per $1000
			+ 0.02 * foundationCost
			+ 20000;
		assign("insurance_cost", var_data((ssc_number_t)ins));
		return ins;
	}

	double markupMultiplierAndCost(double cost, double contingency, double warranty, double useTax, double overhead, double profitMargin)
	{
		double markup;
		markup = cost * (contingency + warranty + useTax + overhead + profitMargin) / 100.0; //convert from percentages to decimal
		assign("markup_cost", var_data((ssc_number_t)markup));
		return markup;
	}

	double totalCost(double rating, double diameter, double hubHt,
		int nTurb, double voltage, double distInter,
		SiteTerrain terrain, TurbineLayout layout, SoilCondition soil,
		double farmSize, double tcc, double topMass,
		int constructionTime, double buildingSize, double temporary,
		double permanent, int weatherDelayDays, int craneBreakdowns,
		int accessRoadEntrances,
		int deliveryAssistRequired, int padMountTransformer,
		int newSwitchyardRequired, double rockTrenchingLength,
		double thermalBackfill, double overheadCollector,
		int performanceBond, double contingency, double warranty,
		double useTax, double overhead, double profitMargin,
		double developmentFee, double transportDist)
	{
		//compute cost for all items EXCEPT turbine & transport- markup and insurance do not apply to turbine & transport costs
		double cost = 0.0;
		cost += engineeringCost(nTurb, farmSize);
		cost += powerPerformanceCost(hubHt, permanent, temporary);
		cost += siteCompoundCost(accessRoadEntrances, constructionTime, farmSize);
		cost += buildingCost(buildingSize);
		cost += transmissionCost(voltage, distInter, newSwitchyardRequired);
		cost += developmentCost(developmentFee);
		cost += accessRoadsCost(terrain, layout, nTurb, diameter, constructionTime, accessRoadEntrances);
		double foundCost = foundationCost(rating, diameter, topMass, hubHt, soil, nTurb);
		cost += foundCost;
		cost += erectionCost(rating, hubHt, nTurb, weatherDelayDays, craneBreakdowns, deliveryAssistRequired);
		cost += electricalMaterialsCost(terrain, layout, farmSize, diameter, nTurb, padMountTransformer, thermalBackfill);
		cost += electricalInstallationCost(terrain, layout, farmSize, diameter, nTurb, rockTrenchingLength, overheadCollector);
		cost += substationCost(voltage, farmSize);
		cost += projectMgmtCost(constructionTime);

		//now find insurance costs and markup costs using the current cost (before including turbine & transport)
		double ins = insuranceMultiplierAndCost(cost, tcc, farmSize, foundCost, performanceBond);
		double markup = markupMultiplierAndCost(cost, contingency, warranty, useTax, overhead, profitMargin);
		cost += ins + markup;

		//finally, add turbine & transport cost
		cost += transportationCost(tcc, rating, nTurb, hubHt, transportDist);

		//return the total cost
		return cost;
	}

	void exec( )
	{
		// get values
		double rating = (double) as_number("machine_rating");
		double diameter = (double)as_number("rotor_diameter");
		double hubHt = (double)as_number("hub_height");
		int nTurb = as_integer("number_of_turbines");
		double voltage = (double)as_number("interconnect_voltage");
		double distInter = (double)as_number("distance_to_interconnect");
		SiteTerrain terrain = (SiteTerrain) as_integer("site_terrain");
		TurbineLayout layout = (TurbineLayout)as_integer("turbine_layout");
		SoilCondition soil = (SoilCondition)as_integer("soil_condition");

		double farmSize = cm_windbos::farmSize(rating, nTurb);

		int constructionTime = (int)as_number("construction_time");
		double buildingSize = (double)as_number("om_building_size");
		double temporary = (double)as_number("quantity_test_met_towers");
		double permanent = (double)as_number("quantity_permanent_met_towers");
		int weatherDelayDays = (int)as_number("weather_delay_days");
		int craneBreakdowns = (int)as_number("crane_breakdowns");
		int accessRoadEntrances = (int)as_number("access_road_entrances");


		double tcc = (double)as_number("turbine_capital_cost");
		double topMass = (double)as_number("tower_top_mass");
		int deliveryAssistRequired = as_integer("delivery_assist_required");
		int padMountTransformer = as_integer("pad_mount_transformer_required");
		int newSwitchyardRequired = as_integer("new_switchyard_required");
		double rockTrenchingLength = (double)as_number("rock_trenching_required");
		double thermalBackfill = (double)as_number("mv_thermal_backfill");
		double overheadCollector = (double)as_number("mv_overhead_collector");
		double performanceBond = (double)as_number("performance_bond");
		double contingency = (double)as_number("contingency");
		double warranty = (double)as_number("warranty_management");
		double useTax = (double)as_number("sales_and_use_tax");
		double overhead = (double)as_number("overhead");
		double profitMargin = (double)as_number("profit_margin");
		double developmentFee = (double)as_number("development_fee");
		double transportDist = (double)as_number("turbine_transportation");


		// run model (execute functions)
		ssc_number_t output = (ssc_number_t)totalCost(rating, diameter, hubHt, nTurb, voltage, distInter, terrain, layout, soil,
			farmSize, tcc, topMass, constructionTime, buildingSize, temporary, permanent, weatherDelayDays, craneBreakdowns, accessRoadEntrances,
			deliveryAssistRequired, padMountTransformer, newSwitchyardRequired, rockTrenchingLength, thermalBackfill, overheadCollector,
			(int)performanceBond, contingency, warranty,	useTax, overhead, profitMargin,	developmentFee, transportDist);

		// assign outputs
		assign( "project_total_budgeted_cost", var_data(output) );
	}
};

DEFINE_MODULE_ENTRY( windbos, "Wind Balance of System cost model", 1 )
