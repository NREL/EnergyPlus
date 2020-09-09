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
#include <algorithm>
#include <sstream>



static var_info vtab_utility_rate5[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "en_electricity_rates",           "Optionally enable/disable electricity_rate",                   "years",  "",                      "Electricity Rates",             "",                         "INTEGER,MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "Lifetime",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "Lifetime", "*", "INTEGER,MIN=0,MAX=1", "" },

	{ SSC_INPUT, SSC_NUMBER, "TOU_demand_single_peak", "Use single monthly peak for TOU demand charge", "0/1", "0=use TOU peak,1=use flat peak", "Electricity Rates", "?=0", "INTEGER,MIN=0,MAX=1", "" },

	// First year or lifetime hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INPUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "System Output", "*", "", "" },

	// input from user as kW and output as kW
	{ SSC_INOUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Load", "", "", "" },
	//  output as kWh - same as load (kW) for hourly simulations
	{ SSC_OUTPUT, SSC_ARRAY, "bill_load", "Bill load (year 1)", "kWh", "", "Load", "*", "", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Lifetime", "*", "MIN=-99", "" },

	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "System Output", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "Load", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual electricity rate escalation",  "%/year", "",                      "Electricity Rates",             "?=0",                       "",                              "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_metering_option", "Metering options", "0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all", "Net metering monthly excess", "Electricity Rates", "?=0", "INTEGER,MIN=0,MAX=4", "" },


	{ SSC_INPUT, SSC_NUMBER, "ur_nm_yearend_sell_rate", "Year end sell rate", "$/kWh", "", "Electricity Rates", "?=0.0", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",            "$",      "",                      "Electricity Rates",             "?=0.0",                     "",                              "" },


// optional input that allows sell rates to be overridden with buy rates - defaults to not override
	{ SSC_INPUT, SSC_NUMBER, "ur_sell_eq_buy", "Set sell rate equal to buy rate", "0/1", "Optional override", "Electricity Rates", "?=0", "BOOLEAN", "" },



	// urdb minimums
	{ SSC_INPUT, SSC_NUMBER, "ur_monthly_min_charge", "Monthly minimum charge", "$", "", "Electricity Rates", "?=0.0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_annual_min_charge", "Annual minimum charge", "$", "", "Electricity Rates", "?=0.0", "", "" },


	// time step rates
	{ SSC_INPUT, SSC_NUMBER, "ur_en_ts_sell_rate", "Enable time step sell rates", "0/1", "", "Electricity Rates", "?=0", "BOOLEAN", "" },
	{ SSC_INPUT, SSC_ARRAY, "ur_ts_sell_rate", "Time step sell rates", "0/1", "", "Electricity Rates", "", "", "" },
	// add separately to UI
	{ SSC_INPUT, SSC_NUMBER, "ur_en_ts_buy_rate", "Enable time step buy rates", "0/1", "", "Electricity Rates", "?=0", "BOOLEAN", "" },
	{ SSC_INPUT, SSC_ARRAY, "ur_ts_buy_rate", "Time step buy rates", "0/1", "", "Electricity Rates", "", "", "" },




	// Energy Charge Inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekday", "Energy charge weekday schedule", "", "12x24", "Electricity Rates", "*", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekend", "Energy charge weekend schedule", "", "12x24", "Electricity Rates", "*", "", "" },

	// ur_ec_tou_mat has 6 columns period, tier, max usage, max usage units, buy rate, sell rate
	// replaces 12(P)*6(T)*(max usage+buy+sell) = 216 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_tou_mat", "Energy rates table", "", "", "Electricity Rates", "*", "", "" },


	// Demand Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",            "Enable demand charge",        "0/1",    "",                      "Electricity Rates",             "?=0",                       "BOOLEAN",                       "" },
	// TOU demand charge
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demand charge weekday schedule", "", "12x24", "Electricity Rates", "", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demand charge weekend schedule", "", "12x24", "Electricity Rates", "", "", "" },

	// ur_dc_tou_mat has 4 columns period, tier, peak demand (kW), demand charge
	// replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_tou_mat", "Demand rates (TOU) table", "", "", "Electricity Rates", "ur_dc_enable=1", "", "" },


	// flat demand charge
	// ur_dc_tou_flat has 4 columns month, tier, peak demand (kW), demand charge
	// replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_flat_mat", "Demand rates (flat) table", "", "", "Electricity Rates", "ur_dc_enable=1", "", "" },


	// outputs
//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_value",             "Energy value in each year",     "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_energy_value",             "Energy value in each year",     "$",    "",                      "Annual",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_electric_load",            "Electricity load total in each year",  "kWh",    "",                      "Annual",             "*",                         "",   "" },

	// use output from annualoutput not scaled output from here
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_net",               "Energy in each year",           "kW",   "",                      "",             "*",                         "",   "" },


		// outputs from Paul, Nate and Sean 9/9/13
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_with_system",      "Total revenue with system",         "$",    "",                      "",             "*",                         "",   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_without_system",   "Total revenue without system",      "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_with_system",    "Electricity bill with system",    "$/yr", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_without_system", "Electricity bill without system", "$/yr", "", "Annual", "*", "", "" },

	// year 1 values for metrics
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_with_system_year1",    "Electricity bill with system (year 1)",    "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_without_system_year1", "Electricity bill without system (year 1)", "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "savings_year1",                  "Electricity bill savings with system (year 1)",             "$/yr",    "", "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "year1_electric_load",            "Electricity load total (year 1)",                "kWh/yr",  "", "Financial Metrics", "*", "", "" },



	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_tofromgrid", "Electricity to/from grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_togrid", "Electricity to grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_fromgrid", "Electricity from grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_load",    "Electricity from system to load (year 1 hourly)",     "kWh", "",                      "",             "*",                         "",                   "" },

// lifetime load (optional for lifetime analysis)
	{ SSC_OUTPUT, SSC_ARRAY, "lifetime_load", "Lifetime electricity load", "kW", "", "Time Series", "system_use_lifetime_output=1", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_tofromgrid",         "Electricity to/from grid peak (year 1 hourly)", "kW",  "",                      "Time Series",             "*",                         "",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_system_to_load",         "Electricity peak from system to load (year 1 hourly)", "kW",  "",                      "Time Series",             "*",                         "",                   "" },


	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_with_system",     "Electricity sales/purchases with system (year 1 hourly)",    "$", "",          "Time Series",             "*",                         "",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_without_system",  "Electricity sales/purchases without system (year 1 hourly)", "$", "",          "Time Series",             "*",                         "",                   "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_with_system", "Energy charge with system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_without_system", "Energy charge without system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_with_system", "Demand charge with system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_without_system", "Demand charge without system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_tou_schedule", "TOU period for energy charges (year 1 hourly)", "", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_tou_schedule",       "TOU period for demand charges (year 1 hourly)", "", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_peak_per_period",    "Electricity peak from grid per TOU period (year 1 hourly)",        "kW", "", "Time Series", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_with_system", "Fixed monthly charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_without_system", "Fixed monthly charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_with_system", "Minimum charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_without_system", "Minimum charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_fixed_with_system", "Demand charge (flat) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_tou_with_system", "Demand charge (TOU) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_with_system", "Energy charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },




	//{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_flat_with_system", "Electricity charge (flat) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_with_system",       "Year 1 monthly energy rate with system",              "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_fixed_without_system",   "Demand charge (flat) without system", "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_tou_without_system",     "Demand charge (TOU) without system",   "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_without_system", "Energy charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_flat_without_system", "Energy charge (flat) without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_without_system",    "Year 1 monthly energy rate without system",           "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },


	// monthly outputs from Sean 7/29/13 "Net Metering Accounting.xlsx" updates from Paul and Sean 8/9/13 and 8/12/13
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_load", "Electricity load", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_w_system", "Demand peak with system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_wo_system", "Demand peak without system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },

// TODO - remove after testing
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_w_system", "Electricity use with system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_wo_system", "Electricity use without system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },

	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_system_generation",    "monthly system generation",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_to_grid",    "Electricity to/from grid",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_needed_from_grid",    "Electricity needed from grid",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_generation", "Excess generation cumulative kWh credit earned", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_dollars", "Excess generation cumulative $ credit earned", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases", "Electricity sales/purchases with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases_wo_sys", "Electricity sales/purchases without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_w_sys", "Electricity bill with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_wo_sys", "Electricity bill without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },


	// convert annual outputs from Arrays to Matrices years x months
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_w_sys_ym", "Electricity bill with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_wo_sys_ym", "Electricity bill without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_fixed_ym", "Fixed monthly charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_fixed_ym", "Fixed monthly charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_minimum_ym", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_minimum_ym", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_dc_fixed_ym", "Demand charge with system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_dc_tou_ym", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_dc_fixed_ym", "Demand charge without system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_dc_tou_ym", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_ym", "Energy charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
//	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_flat_ym", "Energy charge with system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_ym", "Energy charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
//	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_flat_ym", "Energy charge without system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },


	// annual sums
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys", "Electricity bill with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys", "Electricity bill without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed", "Fixed monthly charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed", "Fixed monthly charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed", "Demand charge with system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed", "Demand charge without system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec", "Energy charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec", "Energy charge without system", "$", "", "Charges by Month", "*", "", "" },

	// added for monthly bill balancing per 12/14/16 meeting
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_gross_ym", "Energy charge with system before credits", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "excess_dollars_applied_ym", "Excess generation $ credit applied", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "excess_dollars_earned_ym", "Excess generation $ credit earned", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "excess_kwhs_applied_ym", "Excess generation kWh credit applied", "kWh", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "excess_kwhs_earned_ym", "Excess generation kWh credit earned", "kWh", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	// added for monthly bill balancing per 12/14/16 meeting
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_gross_with_system", "Energy charge with system before credits", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_excess_dollars_applied", "Excess generation $ credit applied", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_excess_dollars_earned", "Excess generation $ credit earned", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_excess_kwhs_applied", "Excess generation kWh credit applied", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_excess_kwhs_earned", "Excess generation kWh credit earned", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },


// for Pablo at IRENA 8/8/15
// first year outputs only per email from Paul 8/9/15

// energy charge wo system
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jan_tp", "Energy charge without system Jan", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_feb_tp", "Energy charge without system Feb", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_mar_tp", "Energy charge without system Mar", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_apr_tp", "Energy charge without system Apr", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_may_tp", "Energy charge without system May", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jun_tp", "Energy charge without system Jun", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jul_tp", "Energy charge without system Jul", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_aug_tp", "Energy charge without system Aug", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_sep_tp", "Energy charge without system Sep", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_oct_tp", "Energy charge without system Oct", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_nov_tp", "Energy charge without system Nov", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_dec_tp", "Energy charge without system Dec", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy use wo system
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jan_tp", "Electricity usage without system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_feb_tp", "Electricity usage without system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_mar_tp", "Electricity usage without system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_apr_tp", "Electricity usage without system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_may_tp", "Electricity usage without system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jun_tp", "Electricity usage without system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jul_tp", "Electricity usage without system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_aug_tp", "Electricity usage without system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_sep_tp", "Electricity usage without system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_oct_tp", "Electricity usage without system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_nov_tp", "Electricity usage without system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_dec_tp", "Electricity usage without system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy charge w system
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jan_tp", "Energy charge with system Jan", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_feb_tp", "Energy charge with system Feb", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_mar_tp", "Energy charge with system Mar", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_apr_tp", "Energy charge with system Apr", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_may_tp", "Energy charge with system May", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jun_tp", "Energy charge with system Jun", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jul_tp", "Energy charge with system Jul", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_aug_tp", "Energy charge with system Aug", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_sep_tp", "Energy charge with system Sep", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_oct_tp", "Energy charge with system Oct", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_nov_tp", "Energy charge with system Nov", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_dec_tp", "Energy charge with system Dec", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy use w system
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jan_tp", "Electricity usage with system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_feb_tp", "Electricity usage with system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_mar_tp", "Electricity usage with system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_apr_tp", "Electricity usage with system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_may_tp", "Electricity usage with system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jun_tp", "Electricity usage with system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jul_tp", "Electricity usage with system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_aug_tp", "Electricity usage with system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_sep_tp", "Electricity usage with system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_oct_tp", "Electricity usage with system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_nov_tp", "Electricity usage with system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_dec_tp", "Electricity usage with system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy surplus w system
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jan_tp", "Electricity exports with system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_feb_tp", "Electricity exports with system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_mar_tp", "Electricity exports with system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_apr_tp", "Electricity exports with system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_may_tp", "Electricity exports with system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jun_tp", "Electricity exports with system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jul_tp", "Electricity exports with system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_aug_tp", "Electricity exports with system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_sep_tp", "Electricity exports with system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_oct_tp", "Electricity exports with system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_nov_tp", "Electricity exports with system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_dec_tp", "Electricity exports with system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

		// monthly peak demand per period
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_peak_w_sys", "Demand peak with system", "kW", "", "Charges by Month", "*", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_peak_wo_sys", "Demand peak without system", "kW", "", "Charges by Month", "*", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_charge_w_sys", "Demand peak charge with system", "$", "", "Charges by Month", "*", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_charge_wo_sys", "Demand peak charge without system", "$", "", "Charges by Month", "*", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },


	var_info_invalid };


class ur_month
{
public:
	// period numbers
	std::vector<int> ec_periods;
	std::vector<int> dc_periods;
	// track period numbers at 12a, 6a, 12p and 6p for rollover applications. Weekdays only considered
	std::vector<int> ec_rollover_periods;
	// monthly values
	// net energy use per month
	ssc_number_t energy_net;
	// hours per period per month
	int hours_per_month;
	// energy use period and tier
	util::matrix_t<ssc_number_t> ec_energy_use;
	// handle changing period tiers on monthly basis if kWh/kW
	std::vector<std::vector<int> >  ec_periods_tiers; // tier numbers
	// energy surplus - extra generated by system that is either sold or curtailed.
	util::matrix_t<ssc_number_t> ec_energy_surplus;
	// peak demand per period
	std::vector<ssc_number_t> dc_tou_peak;
	std::vector<int> dc_tou_peak_hour;
	ssc_number_t dc_flat_peak;
	int dc_flat_peak_hour;
	// energy tou charges
	util::matrix_t<ssc_number_t>  ec_tou_ub_init;
	util::matrix_t<ssc_number_t>  ec_tou_br_init;
	util::matrix_t<ssc_number_t>  ec_tou_sr_init;
	// may change based on units and year
	util::matrix_t<ssc_number_t>  ec_tou_ub;
	util::matrix_t<ssc_number_t>  ec_tou_br;
	util::matrix_t<ssc_number_t>  ec_tou_sr;
	util::matrix_t<int>  ec_tou_units;
	// calculated charges per period and tier
	util::matrix_t<ssc_number_t>  ec_charge;
	// demand tou charges
	util::matrix_t<ssc_number_t>  dc_tou_ub;
	util::matrix_t<ssc_number_t>  dc_tou_ch;
	// demand flat charges
	std::vector<ssc_number_t>  dc_flat_ub;
	std::vector<ssc_number_t>  dc_flat_ch;
	// calculated charges per period
	std::vector<double>  dc_tou_charge;
	ssc_number_t dc_flat_charge;


};

class cm_utilityrate5 : public compute_module
{
private:
	// schedule outputs
	std::vector<int> m_ec_tou_sched;
	std::vector<int> m_dc_tou_sched;
	std::vector<ur_month> m_month;
	std::vector<int> m_ec_periods; // period number

	// time step rates
	std::vector<ssc_number_t> m_ec_ts_sell_rate;
	std::vector<ssc_number_t> m_ec_ts_buy_rate;

	// track initial values - may change based on units
	std::vector<std::vector<int> >  m_ec_periods_tiers_init; // tier numbers
	std::vector<int> m_dc_tou_periods; // period number
	std::vector<std::vector<int> >  m_dc_tou_periods_tiers; // tier numbers
	std::vector<std::vector<int> >  m_dc_flat_tiers; // tier numbers for each month of flat demand charge
	size_t m_num_rec_yearly;

public:
	cm_utilityrate5()
	{
		add_var_info( vtab_utility_rate5 );
	}

	void exec( )
	{
		// if not assigned, we assume electricity rates are enabled
		if (is_assigned("en_electricity_rates")) {
			if (!as_boolean("en_electricity_rates")) {
				remove_var_info(vtab_utility_rate5);
				return;
			}
		}

		ssc_number_t *parr = 0;
		size_t count, i, j;

		size_t nyears = (size_t)as_integer("analysis_period");
		double inflation_rate = as_double("inflation_rate")*0.01;

		// compute annual system output degradation multipliers
		std::vector<ssc_number_t> sys_scale(nyears);

		// degradation
		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		// lifetime degradation applied in technology compute modules
		if (as_integer("system_use_lifetime_output") == 1)
		{
			for (i = 0; i<nyears; i++)
				sys_scale[i] = 1.0;
		}
		else
		{
			parr = as_array("degradation", &count);
			if (count == 1)
			{
				for (i = 0; i<nyears; i++)
					sys_scale[i] = (ssc_number_t)pow((double)(1 - parr[0] * 0.01), (double)i);
			}
			else
			{
				for (i = 0; i<nyears && i<count; i++)
					sys_scale[i] = (ssc_number_t)(1.0 - parr[i] * 0.01);
			}
		}



		// compute load (electric demand) annual escalation multipliers
		std::vector<ssc_number_t> load_scale(nyears);
		parr = as_array("load_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)pow( (double)(1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}

		// compute utility rate out-years escalation multipliers
		std::vector<ssc_number_t> rate_scale(nyears);
		parr = as_array("rate_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)pow( (double)(inflation_rate+1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}

		/* Update all e_sys and e_load values based on new inputs
		grid = gen -load where gen = sys + batt
		1. scale load and system value to hourly values as necessary
		2. use (kWh) e_sys[i] = sum((grid+load) * timestep ) over the hour for each hour i
		3. use (kW)  p_sys[i] = max( grid+load) over the hour for each hour i
		3. use (kWh) e_load[i] = sum(load * timestep ) over the hour for each hour i
		4. use (kW)  p_load[i] = max(load) over the hour for each hour i
		5. After above assignment, proceed as before with same outputs
		*/
		ssc_number_t *pload = NULL, *pgen;
		size_t nrec_load = 0, nrec_gen = 0, step_per_hour_gen=1, step_per_hour_load=1;
		bool bload=false;
		pgen = as_array("gen", &nrec_gen);
		// for lifetime analysis
		size_t nrec_gen_per_year = nrec_gen;
		if (as_integer("system_use_lifetime_output") == 1)
			nrec_gen_per_year = nrec_gen / nyears;
		step_per_hour_gen = nrec_gen_per_year / 8760;
		if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
			throw exec_error("utilityrate5", util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;
		m_num_rec_yearly = nrec_gen_per_year;

		if (is_assigned("load"))
		{ // hourly or sub hourly loads for single year
			bload = true;
			pload = as_array("load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("utilityrate5", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
			if ((nrec_load != m_num_rec_yearly) && (nrec_load != 8760))
				throw exec_error("utilityrate5", util::format("number of load records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nrec_load, (int)m_num_rec_yearly));
		}
//		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;

		// prepare timestep arrays for load and grid values
		std::vector<ssc_number_t>
			e_sys_cy(m_num_rec_yearly), p_sys_cy(m_num_rec_yearly),
			p_load(m_num_rec_yearly), // to handle no load, or num load != num gen
			e_grid_cy(m_num_rec_yearly), p_grid_cy(m_num_rec_yearly),
			e_load_cy(m_num_rec_yearly), p_load_cy(m_num_rec_yearly); // current year load (accounts for escal)



		// assign timestep values for utility rate calculations
		size_t idx = 0;
		ssc_number_t ts_load = 0;
		ssc_number_t year1_elec_load = 0;

		//load - fill out to number of generation records per year
		// handle cases
		// 1. if no load
		// 2. if load has 8760 and gen has more records
		// 3. if number records same for load and gen
		idx = 0;
		for (i = 0; i < 8760; i++)
		{
			for (size_t ii = 0; ii < step_per_hour_gen; ii++)
			{
				size_t ndx = i*step_per_hour_gen + ii;
				ts_load = (bload ? ((idx < nrec_load) ? pload[idx] : 0) : 0);
				year1_elec_load += ts_load;
				// sign correction for utility rate calculations
				p_load[ndx] = -ts_load;
				if (step_per_hour_gen == step_per_hour_load)
					idx++;
				else if (ii == (step_per_hour_gen - 1))
					idx++;
			}
		}

		assign("year1_electric_load", year1_elec_load* ts_hour_gen);


		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(m_num_rec_yearly), revenue_wo_sys(m_num_rec_yearly),
			payment(m_num_rec_yearly), income(m_num_rec_yearly),
			demand_charge_w_sys(m_num_rec_yearly), energy_charge_w_sys(m_num_rec_yearly), energy_charge_gross_w_sys(m_num_rec_yearly),
			demand_charge_wo_sys(m_num_rec_yearly), energy_charge_wo_sys(m_num_rec_yearly),
			ec_tou_sched(m_num_rec_yearly), dc_tou_sched(m_num_rec_yearly), load(m_num_rec_yearly), dc_hourly_peak(m_num_rec_yearly),
			e_tofromgrid(m_num_rec_yearly), p_tofromgrid(m_num_rec_yearly), salespurchases(m_num_rec_yearly);
		std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
			monthly_fixed_charges(12), monthly_minimum_charges(12),
			monthly_dc_fixed(12), monthly_dc_tou(12),
			monthly_ec_charges(12),
			monthly_ec_charges_gross(12),
			monthly_excess_dollars_applied(12),
			monthly_excess_dollars_earned(12),
			monthly_excess_kwhs_applied(12),
			monthly_excess_kwhs_earned(12),
			monthly_ec_rates(12),
			monthly_salespurchases(12),
			monthly_load(12), monthly_system_generation(12), monthly_elec_to_grid(12),
			monthly_elec_needed_from_grid(12),
			monthly_cumulative_excess_energy(12), monthly_cumulative_excess_dollars(12), monthly_bill(12), monthly_peak(12), monthly_test(12);

		/* allocate outputs */
		ssc_number_t *annual_net_revenue = allocate("annual_energy_value", nyears+1);
		ssc_number_t *annual_electric_load = allocate("annual_electric_load", nyears+1);
		ssc_number_t *energy_net = allocate("scaled_annual_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("revenue_without_system", nyears+1);
		ssc_number_t *annual_elec_cost_w_sys = allocate("elec_cost_with_system", nyears+1);
		ssc_number_t *annual_elec_cost_wo_sys = allocate("elec_cost_without_system", nyears+1);

		// matrices
		ssc_number_t *utility_bill_w_sys_ym = allocate("utility_bill_w_sys_ym", nyears + 1, 12);
		ssc_number_t *utility_bill_wo_sys_ym = allocate("utility_bill_wo_sys_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_dc_fixed_ym = allocate("charge_w_sys_dc_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_dc_tou_ym = allocate("charge_w_sys_dc_tou_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_ec_ym = allocate("charge_w_sys_ec_ym", nyears + 1, 12);

		ssc_number_t *ch_w_sys_ec_gross_ym = allocate("charge_w_sys_ec_gross_ym", nyears + 1, 12);
		ssc_number_t *excess_dollars_applied_ym = allocate("excess_dollars_applied_ym", nyears + 1, 12);
		ssc_number_t *excess_dollars_earned_ym = allocate("excess_dollars_earned_ym", nyears + 1, 12);
		ssc_number_t *excess_kwhs_applied_ym = allocate("excess_kwhs_applied_ym", nyears + 1, 12);
		ssc_number_t *excess_kwhs_earned_ym = allocate("excess_kwhs_earned_ym", nyears + 1, 12);


		ssc_number_t *ch_wo_sys_dc_fixed_ym = allocate("charge_wo_sys_dc_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_dc_tou_ym = allocate("charge_wo_sys_dc_tou_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_ec_ym = allocate("charge_wo_sys_ec_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_fixed_ym = allocate("charge_w_sys_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_fixed_ym = allocate("charge_wo_sys_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_minimum_ym = allocate("charge_w_sys_minimum_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_minimum_ym = allocate("charge_wo_sys_minimum_ym", nyears + 1, 12);


		// annual sums
		ssc_number_t *utility_bill_w_sys = allocate("utility_bill_w_sys", nyears + 1);
		ssc_number_t *utility_bill_wo_sys = allocate("utility_bill_wo_sys", nyears + 1);
		ssc_number_t *ch_w_sys_dc_fixed = allocate("charge_w_sys_dc_fixed", nyears + 1);
		ssc_number_t *ch_w_sys_dc_tou = allocate("charge_w_sys_dc_tou", nyears + 1);
		ssc_number_t *ch_w_sys_ec = allocate("charge_w_sys_ec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed = allocate("charge_wo_sys_dc_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou = allocate("charge_wo_sys_dc_tou", nyears + 1);
		ssc_number_t *ch_wo_sys_ec = allocate("charge_wo_sys_ec", nyears + 1);
		ssc_number_t *ch_w_sys_fixed = allocate("charge_w_sys_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed = allocate("charge_wo_sys_fixed", nyears + 1);
		ssc_number_t *ch_w_sys_minimum = allocate("charge_w_sys_minimum", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum = allocate("charge_wo_sys_minimum", nyears + 1);


		// Enphase outputs requested - see emails 2/12/16- first year system to grid and from grid
		ssc_number_t *year1_hourly_e_togrid = allocate("year1_hourly_e_togrid", m_num_rec_yearly);
		ssc_number_t *year1_hourly_e_fromgrid = allocate("year1_hourly_e_fromgrid", m_num_rec_yearly);



		// IRENA outputs array of tier values
		// reverse to tiers columns and periods are rows based on IRENA desired output.
		// tiers and periods determined by input matrices

		setup();


		// note that ec_charge and not ec_energy_use have the correct dimensions after setup
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jan_tp = allocate_matrix("charge_wo_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_feb_tp = allocate_matrix("charge_wo_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_mar_tp = allocate_matrix("charge_wo_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_apr_tp = allocate_matrix("charge_wo_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_may_tp = allocate_matrix("charge_wo_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jun_tp = allocate_matrix("charge_wo_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jul_tp = allocate_matrix("charge_wo_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_aug_tp = allocate_matrix("charge_wo_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_sep_tp = allocate_matrix("charge_wo_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_oct_tp = allocate_matrix("charge_wo_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_nov_tp = allocate_matrix("charge_wo_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_dec_tp = allocate_matrix("charge_wo_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);


		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jan_tp = allocate_matrix("energy_wo_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_feb_tp = allocate_matrix("energy_wo_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_mar_tp = allocate_matrix("energy_wo_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_apr_tp = allocate_matrix("energy_wo_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_may_tp = allocate_matrix("energy_wo_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jun_tp = allocate_matrix("energy_wo_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jul_tp = allocate_matrix("energy_wo_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_aug_tp = allocate_matrix("energy_wo_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_sep_tp = allocate_matrix("energy_wo_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_oct_tp = allocate_matrix("energy_wo_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_nov_tp = allocate_matrix("energy_wo_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_dec_tp = allocate_matrix("energy_wo_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);

// no longer output but still updated internally in ur_update_ec_monthly
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jan_tp = allocate_matrix("surplus_wo_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_feb_tp = allocate_matrix("surplus_wo_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_mar_tp = allocate_matrix("surplus_wo_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_apr_tp = allocate_matrix("surplus_wo_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_may_tp = allocate_matrix("surplus_wo_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jun_tp = allocate_matrix("surplus_wo_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jul_tp = allocate_matrix("surplus_wo_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_aug_tp = allocate_matrix("surplus_wo_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_sep_tp = allocate_matrix("surplus_wo_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_oct_tp = allocate_matrix("surplus_wo_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_nov_tp = allocate_matrix("surplus_wo_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_dec_tp = allocate_matrix("surplus_wo_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);




		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jan_tp = allocate_matrix("charge_w_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_feb_tp = allocate_matrix("charge_w_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_mar_tp = allocate_matrix("charge_w_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_apr_tp = allocate_matrix("charge_w_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_may_tp = allocate_matrix("charge_w_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jun_tp = allocate_matrix("charge_w_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jul_tp = allocate_matrix("charge_w_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_aug_tp = allocate_matrix("charge_w_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_sep_tp = allocate_matrix("charge_w_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_oct_tp = allocate_matrix("charge_w_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_nov_tp = allocate_matrix("charge_w_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_dec_tp = allocate_matrix("charge_w_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);


		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jan_tp = allocate_matrix("energy_w_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_feb_tp = allocate_matrix("energy_w_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_mar_tp = allocate_matrix("energy_w_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_apr_tp = allocate_matrix("energy_w_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_may_tp = allocate_matrix("energy_w_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jun_tp = allocate_matrix("energy_w_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jul_tp = allocate_matrix("energy_w_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_aug_tp = allocate_matrix("energy_w_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_sep_tp = allocate_matrix("energy_w_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_oct_tp = allocate_matrix("energy_w_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_nov_tp = allocate_matrix("energy_w_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_dec_tp = allocate_matrix("energy_w_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);


		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jan_tp = allocate_matrix("surplus_w_sys_ec_jan_tp", m_month[0].ec_charge.nrows() + 2, m_month[0].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_feb_tp = allocate_matrix("surplus_w_sys_ec_feb_tp", m_month[1].ec_charge.nrows() + 2, m_month[1].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_mar_tp = allocate_matrix("surplus_w_sys_ec_mar_tp", m_month[2].ec_charge.nrows() + 2, m_month[2].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_apr_tp = allocate_matrix("surplus_w_sys_ec_apr_tp", m_month[3].ec_charge.nrows() + 2, m_month[3].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_may_tp = allocate_matrix("surplus_w_sys_ec_may_tp", m_month[4].ec_charge.nrows() + 2, m_month[4].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jun_tp = allocate_matrix("surplus_w_sys_ec_jun_tp", m_month[5].ec_charge.nrows() + 2, m_month[5].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jul_tp = allocate_matrix("surplus_w_sys_ec_jul_tp", m_month[6].ec_charge.nrows() + 2, m_month[6].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_aug_tp = allocate_matrix("surplus_w_sys_ec_aug_tp", m_month[7].ec_charge.nrows() + 2, m_month[7].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_sep_tp = allocate_matrix("surplus_w_sys_ec_sep_tp", m_month[8].ec_charge.nrows() + 2, m_month[8].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_oct_tp = allocate_matrix("surplus_w_sys_ec_oct_tp", m_month[9].ec_charge.nrows() + 2, m_month[9].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_nov_tp = allocate_matrix("surplus_w_sys_ec_nov_tp", m_month[10].ec_charge.nrows() + 2, m_month[10].ec_charge.ncols() + 2);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_dec_tp = allocate_matrix("surplus_w_sys_ec_dec_tp", m_month[11].ec_charge.nrows() + 2, m_month[11].ec_charge.ncols() + 2);


		util::matrix_t<ssc_number_t> &monthly_tou_demand_peak_w_sys = allocate_matrix("monthly_tou_demand_peak_w_sys", 13, m_dc_tou_periods.size());
		util::matrix_t<ssc_number_t> &monthly_tou_demand_peak_wo_sys = allocate_matrix("monthly_tou_demand_peak_wo_sys", 13, m_dc_tou_periods.size());
		util::matrix_t<ssc_number_t> &monthly_tou_demand_charge_w_sys = allocate_matrix("monthly_tou_demand_charge_w_sys", 13, m_dc_tou_periods.size());
		util::matrix_t<ssc_number_t> &monthly_tou_demand_charge_wo_sys = allocate_matrix("monthly_tou_demand_charge_wo_sys", 13, m_dc_tou_periods.size());


		// lifetime hourly load
		ssc_number_t *lifetime_load = allocate("lifetime_load", nrec_gen);

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Single meter with monthly rollover credits in $ (Net Billing $)
		4=Two meters with all generation sold and all load purchaseded
		*/
		int metering_option = as_integer("ur_metering_option");
		bool two_meter = (metering_option == 4 );
		bool timestep_reconciliation = (metering_option == 2 || metering_option == 3 || metering_option == 4);

		bool time_series_rates = as_boolean("ur_en_ts_sell_rate") || as_boolean("ur_en_ts_buy_rate");
		if (time_series_rates && !timestep_reconciliation)
		{
			throw exec_error("utilityrate5", "Time series rates are not compatible with net metering. Please disable time series rates or change to net billing / buy all - sell all");
		}

		idx = 0;
		for (i=0;i<nyears;i++)
		{
			for (j = 0; j<m_num_rec_yearly; j++)
			{
				/* for future implementation for lifetime loads
				// update e_load and p_load per year if lifetime output
				// lifetime load values? sell values
				if ((as_integer("system_use_lifetime_output") == 1) && (idx < nrec_load))
				{
					e_load[j] = p_load[j] = 0.0;
					for (size_t ii = 0; (ii < step_per_hour_load) && (idx < nrec_load); ii++)
					{
						ts_load = (bload ? pload[idx] : 0);
						e_load[i] += ts_load * ts_hour_load;
						p_load[i] = ((ts_load > p_load[i]) ? ts_load : p_load[i]);
						idx++;
					}
					lifetime_hourly_load[i*8760 + j] = e_load[i];
					// sign correction for utility rate calculations
					e_load[i] = -e_load[i];
					p_load[i] = -p_load[i];
				}
				*/
				// apply load escalation appropriate for current year
//				e_load_cy[j] = e_load[j] * load_scale[i];
//				p_load_cy[j] = p_load[j] * load_scale[i];
				e_load_cy[j] = p_load[j] * load_scale[i] * ts_hour_gen;
				p_load_cy[j] = p_load[j] * load_scale[i];


				// update e_sys per year if lifetime output
				if ((as_integer("system_use_lifetime_output") == 1) && ( idx < nrec_gen ))
				{
//					e_sys[j] = p_sys[j] = 0.0;
//					ts_power = (idx < nrec_gen) ? pgen[idx] : 0;
//					e_sys[j] = ts_power * ts_hour_gen;
//					p_sys[j] = ((ts_power > p_sys[j]) ? ts_power : p_sys[j]);
					e_sys_cy[j] = pgen[idx] * ts_hour_gen;
					p_sys_cy[j] = pgen[idx];
					// until lifetime load fully implemented
					// report lifetime load in kW and not kWh
					lifetime_load[idx] = -e_load_cy[j] / ts_hour_gen;
					idx++;
				}
				else
				{
					e_sys_cy[j] = pgen[j] * ts_hour_gen;
					p_sys_cy[j] = pgen[j];
				}
				e_sys_cy[j] *= sys_scale[i];
				p_sys_cy[j] *= sys_scale[i];
				// calculate e_grid value (e_sys + e_load)
//				e_sys_cy[j] = e_sys[j] * sys_scale[i];
//				p_sys_cy[j] = p_sys[j] * sys_scale[i];
				// note: load is assumed to have negative sign
				e_grid_cy[j] = e_sys_cy[j] + e_load_cy[j];
				p_grid_cy[j] = p_sys_cy[j] + p_load_cy[j];
			}


			// now calculate revenue without solar system (using load only)
			if (timestep_reconciliation)
			{
				ur_calc_timestep(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0], &demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0],
					&monthly_ec_charges_gross[0],
					&monthly_excess_dollars_earned[0],
					&monthly_excess_dollars_applied[0],
					&monthly_excess_kwhs_earned[0],
					&monthly_excess_kwhs_applied[0],
					&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i]);
			}
			else
			{
				ur_calc(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0], &demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0],
					&monthly_ec_charges_gross[0],
					&monthly_excess_dollars_earned[0],
					&monthly_excess_dollars_applied[0],
					&monthly_excess_kwhs_earned[0],
					&monthly_excess_kwhs_applied[0],
					&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i], i + 1);
			}

			for (j = 0; j < 12; j++)
			{
				utility_bill_wo_sys_ym[(i + 1) * 12 + j] = monthly_bill[j];
				ch_wo_sys_dc_fixed_ym[(i + 1) * 12 + j] = monthly_dc_fixed[j];
				ch_wo_sys_dc_tou_ym[(i + 1) * 12 + j] = monthly_dc_tou[j];
				ch_wo_sys_ec_ym[(i + 1) * 12 + j] = monthly_ec_charges[j];
				//ch_wo_sys_ec_flat_ym[(i + 1) * 12 + j] = monthly_ec_flat_charges[j];
				ch_wo_sys_fixed_ym[(i + 1) * 12 + j] = monthly_fixed_charges[j];
				ch_wo_sys_minimum_ym[(i + 1) * 12 + j] = monthly_minimum_charges[j];

				utility_bill_wo_sys[i + 1] += monthly_bill[j];
				ch_wo_sys_dc_fixed[i + 1] += monthly_dc_fixed[j];
				ch_wo_sys_dc_tou[i + 1] += monthly_dc_tou[j];
				ch_wo_sys_ec[i + 1] += monthly_ec_charges[j];
				//ch_wo_sys_ec_flat[i + 1] += monthly_ec_flat_charges[j];
				ch_wo_sys_fixed[i + 1] += monthly_fixed_charges[j];
				ch_wo_sys_minimum[i + 1] += monthly_minimum_charges[j];
			}


			if (i == 0)
			{
				ur_update_ec_monthly(0, charge_wo_sys_ec_jan_tp, energy_wo_sys_ec_jan_tp, surplus_wo_sys_ec_jan_tp);
				ur_update_ec_monthly(1, charge_wo_sys_ec_feb_tp, energy_wo_sys_ec_feb_tp, surplus_wo_sys_ec_feb_tp);
				ur_update_ec_monthly(2, charge_wo_sys_ec_mar_tp, energy_wo_sys_ec_mar_tp, surplus_wo_sys_ec_mar_tp);
				ur_update_ec_monthly(3, charge_wo_sys_ec_apr_tp, energy_wo_sys_ec_apr_tp, surplus_wo_sys_ec_apr_tp);
				ur_update_ec_monthly(4, charge_wo_sys_ec_may_tp, energy_wo_sys_ec_may_tp, surplus_wo_sys_ec_may_tp);
				ur_update_ec_monthly(5, charge_wo_sys_ec_jun_tp, energy_wo_sys_ec_jun_tp, surplus_wo_sys_ec_jun_tp);
				ur_update_ec_monthly(6, charge_wo_sys_ec_jul_tp, energy_wo_sys_ec_jul_tp, surplus_wo_sys_ec_jul_tp);
				ur_update_ec_monthly(7, charge_wo_sys_ec_aug_tp, energy_wo_sys_ec_aug_tp, surplus_wo_sys_ec_aug_tp);
				ur_update_ec_monthly(8, charge_wo_sys_ec_sep_tp, energy_wo_sys_ec_sep_tp, surplus_wo_sys_ec_sep_tp);
				ur_update_ec_monthly(9, charge_wo_sys_ec_oct_tp, energy_wo_sys_ec_oct_tp, surplus_wo_sys_ec_oct_tp);
				ur_update_ec_monthly(10, charge_wo_sys_ec_nov_tp, energy_wo_sys_ec_nov_tp, surplus_wo_sys_ec_nov_tp);
				ur_update_ec_monthly(11, charge_wo_sys_ec_dec_tp, energy_wo_sys_ec_dec_tp, surplus_wo_sys_ec_dec_tp);


				// demand peak without system
				for (int irow = 0; irow <= 12; irow++)
				{
					for (int icol = 0; icol < (int)m_dc_tou_periods.size(); icol++)
					{
						if (irow == 0)
						{
							monthly_tou_demand_peak_wo_sys.at(0, icol) = (float)m_dc_tou_periods[icol];
							monthly_tou_demand_charge_wo_sys.at(0, icol) = (float)m_dc_tou_periods[icol];
						}
						else
						{
							int ndx = -1;
							int period = m_dc_tou_periods[icol];
							std::vector<int>::iterator result = std::find(m_month[irow - 1].dc_periods.begin(), m_month[irow - 1].dc_periods.end(), period);
							if (result == m_month[irow - 1].dc_periods.end())
							{
								monthly_tou_demand_peak_wo_sys.at(irow, icol) = 0;
								monthly_tou_demand_charge_wo_sys.at(irow, icol) = 0;
							}
							else
							{
								ndx = (int)(result - m_month[irow - 1].dc_periods.begin());
								if (ndx > -1 && ndx < (int)m_month[irow - 1].dc_tou_peak.size())
									monthly_tou_demand_peak_wo_sys.at(irow, icol) = m_month[irow - 1].dc_tou_peak[ndx];
								if (ndx > -1 && ndx < (int)m_month[irow - 1].dc_tou_charge.size())
									monthly_tou_demand_charge_wo_sys.at(irow, icol) = (float)m_month[irow - 1].dc_tou_charge[ndx];
							}
						}
					}
				}



				assign("year1_hourly_dc_without_system", var_data(&demand_charge_wo_sys[0], m_num_rec_yearly));
				assign("year1_hourly_ec_without_system", var_data(&energy_charge_wo_sys[0], m_num_rec_yearly));

				assign("year1_monthly_dc_fixed_without_system", var_data(&monthly_dc_fixed[0], 12));
				assign( "year1_monthly_dc_tou_without_system", var_data(&monthly_dc_tou[0], 12) );
				assign("year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12));

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					salespurchases[ii] = revenue_wo_sys[ii];
				}

				int c = 0;

				for (int m=0;m<12;m++)
				{
					monthly_salespurchases[m] = 0;
					for (size_t d=0;d<util::nday[m];d++)
					{
						for(int h=0;h<24;h++)
						{
							for (size_t s = 0; s < step_per_hour_gen; s++)
							{
								monthly_salespurchases[m] += salespurchases[c];
								c++;
							}
						}
					}
				}
				assign("year1_hourly_salespurchases_without_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_utility_bill_wo_sys", var_data(&monthly_bill[0], 12));
				assign("year1_monthly_fixed_without_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_without_system", var_data(&monthly_minimum_charges[0], 12));

				// peak demand and testing energy use
				for (int ii = 0; ii < 12; ii++)
				{
					monthly_peak[ii] = m_month[ii].dc_flat_peak;
					monthly_test[ii] = -m_month[ii].energy_net;
				}
				assign("year1_monthly_peak_wo_system", var_data(&monthly_peak[0], 12));
				assign("year1_monthly_use_wo_system", var_data(&monthly_test[0], 12));
			}

// with system

			if (timestep_reconciliation)
			{
				if (two_meter)
				{
					ur_calc_timestep(&e_sys_cy[0], &p_sys_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_dollars_applied[0],
						&monthly_excess_kwhs_earned[0],
						&monthly_excess_kwhs_applied[0],
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i], false, false, true);
				}
				else
				{
					ur_calc_timestep(&e_grid_cy[0], &p_grid_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_dollars_applied[0],
						&monthly_excess_kwhs_earned[0],
						&monthly_excess_kwhs_applied[0],
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i]);
				}
			}
			else // monthly reconciliation per 2015.6.30 release
			{
				if (two_meter)
				{
					// calculate revenue with solar system (using system energy & maxpower)
					ur_calc(&e_sys_cy[0], &p_sys_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_dollars_applied[0],
						&monthly_excess_kwhs_earned[0],
						&monthly_excess_kwhs_applied[0],
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i], i + 1, false, false, true);
				}
				else
				{
					// calculate revenue with solar system (using net grid energy & maxpower)
					ur_calc(&e_grid_cy[0], &p_grid_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_dollars_applied[0],
						&monthly_excess_kwhs_earned[0],
						&monthly_excess_kwhs_applied[0],
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i], i + 1);
				}
			}
			if (two_meter)
			{
				// TODO - remove annual_revenue and just use annual bill
				// Two meters - adjust output accordingly
				for (j = 0; j < m_num_rec_yearly; j++)
				{
					revenue_w_sys[j] += revenue_wo_sys[j]; // watch sign
					annual_revenue_w_sys[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
					energy_charge_w_sys[j] += energy_charge_wo_sys[j];
					energy_charge_gross_w_sys[j] += energy_charge_wo_sys[j];
					demand_charge_w_sys[j] += demand_charge_wo_sys[j];
				}
				// adjust monthly outputs as sum of both meters = system meter + load meter

				for (j = 0; j < 12; j++)
				{
					monthly_dc_fixed[j] += ch_wo_sys_dc_fixed_ym[(i + 1) * 12 + j];
					monthly_dc_tou[j] += ch_wo_sys_dc_tou_ym[(i + 1) * 12 + j];
					monthly_ec_charges[j] += ch_wo_sys_ec_ym[(i + 1) * 12 + j];
					monthly_ec_charges_gross[j] += ch_wo_sys_ec_ym[(i + 1) * 12 + j];
					monthly_fixed_charges[j] += ch_wo_sys_fixed_ym[(i + 1) * 12 + j];
					monthly_minimum_charges[j] += ch_wo_sys_minimum_ym[(i + 1) * 12 + j];
					monthly_bill[j] += utility_bill_wo_sys_ym[(i + 1) * 12 + j];
				}

				if (i == 0)
				{

					// for each month add the wo system charge and energy
					// not that first row contains tier num and first column contains period numbers in the charge_wo_sys_ec and energy_wo_sys_ec matrices
					for (int m = 0; m < (int)m_month.size(); m++)
					{
						for (int ir = 0; ir < (int)m_month[m].ec_charge.nrows(); ir++)
						{
							for (int ic = 0; ic < (int)m_month[m].ec_charge.ncols(); ic++)
							{
								ssc_number_t charge_adj = 0;
								ssc_number_t energy_adj = 0;
								switch (m)
								{
								case 0:
									charge_adj = charge_wo_sys_ec_jan_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jan_tp.at(ir + 1, ic + 1);
									break;
								case 1:
									charge_adj = charge_wo_sys_ec_feb_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_feb_tp.at(ir + 1, ic + 1);
									break;
								case 2:
									charge_adj = charge_wo_sys_ec_mar_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_mar_tp.at(ir + 1, ic + 1);
									break;
								case 3:
									charge_adj = charge_wo_sys_ec_apr_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_apr_tp.at(ir + 1, ic + 1);
									break;
								case 4:
									charge_adj = charge_wo_sys_ec_may_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_may_tp.at(ir + 1, ic + 1);
									break;
								case 5:
									charge_adj = charge_wo_sys_ec_jun_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jun_tp.at(ir + 1, ic + 1);
									break;
								case 6:
									charge_adj = charge_wo_sys_ec_jul_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jul_tp.at(ir + 1, ic + 1);
									break;
								case 7:
									charge_adj = charge_wo_sys_ec_aug_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_aug_tp.at(ir + 1, ic + 1);
									break;
								case 8:
									charge_adj = charge_wo_sys_ec_sep_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_sep_tp.at(ir + 1, ic + 1);
									break;
								case 9:
									charge_adj = charge_wo_sys_ec_oct_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_oct_tp.at(ir + 1, ic + 1);
									break;
								case 10:
									charge_adj = charge_wo_sys_ec_nov_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_nov_tp.at(ir + 1, ic + 1);
									break;
								case 11:
									charge_adj = charge_wo_sys_ec_dec_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_dec_tp.at(ir + 1, ic + 1);
									break;
								}
								m_month[m].ec_charge.at(ir, ic) += charge_adj;
								m_month[m].ec_energy_use.at(ir, ic) += energy_adj;
							}
						}
					}
				} // i==0 (first year matrix outputs)
			} // two meter metering option

			if (i == 0)
			{
				ur_update_ec_monthly(0, charge_w_sys_ec_jan_tp, energy_w_sys_ec_jan_tp, surplus_w_sys_ec_jan_tp);
				ur_update_ec_monthly(1, charge_w_sys_ec_feb_tp, energy_w_sys_ec_feb_tp, surplus_w_sys_ec_feb_tp);
				ur_update_ec_monthly(2, charge_w_sys_ec_mar_tp, energy_w_sys_ec_mar_tp, surplus_w_sys_ec_mar_tp);
				ur_update_ec_monthly(3, charge_w_sys_ec_apr_tp, energy_w_sys_ec_apr_tp, surplus_w_sys_ec_apr_tp);
				ur_update_ec_monthly(4, charge_w_sys_ec_may_tp, energy_w_sys_ec_may_tp, surplus_w_sys_ec_may_tp);
				ur_update_ec_monthly(5, charge_w_sys_ec_jun_tp, energy_w_sys_ec_jun_tp, surplus_w_sys_ec_jun_tp);
				ur_update_ec_monthly(6, charge_w_sys_ec_jul_tp, energy_w_sys_ec_jul_tp, surplus_w_sys_ec_jul_tp);
				ur_update_ec_monthly(7, charge_w_sys_ec_aug_tp, energy_w_sys_ec_aug_tp, surplus_w_sys_ec_aug_tp);
				ur_update_ec_monthly(8, charge_w_sys_ec_sep_tp, energy_w_sys_ec_sep_tp, surplus_w_sys_ec_sep_tp);
				ur_update_ec_monthly(9, charge_w_sys_ec_oct_tp, energy_w_sys_ec_oct_tp, surplus_w_sys_ec_oct_tp);
				ur_update_ec_monthly(10, charge_w_sys_ec_nov_tp, energy_w_sys_ec_nov_tp, surplus_w_sys_ec_nov_tp);
				ur_update_ec_monthly(11, charge_w_sys_ec_dec_tp, energy_w_sys_ec_dec_tp, surplus_w_sys_ec_dec_tp);

				// demand peak with system
				for (int irow = 0; irow <= 12; irow++)
				{
					for (int icol = 0; icol < (int)m_dc_tou_periods.size(); icol++)
					{
						if (irow == 0)
						{
							monthly_tou_demand_peak_w_sys.at(0, icol) = (float)m_dc_tou_periods[icol];
							monthly_tou_demand_charge_w_sys.at(0, icol) = (float)m_dc_tou_periods[icol];
						}
						else
						{
							int ndx = -1;
							int period = m_dc_tou_periods[icol];
							std::vector<int>::iterator result = std::find(m_month[irow - 1].dc_periods.begin(), m_month[irow - 1].dc_periods.end(), period);
							if (result == m_month[irow - 1].dc_periods.end())
							{
								monthly_tou_demand_peak_w_sys.at(irow, icol) = 0;
								monthly_tou_demand_charge_w_sys.at(irow, icol) = 0;
							}
							else
							{
								ndx = (int)(result - m_month[irow - 1].dc_periods.begin());
								if (ndx > -1 && ndx < (int)m_month[irow - 1].dc_tou_peak.size())
									monthly_tou_demand_peak_w_sys.at(irow, icol) = m_month[irow - 1].dc_tou_peak[ndx];
								if (ndx > -1 && ndx < (int)m_month[irow - 1].dc_tou_charge.size())
									monthly_tou_demand_charge_w_sys.at(irow, icol) = (float)m_month[irow - 1].dc_tou_charge[ndx];
							}
						}
					}
				}
				assign("year1_hourly_dc_with_system", var_data(&demand_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_ec_with_system", var_data(&energy_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_dc_peak_per_period", var_data(&dc_hourly_peak[0], (int)m_num_rec_yearly));

				// sign reversal based on 9/5/13 meeting reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					ec_tou_sched[ii] = (ssc_number_t)m_ec_tou_sched[ii];
					dc_tou_sched[ii] = (ssc_number_t)m_dc_tou_sched[ii];
					load[ii] = -e_load_cy[ii];
					e_tofromgrid[ii] = e_grid_cy[ii];
					if (e_tofromgrid[ii] > 0)
					{
						year1_hourly_e_togrid[ii] = e_tofromgrid[ii];
						year1_hourly_e_fromgrid[ii] = 0.0;
					}
					else
					{
						year1_hourly_e_togrid[ii] = 0.0;
						year1_hourly_e_fromgrid[ii] = -e_tofromgrid[ii];
					}
					p_tofromgrid[ii] = p_grid_cy[ii];
					salespurchases[ii] = revenue_w_sys[ii];
				}
				assign("year1_hourly_ec_tou_schedule", var_data(&ec_tou_sched[0], (int)m_num_rec_yearly));
				assign("year1_hourly_dc_tou_schedule", var_data(&dc_tou_sched[0], (int)m_num_rec_yearly));
				// monthly outputs - Paul and Sean 7/29/13 - updated 8/9/13 and 8/12/13 and 9/10/13
				monthly_outputs(&e_load_cy[0], &e_sys_cy[0], &e_grid_cy[0], &salespurchases[0],
					&monthly_load[0], &monthly_system_generation[0], &monthly_elec_to_grid[0],
					&monthly_elec_needed_from_grid[0],
					&monthly_salespurchases[0]);

				assign("year1_hourly_e_tofromgrid", var_data(&e_tofromgrid[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_tofromgrid", var_data(&p_tofromgrid[0], (int)m_num_rec_yearly));
				assign("bill_load", var_data(&load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_salespurchases_with_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_load", var_data(&monthly_load[0], 12));
				assign("year1_monthly_system_generation", var_data(&monthly_system_generation[0], 12));
				assign("year1_monthly_electricity_to_grid", var_data(&monthly_elec_to_grid[0], 12));
				assign("year1_monthly_electricity_needed_from_grid", var_data(&monthly_elec_needed_from_grid[0], 12));

				assign("year1_monthly_cumulative_excess_generation", var_data(&monthly_cumulative_excess_energy[0], 12));
				assign("year1_monthly_cumulative_excess_dollars", var_data(&monthly_cumulative_excess_dollars[0], 12));
				assign("year1_monthly_utility_bill_w_sys", var_data(&monthly_bill[0], 12));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough electricity to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(m_num_rec_yearly), edemand(m_num_rec_yearly), pdemand(m_num_rec_yearly), e_sys_to_grid(m_num_rec_yearly), e_sys_to_load(m_num_rec_yearly), p_sys_to_load(m_num_rec_yearly);
				for (j = 0; j<m_num_rec_yearly; j++)
				{
					output[j] = e_sys_cy[j];
					edemand[j] = e_grid_cy[j] < 0.0 ? -e_grid_cy[j] : (ssc_number_t)0.0;
					pdemand[j] = p_grid_cy[j] < 0.0 ? -p_grid_cy[j] : (ssc_number_t)0.0;

					ssc_number_t sys_e_net = output[j] + e_load_cy[j];// loads are assumed negative
					e_sys_to_grid[j] = sys_e_net > 0 ? sys_e_net : (ssc_number_t)0.0;
					e_sys_to_load[j] = sys_e_net > 0 ? -e_load_cy[j] : output[j];

//					ssc_number_t sys_p_net = output[j] + p_load[j];// loads are assumed negative
//					p_sys_to_load[j] = sys_p_net > 0 ? -p_load[j] : output[j];
					ssc_number_t sys_p_net = output[j] + p_load_cy[j];// loads are assumed negative
					p_sys_to_load[j] = sys_p_net > 0 ? -p_load_cy[j] : output[j];
				}

				assign("year1_hourly_system_output", var_data(&output[0], (int)m_num_rec_yearly));
				assign("year1_hourly_e_demand", var_data(&edemand[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], (int)m_num_rec_yearly));

				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], (int)m_num_rec_yearly));

				assign("year1_monthly_fixed_with_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_with_system", var_data(&monthly_minimum_charges[0], 12));
				assign("year1_monthly_dc_fixed_with_system", var_data(&monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
				assign("year1_monthly_ec_charge_gross_with_system", var_data(&monthly_ec_charges_gross[0], 12));
				assign("year1_excess_dollars_applied", var_data(&monthly_excess_dollars_applied[0], 12));
				assign("year1_excess_dollars_earned", var_data(&monthly_excess_dollars_earned[0], 12));
				assign("year1_excess_kwhs_applied", var_data(&monthly_excess_kwhs_applied[0], 12));
				assign("year1_excess_kwhs_earned", var_data(&monthly_excess_kwhs_earned[0], 12));
				// peak demand and testing energy use
				for (int ii = 0; ii < 12; ii++)
				{
					monthly_peak[ii] = m_month[ii].dc_flat_peak;
					monthly_test[ii] = -m_month[ii].energy_net;
				}
				assign("year1_monthly_peak_w_system", var_data(&monthly_peak[0], 12));
				assign("year1_monthly_use_w_system", var_data(&monthly_test[0], 12));

			}

			// determine net-revenue benefit due to solar for year 'i'

			annual_net_revenue[i+1] = 0.0;
			annual_electric_load[i + 1] = 0.0;
			energy_net[i + 1] = 0.0;
			annual_revenue_w_sys[i + 1] = 0.0;
			annual_revenue_wo_sys[i + 1] = 0.0;

			for (j = 0; j<m_num_rec_yearly; j++)
			{
				energy_net[i + 1] +=  e_sys_cy[j];
				annual_net_revenue[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				annual_electric_load[i + 1] += -e_load_cy[j];
				annual_revenue_w_sys[i + 1] += revenue_w_sys[j];
				annual_revenue_wo_sys[i + 1] += revenue_wo_sys[j];
			}

			//Outputs from Paul, Nate and Sean 9/9/13
			annual_elec_cost_w_sys[i + 1] = -annual_revenue_w_sys[i+1];
			annual_elec_cost_wo_sys[i + 1] = -annual_revenue_wo_sys[i+1];


			for (j = 0; j < 12; j++)
			{
				utility_bill_w_sys_ym[(i+1)*12 + j] = monthly_bill[j];
				ch_w_sys_dc_fixed_ym[(i + 1) * 12 + j] = monthly_dc_fixed[j];
				ch_w_sys_dc_tou_ym[(i + 1) * 12 + j] = monthly_dc_tou[j];
				ch_w_sys_ec_ym[(i + 1) * 12 + j] = monthly_ec_charges[j];

				ch_w_sys_ec_gross_ym[(i + 1) * 12 + j] = monthly_ec_charges_gross[j];
				excess_dollars_applied_ym[(i + 1) * 12 + j] = monthly_excess_dollars_applied[j];
				excess_dollars_earned_ym[(i + 1) * 12 + j] = monthly_excess_dollars_earned[j];
				excess_kwhs_applied_ym[(i + 1) * 12 + j] = monthly_excess_kwhs_applied[j];
				excess_kwhs_earned_ym[(i + 1) * 12 + j] = monthly_excess_kwhs_earned[j];

				ch_w_sys_fixed_ym[(i + 1) * 12 + j] = monthly_fixed_charges[j];
				ch_w_sys_minimum_ym[(i + 1) * 12 + j] = monthly_minimum_charges[j];

				utility_bill_w_sys[i + 1] += monthly_bill[j];
				ch_w_sys_dc_fixed[i + 1] += monthly_dc_fixed[j];
				ch_w_sys_dc_tou[i + 1] += monthly_dc_tou[j];
				ch_w_sys_ec[i + 1] += monthly_ec_charges[j];
				ch_w_sys_fixed[i + 1] += monthly_fixed_charges[j];
				ch_w_sys_minimum[i + 1] += monthly_minimum_charges[j];
			}


		}

		assign("elec_cost_with_system_year1", annual_elec_cost_w_sys[1]);
		assign("elec_cost_without_system_year1", annual_elec_cost_wo_sys[1]);
		assign("savings_year1", annual_elec_cost_wo_sys[1] - annual_elec_cost_w_sys[1]);
	}

	void monthly_outputs(ssc_number_t *e_load, ssc_number_t *e_sys, ssc_number_t *e_grid, ssc_number_t *salespurchases, ssc_number_t monthly_load[12], ssc_number_t monthly_generation[12], ssc_number_t monthly_elec_to_grid[12], ssc_number_t monthly_elec_needed_from_grid[12], ssc_number_t monthly_salespurchases[12])
	{
		// calculate the monthly net energy and monthly hours
		int m,h,s;
		size_t d;
		ssc_number_t energy_use[12]; // 12 months
		int c=0;

		size_t steps_per_hour = m_num_rec_yearly / 8760;
		for (m=0;m<12;m++)
		{
			energy_use[m] = 0;
			monthly_load[m] = 0;
			monthly_generation[m] = 0;
			monthly_elec_to_grid[m] = 0;
			monthly_salespurchases[m] = 0;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						energy_use[m] += e_grid[c];
						// Sean's sign convention
						monthly_load[m] -= e_load[c];
						monthly_generation[m] += e_sys[c]; // does not include first year sys_scale
						monthly_elec_to_grid[m] += e_grid[c];
						// 9/10/13 update from Paul
						monthly_salespurchases[m] += salespurchases[c];
						c++;
					}
				}
			}
		}
		//

		for (m=0;m<12;m++)
		{
			if (monthly_elec_to_grid[m] > 0)
				monthly_elec_needed_from_grid[m] = monthly_elec_to_grid[m];
			else
				monthly_elec_needed_from_grid[m]=0;
		}
	}

	void setup()
	{
		size_t nrows, ncols, r, c, m, i, j;
		int period, tier, month;
//		util::matrix_t<float> dc_schedwkday(12, 24, 1);
//		util::matrix_t<float> dc_schedwkend(12, 24, 1);

		for (i = 0; i < m_ec_periods_tiers_init.size(); i++)
			m_ec_periods_tiers_init[i].clear();
		m_ec_periods.clear();

		for (i = 0; i < m_dc_tou_periods_tiers.size(); i++)
			m_dc_tou_periods_tiers[i].clear();
		m_dc_tou_periods.clear();

		for (i = 0; i < m_dc_flat_tiers.size(); i++)
			m_dc_flat_tiers[i].clear();

		m_month.clear();
		for (m = 0; m < 12; m++)
		{
			ur_month urm;
			m_month.push_back(urm);
		}

		m_ec_ts_sell_rate.clear();
		m_ec_ts_buy_rate.clear();

		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");
		
		// should be separate input
		bool en_ts_buy_rate = as_boolean("ur_en_ts_buy_rate");

		if (en_ts_buy_rate)
		{
			if (!is_assigned("ur_ts_buy_rate"))
			{
				throw exec_error("utilityrate5", util::format("Time step buy rate enabled but no time step buy rates specified."));
			}
			else
			{ // hourly or sub hourly loads for single year
				size_t cnt;
				ssc_number_t* ts_br;
				ts_br = as_array("ur_ts_buy_rate", &cnt);
				size_t ts_step_per_hour = cnt / 8760;
				if (ts_step_per_hour < 1 || ts_step_per_hour > 60 || ts_step_per_hour * 8760 != cnt)
					throw exec_error("utilityrate5", util::format("number of buy rate records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)cnt, (int)m_num_rec_yearly));

				// assign timestep values for utility rate calculations
				size_t idx = 0;
				ssc_number_t br;
				br = 0;
				size_t step_per_hour = m_num_rec_yearly / 8760;
				//time step rates - fill out to number of generation records per year
				// handle cases
				// 1. if no time step rate  s
				// 2. if time step rate  has 8760 and gen has more records
				// 3. if number records same for time step rate  and gen
				idx = 0;
				for (i = 0; i < 8760; i++)	{
					for (size_t ii = 0; ii < step_per_hour; ii++)	{
						br = (idx < cnt) ? ts_br[idx] : 0;
						m_ec_ts_buy_rate.push_back(br);
						if (ii < ts_step_per_hour) idx++;
					}
				}
			}
		}


		bool en_ts_sell_rate = as_boolean("ur_en_ts_sell_rate");

		if (en_ts_sell_rate)
		{
			if (!is_assigned("ur_ts_sell_rate"))
			{
				throw exec_error("utilityrate5", util::format("Time step sell rate enabled but no time step sell rates specified."));
			}
			else
			{ // hourly or sub hourly loads for single year
				size_t cnt;
				ssc_number_t * ts_sr;
				ts_sr = as_array("ur_ts_sell_rate", &cnt);
				size_t ts_step_per_hour = cnt / 8760;
				if (ts_step_per_hour < 1 || ts_step_per_hour > 60 || ts_step_per_hour * 8760 != cnt)
					throw exec_error("utilityrate5", util::format("invalid number of sell rate records (%d): must be an integer multiple of 8760", (int)cnt));

				// assign timestep values for utility rate calculations
				size_t idx = 0;
				ssc_number_t sr;
				sr = 0;
				size_t step_per_hour = m_num_rec_yearly / 8760;
				//time step rates - fill out to number of generation records per year
				// handle cases
				// 1. if no time step rate  s
				// 2. if time step rate  has 8760 and gen has more records
				// 3. if number records same for time step rate  and gen
				idx = 0;
				for (i = 0; i < 8760; i++)
				{
					for (size_t ii = 0; ii < step_per_hour; ii++)
					{
						sr = (idx < cnt) ? ts_sr[idx] : 0;
						m_ec_ts_sell_rate.push_back(sr);
						if (ii < ts_step_per_hour) idx++;
					}
				}
			}
		}

		// for reporting purposes
		for (i = 0; i < m_num_rec_yearly; i++)
		{
			m_ec_tou_sched.push_back(1);
			m_dc_tou_sched.push_back(1);
		}

		size_t idx = 0;
		size_t steps_per_hour = m_num_rec_yearly / 8760;

		if (ec_enabled)
		{

			ssc_number_t *ec_weekday = as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekday TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			ssc_number_t *ec_weekend = as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekend TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			util::matrix_t<double> ec_schedwkday(nrows, ncols);
			ec_schedwkday.assign(ec_weekday, nrows, ncols);
			util::matrix_t<double> ec_schedwkend(nrows, ncols);
			ec_schedwkend.assign(ec_weekend, nrows, ncols);

			// for each row (month) determine periods in the month
			// m_monthly_ec_tou_ub max of period tier matrix of period xtier +1
			// columns are period, tier1 max, tier 2 max, ..., tier n max


			int ec_tod[8760];

			if (!util::translate_schedule(ec_tod, ec_schedwkday, ec_schedwkend, 1, 12))
				throw general_error("Could not translate weekday and weekend schedules for energy rates.");
			for (i = 0; i < 8760; i++)
			{
				for (size_t ii = 0; ii < steps_per_hour; ii++)
				{
					if (idx < m_num_rec_yearly)
						m_ec_tou_sched[idx] = ec_tod[i];
					idx++;
				}
			}

			// 6 columns period, tier, max usage, max usage units, buy, sell
			ssc_number_t *ec_tou_in = as_matrix("ur_ec_tou_mat", &nrows, &ncols);
			if (ncols != 6)
			{
				std::ostringstream ss;
				ss << "The energy rate table must have 6 columns. Instead it has " << ncols << " columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			util::matrix_t<double> ec_tou_mat(nrows, ncols);
			ec_tou_mat.assign(ec_tou_in, nrows, ncols);

			bool sell_eq_buy = as_boolean("ur_sell_eq_buy");

			for (r = 0; r < nrows; r++)
			{
				period = (int)ec_tou_mat.at(r, 0);
				if (std::find(m_ec_periods.begin(), m_ec_periods.end(), period) == m_ec_periods.end())
					m_ec_periods.push_back(period);
			}
			// sorted periods smallest to largest
			std::sort(m_ec_periods.begin(), m_ec_periods.end());
			// for each period, get list of tier numbers and then sort and construct
			//m_ec_tou_ub, m_ec_tou_units, m_ec_tou_br, ec_tou_sr vectors of vectors

			for (r = 0; r < m_ec_periods.size(); r++)
			{
				m_ec_periods_tiers_init.push_back(std::vector<int>());
			}

			for (r = 0; r < nrows; r++)
			{
				period = (int)ec_tou_mat.at(r, 0);
				tier = (int)ec_tou_mat.at(r, 1);
				std::vector<int>::iterator result = std::find(m_ec_periods.begin(), m_ec_periods.end(), period);
				if (result == m_ec_periods.end())
				{
					std::ostringstream ss;
					ss << "Energy rate Period " << period << " not found.";
					throw exec_error("utilityrate5", ss.str());
				}
				int ndx = (int)(result - m_ec_periods.begin());
				m_ec_periods_tiers_init[ndx].push_back(tier);
			}
			// sort tier values for each period
			for (r = 0; r < m_ec_periods_tiers_init.size(); r++)
				std::sort(m_ec_periods_tiers_init[r].begin(), m_ec_periods_tiers_init[r].end());

			// find all periods for each month m through schedules
			for (m = 0; m < m_month.size(); m++)
			{
				// energy charges
				for (c = 0; c < ec_schedwkday.ncols(); c++)
				{
					if (std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), ec_schedwkday.at(m, c)) == m_month[m].ec_periods.end())
							m_month[m].ec_periods.push_back((int)ec_schedwkday.at(m, c));

					// rollover periods considered from weekday schedule at 12a [0], 6a [5], 12p [11], and 6p [17]
					if ((m_month[m].ec_rollover_periods.size() < 5) && (c == 0 || c == 5 || c == 11 || c == 17))
					{
						m_month[m].ec_rollover_periods.push_back((int)ec_schedwkday.at(m, c));
					}
				}
				for (c = 0; c < ec_schedwkend.ncols(); c++)
				{
					if (std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), ec_schedwkend.at(m, c)) == m_month[m].ec_periods.end())
						m_month[m].ec_periods.push_back((int)ec_schedwkend.at(m, c));
				}
				std::sort(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end());

				// set m_ec_periods_tiers
				for (size_t pt_period = 0; pt_period < m_ec_periods_tiers_init.size(); pt_period++)
					m_month[m].ec_periods_tiers.push_back(std::vector<int>());
				for (size_t pt_period = 0; pt_period < m_ec_periods_tiers_init.size(); pt_period++)
				{
					for (size_t pt_tier = 0; pt_tier < m_ec_periods_tiers_init[pt_period].size(); pt_tier++)
						m_month[m].ec_periods_tiers[pt_period].push_back(m_ec_periods_tiers_init[pt_period][pt_tier]);
				}


			}

			// periods are rows and tiers are columns - note that columns can change based on rows
			// Initialize each month variables that are constant over the simulation
			for (m = 0; m < m_month.size(); m++)
			{
				int num_periods=0;
				int num_tiers = 0;

				for (i = 0; i < m_month[m].ec_periods.size(); i++)
				{
					// find all periods and check that number of tiers the same for all for the month, if not through error
					std::vector<int>::iterator per_num = std::find(m_ec_periods.begin(), m_ec_periods.end(), m_month[m].ec_periods[i]);
					if (per_num == m_ec_periods.end())
					{
						std::ostringstream ss;
						ss << "Period " << m_month[m].ec_periods[i] << " is in Month " << m << " but is not defined in the energy rate table. Rates for each period in the Weekday and Weekend schedules must be defined in the energy rate table.";
						throw exec_error("utilityrate5", ss.str());
					}
					period = (*per_num);
					int ndx = (int)(per_num - m_ec_periods.begin());
					num_tiers = (int)m_ec_periods_tiers_init[ndx].size();
					if (i == 0)
					{
						// redimension ec_ field of ur_month class
						num_periods = (int)m_month[m].ec_periods.size();
						m_month[m].ec_tou_ub.resize_fill(num_periods, num_tiers, (ssc_number_t)1e+38);
						m_month[m].ec_tou_units.resize_fill(num_periods, num_tiers, 0); // kWh
						m_month[m].ec_tou_br.resize_fill(num_periods, num_tiers, 0);
						m_month[m].ec_tou_sr.resize_fill(num_periods, num_tiers, 0);
					}
					else
					{
						if ((int)m_ec_periods_tiers_init[ndx].size() != num_tiers)
						{
							std::ostringstream ss;
							ss << "The number of tiers in the energy rate table, " << m_ec_periods_tiers_init[ndx].size() << ", is incorrect for Month " << m << " and Period " << m_month[m].ec_periods[i] << ". The correct number of tiers for that month and period is " << num_tiers << ".";
							throw exec_error("utilityrate5", ss.str());
						}
					}
					for (j = 0; j < m_ec_periods_tiers_init[ndx].size(); j++)
					{
						tier = m_ec_periods_tiers_init[ndx][j];
						// initialize for each period and tier
						bool found = false;
						for (r = 0; (r < nrows) && !found; r++)
						{
							if ((period == (int)ec_tou_mat.at(r, 0))
								&& (tier == (int)ec_tou_mat.at(r, 1)))
							{
								m_month[m].ec_tou_ub.at(i,j) = ec_tou_mat.at(r, 2);
								// units kWh, kWh/kW, kWh daily, kWh/kW daily
								m_month[m].ec_tou_units.at(i, j) = (int)ec_tou_mat.at(r, 3);
								if ((m_month[m].ec_tou_units.at(i, j) == 2)
									|| (m_month[m].ec_tou_units.at(i, j) == 3))
									{// kWh daily or kWh/kW daily - adjust max usage by number of days in month (or billing cycle per Eric 12/14/15
									m_month[m].ec_tou_ub.at(i, j) *= util::nday[m];
								}
								m_month[m].ec_tou_br.at(i, j) = ec_tou_mat.at(r, 4);
								// adjust sell rate based on input selections
								ssc_number_t sell = ec_tou_mat.at(r, 5);
								if (sell_eq_buy)
									sell = ec_tou_mat.at(r, 4);
								m_month[m].ec_tou_sr.at(i, j) = sell;
								found = true;
							}
						}

					}
					// copy all sr, br, ub for ec in case units force change
					// copy not same memory location
					m_month[m].ec_tou_ub_init = m_month[m].ec_tou_ub;
					m_month[m].ec_tou_br_init = m_month[m].ec_tou_br;
					m_month[m].ec_tou_sr_init = m_month[m].ec_tou_sr;
				}
			}

		}


// demand charge initialization
		if (dc_enabled)
		{

			ssc_number_t *dc_weekday = as_matrix("ur_dc_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekday TOU matrix for demand rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			ssc_number_t *dc_weekend = as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekend TOU matrix for demand rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			util::matrix_t<double> dc_schedwkday(nrows, ncols);
			dc_schedwkday.assign(dc_weekday, nrows, ncols);
			util::matrix_t<double> dc_schedwkend(nrows, ncols);
			dc_schedwkend.assign(dc_weekend, nrows, ncols);

			// for each row (month) determine periods in the month
			// m_monthly_dc_tou_ub max of period tier matrix of period xtier +1
			// columns are period, tier1 max, tier 2 max, ..., tier n max


			int dc_tod[8760];

			if (!util::translate_schedule(dc_tod, dc_schedwkday, dc_schedwkend, 1, 12))
				throw general_error("Could not translate weekday and weekend schedules for demand charges");

			idx = 0;
			for (i = 0; i < 8760; i++)
			{
				for (size_t ii = 0; ii < steps_per_hour; ii++)
				{
					if (idx < m_num_rec_yearly)
						m_dc_tou_sched[idx] = dc_tod[i];
					idx++;
				}
			}

			// 4 columns period, tier, max usage, charge
			ssc_number_t *dc_tou_in = as_matrix("ur_dc_tou_mat", &nrows, &ncols);
			if (ncols != 4)
			{
				std::ostringstream ss;
				ss << "The demand rate table for TOU periods, 'ur_dc_tou_mat', must have 4 columns. Instead, it has " << ncols << "columns.";
				throw exec_error("utilityrate5", ss.str());
			}
			util::matrix_t<double> dc_tou_mat(nrows, ncols);
			dc_tou_mat.assign(dc_tou_in, nrows, ncols);

			// find all periods for each month m through schedules
			for (m = 0; m < m_month.size(); m++)
			{
				// demand charges
				for (c = 0; c < dc_schedwkday.ncols(); c++)
				{
					if (std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), dc_schedwkday.at(m, c)) == m_month[m].dc_periods.end())
						m_month[m].dc_periods.push_back((int)dc_schedwkday.at(m, c));
				}
				for (c = 0; c < dc_schedwkend.ncols(); c++)
				{
					if (std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), dc_schedwkend.at(m, c)) == m_month[m].dc_periods.end())
						m_month[m].dc_periods.push_back((int)dc_schedwkend.at(m, c));
				}
				std::sort(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end());
			}

			for (r = 0; r < nrows; r++)
			{
				period = (int)dc_tou_mat.at(r, 0);
				if (std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), period) == m_dc_tou_periods.end())
					m_dc_tou_periods.push_back(period);
			}
			// sorted periods smallest to largest
			std::sort(m_dc_tou_periods.begin(), m_dc_tou_periods.end());
			// for each period, get list of tier numbers and then sort and construct
			//m_dc_tou_ub, m_dc_tou_units, m_dc_tou_br, dc_tou_sr vectors of vectors
			for (r = 0; r < m_dc_tou_periods.size(); r++)
			{
				m_dc_tou_periods_tiers.push_back(std::vector<int>());
			}

			for (r = 0; r < nrows; r++)
			{
				period = (int)dc_tou_mat.at(r, 0);
				tier = (int)dc_tou_mat.at(r, 1);
				std::vector<int>::iterator result = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), period);
				if (result == m_dc_tou_periods.end())
				{
					std::ostringstream ss;
					ss << "Demand charge Period " << period << " not found.";
					throw exec_error("utilityrate5", ss.str());
				}
				int ndx = (int)(result - m_dc_tou_periods.begin());
				m_dc_tou_periods_tiers[ndx].push_back(tier);
			}
			// sort tier values for each period
			for (r = 0; r < m_dc_tou_periods_tiers.size(); r++)
				std::sort(m_dc_tou_periods_tiers[r].begin(), m_dc_tou_periods_tiers[r].end());


			// periods are rows and tiers are columns - note that columns can change based on rows
			// Initialize each month variables that are constant over the simulation

			for (m = 0; m < m_month.size(); m++)
			{
				int num_periods = 0;
				int num_tiers = 0;
				num_periods = (int)m_month[m].dc_periods.size();
				for (i = 0; i < m_month[m].dc_periods.size(); i++)
				{
					// find all periods and check that number of tiers the same for all for the month, if not through error
					std::vector<int>::iterator per_num = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), m_month[m].dc_periods[i]);
					if (per_num == m_dc_tou_periods.end())
					{
						std::ostringstream ss;
						ss << "Period " << m_month[m].dc_periods[i] << " is in Month " << m << " but is not defined in the demand rate table.  Rates for each period in the Weekday and Weekend schedules must be defined in the demand rate table.";
						throw exec_error("utilityrate5", ss.str());
					}
					period = (*per_num);
					int ndx = (int)(per_num - m_dc_tou_periods.begin());
						// redimension dc_ field of ur_month class
					if ((int)m_dc_tou_periods_tiers[ndx].size() > num_tiers)
						num_tiers = (int)m_dc_tou_periods_tiers[ndx].size();
				}
						m_month[m].dc_tou_ub.resize_fill(num_periods, num_tiers, (ssc_number_t)1e38);
				m_month[m].dc_tou_ch.resize_fill(num_periods, num_tiers, 0); // kWh
				for (i = 0; i < m_month[m].dc_periods.size(); i++)
				{
					// find all periods and check that number of tiers the same for all for the month, if not through error
					std::vector<int>::iterator per_num = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), m_month[m].dc_periods[i]);
					period = (*per_num);
					int ndx = (int)(per_num - m_dc_tou_periods.begin());
					for (j = 0; j < m_dc_tou_periods_tiers[ndx].size(); j++)
					{
						tier = m_dc_tou_periods_tiers[ndx][j];
						// initialize for each period and tier
						bool found = false;
						for (r = 0; (r < nrows) && !found; r++)
						{
							if ((period == (int)dc_tou_mat.at(r, 0))
								&& (tier == (int)dc_tou_mat.at(r, 1)))
							{
								m_month[m].dc_tou_ub.at(i, j) = dc_tou_mat.at(r, 2);
								m_month[m].dc_tou_ch.at(i, j) = dc_tou_mat.at(r, 3);//demand charge;
								found = true;
							}
						}
					}
				}
			}
			// flat demand charge
			// 4 columns month, tier, max usage, charge
			ssc_number_t *dc_flat_in = as_matrix("ur_dc_flat_mat", &nrows, &ncols);
			if (ncols != 4)
			{
				std::ostringstream ss;
				ss << "The demand rate table, 'ur_dc_flat_mat', by month must have 4 columns. Instead it has " << ncols << " columns";
				throw exec_error("utilityrate5", ss.str());
			}
			util::matrix_t<double> dc_flat_mat(nrows, ncols);
			dc_flat_mat.assign(dc_flat_in, nrows, ncols);

			for (r = 0; r < m_month.size(); r++)
			{
				m_dc_flat_tiers.push_back(std::vector<int>());
			}

			for (r = 0; r < nrows; r++)
			{
				month = (int)dc_flat_mat.at(r, 0);
				tier = (int)dc_flat_mat.at(r, 1);
				if ((month < 0) || (month >= (int)m_month.size()))
				{
					std::ostringstream ss;
					ss << "Demand for Month " << month << " not found.";
					throw exec_error("utilityrate5", ss.str());
				}
				m_dc_flat_tiers[month].push_back(tier);
			}
			// sort tier values for each period
			for (r = 0; r < m_dc_flat_tiers.size(); r++)
				std::sort(m_dc_flat_tiers[r].begin(), m_dc_flat_tiers[r].end());


			// months are rows and tiers are columns - note that columns can change based on rows
			// Initialize each month variables that are constant over the simulation



			for (m = 0; m < m_month.size(); m++)
			{
				m_month[m].dc_flat_ub.clear();
				m_month[m].dc_flat_ch.clear();
				for (j = 0; j < m_dc_flat_tiers[m].size(); j++)
				{
					tier = m_dc_flat_tiers[m][j];
					// initialize for each period and tier
					bool found = false;
					for (r = 0; (r < nrows) && !found; r++)
					{
						if ((m == dc_flat_mat.at(r, 0))
							&& (tier == (int)dc_flat_mat.at(r, 1)))
						{
							m_month[m].dc_flat_ub.push_back(dc_flat_mat.at(r, 2));
							m_month[m].dc_flat_ch.push_back(dc_flat_mat.at(r, 3));//rate_esc;
							found = true;
						}
					}

				}
			}

		}

	}




	void ur_calc( ssc_number_t *e_in, ssc_number_t *p_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *demand_charge, ssc_number_t *energy_charge,
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12],
		ssc_number_t monthly_ec_charges_gross[12],
		ssc_number_t excess_dollars_earned[12],
		ssc_number_t excess_dollars_applied[12],
		ssc_number_t excess_kwhs_earned[12],
		ssc_number_t excess_kwhs_applied[12],
		ssc_number_t *dc_hourly_peak, ssc_number_t monthly_cumulative_excess_energy[12],
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12],
		ssc_number_t rate_esc, size_t year, bool include_fixed=true, bool include_min=true, bool gen_only=false)

	{
		int i;

		for (i=0;i<(int)m_num_rec_yearly;i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i=0;i<12;i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= monthly_dc_fixed[i] = monthly_dc_tou[i]
				= monthly_ec_charges[i]
				= monthly_ec_charges_gross[i]
				= excess_dollars_earned[i]
				= excess_dollars_applied[i]
				= excess_kwhs_earned[i]
				= excess_kwhs_applied[i]
				= monthly_cumulative_excess_energy[i]
				= monthly_cumulative_excess_dollars[i]
				= monthly_bill[i] = 0.0;
		}
		// initialize all montly values

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Two meters with all generation sold and all load purchaseded
		4=Single meter with monthly rollover credits in $ (Net Billing $)
		*/
		int metering_option = as_integer("ur_metering_option");
		bool enable_nm = (metering_option == 0 || metering_option == 1);

		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		bool excess_monthly_dollars = (as_integer("ur_metering_option") == 1);

		bool tou_demand_single_peak = (as_integer("TOU_demand_single_peak") == 1);


		size_t steps_per_hour = m_num_rec_yearly / 8760;
		// calculate the monthly net energy and monthly hours
		int m, h, s, period, tier;
		size_t d;
		int c = 0;
		for (m = 0; m < (int)m_month.size(); m++)
		{
			m_month[m].energy_net = 0;
			m_month[m].hours_per_month = 0;
			m_month[m].dc_flat_peak = 0;
			m_month[m].dc_flat_peak_hour = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						m_month[m].energy_net += e_in[c]; // -load and +gen
						// hours per period per month
						m_month[m].hours_per_month++;
						// peak
						if (p_in[c] < 0 && p_in[c] < -m_month[m].dc_flat_peak)
						{
							m_month[m].dc_flat_peak = -p_in[c];
							m_month[m].dc_flat_peak_hour = c;
						}
						c++;
					}
				}
			}
		}

		// monthly cumulative excess energy (positive = excess energy, negative = excess load)
		if (enable_nm && !excess_monthly_dollars)
		{
			ssc_number_t prev_value = 0;
			for (m = 0; m < 12; m++)
			{
				prev_value = (m > 0) ? monthly_cumulative_excess_energy[m - 1] : 0;
				monthly_cumulative_excess_energy[m] = ((prev_value + m_month[m].energy_net) > 0) ? (prev_value + m_month[m].energy_net) : 0;
			}
		}

		// excess earned
		for (m = 0; m < 12; m++)
		{
			if (m_month[m].energy_net > 0)
				excess_kwhs_earned[m] = m_month[m].energy_net;
		}


		// adjust net energy if net metering with monthly rollover
		if (enable_nm && !excess_monthly_dollars)
		{
			for (m = 1; m < (int)m_month.size(); m++)
			{
				if (m_month[m].energy_net < 0)
				{
					m_month[m].energy_net += monthly_cumulative_excess_energy[m - 1];
					excess_kwhs_applied[m] = monthly_cumulative_excess_energy[m - 1];
				}
			}
		}


		if (ec_enabled)
		{
			// calculate the monthly net energy per tier and period based on units
			c = 0;
			for (m = 0; m < (int)m_month.size(); m++)
			{
				int start_tier = 0;
				int end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
				int num_periods = (int)m_month[m].ec_tou_ub_init.nrows();
				int num_tiers = end_tier - start_tier + 1;

				if (!gen_only) // added for two meter no load scenarios to use load tier sizing
				{
					//start_tier = 0;
					end_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
					//int num_periods = (int)m_month[m].ec_tou_ub_init.nrows();
					num_tiers = end_tier - start_tier + 1;

					// kWh/kW (kWh/kW daily handled in Setup)
					// 1. find kWh/kW tier
					// 2. set min tier and max tier based on next item in ec_tou matrix
					// 3. resize use and chart based on number of tiers in kWh/kW section
					// 4. assumption is that all periods in same month have same tier breakdown
					// 5. assumption is that tier numbering is correct for the kWh/kW breakdown
					// That is, first tier must be kWh/kW
					if ((m_month[m].ec_tou_units.ncols()>0 && m_month[m].ec_tou_units.nrows() > 0)
						&& ((m_month[m].ec_tou_units.at(0, 0) == 1) || (m_month[m].ec_tou_units.at(0, 0) == 3)))
					{
						// monthly total energy / monthly peak to determine which kWh/kW tier
						double mon_kWhperkW = -m_month[m].energy_net; // load negative
						if (m_month[m].dc_flat_peak != 0)
							mon_kWhperkW /= m_month[m].dc_flat_peak;
						// find correct start and end tier based on kWhperkW band
						start_tier = 1;
						bool found = false;
						for (size_t i_tier = 0; i_tier < m_month[m].ec_tou_units.ncols(); i_tier++)
						{
							int units = (int)m_month[m].ec_tou_units.at(0, i_tier);
							if ((units == 1) || (units == 3))
							{
								if (found)
								{
									end_tier = (int)i_tier - 1;
									break;
								}
								else if (mon_kWhperkW < m_month[m].ec_tou_ub_init.at(0, i_tier))
								{
									start_tier = (int)i_tier + 1;
									found = true;
								}
							}
						}
						// last tier since no max specified in rate
						if (!found) start_tier = end_tier;
						if (start_tier >= (int)m_month[m].ec_tou_ub_init.ncols())
							start_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
						if (end_tier < start_tier)
							end_tier = start_tier;
						num_tiers = end_tier - start_tier + 1;
						// resize everytime to handle load and energy changes
						// resize sr, br and ub for use in energy charge calculations below
						util::matrix_t<ssc_number_t> br(num_periods, num_tiers);
						util::matrix_t<ssc_number_t> sr(num_periods, num_tiers);
						util::matrix_t<ssc_number_t> ub(num_periods, num_tiers);
						// assign appropriate values.
						for (period = 0; period < num_periods; period++)
						{
							for (tier = 0; tier < num_tiers; tier++)
							{
								br.at(period, tier) = m_month[m].ec_tou_br_init.at(period, start_tier + tier);
								sr.at(period, tier) = m_month[m].ec_tou_sr_init.at(period, start_tier + tier);
								ub.at(period, tier) = m_month[m].ec_tou_ub_init.at(period, start_tier + tier);
								// update for correct tier number column headings
								m_month[m].ec_periods_tiers[period][tier] = start_tier + m_ec_periods_tiers_init[period][tier];
							}
						}

						m_month[m].ec_tou_br = br;
						m_month[m].ec_tou_sr = sr;
						m_month[m].ec_tou_ub = ub;
					}

					// reset now resized - if necessary
				}
				start_tier = 0;
				end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;

				m_month[m].ec_energy_use.resize_fill(num_periods, num_tiers, 0);
				m_month[m].ec_energy_surplus.resize_fill(num_periods, num_tiers, 0);
				m_month[m].ec_charge.resize_fill(num_periods, num_tiers, 0);



				// accumulate energy per period - place all in tier 0 initially and then
				// break up according to tier boundaries and number of periods

				/*  hour by hour accumulation - changed to monthly per meeting with Paul 2/29/16 */
				// monthly accumulation of energy
				ssc_number_t mon_e_net = 0;
				if (m>0 && enable_nm && !excess_monthly_dollars)
				{
					mon_e_net = monthly_cumulative_excess_energy[m - 1]; // rollover
				}

				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
						{
							mon_e_net += e_in[c];
							int toup = m_ec_tou_sched[c];
							std::vector<int>::iterator per_num = std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), toup);
							if (per_num == m_month[m].ec_periods.end())
							{
								std::ostringstream ss;
								ss << "Energy rate TOU Period " << toup << " not found for Month " << util::schedule_int_to_month(m) << ".";
								throw exec_error("utilityrate5", ss.str());
							}
							int row = (int)(per_num - m_month[m].ec_periods.begin());
							// place all in tier 0 initially and then update appropriately
							// net energy per period per month
							m_month[m].ec_energy_use(row, 0) += e_in[c];
							c++;
						}
					}
				}

				/*
				// rollover energy from correct period - based on matching period number
				if (m > 0 && enable_nm && !excess_monthly_kwhs)
				{
					// check for surplus in previous month for same period
					for (size_t ir = 0; ir < m_month[m - 1].ec_energy_surplus.nrows(); ir++)
					{
						if (m_month[m - 1].ec_energy_surplus.at(ir, 0) > 0) // surplus - check period
						{
							int toup = m_month[m - 1].ec_periods[ir]; // number of rows of previous month
							std::vector<int>::iterator per_num = std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), toup);
							if (per_num == m_month[m].ec_periods.end())
							{
								std::ostringstream ss;
								ss << "utilityrate5: energy charge rollover for period " << toup << " not found for month " << m;
								log(ss.str(), SSC_NOTICE);
							}
							else
							{
								ssc_number_t extra = 0;
								int row = (int)(per_num - m_month[m].ec_periods.begin());
								for (size_t ic = 0; ic < m_month[m - 1].ec_energy_surplus.ncols(); ic++)
									extra += m_month[m - 1].ec_energy_surplus.at(ir, ic);

								m_month[m].ec_energy_use(row, 0) += extra;
							}
						}
					}
				}
				*/

				// rollover energy from correct period - matching time of day - currently four values considered 12a, 6a, 12p, 6p set in loop above.
				if (m > 0 && enable_nm && !excess_monthly_dollars)
				{
					// check for surplus in previous month for same period
					for (size_t ir = 0; ir < m_month[m - 1].ec_energy_surplus.nrows(); ir++)
					{
						if (m_month[m - 1].ec_energy_surplus.at(ir, 0) > 0) // surplus - check period
						{
							int toup_source = m_month[m - 1].ec_periods[ir]; // number of rows of previous month - and period with surplus
							// find source period in rollover map for previous month
							std::vector<int>::iterator source_per_num = std::find(m_month[m-1].ec_rollover_periods.begin(), m_month[m-1].ec_rollover_periods.end(), toup_source);
							if (source_per_num == m_month[m-1].ec_rollover_periods.end())
							{
								std::ostringstream ss;
								ss << "year:" << year << " utilityrate5: Unable to determine period for energy charge rollover: Period " << toup_source << " does not exist for 12 am, 6 am, 12 pm or 6 pm in the previous month, which is Month " << util::schedule_int_to_month(m-1) << ".";
								log(ss.str(), SSC_NOTICE);
							}
							else
							{
								// find corresponding target period for same time of day
								ssc_number_t extra = 0;
								int rollover_index = (int)(source_per_num - m_month[m-1].ec_rollover_periods.begin());
								if (rollover_index < (int)m_month[m].ec_rollover_periods.size())
								{
									int toup_target = m_month[m].ec_rollover_periods[rollover_index];
									std::vector<int>::iterator target_per_num = std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), toup_target);
									if (target_per_num == m_month[m].ec_periods.end())
									{
										std::ostringstream ss;
										ss << "year:" << year << "utilityrate5: Unable to determine period for energy charge rollover: Period " << toup_target << " does not exist for 12 am, 6 am, 12 pm or 6 pm in the current month, which is " << util::schedule_int_to_month(m) << ".";
										log(ss.str(), SSC_NOTICE);
									}
									int target_row = (int)(target_per_num - m_month[m].ec_periods.begin());
									for (size_t ic = 0; ic < m_month[m - 1].ec_energy_surplus.ncols(); ic++)
										extra += m_month[m - 1].ec_energy_surplus.at(ir, ic);

									m_month[m].ec_energy_use(target_row, 0) += extra;
								}
							}
						}
					}
				}

				// set surplus or use
				for (size_t ir = 0; ir < m_month[m].ec_energy_use.nrows(); ir++)
				{
					if (m_month[m].ec_energy_use.at(ir, 0) > 0)
					{
						m_month[m].ec_energy_surplus.at(ir, 0) = m_month[m].ec_energy_use.at(ir, 0);
						m_month[m].ec_energy_use.at(ir, 0) = 0;
					}
					else
						m_month[m].ec_energy_use.at(ir, 0) = -m_month[m].ec_energy_use.at(ir, 0);
				}

				// now ditribute across tier boundaries - upper bounds equally across periods
				// 3/5/16 prorate based on total net per period / total net
				// look at total net distributed among tiers

				ssc_number_t num_per = (ssc_number_t)m_month[m].ec_energy_use.nrows();
				ssc_number_t tot_energy = 0;
				for (size_t ir = 0; ir < num_per; ir++)
					tot_energy += m_month[m].ec_energy_use.at(ir, 0);
				if (tot_energy > 0)
				{
					for (size_t ir = 0; ir < num_per; ir++)
					{
						bool done = false;
						ssc_number_t per_energy = m_month[m].ec_energy_use.at(ir, 0);
						for (size_t ic = 0; ic < m_month[m].ec_tou_ub.ncols() && !done; ic++)
						{
							ssc_number_t ub_tier = m_month[m].ec_tou_ub.at(ir, ic);
							if (per_energy > 0)
							{
								if (tot_energy > ub_tier)
								{
									m_month[m].ec_energy_use.at(ir, ic) = (per_energy/tot_energy) * ub_tier;
									if (ic > 0)
										m_month[m].ec_energy_use.at(ir, ic) -= (per_energy / tot_energy) * m_month[m].ec_tou_ub.at(ir, ic - 1);
								}
								else
								{
									m_month[m].ec_energy_use.at(ir, ic) = (per_energy / tot_energy) * tot_energy;
									if (ic > 0)
										m_month[m].ec_energy_use.at(ir, ic) -= (per_energy / tot_energy)* m_month[m].ec_tou_ub.at(ir, ic - 1);
									done=true;
								}
							}
						}
					}
				}

				// repeat for surplus
				tot_energy = 0;
				for (size_t ir = 0; ir < num_per; ir++)
					tot_energy += m_month[m].ec_energy_surplus.at(ir, 0);
				if (tot_energy > 0)
				{
					for (size_t ir = 0; ir < num_per; ir++)
					{
						bool done = false;
						ssc_number_t per_energy = m_month[m].ec_energy_surplus.at(ir, 0);
						for (size_t ic = 0; ic < m_month[m].ec_tou_ub.ncols() && !done; ic++)
						{
							ssc_number_t ub_tier = m_month[m].ec_tou_ub.at(0, ic);
							if (per_energy > 0)
							{
								if (tot_energy > ub_tier)
								{
									m_month[m].ec_energy_surplus.at(ir, ic) = (per_energy / tot_energy) * ub_tier;
									if (ic > 0)
										m_month[m].ec_energy_surplus.at(ir, ic) -= (per_energy / tot_energy) * m_month[m].ec_tou_ub.at(ir, ic - 1);
								}
								else
								{
									m_month[m].ec_energy_surplus.at(ir, ic) = (per_energy / tot_energy) * tot_energy;
									if (ic > 0)
										m_month[m].ec_energy_surplus.at(ir, ic) -= (per_energy / tot_energy)* m_month[m].ec_tou_ub.at(ir, ic - 1);
									done = true;
								}
							}
						}
					}
				}

			} // end month
		}

		// set peak per period - no tier accumulation
		if (dc_enabled)
		{
			c = 0;
			for (m = 0; m < (int)m_month.size(); m++)
			{
				m_month[m].dc_tou_peak.clear();
				m_month[m].dc_tou_peak_hour.clear();
				for (i = 0; i < (int)m_month[m].dc_periods.size(); i++)
				{
					m_month[m].dc_tou_peak.push_back(0);
					m_month[m].dc_tou_peak_hour.push_back(0);
				}
				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
						{
							int todp = m_dc_tou_sched[c];
							std::vector<int>::iterator per_num = std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), todp);
							if (per_num == m_month[m].dc_periods.end())
							{
								std::ostringstream ss;
								ss << "Demand rate Period " << todp << " not found for Month " << m << ".";
								throw exec_error("utilityrate5", ss.str());
							}
							int row = (int)(per_num - m_month[m].dc_periods.begin());
							if (p_in[c] < 0 && p_in[c] < -m_month[m].dc_tou_peak[row])
							{
								m_month[m].dc_tou_peak[row] = -p_in[c];
								m_month[m].dc_tou_peak_hour[row] = c;
							}
							c++;
						}
					}
				}
			}
		}




// main loop
		c = 0;
		// process one month at a time
		for (m = 0; m < (int)m_month.size(); m++)
		{
			if (m_month[m].hours_per_month <= 0) break;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					// energy charge
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour-1) )
						{
							if (ec_enabled)
							{
								// energy use and surplus distributed correctly above.
								// so calculate for all and not based on monthly net
								// addresses issue if net > 0 but one period net < 0
								ssc_number_t credit_amt = 0;
								for (period = 0; period < (int)m_month[m].ec_tou_sr.nrows(); period++)
								{
									for (tier = 0; tier < (int)m_month[m].ec_tou_sr.ncols(); tier++)
									{
										ssc_number_t cr = m_month[m].ec_energy_surplus.at(period, tier) * m_month[m].ec_tou_sr.at(period, tier) * rate_esc;

//										excess_kwhs_earned[m] += m_month[m].ec_energy_surplus.at(period, tier);

										if (!enable_nm)
										{
											credit_amt += cr;
											m_month[m].ec_charge.at(period, tier) = -cr;
										}
										else if (excess_monthly_dollars)
											monthly_cumulative_excess_dollars[m] += cr;

										/*
										if (!enable_nm || excess_monthly_kwhs)
										{
										credit_amt += cr;
										if (!excess_monthly_kwhs)
										m_month[m].ec_charge.at(period, tier) = -cr;
										}
										*/
									}
								}
								monthly_ec_charges[m] -= credit_amt;

								ssc_number_t charge_amt = 0;
								for (period = 0; period < (int)m_month[m].ec_tou_br.nrows(); period++)
								{
									for (tier = 0; tier < (int)m_month[m].ec_tou_br.ncols(); tier++)
									{
										ssc_number_t ch = m_month[m].ec_energy_use.at(period, tier) * m_month[m].ec_tou_br.at(period, tier) * rate_esc;
										m_month[m].ec_charge.at(period, tier) = ch;
										charge_amt += ch;
									}
								}
								monthly_ec_charges[m] += charge_amt;


								// monthly rollover with year end sell at reduced rate
								if (enable_nm)
								{
									payment[c] += monthly_ec_charges[m];
									/*
									if (monthly_ec_charges[m] < 0)
									{
									monthly_cumulative_excess_kwhs[m] = -monthly_ec_charges[m];
									payment[c] += monthly_ec_charges[m];
									}
									*/
								}
								else // non-net metering - no rollover
								{
									if (m_month[m].energy_net < 0) // must buy from grid
										payment[c] += monthly_ec_charges[m];
									else // surplus - sell to grid
										income[c] -= monthly_ec_charges[m]; // charge is negative for income!
								}

								energy_charge[c] += monthly_ec_charges[m];

								// end of energy charge

							}


							if (dc_enabled)
							{
								// fixed demand charge
								// compute charge based on tier structure for the month
								ssc_number_t charge = 0;
								ssc_number_t d_lower = 0;
								ssc_number_t demand = m_month[m].dc_flat_peak;
								bool found = false;
								for (tier = 0; tier < (int)m_month[m].dc_flat_ub.size() && !found; tier++)
								{
									if (demand < m_month[m].dc_flat_ub[tier])
									{
										found = true;
										charge += (demand - d_lower) *
											m_month[m].dc_flat_ch[tier] * rate_esc;
										m_month[m].dc_flat_charge = charge;
									}
									else
									{
										charge += (m_month[m].dc_flat_ub[tier] - d_lower) *
											m_month[m].dc_flat_ch[tier] * rate_esc;
										d_lower = m_month[m].dc_flat_ub[tier];
									}
								}

								monthly_dc_fixed[m] = charge; // redundant...
								payment[c] += monthly_dc_fixed[m];
								demand_charge[c] = charge;
								dc_hourly_peak[m_month[m].dc_flat_peak_hour] = demand;


								// end of fixed demand charge


								// TOU demand charge for each period find correct tier
								demand = 0;
								d_lower = 0;
								int peak_hour = 0;
								m_month[m].dc_tou_charge.clear();
								for (period = 0; period < (int)m_month[m].dc_tou_ub.nrows(); period++)
								{
									charge = 0;
									d_lower = 0;
									if (tou_demand_single_peak)
									{
										demand = m_month[m].dc_flat_peak;
										if (m_month[m].dc_flat_peak_hour != m_month[m].dc_tou_peak_hour[period]) continue; // only one peak per month.
									}
									else
										demand = m_month[m].dc_tou_peak[period];
									// find tier corresponding to peak demand
									found = false;
									for (tier = 0; tier < (int)m_month[m].dc_tou_ub.ncols() && !found; tier++)
									{
										if (demand < m_month[m].dc_tou_ub.at(period, tier))
										{
											found = true;
											charge += (demand - d_lower) *
												m_month[m].dc_tou_ch.at(period, tier)* rate_esc;
											m_month[m].dc_tou_charge.push_back(charge);
										}
										else
										{
											charge += (m_month[m].dc_tou_ub.at(period, tier) - d_lower) * m_month[m].dc_tou_ch.at(period, tier)* rate_esc;
											d_lower = m_month[m].dc_tou_ub.at(period, tier);
										}
									}

									dc_hourly_peak[peak_hour] = demand;
									// add to payments
									monthly_dc_tou[m] += charge;
									payment[c] += charge; // apply to last hour of the month
									demand_charge[c] += charge; // add TOU charge to hourly demand charge
								}
								// end of TOU demand charge
							}

						} // end of if end of month
						c++;
					} // end of steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess kwhs and rollover
//			monthly_bill[m] = payment[c - 1] - income[c - 1];
			monthly_bill[m] = monthly_ec_charges[m] + monthly_dc_fixed[m] + monthly_dc_tou[m];

			monthly_ec_charges_gross[m] = monthly_ec_charges[m];
			excess_dollars_earned[m] = monthly_cumulative_excess_dollars[m];
			ssc_number_t dollars_applied = 0;
			if (enable_nm)
			{
				// apply previous month rollover kwhs
				if (m > 0)
				{
//					monthly_bill[m] -= monthly_cumulative_excess_dollars[m - 1];
					payment[c - 1] -= monthly_cumulative_excess_dollars[m-1];
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m - 1];
					dollars_applied += monthly_cumulative_excess_dollars[m - 1];
				}
//				if (monthly_bill[m] < 0)
				if (monthly_ec_charges[m] < 0)
				{
					if (excess_monthly_dollars)
					{
						monthly_cumulative_excess_dollars[m] -= monthly_ec_charges[m];
						//						monthly_cumulative_excess_dollars[m] -= monthly_bill[m];
					}
					//					monthly_bill[m] = 0;
					payment[c - 1] -= monthly_ec_charges[m];; // keep demand charges
					monthly_ec_charges[m] = 0;
				}
				else // apply current month rollover and adjust
				{
//					monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m];
					//					if (monthly_bill[m] < 0)
					if (monthly_ec_charges[m] < 0)
					{
						payment[c - 1] -= monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
						if (excess_monthly_dollars)
						{
//							monthly_cumulative_excess_dollars[m] = -monthly_bill[m];
							dollars_applied += monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
							monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
						}
						//						monthly_bill[m] = 0;
						monthly_ec_charges[m] = 0;
					}
					else
					{
						dollars_applied += monthly_cumulative_excess_dollars[m];
						payment[c - 1] -= monthly_cumulative_excess_dollars[m];
						monthly_cumulative_excess_dollars[m] = 0;
					}
				}
			}
			if (monthly_ec_charges_gross[m] < dollars_applied) dollars_applied = monthly_ec_charges_gross[m];
			excess_dollars_applied[m] = dollars_applied;
			monthly_bill[m] = monthly_ec_charges[m] + monthly_dc_fixed[m] + monthly_dc_tou[m];

		} // end of month m (m loop)

		// TODO: check two meter excess


		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		// compute revenue ( = income - payment ) and monthly bill ( = payment - income) and apply fixed and minimum charges
		c = 0;
		ssc_number_t mon_bill = 0, ann_bill = 0;
		ssc_number_t ann_min_charge = as_number("ur_annual_min_charge")*rate_esc;
		ssc_number_t mon_min_charge = as_number("ur_monthly_min_charge")*rate_esc;
		ssc_number_t mon_fixed = as_number("ur_monthly_fixed_charge")*rate_esc;

		// process one month at a time
		for (m = 0; m < 12; m++)
		{
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{
							// apply fixed first
							if (include_fixed)
							{
								payment[c] += mon_fixed;
								monthly_fixed_charges[m] += mon_fixed;
							}
							mon_bill = payment[c] - income[c];
							if (mon_bill < 0) mon_bill = 0; // for calculating min charge when monthly surplus.
							// apply monthly minimum
							if (include_min)
							{
								if (mon_bill < mon_min_charge)
								{
									monthly_minimum_charges[m] += mon_min_charge - mon_bill;
									payment[c] += mon_min_charge - mon_bill;
								}
							}
							ann_bill += mon_bill;
							if (m == 11)
							{
								// apply annual minimum
								if (include_min)
								{
									if (ann_bill < ann_min_charge)
									{
										monthly_minimum_charges[m] += ann_min_charge - ann_bill;
										payment[c] += ann_min_charge - ann_bill;
									}
								}
								// apply annual rollovers AFTER minimum calculations
								if (enable_nm)
								{
									// monthly rollover with year end sell at reduced rate
									if (!excess_monthly_dollars && (monthly_cumulative_excess_energy[11] > 0))
									{
										ssc_number_t year_end_dollars = monthly_cumulative_excess_energy[11] * as_number("ur_nm_yearend_sell_rate")*rate_esc;
										income[c] += year_end_dollars;
										monthly_cumulative_excess_dollars[11] = year_end_dollars;
										excess_dollars_earned[11] += year_end_dollars;
										excess_dollars_applied[11] += year_end_dollars;
									}
									else if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[11] > 0))
									{
										income[c] += monthly_cumulative_excess_dollars[11];
										// ? net metering energy?
									}
								}
							}
							revenue[c] = income[c] - payment[c];
							monthly_bill[m] = -revenue[c];
						}
						c++;
					}
				}
			}
		}

	}

	// updated to timestep for net billing
	void ur_calc_timestep(ssc_number_t *e_in, ssc_number_t *p_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *demand_charge, ssc_number_t *energy_charge,
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12],
		ssc_number_t monthly_ec_charges_gross[12],
		ssc_number_t excess_dollars_earned[12],
		ssc_number_t excess_dollars_applied[12],
		ssc_number_t excess_kwhs_earned[12],
		ssc_number_t excess_kwhs_applied[12],
		ssc_number_t *dc_hourly_peak, ssc_number_t monthly_cumulative_excess_energy[12],
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12],
		ssc_number_t rate_esc, bool include_fixed = true, bool include_min = true, bool gen_only = false)

	{
		int i;
		for (i = 0; i<(int)m_num_rec_yearly; i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i = 0; i<12; i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= monthly_dc_fixed[i] = monthly_dc_tou[i]
				= monthly_ec_charges[i]
				= monthly_ec_charges_gross[i]
				= excess_dollars_earned[i]
				= excess_dollars_applied[i]
				= excess_kwhs_earned[i]
				= excess_kwhs_applied[i]
				= monthly_cumulative_excess_energy[i]
				= monthly_cumulative_excess_dollars[i]
				= monthly_bill[i] = 0.0;
		}

		// 0=hourly (match with 2015.1.30 release, 1=monthly (most common unit in URDB), 2=daily (used for PG&E baseline rates). Currently hidden in UI and set to zero
//		int ur_ec_hourly_acc_period = as_integer("ur_ec_hourly_acc_period");
		int ur_ec_hourly_acc_period = 1; // monthly per 2/25/16 meeting
		// single meter so single net accumulation
		ssc_number_t daily_surplus_energy;
		ssc_number_t monthly_surplus_energy;
		ssc_number_t daily_deficit_energy;
		ssc_number_t monthly_deficit_energy;

		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Single meter with monthly rollover credits in $ (Net Billing $)
		4=Two meters with all generation sold and all load purchaseded
		*/
		//int metering_option = as_integer("ur_metering_option");
		bool excess_monthly_dollars = (as_integer("ur_metering_option") == 3);

		bool tou_demand_single_peak = (as_integer("TOU_demand_single_peak") == 1);


		size_t steps_per_hour = m_num_rec_yearly / 8760;


		// calculate the monthly net energy and monthly hours
		int m, h, s, period, tier;
		size_t d;
		size_t c = 0;
		for (m = 0; m < (int)m_month.size(); m++)
		{
			m_month[m].energy_net = 0;
			m_month[m].hours_per_month = 0;
			m_month[m].dc_flat_peak = 0;
			m_month[m].dc_flat_peak_hour = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						m_month[m].energy_net += e_in[c]; // -load and +gen
						// hours per period per month
						m_month[m].hours_per_month++;
						// peak
						if (p_in[c] < 0 && p_in[c] < -m_month[m].dc_flat_peak)
						{
							m_month[m].dc_flat_peak = -p_in[c];
							m_month[m].dc_flat_peak_hour = (int)c;
						}
						c++;
					}
				}
			}
		}

		// excess earned
		for (m = 0; m < 12; m++)
		{
			if (m_month[m].energy_net > 0)
				excess_kwhs_earned[m] = m_month[m].energy_net;
		}




		if (ec_enabled)
		{
			// calculate the monthly net energy per tier and period based on units
			c = 0;
			for (m = 0; m < (int)m_month.size(); m++)
			{
				// check for kWh/kW
				int start_tier = 0;
				int end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
				int num_periods = (int)m_month[m].ec_tou_ub.nrows();
				int num_tiers = end_tier - start_tier + 1;

				if (!gen_only) // added for two meter no load scenarios to use load tier sizing
				{
					//start_tier = 0;
					end_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
					//int num_periods = (int)m_month[m].ec_tou_ub_init.nrows();
					num_tiers = end_tier - start_tier + 1;


					// kWh/kW (kWh/kW daily handled in Setup)
					// 1. find kWh/kW tier
					// 2. set min tier and max tier based on next item in ec_tou matrix
					// 3. resize use and chart based on number of tiers in kWh/kW section
					// 4. assumption is that all periods in same month have same tier breakdown
					// 5. assumption is that tier numbering is correct for the kWh/kW breakdown
					// That is, first tier must be kWh/kW
					if ((m_month[m].ec_tou_units.ncols() > 0 && m_month[m].ec_tou_units.nrows() > 0)
						&& ((m_month[m].ec_tou_units.at(0, 0) == 1) || (m_month[m].ec_tou_units.at(0, 0) == 3)))
					{
						// monthly total energy / monthly peak to determine which kWh/kW tier
						double mon_kWhperkW = -m_month[m].energy_net; // load negative
						if (m_month[m].dc_flat_peak != 0)
							mon_kWhperkW /= m_month[m].dc_flat_peak;
						// find correct start and end tier based on kWhperkW band
						start_tier = 1;
						bool found = false;
						for (size_t i_tier = 0; i_tier < m_month[m].ec_tou_units.ncols(); i_tier++)
						{
							int units = (int)m_month[m].ec_tou_units.at(0, i_tier);
							if ((units == 1) || (units == 3))
							{
								if (found)
								{
									end_tier = (int)i_tier - 1;
									break;
								}
								else if (mon_kWhperkW < m_month[m].ec_tou_ub_init.at(0, i_tier))
								{
									start_tier = (int)i_tier + 1;
									found = true;
								}
							}
						}
						// last tier since no max specified in rate
						if (!found) start_tier = end_tier;
						if (start_tier >= (int)m_month[m].ec_tou_ub_init.ncols())
							start_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
						if (end_tier < start_tier)
							end_tier = start_tier;
						num_tiers = end_tier - start_tier + 1;
						// resize everytime to handle load and energy changes
						// resize sr, br and ub for use in energy charge calculations below
						util::matrix_t<ssc_number_t> br(num_periods, num_tiers);
						util::matrix_t<ssc_number_t> sr(num_periods, num_tiers);
						util::matrix_t<ssc_number_t> ub(num_periods, num_tiers);
						// assign appropriate values.
						for (period = 0; period < num_periods; period++)
						{
							for (tier = 0; tier < num_tiers; tier++)
							{
								br.at(period, tier) = m_month[m].ec_tou_br_init.at(period, start_tier + tier);
								sr.at(period, tier) = m_month[m].ec_tou_sr_init.at(period, start_tier + tier);
								ub.at(period, tier) = m_month[m].ec_tou_ub_init.at(period, start_tier + tier);
								// update for correct tier number column headings
								m_month[m].ec_periods_tiers[period][tier] = start_tier + m_ec_periods_tiers_init[period][tier];
							}
						}

						m_month[m].ec_tou_br = br;
						m_month[m].ec_tou_sr = sr;
						m_month[m].ec_tou_ub = ub;
					}
				}
				// reset now resized
				start_tier = 0;
				end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;

				m_month[m].ec_energy_surplus.resize_fill(num_periods, num_tiers, 0);
				m_month[m].ec_energy_use.resize_fill(num_periods, num_tiers, 0);
				m_month[m].ec_charge.resize_fill(num_periods, num_tiers, 0);

			}
		}


		// set peak per period - no tier accumulation
		if (dc_enabled)
		{
			c = 0;
			for (m = 0; m < (int)m_month.size(); m++)
			{
				m_month[m].dc_tou_peak.clear();
				m_month[m].dc_tou_peak_hour.clear();
				for (i = 0; i < (int)m_month[m].dc_periods.size(); i++)
				{
					m_month[m].dc_tou_peak.push_back(0);
					m_month[m].dc_tou_peak_hour.push_back(0);
				}
				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
						{
							int todp = m_dc_tou_sched[c];
							std::vector<int>::iterator per_num = std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), todp);
							if (per_num == m_month[m].dc_periods.end())
							{
								std::ostringstream ss;
								ss << "Demand charge Period " << todp << " not found for Month " << m << ".";
								throw exec_error("utilityrate5", ss.str());
							}
							int row = (int)(per_num - m_month[m].dc_periods.begin());
							if (p_in[c] < 0 && p_in[c] < -m_month[m].dc_tou_peak[row])
							{
								m_month[m].dc_tou_peak[row] = -p_in[c];
								m_month[m].dc_tou_peak_hour[row] = (int)c;
							}
							c++;
						}
					}
				}
			}
		}

// main loop
		c = 0; // hourly count
		// process one timestep at a time
		for (m = 0; m < 12; m++)
		{
			monthly_surplus_energy = 0;
			monthly_deficit_energy = 0;
			for (d = 0; d<util::nday[m]; d++)
			{
				daily_surplus_energy = 0;
				daily_deficit_energy = 0;
				//daily_net_energy = 0;
				for (h = 0; h<24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// energy charge
						if (ec_enabled)
						{
							period = m_ec_tou_sched[c];
							// find corresponding monthly period
							// check for valid period
							std::vector<int>::iterator per_num = std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), period);
							if (per_num == m_month[m].ec_periods.end())
							{
								std::ostringstream ss;
								ss << "Energy rate Period " << period << " not found for Month " << m << ".";
								throw exec_error("utilityrate5", ss.str());
							}
							int row = (int)(per_num - m_month[m].ec_periods.begin());

							if (e_in[c] >= 0.0)
							{ // calculate income or credit
								monthly_surplus_energy += e_in[c];
								daily_surplus_energy += e_in[c];

								// base period charge on units specified
								ssc_number_t energy_surplus = e_in[c];
								ssc_number_t cumulative_energy = e_in[c];
								if (ur_ec_hourly_acc_period == 1)
									cumulative_energy = monthly_surplus_energy;
								else if (ur_ec_hourly_acc_period == 2)
									cumulative_energy = daily_surplus_energy;


								// cumulative energy used to determine tier for credit of entire surplus amount
								ssc_number_t credit_amt = 0;
								for (tier = 0; tier < (int)m_month[m].ec_tou_ub.ncols(); tier++)
								{
									ssc_number_t e_upper = m_month[m].ec_tou_ub.at(row, tier);
									if (cumulative_energy < e_upper)
										break;
								}
								if (tier >= (int)m_month[m].ec_tou_ub.ncols())
									tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
								ssc_number_t tier_energy = energy_surplus;
								ssc_number_t sr = m_month[m].ec_tou_sr.at(row, tier);
								// time step sell rates
								if (as_boolean("ur_en_ts_sell_rate")) {
									if (c < m_ec_ts_sell_rate.size()) {
										sr = m_ec_ts_sell_rate[c];
									}
								}
								ssc_number_t tier_credit = tier_energy * sr * rate_esc;

								credit_amt = tier_credit;


								if (excess_monthly_dollars)
								{
									monthly_cumulative_excess_dollars[m] += credit_amt;
								}
								else
								{
									m_month[m].ec_charge.at(row, tier) -= (ssc_number_t)tier_credit;
									//								price[c] += (ssc_number_t)credit_amt;
									monthly_ec_charges[m] -= (ssc_number_t)credit_amt;
									income[c] = (ssc_number_t)credit_amt;
									energy_charge[c] = -(ssc_number_t)credit_amt;
								}
								m_month[m].ec_energy_surplus.at(row, tier) += (ssc_number_t)tier_energy;
								excess_kwhs_earned[m] += tier_energy;
							}
							else
							{ // calculate payment or charge
								monthly_deficit_energy -= e_in[c];
								daily_deficit_energy -= e_in[c];
								double charge_amt = 0;
								double energy_deficit = -e_in[c];
								// base period charge on units specified
								double cumulative_deficit = -e_in[c];
								if (ur_ec_hourly_acc_period == 1)
									cumulative_deficit = monthly_deficit_energy;
								else if (ur_ec_hourly_acc_period == 2)
									cumulative_deficit = daily_deficit_energy;


								// cumulative energy used to determine tier for credit of entire surplus amount
								for (tier = 0; tier < (int)m_month[m].ec_tou_ub.ncols(); tier++)
								{
									double e_upper = m_month[m].ec_tou_ub.at(row, tier);
									if (cumulative_deficit < e_upper)
										break;
								}
								if (tier >= (int)m_month[m].ec_tou_ub.ncols())
									tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
								double tier_energy = energy_deficit;
								double tier_charge = tier_energy * m_month[m].ec_tou_br.at(row, tier) * rate_esc;

								// time step buy rates
								if (as_boolean("ur_en_ts_buy_rate")) {
									if (c < m_ec_ts_buy_rate.size()) {
										tier_charge = m_ec_ts_buy_rate[c] * tier_energy * rate_esc;
									}
								}

								charge_amt = tier_charge;
								m_month[m].ec_energy_use.at(row, tier) += (ssc_number_t)tier_energy;
								m_month[m].ec_charge.at(row, tier) += (ssc_number_t)tier_charge;

								payment[c] = (ssc_number_t)charge_amt;
								monthly_ec_charges[m] += (ssc_number_t)charge_amt;
//								price[c] += (ssc_number_t)charge_amt;
								energy_charge[c] = (ssc_number_t)charge_amt;
							}
						}
						// end of energy charge


						// demand charge - end of month only
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{

							if (dc_enabled)
							{
								// fixed demand charge
								// compute charge based on tier structure for the month
								ssc_number_t charge = 0;
								ssc_number_t d_lower = 0;
								ssc_number_t demand = m_month[m].dc_flat_peak;
								bool found = false;
								for (tier = 0; tier < (int)m_month[m].dc_flat_ub.size() && !found; tier++)
								{
									if (demand < m_month[m].dc_flat_ub[tier])
									{
										found = true;
										charge += (demand - d_lower) *
											m_month[m].dc_flat_ch[tier] * rate_esc;
										m_month[m].dc_flat_charge = charge;
									}
									else
									{
										charge += (m_month[m].dc_flat_ub[tier] - d_lower) *
											m_month[m].dc_flat_ch[tier] * rate_esc;
										d_lower = m_month[m].dc_flat_ub[tier];
									}
								}

								monthly_dc_fixed[m] = charge; // redundant...
								payment[c] += monthly_dc_fixed[m];
								demand_charge[c] = charge;
								dc_hourly_peak[m_month[m].dc_flat_peak_hour] = demand;


								// end of fixed demand charge


								// TOU demand charge for each period find correct tier
								demand = 0;
								d_lower = 0;
								int peak_hour = 0;
								m_month[m].dc_tou_charge.clear();
								for (period = 0; period < (int)m_month[m].dc_tou_ub.nrows(); period++)
								{
									charge = 0;
									d_lower = 0;
									if (tou_demand_single_peak)
									{
										demand = m_month[m].dc_flat_peak;
										if (m_month[m].dc_flat_peak_hour != m_month[m].dc_tou_peak_hour[period]) continue; // only one peak per month.
									}
									else
										demand = m_month[m].dc_tou_peak[period];

									found = false;
									for (tier = 0; tier < (int)m_month[m].dc_tou_ub.ncols() && !found; tier++)
									{
										if (demand < m_month[m].dc_tou_ub.at(period, tier))
										{
											found = true;
											charge += (demand - d_lower) *
												m_month[m].dc_tou_ch.at(period, tier)* rate_esc;
											m_month[m].dc_tou_charge.push_back(charge);
										}
										else
										{
											charge += (m_month[m].dc_tou_ub.at(period, tier) - d_lower) * m_month[m].dc_tou_ch.at(period, tier)* rate_esc;
											d_lower = m_month[m].dc_tou_ub.at(period, tier);
										}
									}

									dc_hourly_peak[peak_hour] = demand;
									// add to payments
									monthly_dc_tou[m] += charge;
									payment[c] += charge; // apply to last hour of the month
									demand_charge[c] += charge; // add TOU charge to hourly demand charge
								}
								// end of TOU demand charge
								// end of TOU demand charge
							} // if demand charges enabled (dc_enabled)
						}	// end of demand charges at end of month

						c++;
					} // steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess kwhs and rollover

			monthly_bill[m] = monthly_ec_charges[m] + monthly_dc_fixed[m] + monthly_dc_tou[m];

			excess_dollars_earned[m] = monthly_cumulative_excess_dollars[m];

			monthly_ec_charges_gross[m] = monthly_ec_charges[m];
			ssc_number_t dollars_applied = 0;
			// apply previous month rollover kwhs
			if (excess_monthly_dollars)
			{
				if (m > 0)
				{
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m - 1];
					payment[c - 1] -= monthly_cumulative_excess_dollars[m - 1];
					dollars_applied += monthly_cumulative_excess_dollars[m - 1];
				}

				if (monthly_ec_charges[m] < 0)
				{
					payment[c - 1] -= monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
					dollars_applied += monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
					monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
					monthly_ec_charges[m] = 0;
					monthly_cumulative_excess_dollars[m] -= monthly_ec_charges[m];
				}
				/*
									if (monthly_ec_charges[m] < 0)
					{
						payment[c - 1] -= monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
						if (excess_monthly_dollars)
						{
//							monthly_cumulative_excess_dollars[m] = -monthly_bill[m];
							dollars_applied += monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
							monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
						}
						//						monthly_bill[m] = 0;
						monthly_ec_charges[m] = 0;
					}
					else
					{
						dollars_applied += monthly_cumulative_excess_dollars[m];
						payment[c - 1] -= monthly_cumulative_excess_dollars[m];
						monthly_cumulative_excess_dollars[m] = 0;
					}



				*/



				/*
				else // apply current month rollover and adjust
				{
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m];
					if (monthly_ec_charges[m] < 0)
					{
						monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
					//	monthly_ec_charges[m] = 0;
					//	payment[c - 1] = 0; // fixed charges applied below
					}
					else
					{
						payment[c - 1] -= monthly_cumulative_excess_dollars[m];
						monthly_cumulative_excess_kwhs[m] = 0;
					}
				}
				*/
			}
			if (monthly_ec_charges_gross[m] < dollars_applied) dollars_applied = monthly_ec_charges_gross[m];
			excess_dollars_applied[m] = dollars_applied;
			monthly_bill[m] = monthly_ec_charges[m] + monthly_dc_fixed[m] + monthly_dc_tou[m];
		} // end of month m (m loop)


		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		// compute revenue ( = income - payment ) and monthly bill ( = payment - income) and apply fixed and minimum charges
		c = 0;
		ssc_number_t mon_bill = 0, ann_bill = 0;
		ssc_number_t ann_min_charge = as_number("ur_annual_min_charge")*rate_esc;
		ssc_number_t mon_min_charge = as_number("ur_monthly_min_charge")*rate_esc;
		ssc_number_t mon_fixed = as_number("ur_monthly_fixed_charge")*rate_esc;

		// process one month at a time
		for (m = 0; m < 12; m++)
		{
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{
							// apply fixed first
							if (include_fixed)
							{
								payment[c] += mon_fixed;
								monthly_fixed_charges[m] += mon_fixed;
							}
							mon_bill = monthly_bill[m] + monthly_fixed_charges[m];
							if (mon_bill < 0) mon_bill = 0; // for calculating min charge with monthly surplus
							// apply monthly minimum
							if (include_min)
							{
								if (mon_bill < mon_min_charge)
								{
									monthly_minimum_charges[m] += mon_min_charge - mon_bill;
									payment[c] += mon_min_charge - mon_bill;
								}
							}
							ann_bill += mon_bill;
							if (m == 11)
							{
								// apply annual minimum
								if (include_min)
								{
									if (ann_bill < ann_min_charge)
									{
										monthly_minimum_charges[m] += ann_min_charge - ann_bill;
										payment[c] += ann_min_charge - ann_bill;
									}
								}
								// apply annual rollovers AFTER minimum calculations
								if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[m] > 0))
								{
									income[c] += monthly_cumulative_excess_dollars[m];
									monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
								}
							}
							monthly_bill[m] += monthly_fixed_charges[m] + monthly_minimum_charges[m];
						}
						revenue[c] = income[c] - payment[c];
						c++;
					}
				}
			}
		}

	}


	void ur_update_ec_monthly(int month, util::matrix_t<double>& charge, util::matrix_t<double>& energy, util::matrix_t<double>& surplus)

	{
		if (month < 0 || month > (int)m_month.size())
		{
			std::ostringstream ss;
			ss << "ur_update_ec_monthly month not found for Month " << month;
			throw exec_error("utilityrate5", ss.str());
		}
		charge.resize_fill(m_month[month].ec_charge.nrows() + 2, m_month[month].ec_charge.ncols() + 2, 0);
		energy.resize_fill(m_month[month].ec_energy_use.nrows() + 2, m_month[month].ec_energy_use.ncols() + 2, 0);
		surplus.resize_fill(m_month[month].ec_energy_surplus.nrows() + 2, m_month[month].ec_energy_surplus.ncols() + 2, 0);
		// output with tier column headings and period row labels
		// and totals for rows and columns.
		int ndx = -1;
		int period = 0;
		if ((m_month[month].ec_periods.size() > 0) && (m_ec_periods.size() > 0))
		{
			// note that all monthly periods have same tiers (checked in setup)
			period = m_month[month].ec_periods[0];
			std::vector<int>::iterator result = std::find(m_ec_periods.begin(), m_ec_periods.end(), period);
			if (result == m_ec_periods.end())
			{
				std::ostringstream ss;
				ss << "Energy rate Period " << period << " not found.";
				throw exec_error("utilityrate5", ss.str());
			}
			ndx = (int)(result - m_ec_periods.begin());
		}
		if (ndx > -1)
		{
			for (int ic = 0; ic < (int)m_month[month].ec_charge.ncols(); ic++)
			{
				charge.at(0, ic + 1) = (float)m_month[month].ec_periods_tiers[ndx][ic];
				energy.at(0, ic + 1) = (float)m_month[month].ec_periods_tiers[ndx][ic];
				surplus.at(0, ic + 1) = (float)m_month[month].ec_periods_tiers[ndx][ic];
			}
			for (int ir = 0; ir < (int)m_month[month].ec_charge.nrows(); ir++)
			{
				charge.at(ir + 1, 0) = (float)m_month[month].ec_periods[ir];
				energy.at(ir + 1, 0) = (float)m_month[month].ec_periods[ir];
				surplus.at(ir + 1, 0) = (float)m_month[month].ec_periods[ir];
			}
			ssc_number_t c_total = 0;
			ssc_number_t e_total = 0;
			ssc_number_t s_total = 0;
			for (int ir = 0; ir < (int)m_month[month].ec_charge.nrows(); ir++)
			{
				ssc_number_t c_row_total = 0;
				ssc_number_t e_row_total = 0;
				ssc_number_t s_row_total = 0;
				for (int ic = 0; ic <(int)m_month[month].ec_charge.ncols(); ic++)
				{
					charge.at(ir + 1, ic + 1) = m_month[month].ec_charge.at(ir, ic);
					c_row_total += m_month[month].ec_charge.at(ir, ic);
					energy.at(ir + 1, ic + 1) = m_month[month].ec_energy_use.at(ir, ic);
					e_row_total += m_month[month].ec_energy_use.at(ir, ic);
					surplus.at(ir + 1, ic + 1) = m_month[month].ec_energy_surplus.at(ir, ic);
					s_row_total += m_month[month].ec_energy_surplus.at(ir, ic);
				}
				charge.at(ir + 1, m_month[month].ec_charge.ncols() + 1) = c_row_total;
				energy.at(ir + 1, m_month[month].ec_charge.ncols() + 1) = e_row_total;
				surplus.at(ir + 1, m_month[month].ec_charge.ncols() + 1) = s_row_total;
				c_total += c_row_total;
				e_total += e_row_total;
				s_total += s_row_total;
			}
			for (int ic = 0; ic < (int)m_month[month].ec_charge.ncols(); ic++)
			{
				ssc_number_t c_col_total = 0;
				ssc_number_t e_col_total = 0;
				ssc_number_t s_col_total = 0;
				for (int ir = 0; ir < (int)m_month[month].ec_charge.nrows(); ir++)
				{
					c_col_total += m_month[month].ec_charge.at(ir, ic);
					e_col_total += m_month[month].ec_energy_use.at(ir, ic);
					s_col_total += m_month[month].ec_energy_surplus.at(ir, ic);
				}
				charge.at(m_month[month].ec_charge.nrows() + 1, ic + 1) = c_col_total;
				energy.at(m_month[month].ec_energy_use.nrows() + 1, ic + 1) = e_col_total;
				surplus.at(m_month[month].ec_energy_surplus.nrows() + 1, ic + 1) = s_col_total;
			}
			charge.at(m_month[month].ec_charge.nrows() + 1, m_month[month].ec_charge.ncols() + 1) = c_total;
			energy.at(m_month[month].ec_energy_use.nrows() + 1, m_month[month].ec_energy_use.ncols() + 1) = e_total;
			surplus.at(m_month[month].ec_energy_surplus.nrows() + 1, m_month[month].ec_energy_surplus.ncols() + 1) = s_total;
		}

	}


};

DEFINE_MODULE_ENTRY( utilityrate5, "Complex utility rate structure net revenue calculator OpenEI Version 4 with net billing", 1 );


