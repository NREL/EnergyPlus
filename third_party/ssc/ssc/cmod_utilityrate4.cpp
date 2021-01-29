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
#include <lib_utility_rate_equations.h>
#include <sstream>


  
static var_info vtab_utility_rate4[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "", "*", "INTEGER,MIN=0,MAX=1", "" },

	// First year or lifetime hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INOUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "Time Series", "*", "", "" },
//	{ SSC_INPUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Time Series", "*", "", "" },
	{ SSC_INOUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Time Series", "*", "", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Financials", "*", "MIN=-99", "" },

	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual electricity rate escalation",  "%/year", "",                      "",             "?=0",                       "",                              "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_metering_option", "Metering options", "0=Single meter with monthly rollover credits in kWh,1=Single meter with monthly rollover credits in $,2=Single meter with no monthly rollover credits,3=Two meters with all generation sold and all load purchased", "Net metering monthly excess", "", "?=0", "INTEGER", "" },

	// 0 to match with 2015.1.30 release, 1 to use most common URDB kWh and 2 to use daily kWh 
//	{ SSC_INPUT, SSC_NUMBER, "ur_ec_hourly_acc_period", "Energy charge hourly reconciliation period", "0=hourly,1=monthly,2=daily", "Non-net metering hourly tier energy", "", "?=0", "INTEGER", "" },
	// 0 to use previous version sell rates and 1 to use single sell rate, namely flat sell rate
//	{ SSC_INPUT, SSC_NUMBER, "ur_ec_sell_rate_option", "Energy charge sell rate option", "0=Sell excess at energy charge sell rates,1=sell excess at specified sell rate", "Non-net metering sell rate", "", "?=0", "INTEGER", "" },

//	{ SSC_INPUT, SSC_NUMBER, "ur_ec_single_sell_rate", "Single TOU sell rate", "$/kWh", "", "", "?=0.0", "", "" },


	{ SSC_INPUT, SSC_NUMBER, "ur_nm_yearend_sell_rate", "Year end sell rate", "$/kWh", "", "", "?=0.0", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",            "$",      "",                      "",             "?=0.0",                     "",                              "" },


// optional input that allows sell rates to be overridden with buy rates - defaults to no override
	{ SSC_INPUT, SSC_NUMBER, "ur_sell_eq_buy", "Set sell rate equal to buy rate", "0/1", "Optional override", "", "?=0", "BOOLEAN", "" },



//	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_buy_rate",         "Flat rate (buy)",                 "$/kWh",  "",                      "",             "*",                         "",                              "" },
//	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_sell_rate",        "Flat rate (sell)",                "$/kWh",  "",                      "",             "?=0.0",                     "",      "" },

	// urdb minimums
	{ SSC_INPUT, SSC_NUMBER, "ur_monthly_min_charge", "Monthly minimum charge", "$", "", "", "?=0.0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_annual_min_charge", "Annual minimum charge", "$", "", "", "?=0.0", "", "" },



	// Energy Charge Inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekday", "Energy charge weekday schedule", "", "12x24", "", "*", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekend", "Energy charge weekend schedule", "", "12x24", "", "*", "", "" },

	// ur_ec_tou_mat has 6 columns period, tier, max usage, max usage units, buy rate, sell rate
	// replaces 12(P)*6(T)*(max usage+buy+sell) = 216 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_tou_mat", "Energy rates table", "", "", "", "*", "", "" },


	// Demand Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",            "Enable demand charge",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },
	// TOU demand charge
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demand charge weekday schedule", "", "12x24", "", "", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demand charge weekend schedule", "", "12x24", "", "", "", "" },

	// ur_dc_tou_mat has 4 columns period, tier, peak demand (kW), demand charge
	// replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_tou_mat", "Demand rates (TOU) table", "", "", "", "ur_dc_enable=1", "", "" },


	// flat demand charge
	// ur_dc_tou_flat has 4 columns month, tier, peak demand (kW), demand charge
	// replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_flat_mat", "Demand rates (flat) table", "", "", "", "ur_dc_enable=1", "", "" },
	

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
	{ SSC_OUTPUT, SSC_NUMBER, "savings_year1",                  "Electricity net savings with system (year 1)",             "$/yr",    "", "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "year1_electric_load",            "Electricity load total (year 1)",                "kWh/yr",  "", "Financial Metrics", "*", "", "" },



//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_grid",         "Year 1 electricity to/from grid",       "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_tofromgrid", "Electricity to/from grid", "kWh", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_togrid", "Electricity to grid", "kWh", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_fromgrid", "Electricity from grid", "kWh", "", "Time Series", "*", "LENGTH=8760", "" },
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_output",  "Year 1 hourly electricity from system",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_demand",       "Year 1 hourly electricity from grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_grid",    "Electricity to grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_load",    "Electricity from system to load",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_load", "Electricity load (year 1)", "kW", "", "Time Series", "*", "LENGTH=8760", "" },

// lifetime load (optional for lifetime analysis)
	{ SSC_OUTPUT, SSC_ARRAY, "lifetime_load", "Lifetime electricity load", "kW", "", "Time Series", "system_use_lifetime_output=1", "", "" },

//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_grid",         "Year 1 subhourly peak to/from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_tofromgrid",         "Electricity to/from grid peak", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_demand",       "Year 1 subhourly peak from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_system_to_load",         "Electricity peak from system to load", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
	

//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_revenue_with_system",     "Year 1 hourly sales/purchases with sytem",    "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_with_system",     "Electricity sales/purchases with sytem",    "$", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_payment_with_system",     "Year 1 hourly electricity purchases with system",    "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_income_with_system",      "Year 1 hourly electricity sales with system",     "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_price_with_system",       "Year 1 hourly energy charge with system",      "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_revenue_without_system",  "Year 1 hourly sales/purchases without sytem", "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_without_system",  "Electricity sales/purchases without sytem", "$", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_payment_without_system",  "Year 1 hourly electricity purchases without system", "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_income_without_system",   "Year 1 hourly electricity sales without system",  "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_price_without_system",    "Year 1 hourly energy charge without system",   "$", "",          "",             "*",                         "LENGTH=8760",                   "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_with_system", "Energy charge with system", "$", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_without_system", "Energy charge without system", "$", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_with_system", "Demand charge with system", "$", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_without_system", "Demand charge without system", "$", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_tou_schedule", "TOU period for energy charges", "", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_tou_schedule",       "TOU period for demand charges", "", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_peak_per_period",    "Electricity peak from grid per TOU period",        "kW", "", "Time Series", "*", "LENGTH=8760", "" },


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
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_w_system", "Peak demand with system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_wo_system", "Peak demand without system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },

// TODO - remove after testing
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_w_system", "Energy use with system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_wo_system", "Energy use without system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },

	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_system_generation",    "monthly system generation",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_to_grid",    "Electricity to/from grid",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_needed_from_grid",    "Electricity needed from grid",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_generation", "Net metering credit in kWh", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_dollars", "Net metering credit in $", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases", "Electricity sales/purchases with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases_wo_sys", "Electricity sales/purchases without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_w_sys", "Utility bill with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_wo_sys", "Utility bill without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },


	// convert annual outputs from Arrays to Matrices years x months
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_w_sys_ym", "Utility bill with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_wo_sys_ym", "Utility bill without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

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
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys", "Utility bill with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys", "Utility bill without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed", "Fixed monthly charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed", "Fixed monthly charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed", "Demand charge with system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed", "Demand charge without system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec", "Energy charge with system", "$", "", "Charges by Month", "*", "", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat", "Energy charge with system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec", "Energy charge without system", "$", "", "Charges by Month", "*", "", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat", "Energy charge without system (flat)", "$", "", "Charges by Month", "*", "", "" },






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



	var_info_invalid };

class cm_utilityrate4 : public compute_module
{
private:
	// schedule outputs
	std::vector<int> m_ec_tou_sched;
	std::vector<int> m_dc_tou_sched;
	std::vector<ur_month> m_month;
	std::vector<int> m_ec_periods; // period number
	// track initial values - may change based on units
	std::vector<std::vector<int> >  m_ec_periods_tiers_init; // tier numbers
	std::vector<int> m_dc_tou_periods; // period number
	std::vector<std::vector<int> >  m_dc_tou_periods_tiers; // tier numbers
	std::vector<std::vector<int> >  m_dc_flat_tiers; // tier numbers for each month of flat demand charge


public:
	cm_utilityrate4()
	{
		add_var_info( vtab_utility_rate4 );
	}

	void exec( )
	{
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


		// prepare 8760 arrays for load and grid values
		std::vector<ssc_number_t> e_sys(8760), p_sys(8760), e_sys_cy(8760), p_sys_cy(8760),
			e_load(8760), p_load(8760),
			e_grid(8760), p_grid(8760),
			e_load_cy(8760), p_load_cy(8760); // current year load (accounts for escal)
		



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
			throw exec_error("utilityrate4", util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;


		if (is_assigned("load"))
		{ // hourly or sub hourly loads for single year
			bload = true;
			pload = as_array("load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("utilityrate4", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
//			if (nrec_load != nrec_gen)
//				throw exec_error("utilityrate4", util::format("number of load records (%d) must be equal to number of gen records (%d)", (int)nrec_load, (int)nrec_gen));
		}
		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;


		// assign hourly values for utility rate calculations
		size_t idx = 0;
		ssc_number_t ts_power = 0;
		ssc_number_t ts_load = 0;
		ssc_number_t year1_elec_load = 0;
		for (i = 0; i < 8760; i++)
		{
			e_sys[i] = p_sys[i] = e_grid[i] = p_grid[i] = e_load[i] = p_load[i] = e_load_cy[i] = p_load_cy[i] = 0.0;
			for (size_t ii = 0; ii < step_per_hour_gen; ii++)
			{
				ts_power = pgen[idx];
				e_sys[i] += ts_power * ts_hour_gen;
				p_sys[i] = ((ts_power > p_sys[i]) ? ts_power : p_sys[i]);
				idx++;
			}
		}
		//load
		idx = 0;
		for (i = 0; i < 8760; i++)
		{
			for (size_t ii = 0; ii < step_per_hour_load; ii++)
			{
				ts_load = (bload ? pload[idx] : 0);
				e_load[i] += ts_load * ts_hour_load;
				p_load[i] = ((ts_load > p_load[i]) ? ts_load : p_load[i]);
				idx++;
			}
			year1_elec_load += e_load[i];
			// sign correction for utility rate calculations
			e_load[i] = -e_load[i];
			p_load[i] = -p_load[i];
		}

		assign("year1_electric_load", year1_elec_load);


		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(8760), revenue_wo_sys(8760),
			payment(8760), income(8760), 
			demand_charge_w_sys(8760), energy_charge_w_sys(8760),
			demand_charge_wo_sys(8760), energy_charge_wo_sys(8760),
			ec_tou_sched(8760), dc_tou_sched(8760), load(8760), dc_hourly_peak(8760),
			e_tofromgrid(8760), p_tofromgrid(8760),	salespurchases(8760);
		std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
			monthly_fixed_charges(12), monthly_minimum_charges(12),
			monthly_dc_fixed(12), monthly_dc_tou(12),
			monthly_ec_charges(12),
			//monthly_ec_flat_charges(12),
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
//		ssc_number_t *ch_w_sys_ec_flat_ym = allocate("charge_w_sys_ec_flat_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_dc_fixed_ym = allocate("charge_wo_sys_dc_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_dc_tou_ym = allocate("charge_wo_sys_dc_tou_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_ec_ym = allocate("charge_wo_sys_ec_ym", nyears + 1, 12);
//		ssc_number_t *ch_wo_sys_ec_flat_ym = allocate("charge_wo_sys_ec_flat_ym", nyears + 1, 12);
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
//		ssc_number_t *ch_w_sys_ec_flat = allocate("charge_w_sys_ec_flat", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed = allocate("charge_wo_sys_dc_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou = allocate("charge_wo_sys_dc_tou", nyears + 1);
		ssc_number_t *ch_wo_sys_ec = allocate("charge_wo_sys_ec", nyears + 1);
//		ssc_number_t *ch_wo_sys_ec_flat = allocate("charge_wo_sys_ec_flat", nyears + 1);
		ssc_number_t *ch_w_sys_fixed = allocate("charge_w_sys_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed = allocate("charge_wo_sys_fixed", nyears + 1);
		ssc_number_t *ch_w_sys_minimum = allocate("charge_w_sys_minimum", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum = allocate("charge_wo_sys_minimum", nyears + 1);


		// Enphase outputs requested - see emails 2/12/16- first year system to grid and from grid
		ssc_number_t *year1_hourly_e_togrid = allocate("year1_hourly_e_togrid", 8760);
		ssc_number_t *year1_hourly_e_fromgrid = allocate("year1_hourly_e_fromgrid", 8760);



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




		// lifetime hourly load
		ssc_number_t *lifetime_hourly_load = allocate("lifetime_load", nrec_gen);




		// false = 2 meters, load and system treated separately
		// true = 1 meter, net grid energy used for bill calculation with either energy or dollar rollover.
		//			bool enable_nm = as_boolean("ur_enable_net_metering");
		int metering_option = as_integer("ur_metering_option");
		bool enable_nm = (metering_option == 0 || metering_option == 1);
//		bool hourly_reconciliation = (metering_option == 3);
		bool hourly_reconciliation = (metering_option == 2); // per 2/25/16 meeting


		idx = 0;
		for (i=0;i<nyears;i++)
		{
			for (j=0;j<8760;j++)
			{
				/* for future implementation for lifetime loads
				// update e_load and p_load per year if lifetime output
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
				e_load_cy[j] = e_load[j] * load_scale[i];
				p_load_cy[j] = p_load[j] * load_scale[i];

				
				// update e_sys per year if lifetime output
				if ((as_integer("system_use_lifetime_output") == 1) )
				{
					e_sys[j] = p_sys[j] = 0.0;
					for (size_t ii = 0; (ii < step_per_hour_gen); ii++)
					{
						ts_power = pgen[idx];
						e_sys[j] += ts_power * ts_hour_gen;
						p_sys[j] = ((ts_power > p_sys[j]) ? ts_power : p_sys[j]);
						// until lifetime load fully implemented
						lifetime_hourly_load[idx] = -e_load_cy[j];
						idx++;
					}
				}

				// calculate e_grid value (e_sys + e_load)
				e_sys_cy[j] = e_sys[j] * sys_scale[i];
				p_sys_cy[j] = p_sys[j] * sys_scale[i];
				// note: load is assumed to have negative sign
				e_grid[j] = e_sys_cy[j] + e_load_cy[j];
				p_grid[j] = p_sys_cy[j] + p_load_cy[j];
			}

			// now calculate revenue without solar system (using load only)
			if (hourly_reconciliation)
			{
				ur_calc_hourly(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0], 
					&demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0],
					//&monthly_ec_flat_charges[0], 
					&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i]);
			}
			else
			{
				ur_calc(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0],
					&demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0], 
					//&monthly_ec_flat_charges[0], 
					&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i], i+1);
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

				//assign( "year1_hourly_revenue_without_system", var_data( &revenue_wo_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_without_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_without_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_without_system", var_data( &price[0], 8760 ) );
				assign("year1_hourly_dc_without_system", var_data(&demand_charge_wo_sys[0], 8760));
				assign("year1_hourly_ec_without_system", var_data(&energy_charge_wo_sys[0], 8760));

				assign( "year1_monthly_dc_fixed_without_system", var_data(&monthly_dc_fixed[0], 12) );
				assign( "year1_monthly_dc_tou_without_system", var_data(&monthly_dc_tou[0], 12) );
				assign("year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12));
//				assign("year1_monthly_ec_charge_flat_without_system", var_data(&monthly_ec_flat_charges[0], 12));

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii=0;ii<8760;ii++) 
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
							monthly_salespurchases[m] += salespurchases[c];
							c++;
						}
					}
				}
				assign( "year1_hourly_salespurchases_without_system", var_data( &salespurchases[0], 8760 ) );
//				assign("year1_monthly_salespurchases_wo_sys", var_data(&monthly_salespurchases[0], 12));
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

			if (hourly_reconciliation)
			{
				ur_calc_hourly(&e_grid[0], &p_grid[0],
					&revenue_w_sys[0], &payment[0], &income[0],
					&demand_charge_w_sys[0], &energy_charge_w_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0],
					//&monthly_ec_flat_charges[0], 
					&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], rate_scale[i]);
			}
			else // monthly reconciliation per 2015.6.30 release
			{
				if (enable_nm)
				{
					// calculate revenue with solar system (using net grid energy & maxpower)
					ur_calc(&e_grid[0], &p_grid[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0], 
						//&monthly_ec_flat_charges[0], 
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0],  rate_scale[i], i+1);
				}
				else
				{
					// calculate revenue with solar system (using system energy & maxpower)
					ur_calc(&e_sys_cy[0], &p_sys_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_dc_fixed[0], &monthly_dc_tou[0],
						&monthly_ec_charges[0], 
						//&monthly_ec_flat_charges[0],
						&dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0],  rate_scale[i], i+1, false, false,true);
					// TODO - remove annual_revenue and just use annual bill
					// Two meters - adjust output accordingly
					for (j = 0; j < 8760; j++)
					{
						revenue_w_sys[j] += revenue_wo_sys[j]; // watch sign
						annual_revenue_w_sys[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
						energy_charge_w_sys[j] += energy_charge_wo_sys[j];
						demand_charge_w_sys[j] += demand_charge_wo_sys[j];
					}
					// adjust monthly outputs as sum of both meters = system meter + load meter 

					for (j = 0; j < 12; j++)
					{
						monthly_dc_fixed[j] += ch_wo_sys_dc_fixed_ym[(i + 1) * 12 + j];
						monthly_dc_tou[j] += ch_wo_sys_dc_tou_ym[(i + 1) * 12 + j];
						monthly_ec_charges[j] += ch_wo_sys_ec_ym[(i + 1) * 12 + j];
						//monthly_ec_flat_charges[j] += ch_wo_sys_ec_flat_ym[(i + 1) * 12 + j];
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

					}
				} // non net metering with monthly reconciliation
			} // monthly reconciliation

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


				//assign( "year1_hourly_revenue_with_system", var_data( &revenue_w_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_with_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_with_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_with_system", var_data( &price[0], 8760 ) );
				assign("year1_hourly_dc_with_system", var_data(&demand_charge_w_sys[0], 8760));
				assign("year1_hourly_ec_with_system", var_data(&energy_charge_w_sys[0], 8760));
				//				assign( "year1_hourly_e_grid", var_data( &e_grid[0], 8760 ) );
				//				assign( "year1_hourly_p_grid", var_data( &p_grid[0], 8760 ) );
				assign("year1_hourly_dc_peak_per_period", var_data(&dc_hourly_peak[0], 8760));

				// sign reversal based on 9/5/13 meeting reverse again 9/6/13
				for (int ii = 0; ii<8760; ii++)
				{
					ec_tou_sched[ii] = (ssc_number_t)m_ec_tou_sched[ii];
					dc_tou_sched[ii] = (ssc_number_t)m_dc_tou_sched[ii];
					load[ii] = -e_load[ii];
					e_tofromgrid[ii] = e_grid[ii];
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
					p_tofromgrid[ii] = p_grid[ii];
					salespurchases[ii] = revenue_w_sys[ii];
				}
				assign("year1_hourly_ec_tou_schedule", var_data(&ec_tou_sched[0], 8760));
				assign("year1_hourly_dc_tou_schedule", var_data(&dc_tou_sched[0], 8760));
				// monthly outputs - Paul and Sean 7/29/13 - updated 8/9/13 and 8/12/13 and 9/10/13
				monthly_outputs(&e_load[0], &e_sys_cy[0], &e_grid[0], &salespurchases[0],
					&monthly_load[0], &monthly_system_generation[0], &monthly_elec_to_grid[0],
					&monthly_elec_needed_from_grid[0],
					&monthly_salespurchases[0]);

				assign("year1_hourly_e_tofromgrid", var_data(&e_tofromgrid[0], 8760));
				assign("year1_hourly_p_tofromgrid", var_data(&p_tofromgrid[0], 8760));
//				assign("year1_hourly_load", var_data(&load[0], 8760));
				assign("load", var_data(&load[0], 8760));
				assign("year1_hourly_salespurchases_with_system", var_data(&salespurchases[0], 8760));
				assign("year1_monthly_load", var_data(&monthly_load[0], 12));
				assign("year1_monthly_system_generation", var_data(&monthly_system_generation[0], 12));
				assign("year1_monthly_electricity_to_grid", var_data(&monthly_elec_to_grid[0], 12));
				assign("year1_monthly_electricity_needed_from_grid", var_data(&monthly_elec_needed_from_grid[0], 12));

				assign("year1_monthly_cumulative_excess_generation", var_data(&monthly_cumulative_excess_energy[0], 12));
				assign("year1_monthly_cumulative_excess_dollars", var_data(&monthly_cumulative_excess_dollars[0], 12));
//				assign("year1_monthly_salespurchases", var_data(&monthly_salespurchases[0], 12));
				assign("year1_monthly_utility_bill_w_sys", var_data(&monthly_bill[0], 12));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough electricity to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(8760), edemand(8760), pdemand(8760), e_sys_to_grid(8760), e_sys_to_load(8760), p_sys_to_load(8760);
				for (j = 0; j<8760; j++)
				{
					output[j] = e_sys_cy[j];
					edemand[j] = e_grid[j] < 0.0 ? -e_grid[j] : (ssc_number_t)0.0;
					pdemand[j] = p_grid[j] < 0.0 ? -p_grid[j] : (ssc_number_t)0.0;

					ssc_number_t sys_e_net = output[j] + e_load[j];// loads are assumed negative
					e_sys_to_grid[j] = sys_e_net > 0 ? sys_e_net : (ssc_number_t)0.0;
					e_sys_to_load[j] = sys_e_net > 0 ? -e_load[j] : output[j];

					ssc_number_t sys_p_net = output[j] + p_load[j];// loads are assumed negative
					p_sys_to_load[j] = sys_p_net > 0 ? -p_load[j] : output[j];
				}

				assign("year1_hourly_system_output", var_data(&output[0], 8760));
				assign("year1_hourly_e_demand", var_data(&edemand[0], 8760));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], 8760));

//				assign("year1_hourly_system_to_grid", var_data(&e_sys_to_grid[0], 8760));
				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], 8760));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], 8760));

				assign("year1_monthly_fixed_with_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_with_system", var_data(&monthly_minimum_charges[0], 12));
				assign("year1_monthly_dc_fixed_with_system", var_data(&monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
//				assign("year1_monthly_ec_charge_flat_with_system", var_data(&monthly_ec_flat_charges[0], 12));

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

			for(j=0;j<8760;j++)
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
//				ch_w_sys_ec_flat_ym[(i + 1) * 12 + j] = monthly_ec_flat_charges[j];
				ch_w_sys_fixed_ym[(i + 1) * 12 + j] = monthly_fixed_charges[j];
				ch_w_sys_minimum_ym[(i + 1) * 12 + j] = monthly_minimum_charges[j];

				utility_bill_w_sys[i + 1] += monthly_bill[j];
				ch_w_sys_dc_fixed[i + 1] += monthly_dc_fixed[j];
				ch_w_sys_dc_tou[i + 1] += monthly_dc_tou[j];
				ch_w_sys_ec[i + 1] += monthly_ec_charges[j];
//				ch_w_sys_ec_flat[i + 1] += monthly_ec_flat_charges[j];
				ch_w_sys_fixed[i + 1] += monthly_fixed_charges[j];
				ch_w_sys_minimum[i + 1] += monthly_minimum_charges[j];
			}


		}

		assign("elec_cost_with_system_year1", annual_elec_cost_w_sys[1]);
		assign("elec_cost_without_system_year1", annual_elec_cost_wo_sys[1]);
		assign("savings_year1", annual_elec_cost_wo_sys[1] - annual_elec_cost_w_sys[1]);
	}

	void monthly_outputs( ssc_number_t e_load[8760], ssc_number_t e_sys[8760], ssc_number_t e_grid[8760], ssc_number_t salespurchases[8760], ssc_number_t monthly_load[12], ssc_number_t monthly_generation[12], ssc_number_t monthly_elec_to_grid[12], ssc_number_t monthly_elec_needed_from_grid[12], ssc_number_t monthly_salespurchases[12])
	{
		// calculate the monthly net energy and monthly hours
		int m,h;
		size_t d;
		ssc_number_t energy_use[12]; // 12 months
		int c=0;
//		bool sell_eq_buy = as_boolean("ur_sell_eq_buy");
//			bool enable_nm = as_boolean("ur_enable_net_metering");
//		int metering_option = as_integer("ur_metering_option");
//		bool enable_nm = (metering_option == 0 || metering_option == 1);


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
		util::matrix_t<float> dc_schedwkday(12, 24, 1);
		util::matrix_t<float> dc_schedwkend(12, 24, 1);

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


//		bool ec_enabled = as_boolean("ur_ec_enable");
		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		// for reporting purposes
		for (i = 0; i < 8760; i++)
		{
			m_ec_tou_sched.push_back(1);
			m_dc_tou_sched.push_back(1);
		}
		if (ec_enabled)
		{

			ssc_number_t *ec_weekday = as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekday TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate4", ss.str());
			}
			ssc_number_t *ec_weekend = as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekend TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate4", ss.str());
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
				m_ec_tou_sched[i] = ec_tod[i];


			// 6 columns period, tier, max usage, max usage units, buy, sell
			ssc_number_t *ec_tou_in = as_matrix("ur_ec_tou_mat", &nrows, &ncols);
			if (ncols != 6)
			{
				std::ostringstream ss;
				ss << "The energy rate table must have 6 columns. Instead it has " << ncols << " columns.";
				throw exec_error("utilityrate4", ss.str());
			}
			util::matrix_t<double> ec_tou_mat(nrows, ncols);
			ec_tou_mat.assign(ec_tou_in, nrows, ncols);

			// adjust sell rate based on input selections
			// 0 = net metering energy rollover, 1=net metering dollar rollover
			// 2= non-net metering monthly, 3= non-net metering hourly

			// non net metering only
//			int ur_ec_sell_rate_option = as_integer("ur_ec_sell_rate_option");
			// 0=sell at ec sell rates, 1= sell at flat sell rate
//			bool ur_ec_sell_at_ec_rates = (ur_ec_sell_rate_option == 0);
//			ssc_number_t ur_ec_single_sell_rate = as_number("ur_ec_single_sell_rate");

//			bool sell_eq_buy = enable_nm;
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
					throw exec_error("utilityrate4", ss.str());
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
						throw exec_error("utilityrate4", ss.str());
					}
					period = (*per_num);
					int ndx = (int)(per_num - m_ec_periods.begin());
					if (i == 0)
					{
						// redimension ec_ field of ur_month class
						num_periods = (int)m_month[m].ec_periods.size();
						num_tiers = (int)m_ec_periods_tiers_init[ndx].size();
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
							throw exec_error("utilityrate4", ss.str());
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
//								else if (!ur_ec_sell_at_ec_rates)
//									sell = ur_ec_single_sell_rate;
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
				throw exec_error("utilityrate4", ss.str());
			}
			ssc_number_t *dc_weekend = as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "The weekend TOU matrix for demand rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
				throw exec_error("utilityrate4", ss.str());
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

			for (i = 0; i < 8760; i++)
				m_dc_tou_sched[i] = dc_tod[i];


			// 4 columns period, tier, max usage, charge
			ssc_number_t *dc_tou_in = as_matrix("ur_dc_tou_mat", &nrows, &ncols);
			if (ncols != 4)
			{
				std::ostringstream ss;
				ss << "The demand rate table for TOU periods must have 4 columns. Instead, it has " << ncols << "columns.";
				throw exec_error("utilityrate4", ss.str());
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
					throw exec_error("utilityrate4", ss.str());
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
				for (i = 0; i < m_month[m].dc_periods.size(); i++)
				{
					// find all periods and check that number of tiers the same for all for the month, if not through error
					std::vector<int>::iterator per_num = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), m_month[m].dc_periods[i]);
					if (per_num == m_dc_tou_periods.end())
					{
						std::ostringstream ss;
						ss << "Period " << m_month[m].dc_periods[i] << " is in Month " << m << " but is not defined in the demand rate table.  Rates for each period in the Weekday and Weekend schedules must be defined in the demand rate table.";
						throw exec_error("utilityrate4", ss.str());
					}
					period = (*per_num);
					int ndx = (int)(per_num - m_dc_tou_periods.begin());
					if (i == 0)
					{
						// redimension dc_ field of ur_month class
						num_periods = (int)m_month[m].dc_periods.size();
						num_tiers = (int)m_dc_tou_periods_tiers[ndx].size();
						m_month[m].dc_tou_ub.resize_fill(num_periods, num_tiers, (ssc_number_t)1e38);
						m_month[m].dc_tou_ch.resize_fill(num_periods, num_tiers, 0); // kWh
					}
					else
					{
						if ((int)m_dc_tou_periods_tiers[ndx].size() != num_tiers)
						{
							std::ostringstream ss;
							ss << "The number of tiers in the demand rate table, " << m_dc_tou_periods_tiers[ndx].size() << ", is incorrect for Month " << m << " and Period " << m_month[m].dc_periods[i] << ". The correct number of tiers for that month and period is " << num_tiers << ".";
							throw exec_error("utilityrate4", ss.str());
						}
					}
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
								m_month[m].dc_tou_ch.at(i, j) = dc_tou_mat.at(r, 3);//rate_esc;
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
					ss << "The demand rate table by month must have 4 columns. Instead it has " << ncols << " columns";
					throw exec_error("utilityrate4", ss.str());
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
						throw exec_error("utilityrate4", ss.str());
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




	void ur_calc( ssc_number_t e_in[8760], ssc_number_t p_in[8760],
		ssc_number_t revenue[8760], ssc_number_t payment[8760], ssc_number_t income[8760], 
		ssc_number_t demand_charge[8760], ssc_number_t energy_charge[8760],
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12], // ssc_number_t monthly_ec_flat_charges[12],
		ssc_number_t dc_hourly_peak[8760], ssc_number_t monthly_cumulative_excess_energy[12], 
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12], 
		ssc_number_t rate_esc, size_t year, bool include_fixed=true, bool include_min=true, bool gen_only=false) 

	{
		int i;

		for (i=0;i<8760;i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i=0;i<12;i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= monthly_dc_fixed[i] = monthly_dc_tou[i] 
				= monthly_ec_charges[i]
				= monthly_cumulative_excess_energy[i] 
				= monthly_cumulative_excess_dollars[i] 
				= monthly_bill[i] = 0.0;
		}
		// initialize all montly values
//		ssc_number_t buy = as_number("ur_flat_buy_rate")*rate_esc;
//		ssc_number_t sell = as_number("ur_flat_sell_rate")*rate_esc;

		//bool sell_eq_buy = as_boolean("ur_sell_eq_buy");

	

		// false = 2 meters, load and system treated separately
		// true = 1 meter, net grid energy used for bill calculation with either energy or dollar rollover.
//		bool enable_nm = as_boolean("ur_enable_net_metering");
		//			bool enable_nm = as_boolean("ur_enable_net_metering");
		int metering_option = as_integer("ur_metering_option");
		bool enable_nm = (metering_option == 0 || metering_option == 1);
		// 0 = net metering energy rollover, 1=net metering dollar rollover
		// 3= non-net metering monthly, 2= non-net metering hourly

		// non net metering only
//		int ur_ec_sell_rate_option = as_integer("ur_ec_sell_rate_option");
		// 0=sell at ec sell rates, 1= sell at flat sell rate
//		bool ur_ec_sell_at_ec_rates = (ur_ec_sell_rate_option==0);
//		ssc_number_t ur_ec_single_sell_rate = as_number("ur_ec_single_sell_rate")*rate_esc;

//		bool sell_eq_buy = enable_nm; // update from 6/25/15 meeting

//		bool ec_enabled = as_boolean("ur_ec_enable");
		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		//bool excess_monthly_dollars = (as_integer("ur_excess_monthly_energy_or_dollars") == 1);
		bool excess_monthly_dollars = (as_integer("ur_metering_option") == 1);
		//		bool apply_excess_to_flat_rate = !ec_enabled;

//		if (sell_eq_buy)
//			sell = buy;
//		else if (!ur_ec_sell_at_ec_rates)
//			sell = ur_ec_single_sell_rate*rate_esc;

		// calculate the monthly net energy and monthly hours
		int m, h, period, tier;
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

	
		// adjust net energy if net metering with monthly rollover
		if (enable_nm && !excess_monthly_dollars)
		{
			for (m = 1; m < (int)m_month.size(); m++)
			{
				if (m_month[m].energy_net < 0)
					m_month[m].energy_net += monthly_cumulative_excess_energy[m - 1];
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
						util::matrix_t<float> br(num_periods, num_tiers);
						util::matrix_t<float> sr(num_periods, num_tiers);
						util::matrix_t<float> ub(num_periods, num_tiers);
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
//				ssc_number_t ub_old = 0; // tier boundaries
//				ssc_number_t ub = 0; // tier boundaries
//				int tier_old = start_tier; // tier boundary
				if (m>0 && enable_nm && !excess_monthly_dollars)
				{
					mon_e_net = monthly_cumulative_excess_energy[m - 1]; // rollover
				}

				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						mon_e_net += e_in[c];
						int toup = m_ec_tou_sched[c];
						std::vector<int>::iterator per_num = std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), toup);
						if (per_num == m_month[m].ec_periods.end())
						{
							std::ostringstream ss;
							ss << "Energy rate TOU Period " << toup << " not found for Month " << util::schedule_int_to_month(m) << ".";
							throw exec_error("utilityrate4", ss.str());
						}
						int row = (int)(per_num - m_month[m].ec_periods.begin());
						// place all in tier 0 initially and then update appropriately
						// net energy per period per month
						m_month[m].ec_energy_use(row, 0) += e_in[c];
						c++;
					}
				}

				/*
				// rollover energy from correct period - based on matching period number
				if (m > 0 && enable_nm && !excess_monthly_dollars)
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
								ss << "utilityrate4: energy charge rollover for period " << toup << " not found for month " << m;
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
								ss << "year:" << year << " utilityrate4: Unable to determine period for energy charge rollover: Period " << toup_source << " does not exist for 12 am, 6 am, 12 pm or 6 pm in the previous month, which is Month " << util::schedule_int_to_month(m-1) << ".";
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
										ss << "year:" << year << "utilityrate4: Unable to determine period for energy charge rollover: Period " << toup_target << " does not exist for 12 am, 6 am, 12 pm or 6 pm in the current month, which is " << util::schedule_int_to_month(m) << ".";
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
						int todp = m_dc_tou_sched[c];
						std::vector<int>::iterator per_num = std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), todp);
						if (per_num == m_month[m].dc_periods.end())
						{
							std::ostringstream ss;
							ss << "Demand rate Period " << todp << " not found for Month " << m << ".";
							throw exec_error("utilityrate4", ss.str());
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
					if (d == util::nday[m] - 1 && h == 23)
					{
// energy charge
						if (ec_enabled)
						{
							// energy use and surplus distributed correctly above.
							// so calculate for all and not based on monthly net
							// addresses issue if net > 0 but one period net < 0
							//if (m_month[m].energy_net >= 0.0)
							{ // calculate income or credit
								ssc_number_t credit_amt = 0;
								for (period = 0; period < (int)m_month[m].ec_tou_sr.nrows(); period++)
								{
									for (tier = 0; tier < (int)m_month[m].ec_tou_sr.ncols(); tier++)
									{
										ssc_number_t cr = m_month[m].ec_energy_surplus.at(period, tier) * m_month[m].ec_tou_sr.at(period, tier) * rate_esc;
										
										if (!enable_nm)
										{
											credit_amt += cr;
											m_month[m].ec_charge.at(period, tier) = -cr;
										}
										else if (excess_monthly_dollars)
											monthly_cumulative_excess_dollars[m] += cr;
										
										/*
										if (!enable_nm || excess_monthly_dollars)
										{
											credit_amt += cr;
											if (!excess_monthly_dollars)
												m_month[m].ec_charge.at(period, tier) = -cr;
										}
										*/
									}
								}
								monthly_ec_charges[m] -= credit_amt;
							}
							//else
	
							{ // calculate payment or charge

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
							}


							// monthly rollover with year end sell at reduced rate
							if (enable_nm)
							{
								payment[c] += monthly_ec_charges[m];
								/*
								if (monthly_ec_charges[m] < 0)
								{
									monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
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
								demand = m_month[m].dc_tou_peak[period];
								// find tier corresponding to peak demand
								bool found = false;
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
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess dollars and rollover
			monthly_bill[m] = payment[c - 1] - income[c - 1];
			if (enable_nm)
			/*
			{
				
				// apply previous month rollover dollars
				if (m > 0)
					monthly_cumulative_excess_dollars[m] += monthly_cumulative_excess_dollars[m - 1];
				if (monthly_bill[m] > 0)
				{
					monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
					if (monthly_bill[m] < 0)
					{
						if (excess_monthly_dollars)
							monthly_cumulative_excess_dollars[m] = 0 - monthly_bill[m];
						monthly_bill[m] = 0;
						payment[c - 1] = 0; // fixed charges applied below
					}
				}
			}
			*/
			
			{
				// apply previous month rollover dollars
				if (m > 0)
				{
					monthly_bill[m] -= monthly_cumulative_excess_dollars[m - 1];
					payment[c - 1] -= monthly_cumulative_excess_dollars[m-1];
				}
				if (monthly_bill[m] < 0)
				{
					if (excess_monthly_dollars)
						monthly_cumulative_excess_dollars[m] -= monthly_bill[m];
					monthly_bill[m] = 0;
					payment[c - 1] = 0; // fixed charges applied below
				}
				else // apply current month rollover and adjust
				{
					monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
					if (monthly_bill[m] < 0)
					{
						if (excess_monthly_dollars)
							monthly_cumulative_excess_dollars[m] = -monthly_bill[m];
						monthly_bill[m] = 0;
						payment[c - 1] = 0; // fixed charges applied below
					}
					else
					{
						payment[c - 1] -= monthly_cumulative_excess_dollars[m];
						monthly_cumulative_excess_dollars[m] = 0;
					}
				}
			}
			
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
					if (d == util::nday[m] - 1 && h == 23)
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
									income[8759] += monthly_cumulative_excess_energy[11] * as_number("ur_nm_yearend_sell_rate")*rate_esc;
								else if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[11] > 0))
									income[8759] += monthly_cumulative_excess_dollars[11];
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



	// will be used for non net metering case to match 2015.1.30 release
	void ur_calc_hourly(ssc_number_t e_in[8760], ssc_number_t p_in[8760],
		ssc_number_t revenue[8760], ssc_number_t payment[8760], ssc_number_t income[8760],
		ssc_number_t demand_charge[8760],
		ssc_number_t energy_charge[8760],
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12], //ssc_number_t monthly_ec_flat_charges[12],
		ssc_number_t dc_hourly_peak[8760], ssc_number_t monthly_cumulative_excess_energy[12],
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12],
		ssc_number_t rate_esc, bool include_fixed = true, bool include_min = true)

	{
		int i;

		for (i = 0; i<8760; i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i = 0; i<12; i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= monthly_dc_fixed[i] = monthly_dc_tou[i]
				= monthly_ec_charges[i] 
				= monthly_cumulative_excess_energy[i]
				= monthly_cumulative_excess_dollars[i]
				= monthly_bill[i] = 0.0;
		}
		// initialize all montly values
		//ssc_number_t buy = as_number("ur_flat_buy_rate")*rate_esc;
		//ssc_number_t sell = as_number("ur_flat_sell_rate")*rate_esc;


		// non net metering only
		//int ur_ec_sell_rate_option = as_integer("ur_ec_sell_rate_option");
		// 0=sell at ec sell rates, 1= sell at flat sell rate
		//bool ur_ec_sell_at_ec_rates = (ur_ec_sell_rate_option == 0);
		//ssc_number_t ur_ec_single_sell_rate = as_number("ur_ec_single_sell_rate")*rate_esc;

		 
		// 0=hourly (match with 2015.1.30 release, 1=monthly (most common unit in URDB), 2=daily (used for PG&E baseline rates). Currently hidden in UI and set to zero
//		int ur_ec_hourly_acc_period = as_integer("ur_ec_hourly_acc_period");
		int ur_ec_hourly_acc_period = 1; // monthly per 2/25/16 meeting
		// single meter so single net accumulation
		double daily_surplus_energy;
		double monthly_surplus_energy; 
		double daily_deficit_energy; 
		double monthly_deficit_energy;

//		bool ec_enabled = as_boolean("ur_ec_enable");
		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");


		//if (!ur_ec_sell_at_ec_rates)
		//	sell = ur_ec_single_sell_rate*rate_esc;

		// calculate the monthly net energy and monthly hours
		int m, h, period, tier;
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



		if (ec_enabled)
		{
			// calculate the monthly net energy per tier and period based on units
			c = 0;
			for (m = 0; m < (int)m_month.size(); m++)
			{
				int start_tier = 0;
				int end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
				int num_periods = (int)m_month[m].ec_tou_ub.nrows();
				int num_tiers = end_tier - start_tier + 1;

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
					util::matrix_t<float> br(num_periods, num_tiers);
					util::matrix_t<float> sr(num_periods, num_tiers);
					util::matrix_t<float> ub(num_periods, num_tiers);
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
						int todp = m_dc_tou_sched[c];
						std::vector<int>::iterator per_num = std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), todp);
						if (per_num == m_month[m].dc_periods.end())
						{
							std::ostringstream ss;
							ss << "Demand charge Period " << todp << " not found for Month " << m << ".";
							throw exec_error("utilityrate4", ss.str());
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






// main loop
		c = 0; // hourly count
		// process one hour at a time
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
							throw exec_error("utilityrate4", ss.str());
						}
						int row = (int)(per_num - m_month[m].ec_periods.begin());

						if (e_in[c] >= 0.0)
						{ // calculate income or credit
							monthly_surplus_energy += e_in[c];
							daily_surplus_energy += e_in[c];

							// base period charge on units specified
							double energy_surplus = e_in[c];
							double cumulative_energy = e_in[c];
							if (ur_ec_hourly_acc_period == 1)
								cumulative_energy = monthly_surplus_energy;
							else if (ur_ec_hourly_acc_period == 2)
								cumulative_energy = daily_surplus_energy;


							// cumulative energy used to determine tier for credit of entire surplus amount
							double credit_amt = 0;
							for (tier = 0; tier < (int)m_month[m].ec_tou_ub.ncols(); tier++)
							{
								double e_upper = m_month[m].ec_tou_ub.at(row, tier);
								if (cumulative_energy < e_upper)
									break;
							}
							if (tier >= (int)m_month[m].ec_tou_ub.ncols())
								tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
							double tier_energy = energy_surplus;
							double tier_credit = tier_energy * m_month[m].ec_tou_sr.at(row, tier) * rate_esc;
							credit_amt += tier_credit;
							m_month[m].ec_charge.at(row, tier) -= (ssc_number_t)tier_credit;
							m_month[m].ec_energy_surplus.at(row, tier) += (ssc_number_t)tier_energy;

							income[c] += (ssc_number_t)credit_amt;
							monthly_ec_charges[m] -= (ssc_number_t)credit_amt;
							energy_charge[c] -= (ssc_number_t)credit_amt;
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
							charge_amt += tier_charge;
							m_month[m].ec_energy_use.at(row, tier) += (ssc_number_t)tier_energy;
							m_month[m].ec_charge.at(row, tier) += (ssc_number_t)tier_charge;

							payment[c] += (ssc_number_t)charge_amt;
							monthly_ec_charges[m] += (ssc_number_t)charge_amt;
							energy_charge[c] += (ssc_number_t)charge_amt;
						}
					}
					// end of energy charge


					// demand charge - end of month only
					if (d == util::nday[m] - 1 && h == 23)
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
								demand = m_month[m].dc_tou_peak[period];
								// find tier corresponding to peak demand
								bool found = false;
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
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess dollars and rollover
//			monthly_bill[m] = monthly_ec_flat_charges[m] + monthly_ec_charges[m] + monthly_dc_fixed[m] + monthly_dc_tou[m];
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
					if (d == util::nday[m] - 1 && h == 23)
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
						}
						monthly_bill[m] += monthly_fixed_charges[m] + monthly_minimum_charges[m];
					}
					revenue[c] = income[c] - payment[c];
					c++;
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
			throw exec_error("utilityrate4", ss.str());
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
				throw exec_error("utilityrate4", ss.str());
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
			float c_total = 0;
			float e_total = 0;
			float s_total = 0;
			for (int ir = 0; ir < (int)m_month[month].ec_charge.nrows(); ir++)
			{
				float c_row_total = 0;
				float e_row_total = 0;
				float s_row_total = 0;
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
				float c_col_total = 0;
				float e_col_total = 0;
				float s_col_total = 0;
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

DEFINE_MODULE_ENTRY( utilityrate4, "Complex utility rate structure net revenue calculator OpenEI Version 4", 1 );


