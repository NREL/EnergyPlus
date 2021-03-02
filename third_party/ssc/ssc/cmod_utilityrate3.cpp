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
#include <sstream>



static var_info vtab_utility_rate3[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "", "*", "INTEGER,MIN=0,MAX=1", "" },

	/* change to load, grid and gen per 4/9/15 meeting
	{ SSC_INPUT,        SSC_ARRAY,      "hourly_energy",            "Energy at grid with system",                "kWh",    "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_INPUT,        SSC_ARRAY,      "p_with_system",            "Max power at grid with system",                 "kW",     "",                      "",             "?",                         "LENGTH=8760",                   "" },
	{ SSC_INPUT, SSC_ARRAY, "e_load", "Energy at grid without system (load only)", "kWh", "", "", "?", "LENGTH=8760", "" },
	{ SSC_INPUT, SSC_ARRAY, "p_load", "Max power at grid without system (load only)", "kW", "", "", "?", "LENGTH=8760", "" },
	*/
	// First year hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INPUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "Time Series", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Time Series", "*", "", "" },
	// optional
//	{ SSC_INPUT, SSC_ARRAY, "grid", "System power delivered to grid", "kW", "", "Time Series", "", "", "" },

// 4/16/15 meeting update
// load can be subhourly but hourly_grid is hourly from performance models.
//	{ SSC_INPUT, SSC_ARRAY, "load", "Electric load", "kW", "", "Time Series", "*", "", "" },
//	{ SSC_INPUT, SSC_ARRAY, "hourly_grid", "Net grid power", "kW", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Financials", "*", "MIN=-99", "" },

	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual utility rate escalation",  "%/year", "",                      "",             "?=0",                       "",                              "" },
	
	{ SSC_INPUT, SSC_NUMBER, "ur_enable_net_metering", "Enable net metering", "0/1", "Enforce net metering", "", "?=1", "BOOLEAN", "" },
//	{ SSC_INPUT, SSC_NUMBER, "ur_sell_eq_buy", "Set sell rate equal to buy rate", "0/1", "Retail and wholesale rates", "", "?=1", "BOOLEAN", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_excess_monthly_energy_or_dollars", "Net metering handling of monthly excess", "0=Rollover energy,1=Rollover dollars", "Net metering monthly excess", "", "?=0", "INTEGER", "" },

	{ SSC_INPUT, SSC_NUMBER, "ur_nm_yearend_sell_rate", "Year end sell rate", "$/kWh", "", "", "?=0.0", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",            "$",      "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_buy_rate",         "Flat rate (buy)",                 "$/kWh",  "",                      "",             "*",                         "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_sell_rate",        "Flat rate (sell)",                "$/kWh",  "",                      "",             "?=0.0",                     "",      "" },

	// urdb minimums
	{ SSC_INPUT, SSC_NUMBER, "ur_monthly_min_charge", "Monthly minimum charge", "$", "", "", "?=0.0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_annual_min_charge", "Annual minimum charge", "$", "", "", "?=0.0", "", "" },



	// Energy Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_enable",            "Enable energy charge",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },

	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekday", "Energy Charge Weekday Schedule", "", "12x24", "", "ur_ec_enable=1", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekend", "Energy Charge Weekend Schedule", "", "12x24", "", "ur_ec_enable=1", "", "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_br",       "Period 1 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_sr",       "Period 1 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_ub",       "Period 1 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_br",       "Period 1 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_sr",       "Period 1 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_ub",       "Period 1 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_br",       "Period 1 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_sr",       "Period 1 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_ub",       "Period 1 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_br",       "Period 1 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_sr",       "Period 1 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_ub",       "Period 1 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_br",       "Period 1 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_sr",       "Period 1 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_ub",       "Period 1 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_br",       "Period 1 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_sr",       "Period 1 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_ub",       "Period 1 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_br",       "Period 2 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_sr",       "Period 2 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_ub",       "Period 2 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_br",       "Period 2 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_sr",       "Period 2 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_ub",       "Period 2 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_br",       "Period 2 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_sr",       "Period 2 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_ub",       "Period 2 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_br",       "Period 2 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_sr",       "Period 2 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_ub",       "Period 2 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_br",       "Period 2 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_sr",       "Period 2 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_ub",       "Period 2 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_br",       "Period 2 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_sr",       "Period 2 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_ub",       "Period 2 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_br",       "Period 3 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_sr",       "Period 3 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_ub",       "Period 3 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_br",       "Period 3 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_sr",       "Period 3 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_ub",       "Period 3 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_br",       "Period 3 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_sr",       "Period 3 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_ub",       "Period 3 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_br",       "Period 3 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_sr",       "Period 3 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_ub",       "Period 3 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_br",       "Period 3 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_sr",       "Period 3 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_ub",       "Period 3 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_br",       "Period 3 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_sr",       "Period 3 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_ub",       "Period 3 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_br",       "Period 4 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_sr",       "Period 4 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_ub",       "Period 4 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_br",       "Period 4 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_sr",       "Period 4 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_ub",       "Period 4 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_br",       "Period 4 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_sr",       "Period 4 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_ub",       "Period 4 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_br",       "Period 4 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_sr",       "Period 4 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_ub",       "Period 4 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_br",       "Period 4 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_sr",       "Period 4 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_ub",       "Period 4 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_br",       "Period 4 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_sr",       "Period 4 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_ub",       "Period 4 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_br",       "Period 5 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_sr",       "Period 5 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_ub",       "Period 5 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_br",       "Period 5 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_sr",       "Period 5 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_ub",       "Period 5 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_br",       "Period 5 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_sr",       "Period 5 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_ub",       "Period 5 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_br",       "Period 5 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_sr",       "Period 5 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_ub",       "Period 5 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_br",       "Period 5 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_sr",       "Period 5 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_ub",       "Period 5 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_br",       "Period 5 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_sr",       "Period 5 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_ub",       "Period 5 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_br",       "Period 6 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_sr",       "Period 6 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_ub",       "Period 6 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_br",       "Period 6 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_sr",       "Period 6 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_ub",       "Period 6 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_br",       "Period 6 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_sr",       "Period 6 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_ub",       "Period 6 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_br",       "Period 6 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_sr",       "Period 6 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_ub",       "Period 6 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_br",       "Period 6 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_sr",       "Period 6 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_ub",       "Period 6 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_br",       "Period 6 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_sr",       "Period 6 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_ub",       "Period 6 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_br",       "Period 7 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_sr",       "Period 7 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_ub",       "Period 7 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_br",       "Period 7 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_sr",       "Period 7 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_ub",       "Period 7 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_br",       "Period 7 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_sr",       "Period 7 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_ub",       "Period 7 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_br",       "Period 7 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_sr",       "Period 7 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_ub",       "Period 7 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_br",       "Period 7 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_sr",       "Period 7 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_ub",       "Period 7 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_br",       "Period 7 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_sr",       "Period 7 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_ub",       "Period 7 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_br",       "Period 8 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_sr",       "Period 8 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_ub",       "Period 8 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_br",       "Period 8 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_sr",       "Period 8 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_ub",       "Period 8 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_br",       "Period 8 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_sr",       "Period 8 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_ub",       "Period 8 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_br",       "Period 8 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_sr",       "Period 8 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_ub",       "Period 8 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_br",       "Period 8 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_sr",       "Period 8 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_ub",       "Period 8 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_br",       "Period 8 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_sr",       "Period 8 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_ub",       "Period 8 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_br",       "Period 9 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_sr",       "Period 9 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_ub",       "Period 9 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_br",       "Period 9 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_sr",       "Period 9 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_ub",       "Period 9 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_br",       "Period 9 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_sr",       "Period 9 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_ub",       "Period 9 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_br",       "Period 9 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_sr",       "Period 9 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_ub",       "Period 9 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_br",       "Period 9 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_sr",       "Period 9 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_ub",       "Period 9 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_br",       "Period 9 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_sr",       "Period 9 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_ub",       "Period 9 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_br",       "Period 10 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_sr",       "Period 10 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_ub",       "Period 10 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_br",       "Period 10 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_sr",       "Period 10 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_ub",       "Period 10 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_br",       "Period 10 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_sr",       "Period 10 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_ub",       "Period 10 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_br",       "Period 10 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_sr",       "Period 10 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_ub",       "Period 10 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_br",       "Period 10 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_sr",       "Period 10 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_ub",       "Period 10 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_br",       "Period 10 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_sr",       "Period 10 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_ub",       "Period 10 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_br",       "Period 11 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_sr",       "Period 11 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_ub",       "Period 11 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_br",       "Period 11 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_sr",       "Period 11 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_ub",       "Period 11 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_br",       "Period 11 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_sr",       "Period 11 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_ub",       "Period 11 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_br",       "Period 11 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_sr",       "Period 11 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_ub",       "Period 11 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_br",       "Period 11 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_sr",       "Period 11 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_ub",       "Period 11 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_br",       "Period 11 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_sr",       "Period 11 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_ub",       "Period 11 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_br",       "Period 12 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_sr",       "Period 12 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_ub",       "Period 12 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_br",       "Period 12 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_sr",       "Period 12 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_ub",       "Period 12 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_br",       "Period 12 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_sr",       "Period 12 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_ub",       "Period 12 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_br",       "Period 12 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_sr",       "Period 12 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_ub",       "Period 12 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_br",       "Period 12 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_sr",       "Period 12 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_ub",       "Period 12 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_br",       "Period 12 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_sr",       "Period 12 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_ub",       "Period 12 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },



	// Demand Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",            "Enable Demand Charge",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },

//	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demend Charge Weekday Schedule", "", "12x24", "", "ur_dc_enable=1", "", "" },
//	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demend Charge Weekend Schedule", "", "12x24", "", "ur_dc_enable=1", "", "" },
// optional input for flat monthly demand charge per email from Mike Gleason 1/16/15
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demend Charge Weekday Schedule", "", "12x24", "", "", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demend Charge Weekend Schedule", "", "12x24", "", "", "", "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t1_dc",       "Period 1 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t1_ub",       "Period 1 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t2_dc",       "Period 1 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t2_ub",       "Period 1 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t3_dc",       "Period 1 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t3_ub",       "Period 1 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t4_dc",       "Period 1 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t4_ub",       "Period 1 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t5_dc",       "Period 1 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t5_ub",       "Period 1 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t6_dc",       "Period 1 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t6_ub",       "Period 1 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t1_dc",       "Period 2 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t1_ub",       "Period 2 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t2_dc",       "Period 2 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t2_ub",       "Period 2 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t3_dc",       "Period 2 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t3_ub",       "Period 2 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t4_dc",       "Period 2 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t4_ub",       "Period 2 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t5_dc",       "Period 2 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t5_ub",       "Period 2 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t6_dc",       "Period 2 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t6_ub",       "Period 2 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t1_dc",       "Period 3 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t1_ub",       "Period 3 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t2_dc",       "Period 3 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t2_ub",       "Period 3 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t3_dc",       "Period 3 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t3_ub",       "Period 3 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t4_dc",       "Period 3 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t4_ub",       "Period 3 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t5_dc",       "Period 3 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t5_ub",       "Period 3 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t6_dc",       "Period 3 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t6_ub",       "Period 3 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t1_dc",       "Period 4 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t1_ub",       "Period 4 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t2_dc",       "Period 4 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t2_ub",       "Period 4 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t3_dc",       "Period 4 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t3_ub",       "Period 4 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t4_dc",       "Period 4 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t4_ub",       "Period 4 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t5_dc",       "Period 4 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t5_ub",       "Period 4 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t6_dc",       "Period 4 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t6_ub",       "Period 4 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t1_dc",       "Period 5 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t1_ub",       "Period 5 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t2_dc",       "Period 5 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t2_ub",       "Period 5 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t3_dc",       "Period 5 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t3_ub",       "Period 5 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t4_dc",       "Period 5 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t4_ub",       "Period 5 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t5_dc",       "Period 5 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t5_ub",       "Period 5 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t6_dc",       "Period 5 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t6_ub",       "Period 5 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t1_dc",       "Period 6 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t1_ub",       "Period 6 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t2_dc",       "Period 6 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t2_ub",       "Period 6 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t3_dc",       "Period 6 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t3_ub",       "Period 6 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t4_dc",       "Period 6 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t4_ub",       "Period 6 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t5_dc",       "Period 6 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t5_ub",       "Period 6 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t6_dc",       "Period 6 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t6_ub",       "Period 6 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t1_dc",       "Period 7 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t1_ub",       "Period 7 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t2_dc",       "Period 7 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t2_ub",       "Period 7 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t3_dc",       "Period 7 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t3_ub",       "Period 7 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t4_dc",       "Period 7 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t4_ub",       "Period 7 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t5_dc",       "Period 7 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t5_ub",       "Period 7 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t6_dc",       "Period 7 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t6_ub",       "Period 7 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t1_dc",       "Period 8 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t1_ub",       "Period 8 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t2_dc",       "Period 8 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t2_ub",       "Period 8 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t3_dc",       "Period 8 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t3_ub",       "Period 8 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t4_dc",       "Period 8 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t4_ub",       "Period 8 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t5_dc",       "Period 8 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t5_ub",       "Period 8 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t6_dc",       "Period 8 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t6_ub",       "Period 8 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t1_dc",       "Period 9 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t1_ub",       "Period 9 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t2_dc",       "Period 9 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t2_ub",       "Period 9 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t3_dc",       "Period 9 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t3_ub",       "Period 9 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t4_dc",       "Period 9 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t4_ub",       "Period 9 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t5_dc",       "Period 9 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t5_ub",       "Period 9 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t6_dc",       "Period 9 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t6_ub",       "Period 9 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t1_dc",       "Period 10 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t1_ub",       "Period 10 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t2_dc",       "Period 10 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t2_ub",       "Period 10 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t3_dc",       "Period 10 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t3_ub",       "Period 10 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t4_dc",       "Period 10 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t4_ub",       "Period 10 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t5_dc",       "Period 10 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t5_ub",       "Period 10 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t6_dc",       "Period 10 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t6_ub",       "Period 10 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t1_dc",       "Period 11 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t1_ub",       "Period 11 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t2_dc",       "Period 11 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t2_ub",       "Period 11 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t3_dc",       "Period 11 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t3_ub",       "Period 11 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t4_dc",       "Period 11 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t4_ub",       "Period 11 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t5_dc",       "Period 11 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t5_ub",       "Period 11 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t6_dc",       "Period 11 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t6_ub",       "Period 11 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t1_dc",       "Period 12 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t1_ub",       "Period 12 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t2_dc",       "Period 12 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t2_ub",       "Period 12 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t3_dc",       "Period 12 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t3_ub",       "Period 12 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t4_dc",       "Period 12 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t4_ub",       "Period 12 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t5_dc",       "Period 12 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t5_ub",       "Period 12 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t6_dc",       "Period 12 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t6_ub",       "Period 12 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t1_dc",       "January Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t1_ub",       "January Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t2_dc",       "January Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t2_ub",       "January Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t3_dc",       "January Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t3_ub",       "January Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t4_dc",       "January Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t4_ub",       "January Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t5_dc",       "January Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t5_ub",       "January Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t6_dc",       "January Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t6_ub",       "January Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t1_dc",       "February Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t1_ub",       "February Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t2_dc",       "February Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t2_ub",       "February Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t3_dc",       "February Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t3_ub",       "February Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t4_dc",       "February Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t4_ub",       "February Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t5_dc",       "February Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t5_ub",       "February Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t6_dc",       "February Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t6_ub",       "February Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t1_dc",       "March Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t1_ub",       "March Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t2_dc",       "March Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t2_ub",       "March Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t3_dc",       "March Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t3_ub",       "March Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t4_dc",       "March Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t4_ub",       "March Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t5_dc",       "March Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t5_ub",       "March Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t6_dc",       "March Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t6_ub",       "March Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t1_dc",       "April Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t1_ub",       "April Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t2_dc",       "April Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t2_ub",       "April Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t3_dc",       "April Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t3_ub",       "April Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t4_dc",       "April Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t4_ub",       "April Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t5_dc",       "April Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t5_ub",       "April Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t6_dc",       "April Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t6_ub",       "April Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t1_dc",       "May Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t1_ub",       "May Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t2_dc",       "May Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t2_ub",       "May Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t3_dc",       "May Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t3_ub",       "May Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t4_dc",       "May Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t4_ub",       "May Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t5_dc",       "May Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t5_ub",       "May Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t6_dc",       "May Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t6_ub",       "May Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t1_dc",       "June Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t1_ub",       "June Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t2_dc",       "June Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t2_ub",       "June Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t3_dc",       "June Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t3_ub",       "June Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t4_dc",       "June Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t4_ub",       "June Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t5_dc",       "June Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t5_ub",       "June Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t6_dc",       "June Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t6_ub",       "June Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t1_dc",       "July Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t1_ub",       "July Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t2_dc",       "July Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t2_ub",       "July Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t3_dc",       "July Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t3_ub",       "July Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t4_dc",       "July Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t4_ub",       "July Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t5_dc",       "July Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t5_ub",       "July Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t6_dc",       "July Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t6_ub",       "July Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t1_dc",       "August Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t1_ub",       "August Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t2_dc",       "August Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t2_ub",       "August Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t3_dc",       "August Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t3_ub",       "August Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t4_dc",       "August Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t4_ub",       "August Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t5_dc",       "August Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t5_ub",       "August Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t6_dc",       "August Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t6_ub",       "August Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t1_dc",       "September Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t1_ub",       "September Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t2_dc",       "September Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t2_ub",       "September Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t3_dc",       "September Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t3_ub",       "September Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t4_dc",       "September Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t4_ub",       "September Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t5_dc",       "September Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t5_ub",       "September Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t6_dc",       "September Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t6_ub",       "September Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t1_dc",       "October Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t1_ub",       "October Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t2_dc",       "October Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t2_ub",       "October Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t3_dc",       "October Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t3_ub",       "October Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t4_dc",       "October Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t4_ub",       "October Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t5_dc",       "October Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t5_ub",       "October Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t6_dc",       "October Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t6_ub",       "October Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t1_dc",       "November Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t1_ub",       "November Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t2_dc",       "November Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t2_ub",       "November Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t3_dc",       "November Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t3_ub",       "November Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t4_dc",       "November Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t4_ub",       "November Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t5_dc",       "November Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t5_ub",       "November Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t6_dc",       "November Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t6_ub",       "November Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t1_dc",       "December Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t1_ub",       "December Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t2_dc",       "December Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t2_ub",       "December Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t3_dc",       "December Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t3_ub",       "December Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t4_dc",       "December Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t4_ub",       "December Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t5_dc",       "December Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t5_ub",       "December Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t6_dc",       "December Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t6_ub",       "December Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },


	
	// outputs
//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_value",             "Energy value in each year",     "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_energy_value",             "Energy value in each year",     "$",    "",                      "Annual",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_electric_load",            "Total electric load in each year",  "kWh",    "",                      "Annual",             "*",                         "",   "" },

	// use output from annualoutput not scaled output from here
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_net",               "Energy in each year",           "kW",   "",                      "",             "*",                         "",   "" },


		// outputs from Paul, Nate and Sean 9/9/13
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_with_system",      "Total revenue with system",         "$",    "",                      "",             "*",                         "",   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_without_system",   "Total revenue without system",      "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_with_system",    "Electricity cost with system",    "$/yr", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_without_system", "Electricity cost without system", "$/yr", "", "Annual", "*", "", "" },

	// year 1 values for metrics
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_with_system_year1",    "Electricity cost with system",    "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_without_system_year1", "Electricity cost without system", "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "savings_year1",                  "Electricity savings",             "$/yr",    "", "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "year1_electric_load",            "Electricity load",                "kWh/yr",  "", "Financial Metrics", "*", "", "" },



//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_grid",         "Year 1 electricity to/from grid",       "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_tofromgrid",         "Electricity to/from grid",       "kWh", "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_output",  "Year 1 hourly electricity from system",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_demand",       "Year 1 hourly electricity from grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_grid",    "Year 1 hourly electricity to grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_load",    "Year 1 hourly system electricity to load",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_load", "Electricity load (year 1)", "kW", "", "Time Series", "*", "LENGTH=8760", "" },

// lifetime load (optional for lifetime analysis)
	{ SSC_OUTPUT, SSC_ARRAY, "lifetime_load", "Lifetime electricity load", "kW", "", "Time Series", "system_use_lifetime_output=1", "", "" },

//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_grid",         "Year 1 subhourly peak to/from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_tofromgrid",         "Electricity to/from grid peak", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_demand",       "Year 1 subhourly peak from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_system_to_load",         "Electricity peak load met by system", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
	

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

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_with_system", "Electricity energy charge with system", "$", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_without_system", "Electricity energy charge without system", "$", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_with_system", "Electricity demand charge with system", "$", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_without_system", "Electricity demand charge without system", "$", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_tou_schedule", "Electricity TOU period for energy charges", "", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_tou_schedule",       "Electricity TOU period for demand charges", "", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_peak_per_period",    "Electricity from grid peak per TOU period",        "kW", "", "Time Series", "*", "LENGTH=8760", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_with_system", "Electricity charge (fixed) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_without_system", "Electricity charge (fixed) without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_with_system", "Electricity charge (minimum) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_without_system", "Electricity charge (minimum) without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_fixed_with_system", "Electricity demand charge (fixed) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_tou_with_system", "Electricity demand charge (TOU) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_with_system", "Electricity energy charge (TOU) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_flat_with_system", "Electricity energy charge (flat) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_with_system",       "Year 1 monthly energy rate with system",              "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_fixed_without_system",   "Electricity demand charge (fixed) without system", "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_tou_without_system",     "Electricity demand charge (TOU) without system",   "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_without_system", "Electricity energy charge (TOU) without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_flat_without_system", "Electricity energy charge (flat) without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_without_system",    "Year 1 monthly energy rate without system",           "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },


	// monthly outputs from Sean 7/29/13 "Net Metering Accounting.xlsx" updates from Paul and Sean 8/9/13 and 8/12/13
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_load",    "Electricity load",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_system_generation",    "monthly system generation",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_to_grid",    "Electricity to/from grid",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_needed_from_grid",    "Electricity needed from grid",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_generation", "Electricity net metering credit", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_dollars", "Dollar net metering credit", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases", "Electricity sales/purchases with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
//	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases_wo_sys", "Electricity sales/purchases without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_w_sys", "Utility bill with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_wo_sys", "Utility bill without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_jan", "Utility bill with system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_feb", "Utility bill with system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_mar", "Utility bill with system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_apr", "Utility bill with system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_may", "Utility bill with system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_jun", "Utility bill with system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_jul", "Utility bill with system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_aug", "Utility bill with system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_sep", "Utility bill with system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_oct", "Utility bill with system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_nov", "Utility bill with system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys_dec", "Utility bill with system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys", "Utility bill with system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_jan", "Utility bill without system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_feb", "Utility bill without system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_mar", "Utility bill without system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_apr", "Utility bill without system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_may", "Utility bill without system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_jun", "Utility bill without system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_jul", "Utility bill without system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_aug", "Utility bill without system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_sep", "Utility bill without system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_oct", "Utility bill without system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_nov", "Utility bill without system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys_dec", "Utility bill without system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys", "Utility bill without system", "$", "", "Charges by Month", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_jan", "Fixed charge with system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_feb", "Fixed charge with system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_mar", "Fixed charge with system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_apr", "Fixed charge with system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_may", "Fixed charge with system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_jun", "Fixed charge with system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_jul", "Fixed charge with system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_aug", "Fixed charge with system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_sep", "Fixed charge with system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_oct", "Fixed charge with system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_nov", "Fixed charge with system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed_dec", "Fixed charge with system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed", "Fixed charge with system", "$", "", "Charges by Month", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_jan", "Fixed charge without system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_feb", "Fixed charge without system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_mar", "Fixed charge without system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_apr", "Fixed charge without system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_may", "Fixed charge without system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_jun", "Fixed charge without system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_jul", "Fixed charge without system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_aug", "Fixed charge without system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_sep", "Fixed charge without system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_oct", "Fixed charge without system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_nov", "Fixed charge without system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed_dec", "Fixed charge without system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed", "Fixed charge without system", "$", "", "Charges by Month", "*", "", "" },




	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_jan", "Minimum charge with system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_feb", "Minimum charge with system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_mar", "Minimum charge with system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_apr", "Minimum charge with system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_may", "Minimum charge with system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_jun", "Minimum charge with system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_jul", "Minimum charge with system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_aug", "Minimum charge with system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_sep", "Minimum charge with system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_oct", "Minimum charge with system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_nov", "Minimum charge with system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum_dec", "Minimum charge with system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_jan", "Minimum charge without system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_feb", "Minimum charge without system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_mar", "Minimum charge without system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_apr", "Minimum charge without system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_may", "Minimum charge without system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_jun", "Minimum charge without system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_jul", "Minimum charge without system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_aug", "Minimum charge without system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_sep", "Minimum charge without system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_oct", "Minimum charge without system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_nov", "Minimum charge without system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum_dec", "Minimum charge without system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "" },





	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jan",      "Demand charge with system (fixed) in Jan",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_feb",      "Demand charge with system (fixed) in Feb",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_mar",      "Demand charge with system (fixed) in Mar",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_apr",      "Demand charge with system (fixed) in Apr",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_may",      "Demand charge with system (fixed) in May",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jun",      "Demand charge with system (fixed) in Jun",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jul",      "Demand charge with system (fixed) in Jul",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_aug",      "Demand charge with system (fixed) in Aug",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_sep",      "Demand charge with system (fixed) in Sep",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_oct",      "Demand charge with system (fixed) in Oct",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_nov",      "Demand charge with system (fixed) in Nov",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed_dec", "Demand charge with system (fixed) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed", "Demand charge with system (fixed)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jan",        "Demand charge with system (TOU) in Jan",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_feb",        "Demand charge with system (TOU) in Feb",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_mar",        "Demand charge with system (TOU) in Mar",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_apr",        "Demand charge with system (TOU) in Apr",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_may",        "Demand charge with system (TOU) in May",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jun",        "Demand charge with system (TOU) in Jun",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jul",        "Demand charge with system (TOU) in Jul",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_aug",        "Demand charge with system (TOU) in Aug",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_sep",        "Demand charge with system (TOU) in Sep",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_oct",        "Demand charge with system (TOU) in Oct",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_nov",        "Demand charge with system (TOU) in Nov",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou_dec", "Demand charge with system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jan",            "Energy charge with system (TOU) in Jan",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_feb",            "Energy charge with system (TOU) in Feb",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_mar",            "Energy charge with system (TOU) in Mar",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_apr",            "Energy charge with system (TOU) in Apr",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_may",            "Energy charge with system (TOU) in May",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jun",            "Energy charge with system (TOU) in Jun",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jul",            "Energy charge with system (TOU) in Jul",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_aug",            "Energy charge with system (TOU) in Aug",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_sep",            "Energy charge with system (TOU) in Sep",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_oct",            "Energy charge with system (TOU) in Oct",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_nov",            "Energy charge with system (TOU) in Nov",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec", "Energy charge with system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec", "Energy charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_jan", "Energy charge with system (flat) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_feb", "Energy charge with system (flat) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_mar", "Energy charge with system (flat) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_apr", "Energy charge with system (flat) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_may", "Energy charge with system (flat) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_jun", "Energy charge with system (flat) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_jul", "Energy charge with system (flat) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_aug", "Energy charge with system (flat) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_sep", "Energy charge with system (flat) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_oct", "Energy charge with system (flat) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_nov", "Energy charge with system (flat) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat_dec", "Energy charge with system (flat) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_flat", "Energy charge with system (flat)", "$", "", "Charges by Month", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jan", "Demand charge without system (fixed) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_feb", "Demand charge without system (fixed) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_mar", "Demand charge without system (fixed) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_apr", "Demand charge without system (fixed) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_may", "Demand charge without system (fixed) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jun", "Demand charge without system (fixed) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jul", "Demand charge without system (fixed) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_aug", "Demand charge without system (fixed) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_sep", "Demand charge without system (fixed) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_oct", "Demand charge without system (fixed) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_nov", "Demand charge without system (fixed) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_dec", "Demand charge without system (fixed) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed", "Demand charge without system (fixed)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jan", "Demand charge without system (TOU) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_feb", "Demand charge without system (TOU) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_mar", "Demand charge without system (TOU) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_apr", "Demand charge without system (TOU) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_may", "Demand charge without system (TOU) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jun", "Demand charge without system (TOU) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jul", "Demand charge without system (TOU) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_aug", "Demand charge without system (TOU) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_sep", "Demand charge without system (TOU) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_oct", "Demand charge without system (TOU) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_nov", "Demand charge without system (TOU) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_dec", "Demand charge without system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan", "Energy charge without system (TOU) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb", "Energy charge without system (TOU) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar", "Energy charge without system (TOU) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr", "Energy charge without system (TOU) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may", "Energy charge without system (TOU) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun", "Energy charge without system (TOU) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul", "Energy charge without system (TOU) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug", "Energy charge without system (TOU) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep", "Energy charge without system (TOU) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct", "Energy charge without system (TOU) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov", "Energy charge without system (TOU) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec", "Energy charge without system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec", "Energy charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_jan", "Energy charge without system (flat) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_feb", "Energy charge without system (flat) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_mar", "Energy charge without system (flat) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_apr", "Energy charge without system (flat) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_may", "Energy charge without system (flat) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_jun", "Energy charge without system (flat) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_jul", "Energy charge without system (flat) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_aug", "Energy charge without system (flat) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_sep", "Energy charge without system (flat) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_oct", "Energy charge without system (flat) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_nov", "Energy charge without system (flat) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat_dec", "Energy charge without system (flat) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_flat", "Energy charge without system (flat)", "$", "", "Charges by Month", "*", "", "" },



// for Pablo at IRENA 8/8/15
// 72 outputs per month for energy
// 72 outputs per month for charges
// repeat with and without system for total of 144*2*12=3456 outputs!
// first year outputs only per email from Paul 8/9/15

// charge wo system
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p1", "Energy charge without system (TOU) in Jan for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p2", "Energy charge without system (TOU) in Jan for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p3", "Energy charge without system (TOU) in Jan for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p4", "Energy charge without system (TOU) in Jan for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p5", "Energy charge without system (TOU) in Jan for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p6", "Energy charge without system (TOU) in Jan for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p7", "Energy charge without system (TOU) in Jan for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p8", "Energy charge without system (TOU) in Jan for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p9", "Energy charge without system (TOU) in Jan for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p10", "Energy charge without system (TOU) in Jan for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p11", "Energy charge without system (TOU) in Jan for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan_p12", "Energy charge without system (TOU) in Jan for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p1", "Energy charge without system (TOU) in Feb for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p2", "Energy charge without system (TOU) in Feb for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p3", "Energy charge without system (TOU) in Feb for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p4", "Energy charge without system (TOU) in Feb for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p5", "Energy charge without system (TOU) in Feb for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p6", "Energy charge without system (TOU) in Feb for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p7", "Energy charge without system (TOU) in Feb for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p8", "Energy charge without system (TOU) in Feb for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p9", "Energy charge without system (TOU) in Feb for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p10", "Energy charge without system (TOU) in Feb for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p11", "Energy charge without system (TOU) in Feb for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb_p12", "Energy charge without system (TOU) in Feb for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p1", "Energy charge without system (TOU) in Mar for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p2", "Energy charge without system (TOU) in Mar for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p3", "Energy charge without system (TOU) in Mar for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p4", "Energy charge without system (TOU) in Mar for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p5", "Energy charge without system (TOU) in Mar for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p6", "Energy charge without system (TOU) in Mar for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p7", "Energy charge without system (TOU) in Mar for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p8", "Energy charge without system (TOU) in Mar for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p9", "Energy charge without system (TOU) in Mar for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p10", "Energy charge without system (TOU) in Mar for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p11", "Energy charge without system (TOU) in Mar for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar_p12", "Energy charge without system (TOU) in Mar for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p1", "Energy charge without system (TOU) in Apr for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p2", "Energy charge without system (TOU) in Apr for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p3", "Energy charge without system (TOU) in Apr for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p4", "Energy charge without system (TOU) in Apr for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p5", "Energy charge without system (TOU) in Apr for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p6", "Energy charge without system (TOU) in Apr for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p7", "Energy charge without system (TOU) in Apr for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p8", "Energy charge without system (TOU) in Apr for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p9", "Energy charge without system (TOU) in Apr for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p10", "Energy charge without system (TOU) in Apr for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p11", "Energy charge without system (TOU) in Apr for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr_p12", "Energy charge without system (TOU) in Apr for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p1", "Energy charge without system (TOU) in May for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p2", "Energy charge without system (TOU) in May for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p3", "Energy charge without system (TOU) in May for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p4", "Energy charge without system (TOU) in May for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p5", "Energy charge without system (TOU) in May for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p6", "Energy charge without system (TOU) in May for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p7", "Energy charge without system (TOU) in May for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p8", "Energy charge without system (TOU) in May for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p9", "Energy charge without system (TOU) in May for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p10", "Energy charge without system (TOU) in May for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p11", "Energy charge without system (TOU) in May for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may_p12", "Energy charge without system (TOU) in May for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p1", "Energy charge without system (TOU) in Jun for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p2", "Energy charge without system (TOU) in Jun for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p3", "Energy charge without system (TOU) in Jun for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p4", "Energy charge without system (TOU) in Jun for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p5", "Energy charge without system (TOU) in Jun for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p6", "Energy charge without system (TOU) in Jun for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p7", "Energy charge without system (TOU) in Jun for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p8", "Energy charge without system (TOU) in Jun for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p9", "Energy charge without system (TOU) in Jun for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p10", "Energy charge without system (TOU) in Jun for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p11", "Energy charge without system (TOU) in Jun for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun_p12", "Energy charge without system (TOU) in Jun for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p1", "Energy charge without system (TOU) in Jul for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p2", "Energy charge without system (TOU) in Jul for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p3", "Energy charge without system (TOU) in Jul for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p4", "Energy charge without system (TOU) in Jul for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p5", "Energy charge without system (TOU) in Jul for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p6", "Energy charge without system (TOU) in Jul for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p7", "Energy charge without system (TOU) in Jul for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p8", "Energy charge without system (TOU) in Jul for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p9", "Energy charge without system (TOU) in Jul for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p10", "Energy charge without system (TOU) in Jul for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p11", "Energy charge without system (TOU) in Jul for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul_p12", "Energy charge without system (TOU) in Jul for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p1", "Energy charge without system (TOU) in Aug for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p2", "Energy charge without system (TOU) in Aug for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p3", "Energy charge without system (TOU) in Aug for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p4", "Energy charge without system (TOU) in Aug for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p5", "Energy charge without system (TOU) in Aug for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p6", "Energy charge without system (TOU) in Aug for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p7", "Energy charge without system (TOU) in Aug for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p8", "Energy charge without system (TOU) in Aug for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p9", "Energy charge without system (TOU) in Aug for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p10", "Energy charge without system (TOU) in Aug for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p11", "Energy charge without system (TOU) in Aug for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug_p12", "Energy charge without system (TOU) in Aug for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p1", "Energy charge without system (TOU) in Sep for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p2", "Energy charge without system (TOU) in Sep for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p3", "Energy charge without system (TOU) in Sep for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p4", "Energy charge without system (TOU) in Sep for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p5", "Energy charge without system (TOU) in Sep for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p6", "Energy charge without system (TOU) in Sep for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p7", "Energy charge without system (TOU) in Sep for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p8", "Energy charge without system (TOU) in Sep for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p9", "Energy charge without system (TOU) in Sep for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p10", "Energy charge without system (TOU) in Sep for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p11", "Energy charge without system (TOU) in Sep for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep_p12", "Energy charge without system (TOU) in Sep for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p1", "Energy charge without system (TOU) in Oct for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p2", "Energy charge without system (TOU) in Oct for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p3", "Energy charge without system (TOU) in Oct for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p4", "Energy charge without system (TOU) in Oct for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p5", "Energy charge without system (TOU) in Oct for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p6", "Energy charge without system (TOU) in Oct for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p7", "Energy charge without system (TOU) in Oct for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p8", "Energy charge without system (TOU) in Oct for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p9", "Energy charge without system (TOU) in Oct for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p10", "Energy charge without system (TOU) in Oct for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p11", "Energy charge without system (TOU) in Oct for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct_p12", "Energy charge without system (TOU) in Oct for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p1", "Energy charge without system (TOU) in Nov for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p2", "Energy charge without system (TOU) in Nov for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p3", "Energy charge without system (TOU) in Nov for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p4", "Energy charge without system (TOU) in Nov for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p5", "Energy charge without system (TOU) in Nov for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p6", "Energy charge without system (TOU) in Nov for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p7", "Energy charge without system (TOU) in Nov for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p8", "Energy charge without system (TOU) in Nov for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p9", "Energy charge without system (TOU) in Nov for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p10", "Energy charge without system (TOU) in Nov for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p11", "Energy charge without system (TOU) in Nov for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov_p12", "Energy charge without system (TOU) in Nov for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p1", "Energy charge without system (TOU) in Dec for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p2", "Energy charge without system (TOU) in Dec for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p3", "Energy charge without system (TOU) in Dec for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p4", "Energy charge without system (TOU) in Dec for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p5", "Energy charge without system (TOU) in Dec for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p6", "Energy charge without system (TOU) in Dec for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p7", "Energy charge without system (TOU) in Dec for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p8", "Energy charge without system (TOU) in Dec for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p9", "Energy charge without system (TOU) in Dec for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p10", "Energy charge without system (TOU) in Dec for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p11", "Energy charge without system (TOU) in Dec for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec_p12", "Energy charge without system (TOU) in Dec for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },


// energy wo system
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p1", "Energy without system (TOU) in Jan for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p2", "Energy without system (TOU) in Jan for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p3", "Energy without system (TOU) in Jan for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p4", "Energy without system (TOU) in Jan for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p5", "Energy without system (TOU) in Jan for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p6", "Energy without system (TOU) in Jan for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p7", "Energy without system (TOU) in Jan for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p8", "Energy without system (TOU) in Jan for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p9", "Energy without system (TOU) in Jan for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p10", "Energy without system (TOU) in Jan for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p11", "Energy without system (TOU) in Jan for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jan_p12", "Energy without system (TOU) in Jan for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p1", "Energy without system (TOU) in Feb for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p2", "Energy without system (TOU) in Feb for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p3", "Energy without system (TOU) in Feb for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p4", "Energy without system (TOU) in Feb for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p5", "Energy without system (TOU) in Feb for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p6", "Energy without system (TOU) in Feb for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p7", "Energy without system (TOU) in Feb for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p8", "Energy without system (TOU) in Feb for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p9", "Energy without system (TOU) in Feb for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p10", "Energy without system (TOU) in Feb for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p11", "Energy without system (TOU) in Feb for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_feb_p12", "Energy without system (TOU) in Feb for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p1", "Energy without system (TOU) in Mar for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p2", "Energy without system (TOU) in Mar for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p3", "Energy without system (TOU) in Mar for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p4", "Energy without system (TOU) in Mar for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p5", "Energy without system (TOU) in Mar for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p6", "Energy without system (TOU) in Mar for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p7", "Energy without system (TOU) in Mar for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p8", "Energy without system (TOU) in Mar for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p9", "Energy without system (TOU) in Mar for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p10", "Energy without system (TOU) in Mar for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p11", "Energy without system (TOU) in Mar for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_mar_p12", "Energy without system (TOU) in Mar for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p1", "Energy without system (TOU) in Apr for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p2", "Energy without system (TOU) in Apr for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p3", "Energy without system (TOU) in Apr for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p4", "Energy without system (TOU) in Apr for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p5", "Energy without system (TOU) in Apr for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p6", "Energy without system (TOU) in Apr for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p7", "Energy without system (TOU) in Apr for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p8", "Energy without system (TOU) in Apr for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p9", "Energy without system (TOU) in Apr for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p10", "Energy without system (TOU) in Apr for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p11", "Energy without system (TOU) in Apr for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_apr_p12", "Energy without system (TOU) in Apr for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p1", "Energy without system (TOU) in May for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p2", "Energy without system (TOU) in May for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p3", "Energy without system (TOU) in May for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p4", "Energy without system (TOU) in May for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p5", "Energy without system (TOU) in May for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p6", "Energy without system (TOU) in May for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p7", "Energy without system (TOU) in May for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p8", "Energy without system (TOU) in May for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p9", "Energy without system (TOU) in May for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p10", "Energy without system (TOU) in May for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p11", "Energy without system (TOU) in May for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_may_p12", "Energy without system (TOU) in May for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p1", "Energy without system (TOU) in Jun for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p2", "Energy without system (TOU) in Jun for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p3", "Energy without system (TOU) in Jun for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p4", "Energy without system (TOU) in Jun for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p5", "Energy without system (TOU) in Jun for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p6", "Energy without system (TOU) in Jun for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p7", "Energy without system (TOU) in Jun for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p8", "Energy without system (TOU) in Jun for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p9", "Energy without system (TOU) in Jun for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p10", "Energy without system (TOU) in Jun for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p11", "Energy without system (TOU) in Jun for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jun_p12", "Energy without system (TOU) in Jun for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p1", "Energy without system (TOU) in Jul for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p2", "Energy without system (TOU) in Jul for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p3", "Energy without system (TOU) in Jul for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p4", "Energy without system (TOU) in Jul for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p5", "Energy without system (TOU) in Jul for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p6", "Energy without system (TOU) in Jul for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p7", "Energy without system (TOU) in Jul for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p8", "Energy without system (TOU) in Jul for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p9", "Energy without system (TOU) in Jul for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p10", "Energy without system (TOU) in Jul for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p11", "Energy without system (TOU) in Jul for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_jul_p12", "Energy without system (TOU) in Jul for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p1", "Energy without system (TOU) in Aug for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p2", "Energy without system (TOU) in Aug for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p3", "Energy without system (TOU) in Aug for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p4", "Energy without system (TOU) in Aug for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p5", "Energy without system (TOU) in Aug for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p6", "Energy without system (TOU) in Aug for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p7", "Energy without system (TOU) in Aug for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p8", "Energy without system (TOU) in Aug for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p9", "Energy without system (TOU) in Aug for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p10", "Energy without system (TOU) in Aug for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p11", "Energy without system (TOU) in Aug for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_aug_p12", "Energy without system (TOU) in Aug for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p1", "Energy without system (TOU) in Sep for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p2", "Energy without system (TOU) in Sep for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p3", "Energy without system (TOU) in Sep for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p4", "Energy without system (TOU) in Sep for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p5", "Energy without system (TOU) in Sep for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p6", "Energy without system (TOU) in Sep for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p7", "Energy without system (TOU) in Sep for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p8", "Energy without system (TOU) in Sep for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p9", "Energy without system (TOU) in Sep for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p10", "Energy without system (TOU) in Sep for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p11", "Energy without system (TOU) in Sep for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_sep_p12", "Energy without system (TOU) in Sep for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p1", "Energy without system (TOU) in Oct for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p2", "Energy without system (TOU) in Oct for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p3", "Energy without system (TOU) in Oct for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p4", "Energy without system (TOU) in Oct for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p5", "Energy without system (TOU) in Oct for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p6", "Energy without system (TOU) in Oct for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p7", "Energy without system (TOU) in Oct for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p8", "Energy without system (TOU) in Oct for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p9", "Energy without system (TOU) in Oct for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p10", "Energy without system (TOU) in Oct for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p11", "Energy without system (TOU) in Oct for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_oct_p12", "Energy without system (TOU) in Oct for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p1", "Energy without system (TOU) in Nov for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p2", "Energy without system (TOU) in Nov for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p3", "Energy without system (TOU) in Nov for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p4", "Energy without system (TOU) in Nov for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p5", "Energy without system (TOU) in Nov for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p6", "Energy without system (TOU) in Nov for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p7", "Energy without system (TOU) in Nov for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p8", "Energy without system (TOU) in Nov for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p9", "Energy without system (TOU) in Nov for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p10", "Energy without system (TOU) in Nov for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p11", "Energy without system (TOU) in Nov for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_nov_p12", "Energy without system (TOU) in Nov for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p1", "Energy without system (TOU) in Dec for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p2", "Energy without system (TOU) in Dec for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p3", "Energy without system (TOU) in Dec for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p4", "Energy without system (TOU) in Dec for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p5", "Energy without system (TOU) in Dec for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p6", "Energy without system (TOU) in Dec for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p7", "Energy without system (TOU) in Dec for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p8", "Energy without system (TOU) in Dec for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p9", "Energy without system (TOU) in Dec for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p10", "Energy without system (TOU) in Dec for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p11", "Energy without system (TOU) in Dec for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_wo_sys_ec_dec_p12", "Energy without system (TOU) in Dec for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	// charge w system
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p1", "Energy charge with system (TOU) in Jan for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p2", "Energy charge with system (TOU) in Jan for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p3", "Energy charge with system (TOU) in Jan for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p4", "Energy charge with system (TOU) in Jan for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p5", "Energy charge with system (TOU) in Jan for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p6", "Energy charge with system (TOU) in Jan for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p7", "Energy charge with system (TOU) in Jan for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p8", "Energy charge with system (TOU) in Jan for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p9", "Energy charge with system (TOU) in Jan for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p10", "Energy charge with system (TOU) in Jan for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p11", "Energy charge with system (TOU) in Jan for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jan_p12", "Energy charge with system (TOU) in Jan for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p1", "Energy charge with system (TOU) in Feb for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p2", "Energy charge with system (TOU) in Feb for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p3", "Energy charge with system (TOU) in Feb for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p4", "Energy charge with system (TOU) in Feb for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p5", "Energy charge with system (TOU) in Feb for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p6", "Energy charge with system (TOU) in Feb for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p7", "Energy charge with system (TOU) in Feb for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p8", "Energy charge with system (TOU) in Feb for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p9", "Energy charge with system (TOU) in Feb for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p10", "Energy charge with system (TOU) in Feb for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p11", "Energy charge with system (TOU) in Feb for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_feb_p12", "Energy charge with system (TOU) in Feb for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p1", "Energy charge with system (TOU) in Mar for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p2", "Energy charge with system (TOU) in Mar for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p3", "Energy charge with system (TOU) in Mar for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p4", "Energy charge with system (TOU) in Mar for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p5", "Energy charge with system (TOU) in Mar for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p6", "Energy charge with system (TOU) in Mar for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p7", "Energy charge with system (TOU) in Mar for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p8", "Energy charge with system (TOU) in Mar for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p9", "Energy charge with system (TOU) in Mar for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p10", "Energy charge with system (TOU) in Mar for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p11", "Energy charge with system (TOU) in Mar for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_mar_p12", "Energy charge with system (TOU) in Mar for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p1", "Energy charge with system (TOU) in Apr for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p2", "Energy charge with system (TOU) in Apr for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p3", "Energy charge with system (TOU) in Apr for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p4", "Energy charge with system (TOU) in Apr for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p5", "Energy charge with system (TOU) in Apr for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p6", "Energy charge with system (TOU) in Apr for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p7", "Energy charge with system (TOU) in Apr for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p8", "Energy charge with system (TOU) in Apr for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p9", "Energy charge with system (TOU) in Apr for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p10", "Energy charge with system (TOU) in Apr for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p11", "Energy charge with system (TOU) in Apr for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_apr_p12", "Energy charge with system (TOU) in Apr for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p1", "Energy charge with system (TOU) in May for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p2", "Energy charge with system (TOU) in May for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p3", "Energy charge with system (TOU) in May for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p4", "Energy charge with system (TOU) in May for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p5", "Energy charge with system (TOU) in May for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p6", "Energy charge with system (TOU) in May for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p7", "Energy charge with system (TOU) in May for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p8", "Energy charge with system (TOU) in May for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p9", "Energy charge with system (TOU) in May for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p10", "Energy charge with system (TOU) in May for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p11", "Energy charge with system (TOU) in May for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_may_p12", "Energy charge with system (TOU) in May for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p1", "Energy charge with system (TOU) in Jun for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p2", "Energy charge with system (TOU) in Jun for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p3", "Energy charge with system (TOU) in Jun for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p4", "Energy charge with system (TOU) in Jun for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p5", "Energy charge with system (TOU) in Jun for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p6", "Energy charge with system (TOU) in Jun for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p7", "Energy charge with system (TOU) in Jun for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p8", "Energy charge with system (TOU) in Jun for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p9", "Energy charge with system (TOU) in Jun for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p10", "Energy charge with system (TOU) in Jun for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p11", "Energy charge with system (TOU) in Jun for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jun_p12", "Energy charge with system (TOU) in Jun for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p1", "Energy charge with system (TOU) in Jul for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p2", "Energy charge with system (TOU) in Jul for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p3", "Energy charge with system (TOU) in Jul for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p4", "Energy charge with system (TOU) in Jul for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p5", "Energy charge with system (TOU) in Jul for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p6", "Energy charge with system (TOU) in Jul for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p7", "Energy charge with system (TOU) in Jul for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p8", "Energy charge with system (TOU) in Jul for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p9", "Energy charge with system (TOU) in Jul for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p10", "Energy charge with system (TOU) in Jul for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p11", "Energy charge with system (TOU) in Jul for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_jul_p12", "Energy charge with system (TOU) in Jul for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p1", "Energy charge with system (TOU) in Aug for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p2", "Energy charge with system (TOU) in Aug for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p3", "Energy charge with system (TOU) in Aug for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p4", "Energy charge with system (TOU) in Aug for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p5", "Energy charge with system (TOU) in Aug for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p6", "Energy charge with system (TOU) in Aug for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p7", "Energy charge with system (TOU) in Aug for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p8", "Energy charge with system (TOU) in Aug for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p9", "Energy charge with system (TOU) in Aug for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p10", "Energy charge with system (TOU) in Aug for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p11", "Energy charge with system (TOU) in Aug for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_aug_p12", "Energy charge with system (TOU) in Aug for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p1", "Energy charge with system (TOU) in Sep for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p2", "Energy charge with system (TOU) in Sep for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p3", "Energy charge with system (TOU) in Sep for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p4", "Energy charge with system (TOU) in Sep for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p5", "Energy charge with system (TOU) in Sep for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p6", "Energy charge with system (TOU) in Sep for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p7", "Energy charge with system (TOU) in Sep for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p8", "Energy charge with system (TOU) in Sep for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p9", "Energy charge with system (TOU) in Sep for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p10", "Energy charge with system (TOU) in Sep for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p11", "Energy charge with system (TOU) in Sep for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_sep_p12", "Energy charge with system (TOU) in Sep for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p1", "Energy charge with system (TOU) in Oct for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p2", "Energy charge with system (TOU) in Oct for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p3", "Energy charge with system (TOU) in Oct for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p4", "Energy charge with system (TOU) in Oct for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p5", "Energy charge with system (TOU) in Oct for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p6", "Energy charge with system (TOU) in Oct for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p7", "Energy charge with system (TOU) in Oct for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p8", "Energy charge with system (TOU) in Oct for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p9", "Energy charge with system (TOU) in Oct for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p10", "Energy charge with system (TOU) in Oct for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p11", "Energy charge with system (TOU) in Oct for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_oct_p12", "Energy charge with system (TOU) in Oct for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p1", "Energy charge with system (TOU) in Nov for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p2", "Energy charge with system (TOU) in Nov for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p3", "Energy charge with system (TOU) in Nov for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p4", "Energy charge with system (TOU) in Nov for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p5", "Energy charge with system (TOU) in Nov for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p6", "Energy charge with system (TOU) in Nov for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p7", "Energy charge with system (TOU) in Nov for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p8", "Energy charge with system (TOU) in Nov for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p9", "Energy charge with system (TOU) in Nov for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p10", "Energy charge with system (TOU) in Nov for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p11", "Energy charge with system (TOU) in Nov for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_nov_p12", "Energy charge with system (TOU) in Nov for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p1", "Energy charge with system (TOU) in Dec for period 1 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p2", "Energy charge with system (TOU) in Dec for period 2 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p3", "Energy charge with system (TOU) in Dec for period 3 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p4", "Energy charge with system (TOU) in Dec for period 4 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p5", "Energy charge with system (TOU) in Dec for period 5 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p6", "Energy charge with system (TOU) in Dec for period 6 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p7", "Energy charge with system (TOU) in Dec for period 7 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p8", "Energy charge with system (TOU) in Dec for period 8 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p9", "Energy charge with system (TOU) in Dec for period 9 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p10", "Energy charge with system (TOU) in Dec for period 10 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p11", "Energy charge with system (TOU) in Dec for period 11 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec_p12", "Energy charge with system (TOU) in Dec for period 12 and tiers 1 through 6", "$", "", "Charges by Month", "*", "", "" },


	// energy w system
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p1", "Energy with system (TOU) in Jan for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p2", "Energy with system (TOU) in Jan for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p3", "Energy with system (TOU) in Jan for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p4", "Energy with system (TOU) in Jan for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p5", "Energy with system (TOU) in Jan for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p6", "Energy with system (TOU) in Jan for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p7", "Energy with system (TOU) in Jan for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p8", "Energy with system (TOU) in Jan for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p9", "Energy with system (TOU) in Jan for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p10", "Energy with system (TOU) in Jan for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p11", "Energy with system (TOU) in Jan for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jan_p12", "Energy with system (TOU) in Jan for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p1", "Energy with system (TOU) in Feb for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p2", "Energy with system (TOU) in Feb for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p3", "Energy with system (TOU) in Feb for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p4", "Energy with system (TOU) in Feb for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p5", "Energy with system (TOU) in Feb for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p6", "Energy with system (TOU) in Feb for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p7", "Energy with system (TOU) in Feb for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p8", "Energy with system (TOU) in Feb for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p9", "Energy with system (TOU) in Feb for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p10", "Energy with system (TOU) in Feb for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p11", "Energy with system (TOU) in Feb for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_feb_p12", "Energy with system (TOU) in Feb for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p1", "Energy with system (TOU) in Mar for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p2", "Energy with system (TOU) in Mar for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p3", "Energy with system (TOU) in Mar for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p4", "Energy with system (TOU) in Mar for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p5", "Energy with system (TOU) in Mar for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p6", "Energy with system (TOU) in Mar for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p7", "Energy with system (TOU) in Mar for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p8", "Energy with system (TOU) in Mar for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p9", "Energy with system (TOU) in Mar for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p10", "Energy with system (TOU) in Mar for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p11", "Energy with system (TOU) in Mar for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_mar_p12", "Energy with system (TOU) in Mar for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p1", "Energy with system (TOU) in Apr for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p2", "Energy with system (TOU) in Apr for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p3", "Energy with system (TOU) in Apr for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p4", "Energy with system (TOU) in Apr for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p5", "Energy with system (TOU) in Apr for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p6", "Energy with system (TOU) in Apr for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p7", "Energy with system (TOU) in Apr for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p8", "Energy with system (TOU) in Apr for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p9", "Energy with system (TOU) in Apr for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p10", "Energy with system (TOU) in Apr for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p11", "Energy with system (TOU) in Apr for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_apr_p12", "Energy with system (TOU) in Apr for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p1", "Energy with system (TOU) in May for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p2", "Energy with system (TOU) in May for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p3", "Energy with system (TOU) in May for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p4", "Energy with system (TOU) in May for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p5", "Energy with system (TOU) in May for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p6", "Energy with system (TOU) in May for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p7", "Energy with system (TOU) in May for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p8", "Energy with system (TOU) in May for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p9", "Energy with system (TOU) in May for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p10", "Energy with system (TOU) in May for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p11", "Energy with system (TOU) in May for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_may_p12", "Energy with system (TOU) in May for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p1", "Energy with system (TOU) in Jun for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p2", "Energy with system (TOU) in Jun for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p3", "Energy with system (TOU) in Jun for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p4", "Energy with system (TOU) in Jun for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p5", "Energy with system (TOU) in Jun for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p6", "Energy with system (TOU) in Jun for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p7", "Energy with system (TOU) in Jun for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p8", "Energy with system (TOU) in Jun for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p9", "Energy with system (TOU) in Jun for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p10", "Energy with system (TOU) in Jun for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p11", "Energy with system (TOU) in Jun for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jun_p12", "Energy with system (TOU) in Jun for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p1", "Energy with system (TOU) in Jul for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p2", "Energy with system (TOU) in Jul for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p3", "Energy with system (TOU) in Jul for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p4", "Energy with system (TOU) in Jul for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p5", "Energy with system (TOU) in Jul for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p6", "Energy with system (TOU) in Jul for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p7", "Energy with system (TOU) in Jul for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p8", "Energy with system (TOU) in Jul for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p9", "Energy with system (TOU) in Jul for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p10", "Energy with system (TOU) in Jul for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p11", "Energy with system (TOU) in Jul for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_jul_p12", "Energy with system (TOU) in Jul for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p1", "Energy with system (TOU) in Aug for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p2", "Energy with system (TOU) in Aug for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p3", "Energy with system (TOU) in Aug for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p4", "Energy with system (TOU) in Aug for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p5", "Energy with system (TOU) in Aug for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p6", "Energy with system (TOU) in Aug for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p7", "Energy with system (TOU) in Aug for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p8", "Energy with system (TOU) in Aug for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p9", "Energy with system (TOU) in Aug for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p10", "Energy with system (TOU) in Aug for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p11", "Energy with system (TOU) in Aug for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_aug_p12", "Energy with system (TOU) in Aug for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p1", "Energy with system (TOU) in Sep for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p2", "Energy with system (TOU) in Sep for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p3", "Energy with system (TOU) in Sep for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p4", "Energy with system (TOU) in Sep for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p5", "Energy with system (TOU) in Sep for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p6", "Energy with system (TOU) in Sep for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p7", "Energy with system (TOU) in Sep for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p8", "Energy with system (TOU) in Sep for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p9", "Energy with system (TOU) in Sep for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p10", "Energy with system (TOU) in Sep for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p11", "Energy with system (TOU) in Sep for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_sep_p12", "Energy with system (TOU) in Sep for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p1", "Energy with system (TOU) in Oct for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p2", "Energy with system (TOU) in Oct for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p3", "Energy with system (TOU) in Oct for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p4", "Energy with system (TOU) in Oct for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p5", "Energy with system (TOU) in Oct for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p6", "Energy with system (TOU) in Oct for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p7", "Energy with system (TOU) in Oct for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p8", "Energy with system (TOU) in Oct for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p9", "Energy with system (TOU) in Oct for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p10", "Energy with system (TOU) in Oct for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p11", "Energy with system (TOU) in Oct for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_oct_p12", "Energy with system (TOU) in Oct for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p1", "Energy with system (TOU) in Nov for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p2", "Energy with system (TOU) in Nov for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p3", "Energy with system (TOU) in Nov for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p4", "Energy with system (TOU) in Nov for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p5", "Energy with system (TOU) in Nov for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p6", "Energy with system (TOU) in Nov for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p7", "Energy with system (TOU) in Nov for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p8", "Energy with system (TOU) in Nov for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p9", "Energy with system (TOU) in Nov for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p10", "Energy with system (TOU) in Nov for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p11", "Energy with system (TOU) in Nov for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_nov_p12", "Energy with system (TOU) in Nov for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p1", "Energy with system (TOU) in Dec for period 1 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p2", "Energy with system (TOU) in Dec for period 2 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p3", "Energy with system (TOU) in Dec for period 3 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p4", "Energy with system (TOU) in Dec for period 4 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p5", "Energy with system (TOU) in Dec for period 5 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p6", "Energy with system (TOU) in Dec for period 6 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p7", "Energy with system (TOU) in Dec for period 7 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p8", "Energy with system (TOU) in Dec for period 8 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p9", "Energy with system (TOU) in Dec for period 9 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p10", "Energy with system (TOU) in Dec for period 10 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p11", "Energy with system (TOU) in Dec for period 11 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "energy_w_sys_ec_dec_p12", "Energy with system (TOU) in Dec for period 12 and tiers 1 through 6", "kWh", "", "Charges by Month", "*", "", "" },



	var_info_invalid };




class cm_utilityrate3 : public compute_module
{
private:
public:
	cm_utilityrate3()
	{
		add_var_info( vtab_utility_rate3 );
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
			throw exec_error("utilityrate3", util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;


		if (is_assigned("load"))
		{ // hourly or sub hourly loads for single yer
			bload = true;
			pload = as_array("load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("utilityrate3", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
//			if (nrec_load != nrec_gen)
//				throw exec_error("utilityrate3", util::format("number of load records (%d) must be equal to number of gen records (%d)", (int)nrec_load, (int)nrec_gen));
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
			payment(8760), income(8760), price(8760), demand_charge(8760), energy_charge(8760),
			ec_tou_sched(8760), dc_tou_sched(8760), load(8760), dc_hourly_peak(8760),
			e_tofromgrid(8760), p_tofromgrid(8760),	salespurchases(8760);
		std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
			monthly_fixed_charges(12), monthly_minimum_charges(12),
			monthly_dc_fixed(12), monthly_dc_tou(12),
			monthly_ec_charges(12), monthly_ec_flat_charges(12), monthly_ec_rates(12),
			monthly_salespurchases(12),
			monthly_load(12), monthly_system_generation(12), monthly_elec_to_grid(12),
			monthly_elec_needed_from_grid(12),
			monthly_cumulative_excess_energy(12), monthly_cumulative_excess_dollars(12), monthly_bill(12);
		ssc_number_t monthly_e_use_period_tier[12][12][6],
			monthly_charge_period_tier[12][12][6];

		/* allocate outputs */		
		ssc_number_t *annual_net_revenue = allocate("annual_energy_value", nyears+1);
		ssc_number_t *annual_electric_load = allocate("annual_electric_load", nyears+1);
		ssc_number_t *energy_net = allocate("scaled_annual_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("revenue_without_system", nyears+1);
		ssc_number_t *annual_elec_cost_w_sys = allocate("elec_cost_with_system", nyears+1);
		ssc_number_t *annual_elec_cost_wo_sys = allocate("elec_cost_without_system", nyears+1);

		ssc_number_t *utility_bill_w_sys_jan = allocate("utility_bill_w_sys_jan", nyears + 1);
		ssc_number_t *utility_bill_w_sys_feb = allocate("utility_bill_w_sys_feb", nyears + 1);
		ssc_number_t *utility_bill_w_sys_mar = allocate("utility_bill_w_sys_mar", nyears + 1);
		ssc_number_t *utility_bill_w_sys_apr = allocate("utility_bill_w_sys_apr", nyears + 1);
		ssc_number_t *utility_bill_w_sys_may = allocate("utility_bill_w_sys_may", nyears + 1);
		ssc_number_t *utility_bill_w_sys_jun = allocate("utility_bill_w_sys_jun", nyears + 1);
		ssc_number_t *utility_bill_w_sys_jul = allocate("utility_bill_w_sys_jul", nyears + 1);
		ssc_number_t *utility_bill_w_sys_aug = allocate("utility_bill_w_sys_aug", nyears + 1);
		ssc_number_t *utility_bill_w_sys_sep = allocate("utility_bill_w_sys_sep", nyears + 1);
		ssc_number_t *utility_bill_w_sys_oct = allocate("utility_bill_w_sys_oct", nyears + 1);
		ssc_number_t *utility_bill_w_sys_nov = allocate("utility_bill_w_sys_nov", nyears + 1);
		ssc_number_t *utility_bill_w_sys_dec = allocate("utility_bill_w_sys_dec", nyears + 1);
		ssc_number_t *utility_bill_w_sys = allocate("utility_bill_w_sys", nyears + 1);


		ssc_number_t *utility_bill_wo_sys_jan = allocate("utility_bill_wo_sys_jan", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_feb = allocate("utility_bill_wo_sys_feb", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_mar = allocate("utility_bill_wo_sys_mar", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_apr = allocate("utility_bill_wo_sys_apr", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_may = allocate("utility_bill_wo_sys_may", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_jun = allocate("utility_bill_wo_sys_jun", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_jul = allocate("utility_bill_wo_sys_jul", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_aug = allocate("utility_bill_wo_sys_aug", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_sep = allocate("utility_bill_wo_sys_sep", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_oct = allocate("utility_bill_wo_sys_oct", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_nov = allocate("utility_bill_wo_sys_nov", nyears + 1);
		ssc_number_t *utility_bill_wo_sys_dec = allocate("utility_bill_wo_sys_dec", nyears + 1);
		ssc_number_t *utility_bill_wo_sys = allocate("utility_bill_wo_sys", nyears + 1);


		ssc_number_t *ch_w_sys_dc_fixed_jan = allocate("charge_w_sys_dc_fixed_jan", nyears + 1);
		ssc_number_t *ch_w_sys_dc_fixed_feb = allocate("charge_w_sys_dc_fixed_feb", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_mar = allocate("charge_w_sys_dc_fixed_mar", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_apr = allocate("charge_w_sys_dc_fixed_apr", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_may = allocate("charge_w_sys_dc_fixed_may", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_jun = allocate("charge_w_sys_dc_fixed_jun", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_jul = allocate("charge_w_sys_dc_fixed_jul", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_aug = allocate("charge_w_sys_dc_fixed_aug", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_sep = allocate("charge_w_sys_dc_fixed_sep", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_oct = allocate("charge_w_sys_dc_fixed_oct", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_nov = allocate("charge_w_sys_dc_fixed_nov", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_dec = allocate("charge_w_sys_dc_fixed_dec", nyears + 1);
		ssc_number_t *ch_w_sys_dc_fixed = allocate("charge_w_sys_dc_fixed", nyears + 1);

		ssc_number_t *ch_w_sys_dc_tou_jan = allocate("charge_w_sys_dc_tou_jan", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_feb = allocate("charge_w_sys_dc_tou_feb", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_mar = allocate("charge_w_sys_dc_tou_mar", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_apr = allocate("charge_w_sys_dc_tou_apr", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_may = allocate("charge_w_sys_dc_tou_may", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_jun = allocate("charge_w_sys_dc_tou_jun", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_jul = allocate("charge_w_sys_dc_tou_jul", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_aug = allocate("charge_w_sys_dc_tou_aug", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_sep = allocate("charge_w_sys_dc_tou_sep", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_oct = allocate("charge_w_sys_dc_tou_oct", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_nov = allocate("charge_w_sys_dc_tou_nov", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_dec = allocate("charge_w_sys_dc_tou_dec", nyears + 1);
		ssc_number_t *ch_w_sys_dc_tou = allocate("charge_w_sys_dc_tou", nyears + 1);

		ssc_number_t *ch_w_sys_ec_jan = allocate("charge_w_sys_ec_jan", nyears + 1);
		ssc_number_t *ch_w_sys_ec_feb = allocate("charge_w_sys_ec_feb", nyears + 1);
		ssc_number_t *ch_w_sys_ec_mar = allocate("charge_w_sys_ec_mar", nyears + 1);
		ssc_number_t *ch_w_sys_ec_apr = allocate("charge_w_sys_ec_apr", nyears + 1);
		ssc_number_t *ch_w_sys_ec_may = allocate("charge_w_sys_ec_may", nyears + 1);
		ssc_number_t *ch_w_sys_ec_jun = allocate("charge_w_sys_ec_jun", nyears + 1);
		ssc_number_t *ch_w_sys_ec_jul = allocate("charge_w_sys_ec_jul", nyears + 1);
		ssc_number_t *ch_w_sys_ec_aug = allocate("charge_w_sys_ec_aug", nyears + 1);
		ssc_number_t *ch_w_sys_ec_sep = allocate("charge_w_sys_ec_sep", nyears + 1);
		ssc_number_t *ch_w_sys_ec_oct = allocate("charge_w_sys_ec_oct", nyears + 1);
		ssc_number_t *ch_w_sys_ec_nov = allocate("charge_w_sys_ec_nov", nyears + 1);
		ssc_number_t *ch_w_sys_ec_dec = allocate("charge_w_sys_ec_dec", nyears + 1);
		ssc_number_t *ch_w_sys_ec = allocate("charge_w_sys_ec", nyears + 1);

		ssc_number_t *ch_w_sys_ec_flat_jan = allocate("charge_w_sys_ec_flat_jan", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_feb = allocate("charge_w_sys_ec_flat_feb", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_mar = allocate("charge_w_sys_ec_flat_mar", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_apr = allocate("charge_w_sys_ec_flat_apr", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_may = allocate("charge_w_sys_ec_flat_may", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_jun = allocate("charge_w_sys_ec_flat_jun", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_jul = allocate("charge_w_sys_ec_flat_jul", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_aug = allocate("charge_w_sys_ec_flat_aug", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_sep = allocate("charge_w_sys_ec_flat_sep", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_oct = allocate("charge_w_sys_ec_flat_oct", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_nov = allocate("charge_w_sys_ec_flat_nov", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat_dec = allocate("charge_w_sys_ec_flat_dec", nyears + 1);
		ssc_number_t *ch_w_sys_ec_flat = allocate("charge_w_sys_ec_flat", nyears + 1);

		ssc_number_t *ch_wo_sys_dc_fixed_jan = allocate("charge_wo_sys_dc_fixed_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_feb = allocate("charge_wo_sys_dc_fixed_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_mar = allocate("charge_wo_sys_dc_fixed_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_apr = allocate("charge_wo_sys_dc_fixed_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_may = allocate("charge_wo_sys_dc_fixed_may", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_jun = allocate("charge_wo_sys_dc_fixed_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_jul = allocate("charge_wo_sys_dc_fixed_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_aug = allocate("charge_wo_sys_dc_fixed_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_sep = allocate("charge_wo_sys_dc_fixed_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_oct = allocate("charge_wo_sys_dc_fixed_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_nov = allocate("charge_wo_sys_dc_fixed_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_dec = allocate("charge_wo_sys_dc_fixed_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed = allocate("charge_wo_sys_dc_fixed", nyears + 1);

		ssc_number_t *ch_wo_sys_dc_tou_jan = allocate("charge_wo_sys_dc_tou_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_feb = allocate("charge_wo_sys_dc_tou_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_mar = allocate("charge_wo_sys_dc_tou_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_apr = allocate("charge_wo_sys_dc_tou_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_may = allocate("charge_wo_sys_dc_tou_may", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_jun = allocate("charge_wo_sys_dc_tou_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_jul = allocate("charge_wo_sys_dc_tou_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_aug = allocate("charge_wo_sys_dc_tou_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_sep = allocate("charge_wo_sys_dc_tou_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_oct = allocate("charge_wo_sys_dc_tou_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_nov = allocate("charge_wo_sys_dc_tou_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_dec = allocate("charge_wo_sys_dc_tou_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou = allocate("charge_wo_sys_dc_tou", nyears + 1);

		ssc_number_t *ch_wo_sys_ec_jan = allocate("charge_wo_sys_ec_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_feb = allocate("charge_wo_sys_ec_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_mar = allocate("charge_wo_sys_ec_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_apr = allocate("charge_wo_sys_ec_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_may = allocate("charge_wo_sys_ec_may", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_jun = allocate("charge_wo_sys_ec_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_jul = allocate("charge_wo_sys_ec_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_aug = allocate("charge_wo_sys_ec_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_sep = allocate("charge_wo_sys_ec_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_oct = allocate("charge_wo_sys_ec_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_nov = allocate("charge_wo_sys_ec_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_dec = allocate("charge_wo_sys_ec_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_ec = allocate("charge_wo_sys_ec", nyears + 1);

		ssc_number_t *ch_wo_sys_ec_flat_jan = allocate("charge_wo_sys_ec_flat_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_feb = allocate("charge_wo_sys_ec_flat_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_mar = allocate("charge_wo_sys_ec_flat_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_apr = allocate("charge_wo_sys_ec_flat_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_may = allocate("charge_wo_sys_ec_flat_may", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_jun = allocate("charge_wo_sys_ec_flat_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_jul = allocate("charge_wo_sys_ec_flat_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_aug = allocate("charge_wo_sys_ec_flat_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_sep = allocate("charge_wo_sys_ec_flat_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_oct = allocate("charge_wo_sys_ec_flat_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_nov = allocate("charge_wo_sys_ec_flat_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat_dec = allocate("charge_wo_sys_ec_flat_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_flat = allocate("charge_wo_sys_ec_flat", nyears + 1);

		ssc_number_t *ch_w_sys_fixed_jan = allocate("charge_w_sys_fixed_jan", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_feb = allocate("charge_w_sys_fixed_feb", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_mar = allocate("charge_w_sys_fixed_mar", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_apr = allocate("charge_w_sys_fixed_apr", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_may = allocate("charge_w_sys_fixed_may", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_jun = allocate("charge_w_sys_fixed_jun", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_jul = allocate("charge_w_sys_fixed_jul", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_aug = allocate("charge_w_sys_fixed_aug", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_sep = allocate("charge_w_sys_fixed_sep", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_oct = allocate("charge_w_sys_fixed_oct", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_nov = allocate("charge_w_sys_fixed_nov", nyears + 1);
		ssc_number_t *ch_w_sys_fixed_dec = allocate("charge_w_sys_fixed_dec", nyears + 1);
		ssc_number_t *ch_w_sys_fixed = allocate("charge_w_sys_fixed", nyears + 1);


		ssc_number_t *ch_wo_sys_fixed_jan = allocate("charge_wo_sys_fixed_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_feb = allocate("charge_wo_sys_fixed_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_mar = allocate("charge_wo_sys_fixed_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_apr = allocate("charge_wo_sys_fixed_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_may = allocate("charge_wo_sys_fixed_may", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_jun = allocate("charge_wo_sys_fixed_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_jul = allocate("charge_wo_sys_fixed_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_aug = allocate("charge_wo_sys_fixed_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_sep = allocate("charge_wo_sys_fixed_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_oct = allocate("charge_wo_sys_fixed_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_nov = allocate("charge_wo_sys_fixed_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed_dec = allocate("charge_wo_sys_fixed_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed = allocate("charge_wo_sys_fixed", nyears + 1);

		ssc_number_t *ch_w_sys_minimum_jan = allocate("charge_w_sys_minimum_jan", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_feb = allocate("charge_w_sys_minimum_feb", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_mar = allocate("charge_w_sys_minimum_mar", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_apr = allocate("charge_w_sys_minimum_apr", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_may = allocate("charge_w_sys_minimum_may", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_jun = allocate("charge_w_sys_minimum_jun", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_jul = allocate("charge_w_sys_minimum_jul", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_aug = allocate("charge_w_sys_minimum_aug", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_sep = allocate("charge_w_sys_minimum_sep", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_oct = allocate("charge_w_sys_minimum_oct", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_nov = allocate("charge_w_sys_minimum_nov", nyears + 1);
		ssc_number_t *ch_w_sys_minimum_dec = allocate("charge_w_sys_minimum_dec", nyears + 1);
		ssc_number_t *ch_w_sys_minimum = allocate("charge_w_sys_minimum", nyears + 1);


		ssc_number_t *ch_wo_sys_minimum_jan = allocate("charge_wo_sys_minimum_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_feb = allocate("charge_wo_sys_minimum_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_mar = allocate("charge_wo_sys_minimum_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_apr = allocate("charge_wo_sys_minimum_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_may = allocate("charge_wo_sys_minimum_may", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_jun = allocate("charge_wo_sys_minimum_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_jul = allocate("charge_wo_sys_minimum_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_aug = allocate("charge_wo_sys_minimum_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_sep = allocate("charge_wo_sys_minimum_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_oct = allocate("charge_wo_sys_minimum_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_nov = allocate("charge_wo_sys_minimum_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum_dec = allocate("charge_wo_sys_minimum_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum = allocate("charge_wo_sys_minimum", nyears + 1);




		// IRENA outputs array of tier values
		ssc_number_t *p_charge_wo_sys_ec_m_p[12][12];
		ssc_number_t *p_charge_w_sys_ec_m_p[12][12];
		ssc_number_t *p_energy_wo_sys_ec_m_p[12][12];
		ssc_number_t *p_energy_w_sys_ec_m_p[12][12];
		std::string postfix;
		// e.g. energy_w_sys_ec_dec_p12
		for (int m = 0; m < 12; m++)
		{
			for (int p = 0; p < 12; p++)
			{
				postfix = util::schedule_int_to_month(m) + "_p" + util::to_string((int)(p + 1));
				p_charge_wo_sys_ec_m_p[m][p] = allocate("charge_wo_sys_ec_" + postfix, 6);
				p_charge_w_sys_ec_m_p[m][p] = allocate("charge_w_sys_ec_" + postfix, 6);
				p_energy_wo_sys_ec_m_p[m][p] = allocate("energy_wo_sys_ec_" + postfix, 6);
				p_energy_w_sys_ec_m_p[m][p] = allocate("energy_w_sys_ec_" + postfix, 6);
			}
		}


		// lifetime hourly load
		ssc_number_t *lifetime_hourly_load = allocate("lifetime_load", nrec_gen);

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
			ur_calc( &e_load_cy[0], &p_load_cy[0],
				&revenue_wo_sys[0], &payment[0], &income[0], &price[0], &demand_charge[0], &energy_charge[0],
				&monthly_fixed_charges[0], &monthly_minimum_charges[0],
				&monthly_dc_fixed[0], &monthly_dc_tou[0],
				&monthly_ec_charges[0], &monthly_ec_flat_charges[0], &monthly_ec_rates[0], &ec_tou_sched[0], &dc_tou_sched[0], &dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], &monthly_e_use_period_tier[0], &monthly_charge_period_tier[0], rate_scale[i]);

	
			utility_bill_wo_sys_jan[i + 1] = monthly_bill[0];
			utility_bill_wo_sys_feb[i + 1] = monthly_bill[1];
			utility_bill_wo_sys_mar[i + 1] = monthly_bill[2];
			utility_bill_wo_sys_apr[i + 1] = monthly_bill[3];
			utility_bill_wo_sys_may[i + 1] = monthly_bill[4];
			utility_bill_wo_sys_jun[i + 1] = monthly_bill[5];
			utility_bill_wo_sys_jul[i + 1] = monthly_bill[6];
			utility_bill_wo_sys_aug[i + 1] = monthly_bill[7];
			utility_bill_wo_sys_sep[i + 1] = monthly_bill[8];
			utility_bill_wo_sys_oct[i + 1] = monthly_bill[9];
			utility_bill_wo_sys_nov[i + 1] = monthly_bill[10];
			utility_bill_wo_sys_dec[i + 1] = monthly_bill[11];
			utility_bill_wo_sys[i + 1] = utility_bill_wo_sys_jan[i + 1]
				+ utility_bill_wo_sys_feb[i + 1]
				+ utility_bill_wo_sys_mar[i + 1]
				+ utility_bill_wo_sys_apr[i + 1]
				+ utility_bill_wo_sys_may[i + 1]
				+ utility_bill_wo_sys_jun[i + 1]
				+ utility_bill_wo_sys_jul[i + 1]
				+ utility_bill_wo_sys_aug[i + 1]
				+ utility_bill_wo_sys_sep[i + 1]
				+ utility_bill_wo_sys_oct[i + 1]
				+ utility_bill_wo_sys_nov[i + 1]
				+ utility_bill_wo_sys_dec[i + 1];



			ch_wo_sys_dc_fixed_jan[i + 1] = monthly_dc_fixed[0];
			ch_wo_sys_dc_fixed_feb[i + 1] = monthly_dc_fixed[1];
			ch_wo_sys_dc_fixed_mar[i + 1] = monthly_dc_fixed[2];
			ch_wo_sys_dc_fixed_apr[i + 1] = monthly_dc_fixed[3];
			ch_wo_sys_dc_fixed_may[i + 1] = monthly_dc_fixed[4];
			ch_wo_sys_dc_fixed_jun[i + 1] = monthly_dc_fixed[5];
			ch_wo_sys_dc_fixed_jul[i + 1] = monthly_dc_fixed[6];
			ch_wo_sys_dc_fixed_aug[i + 1] = monthly_dc_fixed[7];
			ch_wo_sys_dc_fixed_sep[i + 1] = monthly_dc_fixed[8];
			ch_wo_sys_dc_fixed_oct[i + 1] = monthly_dc_fixed[9];
			ch_wo_sys_dc_fixed_nov[i + 1] = monthly_dc_fixed[10];
			ch_wo_sys_dc_fixed_dec[i + 1] = monthly_dc_fixed[11];
			ch_wo_sys_dc_fixed[i + 1] = ch_wo_sys_dc_fixed_jan[i + 1]
				+ ch_wo_sys_dc_fixed_feb[i + 1]
				+ ch_wo_sys_dc_fixed_mar[i + 1]
				+ ch_wo_sys_dc_fixed_apr[i + 1]
				+ ch_wo_sys_dc_fixed_may[i + 1]
				+ ch_wo_sys_dc_fixed_jun[i + 1]
				+ ch_wo_sys_dc_fixed_jul[i + 1]
				+ ch_wo_sys_dc_fixed_aug[i + 1]
				+ ch_wo_sys_dc_fixed_sep[i + 1]
				+ ch_wo_sys_dc_fixed_oct[i + 1]
				+ ch_wo_sys_dc_fixed_nov[i + 1]
				+ ch_wo_sys_dc_fixed_dec[i + 1];


			ch_wo_sys_dc_tou_jan[i + 1] = monthly_dc_tou[0];
			ch_wo_sys_dc_tou_feb[i + 1] = monthly_dc_tou[1];
			ch_wo_sys_dc_tou_mar[i + 1] = monthly_dc_tou[2];
			ch_wo_sys_dc_tou_apr[i + 1] = monthly_dc_tou[3];
			ch_wo_sys_dc_tou_may[i + 1] = monthly_dc_tou[4];
			ch_wo_sys_dc_tou_jun[i + 1] = monthly_dc_tou[5];
			ch_wo_sys_dc_tou_jul[i + 1] = monthly_dc_tou[6];
			ch_wo_sys_dc_tou_aug[i + 1] = monthly_dc_tou[7];
			ch_wo_sys_dc_tou_sep[i + 1] = monthly_dc_tou[8];
			ch_wo_sys_dc_tou_oct[i + 1] = monthly_dc_tou[9];
			ch_wo_sys_dc_tou_nov[i + 1] = monthly_dc_tou[10];
			ch_wo_sys_dc_tou_dec[i + 1] = monthly_dc_tou[11];
			ch_wo_sys_dc_tou[i + 1] = ch_wo_sys_dc_tou_jan[i + 1]
				+ ch_wo_sys_dc_tou_feb[i + 1]
				+ ch_wo_sys_dc_tou_mar[i + 1]
				+ ch_wo_sys_dc_tou_apr[i + 1]
				+ ch_wo_sys_dc_tou_may[i + 1]
				+ ch_wo_sys_dc_tou_jun[i + 1]
				+ ch_wo_sys_dc_tou_jul[i + 1]
				+ ch_wo_sys_dc_tou_aug[i + 1]
				+ ch_wo_sys_dc_tou_sep[i + 1]
				+ ch_wo_sys_dc_tou_oct[i + 1]
				+ ch_wo_sys_dc_tou_nov[i + 1]
				+ ch_wo_sys_dc_tou_dec[i + 1];

			ch_wo_sys_ec_jan[i + 1] = monthly_ec_charges[0];
			ch_wo_sys_ec_feb[i + 1] = monthly_ec_charges[1];
			ch_wo_sys_ec_mar[i + 1] = monthly_ec_charges[2];
			ch_wo_sys_ec_apr[i + 1] = monthly_ec_charges[3];
			ch_wo_sys_ec_may[i + 1] = monthly_ec_charges[4];
			ch_wo_sys_ec_jun[i + 1] = monthly_ec_charges[5];
			ch_wo_sys_ec_jul[i + 1] = monthly_ec_charges[6];
			ch_wo_sys_ec_aug[i + 1] = monthly_ec_charges[7];
			ch_wo_sys_ec_sep[i + 1] = monthly_ec_charges[8];
			ch_wo_sys_ec_oct[i + 1] = monthly_ec_charges[9];
			ch_wo_sys_ec_nov[i + 1] = monthly_ec_charges[10];
			ch_wo_sys_ec_dec[i + 1] = monthly_ec_charges[11];
			ch_wo_sys_ec[i + 1] = ch_wo_sys_ec_jan[i + 1]
				+ ch_wo_sys_ec_feb[i + 1]
				+ ch_wo_sys_ec_mar[i + 1]
				+ ch_wo_sys_ec_apr[i + 1]
				+ ch_wo_sys_ec_may[i + 1]
				+ ch_wo_sys_ec_jun[i + 1]
				+ ch_wo_sys_ec_jul[i + 1]
				+ ch_wo_sys_ec_aug[i + 1]
				+ ch_wo_sys_ec_sep[i + 1]
				+ ch_wo_sys_ec_oct[i + 1]
				+ ch_wo_sys_ec_nov[i + 1]
				+ ch_wo_sys_ec_dec[i + 1];

			ch_wo_sys_ec_flat_jan[i + 1] = monthly_ec_flat_charges[0];
			ch_wo_sys_ec_flat_feb[i + 1] = monthly_ec_flat_charges[1];
			ch_wo_sys_ec_flat_mar[i + 1] = monthly_ec_flat_charges[2];
			ch_wo_sys_ec_flat_apr[i + 1] = monthly_ec_flat_charges[3];
			ch_wo_sys_ec_flat_may[i + 1] = monthly_ec_flat_charges[4];
			ch_wo_sys_ec_flat_jun[i + 1] = monthly_ec_flat_charges[5];
			ch_wo_sys_ec_flat_jul[i + 1] = monthly_ec_flat_charges[6];
			ch_wo_sys_ec_flat_aug[i + 1] = monthly_ec_flat_charges[7];
			ch_wo_sys_ec_flat_sep[i + 1] = monthly_ec_flat_charges[8];
			ch_wo_sys_ec_flat_oct[i + 1] = monthly_ec_flat_charges[9];
			ch_wo_sys_ec_flat_nov[i + 1] = monthly_ec_flat_charges[10];
			ch_wo_sys_ec_flat_dec[i + 1] = monthly_ec_flat_charges[11];
			ch_wo_sys_ec_flat[i + 1] = ch_wo_sys_ec_flat_jan[i + 1]
				+ ch_wo_sys_ec_flat_feb[i + 1]
				+ ch_wo_sys_ec_flat_mar[i + 1]
				+ ch_wo_sys_ec_flat_apr[i + 1]
				+ ch_wo_sys_ec_flat_may[i + 1]
				+ ch_wo_sys_ec_flat_jun[i + 1]
				+ ch_wo_sys_ec_flat_jul[i + 1]
				+ ch_wo_sys_ec_flat_aug[i + 1]
				+ ch_wo_sys_ec_flat_sep[i + 1]
				+ ch_wo_sys_ec_flat_oct[i + 1]
				+ ch_wo_sys_ec_flat_nov[i + 1]
				+ ch_wo_sys_ec_flat_dec[i + 1];

			ch_wo_sys_fixed_jan[i + 1] = monthly_fixed_charges[0];
			ch_wo_sys_fixed_feb[i + 1] = monthly_fixed_charges[1];
			ch_wo_sys_fixed_mar[i + 1] = monthly_fixed_charges[2];
			ch_wo_sys_fixed_apr[i + 1] = monthly_fixed_charges[3];
			ch_wo_sys_fixed_may[i + 1] = monthly_fixed_charges[4];
			ch_wo_sys_fixed_jun[i + 1] = monthly_fixed_charges[5];
			ch_wo_sys_fixed_jul[i + 1] = monthly_fixed_charges[6];
			ch_wo_sys_fixed_aug[i + 1] = monthly_fixed_charges[7];
			ch_wo_sys_fixed_sep[i + 1] = monthly_fixed_charges[8];
			ch_wo_sys_fixed_oct[i + 1] = monthly_fixed_charges[9];
			ch_wo_sys_fixed_nov[i + 1] = monthly_fixed_charges[10];
			ch_wo_sys_fixed_dec[i + 1] = monthly_fixed_charges[11];
			ch_wo_sys_fixed[i + 1] = ch_wo_sys_fixed_jan[i + 1]
				+ ch_wo_sys_fixed_feb[i + 1]
				+ ch_wo_sys_fixed_mar[i + 1]
				+ ch_wo_sys_fixed_apr[i + 1]
				+ ch_wo_sys_fixed_may[i + 1]
				+ ch_wo_sys_fixed_jun[i + 1]
				+ ch_wo_sys_fixed_jul[i + 1]
				+ ch_wo_sys_fixed_aug[i + 1]
				+ ch_wo_sys_fixed_sep[i + 1]
				+ ch_wo_sys_fixed_oct[i + 1]
				+ ch_wo_sys_fixed_nov[i + 1]
				+ ch_wo_sys_fixed_dec[i + 1];

			ch_wo_sys_minimum_jan[i + 1] = monthly_minimum_charges[0];
			ch_wo_sys_minimum_feb[i + 1] = monthly_minimum_charges[1];
			ch_wo_sys_minimum_mar[i + 1] = monthly_minimum_charges[2];
			ch_wo_sys_minimum_apr[i + 1] = monthly_minimum_charges[3];
			ch_wo_sys_minimum_may[i + 1] = monthly_minimum_charges[4];
			ch_wo_sys_minimum_jun[i + 1] = monthly_minimum_charges[5];
			ch_wo_sys_minimum_jul[i + 1] = monthly_minimum_charges[6];
			ch_wo_sys_minimum_aug[i + 1] = monthly_minimum_charges[7];
			ch_wo_sys_minimum_sep[i + 1] = monthly_minimum_charges[8];
			ch_wo_sys_minimum_oct[i + 1] = monthly_minimum_charges[9];
			ch_wo_sys_minimum_nov[i + 1] = monthly_minimum_charges[10];
			ch_wo_sys_minimum_dec[i + 1] = monthly_minimum_charges[11];
			ch_wo_sys_minimum[i + 1] = ch_wo_sys_minimum_jan[i + 1]
				+ ch_wo_sys_minimum_feb[i + 1]
				+ ch_wo_sys_minimum_mar[i + 1]
				+ ch_wo_sys_minimum_apr[i + 1]
				+ ch_wo_sys_minimum_may[i + 1]
				+ ch_wo_sys_minimum_jun[i + 1]
				+ ch_wo_sys_minimum_jul[i + 1]
				+ ch_wo_sys_minimum_aug[i + 1]
				+ ch_wo_sys_minimum_sep[i + 1]
				+ ch_wo_sys_minimum_oct[i + 1]
				+ ch_wo_sys_minimum_nov[i + 1]
				+ ch_wo_sys_minimum_dec[i + 1];







			if (i == 0)
			{
				// IRENA
				for (int m = 0; m < 12; m++)
				{
					for (int p = 0; p < 12; p++)
					{
						for (int t = 0; t < 6; t++)
						{
							p_charge_wo_sys_ec_m_p[m][p][t] = monthly_charge_period_tier[m][p][t];
							p_energy_wo_sys_ec_m_p[m][p][t] = monthly_e_use_period_tier[m][p][t];
						}
					}
				}


				//assign( "year1_hourly_revenue_without_system", var_data( &revenue_wo_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_without_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_without_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_without_system", var_data( &price[0], 8760 ) );
				assign("year1_hourly_dc_without_system", var_data(&demand_charge[0], 8760));
				assign("year1_hourly_ec_without_system", var_data(&energy_charge[0], 8760));

				assign( "year1_monthly_dc_fixed_without_system", var_data(&monthly_dc_fixed[0], 12) );
				assign( "year1_monthly_dc_tou_without_system", var_data(&monthly_dc_tou[0], 12) );
				assign("year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12));
				assign("year1_monthly_ec_charge_flat_without_system", var_data(&monthly_ec_flat_charges[0], 12));
				//assign( "year1_monthly_ec_rate_without_system", var_data(&monthly_ec_rates[0], 12) );

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
			}



			// false = 2 meters, load and system treated separately
			// true = 1 meter, net grid energy used for bill calculation with either energy or dollar rollover.
			bool enable_nm = as_boolean("ur_enable_net_metering");



			if (enable_nm)
			{
				// calculate revenue with solar system (using net grid energy & maxpower)
				ur_calc(&e_grid[0], &p_grid[0],
					&revenue_w_sys[0], &payment[0], &income[0], &price[0], &demand_charge[0],
					&energy_charge[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0], &monthly_ec_flat_charges[0], &monthly_ec_rates[0], &ec_tou_sched[0], &dc_tou_sched[0], &dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], &monthly_e_use_period_tier[0], &monthly_charge_period_tier[0], rate_scale[i]);
			}
			else
			{
				// calculate revenue with solar system (using system energy & maxpower)
				ur_calc(&e_sys_cy[0], &p_sys_cy[0],
					&revenue_w_sys[0], &payment[0], &income[0], &price[0], &demand_charge[0],
					&energy_charge[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_dc_fixed[0], &monthly_dc_tou[0],
					&monthly_ec_charges[0], &monthly_ec_flat_charges[0], &monthly_ec_rates[0], &ec_tou_sched[0], &dc_tou_sched[0], &dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0], &monthly_e_use_period_tier[0], &monthly_charge_period_tier[0], rate_scale[i], false, false);
				// TODO - remove annual_revenue and just use annual bill
				// Two meters - adjust output accordingly
				for (j = 0; j<8760; j++)
				{
					revenue_w_sys[j] += revenue_wo_sys[j]; // watch sign
					annual_revenue_w_sys[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				}
				// adjust monthly outputs as sum of both meters = system meter + load meter 
				monthly_dc_fixed[0] += ch_wo_sys_dc_fixed_jan[i + 1];
				monthly_dc_fixed[1] += ch_wo_sys_dc_fixed_feb[i + 1];
				monthly_dc_fixed[2] += ch_wo_sys_dc_fixed_mar[i + 1];
				monthly_dc_fixed[3] += ch_wo_sys_dc_fixed_apr[i + 1];
				monthly_dc_fixed[4] += ch_wo_sys_dc_fixed_may[i + 1];
				monthly_dc_fixed[5] += ch_wo_sys_dc_fixed_jun[i + 1];
				monthly_dc_fixed[6] += ch_wo_sys_dc_fixed_jul[i + 1];
				monthly_dc_fixed[7] += ch_wo_sys_dc_fixed_aug[i + 1];
				monthly_dc_fixed[8] += ch_wo_sys_dc_fixed_sep[i + 1];
				monthly_dc_fixed[9] += ch_wo_sys_dc_fixed_oct[i + 1];
				monthly_dc_fixed[10] += ch_wo_sys_dc_fixed_nov[i + 1];
				monthly_dc_fixed[11] += ch_wo_sys_dc_fixed_dec[i + 1];

				monthly_dc_tou[0] += ch_wo_sys_dc_tou_jan[i + 1];
				monthly_dc_tou[1] += ch_wo_sys_dc_tou_feb[i + 1];
				monthly_dc_tou[2] += ch_wo_sys_dc_tou_mar[i + 1];
				monthly_dc_tou[3] += ch_wo_sys_dc_tou_apr[i + 1];
				monthly_dc_tou[4] += ch_wo_sys_dc_tou_may[i + 1];
				monthly_dc_tou[5] += ch_wo_sys_dc_tou_jun[i + 1];
				monthly_dc_tou[6] += ch_wo_sys_dc_tou_jul[i + 1];
				monthly_dc_tou[7] += ch_wo_sys_dc_tou_aug[i + 1];
				monthly_dc_tou[8] += ch_wo_sys_dc_tou_sep[i + 1];
				monthly_dc_tou[9] += ch_wo_sys_dc_tou_oct[i + 1];
				monthly_dc_tou[10] += ch_wo_sys_dc_tou_nov[i + 1];
				monthly_dc_tou[11] += ch_wo_sys_dc_tou_dec[i + 1];

				monthly_ec_charges[0] += ch_wo_sys_ec_jan[i + 1];
				monthly_ec_charges[1] += ch_wo_sys_ec_feb[i + 1];
				monthly_ec_charges[2] += ch_wo_sys_ec_mar[i + 1];
				monthly_ec_charges[3] += ch_wo_sys_ec_apr[i + 1];
				monthly_ec_charges[4] += ch_wo_sys_ec_may[i + 1];
				monthly_ec_charges[5] += ch_wo_sys_ec_jun[i + 1];
				monthly_ec_charges[6] += ch_wo_sys_ec_jul[i + 1];
				monthly_ec_charges[7] += ch_wo_sys_ec_aug[i + 1];
				monthly_ec_charges[8] += ch_wo_sys_ec_sep[i + 1];
				monthly_ec_charges[9] += ch_wo_sys_ec_oct[i + 1];
				monthly_ec_charges[10] += ch_wo_sys_ec_nov[i + 1];
				monthly_ec_charges[11] += ch_wo_sys_ec_dec[i + 1];

				monthly_ec_flat_charges[0] += ch_wo_sys_ec_flat_jan[i + 1];
				monthly_ec_flat_charges[1] += ch_wo_sys_ec_flat_feb[i + 1];
				monthly_ec_flat_charges[2] += ch_wo_sys_ec_flat_mar[i + 1];
				monthly_ec_flat_charges[3] += ch_wo_sys_ec_flat_apr[i + 1];
				monthly_ec_flat_charges[4] += ch_wo_sys_ec_flat_may[i + 1];
				monthly_ec_flat_charges[5] += ch_wo_sys_ec_flat_jun[i + 1];
				monthly_ec_flat_charges[6] += ch_wo_sys_ec_flat_jul[i + 1];
				monthly_ec_flat_charges[7] += ch_wo_sys_ec_flat_aug[i + 1];
				monthly_ec_flat_charges[8] += ch_wo_sys_ec_flat_sep[i + 1];
				monthly_ec_flat_charges[9] += ch_wo_sys_ec_flat_oct[i + 1];
				monthly_ec_flat_charges[10] += ch_wo_sys_ec_flat_nov[i + 1];
				monthly_ec_flat_charges[11] += ch_wo_sys_ec_flat_dec[i + 1];

				monthly_fixed_charges[0] += ch_wo_sys_fixed_jan[i + 1];
				monthly_fixed_charges[1] += ch_wo_sys_fixed_feb[i + 1];
				monthly_fixed_charges[2] += ch_wo_sys_fixed_mar[i + 1];
				monthly_fixed_charges[3] += ch_wo_sys_fixed_apr[i + 1];
				monthly_fixed_charges[4] += ch_wo_sys_fixed_may[i + 1];
				monthly_fixed_charges[5] += ch_wo_sys_fixed_jun[i + 1];
				monthly_fixed_charges[6] += ch_wo_sys_fixed_jul[i + 1];
				monthly_fixed_charges[7] += ch_wo_sys_fixed_aug[i + 1];
				monthly_fixed_charges[8] += ch_wo_sys_fixed_sep[i + 1];
				monthly_fixed_charges[9] += ch_wo_sys_fixed_oct[i + 1];
				monthly_fixed_charges[10] += ch_wo_sys_fixed_nov[i + 1];
				monthly_fixed_charges[11] += ch_wo_sys_fixed_dec[i + 1];

				monthly_minimum_charges[0] += ch_wo_sys_minimum_jan[i + 1];
				monthly_minimum_charges[1] += ch_wo_sys_minimum_feb[i + 1];
				monthly_minimum_charges[2] += ch_wo_sys_minimum_mar[i + 1];
				monthly_minimum_charges[3] += ch_wo_sys_minimum_apr[i + 1];
				monthly_minimum_charges[4] += ch_wo_sys_minimum_may[i + 1];
				monthly_minimum_charges[5] += ch_wo_sys_minimum_jun[i + 1];
				monthly_minimum_charges[6] += ch_wo_sys_minimum_jul[i + 1];
				monthly_minimum_charges[7] += ch_wo_sys_minimum_aug[i + 1];
				monthly_minimum_charges[8] += ch_wo_sys_minimum_sep[i + 1];
				monthly_minimum_charges[9] += ch_wo_sys_minimum_oct[i + 1];
				monthly_minimum_charges[10] += ch_wo_sys_minimum_nov[i + 1];
				monthly_minimum_charges[11] += ch_wo_sys_minimum_dec[i + 1];

				monthly_bill[0] += utility_bill_wo_sys_jan[i + 1];
				monthly_bill[1] += utility_bill_wo_sys_feb[i + 1];
				monthly_bill[2] += utility_bill_wo_sys_mar[i + 1];
				monthly_bill[3] += utility_bill_wo_sys_apr[i + 1];
				monthly_bill[4] += utility_bill_wo_sys_may[i + 1];
				monthly_bill[5] += utility_bill_wo_sys_jun[i + 1];
				monthly_bill[6] += utility_bill_wo_sys_jul[i + 1];
				monthly_bill[7] += utility_bill_wo_sys_aug[i + 1];
				monthly_bill[8] += utility_bill_wo_sys_sep[i + 1];
				monthly_bill[9] += utility_bill_wo_sys_oct[i + 1];
				monthly_bill[10] += utility_bill_wo_sys_nov[i + 1];
				monthly_bill[11] += utility_bill_wo_sys_dec[i + 1];

				if (i == 0)
				{
					// IRENA
					for (int m = 0; m < 12; m++)
					{
						for (int p = 0; p < 12; p++)
						{
							for (int t = 0; t < 6; t++)
							{
								monthly_charge_period_tier[m][p][t] += p_charge_wo_sys_ec_m_p[m][p][t];
								monthly_e_use_period_tier[m][p][t] += p_energy_wo_sys_ec_m_p[m][p][t];
							}
						}
					}
				}


			}


			if (i == 0)
			{
				// IRENA
				for (int m = 0; m < 12; m++)
				{
					for (int p = 0; p < 12; p++)
					{
						for (int t = 0; t < 6; t++)
						{
							p_charge_w_sys_ec_m_p[m][p][t] = monthly_charge_period_tier[m][p][t];
							p_energy_w_sys_ec_m_p[m][p][t] = monthly_e_use_period_tier[m][p][t];
						}
					}
				}
				//assign( "year1_hourly_revenue_with_system", var_data( &revenue_w_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_with_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_with_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_with_system", var_data( &price[0], 8760 ) );
				assign("year1_hourly_dc_with_system", var_data(&demand_charge[0], 8760));
				assign("year1_hourly_ec_with_system", var_data(&energy_charge[0], 8760));
				//				assign( "year1_hourly_e_grid", var_data( &e_grid[0], 8760 ) );
				//				assign( "year1_hourly_p_grid", var_data( &p_grid[0], 8760 ) );
				assign("year1_hourly_ec_tou_schedule", var_data(&ec_tou_sched[0], 8760));
				assign("year1_hourly_dc_tou_schedule", var_data(&dc_tou_sched[0], 8760));
				assign("year1_hourly_dc_peak_per_period", var_data(&dc_hourly_peak[0], 8760));

				// sign reversal based on 9/5/13 meeting reverse again 9/6/13
				for (int ii = 0; ii<8760; ii++)
				{
					load[ii] = -e_load[ii];
					e_tofromgrid[ii] = e_grid[ii];
					p_tofromgrid[ii] = p_grid[ii];
					salespurchases[ii] = revenue_w_sys[ii];
				}
				// monthly outputs - Paul and Sean 7/29/13 - updated 8/9/13 and 8/12/13 and 9/10/13
				monthly_outputs(&e_load[0], &e_sys_cy[0], &e_grid[0], &salespurchases[0],
					&monthly_load[0], &monthly_system_generation[0], &monthly_elec_to_grid[0],
					&monthly_elec_needed_from_grid[0],
					&monthly_salespurchases[0]);

				assign("year1_hourly_e_tofromgrid", var_data(&e_tofromgrid[0], 8760));
				assign("year1_hourly_p_tofromgrid", var_data(&p_tofromgrid[0], 8760));
				assign("year1_hourly_load", var_data(&load[0], 8760));
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

				assign("year1_hourly_system_to_grid", var_data(&e_sys_to_grid[0], 8760));
				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], 8760));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], 8760));

				assign("year1_monthly_fixed_with_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_with_system", var_data(&monthly_minimum_charges[0], 12));
				assign("year1_monthly_dc_fixed_with_system", var_data(&monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
				assign("year1_monthly_ec_charge_flat_with_system", var_data(&monthly_ec_flat_charges[0], 12));
				//assign( "year1_monthly_ec_rate_with_system", var_data(&monthly_ec_rates[0], 12) );
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




			utility_bill_w_sys_jan[i + 1] = monthly_bill[0];
			utility_bill_w_sys_feb[i + 1] = monthly_bill[1];
			utility_bill_w_sys_mar[i + 1] = monthly_bill[2];
			utility_bill_w_sys_apr[i + 1] = monthly_bill[3];
			utility_bill_w_sys_may[i + 1] = monthly_bill[4];
			utility_bill_w_sys_jun[i + 1] = monthly_bill[5];
			utility_bill_w_sys_jul[i + 1] = monthly_bill[6];
			utility_bill_w_sys_aug[i + 1] = monthly_bill[7];
			utility_bill_w_sys_sep[i + 1] = monthly_bill[8];
			utility_bill_w_sys_oct[i + 1] = monthly_bill[9];
			utility_bill_w_sys_nov[i + 1] = monthly_bill[10];
			utility_bill_w_sys_dec[i + 1] = monthly_bill[11];
			utility_bill_w_sys[i + 1] = utility_bill_w_sys_jan[i + 1]
				+ utility_bill_w_sys_feb[i + 1]
				+ utility_bill_w_sys_mar[i + 1]
				+ utility_bill_w_sys_apr[i + 1]
				+ utility_bill_w_sys_may[i + 1]
				+ utility_bill_w_sys_jun[i + 1]
				+ utility_bill_w_sys_jul[i + 1]
				+ utility_bill_w_sys_aug[i + 1]
				+ utility_bill_w_sys_sep[i + 1]
				+ utility_bill_w_sys_oct[i + 1]
				+ utility_bill_w_sys_nov[i + 1]
				+ utility_bill_w_sys_dec[i + 1];




			ch_w_sys_dc_fixed_jan[i + 1] = monthly_dc_fixed[0] ;
			ch_w_sys_dc_fixed_feb[i + 1] = monthly_dc_fixed[1] ;
			ch_w_sys_dc_fixed_mar[i + 1] = monthly_dc_fixed[2] ;
			ch_w_sys_dc_fixed_apr[i + 1] = monthly_dc_fixed[3] ;
			ch_w_sys_dc_fixed_may[i + 1] = monthly_dc_fixed[4] ;
			ch_w_sys_dc_fixed_jun[i + 1] = monthly_dc_fixed[5] ;
			ch_w_sys_dc_fixed_jul[i + 1] = monthly_dc_fixed[6] ;
			ch_w_sys_dc_fixed_aug[i + 1] = monthly_dc_fixed[7] ;
			ch_w_sys_dc_fixed_sep[i + 1] = monthly_dc_fixed[8] ;
			ch_w_sys_dc_fixed_oct[i + 1] = monthly_dc_fixed[9] ;
			ch_w_sys_dc_fixed_nov[i + 1] = monthly_dc_fixed[10] ;
			ch_w_sys_dc_fixed_dec[i + 1] = monthly_dc_fixed[11] ;
			ch_w_sys_dc_fixed[i + 1] = ch_w_sys_dc_fixed_jan[i + 1]
				+ ch_w_sys_dc_fixed_feb[i + 1]
				+ ch_w_sys_dc_fixed_mar[i + 1]
				+ ch_w_sys_dc_fixed_apr[i + 1]
				+ ch_w_sys_dc_fixed_may[i + 1]
				+ ch_w_sys_dc_fixed_jun[i + 1]
				+ ch_w_sys_dc_fixed_jul[i + 1]
				+ ch_w_sys_dc_fixed_aug[i + 1]
				+ ch_w_sys_dc_fixed_sep[i + 1]
				+ ch_w_sys_dc_fixed_oct[i + 1]
				+ ch_w_sys_dc_fixed_nov[i + 1]
				+ ch_w_sys_dc_fixed_dec[i + 1];

		
			ch_w_sys_dc_tou_jan[i + 1] = monthly_dc_tou[0] ;
			ch_w_sys_dc_tou_feb[i + 1] = monthly_dc_tou[1] ;
			ch_w_sys_dc_tou_mar[i + 1] = monthly_dc_tou[2] ;
			ch_w_sys_dc_tou_apr[i + 1] = monthly_dc_tou[3] ;
			ch_w_sys_dc_tou_may[i + 1] = monthly_dc_tou[4] ;
			ch_w_sys_dc_tou_jun[i + 1] = monthly_dc_tou[5] ;
			ch_w_sys_dc_tou_jul[i + 1] = monthly_dc_tou[6] ;
			ch_w_sys_dc_tou_aug[i + 1] = monthly_dc_tou[7] ;
			ch_w_sys_dc_tou_sep[i + 1] = monthly_dc_tou[8] ;
			ch_w_sys_dc_tou_oct[i + 1] = monthly_dc_tou[9] ;
			ch_w_sys_dc_tou_nov[i + 1] = monthly_dc_tou[10] ;
			ch_w_sys_dc_tou_dec[i + 1] = monthly_dc_tou[11] ;
			ch_w_sys_dc_tou[i + 1] = ch_w_sys_dc_tou_jan[i + 1]
				+ ch_w_sys_dc_tou_feb[i + 1]
				+ ch_w_sys_dc_tou_mar[i + 1]
				+ ch_w_sys_dc_tou_apr[i + 1]
				+ ch_w_sys_dc_tou_may[i + 1]
				+ ch_w_sys_dc_tou_jun[i + 1]
				+ ch_w_sys_dc_tou_jul[i + 1]
				+ ch_w_sys_dc_tou_aug[i + 1]
				+ ch_w_sys_dc_tou_sep[i + 1]
				+ ch_w_sys_dc_tou_oct[i + 1]
				+ ch_w_sys_dc_tou_nov[i + 1]
				+ ch_w_sys_dc_tou_dec[i + 1];

			ch_w_sys_ec_jan[i + 1] = monthly_ec_charges[0] ;
			ch_w_sys_ec_feb[i + 1] = monthly_ec_charges[1] ;
			ch_w_sys_ec_mar[i + 1] = monthly_ec_charges[2] ;
			ch_w_sys_ec_apr[i + 1] = monthly_ec_charges[3] ;
			ch_w_sys_ec_may[i + 1] = monthly_ec_charges[4] ;
			ch_w_sys_ec_jun[i + 1] = monthly_ec_charges[5] ;
			ch_w_sys_ec_jul[i + 1] = monthly_ec_charges[6] ;
			ch_w_sys_ec_aug[i + 1] = monthly_ec_charges[7] ;
			ch_w_sys_ec_sep[i + 1] = monthly_ec_charges[8] ;
			ch_w_sys_ec_oct[i + 1] = monthly_ec_charges[9] ;
			ch_w_sys_ec_nov[i + 1] = monthly_ec_charges[10] ;
			ch_w_sys_ec_dec[i + 1] = monthly_ec_charges[11] ;			
			ch_w_sys_ec[i + 1] = ch_w_sys_ec_jan[i + 1]
				+ ch_w_sys_ec_feb[i + 1]
				+ ch_w_sys_ec_mar[i + 1]
				+ ch_w_sys_ec_apr[i + 1]
				+ ch_w_sys_ec_may[i + 1]
				+ ch_w_sys_ec_jun[i + 1]
				+ ch_w_sys_ec_jul[i + 1]
				+ ch_w_sys_ec_aug[i + 1]
				+ ch_w_sys_ec_sep[i + 1]
				+ ch_w_sys_ec_oct[i + 1]
				+ ch_w_sys_ec_nov[i + 1]
				+ ch_w_sys_ec_dec[i + 1];


			ch_w_sys_ec_flat_jan[i + 1] = monthly_ec_flat_charges[0];
			ch_w_sys_ec_flat_feb[i + 1] = monthly_ec_flat_charges[1];
			ch_w_sys_ec_flat_mar[i + 1] = monthly_ec_flat_charges[2];
			ch_w_sys_ec_flat_apr[i + 1] = monthly_ec_flat_charges[3];
			ch_w_sys_ec_flat_may[i + 1] = monthly_ec_flat_charges[4];
			ch_w_sys_ec_flat_jun[i + 1] = monthly_ec_flat_charges[5];
			ch_w_sys_ec_flat_jul[i + 1] = monthly_ec_flat_charges[6];
			ch_w_sys_ec_flat_aug[i + 1] = monthly_ec_flat_charges[7];
			ch_w_sys_ec_flat_sep[i + 1] = monthly_ec_flat_charges[8];
			ch_w_sys_ec_flat_oct[i + 1] = monthly_ec_flat_charges[9];
			ch_w_sys_ec_flat_nov[i + 1] = monthly_ec_flat_charges[10];
			ch_w_sys_ec_flat_dec[i + 1] = monthly_ec_flat_charges[11];
			ch_w_sys_ec_flat[i + 1] = ch_w_sys_ec_flat_jan[i + 1]
				+ ch_w_sys_ec_flat_feb[i + 1]
				+ ch_w_sys_ec_flat_mar[i + 1]
				+ ch_w_sys_ec_flat_apr[i + 1]
				+ ch_w_sys_ec_flat_may[i + 1]
				+ ch_w_sys_ec_flat_jun[i + 1]
				+ ch_w_sys_ec_flat_jul[i + 1]
				+ ch_w_sys_ec_flat_aug[i + 1]
				+ ch_w_sys_ec_flat_sep[i + 1]
				+ ch_w_sys_ec_flat_oct[i + 1]
				+ ch_w_sys_ec_flat_nov[i + 1]
				+ ch_w_sys_ec_flat_dec[i + 1];


			ch_w_sys_fixed_jan[i + 1] = monthly_fixed_charges[0];
			ch_w_sys_fixed_feb[i + 1] = monthly_fixed_charges[1];
			ch_w_sys_fixed_mar[i + 1] = monthly_fixed_charges[2];
			ch_w_sys_fixed_apr[i + 1] = monthly_fixed_charges[3];
			ch_w_sys_fixed_may[i + 1] = monthly_fixed_charges[4];
			ch_w_sys_fixed_jun[i + 1] = monthly_fixed_charges[5];
			ch_w_sys_fixed_jul[i + 1] = monthly_fixed_charges[6];
			ch_w_sys_fixed_aug[i + 1] = monthly_fixed_charges[7];
			ch_w_sys_fixed_sep[i + 1] = monthly_fixed_charges[8];
			ch_w_sys_fixed_oct[i + 1] = monthly_fixed_charges[9];
			ch_w_sys_fixed_nov[i + 1] = monthly_fixed_charges[10];
			ch_w_sys_fixed_dec[i + 1] = monthly_fixed_charges[11];
			ch_w_sys_fixed[i + 1] = ch_w_sys_fixed_jan[i + 1]
				+ ch_w_sys_fixed_feb[i + 1]
				+ ch_w_sys_fixed_mar[i + 1]
				+ ch_w_sys_fixed_apr[i + 1]
				+ ch_w_sys_fixed_may[i + 1]
				+ ch_w_sys_fixed_jun[i + 1]
				+ ch_w_sys_fixed_jul[i + 1]
				+ ch_w_sys_fixed_aug[i + 1]
				+ ch_w_sys_fixed_sep[i + 1]
				+ ch_w_sys_fixed_oct[i + 1]
				+ ch_w_sys_fixed_nov[i + 1]
				+ ch_w_sys_fixed_dec[i + 1];

			ch_w_sys_minimum_jan[i + 1] = monthly_minimum_charges[0];
			ch_w_sys_minimum_feb[i + 1] = monthly_minimum_charges[1];
			ch_w_sys_minimum_mar[i + 1] = monthly_minimum_charges[2];
			ch_w_sys_minimum_apr[i + 1] = monthly_minimum_charges[3];
			ch_w_sys_minimum_may[i + 1] = monthly_minimum_charges[4];
			ch_w_sys_minimum_jun[i + 1] = monthly_minimum_charges[5];
			ch_w_sys_minimum_jul[i + 1] = monthly_minimum_charges[6];
			ch_w_sys_minimum_aug[i + 1] = monthly_minimum_charges[7];
			ch_w_sys_minimum_sep[i + 1] = monthly_minimum_charges[8];
			ch_w_sys_minimum_oct[i + 1] = monthly_minimum_charges[9];
			ch_w_sys_minimum_nov[i + 1] = monthly_minimum_charges[10];
			ch_w_sys_minimum_dec[i + 1] = monthly_minimum_charges[11];
			ch_w_sys_minimum[i + 1] = ch_w_sys_minimum_jan[i + 1]
				+ ch_w_sys_minimum_feb[i + 1]
				+ ch_w_sys_minimum_mar[i + 1]
				+ ch_w_sys_minimum_apr[i + 1]
				+ ch_w_sys_minimum_may[i + 1]
				+ ch_w_sys_minimum_jun[i + 1]
				+ ch_w_sys_minimum_jul[i + 1]
				+ ch_w_sys_minimum_aug[i + 1]
				+ ch_w_sys_minimum_sep[i + 1]
				+ ch_w_sys_minimum_oct[i + 1]
				+ ch_w_sys_minimum_nov[i + 1]
				+ ch_w_sys_minimum_dec[i + 1];



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


	void ur_calc( ssc_number_t e_in[8760], ssc_number_t p_in[8760],
		ssc_number_t revenue[8760], ssc_number_t payment[8760], ssc_number_t income[8760], 
		ssc_number_t price[8760], ssc_number_t demand_charge[8760], 
		ssc_number_t energy_charge[8760],
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12], ssc_number_t monthly_ec_flat_charges[12],
		ssc_number_t monthly_ec_rates[12],
		ssc_number_t ec_tou_sched[8760], ssc_number_t dc_tou_sched[8760], 
		ssc_number_t dc_hourly_peak[8760], ssc_number_t monthly_cumulative_excess_energy[12], 
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12], 
		ssc_number_t monthly_e_use_period_tier[12][12][6], 
		ssc_number_t monthly_charge_period_tier[12][12][6],
		ssc_number_t rate_esc, bool include_fixed=true, bool include_min=true) 

	{
		int i;

		for (i=0;i<8760;i++)
			revenue[i] = payment[i] = income[i] = price[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i=0;i<12;i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				= monthly_ec_flat_charges[i]
				= monthly_dc_fixed[i] = monthly_dc_tou[i] 
				= monthly_ec_charges[i] = monthly_ec_rates[i] 
				= monthly_cumulative_excess_energy[i] 
				= monthly_cumulative_excess_dollars[i] 
				= monthly_bill[i] = 0.0;
		}
		// initialize all montly values
		ssc_number_t buy = as_number("ur_flat_buy_rate")*rate_esc;
		ssc_number_t sell = as_number("ur_flat_sell_rate")*rate_esc;

		//bool sell_eq_buy = as_boolean("ur_sell_eq_buy");

		
		// false = 2 meters, load and system treated separately
		// true = 1 meter, net grid energy used for bill calculation with either energy or dollar rollover.
		bool enable_nm = as_boolean("ur_enable_net_metering");
		bool sell_eq_buy = enable_nm; // update from 6/25/15 meeting

		bool ec_enabled = as_boolean("ur_ec_enable");
		bool dc_enabled = as_boolean("ur_dc_enable");

		bool excess_monthly_dollars = (as_integer("ur_excess_monthly_energy_or_dollars") == 1);
//		bool apply_excess_to_flat_rate = !ec_enabled;

		if (sell_eq_buy)
			sell = buy;

		// calculate the monthly net energy and monthly hours
		int m,h;
		size_t d;
		ssc_number_t monthly_energy_net[12]; // 12 months
		// calculate the monthly net energy per month
		int hours_per_month[12];
		int c = 0;
		for (m = 0; m < 12; m++)
		{
			monthly_energy_net[m] = 0;
			hours_per_month[m] = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					// net energy use per month
					monthly_energy_net[m] += e_in[c];
					// hours per period per month
					hours_per_month[m]++;
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
				monthly_cumulative_excess_energy[m] = ((prev_value + monthly_energy_net[m]) > 0) ? (prev_value + monthly_energy_net[m]) : 0;
			}
		}



// TODO schedules can be initialized outside of ur_calc once!
		// 12 periods with 6 tiers each rates 3rd index = 0 = buy and 1=sell
		ssc_number_t ec_rates[12][6][2];
		ssc_number_t ec_energy_ub[12][6];
		size_t nrows, ncols;
		int period, tier;
		ssc_number_t ec_monthly_energy_net[12][12]; // 12 months, 12 periods
		int ec_hours_per_month_per_period[12][12];

		if (ec_enabled)
		{

			ssc_number_t *ec_weekday = as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "energy charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			ssc_number_t *ec_weekend = as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "energy charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			util::matrix_t<double> ec_schedwkday(12, 24);
			ec_schedwkday.assign(ec_weekday, nrows, ncols);
			util::matrix_t<double> ec_schedwkend(12, 24);
			ec_schedwkend.assign(ec_weekend, nrows, ncols);

			int ec_tod[8760];

			if (!util::translate_schedule(ec_tod, ec_schedwkday, ec_schedwkend, 1, 12))
				throw general_error("could not translate weekday and weekend schedules for energy charges");

			for (int i = 0; i < 8760; i++) ec_tou_sched[i] = (ssc_number_t)(ec_tod[i]);

			// tiered rates for all 6 tiers in each of the 12 periods

			for (period = 0; period < 12; period++)
			{
				std::string str_period = util::to_string(period + 1);

				for (tier = 0; tier < 6; tier++)
				{
					std::string str_tier = util::to_string(tier + 1);

					ec_rates[period][tier][0] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_br")*rate_esc;
					ec_rates[period][tier][1] = sell_eq_buy ? ec_rates[period][tier][0] : as_number("ur_ec_p" + str_period + "_t" + str_tier + "_sr")*rate_esc;
					ec_energy_ub[period][tier] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_ub");
				}
			}



			// calculate the monthly net energy per period
			c = 0;
			for (m = 0; m < 12; m++)
			{
				for (period = 0; period < 12; period++)
				{
					ec_monthly_energy_net[m][period] = 0;
					ec_hours_per_month_per_period[m][period] = 0;
					for (tier = 0; tier < 6; tier++)
					{
						monthly_e_use_period_tier[m][period][tier] = 0;
						monthly_charge_period_tier[m][period][tier] = 0;
					}
				}

				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						int todp = ec_tod[c] - 1;
						// net energy use per period per month
						ec_monthly_energy_net[m][todp] += e_in[c];
						// hours per period per month
						ec_hours_per_month_per_period[m][todp]++;
						c++;
					}
				}
			}
		}
// adjust net energy if net metering with monthly rollover
		if (enable_nm && !excess_monthly_dollars)
		{
			for (m = 1; m < 12; m++)
			{
				for (period = 0; period < 12; period++)
				{
					if (monthly_energy_net[m] != 0 && (ec_monthly_energy_net[m][period]<0))
					{
						ssc_number_t reduction = fabs(monthly_cumulative_excess_energy[m - 1] * ec_monthly_energy_net[m][period] / monthly_energy_net[m]);
						ec_monthly_energy_net[m][period] += reduction;
					}
				}
				if (monthly_energy_net[m] < 0)
					monthly_energy_net[m] += monthly_cumulative_excess_energy[m - 1];
			}
		}


// fixed demand charge initialization
		ssc_number_t dc_fixed_charges[12][6];
		ssc_number_t dc_fixed_energy_ub[12][6];

		for (m = 0; m<12; m++)
		{
			for (tier = 0; tier<6; tier++)
			{
				std::string str_tier = util::to_string(tier + 1);
				dc_fixed_charges[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_dc")*rate_esc;
				dc_fixed_energy_ub[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_ub");
			}
		}


// monthly fixed demand charge peaks
		ssc_number_t monthly_peak[12];  // peak usage for the month (negative value)
		ssc_number_t peak_demand = 0;
		ssc_number_t charge = 0;
		int peak_hour[12];
		c = 0;
		for (m = 0; m < 12; m++)
		{
			monthly_peak[m] = 0;
			peak_hour[m] = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					if (p_in[c] < 0 && p_in[c] < monthly_peak[m])
					{
						monthly_peak[m] = p_in[c];
						peak_hour[m] = c;
					}
					c++;
				}
			}
		}

// demand charge schedules
		ssc_number_t *dc_weekday;
		ssc_number_t *dc_weekend;
		// initialize to diurnal all 1 if only flat monthly demand charge specified per Mike Gleason 1/16/15
		util::matrix_t<double> dc_schedwkday(12, 24, 1);
		util::matrix_t<double> dc_schedwkend(12, 24, 1);
		ssc_number_t monthly_period_peak[12][12];  // peak usage for period for the month (negative value)
		int peak_period_hour[12][12];
		ssc_number_t dc_charges[12][6];
		ssc_number_t dc_energy_ub[12][6];
		int dc_tod[8760];


		if (dc_enabled)
		{
			if (is_assigned("ur_dc_sched_weekday"))
			{
				dc_weekday = as_matrix("ur_dc_sched_weekday", &nrows, &ncols);
				if (nrows != 12 || ncols != 24)
				{
					std::ostringstream ss;
					ss << "demand charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
					throw exec_error("utilityrate3", ss.str());
				}
				dc_schedwkday.assign(dc_weekday, nrows, ncols);
			}
			if (is_assigned("ur_dc_sched_weekend"))
			{
				dc_weekend = as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
				if (nrows != 12 || ncols != 24)
				{
					std::ostringstream ss;
					ss << "demand charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
					throw exec_error("utilityrate3", ss.str());
				}
				dc_schedwkend.assign(dc_weekend, nrows, ncols);
			}



			if (!util::translate_schedule(dc_tod, dc_schedwkday, dc_schedwkend, 1, 12))
				throw general_error("could not translate weekday and weekend schedules for demand charge time-of-use rate");

			for (i = 0; i < 8760; i++) dc_tou_sched[i] = (ssc_number_t)(dc_tod[i]);


			// extract rate info
			for (period = 0; period < 12; period++)
			{
				std::string str_period = util::to_string(period + 1);
				for (tier = 0; tier < 6; tier++)
				{
					std::string str_tier = util::to_string(tier + 1);
					dc_charges[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_dc")*rate_esc;
					dc_energy_ub[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_ub");
				}
			}

			c = 0;
			for (m = 0; m < 12; m++)
			{
				for (i = 0; i < 12; i++) // TOU periods
				{
					monthly_period_peak[m][i] = 0;
					peak_period_hour[m][i] = 0;
				}

				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						int todp = dc_tod[c] - 1;
						if (p_in[c] < 0 && p_in[c] < monthly_period_peak[m][todp])
						{
							monthly_period_peak[m][todp] = p_in[c];
							peak_period_hour[m][todp] = c;
						}
						c++;
					}
				}
			}
		}


		c = 0;
		// process one month at a time
		for (m = 0; m < 12; m++)
		{
// flat rate
			if (hours_per_month[m] <= 0) break;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					if (d == util::nday[m] - 1 && h == 23)
					{
						if (enable_nm)
						{
							if (monthly_energy_net[m] < 0)
							{
								payment[c] += -monthly_energy_net[m] * buy;
								monthly_ec_flat_charges[m] += payment[c];
							}
						}
						else // no net metering - so no rollover.
						{
							if (monthly_energy_net[m] < 0) // must buy from grid
							{
								payment[c] += -monthly_energy_net[m] * buy;
								monthly_ec_flat_charges[m] += payment[c];
							}
							else
							{
								income[c] += monthly_energy_net[m] * sell;
								monthly_ec_flat_charges[m] -= income[c];
							}
						}
						// added for Mike Gleason 
						energy_charge[c] += monthly_ec_flat_charges[m];
// Price ?

// end of flat rate

// energy charge
						if (ec_enabled)
						{
							ssc_number_t monthly_energy = 0;
							for (period = 0; period<12; period++)
							{
								//				charge[m][period]=0;
								//				credit[m][period]=0;

								if (ec_monthly_energy_net[m][period] >= 0.0)
								{ // calculate income or credit
									ssc_number_t credit_amt = 0;
									ssc_number_t energy_surplus = ec_monthly_energy_net[m][period];
									tier = 0;
									while (tier < 6)
									{
										ssc_number_t tier_energy = 0;
										ssc_number_t tier_credit = 0;
										// add up the charge amount for this block
										ssc_number_t e_upper = ec_energy_ub[period][tier];
										ssc_number_t e_lower = tier > 0 ? ec_energy_ub[period][tier - 1] : (ssc_number_t)0.0;

										if (energy_surplus > e_upper)
										{
											tier_energy = e_upper - e_lower;
											tier_credit = tier_energy*ec_rates[period][tier][1];
										}
										else
										{
											tier_energy = energy_surplus - e_lower;
											tier_credit = tier_energy*ec_rates[period][tier][1];
										}
										credit_amt += tier_credit;
										monthly_e_use_period_tier[m][period][tier] -= tier_energy;
										monthly_charge_period_tier[m][period][tier] -= tier_credit;
										if (energy_surplus < e_upper)
											break;
										tier++;
									}
									//					credit[m][period] = credit_amt;
									monthly_ec_charges[m] -= credit_amt;
									monthly_energy += energy_surplus;
								}
								else
								{ // calculate payment or charge
									ssc_number_t charge_amt = 0;
									ssc_number_t energy_deficit = -ec_monthly_energy_net[m][period];

									tier = 0;
									while (tier < 6)
									{
										ssc_number_t tier_energy = 0;
										ssc_number_t tier_charge = 0;
										// add up the charge amount for this block
										ssc_number_t e_upper = ec_energy_ub[period][tier];
										ssc_number_t e_lower = tier > 0 ? ec_energy_ub[period][tier - 1] : (ssc_number_t)0.0;

										if (energy_deficit > e_upper)
										{
											tier_energy = e_upper - e_lower;
											tier_charge = tier_energy*ec_rates[period][tier][0];
										}
										else
										{
											tier_energy = energy_deficit - e_lower;
											tier_charge = tier_energy*ec_rates[period][tier][0];
										}
										charge_amt += tier_charge;
										monthly_e_use_period_tier[m][period][tier] += tier_energy;
										monthly_charge_period_tier[m][period][tier] += tier_charge;
										if (energy_deficit < e_upper)
											break;
										tier++;
									}
									//					charge[m][period] = charge_amt;
									monthly_ec_charges[m] += charge_amt;
									monthly_energy -= energy_deficit;
								}
								//monthly_energy += energy_net[m][period];
							}
							monthly_ec_rates[m] = monthly_energy != 0 ? monthly_ec_charges[m] / monthly_energy : (ssc_number_t)0.0;


							// monthly rollover with year end sell at reduced rate
							if (enable_nm)
							{
								payment[c] += monthly_ec_charges[m];
							}
							else // non-net metering - no rollover 
							{
								if (monthly_energy_net[m] < 0) // must buy from grid
									payment[c] += monthly_ec_charges[m];
								else // surplus - sell to grid
									income[c] -= monthly_ec_charges[m]; // charge is negative for income!
							}

							// Price ?
							// added for Mike Gleason 
							energy_charge[c] += monthly_ec_charges[m];

							// end of energy charge

						}


						if (dc_enabled)
						{
							// fixed demand charge

							// compute charge based on tier structure for the month
							tier = 0;
							charge = 0;
							peak_demand = -monthly_peak[m]; // energy demands are negative.
							while (tier < 6)
							{
								// add up the charge amount for this block
								ssc_number_t e_upper = dc_fixed_energy_ub[m][tier];
								ssc_number_t e_lower = tier > 0 ? dc_fixed_energy_ub[m][tier - 1] : (ssc_number_t)0.0;

								if (peak_demand > e_upper)
									charge += (e_upper - e_lower)*dc_fixed_charges[m][tier];
								else
									charge += (peak_demand - e_lower)*dc_fixed_charges[m][tier];

//								log(util::format("Demand fixed, month %d, tier %d, lower %lg, upper %lg, charge %lg, peak %lg", m, tier, e_lower, e_upper, charge, peak_demand),2);
								if (peak_demand < e_upper)
									break;
								tier++;
							}

							monthly_dc_fixed[m] = charge;
							payment[c] += monthly_dc_fixed[m];
							demand_charge[c] = charge;
							dc_hourly_peak[peak_hour[m]] = peak_demand;


							// end of fixed demand charge


							// TOU demand charge
							charge = 0;
							peak_demand = 0;
							int peak_hour = 0;
							for (period = 0; period<12; period++)
							{
								tier = 0;
								peak_demand = -monthly_period_peak[m][period];
								peak_hour = peak_period_hour[m][period];
								while (tier < 6)
								{
									// add up the charge amount for this block
									ssc_number_t e_upper = dc_energy_ub[period][tier];
									ssc_number_t e_lower = tier > 0 ? dc_energy_ub[period][tier - 1] : (ssc_number_t)0.0;
									if (peak_demand > e_upper)
										charge += (e_upper - e_lower)*dc_charges[period][tier];
									else
										charge += (peak_demand - e_lower)*dc_charges[period][tier];

//									log(util::format("TOU demand, month %d, hour %d, peak hour %d, period %d, tier %d, lower %lg, upper %lg, charge %lg, rate %lg, peak %lg", m, c, peak_hour, period, tier, e_lower, e_upper, charge, dc_charges[period][tier], peak_demand), 2);
									if (peak_demand < e_upper)
										break;

									tier++;
								}
								dc_hourly_peak[peak_hour] = peak_demand;
							}
							// add to payments
							monthly_dc_tou[m] = charge;
							payment[c] += monthly_dc_tou[m]; // apply to last hour of the month
							demand_charge[c] += charge; // add TOU charge to hourly demand charge

							// end of TOU demand charge
						}


					} // end of if end of month
					c++;
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess dollars and rollover
			monthly_bill[m] = payment[c - 1] - income[c - 1];
			if (enable_nm)
			{
				if (m > 0) monthly_bill[m] -= monthly_cumulative_excess_dollars[m - 1];
				if (monthly_bill[m] < 0)
				{
					if (excess_monthly_dollars)
						monthly_cumulative_excess_dollars[m] = -monthly_bill[m];
					monthly_bill[m] = 0;
					payment[c - 1] = 0; // fixed charges applied below
				}
			}
		} // end of month m (m loop)


		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		/*
		if (include_fixed)
			process_monthly_charge(payment, monthly_fixed_charges, rate_esc);
		// process min charges
		if (include_min)
		{
			process_monthly_min(payment, monthly_minimum_charges, rate_esc);
			process_annual_min(payment, monthly_minimum_charges, rate_esc);
		}
		*/
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


};

DEFINE_MODULE_ENTRY( utilityrate3, "Complex utility rate structure net revenue calculator OpenEI Version 3", 1 );


