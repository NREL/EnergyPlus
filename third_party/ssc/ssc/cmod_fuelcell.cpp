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

#include <math.h>
#include <memory>

#include "common.h"
#include "core.h"

#include "cmod_fuelcell.h"

var_info vtab_fuelcell_input[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	// simulation inputs
	// { SSC_INOUT,        SSC_NUMBER,      "percent_complete",                  "Estimated simulation status",           "%",       "",                                      "",        "",                      "",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",        "Lifetime simulation",                   "0/1",     "0=SingleYearRepeated,1=RunEveryYear",   "Lifetime",        "?=0",                   "BOOLEAN",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                   "Lifetime analysis period",              "years",   "The number of years in the simulation", "Lifetime",        "system_use_lifetime_output=1","",                           "" },

	// external compute module inputs
	{ SSC_INOUT,        SSC_ARRAY,       "gen",								  "System power generated",                "kW",        "Lifetime system generation", "",                  "",                        "",                              "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                  "Electricity load (year 1)",             "kW",	    "",                  "Load",	                       "",	                      "",	                           "" },

	// fuel cell
	{ SSC_INPUT,        SSC_MATRIX,      "fuelcell_availability_schedule",    "Fuel cell availability schedule ",      "Column 1: Hour of year start shutdown/Column 2: Hours duration of shutdown ", "",   "Fuel Cell","", "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation",              "Fuel cell degradation per hour",        "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation_restart",      "Fuel cell degradation at restart",      "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation_restart_schedule","Fuel cell enable scheduled restarts",  "0/1",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation_restarts_per_year","Fuel cell scheduled restarts per year","",         "",                "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fixed_pct",				  "Fuel cell fixed operation percent",     "%",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dynamic_response_up",      "Fuel cell ramp rate limit up",          "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dynamic_response_down",    "Fuel cell ramp rate limit down",        "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "fuelcell_efficiency",               "Fuel cell efficiency table ",           "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_efficiency_choice",        "Fuel cell efficiency definition choice ","0/1",       "0=OriginalNameplate,1=DegradedNameplate",  "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_available",           "Fuel cell available fuel quantity",     "MCf",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_price",				  "Fuel cell price",                       "$/MCf",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_type",				  "Fuel cell type",                        "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_lhv",                      "Fuel cell lower heating value",         "Btu/ft3",    "",                 "Fuel Cell",				   "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_number_of_units",          "Fuel cell number of units",             "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_operation_options",         "Fuel cell turn off options",            "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_replacement_option",       "Fuel cell replacement option",          "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_replacement_percent",      "Fuel cell replace at percentage",       "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "fuelcell_replacement_schedule",     "Fuel cell replace on schedule",         "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_shutdown_time",            "Fuel cell shutdown hours",              "hours",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_startup_time",             "Fuel cell startup hours",               "hours",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_is_started",             "Fuel cell is started",               "0/1",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_type",                     "Fuel cell type",						   "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_max_power",           "Fuel cell max power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_min_power",           "Fuel cell min power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },

	//  Dispatch
	{ SSC_INPUT,        SSC_ARRAY,       "fuelcell_dispatch",                 "Fuel cell dispatch input per unit",     "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dispatch_choice",          "Fuel cell dispatch choice",             "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_fuelcellcharge",    "Periods 1-6 charging allowed?",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_fuelcelldischarge", "Periods 1-6 discharging allowed?",       "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_percent_fc_discharge","Periods 1-6 percent of max fuelcell output", "",    "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_units_fc_discharge","Periods 1-6 number of fuel cell units?", "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_manual_sched",             "Dispatch schedule for weekday",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_manual_sched_weekend",     "Dispatch schedule for weekend",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },

	{ SSC_INOUT,        SSC_NUMBER,      "capacity_factor",                   "Capacity factor",                        "%",          "",                "",                           "?=0",                     "",                              "" },
	{ SSC_INOUT,        SSC_NUMBER,      "annual_energy",                     "Annual Energy",                          "kWh",        "",                "",                           "?=0",                     "",                              "" },

var_info_invalid };

var_info vtab_fuelcell_output[] = {

	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_power",                     "Electricity from fuel cell",            "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_power_max_percent",         "Fuel cell max power percent available",  "%",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_percent_load",              "Fuel cell percent load",                 "%",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_electrical_efficiency",     "Fuel cell electrical efficiency",       "%",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_power_thermal",             "Heat from fuel cell",                   "kWt",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_fuel_consumption_mcf",      "Fuel consumption of fuel cell",         "MCf",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_to_load",                   "Electricity to load from fuel cell",    "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_to_grid",                   "Electricity to grid from fuel cell",    "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_replacement",                "Fuel cell replacements per year",      "number/year", "",              "Fuel Cell",           "",                           "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",                    "Heat rate conversion factor (MMBTUs/MWhe)",  "MMBTUs/MWhe",   "",      "Fuel Cell",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",                   "Annual Fuel Usage",                          "kWht",          "",      "Fuel Cell",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_fuel_usage_lifetime",            "Annual Fuel Usage (lifetime)",               "kWht",          "",      "Fuel Cell",           "",               "",                    "" },


var_info_invalid };

cm_fuelcell::cm_fuelcell()
{
	add_var_info(vtab_fuelcell_input);
	add_var_info(vtab_fuelcell_output);
	add_var_info(vtab_technology_outputs);
}

// Have to add this since compute module isn't actually fully constructed until compute is called with
// a vartable.
void cm_fuelcell::construct()
{
	std::unique_ptr<fuelCellVariables> tmp(new fuelCellVariables(*this));
	fcVars = std::move(tmp);

	std::unique_ptr<FuelCell> tmp2(new FuelCell(fcVars->unitPowerMax_kW, fcVars->unitPowerMin_kW,
		fcVars->startup_hours, fcVars->is_started, fcVars->shutdown_hours,
		fcVars->dynamicResponseUp_kWperHour, fcVars->dynamicResponseDown_kWperHour,
		fcVars->degradation_kWperHour, fcVars->degradationRestart_kW,
		fcVars->replacementOption, fcVars->replacement_percent, fcVars->replacementSchedule,
		fcVars->shutdownTable, fcVars->efficiencyChoice, fcVars->efficiencyTable,
		fcVars->lowerHeatingValue_BtuPerFt3, fcVars->higherHeatingValue_BtuPerFt3, fcVars->availableFuel_MCf,
		fcVars->shutdownOption, fcVars->dt_hour));
	fuelCell = std::move(tmp2);

	std::unique_ptr<FuelCellDispatch> tmp3(new FuelCellDispatch(fuelCell.get(), fcVars->numberOfUnits,
		fcVars->dispatchOption, fcVars->shutdownOption, fcVars->dt_hour, fcVars->fixed_percent, fcVars->dispatch_kW,
		fcVars->canCharge, fcVars->canDischarge, fcVars->discharge_percentByPeriod, fcVars->discharge_unitsByPeriod,
		fcVars->scheduleWeekday,fcVars->scheduleWeekend));
	fuelCellDispatch = std::move(tmp3);

	allocateOutputs();
}

void cm_fuelcell::exec()
{
	double annual_energy = 0.0;
	double annual_fuel = 0.0;
	// float percent_complete = 0.0;
//	float percent = 0.0;
	// size_t nStatusUpdates = 50;

/*
	if (is_assigned("percent_complete")) {
		percent_complete = as_float("percent_complete");
	}
*/

	construct();
	size_t idx = 0;
 	for (size_t y = 0; y < fcVars->numberOfYears; y++) {

		size_t idx_year = 0;
		size_t annual_index;
		fcVars->numberOfYears > 1 ? annual_index = y + 1 : annual_index = 0;

		for (size_t h = 0; h < 8760; h++){
/*
			// status bar
			if (h % (8760 / nStatusUpdates) == 0)
			{
				// assume that anyone using this module is chaining with two techs
				float techs = 3;
				percent = percent_complete + 100.0f * ((float)idx + 1) / ((float)fcVars->numberOfLifetimeRecords) / techs;
				if (!update("", percent, (float)h)) {
					throw exec_error("fuelcell", "simulation canceled at hour " + util::to_string(h + 1.0));
				}
			}
*/
			for (size_t s = 0; s < fcVars->stepsPerHour; s++) {
				fuelCellDispatch->runSingleTimeStep(h, idx_year, fcVars->systemGeneration_kW[idx], fcVars->electricLoad_kW[idx]);
				p_fuelCellPower_kW[idx] = (ssc_number_t)fuelCellDispatch->getPower();
				p_fuelCellPowerMaxAvailable_percent[idx] = (ssc_number_t)fuelCellDispatch->getPowerMaxPercent();
				p_fuelCellLoad_percent[idx] = (ssc_number_t)fuelCellDispatch->getPercentLoad();
				p_fuelCellElectricalEfficiency_percent[idx] = (ssc_number_t)fuelCellDispatch->getElectricalEfficiencyPercent();
				p_fuelCellPowerThermal_kW[idx] = (ssc_number_t)fuelCellDispatch->getPowerThermal();
				p_fuelCellConsumption_MCf[idx] = (ssc_number_t)fuelCellDispatch->getFuelConsumption();
				p_fuelCellConsumption_MCf_annual[annual_index] += (ssc_number_t)MCF_TO_KWH(p_fuelCellConsumption_MCf[idx], fcVars->lowerHeatingValue_BtuPerFt3);
				p_fuelCellToGrid_kW[idx] = (ssc_number_t)(fuelCellDispatch->getBatteryPower()->powerFuelCellToGrid);
				p_fuelCellToLoad_kW[idx] = (ssc_number_t)(fuelCellDispatch->getBatteryPower()->powerFuelCellToLoad);
				p_gen_kW[idx] = (ssc_number_t)(fcVars->systemGeneration_kW[idx]) + p_fuelCellPower_kW[idx];

				if (y == 0) {
					annual_energy += p_gen_kW[idx] * fcVars->dt_hour;
				}

				idx++;
				idx_year++;
			}
		}
		if (y == 0) {
			annual_fuel = p_fuelCellConsumption_MCf_annual[annual_index];
		}

		// tabulate replacements
		p_fuelCellReplacements[annual_index] = (ssc_number_t)(fuelCell->getTotalReplacements());
		fuelCell->resetReplacements();
	}

	// capacity factor update
	double capacity_factor_in, annual_energy_in, nameplate_in;
	capacity_factor_in = annual_energy_in = nameplate_in = 0;

	if (is_assigned("capacity_factor") && is_assigned("annual_energy")) {
		capacity_factor_in = as_double("capacity_factor");
		annual_energy_in = as_double("annual_energy");
		nameplate_in = (annual_energy_in / (capacity_factor_in * util::percent_to_fraction)) / 8760.;
	}
	double nameplate = nameplate_in + (fcVars->unitPowerMax_kW * fcVars->numberOfUnits);
	assign("capacity_factor", var_data(static_cast<ssc_number_t>(annual_energy * util::fraction_to_percent / (nameplate * 8760.))));
	assign("annual_energy", var_data(static_cast<ssc_number_t>(annual_energy)));
	// assign("percent_complete", var_data((ssc_number_t)percent));

	// Ratio of MMBtu to MWh to get cash flow calculation correct (fuel costs in $/MMBtu)
	assign("system_heat_rate", var_data((ssc_number_t)BTU_PER_KWH / 1000));
	assign("annual_fuel_usage", var_data((ssc_number_t)annual_fuel));
}

void cm_fuelcell::allocateOutputs()
{
	p_fuelCellPower_kW = allocate("fuelcell_power", fcVars->numberOfLifetimeRecords);
	p_fuelCellPowerMaxAvailable_percent = allocate("fuelcell_power_max_percent", fcVars->numberOfLifetimeRecords);
	p_fuelCellLoad_percent = allocate("fuelcell_percent_load", fcVars->numberOfLifetimeRecords);
	p_fuelCellElectricalEfficiency_percent = allocate("fuelcell_electrical_efficiency", fcVars->numberOfLifetimeRecords);
	p_fuelCellPowerThermal_kW = allocate("fuelcell_power_thermal", fcVars->numberOfLifetimeRecords);
	p_fuelCellConsumption_MCf = allocate("fuelcell_fuel_consumption_mcf", fcVars->numberOfLifetimeRecords);
	p_fuelCellToGrid_kW = allocate("fuelcell_to_grid", fcVars->numberOfLifetimeRecords);
	p_fuelCellToLoad_kW = allocate("fuelcell_to_load", fcVars->numberOfLifetimeRecords);

	// annual outputs
	size_t annual_size = fcVars->numberOfYears + 1;
	if (fcVars->numberOfYears == 1) { annual_size = 1; };

	p_fuelCellReplacements = allocate("fuelcell_replacement", annual_size);
	p_fuelCellConsumption_MCf_annual = allocate("annual_fuel_usage_lifetime", annual_size);
	p_fuelCellReplacements[0] = 0;
	p_fuelCellConsumption_MCf_annual[0] = 0;

	p_gen_kW = allocate("gen", fcVars->numberOfLifetimeRecords);

}

cm_fuelcell::~cm_fuelcell(){/* nothing to do */ }
DEFINE_MODULE_ENTRY(fuelcell, "Fuel cell model", 1)
