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
#include "lib_windfile.h"
#include "lib_windwatts.h"

// for adjustment factors
#include "common.h"

static var_info _cm_vtab_generic_system[] = {
//	  VARTYPE           DATATYPE         NAME                           LABEL                                 UNITS           META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
	{ SSC_INPUT,        SSC_NUMBER,      "spec_mode",                  "Spec mode: 0=constant CF,1=profile",  "",             "",      "Plant",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "Derate",                              "%",            "",      "Plant",      "*",               "",                    "" },
	{ SSC_INOUT,        SSC_NUMBER,      "system_capacity",            "Nameplace Capcity",                   "kW",           "",      "Plant",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "user_capacity_factor",       "Capacity Factor",                     "%",            "",      "Plant",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heat_rate",                  "Heat Rate",                           "MMBTUs/MWhe",  "",      "Plant",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion Efficiency",               "%",            "",      "Plant",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_output_array",        "Array of Energy Output Profile",      "kW",           "",      "Plant",      "spec_mode=1",     "",                    "" },

	// optional for lifetime analysis
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",                  "Generic lifetime simulation",                               "0/1",      "",                              "Lifetime",             "?=0",                        "INTEGER,MIN=0,MAX=1",          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                             "Lifetime analysis period",                             "years",    "",                              "Lifetime",             "system_use_lifetime_output=1",   "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,       "generic_degradation",                              "Annual AC degradation",                            "%/year",   "",                              "Lifetime",             "system_use_lifetime_output=1",   "",                             "" },


//    OUTPUTS ----------------------------------------------------------------------------								      														   
//	  VARTYPE           DATATYPE         NAME                          LABEL                                   UNITS           META     GROUP                 REQUIRED_IF        CONSTRAINTS           UI_HINTS
//	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",              "Hourly Energy",                        "kWh",           "",      "Time Series",      "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",             "Monthly Energy",                       "kWh",          "",      "Monthly",      "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",              "Annual Energy",                        "kWh",          "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",           "Annual Fuel Usage",                    "kWht",         "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "water_usage",                "Annual Water Usage",                   "",             "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",           "Heat Rate Conversion Factor",          "MMBTUs/MWhe",  "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "First year kWh/kW", "kWh/kW", "", "Annual", "*", "", "" },


var_info_invalid };

class cm_generic_system : public compute_module
{
private:
public:
	
	cm_generic_system()
	{
		add_var_info( _cm_vtab_generic_system );

		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{
		int spec_mode = as_integer("spec_mode");
		bool system_use_lifetime_output = (as_integer("system_use_lifetime_output") == 1);

		// Warning workaround
		static bool is32BitLifetime = (__ARCHBITS__ == 32 &&	system_use_lifetime_output);
		if (is32BitLifetime)
		throw exec_error( "generic", "Lifetime simulation of generic systems is only available in the 64 bit version of SAM.");

		// Lifetime setup
		ssc_number_t *enet = nullptr;
		size_t nyears = 1;
		if (system_use_lifetime_output) {
			nyears = as_integer("analysis_period");
		}

		// Load parsing
		std::vector<double> load;
		size_t nrec_load = 8760;

		if (is_assigned("load")) {
			load = as_vector_double("load");
			nrec_load = load.size();
		}
		size_t nlifetime = nrec_load * nyears;
		size_t steps_per_hour = nrec_load / 8760;
		double ts_hour = 1 / (double)(steps_per_hour);

		// Degradation and adjustments
		std::vector<ssc_number_t> sys_degradation;
	    sys_degradation.reserve(nyears);
		double derate = (1 - (double)as_number("derate") / 100);

		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("generic system", "failed to setup adjustment factors: " + haf.error());

		if (system_use_lifetime_output)
		{
			// setup system degradation
			size_t i, count_degrad = 0;
			ssc_number_t *degrad = 0;
			degrad = as_array("generic_degradation", &count_degrad);

			if (count_degrad == 1)
			{
				for (i = 0; i < nyears; i++)
					sys_degradation.push_back((ssc_number_t)pow((1.0 - (double)degrad[0] / 100.0), i));
			}
			else if (count_degrad > 0)
			{
				for (i = 0; i < nyears && i < (int)count_degrad; i++) sys_degradation.push_back((ssc_number_t)(1.0 - (double)degrad[i] / 100.0));
			}
		}
		else {
			sys_degradation.push_back(1); // single year mode - degradation handled in financial models.
		}

		size_t idx = 0;
		double annual_output = 0;

		// Constant generation profile
		if (spec_mode == 0)
		{
			double output = (double)as_number("system_capacity")
				* (double)as_number("user_capacity_factor") / 100
				* derate; // kW

			annual_output = 8760 * output; // kWh
			enet = allocate("gen", nlifetime);
			for (size_t iyear = 0; iyear < nyears; iyear++)
			{
				for (size_t ihour = 0; ihour < 8760; ihour++)
				{
					for (size_t ihourstep = 0; ihourstep < steps_per_hour; ihourstep++)
					{
						enet[idx] = (ssc_number_t)(output*haf(ihour)) * sys_degradation[iyear]; // kW
						idx++;
					}
				}
			}
		}
		// Input generation profile
		else
		{
			size_t nrec_gen = 0;
			ssc_number_t *enet_in = as_array("energy_output_array", &nrec_gen); // kW
			size_t steps_per_hour_gen = nrec_gen / 8760;

			if (!enet_in) {
				throw exec_error("generic", util::format("energy_output_array variable had no values."));
			}

			if (nrec_gen < nrec_load) {
				throw exec_error("generic", util::format("energy_output_array %d must be greater than or equal to load array %d", nrec_gen, nrec_load));
			}
			else {
				nlifetime = nrec_gen * nyears;
				steps_per_hour = steps_per_hour_gen;
				ts_hour = 1 / (double)(steps_per_hour);
			}

			enet = allocate("gen", nlifetime);
			for (size_t iyear = 0; iyear < nyears; iyear++){
				for (size_t ihour = 0; ihour < 8760; ihour++){
					for (size_t ihourstep = 0; ihourstep < steps_per_hour_gen; ihourstep++)
					{
						enet[idx] = enet_in[ihour* steps_per_hour_gen + ihourstep] * (ssc_number_t)(derate* haf(ihour))* sys_degradation[iyear];
						idx++;
					}
				}
			}
		}
		double annual_ac_pre_avail = 0, annual_energy = 0;
		idx = 0;

		// Run generic system
		for (size_t iyear = 0; iyear < nyears; iyear++){
			for (size_t hour = 0; hour < 8760; hour++){
				for (size_t jj = 0; jj < steps_per_hour; jj++)
				{
					
					// accumulate system generation before curtailment and availability
					if (iyear == 0) {
						annual_ac_pre_avail += enet[idx] * ts_hour;
					}

					//apply availability and curtailment
					enet[idx] *= haf(hour);

					if (iyear == 0) {
						annual_energy += (ssc_number_t)(enet[idx] * ts_hour);
					}

					idx++;
				}
			}
		}

		accumulate_monthly_for_year("gen", "monthly_energy", ts_hour, steps_per_hour);
		annual_output = accumulate_annual_for_year("gen", "annual_energy", ts_hour, steps_per_hour);

		// if conversion efficiency is zero then set fuel usage to zero per email from Paul 5/17/12
		double fuel_usage = 0.0;
		if (as_double("conv_eff") != 0.0)
			fuel_usage = annual_output * 100.0 / as_double("conv_eff");
		assign("annual_fuel_usage", (ssc_number_t)fuel_usage);

		assign("water_usage", 0.0);
		assign("system_heat_rate", (ssc_number_t)(as_number("heat_rate") *  as_number("conv_eff") / 100.0));

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");


		if (nameplate <= 0) {
			nameplate = annual_output / (8760 * (double)(as_number("user_capacity_factor") / 100) * derate);
		}
		assign("system_capacity", (var_data)((ssc_number_t)nameplate));

		if (nameplate > 0) {
			kWhperkW = annual_output / nameplate;
		}
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	} // exec
};

DEFINE_MODULE_ENTRY( generic_system, "Generic System", 1 );

