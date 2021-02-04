#ifndef _FUELCELL_COMMON_DATA_H_
#define _FUELCELL_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

namespace {
	char load_profile_path_grid[256];
	char gen_path_grid[256];
	char load_profile_path_grid_30[256];
	char gen_path_grid_30[256];
	char gen_path_grid_30_2yr[256];
	char grid_curtailment_default_MW[256];

	int nfc1 = sprintf(gen_path_grid, "%s/test/input_cases/generic_system_data/energy_output_array.csv", SSCDIR);
	int nfc2 = sprintf(load_profile_path_grid, "%s/test/input_cases/general_data/commercial_load.csv", SSCDIR);
	int nfc3 = sprintf(gen_path_grid_30, "%s/test/input_cases/generic_system_data/energy_output_array_30min.csv", SSCDIR);
	int nfc4 = sprintf(load_profile_path_grid_30, "%s/test/input_cases/generic_system_data/load_30min.csv", SSCDIR);
	int nfc5 = sprintf(gen_path_grid_30_2yr, "%s/test/input_cases/generic_system_data/energy_output_array_30min-2yr.csv", SSCDIR);
	int nfc6 = sprintf(grid_curtailment_default_MW, "%s/test/input_cases/general_data/grid_curtailment_default_MW.csv", SSCDIR);

}

void grid_default_60_min(ssc_data_t &data)
{
	set_array(data, "gen", gen_path_grid, 8760);
	ssc_data_set_number(data, "annual_energy", 0);
	ssc_data_set_number(data, "system_use_lifetime_output", 0);
	ssc_data_set_number(data, "analysis_period", 30);
	set_array(data, "load", load_profile_path_grid, 8760);
    ssc_number_t p_load_escalation[1] = { 0 };
    ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
    
	ssc_data_set_number(data, "grid_interconnection_limit_kwac", 180000);
	ssc_data_set_number(data, "enable_interconnection_limit", 1);
	set_array(data, "grid_curtailment", grid_curtailment_default_MW, 8760);
}

void grid_default_30_min(ssc_data_t &data)
{
	set_array(data, "gen", gen_path_grid_30, 8760*2);
	ssc_data_set_number(data, "system_use_lifetime_output", 0);
	ssc_data_set_number(data, "annual_energy", 0);
	ssc_data_set_number(data, "analysis_period", 30);
	set_array(data, "load", load_profile_path_grid_30, 8760 * 2);
    ssc_number_t p_load_escalation[1] = { 0 };
    ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
	ssc_data_set_number(data, "grid_interconnection_limit_kwac", 180000);
	ssc_data_set_number(data, "enable_interconnection_limit", 1);
	set_array(data, "grid_curtailment", grid_curtailment_default_MW, 8760);
}

void grid_default_30_min_lifetime(ssc_data_t &data)
{
	set_array(data, "gen", gen_path_grid_30_2yr, 8760 * 2 * 2);
	ssc_data_set_number(data, "system_use_lifetime_output", 1);
	ssc_data_set_number(data, "annual_energy", 0);
	ssc_data_set_number(data, "analysis_period", 2);
	set_array(data, "load", load_profile_path_grid_30, 8760 * 2);
    ssc_number_t p_load_escalation[1] = { 0 };
    ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
	ssc_data_set_number(data, "grid_interconnection_limit_kwac", 180000);
	ssc_data_set_number(data, "enable_interconnection_limit", 1);
	set_array(data, "grid_curtailment", grid_curtailment_default_MW, 8760);
}

/// Test to mimic a "no-financial" case, which specifies no analysis period 
void grid_default_60_min_no_financial(ssc_data_t &data)
{
	ssc_data_clear(data);
    ssc_data_set_number(data, "analysis_period", 1);
	set_array(data, "gen", gen_path_grid, 8760);
	set_array(data, "load", load_profile_path_grid, 8760);
    ssc_number_t p_load_escalation[1] = { 0 };
    ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
	ssc_data_set_number(data, "annual_energy", 0);
	ssc_data_set_number(data, "grid_interconnection_limit_kwac", 180000);
	ssc_data_set_number(data, "enable_interconnection_limit", 1);
	set_array(data, "grid_curtailment", grid_curtailment_default_MW, 8760);
}

#endif
