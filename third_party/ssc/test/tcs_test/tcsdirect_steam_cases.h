#ifndef _TCSDIRECT_STEAM_CASES_H_
#define _TCSDIRECT_STEAM_CASES_H_

#include <map>
#include "../input_cases/code_generator_utilities.h"
#include "tcsdirect_steam_common_data.h"

/**
*   Data for high-level integration tests that verifies whether results for a direct steam power tower
*   plant in Tucson, AZ matches expected results.
*   Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to tcsdirect_steam
*/

// Power Tower direct steam default congifuration
int tcsdirect_steam_daggett_default(ssc_data_t &data)
{
	tcsdirect_steam_default(data);

	int status = run_module(data, "tcsdirect_steam");

	//single_owner_default(data);
	//status += run_module(data, "singleowner");

	return status;
}

// Power Tower direct steam with alternative condenser type
// Condenser type: Evaporative
// Rest default configurations
int tcsdirect_steam_daggett_evap_condenser(ssc_data_t &data)
{
	tcsdirect_steam_default(data);

	ssc_data_set_number(data, "ct", 1);
	ssc_data_set_number(data, "eta_ref", 0.404);
	ssc_data_set_number(data, "startup_frac", 0.5);
	ssc_data_set_number(data, "P_cond_min", 2);

	int status = run_module(data, "tcsdirect_steam");

	return status;
}

// Power Tower direct steam with alternative condenser type
// Condenser type: Hybrid
// Rest default configurations
int tcsdirect_steam_daggett_hybrid_condenser(ssc_data_t &data)
{
	tcsdirect_steam_default(data);

	ssc_data_set_number(data, "ct", 3);
	ssc_data_set_number(data, "eta_ref", 0.404);
	ssc_data_set_number(data, "startup_frac", 0.5);
	ssc_data_set_number(data, "P_cond_min", 2);

	int status = run_module(data, "tcsdirect_steam");

	return status;
}

// Power Tower direct steam with alternative fossil dispatch mode
// Fossil dispatch mode: Supplemental operation
// Rest default configurations
//int tcsdirect_steam_daggett_fossil_dispatch_supplemental(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//
//	ssc_data_set_number(data, "fossil_mode", 2);
//	ssc_data_set_number(data, "eta_ref", 0.404);
//	ssc_data_set_number(data, "startup_frac", 0.5);
//	ssc_data_set_number(data, "P_cond_min", 2);
//
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}

// Power Tower direct steam with alternative Direct Steam Receiver material
// Direct Steam Receiver material: T91 Steel 
// Rest default configurations
//int tcsdirect_steam_daggett_direct_steam_receiver(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//
//	ssc_data_set_number(data, "mat_boiler", 28);
//	ssc_data_set_number(data, "mat_sh", 28);
//	ssc_data_set_number(data, "mat_rh", 28);
//	ssc_data_set_number(data, "eta_ref", 0.404);
//	ssc_data_set_number(data, "startup_frac", 0.5);
//	ssc_data_set_number(data, "P_cond_min", 2);
//
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}

// Power Tower direct steam with alternative flow pattern
// Flow pattern: 1 
// Rest default configurations
//int tcsdirect_steam_daggett_flow_pattern(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//
//	ssc_data_set_number(data, "flowtype", 1);
//	ssc_data_set_number(data, "eta_ref", 0.404);
//	ssc_data_set_number(data, "startup_frac", 0.5);
//	ssc_data_set_number(data, "P_cond_min", 2);
//
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}

// Power Tower direct steam with alternative Heliostat focusing method
// Heliostat focusing method: Flat
// Rest default configurations
//int tcsdirect_steam_daggett_focusing_method(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//
//	ssc_data_set_number(data, "focus_type", 0);
//	ssc_data_set_number(data, "eta_ref", 0.404);
//	ssc_data_set_number(data, "startup_frac", 0.5);
//	ssc_data_set_number(data, "P_cond_min", 2);
//
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}

// Power Tower direct steam with alternative Heliostat canting method
// Heliostat canting method: Equinox
// Rest default configurations
//int tcsdirect_steam_daggett_canting_method(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//
//	ssc_data_set_number(data, "cant_type", 2);
//	ssc_data_set_number(data, "eta_ref", 0.404);
//	ssc_data_set_number(data, "startup_frac", 0.5);
//	ssc_data_set_number(data, "P_cond_min", 2);
//
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}

// Power Tower direct steam with alternative location
// Location: Tucson, AZ
// Rest default configurations
//int tcsdirect_steam_daggett_tucson_AZ(ssc_data_t &data)
//{
//	tcsdirect_steam_default(data);
//  
//	char solar_resource_path_tucson[512];
//	int n2 = sprintf(solar_resource_path_tucson, "%s/test/input_cases/directsteam_data/tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
//	ssc_data_set_string(data, "solar_resource_file", solar_resource_path_tucson);
//	
//	int status = run_module(data, "tcsdirect_steam");
//
//	return status;
//}




#endif