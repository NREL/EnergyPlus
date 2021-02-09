#ifndef _PVWATTSV5_CASES_
#define _PVWATTSV5_CASES_

#include <stdio.h>
#include <string>
#include "code_generator_utilities.h"

/**
*   Data for high-level integration test that verifies whether results for a no-financials 
*	PVWatts case matches expected results.  
*	Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to PVWattsV5
*/

int pvwattsv5_nofinancial_testfile(ssc_data_t &data)
{
	//this sets whether or not the status prints
	ssc_module_exec_set_print(0);

	//check for out of memory
	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}

	//set the solar resource file name, using the weather file in the input folder
	//ifdef is so that it can run on multiple operating systems
	char hourly[256];
	int a = sprintf(hourly, "%s/test/input_cases/pvsamv1_data/USA AZ Phoenix (TMY2).csv", std::getenv("SSCDIR"));

	//set the variables for the PVWatts default case
	ssc_data_set_number(data, "system_use_lifetime_output", 0);
	ssc_data_set_number(data, "analysis_period", 25);
	ssc_data_set_string(data, "solar_resource_file", hourly); //file set above
	ssc_data_set_number(data, "system_capacity", 4);
	ssc_data_set_number(data, "module_type", 0);
	ssc_data_set_number(data, "dc_ac_ratio", 1.2000000476837158);
	ssc_data_set_number(data, "inv_eff", 96);
	ssc_data_set_number(data, "losses", 14.075660705566406);
	ssc_data_set_number(data, "array_type", 0);
	ssc_data_set_number(data, "tilt", 20);
	ssc_data_set_number(data, "azimuth", 180);
	ssc_data_set_number(data, "gcr", 0.40000000596046448);
	ssc_data_set_number(data, "adjust:constant", 0);

	return 0;

}

#endif



