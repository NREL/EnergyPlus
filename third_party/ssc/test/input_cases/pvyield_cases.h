#ifndef _PVSAMV1_DATA_H_
#define _PVSAMV1_DATA_H_

#include <map>
#include "code_generator_utilities.h"
#include "pvyield_common_data.h"

/**
*   Data for high-level integration test that verifies whether results for a residential PV system in Phoenix
*   matches expected results.  Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to PVSAMV1
*/
int pvyield_test(ssc_data_t &data)
{
	pvyield_no_financial_meteo(data);
	int status = run_module(data, "pvsamv1");

	return status;
}

int pvyield_test_user_support_80603_meteo(ssc_data_t &data)
{
	pvyield_user_support_80603_meteo(data);
	int status = run_module(data, "pvsamv1");

	return status;
}

int pvyield_test_user_support_80603_AZ(ssc_data_t &data)
{
	pvyield_user_support_80603_AZ(data);
	int status = run_module(data, "pvsamv1");

	return status;
}

#endif
