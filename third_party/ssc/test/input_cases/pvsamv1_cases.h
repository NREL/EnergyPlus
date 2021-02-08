#ifndef _PVSAMV1_DATA_H_
#define _PVSAMV1_DATA_H_

#include <map>
#include "code_generator_utilities.h"
#include "pvsamv1_common_data.h"

/**
*   Data for high-level integration test that verifies whether results for a residential PV system in Phoenix
*   matches expected results.  Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to PVSAMV1
*/
int pvsam_residential_pheonix(ssc_data_t &data)
{
	belpe_default(data);
	int status = run_module(data, "belpe");

	pvsamv1_with_residential_default(data);
	status += run_module(data, "pvsamv1");

	utility_rate5_default(data);
	status += run_module(data, "utilityrate5");

	cashloan_default(data);
	status += run_module(data, "cashloan");

	return status;
}

#endif
