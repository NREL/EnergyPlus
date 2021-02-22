#ifndef _TROUGH_PHYSICAL_CASES_H_
#define _TROUGH_PHYSICAL_CASES_H_

#include <map>
#include "../input_cases/code_generator_utilities.h"
#include "trough_physical_common_data.h"

/**
*   Data for high-level integration tests that verifies whether results for a parabolic trough
*   plant in Tucson matches expected results.
*   Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to trough_physical
*/
int trough_physical_tucson(ssc_data_t &data)
{
	trough_physical_default(data);
	int status = run_module(data, "trough_physical");

    //convert_and_adjust_fixed_charge(data);
    //status += run_module(data, "iph_to_lcoefcr");

    //fixed_charge_rate_default(data);
    //status += run_module(data, "lcoefcr");

	return status;
}

#endif
