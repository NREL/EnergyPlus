#ifndef _TROUGH_PHYSICAL_IPH_CASES_H_
#define _TROUGH_PHYSICAL_IPH_CASES_H_

#include <map>
#include "../input_cases/code_generator_utilities.h"
#include "trough_physical_iph_common_data.h"

/**
*   Data for high-level integration tests that verifies whether results for a parabolic trough
*   industrial process heat (iph) plant in Tucson matches expected results.
*   Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to trough_physical_iph
*/
//int trough_physical_iph_tucson(ssc_data_t &data)
//{
//	trough_physical_iph_default(data);
//	int status = run_module(data, "trough_physical_process_heat");
//
//    convert_and_adjust_fixed_charge(data);
//    status += run_module(data, "iph_to_lcoefcr");
//
//    fixed_charge_rate_default(data);
//    status += run_module(data, "lcoefcr");
//
//	return status;
//}

#endif
