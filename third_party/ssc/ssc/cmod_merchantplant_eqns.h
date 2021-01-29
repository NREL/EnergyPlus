#ifndef _CMOD_MERCHANTPLANT_BUILDER_H_
#define _CMOD_MERCHANTPLANT_BUILDER_H_

#include "vartab.h"
#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char* mp_ancillary_services_doc =
    "Checks the capacities specified in the Ancillary Services markets against the system capacity\\n\\n"
    "Input: var_table with key-value pairs\\n"
    "     'analysis_period': double [-], Years in project lifetime to simulate\\n"
	"     'mp_enable_energy_market_revenue': boolean [-], Enable energy market revenue, 0 is false\\n"
	"     'mp_enable_ancserv1': boolean [-]\\n"
	"     'mp_enable_ancserv2': boolean [-]\\n"
	"     'mp_enable_ancserv3': boolean [-]\\n"
	"     'mp_enable_ancserv4': boolean [-]\\n"
	"     'mp_energy_market_revenue': matrix [MW, $/MW]\\n"
	"     'mp_ancserv1_revenue': matrix [MW, $/MW]\\n"
	"     'mp_ancserv2_revenue': matrix [MW, $/MW]\\n"
	"     'mp_ancserv3_revenue': matrix [MW, $/MW]\\n"
	"     'mp_ancserv4_revenue': matrix [MW, $/MW]\\n"
    "     'system_capacity': conditional double [kW], required if gen is not provided\\n"
    "     'gen': conditional array [kW], required if system_capacity is not provided\\n"
    "     'mp_calculate_revenue': boolean [-], 0 is false\\n\\n"
    "Output: key-value pairs added to var_table\\n"
	"     'mp_capacity_check': boolean\\n"
	"     'mp_capacity_check_error': string\\n";

SSCEXPORT void mp_ancillary_services(ssc_data_t data);


#ifdef __cplusplus
}
#endif

#endif
