#ifndef SYSTEM_ADVISOR_MODEL_CMOD_UTILITYRATE5_EQNS_H
#define SYSTEM_ADVISOR_MODEL_CMOD_UTILITYRATE5_EQNS_H

#include "sscapi.h"

#ifdef __cplusplus

extern "C" {
#endif

static const char *ElectricityRates_format_as_URDBv7_doc =
        "Format the electricty rate information in cmod_utilityrate5 to the format specified in:\\n"
        "https://openei.org/services/doc/rest/util_rates/?version=7\\n"
        "Input: var_table with key-value pairs:  \\n"
        "     'ur_metering_option': int, [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]"
        "     'ur_monthly_fixed_charge': double [$], Monthly fixed charge\\n"
        "     'ur_monthly_min_charge': double [$], Monthly minimum charge\\n"
        "     'ur_annual_min_charge': optional double [$], Annual minimum charge\\n"
        "     'ur_ec_sched_weekday': optional matrix [period], Energy charge weekday schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_ec_sched_weekend': optional matrix [period], Energy charge weekend schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_ec_tou_mat': optional matrix [[period, tier, kwh, $], Demand rates (TOU), each row: (period, tier, max usage, units=='kWh', buy rate, sell rate)\\n"
        "     'ur_dc_flat_mat' optional matrix [[month, tier, kW, $]] - Demand rates (flat), each row: (month, tier, peak demand, buy rate) \\n"
        "     'ur_dc_sched_weekday': optional matrix [period], Demand charge weekday schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_dc_sched_weekend': optional matrix [period], Demand charge weekend schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_dc_tou_mat': optional matrix [[period, tier, kWh, bool, $/kWh, $/kWh], Energy rates (TOU), each row: (period, tier, max usage, units=='kW', buy rate, sell rate)\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'urdb_data' - table, data formatted as URDB v7 response\\n"
        "     'log' - string";

SSCEXPORT void ElectricityRates_format_as_URDBv7(ssc_data_t data);

}

#endif //SYSTEM_ADVISOR_MODEL_CMOD_UTILITYRATE5_EQNS_H
