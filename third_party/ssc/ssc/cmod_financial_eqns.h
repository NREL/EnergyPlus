#ifndef _CMOD_FINANCIAL_EQNS_H_
#define _CMOD_FINANCIAL_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

    static const char* Financial_Construction_Financing_Equations_doc =
        "Calculates the financing cost of up to five short-term construction loans, as used on the Financial Construction Financing UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'const_per_percent1' - double [%]\\n"
        "     'const_per_interest_rate1' - double [$]\\n"
        "     'const_per_months1' - double [months]\\n"
        "     'const_per_upfront_rate1' - double [%]\\n"
        "     'const_per_percent2' - double [%]\\n"
        "     'const_per_interest_rate2' - double [$]\\n"
        "     'const_per_months2' - double [months]\\n"
        "     'const_per_upfront_rate2' - double [%]\\n"
        "     'const_per_percent3' - double [%]\\n"
        "     'const_per_interest_rate3' - double [$]\\n"
        "     'const_per_months3' - double [months]\\n"
        "     'const_per_upfront_rate3' - double [%]\\n"
        "     'const_per_percent4' - double [%]\\n"
        "     'const_per_interest_rate4' - double [$]\\n"
        "     'const_per_months4' - double [months]\\n"
        "     'const_per_upfront_rate4' - double [%]\\n"
        "     'const_per_percent5' - double [%]\\n"
        "     'const_per_interest_rate5' - double [$]\\n"
        "     'const_per_months5' - double [months]\\n"
        "     'const_per_upfront_rate5' - double [%]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'const_per_principal1' - double [$]\\n"
        "     'const_per_interest1' - double [$]\\n"
        "     'const_per_total1' - double [$]\\n"
        "     'const_per_principal2' - double [$]\\n"
        "     'const_per_interest2' - double [$]\\n"
        "     'const_per_total2' - double [$]\\n"
        "     'const_per_principal3' - double [$]\\n"
        "     'const_per_interest3' - double [$]\\n"
        "     'const_per_total3' - double [$]\\n"
        "     'const_per_principal4' - double [$]\\n"
        "     'const_per_interest4' - double [$]\\n"
        "     'const_per_total4' - double [$]\\n"
        "     'const_per_principal5' - double [$]\\n"
        "     'const_per_interest5' - double [$]\\n"
        "     'const_per_total5' - double [$]\\n"
        "     'const_per_principal_total' - double [$]\\n"
        "     'const_per_percent_total' - double [$]\\n"
        "     'construction_financing_cost' - double [$]\\n"
        "     'const_per_interest_total' - double [$]\\n";

    SSCEXPORT void Financial_Construction_Financing_Equations(ssc_data_t data);


    static const char* Financial_Capacity_Payments_Equations_doc =
        "Calculates system nameplate, as used on the Financial Capacity Payments UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'system_capacity' - double [kW]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'cp_system_nameplate' - double [MW]\\n";

    SSCEXPORT void Financial_Capacity_Payments_Equations(ssc_data_t data);

#ifdef __cplusplus
}
#endif

#endif
