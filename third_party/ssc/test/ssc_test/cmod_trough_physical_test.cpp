#include <gtest/gtest.h>
#include "trough_physical_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_trough {}
using namespace csp_trough;

//========Tests===================================================================================
NAMESPACE_TEST(csp_trough, PowerTroughCmod, Default_NoFinancial)
{
    ssc_data_t defaults = trough_physical_defaults();
    CmodUnderTest power_trough = CmodUnderTest("trough_physical", defaults);
    int errors = power_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_energy"), 369272759, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_thermal_consumption"), 596547, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_tes_freeze_protection"), 558505, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_field_freeze_protection"), 38042, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("capacity_factor"), 42.20, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_W_cycle_gross"), 420379150, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("kwh_per_kw"), 3696, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("conversion_factor"), 87.84, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_total_water_use"), 80708, kErrorToleranceLo);
    }

    //ssc_data_t defaults = singleowner_defaults();
    //CmodUnderTest singleowner = CmodUnderTest("singleowner", defaults);
    //int errors = singleowner.RunModule();
    //EXPECT_FALSE(errors);
    //if (!errors) {
    //    EXPECT_NEAR_FRAC(singleowner.GetOutput(""), , kErrorToleranceLo);
    //}
}
