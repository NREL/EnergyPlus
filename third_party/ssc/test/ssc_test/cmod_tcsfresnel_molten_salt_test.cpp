#include <gtest/gtest.h>
#include "tcsfresnel_molten_salt_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_tower {}
using namespace csp_tower;

//========Tests===================================================================================
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, Default_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 337249089, kErrorToleranceLo);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 38.50, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 372639466, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3372, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.27, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 30058, kErrorToleranceLo);
    }
}

// Defocusing strategy: Sequenced
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, SequencedDefocusing_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("fthrctrl", 1);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 337249089, kErrorToleranceLo);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 38.50, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 372639466, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3372, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.27, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 30058, kErrorToleranceLo);
    }
}

// Field HTF: Therminol VP-1
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TherminolVp1Htf_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("Fluid", 21);
    power_fresnel.SetInput("field_fluid", 21);
    power_fresnel.SetInput("is_hx", 1);
    power_fresnel.SetInput("V_tank_hot_ini", 1290.5642);
    power_fresnel.SetInput("vol_tank", 6452.821);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 336171001, kErrorToleranceLo);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 38.38, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 371306661, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3362, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.31, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 29944, kErrorToleranceLo);
    }
}

// Optical characterization method: Solar position 
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, SolarPositioinOpticalChar_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("opt_model", 1);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 228344862, kErrorToleranceLo);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 26.07, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 255036717, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 2283, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 93.26, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 21776, kErrorToleranceLo);
    }
}

//// Receiver model type: Polynomial heat loss model
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, PolynomialHeatLoss_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("nLoops", 148);
//    power_fresnel.SetInput("rec_model", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Condenser type: Evaporative
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, EvaporativeCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("CT", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Condenser type: Hybrid
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, HybridCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("CT", 3);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Turbine inlet pressure control: Sliding pressure
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TurbineSlidingPressure_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("tech_type", 3);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// HTF freeze protection mode: Electric heating
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, ElectricFreezeProtection_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("fp_mode", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Storage HTF: Therminol VP-1
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TherminolVp1Storage_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("store_fluid", 21);
//    power_fresnel.SetInput("is_hx", 1);
//    power_fresnel.SetInput("V_tank_hot_ini", 1963.66443);
//    power_fresnel.SetInput("vol_tank", 9818.3223);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Power Cycle: User Defined
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, UserDefinedPowerCycle_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("pc_config", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
