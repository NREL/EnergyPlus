#include <gtest/gtest.h>
#include "tcsdirect_steam_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"


namespace csp_tower {}
using namespace csp_tower;

//========Tests===================================================================================
NAMESPACE_TEST(csp_tower, SteamTowerCmod, Default_NoFinancial)
{
    ssc_data_t defaults = tcsdirect_steam_defaults();
    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
    int errors = steam_tower.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 263809742, kErrorToleranceLo);
        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 30.08, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 296630582, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2635, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 92.64, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 55716, kErrorToleranceLo);
    }
}

// Alternative condenser type : Evaporative
NAMESPACE_TEST(csp_tower, SteamTowerCmod, EvaporativeCondenser_NoFinancial)
{
    ssc_data_t defaults = tcsdirect_steam_defaults();
    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
    steam_tower.SetInput("ct", 1);
    steam_tower.SetInput("eta_ref", 0.404);
    steam_tower.SetInput("startup_frac", 0.5);
    steam_tower.SetInput("P_cond_min", 2);

    int errors = steam_tower.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 280975356, kErrorToleranceLo);
        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 32.03, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 307624737, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2806, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 95.14, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 893431, kErrorToleranceLo);
    }
}

// Alternative condenser type : Hybrid
NAMESPACE_TEST(csp_tower, SteamTowerCmod, HybridCondenser_NoFinancial)
{
    ssc_data_t defaults = tcsdirect_steam_defaults();
    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
    steam_tower.SetInput("ct", 3);
    steam_tower.SetInput("eta_ref", 0.404);
    steam_tower.SetInput("startup_frac", 0.5);
    steam_tower.SetInput("P_cond_min", 2);

    int errors = steam_tower.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 268116066, kErrorToleranceLo);
        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 30.57, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 304066728, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2678, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 91.85, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 55716, kErrorToleranceLo);
    }
}

//// Fossil dispatch
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, FossilDispatch_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("fossil_mode", 2);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Receiver material : T91 Steel
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, ReceiverT91Steel_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("mat_boiler", 28);
//    steam_tower.SetInput("mat_sh", 28);
//    steam_tower.SetInput("mat_rh", 28);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Alternative receiver flow pattern
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, FlowPattern1_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("flowtype", 1);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Alternative heliostat focusing method: Flat
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, HeliostatFlatFocusing_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("focus_type", 0);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//
//// Alternative heliostat canting method: Equinox
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, HeliostatEquinoxCanting_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("cant_type", 2);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Phoenix, AZ
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, Phoeniz_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    char solar_resource_path_tucson[512];
//    int n2 = sprintf(solar_resource_path_tucson, "%s/test/input_cases/directsteam_data/tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
//    steam_tower.SetInput("solar_resource_file", solar_resource_path_tucson);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
