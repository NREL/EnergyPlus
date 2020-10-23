#include <gtest/gtest.h>

#include "cmod_trough_physical_iph_test.h"
#include "../tcs_test/trough_physical_iph_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test trough_physical_iph with all defaults and no-financial model
TEST_F(CMTroughPhysicalIPH, DefaultNoFinancialModel_cmod_trough_physical_iph){
	
	int test_errors = run_module(data, "trough_physical_process_heat");

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
        ssc_number_t annual_gross_energy;
        ssc_data_get_number(data, "annual_gross_energy", &annual_gross_energy);
        EXPECT_NEAR(annual_gross_energy, 24480390.431956, 24480390.431956 * m_error_tolerance_hi) << "Annual Gross Energy";

        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 24480390.431956, 24480390.431956 * m_error_tolerance_hi) << "Annual Energy";

        ssc_number_t annual_electricity_consumption;
        ssc_data_get_number(data, "annual_electricity_consumption", &annual_electricity_consumption);
        EXPECT_NEAR(annual_electricity_consumption, 93788., 93788. * m_error_tolerance_hi) << "Annual Electricity Consumption";

        //ssc_number_t fixed_operating_cost;
        //ssc_data_get_number(data, "fixed_operating_cost", &fixed_operating_cost);
        //EXPECT_NEAR(fixed_operating_cost, 111118, 111118 * m_error_tolerance_hi) << "Fixed Operating Cost";

        ssc_number_t annual_thermal_consumption;
        ssc_data_get_number(data, "annual_thermal_consumption", &annual_thermal_consumption);
        EXPECT_NEAR(annual_thermal_consumption, 247.65286, 247.65286 * m_error_tolerance_hi) << "Annual Thermal Consumption";

        ssc_number_t annual_tes_freeze_protection;
        ssc_data_get_number(data, "annual_tes_freeze_protection", &annual_tes_freeze_protection);
        EXPECT_NEAR(annual_tes_freeze_protection, 247.65286, 247.65286 * m_error_tolerance_hi) << "Annual TES Freeze Protection";

        ssc_number_t annual_field_freeze_protection;
        ssc_data_get_number(data, "annual_field_freeze_protection", &annual_field_freeze_protection);
        EXPECT_NEAR(annual_field_freeze_protection, 0., m_error_tolerance_hi) << "Annual Field Freeze Protection";

        //ssc_number_t lcoe_fcr;
        //ssc_data_get_number(data, "lcoe_fcr", &lcoe_fcr);
        //EXPECT_NEAR(lcoe_fcr, 0.0375859, 0.0375859 * m_error_tolerance_hi) << "LCOE FCR";

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 176.332800, 176.332800 * m_error_tolerance_hi) << "Annual Total Water Use";

		//ssc_number_t VARIABLE;
		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";
	}
}

/// Test trough_physical_iph with all defaults and the financial model in the LCOH Calculator
//TEST_F(CMTroughPhysicalIPH, DefaultLCOHFinancialModel_cmod_trough_physical_iph) {
//
//    ssc_data_t data = ssc_data_create();
//    int test_errors = trough_physical_iph_tucson(data);
//
//    EXPECT_FALSE(test_errors);
//    if (!test_errors)
//    {
//        ssc_number_t annual_gross_energy;
//        ssc_data_get_number(data, "annual_gross_energy", &annual_gross_energy);
//        EXPECT_NEAR(annual_gross_energy, 2.44933e7, 2.44933e7 * m_error_tolerance_lo) << "Annual Gross Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_energy;
//        ssc_data_get_number(data, "annual_energy", &annual_energy);
//        EXPECT_NEAR(annual_energy, 2.44931e7, 2.44931e7 * m_error_tolerance_lo) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_electricity_consumption;
//        ssc_data_get_number(data, "annual_electricity_consumption", &annual_electricity_consumption);
//        EXPECT_NEAR(annual_electricity_consumption, 132796, 132796 * m_error_tolerance_lo) << "Annual Electricity Consumption";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t fixed_operating_cost;
//        ssc_data_get_number(data, "fixed_operating_cost", &fixed_operating_cost);
//        EXPECT_NEAR(fixed_operating_cost, 111726, 111726 * m_error_tolerance_lo) << "Fixed Operating Cost";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_thermal_consumption;
//        ssc_data_get_number(data, "annual_thermal_consumption", &annual_thermal_consumption);
//        EXPECT_NEAR(annual_thermal_consumption, 232.282, 232.282 * m_error_tolerance_lo) << "Annual Thermal Consumption";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_tes_freeze_protection;
//        ssc_data_get_number(data, "annual_tes_freeze_protection", &annual_tes_freeze_protection);
//        EXPECT_NEAR(annual_tes_freeze_protection, 232.282, 232.282 * m_error_tolerance_lo) << "Annual TES Freeze Protection";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_field_freeze_protection;
//        ssc_data_get_number(data, "annual_field_freeze_protection", &annual_field_freeze_protection);
//        EXPECT_NEAR(annual_field_freeze_protection, 0., m_error_tolerance_lo) << "Annual Field Freeze Protection";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t lcoe_fcr;
//        ssc_data_get_number(data, "lcoe_fcr", &lcoe_fcr);
//        EXPECT_NEAR(lcoe_fcr, 0.0376277, 0.0376277 * m_error_tolerance_lo) << "LCOE FCR";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_total_water_use;
//        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//        EXPECT_NEAR(annual_total_water_use, 176.333, 176.333 * m_error_tolerance_lo) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        //ssc_number_t VARIABLE;
//        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_lo) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//    }
//}

//TestResult iphTroughLCOHDefaultResult[] = {
//    /*  SSC Var Name                            Test Type           Test Result             Error Bound % */
//        { "annual_gross_energy",                NR,                 2.44933e7,              0.1 },  // Annual Gross Thermal Energy Production w/ avail derate [kWt-hr]
//        { "annual_energy",                      NR,                 2.44931e7,              0.1 },  // Annual Net Thermal Energy Production w/ avail derate [kWt-hr]
//        { "annual_electricity_consumption",     NR,                 122659,                 0.1 },  // Annual electricity consumptoin w/ avail derate [kWe-hr]
//        { "fixed_operating_cost",               NR,                 111118,                 0.1 },  // Annual fixed operating cost [$/kW]
//        { "annual_thermal_consumption",         NR,                 236.609,                0.1 },  // Annual thermal freeze protection required [kWt-hr]
//        { "annual_tes_freeze_protection",       NR,                 236.609,                0.1 },  // Annual thermal power for TES freeze protection [kWt-hr]
//        { "annual_field_freeze_protection",     NR,                 0.,                     0.1 },  // Annual thermal power for field freeze protection [kWt-hr]
//        { "lcoe_fcr",                           NR,                 0.0375859,              0.1 },  // Levelized cost of energy [$/kWh]
//        { "annual_total_water_use",             NR,                 176.333,                0.1 },  // Total Annual Water Usage [m^3]
//};
