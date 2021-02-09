#include <gtest/gtest.h>

#include "cmod_geothermal_costs_test.h"


//Fixture is currently testing binary plant (Conversion type = 0)
TEST_F(CMGeothermalCosts, CostModuleTest_cmod_geothermal_costs)
{
	//Check whether module runs with any errors:
	int geothermal_errors = run_module(data, "geothermal_costs");
	ASSERT_EQ(geothermal_errors, 0);
		
	ssc_number_t baseline_cost;
	ssc_data_get_number(data, "baseline_cost", &baseline_cost);
	EXPECT_NEAR(baseline_cost, 2300, 100 );
}
