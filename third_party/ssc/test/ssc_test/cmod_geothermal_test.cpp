#include <gtest/gtest.h>
#include "cmod_geothermal_test.h"

TEST_F(CMGeothermal, SingleOwnerDefault_cmod_geothermal) {
	int geo_errors = run_module(data, "geothermal");
	ASSERT_EQ(geo_errors, 0);
	int grid_errors = run_module(data, "grid");
	EXPECT_EQ(grid_errors, 0);
	int singleowner_errors = run_module(data, "singleowner");
	EXPECT_EQ(singleowner_errors, 0);

	if (!geo_errors)	//(!=geothermal_errors) == True;
	{
		ssc_number_t annual_energy, eff_secondlaw;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		ssc_data_get_number(data, "eff_secondlaw", &eff_secondlaw);
		EXPECT_NEAR(annual_energy, 262800000, 0.1);
		EXPECT_GE(eff_secondlaw, 0);
	}

}