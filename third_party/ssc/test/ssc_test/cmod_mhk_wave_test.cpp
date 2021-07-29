#include "cmod_mhk_wave_test.h"

TEST_F(CM_MHKWave, ComputeModuleTest_cmod_mhk_wave) {
	int mhk_wave_errors = run_module(data, "mhk_wave");
	ASSERT_EQ(mhk_wave_errors, 0);
	
	ssc_number_t annual_energy, average_power, capacity_factor, lcoe_fcr;

	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 607850.58949, 0.1);
	
	ssc_data_get_number(data, "average_power", &average_power);
	EXPECT_NEAR(average_power,74.6122, 0.5);
	
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 24.262, 0.1);

	mhk_wave_errors = run_module(data, "lcoefcr");
	ASSERT_EQ(mhk_wave_errors, 0);

	ssc_data_get_number(data, "lcoe_fcr", &lcoe_fcr);
	EXPECT_NEAR(lcoe_fcr, 4.18968, 0.1);

}