#include "cmod_mhk_tidal_test.h"

TEST_F(CM_MHKTidal, ComputeModuleTest_cmod_mhk_tidal) {
	int mhk_tidal_errors = run_module(data, "mhk_tidal");
	ASSERT_EQ( mhk_tidal_errors , 0 );

	ssc_number_t annual_energy, average_power, device_rated_capacity, capacity_factor, lcoe_fcr;

	ssc_data_get_number(data, "annual_energy", &annual_energy);

	EXPECT_NEAR(annual_energy, 2161517.37607, 0.1);

	ssc_data_get_number(data, "device_average_power", &average_power);
	EXPECT_NEAR(average_power, 265.321, 0.1);

	ssc_data_get_number(data, "device_rated_capacity", &device_rated_capacity);
	EXPECT_NEAR(device_rated_capacity, 1115.0, 0.1);

	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 22.1299, 0.1);

	mhk_tidal_errors = run_module(data, "lcoefcr");
	ASSERT_EQ(mhk_tidal_errors, 0);

	ssc_data_get_number(data, "lcoe_fcr", &lcoe_fcr);
	EXPECT_NEAR(lcoe_fcr, 1.67476, 0.1);
}