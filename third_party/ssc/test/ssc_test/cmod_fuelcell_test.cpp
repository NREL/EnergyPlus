#include <gtest/gtest.h>

#include "cmod_fuelcell_test.h"

/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMFuelCell, NoFinancialModelFixed_cmod_fuelcell) {

	// Run with fixed output
	int errors = run_module(data, "fuelcell");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t startup_hours, fixed_pct, dynamic_response;
		ssc_data_get_number(data, "fuelcell_startup_time", &startup_hours);
		ssc_data_get_number(data, "fuelcell_fixed_pct", &fixed_pct);
		ssc_data_get_number(data, "fuelcell_dynamic_response_up", &dynamic_response);

		SetCalculatedArray("fuelcell_power");
		
		// Not started up
		for (size_t h = 0; h < startup_hours; h++) {
			EXPECT_EQ(calculated_array[h], 0);
		}
		// Ramping up, should be the dynamic response limit
		EXPECT_NEAR(calculated_array[(size_t)startup_hours], dynamic_response, 0.1);
		EXPECT_NEAR(calculated_array[(size_t)(startup_hours + 1)], 2 * dynamic_response, 0.1);

		for (size_t h = size_t(startup_hours + 2); h < 100; h++) {
			EXPECT_NEAR(calculated_array[h], fixed_pct, 0.1);
		}
	}
}
/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMFuelCell, NoFinancialModelFixedLifetime_cmod_fuelcell) {

	// Run with fixed output
	ssc_number_t n_years;
	ssc_data_get_number(data, "analysis_period", &n_years);
	size_t n_lifetime = (size_t)(n_years) * 8760;
	ssc_data_set_number(data, "system_use_lifetime_output", 1);

	int errors = run_module(data, "fuelcell");
	EXPECT_FALSE(errors);
	
	if (!errors)
	{
		ssc_number_t startup_hours, fixed_pct, dynamic_response;
		ssc_data_get_number(data, "fuelcell_startup_time", &startup_hours);
		ssc_data_get_number(data, "fuelcell_fixed_pct", &fixed_pct);
		ssc_data_get_number(data, "fuelcell_dynamic_response", &dynamic_response);
		
		int n;
		calculated_array = ssc_data_get_array(data, "fuelcell_power", &n);
		EXPECT_EQ(n_lifetime, (size_t)n);
		
	}
}
/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMFuelCell, FuelCellBattery_cmod_fuelcell) {

	// Run with fixed output
	ssc_data_set_number(data, "system_use_lifetime_output", 1);
	int errors = run_module(data, "fuelcell");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		int errors_battery = run_module(data, "battery");
		EXPECT_FALSE(errors_battery);
		if (!errors_battery)
		{
			int n;
			ssc_number_t * fc_to_load = GetArray("fuelcell_to_load", n);
			ssc_number_t * pv_to_load = GetArray("system_to_load", n);
			ssc_number_t * batt_to_load = GetArray("batt_to_load", n);
			ssc_number_t * grid_to_load = GetArray("grid_to_load", n);
			ssc_number_t * load = GetArray("load", n);

			for (int i = 0; i < n; i += (int)interval) {
				EXPECT_LT(fabs(load[i] - (fc_to_load[i] + pv_to_load[i] + batt_to_load[i] + grid_to_load[i])), 1.0);
			}

			
		}
	}
}


