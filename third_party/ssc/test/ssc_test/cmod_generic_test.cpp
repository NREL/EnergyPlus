#include <gtest/gtest.h>

#include "cmod_generic_test.h"

/// Test Generic System with Battery for SingleOwner PPA
TEST_F(CMGeneric, SingleOwnerWithBattery_cmod_generic) {

	generic_singleowner_battery_60min(data);

	// Test different dispatch strategies
	std::vector<size_t> dispatch_options{ 0,1,3,4 };

	// Run with hourly data
	for (size_t i = 0; i < dispatch_options.size(); i++) {
		ssc_data_set_number(data, "batt_dispatch_choice", (ssc_number_t)dispatch_options[i]);
		EXPECT_FALSE(run_module(data, "generic_system"));
		EXPECT_FALSE(run_module(data, "battery"));
		EXPECT_FALSE(run_module(data, "singleowner"));
	}
	
	// Run with subhourly data
	set_array(data, "energy_output_array", generictest::gen_path_30min, 8760 * 2);
	set_array(data, "batt_custom_dispatch", generictest::batt_dispatch_path_30min, 8760 * 2);
	set_array(data, "batt_room_temperature_celsius", generictest::temperature_path_30min, 8760 * 2);
	for (size_t i = 0; i < dispatch_options.size(); i++) {
		ssc_data_set_number(data, "batt_dispatch_choice", (ssc_number_t)dispatch_options[i]);
		EXPECT_FALSE(run_module(data, "generic_system"));
		EXPECT_FALSE(run_module(data, "battery"));
		EXPECT_FALSE(run_module(data, "singleowner"));
	}

	// Test with incorrect combo of data sizes
	ssc_data_set_number(data, "batt_dispatch_choice", 3);
	set_array(data, "batt_custom_dispatch", generictest::batt_dispatch_path_30min, 8760*2); // 8760 or 8760 * 3 fails with execution error on Linux and windows
	EXPECT_FALSE(run_module(data, "generic_system"));
	EXPECT_FALSE(run_module(data, "battery"));
}

/// Test Generic System with Battery for various timesteps
TEST_F(CMGeneric, CommercialWithBattery_cmod_generic) {

	generic_commerical_battery_60min(data);
	
	// Test different dispatch strategies
	std::vector<size_t> dispatch_options{ 0,3,4 };

	// Run with hourly data, with and without lifetime
	for (size_t l = 0; l < 2; l++) {
		ssc_data_set_number(data, "system_use_lifetime_output", l);
		for (size_t i = 0; i < dispatch_options.size(); i++) {
			ssc_data_set_number(data, "batt_dispatch_choice", (ssc_number_t)dispatch_options[i]);
			EXPECT_FALSE(run_module(data, "generic_system"));
			EXPECT_FALSE(run_module(data, "battery"));
			EXPECT_FALSE(run_module(data, "utilityrate5"));
			EXPECT_FALSE(run_module(data, "cashloan"));
		}
	}

	// Run with subhourly data
	set_array(data, "energy_output_array", generictest::gen_path_30min, 8760 * 2);
	set_array(data, "batt_custom_dispatch", generictest::batt_dispatch_path_30min, 8760 * 2);
	set_array(data, "batt_room_temperature_celsius", generictest::temperature_path_30min, 8760 * 2);
	set_array(data, "load", generictest::load_profile_path_30min, 8760 * 2);

	// With and without lifetime
	for (size_t l = 0; l < 2; l++) {
		ssc_data_set_number(data, "system_use_lifetime_output", l);
		for (size_t i = 0; i < dispatch_options.size(); i++) {
			ssc_data_set_number(data, "batt_dispatch_choice", (ssc_number_t)dispatch_options[i]);
			EXPECT_FALSE(run_module(data, "generic_system"));
			EXPECT_FALSE(run_module(data, "battery"));
			EXPECT_FALSE(run_module(data, "utilityrate5"));
			EXPECT_FALSE(run_module(data, "cashloan"));
		}
	}
	
	// Test with hourly load, subhourly gen
	ssc_data_set_number(data, "batt_dispatch_choice", 3);
	set_array(data, "batt_custom_dispatch", generictest::batt_dispatch_path_30min, 8760 * 2);
	set_array(data, "energy_output_array", generictest::gen_path_30min, 2*8760);
	set_array(data, "batt_room_temperature_celsius", generictest::temperature_path_30min, 8760 * 2);
	set_array(data, "load", generictest::load_profile_path_60min, 8760);
	
	for (size_t l = 0; l < 2; l++) {
		ssc_data_set_number(data, "system_use_lifetime_output", l);
		EXPECT_FALSE(run_module(data, "generic_system"));
		EXPECT_FALSE(run_module(data, "battery"));
	}
}

/*
Doesn't work to to outdated exeception handling methods in SSC which can not be 
handled robustly in a cross-platform environment
https://docs.microsoft.com/en-us/cpp/cpp/errors-and-exception-handling-modern-cpp?view=vs-2017#c-exceptions-versus-windows-seh-exceptions
*/
/*
TEST_F(CMGeneric, CommericalWithBatteryWrongSizes)
{
	// Test with hourly gen, subhourly load, should fail everywhere
	generic_commerical_battery_60min(data);
	set_array(data, "load", generictest::load_profile_path_30min, 2 * 8760);

	EXPECT_TRUE(run_module(data, "generic_system"));
	EXPECT_TRUE(run_module(data, "battery"));
}
*/
