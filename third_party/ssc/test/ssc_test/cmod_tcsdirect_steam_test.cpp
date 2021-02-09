#include <gtest/gtest.h>

#include "cmod_tcsdirect_steam_test.h"
#include "../tcs_test/tcsdirect_steam_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test tcsdirect_steam with all defaults and the single owner financial model
TEST_F(CMTcsDirectSteam, DirectSteam_Default_SingleOwner_cmod_tcsdirect_steam) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsdirect_steam_daggett_default(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 263818768.982372, 263818768.982372 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
		
		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 30.078699, 30.078699 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 296640437.816735, 296640437.816735 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 2634.894072, 2634.894072 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 92.641187, 92.641187 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
		
		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 55716.172107, 55716.172107 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcsdirect_steam with alternative condenser type: Evaporative
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsDirectSteam, DirectSteam_Evap_Condenser_SingleOwner_cmod_tcsdirect_steam) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsdirect_steam_daggett_evap_condenser(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 280986033.690457, 280986033.690457 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 32.035986, 32.035986 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 307636143.733944, 307636143.733944 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 2806.352396, 2806.352396 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 95.142849, 95.142849 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
		
		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 893438.592909, 893438.592909 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi


	}
}

/// Test tcsdirect_steam with alternative condenser type: Hybrid
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsDirectSteam, DirectSteam_Hybrid_Condenser_SingleOwner_cmod_tcsdirect_steam) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsdirect_steam_daggett_hybrid_condenser(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 268125210.231106, 268125210.231106 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 30.569689, 30.569689 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 304076894.573157, 304076894.573157 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 2677.904721, 2677.904721 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 91.850813, 91.850813 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 55716.584942, 55716.584942 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi


	}
}

/// Test tcsdirect_steam with alternative fossil dispatch mode: Supplemental mode
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Fossil_Supplemental_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_fossil_dispatch_supplemental(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 268125210.231106, 268125210.231106 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 30.569689, 30.569689 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 304076894.573157, 304076894.573157 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2677.904721, 2677.904721 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 91.850813, 91.850813 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55716.584942, 55716.584942 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}

/// Test tcsdirect_steam with alternative Direct Steam Receiver material: T91 Steel
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Direct_Steam_Receiver_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_direct_steam_receiver(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.68312e8, 2.68312e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 30.591, 30.591 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.01568e8, 3.01568e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2679.77, 2679.77 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 92.6794, 92.6794 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55574.5, 55574.5 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}

/// Test tcsdirect_steam with alternative flow pattern: 1
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Flow_Pattern_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_flow_pattern(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.67815e8, 2.67815e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 30.5343, 30.5343 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.01038e8, 3.01038e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2674.81, 2674.81 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 92.6707, 92.6707 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55540.2, 55540.2 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}

/// Test tcsdirect_steam with alternative Heliostat focusing method: Flat
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Heliostat_Focusing_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_focusing_method(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.68027e8, 2.68027e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 30.5585, 30.5585 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.01252e8, 3.01252e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2676.92, 2676.92 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 92.678, 92.678 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55547.3, 55547.3 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}

/// Test tcsdirect_steam with alternative Heliostat canting method: Equinox
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Heliostat_Canting_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_canting_method(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.68848e8, 2.68848e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 30.6521, 30.6521 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.02142e8, 3.02142e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2685.12, 2685.12 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 92.6883, 92.6883 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55606.8, 55606.8 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}

/// Test tcsdirect_steam with alternative location: Tucson, AZ
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsDirectSteam, DirectSteam_Location_Tucson_AZ_SingleOwner_cmod_tcsdirect_steam) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsdirect_steam_daggett_tucson_AZ(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.55075e8, 2.55075e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 29.0817, 29.0817 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 2.87524e8, 2.87524e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2547.56, 2547.56 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 92.4107, 92.4107 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 54685.3, 54685.3 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//
//	}
//}


