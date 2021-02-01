#include <gtest/gtest.h>

#include "cmod_tcsfresnel_molten_salt_test.h"
#include "../tcs_test/tcsfresnel_molten_salt_cases.h"
#include "../input_cases/weather_inputs.h"


/// Test tcsfresnel_molten_salt with all default configurations with respect to the No Finanical model
TEST_F(CMTcsFresnelMoltenSalt, Rankine_Default_No_Financial_cmod_tcsfresnel_molten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsfresnel_molten_salt_tucson_default(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 337260930.158212, 337260930.158212 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 38.500145, 38.500145 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 372651034.745992, 372651034.745992 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3372.612727, 3372.612727 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 94.274118, 94.274118 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 30058.992530, 30058.992530 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test tcsfresnel_molten_salt with alternative defocusing strategy: Sequenced
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsFresnelMoltenSalt, Rankine_Defocusing_Strategy_No_Financial_cmod_tcsfresnel_molten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsfresnel_molten_salt_tucson_defocusing_strategy(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 337260930.158212, 337260930.158212 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 38.500145, 38.500145 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 372651034.745992, 372651034.745992 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3372.612727, 3372.612727 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 94.274118, 94.274118 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 30058.992530, 30058.992530 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test tcsfresnel_molten_salt with alternative Field HTF: Therminol VP-1
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsFresnelMoltenSalt, Rankine_Field_HTF_No_Financial_cmod_tcsfresnel_molten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsfresnel_molten_salt_tucson_field_HTF(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 336220223.708381, 336220223.708381 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 38.381343, 38.381343 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 371358844.515444, 371358844.515444 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3362.205652, 3362.205652 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 94.310237, 94.310237 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 29948.205347, 29948.205347 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test tcsfresnel_molten_salt with alternative optical characterization method: Solar position
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsFresnelMoltenSalt, Rankine_Optical_Char_No_Financial_cmod_tcsfresnel_molten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsfresnel_molten_salt_tucson_optical_char_solar(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 228402762.335069, 228402762.335069 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 26.073401, 26.073401 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 255099173.098881, 255099173.098881 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 2284.029943, 2284.029943 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 93.265512, 93.265512 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413000, 3.413000 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 21782.647609, 21782.647609 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test tcsfresnel_molten_salt with alternative receiver model type: Polynomial Heat Loss model
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_Polynomial_Heat_Loss_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_polynomial_heat_loss_model(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.3431e8, 3.3431e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 38.1633, 38.1633 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.70549e8, 3.70549e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3343.1, 3343.1 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 93.9793, 93.9793 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 29823.1, 29823.1 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative condenser type: Evaporative
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_Evap_Condenser_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_evap_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.54549e8, 3.54549e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 40.4736, 40.4736 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.84127e8, 3.84127e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3545.49, 3545.49 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 96.1456, 96.1456 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 998592, 998592 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative condenser type: Hybrid
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_Hybrid_Condenser_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_hybrid_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.30518e8, 3.30518e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 37.7304, 37.7304 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.68433e8, 3.68433e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3305.18, 3305.18 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 93.447, 93.447 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 29597.1, 29597.1 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative turbine inlet pressure control: Sliding pressure 
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_Sliding_P_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_sliding_p(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.14357e8, 3.14357e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 35.8856, 35.8856 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.49548e8, 3.49548e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3143.58, 3143.58 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 93.6795, 93.6795 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 28264.2, 28264.2 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative HTF freeze protection mode: Electric heating
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_HTF_Freeze_Protection_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_HTF_freeze_protection(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 2.95735e8, 2.95735e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 33.7597, 33.7597 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.68433e8, 3.68433e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 2957.35, 2957.35 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 83.6129, 83.6129 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 29597.1, 29597.1 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative storage HTF: Therminol VP-1
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, Rankine_Storage_HTF_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_storage_HTF(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.35099e8, 3.35099e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 38.2534, 38.2534 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.72551e8, 3.72551e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3351, 3351 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 93.6951, 93.6951 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 29869.8, 29869.8 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsfresnel_molten_salt with alternative Power Cycle: User Defined
/// Rest default configurations with respect to the No Financial model
//TEST_F(CMTcsFresnelMoltenSalt, UserDefined_Default_No_Financial_cmod_tcsfresnel_molten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsfresnel_molten_salt_tucson_userdefined_default(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 3.29573e8, 3.29573e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_fuel_usage;
//		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 37.6225, 37.6225 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 3.53789e8, 3.53789e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 3295.73, 3295.73 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 97.0368, 97.0368 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t system_heat_rate;
//		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 2708.93, 2708.93 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}