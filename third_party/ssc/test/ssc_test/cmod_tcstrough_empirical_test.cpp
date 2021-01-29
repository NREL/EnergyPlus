#include <gtest/gtest.h>

#include "cmod_tcstrough_empirical_test.h"
#include "../tcs_test/tcstrough_empirical_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test tcstrough_empirical with all defaults with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Default_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_default(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.44335e8, 3.44335e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 39.347, 39.347 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 4.04116e8, 4.04116e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3446.8, 3446.8 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 88.7574, 88.7574 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcstrough_empirical with alternative Solar Field HTF type: Hitec Solar Salt
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Solar_Field_HTF_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_solar_field_HTF(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.40714e8, 3.40714e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 38.9332, 38.9332 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 3.9992e8, 3.9992e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3410.55, 3410.55 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 88.7454, 88.7454 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Passed with m_error_tolerance_hi used for:
/// annual_W_cycle_gross & conversion_factor & kwh_per_kw

/// Test tcstrough_empirical with alternative Solar Collector Assembly (SCA): EuroTrough ET150
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_SCA_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_SCA(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.44114e8, 3.44114e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 39.3217, 39.3217 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 4.03352e8, 4.03352e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3444.58, 3444.58 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 88.8683, 88.8683 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Passed with m_error_tolerance_hi used for:
/// conversion_factor

/// Test tcstrough_empirical with alternative Heat Collection Element (HCE): Luz Cermet Vacuum, Luz Cermet Hydrogren, and Luz Cermet Broken Glass
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_HCE_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_HCE(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.11168e8, 3.11168e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 35.557, 35.557 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 3.69939e8, 3.69939e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3114.8, 3114.8 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 87.618, 87.618 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcstrough_empirical with alternative Power Cycle: APS Ormat 1MWe 300C
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Power_Cycle_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_power_cycle(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.5066e8, 3.5066e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 40.0698, 40.0698 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 4.27665e8, 4.27665e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3510.11, 3510.11 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 85.4106, 85.4106 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcstrough_empirical with alternative Thermal Storage Fluid Type: Therminol VP-1
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Thermal_Storage_Fluid_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_thermal_storage(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.4418e8, 3.4418e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 39.3292, 39.3292 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 4.03784e8, 4.03784e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3445.24, 3445.24 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 88.7901, 88.7901 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcstrough_empirical with alternative Parasitic Electric Energy Use: 500C Molten Salt HTF
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Parasitic_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_tucson_parasitic(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.58145e8, 3.58145e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 40.9251, 40.9251 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 4.04116e8, 4.04116e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3585.04, 3585.04 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 92.3171, 92.3171 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test tcstrough_empirical with alternativelocation: Phoenix, AZ
/// Rest default configurations with respect to the No Financial model
TEST_F(CMTcsTroughEmpirical, TroughEmpirical_Location_No_Financial_cmod_tcstrough_empirical) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcstrough_empirical_phoenix(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 3.3255e8, 3.3255e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_fuel_usage;
		ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
		EXPECT_NEAR(annual_fuel_usage, 0, 0 * m_error_tolerance_hi) << "Annual fuel usage";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 38.0003, 38.0003 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 3.91116e8, 3.91116e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 3328.83, 3328.83 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 88.5687, 88.5687 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_heat_rate;
		ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
		EXPECT_NEAR(system_heat_rate, 3.413, 3.413 * m_error_tolerance_hi) << "System heat rate";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t system_use_lifetime_output;
		ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
		EXPECT_NEAR(system_use_lifetime_output, 0, 0 * m_error_tolerance_hi) << "Use lifetime output";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

	}
}

/// Test series of Advanced Combinatorial Testing System (ACTS) runs 
//TEST_F(CMTcsTroughEmpirical, ACTS_Test_Empirical) {
//
//	std::vector<double> annual_energys{ 3.43731e8, 3.29297e8, 3.30659e8, 3.10436e8, 3.45672e8, 3.46698e8, 3.4736e8, 3.28189e8, 3.25897e8 };
//	std::vector<double> annual_fuel_usages{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
//	std::vector<double> capacity_factors{ 39.278, 37.6286, 37.7843, 35.4734, 39.4998, 39.617, 39.6926, 37.502, 37.2401 };
//	std::vector<double> annual_W_cycle_grosss{ 4.03426e8, 3.87146e8, 3.88914e8, 3.66079e8, 4.05593e8, 4.06767e8, 4.07472e8, 3.86119e8, 3.83311e8 };
//	std::vector<double> kwh_per_kws{ 3440.76, 3296.27, 3309.9, 3107.47, 3460.18, 3470.45, 3477.07, 3285.17, 3262.23 };
//	std::vector<double> conversion_factors{ 88.7533, 88.6016, 88.5637, 88.3337, 88.7773, 88.784, 88.7995, 88.5383, 88.5641 };
//	std::vector<double> system_heat_rates{ 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413 };
//	std::vector<double> system_use_lifetime_outputs{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
//
//	ssc_data_t data = ssc_data_create();
//
//	for (std::vector<double>::size_type i = 0; i != annual_energys.size(); i++) {
//		int test_errors = ACTS_test_empirical(data, i);
//
//		EXPECT_FALSE(test_errors);
//		if (!test_errors)
//		{
//			ssc_number_t annual_energy;
//			ssc_data_get_number(data, "annual_energy", &annual_energy);
//			EXPECT_NEAR(annual_energy, annual_energys[i], annual_energys[i] * m_error_tolerance_hi) << "Annual Energy";
//
//			ssc_number_t annual_fuel_usage;
//			ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//			EXPECT_NEAR(annual_fuel_usage, annual_fuel_usages[i], annual_fuel_usages[i] * m_error_tolerance_hi) << "Annual fuel usage";
//
//			ssc_number_t capacity_factor;
//			ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//			EXPECT_NEAR(capacity_factor, capacity_factors[i], capacity_factors[i] * m_error_tolerance_hi) << "Capacity Factor";
//
//			ssc_number_t annual_W_cycle_gross;
//			ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//			EXPECT_NEAR(annual_W_cycle_gross, annual_W_cycle_grosss[i], annual_W_cycle_grosss[i] * m_error_tolerance_hi) << "Annual W_cycle Gross";
//
//			ssc_number_t kwh_per_kw;
//			ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//			EXPECT_NEAR(kwh_per_kw, kwh_per_kws[i], kwh_per_kws[i] * m_error_tolerance_hi) << "kwh per kw";
//
//			ssc_number_t conversion_factor;
//			ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//			EXPECT_NEAR(conversion_factor, conversion_factors[i], conversion_factors[i] * m_error_tolerance_hi) << "Conversion Factor";
//
//			ssc_number_t system_heat_rate;
//			ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//			EXPECT_NEAR(system_heat_rate, system_heat_rates[i], system_heat_rates[i] * m_error_tolerance_hi) << "System heat rate";
//
//			ssc_number_t system_use_lifetime_output;
//			ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
//			EXPECT_NEAR(system_use_lifetime_output, system_use_lifetime_outputs[i], system_use_lifetime_outputs[i] * m_error_tolerance_hi) << "Use lifetime output";
//		}
//	}
//}



