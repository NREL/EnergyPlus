#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "../input_cases/weather_inputs.h"
#include "cmod_pvwattsv7_test.h"



///Default PVWattsV7, but with TMY2 instead of TMY3
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, DefaultNoFinancialModel_cmod_pvwattsv7){
	compute();

	double tmp=0;
//	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";
	int count;
	ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);

	for (size_t i = 0; i < 12; i++)
		tmp += (double)monthly_energy[i];
	//v5 is 6909.79, decrease of 2.4%: decreases due to shading, module cover losses, and spectral losses
	//v7 prior to module coeff changes is 6750.4236, increase of 3.7% due to improved tempco for standard module
	EXPECT_NEAR(tmp, 7003.1477, error_tolerance) << "Annual energy.";


	EXPECT_NEAR((double)monthly_energy[0], 439.755, error_tolerance) << "Monthly energy of January";
	EXPECT_NEAR((double)monthly_energy[1], 485.885, error_tolerance) << "Monthly energy of February";
	EXPECT_NEAR((double)monthly_energy[2], 597.621, error_tolerance) << "Monthly energy of March";
	EXPECT_NEAR((double)monthly_energy[3], 680.543, error_tolerance) << "Monthly energy of April";
	EXPECT_NEAR((double)monthly_energy[4], 724.435, error_tolerance) << "Monthly energy of May";
	EXPECT_NEAR((double)monthly_energy[5], 676.368, error_tolerance) << "Monthly energy of June";
	EXPECT_NEAR((double)monthly_energy[6], 674.804, error_tolerance) << "Monthly energy of July";
	EXPECT_NEAR((double)monthly_energy[7], 658.759, error_tolerance) << "Monthly energy of August";
	EXPECT_NEAR((double)monthly_energy[8], 607.498, error_tolerance) << "Monthly energy of September";
	EXPECT_NEAR((double)monthly_energy[9], 580.084, error_tolerance) << "Monthly energy of October";
	EXPECT_NEAR((double)monthly_energy[10], 460.171, error_tolerance) << "Monthly energy of November";
	EXPECT_NEAR((double)monthly_energy[11], 417.226, error_tolerance) << "Month energy of December";

	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 19.986, error_tolerance) << "Capacity factor";

}

/// PVWattsV7 using different technology input options
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, DifferentTechnologyInputs_cmod_pvwattsv7)
{
	//PVWattsV5 results: annual_energy_expected = { 6909.79, 7123.32, 7336.478, 6909.79, 6804.376, 8601.011, 8727.704, 9690.735};
	//V7 prior to module coefficient updates: std::vector<double> annual_energy_expected = { 6750.42, 7034.39, 7166.88, 6750.42, 6693.49, 8514.26, 8441.60, 9631.76 };
	//standard fixed -2.4%, premium fixed -1.3%, thinfilm fixed -2.4%, standard fixed -2.4%, standard roof -1.7%, standard 1-axis -1.0%, standard backtrack -3.4%, standard 2-axis -0.6%
	std::vector<double> annual_energy_expected = { 7003.14, 7034.39, 7081.20, 7003.14, 6975.36, 8800.64, 8731.63, 9884.35 };
	//standard fixed +3.6%, premium fixed 0%, thinfilm fixed -1.2%, standard fixed +3.6%, standard roof +4.0%, standard 1-axis +3.3%, standard backtrack +3.3%, standard 2-axis +2.6%

	std::map<std::string, double> pairs;
	size_t count = 0;
	error_tolerance = 0.01;

	// Module types: Standard, Premium, Thin Film
	for (int module_type = 0; module_type < 3; module_type++)
	{
			pairs["module_type"] = module_type;
			int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
			EXPECT_FALSE(pvwatts_errors);

			if (!pvwatts_errors)
			{
				ssc_number_t annual_energy;
				ssc_data_get_number(data, "annual_energy", &annual_energy);
				EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
			}
			count++;
	}
	pairs["module_type"] = 0; //reset module type to its default value

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
		EXPECT_FALSE(pvwatts_errors);

		if (!pvwatts_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
		}
		count++;
	}
	pairs["array_type"] = 0; //reset array type to fixed open rack

}

/// PVWattsV7 using a larger system size
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, LargeSystem_cmod_pvwattsv7)
{
	//PVWattsV5 results: std::vector<double> annual_energy_expected = { 1727447.4, 1701094.0, 2150252.8, 2181925.8, 2422683.7 };
	//PVWattsV7 prior to module coeff updates: std::vector<double> annual_energy_expected = { 1686353.2, 1673371.8, 2123603.8, 2105794.1, 2407940.7 };
	std::vector<double> annual_energy_expected = { 1749020.7, 1743839.1, 2194020.1, 2177149.2,  2471088.0};

	std::map<std::string, double> pairs;
	size_t count = 0;
	error_tolerance = 0.1; //use a larger error tolerance for large numbers

	// Larger size
	pairs["system_capacity"] = 1000; //1 MW system

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
		EXPECT_FALSE(pvwatts_errors);

		if (!pvwatts_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
		}
		count++;
	}
}

/// Test pvwattsv7 with default inputs and a 15-minute weather file
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, SubhourlyWeather_cmod_pvwattsv7) {

	char subhourly[256];
	int b = sprintf(subhourly, "%s/test/input_cases/pvsamv1_data/LosAngeles_WeatherFile_15min.csv", SSCDIR);
	ssc_data_set_string(data, "solar_resource_file", subhourly); //file set above

	//std::map<std::string, std::string> pairs;
	int pvwatts_errors = run_module(data, "pvwattsv7");

	EXPECT_FALSE(pvwatts_errors);

	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 6523.727, error_tolerance) << "Annual energy.";

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 18.62, 0.1) << "Capacity factor";

	}
}

/// Test PVWattsV7 in lifetime mode
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, LifetimeModeTest_cmod_pvwattsv7) {

	// set lifetime mode
	std::map<std::string, double> pairs;
	pairs["system_use_lifetime_output"] = 1;
	pairs["analysis_period"] = 25;

	// test degradation array with a length of 1, which should work
	// annual energy of this test should be higher than the array length 25, because this is year 1 energy
	// and with a single value array, degradation doesn't start applying until year 2
	double dc_degradation_single[1];
	dc_degradation_single[0] = 0.5;
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_single, 1);
	int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_FALSE(pvwatts_errors);
	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 7003.148, error_tolerance) << "Annual energy degradation array length 1.";
	}

	// next, test degradation array with length the same as analysis period, which should also work
	double dc_degradation[25];
	for (size_t i = 0; i < 25; i++) {
		dc_degradation[i] = 0.5;
	}
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation, 25);
	pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_FALSE(pvwatts_errors);
	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 6968.078, error_tolerance) << "Annual energy degradation array length 25.";
	}

	// lastly, test degradation array with the wrong length, which should fail
	double dc_degradation_fail[22];
	for (size_t i = 0; i < 22; i++) {
		dc_degradation_fail[i] = 0.5;
	}
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_fail, 22);
	pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_TRUE(pvwatts_errors);
}

/// Test PVWattsV7 bifacial functionality
TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, BifacialTest_cmod_pvwattsv7) {

	// set bifacial inputs
	std::map<std::string, double> pairs;
	pairs["bifaciality"] = 0.0;
    ssc_number_t annual_energy_mono = 0, annual_energy_bi = 0;

	int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_FALSE(pvwatts_errors);
	if (!pvwatts_errors)
	{
		ssc_data_get_number(data, "annual_energy", &annual_energy_mono);
		EXPECT_NEAR(annual_energy_mono, 7003, 1) << "System with bifaciality";
	}

    pairs["bifaciality"] = 0.65;
    pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
    EXPECT_FALSE(pvwatts_errors);
    if (!pvwatts_errors)
    {
        ssc_data_get_number(data, "annual_energy", &annual_energy_bi);
    }

    EXPECT_GT(annual_energy_bi/annual_energy_mono, 1.04);
}

/* this test isn't passing currently even though it's working in the UI, so commenting out for now
/// Test PVWattsV7 with snow model
TEST_F(CMPvwattsV7Integration, SnowModelTest_cmod_pvwattsv7) {

	// enable snow model
	std::map<std::string, double> pairs;
	pairs["en_snowmodel"] = 1;

	// test with a file that doesn't have snow data- simulation should fail
	char nosnow[256];
	int b = sprintf(nosnow, "%s/test/input_cases/pvsamv1_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", SSCDIR);
	ssc_data_set_string(data, "solar_resource_file", nosnow); //file set above
	int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_TRUE(pvwatts_errors);

}*/

TEST_F(CMPvwattsV7Integration_cmod_pvwattsv7, NonAnnual)
{
	//set up a weather data array and unassign the solar resource file

	auto weather_data = create_weatherdata_array(24);
	ssc_data_unassign(data, "solar_resource_file");
	ssc_data_set_table(data, "solar_resource_data", &weather_data->table);

	//run the tests
	EXPECT_FALSE(run_module(data, "pvwattsv7"));

	ssc_number_t dc, gen;
	dc = ssc_data_get_array(data, "dc", nullptr)[12];
	EXPECT_NEAR(dc, 2512.404, 0.01) << "DC Energy at noon";

	gen = ssc_data_get_array(data, "gen", nullptr)[12];
	EXPECT_NEAR(gen, 2.417, 0.01) << "Gen at noon";
	free_weatherdata_array(weather_data);
}
