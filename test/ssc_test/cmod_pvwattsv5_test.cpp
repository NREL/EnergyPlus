#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "vartab.h"
#include "../ssc/common.h"

#include "input_cases/weather_inputs.h"
#include "cmod_pvwattsv5_test.h"

///Default PVWattsV5, but with TMY2 instead of TMY3
TEST_F(CMPvwattsV5Integration_cmod_pvwattsv5, DefaultNoFinancialModel){
	compute();

	double tmp=0;
//	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";
	int count;
	ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);

	for (size_t i = 0; i < 12; i++)
		tmp += (double)monthly_energy[i];
	EXPECT_NEAR(tmp, 6909.79, error_tolerance) << "Annual energy.";


	EXPECT_NEAR((double)monthly_energy[0], 435.384, error_tolerance) << "Monthly energy of January";
	EXPECT_NEAR((double)monthly_energy[1], 482.864, error_tolerance) << "Monthly energy of February";
	EXPECT_NEAR((double)monthly_energy[2], 593.982, error_tolerance) << "Monthly energy of March";
	EXPECT_NEAR((double)monthly_energy[3], 673.599, error_tolerance) << "Monthly energy of April";
	EXPECT_NEAR((double)monthly_energy[4], 715.839, error_tolerance) << "Monthly energy of May";
	EXPECT_NEAR((double)monthly_energy[5], 665.064, error_tolerance) << "Monthly energy of June";
	EXPECT_NEAR((double)monthly_energy[6], 665.71, error_tolerance) << "Monthly energy of July";
	EXPECT_NEAR((double)monthly_energy[7], 647.677, error_tolerance) << "Monthly energy of August";
	EXPECT_NEAR((double)monthly_energy[8], 594.505, error_tolerance) << "Monthly energy of September";
	EXPECT_NEAR((double)monthly_energy[9], 568.489, error_tolerance) << "Monthly energy of October";
	EXPECT_NEAR((double)monthly_energy[10], 453.529, error_tolerance) << "Monthly energy of November";
	EXPECT_NEAR((double)monthly_energy[11], 413.149, error_tolerance) << "Month energy of December";

	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 19.7197, error_tolerance) << "Capacity factor";

}

TEST_F(CMPvwattsV5Integration_cmod_pvwattsv5, UsingData){
    auto weather_data = create_weatherdata_array(8760);
    ssc_data_unassign(data, "solar_resource_file");
    ssc_data_set_table(data, "solar_resource_data", &weather_data->table);
    compute();
    //delete weather_data;

    ssc_number_t capacity_factor;
    ssc_data_get_number(data, "capacity_factor", &capacity_factor);
    EXPECT_NEAR(capacity_factor, 11.7368, error_tolerance) << "Capacity factor";

    free_weatherdata_array(weather_data);
}

/// PVWattsV5 using different technology input options
TEST_F(CMPvwattsV5Integration_cmod_pvwattsv5, DifferentTechnologyInputs)
{
//	std::vector<double> annual_energy_expected = { 6909.79, 7123.32, 7336.478, 6909.79, 6804.376, 8711.946, 8727.704, 9690.735 };
	// single axis tracking reduction due to pull request 280
	std::vector<double> annual_energy_expected = { 6909.79, 7123.32, 7336.478, 6909.79, 6804.376, 8601.011, 8727.704, 9690.735 };
	std::map<std::string, double> pairs;
	size_t count = 0;

	// Module types: Standard, Premium, Thin Film
	for (int module_type = 0; module_type < 3; module_type++)
	{
			pairs["module_type"] = module_type;
			int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv5", pairs);
			EXPECT_FALSE(pvwatts_errors);

			if (!pvwatts_errors)
			{
				ssc_number_t annual_energy;
				ssc_data_get_number(data, "annual_energy", &annual_energy);
				EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
			}
			count++;
	}

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{
		pairs["module_type"] = 0; //reset module type to its default value
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv5", pairs);
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

/// PVWattsV5 using a larger system size
TEST_F(CMPvwattsV5Integration_cmod_pvwattsv5, LargeSystem_cmod_pvwattsv5)
{
	std::vector<double> annual_energy_expected = { 1727447.4, 1701094.0, 2150252.8, 2181925.8, 2422683.7 };
	std::map<std::string, double> pairs;
	size_t count = 0;
	error_tolerance = 0.1; //use a larger error tolerance for large numbers

	// Larger size
	pairs["system_capacity"] = 1000; //1 MW system

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv5", pairs);
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


TEST_F(CMPvwattsV5Integration_cmod_pvwattsv5, singleTS) {
    auto data = ssc_data_create();
    ssc_data_set_number(data, "alb", .2);
    ssc_data_set_number(data, "beam", 0.612);
    ssc_data_set_number(data, "day", 6);
    ssc_data_set_number(data, "diffuse", 162.91);
    ssc_data_set_number(data, "hour", 13);
    ssc_data_set_number(data, "lat", 39.744);
    ssc_data_set_number(data, "lon", -105.1778);
    ssc_data_set_number(data, "minute", 20);
    ssc_data_set_number(data, "month", 1);
    ssc_data_set_number(data, "tamb", 10.79);
    ssc_data_set_number(data, "tz", -7);
    ssc_data_set_number(data, "wspd", 1.4500);
    ssc_data_set_number(data, "year", 2019);

    ssc_data_set_number(data, "array_type", 2);
    ssc_data_set_number(data, "azimuth", 180);
    ssc_data_set_number(data, "dc_ac_ratio", 1.2);
    ssc_data_set_number(data, "gcr", 0.4);
    ssc_data_set_number(data, "inv_eff", 96);
    ssc_data_set_number(data, "losses", 0);
    ssc_data_set_number(data, "module_type", 0);
    ssc_data_set_number(data, "system_capacity", 720);
    ssc_data_set_number(data, "tilt", 0);

    auto mod = ssc_module_create("pvwattsv5_1ts");

    // without previous tcell & poa
    EXPECT_TRUE(ssc_module_exec(mod, data));

    double val;
    ssc_data_get_number(data, "poa", &val);
    EXPECT_NEAR(val, 140.21, .1);
    ssc_data_get_number(data, "tcell", &val);
    EXPECT_NEAR(val, 12.77, .1);
    ssc_data_get_number(data, "dc", &val);
    EXPECT_NEAR(val, 106739, 1);
    ssc_data_get_number(data, "ac", &val);
    EXPECT_NEAR(val, 100851, 1);

    EXPECT_TRUE(ssc_module_exec(mod, data));

    // tcell & poa are assigned from above exec call
    val;
    ssc_data_get_number(data, "poa", &val);
    EXPECT_NEAR(val, 140.21, .1);
    ssc_data_get_number(data, "tcell", &val);
    EXPECT_NEAR(val, 13.36, .1);
    ssc_data_get_number(data, "dc", &val);
    EXPECT_NEAR(val, 106459, 1);
    ssc_data_get_number(data, "ac", &val);
    EXPECT_NEAR(val, 100579, 1);
}
