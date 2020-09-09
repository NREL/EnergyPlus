#include "gtest/gtest.h"

#include "cmod_swh_test.h"
#include "input_cases/weather_inputs.h"

TEST_F(CM_SWH, ResidentialDefault_cmod_swh) {

	int swh_errors = run_module(data, "swh");
	ASSERT_EQ(swh_errors, 0);

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 2406.6, 0.1);
	EXPECT_NEAR(annual_energy, 2362.2, 0.1);

}

TEST_F(CM_SWH, ResidentialDefaultUsingData_cmod_swh) {
    auto weather_data = create_weatherdata_array(8760);
    ssc_data_unassign(data, "solar_resource_file");
    ssc_data_set_table(data, "solar_resource_data", &weather_data->table);

    int swh_errors = run_module(data, "swh");
    ASSERT_EQ(swh_errors, 0);

    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, 1229, 1);

    free_weatherdata_array(weather_data);
}
