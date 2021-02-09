#include <gtest/gtest.h>

#include <iostream>
#include <vector>

#include "core.h"
#include <lib_windfile.h>
#include "cmod_windpower.h"
#include "../input_cases/weather_inputs.h"

/**
 * Tests windfile's interpolation of measurement height's pres, tmp, speed, & dir data points to required hub height.
 */

class windDataProviderCalculatorTest : public ::testing::Test {
protected:
	winddata_provider* windDataProvider;

public: 
	double e = .1;
	void SetUp() {}
	void TearDown() {
		if (windDataProvider) delete windDataProvider;
	}
};

TEST_F(windDataProviderCalculatorTest, FindClosestUsingData_lib_windfile_test) {
	// measurement heights: 80, 90
	var_data* windresourcedata = create_winddata_array(1,2);
	windDataProvider = new winddata(windresourcedata);

	//// Case 1: hubheight: 85, can interpolate
	double pres, temp, spd, dir, heightOfClosestMeasuredSpd, heightOfClosestMeasuredDir;
	windDataProvider->read(85, &spd, &dir, &temp, &pres, &heightOfClosestMeasuredSpd, &heightOfClosestMeasuredDir, true);
	EXPECT_NEAR(pres, 0.975, e) << "case 1: hub height can be interpolated.";
	EXPECT_NEAR(temp, 52.5, e) << "case 1: hub height can be interpolated.";
	EXPECT_NEAR(spd, 2.5, e) << "case 1: hub height can be interpolated.";
	EXPECT_NEAR(dir, 190, e) << "case 1: hub height can be interpolated.";
	EXPECT_NEAR(heightOfClosestMeasuredSpd, 85, e) << "case 1: hub height can be interpolated.";

	//// Case 2: hubheight: 95, cannot interpolate, gives closest
	windDataProvider->read(95, &spd, &dir, &temp, &pres, &heightOfClosestMeasuredSpd, &heightOfClosestMeasuredDir, true);
	EXPECT_NEAR(pres, 1.0, e) << "case 2";
	EXPECT_NEAR(temp, 55, e) << "case 2";
	EXPECT_NEAR(spd, 5, e) << "case 2";
	EXPECT_NEAR(dir, 200, e) << "case 2";
	EXPECT_NEAR(heightOfClosestMeasuredSpd, 90, e) << "case 2";
}