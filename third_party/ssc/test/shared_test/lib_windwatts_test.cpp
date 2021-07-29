#include <gtest/gtest.h>

#include <iostream>
#include <vector>

#include <lib_windwatts.h>
#include <lib_windwakemodel.h>
#include "lib_windwakemodel_test.h"


/**
 * windPowerCalculatorTest requires an initialized windTurbine and wakeModel, and XCoords & YCoords.
 * SetUp() allocates the vectors for input and output variables for windPowerUsingResource. 
 */

class windPowerCalculatorTest : public ::testing::Test{
protected:
	windPowerCalculator wpc;
	windTurbine wt;
	int nTurbines;
	// weather data
	double windSpeedData, windDirData, pressureData, tempData;
	
	 //farm data
	double farmPower, farmPowerGross;
	std::vector<double> power, thrust, eff, windSpeed;
	std::vector<double> turbulenceCoeff, distX, distY, distDownwind, distCrosswind;

public:
	double e = 1000;
	void SetUp(){
		// allocate arrays
		nTurbines = 3;
		distDownwind.resize(nTurbines);
		distCrosswind.resize(nTurbines);
		thrust.resize(nTurbines, 0.47669);
		power.resize(nTurbines, 1190);
		eff.resize(nTurbines, 0);
		windSpeed.resize(nTurbines);
		turbulenceCoeff.resize(nTurbines);
		distX = { 0, 5, 10 };
		distY = { 0, 5, 10 };
		distDownwind.resize(nTurbines);
		distCrosswind.resize(nTurbines);

		// initialize values & classes
		for (int i = 0; i < nTurbines; i++){
			windSpeed[i] = 10.;
		}
		createDefaultTurbine(&wt);

		wpc.nTurbines = nTurbines;
		wpc.turbulenceIntensity = 1.0 / 7.0;
		wpc.windTurb = &wt;
		wpc.XCoords = distX;
		wpc.YCoords = distY;
	}
};

TEST_F(windPowerCalculatorTest, windPowerUsingResource_lib_windwatts){
	// weather inputs
	windSpeedData = 10.;
	windDirData = 180;
	tempData = 25;
	pressureData = 1.0;

	std::shared_ptr<fakeWakeModel> fakeWM(new fakeWakeModel());
	wpc.InitializeModel(fakeWM); 
	int run = wpc.windPowerUsingResource(windSpeedData, windDirData, pressureData, tempData, &farmPower,
                                         &farmPowerGross, &power[0], &thrust[0],
                                         &eff[0], &windSpeed[0], &turbulenceCoeff[0], &distDownwind[0],
                                         &distCrosswind[0]); // runs method we want to test
	EXPECT_EQ(run, 3);
}

TEST_F(windPowerCalculatorTest, windPowerUsingWeibull_lib_windwatts){
	// weather inputs
	windSpeedData = 10.;
	windDirData = 180;
	tempData = 25;
	pressureData = 1.0;
	// weibull data
	double weibullK = 2.;
	double avgSpeed = 7.25;
	double refHeight = 50.;
	std::vector<double> energy(wpc.windTurb->powerCurveArrayLength);

	double energyTotal = wpc.windPowerUsingWeibull(weibullK, avgSpeed, refHeight, &energy[0]); // runs method we want to test
	EXPECT_NEAR(energyTotal, 5639180, e);
}

TEST_F(windPowerCalculatorTest, windPowerUsingDistribution_lib_windwatts){
    // mimic a weibull with k factor 2 and avg speed 7.25 for comparison -> scale param : 8.181
    std::vector<std::vector<double>> dst = {{1.5, 180, .12583},
                                            {5, 180, .3933},
                                            {8, 180, .18276},
                                            {10, 180, .1341},
                                            {13.5, 180, .14217},
                                            {19, 180, .0211}};
    std::shared_ptr<wakeModelBase> wakeModel = std::make_shared<fakeWakeModel>();
    wpc.InitializeModel(wakeModel);
    wpc.windPowerUsingDistribution(dst, &farmPower, &farmPowerGross);
    EXPECT_NEAR(farmPower, 15075000, e);
    EXPECT_NEAR(farmPowerGross, 15075000, e);
}

