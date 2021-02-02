#ifndef __WINDWAKE_TEST__
#define __WINDWAKE_TEST__

#include <gtest/gtest.h>

#include <vector>
#include <iosfwd>

#include <lib_physics.h>
#include <lib_windwakemodel.h>

/// Creates a wind turbine with default values and a simple test power curve
static void createDefaultTurbine(windTurbine* wt){
	wt->shearExponent = 0.14;
	wt->measurementHeight = 80;
	wt->hubHeight = 80;
	wt->rotorDiameter = 77;
	std::vector<double>windSpeeds = { 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9, 9.25, 9.5, 9.75, 10, 10.25, 10.5, 10.75, 11, 11.25, 11.5, 11.75, 12, 12.25, 12.5, 12.75, 13, 13.25, 13.5, 13.75, 14, 14.25, 14.5, 14.75, 15, 15.25, 15.5, 15.75, 16, 16.25, 16.5, 16.75, 17, 17.25, 17.5, 17.75, 18, 18.25, 18.5, 18.75, 19, 19.25, 19.5, 19.75, 20, 20.25, 20.5, 20.75, 21, 21.25, 21.5, 21.75, 22, 22.25, 22.5, 22.75, 23, 23.25, 23.5, 23.75, 24, 24.25, 24.5, 24.75, 25, 25.25, 25.5, 25.75, 26, 26.25, 26.5, 26.75, 27, 27.25, 27.5, 27.75, 28, 28.25, 28.5, 28.75, 29, 29.25, 29.5, 29.75, 30, 30.25, 30.5, 30.75, 31, 31.25, 31.5, 31.75, 32, 32.25, 32.5, 32.75, 33, 33.25, 33.5, 33.75, 34, 34.25, 34.5, 34.75, 35, 35.25, 35.5, 35.75, 36, 36.25, 36.5, 36.75, 37, 37.25, 37.5, 37.75, 38, 38.25, 38.5, 38.75, 39, 39.25, 39.5, 39.75, 40 };
	std::vector<double>powerOutput = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21.320, 33.510, 45.690, 65.210, 79.830, 104.25, 128.660, 157.970, 187.270, 216.580, 250.780, 292.320, 333.850, 375.400, 426.720, 475.600, 534.270, 597.810, 656.490, 724.940, 798.290, 871.630, 940.080, 1010, 1060, 1130, 1190, 1240, 1290, 1330, 1370, 1390, 1410, 1430, 1440, 1460, 1470, 1475, 1480, 1485, 1490, 1495, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	wt->setPowerCurve(windSpeeds, powerOutput);
}

/**
* Wind Turbine Class test
*/

class windTurbineTest : public ::testing::Test{
protected:
	windTurbine wt;
	double airDensity;
	double e = 0.01;
public:
	void SetUp(){
		airDensity = 1.22498;
		wt = windTurbine();
	}
};

/**
* fakeWakeModel is assigned during tests to provide a simple wakeModel whose wakeCalculations 
* function does not modify the input parameters.
*/

class fakeWakeModel : public wakeModelBase
{
public:
	fakeWakeModel(){}
	void wakeCalculations(
		const double airDensity, const double distanceDownwind[], const double distanceCrosswind[],
		double power[], double eff[], double thrust[], double windSpeed[], double turbulenceIntensity[]) {};
};


/**
* Simple wake model test requires an initialized windTurbine and test data: thrust, power, eff, windspeed,
* turbulence intensity. These are done in SetUp(). The coordinates of the turbines on the farm are also 
* required, these are assigned in individual tests.
*/

class simpleWakeModelTest : public ::testing::Test{
protected:
	simpleWakeModel swm;
	windTurbine wt;
	int numberTurbines;
	std::vector<double> distDownwind;
	std::vector<double> distCrosswind;
	std::vector<double> thrust;
	std::vector<double> power;
	std::vector<double> eff;
	std::vector<double> windSpeed;
	std::vector<double> turbIntensity;
public:
	double seaLevelAirDensity = physics::AIR_DENSITY_SEA_LEVEL;
	double e = 0.1;
	void SetUp(){
		numberTurbines = 3;
		distDownwind.resize(numberTurbines);
		distCrosswind.resize(numberTurbines);
		thrust.resize(numberTurbines, 0.47669);
		power.resize(numberTurbines, 1190);
		eff.resize(numberTurbines, 0);
		windSpeed.resize(numberTurbines);
		turbIntensity.resize(numberTurbines, 0.1);
		createDefaultTurbine(&wt);
		swm = simpleWakeModel(numberTurbines, &wt);
		for (int i = 0; i < numberTurbines; i++){
			windSpeed[i] = 10.;
		}
	}
};


/**
* Park/WaSp wake Model test requires an initialized windTurbine and test data: thrust, power, eff, windspeed,
* turbulence intensity. These are done in SetUp(). The coordinates of the turbines on the farm are also 
* required, these are assigned in individual tests.
*/

class parkWakeModelTest : public ::testing::Test{
protected:
	parkWakeModel pm;
	windTurbine wt;
	int numberTurbines;
	std::vector<double> distDownwind;
	std::vector<double> distCrosswind;
	std::vector<double> thrust;
	std::vector<double> power;
	std::vector<double> eff;
	std::vector<double> windSpeed;
	std::vector<double> turbIntensity;
public:
	double seaLevelAirDensity = physics::AIR_DENSITY_SEA_LEVEL;
	double e = 0.1;
	void SetUp(){
		numberTurbines = 3;
		distDownwind.resize(numberTurbines);
		distCrosswind.resize(numberTurbines);
		thrust.resize(numberTurbines, 0.47669);
		power.resize(numberTurbines, 1190);
		eff.resize(numberTurbines, 0);
		windSpeed.resize(numberTurbines);
		turbIntensity.resize(numberTurbines, 0.1);
		createDefaultTurbine(&wt);
		pm = parkWakeModel(numberTurbines, &wt);
		for (int i = 0; i < numberTurbines; i++){
			windSpeed[i] = 10.;
		}
	}
};

/**
* Eddy viscosity wake Model test requires a turbulence coefficient, an initialized windTurbine and test data: 
* thrust, power, eff, windspeed, turbulence intensity. These are done in SetUp(). The coordinates of the turbines 
* on the farm are also required, these are assigned in individual tests.
*/

class eddyViscosityWakeModelTest : public ::testing::Test{
protected:
	eddyViscosityWakeModel evm;
	windTurbine wt;
	int numberTurbines;
	double turbCoeff;
	std::vector<double> distDownwind;
	std::vector<double> distCrosswind;
	std::vector<double> thrust;
	std::vector<double> power;
	std::vector<double> eff;
	std::vector<double> windSpeed;
	std::vector<double> turbIntensity;
public:
	double seaLevelAirDensity = physics::AIR_DENSITY_SEA_LEVEL;
	double e = 0.1;
	void SetUp(){
		numberTurbines = 3;
		distDownwind.resize(numberTurbines);
		distCrosswind.resize(numberTurbines);
		thrust.resize(numberTurbines, 0.47669);
		power.resize(numberTurbines, 1190);
		eff.resize(numberTurbines, 0);
		windSpeed.resize(numberTurbines);
		turbIntensity.resize(numberTurbines, 0.1);
		createDefaultTurbine(&wt);
		evm = eddyViscosityWakeModel(numberTurbines, &wt, 0.1);
		for (int i = 0; i < numberTurbines; i++){
			windSpeed[i] = 10.;
		}
	}
};

#endif