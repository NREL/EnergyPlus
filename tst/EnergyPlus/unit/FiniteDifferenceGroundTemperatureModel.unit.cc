// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlus/DataGlobals.hh"
#include "EnergyPlus/DataIPShortCuts.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/GroundTemperatureModeling/FiniteDifferenceGroundTemperatureModel.hh"
#include "EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh"
#include "EnergyPlus/WeatherManager.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( EnergyPlusFixture, FiniteDiffGroundTempModelTest )
{

	using DataGlobals::Pi;
	using WeatherManager::NumDaysInYear;
	using namespace DataIPShortCuts;

	std::shared_ptr< FiniteDiffGroundTempsModel > thisModel( new FiniteDiffGroundTempsModel() );

	thisModel->objectType = objectType_FiniteDiffGroundTemp;
	thisModel->objectName = "Test";
	thisModel->baseConductivity = 1.08;
	thisModel->baseDensity = 962.0;
	thisModel->baseSpecificHeat = 2576.0;
	thisModel->waterContent = 30.0 / 100.0;
	thisModel->saturatedWaterContent = 50.0 / 100.0;
	thisModel->evapotransCoeff = 0.408;

	EXPECT_NEAR( 2.0, thisModel->interpolate( 2.0, 3.0, 1.0, 3.0, 1.0 ), 0.0000001 );

	thisModel->developMesh();

	// Setting weather data manually here
	thisModel->weatherDataArray.dimension( NumDaysInYear );

	Real64 drybulb_minTemp = 5;
	Real64 drybulb_amp = 10;
	Real64 relHum_const = 0.5;
	Real64 windSpeed_const = 3.0;
	Real64 solar_min = 100;
	Real64 solar_amp = 100;

	for ( int day = 1; day <= NumDaysInYear; ++day ) {
		auto & tdwd = thisModel->weatherDataArray( day ); // "This day weather data"

		Real64 theta = 2 * Pi * day / NumDaysInYear;
		Real64 omega = 2 * Pi * 130 / NumDaysInYear; // Shifts min to around the end of Jan

		tdwd.dryBulbTemp = drybulb_amp * std::sin( theta - omega ) + ( drybulb_minTemp + drybulb_amp );
		tdwd.relativeHumidity = relHum_const;
		tdwd.windSpeed = windSpeed_const;
		tdwd.horizontalRadiation = solar_amp * std::sin( theta - omega ) + ( solar_min + solar_amp );;
		tdwd.airDensity = 1.2;
	}

	thisModel->annualAveAirTemp = 15.0;
	thisModel->maxDailyAirTemp = 25.0;
	thisModel->minDailyAirTemp = 5.0;
	thisModel->dayOfMinDailyAirTemp = 30;

	thisModel->performSimulation();

	EXPECT_NEAR( 4.51, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.01 );
	EXPECT_NEAR( 19.14, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.01 );
	EXPECT_NEAR( 7.96, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.01 );
	EXPECT_NEAR( 3.46, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.01 );

	EXPECT_NEAR( 14.36, thisModel->getGroundTempAtTimeInMonths( 3.0, 1 ), 0.01 );
	EXPECT_NEAR( 11.78, thisModel->getGroundTempAtTimeInMonths( 3.0, 6 ), 0.01 );
	EXPECT_NEAR( 15.57, thisModel->getGroundTempAtTimeInMonths( 3.0, 12 ), 0.01 );

	EXPECT_NEAR( 14.58, thisModel->getGroundTempAtTimeInMonths( 25.0, 1 ), 0.01 );
	EXPECT_NEAR( 14.55, thisModel->getGroundTempAtTimeInMonths( 25.0, 6 ), 0.01 );
	EXPECT_NEAR( 14.53, thisModel->getGroundTempAtTimeInMonths( 25.0, 12 ), 0.01 );

	EXPECT_NEAR( 5.04, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 ); 
	EXPECT_NEAR( 19.28, thisModel->getGroundTempAtTimeInSeconds( 0.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 7.32, thisModel->getGroundTempAtTimeInSeconds( 0.0, 30153600), 0.01 );
	EXPECT_NEAR( 3.53, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.01 );

	EXPECT_NEAR( 14.36, thisModel->getGroundTempAtTimeInSeconds( 3.0, 1296000 ), 0.01 );
	EXPECT_NEAR( 11.80, thisModel->getGroundTempAtTimeInSeconds( 3.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 15.46, thisModel->getGroundTempAtTimeInSeconds( 3.0, 30153600 ), 0.01 );

	EXPECT_NEAR( 14.52, thisModel->getGroundTempAtTimeInSeconds( 25.0, 0.0 ), 0.01 );
	EXPECT_NEAR( 14.55, thisModel->getGroundTempAtTimeInSeconds( 25.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 14.52, thisModel->getGroundTempAtTimeInSeconds( 25.0, 30153600 ), 0.01 );
}
