// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlus/DataIPShortCuts.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh"
#include "EnergyPlus/GroundTemperatureModeling/XingGroundTemperatureModel.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( EnergyPlusFixture, XingGroundTempsModelTest )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:Xing,",
		"	Test,			!- Name of object",
		"	1.08,			!- Soil Thermal Conductivity {W/m-K}",
		"	962,			!- Soil Density {kg/m3}",
		"	2576,			!- Soil Specific Heat {J/kg-K}",
		"	11.1,			!- Average Soil Surface Tempeature {C}",
		"	13.4,			!- Soil Surface Temperature Amplitude 1 {deltaC}",
		"	0.7,			!- Soil Surface Temperature Amplitude 2 {deltaC}",
		"	25,			!- Phase Shift of Temperature Amplitude 1 {days}",
		"	30;			!- Phase Shift of Temperature Amplitude 2 {days}",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_XingGroundTemp );

	auto thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( -1.43, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );
	EXPECT_NEAR( 2.15, thisModel->getGroundTempAtTimeInSeconds( 0.0, 6393600 ), 0.1 );		// March 15
	EXPECT_NEAR( 19.74, thisModel->getGroundTempAtTimeInSeconds( 0.0, 22291200 ), 0.1 );	// Sept 15
	EXPECT_NEAR( -2.03, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.1 );	// Feb 15 of next year

	EXPECT_NEAR( -2.71, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.1 );		// January
	EXPECT_NEAR( 23.61, thisModel->getGroundTempAtTimeInMonths( 0.0, 7 ), 0.1 );			// July
	EXPECT_NEAR( 1.62, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.1 );		// December
	EXPECT_NEAR( -2.12, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.1 );		// Feb of next year

	EXPECT_NEAR( 11.1, thisModel->getGroundTempAtTimeInMonths( 100.0, 1 ), 0.1 );		// January--deep
}
