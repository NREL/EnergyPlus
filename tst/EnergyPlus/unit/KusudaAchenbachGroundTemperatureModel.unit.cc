// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlus/DataIPShortCuts.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh"
#include "EnergyPlus/GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( EnergyPlusFixture, KusudaAchenbachGroundTempModelTest1 )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	Test,	!- Name of ground temperature object",
		"	1.08,		!- Soil Thermal Conductivity",
		"	980,		!- Soil Density",
		"	2570,		!- Soil Specific Heat",
		"	15.0,		!- Average Surface Temperature",
		"	5.0,		!- Average Amplitude of Surface Temperature",
		"	1;			!- Phase Shift of Minimum Surface Temperature",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_KusudaGroundTemp );

	auto thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
	EXPECT_NEAR( 20.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 15768000 ), 0.01 );	// June 1
	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 31449600 ), 0.01 );	// Dec 30
	EXPECT_NEAR( 15.0, thisModel->getGroundTempAtTimeInSeconds( 100.0, 0.0 ), 0.01 );		// Very deep

	EXPECT_NEAR( 10.15, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.01 );			// January
	EXPECT_NEAR( 19.75, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.01 );			// June

}

TEST_F( EnergyPlusFixture, KusudaAchenbachGroundTempModelTest2 ) // lNumericFieldBlanks not working correctly for this test
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	Test,	!- Name of ground temperature object",
		"	1.08,		!- Soil Thermal Conductivity",
		"	980,		!- Soil Density",
		"	2570,		!- Soil Specific Heat",
		"	,			!- Average Surface Temperature",
		"	,			!- Average Amplitude of Surface Temperature",
		"	;			!- Phase Shift of Minimum Surface Temperature",
		"Site:GroundTemperature:Shallow,",
		"	16.00,	!- January",
		"	15.00,	!- February",
		"	16.00,	!- March",
		"	17.00,	!- April",
		"	18.00,	!- May",
		"	19.00,	!- June",
		"	20.00,	!- July",
		"	21.00,	!- August",
		"	20.00,	!- Septeber",
		"	19.00,	!- October",
		"	18.00,	!- November",
		"	17.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_KusudaGroundTemp );

	auto thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 16.46, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
	EXPECT_NEAR( 17.17, thisModel->getGroundTempAtTimeInSeconds( 0.0, 11664000 ), 0.01 );	// May 15
	EXPECT_NEAR( 20.12, thisModel->getGroundTempAtTimeInSeconds( 0.0, 24883200 ), 0.01 );	// Oct 15
	EXPECT_NEAR( 16.46, thisModel->getGroundTempAtTimeInSeconds( 0.0, 31536000 ), 0.01 );	// Dec 31

	EXPECT_NEAR( 18.0, thisModel->getGroundTempAtTimeInSeconds( 100.0, 24883200 ), 0.01 );	// Oct 15--deep
}
