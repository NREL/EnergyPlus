// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/GroundTempsFixture.hh"

using namespace EnergyPlus;

using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( GroundTempsFixture, KusudaAchenbachGroundTempModelTest1 )
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

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
	EXPECT_NEAR( 20.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 15768000 ), 0.01 );	// June 1
	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 31449600 ), 0.01 );	// Dec 30
	EXPECT_NEAR( 15.0, thisModel->getGroundTempAtTimeInSeconds( 100.0, 0.0 ), 0.01 );		// Very deep

	EXPECT_NEAR( 10.15, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.01 );			// January
	EXPECT_NEAR( 19.75, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.01 );			// June

}

TEST_F( GroundTempsFixture, KusudaAchenbachGroundTempModelTest2 )
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

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 16.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
}

TEST_F( GroundTempsFixture, SiteBuildingSurfaceGroundTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:BuildingSurface,",
		"	21.00,	!- January",
		"	22.00,	!- February",
		"	23.00,	!- March",
		"	24.00,	!- April",
		"	25.00,	!- May",
		"	26.00,	!- June",
		"	27.00,	!- July",
		"	28.00,	!- August",
		"	29.00,	!- Septeber",
		"	30.00,	!- October",
		"	31.00,	!- November",
		"	32.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_SiteBuildingSurfaceGroundTemp );

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 21.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.1 );		// January
	EXPECT_NEAR( 32.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.1 );	// December
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.1 );	// Feb of next year

	EXPECT_NEAR( 23.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 6393600 ), 0.1 );	// March 15
	EXPECT_NEAR( 29.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 22291200 ), 0.1 );	// Sept 15
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.1 ); // Feb 15 of next year
}

TEST_F( GroundTempsFixture, SiteShallowGroundTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Shallow,",
		"	21.00,	!- January",
		"	22.00,	!- February",
		"	23.00,	!- March",
		"	24.00,	!- April",
		"	25.00,	!- May",
		"	26.00,	!- June",
		"	27.00,	!- July",
		"	28.00,	!- August",
		"	29.00,	!- Septeber",
		"	30.00,	!- October",
		"	31.00,	!- November",
		"	32.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_SiteShallowGroundTemp );

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 21.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.1 );		// January
	EXPECT_NEAR( 32.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.1 );	// December
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.1 );	// Feb of next year

	EXPECT_NEAR( 23.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 6393600 ), 0.1 );	// March 15
	EXPECT_NEAR( 29.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 22291200 ), 0.1 );	// Sept 15
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.1 ); // Feb 15 of next year
}

TEST_F( GroundTempsFixture, SiteDeepGroundTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Deep,",
		"	21.00,	!- January",
		"	22.00,	!- February",
		"	23.00,	!- March",
		"	24.00,	!- April",
		"	25.00,	!- May",
		"	26.00,	!- June",
		"	27.00,	!- July",
		"	28.00,	!- August",
		"	29.00,	!- Septeber",
		"	30.00,	!- October",
		"	31.00,	!- November",
		"	32.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_SiteDeepGroundTemp );

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 21.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.1 );		// January
	EXPECT_NEAR( 32.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.1 );	// December
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.1 );	// Feb of next year

	EXPECT_NEAR( 23.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 6393600 ), 0.1 );	// March 15
	EXPECT_NEAR( 29.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 22291200 ), 0.1 );	// Sept 15
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.1 ); // Feb 15 of next year
}

TEST_F( GroundTempsFixture, SiteFCFactorMethodGroundTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:FCFactorMethod,",
		"	21.00,	!- January",
		"	22.00,	!- February",
		"	23.00,	!- March",
		"	24.00,	!- April",
		"	25.00,	!- May",
		"	26.00,	!- June",
		"	27.00,	!- July",
		"	28.00,	!- August",
		"	29.00,	!- Septeber",
		"	30.00,	!- October",
		"	31.00,	!- November",
		"	32.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_SiteFCFactorMethodGroundTemp );

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 21.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.1 );		// January
	EXPECT_NEAR( 32.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.1 );	// December
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.1 );	// Feb of next year

	EXPECT_NEAR( 23.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 6393600 ), 0.1 );	// March 15
	EXPECT_NEAR( 29.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 22291200 ), 0.1 );	// Sept 15
	EXPECT_NEAR( 22.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.1 ); // Feb 15 of next year
}