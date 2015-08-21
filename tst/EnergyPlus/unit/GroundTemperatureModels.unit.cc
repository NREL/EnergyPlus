// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/GroundTemperatureModeling/FiniteDifferenceGroundTemperatureModel.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh>
#include <EnergyPlus/GroundTemperatureModeling/SiteBuildingSurfaceGroundTemperatures.hh>
#include <EnergyPlus/GroundTemperatureModeling/SiteDeepGroundTemperatures.hh>
#include <EnergyPlus/GroundTemperatureModeling/SiteFCFactorMethodGroundTemperatures.hh>
#include <EnergyPlus/GroundTemperatureModeling/SiteShallowGroundTemperatures.hh>
#include <EnergyPlus/GroundTemperatureModeling/XingGroundTemperatureModel.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/GroundTempsFixture.hh"

using namespace EnergyPlus;

using namespace EnergyPlus::GroundTemperatureManager;

Array1D_string const CurrentObjects( 7, {"Site:GroundTemperature:Undisturbed:KusudaAchenbach",});
 
TEST_F( GroundTempsFixture, KusudaAchenbachGroundTempModelTest )
{
	
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	KA-Test,	!- Name of ground temperature object",
		"	1.08,		!- Soil Thermal Conductivity",
		"	980,		!- Soil Density",
		"	2570,		!- Soil Specific Heat",
		"	15.0,		!- Average Surface Temperature",
		"	5.0,		!- Average Amplitude of Surface Temperature",
		"	1;			!- Phase Shift of Minimum Surface Temperature",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	bool ErrorsFound( false ); // If errors detected in input
	int ControllerNum( 0 ); // Controller number
	int NumArg( 0 );
	int NumNums( 0 );
	int NumAlphas( 0 );
	int IOStat( 0 );
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_KusudaGroundTemp );

	auto & thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.0001 );		// Jan 1
	EXPECT_NEAR( 20.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 15768000 ), 0.0001 );	// June 1
	EXPECT_NEAR( 15.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 100.0 ), 0.0001 );		// Jan 1

	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.0001 );			// January
	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.0001 );			// June

}



//TEST(GroundTempsModelKusudaAchenbachTest, getGroundTemp)
//{
//	ShowMessage( "Begin Test: GroundTempsModelKusudaAchenbachTest, getGroundTemp" );
//
//	// Initialization
//	KusudaGroundTempsModel thisGroundTempsModel;
//	Real64 thisGFunc;
//	Real64 z = 0; // Depth
//	Real64 diffusivityGround = 4.0e-7; // Ground props
//	Real64 simTimeInSeconds = 0; // Simulation time
//
//	thisGroundTempsModel.aveGroundTemp = 15;
//	thisGroundTempsModel.aveGroundTempAmplitude = 5;
//	thisGroundTempsModel.phaseShiftInSecs = 0;
//	
//	//thisGFunc = thisGroundTempsModel.getGroundTemp( z, diffusivityGround, simTimeInSeconds );
//	//EXPECT_DOUBLE_EQ( 10.0, thisGFunc );
//
//}
//
//TEST( GroundTempsModelFiniteDiff, developMesh )
//{
//	FiniteDiffGroundTempsModel thisGTM;
//
//	thisGTM.developMesh();
//}
