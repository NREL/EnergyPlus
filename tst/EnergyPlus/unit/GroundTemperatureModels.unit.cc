// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/GroundTempsManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/GroundTempsFixture.hh"

using namespace EnergyPlus;

using namespace EnergyPlus::GroundTemps;

 
TEST_F( GroundTempsFixture, getGroundTemp )
{
	
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
