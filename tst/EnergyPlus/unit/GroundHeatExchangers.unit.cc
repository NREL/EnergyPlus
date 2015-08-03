// EnergyPlus::GroundHeatExchangers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::GroundHeatExchangers;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataLoopNode;


TEST( GroundHeatExchangerTest, KAGroundTemps)
{
	ShowMessage( "Begin Test: GroundHeatExchangerTest, KAGroundTemps" );

	// Initialization
	GLHESlinky thisGLHE;

	Real64 z = 0.0;
	Real64 dayOfYear = 8;
	Real64 aveGroundTemp = 15.5;
	Real64 aveGroundTempAmplitude = 1.0;
	Real64 phaseShiftInDays = 8;

	thisGLHE.diffusivityGround = 4e-007;

	DayOfSim = 8;

	Real64 groundTemp = thisGLHE.getKAGrndTemp( z, dayOfYear, aveGroundTemp, aveGroundTempAmplitude, phaseShiftInDays );

	EXPECT_DOUBLE_EQ( 14.5, groundTemp );
}

TEST( GroundHeatExchangerTest, Interpolate )
{
	ShowMessage( "Begin Test: GroundHeatExchangerTest, Interpolate" );

	// Initialization
	GLHESlinky thisGLHE;
	Real64 thisLNTTS;
	Real64 thisGFunc;

	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;
	
	// Case when extrapolating beyond lower bound
	thisLNTTS = -1.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( -1.0, thisGFunc );

	// Case when extrapolating beyond opper bound
	thisLNTTS = 6.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 6.0 , thisGFunc );

	// Case when we're actually interpolating
	thisLNTTS = 2.5;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );
}

TEST( SlinkyGroundHeatExchangerTest, GetGFunc )
{

	ShowMessage( "Begin Test: SlinkyGroundHeatExchangerTest, GetGFunc" );

	// Initialization
	GLHESlinky thisGLHE;
	Real64 thisGFunc;
	Real64 time;
	
	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;

	time = std::pow( 10.0, 2.5 );

	thisGFunc = thisGLHE.getGFunc( time );

	EXPECT_EQ( 2.5, thisGFunc );
}

TEST( VerticalGroundHeatExchangerTest, GetGFunc )
{

	// Initialization
	GLHEVert thisGLHE;
	Real64 thisGFunc;
	Real64 time;
	
	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;

	time = std::pow( 2.7182818284590452353602874, 2.5 );

	thisGLHE.boreholeLength = 1.0;
	thisGLHE.boreholeRadius = 1.0;

	// Situation when correction is not applied
	thisGLHE.gReferenceRatio = 1.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );

	//Situation when correction is applied
	thisGLHE.gReferenceRatio = 2.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_NEAR( 2.5 + 0.6931, thisGFunc, 0.0001);

}

TEST( SlinkyGroundHeatExchangerTest, CalcHXResistance )
{
	ShowMessage( "Begin Test: SlinkyGroundHeatExchangerTest, CalcHXResistance" );

	// Initializations
	GLHESlinky thisGLHE;

	PlantLoop.allocate( 1 );
	thisGLHE.loopNum = 1;

	PlantLoop( thisGLHE.loopNum ).FluidName = "WATER";
	PlantLoop( thisGLHE.loopNum ).FluidIndex = 1;
	
	thisGLHE.inletTemp = 5.0;
	thisGLHE.massFlowRate = 0.01;
	thisGLHE.numTrenches = 1;
	thisGLHE.pipeOutDia = 0.02667;
	thisGLHE.pipeThick = 0.004;
	thisGLHE.kPipe = 0.4;

	// Re < 2300 mass flow rate
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.13487, thisGLHE.HXResistance, 0.0001 );

	// 4000 > Re > 2300 mass flow rate
	thisGLHE.massFlowRate = 0.07;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.08582, thisGLHE.HXResistance, 0.0001 );

	// Re > 4000 mass flow rate
	thisGLHE.massFlowRate = 0.1;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.077185, thisGLHE.HXResistance, 0.0001 );

	// Zero mass flow rate
	thisGLHE.massFlowRate = 0.0;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.07094, thisGLHE.HXResistance, 0.0001 );
}

TEST( VerticalGroundHeatExchangerTest, CalcHXResistance )
{
	ShowMessage( "Begin Test: VerticalGroundHeatExchangerTest, CalcHXResistance" );

	// Initializations
	GLHEVert thisGLHE;

	PlantLoop.allocate( 1 );
	thisGLHE.loopNum = 1;

	PlantLoop( thisGLHE.loopNum ).FluidName = "WATER";
	PlantLoop( thisGLHE.loopNum ).FluidIndex = 1;
	
	thisGLHE.inletTemp = 5.0;
	thisGLHE.massFlowRate = 0.01;
	thisGLHE.numBoreholes = 1;
	thisGLHE.pipeOutDia = 0.02667;
	thisGLHE.pipeThick = 0.004;
	thisGLHE.kPipe = 0.4;
	thisGLHE.boreholeRadius = 0.1;
	thisGLHE.kGrout = 1.0;

	// Re < 2300 mass flow rate; 0.0 <= distanceRatio <= 2.5 correction factor
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.49421, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.25 < distanceRatio < 0.5 correction factor
	thisGLHE.UtubeDist = 0.05;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.46859, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.5 <= distanceRatio < 0.75 correction factor
	thisGLHE.UtubeDist = 0.087;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.32891, thisGLHE.HXResistance, 0.0001 );

	// 4000 > Re > 2300 mass flow rate; all other distance ratios correction factor
	thisGLHE.UtubeDist = 0.12;
	thisGLHE.massFlowRate = 0.07;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.18391, thisGLHE.HXResistance, 0.0001 );

	// Re > 4000 mass flow rate; all other distance ratios correction factor
	thisGLHE.massFlowRate = 0.1;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.17526, thisGLHE.HXResistance, 0.0001 );

	// Zero mass flow rate; distance ratio > 0.75 correction factor
	thisGLHE.massFlowRate = 0.0;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.16903, thisGLHE.HXResistance, 0.0001 );
}

TEST( SlinkyGroundHeatExchangerTest, CalcGroundHeatExchanger )
{
	ShowMessage( "Begin Test: SlinkyGroundHeatExchangerTest, CalcGroundHeatExchanger" );

	// Initializations
	GLHESlinky thisGLHE;

	thisGLHE.numCoils = 100;
	thisGLHE.numTrenches = 2;
	thisGLHE.maxSimYears = 10;
	thisGLHE.coilPitch = 0.4;
	thisGLHE.coilDepth = 1.5;
	thisGLHE.coilDiameter = 0.8;
	thisGLHE.pipeOutDia = 0.034;
	thisGLHE.trenchSpacing = 3.0;
	thisGLHE.diffusivityGround = 3.0e-007;
	thisGLHE.AGG = 192;
	thisGLHE.SubAGG = 15;

	// Horizontal G-Functions
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 19.08237, thisGLHE.GFNC( 28 ), 0.0001 );

	// Vertical G-Functions
	thisGLHE.verticalConfig = true;
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 18.91819, thisGLHE.GFNC( 28 ), 0.0001 );

}
