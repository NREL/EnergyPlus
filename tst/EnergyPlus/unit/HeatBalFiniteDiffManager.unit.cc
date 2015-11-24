// EnergyPlus::HeatBalFiniteDiffManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>

using namespace EnergyPlus::HeatBalFiniteDiffManager;

namespace EnergyPlus {

	TEST( HeatBalFiniteDiffManager, CalcNodeHeatFluxTest)
	{

		int const numNodes( 4 );
		int nodeNum( 0 );
		SurfaceFD.allocate( 1 );
		int const SurfNum( 1 );
		SurfaceFD( SurfNum ).QDreport.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).TDpriortimestep.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).TDT.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).CpDelXRhoS1.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).CpDelXRhoS2.allocate( numNodes + 1 );
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux.allocate( 1 );
		DataGlobals::TimeStepZoneSec = 600.0;

		Real64 expectedResult1( 0.0 );
		Real64 expectedResult2( 0.0 );
		Real64 expectedResult3( 0.0 );
		Real64 expectedResult4( 0.0 );
		Real64 expectedResult5( 0.0 );

		// Steady-state case
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum ) = 100.0;
		nodeNum = 1;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult1 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 2;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult2 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 3;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult3 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 4;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult4 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 5;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult5 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		CalcNodeHeatFlux( SurfNum, numNodes );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 1 ), expectedResult1 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 2 ), expectedResult2 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 3 ), expectedResult3 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 4 ), expectedResult4 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 5 ), expectedResult5 , 0.0001 );

		// Reset
		SurfaceFD( SurfNum ).QDreport = 0.0;
		expectedResult1 = 0.0;
		expectedResult2 = 0.0;
		expectedResult3 = 0.0;
		expectedResult4 = 0.0;
		expectedResult5 = 0.0;

		// Unsteady-state case
		DataGlobals::TimeStepZoneSec = 600.0;
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum ) = -200.0;

		nodeNum = 5;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 27.5;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 0.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 0.0;
		expectedResult5 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 4;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 0.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult4 = expectedResult5; // r-layer with zero heat capacity, so flux passes through

		nodeNum = 3;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult3 = expectedResult4; // no change in temperature at nodes 4 and 3, so flux passes through

		nodeNum = 2;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 22.2;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult2 = expectedResult3 + ( SurfaceFD( SurfNum ).TDT( nodeNum ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) ) * SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) / DataGlobals::TimeStepZoneSec;

		nodeNum = 1;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 20.1;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult1 = expectedResult2 + ( SurfaceFD( SurfNum ).TDT( nodeNum + 1 ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum + 1 ) ) * SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum + 1 ) / DataGlobals::TimeStepZoneSec;
		expectedResult1 = expectedResult1 + ( SurfaceFD( SurfNum ).TDT( nodeNum ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) ) * SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) / DataGlobals::TimeStepZoneSec;

		CalcNodeHeatFlux( SurfNum, numNodes );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 1 ), expectedResult1, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 2 ), expectedResult2, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 3 ), expectedResult3, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 4 ), expectedResult4, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 5 ), expectedResult5, 0.0001 );

		SurfaceFD.deallocate( );
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux.deallocate( );

	}

}
