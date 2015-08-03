// EnergyPlus::AirflowNetworkSolver unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
//#include <cassert>
//#include <cmath>
//#include <string>

// ObjexxFCL Headers
//#include <ObjexxFCL/Array.functions.hh>
//#include <ObjexxFCL/Fmath.hh>
//#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataAirflowNetwork.hh>
#include <AirflowNetworkBalanceManager.hh>
#include <AirflowNetworkSolver.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace AirflowNetworkBalanceManager;
using namespace DataAirflowNetwork;
using namespace AirflowNetworkSolver;

TEST( AirflowNetworkSolverTest, HorizontalOpening )
{

	ShowMessage( "Begin Test: AirflowNetworkSolverTest, HorizontalOpening" );

	int i = 1;
	int j = 1;
	int n;
	int m;
	int NF;
	Array1D< Real64 > F;
	Array1D< Real64 > DF;

	n = 1;
	m = 2;


	AirflowNetworkCompData.allocate( j );
	AirflowNetworkCompData( j ).TypeNum = 1;
	MultizoneSurfaceData.allocate( i );
	MultizoneSurfaceData( i ).Width = 10.0;
	MultizoneSurfaceData( i ).Height = 5.0;
	MultizoneSurfaceData( i ).OpenFactor = 1.0;

	RHOZ.allocate( 2 );
	RHOZ( 1 ) = 1.2;
	RHOZ( 2 ) = 1.18;

	MultizoneCompHorOpeningData.allocate( 1 );
	MultizoneCompHorOpeningData( 1 ).FlowCoef = 0.1;
	MultizoneCompHorOpeningData( 1 ).FlowExpo = 0.5;
	MultizoneCompHorOpeningData( 1 ).Slope = 90.0;
	MultizoneCompHorOpeningData( 1 ).DischCoeff = 0.2;

	F.allocate( 2 );
	DF.allocate( 2 );

	AirflowNetworkLinkageData.allocate( i );
	AirflowNetworkLinkageData( i ).NodeHeights( 1 ) = 4.0;
	AirflowNetworkLinkageData( i ).NodeHeights( 2 ) = 2.0;

	AFEHOP( 1, 1, 0.05, 1, 1, 2, F, DF, NF );
	EXPECT_NEAR( 3.47863, F( 1 ), 0.00001 );
	EXPECT_NEAR( 34.7863, DF( 1 ), 0.0001 );
	EXPECT_NEAR( 2.96657, F( 2 ), 0.00001 );
	EXPECT_EQ( 0.0, DF( 2 ) );

	AFEHOP( 1, 1, -0.05, 1, 1, 2, F, DF, NF );
	EXPECT_NEAR( -3.42065, F( 1 ), 0.00001 );
	EXPECT_NEAR( 34.20649, DF( 1 ), 0.0001 );
	EXPECT_NEAR( 2.96657, F( 2 ), 0.00001 );
	EXPECT_EQ( 0.0, DF( 2 ) );

	AirflowNetworkLinkageData.deallocate();
	DF.deallocate();
	F.deallocate();
	RHOZ.deallocate();
	MultizoneCompHorOpeningData.deallocate();
	MultizoneSurfaceData.deallocate();
	AirflowNetworkCompData.deallocate();
}


