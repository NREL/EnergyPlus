// EnergyPlus::HeatBalanceIntRadExchange unit test

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
#include <DataGlobals.hh>
#include <DataSurfaces.hh>
#include <DataHeatBalance.hh>
#include <DataViewFactorInformation.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DataGlobals;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataViewFactorInformation;
using namespace HeatBalanceIntRadExchange;

TEST_F( EnergyPlusFixture, HeatBalanceIntRadExchangeTest_1ZoneUncontrolled )
{
	// HeatBalanceIntRadExchange, Shoebox, no windows

	Array1D< Real64 > SurfaceTemp;
	Array1D< Real64 > NetLWRadToSurf;

	NumOfZones = 1;
	TotSurfaces = 6;

	Construct.allocate( 1 );

	ConstructionData & cd1( Construct( 1 ) );
	cd1.InsideAbsorpThermal = 0.9;
	cd1.TypeIsWindow = false;
	cd1.WindowTypeEQL = false;

	Surface.allocate( TotSurfaces );

	SurfaceData & sd1( Surface( 1 ) );
	sd1.HeatTransSurf = true;
	sd1.Construction = 1;
	sd1.Vertex.allocate( 4 );
	sd1.Vertex = { Vector( 0, 0, 4.572 ), Vector( 0, 0, 0 ), Vector( 15.24, 0, 0 ), Vector( 15.24, 0, 4.572 ) };
	sd1.Area = 69.6772;
	sd1.Azimuth = 180;
	sd1.Tilt = 90;

	SurfaceData & sd2( Surface( 2 ) );
	sd2.HeatTransSurf = true;
	sd2.Construction = 1;
	sd2.Vertex.allocate( 4 );
	sd2.Vertex = { Vector( 15.24, 0, 4.572 ), Vector( 15.24, 0, 0 ), Vector( 15.24, 15.24, 0 ), Vector( 15.24, 15.24, 4.572 ) };
	sd2.Area = 69.6772;
	sd2.Azimuth = 90;
	sd2.Tilt = 90;

	SurfaceData & sd3( Surface( 3 ) );
	sd3.HeatTransSurf = true;
	sd3.Construction = 1;
	sd3.Vertex.allocate( 4 );
	sd3.Vertex = { Vector( 15.24, 15.24, 4.572 ), Vector( 15.24, 15.24, 0 ), Vector( 0, 15.24, 0 ), Vector( 0, 15.24, 4.572 ) };
	sd3.Area = 69.6772;
	sd3.Azimuth = 0;
	sd3.Tilt = 90;

	SurfaceData & sd4( Surface( 4 ) );
	sd4.HeatTransSurf = true;
	sd4.Construction = 1;
	sd4.Vertex.allocate( 4 );
	sd4.Vertex = { Vector( 0, 15.24, 4.572 ), Vector( 0, 15.24, 0 ), Vector( 0, 0, 0 ), Vector( 0, 0, 4.572 ) };
	sd4.Area = 69.6772;
	sd4.Azimuth = 270;
	sd4.Tilt = 90;

	SurfaceData & sd5( Surface( 5 ) );
	sd5.HeatTransSurf = true;
	sd5.Construction = 1;
	sd5.Vertex.allocate( 4 );
	sd5.Vertex = { Vector( 15.24, 0, 0 ), Vector( 0, 0, 0 ), Vector( 0, 15.24, 0 ), Vector( 15.24, 15.24, 0 ) };
	sd5.Area = 232.2576;
	sd5.Azimuth = 90;
	sd5.Tilt = 180;

	SurfaceData & sd6( Surface( 6 ) );
	sd6.HeatTransSurf = true;
	sd6.Construction = 1;
	sd6.Vertex.allocate( 4 );
	sd6.Vertex = { Vector( 0, 15.24, 4.572 ), Vector( 0, 0, 4.572 ), Vector( 15.24, 0, 4.572 ), Vector( 15.24, 15.24, 4.572 ) };
	sd6.Area = 232.2576;
	sd6.Azimuth = 180;
	sd6.Tilt = 0;

	SurfaceWindow.allocate( TotSurfaces );

	Zone.allocate( NumOfZones );
	ZoneData & zd( Zone( 1 ) );
	zd.Name = "ZONE ONE";
	zd.SurfaceFirst = 1;
	zd.SurfaceLast = 6;
		
	ZoneInfo.allocate( NumOfZones );

	SurfaceTemp.allocate( TotSurfaces );
	NetLWRadToSurf.allocate( TotSurfaces );
	
	SurfaceTemp( 1 ) = SurfaceTemp( 2 ) = 17.9554; 
	SurfaceTemp( 3 ) = SurfaceTemp( 4 ) = 17.9560;
	SurfaceTemp( 5 ) = 22.1924; 
	SurfaceTemp( 6 ) = 19.1827;

	NetLWRadToSurf( 1 ) = NetLWRadToSurf( 2 ) = NetLWRadToSurf( 3 ) = NetLWRadToSurf( 4 ) = NetLWRadToSurf( 5 ) = NetLWRadToSurf( 6 ) = 0.0;

	KickOffSimulation = false;
	
	BeginEnvrnFlag = true;

	CalcInteriorRadExchange(SurfaceTemp, 0, NetLWRadToSurf);

	EXPECT_NEAR( 10.4801, NetLWRadToSurf( 1 ), 0.0005 );
	EXPECT_NEAR( 10.4801, NetLWRadToSurf( 2 ), 0.0005 );
	EXPECT_NEAR( 10.4765, NetLWRadToSurf( 3 ), 0.0005 );
	EXPECT_NEAR( 10.4765, NetLWRadToSurf( 4 ), 0.0005 );
	EXPECT_NEAR( -17.6784, NetLWRadToSurf( 5 ), 0.0005 );
	EXPECT_NEAR( 5.1045, NetLWRadToSurf( 6 ), 0.0005 );
	
	NetLWRadToSurf.deallocate();
	SurfaceTemp.deallocate();

	ZoneInfo.deallocate();
	Zone.deallocate();
	SurfaceWindow.deallocate();
	Surface.deallocate();
	Construct.deallocate();

	// Amir Roth 2015-07-02: TODO: Expand to include second model with window
}
