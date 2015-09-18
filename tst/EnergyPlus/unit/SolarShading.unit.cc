// EnergyPlus::SolarShading Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataVectorTypes.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SolarShading;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataSystemVariables;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataBSDFWindow;
using namespace EnergyPlus::DataVectorTypes;
using namespace ObjexxFCL;

TEST( SolarShadingTest, CalcPerSolarBeamTest )
{
	ShowMessage( "Begin Test: SolarShadingTest, CalcPerSolarBeamTest" );

// Test inits for integrated and non-integrated shading calcs

//	static bool ErrorsFound( false ); // If errors detected in input
//	static int ZoneNum( 0 ); // Zone number
//	int NumAlphas( 2 );
//	int NumNumbers( 9 );
	Real64 AvgEqOfTime( 0.0 ); // Average value of Equation of Time for period
	Real64 AvgSinSolarDeclin( 1.0 ); // Average value of Sine of Solar Declination for period
	Real64 AvgCosSolarDeclin( 0.0 ); // Average value of Cosine of Solar Declination for period
	int NumTimeSteps( 6 );

	TimeStep = 1;
	TotSurfaces = 3;
	MaxBkSurf = 3;
	SurfaceWindow.allocate( TotSurfaces );
	SunlitFracHR.allocate( 24, TotSurfaces );
	SunlitFrac.allocate( NumTimeSteps, 24, TotSurfaces );
	SunlitFracWithoutReveal.allocate( NumTimeSteps, 24, TotSurfaces );
	CTHETA.allocate( TotSurfaces );
	CosIncAngHR.allocate( 24, TotSurfaces );
	CosIncAng.allocate( NumTimeSteps, 24, TotSurfaces );
	AOSurf.allocate( TotSurfaces );
	BackSurfaces.allocate( NumTimeSteps, 24, MaxBkSurf, TotSurfaces );
	OverlapAreas.allocate( NumTimeSteps, 24, MaxBkSurf, TotSurfaces );

// Test non-integrated option first, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 for all hours
	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			SurfaceWindow( SurfNum).OutProjSLFracMult( Hour ) = 999.0;
			SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) = 888.0;
		}
	}

	DetailedSolarTimestepIntegration = false;
	CalcPerSolarBeam( AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );

	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			EXPECT_EQ( 1.0, SurfaceWindow( SurfNum ).OutProjSLFracMult( Hour ) );
			EXPECT_EQ( 1.0, SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) );
		}
	}

	// Test integrated option, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 only for the specified hour
	// Re-initialize to new values
	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			SurfaceWindow( SurfNum ).OutProjSLFracMult( Hour ) = 555.0;
			SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) = 444.0;
		}
	}

	DetailedSolarTimestepIntegration = true;
	HourOfDay = 23;
	CalcPerSolarBeam( AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );

	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			if ( Hour == HourOfDay ) {
				EXPECT_EQ( 1.0, SurfaceWindow( SurfNum ).OutProjSLFracMult( Hour ) );
				EXPECT_EQ( 1.0, SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) );
			} else {
				EXPECT_EQ( 555.0, SurfaceWindow( SurfNum ).OutProjSLFracMult( Hour ) );
				EXPECT_EQ( 444.0, SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) );
			}
		}
	}

	// Clean up
	SurfaceWindow.deallocate();
	SunlitFracHR.deallocate();
	SunlitFrac.deallocate();
	SunlitFracWithoutReveal.deallocate();
	CTHETA.deallocate();
	CosIncAngHR.deallocate();
	CosIncAng.deallocate();
	AOSurf.deallocate();
	BackSurfaces.deallocate();
	OverlapAreas.deallocate();
}

TEST( SolarShadingTest, SurfaceScheduledSolarInc )
{
	ShowMessage("Begin Test: SolarShadingTest, SurfaceScheduledSolarInc");
	int SurfSolIncPtr;
	TotSurfIncSolSSG = 4;
	SurfIncSolSSG.allocate( TotSurfIncSolSSG );
	SurfIncSolSSG( 1 ).SurfPtr = 1;
	SurfIncSolSSG( 1 ).ConstrPtr = 1;
	SurfIncSolSSG( 2 ).SurfPtr = 1;
	SurfIncSolSSG( 2 ).ConstrPtr = 2;
	SurfIncSolSSG( 3 ).SurfPtr = 4;
	SurfIncSolSSG( 3 ).ConstrPtr = 10;
	SurfIncSolSSG( 4 ).SurfPtr = 5;
	SurfIncSolSSG( 4 ).ConstrPtr = 1;

	// Test retrieving pointer for surface incident solar schedule

	SurfSolIncPtr = -99;
	SurfSolIncPtr = SurfaceScheduledSolarInc( 1, 1 );
	EXPECT_EQ( 1, SurfSolIncPtr );

	SurfSolIncPtr = -99;
	SurfSolIncPtr = SurfaceScheduledSolarInc( 1, 2 );
	EXPECT_EQ( 2, SurfSolIncPtr );

	SurfSolIncPtr = -99;
	SurfSolIncPtr = SurfaceScheduledSolarInc( 1, 3 );
	EXPECT_EQ( 0, SurfSolIncPtr );

	SurfSolIncPtr = -99;
	SurfSolIncPtr = SurfaceScheduledSolarInc( 5, 1 );
	EXPECT_EQ( 4, SurfSolIncPtr );

	SurfSolIncPtr = -99;
	SurfSolIncPtr = SurfaceScheduledSolarInc( 5, 10 );
	EXPECT_EQ( 0, SurfSolIncPtr );

	SurfIncSolSSG.deallocate();
}

TEST( SolarShadingTest, polygon_contains_point )
{
	ShowMessage( "Begin Test: SolarShadingTest, polygon_contains_point" );

	unsigned int numSides = 4;
	Array1D< Vector > Rectangle3d;

	Rectangle3d.allocate( numSides );

	Rectangle3d( 1 ).x = 0.;
	Rectangle3d( 1 ).y = 0.;
	Rectangle3d( 1 ).z = 0.;

	Rectangle3d( 2 ).x = 10.;
	Rectangle3d( 2 ).y = 0.;
	Rectangle3d( 2 ).z = 0.;

	Rectangle3d( 3 ).x = 10.;
	Rectangle3d( 3 ).y = 10.;
	Rectangle3d( 3 ).z = 0.;

	Rectangle3d( 4 ).x = 0.;
	Rectangle3d( 4 ).y = 10.;
	Rectangle3d( 4 ).z = 0.;

	Vector PointInside;

	PointInside.x = 5.;
	PointInside.y = 5.;
	PointInside.z = 0.;

	Vector PointOutside;

	PointOutside.x = 20.;
	PointOutside.y = 20.;
	PointOutside.z = 0.;

	EXPECT_TRUE(polygon_contains_point( numSides, Rectangle3d, PointInside, false, false, true ) );
	EXPECT_FALSE( polygon_contains_point( numSides, Rectangle3d, PointOutside, false, false, true ) );

	Rectangle3d( 1 ).x = 0.;
	Rectangle3d( 1 ).y = 0.;
	Rectangle3d( 1 ).z = 0.;

	Rectangle3d( 2 ).x = 10.;
	Rectangle3d( 2 ).y = 0.;
	Rectangle3d( 2 ).z = 0.;

	Rectangle3d( 3 ).x = 10.;
	Rectangle3d( 3 ).y = 0.;
	Rectangle3d( 3 ).z = 10.;

	Rectangle3d( 4 ).x = 0.;
	Rectangle3d( 4 ).y = 0.;
	Rectangle3d( 4 ).z = 10.;

	PointInside.x = 5.;
	PointInside.y = 0.;
	PointInside.z = 5.;

	PointOutside.x = 20.;
	PointOutside.y = 0.;
	PointOutside.z = 20.;

	EXPECT_TRUE( polygon_contains_point( numSides, Rectangle3d, PointInside, false, true, false ) );
	EXPECT_FALSE( polygon_contains_point( numSides, Rectangle3d, PointOutside, false, true, false ) );

	Rectangle3d( 1 ).x = 0.;
	Rectangle3d( 1 ).y = 0.;
	Rectangle3d( 1 ).z = 0.;

	Rectangle3d( 2 ).x = 0.;
	Rectangle3d( 2 ).y = 10.;
	Rectangle3d( 2 ).z = 0.;

	Rectangle3d( 3 ).x = 0.;
	Rectangle3d( 3 ).y = 10.;
	Rectangle3d( 3 ).z = 10.;

	Rectangle3d( 4 ).x = 0.;
	Rectangle3d( 4 ).y = 0.;
	Rectangle3d( 4 ).z = 10.;

	PointInside.x = 0.;
	PointInside.y = 5.;
	PointInside.z = 5.;

	PointOutside.x = 0.;
	PointOutside.y = 20.;
	PointOutside.z = 20.;

	EXPECT_TRUE( polygon_contains_point( numSides, Rectangle3d, PointInside, true, false, false ) );
	EXPECT_FALSE( polygon_contains_point( numSides, Rectangle3d, PointOutside, true, false, false ) );

}
