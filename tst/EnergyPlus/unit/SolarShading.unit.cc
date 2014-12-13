// EnergyPlus::SolarShading Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <SolarShading.hh>
#include <DataSurfaces.hh>
#include <DataGlobals.hh>
#include <DataSystemVariables.hh>
#include <DataHeatBalance.hh>
#include <DataBSDFWindow.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SolarShading;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataSystemVariables;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataBSDFWindow;
using namespace ObjexxFCL;

TEST( CalcPerSolarBeamTest, Test1 )
{
// Test inits for integrated and non-integrated shading calcs

//	static bool ErrorsFound( false ); // If errors detected in input
//	static int ZoneNum( 0 ); // Zone number
//	int NumAlphas ( 2 );
//	int NumNumbers ( 9 );
	Real64 AvgEqOfTime ( 0.0 ); // Average value of Equation of Time for period
	Real64 AvgSinSolarDeclin ( 1.0 ); // Average value of Sine of Solar Declination for period
	Real64 AvgCosSolarDeclin ( 0.0 ); // Average value of Cosine of Solar Declination for period
	int NumTimeSteps( 6 );

	TimeStep = 1;
	TotSurfaces = 3;
	MaxBkSurf = 3;
	SurfaceWindow.allocate( TotSurfaces );
	SunlitFracHR.allocate( TotSurfaces, 24 );
	SunlitFrac.allocate( TotSurfaces, 24, NumTimeSteps );
	SunlitFracWithoutReveal.allocate( TotSurfaces, 24, NumTimeSteps );
	CTHETA.allocate( TotSurfaces );
	CosIncAngHR.allocate( TotSurfaces, 24 );
	CosIncAng.allocate( TotSurfaces, 24, NumTimeSteps );
	AOSurf.allocate( TotSurfaces );
	BackSurfaces.allocate( TotSurfaces, MaxBkSurf, 24, NumTimeSteps );
	OverlapAreas.allocate( TotSurfaces, MaxBkSurf, 24, NumTimeSteps );

// Test non-integrated option first, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 for all hours
	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			SurfaceWindow( SurfNum).OutProjSLFracMult( Hour ) = 999.0;
			SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) = 888.0;
		}
	}

	DetailedSolarTimestepIntegration = false;
	CalcPerSolarBeam( AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );

	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 24 ) );

	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 24 ) );

	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 24 ) );

	// Test integrated option, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 only for the specified hour
	// Re-initialize first
	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		for ( int Hour = 1; Hour <= 24; ++Hour ) {
			SurfaceWindow( SurfNum ).OutProjSLFracMult( Hour ) = 999.0;
			SurfaceWindow( SurfNum ).InOutProjSLFracMult( Hour ) = 888.0;
		}
	}

	DetailedSolarTimestepIntegration = true;
	HourOfDay = 23;
	CalcPerSolarBeam( AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );

	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 1 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 2 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 1 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 2 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 3 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 4 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 5 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 6 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 7 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 8 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 9 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 10 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 11 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 12 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 13 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 14 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 15 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 16 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 17 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 18 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 19 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 20 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 21 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).OutProjSLFracMult( 23 ) );
	EXPECT_EQ( 999.0, SurfaceWindow( 3 ).OutProjSLFracMult( 24 ) );

	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 1 ).InOutProjSLFracMult( 24 ) );

	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 2 ).InOutProjSLFracMult( 24 ) );

	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 1 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 2 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 3 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 4 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 5 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 6 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 7 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 8 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 9 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 10 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 11 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 12 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 13 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 14 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 15 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 16 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 17 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 18 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 19 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 20 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 21 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 22 ) );
	EXPECT_EQ( 1.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 23 ) );
	EXPECT_EQ( 888.0, SurfaceWindow( 3 ).InOutProjSLFracMult( 24 ) );

}
