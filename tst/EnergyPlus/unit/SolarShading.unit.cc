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
