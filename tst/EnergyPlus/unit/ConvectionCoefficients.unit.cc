// EnergyPlus::ConvectionCoefficients unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers

// EnergyPlus Headers
#include <ConvectionCoefficients.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace ConvectionCoefficients;

TEST( ConvectionCoefficientsTest, ConvectionCofficients )
{

	ShowMessage( "Begin Test: ConvectionCoefficientsTest, ConvectionCofficients" );

	Real64 DeltaTemp; // [C] temperature difference between surface and air
	Real64 Height; // [m] characteristic size
	Real64 SurfTemp; // [C] surface temperature
	Real64 SupplyAirTemp; // [C] temperature of supply air into zone
	Real64 AirChangeRate; // [ACH] [1/hour] supply air ACH for zone
	int ZoneNum; // index of zone for messaging
	Real64 Hc;

	DeltaTemp = 1.0; 
	Height = 2.0; 
	SurfTemp = 23.0; 
	SupplyAirTemp = 35.0;
	AirChangeRate = 2.0;
	ZoneNum = 1;

	Hc = CalcBeausoleilMorrisonMixedAssistedWall( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum ); 
	EXPECT_NEAR( -1.19516, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedOpposingWall( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( 1.8378, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedStableFloor( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -4.3290, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedUnstableFloor( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -4.24778, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedStableCeiling( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -8.11959, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedUnstableCeiling( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -8.09685, Hc, 0.0001 );

}


