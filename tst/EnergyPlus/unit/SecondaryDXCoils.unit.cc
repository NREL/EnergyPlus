// EnergyPlus::DXCoils unit tests
// Secondary DX cooling and heating coil heat rejection and removal rate 
// from a secondary zone and SHR calculation

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <DXCoils.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <Psychrometrics.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataHVACGlobals;
using DataLoopNode::Node;
using DataEnvironment::OutBaroPress;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTwbFnTdbWPb;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::InitializePsychRoutines;

TEST( SecondaryDXCoolingCoilSingleSpeed, Test1 ) {
	// tests secondary DX coil calculation of single speed DX system or heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );

	// cleanup 
	DXCoil.deallocate();
}
TEST( SecondaryDXCoolingCoilTwoSpeed, Test2 ) {

	// tests secondary DX coil calculation of two speed DX cooling system
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingTwoSpeed;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );

	// cleanup 
	DXCoil.deallocate();
}
TEST( SecondaryDXCoolingCoilMultiSpeed, Test3 ) {

	// tests secondary DX coil calculation of multi speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedCooling;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );
	
	// cleanup 
	DXCoil.deallocate();
	
}
TEST( SecondaryDXHeatingCoilSingleSpeed, Test4 ) {
	// tests secondary DX coil calculation of single speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_HeatingEmpirical;
	DXCoil( DXCoilNum ).MinOATCompressor = -5.0;
	DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 5500.0;
	DXCoil( DXCoilNum ).ElecHeatingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;

	DXCoil( DXCoilNum ).SecZoneAirNodeNum = 1;
	Node.allocate( 2 );
	Node( DXCoil( DXCoilNum ).SecZoneAirNodeNum ).Temp = 10.0;
	Node( DXCoil( DXCoilNum ).SecZoneAirNodeNum ).HumRat  = 0.003;
	DXCoil( DXCoilNum ).SecCoilAirFlow = 1.0;
	DXCoil( DXCoilNum ).CompressorPartLoadRatio = 1.0;
	DXCoil( DXCoilNum ).SecCoilRatedSHR = 1.0;

	OutBaroPress = 101325.0;
	DXCoil( DXCoilNum ).AirInNode = 2;
	Node( DXCoil( DXCoilNum ).AirInNode ).Temp = 20.0;
	InitializePsychRoutines();

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( -5000.0, DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate );
	EXPECT_DOUBLE_EQ( 1.0, DXCoil( DXCoilNum ).SecCoilSHR );


	//// set up arguments
	Real64 const EvapAirMassFlow = 1.2;
	Real64 const TotalHeatRemovalRate = 5500.0;
	Real64 const PartLoadRatio = 1.0;
	Real64 const SecCoilRatedSHR = 1.0;
	Real64 const EvapInletDryBulb = 10.0;
	Real64 const EvapInletHumRat = 0.003;
	Real64 const EvapInletWetBulb = 4.5;
	Real64 const EvapInletEnthalpy = 17607.0;
	Real64 const CondInletDryBulb = 20.0;
	Real64 const SecCoilFlowFraction = 1.0;
	int const SecCoilSHRFT = 0;
	int const SecCoilSHRFF = 0;
	
	// output variable
	Real64 SHRTest;
	
	// make the call
	SHRTest = CalcSecondaryDXCoilsSHR( 
	DXCoilNum, 
	EvapAirMassFlow, 
	TotalHeatRemovalRate, 
	PartLoadRatio, 
	SecCoilRatedSHR, 
	EvapInletDryBulb, 
	EvapInletHumRat, 
	EvapInletWetBulb, 
	EvapInletEnthalpy, 
	CondInletDryBulb, 
	SecCoilFlowFraction, 
	SecCoilSHRFT, 
	SecCoilSHRFF );

	EXPECT_DOUBLE_EQ( 1.0, SHRTest );

	// cleanup 
	DXCoil.deallocate();
	Node.deallocate();
}
TEST( SecondaryDXHeatingCoilMultiSpeed, Test5 ) {

	// tests secondary DX coil calculation of multi speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).NumOfSpeeds = 2;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilSHRFT.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilSHRFF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedHeating;
	DXCoil( DXCoilNum ).MinOATCompressor = -5.0;
	DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 5500.0;
	DXCoil( DXCoilNum ).ElecHeatingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;

	DXCoil( DXCoilNum ).SecZoneAirNodeNum = 1;
	Node.allocate( 2 );
	Node( DXCoil( DXCoilNum ).SecZoneAirNodeNum ).Temp = 10.0;
	Node( DXCoil( DXCoilNum ).SecZoneAirNodeNum ).HumRat = 0.003;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow( 1 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow( 2 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFT( 1 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFF( 1 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFT( 2 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFF( 2 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR( 1 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR( 2 ) = 1.0;

	DXCoil( DXCoilNum ).MSSpeedRatio = 0;
	DXCoil( DXCoilNum ).MSCycRatio = 1;
	DXCoil( DXCoilNum ).MSSpeedNumHS = 1;
	DXCoil( DXCoilNum ).MSSpeedNumLS = 1;

	OutBaroPress = 101325.0;
	DXCoil( DXCoilNum ).AirInNode = 2;
	Node( DXCoil( DXCoilNum ).AirInNode ).Temp = 20.0;
	InitializePsychRoutines();

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( -5000.0, DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate );
	EXPECT_DOUBLE_EQ( 1.0, DXCoil( DXCoilNum ).SecCoilSHR );

	// cleanup 
	DXCoil( DXCoilNum ).MSSecCoilAirFlow.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilSHRFT.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilSHRFF.deallocate();
	DXCoil.deallocate();
	Node.deallocate();
}
