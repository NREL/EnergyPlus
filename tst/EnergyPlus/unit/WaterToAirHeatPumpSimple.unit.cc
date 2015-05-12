// EnergyPlus::WaterToAirHeatPumpSimple Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::WaterToAirHeatPumpSimple;
using General::RoundSigDigits;

TEST( WaterToAirHeatPumpSimpleTest, SizeHVACWaterToAir )
{
	// This unit test is intended to check if supply air Humidity ratio used in the cooling sizing calculation is 
	// reset to the minimum of entering mixed air humidity ratio and the user specified supply air design Humidity  
	// ratio such that the total cooling capacity is always greater than or equal to the sensible cooling capacity.
	// This test was added to test bug issue #4893 fix, a defect that resulted in SHR greater than 1.0.

	ShowMessage( "Begin Test: WaterToAirHeatPumpSimpleTest, SizeHVACWaterToAir" );

	int HPNum( 1 );

	SysSizingRunDone = true;
	ZoneSizingRunDone = true;
	CurSysNum = 0;
	CurZoneEqNum = 1;

	SimpleWatertoAirHP.allocate( HPNum );
	FinalZoneSizing.allocate( CurZoneEqNum );
	ZoneEqSizing.allocate( CurZoneEqNum );
	DesDayWeath.allocate( 1 );
	DesDayWeath( 1 ).Temp.allocate( 24 );

	SimpleWatertoAirHP( HPNum ).WatertoAirHPType = "COOLING";
	SimpleWatertoAirHP( HPNum ).RatedAirVolFlowRate = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolSens = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedWaterVolFlowRate = 0.0;

	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0075;
	FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 15;
	FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 25.5;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.0045;
	FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 25.5;
	FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.0045;
	ZoneEqSizing( CurZoneEqNum ).OAVolFlow = 0.0;

	// performance curve coefficients
	SimpleWatertoAirHP( HPNum ).TotalCoolCap1 = -9.149069561;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap2 = 10.878140260;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap3 = -1.718780157;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap4 =  0.746414818;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap5 =  0.0;

	SimpleWatertoAirHP( HPNum ).RatedCOPCool = 5.12;
	SimpleWatertoAirHP( HPNum ).SensCoolCap1 = -5.462690012;
	SimpleWatertoAirHP( HPNum ).SensCoolCap2 = 17.95968138;
	SimpleWatertoAirHP( HPNum ).SensCoolCap3 =-11.87818402;
	SimpleWatertoAirHP( HPNum ).SensCoolCap4 = -0.980163419;
	SimpleWatertoAirHP( HPNum ).SensCoolCap5 =  0.767285761;
	SimpleWatertoAirHP( HPNum ).SensCoolCap6 = 0.0;

	DesDayWeath( 1 ).Temp( 15 ) = 32.0;
	StdBaroPress = 101325.0;
	ZoneEqDXCoil = true;

	InitializePsychRoutines();
	WaterToAirHeatPumpSimple::SizeHVACWaterToAir( HPNum );

	// check that the design oulet air humidity ratio did not change
	EXPECT_DOUBLE_EQ( 0.0075, FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat );

	// check that the total cooling capacity is >= the sensible cooling capacity
	EXPECT_GE( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, SimpleWatertoAirHP( HPNum ).RatedCapCoolSens );

	if ( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal != 0.0 ) {
		ShowMessage( "SizeHVACWaterToAir: Rated Sensible Heat Ratio = " + RoundSigDigits( SimpleWatertoAirHP( HPNum ).RatedCapCoolSens / SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, 2 ) + " [-]" );
	} 

	// clean up
	SimpleWatertoAirHP.deallocate();
	FinalZoneSizing.deallocate();
	ZoneEqSizing.deallocate();
	DesDayWeath( 1 ).Temp.deallocate();
	DesDayWeath.deallocate();
}
