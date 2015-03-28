// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::Furnaces;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataZoneEnergyDemands;
using namespace DataGlobals;
using namespace ScheduleManager;


TEST( SetVSHPAirFlowTest, VSFurnaceFlowTest )
{
	ShowMessage( "Begin Test: SetVSHPAirFlowTest, VSFurnaceFlowTest" );

	int FurnaceNum( 1 );
	Real64 OnOffAirFlowRatio; // This is a return value
	Real64 PartLoadRatio( 1.0 );
	Node.allocate( 2 );
	CurDeadBandOrSetback.allocate( 1 );
	Schedule.allocate( 1 );

	MSHPMassFlowRateLow = 0.0;
	MSHPMassFlowRateHigh = 0.0;

	Furnace.allocate( 1 );

	Furnace( FurnaceNum ).FurnaceType_Num = UnitarySys_HeatCool;

	Furnace( FurnaceNum ).FurnaceInletNodeNum = 1;
	Furnace( FurnaceNum ).FurnaceOutletNodeNum = 2;
	Furnace( FurnaceNum ).ControlZoneNum = 1;

	Furnace( FurnaceNum ).MaxHeatAirMassFlow = 0.5;
	Furnace( FurnaceNum ).MaxCoolAirMassFlow = 0.75;

	Furnace( FurnaceNum ).HeatMassFlowRate.allocate( 3 );
	Furnace( FurnaceNum ).CoolMassFlowRate.allocate( 3 );
	Furnace( FurnaceNum ).MSHeatingSpeedRatio.allocate( 3 );
	Furnace( FurnaceNum ).MSCoolingSpeedRatio.allocate( 3 );

	Furnace( FurnaceNum ).LastMode = HeatingMode;
	Furnace( FurnaceNum ).IdleMassFlowRate = 0.2;
	Furnace( FurnaceNum ).IdleSpeedRatio = 0.2;
	Furnace( FurnaceNum ).FanAvailSchedPtr = ScheduleAlwaysOn;
	Furnace( FurnaceNum ).FurnaceInletNodeNum = 1;

	Furnace( FurnaceNum ).HeatMassFlowRate( 1 ) = 0.25;
	Furnace( FurnaceNum ).MSHeatingSpeedRatio( 1 ) = 0.25;
	Furnace( FurnaceNum ).HeatMassFlowRate( 2 ) = 0.5;
	Furnace( FurnaceNum ).MSHeatingSpeedRatio( 2 ) = 0.5;
	Furnace( FurnaceNum ).HeatMassFlowRate( 3 ) = 1.0;
	Furnace( FurnaceNum ).MSHeatingSpeedRatio( 3 ) = 1.0;

	Furnace( FurnaceNum ).CoolMassFlowRate( 1 ) = 0.3;
	Furnace( FurnaceNum ).MSCoolingSpeedRatio( 1 ) = 0.3;
	Furnace( FurnaceNum ).CoolMassFlowRate( 2 ) = 0.6;
	Furnace( FurnaceNum ).MSCoolingSpeedRatio( 2 ) = 0.6;
	Furnace( FurnaceNum ).CoolMassFlowRate( 3 ) = 1.2;
	Furnace( FurnaceNum ).MSCoolingSpeedRatio( 3 ) = 1.2;

	CurDeadBandOrSetback( 1 ) = false;

	Furnace( FurnaceNum ).OpMode = CycFanCycCoil;
	// heating air flow at various speeds
	
	Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
//	Furnace( FurnaceNum ).SchedPtr = 0; // denotes incorrect schedule name in Furnace input ( returns 0.0 )
	Furnace( FurnaceNum ).SchedPtr = -1; // denotes missing schedule name in Furnace input ( returns 1.0 )
	HeatingLoad = true;
	CoolingLoad = false;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 1;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.25, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 0.25, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.25, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.25, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 2;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.5, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 0.5, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 3;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 1.0, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 1.0, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 1.0, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 1;
	HeatingLoad = false;
	CoolingLoad = true;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.3, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 0.3, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.3, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.3, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 2;
	HeatingLoad = false;
	CoolingLoad = true;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.6, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 0.6, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.6, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.6, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 3;
	HeatingLoad = false;
	CoolingLoad = true;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 1.2, MSHPMassFlowRateLow );
	EXPECT_DOUBLE_EQ( 1.2, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 1.2, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 1.2, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	// constant fan mode should drop to idle flow rate
	Furnace( FurnaceNum ).OpMode = ContFanCycCoil;

	Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
	Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( 0.0, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.0, MSHPMassFlowRateHigh );
	EXPECT_DOUBLE_EQ( 0.2, CompOffMassFlow );
	EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
	EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
	EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

	// Clean up
	Node.deallocate();
	Furnace.deallocate();
	CurDeadBandOrSetback.deallocate();
	Schedule.deallocate();

}
