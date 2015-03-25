// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/HVACUnitarySystem.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::HVACUnitarySystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataGlobals;


TEST( SetOnOffMassFlowRateTest, Test1 )
{
	int UnitarySysNum ( 1 );
	Real64 OnOffAirFlowRatio; // This is a return value
	Real64 PartLoadRatio( 1.0 );
	MultiOrVarSpeedHeatCoil.allocate( 1 );
	MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
	MultiOrVarSpeedCoolCoil.allocate( 1 );
	MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
	Node.allocate( 1 );

	MSHPMassFlowRateLow = 0.0;
	MSHPMassFlowRateHigh = 0.0;

	UnitarySystem.allocate( 1 );
	UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( 3 );
	UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( 3 );
	UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( 3 );
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( 3 );

	UnitarySystem( UnitarySysNum ).LastMode = HeatingMode;
	UnitarySystem( UnitarySysNum ).IdleMassFlowRate = 0.2;
	UnitarySystem( UnitarySysNum ).IdleSpeedRatio = 0.2;
	UnitarySystem( UnitarySysNum ).FanAvailSchedPtr = ScheduleAlwaysOn;
	UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum = 1;

	UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 ) = 0.25;
	UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 ) = 0.25;
	UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 2 ) = 0.5;
	UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 2 ) = 0.5;
	UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 3 ) = 1.0;
	UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 3 ) = 1.0;

	UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 ) = 0.3;
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 ) = 0.3;
	UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 2 ) = 0.6;
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 2 ) = 0.6;
	UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 3 ) = 1.2;
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 3 ) = 1.2;

	// heating load at various speeds
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 3;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.5, MSHPMassFlowRateLow );
	EXPECT_EQ( 1.0, MSHPMassFlowRateHigh );

	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 2;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.25, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.5, MSHPMassFlowRateHigh );

	// constant fan mode should not drop to idle flow rate as speed = 1
	UnitarySystem( UnitarySysNum ).FanOpMode = ContFanCycCoil;

	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 1;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.25, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.25, MSHPMassFlowRateHigh );

	// heating load with moisture load (cooling coil operates)
	MoistureLoad = -0.001;
	UnitarySystem( UnitarySysNum ).Humidistat = true;
	UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_CoolReheat;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 3;
	HeatingLoad = true;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.6, MSHPMassFlowRateLow );
	EXPECT_EQ( 1.2, MSHPMassFlowRateHigh );
	MoistureLoad = 0.0;
	UnitarySystem( UnitarySysNum ).Humidistat = false;
	UnitarySystem( UnitarySysNum ).DehumidControlType_Num = None;

	// cycling fan mode should drop to idle flow rate as speed = 1
	UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;

	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 1;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = true;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.20, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.25, MSHPMassFlowRateHigh );

	// cooling load at various speeds
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 3;
	HeatingLoad = false;
	CoolingLoad = true;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.6, MSHPMassFlowRateLow );
	EXPECT_EQ( 1.2, MSHPMassFlowRateHigh );

	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 2;
	HeatingLoad = false;
	CoolingLoad = true;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.3, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.6, MSHPMassFlowRateHigh );

	// cycling fan mode should drop to idle flow rate as speed = 1
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 1;
	HeatingLoad = false;
	CoolingLoad = true;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.2, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.3, MSHPMassFlowRateHigh );

	// constant fan mode should not drop to idle flow rate as speed = 1
	UnitarySystem( UnitarySysNum ).FanOpMode = ContFanCycCoil;

	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 1;
	HeatingLoad = false;
	CoolingLoad = true;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.3, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.3, MSHPMassFlowRateHigh );

	// no load condition (operates at idle speed)
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = false;
	CoolingLoad = false;
	SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
	EXPECT_EQ( 0.2, MSHPMassFlowRateLow );
	EXPECT_EQ( 0.2, MSHPMassFlowRateHigh );

	// Clean up
	MultiOrVarSpeedHeatCoil.deallocate();
	MultiOrVarSpeedCoolCoil.deallocate();
	Node.deallocate();
	UnitarySystem.deallocate();

}
