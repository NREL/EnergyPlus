// EnergyPlus::HVACUnitarySystem Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/HVACUnitarySystem.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/HVACFixture.hh"
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>

using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DXCoils;

using namespace EnergyPlus;
using namespace EnergyPlus::HVACUnitarySystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::WaterCoils;
using General::TrimSigDigits;
using DataEnvironment::OutDryBulbTemp;
using WaterCoils::WaterCoil;
using WaterCoils::WaterCoil_SimpleHeating;
using WaterCoils::WaterCoil_Cooling;
using WaterCoils::SimpleAnalysis;
using General::TrimSigDigits;

TEST_F( HVACFixture, SetOnOffMassFlowRateTest )
{
		std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"  ",
		"ScheduleTypeLimits,",
		"  Any Number;             !- Name",
		"  ",
		"Schedule:Compact,",
		"  FanAndCoilAvailSched,   !- Name",
		"  Any Number,             !- Schedule Type Limits Name",
		"  Through: 12/31,         !- Field 1",
		"  For: AllDays,           !- Field 2",
		"  Until: 24:00, 1.0;      !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) ); // read idf objects
	int UnitarySysNum( 1 );
	Real64 OnOffAirFlowRatio; // This is a return value
	Real64 PartLoadRatio( 1.0 );
	DataHVACGlobals::TurnFansOn = true; // enable fan to run
	MultiOrVarSpeedHeatCoil.allocate( 1 );
	MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
	MultiOrVarSpeedCoolCoil.allocate( 1 );
	MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
	Node.allocate( 1 );

	MSHPMassFlowRateLow = 0.0;
	MSHPMassFlowRateHigh = 0.0;

	UnitarySystem.allocate( 1 );
	UnitarySystem( UnitarySysNum ).SysAvailSchedPtr = GetScheduleIndex( "FanAndCoilAvailSched" ); // "Get" the schedule inputs
	UnitarySystem( UnitarySysNum ).FanAvailSchedPtr = GetScheduleIndex( "FanAndCoilAvailSched" );
	Schedule( 1 ).CurrentValue = 1.0; // set availability and fan schedule to 1

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
	UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DataSizing::None;

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

TEST( UnitarySystemSizingTest, ConfirmUnitarySystemSizingTest )
{
	ShowMessage( "Begin Test: UnitarySystemSizingTest, ConfirmUnitarySystemSizingTest" );

	int UnitarySysNum( 1 );
	int AirLoopNum( 1 );
	int iCoolingSizingType( 1 );
	int iHeatingSizingType( 1 );
	bool FirstHVACIteration( true );
	bool SaveOutputFile( false );
	int write_stat;
	Array1D_int SizingTypes( { DataSizing::None, DataSizing::SupplyAirFlowRate, DataSizing::FlowPerFloorArea, DataSizing::FractionOfAutosizedCoolingAirflow, DataSizing::FractionOfAutosizedHeatingAirflow, DataSizing::FlowPerCoolingCapacity, DataSizing::FlowPerHeatingCapacity } );

	//	int const None( 1 );
	//	int const SupplyAirFlowRate( 2 );
	//	int const FlowPerFloorArea( 3 );
	//	int const FractionOfAutosizedCoolingAirflow( 4 );
	//	int const FractionOfAutosizedHeatingAirflow( 5 );
	//	int const FlowPerCoolingCapacity( 6 );
	//	int const FlowPerHeatingCapacity( 7 );
	//	int const CoolingDesignCapacity( 8 );
	//	int const HeatingDesignCapacity( 9 );
	//	int const CapacityPerFloorArea( 10 );
	//	int const FractionOfAutosizedCoolingCapacity( 11 );
	//	int const FractionOfAutosizedHeatingCapacity( 12 );

	HVACUnitarySystem::NumUnitarySystem = 50; // trick code so that UnitarySystemNumericFields.deallocate(); does not occur within code called from unit test
	InitializePsychRoutines();
	FinalZoneSizing.allocate( 1 );
	ZoneEqSizing.allocate( 1 );

	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout_test.eio", flags ); write_stat = flags.ios(); }

	CurSysNum = 0;
	CurOASysNum = 0;
	CurZoneEqNum = 1;
	DataEnvironment::StdRhoAir = 1000; // Prevent divide by zero in ReportSizingManager

	UnitarySystem.allocate( HVACUnitarySystem::NumUnitarySystem );
	UnitarySystem( UnitarySysNum ).UnitarySystemType = "AirLoopHVAC:UnitarySystem";
	UnitarySystem( UnitarySysNum ).UnitarySystemType_Num = UnitarySystem_AnyCoilType;
	UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

	UnitarySystemNumericFields.allocate( 1 );
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames.allocate( 20 );
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames( 3 ) = "Cooling Supply Air Flow Rate";
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames( 7 ) = "Heating Supply Air Flow Rate";
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames( 11 ) = "No Load Supply Air Flow Rate";
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames( 17 ) = "Maximum Supply Air Temperature";
	UnitarySystemNumericFields( UnitarySysNum ).FieldNames( 18 ) = "Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation";

	ZoneSizingRunDone = true;
	ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
	ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

	// test cooling only sizing
	UnitarySystem( UnitarySysNum ).FanExists = true;
	UnitarySystem( UnitarySysNum ).CoolCoilExists = true;
	UnitarySystem( UnitarySysNum ).HeatCoilExists = false;
	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;

	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 30.0;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.001;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 15.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0006;

	for ( int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerCoolingCapacity; ++iSizingType ) {

		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) continue; // not allowed for cooling air flow

		UnitarySystem( UnitarySysNum ).Name = "UnitarySystem:CoolingOnly #" + TrimSigDigits( iSizingType );
		UnitarySystem( UnitarySysNum ).CoolingSAFMethod = SizingTypes( iSizingType );
		UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = AutoSize;

		// for FractionOfAutosizedCoolingAirflow, set sizing data to 1.005 and UnitarySystem MaxCoolAirVolFlow to 1, they will multiply and yield 1.005
		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.0;
		// for FlowPerCoolingCapacity, do the division so sizing will yield 1.005
		if ( iSizingType == DataSizing::FlowPerCoolingCapacity ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.005 / 18827.616766698276;

		SizeUnitarySystem( UnitarySysNum, FirstHVACIteration, AirLoopNum );

		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow );
		EXPECT_EQ( 18827.616766698276, ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad );

	}

	UnitarySystem( UnitarySysNum ).CoolCoilExists = false;
	UnitarySystem( UnitarySysNum ).HeatCoilExists = true;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.005;
	FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow = 1.005;

	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp = 15.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat = 0.001;
	FinalZoneSizing( CurZoneEqNum ).HeatDesTemp = 30.0;

	for ( int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerHeatingCapacity; ++iSizingType ) {

		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) continue; // not allowed for heating air flow
		if ( iSizingType == DataSizing::FlowPerCoolingCapacity ) continue; // not allowed for heating air flow

		UnitarySystem( UnitarySysNum ).Name = "UnitarySystem:HeatingOnly #" + TrimSigDigits( iSizingType );
		UnitarySystem( UnitarySysNum ).HeatingSAFMethod = SizingTypes( iSizingType );
		UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = AutoSize;

		// for FractionOfAutosizedHeatingAirflow, set sizing data to 1.005 and UnitarySystem MaxHeatAirVolFlow to 1, they will multiply and yield 1.005
		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.0;
		// for FlowPerHeatingCapacity, do the division so sizing will yield 1.005
		if ( iSizingType == DataSizing::FlowPerHeatingCapacity ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.005 / 15148.243236712493;

		SizeUnitarySystem( UnitarySysNum, FirstHVACIteration, AirLoopNum );

		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow );
		EXPECT_EQ( 15148.243236712493, ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad );

	}

	UnitarySystem( UnitarySysNum ).CoolCoilExists = true;
	UnitarySystem( UnitarySysNum ).HeatCoilExists = true;
	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.095;
	FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow = 0.095;

	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 30.0;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.001;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 15.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0006;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp = 15.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat = 0.001;
	FinalZoneSizing( CurZoneEqNum ).HeatDesTemp = 30.0;

	for ( int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerHeatingCapacity; ++iSizingType ) {

		iCoolingSizingType = iSizingType;
		iHeatingSizingType = iSizingType;
		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) iHeatingSizingType = DataSizing::FractionOfAutosizedHeatingAirflow;
		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) iCoolingSizingType = DataSizing::FractionOfAutosizedCoolingAirflow;
		if ( iSizingType == DataSizing::FlowPerCoolingCapacity ) iHeatingSizingType = DataSizing::FlowPerHeatingCapacity;
		if ( iSizingType == DataSizing::FlowPerHeatingCapacity ) iCoolingSizingType = DataSizing::FlowPerCoolingCapacity;
		UnitarySystem( UnitarySysNum ).Name = "UnitarySystem:CoolingAndHeating #" + TrimSigDigits( iSizingType );
		UnitarySystem( UnitarySysNum ).CoolingSAFMethod = SizingTypes( iCoolingSizingType );
		UnitarySystem( UnitarySysNum ).HeatingSAFMethod = SizingTypes( iHeatingSizingType );
		UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = AutoSize;
		UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
		UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = AutoSize;

		// for FractionOfAutosizedCoolingAirflow, set sizing data to 1.005 and UnitarySystem MaxCoolAirVolFlow to 1, they will multiply and yield 1.005
		if ( iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;
		if ( iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.0;
		// for FlowPerCoolingCapacity, do the division so sizing will yield 1.005
		if ( iCoolingSizingType == DataSizing::FlowPerCoolingCapacity ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.005 / 18827.616766698276;
		// for FractionOfAutosizedHeatingAirflow, set sizing data to 1.005 and UnitarySystem MaxHeatAirVolFlow to 1, they will multiply and yield 1.005
		if ( iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.005;
		if ( iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.0;
		// for FlowPerHeatingCapacity, do the division so sizing will yield 1.005
		if ( iHeatingSizingType == DataSizing::FlowPerHeatingCapacity ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.005 / 1431.9234900374995;

		SizeUnitarySystem( UnitarySysNum, FirstHVACIteration, AirLoopNum );

		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow );
		EXPECT_EQ( 18827.616766698276, ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad );
		EXPECT_EQ( 1431.9234900374995, ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad );

	}

	// Close and delete eio output file
	if ( SaveOutputFile ) {
		gio::close( OutputFileInits );
	} else {
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
	}

	HVACUnitarySystem::NumUnitarySystem = 0;
	FinalZoneSizing.deallocate();
	ZoneEqSizing.deallocate();
	UnitarySystem.deallocate();
	UnitarySystemNumericFields.deallocate();
	cached_Twb.deallocate();
	cached_Psat.deallocate();

}
TEST( HVACUnitarySystem, CalcUnitaryHeatingSystem ) {

	ShowMessage( "Begin Test: HVACUnitarySystem, CalcUnitaryHeatingSystem" );

	int UnitarySysNum( 1 );
	int AirLoopNum( 1 );
	bool FirstHVACIteration( false );
	int CompOn( 1 );
	Real64 OnOffAirFlowRatio( 1.0 );
	Real64 HeatCoilLoad( 0.0 );
	Real64 HotWaterMassFlowRate( 0.0 );
	Real64 AirMassFlowRate( 0.0 );

	TotNumLoops = 1;
	PlantLoop.allocate( TotNumLoops );
	MultiOrVarSpeedHeatCoil.allocate( 1 );
	MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
	MultiOrVarSpeedCoolCoil.allocate( 1 );
	MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
	Node.allocate( 10 );
	WaterCoil.allocate( 1 );
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
	UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 3 ) = 1.0;
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 3 ) = 1.0;

	// heating load at speed 3
	UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = 3;
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 3;
	UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = 3;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
	HeatingLoad = true;
	CoolingLoad = false;

	// cycling fan mode
	UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;

	// heating load only
	MoistureLoad = 0.0;
	HeatCoilLoad = 12000.0;
	UnitarySystem( UnitarySysNum ).Humidistat = false;

	AirMassFlowRate = 1.0;
	HotWaterMassFlowRate = 1.0;
	UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = HotWaterMassFlowRate;
	UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil = true;
	UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingWater;
	UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
	UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 3;

	WaterCoils::CheckEquipName.allocate( 1 );
	WaterCoils::NumWaterCoils = 1;
	WaterCoils::GetWaterCoilsInputFlag = false;
	WaterCoil( 1 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;
	WaterCoil( 1 ).Name = "Water Heating Coil";
	WaterCoil( 1 ).WaterCoilType = Coil_HeatingWater;
	WaterCoil( 1 ).WaterCoilType_Num = WaterCoil_SimpleHeating;
	WaterCoil( 1 ).DesAirVolFlowRate = 1.0;
	WaterCoil( 1 ).MaxWaterVolFlowRate = HotWaterMassFlowRate;
	WaterCoil( 1 ).UACoil = 400.0;
	WaterCoil( 1 ).InletAirTemp = 10.0;
	WaterCoil( 1 ).InletAirEnthalpy = 18000.0;
	WaterCoil( 1 ).InletAirHumRat = PsyWFnTdbH( WaterCoil( 1 ).InletAirTemp, WaterCoil( 1 ).InletAirEnthalpy );

	WaterCoil( 1 ).AirInletNodeNum = 4;
	WaterCoil( 1 ).AirOutletNodeNum = 5;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Temp = 10.0;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy = 18000;
	Node( WaterCoil( 1 ).AirInletNodeNum ).HumRat = PsyWFnTdbH( Node( WaterCoil( 1 ).AirInletNodeNum ).Temp, Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy );

	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlowRate;

	WaterCoil( 1 ).WaterLoopNum = 1;
	WaterCoil( 1 ).WaterLoopSide = 1;
	WaterCoil( 1 ).WaterLoopBranchNum = 1;
	WaterCoil( 1 ).WaterLoopCompNum = 1;
	WaterCoil( 1 ).WaterInletNodeNum = 6;
	WaterCoil( 1 ).WaterOutletNodeNum = 7;
	WaterCoil( 1 ).InletWaterTemp = 60.0;
	WaterCoil( 1 ).InletWaterMassFlowRate = HotWaterMassFlowRate;
	WaterCoil( 1 ).MaxWaterMassFlowRate = HotWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = WaterCoil( 1 ).InletWaterTemp;
	Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;

	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "WaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

	UnitarySystem( UnitarySysNum ).HeatingCoilIndex = 1;
	UnitarySystem( UnitarySysNum ).HeatingCoilName = WaterCoil( 1 ).Name;
	UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode = WaterCoil( 1 ).WaterInletNodeNum;
	UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum = WaterCoil( 1 ).WaterOutletNodeNum;
	DataGlobals::DoingSizing = true;
	WaterCoil( 1 ).TotWaterHeatingCoilRate = 0.0;

	CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCycRatio, CompOn, OnOffAirFlowRatio );

	EXPECT_NEAR( 15750.0, WaterCoil( 1 ).TotWaterHeatingCoilRate, 2.0 );

	// Clean up
	DataGlobals::DoingSizing = false;
	MultiOrVarSpeedHeatCoil.deallocate();
	MultiOrVarSpeedCoolCoil.deallocate();
	PlantLoop.deallocate();
	WaterCoil.allocate( 1 );
	Node.deallocate();
	UnitarySystem.deallocate();

}
TEST( HVACUnitarySystem, CalcUnitaryCoolingSystem ) {

	ShowMessage( "Begin Test: HVACUnitarySystem, CalcUnitaryCoolingSystem" );

	int CompOn( 1 );
	int UnitarySysNum( 1 );
	int AirLoopNum( 1 );
	bool FirstHVACIteration( false );
	Real64 OnOffAirFlowRatio( 1.0 );
	Real64 CoilCoolHeatRat( 1.0 );
	Real64 AirMassFlowRate( 0.0 );
	Real64 HotWaterMassFlowRate( 0.0 );
	Real64 ColdWaterMassFlowRate( 0.0 );

	TotNumLoops = 1;
	PlantLoop.allocate( TotNumLoops );

	DataEnvironment::OutBaroPress = 101325.0;
	DataEnvironment::StdRhoAir = 1.20;
	InitializePsychRoutines();

	MultiOrVarSpeedHeatCoil.allocate( 1 );
	MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
	MultiOrVarSpeedCoolCoil.allocate( 1 );
	MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
	Node.allocate( 10 );
	WaterCoil.allocate( 1 );
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
	UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 3 ) = 1.0;
	UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 3 ) = 1.0;
	UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;

	// cooling load at speed 3
	UnitarySystem( UnitarySysNum ).Humidistat = false;
	UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = 3;
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = 3;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 3;
	HeatingLoad = false;
	CoolingLoad = true;
	MoistureLoad = 0.0;
	AirMassFlowRate = 1.0;
	HotWaterMassFlowRate = 1.0;
	ColdWaterMassFlowRate = 1.0;

	UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow = ColdWaterMassFlowRate;
	UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil = true;
	UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWater;
	UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
	UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 3;

	WaterCoils::CheckEquipName.allocate( 1 );
	WaterCoils::NumWaterCoils = 1;
	WaterCoils::GetWaterCoilsInputFlag = false;
	WaterCoil( 1 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;
	WaterCoil( 1 ).Name = "Water Cooling Coil";
	WaterCoil( 1 ).WaterCoilType = CoilType_Cooling;
	WaterCoil( 1 ).WaterCoilType_Num = WaterCoil_Cooling;
	WaterCoil( 1 ).WaterCoilModel = CoilModel_Cooling;
	WaterCoil( 1 ).DesAirVolFlowRate = 1.0;
	WaterCoil( 1 ).MaxWaterVolFlowRate = ColdWaterMassFlowRate;
	WaterCoil( 1 ).CoolingCoilAnalysisMode = SimpleAnalysis;
	WaterCoil( 1 ).HeatExchType = CrossFlow;
	WaterCoil( 1 ).UACoilTotal = 4689.0;
	WaterCoil( 1 ).UACoilExternal = 6110.0;
	WaterCoil( 1 ).UACoilInternal = 20164.0;
	WaterCoil( 1 ).TotCoilOutsideSurfArea = 50.0;

	WaterCoil( 1 ).MaxWaterVolFlowRate = 0.001;
	WaterCoil( 1 ).DesInletWaterTemp = 6.67;
	WaterCoil( 1 ).DesInletAirTemp = 30.0;
	WaterCoil( 1 ).DesOutletAirTemp = 12.0;
	WaterCoil( 1 ).DesInletAirHumRat = 0.013;
	WaterCoil( 1 ).DesOutletAirHumRat = 0.008;
	WaterCoil( 1 ).AirInletNodeNum = 4;
	WaterCoil( 1 ).AirOutletNodeNum = 5;
	WaterCoil( 1 ).InletAirTemp = 30.0;
	WaterCoil( 1 ).InletAirEnthalpy = 53000;
	WaterCoil( 1 ).InletAirHumRat = PsyWFnTdbH( WaterCoil( 1 ).InletAirTemp, WaterCoil( 1 ).InletAirEnthalpy );
	WaterCoil( 1 ).InletWaterTemp = 6.0;
	WaterCoil( 1 ).InletAirMassFlowRate = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Temp = 30.0;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy = 53000;
	Node( WaterCoil( 1 ).AirInletNodeNum ).HumRat = PsyWFnTdbH( Node( WaterCoil( 1 ).AirInletNodeNum ).Temp, Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy );

	WaterCoil( 1 ).WaterLoopNum = 1;
	WaterCoil( 1 ).WaterLoopSide = 1;
	WaterCoil( 1 ).WaterLoopBranchNum = 1;
	WaterCoil( 1 ).WaterLoopCompNum = 1;
	WaterCoil( 1 ).WaterInletNodeNum = 6;
	WaterCoil( 1 ).WaterOutletNodeNum = 7;
	WaterCoil( 1 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
	WaterCoil( 1 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = WaterCoil( 1 ).InletWaterTemp;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
	Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "WaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

	UnitarySystem( UnitarySysNum ).CoolingCoilIndex = 1;
	UnitarySystem( UnitarySysNum ).CoolingCoilName = WaterCoil( 1 ).Name;
	UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode = WaterCoil( 1 ).WaterInletNodeNum;
	UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum = WaterCoil( 1 ).WaterOutletNodeNum;

	MyUAAndFlowCalcFlag.allocate( 1 );
	MyUAAndFlowCalcFlag( 1 ) = true;
	DataGlobals::DoingSizing = true;

	WaterCoil( 1 ).TotWaterCoolingCoilRate = 0.0;

	CalcUnitaryCoolingSystem( UnitarySysNum, FirstHVACIteration, AirLoopNum, UnitarySystem( UnitarySysNum ).CoolingCycRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat );

	EXPECT_NEAR( 27530.0, WaterCoil( 1 ).TotWaterCoolingCoilRate, 2.0 );

	// Clean up
	MultiOrVarSpeedHeatCoil.deallocate();
	MultiOrVarSpeedCoolCoil.deallocate();
	MyUAAndFlowCalcFlag.deallocate();
	Node.deallocate();
	PlantLoop.deallocate();
	UnitarySystem.deallocate();
	WaterCoil.allocate( 1 );

}


TEST_F( HVACFixture, UnitarySystem_GetInput ) {
	UnitarySystemData thisUnSys;

	bool ErrorsFound( false );
	bool FirstHVACIteration( false );
	Real64 CpAir( 0.0 ); // specific heat of air
	Real64 Qsens_sys( 0.0 ); // UnitarySystem delivered sensible capacity wrt zone
	Real64 MinHumRatio( 0.0 ); // track minimum of outlet node or zone humidity ratio
	Real64 ZoneTemp( 0.0 ); // control zone temperature
	int InletNode( 0 ); // UnitarySystem inlet node number
	int OutletNode( 0 ); // UnitarySystem outlet node number
	int ControlZoneNum( 0 ); // index to control zone

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"  ",
		"Zone,",
		"  EAST ZONE,              !- Name",
		"  0,                      !- Direction of Relative North{ deg }",
		"  0,                      !- X Origin{ m }",
		"  0,                      !- Y Origin{ m }",
		"  0,                      !- Z Origin{ m }",
		"  1,                      !- Type",
		"  1,                      !- Multiplier",
		"  autocalculate,          !- Ceiling Height{ m }",
		"  autocalculate;          !- Volume{ m3 }",
		"  ",
		"ZoneHVAC:EquipmentConnections,",
		"EAST ZONE,                 !- Zone Name",
		"  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
		"  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
		"  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
		"  Zone 2 Node,             !- Zone Air Node Name",
		"  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
		"  ",
		"ZoneHVAC:EquipmentList,",
		"  Zone2Equipment,          !- Name",
		"  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
		"  GasHeat DXAC Furnace 1,          !- Zone Equipment 1 Name",
		"  1,                       !- Zone Equipment 1 Cooling Sequence",
		"  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
		"  ",
		"AirLoopHVAC:UnitarySystem,",
		"  GasHeat DXAC Furnace 1, !- Name",
		"  Load,                   !- Control Type",
		"  East Zone,              !- Controlling Zone or Thermostat Location",
		"  None,                   !- Dehumidification Control Type",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  Zone Exhaust Node,         !- Air Inlet Node Name",
		"  Zone 2 Inlet Node,   !- Air Outlet Node Name",
		"  Fan:OnOff,              !- Supply Fan Object Type",
		"  Supply Fan 1,           !- Supply Fan Name",
		"  BlowThrough,            !- Fan Placement",
		"  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
		"  Coil:Heating:Gas,       !- Heating Coil Object Type",
		"  Furnace Heating Coil 1, !- Heating Coil Name",
		"  ,                       !- DX Heating Coil Sizing Ratio",
		"  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
		"  Furnace ACDXCoil 1,     !- Cooling Coil Name",
		"  ,                       !- Use DOAS DX Cooling Coil",
		"  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
		"  ,                       !- Latent Load Control",
		"  Coil:Heating:Gas,       !- Supplemental Heating Coil Object Type",
		"  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
		"  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
		"  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
		"  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
		"  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  80;                     !- Maximum Supply Air Temperature{ C }",
		"  ",
		"Fan:OnOff,",
		"  Supply Fan 1,           !- Name",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  0.7,                    !- Fan Total Efficiency",
		"  600.0,                  !- Pressure Rise{ Pa }",
		"  1.6,                    !- Maximum Flow Rate{ m3 / s }",
		"  0.9,                    !- Motor Efficiency",
		"  1.0,                    !- Motor In Airstream Fraction",
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
		"  ",
		"Coil:Cooling:DX:SingleSpeed,",
		"  Furnace ACDXCoil 1,      !- Name",
		"  FanAndCoilAvailSched,    !- Availability Schedule Name",
		"  32000,                   !- Gross Rated Total Cooling Capacity {W}",
		"  0.75,                    !- Gross Rated Sensible Heat Ratio",
		"  3.0,                     !- Gross Rated Cooling COP {W/W}",
		"  1.6,                     !- Rated Air Flow Rate {m3/s}",
		"  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
		"  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
		"  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
		"  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
		"  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
		"  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
		"  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
		"  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
		"  4,                       !- Maximum Cycling Rate {cycles/hr}",
		"  45;                      !- Latent Capacity Time Constant {s}",
		"  ",
		"Coil:Heating:Gas,",
		"  Furnace Heating Coil 1, !- Name",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  0.8,                    !- Gas Burner Efficiency",
		"  32000,                  !- Nominal Capacity{ W }",
		"  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
		"  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
		"  ",
		"Coil:Heating:Gas,",
		"  Humidistat Reheat Coil 1, !- Name",
		"  FanAndCoilAvailSched, !- Availability Schedule Name",
		"  0.8, !- Gas Burner Efficiency",
		"  32000, !- Nominal Capacity{ W }",
		"  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
		"  Zone 2 Inlet Node;    !- Air Outlet Node Name",
		"  ",
		"ScheduleTypeLimits,",
		"  Any Number;             !- Name",
		"  ",
		"Schedule:Compact,",
		"  FanAndCoilAvailSched,   !- Name",
		"  Any Number,             !- Schedule Type Limits Name",
		"  Through: 12/31,         !- Field 1",
		"  For: AllDays,           !- Field 2",
		"  Until: 24:00, 1.0;      !- Field 3",
		"  ",
		"Schedule:Compact,",
		"  ContinuousFanSchedule,  !- Name",
		"  Any Number,             !- Schedule Type Limits Name",
		"  Through: 12/31,         !- Field 1",
		"  For: AllDays,           !- Field 2",
		"  Until: 24:00, 1.0;      !- Field 3",
		"  ",
		"Curve:Quadratic,",
		"  WindACCoolCapFFF,       !- Name",
		"  0.8,                    !- Coefficient1 Constant",
		"  0.2,                    !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",
		"  ",
		"Curve:Quadratic,",
		"  WindACEIRFFF,           !- Name",
		"  1.1552,                 !- Coefficient1 Constant",
		"  -0.1808,                !- Coefficient2 x",
		"  0.0256,                 !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",
		"  ",
		"Curve:Quadratic,",
		"  WindACPLFFPLR,          !- Name",
		"  0.85,                   !- Coefficient1 Constant",
		"  0.15,                   !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.0,                    !- Minimum Value of x",
		"  1.0;                    !- Maximum Value of x",
		"  ",
		"Curve:Biquadratic,",
		"  WindACCoolCapFT,        !- Name",
		"  0.942587793,            !- Coefficient1 Constant",
		"  0.009543347,            !- Coefficient2 x",
		"  0.000683770,            !- Coefficient3 x**2",
		"  -0.011042676,           !- Coefficient4 y",
		"  0.000005249,            !- Coefficient5 y**2",
		"  -0.000009720,           !- Coefficient6 x*y",
		"  12.77778,               !- Minimum Value of x",
		"  23.88889,               !- Maximum Value of x",
		"  18.0,                   !- Minimum Value of y",
		"  46.11111,               !- Maximum Value of y",
		"  ,                       !- Minimum Curve Output",
		"  ,                       !- Maximum Curve Output",
		"  Temperature,            !- Input Unit Type for X",
		"  Temperature,            !- Input Unit Type for Y",
		"  Dimensionless;          !- Output Unit Type",
		"  ",
		"Curve:Biquadratic,",
		"  WindACEIRFT,            !- Name",
		"  0.342414409,            !- Coefficient1 Constant",
		"  0.034885008,            !- Coefficient2 x",
		"  -0.000623700,           !- Coefficient3 x**2",
		"  0.004977216,            !- Coefficient4 y",
		"  0.000437951,            !- Coefficient5 y**2",
		"  -0.000728028,           !- Coefficient6 x*y",
		"  12.77778,               !- Minimum Value of x",
		"  23.88889,               !- Maximum Value of x",
		"  18.0,                   !- Minimum Value of y",
		"  46.11111,               !- Maximum Value of y",
		"  ,                       !- Minimum Curve Output",
		"  ,                       !- Maximum Curve Output",
		"  Temperature,            !- Input Unit Type for X",
		"  Temperature,            !- Input Unit Type for Y",
		"  Dimensionless;          !- Output Unit Type",	
	} );

	ASSERT_FALSE( process_idf( idf_objects ) ); // read idf objects

	GetZoneData( ErrorsFound ); // read zone data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	GetZoneEquipmentData1(); // read zone equipment configuration and list objects

	ZoneEquipList( 1 ).EquipIndex.allocate( 1 );
	ZoneEquipList( 1 ).EquipIndex( 1 ) = 1; // initialize equipment index for ZoneHVAC
	
	GetUnitarySystemInput(); // get UnitarySystem input from object above
	HVACUnitarySystem::GetInputFlag = false; // don't call GetInput more than once (SimUnitarySystem call below will call GetInput if this flag is not set to false)

	ASSERT_EQ( 1, NumUnitarySystem ); // only 1 unitary system above so expect 1 as number of unitary system objects
	EXPECT_EQ( UnitarySystem( 1 ).UnitarySystemType, cFurnaceTypes( UnitarySystem( 1 ).UnitarySystemType_Num ) ); // compare UnitarySystem type string to valid type
	
	DataGlobals::SysSizingCalc = true; // DISABLE SIZING - don't call HVACUnitarySystem::SizeUnitarySystem, much more work needed to set up sizing arrays 

	InletNode = UnitarySystem( 1 ).UnitarySystemInletNodeNum;
	OutletNode = UnitarySystem( 1 ).UnitarySystemOutletNodeNum;
	ControlZoneNum = UnitarySystem( 1 ).NodeNumOfControlledZone;

	// set up unitary system inlet condtions
	Node( InletNode ).Temp = 26.666667; // AHRI condition 80F dry-bulb temp
	Node( InletNode ).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
	Node( InletNode ).Enthalpy = PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat );

	// set zone temperature
	Node( ControlZoneNum ).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity

	// initialize other incidentals that are used within the UnitarySystem module during calculations
	CurZoneEqNum = 1;
	ZoneSysEnergyDemand.allocate( 1 );
	ZoneSysMoistureDemand.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired = 1000.0; // heating load
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP = 2000.0;
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP = 1000.0;
	ZoneSysMoistureDemand( ControlZoneNum ).OutputRequiredToDehumidifyingSP = 0.0;
	TempControlType.allocate( 1 );
	TempControlType( 1 ) = DataHVACGlobals::DualSetPointWithDeadBand;
	CurDeadBandOrSetback.allocate( 1 );
	CurDeadBandOrSetback( 1 ) = false;
	Schedule( 1 ).CurrentValue = 1.0;
	DataGlobals::BeginEnvrnFlag = true;
	DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 ); // initialize RhoAir

	SimUnitarySystem( UnitarySystem( 1 ).Name, FirstHVACIteration, UnitarySystem( 1 ).ControlZoneNum, ZoneEquipList( 1 ).EquipIndex( 1 ), _, _, _, _, true );

	ZoneTemp = Node( ControlZoneNum ).Temp;
	CpAir = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );

	// calculation at end of CalcUnitarySystemToLoad():
	//	SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;

	MinHumRatio = Node( ControlZoneNum ).HumRat; // zone humidity ratio
	if ( Node( OutletNode ).Temp < Node( ControlZoneNum ).Temp ) MinHumRatio = Node( OutletNode ).HumRat; // use lower of zone and outlet humidity ratio
	Qsens_sys = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) );

	// test model performance
	EXPECT_NEAR( ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired, Qsens_sys, 0.01 ); // Watts
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, UnitarySystem( 1 ).MaxHeatAirMassFlow * UnitarySystem( 1 ).PartLoadFrac ); // cycling fan
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, Node( OutletNode ).MassFlowRate );


	ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired = -1000.0; // cooling load
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP = -1000.0;
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP = -2000.0;

	// set zone temperature
	Node( ControlZoneNum ).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
	DataEnvironment::OutDryBulbTemp = 35.0; // initialize weather
	DataEnvironment::OutHumRat = 0.1;
	DataEnvironment::OutBaroPress=101325.0;
	DataEnvironment::OutWetBulbTemp = 30.0;
	DXCoil( 1 ).RatedCBF( 1 ) = 0.1; // autosizing is disabled so initialize coil bypass factor
	DXCoil( 1 ).RatedAirMassFlowRate( 1 ) = 1.9268939689375426; // autosizing is disabled so initialize cooling coil rated air mass flow rate

	SimUnitarySystem( UnitarySystem( 1 ).Name, FirstHVACIteration, UnitarySystem( 1 ).ControlZoneNum, ZoneEquipList( 1 ).EquipIndex( 1 ), _, _, _, _, true );

	ZoneTemp = Node( ControlZoneNum ).Temp;
	CpAir = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );

	// calculation at end of CalcUnitarySystemToLoad():
	//	SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;

	MinHumRatio = Node( ControlZoneNum ).HumRat; // zone humidity ratio
	if ( Node( OutletNode ).Temp < Node( ControlZoneNum ).Temp ) MinHumRatio = Node( OutletNode ).HumRat; // use lower of zone and outlet humidity ratio
	Qsens_sys = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) );

	// test model performance
	EXPECT_NEAR( ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired, Qsens_sys, 0.025 ); // Watts
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, UnitarySystem( 1 ).MaxCoolAirMassFlow * UnitarySystem( 1 ).PartLoadFrac ); // cycling fan
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, Node( OutletNode ).MassFlowRate );


	// clean up non clear-state arrays and reset scalars
	ZoneSysEnergyDemand.deallocate();
	ZoneSysMoistureDemand.deallocate();
	CurDeadBandOrSetback.deallocate();
	TempControlType.deallocate();
	DataGlobals::BeginEnvrnFlag = false;
	DataEnvironment::StdRhoAir = 0.0;
	DataEnvironment::OutDryBulbTemp = 0.0;
	DataEnvironment::OutHumRat = 0.0;
	DataEnvironment::OutBaroPress=0.0;
	DataEnvironment::OutWetBulbTemp = 0.0;

}

TEST_F( HVACFixture, UnitarySystem_VSDXCoilSizing ) {
	UnitarySystemData thisUnSys;

	bool ErrorsFound( false );

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",

		"Zone,",
		"  EAST ZONE,              !- Name",
		"  0,                      !- Direction of Relative North{ deg }",
		"  0,                      !- X Origin{ m }",
		"  0,                      !- Y Origin{ m }",
		"  0,                      !- Z Origin{ m }",
		"  1,                      !- Type",
		"  1,                      !- Multiplier",
		"  autocalculate,          !- Ceiling Height{ m }",
		"  autocalculate;          !- Volume{ m3 }",

		"ZoneHVAC:EquipmentConnections,",
		"EAST ZONE,                 !- Zone Name",
		"  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
		"  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
		"  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
		"  Zone 2 Node,             !- Zone Air Node Name",
		"  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		"  Zone2Equipment,          !- Name",
		"  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
		"  GasHeat DXAC Furnace 1,          !- Zone Equipment 1 Name",
		"  1,                       !- Zone Equipment 1 Cooling Sequence",
		"  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

		"AirLoopHVAC:UnitarySystem,",
		"  GasHeat DXAC Furnace 1, !- Name",
		"  Load,                   !- Control Type",
		"  East Zone,              !- Controlling Zone or Thermostat Location",
		"  None,                   !- Dehumidification Control Type",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  Zone Exhaust Node,         !- Air Inlet Node Name",
		"  Zone 2 Inlet Node,   !- Air Outlet Node Name",
		"  Fan:OnOff,              !- Supply Fan Object Type",
		"  Supply Fan 1,           !- Supply Fan Name",
		"  BlowThrough,            !- Fan Placement",
		"  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
		"  Coil:Heating:DX:VariableSpeed,       !- Heating Coil Object Type",
		"  Furnace Heating Coil 1, !- Heating Coil Name",
		"  ,                       !- DX Heating Coil Sizing Ratio",
		"  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
		"  Furnace ACDXCoil 1,     !- Cooling Coil Name",
		"  ,                       !- Use DOAS DX Cooling Coil",
		"  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
		"  ,                       !- Latent Load Control",
		"  Coil:Heating:Gas,       !- Supplemental Heating Coil Object Type",
		"  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
		"  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
		"  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
		"  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
		"  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  80;                     !- Maximum Supply Air Temperature{ C }",

		"Fan:OnOff,",
		"  Supply Fan 1,           !- Name",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  0.7,                    !- Fan Total Efficiency",
		"  600.0,                  !- Pressure Rise{ Pa }",
		"  1.6,                    !- Maximum Flow Rate{ m3 / s }",
		"  0.9,                    !- Motor Efficiency",
		"  1.0,                    !- Motor In Airstream Fraction",
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

		"Coil:Cooling:DX:SingleSpeed,",
		"  Furnace ACDXCoil 1,      !- Name",
		"  FanAndCoilAvailSched,    !- Availability Schedule Name",
		"  32000,                   !- Gross Rated Total Cooling Capacity {W}",
		"  0.75,                    !- Gross Rated Sensible Heat Ratio",
		"  3.0,                     !- Gross Rated Cooling COP {W/W}",
		"  1.6,                     !- Rated Air Flow Rate {m3/s}",
		"  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
		"  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
		"  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
		"  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
		"  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
		"  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
		"  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
		"  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
		"  4,                       !- Maximum Cycling Rate {cycles/hr}",
		"  45;                      !- Latent Capacity Time Constant {s}",

		"Coil:Heating:DX:VariableSpeed, ",
		"  Furnace Heating Coil 1, !- Name",
		"  Heating Coil Air Inlet Node,  !- Indoor Air Inlet Node Name",
		"  Reheat Coil Air Inlet Node,  !- Indoor Air Outlet Node Name",
		"  10.0,                    !- Number of Speeds {dimensionless}",
		"  10.0,                    !- Nominal Speed Level {dimensionless}",
		"  autosize,                   !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
		"  1.7,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
		"  HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
		"      ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
		"  -5.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
		"  ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
		"  5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
		"  200.0,                   !- Crankcase Heater Capacity {W}",
		"  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
		"  Resistive,               !- Defrost Strategy",
		"  TIMED,                   !- Defrost Control",
		"  0.166667,                !- Defrost Time Period Fraction",
		"  20000,                   !- Resistive Defrost Heater Capacity {W}",
		"  1838.7,                  !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
		"  0.1661088,               !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 1 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 1 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  2295.5,                  !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
		"  0.179322,                !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 2 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 2 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  2751.3,                  !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
		"  0.1925352,               !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 3 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 3 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  3659.6,                  !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
		"  0.2189616,               !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 4 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 4 Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  4563.7,                  !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
		"   0.245388,                !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 5 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 5 Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  5463.3,                  !- Speed 6 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 6 Reference Unit Gross Rated Heating COP {dimensionless}",
		"  0.2718144,               !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
		"  HPACHeatCapFT,           !- Speed 6 Heating Capacity Function of Temperature Curve Name",
		"  HPACHeatCapFFF,          !- Speed 6 Heating Capacity Function of Air Flow Fraction Curve Name",
		"  HPACHeatEIRFT,           !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
		"  HPACHeatEIRFFF,          !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"   6358.4,                  !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.2982408,               !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,           !- Speed 7 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,          !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,           !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,          !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    7248.5,                  !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                     !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
		"     0.3246672,               !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,           !- Speed 8 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,          !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,           !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,          !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    8133.6,                  !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                     !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.3510936,               !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,           !- Speed 9 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,          !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name",
		"     HPACHeatEIRFT,           !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,          !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    9013.2,                  !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                     !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.37752,                 !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,           !- Speed 10 Heating Capacity Function of Temperature Curve Name",
		"     HPACHeatCapFFF,          !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name",
 		"     HPACHeatEIRFT,           !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
		"      HPACHeatEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

		"  Curve:Quadratic,",
		"    HPACHeatEIRFFF,          !- Name",
		"    1.3824,                  !- Coefficient1 Constant",
		"    -0.4336,                 !- Coefficient2 x",
		"    0.0512,                  !- Coefficient3 x**2",
		"    0.0,                     !- Minimum Value of x",
		"    1.0;                     !- Maximum Value of x",


		"  Curve:Quadratic,",
		"    HPACCOOLPLFFPLR,         !- Name",
		"    0.85,                    !- Coefficient1 Constant",
		"    0.15,                    !- Coefficient2 x",
		"    0.0,                     !- Coefficient3 x**2",
		"    0.0,                     !- Minimum Value of x",
		"    1.0;                     !- Maximum Value of x",

		"  Curve:Cubic,",
		"    HPACHeatCapFFF,          !- Name",
		"    0.84,                    !- Coefficient1 Constant",
		"    0.16,                    !- Coefficient2 x",
		"    0.0,                     !- Coefficient3 x**2",
		"    0.0,                     !- Coefficient4 x**3",
		"    0.5,                     !- Minimum Value of x",
		"    1.5;                     !- Maximum Value of x",


		"    Curve:Biquadratic,",
		"      HPACHeatCapFT,           !- Name",
		"      0.8529681407,            !- Coefficient1 Constant",
		"      -0.0004847169,           !- Coefficient2 x",
		"     -0.0000010693,           !- Coefficient3 x**2",
		"      0.0185542164,            !- Coefficient4 y",
		"      0.0000872425,            !- Coefficient5 y**2",
		"      -0.0000166868,           !- Coefficient6 x*y",
		"      17.78,                   !- Minimum Value of x",
		"      23.33,                   !- Maximum Value of x",
		"      -28.89,                  !- Minimum Value of y",
		"      17.22,                   !- Maximum Value of y",
		"      0.3799,                  !- Minimum Curve Output",
		"      1.1896,                  !- Maximum Curve Output",
		"      Temperature,             !- Input Unit Type for X",
		"      Temperature,             !- Input Unit Type for Y",
		"      Dimensionless;           !- Output Unit Type",

		"    Curve:Biquadratic,",
		"      HPACHeatEIRFT,           !- Name",
		"      0.7077081462,            !- Coefficient1 Constant",
		"      0.0148163478,            !- Coefficient2 x",
		"      0.0002622589,            !- Coefficient3 x**2",
		"      -0.0113239622,           !- Coefficient4 y",
		"      0.0002939277,            !- Coefficient5 y**2",
		"      -0.0003605284,           !- Coefficient6 x*y",
		"      17.78,                   !- Minimum Value of x",
		"      23.33,                   !- Maximum Value of x",
		"      -28.89,                  !- Minimum Value of y",
		"      17.22,                   !- Maximum Value of y",
		"      0.8266,                  !- Minimum Curve Output",
		"      2.0277,                  !- Maximum Curve Output",
		"      Temperature,             !- Input Unit Type for X",
		"      Temperature,             !- Input Unit Type for Y",
		"      Dimensionless;           !- Output Unit Type",

		"Coil:Heating:Gas,",
		"  Humidistat Reheat Coil 1, !- Name",
		"  FanAndCoilAvailSched, !- Availability Schedule Name",
		"  0.8, !- Gas Burner Efficiency",
		"  32000, !- Nominal Capacity{ W }",
		"  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
		"  Zone 2 Inlet Node;    !- Air Outlet Node Name",

		"ScheduleTypeLimits,",
		"  Any Number;             !- Name",

		"Schedule:Compact,",
		"  FanAndCoilAvailSched,   !- Name",
		"  Any Number,             !- Schedule Type Limits Name",
		"  Through: 12/31,         !- Field 1",
		"  For: AllDays,           !- Field 2",
		"  Until: 24:00, 1.0;      !- Field 3",

		"Schedule:Compact,",
		"  ContinuousFanSchedule,  !- Name",
		"  Any Number,             !- Schedule Type Limits Name",
		"  Through: 12/31,         !- Field 1",
		"  For: AllDays,           !- Field 2",
		"  Until: 24:00, 1.0;      !- Field 3",

		"Curve:Quadratic,",
		"  WindACCoolCapFFF,       !- Name",
		"  0.8,                    !- Coefficient1 Constant",
		"  0.2,                    !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",

		"Curve:Quadratic,",
		"  WindACEIRFFF,           !- Name",
		"  1.1552,                 !- Coefficient1 Constant",
		"  -0.1808,                !- Coefficient2 x",
		"  0.0256,                 !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",

		"Curve:Quadratic,",
		"  WindACPLFFPLR,          !- Name",
		"  0.85,                   !- Coefficient1 Constant",
		"  0.15,                   !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.0,                    !- Minimum Value of x",
		"  1.0;                    !- Maximum Value of x",

		"Curve:Biquadratic,",
		"  WindACCoolCapFT,        !- Name",
		"  0.942587793,            !- Coefficient1 Constant",
		"  0.009543347,            !- Coefficient2 x",
		"  0.000683770,            !- Coefficient3 x**2",
		"  -0.011042676,           !- Coefficient4 y",
		"  0.000005249,            !- Coefficient5 y**2",
		"  -0.000009720,           !- Coefficient6 x*y",
		"  12.77778,               !- Minimum Value of x",
		"  23.88889,               !- Maximum Value of x",
		"  18.0,                   !- Minimum Value of y",
		"  46.11111,               !- Maximum Value of y",
		"  ,                       !- Minimum Curve Output",
		"  ,                       !- Maximum Curve Output",
		"  Temperature,            !- Input Unit Type for X",
		"  Temperature,            !- Input Unit Type for Y",
		"  Dimensionless;          !- Output Unit Type",

		"Curve:Biquadratic,",
		"  WindACEIRFT,            !- Name",
		"  0.342414409,            !- Coefficient1 Constant",
		"  0.034885008,            !- Coefficient2 x",
		"  -0.000623700,           !- Coefficient3 x**2",
		"  0.004977216,            !- Coefficient4 y",
		"  0.000437951,            !- Coefficient5 y**2",
		"  -0.000728028,           !- Coefficient6 x*y",
		"  12.77778,               !- Minimum Value of x",
		"  23.88889,               !- Maximum Value of x",
		"  18.0,                   !- Minimum Value of y",
		"  46.11111,               !- Maximum Value of y",
		"  ,                       !- Minimum Curve Output",
		"  ,                       !- Maximum Curve Output",
		"  Temperature,            !- Input Unit Type for X",
		"  Temperature,            !- Input Unit Type for Y",
		"  Dimensionless;          !- Output Unit Type",	
	} );

	ASSERT_FALSE( process_idf( idf_objects ) ); // read idf objects

	GetZoneData( ErrorsFound ); // read zone data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	GetZoneEquipmentData1(); // read zone equipment configuration and list objects

	ZoneEquipList( 1 ).EquipIndex.allocate( 1 );
	ZoneEquipList( 1 ).EquipIndex( 1 ) = 1; // initialize equipment index for ZoneHVAC
	
	GetUnitarySystemInput(); // get UnitarySystem input from object above
	HVACUnitarySystem::GetInputFlag = false; // don't call GetInput more than once (SimUnitarySystem call below will call GetInput if this flag is not set to false)

	ASSERT_EQ( 1, NumUnitarySystem ); // only 1 unitary system above so expect 1 as number of unitary system objects

	ASSERT_EQ( UnitarySystem( 1 ).DesignHeatingCapacity , AutoSize );


	// clean up non clear-state arrays and reset scalars
	ZoneSysEnergyDemand.deallocate();
	ZoneSysMoistureDemand.deallocate();
	CurDeadBandOrSetback.deallocate();
	TempControlType.deallocate();
	DataGlobals::BeginEnvrnFlag = false;
	DataEnvironment::StdRhoAir = 0.0;
	DataEnvironment::OutDryBulbTemp = 0.0;
	DataEnvironment::OutHumRat = 0.0;
	DataEnvironment::OutBaroPress=0.0;
	DataEnvironment::OutWetBulbTemp = 0.0;

}

