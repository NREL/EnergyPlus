// EnergyPlus::HVACUnitarySystem Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <General.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HVACUnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/DataPlant.hh>

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

TEST( SetOnOffMassFlowRateTest, Test1 )
{
	ShowMessage( "Begin Test: SetOnOffMassFlowRateTest, Test1" );

	int UnitarySysNum( 1 );
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

	InitializePsychRoutines();
	FinalZoneSizing.allocate( 1 );
	ZoneEqSizing.allocate( 1 );

	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout_test.eio", flags ); write_stat = flags.ios(); }

	CurSysNum = 0;
	CurOASysNum = 0;
	CurZoneEqNum = 1;

	UnitarySystem.allocate( 1 );
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

		// when l = 2, MaxCoolAirVolFlow is already set to 1.005 on previous call and represents floor area x flow rate ratio
		if ( iSizingType == DataSizing::FlowPerFloorArea ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.0;
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

		// when l = 2, MaxCoolAirVolFlow is already set to 1.005 on previous call and represents floor area x flow rate ratio
		if ( iSizingType == DataSizing::FlowPerFloorArea ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.005;
		if ( iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.0;
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

		// when l = 2, MaxCoolAirVolFlow is already set to 1.005 on previous call and represents floor area x flow rate ratio
		if ( iSizingType == DataSizing::FlowPerFloorArea ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.005;
		if ( iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 1.005;
		if ( iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.0;
		if ( iCoolingSizingType == DataSizing::FlowPerCoolingCapacity ) UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = 1.005 / 18827.616766698276;
		if ( iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.005;
		if ( iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.0;
		if ( iHeatingSizingType == DataSizing::FlowPerHeatingCapacity ) UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = 1.005 / 1431.9234900374995;

		SizeUnitarySystem( UnitarySysNum, FirstHVACIteration, AirLoopNum );

		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow );
		EXPECT_EQ( 1.005, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow );
		EXPECT_EQ( 18827.616766698276, ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad );
		// why is the heating capacity so much lower ???
		EXPECT_EQ( 1431.9234900374995, ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad );

	}

	// Close and delete eio output file
	if ( SaveOutputFile ) {
		gio::close( OutputFileInits );
	} else {
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
	}

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
	WaterCoil( 1 ).InletAirHumRat = 0.003;
	WaterCoil( 1 ).InletAirEnthalpy = 18000.0;
	WaterCoil( 1 ).AirInletNodeNum = 4;
	WaterCoil( 1 ).AirOutletNodeNum = 5;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Temp = 10.0;
	Node( WaterCoil( 1 ).AirInletNodeNum ).HumRat = 0.003;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy = 18000;
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

	CalcUnitaryHeatingSystem( UnitarySysNum, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCycRatio, CompOn, OnOffAirFlowRatio );

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
	bool FirstHVACIteration( false );
	Real64 OnOffAirFlowRatio( 1.0 );
	Real64 CoilCoolHeatRat( 1.0 );
	Real64 AirMassFlowRate( 0.0 );
	Real64 HotWaterMassFlowRate( 0.0 );
	Real64 ColdWaterMassFlowRate( 0.0 );

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
	UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;

	// cooling load at speed 3
	UnitarySystem( UnitarySysNum ).Humidistat = false;
	UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = 3;
	UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
	UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = 3;
	UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 3;
	HeatingLoad = false;
	CoolingLoad = true;
	// cooling load only
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
	WaterCoil( 1 ).InletAirHumRat = 0.0085;
	WaterCoil( 1 ).InletWaterTemp = 6.0;
	WaterCoil( 1 ).InletAirMassFlowRate = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlowRate;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Temp = 30.0;
	Node( WaterCoil( 1 ).AirInletNodeNum ).HumRat = 0.0085;
	Node( WaterCoil( 1 ).AirInletNodeNum ).Enthalpy = 53000;

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
	DataEnvironment::OutBaroPress = 101325.0;
	DataEnvironment::StdRhoAir = 1.20;

	InitializePsychRoutines();
	WaterCoil( 1 ).TotWaterCoolingCoilRate = 0.0;
	
	CalcUnitaryCoolingSystem( UnitarySysNum, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCycRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat );

	EXPECT_NEAR( 26672.0, WaterCoil( 1 ).TotWaterCoolingCoilRate, 2.0 );

	// Clean up
	MultiOrVarSpeedHeatCoil.deallocate();
	MultiOrVarSpeedCoolCoil.deallocate();
	MyUAAndFlowCalcFlag.deallocate();
	Node.deallocate();
	PlantLoop.deallocate();
	UnitarySystem.deallocate();
	WaterCoil.allocate( 1 );

}

