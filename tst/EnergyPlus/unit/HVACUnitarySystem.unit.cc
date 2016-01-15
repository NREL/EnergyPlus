// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::HVACUnitarySystem Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

#include <General.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/HVACUnitarySystem.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>

using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::VariableSpeedCoils;

using namespace EnergyPlus;
using namespace EnergyPlus::HVACUnitarySystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataBranchNodeConnections;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneEquipment;
using namespace DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HeatingCoils;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::WaterCoils;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::VariableSpeedCoils;
using General::TrimSigDigits;
using DataEnvironment::OutDryBulbTemp;
using WaterCoils::WaterCoil;
using WaterCoils::WaterCoil_SimpleHeating;
using WaterCoils::WaterCoil_Cooling;
using WaterCoils::SimpleAnalysis;
using General::TrimSigDigits;

TEST_F( EnergyPlusFixture, SetOnOffMassFlowRateTest )
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

}

TEST_F( EnergyPlusFixture, UnitarySystemSizingTest_ConfirmUnitarySystemSizingTest )
{
	int UnitarySysNum( 1 );
	int AirLoopNum( 1 );
	int iCoolingSizingType( 1 );
	int iHeatingSizingType( 1 );
	bool FirstHVACIteration( true );
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
	FinalZoneSizing.allocate( 1 );
	ZoneEqSizing.allocate( 1 );

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

}

TEST_F( EnergyPlusFixture, HVACUnitarySystem_CalcUnitaryHeatingSystem ) {

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

}

TEST_F( EnergyPlusFixture, HVACUnitarySystem_CalcUnitaryCoolingSystem ) {

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

}

TEST_F( EnergyPlusFixture, UnitarySystem_GetInput ) {

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

	ZoneEqSizing.allocate( 1 );
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

	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP.allocate( 1 );
	ZoneSysMoistureDemand( ControlZoneNum ).SequencedOutputRequiredToDehumidSP.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP;
	ZoneSysMoistureDemand( ControlZoneNum ).SequencedOutputRequiredToDehumidSP( 1 ) = ZoneSysMoistureDemand( ControlZoneNum ).OutputRequiredToDehumidifyingSP;

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
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP;
	ZoneSysMoistureDemand( ControlZoneNum ).SequencedOutputRequiredToDehumidSP( 1 ) = ZoneSysMoistureDemand( ControlZoneNum ).OutputRequiredToDehumidifyingSP;

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

	// new tests for #5287, need to add an air loop to do this unit test justice
	EXPECT_TRUE( UnitarySystem( 1 ).FanIndex > 0 ); // ZoneHVAC must contain a fan object to provide flow
	EXPECT_EQ( UnitarySystem( 1 ).FanType_Num, DataHVACGlobals::FanType_SimpleOnOff ); // fan must be FanOnOff when used with cycling fan

}

TEST_F( EnergyPlusFixture, UnitarySystem_VSDXCoilSizing ) {
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
		"  GasHeat DXAC Furnace 1,  !- Zone Equipment 1 Name",
		"  1,                       !- Zone Equipment 1 Cooling Sequence",
		"  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

		"AirLoopHVAC:UnitarySystem,",
		"  GasHeat DXAC Furnace 1, !- Name",
		"  Load,                   !- Control Type",
		"  East Zone,              !- Controlling Zone or Thermostat Location",
		"  None,                   !- Dehumidification Control Type",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  Zone 2 Inlet Node,      !- Air Outlet Node Name",
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
		"  autosize,                !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
		"  1.7,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
		"  HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
		"      ,                    !- Defrost Energy Input Ratio Function of Temperature Curve Name",
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
		"   0.245388,               !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
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
		"   6358.4,                 !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.2982408,             !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 7 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,         !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    7248.5,                !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
		"     0.3246672,            !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 8 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,         !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    8133.6,                !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.3510936,             !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 9 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name",
		"     HPACHeatEIRFT,        !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    9013.2,                !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.37752,               !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 10 Heating Capacity Function of Temperature Curve Name",
		"     HPACHeatCapFFF,       !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name",
 		"     HPACHeatEIRFT,        !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
		"      HPACHeatEIRFFF;      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

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
		"     -0.0000010693,            !- Coefficient3 x**2",
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
		"  Humidistat Reheat Coil 1,    !- Name",
		"  FanAndCoilAvailSched,        !- Availability Schedule Name",
		"  0.8,                         !- Gas Burner Efficiency",
		"  32000,                       !- Nominal Capacity{ W }",
		"  Reheat Coil Air Inlet Node,  !- Air Inlet Node Name",
		"  Zone 2 Inlet Node;           !- Air Outlet Node Name",

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

}


TEST_F( EnergyPlusFixture, UnitarySystem_VarSpeedCoils ) {

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
		"  Coil:Cooling:DX:VariableSpeed, !- Cooling Coil Object Type",
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
		"Coil:Cooling:DX:VariableSpeed,",
		"  Furnace ACDXCoil 1, !- Name",
		"  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
		"  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
		"  10.0, !- Number of Speeds{ dimensionless }",
		"  10.0, !- Nominal Speed Level{ dimensionless }",
		"  32000.0, !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
		"  1.6, !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
		"  0.0, !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
		"  0.0, !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
		"  PLFFPLR, !- Energy Part Load Fraction Curve Name",
		"  , !- Condenser Air Inlet Node Name",
		"  AirCooled, !- Condenser Type",
		"  , !- Evaporative Condenser Pump Rated Power Consumption{ W }",
		"  200.0, !- Crankcase Heater Capacity{ W }",
		"  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
		"  , !- Supply Water Storage Tank Name",
		"  , !- Condensate Collection Water Storage Tank Name",
		"  , !- Basin Heater Capacity{ W / K }",
		"  , !- Basin Heater Setpoint Temperature{ C }",
		"  , !- Basin Heater Operating Schedule Name",
		"  1524.1, !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.1359072, !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.26, !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  1877.9, !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.151008, !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.30, !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  2226.6, !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.1661088, !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.33, !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  2911.3, !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.1963104, !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.38, !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  3581.7, !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.226512, !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.44, !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  4239.5, !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 6 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.2567136, !- Speed 6 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.50, !- Speed 6 Reference Unit Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 6 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  4885.7, !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 7 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.2869152, !- Speed 7 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.57, !- Speed 7 Reference Unit Condenser Flow Rate{ m3 / s }",
		"  , !- Speed 7 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  5520.7, !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 8 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.3171168, !- Speed 8 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.63, !- Speed 8 Reference Unit Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 8 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  6144.8, !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 9 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.3473184, !- Speed 9 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.69, !- Speed 9 Reference Unit Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 9 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF, !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"  6758.0, !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity{ w }",
		"  0.75, !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
		"  4.0, !- Speed 10 Reference Unit Gross Rated Cooling COP{ dimensionless }",
		"  0.37752, !- Speed 10 Reference Unit Rated Air Flow Rate{ m3 / s }",
		"  0.74, !- Speed 10 Reference Unit Condenser Air Flow Rate{ m3 / s }",
		"  , !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
		"  CoolCapFT, !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
		"  CoolCapFFF, !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
		"  COOLEIRFT, !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
		"  COOLEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",
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
		"  CoolCapFFF,       !- Name",
		"  0.8,                    !- Coefficient1 Constant",
		"  0.2,                    !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",
		"  ",
		"Curve:Quadratic,",
		"  COOLEIRFFF,           !- Name",
		"  1.1552,                 !- Coefficient1 Constant",
		"  -0.1808,                !- Coefficient2 x",
		"  0.0256,                 !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",
		"  ",
		"Curve:Quadratic,",
		"  PLFFPLR,          !- Name",
		"  0.85,                   !- Coefficient1 Constant",
		"  0.15,                   !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.0,                    !- Minimum Value of x",
		"  1.0;                    !- Maximum Value of x",
		"  ",
		"Curve:Biquadratic,",
		"  CoolCapFT,        !- Name",
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
		"  COOLEIRFT,            !- Name",
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

	ZoneEqSizing.allocate( 1 );
	ZoneEquipList( 1 ).EquipIndex.allocate( 1 );
	ZoneEquipList( 1 ).EquipIndex( 1 ) = 1; // initialize equipment index for ZoneHVAC

	GetUnitarySystemInput(); // get UnitarySystem input from object above
	HVACUnitarySystem::GetInputFlag = false; // don't call GetInput more than once (SimUnitarySystem call below will call GetInput if this flag is not set to false)

	ASSERT_EQ( 1, NumUnitarySystem ); // only 1 unitary system above so expect 1 as number of unitary system objects
	EXPECT_EQ( UnitarySystem( 1 ).UnitarySystemType, cFurnaceTypes( UnitarySystem( 1 ).UnitarySystemType_Num ) ); // compare UnitarySystem type string to valid type

	DataGlobals::SysSizingCalc = false; // DISABLE SIZING - don't call HVACUnitarySystem::SizeUnitarySystem, much more work needed to set up sizing arrays

	InletNode = UnitarySystem( 1 ).UnitarySystemInletNodeNum;
	OutletNode = UnitarySystem( 1 ).UnitarySystemOutletNodeNum;
	ControlZoneNum = UnitarySystem( 1 ).NodeNumOfControlledZone;

	// set up unitary system inlet condtions
	Node( InletNode ).Temp = 26.666667; // AHRI condition 80F dry-bulb temp
	Node( InletNode ).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
	Node( InletNode ).Enthalpy = PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat );

	// set zone temperature
	Node( ControlZoneNum ).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity
	DataEnvironment::OutDryBulbTemp = 35.0; // initialize weather
	DataEnvironment::OutHumRat = 0.1;
	DataEnvironment::OutBaroPress = 101325.0;
	DataEnvironment::OutWetBulbTemp = 30.0;

	// initialize other incidentals that are used within the UnitarySystem module during calculations
	CurZoneEqNum = 1;
	ZoneSysEnergyDemand.allocate( 1 );
	ZoneSysMoistureDemand.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired = 1000.0; // heating load
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP = 2000.0;
	ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP = 1000.0;
	ZoneSysMoistureDemand( ControlZoneNum ).OutputRequiredToDehumidifyingSP = 0.0;

	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP.allocate( 1 );
	ZoneSysMoistureDemand( ControlZoneNum ).SequencedOutputRequiredToDehumidSP.allocate( 1 );
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP;
	ZoneSysMoistureDemand( ControlZoneNum ).SequencedOutputRequiredToDehumidSP( 1 ) = ZoneSysMoistureDemand( ControlZoneNum ).OutputRequiredToDehumidifyingSP;

	TempControlType.allocate( 1 );
	TempControlType( 1 ) = DataHVACGlobals::DualSetPointWithDeadBand;
	CurDeadBandOrSetback.allocate( 1 );
	CurDeadBandOrSetback( 1 ) = false;
	Schedule( 1 ).CurrentValue = 1.0;
	DataGlobals::BeginEnvrnFlag = true;
	DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 ); // initialize RhoAir

	SetPredefinedTables();
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
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToCoolingSP;
	ZoneSysEnergyDemand( ControlZoneNum ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( ControlZoneNum ).OutputRequiredToHeatingSP;

	// set zone temperature
	Node( ControlZoneNum ).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
	DataEnvironment::OutDryBulbTemp = 35.0; // initialize weather
	DataEnvironment::OutHumRat = 0.1;
	DataEnvironment::OutBaroPress=101325.0;
	DataEnvironment::OutWetBulbTemp = 30.0;

	SimUnitarySystem( UnitarySystem( 1 ).Name, FirstHVACIteration, UnitarySystem( 1 ).ControlZoneNum, ZoneEquipList( 1 ).EquipIndex( 1 ), _, _, _, _, true );

	ZoneTemp = Node( ControlZoneNum ).Temp;
	CpAir = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );

	// calculation at end of CalcUnitarySystemToLoad():
	//	SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;

	MinHumRatio = Node( ControlZoneNum ).HumRat; // zone humidity ratio
	if ( Node( OutletNode ).Temp < Node( ControlZoneNum ).Temp ) MinHumRatio = Node( OutletNode ).HumRat; // use lower of zone and outlet humidity ratio
	Qsens_sys = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) );

	// test model performance
	EXPECT_NEAR( ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired, Qsens_sys, 1.0 ); // Watts
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, UnitarySystem( 1 ).CoolMassFlowRate( UnitarySystem( 1 ).CoolingSpeedNum ) );
	EXPECT_DOUBLE_EQ( Node( InletNode ).MassFlowRate, Node( OutletNode ).MassFlowRate );

}

TEST_F( EnergyPlusFixture, UnitarySystem_GetBadSupplyAirMethodInput ) {

	bool ErrorsFound( false );

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
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  Zone 2 Inlet Node,      !- Air Outlet Node Name",
		"  Fan:OnOff,              !- Supply Fan Object Type",
		"  Supply Fan 1,           !- Supply Fan Name",
		"  BlowThrough,            !- Fan Placement",
		"  FanAndCoilAvailSched,   !- Supply Air Fan Operating Mode Schedule Name",
		"  Coil:Heating:Gas,       !- Heating Coil Object Type",
		"  Furnace Heating Coil 1, !- Heating Coil Name",
		"  1,                      !- DX Heating Coil Sizing Ratio",
		"  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
		"  Furnace ACDXCoil 1,     !- Cooling Coil Name",
		"  No,                     !- Use DOAS DX Cooling Coil",
		"  2,                      !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
		"  SensibleOnlyLoadControl, !- Latent Load Control",
		"  ,                       !- Supplemental Heating Coil Object Type",
		"  ,                       !- Supplemental Heating Coil Name",
		"  ,                       !- Supply Air Flow Rate Method During Cooling Operation",
		"  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  ,                       !- Supply air Flow Rate Method During Heating Operation", // blank input not allowed with gas heating coil (no air flow input field)
		"  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  ,                       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
		"  ,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  80,                     !- Maximum Supply Air Temperature{ C }",
		"  21,                     !- Maximum Outdoor Dry - Bulb Temperature for Supplemental Heater Operation{ C }",
		"  ,                       !- Outdoor Dry - Bulb Temperature Sensor Node Name",
		"  2.5,                    !- Maximum Cycling Rate{ cycles / hr }",
		"  60,                     !- Heat Pump Time Constant{ s }",
		"  0.01,                   !- Fraction of On - Cycle Power Use",
		"  60,                     !- Heat Pump Fan Delay Time{ s }",
		"  ,                       !- Ancillary On - Cycle Electric Power{ W }",
		"  ,                       !- Ancillary Off - Cycle Electric Power{ W }",
		"  ,                       !- Design Heat Recovery Water Flow Rate{ m3 / s }",
		"  80;                     !- Maximum Temperature for Heat Recovery{ C }",
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
		"  Biquadratic,             !- Total Cooling Capacity Function of Temperature Curve Name",
		"  Quadratic,               !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"  Biquadratic,             !- Energy Input Ratio Function of Temperature Curve Name",
		"  Quadratic,               !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"  Quadratic,               !- Part Load Fraction Correlation Curve Name",
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
		"  Zone 2 Inlet Node;      !- Air Outlet Node Name",
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
		"Curve:Quadratic,",
		"  Quadratic,              !- Name",
		"  0.8,                    !- Coefficient1 Constant",
		"  0.2,                    !- Coefficient2 x",
		"  0.0,                    !- Coefficient3 x**2",
		"  0.5,                    !- Minimum Value of x",
		"  1.5;                    !- Maximum Value of x",
		"  ",
		"Curve:Biquadratic,",
		"  Biquadratic,            !- Name",
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
	} );

	ASSERT_FALSE( process_idf( idf_objects ) ); // read idf objects

	GetZoneData( ErrorsFound ); // read zone data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	GetZoneEquipmentData1(); // read zone equipment configuration and list objects

	HeatingCoils::GetCoilsInputFlag = true;
	HeatingCoils::HeatingCoil.deallocate();

	GetUnitarySystemInputData( ErrorsFound ); // get UnitarySystem input from object above
	EXPECT_TRUE( ErrorsFound ); // expect error on ill-formed input

}

TEST_F( EnergyPlusFixture, UnitarySystem_MultispeedDXCoilSizing ) {

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",

		"  Timestep,6;",

		"  Site:Location,",
		"    USA IL-CHICAGO-OHARE,    !- Name",
		"    41.77,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190;                     !- Elevation {m}",

		"SimulationControl, YES, NO, NO, YES, NO;",

		"  SizingPeriod:DesignDay,",
		"    CHICAGO Ann Clg .4% Condns WB=>MDB,  !- Name",
		"    7,                       !- Month",
		"    21,                      !- Day of Month",
		"    SummerDesignDay,         !- Day Type",
		"    31.2,                    !- Maximum Dry-Bulb Temperature {C}",
		"    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
		"    Wetbulb,                 !- Humidity Condition Type",
		"    25.5,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
		"    ,                        !- Humidity Condition Day Schedule Name",
		"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
		"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
		"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
		"    99063.,                  !- Barometric Pressure {Pa}",
		"    5.3,                     !- Wind Speed {m/s}",
		"    230,                     !- Wind Direction {deg}",
		"    No,                      !- Rain Indicator",
		"    No,                      !- Snow Indicator",
		"    No,                      !- Daylight Saving Time Indicator",
		"    ASHRAEClearSky,          !- Solar Model Indicator",
		"    ,                        !- Beam Solar Day Schedule Name",
		"    ,                        !- Diffuse Solar Day Schedule Name",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
		"    1.00;                    !- Sky Clearness",

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
		"  Multispeed DXAC,         !- Zone Equipment 1 Name",
		"  1,                       !- Zone Equipment 1 Cooling Sequence",
		"  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

		"AirLoopHVAC:UnitarySystem,",
		"  Multispeed DXAC,        !- Name",
		"  Load,                   !- Control Type",
		"  East Zone,              !- Controlling Zone or Thermostat Location",
		"  None,                   !- Dehumidification Control Type",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  Zone 2 Inlet Node,      !- Air Outlet Node Name",
		"  Fan:OnOff,              !- Supply Fan Object Type",
		"  Supply Fan 1,           !- Supply Fan Name",
		"  BlowThrough,            !- Fan Placement",
		"  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
		"  Coil:Heating:DX:VariableSpeed,       !- Heating Coil Object Type",
		"  VS Heating Coil 1,      !- Heating Coil Name",
		"  ,                       !- DX Heating Coil Sizing Ratio",
		"  Coil:Cooling:DX:MultiSpeed, !- Cooling Coil Object Type",
		"  MultiSpd Cooling Coil,  !- Cooling Coil Name",
		"  ,                       !- Use DOAS DX Cooling Coil",
		"  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
		"  ,                       !- Latent Load Control",
		"  Coil:Heating:Gas,       !- Supplemental Heating Coil Object Type",
		"  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
		"  FlowPerCoolingCapacity, !- Supply Air Flow Rate Method During Cooling Operation",
		"  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  FlowPerHeatingCapacity, !- Supply air Flow Rate Method During Heating Operation",
		"  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
		"  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
		"  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
		"  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
		"  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
		"  80,                     !- Maximum Supply Air Temperature{ C }",
        "  21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                        !- Maximum Cycling Rate {cycles/hr}",
        "  ,                        !- Heat Pump Time Constant {s}",
        "  ,                        !- Fraction of On-Cycle Power Use",
        "  ,                        !- Heat Pump Fan Delay Time {s}",
        "  ,                        !- Ancillary On-Cycle Electric Power {W}",
        "  ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "  ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                        !- Maximum Temperature for Heat Recovery {C}",
        "  ,                        !- Heat Recovery Water Inlet Node Name",
        "  ,                        !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Heat Pump Object Type",
        "  MultiSpeed Performance;  !- Design Specification Multispeed Heat Pump Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  MultiSpeed Performance,  !- Name",
        "  1,                       !- Number of Speeds for Heating",
        "  3,                       !- Number of Speeds for Cooling",
        "  autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 4 Supply Air Flow Ratio",
        "  autosize;                !- Cooling Speed 4 Supply Air Flow Ratio",

		"Fan:OnOff,",
		"  Supply Fan 1,           !- Name",
		"  FanAndCoilAvailSched,   !- Availability Schedule Name",
		"  0.7,                    !- Fan Total Efficiency",
		"  600.0,                  !- Pressure Rise{ Pa }",
		"  autosize,               !- Maximum Flow Rate{ m3 / s }",
		"  0.9,                    !- Motor Efficiency",
		"  1.0,                    !- Motor In Airstream Fraction",
		"  Zone Exhaust Node,      !- Air Inlet Node Name",
		"  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  MultiSpd Cooling Coil,   !- Name",
        ",                          !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  ,                        !- Condenser Air Inlet Node Name",
        "  AirCooled,               !- Condenser Type",
        "  ,                        !- Supply Water Storage Tank Name",
        "  ,                        !- Condensate Collection Water Storage Tank Name",
        "  No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "  0.0,                     !- Crankcase Heater Capacity {W}",
        "  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ,                        !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity,             !- Fuel Type",
        "  3,                       !- Number of Speeds",
        "  autosize,                !- Speed 1 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 1 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 1 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  0,                       !- Speed 1 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 1 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 2 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 2 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 2 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 2 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 2 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 3 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 3 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 3 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 3 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 3 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ;                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",

		"Coil:Heating:DX:VariableSpeed, ",
		"  VS Heating Coil 1,       !- Name",
		"  Heating Coil Air Inlet Node,  !- Indoor Air Inlet Node Name",
		"  Reheat Coil Air Inlet Node,  !- Indoor Air Outlet Node Name",
		"  10.0,                    !- Number of Speeds {dimensionless}",
		"  10.0,                    !- Nominal Speed Level {dimensionless}",
		"  autosize,                !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
		"  1.7,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
		"  HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
		"      ,                    !- Defrost Energy Input Ratio Function of Temperature Curve Name",
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
		"   0.245388,               !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
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
		"   6358.4,                 !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
		"  5.0,                     !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.2982408,             !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 7 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,         !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    7248.5,                !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
		"     0.3246672,            !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 8 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name",
		"    HPACHeatEIRFT,         !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    8133.6,                !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.3510936,             !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 9 Heating Capacity Function of Temperature Curve Name",
		"    HPACHeatCapFFF,        !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name",
		"     HPACHeatEIRFT,        !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
		"    HPACHeatEIRFFF,        !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
		"    9013.2,                !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
		"    5.0,                   !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
		"    0.37752,               !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
		"    HPACHeatCapFT,         !- Speed 10 Heating Capacity Function of Temperature Curve Name",
		"     HPACHeatCapFFF,       !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name",
 		"     HPACHeatEIRFT,        !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
		"      HPACHeatEIRFFF;      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

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
		"     -0.0000010693,            !- Coefficient3 x**2",
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

		"  Sizing:Zone,",
		"    EAST ZONE,      !- Zone or ZoneList Name",
		"    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
		"    12.8000,                 !- Zone Cooling Design Supply Air Temperature {C}",
		"    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
		"    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
		"    50.0000,                 !- Zone Heating Design Supply Air Temperature {C}",
		"    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
		"    0.0085,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    0.0080,                  !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    SZ DSOA Zone One,        !- Design Specification Outdoor Air Object Name",
		"    ,                        !- Zone Heating Sizing Factor",
		"    ,                        !- Zone Cooling Sizing Factor",
		"    DesignDay,               !- Cooling Design Air Flow Method",
		"    ,                        !- Cooling Design Air Flow Rate {m3/s}",
		"    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Cooling Minimum Air Flow {m3/s}",
		"    ,                        !- Cooling Minimum Air Flow Fraction",
		"    DesignDay,               !- Heating Design Air Flow Method",
		"    ,                        !- Heating Design Air Flow Rate {m3/s}",
		"    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Heating Maximum Air Flow {m3/s}",
		"    ;                        !- Heating Maximum Air Flow Fraction",

		"  DesignSpecification:OutdoorAir,",
		"    SZ DSOA Zone One,  !- Name",
		"    Flow/Person,             !- Outdoor Air Method",
		"    0.0125;                  !- Outdoor Air Flow per Person {m3/s-person}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall001,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"     ,                       !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall002,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall003,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall004,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Flr001,            !- Name",
		"    Floor,                   !- Surface Type",
		"    FLOOR,                   !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Adiabatic,               !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    NoSun,                   !- Sun Exposure",
		"    NoWind,                  !- Wind Exposure",
		"    1.000000,                !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Roof001,           !- Name",
		"    Roof,                    !- Surface Type",
		"    ROOF31,                  !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0,                       !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

		"  Construction,",
		"    R13WALL,                 !- Name",
		"    R13LAYER;                !- Outside Layer",

		"  Construction,",
		"   FLOOR,                   !- Name",
		"    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

		"  Construction,",
		"    ROOF31,                  !- Name",
		"    R31LAYER;                !- Outside Layer",

		"  Material:NoMass,",
		"    R13LAYER,                !- Name",
		"    Rough,                   !- Roughness",
		"    2.290965,                !- Thermal Resistance {m2-K/W}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.7500000,               !- Solar Absorptance",
		"    0.7500000;               !- Visible Absorptance",

		"  Material:NoMass,",
		"    R31LAYER,                !- Name",
		"    Rough,                   !- Roughness",
		"    5.456,                   !- Thermal Resistance {m2-K/W}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.7500000,               !- Solar Absorptance",
		"    0.7500000;               !- Visible Absorptance",

		"  Material,",
		"    C5 - 4 IN HW CONCRETE,   !- Name",
		"    MediumRough,             !- Roughness",
		"    0.1014984,               !- Thickness {m}",
		"    1.729577,                !- Conductivity {W/m-K}",
		"    2242.585,                !- Density {kg/m3}",
		"    836.8000,                !- Specific Heat {J/kg-K}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.6500000,               !- Solar Absorptance",
		"    0.6500000;               !- Visible Absorptance",

		"  ZoneControl:Thermostat,",
		"    EAST ZONE Thermostat,    !- Name",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		"    Zone One DualSPSched;    !- Control 1 Name",

		"  Schedule:Compact,",
		"    Dual Zone Control Type Sched,  !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,4;          !- Field 3",

		"  ThermostatSetpoint:DualSetpoint,",
		"    Zone One DualSPSched,    !- Name",
		"    HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
		"    CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

		"  Schedule:Compact,",
		"    CLGSETP_SCH,             !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 19",
		"    Until: 24:00,26.7;       !- Field 20",

		"  Schedule:Compact,",
		"    HTGSETP_SCH,             !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 22",
		"    Until: 24:00,15.6;       !- Field 23",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) ); // read idf objects

	SimulationManager::GetProjectData();

	DataGlobals::BeginSimFlag = true;
	DataGlobals::DoingSizing = true;
	SizingManager::ManageSizing();
	DataGlobals::DoingSizing = false;
	DataGlobals::SysSizingCalc = false;
	DataSizing::CurZoneEqNum = 1;
	DataSizing::ZoneEqSizing.allocate( 1 );

	GetUnitarySystemInput(); // get UnitarySystem input from object above
	HVACUnitarySystem::GetInputFlag = false; // don't call GetInput more than once

	OutputReportPredefined::SetPredefinedTables();

	ZoneSizingRunDone = true;
	ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
	ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

	SizeUnitarySystem( NumUnitarySystem, true, 0 );

	ASSERT_EQ( 1, NumUnitarySystem ); // only 1 unitary system above so expect 1 as number of unitary system objects

	ASSERT_EQ( UnitarySystem( 1 ).DesignHeatingCapacity * 0.00005, UnitarySystem( 1 ).MaxHeatAirVolFlow );
	ASSERT_EQ( UnitarySystem( 1 ).DesignCoolingCapacity * 0.00005, UnitarySystem( 1 ).MaxCoolAirVolFlow );
	ASSERT_EQ( UnitarySystem( 1 ).DesignCoolingCapacity, DXCoil( UnitarySystem( 1 ).CoolingCoilIndex ).MSRatedTotCap( UnitarySystem( 1 ).NumOfSpeedCooling ) );

}

