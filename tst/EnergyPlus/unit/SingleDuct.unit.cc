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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <General.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <HeatBalanceManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SingleDuct.hh>
#include <ZoneAirLoopEquipmentManager.hh>

using namespace EnergyPlus;


TEST_F( EnergyPlusFixture, VAVNoReheatTerminalUnitSchedule ) {
	std::string const idf_objects = delimited_string( {
		"Version,8.4;",
		"  Zone,",
		"    Zone 1;                !- Name",
		"ZoneHVAC:EquipmentConnections,",
		"    Zone 1,                !- Zone Name",
		"    Zone 1 Equipment,             !- Zone Conditioning Equipment List Name",
		"    Zone 1 Supply Inlet,       !- Zone Air Inlet Node or NodeList Name",
		"    ,      !- Zone Air Exhaust Node or NodeList Name",
		"    Zone 1 Air Node,           !- Zone Air Node Name",
		"    Zone 1 Return Node;       !- Zone Return Air Node Name",
		"ZoneHVAC:EquipmentList,",
		"    Zone 1 Equipment,             !- Name",
		"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
		"    Zone 1 ADU,            !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
		"ZoneHVAC:AirDistributionUnit,",
		"    Zone 1 ADU,    !- Name",
		"    Zone 1 Supply Inlet,     !- Air Distribution Unit Outlet Node Name",
		"    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"    Zone 1 VAV No Reheat;           !- Air Terminal Name",
		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"    Zone 1 VAV No Reheat,    !- Name",
		"    AlwaysOff,               !- Availability Schedule Name",
		"    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
		"    Zone 1 Zone Equip Inlet, !- Air Inlet Node Name",
		"    2.0,                     !- Maximum Air Flow Rate {m3/s}",
		"    Constant,                !- Zone Minimum Air Flow Input Method",
		"    0.5;                     !- Constant Minimum Air Flow Fraction",
		"Schedule:Constant,",
		"    AlwaysOff,               !- Name",
		"    ,                        !- Schedule Type Limits Name",
		"    0;                       !- Hourly Value",
		"Schedule:Constant,",
		"    AlwaysOn,               !- Name",
		"    ,                        !- Schedule Type Limits Name",
		"    1;                       !- Hourly Value",

	} ) ;

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules
	ScheduleManager::ScheduleInputProcessed = true;
	DataEnvironment::Month = 1;
	DataEnvironment::DayOfMonth = 21;
	DataGlobals::HourOfDay = 1;
	DataGlobals::TimeStep = 1;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 0;
	DataEnvironment::DayOfYear_Schedule = General::JulianDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
	DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 ); 
	ScheduleManager::UpdateScheduleValues();

	bool ErrorsFound = false;
	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);
	DataZoneEquipment::GetZoneEquipmentData1();
	ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
	SingleDuct::GetSysInput();
	EXPECT_TRUE( compare_err_stream( "" ) );
	DataHeatBalFanSys::TempControlType.allocate( 1 );
	DataHeatBalFanSys::TempControlType( 1 ) = DataHVACGlobals::DualSetPointWithDeadBand;

	// node number table
	//  1   Zone 1 Air Node
	//  2   Zone 1 Return Node
	//  3   Zone 1 Supply Inlet
	//  4   Zone 1 Zone Equip Inlet

	DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);

	// Setup for Zone 1 VAV No Reheat
	int SysNum = 1;
	int ZoneNum = 1;
	int ZoneNodeNum = 1;
	int InletNodeNum = 4;
	bool FirstHVACIteration = true;
	Real64 SysMinMassFlow = 1.0 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV
	Real64 SysMaxMassFlow = 2.0 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV

	// Test with heating load
	DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate

	// First test - AlwaysOff Schedule - expecting no flow
	SingleDuct::Sys( SysNum ).SchedPtr = 1;
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	DataGlobals::BeginEnvrnFlag = false;
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Second test - AlwaysOn Schedule - expecting flow
	// Reset flows and switch to AlwaysOn Schedule
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	SingleDuct::Sys( SysNum ).SchedPtr = 2;
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( SysMinMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Test with cooling load
	DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = -2000.0; // Cooling load - expect max flow rate

	// First test - AlwaysOff Schedule - expecting no flow
	SingleDuct::Sys( SysNum ).SchedPtr = 1;
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	DataGlobals::BeginEnvrnFlag = false;
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Second test - AlwaysOn Schedule - expecting flow
	// Reset flows and switch to AlwaysOn Schedule
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	SingleDuct::Sys( SysNum ).SchedPtr = 2;
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Cleanup
	DataHeatBalFanSys::TempControlType.deallocate();
	DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();

}

TEST_F( EnergyPlusFixture, VAVReheatTerminalUnitSchedule ) {
	std::string const idf_objects = delimited_string( {
		"Version,8.4;",
		"  Zone,",
		"    Zone 1;                !- Name",
		"ZoneHVAC:EquipmentConnections,",
		"    Zone 1,                !- Zone Name",
		"    Zone 1 Equipment,             !- Zone Conditioning Equipment List Name",
		"    Zone 1 Supply Inlet,       !- Zone Air Inlet Node or NodeList Name",
		"    ,      !- Zone Air Exhaust Node or NodeList Name",
		"    Zone 1 Air Node,           !- Zone Air Node Name",
		"    Zone 1 Return Node;       !- Zone Return Air Node Name",
		"ZoneHVAC:EquipmentList,",
		"    Zone 1 Equipment,             !- Name",
		"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
		"    Zone 1 ADU,            !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
		"ZoneHVAC:AirDistributionUnit,",
		"    Zone 1 ADU,    !- Name",
		"    Zone 1 Supply Inlet,     !- Air Distribution Unit Outlet Node Name",
		"    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
		"    Zone 1 VAV Reheat;           !- Air Terminal Name",
		"AirTerminal:SingleDuct:VAV:Reheat,",
		"    Zone 1 VAV Reheat,       !- Name",
		"    AlwaysOff,               !- Availability Schedule Name",
		"    Zone 1 VAV Reheat Coil Air Inlet,  !- Damper Air Outlet Node Name",
		"    Zone 1 Zone Equip Inlet, !- Air Inlet Node Name",
		"    1.0,                     !- Maximum Air Flow Rate {m3/s}",
		"    Constant,                !- Zone Minimum Air Flow Input Method",
		"    0.25,                     !- Constant Minimum Air Flow Fraction",
		"    ,                        !- Fixed Minimum Air Flow Rate{m3/s}",
		"    ,                        !- Minimum Air Flow Fraction Schedule Name",
		"    Coil:Heating:Electric,   !- Reheat Coil Object Type",
		"    Zone 1 Reheat Coil,      !- Reheat Coil Name",
		"    ,                        !- Maximum Hot Water or Steam Flow Rate{m3/s}",
		"    ,                        !- Minimum Hot Water or Steam Flow Rate{m3/s}",
		"    Zone 1 Supply Inlet;     !- Air Outlet Node Name",
		"Coil:Heating:Electric,",
		"    Zone 1 Reheat Coil,      !- Name",
		"    ,                        !- Availability Schedule Name",
		"    1,                       !- Efficiency",
		"    100,                     !- Nominal Capacity of the Coil {W}",
		"    Zone 1 VAV Reheat Coil Air Inlet,  !- Air Inlet Node Name",
		"    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
		"    ;                        !- Temperature Setpoint Node Name",
		"Schedule:Constant,",
		"    AlwaysOff,               !- Name",
		"    ,                        !- Schedule Type Limits Name",
		"    0;                       !- Hourly Value",
		"Schedule:Constant,",
		"    AlwaysOn,               !- Name",
		"    ,                        !- Schedule Type Limits Name",
		"    1;                       !- Hourly Value",

	} ) ;

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules
	ScheduleManager::ScheduleInputProcessed = true;
	DataEnvironment::Month = 1;
	DataEnvironment::DayOfMonth = 21;
	DataGlobals::HourOfDay = 1;
	DataGlobals::TimeStep = 1;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 0;
	DataEnvironment::DayOfYear_Schedule = General::JulianDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
	DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 ); 
	ScheduleManager::UpdateScheduleValues();

	bool ErrorsFound = false;
	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);
	DataZoneEquipment::GetZoneEquipmentData1();
	ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
	SingleDuct::GetSysInput();
	EXPECT_TRUE( compare_err_stream( "" ) );
	DataHeatBalFanSys::TempControlType.allocate( 1 );
	DataHeatBalFanSys::TempControlType( 1 ) = DataHVACGlobals::DualSetPointWithDeadBand;

	// node number table
	//  1   Zone 1 Air Node
	//  2   Zone 1 Return Node
	//  3   Zone 1 Supply Inlet
	//  4   Zone 1 VAV Reheat Coil Air Inlet
	//  5   Zone 1 Zone Equip Inlet

	DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);

	// Setup for Zone 1 VAV No Reheat
	int SysNum = 1;
	int ZoneNum = 1;
	int ZoneNodeNum = 1;
	int InletNodeNum = 5;
	bool FirstHVACIteration = true;
	Real64 SysMinMassFlow = 0.25 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV
	Real64 SysMaxMassFlow = 1.0 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV

	// Test with heating load
	DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate

	// First test - AlwaysOff Schedule - expecting no flow
	SingleDuct::Sys( SysNum ).SchedPtr = 1;
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	DataGlobals::BeginEnvrnFlag = false;
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Second test - AlwaysOn Schedule - expecting flow
	// Reset flows and switch to AlwaysOn Schedule
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	SingleDuct::Sys( SysNum ).SchedPtr = 2;
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( SysMinMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Test with cooling load
	DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = -2000.0; // Cooling load - expect max flow rate

	// First test - AlwaysOff Schedule - expecting no flow
	SingleDuct::Sys( SysNum ).SchedPtr = 1;
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	DataGlobals::BeginEnvrnFlag = false;
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( 0.0, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Second test - AlwaysOn Schedule - expecting flow
	// Reset flows and switch to AlwaysOn Schedule
	DataLoopNode::Node( InletNodeNum ).MassFlowRate = SysMinMassFlow;
	DataLoopNode::Node( InletNodeNum ).MassFlowRateMaxAvail = SysMaxMassFlow;
	SingleDuct::Sys( SysNum ).SchedPtr = 2;
	FirstHVACIteration = true;
	SingleDuct::InitSys( SysNum, FirstHVACIteration ); // Run thru init once with FirstHVACIteration set to true
	FirstHVACIteration = false;
	SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
	SingleDuct::SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRateMaxAvail );
	EXPECT_EQ( SysMaxMassFlow, SingleDuct::SysOutlet( SysNum ).AirMassFlowRate );

	// Cleanup
	DataHeatBalFanSys::TempControlType.deallocate();
	DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();

}
