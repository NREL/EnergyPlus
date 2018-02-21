// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus::AirTerminal SingleDuct Unit Tests
// AirTerminal:SingleDuct:ConstantVolume:NoReheat

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

#include <EMSManager.hh>
#include <DataRuntimeLanguage.hh>

// EnergyPlus Headers
using namespace EnergyPlus::DataDefineEquip;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;

using namespace EnergyPlus::EMSManager;
using namespace EnergyPlus::DataRuntimeLanguage;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctCVNoReheat_GetInput ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string({
			"Version,8.8;",
			"  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
			"    SDCVNoReheatAT1,         !- Name",
			"    AvailSchedule,           !- Availability Schedule Name",
			"    Zone1NoReheatAirInletNode,   !- Air Inlet Node Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Outlet Node Name",
			"    0.50;                    !- Maximum Air Flow Rate {m3/s}",

			"  Schedule:Compact,",
			"    AvailSchedule,           !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    SequentialLoad,          !- Load Distribution Scheme",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SDCVNoReheatADU1,        !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SDCVNoReheatADU1,        !- Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
			"    SDCVNoReheatAT1;         !- Air Terminal Name",

			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.40,                    !- Ceiling Height {m}",
			"    240.0;                   !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    West Zone,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

			"  NodeList,",
			"    Zone1Inlets,             !- Name",
			"    Zone1NoReheatAirOutletNode;   !- Node 1 Name",

		});

		ASSERT_TRUE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();

		EXPECT_EQ( "AirTerminal:SingleDuct:ConstantVolume:NoReheat", Sys( 1 ).SysType ); // AT SD constant volume no reheat object type
		EXPECT_EQ( "SDCVNOREHEATAT1", Sys( 1 ).SysName ); // AT SD constant volume no reheat name
		EXPECT_EQ( "AVAILSCHEDULE", Sys( 1 ).Schedule ); // AT SD constant volume no reheat availability schedule name
		EXPECT_EQ( 0.50, Sys( 1 ).MaxAirVolFlowRate ); // maximum volume flow Rate
		ASSERT_TRUE( Sys( 1 ).NoOAFlowInputFromUser ); // no OA flow input from user
		EXPECT_EQ( DataZoneEquipment::PerPersonDCVByCurrentLevel, Sys( 1 ).OAPerPersonMode ); // default value when A6 input field is blank

	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctCVNoReheat_SimConstVolNoReheat ) {

		bool ErrorsFound( false );
		bool FirstHVACIteration( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.8;",
			"  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
			"    SDCVNoReheatAT1,         !- Name",
			"    AvailSchedule,           !- Availability Schedule Name",
			"    Zone1NoReheatAirInletNode,   !- Air Inlet Node Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Outlet Node Name",
			"    1.0;                    !- Maximum Air Flow Rate {m3/s}",

			"  Schedule:Compact,",
			"    AvailSchedule,           !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    SequentialLoad,          !- Load Distribution Scheme",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SDCVNoReheatADU1,        !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SDCVNoReheatADU1,        !- Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
			"    SDCVNoReheatAT1;         !- Air Terminal Name",

			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.40,                    !- Ceiling Height {m}",
			"    240.0;                   !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    West Zone,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

			"  NodeList,",
			"    Zone1Inlets,             !- Name",
			"    Zone1NoReheatAirOutletNode;   !- Node 1 Name",

		} );

		ASSERT_TRUE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();
		DataEnvironment::StdRhoAir = 1.0;
		//FirstHVACIteration = false;
		int const SysNum( 1 );
		Real64 MassFlowRateMaxAvail = Sys( SysNum ).MaxAirVolFlowRate * DataEnvironment::StdRhoAir;
		SysInlet( SysNum ).AirMassFlowRate = MassFlowRateMaxAvail;
		Schedule( Sys( SysNum ).SchedPtr ).CurrentValue = 1.0; // unit is always available
		int const ZonePtr = Sys( SysNum ).ActualZoneNum;
		int const ZoneAirNodeNum = ZoneEquipConfig( ZonePtr ).ZoneNode;
		// run SimConstVolNoReheat() function
		Sys( SysNum ).SimConstVolNoReheat( SysNum, FirstHVACIteration, ZonePtr, ZoneAirNodeNum );
		// check the TA outlet air mass flow rate
		EXPECT_EQ( MassFlowRateMaxAvail, SysOutlet( SysNum ).AirMassFlowRate );
	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctCVNoReheat_Sim ) {

		bool ErrorsFound( false );
		bool FirstHVACIteration( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.8;",
			"  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
			"    SDCVNoReheatAT1,         !- Name",
			"    AvailSchedule,           !- Availability Schedule Name",
			"    Zone1NoReheatAirInletNode,   !- Air Inlet Node Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Outlet Node Name",
			"    1.0;                    !- Maximum Air Flow Rate {m3/s}",

			"  Schedule:Compact,",
			"    AvailSchedule,           !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    SequentialLoad,          !- Load Distribution Scheme",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SDCVNoReheatADU1,        !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SDCVNoReheatADU1,        !- Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
			"    SDCVNoReheatAT1;         !- Air Terminal Name",

			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.40,                    !- Ceiling Height {m}",
			"    240.0;                   !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    West Zone,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

			"  NodeList,",
			"    Zone1Inlets,             !- Name",
			"    Zone1NoReheatAirOutletNode;   !- Node 1 Name",

		} );

		ASSERT_TRUE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();

		DataGlobals::SysSizingCalc = true;
		DataGlobals::BeginEnvrnFlag = true;
		DataEnvironment::StdRhoAir = 1.0;
		DataEnvironment::OutBaroPress = 101325.0;

		int const SysNum( 1 );
		int const InletNode = Sys( SysNum ).InletNodeNum;
		int const ZonePtr = Sys( SysNum ).ActualZoneNum;
		int const ZoneAirNodeNum = ZoneEquipConfig( ZonePtr ).ZoneNode;
		Schedule( Sys( SysNum ).SchedPtr ).CurrentValue = 1.0; // unit is always available

		// design maximum air mass flow rate
		Real64 MassFlowRateMaxAvail = Sys( SysNum ).MaxAirVolFlowRate * DataEnvironment::StdRhoAir;
		EXPECT_EQ( 1.0, Sys( SysNum ).MaxAirVolFlowRate );
		EXPECT_EQ( 1.0, MassFlowRateMaxAvail );

		// set air inlet node properties
		Node( InletNode ).Temp = 50.0;
		Node( InletNode ).HumRat = 0.0075;
		Node( InletNode ).Enthalpy = Psychrometrics::PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat );;
		// set zone air node properties
		Node( ZoneAirNodeNum ).Temp = 20.0;
		Node( ZoneAirNodeNum ).HumRat = 0.0075;
		Node( ZoneAirNodeNum ).Enthalpy = Psychrometrics::PsyHFnTdbW( Node( ZoneAirNodeNum ).Temp, Node( ZoneAirNodeNum ).HumRat );;
		// calculate the heating rate provided by TA unit
		Real64 CpAir = PsyCpAirFnWTdb( 0.5 * ( Node( InletNode ).HumRat + Node( ZoneAirNodeNum ).HumRat), 0.5 * ( Node( InletNode ).Temp + Node( ZoneAirNodeNum ).Temp) );
		Real64 SensHeatRateProvided = MassFlowRateMaxAvail * CpAir * ( Node( InletNode ).Temp - Node( ZoneAirNodeNum ).Temp);

		// set inlet mass flow rate to zero
		Node( InletNode ).MassFlowRateMaxAvail = 0.0;
		FirstHVACIteration = true;
		SingleDuct::GetInputFlag = false;
		// run SimulateSingleDuct() function
		SimulateSingleDuct( AirDistUnit( 1 ).EquipName( 1 ), FirstHVACIteration, ZonePtr, ZoneAirNodeNum, AirDistUnit( 1 ).EquipIndex( 1 ) );
		// check AT air mass flow rates
		EXPECT_EQ( MassFlowRateMaxAvail, Sys( SysNum ).AirMassFlowRateMax ); // design maximum mass flow rate
		EXPECT_EQ( 0.0, SysInlet( SysNum ).AirMassFlowRateMaxAvail ); // maximum available mass flow rate
		EXPECT_EQ( 0.0, SysInlet( SysNum ).AirMassFlowRate ); // outlet mass flow rate is zero
		EXPECT_EQ( 0.0, SysOutlet( SysNum ).AirMassFlowRate ); // outlet mass flow rate is zero
		EXPECT_EQ( 0.0, Sys( SysNum ).HeatRate ); // delivered heat rate is zero

		FirstHVACIteration = false;
		Node( InletNode ).MassFlowRateMaxAvail = MassFlowRateMaxAvail;
		EXPECT_EQ( 1.0, MassFlowRateMaxAvail );
		// run SimulateSingleDuct() function
		SimulateSingleDuct( AirDistUnit( 1 ).EquipName( 1 ), FirstHVACIteration, ZonePtr, ZoneAirNodeNum, AirDistUnit( 1 ).EquipIndex( 1 ) );
		// check AT air mass flow rates
		EXPECT_EQ( MassFlowRateMaxAvail, SysInlet( SysNum ).AirMassFlowRate );
		EXPECT_EQ( MassFlowRateMaxAvail, SysOutlet( SysNum ).AirMassFlowRate );
		// check heating rate delivered
		EXPECT_NEAR( SensHeatRateProvided, Sys( SysNum ).HeatRate, 0.001 );
		// outlet and inlet nodes air conditions must match exactly
		EXPECT_EQ( SysOutlet( SysNum ).AirTemp, SysInlet( SysNum ).AirTemp );
		EXPECT_EQ( SysOutlet( SysNum ).AirHumRat, SysInlet( SysNum ).AirHumRat );
		EXPECT_EQ( SysOutlet( SysNum ).AirEnthalpy, SysInlet( SysNum ).AirEnthalpy );
		EXPECT_EQ( SysOutlet( SysNum ).AirMassFlowRate, SysInlet( SysNum ).AirMassFlowRate );
	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctCVNoReheat_EMSOverrideAirFlow ) {

		bool ErrorsFound( false );
		bool FirstHVACIteration( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.8;",

			"  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
			"    SDCVNoReheatAT1,         !- Name",
			"    AvailSchedule,           !- Availability Schedule Name",
			"    Zone1NoReheatAirInletNode,   !- Air Inlet Node Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Outlet Node Name",
			"    1.0;                    !- Maximum Air Flow Rate {m3/s}",

			"  Schedule:Compact,",
			"    AvailSchedule,           !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    SequentialLoad,          !- Load Distribution Scheme",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SDCVNoReheatADU1,        !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SDCVNoReheatADU1,        !- Name",
			"    Zone1NoReheatAirOutletNode,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
			"    SDCVNoReheatAT1;         !- Air Terminal Name",

			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.40,                    !- Ceiling Height {m}",
			"    240.0;                   !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    West Zone,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

			"  NodeList,",
			"    Zone1Inlets,             !- Name",
			"    Zone1NoReheatAirOutletNode;   !- Node 1 Name",
		} );

		ASSERT_TRUE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();

		DataGlobals::SysSizingCalc = true;
		DataGlobals::BeginEnvrnFlag = true;
		DataEnvironment::StdRhoAir = 1.0;
		DataEnvironment::OutBaroPress = 101325.0;

		int const SysNum( 1 );
		int const InletNode = Sys( SysNum ).InletNodeNum;
		int const ZonePtr = Sys( SysNum ).ActualZoneNum;
		int const ZoneAirNodeNum = ZoneEquipConfig( ZonePtr ).ZoneNode;
		Schedule( Sys( SysNum ).SchedPtr ).CurrentValue = 1.0; // unit is always available
		// design maximum air mass flow rate
		Real64 MassFlowRateMaxAvail = Sys( SysNum ).MaxAirVolFlowRate * DataEnvironment::StdRhoAir;
		EXPECT_EQ( 1.0, Sys( SysNum ).MaxAirVolFlowRate );
		EXPECT_EQ( 1.0, MassFlowRateMaxAvail );

		// set air inlet node properties
		Node( InletNode ).Temp = 50.0;
		Node( InletNode ).HumRat = 0.0075;
		Node( InletNode ).Enthalpy = Psychrometrics::PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat );;
		// set zone air node properties
		Node( ZoneAirNodeNum ).Temp = 20.0;
		Node( ZoneAirNodeNum ).HumRat = 0.0075;
		Node( ZoneAirNodeNum ).Enthalpy = Psychrometrics::PsyHFnTdbW( Node( ZoneAirNodeNum ).Temp, Node( ZoneAirNodeNum ).HumRat );;
		SingleDuct::GetInputFlag = false;
		FirstHVACIteration = false;
		Node( InletNode ).MassFlowRateMaxAvail = MassFlowRateMaxAvail;
		EXPECT_EQ( 1.0, MassFlowRateMaxAvail );
		// run SimulateSingleDuct() function
		SimulateSingleDuct( AirDistUnit( 1 ).EquipName( 1 ), FirstHVACIteration, ZonePtr, ZoneAirNodeNum, AirDistUnit( 1 ).EquipIndex( 1 ) );
		// check AT air mass flow rates
		EXPECT_EQ( MassFlowRateMaxAvail, SysInlet( SysNum ).AirMassFlowRate );
		EXPECT_EQ( MassFlowRateMaxAvail, SysOutlet( SysNum ).AirMassFlowRate );
		// outlet and inlet nodes air conditions must match exactly
		EXPECT_EQ( SysOutlet( SysNum ).AirTemp, SysInlet( SysNum ).AirTemp );
		EXPECT_EQ( SysOutlet( SysNum ).AirHumRat, SysInlet( SysNum ).AirHumRat );
		EXPECT_EQ( SysOutlet( SysNum ).AirEnthalpy, SysInlet( SysNum ).AirEnthalpy );
		EXPECT_EQ( SysOutlet( SysNum ).AirMassFlowRate, SysInlet( SysNum ).AirMassFlowRate );
		// sets EMS actuators
		Sys( SysNum ).EMSOverrideAirFlow = true;
		Sys( SysNum ).EMSMassFlowRateValue = 0.5;
		// run SimulateSingleDuct() function
		SimulateSingleDuct( AirDistUnit( 1 ).EquipName( 1 ), FirstHVACIteration, ZonePtr, ZoneAirNodeNum, AirDistUnit( 1 ).EquipIndex( 1 ) );
		// check AT air mass flow rates
		EXPECT_EQ( Sys( SysNum ).EMSMassFlowRateValue, SysInlet( SysNum ).AirMassFlowRate );
		EXPECT_EQ( Sys( SysNum ).EMSMassFlowRateValue, SysOutlet( SysNum ).AirMassFlowRate );

	}
}
