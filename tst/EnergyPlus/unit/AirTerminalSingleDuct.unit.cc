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

// EnergyPlus::AirTerminal SingleDuct Unit Tests
// AirTerminal:SingleDuct:ConstantVolume:Reheat
// AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction
// AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat
// AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

#include "Fixtures/EnergyPlusFixture.hh"


#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

// EnergyPlus Headers
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACSingleDuctInduc;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctCVReheat_GetInputTest ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string({
			"Version,8.4;",
			"  AirTerminal:SingleDuct:ConstantVolume:Reheat,",
			"    Reheat Zone 1,           !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    Zone 1 Reheat Air Outlet Node,  !- Air Outlet Node Name",
			"    Zone 1 Reheat Air Inlet Node,  !- Air Inlet Node Name",
			"    0.47,                    !- Maximum Air Flow Rate {m3/s}",
			"    Coil:Heating:Water,      !- Reheat Coil Object Type",
			"    Reheat Coil Zone 1,      !- Reheat Coil Name",
			"    0.0013,                  !- Maximum Hot Water or Steam Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
			"    0.001;                   !- Convergence Tolerance",
						
			"  Coil:Heating:Water,",
			"    Reheat Coil Zone 1,      !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    400.0,                   !- U-Factor Times Area Value {W/K}",
			"    0.0013,                  !- Maximum Water Flow Rate {m3/s}",
			"    Zone 1 Reheat Water Inlet Node,  !- Water Inlet Node Name",
			"    Zone 1 Reheat Water Outlet Node,  !- Water Outlet Node Name",
			"    Zone 1 Reheat Air Inlet Node,  !- Air Inlet Node Name",
			"    Zone 1 Reheat Air Outlet Node,  !- Air Outlet Node Name",
			"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
			"    autosize,                !- Rated Capacity {W}",
			"    82.2,                    !- Rated Inlet Water Temperature {C}",
			"    16.6,                    !- Rated Inlet Air Temperature {C}",
			"    71.1,                    !- Rated Outlet Water Temperature {C}",
			"    32.2,                    !- Rated Outlet Air Temperature {C}",
			"    ;                        !- Rated Ratio for Air and Water Convection",
												
			"  Schedule:Compact,",
			"    FanAndCoilAvailSched,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,           !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",
			
			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    Zone1TermReheat,         !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    Zone1TermReheat,         !- Name",
			"    Zone 1 Reheat Air Outlet Node,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Air Terminal Object Type",
			"    Reheat Zone 1;           !- Air Terminal Name",

			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    West Zone,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();
		
		EXPECT_EQ( "AirTerminal:SingleDuct:ConstantVolume:Reheat", Sys( 1 ).SysType ); // AT SD VAV Reheat Type
		EXPECT_EQ( "REHEAT ZONE 1", Sys( 1 ).SysName ); // AT SD VAV Reheat Name
		EXPECT_GT( Sys( 1 ).ReheatControlNode, 0 );  // none zero integer node index is expected

	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuct4PipeInduction_GetInputTest ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.4;",
			"  AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction,",
			"    SPACE1-1 FPIU,           !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- Maximum Total Air Flow Rate {m3/s}",
			"    1.0,                     !- Induction Ratio",
			"    SPACE1-1 ATU Supply Node,!- Supply Air Inlet Node Name",
			"    SPACE1-1 ATU Induc Node, !- Induced Air Inlet Node Name",
			"    SPACE1-1 In Node,        !- Air Outlet Node Name",
			"    Coil:Heating:Water,      !- Heating Coil Object Type",
			"    SPACE1-1 HW Coil,        !- Heating Coil Name",
			"    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Hot Water Flow Rate {m3/s}",
			"    0.002,                   !- Heating Convergence Tolerance",
			"    Coil:Cooling:Water,      !- Cooling Coil Object Type",
			"    SPACE1-1 CW Coil,        !- Cooling Coil Name",
			"    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Cold Water Flow Rate {m3/s}",
			"    0.002,                   !- Cooling Convergence Tolerance",
			"    SPACE1-1 ATU Mixer;      !- Zone Mixer Name",

			"  Coil:Heating:Water,",
			"    SPACE1-1 HW Coil,        !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- U-Factor Times Area Value {W/K}",
			"    autosize,                !- Maximum Water Flow Rate {m3/s}",
			"    SPACE1-1 HW Coil Water In Node,  !- Water Inlet Node Name",
			"    SPACE1-1 HW Coil Water Out Node,  !- Water Outlet Node Name",
			"    SPACE1-1 ATU Induc Node, !- Air Inlet Node Name",
			"    SPACE1-1 HW Coil Air Out Node,  !- Air Outlet Node Name",
			"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
			"    autosize,                !- Rated Capacity {W}",
			"    82.2,                    !- Rated Inlet Water Temperature {C}",
			"    16.6,                    !- Rated Inlet Air Temperature {C}",
			"    71.1,                    !- Rated Outlet Water Temperature {C}",
			"    32.2,                    !- Rated Outlet Air Temperature {C}",
			"    ;                        !- Rated Ratio for Air and Water Convection",

			"  Coil:Cooling:Water,",
			"    SPACE1-1 CW Coil,        !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- Design Water Flow Rate {m3/s}",
			"    autosize,                !- Design Air Flow Rate {m3/s}",
			"    autosize,                !- Design Inlet Water Temperature {C}",
			"    autosize,                !- Design Inlet Air Temperature {C}",
			"    autosize,                !- Design Outlet Air Temperature {C}",
			"    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
			"    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
			"    SPACE1-1 CW Coil Water In Node,  !- Water Inlet Node Name",
			"    SPACE1-1 CW Coil Water Out Node,  !- Water Outlet Node Name",
			"    SPACE1-1 HW Coil Air Out Node,  !- Air Inlet Node Name",
			"    SPACE1-1 CW Coil Air Out Node,  !- Air Outlet Node Name",
			"    SimpleAnalysis,          !- Type of Analysis",
			"    CrossFlow;               !- Heat Exchanger Configuration",

			"  AirLoopHVAC:ZoneMixer,",
			"    SPACE1-1 ATU Mixer,      !- Name",
			"    SPACE1-1 In Node,        !- Outlet Node Name",
			"    SPACE1-1 ATU Supply Node,!- Inlet 1 Node Name",
			"    SPACE1-1 CW Coil Air Out Node;  !- Inlet 2 Node Name",

			"  Schedule:Compact,",
			"    FanAndCoilAvailSched,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,           !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    SPACE1-1 Eq,             !- Name",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SPACE1-1 ATU,            !- Name",
			"    SPACE1-1 In Node,        !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction,  !- Air Terminal Object Type",
			"    SPACE1-1 FPIU;           !- Air Terminal Name",

			"  Zone,",
			"    SPACE1-1,                !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.438400269,             !- Ceiling Height {m}",
			"    239.247360229;           !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    SPACE1-1,                !- Zone Name",
			"    SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"    SPACE1-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
			"    SPACE1-1 Exh Nodes,      !- Zone Air Exhaust Node or NodeList Name",
			"    SPACE1-1 Node,           !- Zone Air Node Name",
			"    SPACE1-1 Out Node;       !- Zone Return Air Node Name",

			"  NodeList,",
			"    SPACE1-1 In Nodes,       !- Name",
			"    SPACE1-1 In Node;        !- Node 1 Name",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetIndUnits();

		EXPECT_EQ( "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction", IndUnit( 1 ).UnitType ); // AT SD VAV Reheat Type
		EXPECT_EQ( "SPACE1-1 FPIU", IndUnit( 1 ).Name ); // AT SD VAV Reheat Name
		EXPECT_GT( IndUnit( 1 ).HWControlNode, 0 );  // none zero integer node index is expected
		EXPECT_GT( IndUnit( 1 ).CWControlNode, 0 );  // none zero integer node index is expected
	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctVAVHeatCool_GetInputTest ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.4;",
			"  AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat,",
			"    Zone 1 VAV System,       !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    Zone 1 Reheat Air Inlet Node,  !- Damper Air Outlet Node Name",
			"    Zone 1 VAV Inlet Node,   !- Air Inlet Node Name",
			"    autosize,                !- Maximum Air Flow Rate {m3/s}",
			"    0.25,                    !- Zone Minimum Air Flow Fraction",
			"    Coil:Heating:Electric,   !- Reheat Coil Object Type",
			"    Reheat Coil Zone 1,      !- Reheat Coil Name",
			"    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
			"    Zone 1 Reheat Air Outlet Node,  !- Air Outlet Node Name",
			"    0.001;                   !- Convergence Tolerance",

			"  Coil:Heating:Electric,",
			"    Reheat Coil Zone 1,      !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    1.0,                     !- Efficiency",
			"    autosize,                !- Nominal Capacity {W}",
			"    Zone 1 Reheat Air Inlet Node,  !- Air Inlet Node Name",
			"    Zone 1 Reheat Air Outlet Node;  !- Air Outlet Node Name",

			"  Schedule:Compact,",
			"    FanAndCoilAvailSched,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,           !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    Zone1Equipment,          !- Name",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    Zone1TermReheat,         !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    Zone1TermReheat,         !- Name",
			"    Zone 1 Reheat Air Outlet Node,  !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat,  !- Air Terminal Object Type",
			"    Zone 1 VAV System;       !- Air Terminal Name",

			"  Zone,",
			"    WEST ZONE,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    WEST ZONE,               !- Zone Name",
			"    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
			"    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Zone 1 Node,             !- Zone Air Node Name",
			"    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();

		EXPECT_EQ( "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat", Sys( 1 ).SysType ); // AT SD VAV HeatCool Reheat Type
		EXPECT_EQ( "ZONE 1 VAV SYSTEM", Sys( 1 ).SysName ); // AT SD VAV HeatCool Reheat Name
		EXPECT_EQ( "COIL:HEATING:ELECTRIC", Sys( 1 ).ReheatComp ); // Reheat Coil Type
		EXPECT_EQ( "REHEAT COIL ZONE 1", Sys( 1 ).ReheatName ); // Reheat Coil Name
	}

	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctVAVReheatVarSpeedFan_GetInputTest ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.4;",
			"  AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan,",
			"    SPACE1-1 VAV Reheat,     !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}",
			"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}",
			"    0.05,                    !- Zone Minimum Air Flow Fraction",
			"    SPACE1 - 1 ATU In Node, !- Air Inlet Node Name",
			"    SPACE1 - 1 In Node, !- Air Outlet Node Name",
			"    Fan:VariableVolume,      !- Fan Object Type",
			"    SPACE1-1 Zone Fan,       !- Fan Name",
			"    Coil:Heating:Water,      !- Heating Coil Object Type",
			"    SPACE1-1 Zone Coil,      !- Heating Coil Name",
			"    autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
			"    0.001;                   !- Heating Convergence Tolerance",

			"  Fan:VariableVolume,",
			"    SPACE1-1 Zone Fan,       !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    0.7,                     !- Fan Total Efficiency",
			"    125.0,                   !- Pressure Rise {Pa}",
			"    autosize,                !- Maximum Flow Rate {m3/s}",
			"    FixedFlowRate,           !- Fan Power Minimum Flow Rate Input Method",
			"    ,                        !- Fan Power Minimum Flow Fraction",
			"    0.0,                     !- Fan Power Minimum Air Flow Rate {m3/s}",
			"    0.9,                     !- Motor Efficiency",
			"    1.0,                     !- Motor In Airstream Fraction",
			"    0.00153028,              !- Fan Power Coefficient 1",
			"    0.00520806,              !- Fan Power Coefficient 2",
			"    1.1086242,               !- Fan Power Coefficient 3",
			"    -.11635563,              !- Fan Power Coefficient 4",
			"    0.000,                   !- Fan Power Coefficient 5",
			"    SPACE1-1 ATU In Node,    !- Air Inlet Node Name",
			"    SPACE1-1 Zone Coil Air In Node;  !- Air Outlet Node Name",

			"  Coil:Heating:Water,",
			"    SPACE1-1 Zone Coil,      !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- U-Factor Times Area Value {W/K}",
			"    autosize,                !- Maximum Water Flow Rate {m3/s}",
			"    SPACE1-1 Zone Coil Water In Node,  !- Water Inlet Node Name",
			"    SPACE1-1 Zone Coil Water Out Node,  !- Water Outlet Node Name",
			"    SPACE1-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
			"    SPACE1-1 In Node,        !- Air Outlet Node Name",
			"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
			"    autosize,                !- Rated Capacity {W}",
			"    82.2,                    !- Rated Inlet Water Temperature {C}",
			"    16.6,                    !- Rated Inlet Air Temperature {C}",
			"    71.1,                    !- Rated Outlet Water Temperature {C}",
			"    32.2,                    !- Rated Outlet Air Temperature {C}",
			"    ;                        !- Rated Ratio for Air and Water Convection",

			"  Schedule:Compact,",
			"    FanAndCoilAvailSched,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    SPACE1-1 Eq,             !- Name",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SPACE1-1 ATU,            !- Name",
			"    SPACE1-1 In Node,        !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan,  !- Air Terminal Object Type",
			"    SPACE1-1 VAV Reheat;     !- Air Terminal Name",

			"  Zone,",
			"    SPACE1-1,                !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    2.438400269,             !- Ceiling Height {m}",
			"    239.247360229;           !- Volume {m3}",

			"  ZoneHVAC:EquipmentConnections,",
			"    SPACE1-1,                !- Zone Name",
			"    SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"    SPACE1-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    SPACE1-1 Node,           !- Zone Air Node Name",
			"    SPACE1-1 Out Node;       !- Zone Return Air Node Name",

			"  NodeList,",
			"    SPACE1-1 In Nodes,       !- Name",
			"    SPACE1-1 In Node;        !- Node 1 Name",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();
		GetSysInput();

		EXPECT_EQ( "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", Sys( 1 ).SysType ); // AT SD VAV HeatCool Reheat Type
		EXPECT_EQ( "SPACE1-1 VAV REHEAT", Sys( 1 ).SysName ); // AT SD VAV HeatCool Reheat Name
		EXPECT_EQ( "COIL:HEATING:WATER", Sys( 1 ).ReheatComp ); // Reheat Coil Type
		EXPECT_EQ( "SPACE1-1 ZONE COIL", Sys( 1 ).ReheatName ); // Reheat Coil Name

	}
}
