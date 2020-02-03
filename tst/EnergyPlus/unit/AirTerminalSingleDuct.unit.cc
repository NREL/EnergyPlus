// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
// AirTerminal:SingleDuct:ConstantVolume:Reheat
// AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction
// AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat
// AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

// EnergyPlus Headers
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACSingleDuctInduc;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;
using namespace EnergyPlus::DataDefineEquip;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataHeatBalFanSys;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, AirTerminalSingleDuctCVReheat_GetInputTest)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
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
        "    SequentialLoad,          !- Load Distribution Scheme",
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

    ASSERT_TRUE(process_idf(idf_objects));

    NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(OutputFiles::getSingleton());  // read schedules

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetSysInput();

    EXPECT_EQ("AirTerminal:SingleDuct:ConstantVolume:Reheat", Sys(1).SysType); // AT SD VAV Reheat Type
    EXPECT_EQ("REHEAT ZONE 1", Sys(1).SysName);                                // AT SD VAV Reheat Name
    EXPECT_GT(Sys(1).ReheatControlNode, 0);                                    // none zero integer node index is expected
}

TEST_F(EnergyPlusFixture, AirTerminalSingleDuct4PipeInduction_GetInputTest)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
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
        "    SequentialLoad,          !- Load Distribution Scheme",
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

    });

    process_idf(idf_objects);

    NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(OutputFiles::getSingleton());  // read schedules

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetIndUnits();

    EXPECT_EQ("AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction", IndUnit(1).UnitType); // AT SD VAV Reheat Type
    EXPECT_EQ("SPACE1-1 FPIU", IndUnit(1).Name);                                               // AT SD VAV Reheat Name
    EXPECT_GT(IndUnit(1).HWControlNode, 0);                                                    // none zero integer node index is expected
    EXPECT_GT(IndUnit(1).CWControlNode, 0);                                                    // none zero integer node index is expected
}

TEST_F(EnergyPlusFixture, AirTerminalSingleDuctVAVHeatCool_GetInputTest)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
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
        "    SequentialLoad,          !- Load Distribution Scheme",
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

    });

    process_idf(idf_objects);

    NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(OutputFiles::getSingleton());  // read schedules

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetSysInput();

    EXPECT_EQ("AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat", Sys(1).SysType); // AT SD VAV HeatCool Reheat Type
    EXPECT_EQ("ZONE 1 VAV SYSTEM", Sys(1).SysName);                             // AT SD VAV HeatCool Reheat Name
    EXPECT_EQ("COIL:HEATING:ELECTRIC", Sys(1).ReheatComp);                      // Reheat Coil Type
    EXPECT_EQ("REHEAT COIL ZONE 1", Sys(1).ReheatName);                         // Reheat Coil Name
}

TEST_F(EnergyPlusFixture, AirTerminalSingleDuctVAVReheatVarSpeedFan_GetInputTest)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
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
        "    SequentialLoad,          !- Load Distribution Scheme",
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

    });

    ASSERT_TRUE(process_idf(idf_objects));

    NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(OutputFiles::getSingleton());  // read schedules

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetSysInput();

    EXPECT_EQ("AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", Sys(1).SysType); // AT SD VAV HeatCool Reheat Type
    EXPECT_EQ("SPACE1-1 VAV REHEAT", Sys(1).SysName);                                // AT SD VAV HeatCool Reheat Name
    EXPECT_EQ("COIL:HEATING:WATER", Sys(1).ReheatComp);                              // Reheat Coil Type
    EXPECT_EQ("SPACE1-1 ZONE COIL", Sys(1).ReheatName);                              // Reheat Coil Name
}

TEST_F(EnergyPlusFixture, AirTerminalSingleDuctVAVReheat_NormalActionTest)
{
    std::string const idf_objects = delimited_string({

        " AirTerminal:SingleDuct:VAV:Reheat,",
        "    VAV Elec Rht,                          !- Name",
        "    FanAndCoilAvailSched,                  !- Availability Schedule Name",
        "    VAV Elec Rht Damper Outlet,            !- Damper Air Outlet Node Name",
        "    Node 13,                               !- Air Inlet Node Name",
        "    1.0,                                   !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                              !- Zone Minimum Air Flow Input Method",
        "    0.2,                                   !- Constant Minimum Air Flow Fraction",
        "    ,                                      !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                                      !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Electric,                 !- Reheat Coil Object Type",
        "    VAV Elec Rht Coil,                     !- Reheat Coil Name",
        "    0,                                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0,                                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    Node 6,                                !- Air Outlet Node Name",
        "    0.001,                                 !- Convergence Tolerance",
        "    Normal,                                !- Damper Heating Action",
        "    Autocalculate,                         !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    Autocalculate,                         !- Maximum Flow Fraction During Reheat",
        "    35;                                    !- Maximum Reheat Air Temperature {C}",

        " Coil:Heating:Electric,",
        "    VAV Elec Rht Coil,                     !- Name",
        "    FanAndCoilAvailSched,                  !- Availability Schedule Name",
        "    1,                                     !- Efficiency",
        "    Autosize,                              !- Nominal Capacity {W}",
        "    VAV Elec Rht Damper Outlet,            !- Air Inlet Node Name",
        "    Node 6;                                !- Air Outlet Node Name",

        "  Schedule:Compact,",
        "    FanAndCoilAvailSched,                  !- Name",
        "    Fraction,                              !- Schedule Type Limits Name",
        "    Through: 12/31,                        !- Field 1",
        "    For: AllDays,                          !- Field 2",
        "    Until: 24:00,1.0;                      !- Field 3",

        " ZoneHVAC:EquipmentConnections,",
        "   Thermal Zone 1,                         !- Zone Name",
        "   Thermal Zone 1 Equipment List,          !- Zone Conditioning Equipment List Name",
        "   Thermal Zone 1 Inlet Node List,         !- Zone Air Inlet Node or NodeList Name",
        "   ,                                       !- Zone Air Exhaust Node or NodeList Name",
        "   Node 1,                                 !- Zone Air Node Name",
        "   Node 12;                                !- Zone Return Air Node or NodeList Name",

        " NodeList,",
        "   Thermal Zone 1 Inlet Node List,         !- Name",
        "   Node 6;                                 !- Node Name 1",

        " ZoneHVAC:AirDistributionUnit,",
        "   ADU VAV Elec Rht,                       !- Name",
        "   Node 6,                                 !- Air Distribution Unit Outlet Node Name",
        "   AirTerminal:SingleDuct:VAV:Reheat,      !- Air Terminal Object Type",
        "   VAV Elec Rht;                           !- Air Terminal Name",

        "  Zone,",
        "    Thermal Zone 1,                        !- Name",
        "    0,                                     !- Direction of Relative North {deg}",
        "    0,                                     !- X Origin {m}",
        "    0,                                     !- Y Origin {m}",
        "    0,                                     !- Z Origin {m}",
        "    1,                                     !- Type",
        "    1,                                     !- Multiplier",
        "    3.0,                                   !- Ceiling Height {m}",
        "    250.0;                                 !- Volume {m3}",

        "  ZoneHVAC:EquipmentList,",
        "    Thermal Zone 1 Equipment List,         !- Name",
        "    ,                                      !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,          !- Zone Equipment Object Type 1",
        "    ADU VAV Elec Rht,                      !- Zone Equipment Name 1",
        "    1,                                     !- Zone Equipment Cooling Sequence 1",
        "    1;                                     !- Zone Equipment Heating or No-Load Sequence 1",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    NumOfTimeStepInHour = 1;
    MinutesPerTimeStep = 60;
    ProcessScheduleInput(OutputFiles::getSingleton());
    bool ErrorsFound(false);
    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetSysInput();
    SingleDuct::GetInputFlag = false;

    auto &thisZoneEquip(ZoneEquipConfig(NumOfZones));

    DataGlobals::SysSizingCalc = true;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = 1.0;
    DataEnvironment::OutBaroPress = 101325.0;

    int const SysNum(1);
    int const InletNode = Sys(SysNum).InletNodeNum;
    int const OutletNode = Sys(SysNum).OutletNodeNum;
    int const ZonePtr = Sys(SysNum).ActualZoneNum;
    int const ZoneAirNodeNum = thisZoneEquip.ZoneNode;
    Schedule(Sys(SysNum).SchedPtr).CurrentValue = 1.0; // unit is always available

    // design maximum air mass flow rate
    Real64 MassFlowRateMaxAvail = Sys(SysNum).MaxAirVolFlowRate * DataEnvironment::StdRhoAir;
    EXPECT_EQ(1.0, Sys(SysNum).MaxAirVolFlowRate);
    EXPECT_EQ(1.0, MassFlowRateMaxAvail);
    EXPECT_EQ("COIL:HEATING:ELECTRIC", Sys(SysNum).ReheatComp);
    EXPECT_EQ(Normal, Sys(SysNum).DamperHeatingAction);
    EXPECT_EQ(0.2, Sys(SysNum).ZoneMinAirFrac);

    // set air inlet node properties
    Node(InletNode).Temp = 15.0;
    Node(InletNode).HumRat = 0.005;
    Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat);
    // set inlet mass flow rate to zero
    Node(InletNode).MassFlowRateMaxAvail = MassFlowRateMaxAvail;

    // set zone air node properties
    Node(ZoneAirNodeNum).Temp = 20.0;
    Node(ZoneAirNodeNum).HumRat = 0.005;
    Node(ZoneAirNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(Node(ZoneAirNodeNum).Temp, Node(ZoneAirNodeNum).HumRat);

    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputRequired = 1000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 1000.0;
    TempControlType.allocate(1);
    TempControlType(1) = 4;

    // calc min air mass flow rate for Normal Damper Heating Action
    Real64 expectedMassFlowAirReheatMin = 0.2 * MassFlowRateMaxAvail;
    bool FirstHVACIteration = false;

    auto &thisAirDistUnit(DataDefineEquip::AirDistUnit(ZonePtr));
    // run SimulateSingleDuct() function
    SimulateSingleDuct(thisAirDistUnit.EquipName(1), FirstHVACIteration, ZonePtr, ZoneAirNodeNum, thisAirDistUnit.EquipIndex(1));
    // check min, actual and max air mass flow rates during reheat with Normal Action
    EXPECT_EQ(expectedMassFlowAirReheatMin, SysOutlet(SysNum).AirMassFlowRate);
    EXPECT_EQ(expectedMassFlowAirReheatMin, Node(InletNode).MassFlowRate);
    EXPECT_EQ(expectedMassFlowAirReheatMin, Node(OutletNode).MassFlowRate);
    EXPECT_EQ(1.0, Sys(SysNum).AirMassFlowRateMax);
}

TEST_F(EnergyPlusFixture, SingleDuctVAVReheatVSFan_DamperPositionTest)
{
    std::string const idf_objects = delimited_string({
        "   Zone,",
        "    Thermal Zone;               !- Name",

        "   ZoneHVAC:EquipmentConnections,",
        "     Thermal Zone,              !- Zone Name",
        "     Thermal Zone Equipment,    !- Zone Conditioning Equipment List Name",
        "     Zone 1 In Node,            !- Zone Air Inlet Node or NodeList Name",
        "     ,                          !- Zone Air Exhaust Node or NodeList Name",
        "     Zone 1 Air Node,           !- Zone Air Node Name",
        "     Zone 1 Return Node;        !- Zone Return Air Node Name",

        "   ZoneHVAC:EquipmentList,",
        "     Thermal Zone Equipment,    !- Name",
        "     SequentialLoad,            !- Load Distribution Scheme",
        "     ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "     ADU VAV Rht VS Fan,        !- Zone Equipment 1 Name",
        "     1,                         !- Zone Equipment 1 Cooling Sequence",
        "     1;                         !- Zone Equipment 1 Heating or No-Load Sequence",

        "   ZoneHVAC:AirDistributionUnit,",
        "     ADU VAV Rht VS Fan,      !- Name",
        "     Zone 1 In Node,          !- Air Distribution Unit Outlet Node Name",
        "     AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan,  !- Air Terminal Object Type",
        "     VAV Rht VS Fan AirTerm;  !- Air Terminal Name",

        "   AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan,",
        "     VAV Rht VS Fan AirTerm,  !- Name",
        "     ,                        !- Availability Schedule Name",
        "     1.0,                     !- Maximum Cooling Air Flow Rate {m3/s}",
        "     0.5,                     !- Maximum Heating Air Flow Rate {m3/s}",
        "     0.05,                    !- Zone Minimum Air Flow Fraction",
        "     Zone 1 ATU In Node,      !- Air Inlet Node Name",
        "     Zone 1 In Node,          !- Air Outlet Node Name",
        "     Fan:SystemModel,         !- Fan Object Type",
        "     Zone 1 VS Fan,           !- Fan Name",
        "     Coil:Heating:Electric,   !- Heating Coil Object Type",
        "     Zone 1 Reheat Coil,      !- Heating Coil Name",
        "     autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "     0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "     0.001;                   !- Heating Convergence Tolerance",

        "   Coil:Heating:Electric,",
        "     Zone 1 Reheat Coil,      !- Name",
        "     ,                        !- Availability Schedule Name",
        "     1.0,                     !- Efficiency",
        "     autosize,                !- Nominal Capacity {W}",
        "     Reheat Air Inlet Node,   !- Air Inlet Node Name",
        "     Zone 1 In Node;          !- Air Outlet Node Name",

        "   Fan:SystemModel,",
        "     Zone 1 VS Fan,           !- Name",
        "     ,                        !- Availability Schedule Name",
        "     Zone 1 ATU In Node,      !- Air Inlet Node Name",
        "     Reheat Air Inlet Node,   !- Air Outlet Node Name",
        "     1.0,                     !- Design Maximum Air Flow Rate {m3/s}",
        "     Continuous,              !- Speed Control Method",
        "     0.0,                     !- Electric Power Minimum Flow Rate Fraction",
        "     125.0,                   !- Design Pressure Rise {Pa}",
        "     0.9,                     !- Motor Efficiency",
        "     1.0,                     !- Motor In Air Stream Fraction",
        "     AUTOSIZE,                !- Design Electric Power Consumption {W}",
        "     TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "     ,                        !- Electric Power Per Unit Flow Rate {W/(m3/s)}",
        "     ,                        !- Electric Power Per Unit Flow Rate Per Unit Pressure {W/((m3/s)-Pa)}",
        "     0.7,                     !- Fan Total Efficiency",
        "     VAV Fan Curve,           !- Electric Power Function of Flow Fraction Curve Name",
        "     ,                        !- Night Ventilation Mode Pressure Rise {Pa}",
        "     ,                        !- Night Ventilation Mode Flow Fraction",
        "     ,                        !- Motor Loss Zone Name",
        "     ,                        !- Motor Loss Radiative Fraction",
        "     ATU Fan Energy;          !- End-Use Subcategory",

        "   Curve:Quartic,",
        "     VAV Fan Curve,           !- Name",
        "     0.00153028,              !- Coefficient1 Constant",
        "     0.00520806,              !- Coefficient2 x",
        "     1.1086242,               !- Coefficient3 x**2",
        "     -.11635563,              !- Coefficient4 x**3",
        "     0.0,                     !- Coefficient5 x**4",
        "     0.0,                     !- Minimum Value of x",
        "     1.0,                     !- Maximum Value of x",
        "     0.0,                     !- Minimum Curve Output",
        "     1.0,                     !- Maximum Curve Output",
        "     Dimensionless,           !- Input Unit Type for X",
        "     Dimensionless;           !- Output Unit Type",
        });

    ASSERT_TRUE(process_idf(idf_objects));
    // setup variables for VAV Reheat VS Fan
    int SysNum = 1;
    int ZoneNum = 1;
    int ZoneNodeNum = 1;
    int InletNodeNum = 5;
    bool ErrorsFound = false;
    bool FirstHVACIteration = true;

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput(OutputFiles::getSingleton());
    ScheduleManager::ScheduleInputProcessed = true;
    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    SingleDuct::GetSysInput();
    EXPECT_TRUE(compare_err_stream(""));

    auto &thisAirTerminal = SingleDuct::Sys(SysNum);
    auto &thisAirTerminalOutlet = SingleDuct::SysOutlet(SysNum);

    // check VAV reheat VS Fan air terminal inputs
    EXPECT_EQ("AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", thisAirTerminal.SysType);
    EXPECT_EQ("VAV RHT VS FAN AIRTERM", thisAirTerminal.SysName); 
    EXPECT_EQ("COIL:HEATING:ELECTRIC", thisAirTerminal.ReheatComp);
    EXPECT_EQ("ZONE 1 REHEAT COIL", thisAirTerminal.ReheatName);
    EXPECT_EQ("FAN:SYSTEMMODEL", thisAirTerminal.FanType);
    EXPECT_EQ("ZONE 1 VS FAN", thisAirTerminal.FanName);
    EXPECT_EQ(thisAirTerminal.ZoneMinAirFrac, 0.05);
    EXPECT_EQ(thisAirTerminal.MaxAirVolFlowRate, 1.0);
    // test 1: 0.05 fraction damper position
    Real64 SysMinMassFlowRes = 1.0 * DataEnvironment::StdRhoAir * 0.05; 
    Real64 SysMaxMassFlowRes = 1.0 * DataEnvironment::StdRhoAir;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 0.0;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMaxMassFlowRes;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlowRes;
    CurDeadBandOrSetback.allocate(1);
    CurDeadBandOrSetback(1) = false;
    DataGlobals::BeginEnvrnFlag = true;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration);
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration);
    SingleDuct::SimVAVVS(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    // check inputs and calculated values for zone air fraction 0.05
    EXPECT_EQ(0.05,thisAirTerminal.ZoneMinAirFrac);  // user input
    EXPECT_EQ(0.05, thisAirTerminal.DamperPosition);
    EXPECT_EQ(SysMinMassFlowRes, thisAirTerminal.AirMassFlowRateMax * thisAirTerminal.ZoneMinAirFrac);
    EXPECT_EQ(SysMinMassFlowRes, thisAirTerminalOutlet.AirMassFlowRate);

    // test 2: 0.10 fraction damper position
    thisAirTerminal.ZoneMinAirFrac = 0.10; // modified user input
    SysMinMassFlowRes = 1.0 * DataEnvironment::StdRhoAir * 0.10; 
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 0.0;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMaxMassFlowRes;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlowRes;
    DataGlobals::BeginEnvrnFlag = true;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration);
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration);
    SingleDuct::SimVAVVS(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    // check inputs and calculated values for zone air fraction 0.10
    EXPECT_EQ(0.10, thisAirTerminal.ZoneMinAirFrac);
    EXPECT_EQ(0.10, thisAirTerminal.DamperPosition);
    EXPECT_EQ(SysMinMassFlowRes, thisAirTerminal.AirMassFlowRateMax * thisAirTerminal.ZoneMinAirFrac);
    EXPECT_EQ(SysMinMassFlowRes, thisAirTerminalOutlet.AirMassFlowRate);
}

} // namespace EnergyPlus
