// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::ZoneEquipmentManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::ZoneEquipmentManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceAirManager;
using namespace EnergyPlus::HeatBalanceManager;

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space In Node,            !- Zone Air Inlet Node or NodeList Name",
        " Space Exh Nodes,          !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
        " Exhaust Fan,              !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No - Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",

        "Fan:ZoneExhaust,",
        "Exhaust Fan,               !- Name",
        ",                          !- Availability Schedule Name",
        "0.338,                     !- Fan Total Efficiency",
        "125.0000,                  !- Pressure Rise{Pa}",
        "0.3000,                    !- Maximum Flow Rate{m3/s}",
        "Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
        "Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
        "Zone Exhaust Fans;         !- End - Use Subcategory",

        "NodeList,",
        "  Space Exh Nodes,  !- Name",
        "  Space ZoneHVAC Exh Node, !- Node 1 Name",
        "  Exhaust Fan Inlet Node; !- Node 1 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(*state, ErrorsFound);
    int ZoneNum = 1;
    int NodeNum;
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }

    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 0;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 1;
    // Avoid zero values in volume flow balance check
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataEnvrn->OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    // Test here - if zone equipment exhausts slightly more than it supplies, there should be no unbalanced exhaust flow warning
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 1.000000001;
    CalcZoneMassBalance(*state, false);
    EXPECT_FALSE(has_err_output());

    // Add excess balanced zone exhaust from exhaust fan, still no warning
    ZoneEquipConfig(ZoneNum).ZoneExh = 0.5;
    ZoneEquipConfig(ZoneNum).ZoneExhBalanced = 0.5;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(2)).MassFlowRate = 0.5;
    CalcZoneMassBalance(*state, false);
    EXPECT_FALSE(has_err_output());

    // Add excess unbalanced zone exhaust from exhaust fan, now there should be warning
    ZoneEquipConfig(ZoneNum).ZoneExh = 0.5;
    ZoneEquipConfig(ZoneNum).ZoneExhBalanced = 0.0;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(2)).MassFlowRate = 0.5;
    CalcZoneMassBalance(*state, false);
    EXPECT_TRUE(has_err_output());

    // Deallocate everything - should all be taken care of in clear_states
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_MultiCrossMixingTest)
{

    std::string const idf_objects = delimited_string({
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

        "  Zone,",
        "    SPACE2-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "  Zone,",
        "    SPACE3-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "  Zone,",
        "    SPACE4-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "  Zone,",
        "    SPACE5-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    447.682556152;           !- Volume {m3}",

        "  Schedule:Compact,",
        "    MixingAvailSched,        !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.00,       !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: Weekdays,           !- Field 6",
        "    Until: 7:00,1.00,        !- Field 7",
        "    Until: 17:00,1.00,       !- Field 9",
        "    Until: 24:00,1.00,       !- Field 11",
        "    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 13",
        "    Until: 24:00,1.00,       !- Field 14",
        "    For: SummerDesignDay WinterDesignDay, !- Field 16",
        "    Until: 24:00,1.00,       !- Field 17",
        "    Through: 12/31,          !- Field 19",
        "    For: AllDays,            !- Field 20",
        "    Until: 24:00,1.00;       !- Field 21",

        "  Schedule:Compact,",
        "    MinIndoorTemp,           !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,18;         !- Field 3",

        "  Schedule:Compact,",
        "    MaxIndoorTemp,           !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,100;        !- Field 3",

        "  Schedule:Compact,",
        "    DeltaTemp,               !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,2;          !- Field 3",

        "  Schedule:Compact,",
        "    MinOutdoorTemp,          !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,-100;       !- Field 3",

        "  Schedule:Compact,",
        "    MaxOutdoorTemp,          !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,100;        !- Field 3",

        "  ZoneCrossMixing,",
        "    SPACE2-4 XMixng 1,       !- Name",
        "    SPACE2-1,                !- Zone Name",
        "    MixingAvailSched,        !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.1,                     !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow Rate per Person {m3/s-person}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    SPACE4-1,                !- Source Zone Name",
        "    1.0,                     !- Delta Temperature {deltaC}",
        "    ,                        !- Delta Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
        "    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
        "    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

        "  ZoneCrossMixing,",
        "    SPACE4-2 XMixng 1,       !- Name",
        "    SPACE4-1,                !- Zone Name",
        "    MixingAvailSched,        !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.1,                     !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow Rate per Person {m3/s-person}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    SPACE2-1,                !- Source Zone Name",
        "    1.0,                     !- Delta Temperature {deltaC}",
        "    ,                        !- Delta Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
        "    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
        "    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

        "  ZoneCrossMixing,",
        "    SPACE3-4 XMixng 1,       !- Name",
        "    SPACE3-1,                !- Zone Name",
        "    MixingAvailSched,        !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.2,                     !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow Rate per Person {m3/s-person}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    SPACE4-1,                !- Source Zone Name",
        "    0.0,                     !- Delta Temperature {deltaC}",
        "    ,                        !- Delta Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
        "    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
        "    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

        "  ZoneCrossMixing,",
        "    SPACE1-4 XMixng 1,       !- Name",
        "    SPACE1-1,                !- Zone Name",
        "    MixingAvailSched,        !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.3,                     !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow Rate per Person {m3/s-person}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    SPACE4-1,                !- Source Zone Name",
        "    0.0,                     !- Delta Temperature {deltaC}",
        "    ,                        !- Delta Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
        "    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
        "    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

        "  ZoneCrossMixing,",
        "    SPACE1-3 XMixng 1,       !- Name",
        "    SPACE1-1,                !- Zone Name",
        "    MixingAvailSched,        !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.3,                     !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow Rate per Person {m3/s-person}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    SPACE3-1,                !- Source Zone Name",
        "    0.0,                     !- Delta Temperature {deltaC}",
        "    ,                        !- Delta Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
        "    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
        "    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
        "    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
        "    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    ScheduleManager::ProcessScheduleInput(*state);
    GetZoneData(*state, ErrorsFound);
    DataHeatBalFanSys::ZoneReOrder.allocate(state->dataGlobal->NumOfZones);

    GetSimpleAirModelInputs(*state, ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    DataHeatBalFanSys::MAT.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::MCPM.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::MCPTM.allocate(state->dataGlobal->NumOfZones);

    DataHeatBalFanSys::MCPI.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::OAMFL.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::MCPTI.allocate(state->dataGlobal->NumOfZones);

    DataHeatBalFanSys::MixingMassFlowZone.allocate(state->dataGlobal->NumOfZones);
    DataHeatBalFanSys::MixingMassFlowXHumRat.allocate(state->dataGlobal->NumOfZones);

    DataHeatBalFanSys::MAT(1) = 21.0;
    DataHeatBalFanSys::MAT(2) = 22.0;
    DataHeatBalFanSys::MAT(3) = 23.0;
    DataHeatBalFanSys::MAT(4) = 24.0;
    DataHeatBalFanSys::MAT(5) = 25.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(2) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(3) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(4) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(5) = 0.001;

    DataHeatBalance::AirFlowFlag = 1;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "MIXINGAVAILSCHED")).CurrentValue = 1.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "MININDOORTEMP")).CurrentValue = 18.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "MAXINDOORTEMP")).CurrentValue = 100.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "DELTATEMP")).CurrentValue = 2.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "MINOUTDOORTEMP")).CurrentValue = -100.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "MAXOUTDOORTEMP")).CurrentValue = 100.0;
    state->dataEnvrn->OutBaroPress = 101325.0;

    InitSimpleMixingConvectiveHeatGains(*state);

    CalcAirFlowSimple(*state, 2);

    EXPECT_NEAR(720.738493, DataHeatBalFanSys::MCPM(1), 0.00001);
    EXPECT_NEAR(119.818784, DataHeatBalFanSys::MCPM(2), 0.00001);
    EXPECT_NEAR(599.907893, DataHeatBalFanSys::MCPM(3), 0.00001);
    EXPECT_NEAR(719.116710, DataHeatBalFanSys::MCPM(4), 0.00001);
    EXPECT_NEAR(16937.0496, DataHeatBalFanSys::MCPTM(1), 0.001);
    EXPECT_NEAR(2875.6508, DataHeatBalFanSys::MCPTM(2), 0.001);
    EXPECT_NEAR(13315.7667, DataHeatBalFanSys::MCPTM(3), 0.001);
    EXPECT_NEAR(15699.7370, DataHeatBalFanSys::MCPTM(4), 0.001);
    EXPECT_NEAR(0.71594243, DataHeatBalFanSys::MixingMassFlowZone(1), 0.00001);
    EXPECT_NEAR(0.11902146, DataHeatBalFanSys::MixingMassFlowZone(2), 0.00001);
    EXPECT_NEAR(0.59591588, DataHeatBalFanSys::MixingMassFlowZone(3), 0.00001);
    EXPECT_NEAR(0.71433143, DataHeatBalFanSys::MixingMassFlowZone(4), 0.00001);
    EXPECT_NEAR(0.00071594243, DataHeatBalFanSys::MixingMassFlowXHumRat(1), 0.0000001);
    EXPECT_NEAR(0.00011902146, DataHeatBalFanSys::MixingMassFlowXHumRat(2), 0.0000001);
    EXPECT_NEAR(0.00059591588, DataHeatBalFanSys::MixingMassFlowXHumRat(3), 0.0000001);
    EXPECT_NEAR(0.00071433143, DataHeatBalFanSys::MixingMassFlowXHumRat(4), 0.0000001);

    // Deallocate everything - should all be taken care of in clear_states

    DataHeatBalFanSys::MAT.deallocate();
    DataHeatBalFanSys::ZoneAirHumRat.deallocate();
    DataHeatBalFanSys::MCPM.deallocate();
    DataHeatBalFanSys::MCPTM.deallocate();
    DataHeatBalFanSys::MCPI.deallocate();
    DataHeatBalFanSys::OAMFL.deallocate();
    DataHeatBalFanSys::MCPTI.deallocate();
    DataHeatBalFanSys::MixingMassFlowZone.deallocate();
    DataHeatBalFanSys::MixingMassFlowXHumRat.deallocate();
    DataHeatBalFanSys::ZoneReOrder.deallocate();
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest2)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,            !- Zone Air Inlet Node or NodeList Name",
        " Space Exh Nodes,           !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Return Nodes;           !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
        " Exhaust Fan,              !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No - Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",

        "Fan:ZoneExhaust,",
        "Exhaust Fan,               !- Name",
        ",                          !- Availability Schedule Name",
        "0.338,                     !- Fan Total Efficiency",
        "125.0000,                  !- Pressure Rise{Pa}",
        "0.3000,                    !- Maximum Flow Rate{m3/s}",
        "Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
        "Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
        "Zone Exhaust Fans;         !- End - Use Subcategory",

        "NodeList,",
        "  Space Exh Nodes,  !- Name",
        "  Space ZoneHVAC Exh Node, !- Node 1 Name",
        "  Exhaust Fan Inlet Node; !- Node 2 Name",

        "NodeList,",
        "  Space Inlet Nodes,  !- Name",
        "  Space Inlet Node 1, !- Node 1 Name",
        "  Space Inlet Node 2, !- Node 2 Name",
        "  Space Inlet Node 3; !- Node 3 Name",

        "NodeList,",
        "  Space Return Nodes,  !- Name",
        "  Space Return Node 1, !- Node 1 Name",
        "  Space Return Node 2, !- Node 2 Name",
        "  Space Return Node 3; !- Node 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(*state, ErrorsFound);

    int ZoneNum = 1;
    for (int NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(1) = 2; // Intentionally not in 1,2,3 order
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(2) = 3;
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(3) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 3; // Intentionally in a different order
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(2) = 2;
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(3) = 1;
    int inletNode1 = ZoneEquipConfig(ZoneNum).InletNode(1);
    int inletNode2 = ZoneEquipConfig(ZoneNum).InletNode(2);
    int inletNode3 = ZoneEquipConfig(ZoneNum).InletNode(3);
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 2; // Intentionally in a different order
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(2) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(3) = 3;
    int returnNode1 = ZoneEquipConfig(ZoneNum).ReturnNode(1);
    int returnNode2 = ZoneEquipConfig(ZoneNum).ReturnNode(2);
    int returnNode3 = ZoneEquipConfig(ZoneNum).ReturnNode(3);

    // Avoid zero values in volume flow balance check
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataEnvrn->OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    DataHVACGlobals::NumPrimaryAirSys = 3;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(3);
    state->dataAirLoop->AirLoopFlow.allocate(3);

    state->dataAirSystemsData->PrimaryAirSystems(1).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(1).DesReturnFrac = 1.0;
    state->dataAirSystemsData->PrimaryAirSystems(2).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(2).DesReturnFrac = 1.0;
    state->dataAirSystemsData->PrimaryAirSystems(3).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(3).DesReturnFrac = 1.0;
    state->dataGlobal->DoingSizing = false;
    state->dataGlobal->isPulseZoneSizing = false;

    // Case 1 - send zero, expect zero back
    Node(inletNode1).MassFlowRate = 0.0;
    Node(inletNode2).MassFlowRate = 0.0;
    Node(inletNode3).MassFlowRate = 0.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    Real64 StdTotalReturnMassFlow = 0.0;
    Real64 FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(*state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 0.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 0.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 0.0);

    // Case 2 - send zero, expect sum of inlet flow back
    Node(inletNode2).MassFlowRate = 2.0;
    Node(inletNode1).MassFlowRate = 1.0;
    Node(inletNode3).MassFlowRate = 3.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    StdTotalReturnMassFlow = 0.0;
    FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(*state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 6.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 1.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);

    // Deallocate everything - should all be taken care of in clear_states
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest3)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space In Node,            !- Zone Air Inlet Node or NodeList Name",
        " Space Exh Nodes,          !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node,           !- Zone Return Air Node Name",
        ",                          !- Zone Return Air Node 1 Flow Rate Fraction Schedule Name",
        " ReturnFlowBasisNodes;     !- Zone Return Air Node 1 Flow Rate Basis Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
        " Exhaust Fan,              !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No - Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",

        "Fan:ZoneExhaust,",
        "Exhaust Fan,               !- Name",
        ",                          !- Availability Schedule Name",
        "0.338,                     !- Fan Total Efficiency",
        "125.0000,                  !- Pressure Rise{Pa}",
        "0.3000,                    !- Maximum Flow Rate{m3/s}",
        "Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
        "Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
        "Zone Exhaust Fans;         !- End - Use Subcategory",

        "NodeList,",
        "  Space Exh Nodes,  !- Name",
        "  Space ZoneHVAC Exh Node, !- Node 1 Name",
        "  Exhaust Fan Inlet Node; !- Node 1 Name",

        "NodeList,",
        "  ReturnFlowBasisNodes,  !- Name",
        "  Basis Node 1,          !- Node 1 Name",
        "  Basis Node 2,          !- Node 2 Name",
        "  Basis Node 3;          !- Node 3 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(*state, ErrorsFound);
    int ZoneNum = 1;
    int NodeNum;
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }

    DataHVACGlobals::NumPrimaryAirSys = 1;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirLoop->AirLoopFlow.allocate(1);

    state->dataAirSystemsData->PrimaryAirSystems(1).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(1).DesReturnFrac = 1.0;
    state->dataGlobal->DoingSizing = false;
    state->dataGlobal->isPulseZoneSizing = false;

    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 1;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 0.0;

    // Avoid zero values in volume flow balance check
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataEnvrn->OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    // Set return node basis node flows to zero, return flow should be zero
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumReturnFlowBasisNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(NodeNum)).MassFlowRate = 0.0;
    }
    CalcZoneMassBalance(*state, false);
    EXPECT_EQ(Node(ZoneEquipConfig(ZoneNum).ReturnNode(1)).MassFlowRate,0.0);

    // Set return node basis node flows to non-zero values, return flow should be the sum
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(1)).MassFlowRate = 0.05;
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(2)).MassFlowRate = 0.10;
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(3)).MassFlowRate = 0.20;
    CalcZoneMassBalance(*state, false);
    EXPECT_NEAR(Node(ZoneEquipConfig(ZoneNum).ReturnNode(1)).MassFlowRate, 0.35, 0.00001);
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest4)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,            !- Zone Air Inlet Node or NodeList Name",
        " Space Exh Nodes,           !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Return Nodes;           !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
        " Exhaust Fan,              !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

        "Fan:ZoneExhaust,",
        "Exhaust Fan,               !- Name",
        ",                          !- Availability Schedule Name",
        "0.338,                     !- Fan Total Efficiency",
        "125.0000,                  !- Pressure Rise{Pa}",
        "0.3000,                    !- Maximum Flow Rate{m3/s}",
        "Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
        "Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
        "Zone Exhaust Fans;         !- End - Use Subcategory",

        "NodeList,",
        "  Space Exh Nodes,  !- Name",
        "  Space ZoneHVAC Exh Node, !- Node 1 Name",
        "  Exhaust Fan Inlet Node; !- Node 2 Name",

        "NodeList,",
        "  Space Inlet Nodes,  !- Name",
        "  Space Inlet Node 1, !- Node 1 Name",
        "  Space Inlet Node 2, !- Node 2 Name",
        "  Space Inlet Node 3; !- Node 3 Name",

        "NodeList,",
        "  Space Return Nodes,  !- Name",
        "  Space Return Node 1, !- Node 1 Name",
        "  Space Return Node 2, !- Node 2 Name",
        "  Space Return Node 3; !- Node 3 Name",

        });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(*state, ErrorsFound);

    int ZoneNum = 1;
    for (int NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(1) = 2; // Intentionally not in 1,2,3 order
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(2) = 3;
    ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(3) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 3; // Intentionally in a different order
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(2) = 2;
    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(3) = 1;
    int inletNode1 = ZoneEquipConfig(ZoneNum).InletNode(1);
    int inletNode2 = ZoneEquipConfig(ZoneNum).InletNode(2);
    int inletNode3 = ZoneEquipConfig(ZoneNum).InletNode(3);
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 2; // Intentionally in a different order
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(2) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(3) = 3;
    int returnNode1 = ZoneEquipConfig(ZoneNum).ReturnNode(1);
    int returnNode2 = ZoneEquipConfig(ZoneNum).ReturnNode(2);
    int returnNode3 = ZoneEquipConfig(ZoneNum).ReturnNode(3);

    // Avoid zero values in volume flow balance check
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataEnvrn->OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    DataHVACGlobals::NumPrimaryAirSys = 3;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(3);
    state->dataAirLoop->AirLoopFlow.allocate(3);

    // Add an outdoor air system to airloop 2
    state->dataAirSystemsData->PrimaryAirSystems(1).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(1).DesReturnFrac = 1.0;
    state->dataAirSystemsData->PrimaryAirSystems(2).OASysExists = true;
    state->dataAirLoop->AirLoopFlow(2).DesReturnFrac = 0.9;
    state->dataAirLoop->AirLoopFlow(2).MaxOutAir = 0.1;
    state->dataAirLoop->AirLoopFlow(2).OAFlow = 0.1;
    state->dataAirSystemsData->PrimaryAirSystems(3).OASysExists = false;
    state->dataAirLoop->AirLoopFlow(3).DesReturnFrac = 1.0;
    state->dataGlobal->DoingSizing = false;
    state->dataGlobal->isPulseZoneSizing = false;

    // Case 1 - send zero, expect zero back
    Node(inletNode1).MassFlowRate = 0.0;
    Node(inletNode2).MassFlowRate = 0.0;
    Node(inletNode3).MassFlowRate = 0.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    Real64 StdTotalReturnMassFlow = 0.0;
    Real64 FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(*state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 0.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 0.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 0.0);

    // Case 2 - send sum of inlet flow back, except system 2 back at 0.9
    Node(inletNode2).MassFlowRate = 2.0;
    Node(inletNode1).MassFlowRate = 1.0;
    Node(inletNode3).MassFlowRate = 3.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    StdTotalReturnMassFlow = 6.0;
    FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(*state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 5.9);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 0.9);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);

    // Case 3 - add exhaust flow, but set system 2 MaxOutAir to zero, expect sum of inlet flow back
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 1.000000001;
    state->dataAirSystemsData->PrimaryAirSystems(2).OASysExists = true;
    state->dataAirLoop->AirLoopFlow(2).DesReturnFrac = 0.9;
    state->dataAirLoop->AirLoopFlow(2).MaxOutAir = 0.0;
    state->dataAirLoop->AirLoopFlow(2).OAFlow = 0.0;

    Node(inletNode2).MassFlowRate = 2.0;
    Node(inletNode1).MassFlowRate = 1.0;
    Node(inletNode3).MassFlowRate = 3.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    StdTotalReturnMassFlow = 6.0;
    FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(*state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 6.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 1.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeSequentialLoad)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
        " ,                         !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 2 ADU,       !- Zone Equipment 2 Name",
        " 2,                        !- Zone Equipment 2 Cooling Sequence",
        " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 2 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
        " 3,                        !- Zone Equipment 3 Cooling Sequence",
        " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 1 ADU,       !- Name",
        " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 1;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 2 ADU,       !- Name",
        " Zone Equip Inlet 2,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 2;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 3 ADU,       !- Name",
        " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 3;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 1,          !- Name",
        " ,    !- Availability Schedule Name",
        " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 1,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 2,          !- Name",
        " ,    !- Availability Schedule Name",
        " Zone Equip Inlet 2 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 2,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 3,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 3,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "NodeList,",
        "  Space Inlet Nodes,       !- Name",
        "  Zone Equip Inlet 1,      !- Node 1 Name",
        "  Zone Equip Inlet 2,      !- Node 2 Name",
        "  Zone Equip Inlet 3;      !- Node 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(3);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));
    state->dataZoneEquipmentManager->PrioritySimOrder.allocate(3);

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);

    // Sequential Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // Sequential Test 3 - Cooling, FirstHVACIteration = true
    energy.TotalOutputRequired = -1000.0;
    energy.OutputRequiredToHeatingSP = -1000.0;
    energy.OutputRequiredToCoolingSP = -2000.0;
    firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);

    // Sequential Test 4 - Cooling, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeUniformLoad)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
        " ,                         !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " UniformLoad,              !- Load Distribution Scheme",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 2 ADU,       !- Zone Equipment 2 Name",
        " 2,                        !- Zone Equipment 2 Cooling Sequence",
        " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 2 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
        " 0,                        !- Zone Equipment 3 Cooling Sequence",
        " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 1 ADU,       !- Name",
        " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 1;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 2 ADU,       !- Name",
        " Zone Equip Inlet 2,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 2;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 3 ADU,       !- Name",
        " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 3;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 1,          !- Name",
        " ,    !- Availability Schedule Name",
        " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 1,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 2,          !- Name",
        " ,    !- Availability Schedule Name",
        " Zone Equip Inlet 2 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 2,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 3,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 3,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "NodeList,",
        "  Space Inlet Nodes,       !- Name",
        "  Zone Equip Inlet 1,      !- Node 1 Name",
        "  Zone Equip Inlet 2,      !- Node 2 Name",
        "  Zone Equip Inlet 3;      !- Node 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(3);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));

    // UniformLoad Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP / 3.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // UniformLoad Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP / 3.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP / 3.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // UniformLoad Test 3 - Cooling, FirstHVACIteration = true, 2 pieces of equipment are active for cooling
    energy.TotalOutputRequired = -1000.0;
    energy.OutputRequiredToHeatingSP = -1000.0;
    energy.OutputRequiredToCoolingSP = -2000.0;
    firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // UniformLoad Test 4 - Cooling, FirstHVACIteration = false, only 2 pieces of equipment are active for cooling
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP / 2.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeUniformPLR)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
        " ,                         !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " UniformPLR,               !- Load Distribution Scheme",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 2 ADU,       !- Zone Equipment 2 Name",
        " 2,                        !- Zone Equipment 2 Cooling Sequence",
        " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 2 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
        " 0,                        !- Zone Equipment 3 Cooling Sequence",
        " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 1 ADU,       !- Name",
        " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 1;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 2 ADU,       !- Name",
        " Zone Equip Inlet 2,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 2;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 3 ADU,       !- Name",
        " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 3;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 1,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 1,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 2,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 2 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 2,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 3,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 3,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "NodeList,",
        "  Space Inlet Nodes,       !- Name",
        "  Zone Equip Inlet 1,      !- Node 1 Name",
        "  Zone Equip Inlet 2,      !- Node 2 Name",
        "  Zone Equip Inlet 3;      !- Node 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(3);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));

    // Set up capacities for PLR calcs
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad = 4000;
    // For finalzonesizing, desing cooling load is positive
    DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad = 2500;
    auto &thisZEqList(DataZoneEquipment::ZoneEquipList(ZoneNum));
    thisZEqList.HeatingCapacity(1) = 2000.0;
    thisZEqList.HeatingCapacity(2) = 1000.0;
    thisZEqList.HeatingCapacity(3) = 500.0;
    Real64 totHeatingCapcity = thisZEqList.HeatingCapacity(1) + thisZEqList.HeatingCapacity(2) + thisZEqList.HeatingCapacity(3);
    // For zone equipment list, cooling capacity is negative
    thisZEqList.CoolingCapacity(1) = -1200.0;
    thisZEqList.CoolingCapacity(2) = -800.0;
    thisZEqList.CoolingCapacity(3) = -500.0;
    Real64 totCoolingCapcity = thisZEqList.CoolingCapacity(1) + thisZEqList.CoolingCapacity(2); // only include the first two equipment for cooling

    // UniformPLR Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    Real64 plr = energy.TotalOutputRequired / totHeatingCapcity;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    // Check sequenced load processing for unitary systems
    // EquipIndex doesn't get set until the units are simulated, so hard-wire them here
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 3;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal 2 for UniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 0);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal 2 for UniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);

    // UniformPLR Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), thisZEqList.HeatingCapacity(3) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), thisZEqList.HeatingCapacity(3) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), thisZEqList.HeatingCapacity(3) * plr);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // UniformPLR Test 3 - Cooling, FirstHVACIteration = true, 2 pieces of equipment are active for cooling
    energy.TotalOutputRequired = -1000.0;
    plr = energy.TotalOutputRequired / totCoolingCapcity;
    energy.OutputRequiredToHeatingSP = -1000.0;
    energy.OutputRequiredToCoolingSP = -2000.0;
    firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);

    // UniformPLR Test 4 - Cooling, FirstHVACIteration = false, only 2 pieces of equipment are active for cooling
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeSequentialUniformPLR)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
        " ,                         !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialUniformPLR,               !- Load Distribution Scheme",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 2 ADU,       !- Zone Equipment 2 Name",
        " 2,                        !- Zone Equipment 2 Cooling Sequence",
        " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 2 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
        " 0,                        !- Zone Equipment 3 Cooling Sequence",
        " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 1 ADU,       !- Name",
        " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 1;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 2 ADU,       !- Name",
        " Zone Equip Inlet 2,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 2;           !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 3 ADU,       !- Name",
        " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 3;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 1,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 1,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 2,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 2 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 2,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 3,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 3,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "NodeList,",
        "  Space Inlet Nodes,       !- Name",
        "  Zone Equip Inlet 1,      !- Node 1 Name",
        "  Zone Equip Inlet 2,      !- Node 2 Name",
        "  Zone Equip Inlet 3;      !- Node 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(3);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(3);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));

    // Set up capacities for PLR calcs
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad = 4000;
    // For finalzonesizing, desing cooling load is positive
    DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad = 2500;
    auto &thisZEqList(DataZoneEquipment::ZoneEquipList(ZoneNum));
    thisZEqList.HeatingCapacity(1) = 2000.0;
    thisZEqList.HeatingCapacity(2) = 1000.0;
    thisZEqList.HeatingCapacity(3) = 500.0;
    Real64 totHeatingCapcity = thisZEqList.HeatingCapacity(1) + thisZEqList.HeatingCapacity(2) + thisZEqList.HeatingCapacity(3);
    // For zone equipment list, cooling capacity is negative
    thisZEqList.CoolingCapacity(1) = -1200.0;
    thisZEqList.CoolingCapacity(2) = -800.0;
    thisZEqList.CoolingCapacity(3) = -500.0;
    Real64 totCoolingCapcity = thisZEqList.CoolingCapacity(1) + thisZEqList.CoolingCapacity(2); // only include the first two equipment for cooling

    // SequentialUniformPLR Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    Real64 plr = energy.TotalOutputRequired / totHeatingCapcity;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), DataSizing::FinalZoneSizing(ZoneNum).DesHeatLoad);
    // Check sequenced load processing for unitary systems
    // EquipIndex doesn't get set until the units are simulated, so hard-wire them here
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 3;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal equipmnum+1 for SequentialUniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 0);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal equipmnum+1 for SequentialUniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 4);

    // SequentialUniformPLR Test 2 - Heating, FirstHVACIteration = false, low load requiring only 1 piece of equipment
    firstHVACIteration = false;
    energy.TotalOutputRequired = 1000.0;
    plr = energy.TotalOutputRequired / thisZEqList.HeatingCapacity(1);
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // SequentialUniformPLR Test 2b - Heating, FirstHVACIteration = false, Higher load requiring 2 equipment
    firstHVACIteration = false;
    energy.TotalOutputRequired = 2100.0;
    plr = energy.TotalOutputRequired / (thisZEqList.HeatingCapacity(1) + thisZEqList.HeatingCapacity(2));
    energy.OutputRequiredToHeatingSP = 2100.0;
    energy.OutputRequiredToCoolingSP = 2200.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // SequentialUniformPLR Test 2c - Heating, FirstHVACIteration = false, Higher load requiring 3 equipment
    firstHVACIteration = false;
    energy.TotalOutputRequired = 3600.0;
    plr = energy.TotalOutputRequired / (thisZEqList.HeatingCapacity(1) + thisZEqList.HeatingCapacity(2) + thisZEqList.HeatingCapacity(3));
    energy.OutputRequiredToHeatingSP = 3600.0;
    energy.OutputRequiredToCoolingSP = 3800.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), thisZEqList.HeatingCapacity(3) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), thisZEqList.HeatingCapacity(3) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.HeatingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.HeatingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), thisZEqList.HeatingCapacity(3) * plr);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // SequentialUniformPLR Test 3 - Cooling, FirstHVACIteration = true
    energy.TotalOutputRequired = -1000.0;
    plr = energy.TotalOutputRequired / totCoolingCapcity;
    energy.OutputRequiredToHeatingSP = -1000.0;
    energy.OutputRequiredToCoolingSP = -2000.0;
    firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), -DataSizing::FinalZoneSizing(ZoneNum).DesCoolLoad);

    // SequentialUniformPLR Test 4a - Cooling, FirstHVACIteration = false, low load requiring 1 equipment
    firstHVACIteration = false;
    energy.TotalOutputRequired = -1000.0;
    plr = energy.TotalOutputRequired / (thisZEqList.CoolingCapacity(1));
    energy.OutputRequiredToHeatingSP = -1000.0;
    energy.OutputRequiredToCoolingSP = -1200.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // SequentialUniformPLR Test 4b - Cooling, FirstHVACIteration = false, higher load requiring 2 equipment
    firstHVACIteration = false;
    energy.TotalOutputRequired = -1500.0;
    plr = energy.TotalOutputRequired / (thisZEqList.CoolingCapacity(1) + thisZEqList.CoolingCapacity(2));
    energy.OutputRequiredToHeatingSP = -1500.0;
    energy.OutputRequiredToCoolingSP = -1600.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));

    // SequentialUniformPLR Test 4c - Cooling, FirstHVACIteration = false, high load requiring mode than 2 equipment, but only 2 are active for
    // cooling
    firstHVACIteration = false;
    energy.TotalOutputRequired = -2500.0;
    plr = energy.TotalOutputRequired / (thisZEqList.CoolingCapacity(1) + thisZEqList.CoolingCapacity(2));
    energy.OutputRequiredToHeatingSP = -2500.0;
    energy.OutputRequiredToCoolingSP = -2600.0;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    EXPECT_EQ(energy.SequencedOutputRequired(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequired(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), 0.0);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), thisZEqList.CoolingCapacity(1) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), thisZEqList.CoolingCapacity(2) * plr);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), 0.0);
    // also expect remaining load to match first load here
    EXPECT_EQ(energy.RemainingOutputRequired, energy.SequencedOutputRequired(1));
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, energy.SequencedOutputRequiredToHeatingSP(1));
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, energy.SequencedOutputRequiredToCoolingSP(1));
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeSequentialLoad_MixedEquip)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Space;                   !- Name",

        "ZoneHVAC:EquipmentConnections,",
        " Space,                    !- Zone Name",
        " Space Equipment,          !- Zone Conditioning Equipment List Name",
        " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
        " Space Exhaust Nodes,      !- Zone Air Exhaust Node or NodeList Name",
        " Space Node,               !- Zone Air Node Name",
        " Space Ret Node;           !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        " Space Equipment,          !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 1 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:IdealLoadsAirSystem,",
        " Ideal System A,           !- Name",
        " 2,                        !- Zone Equipment 2 Cooling Sequence",
        " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 2 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
        " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
        " 3,                        !- Zone Equipment 3 Cooling Sequence",
        " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
        " ,                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",
        " ZoneHVAC:IdealLoadsAirSystem,",
        " Ideal System B,           !- Name",
        " 4,                        !- Zone Equipment 4 Cooling Sequence",
        " 4,                        !- Zone Equipment 4 Heating or No-Load Sequence",
        " ,                         !- Zone Equipment 4 Sequential Cooling Fraction",
        " ;                         !- Zone Equipment 4 Sequential Heating or No-Load Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 1 ADU,       !- Name",
        " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 1;           !- Air Terminal Name",

        "ZoneHVAC:IdealLoadsAirSystem,",
        " Ideal System A,           !- Name",
        " ,                         !- Availability Schedule Name",
        " Zone Equip Inlet 2,       !- Zone Supply Air Node Name",
        " Zone Equip Exhaust 2;     !- Zone Exhaust Air Node Name",

        "ZoneHVAC:AirDistributionUnit,",
        " Air Terminal 3 ADU,       !- Name",
        " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
        " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        " Air Terminal 3;           !- Air Terminal Name",

        "ZoneHVAC:IdealLoadsAirSystem,",
        " Ideal System B,           !- Name",
        " ,                         !- Availability Schedule Name",
        " Zone Equip Inlet 4,       !- Zone Supply Air Node Name",
        " Zone Equip Exhaust 4;     !- Zone Exhaust Air Node Name",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 1,          !- Name",
        " ,    !- Availability Schedule Name",
        " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 1,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        " Air Terminal 3,          !- Name",
        " ,                        !- Availability Schedule Name",
        " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
        " Zone Equip Inlet 3,      !- Air Outlet Node Name",
        " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

        "NodeList,",
        "  Space Inlet Nodes,       !- Name",
        "  Zone Equip Inlet 1,      !- Node 1 Name",
        "  Zone Equip Inlet 2,      !- Node 2 Name",
        "  Zone Equip Inlet 3,      !- Node 3 Name",
        "  Zone Equip Inlet 4;      !- Node 4 Name",

        "NodeList,",
        "  Space Exhaust Nodes,       !- Name",
        "  Zone Equip Exhaust 2,      !- Node 1 Name",
        "  Zone Equip Exhaust 4;      !- Node 2 Name",

        });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    int NumEquip = 4;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).NumZoneEquipment = NumEquip;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(NumEquip);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));
    state->dataZoneEquipmentManager->PrioritySimOrder.allocate(NumEquip);

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(4), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(4), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(4), energy.OutputRequiredToCoolingSP);

    // Check sequenced load processing for unitary systems
    // EquipIndex doesn't get set until the units are simulated, so hard-wire them here
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(4) = 2;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 1);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 3);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 3);

    // Sequential Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    SetZoneEquipSimOrder(*state, ZoneNum, ZoneNum);
    DistributeSystemOutputRequired(*state, ZoneNum, firstHVACIteration);
    // Equipment 1 provides 100W of heating
    Real64 SysOutputProvided = 100.0;
    Real64 LatOutputProvided = 0.0;
    int EquipNum = 1;
    UpdateSystemOutputRequired(*state, ZoneNum, SysOutputProvided, LatOutputProvided, EquipNum);

    // Expect next sequenced load #2 to be Total minus SysOutputProvided here, others unchanged
    Real64 expectedHeatLoad = energy.OutputRequiredToHeatingSP - SysOutputProvided;
    Real64 expectedCoolLoad = energy.OutputRequiredToCoolingSP - SysOutputProvided;
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(2), expectedHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(4), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), expectedHeatLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(4), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), expectedCoolLoad);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(4), energy.OutputRequiredToCoolingSP);
    // also expect remaining load to Total minus SysOutputProvided here
    EXPECT_EQ(energy.RemainingOutputRequired, expectedHeatLoad);
    EXPECT_EQ(energy.RemainingOutputReqToHeatSP, expectedHeatLoad);
    EXPECT_EQ(energy.RemainingOutputReqToCoolSP, expectedCoolLoad);

}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeSequentialLoad_MixedEquip_WithFractions)
{

    std::string const idf_objects = delimited_string({
         "Zone,",
         "  Space;                   !- Name",

         "ZoneHVAC:EquipmentConnections,",
         " Space,                    !- Zone Name",
         " Space Equipment,          !- Zone Conditioning Equipment List Name",
         " Space Inlet Nodes,        !- Zone Air Inlet Node or NodeList Name",
         " Space Exhaust Nodes,      !- Zone Air Exhaust Node or NodeList Name",
         " Space Node,               !- Zone Air Node Name",
         " Space Ret Node;           !- Zone Return Air Node Name",

         "ScheduleTypeLimits,",
         "Fraction,       !- Name",
         "0.0,            !- Lower limit value",
         "1.0,            !- Upper limit value",
         "Continuous,     !- Numeric Type",
         "Dimensionless;  !- Unit Type",

         "Schedule:Constant,",
         "Air Terminal 1 ADU Cooling Fraction,",
         "Fraction,",
         "0.3;",

         "Schedule:Constant,",
         "Air Terminal 1 ADU Heating Fraction,",
         "Fraction,",
         "0.4;",

         "Schedule:Constant,",
         "Ideal System A Cooling Fraction,",
         "Fraction,",
         "0.5;",

         "Schedule:Constant,",
         "Ideal System A Heating Fraction,",
         "Fraction,",
         "0.6;",

         "ZoneHVAC:EquipmentList,",
         " Space Equipment,          !- Name",
         " SequentialLoad,           !- Load Distribution Scheme",
         " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
         " Air Terminal 1 ADU,       !- Zone Equipment 1 Name",
         " 1,                        !- Zone Equipment 1 Cooling Sequence",
         " 1,                        !- Zone Equipment 1 Heating or No-Load Sequence",
         " Air Terminal 1 ADU Cooling Fraction,        !- Zone Equipment 1 Sequential Cooling Fraction",
         " Air Terminal 1 ADU Heating Fraction,        !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
         " ZoneHVAC:IdealLoadsAirSystem,",
         " Ideal System A,           !- Name",
         " 2,                        !- Zone Equipment 2 Cooling Sequence",
         " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
         " Ideal System A Cooling Fraction,                         !- Zone Equipment 2 Sequential Cooling Fraction",
         " Ideal System A Heating Fraction,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
         " ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type",
         " Air Terminal 3 ADU,       !- Zone Equipment 3 Name",
         " 3,                        !- Zone Equipment 3 Cooling Sequence",
         " 3,                        !- Zone Equipment 3 Heating or No-Load Sequence",
         " ,                         !- Zone Equipment 3 Sequential Cooling Fraction",
         " ,                         !- Zone Equipment 3 Sequential Heating or No-Load Fraction",
         " ZoneHVAC:IdealLoadsAirSystem,",
         " Ideal System B,           !- Name",
         " 4,                        !- Zone Equipment 4 Cooling Sequence",
         " 4,                        !- Zone Equipment 4 Heating or No-Load Sequence",
         " ,                         !- Zone Equipment 4 Sequential Cooling Fraction",
         " ;                         !- Zone Equipment 4 Sequential Heating or No-Load Fraction",

         "ZoneHVAC:AirDistributionUnit,",
         " Air Terminal 1 ADU,       !- Name",
         " Zone Equip Inlet 1,       !- Air Distribution Unit Outlet Node Name",
         " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
         " Air Terminal 1;           !- Air Terminal Name",

         "ZoneHVAC:IdealLoadsAirSystem,",
         " Ideal System A,           !- Name",
         " ,                         !- Availability Schedule Name",
         " Zone Equip Inlet 2,       !- Zone Supply Air Node Name",
         " Zone Equip Exhaust 2;     !- Zone Exhaust Air Node Name",

         "ZoneHVAC:AirDistributionUnit,",
         " Air Terminal 3 ADU,       !- Name",
         " Zone Equip Inlet 3,       !- Air Distribution Unit Outlet Node Name",
         " AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
         " Air Terminal 3;           !- Air Terminal Name",

         "ZoneHVAC:IdealLoadsAirSystem,",
         " Ideal System B,           !- Name",
         " ,                         !- Availability Schedule Name",
         " Zone Equip Inlet 4,       !- Zone Supply Air Node Name",
         " Zone Equip Exhaust 4;     !- Zone Exhaust Air Node Name",

         "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
         " Air Terminal 1,          !- Name",
         " ,    !- Availability Schedule Name",
         " Zone Equip Inlet 1 2AT,  !- Air Inlet Node Name",
         " Zone Equip Inlet 1,      !- Air Outlet Node Name",
         " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

         "AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
         " Air Terminal 3,          !- Name",
         " ,                        !- Availability Schedule Name",
         " Zone Equip Inlet 3 2AT,  !- Air Inlet Node Name",
         " Zone Equip Inlet 3,      !- Air Outlet Node Name",
         " 0.2;                     !- Maximum Air Flow Rate {m3/s}",

         "NodeList,",
         "  Space Inlet Nodes,       !- Name",
         "  Zone Equip Inlet 1,      !- Node 1 Name",
         "  Zone Equip Inlet 2,      !- Node 2 Name",
         "  Zone Equip Inlet 3,      !- Node 3 Name",
         "  Zone Equip Inlet 4;      !- Node 4 Name",

         "NodeList,",
         "  Space Exhaust Nodes,       !- Name",
         "  Zone Equip Exhaust 2,      !- Node 1 Name",
         "  Zone Equip Exhaust 4;      !- Node 2 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    bool ErrorsFound = false;
    GetZoneData(*state, ErrorsFound);
    AllocateHeatBalArrays(*state);
    GetZoneEquipmentData1(*state);
    ZoneEquipInputsFilled = true;
    int ZoneNum = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = false;
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    int NumEquip = 4;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).NumZoneEquipment = NumEquip;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequired.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequired.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToHumidSP.allocate(NumEquip);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).SequencedOutputRequiredToDehumidSP.allocate(NumEquip);
    auto &energy(DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum));
    state->dataZoneEquipmentManager->PrioritySimOrder.allocate(NumEquip);

    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "AIR TERMINAL 1 ADU COOLING FRACTION")).CurrentValue = 0.3;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "AIR TERMINAL 1 ADU HEATING FRACTION")).CurrentValue = 0.4;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "IDEAL SYSTEM A COOLING FRACTION")).CurrentValue = 0.5;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex(*state, "IDEAL SYSTEM A HEATING FRACTION")).CurrentValue = 0.6;

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    EXPECT_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired * 0.4);
    EXPECT_EQ(energy.SequencedOutputRequired(2), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(3), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequired(4), energy.TotalOutputRequired);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP * 0.4);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(2), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(3), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToHeatingSP(4), energy.OutputRequiredToHeatingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP * 0.4);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(2), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(3), energy.OutputRequiredToCoolingSP);
    EXPECT_EQ(energy.SequencedOutputRequiredToCoolingSP(4), energy.OutputRequiredToCoolingSP);

    // Check sequenced load processing for unitary systems
    // EquipIndex doesn't get set until the units are simulated, so hard-wire them here
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(4) = 2;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 1);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesForInletNode(*state, zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 3);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 3);

    // Sequential Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(*state, ZoneNum, firstHVACIteration, true);
    // Equipment 1 provides 100W of heating
    Real64 SysOutputProvided = 100.0;
    Real64 LatOutputProvided = 0.0;
    int EquipNum = 1;
    UpdateSystemOutputRequired(*state, ZoneNum, SysOutputProvided, LatOutputProvided, EquipNum);

    // Expect next sequenced load fractions to be applied here on the first and second equipments
    Real64 expectedHeatLoad = energy.UnadjRemainingOutputReqToHeatSP * 0.6;
    Real64 expectedCoolLoad = energy.UnadjRemainingOutputReqToCoolSP * 0.6;
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequired(1), energy.TotalOutputRequired * 0.4);
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequired(2), expectedHeatLoad);
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequiredToHeatingSP(1), energy.OutputRequiredToHeatingSP * 0.4);
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequiredToHeatingSP(2), expectedHeatLoad);
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequiredToCoolingSP(1), energy.OutputRequiredToCoolingSP * 0.4);
    EXPECT_DOUBLE_EQ(energy.SequencedOutputRequiredToCoolingSP(2), expectedCoolLoad);
    EXPECT_DOUBLE_EQ(energy.RemainingOutputRequired, expectedHeatLoad);
    EXPECT_DOUBLE_EQ(energy.RemainingOutputReqToHeatSP, expectedHeatLoad);
    EXPECT_DOUBLE_EQ(energy.RemainingOutputReqToCoolSP, expectedCoolLoad);

}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_RezeroZoneSizingArrays) {

    state->dataEnvrn->TotDesDays = 12;
    state->dataEnvrn->TotRunDesPersDays = 3;
    int totDesDays = state->dataEnvrn->TotDesDays + state->dataEnvrn->TotRunDesPersDays;
    state->dataGlobal->NumOfZones = 5;
    state->dataZoneEquipmentManager->NumOfTimeStepInDay = 4;
    DataSizing::ZoneSizing.allocate(totDesDays, state->dataGlobal->NumOfZones);
    DataSizing::CalcZoneSizing.allocate(totDesDays, state->dataGlobal->NumOfZones);
    DataSizing::FinalZoneSizing.allocate(state->dataGlobal->NumOfZones);
    DataSizing::CalcFinalZoneSizing.allocate(state->dataGlobal->NumOfZones);

    for (int CtrlZoneNum = 1; CtrlZoneNum <= state->dataGlobal->NumOfZones; ++CtrlZoneNum) {
        for (int DesDayNum = 1; DesDayNum <= state->dataEnvrn->TotDesDays + state->dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            auto &thisSizingType(DataSizing::ZoneSizing(DesDayNum, CtrlZoneNum));
            thisSizingType.ZoneName = "test";
            thisSizingType.ADUName = "test";
            thisSizingType.CoolDesDay = "test";
            thisSizingType.HeatDesDay = "test";
            thisSizingType.cHeatDDDate = "test";
            thisSizingType.cCoolDDDate = "test";
            thisSizingType.AccountForDOAS = true;
            thisSizingType.EMSOverrideDesHeatMassOn = true;
            thisSizingType.EMSOverrideDesCoolMassOn = true;
            thisSizingType.EMSOverrideDesHeatLoadOn = true;
            thisSizingType.EMSOverrideDesCoolLoadOn = true;
            thisSizingType.EMSOverrideDesHeatVolOn = true;
            thisSizingType.EMSOverrideDesCoolVolOn = true;
            thisSizingType.ZnCoolDgnSAMethod = 1;
            thisSizingType.ZnHeatDgnSAMethod = 1;
            thisSizingType.ZoneDesignSpecOAIndex = 1;
            thisSizingType.OADesMethod = 1;
            thisSizingType.CoolAirDesMethod = 1;
            thisSizingType.HeatAirDesMethod = 1;
            thisSizingType.DOASControlStrategy = 1;
            thisSizingType.ActualZoneNum = 1;
            thisSizingType.TimeStepNumAtHeatMax = 1;
            thisSizingType.TimeStepNumAtCoolMax = 1;
            thisSizingType.HeatDDNum = 1;
            thisSizingType.CoolDDNum = 1;
            thisSizingType.CoolDesTemp = 1.0;
            thisSizingType.HeatDesTemp = 1.0;
            thisSizingType.CoolDesTempDiff = 1.0;
            thisSizingType.HeatDesTempDiff = 1.0;
            thisSizingType.CoolDesHumRat = 1.0;
            thisSizingType.HeatDesHumRat = 1.0;
            thisSizingType.DesOAFlowPPer = 1.0;
            thisSizingType.DesOAFlowPerArea = 1.0;
            thisSizingType.DesOAFlow = 1.0;
            thisSizingType.InpDesCoolAirFlow = 1.0;
            thisSizingType.DesCoolMinAirFlowPerArea = 1.0;
            thisSizingType.DesCoolMinAirFlow = 1.0;
            thisSizingType.DesCoolMinAirFlowFrac = 1.0;
            thisSizingType.InpDesHeatAirFlow = 1.0;
            thisSizingType.DesHeatMaxAirFlowPerArea = 1.0;
            thisSizingType.DesHeatMaxAirFlow = 1.0;
            thisSizingType.DesHeatMaxAirFlowFrac = 1.0;
            thisSizingType.HeatSizingFactor = 1.0;
            thisSizingType.CoolSizingFactor = 1.0;
            thisSizingType.DOASLowSetpoint = 1.0;
            thisSizingType.DOASHighSetpoint = 1.0;
            thisSizingType.DesHeatMassFlow = 1.0;
            thisSizingType.DesHeatMassFlowNoOA = 1.0;
            thisSizingType.DesHeatOAFlowFrac = 1.0;
            thisSizingType.EMSValueDesHeatMassFlow = 1.0;
            thisSizingType.DesCoolMassFlow = 1.0;
            thisSizingType.DesCoolMassFlowNoOA = 1.0;
            thisSizingType.DesCoolOAFlowFrac = 1.0;
            thisSizingType.EMSValueDesCoolMassFlow = 1.0;
            thisSizingType.DesHeatLoad = 1.0;
            thisSizingType.NonAirSysDesHeatLoad = 1.0;
            thisSizingType.EMSValueDesHeatLoad = 1.0;
            thisSizingType.DesCoolLoad = 1.0;
            thisSizingType.NonAirSysDesCoolLoad = 1.0;
            thisSizingType.EMSValueDesCoolLoad = 1.0;
            thisSizingType.DesHeatDens = 1.0;
            thisSizingType.DesCoolDens = 1.0;
            thisSizingType.DesHeatVolFlow = 1.0;
            thisSizingType.DesHeatVolFlowNoOA = 1.0;
            thisSizingType.NonAirSysDesHeatVolFlow = 1.0;
            thisSizingType.EMSValueDesHeatVolFlow = 1.0;
            thisSizingType.DesCoolVolFlow = 1.0;
            thisSizingType.DesCoolVolFlowNoOA = 1.0;
            thisSizingType.NonAirSysDesCoolVolFlow = 1.0;
            thisSizingType.EMSValueDesCoolVolFlow = 1.0;
            thisSizingType.DesHeatVolFlowMax = 1.0;
            thisSizingType.DesCoolVolFlowMin = 1.0;
            thisSizingType.DesHeatCoilInTemp = 1.0;
            thisSizingType.DesCoolCoilInTemp = 1.0;
            thisSizingType.DesHeatCoilInHumRat = 1.0;
            thisSizingType.DesCoolCoilInHumRat = 1.0;
            thisSizingType.DesHeatCoilInTempTU = 1.0;
            thisSizingType.DesCoolCoilInTempTU = 1.0;
            thisSizingType.DesHeatCoilInHumRatTU = 1.0;
            thisSizingType.DesCoolCoilInHumRatTU = 1.0;
            thisSizingType.HeatMassFlow = 1.0;
            thisSizingType.CoolMassFlow = 1.0;
            thisSizingType.HeatLoad = 1.0;
            thisSizingType.CoolLoad = 1.0;
            thisSizingType.HeatZoneTemp = 1.0;
            thisSizingType.HeatOutTemp = 1.0;
            thisSizingType.HeatZoneRetTemp = 1.0;
            thisSizingType.HeatTstatTemp = 1.0;
            thisSizingType.CoolZoneTemp = 1.0;
            thisSizingType.CoolOutTemp = 1.0;
            thisSizingType.CoolZoneRetTemp = 1.0;
            thisSizingType.CoolTstatTemp = 1.0;
            thisSizingType.HeatZoneHumRat = 1.0;
            thisSizingType.CoolZoneHumRat = 1.0;
            thisSizingType.HeatOutHumRat = 1.0;
            thisSizingType.CoolOutHumRat = 1.0;
            thisSizingType.ZoneTempAtHeatPeak = 1.0;
            thisSizingType.ZoneRetTempAtHeatPeak = 1.0;
            thisSizingType.OutTempAtHeatPeak = 1.0;
            thisSizingType.ZoneTempAtCoolPeak = 1.0;
            thisSizingType.ZoneRetTempAtCoolPeak = 1.0;
            thisSizingType.OutTempAtCoolPeak = 1.0;
            thisSizingType.ZoneHumRatAtHeatPeak = 1.0;
            thisSizingType.ZoneHumRatAtCoolPeak = 1.0;
            thisSizingType.OutHumRatAtHeatPeak = 1.0;
            thisSizingType.OutHumRatAtCoolPeak = 1.0;
            thisSizingType.MinOA = 1.0;
            thisSizingType.DesCoolMinAirFlow2 = 1.0;
            thisSizingType.DesHeatMaxAirFlow2 = 1.0;
            thisSizingType.ZoneADEffCooling = 1.0;
            thisSizingType.ZoneADEffHeating = 1.0;
            thisSizingType.ZoneSecondaryRecirculation = 1.0;
            thisSizingType.ZoneVentilationEff = 1.0;
            thisSizingType.ZonePrimaryAirFraction = 1.0;
            thisSizingType.ZonePrimaryAirFractionHtg = 1.0;
            thisSizingType.ZoneOAFracCooling = 1.0;
            thisSizingType.ZoneOAFracHeating = 1.0;
            thisSizingType.TotalOAFromPeople = 1.0;
            thisSizingType.TotalOAFromArea = 1.0;
            thisSizingType.TotPeopleInZone = 1.0;
            thisSizingType.TotalZoneFloorArea = 1.0;
            thisSizingType.ZonePeakOccupancy = 1.0;
            thisSizingType.SupplyAirAdjustFactor = 1.0;
            thisSizingType.ZpzClgByZone = 1.0;
            thisSizingType.ZpzHtgByZone = 1.0;
            thisSizingType.VozClgByZone = 1.0;
            thisSizingType.VozHtgByZone = 1.0;
            thisSizingType.DOASHeatLoad = 1.0;
            thisSizingType.DOASCoolLoad = 1.0;
            thisSizingType.DOASHeatAdd = 1.0;
            thisSizingType.DOASLatAdd = 1.0;
            thisSizingType.DOASSupMassFlow = 1.0;
            thisSizingType.DOASSupTemp = 1.0;
            thisSizingType.DOASSupHumRat = 1.0;
            thisSizingType.DOASTotCoolLoad = 1.0;

            thisSizingType.DOASHeatLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASCoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASHeatAddSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASLatAddSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASSupMassFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASSupTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASSupHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DOASTotCoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatFlowSeqNoOA.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolFlowSeqNoOA.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatZoneTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatOutTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatZoneRetTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatTstatTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DesHeatSetPtSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolZoneTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolOutTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolZoneRetTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolTstatTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.DesCoolSetPtSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatZoneHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolZoneHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.HeatOutHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType.CoolOutHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);

            for (int TimeStepIndex = 1; TimeStepIndex <= state->dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                thisSizingType.DOASHeatLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASCoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASHeatAddSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASLatAddSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASSupMassFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASSupTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASSupHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType.DOASTotCoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatFlowSeqNoOA(TimeStepIndex) = 1.0;
                thisSizingType.CoolFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolFlowSeqNoOA(TimeStepIndex) = 1.0;
                thisSizingType.HeatLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatZoneTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatOutTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatZoneRetTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatTstatTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.DesHeatSetPtSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolZoneTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolOutTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolZoneRetTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolTstatTempSeq(TimeStepIndex) = 1.0;
                thisSizingType.DesCoolSetPtSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatZoneHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolZoneHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType.HeatOutHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType.CoolOutHumRatSeq(TimeStepIndex) = 1.0;
            }

            auto &thisSizingType2(DataSizing::CalcZoneSizing(DesDayNum, CtrlZoneNum));
            thisSizingType2.ZoneName = "test";
            thisSizingType2.ADUName = "test";
            thisSizingType2.CoolDesDay = "test";
            thisSizingType2.HeatDesDay = "test";
            thisSizingType2.cHeatDDDate = "test";
            thisSizingType2.cCoolDDDate = "test";
            thisSizingType2.AccountForDOAS = true;
            thisSizingType2.EMSOverrideDesHeatMassOn = true;
            thisSizingType2.EMSOverrideDesCoolMassOn = true;
            thisSizingType2.EMSOverrideDesHeatLoadOn = true;
            thisSizingType2.EMSOverrideDesCoolLoadOn = true;
            thisSizingType2.EMSOverrideDesHeatVolOn = true;
            thisSizingType2.EMSOverrideDesCoolVolOn = true;
            thisSizingType2.ZnCoolDgnSAMethod = 1;
            thisSizingType2.ZnHeatDgnSAMethod = 1;
            thisSizingType2.ZoneDesignSpecOAIndex = 1;
            thisSizingType2.OADesMethod = 1;
            thisSizingType2.CoolAirDesMethod = 1;
            thisSizingType2.HeatAirDesMethod = 1;
            thisSizingType2.DOASControlStrategy = 1;
            thisSizingType2.ActualZoneNum = 1;
            thisSizingType2.TimeStepNumAtHeatMax = 1;
            thisSizingType2.TimeStepNumAtCoolMax = 1;
            thisSizingType2.HeatDDNum = 1;
            thisSizingType2.CoolDDNum = 1;
            thisSizingType2.CoolDesTemp = 1.0;
            thisSizingType2.HeatDesTemp = 1.0;
            thisSizingType2.CoolDesTempDiff = 1.0;
            thisSizingType2.HeatDesTempDiff = 1.0;
            thisSizingType2.CoolDesHumRat = 1.0;
            thisSizingType2.HeatDesHumRat = 1.0;
            thisSizingType2.DesOAFlowPPer = 1.0;
            thisSizingType2.DesOAFlowPerArea = 1.0;
            thisSizingType2.DesOAFlow = 1.0;
            thisSizingType2.InpDesCoolAirFlow = 1.0;
            thisSizingType2.DesCoolMinAirFlowPerArea = 1.0;
            thisSizingType2.DesCoolMinAirFlow = 1.0;
            thisSizingType2.DesCoolMinAirFlowFrac = 1.0;
            thisSizingType2.InpDesHeatAirFlow = 1.0;
            thisSizingType2.DesHeatMaxAirFlowPerArea = 1.0;
            thisSizingType2.DesHeatMaxAirFlow = 1.0;
            thisSizingType2.DesHeatMaxAirFlowFrac = 1.0;
            thisSizingType2.HeatSizingFactor = 1.0;
            thisSizingType2.CoolSizingFactor = 1.0;
            thisSizingType2.DOASLowSetpoint = 1.0;
            thisSizingType2.DOASHighSetpoint = 1.0;
            thisSizingType2.DesHeatMassFlow = 1.0;
            thisSizingType2.DesHeatMassFlowNoOA = 1.0;
            thisSizingType2.DesHeatOAFlowFrac = 1.0;
            thisSizingType2.EMSValueDesHeatMassFlow = 1.0;
            thisSizingType2.DesCoolMassFlow = 1.0;
            thisSizingType2.DesCoolMassFlowNoOA = 1.0;
            thisSizingType2.DesCoolOAFlowFrac = 1.0;
            thisSizingType2.EMSValueDesCoolMassFlow = 1.0;
            thisSizingType2.DesHeatLoad = 1.0;
            thisSizingType2.NonAirSysDesHeatLoad = 1.0;
            thisSizingType2.EMSValueDesHeatLoad = 1.0;
            thisSizingType2.DesCoolLoad = 1.0;
            thisSizingType2.NonAirSysDesCoolLoad = 1.0;
            thisSizingType2.EMSValueDesCoolLoad = 1.0;
            thisSizingType2.DesHeatDens = 1.0;
            thisSizingType2.DesCoolDens = 1.0;
            thisSizingType2.DesHeatVolFlow = 1.0;
            thisSizingType2.DesHeatVolFlowNoOA = 1.0;
            thisSizingType2.NonAirSysDesHeatVolFlow = 1.0;
            thisSizingType2.EMSValueDesHeatVolFlow = 1.0;
            thisSizingType2.DesCoolVolFlow = 1.0;
            thisSizingType2.DesCoolVolFlowNoOA = 1.0;
            thisSizingType2.NonAirSysDesCoolVolFlow = 1.0;
            thisSizingType2.EMSValueDesCoolVolFlow = 1.0;
            thisSizingType2.DesHeatVolFlowMax = 1.0;
            thisSizingType2.DesCoolVolFlowMin = 1.0;
            thisSizingType2.DesHeatCoilInTemp = 1.0;
            thisSizingType2.DesCoolCoilInTemp = 1.0;
            thisSizingType2.DesHeatCoilInHumRat = 1.0;
            thisSizingType2.DesCoolCoilInHumRat = 1.0;
            thisSizingType2.DesHeatCoilInTempTU = 1.0;
            thisSizingType2.DesCoolCoilInTempTU = 1.0;
            thisSizingType2.DesHeatCoilInHumRatTU = 1.0;
            thisSizingType2.DesCoolCoilInHumRatTU = 1.0;
            thisSizingType2.HeatMassFlow = 1.0;
            thisSizingType2.CoolMassFlow = 1.0;
            thisSizingType2.HeatLoad = 1.0;
            thisSizingType2.CoolLoad = 1.0;
            thisSizingType2.HeatZoneTemp = 1.0;
            thisSizingType2.HeatOutTemp = 1.0;
            thisSizingType2.HeatZoneRetTemp = 1.0;
            thisSizingType2.HeatTstatTemp = 1.0;
            thisSizingType2.CoolZoneTemp = 1.0;
            thisSizingType2.CoolOutTemp = 1.0;
            thisSizingType2.CoolZoneRetTemp = 1.0;
            thisSizingType2.CoolTstatTemp = 1.0;
            thisSizingType2.HeatZoneHumRat = 1.0;
            thisSizingType2.CoolZoneHumRat = 1.0;
            thisSizingType2.HeatOutHumRat = 1.0;
            thisSizingType2.CoolOutHumRat = 1.0;
            thisSizingType2.ZoneTempAtHeatPeak = 1.0;
            thisSizingType2.ZoneRetTempAtHeatPeak = 1.0;
            thisSizingType2.OutTempAtHeatPeak = 1.0;
            thisSizingType2.ZoneTempAtCoolPeak = 1.0;
            thisSizingType2.ZoneRetTempAtCoolPeak = 1.0;
            thisSizingType2.OutTempAtCoolPeak = 1.0;
            thisSizingType2.ZoneHumRatAtHeatPeak = 1.0;
            thisSizingType2.ZoneHumRatAtCoolPeak = 1.0;
            thisSizingType2.OutHumRatAtHeatPeak = 1.0;
            thisSizingType2.OutHumRatAtCoolPeak = 1.0;
            thisSizingType2.MinOA = 1.0;
            thisSizingType2.DesCoolMinAirFlow2 = 1.0;
            thisSizingType2.DesHeatMaxAirFlow2 = 1.0;
            thisSizingType2.ZoneADEffCooling = 1.0;
            thisSizingType2.ZoneADEffHeating = 1.0;
            thisSizingType2.ZoneSecondaryRecirculation = 1.0;
            thisSizingType2.ZoneVentilationEff = 1.0;
            thisSizingType2.ZonePrimaryAirFraction = 1.0;
            thisSizingType2.ZonePrimaryAirFractionHtg = 1.0;
            thisSizingType2.ZoneOAFracCooling = 1.0;
            thisSizingType2.ZoneOAFracHeating = 1.0;
            thisSizingType2.TotalOAFromPeople = 1.0;
            thisSizingType2.TotalOAFromArea = 1.0;
            thisSizingType2.TotPeopleInZone = 1.0;
            thisSizingType2.TotalZoneFloorArea = 1.0;
            thisSizingType2.ZonePeakOccupancy = 1.0;
            thisSizingType2.SupplyAirAdjustFactor = 1.0;
            thisSizingType2.ZpzClgByZone = 1.0;
            thisSizingType2.ZpzHtgByZone = 1.0;
            thisSizingType2.VozClgByZone = 1.0;
            thisSizingType2.VozHtgByZone = 1.0;
            thisSizingType2.DOASHeatLoad = 1.0;
            thisSizingType2.DOASCoolLoad = 1.0;
            thisSizingType2.DOASHeatAdd = 1.0;
            thisSizingType2.DOASLatAdd = 1.0;
            thisSizingType2.DOASSupMassFlow = 1.0;
            thisSizingType2.DOASSupTemp = 1.0;
            thisSizingType2.DOASSupHumRat = 1.0;
            thisSizingType2.DOASTotCoolLoad = 1.0;

            thisSizingType2.DOASHeatLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASCoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASHeatAddSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASLatAddSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASSupMassFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASSupTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASSupHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DOASTotCoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatFlowSeqNoOA.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolFlowSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolFlowSeqNoOA.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolLoadSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatZoneTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatOutTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatZoneRetTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatTstatTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DesHeatSetPtSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolZoneTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolOutTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolZoneRetTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolTstatTempSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.DesCoolSetPtSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatZoneHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolZoneHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.HeatOutHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);
            thisSizingType2.CoolOutHumRatSeq.allocate(state->dataZoneEquipmentManager->NumOfTimeStepInDay);

            for ( int TimeStepIndex = 1; TimeStepIndex <= state->dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex ) {
                thisSizingType2.DOASHeatLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASCoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASHeatAddSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASLatAddSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASSupMassFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASSupTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASSupHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DOASTotCoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatFlowSeqNoOA(TimeStepIndex) = 1.0;
                thisSizingType2.CoolFlowSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolFlowSeqNoOA(TimeStepIndex) = 1.0;
                thisSizingType2.HeatLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolLoadSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatZoneTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatOutTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatZoneRetTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatTstatTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DesHeatSetPtSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolZoneTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolOutTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolZoneRetTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolTstatTempSeq(TimeStepIndex) = 1.0;
                thisSizingType2.DesCoolSetPtSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatZoneHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolZoneHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType2.HeatOutHumRatSeq(TimeStepIndex) = 1.0;
                thisSizingType2.CoolOutHumRatSeq(TimeStepIndex) = 1.0;
            }
        }
    }

    ZoneEquipmentManager::RezeroZoneSizingArrays(*state);

    for ( int CtrlZoneNum = 1; CtrlZoneNum <= state->dataGlobal->NumOfZones; ++CtrlZoneNum ) {
        for ( int DesDayNum = 1; DesDayNum <= state->dataEnvrn->TotDesDays + state->dataEnvrn->TotRunDesPersDays; ++DesDayNum ) {
            auto &thisSizingType(DataSizing::ZoneSizing(DesDayNum, CtrlZoneNum));
            //EXPECT_EQ(thisSizingType.ZoneName, "");
            //EXPECT_EQ(thisSizingType.ADUName, "");
            EXPECT_EQ(thisSizingType.CoolDesDay, "");
            EXPECT_EQ(thisSizingType.HeatDesDay, "");
            EXPECT_EQ(thisSizingType.cHeatDDDate, "");
            EXPECT_EQ(thisSizingType.cCoolDDDate, "");
            //EXPECT_FALSE(thisSizingType.AccountForDOAS);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesHeatMassOn);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesCoolMassOn);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesHeatLoadOn);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesCoolLoadOn);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesHeatVolOn);
            //EXPECT_FALSE(thisSizingType.EMSOverrideDesCoolVolOn);
            //EXPECT_EQ(thisSizingType.ZnCoolDgnSAMethod, 0);
            //EXPECT_EQ(thisSizingType.ZnHeatDgnSAMethod, 0);
            //EXPECT_EQ(thisSizingType.ZoneDesignSpecOAIndex, 0);
            //EXPECT_EQ(thisSizingType.OADesMethod, 0);
            //EXPECT_EQ(thisSizingType.CoolAirDesMethod, 0);
            //EXPECT_EQ(thisSizingType.HeatAirDesMethod, 0);
            //EXPECT_EQ(thisSizingType.DOASControlStrategy, 0);
            //EXPECT_EQ(thisSizingType.ActualZoneNum, 0);
            EXPECT_EQ(thisSizingType.TimeStepNumAtHeatMax, 0);
            EXPECT_EQ(thisSizingType.TimeStepNumAtCoolMax, 0);
            EXPECT_EQ(thisSizingType.HeatDDNum, 0);
            EXPECT_EQ(thisSizingType.CoolDDNum, 0);
            //EXPECT_EQ(thisSizingType.CoolDesTemp, 0.0);
            //EXPECT_EQ(thisSizingType.HeatDesTemp, 0.0);
            //EXPECT_EQ(thisSizingType.CoolDesTempDiff, 0.0);
            //EXPECT_EQ(thisSizingType.HeatDesTempDiff, 0.0);
            //EXPECT_EQ(thisSizingType.CoolDesHumRat, 0.0);
            //EXPECT_EQ(thisSizingType.HeatDesHumRat, 0.0);
            //EXPECT_EQ(thisSizingType.DesOAFlowPPer, 0.0);
            //EXPECT_EQ(thisSizingType.DesOAFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType.DesOAFlow, 0.0);
            //EXPECT_EQ(thisSizingType.InpDesCoolAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolMinAirFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolMinAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolMinAirFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType.InpDesHeatAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatMaxAirFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatMaxAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatMaxAirFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType.HeatSizingFactor, 0.0);
            //EXPECT_EQ(thisSizingType.CoolSizingFactor, 0.0);
            //EXPECT_EQ(thisSizingType.DOASLowSetpoint, 0.0);
            //EXPECT_EQ(thisSizingType.DOASHighSetpoint, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatMassFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatMassFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatOAFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesHeatMassFlow, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolMassFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolMassFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolOAFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesCoolMassFlow, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatLoad, 0.0);
            //EXPECT_EQ(thisSizingType.NonAirSysDesHeatLoad, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesHeatLoad, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType.NonAirSysDesCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesCoolLoad, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatDens, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolDens, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatVolFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType.NonAirSysDesHeatVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesHeatVolFlow, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolVolFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType.NonAirSysDesCoolVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType.EMSValueDesCoolVolFlow, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatVolFlowMax, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolVolFlowMin, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatCoilInTemp, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolCoilInTemp, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatCoilInHumRat, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolCoilInHumRat, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatCoilInTempTU, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolCoilInTempTU, 0.0);
            EXPECT_EQ(thisSizingType.DesHeatCoilInHumRatTU, 0.0);
            EXPECT_EQ(thisSizingType.DesCoolCoilInHumRatTU, 0.0);
            EXPECT_EQ(thisSizingType.HeatMassFlow, 0.0);
            EXPECT_EQ(thisSizingType.CoolMassFlow, 0.0);
            EXPECT_EQ(thisSizingType.HeatLoad, 0.0);
            EXPECT_EQ(thisSizingType.CoolLoad, 0.0);
            EXPECT_EQ(thisSizingType.HeatZoneTemp, 0.0);
            EXPECT_EQ(thisSizingType.HeatOutTemp, 0.0);
            EXPECT_EQ(thisSizingType.HeatZoneRetTemp, 0.0);
            EXPECT_EQ(thisSizingType.HeatTstatTemp, 0.0);
            EXPECT_EQ(thisSizingType.CoolZoneTemp, 0.0);
            EXPECT_EQ(thisSizingType.CoolOutTemp, 0.0);
            EXPECT_EQ(thisSizingType.CoolZoneRetTemp, 0.0);
            EXPECT_EQ(thisSizingType.CoolTstatTemp, 0.0);
            EXPECT_EQ(thisSizingType.HeatZoneHumRat, 0.0);
            EXPECT_EQ(thisSizingType.CoolZoneHumRat, 0.0);
            EXPECT_EQ(thisSizingType.HeatOutHumRat, 0.0);
            EXPECT_EQ(thisSizingType.CoolOutHumRat, 0.0);
            EXPECT_EQ(thisSizingType.ZoneTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType.ZoneRetTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType.OutTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType.ZoneTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType.ZoneRetTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType.OutTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType.ZoneHumRatAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType.ZoneHumRatAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType.OutHumRatAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType.OutHumRatAtCoolPeak, 0.0);
            //EXPECT_EQ(thisSizingType.MinOA, 0.0);
            //EXPECT_EQ(thisSizingType.DesCoolMinAirFlow2, 0.0);
            //EXPECT_EQ(thisSizingType.DesHeatMaxAirFlow2, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneADEffCooling, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneADEffHeating, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneSecondaryRecirculation, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneVentilationEff, 0.0);
            //EXPECT_EQ(thisSizingType.ZonePrimaryAirFraction, 0.0);
            //EXPECT_EQ(thisSizingType.ZonePrimaryAirFractionHtg, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneOAFracCooling, 0.0);
            //EXPECT_EQ(thisSizingType.ZoneOAFracHeating, 0.0);
            //EXPECT_EQ(thisSizingType.TotalOAFromPeople, 0.0);
            //EXPECT_EQ(thisSizingType.TotalOAFromArea, 0.0);
            //EXPECT_EQ(thisSizingType.TotPeopleInZone, 0.0);
            //EXPECT_EQ(thisSizingType.TotalZoneFloorArea, 0.0);
            //EXPECT_EQ(thisSizingType.ZonePeakOccupancy, 0.0);
            //EXPECT_EQ(thisSizingType.SupplyAirAdjustFactor, 0.0);
            //EXPECT_EQ(thisSizingType.ZpzClgByZone, 0.0);
            //EXPECT_EQ(thisSizingType.ZpzHtgByZone, 0.0);
            //EXPECT_EQ(thisSizingType.VozClgByZone, 0.0);
            //EXPECT_EQ(thisSizingType.VozHtgByZone, 0.0);
            EXPECT_EQ(thisSizingType.DOASHeatLoad, 0.0);
            EXPECT_EQ(thisSizingType.DOASCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType.DOASHeatAdd, 0.0);
            //EXPECT_EQ(thisSizingType.DOASLatAdd, 0.0);
            EXPECT_EQ(thisSizingType.DOASSupMassFlow, 0.0);
            EXPECT_EQ(thisSizingType.DOASSupTemp, 0.0);
            EXPECT_EQ(thisSizingType.DOASSupHumRat, 0.0);
            EXPECT_EQ(thisSizingType.DOASTotCoolLoad, 0.0);

            for ( int TimeStepIndex = 1; TimeStepIndex <= state->dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex ) {
                EXPECT_EQ(thisSizingType.DOASHeatLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASCoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASHeatAddSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASLatAddSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASSupMassFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASSupTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASSupHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DOASTotCoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatFlowSeqNoOA(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolFlowSeqNoOA(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatZoneTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatOutTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatZoneRetTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatTstatTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DesHeatSetPtSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolZoneTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolOutTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolZoneRetTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolTstatTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.DesCoolSetPtSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatZoneHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolZoneHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.HeatOutHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType.CoolOutHumRatSeq(TimeStepIndex), 0.0);
            }

            auto &thisSizingType2(DataSizing::CalcZoneSizing(DesDayNum, CtrlZoneNum));
            //EXPECT_EQ(thisSizingType2.ZoneName, "");
            //EXPECT_EQ(thisSizingType2.ADUName, "");
            EXPECT_EQ(thisSizingType2.CoolDesDay, "");
            EXPECT_EQ(thisSizingType2.HeatDesDay, "");
            EXPECT_EQ(thisSizingType2.cHeatDDDate, "");
            EXPECT_EQ(thisSizingType2.cCoolDDDate, "");
            //EXPECT_FALSE(thisSizingType2.AccountForDOAS);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesHeatMassOn);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesCoolMassOn);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesHeatLoadOn);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesCoolLoadOn);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesHeatVolOn);
            //EXPECT_FALSE(thisSizingType2.EMSOverrideDesCoolVolOn);
            //EXPECT_EQ(thisSizingType2.ZnCoolDgnSAMethod, 0);
            //EXPECT_EQ(thisSizingType2.ZnHeatDgnSAMethod, 0);
            //EXPECT_EQ(thisSizingType2.ZoneDesignSpecOAIndex, 0);
            //EXPECT_EQ(thisSizingType2.OADesMethod, 0);
            //EXPECT_EQ(thisSizingType2.CoolAirDesMethod, 0);
            //EXPECT_EQ(thisSizingType2.HeatAirDesMethod, 0);
            //EXPECT_EQ(thisSizingType2.DOASControlStrategy, 0);
            //EXPECT_EQ(thisSizingType2.ActualZoneNum, 0);
            EXPECT_EQ(thisSizingType2.TimeStepNumAtHeatMax, 0);
            EXPECT_EQ(thisSizingType2.TimeStepNumAtCoolMax, 0);
            EXPECT_EQ(thisSizingType2.HeatDDNum, 0);
            EXPECT_EQ(thisSizingType2.CoolDDNum, 0);
            //EXPECT_EQ(thisSizingType2.CoolDesTemp, 0.0);
            //EXPECT_EQ(thisSizingType2.HeatDesTemp, 0.0);
            //EXPECT_EQ(thisSizingType2.CoolDesTempDiff, 0.0);
            //EXPECT_EQ(thisSizingType2.HeatDesTempDiff, 0.0);
            //EXPECT_EQ(thisSizingType2.CoolDesHumRat, 0.0);
            //EXPECT_EQ(thisSizingType2.HeatDesHumRat, 0.0);
            //EXPECT_EQ(thisSizingType2.DesOAFlowPPer, 0.0);
            //EXPECT_EQ(thisSizingType2.DesOAFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType2.DesOAFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.InpDesCoolAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolMinAirFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolMinAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolMinAirFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType2.InpDesHeatAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatMaxAirFlowPerArea, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatMaxAirFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatMaxAirFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType2.HeatSizingFactor, 0.0);
            //EXPECT_EQ(thisSizingType2.CoolSizingFactor, 0.0);
            //EXPECT_EQ(thisSizingType2.DOASLowSetpoint, 0.0);
            //EXPECT_EQ(thisSizingType2.DOASHighSetpoint, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatMassFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatMassFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatOAFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesHeatMassFlow, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolMassFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolMassFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolOAFlowFrac, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesCoolMassFlow, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatLoad, 0.0);
            //EXPECT_EQ(thisSizingType2.NonAirSysDesHeatLoad, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesHeatLoad, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType2.NonAirSysDesCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesCoolLoad, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatDens, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolDens, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatVolFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType2.NonAirSysDesHeatVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesHeatVolFlow, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolVolFlowNoOA, 0.0);
            //EXPECT_EQ(thisSizingType2.NonAirSysDesCoolVolFlow, 0.0);
            //EXPECT_EQ(thisSizingType2.EMSValueDesCoolVolFlow, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatVolFlowMax, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolVolFlowMin, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatCoilInTemp, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolCoilInTemp, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatCoilInHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolCoilInHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatCoilInTempTU, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolCoilInTempTU, 0.0);
            EXPECT_EQ(thisSizingType2.DesHeatCoilInHumRatTU, 0.0);
            EXPECT_EQ(thisSizingType2.DesCoolCoilInHumRatTU, 0.0);
            EXPECT_EQ(thisSizingType2.HeatMassFlow, 0.0);
            EXPECT_EQ(thisSizingType2.CoolMassFlow, 0.0);
            EXPECT_EQ(thisSizingType2.HeatLoad, 0.0);
            EXPECT_EQ(thisSizingType2.CoolLoad, 0.0);
            EXPECT_EQ(thisSizingType2.HeatZoneTemp, 0.0);
            EXPECT_EQ(thisSizingType2.HeatOutTemp, 0.0);
            EXPECT_EQ(thisSizingType2.HeatZoneRetTemp, 0.0);
            EXPECT_EQ(thisSizingType2.HeatTstatTemp, 0.0);
            EXPECT_EQ(thisSizingType2.CoolZoneTemp, 0.0);
            EXPECT_EQ(thisSizingType2.CoolOutTemp, 0.0);
            EXPECT_EQ(thisSizingType2.CoolZoneRetTemp, 0.0);
            EXPECT_EQ(thisSizingType2.CoolTstatTemp, 0.0);
            EXPECT_EQ(thisSizingType2.HeatZoneHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.CoolZoneHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.HeatOutHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.CoolOutHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneRetTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType2.OutTempAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneRetTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType2.OutTempAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneHumRatAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType2.ZoneHumRatAtCoolPeak, 0.0);
            EXPECT_EQ(thisSizingType2.OutHumRatAtHeatPeak, 0.0);
            EXPECT_EQ(thisSizingType2.OutHumRatAtCoolPeak, 0.0);
            //EXPECT_EQ(thisSizingType2.MinOA, 0.0);
            //EXPECT_EQ(thisSizingType2.DesCoolMinAirFlow2, 0.0);
            //EXPECT_EQ(thisSizingType2.DesHeatMaxAirFlow2, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneADEffCooling, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneADEffHeating, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneSecondaryRecirculation, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneVentilationEff, 0.0);
            //EXPECT_EQ(thisSizingType2.ZonePrimaryAirFraction, 0.0);
            //EXPECT_EQ(thisSizingType2.ZonePrimaryAirFractionHtg, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneOAFracCooling, 0.0);
            //EXPECT_EQ(thisSizingType2.ZoneOAFracHeating, 0.0);
            //EXPECT_EQ(thisSizingType2.TotalOAFromPeople, 0.0);
            //EXPECT_EQ(thisSizingType2.TotalOAFromArea, 0.0);
            //EXPECT_EQ(thisSizingType2.TotPeopleInZone, 0.0);
            //EXPECT_EQ(thisSizingType2.TotalZoneFloorArea, 0.0);
            //EXPECT_EQ(thisSizingType2.ZonePeakOccupancy, 0.0);
            //EXPECT_EQ(thisSizingType2.SupplyAirAdjustFactor, 0.0);
            //EXPECT_EQ(thisSizingType2.ZpzClgByZone, 0.0);
            //EXPECT_EQ(thisSizingType2.ZpzHtgByZone, 0.0);
            //EXPECT_EQ(thisSizingType2.VozClgByZone, 0.0);
            //EXPECT_EQ(thisSizingType2.VozHtgByZone, 0.0);
            EXPECT_EQ(thisSizingType2.DOASHeatLoad, 0.0);
            EXPECT_EQ(thisSizingType2.DOASCoolLoad, 0.0);
            //EXPECT_EQ(thisSizingType2.DOASHeatAdd, 0.0);
            //EXPECT_EQ(thisSizingType2.DOASLatAdd, 0.0);
            EXPECT_EQ(thisSizingType2.DOASSupMassFlow, 0.0);
            EXPECT_EQ(thisSizingType2.DOASSupTemp, 0.0);
            EXPECT_EQ(thisSizingType2.DOASSupHumRat, 0.0);
            EXPECT_EQ(thisSizingType2.DOASTotCoolLoad, 0.0);

            for ( int TimeStepIndex = 1; TimeStepIndex <= state->dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex ) {
                EXPECT_EQ(thisSizingType2.DOASHeatLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASCoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASHeatAddSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASLatAddSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASSupMassFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASSupTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASSupHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DOASTotCoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatFlowSeqNoOA(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolFlowSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolFlowSeqNoOA(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolLoadSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatZoneTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatOutTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatZoneRetTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatTstatTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DesHeatSetPtSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolZoneTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolOutTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolZoneRetTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolTstatTempSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.DesCoolSetPtSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatZoneHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolZoneHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.HeatOutHumRatSeq(TimeStepIndex), 0.0);
                EXPECT_EQ(thisSizingType2.CoolOutHumRatSeq(TimeStepIndex), 0.0);
            }
        }
    }
}
