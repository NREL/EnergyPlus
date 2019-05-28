// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(ErrorsFound);
    int ZoneNum = 1;
    int NodeNum;
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }

    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 0;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 1;
    // Avoid zero values in volume flow balance check
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    // Test here - if zone equipment exhausts slightly more than it supplies, there should be no unbalanced exhaust flow warning
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 1.000000001;
    CalcZoneMassBalance(false);
    EXPECT_FALSE(has_err_output());

    // Add excess balanced zone exhaust from exhaust fan, still no warning
    ZoneEquipConfig(ZoneNum).ZoneExh = 0.5;
    ZoneEquipConfig(ZoneNum).ZoneExhBalanced = 0.5;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(2)).MassFlowRate = 0.5;
    CalcZoneMassBalance(false);
    EXPECT_FALSE(has_err_output());

    // Add excess unbalanced zone exhaust from exhaust fan, now there should be warning
    ZoneEquipConfig(ZoneNum).ZoneExh = 0.5;
    ZoneEquipConfig(ZoneNum).ZoneExhBalanced = 0.0;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(2)).MassFlowRate = 0.5;
    CalcZoneMassBalance(false);
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
    ScheduleManager::ProcessScheduleInput();
    GetZoneData(ErrorsFound);
    DataHeatBalFanSys::ZoneReOrder.allocate(NumOfZones);

    GetSimpleAirModelInputs(ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    DataHeatBalFanSys::MAT.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(NumOfZones);
    DataHeatBalFanSys::MCPM.allocate(NumOfZones);
    DataHeatBalFanSys::MCPTM.allocate(NumOfZones);

    DataHeatBalFanSys::MCPI.allocate(NumOfZones);
    DataHeatBalFanSys::OAMFL.allocate(NumOfZones);
    DataHeatBalFanSys::MCPTI.allocate(NumOfZones);

    DataHeatBalFanSys::MixingMassFlowZone.allocate(NumOfZones);
    DataHeatBalFanSys::MixingMassFlowXHumRat.allocate(NumOfZones);

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
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("MIXINGAVAILSCHED")).CurrentValue = 1.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("MININDOORTEMP")).CurrentValue = 18.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("MAXINDOORTEMP")).CurrentValue = 100.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("DELTATEMP")).CurrentValue = 2.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("MINOUTDOORTEMP")).CurrentValue = -100.0;
    ScheduleManager::Schedule(ScheduleManager::GetScheduleIndex("MAXOUTDOORTEMP")).CurrentValue = 100.0;
    DataEnvironment::OutBaroPress = 101325.0;

    InitSimpleMixingConvectiveHeatGains();

    CalcAirFlowSimple(2);

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(ErrorsFound);

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
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;
    
    DataHVACGlobals::NumPrimaryAirSys = 3;
    DataAirSystems::PrimaryAirSystem.allocate(3);
    DataAirLoop::AirLoopFlow.allocate(3);

    DataAirSystems::PrimaryAirSystem(1).OASysExists = false;
    DataAirLoop::AirLoopFlow(1).DesReturnFrac = 1.0;
    DataAirSystems::PrimaryAirSystem(2).OASysExists = false;
    DataAirLoop::AirLoopFlow(2).DesReturnFrac = 1.0;
    DataAirSystems::PrimaryAirSystem(3).OASysExists = false;
    DataAirLoop::AirLoopFlow(3).DesReturnFrac = 1.0;
    DataGlobals::DoingSizing = false;
    DataGlobals::isPulseZoneSizing = false;

    // Case 1 - send zero, expect zero back
    Node(inletNode1).MassFlowRate = 0.0;
    Node(inletNode2).MassFlowRate = 0.0;
    Node(inletNode3).MassFlowRate = 0.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    Real64 StdTotalReturnMassFlow = 0.0;
    Real64 FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
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

    CalcZoneReturnFlows(ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 6.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 1.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);

    // Deallocate everything - should all be taken care of in clear_states
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest3)
{

    std::string const idf_objects = delimited_string({
        " Version,9.2;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(ErrorsFound);
    int ZoneNum = 1;
    int NodeNum;
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
    }

    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirLoop::AirLoopFlow.allocate(1);

    DataAirSystems::PrimaryAirSystem(1).OASysExists = false;
    DataAirLoop::AirLoopFlow(1).DesReturnFrac = 1.0;
    DataGlobals::DoingSizing = false;
    DataGlobals::isPulseZoneSizing = false;

    ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(ZoneNum).ReturnNodeInletNum(1) = 1;
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 0.0;

    // Avoid zero values in volume flow balance check
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    // Set return node basis node flows to zero, return flow should be zero
    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumReturnFlowBasisNodes; ++NodeNum) {
        Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(NodeNum)).MassFlowRate = 0.0;
    }
    CalcZoneMassBalance(false);
    EXPECT_EQ(Node(ZoneEquipConfig(ZoneNum).ReturnNode(1)).MassFlowRate,0.0);

    // Set return node basis node flows to non-zero values, return flow should be the sum
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(1)).MassFlowRate = 0.05;
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(2)).MassFlowRate = 0.10;
    Node(ZoneEquipConfig(ZoneNum).ReturnFlowBasisNode(3)).MassFlowRate = 0.20;
    CalcZoneMassBalance(false);
    EXPECT_NEAR(Node(ZoneEquipConfig(ZoneNum).ReturnNode(1)).MassFlowRate, 0.35, 0.00001);
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest4)
{

    std::string const idf_objects = delimited_string({
        " Version,8.4;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
    ZoneEquipInputsFilled = true;
    GetSimpleAirModelInputs(ErrorsFound);

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
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 100000.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).Temp = 20.0;
    Node(ZoneEquipConfig(ZoneNum).ZoneNode).HumRat = 0.004;

    DataHVACGlobals::NumPrimaryAirSys = 3;
    DataAirSystems::PrimaryAirSystem.allocate(3);
    DataAirLoop::AirLoopFlow.allocate(3);

    // Add an outdoor air system to airloop 2
    DataAirSystems::PrimaryAirSystem(1).OASysExists = false;
    DataAirLoop::AirLoopFlow(1).DesReturnFrac = 1.0;
    DataAirSystems::PrimaryAirSystem(2).OASysExists = true;
    DataAirLoop::AirLoopFlow(2).DesReturnFrac = 0.9;
    DataAirLoop::AirLoopFlow(2).MaxOutAir = 0.1;
    DataAirLoop::AirLoopFlow(2).OAFlow = 0.1;
    DataAirSystems::PrimaryAirSystem(3).OASysExists = false;
    DataAirLoop::AirLoopFlow(3).DesReturnFrac = 1.0;
    DataGlobals::DoingSizing = false;
    DataGlobals::isPulseZoneSizing = false;

    // Case 1 - send zero, expect zero back
    Node(inletNode1).MassFlowRate = 0.0;
    Node(inletNode2).MassFlowRate = 0.0;
    Node(inletNode3).MassFlowRate = 0.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    Real64 StdTotalReturnMassFlow = 0.0;
    Real64 FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
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

    CalcZoneReturnFlows(ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 5.9);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 0.9);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);

    // Case 3 - add exhaust flow, but set system 2 MaxOutAir to zero, expect sum of inlet flow back 
    Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 1.000000001;
    DataAirSystems::PrimaryAirSystem(2).OASysExists = true;
    DataAirLoop::AirLoopFlow(2).DesReturnFrac = 0.9;
    DataAirLoop::AirLoopFlow(2).MaxOutAir = 0.0;
    DataAirLoop::AirLoopFlow(2).OAFlow = 0.0;

    Node(inletNode2).MassFlowRate = 2.0;
    Node(inletNode1).MassFlowRate = 1.0;
    Node(inletNode3).MassFlowRate = 3.0;
    Node(returnNode1).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
    Node(returnNode2).MassFlowRate = 0.32;
    Node(returnNode3).MassFlowRate = 0.45;

    StdTotalReturnMassFlow = 6.0;
    FinalTotalReturnMassFlow = 0.0;

    CalcZoneReturnFlows(ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
    EXPECT_EQ(FinalTotalReturnMassFlow, 6.0);
    EXPECT_EQ(Node(returnNode1).MassFlowRate, 2.0);
    EXPECT_EQ(Node(returnNode2).MassFlowRate, 1.0);
    EXPECT_EQ(Node(returnNode3).MassFlowRate, 3.0);
}

TEST_F(EnergyPlusFixture, ZoneEquipmentManager_DistributeSequentialLoad)
{

    std::string const idf_objects = delimited_string({
        " Version,9.2;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    ZoneEquipmentManager::PrioritySimOrder.allocate(3);

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
        " Version,9.2;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
        " Version,9.2;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 3;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal 2 for UniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 0);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal 2 for UniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);

    // UniformPLR Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
        " Version,9.2;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 3;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal equipmnum+1 for SequentialUniformPLR
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 2);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
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
        " Version,9.0;",

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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    ZoneEquipmentManager::PrioritySimOrder.allocate(NumEquip);

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(4) = 2;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 1);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 3);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 3);

    // Sequential Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration);
    SetZoneEquipSimOrder(ZoneNum, ZoneNum);
    DistributeSystemOutputRequired(ZoneNum, firstHVACIteration);
    // Equipment 1 provides 100W of heating
    Real64 SysOutputProvided = 100.0;
    Real64 LatOutputProvided = 0.0;
    int EquipNum = 1;
    UpdateSystemOutputRequired(ZoneNum, SysOutputProvided, LatOutputProvided, EquipNum);

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
         " Version,9.0;",

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
         " 0.3,                         !- Zone Equipment 1 Sequential Cooling Fraction",
         " 0.4,                         !- Zone Equipment 1 Sequential Heating or No-Load Fraction",
         " ZoneHVAC:IdealLoadsAirSystem,",
         " Ideal System A,           !- Name",
         " 2,                        !- Zone Equipment 2 Cooling Sequence",
         " 2,                        !- Zone Equipment 2 Heating or No-Load Sequence",
         " 0.5,                         !- Zone Equipment 2 Sequential Cooling Fraction",
         " 0.6,                         !- Zone Equipment 2 Sequential Heating or No-Load Fraction",
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
    GetZoneData(ErrorsFound);
    AllocateHeatBalArrays();
    GetZoneEquipmentData1();
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
    ZoneEquipmentManager::PrioritySimOrder.allocate(NumEquip);

    // Sequential Test 1 - Heating, FirstHVACIteration = true
    energy.TotalOutputRequired = 1000.0;
    energy.OutputRequiredToHeatingSP = 1000.0;
    energy.OutputRequiredToCoolingSP = 2000.0;
    bool firstHVACIteration = true;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
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
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(2) = 1;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(3) = 2;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(4) = 2;
    int zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 1", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    int coolingPriority = 0;
    int heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 1);
    EXPECT_EQ(heatingPriority, 1);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 1);
    zoneInlet = UtilityRoutines::FindItemInList("ZONE EQUIP INLET 3", DataLoopNode::NodeID, DataLoopNode::NumOfNodes);
    coolingPriority = 0;
    heatingPriority = 0;
    DataZoneEquipment::ZoneEquipList(1).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
    EXPECT_EQ(coolingPriority, 3);
    EXPECT_EQ(heatingPriority, 3);
    // DataHVACGlobals::MinAirLoopIterationsAfterFirst should equal the highest air terminal equipment num for sequential loading
    EXPECT_EQ(DataHVACGlobals::MinAirLoopIterationsAfterFirst, 3);

    // Sequential Test 2 - Heating, FirstHVACIteration = false
    firstHVACIteration = false;
    InitSystemOutputRequired(ZoneNum, firstHVACIteration, true);
    // Equipment 1 provides 100W of heating
    Real64 SysOutputProvided = 100.0;
    Real64 LatOutputProvided = 0.0;
    int EquipNum = 1;
    UpdateSystemOutputRequired(ZoneNum, SysOutputProvided, LatOutputProvided, EquipNum);

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
