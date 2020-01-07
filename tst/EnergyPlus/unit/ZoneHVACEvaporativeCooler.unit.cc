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

// EnergyPlus::EvaporativeCoolers / ZoneHVAC:EvaporativeCoolerUnit Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EvaporativeCoolers;

class ZoneHVACEvapCoolerUnitTest : public EnergyPlusFixture
{
public:
    int UnitNum = 1;
    int EvapCoolNum = 1;
    int NumOfNodes = 10;
    bool ErrorsFound = false;
    bool FirstHVACIteration = true;

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        DataSizing::ZoneEqSizing.allocate(1);
        DataEnvironment::OutBaroPress = 101325.0;
        DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);

        DataEnvironment::OutDryBulbTemp = 20.0;
        DataEnvironment::OutHumRat = 0.0075;
        DataEnvironment::OutWetBulbTemp =
            Psychrometrics::PsyTwbFnTdbWPb(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat, DataEnvironment::OutBaroPress);

        DataGlobals::NumOfZones = 1;
        DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipConfig.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipList.allocate(DataGlobals::NumOfZones);
        DataLoopNode::Node.allocate(NumOfNodes);
        DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
        DataHeatBalFanSys::ZoneThermostatSetPointHi.allocate(1);

        DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "One Zone";
        DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
        DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 3;
        DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 4;
        DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 9;
        DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 10;
        DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;

        DataHeatBalance::Zone(1).Name = DataZoneEquipment::ZoneEquipConfig(1).ZoneName;
        DataHeatBalance::Zone(1).ZoneEqNum = 1;
        DataHeatBalance::Zone(1).Multiplier = 1.0;
        DataHeatBalance::Zone(1).Volume = 1000.0;
        DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum).SystemZoneNodeNumber =
            DataZoneEquipment::ZoneEquipConfig(1).ZoneNode;
        DataHeatBalance::Zone(1).ZoneVolCapMultpMoist = 1.0;

        DataZoneEquipment::ZoneEquipList(1).Name = "ZONEHVACEVAPEQUIPMENT";
        DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipType.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipName.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipData.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType(1) = "ZoneHVAC:EvaporativeCoolerUnit";
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num(1) = DataZoneEquipment::ZoneEvaporativeCoolerUnit_Num;
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown();
    }
};

TEST_F(ZoneHVACEvapCoolerUnitTest, DirectCelDekPad_CyclingUnit_Sim)
{
    int ActualZoneNum = 1;
    int ZoneEquipIndex = 1;
    Real64 SensOutputProvided(0.0);
    Real64 LatOutputProvided(0.0);

    std::string const idf_objects = delimited_string({

        " ZoneHVAC:EvaporativeCoolerUnit,",
        "   ZoneEvapCooler Unit,          !- Name",
        "   ,                             !- Availability Schedule Name",
        "   ,                             !- Availability Manager List Name",
        "   ZoneEvapCool OA Inlet,        !- Outdoor Air Inlet Node Name",
        "   ZoneEvapCool Inlet Node,      !- Cooler Outlet Node Name",
        "   ZoneEvapCool Relief Node,     !- Zone Relief Air Node Name",
        "   Fan:OnOff,                    !- Supply Air Fan Object Type",
        "   ZoneEvapCool Supply Fan,      !- Supply Air Fan Name",
        "   1.0,                          !- Design Supply Air Flow Rate {m3/s}",
        "   BlowThrough,                  !- Fan Placement",
        "   ZoneTemperatureDeadbandOnOffCycling,  !- Cooler Unit Control Method",
        "   1.0,                          !- Throttling Range Temperature Difference {deltaC}",
        "   100.0,                        !- Cooling Load Control Threshold Heat Transfer Rate {W}",
        "   EvaporativeCooler:Direct:CelDekPad,  !- First Evaporative Cooler Object Type",
        "   Direct CelDekPad EvapCooler;  !- First Evaporative Cooler Object Name",

        " Fan:OnOff,",
        "    ZoneEvapCool Supply Fan,     !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.7,                         !- Fan Total Efficiency",
        "    300.0,                       !- Pressure Rise {Pa}",
        "    1.0,                         !- Maximum Flow Rate {m3/s}",
        "    0.9,                         !- Motor Efficiency",
        "    1.0,                         !- Motor In Airstream Fraction",
        "    ZoneEvapCool OA Inlet,       !- Air Inlet Node Name",
        "    ZoneEvapCool Fan outlet;     !- Air Outlet Node Name",

        " EvaporativeCooler:Direct:CelDekPad,",
        "    Direct CelDekPad EvapCooler, !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.6,                         !- Direct Pad Area {m2}",
        "    0.17,                        !- Direct Pad Depth {m}",
        "    55,                          !- Recirculating Water Pump Power Consumption {W}",
        "    ZoneEvapCool Fan outlet,     !- Air Inlet Node Name",
        "    ZoneEvapCool Inlet Node,     !- Air Outlet Node Name",
        "    ;                            !- Control Type",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    Fans::GetFanInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetEvapInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetInputZoneEvaporativeCoolerUnit();
    ASSERT_FALSE(ErrorsFound);

    DataGlobals::BeginEnvrnFlag = true;
    DataZoneEquipment::ZoneEquipInputsFilled = true;

    auto &thisZoneEvapCooler(ZoneEvapUnit(UnitNum));

    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = thisZoneEvapCooler.UnitReliefNodeNum;

    DataLoopNode::Node.redimension(NumOfNodes);
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp = 24.0;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat = 0.0080;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp,
                                   DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat);

    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);

    DataHeatBalFanSys::ZoneThermostatSetPointHi(1) = 23.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    DataZoneEquipment::ZoneEquipList(1).EquipName(1) = thisZoneEvapCooler.Name;

    // Evap Cooler Unit Control Method = Zone Temperature Dead Band OnOff Cycling
    EXPECT_EQ(thisZoneEvapCooler.OpMode, DataHVACGlobals::CycFanCycCoil);
    EXPECT_EQ(thisZoneEvapCooler.ControlSchemeType, ZoneTemperatureDeadBandOnOffCycling);
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    Real64 FullSensibleOutput = 0.0;
    Real64 FullLatentOutput = 0.0;
    Real64 PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    Real64 HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.4746755, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);

    // Evap Cooler Unit Control Method = Zone Cooling Load OnOff Cycling
    thisZoneEvapCooler.ControlSchemeType = ZoneCoolingLoadOnOffCycling;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.4746755, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);
}

TEST_F(ZoneHVACEvapCoolerUnitTest, DirectResearchSpecial_CyclingUnit_Sim)
{
    int ActualZoneNum = 1;
    int ZoneEquipIndex = 1;
    Real64 SensOutputProvided(0.0);
    Real64 LatOutputProvided(0.0);

    std::string const idf_objects = delimited_string({

        " ZoneHVAC:EvaporativeCoolerUnit,",
        "   ZoneEvapCooler Unit,          !- Name",
        "   ,                             !- Availability Schedule Name",
        "   ,                             !- Availability Manager List Name",
        "   ZoneEvapCool OA Inlet,        !- Outdoor Air Inlet Node Name",
        "   ZoneEvapCool Inlet Node,      !- Cooler Outlet Node Name",
        "   ZoneEvapCool Relief Node,     !- Zone Relief Air Node Name",
        "   Fan:OnOff,                    !- Supply Air Fan Object Type",
        "   ZoneEvapCool Supply Fan,      !- Supply Air Fan Name",
        "   1.0,                          !- Design Supply Air Flow Rate {m3/s}",
        "   BlowThrough,                  !- Fan Placement",
        "   ZoneTemperatureDeadbandOnOffCycling,  !- Cooler Unit Control Method",
        "   1.0,                          !- Throttling Range Temperature Difference {deltaC}",
        "   100.0,                        !- Cooling Load Control Threshold Heat Transfer Rate {W}",
        "   EvaporativeCooler:Direct:ResearchSpecial,  !- First Evaporative Cooler Object Type",
        "   Direct ResearchSpecial EvapCooler;  !- First Evaporative Cooler Object Name",

        " Fan:OnOff,",
        "    ZoneEvapCool Supply Fan,     !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.7,                         !- Fan Total Efficiency",
        "    300.0,                       !- Pressure Rise {Pa}",
        "    1.0,                         !- Maximum Flow Rate {m3/s}",
        "    0.9,                         !- Motor Efficiency",
        "    1.0,                         !- Motor In Airstream Fraction",
        "    ZoneEvapCool OA Inlet,       !- Air Inlet Node Name",
        "    ZoneEvapCool Fan outlet;     !- Air Outlet Node Name",

        " EvaporativeCooler:Direct:ResearchSpecial,",
        "   Direct ResearchSpecial EvapCooler,  !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.7,                         !- Cooler Design Effectiveness",
        "    ,                            !- Effectiveness Flow Ratio Modifier Curve Name",
        "    1.0,                         !- Primary Air Design Flow Rate {m3/s}",
        "    55,                          !- Recirculating Water Pump Power Consumption {W}",
        "    ,                            !- Water Pump Power Sizing Factor {W/(m3/s)}",
        "    ,                            !- Water Pump Power Modifier Curve Name",
        "    ZoneEvapCool Fan outlet,     !- Air Inlet Node Name",
        "    ZoneEvapCool Inlet Node,     !- Air Outlet Node Name",
        "    ZoneEvapCool Inlet Node;     !- Sensor Node Name",
    });
    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    Fans::GetFanInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetEvapInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetInputZoneEvaporativeCoolerUnit();
    ASSERT_FALSE(ErrorsFound);

    DataGlobals::BeginEnvrnFlag = true;
    DataZoneEquipment::ZoneEquipInputsFilled = true;

    auto &thisZoneEvapCooler(ZoneEvapUnit(UnitNum));

    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = thisZoneEvapCooler.UnitReliefNodeNum;

    DataLoopNode::Node.redimension(NumOfNodes);
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp = 24.0;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat = 0.0080;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp,
                                   DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat);

    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);

    DataHeatBalFanSys::ZoneThermostatSetPointHi(1) = 23.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    DataZoneEquipment::ZoneEquipList(1).EquipName(1) = thisZoneEvapCooler.Name;

    // Evap Cooler Unit Control Method = Zone Temperature Dead Band OnOff Cycling
    EXPECT_EQ(thisZoneEvapCooler.OpMode, DataHVACGlobals::CycFanCycCoil);
    EXPECT_EQ(thisZoneEvapCooler.ControlSchemeType, ZoneTemperatureDeadBandOnOffCycling);
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    Real64 FullSensibleOutput = 0.0;
    Real64 FullLatentOutput = 0.0;
    Real64 PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    Real64 HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.500000, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);

    // Evap Cooler Unit Control Method = Zone Cooling Load OnOff Cycling
    thisZoneEvapCooler.ControlSchemeType = ZoneCoolingLoadOnOffCycling;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.500000, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);
}

TEST_F(ZoneHVACEvapCoolerUnitTest, IndirectWetCoil_CyclingUnit_Sim)
{
    int ActualZoneNum = 1;
    int ZoneEquipIndex = 1;
    Real64 SensOutputProvided(0.0);
    Real64 LatOutputProvided(0.0);

    std::string const idf_objects = delimited_string({

        " ZoneHVAC:EvaporativeCoolerUnit,",
        "   ZoneEvapCooler Unit,          !- Name",
        "   ,                             !- Availability Schedule Name",
        "   ,                             !- Availability Manager List Name",
        "   ZoneEvapCool OA Inlet,        !- Outdoor Air Inlet Node Name",
        "   ZoneEvapCool Inlet Node,      !- Cooler Outlet Node Name",
        "   ZoneEvapCool Relief Node,     !- Zone Relief Air Node Name",
        "   Fan:OnOff,                    !- Supply Air Fan Object Type",
        "   ZoneEvapCool Supply Fan,      !- Supply Air Fan Name",
        "   1.0,                          !- Design Supply Air Flow Rate {m3/s}",
        "   BlowThrough,                  !- Fan Placement",
        "   ZoneTemperatureDeadbandOnOffCycling,  !- Cooler Unit Control Method",
        "   1.0,                          !- Throttling Range Temperature Difference {deltaC}",
        "   100.0,                        !- Cooling Load Control Threshold Heat Transfer Rate {W}",
        "   EvaporativeCooler:Indirect:WetCoil,  !- First Evaporative Cooler Object Type",
        "   Indirect WetCoil EvapCooler;  !- First Evaporative Cooler Object Name",

        " Fan:OnOff,",
        "    ZoneEvapCool Supply Fan,     !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.7,                         !- Fan Total Efficiency",
        "    300.0,                       !- Pressure Rise {Pa}",
        "    1.0,                         !- Maximum Flow Rate {m3/s}",
        "    0.9,                         !- Motor Efficiency",
        "    1.0,                         !- Motor In Airstream Fraction",
        "    ZoneEvapCool OA Inlet,       !- Air Inlet Node Name",
        "    ZoneEvapCool Fan outlet;     !- Air Outlet Node Name",

        " EvaporativeCooler:Indirect:WetCoil,",
        "   Indirect WetCoil EvapCooler,  !- Name",
        "    ,                            !- Availability Schedule Name",
        "    0.7,                         !- Coil Maximum Efficiency",
        "    ,                            !- Coil Flow Ratio",
        "    55,                          !- Recirculating Water Pump Power Consumption {W}",
        "    1.0,                         !- Secondary Air Fan Flow Rate {m3/s}",
        "    0.7,                         !- Secondary Air Fan Total Efficiency",
        "    300,                         !- Secondary Air Fan Delta Pressure {Pa}",
        "    ZoneEvapCool Fan outlet,     !- Primary Air Inlet Node Name",
        "    ZoneEvapCool Inlet Node,     !- Primary Air Outlet Node Name",
        "    ,                            !- Control Type",
        "    ,                            !- Water Supply Storage Tank Name",
        "    Secondary OA inlet node;     !- Secondary Air Inlet Node Name",

        "    OutdoorAir:Node,",
        "    Secondary OA inlet node;     !- Name",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    Fans::GetFanInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetEvapInput();
    ASSERT_FALSE(ErrorsFound);
    EvaporativeCoolers::GetInputZoneEvaporativeCoolerUnit();
    ASSERT_FALSE(ErrorsFound);

    OutAirNodeManager::SetOutAirNodes();

    DataGlobals::BeginEnvrnFlag = true;
    DataZoneEquipment::ZoneEquipInputsFilled = true;

    auto &thisZoneEvapCooler(ZoneEvapUnit(UnitNum));
    auto &thisEvapCooler(EvapCond(EvapCoolNum));

    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = thisZoneEvapCooler.UnitReliefNodeNum;

    DataLoopNode::Node.redimension(NumOfNodes);
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp = 24.0;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat = 0.0080;
    DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).Temp,
                                   DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(1).ZoneNode).HumRat);

    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(thisZoneEvapCooler.OAInletNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);

    DataLoopNode::Node(thisEvapCooler.SecondaryInletNode).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(thisEvapCooler.SecondaryInletNode).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(thisEvapCooler.SecondaryInletNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);

    DataHeatBalFanSys::ZoneThermostatSetPointHi(1) = 23.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    DataZoneEquipment::ZoneEquipList(1).EquipName(1) = thisZoneEvapCooler.Name;

    // Evap Cooler Unit Control Method = Zone Temperature Dead Band OnOff Cycling
    EXPECT_EQ(thisZoneEvapCooler.OpMode, DataHVACGlobals::CycFanCycCoil);
    EXPECT_EQ(thisZoneEvapCooler.ControlSchemeType, ZoneTemperatureDeadBandOnOffCycling);
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    Real64 FullSensibleOutput = 0.0;
    Real64 FullLatentOutput = 0.0;
    Real64 PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    Real64 HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.500000, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);

    // Evap Cooler Unit Control Method = Zone Cooling Load OnOff Cycling
    thisZoneEvapCooler.ControlSchemeType = ZoneCoolingLoadOnOffCycling;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -15000.0;
    EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(thisZoneEvapCooler.Name, ActualZoneNum, SensOutputProvided, LatOutputProvided, ZoneEquipIndex);
    PartLoadRatio = 1.0;
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, PartLoadRatio, FullSensibleOutput, FullLatentOutput);
    EXPECT_NEAR(FullSensibleOutput, SensOutputProvided, 0.01);
    EXPECT_NEAR(FullSensibleOutput, -thisZoneEvapCooler.UnitSensibleCoolingRate, 0.01);
    // test the unit at half of the full flow capacity
    HalfOfFullLoad = 0.50 * FullSensibleOutput;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = HalfOfFullLoad;
    // calculate part load ratio for cycling fan
    EvaporativeCoolers::ControlZoneEvapUnitOutput(UnitNum, HalfOfFullLoad);
    EXPECT_NEAR(0.500000, thisZoneEvapCooler.UnitPartLoadRatio, 0.000001);
    EvaporativeCoolers::CalcZoneEvapUnitOutput(UnitNum, thisZoneEvapCooler.UnitPartLoadRatio, SensOutputProvided, LatOutputProvided);
    EXPECT_NEAR(HalfOfFullLoad, SensOutputProvided, 0.01);
}
