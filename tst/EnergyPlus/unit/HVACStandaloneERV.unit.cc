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

// Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace HVACStandAloneERV;
using namespace ObjexxFCL;
using namespace DataHeatBalance;
using namespace DataHVACGlobals;
using namespace DataZoneEquipment;
using namespace DataSizing;
using namespace Fans;
using namespace ScheduleManager;

TEST_F(EnergyPlusFixture, HVACStandAloneERV_Test1)
{
    std::string const idf_objects = delimited_string({
        "  Fan:OnOff,",
        "    ERV Supply Fan,          !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    75.0,                    !- Pressure Rise {Pa}",
        "    20000.0,                 !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    HR Supply Outlet Node,   !- Air Inlet Node Name",
        "    Supply Fan Outlet Node;  !- Air Outlet Node Name",

        "  Fan:OnOff,",
        "    ERV Exhaust Fan,         !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    75.0,                    !- Pressure Rise {Pa}",
        "    20000.0,                 !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    HR Secondary Outlet Node,!- Air Inlet Node Name",
        "    Exhaust Fan Outlet Node; !- Air Outlet Node Name",

        "  Schedule:Compact,",
        "    FanAndCoilAvailSched,    !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    state->dataEnvrn->StdRhoAir = 1.0;
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone 1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = state->dataZoneEquip->ZoneEquipConfig(1).ZoneName;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);

    state->dataHeatBal->TotPeople = 2; // Total number of people statements
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 100.0;
    state->dataHeatBal->People(1).NumberOfPeoplePtr =
        DataGlobalConstants::ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value
    state->dataHeatBal->People(2).ZonePtr = 1;
    state->dataHeatBal->People(2).NumberOfPeople = 200.0;
    state->dataHeatBal->People(2).NumberOfPeoplePtr =
        DataGlobalConstants::ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value

    state->dataHVACStandAloneERV->StandAloneERV.allocate(1);

    // size on floor area
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerFloorArea = 1.0;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerOccupant = 0.0;
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirFanType_Num = DataHVACGlobals::FanType_SimpleOnOff;
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirFanName = "ERV SUPPLY FAN";
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirFanIndex = 1;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirFanType_Num = DataHVACGlobals::FanType_SimpleOnOff;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirFanName = "ERV EXHAUST FAN";
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirFanIndex = 2;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).FloorArea = 1000.0;
    SizeStandAloneERV(*state, 1);
    EXPECT_EQ(1000.0, state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow);

    // size on occupancy
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow = AutoSize; // Need to reset this for each pass
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerFloorArea = 0.0;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerOccupant = 10.0;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).FloorArea = 1000.0;
    SizeStandAloneERV(*state, 1);
    EXPECT_EQ(3000.0, state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow);

    // size on floor area and occupancy
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerFloorArea = 1.0;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerOccupant = 10.0;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).FloorArea = 1000.0;
    SizeStandAloneERV(*state, 1);
    EXPECT_EQ(4000.0, state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow);

    // size on floor area and occupancy using zone multiplier
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow = AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirVolFlow = AutoSize;
    state->dataHeatBal->Zone(1).Multiplier = 5.0;
    SizeStandAloneERV(*state, 1);
    EXPECT_EQ(20000.0, state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow);
}

TEST_F(EnergyPlusFixture, HVACStandAloneERV_Test2)
{

    std::string const idf_objects = delimited_string({
        "  Fan:OnOff,",
        "    ERV Supply Fan,          !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    75.0,                    !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    HR Supply Outlet Node,   !- Air Inlet Node Name",
        "    Supply Fan Outlet Node;  !- Air Outlet Node Name",

        "  Fan:OnOff,",
        "    ERV Exhaust Fan,         !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    75.0,                    !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    HR Secondary Outlet Node,!- Air Inlet Node Name",
        "    Exhaust Fan Outlet Node; !- Air Outlet Node Name",

        "  Schedule:Compact,",
        "    FanAndCoilAvailSched,    !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    state->dataEnvrn->StdRhoAir = 1.0;

    state->dataGlobal->NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(*state);               // read schedules

    GetFanInput(*state);

    state->dataSize->CurZoneEqNum = 1;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone 1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = state->dataZoneEquip->ZoneEquipConfig(1).ZoneName;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).FloorArea = 100.0;

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(25);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesCoolVolFlow = 0.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatVolFlow = 0.0;

    state->dataHeatBal->TotPeople = 2; // Total number of people objects
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 10.0;
    state->dataHeatBal->People(1).NumberOfPeoplePtr = DataGlobalConstants::ScheduleAlwaysOn; // always returns a 1 for schedule value
    state->dataHeatBal->People(2).ZonePtr = 1;
    state->dataHeatBal->People(2).NumberOfPeople = 20.0;
    state->dataHeatBal->People(2).NumberOfPeoplePtr = DataGlobalConstants::ScheduleAlwaysOn; // always returns a 1 for schedule value

    state->dataHVACStandAloneERV->StandAloneERV.allocate(1);
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow = DataSizing::AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirVolFlow = DataSizing::AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).DesignSAFanVolFlowRate = DataSizing::AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).DesignEAFanVolFlowRate = DataSizing::AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).DesignHXVolFlowRate = DataSizing::AutoSize;
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirFanName = state->dataFans->Fan(1).FanName;
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirFanIndex = 1;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirFanName = state->dataFans->Fan(2).FanName;
    state->dataHVACStandAloneERV->StandAloneERV(1).ExhaustAirFanIndex = 2;
    state->dataHVACStandAloneERV->StandAloneERV(1).HeatExchangerTypeNum = HX_AIRTOAIR_GENERIC;
    state->dataHVACStandAloneERV->StandAloneERV(1).HeatExchangerName = "ERV Heat Exchanger";
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerFloorArea = 0.01;
    state->dataHVACStandAloneERV->StandAloneERV(1).AirVolFlowPerOccupant = 0.0;
    state->dataHVACStandAloneERV->StandAloneERV(1).HighRHOAFlowRatio = 1.2;

    SizeStandAloneERV(*state, 1);

    EXPECT_EQ(1.0, state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirVolFlow);
    EXPECT_EQ(1.2, state->dataHVACStandAloneERV->StandAloneERV(1).DesignSAFanVolFlowRate);
    EXPECT_EQ(1.2, state->dataHVACStandAloneERV->StandAloneERV(1).DesignEAFanVolFlowRate);
}
