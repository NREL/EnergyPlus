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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

using namespace EnergyPlus;
using namespace SimulationManager;
using namespace DataSizing;

TEST_F(EnergyPlusFixture, ParallelPIUTest1)
{
    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    SPACE2-1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 In Node,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 ATU Sec Node,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Air Node,           !- Zone Air Node Name",
        "    SPACE2-1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ADU,    !- Name",
        "    SPACE2-1 In Node,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ParallelPIU:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 Parallel PIU Reheat;           !- Air Terminal Name",
        " AirTerminal:SingleDuct:ParallelPIU:Reheat,",
        " SPACE2-1 Parallel PIU Reheat,     !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 0.1,                !- Maximum Primary Air Flow Rate {m3/s}",
        " 0.05,                !- Maximum Secondary Air Flow Rate {m3/s}",
        " 0.2,                !- Minimum Primary Air Flow Fraction",
        " 0.1,                !- Fan On Flow Fraction",
        " SPACE2-1 ATU In Node,    !- Supply Air Inlet Node Name",
        " SPACE2-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
        " SPACE2-1 In Node,        !- Outlet Node Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
        " SPACE2-1 PIU Mixer,      !- Zone Mixer Name",
        " SPACE2-1 PIU Fan,        !- Fan Name",
        " Coil:Heating:Electric,      !- Reheat Coil Object Type",
        " SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        " 0.0,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0001;                  !- Convergence Tolerance",
        "",
        " Fan:ConstantVolume,",
        " SPACE2-1 PIU Fan,        !- Name",
        " AlwaysOff,           !- Availability Schedule Name",
        " 0.5,                     !- Fan Total Efficiency",
        " 50.0,                    !- Pressure Rise {Pa}",
        " 0.05,                !- Maximum Flow Rate {m3/s}",
        " 0.9,                     !- Motor Efficiency",
        " 1.0,                     !- Motor In Airstream Fraction",
        " SPACE2-1 ATU Sec Node,   !- Air Inlet Node Name",
        " SPACE2-1 ATU Fan Outlet Node;  !- Air Outlet Node Name",
        "",
        " AirLoopHVAC:ZoneMixer,",
        " SPACE2-1 PIU Mixer,      !- Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Outlet Node Name",
        " SPACE2-1 ATU In Node,    !- Inlet 1 Node Name",
        " SPACE2-1 ATU Fan Outlet Node;  !- Inlet 2 Node Name",
        "",
        " Coil:Heating:Electric,",
        " SPACE2-1 Zone Coil,      !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 1.0,                     !- Efficiency",
        " 1000,                !- Nominal Capacity",
        " SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        " SPACE2-1 In Node;       !- Air Outlet Node Name",
        "",
        "Schedule:Constant,",
        "    AlwaysOff,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    0;                       !- Hourly Value",
        "Schedule:Constant,",
        "    AlwaysOn,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    1;                       !- Hourly Value",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    state->dataEnvrn->StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(*state, 101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues(*state);

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    Fans::GetFanInput(*state);
    state->dataFans->GetFanInputFlag = false;
    PoweredInductionUnits::GetPIUs(*state);
    EXPECT_TRUE(compare_err_stream(""));
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   SPACE2-1 Air Node
    //  2   SPACE2-1 Return Node
    //  3   SPACE2-1 In Node
    //  4   SPACE2-1 ATU Sec Node
    //  5   SPACE2-1 ATU Fan Outlet Node
    //  6   SPACE2-1 ATU In Node
    //  7   SPACE2-1 Zone Coil Air In Node

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int ZoneNum = 1;
    int SysNum = 1;
    int ZoneNodeNum = 1;
    int SecNodeNum = state->dataPowerInductionUnits->PIU(SysNum).SecAirInNode;
    int PriNodeNum = state->dataPowerInductionUnits->PIU(SysNum).PriAirInNode;
    bool FirstHVACIteration = true;
    Real64 SecMaxMassFlow = 0.05 * state->dataEnvrn->StdRhoAir; // From inputs

    state->dataGlobal->BeginEnvrnFlag = true; // Must be true for initial pass thru InitPIU for this terminal unit
    FirstHVACIteration = true;
    PoweredInductionUnits::InitPIU(*state, SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    Fans::InitFan(*state, 1, FirstHVACIteration);
    state->dataGlobal->BeginEnvrnFlag = false;
    FirstHVACIteration = false;

    // Note that the fan schedule is always off, so the PIU fan should only run if the night cycle turn on flag is true

    // First test - Heating load, TurnZoneFansOn is false, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Second test - Heating load, TurnZoneFansOn is true, no primary flow - expecting secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Third test - Cooling load TurnZoneFansOn is true, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Fourth test - Cooling load TurnFansOn is true, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Fifth test - Heating load TurnFansOn is true, no primary flow - expecting secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Sixth test - Heating load TurnFansOn is true, yes primary flow, deadbandorsetback is true - expecting secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = state->dataPowerInductionUnits->PIU(SysNum).MinPriAirMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = true;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.2, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Seventh test - Heating load TurnFansOn is true, yes primary flow - expecting secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = state->dataPowerInductionUnits->PIU(SysNum).MinPriAirMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.2, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Eighth test - Cooling load TurnFansOn is true, yes primary flow - expecting secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = state->dataPowerInductionUnits->PIU(SysNum).MinPriAirMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Cooling load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(1.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Cleanup
    state->dataHeatBalFanSys->TempControlType.deallocate();
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.deallocate();
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.deallocate();
}

TEST_F(EnergyPlusFixture, SeriesPIUTest1)
{
    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    SPACE2-1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 In Node,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 ATU Sec Node,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Air Node,           !- Zone Air Node Name",
        "    SPACE2-1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ADU,    !- Name",
        "    SPACE2-1 In Node,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:SeriesPIU:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 Series PIU Reheat;           !- Air Terminal Name",
        " AirTerminal:SingleDuct:SeriesPIU:Reheat,",
        " SPACE2-1 Series PIU Reheat,     !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 0.15,                !- Maximum Air Flow Rate {m3/s}",
        " 0.05,                !- Maximum Primary Air Flow Rate {m3/s}",
        " 0.2,                !- Minimum Primary Air Flow Fraction",
        " SPACE2-1 ATU In Node,    !- Supply Air Inlet Node Name",
        " SPACE2-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
        " SPACE2-1 In Node,        !- Outlet Node Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
        " SPACE2-1 PIU Mixer,      !- Zone Mixer Name",
        " SPACE2-1 PIU Fan,        !- Fan Name",
        " Coil:Heating:Electric,      !- Reheat Coil Object Type",
        " SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        " 0.0,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0001;                  !- Convergence Tolerance",
        "",
        " Fan:ConstantVolume,",
        " SPACE2-1 PIU Fan,        !- Name",
        " AlwaysOff,           !- Availability Schedule Name",
        " 0.5,                     !- Fan Total Efficiency",
        " 50.0,                    !- Pressure Rise {Pa}",
        " 0.05,                !- Maximum Flow Rate {m3/s}",
        " 0.9,                     !- Motor Efficiency",
        " 1.0,                     !- Motor In Airstream Fraction",
        " SPACE2-1 ATU Fan Inlet Node,   !- Air Inlet Node Name",
        " SPACE2-1 Zone Coil Air In Node;  !- Air Outlet Node Name",
        "",
        " AirLoopHVAC:ZoneMixer,",
        " SPACE2-1 PIU Mixer,      !- Name",
        " SPACE2-1 ATU Fan Inlet Node,  !- Outlet Node Name",
        " SPACE2-1 ATU In Node,    !- Inlet 1 Node Name",
        " SPACE2-1 ATU Sec Node;  !- Inlet 2 Node Name",
        "",
        " Coil:Heating:Electric,",
        " SPACE2-1 Zone Coil,      !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 1.0,                     !- Efficiency",
        " 1000,                !- Nominal Capacity",
        " SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        " SPACE2-1 In Node;       !- Air Outlet Node Name",
        "",
        "Schedule:Constant,",
        "    AlwaysOff,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    0;                       !- Hourly Value",
        "Schedule:Constant,",
        "    AlwaysOn,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    1;                       !- Hourly Value",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    state->dataEnvrn->StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(*state, 101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues(*state);

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    Fans::GetFanInput(*state);
    state->dataFans->GetFanInputFlag = false;
    PoweredInductionUnits::GetPIUs(*state);
    EXPECT_TRUE(compare_err_stream(""));
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   SPACE2-1 Air Node
    //  2   SPACE2-1 Return Node
    //  3   SPACE2-1 In Node
    //  4   SPACE2-1 ATU Sec Node
    //  5   SPACE2-1 ATU Fan Outlet Node
    //  6   SPACE2-1 ATU In Node
    //  7   SPACE2-1 Zone Coil Air In Node

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int ZoneNum = 1;
    int SysNum = 1;
    int ZoneNodeNum = 1;
    int SecNodeNum = state->dataPowerInductionUnits->PIU(SysNum).SecAirInNode;
    int PriNodeNum = state->dataPowerInductionUnits->PIU(SysNum).PriAirInNode;
    bool FirstHVACIteration = true;

    state->dataGlobal->BeginEnvrnFlag = true; // Must be true for initial pass thru InitPIU for this terminal unit
    FirstHVACIteration = true;
    PoweredInductionUnits::InitPIU(*state, SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    Fans::InitFan(*state, 1, FirstHVACIteration);
    state->dataGlobal->BeginEnvrnFlag = false;
    FirstHVACIteration = false;

    // From inputs
    Real64 SecMaxMassFlow = state->dataPowerInductionUnits->PIU(SysNum).MaxTotAirMassFlow;
    Real64 PriMaxMassFlow = state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow;
    Real64 PriMinMassFlow =
        state->dataPowerInductionUnits->PIU(SysNum).MaxPriAirMassFlow * state->dataPowerInductionUnits->PIU(SysNum).MinPriAirFlowFrac;
    Real64 SecMassFlowAtPrimMin = state->dataPowerInductionUnits->PIU(SysNum).MaxTotAirMassFlow - PriMinMassFlow;
    Real64 SecMassFlowAtPrimMax = state->dataPowerInductionUnits->PIU(SysNum).MaxTotAirMassFlow - PriMaxMassFlow;

    // Note that the fan schedule is always off, so the PIU fan should only run if the night cycle turn on flag is true

    // First test - Heating load, TurnZoneFansOn is false, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Second test - Heating load, TurnZoneFansOn is true, no primary flow - expecting max secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Third test - Cooling load TurnZoneFansOn is true, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = false;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Fourth test - Cooling load TurnFansOn is true, no primary flow - expecting no secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Fifth test - Heating load TurnFansOn is true, no primary flow - expecting max secondary flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Sixth test - Heating load TurnFansOn is true, yes min primary flow, deadbandorsetback is true - expecting secondary flow at primary min flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = PriMinMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = true;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMin, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(1.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Seventh test - Heating load TurnFansOn is true, yes min primary flow - expecting secondary flow at primary min flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = PriMinMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMin, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(1.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Eighth test - Cooling load TurnFansOn is true, yes primary flow at max - expecting secondary flow at primary max flow
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = PriMaxMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = PriMaxMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(*state, SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMax, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(1.0, state->dataPowerInductionUnits->PIU(SysNum).PriDamperPosition);

    // Cleanup
    state->dataHeatBalFanSys->TempControlType.deallocate();
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.deallocate();
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.deallocate();
}

// cf: https://github.com/NREL/EnergyPlus/issues/7183
TEST_F(EnergyPlusFixture, PIUArrayOutOfBounds)
{

    state->dataPowerInductionUnits->NumSeriesPIUs = 1;
    state->dataPowerInductionUnits->NumPIUs = 1;
    state->dataPowerInductionUnits->PIU.allocate(1);
    int PIUNum = 1;
    state->dataPowerInductionUnits->PIU(PIUNum).Name = "Series PIU";
    state->dataPowerInductionUnits->PIU(PIUNum).UnitType_Num = DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat;
    state->dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = PoweredInductionUnits::iHCoilType::Electric;

    // Go into all of the autosize blocks (aside from Heating/Steam coils)
    state->dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow = AutoSize;
    state->dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow = AutoSize;

    state->dataSize->CurSysNum = 0;
    state->dataSize->SysSizingRunDone = false;
    state->dataSize->ZoneSizingRunDone = true;

    // Test array out of bounds error. Notice that CurZoneEqNum is 2, while CurTermUnitSizingNum is 1
    // CurZoneEqNum = Current Zone Equipment index (0 if not simulating ZoneEq)
    // CurTermUnitSizingNum = Current terminal unit sizing index for TermUnitSizing and TermUnitFinalZoneSizing
    state->dataSize->CurZoneEqNum = 2;
    state->dataSize->FinalZoneSizing.allocate(2);
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesCoolVolFlow = 2.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatVolFlow = 1.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInTempTU = 10.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInHumRatTU = 0.006;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.008;

    state->dataSize->CurTermUnitSizingNum = 1;
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitFinalZoneSizing.allocate(1);
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).AirVolFlow = 1.0;
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).MinFlowFrac = 0.5;
    state->dataSize->TermUnitSingDuct = true;
    state->dataSize->TermUnitFinalZoneSizing(state->dataSize->CurTermUnitSizingNum) = state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum);

    // Call the sizing routine now
    PoweredInductionUnits::SizePIU(*state, PIUNum);

    EXPECT_TRUE(compare_err_stream(""));
}

TEST_F(EnergyPlusFixture, SeriesPIUZoneOAVolumeFlowRateTest)
{
    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    SPACE2-1;                !- Name",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 In Node,        !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 ATU Sec Node,   !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Air Node,       !- Zone Air Node Name",
        "    SPACE2-1 Return Node;    !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ADU,            !- Name",
        "    SPACE2-1 In Node,        !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:SeriesPIU:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 Series PIU Reheat;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:SeriesPIU:Reheat,",
        "    SPACE2-1 Series PIU Reheat,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    0.15,                    !- Maximum Air Flow Rate {m3/s}",
        "    0.05,                    !- Maximum Primary Air Flow Rate {m3/s}",
        "    0.2,                     !- Minimum Primary Air Flow Fraction",
        "    SPACE2-1 ATU In Node,    !- Supply Air Inlet Node Name",
        "    SPACE2-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
        "    SPACE2-1 In Node,        !- Outlet Node Name",
        "    SPACE2-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
        "    SPACE2-1 PIU Mixer,      !- Zone Mixer Name",
        "    SPACE2-1 PIU Fan,        !- Fan Name",
        "    Coil:Heating:Electric,      !- Reheat Coil Object Type",
        "    SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        "    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0001;                  !- Convergence Tolerance",

        "Fan:ConstantVolume,",
        "    SPACE2-1 PIU Fan,        !- Name",
        "    ,                        !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    50.0,                    !- Pressure Rise {Pa}",
        "    0.05,                    !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    SPACE2-1 ATU Fan Inlet Node,   !- Air Inlet Node Name",
        "    SPACE2-1 Zone Coil Air In Node;  !- Air Outlet Node Name",

        "AirLoopHVAC:ZoneMixer,",
        "    SPACE2-1 PIU Mixer,      !- Name",
        "    SPACE2-1 ATU Fan Inlet Node,  !- Outlet Node Name",
        "    SPACE2-1 ATU In Node,    !- Inlet 1 Node Name",
        "    SPACE2-1 ATU Sec Node;   !- Inlet 2 Node Name",

        "Coil:Heating:Electric,",
        "    SPACE2-1 Zone Coil,      !- Name",
        "    ,                        !- Availability Schedule Name",
        "    1.0,                     !- Efficiency",
        "    2000,                    !- Nominal Capacity",
        "    SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        "    SPACE2-1 In Node;        !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    state->dataEnvrn->StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(*state, 101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues(*state);

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    Fans::GetFanInput(*state);
    state->dataFans->GetFanInputFlag = false;
    PoweredInductionUnits::GetPIUs(*state);
    EXPECT_TRUE(compare_err_stream(""));
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback(1) = false;

    // Setup for Zone 1 series PIU Reheat air terminal
    int ZoneNum = 1;
    int PIUNum = 1;
    int ZoneNodeNum = 1;

    auto &thisSeriesAT = state->dataPowerInductionUnits->PIU(PIUNum);
    int SecNodeNum = thisSeriesAT.SecAirInNode;
    int PriNodeNum = thisSeriesAT.PriAirInNode;
    bool FirstHVACIteration = true;

    state->dataGlobal->BeginEnvrnFlag = true;
    FirstHVACIteration = true;
    PoweredInductionUnits::InitPIU(*state, PIUNum, FirstHVACIteration);
    Fans::InitFan(*state, 1, FirstHVACIteration);
    state->dataGlobal->BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    state->dataHVACGlobal->TurnFansOn = true;
    state->dataHVACGlobal->TurnZoneFansOnlyOn = false;

    // From inputs
    Real64 SecMaxMassFlow = thisSeriesAT.MaxTotAirMassFlow;
    Real64 PriMaxMassFlow = thisSeriesAT.MaxPriAirMassFlow;
    Real64 PriMinMassFlow = thisSeriesAT.MaxPriAirMassFlow * thisSeriesAT.MinPriAirFlowFrac;
    Real64 SecMassFlowAtPrimMin = thisSeriesAT.MaxTotAirMassFlow - PriMinMassFlow;
    Real64 SecMassFlowAtPrimMax = thisSeriesAT.MaxTotAirMassFlow - PriMaxMassFlow;

    // Needs an airloop, assume 20% outdoor air
    Real64 const AirLoopOAFraction = 0.20;
    thisSeriesAT.AirLoopNum = 1;
    state->dataAirLoop->AirLoopFlow.allocate(1);
    state->dataAirLoop->AirLoopFlow(thisSeriesAT.AirLoopNum).OAFrac = AirLoopOAFraction;

    state->dataZoneEquip->ZoneEquipConfig(thisSeriesAT.CtrlZoneNum).InletNodeAirLoopNum(thisSeriesAT.ctrlZoneInNodeIndex) = 1;
    // set heating zone and AT unit inlet conditions
    state->dataLoopNodes->Node(ZoneNodeNum).Temp = 20.0;
    state->dataLoopNodes->Node(ZoneNodeNum).HumRat = 0.005;
    state->dataLoopNodes->Node(ZoneNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(ZoneNodeNum).Temp, state->dataLoopNodes->Node(ZoneNodeNum).HumRat);
    state->dataLoopNodes->Node(SecNodeNum).Temp = state->dataLoopNodes->Node(ZoneNodeNum).Temp;
    state->dataLoopNodes->Node(SecNodeNum).HumRat = state->dataLoopNodes->Node(ZoneNodeNum).HumRat;
    state->dataLoopNodes->Node(SecNodeNum).Enthalpy = state->dataLoopNodes->Node(ZoneNodeNum).Enthalpy;
    state->dataLoopNodes->Node(PriNodeNum).Temp = 5.0;
    state->dataLoopNodes->Node(PriNodeNum).HumRat = 0.006;
    state->dataLoopNodes->Node(PriNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(PriNodeNum).Temp, state->dataLoopNodes->Node(PriNodeNum).HumRat);

    // test 1:  Heating load, at 0.0 primary air flow rate
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0;
    PoweredInductionUnits::CalcSeriesPIU(*state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    PoweredInductionUnits::ReportPIU(*state, PIUNum);
    Real64 expect_OutdoorAirFlowRate = (0.0 / state->dataEnvrn->StdRhoAir) * AirLoopOAFraction;
    EXPECT_EQ(SecMaxMassFlow, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(0.0, state->dataLoopNodes->Node(PriNodeNum).MassFlowRate);
    EXPECT_EQ(expect_OutdoorAirFlowRate, thisSeriesAT.OutdoorAirFlowRate);

    // test 2:  Heating load, at minimum primary flow rate
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = PriMinMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMinAvail = PriMinMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0;
    PoweredInductionUnits::CalcSeriesPIU(*state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    PoweredInductionUnits::ReportPIU(*state, PIUNum);
    expect_OutdoorAirFlowRate = (PriMinMassFlow / state->dataEnvrn->StdRhoAir) * AirLoopOAFraction;
    EXPECT_EQ(SecMassFlowAtPrimMin, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(PriMinMassFlow, state->dataLoopNodes->Node(PriNodeNum).MassFlowRate);
    EXPECT_EQ(expect_OutdoorAirFlowRate, thisSeriesAT.OutdoorAirFlowRate);

    // test 3: - Cooling load, at maximum primary air flow rate
    // set cooling zone and AT unit inlet conditions
    state->dataLoopNodes->Node(ZoneNodeNum).Temp = 24.0;
    state->dataLoopNodes->Node(ZoneNodeNum).HumRat = 0.0080;
    state->dataLoopNodes->Node(ZoneNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(ZoneNodeNum).Temp, state->dataLoopNodes->Node(ZoneNodeNum).HumRat);
    state->dataLoopNodes->Node(SecNodeNum).Temp = state->dataLoopNodes->Node(ZoneNodeNum).Temp;
    state->dataLoopNodes->Node(SecNodeNum).HumRat = state->dataLoopNodes->Node(ZoneNodeNum).HumRat;
    state->dataLoopNodes->Node(SecNodeNum).Enthalpy = state->dataLoopNodes->Node(ZoneNodeNum).Enthalpy;
    state->dataLoopNodes->Node(PriNodeNum).Temp = 15.0;
    state->dataLoopNodes->Node(PriNodeNum).HumRat = 0.0075;
    state->dataLoopNodes->Node(PriNodeNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(PriNodeNum).Temp, state->dataLoopNodes->Node(PriNodeNum).HumRat);

    state->dataLoopNodes->Node(PriNodeNum).MassFlowRate = PriMaxMassFlow;
    state->dataLoopNodes->Node(PriNodeNum).MassFlowRateMaxAvail = PriMaxMassFlow;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputRequired = -3000.0;
    PoweredInductionUnits::CalcSeriesPIU(*state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    PoweredInductionUnits::ReportPIU(*state, PIUNum);
    expect_OutdoorAirFlowRate = (PriMaxMassFlow / state->dataEnvrn->StdRhoAir) * AirLoopOAFraction;
    EXPECT_EQ(SecMassFlowAtPrimMax, state->dataLoopNodes->Node(SecNodeNum).MassFlowRate);
    EXPECT_EQ(PriMaxMassFlow, state->dataLoopNodes->Node(PriNodeNum).MassFlowRate);
    EXPECT_EQ(expect_OutdoorAirFlowRate, thisSeriesAT.OutdoorAirFlowRate);
}
