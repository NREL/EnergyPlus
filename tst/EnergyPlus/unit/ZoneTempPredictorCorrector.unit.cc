// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataStringGlobals;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataRoomAirModel;
using namespace EnergyPlus::HybridModel;
using namespace SimulationManager;

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_CorrectZoneHumRatTest)
{

    state->dataHVACGlobal->TimeStepSys = 15.0 / 60.0; // System timestep in hours

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone 1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;

    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataLoopNodes->Node.allocate(5);

    state->dataHeatBal->Zone.allocate(1);
    state->dataHybridModel->HybridModelZone.allocate(1);
    state->dataHeatBal->Zone(1).Name = state->dataZoneEquip->ZoneEquipConfig(1).ZoneName;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->CurZoneEqNum = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).Volume = 1000.0;
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataHeatBalFanSys->ZoneLatentGain(1) = 0.0;
    state->dataHeatBalFanSys->SumLatentHTRadSys.allocate(1);
    state->dataHeatBalFanSys->SumLatentHTRadSys(1) = 0.0;
    state->dataHeatBalFanSys->SumLatentPool.allocate(1);
    state->dataHeatBalFanSys->SumLatentPool(1) = 0.0;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->ZT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->ZT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).HTSurfaceLast = 2;
    state->dataSurface->Surface.allocate(2);

    state->dataZonePlenum->NumZoneReturnPlenums = 0;
    state->dataZonePlenum->NumZoneSupplyPlenums = 0;

    state->dataHeatBalFanSys->OAMFL.allocate(1);
    state->dataHeatBalFanSys->VAMFL.allocate(1);
    state->dataHeatBalFanSys->EAMFL.allocate(1);
    state->dataHeatBalFanSys->EAMFLxHumRat.allocate(1);
    state->dataHeatBalFanSys->CTMFL.allocate(1);

    state->dataHeatBalFanSys->SumHmARaW.allocate(1);
    state->dataHeatBalFanSys->SumHmARa.allocate(1);
    state->dataHeatBalFanSys->MixingMassFlowXHumRat.allocate(1);
    state->dataHeatBalFanSys->MixingMassFlowZone.allocate(1);
    state->afn->SimulateAirflowNetwork = 0;
    state->dataHeatBalFanSys->MDotOA.allocate(1);

    state->dataHeatBal->ZoneAirSolutionAlgo = DataHeatBalance::SolutionAlgo::EulerMethod;
    state->dataHeatBalFanSys->ZoneAirHumRatTemp.allocate(1);
    state->dataHeatBalFanSys->ZoneW1.allocate(1);

    state->dataRoomAirMod->AirModel.allocate(1);
    state->dataHeatBal->ZoneIntGain.allocate(1);

    // Case 1 - All flows at the same humrat
    state->dataHeatBalFanSys->ZoneW1(1) = 0.008;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    state->dataLoopNodes->Node(1).HumRat = 0.008;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    state->dataLoopNodes->Node(2).HumRat = 0.008;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExhBalanced = 0.0;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.00; // Zone exhaust node 1
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExh = state->dataLoopNodes->Node(3).MassFlowRate;
    state->dataLoopNodes->Node(3).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(4).MassFlowRate = 0.03; // Zone return node
    state->dataLoopNodes->Node(4).HumRat = 0.000;
    state->dataLoopNodes->Node(5).HumRat = 0.000;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataEnvrn->OutHumRat = 0.008;
    state->dataHeatBalFanSys->MixingMassFlowXHumRat(1) = 0.0;
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.0;
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;

    // HybridModel
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);

    // Case 2 - Unbalanced exhaust flow
    state->dataHeatBalFanSys->ZoneW1(1) = 0.008;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    state->dataLoopNodes->Node(1).HumRat = 0.008;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    state->dataLoopNodes->Node(2).HumRat = 0.008;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExhBalanced = 0.0;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExh = state->dataLoopNodes->Node(3).MassFlowRate;
    state->dataLoopNodes->Node(3).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(4).MassFlowRate = 0.01; // Zone return node
    state->dataLoopNodes->Node(4).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(5).HumRat = 0.000;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataEnvrn->OutHumRat = 0.004;
    state->dataHeatBalFanSys->MixingMassFlowXHumRat(1) = 0.0;
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.0;
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);

    // Case 3 - Balanced exhaust flow with proper source flow from mixing
    state->dataHeatBalFanSys->ZoneW1(1) = 0.008;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    state->dataLoopNodes->Node(1).HumRat = 0.008;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    state->dataLoopNodes->Node(2).HumRat = 0.008;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExhBalanced = 0.02;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExh = state->dataLoopNodes->Node(3).MassFlowRate;
    state->dataLoopNodes->Node(3).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(4).MassFlowRate = 0.03; // Zone return node
    state->dataLoopNodes->Node(4).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(5).HumRat = 0.000;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataEnvrn->OutHumRat = 0.004;
    state->dataHeatBalFanSys->MixingMassFlowXHumRat(1) = 0.02 * 0.008;
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.02;
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);

    // Case 4 - Balanced exhaust flow without source flow from mixing
    state->dataHeatBalFanSys->ZoneW1(1) = 0.008;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    state->dataLoopNodes->Node(1).HumRat = 0.008;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    state->dataLoopNodes->Node(2).HumRat = 0.008;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExhBalanced = 0.02;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneExh = state->dataLoopNodes->Node(3).MassFlowRate;
    state->dataLoopNodes->Node(3).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(4).MassFlowRate = 0.01; // Zone return node
    state->dataLoopNodes->Node(4).HumRat = state->dataHeatBalFanSys->ZoneW1(1);
    state->dataLoopNodes->Node(5).HumRat = 0.000;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataEnvrn->OutHumRat = 0.004;
    state->dataHeatBalFanSys->MixingMassFlowXHumRat(1) = 0.0;
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.0;
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);

    // Add a section to check #6119 by L. Gu on 5/16/17
    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);

    // Issue 6233
    state->dataHeatBal->Zone(1).IsControlled = true;
    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.008, state->dataLoopNodes->Node(5).HumRat, 0.00001);
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_ReportingTest)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Aug 2015

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Core_top,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_top Thermostat,     !- Name",
        "  Core_top,                !- Zone or ZoneList Name",
        "  Single Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeating,  !- Control 1 Object Type",
        "  Core_top HeatSPSched;    !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Heating Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleHeating,",
        "  Core_top HeatSPSched,    !- Name",
        "  SNGL_HTGSETP_SCH;        !- Heating Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  SNGL_HTGSETP_SCH,        !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,15.0;       !- Field 3",
        " ",
        "Zone,",
        "  Core_middle,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_middle Thermostat,  !- Name",
        "  Core_middle,             !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,  !- Control 1 Object Type",
        "  Core_middle CoolSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,2;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleCooling,",
        "  Core_middle CoolSPSched, !- Name",
        "  SNGL_CLGSETP_SCH;        !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  SNGL_CLGSETP_SCH,        !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
        " ",
        "Zone,",
        "  Core_basement,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_basement Thermostat,  !- Name",
        "  Core_basement,             !- Zone or ZoneList Name",
        "  Single Cooling Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type",
        "  Core_basement CoolHeatSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Heating Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,3;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleHeatingOrCooling,",
        "  Core_basement CoolHeatSPSched, !- Name",
        "  CLGHTGSETP_SCH;             !- Heating Setpoint Temperature Schedule Name",
        " ",
        "Zone,",
        "  Core_bottom,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,  !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "  Core_bottom DualSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Dual Zone Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,4;          !- Field 3",
        " ",
        "ThermostatSetpoint:DualSetpoint,",
        "  Core_bottom DualSPSched, !- Name",
        "  HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "  CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  CLGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
        " ",
        "Schedule:Compact,",
        "  HTGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,15.0;       !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGHTGSETP_SCH,          !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    int HeatZoneNum(1);
    int CoolZoneNum(2);
    int CoolHeatZoneNum(3);
    int DualZoneNum(4);

    state->dataGlobal->NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(*state);               // read schedules

    GetZoneAirSetPoints(*state);

    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->TempControlType.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->TempControlTypeRpt.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneTempPredictorCorrector->ZoneSetPointLast.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneEnergyDemand->Setback.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneTempPredictorCorrector->TempDepZnLd.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneTempPredictorCorrector->TempIndZnLd.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataZoneTempPredictorCorrector->TempDepZnLd = 0.0;
    state->dataZoneTempPredictorCorrector->TempIndZnLd = 0.0;

    state->dataHeatBal->ZoneSNLoadPredictedRate.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->LoadCorrectionFactor.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBal->ZoneSNLoadPredictedHSPRate.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBal->ZoneSNLoadPredictedCSPRate.allocate(state->dataZoneCtrls->NumTempControlledZones);

    state->dataHeatBalFanSys->LoadCorrectionFactor(HeatZoneNum) = 1.0;
    state->dataHeatBalFanSys->LoadCorrectionFactor(CoolZoneNum) = 1.0;
    state->dataHeatBalFanSys->LoadCorrectionFactor(CoolHeatZoneNum) = 1.0;
    state->dataHeatBalFanSys->LoadCorrectionFactor(DualZoneNum) = 1.0;

    // The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
    //	extern int const SingleHeatingSetPoint; = 1
    //	extern int const SingleCoolingSetPoint; = 2
    //	extern int const SingleHeatCoolSetPoint; = 3
    //	extern int const DualSetPointWithDeadBand; = 4
    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(HeatZoneNum).CTSchedIndex).CurrentValue =
        static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating);
    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(CoolZoneNum).CTSchedIndex).CurrentValue =
        static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling);
    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(CoolHeatZoneNum).CTSchedIndex).CurrentValue =
        static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool);

    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(DualZoneNum).CTSchedIndex).CurrentValue =
        0; // simulate no thermostat or non-controlled zone

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired =
        0.0; // no load and no thermostat since control type is set to 0 above
    CalcZoneAirTempSetPoints(*state);
    CalcPredictedSystemLoad(*state, DualZoneNum, 1.0);

    EXPECT_EQ(0.0,
              state->dataHeatBalFanSys->TempZoneThermostatSetPoint(
                  DualZoneNum)); // Set point initialized to 0 and never set since thermostat control type = 0

    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(DualZoneNum).CTSchedIndex).CurrentValue =
        static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand); // reset Tstat control schedule to dual thermostat control

    // set up a back calculated load
    // for the first few, TempIndZnLd() = 0.0
    // LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    // LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    int SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(HeatZoneNum).SchIndx_SingleHeatSetPoint;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired = -1000.0; // cooling load
    state->dataZoneTempPredictorCorrector->TempDepZnLd(HeatZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;

    CalcZoneAirTempSetPoints(*state);
    CalcPredictedSystemLoad(*state, HeatZoneNum, 1.0);

    EXPECT_EQ(20.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(HeatZoneNum));
    EXPECT_EQ(-1000.0,
              state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum)
                  .TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load
    EXPECT_TRUE(state->dataZoneEnergyDemand->CurDeadBandOrSetback(HeatZoneNum)); // Tstat should show there is no load on a single heating SP

    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(HeatZoneNum).SchIndx_SingleHeatSetPoint;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 21.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired = 1000.0; // heating load
    state->dataZoneTempPredictorCorrector->TempDepZnLd(HeatZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(CoolZoneNum).SchIndx_SingleCoolSetPoint;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 23.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolZoneNum).TotalOutputRequired = -3000.0; // cooling load
    state->dataZoneTempPredictorCorrector->TempDepZnLd(CoolZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(CoolHeatZoneNum).SchIndx_SingleHeatCoolSetPoint;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 22.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolHeatZoneNum).TotalOutputRequired = -4000.0; // cooling load
    state->dataZoneTempPredictorCorrector->TempDepZnLd(CoolHeatZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolHeatZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBandCool;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 24.0;
    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBandHeat;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired = 2500.0; // heating load
    state->dataZoneTempPredictorCorrector->TempDepZnLd(DualZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;

    CalcZoneAirTempSetPoints(*state);
    CalcPredictedSystemLoad(*state, HeatZoneNum, 1.0);

    EXPECT_EQ(21.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(HeatZoneNum));
    EXPECT_FALSE(state->dataZoneEnergyDemand->CurDeadBandOrSetback(HeatZoneNum)); // Tstat should show there is load on a single heating SP
    EXPECT_EQ(1000.0,
              state->dataZoneEnergyDemand->ZoneSysEnergyDemand(HeatZoneNum)
                  .TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(*state, CoolZoneNum, 1.0);

    EXPECT_EQ(23.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(CoolZoneNum));
    EXPECT_FALSE(state->dataZoneEnergyDemand->CurDeadBandOrSetback(CoolZoneNum)); // Tstat should show there is load on a single cooling SP
    EXPECT_EQ(-3000.0,
              state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolZoneNum)
                  .TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(*state, CoolHeatZoneNum, 1.0);

    ASSERT_EQ(22.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(CoolHeatZoneNum));
    EXPECT_FALSE(
        state->dataZoneEnergyDemand->CurDeadBandOrSetback(CoolHeatZoneNum)); // Tstat should show there is load on a single heating or cooling SP
    EXPECT_EQ(-4000.0,
              state->dataZoneEnergyDemand->ZoneSysEnergyDemand(CoolHeatZoneNum)
                  .TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(*state, DualZoneNum, 1.0);

    EXPECT_EQ(20.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(DualZoneNum));
    EXPECT_FALSE(state->dataZoneEnergyDemand->CurDeadBandOrSetback(DualZoneNum)); // Tstat should show there is load on a dual SP
    EXPECT_EQ(2500.0,
              state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum)
                  .TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    SetPointTempSchedIndex = state->dataZoneCtrls->TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBandCool;
    state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue = 25.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired = 1000.0;
    // LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    state->dataZoneTempPredictorCorrector->TempDepZnLd(DualZoneNum) =
        state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired /
        state->dataScheduleMgr->Schedule(SetPointTempSchedIndex).CurrentValue;
    state->dataZoneTempPredictorCorrector->TempIndZnLd(DualZoneNum) = 3500.0; // results in a cooling load

    CalcZoneAirTempSetPoints(*state);
    CalcPredictedSystemLoad(*state, DualZoneNum, 1.0);

    EXPECT_EQ(25.0, state->dataHeatBalFanSys->TempZoneThermostatSetPoint(DualZoneNum));
    EXPECT_FALSE(state->dataZoneEnergyDemand->CurDeadBandOrSetback(DualZoneNum)); // Tstat should show there is load on a dual SP
    EXPECT_EQ(-2500.0, state->dataZoneEnergyDemand->ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired); // should show a cooling load
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_AdaptiveThermostat)
{
    // AUTHOR: Xuan Luo
    // DATE WRITTEN: Jan 2017

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Core_top,                !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_middle,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_basement,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_bottom,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_top Thermostat,                   !- Name",
        "  Core_top,                              !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,     !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,      !- Control 1 Object Type",
        "  Core_top CoolSPSched;                  !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_top Thermostat,                   !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveASH55CentralLine;              !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_middle Thermostat,                !- Name",
        "  Core_middle,                           !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,     !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,      !- Control 1 Object Type",
        "  Core_middle CoolSPSched;               !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_middle Thermostat,                !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveCEN15251CentralLine;           !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_basement Thermostat,                   !- Name",
        "  Core_basement,                              !- Zone or ZoneList Name",
        "  Single Cooling Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type",
        "  Core_basement CoolHeatSPSched;              !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_basement Thermostat,              !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  None;                                  !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,                !- Name",
        "  Core_bottom,                           !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,          !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,       !- Control 1 Object Type",
        "  Core_bottom DualSPSched;               !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_bottom Thermostat,                !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveASH55CentralLine;              !- Adaptive Comfort Model Type",
        " ",
        "ThermostatSetpoint:SingleCooling,",
        "  Core_middle CoolSPSched,               !- Name",
        "  SNGL_CLGSETP_SCH;                      !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "ThermostatSetpoint:SingleHeatingOrCooling,",
        "  Core_basement CoolHeatSPSched,         !- Name",
        "  CLGHTGSETP_SCH;                        !- Heating Setpoint Temperature Schedule Name",
        " ",
        "ThermostatSetpoint:DualSetpoint,",
        "  Core_bottom DualSPSched,               !- Name",
        "  HTGSETP_SCH,                           !- Heating Setpoint Temperature Schedule Name",
        "  CLGSETP_SCH;                           !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Control Type Sched,  !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,2;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  SNGL_CLGSETP_SCH,                      !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Heating Control Type Sched,  !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,3;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  Dual Zone Control Type Sched,          !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,4;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGSETP_SCH,                           !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  HTGSETP_SCH,                           !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,15.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGHTGSETP_SCH,                        !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // Tstat should show if the idf is legel

    int ZoneNum(4);
    int CoolZoneASHNum(1);
    int CoolZoneCENNum(2);
    int NoneAdapZoneNum(3);
    int DualZoneNum(4);
    int summerDesignDayTypeIndex(9);
    int constexpr ASH55_CENTRAL(2);
    int constexpr CEN15251_CENTRAL(5);

    state->dataEnvrn->DayOfYear = 1;
    state->dataWeatherManager->Envrn = 1;
    state->dataWeatherManager->Environment.allocate(1);
    state->dataWeatherManager->DesDayInput.allocate(1);
    state->dataWeatherManager->Environment(state->dataWeatherManager->Envrn).KindOfEnvrn = DataGlobalConstants::KindOfSim::RunPeriodWeather;
    state->dataWeatherManager->DesDayInput(state->dataWeatherManager->Envrn).DayType = summerDesignDayTypeIndex;
    state->dataWeatherManager->DesDayInput(state->dataWeatherManager->Envrn).MaxDryBulb = 30.0;
    state->dataWeatherManager->DesDayInput(state->dataWeatherManager->Envrn).DailyDBRange = 10.0;
    Real64 ZoneAirSetPoint = 0.0;

    bool ErrorsFound(false); // If errors detected in input
    GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound); // Tstat should show if there is error in zone processing
    ASSERT_FALSE(state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule
                     .initialized); // Tstat should show there adaptive model is not initialized

    Array1D<Real64> runningAverageASH_1(365, 0.0);
    Array1D<Real64> runningAverageCEN_1(365, 0.0);
    CalculateAdaptiveComfortSetPointSchl(*state, runningAverageASH_1, runningAverageCEN_1);
    // Tstat should show flage that adaptive comfort is not applicable (-1)
    ASSERT_EQ(
        -1, state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(-1,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(
                  state->dataEnvrn->DayOfYear));

    Array1D<Real64> runningAverageASH_2(365, 40.0);
    Array1D<Real64> runningAverageCEN_2(365, 40.0);
    CalculateAdaptiveComfortSetPointSchl(*state, runningAverageASH_2, runningAverageCEN_2);
    // Tstat should show flage that adaptive comfort is not applicable (-1)
    ASSERT_EQ(
        -1, state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(
        -1,
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(state->dataEnvrn->DayOfYear));
    ASSERT_EQ(-1,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(
                  state->dataEnvrn->DayOfYear));

    Array1D<Real64> runningAverageASH(365, 25.0);
    Array1D<Real64> runningAverageCEN(365, 25.0);
    CalculateAdaptiveComfortSetPointSchl(*state, runningAverageASH, runningAverageCEN);
    ASSERT_TRUE(
        state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized); // Tstat should show there adaptive model is initialized
    ASSERT_EQ(25.55,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(
                  state->dataEnvrn->DayOfYear)); // Tstat should show ASH 55 CENTRAL LINE model set point
    ASSERT_EQ(28.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(
                  state->dataEnvrn->DayOfYear)); // Tstat should show ASH 55 Upper 90 LINE model set point
    ASSERT_EQ(29.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(
                  state->dataEnvrn->DayOfYear)); // Tstat should show ASH 55 Upper 80 LINE model set point
    ASSERT_EQ(27.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(
                  state->dataEnvrn->DayOfYear)); // Tstat should show CEN 15251 CENTRAL LINE model set point
    ASSERT_EQ(29.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(
                  state->dataEnvrn->DayOfYear)); // Tstat should show CEN 15251 Upper I LINE model set point
    ASSERT_EQ(30.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(
                  state->dataEnvrn->DayOfYear)); // Tstat should show CEN 15251 Upper II LINE model set point
    ASSERT_EQ(31.05,
              state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(
                  state->dataEnvrn->DayOfYear)); // Tstat should show CEN 15251 Upper III LINE model set point
    ASSERT_EQ(25.55,
              state->dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay[0]); // Tstat should show ASH 55 CENTRAL LINE model set point
    ASSERT_EQ(27.05,
              state->dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay[3]); // Tstat should show CEN 15251 CENTRAL LINE model set point

    state->dataZoneCtrls->TempControlledZone.allocate(ZoneNum);
    state->dataZoneCtrls->TempControlledZone(CoolZoneASHNum).AdaptiveComfortTempControl = true;
    state->dataZoneCtrls->TempControlledZone(CoolZoneASHNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;
    state->dataZoneCtrls->TempControlledZone(CoolZoneCENNum).AdaptiveComfortTempControl = true;
    state->dataZoneCtrls->TempControlledZone(CoolZoneCENNum).AdaptiveComfortModelTypeIndex = CEN15251_CENTRAL;
    state->dataZoneCtrls->TempControlledZone(NoneAdapZoneNum).AdaptiveComfortTempControl = true;
    state->dataZoneCtrls->TempControlledZone(NoneAdapZoneNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;
    state->dataZoneCtrls->TempControlledZone(DualZoneNum).AdaptiveComfortTempControl = true;
    state->dataZoneCtrls->TempControlledZone(DualZoneNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;

    ZoneAirSetPoint = 0.0;
    AdjustOperativeSetPointsforAdapComfort(*state, CoolZoneASHNum, ZoneAirSetPoint);
    ASSERT_EQ(25.55, ZoneAirSetPoint); // Tstat should show set point overwritten by ASH 55 CENTRAL LINE model

    ZoneAirSetPoint = 0.0;
    AdjustOperativeSetPointsforAdapComfort(*state, CoolZoneCENNum, ZoneAirSetPoint);
    ASSERT_EQ(27.05, ZoneAirSetPoint); // Tstat should show set point overwritten by CEN 15251 CENTRAL LINE model

    ZoneAirSetPoint = 0.0;
    state->dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(state->dataEnvrn->DayOfYear) = -1;
    AdjustOperativeSetPointsforAdapComfort(*state, NoneAdapZoneNum, ZoneAirSetPoint);
    ASSERT_EQ(0, ZoneAirSetPoint); // Tstat should show set point is not overwritten

    ZoneAirSetPoint = 26.0;
    AdjustOperativeSetPointsforAdapComfort(*state, DualZoneNum, ZoneAirSetPoint);
    ASSERT_EQ(26.0, ZoneAirSetPoint); // Tstat should show set point is not overwritten
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_CalcZoneSums_SurfConvectionTest)
{
    // AUTHOR: L. Gu, FSEC
    // DATE WRITTEN: Jan 2017
    // #5906 Adaptive convection resulting in extremely low zone temperature which causes fatal error

    int ZoneNum = 1;         // Zone number
    Real64 SumIntGain = 0.0; // Zone sum of convective internal gains
    Real64 SumHA = 0.0;      // Zone sum of Hc*Area
    Real64 SumHATsurf = 0.0; // Zone sum of Hc*Area*Tsurf
    Real64 SumHATref = 0.0;  // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
    Real64 SumMCp = 0.0;     // Zone sum of MassFlowRate*Cp
    Real64 SumMCpT = 0.0;    // Zone sum of MassFlowRate*Cp*T
    Real64 SumSysMCp = 0.0;  // Zone sum of air system MassFlowRate*Cp
    Real64 SumSysMCpT = 0.0; // Zone sum of air system MassFlowRate*Cp*T

    state->dataHeatBal->ZoneIntGain.allocate(ZoneNum);
    state->dataHeatBalFanSys->SumConvHTRadSys.allocate(ZoneNum);
    state->dataHeatBalFanSys->SumConvPool.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPI.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPV.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPM.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPE.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPC.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPTI.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPTV.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPTM.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPTE.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPTC.allocate(ZoneNum);
    state->dataHeatBalFanSys->MDotCPOA.allocate(ZoneNum);
    state->dataHeatBalFanSys->MCPI(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPV(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPM(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPE(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPC(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPTI(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPTV(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPTM(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPTE(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MCPTC(ZoneNum) = 0.0;
    state->dataHeatBalFanSys->MDotCPOA(ZoneNum) = 0.0;

    state->dataHeatBalFanSys->SumConvHTRadSys(1) = 0.0;
    state->dataHeatBalFanSys->SumConvPool(1) = 0.0;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone 1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;

    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = state->dataZoneEquip->ZoneEquipConfig(1).ZoneName;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->CurZoneEqNum = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1.0;
    state->dataHeatBal->Zone(1).Volume = 1000.0;
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataHeatBalFanSys->ZoneLatentGain(1) = 0.0;
    state->dataHeatBalFanSys->SumLatentHTRadSys.allocate(1);
    state->dataHeatBalFanSys->SumLatentHTRadSys(1) = 0.0;
    state->dataHeatBalFanSys->SumLatentPool.allocate(1);
    state->dataHeatBalFanSys->SumLatentPool(1) = 0.0;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).HTSurfaceLast = 3;
    state->dataSurface->Surface.allocate(3);
    state->dataHeatBalSurf->SurfHConvInt.allocate(3);
    state->dataLoopNodes->Node.allocate(4);
    state->dataHeatBal->SurfTempEffBulkAir.allocate(3);
    state->dataHeatBalSurf->SurfTempInTmp.allocate(3);

    state->dataSurface->SurfTAirRef.allocate(3);
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::RefAirTemp::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::RefAirTemp::ZoneSupplyAirTemp;

    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(1).Area = 10.0;
    state->dataSurface->Surface(2).Area = 10.0;
    state->dataSurface->Surface(3).Area = 10.0;
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBal->SurfTempEffBulkAir(1) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(2) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(3) = 10.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;

    state->dataZonePlenum->NumZoneReturnPlenums = 0;
    state->dataZonePlenum->NumZoneSupplyPlenums = 0;

    CalcZoneSums(*state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);
    EXPECT_EQ(5.0, SumHA);
    EXPECT_EQ(300.0, SumHATsurf);
    EXPECT_EQ(150.0, SumHATref);

    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    CalcZoneSums(*state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);
    EXPECT_EQ(10.0, SumHA);
    EXPECT_EQ(300.0, SumHATsurf);
    EXPECT_EQ(50.0, SumHATref);

    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.2;
    CalcZoneSums(*state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);
    EXPECT_NEAR(302.00968500, SumSysMCp, 0.0001);
    EXPECT_NEAR(6040.1937, SumSysMCpT, 0.0001);

    CalcZoneSums(*state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, false);
    EXPECT_EQ(0.0, SumSysMCp);
    EXPECT_EQ(0.0, SumSysMCpT);
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_EMSOverrideSetpointTest)
{
    // AUTHOR: L. Gu, FSEC
    // DATE WRITTEN: Jun. 2017
    // #5870 EMS actuators for Zone Temperature Control not working

    state->dataZoneCtrls->NumTempControlledZones = 1;
    state->dataZoneCtrls->NumComfortControlledZones = 0;
    state->dataZoneCtrls->TempControlledZone.allocate(1);
    state->dataZoneCtrls->TempControlledZone(1).EMSOverrideHeatingSetPointOn = true;
    state->dataZoneCtrls->TempControlledZone(1).EMSOverrideCoolingSetPointOn = true;
    state->dataZoneCtrls->TempControlledZone(1).ActualZoneNum = 1;
    state->dataZoneCtrls->TempControlledZone(1).EMSOverrideHeatingSetPointValue = 23;
    state->dataZoneCtrls->TempControlledZone(1).EMSOverrideCoolingSetPointValue = 26;

    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlTypeRpt.allocate(1);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(1);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;

    OverrideAirSetPointsforEMSCntrl(*state);
    EXPECT_EQ(23.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    state->dataZoneCtrls->NumTempControlledZones = 0;
    state->dataZoneCtrls->NumComfortControlledZones = 1;
    state->dataZoneCtrls->ComfortControlledZone.allocate(1);
    state->dataHeatBalFanSys->ComfortControlType.allocate(1);
    state->dataZoneCtrls->ComfortControlledZone(1).ActualZoneNum = 1;
    state->dataZoneCtrls->ComfortControlledZone(1).EMSOverrideHeatingSetPointOn = true;
    state->dataZoneCtrls->ComfortControlledZone(1).EMSOverrideCoolingSetPointOn = true;
    state->dataHeatBalFanSys->ComfortControlType(1) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;
    state->dataZoneCtrls->ComfortControlledZone(1).EMSOverrideHeatingSetPointValue = 22;
    state->dataZoneCtrls->ComfortControlledZone(1).EMSOverrideCoolingSetPointValue = 25;

    OverrideAirSetPointsforEMSCntrl(*state);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(25.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, temperatureAndCountInSch_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",
        " ",
        "Schedule:Compact,",
        " Sched1,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 20.0;        !- Field 26",
        " ",
        "Schedule:Compact,",
        " Sched2,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 1/31,            !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 24.0,        !- Field 26",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 26.0;        !- Field 26",
        " ",
        "Schedule:Compact,",
        " Sched3,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 1/31,            !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 09:00, 24.0,        !- Field 26",
        " Until: 17:00, 26.0,        !- Field 26",
        " Until: 24:00, 24.0,        !- Field 26",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 26.0;        !- Field 26",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataEnvrn->CurrentYearIsLeapYear = false;

    Real64 valueAtTime;
    int numDays;
    std::string monthAssumed;
    constexpr int wednesday = 4;

    state->dataEnvrn->Latitude = 30.; // northern hemisphere
    int sched1Index = GetScheduleIndex(*state, "SCHED1");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched1Index, false, wednesday, 11);

    EXPECT_EQ(20, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("January", monthAssumed);

    // test month selected based on hemisphere and isSummer flag.
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched1Index, true, wednesday, 11);
    EXPECT_EQ("July", monthAssumed);

    state->dataEnvrn->Latitude = -30.; // southern hemisphere
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched1Index, false, wednesday, 11);
    EXPECT_EQ("July", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched1Index, true, wednesday, 11);
    EXPECT_EQ("January", monthAssumed);

    state->dataEnvrn->Latitude = 30.; // northern hemisphere
    int sched2Index = GetScheduleIndex(*state, "SCHED2");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched2Index, false, wednesday, 11);

    EXPECT_EQ(24, valueAtTime);
    EXPECT_EQ(31, numDays);
    EXPECT_EQ("January", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched2Index, true, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(334, numDays);
    EXPECT_EQ("July", monthAssumed);

    int sched3Index = GetScheduleIndex(*state, "SCHED3");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched3Index, false, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("January", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched3Index, true, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("July", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(*state, sched3Index, false, wednesday, 19);

    EXPECT_EQ(24, valueAtTime);
    EXPECT_EQ(31, numDays);
    EXPECT_EQ("January", monthAssumed);
}

TEST_F(EnergyPlusFixture, SetPointWithCutoutDeltaT_test)
{
    // On/Off thermostat
    state->dataScheduleMgr->Schedule.allocate(3);

    state->dataZoneCtrls->NumTempControlledZones = 1;

    // SingleHeatingSetPoint
    state->dataZoneCtrls->TempControlledZone.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(1);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(1);
    state->dataHeatBalFanSys->ZoneT1.allocate(1);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataHeatBalFanSys->AIRRAT.allocate(1);
    state->dataZoneTempPredictorCorrector->TempDepZnLd.allocate(1);
    state->dataZoneTempPredictorCorrector->TempIndZnLd.allocate(1);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(1);
    state->dataHeatBal->Zone.allocate(1);
    state->dataZoneTempPredictorCorrector->ZoneSetPointLast.allocate(1);
    state->dataZoneEnergyDemand->Setback.allocate(1);

    state->dataHeatBal->ZoneSNLoadPredictedRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadPredictedHSPRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadPredictedCSPRate.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(1);
    state->dataHeatBalFanSys->LoadCorrectionFactor.allocate(1);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(1);

    state->dataHeatBal->ZoneAirSolutionAlgo = DataHeatBalance::SolutionAlgo::EulerMethod;

    state->dataZoneCtrls->TempControlledZone(1).DeltaTCutSet = 2.0;
    state->dataZoneCtrls->TempControlledZone(1).ActualZoneNum = 1;
    state->dataZoneCtrls->TempControlledZone(1).CTSchedIndex = 1;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlTypeRpt.allocate(1);
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleHeatSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleHeating.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleHeating(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 22.0;
    state->dataHeatBalFanSys->AIRRAT(1) = 2000;
    state->dataZoneTempPredictorCorrector->TempDepZnLd(1) = 1.0;
    state->dataZoneTempPredictorCorrector->TempIndZnLd(1) = 1.0;
    state->dataHeatBalFanSys->MAT(1) = 20.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);
    state->dataZoneTempPredictorCorrector->NumOnOffCtrZone = 1;

    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));

    state->dataHeatBalFanSys->MAT(1) = 23.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = false;

    // SingleCoolingSetPoint
    state->dataScheduleMgr->Schedule(1).CurrentValue = 2;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleCoolSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleCooling.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleCooling(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 26.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);

    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = false;

    state->dataHeatBalFanSys->MAT(1) = 27.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // SingleHeatCoolSetPoint
    state->dataScheduleMgr->Schedule(1).CurrentValue = 3;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleHeatCoolSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleHeatCool.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleHeatCool(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 24.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);

    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool.allocate(1);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 4;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_DualSetPointWDeadBandHeat = 2;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_DualSetPointWDeadBandCool = 3;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).HeatTempSchedIndex = 2;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).CoolTempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 22.0;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 26.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);

    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = true;
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = false;

    // DualSetPointWithDeadBand : Adjust heating setpoint
    state->dataHeatBalFanSys->MAT(1) = 21.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = true;
    state->dataHeatBalFanSys->MAT(1) = 27.0;
    state->dataHeatBalFanSys->ZoneT1(1) = state->dataHeatBalFanSys->MAT(1);
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, TempAtPrevTimeStepWithCutoutDeltaT_test)
{
    state->dataScheduleMgr->Schedule.allocate(3);
    state->dataZoneCtrls->NumTempControlledZones = 1;

    // SingleHeatingSetPoint
    state->dataZoneCtrls->TempControlledZone.allocate(state->dataZoneCtrls->NumTempControlledZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(1);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(1);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(1);
    state->dataHeatBalFanSys->XMPT.allocate(1);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataHeatBalFanSys->AIRRAT.allocate(1);
    state->dataZoneTempPredictorCorrector->TempDepZnLd.allocate(1);
    state->dataZoneTempPredictorCorrector->TempIndZnLd.allocate(1);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(1);
    state->dataHeatBal->Zone.allocate(1);
    state->dataZoneTempPredictorCorrector->ZoneSetPointLast.allocate(1);
    state->dataZoneEnergyDemand->Setback.allocate(1);

    state->dataHeatBal->ZoneSNLoadPredictedRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadPredictedHSPRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadPredictedCSPRate.allocate(1);
    state->dataZoneEnergyDemand->CurDeadBandOrSetback.allocate(1);
    state->dataHeatBalFanSys->LoadCorrectionFactor.allocate(1);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(1);

    state->dataHeatBal->ZoneAirSolutionAlgo = DataHeatBalance::SolutionAlgo::ThirdOrder;

    state->dataZoneCtrls->TempControlledZone(1).DeltaTCutSet = 2.0;
    state->dataZoneCtrls->TempControlledZone(1).ActualZoneNum = 1;
    state->dataZoneCtrls->TempControlledZone(1).CTSchedIndex = 1;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlTypeRpt.allocate(1);
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleHeatSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleHeating.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleHeating(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 22.0;
    state->dataHeatBalFanSys->AIRRAT(1) = 2000;
    state->dataZoneTempPredictorCorrector->TempDepZnLd(1) = 1.0;
    state->dataZoneTempPredictorCorrector->TempIndZnLd(1) = 1.0;
    state->dataHeatBalFanSys->MAT(1) = 20.0;
    state->dataHeatBalFanSys->XMPT(1) = 23.0;
    state->dataZoneTempPredictorCorrector->NumOnOffCtrZone = 1;

    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));

    state->dataZoneCtrls->TempControlledZone(1).HeatModeLastSave = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, true, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));

    // SingleCoolingSetPoint
    state->dataScheduleMgr->Schedule(1).CurrentValue = 2;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleCoolSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleCooling.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleCooling(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 26.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->XMPT(1) = 27;

    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = false;

    state->dataZoneCtrls->TempControlledZone(1).CoolModeLastSave = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, true, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // SingleHeatCoolSetPoint
    state->dataScheduleMgr->Schedule(1).CurrentValue = 3;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_SingleHeatCoolSetPoint = 3;
    state->dataZoneTempPredictorCorrector->SetPointSingleHeatCool.allocate(1);
    state->dataZoneTempPredictorCorrector->SetPointSingleHeatCool(1).TempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 24.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->XMPT(1) = state->dataHeatBalFanSys->MAT(1);

    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool.allocate(1);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 4;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_DualSetPointWDeadBandHeat = 2;
    state->dataZoneCtrls->TempControlledZone(1).SchIndx_DualSetPointWDeadBandCool = 3;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).HeatTempSchedIndex = 2;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).CoolTempSchedIndex = 3;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 22.0;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 26.0;
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->XMPT(1) = 21.0;

    state->dataZoneCtrls->TempControlledZone(1).CoolModeLast = true;
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, false, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLast = false;

    // DualSetPointWithDeadBand : Adjust heating setpoint
    state->dataZoneCtrls->TempControlledZone(1).HeatModeLastSave = true;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, true, false, 0.01);
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    state->dataZoneCtrls->TempControlledZone(1).CoolModeLastSave = true;
    state->dataHeatBalFanSys->XMPT(1) = 27.0;
    CalcZoneAirTempSetPoints(*state);
    PredictSystemLoads(*state, true, false, 0.01);
    EXPECT_EQ(22.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, ReportMoistLoadsZoneMultiplier_Test)
{
    Real64 TotOutReq;
    Real64 OutReqToHumSP;
    Real64 OutReqToDehumSP;
    Real64 SingleZoneTotRate;
    Real64 SingleZoneHumRate;
    Real64 SingleZoneDehRate;
    Real64 ZoneMultiplier;
    Real64 ZoneMultiplierList;
    Real64 ExpectedResult;
    Real64 AcceptableTolerance = 0.00001;

    // Test 1: Zone Multipliers are all unity (1.0).  So, single zone loads should be the same as total loads
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 1.0;
    ReportMoistLoadsZoneMultiplier(
        TotOutReq, OutReqToHumSP, OutReqToDehumSP, SingleZoneTotRate, SingleZoneHumRate, SingleZoneDehRate, ZoneMultiplier, ZoneMultiplierList);
    EXPECT_NEAR(TotOutReq, SingleZoneTotRate, AcceptableTolerance);
    EXPECT_NEAR(OutReqToHumSP, SingleZoneHumRate, AcceptableTolerance);
    EXPECT_NEAR(OutReqToDehumSP, SingleZoneDehRate, AcceptableTolerance);

    // Test 2a: Zone Multiplier (non-list) is greater than 1, list Zone Multiplier is still one
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 7.0;
    ZoneMultiplierList = 1.0;
    ReportMoistLoadsZoneMultiplier(
        TotOutReq, OutReqToHumSP, OutReqToDehumSP, SingleZoneTotRate, SingleZoneHumRate, SingleZoneDehRate, ZoneMultiplier, ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHumRate, AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneDehRate, AcceptableTolerance);
    ExpectedResult = 7000.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 14000.0;
    EXPECT_NEAR(OutReqToHumSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 21000.0;
    EXPECT_NEAR(OutReqToDehumSP, ExpectedResult, AcceptableTolerance);

    // Test 2b: list Zone Multiplier is greater than 1, non-list Zone Multiplier is one
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 7.0;
    ReportMoistLoadsZoneMultiplier(
        TotOutReq, OutReqToHumSP, OutReqToDehumSP, SingleZoneTotRate, SingleZoneHumRate, SingleZoneDehRate, ZoneMultiplier, ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHumRate, AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneDehRate, AcceptableTolerance);
    ExpectedResult = 7000.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 14000.0;
    EXPECT_NEAR(OutReqToHumSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 21000.0;
    EXPECT_NEAR(OutReqToDehumSP, ExpectedResult, AcceptableTolerance);

    // Test 3: both zone multipliers are greater than 1.0
    TotOutReq = 300.0;
    OutReqToHumSP = 150.0;
    OutReqToDehumSP = 100.0;
    ZoneMultiplier = 2.0;
    ZoneMultiplierList = 3.0;
    ReportMoistLoadsZoneMultiplier(
        TotOutReq, OutReqToHumSP, OutReqToDehumSP, SingleZoneTotRate, SingleZoneHumRate, SingleZoneDehRate, ZoneMultiplier, ZoneMultiplierList);
    ExpectedResult = 300.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 150.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHumRate, AcceptableTolerance);
    ExpectedResult = 100.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneDehRate, AcceptableTolerance);
    ExpectedResult = 1800.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 900.0;
    EXPECT_NEAR(OutReqToHumSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 600.0;
    EXPECT_NEAR(OutReqToDehumSP, ExpectedResult, AcceptableTolerance);
}

TEST_F(EnergyPlusFixture, ReportSensibleLoadsZoneMultiplier_Test)
{
    Real64 TotOutReq;
    Real64 OutReqToHeatSP;
    Real64 OutReqToCoolSP;
    Real64 SingleZoneTotRate;
    Real64 SingleZoneHeatRate;
    Real64 SingleZoneCoolRate;
    Real64 HeatToSP;
    Real64 CoolToSP;
    Real64 CorrectionFactor;
    Real64 ZoneMultiplier;
    Real64 ZoneMultiplierList;
    Real64 ExpectedResult;
    Real64 AcceptableTolerance = 0.00001;

    // Test 1: Zone Multipliers and Load Correction Factor are all unity (1.0).  So, single zone loads should be the same as total loads
    TotOutReq = 1000.0;
    OutReqToHeatSP = 0.0;
    OutReqToCoolSP = 0.0;
    HeatToSP = 2000.0;
    CoolToSP = 3000.0;
    CorrectionFactor = 1.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 1.0;
    ReportSensibleLoadsZoneMultiplier(TotOutReq,
                                      OutReqToHeatSP,
                                      OutReqToCoolSP,
                                      SingleZoneTotRate,
                                      SingleZoneHeatRate,
                                      SingleZoneCoolRate,
                                      HeatToSP,
                                      CoolToSP,
                                      CorrectionFactor,
                                      ZoneMultiplier,
                                      ZoneMultiplierList);
    EXPECT_NEAR(TotOutReq, SingleZoneTotRate, AcceptableTolerance);
    EXPECT_NEAR(OutReqToHeatSP, SingleZoneHeatRate, AcceptableTolerance);
    EXPECT_NEAR(OutReqToCoolSP, SingleZoneCoolRate, AcceptableTolerance);

    // Test 2a: Zone Multiplier (non-list) is greater than 1, list Zone Multiplier and Load Correction are still one
    TotOutReq = 1000.0;
    OutReqToHeatSP = 0.0;
    OutReqToCoolSP = 0.0;
    HeatToSP = 2000.0;
    CoolToSP = 3000.0;
    CorrectionFactor = 1.0;
    ZoneMultiplier = 4.0;
    ZoneMultiplierList = 1.0;
    ReportSensibleLoadsZoneMultiplier(TotOutReq,
                                      OutReqToHeatSP,
                                      OutReqToCoolSP,
                                      SingleZoneTotRate,
                                      SingleZoneHeatRate,
                                      SingleZoneCoolRate,
                                      HeatToSP,
                                      CoolToSP,
                                      CorrectionFactor,
                                      ZoneMultiplier,
                                      ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHeatRate, AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneCoolRate, AcceptableTolerance);
    ExpectedResult = 4000.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 8000.0;
    EXPECT_NEAR(OutReqToHeatSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 12000.0;
    EXPECT_NEAR(OutReqToCoolSP, ExpectedResult, AcceptableTolerance);

    // Test 2b: list Zone Multiplier is greater than 1, non-list Zone Multiplier and Load Correction are still one
    TotOutReq = 1000.0;
    OutReqToHeatSP = 0.0;
    OutReqToCoolSP = 0.0;
    HeatToSP = 2000.0;
    CoolToSP = 3000.0;
    CorrectionFactor = 1.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 5.0;
    ReportSensibleLoadsZoneMultiplier(TotOutReq,
                                      OutReqToHeatSP,
                                      OutReqToCoolSP,
                                      SingleZoneTotRate,
                                      SingleZoneHeatRate,
                                      SingleZoneCoolRate,
                                      HeatToSP,
                                      CoolToSP,
                                      CorrectionFactor,
                                      ZoneMultiplier,
                                      ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHeatRate, AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneCoolRate, AcceptableTolerance);
    ExpectedResult = 5000.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 10000.0;
    EXPECT_NEAR(OutReqToHeatSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 15000.0;
    EXPECT_NEAR(OutReqToCoolSP, ExpectedResult, AcceptableTolerance);

    // Test 2c: list Zone Multiplier and Zone Multiplier are unity, Load Correction is not equal to 1.0
    TotOutReq = 1000.0;
    OutReqToHeatSP = 0.0;
    OutReqToCoolSP = 0.0;
    HeatToSP = 2000.0;
    CoolToSP = 3000.0;
    CorrectionFactor = 1.1;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 1.0;
    ReportSensibleLoadsZoneMultiplier(TotOutReq,
                                      OutReqToHeatSP,
                                      OutReqToCoolSP,
                                      SingleZoneTotRate,
                                      SingleZoneHeatRate,
                                      SingleZoneCoolRate,
                                      HeatToSP,
                                      CoolToSP,
                                      CorrectionFactor,
                                      ZoneMultiplier,
                                      ZoneMultiplierList);
    ExpectedResult = 1100.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2200.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHeatRate, AcceptableTolerance);
    ExpectedResult = 3300.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneCoolRate, AcceptableTolerance);
    ExpectedResult = 1100.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 2200.0;
    EXPECT_NEAR(OutReqToHeatSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 3300.0;
    EXPECT_NEAR(OutReqToCoolSP, ExpectedResult, AcceptableTolerance);

    // Test 3: none of the multipliers are unity
    TotOutReq = 1000.0;
    OutReqToHeatSP = 0.0;
    OutReqToCoolSP = 0.0;
    HeatToSP = 2000.0;
    CoolToSP = 3000.0;
    CorrectionFactor = 1.2;
    ZoneMultiplier = 2.0;
    ZoneMultiplierList = 1.5;
    ReportSensibleLoadsZoneMultiplier(TotOutReq,
                                      OutReqToHeatSP,
                                      OutReqToCoolSP,
                                      SingleZoneTotRate,
                                      SingleZoneHeatRate,
                                      SingleZoneCoolRate,
                                      HeatToSP,
                                      CoolToSP,
                                      CorrectionFactor,
                                      ZoneMultiplier,
                                      ZoneMultiplierList);
    ExpectedResult = 1200.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneTotRate, AcceptableTolerance);
    ExpectedResult = 2400.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneHeatRate, AcceptableTolerance);
    ExpectedResult = 3600.0;
    EXPECT_NEAR(ExpectedResult, SingleZoneCoolRate, AcceptableTolerance);
    ExpectedResult = 3600.0;
    EXPECT_NEAR(TotOutReq, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 7200.0;
    EXPECT_NEAR(OutReqToHeatSP, ExpectedResult, AcceptableTolerance);
    ExpectedResult = 10800.0;
    EXPECT_NEAR(OutReqToCoolSP, ExpectedResult, AcceptableTolerance);
}
