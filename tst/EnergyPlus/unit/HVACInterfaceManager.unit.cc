// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::HVACInterfaceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/Psychrometrics.hh>

namespace EnergyPlus {
TEST_F(EnergyPlusFixture, ExcessiveHeatStorage_Test)
{
    state->init_state(*state);
    using namespace DataPlant;
    using namespace HVACInterfaceManager;
    Real64 TankOutletTemp;
    state->dataHVACGlobal->TimeStepSys = 1;
    state->dataHVACGlobal->TimeStepSysSec = state->dataHVACGlobal->TimeStepSys * Constant::SecInHour;
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    // Set Up PlantLoop Variables
    state->dataPlnt->PlantLoop(1).Mass = 50;
    state->dataPlnt->PlantLoop(1).FluidName = "Water";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).OutNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).InNodeNum = 1;
    // Note LastTempInterfaceTankOutlet ends up getting reset to zero on the first pass
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LastTempInterfaceTankOutlet = 80;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).TotalPumpHeat = 500;

    auto &dln = state->dataLoopNodes;
    for (int i = 0; i < state->dataPlnt->TotNumLoops; ++i) dln->nodes.push_back(new Node::NodeData);

    dln->nodes(1)->Temp = 100;
    dln->nodes(1)->MassFlowRate = 10;
    state->dataPlnt->PlantLoop(1).OutNodeFlowrate = 10;

    // LoopSideInlet_MdotCpDeltaT should be < LoopSideInlet_McpDTdt
    // Therefore CapExcessStorageTime AND TotalTime will increase by 1 timestep
    UpdateHalfLoopInletTemp(*state, 1, DataPlant::LoopSideLocation::Demand, TankOutletTemp);
    // Excess storage calcs moved here
    PlantManager::UpdateNodeThermalHistory(*state);
    EXPECT_NEAR((2928.82 - 500), state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_MdotCpDeltaT, 0.001);
    EXPECT_NEAR(2928.82, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_McpDTdt, 0.001);
    EXPECT_EQ(1, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_CapExcessStorageTime);
    EXPECT_EQ(1, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_TotalTime);

    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LastTempInterfaceTankOutlet = 120; // random

    // LoopSideInlet_MdotCpDeltaT should be > LoopSideInlet_McpDTdt
    // Therefore TotalTime will increase by 1 more timestep, but CapExcessStorageTime will NOT increase
    UpdateHalfLoopInletTemp(*state, 1, DataPlant::LoopSideLocation::Demand, TankOutletTemp);
    // Excess storage calcs moved here
    PlantManager::UpdateNodeThermalHistory(*state);
    EXPECT_NEAR((-588.264 - 500), state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_MdotCpDeltaT, 0.001);
    EXPECT_NEAR(-588.264, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_McpDTdt, .001);
    EXPECT_EQ(1, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_CapExcessStorageTime);
    EXPECT_EQ(2, state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).LoopSideInlet_TotalTime);
}

TEST_F(EnergyPlusFixture, UpdateHVACInterface_Test)
{
    using namespace DataPlant;
    using namespace HVACInterfaceManager;

    int AirLoopNum = 1;
    int InNodeNum = 1;
    int OutNodeNum = 2;
    bool OutOfToleranceFlag = false;

    state->dataHVACInterfaceMgr->TmpRealARR.allocate(10);
    state->dataConvergeParams->AirLoopConvergence.allocate(AirLoopNum);

    auto &dln = state->dataLoopNodes;
    for (int i = 0; i < 2; ++i) dln->nodes.push_back(new Node::NodeData);
    
    DataConvergParams::CalledFrom CalledFrom = DataConvergParams::CalledFrom::AirSystemDemandSide;

    auto *inNode = dln->nodes(InNodeNum);
    auto *outNode = dln->nodes(OutNodeNum);
    
    inNode->MassFlowRate = 0.01;
    outNode->MassFlowRate = 0.01;
    inNode->HumRat = 0.001;
    outNode->HumRat = 0.001;
    inNode->Temp = 23.0;
    outNode->Temp = 23.0;
    inNode->Enthalpy = Psychrometrics::PsyHFnTdbW(23.0, 0.001);
    outNode->Enthalpy = Psychrometrics::PsyHFnTdbW(23.0, 0.001);
    inNode->Press = 101325.0;
    outNode->Press = 101325.0;
    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataContaminantBalance->Contaminant.GenericContamSimulation = true;
    inNode->CO2 = 400.0;
    outNode->CO2 = 400.0;
    inNode->GenContam = 20.0;
    outNode->GenContam = 20.0;

    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemDemandSide, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_FALSE(OutOfToleranceFlag);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[0]);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[0]);

    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemSupplySideDeck1, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_FALSE(OutOfToleranceFlag);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[1]);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[1]);

    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemSupplySideDeck2, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_FALSE(OutOfToleranceFlag);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[2]);
    EXPECT_FALSE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[2]);

    inNode->CO2 = 400.0;
    inNode->GenContam = 20.0;
    outNode->CO2 = 401.0;
    outNode->GenContam = 20.5;

    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemDemandSide, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_TRUE(OutOfToleranceFlag);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[0]);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[0]);

    inNode->CO2 = 400.0;
    inNode->GenContam = 20.0;
    outNode->CO2 = 401.0;
    outNode->GenContam = 20.5;
    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemSupplySideDeck1, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_TRUE(OutOfToleranceFlag);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[1]);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[1]);

    inNode->CO2 = 400.0;
    inNode->GenContam = 20.0;
    outNode->CO2 = 401.0;
    outNode->GenContam = 20.5;
    UpdateHVACInterface(*state, AirLoopNum, DataConvergParams::CalledFrom::AirSystemSupplySideDeck2, OutNodeNum, InNodeNum, OutOfToleranceFlag);

    EXPECT_TRUE(OutOfToleranceFlag);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACCO2NotConverged[2]);
    EXPECT_TRUE(state->dataConvergeParams->AirLoopConvergence(1).HVACGenContamNotConverged[2]);
}
} // namespace EnergyPlus
