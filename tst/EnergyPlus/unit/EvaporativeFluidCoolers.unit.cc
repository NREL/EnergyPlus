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

// EnergyPlus::Evaporative Cooler Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EvaporativeFluidCoolers.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimAirServingZones.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::EvaporativeFluidCoolers;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, EvapFluidCoolerSpecs_getDesignCapacitiesTest)
{
    Real64 MaxLoad;
    Real64 MinLoad;
    Real64 OptLoad;
    Real64 ExpectedMaxLoad;
    Real64 ExpectedMinLoad;
    Real64 ExpectedOptLoad;

    // Set up information required to actually run the routines that get called as a result of running this test.
    // In general, values set here attempt to avoid as much code as possible so that only the defect code is run.
    // Obviously, not everything can be skipped so some of this information is needed to avoid crashes in other routines.
    state->dataEvapFluidCoolers->SimpleEvapFluidCooler.allocate(1);
    auto &thisEFC = state->dataEvapFluidCoolers->SimpleEvapFluidCooler(1);
    thisEFC.Type = DataPlant::PlantEquipmentType::EvapFluidCooler_TwoSpd;
    thisEFC.MyOneTimeFlag = false;
    thisEFC.OneTimeFlagForEachEvapFluidCooler = false;
    thisEFC.MyEnvrnFlag = false;
    state->dataGlobal->BeginEnvrnFlag = true;
    thisEFC.WaterInletNodeNum = 1;
    thisEFC.WaterOutletNodeNum = 2;
    thisEFC.OutdoorAirInletNodeNum = 0;
    thisEFC.plantLoc.loopNum = 1;
    thisEFC.plantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    thisEFC.plantLoc.branchNum = 1;
    thisEFC.plantLoc.compNum = 1;
    PlantLocation pl;
    state->dataEnvrn->OutDryBulbTemp = 20.0;
    state->dataEnvrn->OutHumRat = 0.02;
    state->dataEnvrn->OutBaroPress = 101325.;
    state->dataEnvrn->OutWetBulbTemp = 8.0;
    state->dataLoopNodes->Node.allocate(2);
    state->dataLoopNodes->Node(thisEFC.WaterInletNodeNum).Temp = 20.0;
    state->dataLoopNodes->Node(1).Temp = 23.0;
    state->dataLoopNodes->Node(1).MassFlowRateRequest = 0.05;
    state->dataLoopNodes->Node(1).MassFlowRateMinAvail = 0.0;
    state->dataLoopNodes->Node(1).MassFlowRateMin = 0.0;
    state->dataLoopNodes->Node(1).MassFlowRateMax = 0.05;
    state->dataLoopNodes->Node(1).MassFlowRateMaxAvail = 0.05;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).FlowLock = DataPlant::FlowLock::Locked;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).MyLoad = 1.0;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).ON = false;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).CurOpSchemeType = DataPlant::OpScheme::Invalid;
    thisEFC.DesignWaterFlowRateWasAutoSized = false;
    thisEFC.LowSpeedAirFlowRateWasAutoSized = false;
    thisEFC.HighSpeedEvapFluidCoolerUAWasAutoSized = false;
    thisEFC.PerformanceInputMethod_Num = PIM::UFactor;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantFinalSizesOkayToReport = false;
    state->dataSize->SaveNumPlantComps = 0;
    thisEFC.DesignWaterFlowRate = 0.001;
    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).DeltaT = 5.0;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataSize->PlantSizData(1).ExitTemp = 20.0;

    // Now set the specific data for the actual test
    MaxLoad = 0.0;
    OptLoad = 0.0;
    MinLoad = 999.9;
    thisEFC.HighSpeedStandardDesignCapacity = 1.0;
    thisEFC.HeatRejectCapNomCapSizingRatio = 2.0;
    ExpectedMaxLoad = 20902.8677;
    ExpectedOptLoad = 10451.4338;
    ExpectedMinLoad = 0.0;

    // Call the routine to be tested and see if the fix is correct
    PlantLocation loc = PlantLocation(1, DataPlant::LoopSideLocation::Demand, 1, 1);
    thisEFC.onInitLoopEquip(*state, loc);
    thisEFC.getDesignCapacities(*state, pl, MaxLoad, MinLoad, OptLoad);
    EXPECT_NEAR(MaxLoad, ExpectedMaxLoad, 0.01);
    EXPECT_NEAR(MinLoad, ExpectedMinLoad, 0.01);
    EXPECT_NEAR(OptLoad, ExpectedOptLoad, 0.01);
}

} // namespace EnergyPlus
