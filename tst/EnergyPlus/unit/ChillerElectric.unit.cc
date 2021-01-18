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

// EnergyPlus::PlantChillers, Chiller:ConstantCOP Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::PlantChillers;

TEST_F(EnergyPlusFixture, ChillerElectric_WaterCooled_Autosize)
{

    state->dataPlnt->TotNumLoops = 4;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    std::string const idf_objects = delimited_string({
        "  Chiller:Electric,",
        "    Big Chiller,             !- Name",
        "    WaterCooled,             !- Condenser Type",
        "    100000.0,                !- Nominal Capacity {W}",
        "    4.75,                    !- Nominal COP {W/W}",
        "    Big Chiller Inlet Node,  !- Chilled Water Inlet Node Name",
        "    Big Chiller Outlet Node, !- Chilled Water Outlet Node Name",
        "    Big Cond Inlet Node,     !- Condenser Inlet Node Name",
        "    Big Cond Outlet Node,    !- Condenser Outlet Node Name",
        "    0.15,                    !- Minimum Part Load Ratio",
        "    1.0,                     !- Maximum Part Load Ratio",
        "    0.65,                    !- Optimum Part Load Ratio",
        "    29.44,                   !- Design Condenser Inlet Temperature {C}",
        "    2.682759,                !- Temperature Rise Coefficient",
        "    6.667,                   !- Design Chilled Water Outlet Temperature {C}",
        "    0.0011,                  !- Design Chilled Water Flow Rate {m3/s}",
        "    0.0011,                  !- Design Condenser Fluid Flow Rate {m3/s}",
        "    0.94483600,              !- Coefficient 1 of Capacity Ratio Curve",
        "    -.05700880,              !- Coefficient 2 of Capacity Ratio Curve",
        "    -.00185486,              !- Coefficient 3 of Capacity Ratio Curve",
        "    1.907846,                !- Coefficient 1 of Power Ratio Curve",
        "    -1.20498700,             !- Coefficient 2 of Power Ratio Curve",
        "    0.26346230,              !- Coefficient 3 of Power Ratio Curve",
        "    0.03303,                 !- Coefficient 1 of Full Load Ratio Curve",
        "    0.6852,                  !- Coefficient 2 of Full Load Ratio Curve",
        "    0.2818,                  !- Coefficient 3 of Full Load Ratio Curve",
        "    5,                       !- Chilled Water Outlet Temperature Lower Limit {C}",
        "    LeavingSetpointModulated;!- Chiller Flow Mode",
    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loop(state->dataPlnt->PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    ElectricChillerSpecs::getInput(*state);

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state->dataPlantChillers->ElectricChiller(1).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataPlantChillers->ElectricChiller(1).EvapInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state->dataPlantChillers->ElectricChiller(1).EvapOutletNodeNum;

    state->dataPlnt->PlantLoop(2).Name = "CondenserWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidName = "CondenserWater";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state->dataPlantChillers->ElectricChiller(1).Name;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataPlantChillers->ElectricChiller(1).CondInletNodeNum;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state->dataPlantChillers->ElectricChiller(1).CondOutletNodeNum;

    DataSizing::PlantSizData.allocate(2);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(1).DeltaT = 5.0;

    DataSizing::PlantSizData(2).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(2).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    bool RunFlag(true);
    Real64 MyLoad(-20000.0);

    Psychrometrics::InitializePsychRoutines();
    auto &thisChiller = state->dataPlantChillers->ElectricChiller(1);
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.size(*state);
    // run init again after sizing is complete to set mass flow rate
    state->dataGlobal->BeginEnvrnFlag = true;
    thisChiller.initialize(*state, RunFlag, MyLoad);
    // check hardsized chiller nominal capacity
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).NomCap, 100000.00);
    // check hardsized chiller evap water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRate, 0.0011);
    // check hardsized chiller cond water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).CondVolFlowRate, 0.0011);

    // Reset nom cap, Evap Vol Flow Rate and Cond Vol FLow Rate to autosize
    state->dataPlantChillers->ElectricChiller(1).NomCap = DataSizing::AutoSize;
    state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRate = DataSizing::AutoSize;
    state->dataPlantChillers->ElectricChiller(1).CondVolFlowRate = DataSizing::AutoSize;
    // Reset outosize flags
    state->dataPlantChillers->ElectricChiller(1).NomCapWasAutoSized = true;
    state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRateWasAutoSized = true;
    state->dataPlantChillers->ElectricChiller(1).CondVolFlowRateWasAutoSized = true;

    // do autosizing calc
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.size(*state);
    // check autocalculate chiller nominal capacity
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).NomCap, 20987.509055700004);
    // check autocalculate chiller evap water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRate, 0.0010000000000000000);
    // check autocalculate chiller cond water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).CondVolFlowRate, 0.0012208075356136608);
}

TEST_F(EnergyPlusFixture, ChillerElectric_WaterCooled_Simulate)
{

    state->dataPlnt->TotNumLoops = 4;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    DataHVACGlobals::TimeStepSys = 60;

    std::string const idf_objects = delimited_string({
        "  Chiller:Electric,",
        "    Big Chiller,             !- Name",
        "    WaterCooled,             !- Condenser Type",
        "    100000.0,                !- Nominal Capacity {W}",
        "    4.75,                    !- Nominal COP {W/W}",
        "    Big Chiller Inlet Node,  !- Chilled Water Inlet Node Name",
        "    Big Chiller Outlet Node, !- Chilled Water Outlet Node Name",
        "    Big Cond Inlet Node,     !- Condenser Inlet Node Name",
        "    Big Cond Outlet Node,    !- Condenser Outlet Node Name",
        "    0.15,                    !- Minimum Part Load Ratio",
        "    1.0,                     !- Maximum Part Load Ratio",
        "    0.65,                    !- Optimum Part Load Ratio",
        "    29.44,                   !- Design Condenser Inlet Temperature {C}",
        "    2.682759,                !- Temperature Rise Coefficient",
        "    6.667,                   !- Design Chilled Water Outlet Temperature {C}",
        "    0.0011,                  !- Design Chilled Water Flow Rate {m3/s}",
        "    0.0011,                  !- Design Condenser Fluid Flow Rate {m3/s}",
        "    0.94483600,              !- Coefficient 1 of Capacity Ratio Curve",
        "    -.05700880,              !- Coefficient 2 of Capacity Ratio Curve",
        "    -.00185486,              !- Coefficient 3 of Capacity Ratio Curve",
        "    1.907846,                !- Coefficient 1 of Power Ratio Curve",
        "    -1.20498700,             !- Coefficient 2 of Power Ratio Curve",
        "    0.26346230,              !- Coefficient 3 of Power Ratio Curve",
        "    0.03303,                 !- Coefficient 1 of Full Load Ratio Curve",
        "    0.6852,                  !- Coefficient 2 of Full Load Ratio Curve",
        "    0.2818,                  !- Coefficient 3 of Full Load Ratio Curve",
        "    5,                       !- Chilled Water Outlet Temperature Lower Limit {C}",
        "    LeavingSetpointModulated;!- Chiller Flow Mode",
    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loop(state->dataPlnt->PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    ElectricChillerSpecs::getInput(*state);

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state->dataPlantChillers->ElectricChiller(1).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataPlantChillers->ElectricChiller(1).EvapInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state->dataPlantChillers->ElectricChiller(1).EvapOutletNodeNum;

    state->dataPlnt->PlantLoop(2).Name = "CondenserWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidName = "CondenserWater";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state->dataPlantChillers->ElectricChiller(1).Name;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataPlantChillers->ElectricChiller(1).CondInletNodeNum;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state->dataPlantChillers->ElectricChiller(1).CondOutletNodeNum;

    DataSizing::PlantSizData.allocate(2);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(1).DeltaT = 5.0;

    DataSizing::PlantSizData(2).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(2).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    bool RunFlag(true);
    Real64 MyLoad(-20000.0);

    Psychrometrics::InitializePsychRoutines();
    auto &thisChiller = state->dataPlantChillers->ElectricChiller(1);
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.size(*state);
    // run init again after sizing is complete to set mass flow rate
    state->dataGlobal->BeginEnvrnFlag = true;
    thisChiller.initialize(*state, RunFlag, MyLoad);
    // check hardsized chiller nominal capacity
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).NomCap, 100000.00);
    // check hardsized chiller evap water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRate, 0.0011);
    // check hardsized chiller cond water vol flow rate
    EXPECT_DOUBLE_EQ(state->dataPlantChillers->ElectricChiller(1).CondVolFlowRate, 0.0011);

    // Reset nom cap, Evap Vol Flow Rate and Cond Vol FLow Rate to autosize
    state->dataPlantChillers->ElectricChiller(1).NomCap = DataSizing::AutoSize;
    state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRate = DataSizing::AutoSize;
    state->dataPlantChillers->ElectricChiller(1).CondVolFlowRate = DataSizing::AutoSize;
    // Reset autosize flags
    state->dataPlantChillers->ElectricChiller(1).NomCapWasAutoSized = true;
    state->dataPlantChillers->ElectricChiller(1).EvapVolFlowRateWasAutoSized = true;
    state->dataPlantChillers->ElectricChiller(1).CondVolFlowRateWasAutoSized = true;

    // Do autosizing calcs
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.size(*state);

    // simulate
    PlantLocation loc(1, 1, 1, 1);
    bool firstHVAC = true;
    Real64 curLoad = -10000.0;
    bool runFlag = true;

    thisChiller.simulate(*state, loc, firstHVAC, curLoad, runFlag);

    Real64 TestCOP = thisChiller.QEvaporator / thisChiller.Power;
    EXPECT_NEAR(TestCOP, thisChiller.ActualCOP, 1E-3);
}
