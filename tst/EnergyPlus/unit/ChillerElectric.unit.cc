// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::NodeInputManager;
using namespace EnergyPlus::PlantChillers;

TEST_F(EnergyPlusFixture, ChillerElectric_WaterCooled_Autosize)
{

    DataPlant::TotNumLoops = 4;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

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

    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    GetElectricChillerInput();

    DataPlant::PlantLoop(1).Name = "ChilledWaterLoop";
    DataPlant::PlantLoop(1).FluidName = "ChilledWater";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = ElectricChiller(1).Base.Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = ElectricChiller(1).Base.EvapInletNodeNum;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = ElectricChiller(1).Base.EvapOutletNodeNum;

    DataPlant::PlantLoop(2).Name = "CondenserWaterLoop";
    DataPlant::PlantLoop(2).FluidName = "CondenserWater";
    DataPlant::PlantLoop(2).FluidIndex = 1;
    DataPlant::PlantLoop(2).PlantSizNum = 2;
    DataPlant::PlantLoop(2).FluidName = "WATER";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = ElectricChiller(1).Base.Name;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = ElectricChiller(1).Base.CondInletNodeNum;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = ElectricChiller(1).Base.CondOutletNodeNum;

    DataSizing::PlantSizData.allocate(2);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(1).DeltaT = 5.0;

    DataSizing::PlantSizData(2).DesVolFlowRate = 0.001;
    DataSizing::PlantSizData(2).DeltaT = 5.0;

    DataPlant::PlantFirstSizesOkayToFinalize = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFinalSizesOkayToReport = true;

    bool RunFlag(true);
    Real64 MyLoad(-20000.0);

    Psychrometrics::InitializePsychRoutines();
    InitElectricChiller(1, RunFlag, MyLoad);
    SizeElectricChiller(1);
    // run init again after sizing is complete to set mass flow rate
    DataGlobals::BeginEnvrnFlag = true;
    InitElectricChiller(1, RunFlag, MyLoad);
    // check hardsized chiller nominal capacity
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.NomCap, 100000.00);
    // check hardsized chiller evap water vol flow rate
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.EvapVolFlowRate, 0.0011);
    // check hardsized chiller cond water vol flow rate
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.CondVolFlowRate, 0.0011);

    // Reset nom cap, Evap Vol Flow Rate and Cond Vol FLow Rate to autosize
    ElectricChiller(1).Base.NomCap = DataSizing::AutoSize;
    ElectricChiller(1).Base.EvapVolFlowRate = DataSizing::AutoSize;
    ElectricChiller(1).Base.CondVolFlowRate = DataSizing::AutoSize;
    // Reset outosize flags
    ElectricChiller(1).Base.NomCapWasAutoSized = true;
    ElectricChiller(1).Base.EvapVolFlowRateWasAutoSized = true;
    ElectricChiller(1).Base.CondVolFlowRateWasAutoSized = true;

    // do autosizing calc
    InitElectricChiller(1, RunFlag, MyLoad);
    SizeElectricChiller(1);
    // check autocalculate chiller nominal capacity
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.NomCap, 20987.509055700004);
    // check autocalculate chiller evap water vol flow rate
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.EvapVolFlowRate, 0.0010000000000000000);
    // check autocalculate chiller cond water vol flow rate
    EXPECT_DOUBLE_EQ(ElectricChiller(1).Base.CondVolFlowRate, 0.0012208075356136608);
}
