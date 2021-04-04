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

// EnergyPlus::Boilers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::Boilers;
using namespace EnergyPlus::DataBranchAirLoopPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;

TEST_F(EnergyPlusFixture, Boiler_HotWaterSizingTest)
{
    // unit test for autosizing boiler nominal capacity in Boiler:HotWater
    state->dataBoilers->Boiler.allocate(1);
    // Hardsized Hot Water Boiler
    state->dataBoilers->Boiler(1).LoopNum = 1;
    state->dataBoilers->Boiler(1).SizFac = 1.2;
    state->dataBoilers->Boiler(1).NomCap = 40000.0;
    state->dataBoilers->Boiler(1).NomCapWasAutoSized = false;
    state->dataBoilers->Boiler(1).VolFlowRate = 1.0;
    state->dataBoilers->Boiler(1).VolFlowRateWasAutoSized = false;

    state->dataPlnt->PlantLoop.allocate(1);
    state->dataSize->PlantSizData.allocate(1);
    // Hot Water Loop
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataSize->PlantSizData(1).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(1).DeltaT = 10.0;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    // now call sizing routine
    state->dataBoilers->Boiler(1).SizeBoiler(*state);
    // see if boiler volume flow rate returned is hard-sized value
    EXPECT_DOUBLE_EQ(state->dataBoilers->Boiler(1).VolFlowRate, 1.0);
    // see if boiler nominal capacity returned is hard-sized value
    EXPECT_DOUBLE_EQ(state->dataBoilers->Boiler(1).NomCap, 40000.0);

    // Autosized Hot Water Boiler
    state->dataBoilers->Boiler(1).NomCapWasAutoSized = true;
    state->dataBoilers->Boiler(1).VolFlowRateWasAutoSized = true;
    state->dataBoilers->Boiler(1).NomCap = DataSizing::AutoSize;
    state->dataBoilers->Boiler(1).VolFlowRate = DataSizing::AutoSize;
    // now call sizing routine
    state->dataBoilers->Boiler(1).SizeBoiler(*state);
    // see if boiler volume flow rate returned is autosized value
    EXPECT_NEAR(state->dataBoilers->Boiler(1).VolFlowRate, 1.2, 0.000001);
    // see if boiler nominal capacity returned is autosized value
    EXPECT_NEAR(state->dataBoilers->Boiler(1).NomCap, 49376304.0, 1.0);
    // clear
    state->dataBoilers->Boiler.deallocate();
    state->dataSize->PlantSizData.deallocate();
    state->dataPlnt->PlantLoop.deallocate();
}
TEST_F(EnergyPlusFixture, Boiler_HotWaterAutoSizeTempTest)
{
    // unit test for checking hot water temperature for autosizing
    // boiler nominal capacity in Boiler:HotWater
    state->dataBoilers->Boiler.allocate(1);
    // Autosized Hot Water Boiler
    state->dataBoilers->Boiler(1).LoopNum = 1;
    state->dataBoilers->Boiler(1).SizFac = 1.2;
    state->dataBoilers->Boiler(1).NomCap = DataSizing::AutoSize;
    state->dataBoilers->Boiler(1).NomCapWasAutoSized = true;
    state->dataBoilers->Boiler(1).VolFlowRate = DataSizing::AutoSize;
    state->dataBoilers->Boiler(1).VolFlowRateWasAutoSized = true;

    state->dataPlnt->PlantLoop.allocate(1);
    state->dataSize->PlantSizData.allocate(1);
    // Hot Water Loop
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataSize->PlantSizData(1).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(1).DeltaT = 10.0;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // calculate nominal capacity at 60.0 C hot water temperature
    Real64 rho = FluidProperties::GetDensityGlycol(*state,
                                                   state->dataPlnt->PlantLoop(state->dataBoilers->Boiler(1).LoopNum).FluidName,
                                                   60.0,
                                                   state->dataPlnt->PlantLoop(state->dataBoilers->Boiler(1).LoopNum).FluidIndex,
                                                   "Boiler_HotWaterAutoSizeTempTest");
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(*state,
                                                       state->dataPlnt->PlantLoop(state->dataBoilers->Boiler(1).LoopNum).FluidName,
                                                       60.0,
                                                       state->dataPlnt->PlantLoop(state->dataBoilers->Boiler(1).LoopNum).FluidIndex,
                                                       "Boiler_HotWaterAutoSizeTempTest");

    Real64 NomCapBoilerExpected =
        rho * state->dataSize->PlantSizData(1).DesVolFlowRate * Cp * state->dataSize->PlantSizData(1).DeltaT * state->dataBoilers->Boiler(1).SizFac;

    // now call sizing routine
    state->dataBoilers->Boiler(1).SizeBoiler(*state);
    // see if boiler volume flow rate returned is autosized value
    EXPECT_DOUBLE_EQ(state->dataBoilers->Boiler(1).VolFlowRate, 1.2);
    // see if boiler nominal capacity returned is autosized value
    EXPECT_DOUBLE_EQ(state->dataBoilers->Boiler(1).NomCap, NomCapBoilerExpected);
}

// Cf: https://github.com/NREL/EnergyPlus/issues/6164
// This boiler has empty field for "Design Water Flow Rate", IDD now should make default='Autosize'
TEST_F(EnergyPlusFixture, Boiler_HotWater_BlankDesignWaterFlowRate)
{
    std::string const idf_objects = delimited_string({
        "Boiler:HotWater,",
        "  Boiler 1,                !- Name",
        "  NaturalGas,              !- Fuel Type",
        "  2344000,                 !- Nominal Capacity {W}",
        "  0.8,                     !- Nominal Thermal Efficiency",
        "  ,                        !- Efficiency Curve Temperature Evaluation Variable",
        "  ,                        !- Normalized Boiler Efficiency Curve Name",
        "  ,                        !- Design Water Flow Rate {m3/s}",
        "  ,                        !- Minimum Part Load Ratio",
        "  1,                       !- Maximum Part Load Ratio",
        "  1,                       !- Optimum Part Load Ratio",
        "  Node boiler 1 inlet,     !- Boiler Water Inlet Node Name",
        "  Node boiler 1 outlet,    !- Boiler Water Outlet Node Name",
        "  99.9,                    !- Water Outlet Upper Temperature Limit {C}",
        "  NotModulated,            !- Boiler Flow Mode",
        "  ,                        !- Parasitic Electric Load {W}",
        "  1;                       !- Sizing Factor",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetBoilerInput(*state);

    EXPECT_EQ(1, state->dataBoilers->numBoilers);
    EXPECT_EQ(AutoSize, state->dataBoilers->Boiler(1).VolFlowRate);

    // Additional tests for fuel type input
    EXPECT_EQ(state->dataBoilers->Boiler(1).BoilerFuelTypeForOutputVariable, "NaturalGas");
}

TEST_F(EnergyPlusFixture, Boiler_HotWater_BoilerEfficiency)
{

    bool RunFlag(true);
    Real64 MyLoad(1000000.0);

    state->dataPlnt->TotNumLoops = 2;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    Psychrometrics::InitializePsychRoutines(*state);

    std::string const idf_objects = delimited_string({
        "Boiler:HotWater,",
        "  Boiler 1,                !- Name",
        "  NaturalGas,              !- Fuel Type",
        "  Autosize,                !- Nominal Capacity {W}",
        "  0.8,                     !- Nominal Thermal Efficiency",
        "  LeavingBoiler,           !- Efficiency Curve Temperature Evaluation Variable",
        "  BoilerEfficiency,        !- Normalized Boiler Efficiency Curve Name",
        "  Autosize,                !- Design Water Flow Rate {m3/s}",
        "  0.0,                     !- Minimum Part Load Ratio",
        "  1.2,                     !- Maximum Part Load Ratio",
        "  1.0,                     !- Optimum Part Load Ratio",
        "  Node boiler 1 inlet,     !- Boiler Water Inlet Node Name",
        "  Node boiler 1 outlet,    !- Boiler Water Outlet Node Name",
        "  99.9,                    !- Water Outlet Upper Temperature Limit {C}",
        "  NotModulated,            !- Boiler Flow Mode",
        "  ,                        !- Parasitic Electric Load {W}",
        "  1;                       !- Sizing Factor",

        "Curve:Quadratic,",
        "  BoilerEfficiency,        !- Name",
        "  0.5887682,               !- Coefficient1 Constant",
        "  0.7888184,               !- Coefficient2 x",
        "  -0.3862498,              !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",
    });

    EXPECT_TRUE(process_idf(idf_objects, false));

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

    GetBoilerInput(*state);
    auto &thisBoiler = state->dataBoilers->Boiler(1);

    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "HotWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = thisBoiler.Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Boiler_Simple;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = thisBoiler.BoilerInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = thisBoiler.BoilerOutletNodeNum;

    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.1;
    state->dataSize->PlantSizData(1).DeltaT = 10;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    thisBoiler.InitBoiler(*state);
    thisBoiler.SizeBoiler(*state);

    // run through init again after sizing is complete to set mass flow rate and run calc function
    state->dataGlobal->BeginEnvrnFlag = true;
    thisBoiler.InitBoiler(*state);
    thisBoiler.CalcBoilerModel(*state, MyLoad, RunFlag, DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive);

    // check boiler part load ratio and the resultant boiler efficiency
    EXPECT_NEAR(thisBoiler.BoilerPLR, 0.24, 0.01);
    Real64 ExpectedBoilerEff = (0.5887682 + 0.7888184 * thisBoiler.BoilerPLR - 0.3862498 * pow(thisBoiler.BoilerPLR, 2)) * thisBoiler.NomEffic;
    EXPECT_NEAR(thisBoiler.BoilerEff, ExpectedBoilerEff, 0.01);
}
