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

// EnergyPlus::BoilerSteam Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::BoilerSteam;
using namespace EnergyPlus::DataBranchAirLoopPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::Psychrometrics;

TEST_F(EnergyPlusFixture, BoilerSteam_GetInput)
{

    std::string const idf_objects = delimited_string({
        "  Boiler:Steam,                                                                                            ",
        "    Steam Boiler Plant Boiler,  !- Name                                                                    ",
        "    NaturalGas,                !- Fuel Type                                                                ",
        "    160000,                    !- Maximum Operating Pressure{ Pa }                                         ",
        "    0.8,                       !- Theoretical Efficiency                                                   ",
        "    115,                       !- Design Outlet Steam Temperature{ C }                                     ",
        "    autosize,                  !- Nominal Capacity{ W }                                                    ",
        "    0.00001,                   !- Minimum Part Load Ratio                                                  ",
        "    1.0,                       !- Maximum Part Load Ratio                                                  ",
        "    0.2,                       !- Optimum Part Load Ratio                                                  ",
        "    0.8,                       !- Coefficient 1 of Fuel Use Function of Part Load Ratio Curve              ",
        "    0.1,                       !- Coefficient 2 of Fuel Use Function of Part Load Ratio Curve              ",
        "    0.1,                       !- Coefficient 3 of Fuel Use Function of Part Load Ratio Curve              ",
        "    Steam Boiler Plant Boiler Inlet Node,  !- Water Inlet Node Name                                        ",
        "    Steam Boiler Plant Boiler Outlet Node;  !- Steam Outlet Node Name                                      ",
    });

    ASSERT_TRUE(process_idf(idf_objects, false));
    GetBoilerInput(state.dataSteamBoilers);
    auto &thisBoiler = state.dataSteamBoilers.Boiler(state.dataSteamBoilers.numBoilers);
    EXPECT_EQ(thisBoiler.Name, "STEAM BOILER PLANT BOILER");
    EXPECT_EQ(thisBoiler.FuelType, AssignResourceTypeNum("NATURALGAS"));
    EXPECT_EQ(thisBoiler.BoilerMaxOperPress, 160000);
    EXPECT_EQ(thisBoiler.NomEffic, 0.8);
    EXPECT_EQ(thisBoiler.TempUpLimitBoilerOut, 115);
    EXPECT_EQ(thisBoiler.NomCap, AutoSize);
    EXPECT_EQ(thisBoiler.MinPartLoadRat, 0.00001);
    EXPECT_EQ(thisBoiler.MaxPartLoadRat, 1.0);
    EXPECT_EQ(thisBoiler.OptPartLoadRat, 0.2);
    EXPECT_EQ(thisBoiler.FullLoadCoef(1), 0.8);
    EXPECT_EQ(thisBoiler.FullLoadCoef(2), 0.1);
    EXPECT_EQ(thisBoiler.FullLoadCoef(3), 0.1);
    EXPECT_EQ(thisBoiler.SizFac, 1.0);

    // Additional tests for fuel type input
    EXPECT_EQ(thisBoiler.BoilerFuelTypeForOutputVariable, "Gas");

}

TEST_F(EnergyPlusFixture, BoilerSteam_BoilerEfficiency)
{

    bool RunFlag(true);
    Real64 MyLoad(1000000.0);

    DataPlant::TotNumLoops = 2;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    Psychrometrics::InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "  Boiler:Steam,                                                                                            ",
        "    Steam Boiler Plant Boiler,  !- Name                                                                    ",
        "    NaturalGas,                !- Fuel Type                                                                ",
        "    160000,                    !- Maximum Operating Pressure{ Pa }                                         ",
        "    0.8,                       !- Theoretical Efficiency                                                   ",
        "    115,                       !- Design Outlet Steam Temperature{ C }                                     ",
        "    autosize,                  !- Nominal Capacity{ W }                                                    ",
        "    0.00001,                   !- Minimum Part Load Ratio                                                  ",
        "    1.0,                       !- Maximum Part Load Ratio                                                  ",
        "    0.2,                       !- Optimum Part Load Ratio                                                  ",
        "    0.8,                       !- Coefficient 1 of Fuel Use Function of Part Load Ratio Curve              ",
        "    0.1,                       !- Coefficient 2 of Fuel Use Function of Part Load Ratio Curve              ",
        "    0.1,                       !- Coefficient 3 of Fuel Use Function of Part Load Ratio Curve              ",
        "    Steam Boiler Plant Boiler Inlet Node,  !- Water Inlet Node Name                                        ",
        "    Steam Boiler Plant Boiler Outlet Node;  !- Steam Outlet Node Name                                      ",
    });

    EXPECT_TRUE(process_idf(idf_objects, false));

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

    GetBoilerInput(state.dataSteamBoilers);
    auto &thisBoiler = state.dataSteamBoilers.Boiler(state.dataSteamBoilers.numBoilers);

    DataPlant::PlantLoop(1).Name = "SteamLoop";
    DataPlant::PlantLoop(1).FluidName = "Steam";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(1).FluidName = "STEAM";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = thisBoiler.Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Boiler_Steam;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = thisBoiler.BoilerInletNodeNum;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = thisBoiler.BoilerOutletNodeNum;

    DataSizing::PlantSizData.allocate(1);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.1;
    DataSizing::PlantSizData(1).DeltaT = 10;

    DataPlant::PlantFirstSizesOkayToFinalize = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFinalSizesOkayToReport = true;

    DataGlobals::BeginEnvrnFlag = true;
    thisBoiler.initialize(state.dataBranchInputManager);
    thisBoiler.calculate(MyLoad, RunFlag, DataBranchAirLoopPlant::ControlType_SeriesActive);

    // check boiler fuel used and the resultant boiler efficiency
    EXPECT_EQ(thisBoiler.BoilerLoad, 1000000);
    EXPECT_NEAR(thisBoiler.FuelUsed, 1562498, 1.0);
    Real64 ExpectedBoilerEff = thisBoiler.BoilerLoad / thisBoiler.FuelUsed;
    EXPECT_NEAR(thisBoiler.BoilerEff, ExpectedBoilerEff,0.01);
}
