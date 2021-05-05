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

// EnergyPlus::HeatBalFiniteDiffManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalFiniteDiffManager;
using namespace EnergyPlus::HeatBalanceManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalFiniteDiffManager_CalcNodeHeatFluxTest)
{
    auto &SurfaceFD = state->dataHeatBalFiniteDiffMgr->SurfaceFD;
    int const numNodes(4);
    int nodeNum(0);
    SurfaceFD.allocate(1);
    int const SurfNum(1);
    SurfaceFD(SurfNum).QDreport.allocate(numNodes + 1);
    SurfaceFD(SurfNum).TDpriortimestep.allocate(numNodes + 1);
    SurfaceFD(SurfNum).TDT.allocate(numNodes + 1);
    SurfaceFD(SurfNum).CpDelXRhoS1.allocate(numNodes + 1);
    SurfaceFD(SurfNum).CpDelXRhoS2.allocate(numNodes + 1);
    state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 600.0;

    Real64 expectedResult1(0.0);
    Real64 expectedResult2(0.0);
    Real64 expectedResult3(0.0);
    Real64 expectedResult4(0.0);
    Real64 expectedResult5(0.0);

    // Steady-state case
    state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) = 100.0;
    nodeNum = 1;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 20.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 20.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult1 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    nodeNum = 2;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 22.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 22.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult2 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    nodeNum = 3;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 23.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 23.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult3 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    nodeNum = 4;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 26.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 26.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult4 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    nodeNum = 5;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 27.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 27.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult5 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    CalcNodeHeatFlux(*state, SurfNum, numNodes);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(1), expectedResult1, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(2), expectedResult2, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(3), expectedResult3, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(4), expectedResult4, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(5), expectedResult5, 0.0001);

    // Reset
    SurfaceFD(SurfNum).QDreport = 0.0;
    expectedResult1 = 0.0;
    expectedResult2 = 0.0;
    expectedResult3 = 0.0;
    expectedResult4 = 0.0;
    expectedResult5 = 0.0;

    // Unsteady-state case
    state->dataGlobal->TimeStepZoneSec = 600.0;
    state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) = -200.0;

    nodeNum = 5;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 27.5;
    SurfaceFD(SurfNum).TDT(nodeNum) = 27.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 0.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 0.0;
    expectedResult5 = state->dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum);

    nodeNum = 4;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 26.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 26.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 0.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult4 = expectedResult5; // r-layer with zero heat capacity, so flux passes through

    nodeNum = 3;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 23.0;
    SurfaceFD(SurfNum).TDT(nodeNum) = 23.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult3 = expectedResult4; // no change in temperature at nodes 4 and 3, so flux passes through

    nodeNum = 2;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 22.2;
    SurfaceFD(SurfNum).TDT(nodeNum) = 22.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult2 = expectedResult3 + (SurfaceFD(SurfNum).TDT(nodeNum) - SurfaceFD(SurfNum).TDpriortimestep(nodeNum)) *
                                            SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) / state->dataGlobal->TimeStepZoneSec;

    nodeNum = 1;
    SurfaceFD(SurfNum).TDpriortimestep(nodeNum) = 20.1;
    SurfaceFD(SurfNum).TDT(nodeNum) = 20.0;
    SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum) = 1000.0;
    SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) = 2000.0;
    expectedResult1 = expectedResult2 + (SurfaceFD(SurfNum).TDT(nodeNum + 1) - SurfaceFD(SurfNum).TDpriortimestep(nodeNum + 1)) *
                                            SurfaceFD(SurfNum).CpDelXRhoS1(nodeNum + 1) / state->dataGlobal->TimeStepZoneSec;
    expectedResult1 = expectedResult1 + (SurfaceFD(SurfNum).TDT(nodeNum) - SurfaceFD(SurfNum).TDpriortimestep(nodeNum)) *
                                            SurfaceFD(SurfNum).CpDelXRhoS2(nodeNum) / state->dataGlobal->TimeStepZoneSec;

    CalcNodeHeatFlux(*state, SurfNum, numNodes);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(1), expectedResult1, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(2), expectedResult2, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(3), expectedResult3, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(4), expectedResult4, 0.0001);
    EXPECT_NEAR(SurfaceFD(SurfNum).QDreport(5), expectedResult5, 0.0001);
}

TEST_F(EnergyPlusFixture, HeatBalFiniteDiffManager_adjustPropertiesForPhaseChange)
{
    // create a single PCM object in the input and process it
    std::string const idf_objects = delimited_string({"  MaterialProperty:PhaseChangeHysteresis,",
                                                      "    PCMNAME,   !- Name",
                                                      "    10000,                   !- Latent Heat during the Entire Phase Change Process {J/kg}",
                                                      "    1.5,                     !- Liquid State Thermal Conductivity {W/m-K}",
                                                      "    2200,                    !- Liquid State Density {kg/m3}",
                                                      "    2000,                    !- Liquid State Specific Heat {J/kg-K}",
                                                      "    1,                       !- High Temperature Difference of Melting Curve {deltaC}",
                                                      "    20,                      !- Peak Melting Temperature {C}",
                                                      "    1,                       !- Low Temperature Difference of Melting Curve {deltaC}",
                                                      "    1.8,                     !- Solid State Thermal Conductivity {W/m-K}",
                                                      "    2300,                    !- Solid State Density {kg/m3}",
                                                      "    2000,                    !- Solid State Specific Heat {J/kg-K}",
                                                      "    1,                       !- High Temperature Difference of Freezing Curve {deltaC}",
                                                      "    23,                      !- Peak Freezing Temperature {C}",
                                                      "    1;                       !- Low Temperature Difference of Freezing Curve {deltaC}"});
    ASSERT_TRUE(process_idf(idf_objects, false));

    // allocate a finite difference surface object and needed member variables
    int const surfaceIndex = 1;
    int const finiteDiffLayerIndex = 1;
    auto &SurfaceFD = state->dataHeatBalFiniteDiffMgr->SurfaceFD;
    SurfaceFD.allocate(1);
    SurfaceFD(surfaceIndex).PhaseChangeTemperatureReverse.allocate(1);
    SurfaceFD(surfaceIndex).PhaseChangeTemperatureReverse(finiteDiffLayerIndex) = 20.0;
    SurfaceFD(surfaceIndex).PhaseChangeState.allocate(1);
    SurfaceFD(surfaceIndex).PhaseChangeState(finiteDiffLayerIndex) = HysteresisPhaseChange::PhaseChangeStates::LIQUID;
    SurfaceFD(surfaceIndex).PhaseChangeStateOld.allocate(1);
    SurfaceFD(surfaceIndex).PhaseChangeStateOld(finiteDiffLayerIndex) = HysteresisPhaseChange::PhaseChangeStates::MELTING;

    // create a materials data object and assign the phase change variable based on above IDF processing
    Material::MaterialProperties material;
    material.phaseChange = HysteresisPhaseChange::HysteresisPhaseChange::factory(*state, "PCMNAME");

    // create local variables to calculate and call the new worker function
    Real64 newSpecificHeat, newDensity, newThermalConductivity;
    adjustPropertiesForPhaseChange(
        *state, finiteDiffLayerIndex, surfaceIndex, material, 20.0, 20.1, newSpecificHeat, newDensity, newThermalConductivity);

    // check the values are correct
    EXPECT_NEAR(10187.3, newSpecificHeat, 0.1);
    EXPECT_NEAR(2250, newDensity, 0.1);
    EXPECT_NEAR(1.65, newThermalConductivity, 0.1);

    // deallocate
    SurfaceFD.deallocate();
}

TEST_F(EnergyPlusFixture, DISABLED_HeatBalFiniteDiffManager_skipNotUsedConstructionAndAirLayer)
{
    bool ErrorsFound(false);
    // create three construction objects with one object not in use and another object assigned to surfaces, and one object as air wall.
    std::string const idf_objects = delimited_string({
        "Material,",
        "   MAT - CC05 4 HW CONCRETE, !- Name",
        "   Rough, !- Roughness",
        "   0.1016, !- Thickness{ m }",
        "   1.311, !- Conductivity{ W / m - K }",
        "   2240, !- Density{ kg / m3 }",
        "   836.800000000001, !- Specific Heat{ J / kg - K }",
        "   0.9, !- Thermal Absorptance",
        "   0.85, !- Solar Absorptance",
        "   0.85;                    !- Visible Absorptance",
        "Material:AirGap,",
        "   F05 Ceiling air space resistance, !- Name",
        "   0.18;                    !- Thermal Resistance{ m2 - K / W }",
        "Material:NoMass,",
        "   CP02 CARPET PAD, !- Name",
        "   Smooth, !- Roughness",
        "   0.1, !- Thermal Resistance{ m2 - K / W }",
        "   0.9, !- Thermal Absorptance",
        "   0.8, !- Solar Absorptance",
        "   0.8;                     !- Visible Absorptance",

        "Material,",
        "   F16 Acoustic tile, !- Name",
        "   MediumSmooth, !- Roughness",
        "   0.0191, !- Thickness{ m }",
        "   0.06, !- Conductivity{ W / m - K }",
        "   368, !- Density{ kg / m3 }",
        "   590.000000000002, !- Specific Heat{ J / kg - K }",
        "   0.9, !- Thermal Absorptance",
        "   0.3, !- Solar Absorptance",
        "   0.3;                     !- Visible Absorptance",

        "Material,",
        "   M11 100mm lightweight concrete, !- Name",
        "   MediumRough, !- Roughness",
        "   0.1016, !- Thickness{ m }",
        "   0.53, !- Conductivity{ W / m - K }",
        "   1280, !- Density{ kg / m3 }",
        "   840.000000000002, !- Specific Heat{ J / kg - K }",
        "   0.9, !- Thermal Absorptance",
        "   0.5, !- Solar Absorptance",
        "   0.5;                     !- Visible Absorptance",

        "Construction,",
        "  ExtSlabCarpet 4in ClimateZone 1 - 8, !- Name",
        "  MAT - CC05 4 HW CONCRETE, !- Outside Layer",
        "  CP02 CARPET PAD;         !- Layer 2",
        "Construction,",
        "   Interior Floor, !- Name",
        "   F16 Acoustic tile, !- Outside Layer",
        "   F05 Ceiling air space resistance, !- Layer 2",
        "   M11 100mm lightweight concrete;  !- Layer 3",
        "Construction:AirBoundary,",
        "   Air Wall_ConstructionAirBoundary,  !- Name",
        "   None,                    !- Air Exchange Method",
        "   0;                       !- Simple Mixing Air Changes per Hour {1 / hr}",
        "Output:Constructions,",
        "Constructions;",
        "Output:Constructions,",
        "Materials;",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ErrorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);                                // expect no errors

    ErrorsFound = false;
    HeatBalanceManager::GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);                                 // expect no errors

    // allocate properties for construction objects when it is used or not for building surfaces in the model

    state->dataConstruction->Construct(1).IsUsed = false;
    state->dataConstruction->Construct(2).IsUsed = true;
    state->dataConstruction->Construct(3).IsUsed = true;

    // call the function for initialization of finite difference calculation
    InitialInitHeatBalFiniteDiff(*state);
    auto &ConstructFD = state->dataHeatBalFiniteDiffMgr->ConstructFD;
    // check the values are correct
    EXPECT_EQ(0, ConstructFD(1).Name.size());
    EXPECT_EQ(3, ConstructFD(2).Name.size());
    EXPECT_EQ(0, ConstructFD(3).Name.size());
    EXPECT_EQ("F16 ACOUSTIC TILE", ConstructFD(2).Name(1));
    EXPECT_EQ("F05 CEILING AIR SPACE RESISTANCE", ConstructFD(2).Name(2));
    EXPECT_EQ("M11 100MM LIGHTWEIGHT CONCRETE", ConstructFD(2).Name(3));

    // deallocate
    ConstructFD.deallocate();
}

} // namespace EnergyPlus
