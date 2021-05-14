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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/WindowManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WindowManager;

TEST_F(EnergyPlusFixture, WindowFrameTest)
{

    state->dataIPShortCut->lAlphaFieldBlanks = true;

    std::string const idf_objects =
        delimited_string({"Material,",
                          "  Concrete Block,          !- Name",
                          "  MediumRough,             !- Roughness",
                          "  0.1014984,               !- Thickness {m}",
                          "  0.3805070,               !- Conductivity {W/m-K}",
                          "  608.7016,                !- Density {kg/m3}",
                          "  836.8000;                !- Specific Heat {J/kg-K}",
                          "Construction,",
                          "  WallConstruction,        !- Name",
                          "  Concrete Block;          !- Outside Layer",
                          "WindowMaterial:SimpleGlazingSystem,",
                          "  WindowMaterial,          !- Name",
                          "  5.778,                   !- U-Factor {W/m2-K}",
                          "  0.819,                   !- Solar Heat Gain Coefficient",
                          "  0.881;                   !- Visible Transmittance",
                          "Construction,",
                          "  WindowConstruction,      !- Name",
                          "  WindowMaterial;          !- Outside Layer",
                          "WindowProperty:FrameAndDivider,",
                          "  WindowFrame,             !- Name",
                          "  0.05,                    !- Frame Width {m}",
                          "  0.00,                    !- Frame Outside Projection {m}",
                          "  0.00,                    !- Frame Inside Projection {m}",
                          "  5.0,                     !- Frame Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Frame Solar Absorptance",
                          "  0.8,                     !- Frame Visible Absorptance",
                          "  0.9,                     !- Frame Thermal Hemispherical Emissivity",
                          "  DividedLite,             !- Divider Type",
                          "  0.02,                    !- Divider Width {m}",
                          "  2,                       !- Number of Horizontal Dividers",
                          "  2,                       !- Number of Vertical Dividers",
                          "  0.00,                    !- Divider Outside Projection {m}",
                          "  0.00,                    !- Divider Inside Projection {m}",
                          "  5.0,                     !- Divider Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Divider Solar Absorptance",
                          "  0.8,                     !- Divider Visible Absorptance",
                          "  0.9;                     !- Divider Thermal Hemispherical Emissivity",
                          "FenestrationSurface:Detailed,",
                          "  FenestrationSurface,     !- Name",
                          "  Window,                  !- Surface Type",
                          "  WindowConstruction,      !- Construction Name",
                          "  Wall,                    !- Building Surface Name",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  0.5000000,               !- View Factor to Ground",
                          "  WindowFrame,             !- Frame and Divider Name",
                          "  1.0,                     !- Multiplier",
                          "  4,                       !- Number of Vertices",
                          "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Wall,                    !- Name",
                          "  Wall,                    !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  SunExposed,              !- Sun Exposure",
                          "  WindExposed,             !- Wind Exposure",
                          "  0.5000000,               !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Floor,                   !- Name",
                          "  Floor,                   !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  NoSun,                   !- Sun Exposure",
                          "  NoWind,                  !- Wind Exposure",
                          "  1.0,                     !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                          "Zone,",
                          "  Zone,                    !- Name",
                          "  0,                       !- Direction of Relative North {deg}",
                          "  6.000000,                !- X Origin {m}",
                          "  6.000000,                !- Y Origin {m}",
                          "  0,                       !- Z Origin {m}",
                          "  1,                       !- Type",
                          "  1,                       !- Multiplier",
                          "  autocalculate,           !- Ceiling Height {m}",
                          "  autocalculate;           !- Volume {m3}"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataHeatBal->ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::SetPreConstructionInputParameters(*state);

    Psychrometrics::InitializePsychRoutines(*state);

    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->TimeStepZone = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataEnvrn->OutBaroPress = 100000;

    state->dataHeatBalFanSys->ZTAV.allocate(1);
    state->dataHeatBalFanSys->ZT.allocate(1);
    state->dataHeatBal->ZoneMRT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(1);

    state->dataHeatBalFanSys->ZT(1) = 0.0;
    state->dataHeatBalFanSys->ZTAV(1) = 0.0;
    state->dataHeatBal->ZoneMRT(1) = 0.0;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.0;

    HeatBalanceManager::ManageHeatBalance(*state);

    // This test will emulate NFRC 100 U-factor test
    int winNum;

    for (size_t i = 1; i <= state->dataSurface->Surface.size(); ++i) {
        if (state->dataSurface->Surface(i).Class == DataSurfaces::SurfaceClass::Window) {
            winNum = i;
        }
    }

    int cNum;

    for (size_t i = 1; i <= state->dataConstruction->Construct.size(); ++i) {
        if (state->dataConstruction->Construct(i).TypeIsWindow) {
            cNum = i;
        }
    }

    Real64 T_in = 21.0;
    Real64 T_out = -18.0;
    Real64 I_s = 0.0;
    Real64 v_ws = 5.5;

    // Overrides for testing
    state->dataHeatBal->CosIncAng.dimension(1, 1, 3, 1.0);
    state->dataHeatBal->SunlitFrac.dimension(1, 1, 3, 1.0);
    state->dataHeatBal->SunlitFracWithoutReveal.dimension(1, 1, 3, 1.0);

    state->dataSurface->SurfOutDryBulbTemp(winNum) = T_out;
    state->dataHeatBal->SurfTempEffBulkAir(winNum) = T_in;
    state->dataSurface->SurfWinIRfromParentZone(winNum) = DataGlobalConstants::StefanBoltzmann * std::pow(T_in + DataGlobalConstants::KelvinConv, 4);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.dimension(1, 0.01);
    state->dataHeatBalFanSys->ZoneAirHumRat.dimension(1, 0.01);
    state->dataHeatBalFanSys->MAT.dimension(1, T_in);

    // initial guess temperatures
    int numTemps = 2 + 2 * state->dataConstruction->Construct(cNum).TotGlassLayers;
    Real64 inSurfTemp = T_in - (1.0 / (numTemps - 1)) * (T_in - T_out);
    Real64 outSurfTemp = T_out + (1.0 / (numTemps - 1)) * (T_in - T_out);

    Real64 h_exterior_f = 4 + v_ws * 4;
    Real64 h_exterior;

    state->dataEnvrn->BeamSolarRad = I_s;

    if (I_s > 0.0) {
        state->dataEnvrn->SunIsUp = true;
    }

    HeatBalanceSurfaceManager::InitSolarHeatGains(*state);
    SolarShading::CalcInteriorSolarDistribution(*state);

    // Calculate heat balance (iteratively solve for surface temperatures)
    Real64 outSurfTempPrev = outSurfTemp;
    Real64 inSurfTempPrev = inSurfTemp;

    Real64 outSurfTempDiff;
    Real64 inSurfTempDiff;

    int maxIterations = 20;
    Real64 tolerance = 0.1; // deg C

    // Save tilt information for natural convection calculations
    Real64 tiltSave = state->dataSurface->Surface(winNum).Tilt;

    for (int i = 0; i < maxIterations; i++) {

        // Use complementary angle for exterior natural convection calculations
        state->dataSurface->Surface(1).Tilt = 180 - tiltSave;
        state->dataSurface->Surface(1).CosTilt = cos(state->dataSurface->Surface(winNum).Tilt * DataGlobalConstants::Pi / 180);
        state->dataSurface->Surface(1).SinTilt = sin(state->dataSurface->Surface(winNum).Tilt * DataGlobalConstants::Pi / 180);
        ConvectionCoefficients::CalcISO15099WindowIntConvCoeff(
            *state,
            winNum,
            outSurfTemp,
            T_out); // This subroutine sets the global HConvIn( 1 ) variable. We will use it to set the exterior natural convection.
        h_exterior = h_exterior_f + state->dataHeatBal->HConvIn(winNum); // add natural convection

        // revert tilt for interior natural convection calculations
        state->dataSurface->Surface(1).Tilt = tiltSave;
        state->dataSurface->Surface(1).CosTilt = cos(tiltSave * DataGlobalConstants::Pi / 180);
        state->dataSurface->Surface(1).SinTilt = sin(tiltSave * DataGlobalConstants::Pi / 180);
        ConvectionCoefficients::CalcISO15099WindowIntConvCoeff(
            *state,
            winNum,
            inSurfTemp,
            T_in); // This time it's actually being used as intended. HConvIn( 1 ) is referenced from the actual heat balance calculation.

        WindowManager::CalcWindowHeatBalance(*state, winNum, h_exterior, inSurfTemp, outSurfTemp);

        outSurfTempDiff = std::fabs(outSurfTemp - outSurfTempPrev);
        inSurfTempDiff = std::fabs(inSurfTemp - inSurfTempPrev);

        if ((outSurfTempDiff < tolerance) && (inSurfTempDiff < tolerance)) {
            break;
        }

        outSurfTempPrev = outSurfTemp;
        inSurfTempPrev = inSurfTemp;
    }

    EXPECT_GT(state->dataSurface->SurfWinHeatLossRep(winNum), state->dataSurface->SurfWinHeatTransfer(winNum));
}

TEST_F(EnergyPlusFixture, WindowManager_TransAndReflAtPhi)
{

    Real64 cs = 0.86603; // Cosine of incidence angle
    Real64 tf0 = 0.8980; // Transmittance at zero incidence angle
    Real64 rf0 = 0.0810; // Front reflectance at zero incidence angle
    Real64 rb0 = 0.0810; // Back reflectance at zero incidence angle

    Real64 tfp = 0.; // Transmittance at cs
    Real64 rfp = 0.; // Front reflectance at cs
    Real64 rbp = 0.; // Back reflectance at cs

    bool SimpleGlazingSystem = false; // .TRUE. if simple block model being used
    Real64 SimpleGlazingSHGC = 0.;    // SHGC value to use in alternate model for simple glazing system
    Real64 SimpleGlazingU = 0.;       // U-factor value to use in alternate model for simple glazing system

    TransAndReflAtPhi(cs, tf0, rf0, rb0, tfp, rfp, rbp, SimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU);

    EXPECT_NEAR(tfp, 0.89455, 0.0001);
    EXPECT_NEAR(rfp, 0.08323, 0.0001);
    EXPECT_NEAR(rbp, 0.08323, 0.0001);

    tf0 = 0.25; // Transmittance at zero incidence angle
    rf0 = 0.55; // Front reflectance at zero incidence angle
    rb0 = 0.55; // Back reflectance at zero incidence angle

    tfp = 0.; // Transmittance at cs
    rfp = 0.; // Front reflectance at cs
    rbp = 0.; // Back reflectance at cs

    SimpleGlazingSystem = true; // .TRUE. if simple block model being used
    SimpleGlazingSHGC = 0.335;  // SHGC value to use in alternate model for simple glazing system
    SimpleGlazingU = 1.704;     // U-factor value to use in alternate model for simple glazing system

    for (Real64 theta = 0.0; theta <= DataGlobalConstants::PiOvr2; theta += DataGlobalConstants::PiOvr2 / 10.0) {
        cs = std::cos(theta); // Cosine of incidence angle
        TransAndReflAtPhi(cs, tf0, rf0, rb0, tfp, rfp, rbp, SimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU);
        Real64 afp = 1. - tfp - rfp;

        EXPECT_GE(afp, 0.00);
    }
}

TEST_F(EnergyPlusFixture, WindowManager_RefAirTempTest)
{
    // GitHub issue 6037
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Material,",
                          "  Concrete Block,          !- Name",
                          "  MediumRough,             !- Roughness",
                          "  0.1014984,               !- Thickness {m}",
                          "  0.3805070,               !- Conductivity {W/m-K}",
                          "  608.7016,                !- Density {kg/m3}",
                          "  836.8000;                !- Specific Heat {J/kg-K}",
                          "Construction,",
                          "  WallConstruction,        !- Name",
                          "  Concrete Block;          !- Outside Layer",
                          "WindowMaterial:SimpleGlazingSystem,",
                          "  WindowMaterial,          !- Name",
                          "  5.778,                   !- U-Factor {W/m2-K}",
                          "  0.819,                   !- Solar Heat Gain Coefficient",
                          "  0.881;                   !- Visible Transmittance",
                          "Construction,",
                          "  WindowConstruction,      !- Name",
                          "  WindowMaterial;          !- Outside Layer",
                          "WindowProperty:FrameAndDivider,",
                          "  WindowFrame,             !- Name",
                          "  0.05,                    !- Frame Width {m}",
                          "  0.00,                    !- Frame Outside Projection {m}",
                          "  0.00,                    !- Frame Inside Projection {m}",
                          "  5.0,                     !- Frame Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Frame Solar Absorptance",
                          "  0.8,                     !- Frame Visible Absorptance",
                          "  0.9,                     !- Frame Thermal Hemispherical Emissivity",
                          "  DividedLite,             !- Divider Type",
                          "  0.02,                    !- Divider Width {m}",
                          "  2,                       !- Number of Horizontal Dividers",
                          "  2,                       !- Number of Vertical Dividers",
                          "  0.00,                    !- Divider Outside Projection {m}",
                          "  0.00,                    !- Divider Inside Projection {m}",
                          "  5.0,                     !- Divider Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Divider Solar Absorptance",
                          "  0.8,                     !- Divider Visible Absorptance",
                          "  0.9;                     !- Divider Thermal Hemispherical Emissivity",
                          "FenestrationSurface:Detailed,",
                          "  FenestrationSurface,     !- Name",
                          "  Window,                  !- Surface Type",
                          "  WindowConstruction,      !- Construction Name",
                          "  Wall,                    !- Building Surface Name",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  0.5000000,               !- View Factor to Ground",
                          "  WindowFrame,             !- Frame and Divider Name",
                          "  1.0,                     !- Multiplier",
                          "  4,                       !- Number of Vertices",
                          "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Wall,                    !- Name",
                          "  Wall,                    !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  SunExposed,              !- Sun Exposure",
                          "  WindExposed,             !- Wind Exposure",
                          "  0.5000000,               !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Floor,                   !- Name",
                          "  Floor,                   !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  NoSun,                   !- Sun Exposure",
                          "  NoWind,                  !- Wind Exposure",
                          "  1.0,                     !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                          "Zone,",
                          "  Zone,                    !- Name",
                          "  0,                       !- Direction of Relative North {deg}",
                          "  6.000000,                !- X Origin {m}",
                          "  6.000000,                !- Y Origin {m}",
                          "  0,                       !- Z Origin {m}",
                          "  1,                       !- Type",
                          "  1,                       !- Multiplier",
                          "  autocalculate,           !- Ceiling Height {m}",
                          "  autocalculate;           !- Volume {m3}"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataHeatBal->ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    HeatBalanceManager::GetFrameAndDividerData(*state, ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    HeatBalanceManager::GetBuildingData(*state, ErrorsFound);

    Psychrometrics::InitializePsychRoutines(*state);

    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->TimeStepZone = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataEnvrn->OutBaroPress = 100000;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataHeatBal->Zone(1).IsControlled = true;
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

    state->dataLoopNodes->Node.allocate(4);
    state->dataHeatBal->SurfTempEffBulkAir.allocate(3);
    state->dataHeatBalSurf->TempSurfInTmp.allocate(3);

    int surfNum1 = UtilityRoutines::FindItemInList("WALL", state->dataSurface->Surface);
    int surfNum2 = UtilityRoutines::FindItemInList("FENESTRATIONSURFACE", state->dataSurface->Surface);
    int surfNum3 = UtilityRoutines::FindItemInList("FLOOR", state->dataSurface->Surface);

    state->dataSurface->Surface(surfNum1).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum2).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum3).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum1).Area = 10.0;
    state->dataSurface->Surface(surfNum2).Area = 10.0;
    state->dataSurface->Surface(surfNum3).Area = 10.0;
    state->dataSurface->Surface(surfNum1).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum2).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum3).SolarEnclIndex = 1;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum1) = 15.0;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum2) = 20.0;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum3) = 25.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum1) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum2) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum3) = 10.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBal->HConvIn.allocate(3);
    state->dataHeatBal->HConvIn(surfNum1) = 0.5;
    state->dataHeatBal->HConvIn(surfNum2) = 0.5;
    state->dataHeatBal->HConvIn(surfNum3) = 0.5;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;

    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->QHTRadSysSurf.allocate(3);
    state->dataHeatBalFanSys->QHWBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QSteamBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QElecBaseboardSurf.allocate(3);
    state->dataHeatBal->SurfWinQRadSWwinAbs.allocate(3, 1);
    state->dataHeatBal->SurfQRadThermInAbs.allocate(3);
    state->dataHeatBal->SurfQRadSWOutIncident.allocate(3);
    state->dataSurface->SurfWinTransSolar.allocate(3);
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);
    state->dataSurface->SurfWinHeatGain.allocate(3);
    state->dataSurface->SurfWinHeatTransfer.allocate(3);
    state->dataSurface->SurfWinGainConvGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRepEnergy.allocate(3);
    state->dataHeatBal->EnclSolQSWRad.allocate(1);
    state->dataSurface->SurfWinLossSWZoneToOutWinRep.allocate(3);
    state->dataSurface->SurfWinSysSolTransmittance.allocate(3);
    state->dataSurface->SurfWinSysSolAbsorptance.allocate(3);
    state->dataSurface->SurfWinSysSolReflectance.allocate(3);
    state->dataSurface->InsideGlassCondensationFlag.allocate(3);
    state->dataSurface->SurfWinGainFrameDividerToZoneRep.allocate(3);
    state->dataSurface->InsideFrameCondensationFlag.allocate(3);
    state->dataSurface->InsideDividerCondensationFlag.allocate(3);

    state->dataSurface->SurfTAirRef(surfNum1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(surfNum2) = DataSurfaces::ZoneSupplyAirTemp;
    state->dataSurface->SurfTAirRef(surfNum3) = DataSurfaces::AdjacentAirTemp;

    state->dataHeatBalSurf->QdotConvOutRep.allocate(3);
    state->dataHeatBalSurf->QdotConvOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QConvOutReport.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRep.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QRadOutReport.allocate(3);
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs.allocate(3);
    state->dataHeatBalSurf->QAirExtReport.allocate(3);
    state->dataHeatBalSurf->QHeatEmiReport.allocate(3);

    state->dataHeatBal->SurfQRadSWOutIncident = 0.0;
    state->dataHeatBal->SurfWinQRadSWwinAbs = 0.0;
    state->dataHeatBal->SurfQRadThermInAbs = 0.0;

    state->dataHeatBalFanSys->QHTRadSysSurf = 0.0;
    state->dataHeatBalFanSys->QHWBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QSteamBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QElecBaseboardSurf = 0.0;
    state->dataSurface->SurfWinTransSolar = 0.0;
    state->dataHeatBal->EnclSolQSWRad = 0.0;

    Real64 inSurfTemp;
    Real64 outSurfTemp;

    // Calculate temperature based on supply flow rate
    WindowManager::CalcWindowHeatBalance(*state, surfNum2, state->dataHeatBal->HConvIn(surfNum2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(20.0, state->dataHeatBal->SurfTempEffBulkAir(surfNum2), 0.0001);
    // Calculate temperature based on zone temperature with supply flow rate = 0
    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    WindowManager::CalcWindowHeatBalance(*state, surfNum2, state->dataHeatBal->HConvIn(surfNum2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(25.0, state->dataHeatBal->SurfTempEffBulkAir(surfNum2), 0.0001);

    // Adjacent surface
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataSurface->Surface(1).ExtBoundCond = 2;
    WindowManager::CalcWindowHeatBalance(*state, surfNum2, state->dataHeatBal->HConvIn(surfNum2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(20.0, state->dataHeatBal->SurfTempEffBulkAir(surfNum2), 0.0001);

    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    state->dataSurface->Surface(1).ExtBoundCond = 2;
    state->dataSurface->Surface(2).ExtBoundCond = 1;
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneSupplyAirTemp;
    WindowManager::CalcWindowHeatBalance(*state, surfNum2, state->dataHeatBal->HConvIn(surfNum2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(25.0, state->dataHeatBal->SurfTempEffBulkAir(surfNum2), 0.0001);
}

TEST_F(EnergyPlusFixture, SpectralAngularPropertyTest)
{
    state->dataIPShortCut->lAlphaFieldBlanks = true;

    std::string const idf_objects = delimited_string({

        "  Building,",
        "    Small Office with AirflowNetwork model,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.001,                   !- Loads Convergence Tolerance Value",
        "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  Timestep,6;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  ZoneCapacitanceMultiplier:ResearchSpecial,",
        "    Multiplier,              !- Name",
        "    ,                        !- Zone or ZoneList Name",
        "    1.0,                     !- Temperature Capacity Multiplier",
        "    1.0,                     !- Humidity Capacity Multiplier",
        "    1.0,                     !- Carbon Dioxide Capacity Multiplier",
        "    ;                        !- Generic Contaminant Capacity Multiplier",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  RunPeriod,",
        "    WinterDay,               !- Name",
        "    1,                       !- Begin Month",
        "    14,                      !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    1,                       !- End Month",
        "    14,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  RunPeriod,",
        "    SummerDay,               !- Name",
        "    7,                       !- Begin Month",
        "    7,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    7,                       !- End Month",
        "    7,                       !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    No;                      !- Use Weather File Snow Indicators",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C4 - 4 IN COMMON BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1922.216,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7600000,               !- Solar Absorptance",
        "    0.7600000;               !- Visible Absorptance",

        "  Material,",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C6 - 8 IN CLAY TILE,     !- Name",
        "    Smooth,                  !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    0.5707605,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.8200000,               !- Solar Absorptance",
        "    0.8200000;               !- Visible Absorptance",

        "  Material,",
        "    C10 - 8 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
        "    Rough,                   !- Roughness",
        "    1.2710161E-02,           !- Thickness {m}",
        "    1.435549,                !- Conductivity {W/m-K}",
        "    881.0155,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5500000,               !- Solar Absorptance",
        "    0.5500000;               !- Visible Absorptance",

        "  Material,",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
        "    Rough,                   !- Roughness",
        "    9.5402403E-03,           !- Thickness {m}",
        "    0.1902535,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    B5 - 1 IN DENSE INSULATION,  !- Name",
        "    VeryRough,               !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    4.3239430E-02,           !- Conductivity {W/m-K}",
        "    91.30524,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    1.375in-Solid-Core,      !- Name",
        "    Smooth,                  !- Roughness",
        "    3.4925E-02,              !- Thickness {m}",
        "    0.1525000,               !- Conductivity {W/m-K}",
        "    614.5000,                !- Density {kg/m3}",
        "    1630.0000,               !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  WindowMaterial:Glazing,",
        "    SPECTRAL GLASS INNER PANE,  !- Name",
        "    Spectral,                !- Optical Data Type",
        "    TestSpectralDataSet,     !- Window Glass Spectral Data Set Name",
        "    0.0099,                  !- Thickness {m}",
        "    0.0,                     !- Solar Transmittance at Normal Incidence",
        "    0.0,                     !- Front Side Solar Reflectance at Normal Incidence",
        "    0.0,                     !- Back Side Solar Reflectance at Normal Incidence",
        "    0.0,                     !- Visible Transmittance at Normal Incidence",
        "    0.0,                     !- Front Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.798;                   !- Conductivity {W/m-K}",

        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS DARK STATE,!- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.111,                   !- Solar Transmittance at Normal Incidence",
        "    0.179,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.179,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.128,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS LIGHT STATE,  !- Name",
        "    SpectralAndAngle,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.0099,                  !- Thickness {m}",
        "    0.0,                     !- Solar Transmittance at Normal Incidence",
        "    0.0,                     !- Front Side Solar Reflectance at Normal Incidence",
        "    0.0,                     !- Back Side Solar Reflectance at Normal Incidence",
        "    0.0,                     !- Visible Transmittance at Normal Incidence",
        "    0.0,                     !- Front Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.798,                   !- Conductivity {W/m-K}",
        "    ,      !- Dirt Correction Factor for Solar and Visible Transmittance",
        "    ,      !- Solar Diffusing",
        "    ,      !- Young's modulus",
        "    ,      !- Poisson's ratio",
        "    TransmittanceData,       !- Window Glass Spectral and Incident Angle Transmittance Data Set Table Name",
        "    FrontReflectanceData,    !- Window Glass Spectral and Incident Angle Front Reflectance Data Set Table Name",
        "    BackRefelectanceData;    !- Window Glass Spectral and Incident Angle Back Reflectance Data Set Table Name",

        "Table:IndependentVariable,",
        "  Incidence Angles,          !- Name",
        "  Linear,                    !- Interpolation Method",
        "  Constant,                  !- Extrapolation Method",
        "  0,                         !- Minimum Value",
        "  90,                        !- Maximum Value",
        "  ,                          !- Normalization Reference Value",
        "  ,                          !- Unit Type",
        "  ,                          !- External File Name",
        "  ,                          !- External File Column Number",
        "  ,                          !- External File Starting Row Number",
        "  0,                         !- Value 1",
        "  90;",

        "Table:IndependentVariable,",
        "  Wavelengths,               !- Name",
        "  Linear,                    !- Interpolation Method",
        "  Constant,                  !- Extrapolation Method",
        "  0.30,                      !- Minimum Value",
        "  2.50,                      !- Maximum Value",
        "  ,                          !- Normalization Reference Value",
        "  ,                          !- Unit Type",
        "  ,                          !- External File Name",
        "  ,                          !- External File Column Number",
        "  ,                          !- External File Starting Row Number",
        "  0.300,                     !- Value 1",
        "  0.310,",
        "  0.320,",
        "  0.330,",
        "  0.340,",
        "  0.350,",
        "  0.360,",
        "  0.370,",
        "  0.380,",
        "  0.390,",
        "  0.400,",
        "  0.410,",
        "  0.420,",
        "  0.430,",
        "  0.440,",
        "  0.450,",
        "  0.460,",
        "  0.470,",
        "  0.480,",
        "  0.490,",
        "  0.500,",
        "  0.510,",
        "  0.520,",
        "  0.530,",
        "  0.540,",
        "  0.550,",
        "  0.560,",
        "  0.570,",
        "  0.580,",
        "  0.590,",
        "  0.600,",
        "  0.610,",
        "  0.620,",
        "  0.630,",
        "  0.640,",
        "  0.650,",
        "  0.660,",
        "  0.670,",
        "  0.680,",
        "  0.690,",
        "  0.700,",
        "  0.710,",
        "  0.720,",
        "  0.730,",
        "  0.740,",
        "  0.750,",
        "  0.760,",
        "  0.770,",
        "  0.780,",
        "  0.790,",
        "  0.800,",
        "  0.810,",
        "  0.820,",
        "  0.830,",
        "  0.840,",
        "  0.850,",
        "  0.860,",
        "  0.870,",
        "  0.880,",
        "  0.890,",
        "  0.900,",
        "  0.950,",
        "  1.000,",
        "  1.050,",
        "  1.100,",
        "  1.150,",
        "  1.200,",
        "  1.250,",
        "  1.300,",
        "  1.350,",
        "  1.400,",
        "  1.450,",
        "  1.500,",
        "  1.550,",
        "  1.600,",
        "  1.650,",
        "  1.700,",
        "  1.750,",
        "  1.800,",
        "  1.850,",
        "  1.900,",
        "  1.950,",
        "  2.000,",
        "  2.050,",
        "  2.100,",
        "  2.150,",
        "  2.200,",
        "  2.250,",
        "  2.300,",
        "  2.350,",
        "  2.400,",
        "  2.450,",
        "  2.500;",

        "Table:IndependentVariableList,",
        "  Spectral and Incident Angle Data Set,  !- Name",
        "  Incidence Angles,     !- Independent Variable 1 Name",
        "  Wavelengths;          !- Independent Variable 2 Name",

        "Table:Lookup,",
        "  TransmittanceData,         !- Name",
        "  Spectral and Incident Angle Data Set,  !- Independent Variable List Name",
        "  ,                          !- Normalization Method",
        "  ,                          !- Normalization Divisor",
        "  0.0,                       !- Minimum Output",
        "  1.0,                       !- Maximum Output",
        "  Dimensionless,             !- Output Unit Type",
        "  ,                          !- External File Name",
        "  ,                          !- External File Column Number",
        "  ,                          !- External File Starting Row Number",
        "  0.00100,                   !- Output Value 1",
        "  0.00100,",
        "  0.00100,",
        "  0.00100,",
        "  0.00100,",
        "  0.00100,",
        "  0.00900,",
        "  0.12000,",
        "  0.49200,",
        "  0.78200,",
        "  0.85600,",
        "  0.85800,",
        "  0.85800,",
        "  0.86000,",
        "  0.86100,",
        "  0.87100,",
        "  0.88000,",
        "  0.88300,",
        "  0.88700,",
        "  0.89000,",
        "  0.89000,",
        "  0.89100,",
        "  0.88700,",
        "  0.89000,",
        "  0.88300,",
        "  0.88800,",
        "  0.88200,",
        "  0.88100,",
        "  0.86500,",
        "  0.85800,",
        "  0.86500,",
        "  0.85600,",
        "  0.84500,",
        "  0.83700,",
        "  0.82700,",
        "  0.82000,",
        "  0.80700,",
        "  0.79800,",
        "  0.79100,",
        "  0.78100,",
        "  0.76800,",
        "  0.76100,",
        "  0.74400,",
        "  0.71300,",
        "  0.70300,",
        "  0.69400,",
        "  0.68500,",
        "  0.67500,",
        "  0.66700,",
        "  0.65500,",
        "  0.64600,",
        "  0.63800,",
        "  0.62900,",
        "  0.62300,",
        "  0.61400,",
        "  0.60800,",
        "  0.60100,",
        "  0.59700,",
        "  0.59200,",
        "  0.58700,",
        "  0.58200,",
        "  0.56800,",
        "  0.56200,",
        "  0.55600,",
        "  0.56300,",
        "  0.55600,",
        "  0.54700,",
        "  0.57700,",
        "  0.59800,",
        "  0.60800,",
        "  0.60300,",
        "  0.61400,",
        "  0.64800,",
        "  0.68000,",
        "  0.69900,",
        "  0.70600,",
        "  0.57000,",
        "  0.58500,",
        "  0.63700,",
        "  0.65500,",
        "  0.63700,",
        "  0.63400,",
        "  0.63400,",
        "  0.58600,",
        "  0.58800,",
        "  0.59700,",
        "  0.57600,",
        "  0.40400,",
        "  0.17900,",
        "  0.21900,",
        "  0.24000,",
        "  0.20000,",
        "  0.21400,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000,",
        "  0.00000;",

        "Table:Lookup,",
        "  FrontReflectanceData,      !- Name",
        "  Spectral and Incident Angle Data Set,  !- Independent Variable List Name",
        "  ,                          !- Normalization Method",
        "  ,                          !- Normalization Divisor",
        "  0.0,                       !- Minimum Output",
        "  1.0,                       !- Maximum Output",
        "  Dimensionless,             !- Output Unit Type",
        "  ,                          !- External File Name",
        "  ,                          !- External File Column Number",
        "  ,                          !- External File Starting Row Number",
        "  0.04500,                   !- Output Value 1",
        "  0.04400,",
        "  0.04400,",
        "  0.04200,",
        "  0.04100,",
        "  0.04000,",
        "  0.04000,",
        "  0.04000,",
        "  0.05100,",
        "  0.07000,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07600,",
        "  0.07500,",
        "  0.07600,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07400,",
        "  0.07400,",
        "  0.07400,",
        "  0.07400,",
        "  0.07100,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.06900,",
        "  0.06700,",
        "  0.06700,",
        "  0.06500,",
        "  0.06500,",
        "  0.06500,",
        "  0.06400,",
        "  0.06400,",
        "  0.06200,",
        "  0.06400,",
        "  0.06200,",
        "  0.06100,",
        "  0.06100,",
        "  0.06000,",
        "  0.06000,",
        "  0.06000,",
        "  0.05900,",
        "  0.05900,",
        "  0.05700,",
        "  0.05700,",
        "  0.05600,",
        "  0.05600,",
        "  0.05500,",
        "  0.05400,",
        "  0.05400,",
        "  0.05400,",
        "  0.05500,",
        "  0.05100,",
        "  0.05100,",
        "  0.05000,",
        "  0.05100,",
        "  0.05000,",
        "  0.05000,",
        "  0.05100,",
        "  0.05400,",
        "  0.05500,",
        "  0.05200,",
        "  0.05500,",
        "  0.05700,",
        "  0.05900,",
        "  0.06000,",
        "  0.06000,",
        "  0.05100,",
        "  0.05100,",
        "  0.05500,",
        "  0.05700,",
        "  0.05700,",
        "  0.05700,",
        "  0.05700,",
        "  0.05200,",
        "  0.05400,",
        "  0.05400,",
        "  0.05100,",
        "  0.04500,",
        "  0.03700,",
        "  0.03700,",
        "  0.03900,",
        "  0.04000,",
        "  0.03900,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000;",

        "Table:Lookup,",
        "  BackRefelectanceData,      !- Name",
        "  Spectral and Incident Angle Data Set,  !- Independent Variable List Name",
        "  ,                          !- Normalization Method",
        "  ,                          !- Normalization Divisor",
        "  0.0,                       !- Minimum Output",
        "  1.0,                       !- Maximum Output",
        "  Dimensionless,             !- Output Unit Type",
        "  ,                          !- External File Name",
        "  ,                          !- External File Column Number",
        "  ,                          !- External File Starting Row Number",
        "  0.04500,                   !- Output Value 1",
        "  0.04400,",
        "  0.04400,",
        "  0.04200,",
        "  0.04100,",
        "  0.04000,",
        "  0.04000,",
        "  0.04000,",
        "  0.05100,",
        "  0.07000,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07600,",
        "  0.07500,",
        "  0.07600,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07500,",
        "  0.07400,",
        "  0.07400,",
        "  0.07400,",
        "  0.07400,",
        "  0.07100,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.07000,",
        "  0.06900,",
        "  0.06700,",
        "  0.06700,",
        "  0.06500,",
        "  0.06500,",
        "  0.06500,",
        "  0.06400,",
        "  0.06400,",
        "  0.06200,",
        "  0.06400,",
        "  0.06200,",
        "  0.06100,",
        "  0.06100,",
        "  0.06000,",
        "  0.06000,",
        "  0.06000,",
        "  0.05900,",
        "  0.05900,",
        "  0.05700,",
        "  0.05700,",
        "  0.05600,",
        "  0.05600,",
        "  0.05500,",
        "  0.05400,",
        "  0.05400,",
        "  0.05400,",
        "  0.05500,",
        "  0.05100,",
        "  0.05100,",
        "  0.05000,",
        "  0.05100,",
        "  0.05000,",
        "  0.05000,",
        "  0.05100,",
        "  0.05400,",
        "  0.05500,",
        "  0.05200,",
        "  0.05500,",
        "  0.05700,",
        "  0.05900,",
        "  0.06000,",
        "  0.06000,",
        "  0.05100,",
        "  0.05100,",
        "  0.05500,",
        "  0.05700,",
        "  0.05700,",
        "  0.05700,",
        "  0.05700,",
        "  0.05200,",
        "  0.05400,",
        "  0.05400,",
        "  0.05100,",
        "  0.04500,",
        "  0.03700,",
        "  0.03700,",
        "  0.03900,",
        "  0.04000,",
        "  0.03900,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000,",
        "  1.00000;",

        "  WindowMaterial:Gas,",
        "    WinAirGap,               !- Name",
        "    AIR,                     !- Gas Type",
        "    0.013;                   !- Thickness {m}",

        "  MaterialProperty:GlazingSpectralData,",
        "    TestSpectralDataSet,     !- Name",
        "    0.300,0.000,0.045,0.045,",
        "    0.310,0.000,0.044,0.044,",
        "    0.320,0.000,0.044,0.044,",
        "    0.330,0.000,0.042,0.042,",
        "    0.340,0.000,0.041,0.041,",
        "    0.350,0.000,0.040,0.040,",
        "    0.360,0.009,0.040,0.040,",
        "    0.370,0.120,0.040,0.040,",
        "    0.380,0.492,0.051,0.051,",
        "    0.390,0.782,0.070,0.070,",
        "    0.400,0.856,0.075,0.075,",
        "    0.410,0.858,0.075,0.075,",
        "    0.420,0.858,0.075,0.075,",
        "    0.430,0.860,0.075,0.075,",
        "    0.440,0.861,0.075,0.075,",
        "    0.450,0.871,0.075,0.075,",
        "    0.460,0.880,0.076,0.076,",
        "    0.470,0.883,0.075,0.075,",
        "    0.480,0.887,0.076,0.076,",
        "    0.490,0.890,0.075,0.075,",
        "    0.500,0.890,0.075,0.075,",
        "    0.510,0.891,0.075,0.075,",
        "    0.520,0.887,0.075,0.075,",
        "    0.530,0.890,0.075,0.075,",
        "    0.540,0.883,0.074,0.074,",
        "    0.550,0.888,0.074,0.074,",
        "    0.560,0.882,0.074,0.074,",
        "    0.570,0.881,0.074,0.074,",
        "    0.580,0.865,0.071,0.071,",
        "    0.590,0.858,0.070,0.070,",
        "    0.600,0.865,0.070,0.070,",
        "    0.610,0.856,0.070,0.070,",
        "    0.620,0.845,0.070,0.070,",
        "    0.630,0.837,0.070,0.070,",
        "    0.640,0.827,0.069,0.069,",
        "    0.650,0.820,0.067,0.067,",
        "    0.660,0.807,0.067,0.067,",
        "    0.670,0.798,0.065,0.065,",
        "    0.680,0.791,0.065,0.065,",
        "    0.690,0.781,0.065,0.065,",
        "    0.700,0.768,0.064,0.064,",
        "    0.710,0.761,0.064,0.064,",
        "    0.720,0.744,0.062,0.062,",
        "    0.730,0.713,0.064,0.064,",
        "    0.740,0.703,0.062,0.062,",
        "    0.750,0.694,0.061,0.061,",
        "    0.760,0.685,0.061,0.061,",
        "    0.770,0.675,0.060,0.060,",
        "    0.780,0.667,0.060,0.060,",
        "    0.790,0.655,0.060,0.060,",
        "    0.800,0.646,0.059,0.059,",
        "    0.810,0.638,0.059,0.059,",
        "    0.820,0.629,0.057,0.057,",
        "    0.830,0.623,0.057,0.057,",
        "    0.840,0.614,0.056,0.056,",
        "    0.850,0.608,0.056,0.056,",
        "    0.860,0.601,0.055,0.055,",
        "    0.870,0.597,0.054,0.054,",
        "    0.880,0.592,0.054,0.054,",
        "    0.890,0.587,0.054,0.054,",
        "    0.900,0.582,0.055,0.055,",
        "    0.950,0.568,0.051,0.051,",
        "    1.000,0.562,0.051,0.051,",
        "    1.050,0.556,0.050,0.050,",
        "    1.100,0.563,0.051,0.051,",
        "    1.150,0.556,0.050,0.050,",
        "    1.200,0.547,0.050,0.050,",
        "    1.250,0.577,0.051,0.051,",
        "    1.300,0.598,0.054,0.054,",
        "    1.350,0.608,0.055,0.055,",
        "    1.400,0.603,0.052,0.052,",
        "    1.450,0.614,0.055,0.055,",
        "    1.500,0.648,0.057,0.057,",
        "    1.550,0.680,0.059,0.059,",
        "    1.600,0.699,0.060,0.060,",
        "    1.650,0.706,0.060,0.060,",
        "    1.700,0.570,0.051,0.051,",
        "    1.750,0.585,0.051,0.051,",
        "    1.800,0.637,0.055,0.055,",
        "    1.850,0.655,0.057,0.057,",
        "    1.900,0.637,0.057,0.057,",
        "    1.950,0.634,0.057,0.057,",
        "    2.000,0.634,0.057,0.057,",
        "    2.050,0.586,0.052,0.052,",
        "    2.100,0.588,0.054,0.054,",
        "    2.150,0.597,0.054,0.054,",
        "    2.200,0.576,0.051,0.051,",
        "    2.250,0.404,0.045,0.045,",
        "    2.300,0.179,0.037,0.037,",
        "    2.350,0.219,0.037,0.037,",
        "    2.400,0.240,0.039,0.039,",
        "    2.450,0.200,0.040,0.040,",
        "    2.500,0.214,0.039,0.039;",
        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    PARTITION06,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C6 - 8 IN CLAY TILE,     !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    FLOOR SLAB 8 IN,         !- Name",
        "    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

        "  Construction,",
        "    ROOF34,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 3",

        "  Construction,",
        "    CEILING:ZONE,            !- Name",
        "    B5 - 1 IN DENSE INSULATION,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2",

        "  Construction,",
        "    CEILING:ATTIC,           !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    B5 - 1 IN DENSE INSULATION;  !- Layer 2",

        "  Construction,",
        "    ELECTRO-CON-LIGHT,       !- Name",
        "    ELECTRO GLASS LIGHT STATE,  !- Outside Layer",
        "    WinAirGap,               !- Layer 2",
        "    SPECTRAL GLASS INNER PANE;  !- Layer 3",

        "  Construction,",
        "    ELECTRO-CON-DARK,        !- Name",
        "    ELECTRO GLASS LIGHT STATE,  !- Outside Layer",
        "    WinAirGap,               !- Layer 2",
        "    SPECTRAL GLASS INNER PANE;  !- Layer 3",

        "  Construction,",
        "    DOOR-CON,                !- Name",
        "    1.375in-Solid-Core;      !- Outside Layer",

        "  Zone,",
        "    West Zone,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    EAST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    NORTH ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    ATTIC ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr002,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall003,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr003,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall006,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall007,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall008,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof002,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof003,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr002,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr003,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    ELECTRO-CON-LIGHT,       !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall003:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn001:Wall003,           !- Building Surface Name",
        "    Zn003:Wall004:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    3.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall002:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    ELECTRO-CON-LIGHT,       !- Construction Name",
        "    Zn003:Wall002,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    5.548000,12.19200,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.548000,12.19200,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall004:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn003:Wall004,           !- Building Surface Name",
        "    Zn001:Wall003:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  WindowShadingControl,",
        "    WIN-CONTROL-GLARE,       !- Name",
        "    West Zone,               !- Zone Name",
        "    1,                       !- Shading Control Sequence Number ",
        "    SwitchableGlazing,       !- Shading Type",
        "    ELECTRO-CON-DARK,        !- Construction with Shading Name",
        "    OnIfHighGlare,           !- Shading Control Type",
        "    ,                        !- Schedule Name",
        "    0.0,                     !- Setpoint {W/m2, W or deg C}",
        "    No,                      !- Shading Control Is Scheduled",
        "    YES,                     !- Glare Control Is Active",
        "    ,                        !- Shading Device Material Name",
        "    FixedSlatAngle,          !- Type of Slat Angle Control for Blinds",
        "    ,                        !- Slat Angle Schedule Name",
        "    ,                        !- Setpoint 2",
        "    ,                        !- Daylighting Control Object Name",
        "    ,                        !- Multiple Surface Control Type",
        "    Zn001:Wall001:Win001;    !- Fenestration Surface 1 Name",

        "  WindowProperty:FrameAndDivider,",
        "    TestFrameAndDivider,     !- Name",
        "    0.05,                    !- Frame Width {m}",
        "    0.05,                    !- Frame Outside Projection {m}",
        "    0.05,                    !- Frame Inside Projection {m}",
        "    5.0,                     !- Frame Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
        "    0.8,                     !- Frame Solar Absorptance",
        "    0.8,                     !- Frame Visible Absorptance",
        "    0.9,                     !- Frame Thermal Hemispherical Emissivity",
        "    DividedLite,             !- Divider Type",
        "    0.02,                    !- Divider Width {m}",
        "    2,                       !- Number of Horizontal Dividers",
        "    2,                       !- Number of Vertical Dividers",
        "    0.02,                    !- Divider Outside Projection {m}",
        "    0.02,                    !- Divider Inside Projection {m}",
        "    5.0,                     !- Divider Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
        "    0.8,                     !- Divider Solar Absorptance",
        "    0.8,                     !- Divider Visible Absorptance",
        "    0.9;                     !- Divider Thermal Hemispherical Emissivity",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;

    HeatBalanceManager::GetWindowGlassSpectralData(*state, FoundError);
    EXPECT_FALSE(FoundError);
    HeatBalanceManager::GetMaterialData(*state, FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetFrameAndDividerData(*state, FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetConstructData(*state, FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetZoneData(*state, FoundError); // Read Zone data from input file
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::GetGeometryParameters(*state, FoundError);
    EXPECT_FALSE(FoundError);

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(4);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(4);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosZoneRelNorth(2) = std::cos(-state->dataHeatBal->Zone(2).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosZoneRelNorth(3) = std::cos(-state->dataHeatBal->Zone(3).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosZoneRelNorth(4) = std::cos(-state->dataHeatBal->Zone(4).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(2) = std::sin(-state->dataHeatBal->Zone(2).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(3) = std::sin(-state->dataHeatBal->Zone(3).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(4) = std::sin(-state->dataHeatBal->Zone(4).RelNorth * DataGlobalConstants::DegToRadians);

    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(*state, FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                            // expect no errors

    WindowManager::InitGlassOpticalCalculations(*state);

    int NumAngles = 10; // Number of incident angles
    Real64 sum;
    // total transmittance
    Array1D<Real64> correctT(
        NumAngles, {0.529017128, 0.472866571, 0.414862350, 0.355230972, 0.294204731, 0.232087506, 0.169331950, 0.10672958, 0.04626078, 0.0});
    // total reflectance
    Array1D<Real64> correctR(
        NumAngles, {0.097222311, 0.194253146, 0.29213968, 0.39110239, 0.491349618, 0.59297952, 0.695822715, 0.79917258, 0.90138662, 1.00000000});
    // Layer 1 absortance
    Array1D<Real64> correctabs1(
        NumAngles, {0.242079608, 0.214464137, 0.187033583, 0.159840540, 0.132932950, 0.10633161, 0.079994699, 0.053758780, 0.027261664, 0.0});
    // Layer 2 absortance
    Array1D<Real64> correctabs2(
        NumAngles, {0.131680954, 0.118416146, 0.105964377, 0.093826087, 0.08151269, 0.068601358, 0.054850634, 0.040339052, 0.025090929, 0.0});

    for (int i = 1; i <= NumAngles; i++) {
        EXPECT_NEAR(correctT(i), state->dataWindowManager->tsolPhi(i), 0.0001);
        EXPECT_NEAR(correctR(i), state->dataWindowManager->rfsolPhi(i), 0.0001);
        EXPECT_NEAR(correctabs1(i), state->dataWindowManager->solabsPhi(1, i), 0.0001);
        EXPECT_NEAR(correctabs2(i), state->dataWindowManager->solabsPhi(2, i), 0.0001);
        sum = state->dataWindowManager->tsolPhi(i) + state->dataWindowManager->rfsolPhi(i) + state->dataWindowManager->solabsPhi(1, i) +
              state->dataWindowManager->solabsPhi(2, i);
        EXPECT_NEAR(sum, 1.0, 0.0001);
    }

    state->dataSurfaceGeometry->CosZoneRelNorth.deallocate();
    state->dataSurfaceGeometry->SinZoneRelNorth.deallocate();
}

TEST_F(EnergyPlusFixture, WindowManager_SrdLWRTest)
{
    // GitHub issue 6037
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Material,",
                          "  Concrete Block,          !- Name",
                          "  MediumRough,             !- Roughness",
                          "  0.1014984,               !- Thickness {m}",
                          "  0.3805070,               !- Conductivity {W/m-K}",
                          "  608.7016,                !- Density {kg/m3}",
                          "  836.8000;                !- Specific Heat {J/kg-K}",
                          "Construction,",
                          "  WallConstruction,        !- Name",
                          "  Concrete Block;          !- Outside Layer",
                          "WindowMaterial:SimpleGlazingSystem,",
                          "  WindowMaterial,          !- Name",
                          "  5.778,                   !- U-Factor {W/m2-K}",
                          "  0.819,                   !- Solar Heat Gain Coefficient",
                          "  0.881;                   !- Visible Transmittance",
                          "Construction,",
                          "  WindowConstruction,      !- Name",
                          "  WindowMaterial;          !- Outside Layer",
                          "WindowProperty:FrameAndDivider,",
                          "  WindowFrame,             !- Name",
                          "  0.05,                    !- Frame Width {m}",
                          "  0.00,                    !- Frame Outside Projection {m}",
                          "  0.00,                    !- Frame Inside Projection {m}",
                          "  5.0,                     !- Frame Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Frame Solar Absorptance",
                          "  0.8,                     !- Frame Visible Absorptance",
                          "  0.9,                     !- Frame Thermal Hemispherical Emissivity",
                          "  DividedLite,             !- Divider Type",
                          "  0.02,                    !- Divider Width {m}",
                          "  2,                       !- Number of Horizontal Dividers",
                          "  2,                       !- Number of Vertical Dividers",
                          "  0.00,                    !- Divider Outside Projection {m}",
                          "  0.00,                    !- Divider Inside Projection {m}",
                          "  5.0,                     !- Divider Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Divider Solar Absorptance",
                          "  0.8,                     !- Divider Visible Absorptance",
                          "  0.9;                     !- Divider Thermal Hemispherical Emissivity",
                          "FenestrationSurface:Detailed,",
                          "  FenestrationSurface,     !- Name",
                          "  Window,                  !- Surface Type",
                          "  WindowConstruction,      !- Construction Name",
                          "  Wall,                    !- Building Surface Name",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  0.5000000,               !- View Factor to Ground",
                          "  WindowFrame,             !- Frame and Divider Name",
                          "  1.0,                     !- Multiplier",
                          "  4,                       !- Number of Vertices",
                          "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "SurfaceProperty:LocalEnvironment,",
                          "  LocEnv:FenestrationSurface,          !- Name",
                          "  FenestrationSurface,                 !- Exterior Surface Name",
                          "  ,                             !- External Shading Fraction Schedule Name",
                          "  SrdSurfs:FenestrationSurface,        !- Surrounding Surfaces Object Name",
                          "  ;                             !- Outdoor Air Node Name",
                          "SurfaceProperty:SurroundingSurfaces,",
                          "  SrdSurfs:FenestrationSurface, !- Name",
                          "  0.3,",
                          "  Sky Temp Sch,",
                          "  0.1,",
                          "  Ground Temp Sch,",
                          "  SurroundingSurface1,",
                          "  0.6,",
                          "  Surrounding Temp Sch 1;",
                          "Schedule:Compact,",
                          "  Surrounding Temp Sch 1,       !- Name",
                          "  Any Number,                   !- Schedule Type Limits Name",
                          "  Through: 12/31,               !- Field 1",
                          "  For: AllDays,                 !- Field 2",
                          "  Until: 24:00, 15.0;           !- Field 3",
                          "BuildingSurface:Detailed,",
                          "  Wall,                    !- Name",
                          "  Wall,                    !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  SunExposed,              !- Sun Exposure",
                          "  WindExposed,             !- Wind Exposure",
                          "  0.5000000,               !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,"
                          "  Floor,                   !- Name",
                          "  Floor,                   !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  NoSun,                   !- Sun Exposure",
                          "  NoWind,                  !- Wind Exposure",
                          "  1.0,                     !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                          "Zone,"
                          "  Zone,                    !- Name",
                          "  0,                       !- Direction of Relative North {deg}",
                          "  6.000000,                !- X Origin {m}",
                          "  6.000000,                !- Y Origin {m}",
                          "  0,                       !- Z Origin {m}",
                          "  1,                       !- Type",
                          "  1,                       !- Multiplier",
                          "  autocalculate,           !- Ceiling Height {m}",
                          "  autocalculate;           !- Volume {m3}"});

    ASSERT_TRUE(process_idf(idf_objects));
    ScheduleManager::ProcessScheduleInput(*state);
    state->dataHeatBal->ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    HeatBalanceManager::GetFrameAndDividerData(*state, ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    HeatBalanceManager::GetBuildingData(*state, ErrorsFound);

    EXPECT_TRUE(state->dataGlobal->AnyLocalEnvironmentsInModel);

    Psychrometrics::InitializePsychRoutines(*state);

    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->TimeStepZone = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataEnvrn->OutBaroPress = 100000;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);

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

    state->dataLoopNodes->Node.allocate(4);
    state->dataHeatBal->SurfTempEffBulkAir.allocate(3);
    state->dataHeatBalSurf->TempSurfInTmp.allocate(3);

    int surfNum1 = UtilityRoutines::FindItemInList("WALL", state->dataSurface->Surface);
    int surfNum2 = UtilityRoutines::FindItemInList("FENESTRATIONSURFACE", state->dataSurface->Surface);
    int surfNum3 = UtilityRoutines::FindItemInList("FLOOR", state->dataSurface->Surface);

    state->dataSurface->Surface(surfNum1).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum2).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum3).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum1).Area = 10.0;
    state->dataSurface->Surface(surfNum2).Area = 10.0;
    state->dataSurface->Surface(surfNum3).Area = 10.0;
    state->dataSurface->Surface(surfNum1).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum2).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum3).SolarEnclIndex = 1;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum1) = 15.0;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum2) = 20.0;
    state->dataHeatBalSurf->TempSurfInTmp(surfNum3) = 25.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum1) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum2) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum3) = 10.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBal->HConvIn.allocate(3);
    state->dataHeatBal->HConvIn(surfNum1) = 0.5;
    state->dataHeatBal->HConvIn(surfNum2) = 0.5;
    state->dataHeatBal->HConvIn(surfNum3) = 0.5;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;

    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->QHTRadSysSurf.allocate(3);
    state->dataHeatBalFanSys->QHWBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QSteamBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QElecBaseboardSurf.allocate(3);
    state->dataHeatBal->SurfWinQRadSWwinAbs.allocate(3, 1);
    state->dataHeatBal->SurfQRadThermInAbs.allocate(3);
    state->dataHeatBal->SurfQRadSWOutIncident.allocate(3);
    state->dataSurface->SurfWinTransSolar.allocate(3);
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);
    state->dataSurface->SurfWinHeatGain.allocate(3);
    state->dataSurface->SurfWinHeatTransfer.allocate(3);
    state->dataSurface->SurfWinGainConvGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRepEnergy.allocate(3);
    state->dataHeatBal->EnclSolQSWRad.allocate(1);
    state->dataSurface->SurfWinLossSWZoneToOutWinRep.allocate(3);
    state->dataSurface->SurfWinSysSolTransmittance.allocate(3);
    state->dataSurface->SurfWinSysSolAbsorptance.allocate(3);
    state->dataSurface->SurfWinSysSolReflectance.allocate(3);
    state->dataSurface->InsideGlassCondensationFlag.allocate(3);
    state->dataSurface->SurfWinGainFrameDividerToZoneRep.allocate(3);
    state->dataSurface->InsideFrameCondensationFlag.allocate(3);
    state->dataSurface->InsideDividerCondensationFlag.allocate(3);
    state->dataSurface->SurfTAirRef(surfNum1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(surfNum2) = DataSurfaces::ZoneSupplyAirTemp;
    state->dataSurface->SurfTAirRef(surfNum3) = DataSurfaces::AdjacentAirTemp;

    state->dataHeatBalSurf->QdotConvOutRep.allocate(3);
    state->dataHeatBalSurf->QdotConvOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QConvOutReport.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRep.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QRadOutReport.allocate(3);
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs.allocate(3);
    state->dataHeatBalSurf->QAirExtReport.allocate(3);
    state->dataHeatBalSurf->QHeatEmiReport.allocate(3);

    state->dataHeatBal->SurfQRadSWOutIncident = 0.0;
    state->dataHeatBal->SurfWinQRadSWwinAbs = 0.0;
    state->dataHeatBal->SurfQRadThermInAbs = 0.0;

    state->dataHeatBalFanSys->QHTRadSysSurf = 0.0;
    state->dataHeatBalFanSys->QHWBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QSteamBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QElecBaseboardSurf = 0.0;
    state->dataSurface->SurfWinTransSolar = 0.0;
    state->dataHeatBal->EnclSolQSWRad = 0.0;

    Real64 inSurfTemp;
    Real64 outSurfTemp;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 25.0; // Srd Srfs Temp
    // Calculate temperature based on supply flow rate

    WindowManager::CalcWindowHeatBalance(*state, surfNum2, state->dataHeatBal->HConvIn(surfNum2), inSurfTemp, outSurfTemp);
    // Test if LWR from surrounding surfaces correctly calculated
    EXPECT_DOUBLE_EQ(DataGlobalConstants::StefanBoltzmann * 0.84 * 0.6 *
                         (pow_4(25.0 + DataGlobalConstants::KelvinConv) - pow_4(state->dataWindowManager->thetas(1))),
                     state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(surfNum2));
    EXPECT_NEAR(-24.9342, state->dataHeatBalSurf->QHeatEmiReport(surfNum2), 3);
}
TEST_F(EnergyPlusFixture, WindowMaterialComplexShadeTest)
{

    std::string const idf_objects = delimited_string({"WindowMaterial:ComplexShade,",
                                                      "Shade_14_Layer,          !- Name",
                                                      "VenetianHorizontal,      !- Layer Type",
                                                      "1.016000e-003,           !- Thickness {m}",
                                                      "1.592276e+002,           !- Conductivity {W / m - K}",
                                                      "0.000000e+000,           !- IR Transmittance",
                                                      "0.9,                     !- Front Emissivity",
                                                      "0.9,                       !- Back Emissivity",
                                                      "0.000000e+000,           !- Top Opening Multiplier",
                                                      "0.000000e+000,           !- Bottom Opening Multiplier",
                                                      "0.000000e+000,           !- Left Side Opening Multiplier",
                                                      "0.000000e+000,           !- Right Side Opening Multiplier",
                                                      "5.000000e-002,           !- Front Opening Multiplier",
                                                      "0.0254,                  !- Slat Width {m}",
                                                      "0.0201,                  !- Slat Spacing {m}",
                                                      "0.0010,                  !- Slat Thickness {m}",
                                                      "45.0000,                 !- Slat Angle {deg}",
                                                      "159.2276,                !- Slat Conductivity {W / m - K}",
                                                      "0.0000;                  !- Slat Curve {m}"});

    ASSERT_TRUE(process_idf(idf_objects));
    bool errors_found = false;
    HeatBalanceManager::GetMaterialData(*state, errors_found);
    EXPECT_FALSE(errors_found);
    EXPECT_EQ(state->dataHeatBal->ComplexShade(1).Name, "SHADE_14_LAYER");
    EXPECT_EQ(state->dataHeatBal->ComplexShade(1).LayerType, TARCOGParams::TARCOGLayerType::VENETBLIND_HORIZ);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).Thickness, 1.016000e-003, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).Conductivity, 1.592276e+002, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).IRTransmittance, 0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).FrontEmissivity, 0.9, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).BackEmissivity, 0.9, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).TopOpeningMultiplier, 0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).BottomOpeningMultiplier, 0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).LeftOpeningMultiplier, 0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).RightOpeningMultiplier, 0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).FrontOpeningMultiplier, 5.000000e-002, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatWidth, 0.0254, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatSpacing, 0.0201, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatThickness, 0.0010, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatAngle, 45.0, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatConductivity, 159.2276, 1e-5);
    EXPECT_NEAR(state->dataHeatBal->ComplexShade(1).SlatCurve, 0, 1e-5);
}
