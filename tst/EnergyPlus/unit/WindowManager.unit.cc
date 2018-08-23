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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <ConvectionCoefficients.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataSurfaces.hh>
#include <ElectricPowerServiceManager.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SolarShading.hh>
#include <WindowManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WindowManager;

TEST_F(EnergyPlusFixture, WindowFrameTest)
{

    DataIPShortCuts::lAlphaFieldBlanks = true;

    std::string const idf_objects =
        delimited_string({"Version,8.4;",
                          "Material,",
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
                          "BuildingSurface:Detailed,"
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

    DataHeatBalance::ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject();
    HeatBalanceManager::SetPreConstructionInputParameters();

    Psychrometrics::InitializePsychRoutines();

    DataGlobals::TimeStep = 1;
    DataGlobals::TimeStepZone = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::BeginSimFlag = true;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::OutBaroPress = 100000;

    HeatBalanceManager::ManageHeatBalance();

    // This test will emulate NFRC 100 U-factor test
    int winNum;

    for (size_t i = 1; i <= DataSurfaces::Surface.size(); ++i) {
        if (DataSurfaces::Surface(i).Class == DataSurfaces::SurfaceClass_Window) {
            winNum = i;
        }
    }

    int cNum;

    for (size_t i = 1; i <= DataHeatBalance::Construct.size(); ++i) {
        if (DataHeatBalance::Construct(i).TypeIsWindow) {
            cNum = i;
        }
    }

    Real64 T_in = 21.0;
    Real64 T_out = -18.0;
    Real64 I_s = 0.0;
    Real64 v_ws = 5.5;

    // Overrides for testing
    DataHeatBalance::CosIncAng.dimension(1, 1, 3, 1.0);
    DataHeatBalance::SunlitFrac.dimension(1, 1, 3, 1.0);
    DataHeatBalance::SunlitFracWithoutReveal.dimension(1, 1, 3, 1.0);

    DataSurfaces::Surface(winNum).OutDryBulbTemp = T_out;
    DataHeatBalance::TempEffBulkAir(winNum) = T_in;
    DataSurfaces::SurfaceWindow(winNum).IRfromParentZone = DataGlobals::StefanBoltzmann * std::pow(T_in + DataGlobals::KelvinConv, 4);
    DataHeatBalFanSys::ZoneAirHumRatAvg.dimension(1, 0.01);
    DataHeatBalFanSys::ZoneAirHumRat.dimension(1, 0.01);
    DataHeatBalFanSys::MAT.dimension(1, T_in);

    // initial guess temperatures
    int numTemps = 2 + 2 * DataHeatBalance::Construct(cNum).TotGlassLayers;
    Real64 inSurfTemp = T_in - (1.0 / (numTemps - 1)) * (T_in - T_out);
    Real64 outSurfTemp = T_out + (1.0 / (numTemps - 1)) * (T_in - T_out);

    Real64 h_exterior_f = 4 + v_ws * 4;
    Real64 h_exterior;

    DataEnvironment::BeamSolarRad = I_s;

    if (I_s > 0.0) {
        DataEnvironment::SunIsUp = true;
    }

    HeatBalanceSurfaceManager::InitSolarHeatGains();
    SolarShading::CalcInteriorSolarDistribution();

    // Calculate heat balance (iteratively solve for surface temperatures)
    Real64 outSurfTempPrev = outSurfTemp;
    Real64 inSurfTempPrev = inSurfTemp;

    Real64 outSurfTempDiff;
    Real64 inSurfTempDiff;

    int maxIterations = 20;
    Real64 tolerance = 0.1; // deg C

    // Save tilt information for natural convection calculations
    Real64 tiltSave = DataSurfaces::Surface(winNum).Tilt;

    for (int i = 0; i < maxIterations; i++) {

        // Use complementary angle for exterior natural convection calculations
        DataSurfaces::Surface(1).Tilt = 180 - tiltSave;
        DataSurfaces::Surface(1).CosTilt = cos(DataSurfaces::Surface(winNum).Tilt * DataGlobals::Pi / 180);
        DataSurfaces::Surface(1).SinTilt = sin(DataSurfaces::Surface(winNum).Tilt * DataGlobals::Pi / 180);
        ConvectionCoefficients::CalcISO15099WindowIntConvCoeff(
            winNum, outSurfTemp,
            T_out); // This subroutine sets the global HConvIn( 1 ) variable. We will use it to set the exterior natural convection.
        h_exterior = h_exterior_f + DataHeatBalance::HConvIn(winNum); // add natural convection

        // revert tilt for interior natural convection calculations
        DataSurfaces::Surface(1).Tilt = tiltSave;
        DataSurfaces::Surface(1).CosTilt = cos(tiltSave * DataGlobals::Pi / 180);
        DataSurfaces::Surface(1).SinTilt = sin(tiltSave * DataGlobals::Pi / 180);
        ConvectionCoefficients::CalcISO15099WindowIntConvCoeff(
            winNum, inSurfTemp,
            T_in); // This time it's actually being used as intended. HConvIn( 1 ) is referenced from the actual heat balance calculation.

        WindowManager::CalcWindowHeatBalance(winNum, h_exterior, inSurfTemp, outSurfTemp);

        outSurfTempDiff = std::fabs(outSurfTemp - outSurfTempPrev);
        inSurfTempDiff = std::fabs(inSurfTemp - inSurfTempPrev);

        if ((outSurfTempDiff < tolerance) && (inSurfTempDiff < tolerance)) {
            break;
        }

        outSurfTempPrev = outSurfTemp;
        inSurfTempPrev = inSurfTemp;
    }

    EXPECT_GT(DataSurfaces::WinHeatLossRep(winNum), DataSurfaces::WinHeatTransfer(winNum));
}

TEST_F(EnergyPlusFixture, WindowManager_TransAndReflAtPhi)
{

    Real64 const cs = 0.86603; // Cosine of incidence angle
    Real64 const tf0 = 0.8980; // Transmittance at zero incidence angle
    Real64 const rf0 = 0.0810; // Front reflectance at zero incidence angle
    Real64 const rb0 = 0.0810; // Back reflectance at zero incidence angle

    Real64 tfp = 0.; // Transmittance at cs
    Real64 rfp = 0.; // Front reflectance at cs
    Real64 rbp = 0.; // Back reflectance at cs

    bool const SimpleGlazingSystem = false; // .TRUE. if simple block model being used
    Real64 const SimpleGlazingSHGC = 0.;    // SHGC value to use in alternate model for simple glazing system
    Real64 const SimpleGlazingU = 0.;       // U-factor value to use in alternate model for simple glazing system

    TransAndReflAtPhi(cs, tf0, rf0, rb0, tfp, rfp, rbp, SimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU);

    EXPECT_NEAR(tfp, 0.89455, 0.0001);
    EXPECT_NEAR(rfp, 0.08323, 0.0001);
    EXPECT_NEAR(rbp, 0.08323, 0.0001);
}

TEST_F(EnergyPlusFixture, WindowManager_RefAirTempTest)
{
    // GitHub issue 6037
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Version,8.4;",
                          "Material,",
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
                          "BuildingSurface:Detailed,"
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

    DataHeatBalance::ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject();
    HeatBalanceManager::SetPreConstructionInputParameters();
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    HeatBalanceManager::GetFrameAndDividerData(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    HeatBalanceManager::GetBuildingData(ErrorsFound);

    Psychrometrics::InitializePsychRoutines();

    DataGlobals::TimeStep = 1;
    DataGlobals::TimeStepZone = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::BeginSimFlag = true;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::OutBaroPress = 100000;

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "Zone";
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
    DataHeatBalance::Zone(1).ZoneEqNum = 1;
    DataHeatBalance::Zone(1).IsControlled = true;
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 2;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(2);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 1;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(2) = 2;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 3;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 4;

    DataLoopNode::Node.allocate(4);
    DataHeatBalance::TempEffBulkAir.allocate(3);
    DataHeatBalSurface::TempSurfInTmp.allocate(3);

    DataSurfaces::Surface(1).HeatTransSurf = true;
    DataSurfaces::Surface(2).HeatTransSurf = true;
    DataSurfaces::Surface(3).HeatTransSurf = true;
    DataSurfaces::Surface(1).Area = 10.0;
    DataSurfaces::Surface(2).Area = 10.0;
    DataSurfaces::Surface(3).Area = 10.0;
    DataSurfaces::Surface(1).TAirRef = DataSurfaces::ZoneMeanAirTemp;
    DataSurfaces::Surface(2).TAirRef = DataSurfaces::ZoneSupplyAirTemp;
    DataSurfaces::Surface(3).TAirRef = DataSurfaces::AdjacentAirTemp;
    DataHeatBalSurface::TempSurfInTmp(1) = 15.0;
    DataHeatBalSurface::TempSurfInTmp(2) = 20.0;
    DataHeatBalSurface::TempSurfInTmp(3) = 25.0;
    DataHeatBalance::TempEffBulkAir(1) = 10.0;
    DataHeatBalance::TempEffBulkAir(2) = 10.0;
    DataHeatBalance::TempEffBulkAir(3) = 10.0;

    DataLoopNode::Node(1).Temp = 20.0;
    DataLoopNode::Node(2).Temp = 20.0;
    DataLoopNode::Node(3).Temp = 20.0;
    DataLoopNode::Node(4).Temp = 20.0;
    DataLoopNode::Node(1).MassFlowRate = 0.1;
    DataLoopNode::Node(2).MassFlowRate = 0.1;
    DataLoopNode::Node(3).MassFlowRate = 0.1;
    DataLoopNode::Node(4).MassFlowRate = 0.1;

    DataHeatBalance::HConvIn.allocate(3);
    DataHeatBalance::HConvIn(1) = 0.5;
    DataHeatBalance::HConvIn(2) = 0.5;
    DataHeatBalance::HConvIn(3) = 0.5;
    DataHeatBalance::Zone(1).IsControlled = true;
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.011;
    DataHeatBalFanSys::ZoneAirHumRatAvg.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRatAvg(1) = DataHeatBalFanSys::ZoneAirHumRat(1) = 0.011;

    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 25.0;
    DataHeatBalFanSys::QHTRadSysSurf.allocate(3);
    DataHeatBalFanSys::QHWBaseboardSurf.allocate(3);
    DataHeatBalFanSys::QSteamBaseboardSurf.allocate(3);
    DataHeatBalFanSys::QElecBaseboardSurf.allocate(3);
    DataHeatBalance::QRadSWwinAbs.allocate(1, 3);
    DataHeatBalance::QRadThermInAbs.allocate(3);
    DataHeatBalance::QRadSWOutIncident.allocate(3);
    DataSurfaces::WinTransSolar.allocate(3);
    DataHeatBalance::ZoneWinHeatGain.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRep.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRepEnergy.allocate(1);
    DataSurfaces::WinHeatGain.allocate(3);
    DataSurfaces::WinHeatTransfer.allocate(3);
    DataSurfaces::WinGainConvGlazToZoneRep.allocate(3);
    DataSurfaces::WinGainIRGlazToZoneRep.allocate(3);
    DataSurfaces::WinGapConvHtFlowRep.allocate(3);
    DataSurfaces::WinGapConvHtFlowRepEnergy.allocate(3);
    DataHeatBalance::QS.allocate(1);
    DataSurfaces::WinLossSWZoneToOutWinRep.allocate(3);
    DataSurfaces::WinSysSolTransmittance.allocate(3);
    DataSurfaces::WinSysSolAbsorptance.allocate(3);
    DataSurfaces::WinSysSolReflectance.allocate(3);
    DataSurfaces::InsideGlassCondensationFlag.allocate(3);
    DataSurfaces::WinGainFrameDividerToZoneRep.allocate(3);
    DataSurfaces::InsideFrameCondensationFlag.allocate(3);
    DataSurfaces::InsideDividerCondensationFlag.allocate(3);

    DataHeatBalSurface::QdotConvOutRep.allocate(3);
    DataHeatBalSurface::QdotConvOutRepPerArea.allocate(3);
    DataHeatBalSurface::QConvOutReport.allocate(3);
    DataHeatBalSurface::QdotRadOutRep.allocate(3);
    DataHeatBalSurface::QdotRadOutRepPerArea.allocate(3);
    DataHeatBalSurface::QRadOutReport.allocate(3);
    DataHeatBalSurface::QRadLWOutSrdSurfs.allocate(3);

    DataHeatBalance::QRadSWOutIncident = 0.0;
    DataHeatBalance::QRadSWwinAbs = 0.0;
    DataHeatBalance::QRadThermInAbs = 0.0;

    DataHeatBalFanSys::QHTRadSysSurf = 0.0;
    DataHeatBalFanSys::QHWBaseboardSurf = 0.0;
    DataHeatBalFanSys::QSteamBaseboardSurf = 0.0;
    DataHeatBalFanSys::QElecBaseboardSurf = 0.0;
    DataSurfaces::WinTransSolar = 0.0;
    DataHeatBalance::QS = 0.0;

    Real64 inSurfTemp;
    Real64 outSurfTemp;

    // Claculate temperature based on supply flow rate
    WindowManager::CalcWindowHeatBalance(2, DataHeatBalance::HConvIn(2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(20.0, DataHeatBalance::TempEffBulkAir(2), 0.0001);
    // Claculate temperature based on zone temperature with supply flow rate = 0
    DataLoopNode::Node(1).MassFlowRate = 0.0;
    DataLoopNode::Node(2).MassFlowRate = 0.0;
    WindowManager::CalcWindowHeatBalance(2, DataHeatBalance::HConvIn(2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(25.0, DataHeatBalance::TempEffBulkAir(2), 0.0001);

    // Adjacent surface
    DataLoopNode::Node(1).MassFlowRate = 0.1;
    DataLoopNode::Node(2).MassFlowRate = 0.1;
    DataSurfaces::Surface(1).ExtBoundCond = 2;
    WindowManager::CalcWindowHeatBalance(2, DataHeatBalance::HConvIn(2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(20.0, DataHeatBalance::TempEffBulkAir(2), 0.0001);

    DataLoopNode::Node(1).MassFlowRate = 0.0;
    DataLoopNode::Node(2).MassFlowRate = 0.0;
    DataSurfaces::Surface(1).ExtBoundCond = 2;
    DataSurfaces::Surface(2).ExtBoundCond = 1;
    DataSurfaces::Surface(1).TAirRef = DataSurfaces::ZoneSupplyAirTemp;
    WindowManager::CalcWindowHeatBalance(2, DataHeatBalance::HConvIn(2), inSurfTemp, outSurfTemp);
    EXPECT_NEAR(25.0, DataHeatBalance::TempEffBulkAir(2), 0.0001);
}

TEST_F(EnergyPlusFixture, SpectralAngularPropertyTest)
{
    DataIPShortCuts::lAlphaFieldBlanks = true;

    std::string const idf_objects = delimited_string({

        "  Version,9.0;",

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

        "  Output:DebuggingData,0,0;",

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
        "    ,                        !- Name",
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
        "    ,                        !- Name",
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

        "Table:TwoIndependentVariables,",
        "  TransmittanceData,         !- Name",
        "  BiQuadratic,               !- Curve Type",
        "  LinearInterpolationOfTable,!- Interpolation Type",
        "  0.0,                       !- Minimum Value of x,",
        "  90.0,                      !- Maximum Value of x,",
        "  0.30,                      !- Minimum Value of y,",
        "  2.50,                      !- Maximum Value of y,",
        "  0.0,                       !- Minimum Table Output",
        "  1.0,                       !- Maximum Table Output",
        "  Angle,                     !- Input Unit Type for x",
        "  Wavelength,                !- Input Unit Type for y",
        "  Dimensionless,             !- Output Unit Type",
        "  1.0,                       !- Normalization Point",
        "  ,                          !- External File Name",
        " 0.0,  0.300, 0.00100,",
        " 0.0,  0.310, 0.00100,",
        " 0.0,  0.320, 0.00100,",
        " 0.0,  0.330, 0.00100,",
        " 0.0,  0.340, 0.00100,",
        " 0.0,  0.350, 0.00100,",
        " 0.0,  0.360, 0.00900,",
        " 0.0,  0.370, 0.12000,",
        " 0.0,  0.380, 0.49200,",
        " 0.0,  0.390, 0.78200,",
        " 0.0,  0.400, 0.85600,",
        " 0.0,  0.410, 0.85800,",
        " 0.0,  0.420, 0.85800,",
        " 0.0,  0.430, 0.86000,",
        " 0.0,  0.440, 0.86100,",
        " 0.0,  0.450, 0.87100,",
        " 0.0,  0.460, 0.88000,",
        " 0.0,  0.470, 0.88300,",
        " 0.0,  0.480, 0.88700,",
        " 0.0,  0.490, 0.89000,",
        " 0.0,  0.500, 0.89000,",
        " 0.0,  0.510, 0.89100,",
        " 0.0,  0.520, 0.88700,",
        " 0.0,  0.530, 0.89000,",
        " 0.0,  0.540, 0.88300,",
        " 0.0,  0.550, 0.88800,",
        " 0.0,  0.560, 0.88200,",
        " 0.0,  0.570, 0.88100,",
        " 0.0,  0.580, 0.86500,",
        " 0.0,  0.590, 0.85800,",
        " 0.0,  0.600, 0.86500,",
        " 0.0,  0.610, 0.85600,",
        " 0.0,  0.620, 0.84500,",
        " 0.0,  0.630, 0.83700,",
        " 0.0,  0.640, 0.82700,",
        " 0.0,  0.650, 0.82000,",
        " 0.0,  0.660, 0.80700,",
        " 0.0,  0.670, 0.79800,",
        " 0.0,  0.680, 0.79100,",
        " 0.0,  0.690, 0.78100,",
        " 0.0,  0.700, 0.76800,",
        " 0.0,  0.710, 0.76100,",
        " 0.0,  0.720, 0.74400,",
        " 0.0,  0.730, 0.71300,",
        " 0.0,  0.740, 0.70300,",
        " 0.0,  0.750, 0.69400,",
        " 0.0,  0.760, 0.68500,",
        " 0.0,  0.770, 0.67500,",
        " 0.0,  0.780, 0.66700,",
        " 0.0,  0.790, 0.65500,",
        " 0.0,  0.800, 0.64600,",
        " 0.0,  0.810, 0.63800,",
        " 0.0,  0.820, 0.62900,",
        " 0.0,  0.830, 0.62300,",
        " 0.0,  0.840, 0.61400,",
        " 0.0,  0.850, 0.60800,",
        " 0.0,  0.860, 0.60100,",
        " 0.0,  0.870, 0.59700,",
        " 0.0,  0.880, 0.59200,",
        " 0.0,  0.890, 0.58700,",
        " 0.0,  0.900, 0.58200,",
        " 0.0,  0.950, 0.56800,",
        " 0.0,  1.000, 0.56200,",
        " 0.0,  1.050, 0.55600,",
        " 0.0,  1.100, 0.56300,",
        " 0.0,  1.150, 0.55600,",
        " 0.0,  1.200, 0.54700,",
        " 0.0,  1.250, 0.57700,",
        " 0.0,  1.300, 0.59800,",
        " 0.0,  1.350, 0.60800,",
        " 0.0,  1.400, 0.60300,",
        " 0.0,  1.450, 0.61400,",
        " 0.0,  1.500, 0.64800,",
        " 0.0,  1.550, 0.68000,",
        " 0.0,  1.600, 0.69900,",
        " 0.0,  1.650, 0.70600,",
        " 0.0,  1.700, 0.57000,",
        " 0.0,  1.750, 0.58500,",
        " 0.0,  1.800, 0.63700,",
        " 0.0,  1.850, 0.65500,",
        " 0.0,  1.900, 0.63700,",
        " 0.0,  1.950, 0.63400,",
        " 0.0,  2.000, 0.63400,",
        " 0.0,  2.050, 0.58600,",
        " 0.0,  2.100, 0.58800,",
        " 0.0,  2.150, 0.59700,",
        " 0.0,  2.200, 0.57600,",
        " 0.0,  2.250, 0.40400,",
        " 0.0,  2.300, 0.17900,",
        " 0.0,  2.350, 0.21900,",
        " 0.0,  2.400, 0.24000,",
        " 0.0,  2.450, 0.20000,",
        " 0.0,  2.500, 0.21400,",
        "90.0,  0.300, 0.00000,",
        "90.0,  0.310, 0.00000,",
        "90.0,  0.320, 0.00000,",
        "90.0,  0.330, 0.00000,",
        "90.0,  0.340, 0.00000,",
        "90.0,  0.350, 0.00000,",
        "90.0,  0.360, 0.00000,",
        "90.0,  0.370, 0.00000,",
        "90.0,  0.380, 0.00000,",
        "90.0,  0.390, 0.00000,",
        "90.0,  0.400, 0.00000,",
        "90.0,  0.410, 0.00000,",
        "90.0,  0.420, 0.00000,",
        "90.0,  0.430, 0.00000,",
        "90.0,  0.440, 0.00000,",
        "90.0,  0.450, 0.00000,",
        "90.0,  0.460, 0.00000,",
        "90.0,  0.470, 0.00000,",
        "90.0,  0.480, 0.00000,",
        "90.0,  0.490, 0.00000,",
        "90.0,  0.500, 0.00000,",
        "90.0,  0.510, 0.00000,",
        "90.0,  0.520, 0.00000,",
        "90.0,  0.530, 0.00000,",
        "90.0,  0.540, 0.00000,",
        "90.0,  0.550, 0.00000,",
        "90.0,  0.560, 0.00000,",
        "90.0,  0.570, 0.00000,",
        "90.0,  0.580, 0.00000,",
        "90.0,  0.590, 0.00000,",
        "90.0,  0.600, 0.00000,",
        "90.0,  0.610, 0.00000,",
        "90.0,  0.620, 0.00000,",
        "90.0,  0.630, 0.00000,",
        "90.0,  0.640, 0.00000,",
        "90.0,  0.650, 0.00000,",
        "90.0,  0.660, 0.00000,",
        "90.0,  0.670, 0.00000,",
        "90.0,  0.680, 0.00000,",
        "90.0,  0.690, 0.00000,",
        "90.0,  0.700, 0.00000,",
        "90.0,  0.710, 0.00000,",
        "90.0,  0.720, 0.00000,",
        "90.0,  0.730, 0.00000,",
        "90.0,  0.740, 0.00000,",
        "90.0,  0.750, 0.00000,",
        "90.0,  0.760, 0.00000,",
        "90.0,  0.770, 0.00000,",
        "90.0,  0.780, 0.00000,",
        "90.0,  0.790, 0.00000,",
        "90.0,  0.800, 0.00000,",
        "90.0,  0.810, 0.00000,",
        "90.0,  0.820, 0.00000,",
        "90.0,  0.830, 0.00000,",
        "90.0,  0.840, 0.00000,",
        "90.0,  0.850, 0.00000,",
        "90.0,  0.860, 0.00000,",
        "90.0,  0.870, 0.00000,",
        "90.0,  0.880, 0.00000,",
        "90.0,  0.890, 0.00000,",
        "90.0,  0.900, 0.00000,",
        "90.0,  0.950, 0.00000,",
        "90.0,  1.000, 0.00000,",
        "90.0,  1.050, 0.00000,",
        "90.0,  1.100, 0.00000,",
        "90.0,  1.150, 0.00000,",
        "90.0,  1.200, 0.00000,",
        "90.0,  1.250, 0.00000,",
        "90.0,  1.300, 0.00000,",
        "90.0,  1.350, 0.00000,",
        "90.0,  1.400, 0.00000,",
        "90.0,  1.450, 0.00000,",
        "90.0,  1.500, 0.00000,",
        "90.0,  1.550, 0.00000,",
        "90.0,  1.600, 0.00000,",
        "90.0,  1.650, 0.00000,",
        "90.0,  1.700, 0.00000,",
        "90.0,  1.750, 0.00000,",
        "90.0,  1.800, 0.00000,",
        "90.0,  1.850, 0.00000,",
        "90.0,  1.900, 0.00000,",
        "90.0,  1.950, 0.00000,",
        "90.0,  2.000, 0.00000,",
        "90.0,  2.050, 0.00000,",
        "90.0,  2.100, 0.00000,",
        "90.0,  2.150, 0.00000,",
        "90.0,  2.200, 0.00000,",
        "90.0,  2.250, 0.00000,",
        "90.0,  2.300, 0.00000,",
        "90.0,  2.350, 0.00000,",
        "90.0,  2.400, 0.00000,",
        "90.0,  2.450, 0.00000,",
        "90.0,  2.500, 0.00000;",

        "Table:TwoIndependentVariables,",
        "  FrontReflectanceData,      !- Name",
        "  BiQuadratic,               !- Curve Type",
        "  LinearInterpolationOfTable,!- Interpolation Type",
        "  0.0,                       !- Minimum Value of x,",
        "  90.0,                      !- Maximum Value of x,",
        "  0.30,                      !- Minimum Value of y,",
        "  2.50,                      !- Maximum Value of y,",
        "  0.0,                       !- Minimum Table Output",
        "  1.0,                       !- Maximum Table Output",
        "  Angle,                     !- Input Unit Type for x",
        "  Wavelength,                !- Input Unit Type for y",
        "  Dimensionless,             !- Output Unit Type",
        "  1.0,                       !- Normalization Point",
        "  ,                          !- External File Name",
        " 0.0,  0.300, 0.04500,",
        " 0.0,  0.310, 0.04400,",
        " 0.0,  0.320, 0.04400,",
        " 0.0,  0.330, 0.04200,",
        " 0.0,  0.340, 0.04100,",
        " 0.0,  0.350, 0.04000,",
        " 0.0,  0.360, 0.04000,",
        " 0.0,  0.370, 0.04000,",
        " 0.0,  0.380, 0.05100,",
        " 0.0,  0.390, 0.07000,",
        " 0.0,  0.400, 0.07500,",
        " 0.0,  0.410, 0.07500,",
        " 0.0,  0.420, 0.07500,",
        " 0.0,  0.430, 0.07500,",
        " 0.0,  0.440, 0.07500,",
        " 0.0,  0.450, 0.07500,",
        " 0.0,  0.460, 0.07600,",
        " 0.0,  0.470, 0.07500,",
        " 0.0,  0.480, 0.07600,",
        " 0.0,  0.490, 0.07500,",
        " 0.0,  0.500, 0.07500,",
        " 0.0,  0.510, 0.07500,",
        " 0.0,  0.520, 0.07500,",
        " 0.0,  0.530, 0.07500,",
        " 0.0,  0.540, 0.07400,",
        " 0.0,  0.550, 0.07400,",
        " 0.0,  0.560, 0.07400,",
        " 0.0,  0.570, 0.07400,",
        " 0.0,  0.580, 0.07100,",
        " 0.0,  0.590, 0.07000,",
        " 0.0,  0.600, 0.07000,",
        " 0.0,  0.610, 0.07000,",
        " 0.0,  0.620, 0.07000,",
        " 0.0,  0.630, 0.07000,",
        " 0.0,  0.640, 0.06900,",
        " 0.0,  0.650, 0.06700,",
        " 0.0,  0.660, 0.06700,",
        " 0.0,  0.670, 0.06500,",
        " 0.0,  0.680, 0.06500,",
        " 0.0,  0.690, 0.06500,",
        " 0.0,  0.700, 0.06400,",
        " 0.0,  0.710, 0.06400,",
        " 0.0,  0.720, 0.06200,",
        " 0.0,  0.730, 0.06400,",
        " 0.0,  0.740, 0.06200,",
        " 0.0,  0.750, 0.06100,",
        " 0.0,  0.760, 0.06100,",
        " 0.0,  0.770, 0.06000,",
        " 0.0,  0.780, 0.06000,",
        " 0.0,  0.790, 0.06000,",
        " 0.0,  0.800, 0.05900,",
        " 0.0,  0.810, 0.05900,",
        " 0.0,  0.820, 0.05700,",
        " 0.0,  0.830, 0.05700,",
        " 0.0,  0.840, 0.05600,",
        " 0.0,  0.850, 0.05600,",
        " 0.0,  0.860, 0.05500,",
        " 0.0,  0.870, 0.05400,",
        " 0.0,  0.880, 0.05400,",
        " 0.0,  0.890, 0.05400,",
        " 0.0,  0.900, 0.05500,",
        " 0.0,  0.950, 0.05100,",
        " 0.0,  1.000, 0.05100,",
        " 0.0,  1.050, 0.05000,",
        " 0.0,  1.100, 0.05100,",
        " 0.0,  1.150, 0.05000,",
        " 0.0,  1.200, 0.05000,",
        " 0.0,  1.250, 0.05100,",
        " 0.0,  1.300, 0.05400,",
        " 0.0,  1.350, 0.05500,",
        " 0.0,  1.400, 0.05200,",
        " 0.0,  1.450, 0.05500,",
        " 0.0,  1.500, 0.05700,",
        " 0.0,  1.550, 0.05900,",
        " 0.0,  1.600, 0.06000,",
        " 0.0,  1.650, 0.06000,",
        " 0.0,  1.700, 0.05100,",
        " 0.0,  1.750, 0.05100,",
        " 0.0,  1.800, 0.05500,",
        " 0.0,  1.850, 0.05700,",
        " 0.0,  1.900, 0.05700,",
        " 0.0,  1.950, 0.05700,",
        " 0.0,  2.000, 0.05700,",
        " 0.0,  2.050, 0.05200,",
        " 0.0,  2.100, 0.05400,",
        " 0.0,  2.150, 0.05400,",
        " 0.0,  2.200, 0.05100,",
        " 0.0,  2.250, 0.04500,",
        " 0.0,  2.300, 0.03700,",
        " 0.0,  2.350, 0.03700,",
        " 0.0,  2.400, 0.03900,",
        " 0.0,  2.450, 0.04000,",
        " 0.0,  2.500, 0.03900,",
        "90.0,  0.300, 1.00000,",
        "90.0,  0.310, 1.00000,",
        "90.0,  0.320, 1.00000,",
        "90.0,  0.330, 1.00000,",
        "90.0,  0.340, 1.00000,",
        "90.0,  0.350, 1.00000,",
        "90.0,  0.360, 1.00000,",
        "90.0,  0.370, 1.00000,",
        "90.0,  0.380, 1.00000,",
        "90.0,  0.390, 1.00000,",
        "90.0,  0.400, 1.00000,",
        "90.0,  0.410, 1.00000,",
        "90.0,  0.420, 1.00000,",
        "90.0,  0.430, 1.00000,",
        "90.0,  0.440, 1.00000,",
        "90.0,  0.450, 1.00000,",
        "90.0,  0.460, 1.00000,",
        "90.0,  0.470, 1.00000,",
        "90.0,  0.480, 1.00000,",
        "90.0,  0.490, 1.00000,",
        "90.0,  0.500, 1.00000,",
        "90.0,  0.510, 1.00000,",
        "90.0,  0.520, 1.00000,",
        "90.0,  0.530, 1.00000,",
        "90.0,  0.540, 1.00000,",
        "90.0,  0.550, 1.00000,",
        "90.0,  0.560, 1.00000,",
        "90.0,  0.570, 1.00000,",
        "90.0,  0.580, 1.00000,",
        "90.0,  0.590, 1.00000,",
        "90.0,  0.600, 1.00000,",
        "90.0,  0.610, 1.00000,",
        "90.0,  0.620, 1.00000,",
        "90.0,  0.630, 1.00000,",
        "90.0,  0.640, 1.00000,",
        "90.0,  0.650, 1.00000,",
        "90.0,  0.660, 1.00000,",
        "90.0,  0.670, 1.00000,",
        "90.0,  0.680, 1.00000,",
        "90.0,  0.690, 1.00000,",
        "90.0,  0.700, 1.00000,",
        "90.0,  0.710, 1.00000,",
        "90.0,  0.720, 1.00000,",
        "90.0,  0.730, 1.00000,",
        "90.0,  0.740, 1.00000,",
        "90.0,  0.750, 1.00000,",
        "90.0,  0.760, 1.00000,",
        "90.0,  0.770, 1.00000,",
        "90.0,  0.780, 1.00000,",
        "90.0,  0.790, 1.00000,",
        "90.0,  0.800, 1.00000,",
        "90.0,  0.810, 1.00000,",
        "90.0,  0.820, 1.00000,",
        "90.0,  0.830, 1.00000,",
        "90.0,  0.840, 1.00000,",
        "90.0,  0.850, 1.00000,",
        "90.0,  0.860, 1.00000,",
        "90.0,  0.870, 1.00000,",
        "90.0,  0.880, 1.00000,",
        "90.0,  0.890, 1.00000,",
        "90.0,  0.900, 1.00000,",
        "90.0,  0.950, 1.00000,",
        "90.0,  1.000, 1.00000,",
        "90.0,  1.050, 1.00000,",
        "90.0,  1.100, 1.00000,",
        "90.0,  1.150, 1.00000,",
        "90.0,  1.200, 1.00000,",
        "90.0,  1.250, 1.00000,",
        "90.0,  1.300, 1.00000,",
        "90.0,  1.350, 1.00000,",
        "90.0,  1.400, 1.00000,",
        "90.0,  1.450, 1.00000,",
        "90.0,  1.500, 1.00000,",
        "90.0,  1.550, 1.00000,",
        "90.0,  1.600, 1.00000,",
        "90.0,  1.650, 1.00000,",
        "90.0,  1.700, 1.00000,",
        "90.0,  1.750, 1.00000,",
        "90.0,  1.800, 1.00000,",
        "90.0,  1.850, 1.00000,",
        "90.0,  1.900, 1.00000,",
        "90.0,  1.950, 1.00000,",
        "90.0,  2.000, 1.00000,",
        "90.0,  2.050, 1.00000,",
        "90.0,  2.100, 1.00000,",
        "90.0,  2.150, 1.00000,",
        "90.0,  2.200, 1.00000,",
        "90.0,  2.250, 1.00000,",
        "90.0,  2.300, 1.00000,",
        "90.0,  2.350, 1.00000,",
        "90.0,  2.400, 1.00000,",
        "90.0,  2.450, 1.00000,",
        "90.0,  2.500, 1.00000;",

        "Table:TwoIndependentVariables,",
        "  BackRefelectanceData,      !- Name",
        "  BiQuadratic,               !- Curve Type",
        "  LinearInterpolationOfTable,!- Interpolation Type",
        "  0.0,                       !- Minimum Value of x,",
        "  90.0,                      !- Maximum Value of x,",
        "  0.30,                      !- Minimum Value of y,",
        "  2.50,                      !- Maximum Value of y,",
        "  0.0,                       !- Minimum Table Output",
        "  1.0,                       !- Maximum Table Output",
        "  Angle,                     !- Input Unit Type for x",
        "  Wavelength,                !- Input Unit Type for y",
        "  Dimensionless,             !- Output Unit Type",
        "  1.0,                       !- Normalization Point",
        "  ,                          !- External File Name",
        " 0.0,  0.300, 0.04500,",
        " 0.0,  0.310, 0.04400,",
        " 0.0,  0.320, 0.04400,",
        " 0.0,  0.330, 0.04200,",
        " 0.0,  0.340, 0.04100,",
        " 0.0,  0.350, 0.04000,",
        " 0.0,  0.360, 0.04000,",
        " 0.0,  0.370, 0.04000,",
        " 0.0,  0.380, 0.05100,",
        " 0.0,  0.390, 0.07000,",
        " 0.0,  0.400, 0.07500,",
        " 0.0,  0.410, 0.07500,",
        " 0.0,  0.420, 0.07500,",
        " 0.0,  0.430, 0.07500,",
        " 0.0,  0.440, 0.07500,",
        " 0.0,  0.450, 0.07500,",
        " 0.0,  0.460, 0.07600,",
        " 0.0,  0.470, 0.07500,",
        " 0.0,  0.480, 0.07600,",
        " 0.0,  0.490, 0.07500,",
        " 0.0,  0.500, 0.07500,",
        " 0.0,  0.510, 0.07500,",
        " 0.0,  0.520, 0.07500,",
        " 0.0,  0.530, 0.07500,",
        " 0.0,  0.540, 0.07400,",
        " 0.0,  0.550, 0.07400,",
        " 0.0,  0.560, 0.07400,",
        " 0.0,  0.570, 0.07400,",
        " 0.0,  0.580, 0.07100,",
        " 0.0,  0.590, 0.07000,",
        " 0.0,  0.600, 0.07000,",
        " 0.0,  0.610, 0.07000,",
        " 0.0,  0.620, 0.07000,",
        " 0.0,  0.630, 0.07000,",
        " 0.0,  0.640, 0.06900,",
        " 0.0,  0.650, 0.06700,",
        " 0.0,  0.660, 0.06700,",
        " 0.0,  0.670, 0.06500,",
        " 0.0,  0.680, 0.06500,",
        " 0.0,  0.690, 0.06500,",
        " 0.0,  0.700, 0.06400,",
        " 0.0,  0.710, 0.06400,",
        " 0.0,  0.720, 0.06200,",
        " 0.0,  0.730, 0.06400,",
        " 0.0,  0.740, 0.06200,",
        " 0.0,  0.750, 0.06100,",
        " 0.0,  0.760, 0.06100,",
        " 0.0,  0.770, 0.06000,",
        " 0.0,  0.780, 0.06000,",
        " 0.0,  0.790, 0.06000,",
        " 0.0,  0.800, 0.05900,",
        " 0.0,  0.810, 0.05900,",
        " 0.0,  0.820, 0.05700,",
        " 0.0,  0.830, 0.05700,",
        " 0.0,  0.840, 0.05600,",
        " 0.0,  0.850, 0.05600,",
        " 0.0,  0.860, 0.05500,",
        " 0.0,  0.870, 0.05400,",
        " 0.0,  0.880, 0.05400,",
        " 0.0,  0.890, 0.05400,",
        " 0.0,  0.900, 0.05500,",
        " 0.0,  0.950, 0.05100,",
        " 0.0,  1.000, 0.05100,",
        " 0.0,  1.050, 0.05000,",
        " 0.0,  1.100, 0.05100,",
        " 0.0,  1.150, 0.05000,",
        " 0.0,  1.200, 0.05000,",
        " 0.0,  1.250, 0.05100,",
        " 0.0,  1.300, 0.05400,",
        " 0.0,  1.350, 0.05500,",
        " 0.0,  1.400, 0.05200,",
        " 0.0,  1.450, 0.05500,",
        " 0.0,  1.500, 0.05700,",
        " 0.0,  1.550, 0.05900,",
        " 0.0,  1.600, 0.06000,",
        " 0.0,  1.650, 0.06000,",
        " 0.0,  1.700, 0.05100,",
        " 0.0,  1.750, 0.05100,",
        " 0.0,  1.800, 0.05500,",
        " 0.0,  1.850, 0.05700,",
        " 0.0,  1.900, 0.05700,",
        " 0.0,  1.950, 0.05700,",
        " 0.0,  2.000, 0.05700,",
        " 0.0,  2.050, 0.05200,",
        " 0.0,  2.100, 0.05400,",
        " 0.0,  2.150, 0.05400,",
        " 0.0,  2.200, 0.05100,",
        " 0.0,  2.250, 0.04500,",
        " 0.0,  2.300, 0.03700,",
        " 0.0,  2.350, 0.03700,",
        " 0.0,  2.400, 0.03900,",
        " 0.0,  2.450, 0.04000,",
        " 0.0,  2.500, 0.03900,",
        "90.0,  0.300, 1.00000,",
        "90.0,  0.310, 1.00000,",
        "90.0,  0.320, 1.00000,",
        "90.0,  0.330, 1.00000,",
        "90.0,  0.340, 1.00000,",
        "90.0,  0.350, 1.00000,",
        "90.0,  0.360, 1.00000,",
        "90.0,  0.370, 1.00000,",
        "90.0,  0.380, 1.00000,",
        "90.0,  0.390, 1.00000,",
        "90.0,  0.400, 1.00000,",
        "90.0,  0.410, 1.00000,",
        "90.0,  0.420, 1.00000,",
        "90.0,  0.430, 1.00000,",
        "90.0,  0.440, 1.00000,",
        "90.0,  0.450, 1.00000,",
        "90.0,  0.460, 1.00000,",
        "90.0,  0.470, 1.00000,",
        "90.0,  0.480, 1.00000,",
        "90.0,  0.490, 1.00000,",
        "90.0,  0.500, 1.00000,",
        "90.0,  0.510, 1.00000,",
        "90.0,  0.520, 1.00000,",
        "90.0,  0.530, 1.00000,",
        "90.0,  0.540, 1.00000,",
        "90.0,  0.550, 1.00000,",
        "90.0,  0.560, 1.00000,",
        "90.0,  0.570, 1.00000,",
        "90.0,  0.580, 1.00000,",
        "90.0,  0.590, 1.00000,",
        "90.0,  0.600, 1.00000,",
        "90.0,  0.610, 1.00000,",
        "90.0,  0.620, 1.00000,",
        "90.0,  0.630, 1.00000,",
        "90.0,  0.640, 1.00000,",
        "90.0,  0.650, 1.00000,",
        "90.0,  0.660, 1.00000,",
        "90.0,  0.670, 1.00000,",
        "90.0,  0.680, 1.00000,",
        "90.0,  0.690, 1.00000,",
        "90.0,  0.700, 1.00000,",
        "90.0,  0.710, 1.00000,",
        "90.0,  0.720, 1.00000,",
        "90.0,  0.730, 1.00000,",
        "90.0,  0.740, 1.00000,",
        "90.0,  0.750, 1.00000,",
        "90.0,  0.760, 1.00000,",
        "90.0,  0.770, 1.00000,",
        "90.0,  0.780, 1.00000,",
        "90.0,  0.790, 1.00000,",
        "90.0,  0.800, 1.00000,",
        "90.0,  0.810, 1.00000,",
        "90.0,  0.820, 1.00000,",
        "90.0,  0.830, 1.00000,",
        "90.0,  0.840, 1.00000,",
        "90.0,  0.850, 1.00000,",
        "90.0,  0.860, 1.00000,",
        "90.0,  0.870, 1.00000,",
        "90.0,  0.880, 1.00000,",
        "90.0,  0.890, 1.00000,",
        "90.0,  0.900, 1.00000,",
        "90.0,  0.950, 1.00000,",
        "90.0,  1.000, 1.00000,",
        "90.0,  1.050, 1.00000,",
        "90.0,  1.100, 1.00000,",
        "90.0,  1.150, 1.00000,",
        "90.0,  1.200, 1.00000,",
        "90.0,  1.250, 1.00000,",
        "90.0,  1.300, 1.00000,",
        "90.0,  1.350, 1.00000,",
        "90.0,  1.400, 1.00000,",
        "90.0,  1.450, 1.00000,",
        "90.0,  1.500, 1.00000,",
        "90.0,  1.550, 1.00000,",
        "90.0,  1.600, 1.00000,",
        "90.0,  1.650, 1.00000,",
        "90.0,  1.700, 1.00000,",
        "90.0,  1.750, 1.00000,",
        "90.0,  1.800, 1.00000,",
        "90.0,  1.850, 1.00000,",
        "90.0,  1.900, 1.00000,",
        "90.0,  1.950, 1.00000,",
        "90.0,  2.000, 1.00000,",
        "90.0,  2.050, 1.00000,",
        "90.0,  2.100, 1.00000,",
        "90.0,  2.150, 1.00000,",
        "90.0,  2.200, 1.00000,",
        "90.0,  2.250, 1.00000,",
        "90.0,  2.300, 1.00000,",
        "90.0,  2.350, 1.00000,",
        "90.0,  2.400, 1.00000,",
        "90.0,  2.450, 1.00000,",
        "90.0,  2.500, 1.00000;",

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

    SimulationManager::GetProjectData();
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(FoundError); // read project control data
    EXPECT_FALSE(FoundError);                              // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters();
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;

    HeatBalanceManager::GetWindowGlassSpectralData(FoundError);
    EXPECT_FALSE(FoundError);
    HeatBalanceManager::GetMaterialData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetFrameAndDividerData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetConstructData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetZoneData(FoundError); // Read Zone data from input file
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::GetGeometryParameters(FoundError);
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::CosZoneRelNorth.allocate(4);
    SurfaceGeometry::SinZoneRelNorth.allocate(4);

    SurfaceGeometry::CosZoneRelNorth(1) = std::cos(-DataHeatBalance::Zone(1).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::CosZoneRelNorth(2) = std::cos(-DataHeatBalance::Zone(2).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::CosZoneRelNorth(3) = std::cos(-DataHeatBalance::Zone(3).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::CosZoneRelNorth(4) = std::cos(-DataHeatBalance::Zone(4).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(1) = std::sin(-DataHeatBalance::Zone(1).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(2) = std::sin(-DataHeatBalance::Zone(2).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(3) = std::sin(-DataHeatBalance::Zone(3).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(4) = std::sin(-DataHeatBalance::Zone(4).RelNorth * DataGlobals::DegToRadians);

    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;

    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                    // expect no errors

    WindowManager::InitGlassOpticalCalculations();

    int NumAngles = 10; // Number of incident angles
    Real64 sum;
    // total transmittance
    Array1D<Real64> correctT(
        NumAngles, {0.529017128, 0.472866571, 0.370790548, 0.248928459, 0.138553744, 0.061213244, 0.020072976, 0.00430128, 0.00042861, 0.0});
    // total reflectance
    Array1D<Real64> correctR(
        NumAngles, {0.097222311, 0.194253146, 0.36875691, 0.57565985, 0.762546964, 0.89393376, 0.964537901, 0.99210584, 0.99912202, 1.00000000});
    // Layer 1 absortance
    Array1D<Real64> correctabs1(
        NumAngles, {0.242079608, 0.214464137, 0.165811001, 0.109778385, 0.060620181, 0.02682869, 0.008920102, 0.001979289, 0.000219736, 0.0});
    // Layer 2 absortance
    Array1D<Real64> correctabs2(
        NumAngles, {0.131680954, 0.118416146, 0.094641541, 0.065633305, 0.03827911, 0.018024306, 0.006469021, 0.001613591, 0.000229628, 0.0});

    for (int i = 1; i <= NumAngles; i++) {
        EXPECT_NEAR(correctT(i), WindowManager::tsolPhi(i), 0.0001);
        EXPECT_NEAR(correctR(i), WindowManager::rfsolPhi(i), 0.0001);
        EXPECT_NEAR(correctabs1(i), WindowManager::solabsPhi(1, i), 0.0001);
        EXPECT_NEAR(correctabs2(i), WindowManager::solabsPhi(2, i), 0.0001);
        sum = tsolPhi(i) + rfsolPhi(i) + solabsPhi(1, i) + solabsPhi(2, i);
        EXPECT_NEAR(sum, 1.0, 0.0001);
    }

    SurfaceGeometry::CosZoneRelNorth.deallocate();
    SurfaceGeometry::SinZoneRelNorth.deallocate();
}

TEST_F(EnergyPlusFixture, WindowManager_SrdLWRTest)
{
    // GitHub issue 6037
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Version,8.4;",
                          "Material,",
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
                          "BuildingSurface:Detailed,"
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
    ScheduleManager::ProcessScheduleInput();
    DataHeatBalance::ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject();
    HeatBalanceManager::SetPreConstructionInputParameters();
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    HeatBalanceManager::GetFrameAndDividerData(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    HeatBalanceManager::GetBuildingData(ErrorsFound);

    EXPECT_TRUE(DataGlobals::AnyLocalEnvironmentsInModel);

    Psychrometrics::InitializePsychRoutines();

    DataGlobals::TimeStep = 1;
    DataGlobals::TimeStepZone = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::BeginSimFlag = true;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::OutBaroPress = 100000;

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "Zone";
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);

    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 2;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(2);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 1;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(2) = 2;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 3;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 4;

    DataLoopNode::Node.allocate(4);
    DataHeatBalance::TempEffBulkAir.allocate(3);
    DataHeatBalSurface::TempSurfInTmp.allocate(3);

    DataSurfaces::Surface(1).HeatTransSurf = true;
    DataSurfaces::Surface(2).HeatTransSurf = true;
    DataSurfaces::Surface(3).HeatTransSurf = true;
    DataSurfaces::Surface(1).Area = 10.0;
    DataSurfaces::Surface(2).Area = 10.0;
    DataSurfaces::Surface(3).Area = 10.0;
    DataSurfaces::Surface(1).TAirRef = DataSurfaces::ZoneMeanAirTemp;
    DataSurfaces::Surface(2).TAirRef = DataSurfaces::ZoneSupplyAirTemp;
    DataSurfaces::Surface(3).TAirRef = DataSurfaces::AdjacentAirTemp;
    DataHeatBalSurface::TempSurfInTmp(1) = 15.0;
    DataHeatBalSurface::TempSurfInTmp(2) = 20.0;
    DataHeatBalSurface::TempSurfInTmp(3) = 25.0;
    DataHeatBalance::TempEffBulkAir(1) = 10.0;
    DataHeatBalance::TempEffBulkAir(2) = 10.0;
    DataHeatBalance::TempEffBulkAir(3) = 10.0;

    DataLoopNode::Node(1).Temp = 20.0;
    DataLoopNode::Node(2).Temp = 20.0;
    DataLoopNode::Node(3).Temp = 20.0;
    DataLoopNode::Node(4).Temp = 20.0;
    DataLoopNode::Node(1).MassFlowRate = 0.1;
    DataLoopNode::Node(2).MassFlowRate = 0.1;
    DataLoopNode::Node(3).MassFlowRate = 0.1;
    DataLoopNode::Node(4).MassFlowRate = 0.1;

    DataHeatBalance::HConvIn.allocate(3);
    DataHeatBalance::HConvIn(1) = 0.5;
    DataHeatBalance::HConvIn(2) = 0.5;
    DataHeatBalance::HConvIn(3) = 0.5;
    DataHeatBalance::Zone(1).IsControlled = true;
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.011;
    DataHeatBalFanSys::ZoneAirHumRatAvg.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRatAvg(1) = DataHeatBalFanSys::ZoneAirHumRat(1) = 0.011;

    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 25.0;
    DataHeatBalFanSys::QHTRadSysSurf.allocate(3);
    DataHeatBalFanSys::QHWBaseboardSurf.allocate(3);
    DataHeatBalFanSys::QSteamBaseboardSurf.allocate(3);
    DataHeatBalFanSys::QElecBaseboardSurf.allocate(3);
    DataHeatBalance::QRadSWwinAbs.allocate(1, 3);
    DataHeatBalance::QRadThermInAbs.allocate(3);
    DataHeatBalance::QRadSWOutIncident.allocate(3);
    DataSurfaces::WinTransSolar.allocate(3);
    DataHeatBalance::ZoneWinHeatGain.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRep.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRepEnergy.allocate(1);
    DataSurfaces::WinHeatGain.allocate(3);
    DataSurfaces::WinHeatTransfer.allocate(3);
    DataSurfaces::WinGainConvGlazToZoneRep.allocate(3);
    DataSurfaces::WinGainIRGlazToZoneRep.allocate(3);
    DataSurfaces::WinGapConvHtFlowRep.allocate(3);
    DataSurfaces::WinGapConvHtFlowRepEnergy.allocate(3);
    DataHeatBalance::QS.allocate(1);
    DataSurfaces::WinLossSWZoneToOutWinRep.allocate(3);
    DataSurfaces::WinSysSolTransmittance.allocate(3);
    DataSurfaces::WinSysSolAbsorptance.allocate(3);
    DataSurfaces::WinSysSolReflectance.allocate(3);
    DataSurfaces::InsideGlassCondensationFlag.allocate(3);
    DataSurfaces::WinGainFrameDividerToZoneRep.allocate(3);
    DataSurfaces::InsideFrameCondensationFlag.allocate(3);
    DataSurfaces::InsideDividerCondensationFlag.allocate(3);

    DataHeatBalSurface::QdotConvOutRep.allocate(3);
    DataHeatBalSurface::QdotConvOutRepPerArea.allocate(3);
    DataHeatBalSurface::QConvOutReport.allocate(3);
    DataHeatBalSurface::QdotRadOutRep.allocate(3);
    DataHeatBalSurface::QdotRadOutRepPerArea.allocate(3);
    DataHeatBalSurface::QRadOutReport.allocate(3);
    DataHeatBalSurface::QRadLWOutSrdSurfs.allocate(3);

    DataHeatBalance::QRadSWOutIncident = 0.0;
    DataHeatBalance::QRadSWwinAbs = 0.0;
    DataHeatBalance::QRadThermInAbs = 0.0;

    DataHeatBalFanSys::QHTRadSysSurf = 0.0;
    DataHeatBalFanSys::QHWBaseboardSurf = 0.0;
    DataHeatBalFanSys::QSteamBaseboardSurf = 0.0;
    DataHeatBalFanSys::QElecBaseboardSurf = 0.0;
    DataSurfaces::WinTransSolar = 0.0;
    DataHeatBalance::QS = 0.0;

    Real64 inSurfTemp;
    Real64 outSurfTemp;
    Real64 const StefanBoltzmann(5.6697E-8);
    Real64 const KelvinConv(273.15);
    ScheduleManager::Schedule(1).CurrentValue = 25.0; // Srd Srfs Temp
    // Claculate temperature based on supply flow rate

    WindowManager::CalcWindowHeatBalance(2, DataHeatBalance::HConvIn(2), inSurfTemp, outSurfTemp);
    // Test if LWR from surrounding surfaces correctly calculated
    EXPECT_DOUBLE_EQ(StefanBoltzmann * 0.84 * 0.6 * (pow_4(25.0 + KelvinConv) - pow_4(thetas(1))), DataHeatBalSurface::QRadLWOutSrdSurfs(2));
}
