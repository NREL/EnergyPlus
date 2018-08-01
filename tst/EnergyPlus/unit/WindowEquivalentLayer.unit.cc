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

// EnergyPlus::WindowEquivalentLayer unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>

#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingManager.hh>

#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/WeatherManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ScheduleManager;

using namespace EnergyPlus::WindowEquivalentLayer;

TEST_F(EnergyPlusFixture, WindowEquivalentLayer_GetInput)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,9.0;",

        "  Construction:WindowEquivalentLayer,",
        "  CLR CLR VB,                !- Name",
        "  GLZCLR,                    !- Outside Layer",
        "  Air GAP SealedOut 20mm,    !- Layer 2",
        "  GLZCLR,                    !- Layer 3",
        "  Air GAP SealedIndoor 20mm, !- Layer 4",
        "  VBU8D6+45SW1;              !- Layer 5",

        "WindowMaterial:Glazing:EquivalentLayer,",
        "  GLZCLR,                    !-  Name",
        "  SpectralAverage,           !-  Optical Data Type",
        "  ,                          !-  Window Glass Spectral Data Set Name",
        "  0.83,                      !-  Front Side Beam-Beam Solar Transmittance",
        "  0.83,                      !-  Back Side Beam-Beam Solar Transmittance",
        "  0.08,                      !-  Front Side Beam-Beam Solar Reflectance",
        "  0.08,                      !-  Back Side Beam-Beam Solar Reflectance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Reflectance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Reflectance",
        "  0.0,                       !-  Front Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !-  Back Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !-  Front Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !-  Back Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !-  Front Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !-  Back Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !-  Front Side Beam-Diffuse Visible Reflectance",
        "  0.0,                       !-  Back Side Beam-Diffuse Visible Reflectance",
        "  0.76,                      !-  Diffuse-Diffuse Solar Transmittance",
        "  0.14,                      !-  Front Side Diffuse-Diffuse Solar Reflectance",
        "  0.14,                      !-  Back Side Diffuse-Diffuse Solar Reflectance",
        "  0.0,                       !-  Diffuse-Diffuse Visible Transmittance",
        "  0.0,                       !-  Front Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Back Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Infrared Transmittance (front and back)",
        "  0.84,                      !-  Front Side Infrared Emissivity",
        "  0.84;                      !-  Back Side Infrared Emissivity",

        "WindowMaterial:Blind:EquivalentLayer,",
        "  VBU8D6+45SW1,           ! - Name",
        "  Horizontal,             ! - Slat Orientation",
        "  0.025,                  ! - Slat Width",
        "  0.025,                  ! - Slat Separation",
        "  0.0,                    ! - Slat Crown",
        "  45.0,                   ! - Slat Angle",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Solar Transmittance",
        "  0.80,                   ! - Front Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.60,                   ! - Back Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Visible Transmittance",
        "  0.0,                    ! - Front Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Back Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Slat Infrared Transmittance",
        "  0.90,                   ! - Front Side Slat Infrared Emissivity",
        "  0.90,                   ! - Back Side Slat Infrared Emissivity",
        "  BlockBeamSolar;         ! - Slat Angle Control",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedOut 20mm,    !- Name",
        "  Air,                       !- Gas Type",
        "  0.0200,                    !- Thickness",
        "  Sealed;                    !- Gap Vent Type",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedIndoor 20mm, !- Name",
        "  Air,                       !- Gas Type",
        "  0.020,                     !- Thickness",
        "  Sealed;                    !- Gap Vent Type ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    HeatBalanceManager::GetMaterialData(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);

    int VBMatNum(0);
    for (int i = 1; i <= 4; i++) {
        if (DataHeatBalance::Material(i).Group == DataHeatBalance::BlindEquivalentLayer) {
            VBMatNum = i;
            break;
        }
    }
    EXPECT_EQ(1, DataHeatBalance::TotBlindsEQL);
    EXPECT_EQ(DataHeatBalance::Material(VBMatNum).Group, DataHeatBalance::BlindEquivalentLayer);
    EXPECT_EQ(DataHeatBalance::Material(VBMatNum).SlatAngleType, WindowEquivalentLayer::lscVBNOBM);

    int ConstrNum = 1;
    int EQLNum = 0;
    InitEquivalentLayerWindowCalculations();
    EQLNum = DataHeatBalance::Construct(ConstrNum).EQLConsPtr;
    EXPECT_EQ(CFS(EQLNum).L(CFS(EQLNum).VBLayerPtr).CNTRL, WindowEquivalentLayer::lscVBNOBM);
}

TEST_F(EnergyPlusFixture, WindowEquivalentLayer_VBMaximizeBeamSolar)
{
    // GitHub issue 5750
    int SurfNum(0);
    int VBMatNum(0);
    Real64 ProfAngVer(0);
    static Array2D<Real64> AbsSolBeam(2, CFSMAXNL + 1);

    std::string const idf_objects = delimited_string({

        "  Version,9.0;",

        "  Timestep,1;",

        "  Building,",
        "    Simple One Zone w Windows,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.004,                   !- Temperature Convergence Tolerance Value {deltaC}",
        "    MinimalShadowing,        !- Solar Distribution",
        "    30,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  SizingPeriod:DesignDay,",
        "    Denver Stapleton Intl Arpt Ann Clg 1% Condns DB=>MWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    32.6,                    !- Maximum Dry-Bulb Temperature {C}",
        "    15.2,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    15.6,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    83411.,                  !- Barometric Pressure {Pa}",
        "    4,                       !- Wind Speed {m/s}",
        "    120,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.00;                    !- Sky Clearness",

        "  Site:Location,",
        "    Denver Stapleton Intl Arpt CO USA WMO=724690,  !- Name",
        "    39.77,                   !- Latitude {deg}",
        "    -104.87,                 !- Longitude {deg}",
        "    -7.00,                   !- Time Zone {hr}",
        "    1611.00;                 !- Elevation {m}",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "    FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Site:GroundTemperature:BuildingSurface,18.89,18.92,19.02,19.12,19.21,19.23,19.07,19.32,19.09,19.21,19.13,18.96;",

        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,            !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    CLR CLR VB,              !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,       !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,       !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,       !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;       !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;            !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,   !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,   !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;   !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572, !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572, !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572, !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572; !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction:WindowEquivalentLayer,",
        "  CLR CLR VB,                !- Name",
        "  GLZCLR,                    !- Outside Layer",
        "  Air GAP SealedOut 20mm,    !- Layer 2",
        "  GLZCLR,                    !- Layer 3",
        "  Air GAP SealedIndoor 20mm, !- Layer 4",
        "  VBU8D6+45SW1;              !- Layer 5",

        "WindowMaterial:Glazing:EquivalentLayer,",
        "  GLZCLR,                    !-  Name",
        "  SpectralAverage,           !-  Optical Data Type",
        "  ,                          !-  Window Glass Spectral Data Set Name",
        "  0.83,                      !-  Front Side Beam-Beam Solar Transmittance",
        "  0.83,                      !-  Back Side Beam-Beam Solar Transmittance",
        "  0.08,                      !-  Front Side Beam-Beam Solar Reflectance",
        "  0.08,                      !-  Back Side Beam-Beam Solar Reflectance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Reflectance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Reflectance",
        "  0.0,                       !- Front Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !- Back Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !- Front Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !- Back Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !- Front Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !- Back Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !- Front Side Beam-Diffuse Visible Reflectance",
        "  0.0,                       !- Back Side Beam-Diffuse Visible Reflectance",
        "  0.76,                      !-  Diffuse-Diffuse Solar Transmittance",
        "  0.14,                      !-  Front Side Diffuse-Diffuse Solar Reflectance",
        "  0.14,                      !-  Back Side Diffuse-Diffuse Solar Reflectance",
        "  0.0,                       !-  Diffuse-Diffuse Visible Transmittance",
        "  0.0,                       !-  Front Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Back Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Infrared Transmittance (front and back)",
        "  0.84,                      !-  Front Side Infrared Emissivity",
        "  0.84;                      !-  Back Side Infrared Emissivity",

        "WindowMaterial:Blind:EquivalentLayer,",
        "  VBU8D6+45SW1,           ! - Name",
        "  Horizontal,             ! - Slat Orientation",
        "  0.025,                  ! - Slat Width",
        "  0.025,                  ! - Slat Separation",
        "  0.0,                    ! - Slat Crown",
        "  45.0,                   ! - Slat Angle",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Solar Transmittance",
        "  0.80,                   ! - Front Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.60,                   ! - Back Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Visible Transmittance",
        "  0.0,                    ! - Front Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Back Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Slat Infrared Transmittance",
        "  0.90,                   ! - Front Side Slat Infrared Emissivity",
        "  0.90,                   ! - Back Side Slat Infrared Emissivity",
        "  MaximizeSolar;          ! - Slat Angle Control",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedOut 20mm,    !- Name",
        "  Air,                       !- Gas Type",
        "  0.0200,                    !- Thickness",
        "  Sealed;                    !- Gap Vent Type",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedIndoor 20mm, !- Name",
        "  Air,                       !- Gas Type",
        "  0.020,                     !- Thickness",
        "  Sealed;                    !- Gap Vent Type ",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2); //
    SimulationManager::ManageSimulation();
    // re-set the hour of the day to mide day
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 12;
    DataGlobals::CurrentTime = 12.0;
    WeatherManager::DetermineSunUpDown(DataEnvironment::SOLCOS);
    // get window surface index
    for (int iSurf = 1; iSurf <= DataSurfaces::TotSurfaces; iSurf++) {
        if (DataSurfaces::SurfaceWindow(iSurf).WindowModelType == DataSurfaces::WindowEQLModel) {
            SurfNum = iSurf;
            break;
        }
    }
    // get venetian blind material index
    for (int i = 1; i <= 7; i++) {
        if (DataHeatBalance::Material(i).Group == DataHeatBalance::BlindEquivalentLayer) {
            VBMatNum = i;
            break;
        }
    }
    // get equivalent layer window optical properties
    CalcEQLOpticalProperty(SurfNum, DataWindowEquivalentLayer::isBEAM, AbsSolBeam);
    // check that the slat angle control type is set to MaximizeSolar
    EXPECT_EQ(DataHeatBalance::Material(VBMatNum).SlatAngleType, WindowEquivalentLayer::lscVBPROF);
    // check the slat angle
    EXPECT_NEAR(-71.0772, DataSurfaces::SurfaceWindow(SurfNum).SlatAngThisTSDeg, 0.0001);
    // check that for MaximizeSolar slat angle control, the slat angle = -ve vertical profile angle
    DaylightingManager::ProfileAngle(SurfNum, DataEnvironment::SOLCOS, DataHeatBalance::Horizontal, ProfAngVer);
    EXPECT_NEAR(-DataGlobals::RadToDeg * ProfAngVer, DataSurfaces::SurfaceWindow(SurfNum).SlatAngThisTSDeg, 0.0001);
}

TEST_F(EnergyPlusFixture, WindowEquivalentLayer_VBBlockBeamSolar)
{
    // GitHub issue 5750
    int SurfNum(0);
    int VBMatNum(0);
    Real64 ProfAngVer(0);
    static Array2D<Real64> AbsSolBeam(2, CFSMAXNL + 1);

    std::string const idf_objects = delimited_string({

        "  Version,9.0;",

        "  Timestep,1;",

        "  Building,",
        "    Simple One Zone w Windows,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.004,                   !- Temperature Convergence Tolerance Value {deltaC}",
        "    MinimalShadowing,        !- Solar Distribution",
        "    30,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  SizingPeriod:DesignDay,",
        "    Denver Stapleton Intl Arpt Ann Clg 1% Condns DB=>MWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    32.6,                    !- Maximum Dry-Bulb Temperature {C}",
        "    15.2,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    15.6,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    83411.,                  !- Barometric Pressure {Pa}",
        "    4,                       !- Wind Speed {m/s}",
        "    120,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.00;                    !- Sky Clearness",

        "  Site:Location,",
        "    Denver Stapleton Intl Arpt CO USA WMO=724690,  !- Name",
        "    39.77,                   !- Latitude {deg}",
        "    -104.87,                 !- Longitude {deg}",
        "    -7.00,                   !- Time Zone {hr}",
        "    1611.00;                 !- Elevation {m}",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "    FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Site:GroundTemperature:BuildingSurface,18.89,18.92,19.02,19.12,19.21,19.23,19.07,19.32,19.09,19.21,19.13,18.96;",

        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,            !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    CLR CLR VB,              !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,       !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,       !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,       !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;       !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;            !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,   !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,   !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;   !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572, !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572, !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572, !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572; !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction:WindowEquivalentLayer,",
        "  CLR CLR VB,                !- Name",
        "  GLZCLR,                    !- Outside Layer",
        "  Air GAP SealedOut 20mm,    !- Layer 2",
        "  GLZCLR,                    !- Layer 3",
        "  Air GAP SealedIndoor 20mm, !- Layer 4",
        "  VBU8D6+45SW1;              !- Layer 5",

        "WindowMaterial:Glazing:EquivalentLayer,",
        "  GLZCLR,                    !-  Name",
        "  SpectralAverage,           !-  Optical Data Type",
        "  ,                          !-  Window Glass Spectral Data Set Name",
        "  0.83,                      !-  Front Side Beam-Beam Solar Transmittance",
        "  0.83,                      !-  Back Side Beam-Beam Solar Transmittance",
        "  0.08,                      !-  Front Side Beam-Beam Solar Reflectance",
        "  0.08,                      !-  Back Side Beam-Beam Solar Reflectance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Transmittance",
        "  0.0,                       !-  Front Side Beam-Beam Visible Reflectance",
        "  0.0,                       !-  Back Side Beam-Beam Visible Reflectance",
        "  0.0,                       !- Front Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !- Back Side Beam-Diffuse Solar Transmittance",
        "  0.0,                       !- Front Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !- Back Side Beam-Diffuse Solar Reflectance",
        "  0.0,                       !- Front Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !- Back Side Beam-Diffuse Visible Transmittance",
        "  0.0,                       !- Front Side Beam-Diffuse Visible Reflectance",
        "  0.0,                       !- Back Side Beam-Diffuse Visible Reflectance",
        "  0.76,                      !-  Diffuse-Diffuse Solar Transmittance",
        "  0.14,                      !-  Front Side Diffuse-Diffuse Solar Reflectance",
        "  0.14,                      !-  Back Side Diffuse-Diffuse Solar Reflectance",
        "  0.0,                       !-  Diffuse-Diffuse Visible Transmittance",
        "  0.0,                       !-  Front Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Back Side Diffuse-Diffuse Visible Reflectance",
        "  0.0,                       !-  Infrared Transmittance (front and back)",
        "  0.84,                      !-  Front Side Infrared Emissivity",
        "  0.84;                      !-  Back Side Infrared Emissivity",

        "WindowMaterial:Blind:EquivalentLayer,",
        "  VBU8D6+45SW1,           ! - Name",
        "  Horizontal,             ! - Slat Orientation",
        "  0.025,                  ! - Slat Width",
        "  0.025,                  ! - Slat Separation",
        "  0.0,                    ! - Slat Crown",
        "  45.0,                   ! - Slat Angle",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Reflectance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Transmittance",
        "  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Solar Transmittance",
        "  0.80,                   ! - Front Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.60,                   ! - Back Side Slat Diffuse-Diffuse Solar Reflectance",
        "  0.0,                    ! - Slat Diffuse-Diffuse Visible Transmittance",
        "  0.0,                    ! - Front Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Back Side Slat Diffuse-Diffuse Visible Reflectance",
        "  0.0,                    ! - Slat Infrared Transmittance",
        "  0.90,                   ! - Front Side Slat Infrared Emissivity",
        "  0.90,                   ! - Back Side Slat Infrared Emissivity",
        "  BlockBeamSolar;         ! - Slat Angle Control",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedOut 20mm,    !- Name",
        "  Air,                       !- Gas Type",
        "  0.0200,                    !- Thickness",
        "  Sealed;                    !- Gap Vent Type",

        " WindowMaterial:Gap:EquivalentLayer,",
        "  Air GAP SealedIndoor 20mm, !- Name",
        "  Air,                       !- Gas Type",
        "  0.020,                     !- Thickness",
        "  Sealed;                    !- Gap Vent Type ",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2);
    SimulationManager::ManageSimulation();
    // re-set the hour of the day to noon
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 12;
    DataGlobals::CurrentTime = 12.0;
    WeatherManager::DetermineSunUpDown(DataEnvironment::SOLCOS);
    // get equivalent layer window surface index
    for (int iSurf = 1; iSurf <= DataSurfaces::TotSurfaces; iSurf++) {
        if (DataSurfaces::SurfaceWindow(iSurf).WindowModelType == DataSurfaces::WindowEQLModel) {
            SurfNum = iSurf;
            break;
        }
    }
    // get venetian blind material index
    for (int i = 1; i <= 7; i++) {
        if (DataHeatBalance::Material(i).Group == DataHeatBalance::BlindEquivalentLayer) {
            VBMatNum = i;
            break;
        }
    }
    // calc window optical property
    CalcEQLOpticalProperty(SurfNum, DataWindowEquivalentLayer::isBEAM, AbsSolBeam);
    // check VB slat angle for BlockBeamSolar slat angle control
    EXPECT_EQ(DataHeatBalance::Material(VBMatNum).SlatAngleType, WindowEquivalentLayer::lscVBNOBM);
    // check the VB slat angle
    EXPECT_NEAR(18.9228, DataSurfaces::SurfaceWindow(SurfNum).SlatAngThisTSDeg, 0.0001);
    // check that for BlockBeamSolar slat angle control, the slat angle = 90 - ProfAngVer
    DaylightingManager::ProfileAngle(SurfNum, DataEnvironment::SOLCOS, DataHeatBalance::Horizontal, ProfAngVer);
    EXPECT_NEAR(90.0 - DataGlobals::RadToDeg * ProfAngVer, DataSurfaces::SurfaceWindow(SurfNum).SlatAngThisTSDeg, 0.0001);
    // get the slat angle from profile angle
    Real64 SlateAngleBlockBeamSolar = VB_CriticalSlatAngle(DataGlobals::RadToDeg * ProfAngVer);
    EXPECT_NEAR(SlateAngleBlockBeamSolar, DataSurfaces::SurfaceWindow(SurfNum).SlatAngThisTSDeg, 0.0001);
}
