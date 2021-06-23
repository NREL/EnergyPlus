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

// EnergyPlus::SolarShading Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataShadowingCombinations.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SolarShading;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSystemVariables;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataBSDFWindow;
using namespace EnergyPlus::DataVectorTypes;
using namespace EnergyPlus::DataShadowingCombinations;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, SolarShadingTest_CalcPerSolarBeamTest)
{
    // Test inits for integrated and non-integrated shading calcs

    //	static bool ErrorsFound( false ); // If errors detected in input
    //	static int ZoneNum( 0 ); // Zone number
    //	int NumAlphas( 2 );
    //	int NumNumbers( 9 );
    Real64 AvgEqOfTime(0.0);       // Average value of Equation of Time for period
    Real64 AvgSinSolarDeclin(1.0); // Average value of Sine of Solar Declination for period
    Real64 AvgCosSolarDeclin(0.0); // Average value of Cosine of Solar Declination for period
    int NumTimeSteps(6);

    state->dataGlobal->TimeStep = 1;
    state->dataSurface->TotSurfaces = 3;
    state->dataBSDFWindow->MaxBkSurf = 3;
    state->dataSurface->SurfaceWindow.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBal->SunlitFracHR.allocate(24, state->dataSurface->TotSurfaces);
    state->dataHeatBal->SunlitFrac.allocate(NumTimeSteps, 24, state->dataSurface->TotSurfaces);
    state->dataHeatBal->SunlitFracWithoutReveal.allocate(NumTimeSteps, 24, state->dataSurface->TotSurfaces);
    state->dataSolarShading->CTHETA.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBal->CosIncAngHR.allocate(24, state->dataSurface->TotSurfaces);
    state->dataHeatBal->CosIncAng.allocate(NumTimeSteps, 24, state->dataSurface->TotSurfaces);
    state->dataSurface->SurfOpaqAO.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBal->BackSurfaces.allocate(NumTimeSteps, 24, state->dataBSDFWindow->MaxBkSurf, state->dataSurface->TotSurfaces);
    state->dataHeatBal->OverlapAreas.allocate(NumTimeSteps, 24, state->dataBSDFWindow->MaxBkSurf, state->dataSurface->TotSurfaces);
    state->dataSurface->SurfSunCosHourly.dimension(24, 3, 0.0);

    // Test non-integrated option first, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 for all hours
    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            state->dataSurface->SurfaceWindow(SurfNum).OutProjSLFracMult(Hour) = 999.0;
            state->dataSurface->SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour) = 888.0;
        }
    }

    state->dataSysVars->DetailedSolarTimestepIntegration = false;
    CalcPerSolarBeam(*state, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);

    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            EXPECT_EQ(1.0, state->dataSurface->SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
            EXPECT_EQ(1.0, state->dataSurface->SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
        }
    }

    // Test integrated option, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 only for the specified hour
    // Re-initialize to new values
    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            state->dataSurface->SurfaceWindow(SurfNum).OutProjSLFracMult(Hour) = 555.0;
            state->dataSurface->SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour) = 444.0;
        }
    }

    state->dataSysVars->DetailedSolarTimestepIntegration = true;
    state->dataGlobal->HourOfDay = 23;
    CalcPerSolarBeam(*state, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);

    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            if (Hour == state->dataGlobal->HourOfDay) {
                EXPECT_EQ(1.0, state->dataSurface->SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
                EXPECT_EQ(1.0, state->dataSurface->SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
            } else {
                EXPECT_EQ(555.0, state->dataSurface->SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
                EXPECT_EQ(444.0, state->dataSurface->SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
            }
        }
    }

    // Clean up
    state->dataSurface->SurfaceWindow.deallocate();
    state->dataHeatBal->SunlitFracHR.deallocate();
    state->dataHeatBal->SunlitFrac.deallocate();
    state->dataHeatBal->SunlitFracWithoutReveal.deallocate();
    state->dataSolarShading->CTHETA.deallocate();
    state->dataHeatBal->CosIncAngHR.deallocate();
    state->dataHeatBal->CosIncAng.deallocate();
    state->dataSurface->SurfOpaqAO.deallocate();
    state->dataHeatBal->BackSurfaces.deallocate();
    state->dataHeatBal->OverlapAreas.deallocate();
}

TEST_F(EnergyPlusFixture, SolarShadingTest_SurfaceScheduledSolarInc)
{
    int SurfSolIncPtr;
    state->dataSurface->TotSurfIncSolSSG = 4;
    state->dataSurface->SurfIncSolSSG.allocate(state->dataSurface->TotSurfIncSolSSG);
    state->dataSurface->SurfIncSolSSG(1).SurfPtr = 1;
    state->dataSurface->SurfIncSolSSG(1).ConstrPtr = 1;
    state->dataSurface->SurfIncSolSSG(2).SurfPtr = 1;
    state->dataSurface->SurfIncSolSSG(2).ConstrPtr = 2;
    state->dataSurface->SurfIncSolSSG(3).SurfPtr = 4;
    state->dataSurface->SurfIncSolSSG(3).ConstrPtr = 10;
    state->dataSurface->SurfIncSolSSG(4).SurfPtr = 5;
    state->dataSurface->SurfIncSolSSG(4).ConstrPtr = 1;

    // Test retrieving pointer for surface incident solar schedule

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(*state, 1, 1);
    EXPECT_EQ(1, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(*state, 1, 2);
    EXPECT_EQ(2, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(*state, 1, 3);
    EXPECT_EQ(0, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(*state, 5, 1);
    EXPECT_EQ(4, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(*state, 5, 10);
    EXPECT_EQ(0, SurfSolIncPtr);

    state->dataSurface->SurfIncSolSSG.deallocate();
}

TEST_F(EnergyPlusFixture, SolarShadingTest_polygon_contains_point)
{
    unsigned int numSides = 4;
    Array1D<Vector> Rectangle3d;

    Rectangle3d.allocate(numSides);

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 10.;
    Rectangle3d(2).y = 0.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 10.;
    Rectangle3d(3).y = 10.;
    Rectangle3d(3).z = 0.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 10.;
    Rectangle3d(4).z = 0.;

    Vector PointInside;

    PointInside.x = 5.;
    PointInside.y = 5.;
    PointInside.z = 0.;

    Vector PointOutside;

    PointOutside.x = 20.;
    PointOutside.y = 20.;
    PointOutside.z = 0.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, false, false, true));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, false, false, true));

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 10.;
    Rectangle3d(2).y = 0.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 10.;
    Rectangle3d(3).y = 0.;
    Rectangle3d(3).z = 10.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 0.;
    Rectangle3d(4).z = 10.;

    PointInside.x = 5.;
    PointInside.y = 0.;
    PointInside.z = 5.;

    PointOutside.x = 20.;
    PointOutside.y = 0.;
    PointOutside.z = 20.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, false, true, false));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, false, true, false));

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 0.;
    Rectangle3d(2).y = 10.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 0.;
    Rectangle3d(3).y = 10.;
    Rectangle3d(3).z = 10.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 0.;
    Rectangle3d(4).z = 10.;

    PointInside.x = 0.;
    PointInside.y = 5.;
    PointInside.z = 5.;

    PointOutside.x = 0.;
    PointOutside.y = 20.;
    PointOutside.z = 20.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, true, false, false));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, true, false, false));
}

TEST_F(EnergyPlusFixture, SolarShadingTest_FigureSolarBeamAtTimestep)
{
    std::string const idf_objects = delimited_string({
        "  Building,",
        "    DemoFDT,                 !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    3.9999999E-02,           !- Loads Convergence Tolerance Value",
        "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullExterior,            !- Solar Distribution",
        "    ,                        !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",
        "  ShadowCalculation,",
        "    PolygonClipping,         !- Shading Calculation Method",
        "    Timestep,                !- Shading Calculation Update Frequency Method",
        "    ,                        !- Shading Calculation Update Frequency",
        "    ,                        !- Maximum Figures in Shadow Overlap Calculations",
        "    ,                        !- Polygon Clipping Algorithm",
        "    ,                        !- Pixel Counting Resolution",
        "    DetailedSkyDiffuseModeling;  !- Sky Diffuse Modeling Algorithm",
        "  SurfaceConvectionAlgorithm:Inside,TARP;",
        "  SurfaceConvectionAlgorithm:Outside,TARP;",
        "  HeatBalanceAlgorithm,ConductionTransferFunction;",
        "  Timestep,6;",
        "  RunPeriod,",
        "    RP1,                     !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    ,                        !- Day of Week for Start Day",
        "    ,                        !- Use Weather File Holidays and Special Days",
        "    ,                        !- Use Weather File Daylight Saving Period",
        "    ,                        !- Apply Weekend Holiday Rule",
        "    ,                        !- Use Weather File Rain Indicators",
        "    ;                        !- Use Weather File Snow Indicators",
        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    Continuous;              !- Numeric Type",
        "  ScheduleTypeLimits,",
        "    ON/OFF,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    Discrete;                !- Numeric Type",
        "  Schedule:Compact,",
        "    SunShading,              !- Name",
        "    ON/OFF,                  !- Schedule Type Limits Name",
        "    Through: 4/30,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    until: 24:00,1,          !- Field 3",
        "    Through: 10/31,          !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    until: 24:00,0,          !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    until: 24:00,1;          !- Field 11",
        "  Material,",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.245296,                !- Conductivity {W/m-K}",
        "    2082.400,                !- Density {kg/m3}",
        "    920.4800,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9300000,               !- Solar Absorptance",
        "    0.9300000;               !- Visible Absorptance",
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
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",
        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",
        "  WindowMaterial:Glazing,",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.80,                    !- Solar Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.80,                    !- Visible Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "  WindowMaterial:Gas,",
        "    AIRGAP,                  !- Name",
        "    AIR,                     !- Gas Type",
        "    0.0125;                  !- Thickness {m}",
        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",
        "  Construction,",
        "    EXTWALL09,               !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  Construction,",
        "    INTERIOR,                !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    SLAB FLOOR,              !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    ROOF31,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    DOUBLE PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer",
        "    AIRGAP,                  !- Layer 2",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3",
        "  Construction,",
        "    PARTITION02,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE,  !- Layer 4",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
        "  Construction,",
        "    single PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer",
        "  Construction,",
        "    EXTWALLdemo,             !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    Counterclockwise,        !- Vertex Entry Direction",
        "    Relative;                !- Coordinate System",
        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    0,                       !- Ceiling Height {m}",
        "    0;                       !- Volume {m3}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-North,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALLdemo,             !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-East,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-South,        !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-West,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:roof,              !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:floor,             !- Name",
        "    Floor,                   !- Surface Type",
        "    SLAB FLOOR,              !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}",
        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall-South:Win001, !- Name",
        "    Window,                  !- Surface Type",
        "    DOUBLE PANE HW WINDOW,   !- Construction Name",
        "    Zn001:Wall-South,        !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  WindowProperty:FrameAndDivider,",
        "    TestFrameAndDivider,     !- Name",
        "    0.05,                    !- Frame Width {m}",
        "    0.05,                    !- Frame Outside Projection {m}",
        "    0.05,                    !- Frame Inside Projection {m}",
        "    5.0,                     !- Frame Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
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
        "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
        "    0.8,                     !- Divider Solar Absorptance",
        "    0.8,                     !- Divider Visible Absorptance",
        "    0.9;                     !- Divider Thermal Hemispherical Emissivity",
        "  Shading:Zone:Detailed,",
        "    Zn001:Wall-South:Shade001,  !- Name",
        "    Zn001:Wall-South,        !- Base Surface Name",
        "    SunShading,              !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  ShadingProperty:Reflectance,",
        "    Zn001:Wall-South:Shade001,  !- Shading Surface Name",
        "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
        "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shading",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(*state, FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                            // expect no errors

    //	compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    state->dataEnvrn->DayOfYear_Schedule = 168;
    state->dataEnvrn->DayOfWeek = 6;
    state->dataGlobal->TimeStep = 4;
    state->dataGlobal->HourOfDay = 9;

    //	compare_err_stream( "" ); // just for debugging

    state->dataSurface->ShadingTransmittanceVaries = true;
    state->dataSysVars->DetailedSkyDiffuseAlgorithm = true;
    state->dataHeatBal->SolarDistribution = FullExterior;

    state->dataSolarShading->CalcSkyDifShading = true;
    SolarShading::InitSolarCalculations(*state);
    SolarShading::SkyDifSolarShading(*state);
    state->dataSolarShading->CalcSkyDifShading = false;

    FigureSolarBeamAtTimestep(*state, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep);

    int windowSurfNum = UtilityRoutines::FindItemInList("ZN001:WALL-SOUTH:WIN001", state->dataSurface->Surface);
    EXPECT_NEAR(0.6504, state->dataHeatBal->DifShdgRatioIsoSkyHRTS(4, 9, windowSurfNum), 0.0001);
    EXPECT_NEAR(0.9152, state->dataHeatBal->DifShdgRatioHorizHRTS(4, 9, windowSurfNum), 0.0001);
}

TEST_F(EnergyPlusFixture, SolarShadingTest_ExternalShadingIO)
{
    std::string const idf_objects = delimited_string({

        "  Building,",
        "    DemoFDT,                 !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    3.9999999E-02,           !- Loads Convergence Tolerance Value",
        "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullExterior,            !- Solar Distribution",
        "    ,                        !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",
        "  ShadowCalculation,",
        "    Scheduled,               !- Shading Calculation Method",
        "    Timestep,                !- Shading Calculation Update Frequency Method",
        "    ,                        !- Shading Calculation Update Frequency",
        "    ,                           !- Maximum Figures in Shadow Overlap Calculations",
        "    ,                           !- Polygon Clipping Algorithm",
        "    ,                        !- Pixel Counting Resolution",
        "    DetailedSkyDiffuseModeling, !- Sky Diffuse Modeling Algorithm",
        "    Yes;                        !- Output External Shading Calculation Results",
        "  SurfaceConvectionAlgorithm:Inside,TARP;",
        "  SurfaceConvectionAlgorithm:Outside,TARP;",
        "  HeatBalanceAlgorithm,ConductionTransferFunction;",
        "  Timestep,6;",
        "  RunPeriod,",
        "    RP1,                     !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    ,                        !- Day of Week for Start Day",
        "    ,                        !- Use Weather File Holidays and Special Days",
        "    ,                        !- Use Weather File Daylight Saving Period",
        "    ,                        !- Apply Weekend Holiday Rule",
        "    ,                        !- Use Weather File Rain Indicators",
        "    ;                        !- Use Weather File Snow Indicators",
        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    Continuous;              !- Numeric Type",
        "  ScheduleTypeLimits,",
        "    ON/OFF,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    Discrete;                !- Numeric Type",
        "  Schedule:Compact,",
        "    SunShading,              !- Name",
        "    ON/OFF,                  !- Schedule Type Limits Name",
        "    Through: 4/30,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    until: 24:00,1,          !- Field 3",
        "    Through: 10/31,          !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    until: 24:00,0,          !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    until: 24:00,1;          !- Field 11",
        "  Material,",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.245296,                !- Conductivity {W/m-K}",
        "    2082.400,                !- Density {kg/m3}",
        "    920.4800,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9300000,               !- Solar Absorptance",
        "    0.9300000;               !- Visible Absorptance",
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
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",
        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",
        "  WindowMaterial:Glazing,",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.80,                    !- Solar Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.80,                    !- Visible Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "  WindowMaterial:Gas,",
        "    AIRGAP,                  !- Name",
        "    AIR,                     !- Gas Type",
        "    0.0125;                  !- Thickness {m}",
        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",
        "  Construction,",
        "    EXTWALL09,               !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  Construction,",
        "    INTERIOR,                !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    SLAB FLOOR,              !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    ROOF31,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    DOUBLE PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer",
        "    AIRGAP,                  !- Layer 2",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3",
        "  Construction,",
        "    PARTITION02,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE,  !- Layer 4",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
        "  Construction,",
        "    single PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer",
        "  Construction,",
        "    EXTWALLdemo,             !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    Counterclockwise,        !- Vertex Entry Direction",
        "    Relative;                !- Coordinate System",
        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    0,                       !- Ceiling Height {m}",
        "    0;                       !- Volume {m3}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-North,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALLdemo,             !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-East,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-South,        !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-West,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:roof,              !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  SurfaceProperty:LocalEnvironment,",
        "    LocEnv:Zn001:roof,",
        "    Zn001:roof,",
        "    ExtShadingSch:Zn001:roof,",
        "    ,",
        "    ;",
        "  Schedule:Compact,",
        "    ExtShadingSch:Zn001:roof,",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    0.5432;                  !- Field 4",
        "  BuildingSurface:Detailed,",
        "    Zn001:floor,             !- Name",
        "    Floor,                   !- Surface Type",
        "    SLAB FLOOR,              !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}",
        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall-South:Win001, !- Name",
        "    Window,                  !- Surface Type",
        "    DOUBLE PANE HW WINDOW,   !- Construction Name",
        "    Zn001:Wall-South,        !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  WindowProperty:FrameAndDivider,",
        "    TestFrameAndDivider,     !- Name",
        "    0.05,                    !- Frame Width {m}",
        "    0.05,                    !- Frame Outside Projection {m}",
        "    0.05,                    !- Frame Inside Projection {m}",
        "    5.0,                     !- Frame Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
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
        "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
        "    0.8,                     !- Divider Solar Absorptance",
        "    0.8,                     !- Divider Visible Absorptance",
        "    0.9;                     !- Divider Thermal Hemispherical Emissivity",
        "  Shading:Zone:Detailed,",
        "    Zn001:Wall-South:Shade001,  !- Name",
        "    Zn001:Wall-South,        !- Base Surface Name",
        "    SunShading,              !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  ShadingProperty:Reflectance,",
        "    Zn001:Wall-South:Shade001,  !- Shading Surface Name",
        "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
        "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shading",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    compare_err_stream("");                                 // just for debugging
    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    state->dataEnvrn->DayOfYear_Schedule = 168;
    state->dataEnvrn->DayOfWeek = 6;
    state->dataGlobal->TimeStep = 4;
    state->dataGlobal->HourOfDay = 9;
    state->dataGlobal->DoingSizing = false;
    state->dataGlobal->KindOfSim = DataGlobalConstants::KindOfSim::RunPeriodWeather;

    compare_err_stream(""); // just for debugging

    state->dataSurface->ShadingTransmittanceVaries = true;
    state->dataSysVars->DetailedSkyDiffuseAlgorithm = true;
    state->dataSysVars->shadingMethod = DataSystemVariables::ShadingMethod::Scheduled;
    state->dataHeatBal->SolarDistribution = FullExterior;

    state->dataSolarShading->CalcSkyDifShading = true;
    SolarShading::InitSolarCalculations(*state);
    SolarShading::SkyDifSolarShading(*state);
    state->dataSolarShading->CalcSkyDifShading = false;

    ScheduleManager::UpdateScheduleValues(*state);
    state->dataBSDFWindow->SUNCOSTS(4, 9, 1) = 0.1;
    state->dataBSDFWindow->SUNCOSTS(4, 9, 2) = 0.1;
    state->dataBSDFWindow->SUNCOSTS(4, 9, 3) = 0.1;
    FigureSolarBeamAtTimestep(*state, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep);

    EXPECT_TRUE(state->dataSysVars->shadingMethod == DataSystemVariables::ShadingMethod::Scheduled);
    EXPECT_DOUBLE_EQ(0.5432, ScheduleManager::LookUpScheduleValue(*state, 2, 9, 4));
    EXPECT_FALSE(state->dataSolarShading->SUNCOS(3) < 0.00001);
    EXPECT_DOUBLE_EQ(0.00001, DataEnvironment::SunIsUpValue);
    ;
    EXPECT_FALSE(state->dataSolarShading->SUNCOS(3) < DataEnvironment::SunIsUpValue);

    int surfNum = UtilityRoutines::FindItemInList("ZN001:WALL-SOUTH", state->dataSurface->Surface);
    EXPECT_DOUBLE_EQ(1, state->dataHeatBal->SunlitFrac(4, 9, surfNum));
    surfNum = UtilityRoutines::FindItemInList("ZN001:WALL-SOUTH:WIN001", state->dataSurface->Surface);
    EXPECT_DOUBLE_EQ(1, state->dataHeatBal->SunlitFrac(4, 9, surfNum));
    surfNum = UtilityRoutines::FindItemInList("ZN001:ROOF", state->dataSurface->Surface);
    EXPECT_DOUBLE_EQ(0.5432, state->dataHeatBal->SunlitFrac(4, 9, surfNum));
}

TEST_F(EnergyPlusFixture, SolarShadingTest_DisableGroupSelfShading)
{
    std::string const idf_objects = delimited_string({
        "  Building,",
        "    DemoFDT,                 !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    3.9999999E-02,           !- Loads Convergence Tolerance Value",
        "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullExterior,            !- Solar Distribution",
        "    ,                        !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",
        "  ShadowCalculation,",
        "    PolygonClipping,         !- Shading Calculation Method",
        "    Timestep,                !- Shading Calculation Update Frequency Method",
        "    ,                        !- Shading Calculation Update Frequency",
        "    ,                           !- Maximum Figures in Shadow Overlap Calculations",
        "    ,                           !- Polygon Clipping Algorithm",
        "    ,                           !- Pixel Counting Resolution",
        "    ,                           !- Sky Diffuse Modeling Algorithm",
        "    ,                           !- Output External Shading Calculation Results",
        "    Yes,                        !- Disable Shading within A Zone Group",
        "    ,                           !- Disable Shading between Zone Groups",
        "    ShadingZoneGroup;           !- Shading Group 1 ZoneList Name",
        "  SurfaceConvectionAlgorithm:Inside,TARP;",
        "  SurfaceConvectionAlgorithm:Outside,TARP;",
        "  HeatBalanceAlgorithm,ConductionTransferFunction;",
        "  Timestep,6;",
        "  RunPeriod,",
        "    RP1,                     !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    ,                        !- Day of Week for Start Day",
        "    ,                        !- Use Weather File Holidays and Special Days",
        "    ,                        !- Use Weather File Daylight Saving Period",
        "    ,                        !- Apply Weekend Holiday Rule",
        "    ,                        !- Use Weather File Rain Indicators",
        "    ;                        !- Use Weather File Snow Indicators",
        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    Continuous;              !- Numeric Type",
        "  ScheduleTypeLimits,",
        "    ON/OFF,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    Discrete;                !- Numeric Type",
        "  Schedule:Compact,",
        "    SunShading,              !- Name",
        "    ON/OFF,                  !- Schedule Type Limits Name",
        "    Through: 4/30,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    until: 24:00,1,          !- Field 3",
        "    Through: 10/31,          !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    until: 24:00,0,          !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    until: 24:00,1;          !- Field 11",
        "  Material,",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.245296,                !- Conductivity {W/m-K}",
        "    2082.400,                !- Density {kg/m3}",
        "    920.4800,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9300000,               !- Solar Absorptance",
        "    0.9300000;               !- Visible Absorptance",
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
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",
        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",
        "  WindowMaterial:Glazing,",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.80,                    !- Solar Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.80,                    !- Visible Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "  WindowMaterial:Gas,",
        "    AIRGAP,                  !- Name",
        "    AIR,                     !- Gas Type",
        "    0.0125;                  !- Thickness {m}",
        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",
        "  Construction,",
        "    EXTWALL09,               !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  Construction,",
        "    INTERIOR,                !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    SLAB FLOOR,              !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    ROOF31,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    DOUBLE PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer",
        "    AIRGAP,                  !- Layer 2",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3",
        "  Construction,",
        "    PARTITION02,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE,  !- Layer 4",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
        "  Construction,",
        "    single PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer",
        "  Construction,",
        "    EXTWALLdemo,             !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    Counterclockwise,        !- Vertex Entry Direction",
        "    Relative;                !- Coordinate System",
        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    0,                       !- Ceiling Height {m}",
        "    0;                       !- Volume {m3}",
        "  ZoneList,",
        "    ShadingZoneGroup,",
        "    ZONE ONE;",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-North,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALLdemo,             !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-East,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-South,        !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-West,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:roof,              !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  Schedule:Compact,",
        "    ExtShadingSch:Zn001:roof,",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    0.5432;                  !- Field 4",
        "  BuildingSurface:Detailed,",
        "    Zn001:floor,             !- Name",
        "    Floor,                   !- Surface Type",
        "    SLAB FLOOR,              !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}",
        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall-South:Win001, !- Name",
        "    Window,                  !- Surface Type",
        "    DOUBLE PANE HW WINDOW,   !- Construction Name",
        "    Zn001:Wall-South,        !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  WindowProperty:FrameAndDivider,",
        "    TestFrameAndDivider,     !- Name",
        "    0.05,                    !- Frame Width {m}",
        "    0.05,                    !- Frame Outside Projection {m}",
        "    0.05,                    !- Frame Inside Projection {m}",
        "    5.0,                     !- Frame Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
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
        "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
        "    0.8,                     !- Divider Solar Absorptance",
        "    0.8,                     !- Divider Visible Absorptance",
        "    0.9;                     !- Divider Thermal Hemispherical Emissivity",
        "  Shading:Zone:Detailed,",
        "    Zn001:Wall-South:Shade001,  !- Name",
        "    Zn001:Wall-South,        !- Base Surface Name",
        "    SunShading,              !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  ShadingProperty:Reflectance,",
        "    Zn001:Wall-South:Shade001,  !- Shading Surface Name",
        "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
        "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shading",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    SizingManager::GetZoneSizingInput(*state);

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    compare_err_stream("");                                 // just for debugging
    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    compare_err_stream(""); // just for debugging

    SolarShading::GetShadowingInput(*state);

    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; SurfNum++) {
        if (state->dataSurface->Surface(SurfNum).ExtBoundCond == 0 && state->dataSurface->Surface(SurfNum).Zone != 0) {
            int ZoneSize = state->dataSurface->SurfShadowDisabledZoneList(SurfNum).size();
            EXPECT_EQ(1, ZoneSize);
            std::vector<int> DisabledZones = state->dataSurface->SurfShadowDisabledZoneList(SurfNum);
            for (int i : DisabledZones) {
                EXPECT_EQ(1, i);
            }
        }
    }
}

TEST_F(EnergyPlusFixture, SolarShadingTest_PolygonClippingDirect)
{
    std::string const idf_objects = delimited_string({
        "  Building,",
        "    DemoFDT,                 !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    3.9999999E-02,           !- Loads Convergence Tolerance Value",
        "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullExterior,            !- Solar Distribution",
        "    ,                        !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",
        "  ShadowCalculation,",
        "    PolygonClipping,         !- Shading Calculation Method",
        "    Timestep,                !- Shading Calculation Update Frequency Method",
        "    ,                        !- Shading Calculation Update Frequency",
        "    ,                        !- Maximum Figures in Shadow Overlap Calculations",
        "    ,                        !- Polygon Clipping Algorithm",
        "    ,                        !- Pixel Counting Resolution",
        "    DetailedSkyDiffuseModeling;  !- Sky Diffuse Modeling Algorithm",
        "  SurfaceConvectionAlgorithm:Inside,TARP;",
        "  SurfaceConvectionAlgorithm:Outside,TARP;",
        "  HeatBalanceAlgorithm,ConductionTransferFunction;",
        "  Timestep,6;",
        "  RunPeriod,",
        "    RP1,                     !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    ,                        !- Day of Week for Start Day",
        "    ,                        !- Use Weather File Holidays and Special Days",
        "    ,                        !- Use Weather File Daylight Saving Period",
        "    ,                        !- Apply Weekend Holiday Rule",
        "    ,                        !- Use Weather File Rain Indicators",
        "    ;                        !- Use Weather File Snow Indicators",
        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    Continuous;              !- Numeric Type",
        "  ScheduleTypeLimits,",
        "    ON/OFF,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    Discrete;                !- Numeric Type",
        "  Schedule:Compact,",
        "    SunShading,              !- Name",
        "    ON/OFF,                  !- Schedule Type Limits Name",
        "    Through: 4/30,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    until: 24:00,1,          !- Field 3",
        "    Through: 10/31,          !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    until: 24:00,0,          !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    until: 24:00,1;          !- Field 11",
        "  Material,",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.245296,                !- Conductivity {W/m-K}",
        "    2082.400,                !- Density {kg/m3}",
        "    920.4800,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9300000,               !- Solar Absorptance",
        "    0.9300000;               !- Visible Absorptance",
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
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",
        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",
        "  WindowMaterial:Glazing,",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.80,                    !- Solar Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.80,                    !- Visible Transmittance at Normal Incidence",
        "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "  WindowMaterial:Gas,",
        "    AIRGAP,                  !- Name",
        "    AIR,                     !- Gas Type",
        "    0.0125;                  !- Thickness {m}",
        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",
        "  Construction,",
        "    EXTWALL09,               !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  Construction,",
        "    INTERIOR,                !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    SLAB FLOOR,              !- Name",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    ROOF31,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",
        "  Construction,",
        "    DOUBLE PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer",
        "    AIRGAP,                  !- Layer 2",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3",
        "  Construction,",
        "    PARTITION02,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C12 - 2 IN HW CONCRETE,  !- Layer 4",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
        "  Construction,",
        "    single PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer",
        "  Construction,",
        "    EXTWALLdemo,             !- Name",
        "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4",
        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    Counterclockwise,        !- Vertex Entry Direction",
        "    Relative;                !- Coordinate System",
        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    0,                       !- Ceiling Height {m}",
        "    0;                       !- Volume {m3}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-North,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALLdemo,             !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-East,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-South,        !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:Wall-West,         !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL09,               !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:roof,              !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Zn001:floor,             !- Name",
        "    Floor,                   !- Surface Type",
        "    SLAB FLOOR,              !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}",
        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall-South:Win001, !- Name",
        "    Window,                  !- Surface Type",
        "    DOUBLE PANE HW WINDOW,   !- Construction Name",
        "    Zn001:Wall-South,        !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    TestFrameAndDivider,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  WindowProperty:FrameAndDivider,",
        "    TestFrameAndDivider,     !- Name",
        "    0.05,                    !- Frame Width {m}",
        "    0.05,                    !- Frame Outside Projection {m}",
        "    0.05,                    !- Frame Inside Projection {m}",
        "    5.0,                     !- Frame Conductance {W/m2-K}",
        "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
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
        "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
        "    0.8,                     !- Divider Solar Absorptance",
        "    0.8,                     !- Divider Visible Absorptance",
        "    0.9;                     !- Divider Thermal Hemispherical Emissivity",
        "  Shading:Zone:Detailed,",
        "    Zn001:Wall-South:Shade001,  !- Name",
        "    Zn001:Wall-South,        !- Base Surface Name",
        "    SunShading,              !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",
        "  ShadingProperty:Reflectance,",
        "    Zn001:Wall-South:Shade001,  !- Shading Surface Name",
        "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
        "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shading",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(*state, FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                            // expect no errors

    //	compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    state->dataEnvrn->DayOfYear_Schedule = 168;
    state->dataEnvrn->DayOfWeek = 6;
    state->dataGlobal->TimeStep = 4;
    state->dataGlobal->HourOfDay = 9;

    //	compare_err_stream( "" ); // just for debugging
    EXPECT_FALSE(state->dataSysVars->SlaterBarsky);

    state->dataSurface->ShadingTransmittanceVaries = true;
    state->dataSysVars->DetailedSkyDiffuseAlgorithm = true;
    state->dataHeatBal->SolarDistribution = FullExterior;
    state->dataSysVars->SlaterBarsky = true;

    state->dataSolarShading->CalcSkyDifShading = true;
    SolarShading::InitSolarCalculations(*state);
    SolarShading::SkyDifSolarShading(*state);
    state->dataSolarShading->CalcSkyDifShading = false;

    FigureSolarBeamAtTimestep(*state, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep);
    int surfNum = UtilityRoutines::FindItemInList("ZN001:WALL-SOUTH:WIN001", state->dataSurface->Surface);
    EXPECT_NEAR(0.6504, state->dataHeatBal->DifShdgRatioIsoSkyHRTS(4, 9, surfNum), 0.0001);
    EXPECT_NEAR(0.9152, state->dataHeatBal->DifShdgRatioHorizHRTS(4, 9, surfNum), 0.0001);

    state->dataSysVars->SlaterBarsky = false;
}

TEST_F(EnergyPlusFixture, SolarShadingTest_CHKBKS)
{
    int numofsurface;
    numofsurface = 4;
    state->dataSurface->Surface.allocate(numofsurface);
    Vector VtxA1(0.0, 0.0, 5.0);
    Vector VtxA2(0.0, 0.0, 0.0);
    Vector VtxA3(5.0, 0.0, 0.0);
    Vector VtxA4(5.0, 0.0, 5.0);

    Vector VtxB1(0.0, -10.0, 5.0);
    Vector VtxB2(0.0, -10.0, 2.5);
    Vector VtxB3(0.0, -5.0, 2.5);
    Vector VtxB4(0.0, -5.0, 0.0);
    Vector VtxB5(0.0, 0.0, 0.0);
    Vector VtxB6(0.0, 0.0, 5.0);

    state->dataSurface->Surface(1).Sides = 4;

    state->dataSurface->Surface(1).Vertex.allocate(4);
    state->dataSurface->Surface(1).Vertex(1) = VtxA1;
    state->dataSurface->Surface(1).Vertex(2) = VtxA2;
    state->dataSurface->Surface(1).Vertex(3) = VtxA3;
    state->dataSurface->Surface(1).Vertex(4) = VtxA4;

    state->dataSurface->Surface(1).Name = "Surf_Back";
    state->dataSurface->Surface(1).ZoneName = "Zone1";

    state->dataSurface->Surface(2).Sides = 6; // receiving
    state->dataSurface->Surface(2).Vertex.allocate(6);

    state->dataSurface->Surface(2).Vertex(1) = VtxB1;
    state->dataSurface->Surface(2).Vertex(2) = VtxB2;
    state->dataSurface->Surface(2).Vertex(3) = VtxB3;
    state->dataSurface->Surface(2).Vertex(4) = VtxB4;
    state->dataSurface->Surface(2).Vertex(5) = VtxB5;
    state->dataSurface->Surface(2).Vertex(6) = VtxB6;

    state->dataSurface->Surface(2).Name = "Surf_Recv";
    state->dataSurface->Surface(2).ZoneName = "Zone1";

    CHKBKS(*state, 1, 2);

    EXPECT_TRUE(this->has_err_output(false));

    std::string const error_string =
        delimited_string({"   ** Severe  ** Problem in interior solar distribution calculation (CHKBKS)",
                          "   **   ~~~   **    Solar Distribution = FullInteriorExterior will not work in Zone=Zone1",
                          "   **   ~~~   **    because one or more of vertices, such as Vertex 3 of back surface=Surf_Back"
                          ", is in front of receiving surface=Surf_Recv",
                          "   **   ~~~   **    (Dot Product indicator=             62.5000)",
                          "   **   ~~~   **    Check surface geometry; if OK, use Solar Distribution = FullExterior instead. Use Output:Diagnostics, "
                          "DisplayExtraWarnings; for more details."});

    EXPECT_TRUE(compare_err_stream(error_string, true));

    state->dataSurface->Surface(3).Sides = 4;

    state->dataSurface->Surface(3).Vertex.allocate(4);
    state->dataSurface->Surface(3).Vertex(1) = VtxA1;
    state->dataSurface->Surface(3).Vertex(2) = VtxA2;
    state->dataSurface->Surface(3).Vertex(3) = VtxA3;
    state->dataSurface->Surface(3).Vertex(4) = VtxA4;

    state->dataSurface->Surface(3).Name = "Surf_Back2";
    state->dataSurface->Surface(3).ZoneName = "Zone2";

    state->dataSurface->Surface(4).Sides = 6; // receiving
    state->dataSurface->Surface(4).Vertex.allocate(6);

    state->dataSurface->Surface(4).Vertex(1) = VtxB6;
    state->dataSurface->Surface(4).Vertex(2) = VtxB5;
    state->dataSurface->Surface(4).Vertex(3) = VtxB4;
    state->dataSurface->Surface(4).Vertex(4) = VtxB3;
    state->dataSurface->Surface(4).Vertex(5) = VtxB2;
    state->dataSurface->Surface(4).Vertex(6) = VtxB1;

    state->dataSurface->Surface(4).Name = "Surf_Recv2";
    state->dataSurface->Surface(4).ZoneName = "Zone2";

    CHKBKS(*state, 3, 4);

    EXPECT_FALSE(this->has_err_output(true));
}

TEST_F(EnergyPlusFixture, SolarShadingTest_NonConvexErrors)
{
    std::string const idf_objects = R"IDF(
RunPeriod,
  Annual,                  !- Name
  1,                       !- Begin Month
  1,                       !- Begin Day of Month
  ,                        !- Begin Year
  12,                      !- End Month
  31,                      !- End Day of Month
  ,                        !- End Year
  Sunday,                  !- Day of Week for Start Day
  No,                      !- Use Weather File Holidays and Special Days
  No,                      !- Use Weather File Daylight Saving Period
  No,                      !- Apply Weekend Holiday Rule
  Yes,                     !- Use Weather File Rain Indicators
  Yes;                     !- Use Weather File Snow Indicators

ShadowCalculation,
  PolygonClipping,  !- Calculation Method
  Periodic,  !- Caclulation Update Frequency Method
  20,  !- Calculation Update Frequency
  15000;  !- Maximum Figures in Shadow Overlap Calculations

Timestep,4;

Building,
  Non-convex,    !- Name
  0,      !- North Axis {deg}
  City,          !- Terrain
  0.5,  !- Loads Convergence Tolerance Value
  0.5,  !- Temperature Convergence Tolerance Value {deltaC}
  FullInteriorAndExterior, !- Solar Distribution
  25,  !- Maximum Number of Warmup Days
  2;  !- Minimum Number of Warmup Days

GlobalGeometryRules,
    UpperLeftCorner,          !- Starting Vertex Position
    Counterclockwise,         !- Vertex Entry Direction
    Relative;                 !- Coordinate System

Zone,
    Zone A,                   !- Name
    0,                        !- Direction of Relative North
    0,                        !- X Origin
    0,                        !- Y Origin
    0,                        !- Z Origin
    ,                         !- Type
    1;                        !- Multiplier

BuildingSurface:Detailed,
    A48756,                   !- Name
    Floor,                    !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    NoSun,                    !- Sun Exposure
    NoWind,                   !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    6.096, 6.096, 0,                        !- Vertex 1 Zcoordinate
    6.096, 0, 0,                        !- Vertex 2 Zcoordinate
    0, 0, 0,                        !- Vertex 3 Zcoordinate
    0, 6.096, 0;                        !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    69C03D,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    6,                        !- Number of Vertices
    0, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    0, 6.096, 0,                        !- Vertex 2 Zcoordinate
    0, 0, 0,                        !- Vertex 3 Zcoordinate
    0, 0, 3.048,                    !- Vertex 4 Zcoordinate
    0, 3.048, 3.048,                    !- Vertex 5 Zcoordinate
    0, 3.048, 6.096;                    !- Vertex 6 Zcoordinate

BuildingSurface:Detailed,
    5BB552,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    6,                        !- Number of Vertices
    6.096, 3.048, 6.096,                    !- Vertex 1 Zcoordinate
    6.096, 3.048, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 0, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 0, 0,                        !- Vertex 4 Zcoordinate
    6.096, 6.096, 0,                        !- Vertex 5 Zcoordinate
    6.096, 6.096, 6.096;                    !- Vertex 6 Zcoordinate

BuildingSurface:Detailed,
    682F88,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    6.096, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    6.096, 6.096, 0,                        !- Vertex 2 Zcoordinate
    0, 6.096, 0,                        !- Vertex 3 Zcoordinate
    0, 6.096, 6.096;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    C292CF,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 0, 3.048,                    !- Vertex 1 Zcoordinate
    0, 0, 0,                        !- Vertex 2 Zcoordinate
    6.096, 0, 0,                        !- Vertex 3 Zcoordinate
    6.096, 0, 3.048;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    319080,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 3.048, 6.096,                    !- Vertex 1 Zcoordinate
    0, 3.048, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 3.048, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 3.048, 6.096;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    866E33,                   !- Name
    Roof,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 3.048, 3.048,                    !- Vertex 1 Zcoordinate
    0, 0, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 0, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 3.048, 3.048;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    32CB6F,                   !- Name
    Roof,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    0, 3.048, 6.096,                    !- Vertex 2 Zcoordinate
    6.096, 3.048, 6.096,                    !- Vertex 3 Zcoordinate
    6.096, 6.096, 6.096;                    !- Vertex 4 Zcoordinate

FenestrationSurface:Detailed,
    DAF2B2,                   !- Name
    Window,                   !- Surface Type
    Exterior Window,          !- Construction Name
    319080,                   !- Building Surface Name
    ,                         !- Outside Boundary Condition Object
    ,                         !- View Factor to Ground
    ,                         !- Frame and Divider Name
    ,                         !- Multiplier
    4,                        !- Number of Vertices
    0.6096, 3.048, 5.4864,                   !- Vertex 1 Zcoordinate
    0.6096, 3.048, 3.6576,                   !- Vertex 2 Zcoordinate
    5.4864, 3.048, 3.6576,                   !- Vertex 3 Zcoordinate
    5.4864, 3.048, 5.4864;                   !- Vertex 4 Zcoordinate

Material,
  Wall Continuous Insulation,  !- Name
  MediumSmooth,               !- Roughness
  0.038037600000000005,  !- Thickness {m}
  0.0288,  !- Conductivity {W/m-K}
  32,                      !- Density {kg/m3}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Extruded polystyrene, smooth skin - aged 180 days)
  1465;                     !- Specific Heat {J/kg-K}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Extruded polystyrene, smooth skin - aged 180 days)

Material,
  Wall Cavity Effective Insulation,  !- Name
  VeryRough,               !- Roughness
  0.08889999999999999,  !- Thickness {m}
  0.08389829384297165,  !- Conductivity {W/m-K}
  8.17,                      !- Density {kg/m3}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Glass-fiber batts)
  837;                     !- Specific Heat {J/kg-K}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Glass-fiber batts)

Construction,
  Exterior Wall,           !- Name
  Stucco,  !- Layer
  Wall Continuous Insulation,  !- Layer
  Wall Cavity Effective Insulation,  !- Layer
  Gypsum 5/8in;  !- Layer

Construction,
  Exterior Window,         !- Name
  Theoretical Glazing;     !- Outside Layer

Material,
  Gypsum 5/8in,           !- Name
  MediumSmooth,            !- Roughness
  0.016,                   !- Thickness {m}
  0.1622,                  !- Conductivity {W/m-K}
  800,                     !- Density {kg/m3}
  1090,                    !- Specific Heat {J/kg-K}
  0.9,                     !- Thermal Absorptance
  0.7,                     !- Solar Absorptance
  0.7;                     !- Visible Absorptance

Material,
  Stucco,           !- Name
  Smooth,                  !- Roughness
  0.01015,                 !- Thickness {m}
  0.72,                    !- Conductivity {W/m-K}
  1856,                    !- Density {kg/m3}
  840,                     !- Specific Heat {J/kg-K}
  0.9,                     !- Thermal Absorptance
  0.7,                     !- Solar Absorptance
  0.7;                     !- Visible Absorptance

WindowMaterial:SimpleGlazingSystem,
  Theoretical Glazing,     !- Name
  3.1232254400908577,  !- U-Factor {W/m2-K}
  0.4,  !- Solar Heat Gain Coefficient
  0.4;  !- Visible Transmittance
)IDF";

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(*state, FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                            // expect no errors

    //	compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    state->dataGlobal->BeginSimFlag = true;
    HeatBalanceManager::InitHeatBalance(*state);
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);

    std::string error_string = delimited_string({"** Severe  ** Problem in interior solar distribution calculation (CHKBKS)"});

    match_err_stream(error_string);

    error_string =
        delimited_string({"** Severe  ** DetermineShadowingCombinations: There are 1 surfaces which are casting surfaces and are non-convex."});

    match_err_stream(error_string);
}

TEST_F(EnergyPlusFixture, SolarShadingTest_GPUNonConvexErrors)
{
    std::string const idf_objects = R"IDF(
RunPeriod,
  Annual,                  !- Name
  1,                       !- Begin Month
  1,                       !- Begin Day of Month
  ,                        !- Begin Year
  12,                      !- End Month
  31,                      !- End Day of Month
  ,                        !- End Year
  Sunday,                  !- Day of Week for Start Day
  No,                      !- Use Weather File Holidays and Special Days
  No,                      !- Use Weather File Daylight Saving Period
  No,                      !- Apply Weekend Holiday Rule
  Yes,                     !- Use Weather File Rain Indicators
  Yes;                     !- Use Weather File Snow Indicators

ShadowCalculation,
  PixelCounting,  !- Calculation Method
  Periodic,  !- Caclulation Update Frequency Method
  20,  !- Calculation Update Frequency
  15000;  !- Maximum Figures in Shadow Overlap Calculations

Timestep,4;

Building,
  Non-convex,    !- Name
  0,      !- North Axis {deg}
  City,          !- Terrain
  0.5,  !- Loads Convergence Tolerance Value
  0.5,  !- Temperature Convergence Tolerance Value {deltaC}
  FullInteriorAndExterior, !- Solar Distribution
  25,  !- Maximum Number of Warmup Days
  2;  !- Minimum Number of Warmup Days

GlobalGeometryRules,
    UpperLeftCorner,          !- Starting Vertex Position
    Counterclockwise,         !- Vertex Entry Direction
    Relative;                 !- Coordinate System

Zone,
    Zone A,                   !- Name
    0,                        !- Direction of Relative North
    0,                        !- X Origin
    0,                        !- Y Origin
    0,                        !- Z Origin
    ,                         !- Type
    1;                        !- Multiplier

BuildingSurface:Detailed,
    A48756,                   !- Name
    Floor,                    !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    NoSun,                    !- Sun Exposure
    NoWind,                   !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    6.096, 6.096, 0,                        !- Vertex 1 Zcoordinate
    6.096, 0, 0,                        !- Vertex 2 Zcoordinate
    0, 0, 0,                        !- Vertex 3 Zcoordinate
    0, 6.096, 0;                        !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    69C03D,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    6,                        !- Number of Vertices
    0, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    0, 6.096, 0,                        !- Vertex 2 Zcoordinate
    0, 0, 0,                        !- Vertex 3 Zcoordinate
    0, 0, 3.048,                    !- Vertex 4 Zcoordinate
    0, 3.048, 3.048,                    !- Vertex 5 Zcoordinate
    0, 3.048, 6.096;                    !- Vertex 6 Zcoordinate

BuildingSurface:Detailed,
    5BB552,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    6,                        !- Number of Vertices
    6.096, 3.048, 6.096,                    !- Vertex 1 Zcoordinate
    6.096, 3.048, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 0, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 0, 0,                        !- Vertex 4 Zcoordinate
    6.096, 6.096, 0,                        !- Vertex 5 Zcoordinate
    6.096, 6.096, 6.096;                    !- Vertex 6 Zcoordinate

BuildingSurface:Detailed,
    682F88,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    6.096, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    6.096, 6.096, 0,                        !- Vertex 2 Zcoordinate
    0, 6.096, 0,                        !- Vertex 3 Zcoordinate
    0, 6.096, 6.096;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    C292CF,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 0, 3.048,                    !- Vertex 1 Zcoordinate
    0, 0, 0,                        !- Vertex 2 Zcoordinate
    6.096, 0, 0,                        !- Vertex 3 Zcoordinate
    6.096, 0, 3.048;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    319080,                   !- Name
    Wall,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 3.048, 6.096,                    !- Vertex 1 Zcoordinate
    0, 3.048, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 3.048, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 3.048, 6.096;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    866E33,                   !- Name
    Roof,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 3.048, 3.048,                    !- Vertex 1 Zcoordinate
    0, 0, 3.048,                    !- Vertex 2 Zcoordinate
    6.096, 0, 3.048,                    !- Vertex 3 Zcoordinate
    6.096, 3.048, 3.048;                    !- Vertex 4 Zcoordinate

BuildingSurface:Detailed,
    32CB6F,                   !- Name
    Roof,                     !- Surface Type
    Exterior Wall,            !- Construction Name
    Zone A,                   !- Zone Name
    Outdoors,                 !- Outside Boundary Condition
    ,                         !- Outside Boundary Condition Object
    SunExposed,               !- Sun Exposure
    WindExposed,              !- Wind Exposure
    ,                         !- View Factor to Ground
    4,                        !- Number of Vertices
    0, 6.096, 6.096,                    !- Vertex 1 Zcoordinate
    0, 3.048, 6.096,                    !- Vertex 2 Zcoordinate
    6.096, 3.048, 6.096,                    !- Vertex 3 Zcoordinate
    6.096, 6.096, 6.096;                    !- Vertex 4 Zcoordinate

FenestrationSurface:Detailed,
    DAF2B2,                   !- Name
    Window,                   !- Surface Type
    Exterior Window,          !- Construction Name
    319080,                   !- Building Surface Name
    ,                         !- Outside Boundary Condition Object
    ,                         !- View Factor to Ground
    ,                         !- Frame and Divider Name
    ,                         !- Multiplier
    4,                        !- Number of Vertices
    0.6096, 3.048, 5.4864,                   !- Vertex 1 Zcoordinate
    0.6096, 3.048, 3.6576,                   !- Vertex 2 Zcoordinate
    5.4864, 3.048, 3.6576,                   !- Vertex 3 Zcoordinate
    5.4864, 3.048, 5.4864;                   !- Vertex 4 Zcoordinate

Material,
  Wall Continuous Insulation,  !- Name
  MediumSmooth,               !- Roughness
  0.038037600000000005,  !- Thickness {m}
  0.0288,  !- Conductivity {W/m-K}
  32,                      !- Density {kg/m3}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Extruded polystyrene, smooth skin - aged 180 days)
  1465;                     !- Specific Heat {J/kg-K}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Extruded polystyrene, smooth skin - aged 180 days)

Material,
  Wall Cavity Effective Insulation,  !- Name
  VeryRough,               !- Roughness
  0.08889999999999999,  !- Thickness {m}
  0.08389829384297165,  !- Conductivity {W/m-K}
  8.17,                      !- Density {kg/m3}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Glass-fiber batts)
  837;                     !- Specific Heat {J/kg-K}(2013 ASHRAE Handbook - Fundamentals Section 26 Table 1 - Glass-fiber batts)

Construction,
  Exterior Wall,           !- Name
  Stucco,  !- Layer
  Wall Continuous Insulation,  !- Layer
  Wall Cavity Effective Insulation,  !- Layer
  Gypsum 5/8in;  !- Layer

Construction,
  Exterior Window,         !- Name
  Theoretical Glazing;     !- Outside Layer

Material,
  Gypsum 5/8in,           !- Name
  MediumSmooth,            !- Roughness
  0.016,                   !- Thickness {m}
  0.1622,                  !- Conductivity {W/m-K}
  800,                     !- Density {kg/m3}
  1090,                    !- Specific Heat {J/kg-K}
  0.9,                     !- Thermal Absorptance
  0.7,                     !- Solar Absorptance
  0.7;                     !- Visible Absorptance

Material,
  Stucco,           !- Name
  Smooth,                  !- Roughness
  0.01015,                 !- Thickness {m}
  0.72,                    !- Conductivity {W/m-K}
  1856,                    !- Density {kg/m3}
  840,                     !- Specific Heat {J/kg-K}
  0.9,                     !- Thermal Absorptance
  0.7,                     !- Solar Absorptance
  0.7;                     !- Visible Absorptance

WindowMaterial:SimpleGlazingSystem,
  Theoretical Glazing,     !- Name
  3.1232254400908577,  !- U-Factor {W/m2-K}
  0.4,  !- Solar Heat Gain Coefficient
  0.4;  !- Visible Transmittance
)IDF";

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData(*state);
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(*state, FoundError); // read project control data
    EXPECT_FALSE(FoundError);                                      // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

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

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(*state, FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                            // expect no errors

    //	compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    state->dataGlobal->BeginSimFlag = true;
    HeatBalanceManager::InitHeatBalance(*state);
    EXPECT_FALSE(FoundError);

    if (state->dataSolarShading->penumbra) {
        SolarShading::AllocateModuleArrays(*state);
        SolarShading::DetermineShadowingCombinations(*state);

        std::string error_string = delimited_string({"** Severe  ** Problem in interior solar distribution calculation (CHKBKS)"});

        EXPECT_FALSE(match_err_stream(error_string));

        error_string =
            delimited_string({"** Severe  ** DetermineShadowingCombinations: There are 1 surfaces which are casting surfaces and are non-convex."});

        EXPECT_FALSE(match_err_stream(error_string));
    } else {
        EXPECT_FALSE(false) << "Machine cannot create a valid OpenGL instance."; // Replace with GTEST_SKIP() when it's released?
    }
}

TEST_F(EnergyPlusFixture, SolarShadingTest_selectActiveWindowShadingControl)
{
    state->dataSurface->TotSurfaces = 2;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    int curSurface = 1;
    state->dataSurface->Surface(curSurface).windowShadingControlList.push_back(57);

    int curIndexActiveWindowShadingControl = selectActiveWindowShadingControlIndex(*state, curSurface);
    int activeWindowShadingControl = state->dataSurface->Surface(curSurface).windowShadingControlList[curIndexActiveWindowShadingControl];
    EXPECT_EQ(activeWindowShadingControl, 57);

    curSurface = 2;
    state->dataSurface->Surface(curSurface).windowShadingControlList.push_back(1);
    state->dataSurface->Surface(curSurface).windowShadingControlList.push_back(2);
    state->dataSurface->Surface(curSurface).windowShadingControlList.push_back(3);

    state->dataSurface->WindowShadingControl.allocate(3);
    state->dataSurface->WindowShadingControl(1).Schedule = 1;
    state->dataSurface->WindowShadingControl(2).Schedule = 2;
    state->dataSurface->WindowShadingControl(3).Schedule = 3;

    state->dataScheduleMgr->Schedule.allocate(3);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 0;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 1;

    curIndexActiveWindowShadingControl = selectActiveWindowShadingControlIndex(*state, curSurface);
    activeWindowShadingControl = state->dataSurface->Surface(curSurface).windowShadingControlList[curIndexActiveWindowShadingControl];
    EXPECT_EQ(activeWindowShadingControl, 3);

    state->dataScheduleMgr->Schedule(1).CurrentValue = 0;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 1;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 0;

    curIndexActiveWindowShadingControl = selectActiveWindowShadingControlIndex(*state, curSurface);
    activeWindowShadingControl = state->dataSurface->Surface(curSurface).windowShadingControlList[curIndexActiveWindowShadingControl];
    EXPECT_EQ(activeWindowShadingControl, 2);

    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 0;

    curIndexActiveWindowShadingControl = selectActiveWindowShadingControlIndex(*state, curSurface);
    activeWindowShadingControl = state->dataSurface->Surface(curSurface).windowShadingControlList[curIndexActiveWindowShadingControl];
    EXPECT_EQ(activeWindowShadingControl, 1);
}

TEST_F(EnergyPlusFixture, SolarShadingTest_ShadingFlagTest)
{
    WinShadingType ShadingFlag = WinShadingType::IntShade;
    EXPECT_TRUE(IS_SHADED(ShadingFlag));
    EXPECT_TRUE(ANY_SHADE(ShadingFlag));
    EXPECT_TRUE(ANY_SHADE_SCREEN(ShadingFlag));
    EXPECT_TRUE(ANY_INTERIOR_SHADE_BLIND(ShadingFlag));
    EXPECT_FALSE(ANY_BLIND(ShadingFlag));
    EXPECT_FALSE(ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadingFlag));
    EXPECT_FALSE(ANY_BETWEENGLASS_SHADE_BLIND(ShadingFlag));

    ShadingFlag = WinShadingType::ExtBlind;
    EXPECT_TRUE(ANY_BLIND(ShadingFlag));
    EXPECT_TRUE(ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadingFlag));

    ShadingFlag = WinShadingType::GlassConditionallyLightened;
    EXPECT_FALSE(IS_SHADED_NO_GLARE_CTRL(ShadingFlag));
}
