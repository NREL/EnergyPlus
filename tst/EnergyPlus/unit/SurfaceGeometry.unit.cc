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

// EnergyPlus::SurfaceGeometry Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::HeatBalanceManager;

TEST_F(EnergyPlusFixture, BaseSurfaceRectangularTest)
{

    // Test base surfaces for rectangular shape in ProcessSurfaceVertices

    state->dataSurface->TotSurfaces = 5;
    state->dataSurface->MaxVerticesPerSurface = 5;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->ShadeV.allocate(state->dataSurface->TotSurfaces);
    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; ++SurfNum) {
        state->dataSurface->Surface(SurfNum).Vertex.allocate(state->dataSurface->MaxVerticesPerSurface);
    }

    bool ErrorsFound(false);
    int ThisSurf(0);

    // Surface 1 - Rectangle
    ThisSurf = 1;
    state->dataSurface->Surface(ThisSurf).Azimuth = 180.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 90.0;
    state->dataSurface->Surface(ThisSurf).Sides = 4;
    state->dataSurface->Surface(ThisSurf).GrossArea = 10.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 2.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(*state, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Rectangle, state->dataSurface->Surface(ThisSurf).Shape));

    // Surface 2 - Isosceles Trapezoid
    ThisSurf = 2;
    state->dataSurface->Surface(ThisSurf).Azimuth = 180.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 90.0;
    state->dataSurface->Surface(ThisSurf).Sides = 4;
    state->dataSurface->Surface(ThisSurf).GrossArea = 8.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 4.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 2.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 1.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(*state, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Quadrilateral, state->dataSurface->Surface(ThisSurf).Shape));

    // Surface 3 - Parallelogram
    ThisSurf = 3;
    state->dataSurface->Surface(ThisSurf).Azimuth = 180.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 90.0;
    state->dataSurface->Surface(ThisSurf).Sides = 4;
    state->dataSurface->Surface(ThisSurf).GrossArea = 10.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 7.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 2.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 2.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(*state, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Quadrilateral, state->dataSurface->Surface(ThisSurf).Shape));

    // Surface 4 - Triangle
    ThisSurf = 4;
    state->dataSurface->Surface(ThisSurf).Azimuth = 180.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 90.0;
    state->dataSurface->Surface(ThisSurf).Sides = 3;
    state->dataSurface->Surface(ThisSurf).GrossArea = 10.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 2.0;

    ProcessSurfaceVertices(*state, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Triangle, state->dataSurface->Surface(ThisSurf).Shape));

    // Surface 5 - Polygon
    ThisSurf = 5;
    state->dataSurface->Surface(ThisSurf).Azimuth = 180.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 90.0;
    state->dataSurface->Surface(ThisSurf).Sides = 5;
    state->dataSurface->Surface(ThisSurf).GrossArea = 10.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 7.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 2.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 3.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 5.0;

    state->dataSurface->Surface(ThisSurf).Vertex(5).x = 1.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).y = 0.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).z = 3.0;

    ProcessSurfaceVertices(*state, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Polygonal, state->dataSurface->Surface(ThisSurf).Shape));
}

TEST_F(EnergyPlusFixture, DataSurfaces_SurfaceShape)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        " BuildingSurface:Detailed,",
        "    Surface 1 - Triangle,    !- Name",
        "    Floor,                   !- Surface Type",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
        "    Zone1,                   !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    ,                        !- Number of Vertices",
        "    0.0, 0.0, 0.0,           !- Vertex 1 X-coordinate {m}",
        "    1.0, 2.0, 0.0,           !- Vertex 2 X-coordinate {m}",
        "    2.0, 0.0, 0.0;           !- Vertex 3 X-coordinate {m}",

        " BuildingSurface:Detailed,",
        "    Surface 2 - Quadrilateral,  !- Name",
        "    Floor,                   !- Surface Type",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
        "    Zone1,                   !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    ,                        !- Number of Vertices",
        "    -73.4395447868102,       !- Vertex 1 X-coordinate {m}",
        "    115.81641271866,         !- Vertex 1 Y-coordinate {m}",
        "    -4.90860981523342e-014,  !- Vertex 1 Z-coordinate {m}",
        "    -58.0249751030646,       !- Vertex 2 X-coordinate {m}",
        "    93.1706338416311,        !- Vertex 2 Y-coordinate {m}",
        "    -6.93120848813091e-014,  !- Vertex 2 Z-coordinate {m}",
        "    -68.9295447868101,       !- Vertex 3 X-coordinate {m}",
        "    74.3054685889134,        !- Vertex 3 Y-coordinate {m}",
        "    -6.06384403665968e-014,  !- Vertex 3 Z-coordinate {m}",
        "    -58.0345461881513,       !- Vertex 4 X-coordinate {m}",
        "    93.1761597101821,        !- Vertex 4 Y-coordinate {m}",
        "    -6.9300904918858e-014;   !- Vertex 4 Z-coordinate {m}",

        " BuildingSurface:Detailed,",
        "    Surface 3 - Rectangle,   !- Name",
        "    Wall,                    !- Surface Type",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
        "    Zone1,                   !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    ,                        !- Number of Vertices",
        "    0.0, 0.0, 0.0,           !- X,Y,Z ==> Vertex 1 {m}",
        "    1.0, 0.0, 0.0,           !- X,Y,Z ==> Vertex 2 {m}",
        "    1.0, 0.0, 1.0,           !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0, 0.0, 1.0;           !- X,Y,Z ==> Vertex 4 {m}",

        " FenestrationSurface:Detailed,",
        "    Surface 4 - RectangularDoorWindow,    !- Name",
        "    Window,                  !- Surface Type",
        "    SINGLE PANE HW WINDOW,   !- Construction Name",
        "    Surface 3 - Rectangle,   !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    Autocalculate,           !- Number of Vertices",
        "    0.2, 0.0, 0.2,           !- X,Y,Z ==> Vertex 1 {m}",
        "    0.8, 0.0, 0.2,           !- X,Y,Z ==> Vertex 2 {m}",
        "    0.8, 0.0, 0.8,           !- X,Y,Z ==> Vertex 3 {m}",
        "    0.2, 0.0, 0.8;           !- X,Y,Z ==> Vertex 4 {m}",

        " Shading:Overhang:Projection,",
        "    Surface 5 - RectangularOverhang,  !- Name",
        "    Surface 9 - TriangularDoor,       !- Window or Door Name",
        "    .01,                     !- Height above Window or Door {m}",
        "    91,                      !- Tilt Angle from Window/Door {deg}",
        "    .01,                     !- Left extension from Window/Door Width {m}",
        "    .01,                     !- Right extension from Window/Door Width {m}",
        "    .2;                      !- Depth as Fraction of Window/Door Height {dimensionless}",

        " Shading:Fin:Projection,",
        "    Surface 6 - RectangularLeftFin,    !- Name",
        "    Surface 3 - Rectangle,   !- Window or Door Name",
        "    .01,                     !- Left Extension from Window/Door {m}",
        "    .01,                     !- Left Distance Above Top of Window {m}",
        "    .01,                     !- Left Distance Below Bottom of Window {m}",
        "    90,                      !- Left Tilt Angle from Window/Door {deg}",
        "    .1,                      !- Left Depth as Fraction of Window/Door Width {dimensionless}",
        "    0,                       !- Right Extension from Window/Door {m}",
        "    0,                       !- Right Distance Above Top of Window {m}",
        "    0,                       !- Right Distance Below Bottom of Window {m}",
        "    0,                       !- Right Tilt Angle from Window/Door {deg}",
        "    0;                       !- Right Depth as Fraction of Window/Door Width {dimensionless}",

        " Shading:Fin:Projection,",
        "    Surface 7 - RectangularRightFin,   !- Name",
        "    Surface 3 - Rectangle,   !- Window or Door Name",
        "    0,                       !- Left Extension from Window/Door {m}",
        "    0,                       !- Left Distance Above Top of Window {m}",
        "    0,                       !- Left Distance Below Bottom of Window {m}",
        "    0,                       !- Left Tilt Angle from Window/Door {deg}",
        "    0,                       !- Left Depth as Fraction of Window/Door Width {dimensionless}",
        "    .01,                     !- Right Extension from Window/Door {m}",
        "    .01,                     !- Right Distance Above Top of Window {m}",
        "    .01,                     !- Right Distance Below Bottom of Window {m}",
        "    90,                      !- Right Tilt Angle from Window/Door {deg}",
        "    .1;                      !- Right Depth as Fraction of Window/Door Width {dimensionless}",

        " FenestrationSurface:Detailed,",
        "    Surface 8 - TriangularWindow,    !- Name",
        "    Window,                  !- Surface Type",
        "    SINGLE PANE HW WINDOW,   !- Construction Name",
        "    Surface 3 - Rectangle,   !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    Autocalculate,           !- Number of Vertices",
        "    0.05, 0.0, 0.05,         !- X,Y,Z ==> Vertex 1 {m}",
        "    0.15, 0.0, 0.05,         !- X,Y,Z ==> Vertex 2 {m}",
        "    0.10, 0.0, 0.15;         !- X,Y,Z ==> Vertex 3 {m}",

        " FenestrationSurface:Detailed,",
        "    Surface 9 - TriangularDoor,      !- Name",
        "    Door,                    !- Surface Type",
        "    External door,           !- Construction Name",
        "    Surface 3 - Rectangle,   !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    3,                       !- Number of Vertices",
        "    0.80, 0.0, 0.05,         !- X,Y,Z ==> Vertex 1 {m}",
        "    0.95, 0.0, 0.05,         !- X,Y,Z ==> Vertex 2 {m}",
        "    0.90, 0.0, 0.15;         !- X,Y,Z ==> Vertex 3 {m}",

        " BuildingSurface:Detailed,",
        "    Surface 10 - Polygonal,  !- Name",
        "    Floor,                   !- Surface Type",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
        "    Zone1,                   !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    ,                        !- Number of Vertices",
        "    0.0, 0.0, 0.0,           !- Vertex 1 X-coordinate {m}",
        "    0.0, 1.0, 0.0,           !- Vertex 2 X-coordinate {m}",
        "    0.5, 2.0, 0.0,           !- Vertex 3 X-coordinate {m}",
        "    1.0, 1.0, 0.0,           !- Vertex 4 X-coordinate {m}",
        "    1.0, 0.0, 0.0;           !- Vertex 5 X-coordinate {m}",

        " Zone,",
        "    Zone1,                   !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0.0,                     !- X Origin {m}",
        "    0.0,                     !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    ,                        !- Type",
        "    ,                        !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    ,                        !- Volume {m3}",
        "    ,                        !- Floor Area {m2}",
        "    ,                        !- Zone Inside Convection Algorithm",
        "    ,                        !- Zone Outside Convection Algorithm",
        "    No;                      !- Part of Total Floor Area",

        " Construction,",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Name",
        "    MAT-CC05 4 HW CONCRETE,  !- Outside Layer",
        "    CP02 CARPET PAD;         !- Layer 2",

        " Construction,",
        "    External door,           !- Name",
        "    Painted Oak;             !- Outside Layer",

        " Material,",
        "    MAT-CC05 4 HW CONCRETE,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1016,                  !- Thickness {m}",
        "    1.311,                   !- Conductivity {W/m-K}",
        "    2240,                    !- Density {kg/m3}",
        "    836.800000000001,        !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.85,                    !- Solar Absorptance",
        "    0.85;                    !- Visible Absorptance",

        " Material,",
        "    Painted Oak,             !- Name",
        "    Rough,                   !- Roughness",
        "    0.035,                   !- Thickness {m}",
        "    0.19,                    !- Conductivity {W/m-K}",
        "    700,                     !- Density {kg/m3}",
        "    2390,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.5,                     !- Solar Absorptance",
        "    0.5;                     !- Visible Absorptance",

        " Material:NoMass,",
        "    CP02 CARPET PAD,         !- Name",
        "    Smooth,                  !- Roughness",
        "    0.1,                     !- Thermal Resistance {m2-K/W}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.8,                     !- Solar Absorptance",
        "    0.8;                     !- Visible Absorptance",

        " Construction,",
        "    SINGLE PANE HW WINDOW,   !- Name",
        "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer",

        " WindowMaterial:Glazing,",
        "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    6.0000001E-03,           !- Thickness {m}",
        "    0.7750000,               !- Solar Transmittance at Normal Incidence",
        "    7.1000002E-02,           !- Front Side Solar Reflectance at Normal Incidence",
        "    7.1000002E-02,           !- Back Side Solar Reflectance at Normal Incidence",
        "    0.8810000,               !- Visible Transmittance at Normal Incidence",
        "    7.9999998E-02,           !- Front Side Visible Reflectance at Normal Incidence",
        "    7.9999998E-02,           !- Back Side Visible Reflectance at Normal Incidence",
        "    0,                       !- Infrared Transmittance at Normal Incidence",
        "    0.8400000,               !- Front Side Infrared Hemispherical Emissivity",
        "    0.8400000,               !- Back Side Infrared Hemispherical Emissivity",
        "    0.9000000;               !- Conductivity {W/m-K}",

        "SurfaceConvectionAlgorithm:Inside,TARP;",

        "SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "HeatBalanceAlgorithm,ConductionTransferFunction;",

        "ZoneAirHeatBalanceAlgorithm,",
        "    AnalyticalSolution;      !- Algorithm",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(*state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);                  // expect no errors

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    GetSurfaceData(*state, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);           // expect no errors

    // compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::AllocateSurfaceArrays(*state);

    //  Adding additional surfaces will change the index of the following based on where the surfaces are added in the array.
    //	If adding new tests, break here and look at EnergyPlus::state->dataSurface->Surface to see the order.

    //	enum surfaceShape:Triangle = 1
    //	"Surface 1 - Triangle"
    int surfNum = UtilityRoutines::FindItemInList("SURFACE 1 - TRIANGLE", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Triangle, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:Quadrilateral = 2
    //	"Surface 2 - Quadrilateral"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 2 - QUADRILATERAL", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Quadrilateral, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:Rectangle = 3
    //	"Surface 3 - Rectangle"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 3 - RECTANGLE", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Rectangle, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:RectangularDoorWindow = 4
    //	"Surface 4 - RectangularDoorWindow"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 4 - RECTANGULARDOORWINDOW", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::RectangularDoorWindow, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:RectangularOverhang = 5
    //	"Surface 5 - RectangularOverhang"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 5 - RECTANGULAROVERHANG", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(compare_enums(SurfaceShape::RectangularOverhang,
                               state->dataSurface->Surface(surfNum).Shape,
                               false)); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularLeftFin = 6
    //	"Surface 6 - RectangularLeftFin"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 6 - RECTANGULARLEFTFIN Left", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(compare_enums(SurfaceShape::RectangularLeftFin,
                               state->dataSurface->Surface(surfNum).Shape,
                               false)); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularRightFin = 7
    //	"Surface 7 - RectangularRightFin"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 7 - RECTANGULARRIGHTFIN Right", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(compare_enums(SurfaceShape::RectangularRightFin,
                               state->dataSurface->Surface(surfNum).Shape,
                               false)); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:TriangularWindow = 8
    //	"Surface 8 - TriangularWindow"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 8 - TRIANGULARWINDOW", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::TriangularWindow, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:TriangularDoor = 9
    //	"Surface 9 - TriangularDoor"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 9 - TRIANGULARDOOR", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::TriangularDoor, state->dataSurface->Surface(surfNum).Shape));

    //	enum surfaceShape:Polygonal = 10
    //	"Surface 10 - Polygonal"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 10 - POLYGONAL", state->dataSurface->Surface);
    ProcessSurfaceVertices(*state, surfNum, ErrorsFound);
    EXPECT_TRUE(compare_enums(SurfaceShape::Polygonal, state->dataSurface->Surface(surfNum).Shape));
}

TEST_F(EnergyPlusFixture, ConfirmCheckSubSurfAzTiltNorm)
{
    SurfaceData BaseSurface;
    SurfaceData SubSurface;
    bool surfaceError;

    // Case 1 - Base surface and subsurface face the same way - should be no error message and no surfaceError
    surfaceError = false;
    BaseSurface.Azimuth = 0.;
    BaseSurface.Tilt = 0.;
    BaseSurface.NewellSurfaceNormalVector.x = 0.;
    BaseSurface.NewellSurfaceNormalVector.y = 0.;
    BaseSurface.NewellSurfaceNormalVector.z = 1.;

    SubSurface.Azimuth = 0.;
    SubSurface.Tilt = 0.;
    SubSurface.NewellSurfaceNormalVector.x = 0.;
    SubSurface.NewellSurfaceNormalVector.y = 0.;
    SubSurface.NewellSurfaceNormalVector.z = 1.;
    checkSubSurfAzTiltNorm(*state, BaseSurface, SubSurface, surfaceError);
    EXPECT_FALSE(surfaceError);
    EXPECT_FALSE(has_err_output());

    // Case 2 - Base surface and subsurface face the opposite way - should be error message and surfaceError=true
    surfaceError = false;
    SubSurface.Azimuth = 180.;
    SubSurface.Tilt = 180.;
    SubSurface.NewellSurfaceNormalVector.x = 1.;
    SubSurface.NewellSurfaceNormalVector.y = 0.;
    SubSurface.NewellSurfaceNormalVector.z = 0.;
    checkSubSurfAzTiltNorm(*state, BaseSurface, SubSurface, surfaceError);
    EXPECT_TRUE(surfaceError);
    EXPECT_TRUE(has_err_output());

    // Case 3 - Base surface is horizontal and subsurface is different by 45 degrees azimuth - should be no warning message and surfaceError=false
    surfaceError = false;
    SubSurface.Azimuth = 45.;
    SubSurface.Tilt = 0.;
    SubSurface.NewellSurfaceNormalVector.x = 0.;
    SubSurface.NewellSurfaceNormalVector.y =
        1.; // This doesn't match the tilt and azimuth, but want it to be different so tilt and azimuth tests are executed
    SubSurface.NewellSurfaceNormalVector.z = 1.;
    checkSubSurfAzTiltNorm(*state, BaseSurface, SubSurface, surfaceError);
    EXPECT_FALSE(surfaceError);
    EXPECT_FALSE(has_err_output());

    // Case 4 - Base surface is not horizontal and subsurface is different by 45 degrees azimuth and tilt - should be warning error message but
    // surfaceError=false
    surfaceError = false;
    BaseSurface.Azimuth = 90.;
    BaseSurface.Tilt = 90.;
    BaseSurface.NewellSurfaceNormalVector.x = 1.;
    BaseSurface.NewellSurfaceNormalVector.y = 0.;
    BaseSurface.NewellSurfaceNormalVector.z = 0.;

    SubSurface.Azimuth = 45.;
    SubSurface.Tilt = 45.;
    SubSurface.NewellSurfaceNormalVector.x = 1.;
    SubSurface.NewellSurfaceNormalVector.y = 1.;
    SubSurface.NewellSurfaceNormalVector.z = 1.;
    checkSubSurfAzTiltNorm(*state, BaseSurface, SubSurface, surfaceError);
    EXPECT_FALSE(surfaceError);
    EXPECT_TRUE(has_err_output());
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_MakeMirrorSurface)
{
    std::string const idf_objects = delimited_string({
        "BuildingSurface:Detailed,",
        " FRONT-1,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        "    ,                        !- Space Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " SunExposed,               !- Sun Exposure",
        " WindExposed,              !- Wind Exposure",
        " 0.50000,                  !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
        " ",
        "Zone,",
        "  Space,                   !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.0,                     !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  6.,                      !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Construction,",
        " INT-WALL-1,               !- Name",
        " GP02;                     !- Outside Layer",
        " ",
        "Material,",
        " GP02,                     !- Name",
        " MediumSmooth,             !- Roughness",
        " 1.5900001E-02,            !- Thickness{ m }",
        " 0.1600000,                !- Conductivity{ W / m - K }",
        " 801.0000,                 !- Density{ kg / m3 }",
        " 837.0000,                 !- Specific Heat{ J / kg - K }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.7500000,                !- Solar Absorptance",
        " 0.7500000;                !- Visible Absorptance",
        " ",
        "  Timestep, 4;",
        " ",
        "BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
        " ",
        "SimulationControl, YES, NO, NO, YES, NO;",
        " ",
        "  Site:Location,",
        "    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
        "    25.82,                   !- Latitude {deg}",
        "    -80.30,                  !- Longitude {deg}",
        "    -5.00,                   !- Time Zone {hr}",
        "    11;                      !- Elevation {m}",
        " ",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool FoundError = false;
    GetMaterialData(*state, FoundError);
    GetConstructData(*state, FoundError);
    GetZoneData(*state, FoundError); // Read Zone data from input file
    state->dataHeatBal->AnyCTF = true;
    SetupZoneGeometry(*state, FoundError); // this calls GetSurfaceData()

    EXPECT_FALSE(FoundError);

    // test coordinate on existing surface
    EXPECT_EQ(state->dataSurface->TotSurfaces, 1);

    EXPECT_EQ(state->dataSurface->Surface(state->dataSurface->TotSurfaces).Name, "FRONT-1");

    // move surface to SurfaceTmp since MakeMirrorSurface uses that array
    state->dataSurfaceGeometry->SurfaceTmp.allocate(10);
    state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces) = state->dataSurface->Surface(state->dataSurface->TotSurfaces);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Name, "FRONT-1");

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).x, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).z, 2.4);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).x, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).z, 0.);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).x, 30.5);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).z, 0.);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).x, 30.5);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).z, 2.4);

    MakeMirrorSurface(
        *state, state->dataSurface->TotSurfaces); // This call increments TotSurfaces so the references after this are for the created mirror surface

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Name, "Mir-FRONT-1");

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).x, 30.5);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(1).z, 2.4);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).x, 30.5);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(2).z, 0.);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).x, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(3).z, 0.);

    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).x, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).y, 0.);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(state->dataSurface->TotSurfaces).Vertex(4).z, 2.4);
}

TEST_F(EnergyPlusFixture, SurfacesGeometry_CalcSurfaceCentroid_NonconvexRealisticZ)
{
    state->dataSurface->TotSurfaces = 10;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    state->dataSurface->Surface(1).Class = DataSurfaces::SurfaceClass::Roof;
    state->dataSurface->Surface(1).GrossArea = 1000.;
    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.allocate(4);

    state->dataSurface->Surface(1).Vertex(1).x = 2000.;
    state->dataSurface->Surface(1).Vertex(1).y = -1000.;
    state->dataSurface->Surface(1).Vertex(1).z = 10.;

    state->dataSurface->Surface(1).Vertex(2).x = 1.;
    state->dataSurface->Surface(1).Vertex(2).y = 0.;
    state->dataSurface->Surface(1).Vertex(2).z = 10.;

    state->dataSurface->Surface(1).Vertex(3).x = 2000.;
    state->dataSurface->Surface(1).Vertex(3).y = 1000.;
    state->dataSurface->Surface(1).Vertex(3).z = 10.;

    state->dataSurface->Surface(1).Vertex(4).x = 0.;
    state->dataSurface->Surface(1).Vertex(4).y = 0.;
    state->dataSurface->Surface(1).Vertex(4).z = 10.;

    CalcSurfaceCentroid(*state);

    EXPECT_EQ(state->dataSurface->Surface(1).Centroid.x, 667.);
    EXPECT_EQ(state->dataSurface->Surface(1).Centroid.y, 0.);
    EXPECT_EQ(state->dataSurface->Surface(1).Centroid.z, 10.);
}

TEST_F(EnergyPlusFixture, MakeEquivalentRectangle)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "FenestrationSurface:Detailed,                                   ",
        "	Surface-1-Rectangle,     !- Name                             ",
        "	Window,                  !- Surface Type                     ",
        "	SINGLE PANE HW WINDOW,   !- Construction Name                ",
        "	WallExample,             !- Building Surface Name            ",
        "	,                        !- Outside Boundary Condition Object",
        "	0.50000,                 !- View Factor to Ground            ",
        "	,                        !- Frame and Divider Name           ",
        "	1,                       !- Multiplier                       ",
        "	4,                       !- Number of Vertices               ",
        "	0.0, 11.4, 2.1,          !- X,Y,Z ==> Vertex 1 {m}           ",
        "	0.0, 11.4, 0.9,          !- X,Y,Z ==> Vertex 2 {m}           ",
        "	0.0, 3.8,  0.9,          !- X,Y,Z ==> Vertex 3 {m}           ",
        "	0.0, 3.8,  2.1;          !- X,Y,Z ==> Vertex 4 {m}           ",
        "                                                                ",
        "FenestrationSurface:Detailed,                                   ",
        "	Surface-2-Trapzoid,      !- Name                             ",
        "	Window,                  !- Surface Type                     ",
        "	SINGLE PANE HW WINDOW,   !- Construction Name                ",
        "	WallExample,             !- Building Surface Name            ",
        "	,                        !- Outside Boundary Condition Object",
        "	0.50000,                 !- View Factor to Ground            ",
        "	,                        !- Frame and Divider Name           ",
        "	1,                       !- Multiplier                       ",
        "	4,                       !- Number of Vertices               ",
        "	0.0, 11.2, 2.1,          !- X,Y,Z ==> Vertex 1 {m}           ",
        "	0.0, 11.6, 0.9,          !- X,Y,Z ==> Vertex 2 {m}           ",
        "	0.0, 3.6,  0.9,          !- X,Y,Z ==> Vertex 3 {m}           ",
        "	0.0, 4.0,  2.1;          !- X,Y,Z ==> Vertex 4 {m}           ",
        "                                                                ",
        "FenestrationSurface:Detailed,                                   ",
        "	Surface-3-parallelogram, !- Name                             ",
        "	Window,                  !- Surface Type                     ",
        "	SINGLE PANE HW WINDOW,   !- Construction Name                ",
        "	WallExample,             !- Building Surface Name            ",
        "	,                        !- Outside Boundary Condition Object",
        "	0.50000,                 !- View Factor to Ground            ",
        "	,                        !- Frame and Divider Name           ",
        "	1,                       !- Multiplier                       ",
        "	4,                       !- Number of Vertices               ",
        "	0.0, 11.4, 2.1,          !- X,Y,Z ==> Vertex 1 {m}           ",
        "	0.0, 12.4, 0.9,          !- X,Y,Z ==> Vertex 2 {m}           ",
        "	0.0, 4.8,  0.9,          !- X,Y,Z ==> Vertex 3 {m}           ",
        "	0.0, 3.8,  2.1;          !- X,Y,Z ==> Vertex 4 {m}           ",
        "                                                                ",
        "BuildingSurface:Detailed,                                       ",
        "	WallExample,   !- Name                                       ",
        "	Wall,                    !- Surface Type                     ",
        "	ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name     ",
        "	ZoneExample,             !- Zone Name                        ",
        "   ,                        !- Space Name",
        "	Outdoors,                !- Outside Boundary Condition       ",
        "	,                        !- Outside Boundary Condition Object",
        "	NoSun,                   !- Sun Exposure                     ",
        "	NoWind,                  !- Wind Exposure                    ",
        "	,                        !- View Factor to Ground            ",
        "	,                        !- Number of Vertices               ",
        "	0.0, 15.2, 2.4,          !- X,Y,Z ==> Vertex 1 {m}           ",
        "	0.0, 15.2, 0.0,          !- X,Y,Z ==> Vertex 2 {m}           ",
        "	0.0, 0.0,  0.0,          !- X,Y,Z ==> Vertex 3 {m}           ",
        "	0.0, 0.0,  2.4;          !- X,Y,Z ==> Vertex 4 {m}           ",
        "	                                                             ",
        "BuildingSurface:Detailed,                                       ",
        "	FloorExample,            !- Name                             ",
        "	Floor,                   !- Surface Type                     ",
        "	ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name     ",
        "	ZoneExample,             !- Zone Name                        ",
        "   ,                        !- Space Name",
        "	Outdoors,                !- Outside Boundary Condition       ",
        "	,                        !- Outside Boundary Condition Object",
        "	NoSun,                   !- Sun Exposure                     ",
        "	NoWind,                  !- Wind Exposure                    ",
        "	,                        !- View Factor to Ground            ",
        "	,                        !- Number of Vertices               ",
        "	0.0, 0.0,  0.0,          !- Vertex 1 X-coordinate {m}        ",
        "	5.0, 15.2, 0.0,          !- Vertex 2 X-coordinate {m}        ",
        "	0.0, 15.2, 0.0;          !- Vertex 3 X-coordinate {m}        ",
        "                                                                ",
        "Zone,                                                           ",
        "	ZoneExample,             !- Name                             ",
        "	0,                       !- Direction of Relative North {deg}",
        "	0.0,                     !- X Origin {m}                     ",
        "	0.0,                     !- Y Origin {m}                     ",
        "	0.0,                     !- Z Origin {m}                     ",
        "	,                        !- Type                             ",
        "	,                        !- Multiplier                       ",
        "	,                        !- Ceiling Height {m}               ",
        "	,                        !- Volume {m3}                      ",
        "	,                        !- Floor Area {m2}                  ",
        "	,                        !- Zone Inside Convection Algorithm ",
        "	,                        !- Zone Outside Convection Algorithm",
        "	No;                      !- Part of Total Floor Area         ",
        "                                                                ",
        "Construction,                                                   ",
        "	ExtSlabCarpet 4in ClimateZone 1-8,  !- Name                  ",
        "	MAT-CC05 4 HW CONCRETE,  !- Outside Layer                    ",
        "	CP02 CARPET PAD;         !- Layer 2                          ",
        "                                                                ",
        "Construction,                                                   ",
        "	SINGLE PANE HW WINDOW,   !- Name                             ",
        "	CLEAR 3MM;  !- Outside Layer                                 ",
        "                                                                ",
        "Material,                                                       ",
        "	MAT-CC05 4 HW CONCRETE,  !- Name                             ",
        "	Rough,                   !- Roughness                        ",
        "	0.1016,                  !- Thickness {m}                    ",
        "	1.311,                   !- Conductivity {W/m-K}             ",
        "	2240,                    !- Density {kg/m3}                  ",
        "	836.800000000001,        !- Specific Heat {J/kg-K}           ",
        "	0.9,                     !- Thermal Absorptance              ",
        "	0.85,                    !- Solar Absorptance                ",
        "	0.85;                    !- Visible Absorptance              ",
        "                                                                ",
        "Material:NoMass,                                                ",
        "	CP02 CARPET PAD,         !- Name                             ",
        "	Smooth,                  !- Roughness                        ",
        "	0.1,                     !- Thermal Resistance {m2-K/W}      ",
        "	0.9,                     !- Thermal Absorptance              ",
        "	0.8,                     !- Solar Absorptance                ",
        "	0.8;                     !- Visible Absorptance              ",
        "                                                                ",
        "WindowMaterial:Glazing,                                                          ",
        "	CLEAR 3MM,               !- Name                                              ",
        "	SpectralAverage,         !- Optical Data Type                                 ",
        "	,                        !- Window Glass Spectral Data Set Name               ",
        "	0.003,                   !- Thickness {m}                                     ",
        "	0.837,                   !- Solar Transmittance at Normal Incidence           ",
        "	0.075,                   !- Front Side Solar Reflectance at Normal Incidence  ",
        "	0.075,                   !- Back Side Solar Reflectance at Normal Incidence   ",
        "	0.898,                   !- Visible Transmittance at Normal Incidence         ",
        "	0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "	0.081,                   !- Back Side Visible Reflectance at Normal Incidence ",
        "	0.0,                     !- Infrared Transmittance at Normal Incidence        ",
        "	0.84,                    !- Front Side Infrared Hemispherical Emissivity      ",
        "	0.84,                    !- Back Side Infrared Hemispherical Emissivity       ",
        "	0.9;                     !- Conductivity {W/m-K}                              ",
    });

    // Prepare data for the test
    ASSERT_TRUE(process_idf(idf_objects));
    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);
    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);
    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);
    GetProjectControlData(*state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);
    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    GetSurfaceData(*state, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);           // expect no errors

    // For each surface Run the test then Check the result
    // (1) rectangle window
    int surfNum = UtilityRoutines::FindItemInList("SURFACE-1-RECTANGLE", state->dataSurface->Surface);
    MakeEquivalentRectangle(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(7.60, state->dataSurface->Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.20, state->dataSurface->Surface(surfNum).Height, 0.01);
    // (2) trapzoid window
    surfNum = UtilityRoutines::FindItemInList("SURFACE-2-TRAPZOID", state->dataSurface->Surface);
    MakeEquivalentRectangle(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(7.80, state->dataSurface->Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.17, state->dataSurface->Surface(surfNum).Height, 0.01);
    // (3) parallelogram window
    surfNum = UtilityRoutines::FindItemInList("SURFACE-3-PARALLELOGRAM", state->dataSurface->Surface);
    MakeEquivalentRectangle(*state, surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(8.08, state->dataSurface->Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.13, state->dataSurface->Surface(surfNum).Height, 0.01);
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_distance)
{

    DataVectorTypes::Vector a;
    DataVectorTypes::Vector b;

    a.x = 0.;
    a.y = 0.;
    a.z = 0.;

    b.x = 0.;
    b.y = 0.;
    b.z = 0.;

    EXPECT_EQ(0., distance(a, b));

    b.x = 1.;
    b.y = 1.;
    b.z = 1.;

    EXPECT_NEAR(1.7321, distance(a, b), 0.0001);

    b.x = 7.;
    b.y = 11.;
    b.z = 17.;

    EXPECT_NEAR(21.4243, distance(a, b), 0.0001);

    a.x = 2.;
    a.y = 3.;
    a.z = 4.;

    EXPECT_NEAR(16.0624, distance(a, b), 0.0001);

    a.x = -2.;
    a.y = -3.;
    a.z = -4.;

    EXPECT_NEAR(26.7955, distance(a, b), 0.0001);
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isAlmostEqual3dPt)
{
    DataVectorTypes::Vector a;
    DataVectorTypes::Vector b;

    a.x = 0.;
    a.y = 0.;
    a.z = 0.;

    b.x = 0.;
    b.y = 0.;
    b.z = 0.;

    EXPECT_TRUE(isAlmostEqual3dPt(a, b));

    b.x = 7.;
    b.y = 11.;
    b.z = 17.;

    EXPECT_FALSE(isAlmostEqual3dPt(a, b));

    a.x = 7.;
    a.y = 11.;
    a.z = 17.;

    EXPECT_TRUE(isAlmostEqual3dPt(a, b));

    b.x = 7.01;
    b.y = 11.01;
    b.z = 17.01;

    EXPECT_TRUE(isAlmostEqual3dPt(a, b));

    b.x = 7.05;
    b.y = 11.05;
    b.z = 17.05;

    EXPECT_FALSE(isAlmostEqual3dPt(a, b));

    a.x = -7.;
    a.y = -11.;
    a.z = -17.;

    b.x = -7.01;
    b.y = -11.01;
    b.z = -17.01;

    EXPECT_TRUE(isAlmostEqual3dPt(a, b));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isAlmostEqual2dPt)
{
    DataVectorTypes::Vector_2d a;
    DataVectorTypes::Vector_2d b;

    a.x = 0.;
    a.y = 0.;

    b.x = 0.;
    b.y = 0.;

    EXPECT_TRUE(isAlmostEqual2dPt(a, b));

    b.x = 7.;
    b.y = 11.;

    EXPECT_FALSE(isAlmostEqual2dPt(a, b));

    a.x = 7.;
    a.y = 11.;

    EXPECT_TRUE(isAlmostEqual2dPt(a, b));

    b.x = 7.01;
    b.y = 11.01;

    EXPECT_TRUE(isAlmostEqual2dPt(a, b));

    b.x = 7.05;
    b.y = 11.05;

    EXPECT_FALSE(isAlmostEqual2dPt(a, b));

    a.x = -7.;
    a.y = -11.;

    b.x = -7.01;
    b.y = -11.01;

    EXPECT_TRUE(isAlmostEqual2dPt(a, b));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isPointOnLineBetweenPoints)
{
    DataVectorTypes::Vector a;
    DataVectorTypes::Vector b;
    DataVectorTypes::Vector t;

    a.x = 0.;
    a.y = 0.;
    a.z = 0.;

    b.x = 10.;
    b.y = 10.;
    b.z = 10.;

    t.x = 6.;
    t.y = 6.;
    t.z = 6.;

    EXPECT_TRUE(isPointOnLineBetweenPoints(a, b, t));

    t.x = 6.00;
    t.y = 6.01;
    t.z = 6.00;

    EXPECT_TRUE(isPointOnLineBetweenPoints(a, b, t));

    t.x = 7.;
    t.y = 11.;
    t.z = 17.;

    EXPECT_FALSE(isPointOnLineBetweenPoints(a, b, t));

    a.x = 5.;
    a.y = 3.;
    a.z = 13.;

    b.x = 7.;
    b.y = 11.;
    b.z = 4.;

    t.x = 6.;
    t.y = 7.;
    t.z = 8.5;

    EXPECT_TRUE(isPointOnLineBetweenPoints(a, b, t));

    a.x = -5.;
    a.y = 3.;
    a.z = -13.;

    b.x = 7.;
    b.y = -11.;
    b.z = 4.;

    t.x = 1.;
    t.y = -4.;
    t.z = -4.5;

    EXPECT_TRUE(isPointOnLineBetweenPoints(a, b, t));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isPointOnLineBetweenPoints_FalsePositive)
{
    // cf #7383

    DataVectorTypes::Vector a(0.0, 0.0, 0.0);
    DataVectorTypes::Vector b(30.0, 0.0, 0.0);
    DataVectorTypes::Vector t(15.0, 0.0, 0.3); // Notice wrong z, it's 30cm off!

    EXPECT_FALSE(isPointOnLineBetweenPoints(a, b, t));
    EXPECT_EQ(0.3, distanceFromPointToLine(a, b, t));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_findIndexOfVertex)
{
    DataVectorTypes::Vector a;
    std::vector<DataVectorTypes::Vector> list;

    a.x = 0.;
    a.y = 0.;
    a.z = 0.;

    EXPECT_EQ(-1, findIndexOfVertex(a, list)); // not found

    list.emplace_back(a);

    EXPECT_EQ(0, findIndexOfVertex(a, list));

    list.emplace_back(DataVectorTypes::Vector(3., 2., 4.));
    list.emplace_back(DataVectorTypes::Vector(4., 3., 5.));
    list.emplace_back(DataVectorTypes::Vector(8., 1., 2.));
    list.emplace_back(DataVectorTypes::Vector(4., 7., 3.));

    EXPECT_EQ(0, findIndexOfVertex(a, list));

    a.x = 4.;
    a.y = 3.;
    a.z = 5.;

    EXPECT_EQ(2, findIndexOfVertex(a, list));

    a.x = 4.01;
    a.y = 7.01;
    a.z = 3.01;

    EXPECT_EQ(4, findIndexOfVertex(a, list));

    a.x = 4.03;
    a.y = 7.03;
    a.z = 3.03;

    EXPECT_EQ(-1, findIndexOfVertex(a, list)); // not found
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_listOfFacesFacingAzimuth_test)
{
    DataVectorTypes::Polyhedron zonePoly;
    std::vector<int> results;

    state->dataSurface->Surface.allocate(9);
    state->dataSurface->Surface(1).Azimuth = 0;
    state->dataSurface->Surface(2).Azimuth = 30.;
    state->dataSurface->Surface(3).Azimuth = 30.;
    state->dataSurface->Surface(4).Azimuth = 30.;
    state->dataSurface->Surface(5).Azimuth = 45.;
    state->dataSurface->Surface(6).Azimuth = 45.;
    state->dataSurface->Surface(7).Azimuth = 72.;
    state->dataSurface->Surface(8).Azimuth = 72.5;
    state->dataSurface->Surface(9).Azimuth = 73.;

    zonePoly.NumSurfaceFaces = 9;
    zonePoly.SurfaceFace.allocate(9);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(5).SurfNum = 5;
    zonePoly.SurfaceFace(6).SurfNum = 6;
    zonePoly.SurfaceFace(7).SurfNum = 7;
    zonePoly.SurfaceFace(8).SurfNum = 8;
    zonePoly.SurfaceFace(9).SurfNum = 9;

    results = listOfFacesFacingAzimuth(*state, zonePoly, 90.);
    EXPECT_EQ(size_t(0), results.size());

    results = listOfFacesFacingAzimuth(*state, zonePoly, 0.);
    EXPECT_EQ(size_t(1), results.size());
    EXPECT_EQ(1, results.at(0));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 30.);
    EXPECT_EQ(size_t(3), results.size());
    EXPECT_EQ(2, results.at(0));
    EXPECT_EQ(3, results.at(1));
    EXPECT_EQ(4, results.at(2));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 45.);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(5, results.at(0));
    EXPECT_EQ(6, results.at(1));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 71.9);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 72.0);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 72.1);
    EXPECT_EQ(size_t(3), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));
    EXPECT_EQ(9, results.at(2));

    results = listOfFacesFacingAzimuth(*state, zonePoly, 73.0);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(8, results.at(0));
    EXPECT_EQ(9, results.at(1));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areSurfaceHorizAndVert_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    state->dataSurface->Surface.allocate(9);
    state->dataSurface->Surface(1).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(1).Tilt = 180.;

    state->dataSurface->Surface(2).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(2).Tilt = 179.5;

    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 89.1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Tilt = 90.;

    state->dataSurface->Surface(5).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(5).Tilt = 90.;

    state->dataSurface->Surface(6).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(6).Tilt = 90.9;

    state->dataSurface->Surface(7).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(7).Tilt = -0.9;

    state->dataSurface->Surface(8).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(8).Tilt = 0.;

    state->dataSurface->Surface(9).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(9).Tilt = 0.9;

    zonePoly.NumSurfaceFaces = 9;
    zonePoly.SurfaceFace.allocate(9);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(5).SurfNum = 5;
    zonePoly.SurfaceFace(6).SurfNum = 6;
    zonePoly.SurfaceFace(7).SurfNum = 7;
    zonePoly.SurfaceFace(8).SurfNum = 8;
    zonePoly.SurfaceFace(9).SurfNum = 9;

    bool isFloorHorizontal;
    bool isCeilingHorizontal;
    bool areWallsVertical;

    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(1).Tilt = 170.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(1).Tilt = 180.;
    state->dataSurface->Surface(2).Tilt = 178.9;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(2).Tilt = 181.0;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(2).Tilt = 181.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(2).Tilt = 179.5;
    state->dataSurface->Surface(8).Tilt = 180.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(8).Tilt = 1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(8).Tilt = -1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    state->dataSurface->Surface(8).Tilt = 0.;
    state->dataSurface->Surface(4).Tilt = 270.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    state->dataSurface->Surface(4).Tilt = 91.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    state->dataSurface->Surface(4).Tilt = 88.9;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    state->dataSurface->Surface(1).Tilt = 170.;
    state->dataSurface->Surface(8).Tilt = 1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(*state, zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areWallHeightSame_test)
{
    DataVectorTypes::Polyhedron zonePoly;
    std::vector<int> results;

    state->dataSurface->Surface.allocate(3);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;

    zonePoly.NumSurfaceFaces = 3;
    zonePoly.SurfaceFace.allocate(3);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);
    zonePoly.SurfaceFace(1).FacePoints(1).z = 10.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 10.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);
    zonePoly.SurfaceFace(2).FacePoints(1).z = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 10.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 10.;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);
    zonePoly.SurfaceFace(3).FacePoints(1).z = 0.;
    zonePoly.SurfaceFace(3).FacePoints(2).z = 10.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = 10.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = 0.;

    EXPECT_TRUE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 9.;
    EXPECT_TRUE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 11.;
    EXPECT_FALSE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 10.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 10.02;
    EXPECT_TRUE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(2).FacePoints(2).z = 10.03;
    EXPECT_FALSE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(1).FacePoints(1).z = -10.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = -0.5;
    zonePoly.SurfaceFace(1).FacePoints(3).z = -0.5;
    zonePoly.SurfaceFace(1).FacePoints(4).z = -10.;

    zonePoly.SurfaceFace(2).FacePoints(1).z = -0.5;
    zonePoly.SurfaceFace(2).FacePoints(2).z = -10.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = -0.5;
    zonePoly.SurfaceFace(2).FacePoints(4).z = -10.;

    zonePoly.SurfaceFace(3).FacePoints(1).z = -0.5;
    zonePoly.SurfaceFace(3).FacePoints(2).z = -10.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = -10.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = -0.5;

    EXPECT_TRUE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(1).z = -0.6;
    EXPECT_TRUE(areWallHeightSame(*state, zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(1).z = -0.4;
    EXPECT_FALSE(areWallHeightSame(*state, zonePoly));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_findPossibleOppositeFace_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    state->dataSurface->Surface.allocate(4);
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).Area = 10.;

    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).Area = 10.;

    state->dataSurface->Surface(3).Azimuth = 180.;
    state->dataSurface->Surface(3).Area = 10.;

    state->dataSurface->Surface(4).Azimuth = 270.;
    state->dataSurface->Surface(4).Area = 10.;

    zonePoly.NumSurfaceFaces = 4;
    zonePoly.SurfaceFace.allocate(4);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;

    EXPECT_EQ(3, findPossibleOppositeFace(*state, zonePoly, 1));
    EXPECT_EQ(1, findPossibleOppositeFace(*state, zonePoly, 3));

    EXPECT_EQ(4, findPossibleOppositeFace(*state, zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(*state, zonePoly, 4));

    state->dataSurface->Surface(2).Azimuth = 90.5;

    EXPECT_EQ(4, findPossibleOppositeFace(*state, zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(*state, zonePoly, 4));

    state->dataSurface->Surface(2).Azimuth = 89.5;

    EXPECT_EQ(4, findPossibleOppositeFace(*state, zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(*state, zonePoly, 4));

    state->dataSurface->Surface(2).Azimuth = 45.;

    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 2)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 4)); // not found

    state->dataSurface->Surface(1).Area = 9.;

    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 1)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 3)); // not found

    state->dataSurface->Surface(1).Area = 10.;

    EXPECT_EQ(3, findPossibleOppositeFace(*state, zonePoly, 1));
    EXPECT_EQ(1, findPossibleOppositeFace(*state, zonePoly, 3));

    zonePoly.SurfaceFace(1).NSides = 3;

    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 1)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(*state, zonePoly, 3)); // not found
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areCornersEquidistant_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 2;
    zonePoly.SurfaceFace.allocate(2);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    Real64 dist;
    EXPECT_TRUE(areCornersEquidistant(zonePoly, 1, 2, dist));
    EXPECT_EQ(8., dist);

    zonePoly.SurfaceFace(2).FacePoints(4).y = 7.;
    EXPECT_FALSE(areCornersEquidistant(zonePoly, 1, 2, dist));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areOppositeWallsSame_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    state->dataSurface->Surface.allocate(4);
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Area = 30.;

    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Area = 24.;

    state->dataSurface->Surface(3).Azimuth = 180.;
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Area = 30.;

    state->dataSurface->Surface(4).Azimuth = 270.;
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Area = 24.;

    zonePoly.NumSurfaceFaces = 4;
    zonePoly.SurfaceFace.allocate(4);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);

    zonePoly.SurfaceFace(3).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(3).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);

    zonePoly.SurfaceFace(4).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(4).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(4).z = 3.;

    Real64 area;
    Real64 dist;

    EXPECT_TRUE(areOppositeWallsSame(*state, zonePoly, area, dist));
    EXPECT_EQ(30., area);
    EXPECT_EQ(8., dist);

    state->dataSurface->Surface(3).Area = 29.; // make surface 1 and 3 no longer match areas - now compare 2 and 4
    EXPECT_TRUE(areOppositeWallsSame(*state, zonePoly, area, dist));
    EXPECT_EQ(24., area);
    EXPECT_EQ(10., dist);

    state->dataSurface->Surface(4).Area = 23.; // make surface 2 and 4 no longer match areas
    EXPECT_FALSE(areOppositeWallsSame(*state, zonePoly, area, dist));

    state->dataSurface->Surface(3).Area = 30.; // make surface 1 and 3 have same areas again
    state->dataSurface->Surface(4).Area = 24.; // make surface 2 and 4 have same areas again

    EXPECT_TRUE(areOppositeWallsSame(*state, zonePoly, area, dist)); // retest

    zonePoly.SurfaceFace(3).FacePoints(3).y = 7.;                    // move one corner in so distances are not all equal
    EXPECT_TRUE(areOppositeWallsSame(*state, zonePoly, area, dist)); // should pick other walls
    EXPECT_EQ(24., area);
    EXPECT_EQ(10., dist);

    zonePoly.SurfaceFace(4).FacePoints(3).x = 11.;                    // move one corner out so distances are not all equal
    EXPECT_FALSE(areOppositeWallsSame(*state, zonePoly, area, dist)); // now neither wall matches
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areFloorAndCeilingSame_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    state->dataSurface->Surface.allocate(2);
    state->dataSurface->Surface(1).Class = SurfaceClass::Floor;

    state->dataSurface->Surface(2).Class = SurfaceClass::Roof;

    zonePoly.NumSurfaceFaces = 2;
    zonePoly.SurfaceFace.allocate(2);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 0.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    EXPECT_TRUE(areFloorAndCeilingSame(*state, zonePoly));

    zonePoly.SurfaceFace(2).FacePoints(4).x = 7.; // move one corner

    EXPECT_FALSE(areFloorAndCeilingSame(*state, zonePoly));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_makeListOfUniqueVertices_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 6;
    zonePoly.SurfaceFace.allocate(6);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);

    zonePoly.SurfaceFace(3).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(3).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);

    zonePoly.SurfaceFace(4).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(4).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(5).SurfNum = 1;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);

    zonePoly.SurfaceFace(5).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(4).z = 0.;

    zonePoly.SurfaceFace(6).SurfNum = 2;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);

    zonePoly.SurfaceFace(6).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(3).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(4).z = 3.;

    std::vector<Vector> uniqueVertices = makeListOfUniqueVertices(zonePoly);

    EXPECT_EQ(size_t(8), uniqueVertices.size());
    EXPECT_EQ(Vector(0., 0., 3.), uniqueVertices.at(0));
    EXPECT_EQ(Vector(0., 0., 0.), uniqueVertices.at(1));
    EXPECT_EQ(Vector(10., 0., 0.), uniqueVertices.at(2));
    EXPECT_EQ(Vector(10., 0., 3.), uniqueVertices.at(3));
    EXPECT_EQ(Vector(0., 8., 3.), uniqueVertices.at(4));
    EXPECT_EQ(Vector(0., 8., 0.), uniqueVertices.at(5));
    EXPECT_EQ(Vector(10., 8., 3.), uniqueVertices.at(6));
    EXPECT_EQ(Vector(10., 8., 0.), uniqueVertices.at(7));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_numberOfEdgesNotTwoForEnclosedVolumeTest_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 6;
    zonePoly.SurfaceFace.allocate(6);
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);

    zonePoly.SurfaceFace(3).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(3).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);

    zonePoly.SurfaceFace(4).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(4).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(5).SurfNum = 1;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);

    zonePoly.SurfaceFace(5).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(4).z = 0.;

    zonePoly.SurfaceFace(6).SurfNum = 2;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);

    zonePoly.SurfaceFace(6).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(3).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(4).z = 3.;

    std::vector<Vector> uniqueVertices = makeListOfUniqueVertices(zonePoly);

    EXPECT_EQ(size_t(8), uniqueVertices.size());

    std::vector<EdgeOfSurf> e1 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(0), e1.size());

    zonePoly.SurfaceFace(6).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(4).z = 0.;

    uniqueVertices = makeListOfUniqueVertices(zonePoly);
    EXPECT_EQ(size_t(8), uniqueVertices.size());

    std::vector<EdgeOfSurf> e2 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(4), e2.size());
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_updateZonePolygonsForMissingColinearPoints_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 7;
    zonePoly.SurfaceFace.allocate(7);

    // split old surface 1 into two new surfaces 1 and 7
    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);

    zonePoly.SurfaceFace(1).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(1).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(3).x = 4.;
    zonePoly.SurfaceFace(1).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(1).FacePoints(4).x = 4.;
    zonePoly.SurfaceFace(1).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(1).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(7).SurfNum = 7;
    zonePoly.SurfaceFace(7).NSides = 4;
    zonePoly.SurfaceFace(7).FacePoints.allocate(4);

    zonePoly.SurfaceFace(7).FacePoints(1).x = 4.;
    zonePoly.SurfaceFace(7).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(7).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(7).FacePoints(2).x = 4.;
    zonePoly.SurfaceFace(7).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(7).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(7).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(7).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(7).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(7).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(7).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(7).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);

    zonePoly.SurfaceFace(2).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(2).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(2).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(2).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);

    zonePoly.SurfaceFace(3).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(3).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(3).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(3).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(3).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(3).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(3).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);

    zonePoly.SurfaceFace(4).FacePoints(1).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(4).FacePoints(2).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(4).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(4).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(4).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(4).FacePoints(4).z = 3.;

    zonePoly.SurfaceFace(5).SurfNum = 5;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);

    zonePoly.SurfaceFace(5).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(1).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(5).FacePoints(2).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(2).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(3).y = 8.;
    zonePoly.SurfaceFace(5).FacePoints(3).z = 0.;

    zonePoly.SurfaceFace(5).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(5).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(5).FacePoints(4).z = 0.;

    zonePoly.SurfaceFace(6).SurfNum = 6;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);

    zonePoly.SurfaceFace(6).FacePoints(1).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(1).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(1).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(2).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(2).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(3).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(3).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(3).z = 3.;

    zonePoly.SurfaceFace(6).FacePoints(4).x = 10.;
    zonePoly.SurfaceFace(6).FacePoints(4).y = 8.;
    zonePoly.SurfaceFace(6).FacePoints(4).z = 3.;

    std::vector<Vector> uniqueVertices = makeListOfUniqueVertices(zonePoly);

    EXPECT_EQ(size_t(10), uniqueVertices.size());

    std::vector<EdgeOfSurf> e1 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(6), e1.size());

    DataVectorTypes::Polyhedron updatedZonePoly = updateZonePolygonsForMissingColinearPoints(
        zonePoly, uniqueVertices); // this is done after initial test since it is computationally intensive.

    std::vector<EdgeOfSurf> e2 = edgesNotTwoForEnclosedVolumeTest(updatedZonePoly, uniqueVertices);
    EXPECT_EQ(size_t(0), e2.size());
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isEnclosedVolume_SimpleBox_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 6;
    zonePoly.SurfaceFace.allocate(6);

    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);
    zonePoly.SurfaceFace(1).FacePoints(1) = Vector(0., 0., 3.);
    zonePoly.SurfaceFace(1).FacePoints(2) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(1).FacePoints(3) = Vector(10., 0., 0.);
    zonePoly.SurfaceFace(1).FacePoints(4) = Vector(10., 0., 3.);

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);
    zonePoly.SurfaceFace(2).FacePoints(1) = Vector(0., 8., 3.);
    zonePoly.SurfaceFace(2).FacePoints(2) = Vector(0., 8., 0.);
    zonePoly.SurfaceFace(2).FacePoints(3) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(2).FacePoints(4) = Vector(0., 0., 3.);

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);
    zonePoly.SurfaceFace(3).FacePoints(1) = Vector(10., 8., 3.);
    zonePoly.SurfaceFace(3).FacePoints(2) = Vector(10., 8., 0.);
    zonePoly.SurfaceFace(3).FacePoints(3) = Vector(0., 8., 0.);
    zonePoly.SurfaceFace(3).FacePoints(4) = Vector(0., 8., 3.);

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);
    zonePoly.SurfaceFace(4).FacePoints(1) = Vector(10., 0., 3.);
    zonePoly.SurfaceFace(4).FacePoints(2) = Vector(10., 0., 0.);
    zonePoly.SurfaceFace(4).FacePoints(3) = Vector(10., 8., 0.);
    zonePoly.SurfaceFace(4).FacePoints(4) = Vector(10., 8., 3.);

    zonePoly.SurfaceFace(5).SurfNum = 1;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);
    zonePoly.SurfaceFace(5).FacePoints(1) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(5).FacePoints(2) = Vector(0., 8, 0.);
    zonePoly.SurfaceFace(5).FacePoints(3) = Vector(10., 8, 0.);
    zonePoly.SurfaceFace(5).FacePoints(4) = Vector(10., 0, 0.);

    zonePoly.SurfaceFace(6).SurfNum = 2;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);
    zonePoly.SurfaceFace(6).FacePoints(1) = Vector(0., 8., 3.);
    zonePoly.SurfaceFace(6).FacePoints(2) = Vector(0., 0., 3.);
    zonePoly.SurfaceFace(6).FacePoints(3) = Vector(10., 0., 3.);
    zonePoly.SurfaceFace(6).FacePoints(4) = Vector(10., 8., 3.);

    std::vector<EdgeOfSurf> edgeNot2;
    EXPECT_TRUE(isEnclosedVolume(zonePoly, edgeNot2));

    // leave gap
    zonePoly.SurfaceFace(1).FacePoints(3) = Vector(9., 0., 0.);
    EXPECT_FALSE(isEnclosedVolume(zonePoly, edgeNot2));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isEnclosedVolume_BoxWithSplitSide_test)
{
    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 7;
    zonePoly.SurfaceFace.allocate(7);

    // split old surface 1 into two new surfaces 1 and 7

    zonePoly.SurfaceFace(1).SurfNum = 1;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);
    zonePoly.SurfaceFace(1).FacePoints(1) = Vector(0., 0., 3.);
    zonePoly.SurfaceFace(1).FacePoints(2) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(1).FacePoints(3) = Vector(4., 0., 0.);
    zonePoly.SurfaceFace(1).FacePoints(4) = Vector(4., 0., 3.);

    zonePoly.SurfaceFace(7).SurfNum = 7;
    zonePoly.SurfaceFace(7).NSides = 4;
    zonePoly.SurfaceFace(7).FacePoints.allocate(4);
    zonePoly.SurfaceFace(7).FacePoints(1) = Vector(4., 0., 3.);
    zonePoly.SurfaceFace(7).FacePoints(2) = Vector(4., 0., 0.);
    zonePoly.SurfaceFace(7).FacePoints(3) = Vector(10., 0., 0.);
    zonePoly.SurfaceFace(7).FacePoints(4) = Vector(10., 0., 3.);

    zonePoly.SurfaceFace(2).SurfNum = 2;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);
    zonePoly.SurfaceFace(2).FacePoints(1) = Vector(0., 8., 3.);
    zonePoly.SurfaceFace(2).FacePoints(2) = Vector(0., 8., 0.);
    zonePoly.SurfaceFace(2).FacePoints(3) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(2).FacePoints(4) = Vector(0., 0., 3.);

    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);
    zonePoly.SurfaceFace(3).FacePoints(1) = Vector(10., 8., 3.);
    zonePoly.SurfaceFace(3).FacePoints(2) = Vector(10., 8., 0.);
    zonePoly.SurfaceFace(3).FacePoints(3) = Vector(0., 8., 0.);
    zonePoly.SurfaceFace(3).FacePoints(4) = Vector(0., 8., 3.);

    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);
    zonePoly.SurfaceFace(4).FacePoints(1) = Vector(10., 0., 3.);
    zonePoly.SurfaceFace(4).FacePoints(2) = Vector(10., 0., 0.);
    zonePoly.SurfaceFace(4).FacePoints(3) = Vector(10., 8., 0.);
    zonePoly.SurfaceFace(4).FacePoints(4) = Vector(10., 8., 3.);

    zonePoly.SurfaceFace(5).SurfNum = 1;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);
    zonePoly.SurfaceFace(5).FacePoints(1) = Vector(0., 0., 0.);
    zonePoly.SurfaceFace(5).FacePoints(2) = Vector(0., 8, 0.);
    zonePoly.SurfaceFace(5).FacePoints(3) = Vector(10., 8, 0.);
    zonePoly.SurfaceFace(5).FacePoints(4) = Vector(10., 0, 0.);

    zonePoly.SurfaceFace(6).SurfNum = 2;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);
    zonePoly.SurfaceFace(6).FacePoints(1) = Vector(0., 8., 3.);
    zonePoly.SurfaceFace(6).FacePoints(2) = Vector(0., 0., 3.);
    zonePoly.SurfaceFace(6).FacePoints(3) = Vector(10., 0., 3.);
    zonePoly.SurfaceFace(6).FacePoints(4) = Vector(10., 8., 3.);

    std::vector<EdgeOfSurf> edgeNot2;
    EXPECT_TRUE(isEnclosedVolume(zonePoly, edgeNot2));

    // leave gap
    zonePoly.SurfaceFace(1).FacePoints(3) = Vector(9., 0., 0.);
    EXPECT_FALSE(isEnclosedVolume(zonePoly, edgeNot2));
}

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_isEnclosedVolume_VeryFlatShape)
{
    // Test for #7383
    // This is a shoebox model that has a very flat aspect: a 30x10x0.3m one
    // The south wall is split in two equal segments, the rest is just one surface per orientation

    DataVectorTypes::Polyhedron zonePoly;

    zonePoly.NumSurfaceFaces = 7;
    zonePoly.SurfaceFace.allocate(7);

    // 1-SOUTH-1
    zonePoly.SurfaceFace(1).SurfNum = 2;
    zonePoly.SurfaceFace(1).NSides = 4;
    zonePoly.SurfaceFace(1).FacePoints.allocate(4);
    zonePoly.SurfaceFace(1).FacePoints(1) = Vector(0.0, 0.0, 0.3);
    zonePoly.SurfaceFace(1).FacePoints(2) = Vector(0.0, 0.0, 0.0);
    zonePoly.SurfaceFace(1).FacePoints(3) = Vector(15.0, 0.0, 0.0);
    zonePoly.SurfaceFace(1).FacePoints(4) = Vector(15.0, 0.0, 0.3);

    // 1-SOUTH-2
    zonePoly.SurfaceFace(2).SurfNum = 1;
    zonePoly.SurfaceFace(2).NSides = 4;
    zonePoly.SurfaceFace(2).FacePoints.allocate(4);
    zonePoly.SurfaceFace(2).FacePoints(1) = Vector(15.0, 0.0, 0.3);
    zonePoly.SurfaceFace(2).FacePoints(2) = Vector(15.0, 0.0, 0.0);
    zonePoly.SurfaceFace(2).FacePoints(3) = Vector(30.0, 0.0, 0.0);
    zonePoly.SurfaceFace(2).FacePoints(4) = Vector(30.0, 0.0, 0.3);

    // 4-NORTH
    zonePoly.SurfaceFace(3).SurfNum = 3;
    zonePoly.SurfaceFace(3).NSides = 4;
    zonePoly.SurfaceFace(3).FacePoints.allocate(4);
    zonePoly.SurfaceFace(3).FacePoints(1) = Vector(30.0, 10.0, 0.3);
    zonePoly.SurfaceFace(3).FacePoints(2) = Vector(30.0, 10.0, 0.0);
    zonePoly.SurfaceFace(3).FacePoints(3) = Vector(0.0, 10.0, 0.0);
    zonePoly.SurfaceFace(3).FacePoints(4) = Vector(0.0, 10.0, 0.3);

    // 3-EAST
    zonePoly.SurfaceFace(4).SurfNum = 4;
    zonePoly.SurfaceFace(4).NSides = 4;
    zonePoly.SurfaceFace(4).FacePoints.allocate(4);
    zonePoly.SurfaceFace(4).FacePoints(1) = Vector(30.0, 0.0, 0.3);
    zonePoly.SurfaceFace(4).FacePoints(2) = Vector(30.0, 0.0, 0.0);
    zonePoly.SurfaceFace(4).FacePoints(3) = Vector(30.0, 10.0, 0.0);
    zonePoly.SurfaceFace(4).FacePoints(4) = Vector(30.0, 10.0, 0.3);

    // ROOF
    zonePoly.SurfaceFace(5).SurfNum = 5;
    zonePoly.SurfaceFace(5).NSides = 4;
    zonePoly.SurfaceFace(5).FacePoints.allocate(4);
    zonePoly.SurfaceFace(5).FacePoints(1) = Vector(30.0, 0.0, 0.3);
    zonePoly.SurfaceFace(5).FacePoints(2) = Vector(30.0, 10.0, 0.3);
    zonePoly.SurfaceFace(5).FacePoints(3) = Vector(0.0, 10.0, 0.3);
    zonePoly.SurfaceFace(5).FacePoints(4) = Vector(0.0, 0.0, 0.3);

    // 2-WEST
    zonePoly.SurfaceFace(6).SurfNum = 6;
    zonePoly.SurfaceFace(6).NSides = 4;
    zonePoly.SurfaceFace(6).FacePoints.allocate(4);
    zonePoly.SurfaceFace(6).FacePoints(1) = Vector(0.0, 10.0, 0.3);
    zonePoly.SurfaceFace(6).FacePoints(2) = Vector(0.0, 10.0, 0.0);
    zonePoly.SurfaceFace(6).FacePoints(3) = Vector(0.0, 0.0, 0.0);
    zonePoly.SurfaceFace(6).FacePoints(4) = Vector(0.0, 0.0, 0.3);

    // FLOOR
    zonePoly.SurfaceFace(7).SurfNum = 7;
    zonePoly.SurfaceFace(7).NSides = 4;
    zonePoly.SurfaceFace(7).FacePoints.allocate(4);
    zonePoly.SurfaceFace(7).FacePoints(1) = Vector(0.0, 0.0, 0.0);
    zonePoly.SurfaceFace(7).FacePoints(2) = Vector(0.0, 10.0, 0.0);
    zonePoly.SurfaceFace(7).FacePoints(3) = Vector(30.0, 10.0, 0.0);
    zonePoly.SurfaceFace(7).FacePoints(4) = Vector(30.0, 0.0, 0.0);

    std::vector<EdgeOfSurf> edgeNot2;
    EXPECT_TRUE(isEnclosedVolume(zonePoly, edgeNot2));
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_SimpleBox_test)
{
    Array1D_bool enteredCeilingHeight;
    state->dataGlobal->NumOfZones = 1;
    enteredCeilingHeight.dimension(state->dataGlobal->NumOfZones, false);
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).HasFloor = true;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceLast = 6;

    state->dataSurface->Surface.allocate(6);

    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.dimension(4);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Vertex(1) = Vector(0., 0., 3.);
    state->dataSurface->Surface(1).Vertex(2) = Vector(0., 0., 0.);
    state->dataSurface->Surface(1).Vertex(3) = Vector(10., 0., 0.);
    state->dataSurface->Surface(1).Vertex(4) = Vector(10., 0., 3.);

    state->dataSurface->Surface(2).Sides = 4;
    state->dataSurface->Surface(2).Vertex.dimension(4);
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(2).Vertex(2) = Vector(0., 8., 0.);
    state->dataSurface->Surface(2).Vertex(3) = Vector(0., 0., 0.);
    state->dataSurface->Surface(2).Vertex(4) = Vector(0., 0., 3.);

    state->dataSurface->Surface(3).Sides = 4;
    state->dataSurface->Surface(3).Vertex.dimension(4);
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Vertex(1) = Vector(10., 8., 3.);
    state->dataSurface->Surface(3).Vertex(2) = Vector(10., 8., 0.);
    state->dataSurface->Surface(3).Vertex(3) = Vector(0., 8., 0.);
    state->dataSurface->Surface(3).Vertex(4) = Vector(0., 8., 3.);

    state->dataSurface->Surface(4).Sides = 4;
    state->dataSurface->Surface(4).Vertex.dimension(4);
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Vertex(1) = Vector(10., 0., 3.);
    state->dataSurface->Surface(4).Vertex(2) = Vector(10., 0., 0.);
    state->dataSurface->Surface(4).Vertex(3) = Vector(10., 8., 0.);
    state->dataSurface->Surface(4).Vertex(4) = Vector(10., 8., 3.);

    state->dataSurface->Surface(5).Sides = 4;
    state->dataSurface->Surface(5).Vertex.dimension(4);
    state->dataSurface->Surface(5).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(5).Tilt = 180.;
    state->dataSurface->Surface(5).Vertex(1) = Vector(0., 0., 0.);
    state->dataSurface->Surface(5).Vertex(2) = Vector(0., 8, 0.);
    state->dataSurface->Surface(5).Vertex(3) = Vector(10., 8, 0.);
    state->dataSurface->Surface(5).Vertex(4) = Vector(10., 0, 0.);

    state->dataSurface->Surface(6).Sides = 4;
    state->dataSurface->Surface(6).Vertex.dimension(4);
    state->dataSurface->Surface(6).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(6).Tilt = 0.;
    state->dataSurface->Surface(6).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(6).Vertex(2) = Vector(0., 0., 3.);
    state->dataSurface->Surface(6).Vertex(3) = Vector(10., 0., 3.);
    state->dataSurface->Surface(6).Vertex(4) = Vector(10., 8., 3.);

    CalculateZoneVolume(*state, enteredCeilingHeight);
    EXPECT_EQ(240., state->dataHeatBal->Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxOneWallMissing_test)
{
    Array1D_bool enteredCeilingHeight;
    state->dataGlobal->NumOfZones = 1;
    enteredCeilingHeight.dimension(state->dataGlobal->NumOfZones, false);
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).HasFloor = true;
    //    Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceLast = 5;

    state->dataSurface->Surface.allocate(5);

    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.dimension(4);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Vertex(1) = Vector(0., 0., 3.);
    state->dataSurface->Surface(1).Vertex(2) = Vector(0., 0., 0.);
    state->dataSurface->Surface(1).Vertex(3) = Vector(10., 0., 0.);
    state->dataSurface->Surface(1).Vertex(4) = Vector(10., 0., 3.);

    state->dataSurface->Surface(2).Sides = 4;
    state->dataSurface->Surface(2).Vertex.dimension(4);
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(2).Vertex(2) = Vector(0., 8., 0.);
    state->dataSurface->Surface(2).Vertex(3) = Vector(0., 0., 0.);
    state->dataSurface->Surface(2).Vertex(4) = Vector(0., 0., 3.);

    state->dataSurface->Surface(3).Sides = 4;
    state->dataSurface->Surface(3).Vertex.dimension(4);
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Vertex(1) = Vector(10., 8., 3.);
    state->dataSurface->Surface(3).Vertex(2) = Vector(10., 8., 0.);
    state->dataSurface->Surface(3).Vertex(3) = Vector(0., 8., 0.);
    state->dataSurface->Surface(3).Vertex(4) = Vector(0., 8., 3.);

    state->dataSurface->Surface(4).Sides = 4;
    state->dataSurface->Surface(4).Vertex.dimension(4);
    state->dataSurface->Surface(4).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(4).Tilt = 180.;
    state->dataSurface->Surface(4).Vertex(1) = Vector(0., 0., 0.);
    state->dataSurface->Surface(4).Vertex(2) = Vector(0., 8, 0.);
    state->dataSurface->Surface(4).Vertex(3) = Vector(10., 8, 0.);
    state->dataSurface->Surface(4).Vertex(4) = Vector(10., 0, 0.);

    state->dataSurface->Surface(5).Sides = 4;
    state->dataSurface->Surface(5).Vertex.dimension(4);
    state->dataSurface->Surface(5).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(5).Tilt = 0.;
    state->dataSurface->Surface(5).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(5).Vertex(2) = Vector(0., 0., 3.);
    state->dataSurface->Surface(5).Vertex(3) = Vector(10., 0., 3.);
    state->dataSurface->Surface(5).Vertex(4) = Vector(10., 8., 3.);

    state->dataHeatBal->Zone(1).FloorArea = 80.;
    state->dataHeatBal->Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(*state, enteredCeilingHeight);
    EXPECT_EQ(240., state->dataHeatBal->Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoCeiling_test)
{
    Array1D_bool enteredCeilingHeight;
    state->dataGlobal->NumOfZones = 1;
    enteredCeilingHeight.dimension(state->dataGlobal->NumOfZones, false);
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).HasFloor = true;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceLast = 5;

    state->dataSurface->Surface.allocate(5);

    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.dimension(4);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Vertex(1) = Vector(0., 0., 3.);
    state->dataSurface->Surface(1).Vertex(2) = Vector(0., 0., 0.);
    state->dataSurface->Surface(1).Vertex(3) = Vector(10., 0., 0.);
    state->dataSurface->Surface(1).Vertex(4) = Vector(10., 0., 3.);

    state->dataSurface->Surface(2).Sides = 4;
    state->dataSurface->Surface(2).Vertex.dimension(4);
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(2).Vertex(2) = Vector(0., 8., 0.);
    state->dataSurface->Surface(2).Vertex(3) = Vector(0., 0., 0.);
    state->dataSurface->Surface(2).Vertex(4) = Vector(0., 0., 3.);

    state->dataSurface->Surface(3).Sides = 4;
    state->dataSurface->Surface(3).Vertex.dimension(4);
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Vertex(1) = Vector(10., 8., 3.);
    state->dataSurface->Surface(3).Vertex(2) = Vector(10., 8., 0.);
    state->dataSurface->Surface(3).Vertex(3) = Vector(0., 8., 0.);
    state->dataSurface->Surface(3).Vertex(4) = Vector(0., 8., 3.);

    state->dataSurface->Surface(4).Sides = 4;
    state->dataSurface->Surface(4).Vertex.dimension(4);
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Vertex(1) = Vector(10., 0., 3.);
    state->dataSurface->Surface(4).Vertex(2) = Vector(10., 0., 0.);
    state->dataSurface->Surface(4).Vertex(3) = Vector(10., 8., 0.);
    state->dataSurface->Surface(4).Vertex(4) = Vector(10., 8., 3.);

    state->dataSurface->Surface(5).Sides = 4;
    state->dataSurface->Surface(5).Vertex.dimension(4);
    state->dataSurface->Surface(5).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(5).Tilt = 180.;
    state->dataSurface->Surface(5).Vertex(1) = Vector(0., 0., 0.);
    state->dataSurface->Surface(5).Vertex(2) = Vector(0., 8, 0.);
    state->dataSurface->Surface(5).Vertex(3) = Vector(10., 8, 0.);
    state->dataSurface->Surface(5).Vertex(4) = Vector(10., 0, 0.);

    state->dataHeatBal->Zone(1).FloorArea = 80.;
    state->dataHeatBal->Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(*state, enteredCeilingHeight);
    EXPECT_EQ(240., state->dataHeatBal->Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoFloor_test)
{
    Array1D_bool enteredCeilingHeight;
    state->dataGlobal->NumOfZones = 1;
    enteredCeilingHeight.dimension(state->dataGlobal->NumOfZones, false);
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).HasFloor = true;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceLast = 5;

    state->dataSurface->Surface.allocate(5);

    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.dimension(4);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Vertex(1) = Vector(0., 0., 3.);
    state->dataSurface->Surface(1).Vertex(2) = Vector(0., 0., 0.);
    state->dataSurface->Surface(1).Vertex(3) = Vector(10., 0., 0.);
    state->dataSurface->Surface(1).Vertex(4) = Vector(10., 0., 3.);

    state->dataSurface->Surface(2).Sides = 4;
    state->dataSurface->Surface(2).Vertex.dimension(4);
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(2).Vertex(2) = Vector(0., 8., 0.);
    state->dataSurface->Surface(2).Vertex(3) = Vector(0., 0., 0.);
    state->dataSurface->Surface(2).Vertex(4) = Vector(0., 0., 3.);

    state->dataSurface->Surface(3).Sides = 4;
    state->dataSurface->Surface(3).Vertex.dimension(4);
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Vertex(1) = Vector(10., 8., 3.);
    state->dataSurface->Surface(3).Vertex(2) = Vector(10., 8., 0.);
    state->dataSurface->Surface(3).Vertex(3) = Vector(0., 8., 0.);
    state->dataSurface->Surface(3).Vertex(4) = Vector(0., 8., 3.);

    state->dataSurface->Surface(4).Sides = 4;
    state->dataSurface->Surface(4).Vertex.dimension(4);
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Vertex(1) = Vector(10., 0., 3.);
    state->dataSurface->Surface(4).Vertex(2) = Vector(10., 0., 0.);
    state->dataSurface->Surface(4).Vertex(3) = Vector(10., 8., 0.);
    state->dataSurface->Surface(4).Vertex(4) = Vector(10., 8., 3.);

    state->dataSurface->Surface(5).Sides = 4;
    state->dataSurface->Surface(5).Vertex.dimension(4);
    state->dataSurface->Surface(5).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(5).Tilt = 0.;
    state->dataSurface->Surface(5).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(5).Vertex(2) = Vector(0., 0., 3.);
    state->dataSurface->Surface(5).Vertex(3) = Vector(10., 0., 3.);
    state->dataSurface->Surface(5).Vertex(4) = Vector(10., 8., 3.);

    state->dataHeatBal->Zone(1).CeilingArea = 80.;
    state->dataHeatBal->Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(*state, enteredCeilingHeight);
    EXPECT_EQ(240., state->dataHeatBal->Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoCeilingFloor_test)
{
    Array1D_bool enteredCeilingHeight;
    state->dataGlobal->NumOfZones = 1;
    enteredCeilingHeight.dimension(state->dataGlobal->NumOfZones, false);
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).AllSurfaceLast = 4;

    state->dataSurface->Surface.allocate(4);

    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(1).Vertex.dimension(4);
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Azimuth = 180.;
    state->dataSurface->Surface(1).Area = 30.;
    state->dataSurface->Surface(1).Vertex(1) = Vector(0., 0., 3.);
    state->dataSurface->Surface(1).Vertex(2) = Vector(0., 0., 0.);
    state->dataSurface->Surface(1).Vertex(3) = Vector(10., 0., 0.);
    state->dataSurface->Surface(1).Vertex(4) = Vector(10., 0., 3.);

    state->dataSurface->Surface(2).Sides = 4;
    state->dataSurface->Surface(2).Vertex.dimension(4);
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Azimuth = 270.;
    state->dataSurface->Surface(2).Area = 24.;
    state->dataSurface->Surface(2).Vertex(1) = Vector(0., 8., 3.);
    state->dataSurface->Surface(2).Vertex(2) = Vector(0., 8., 0.);
    state->dataSurface->Surface(2).Vertex(3) = Vector(0., 0., 0.);
    state->dataSurface->Surface(2).Vertex(4) = Vector(0., 0., 3.);

    state->dataSurface->Surface(3).Sides = 4;
    state->dataSurface->Surface(3).Vertex.dimension(4);
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).Area = 30.;
    state->dataSurface->Surface(3).Vertex(1) = Vector(10., 8., 3.);
    state->dataSurface->Surface(3).Vertex(2) = Vector(10., 8., 0.);
    state->dataSurface->Surface(3).Vertex(3) = Vector(0., 8., 0.);
    state->dataSurface->Surface(3).Vertex(4) = Vector(0., 8., 3.);

    state->dataSurface->Surface(4).Sides = 4;
    state->dataSurface->Surface(4).Vertex.dimension(4);
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).Area = 24.;
    state->dataSurface->Surface(4).Vertex(1) = Vector(10., 0., 3.);
    state->dataSurface->Surface(4).Vertex(2) = Vector(10., 0., 0.);
    state->dataSurface->Surface(4).Vertex(3) = Vector(10., 8., 0.);
    state->dataSurface->Surface(4).Vertex(4) = Vector(10., 8., 3.);

    CalculateZoneVolume(*state, enteredCeilingHeight);
    EXPECT_EQ(240., state->dataHeatBal->Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, MakeRectangularVertices)
{
    int surfNum = 1;
    int zoneNum = 1;

    state->dataSurfaceGeometry->SurfaceTmp.allocate(surfNum);
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Class = SurfaceClass::Wall;
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Zone = zoneNum;
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Azimuth = 0.;
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Tilt = 90.;
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Sides = 4;
    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex.allocate(4);

    state->dataHeatBal->Zone.allocate(zoneNum);
    state->dataHeatBal->Zone(zoneNum).RelNorth = 0.;

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(zoneNum);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(zoneNum);
    state->dataSurfaceGeometry->CosZoneRelNorth(zoneNum) = std::cos(-state->dataHeatBal->Zone(zoneNum).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(zoneNum) = std::sin(-state->dataHeatBal->Zone(zoneNum).RelNorth * DataGlobalConstants::DegToRadians);

    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    // facing north

    MakeRectangularVertices(*state, 1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(-5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(-5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing east

    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Azimuth = 90.;

    MakeRectangularVertices(*state, 1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0, state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing south

    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Azimuth = 180.;

    MakeRectangularVertices(*state, 1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0, state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing west

    state->dataSurfaceGeometry->SurfaceTmp(surfNum).Azimuth = 270.;

    MakeRectangularVertices(*state, 1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(-5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(0., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(-5., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., state->dataSurfaceGeometry->SurfaceTmp(surfNum).Vertex(4).z, 0.001);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_VertexNumberMismatchTest)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Material,",
        "  8 in.Concrete Block Basement Wall,     !- Name",
        "  MediumRough,                            !- Roughness",
        "  0.2032,                                 !- Thickness{ m }",
        "  1.326,                                  !- Conductivity{ W / m - K }",
        "  1841.99999999999,                       !- Density{ kg / m3 }",
        "  911.999999999999,                       !- Specific Heat{ J / kg - K }",
        "  0.9,                                    !- Thermal Absorptance",
        "  0.7,                                    !- Solar Absorptance",
        "  0.7;                                    !- Visible Absorptance",
        "Construction,",
        "   Typical,   !- Name",
        "   8 in.Concrete Block Basement Wall;     !- Layer 1",

        "BuildingSurface:Detailed,                                   ",
        "	016W88_WaterMeter - Floor : a, !- Name",
        "	Floor, !- Surface Type",
        "	Typical, !- Construction Name",
        "	ZONE 1, !- Zone Name",
        "    ,                        !- Space Name",
        "	Surface, !- Outside Boundary Condition",
        "	006W27_Restrooms - RoofCeiling : a, !- Outside Boundary Condition Object",
        "	NoSun, !- Sun Exposure",
        "	NoWind, !- Wind Exposure",
        "	, !- View Factor to Ground",
        "	, !- Number of Vertices",
        "	251.4600375, 3.5052, 0, !- X, Y, Z Vertex 1 {m}",
        "	251.4600375, 0, 0, !- X, Y, Z Vertex 2 {m}",
        "	249.9571375, 0, 0, !- X, Y, Z Vertex 3 {m}",
        "	248.5215375, 1.0, 0, !- X, Y, Z Vertex 4 {m}",
        "	248.5215375, 3.5052, 0;                 !- X, Y, Z Vertex 5 {m}",

        "BuildingSurface:Detailed,",
        "   006W27_Restrooms - RoofCeiling : a, !- Name",
        "   Ceiling, !- Surface Type",
        "   Typical, !- Construction Name",
        "   ZONE 2, !- Zone Name",
        "    ,                        !- Space Name",
        "   Surface, !- Outside Boundary Condition",
        "   016W88_WaterMeter - Floor : a, !- Outside Boundary Condition Object",
        "   NoSun, !- Sun Exposure",
        "   NoWind, !- Wind Exposure",
        "   , !- View Factor to Ground",
        "   , !- Number of Vertices",
        "   174.6425, 0, 6.1976, !- X, Y, Z Vertex 1 {m}",
        "   174.6425, 3.5052, 6.1976, !- X, Y, Z Vertex 2 {m}",
        "   171.704, 3.5052, 6.1976, !- X, Y, Z Vertex 3 {m}",
        "   171.704, 0, 6.1976;                     !- X, Y, Z Vertex 4 {m}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfZones = 2;
    state->dataHeatBal->Zone.allocate(2);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(2).Name = "ZONE 2";
    state->dataSurfaceGeometry->SurfaceTmp.allocate(2);
    int SurfNum = 0;
    int TotHTSurfs = 2;
    Array1D_string const BaseSurfCls(3, {"WALL", "FLOOR", "ROOF"});
    Array1D<DataSurfaces::SurfaceClass> const BaseSurfIDs(
        3, {DataSurfaces::SurfaceClass::Wall, DataSurfaces::SurfaceClass::Floor, DataSurfaces::SurfaceClass::Roof});
    int NeedToAddSurfaces;

    GetGeometryParameters(*state, ErrorsFound);
    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(2);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(2);

    state->dataSurfaceGeometry->CosZoneRelNorth = 1.0;
    state->dataSurfaceGeometry->SinZoneRelNorth = 0.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;

    GetHTSurfaceData(*state, ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);

    EXPECT_EQ(2, SurfNum);
    std::string const error_string =
        delimited_string({"   ** Severe  ** BuildingSurface:Detailed=\"016W88_WATERMETER - FLOOR : A\", invalid Construction Name=\"TYPICAL\".",
                          "   ** Severe  ** BuildingSurface:Detailed=\"006W27_RESTROOMS - ROOFCEILING : A\", invalid Construction Name=\"TYPICAL\".",
                          "   ** Severe  ** RoofCeiling:Detailed=\"016W88_WATERMETER - FLOOR : A\", Vertex size mismatch between base surface "
                          ":016W88_WATERMETER - FLOOR : A and outside boundary surface: 006W27_RESTROOMS - ROOFCEILING : A",
                          "   **   ~~~   ** The vertex sizes are 5 for base surface and 4 for outside boundary surface. Please check inputs.",
                          "   ** Severe  ** RoofCeiling:Detailed=\"006W27_RESTROOMS - ROOFCEILING : A\", Vertex size mismatch between base surface "
                          ":006W27_RESTROOMS - ROOFCEILING : A and outside boundary surface: 016W88_WATERMETER - FLOOR : A",
                          "   **   ~~~   ** The vertex sizes are 4 for base surface and 5 for outside boundary surface. Please check inputs."});
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_CheckConvexityTest)
{

    // Test a multiple vertex surfaces in ProcessSurfaceVertices and CalcCoordinateTransformation for #6384

    state->dataSurface->TotSurfaces = 2;
    state->dataSurface->MaxVerticesPerSurface = 9;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->ShadeV.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->Surface(1).Vertex.allocate(7);
    state->dataSurface->Surface(2).Vertex.allocate(9);
    state->dataSurfaceGeometry->SurfaceTmp.allocate(state->dataSurface->TotSurfaces);
    state->dataSurfaceGeometry->SurfaceTmp(1).Vertex.allocate(7);
    state->dataSurfaceGeometry->SurfaceTmp(2).Vertex.allocate(9);

    int ThisSurf(0);

    // Surface 1 - Rectangle with 7 points
    ThisSurf = 1;
    state->dataSurface->Surface(ThisSurf).Azimuth = 0.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 180.0;
    state->dataSurface->Surface(ThisSurf).Sides = 7;
    state->dataSurface->Surface(ThisSurf).GrossArea = 20.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 2.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 3.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 4.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(5).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).y = 6.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(6).x = 15.0;
    state->dataSurface->Surface(ThisSurf).Vertex(6).y = 6.0;
    state->dataSurface->Surface(ThisSurf).Vertex(6).z = 3.0;

    state->dataSurface->Surface(ThisSurf).Vertex(7).x = 15.0;
    state->dataSurface->Surface(ThisSurf).Vertex(7).y = 2.0;
    state->dataSurface->Surface(ThisSurf).Vertex(7).z = 3.0;

    state->dataSurfaceGeometry->SurfaceTmp(ThisSurf) = state->dataSurface->Surface(ThisSurf);
    CheckConvexity(*state, ThisSurf, state->dataSurfaceGeometry->SurfaceTmp(ThisSurf).Sides);
    state->dataSurface->Surface(ThisSurf) = state->dataSurfaceGeometry->SurfaceTmp(ThisSurf);
    EXPECT_EQ(4, state->dataSurface->Surface(ThisSurf).Sides);
    EXPECT_EQ(10.0, state->dataSurface->Surface(ThisSurf).Vertex(2).x);
    EXPECT_EQ(6.0, state->dataSurface->Surface(ThisSurf).Vertex(2).y);
    EXPECT_EQ(15.0, state->dataSurface->Surface(ThisSurf).Vertex(3).x);
    EXPECT_EQ(6.0, state->dataSurface->Surface(ThisSurf).Vertex(3).y);

    // Surface 2 - Rectangle with 9 points
    ThisSurf = 2;
    state->dataSurface->Surface(ThisSurf).Azimuth = 0.0;
    state->dataSurface->Surface(ThisSurf).Tilt = 0.0;
    state->dataSurface->Surface(ThisSurf).Sides = 9;
    state->dataSurface->Surface(ThisSurf).GrossArea = 30.0;

    state->dataSurface->Surface(ThisSurf).Vertex(1).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).y = 2.0;
    state->dataSurface->Surface(ThisSurf).Vertex(1).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(2).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).y = 3.0;
    state->dataSurface->Surface(ThisSurf).Vertex(2).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(3).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).y = 4.0;
    state->dataSurface->Surface(ThisSurf).Vertex(3).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(4).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).y = 5.0;
    state->dataSurface->Surface(ThisSurf).Vertex(4).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(5).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).y = 6.0;
    state->dataSurface->Surface(ThisSurf).Vertex(5).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(6).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(6).y = 7.0;
    state->dataSurface->Surface(ThisSurf).Vertex(6).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(7).x = 10.0;
    state->dataSurface->Surface(ThisSurf).Vertex(7).y = 8.0;
    state->dataSurface->Surface(ThisSurf).Vertex(7).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(8).x = 15.0;
    state->dataSurface->Surface(ThisSurf).Vertex(8).y = 8.0;
    state->dataSurface->Surface(ThisSurf).Vertex(8).z = 0.0;

    state->dataSurface->Surface(ThisSurf).Vertex(9).x = 15.0;
    state->dataSurface->Surface(ThisSurf).Vertex(9).y = 2.0;
    state->dataSurface->Surface(ThisSurf).Vertex(9).z = 0.0;

    state->dataSurfaceGeometry->SurfaceTmp(ThisSurf) = state->dataSurface->Surface(ThisSurf);
    CheckConvexity(*state, ThisSurf, state->dataSurfaceGeometry->SurfaceTmp(ThisSurf).Sides);
    state->dataSurface->Surface(ThisSurf) = state->dataSurfaceGeometry->SurfaceTmp(ThisSurf);
    EXPECT_EQ(4, state->dataSurface->Surface(ThisSurf).Sides);
    EXPECT_EQ(10.0, state->dataSurface->Surface(ThisSurf).Vertex(2).x);
    EXPECT_EQ(8.0, state->dataSurface->Surface(ThisSurf).Vertex(2).y);
    EXPECT_EQ(15.0, state->dataSurface->Surface(ThisSurf).Vertex(3).x);
    EXPECT_EQ(8.0, state->dataSurface->Surface(ThisSurf).Vertex(3).y);
}

TEST_F(EnergyPlusFixture, InitialAssociateWindowShadingControlFenestration_test)
{
    state->dataSurface->TotWinShadingControl = 3;
    state->dataSurface->WindowShadingControl.allocate(state->dataSurface->TotWinShadingControl);
    int zn = 1;

    state->dataSurface->WindowShadingControl(1).Name = "WSC1";
    state->dataSurface->WindowShadingControl(1).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(1).SequenceNumber = 2;
    state->dataSurface->WindowShadingControl(1).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(1).FenestrationCount = 3;
    state->dataSurface->WindowShadingControl(1).FenestrationName.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    state->dataSurface->WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    state->dataSurface->WindowShadingControl(1).FenestrationName(3) = "Fene-03";

    state->dataSurface->WindowShadingControl(2).Name = "WSC2";
    state->dataSurface->WindowShadingControl(2).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(2).SequenceNumber = 3;
    state->dataSurface->WindowShadingControl(2).MultiSurfaceCtrlIsGroup = false;
    state->dataSurface->WindowShadingControl(2).FenestrationCount = 4;
    state->dataSurface->WindowShadingControl(2).FenestrationName.allocate(state->dataSurface->WindowShadingControl(2).FenestrationCount);
    state->dataSurface->WindowShadingControl(2).FenestrationName(1) = "Fene-04";
    state->dataSurface->WindowShadingControl(2).FenestrationName(2) = "Fene-05";
    state->dataSurface->WindowShadingControl(2).FenestrationName(3) = "Fene-06";
    state->dataSurface->WindowShadingControl(2).FenestrationName(4) = "Fene-07";

    state->dataSurface->WindowShadingControl(3).Name = "WSC3";
    state->dataSurface->WindowShadingControl(3).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(3).SequenceNumber = 1;
    state->dataSurface->WindowShadingControl(3).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(3).FenestrationCount = 2;
    state->dataSurface->WindowShadingControl(3).FenestrationName.allocate(state->dataSurface->WindowShadingControl(3).FenestrationCount);
    state->dataSurface->WindowShadingControl(3).FenestrationName(1) = "Fene-08";
    state->dataSurface->WindowShadingControl(3).FenestrationName(2) = "Fene-09";

    state->dataConstruction->Construct.allocate(1);
    state->dataConstruction->Construct(1).WindowTypeEQL = false;

    state->dataSurfaceGeometry->SurfaceTmp.allocate(9);

    state->dataSurfaceGeometry->SurfaceTmp(1).Name = "Fene-04";
    state->dataSurfaceGeometry->SurfaceTmp(1).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(1).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(2).Name = "Fene-05";
    state->dataSurfaceGeometry->SurfaceTmp(2).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(2).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(3).Name = "Fene-06";
    state->dataSurfaceGeometry->SurfaceTmp(3).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(3).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(4).Name = "Fene-01";
    state->dataSurfaceGeometry->SurfaceTmp(4).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(4).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(5).Name = "Fene-02";
    state->dataSurfaceGeometry->SurfaceTmp(5).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(5).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(6).Name = "Fene-03";
    state->dataSurfaceGeometry->SurfaceTmp(6).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(6).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(7).Name = "Fene-07";
    state->dataSurfaceGeometry->SurfaceTmp(7).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(7).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(8).Name = "Fene-08";
    state->dataSurfaceGeometry->SurfaceTmp(8).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(8).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(9).Name = "Fene-09";
    state->dataSurfaceGeometry->SurfaceTmp(9).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(9).ExtBoundCond = ExternalEnvironment;

    bool Err = false;

    int surfNum = 1;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 2);
    EXPECT_FALSE(Err);

    surfNum = 2;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 2);
    EXPECT_FALSE(Err);

    surfNum = 3;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 2);
    EXPECT_FALSE(Err);

    surfNum = 4;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 1);
    EXPECT_FALSE(Err);

    surfNum = 5;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 1);
    EXPECT_FALSE(Err);

    surfNum = 6;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 1);
    EXPECT_FALSE(Err);

    surfNum = 7;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 2);
    EXPECT_FALSE(Err);

    surfNum = 8;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 3);
    EXPECT_FALSE(Err);

    surfNum = 9;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).activeWindowShadingControl, 3);
    EXPECT_FALSE(Err);
}

TEST_F(EnergyPlusFixture, InitialAssociateWindowShadingControlFenestration_Multi_test)
{
    state->dataSurface->TotWinShadingControl = 3;
    state->dataSurface->WindowShadingControl.allocate(state->dataSurface->TotWinShadingControl);
    int zn = 1;

    state->dataSurface->WindowShadingControl(1).Name = "WSC1";
    state->dataSurface->WindowShadingControl(1).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(1).SequenceNumber = 2;
    state->dataSurface->WindowShadingControl(1).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(1).FenestrationCount = 3;
    state->dataSurface->WindowShadingControl(1).FenestrationName.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    state->dataSurface->WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    state->dataSurface->WindowShadingControl(1).FenestrationName(3) = "Fene-03";

    state->dataSurface->WindowShadingControl(2).Name = "WSC2";
    state->dataSurface->WindowShadingControl(2).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(2).SequenceNumber = 3;
    state->dataSurface->WindowShadingControl(2).MultiSurfaceCtrlIsGroup = false;
    state->dataSurface->WindowShadingControl(2).FenestrationCount = 4;
    state->dataSurface->WindowShadingControl(2).FenestrationName.allocate(state->dataSurface->WindowShadingControl(2).FenestrationCount);
    state->dataSurface->WindowShadingControl(2).FenestrationName(1) = "Fene-02";
    state->dataSurface->WindowShadingControl(2).FenestrationName(2) = "Fene-03";
    state->dataSurface->WindowShadingControl(2).FenestrationName(3) = "Fene-04";
    state->dataSurface->WindowShadingControl(2).FenestrationName(4) = "Fene-05";

    state->dataSurface->WindowShadingControl(3).Name = "WSC3";
    state->dataSurface->WindowShadingControl(3).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(3).SequenceNumber = 1;
    state->dataSurface->WindowShadingControl(3).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(3).FenestrationCount = 2;
    state->dataSurface->WindowShadingControl(3).FenestrationName.allocate(state->dataSurface->WindowShadingControl(3).FenestrationCount);
    state->dataSurface->WindowShadingControl(3).FenestrationName(1) = "Fene-03";
    state->dataSurface->WindowShadingControl(3).FenestrationName(2) = "Fene-05";

    state->dataConstruction->Construct.allocate(1);
    state->dataConstruction->Construct(1).WindowTypeEQL = false;

    state->dataSurfaceGeometry->SurfaceTmp.allocate(5);

    state->dataSurfaceGeometry->SurfaceTmp(1).Name = "Fene-01";
    state->dataSurfaceGeometry->SurfaceTmp(1).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(1).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(2).Name = "Fene-02";
    state->dataSurfaceGeometry->SurfaceTmp(2).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(2).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(3).Name = "Fene-03";
    state->dataSurfaceGeometry->SurfaceTmp(3).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(3).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(4).Name = "Fene-04";
    state->dataSurfaceGeometry->SurfaceTmp(4).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(4).ExtBoundCond = ExternalEnvironment;

    state->dataSurfaceGeometry->SurfaceTmp(5).Name = "Fene-05";
    state->dataSurfaceGeometry->SurfaceTmp(5).Construction = 1;
    state->dataSurfaceGeometry->SurfaceTmp(5).ExtBoundCond = ExternalEnvironment;

    bool Err = false;

    int surfNum = 1;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList.size(), 1u);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[0], 1);
    EXPECT_FALSE(Err);

    surfNum = 2;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList.size(), 2u);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[0], 1);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[1], 2);
    EXPECT_FALSE(Err);

    surfNum = 3;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList.size(), 3u);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[0], 1);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[1], 2);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[2], 3);
    EXPECT_FALSE(Err);

    surfNum = 4;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList.size(), 1u);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[0], 2);
    EXPECT_FALSE(Err);

    surfNum = 5;
    InitialAssociateWindowShadingControlFenestration(*state, Err, surfNum);
    EXPECT_TRUE(state->dataSurfaceGeometry->SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList.size(), 2u);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[0], 2);
    EXPECT_EQ(state->dataSurfaceGeometry->SurfaceTmp(surfNum).windowShadingControlList[1], 3);
    EXPECT_FALSE(Err);
}

TEST_F(EnergyPlusFixture, FinalAssociateWindowShadingControlFenestration_test)
{
    state->dataSurface->TotWinShadingControl = 3;
    state->dataSurface->WindowShadingControl.allocate(state->dataSurface->TotWinShadingControl);
    int zn = 1;

    state->dataSurface->WindowShadingControl(1).Name = "WSC1";
    state->dataSurface->WindowShadingControl(1).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(1).SequenceNumber = 2;
    state->dataSurface->WindowShadingControl(1).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(1).FenestrationCount = 3;
    state->dataSurface->WindowShadingControl(1).FenestrationName.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationIndex.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    state->dataSurface->WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    state->dataSurface->WindowShadingControl(1).FenestrationName(3) = "Fene-03";

    state->dataSurface->WindowShadingControl(2).Name = "WSC2";
    state->dataSurface->WindowShadingControl(2).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(2).SequenceNumber = 3;
    state->dataSurface->WindowShadingControl(2).MultiSurfaceCtrlIsGroup = false;
    state->dataSurface->WindowShadingControl(2).FenestrationCount = 4;
    state->dataSurface->WindowShadingControl(2).FenestrationName.allocate(state->dataSurface->WindowShadingControl(2).FenestrationCount);
    state->dataSurface->WindowShadingControl(2).FenestrationIndex.allocate(state->dataSurface->WindowShadingControl(2).FenestrationCount);
    state->dataSurface->WindowShadingControl(2).FenestrationName(1) = "Fene-04";
    state->dataSurface->WindowShadingControl(2).FenestrationName(2) = "Fene-05";
    state->dataSurface->WindowShadingControl(2).FenestrationName(3) = "Fene-06";
    state->dataSurface->WindowShadingControl(2).FenestrationName(4) = "Fene-07";

    state->dataSurface->WindowShadingControl(3).Name = "WSC3";
    state->dataSurface->WindowShadingControl(3).ZoneIndex = zn;
    state->dataSurface->WindowShadingControl(3).SequenceNumber = 1;
    state->dataSurface->WindowShadingControl(3).MultiSurfaceCtrlIsGroup = true;
    state->dataSurface->WindowShadingControl(3).FenestrationCount = 2;
    state->dataSurface->WindowShadingControl(3).FenestrationName.allocate(state->dataSurface->WindowShadingControl(3).FenestrationCount);
    state->dataSurface->WindowShadingControl(3).FenestrationIndex.allocate(state->dataSurface->WindowShadingControl(3).FenestrationCount);
    state->dataSurface->WindowShadingControl(3).FenestrationName(1) = "Fene-08";
    state->dataSurface->WindowShadingControl(3).FenestrationName(2) = "Fene-09";

    state->dataSurface->TotSurfaces = 12;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    state->dataSurface->Surface(1).Name = "Fene-07";
    state->dataSurface->Surface(1).windowShadingControlList.push_back(2);

    state->dataSurface->Surface(2).Name = "Fene-01";
    state->dataSurface->Surface(2).windowShadingControlList.push_back(1);

    state->dataSurface->Surface(3).Name = "Fene-08";
    state->dataSurface->Surface(3).windowShadingControlList.push_back(3);

    state->dataSurface->Surface(4).Name = "Fene-02";
    state->dataSurface->Surface(4).windowShadingControlList.push_back(1);

    state->dataSurface->Surface(5).Name = "Fene-10";
    state->dataSurface->Surface(5).windowShadingControlList.push_back(0);

    state->dataSurface->Surface(6).Name = "Fene-03";
    state->dataSurface->Surface(6).windowShadingControlList.push_back(1);

    state->dataSurface->Surface(7).Name = "Fene-09";
    state->dataSurface->Surface(7).windowShadingControlList.push_back(3);

    state->dataSurface->Surface(8).Name = "Fene-04";
    state->dataSurface->Surface(8).windowShadingControlList.push_back(2);

    state->dataSurface->Surface(9).Name = "Fene-10";
    state->dataSurface->Surface(9).windowShadingControlList.push_back(0);

    state->dataSurface->Surface(10).Name = "Fene-05";
    state->dataSurface->Surface(10).windowShadingControlList.push_back(2);

    state->dataSurface->Surface(11).Name = "Fene-11";
    state->dataSurface->Surface(11).windowShadingControlList.push_back(0);

    state->dataSurface->Surface(12).Name = "Fene-06";
    state->dataSurface->Surface(12).windowShadingControlList.push_back(2);

    bool Err = false;

    FinalAssociateWindowShadingControlFenestration(*state, Err);
    EXPECT_FALSE(Err);

    EXPECT_EQ(state->dataSurface->WindowShadingControl(1).FenestrationIndex(1), 2);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(1).FenestrationIndex(2), 4);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(1).FenestrationIndex(3), 6);

    EXPECT_EQ(state->dataSurface->WindowShadingControl(2).FenestrationIndex(1), 8);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(2).FenestrationIndex(2), 10);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(2).FenestrationIndex(3), 12);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(2).FenestrationIndex(4), 1);

    EXPECT_EQ(state->dataSurface->WindowShadingControl(3).FenestrationIndex(1), 3);
    EXPECT_EQ(state->dataSurface->WindowShadingControl(3).FenestrationIndex(2), 7);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_isWindowShadingControlSimilar_Test)
{
    state->dataSurface->WindowShadingControl.allocate(2);

    state->dataSurface->WindowShadingControl(1).Name = "TheShadingControl";
    state->dataSurface->WindowShadingControl(1).ZoneIndex = 57;
    state->dataSurface->WindowShadingControl(1).SequenceNumber = 3;
    state->dataSurface->WindowShadingControl(1).ShadingType = WinShadingType::ExtShade;
    state->dataSurface->WindowShadingControl(1).ShadingDevice = 17;
    state->dataSurface->WindowShadingControl(1).ShadingControlType = WindowShadingControlType::OnIfScheduled;
    state->dataSurface->WindowShadingControl(1).Schedule = 83;
    state->dataSurface->WindowShadingControl(1).SetPoint = 200;
    state->dataSurface->WindowShadingControl(1).SetPoint2 = 170;
    state->dataSurface->WindowShadingControl(1).ShadingControlIsScheduled = true;
    state->dataSurface->WindowShadingControl(1).GlareControlIsActive = false;
    state->dataSurface->WindowShadingControl(1).SlatAngleSchedule = 84;
    state->dataSurface->WindowShadingControl(1).SlatAngleControlForBlinds = WSC_SAC_BlockBeamSolar;
    state->dataSurface->WindowShadingControl(1).DaylightingControlName = "TheDaylightingControl";
    state->dataSurface->WindowShadingControl(1).DaylightControlIndex = 7;
    state->dataSurface->WindowShadingControl(1).MultiSurfaceCtrlIsGroup = false;

    state->dataSurface->WindowShadingControl(1).FenestrationCount = 3;
    state->dataSurface->WindowShadingControl(1).FenestrationName.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    state->dataSurface->WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    state->dataSurface->WindowShadingControl(1).FenestrationName(3) = "Fene-03";
    state->dataSurface->WindowShadingControl(1).FenestrationIndex.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(1) = 11;
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(2) = 12;
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(3) = 13;

    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    // no changes
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));

    // changes to portions of struct that are not "similar"
    // these should not impact similarity so changes are ignored

    state->dataSurface->WindowShadingControl(2).Name = "Different";
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).SequenceNumber = 9;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).ShadingDevice = 21;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).Schedule = 91;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).SlatAngleSchedule = 76;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).FenestrationCount = 4;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).FenestrationName(3) = "Fene-Different";
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).FenestrationIndex(3) = 17;
    EXPECT_TRUE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    // changes to portions of struct that are "similar"
    // these are important so they should be shown as false

    state->dataSurface->WindowShadingControl(2).ZoneIndex = 83;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).ShadingType = WinShadingType::BGBlind;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).ShadingControlType = WindowShadingControlType::OffNight_OnDay_HiSolarWindow;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).SetPoint = 140;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).SetPoint2 = 169;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).ShadingControlIsScheduled = false;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).GlareControlIsActive = true;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).SlatAngleControlForBlinds = WSC_SAC_FixedSlatAngle;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).DaylightingControlName = "Different";
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).DaylightControlIndex = 12;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);

    state->dataSurface->WindowShadingControl(2).MultiSurfaceCtrlIsGroup = true;
    EXPECT_FALSE(isWindowShadingControlSimilar(*state, 1, 2));
    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_CheckWindowShadingControlSimilarForWindow_Test)
{
    state->dataSurface->Surface.allocate(1);

    state->dataSurface->Surface(1).HasShadeControl = true;
    state->dataSurface->Surface(1).windowShadingControlList.push_back(1);
    state->dataSurface->Surface(1).windowShadingControlList.push_back(2);
    state->dataSurface->Surface(1).windowShadingControlList.push_back(3);

    state->dataSurface->WindowShadingControl.allocate(3);

    state->dataSurface->WindowShadingControl(1).Name = "TheShadingControl";
    state->dataSurface->WindowShadingControl(1).ZoneIndex = 57;
    state->dataSurface->WindowShadingControl(1).SequenceNumber = 3;
    state->dataSurface->WindowShadingControl(1).ShadingType = WinShadingType::ExtShade;
    state->dataSurface->WindowShadingControl(1).ShadingDevice = 17;
    state->dataSurface->WindowShadingControl(1).ShadingControlType = WindowShadingControlType::OnIfScheduled;
    state->dataSurface->WindowShadingControl(1).Schedule = 83;
    state->dataSurface->WindowShadingControl(1).SetPoint = 200;
    state->dataSurface->WindowShadingControl(1).SetPoint2 = 170;
    state->dataSurface->WindowShadingControl(1).ShadingControlIsScheduled = true;
    state->dataSurface->WindowShadingControl(1).GlareControlIsActive = false;
    state->dataSurface->WindowShadingControl(1).SlatAngleSchedule = 84;
    state->dataSurface->WindowShadingControl(1).SlatAngleControlForBlinds = WSC_SAC_BlockBeamSolar;
    state->dataSurface->WindowShadingControl(1).DaylightingControlName = "TheDaylightingControl";
    state->dataSurface->WindowShadingControl(1).DaylightControlIndex = 7;
    state->dataSurface->WindowShadingControl(1).MultiSurfaceCtrlIsGroup = false;

    state->dataSurface->WindowShadingControl(1).FenestrationCount = 3;
    state->dataSurface->WindowShadingControl(1).FenestrationName.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    state->dataSurface->WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    state->dataSurface->WindowShadingControl(1).FenestrationName(3) = "Fene-03";
    state->dataSurface->WindowShadingControl(1).FenestrationIndex.allocate(state->dataSurface->WindowShadingControl(1).FenestrationCount);
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(1) = 11;
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(2) = 12;
    state->dataSurface->WindowShadingControl(1).FenestrationIndex(3) = 13;

    state->dataSurface->WindowShadingControl(2) = state->dataSurface->WindowShadingControl(1);
    state->dataSurface->WindowShadingControl(3) = state->dataSurface->WindowShadingControl(1);

    bool errorsOccurred = false;

    CheckWindowShadingControlSimilarForWindow(*state, errorsOccurred);
    EXPECT_FALSE(errorsOccurred);

    state->dataSurface->WindowShadingControl(2).SetPoint = 140;
    CheckWindowShadingControlSimilarForWindow(*state, errorsOccurred);
    EXPECT_TRUE(errorsOccurred);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_createAirMaterialFromDistance_Test)
{
    state->dataHeatBal->TotMaterials = 0;
    createAirMaterialFromDistance(*state, 0.008, "test_air_");
    EXPECT_EQ(state->dataHeatBal->TotMaterials, 1);
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).Name, "test_air_8MM");
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).Thickness, 0.008);
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).GasCon(1, 1), 2.873e-3);
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).GasCon(2, 1), 7.760e-5);

    createAirMaterialFromDistance(*state, 0.012, "test_air_");
    EXPECT_EQ(state->dataHeatBal->TotMaterials, 2);
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).Name, "test_air_12MM");
    EXPECT_EQ(state->dataMaterial->Material(state->dataHeatBal->TotMaterials).Thickness, 0.012);

    createAirMaterialFromDistance(*state, 0.008, "test_air_");
    EXPECT_EQ(state->dataHeatBal->TotMaterials, 2);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_createConstructionWithStorm_Test)
{
    state->dataHeatBal->TotConstructs = 1;
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);

    state->dataMaterial->Material.allocate(60);
    state->dataMaterial->Material(47).AbsorpThermalFront = 0.11;

    // Case 1a: Constructs with regular materials are a reverse of each other--material layers match in reverse (should get a "false" answer)
    state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).TotLayers = 3;
    state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(1) = 11;
    state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(2) = 22;
    state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(3) = 33;

    createConstructionWithStorm(*state, 1, "construction_A", 47, 59);
    EXPECT_EQ(state->dataHeatBal->TotConstructs, 2);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).Name, "construction_A");
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(1), 47);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(2), 59);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(3), 11);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(4), 22);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).LayerPoint(5), 33);
    EXPECT_EQ(state->dataConstruction->Construct(state->dataHeatBal->TotConstructs).OutsideAbsorpThermal, 0.11);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_HeatTransferAlgorithmTest)
{
    // Test surface heat transfer algorithms and heat balance surface lists
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "  HeatBalanceAlgorithm,",
        "  MoisturePenetrationDepthConductionTransferFunction; !- Algorithm",
        "Material,",
        "    Gypsum Board,            !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0159,                  !- Thickness {m}",
        "    0.16,                    !- Conductivity {W/m-K}",
        "    800,                     !- Density {kg/m3}",
        "    1090,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Material,",
        "    InsulationBatt,        !- Name",
        "    VeryRough,               !- Roughness",
        "    0.3048,                  !- Thickness {m}",
        "    0.05,                    !- Conductivity {W/m-K}",
        "    19,                      !- Density {kg/m3}",
        "    960,                     !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Material,",
        "    InfiniteRPCM23C,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Project semi-exposed ceiling,  !- Name",
        "    InsulationBatt,        !- Outside Layer",
        "    InfiniteRPCM23C,         !- Layer 2",
        "    Gypsum Board;            !- Layer 3",

        "Construction,",
        "    Project semi-exposed ceiling_Rev,  !- Name",
        "    Gypsum Board,            !- Outside Layer",
        "    InfiniteRPCM23C,         !- Layer 2",
        "    InsulationBatt;        !- Layer 3",

        "BuildingSurface:Detailed,",
        "    DATATELCOM_Ceiling_1_0_0,  !- Name",
        "    Ceiling,                 !- Surface Type",
        "    Project semi-exposed ceiling,  !- Construction Name",
        "    DATATELCOM,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1_Floor_4_0_10000,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.64595244,              !- Vertex 1 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 1 Y-coordinate {m}",
        "    3.499104,                !- Vertex 1 Z-coordinate {m}",
        "    13.20708687,             !- Vertex 2 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 2 Y-coordinate {m}",
        "    3.499104,                !- Vertex 2 Z-coordinate {m}",
        "    13.20708687,             !- Vertex 3 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 3 Y-coordinate {m}",
        "    3.499104,                !- Vertex 3 Z-coordinate {m}",
        "    9.64595244,              !- Vertex 4 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 4 Y-coordinate {m}",
        "    3.499104;                !- Vertex 4 Z-coordinate {m}",

        "BuildingSurface:Detailed,",
        "    Zone1_Floor_4_0_10000,  !- Name",
        "    Floor,                   !- Surface Type",
        "    Project semi-exposed ceiling_Rev,  !- Construction Name",
        "    ZONE1,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    DATATELCOM_Ceiling_1_0_0,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    13.20708687,             !- Vertex 1 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 1 Y-coordinate {m}",
        "    3.499104,                !- Vertex 1 Z-coordinate {m}",
        "    9.64595244,              !- Vertex 2 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 2 Y-coordinate {m}",
        "    3.499104,                !- Vertex 2 Z-coordinate {m}",
        "    9.64595244,              !- Vertex 3 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 3 Y-coordinate {m}",
        "    3.499104,                !- Vertex 3 Z-coordinate {m}",
        "    13.20708687,             !- Vertex 4 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 4 Y-coordinate {m}",
        "    3.499104;                !- Vertex 4 Z-coordinate {m}",

        "BuildingSurface:Detailed,",
        "    Zone1_Floor_4_0_20000,  !- Name",
        "    Floor,                   !- Surface Type",
        "    Project semi-exposed ceiling_Rev,  !- Construction Name",
        "    ZONE1,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    23.20708687,             !- Vertex 1 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 1 Y-coordinate {m}",
        "    3.499104,                !- Vertex 1 Z-coordinate {m}",
        "    19.64595244,              !- Vertex 2 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 2 Y-coordinate {m}",
        "    3.499104,                !- Vertex 2 Z-coordinate {m}",
        "    19.64595244,              !- Vertex 3 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 3 Y-coordinate {m}",
        "    3.499104,                !- Vertex 3 Z-coordinate {m}",
        "    23.20708687,             !- Vertex 4 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 4 Y-coordinate {m}",
        "    3.499104;                !- Vertex 4 Z-coordinate {m}",

        "BuildingSurface:Detailed,",
        "    Zone1_Floor_4_0_30000,  !- Name",
        "    Floor,                   !- Surface Type",
        "    Project semi-exposed ceiling_Rev,  !- Construction Name",
        "    ZONE1,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    23.20708687,             !- Vertex 1 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 1 Y-coordinate {m}",
        "    3.499104,                !- Vertex 1 Z-coordinate {m}",
        "    19.64595244,              !- Vertex 2 X-coordinate {m}",
        "    8.1545602599,            !- Vertex 2 Y-coordinate {m}",
        "    3.499104,                !- Vertex 2 Z-coordinate {m}",
        "    19.64595244,              !- Vertex 3 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 3 Y-coordinate {m}",
        "    3.499104,                !- Vertex 3 Z-coordinate {m}",
        "    23.20708687,             !- Vertex 4 X-coordinate {m}",
        "    10.0470868899,           !- Vertex 4 Y-coordinate {m}",
        "    3.499104;                !- Vertex 4 Z-coordinate {m}",

        "SurfaceProperty:HeatTransferAlgorithm:Construction,",
        "    Ceilings CondFD,               !- Name",
        "    ConductionFiniteDifference,    !- Algorithm",
        "    Project semi-exposed ceiling;  !- Construction Name",

        "SurfaceProperty:HeatTransferAlgorithm,",
        "    Zone1_Floor_4_0_20000,       !- Surface Name",
        "    CombinedHeatAndMoistureFiniteElement;  !- Algorithm",

        "SurfaceProperty:HeatTransferAlgorithm,",
        "    Zone1_Floor_4_0_30000,       !- Surface Name",
        "    ConductionTransferFunction;  !- Algorithm",

        "Zone,",
        "    DATATELCOM,       !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    23.5824,                 !- Volume {m3}",
        "    6.7395,                  !- Floor Area {m2}",
        "    TARP;                    !- Zone Inside Convection Algorithm",
        "Zone,",
        "    ZONE1,             !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    999.6521,                !- Volume {m3}",
        "    454.1032,                !- Floor Area {m2}",
        "    TARP;                    !- Zone Inside Convection Algorithm",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(*state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);                  // expect no errors

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(2);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(2);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosZoneRelNorth(2) = state->dataSurfaceGeometry->CosZoneRelNorth(1);
    state->dataSurfaceGeometry->SinZoneRelNorth(2) = state->dataSurfaceGeometry->SinZoneRelNorth(1);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;

    GetSurfaceData(*state, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);           // expect no errors

    int surfNum = UtilityRoutines::FindItemInList("DATATELCOM_CEILING_1_0_0", state->dataSurface->Surface);
    EXPECT_TRUE(compare_enums(DataSurfaces::HeatTransferModel::CondFD, state->dataSurface->Surface(surfNum).HeatTransferAlgorithm));
    EXPECT_TRUE(state->dataHeatBal->AnyCondFD);

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_10000", state->dataSurface->Surface);
    EXPECT_TRUE(compare_enums(DataSurfaces::HeatTransferModel::CondFD, state->dataSurface->Surface(surfNum).HeatTransferAlgorithm));
    EXPECT_TRUE(state->dataHeatBal->AnyEMPD); // input as EMPD but then later overriden to CondFD - see error message below

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_20000", state->dataSurface->Surface);
    EXPECT_TRUE(compare_enums(DataSurfaces::HeatTransferModel::HAMT, state->dataSurface->Surface(surfNum).HeatTransferAlgorithm));
    EXPECT_TRUE(state->dataHeatBal->AnyHAMT);

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_30000", state->dataSurface->Surface);
    EXPECT_TRUE(compare_enums(DataSurfaces::HeatTransferModel::CTF, state->dataSurface->Surface(surfNum).HeatTransferAlgorithm));
    EXPECT_TRUE(state->dataHeatBal->AnyCTF);

    std::string const error_string = delimited_string({
        "   ** Warning ** GetSurfaceData: Entered Zone Floor Area(s) differ more than 5% from the sum of the Space Floor Area(s).",
        "   **   ~~~   ** ...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.",
        "   ** Warning ** The moisture penetration depth conduction transfer function algorithm is used but the input file includes no "
        "MaterialProperty:MoisturePenetrationDepth:Settings objects.",
        "   ** Warning ** The combined heat and moisture finite element algorithm is used but the input file includes no "
        "MaterialProperty:HeatAndMoistureTransfer:* objects.",
        "   **   ~~~   ** Certain materials objects are necessary to achieve proper results with the heat transfer algorithm(s) selected.",
        "   ** Warning ** An interior surface is defined as two surfaces with reverse constructions. The HeatTransferAlgorithm in both constructions "
        "should be same.",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: DATATELCOM_CEILING_1_0_0, is CondFD - ConductionFiniteDifference",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: ZONE1_FLOOR_4_0_10000, is EMPD - MoisturePenetrationDepthConductionTransferFunction",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: ZONE1_FLOOR_4_0_10000, is assigned to CondFD - ConductionFiniteDifference. "
        "Simulation continues.",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));

    // Check heat balance surface lists
    // Remember that ZoneHTSurfaceList includes all HT surfaces in the zone PLUS any adjacent interzone surfaces - same for ZoneIZSurfaceList
    EXPECT_EQ(state->dataSurface->AllHTSurfaceList.size(), 4u);
    EXPECT_EQ(state->dataSurface->AllIZSurfaceList.size(), 2u);

    int zoneNum = UtilityRoutines::FindItemInList("DATATELCOM", state->dataHeatBal->Zone);
    EXPECT_EQ(state->dataHeatBal->Zone(zoneNum).ZoneHTSurfaceList.size(), 2u);
    EXPECT_EQ(state->dataHeatBal->Zone(zoneNum).ZoneIZSurfaceList.size(), 2u);

    zoneNum = UtilityRoutines::FindItemInList("ZONE1", state->dataHeatBal->Zone);
    EXPECT_EQ(state->dataHeatBal->Zone(zoneNum).ZoneHTSurfaceList.size(), 4u);
    EXPECT_EQ(state->dataHeatBal->Zone(zoneNum).ZoneIZSurfaceList.size(), 2u);
}

// Test for #7071: if a Surface references an outside boundary surface that cannot be found, we handle it gracefully with an error message
// instead of throwing an assert 'contains' error
TEST_F(EnergyPlusFixture, SurfaceGeometry_SurfaceReferencesNonExistingSurface)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Material,",
        "  8 in.Concrete Block Basement Wall,      !- Name",
        "  MediumRough,                            !- Roughness",
        "  0.2032,                                 !- Thickness{ m }",
        "  1.326,                                  !- Conductivity{ W / m - K }",
        "  1841.99999999999,                       !- Density{ kg / m3 }",
        "  911.999999999999,                       !- Specific Heat{ J / kg - K }",
        "  0.9,                                    !- Thermal Absorptance",
        "  0.7,                                    !- Solar Absorptance",
        "  0.7;                                    !- Visible Absorptance",
        "",
        "Construction,",
        "   Typical,   !- Name",
        "   8 in.Concrete Block Basement Wall;     !- Layer 1",

        "BuildingSurface:Detailed,                                   ",
        "  Surface A,               !- Name",
        "  Wall,                    !- Surface Type",
        "  Typical,                 !- Construction Name",
        "  ZONE 1,                  !- Zone Name",
        "    ,                        !- Space Name",
        "  Surface,                 !- Outside Boundary Condition",
        "  Surface B,               !- Outside Boundary Condition Object", // Surface B doesn't exist!
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  ,                        !- Number of Vertices",
        "  251.4600375, 3.5052, 0, !- X, Y, Z Vertex 1 {m}",
        "  251.4600375, 0, 0, !- X, Y, Z Vertex 2 {m}",
        "  249.9571375, 0, 0, !- X, Y, Z Vertex 3 {m}",
        "  248.5215375, 1.0, 0, !- X, Y, Z Vertex 4 {m}",
        "  248.5215375, 3.5052, 0;                 !- X, Y, Z Vertex 5 {m}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Read Material and Construction, and expect no errors
    GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataGlobal->NumOfZones = 2;
    state->dataHeatBal->Zone.allocate(2);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(2).Name = "ZONE 2";
    state->dataSurfaceGeometry->SurfaceTmp.allocate(1);
    int SurfNum = 0;
    int TotHTSurfs = 1;
    Array1D_string const BaseSurfCls(3, {"WALL", "FLOOR", "ROOF"});
    Array1D<DataSurfaces::SurfaceClass> const BaseSurfIDs(
        3, {DataSurfaces::SurfaceClass::Wall, DataSurfaces::SurfaceClass::Floor, DataSurfaces::SurfaceClass::Roof});
    int NeedToAddSurfaces;

    GetGeometryParameters(*state, ErrorsFound);
    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);

    state->dataSurfaceGeometry->CosZoneRelNorth = 1.0;
    state->dataSurfaceGeometry->SinZoneRelNorth = 0.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;

    GetHTSurfaceData(*state, ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);

    // We expect one surface, but an error since Surface B cannot be located
    EXPECT_EQ(1, SurfNum);
    EXPECT_TRUE(ErrorsFound);

    std::string const error_string = delimited_string(
        {"   ** Severe  ** RoofCeiling:Detailed=\"SURFACE A\" references an outside boundary surface that cannot be found:SURFACE B"});
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_InternalMassSurfacesCount)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    G SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    Office,                  !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M SE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T SE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T Corridor,              !- Name",
        "    ,                        !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G Corridor,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M Corridor,              !- Name",
        "    ,                        !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Construction,",
        "    InteriorFurnishings,     !- Name",
        "    Std Wood 6inch;          !- Outside Layer",

        "  Material,",
        "    Std Wood 6inch,          !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.15,                    !- Thickness {m}",
        "    0.12,                    !- Conductivity {W/m-K}",
        "    540.0000,                !- Density {kg/m3}",
        "    1210,                    !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "  InternalMass,",
        "    GFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_GF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    88.249272671219;         !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_GF,      !- Name",
        "    G SW Apartment,          !- Zone 1 Name",
        "    G NW Apartment,          !- Zone 2 Name",
        "    Office,                  !- Zone 3 Name",
        "    G NE Apartment,          !- Zone 4 Name",
        "    G N1 Apartment,          !- Zone 5 Name",
        "    G N2 Apartment,          !- Zone 6 Name",
        "    G S1 Apartment,          !- Zone 7 Name",
        "    G S2 Apartment;          !- Zone 8 Name",

        "  InternalMass,",
        "    MFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_MF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    176.498545342438;        !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_MF,      !- Name",
        "    M SW Apartment,          !- Zone 1 Name",
        "    M NW Apartment,          !- Zone 2 Name",
        "    M SE Apartment,          !- Zone 3 Name",
        "    M NE Apartment,          !- Zone 4 Name",
        "    M N1 Apartment,          !- Zone 5 Name",
        "    M N2 Apartment,          !- Zone 6 Name",
        "    M S1 Apartment,          !- Zone 7 Name",
        "    M S2 Apartment;          !- Zone 8 Name",

        "  InternalMass,",
        "    TFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_TF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    88.249272671219;         !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_TF,      !- Name",
        "    T SW Apartment,          !- Zone 1 Name",
        "    T NW Apartment,          !- Zone 2 Name",
        "    T SE Apartment,          !- Zone 3 Name",
        "    T NE Apartment,          !- Zone 4 Name",
        "    T N1 Apartment,          !- Zone 5 Name",
        "    T N2 Apartment,          !- Zone 6 Name",
        "    T S1 Apartment,          !- Zone 7 Name",
        "    T S2 Apartment;          !- Zone 8 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Read Materials
    GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Construction
    GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Read Zones
    GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    // Read InternalMass Object Count
    int TotIntMass = state->dataInputProcessing->inputProcessor->getNumObjectsFound(*state, "InternalMass");
    // check the three internal mass objects
    EXPECT_EQ(3, TotIntMass);

    // Read InternalMass Surfaces Count
    int TotalNumOfInternalMassSurfaces = GetNumIntMassSurfaces(*state);
    // check the 24 internal mass surfaces created from the three zoneLists
    EXPECT_EQ(24, TotalNumOfInternalMassSurfaces);
}
TEST_F(EnergyPlusFixture, SurfaceGeometry_CreateInternalMassSurfaces)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    G SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    Office,                  !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M SE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T SW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T NW Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T SE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T NE Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    34.7455054899131,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T N1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T N2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    9.29594664423115,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T S1 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    11.5818351633044,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T S2 Apartment,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    23.1636703266088,        !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    T Corridor,              !- Name",
        "    ,                        !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    9.14355407629293,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    G Corridor,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Zone,",
        "    M Corridor,              !- Name",
        "    ,                        !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    7.61962839691078,        !- Y Origin {m}",
        "    3.04785135876431,        !- Z Origin {m}",
        "    1,                       !- Type",
        "    1;                       !- Multiplier",

        "  Construction,",
        "    InteriorFurnishings,     !- Name",
        "    Std Wood 6inch;          !- Outside Layer",

        "  Material,",
        "    Std Wood 6inch,          !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.15,                    !- Thickness {m}",
        "    0.12,                    !- Conductivity {W/m-K}",
        "    540.0000,                !- Density {kg/m3}",
        "    1210,                    !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "  InternalMass,",
        "    GFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_GF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    88.249272671219;         !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_GF,      !- Name",
        "    G SW Apartment,          !- Zone 1 Name",
        "    G NW Apartment,          !- Zone 2 Name",
        "    Office,                  !- Zone 3 Name",
        "    G NE Apartment,          !- Zone 4 Name",
        "    G N1 Apartment,          !- Zone 5 Name",
        "    G N2 Apartment,          !- Zone 6 Name",
        "    G S1 Apartment,          !- Zone 7 Name",
        "    G S2 Apartment;          !- Zone 8 Name",

        "  InternalMass,",
        "    MFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_MF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    176.498545342438;        !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_MF,      !- Name",
        "    M SW Apartment,          !- Zone 1 Name",
        "    M NW Apartment,          !- Zone 2 Name",
        "    M SE Apartment,          !- Zone 3 Name",
        "    M NE Apartment,          !- Zone 4 Name",
        "    M N1 Apartment,          !- Zone 5 Name",
        "    M N2 Apartment,          !- Zone 6 Name",
        "    M S1 Apartment,          !- Zone 7 Name",
        "    M S2 Apartment;          !- Zone 8 Name",

        "  InternalMass,",
        "    TFloorZonesIntMass,      !- Name",
        "    InteriorFurnishings,     !- Construction Name",
        "    IntMassZoneList_TF,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    88.249272671219;         !- Surface Area {m2}",

        "  ZoneList,",
        "    IntMassZoneList_TF,      !- Name",
        "    T SW Apartment,          !- Zone 1 Name",
        "    T NW Apartment,          !- Zone 2 Name",
        "    T SE Apartment,          !- Zone 3 Name",
        "    T NE Apartment,          !- Zone 4 Name",
        "    T N1 Apartment,          !- Zone 5 Name",
        "    T N2 Apartment,          !- Zone 6 Name",
        "    T S1 Apartment,          !- Zone 7 Name",
        "    T S2 Apartment;          !- Zone 8 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Read Materials
    GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Construction
    GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Read Zones
    GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    // Read InternalMass Object Count
    int TotIntMass = state->dataInputProcessing->inputProcessor->getNumObjectsFound(*state, "InternalMass");
    EXPECT_EQ(3, TotIntMass);

    // Read InternalMass Surfaces Count
    int TotalNumOfInternalMassSurfaces = GetNumIntMassSurfaces(*state);
    EXPECT_EQ(24, TotalNumOfInternalMassSurfaces);

    state->dataSurface->TotSurfaces = TotalNumOfInternalMassSurfaces;
    state->dataSurfaceGeometry->SurfaceTmp.allocate(state->dataSurface->TotSurfaces);

    int SurfNum = 0;
    GetIntMassSurfaceData(*state, ErrorsFound, SurfNum);
    ASSERT_FALSE(ErrorsFound);

    // check internal mass surface count and object names
    EXPECT_EQ(8, state->dataSurface->IntMassObjects(1).NumOfZones);
    EXPECT_EQ("GFLOORZONESINTMASS", state->dataSurface->IntMassObjects(1).Name);
    EXPECT_EQ(8, state->dataSurface->IntMassObjects(2).NumOfZones);
    EXPECT_EQ("MFLOORZONESINTMASS", state->dataSurface->IntMassObjects(2).Name);
    EXPECT_EQ(8, state->dataSurface->IntMassObjects(3).NumOfZones);
    EXPECT_EQ("TFLOORZONESINTMASS", state->dataSurface->IntMassObjects(3).Name);
    // check total count of internal surfaces created
    EXPECT_EQ(24, state->dataSurface->TotSurfaces);

    // check unique internal surface name created created from a combination
    // of zone name and internal mass object name represented in the zone
    // first zone in the ground floor ZoneList
    EXPECT_EQ("G SW APARTMENT", state->dataHeatBal->Zone(1).Name);
    EXPECT_EQ("GFLOORZONESINTMASS", state->dataSurface->IntMassObjects(1).Name);
    EXPECT_EQ("G SW APARTMENT GFLOORZONESINTMASS", state->dataSurfaceGeometry->SurfaceTmp(1).Name);
    // first zone in the middle floor ZoneList
    EXPECT_EQ("M SW APARTMENT", state->dataHeatBal->Zone(9).Name);
    EXPECT_EQ("MFLOORZONESINTMASS", state->dataSurface->IntMassObjects(2).Name);
    EXPECT_EQ("M SW APARTMENT MFLOORZONESINTMASS", state->dataSurfaceGeometry->SurfaceTmp(9).Name);
    // first zone in the top floor ZoneList
    EXPECT_EQ("T SW APARTMENT", state->dataHeatBal->Zone(17).Name);
    EXPECT_EQ("TFLOORZONESINTMASS", state->dataSurface->IntMassObjects(3).Name);
    EXPECT_EQ("T SW APARTMENT TFLOORZONESINTMASS", state->dataSurfaceGeometry->SurfaceTmp(17).Name);
}

TEST_F(EnergyPlusFixture, WorldCoord_with_RelativeRectSurfCoord_test1)
{
    // Case 1) NOT world coordinate system (Relative) - No error

    std::string const idf_objects = delimited_string({

        "GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    Relative,                !- Coordinate System",
        "    Relative,                !- Daylighting Reference Point Coordinate System",
        "    Relative;                !- Rectangular Surface Coordinate System",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(1).OriginX = 0;
    state->dataHeatBal->Zone(1).OriginY = 0;
    state->dataHeatBal->Zone(1).OriginZ = 0;

    GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(has_err_output(true));
}

TEST_F(EnergyPlusFixture, WorldCoord_with_RelativeRectSurfCoord_test2)
{
    // Case 2) World coordinate system & All zero zone origins - No error

    std::string const idf_objects = delimited_string({

        "GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World,                   !- Coordinate System",
        "    Relative,                !- Daylighting Reference Point Coordinate System",
        "    Relative;                !- Rectangular Surface Coordinate System",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(1).OriginX = 0;
    state->dataHeatBal->Zone(1).OriginY = 0;
    state->dataHeatBal->Zone(1).OriginZ = 0;

    GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(has_err_output(true));
}

TEST_F(EnergyPlusFixture, WorldCoord_with_RelativeRectSurfCoord_test3)
{
    // Case 3) World coordinate system & Relative Rect. surf, coordinate system & Non-zero zone origin

    std::string const idf_objects = delimited_string({

        "GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World,                   !- Coordinate System",
        "    Relative,                !- Daylighting Reference Point Coordinate System",
        "    Relative;                !- Rectangular Surface Coordinate System",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(1).OriginX = 6;
    state->dataHeatBal->Zone(1).OriginY = 6;
    state->dataHeatBal->Zone(1).OriginZ = 0;

    GetGeometryParameters(*state, ErrorsFound);
    EXPECT_TRUE(has_err_output(false));

    std::string error_string = delimited_string({
        "   ** Warning ** GlobalGeometryRules: Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on "
        "the default SurfaceGeometry for 'Relative to zone' coordinate.",
        "   **   ~~~   ** Coordinate System=\"WORLD\"; while ",
        "   **   ~~~   ** Rectangular Surface Coordinate System=\"RELATIVE\".",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, WorldCoord_with_RelativeRectSurfCoord_test4)
{
    // Case 4) World coordinate system & Defalut Rect. surf, coordinate system & Non-zero zone origin

    std::string const idf_objects = delimited_string({

        "GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World,                   !- Coordinate System",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(1).OriginX = 6;
    state->dataHeatBal->Zone(1).OriginY = 6;
    state->dataHeatBal->Zone(1).OriginZ = 0;

    GetGeometryParameters(*state, ErrorsFound);
    EXPECT_TRUE(has_err_output(false));

    std::string error_string = delimited_string({
        "   ** Warning ** GlobalGeometryRules: Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on "
        "the default SurfaceGeometry for 'Relative to zone' coordinate.",
        "   **   ~~~   ** Coordinate System=\"WORLD\"; while ",
        "   **   ~~~   ** Rectangular Surface Coordinate System=\"defaults to RELATIVE\".",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_CheckForReversedLayers)
{
    bool RevLayerDiffs;
    state->dataConstruction->Construct.allocate(6);
    state->dataMaterial->Material.allocate(7);

    // Case 1a: Constructs with regular materials are a reverse of each other--material layers match in reverse (should get a "false" answer)
    state->dataConstruction->Construct(1).TotLayers = 3;
    state->dataConstruction->Construct(1).LayerPoint(1) = 1;
    state->dataConstruction->Construct(1).LayerPoint(2) = 2;
    state->dataConstruction->Construct(1).LayerPoint(3) = 3;
    state->dataConstruction->Construct(2).TotLayers = 3;
    state->dataConstruction->Construct(2).LayerPoint(1) = 3;
    state->dataConstruction->Construct(2).LayerPoint(2) = 2;
    state->dataConstruction->Construct(2).LayerPoint(3) = 1;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(*state, RevLayerDiffs, 1, 2, 3);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 1a: Constructs with regular materials are not reverse of each other--material layers do not match in reverse (should get a "true" answer)
    state->dataConstruction->Construct(2).LayerPoint(1) = 1;
    state->dataConstruction->Construct(2).LayerPoint(3) = 3;
    state->dataMaterial->Material(1).Group = DataHeatBalance::MaterialGroup::RegularMaterial;
    state->dataMaterial->Material(2).Group = DataHeatBalance::MaterialGroup::RegularMaterial;
    state->dataMaterial->Material(3).Group = DataHeatBalance::MaterialGroup::RegularMaterial;
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(*state, RevLayerDiffs, 1, 2, 3);
    EXPECT_TRUE(RevLayerDiffs);

    // Case 2a: Constructs are reverse of each other using WindowGlass, front/back properties properly switched (should get a "false" answer)
    state->dataConstruction->Construct(3).TotLayers = 3;
    state->dataConstruction->Construct(3).LayerPoint(1) = 4;
    state->dataConstruction->Construct(3).LayerPoint(2) = 2;
    state->dataConstruction->Construct(3).LayerPoint(3) = 5;
    state->dataConstruction->Construct(4).TotLayers = 3;
    state->dataConstruction->Construct(4).LayerPoint(1) = 4;
    state->dataConstruction->Construct(4).LayerPoint(2) = 2;
    state->dataConstruction->Construct(4).LayerPoint(3) = 5;
    state->dataMaterial->Material(4).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(4).Thickness = 0.15;
    state->dataMaterial->Material(4).ReflectSolBeamFront = 0.35;
    state->dataMaterial->Material(4).ReflectSolBeamBack = 0.25;
    state->dataMaterial->Material(4).TransVis = 0.45;
    state->dataMaterial->Material(4).ReflectVisBeamFront = 0.34;
    state->dataMaterial->Material(4).ReflectVisBeamBack = 0.24;
    state->dataMaterial->Material(4).TransThermal = 0.44;
    state->dataMaterial->Material(4).AbsorpThermalFront = 0.33;
    state->dataMaterial->Material(4).AbsorpThermalBack = 0.23;
    state->dataMaterial->Material(4).Conductivity = 0.43;
    state->dataMaterial->Material(4).GlassTransDirtFactor = 0.67;
    state->dataMaterial->Material(4).SolarDiffusing = true;
    state->dataMaterial->Material(4).YoungModulus = 0.89;
    state->dataMaterial->Material(4).PoissonsRatio = 1.11;
    state->dataMaterial->Material(5).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(5).Thickness = 0.15;
    state->dataMaterial->Material(5).ReflectSolBeamFront = 0.25;
    state->dataMaterial->Material(5).ReflectSolBeamBack = 0.35;
    state->dataMaterial->Material(5).TransVis = 0.45;
    state->dataMaterial->Material(5).ReflectVisBeamFront = 0.24;
    state->dataMaterial->Material(5).ReflectVisBeamBack = 0.34;
    state->dataMaterial->Material(5).TransThermal = 0.44;
    state->dataMaterial->Material(5).AbsorpThermalFront = 0.23;
    state->dataMaterial->Material(5).AbsorpThermalBack = 0.33;
    state->dataMaterial->Material(5).Conductivity = 0.43;
    state->dataMaterial->Material(5).GlassTransDirtFactor = 0.67;
    state->dataMaterial->Material(5).SolarDiffusing = true;
    state->dataMaterial->Material(5).YoungModulus = 0.89;
    state->dataMaterial->Material(5).PoissonsRatio = 1.11;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(*state, RevLayerDiffs, 3, 4, 3);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 2b: Constructs are reverse of each other using WindowGlass, front/back properties NOT properly switched (should get a "true" answer)
    state->dataMaterial->Material(5).ReflectVisBeamFront = 0.34; // correct would be 0.24
    state->dataMaterial->Material(5).ReflectVisBeamBack = 0.24;  // correct would be 0.34
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(*state, RevLayerDiffs, 3, 4, 3);
    EXPECT_TRUE(RevLayerDiffs);

    // Case 3a: Single layer constructs using Equivalent Glass, front/back properties properly switched (should get a "false" answer)
    state->dataConstruction->Construct(5).TotLayers = 1;
    state->dataConstruction->Construct(5).LayerPoint(1) = 6;
    state->dataConstruction->Construct(6).TotLayers = 1;
    state->dataConstruction->Construct(6).LayerPoint(1) = 7;
    state->dataMaterial->Material(6).Group = DataHeatBalance::MaterialGroup::GlassEquivalentLayer;
    state->dataMaterial->Material(6).TausFrontBeamBeam = 0.39;
    state->dataMaterial->Material(6).TausBackBeamBeam = 0.29;
    state->dataMaterial->Material(6).ReflFrontBeamBeam = 0.38;
    state->dataMaterial->Material(6).ReflBackBeamBeam = 0.28;
    state->dataMaterial->Material(6).TausFrontBeamBeamVis = 0.37;
    state->dataMaterial->Material(6).TausBackBeamBeamVis = 0.27;
    state->dataMaterial->Material(6).ReflFrontBeamBeamVis = 0.36;
    state->dataMaterial->Material(6).ReflBackBeamBeamVis = 0.26;
    state->dataMaterial->Material(6).TausFrontBeamDiff = 0.35;
    state->dataMaterial->Material(6).TausBackBeamDiff = 0.25;
    state->dataMaterial->Material(6).ReflFrontBeamDiff = 0.34;
    state->dataMaterial->Material(6).ReflBackBeamDiff = 0.24;
    state->dataMaterial->Material(6).TausFrontBeamDiffVis = 0.33;
    state->dataMaterial->Material(6).TausBackBeamDiffVis = 0.23;
    state->dataMaterial->Material(6).ReflFrontBeamDiffVis = 0.32;
    state->dataMaterial->Material(6).ReflBackBeamDiffVis = 0.22;
    state->dataMaterial->Material(6).TausDiffDiff = 0.456;
    state->dataMaterial->Material(6).ReflFrontDiffDiff = 0.31;
    state->dataMaterial->Material(6).ReflBackDiffDiff = 0.21;
    state->dataMaterial->Material(6).TausDiffDiffVis = 0.345;
    state->dataMaterial->Material(6).ReflFrontDiffDiffVis = 0.30;
    state->dataMaterial->Material(6).ReflBackDiffDiffVis = 0.20;
    state->dataMaterial->Material(6).TausThermal = 0.234;
    state->dataMaterial->Material(6).EmissThermalFront = 0.888;
    state->dataMaterial->Material(6).EmissThermalBack = 0.777;
    state->dataMaterial->Material(6).Resistance = 1.234;
    state->dataMaterial->Material(7).Group = DataHeatBalance::MaterialGroup::GlassEquivalentLayer;
    state->dataMaterial->Material(7).TausFrontBeamBeam = 0.29;
    state->dataMaterial->Material(7).TausBackBeamBeam = 0.39;
    state->dataMaterial->Material(7).ReflFrontBeamBeam = 0.28;
    state->dataMaterial->Material(7).ReflBackBeamBeam = 0.38;
    state->dataMaterial->Material(7).TausFrontBeamBeamVis = 0.27;
    state->dataMaterial->Material(7).TausBackBeamBeamVis = 0.37;
    state->dataMaterial->Material(7).ReflFrontBeamBeamVis = 0.26;
    state->dataMaterial->Material(7).ReflBackBeamBeamVis = 0.36;
    state->dataMaterial->Material(7).TausFrontBeamDiff = 0.25;
    state->dataMaterial->Material(7).TausBackBeamDiff = 0.35;
    state->dataMaterial->Material(7).ReflFrontBeamDiff = 0.24;
    state->dataMaterial->Material(7).ReflBackBeamDiff = 0.34;
    state->dataMaterial->Material(7).TausFrontBeamDiffVis = 0.23;
    state->dataMaterial->Material(7).TausBackBeamDiffVis = 0.33;
    state->dataMaterial->Material(7).ReflFrontBeamDiffVis = 0.22;
    state->dataMaterial->Material(7).ReflBackBeamDiffVis = 0.32;
    state->dataMaterial->Material(7).TausDiffDiff = 0.456;
    state->dataMaterial->Material(7).ReflFrontDiffDiff = 0.21;
    state->dataMaterial->Material(7).ReflBackDiffDiff = 0.31;
    state->dataMaterial->Material(7).TausDiffDiffVis = 0.345;
    state->dataMaterial->Material(7).ReflFrontDiffDiffVis = 0.20;
    state->dataMaterial->Material(7).ReflBackDiffDiffVis = 0.30;
    state->dataMaterial->Material(7).TausThermal = 0.234;
    state->dataMaterial->Material(7).EmissThermalFront = 0.777;
    state->dataMaterial->Material(7).EmissThermalBack = 0.888;
    state->dataMaterial->Material(7).Resistance = 1.234;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(*state, RevLayerDiffs, 5, 6, 1);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 3a: Single layer constructs using Equivalent Glass, front/back properties NOT properly switched (should get a "true" answer)
    state->dataMaterial->Material(7).EmissThermalFront = 0.888;
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(*state, RevLayerDiffs, 5, 6, 1);
    EXPECT_TRUE(RevLayerDiffs);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresNoAirBoundaries)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).Name, "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(3).Name, "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[0], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(3).spaceNames[0], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).radiantEnclosureNum, 2);
    EXPECT_EQ(state->dataHeatBal->space(3).radiantEnclosureNum, 3);

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).Name, "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(3).Name, "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[0], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(3).spaceNames[0], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).solarEnclosureNum, 2);
    EXPECT_EQ(state->dataHeatBal->space(3).solarEnclosureNum, 3);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresWithAirBoundaries1)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",
        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[1], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[2], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).radiantEnclosureNum, 1);

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[1], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[2], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).solarEnclosureNum, 1);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresWithAirBoundaries2)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",

        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    ErrorsFound = false;

    // std::string const error_string = delimited_string({
    //"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone 6\" did not find a matching radiant or solar
    // enclosure name."
    //    });
    // EXPECT_TRUE(compare_err_stream(error_string, true));

    // For this test case, Zones 1 and 3 share radiant and solar enclosures

    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[1], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).Name, "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[0], "Space 2"));
    EXPECT_EQ(state->dataHeatBal->space(1).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).radiantEnclosureNum, 2);
    EXPECT_EQ(state->dataHeatBal->space(3).radiantEnclosureNum, 1);

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[1], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).Name, "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[0], "Space 2"));
    EXPECT_EQ(state->dataHeatBal->space(1).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).solarEnclosureNum, 2);
    EXPECT_EQ(state->dataHeatBal->space(3).solarEnclosureNum, 1);

    // Check surface order
    int Zone1Surface1 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone1-Surface1"), state->dataSurface->Surface);
    int Zone1Surface2 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone1-Surface2"), state->dataSurface->Surface);
    int Zone2Surface1 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone2-Surface1"), state->dataSurface->Surface);
    int Zone3Surface1 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone3-Surface1"), state->dataSurface->Surface);
    int Zone1Floor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone1-Floor"), state->dataSurface->Surface);
    int Zone2Floor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone2-Floor"), state->dataSurface->Surface);
    int Zone3Floor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone3-Floor"), state->dataSurface->Surface);

    EXPECT_EQ(state->dataHeatBal->Zone(1).AllSurfaceFirst, Zone1Surface2);     // air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(1).AllSurfaceFirst + 1, Zone1Surface1); // air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(2).AllSurfaceFirst, Zone2Surface1);     // no air boundary surfaces in Zone 2
    EXPECT_EQ(state->dataHeatBal->Zone(3).AllSurfaceFirst, Zone3Surface1);     // air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(1).HTSurfaceFirst, Zone1Surface1);      // first non-air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(2).HTSurfaceFirst, Zone2Surface1);      // first non-air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(3).HTSurfaceFirst, Zone3Floor);         // first non-air boundary surface
    EXPECT_EQ(state->dataHeatBal->Zone(1).AllSurfaceLast, Zone1Floor);
    EXPECT_EQ(state->dataHeatBal->Zone(2).AllSurfaceLast, Zone2Floor);
    EXPECT_EQ(state->dataHeatBal->Zone(3).AllSurfaceLast, Zone3Floor);
    EXPECT_EQ(state->dataHeatBal->Zone(1).HTSurfaceLast, Zone1Floor);
    EXPECT_EQ(state->dataHeatBal->Zone(2).HTSurfaceLast, Zone2Floor);
    EXPECT_EQ(state->dataHeatBal->Zone(3).HTSurfaceLast, Zone3Floor);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresWithAirBoundaries3)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "Space,",
        "Space 4,             !- Name",
        "Zone 4;             !- Zone Name",

        "Space,",
        "Space 5,             !- Name",
        "Zone 5;             !- Zone Name",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",
        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone4-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone4-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 4,       !- Zone Name",
        "    Space 4,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone4-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 4,       !- Zone Name",
        "    Space 4,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone5-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone5-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 5,       !- Zone Name",
        "    Space 5,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone4-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface3,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone5-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone5-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 5,       !- Zone Name",
        "    Space 5,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface3,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone4-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 4,       !- Zone Name",
        "    Space 4,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone5-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 5,       !- Zone Name",
        "    Space 5,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    // For this test case, there are air boundaries
    // Between Zones 1 and 2
    // None between Zones 2 and 3
    // Between Zones 3 and 4
    // Between Zones 4 and 5
    // Between Zones 1 and 5
    // This should trigger the enclosure merging and all five zones should share a radiant and solar enclosure

    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[1], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[2], "Space 5"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[3], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[4], "Space 4"));
    EXPECT_EQ(state->dataHeatBal->space(1).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(4).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(5).radiantEnclosureNum, 1);

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[1], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[2], "Space 5"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[3], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[4], "Space 4"));
    EXPECT_EQ(state->dataHeatBal->space(1).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(2).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(4).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(5).solarEnclosureNum, 1);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresWithAirBoundaries4)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "Zone,",
        "Zone 6;             !- Name",

        "Zone,",
        "Zone 7;             !- Name",

        "Zone,",
        "Zone 8;             !- Name",

        "Zone,",
        "Zone 9;             !- Name",

        "Zone,",
        "Zone 10;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "Space,",
        "Space 4,             !- Name",
        "Zone 4;             !- Zone Name",

        "Space,",
        "Space 5,             !- Name",
        "Zone 5;             !- Zone Name",

        "Space,",
        "Space 6,             !- Name",
        "Zone 6;             !- Zone Name",

        "Space,",
        "Space 7,             !- Name",
        "Zone 7;             !- Zone Name",

        "Space,",
        "Space 8,             !- Name",
        "Zone 8;             !- Zone Name",

        "Space,",
        "Space 9,             !- Name",
        "Zone 9;             !- Zone Name",

        "Space,",
        "Space 10,             !- Name",
        "Zone 10;             !- Zone Name",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",
        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone4-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone4-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone2-Surface3,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone9-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone5-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone4-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 4,       !- Zone Name",
        "    Space 4,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone5-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 5,       !- Zone Name",
        "    Space 5,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone6-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 6,       !- Zone Name",
        "    Space 6,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone7-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone7-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 7,       !- Zone Name",
        "    Space 7,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone6-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone7-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 7,       !- Zone Name",
        "    Space 7,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone10-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone9-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 9,       !- Zone Name",
        "    Space 9,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone2-Surface3,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone9-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 9,       !- Zone Name",
        "    Space 9,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone10-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone10-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 10,       !- Zone Name",
        "    Space 10,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone9-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone4-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 4,       !- Zone Name",
        "    Space 4,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone5-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 5,       !- Zone Name",
        "    Space 5,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone6-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 6,       !- Zone Name",
        "    Space 6,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone7-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 7,       !- Zone Name",
        "    Space 7,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone8-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 8,       !- Zone Name",
        "    Space 8,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone9-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 9,       !- Zone Name",
        "    Space 9,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone10-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 10,       !- Zone Name",
        "    Space 10,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    // This test is designed to reproduce issue #9218 Crash with many airwalls collapsing into a single enclosure.
    // That proved difficult to reproduce, however the root cause of the crash is space enclosure nums that are not consistent
    // with the enclosures that hold them. This test demostrates that failure.
    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[1], "Space 5"));

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).Name, "Radiant Enclosure 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[0], "Space 6"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[1], "Space 7"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[2], "Space 10"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[3], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[4], "Space 4"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[5], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[6], "Space 9"));

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(3).Name, "Zone 8"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(3).spaceNames[0], "Space 8"));

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[1], "Space 5"));

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).Name, "Solar Enclosure 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[0], "Space 6"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[1], "Space 7"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[2], "Space 10"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[3], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[4], "Space 4"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[5], "Space 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[6], "Space 9"));

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(3).Name, "Zone 8"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(3).spaceNames[0], "Space 8"));

    // Loop through all spaces on all enclosures and check that space radiantEnlosureNum matches the enclosure number
    // Before the fix, Space 3 and Space 5 have incorrect radiantEnclosureNum = 2 (should be 1)
    // And all of the spaces in enclousre 2 have incorrect radiantEnclosureNum = 3
    for (int enclNum = 1; enclNum <= state->dataViewFactor->NumOfRadiantEnclosures; ++enclNum) {
        for (int spaceNum : state->dataViewFactor->EnclRadInfo(enclNum).spaceNums) {
            EXPECT_EQ(state->dataHeatBal->space(spaceNum).radiantEnclosureNum, enclNum);
        }
    }

    // Check solar enclosures
    for (int enclNum = 1; enclNum <= state->dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        for (int spaceNum : state->dataViewFactor->EnclSolInfo(enclNum).spaceNums) {
            EXPECT_EQ(state->dataHeatBal->space(spaceNum).solarEnclosureNum, enclNum);
        }
    }
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_SetupEnclosuresWithAirBoundaries5_SameSpace)
{

    // Test for issue #9233 Enclosure area incorrect when air boundary surface has the same space on both sides
    // If a Construction:AirBoundary surface has the same Space on both sides, the enclosure space list will include that space twice, and the floor
    // area and other attributes of the enclosure will double-count that Space.

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Space,",
        "Space 1,             !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 2,             !- Name",
        "Zone 2;             !- Zone Name",

        "Space,",
        "Space 3,             !- Name",
        "Zone 3;             !- Zone Name",

        "  Construction,",
        "    Dbl Clr 3mm/6mm Air,     !- Name",
        "    CLEAR 3MM,               !- Outside Layer",
        "    AIR 6MM,                 !- Layer 2",
        "    CLEAR 3MM;               !- Layer 3",

        "  WindowMaterial:Glazing,",
        "    CLEAR 3MM,               !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.837,                   !- Solar Transmittance at Normal Incidence",
        "    0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.898,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "  WindowMaterial:Gas,",
        "    AIR 6MM,                 !- Name",
        "    AIR,                     !- Gas Type",
        "    0.006;                   !- Thickness {m}",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",
        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface1b,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1b,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface2,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone3-Surface1,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone3-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone1-Surface2,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 1",
        "    0,1,1,              !- Vertex 1",
        "    0,0,1;              !- Vertex 1",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    SunExposed,                   !- Sun Exposure",
        "    WindExposed,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "FenestrationSurface:Detailed,",
        "WF - 1,            !-Name ",
        " WINDOW, !-Surface Type",
        " Dbl Clr 3mm/6mm Air,                !-Construction Name ",
        "Zone1-Floor, !-Building Surface Name",
        ",        !-Outside Boundary Condition Object ",
        "autocalculate,                 !- View Factor to Ground",
        ", !-Frame and Divider Name",
        "1,                       !- Multiplier",
        "4,                       !- Number of Vertices",
        "    0.1,0.1,0,              !- Vertex 1",
        "    0.1,0.9,0,              !- Vertex 2",
        "    0.9,0.9,0,              !- Vertex 3",
        "    0.9,0.1,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor2,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    SunExposed,                   !- Sun Exposure",
        "    WindExposed,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    Space 2,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors

    EXPECT_EQ(state->dataViewFactor->NumOfRadiantEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(1).spaceNames[1], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).radiantEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).radiantEnclosureNum, 1);
    Real64 enclArea = state->dataHeatBal->space(1).floorArea + state->dataHeatBal->space(3).floorArea;
    EXPECT_EQ(state->dataViewFactor->EnclRadInfo(1).FloorArea, enclArea);

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).Name, "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclRadInfo(2).spaceNames[0], "Space 2"));
    EXPECT_EQ(state->dataHeatBal->space(2).radiantEnclosureNum, 2);
    enclArea = state->dataHeatBal->space(2).floorArea;
    EXPECT_EQ(state->dataViewFactor->EnclRadInfo(2).FloorArea, enclArea);

    EXPECT_EQ(state->dataViewFactor->NumOfSolarEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[0], "Space 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(1).spaceNames[1], "Space 3"));
    EXPECT_EQ(state->dataHeatBal->space(1).solarEnclosureNum, 1);
    EXPECT_EQ(state->dataHeatBal->space(3).solarEnclosureNum, 1);
    enclArea = state->dataHeatBal->space(1).floorArea + state->dataHeatBal->space(3).floorArea;
    Real64 enclExtWindowArea = state->dataHeatBal->space(1).extWindowArea + state->dataHeatBal->space(3).extWindowArea;
    EXPECT_EQ(state->dataViewFactor->EnclSolInfo(1).ExtWindowArea, enclExtWindowArea);
    Real64 enclTotSurfArea = state->dataHeatBal->space(1).totalSurfArea + state->dataHeatBal->space(3).totalSurfArea;
    EXPECT_EQ(state->dataViewFactor->EnclSolInfo(1).TotalSurfArea, enclTotSurfArea);

    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).Name, "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(state->dataViewFactor->EnclSolInfo(2).spaceNames[0], "Space 2"));
    EXPECT_EQ(state->dataHeatBal->space(2).solarEnclosureNum, 2);
    enclArea = state->dataHeatBal->space(2).floorArea;
    EXPECT_EQ(state->dataViewFactor->EnclSolInfo(2).FloorArea, enclArea);
    enclExtWindowArea = state->dataHeatBal->space(2).extWindowArea;
    EXPECT_EQ(state->dataViewFactor->EnclSolInfo(2).ExtWindowArea, enclExtWindowArea);
    enclTotSurfArea = state->dataHeatBal->space(2).totalSurfArea;
    EXPECT_EQ(state->dataViewFactor->EnclSolInfo(2).TotalSurfArea, enclTotSurfArea);
}

TEST_F(EnergyPlusFixture, GetSurfaceData_SurfaceOrder)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
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
        "    CB11,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2032000,               !- Thickness {m}",
        "    1.048000,                !- Conductivity {W/m-K}",
        "    1105.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material,",
        "    GP01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1600000,               !- Conductivity {W/m-K}",
        "    801.0000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    9.0099998E-02,           !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN05,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2458000,               !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    PW03,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    545.0000,                !- Density {kg/m3}",
        "    1213.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "  Material,",
        "    CC03,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1016000,               !- Thickness {m}",
        "    1.310000,                !- Conductivity {W/m-K}",
        "    2243.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    HF-A3,                   !- Name",
        "    Smooth,                  !- Roughness",
        "    1.5000000E-03,           !- Thickness {m}",
        "    44.96960,                !- Conductivity {W/m-K}",
        "    7689.000,                !- Density {kg/m3}",
        "    418.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    AR02,                    !- Name",
        "    VeryRough,               !- Roughness",
        "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    CP02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2170000,               !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  WindowMaterial:Glazing,",
        "    CLEAR 3MM,               !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.837,                   !- Solar Transmittance at Normal Incidence",
        "    0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.898,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "  WindowMaterial:Gas,",
        "    AIR 6MM,                 !- Name",
        "    AIR,                     !- Gas Type",
        "    0.006;                   !- Thickness {m}",

        "  Construction,",
        "    EXTWALL:LIVING,          !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    CB11,                    !- Layer 2",
        "    GP01;                    !- Layer 3",

        "  Construction,",
        "    INTERIORWall,            !- Name",
        "    GP01,                    !- Outside Layer",
        "    IN02,                    !- Layer 2",
        "    GP01;                    !- Layer 3",

        "  Construction,",
        "    FLOOR:GARAGE,            !- Name",
        "    CC03;                    !- Outside Layer",

        "  Construction,",
        "    FLOOR:LIVING,            !- Name",
        "    CC03,                    !- Outside Layer",
        "    CP02;                    !- Layer 2",

        "  Construction,",
        "    ROOF,                    !- Name",
        "    AR02,                    !- Outside Layer",
        "    PW03;                    !- Layer 2",

        "  Construction,",
        "    EXTWALL:GARAGE,          !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    CB11;                    !- Layer 2",

        "  Construction,",
        "    CEILING:LIVING,          !- Name",
        "    IN05,                    !- Outside Layer",
        "    GP01;                    !- Layer 2",

        "  Construction,",
        "    reverseCEILING:LIVING,   !- Name",
        "    GP01,                    !- Outside Layer",
        "    IN05;                    !- Layer 2",

        "  Construction,",
        "    GABLE,                   !- Name",
        "    PW03;                    !- Outside Layer",

        "  Construction,",
        "    Dbl Clr 3mm/6mm Air,     !- Name",
        "    CLEAR 3MM,               !- Outside Layer",
        "    AIR 6MM,                 !- Layer 2",
        "    CLEAR 3MM;               !- Layer 3",

        "  Construction,",
        "    Garage:SteelDoor,        !- Name",
        "    HF-A3;                   !- Outside Layer",

        "  Construction,",
        "    CEILING:Garage,          !- Name",
        "    GP01;                    !- Outside Layer",

        "  Zone,",
        "    LIVING ZONE;             !- Name",

        "  Zone,",
        "    GARAGE ZONE;             !- Name",

        "  Zone,",
        "    ATTIC ZONE;              !- Name",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  WindowMaterial:Glazing,",
        "    Clear Acrylic Plastic,   !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.92,                    !- Solar Transmittance at Normal Incidence",
        "    0.05,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.05,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.92,                    !- Visible Transmittance at Normal Incidence",
        "    0.05,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.05,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.00,                    !- Infrared Transmittance at Normal Incidence",
        "    0.90,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.90,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.90;                    !- Conductivity {W/m-K}",

        "  WindowMaterial:Glazing,",
        "    Diffusing Acrylic Plastic,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.0022,                  !- Thickness {m}",
        "    0.90,                    !- Solar Transmittance at Normal Incidence",
        "    0.08,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.08,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.90,                    !- Visible Transmittance at Normal Incidence",
        "    0.08,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.08,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.00,                    !- Infrared Transmittance at Normal Incidence",
        "    0.90,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.90,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.90;                    !- Conductivity {W/m-K}",
        "",
        "  Material,",
        "    Very High Reflectivity Surface,  !- Name",
        "    Smooth,                  !- Roughness",
        "    0.0005,                  !- Thickness {m}",
        "    237,                     !- Conductivity {W/m-K}",
        "    2702,                    !- Density {kg/m3}",
        "    903,                     !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.05,                    !- Solar Absorptance",
        "    0.05;                    !- Visible Absorptance",
        "  Construction,",
        "    TDD Pipe,                !- Name",
        "    Very High Reflectivity Surface;  !- Outside Layer",
        "",
        "  Construction,",
        "    TDD Dome,                !- Name",
        "    Clear Acrylic Plastic;   !- Outside Layer",
        "",
        "  Construction,",
        "    TDD Diffuser,            !- Name",
        "    Diffusing Acrylic Plastic;  !- Outside Layer",

        "  DaylightingDevice:Tubular,",
        "    Pipe1,                   !- Name",
        "    TubularDaylightingDome1,                   !- Dome Name",
        "    TubularDaylightingDiffuser1,               !- Diffuser Name",
        "    TDD Pipe,                !- Construction Name",
        "    0.3556,                  !- Diameter {m}",
        "    1.4,                     !- Total Length {m}",
        "    0.28,                    !- Effective Thermal Resistance {m2-K/W}",
        "    ATTIC ZONE,              !- Transition Zone 1 Name",
        "    1.1;                     !- Transition Zone 1 Length {m}",

        "  FenestrationSurface:Detailed,",
        "    TubularDaylightingDome1,                   !- Name",
        "    TubularDaylightDome,     !- Surface Type",
        "    TDD Dome,                !- Construction Name",
        "    NorthRoof1,              !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    13.782,5.389,4.6838,  !- X,Y,Z ==> Vertex 1 {m}",
        "    13.782,6.3172,3.8804,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.3172,3.8804,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,5.389,4.6838;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    TubularDaylightingDiffuser1,               !- Name",
        "    TubularDaylightDiffuser, !- Surface Type",
        "    TDD Diffuser,            !- Construction Name",
        "    Living:Ceiling,          !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.3425,3.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.3425,2.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.6575,2.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.6575,3.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:North,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,10.778,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,10.778,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:East,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    17.242,0,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:South,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,2.4383,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,0,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:West,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,10.778,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Garage:Interior,         !- Name",
        "    WALL,                    !- Surface Type",
        "    INTERIORWall,            !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Living:Interior,         !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,10.778,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Interior,         !- Name",
        "    WALL,                    !- Surface Type",
        "    INTERIORWall,            !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Garage:Interior,         !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,10.778,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.323,10.778,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.323,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Floor,            !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR:LIVING,            !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Living:Floor,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,10.778,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Ceiling,          !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Attic:LivingFloor,       !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,0,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Attic:LivingFloor,       !- Name",
        "    FLOOR,                   !- Surface Type",
        "    reverseCEILING:LIVING,   !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Living:Ceiling,          !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,0,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    NorthRoof1,              !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    13.782,5.389,4.6838,  !- X,Y,Z ==> Vertex 1 {m}",
        "    13.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,7.3172,3.8804,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,5.389,4.6838;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    SouthRoof,               !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,5.389000,4.683800,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,2.438400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.24200,0.000000,2.438400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.24200,5.389000,4.683800;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    NorthRoof2,              !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    13.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.332,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,10.778,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,7.3172,3.8804;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    NorthRoof3,              !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    17.242,5.389,4.6838,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,7.3172,3.8804,  !- X,Y,Z ==> Vertex 2 {m}",
        "    13.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 3 {m}",
        "    13.782,5.389,4.6838;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    NorthRoof4,              !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    3,                       !- Number of Vertices",
        "    17.242,7.3172,3.8804,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    13.782,7.3172,3.8804;  !- X,Y,Z ==> Vertex 3 {m}",

        "  BuildingSurface:Detailed,",
        "    EastGable,               !- Name",
        "    WALL,                    !- Surface Type",
        "    GABLE,                   !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    3,                       !- Number of Vertices",
        "    17.242,5.389,4.6838,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,0.0,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,2.4384;  !- X,Y,Z ==> Vertex 3 {m}",

        "  BuildingSurface:Detailed,",
        "    WestGable,               !- Name",
        "    WALL,                    !- Surface Type",
        "    GABLE,                   !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    3,                       !- Number of Vertices",
        "    0.0,5.389,4.6838,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,0.0,2.4384;  !- X,Y,Z ==> Vertex 3 {m}",

        "  BuildingSurface:Detailed,",
        "    EastRoof,                !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    13.782,16.876,3.8804,  !- X,Y,Z ==> Vertex 1 {m}",
        "    13.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,16.876,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    WestRoof,                !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.9,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,16.876,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    13.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 3 {m}",
        "    13.782,16.876,3.8804;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    NorthGable,        !- Name",
        "    WALL,                    !- Surface Type",
        "    GABLE,                   !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    3,                       !- Number of Vertices",
        "    13.782,16.876,3.8804,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,16.876,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.323,16.876,2.4384;  !- X,Y,Z ==> Vertex 3 {m}",

        "  BuildingSurface:Detailed,",
        "    Garage:EastWall,         !- Name",
        "    WALL,                    !- Surface Type",
        "    EXTWALL:GARAGE,          !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,10.778,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,16.876,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,16.876,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Garage:WestWall,         !- Name",
        "    WALL,                    !- Surface Type",
        "    EXTWALL:GARAGE,          !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,16.876,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,16.876,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.323,10.778,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.323,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  InternalMass,",
        "    EVChargingStation,      !- Name",
        "    Garage:SteelDoor,     !- Construction Name",
        "    GARAGE ZONE,      !- Zone or ZoneList Name",
        "    ,                        !- Space Name",
        "    88.249272671219;         !- Surface Area {m2}",

        "  BuildingSurface:Detailed,",
        "    Garage:FrontDoor,        !- Name",
        "    WALL,                    !- Surface Type",
        "    Garage:SteelDoor,        !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    17.242,16.876,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,16.876,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.323,16.876,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.323,16.876,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Attic:GarageFloor,       !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CEILING:Garage,          !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Garage:Ceiling,          !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,10.778,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,16.876,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,16.876,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,10.778,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Garage:Ceiling,          !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:Garage,          !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Attic:GarageFloor,       !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,16.876,2.4384,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,10.778,2.4384,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,16.876,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Garage:Floor,            !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR:GARAGE,            !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Garage:Floor,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    10.323,10.778,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.323,16.876,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,16.876,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,10.778,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    NorthWindow,             !- Name",
        "    Window,                  !- Surface Type",
        "    Dbl Clr 3mm/6mm Air,     !- Construction Name",
        "    Living:North,            !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    6.572,10.778,2.1336,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.572,10.778,0.6096,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2,10.778,0.6096,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2,10.778,2.1336;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    EastWindow,              !- Name",
        "    Window,                  !- Surface Type",
        "    Dbl Clr 3mm/6mm Air,     !- Construction Name",
        "    Living:East,             !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    17.242,2,2.1336,  !- X,Y,Z ==> Vertex 1 {m}",
        "    17.242,2,0.6096,  !- X,Y,Z ==> Vertex 2 {m}",
        "    17.242,6.572,0.6096,  !- X,Y,Z ==> Vertex 3 {m}",
        "    17.242,6.572,2.1336;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    SouthWindow,             !- Name",
        "    Window,                  !- Surface Type",
        "    Dbl Clr 3mm/6mm Air,     !- Construction Name",
        "    Living:South,            !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2,0,2.1336,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2,0,0.6096,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.572,0,0.6096,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.572,0,2.1336;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    WestWindow,              !- Name",
        "    Window,                  !- Surface Type",
        "    Dbl Clr 3mm/6mm Air,     !- Construction Name",
        "    Living:West,             !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0,6.572,2.1336,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.572,0.6096,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,2,0.6096,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,2,2.1336;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    WestDoor,              !- Name",
        "    Door,                  !- Surface Type",
        "    Garage:SteelDoor,     !- Construction Name",
        "    Living:West,             !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0,9.0,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,9.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,8.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,8.0,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    AtticSkylight,              !- Name",
        "    Window,                  !- Surface Type",
        "    Dbl Clr 3mm/6mm Air,     !- Construction Name",
        "    EastRoof,             !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    14.782,16.876,3.8804,  !- X,Y,Z ==> Vertex 1 {m}",
        "    14.782,7.3172,3.8804,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.242,10.778,2.4384,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.242,16.876,2.4384;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Shading:Zone:Detailed,",
        "    ZoneShade:Living:South:Shade001,  !- Name",
        "    Living:South,        !- Base Surface Name",
        "    ,              !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "Shading:Building:Detailed,",
        "  BuildingShade:TiltedShadeSurface,             !- Name",
        "  ,                        !- Transmittance Schedule Name",
        "  4,                       !- Number of Vertices",
        "  -40.0,2.0,20.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "  -40.0,0.00,20.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "  -45.0,0.00,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "  -45.0,2.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "Shading:Site:Detailed,",
        "  SiteShade:FlatShadeSurface,             !- Name",
        "  ,                        !- Transmittance Schedule Name",
        "  4,                       !- Number of Vertices",
        "  40.0,2.0,10.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "  40.0,0.00,10.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "  45.0,0.00,10.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "  45.0,2.0,10.0;  !- X,Y,Z ==> Vertex 4 {m}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(*state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);                  // expect no errors

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors

    GetSurfaceData(*state, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);           // expect no errors

    // compare_err_stream( "" ); // just for debugging

    // This tests the sorted order of surfaces in two ways:
    //
    // Simulation Order:
    //   All shading surfaces (including mirrored surfaces)
    //      Shading:Site
    //      Shading:Building
    //      Shading:Zone (and variants)
    //
    //   By zone (in the order the zones appear in the idf):
    //      Walls
    //      Floors
    //      Roofs
    //      Internal Mass
    //      Doors
    //      Windows
    //      TDD_Domes
    //
    // Reporting Order (preserving the old surface order scheme)
    //
    //   All shading surfaces
    //   By zone (in the order the zones appear in the idf):
    //      Walls
    //          Subsurfaces follow each base surface in the order they appear in the idf
    //      Floors
    //          Subsurfaces follow each base surface in the order they appear in the idf
    //      Roofs/Ceilings
    //          Subsurfaces follow each base surface in the order they appear in the idf
    //      Internal Mass
    //
    // Special cases:
    //      TubularDaylightDome is treated as a separate surface type
    //      TubularDaylightDiffuser is treated as a window subsurface

    // For this test, the order should be
    // Simulation Order (1-based):
    //  SHADING SURFACES:
    //      1. TiltedShadeSurface
    //      2. Mir-TiltedShadeSurface (every shading surface is mirrored by default)
    //      3. FlatShadeSurface
    //      4. Mir-FlatShadeSurface
    //      5. Living:South:Shade001
    //      6. Mir-Living:South:Shade001
    //  LIVING ZONE:
    //      7. Living:North (wall)
    //      8. Living:East (wall)
    //      9. Living:South (wall)
    //      10. Living:West (wall)
    //      11. Living:Interior (wall)
    //      12. Living:Ceiling (ceiling)
    //      13. Living:Floor (floor)
    //      14. WestDoor (door)
    //      15. TubularDaylightingDiffuser1 ("window")
    //      16. NorthWindow (window)
    //      17. EastWindow (window)
    //      18. SouthWindow (window)
    //      19. WestWindow (window)
    //  GARAGE ZONE:
    //      20. Garage:Interior (wall)
    //      21. Garage:EastWall (wall)
    //      22. Garage:WestWall (wall)
    //      23. Garage:FrontDoor (wall)
    //      24. Garage:Floor (floor)
    //      25. Garage:Ceiling (ceiling)
    //      26. EVChargingStation (internal mass)
    //
    //  ATTIC ZONE:
    //      27. EastGable (wall)
    //      28. WestGable (wall)
    //      29. NorthGable (wall)
    //      30. Attic:LivingFloor (floor)
    //      31. Attic:GarageFloor (floor)
    //      32. NorthRoof1 (roof)
    //      33. SouthRoof (roof)
    //      34. NorthRoof2 (roof)
    //      35. NorthRoof3 (roof)
    //      36. NorthRoof4 (roof)
    //      37. EastRoof (roof)
    //      38. WestRoof (roof)
    //      39. AtticSkylight (window)
    //      40. TubularDaylightingDome1 (not a window)

    // For this test, the order should be
    // Reporting (legacy) Order (zero-based):
    //  SHADING SURFACES:
    //      0. TiltedShadeSurface
    //      1. Mir-TiltedShadeSurface (every shading surface is mirrored by default)
    //      2. FlatShadeSurface
    //      3. Mir-FlatShadeSurface
    //      4. Living:South:Shade001
    //      5. Mir-Living:South:Shade001
    //  LIVING ZONE:
    //      6. Living:North (wall)
    //      7.   NorthWindow (window)
    //      8. Living:East (wall)
    //      9.  EastWindow (window)
    //      10. Living:South (wall)
    //      11.   SouthWindow (window)
    //      12. Living:West (wall)
    //      13.   WestWindow (window)
    //      14.   WestDoor (door)
    //      15. Living:Interior (wall)
    //      16. Living:Ceiling (ceiling)
    //      17.   TubularDaylightingDiffuser1 ("window")
    //      18. Living:Floor (floor)
    //  GARAGE ZONE:
    //      19. Garage:Interior (wall)
    //      20. Garage:EastWall (wall)
    //      21. Garage:WestWall (wall)
    //      22. Garage:FrontDoor (wall)
    //      23. Garage:Floor (floor)
    //      24. Garage:Ceiling (ceiling)
    //      25. EVChargingStation (internal mass)
    //
    //  ATTIC ZONE:
    //      26. EastGable (wall)
    //      27. WestGable (wall)
    //      28. NorthGable (wall)
    //      29. Attic:LivingFloor (floor)
    //      30. Attic:GarageFloor (floor)
    //      31. NorthRoof1 (roof)

    //      32. SouthRoof (roof)
    //      33. NorthRoof2 (roof)
    //      34. NorthRoof3 (roof)
    //      35. NorthRoof4 (roof)
    //      36. EastRoof (roof)
    //      37. WestRoof (roof)
    //      38.   AtticSkylight (window)
    //      39.   TubularDaylightingDome1 (not a window)

    // Simulation Order (1-based):
    //  SHADING SURFACES:
    int siteShadeShadeFlatShadeSurface =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SiteShade:FlatShadeSurface"), state->dataSurface->Surface);
    int mirSiteShadeFlatShadeSurface =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("SiteShade:FlatShadeSurface"), state->dataSurface->Surface);
    int buildingShadeTiltedShadeSurface =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("BuildingShade:TiltedShadeSurface"), state->dataSurface->Surface);
    int mirBuildingShadeTiltedShadeSurface =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("BuildingShade:TiltedShadeSurface"), state->dataSurface->Surface);
    int zoneShadeLivingSouthShade001 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("ZoneShade:Living:South:Shade001"), state->dataSurface->Surface);
    int mirZoneShadeLivingSouthShade001 =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("ZoneShade:Living:South:Shade001"), state->dataSurface->Surface);
    EXPECT_EQ(siteShadeShadeFlatShadeSurface, 1);
    EXPECT_EQ(mirSiteShadeFlatShadeSurface, 2);
    EXPECT_EQ(buildingShadeTiltedShadeSurface, 3);
    EXPECT_EQ(mirBuildingShadeTiltedShadeSurface, 4);
    EXPECT_EQ(zoneShadeLivingSouthShade001, 5);
    EXPECT_EQ(mirZoneShadeLivingSouthShade001, 6);

    //  LIVING ZONE:
    int wallLivingNorth = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:North"), state->dataSurface->Surface);
    int wallLivingEast = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:East"), state->dataSurface->Surface);
    int wallLivingSouth = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:South"), state->dataSurface->Surface);
    int wallLivingWest = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:West"), state->dataSurface->Surface);
    int wallLivingInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Interior"), state->dataSurface->Surface);
    int floorLivingFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Floor"), state->dataSurface->Surface);
    int ceilingLivingCeiling = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Ceiling"), state->dataSurface->Surface);
    int doorWestDoor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestDoor"), state->dataSurface->Surface);
    int windowTubularDaylightingDiffuser1 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("TubularDaylightingDiffuser1"), state->dataSurface->Surface);
    int windowNorthWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthWindow"), state->dataSurface->Surface);
    int windowEastWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastWindow"), state->dataSurface->Surface);
    int windowSouthWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SouthWindow"), state->dataSurface->Surface);
    int windowWestWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestWindow"), state->dataSurface->Surface);

    EXPECT_EQ(wallLivingNorth, 7);
    EXPECT_EQ(wallLivingEast, 8);
    EXPECT_EQ(wallLivingSouth, 9);
    EXPECT_EQ(wallLivingWest, 10);
    EXPECT_EQ(wallLivingInterior, 11);
    EXPECT_EQ(floorLivingFloor, 12);
    EXPECT_EQ(ceilingLivingCeiling, 13);
    EXPECT_EQ(doorWestDoor, 14);
    EXPECT_EQ(windowTubularDaylightingDiffuser1, 19);
    EXPECT_EQ(windowNorthWindow, 15);
    EXPECT_EQ(windowEastWindow, 16);
    EXPECT_EQ(windowSouthWindow, 17);
    EXPECT_EQ(windowWestWindow, 18);
    EXPECT_EQ(state->dataHeatBal->Zone(1).HTSurfaceFirst, 7);
    EXPECT_EQ(state->dataHeatBal->Zone(1).HTSurfaceLast, 19);
    EXPECT_EQ(state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceFirst, 7);
    EXPECT_EQ(state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceLast, 14);
    EXPECT_EQ(state->dataHeatBal->Zone(1).WindowSurfaceFirst, 15);
    EXPECT_EQ(state->dataHeatBal->Zone(1).WindowSurfaceLast, 19);

    //  GARAGE ZONE:
    int wallGarageInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Interior"), state->dataSurface->Surface);
    int wallGarageEast = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:EastWall"), state->dataSurface->Surface);
    int wallGarageWest = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:WestWall"), state->dataSurface->Surface);
    int wallGarageFrontDoor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:FrontDoor"), state->dataSurface->Surface);
    int floorGarageFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Floor"), state->dataSurface->Surface);
    int ceilingGarageInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Ceiling"), state->dataSurface->Surface);
    int intmassEVChargingStation = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EVChargingStation"), state->dataSurface->Surface);

    EXPECT_EQ(wallGarageInterior, 20);
    EXPECT_EQ(wallGarageEast, 21);
    EXPECT_EQ(wallGarageWest, 22);
    EXPECT_EQ(wallGarageFrontDoor, 23);
    EXPECT_EQ(floorGarageFloor, 24);
    EXPECT_EQ(ceilingGarageInterior, 25);
    EXPECT_EQ(intmassEVChargingStation, 26);
    EXPECT_EQ(state->dataHeatBal->Zone(2).HTSurfaceFirst, 20);
    EXPECT_EQ(state->dataHeatBal->Zone(2).HTSurfaceLast, 26);
    EXPECT_EQ(state->dataHeatBal->Zone(2).OpaqOrIntMassSurfaceFirst, 20);
    EXPECT_EQ(state->dataHeatBal->Zone(2).OpaqOrIntMassSurfaceLast, 26);
    EXPECT_EQ(state->dataHeatBal->Zone(2).WindowSurfaceFirst, 0);
    EXPECT_EQ(state->dataHeatBal->Zone(2).WindowSurfaceLast, -1);

    //  ATTIC ZONE:
    int wallEastGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastGable"), state->dataSurface->Surface);
    int wallWestGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestGable"), state->dataSurface->Surface);
    int wallNorthGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthGable"), state->dataSurface->Surface);
    int floorAtticLivingFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Attic:LivingFloor"), state->dataSurface->Surface);
    int floorAtticGarageFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Attic:GarageFloor"), state->dataSurface->Surface);
    int roofNorthRoof1 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof1"), state->dataSurface->Surface);
    int roofSouthRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SouthRoof"), state->dataSurface->Surface);
    int roofNorthRoof2 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof2"), state->dataSurface->Surface);
    int roofNorthRoof3 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof3"), state->dataSurface->Surface);
    int roofNorthRoof4 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof4"), state->dataSurface->Surface);
    int roofEastRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastRoof"), state->dataSurface->Surface);
    int roofWestRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestRoof"), state->dataSurface->Surface);
    int nonwindowTubularDaylightingDome1 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("TubularDaylightingDome1"), state->dataSurface->Surface);
    int windowAtticSkylight = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("AtticSkylight"), state->dataSurface->Surface);

    EXPECT_EQ(wallEastGable, 27);
    EXPECT_EQ(wallWestGable, 28);
    EXPECT_EQ(wallNorthGable, 29);
    EXPECT_EQ(floorAtticLivingFloor, 30);
    EXPECT_EQ(floorAtticGarageFloor, 31);
    EXPECT_EQ(roofNorthRoof1, 32);
    EXPECT_EQ(roofSouthRoof, 33);
    EXPECT_EQ(roofNorthRoof2, 34);
    EXPECT_EQ(roofNorthRoof3, 35);
    EXPECT_EQ(roofNorthRoof4, 36);
    EXPECT_EQ(roofEastRoof, 37);
    EXPECT_EQ(roofWestRoof, 38);
    EXPECT_EQ(nonwindowTubularDaylightingDome1, 40);
    EXPECT_EQ(windowAtticSkylight, 39);
    EXPECT_EQ(state->dataHeatBal->Zone(3).HTSurfaceFirst, 27);
    EXPECT_EQ(state->dataHeatBal->Zone(3).HTSurfaceLast, 40);
    EXPECT_EQ(state->dataHeatBal->Zone(3).OpaqOrIntMassSurfaceFirst, 27);
    EXPECT_EQ(state->dataHeatBal->Zone(3).OpaqOrIntMassSurfaceLast, 38);
    EXPECT_EQ(state->dataHeatBal->Zone(3).WindowSurfaceFirst, 39);
    EXPECT_EQ(state->dataHeatBal->Zone(3).WindowSurfaceLast, 39);
    EXPECT_EQ(state->dataHeatBal->Zone(3).OpaqOrWinSurfaceLast, 39);
    EXPECT_EQ(state->dataHeatBal->Zone(3).TDDDomeFirst, 40);
    EXPECT_EQ(state->dataHeatBal->Zone(3).TDDDomeLast, 40);

    // Reporting (legacy) Order (zero-based)
    //  SHADING SURFACES:
    EXPECT_EQ(siteShadeShadeFlatShadeSurface, state->dataSurface->AllSurfaceListReportOrder[0]);
    EXPECT_EQ(mirSiteShadeFlatShadeSurface, state->dataSurface->AllSurfaceListReportOrder[1]);
    EXPECT_EQ(buildingShadeTiltedShadeSurface, state->dataSurface->AllSurfaceListReportOrder[2]);
    EXPECT_EQ(mirBuildingShadeTiltedShadeSurface, state->dataSurface->AllSurfaceListReportOrder[3]);
    EXPECT_EQ(zoneShadeLivingSouthShade001, state->dataSurface->AllSurfaceListReportOrder[4]);
    EXPECT_EQ(mirZoneShadeLivingSouthShade001, state->dataSurface->AllSurfaceListReportOrder[5]);

    //  LIVING ZONE:
    EXPECT_EQ(wallLivingNorth, state->dataSurface->AllSurfaceListReportOrder[6]);
    EXPECT_EQ(windowNorthWindow, state->dataSurface->AllSurfaceListReportOrder[7]);
    EXPECT_EQ(wallLivingEast, state->dataSurface->AllSurfaceListReportOrder[8]);
    EXPECT_EQ(windowEastWindow, state->dataSurface->AllSurfaceListReportOrder[9]);
    EXPECT_EQ(wallLivingSouth, state->dataSurface->AllSurfaceListReportOrder[10]);
    EXPECT_EQ(windowSouthWindow, state->dataSurface->AllSurfaceListReportOrder[11]);
    EXPECT_EQ(wallLivingWest, state->dataSurface->AllSurfaceListReportOrder[12]);
    EXPECT_EQ(windowWestWindow, state->dataSurface->AllSurfaceListReportOrder[13]);
    EXPECT_EQ(doorWestDoor, state->dataSurface->AllSurfaceListReportOrder[14]);
    EXPECT_EQ(wallLivingInterior, state->dataSurface->AllSurfaceListReportOrder[15]);
    EXPECT_EQ(floorLivingFloor, state->dataSurface->AllSurfaceListReportOrder[16]);
    EXPECT_EQ(ceilingLivingCeiling, state->dataSurface->AllSurfaceListReportOrder[17]);
    EXPECT_EQ(windowTubularDaylightingDiffuser1, state->dataSurface->AllSurfaceListReportOrder[18]);

    //  GARAGE ZONE:
    EXPECT_EQ(wallGarageInterior, state->dataSurface->AllSurfaceListReportOrder[19]);
    EXPECT_EQ(wallGarageEast, state->dataSurface->AllSurfaceListReportOrder[20]);
    EXPECT_EQ(wallGarageWest, state->dataSurface->AllSurfaceListReportOrder[21]);
    EXPECT_EQ(wallGarageFrontDoor, state->dataSurface->AllSurfaceListReportOrder[22]);
    EXPECT_EQ(floorGarageFloor, state->dataSurface->AllSurfaceListReportOrder[23]);
    EXPECT_EQ(ceilingGarageInterior, state->dataSurface->AllSurfaceListReportOrder[24]);
    EXPECT_EQ(intmassEVChargingStation, state->dataSurface->AllSurfaceListReportOrder[25]);

    //  ATTIC ZONE:
    EXPECT_EQ(wallEastGable, state->dataSurface->AllSurfaceListReportOrder[26]);
    EXPECT_EQ(wallWestGable, state->dataSurface->AllSurfaceListReportOrder[27]);
    EXPECT_EQ(wallNorthGable, state->dataSurface->AllSurfaceListReportOrder[28]);
    EXPECT_EQ(floorAtticLivingFloor, state->dataSurface->AllSurfaceListReportOrder[29]);
    EXPECT_EQ(floorAtticGarageFloor, state->dataSurface->AllSurfaceListReportOrder[30]);
    EXPECT_EQ(roofNorthRoof1, state->dataSurface->AllSurfaceListReportOrder[31]);
    EXPECT_EQ(nonwindowTubularDaylightingDome1, state->dataSurface->AllSurfaceListReportOrder[32]);
    EXPECT_EQ(roofSouthRoof, state->dataSurface->AllSurfaceListReportOrder[33]);
    EXPECT_EQ(roofNorthRoof2, state->dataSurface->AllSurfaceListReportOrder[34]);
    EXPECT_EQ(roofNorthRoof3, state->dataSurface->AllSurfaceListReportOrder[35]);
    EXPECT_EQ(roofNorthRoof4, state->dataSurface->AllSurfaceListReportOrder[36]);
    EXPECT_EQ(roofEastRoof, state->dataSurface->AllSurfaceListReportOrder[37]);
    EXPECT_EQ(windowAtticSkylight, state->dataSurface->AllSurfaceListReportOrder[38]);
    EXPECT_EQ(roofWestRoof, state->dataSurface->AllSurfaceListReportOrder[39]);
}

TEST_F(EnergyPlusFixture, Use_Gross_Roof_Area_for_Averge_Height)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "Material:NoMass,",
        "R13LAYER,                !- Name",
        "Rough,                   !- Roughness",
        "2.290965,                !- Thermal Resistance {m2-K/W}",
        "0.9000000,               !- Thermal Absorptance",
        "0.7500000,               !- Solar Absorptance",
        "0.7500000;               !- Visible Absorptance",

        "Material:NoMass,",
        "R31LAYER,                !- Name",
        "Rough,                   !- Roughness",
        "5.456,                   !- Thermal Resistance {m2-K/W}",
        "0.9000000,               !- Thermal Absorptance",
        "0.7500000,               !- Solar Absorptance",
        "0.7500000;               !-Visible Absorptance",

        "Material,",
        "C5 - 4 IN HW CONCRETE,   !- Name",
        "MediumRough,             !- Roughness",
        "0.1014984,               !- Thickness {m}",
        "1.729577,                !- Conductivity {W/m-K}",
        "2242.585,                !- Density {kg/m3}",
        "836.8000,                !- Specific Heat {J/kg-K}",
        "0.9000000,               !- Thermal Absorptance",
        "0.6500000,               !- Solar Absorptance",
        "0.6500000;               !- Visible Absorptance",

        "Construction,",
        "R13WALL,                 !- Name",
        "R13LAYER;                !- Outside Layer",

        "Construction,",
        "FLOOR,                   !- Name",
        "C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "Construction,",
        "ROOF31,                  !- Name",
        "R31LAYER;                !- Outside Layer",

        "BuildingSurface:Detailed,",
        "    00_Floor,                !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000, 0, 0.0,                   !- X,Y,Z  1 {m}",
        "    0, 4, 0.0,                          !- X,Y,Z  2 {m}",
        "    8, 4, 0,                            !- X,Y,Z  3 {m}",
        "    8, 0, 0;                            !- X,Y,Z  4 {m}",

        "    BuildingSurface:Detailed,",
        "    01Wall_E,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    8, 0, 2.5,                          !- X,Y,Z  1 {m}",
        "    8, 0, 0,                            !- X,Y,Z  2 {m}",
        "    8, 4, 0,                            !- X,Y,Z  3 {m}",
        "    8, 4, 2.5;                          !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "02Wall_S, !-Name",
        "Wall, !-Surface Type",
        "R13WALL, !-Construction Name",
        "ZONE ONE, !-Zone Name",
        ",         !- Space Name",
        "Outdoors, !-Outside Boundary Condition",
        ", !-Outside Boundary Condition Object",
        "SunExposed, !-Sun Exposure",
        "WindExposed, !-Wind Exposure",
        "0.5000000, !-View Factor to Ground",
        "6, !-Number of Vertices",
        "0, 0, 2.5, !-X, Y, Z 1{m}",
        "0, 0, 0, !-X, Y, Z 2{m}",
        "8, 0, 0, !-X, Y, Z 3{m}",
        "8, 0, 2.5, !-X, Y, Z 4{m}",
        "6, 0, 4, !-X, Y, Z 5{m}",
        "2, 0, 4; !-X, Y, Z 6{m}",

        "BuildingSurface:Detailed,",
        "    03Wall_W,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0, 4, 2.5,                          !- X,Y,Z  1 {m}",
        "    0, 4, 0,                            !- X,Y,Z  2 {m}",
        "    0, 0, 0,                            !- X,Y,Z  3 {m}",
        "    0, 0, 2.5;                          !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    04Wall_N,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    6,                       !- Number of Vertices",
        "    8, 4, 2.5,                          !- X,Y,Z  1 {m}",
        "    8, 4, 0,                            !- X,Y,Z  2 {m}",
        "    0, 4, 0,                            !- X,Y,Z  3 {m}",
        "    0, 4, 2.5,                          !- X,Y,Z  4 {m}",
        "    2, 4, 4,                         !- X,Y,Z  5 {m}",
        "    6, 4, 4;                         !- X,Y,Z  6 {m}",

        "BuildingSurface:Detailed,",
        "    05Roof_C,                !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    2, 4, 4,                         !- X,Y,Z  1 {m}",
        "    2, 0, 4,                         !- X,Y,Z  2 {m}",
        "    6, 0, 4,                         !- X,Y,Z  3 {m}",
        "    6, 4, 4;                         !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "06Roof_E,                !- Name",
        "Roof,                    !- Surface Type",
        "R13WALL,                 !- Construction Name",
        "ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "Outdoors,                !- Outside Boundary Condition",
        ",                        !- Outside Boundary Condition Object",
        "SunExposed,              !- Sun Exposure",
        "WindExposed,             !- Wind Exposure",
        "autocalculate,           !- View Factor to Ground",
        "4,                       !- Number of Vertices",
        "6, 0, 4,                         !- X,Y,Z  1 {m}",
        "8, 0, 2.5,                          !- X,Y,Z  2 {m}",
        "8, 4, 2.5,                          !- X,Y,Z  3 {m}",
        "6, 4, 4;                         !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "07Roof_W,                !- Name",
        "Roof,                    !- Surface Type",
        "R13WALL,                 !- Construction Name",
        "ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "Outdoors,                !- Outside Boundary Condition",
        ",                        !- Outside Boundary Condition Object",
        "SunExposed,              !- Sun Exposure",
        "WindExposed,             !- Wind Exposure",
        "autocalculate,           !- View Factor to Ground",
        "4,                       !- Number of Vertices",
        "2, 4, 4,                         !- X,Y,Z  1 {m}",
        "0, 4, 2.5,                          !- X,Y,Z  2 {m}",
        "0, 0, 2.5,                          !- X,Y,Z  3 {m}",
        "2, 0, 4;                         !- X,Y,Z  4 {m}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetSurfaceData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Center
    EXPECT_NEAR(state->dataSurface->Surface(6).GrossArea, 16.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(6).Area, 16.0, 1e-6);

    // East
    EXPECT_NEAR(state->dataSurface->Surface(7).GrossArea, 10.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(7).Area, 10.0, 1e-6);

    // West
    EXPECT_NEAR(state->dataSurface->Surface(8).GrossArea, 10.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(8).Area, 10.0, 1e-6);

    Real64 totalGrossCeilingArea = 0.0;
    totalGrossCeilingArea =
        state->dataSurface->Surface(6).GrossArea + state->dataSurface->Surface(7).GrossArea + state->dataSurface->Surface(8).GrossArea;

    Real64 totalNetCeilingArea = 0.0;
    totalNetCeilingArea = state->dataSurface->Surface(6).Area + state->dataSurface->Surface(7).Area + state->dataSurface->Surface(8).Area;

    Real64 ceilingHeight_expected = 0.0;
    ceilingHeight_expected = 3.25 * (state->dataSurface->Surface(7).GrossArea + state->dataSurface->Surface(8).GrossArea) / totalGrossCeilingArea +
                             4.0 * state->dataSurface->Surface(6).GrossArea / totalGrossCeilingArea;

    EXPECT_NEAR(state->dataHeatBal->Zone(1).CeilingHeight, ceilingHeight_expected, 1e-6);
}

TEST_F(EnergyPlusFixture, Use_Gross_Roof_Area_for_Averge_Height_with_Window)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "Material:NoMass,",
        "R13LAYER,                !- Name",
        "Rough,                   !- Roughness",
        "2.290965,                !- Thermal Resistance {m2-K/W}",
        "0.9000000,               !- Thermal Absorptance",
        "0.7500000,               !- Solar Absorptance",
        "0.7500000;               !- Visible Absorptance",

        "Material:NoMass,",
        "R31LAYER,                !- Name",
        "Rough,                   !- Roughness",
        "5.456,                   !- Thermal Resistance {m2-K/W}",
        "0.9000000,               !- Thermal Absorptance",
        "0.7500000,               !- Solar Absorptance",
        "0.7500000;               !-Visible Absorptance",

        "Material,",
        "C5 - 4 IN HW CONCRETE,   !- Name",
        "MediumRough,             !- Roughness",
        "0.1014984,               !- Thickness {m}",
        "1.729577,                !- Conductivity {W/m-K}",
        "2242.585,                !- Density {kg/m3}",
        "836.8000,                !- Specific Heat {J/kg-K}",
        "0.9000000,               !- Thermal Absorptance",
        "0.6500000,               !- Solar Absorptance",
        "0.6500000;               !- Visible Absorptance",

        "WindowMaterial:Glazing,",
        "    CLEAR 3MM,               !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.837,                   !- Solar Transmittance at Normal Incidence",
        "    0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.898,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Glazing,",
        "    GREY 3MM,                !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.626,                   !- Solar Transmittance at Normal Incidence",
        "    0.061,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.061,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.611,                   !- Visible Transmittance at Normal Incidence",
        "    0.061,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.061,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Gas,",
        "    AIR 13MM,                !- Name",
        "    Air,                     !- Gas Type",
        "    0.0127;                  !- Thickness {m}",

        "Construction,",
        "R13WALL,                 !- Name",
        "R13LAYER;                !- Outside Layer",

        "Construction,",
        "FLOOR,                   !- Name",
        "C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "Construction,",
        "ROOF31,                  !- Name",
        "R31LAYER;                !- Outside Layer",

        "Construction,",
        "    Dbl Clr 3mm/13mm Air,    !- Name",
        "    CLEAR 3MM,               !- Outside Layer",
        "    AIR 13MM,                !- Layer 2",
        "    CLEAR 3MM;               !- Layer 3",

        "BuildingSurface:Detailed,",
        "    00_Floor,                !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000, 0, 0.0,                   !- X,Y,Z  1 {m}",
        "    0, 4, 0.0,                          !- X,Y,Z  2 {m}",
        "    8, 4, 0,                            !- X,Y,Z  3 {m}",
        "    8, 0, 0;                            !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    01Wall_E,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    8, 0, 2.5,                          !- X,Y,Z  1 {m}",
        "    8, 0, 0,                            !- X,Y,Z  2 {m}",
        "    8, 4, 0,                            !- X,Y,Z  3 {m}",
        "    8, 4, 2.5;                          !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "02Wall_S, !-Name",
        "Wall, !-Surface Type",
        "R13WALL, !-Construction Name",
        "ZONE ONE, !-Zone Name",
        ",         !- Space Name",
        "Outdoors, !-Outside Boundary Condition",
        ", !-Outside Boundary Condition Object",
        "SunExposed, !-Sun Exposure",
        "WindExposed, !-Wind Exposure",
        "0.5000000, !-View Factor to Ground",
        "6, !-Number of Vertices",
        "0, 0, 2.5, !-X, Y, Z 1{m}",
        "0, 0, 0, !-X, Y, Z 2{m}",
        "8, 0, 0, !-X, Y, Z 3{m}",
        "8, 0, 2.5, !-X, Y, Z 4{m}",
        "6, 0, 4, !-X, Y, Z 5{m}",
        "2, 0, 4; !-X, Y, Z 6{m}",

        "BuildingSurface:Detailed,",
        "    03Wall_W,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0, 4, 2.5,                          !- X,Y,Z  1 {m}",
        "    0, 4, 0,                            !- X,Y,Z  2 {m}",
        "    0, 0, 0,                            !- X,Y,Z  3 {m}",
        "    0, 0, 2.5;                          !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    04Wall_N,                !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    6,                       !- Number of Vertices",
        "    8, 4, 2.5,                          !- X,Y,Z  1 {m}",
        "    8, 4, 0,                            !- X,Y,Z  2 {m}",
        "    0, 4, 0,                            !- X,Y,Z  3 {m}",
        "    0, 4, 2.5,                          !- X,Y,Z  4 {m}",
        "    2, 4, 4,                         !- X,Y,Z  5 {m}",
        "    6, 4, 4;                         !- X,Y,Z  6 {m}",

        "BuildingSurface:Detailed,",
        "    05Roof_C,                !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    2, 4, 4,                         !- X,Y,Z  1 {m}",
        "    2, 0, 4,                         !- X,Y,Z  2 {m}",
        "    6, 0, 4,                         !- X,Y,Z  3 {m}",
        "    6, 4, 4;                         !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "06Roof_E,                !- Name",
        "Roof,                    !- Surface Type",
        "R13WALL,                 !- Construction Name",
        "ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "Outdoors,                !- Outside Boundary Condition",
        ",                        !- Outside Boundary Condition Object",
        "SunExposed,              !- Sun Exposure",
        "WindExposed,             !- Wind Exposure",
        "autocalculate,           !- View Factor to Ground",
        "4,                       !- Number of Vertices",
        "6, 0, 4,                         !- X,Y,Z  1 {m}",
        "8, 0, 2.5,                          !- X,Y,Z  2 {m}",
        "8, 4, 2.5,                          !- X,Y,Z  3 {m}",
        "6, 4, 4;                         !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "07Roof_W,                !- Name",
        "Roof,                    !- Surface Type",
        "R13WALL,                 !- Construction Name",
        "ZONE ONE,                !- Zone Name",
        "    ,                        !- Space Name",
        "Outdoors,                !- Outside Boundary Condition",
        ",                        !- Outside Boundary Condition Object",
        "SunExposed,              !- Sun Exposure",
        "WindExposed,             !- Wind Exposure",
        "autocalculate,           !- View Factor to Ground",
        "4,                       !- Number of Vertices",
        "2, 4, 4,                         !- X,Y,Z  1 {m}",
        "0, 4, 2.5,                          !- X,Y,Z  2 {m}",
        "0, 0, 2.5,                          !- X,Y,Z  3 {m}",
        "2, 0, 4;                         !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    Skylight_C,              !- Name",
        "    WINDOW,                  !- Surface Type",
        "    Dbl Clr 3mm/13mm Air,    !- Construction Name",
        "    05Roof_C,                !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    autocalculate,           !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.2, 0.2, 4,                         !- X,Y,Z  1 {m}",
        "    5.8, 0.2, 4,                         !- X,Y,Z  2 {m}",
        "    5.8, 3.8, 4,                         !- X,Y,Z  3 {m}",
        "    2.2, 3.8, 4;                         !- X,Y,Z  4 {m}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetSurfaceData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Center
    EXPECT_NEAR(state->dataSurface->Surface(6).GrossArea, 16.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(6).Area, 3.04, 1e-6);

    // East
    EXPECT_NEAR(state->dataSurface->Surface(7).GrossArea, 10.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(7).Area, 10.0, 1e-6);

    // West
    EXPECT_NEAR(state->dataSurface->Surface(8).GrossArea, 10.0, 1e-6);
    EXPECT_NEAR(state->dataSurface->Surface(8).Area, 10.0, 1e-6);

    Real64 totalGrossCeilingArea = 0.0;
    totalGrossCeilingArea =
        state->dataSurface->Surface(6).GrossArea + state->dataSurface->Surface(7).GrossArea + state->dataSurface->Surface(8).GrossArea;

    Real64 totalNetCeilingArea = 0.0;
    totalNetCeilingArea = state->dataSurface->Surface(6).Area + state->dataSurface->Surface(7).Area + state->dataSurface->Surface(8).Area;

    Real64 ceilingHeight_expected = 0.0;
    ceilingHeight_expected = 3.25 * (state->dataSurface->Surface(7).GrossArea + state->dataSurface->Surface(8).GrossArea) / totalGrossCeilingArea +
                             4.0 * state->dataSurface->Surface(6).GrossArea / totalGrossCeilingArea;

    Real64 ceilingHeight_old = 0.0;
    ceilingHeight_old = 3.25 * (state->dataSurface->Surface(7).Area + state->dataSurface->Surface(8).Area) / totalNetCeilingArea +
                        4.0 * state->dataSurface->Surface(6).Area / totalNetCeilingArea;

    EXPECT_NEAR(state->dataHeatBal->Zone(1).CeilingHeight, ceilingHeight_expected, 1e-6);
    EXPECT_NE(state->dataHeatBal->Zone(1).CeilingHeight, ceilingHeight_old);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_GetKivaFoundationTest)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Foundation:Kiva:Settings,",
        "  1.8,                     !- Soil Conductivity {W / m - K}",
        "  3200,                    !- Soil Density {kg / m3}",
        "  836,                     !- Soil Specific Heat {J / kg - K}",
        "  0.9,                     !- Ground Solar Absorptivity {dimensionless}",
        "  0.9,                     !- Ground Thermal Absorptivity {dimensionless}",
        "  0.03,                    !- Ground Surface Roughness {m}",
        "  40,                      !- Far - Field Width {m}",
        "  ZeroFlux,                !- Deep - Ground Boundary Condition",
        "  AutoCalculate;           !- Deep - Ground Depth",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetFoundationData(*state, ErrorsFound);
    std::string const error_string = delimited_string({
        "   ** Severe  ** Foundation:Kiva:Settings, Deep-Ground Depth should not be set to Autocalculate unless Deep-Ground Boundary Condition is "
        "set to Autoselect",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_GetKivaFoundationTest2)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Foundation:Kiva:Settings,",
        "  1.8,                     !- Soil Conductivity {W / m - K}",
        "  3200,                    !- Soil Density {kg / m3}",
        "  836,                     !- Soil Specific Heat {J / kg - K}",
        "  0.9,                     !- Ground Solar Absorptivity {dimensionless}",
        "  0.9,                     !- Ground Thermal Absorptivity {dimensionless}",
        "  0.03,                    !- Ground Surface Roughness {m}",
        "  40,                      !- Far-Field Width {m}",
        "  Autoselect,              !- Deep-Ground Boundary Condition",
        "  20;                      !- Deep-Ground Depth",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->Elevation = 600.;

    GetFoundationData(*state, ErrorsFound);
    std::string const error_string =
        delimited_string({"   ** Warning ** Foundation:Kiva:Settings, when Deep-Ground Boundary Condition is Autoselect,\n"
                          "   **   ~~~   ** the user-specified Deep-Ground Depth (20.0 m)\n"
                          "   **   ~~~   ** will be overridden with the Autoselected depth (40.0 m)"});
    EXPECT_TRUE(compare_err_stream(error_string, true));
}
TEST_F(EnergyPlusFixture, SurfaceGeometry_ZoneAndSpaceAreas)
{

    // Test for issue #9302 and beyond - User input Zone Floor Area not used for Space Floor Area

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1,             !- Name",
        "    90,                      !- Direction of Relative North {deg}",
        "    16.12975,                !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    ,                        !- Type",
        "    1,                       !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    ,                        !- Volume {m3}",
        "    30.0;                     !- Floor Area {m2}",

        "Zone,",
        "Zone 2,             !- Name",
        "    90,                      !- Direction of Relative North {deg}",
        "    16.12975,                !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    ,                        !- Type",
        "    1,                       !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    ,                        !- Volume {m3}",
        "    20.0;                     !- Floor Area {m2}",

        "Zone,",
        "Zone 3;             !- Name",

        "Space,",
        "Space 1a,            !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 1b,            !- Name",
        "Zone 1;             !- Zone Name",

        "Space,",
        "Space 3,            !- Name",
        "Zone 3,             !- Zone Name",
        "5.0;                !- Floor Area {m2}",

        "Material,",
        "    Some Material,         !- Name",
        "    VeryRough,               !- Roughness",
        "    0.006,                   !- Thickness {m}",
        "    0.815,                   !- Conductivity {W/m-K}",
        "    929,                     !- Density {kg/m3}",
        "    3140,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    0.7;                     !- Visible Absorptance",
        "Construction,",
        "    Some Construction,  !- Name",
        "    Some Material;        !- Outside Layer",
        "Construction:AirBoundary,",
        "Grouped Air Boundary, !- Name",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1a,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    SunExposed,                   !- Sun Exposure",
        "    WindExposed,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone1-Floor2,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
        "    Space 1b,                 !- Space Name",
        "    Outdoors,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    SunExposed,                   !- Sun Exposure",
        "    WindExposed,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,2,0,              !- Vertex 2",
        "    1,2,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone2-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
        "    ,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

        "BuildingSurface:Detailed,",
        "    Zone3-Floor,  !- Name",
        "    Floor,                 !- Surface Type",
        "    Some Construction,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
        "    Space 3,                 !- Space Name",
        "    Ground,                 !- Outside Boundary Condition",
        "    ,  !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    ,                        !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,              !- Vertex 1",
        "    0,1,0,              !- Vertex 2",
        "    1,1,0,              !- Vertex 3",
        "    1,0,0;              !- Vertex 4",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(*state, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);            // expect no errors

    GetConstructData(*state, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);             // expect no errors

    GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);        // expect no errors

    SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors

    // Zone 1 with user-entered floor area of 30.0 consists of Space 1 and Space 2
    // Space 1 has a floor surface of area 1.0, user-entered floor area is blank
    // Space 2 has a floor surface of area 2.0, user-entered floor area is blank
    EXPECT_EQ(state->dataHeatBal->space(1).Name, "SPACE 1A");
    EXPECT_NEAR(state->dataHeatBal->space(1).userEnteredFloorArea, DataGlobalConstants::AutoCalculate, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(1).calcFloorArea, 1.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(1).floorArea, 10.0, 0.001);

    EXPECT_EQ(state->dataHeatBal->space(2).Name, "SPACE 1B");
    EXPECT_NEAR(state->dataHeatBal->space(2).userEnteredFloorArea, DataGlobalConstants::AutoCalculate, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(2).calcFloorArea, 2.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(2).floorArea, 20.0, 0.001);

    EXPECT_EQ(state->dataHeatBal->Zone(1).Name, "ZONE 1");
    EXPECT_NEAR(state->dataHeatBal->Zone(1).UserEnteredFloorArea, 30.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(1).CalcFloorArea, 3.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(1).FloorArea, 30.0, 0.001);
    Real64 zone1Area = state->dataHeatBal->space(1).floorArea + state->dataHeatBal->space(2).floorArea;
    EXPECT_NEAR(state->dataHeatBal->Zone(1).FloorArea, zone1Area, 0.001);

    // Zone 3 consists of Space 3, user-entered floor area is blank
    // Space 3 has a floor surface of area 1.0, user-entered floor is 5.0
    EXPECT_EQ(state->dataHeatBal->Zone(3).Name, "ZONE 3");
    EXPECT_NEAR(state->dataHeatBal->Zone(3).UserEnteredFloorArea, DataGlobalConstants::AutoCalculate, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(3).CalcFloorArea, 5.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(3).FloorArea, 5.0, 0.001);
    EXPECT_EQ(state->dataHeatBal->space(3).Name, "SPACE 3");
    EXPECT_NEAR(state->dataHeatBal->space(3).userEnteredFloorArea, 5.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(3).calcFloorArea, 1.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(3).floorArea, 5.0, 0.001);

    // Zone 2 consists of auto-generated Space 4, user-entered floor area is 20.0
    // Space 4 has a floor surface of area 1.0, user-entered floor is blank
    EXPECT_EQ(state->dataHeatBal->Zone(2).Name, "ZONE 2");
    EXPECT_NEAR(state->dataHeatBal->Zone(2).UserEnteredFloorArea, 20.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(2).CalcFloorArea, 1.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->Zone(2).FloorArea, 20.0, 0.001);
    EXPECT_EQ(state->dataHeatBal->space(4).Name, "ZONE 2");
    EXPECT_NEAR(state->dataHeatBal->space(4).userEnteredFloorArea, DataGlobalConstants::AutoCalculate, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(4).calcFloorArea, 1.0, 0.001);
    EXPECT_NEAR(state->dataHeatBal->space(4).floorArea, 20.0, 0.001);
}
