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

// EnergyPlus::SurfaceGeometry Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::HeatBalanceManager;
// using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, BaseSurfaceRectangularTest)
{

    // Test base surfaces for rectangular shape in ProcessSurfaceVertices

    TotSurfaces = 5;
    MaxVerticesPerSurface = 5;
    Surface.allocate(TotSurfaces);
    ShadeV.allocate(TotSurfaces);
    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
        Surface(SurfNum).Vertex.allocate(MaxVerticesPerSurface);
    }

    bool ErrorsFound(false);
    int ThisSurf(0);

    // Surface 1 - Rectangle
    ThisSurf = 1;
    Surface(ThisSurf).Azimuth = 180.0;
    Surface(ThisSurf).Tilt = 90.0;
    Surface(ThisSurf).Sides = 4;
    Surface(ThisSurf).GrossArea = 10.0;

    Surface(ThisSurf).Vertex(1).x = 0.0;
    Surface(ThisSurf).Vertex(1).y = 0.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 5.0;
    Surface(ThisSurf).Vertex(2).y = 0.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 5.0;
    Surface(ThisSurf).Vertex(3).y = 0.0;
    Surface(ThisSurf).Vertex(3).z = 2.0;

    Surface(ThisSurf).Vertex(4).x = 0.0;
    Surface(ThisSurf).Vertex(4).y = 0.0;
    Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Rectangle, Surface(ThisSurf).Shape);

    // Surface 2 - Isosceles Trapezoid
    ThisSurf = 2;
    Surface(ThisSurf).Azimuth = 180.0;
    Surface(ThisSurf).Tilt = 90.0;
    Surface(ThisSurf).Sides = 4;
    Surface(ThisSurf).GrossArea = 8.0;

    Surface(ThisSurf).Vertex(1).x = 0.0;
    Surface(ThisSurf).Vertex(1).y = 0.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 5.0;
    Surface(ThisSurf).Vertex(2).y = 0.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 4.0;
    Surface(ThisSurf).Vertex(3).y = 0.0;
    Surface(ThisSurf).Vertex(3).z = 2.0;

    Surface(ThisSurf).Vertex(4).x = 1.0;
    Surface(ThisSurf).Vertex(4).y = 0.0;
    Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Quadrilateral, Surface(ThisSurf).Shape);

    // Surface 3 - Parallelogram
    ThisSurf = 3;
    Surface(ThisSurf).Azimuth = 180.0;
    Surface(ThisSurf).Tilt = 90.0;
    Surface(ThisSurf).Sides = 4;
    Surface(ThisSurf).GrossArea = 10.0;

    Surface(ThisSurf).Vertex(1).x = 0.0;
    Surface(ThisSurf).Vertex(1).y = 0.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 5.0;
    Surface(ThisSurf).Vertex(2).y = 0.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 7.0;
    Surface(ThisSurf).Vertex(3).y = 0.0;
    Surface(ThisSurf).Vertex(3).z = 2.0;

    Surface(ThisSurf).Vertex(4).x = 2.0;
    Surface(ThisSurf).Vertex(4).y = 0.0;
    Surface(ThisSurf).Vertex(4).z = 2.0;

    ProcessSurfaceVertices(ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Quadrilateral, Surface(ThisSurf).Shape);

    // Surface 4 - Triangle
    ThisSurf = 4;
    Surface(ThisSurf).Azimuth = 180.0;
    Surface(ThisSurf).Tilt = 90.0;
    Surface(ThisSurf).Sides = 3;
    Surface(ThisSurf).GrossArea = 10.0;

    Surface(ThisSurf).Vertex(1).x = 0.0;
    Surface(ThisSurf).Vertex(1).y = 0.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 5.0;
    Surface(ThisSurf).Vertex(2).y = 0.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 0.0;
    Surface(ThisSurf).Vertex(3).y = 0.0;
    Surface(ThisSurf).Vertex(3).z = 2.0;

    ProcessSurfaceVertices(ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Triangle, Surface(ThisSurf).Shape);

    // Surface 5 - Polygon
    ThisSurf = 5;
    Surface(ThisSurf).Azimuth = 180.0;
    Surface(ThisSurf).Tilt = 90.0;
    Surface(ThisSurf).Sides = 5;
    Surface(ThisSurf).GrossArea = 10.0;

    Surface(ThisSurf).Vertex(1).x = 0.0;
    Surface(ThisSurf).Vertex(1).y = 0.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 5.0;
    Surface(ThisSurf).Vertex(2).y = 0.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 7.0;
    Surface(ThisSurf).Vertex(3).y = 0.0;
    Surface(ThisSurf).Vertex(3).z = 2.0;

    Surface(ThisSurf).Vertex(4).x = 3.0;
    Surface(ThisSurf).Vertex(4).y = 0.0;
    Surface(ThisSurf).Vertex(4).z = 5.0;

    Surface(ThisSurf).Vertex(5).x = 1.0;
    Surface(ThisSurf).Vertex(5).y = 0.0;
    Surface(ThisSurf).Vertex(5).z = 3.0;

    ProcessSurfaceVertices(ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Polygonal, Surface(ThisSurf).Shape);
}

TEST_F(EnergyPlusFixture, DataSurfaces_SurfaceShape)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,",
        "    8.4;                     !- Version Identifier",

        " BuildingSurface:Detailed,",
        "    Surface 1 - Triangle,    !- Name",
        "    Floor,                   !- Surface Type",
        "    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
        "    Zone1,                   !- Zone Name",
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

    GetProjectControlData(ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);          // expect no errors

    GetMaterialData(ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    CosZoneRelNorth.allocate(1);
    SinZoneRelNorth.allocate(1);

    CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;

    GetSurfaceData(ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    // compare_err_stream( "" ); // just for debugging

    AllocateModuleArrays();

    //  Adding additional surfaces will change the index of the following based on where the surfaces are added in the array.
    //	If adding new tests, break here and look at EnergyPlus::DataSurfaces::Surface to see the order.

    //	enum surfaceShape:Triangle = 1
    //	Surface( 11 ).Name = "Surface 1 - Triangle"
    ProcessSurfaceVertices(11, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Triangle, Surface(11).Shape);

    //	enum surfaceShape:Quadrilateral = 2
    //	Surface( 12 ).Name = "Surface 2 - Quadrilateral"
    ProcessSurfaceVertices(12, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Quadrilateral, Surface(12).Shape);

    //	enum surfaceShape:Rectangle = 3
    //	Surface( 7 ).Name = "Surface 3 - Rectangle"
    ProcessSurfaceVertices(7, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Rectangle, Surface(7).Shape);

    //	enum surfaceShape:RectangularDoorWindow = 4
    //	Surface( 8 ).Name = "Surface 4 - RectangularDoorWindow"
    ProcessSurfaceVertices(8, ErrorsFound);
    EXPECT_EQ(SurfaceShape::RectangularDoorWindow, Surface(8).Shape);

    //	enum surfaceShape:RectangularOverhang = 5
    //	Surface( 1 ).Name = "Surface 5 - RectangularOverhang"
    ProcessSurfaceVertices(1, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularOverhang, Surface(1).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularLeftFin = 6
    //	Surface( 3 ).Name = "Surface 6 - RectangularLeftFin"
    ProcessSurfaceVertices(3, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularLeftFin, Surface(3).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularRightFin = 7
    //	Surface( 5 ).Name = "Surface 7 - RectangularRightFin"
    ProcessSurfaceVertices(5, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularRightFin, Surface(5).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:TriangularWindow = 8
    //	Surface( 9 ).Name = "Surface 8 - TriangularWindow"
    ProcessSurfaceVertices(9, ErrorsFound);
    EXPECT_EQ(SurfaceShape::TriangularWindow, Surface(9).Shape);

    //	enum surfaceShape:TriangularDoor = 9
    //	Surface( 10 ).Name = "Surface 9 - TriangularDoor"
    ProcessSurfaceVertices(10, ErrorsFound);
    EXPECT_EQ(SurfaceShape::TriangularDoor, Surface(10).Shape);

    //	enum surfaceShape:Polygonal = 10
    //	Surface( 13 ).Name = "Surface 10 - Polygonal"
    ProcessSurfaceVertices(13, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Polygonal, Surface(13).Shape);
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
    checkSubSurfAzTiltNorm(BaseSurface, SubSurface, surfaceError);
    EXPECT_FALSE(surfaceError);
    EXPECT_FALSE(has_err_output());

    // Case 2 - Base surface and subsurface face the opposite way - should be error message and surfaceError=true
    surfaceError = false;
    SubSurface.Azimuth = 180.;
    SubSurface.Tilt = 180.;
    SubSurface.NewellSurfaceNormalVector.x = 1.;
    SubSurface.NewellSurfaceNormalVector.y = 0.;
    SubSurface.NewellSurfaceNormalVector.z = 0.;
    checkSubSurfAzTiltNorm(BaseSurface, SubSurface, surfaceError);
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
    checkSubSurfAzTiltNorm(BaseSurface, SubSurface, surfaceError);
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
    checkSubSurfAzTiltNorm(BaseSurface, SubSurface, surfaceError);
    EXPECT_FALSE(surfaceError);
    EXPECT_TRUE(has_err_output());
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_MakeMirrorSurface)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "BuildingSurface:Detailed,",
        " FRONT-1,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
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
    GetMaterialData(FoundError);
    GetConstructData(FoundError);
    GetZoneData(FoundError); // Read Zone data from input file
    HeatTransferAlgosUsed.allocate(1);
    HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SetupZoneGeometry(FoundError); // this calls GetSurfaceData()

    EXPECT_FALSE(FoundError);

    // test coordinate on existing surface
    EXPECT_EQ(TotSurfaces, 1);

    EXPECT_EQ(Surface(TotSurfaces).Name, "FRONT-1");

    // move surface to SurfaceTmp since MakeMirrorSurface uses that array
    SurfaceTmp.allocate(10);
    SurfaceTmp(TotSurfaces) = Surface(TotSurfaces);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Name, "FRONT-1");

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).x, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).z, 2.4);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).x, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).z, 0.);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).x, 30.5);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).z, 0.);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).x, 30.5);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).z, 2.4);

    MakeMirrorSurface(TotSurfaces); // This call increments TotSurfaces so the references after this are for the created mirror surface

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Name, "Mir-FRONT-1");

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).x, 30.5);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(1).z, 2.4);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).x, 30.5);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(2).z, 0.);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).x, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(3).z, 0.);

    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).x, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).y, 0.);
    EXPECT_EQ(SurfaceTmp(TotSurfaces).Vertex(4).z, 2.4);
}

TEST_F(EnergyPlusFixture, SurfacesGeometry_CalcSurfaceCentroid_NonconvexRealisticZ)
{
    TotSurfaces = 10;
    Surface.allocate(TotSurfaces);

    Surface(1).Class = SurfaceClass_Roof;
    Surface(1).GrossArea = 1000.;
    Surface(1).Sides = 4;
    Surface(1).Vertex.allocate(4);

    Surface(1).Vertex(1).x = 2000.;
    Surface(1).Vertex(1).y = -1000.;
    Surface(1).Vertex(1).z = 10.;

    Surface(1).Vertex(2).x = 1.;
    Surface(1).Vertex(2).y = 0.;
    Surface(1).Vertex(2).z = 10.;

    Surface(1).Vertex(3).x = 2000.;
    Surface(1).Vertex(3).y = 1000.;
    Surface(1).Vertex(3).z = 10.;

    Surface(1).Vertex(4).x = 0.;
    Surface(1).Vertex(4).y = 0.;
    Surface(1).Vertex(4).z = 10.;

    CalcSurfaceCentroid();

    EXPECT_EQ(Surface(1).Centroid.x, 667.);
    EXPECT_EQ(Surface(1).Centroid.y, 0.);
    EXPECT_EQ(Surface(1).Centroid.z, 10.);
}

TEST_F(EnergyPlusFixture, MakeEquivalentRectangle)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,                                                        ",
        "	8.6;                     !- Version Identifier               ",
        "	                                                             ",
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
    GetMaterialData(ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);
    GetConstructData(ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);
    GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);
    GetProjectControlData(ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);
    CosZoneRelNorth.allocate(1);
    SinZoneRelNorth.allocate(1);
    CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;
    GetSurfaceData(ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    // Run the test
    for (int SurfNum = 2; SurfNum < 5; SurfNum++) {
        MakeEquivalentRectangle(SurfNum, ErrorsFound);
        EXPECT_FALSE(ErrorsFound); // expect no errors
    }

    // Check the result
    // (1) rectangle window
    EXPECT_NEAR(7.60, Surface(2).Width, 0.01);
    EXPECT_NEAR(1.20, Surface(2).Height, 0.01);
    // (2) trapzoid window
    EXPECT_NEAR(7.80, Surface(3).Width, 0.01);
    EXPECT_NEAR(1.17, Surface(3).Height, 0.01);
    // (3) parallelogram window
    EXPECT_NEAR(8.08, Surface(4).Width, 0.01);
    EXPECT_NEAR(1.13, Surface(4).Height, 0.01);
}

TEST(SurfaceGeometryUnitTests, distance)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, distance");

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

TEST(SurfaceGeometryUnitTests, isAlmostEqual3dPt)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, isAlmostEqual3dPt");

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

TEST(SurfaceGeometryUnitTests, isAlmostEqual2dPt)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, isAlmostEqual2dPt");

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

TEST(SurfaceGeometryUnitTests, isPointOnLineBetweenPoints)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, isPointOnLineBetweenPoints");

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

TEST(SurfaceGeometryUnitTests, findIndexOfVertex)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, findIndexOfVertex");

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

TEST(SurfaceGeometryUnitTests, listOfFacesFacingAzimuth_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, listOfFacesFacingAzimuth_test");

    DataVectorTypes::Polyhedron zonePoly;
    std::vector<int> results;

    Surface.allocate(9);
    Surface(1).Azimuth = 0;
    Surface(2).Azimuth = 30.;
    Surface(3).Azimuth = 30.;
    Surface(4).Azimuth = 30.;
    Surface(5).Azimuth = 45.;
    Surface(6).Azimuth = 45.;
    Surface(7).Azimuth = 72.;
    Surface(8).Azimuth = 72.5;
    Surface(9).Azimuth = 73.;

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

    results = listOfFacesFacingAzimuth(zonePoly, 90.);
    EXPECT_EQ(size_t(0), results.size());

    results = listOfFacesFacingAzimuth(zonePoly, 0.);
    EXPECT_EQ(size_t(1), results.size());
    EXPECT_EQ(1, results.at(0));

    results = listOfFacesFacingAzimuth(zonePoly, 30.);
    EXPECT_EQ(size_t(3), results.size());
    EXPECT_EQ(2, results.at(0));
    EXPECT_EQ(3, results.at(1));
    EXPECT_EQ(4, results.at(2));

    results = listOfFacesFacingAzimuth(zonePoly, 45.);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(5, results.at(0));
    EXPECT_EQ(6, results.at(1));

    results = listOfFacesFacingAzimuth(zonePoly, 71.9);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));

    results = listOfFacesFacingAzimuth(zonePoly, 72.0);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));

    results = listOfFacesFacingAzimuth(zonePoly, 72.1);
    EXPECT_EQ(size_t(3), results.size());
    EXPECT_EQ(7, results.at(0));
    EXPECT_EQ(8, results.at(1));
    EXPECT_EQ(9, results.at(2));

    results = listOfFacesFacingAzimuth(zonePoly, 73.0);
    EXPECT_EQ(size_t(2), results.size());
    EXPECT_EQ(8, results.at(0));
    EXPECT_EQ(9, results.at(1));
}

TEST(SurfaceGeometryUnitTests, areSurfaceHorizAndVert_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, areSurfaceHorizAndVert_test");

    DataVectorTypes::Polyhedron zonePoly;

    Surface.allocate(9);
    Surface(1).Class = SurfaceClass_Floor;
    Surface(1).Tilt = 180.;

    Surface(2).Class = SurfaceClass_Floor;
    Surface(2).Tilt = 179.5;

    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 89.1;

    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Tilt = 90.;

    Surface(5).Class = SurfaceClass_Wall;
    Surface(5).Tilt = 90.;

    Surface(6).Class = SurfaceClass_Wall;
    Surface(6).Tilt = 90.9;

    Surface(7).Class = SurfaceClass_Roof;
    Surface(7).Tilt = -0.9;

    Surface(8).Class = SurfaceClass_Roof;
    Surface(8).Tilt = 0.;

    Surface(9).Class = SurfaceClass_Roof;
    Surface(9).Tilt = 0.9;

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

    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(1).Tilt = 170.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(1).Tilt = 180.;
    Surface(2).Tilt = 178.9;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(2).Tilt = 181.0;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(2).Tilt = 181.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(2).Tilt = 179.5;
    Surface(8).Tilt = 180.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(8).Tilt = 1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(8).Tilt = -1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_TRUE(areWallsVertical);

    Surface(8).Tilt = 0.;
    Surface(4).Tilt = 270.;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    Surface(4).Tilt = 91.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    Surface(4).Tilt = 88.9;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_TRUE(isFloorHorizontal);
    EXPECT_TRUE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);

    Surface(1).Tilt = 170.;
    Surface(8).Tilt = 1.1;
    std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(zonePoly);
    EXPECT_FALSE(isFloorHorizontal);
    EXPECT_FALSE(isCeilingHorizontal);
    EXPECT_FALSE(areWallsVertical);
}

TEST(SurfaceGeometryUnitTests, areWallHeightSame_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, areWallHeightSame_test");

    DataVectorTypes::Polyhedron zonePoly;
    std::vector<int> results;

    Surface.allocate(3);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(2).Class = SurfaceClass_Wall;
    Surface(3).Class = SurfaceClass_Wall;

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

    EXPECT_TRUE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 9.;
    EXPECT_TRUE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 11.;
    EXPECT_FALSE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(2).z = 10.;
    zonePoly.SurfaceFace(2).FacePoints(2).z = 10.02;
    EXPECT_TRUE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(2).FacePoints(2).z = 10.03;
    EXPECT_FALSE(areWallHeightSame(zonePoly));

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

    EXPECT_TRUE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(1).z = -0.6;
    EXPECT_TRUE(areWallHeightSame(zonePoly));

    zonePoly.SurfaceFace(3).FacePoints(1).z = -0.4;
    EXPECT_FALSE(areWallHeightSame(zonePoly));
}

TEST(SurfaceGeometryUnitTests, findPossibleOppositeFace_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, findPossibleOppositeFace_test");

    DataVectorTypes::Polyhedron zonePoly;

    Surface.allocate(4);
    Surface(1).Azimuth = 0.;
    Surface(1).Area = 10.;

    Surface(2).Azimuth = 90.;
    Surface(2).Area = 10.;

    Surface(3).Azimuth = 180.;
    Surface(3).Area = 10.;

    Surface(4).Azimuth = 270.;
    Surface(4).Area = 10.;

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

    EXPECT_EQ(3, findPossibleOppositeFace(zonePoly, 1));
    EXPECT_EQ(1, findPossibleOppositeFace(zonePoly, 3));

    EXPECT_EQ(4, findPossibleOppositeFace(zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(zonePoly, 4));

    Surface(2).Azimuth = 90.5;

    EXPECT_EQ(4, findPossibleOppositeFace(zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(zonePoly, 4));

    Surface(2).Azimuth = 89.5;

    EXPECT_EQ(4, findPossibleOppositeFace(zonePoly, 2));
    EXPECT_EQ(2, findPossibleOppositeFace(zonePoly, 4));

    Surface(2).Azimuth = 45.;

    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 2)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 4)); // not found

    Surface(1).Area = 9.;

    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 1)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 3)); // not found

    Surface(1).Area = 10.;

    EXPECT_EQ(3, findPossibleOppositeFace(zonePoly, 1));
    EXPECT_EQ(1, findPossibleOppositeFace(zonePoly, 3));

    zonePoly.SurfaceFace(1).NSides = 3;

    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 1)); // not found
    EXPECT_EQ(-1, findPossibleOppositeFace(zonePoly, 3)); // not found
}

TEST(SurfaceGeometryUnitTests, areCornersEquidistant_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, areCornersEquidistant_test");

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

TEST(SurfaceGeometryUnitTests, areOppositeWallsSame_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, areOppositeWallsSame_test");

    DataVectorTypes::Polyhedron zonePoly;

    Surface.allocate(4);
    Surface(1).Azimuth = 0.;
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Area = 30.;

    Surface(2).Azimuth = 90.;
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Area = 24.;

    Surface(3).Azimuth = 180.;
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Area = 30.;

    Surface(4).Azimuth = 270.;
    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Area = 24.;

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

    EXPECT_TRUE(areOppositeWallsSame(zonePoly, area, dist));
    EXPECT_EQ(30., area);
    EXPECT_EQ(8., dist);

    Surface(3).Area = 29.; // make surface 1 and 3 no longer match areas - now compare 2 and 4
    EXPECT_TRUE(areOppositeWallsSame(zonePoly, area, dist));
    EXPECT_EQ(24., area);
    EXPECT_EQ(10., dist);

    Surface(4).Area = 23.; // make surface 2 and 4 no longer match areas
    EXPECT_FALSE(areOppositeWallsSame(zonePoly, area, dist));

    Surface(3).Area = 30.; // make surface 1 and 3 have same areas again
    Surface(4).Area = 24.; // make surface 2 and 4 have same areas again

    EXPECT_TRUE(areOppositeWallsSame(zonePoly, area, dist)); // retest

    zonePoly.SurfaceFace(3).FacePoints(3).y = 7.;            // move one corner in so distances are not all equal
    EXPECT_TRUE(areOppositeWallsSame(zonePoly, area, dist)); // should pick other walls
    EXPECT_EQ(24., area);
    EXPECT_EQ(10., dist);

    zonePoly.SurfaceFace(4).FacePoints(3).x = 11.;            // move one corner out so distances are not all equal
    EXPECT_FALSE(areOppositeWallsSame(zonePoly, area, dist)); // now neither wall matches
}

TEST(SurfaceGeometryUnitTests, areFloorAndCeilingSame_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, areFloorAndCeilingSame_test");

    DataVectorTypes::Polyhedron zonePoly;

    Surface.allocate(2);
    Surface(1).Class = SurfaceClass_Floor;

    Surface(2).Class = SurfaceClass_Roof;

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

    EXPECT_TRUE(areFloorAndCeilingSame(zonePoly));

    zonePoly.SurfaceFace(2).FacePoints(4).x = 7.; // move one corner

    EXPECT_FALSE(areFloorAndCeilingSame(zonePoly));
}

TEST(SurfaceGeometryUnitTests, makeListOfUniqueVertices_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, makeListOfUniqueVertices_test");

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

    std::vector<Vector> uniqueVertices;
    makeListOfUniqueVertices(zonePoly, uniqueVertices);

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

TEST(SurfaceGeometryUnitTests, numberOfEdgesNotTwoForEnclosedVolumeTest_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, numberOfEdgesNotTwoForEnclosedVolumeTest_test");

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

    std::vector<Vector> uniqueVertices;
    makeListOfUniqueVertices(zonePoly, uniqueVertices);

    EXPECT_EQ(size_t(8), uniqueVertices.size());

    std::vector<EdgeOfSurf> e1 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(0), e1.size());

    zonePoly.SurfaceFace(6).FacePoints(4).x = 0.;
    zonePoly.SurfaceFace(6).FacePoints(4).y = 0.;
    zonePoly.SurfaceFace(6).FacePoints(4).z = 0.;

    makeListOfUniqueVertices(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(8), uniqueVertices.size());

    std::vector<EdgeOfSurf> e2 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(4), e2.size());
}

TEST(SurfaceGeometryUnitTests, updateZonePolygonsForMissingColinearPoints_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, updateZonePolygonsForMissingColinearPoints_test");

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

    std::vector<Vector> uniqueVertices;
    makeListOfUniqueVertices(zonePoly, uniqueVertices);

    EXPECT_EQ(size_t(10), uniqueVertices.size());

    std::vector<EdgeOfSurf> e1 = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
    EXPECT_EQ(size_t(6), e1.size());

    DataVectorTypes::Polyhedron updatedZonePoly = updateZonePolygonsForMissingColinearPoints(
        zonePoly, uniqueVertices); // this is done after initial test since it is computationally intensive.

    std::vector<EdgeOfSurf> e2 = edgesNotTwoForEnclosedVolumeTest(updatedZonePoly, uniqueVertices);
    EXPECT_EQ(size_t(0), e2.size());
}

TEST(SurfaceGeometryUnitTests, insertVertexOnFace_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, insertVertexOnFace_test");

    DataVectorTypes::Face faceOfPoly;

    // insert in first position

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 1, Vector(99., 99., 99.));

    EXPECT_EQ(5, faceOfPoly.NSides);
    EXPECT_EQ(Vector(99., 99., 99.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(4));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(5));

    faceOfPoly.FacePoints.deallocate();

    // insert in second position

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 2, Vector(99., 99., 99.));

    EXPECT_EQ(5, faceOfPoly.NSides);
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(99., 99., 99.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(4));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(5));

    faceOfPoly.FacePoints.deallocate();

    // insert in third position

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 3, Vector(99., 99., 99.));

    EXPECT_EQ(5, faceOfPoly.NSides);
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(99., 99., 99.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(4));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(5));

    faceOfPoly.FacePoints.deallocate();

    // insert in fourth position

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 4, Vector(99., 99., 99.));

    EXPECT_EQ(5, faceOfPoly.NSides);
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(99., 99., 99.), faceOfPoly.FacePoints(4));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(5));

    faceOfPoly.FacePoints.deallocate();

    // insert in zero position (invalid)

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 0, Vector(99., 99., 99.));

    EXPECT_EQ(4, faceOfPoly.NSides);
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(4));

    faceOfPoly.FacePoints.deallocate();

    // insert in fifth position (invalid)

    faceOfPoly.NSides = 4;
    faceOfPoly.FacePoints.allocate(4);

    faceOfPoly.FacePoints(1) = Vector(1., 1., 1.);
    faceOfPoly.FacePoints(2) = Vector(2., 2., 2.);
    faceOfPoly.FacePoints(3) = Vector(3., 3., 3.);
    faceOfPoly.FacePoints(4) = Vector(4., 4., 4.);

    insertVertexOnFace(faceOfPoly, 5, Vector(99., 99., 99.));

    EXPECT_EQ(4, faceOfPoly.NSides);
    EXPECT_EQ(Vector(1., 1., 1.), faceOfPoly.FacePoints(1));
    EXPECT_EQ(Vector(2., 2., 2.), faceOfPoly.FacePoints(2));
    EXPECT_EQ(Vector(3., 3., 3.), faceOfPoly.FacePoints(3));
    EXPECT_EQ(Vector(4., 4., 4.), faceOfPoly.FacePoints(4));

    faceOfPoly.FacePoints.deallocate();
}

TEST(SurfaceGeometryUnitTests, isEnclosedVolume_SimpleBox_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, isEnclosedVolume_SimpleBox_test");

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

TEST(SurfaceGeometryUnitTests, isEnclosedVolume_BoxWithSplitSide_test)
{
    ShowMessage("Begin Test: SurfaceGeometryUnitTests, isEnclosedVolume_BoxWithSplitSide_test");

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

TEST_F(EnergyPlusFixture, CalculateZoneVolume_SimpleBox_test)
{
    using DataGlobals::NumOfZones;

    Array1D_bool enteredCeilingHeight;
    NumOfZones = 1;
    enteredCeilingHeight.dimension(NumOfZones, false);
    Zone.dimension(NumOfZones);
    Zone(1).HasFloor = true;
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 6;

    Surface.dimension(6);

    Surface(1).Sides = 4;
    Surface(1).Vertex.dimension(4);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Tilt = 90.;
    Surface(1).Vertex(1) = Vector(0., 0., 3.);
    Surface(1).Vertex(2) = Vector(0., 0., 0.);
    Surface(1).Vertex(3) = Vector(10., 0., 0.);
    Surface(1).Vertex(4) = Vector(10., 0., 3.);

    Surface(2).Sides = 4;
    Surface(2).Vertex.dimension(4);
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Tilt = 90.;
    Surface(2).Vertex(1) = Vector(0., 8., 3.);
    Surface(2).Vertex(2) = Vector(0., 8., 0.);
    Surface(2).Vertex(3) = Vector(0., 0., 0.);
    Surface(2).Vertex(4) = Vector(0., 0., 3.);

    Surface(3).Sides = 4;
    Surface(3).Vertex.dimension(4);
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 90.;
    Surface(3).Vertex(1) = Vector(10., 8., 3.);
    Surface(3).Vertex(2) = Vector(10., 8., 0.);
    Surface(3).Vertex(3) = Vector(0., 8., 0.);
    Surface(3).Vertex(4) = Vector(0., 8., 3.);

    Surface(4).Sides = 4;
    Surface(4).Vertex.dimension(4);
    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Tilt = 90.;
    Surface(4).Vertex(1) = Vector(10., 0., 3.);
    Surface(4).Vertex(2) = Vector(10., 0., 0.);
    Surface(4).Vertex(3) = Vector(10., 8., 0.);
    Surface(4).Vertex(4) = Vector(10., 8., 3.);

    Surface(5).Sides = 4;
    Surface(5).Vertex.dimension(4);
    Surface(5).Class = SurfaceClass_Floor;
    Surface(5).Tilt = 180.;
    Surface(5).Vertex(1) = Vector(0., 0., 0.);
    Surface(5).Vertex(2) = Vector(0., 8, 0.);
    Surface(5).Vertex(3) = Vector(10., 8, 0.);
    Surface(5).Vertex(4) = Vector(10., 0, 0.);

    Surface(6).Sides = 4;
    Surface(6).Vertex.dimension(4);
    Surface(6).Class = SurfaceClass_Roof;
    Surface(6).Tilt = 0.;
    Surface(6).Vertex(1) = Vector(0., 8., 3.);
    Surface(6).Vertex(2) = Vector(0., 0., 3.);
    Surface(6).Vertex(3) = Vector(10., 0., 3.);
    Surface(6).Vertex(4) = Vector(10., 8., 3.);

    CalculateZoneVolume(enteredCeilingHeight);
    EXPECT_EQ(240., Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxOneWallMissing_test)
{
    using DataGlobals::NumOfZones;

    Array1D_bool enteredCeilingHeight;
    NumOfZones = 1;
    enteredCeilingHeight.dimension(NumOfZones, false);
    Zone.dimension(NumOfZones);
    Zone(1).HasFloor = true;
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 5;

    Surface.dimension(5);

    Surface(1).Sides = 4;
    Surface(1).Vertex.dimension(4);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Tilt = 90.;
    Surface(1).Vertex(1) = Vector(0., 0., 3.);
    Surface(1).Vertex(2) = Vector(0., 0., 0.);
    Surface(1).Vertex(3) = Vector(10., 0., 0.);
    Surface(1).Vertex(4) = Vector(10., 0., 3.);

    Surface(2).Sides = 4;
    Surface(2).Vertex.dimension(4);
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Tilt = 90.;
    Surface(2).Vertex(1) = Vector(0., 8., 3.);
    Surface(2).Vertex(2) = Vector(0., 8., 0.);
    Surface(2).Vertex(3) = Vector(0., 0., 0.);
    Surface(2).Vertex(4) = Vector(0., 0., 3.);

    Surface(3).Sides = 4;
    Surface(3).Vertex.dimension(4);
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 90.;
    Surface(3).Vertex(1) = Vector(10., 8., 3.);
    Surface(3).Vertex(2) = Vector(10., 8., 0.);
    Surface(3).Vertex(3) = Vector(0., 8., 0.);
    Surface(3).Vertex(4) = Vector(0., 8., 3.);

    Surface(4).Sides = 4;
    Surface(4).Vertex.dimension(4);
    Surface(4).Class = SurfaceClass_Floor;
    Surface(4).Tilt = 180.;
    Surface(4).Vertex(1) = Vector(0., 0., 0.);
    Surface(4).Vertex(2) = Vector(0., 8, 0.);
    Surface(4).Vertex(3) = Vector(10., 8, 0.);
    Surface(4).Vertex(4) = Vector(10., 0, 0.);

    Surface(5).Sides = 4;
    Surface(5).Vertex.dimension(4);
    Surface(5).Class = SurfaceClass_Roof;
    Surface(5).Tilt = 0.;
    Surface(5).Vertex(1) = Vector(0., 8., 3.);
    Surface(5).Vertex(2) = Vector(0., 0., 3.);
    Surface(5).Vertex(3) = Vector(10., 0., 3.);
    Surface(5).Vertex(4) = Vector(10., 8., 3.);

    Zone(1).FloorArea = 80.;
    Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(enteredCeilingHeight);
    EXPECT_EQ(240., Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoCeiling_test)
{
    using DataGlobals::NumOfZones;

    Array1D_bool enteredCeilingHeight;
    NumOfZones = 1;
    enteredCeilingHeight.dimension(NumOfZones, false);
    Zone.dimension(NumOfZones);
    Zone(1).HasFloor = true;
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 5;

    Surface.dimension(5);

    Surface(1).Sides = 4;
    Surface(1).Vertex.dimension(4);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Tilt = 90.;
    Surface(1).Vertex(1) = Vector(0., 0., 3.);
    Surface(1).Vertex(2) = Vector(0., 0., 0.);
    Surface(1).Vertex(3) = Vector(10., 0., 0.);
    Surface(1).Vertex(4) = Vector(10., 0., 3.);

    Surface(2).Sides = 4;
    Surface(2).Vertex.dimension(4);
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Tilt = 90.;
    Surface(2).Vertex(1) = Vector(0., 8., 3.);
    Surface(2).Vertex(2) = Vector(0., 8., 0.);
    Surface(2).Vertex(3) = Vector(0., 0., 0.);
    Surface(2).Vertex(4) = Vector(0., 0., 3.);

    Surface(3).Sides = 4;
    Surface(3).Vertex.dimension(4);
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 90.;
    Surface(3).Vertex(1) = Vector(10., 8., 3.);
    Surface(3).Vertex(2) = Vector(10., 8., 0.);
    Surface(3).Vertex(3) = Vector(0., 8., 0.);
    Surface(3).Vertex(4) = Vector(0., 8., 3.);

    Surface(4).Sides = 4;
    Surface(4).Vertex.dimension(4);
    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Tilt = 90.;
    Surface(4).Vertex(1) = Vector(10., 0., 3.);
    Surface(4).Vertex(2) = Vector(10., 0., 0.);
    Surface(4).Vertex(3) = Vector(10., 8., 0.);
    Surface(4).Vertex(4) = Vector(10., 8., 3.);

    Surface(5).Sides = 4;
    Surface(5).Vertex.dimension(4);
    Surface(5).Class = SurfaceClass_Floor;
    Surface(5).Tilt = 180.;
    Surface(5).Vertex(1) = Vector(0., 0., 0.);
    Surface(5).Vertex(2) = Vector(0., 8, 0.);
    Surface(5).Vertex(3) = Vector(10., 8, 0.);
    Surface(5).Vertex(4) = Vector(10., 0, 0.);

    Zone(1).FloorArea = 80.;
    Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(enteredCeilingHeight);
    EXPECT_EQ(240., Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoFloor_test)
{
    using DataGlobals::NumOfZones;

    Array1D_bool enteredCeilingHeight;
    NumOfZones = 1;
    enteredCeilingHeight.dimension(NumOfZones, false);
    Zone.dimension(NumOfZones);
    Zone(1).HasFloor = true;
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 5;

    Surface.dimension(5);

    Surface(1).Sides = 4;
    Surface(1).Vertex.dimension(4);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Tilt = 90.;
    Surface(1).Vertex(1) = Vector(0., 0., 3.);
    Surface(1).Vertex(2) = Vector(0., 0., 0.);
    Surface(1).Vertex(3) = Vector(10., 0., 0.);
    Surface(1).Vertex(4) = Vector(10., 0., 3.);

    Surface(2).Sides = 4;
    Surface(2).Vertex.dimension(4);
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Tilt = 90.;
    Surface(2).Vertex(1) = Vector(0., 8., 3.);
    Surface(2).Vertex(2) = Vector(0., 8., 0.);
    Surface(2).Vertex(3) = Vector(0., 0., 0.);
    Surface(2).Vertex(4) = Vector(0., 0., 3.);

    Surface(3).Sides = 4;
    Surface(3).Vertex.dimension(4);
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 90.;
    Surface(3).Vertex(1) = Vector(10., 8., 3.);
    Surface(3).Vertex(2) = Vector(10., 8., 0.);
    Surface(3).Vertex(3) = Vector(0., 8., 0.);
    Surface(3).Vertex(4) = Vector(0., 8., 3.);

    Surface(4).Sides = 4;
    Surface(4).Vertex.dimension(4);
    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Tilt = 90.;
    Surface(4).Vertex(1) = Vector(10., 0., 3.);
    Surface(4).Vertex(2) = Vector(10., 0., 0.);
    Surface(4).Vertex(3) = Vector(10., 8., 0.);
    Surface(4).Vertex(4) = Vector(10., 8., 3.);

    Surface(5).Sides = 4;
    Surface(5).Vertex.dimension(4);
    Surface(5).Class = SurfaceClass_Roof;
    Surface(5).Tilt = 0.;
    Surface(5).Vertex(1) = Vector(0., 8., 3.);
    Surface(5).Vertex(2) = Vector(0., 0., 3.);
    Surface(5).Vertex(3) = Vector(10., 0., 3.);
    Surface(5).Vertex(4) = Vector(10., 8., 3.);

    Zone(1).CeilingArea = 80.;
    Zone(1).CeilingHeight = 3.;

    CalculateZoneVolume(enteredCeilingHeight);
    EXPECT_EQ(240., Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, CalculateZoneVolume_BoxNoCeilingFloor_test)
{
    using DataGlobals::NumOfZones;

    Array1D_bool enteredCeilingHeight;
    NumOfZones = 1;
    enteredCeilingHeight.dimension(NumOfZones, false);
    Zone.dimension(NumOfZones);
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 4;

    Surface.dimension(4);

    Surface(1).Sides = 4;
    Surface(1).Vertex.dimension(4);
    Surface(1).Class = SurfaceClass_Wall;
    Surface(1).Tilt = 90.;
    Surface(1).Azimuth = 180.;
    Surface(1).Area = 30.;
    Surface(1).Vertex(1) = Vector(0., 0., 3.);
    Surface(1).Vertex(2) = Vector(0., 0., 0.);
    Surface(1).Vertex(3) = Vector(10., 0., 0.);
    Surface(1).Vertex(4) = Vector(10., 0., 3.);

    Surface(2).Sides = 4;
    Surface(2).Vertex.dimension(4);
    Surface(2).Class = SurfaceClass_Wall;
    Surface(2).Tilt = 90.;
    Surface(2).Azimuth = 270.;
    Surface(2).Area = 24.;
    Surface(2).Vertex(1) = Vector(0., 8., 3.);
    Surface(2).Vertex(2) = Vector(0., 8., 0.);
    Surface(2).Vertex(3) = Vector(0., 0., 0.);
    Surface(2).Vertex(4) = Vector(0., 0., 3.);

    Surface(3).Sides = 4;
    Surface(3).Vertex.dimension(4);
    Surface(3).Class = SurfaceClass_Wall;
    Surface(3).Tilt = 90.;
    Surface(3).Azimuth = 0.;
    Surface(3).Area = 30.;
    Surface(3).Vertex(1) = Vector(10., 8., 3.);
    Surface(3).Vertex(2) = Vector(10., 8., 0.);
    Surface(3).Vertex(3) = Vector(0., 8., 0.);
    Surface(3).Vertex(4) = Vector(0., 8., 3.);

    Surface(4).Sides = 4;
    Surface(4).Vertex.dimension(4);
    Surface(4).Class = SurfaceClass_Wall;
    Surface(4).Tilt = 90.;
    Surface(4).Azimuth = 90.;
    Surface(4).Area = 24.;
    Surface(4).Vertex(1) = Vector(10., 0., 3.);
    Surface(4).Vertex(2) = Vector(10., 0., 0.);
    Surface(4).Vertex(3) = Vector(10., 8., 0.);
    Surface(4).Vertex(4) = Vector(10., 8., 3.);

    CalculateZoneVolume(enteredCeilingHeight);
    EXPECT_EQ(240., Zone(1).Volume);
}

TEST_F(EnergyPlusFixture, MakeRectangularVertices)
{
    int surfNum = 1;
    int zoneNum = 1;
    SurfaceTmp.allocate(surfNum);
    SurfaceTmp(surfNum).Class = SurfaceClass_Wall;
    SurfaceTmp(surfNum).Zone = zoneNum;
    SurfaceTmp(surfNum).Azimuth = 0.;
    SurfaceTmp(surfNum).Tilt = 90.;
    SurfaceTmp(surfNum).Sides = 4;
    SurfaceTmp(surfNum).Vertex.allocate(4);

    Zone.allocate(zoneNum);
    Zone(zoneNum).RelNorth = 0.;

    CosZoneRelNorth.allocate(zoneNum);
    SinZoneRelNorth.allocate(zoneNum);
    CosZoneRelNorth(zoneNum) = std::cos(-Zone(zoneNum).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(zoneNum) = std::sin(-Zone(zoneNum).RelNorth * DataGlobals::DegToRadians);

    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;

    // facing north

    MakeRectangularVertices(1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(-5., SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(-5., SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing east

    SurfaceTmp(surfNum).Azimuth = 90.;

    MakeRectangularVertices(1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0, SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(5., SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(5., SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing south

    SurfaceTmp(surfNum).Azimuth = 180.;

    MakeRectangularVertices(1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0, SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(5., SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(5., SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(4).z, 0.001);

    // facing west

    SurfaceTmp(surfNum).Azimuth = 270.;

    MakeRectangularVertices(1, 0., 0., 0., 5., 3., false);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(1).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(1).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).x, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(2).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).x, 0.001);
    EXPECT_NEAR(-5., SurfaceTmp(surfNum).Vertex(3).y, 0.001);
    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(3).z, 0.001);

    EXPECT_NEAR(0., SurfaceTmp(surfNum).Vertex(4).x, 0.001);
    EXPECT_NEAR(-5., SurfaceTmp(surfNum).Vertex(4).y, 0.001);
    EXPECT_NEAR(3., SurfaceTmp(surfNum).Vertex(4).z, 0.001);
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_VertexNumberMismatchTest)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,                                                        ",
        "	8.6;                     !- Version Identifier               ",
        "	                                                             ",
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

    DataGlobals::NumOfZones = 2;
    Zone.allocate(2);
    Zone(1).Name = "ZONE 1";
    Zone(2).Name = "ZONE 2";
    SurfaceTmp.allocate(2);
    int SurfNum = 0;
    int TotHTSurfs = 2;
    Array1D_string const BaseSurfCls(3, {"WALL", "FLOOR", "ROOF"});
    Array1D_int const BaseSurfIDs(3, {1, 2, 3});
    int NeedToAddSurfaces;

    GetGeometryParameters(ErrorsFound);
    CosZoneRelNorth.allocate(2);
    SinZoneRelNorth.allocate(2);

    CosZoneRelNorth = 1.0;
    SinZoneRelNorth = 0.0;
    SinBldgRelNorth = 0.0;
    CosBldgRelNorth = 1.0;

    GetHTSurfaceData(ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);

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

    TotSurfaces = 2;
    MaxVerticesPerSurface = 9;
    Surface.allocate(TotSurfaces);
    ShadeV.allocate(TotSurfaces);
    Surface(1).Vertex.allocate(7);
    Surface(2).Vertex.allocate(9);
    SurfaceTmp.allocate(TotSurfaces);
    SurfaceTmp(1).Vertex.allocate(7);
    SurfaceTmp(2).Vertex.allocate(9);

    int ThisSurf(0);

    // Surface 1 - Rectangle with 7 points
    ThisSurf = 1;
    Surface(ThisSurf).Azimuth = 0.0;
    Surface(ThisSurf).Tilt = 180.0;
    Surface(ThisSurf).Sides = 7;
    Surface(ThisSurf).GrossArea = 20.0;

    Surface(ThisSurf).Vertex(1).x = 10.0;
    Surface(ThisSurf).Vertex(1).y = 2.0;
    Surface(ThisSurf).Vertex(1).z = 3.0;

    Surface(ThisSurf).Vertex(2).x = 10.0;
    Surface(ThisSurf).Vertex(2).y = 3.0;
    Surface(ThisSurf).Vertex(2).z = 3.0;

    Surface(ThisSurf).Vertex(3).x = 10.0;
    Surface(ThisSurf).Vertex(3).y = 4.0;
    Surface(ThisSurf).Vertex(3).z = 3.0;

    Surface(ThisSurf).Vertex(4).x = 10.0;
    Surface(ThisSurf).Vertex(4).y = 5.0;
    Surface(ThisSurf).Vertex(4).z = 3.0;

    Surface(ThisSurf).Vertex(5).x = 10.0;
    Surface(ThisSurf).Vertex(5).y = 6.0;
    Surface(ThisSurf).Vertex(5).z = 3.0;

    Surface(ThisSurf).Vertex(6).x = 15.0;
    Surface(ThisSurf).Vertex(6).y = 6.0;
    Surface(ThisSurf).Vertex(6).z = 3.0;

    Surface(ThisSurf).Vertex(7).x = 15.0;
    Surface(ThisSurf).Vertex(7).y = 2.0;
    Surface(ThisSurf).Vertex(7).z = 3.0;

    SurfaceTmp(ThisSurf) = Surface(ThisSurf);
    CheckConvexity(ThisSurf, SurfaceTmp(ThisSurf).Sides);
    Surface(ThisSurf) = SurfaceTmp(ThisSurf);
    EXPECT_EQ(4, Surface(ThisSurf).Sides);
    EXPECT_EQ(10.0, Surface(ThisSurf).Vertex(2).x);
    EXPECT_EQ(6.0, Surface(ThisSurf).Vertex(2).y);
    EXPECT_EQ(15.0, Surface(ThisSurf).Vertex(3).x);
    EXPECT_EQ(6.0, Surface(ThisSurf).Vertex(3).y);

    // Surface 2 - Rectangle with 9 points
    ThisSurf = 2;
    Surface(ThisSurf).Azimuth = 0.0;
    Surface(ThisSurf).Tilt = 0.0;
    Surface(ThisSurf).Sides = 9;
    Surface(ThisSurf).GrossArea = 30.0;

    Surface(ThisSurf).Vertex(1).x = 10.0;
    Surface(ThisSurf).Vertex(1).y = 2.0;
    Surface(ThisSurf).Vertex(1).z = 0.0;

    Surface(ThisSurf).Vertex(2).x = 10.0;
    Surface(ThisSurf).Vertex(2).y = 3.0;
    Surface(ThisSurf).Vertex(2).z = 0.0;

    Surface(ThisSurf).Vertex(3).x = 10.0;
    Surface(ThisSurf).Vertex(3).y = 4.0;
    Surface(ThisSurf).Vertex(3).z = 0.0;

    Surface(ThisSurf).Vertex(4).x = 10.0;
    Surface(ThisSurf).Vertex(4).y = 5.0;
    Surface(ThisSurf).Vertex(4).z = 0.0;

    Surface(ThisSurf).Vertex(5).x = 10.0;
    Surface(ThisSurf).Vertex(5).y = 6.0;
    Surface(ThisSurf).Vertex(5).z = 0.0;

    Surface(ThisSurf).Vertex(6).x = 10.0;
    Surface(ThisSurf).Vertex(6).y = 7.0;
    Surface(ThisSurf).Vertex(6).z = 0.0;

    Surface(ThisSurf).Vertex(7).x = 10.0;
    Surface(ThisSurf).Vertex(7).y = 8.0;
    Surface(ThisSurf).Vertex(7).z = 0.0;

    Surface(ThisSurf).Vertex(8).x = 15.0;
    Surface(ThisSurf).Vertex(8).y = 8.0;
    Surface(ThisSurf).Vertex(8).z = 0.0;

    Surface(ThisSurf).Vertex(9).x = 15.0;
    Surface(ThisSurf).Vertex(9).y = 2.0;
    Surface(ThisSurf).Vertex(9).z = 0.0;

    SurfaceTmp(ThisSurf) = Surface(ThisSurf);
    CheckConvexity(ThisSurf, SurfaceTmp(ThisSurf).Sides);
    Surface(ThisSurf) = SurfaceTmp(ThisSurf);
    EXPECT_EQ(4, Surface(ThisSurf).Sides);
    EXPECT_EQ(10.0, Surface(ThisSurf).Vertex(2).x);
    EXPECT_EQ(8.0, Surface(ThisSurf).Vertex(2).y);
    EXPECT_EQ(15.0, Surface(ThisSurf).Vertex(3).x);
    EXPECT_EQ(8.0, Surface(ThisSurf).Vertex(3).y);
}

TEST_F(EnergyPlusFixture, InitialAssociateWindowShadingControlFenestration_test)
{
    TotWinShadingControl = 3;
    WindowShadingControl.allocate(TotWinShadingControl);
    int zn = 1;

    WindowShadingControl(1).Name = "WSC1";
    WindowShadingControl(1).ZoneIndex = zn;
    WindowShadingControl(1).SequenceNumber = 2;
    WindowShadingControl(1).MultiSurfaceCtrlIsGroup = true;
    WindowShadingControl(1).FenestrationCount = 3;
    WindowShadingControl(1).FenestrationName.allocate(WindowShadingControl(1).FenestrationCount);
    WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    WindowShadingControl(1).FenestrationName(3) = "Fene-03";

    WindowShadingControl(2).Name = "WSC2";
    WindowShadingControl(2).ZoneIndex = zn;
    WindowShadingControl(2).SequenceNumber = 3;
    WindowShadingControl(2).MultiSurfaceCtrlIsGroup = false;
    WindowShadingControl(2).FenestrationCount = 4;
    WindowShadingControl(2).FenestrationName.allocate(WindowShadingControl(2).FenestrationCount);
    WindowShadingControl(2).FenestrationName(1) = "Fene-04";
    WindowShadingControl(2).FenestrationName(2) = "Fene-05";
    WindowShadingControl(2).FenestrationName(3) = "Fene-06";
    WindowShadingControl(2).FenestrationName(4) = "Fene-07";

    WindowShadingControl(3).Name = "WSC3";
    WindowShadingControl(3).ZoneIndex = zn;
    WindowShadingControl(3).SequenceNumber = 1;
    WindowShadingControl(3).MultiSurfaceCtrlIsGroup = true;
    WindowShadingControl(3).FenestrationCount = 2;
    WindowShadingControl(3).FenestrationName.allocate(WindowShadingControl(3).FenestrationCount);
    WindowShadingControl(3).FenestrationName(1) = "Fene-08";
    WindowShadingControl(3).FenestrationName(2) = "Fene-09";

    Construct.allocate(1);
    Construct(1).WindowTypeEQL = false;

    SurfaceTmp.allocate(9);

    SurfaceTmp(1).Name = "Fene-04";
    SurfaceTmp(1).Construction = 1;
    SurfaceTmp(1).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(2).Name = "Fene-05";
    SurfaceTmp(2).Construction = 1;
    SurfaceTmp(2).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(3).Name = "Fene-06";
    SurfaceTmp(3).Construction = 1;
    SurfaceTmp(3).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(4).Name = "Fene-01";
    SurfaceTmp(4).Construction = 1;
    SurfaceTmp(4).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(5).Name = "Fene-02";
    SurfaceTmp(5).Construction = 1;
    SurfaceTmp(5).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(6).Name = "Fene-03";
    SurfaceTmp(6).Construction = 1;
    SurfaceTmp(6).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(7).Name = "Fene-07";
    SurfaceTmp(7).Construction = 1;
    SurfaceTmp(7).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(8).Name = "Fene-08";
    SurfaceTmp(8).Construction = 1;
    SurfaceTmp(8).ExtBoundCond = ExternalEnvironment;

    SurfaceTmp(9).Name = "Fene-09";
    SurfaceTmp(9).Construction = 1;
    SurfaceTmp(9).ExtBoundCond = ExternalEnvironment;

    bool Err = false;

    int surfNum = 1;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 2);
    EXPECT_FALSE(Err);

    surfNum = 2;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 2);
    EXPECT_FALSE(Err);

    surfNum = 3;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 2);
    EXPECT_FALSE(Err);

    surfNum = 4;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 1);
    EXPECT_FALSE(Err);

    surfNum = 5;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 1);
    EXPECT_FALSE(Err);

    surfNum = 6;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 1);
    EXPECT_FALSE(Err);

    surfNum = 7;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 2);
    EXPECT_FALSE(Err);

    surfNum = 8;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 3);
    EXPECT_FALSE(Err);

    surfNum = 9;
    InitialAssociateWindowShadingControlFenestration(Err, surfNum);
    EXPECT_TRUE(SurfaceTmp(surfNum).HasShadeControl);
    EXPECT_EQ(SurfaceTmp(surfNum).WindowShadingControlPtr, 3);
    EXPECT_FALSE(Err);
}

TEST_F(EnergyPlusFixture, FinalAssociateWindowShadingControlFenestration_test)
{
    TotWinShadingControl = 3;
    WindowShadingControl.allocate(TotWinShadingControl);
    int zn = 1;

    WindowShadingControl(1).Name = "WSC1";
    WindowShadingControl(1).ZoneIndex = zn;
    WindowShadingControl(1).SequenceNumber = 2;
    WindowShadingControl(1).MultiSurfaceCtrlIsGroup = true;
    WindowShadingControl(1).FenestrationCount = 3;
    WindowShadingControl(1).FenestrationName.allocate(WindowShadingControl(1).FenestrationCount);
    WindowShadingControl(1).FenestrationIndex.allocate(WindowShadingControl(1).FenestrationCount);
    WindowShadingControl(1).FenestrationName(1) = "Fene-01";
    WindowShadingControl(1).FenestrationName(2) = "Fene-02";
    WindowShadingControl(1).FenestrationName(3) = "Fene-03";

    WindowShadingControl(2).Name = "WSC2";
    WindowShadingControl(2).ZoneIndex = zn;
    WindowShadingControl(2).SequenceNumber = 3;
    WindowShadingControl(2).MultiSurfaceCtrlIsGroup = false;
    WindowShadingControl(2).FenestrationCount = 4;
    WindowShadingControl(2).FenestrationName.allocate(WindowShadingControl(2).FenestrationCount);
    WindowShadingControl(2).FenestrationIndex.allocate(WindowShadingControl(2).FenestrationCount);
    WindowShadingControl(2).FenestrationName(1) = "Fene-04";
    WindowShadingControl(2).FenestrationName(2) = "Fene-05";
    WindowShadingControl(2).FenestrationName(3) = "Fene-06";
    WindowShadingControl(2).FenestrationName(4) = "Fene-07";

    WindowShadingControl(3).Name = "WSC3";
    WindowShadingControl(3).ZoneIndex = zn;
    WindowShadingControl(3).SequenceNumber = 1;
    WindowShadingControl(3).MultiSurfaceCtrlIsGroup = true;
    WindowShadingControl(3).FenestrationCount = 2;
    WindowShadingControl(3).FenestrationName.allocate(WindowShadingControl(3).FenestrationCount);
    WindowShadingControl(3).FenestrationIndex.allocate(WindowShadingControl(3).FenestrationCount);
    WindowShadingControl(3).FenestrationName(1) = "Fene-08";
    WindowShadingControl(3).FenestrationName(2) = "Fene-09";


    TotSurfaces = 12;
    Surface.allocate(TotSurfaces);

    Surface(1).Name = "Fene-07";
    Surface(1).WindowShadingControlPtr = 2;

    Surface(2).Name = "Fene-01";
    Surface(2).WindowShadingControlPtr = 1;

    Surface(3).Name = "Fene-08";
    Surface(3).WindowShadingControlPtr = 3;

    Surface(4).Name = "Fene-02";
    Surface(4).WindowShadingControlPtr = 1;

    Surface(5).Name = "Fene-10";
    Surface(5).WindowShadingControlPtr = 0;

    Surface(6).Name = "Fene-03";
    Surface(6).WindowShadingControlPtr = 1;

    Surface(7).Name = "Fene-09";
    Surface(7).WindowShadingControlPtr = 3;

    Surface(8).Name = "Fene-04";
    Surface(8).WindowShadingControlPtr = 2;

    Surface(9).Name = "Fene-10";
    Surface(9).WindowShadingControlPtr = 0;

    Surface(10).Name = "Fene-05";
    Surface(10).WindowShadingControlPtr = 2;

    Surface(11).Name = "Fene-11";
    Surface(11).WindowShadingControlPtr = 0;

    Surface(12).Name = "Fene-06";
    Surface(12).WindowShadingControlPtr = 2;


    bool Err = false;

    FinalAssociateWindowShadingControlFenestration(Err);
    EXPECT_FALSE(Err);

    EXPECT_EQ(WindowShadingControl(1).FenestrationIndex(1), 2);
    EXPECT_EQ(WindowShadingControl(1).FenestrationIndex(2), 4);
    EXPECT_EQ(WindowShadingControl(1).FenestrationIndex(3), 6);

    EXPECT_EQ(WindowShadingControl(2).FenestrationIndex(1), 8);
    EXPECT_EQ(WindowShadingControl(2).FenestrationIndex(2), 10);
    EXPECT_EQ(WindowShadingControl(2).FenestrationIndex(3), 12);
    EXPECT_EQ(WindowShadingControl(2).FenestrationIndex(4), 1);

    EXPECT_EQ(WindowShadingControl(3).FenestrationIndex(1), 3);
    EXPECT_EQ(WindowShadingControl(3).FenestrationIndex(2), 7);

}

TEST_F(EnergyPlusFixture, SurfaceGeometry_HeatTransferAlgorithmTest)
{
    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,                                                        ",
        "	8.9;                     !- Version Identifier               ",
        "	                                                             ",
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

        "SurfaceProperty:HeatTransferAlgorithm:Construction,",
        "    Ceilings,                !- Name",
        "    ConductionFiniteDifference,  !- Algorithm",
        "    Project semi-exposed ceiling;  !- Construction Name",

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

    GetProjectControlData(ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);          // expect no errors

    GetMaterialData(ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    CosZoneRelNorth.allocate(2);
    SinZoneRelNorth.allocate(2);

    CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    CosZoneRelNorth(2) = CosZoneRelNorth(1);
    SinZoneRelNorth(2) = SinZoneRelNorth(1);
    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;

    GetSurfaceData(ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    EXPECT_EQ(5, Surface(1).HeatTransferAlgorithm);
    EXPECT_EQ(5, Surface(2).HeatTransferAlgorithm);
    std::string const error_string = delimited_string({
        "   ** Warning ** GetSurfaceData: Entered Zone Floor Areas differ from calculated Zone Floor Area(s).",
        "   **   ~~~   ** ...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.",
        "   ** Warning ** An interior surface is defined as two surfaces with reverse constructions. The HeatTransferAlgorithm in both constructions should be same.",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: DATATELCOM_CEILING_1_0_0, is CondFD - ConductionFiniteDifference",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: ZONE1_FLOOR_4_0_10000, is CTF - ConductionTransferFunction",
        "   **   ~~~   ** The HeatTransferAlgorithm of Surface: ZONE1_FLOOR_4_0_10000, is assigned to CondFD - ConductionFiniteDifference. Simulation continues.",
        });
    EXPECT_TRUE(compare_err_stream(error_string, true));
    
}
