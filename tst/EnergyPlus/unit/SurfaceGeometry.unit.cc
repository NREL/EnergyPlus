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

// EnergyPlus::SurfaceGeometry Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::HeatBalanceManager;

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

    ProcessSurfaceVertices(state.files, ThisSurf, ErrorsFound);
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

    ProcessSurfaceVertices(state.files, ThisSurf, ErrorsFound);
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

    ProcessSurfaceVertices(state.files, ThisSurf, ErrorsFound);
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

    ProcessSurfaceVertices(state.files, ThisSurf, ErrorsFound);
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

    ProcessSurfaceVertices(state.files, ThisSurf, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(SurfaceShape::Polygonal, Surface(ThisSurf).Shape);
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

    GetProjectControlData(state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);          // expect no errors

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    CosZoneRelNorth.allocate(1);
    SinZoneRelNorth.allocate(1);

    CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;

    GetSurfaceData(state, state.files, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    // compare_err_stream( "" ); // just for debugging

    AllocateModuleArrays();

    //  Adding additional surfaces will change the index of the following based on where the surfaces are added in the array.
    //	If adding new tests, break here and look at EnergyPlus::DataSurfaces::Surface to see the order.

    //	enum surfaceShape:Triangle = 1
    //	"Surface 1 - Triangle"
    int surfNum = UtilityRoutines::FindItemInList("SURFACE 1 - TRIANGLE", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Triangle, Surface(surfNum).Shape);

    //	enum surfaceShape:Quadrilateral = 2
    //	"Surface 2 - Quadrilateral"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 2 - QUADRILATERAL", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Quadrilateral, Surface(surfNum).Shape);

    //	enum surfaceShape:Rectangle = 3
    //	"Surface 3 - Rectangle"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 3 - RECTANGLE", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Rectangle, Surface(surfNum).Shape);

    //	enum surfaceShape:RectangularDoorWindow = 4
    //	"Surface 4 - RectangularDoorWindow"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 4 - RECTANGULARDOORWINDOW", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::RectangularDoorWindow, Surface(surfNum).Shape);

    //	enum surfaceShape:RectangularOverhang = 5
    //	"Surface 5 - RectangularOverhang"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 5 - RECTANGULAROVERHANG", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularOverhang, Surface(surfNum).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularLeftFin = 6
    //	"Surface 6 - RectangularLeftFin"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 6 - RECTANGULARLEFTFIN Left", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularLeftFin, Surface(surfNum).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:RectangularRightFin = 7
    //	"Surface 7 - RectangularRightFin"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 7 - RECTANGULARRIGHTFIN Right", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_NE(SurfaceShape::RectangularRightFin, Surface(surfNum).Shape); // fins and overhangs will not get set to the proper surface shape.

    //	enum surfaceShape:TriangularWindow = 8
    //	"Surface 8 - TriangularWindow"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 8 - TRIANGULARWINDOW", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::TriangularWindow, Surface(surfNum).Shape);

    //	enum surfaceShape:TriangularDoor = 9
    //	"Surface 9 - TriangularDoor"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 9 - TRIANGULARDOOR", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::TriangularDoor, Surface(surfNum).Shape);

    //	enum surfaceShape:Polygonal = 10
    //	"Surface 10 - Polygonal"
    surfNum = UtilityRoutines::FindItemInList("SURFACE 10 - POLYGONAL", DataSurfaces::Surface);
    ProcessSurfaceVertices(state.files, surfNum, ErrorsFound);
    EXPECT_EQ(SurfaceShape::Polygonal, Surface(surfNum).Shape);
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
    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, FoundError);
    GetConstructData(state.files, FoundError);
    GetZoneData(FoundError); // Read Zone data from input file
    DataHeatBalance::AnyCTF = true;
    SetupZoneGeometry(state, FoundError); // this calls GetSurfaceData()

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
    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);
    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);
    GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);
    GetProjectControlData(state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);
    CosZoneRelNorth.allocate(1);
    SinZoneRelNorth.allocate(1);
    CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DataGlobals::DegToRadians);
    CosBldgRelNorth = 1.0;
    SinBldgRelNorth = 0.0;
    GetSurfaceData(state, state.files, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    // For each surface Run the test then Check the result
    // (1) rectangle window
    int surfNum = UtilityRoutines::FindItemInList("SURFACE-1-RECTANGLE", DataSurfaces::Surface);
    MakeEquivalentRectangle(surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(7.60, Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.20, Surface(surfNum).Height, 0.01);
    // (2) trapzoid window
    surfNum = UtilityRoutines::FindItemInList("SURFACE-2-TRAPZOID", DataSurfaces::Surface);
    MakeEquivalentRectangle(surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(7.80, Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.17, Surface(surfNum).Height, 0.01);
    // (3) parallelogram window
    surfNum = UtilityRoutines::FindItemInList("SURFACE-3-PARALLELOGRAM", DataSurfaces::Surface);
    MakeEquivalentRectangle(surfNum, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_NEAR(8.08, Surface(surfNum).Width, 0.01);
    EXPECT_NEAR(1.13, Surface(surfNum).Height, 0.01);
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

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areSurfaceHorizAndVert_test)
{
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

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areWallHeightSame_test)
{
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

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_findPossibleOppositeFace_test)
{
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

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_areFloorAndCeilingSame_test)
{
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

TEST_F(EnergyPlusFixture, SurfaceGeometryUnitTests_insertVertexOnFace_test)
{
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

    CalculateZoneVolume(state.files, enteredCeilingHeight);
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

    CalculateZoneVolume(state.files, enteredCeilingHeight);
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

    CalculateZoneVolume(state.files, enteredCeilingHeight);
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

    CalculateZoneVolume(state.files, enteredCeilingHeight);
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

    CalculateZoneVolume(state.files, enteredCeilingHeight);
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

    GetGeometryParameters(state.files, ErrorsFound);
    CosZoneRelNorth.allocate(2);
    SinZoneRelNorth.allocate(2);

    CosZoneRelNorth = 1.0;
    SinZoneRelNorth = 0.0;
    SinBldgRelNorth = 0.0;
    CosBldgRelNorth = 1.0;

    GetHTSurfaceData(state, state.files, ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);

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

    dataConstruction.Construct.allocate(1);
    dataConstruction.Construct(1).WindowTypeEQL = false;

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

        "BuildingSurface:Detailed,",
        "    Zone1_Floor_4_0_20000,  !- Name",
        "    Floor,                   !- Surface Type",
        "    Project semi-exposed ceiling_Rev,  !- Construction Name",
        "    ZONE1,             !- Zone Name",
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

    GetProjectControlData(state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);          // expect no errors

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
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

    GetSurfaceData(state, state.files, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);   // expect no errors

    int surfNum = UtilityRoutines::FindItemInList("DATATELCOM_CEILING_1_0_0", DataSurfaces::Surface);
    EXPECT_EQ(DataSurfaces::HeatTransferModel_CondFD, DataSurfaces::Surface(surfNum).HeatTransferAlgorithm);
    EXPECT_TRUE(DataHeatBalance::AnyCondFD);

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_10000", DataSurfaces::Surface);
    EXPECT_EQ(DataSurfaces::HeatTransferModel_CondFD, DataSurfaces::Surface(surfNum).HeatTransferAlgorithm);
    EXPECT_TRUE(DataHeatBalance::AnyEMPD); // input as EMPD but then later overriden to CondFD - see error message below

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_20000", DataSurfaces::Surface);
    EXPECT_EQ(DataSurfaces::HeatTransferModel_HAMT, DataSurfaces::Surface(surfNum).HeatTransferAlgorithm);
    EXPECT_TRUE(DataHeatBalance::AnyHAMT);

    surfNum = UtilityRoutines::FindItemInList("ZONE1_FLOOR_4_0_30000", DataSurfaces::Surface);
    EXPECT_EQ(DataSurfaces::HeatTransferModel_CTF, DataSurfaces::Surface(surfNum).HeatTransferAlgorithm);
    EXPECT_TRUE(DataHeatBalance::AnyCTF);

    std::string const error_string = delimited_string({
        "   ** Warning ** GetSurfaceData: Entered Zone Floor Areas differ from calculated Zone Floor Area(s).",
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
    EXPECT_EQ(DataSurfaces::AllHTSurfaceList.size(), 4u);
    EXPECT_EQ(DataSurfaces::AllIZSurfaceList.size(), 2u);

    int zoneNum = UtilityRoutines::FindItemInList("DATATELCOM", DataHeatBalance::Zone);
    EXPECT_EQ(DataHeatBalance::Zone(zoneNum).ZoneHTSurfaceList.size(), 2u);
    EXPECT_EQ(DataHeatBalance::Zone(zoneNum).ZoneIZSurfaceList.size(), 2u);

    zoneNum = UtilityRoutines::FindItemInList("ZONE1", DataHeatBalance::Zone);
    EXPECT_EQ(DataHeatBalance::Zone(zoneNum).ZoneHTSurfaceList.size(), 4u);
    EXPECT_EQ(DataHeatBalance::Zone(zoneNum).ZoneIZSurfaceList.size(), 2u);
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
    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    GetConstructData(state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    DataGlobals::NumOfZones = 2;
    Zone.allocate(2);
    Zone(1).Name = "ZONE 1";
    Zone(2).Name = "ZONE 2";
    SurfaceTmp.allocate(1);
    int SurfNum = 0;
    int TotHTSurfs = 1;
    Array1D_string const BaseSurfCls(3, {"WALL", "FLOOR", "ROOF"});
    Array1D_int const BaseSurfIDs(3, {1, 2, 3});
    int NeedToAddSurfaces;

    GetGeometryParameters(state.files, ErrorsFound);
    CosZoneRelNorth.allocate(1);
    SinZoneRelNorth.allocate(1);

    CosZoneRelNorth = 1.0;
    SinZoneRelNorth = 0.0;
    SinBldgRelNorth = 0.0;
    CosBldgRelNorth = 1.0;

    GetHTSurfaceData(state, state.files, ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);

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
    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Construction
    GetConstructData(state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Read Zones
    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    // Read InternalMass Object Count
    int TotIntMass = inputProcessor->getNumObjectsFound("InternalMass");
    // check the three internal mass objects
    EXPECT_EQ(3, TotIntMass);

    // Read InternalMass Surfaces Count
    int TotalNumOfInternalMassSurfaces = GetNumIntMassSurfaces();
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
    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Construction
    GetConstructData(state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Read Zones
    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    // Read InternalMass Object Count
    int TotIntMass = inputProcessor->getNumObjectsFound("InternalMass");
    EXPECT_EQ(3, TotIntMass);

    // Read InternalMass Surfaces Count
    int TotalNumOfInternalMassSurfaces = GetNumIntMassSurfaces();
    EXPECT_EQ(24, TotalNumOfInternalMassSurfaces);

    DataSurfaces::TotSurfaces = TotalNumOfInternalMassSurfaces;
    SurfaceTmp.allocate(TotSurfaces);

    int SurfNum = 0;
    GetIntMassSurfaceData(ErrorsFound, SurfNum);
    ASSERT_FALSE(ErrorsFound);

    // check internal mass surface count and object names
    EXPECT_EQ(8, DataSurfaces::IntMassObjects(1).NumOfZones);
    EXPECT_EQ("GFLOORZONESINTMASS", DataSurfaces::IntMassObjects(1).Name);
    EXPECT_EQ(8, DataSurfaces::IntMassObjects(2).NumOfZones);
    EXPECT_EQ("MFLOORZONESINTMASS", DataSurfaces::IntMassObjects(2).Name);
    EXPECT_EQ(8, DataSurfaces::IntMassObjects(3).NumOfZones);
    EXPECT_EQ("TFLOORZONESINTMASS", DataSurfaces::IntMassObjects(3).Name);
    // check total count of internal surfaces created
    EXPECT_EQ(24, TotSurfaces);

    // check unique internal surface name created created from a combination
    // of zone name and internal mass object name represented in the zone
    // first zone in the ground floor ZoneList
    EXPECT_EQ("G SW APARTMENT", Zone(1).Name);
    EXPECT_EQ("GFLOORZONESINTMASS", DataSurfaces::IntMassObjects(1).Name);
    EXPECT_EQ("G SW APARTMENT GFLOORZONESINTMASS", SurfaceTmp(1).Name);
    // first zone in the middle floor ZoneList
    EXPECT_EQ("M SW APARTMENT", Zone(9).Name);
    EXPECT_EQ("MFLOORZONESINTMASS", DataSurfaces::IntMassObjects(2).Name);
    EXPECT_EQ("M SW APARTMENT MFLOORZONESINTMASS", SurfaceTmp(9).Name);
    // first zone in the top floor ZoneList
    EXPECT_EQ("T SW APARTMENT", Zone(17).Name);
    EXPECT_EQ("TFLOORZONESINTMASS", DataSurfaces::IntMassObjects(3).Name);
    EXPECT_EQ("T SW APARTMENT TFLOORZONESINTMASS", SurfaceTmp(17).Name);
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

    DataGlobals::NumOfZones = 1;
    Zone.allocate(1);
    Zone(1).Name = "ZONE 1";
    Zone(1).OriginX = 0;
    Zone(1).OriginY = 0;
    Zone(1).OriginZ = 0;

    GetGeometryParameters(state.files, ErrorsFound);
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

    DataGlobals::NumOfZones = 1;
    Zone.allocate(1);
    Zone(1).Name = "ZONE 1";
    Zone(1).OriginX = 0;
    Zone(1).OriginY = 0;
    Zone(1).OriginZ = 0;

    GetGeometryParameters(state.files, ErrorsFound);
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

    DataGlobals::NumOfZones = 1;
    Zone.allocate(1);
    Zone(1).Name = "ZONE 1";
    Zone(1).OriginX = 6;
    Zone(1).OriginY = 6;
    Zone(1).OriginZ = 0;

    GetGeometryParameters(state.files, ErrorsFound);
    EXPECT_TRUE(has_err_output(false));

    std::string error_string = delimited_string({
        "   ** Warning ** GlobalGeometryRules: Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on the default SurfaceGeometry for 'Relative to zone' coordinate.",
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

    DataGlobals::NumOfZones = 1;
    Zone.allocate(1);
    Zone(1).Name = "ZONE 1";
    Zone(1).OriginX = 6;
    Zone(1).OriginY = 6;
    Zone(1).OriginZ = 0;

    GetGeometryParameters(state.files, ErrorsFound);
    EXPECT_TRUE(has_err_output(false));

    std::string error_string = delimited_string({
        "   ** Warning ** GlobalGeometryRules: Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on the default SurfaceGeometry for 'Relative to zone' coordinate.",
        "   **   ~~~   ** Coordinate System=\"WORLD\"; while ",
        "   **   ~~~   ** Rectangular Surface Coordinate System=\"defaults to RELATIVE\".",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, SurfaceGeometry_CheckForReversedLayers)
{
    bool RevLayerDiffs;
    dataConstruction.Construct.allocate(6);
    dataMaterial.Material.allocate(7);

    // Case 1a: Constructs with regular materials are a reverse of each other--material layers match in reverse (should get a "false" answer)
    dataConstruction.Construct(1).TotLayers = 3;
    dataConstruction.Construct(1).LayerPoint(1) = 1;
    dataConstruction.Construct(1).LayerPoint(2) = 2;
    dataConstruction.Construct(1).LayerPoint(3) = 3;
    dataConstruction.Construct(2).TotLayers = 3;
    dataConstruction.Construct(2).LayerPoint(1) = 3;
    dataConstruction.Construct(2).LayerPoint(2) = 2;
    dataConstruction.Construct(2).LayerPoint(3) = 1;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(RevLayerDiffs, 1, 2, 3);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 1a: Constructs with regular materials are not reverse of each other--material layers do not match in reverse (should get a "true" answer)
    dataConstruction.Construct(2).LayerPoint(1) = 1;
    dataConstruction.Construct(2).LayerPoint(3) = 3;
    dataMaterial.Material(1).Group = RegularMaterial;
    dataMaterial.Material(2).Group = RegularMaterial;
    dataMaterial.Material(3).Group = RegularMaterial;
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(RevLayerDiffs, 1, 2, 3);
    EXPECT_TRUE(RevLayerDiffs);

    // Case 2a: Constructs are reverse of each other using WindowGlass, front/back properties properly switched (should get a "false" answer)
    dataConstruction.Construct(3).TotLayers = 3;
    dataConstruction.Construct(3).LayerPoint(1) = 4;
    dataConstruction.Construct(3).LayerPoint(2) = 2;
    dataConstruction.Construct(3).LayerPoint(3) = 5;
    dataConstruction.Construct(4).TotLayers = 3;
    dataConstruction.Construct(4).LayerPoint(1) = 4;
    dataConstruction.Construct(4).LayerPoint(2) = 2;
    dataConstruction.Construct(4).LayerPoint(3) = 5;
    dataMaterial.Material(4).Group = WindowGlass;
    dataMaterial.Material(4).Thickness = 0.15;
    dataMaterial.Material(4).ReflectSolBeamFront = 0.35;
    dataMaterial.Material(4).ReflectSolBeamBack = 0.25;
    dataMaterial.Material(4).TransVis = 0.45;
    dataMaterial.Material(4).ReflectVisBeamFront = 0.34;
    dataMaterial.Material(4).ReflectVisBeamBack = 0.24;
    dataMaterial.Material(4).TransThermal = 0.44;
    dataMaterial.Material(4).AbsorpThermalFront = 0.33;
    dataMaterial.Material(4).AbsorpThermalBack = 0.23;
    dataMaterial.Material(4).Conductivity = 0.43;
    dataMaterial.Material(4).GlassTransDirtFactor = 0.67;
    dataMaterial.Material(4).SolarDiffusing = true;
    dataMaterial.Material(4).YoungModulus = 0.89;
    dataMaterial.Material(4).PoissonsRatio = 1.11;
    dataMaterial.Material(5).Group = WindowGlass;
    dataMaterial.Material(5).Thickness = 0.15;
    dataMaterial.Material(5).ReflectSolBeamFront = 0.25;
    dataMaterial.Material(5).ReflectSolBeamBack = 0.35;
    dataMaterial.Material(5).TransVis = 0.45;
    dataMaterial.Material(5).ReflectVisBeamFront = 0.24;
    dataMaterial.Material(5).ReflectVisBeamBack = 0.34;
    dataMaterial.Material(5).TransThermal = 0.44;
    dataMaterial.Material(5).AbsorpThermalFront = 0.23;
    dataMaterial.Material(5).AbsorpThermalBack = 0.33;
    dataMaterial.Material(5).Conductivity = 0.43;
    dataMaterial.Material(5).GlassTransDirtFactor = 0.67;
    dataMaterial.Material(5).SolarDiffusing = true;
    dataMaterial.Material(5).YoungModulus = 0.89;
    dataMaterial.Material(5).PoissonsRatio = 1.11;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(RevLayerDiffs, 3, 4, 3);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 2b: Constructs are reverse of each other using WindowGlass, front/back properties NOT properly switched (should get a "true" answer)
    dataMaterial.Material(5).ReflectVisBeamFront = 0.34; // correct would be 0.24
    dataMaterial.Material(5).ReflectVisBeamBack = 0.24;  // correct would be 0.34
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(RevLayerDiffs, 3, 4, 3);
    EXPECT_TRUE(RevLayerDiffs);

    // Case 3a: Single layer constructs using Equivalent Glass, front/back properties properly switched (should get a "false" answer)
    dataConstruction.Construct(5).TotLayers = 1;
    dataConstruction.Construct(5).LayerPoint(1) = 6;
    dataConstruction.Construct(6).TotLayers = 1;
    dataConstruction.Construct(6).LayerPoint(1) = 7;
    dataMaterial.Material(6).Group = GlassEquivalentLayer;
    dataMaterial.Material(6).TausFrontBeamBeam = 0.39;
    dataMaterial.Material(6).TausBackBeamBeam = 0.29;
    dataMaterial.Material(6).ReflFrontBeamBeam = 0.38;
    dataMaterial.Material(6).ReflBackBeamBeam = 0.28;
    dataMaterial.Material(6).TausFrontBeamBeamVis = 0.37;
    dataMaterial.Material(6).TausBackBeamBeamVis = 0.27;
    dataMaterial.Material(6).ReflFrontBeamBeamVis = 0.36;
    dataMaterial.Material(6).ReflBackBeamBeamVis = 0.26;
    dataMaterial.Material(6).TausFrontBeamDiff = 0.35;
    dataMaterial.Material(6).TausBackBeamDiff = 0.25;
    dataMaterial.Material(6).ReflFrontBeamDiff = 0.34;
    dataMaterial.Material(6).ReflBackBeamDiff = 0.24;
    dataMaterial.Material(6).TausFrontBeamDiffVis = 0.33;
    dataMaterial.Material(6).TausBackBeamDiffVis = 0.23;
    dataMaterial.Material(6).ReflFrontBeamDiffVis = 0.32;
    dataMaterial.Material(6).ReflBackBeamDiffVis = 0.22;
    dataMaterial.Material(6).TausDiffDiff = 0.456;
    dataMaterial.Material(6).ReflFrontDiffDiff = 0.31;
    dataMaterial.Material(6).ReflBackDiffDiff = 0.21;
    dataMaterial.Material(6).TausDiffDiffVis = 0.345;
    dataMaterial.Material(6).ReflFrontDiffDiffVis = 0.30;
    dataMaterial.Material(6).ReflBackDiffDiffVis = 0.20;
    dataMaterial.Material(6).TausThermal = 0.234;
    dataMaterial.Material(6).EmissThermalFront = 0.888;
    dataMaterial.Material(6).EmissThermalBack = 0.777;
    dataMaterial.Material(6).Resistance = 1.234;
    dataMaterial.Material(7).Group = GlassEquivalentLayer;
    dataMaterial.Material(7).TausFrontBeamBeam = 0.29;
    dataMaterial.Material(7).TausBackBeamBeam = 0.39;
    dataMaterial.Material(7).ReflFrontBeamBeam = 0.28;
    dataMaterial.Material(7).ReflBackBeamBeam = 0.38;
    dataMaterial.Material(7).TausFrontBeamBeamVis = 0.27;
    dataMaterial.Material(7).TausBackBeamBeamVis = 0.37;
    dataMaterial.Material(7).ReflFrontBeamBeamVis = 0.26;
    dataMaterial.Material(7).ReflBackBeamBeamVis = 0.36;
    dataMaterial.Material(7).TausFrontBeamDiff = 0.25;
    dataMaterial.Material(7).TausBackBeamDiff = 0.35;
    dataMaterial.Material(7).ReflFrontBeamDiff = 0.24;
    dataMaterial.Material(7).ReflBackBeamDiff = 0.34;
    dataMaterial.Material(7).TausFrontBeamDiffVis = 0.23;
    dataMaterial.Material(7).TausBackBeamDiffVis = 0.33;
    dataMaterial.Material(7).ReflFrontBeamDiffVis = 0.22;
    dataMaterial.Material(7).ReflBackBeamDiffVis = 0.32;
    dataMaterial.Material(7).TausDiffDiff = 0.456;
    dataMaterial.Material(7).ReflFrontDiffDiff = 0.21;
    dataMaterial.Material(7).ReflBackDiffDiff = 0.31;
    dataMaterial.Material(7).TausDiffDiffVis = 0.345;
    dataMaterial.Material(7).ReflFrontDiffDiffVis = 0.20;
    dataMaterial.Material(7).ReflBackDiffDiffVis = 0.30;
    dataMaterial.Material(7).TausThermal = 0.234;
    dataMaterial.Material(7).EmissThermalFront = 0.777;
    dataMaterial.Material(7).EmissThermalBack = 0.888;
    dataMaterial.Material(7).Resistance = 1.234;
    RevLayerDiffs = true;
    // ExpectResult = false;
    CheckForReversedLayers(RevLayerDiffs, 5, 6, 1);
    EXPECT_FALSE(RevLayerDiffs);

    // Case 3a: Single layer constructs using Equivalent Glass, front/back properties NOT properly switched (should get a "true" answer)
    dataMaterial.Material(7).EmissThermalFront = 0.888;
    RevLayerDiffs = false;
    // ExpectResult = true;
    CheckForReversedLayers(RevLayerDiffs, 5, 6, 1);
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

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    SetupZoneGeometry(state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    EXPECT_EQ(DataViewFactorInformation::NumOfRadiantEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).Name, "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(2).Name, "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(3).Name, "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(2).ZoneNames[0], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(3).ZoneNames[0], "Zone 3"));
    EXPECT_EQ(DataHeatBalance::Zone(1).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).RadiantEnclosureNum, 2);
    EXPECT_EQ(DataHeatBalance::Zone(3).RadiantEnclosureNum, 3);

    EXPECT_EQ(DataViewFactorInformation::NumOfSolarEnclosures, 3);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).Name, "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(2).Name, "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(3).Name, "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(2).ZoneNames[0], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(3).ZoneNames[0], "Zone 3"));
    EXPECT_EQ(DataHeatBalance::Zone(1).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).SolarEnclosureNum, 2);
    EXPECT_EQ(DataHeatBalance::Zone(3).SolarEnclosureNum, 3);

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
        "GroupedZones,            !- Solar and Daylighting Method",
        "GroupedZones,            !- Radiant Exchange Method",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
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

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    SetupZoneGeometry(state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    EXPECT_EQ(DataViewFactorInformation::NumOfRadiantEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[1], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[2], "Zone 3"));
    EXPECT_EQ(DataHeatBalance::Zone(1).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(3).RadiantEnclosureNum, 1);

    EXPECT_EQ(DataViewFactorInformation::NumOfSolarEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[1], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[2], "Zone 3"));
    EXPECT_EQ(DataHeatBalance::Zone(1).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(3).SolarEnclosureNum, 1);

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
        "Grouped Radiant Air Boundary, !- Name",
        "InteriorWindow,            !- Solar and Daylighting Method",
        "GroupedZones,            !- Radiant Exchange Method",
        "None;                    !- Air Exchange Method",

        "Construction:AirBoundary,",
        "Grouped Solar Air Boundary, !- Name",
        "GroupedZones,            !- Solar and Daylighting Method",
        "IRTSurface,            !- Radiant Exchange Method",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Radiant Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
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
        "    Grouped Radiant Air Boundary,  !- Construction Name",
        "    Zone 2,       !- Zone Name",
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
        "    Grouped Solar Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
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
        "    Grouped Solar Air Boundary,  !- Construction Name",
        "    Zone 3,       !- Zone Name",
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

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    SetupZoneGeometry(state, ErrorsFound);
    // SetupZoneGeometry calls SurfaceGeometry::GetSurfaceData
    // SetupZoneGeometry calls SurfaceGeometry::SetupSolarEnclosuresAndAirBoundaries
    // SetupZoneGeometry calls SurfaceGeometry::SetupRadiantEnclosuresAndAirBoundaries
    EXPECT_FALSE(ErrorsFound); // expect no errors

    ErrorsFound = false;

    //std::string const error_string = delimited_string({
    //"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone 6\" did not find a matching radiant or solar enclosure name."
    //    });
    //EXPECT_TRUE(compare_err_stream(error_string, true));

    // For this test case, Zones 1 and 2 share a radiant enclosure and Zone 1 and 3 share a solar enclosure

    EXPECT_EQ(DataViewFactorInformation::NumOfRadiantEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[1], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(2).Name, "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(2).ZoneNames[0], "Zone 3"));
    EXPECT_EQ(DataHeatBalance::Zone(1).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(3).RadiantEnclosureNum, 2);

    EXPECT_EQ(DataViewFactorInformation::NumOfSolarEnclosures, 2);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[1], "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(2).Name, "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(2).ZoneNames[0], "Zone 2"));
    EXPECT_EQ(DataHeatBalance::Zone(1).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).SolarEnclosureNum, 2);
    EXPECT_EQ(DataHeatBalance::Zone(3).SolarEnclosureNum, 1);

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
        "GroupedZones,            !- Solar and Daylighting Method",
        "GroupedZones,            !- Radiant Exchange Method",
        "None;                    !- Air Exchange Method",

        "BuildingSurface:Detailed,",
        "    Zone1-Surface1,  !- Name",
        "    Wall,                 !- Surface Type",
        "    Grouped Air Boundary,  !- Construction Name",
        "    Zone 1,       !- Zone Name",
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

        });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);    // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    SetupZoneGeometry(state, ErrorsFound);
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

    EXPECT_EQ(DataViewFactorInformation::NumOfRadiantEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).Name, "Radiant Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[1], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[2], "Zone 5"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[3], "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneRadiantInfo(1).ZoneNames[4], "Zone 4"));
    EXPECT_EQ(DataHeatBalance::Zone(1).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(3).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(4).RadiantEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(5).RadiantEnclosureNum, 1);

    EXPECT_EQ(DataViewFactorInformation::NumOfSolarEnclosures, 1);
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).Name, "Solar Enclosure 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[0], "Zone 1"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[1], "Zone 2"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[2], "Zone 5"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[3], "Zone 3"));
    EXPECT_TRUE(UtilityRoutines::SameString(DataViewFactorInformation::ZoneSolarInfo(1).ZoneNames[4], "Zone 4"));
    EXPECT_EQ(DataHeatBalance::Zone(1).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(2).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(3).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(4).SolarEnclosureNum, 1);
    EXPECT_EQ(DataHeatBalance::Zone(5).SolarEnclosureNum, 1);

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
        "    88.249272671219;         !- Surface Area {m2}",

        "  BuildingSurface:Detailed,",
        "    Garage:FrontDoor,        !- Name",
        "    WALL,                    !- Surface Type",
        "    Garage:SteelDoor,        !- Construction Name",
        "    GARAGE ZONE,             !- Zone Name",
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

    GetProjectControlData(state, ErrorsFound); // read project control data
    EXPECT_FALSE(ErrorsFound);                             // expect no errors

    GetMaterialData(state, *state.dataWindowEquivalentLayer, state.files, ErrorsFound); // read material data
    EXPECT_FALSE(ErrorsFound);                       // expect no errors

    GetConstructData(state.files, ErrorsFound); // read construction data
    EXPECT_FALSE(ErrorsFound);     // expect no errors

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors

    SetupZoneGeometry(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound); // expect no errors

    GetSurfaceData(state, state.files, ErrorsFound); // setup zone geometry and get zone data
    EXPECT_FALSE(ErrorsFound);                                                            // expect no errors

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
    //      TubularDaylightDome is treated as a "non-window" subsurface
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
    //      39. TubularDaylightingDome1 (not a window)
    //      40. AtticSkylight (window)

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
    //      32.   TubularDaylightingDome1 (not a window)
    //      33. SouthRoof (roof)
    //      34. NorthRoof2 (roof)
    //      35. NorthRoof3 (roof)
    //      36. NorthRoof4 (roof)
    //      37. EastRoof (roof)
    //      38.   AtticSkylight (window)
    //      39. WestRoof (roof)

    // Simulation Order (1-based):
    //  SHADING SURFACES:
    int siteShadeShadeFlatShadeSurface =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SiteShade:FlatShadeSurface"), DataSurfaces::Surface);
    int mirSiteShadeFlatShadeSurface =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("SiteShade:FlatShadeSurface"), DataSurfaces::Surface);
    int buildingShadeTiltedShadeSurface =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("BuildingShade:TiltedShadeSurface"), DataSurfaces::Surface);
    int mirBuildingShadeTiltedShadeSurface =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("BuildingShade:TiltedShadeSurface"), DataSurfaces::Surface);
    int zoneShadeLivingSouthShade001 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("ZoneShade:Living:South:Shade001"), DataSurfaces::Surface);
    int mirZoneShadeLivingSouthShade001 =
        UtilityRoutines::FindItemInList("Mir-" + UtilityRoutines::MakeUPPERCase("ZoneShade:Living:South:Shade001"), DataSurfaces::Surface);
    EXPECT_EQ(siteShadeShadeFlatShadeSurface, 1);
    EXPECT_EQ(mirSiteShadeFlatShadeSurface, 2);
    EXPECT_EQ(buildingShadeTiltedShadeSurface, 3);
    EXPECT_EQ(mirBuildingShadeTiltedShadeSurface, 4);
    EXPECT_EQ(zoneShadeLivingSouthShade001, 5);
    EXPECT_EQ(mirZoneShadeLivingSouthShade001, 6);

    //  LIVING ZONE:
    int wallLivingNorth = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:North"), DataSurfaces::Surface);
    int wallLivingEast = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:East"), DataSurfaces::Surface);
    int wallLivingSouth = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:South"), DataSurfaces::Surface);
    int wallLivingWest = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:West"), DataSurfaces::Surface);
    int wallLivingInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Interior"), DataSurfaces::Surface);
    int floorLivingFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Floor"), DataSurfaces::Surface);
    int ceilingLivingCeiling = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Living:Ceiling"), DataSurfaces::Surface);
    int doorWestDoor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestDoor"), DataSurfaces::Surface);
    int windowTubularDaylightingDiffuser1 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("TubularDaylightingDiffuser1"), DataSurfaces::Surface);
    int windowNorthWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthWindow"), DataSurfaces::Surface);
    int windowEastWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastWindow"), DataSurfaces::Surface);
    int windowSouthWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SouthWindow"), DataSurfaces::Surface);
    int windowWestWindow = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestWindow"), DataSurfaces::Surface);

    EXPECT_EQ(wallLivingNorth, 7);
    EXPECT_EQ(wallLivingEast, 8);
    EXPECT_EQ(wallLivingSouth, 9);
    EXPECT_EQ(wallLivingWest, 10);
    EXPECT_EQ(wallLivingInterior, 11);
    EXPECT_EQ(floorLivingFloor, 12);
    EXPECT_EQ(ceilingLivingCeiling, 13);
    EXPECT_EQ(doorWestDoor, 14);
    EXPECT_EQ(windowTubularDaylightingDiffuser1, 15);
    EXPECT_EQ(windowNorthWindow, 16);
    EXPECT_EQ(windowEastWindow, 17);
    EXPECT_EQ(windowSouthWindow, 18);
    EXPECT_EQ(windowWestWindow, 19);
    EXPECT_EQ(Zone(1).SurfaceFirst, 7);
    EXPECT_EQ(Zone(1).SurfaceLast, 19);
    EXPECT_EQ(Zone(1).NonWindowSurfaceFirst, 7);
    EXPECT_EQ(Zone(1).NonWindowSurfaceLast, 14);
    EXPECT_EQ(Zone(1).WindowSurfaceFirst, 15);
    EXPECT_EQ(Zone(1).WindowSurfaceLast, 19);

    //  GARAGE ZONE:
    int wallGarageInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Interior"), DataSurfaces::Surface);
    int wallGarageEast = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:EastWall"), DataSurfaces::Surface);
    int wallGarageWest = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:WestWall"), DataSurfaces::Surface);
    int wallGarageFrontDoor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:FrontDoor"), DataSurfaces::Surface);
    int floorGarageFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Floor"), DataSurfaces::Surface);
    int ceilingGarageInterior = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Garage:Ceiling"), DataSurfaces::Surface);
    int intmassEVChargingStation = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EVChargingStation"), DataSurfaces::Surface);

    EXPECT_EQ(wallGarageInterior, 20);
    EXPECT_EQ(wallGarageEast, 21);
    EXPECT_EQ(wallGarageWest, 22);
    EXPECT_EQ(wallGarageFrontDoor, 23);
    EXPECT_EQ(floorGarageFloor, 24);
    EXPECT_EQ(ceilingGarageInterior, 25);
    EXPECT_EQ(intmassEVChargingStation, 26);
    EXPECT_EQ(Zone(2).SurfaceFirst, 20);
    EXPECT_EQ(Zone(2).SurfaceLast, 26);
    EXPECT_EQ(Zone(2).NonWindowSurfaceFirst, 20);
    EXPECT_EQ(Zone(2).NonWindowSurfaceLast, 26);
    EXPECT_EQ(Zone(2).WindowSurfaceFirst, 0);
    EXPECT_EQ(Zone(2).WindowSurfaceLast, -1);

    //  ATTIC ZONE:
    int wallEastGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastGable"), DataSurfaces::Surface);
    int wallWestGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestGable"), DataSurfaces::Surface);
    int wallNorthGable = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthGable"), DataSurfaces::Surface);
    int floorAtticLivingFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Attic:LivingFloor"), DataSurfaces::Surface);
    int floorAtticGarageFloor = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Attic:GarageFloor"), DataSurfaces::Surface);
    int roofNorthRoof1 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof1"), DataSurfaces::Surface);
    int roofSouthRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("SouthRoof"), DataSurfaces::Surface);
    int roofNorthRoof2 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof2"), DataSurfaces::Surface);
    int roofNorthRoof3 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof3"), DataSurfaces::Surface);
    int roofNorthRoof4 = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("NorthRoof4"), DataSurfaces::Surface);
    int roofEastRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("EastRoof"), DataSurfaces::Surface);
    int roofWestRoof = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("WestRoof"), DataSurfaces::Surface);
    int nonwindowTubularDaylightingDome1 =
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("TubularDaylightingDome1"), DataSurfaces::Surface);
    int windowAtticSkylight = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("AtticSkylight"), DataSurfaces::Surface);

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
    EXPECT_EQ(nonwindowTubularDaylightingDome1, 39);
    EXPECT_EQ(windowAtticSkylight, 40);
    EXPECT_EQ(Zone(3).SurfaceFirst, 27);
    EXPECT_EQ(Zone(3).SurfaceLast, 40);
    EXPECT_EQ(Zone(3).NonWindowSurfaceFirst, 27);
    EXPECT_EQ(Zone(3).NonWindowSurfaceLast, 39);
    EXPECT_EQ(Zone(3).WindowSurfaceFirst, 40);
    EXPECT_EQ(Zone(3).WindowSurfaceLast, 40);

    // Reporting (legacy) Order (zero-based)
    //  SHADING SURFACES:
    EXPECT_EQ(siteShadeShadeFlatShadeSurface, DataSurfaces::AllSurfaceListReportOrder[0]);
    EXPECT_EQ(mirSiteShadeFlatShadeSurface, DataSurfaces::AllSurfaceListReportOrder[1]);
    EXPECT_EQ(buildingShadeTiltedShadeSurface, DataSurfaces::AllSurfaceListReportOrder[2]);
    EXPECT_EQ(mirBuildingShadeTiltedShadeSurface, DataSurfaces::AllSurfaceListReportOrder[3]);
    EXPECT_EQ(zoneShadeLivingSouthShade001, DataSurfaces::AllSurfaceListReportOrder[4]);
    EXPECT_EQ(mirZoneShadeLivingSouthShade001, DataSurfaces::AllSurfaceListReportOrder[5]);

    //  LIVING ZONE:
    EXPECT_EQ(wallLivingNorth, DataSurfaces::AllSurfaceListReportOrder[6]);
    EXPECT_EQ(windowNorthWindow, DataSurfaces::AllSurfaceListReportOrder[7]);
    EXPECT_EQ(wallLivingEast, DataSurfaces::AllSurfaceListReportOrder[8]);
    EXPECT_EQ(windowEastWindow, DataSurfaces::AllSurfaceListReportOrder[9]);
    EXPECT_EQ(wallLivingSouth, DataSurfaces::AllSurfaceListReportOrder[10]);
    EXPECT_EQ(windowSouthWindow, DataSurfaces::AllSurfaceListReportOrder[11]);
    EXPECT_EQ(wallLivingWest, DataSurfaces::AllSurfaceListReportOrder[12]);
    EXPECT_EQ(windowWestWindow, DataSurfaces::AllSurfaceListReportOrder[13]);
    EXPECT_EQ(doorWestDoor, DataSurfaces::AllSurfaceListReportOrder[14]);
    EXPECT_EQ(wallLivingInterior, DataSurfaces::AllSurfaceListReportOrder[15]);
    EXPECT_EQ(floorLivingFloor, DataSurfaces::AllSurfaceListReportOrder[16]);
    EXPECT_EQ(ceilingLivingCeiling, DataSurfaces::AllSurfaceListReportOrder[17]);
    EXPECT_EQ(windowTubularDaylightingDiffuser1, DataSurfaces::AllSurfaceListReportOrder[18]);

    //  GARAGE ZONE:
    EXPECT_EQ(wallGarageInterior, DataSurfaces::AllSurfaceListReportOrder[19]);
    EXPECT_EQ(wallGarageEast, DataSurfaces::AllSurfaceListReportOrder[20]);
    EXPECT_EQ(wallGarageWest, DataSurfaces::AllSurfaceListReportOrder[21]);
    EXPECT_EQ(wallGarageFrontDoor, DataSurfaces::AllSurfaceListReportOrder[22]);
    EXPECT_EQ(floorGarageFloor, DataSurfaces::AllSurfaceListReportOrder[23]);
    EXPECT_EQ(ceilingGarageInterior, DataSurfaces::AllSurfaceListReportOrder[24]);
    EXPECT_EQ(intmassEVChargingStation, DataSurfaces::AllSurfaceListReportOrder[25]);

    //  ATTIC ZONE:
    EXPECT_EQ(wallEastGable, DataSurfaces::AllSurfaceListReportOrder[26]);
    EXPECT_EQ(wallWestGable, DataSurfaces::AllSurfaceListReportOrder[27]);
    EXPECT_EQ(wallNorthGable, DataSurfaces::AllSurfaceListReportOrder[28]);
    EXPECT_EQ(floorAtticLivingFloor, DataSurfaces::AllSurfaceListReportOrder[29]);
    EXPECT_EQ(floorAtticGarageFloor, DataSurfaces::AllSurfaceListReportOrder[30]);
    EXPECT_EQ(roofNorthRoof1, DataSurfaces::AllSurfaceListReportOrder[31]);
    EXPECT_EQ(nonwindowTubularDaylightingDome1, DataSurfaces::AllSurfaceListReportOrder[32]);
    EXPECT_EQ(roofSouthRoof, DataSurfaces::AllSurfaceListReportOrder[33]);
    EXPECT_EQ(roofNorthRoof2, DataSurfaces::AllSurfaceListReportOrder[34]);
    EXPECT_EQ(roofNorthRoof3, DataSurfaces::AllSurfaceListReportOrder[35]);
    EXPECT_EQ(roofNorthRoof4, DataSurfaces::AllSurfaceListReportOrder[36]);
    EXPECT_EQ(roofEastRoof, DataSurfaces::AllSurfaceListReportOrder[37]);
    EXPECT_EQ(windowAtticSkylight, DataSurfaces::AllSurfaceListReportOrder[38]);
    EXPECT_EQ(roofWestRoof, DataSurfaces::AllSurfaceListReportOrder[39]);
}
