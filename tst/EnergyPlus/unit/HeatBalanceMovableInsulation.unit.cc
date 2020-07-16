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

// EnergyPlus::HeatBalanceMovableInsulation Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceMovableInsulation.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceMovableInsulation;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceMovableInsulation_EvalOutsideMovableInsulation)
{

    int SurfNum;
    Real64 HMovInsul;
    int RoughIndexMovInsul;
    Real64 AbsExt;

    SurfNum = 1;
    DataSurfaces::Surface.allocate(SurfNum);
    DataSurfaces::Surface(SurfNum).SchedMovInsulExt = -1;
    DataSurfaces::Surface(SurfNum).MaterialMovInsulExt = 1;

    dataMaterial.Material.allocate(1);
    dataMaterial.Material(1).Resistance = 1.25;
    dataMaterial.Material(1).Roughness = 1;
    dataMaterial.Material(1).Group = 0;
    dataMaterial.Material(1).AbsorpSolar = 0.75;
    dataMaterial.Material(1).Trans = 0.25;
    dataMaterial.Material(1).ReflectSolBeamFront = 0.20;

    AbsExt = 0.0;
    EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);
    EXPECT_EQ(0.75, AbsExt);

    AbsExt = 0.0;
    dataMaterial.Material(1).Group = DataHeatBalance::WindowGlass;
    EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);
    EXPECT_EQ(0.55, AbsExt);

    AbsExt = 0.0;
    dataMaterial.Material(1).Group = DataHeatBalance::GlassEquivalentLayer;
    EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);
    EXPECT_EQ(0.55, AbsExt);
}

TEST_F(EnergyPlusFixture, HeatBalanceMovableInsulation_EvalInsideMovableInsulation)
{

    int SurfNum;
    Real64 HMovInsul;
    Real64 AbsExt;

    SurfNum = 1;
    DataSurfaces::Surface.allocate(SurfNum);
    DataSurfaces::Surface(SurfNum).SchedMovInsulInt = -1;
    DataSurfaces::Surface(SurfNum).MaterialMovInsulInt = 1;

    dataMaterial.Material.allocate(1);
    dataMaterial.Material(1).Resistance = 1.25;
    dataMaterial.Material(1).Roughness = 1;
    dataMaterial.Material(1).Group = 0;
    dataMaterial.Material(1).AbsorpSolar = 0.75;
    dataMaterial.Material(1).Trans = 0.25;
    dataMaterial.Material(1).ReflectSolBeamFront = 0.20;

    AbsExt = 0.0;
    EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsExt);
    EXPECT_EQ(0.75, AbsExt);

    AbsExt = 0.0;
    dataMaterial.Material(1).Group = DataHeatBalance::WindowGlass;
    EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsExt);
    EXPECT_EQ(0.55, AbsExt);

    AbsExt = 0.0;
    dataMaterial.Material(1).Group = DataHeatBalance::GlassEquivalentLayer;
    EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsExt);
    EXPECT_EQ(0.55, AbsExt);
}
TEST_F(EnergyPlusFixture, SurfaceControlMovableInsulation_InvalidWindowSimpleGlazingTest)
{

    std::string const idf_objects = delimited_string({

        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

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

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
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

        "  WindowMaterial:SimpleGlazingSystem,",
        "    SimpleGlazingSystem,     !- Name",
        "    2.8,                     !- U-Factor {W/m2-K}",
        "    0.7;                     !- Solar Heat Gain Coefficient",

        "  SurfaceControl:MovableInsulation,",
        "    Outside,                 !- Insulation Type",
        "    Zn001:Wall001,           !- Surface Name",
        "    SimpleGlazingSystem,     !- Material Name",
        "    ON;                      !- Schedule Name",

        "  Schedule:Compact,",
        "    ON,                      !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1.00;       !- Field 3",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // set error to false
    bool ErrorsFound(false);
    // set zone data
    DataGlobals::NumOfZones = 1;
    DataHeatBalance::Zone.allocate(1);
    DataHeatBalance::Zone(1).Name = "ZONE ONE";
    // get schedule data
    ScheduleManager::ProcessScheduleInput(state.files);
    // get materials data
    HeatBalanceManager::GetMaterialData(state.dataWindowEquivalentLayer, state.files, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(4, DataHeatBalance::TotMaterials);
    EXPECT_EQ(dataMaterial.Material(4).Group, DataHeatBalance::WindowSimpleGlazing);
    // get construction data
    HeatBalanceManager::GetConstructData(state.files, ErrorsFound);
    EXPECT_EQ(1, DataHeatBalance::TotConstructs);
    EXPECT_FALSE(ErrorsFound);
    // set relative coordinate
    SurfaceGeometry::GetGeometryParameters(state.files, ErrorsFound);
    SurfaceGeometry::CosZoneRelNorth.allocate(2);
    SurfaceGeometry::SinZoneRelNorth.allocate(2);
    SurfaceGeometry::CosZoneRelNorth = 1.0;
    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinZoneRelNorth = 0.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;
    // set surface data
    DataSurfaces::TotSurfaces = 1;
    SurfaceGeometry::SurfaceTmp.allocate(1);
    int SurfNum = 0;
    int TotHTSurfs = DataSurfaces::TotSurfaces = 1;
    Array1D_string const BaseSurfCls(1, {"WALL"});
    Array1D_int const BaseSurfIDs(1, {1});
    int NeedToAddSurfaces;
    // get heat tranfer surface data
    SurfaceGeometry::GetHTSurfaceData(state.files, ErrorsFound, SurfNum, TotHTSurfs, 0, 0, 0, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces);
    // get movable insulation object data
    SurfaceGeometry::GetMovableInsulationData(ErrorsFound);
    // check movable insulation material
    EXPECT_EQ(SurfaceGeometry::SurfaceTmp(1).BaseSurfName, "ZN001:WALL001");             // base surface name
    EXPECT_EQ(SurfaceGeometry::SurfaceTmp(1).MaterialMovInsulExt, 4);                    // index to movable insulation material
    EXPECT_EQ(dataMaterial.Material(4).Name, "SIMPLEGLAZINGSYSTEM");                 // name of movable insulation material
    EXPECT_EQ(dataMaterial.Material(4).Group, DataHeatBalance::WindowSimpleGlazing); // invalid material group type
    EXPECT_TRUE(ErrorsFound);                                                            // error found due to invalid material
}
} // namespace EnergyPlus
