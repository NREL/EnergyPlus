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

// EnergyPlus::ConvectionCoefficients unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers

// EnergyPlus Headers
#include <BaseboardElectric.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataLoopNode.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <SurfaceGeometry.hh>
#include <UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ConvectionCoefficients;

TEST_F(EnergyPlusFixture, ConvectionCoefficientsTest_ConvectionCofficients)
{

    Real64 DeltaTemp;     // [C] temperature difference between surface and air
    Real64 Height;        // [m] characteristic size
    Real64 SurfTemp;      // [C] surface temperature
    Real64 SupplyAirTemp; // [C] temperature of supply air into zone
    Real64 AirChangeRate; // [ACH] [1/hour] supply air ACH for zone
    int ZoneNum;          // index of zone for messaging
    Real64 Hc;

    DeltaTemp = 1.0;
    Height = 2.0;
    SurfTemp = 23.0;
    SupplyAirTemp = 35.0;
    AirChangeRate = 2.0;
    ZoneNum = 1;

    Hc = CalcBeausoleilMorrisonMixedAssistedWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(-1.19516, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedOpposingWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(1.8378, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedStableFloor(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(-4.3290, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedUnstableFloor(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(-4.24778, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedStableCeiling(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(-8.11959, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedUnstableCeiling(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum);
    EXPECT_NEAR(-8.09685, Hc, 0.0001);
}

TEST_F(EnergyPlusFixture, ConvectionCoefficientsTest_DynamicIntConvSurfaceClassification)
{

    std::string const idf_objects = delimited_string({
        " Version,8.8;",

        "  Zone,",
        "    Zone 1,                  !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0;                       !- Z Origin {m}",

        "  SurfaceConvectionAlgorithm:Inside,AdaptiveConvectionAlgorithm;",

        "  ZoneHVAC:EquipmentConnections,",
        "    Zone 1,                  !- Zone Name",
        "    Zone 1 Eq,               !- Zone Conditioning Equipment List Name",
        "    ,                        !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Node,           !- Zone Air Node Name",
        "    SPACE2-1 ret node;       !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    Zone 1 Eq,               !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 1 Object Type",
        "    Zone 1 Baseboard,        !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        " ZoneHVAC:Baseboard:Convective:Electric,",
        "    Zone 1 Baseboard,        !- Name",
        "    ,                        !- Availability Schedule Name",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    1000.0,                  !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    0.97;                    !- Efficiency",

        "  GlobalGeometryRules,LowerLeftCorner,CounterClockwise,World,World;",

        "  BuildingSurface:Detailed,",
        "    Vertical Wall,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Down Wall,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,-2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,-2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Up Wall,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Up Wall,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,10.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Down Wall,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,3.0,  !- X,Y,Z ==> Vertex 4 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.0,0.0,3.0;  !- X,Y,Z ==> Vertex 2 {m}",

        "  BuildingSurface:Detailed,",
        "    Vertical Roof,                 !- Name",
        "    ROOF,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Down Roof,                 !- Name",
        "    ROOF,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,-2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,-2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Up Roof,                 !- Name",
        "    ROOF,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Up Roof,                 !- Name",
        "    ROOF,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,10.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Down Roof,                 !- Name",
        "    ROOF,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,3.0,  !- X,Y,Z ==> Vertex 4 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.0,0.0,3.0;  !- X,Y,Z ==> Vertex 2 {m}",

        "  BuildingSurface:Detailed,",
        "    Vertical Floor,                 !- Name",
        "    FLOOR,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Down Floor,                 !- Name",
        "    FLOOR,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,-2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,-2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Tilted Up Floor,                 !- Name",
        "    FLOOR,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,2.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,2.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Up Floor,                 !- Name",
        "    FLOOR,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    10.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,10.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Horizontal Down Floor,                 !- Name",
        "    FLOOR,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    Zone 1,                  !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,3.0,  !- X,Y,Z ==> Vertex 4 {m}",
        "    10.0,10.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    10.0,0.0,3.0;  !- X,Y,Z ==> Vertex 2 {m}",

        "  Construction,",
        "    WALL-1,                  !- Name",
        "    GP01;                    !- Outside Layer",

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

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays();
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays();

    DataZoneEquipment::GetZoneEquipmentData1();

    BaseboardElectric::GetBaseboardInput();

    DataGlobals::ZoneSizingCalc = true;

    // Surface 1 is a vertical wall
    // Surface 2 is a wall tilted down (interior faces up)
    // Surface 3 is a wall tilted up (interior faces down)
    // Surface 4 is a horizontal wall facing up (interior faces down)
    // Surface 5 is a horizontal wall facing down (interior faces up)

    // apparently floors get added to the data structure before roofs regardless of idf order
    // Surface 6 is a vertical floor
    // Surface 7 is a floor tilted down (interior faces up)
    // Surface 8 is a floor tilted up (interior faces down) - this gets automatically flipped over so interior faces up
    // Surface 9 is a horizontal floor facing up (interior faces down) - this gets automatically flipped over so interior faces up
    // Surface 10 is a horizontal floor facing down (interior faces up)

    // Surface 11 is a vertical roof
    // Surface 12 is a roof tilted down (interior faces up) - this gets automatically flipped over so interior faces down
    // Surface 13 is a roof tilted up (interior faces down)
    // Surface 14 is a horizontal roof facing up (interior faces down)
    // Surface 15 is a horizontal roof facing down (interior faces up) - this gets automatically flipped over so interior faces down

    // Surface temps are 20C
    for (int surf = 1; surf <= DataSurfaces::TotSurfaces; ++surf) {
        DataHeatBalSurface::TH(2, 1, surf) = 20.0;
    }

    // Case 1 - Zone air warmer than surfaces
    DataHeatBalFanSys::MAT(1) = 30.0;

    DynamicIntConvSurfaceClassification(1);
    EXPECT_EQ(DataSurfaces::Surface(1).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(2);
    EXPECT_EQ(DataSurfaces::Surface(2).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(3);
    EXPECT_EQ(DataSurfaces::Surface(3).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(4);
    EXPECT_EQ(DataSurfaces::Surface(4).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(5);
    EXPECT_EQ(DataSurfaces::Surface(5).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    // vertical floor is currently not a valid case, so returns zero with a severe error
    DynamicIntConvSurfaceClassification(6);
    EXPECT_EQ(DataSurfaces::Surface(6).IntConvClassification, 0);

    DynamicIntConvSurfaceClassification(7);
    EXPECT_EQ(DataSurfaces::Surface(7).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(8);
    EXPECT_EQ(DataSurfaces::Surface(8).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(9);
    EXPECT_EQ(DataSurfaces::Surface(9).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(10);
    EXPECT_EQ(DataSurfaces::Surface(10).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(11);
    EXPECT_EQ(DataSurfaces::Surface(11).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(12);
    EXPECT_EQ(DataSurfaces::Surface(12).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(13);
    EXPECT_EQ(DataSurfaces::Surface(13).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(14);
    EXPECT_EQ(DataSurfaces::Surface(14).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(15);
    EXPECT_EQ(DataSurfaces::Surface(15).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    // Case 2 - Zone air colder than surfaces
    DataHeatBalFanSys::MAT(1) = 10.0;

    DynamicIntConvSurfaceClassification(1);
    EXPECT_EQ(DataSurfaces::Surface(1).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(2);
    EXPECT_EQ(DataSurfaces::Surface(2).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(3);
    EXPECT_EQ(DataSurfaces::Surface(3).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(4);
    EXPECT_EQ(DataSurfaces::Surface(4).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(5);
    EXPECT_EQ(DataSurfaces::Surface(5).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    // vertical floor is currently not a valid case, so returns zero with a severe error
    DynamicIntConvSurfaceClassification(6);
    EXPECT_EQ(DataSurfaces::Surface(6).IntConvClassification, 0);

    DynamicIntConvSurfaceClassification(7);
    EXPECT_EQ(DataSurfaces::Surface(7).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(8);
    EXPECT_EQ(DataSurfaces::Surface(8).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(9);
    EXPECT_EQ(DataSurfaces::Surface(9).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(10);
    EXPECT_EQ(DataSurfaces::Surface(10).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(11);
    EXPECT_EQ(DataSurfaces::Surface(11).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(12);
    EXPECT_EQ(DataSurfaces::Surface(12).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(13);
    EXPECT_EQ(DataSurfaces::Surface(13).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(14);
    EXPECT_EQ(DataSurfaces::Surface(14).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(15);
    EXPECT_EQ(DataSurfaces::Surface(15).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);
}

TEST_F(EnergyPlusFixture, ConvectionCoefficientsTest_EvaluateIntHcModelsFisherPedersen)
{

    int SurfNum;
    int ConvModelEquationNum;
    Real64 Hc;
    Real64 HcExpectedValue;

    SurfNum = 1;

    DataSurfaces::Surface.allocate( 1 );
    DataHeatBalance::Zone.allocate( 1 );
    DataLoopNode::Node.allocate( 1 );

    DataSurfaces::Surface( SurfNum ).Zone = 1;
    DataHeatBalance::Zone( 1 ).SystemZoneNodeNumber = 1;
    DataHeatBalance::Zone( 1 ).Multiplier = 1.0;
    DataHeatBalance::Zone( 1 ).ListMultiplier = 1.0;
    DataHeatBalance::Zone( 1 ).Volume = 1.0;
    DataEnvironment::OutBaroPress = 101325.0;
    DataLoopNode::Node( 1 ).Temp = 20.0;
    DataLoopNode::Node( 1 ).MassFlowRate = 1.17653/3600.0;

    // Test 1: Floor Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserFloor;
    Hc = 0.0;
    HcExpectedValue = 3.955;
    DataSurfaces::Surface(SurfNum).TAirRef = 0;

    EvaluateIntHcModels( SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 2: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserCeiling;
    Hc = 0.0;
    HcExpectedValue = 6.333;
    DataSurfaces::Surface(SurfNum).TAirRef = 0;

    EvaluateIntHcModels( SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 3: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserWalls;
    Hc = 0.0;
    HcExpectedValue = 2.220;
    DataSurfaces::Surface(SurfNum).TAirRef = 0;

    EvaluateIntHcModels( SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

}

TEST_F(EnergyPlusFixture, ConvectionCoefficientsTest_EvaluateHnModels)
{
    
    int SurfNum;
    Real64 DeltaTemp;
    Real64 CosineTilt;
    Real64 Hn;
    Array1D<Real64> SurfTemp;
    Array1D<Real64> HcIn;
    Array1D<Real64> Vhc;

    SurfNum = 1;
    DataSurfaces::Surface.allocate(SurfNum);
    DataSurfaces::Surface(SurfNum).Zone = 1;
    DataRoomAirModel::AirModel.allocate(1);
    EnergyPlus::DataHeatBalance::TempEffBulkAir.allocate(1);
    EnergyPlus::DataHeatBalance::TempEffBulkAir(1) = 1.0;
    SurfTemp.allocate(1);
    HcIn.allocate(1);
    Vhc.allocate(1);
    
    // Test 1: CalcWaltonUnstableHorizontalOrTilt calculation for Hn
    DeltaTemp = 1.0;
    CosineTilt = 1.0;
    Hn = 0.0;
    Hn = CalcWaltonUnstableHorizontalOrTilt(DeltaTemp, CosineTilt);
    EXPECT_NEAR(Hn, 1.520, 0.001);

    //Test 2/3: CalcDetailedHcInForDVModel calculation for Hn
    DataSurfaces::Surface(SurfNum).HeatTransSurf = true;
    DataSurfaces::Surface(SurfNum).TAirRef = DataSurfaces::AdjacentAirTemp;
    DataSurfaces::Surface(SurfNum).IntConvCoeff = 0.0;
    DataRoomAirModel::AirModel(DataSurfaces::Surface(SurfNum).Zone).AirModelType = DataRoomAirModel::RoomAirModel_UCSDDV;
    DataSurfaces::Surface(SurfNum).CosTilt = 1.0;
    SurfTemp(1) = 0.0;
    HcIn(1) = 0.0;
    CalcDetailedHcInForDVModel(SurfNum, SurfTemp, HcIn);
    Hn = HcIn(1);
    EXPECT_NEAR(Hn, 1.520, 0.001);

    DataSurfaces::Surface(SurfNum).HeatTransSurf = true;
    DataSurfaces::Surface(SurfNum).TAirRef = DataSurfaces::AdjacentAirTemp;
    DataSurfaces::Surface(SurfNum).IntConvCoeff = 0.0;
    DataRoomAirModel::AirModel(DataSurfaces::Surface(SurfNum).Zone).AirModelType = DataRoomAirModel::RoomAirModel_UCSDCV;
    DataSurfaces::Surface(SurfNum).CosTilt = 1.0;
    SurfTemp(1) = 0.0;
    HcIn(1) = 0.0;
    Vhc(1) = 1.0;
    CalcDetailedHcInForDVModel(SurfNum, SurfTemp, HcIn, Vhc);
    Hn = HcIn(1);
    EXPECT_NEAR(Hn, 4.347, 0.001);

}

