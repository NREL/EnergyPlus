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

// EnergyPlus::ConvectionCoefficients unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers

// EnergyPlus Headers
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ConvectionCoefficients;

class ConvectionCoefficientsFixture : public EnergyPlus::EnergyPlusFixture
{
public:

protected:
    void SetUp() override
    {
        EnergyPlus::EnergyPlusFixture::SetUp();
    }

    void TearDown() override
    {
        EnergyPlus::EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
    };

    std::string getIDFString()
    {
        std::vector<std::string> idf_lines = {

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

        };

        return delimited_string(idf_lines);
    }
};

TEST_F(ConvectionCoefficientsFixture, ConvectionCofficients)
{

    Real64 DeltaTemp;     // [C] temperature difference between surface and air
    Real64 Height;        // [m] characteristic size
    Real64 SurfTemp;      // [C] surface temperature
    Real64 SupplyAirTemp; // [C] temperature of supply air into zone
    Real64 AirChangeRate; // [ACH] [1/hour] supply air ACH for zone
    Real64 Hc;

    DeltaTemp = 1.0;
    Height = 2.0;
    SurfTemp = 23.0;
    SupplyAirTemp = 35.0;
    AirChangeRate = 2.0;

    Hc = CalcBeausoleilMorrisonMixedAssistedWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(-1.19516, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedOpposingWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(1.8378, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedStableFloor(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(-4.3290, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedUnstableFloor(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(-4.24778, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedStableCeiling(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(-8.11959, Hc, 0.0001);

    Hc = CalcBeausoleilMorrisonMixedUnstableCeiling(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    EXPECT_NEAR(-8.09685, Hc, 0.0001);
}

TEST_F(ConvectionCoefficientsFixture, DynamicIntConvSurfaceClassification)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

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

    DynamicIntConvSurfaceClassification(*state, 1);
    EXPECT_EQ(DataSurfaces::Surface(1).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(*state, 2);
    EXPECT_EQ(DataSurfaces::Surface(2).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 3);
    EXPECT_EQ(DataSurfaces::Surface(3).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 4);
    EXPECT_EQ(DataSurfaces::Surface(4).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 5);
    EXPECT_EQ(DataSurfaces::Surface(5).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    // vertical floor is currently not a valid case, so returns zero with a severe error
    DynamicIntConvSurfaceClassification(*state, 6);
    EXPECT_EQ(DataSurfaces::Surface(6).IntConvClassification, 0);

    DynamicIntConvSurfaceClassification(*state, 7);
    EXPECT_EQ(DataSurfaces::Surface(7).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 8);
    EXPECT_EQ(DataSurfaces::Surface(8).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 9);
    EXPECT_EQ(DataSurfaces::Surface(9).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(*state, 10);
    EXPECT_EQ(DataSurfaces::Surface(10).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(*state, 11);
    EXPECT_EQ(DataSurfaces::Surface(11).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(*state, 12);
    EXPECT_EQ(DataSurfaces::Surface(12).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 13);
    EXPECT_EQ(DataSurfaces::Surface(13).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 14);
    EXPECT_EQ(DataSurfaces::Surface(14).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(*state, 15);
    EXPECT_EQ(DataSurfaces::Surface(15).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    // Case 2 - Zone air colder than surfaces
    DataHeatBalFanSys::MAT(1) = 10.0;

    DynamicIntConvSurfaceClassification(*state, 1);
    EXPECT_EQ(DataSurfaces::Surface(1).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(*state, 2);
    EXPECT_EQ(DataSurfaces::Surface(2).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 3);
    EXPECT_EQ(DataSurfaces::Surface(3).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 4);
    EXPECT_EQ(DataSurfaces::Surface(4).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 5);
    EXPECT_EQ(DataSurfaces::Surface(5).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    // vertical floor is currently not a valid case, so returns zero with a severe error
    DynamicIntConvSurfaceClassification(*state, 6);
    EXPECT_EQ(DataSurfaces::Surface(6).IntConvClassification, 0);

    DynamicIntConvSurfaceClassification(*state, 7);
    EXPECT_EQ(DataSurfaces::Surface(7).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 8);
    EXPECT_EQ(DataSurfaces::Surface(8).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableTilted);

    DynamicIntConvSurfaceClassification(*state, 9);
    EXPECT_EQ(DataSurfaces::Surface(9).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(*state, 10);
    EXPECT_EQ(DataSurfaces::Surface(10).IntConvClassification, DataSurfaces::InConvClass_A3_UnstableHoriz);

    DynamicIntConvSurfaceClassification(*state, 11);
    EXPECT_EQ(DataSurfaces::Surface(11).IntConvClassification, DataSurfaces::InConvClass_A3_VertWalls);

    DynamicIntConvSurfaceClassification(*state, 12);
    EXPECT_EQ(DataSurfaces::Surface(12).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 13);
    EXPECT_EQ(DataSurfaces::Surface(13).IntConvClassification, DataSurfaces::InConvClass_A3_StableTilted);

    DynamicIntConvSurfaceClassification(*state, 14);
    EXPECT_EQ(DataSurfaces::Surface(14).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);

    DynamicIntConvSurfaceClassification(*state, 15);
    EXPECT_EQ(DataSurfaces::Surface(15).IntConvClassification, DataSurfaces::InConvClass_A3_StableHoriz);
}

TEST_F(ConvectionCoefficientsFixture, EvaluateIntHcModelsFisherPedersen)
{

    int SurfNum;
    int ConvModelEquationNum;
    Real64 Hc;
    Real64 HcExpectedValue;

    SurfNum = 1;

    DataSurfaces::TotSurfaces = 1;
    state->dataGlobal->NumOfZones = 1;
    DataSurfaces::Surface.allocate( 1 );
    state->dataConstruction->Construct.allocate( 1 );
    DataHeatBalance::Zone.allocate( 1 );
    DataLoopNode::Node.allocate( 1 );

    DataSurfaces::Surface( SurfNum ).Zone = 1;
    DataSurfaces::Surface( SurfNum ).Construction = 1;
    DataSurfaces::Surface(SurfNum).TAirRef = 0;
    state->dataConstruction->Construct( 1 ).TypeIsWindow = false;
    DataHeatBalance::Zone( 1 ).SystemZoneNodeNumber = 1;
    DataHeatBalance::Zone( 1 ).Multiplier = 1.0;
    DataHeatBalance::Zone( 1 ).ListMultiplier = 1.0;
    state->dataEnvrn->OutBaroPress = 101325.0;
    DataLoopNode::Node( 1 ).Temp = 20.0;
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    for (int surf = 1; surf <= DataSurfaces::TotSurfaces; ++surf) {
        DataHeatBalSurface::TH(2, 1, surf) = 20.0;
    }

    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 30.0;


    // Case 1 - Low ACH (should default to CalcASHRAETARPNatural)
    Real64 ACH = 0.25;
    DataHeatBalance::Zone( 1 ).Volume = 125.0;
    DataLoopNode::Node( 1 ).MassFlowRate = 1.17653/3600.0 * DataHeatBalance::Zone( 1 ).Volume * ACH;


    // Test 1: Floor Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserFloor;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = -1;

    HcExpectedValue = CalcASHRAETARPNatural(DataHeatBalSurface::TH(2, 1, 1), DataHeatBalFanSys::MAT(1), -DataSurfaces::Surface( SurfNum ).CosTilt);

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 2: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserCeiling;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = 1;

    HcExpectedValue = CalcASHRAETARPNatural(DataHeatBalSurface::TH(2, 1, 1), DataHeatBalFanSys::MAT(1), -DataSurfaces::Surface( SurfNum ).CosTilt);

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 3: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserWalls;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = 0;

    HcExpectedValue = CalcASHRAETARPNatural(DataHeatBalSurface::TH(2, 1, 1), DataHeatBalFanSys::MAT(1), -DataSurfaces::Surface( SurfNum ).CosTilt);

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );


    // Case 2 - High ACH
    ACH = 3.1;
    DataHeatBalance::Zone( 1 ).Volume = 125.0;
    DataLoopNode::Node( 1 ).MassFlowRate = 1.17653/3600.0 * DataHeatBalance::Zone( 1 ).Volume * ACH;

    // Test 1: Floor Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserFloor;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = -1;

    HcExpectedValue = 4.122;

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 2: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserCeiling;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = 1;

    HcExpectedValue = 9.476;

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );

    // Test 3: Ceiling Diffuser Model
    ConvModelEquationNum = HcInt_FisherPedersenCeilDiffuserWalls;
    Hc = 0.0;
    DataSurfaces::Surface( SurfNum ).CosTilt = 0;

    HcExpectedValue = 3.212;

    EvaluateIntHcModels(*state, SurfNum, ConvModelEquationNum, Hc );
    EXPECT_EQ( DataSurfaces::Surface(SurfNum).TAirRef, DataSurfaces::ZoneMeanAirTemp );
    EXPECT_NEAR( Hc, HcExpectedValue, 0.1 );
}

TEST_F(ConvectionCoefficientsFixture, EvaluateHnModels)
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
    state->dataRoomAirMod->AirModel.allocate(1);
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
    state->dataRoomAirMod->AirModel(DataSurfaces::Surface(SurfNum).Zone).AirModelType = DataRoomAirModel::RoomAirModel::UCSDDV;
    DataSurfaces::Surface(SurfNum).CosTilt = 1.0;
    SurfTemp(1) = 0.0;
    HcIn(1) = 0.0;
    CalcDetailedHcInForDVModel(*state, SurfNum, SurfTemp, HcIn);
    Hn = HcIn(1);
    EXPECT_NEAR(Hn, 1.520, 0.001);

    DataSurfaces::Surface(SurfNum).HeatTransSurf = true;
    DataSurfaces::Surface(SurfNum).TAirRef = DataSurfaces::AdjacentAirTemp;
    DataSurfaces::Surface(SurfNum).IntConvCoeff = 0.0;
    state->dataRoomAirMod->AirModel(DataSurfaces::Surface(SurfNum).Zone).AirModelType = DataRoomAirModel::RoomAirModel::UCSDCV;
    DataSurfaces::Surface(SurfNum).CosTilt = 1.0;
    SurfTemp(1) = 0.0;
    HcIn(1) = 0.0;
    Vhc(1) = 1.0;
    CalcDetailedHcInForDVModel(*state, SurfNum, SurfTemp, HcIn, Vhc);
    Hn = HcIn(1);
    EXPECT_NEAR(Hn, 4.347, 0.001);

}

TEST_F(ConvectionCoefficientsFixture, TestCalcZoneSystemACH)
{

    int ZoneNum = 1;
    int TotalNumberofZones = 1;
    Real64 ACHAnswer;
    Real64 ACHExpected;

    if (!allocated(DataHeatBalance::Zone)) DataHeatBalance::Zone.allocate(TotalNumberofZones);
    DataHeatBalance::Zone(ZoneNum).Volume = 100.0;
    DataHeatBalance::Zone(ZoneNum).SystemZoneNodeNumber = 1;
    state->dataGlobal->BeginEnvrnFlag = false;
    DataHeatBalance::Zone(ZoneNum).Multiplier = 1.0;
    DataHeatBalance::Zone(ZoneNum).ListMultiplier = 1.0;
    state->dataEnvrn->OutBaroPress = 101400.0;
    Real64 ZoneNode = DataHeatBalance::Zone(ZoneNum).SystemZoneNodeNumber;

    // Test 1: Node not allocated, returns a zero ACH
    if (allocated(EnergyPlus::DataLoopNode::Node)) EnergyPlus::DataLoopNode::Node.deallocate();
    ACHExpected = 0.0;
    ACHAnswer = CalcZoneSystemACH(*state, ZoneNum);
    EXPECT_NEAR(ACHExpected, ACHAnswer, 0.0001);

    // Test 2: Node now allocated, needs to return a proper ACH
    EnergyPlus::DataLoopNode::Node.allocate(DataHeatBalance::Zone(ZoneNum).SystemZoneNodeNumber);
    EnergyPlus::DataLoopNode::Node(ZoneNode).Temp = 20.0;
    EnergyPlus::DataLoopNode::Node(ZoneNode).MassFlowRate = 0.2;
    ACHExpected = 6.11506;
    ACHAnswer = CalcZoneSystemACH(*state, ZoneNum);
    EXPECT_NEAR(ACHExpected, ACHAnswer, 0.0001);

}

TEST_F(ConvectionCoefficientsFixture, TestCalcFisherPedersenCeilDiffuserNatConv)
{

    Real64 Hforced;
    Real64 ACH;
    Real64 Tsurf;
    Real64 Tair;
    Real64 cosTilt;
    Real64 humRat;
    Real64 height;
    bool isWindow;
    Real64 ExpectedHconv;
    Real64 CalculatedHconv;

    state->dataEnvrn->OutBaroPress = 101325.0;

    // Test 1: Non-window, all natural
    Hforced = 10.0;
    ACH = 0.25;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 1.2994;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserNatConv(*state, Hforced,ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 2: Window, all natural
    Hforced = 10.0;
    ACH = 0.25;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = true;
    ExpectedHconv = 0.8067;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserNatConv(*state, Hforced,ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 3: Non-window, all natural
    Hforced = 10.0;
    ACH = 0.5;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 1.2994;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserNatConv(*state, Hforced,ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 4: Non-window, transition
    Hforced = 10.0;
    ACH = 0.75;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 2.16942;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserNatConv(*state, Hforced,ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 5: Non-window, all ceiling diffuser correlation
    Hforced = 10.0;
    ACH = 3.0;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 10.0;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserNatConv(*state, Hforced,ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

}

TEST_F(ConvectionCoefficientsFixture, TestCalcFisherPedersenCeilDiffuserCorrelations)
{

    Real64 ACH;
    Real64 Tsurf;
    Real64 Tair;
    Real64 cosTilt;
    Real64 humRat;
    Real64 height;
    bool isWindow;
    Real64 ExpectedHconv;
    Real64 CalculatedHconv;

    state->dataEnvrn->OutBaroPress = 101325.0;

    // Test 1: Forced Convection All Correlations (Floor, Ceiling, Wall)
    ACH = 3.3;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 4.13721502661183;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserFloor(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    ExpectedHconv = 9.70692167003631;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserCeiling(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    ExpectedHconv = 3.28943537910741;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserWalls(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 2: Natural Convection All Correlations (Floor, Ceiling, Wall)--note, all should give same answer because of how variables are set
    ACH = 0.25;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 1.2994;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserFloor(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    CalculatedHconv = CalcFisherPedersenCeilDiffuserCeiling(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    CalculatedHconv = CalcFisherPedersenCeilDiffuserWalls(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

    // Test 3: Mixed Covection All Correlations (Floor, Ceiling, Wall)
    ACH = 1.75;
    Tsurf = 23.0;
    Tair = 18.0;
    cosTilt = 1.0;
    humRat = 0.08;
    height = 1.0;
    isWindow = false;
    ExpectedHconv = 2.70653;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserFloor(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    ExpectedHconv = 5.32826;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserCeiling(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);
    ExpectedHconv = 2.23620;
    CalculatedHconv = CalcFisherPedersenCeilDiffuserWalls(*state, ACH,Tsurf,Tair,cosTilt,humRat,height,isWindow);
    EXPECT_NEAR(ExpectedHconv, CalculatedHconv, 0.0001);

}

TEST_F(ConvectionCoefficientsFixture, TestWindward)
{

    bool AgainstWind;

    Real64 CosTilt;
    Real64 Azimuth;
    Real64 WindDirection;

    // Test 1: Horizontal surface
    CosTilt = 1.0;
    Azimuth = 180.0;
    WindDirection = 180.0;
    AgainstWind = Windward(CosTilt,Azimuth,WindDirection);
    EXPECT_TRUE(AgainstWind);

    // Test 2: Vertical surface, Azimuth and WindDiretion within 90 degrees of one another (windward or against wind)
    CosTilt = 0.5;
    Azimuth = 269.0;
    WindDirection = 180.0;
    AgainstWind = Windward(CosTilt,Azimuth,WindDirection);
    EXPECT_TRUE(AgainstWind);

    // Test 3: Vertical surface, Azimuth and WindDiretion not within 90 degrees of one another (leeward or not against wind)
    CosTilt = 0.5;
    Azimuth = 271.0;
    WindDirection = 180.0;
    AgainstWind = Windward(CosTilt,Azimuth,WindDirection);
    EXPECT_FALSE(AgainstWind);

}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedAssistedWall)
{
    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedAssistedWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 2.667, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedAssistedWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedAssistedWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedOpposingWall)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedOpposingWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 2.673, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedOpposingWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedOpposingWall(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedStableFloor)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedStableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 0.962, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedStableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedStableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedUnstableFloor)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedUnstableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 3.583, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedUnstableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedUnstableFloor(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedStableCeiling)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedStableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 0.937, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedStableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedStableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}

TEST_F(ConvectionCoefficientsFixture, CalcBeausoleilMorrisonMixedUnstableCeiling)
{

    std::string const idf_objects = this->getIDFString();

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataEnvrn->OutBaroPress = 101325.0;

    bool errorsFound(false);
    HeatBalanceManager::GetProjectControlData(*state, errorsFound); // read project control data
    EXPECT_FALSE(errorsFound);                              // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, errorsFound); // read material data
    EXPECT_FALSE(errorsFound);                        // expect no errors

    errorsFound = false;
    HeatBalanceManager::GetConstructData(*state, errorsFound); // read construction data
    EXPECT_FALSE(errorsFound);                         // expect no errors

    HeatBalanceManager::GetZoneData(*state, errorsFound);
    ASSERT_FALSE(errorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, errorsFound);
    ASSERT_FALSE(errorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    HeatBalanceSurfaceManager::AllocateSurfaceHeatBalArrays(*state);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    BaseboardElectric::GetBaseboardInput(*state);

    state->dataGlobal->ZoneSizingCalc = true;

    Real64 tolerance = 1E-3;

    // Calculate convection coefficient DeltaT and Height != 0
    Real64 deltaTemp = 10.0;
    Real64 height = 1.0;
    Real64 surfTemp = 20.0;
    int zoneNum = 1;
    Real64 convCoeff = CalcBeausoleilMorrisonMixedUnstableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 3.581, tolerance);

    // DeltaT = 0 Error Path
    deltaTemp = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedUnstableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);

    // Height = 0 Error Path
    deltaTemp = 10.0;
    height = 0.0;
    convCoeff = CalcBeausoleilMorrisonMixedUnstableCeiling(*state, deltaTemp, height, surfTemp, zoneNum);
    EXPECT_NEAR(convCoeff, 9.999, tolerance);
}


TEST_F(ConvectionCoefficientsFixture, ConvectionCoefficientsTest_CalcASHRAESimpleIntConvCoeff)
{
    // Unit test for the function CalcASHRAESimpleIntConvCoeff, used to determine the Convection Coefficient
    // for the Ashrae Simple algorithm setting

    Real64 Tsurf;
    Real64 Tamb;
    Real64 CosTilt;
    Real64 ConvectionCoefficient;
    Real64 ExpectedCoefficient;

    // Scenario: Vertical Surface
    // Hcov expected = 3.076
    // Delta_T is not relevant for this calculation

    Tsurf = 30.0;
    Tamb = 20.0;
    CosTilt = 0.0; // cos(90 degrees)
    ExpectedCoefficient = 3.076;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: Vertical Surface, CosTilt not exactly zero
    // Hcov expected = 3.076

    Tsurf = 19.0;
    Tamb = 20.0;
    CosTilt = 0.0001; // cos(90 degrees)
    ExpectedCoefficient = 3.076;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: Vertical Surface, Zero Delta T
    // Hcov expected = 3.076

    Tsurf = 23.0;
    Tamb = 23.0;
    CosTilt = 0; // cos(90 degrees)
    ExpectedCoefficient = 3.076;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    // Scenario: Horizontal Surface with reduced convection
    // Hcov expected = 0.948
    // A negative Delta_T is required for reduced convection

    Tsurf = 30.0;
    Tamb = 20.0;
    CosTilt = 0.9239; // cos(22.5 degrees)
    ExpectedCoefficient = 0.948;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);


    //Scenario: Horizontal surface with enhanced convection:
    // Hcov expected = 4.040
    // A positive Delta_T is required for enhanced convection

    Tsurf = 20.0;
    Tamb = 30.0;
    CosTilt = 0.9239; // cos(22.5 degrees)
    ExpectedCoefficient = 4.040;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: horizontal surface, enhanced convection
    // 180 degree surface, negative Delta_T
    // Hcov expected = 4.040

    Tsurf = 30.0;
    Tamb = 20.0;
    CosTilt = -1; // cos(180 degrees)
    ExpectedCoefficient = 4.040;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: horizontal surface, reduced convection
    // 180 degree surface, positive Delta_T
    // Hcov expected = 0.948

    Tsurf = 20.0;
    Tamb = 30.0;
    CosTilt = -1; // cos(180 degrees)
    ExpectedCoefficient = 0.948;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: tilted surface with reduced convection
    // Hcov expected = 2.281
    // A negative Delta_T is required for reduced convection

    Tsurf = 30.0;
    Tamb = 20.0;
    CosTilt = 0.707; // cos(45 degrees)
    ExpectedCoefficient = 2.281;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);

    //Scenario: tilted surface with enhanced convection
    // Hcov expected = 3.870

    Tsurf = 20.0;
    Tamb = 30.0;
    CosTilt = 0.707; // cos(45 degrees)
    ExpectedCoefficient = 3.870;

    ConvectionCoefficient = CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, CosTilt);
    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);
}

TEST_F(ConvectionCoefficientsFixture, ConvectionCoefficientsTest_HConvInDependence)
{
    Real64 ConvectionCoefficient;
    Real64 ExpectedCoefficient = 3.076;

    DataSurfaces::Surface.allocate(1);
    DataSurfaces::Surface(1).CosTilt = 0;

    DataHeatBalance::HConvIn.allocate(1);

    CalcASHRAESimpleIntConvCoeff(*state, 1, 20.0, 30.0);

    ConvectionCoefficient = DataHeatBalance::HConvIn(1);

    EXPECT_EQ(ConvectionCoefficient, ExpectedCoefficient);
}

TEST_F(EnergyPlusFixture, AdaptiveModelSelections_ProperConstruction)
{
    // This unit test checks to make sure the InsideFaceAdaptiveConvectionAlgo and OutsideFaceAdaptiveConvectionAlgo objects default construct their
    // members to the correct algorithm integer identifiers

    std::string const idf_objects = delimited_string({
        "SurfaceConvectionAlgorithm:Inside,AdaptiveConvectionAlgorithm;",
        "SurfaceConvectionAlgorithm:Outside,AdaptiveConvectionAlgorithm;",

        "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections,",
        "Default Algorithm;       !- Name",

        "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections,",
        "Default algorithms;      !- Name"
    });

    ASSERT_TRUE(process_idf(idf_objects));

    int algorithm_identifier;

    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FohannoPolidoriVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondUnstableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KaradagChilledCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq6NonHeatedWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FohannoPolidoriVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq5WallNearHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FisherPedersenCeilDiffuserCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedAssistingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedOppossingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);

    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowWindward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowLeeward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_ClearRoof);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalASHRAEVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonUnstableHorizontalOrTilt);

}

TEST_F(EnergyPlusFixture, AdaptiveModelSelections_Implicit)
{
    // This unit test checks to make sure the InsideFaceAdaptiveConvectionAlgo and OutsideFaceAdaptiveConvectionAlgo objects assign their
    // members to the correct algorithm integer identifiers inside the GetUserConvectionCoefficients() function
    std::string const idf_objects = delimited_string({
                                                         "SurfaceConvectionAlgorithm:Inside,AdaptiveConvectionAlgorithm;",
                                                         "SurfaceConvectionAlgorithm:Outside,AdaptiveConvectionAlgorithm;",

                                                         "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections,",
                                                         "Default Algorithm,       !- Name",
                                                         "FohannoPolidoriVerticalWall;  !- Simple Buoyancy Vertical Wall Equation Source",

                                                         "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections,",
                                                         "Default algorithms,      !- Name",
                                                         "TARPWindward;            !- Wind Convection Windward Vertical Wall Equation Source",

                                                     });

    ASSERT_TRUE(process_idf(idf_objects));

    DataHeatBalSurface::TempSurfInTmp.allocate(6);
    DataHeatBalSurface::TempSurfInTmp(1) = 15.0;
    DataHeatBalSurface::TempSurfInTmp(2) = 20.0;
    DataHeatBalSurface::TempSurfInTmp(3) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(4) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(5) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(6) = 25.0;
    ConvectionCoefficients::InitInteriorConvectionCoeffs(*state, DataHeatBalSurface::TempSurfInTmp);

    int algorithm_identifier;

    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FohannoPolidoriVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondUnstableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KaradagChilledCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq6NonHeatedWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FohannoPolidoriVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq5WallNearHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FisherPedersenCeilDiffuserCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedAssistingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedOppossingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);

    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowWindward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowLeeward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_ClearRoof);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalASHRAEVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonUnstableHorizontalOrTilt);

    DataHeatBalSurface::TempSurfInTmp.deallocate();
}


TEST_F(EnergyPlusFixture, AdaptiveModelSelections_ExplicitSelection)
{
    // This unit test checks to make sure the InsideFaceAdaptiveConvectionAlgo and OutsideFaceAdaptiveConvectionAlgo objects assign their
    // members to the correct algorithm integer identifiers inside the GetUserConvectionCoefficients() function when non-default assignments
    // are specified in the idf
    std::string const idf_objects = delimited_string({

                                                         "SurfaceConvectionAlgorithm:Inside,AdaptiveConvectionAlgorithm;",
                                                         "SurfaceConvectionAlgorithm:Outside,AdaptiveConvectionAlgorithm;",

                                                         "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections,",
                                                         "Default Algorithm,       !- Name",
                                                         "ISO15099Windows,         !- Simple Buoyancy Vertical Wall Equation Source",
                                                         ",                        !- Simple Buoyancy Vertical Wall User Curve Name",
                                                         "AlamdariHammondStableHorizontal,  !- Simple Buoyancy Stable Horizontal Equation Source",
                                                         ",                        !- Simple Buoyancy Stable Horizontal Equation User Curve Name",
                                                         "AlamdariHammondUnstableHorizontal,  !- Simple Buoyancy Unstable Horizontal Equation Source",
                                                         ",                        !- Simple Buoyancy Unstable Horizontal Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Simple Buoyancy Stable Tilted Equation Source",
                                                         ",                        !- Simple Buoyancy Stable Tilted Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Simple Buoyancy Unstable Tilted Equation Source",
                                                         ",                        !- Simple Buoyancy Unstable Tilted Equation User Curve Name",
                                                         "ISO15099Windows,         !- Simple Buoyancy Windows Equation Source",
                                                         ",                        !- Simple Buoyancy Windows Equation User Curve Name",
                                                         "KhalifaEq3WallAwayFromHeat,  !- Floor Heat Ceiling Cool Vertical Wall Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Vertical Wall Equation User Curve Name",
                                                         "AlamdariHammondStableHorizontal,  !- Floor Heat Ceiling Cool Stable Horizontal Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Stable Horizontal Equation User Curve Name",
                                                         "KhalifaEq4CeilingAwayFromHeat,  !- Floor Heat Ceiling Cool Unstable Horizontal Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Unstable Horizontal Equation User Curve Name",
                                                         "AwbiHattonHeatedFloor,   !- Floor Heat Ceiling Cool Heated Floor Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Heated Floor Equation User Curve Name",
                                                         "KaradagChilledCeiling,   !- Floor Heat Ceiling Cool Chilled Ceiling Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Chilled Ceiling Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Floor Heat Ceiling Cool Stable Tilted Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Stable Tilted Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Floor Heat Ceiling Cool Unstable Tilted Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Unstable Tilted Equation User Curve Name",
                                                         "ISO15099Windows,         !- Floor Heat Ceiling Cool Window Equation Source",
                                                         ",                        !- Floor Heat Ceiling Cool Window Equation User Curve Name",
                                                         "KhalifaEq6NonHeatedWalls,!- Wall Panel Heating Vertical Wall Equation Source",
                                                         ",                        !- Wall Panel Heating Vertical Wall Equation User Curve Name",
                                                         "AwbiHattonHeatedWall,    !- Wall Panel Heating Heated Wall Equation Source",
                                                         ",                        !- Wall Panel Heating Heated Wall Equation User Curve Name",
                                                         "AlamdariHammondStableHorizontal,  !- Wall Panel Heating Stable Horizontal Equation Source",
                                                         ",                        !- Wall Panel Heating Stable Horizontal Equation User Curve Name",
                                                         "KhalifaEq7Ceiling,       !- Wall Panel Heating Unstable Horizontal Equation Source",
                                                         ",                        !- Wall Panel Heating Unstable Horizontal Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Wall Panel Heating Stable Tilted Equation Source",
                                                         ",                        !- Wall Panel Heating Stable Tilted Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Wall Panel Heating Unstable Tilted Equation Source",
                                                         ",                        !- Wall Panel Heating Unstable Tilted Equation User Curve Name",
                                                         "ISO15099Windows,         !- Wall Panel Heating Window Equation Source",
                                                         ",                        !- Wall Panel Heating Window Equation User Curve Name",
                                                         "FohannoPolidoriVerticalWall,  !- Convective Zone Heater Vertical Wall Equation Source",
                                                         ",                        !- Convective Zone Heater Vertical Wall Equation User Curve Name",
                                                         "KhalifaEq5WallNearHeat,  !- Convective Zone Heater Vertical Walls Near Heater Equation Source",
                                                         ",                        !- Convective Zone Heater Vertical Walls Near Heater Equation User Curve Name",
                                                         "AlamdariHammondStableHorizontal,  !- Convective Zone Heater Stable Horizontal Equation Source",
                                                         ",                        !- Convective Zone Heater Stable Horizontal Equation User Curve Name",
                                                         "KhalifaEq7Ceiling,       !- Convective Zone Heater Unstable Horizontal Equation Source",
                                                         ",                        !- Convective Zone Heater Unstable Horizontal Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Convective Zone Heater Stable Tilted Equation Source",
                                                         ",                        !- Convective Zone Heater Stable Tilted Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Convective Zone Heater Unstable Tilted Equation Source",
                                                         ",                        !- Convective Zone Heater Unstable Tilted Equation User Curve Name",
                                                         "ISO15099Windows,         !- Convective Zone Heater Windows Equation Source",
                                                         ",                        !- Convective Zone Heater Windows Equation User Curve Name",
                                                         "GoldsteinNovoselacCeilingDiffuserWalls,  !- Central Air Diffuser Wall Equation Source",
                                                         ",                        !- Central Air Diffuser Wall Equation User Curve Name",
                                                         "FisherPedersenCeilingDiffuserCeiling,  !- Central Air Diffuser Ceiling Equation Source",
                                                         ",                        !- Central Air Diffuser Ceiling Equation User Curve Name",
                                                         "GoldsteinNovoselacCeilingDiffuserFloor,  !- Central Air Diffuser Floor Equation Source",
                                                         ",                        !- Central Air Diffuser Floor Equation User Curve Name",
                                                         "GoldsteinNovoselacCeilingDiffuserWindow,  !- Central Air Diffuser Window Equation Source",
                                                         ",                        !- Central Air Diffuser Window Equation User Curve Name",
                                                         "KhalifaEq3WallAwayFromHeat,  !- Mechanical Zone Fan Circulation Vertical Wall Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Vertical Wall Equation User Curve Name",
                                                         "AlamdariHammondStableHorizontal,  !- Mechanical Zone Fan Circulation Stable Horizontal Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Stable Horizontal Equation User Curve Name",
                                                         "KhalifaEq4CeilingAwayFromHeat,  !- Mechanical Zone Fan Circulation Unstable Horizontal Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Unstable Horizontal Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Mechanical Zone Fan Circulation Stable Tilted Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Stable Tilted Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Mechanical Zone Fan Circulation Unstable Tilted Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Unstable Tilted Equation User Curve Name",
                                                         "ISO15099Windows,         !- Mechanical Zone Fan Circulation Window Equation Source",
                                                         ",                        !- Mechanical Zone Fan Circulation Window Equation User Curve Name",
                                                         ",  !- Mixed Regime Buoyancy Assisting Flow on Walls Equation Source",
                                                         ",                        !- Mixed Regime Buoyancy Assisting Flow on Walls Equation User Curve Name",
                                                         ",  !- Mixed Regime Buoyancy Opposing Flow on Walls Equation Source",
                                                         ",                        !- Mixed Regime Buoyancy Opposing Flow on Walls Equation User Curve Name",
                                                         ",  !- Mixed Regime Stable Floor Equation Source",
                                                         ",                        !- Mixed Regime Stable Floor Equation User Curve Name",
                                                         ",  !- Mixed Regime Unstable Floor Equation Source",
                                                         ",                        !- Mixed Regime Unstable Floor Equation User Curve Name",
                                                         ",  !- Mixed Regime Stable Ceiling Equation Source",
                                                         ",                        !- Mixed Regime Stable Ceiling Equation User Curve Name",
                                                         ",  !- Mixed Regime Unstable Ceiling Equation Source",
                                                         ",                        !- Mixed Regime Unstable Ceiling Equation User Curve Name",
                                                         ",  !- Mixed Regime Window Equation Source",
                                                         ";                        !- Mixed Regime Window Equation User Curve Name",

                                                         "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections,",
                                                         "Default algorithms,      !- Name",
                                                         "TARPWindward,            !- Wind Convection Windward Vertical Wall Equation Source",
                                                         ",                        !- Wind Convection Windward Equation Vertical Wall User Curve Name",
                                                         "TARPLeeward,             !- Wind Convection Leeward Vertical Wall Equation Source",
                                                         ",                        !- Wind Convection Leeward Vertical Wall Equation User Curve Name",
                                                         "ClearRoof,               !- Wind Convection Horizontal Roof Equation Source",
                                                         ",                        !- Wind Convection Horizontal Roof User Curve Name",
                                                         "ASHRAEVerticalWall,      !- Natural Convection Vertical Wall Equation Source",
                                                         ",                        !- Natural Convection Vertical Wall Equation User Curve Name",
                                                         "WaltonStableHorizontalOrTilt,  !- Natural Convection Stable Horizontal Equation Source",
                                                         ",                        !- Natural Convection Stable Horizontal Equation User Curve Name",
                                                         "WaltonUnstableHorizontalOrTilt,  !- Natural Convection Unstable Horizontal Equation Source",
                                                         ";                        !- Natural Convection Unstable Horizontal Equation User Curve Name",
                                                     });

    ASSERT_TRUE(process_idf(idf_objects));

    DataHeatBalSurface::TempSurfInTmp.allocate(6);
    DataHeatBalSurface::TempSurfInTmp(1) = 15.0;
    DataHeatBalSurface::TempSurfInTmp(2) = 20.0;
    DataHeatBalSurface::TempSurfInTmp(3) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(4) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(5) = 25.0;
    DataHeatBalSurface::TempSurfInTmp(6) = 25.0;
    ConvectionCoefficients::InitInteriorConvectionCoeffs(*state, DataHeatBalSurface::TempSurfInTmp);
    ConvectionCoefficients::GetUserConvectionCoefficients(*state);

    int algorithm_identifier;

    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondUnstableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KaradagChilledCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq6NonHeatedWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AwbiHattonHeatedWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FohannoPolidoriVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq5WallNearHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq7Ceiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWalls);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_FisherPedersenCeilDiffuserCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq3WallAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_AlamdariHammondStableHorizontal);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_KhalifaEq4CeilingAwayFromHeat);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_WaltonUnstableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_ISO15099Windows);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedAssistingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedOppossingWall);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableFloor);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedStableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_BeausoleilMorrisonMixedUnstableCeiling);
    algorithm_identifier = state->dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum;
    ASSERT_EQ(algorithm_identifier, HcInt_GoldsteinNovoselacCeilingDiffuserWindow);

    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowWindward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_SparrowLeeward);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_ClearRoof);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalASHRAEVerticalWall);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonStableHorizontalOrTilt);
    algorithm_identifier = state->dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum;
    ASSERT_EQ(algorithm_identifier, HcExt_NaturalWaltonUnstableHorizontalOrTilt);

    DataHeatBalSurface::TempSurfInTmp.deallocate();
}

TEST_F(ConvectionCoefficientsFixture, TestASTMC1340)
{
    Real64 Tsurf;
    Real64 Tair;
    Real64 AirStreamV;
    Real64 Tilt;
    Real64 Hin;

    DataSurfaces::Surface.allocate(3);
    DataHeatBalance::Zone.allocate(3);

    // Horizontal Roof, heat flow down
    DataSurfaces::Surface(1).Zone = 1;
    DataHeatBalance::Zone(1).Volume = 1000;
    DataSurfaces::Surface(1).Class = DataSurfaces::SurfaceClass::Roof;
    DataSurfaces::Surface(1).Tilt = 0;
    DataSurfaces::Surface(1).Area = 100;
    DataSurfaces::Surface(1).ExtBoundCond = 0;
    DataSurfaces::Surface(1).WindSpeed = 1;

    Tsurf = 18.0;
    Tair = 15.0;
    AirStreamV = 2.0;
    Tilt = DataSurfaces::Surface(1).Tilt;

    Hin = ConvectionCoefficients::CalcASTMC1340ConvCoeff(1, Tsurf, Tair, AirStreamV, Tilt);

    EXPECT_NEAR(Hin, 1.977, 0.001);

    //Pitched Roof, heat flow up
    DataSurfaces::Surface(2).Zone = 2;
    DataHeatBalance::Zone(2).Volume = 1000;
    DataSurfaces::Surface(2).Class = DataSurfaces::SurfaceClass::Roof;
    DataSurfaces::Surface(2).Tilt = 20;
    DataSurfaces::Surface(2).Area = 100;
    DataSurfaces::Surface(2).ExtBoundCond = 0;
    DataSurfaces::Surface(2).WindSpeed = 1;
    DataSurfaces::Surface(2).Height = 8;

    Tsurf = 15.0;
    Tair = 18.0;
    AirStreamV = 2.0;
    Tilt = DataSurfaces::Surface(2).Tilt;

    Hin = ConvectionCoefficients::CalcASTMC1340ConvCoeff(2, Tsurf, Tair, AirStreamV, Tilt);

    EXPECT_NEAR(Hin, 2.666, 0.001);
    
    // Vertical Wall
    DataSurfaces::Surface(3).Zone = 3;
    DataHeatBalance::Zone(3).Volume = 1000;
    DataSurfaces::Surface(3).Class = DataSurfaces::SurfaceClass::Wall;
    DataSurfaces::Surface(3).Tilt = 90;
    DataSurfaces::Surface(3).Area = 100;
    DataSurfaces::Surface(3).ExtBoundCond = 1;
    DataSurfaces::Surface(3).Height = 3;

    Tsurf = 15.0;
    Tair = 18.0;
    AirStreamV = 0.0055;
    Tilt = DataSurfaces::Surface(3).Tilt;

    Hin = ConvectionCoefficients::CalcASTMC1340ConvCoeff(3, Tsurf, Tair, AirStreamV, Tilt);

    EXPECT_NEAR(Hin, 1.756, 0.001);

}
