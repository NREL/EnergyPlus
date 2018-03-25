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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEnergyDemands;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, BaseboardConvWater_SizingTest)
{
    int BaseboardNum(0);
    int CntrlZoneNum(0);
    bool FirstHVACIteration(false);

    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    SPACE2-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "  Zone,",
        "    SPACE3-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "  Zone,",
        "    SPACE4-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 in node,        !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Node,           !- Zone Air Node Name",
        "    SPACE2-1 ret node;       !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE3-1,                !- Zone Name",
        "    SPACE3-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE3-1 in node,        !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE3-1 Node,           !- Zone Air Node Name",
        "    SPACE3-1 ret node;       !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE4-1,                !- Zone Name",
        "    SPACE4-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE4-1 in node,       !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE4-1 Node,           !- Zone Air Node Name",
        "    SPACE4-1 ret node;       !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:Baseboard:Convective:Water,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 Baseboard,      !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE3-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:Baseboard:Convective:Water,  !- Zone Equipment 1 Object Type",
        "    SPACE3-1 Baseboard,      !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE4-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:Baseboard:Convective:Water,  !- Zone Equipment 1 Object Type",
        "    SPACE4-1 Baseboard,      !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        " ZoneHVAC:Baseboard:Convective:Water,",
        "    SPACE2-1 Baseboard,      !- Name",
        "    always_on,               !- Availability Schedule Name",
        "    SPACE2-1 Baseboard Inlet Node,   !- Inlet Node Name",
        "    SPACE2-1 Baseboard Outlet Node,  !- Outlet Node Name",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    1000.0,                  !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    autosize,                !- U-Factor Times Area Value",
        "    autosize,                !- Maximum Water Flow Rate",
        "    0.001;                   !- Convergence Tolerance",

        "  BuildingSurface:Detailed,",
        "    RIGHT-1,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5,0.0,2.4,   !- X,Y,Z ==> Vertex 1 {m}",
        "    30.5,0.0,0.0,   !- X,Y,Z ==> Vertex 2 {m}",
        "    30.5,15.2,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    30.5,15.2,2.4;  !- X,Y,Z ==> Vertex 4 {m}",

        "  ZoneHVAC:Baseboard:Convective:Water,",
        "    SPACE3-1 Baseboard,      !- Name",
        "    always_on,               !- Availability Schedule Name",
        "    SPACE3-1 Baseboard Inlet Node,   !- Inlet Node Name",
        "    SPACE3-1 Baseboard Outlet Node,  !- Outlet Node Name",
        "    CapacityPerFloorArea,    !- Heating Design Capacity Method",
        "    ,                        !- Heating Design Capacity {W}",
        "    40.0,                    !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    autosize,                !- U-Factor Times Area Value",
        "    autosize,                !- Maximum Water Flow Rate",
        "    0.001;                   !- Convergence Tolerance",

        "  BuildingSurface:Detailed,",
        "    FRONT-1,                  !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 0.0, 2.4,    !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0, 0.0, 0.0,    !- X,Y,Z ==> Vertex 2 {m}",
        "    20.0, 0.0, 0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    20.0, 0.0, 2.4;   !- X,Y,Z ==> Vertex 4 {m}",

        "  ZoneHVAC:Baseboard:Convective:Water,",
        "    SPACE4-1 Baseboard,      !- Name",
        "    always_on,               !- Availability Schedule Name",
        "    SPACE4-1 Baseboard Inlet Node,   !- Inlet Node Name",
        "    SPACE4-1 Baseboard Outlet Node,  !- Outlet Node Name",
        "    FractionOfAutosizedHeatingCapacity,   !- Heating Design Capacity Method",
        "    ,                        !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    0.5,                     !- Fraction of Autosized Heating Design Capacity",
        "    autosize,                !- U-Factor Times Area Value",
        "    autosize,                !- Maximum Water Flow Rate",
        "    0.001;                   !- Convergence Tolerance",

        "  BuildingSurface:Detailed,",
        "    LEFT-1,                  !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,15.2,2.4,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,15.2,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,0.0,0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,2.4;   !- X,Y,Z ==> Vertex 4 {m}",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  Schedule:Compact,",
        "    always_on,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3"

        "SurfaceConvectionAlgorithm:Inside,TARP;",

        "SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "HeatBalanceAlgorithm,ConductionTransferFunction;",

        "ZoneAirHeatBalanceAlgorithm,",
        "    AnalyticalSolution;      !- Algorithm",

        "  Construction,",
        "    WALL-1,                  !- Name",
        "    WD01,                    !- Outside Layer",
        "    PW03,                    !- Layer 2",
        "    IN02,                    !- Layer 3",
        "    GP01;                    !- Layer 4",

        "  Material,",
        "    WD01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.9099999E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    513.0000,                !- Density {kg/m3}",
        "    1381.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "  Material,",
        "    PW03,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    545.0000,                !- Density {kg/m3",
        "    1213.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

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

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(); // read schedules

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

    SurfaceGeometry::CosZoneRelNorth.allocate(3);
    SurfaceGeometry::SinZoneRelNorth.allocate(3);
    SurfaceGeometry::CosZoneRelNorth(1) = std::cos(-DataHeatBalance::Zone(1).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::CosZoneRelNorth(2) = std::cos(-DataHeatBalance::Zone(2).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::CosZoneRelNorth(3) = std::cos(-DataHeatBalance::Zone(3).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(1) = std::sin(-DataHeatBalance::Zone(1).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(2) = std::sin(-DataHeatBalance::Zone(2).RelNorth * DataGlobals::DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(3) = std::sin(-DataHeatBalance::Zone(3).RelNorth * DataGlobals::DegToRadians);

    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(errorsFound);
    ASSERT_FALSE(errorsFound);

    ZoneSizingInput.allocate(3);
    NumZoneSizingInput = 3;
    ZoneSizingInput(1).ZoneNum = 1;
    ZoneSizingInput(2).ZoneNum = 2;
    ZoneSizingInput(3).ZoneNum = 3;

    int TotNumLoops = 1;
    PlantLoop.allocate(TotNumLoops);
    PlantSizData.allocate(TotNumLoops);
    PlantSizData(1).DeltaT = 10.0;
    // unit test results are based on a low HW temp at 40 C. Baseboard does not have the capacity to meet the zone load.
    PlantSizData(1).ExitTemp = 40.0;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.PlantSizNum = 1;
        loop.FluidName = "WATER";
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    DataZoneEquipment::GetZoneEquipmentData1();
    // get electric baseboard inputs
    BaseboardRadiator::GetBaseboardInput();
    BaseboardRadiator::MySizeFlag.allocate(3);
    BaseboardRadiator::MySizeFlag = true;

    DataSizing::FinalZoneSizing.allocate(3);
    DataSizing::ZoneEqSizing.allocate(3);
    DataSizing::ZoneSizingRunDone = true;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(3);
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(3);

    BaseboardNum = 1;
    CntrlZoneNum = 1;
    DataSizing::CurZoneEqNum = CntrlZoneNum;
    FirstHVACIteration = true;
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod(DataHVACGlobals::HeatingCapacitySizing) =
        BaseboardRadiator::Baseboard(BaseboardNum).HeatingCapMethod;
    DataSizing::FinalZoneSizing(CntrlZoneNum).NonAirSysDesHeatLoad = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CntrlZoneNum).RemainingOutputReqToHeatSP = 2000.0;
    DataZoneEnergyDemands::CurDeadBandOrSetback(CntrlZoneNum) = false;
    FinalZoneSizing(CntrlZoneNum).ZoneTempAtHeatPeak = 20.0;
    FinalZoneSizing(CntrlZoneNum).ZoneHumRatAtHeatPeak = 0.005;
    // do baseboard sizing
    BaseboardRadiator::Baseboard(BaseboardNum).LoopNum = 1;
    BaseboardRadiator::Baseboard(BaseboardNum).ZonePtr = 1;
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    // check UA value
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity, 1000.0);
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 1000.0);
    // check UA value with autosized scaled capacity
    BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity = DataSizing::AutoSize;
    BaseboardRadiator::Baseboard(BaseboardNum).WaterVolFlowRateMax = DataSizing::AutoSize;
    BaseboardRadiator::Baseboard(BaseboardNum).UA = DataSizing::AutoSize; // reset to autosize to test new calculation
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    EXPECT_EQ(DataZoneEnergyDemands::ZoneSysEnergyDemand(CntrlZoneNum).RemainingOutputReqToHeatSP, 2000.0); // design load = 2000
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 2000.0);                                       // UA = design load

    BaseboardNum = 2;
    CntrlZoneNum = 2;
    DataSizing::CurZoneEqNum = CntrlZoneNum;
    FirstHVACIteration = true;
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod(DataHVACGlobals::HeatingCapacitySizing) =
        BaseboardRadiator::Baseboard(BaseboardNum).HeatingCapMethod;
    DataSizing::FinalZoneSizing(CntrlZoneNum).NonAirSysDesHeatLoad = 2000.0;
    DataZoneEnergyDemands::CurDeadBandOrSetback(CntrlZoneNum) = false;
    FinalZoneSizing(CntrlZoneNum).ZoneTempAtHeatPeak = 20.0;
    FinalZoneSizing(CntrlZoneNum).ZoneHumRatAtHeatPeak = 0.005;
    DataHeatBalance::Zone(CntrlZoneNum).FloorArea = 100.0;
    // do baseboard sizing
    BaseboardRadiator::Baseboard(BaseboardNum).LoopNum = 1;
    BaseboardRadiator::Baseboard(BaseboardNum).ZonePtr = 2;
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    // check UA value
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity, 40.0);
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 4000.0);
    BaseboardRadiator::Baseboard(BaseboardNum).UA = DataSizing::AutoSize; // reset to autosize to test new calculation
    BaseboardRadiator::Baseboard(BaseboardNum).WaterVolFlowRateMax = DataSizing::AutoSize;
    // check UA value with autosized UA
    BaseboardRadiator::Baseboard(BaseboardNum).HeatingCapMethod = DataSizing::HeatingDesignCapacity;
    BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity = DataSizing::AutoSize;
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 2000.0);

    BaseboardNum = 3;
    CntrlZoneNum = 3;
    DataSizing::CurZoneEqNum = CntrlZoneNum;
    FirstHVACIteration = true;
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);
    DataSizing::ZoneEqSizing(CntrlZoneNum).SizingMethod(DataHVACGlobals::HeatingCapacitySizing) =
        BaseboardRadiator::Baseboard(BaseboardNum).HeatingCapMethod;
    DataSizing::FinalZoneSizing(CntrlZoneNum).NonAirSysDesHeatLoad = 3000.0;
    DataZoneEnergyDemands::CurDeadBandOrSetback(CntrlZoneNum) = false;
    FinalZoneSizing(CntrlZoneNum).ZoneTempAtHeatPeak = 20.0;
    FinalZoneSizing(CntrlZoneNum).ZoneHumRatAtHeatPeak = 0.005;
    DataHeatBalance::Zone(CntrlZoneNum).FloorArea = 100.0;
    // do baseboard sizing
    BaseboardRadiator::Baseboard(BaseboardNum).LoopNum = 1;
    BaseboardRadiator::Baseboard(BaseboardNum).ZonePtr = 3;
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    // check UA value
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity, 0.50);
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 1500.0);
    BaseboardRadiator::Baseboard(BaseboardNum).UA = DataSizing::AutoSize; // reset to autosize to test new calculation
    BaseboardRadiator::Baseboard(BaseboardNum).WaterVolFlowRateMax = DataSizing::AutoSize;
    // check UA value with autosized scaled capacity
    BaseboardRadiator::Baseboard(BaseboardNum).HeatingCapMethod = DataSizing::HeatingDesignCapacity;
    BaseboardRadiator::Baseboard(BaseboardNum).ScaledHeatingCapacity = DataSizing::AutoSize;
    BaseboardRadiator::SizeBaseboard(BaseboardNum);
    EXPECT_EQ(BaseboardRadiator::Baseboard(BaseboardNum).UA, 3000.0);
}

} // namespace EnergyPlus
