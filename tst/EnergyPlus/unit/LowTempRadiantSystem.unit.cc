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

// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::LowTempRadiantSystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::FluidProperties;

using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSurfaceLists;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::PlantManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::General;

class LowTempRadiantSystemTest : public EnergyPlusFixture
{
public:
    int RadSysNum;
    int SystemType;
    Real64 ExpectedResult1;
    Real64 ExpectedResult2;
    Real64 ExpectedResult3;
    Real64 const CpWater = 4180.0;  // For estimating the expected result
    Real64 const RhoWater = 1000.0; // For estimating the expected result

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        ElecRadSys.allocate(1);
        HydrRadSys.allocate(1);
        CFloRadSys.allocate(1);
        FinalZoneSizing.allocate(1);
        ZoneEqSizing.allocate(1);
        Zone.allocate(1);
        CurZoneEqNum = 1;
        ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(25);
        ZoneSizingRunDone = true;

        CurSysNum = 0;
        RadSysNum = 1;
        SystemType = ElectricSystem;
        ElecRadSysNumericFields.allocate(1);
        ElecRadSysNumericFields(RadSysNum).FieldNames.allocate(1);
        HydronicRadiantSysNumericFields.allocate(1);
        HydronicRadiantSysNumericFields(RadSysNum).FieldNames.allocate(15);
        HydrRadSys(RadSysNum).NumCircuits.allocate(1);
        CFloRadSys(RadSysNum).NumCircuits.allocate(1);
        // set up plant loop
        TotNumLoops = 2;
        PlantLoop.allocate(TotNumLoops);
        PlantSizData.allocate(TotNumLoops);
        NumPltSizInput = TotNumLoops;

        for (int loopindex = 1; loopindex <= TotNumLoops; ++loopindex) {
            auto &loop(PlantLoop(loopindex));
            loop.LoopSide.allocate(2);
            auto &loopside(PlantLoop(loopindex).LoopSide(1));
            loopside.TotalBranches = 1;
            loopside.Branch.allocate(1);
            auto &loopsidebranch(PlantLoop(loopindex).LoopSide(1).Branch(1));
            loopsidebranch.TotalComponents = 1;
            loopsidebranch.Comp.allocate(1);
        }
        PlantLoop(1).Name = "Hot Water Loop";
        PlantLoop(1).FluidName = "WATER";
        PlantLoop(1).FluidIndex = 1;

        PlantLoop(2).Name = "Chilled Water Loop";
        PlantLoop(2).FluidName = "WATER";
        PlantLoop(2).FluidIndex = 1;

        PlantSizData(1).PlantLoopName = "Hot Water Loop";
        PlantSizData(1).ExitTemp = 80.0;
        PlantSizData(1).DeltaT = 10.0;

        PlantSizData(2).PlantLoopName = "Chilled Water Loop";
        PlantSizData(2).ExitTemp = 6.0;
        PlantSizData(2).DeltaT = 5.0;

        ExpectedResult1 = 0.0;
        ExpectedResult2 = 0.0;
        ExpectedResult3 = 0.0;
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(LowTempRadiantSystemTest, SizeLowTempRadiantElectric)
{
    SystemType = ElectricSystem;
    ElecRadSys(RadSysNum).Name = "LowTempElectric 1";
    ElecRadSys(RadSysNum).ZonePtr = 1;
    ElecRadSysNumericFields(RadSysNum).FieldNames(1) = "Heating Design Capacity";

    // Electric - HeatingDesignCapacity method
    ElecRadSys(RadSysNum).MaxElecPower = AutoSize;
    ElecRadSys(RadSysNum).HeatingCapMethod = HeatingDesignCapacity;
    ElecRadSys(RadSysNum).ScaledHeatingCapacity = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 1200.0;
    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(1200.0, ElecRadSys(RadSysNum).MaxElecPower, 0.1);

    // Electric - CapacityPerFloorArea method - hold until scalable sizing issue is resolved
    ElecRadSys(RadSysNum).MaxElecPower = AutoSize;
    ElecRadSys(RadSysNum).HeatingCapMethod = CapacityPerFloorArea;
    ElecRadSys(RadSysNum).ScaledHeatingCapacity = 1.5;
    Zone(1).FloorArea = 500.0;
    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(750.0, ElecRadSys(RadSysNum).MaxElecPower, 0.1);

    // Electric - FractionOfAutosizedHeatingCapacity method - hold until scalable sizing issue is resolved
    ElecRadSys(RadSysNum).MaxElecPower = AutoSize;
    ElecRadSys(RadSysNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
    ElecRadSys(RadSysNum).ScaledHeatingCapacity = 10.0;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 880.0;
    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(8800.0, ElecRadSys(RadSysNum).MaxElecPower, 0.1);
}

TEST_F(LowTempRadiantSystemTest, SizeLowTempRadiantVariableFlow)
{
    SystemType = HydronicSystem;
    HydrRadSys(RadSysNum).Name = "LowTempVarFlow 1";
    HydrRadSys(RadSysNum).ZonePtr = 1;
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(3) = "Heating Design Capacity";
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(8) = "Cooling Design Capacity";

    HydrRadSys(RadSysNum).HotWaterInNode = 1;
    HydrRadSys(RadSysNum).HotWaterOutNode = 2;
    HydrRadSys(RadSysNum).HWLoopNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = HydrRadSys(RadSysNum).HotWaterInNode;

    HydrRadSys(RadSysNum).ColdWaterInNode = 3;
    HydrRadSys(RadSysNum).ColdWaterOutNode = 4;
    HydrRadSys(RadSysNum).CWLoopNum = 2;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = HydrRadSys(RadSysNum).ColdWaterInNode;

    // Hydronic - HeatingDesignCapacity/CoolingDesignCapacity method
    HydrRadSys(RadSysNum).WaterVolFlowMaxHeat = AutoSize;
    HydrRadSys(RadSysNum).HeatingCapMethod = HeatingDesignCapacity;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 1200.0;
    ExpectedResult1 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
    ExpectedResult1 = ExpectedResult1 / (PlantSizData(1).DeltaT * RhoWater * CpWater);

    HydrRadSys(RadSysNum).WaterVolFlowMaxCool = AutoSize;
    HydrRadSys(RadSysNum).CoolingCapMethod = CoolingDesignCapacity;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad = 2200.0;
    ExpectedResult2 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
    ExpectedResult2 = ExpectedResult2 / (PlantSizData(2).DeltaT * RhoWater * CpWater);

    HydrRadSys(RadSysNum).NumCircCalcMethod = 0;
    HydrRadSys(RadSysNum).NumOfSurfaces = 1;
    HydrRadSys(RadSysNum).TubeLength = AutoSize;
    HydrRadSys(RadSysNum).TotalSurfaceArea = 1500.0;
    ExpectedResult3 = HydrRadSys(RadSysNum).TotalSurfaceArea / 0.15;
    HydrRadSys(RadSysNum).SurfacePtr.allocate(1);
    HydrRadSys(RadSysNum).SurfacePtr(1) = 1;
    Surface.allocate(1);
    Surface(1).Construction = 1;
    Surface(1).Area = 1500.0;
    Construct.allocate(1);
    Construct(1).ThicknessPerpend = 0.075;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).WaterVolFlowMaxHeat, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).WaterVolFlowMaxCool, 0.1);
    EXPECT_NEAR(ExpectedResult3, HydrRadSys(RadSysNum).TubeLength, 0.1);

    // Hydronic - CapacityPerFloorArea method
    HydrRadSys(RadSysNum).WaterVolFlowMaxHeat = AutoSize;
    HydrRadSys(RadSysNum).HeatingCapMethod = CapacityPerFloorArea;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = 10.0;
    Zone(1).FloorArea = 500.0;
    ExpectedResult1 = HydrRadSys(RadSysNum).ScaledHeatingCapacity * Zone(1).FloorArea;
    ExpectedResult1 = ExpectedResult1 / (PlantSizData(1).DeltaT * RhoWater * CpWater);

    HydrRadSys(RadSysNum).WaterVolFlowMaxCool = AutoSize;
    HydrRadSys(RadSysNum).CoolingCapMethod = CapacityPerFloorArea;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = 20.0;
    ExpectedResult2 = HydrRadSys(RadSysNum).ScaledCoolingCapacity * Zone(1).FloorArea;
    ExpectedResult2 = ExpectedResult2 / (PlantSizData(2).DeltaT * RhoWater * CpWater);

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).WaterVolFlowMaxHeat, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).WaterVolFlowMaxCool, 0.1);

    // Hydronic - FractionOfAutosizedHeating/CoolingCapacity method
    HydrRadSys(RadSysNum).WaterVolFlowMaxHeat = AutoSize;
    HydrRadSys(RadSysNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = 1.2;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 880.0;
    ExpectedResult1 = HydrRadSys(RadSysNum).ScaledHeatingCapacity * FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
    ExpectedResult1 = ExpectedResult1 / (PlantSizData(1).DeltaT * RhoWater * CpWater);

    HydrRadSys(RadSysNum).WaterVolFlowMaxCool = AutoSize;
    HydrRadSys(RadSysNum).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = 1.5;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad = 1200.0;
    ExpectedResult2 = HydrRadSys(RadSysNum).ScaledCoolingCapacity * FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
    ExpectedResult2 = ExpectedResult2 / (PlantSizData(2).DeltaT * RhoWater * CpWater);

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).WaterVolFlowMaxHeat, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).WaterVolFlowMaxCool, 0.1);
}

TEST_F(LowTempRadiantSystemTest, SizeCapacityLowTempRadiantVariableFlow)
{
    SystemType = HydronicSystem;
    HydrRadSys(RadSysNum).Name = "LowTempVarFlow 1";
    HydrRadSys(RadSysNum).ZonePtr = 1;
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(3) = "Heating Design Capacity";
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(8) = "Cooling Design Capacity";

    HydrRadSys(RadSysNum).HotWaterInNode = 1;
    HydrRadSys(RadSysNum).HotWaterOutNode = 2;
    HydrRadSys(RadSysNum).HWLoopNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = HydrRadSys(RadSysNum).HotWaterInNode;

    HydrRadSys(RadSysNum).ColdWaterInNode = 3;
    HydrRadSys(RadSysNum).ColdWaterOutNode = 4;
    HydrRadSys(RadSysNum).CWLoopNum = 2;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = HydrRadSys(RadSysNum).ColdWaterInNode;

    // Hydronic - HeatingDesignCapacity/CoolingDesignCapacity Autosize Method
    HydrRadSys(RadSysNum).HeatingCapMethod = HeatingDesignCapacity;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = AutoSize;
    FinalZoneSizing.allocate(CurZoneEqNum);
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 1200.0;
    ExpectedResult1 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;

    HydrRadSys(RadSysNum).CoolingCapMethod = CoolingDesignCapacity;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad = 2200.0;
    ExpectedResult2 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;

    HydrRadSys(RadSysNum).SurfacePtr.allocate(1);
    HydrRadSys(RadSysNum).SurfacePtr(1) = 1;
    Surface.allocate(1);
    Surface(1).Construction = 1;
    Surface(1).Area = 1500.0;
    Construct.allocate(1);
    Construct(1).ThicknessPerpend = 0.075;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).ScaledHeatingCapacity, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).ScaledCoolingCapacity, 0.1);

    // Hydronic - CapacityPerFloorArea Capacity Sizing Method
    Zone(1).FloorArea = 50.0;
    HydrRadSys(RadSysNum).HeatingCapMethod = CapacityPerFloorArea;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = 200.0;
    ExpectedResult1 = HydrRadSys(RadSysNum).ScaledHeatingCapacity * Zone(1).FloorArea;

    HydrRadSys(RadSysNum).CoolingCapMethod = CapacityPerFloorArea;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = 250.0;
    ExpectedResult2 = HydrRadSys(RadSysNum).ScaledCoolingCapacity * Zone(1).FloorArea;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).ScaledHeatingCapacity, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).ScaledCoolingCapacity, 0.1);

    // Hydronic - FractionOfAutosizedHeating/CoolingCapacity Sizing Method
    HydrRadSys(RadSysNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
    HydrRadSys(RadSysNum).ScaledHeatingCapacity = 1.2;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 880.0;
    ExpectedResult1 = HydrRadSys(RadSysNum).ScaledHeatingCapacity * FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;

    HydrRadSys(RadSysNum).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
    HydrRadSys(RadSysNum).ScaledCoolingCapacity = 1.5;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad = 1200.0;
    ExpectedResult2 = HydrRadSys(RadSysNum).ScaledCoolingCapacity * FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, HydrRadSys(RadSysNum).ScaledHeatingCapacity, 0.1);
    EXPECT_NEAR(ExpectedResult2, HydrRadSys(RadSysNum).ScaledCoolingCapacity, 0.1);
}

TEST_F(LowTempRadiantSystemTest, SizeLowTempRadiantConstantFlow)
{
    SystemType = ConstantFlowSystem;
    CFloRadSys(RadSysNum).Name = "LowTempConstantFlow 1";
    CFloRadSys(RadSysNum).ZonePtr = 1;
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(2) = "Rated Flow Rate";
    HydronicRadiantSysNumericFields(RadSysNum).FieldNames(3) = "Total length of pipe embedded in surface";

    CFloRadSys(RadSysNum).HotWaterInNode = 1;
    CFloRadSys(RadSysNum).HotWaterOutNode = 2;
    CFloRadSys(RadSysNum).HWLoopNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = CFloRadSys(RadSysNum).HotWaterInNode;

    CFloRadSys(RadSysNum).ColdWaterInNode = 3;
    CFloRadSys(RadSysNum).ColdWaterOutNode = 4;
    CFloRadSys(RadSysNum).CWLoopNum = 2;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = CFloRadSys(RadSysNum).ColdWaterInNode;

    // Hydronic - Hot water volume flow rate autosize
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterOutNode = 0;
    CFloRadSys(RadSysNum).WaterVolFlowMax = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad = 1200.0;
    ExpectedResult1 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
    ExpectedResult1 = ExpectedResult1 / (PlantSizData(1).DeltaT * RhoWater * CpWater);

    CFloRadSys(RadSysNum).SurfacePtr.allocate(1);
    CFloRadSys(RadSysNum).SurfacePtr(1) = 1;
    Surface.allocate(1);
    Surface(1).Construction = 1;
    Surface(1).Area = 150.0;
    Construct.allocate(1);
    Construct(1).ThicknessPerpend = 0.075;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult1, CFloRadSys(RadSysNum).WaterVolFlowMax, 0.001);

    // Hydronic - cold water volume flow rate autosize
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).HotWaterOutNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 3;
    CFloRadSys(RadSysNum).ColdWaterOutNode = 4;
    CFloRadSys(RadSysNum).WaterVolFlowMax = AutoSize;
    FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad = 2200.0;
    ExpectedResult2 = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
    ExpectedResult2 = ExpectedResult2 / (PlantSizData(2).DeltaT * RhoWater * CpWater);

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(ExpectedResult2, CFloRadSys(RadSysNum).WaterVolFlowMax, 0.001);

    // Hydronic - maximum water volume flow rate autosize
    CFloRadSys(RadSysNum).WaterVolFlowMax = AutoSize;
    CFloRadSys(RadSysNum).HotWaterInNode = 1;
    CFloRadSys(RadSysNum).HotWaterOutNode = 2;
    CFloRadSys(RadSysNum).ColdWaterInNode = 3;
    CFloRadSys(RadSysNum).ColdWaterOutNode = 4;

    // Hydronic - embeded tube length autosize
    CFloRadSys(RadSysNum).NumCircCalcMethod = 0;
    CFloRadSys(RadSysNum).NumOfSurfaces = 1;
    CFloRadSys(RadSysNum).TubeLength = AutoSize;
    CFloRadSys(RadSysNum).TotalSurfaceArea = 150.0;
    ExpectedResult3 = CFloRadSys(RadSysNum).TotalSurfaceArea / 0.15;

    SizeLowTempRadiantSystem(RadSysNum, SystemType);
    EXPECT_NEAR(std::max(ExpectedResult1, ExpectedResult2), CFloRadSys(RadSysNum).WaterVolFlowMax, 0.001);
    EXPECT_NEAR(ExpectedResult3, CFloRadSys(RadSysNum).TubeLength, 0.1);
}

TEST_F(EnergyPlusFixture, AutosizeLowTempRadiantVariableFlowTest)
{

    int RadSysNum(1);
    Real64 HeatingCapacity;
    Real64 CoolingCapacity;
    Real64 HotWaterFlowRate;
    Real64 ChilledWaterFlowRate;
    Real64 TubeLengthDes;
    Real64 Density;
    Real64 Cp;
    bool ErrorsFound = false;

    std::string const idf_objects = delimited_string({
        "  Version,8.4;",

        "  Building,",
        "    NONE,                    !- Name",
        "    0.0000000E+00,           !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    3.9999999E-02,           !- Loads Convergence Tolerance Value",
        "    0.4000000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  Zone,",
        "    West Zone,               !- Name",
        "    0.0000000E+00,           !- Direction of Relative North {deg}",
        "    0.0000000E+00,           !- X Origin {m}",
        "    0.0000000E+00,           !- Y Origin {m}",
        "    0.0000000E+00,           !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.5,                     !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  ZoneHVAC:EquipmentConnections,",
        "    West Zone,               !- Zone Name",
        "    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
        "    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 1 Node,             !- Zone Air Node Name",
        "    Zone 1 Outlet Node;      !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    Zone1Equipment,          !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:LowTemperatureRadiant:VariableFlow,  !- Zone Equipment 1 Object Type",
        "    West Zone Radiant Floor, !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:LowTemperatureRadiant:VariableFlow,",
        "    West Zone Radiant Floor, !- Name",
        "    RadiantSysAvailSched,    !- Availability Schedule Name",
        "    West Zone,               !- Zone Name",
        "    Zn001:Flr001,            !- Surface Name or Radiant Surface Group Name",
        "    0.012,                   !- Hydronic Tubing Inside Diameter {m}",
        "    autosize,                !- Hydronic Tubing Length {m}",
        "    MeanAirTemperature,      !- Temperature Control Type",
        "    FractionOfAutosizedHeatingCapacity,  !- Heating Design Capacity Method",
        "    ,                        !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    0.9,                     !- Fraction of Autosized Heating Design Capacity",
        "    autosize,                !- Maximum Hot Water Flow {m3/s}",
        "    West Zone Radiant Water Inlet Node,  !- Heating Water Inlet Node Name",
        "    West Zone Radiant Water Outlet Node, !- Heating Water Outlet Node Name",
        "    2.0,                     !- Heating Control Throttling Range {deltaC}",
        "    Radiant Heating Setpoints,  !- Heating Control Temperature Schedule Name",
        "    FractionOfAutosizedCoolingCapacity,   !- Cooling Design Capacity Method",
        "    ,                        !- Cooling Design Capacity {W}",
        "    ,                        !- Cooling Design Capacity Per Floor Area {W/m2}",
        "    1.2,                     !- Fraction of Autosized Cooling Design Capacity",
        "    autosize,                !- Maximum Cold Water Flow {m3/s}",
        "    Zone 1 Cooling Water Inlet Node,     !- Cooling Water Inlet Node Name",
        "    Zone 1 Cooling Water Outlet Node,    !- Cooling Water Outlet Node Name",
        "    2.0,                     !- Cooling Control Throttling Range {deltaC}",
        "    Radiant Cooling Setpoints,           !- Cooling Control Temperature Schedule Name",
        "    ,                        !- Condensation Control Type",
        "    ,                        !- Condensation Control Dewpoint Offset {C}",
        "    ,                        !- Number of Circuits",
        "    ;                        !- Circuit Length {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    Slab Floor with Radiant, !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 0.0, 0.0,           !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0, 6.096000,0.0,       !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000, 0.0, 0.0;      !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction:InternalSource,",
        "    Slab Floor with Radiant, !- Name",
        "    4,                       !- Source Present After Layer Number",
        "    4,                       !- Temperature Calculation Requested After Layer Number",
        "    1,                       !- Dimensions for the CTF Calculation",
        "    0.1524,                  !- Tube Spacing {m}",
        "    CONCRETE - DRIED SAND AND GRAVEL 4 IN,  !- Outside Layer",
        "    INS - EXPANDED EXT POLYSTYRENE R12 2 IN,  !- Layer 2",
        "    GYP1,                    !- Layer 3",
        "    GYP2,                    !- Layer 4",
        "    FINISH FLOORING - TILE 1 / 16 IN;  !- Layer 5",

        "  Material,",
        "    CONCRETE - DRIED SAND AND GRAVEL 4 IN,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1000000,               !- Thickness {m}",
        "    1.290000,                !- Conductivity {W/m-K}",
        "    2242.580,                !- Density {kg/m3}",
        "    830.00000,               !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6000000,               !- Solar Absorptance",
        "    0.6000000;               !- Visible Absorptance",

        "  Material,",
        "    INS - EXPANDED EXT POLYSTYRENE R12 2 IN,  !- Name",
        "    Rough,                   !- Roughness",
        "    5.0000001E-02,           !- Thickness {m}",
        "    2.0000000E-02,           !- Conductivity {W/m-K}",
        "    56.06000,                !- Density {kg/m3}",
        "    1210.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    GYP1,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    7.8450000E-01,           !- Conductivity {W/m-K}",
        "    1842.1221,               !- Density {kg/m3}",
        "    988.000,                 !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    GYP2,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    7.8450000E-01,           !- Conductivity {W/m-K}",
        "    1842.1221,               !- Density {kg/m3}",
        "    988.000,                 !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    FINISH FLOORING - TILE 1 / 16 IN,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.6000000E-03,           !- Thickness {m}",
        "    0.1700000,               !- Conductivity {W/m-K}",
        "    1922.210,                !- Density {kg/m3}",
        "    1250.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Schedule:Compact,",
        "    RADIANTSYSAVAILSCHED,    !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1.00,       !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: Alldays,            !- Field 6",
        "    Until: 24:00,0.00,       !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: Alldays,            !- Field 10",
        "    Until: 24:00,1.00;       !- Field 11",

        "  Schedule:Compact,",
        "    HW LOOP TEMP SCHEDULE,   !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,60.00;      !- Field 3",

        "  Schedule:Compact,",
        "    RADIANT HEATING SETPOINTS,  !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 7:00,12.00,       !- Field 3",
        "    Until: 17:00,17.00,      !- Field 5",
        "    Until: 24:00,12.00;      !- Field 7",

        "  Sizing:Plant,",
        "    Hot Water Loop,          !- Plant or Condenser Loop Name",
        "    heating,                 !- Loop Type",
        "    60.,                     !- Design Loop Exit Temperature {C}",
        "    10;                      !- Loop Design Temperature Difference {deltaC}",

        "  PlantLoop,",
        "    Hot Water Loop,          !- Name",
        "    Water,                   !- Fluid Type",
        "    ,                        !- User Defined Fluid Type",
        "    Hot Loop Operation,      !- Plant Equipment Operation Scheme Name",
        "    HW Supply Outlet Node,   !- Loop Temperature Setpoint Node Name",
        "    100,                     !- Maximum Loop Temperature {C}",
        "    10,                      !- Minimum Loop Temperature {C}",
        "    0.0043,                  !- Maximum Loop Flow Rate {m3/s}",
        "    0,                       !- Minimum Loop Flow Rate {m3/s}",
        "    autocalculate,           !- Plant Loop Volume {m3}",
        "    HW Supply Inlet Node,    !- Plant Side Inlet Node Name",
        "    HW Supply Outlet Node,   !- Plant Side Outlet Node Name",
        "    Heating Supply Side Branches,  !- Plant Side Branch List Name",
        "    Heating Supply Side Connectors,  !- Plant Side Connector List Name",
        "    HW Demand Inlet Node,    !- Demand Side Inlet Node Name",
        "    HW Demand Outlet Node,   !- Demand Side Outlet Node Name",
        "    Heating Demand Side Branches,  !- Demand Side Branch List Name",
        "    Heating Demand Side Connectors,  !- Demand Side Connector List Name",
        "    Optimal,                 !- Load Distribution Scheme",
        "    ,                        !- Availability Manager List Name",
        "    ,                        !- Plant Loop Demand Calculation Scheme",
        "    ,                        !- Common Pipe Simulation",
        "    ,                        !- Pressure Simulation Type",
        "    2.0;                     !- Loop Circulation Time {minutes}",

        "  SetpointManager:Scheduled,",
        "    Hot Water Loop Setpoint Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    HW Loop Temp Schedule,   !- Schedule Name",
        "    Hot Water Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",

        "  NodeList,",
        "    Hot Water Loop Setpoint Node List,  !- Name",
        "    HW Supply Outlet Node;   !- Node 1 Name",

        "  BranchList,",
        "    Heating Supply Side Branches,  !- Name",
        "    Heating Supply Inlet Branch,  !- Branch 1 Name",
        "    Heating Purchased Hot Water Branch,  !- Branch 2 Name",
        "    Heating Supply Bypass Branch,  !- Branch 3 Name",
        "    Heating Supply Outlet Branch;  !- Branch 4 Name",

        "  ConnectorList,",
        "    Heating Supply Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Heating Supply Splitter, !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Heating Supply Mixer;    !- Connector 2 Name",

        "  Branch,",
        "    Heating Supply Inlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pump:VariableSpeed,      !- Component 1 Object Type",
        "    HW Circ Pump,            !- Component 1 Name",
        "    HW Supply Inlet Node,    !- Component 1 Inlet Node Name",
        "    HW Pump Outlet Node;     !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Heating Purchased Hot Water Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    DistrictHeating,         !- Component 1 Object Type",
        "    Purchased Heating,       !- Component 1 Name",
        "    Purchased Heat Inlet Node,  !- Component 1 Inlet Node Name",
        "    Purchased Heat Outlet Node;  !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Heating Supply Bypass Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Heating Supply Side Bypass,  !- Component 1 Name",
        "    Heating Supply Bypass Inlet Node,  !- Component 1 Inlet Node Name",
        "    Heating Supply Bypass Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Heating Supply Side Bypass,  !- Name",
        "    Heating Supply Bypass Inlet Node,  !- Inlet Node Name",
        "    Heating Supply Bypass Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    Heating Supply Outlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Heating Supply Outlet,   !- Component 1 Name",
        "    Heating Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
        "    HW Supply Outlet Node;   !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Heating Supply Outlet,   !- Name",
        "    Heating Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
        "    HW Supply Outlet Node;   !- Outlet Node Name",

        "  BranchList,",
        "    Heating Demand Side Branches,  !- Name",
        "    Reheat Inlet Branch,     !- Branch 1 Name",
        "    Zone 1 Radiant Branch,   !- Branch 5 Name",
        "    Reheat Bypass Branch,    !- Branch 8 Name",
        "    Reheat Outlet Branch;    !- Branch 9 Name",

        "  ConnectorList,",
        "    Heating Demand Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Reheat Splitter,         !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Reheat Mixer;            !- Connector 2 Name",

        "  Branch,",
        "    Reheat Inlet Branch,     !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Reheat Inlet Pipe,       !- Component 1 Name",
        "    HW Demand Inlet Node,    !- Component 1 Inlet Node Name",
        "    HW Demand Entrance Pipe Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Reheat Inlet Pipe,       !- Name",
        "    HW Demand Inlet Node,    !- Inlet Node Name",
        "    HW Demand Entrance Pipe Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    Reheat Outlet Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Reheat Outlet Pipe,      !- Component 1 Name",
        "    HW Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
        "    HW Demand Outlet Node;   !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Reheat Outlet Pipe,      !- Name",
        "    HW Demand Exit Pipe Inlet Node,  !- Inlet Node Name",
        "    HW Demand Outlet Node;   !- Outlet Node Name",

        "  Branch,",
        "    Zone 1 Radiant Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    ZoneHVAC:LowTemperatureRadiant:VariableFlow,  !- Component 1 Object Type",
        "    West Zone Radiant Floor, !- Component 1 Name",
        "    West Zone Radiant Water Inlet Node,  !- Component 1 Inlet Node Name",
        "    West Zone Radiant Water Outlet Node;  !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Reheat Bypass Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Reheat Bypass,           !- Component 1 Name",
        "    Reheat Bypass Inlet Node,!- Component 1 Inlet Node Name",
        "    Reheat Bypass Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Reheat Bypass,           !- Name",
        "    Reheat Bypass Inlet Node,!- Inlet Node Name",
        "    Reheat Bypass Outlet Node;  !- Outlet Node Name",

        "  Connector:Splitter,",
        "    Reheat Splitter,         !- Name",
        "    Reheat Inlet Branch,     !- Inlet Branch Name",
        "    Zone 1 Radiant Branch,   !- Outlet Branch 4 Name",
        "    Reheat Bypass Branch;    !- Outlet Branch 7 Name",

        "  Connector:Mixer,",
        "    Reheat Mixer,            !- Name",
        "    Reheat Outlet Branch,    !- Outlet Branch Name",
        "    Zone 1 Radiant Branch,   !- Inlet Branch 4 Name",
        "    Reheat Bypass Branch;    !- Inlet Branch 7 Name",

        "  Connector:Splitter,",
        "    Heating Supply Splitter, !- Name",
        "    Heating Supply Inlet Branch,  !- Inlet Branch Name",
        "    Heating Purchased Hot Water Branch,  !- Outlet Branch 1 Name",
        "    Heating Supply Bypass Branch;  !- Outlet Branch 2 Name",

        "  Connector:Mixer,",
        "    Heating Supply Mixer,    !- Name",
        "    Heating Supply Outlet Branch,  !- Outlet Branch Name",
        "    Heating Purchased Hot Water Branch,  !- Inlet Branch 1 Name",
        "    Heating Supply Bypass Branch;  !- Inlet Branch 2 Name",

        "  PlantEquipmentOperationSchemes,",
        "    Hot Loop Operation,      !- Name",
        "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
        "    Purchased Only,          !- Control Scheme 1 Name",
        "    ON;                      !- Control Scheme 1 Schedule Name",

        "  PlantEquipmentOperation:HeatingLoad,",
        "    Purchased Only,          !- Name",
        "    0,                       !- Load Range 1 Lower Limit {W}",
        "    1000000,                 !- Load Range 1 Upper Limit {W}",
        "    heating plant;           !- Range 1 Equipment List Name",

        "  PlantEquipmentList,",
        "    heating plant,           !- Name",
        "    DistrictHeating,         !- Equipment 1 Object Type",
        "    Purchased Heating;       !- Equipment 1 Name",

        "  Pump:VariableSpeed,",
        "    HW Circ Pump,            !- Name",
        "    HW Supply Inlet Node,    !- Inlet Node Name",
        "    HW Pump Outlet Node,     !- Outlet Node Name",
        "    .0043,                   !- Rated Flow Rate {m3/s}",
        "    300000,                  !- Rated Pump Head {Pa}",
        "    2000,                    !- Rated Power Consumption {W}",
        "    .87,                     !- Motor Efficiency",
        "    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "    0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "    0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "    0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "    0,                       !- Minimum Flow Rate {m3/s}",
        "    INTERMITTENT;            !- Pump Control Type",

        "  DistrictHeating,",
        "    Purchased Heating,       !- Name",
        "    Purchased Heat Inlet Node,  !- Hot Water Inlet Node Name",
        "    Purchased Heat Outlet Node,  !- Hot Water Outlet Node Name",
        "    1000000;                 !- Nominal Capacity {W}",

        "  Sizing:Plant,",
        "    Chilled Water Loop,      !- Plant or Condenser Loop Name",
        "    cooling,                 !- Loop Type",
        "    6.7,                     !- Design Loop Exit Temperature {C}",
        "    2;                       !- Loop Design Temperature Difference {deltaC}",

        "  PlantLoop,",
        "    Chilled Water Loop,      !- Name",
        "    Water,                   !- Fluid Type",
        "    ,                        !- User Defined Fluid Type",
        "    CW Loop Operation,       !- Plant Equipment Operation Scheme Name",
        "    CW Supply Outlet Node,   !- Loop Temperature Setpoint Node Name",
        "    98,                      !- Maximum Loop Temperature {C}",
        "    1,                       !- Minimum Loop Temperature {C}",
        "    0.0011,                  !- Maximum Loop Flow Rate {m3/s}",
        "    0,                       !- Minimum Loop Flow Rate {m3/s}",
        "    autocalculate,           !- Plant Loop Volume {m3}",
        "    CW Supply Inlet Node,    !- Plant Side Inlet Node Name",
        "    CW Supply Outlet Node,   !- Plant Side Outlet Node Name",
        "    Cooling Supply Side Branches,  !- Plant Side Branch List Name",
        "    Cooling Supply Side Connectors,  !- Plant Side Connector List Name",
        "    CW Demand Inlet Node,    !- Demand Side Inlet Node Name",
        "    CW Demand Outlet Node,   !- Demand Side Outlet Node Name",
        "    Cooling Demand Side Branches,  !- Demand Side Branch List Name",
        "    Cooling Demand Side Connectors,  !- Demand Side Connector List Name",
        "    Optimal,                 !- Load Distribution Scheme",
        "    ,                        !- Availability Manager List Name",
        "    ,                        !- Plant Loop Demand Calculation Scheme",
        "    ,                        !- Common Pipe Simulation",
        "    ,                        !- Pressure Simulation Type",
        "    2.0;                     !- Loop Circulation Time {minutes}",

        "  SetpointManager:Scheduled,",
        "    Chilled Water Loop Setpoint Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    CW Loop Temp Schedule,   !- Schedule Name",
        "    Chilled Water Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",

        "	Schedule:Compact,",
        "	 CW LOOP TEMP SCHEDULE, !- Name",
        "	 TEMPERATURE,           !- Schedule Type Limits Name",
        "	 Through: 12/31,        !- Field 1",
        "	 For: Alldays,          !- Field 2",
        "	 Until: 24:00, 10.0;    !- Field 3",

        "  NodeList,",
        "    Chilled Water Loop Setpoint Node List,  !- Name",
        "    CW Supply Outlet Node;   !- Node 1 Name",

        "  BranchList,",
        "    Cooling Supply Side Branches,  !- Name",
        "    CW Pump Branch,          !- Branch 1 Name",
        "    Purchased Cooling Branch,!- Branch 4 Name",
        "    Supply Bypass Branch,    !- Branch 5 Name",
        "    Cooling Supply Outlet;   !- Branch 6 Name",

        "  BranchList,",
        "    Cooling Demand Side Branches,  !- Name",
        "    Cooling Demand Inlet,    !- Branch 1 Name",
        "    Zone 1 Cooling Branch,   !- Branch 2 Name",
        "    Demand Bypass Branch,    !- Branch 3 Name",
        "    Cooling Demand Outlet;   !- Branch 4 Name",

        "  ConnectorList,",
        "    Cooling Supply Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    CW Loop Splitter,        !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    CW Loop Mixer;           !- Connector 2 Name",

        "  ConnectorList,",
        "    Cooling Demand Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    CW Demand Splitter,      !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    CW Demand Mixer;         !- Connector 2 Name",

        "  Branch,",
        "    Cooling Demand Inlet,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Demand Side Inlet Pipe,  !- Component 1 Name",
        "    CW Demand Inlet Node,    !- Component 1 Inlet Node Name",
        "    CW Demand Entrance Pipe Outlet Node;  !- Component 1 Outlet Node Name",

        "  Schedule:Compact,",
        "    RADIANT COOLING SETPOINTS,  !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,26.0;       !- Field 3",

        "  Pipe:Adiabatic,",
        "    Demand Side Inlet Pipe,  !- Name",
        "    CW Demand Inlet Node,    !- Inlet Node Name",
        "    CW Demand Entrance Pipe Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    Zone 1 Cooling Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    ZoneHVAC:LowTemperatureRadiant:VariableFlow,  !- Component 1 Object Type",
        "    West Zone Radiant Floor,   !- Component 1 Name",
        "    Zone 1 Cooling Water Inlet Node,  !- Component 1 Inlet Node Name",
        "    Zone 1 Cooling Water Outlet Node; !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Demand Bypass Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Demand Side Bypass,      !- Component 1 Name",
        "    CW Demand Bypass Inlet Node,  !- Component 1 Inlet Node Name",
        "    CW Demand Bypass Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Demand Side Bypass,      !- Name",
        "    CW Demand Bypass Inlet Node,  !- Inlet Node Name",
        "    CW Demand Bypass Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    Cooling Demand Outlet,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    CW Demand Side Outlet Pipe,  !- Component 1 Name",
        "    CW Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
        "    CW Demand Outlet Node;   !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    CW Demand Side Outlet Pipe,  !- Name",
        "    CW Demand Exit Pipe Inlet Node,  !- Inlet Node Name",
        "    CW Demand Outlet Node;   !- Outlet Node Name",

        "  Branch,",
        "    Cooling Supply Outlet,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Supply Side Outlet Pipe, !- Component 1 Name",
        "    Supply Side Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
        "    CW Supply Outlet Node;   !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Supply Side Outlet Pipe, !- Name",
        "    Supply Side Exit Pipe Inlet Node,  !- Inlet Node Name",
        "    CW Supply Outlet Node;   !- Outlet Node Name",

        "  Branch,",
        "    CW Pump Branch,          !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pump:VariableSpeed,      !- Component 1 Object Type",
        "    Circ Pump,               !- Component 1 Name",
        "    CW Supply Inlet Node,    !- Component 1 Inlet Node Name",
        "    CW Pump Outlet Node;     !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Purchased Cooling Branch,!- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    DistrictCooling,         !- Component 1 Object Type",
        "    Purchased Cooling,       !- Component 1 Name",
        "    Purchased Cooling Inlet Node,  !- Component 1 Inlet Node Name",
        "    Purchased Cooling Outlet Node; !- Component 1 Outlet Node Name",

        "  Branch,",
        "    Supply Bypass Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Supply Side Bypass,      !- Component 1 Name",
        "    CW Supply Bypass Inlet Node,  !- Component 1 Inlet Node Name",
        "    CW Supply Bypass Outlet Node; !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    Supply Side Bypass,      !- Name",
        "    CW Supply Bypass Inlet Node,  !- Inlet Node Name",
        "    CW Supply Bypass Outlet Node; !- Outlet Node Name",

        "  Connector:Splitter,",
        "    CW Loop Splitter,        !- Name",
        "    CW Pump Branch,          !- Inlet Branch Name",
        "    Purchased Cooling Branch,!- Outlet Branch 3 Name",
        "    Supply Bypass Branch;    !- Outlet Branch 4 Name",

        "  Connector:Mixer,",
        "    CW Loop Mixer,           !- Name",
        "    Cooling Supply Outlet,   !- Outlet Branch Name",
        "    Purchased Cooling Branch,!- Inlet Branch 3 Name",
        "    Supply Bypass Branch;    !- Inlet Branch 4 Name",

        "  Connector:Splitter,",
        "    CW Demand Splitter,      !- Name",
        "    Cooling Demand Inlet,    !- Inlet Branch Name",
        "    Demand Bypass Branch,    !- Outlet Branch 1 Name",
        "    Zone 1 Cooling Branch;   !- Outlet Branch 2 Name",

        "  Connector:Mixer,",
        "    CW Demand Mixer,         !- Name",
        "    Cooling Demand Outlet,   !- Outlet Branch Name",
        "    Zone 1 Cooling Branch,   !- Inlet Branch 1 Name",
        "    Demand Bypass Branch;    !- Inlet Branch 2 Name",

        "  PlantEquipmentOperationSchemes,",
        "    CW Loop Operation,       !- Name",
        "    PlantEquipmentOperation:CoolingLoad,  !- Control Scheme 1 Object Type",
        "    Always Operation,        !- Control Scheme 1 Name",
        "    Always;                  !- Control Scheme 1 Schedule Name",

        "  Schedule:Compact,",
        "    Always,                  !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  PlantEquipmentOperation:CoolingLoad,",
        "    Always Operation,        !- Name",
        "    0,                       !- Load Range 1 Lower Limit {W}",
        "    70000,                   !- Load Range 1 Upper Limit {W}",
        "    Purchased Only;          !- Range 3 Equipment List Name",

        "  PlantEquipmentList,",
        "    Purchased Only,          !- Name",
        "    DistrictCooling,         !- Equipment 1 Object Type",
        "    Purchased Cooling;       !- Equipment 1 Name",

        "  DistrictCooling,",
        "    Purchased Cooling,             !- Name",
        "    Purchased Cooling Inlet Node,  !- Chilled Water Inlet Node Name",
        "    Purchased Cooling Outlet Node, !- Chilled Water Outlet Node Name",
        "    680000;                        !- Nominal Capacity {W}",

        "  Pump:VariableSpeed,",
        "    Circ Pump,               !- Name",
        "    CW Supply Inlet Node,    !- Inlet Node Name",
        "    CW Pump Outlet Node,     !- Outlet Node Name",
        "    .0011,                   !- Rated Flow Rate {m3/s}",
        "    300000,                  !- Rated Pump Head {Pa}",
        "    500,                     !- Rated Power Consumption {W}",
        "    .87,                     !- Motor Efficiency",
        "    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "    0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "    0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "    0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "    0,                       !- Minimum Flow Rate {m3/s}",
        "    INTERMITTENT;            !- Pump Control Type",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ("WEST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;

    HeatBalanceManager::SetPreConstructionInputParameters();
    GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetPlantSizingInput();
    GetPlantLoopData();
    GetPlantInput();
    SetupInitialPlantCallingOrder();
    SetupBranchControlTypes();
    DataSurfaces::WorldCoordSystem = true;
    GetSurfaceListsInputs();

    ErrorsFound = false;
    SetupZoneGeometry(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    GetLowTempRadiantSystem();
    EXPECT_EQ(1, LowTempRadiantSystem::NumOfHydrLowTempRadSys);
    EXPECT_EQ("WEST ZONE RADIANT FLOOR", RadSysTypes(RadSysNum).Name);
    EXPECT_EQ(LowTempRadiantSystem::HydronicSystem, RadSysTypes(RadSysNum).SystemType);

    ErrorsFound = false;
    PlantUtilities::ScanPlantLoopsForObject(HydrRadSys(RadSysNum).Name, TypeOf_LowTempRadiant_VarFlow, HydrRadSys(RadSysNum).HWLoopNum,
                                            HydrRadSys(RadSysNum).HWLoopSide, HydrRadSys(RadSysNum).HWBranchNum, HydrRadSys(RadSysNum).HWCompNum, _,
                                            _, _, HydrRadSys(RadSysNum).HotWaterInNode, _, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ErrorsFound = false;
    PlantUtilities::ScanPlantLoopsForObject(HydrRadSys(RadSysNum).Name, TypeOf_LowTempRadiant_VarFlow, HydrRadSys(RadSysNum).CWLoopNum,
                                            HydrRadSys(RadSysNum).CWLoopSide, HydrRadSys(RadSysNum).CWBranchNum, HydrRadSys(RadSysNum).CWCompNum, _,
                                            _, _, HydrRadSys(RadSysNum).ColdWaterInNode, _, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    DataSizing::CurZoneEqNum = 1;
    ZoneSizingRunDone = true;
    FinalZoneSizing.allocate(DataSizing::CurZoneEqNum);
    ZoneEqSizing.allocate(DataSizing::CurZoneEqNum);

    ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::HeatingCapacitySizing) = DataSizing::FractionOfAutosizedHeatingCapacity;
    ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::CoolingCapacitySizing) = DataSizing::FractionOfAutosizedCoolingCapacity;
    // heating capacity sizing calculation
    FinalZoneSizing(DataSizing::CurZoneEqNum).NonAirSysDesHeatLoad = 10000.0;
    HeatingCapacity = FinalZoneSizing(DataSizing::CurZoneEqNum).NonAirSysDesHeatLoad * HydrRadSys(RadSysNum).ScaledHeatingCapacity;
    // cooling capacity sizing calculation
    FinalZoneSizing(DataSizing::CurZoneEqNum).NonAirSysDesCoolLoad = 10000.0;
    CoolingCapacity = FinalZoneSizing(DataSizing::CurZoneEqNum).NonAirSysDesCoolLoad * HydrRadSys(RadSysNum).ScaledCoolingCapacity;
    // hot water flow rate sizing calculation
    Density = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidName, 60.0, PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidIndex,
                               "AutosizeLowTempRadiantVariableFlowTest");
    Cp = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidName, 60.0, PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidIndex,
                               "AutosizeLowTempRadiantVariableFlowTest");
    HotWaterFlowRate = HeatingCapacity / (PlantSizData(1).DeltaT * Cp * Density);
    // chilled water flow rate sizing calculation
    Density = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidName, 5.0, PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidIndex,
                               "AutosizeLowTempRadiantVariableFlowTest");
    Cp = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidName, 5.0, PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidIndex,
                               "AutosizeLowTempRadiantVariableFlowTest");
    ChilledWaterFlowRate = CoolingCapacity / (PlantSizData(2).DeltaT * Cp * Density);
    // tuble length sizing calculation
    HydrRadSys(RadSysNum).TotalSurfaceArea = Surface(HydrRadSys(RadSysNum).SurfacePtr(1)).Area;
    TubeLengthDes = HydrRadSys(RadSysNum).TotalSurfaceArea / 0.1524; // tube length uses the construction perpendicular spacing

    // do autosize calculations
    SizeLowTempRadiantSystem(RadSysNum, RadSysTypes(1).SystemType);
    // Test autosized heat/cool capacity
    EXPECT_EQ(HeatingCapacity, HydrRadSys(RadSysNum).ScaledHeatingCapacity);
    EXPECT_EQ(CoolingCapacity, HydrRadSys(RadSysNum).ScaledCoolingCapacity);
    // Test autosized heat/cool flow rate
    EXPECT_EQ(HotWaterFlowRate, HydrRadSys(RadSysNum).WaterVolFlowMaxHeat);
    EXPECT_EQ(ChilledWaterFlowRate, HydrRadSys(RadSysNum).WaterVolFlowMaxCool);
    // Test autosized tube length
    EXPECT_EQ(TubeLengthDes, HydrRadSys(RadSysNum).TubeLength);
}

TEST_F(LowTempRadiantSystemTest, InitLowTempRadiantSystem)
{

    bool InitErrorFound;

    RadSysNum = 1;
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    TotalNumOfRadSystems = 0;
    BeginEnvrnFlag = false;
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    BeginTimeStepFlag = false;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).WaterVolFlowMax = 1.0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.2;

    CFloRadSys(RadSysNum).CoolingSystem = true;
    CFloRadSys(RadSysNum).HeatingSystem = false;
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    EXPECT_EQ(3.0, CFloRadSys(RadSysNum).ChWaterMassFlowRate);
    EXPECT_EQ(0.0, CFloRadSys(RadSysNum).WaterMassFlowRate);

    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = true;
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    EXPECT_EQ(2.0, CFloRadSys(RadSysNum).HotWaterMassFlowRate);
    EXPECT_EQ(0.0, CFloRadSys(RadSysNum).WaterMassFlowRate);
}

TEST_F(LowTempRadiantSystemTest, InitLowTempRadiantSystemCFloPump)
{

    bool InitErrorFound;

    // Test 1: with autosize for max flow, nothing should happen
    LowTempRadiantSystem::clear_state();
    RadSysNum = 1;
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys.allocate(NumOfCFloLowTempRadSys);
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    CFloRadSys(RadSysNum).Name = "NoNameRadSys";
    TotalNumOfRadSystems = 0;
    BeginEnvrnFlag = false;
    DataZoneEquipment::ZoneEquipInputsFilled = false;
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    BeginTimeStepFlag = false;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.0;
    CFloRadSys(RadSysNum).PumpEffic = 0.0;
    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = false;

    CFloRadSys(RadSysNum).WaterVolFlowMax = AutoSize;
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    EXPECT_EQ(CFloRadSys(RadSysNum).PumpEffic, 0.0);
    EXPECT_EQ(InitErrorFound, false);

    // Test 2: pump efficiency below 50%
    LowTempRadiantSystem::clear_state();
    RadSysNum = 1;
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys.allocate(NumOfCFloLowTempRadSys);
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    CFloRadSys(RadSysNum).Name = "NoNameRadSys";
    TotalNumOfRadSystems = 0;
    BeginEnvrnFlag = false;
    DataZoneEquipment::ZoneEquipInputsFilled = false;
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    BeginTimeStepFlag = false;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.0;
    CFloRadSys(RadSysNum).PumpEffic = 0.0;
    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = false;

    CFloRadSys(RadSysNum).WaterVolFlowMax = 0.4; // because of how other parameters are set, this value is equal to the pump efficiency
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    std::string const error_string02 =
        delimited_string({"   ** Warning ** Check input.  Calc Pump Efficiency=" + General::RoundSigDigits(CFloRadSys(RadSysNum).PumpEffic, 5) +
                          "% which is less than 50%, for pump in radiant system " + CFloRadSys(RadSysNum).Name});
    EXPECT_EQ(CFloRadSys(RadSysNum).WaterVolFlowMax, CFloRadSys(RadSysNum).PumpEffic);
    EXPECT_TRUE(compare_err_stream(error_string02, true));
    EXPECT_EQ(InitErrorFound, false);

    // Test 3: pump efficiency between 95% and 100%
    LowTempRadiantSystem::clear_state();
    RadSysNum = 1;
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys.allocate(NumOfCFloLowTempRadSys);
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    CFloRadSys(RadSysNum).Name = "NoNameRadSys";
    TotalNumOfRadSystems = 0;
    BeginEnvrnFlag = false;
    DataZoneEquipment::ZoneEquipInputsFilled = false;
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    BeginTimeStepFlag = false;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.0;
    CFloRadSys(RadSysNum).PumpEffic = 0.0;
    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = false;

    CFloRadSys(RadSysNum).WaterVolFlowMax = 0.98; // because of how other parameters are set, this value is equal to the pump efficiency
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    std::string const error_string03 =
        delimited_string({"   ** Warning ** Check input.  Calc Pump Efficiency=" + General::RoundSigDigits(CFloRadSys(RadSysNum).PumpEffic, 5) +
                          "% is approaching 100%, for pump in radiant system " + CFloRadSys(RadSysNum).Name});
    EXPECT_EQ(CFloRadSys(RadSysNum).WaterVolFlowMax, CFloRadSys(RadSysNum).PumpEffic);
    EXPECT_TRUE(compare_err_stream(error_string03, true));
    EXPECT_EQ(InitErrorFound, false);

    // Test 4: pump efficiency over 100%
    LowTempRadiantSystem::clear_state();
    RadSysNum = 1;
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys.allocate(NumOfCFloLowTempRadSys);
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    CFloRadSys(RadSysNum).Name = "NoNameRadSys";
    TotalNumOfRadSystems = 0;
    BeginEnvrnFlag = false;
    DataZoneEquipment::ZoneEquipInputsFilled = false;
    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    BeginTimeStepFlag = false;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.0;
    CFloRadSys(RadSysNum).PumpEffic = 0.0;
    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = false;

    CFloRadSys(RadSysNum).WaterVolFlowMax = 1.23; // because of how other parameters are set, this value is equal to the pump efficiency
    InitLowTempRadiantSystem(false, RadSysNum, SystemType, InitErrorFound);
    std::string const error_string04 =
        delimited_string({"   ** Severe  ** Check input.  Calc Pump Efficiency=" + General::RoundSigDigits(CFloRadSys(RadSysNum).PumpEffic, 5) +
                          "% which is bigger than 100%, for pump in radiant system " + CFloRadSys(RadSysNum).Name});
    EXPECT_EQ(CFloRadSys(RadSysNum).WaterVolFlowMax, CFloRadSys(RadSysNum).PumpEffic);
    EXPECT_TRUE(compare_err_stream(error_string04, true));
    EXPECT_EQ(InitErrorFound, true);
}

TEST_F(EnergyPlusFixture, LowTempElecRadSurfaceGroupTest)
{

    int RadSysNum(1);

    std::string const idf_objects = delimited_string({

        "  ZoneHVAC:LowTemperatureRadiant:Electric,",
        "    West Zone Radiant Floor, !- Name",
        "    RadiantSysAvailSched,    !- Availability Schedule Name",
        "    West Zone,               !- Zone Name",
        "    West Zone Surface Group, !- Surface Name or Radiant Surface Group Name",
        "    heatingdesigncapacity,   !- Heating Design Capacity Method",
        "    100,                     !- Heating Design Capacity{ W }",
        "    ,                        !- Heating Design Capacity Per Floor Area{ W/m2 }",
        "    1.0,                     !- Fraction of Autosized Heating Design Capacity",
        "    MeanAirTemperature,      !- Temperature Control Type",
        "    2.0,                     !- Heating Throttling Range {deltaC}",
        "    Radiant Heating Setpoints;  !- Heating Control Temperature Schedule Name",

        "  ZoneHVAC:LowTemperatureRadiant:Electric,",
        "    East Zone Radiant Floor, !- Name",
        "    RadiantSysAvailSched,    !- Availability Schedule Name",
        "    East Zone,               !- Zone Name",
        "    East Zone Surface Group, !- Surface Name or Radiant Surface Group Name",
        "    heatingdesigncapacity,   !- Heating Design Capacity Method",
        "    100,                     !- Heating Design Capacity{ W }",
        "    ,                        !- Heating Design Capacity Per Floor Area{ W/m2 }",
        "    1.0,                     !- Fraction of Autosized Heating Design Capacity",
        "    MeanAirTemperature,      !- Temperature Control Type",
        "    2.0,                     !- Heating Throttling Range {deltaC}",
        "    Radiant Heating Setpoints;  !- Heating Control Temperature Schedule Name",

        "  ZoneHVAC:LowTemperatureRadiant:SurfaceGroup,",
        "    East Zone Surface Group, !- Name",
        "    Zn002:Flr001,             !- Surface 1 Name",
        "     0.5,                     !- Flow Fraction for Surface 1",
        "    Zn002:Flr002,             !- Surface 2 Name",
        "     0.5;                     !- Flow Fraction for Surface 2",

        "  ZoneHVAC:LowTemperatureRadiant:SurfaceGroup,",
        "    West Zone Surface Group, !- Name",
        "    Zn001:Flr001,             !- Surface 1 Name",
        "     0.5,                     !- Flow Fraction for Surface 1",
        "    Zn001:Flr002,             !- Surface 2 Name",
        "     0.5;                     !- Flow Fraction for Surface 2",

        "  Schedule:Compact,",
        "    RADIANTSYSAVAILSCHED,    !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1.00;       !- Field 3",

        "  Schedule:Compact,",
        "    Radiant Heating Setpoints,   !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,20.0;       !- Field 3",

    });
    ASSERT_TRUE(process_idf(idf_objects));

    Zone.allocate(2);
    Zone(1).Name = "WEST ZONE";
    Zone(2).Name = "EAST ZONE";

    DataSurfaces::TotSurfaces = 4;
    Surface.allocate(4);
    Surface(1).Name = "ZN001:FLR001";
    Surface(1).ZoneName = "WEST ZONE";
    Surface(1).Zone = 1;
    Surface(2).Name = "ZN001:FLR002";
    Surface(2).ZoneName = "WEST ZONE";
    Surface(2).Zone = 1;
    Surface(3).Name = "ZN002:FLR001";
    Surface(3).ZoneName = "EAST ZONE";
    Surface(3).Zone = 2;
    Surface(4).Name = "ZN002:FLR002";
    Surface(4).ZoneName = "EAST ZONE";
    Surface(4).Zone = 2;

    GetLowTempRadiantSystem();
    EXPECT_EQ(2, LowTempRadiantSystem::NumOfElecLowTempRadSys);
    EXPECT_EQ("WEST ZONE RADIANT FLOOR", RadSysTypes(RadSysNum).Name);
    EXPECT_EQ("EAST ZONE RADIANT FLOOR", RadSysTypes(RadSysNum + 1).Name);
    EXPECT_EQ(LowTempRadiantSystem::ElectricSystem, RadSysTypes(RadSysNum).SystemType);
    EXPECT_EQ(LowTempRadiantSystem::ElecRadSys(1).ZoneName, "WEST ZONE");
    EXPECT_EQ(LowTempRadiantSystem::ElecRadSys(1).SurfListName, "WEST ZONE SURFACE GROUP");
    // the 2nd surface list group holds data for 1st elec rad sys (#5958)
    EXPECT_EQ(DataSurfaceLists::SurfList(2).Name, "WEST ZONE SURFACE GROUP");
    EXPECT_EQ(LowTempRadiantSystem::ElecRadSys(1).NumOfSurfaces, 2);
    // surface ptr's are not set correctly when elec rad sys "index" (e.g., ElecRadSys(N)) is not the same as surface group "index"
    // #5958 fixes this issue
    EXPECT_EQ(LowTempRadiantSystem::ElecRadSys(1).SurfacePtr(1), 1);
    EXPECT_EQ(LowTempRadiantSystem::ElecRadSys(1).SurfacePtr(2), 2);
}

TEST_F(LowTempRadiantSystemTest, CalcLowTempCFloRadiantSystem_OperationMode)
{
    // # ZoneHVAC:LowTemperatureRadiant:VariableFlow array bounds error #5905

    Real64 Load;

    RadSysNum = 1;
    LowTempRadiantSystem::clear_state();
    SystemType = ConstantFlowSystem;
    NumOfCFloLowTempRadSys = 1;
    CFloRadSys.allocate(NumOfCFloLowTempRadSys);
    Schedule.allocate(3);
    DataHeatBalFanSys::MAT.allocate(1);
    Schedule(1).CurrentValue = 1;
    Schedule(2).CurrentValue = 22.0;
    Schedule(3).CurrentValue = 25.0;
    DataHeatBalFanSys::MAT(1) = 21.0;
    CFloRadSys(RadSysNum).NumOfSurfaces = 0;
    TotalNumOfRadSystems = 0;
    CFloRadSys(RadSysNum).ZonePtr = 1;
    CFloRadSys(RadSysNum).SchedPtr = 1;
    CFloRadSys(RadSysNum).ControlType = 1;
    CFloRadSys(RadSysNum).HotCtrlHiTempSchedPtr = 2;
    CFloRadSys(RadSysNum).ColdCtrlLoTempSchedPtr = 3;

    CFloRadSys(RadSysNum).HotWaterInNode = 0;
    CFloRadSys(RadSysNum).ColdWaterInNode = 0;
    CFloRadSys(RadSysNum).VolFlowSchedPtr = 0;
    CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    CFloRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = 2.0;
    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = 3.0;
    CFloRadSys(RadSysNum).CWLoopNum = 0;
    CFloRadSys(RadSysNum).HWLoopNum = 0;
    CFloRadSys(RadSysNum).WaterVolFlowMax = 1.0;
    CFloRadSys(RadSysNum).NomPumpHead = 1.0;
    CFloRadSys(RadSysNum).NomPowerUse = 1.0;
    CFloRadSys(RadSysNum).MotorEffic = 1.2;

    // heating
    CFloRadSys(RadSysNum).CoolingSystem = true;
    CFloRadSys(RadSysNum).HeatingSystem = false;
    Load = 1000.0;
    CalcLowTempCFloRadiantSystem(RadSysNum, Load);
    EXPECT_EQ(NotOperating, OperatingMode);

    // Cooling
    CFloRadSys(RadSysNum).CoolingSystem = false;
    CFloRadSys(RadSysNum).HeatingSystem = true;
    DataHeatBalFanSys::MAT(1) = 26.0;
    CalcLowTempCFloRadiantSystem(RadSysNum, Load);
    EXPECT_EQ(NotOperating, OperatingMode);

    CFloRadSys.deallocate();
    Schedule.deallocate();
    DataHeatBalFanSys::MAT.deallocate();
}

TEST_F(LowTempRadiantSystemTest, CalcLowTempHydrRadiantSystem_OperationMode)
{
    // # ZoneHVAC:LowTemperatureRadiant:VariableFlow array bounds error #5905

    Real64 Load;

    RadSysNum = 1;
    LowTempRadiantSystem::clear_state();

    //	SystemType = ConstantFlowSystem;
    NumOfHydrLowTempRadSys = 1;
    HydrRadSys.allocate(NumOfHydrLowTempRadSys);
    Schedule.allocate(3);
    DataHeatBalFanSys::MAT.allocate(1);
    Schedule(1).CurrentValue = 1;
    Schedule(2).CurrentValue = 22.0;
    Schedule(3).CurrentValue = 25.0;
    DataHeatBalFanSys::MAT(1) = 21.0;
    HydrRadSys(RadSysNum).NumOfSurfaces = 0;
    TotalNumOfRadSystems = 0;
    HydrRadSys(RadSysNum).ZonePtr = 1;
    HydrRadSys(RadSysNum).SchedPtr = 1;
    HydrRadSys(RadSysNum).ControlType = 1;
    HydrRadSys(RadSysNum).HotSetptSchedPtr = 2;
    HydrRadSys(RadSysNum).ColdSetptSchedPtr = 3;

    HydrRadSys(RadSysNum).HotWaterInNode = 0;
    HydrRadSys(RadSysNum).ColdWaterInNode = 0;
    HydrRadSys(RadSysNum).EMSOverrideOnWaterMdot = false;
    HydrRadSys(RadSysNum).WaterMassFlowRate = 1.0;
    HydrRadSys(RadSysNum).CWLoopNum = 0;
    HydrRadSys(RadSysNum).HWLoopNum = 0;

    // heating
    OperatingMode = 0;
    HydrRadSys(RadSysNum).CoolingSystem = true;
    HydrRadSys(RadSysNum).HeatingSystem = false;
    Load = 1000.0;
    CalcLowTempHydrRadiantSystem(RadSysNum, Load);
    EXPECT_EQ(0, LowTempRadiantSystem::OperatingMode);

    // Cooling
    HydrRadSys(RadSysNum).CoolingSystem = false;
    HydrRadSys(RadSysNum).HeatingSystem = true;
    DataHeatBalFanSys::MAT(1) = 26.0;
    CalcLowTempHydrRadiantSystem(RadSysNum, Load);
    EXPECT_EQ(NotOperating, OperatingMode);

    HydrRadSys.deallocate();
    Schedule.deallocate();
    DataHeatBalFanSys::MAT.deallocate();
}

TEST_F(LowTempRadiantSystemTest, SizeRadSysTubeLengthTest)
{
    // # Low Temperature Radiant System (variable and constant flow) autosizing tube length issue #6202
    Real64 FuncCalc;
    int RadSysType;

    RadSysNum = 1;
    LowTempRadiantSystem::clear_state();

    HydrRadSys.allocate(3);
    CFloRadSys.allocate(3);

    HydrRadSys(1).NumOfSurfaces = 1;
    HydrRadSys(1).SurfacePtr.allocate(1);
    HydrRadSys(1).SurfacePtr(1) = 1;
    HydrRadSys(2).NumOfSurfaces = 2;
    HydrRadSys(2).SurfacePtr.allocate(2);
    HydrRadSys(2).SurfacePtr(1) = 1;
    HydrRadSys(2).SurfacePtr(2) = 2;
    HydrRadSys(3).NumOfSurfaces = 1;
    HydrRadSys(3).SurfacePtr.allocate(1);
    HydrRadSys(3).SurfacePtr(1) = 3;

    CFloRadSys(1).NumOfSurfaces = 1;
    CFloRadSys(1).SurfacePtr.allocate(1);
    CFloRadSys(1).SurfacePtr(1) = 1;
    CFloRadSys(2).NumOfSurfaces = 2;
    CFloRadSys(2).SurfacePtr.allocate(2);
    CFloRadSys(2).SurfacePtr(1) = 1;
    CFloRadSys(2).SurfacePtr(2) = 2;
    CFloRadSys(3).NumOfSurfaces = 1;
    CFloRadSys(3).SurfacePtr.allocate(1);
    CFloRadSys(3).SurfacePtr(1) = 3;

    Surface.allocate(3);
    Surface(1).Construction = 1;
    Surface(1).Area = 100.0;
    Surface(2).Construction = 2;
    Surface(2).Area = 200.0;
    Surface(3).Construction = 3;
    Surface(3).Area = 300.0;

    Construct.allocate(3);
    Construct(1).ThicknessPerpend = 0.05;
    Construct(2).ThicknessPerpend = 0.125;

    // Test 1: Hydronic radiant system 1 (one surface)
    RadSysType = HydronicSystem;
    RadSysNum = 1;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 1000.0, 0.1);

    // Test 2: Hydronic radiant system 2 (two surfaces)
    RadSysType = HydronicSystem;
    RadSysNum = 2;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 1800.0, 0.1);

    // Test 3: Constant flow radiant system 1 (one surface)
    RadSysType = ConstantFlowSystem;
    RadSysNum = 1;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 1000.0, 0.1);

    // Test 4: Constant flow radiant system 2 (two surfaces)
    RadSysType = ConstantFlowSystem;
    RadSysNum = 2;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 1800.0, 0.1);

    // Test 5: Hydronic radiant system 3 (thickness out of range, low side)
    RadSysType = HydronicSystem;
    RadSysNum = 3;
    Construct(3).ThicknessPerpend = 0.004;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 2000.0, 0.1);

    // Test 6: Hydronic radiant system 3 (thickness out of range, high side)
    RadSysType = HydronicSystem;
    RadSysNum = 3;
    Construct(3).ThicknessPerpend = 0.6;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 2000.0, 0.1);

    // Test 7: Constant flow radiant system 3 (thickness out of range, low side)
    RadSysType = ConstantFlowSystem;
    RadSysNum = 3;
    Construct(3).ThicknessPerpend = 0.004;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 2000.0, 0.1);

    // Test 8: Constant flow radiant system 3 (thickness out of range, high side)
    RadSysType = ConstantFlowSystem;
    RadSysNum = 3;
    Construct(3).ThicknessPerpend = 0.6;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 2000.0, 0.1);

    // Test 9: Wrong system type
    RadSysType = 0;
    RadSysNum = 1;
    FuncCalc = SizeRadSysTubeLength(RadSysType, RadSysNum);
    EXPECT_NEAR(FuncCalc, 60.0, 0.1);
}
