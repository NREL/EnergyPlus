// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::SwimmingPool Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SwimmingPool.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SwimmingPool;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataPlant;

TEST_F(EnergyPlusFixture, SwimmingPool_MakeUpWaterVolFlow)
{
    // Tests for MakeUpWaterVolFlowFunct
    EXPECT_EQ(0.05, MakeUpWaterVolFlowFunct(5, 100));
    EXPECT_NEAR(0.00392, MakeUpWaterVolFlowFunct(0.1, 25.5), .0001);
    EXPECT_EQ(-180, MakeUpWaterVolFlowFunct(-9, .05));
    EXPECT_NE(10, MakeUpWaterVolFlowFunct(10, 0.01));

    // Tests for MakeUpWaterVolFunct
    EXPECT_EQ(0.05, MakeUpWaterVolFunct(5, 100));
    EXPECT_NEAR(0.00392, MakeUpWaterVolFunct(0.1, 25.5), .0001);
    EXPECT_EQ(-180, MakeUpWaterVolFunct(-9, .05));
    EXPECT_NE(10, MakeUpWaterVolFunct(10, 0.01));
}

TEST_F(EnergyPlusFixture, SwimmingPool_CalcSwimmingPoolEvap)
{
    int SurfNum;
    int PoolNum;
    Real64 MAT;
    Real64 HumRat;
    Real64 EvapRate;

    // Tests for CalcSwimmingPoolEvap--Evaporate Rate Calculation for Swimming Pools
    state->dataSwimmingPools->NumSwimmingPools = 1;
    state->dataSwimmingPools->Pool.allocate(1);
    state->dataSurface->Surface.allocate(1);
    state->dataSurface->Surface(1).Area = 10.0;
    SurfNum = 1;
    PoolNum = 1;
    state->dataEnvrn->OutBaroPress = 101400.0;

    auto &thisPool = state->dataSwimmingPools->Pool(PoolNum);

    // Test 1
    state->dataSwimmingPools->Pool(PoolNum).PoolWaterTemp = 30.0;
    MAT = 20.0;
    HumRat = 0.005;
    state->dataSwimmingPools->Pool(PoolNum).CurActivityFactor = 0.5;
    state->dataSwimmingPools->Pool(PoolNum).CurCoverEvapFac = 0.3;
    thisPool.calcSwimmingPoolEvap(*state, EvapRate, SurfNum, MAT, HumRat);
    EXPECT_NEAR(0.000207, EvapRate, 0.000001);
    EXPECT_NEAR(4250.0, state->dataSwimmingPools->Pool(PoolNum).SatPressPoolWaterTemp, 10.0);
    EXPECT_NEAR(810.0, state->dataSwimmingPools->Pool(PoolNum).PartPressZoneAirTemp, 10.0);

    // Test 2
    state->dataSwimmingPools->Pool(PoolNum).PoolWaterTemp = 27.0;
    MAT = 22.0;
    HumRat = 0.010;
    state->dataSwimmingPools->Pool(PoolNum).CurActivityFactor = 1.0;
    state->dataSwimmingPools->Pool(PoolNum).CurCoverEvapFac = 1.0;
    thisPool.calcSwimmingPoolEvap(*state, EvapRate, SurfNum, MAT, HumRat);
    EXPECT_NEAR(0.000788, EvapRate, 0.000001);
    EXPECT_NEAR(3570.0, state->dataSwimmingPools->Pool(PoolNum).SatPressPoolWaterTemp, 10.0);
    EXPECT_NEAR(1600.0, state->dataSwimmingPools->Pool(PoolNum).PartPressZoneAirTemp, 10.0);
}

TEST_F(EnergyPlusFixture, SwimmingPool_InitSwimmingPoolPlantLoopIndex)
{
    // Tests for InitSwimmingPoolPlantLoopIndex
    state->dataSwimmingPools->NumSwimmingPools = 2;
    state->dataPlnt->TotNumLoops = 2;
    state->dataSwimmingPools->Pool.allocate(state->dataSwimmingPools->NumSwimmingPools);
    state->dataSwimmingPools->Pool(1).Name = "FirstPool";
    state->dataSwimmingPools->Pool(2).Name = "SecondPool";
    state->dataSwimmingPools->Pool(1).WaterInletNode = 1;
    state->dataSwimmingPools->Pool(2).WaterInletNode = 11;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);

    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::SwimmingPool_Indoor;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = "FirstPool";
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::SwimmingPool_Indoor;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp(1).Name = "SecondPool";
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp(1).NodeNumIn = 11;

    // Test 1
    state->dataSwimmingPools->Pool(1).initSwimmingPoolPlantLoopIndex(*state);
    EXPECT_EQ(state->dataSwimmingPools->Pool(1).HWplantLoc.loopNum, 1);
    EXPECT_TRUE(compare_enums(state->dataSwimmingPools->Pool(1).HWplantLoc.loopSideNum, DataPlant::LoopSideLocation::Demand));
    EXPECT_EQ(state->dataSwimmingPools->Pool(1).HWplantLoc.branchNum, 1);
    EXPECT_EQ(state->dataSwimmingPools->Pool(1).HWplantLoc.compNum, 1);

    // Test 2
    state->dataSwimmingPools->Pool(1).MyPlantScanFlagPool = true;
    state->dataSwimmingPools->Pool(2).initSwimmingPoolPlantLoopIndex(*state);
    EXPECT_EQ(state->dataSwimmingPools->Pool(2).HWplantLoc.loopNum, 2);
    EXPECT_TRUE(compare_enums(state->dataSwimmingPools->Pool(2).HWplantLoc.loopSideNum, DataPlant::LoopSideLocation::Supply));
    EXPECT_EQ(state->dataSwimmingPools->Pool(2).HWplantLoc.branchNum, 1);
    EXPECT_EQ(state->dataSwimmingPools->Pool(2).HWplantLoc.compNum, 1);
}

TEST_F(EnergyPlusFixture, SwimmingPool_InitSwimmingPoolPlantNodeFlow)
{
    int constexpr PoolNum = 1;

    // Tests for InitSwimmingPoolPlantLoopIndex
    state->dataSwimmingPools->NumSwimmingPools = 1;
    state->dataPlnt->TotNumLoops = 1;
    state->dataSwimmingPools->Pool.allocate(state->dataSwimmingPools->NumSwimmingPools);

    state->dataSwimmingPools->Pool(1).Name = "FirstPool";
    state->dataSwimmingPools->Pool(1).WaterInletNode = 1;
    state->dataSwimmingPools->Pool(1).WaterOutletNode = 2;
    state->dataSwimmingPools->Pool(1).HWplantLoc.loopNum = 1;
    state->dataSwimmingPools->Pool(1).HWplantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataSwimmingPools->Pool(1).HWplantLoc.branchNum = 1;
    state->dataSwimmingPools->Pool(1).HWplantLoc.compNum = 1;

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);

    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Supply).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::SwimmingPool_Indoor;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = "FirstPool";
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn = 1;

    state->dataLoopNodes->Node.allocate(2);

    // Test 1
    auto &thisPool = state->dataSwimmingPools->Pool(PoolNum);

    state->dataSwimmingPools->Pool(1).WaterMassFlowRate = 0.75;
    state->dataSwimmingPools->Pool(1).WaterMassFlowRateMax = 0.75;
    state->dataSwimmingPools->Pool(1).WaterVolFlowMax = 0.00075;
    state->dataSwimmingPools->Pool(1).MyPlantScanFlagPool = false;
    state->dataSize->SaveNumPlantComps = 0;
    state->dataSize->CompDesWaterFlow.deallocate();
    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(1).MassFlowRateMax = 0.0;
    thisPool.initSwimmingPoolPlantNodeFlow(*state);
    EXPECT_EQ(state->dataSize->CompDesWaterFlow(1).SupNode, 1);
    EXPECT_EQ(state->dataSize->CompDesWaterFlow(1).DesVolFlowRate, 0.00075);

    // Test 2
    state->dataSwimmingPools->Pool(1).WaterMassFlowRate = 0.5;
    state->dataSwimmingPools->Pool(1).WaterMassFlowRateMax = 2.0;
    state->dataSwimmingPools->Pool(1).WaterVolFlowMax = 0.002;
    state->dataSwimmingPools->Pool(1).MyPlantScanFlagPool = false;
    state->dataSize->SaveNumPlantComps = 0;
    state->dataSize->CompDesWaterFlow.deallocate();
    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(1).MassFlowRateMax = 0.0;
    thisPool.initSwimmingPoolPlantNodeFlow(*state);
    EXPECT_EQ(state->dataSize->CompDesWaterFlow(1).SupNode, 1);
    EXPECT_EQ(state->dataSize->CompDesWaterFlow(1).DesVolFlowRate, 0.002);
}

TEST_F(EnergyPlusFixture, SwimmingPool_ErrorCheckSetupPoolSurfaceTest)
{

    // Tests for InitSwimmingPoolPlantLoopIndex
    state->dataSwimmingPools->NumSwimmingPools = 1;
    state->dataSwimmingPools->Pool.allocate(state->dataSwimmingPools->NumSwimmingPools);
    state->dataSurface->Surface.allocate(1);
    state->dataConstruction->Construct.allocate(1);
    state->dataSurface->SurfIsPool.allocate(1);
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool.allocate(1);
    state->dataSurface->SurfMaterialMovInsulInt.allocate(1);
    // testing variables
    static constexpr std::string_view Alpha1("FirstString");
    static constexpr std::string_view Alpha2("SecondString");
    static constexpr std::string_view AlphaField2("cSecondString");
    bool ErrFnd = false;

    auto &poolReference = state->dataSwimmingPools->Pool(1);

    // Test 1: SurfacePtr is zero--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 0;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 2: The surface is already pointing to a radiant system, ventilated slab, or other pool--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = true;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 3: The heat transfer method has to be CTFs to simulate the pool, otherwise it should produce and error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CondFD;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 4: The pool surface is defined as a window--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(poolReference.SurfacePtr).Class = DataSurfaces::SurfaceClass::Window;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 5: The pool is defined with movable insulation--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(poolReference.SurfacePtr).Class = DataSurfaces::SurfaceClass::Floor;
    state->dataSurface->SurfMaterialMovInsulInt(poolReference.SurfacePtr) = 1;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 6: The pool is defined with a source/sink (usually associated with radiant systems)--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(poolReference.SurfacePtr).Class = DataSurfaces::SurfaceClass::Floor;
    state->dataSurface->SurfMaterialMovInsulInt(poolReference.SurfacePtr) = 1;
    state->dataSurface->Surface(poolReference.SurfacePtr).Construction = 1;
    state->dataConstruction->Construct(state->dataSurface->Surface(poolReference.SurfacePtr).Construction).SourceSinkPresent = true;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 7: The pool is not defined as a floor--this is not allowed and should produce an error
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(poolReference.SurfacePtr).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->SurfMaterialMovInsulInt(poolReference.SurfacePtr) = 1;
    state->dataConstruction->Construct(state->dataSurface->Surface(poolReference.SurfacePtr).Construction).SourceSinkPresent = false;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_TRUE(ErrFnd);

    // Test 8: Everything about the surface that is being used by the "legal"--no error is produced
    ErrFnd = false;
    poolReference.SurfacePtr = 1;
    state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr) = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(poolReference.SurfacePtr).Class = DataSurfaces::SurfaceClass::Floor;
    state->dataSurface->SurfMaterialMovInsulInt(poolReference.SurfacePtr) = 0;
    state->dataConstruction->Construct(state->dataSurface->Surface(poolReference.SurfacePtr).Construction).SourceSinkPresent = false;
    state->dataSurface->Surface(poolReference.SurfacePtr).Zone = 7;
    state->dataSurface->SurfIsPool(poolReference.SurfacePtr) = false;
    poolReference.ZonePtr = 0;

    poolReference.ErrorCheckSetupPoolSurface(*state, Alpha1, Alpha2, AlphaField2, ErrFnd);

    EXPECT_FALSE(ErrFnd);
    EXPECT_TRUE(state->dataSurface->SurfIsRadSurfOrVentSlabOrPool(poolReference.SurfacePtr));
    EXPECT_TRUE(state->dataSurface->SurfIsPool(poolReference.SurfacePtr));
    EXPECT_EQ(state->dataSurface->Surface(poolReference.SurfacePtr).Zone, poolReference.ZonePtr);
}

TEST_F(EnergyPlusFixture, SwimmingPool_MultiplePoolUpdatePoolSourceValAvgTest)
{
    // Test of UpdatePoolSourceValAvg for Multiple Swimming Pools (Fix for Defect #9797)
    // The specification of multiple pools in single input file led to weird results and
    // was traced back to improper assignment statements in UpdatePoolSourceValAvg.

    auto &PoolData = state->dataSwimmingPools;
    auto &SurfData = state->dataSurface;
    auto &HBFanData = state->dataHeatBalFanSys;

    Real64 closeEnough = 0.00001;
    PoolData->NumSwimmingPools = 2;
    PoolData->Pool.allocate(PoolData->NumSwimmingPools);
    auto &Pool1Data = state->dataSwimmingPools->Pool(1);
    auto &Pool2Data = state->dataSwimmingPools->Pool(2);

    SurfData->TotSurfaces = 2;
    SurfData->Surface.allocate(SurfData->TotSurfaces);

    Real64 noResult = -9999.0;
    HBFanData->QPoolSurfNumerator.allocate(SurfData->TotSurfaces);
    HBFanData->QPoolSurfNumerator = noResult;
    HBFanData->PoolHeatTransCoefs.allocate(SurfData->TotSurfaces);
    HBFanData->PoolHeatTransCoefs = noResult;

    SurfData->Surface(1).ExtBoundCond = 0; // All connected to exterior
    SurfData->Surface(2).ExtBoundCond = 0; // All connected to exterior
    Pool1Data.SurfacePtr = 1;
    Pool2Data.SurfacePtr = 2;

    // Test 1: both pools off
    Pool1Data.QPoolSrcAvg = 0.0;
    Pool1Data.HeatTransCoefsAvg = 0.0;
    Pool2Data.QPoolSrcAvg = 0.0;
    Pool2Data.HeatTransCoefsAvg = 0.0;

    // Call subroutine
    bool poolOnFlag = false;
    UpdatePoolSourceValAvg(*state, poolOnFlag);

    // Test data transfer
    EXPECT_FALSE(poolOnFlag);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(1), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(2), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(1), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(2), 0.0, closeEnough);

    // Test 2a: pool 1 on, pool 2 off
    Pool1Data.QPoolSrcAvg = 100.0;
    Pool1Data.HeatTransCoefsAvg = 10.0;
    Pool2Data.QPoolSrcAvg = 0.0;
    Pool2Data.HeatTransCoefsAvg = 0.0;
    HBFanData->QPoolSurfNumerator = noResult;
    HBFanData->PoolHeatTransCoefs = noResult;

    // Call subroutine
    poolOnFlag = false;
    UpdatePoolSourceValAvg(*state, poolOnFlag);

    // Test data transfer
    EXPECT_TRUE(poolOnFlag);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(1), Pool1Data.QPoolSrcAvg, closeEnough);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(2), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(1), Pool1Data.HeatTransCoefsAvg, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(2), 0.0, closeEnough);

    // Test 2b: pool 1 off, pool 2 on
    Pool1Data.QPoolSrcAvg = 0.0;
    Pool1Data.HeatTransCoefsAvg = 0.0;
    Pool2Data.QPoolSrcAvg = 200.0;
    Pool2Data.HeatTransCoefsAvg = 20.0;
    HBFanData->QPoolSurfNumerator = noResult;
    HBFanData->PoolHeatTransCoefs = noResult;

    // Call subroutine
    poolOnFlag = false;
    UpdatePoolSourceValAvg(*state, poolOnFlag);

    // Test data transfer
    EXPECT_TRUE(poolOnFlag);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(1), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(2), Pool2Data.QPoolSrcAvg, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(1), 0.0, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(2), Pool2Data.HeatTransCoefsAvg, closeEnough);

    // Test 3: both pools on
    Pool1Data.QPoolSrcAvg = 100.0;
    Pool1Data.HeatTransCoefsAvg = 10.0;
    Pool2Data.QPoolSrcAvg = 200.0;
    Pool2Data.HeatTransCoefsAvg = 20.0;
    HBFanData->QPoolSurfNumerator = noResult;
    HBFanData->PoolHeatTransCoefs = noResult;

    // Call subroutine
    poolOnFlag = false;
    UpdatePoolSourceValAvg(*state, poolOnFlag);

    // Test data transfer
    EXPECT_TRUE(poolOnFlag);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(1), Pool1Data.QPoolSrcAvg, closeEnough);
    EXPECT_NEAR(HBFanData->QPoolSurfNumerator(2), Pool2Data.QPoolSrcAvg, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(1), Pool1Data.HeatTransCoefsAvg, closeEnough);
    EXPECT_NEAR(HBFanData->PoolHeatTransCoefs(2), Pool2Data.HeatTransCoefsAvg, closeEnough);
}

TEST_F(EnergyPlusFixture, SwimmingPool_anySwimmingPoolsInputFlagTest)
{
    // Test 1: no swimming pools--anySwimmingPoolsInput stays false
    std::string const idf_objects1 = delimited_string({
        "  SurfaceConvectionAlgorithm:Inside,",
        "    TARP;",
    });
    ASSERT_TRUE(process_idf(idf_objects1));
    GetSwimmingPool(*state);
    ASSERT_FALSE(state->dataSwimmingPools->anySwimmingPoolsInput);

    // Test 2: there's a swimming pool--anySwimmingPoolsInput gets set to true
    state->dataSwimmingPools->clear_state();
    std::string const idf_objects2 = delimited_string({
        "SwimmingPool:Indoor,",
        "  Test Pool,               !- Name",
        "  F1-1,                    !- Surface Name",
        "  1.5,                     !- Average Depth {m}",
        "  Sched,                   !- Activity Factor Schedule Name",
        "  Sched,                   !- Make-up Water Supply Schedule Name",
        "  Sched,                   !- Cover Schedule Name",
        "  0.8,                     !- Cover Evaporation Factor",
        "  0.2,                     !- Cover Convection Factor",
        "  0.9,                     !- Cover Short-Wavelength Radiation Factor",
        "  0.5,                     !- Cover Long-Wavelength Radiation Factor",
        "  Pool Water Inlet Node,   !- Pool Water Inlet Node",
        "  Pool Water Outlet Node,  !- Pool Water Outlet Node",
        "  0.1,                     !- Pool Heating System Maximum Water Flow Rate {m3/s}",
        "  0.6,                     !- Pool Miscellaneous Equipment Power {W/(m3/s)}",
        "  Sched,                   !- Setpoint Temperature Schedule",
        "  15,                      !- Maximum Number of People",
        "  Sched,                   !- People Schedule",
        "  Sched;                   !- People Heat Gain Schedule",

        "Schedule:Compact,",
        "  Sched,                   !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,16.67;      !- Field 3",

        "BuildingSurface:Detailed,",
        "  F1-1,                    !- Name",
        "  FLOOR,                   !- Surface Type",
        "  FLOOR-SLAB-1,            !- Construction Name",
        "  SPACE1-1,                !- Zone Name",
        "  ,                        !- Space Name",
        "  Ground,                  !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  0.0,                     !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  26.8,3.7,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "  30.5,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "  3.7,3.7,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "Zone,",
        "  SPACE1-1,                !- Name",
        "  0,                       !- Direction of Relative North {deg}",
        "  0,                       !- X Origin {m}",
        "  0,                       !- Y Origin {m}",
        "  0,                       !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.438400269,             !- Ceiling Height {m}",
        "  239.247360229;           !- Volume {m3}",

        "Construction,",
        "  FLOOR-SLAB-1,            !- Name",
        "  CC03;                    !- Outside Layer",

        "Material,",
        "  CC03,                    !- Name",
        "  MediumRough,             !- Roughness",
        "  0.1016000,               !- Thickness {m}",
        "  1.310000,                !- Conductivity {W/m-K}",
        "  2243.000,                !- Density {kg/m3}",
        "  837.0000,                !- Specific Heat {J/kg-K}",
        "  0.9000000,               !- Thermal Absorptance",
        "  0.6500000,               !- Solar Absorptance",
        "  0.6500000;               !- Visible Absorptance",
    });
    ASSERT_TRUE(process_idf(idf_objects2));
    bool errsFound = false;
    Material::GetMaterialData(*state, errsFound); // read material data
    EXPECT_FALSE(errsFound);
    HeatBalanceManager::GetConstructData(*state, errsFound); // read construction data
    EXPECT_FALSE(errsFound);
    HeatBalanceManager::GetZoneData(*state, errsFound);
    EXPECT_FALSE(errsFound);
    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * Constant::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * Constant::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    SurfaceGeometry::GetSurfaceData(*state, errsFound);
    EXPECT_FALSE(errsFound);
    SwimmingPool::GetSwimmingPool(*state);
    ASSERT_TRUE(state->dataSwimmingPools->anySwimmingPoolsInput);
}
