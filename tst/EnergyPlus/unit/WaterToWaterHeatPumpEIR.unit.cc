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

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/WaterToWaterHeatPumpEIR.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EIRWaterToWaterHeatPumps;

TEST_F(EnergyPlusFixture, TestEIRWWHPHeatingConstructionFullObjectsHeatingAndCooling) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  hp cooling side,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  hp heating side,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[0];
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[1];

    // validate the heating side
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisHeatingWWHP->plantTypeOfNum);
    EXPECT_EQ(thisCoolingWWHP, thisHeatingWWHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisHeatingWWHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingWWHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingWWHP->powerRatioFuncPLRCurveIndex);

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingWWHP->plantTypeOfNum);
    EXPECT_EQ(thisHeatingWWHP, thisCoolingWWHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingWWHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingWWHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingWWHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "fake"),
            std::runtime_error
    );
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"),
            std::runtime_error
    );
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "fake"),
            std::runtime_error
    );
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"),
            std::runtime_error
    );
}

TEST_F(EnergyPlusFixture, TestEIRWWHPHeatingConstructionFullObjectsHeatingNoCompanion) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  ,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[0];

    // validate the heating side
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisHeatingWWHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisHeatingWWHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisHeatingWWHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingWWHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingWWHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "fake"),
            std::runtime_error
    );
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"),
            std::runtime_error
    );
}

TEST_F(EnergyPlusFixture, TestEIRWWHPHeatingConstructionFullObjectsCoolingNoCompanion) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  ,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingWWHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisCoolingWWHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingWWHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingWWHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingWWHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "fake"),
            std::runtime_error
    );
    EXPECT_THROW(
            EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"),
            std::runtime_error
    );
}

TEST_F(EnergyPlusFixture, TestEIRWWHPInitialization) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  ,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(2);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto & wwhpPlantLoadSideComp = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    wwhpPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    // then the source side
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto & wwhpPlantLoadSourceComp = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    wwhpPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

    // do a bit of extra wiring up to the plant
    wwhpPlantLoadSideComp.Name = thisCoolingWWHP->name;
    wwhpPlantLoadSideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    wwhpPlantLoadSourceComp.Name = thisCoolingWWHP->name;
    wwhpPlantLoadSourceComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;

    // call for initialization, oneTimeInit only first
    DataGlobals::BeginEnvrnFlag = false;
    thisCoolingWWHP->onInitLoopEquip(myLocation);

    // validate that location work got done correctly
    EXPECT_EQ(1, thisCoolingWWHP->loadSideLocation.loopNum);
    EXPECT_EQ(2, thisCoolingWWHP->loadSideLocation.loopSideNum);
    EXPECT_EQ(1, thisCoolingWWHP->loadSideLocation.branchNum);
    EXPECT_EQ(1, thisCoolingWWHP->loadSideLocation.compNum);
    EXPECT_EQ(2, thisCoolingWWHP->sourceSideLocation.loopNum);
    EXPECT_EQ(1, thisCoolingWWHP->sourceSideLocation.loopSideNum);
    EXPECT_EQ(1, thisCoolingWWHP->sourceSideLocation.branchNum);
    EXPECT_EQ(1, thisCoolingWWHP->sourceSideLocation.compNum);

    // now call for initialization again, for begin environment
    DataGlobals::BeginEnvrnFlag = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;
    thisCoolingWWHP->onInitLoopEquip(myLocation);

    // validate that plant sizing went ok
    Real64 const flowTol = 0.001;
    Real64 const rho = 999.89; // easy to edit here if the expected density gets adjusted in E+
    Real64 const expectedLoadSideMassFlow = rho * thisCoolingWWHP->loadSideDesignVolFlowRate;
    Real64 const expectedSourceSideMassFlow = rho * thisCoolingWWHP->sourceSideDesignVolFlowRate;
    EXPECT_NEAR(
            expectedLoadSideMassFlow,
            thisCoolingWWHP->loadSideDesignMassFlowRate,
            flowTol
    );
    EXPECT_NEAR(
            expectedSourceSideMassFlow,
            thisCoolingWWHP->sourceSideDesignMassFlowRate,
            flowTol
    );
    EXPECT_NEAR(
            0.0,
            DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRateMin,
            flowTol
    );
    EXPECT_NEAR(
            0.0,
            DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRateMinAvail,
            flowTol
    );
    EXPECT_NEAR(
            expectedLoadSideMassFlow,
            DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRateMax,
            flowTol
    );
    EXPECT_NEAR(
            expectedLoadSideMassFlow,
            DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRateMaxAvail,
            flowTol
    );
    EXPECT_NEAR(
            0.0,
            DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMin,
            flowTol
    );
    EXPECT_NEAR(
            0.0,
            DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMinAvail,
            flowTol
    );
    EXPECT_NEAR(
            expectedSourceSideMassFlow,
            DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMax,
            flowTol
    );
    EXPECT_NEAR(
            expectedSourceSideMassFlow,
            DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMaxAvail,
            flowTol
    );

}

TEST_F(EnergyPlusFixture, TestEIRWWHPCoolingOutletSetpointWorker) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  ,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(1);
    auto & wwhpPlantLoadSideLoop = DataPlant::PlantLoop(1);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &wwhpPlantLoadSideComp = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    wwhpPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

    // do a little setup here
    thisCoolingWWHP->loadSideLocation.loopNum = 1;
    thisCoolingWWHP->loadSideLocation.loopSideNum = 2;
    thisCoolingWWHP->loadSideLocation.branchNum = 1;
    thisCoolingWWHP->loadSideLocation.compNum = 1;
    thisCoolingWWHP->loadSideNodes.outlet = 1;

    // the factory would've called GetOnlySingleNode for the in/out pairs on the WWHP, add another one for the loop
    // outlet setpoint node
    DataLoopNode::Node.allocate(5);
    wwhpPlantLoadSideLoop.TempSetPointNodeNum = 5;

    // set up the plant setpoint conditions and test for single setpoint operation
    wwhpPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::SingleSetPoint;
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.outlet).TempSetPoint = 3.141;
    DataLoopNode::Node(5).TempSetPoint = 2.718;
    EXPECT_NEAR(
            3.141,
            thisCoolingWWHP->getLoadSideOutletSetpointTemp(),
            0.001
    );
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(
            2.718,
            thisCoolingWWHP->getLoadSideOutletSetpointTemp(),
            0.001
    );

    // test for dual setpoint operation
    wwhpPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::DualSetPointDeadBand;
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.outlet).TempSetPointHi = 6.282;
    DataLoopNode::Node(5).TempSetPointHi = 5.436;
    EXPECT_NEAR(
            6.282,
            thisCoolingWWHP->getLoadSideOutletSetpointTemp(),
            0.001
    );
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(
            5.436,
            thisCoolingWWHP->getLoadSideOutletSetpointTemp(),
            0.001
    );
}

TEST_F(EnergyPlusFixture, TestEIRWWHPCoolingSetRunStateAndFlowWorker) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  ,",
                            "  0.001,",
                            "  0.001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  1,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(2);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &wwhpPlantLoadSideComp = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    wwhpPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    // then the source side
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &wwhpPlantLoadSourceComp = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    wwhpPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

    // do a bit of extra wiring up to the plant
    wwhpPlantLoadSideComp.Name = thisCoolingWWHP->name;
    wwhpPlantLoadSideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    wwhpPlantLoadSourceComp.Name = thisCoolingWWHP->name;
    wwhpPlantLoadSourceComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;

    // call for all initialization
    DataGlobals::BeginEnvrnFlag = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;
    thisCoolingWWHP->onInitLoopEquip(myLocation);

    // now call the run state worker with no load, to try to turn it off
    thisCoolingWWHP->setRunStateAndFlowRates(false);
    EXPECT_FALSE(thisCoolingWWHP->running);
}