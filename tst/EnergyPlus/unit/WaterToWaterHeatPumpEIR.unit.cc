// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#pragma clang diagnostic push
#pragma ide diagnostic ignored "OCDFAInspection"
#pragma ide diagnostic ignored "cert-err58-cpp"
#pragma ide diagnostic ignored "modernize-use-equals-delete"

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/WaterToWaterHeatPumpEIR.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EIRWaterToWaterHeatPumps;

class EIRWWHPFixture : public EnergyPlusFixture {};

TEST_F(EIRWWHPFixture, ConstructionFullObjectsHeatingAndCooling) {
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
                            "  2,",
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
                            "  2,",
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
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[1];
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

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

TEST_F(EIRWWHPFixture, PairingCompanionCoils) {
    eir_wwhp.resize(2);
    EIRWaterToWaterHeatPump *coil1 = &eir_wwhp[0];
    EIRWaterToWaterHeatPump *coil2 = &eir_wwhp[1];

    {
        // a successful try
        coil1->name = "name1";
        coil1->companionCoilName = "name2";
        coil1->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRCooling;
        coil1->companionHeatPumpCoil = nullptr;
        coil2->name = "name2";
        coil2->companionCoilName = "name1";
        coil2->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRHeating;
        coil2->companionHeatPumpCoil = nullptr;
        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::pairUpCompanionCoils();
        EXPECT_EQ(coil2, coil1->companionHeatPumpCoil);
        EXPECT_EQ(coil1, coil2->companionHeatPumpCoil);
    }

    {
        // but what if we can't find a companion!
        coil1->name = "name1";
        coil1->companionCoilName = "name6";
        coil1->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRCooling;
        coil1->companionHeatPumpCoil = nullptr;
        coil2->name = "name2";
        coil2->companionCoilName = "name1";
        coil2->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRHeating;
        coil2->companionHeatPumpCoil = nullptr;
        EXPECT_THROW(EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::pairUpCompanionCoils(), std::runtime_error);
    }

    {
        // or what if we find a companion but it's the same coil type
        coil1->name = "name1";
        coil1->companionCoilName = "name2";
        coil1->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRCooling;
        coil1->companionHeatPumpCoil = nullptr;
        coil2->name = "name2";
        coil2->companionCoilName = "name1";
        coil2->plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRCooling;
        coil2->companionHeatPumpCoil = nullptr;
        EXPECT_THROW(EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::pairUpCompanionCoils(), std::runtime_error);
    }

}

TEST_F(EIRWWHPFixture, HeatingConstructionFullObjectsNoCompanion) {
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
                            "  1,",
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

TEST_F(EIRWWHPFixture, CoolingConstructionFullObjectsNoCompanion) {
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
                            "  1,",
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

TEST_F(EIRWWHPFixture, CoolingConstructionFullObjectWithDefaults) {
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
                            "  ,",
                            "  25.56,",
                            "  40.0,",
                            "  ,",
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
    EXPECT_NEAR(1, thisCoolingWWHP->sizingFactor, 0.001);

}

TEST_F(EIRWWHPFixture, CoolingConstructionFullyAutoSized) {
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
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  ,",
                            "  25.56,",
                            "  40.0,",
                            "  1,",
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

TEST_F(EIRWWHPFixture, Initialization) {
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
                            "  ,",
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

TEST_F(EIRWWHPFixture, TestSizing_FullyAutosizedCoolingWithCompanion) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  hp heating side,",
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 5,",
                            "  node 6,",
                            "  node 7,",
                            "  node 8,",
                            "  hp cooling side,",
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
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
    EXPECT_EQ(2u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingWWHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);

    // We'll set up two plant loops: a load and a source loop
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    Real64 const plantSizingLoadVolFlow = 0.01;
    Real64 const plantSizingLoadDeltaT = 1.0;

    DataSizing::PlantSizData.allocate(2);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.010;
    DataSizing::PlantSizData(1).DeltaT = 1.0;
    DataSizing::PlantSizData(2).DesVolFlowRate = 0.030;
    DataSizing::PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingWWHP->name;
    loop2demandComponent1.Name = thisHeatingWWHP->name;
    loop1supplyComponent2.Name = thisCoolingWWHP->name;
    loop2demandComponent2.Name = thisCoolingWWHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingWWHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingWWHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myCoolingSourceLocation = PlantLocation(2, 1, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingWWHP->onInitLoopEquip(myCoolingLoadLocation);
    thisHeatingWWHP->onInitLoopEquip(myHeatingLoadLocation);

    DataPlant::PlantFinalSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // assign the plant sizing data
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(2).PlantSizNum = 2;

    // The load side should be what is imposed by the plant sizing data for the design flow rate.
    // The source side should be calculated based on the COP, which was set at 1.0 for convenience.
    //   This works out to a multiplier of 2 on the source flow rate.  With the same deltaT on the design of the source
    //   loop, the flow rate must be twice as high.
    // The source side has a slightly different set of thermal properties so the expected flow is scaled by those.
    Real64 const expectedLoadCp = 4197.93;
    Real64 const expectedLoadRho = 999.898;
    Real64 const expectedSourceCp = 4185.0;
    Real64 const expectedSourceRho = 983.2;
    Real64 const expectedLoadFlow = plantSizingLoadVolFlow;
    Real64 expectedCapacity = expectedLoadRho * expectedLoadFlow * expectedLoadCp * plantSizingLoadDeltaT;
    Real64 const baseExpectedSourceFlow = plantSizingLoadVolFlow * 2.0;
    Real64 expectedSourceFlow = baseExpectedSourceFlow * (expectedLoadRho * expectedLoadCp) / (expectedSourceRho * expectedSourceCp);
    thisCoolingWWHP->size();
    EXPECT_NEAR(expectedLoadFlow, thisCoolingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisCoolingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedCapacity, thisCoolingWWHP->referenceCapacity, 0.0001);

    // with a sizing run complete, we can also go ahead and get the design capacities...
    // they should be nonzero for the load side of things
    Real64 tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingWWHP->getDesignCapacities(myCoolingLoadLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpMax, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpOpt, 0.001);
    // but always zero for the source side of things
    tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingWWHP->getDesignCapacities(myCoolingSourceLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(0.0, tmpMax, 0.001);
    EXPECT_NEAR(0.0, tmpOpt, 0.001);

    // we can reset things and do a few more corner cases here

    // lets just try it again but with the plant sizing data set to zero flow, it should try to use the companion
    // but the companion isn't sized yet, so it should get zero conditions
    thisCoolingWWHP->loadSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingWWHP->sourceSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingWWHP->referenceCapacity = DataSizing::AutoSize;
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.0;
    thisCoolingWWHP->size();
    EXPECT_NEAR(0.0, thisCoolingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingWWHP->referenceCapacity, 0.0001);

    // but now let's try to size the heating coil, which will try to use the cooling coil's sized data
    thisCoolingWWHP->loadSideDesignVolFlowRate = expectedLoadFlow;
    thisCoolingWWHP->sourceSideDesignVolFlowRate = expectedSourceFlow;
    thisCoolingWWHP->referenceCapacity = expectedCapacity;
    expectedSourceFlow = baseExpectedSourceFlow * (expectedSourceRho * expectedSourceCp) / (expectedLoadRho * expectedLoadCp);
    expectedCapacity = expectedSourceRho * expectedLoadFlow * expectedSourceCp * plantSizingLoadDeltaT;
    thisHeatingWWHP->size();
    EXPECT_NEAR(expectedLoadFlow, thisHeatingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisHeatingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedCapacity, thisHeatingWWHP->referenceCapacity, 0.0001);
}

TEST_F(EIRWWHPFixture, TestSizing_FullyHardsizedHeatingWithCompanion) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  hp heating side,",
                            "  0.01,",
                            "  0.02,",
                            "  1200,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 5,",
                            "  node 6,",
                            "  node 7,",
                            "  node 8,",
                            "  hp cooling side,",
                            "  0.01,",
                            "  0.02,",
                            "  1200,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
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
    EXPECT_EQ(2u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingWWHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);

    // We'll set up two plant loops: a load and a source loop
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).PlantSizNum = 2;

    DataSizing::PlantSizData.allocate(2);
    DataSizing::PlantSizData(1).DesVolFlowRate = 0.020;
    DataSizing::PlantSizData(1).DeltaT = 1.0;
    DataSizing::PlantSizData(2).DesVolFlowRate = 0.030;
    DataSizing::PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingWWHP->name;
    loop2demandComponent1.Name = thisHeatingWWHP->name;
    loop1supplyComponent2.Name = thisCoolingWWHP->name;
    loop2demandComponent2.Name = thisCoolingWWHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingWWHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingWWHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;
    DataGlobals::DisplayExtraWarnings = true;

    // initialize so the components can find themselves on the plant
    thisHeatingWWHP->onInitLoopEquip(myLoadLocation);

    DataPlant::PlantFinalSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // The values really should just come out all as the hard-sized values, this just makes sure that function didn't
    // botch something up.
    thisHeatingWWHP->size();
    EXPECT_NEAR(0.01, thisHeatingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.02, thisHeatingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1200, thisHeatingWWHP->referenceCapacity, 0.0001);

    // Call it again, but this time with PlantSizing on, it should come out the same again
    DataGlobals::DoPlantSizing = true;
    thisHeatingWWHP->size();
    EXPECT_NEAR(0.01, thisHeatingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.02, thisHeatingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1200, thisHeatingWWHP->referenceCapacity, 0.0001);

}

TEST_F(EIRWWHPFixture, TestSizing_WithCompanionNoPlantSizing) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Cooling,",
                            "  hp cooling side,",
                            "  node 1,",
                            "  node 2,",
                            "  node 3,",
                            "  node 4,",
                            "  hp heating side,",
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 5,",
                            "  node 6,",
                            "  node 7,",
                            "  node 8,",
                            "  hp cooling side,",
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  1.0,",
                            "  ,",
                            "  ,",
                            "  1,",
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
    EXPECT_EQ(2u, eir_wwhp.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];
    EIRWaterToWaterHeatPump *thisHeatingWWHP = &eir_wwhp[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingWWHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);

    // We'll set up two plant loops: a load and a source loop
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingWWHP->name;
    loop2demandComponent1.Name = thisHeatingWWHP->name;
    loop1supplyComponent2.Name = thisCoolingWWHP->name;
    loop2demandComponent2.Name = thisCoolingWWHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingWWHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingWWHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingWWHP->onInitLoopEquip(myCoolingLoadLocation);
    thisHeatingWWHP->onInitLoopEquip(myHeatingLoadLocation);

    DataPlant::PlantFinalSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // let's just fake that the companion coil already got autosized properly
    thisHeatingWWHP->loadSideDesignVolFlowRate = 0.1;
    thisHeatingWWHP->sourceSideDesignVolFlowRate = 0.2;
    thisHeatingWWHP->referenceCapacity = 1000.0;

    // With no plant sizing, it will try to use the autosized companion instead
    // the load flow should be the companion load flow
    // with no source plant sizing, the source flow will actually work out to be the same as the load flow (not the source flow)
    // the capacity will be the companion capacity
    thisCoolingWWHP->size();
    EXPECT_NEAR(0.1, thisCoolingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.1, thisCoolingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1000.0, thisCoolingWWHP->referenceCapacity, 0.0001);
}

TEST_F(EIRWWHPFixture, TestSizing_NoCompanionNoPlantSizingError) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 5,",
                            "  node 6,",
                            "  node 7,",
                            "  node 8,",
                            "  ,",
                            "  Autosize,",
                            "  Autosize,",
                            "  Autosize,",
                            "  1.0,",
                            "  1,",
                            "  1,",
                            "  1,",
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

    // validate that we have the right ones
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);

    // We'll set up two plant loops: a load and a source loop
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    loop1supplyComponent1.Name = thisHeatingWWHP->name;
    loop2demandComponent1.Name = thisHeatingWWHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingWWHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingWWHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisHeatingWWHP->onInitLoopEquip(myHeatingLoadLocation);

    DataPlant::PlantFinalSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // with no plant sizing available and no companion coil to size from, it should throw a fatal
    EXPECT_THROW(thisHeatingWWHP->size(), std::runtime_error);

}

TEST_F(EIRWWHPFixture, TestSizing_NoCompanionNoPlantSizingHardSized) {
    std::string const idf_objects =
            delimited_string(
                    {
                            "HeatPump:WaterToWater:EIR:Heating,",
                            "  hp heating side,",
                            "  node 5,",
                            "  node 6,",
                            "  node 7,",
                            "  node 8,",
                            "  ,",
                            "  0.1,",
                            "  0.1,",
                            "  1000,",
                            "  1.0,",
                            "  1,",
                            "  1,",
                            "  1,",
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

    // validate that we have the right ones
    EXPECT_EQ("HP HEATING SIDE", thisHeatingWWHP->name);

    // We'll set up two plant loops: a load and a source loop
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    loop1supplyComponent1.Name = thisHeatingWWHP->name;
    loop2demandComponent1.Name = thisHeatingWWHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingWWHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingWWHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisHeatingWWHP->onInitLoopEquip(myHeatingLoadLocation);

    DataPlant::PlantFinalSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToReport = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // this should report out to the sizing output, but just the user defined stuff
    thisHeatingWWHP->size();
    EXPECT_NEAR(0.1, thisHeatingWWHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.1, thisHeatingWWHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1000, thisHeatingWWHP->referenceCapacity, 0.0001);
}

TEST_F(EIRWWHPFixture, CoolingOutletSetpointWorker) {
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
                            "  ,",
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
    auto &wwhpPlantLoadSideLoop = DataPlant::PlantLoop(1);
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
            thisCoolingWWHP->getLoadSideOutletSetPointTemp(),
            0.001
    );
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(
            2.718,
            thisCoolingWWHP->getLoadSideOutletSetPointTemp(),
            0.001
    );

    // test for dual setpoint operation
    wwhpPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::DualSetPointDeadBand;
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.outlet).TempSetPointHi = 6.282;
    DataLoopNode::Node(5).TempSetPointHi = 5.436;
    EXPECT_NEAR(
            6.282,
            thisCoolingWWHP->getLoadSideOutletSetPointTemp(),
            0.001
    );
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(
            5.436,
            thisCoolingWWHP->getLoadSideOutletSetPointTemp(),
            0.001
    );

}

TEST_F(EIRWWHPFixture, Initialization2) {
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
                            "  ,",
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

    // call with run flag off, loose limits on node min/max
    thisCoolingWWHP->running = false;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag off, nonzero minimums
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRateMinAvail = 0.1;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMinAvail = 0.2;
    thisCoolingWWHP->running = false;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.1,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.2,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag off, load side flow locked
    DataPlant::PlantLoop(1).LoopSide(2).FlowLock = true;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRate = 0.24;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRateMinAvail = 0.0;
    thisCoolingWWHP->running = false;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.24,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag ON, flow locked at zero on load side
    DataPlant::PlantLoop(1).LoopSide(2).FlowLock = true;
    DataPlant::PlantLoop(2).LoopSide(1).FlowLock = true;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRate = 0.2;
    thisCoolingWWHP->running = true;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.2,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag ON, flow locked at zero on source side
    DataPlant::PlantLoop(1).LoopSide(2).FlowLock = true;
    DataPlant::PlantLoop(2).LoopSide(1).FlowLock = true;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRate = 0.2;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingWWHP->running = true;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.2,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag ON, flow locked at zero on both sides
    DataPlant::PlantLoop(1).LoopSide(2).FlowLock = true;
    DataPlant::PlantLoop(2).LoopSide(1).FlowLock = true;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingWWHP->running = true;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.0,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

    // call with run flag ON, flow locked at nonzero both
    DataPlant::PlantLoop(1).LoopSide(2).FlowLock = true;
    DataPlant::PlantLoop(2).LoopSide(1).FlowLock = true;
    DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).MassFlowRate = 0.14;
    DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).MassFlowRate = 0.13;
    thisCoolingWWHP->running = true;
    thisCoolingWWHP->setOperatingFlowRates();
    EXPECT_NEAR(
            0.14,
            thisCoolingWWHP->loadSideMassFlowRate,
            0.001
    );
    EXPECT_NEAR(
            0.13,
            thisCoolingWWHP->sourceSideMassFlowRate,
            0.001
    );

}

TEST_F(EIRWWHPFixture, OnInitLoopEquipTopologyErrorCases) {
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
                            "  0.0001,",
                            "  0.0001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  ,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  0.95,",
                            "  0,",
                            "  1,",
                            "  1;"
                    }
            );
    ASSERT_TRUE(process_idf(idf_objects));

    // set up a couple simple plant loops with one branch per loop-side and one component per branch
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopDemandCalcScheme = DataPlant::SingleSetPoint;
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopDemandCalcScheme = DataPlant::SingleSetPoint;
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &wwhpPlantSupplySideComp = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &wwhpPlantDemandSideComp = DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1);
    auto &extraWwhpPlantSupplySideComp = DataPlant::PlantLoop(2).LoopSide(2).Branch(1).Comp(1);
    auto &extraWwhpPlantDemandSideComp = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    wwhpPlantSupplySideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    wwhpPlantDemandSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    extraWwhpPlantSupplySideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    extraWwhpPlantDemandSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");
    EXPECT_EQ(1u, eir_wwhp.size());
    EIRWaterToWaterHeatPump *thisCoolingWWHP = &eir_wwhp[0];

    // init the plant component data with the name we have now from the factory call
    wwhpPlantSupplySideComp.Name = thisCoolingWWHP->name;
    wwhpPlantDemandSideComp.Name = thisCoolingWWHP->name;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    DataGlobals::BeginEnvrnFlag = true;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // test the case where the heat pump is connected to both the supply and demand sides of the same loop
    wwhpPlantSupplySideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    wwhpPlantDemandSideComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;
    extraWwhpPlantSupplySideComp.NodeNumIn = -1;
    extraWwhpPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil load and supply sides were on the same loop
    EXPECT_THROW(thisCoolingWWHP->onInitLoopEquip(myLoadLocation), std::runtime_error);

    // test the case where the heat pump source side cannot be found
    wwhpPlantSupplySideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    wwhpPlantDemandSideComp.NodeNumIn = -1;
    extraWwhpPlantSupplySideComp.NodeNumIn = -1;
    extraWwhpPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil source side inlet node is not found on plant
    EXPECT_THROW(thisCoolingWWHP->onInitLoopEquip(myLoadLocation), std::runtime_error);

    // test the case where the heat pump load side cannot be found
    wwhpPlantSupplySideComp.NodeNumIn = -1;
    wwhpPlantDemandSideComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;
    extraWwhpPlantSupplySideComp.NodeNumIn = -1;
    extraWwhpPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil load side inlet node is not found on plant
    EXPECT_THROW(thisCoolingWWHP->onInitLoopEquip(myLoadLocation), std::runtime_error);

    // test the case where the heat pump source side is found, but it's on the supply side of a loop
    // still need to drop the load side onto a (extra) plant supply to trigger this condition
    wwhpPlantSupplySideComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;
    wwhpPlantDemandSideComp.NodeNumIn = -1;
    extraWwhpPlantSupplySideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    extraWwhpPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil source was found on a supply side
    EXPECT_THROW(thisCoolingWWHP->onInitLoopEquip(myLoadLocation), std::runtime_error);

    // test the case where the heat pump load side is found, but it's on the demand side of a loop
    // still need to drop the source side onto a (extra) plant demand to trigger this condition
    wwhpPlantSupplySideComp.NodeNumIn = -1;
    wwhpPlantDemandSideComp.NodeNumIn = thisCoolingWWHP->loadSideNodes.inlet;
    extraWwhpPlantSupplySideComp.NodeNumIn = -1;
    extraWwhpPlantDemandSideComp.NodeNumIn = thisCoolingWWHP->sourceSideNodes.inlet;
    // call for all initialization, it should abort because the coil load was found on a demand side
    EXPECT_THROW(thisCoolingWWHP->onInitLoopEquip(myLoadLocation), std::runtime_error);

}

TEST_F(EIRWWHPFixture, CoolingSimulate) {
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
                            "  0.0001,",
                            "  0.0001,",
                            "  1000,",
                            "  3.14,",
                            "  25.56,",
                            "  40.0,",
                            "  ,",
                            "  dummyCurve,",
                            "  dummyCurve,",
                            "  dummyCurve;",
                            "Curve:Linear,",
                            "  dummyCurve,",
                            "  0.95,",
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
    DataPlant::PlantLoop(1).LoopDemandCalcScheme = DataPlant::SingleSetPoint;
    DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &wwhpPlantLoadSideComp = DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    wwhpPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    wwhpPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    // then the source side
    DataPlant::PlantLoop(2).LoopSide.allocate(2);
    DataPlant::PlantLoop(2).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &wwhpPlantLoadSourceComp = DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    wwhpPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation mySourceLocation = PlantLocation(2, 1, 1, 1);

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
    thisCoolingWWHP->onInitLoopEquip(myLoadLocation);

    // call from load side location, firsthvac, no load, not running, verify the unit doesn't have any values lingering
    thisCoolingWWHP->loadSideHeatTransfer = 1000;
    thisCoolingWWHP->loadSideInletTemp = 23.0;
    thisCoolingWWHP->loadSideOutletTemp = 42.0;
    thisCoolingWWHP->powerUsage = 4.0;
    thisCoolingWWHP->sourceSideHeatTransfer = 60.0;
    thisCoolingWWHP->sourceSideInletTemp = 43.0;
    thisCoolingWWHP->sourceSideOutletTemp = 83.0;
    bool firstHVAC = true;
    Real64 curLoad = 0.0;
    bool runFlag = false;
    thisCoolingWWHP->simulate(myLoadLocation, firstHVAC, curLoad, runFlag);
    EXPECT_NEAR(0.0, thisCoolingWWHP->loadSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingWWHP->sourceSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingWWHP->powerUsage, 0.001);
    EXPECT_NEAR(thisCoolingWWHP->loadSideInletTemp, thisCoolingWWHP->loadSideOutletTemp, 0.001);
    EXPECT_NEAR(thisCoolingWWHP->sourceSideInletTemp, thisCoolingWWHP->sourceSideOutletTemp, 0.001);

    // call from source side location, firsthvac, no load, not running, connected loop should be triggered to resimulate
    DataPlant::PlantLoop(1).LoopSide(2).SimLoopSideNeeded = false;
    DataPlant::PlantLoop(2).LoopSide(1).SimLoopSideNeeded = false;
    thisCoolingWWHP->simulate(mySourceLocation, firstHVAC, curLoad, runFlag);
    EXPECT_TRUE(DataPlant::PlantLoop(2).LoopSide(1).SimLoopSideNeeded);

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit can meet load)
    {
        firstHVAC = true;
        curLoad = 800;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp =
                specifiedLoadSetpoint + curLoad / (expectedLoadMassFlowRate * expectedCp);
        DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingWWHP->simulate(myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(specifiedLoadSetpoint, thisCoolingWWHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(curLoad, thisCoolingWWHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit cannot meet load)
    {
        firstHVAC = true;
        curLoad = 1200;
        Real64 availableCapacity = 950.0;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp =
                specifiedLoadSetpoint + curLoad / (expectedLoadMassFlowRate * expectedCp);
        DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        DataLoopNode::Node(thisCoolingWWHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        DataLoopNode::Node(thisCoolingWWHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingWWHP->simulate(myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(15.597, thisCoolingWWHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(availableCapacity, thisCoolingWWHP->loadSideHeatTransfer, 0.001);
    }

}

#pragma clang diagnostic pop
#pragma clang diagnostic pop
