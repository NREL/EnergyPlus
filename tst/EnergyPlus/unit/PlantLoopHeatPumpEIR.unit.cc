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

#pragma clang diagnostic push
#pragma ide diagnostic ignored "OCDFAInspection"
#pragma ide diagnostic ignored "cert-err58-cpp"
#pragma ide diagnostic ignored "modernize-use-equals-delete"

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EIRPlantLoopHeatPumps;

TEST_F(EnergyPlusFixture, ConstructionFullObjectsHeatingAndCooling_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp cooling side,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  2,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  2,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the heating side
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisHeatingPLHP->plantTypeOfNum);
    EXPECT_EQ(thisCoolingPLHP, thisHeatingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisHeatingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncPLRCurveIndex);

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_EQ(thisHeatingPLHP, thisCoolingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, PairingCompanionCoils)
{
    state->dataEIRPlantLoopHeatPump->heatPumps.resize(2);
    EIRPlantLoopHeatPump *coil1 = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *coil2 = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

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
        EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::pairUpCompanionCoils(*state);
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
        EXPECT_THROW(EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::pairUpCompanionCoils(*state), std::runtime_error);
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
        EXPECT_THROW(EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::pairUpCompanionCoils(*state), std::runtime_error);
    }
}

TEST_F(EnergyPlusFixture, HeatingConstructionFullObjectsNoCompanion)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the heating side
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisHeatingPLHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisHeatingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisHeatingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, CoolingConstructionFullObjectsNoCompanion)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisCoolingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, CoolingConstructionFullObjectWithDefaults)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  ,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_NEAR(1, thisCoolingPLHP->sizingFactor, 0.001);
}

TEST_F(EnergyPlusFixture, CoolingConstructionFullyAutoSized_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
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
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisCoolingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, CatchErrorsOnBadCurves)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  ,",
                                                      "  1,",
                                                      "  dummyCurveA,",
                                                      "  dummyCurveB,",
                                                      "  dummyCurveC;"});
    ASSERT_TRUE(process_idf(idf_objects));
    // call the factory with a valid name to trigger reading inputs, it should throw for the bad curves
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, Initialization)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // call for initialization, oneTimeInit only first
    state->dataGlobal->BeginEnvrnFlag = false;
    thisCoolingPLHP->onInitLoopEquip(*state, myLocation);

    // validate that location work got done correctly
    EXPECT_EQ(1, thisCoolingPLHP->loadSideLocation.loopNum);
    EXPECT_EQ(2, thisCoolingPLHP->loadSideLocation.loopSideNum);
    EXPECT_EQ(1, thisCoolingPLHP->loadSideLocation.branchNum);
    EXPECT_EQ(1, thisCoolingPLHP->loadSideLocation.compNum);
    EXPECT_EQ(2, thisCoolingPLHP->sourceSideLocation.loopNum);
    EXPECT_EQ(1, thisCoolingPLHP->sourceSideLocation.loopSideNum);
    EXPECT_EQ(1, thisCoolingPLHP->sourceSideLocation.branchNum);
    EXPECT_EQ(1, thisCoolingPLHP->sourceSideLocation.compNum);

    // now call for initialization again, for begin environment
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLocation);

    // validate that plant sizing went ok
    Real64 const flowTol = 0.001;
    Real64 const rho = 999.89; // easy to edit here if the expected density gets adjusted in E+
    Real64 const expectedLoadSideMassFlow = rho * thisCoolingPLHP->loadSideDesignVolFlowRate;
    Real64 const expectedSourceSideMassFlow = rho * thisCoolingPLHP->sourceSideDesignVolFlowRate;
    EXPECT_NEAR(expectedLoadSideMassFlow, thisCoolingPLHP->loadSideDesignMassFlowRate, flowTol);
    EXPECT_NEAR(expectedSourceSideMassFlow, thisCoolingPLHP->sourceSideDesignMassFlowRate, flowTol);
    EXPECT_NEAR(0.0, state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMin, flowTol);
    EXPECT_NEAR(0.0, state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMinAvail, flowTol);
    EXPECT_NEAR(expectedLoadSideMassFlow, state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMax, flowTol);
    EXPECT_NEAR(expectedLoadSideMassFlow, state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMaxAvail, flowTol);
    EXPECT_NEAR(0.0, state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMin, flowTol);
    EXPECT_NEAR(0.0, state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMinAvail, flowTol);
    EXPECT_NEAR(expectedSourceSideMassFlow, state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMax, flowTol);
    EXPECT_NEAR(expectedSourceSideMassFlow, state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMaxAvail, flowTol);
}

TEST_F(EnergyPlusFixture, TestSizing_FullyAutosizedCoolingWithCompanion_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    Real64 const plantSizingLoadVolFlow = 0.01;
    Real64 const plantSizingLoadDeltaT = 1.0;

    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.010;
    state->dataSize->PlantSizData(1).DeltaT = 1.0;
    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.030;
    state->dataSize->PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop2demandComponent1.Name = thisHeatingPLHP->name;
    loop1supplyComponent2.Name = thisCoolingPLHP->name;
    loop2demandComponent2.Name = thisCoolingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myCoolingSourceLocation = PlantLocation(2, 1, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingPLHP->onInitLoopEquip(*state, myCoolingLoadLocation);
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // assign the plant sizing data
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;

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
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(expectedLoadFlow, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedCapacity, thisCoolingPLHP->referenceCapacity, 0.0001);

    // with a sizing run complete, we can also go ahead and get the design capacities...
    // they should be nonzero for the load side of things
    Real64 tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingPLHP->getDesignCapacities(*state, myCoolingLoadLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpMax, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpOpt, 0.001);
    // but always zero for the source side of things
    tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingPLHP->getDesignCapacities(*state, myCoolingSourceLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(0.0, tmpMax, 0.001);
    EXPECT_NEAR(0.0, tmpOpt, 0.001);

    // we can reset things and do a few more corner cases here

    // lets just try it again but with the plant sizing data set to zero flow, it should try to use the companion
    // but the companion isn't sized yet, so it should get zero conditions
    thisCoolingPLHP->loadSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingPLHP->sourceSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingPLHP->referenceCapacity = DataSizing::AutoSize;
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.0;
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->referenceCapacity, 0.0001);

    // but now let's try to size the heating coil, which will try to use the cooling coil's sized data
    thisCoolingPLHP->loadSideDesignVolFlowRate = expectedLoadFlow;
    thisCoolingPLHP->sourceSideDesignVolFlowRate = expectedSourceFlow;
    thisCoolingPLHP->referenceCapacity = expectedCapacity;
    expectedSourceFlow = baseExpectedSourceFlow * (expectedSourceRho * expectedSourceCp) / (expectedLoadRho * expectedLoadCp);
    expectedCapacity = expectedSourceRho * expectedLoadFlow * expectedSourceCp * plantSizingLoadDeltaT;
    thisHeatingPLHP->sizeLoadSide(*state);
    thisHeatingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(expectedLoadFlow, thisHeatingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisHeatingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedCapacity, thisHeatingPLHP->referenceCapacity, 0.0001);
}

TEST_F(EnergyPlusFixture, TestSizing_FullyHardsizedHeatingWithCompanion)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  0.01,",
                                                      "  0.02,",
                                                      "  1200,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  0.01,",
                                                      "  0.02,",
                                                      "  1200,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;

    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.020;
    state->dataSize->PlantSizData(1).DeltaT = 1.0;
    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.030;
    state->dataSize->PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop2demandComponent1.Name = thisHeatingPLHP->name;
    loop1supplyComponent2.Name = thisCoolingPLHP->name;
    loop2demandComponent2.Name = thisCoolingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataGlobal->DisplayExtraWarnings = true;

    // initialize so the components can find themselves on the plant
    thisHeatingPLHP->onInitLoopEquip(*state, myLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // The values really should just come out all as the hard-sized values, this just makes sure that function didn't
    // botch something up.
    thisHeatingPLHP->sizeLoadSide(*state);
    thisHeatingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(0.01, thisHeatingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.02, thisHeatingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1200, thisHeatingPLHP->referenceCapacity, 0.0001);

    // Call it again, but this time with PlantSizing on, it should come out the same again
    state->dataGlobal->DoPlantSizing = true;
    thisHeatingPLHP->sizeLoadSide(*state);
    thisHeatingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(0.01, thisHeatingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.02, thisHeatingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1200, thisHeatingPLHP->referenceCapacity, 0.0001);
}

TEST_F(EnergyPlusFixture, TestSizing_WithCompanionNoPlantSizing)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(2);
    auto &loop2demandComponent2 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    loop2demandComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop2demandComponent1.Name = thisHeatingPLHP->name;
    loop1supplyComponent2.Name = thisCoolingPLHP->name;
    loop2demandComponent2.Name = thisCoolingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    loop2demandComponent2.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingPLHP->onInitLoopEquip(*state, myCoolingLoadLocation);
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // let's just fake that the companion coil already got autosized properly
    thisHeatingPLHP->loadSideDesignVolFlowRate = 0.1;
    thisHeatingPLHP->sourceSideDesignVolFlowRate = 0.2;
    thisHeatingPLHP->referenceCapacity = 1000.0;

    // With no plant sizing, it will try to use the autosized companion instead
    // the load flow should be the companion load flow
    // with no source plant sizing, the source flow will actually work out to be the same as the load flow (not the source flow)
    // the capacity will be the companion capacity
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(0.1, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.1, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1000.0, thisCoolingPLHP->referenceCapacity, 0.0001);
}

TEST_F(EnergyPlusFixture, TestSizing_NoCompanionNoPlantSizingError)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  ,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate that we have the right ones
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop2demandComponent1.Name = thisHeatingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // with no plant sizing available and no companion coil to size from, it should throw a fatal
    EXPECT_THROW(thisHeatingPLHP->sizeLoadSide(*state), std::runtime_error);
}

TEST_F(EnergyPlusFixture, TestSizing_NoCompanionNoPlantSizingHardSized)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  ,",
                                                      "  0.1,",
                                                      "  0.1,",
                                                      "  1000,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate that we have the right ones
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop2demandComponent1 = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop2demandComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop2demandComponent1.Name = thisHeatingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop2demandComponent1.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // this should report out to the sizing output, but just the user defined stuff
    thisHeatingPLHP->sizeLoadSide(*state);
    thisHeatingPLHP->sizeSrcSideWSHP(*state);
    EXPECT_NEAR(0.1, thisHeatingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.1, thisHeatingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(1000, thisHeatingPLHP->referenceCapacity, 0.0001);
}

TEST_F(EnergyPlusFixture, CoolingOutletSetpointWorker)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    auto &PLHPPlantLoadSideLoop = state->dataPlnt->PlantLoop(1);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a little setup here
    thisCoolingPLHP->loadSideLocation.loopNum = 1;
    thisCoolingPLHP->loadSideLocation.loopSideNum = 2;
    thisCoolingPLHP->loadSideLocation.branchNum = 1;
    thisCoolingPLHP->loadSideLocation.compNum = 1;
    thisCoolingPLHP->loadSideNodes.outlet = 1;

    // the factory would've called GetOnlySingleNode for the in/out pairs on the PLHP, add another one for the loop
    // outlet setpoint node
    state->dataLoopNodes->Node.allocate(5);
    PLHPPlantLoadSideLoop.TempSetPointNodeNum = 5;

    // set up the plant setpoint conditions and test for single setpoint operation
    PLHPPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = 3.141;
    state->dataLoopNodes->Node(5).TempSetPoint = 2.718;
    EXPECT_NEAR(3.141, thisCoolingPLHP->getLoadSideOutletSetPointTemp(*state), 0.001);
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(2.718, thisCoolingPLHP->getLoadSideOutletSetPointTemp(*state), 0.001);

    // test for dual setpoint operation
    PLHPPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPointHi = 6.282;
    state->dataLoopNodes->Node(5).TempSetPointHi = 5.436;
    EXPECT_NEAR(6.282, thisCoolingPLHP->getLoadSideOutletSetPointTemp(*state), 0.001);
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;
    EXPECT_NEAR(5.436, thisCoolingPLHP->getLoadSideOutletSetPointTemp(*state), 0.001);
}

TEST_F(EnergyPlusFixture, Initialization2_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLocation);

    // call with run flag off, loose limits on node min/max
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag off, nonzero minimums
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMinAvail = 0.1;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMinAvail = 0.2;
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.1, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.2, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag off, load side flow locked
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.24;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRateMinAvail = 0.0;
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.24, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at zero on load side
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataPlnt->PlantLoop(2).LoopSide(1).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRate = 0.2;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.2, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at zero on source side
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataPlnt->PlantLoop(2).LoopSide(1).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.2;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.2, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at zero on both sides
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataPlnt->PlantLoop(2).LoopSide(1).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at nonzero both
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataPlnt->PlantLoop(2).LoopSide(1).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.14;
    state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).MassFlowRate = 0.13;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesWSHP(*state);
    EXPECT_NEAR(0.14, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.13, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);
}

TEST_F(EnergyPlusFixture, OnInitLoopEquipTopologyErrorCases)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  0.0001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up a couple simple plant loops with one branch per loop-side and one component per branch
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantSupplySideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &PLHPPlantDemandSideComp = state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1);
    auto &extraPLHPPlantSupplySideComp = state->dataPlnt->PlantLoop(2).LoopSide(2).Branch(1).Comp(1);
    auto &extraPLHPPlantDemandSideComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantSupplySideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    PLHPPlantDemandSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    extraPLHPPlantSupplySideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    extraPLHPPlantDemandSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // init the plant component data with the name we have now from the factory call
    PLHPPlantSupplySideComp.Name = thisCoolingPLHP->name;
    PLHPPlantDemandSideComp.Name = thisCoolingPLHP->name;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // test the case where the heat pump is connected to both the supply and demand sides of the same loop
    PLHPPlantSupplySideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantDemandSideComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;
    extraPLHPPlantSupplySideComp.NodeNumIn = -1;
    extraPLHPPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil load and supply sides were on the same loop
    EXPECT_THROW(thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation), std::runtime_error);

    // test the case where the heat pump source side cannot be found
    PLHPPlantSupplySideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantDemandSideComp.NodeNumIn = -1;
    extraPLHPPlantSupplySideComp.NodeNumIn = -1;
    extraPLHPPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil source side inlet node is not found on plant
    EXPECT_THROW(thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation), std::runtime_error);

    // test the case where the heat pump load side cannot be found
    PLHPPlantSupplySideComp.NodeNumIn = -1;
    PLHPPlantDemandSideComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;
    extraPLHPPlantSupplySideComp.NodeNumIn = -1;
    extraPLHPPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil load side inlet node is not found on plant
    EXPECT_THROW(thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation), std::runtime_error);

    // test the case where the heat pump source side is found, but it's on the supply side of a loop
    // still need to drop the load side onto a (extra) plant supply to trigger this condition
    PLHPPlantSupplySideComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;
    PLHPPlantDemandSideComp.NodeNumIn = -1;
    extraPLHPPlantSupplySideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    extraPLHPPlantDemandSideComp.NodeNumIn = -1;
    // call for all initialization, it should abort because the coil source was found on a supply side
    EXPECT_THROW(thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation), std::runtime_error);

    // test the case where the heat pump load side is found, but it's on the demand side of a loop
    // still need to drop the source side onto a (extra) plant demand to trigger this condition
    PLHPPlantSupplySideComp.NodeNumIn = -1;
    PLHPPlantDemandSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    extraPLHPPlantSupplySideComp.NodeNumIn = -1;
    extraPLHPPlantDemandSideComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;
    // call for all initialization, it should abort because the coil load was found on a demand side
    EXPECT_THROW(thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation), std::runtime_error);
}

TEST_F(EnergyPlusFixture, CoolingSimulate_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  0.0001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation mySourceLocation = PlantLocation(2, 1, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation);

    // call from load side location, firsthvac, no load, not running, verify the unit doesn't have any values lingering
    thisCoolingPLHP->loadSideHeatTransfer = 1000;
    thisCoolingPLHP->loadSideInletTemp = 23.0;
    thisCoolingPLHP->loadSideOutletTemp = 42.0;
    thisCoolingPLHP->powerUsage = 4.0;
    thisCoolingPLHP->sourceSideHeatTransfer = 60.0;
    thisCoolingPLHP->sourceSideInletTemp = 43.0;
    thisCoolingPLHP->sourceSideOutletTemp = 83.0;
    bool firstHVAC = true;
    Real64 curLoad = 0.0;
    bool runFlag = false;
    thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->powerUsage, 0.001);
    EXPECT_NEAR(thisCoolingPLHP->loadSideInletTemp, thisCoolingPLHP->loadSideOutletTemp, 0.001);
    EXPECT_NEAR(thisCoolingPLHP->sourceSideInletTemp, thisCoolingPLHP->sourceSideOutletTemp, 0.001);

    // call from source side location, firsthvac, no load, not running, connected loop should be triggered to resimulate
    state->dataPlnt->PlantLoop(1).LoopSide(2).SimLoopSideNeeded = false;
    state->dataPlnt->PlantLoop(2).LoopSide(1).SimLoopSideNeeded = false;
    thisCoolingPLHP->simulate(*state, mySourceLocation, firstHVAC, curLoad, runFlag);
    EXPECT_TRUE(state->dataPlnt->PlantLoop(2).LoopSide(1).SimLoopSideNeeded);

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit can meet load)
    {
        firstHVAC = true;
        curLoad = -800;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(specifiedLoadSetpoint, thisCoolingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(-curLoad, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit cannot meet load)
    {
        firstHVAC = true;
        curLoad = -1200;
        Real64 availableCapacity = 950.0;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(15.597, thisCoolingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(availableCapacity, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    }
}

TEST_F(EnergyPlusFixture, HeatingSimulate_WaterSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  0.0001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisHeatingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisHeatingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisHeatingPLHP->onInitLoopEquip(*state, myLoadLocation);

    // call it from the load side, but this time there is a negative (cooling) load - shouldn't try to run
    {
        bool firstHVAC = true;
        Real64 curLoad = -900;
        bool runFlag = true; // plant actually shouldn't do this but the component can be smart enough to handle it
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const loadInletTemp = 46;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = loadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(loadInletTemp, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(0.0, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }

    // call it from the load side, but this time there is load (still firsthvac, unit can meet load)
    {
        bool firstHVAC = true;
        Real64 curLoad = 800;
        bool runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4180;
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(specifiedLoadSetpoint, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(curLoad, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit cannot meet load)
    {
        bool firstHVAC = true;
        Real64 curLoad = 1200;
        Real64 availableCapacity = 950.0;
        bool runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4180;
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(44.402, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(availableCapacity, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }
}

TEST_F(EnergyPlusFixture, TestConcurrentOperationChecking)
{
    state->dataEIRPlantLoopHeatPump->heatPumps.resize(4);
    EIRPlantLoopHeatPump *coil1 = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *coil2 = &state->dataEIRPlantLoopHeatPump->heatPumps[1];
    EIRPlantLoopHeatPump *coil3 = &state->dataEIRPlantLoopHeatPump->heatPumps[2];
    EIRPlantLoopHeatPump *coil4 = &state->dataEIRPlantLoopHeatPump->heatPumps[3];

    // pair up the last two
    coil3->companionHeatPumpCoil = coil4;
    coil4->companionHeatPumpCoil = coil3;

    // set all of them to running
    coil1->running = true;
    coil2->running = true;
    coil3->running = true;
    coil4->running = true;

    // check to warn about concurrent operation
    EIRPlantLoopHeatPump::checkConcurrentOperation(*state);

    // that will just add a recurring warning to the end, so to check whether
    //  a warning was actually made, I'll just check the warning index values
    ASSERT_EQ(0, coil1->recurringConcurrentOperationWarningIndex);
    ASSERT_EQ(0, coil2->recurringConcurrentOperationWarningIndex);
    ASSERT_EQ(1, coil3->recurringConcurrentOperationWarningIndex);
    ASSERT_EQ(1, coil4->recurringConcurrentOperationWarningIndex);
}

TEST_F(EnergyPlusFixture, ConstructionFullObjectsHeatingAndCooling_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp cooling side,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  2,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  0.001,",
                                                      "  0.001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  2,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the heating side
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisHeatingPLHP->plantTypeOfNum);
    EXPECT_EQ(thisCoolingPLHP, thisHeatingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisHeatingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisHeatingPLHP->powerRatioFuncPLRCurveIndex);

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_EQ(thisHeatingPLHP, thisCoolingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, CoolingSimulate_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  1,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation);

    // call from load side location, firsthvac, no load, not running, verify the unit doesn't have any values lingering
    thisCoolingPLHP->loadSideHeatTransfer = 1000;
    thisCoolingPLHP->loadSideInletTemp = 23.0;
    thisCoolingPLHP->loadSideOutletTemp = 42.0;
    thisCoolingPLHP->powerUsage = 4.0;
    thisCoolingPLHP->sourceSideHeatTransfer = 60.0;
    thisCoolingPLHP->sourceSideInletTemp = 43.0;
    thisCoolingPLHP->sourceSideOutletTemp = 83.0;
    bool firstHVAC = true;
    Real64 curLoad = 0.0;
    bool runFlag = false;
    thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideHeatTransfer, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->powerUsage, 0.001);
    EXPECT_NEAR(thisCoolingPLHP->loadSideInletTemp, thisCoolingPLHP->loadSideOutletTemp, 0.001);
    EXPECT_NEAR(thisCoolingPLHP->sourceSideInletTemp, thisCoolingPLHP->sourceSideOutletTemp, 0.001);

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit can meet load)
    {
        firstHVAC = true;
        curLoad = -800;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(specifiedLoadSetpoint, thisCoolingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(-curLoad, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit cannot meet load)
    {
        firstHVAC = true;
        curLoad = -1200;
        Real64 availableCapacity = 950.0;
        runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4183;
        Real64 const specifiedLoadSetpoint = 15;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisCoolingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisCoolingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(15.597, thisCoolingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(availableCapacity, thisCoolingPLHP->loadSideHeatTransfer, 0.001);
    }
}

TEST_F(EnergyPlusFixture, HeatingSimulate_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  1,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisHeatingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisHeatingPLHP->onInitLoopEquip(*state, myLoadLocation);

    // call it from the load side, but this time there is a negative (cooling) load - shouldn't try to run
    {
        bool firstHVAC = true;
        Real64 curLoad = -900;
        bool runFlag = true; // plant actually shouldn't do this but the component can be smart enough to handle it
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const loadInletTemp = 46;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = loadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(loadInletTemp, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(0.0, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }

    // call it from the load side, but this time there is load (still firsthvac, unit can meet load)
    {
        bool firstHVAC = true;
        Real64 curLoad = 800;
        bool runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4180;
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to meet setpoint and have some pre-evaluated conditions
        EXPECT_NEAR(specifiedLoadSetpoint, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(curLoad, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is load (still firsthvac, unit cannot meet load)
    {
        bool firstHVAC = true;
        Real64 curLoad = 1200;
        Real64 availableCapacity = 950.0;
        bool runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4180;
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(44.402, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(availableCapacity, thisHeatingPLHP->loadSideHeatTransfer, 0.001);
    }

    // now we can call it again from the load side, but this time there is no load (still firsthvac)
    {
        bool firstHVAC = true;
        Real64 curLoad = 0.0;
        bool runFlag = true;
        Real64 const expectedLoadMassFlowRate = 0.09999;
        Real64 const expectedCp = 4180;
        Real64 const specifiedLoadSetpoint = 45;
        Real64 const calculatedLoadInletTemp = specifiedLoadSetpoint - curLoad / (expectedLoadMassFlowRate * expectedCp);
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.outlet).TempSetPoint = specifiedLoadSetpoint;
        state->dataLoopNodes->Node(thisHeatingPLHP->loadSideNodes.inlet).Temp = calculatedLoadInletTemp;
        state->dataLoopNodes->Node(thisHeatingPLHP->sourceSideNodes.inlet).Temp = 30;
        thisHeatingPLHP->simulate(*state, myLoadLocation, firstHVAC, curLoad, runFlag);
        // expect it to miss setpoint and be at max capacity
        EXPECT_NEAR(45.0, thisHeatingPLHP->loadSideOutletTemp, 0.001);
        EXPECT_NEAR(30.0, thisHeatingPLHP->sourceSideOutletTemp, 0.001);
    }
}

TEST_F(EnergyPlusFixture, CoolingConstructionFullyAutoSized_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
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
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // validate the cooling side
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisCoolingPLHP->plantTypeOfNum);
    EXPECT_EQ(nullptr, thisCoolingPLHP->companionHeatPumpCoil);
    EXPECT_EQ(1, thisCoolingPLHP->capFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncTempCurveIndex);
    EXPECT_EQ(1, thisCoolingPLHP->powerRatioFuncPLRCurveIndex);

    // calling the factory with an invalid name or type will call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "fake"), std::runtime_error);
    EXPECT_THROW(EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, ClearState)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
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
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");
    EXPECT_EQ(state->dataEIRPlantLoopHeatPump->heatPumps.size(), 1u);

    // test that vector is cleared
    state->dataEIRPlantLoopHeatPump->clear_state();
    EXPECT_EQ(state->dataEIRPlantLoopHeatPump->heatPumps.size(), 0u);
}

TEST_F(EnergyPlusFixture, Initialization2_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.001,",
                                                      "  1,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLocation);

    // call with run flag off, loose limits on node min/max
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag off, nonzero minimums
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRateMinAvail = 0.1;
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.1, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag off, load side flow locked
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.24;
    thisCoolingPLHP->running = false;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.24, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at zero on load side
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at zero on source side
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.2;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.2, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(1.29, thisCoolingPLHP->sourceSideMassFlowRate, 0.1);

    // call with run flag ON, flow locked at zero on both sides
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.0;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideMassFlowRate, 0.001);

    // call with run flag ON, flow locked at nonzero both
    state->dataPlnt->PlantLoop(1).LoopSide(2).FlowLock = DataPlant::iFlowLock::Locked;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.inlet).MassFlowRate = 0.14;
    thisCoolingPLHP->running = true;
    thisCoolingPLHP->setOperatingFlowRatesASHP(*state);
    EXPECT_NEAR(0.14, thisCoolingPLHP->loadSideMassFlowRate, 0.001);
    EXPECT_NEAR(1.29, thisCoolingPLHP->sourceSideMassFlowRate, 0.1);
}

TEST_F(EnergyPlusFixture, TestSizing_FullyAutosizedCoolingWithCompanion_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  AirSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);

    Real64 const plantSizingLoadVolFlow = 0.01;
    Real64 const plantSizingLoadDeltaT = 1.0;

    Real64 const plantSizingSrcDeltaT = 10.0;

    Real64 const COP = 1;

    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.01;
    state->dataSize->PlantSizData(1).DeltaT = 1.0;
    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.03;
    state->dataSize->PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop1supplyComponent2.Name = thisCoolingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingPLHP->onInitLoopEquip(*state, myCoolingLoadLocation);
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // assign the plant sizing data
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;

    Real64 expectedLoadCp = 4197.93;
    Real64 expectedLoadRho = 999.898;
    Real64 expectedSourceCp = 1004.0;
    Real64 expectedSourceRho = 1.2;
    Real64 expectedLoadFlow = plantSizingLoadVolFlow;
    Real64 expectedCapacity = expectedLoadRho * expectedLoadFlow * expectedLoadCp * plantSizingLoadDeltaT;
    Real64 expectedSourceLoad = expectedCapacity * (1 + 1 / COP);
    Real64 expectedSourceFlow = expectedSourceLoad / (expectedSourceCp * expectedSourceRho * plantSizingSrcDeltaT);
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideASHP(*state);
    EXPECT_NEAR(expectedLoadFlow, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.1);
    EXPECT_NEAR(expectedCapacity, thisCoolingPLHP->referenceCapacity, 0.0001);

    // with a sizing run complete, we can also go ahead and get the design capacities...
    // they should be nonzero for the load side of things
    Real64 tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingPLHP->getDesignCapacities(*state, myCoolingLoadLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpMax, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpOpt, 0.001);

    // we can reset things and do a few more corner cases here

    // lets just try it again but with the plant sizing data set to zero flow, it should try to use the companion
    // but the companion isn't sized yet, so it should get zero conditions
    thisCoolingPLHP->loadSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingPLHP->sourceSideDesignVolFlowRate = DataSizing::AutoSize;
    thisCoolingPLHP->referenceCapacity = DataSizing::AutoSize;
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.0;
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideASHP(*state);
    EXPECT_NEAR(0.0, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(0.0, thisCoolingPLHP->referenceCapacity, 0.0001);

    // but now let's try to size the heating coil, which will try to use the cooling coil's sized data
    thisCoolingPLHP->loadSideDesignVolFlowRate = expectedLoadFlow;
    thisCoolingPLHP->sourceSideDesignVolFlowRate = expectedSourceFlow;
    thisCoolingPLHP->referenceCapacity = expectedCapacity;
    expectedLoadCp = 4185;
    expectedLoadRho = 983.2;
    expectedCapacity = expectedLoadRho * expectedLoadFlow * expectedLoadCp * plantSizingLoadDeltaT;
    expectedSourceLoad = expectedCapacity * (1 + 1 / COP);
    expectedSourceFlow = expectedSourceLoad / (expectedSourceCp * expectedSourceRho * plantSizingSrcDeltaT);
    thisHeatingPLHP->sizeLoadSide(*state);
    thisHeatingPLHP->sizeSrcSideASHP(*state);
    EXPECT_NEAR(expectedLoadFlow, thisHeatingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisHeatingPLHP->sourceSideDesignVolFlowRate, 0.1);
    EXPECT_NEAR(expectedCapacity, thisHeatingPLHP->referenceCapacity, 0.0001);
}

TEST_F(EnergyPlusFixture, TestSizing_HardsizedFlowAutosizedCoolingWithCompanion_AirSource)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  AirSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  Autosize,",
                                                      "  2.0,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  AirSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  Autosize,",
                                                      "  2.0,",
                                                      "  Autosize,",
                                                      "  1.0,",
                                                      "  1,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  1,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[1];

    // validate that we have the right ones
    EXPECT_EQ("HP COOLING SIDE", thisCoolingPLHP->name);
    EXPECT_EQ("HP HEATING SIDE", thisHeatingPLHP->name);

    // We'll set up two plant loops: a load and a source loop
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 2;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(2);

    Real64 const plantSizingLoadVolFlow = 0.01;
    Real64 const plantSizingLoadDeltaT = 1.0;

    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.01;
    state->dataSize->PlantSizData(1).DeltaT = 1.0;
    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.03;
    state->dataSize->PlantSizData(2).DeltaT = 1.0;

    auto &loop1supplyComponent1 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    auto &loop1supplyComponent2 = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(2);

    loop1supplyComponent1.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    loop1supplyComponent2.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    loop1supplyComponent1.Name = thisHeatingPLHP->name;
    loop1supplyComponent2.Name = thisCoolingPLHP->name;

    loop1supplyComponent1.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    loop1supplyComponent2.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;

    // the init call expects a "from" calling point
    PlantLocation myCoolingLoadLocation = PlantLocation(1, 2, 1, 1);
    PlantLocation myHeatingLoadLocation = PlantLocation(1, 2, 1, 2);

    // set a couple global flags
    state->dataGlobal->BeginEnvrnFlag = true;

    // initialize so the components can find themselves on the plant
    thisCoolingPLHP->onInitLoopEquip(*state, myCoolingLoadLocation);
    thisHeatingPLHP->onInitLoopEquip(*state, myHeatingLoadLocation);

    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // assign the plant sizing data
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;

    Real64 expectedLoadCp = 4197.93;
    Real64 expectedLoadRho = 999.898;
    Real64 expectedLoadFlow = plantSizingLoadVolFlow;
    Real64 expectedCapacity = expectedLoadRho * expectedLoadFlow * expectedLoadCp * plantSizingLoadDeltaT;
    Real64 expectedSourceFlow = 2.0;
    thisCoolingPLHP->sizeLoadSide(*state);
    thisCoolingPLHP->sizeSrcSideASHP(*state);
    EXPECT_NEAR(expectedLoadFlow, thisCoolingPLHP->loadSideDesignVolFlowRate, 0.0001);
    EXPECT_NEAR(expectedSourceFlow, thisCoolingPLHP->sourceSideDesignVolFlowRate, 0.1);
    EXPECT_NEAR(expectedCapacity, thisCoolingPLHP->referenceCapacity, 0.0001);

    // with a sizing run complete, we can also go ahead and get the design capacities...
    // they should be nonzero for the load side of things
    Real64 tmpMin = -1.0, tmpMax = -1.0, tmpOpt = -1.0;
    thisCoolingPLHP->getDesignCapacities(*state, myCoolingLoadLocation, tmpMax, tmpMin, tmpOpt);
    EXPECT_NEAR(0.0, tmpMin, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpMax, 0.001);
    EXPECT_NEAR(expectedCapacity, tmpOpt, 0.001);
}

TEST_F(EnergyPlusFixture, Test_DoPhysics)
{

    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  hp heating side,",
                                                      "  0.005,",
                                                      "  0.002,",
                                                      "  20000,",
                                                      "  3.0,",
                                                      "  1,",
                                                      "  CapCurveFuncTemp,",
                                                      "  EIRCurveFuncTemp,",
                                                      "  EIRCurveFuncPLR;",
                                                      "HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 5,",
                                                      "  node 6,",
                                                      "  WaterSource,",
                                                      "  node 7,",
                                                      "  node 8,",
                                                      "  hp cooling side,",
                                                      "  0.005,",
                                                      "  0.002,",
                                                      "  20000,",
                                                      "  3.0,",
                                                      "  1,",
                                                      "  CapCurveFuncTemp,",
                                                      "  EIRCurveFuncTemp,",
                                                      "  EIRCurveFuncPLR;",
                                                      "Curve:Biquadratic,",
                                                      "  CapCurveFuncTemp,",
                                                      "  1.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  5.0,",
                                                      "  10.0,",
                                                      "  24.0,",
                                                      "  35.0,",
                                                      "  ,",
                                                      "  ,",
                                                      "  Temperature,",
                                                      "  Temperature,",
                                                      "  Dimensionless;",
                                                      "Curve:Biquadratic,",
                                                      "  EIRCurveFuncTemp,",
                                                      "  1.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  1.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  5.0,",
                                                      "  10.0,",
                                                      "  24.0,",
                                                      "  35.0,",
                                                      "  ,",
                                                      "  ,",
                                                      "  Temperature,",
                                                      "  Temperature,",
                                                      "  Dimensionless;",
                                                      "Curve:Quadratic,",
                                                      "  EIRCurveFuncPLR,",
                                                      "  1.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  0.0,",
                                                      "  1.0;"});

    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideLoop = state->dataPlnt->PlantLoop(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);

    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(2u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a little setup here
    thisCoolingPLHP->loadSideLocation.loopNum = 1;
    thisCoolingPLHP->loadSideLocation.loopSideNum = 2;
    thisCoolingPLHP->loadSideLocation.branchNum = 1;
    thisCoolingPLHP->loadSideLocation.compNum = 1;
    thisCoolingPLHP->loadSideNodes.outlet = 1;

    // the factory would've called GetOnlySingleNode for the in/out pairs on the PLHP, add another one for the loop
    // outlet setpoint node
    state->dataLoopNodes->Node.allocate(5);
    PLHPPlantLoadSideLoop.TempSetPointNodeNum = 5;

    // set up the plant setpoint conditions and test for single setpoint operation
    PLHPPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPoint = 3.141;
    state->dataLoopNodes->Node(5).TempSetPoint = 2.718;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;

    // test for dual setpoint operation
    PLHPPlantLoadSideLoop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    state->dataLoopNodes->Node(thisCoolingPLHP->loadSideNodes.outlet).TempSetPointHi = 6.282;
    state->dataLoopNodes->Node(5).TempSetPointHi = 5.436;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType;

    state->dataHVACGlobal->TimeStepSys = 60;

    Real64 curLoad = -10000;

    thisCoolingPLHP->loadSideMassFlowRate = 0.3;
    thisCoolingPLHP->sourceSideMassFlowRate = 0.8;
    thisCoolingPLHP->loadSideInletTemp = 20;
    thisCoolingPLHP->sourceSideInletTemp = 20;
    thisCoolingPLHP->doPhysics(*state, curLoad);

    EXPECT_NEAR(thisCoolingPLHP->loadSideOutletTemp, 12.00, 0.1);
    EXPECT_NEAR(thisCoolingPLHP->sourceSideOutletTemp, 47.66, 0.1);
}

TEST_F(EnergyPlusFixture, CoolingMetering)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Cooling,",
                                                      "  hp cooling side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  0.0001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRCooling;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRCooling, "HP COOLING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisCoolingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisCoolingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisCoolingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisCoolingPLHP->sourceSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisCoolingPLHP->onInitLoopEquip(*state, myLoadLocation);

    int NumFound;

    std::string TypeOfComp = "HeatPump:PlantLoop:EIR:Cooling";
    std::string NameOfComp = thisCoolingPLHP->name;
    int NumVariables = GetNumMeteredVariables(*state, TypeOfComp, NameOfComp);
    Array1D_int VarIndexes(NumVariables);                            // Variable Numbers
    Array1D<OutputProcessor::VariableType> VarTypes(NumVariables);   // Variable Types (1=integer, 2=real, 3=meter)
    Array1D<OutputProcessor::TimeStepType> IndexTypes(NumVariables); // Variable Index Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar(NumVariables);        // units from enum for each variable
    std::map<int, DataGlobalConstants::ResourceType> ResourceTypes;  // ResourceTypes for each variable
    Array1D_string EndUses(NumVariables);                            // EndUses for each variable
    Array1D_string Groups(NumVariables);                             // Groups for each variable
    Array1D_string Names(NumVariables);                              // Variable Names for each variable

    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    GetMeteredVariables(
        *state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);

    EXPECT_EQ(2, NumFound);
    EXPECT_EQ(ResourceTypes.at(1), DataGlobalConstants::ResourceType::EnergyTransfer); // ENERGYTRANSFER
    EXPECT_EQ(EndUses(1), "");
    EXPECT_EQ(Groups(1), "PLANT");
    EXPECT_EQ(ResourceTypes.at(2), DataGlobalConstants::ResourceType::Electricity); // Electric
    EXPECT_EQ(EndUses(2), "COOLING");
    EXPECT_EQ(Groups(2), "PLANT");
}

TEST_F(EnergyPlusFixture, HeatingMetering)
{
    std::string const idf_objects = delimited_string({"HeatPump:PlantLoop:EIR:Heating,",
                                                      "  hp heating side,",
                                                      "  node 1,",
                                                      "  node 2,",
                                                      "  WaterSource,",
                                                      "  node 3,",
                                                      "  node 4,",
                                                      "  ,",
                                                      "  0.0001,",
                                                      "  0.0001,",
                                                      "  1000,",
                                                      "  3.14,",
                                                      "  ,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve,",
                                                      "  dummyCurve;",
                                                      "Curve:Linear,",
                                                      "  dummyCurve,",
                                                      "  0.95,",
                                                      "  0,",
                                                      "  1,",
                                                      "  1;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // set up the plant loops
    // first the load side
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSideComp = state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1);
    PLHPPlantLoadSideComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;
    PLHPPlantLoadSideComp.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType;
    // then the source side
    state->dataPlnt->PlantLoop(2).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(2).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &PLHPPlantLoadSourceComp = state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1);
    PLHPPlantLoadSourceComp.TypeOf_Num = DataPlant::TypeOf_HeatPumpEIRHeating;

    // the init call expects a "from" calling point
    PlantLocation myLoadLocation = PlantLocation(1, 2, 1, 1);

    // call the factory with a valid name to trigger reading inputs
    EIRPlantLoopHeatPump::factory(*state, DataPlant::TypeOf_HeatPumpEIRHeating, "HP HEATING SIDE");

    // verify the size of the vector and the processed condition
    EXPECT_EQ(1u, state->dataEIRPlantLoopHeatPump->heatPumps.size());

    // for now we know the order is maintained, so get each heat pump object
    EIRPlantLoopHeatPump *thisHeatingPLHP = &state->dataEIRPlantLoopHeatPump->heatPumps[0];

    // do a bit of extra wiring up to the plant
    PLHPPlantLoadSideComp.Name = thisHeatingPLHP->name;
    PLHPPlantLoadSideComp.NodeNumIn = thisHeatingPLHP->loadSideNodes.inlet;
    PLHPPlantLoadSourceComp.Name = thisHeatingPLHP->name;
    PLHPPlantLoadSourceComp.NodeNumIn = thisHeatingPLHP->sourceSideNodes.inlet;

    // call for all initialization
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    thisHeatingPLHP->onInitLoopEquip(*state, myLoadLocation);

    int NumFound;

    std::string TypeOfComp = "HeatPump:PlantLoop:EIR:Heating";
    std::string NameOfComp = thisHeatingPLHP->name;
    int NumVariables = GetNumMeteredVariables(*state, TypeOfComp, NameOfComp);
    Array1D_int VarIndexes(NumVariables);                            // Variable Numbers
    Array1D<OutputProcessor::VariableType> VarTypes(NumVariables);   // Variable Types (1=integer, 2=real, 3=meter)
    Array1D<OutputProcessor::TimeStepType> IndexTypes(NumVariables); // Variable Index Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar(NumVariables);        // units from enum for each variable
    std::map<int, DataGlobalConstants::ResourceType> ResourceTypes;  // ResourceTypes for each variable
    Array1D_string EndUses(NumVariables);                            // EndUses for each variable
    Array1D_string Groups(NumVariables);                             // Groups for each variable
    Array1D_string Names(NumVariables);                              // Variable Names for each variable

    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    GetMeteredVariables(
        *state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);

    EXPECT_EQ(2, NumFound);
    EXPECT_EQ(ResourceTypes.at(1), DataGlobalConstants::ResourceType::EnergyTransfer); // ENERGYTRANSFER
    EXPECT_EQ(EndUses(1), "");
    EXPECT_EQ(Groups(1), "PLANT");
    EXPECT_EQ(ResourceTypes.at(2), DataGlobalConstants::ResourceType::Electricity); // Electric
    EXPECT_EQ(EndUses(2), "HEATING");
    EXPECT_EQ(Groups(2), "PLANT");
}

#pragma clang diagnostic pop
#pragma clang diagnostic pop
