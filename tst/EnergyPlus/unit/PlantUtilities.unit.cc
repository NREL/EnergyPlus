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

// EnergyPlus::PurchasedAirManager (Ideal Loads) Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::PlantUtilities;
using namespace ObjexxFCL;
using namespace DataSizing;

TEST_F(EnergyPlusFixture, PlantUtilities_RegisterPlantCompDesignFlowTest1)
{
    // first call just puts first value in array
    int TestNodeNum1 = 123;
    Real64 TestFlowRate1 = 45.6;
    SaveNumPlantComps = 0;
    RegisterPlantCompDesignFlow(TestNodeNum1, TestFlowRate1);
    EXPECT_EQ(TestNodeNum1, CompDesWaterFlow(1).SupNode);
    EXPECT_EQ(TestFlowRate1, CompDesWaterFlow(1).DesVolFlowRate);

    // second call searches array and since node not found adds an entry to array
    int TestNodeNum2 = 234;
    Real64 TestFlowRate2 = 56.7;
    RegisterPlantCompDesignFlow(TestNodeNum2, TestFlowRate2);
    EXPECT_EQ(TestNodeNum2, CompDesWaterFlow(2).SupNode);
    EXPECT_EQ(TestFlowRate2, CompDesWaterFlow(2).DesVolFlowRate);

    // third call searches array and since node was found adds an entry to array
    Real64 TestFlowRate3 = 67.8;
    RegisterPlantCompDesignFlow(TestNodeNum1, TestFlowRate3);
    EXPECT_EQ(TestFlowRate3, CompDesWaterFlow(1).DesVolFlowRate);
}

TEST_F(EnergyPlusFixture, TestRegulateCondenserCompFlowReqOp)
{
    // This test captures all code paths through the RegulateCondenserCompFlowReqOp function
    // We only need a single component to check here
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    auto &thisComponent = DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1);
    Real64 flowRequest = 3.14;
    Real64 returnedFlow;

    // if the component's ON flag is false, then it should return zero flow request no matter the other values
    thisComponent.ON = false;

    thisComponent.CurOpSchemeType = DataPlant::HeatingRBOpSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::UncontrolledOpSchemeType; // NOT meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);

    // if the component's ON flag is true, then it needs to make decisions
    thisComponent.ON = true;

    thisComponent.CurOpSchemeType = DataPlant::HeatingRBOpSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::CoolingRBOpSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::CompSetPtBasedSchemeType; // meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(0.0, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);

    thisComponent.CurOpSchemeType = DataPlant::UncontrolledOpSchemeType; // NOT meaningful load

    thisComponent.MyLoad = 0.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
    thisComponent.MyLoad = 1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
    thisComponent.MyLoad = -1000.0;
    returnedFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(1, 1, 1, 1, flowRequest);
    EXPECT_NEAR(flowRequest, returnedFlow, 0.00001);
}

TEST_F(EnergyPlusFixture, TestAnyPlantSplitterMixerLacksContinuity)
{
    // This test captures all code paths through the AnyPlantSplitterMixerLacksContinuity function

    // We need to set up a two sided plant loop, we'll have one side not have a splitter for convenience
    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).SplitterExists = false;
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).Branch(1).NodeNumOut = 2;
    DataPlant::PlantLoop(1).LoopSide(2).Branch(2).NodeNumOut = 3;
    DataPlant::PlantLoop(1).LoopSide(2).SplitterExists = true;
    DataPlant::PlantLoop(1).LoopSide(2).Splitter.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Splitter(1).NodeNumIn = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Splitter(1).TotalOutletNodes = 2;
    DataPlant::PlantLoop(1).LoopSide(2).Splitter(1).BranchNumOut.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).Splitter(1).BranchNumOut(1) = 1;
    DataPlant::PlantLoop(1).LoopSide(2).Splitter(1).BranchNumOut(2) = 2;

    DataLoopNode::Node.allocate(3);

    // case 1: inlet flow and outlet flow match up
    DataLoopNode::Node(1).MassFlowRate = 3.0;
    DataLoopNode::Node(2).MassFlowRate = 1.0;
    DataLoopNode::Node(3).MassFlowRate = 2.0;
    EXPECT_FALSE(PlantUtilities::AnyPlantSplitterMixerLacksContinuity());

    // case 2: inlet flow > outlet flow
    DataLoopNode::Node(1).MassFlowRate = 4.0;
    DataLoopNode::Node(2).MassFlowRate = 1.0;
    DataLoopNode::Node(3).MassFlowRate = 2.0;
    EXPECT_TRUE(PlantUtilities::AnyPlantSplitterMixerLacksContinuity());

    // case 3: inlet flow < outlet flow
    DataLoopNode::Node(1).MassFlowRate = 1.0;
    DataLoopNode::Node(2).MassFlowRate = 2.0;
    DataLoopNode::Node(3).MassFlowRate = 3.0;
    EXPECT_TRUE(PlantUtilities::AnyPlantSplitterMixerLacksContinuity());

    // case 4: all zero flow
    DataLoopNode::Node(1).MassFlowRate = 0.0;
    DataLoopNode::Node(2).MassFlowRate = 0.0;
    DataLoopNode::Node(3).MassFlowRate = 0.0;
    EXPECT_FALSE(PlantUtilities::AnyPlantSplitterMixerLacksContinuity());
}

TEST_F(EnergyPlusFixture, TestPullCompInterconnectTrigger)
{
    // This test captures all code paths through the PullCompInterconnectTrigger function

    // We'll set up two plant loops, the one to test and the connected one
    // each one will need a single loop side, but no branches are checked or anything like that
    int thisLoopNum = 1, thisLoopSideNum = 1, thisBranchNum = 1, thisCompNum = 1;
    int connectedLoopNum = 2, connectedLoopSideNum = 1;
    int criteriaCheckIndex1 = 0, criteriaCheckIndex2 = 0, criteriaCheckIndex3 = 0;
    Real64 criteriaValue1 = 0.0, criteriaValue2 = 0.0, criteriaValue3 = 0.0;

    DataPlant::PlantLoop.allocate(2);
    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(2).LoopSide.allocate(1);
    auto &connectedLoopSide = DataPlant::PlantLoop(2).LoopSide(1);

    // the first time we call each criteria check, we should just get an index back and it should trigger the connected loop
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex1, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_MassFlowRate, criteriaValue1);
    EXPECT_EQ(1, criteriaCheckIndex1);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex2, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_Temperature, criteriaValue2);
    EXPECT_EQ(2, criteriaCheckIndex2);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex3, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_HeatTransferRate, criteriaValue3);
    EXPECT_EQ(3, criteriaCheckIndex3);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    // the second and afterward times we call each check, we need to actually expect it to check criteria

    // call it with a nonzero value here, and it should trigger the sim flag
    criteriaValue1 = 2.718;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex1, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_MassFlowRate, criteriaValue1);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    criteriaValue2 = 2.718;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex2, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_Temperature, criteriaValue2);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    criteriaValue3 = 2.718;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex3, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_HeatTransferRate, criteriaValue3);
    EXPECT_TRUE(connectedLoopSide.SimLoopSideNeeded);

    // call it with the same nonzero value here, and it should *not* trigger the sim flag
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex1, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_MassFlowRate, criteriaValue1);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);

    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex2, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_Temperature, criteriaValue2);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);

    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex3, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_HeatTransferRate, criteriaValue3);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);

    // call it with a tiny (within tolerance) change and it should still not trigger it
    criteriaValue1 += DataPlant::CriteriaDelta_MassFlowRate / 2.0;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex1, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_MassFlowRate, criteriaValue1);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);

    criteriaValue2 += DataPlant::CriteriaDelta_Temperature / 2.0;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex2, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_Temperature, criteriaValue2);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);

    criteriaValue3 += DataPlant::CriteriaDelta_HeatTransferRate / 2.0;
    connectedLoopSide.SimLoopSideNeeded = false;
    PlantUtilities::PullCompInterconnectTrigger(thisLoopNum, thisLoopSideNum, thisBranchNum, thisCompNum, criteriaCheckIndex3, connectedLoopNum,
                                                connectedLoopSideNum, DataPlant::CriteriaType_HeatTransferRate, criteriaValue3);
    EXPECT_FALSE(connectedLoopSide.SimLoopSideNeeded);
}

TEST_F(EnergyPlusFixture, TestIntegerIsWithinTwoValues)
{

    // valid expected true cases
    EXPECT_TRUE(PlantUtilities::IntegerIsWithinTwoValues(1, 0, 2));
    EXPECT_TRUE(PlantUtilities::IntegerIsWithinTwoValues(0, -1, 1));

    // valid expected false cases
    EXPECT_FALSE(PlantUtilities::IntegerIsWithinTwoValues(0, 1, 2));
    EXPECT_FALSE(PlantUtilities::IntegerIsWithinTwoValues(-1, 0, 1));

    // odd expected false cases
    EXPECT_FALSE(PlantUtilities::IntegerIsWithinTwoValues(1, 2, 0));
}

TEST_F(EnergyPlusFixture, TestCheckPlantConvergence)
{

    // For this case, we need a single plant loop and loop side
    // We will leverage the LogPlantConvergencePoints function to manage the history terms
    // That function is nice because it is very tightly contained, so we don't have to set up a lot of global state
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataLoopNode::Node.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).NodeNumIn = 1;
    DataPlant::PlantLoop(1).LoopSide(1).NodeNumOut = 2;
    auto &inNode = DataLoopNode::Node(1);
    auto &outNode = DataLoopNode::Node(2);
    Real64 const roomTemp = 25.0;
    Real64 const nonZeroFlow = 3.14;

    // History terms should be allocated to 5 zeros
    EXPECT_EQ(5u, DataPlant::PlantLoop(1).LoopSide(1).InletNode.TemperatureHistory.size());
    EXPECT_EQ(5u, DataPlant::PlantLoop(1).LoopSide(1).OutletNode.TemperatureHistory.size());
    EXPECT_EQ(5u, DataPlant::PlantLoop(1).LoopSide(1).InletNode.MassFlowRateHistory.size());
    EXPECT_EQ(5u, DataPlant::PlantLoop(1).LoopSide(1).OutletNode.MassFlowRateHistory.size());
    EXPECT_NEAR(0.0, sum(DataPlant::PlantLoop(1).LoopSide(1).InletNode.TemperatureHistory), 0.001);
    EXPECT_NEAR(0.0, sum(DataPlant::PlantLoop(1).LoopSide(1).OutletNode.TemperatureHistory), 0.001);
    EXPECT_NEAR(0.0, sum(DataPlant::PlantLoop(1).LoopSide(1).InletNode.MassFlowRateHistory), 0.001);
    EXPECT_NEAR(0.0, sum(DataPlant::PlantLoop(1).LoopSide(1).OutletNode.MassFlowRateHistory), 0.001);

    // If we check the plant convergence right now with first hvac true, it should require a resimulation
    EXPECT_FALSE(PlantUtilities::CheckPlantConvergence(1, 1, true));

    // But if we check it with first hvac false, everything should be stable and pass
    EXPECT_TRUE(PlantUtilities::CheckPlantConvergence(1, 1, false));

    // Now let's introduce a disturbance by changing the inlet node temperature and logging it
    inNode.Temp = roomTemp;
    PlantUtilities::LogPlantConvergencePoints(false);
    // We expect it to be false here since the temperature changed
    EXPECT_FALSE(PlantUtilities::CheckPlantConvergence(1, 1, false));
    // But if we run it 4 more times and let the value propagate, we expect it to be stable and pass
    // Need to call it 5 times total to fully initialize the history
    for (int i = 1; i < 5; ++i) {
        PlantUtilities::LogPlantConvergencePoints(false);
    }
    EXPECT_TRUE(PlantUtilities::CheckPlantConvergence(1, 1, false));

    // Repeat this for the outlet node temperature
    outNode.Temp = roomTemp;
    PlantUtilities::LogPlantConvergencePoints(false);
    // We expect it to be false here since the temperature changed
    EXPECT_FALSE(PlantUtilities::CheckPlantConvergence(1, 1, false));
    // But if we run it 4 more times and let the value propagate, we expect it to be stable and pass
    // Need to call it 5 times total to fully initialize the history
    for (int i = 1; i < 5; ++i) {
        PlantUtilities::LogPlantConvergencePoints(false);
    }
    EXPECT_TRUE(PlantUtilities::CheckPlantConvergence(1, 1, false));

    // Repeat this for the inlet node mass flow rate
    inNode.MassFlowRate = nonZeroFlow;
    PlantUtilities::LogPlantConvergencePoints(false);
    // We expect it to be false here since the temperature changed
    EXPECT_FALSE(PlantUtilities::CheckPlantConvergence(1, 1, false));
    // But if we run it 4 more times and let the value propagate, we expect it to be stable and pass
    // Need to call it 5 times total to fully initialize the history
    for (int i = 1; i < 5; ++i) {
        PlantUtilities::LogPlantConvergencePoints(false);
    }
    EXPECT_TRUE(PlantUtilities::CheckPlantConvergence(1, 1, false));

    // And finally the outlet node mass flow rate
    outNode.MassFlowRate = nonZeroFlow;
    PlantUtilities::LogPlantConvergencePoints(false);
    // We expect it to be false here since the temperature changed
    EXPECT_FALSE(PlantUtilities::CheckPlantConvergence(1, 1, false));
    // But if we run it 4 more times and let the value propagate, we expect it to be stable and pass
    // Need to call it 5 times total to fully initialize the history
    for (int i = 1; i < 5; ++i) {
        PlantUtilities::LogPlantConvergencePoints(false);
    }
    EXPECT_TRUE(PlantUtilities::CheckPlantConvergence(1, 1, false));
}
