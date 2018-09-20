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

// EnergyPlus::EMSManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <CurveManager.hh>
#include <DataRuntimeLanguage.hh>
#include <EMSManager.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EMSManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataRuntimeLanguage;
using namespace EnergyPlus::PlantUtilities;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, EMSManager_TestForUniqueEMSActuators)
{
    EMSActuatorAvailable.allocate(100);

    std::string componentTypeName1("Chiller1");
    std::string componentTypeName2("Chiller2");
    std::string uniqueIDName1("Plant Component Chiller:Electric:ReformulatedEIR");
    std::string controlTypeName1("On/Off Supervisory");
    std::string units1("None");
    bool EMSActuated1(true);
    bool testBoolean1(true);
    bool testBoolean2(true);
    bool testBoolean3(true);

    // calling three times but twice with same names should still result in only two item in the resulting list
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean1);
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean2);
    SetupEMSActuator(componentTypeName2, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean3);
    EXPECT_EQ(2, numEMSActuatorsAvailable);

    // repeat with integers
    std::string controlTypeName2("ModeOfSomething");
    int testInt1(7);
    int testInt2(9);
    int testInt3(11);
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt1);
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt2);
    SetupEMSActuator(componentTypeName2, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt3);
    EXPECT_EQ(4, numEMSActuatorsAvailable);

    // repeat with reals
    std::string controlTypeName3("ValueOfResults");
    Real64 testReal1(0.123);
    Real64 testReal2(0.456);
    Real64 testReal3(0.789);
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal1);
    SetupEMSActuator(componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal2);
    SetupEMSActuator(componentTypeName2, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal3);
    EXPECT_EQ(6, numEMSActuatorsAvailable);

    EMSActuatorAvailable.deallocate();
}

TEST_F(EnergyPlusFixture, Dual_NodeTempSetpoints)
{

    std::string const idf_objects = delimited_string({
        "Version,8.4;",

        "OutdoorAir:Node, Test node;",

        "EnergyManagementSystem:Actuator,",
        "TempSetpointLo,          !- Name",
        "Test node,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Minimum Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpointHi,          !- Name",
        "Test node,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Maximum Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Dual Setpoint Test Manager,  !- Name",
        "BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
        "DualSetpointTestControl;  !- Program Name 1",

        "EnergyManagementSystem:Program,",
        "DualSetpointTestControl,",
        "Set TempSetpointLo = 16.0,",
        "Set TempSetpointHi  = 20.0;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutAirNodeManager::SetOutAirNodes();

    EMSManager::CheckIfAnyEMS();

    EMSManager::FinishProcessingUserInput = true;

    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);

    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);

    EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointHi, 20.0, 0.000001);

    EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointLo, 16.0, 0.000001);
}

TEST_F(EnergyPlusFixture, CheckActuatorInit)
{
    // this test checks that new actuators have the Erl variable associated with them set to Null right away, issue #5710
    std::string const idf_objects = delimited_string({
        "Version,8.6;",

        "OutdoorAir:Node, Test node;",

        "EnergyManagementSystem:Actuator,",
        "TempSetpointLo,          !- Name",
        "Test node,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Minimum Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Dual Setpoint Test Manager,  !- Name",
        "EndOfSystemTimestepBeforeHVACReporting,  !- EnergyPlus Model Calling Point",
        "DualSetpointTestControl;  !- Program Name 1",

        "EnergyManagementSystem:Program,",
        "DualSetpointTestControl,",
        "Set TempSetpointLo = 16.0;",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    OutAirNodeManager::SetOutAirNodes();
    EMSManager::GetEMSInput();

    // now check that Erl variable is Null
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(1).Value.Type, DataRuntimeLanguage::ValueNull);
}

TEST_F(EnergyPlusFixture, SupervisoryControl_PlantComponent_SetActuatedBranchFlowRate)
{

    // test EMS actuator for Plant Component
    // test SetActuatedBranchFlowRate for expected response

    std::string const idf_objects = delimited_string({
        " EnergyManagementSystem:Actuator,",
        "  CoilActuator,          !- Name",
        "  Zone1FanCoilHeatingCoil,  !- Actuated Component Unique Name",
        "  Plant Component Coil:Heating:Water,    !- Actuated Component Type",
        "  On/Off Supervisory;    !- Actuated Component Control Type",

        " EnergyManagementSystem:ProgramCallingManager,",
        "  Supervisory Control Manager,  !- Name",
        "  BeginTimestepBeforePredictor,  !- EnergyPlus Model Calling Point",
        "  HeatCoilController;  !- Program Name 1",

        " EnergyManagementSystem:Program,",
        "  HeatCoilController,",
        "  Set CoilActuator = 0.0;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // sets number of EMS objects
    EMSManager::CheckIfAnyEMS();

    // allows NodeSetpoint and AvailabilityManagers actuators to be setup
    EMSManager::FinishProcessingUserInput = true;

    // set up plant loop
    DataPlant::TotNumLoops = 1;
    PlantLoop.allocate(1);
    PlantLoop(1).Name = "MyPlant";
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    // create 2 components on a single branch to simulate water flow control for entire branch
    PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(2);
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = 41; // Coil:Heating:Water
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "Zone1FanCoilHeatingCoil";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).TypeOf_Num = 21; // Pipe:Adiabatic
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).Name = "Pipe";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).NodeNumIn = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).NodeNumOut = 3;
    PlantCondLoopOperation::SetupPlantEMSActuators();

    // set flow, max and maxavail on the nodes
    Node.allocate(3);
    Real64 NodeMdot(1.5);
    Node(1).MassFlowRate = NodeMdot;
    Node(1).MassFlowRateMax = NodeMdot;
    Node(1).MassFlowRateMaxAvail = NodeMdot;
    Node(1).MassFlowRateRequest = NodeMdot;
    Node(2).MassFlowRate = NodeMdot;
    Node(2).MassFlowRateMax = NodeMdot;
    Node(2).MassFlowRateMaxAvail = NodeMdot;
    Node(2).MassFlowRateRequest = NodeMdot;
    Node(3).MassFlowRate = NodeMdot;
    Node(3).MassFlowRateMax = NodeMdot;
    Node(3).MassFlowRateMaxAvail = NodeMdot;
    Node(3).MassFlowRateRequest = NodeMdot;

    bool anyRan;
    // set up EMS
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);

    // set dummy EMS value
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue = 1.0;

    // dummy value set above should be zero'd on this call since EMS 0's values on begin environment (whether EMS program runs on this call or not)
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);

    EXPECT_FALSE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 0.0, 0.000001);

    // expect node data to represent full flow
    // SetActuatedBranchFlowRate( CompFlow, ActuatedNode, LoopNum, LoopSideNum, BranchNum, ResetMode )
    SetActuatedBranchFlowRate(NodeMdot, 1, 1, 1, 1, false);
    EXPECT_EQ(Node(1).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    SetActuatedBranchFlowRate(NodeMdot, 2, 1, 1, 1, false);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, NodeMdot);

    // set dummy EMS value
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue = 1.0;

    // dummy value set above should remain on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, anyRan);

    EXPECT_FALSE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 1.0, 0.000001);
    SetActuatedBranchFlowRate(NodeMdot, 1, 1, 1, 1, false);
    EXPECT_EQ(Node(1).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    SetActuatedBranchFlowRate(NodeMdot, 2, 1, 1, 1, false);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, NodeMdot);

    // dummy value set above should reset to 0 on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
    // override flag should also be true
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan);

    EXPECT_TRUE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 0.0, 0.000001);

    // expect node data to represent no flow. Request is also 0's in this function. Max and MaxAvail are not changed
    SetActuatedBranchFlowRate(NodeMdot, 1, 1, 1, 1, false);
    EXPECT_EQ(Node(1).MassFlowRate, 0.0);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, 0.0);
    EXPECT_EQ(Node(2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, 0.0);
    SetActuatedBranchFlowRate(NodeMdot, 2, 1, 1, 1, false);
    EXPECT_EQ(Node(2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, 0.0);
    EXPECT_EQ(Node(3).MassFlowRate, 0.0);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, 0.0);
}

TEST_F(EnergyPlusFixture, SupervisoryControl_PlantComponent_SetComponentFlowRate)
{

    // test EMS actuator for Plant Component
    // test SetComponentFlowRate for expected response

    std::string const idf_objects = delimited_string({
        " EnergyManagementSystem:Actuator,",
        "  CoilActuator,          !- Name",
        "  Zone1FanCoilHeatingCoil,  !- Actuated Component Unique Name",
        "  Plant Component Coil:Heating:Water,    !- Actuated Component Type",
        "  On/Off Supervisory;    !- Actuated Component Control Type",

        " EnergyManagementSystem:ProgramCallingManager,",
        "  Supervisory Control Manager,  !- Name",
        "  BeginTimestepBeforePredictor,  !- EnergyPlus Model Calling Point",
        "  HeatCoilController;  !- Program Name 1",

        " EnergyManagementSystem:Program,",
        "  HeatCoilController,",
        "  Set CoilActuator = 0.0;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // sets number of EMS objects
    EMSManager::CheckIfAnyEMS();

    // allows NodeSetpoint and AvailabilityManagers actuators to be setup
    EMSManager::FinishProcessingUserInput = true;

    // set up plant loop
    DataPlant::TotNumLoops = 1;
    PlantLoop.allocate(1);
    PlantLoop(1).Name = "MyPlant";
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    // create 2 components on a single branch to simulate water flow control for entire branch
    PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(2);
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = 41; // Coil:Heating:Water
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "Zone1FanCoilHeatingCoil";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).TypeOf_Num = 21; // Pipe:Adiabatic
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).Name = "Pipe";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).NodeNumIn = 2;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(2).NodeNumOut = 3;
    PlantCondLoopOperation::SetupPlantEMSActuators();

    // set flow, max and maxavail on the nodes
    Node.allocate(3);
    Real64 NodeMdot(1.5);
    Node(1).MassFlowRate = NodeMdot;
    Node(1).MassFlowRateMax = NodeMdot;
    Node(1).MassFlowRateMaxAvail = NodeMdot;
    Node(1).MassFlowRateRequest = NodeMdot;
    Node(2).MassFlowRate = NodeMdot;
    Node(2).MassFlowRateMax = NodeMdot;
    Node(2).MassFlowRateMaxAvail = NodeMdot;
    Node(2).MassFlowRateRequest = NodeMdot;
    Node(3).MassFlowRate = NodeMdot;
    Node(3).MassFlowRateMax = NodeMdot;
    Node(3).MassFlowRateMaxAvail = NodeMdot;
    Node(3).MassFlowRateRequest = NodeMdot;

    bool anyRan;
    // set up EMS
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    // set dummy EMS value
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue = 1.0;

    // dummy value set above should be zero'd on this call since EMS 0's values on begin environment (whether EMS program runs on this call or not)
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);

    EXPECT_FALSE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 0.0, 0.000001);

    // expect node data to represent full flow
    // SetComponentFlowRate( CompFlow, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex )
    SetComponentFlowRate(NodeMdot, 1, 2, 1, 1, 1, 1);
    EXPECT_EQ(Node(1).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    SetComponentFlowRate(NodeMdot, 2, 3, 1, 1, 1, 1);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, NodeMdot);

    // set dummy EMS value
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue = 1.0;

    // dummy value set above should remain on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, anyRan);

    EXPECT_FALSE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 1.0, 0.000001);

    // expect node data to represent full flow
    SetComponentFlowRate(NodeMdot, 1, 2, 1, 1, 1, 1);
    EXPECT_EQ(Node(1).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    SetComponentFlowRate(NodeMdot, 2, 3, 1, 1, 1, 1);
    EXPECT_EQ(Node(2).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRate, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, NodeMdot);

    // dummy value set above should reset to 0 on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
    // override flag should also be true
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan);

    EXPECT_TRUE(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideOn);
    EXPECT_NEAR(PlantLoop(1).LoopSide(1).Branch(1).Comp(1).EMSLoadOverrideValue, 0.0, 0.000001);
    Real64 tempNodeMdot(NodeMdot);

    // expect node data to represent no flow. Max, MaxAvail, and Request are not changed
    SetComponentFlowRate(tempNodeMdot, 1, 2, 1, 1, 1, 1);
    EXPECT_EQ(Node(1).MassFlowRate, 0.0);
    EXPECT_EQ(Node(1).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(1).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    tempNodeMdot = NodeMdot;
    SetComponentFlowRate(tempNodeMdot, 2, 3, 1, 1, 1, 1);
    EXPECT_EQ(Node(2).MassFlowRate, 0.0);
    EXPECT_EQ(Node(2).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(2).MassFlowRateRequest, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRate, 0.0);
    EXPECT_EQ(Node(3).MassFlowRateMax, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateMaxAvail, NodeMdot);
    EXPECT_EQ(Node(3).MassFlowRateRequest, NodeMdot);
}

TEST_F(EnergyPlusFixture, Test_EMSLogic)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Nov 2015
    // Tests to evaluate robustness of EMS programming language math expressions
    // specific issue was use of "-" just after operand, e.g., IF MyVar == -X,

    std::string const idf_objects = delimited_string({
        "Version,8.4;",

        "OutdoorAir:Node, Test node 1;",
        "OutdoorAir:Node, Test node 2;",
        "OutdoorAir:Node, Test node 3;",
        "OutdoorAir:Node, Test node 4;",
        "OutdoorAir:Node, Test node 5;",
        "OutdoorAir:Node, Test node 6;",
        "OutdoorAir:Node, Test node 7;",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint1,          !- Name",
        "Test node 1,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint2,          !- Name",
        "Test node 2,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint3,          !- Name",
        "Test node 3,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint4,          !- Name",
        "Test node 4,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint5,          !- Name",
        "Test node 5,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint6,          !- Name",
        "Test node 6,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint7,          !- Name",
        "Test node 7,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Logic Manager 1,  !- Name",
        "BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
        "LogicTest1;  !- Program Name 1",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Logic Manager 2,  !- Name",
        "BeginTimestepBeforePredictor,  !- EnergyPlus Model Calling Point",
        "LogicTest2;  !- Program Name 1",

        "EnergyManagementSystem:Program,",
        "LogicTest1,",
        "Set MyVar1 = 10,",
        "Set MyVar2 = -10,",
        "Set MyVar3 = -10+3-1-2,",
        "Set MyVar4 = 10,",
        "Set MyVar5 = -PI,",
        "Set MyVar6 = ( 10 - ( ( @MOD 15.5 3 ) * 40 ) ),",
        "Set MyVar7 = ( 9.9 - ( @MOD 15.5 3 ) * 20 ),",

        "IF MyVar1 == 10,",
        "  Set TempSetpoint1 = 11.0,",
        "ELSE,",
        "  Set TempSetpoint1 = 21.0,",
        "ENDIF,",
        "IF MyVar2 == -10,",
        "  Set TempSetpoint2 = 12.0,",
        "ELSE,",
        "  Set TempSetpoint2 = 22.0,",
        "ENDIF,",
        "IF -10 == MyVar3,",
        "  Set TempSetpoint3 = 13.0,",
        "ELSE,",
        "  Set TempSetpoint3 = 23.0,",
        "ENDIF,",
        "IF MyVar4 == -20+30,",
        "  Set TempSetpoint4 = 14.0,",
        "ELSE,",
        "  Set TempSetpoint4 = 24.0,",
        "ENDIF,",
        "IF MyVar5 == -PI,",
        "  Set TempSetpoint5 = 15.0,",
        "ELSE,",
        "  Set TempSetpoint5 = 25.0,",
        "ENDIF,",
        "IF MyVar6 == -10,",
        "  Set TempSetpoint6 = 16.0,",
        "ELSE,",
        "  Set TempSetpoint6 = 26.0,",
        "ENDIF,",
        "IF MyVar7 > -11.0+3-1+8.89,",
        "  Set TempSetpoint7 = 17.0,",
        "ELSE,",
        "  Set TempSetpoint7 = 27.0,",
        "ENDIF;",

        "EnergyManagementSystem:Program,",
        "LogicTest2,",
        "Set MyVar1 = 10,",
        "Set MyVar2 = -10,",
        "Set MyVar3 = -10 + 3 - 1 - 2,",
        "Set MyVar4 = 10,",
        "Set MyVar5 = -PI,",
        "Set MyVar6 = ( 10 - ( ( @MOD 15.5 3 ) * 40 ) ),",
        "Set MyVar7 = ( 9.9 - ( @MOD 15.5 3 ) * 20 ),",

        "IF ( MyVar1 <> 10 ),",
        "  Set TempSetpoint1 = 11.0,",
        "ELSE,",
        "  Set TempSetpoint1 = 21.0,",
        "ENDIF,",
        "IF ( MyVar2 <> -10 ),",
        "  Set TempSetpoint2 = 12.0,",
        "ELSE,",
        "  Set TempSetpoint2 = 22.0,",
        "ENDIF,",
        "IF ( -10 <> MyVar3 ),",
        "  Set TempSetpoint3 = 13.0,",
        "ELSE,",
        "  Set TempSetpoint3 = 23.0,",
        "ENDIF,",
        "IF ( MyVar4 <> ( -20+30 ) ),",
        "  Set TempSetpoint4 = 14.0,",
        "ELSE,",
        "  Set TempSetpoint4 = 24.0,",
        "ENDIF,",
        "IF ( MyVar5 <> -PI ),",
        "  Set TempSetpoint5 = 15.0,",
        "ELSE,",
        "  Set TempSetpoint5 = 25.0,",
        "ENDIF,",
        "IF ( MyVar6 <> -10 ),",
        "  Set TempSetpoint6 = 16.0,",
        "ELSE,",
        "  Set TempSetpoint6 = 26.0,",
        "ENDIF,",
        "IF ( MyVar7 == -0.1 ),",
        "  Set TempSetpoint7 = 17.0,",
        "ELSE,",
        "  Set TempSetpoint7 = 27.0,",
        "ENDIF;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutAirNodeManager::SetOutAirNodes();

    EMSManager::CheckIfAnyEMS();
    EMSManager::FinishProcessingUserInput = true;
    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);

    EXPECT_NEAR(DataLoopNode::Node(1).TempSetPoint, 11.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(2).TempSetPoint, 12.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(3).TempSetPoint, 13.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(4).TempSetPoint, 14.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(5).TempSetPoint, 15.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(6).TempSetPoint, 16.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 17.0, 0.0000001);

    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan);

    EXPECT_NEAR(DataLoopNode::Node(1).TempSetPoint, 21.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(2).TempSetPoint, 22.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(3).TempSetPoint, 23.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(4).TempSetPoint, 24.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(5).TempSetPoint, 25.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(6).TempSetPoint, 26.0, 0.0000001);
    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 27.0, 0.0000001);
}

TEST_F(EnergyPlusFixture, Debug_EMSLogic)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Nov 2015
    // Tests to evaluate robustness of EMS programming language math expressions
    // specific issue was use of "-" just after operand, e.g., Set MyVar == -X,
    // This unit test is purposely singular to enable easy debugging
    // If a problem is suspected, change this unit test accordingly and test
    // Much easier to single step this unit test than a more complex version

    std::string const idf_objects = delimited_string({
        "Version,8.4;",

        "OutdoorAir:Node, Test node 1;",

        "EnergyManagementSystem:Actuator,", "TempSetpoint1,          !- Name", "Test node 1,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type", "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:ProgramCallingManager,", "Logic Manager 1,  !- Name", "BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
        "LogicTest1;  !- Program Name 1",

        "EnergyManagementSystem:Program,", "LogicTest1,", "Set MyVar1 = ( -2 ),", "Set MyVar2 = ( -2 ),", "Set TempSetpoint1 = MyVar1 / MyVar2;",

        //		"IF MyVar1 == 8,",
        //		"  Set TempSetpoint1 = 11.0,",
        //		"ELSE,",
        //		"  Set TempSetpoint1 = 21.0,",
        //		"ENDIF;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutAirNodeManager::SetOutAirNodes();

    EMSManager::CheckIfAnyEMS();
    EMSManager::FinishProcessingUserInput = true;
    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);

    EXPECT_NEAR(DataLoopNode::Node(1).TempSetPoint, 1.0, 0.0000001);
}

TEST_F(EnergyPlusFixture, TestAnyRanArgument)
{
    // small test to demonstrate new boolean argument.
    // shows a simple way to setup sensor on a node, need to call SetupNodeVarsForReporting()

    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "OutdoorAir:Node, Test node;",

        "EnergyManagementSystem:Sensor,",
        "Node_mdot,",
        "Test node,",
        "System Node Mass Flow Rate;",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Test inside HVAC system iteration Loop,",
        "InsideHVACSystemIterationLoop,",
        "Test_InsideHVACSystemIterationLoop;",

        "EnergyManagementSystem:Program,",
        "Test_InsideHVACSystemIterationLoop,",
        "set dumm1 = Node_mdot;",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutAirNodeManager::SetOutAirNodes();
    NodeInputManager::SetupNodeVarsForReporting();
    EMSManager::CheckIfAnyEMS();

    EMSManager::FinishProcessingUserInput = true;

    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    EXPECT_FALSE(anyRan);

    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);
    EXPECT_FALSE(anyRan);

    EMSManager::ManageEMS(DataGlobals::emsCallFromHVACIterationLoop, anyRan);
    EXPECT_TRUE(anyRan);
}

TEST_F(EnergyPlusFixture, TestUnInitializedEMSVariable1)
{
    // this tests the new initialized variable added to Erl variable value data structure, for issue #4943
    // this is also what is checked to see if an actuator has been used for issue #4404.
    std::string const idf_objects = delimited_string({
        "Version,8.6;",

        "EnergyManagementSystem:GlobalVariable,",
        "TempSetpoint1;          !- Name",

        "EnergyManagementSystem:Program,",
        "InitVariableTest,",
        "Set TempSetpoint1 = 21.0;"

        "EnergyManagementSystem:ProgramCallingManager,",
        "Test Program Manager 1,  !- Name",
        "BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
        "InitVariableTest;  !- Program Name 1",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    EMSManager::CheckIfAnyEMS();
    EMSManager::FinishProcessingUserInput = true;
    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    // Expect the variable to not yet be initialized
    EXPECT_FALSE(ErlVariable(25).Value.initialized);
    // next run a small program that sets the value
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginNewEvironment, anyRan);
    // check that it worked and the value came thru
    EXPECT_NEAR(ErlVariable(25).Value.Number, 21.0, 0.0000001);
    // check of state to see if now initialized
    EXPECT_TRUE(ErlVariable(25).Value.initialized);
}

TEST_F(EnergyPlusFixture, TestUnInitializedEMSVariable2)
{
    // this tests the new initialized variable added to Erl variable value data structure in a slightly different way
    // we call the routine EvaluateExpression and examine the new bool argument for fatal errors.
    std::string const idf_objects = delimited_string({
        "Version,8.6;",

        "OutdoorAir:Node, Test node 1;",

        "EnergyManagementSystem:Actuator,",
        "TempSetpoint1,          !- Name",
        "Test node 1,  !- Actuated Component Unique Name",
        "System Node Setpoint,    !- Actuated Component Type",
        "Temperature Setpoint;    !- Actuated Component Control Type",

        "EnergyManagementSystem:Program,",
        "SetNodeSetpointTest,",
        "Set TempSetpoint1 = testGlobalVar;"

        "EnergyManagementSystem:Program,",
        "SetGlobalValue,",
        "SET testGlobalVar = 21.0;"

        "EnergyManagementSystem:GlobalVariable, ",
        "testGlobalVar;"

        "EnergyManagementSystem:ProgramCallingManager,",
        "Test Program Manager 1,  !- Name",
        "BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
        "SetNodeSetpointTest;  !- Program Name 1",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Test Program Manager 2,  !- Name",
        "BeginTimestepBeforePredictor,  !- EnergyPlus Model Calling Point",
        "SetGlobalValue;  !- Program Name 1",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutAirNodeManager::SetOutAirNodes();

    EMSManager::CheckIfAnyEMS();
    EMSManager::FinishProcessingUserInput = true;
    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromSetupSimulation, anyRan);
    // Expect the variable to not yet be initialized, call EvaluateExpresssion and check argument

    ErlValueType ReturnValue;
    bool seriousErrorFound = false;
    EMSManager::FinishProcessingUserInput = false;
    ReturnValue = RuntimeLanguageProcessor::EvaluateExpression(
        ErlStack(UtilityRoutines::FindItemInList("SETNODESETPOINTTEST", ErlStack)).Instruction(1).Argument2,
        seriousErrorFound); // we just check the logic and don't throw the fatal errors.
    EXPECT_TRUE(seriousErrorFound);

    // next run a small program that sets the global variable value
    EMSManager::ManageEMS(DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan);
    // now check that it worked, should stay false
    seriousErrorFound = false;
    ReturnValue = RuntimeLanguageProcessor::EvaluateExpression(
        ErlStack(UtilityRoutines::FindItemInList("SETNODESETPOINTTEST", ErlStack)).Instruction(1).Argument2, seriousErrorFound);
    EXPECT_FALSE(seriousErrorFound);
}

TEST_F(EnergyPlusFixture, EMSManager_CheckIfAnyEMS_OutEMS)
{

    using DataGlobals::AnyEnergyManagementSystemInModel;

    std::string const idf_objects = delimited_string({
        "  Output:EnergyManagementSystem,                                                                ",
        "    Verbose,                 !- Actuator Availability Dictionary Reporting                      ",
        "    Verbose,                 !- Internal Variable Availability Dictionary Reporting             ",
        "    Verbose;                 !- EMS Runtime Language Debug Output Level                         ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    CheckIfAnyEMS();
    EXPECT_TRUE(AnyEnergyManagementSystemInModel);
}

TEST_F(EnergyPlusFixture, EMSManager_TestFuntionCall)
{
    // test to demonstrate accurate function calls.

    std::string const idf_objects = delimited_string({

        "Curve:Quadratic,",
        "  TestCurve,       !- Name",
        "  0.8,             !- Coefficient1 Constant",
        "  0.2,             !- Coefficient2 x",
        "  0.0,             !- Coefficient3 x**2",
        "  0.5,             !- Minimum Value of x",
        "  1.5;             !- Maximum Value of x",

        "EnergyManagementSystem:ProgramCallingManager,",
        "Test inside HVAC system iteration Loop,",
        "InsideHVACSystemIterationLoop,",
        "Test_InsideHVACSystemIterationLoop;",

        "EnergyManagementSystem:Program,",
        "Test_InsideHVACSystemIterationLoop,",
        "set Var1 = @Round 2.1,",
        "set Var2 = @Mod 7 3,",
        "set Var3 = @Sin 0.785398163,", // 45 degrees
        "set Var4 = @Cos 0.785398163,", // 45 degrees
        "set Var5 = @ArcCos 0.70710678090550916,",
        "set Var6 = @ArcSin 0.70710678090550916,",
        "set Var7 = @DegToRad 45.0,",
        "set Var8 = @RadToDeg 0.785398163,",
        "set Var9 = @Exp 1.0,",
        "set Var10 = @Ln 2.0,",
        "set Var11 = @Max 0.5 1.5,",
        "set Var12 = @Min 0.5 1.5,",
        "set Var13 = @Abs 1.3,", // -3.1 does not work, (-3.1) does not work, "set absNum = -3.1" and use @Abs absNum does work if EMS line of code
                                 // occurs before this line
        "set Var14 = @SEEDRANDOM 2.65,",
        "set Var15 = @RANDOMUNIFORM 0.0 1.0,",
        "set Var16 = @RANDOMNORMAL 1.5 0.5 0.75 2.25,", // negative number does not get processed correctly
        "set Var17 = @RhoAirFnPbTdbW 101325.0 30.0 0.01,",
        "set Var18 = @CpAirFnWTdb 0.01 30.0,",
        "set Var19 = @HfgAirFnWTdb 0.01 30.0,",
        "set Var20 = @HgAirFnWTdb 0.01 30.0,",
        "set Var21 = @TdpFnTdbTwbPb 30.0 16.0 101325.0,",
        "set Var22 = @TdpFnWPb 0.01 101325.0,",
        "set Var23 = @HFnTdbW 30.0 0.01,",
        "set Var24 = @HFnTdbRhPb 30.0 0.5 101325.0,",
        "set Var25 = @TdbFnHW 30000.0 0.01,",
        "set Var26 = @RhovFnTdbRh 30.0 0.5,",
        "set Var27 = @RhovFnTdbRhLBnd0C 30.0 0.5,",
        "set Var28 = @RhovFnTdbWPb 30.0 0.01 101325.0,",
        "set Var29 = @RhFnTdbRhov 30.0 0.01,",
        "set Var30 = @RhFnTdbRhovLBnd0C 30.0 0.01,",
        "set Var31 = @RhFnTdbWPb 30.0 0.01 101325.0,",
        "set Var32 = @TwbFnTdbWPb 30.0 0.01 101325.0,",
        "set Var33 = @VFnTdbWPb 30.0 0.01 101325.0,",
        "set Var34 = @WFnTdpPb 16.0 101325.0,",
        "set Var35 = @WFnTdbH 20.0 30000.0,",
        "set Var36 = @WFnTdbTwbPb 30.0 16.0 101325.0,",
        "set Var37 = @WFnTdbRhPb 30.0 0.5 101325.0,", // this function implementation looks suspicious given it takes up to 4 arguments (CalledFrom is
                                                      // 4th)
        "set Var38 = @PsatFnTemp 30.0,",
        "set Var39 = @TsatFnHPb 30000.0 101325.0,",
        "set Var40 = @CpCW 30.0,",
        "set Var41 = @CpHW 60.0,",
        "set Var42 = @RhoH2O 20.0,",
        "set Var43 = @SEVEREWARNEP 0.0,",
        "set Var44 = @WARNEP 0.0,",
        "set Var45 = @TRENDVALUE Variable_Trend1 1,",
        "set Var46 = @TRENDAVERAGE Variable_Trend2 4,",
        "set Var47 = @TRENDMAX Variable_Trend3 4,",
        "set Var48 = @TRENDMIN Variable_Trend4 4,",
        "set Var49 = @TRENDDIRECTION Variable_Trend5 4,",
        "set Var50 = @TRENDSUM Variable_Trend6 4,",
        "set Var51 = @CURVEVALUE 1 0.75,",
        "set absNum = -3.1,",
        "set Var53 = @Abs absNum,", // -3.1 does not work, (-3.1) does not work, "set absNum = -3.1" and use @Abs absNum does work if EMS "set absNum"
                                    // line of code occurs before this line
        "Run Test_Trend_Variables;"

        //			"set Var54 = @TsatFnPb 0.0,", // not public in PsycRoutines so not available to EMS (commented out at line 2397 of
        // RuntimeLanguageProcessor.cc)
        //			"set Var55 = @FATALHALTEP 0.0,", // terminates program, not unit test friendly (but it was also verified here that it
        // halts program)

        "EnergyManagementSystem:GlobalVariable,",
        "  argTrendValue;       !- Erl Variable 1 Name",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend1,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend2,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend3,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend4,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend5,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:TrendVariable,",
        "  Variable_Trend6,     !- Name",
        "  argTrendValue,       !- EMS Variable Name",
        "  4;                   !- Number of Timesteps to be Logged",

        "EnergyManagementSystem:Subroutine,",
        "  Test_Trend_Variables,                             !- Name",
        "  Set TrendVal = @TrendValue Variable_Trend1 1,     !- Program Line 1",
        "  SET argTrendValue = TrendVal,                     !- Program Line 2",
        "  Set TrendVal = @TrendAverage Variable_Trend2 4,   !- Program Line 3",
        "  SET argTrendAvg = TrendVal,                       !- Program Line 4",
        "  Set TrendVal = @TrendMax Variable_Trend3 4,       !- Program Line 5",
        "  SET argTrendMax = TrendVal,                       !- Program Line 6",
        "  Set TrendVal = @TrendMin Variable_Trend4 4,       !- Program Line 7",
        "  SET argTrendMin = TrendVal,                       !- Program Line 8",
        "  Set TrendVal = @TrendDirection Variable_Trend5 4, !- Program Line 9",
        "  SET argTrendDirection = TrendVal,                 !- Program Line 10",
        "  Set TrendVal = @TrendSum Variable_Trend6 4,       !- Program Line 11",
        "  SET argTrendSum = TrendVal;                       !- Program Line 12",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::TimeStepZone = 0.25;

    EMSManager::CheckIfAnyEMS(); // get EMS input
    EMSManager::FinishProcessingUserInput = true;
    bool ErrorsFound(false);
    CurveManager::GetCurveInputData(ErrorsFound); // process curve for use with EMS
    EXPECT_FALSE(ErrorsFound);

    bool anyRan;
    EMSManager::ManageEMS(DataGlobals::emsCallFromHVACIterationLoop, anyRan);
    EXPECT_TRUE(anyRan);

    for (int i = 1; i <= 6; ++i) {
        DataRuntimeLanguage::TrendVariable(i).TrendValARR(1) = 1.1; // initialize history for trend variables
        DataRuntimeLanguage::TrendVariable(i).TrendValARR(2) = 2.2;
        DataRuntimeLanguage::TrendVariable(i).TrendValARR(3) = 3.3;
        DataRuntimeLanguage::TrendVariable(i).TrendValARR(4) = 4.4;
    }

    EMSManager::ManageEMS(DataGlobals::emsCallFromHVACIterationLoop, anyRan); // process trend functions again using above data
    EXPECT_TRUE(anyRan);

    int index(0);
    int offset(25); // first 24 values in ErlExpression() are key words + 1 EMS global variable
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(1).Operator, FuncRound);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(1).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(1).Operand.size(), 1u);
    index = 1 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR1");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 2.0); // round(2.1)

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(2).Operator, FuncMod);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(2).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(2).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(2).Operand(2).Type, 1); // argument was passed to EMS function
    index = 2 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR2");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.0); // mod( 7, 3 )

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(3).Operator, FuncSin);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(3).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(3).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(3).Operand(1).Type, 1); // argument was passed to EMS function
    index = 3 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR3");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.70710678, 0.00000001); // Sin(45) or Sin(0.7854) = 0.707107

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(4).Operator, FuncCos);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(4).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(4).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(4).Operand(1).Type, 1); // argument was passed to EMS function
    index = 4 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR4");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.70710678, 0.00000001); // Cos(45) or Cos(0.7854) = 0.707107

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(5).Operator, FuncArcCos);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(5).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(5).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(5).Operand(1).Type, 1); // argument was passed to EMS function
    index = 5 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR5");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.78539816, 0.00000001); // ArcCos(Cos(45)) = 0.7854 rad

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(6).Operator, FuncArcSin);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(6).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(6).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(6).Operand(1).Type, 1); // argument was passed to EMS function
    index = 6 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR6");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.78539816, 0.00000001); // ArcSin(Sin(45)) = 0.7854 rad

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(7).Operator, FuncDegToRad);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(7).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(7).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(7).Operand(1).Type, 1); // argument was passed to EMS function
    index = 7 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR7");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.78539816, 0.00000001); // DegToRad(45) = 0.7854 rad

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(8).Operator, FuncRadToDeg);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(8).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(8).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(8).Operand(1).Type, 1); // argument was passed to EMS function
    index = 8 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR8");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 45.0, 0.0000001); // RadToDeg(0.7854 rad) = 45 deg

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(9).Operator, FuncExp);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(9).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(9).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(9).Operand(1).Type, 1); // argument was passed to EMS function
    index = 9 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR9");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 2.718281828, 0.00000001); // e^1 = 2.71828

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(10).Operator, FuncLn);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(10).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(10).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(10).Operand(1).Type, 1); // argument was passed to EMS function
    index = 10 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR10");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.69314718, 0.00000001); // e^1 = 0.693147

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(11).Operator, FuncMax);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(11).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(11).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(11).Operand(2).Type, 1); // argument was passed to EMS function
    index = 11 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR11");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.5); // max(0.5, 1.5) = 1.5

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(12).Operator, FuncMin);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(12).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(12).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(12).Operand(2).Type, 1); // argument was passed to EMS function
    index = 12 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR12");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.5); // min(0.5, 1.5) = 0.5

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(13).Operator, FuncABS);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(13).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(13).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(13).Operand(1).Type, 1); // argument was passed to EMS function
    index = 13 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR13");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.3); // abs(1.3) = 1.3

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(14).Operator, FuncRandSeed);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(14).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(14).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(14).Operand(1).Type, 1); // argument was passed to EMS function
    index = 14 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR14");
    // seed may differ by processor, don't test seed generator
    // EXPECT_EQ( DataRuntimeLanguage::ErlVariable( index ).Value.Number, 2.0 ); // @SeedRandom( 2.65 )

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(15).Operator, FuncRandU);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(15).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(15).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(15).Operand(2).Type, 1); // argument was passed to EMS function
    index = 15 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR15");
    // don't test random number generator
    // EXPECT_NEAR( DataRuntimeLanguage::ErlVariable( index ).Value.Number, 0.148876574, 0.00000001 ); // @RANDOMUNIFORM 0.0 1.0

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(16).Operator, FuncRandG);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(16).NumOperands, 4);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(16).Operand.size(), 4u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(16).Operand(4).Type, 1); // argument was passed to EMS function
    index = 16 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR16");
    // don't test random number generator
    // EXPECT_NEAR( DataRuntimeLanguage::ErlVariable( index ).Value.Number, 1.30797328, 0.00000001 ); // @RANDOMNORMAL 1.5 0.5 0.75 2.25 (mean, std,
    // min, max)

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(17).Operator, FuncRhoAirFnPbTdbW);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(17).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(17).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(17).Operand(3).Type, 1); // argument was passed to EMS function
    index = 17 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR17");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.146173145, 0.00000001); // RhoAirFnPbTdbW 101325.0 30.0 0.01 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(18).Operator, FuncCpAirFnWTdb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(18).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(18).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(18).Operand(2).Type, 1); // argument was passed to EMS function
    index = 18 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR18");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1023.42949999999, 0.00000001); // CpAirFnWTdb 0.01 30.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(19).Operator, FuncHfgAirFnWTdb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(19).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(19).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(19).Operand(2).Type, 1); // argument was passed to EMS function
    index = 19 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR19");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 2431308.50000000, 0.00000001); // HfgAirFnWTdb 0.01 30.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(20).Operator, FuncHgAirFnWTdb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(20).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(20).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(20).Operand(2).Type, 1); // argument was passed to EMS function
    index = 20 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR20");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 2556708.50000000, 0.00000001); // HgAirFnWTdb 0.01 30.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(21).Operator, FuncTdpFnTdbTwbPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(21).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(21).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(21).Operand(3).Type, 1); // argument was passed to EMS function
    index = 21 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR21");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 5.573987554, 0.00000001); // TdpFnTdbTwbPb 30.0 16.0 101325 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(22).Operator, FuncTdpFnWPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(22).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(22).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(22).Operand(2).Type, 1); // argument was passed to EMS function
    index = 22 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR22");                            // verified at sugartech site as 14.0439
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 14.044515576, 0.00000001); // TdpFnWPb 0.01 101325 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(23).Operator, FuncHFnTdbW);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(23).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(23).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(23).Operand(2).Type, 1); // argument was passed to EMS function
    index = 23 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR23");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 55712.28500000, 0.00000001); // HFnTdbW 30.0 0.01 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(24).Operator, FuncHFnTdbRhPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(24).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(24).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(24).Operand(3).Type, 1); // argument was passed to EMS function
    index = 24 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR24");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 64177.426349195, 0.00000001); // HFnTdbRhPb 30.0 0.5 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(25).Operator, FuncTdbFnHW);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(25).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(25).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(25).Operand(2).Type, 1); // argument was passed to EMS function
    index = 25 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR25");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 4.876349567, 0.00000001); // TdbFnHW 30000.0 0.01 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(26).Operator, FuncRhovFnTdbRh);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(26).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(26).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(26).Operand(2).Type, 1); // argument was passed to EMS function
    index = 26 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR26");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.015174171, 0.00000001); // RhovFnTdbRh 30.0 0.5 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(27).Operator, FuncRhovFnTdbRhLBnd0C); // fails before #5284, returns FuncRhovFnTdbRh( 41 )
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(27).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(27).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(27).Operand(2).Type, 1); // argument was passed to EMS function
    index = 27 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR27");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.015156240, 0.00000001); // RhovFnTdbRhLBnd0C 30.0 0.5 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(28).Operator, FuncRhovFnTdbWPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(28).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(28).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(28).Operand(2).Type, 1); // argument was passed to EMS function
    index = 28 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name,
              "VAR28"); // http://www.gribble.org/cycling/air_density.html 30 C db, 1013.25 hPa, 16 C dp = 0.011565 g/m3
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.011459487,
                0.00000001); // RhovFnTdbWPb 30.0 0.01 101325.0 = ** this and previous 2 numbers seem very different **

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(29).Operator, FuncRhFnTdbRhov);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(29).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(29).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(29).Operand(2).Type, 1); // argument was passed to EMS function
    index = 29 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR29");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.3295072808, 0.00000001); // RhFnTdbRhov 30.0 0.01 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(30).Operator, FuncRhFnTdbRhovLBnd0C); // fails before #5284, returns int const FuncRhFnTdbRhov( 44 )
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(30).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(30).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(30).Operand(2).Type, 1); // argument was passed to EMS function
    index = 30 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR30");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.3298971165, 0.00000001); // RhFnTdbRhovLBnd0C 30.0 0.01 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(31).Operator, FuncRhFnTdbWPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(31).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(31).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(31).Operand(3).Type, 1); // argument was passed to EMS function
    index = 31 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR31");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.377598442, 0.00000001); // RhFnTdbWPb 30.0 0.01 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(32).Operator, FuncTwbFnTdbWPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(32).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(32).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(32).Operand(3).Type, 1); // argument was passed to EMS function
    index = 32 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR32"); // verified at sugartech site using 30 C db and 0.01 kg/kg = 19.60536624685125 C
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 19.589790661, 0.00000001); // TwbFnTdbWPb 30.0 0.01 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(33).Operator, FuncVFnTdbWPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(33).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(33).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(33).Operand(3).Type, 1); // argument was passed to EMS function
    index = 33 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR33"); // http://www.sugartech.co.za/psychro/ 30 C db, 14.043895 dp = 0.8432375 m3/kg
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.873152783, 0.00000001); // VFnTdbWPb 30.0 0.01 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(34).Operator, FuncWFnTdpPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(34).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(34).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(34).Operand(2).Type, 1); // argument was passed to EMS function
    index = 34 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR34");                            // verified at sugartech site as 0.011366881 kg/kg
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.0113664167, 0.00000001); // WFnTdpPb 16.0 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(35).Operator, FuncWFnTdbH);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(35).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(35).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(35).Operand(2).Type, 1); // argument was passed to EMS function
    index = 35 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name,
              "VAR35"); // http://www.sugartech.co.za/psychro/ 20 C db, 0.42830288 C dp, 30000 H = 0.00389466 kg/kg
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.00390178711, 0.00000001); // WFnTdbH 20.0 30000.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(36).Operator, FuncWFnTdbTwbPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(36).NumOperands, 3);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(36).Operand.size(), 3u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(36).Operand(3).Type, 1); // argument was passed to EMS function
    index = 36 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR36"); // http://www.sugartech.co.za/psychro/ 30 C db, 16 C wb = 0.00559757 kg/kg
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.005624362, 0.00000001); // WFnTdbTwbPb 30.0 16.0 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(37).Operator, FuncWFnTdbRhPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(37).NumOperands, 4); // why is this 4?
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(37).Operand.size(), 4u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(37).Operand(3).Type, 1); // argument was passed to EMS function
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(37).Operand(4).Type, 0); // 4th argument not passed to EMS function
    index = 37 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR37"); // http://www.sugartech.co.za/psychro/ 30 C db, 50% rh = 0.01331149 kg/kg
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.0133109528, 0.00000001); // WFnTdbRhPb 30.0 0.5 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(38).Operator, FuncPsatFnTemp);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(38).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(38).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(38).Operand(1).Type, 1); // argument was passed to EMS function
    index = 38 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name,
              "VAR38"); // http://www.sugartech.co.za/psychro/ 30 C db, 100% rh = 42.46019 mbar = 4246.019 Pa
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 4246.030243592, 0.00000001); // PsatFnTemp 30.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(39).Operator, FuncTsatFnHPb);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(39).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(39).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(39).Operand(2).Type, 1); // argument was passed to EMS function
    index = 39 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR39"); // http://www.sugartech.co.za/psychro/ 10.303 C db gives H = 29999.9999
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 10.318382617, 0.00000001); // TsatFnHPb 30000.0 101325.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(40).Operator, FuncCpCW);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(40).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(40).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(40).Operand(1).Type, 1); // argument was passed to EMS function
    index = 40 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR40");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 4180.0, 0.00000001); // CpCW 30.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(41).Operator, FuncCpHW);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(41).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(41).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(41).Operand(1).Type, 1); // argument was passed to EMS function
    index = 41 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR41");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 4180.0, 0.00000001); // CpHW 60.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(42).Operator, FuncRhoH2O);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(42).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(42).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(42).Operand(1).Type, 1); // argument was passed to EMS function
    index = 42 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR42");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 998.2331862652, 0.00000001); // RhoH2O 60.0 =

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(43).Operator, FuncSevereWarnEp);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(43).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(43).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(43).Operand(1).Type, 1); // argument was passed to EMS function
    index = 43 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR43");

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(44).Operator, FuncWarnEp);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(44).NumOperands, 1);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(44).Operand.size(), 1u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(44).Operand(1).Type, 1); // argument was passed to EMS function
    index = 44 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR44");

    // all trend variables hold 4 values: 1.1, 2.2, 3.3, 4.4
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(45).Operator, FuncTrendValue);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(45).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(45).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(45).Operand(2).Type, 1); // argument was passed to EMS function
    index = 45 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR45");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.1, 0.00000001); // TrendValue Variable_Trend1 1

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(46).Operator, FuncTrendAverage);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(46).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(46).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(46).Operand(2).Type, 1); // argument was passed to EMS function
    index = 46 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR46");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 2.75, 0.00000001); // TrendAverage Variable_Trend2 4

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(47).Operator, FuncTrendMax);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(47).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(47).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(47).Operand(2).Type, 1); // argument was passed to EMS function
    index = 47 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR47");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 4.4, 0.00000001); // TrendMax Variable_Trend3 4

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(48).Operator, FuncTrendMin);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(48).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(48).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(48).Operand(2).Type, 1); // argument was passed to EMS function
    index = 48 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR48");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 1.1, 0.00000001); // TrendMin Variable_Trend4 4

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(49).Operator, FuncTrendDirection);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(49).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(49).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(49).Operand(2).Type, 1); // argument was passed to EMS function
    index = 49 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR49");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, -4.4,
                0.00000001); // TrendDirection Variable_Trend5 4 (-1.1 per 0.25 hrs = -4.4/hr)

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(50).Operator, FuncTrendSum);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(50).NumOperands, 2);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(50).Operand.size(), 2u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(50).Operand(2).Type, 1); // argument was passed to EMS function
    index = 50 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR50");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 11.0, 0.00000001); // TrendSum Variable_Trend6 4

    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operator, FuncCurveValue);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).NumOperands, 6);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand.size(), 6u);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand(2).Type, 1); // argument was passed to EMS function
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand(3).Type, 0); // 3rd argument not passed to EMS function
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand(4).Type, 0); // 4th argument not passed to EMS function
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand(5).Type, 0); // 5th argument not passed to EMS function
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(51).Operand(6).Type, 0); // 6th argument not passed to EMS function
    index = 51 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR51");
    EXPECT_NEAR(DataRuntimeLanguage::ErlVariable(index).Value.Number, 0.95, 0.00000001); // CurveValue 0.75 = 0.95

    // test these functions as needed to verify results

    // test ABS using negative number
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(53).Operator, FuncABS);
    EXPECT_EQ(DataRuntimeLanguage::ErlExpression(53).NumOperands, 1);
    index = 53 + offset;
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Name, "VAR53");
    EXPECT_EQ(DataRuntimeLanguage::ErlVariable(index).Value.Number, 3.1); // set absNum = -3.1, abs(absNum) = 3.1

    //		EXPECT_EQ( DataRuntimeLanguage::ErlExpression( 54 ).Operator, FuncTsatFnPb ); // not public in PsycRoutines so not available to EMS
    //(commented out at line 2397 of RuntimeLanguageProcessor.cc) 		EXPECT_EQ( DataRuntimeLanguage::ErlExpression( 55 ).Operator,
    // FuncFatalHaltEp ); // terminates program, not unit test friendly
}

TEST_F(EnergyPlusFixture, EMSManager_TestOANodeAsActuators)
{
//    EMSActuatorAvailable.allocate(100);
    NumOfNodes = 3;
    numActuatorsUsed = 3;
    Node.allocate(3);
    NodeID.allocate(3);
    EMSActuatorUsed.allocate(3);
    OutAirNodeManager::NumOutsideAirNodes = 3;
    OutAirNodeManager::OutsideAirNodeList.allocate(3);

    NodeID(1) = "Node1";
    NodeID(2) = "Node2";
    NodeID(3) = "Node3";

    Node(1).TempSetPoint = 23.0;
    Node(2).TempSetPoint = 23.0;
    Node(3).TempSetPoint = 23.0;

    Node(1).MassFlowRate = 0.1;
    Node(2).MassFlowRate = 0.1;
    Node(3).MassFlowRate = 0.1;

    OutAirNodeManager::OutsideAirNodeList(1) = 1;
    OutAirNodeManager::OutsideAirNodeList(2) = 2;
    OutAirNodeManager::OutsideAirNodeList(3) = 3;
    EMSActuatorUsed(1).ComponentTypeName = "Outdoor Air System Node";
    EMSActuatorUsed(2).ComponentTypeName = "";
    EMSActuatorUsed(3).ComponentTypeName = "Outdoor Air System Node";
    EMSActuatorUsed(1).UniqueIDName = NodeID(1);
    EMSActuatorUsed(2).UniqueIDName = NodeID(2);
    EMSActuatorUsed(3).UniqueIDName = NodeID(3);

    SetupNodeSetPointsAsActuators();

    EXPECT_TRUE(Node(1).IsLocalNode);
    EXPECT_FALSE(Node(2).IsLocalNode);
    EXPECT_TRUE(Node(3).IsLocalNode);

}
