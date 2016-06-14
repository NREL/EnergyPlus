// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::EMSManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EMSManager.hh>
#include <DataRuntimeLanguage.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantUtilities.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EMSManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataRuntimeLanguage;
using namespace EnergyPlus::PlantUtilities;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, EMSManager_TestForUniqueEMSActuators )
{
	EMSActuatorAvailable.allocate(100);

	std::string componentTypeName1( "Chiller1" );
	std::string componentTypeName2( "Chiller2" );
	std::string uniqueIDName1( "Plant Component Chiller:Electric:ReformulatedEIR" );
	std::string controlTypeName1( "On/Off Supervisory" );
	std::string units1( "None" );
	bool EMSActuated1( true );
	bool testBoolean1( true );
	bool testBoolean2( true );
	bool testBoolean3( true );

	//calling three times but twice with same names should still result in only two item in the resulting list
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean3 );
	EXPECT_EQ( 2, numEMSActuatorsAvailable );


	// repeat with integers
	std::string controlTypeName2( "ModeOfSomething" );
	int testInt1( 7 );
	int testInt2( 9 );
	int testInt3( 11 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt3 );
	EXPECT_EQ( 4, numEMSActuatorsAvailable );

	// repeat with reals
	std::string controlTypeName3( "ValueOfResults" );
	Real64 testReal1( 0.123 );
	Real64 testReal2( 0.456 );
	Real64 testReal3( 0.789 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal3 );
	EXPECT_EQ( 6, numEMSActuatorsAvailable );

	EMSActuatorAvailable.deallocate();

}

TEST_F( EnergyPlusFixture, Dual_NodeTempSetpoints ) {

		std::string const idf_objects = delimited_string( {
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

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		OutAirNodeManager::SetOutAirNodes();

		EMSManager::CheckIfAnyEMS();

		EMSManager::FinishProcessingUserInput = true;

		bool anyRan;
		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );

		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );


		EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointHi, 20.0, 0.000001 );

		EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointLo, 16.0, 0.000001 );

}

TEST_F( EnergyPlusFixture, SupervisoryControl_PlantComponent_SetActuatedBranchFlowRate ) {

		// test EMS actuator for Plant Component
		// test SetActuatedBranchFlowRate for expected response

		std::string const idf_objects = delimited_string( { 
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

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		// sets number of EMS objects
		EMSManager::CheckIfAnyEMS();

		// allows NodeSetpoint and AvailabilityManagers actuators to be setup
		EMSManager::FinishProcessingUserInput = true;

		// set up plant loop
		DataPlant::TotNumLoops = 1;
		PlantLoop.allocate( 1 );
		PlantLoop( 1 ).Name = "MyPlant";
		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}
		// create 2 components on a single branch to simulate water flow control for entire branch
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 2 );
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = 41; // Coil:Heating:Water
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = "Zone1FanCoilHeatingCoil";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 1;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).TypeOf_Num = 21; // Pipe:Adiabatic
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).Name = "Pipe";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).NodeNumIn = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).NodeNumOut = 3;
		PlantCondLoopOperation::SetupPlantEMSActuators();

		// set flow, max and maxavail on the nodes
		Node.allocate( 3 );
		Real64 NodeMdot( 1.5 );
		Node( 1 ).MassFlowRate = NodeMdot;
		Node( 1 ).MassFlowRateMax = NodeMdot;
		Node( 1 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 1 ).MassFlowRateRequest = NodeMdot;
		Node( 2 ).MassFlowRate = NodeMdot;
		Node( 2 ).MassFlowRateMax = NodeMdot;
		Node( 2 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 2 ).MassFlowRateRequest = NodeMdot;
		Node( 3 ).MassFlowRate = NodeMdot;
		Node( 3 ).MassFlowRateMax = NodeMdot;
		Node( 3 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 3 ).MassFlowRateRequest = NodeMdot;

		bool anyRan;
		// set up EMS
		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );

		//set dummy EMS value
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue = 1.0;

		// dummy value set above should be zero'd on this call since EMS 0's values on begin environment (whether EMS program runs on this call or not)
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );

		EXPECT_FALSE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 0.0, 0.000001 );

		// expect node data to represent full flow
		// SetActuatedBranchFlowRate( CompFlow, ActuatedNode, LoopNum, LoopSideNum, BranchNum, ResetMode )
		SetActuatedBranchFlowRate( NodeMdot, 1, 1, 1, 1, false );
		EXPECT_EQ(Node( 1 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		SetActuatedBranchFlowRate( NodeMdot, 2, 1, 1, 1, false );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, NodeMdot );
	
		//set dummy EMS value
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue = 1.0;

		// dummy value set above should remain on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, anyRan );

		EXPECT_FALSE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 1.0, 0.000001 );
		SetActuatedBranchFlowRate( NodeMdot, 1, 1, 1, 1, false );
		EXPECT_EQ(Node( 1 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		SetActuatedBranchFlowRate( NodeMdot, 2, 1, 1, 1, false );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, NodeMdot );

		// dummy value set above should reset to 0 on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
		// override flag should also be true
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan );

		EXPECT_TRUE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 0.0, 0.000001 );

		// expect node data to represent no flow. Request is also 0's in this function. Max and MaxAvail are not changed
		SetActuatedBranchFlowRate( NodeMdot, 1, 1, 1, 1, false );
		EXPECT_EQ(Node( 1 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, 0.0 );
		EXPECT_EQ(Node( 2 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, 0.0 );
		SetActuatedBranchFlowRate( NodeMdot, 2, 1, 1, 1, false );
		EXPECT_EQ(Node( 2 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, 0.0 );
		EXPECT_EQ(Node( 3 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, 0.0 );

}

TEST_F( EnergyPlusFixture, SupervisoryControl_PlantComponent_SetComponentFlowRate ) {

		// test EMS actuator for Plant Component
		// test SetComponentFlowRate for expected response

		std::string const idf_objects = delimited_string( { 
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

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		// sets number of EMS objects
		EMSManager::CheckIfAnyEMS();

		// allows NodeSetpoint and AvailabilityManagers actuators to be setup
		EMSManager::FinishProcessingUserInput = true;

		// set up plant loop
		DataPlant::TotNumLoops = 1;
		PlantLoop.allocate( 1 );
		PlantLoop( 1 ).Name = "MyPlant";
		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}
		// create 2 components on a single branch to simulate water flow control for entire branch
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 2 );
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = 41; // Coil:Heating:Water
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = "Zone1FanCoilHeatingCoil";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 1;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).TypeOf_Num = 21; // Pipe:Adiabatic
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).Name = "Pipe";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).NodeNumIn = 2;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 2 ).NodeNumOut = 3;
		PlantCondLoopOperation::SetupPlantEMSActuators();

		// set flow, max and maxavail on the nodes
		Node.allocate( 3 );
		Real64 NodeMdot( 1.5 );
		Node( 1 ).MassFlowRate = NodeMdot;
		Node( 1 ).MassFlowRateMax = NodeMdot;
		Node( 1 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 1 ).MassFlowRateRequest = NodeMdot;
		Node( 2 ).MassFlowRate = NodeMdot;
		Node( 2 ).MassFlowRateMax = NodeMdot;
		Node( 2 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 2 ).MassFlowRateRequest = NodeMdot;
		Node( 3 ).MassFlowRate = NodeMdot;
		Node( 3 ).MassFlowRateMax = NodeMdot;
		Node( 3 ).MassFlowRateMaxAvail = NodeMdot;
		Node( 3 ).MassFlowRateRequest = NodeMdot;

		bool anyRan;
		// set up EMS
		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );
		//set dummy EMS value
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue = 1.0;

		// dummy value set above should be zero'd on this call since EMS 0's values on begin environment (whether EMS program runs on this call or not)
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );

		EXPECT_FALSE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 0.0, 0.000001 );

		// expect node data to represent full flow
		// SetComponentFlowRate( CompFlow, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex )
		SetComponentFlowRate( NodeMdot, 1, 2, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 1 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		SetComponentFlowRate( NodeMdot, 2, 3, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, NodeMdot );
	
		//set dummy EMS value
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue = 1.0;

		// dummy value set above should remain on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, anyRan );

		EXPECT_FALSE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 1.0, 0.000001 );

		// expect node data to represent full flow
		SetComponentFlowRate( NodeMdot, 1, 2, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 1 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		SetComponentFlowRate( NodeMdot, 2, 3, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 2 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRate, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, NodeMdot );

		// dummy value set above should reset to 0 on this call since EMS calling manager uses BeginTimestepBeforePredictor as the calling point
		// override flag should also be true
		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan );

		EXPECT_TRUE(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideOn );
		EXPECT_NEAR(PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).EMSLoadOverrideValue, 0.0, 0.000001 );
		Real64 tempNodeMdot( NodeMdot );

		// expect node data to represent no flow. Max, MaxAvail, and Request are not changed
		SetComponentFlowRate( tempNodeMdot, 1, 2, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 1 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 1 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 1 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		tempNodeMdot = NodeMdot;
		SetComponentFlowRate( tempNodeMdot, 2, 3, 1, 1, 1, 1 );
		EXPECT_EQ(Node( 2 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 2 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 2 ).MassFlowRateRequest, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRate, 0.0 );
		EXPECT_EQ(Node( 3 ).MassFlowRateMax, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateMaxAvail, NodeMdot );
		EXPECT_EQ(Node( 3 ).MassFlowRateRequest, NodeMdot );

}

TEST_F( EnergyPlusFixture, Test_EMSLogic ) {
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Nov 2015
	// Tests to evaluate robustness of EMS programming language math expressions
	// specific issue was use of "-" just after operand, e.g., IF MyVar == -X,

	std::string const idf_objects = delimited_string( {
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

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutAirNodeManager::SetOutAirNodes();

	EMSManager::CheckIfAnyEMS();
	EMSManager::FinishProcessingUserInput = true;
	bool anyRan;
	EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );
	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );


	EXPECT_NEAR( DataLoopNode::Node( 1 ).TempSetPoint, 11.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 2 ).TempSetPoint, 12.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 3 ).TempSetPoint, 13.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 4 ).TempSetPoint, 14.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 5 ).TempSetPoint, 15.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 6 ).TempSetPoint, 16.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 7 ).TempSetPoint, 17.0, 0.0000001 );

	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginTimestepBeforePredictor, anyRan );


	EXPECT_NEAR( DataLoopNode::Node( 1 ).TempSetPoint, 21.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 2 ).TempSetPoint, 22.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 3 ).TempSetPoint, 23.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 4 ).TempSetPoint, 24.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 5 ).TempSetPoint, 25.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 6 ).TempSetPoint, 26.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 7 ).TempSetPoint, 27.0, 0.0000001 );

}

TEST_F( EnergyPlusFixture, Debug_EMSLogic ) {
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Nov 2015
	// Tests to evaluate robustness of EMS programming language math expressions
	// specific issue was use of "-" just after operand, e.g., Set MyVar == -X,
	// This unit test is purposely singular to enable easy debugging
	// If a problem is suspected, change this unit test accordingly and test
	// Much easier to single step this unit test than a more complex version

	std::string const idf_objects = delimited_string( {
		"Version,8.4;",

		"OutdoorAir:Node, Test node 1;",

		"EnergyManagementSystem:Actuator,",
		"TempSetpoint1,          !- Name",
		"Test node 1,  !- Actuated Component Unique Name",
		"System Node Setpoint,    !- Actuated Component Type",
		"Temperature Setpoint;    !- Actuated Component Control Type",

		"EnergyManagementSystem:ProgramCallingManager,",
		"Logic Manager 1,  !- Name",
		"BeginNewEnvironment,  !- EnergyPlus Model Calling Point",
		"LogicTest1;  !- Program Name 1",

		"EnergyManagementSystem:Program,",
		"LogicTest1,",
		"Set MyVar1 = ( -2 ),",
		"Set MyVar2 = ( -2 ),",
		"Set TempSetpoint1 = MyVar1 / MyVar2;",

//		"IF MyVar1 == 8,",
//		"  Set TempSetpoint1 = 11.0,",
//		"ELSE,",
//		"  Set TempSetpoint1 = 21.0,",
//		"ENDIF;",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutAirNodeManager::SetOutAirNodes();

	EMSManager::CheckIfAnyEMS();
	EMSManager::FinishProcessingUserInput = true;
	bool anyRan;
	EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );
	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );


	EXPECT_NEAR( DataLoopNode::Node( 1 ).TempSetPoint, 1.0, 0.0000001 );

}


TEST_F( EnergyPlusFixture, TestAnyRanArgument ) {
		// small test to demonstrate new boolean argument.
		// shows a simple way to setup sensor on a node, need to call SetupNodeVarsForReporting()

		std::string const idf_objects = delimited_string( {
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

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		OutAirNodeManager::SetOutAirNodes();
		NodeInputManager::SetupNodeVarsForReporting();
		EMSManager::CheckIfAnyEMS();

		EMSManager::FinishProcessingUserInput = true;

		bool anyRan;
		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation, anyRan );
		EXPECT_FALSE( anyRan );

		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment, anyRan );
		EXPECT_FALSE( anyRan );

		EMSManager::ManageEMS( DataGlobals::emsCallFromHVACIterationLoop, anyRan );
		EXPECT_TRUE( anyRan );

}


TEST_F( EnergyPlusFixture, EMSManager_CheckIfAnyEMS_OutEMS ) {

	using DataGlobals::AnyEnergyManagementSystemInModel;

	std::string const idf_objects = delimited_string( {
    	"  Output:EnergyManagementSystem,                                                                ",
		"    Verbose,                 !- Actuator Availability Dictionary Reporting                      ",
		"    Verbose,                 !- Internal Variable Availability Dictionary Reporting             ",
		"    Verbose;                 !- EMS Runtime Language Debug Output Level                         ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	CheckIfAnyEMS();
	EXPECT_TRUE( AnyEnergyManagementSystemInModel );


}




