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


using namespace EnergyPlus;
using namespace EnergyPlus::EMSManager;
using namespace EnergyPlus::DataRuntimeLanguage;
using namespace ObjexxFCL;

TEST( EMSManager, TestForUniqueEMSActuators )
{

	ShowMessage( "Begin Test: EMSManager, TestForUniqueEMSActuators" );
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
		"DualSetpiontTestControl;  !- Program Name 1",

		"EnergyManagementSystem:Program,",
		"DualSetpiontTestControl,",
		"Set TempSetpointLo = 16.0,",
		"Set TempSetpointHi  = 20.0;",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		OutAirNodeManager::SetOutAirNodes();

		EMSManager::CheckIfAnyEMS();

		EMSManager::FinishProcessingUserInput = true;

		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation );

		EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment );


		EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointHi, 20.0, 0.000001 );

		EXPECT_NEAR(DataLoopNode::Node(1).TempSetPointLo, 16.0, 0.000001 );

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
	EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation );
	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment );


	EXPECT_NEAR( DataLoopNode::Node( 1 ).TempSetPoint, 11.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 2 ).TempSetPoint, 12.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 3 ).TempSetPoint, 13.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 4 ).TempSetPoint, 14.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 5 ).TempSetPoint, 15.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 6 ).TempSetPoint, 16.0, 0.0000001 );
	EXPECT_NEAR( DataLoopNode::Node( 7 ).TempSetPoint, 17.0, 0.0000001 );

	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginTimestepBeforePredictor );


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
	EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation );
	EMSManager::ManageEMS( DataGlobals::emsCallFromBeginNewEvironment );


	EXPECT_NEAR( DataLoopNode::Node( 1 ).TempSetPoint, 1.0, 0.0000001 );

}
