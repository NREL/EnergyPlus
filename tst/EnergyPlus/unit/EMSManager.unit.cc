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
