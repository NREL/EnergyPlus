// EnergyPlus::NodeInputManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <HeatBalanceManager.hh>
#include <NodeInputManager.hh>
#include <DataLoopNode.hh>
#include <OutAirNodeManager.hh>
#include <EMSManager.hh>
#include <DataEnvironment.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::NodeInputManager;
using namespace EnergyPlus::DataLoopNode;
using namespace ObjexxFCL;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, NodeMoreInfoEMSsensorCheck1 ) {
	
		std::string const idf_objects = delimited_string( { 
			"Version,8.5;",

			"OutdoorAir:Node, Test node;",

			"EnergyManagementSystem:Sensor,",
			"test_node_wb,",
			"Test Node, ",
			"System Node Wetbulb Temperature;",

			"EnergyManagementSystem:Sensor,",
			"test_node_rh,",
			"Test Node, ",
			"System Node Relative Humidity;",

			"EnergyManagementSystem:Sensor,",
			"test_node_dp,",
			"Test Node, ",
			"System Node Dewpoint Temperature;",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		OutAirNodeManager::SetOutAirNodes();

		NodeInputManager::SetupNodeVarsForReporting();

		EMSManager::CheckIfAnyEMS();

		EMSManager::FinishProcessingUserInput = true;

		EMSManager::ManageEMS( DataGlobals::emsCallFromSetupSimulation );

		DataLoopNode::Node( 1 ).Temp = 20.0;
		DataLoopNode::Node( 1 ).HumRat = 0.01;
		DataEnvironment::OutBaroPress = 100000;

		NodeInputManager::CalcMoreNodeInfo();

		EXPECT_NEAR( DataLoopNode::MoreNodeInfo( 1 ).RelHumidity, 67.65, 0.01 );
		EXPECT_NEAR( DataLoopNode::MoreNodeInfo( 1 ).AirDewPointTemp, 13.84, 0.01 );
		EXPECT_NEAR( DataLoopNode::MoreNodeInfo( 1 ).WetBulbTemp, 16.11, 0.01 );

	}


TEST( CheckUniqueNodesTest, Test1 )
{
	bool UniqueNodeError( false ); 

	InitUniqueNodeCheck( "Context" );
	// set up initial list using names
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "TestInputNode1", _ , "ObjectName" );
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "TestOutputNode1", _ , "ObjectName" );
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "TestInputNode2", _ , "ObjectName" );
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "TestOutputNode2", _ , "ObjectName" );

	// now to test if a new node is in the list - should not be an error and should be false
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "NonUsedNode", _ , "ObjectName");
	EXPECT_FALSE( UniqueNodeError );

	//try one that is already in the list - should be an error and show up as true
	CheckUniqueNodes( "NodeFieldName", "NodeName", UniqueNodeError, "TestInputNode2", _ , "ObjectName" );
	EXPECT_TRUE( UniqueNodeError );

	EndUniqueNodeCheck( "Context" );

}

}
