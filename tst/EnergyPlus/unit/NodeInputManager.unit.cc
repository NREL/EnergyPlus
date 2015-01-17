// EnergyPlus::NodeInputManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <HeatBalanceManager.hh>
#include <NodeInputManager.hh>
#include <DataLoopNode.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::NodeInputManager;
using namespace EnergyPlus::DataLoopNode;
using namespace ObjexxFCL;

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
