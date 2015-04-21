// EnergyPlus::MixedAir Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <MixedAir.hh>
#include <OutAirNodeManager.hh>
#include <InputProcessor.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataLoopNode.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::OutAirNodeManager;
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace ObjexxFCL;

TEST( ProcessOAControllerTest, Test1 )
{
	ShowMessage( "Begin Test: ProcessOAControllerTest, Test1" );

// Test input processing of portion of Controller:OutdoorAir object
	//  Controller:OutdoorAir,
	//    OA Controller 1,         !- Name
	//    Relief Air Outlet Node 1,!- Relief Air Outlet Node Name
	//    VAV Sys 1 Inlet Node,    !- Return Air Node Name
	//    Mixed Air Node 1,        !- Mixed Air Node Name
	//    Outside Air Inlet Node 1,!- Actuator Node Name
	//    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}
	//    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}

	bool ErrorsFound( false ); // If errors detected in input
	int ControllerNum( 0 ); // Controller number
	int NumAlphas ( 17 );
	int NumNumbers ( 7 );
	int NumOfOAControllers( 2 );
	cCurrentModuleObject = "Controller:OutdoorAir";
	OAController.allocate( NumOfOAControllers );

	// Set up OutdoorAir:Node list
	GetOutAirNodesInput();
	OutsideAirNodeList.allocate( 1 );
	OutsideAirNodeList( 1 ) = 2; // Nodes will be registered in the order they appear in the firat controller object, so the OA actuator node will be node 5

	//Set up Node array
	NumOfNodes = 10;
	// GetOnlySingleNode requires that the IDD object definition for NodeList is present, so populate it here
	ListOfObjects.dimension( 1 );
	ListOfObjects( 1 ) = "NodeList";
	iListOfObjects.dimension( 1 );
	iListOfObjects( 1 ) = 1;
	NumObjectDefs = 1;
	ObjectDef.allocate( 1 );
	ObjectDef( 1 ).NumParams = 4;
	ObjectDef( 1 ).NumAlpha = 4;
	ObjectDef( 1 ).NumNumeric = 0;


	// Set up Controller input objects
	lNumericFieldBlanks.allocate ( NumNumbers );
	lAlphaFieldBlanks.allocate( NumAlphas );
	cAlphaFieldNames.allocate( NumAlphas );
	cNumericFieldNames.allocate( NumNumbers );
	cAlphaArgs.allocate( NumAlphas );
	rNumericArgs.allocate( NumNumbers );
	lNumericFieldBlanks = false;
	lAlphaFieldBlanks = false;
	cAlphaFieldNames = " ";
	cNumericFieldNames = " ";
	cAlphaArgs = " ";
	rNumericArgs = 0.0;

	ControllerNum = 1;
	cAlphaArgs( 1 ) = "OA Controller 1"; // Name
	// cAlphaArgs( 2 ) = "Relief Node 1"; // Relief Air Outlet Node Name - not used
	// cAlphaArgs( 3 ) = "Return Node 1"; // Return Air Node Name - not used
	cAlphaArgs( 4 ) = "Mixed Air Node 1"; // Mixed Air Node Name
	cAlphaArgs( 5 ) = "OA Inlet Node 1"; // Actuator Node Name
	rNumericArgs( 1 ) = 0.00; // Minimum Outdoor Air Flow Rate {m3/s}
	rNumericArgs( 2 ) = 0.01; // Maximum Outdoor Air Flow Rate {m3/s}
	cAlphaArgs( 6 ) = "NoEconomizer";
	cAlphaArgs( 7 ) = "ModulateFlow";
	lNumericFieldBlanks( 3 ) = true;
	lNumericFieldBlanks( 4 ) = true;
	lNumericFieldBlanks( 5 ) = true;
	lAlphaFieldBlanks( 8 ) = true;
	lNumericFieldBlanks( 6 ) = true;
	cAlphaArgs( 9 ) = "NoLockout";
	cAlphaArgs( 10 ) = "ProportionalMinimum";
	lAlphaFieldBlanks( 11 ) = true;
	lAlphaFieldBlanks( 12 ) = true;
	lAlphaFieldBlanks( 13 ) = true;
	lAlphaFieldBlanks( 14 ) = true;
	lAlphaFieldBlanks( 15 ) = true;
	lAlphaFieldBlanks( 16 ) = true;
	lAlphaFieldBlanks( 17 ) = true;
	lNumericFieldBlanks( 7 ) = true;

	ErrorsFound = false;
	ProcessOAControllerInputs( cCurrentModuleObject, ControllerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( 2, OAController( 1 ).OANode );
	EXPECT_TRUE( CheckOutAirNodeNumber( OAController( 1 ).OANode ) );

	ControllerNum = 2;
	cAlphaArgs( 1 ) = "OA Controller 2"; // Name
	// cAlphaArgs( 2 ) = "Relief Node 2"; // Relief Air Outlet Node Name - not used
	// cAlphaArgs( 3 ) = "Return Node 2"; // Return Air Node Name - not used
	cAlphaArgs( 4 ) = "Mixed Air Node 2"; // Mixed Air Node Name
	cAlphaArgs( 5 ) = "OA Inlet Node 2"; // Actuator Node Name
	rNumericArgs( 1 ) = 0.00; // Minimum Outdoor Air Flow Rate {m3/s}
	rNumericArgs( 2 ) = 0.01; // Maximum Outdoor Air Flow Rate {m3/s}
	cAlphaArgs( 6 ) = "NoEconomizer";
	cAlphaArgs( 7 ) = "ModulateFlow";
	lNumericFieldBlanks( 3 ) = true;
	lNumericFieldBlanks( 4 ) = true;
	lNumericFieldBlanks( 5 ) = true;
	lAlphaFieldBlanks( 8 ) = true;
	lNumericFieldBlanks( 6 ) = true;
	cAlphaArgs( 9 ) = "NoLockout";
	cAlphaArgs( 10 ) = "ProportionalMinimum";
	lAlphaFieldBlanks( 11 ) = true;
	lAlphaFieldBlanks( 12 ) = true;
	lAlphaFieldBlanks( 13 ) = true;
	lAlphaFieldBlanks( 14 ) = true;
	lAlphaFieldBlanks( 15 ) = true;
	lAlphaFieldBlanks( 16 ) = true;
	lAlphaFieldBlanks( 17 ) = true;
	lNumericFieldBlanks( 7 ) = true;

	ErrorsFound = false;
	ProcessOAControllerInputs( cCurrentModuleObject, ControllerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( 4, OAController( 2 ).OANode );
	EXPECT_FALSE( CheckOutAirNodeNumber( OAController( 2 ).OANode ) );

	// Clean up
	OAController.deallocate();
	OutsideAirNodeList.deallocate();
	ObjectDef.deallocate();
	lNumericFieldBlanks.deallocate();
	lAlphaFieldBlanks.deallocate();
	cAlphaFieldNames.deallocate();
	cNumericFieldNames.deallocate();
	cAlphaArgs.deallocate();
	rNumericArgs.deallocate();
}
