// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataGlobals.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;
using namespace ObjexxFCL;

TEST( ProcessZoneDataTest, Test1 )
{
// Test input processing of Zone object
//	Zone,
//		ZONE ONE, !- Name
//		0, !- Direction of Relative North{ deg }
//		0, 0, 0, !- X, Y, Z{ m }
//		1, !- Type
//		1, !- Multiplier
//		autocalculate, !- Ceiling Height{ m }
//		autocalculate, !- Volume{ m3 }
//		, !- Floor Area{ m2 }
//		AdaptiveConvectionAlgorithm;  !- Zone Inside Convection Algorithm

	static bool ErrorsFound( false ); // If errors detected in input
	static int ZoneNum( 0 ); // Zone number
	int NumAlphas ( 2 );
	int NumNumbers ( 9 );

	cCurrentModuleObject = "Zone";
	NumOfZones = 2;
	Zone.allocate( NumOfZones );

	// Set up a Zone object
	NumAlphas = 2;
	NumNumbers = 9;
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

	ZoneNum = 1;
	cAlphaArgs( 1 ) = "Zone One"; // Name
	rNumericArgs( 1 ) = 0.0; // Direction of Relative North[deg]
	rNumericArgs( 2 ) = 0.0; // X [m]
	rNumericArgs( 3 ) = 0.0; // Y [m]
	rNumericArgs( 4 ) = 0.0; // Z [m]
	rNumericArgs( 5 ) = 0.0; // Type
	rNumericArgs( 6 ) = 0.0; // Multiplier
	lNumericFieldBlanks( 7 ) = true; // Ceiling Height{ m }
	lNumericFieldBlanks( 8 ) = true; // Volume{ m3 }
	lNumericFieldBlanks( 9 ) = true; // Floor Area{ m2 }
	cAlphaArgs( 2 ) = "ADAPTIVECONVECTIONALGORITHM"; // Zone Inside Convection Algorithm - Must be UPPERCASE by this point

	ErrorsFound = false;
	ProcessZoneData( cCurrentModuleObject, ZoneNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );

	ZoneNum = 2;
	cAlphaArgs( 1 ) = "Zone Two"; // Name
	cAlphaArgs( 2 ) = "InvalidChoice"; // Zone Inside Convection Algorithm - Must be UPPERCASE by this point
	ErrorsFound = false;
	ProcessZoneData( cCurrentModuleObject, ZoneNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );
	EXPECT_TRUE( ErrorsFound );

	ZoneNum = 2;
	cAlphaArgs( 1 ) = "Zone Two"; // Name
	cAlphaArgs( 2 ) = "TARP"; // Zone Inside Convection Algorithm - Must be UPPERCASE by this point
	ErrorsFound = false;
	ProcessZoneData( cCurrentModuleObject, ZoneNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( "Zone One", Zone( 1 ).Name );
	EXPECT_EQ( AdaptiveConvectionAlgorithm, Zone( 1 ).InsideConvectionAlgo );
	EXPECT_EQ( "Zone Two", Zone( 2 ).Name );
	EXPECT_EQ( ASHRAETARP, Zone( 2 ).InsideConvectionAlgo );

}
