// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;
using namespace ObjexxFCL;

TEST( HeatBalanceManagerTest, ProcessZoneData )
{
	ShowMessage( "Begin Test: HeatBalanceManagerTest, ProcessZoneData" );

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

	bool ErrorsFound( false ); // If errors detected in input
	int ZoneNum( 0 ); // Zone number
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

	// Clean up
	Zone.deallocate();
	lNumericFieldBlanks.deallocate();
	lAlphaFieldBlanks.deallocate();
	cAlphaFieldNames.deallocate();
	cNumericFieldNames.deallocate();
	cAlphaArgs.deallocate();
	rNumericArgs.deallocate();

}
TEST( HeatBalanceManagerTest, GetWindowConstructData )
{
	ShowMessage( "Begin Test: HeatBalanceManagerTest, GetWindowConstructData" );

	// Test get input for window construction object
	// Construction,
	//	 WINDOWWBLIND, !- Name
	//	 GLASS,        !- Outside Layer
	//	 AIRGAP,       !- Layer 2
	//	 GLASS;        !- Layer 3

	bool ErrorsFound( false ); // If errors detected in input
	int ZoneNum( 0 ); // Zone number
	int NumAlphas( 4 );
	int NumNumbers( 0 );

	ZoneNum = 0;
	NumAlphas = 4;
	NumNumbers = 0;

	//TotConstructs = 1;
	//Construct.allocate( TotConstructs );
	//Construct( 1 ).Name = "WINDOWWBLIND";

	TotMaterials = 3;
	Material.allocate( TotMaterials );
	Material( 1 ).Name = "GLASS";
	Material( 2 ).Name = "AIRGAP";
	Material( 3 ).Name = "GLASS";

	// Material layer group index
	Material( 1 ).Group = 3; // WindowGlass
	Material( 2 ).Group = 4; // WindowGas
	Material( 3 ).Group = 3; // WindowGlass

	// GetNumObjectsFound function requires the IDF object for Construction
	NumObjectDefs = 1;
	ListOfObjects.allocate( NumObjectDefs );
	ListOfObjects( NumObjectDefs ) = "CONSTRUCTION";
	iListOfObjects.allocate( NumObjectDefs );
	iListOfObjects( NumObjectDefs ) = 1;
	ObjectStartRecord.allocate( NumObjectDefs );
	ObjectStartRecord( NumObjectDefs ) = 1;
	ObjectGotCount.allocate( NumObjectDefs );
	IDFRecordsGotten.allocate( NumObjectDefs );
	IDFRecordsGotten( NumObjectDefs ) = false;
	ObjectDef.allocate( NumObjectDefs );
	ObjectDef( 1 ).NumFound = 1;
	ObjectDef( 1 ).NumParams = 4;
	ObjectDef( 1 ).NumAlpha = 4;
	ObjectDef( 1 ).NumNumeric = 0;
	ObjectDef( 1 ).AlphFieldChks.allocate( NumAlphas );
	ObjectDef( 1 ).AlphFieldChks = " ";
	ObjectDef( 1 ).NumRangeChks.allocate( NumNumbers );

	NumIDFRecords = 1;
	IDFRecords.allocate( NumIDFRecords );
	IDFRecords( 1 ).Name = ListOfObjects( 1 );
	IDFRecords( 1 ).NumNumbers = NumNumbers;
	IDFRecords( 1 ).NumAlphas = NumAlphas;
	IDFRecords( 1 ).ObjectDefPtr = ObjectDef( 1 ).NumFound;
	IDFRecords( 1 ).Alphas.allocate( NumAlphas );
	IDFRecords( 1 ).Alphas( 1 ) = "WINDOWWBLIND";
	IDFRecords( 1 ).Alphas( 2 ) = Material( 1 ).Name;
	IDFRecords( 1 ).Alphas( 3 ) = Material( 2 ).Name;
	IDFRecords( 1 ).Alphas( 4 ) = Material( 3 ).Name;
	IDFRecords( 1 ).AlphBlank.allocate( NumAlphas );
	IDFRecords( 1 ).AlphBlank( 1 ) = false;
	IDFRecords( 1 ).AlphBlank( 2 ) = false;
	IDFRecords( 1 ).AlphBlank( 3 ) = false;
	IDFRecords( 1 ).AlphBlank( 4 ) = false;
	IDFRecords( 1 ).Numbers.allocate( NumNumbers );
	IDFRecords( 1 ).Numbers = 0;
	IDFRecords( 1 ).NumBlank.allocate( NumNumbers );
	IDFRecords( 1 ).NumBlank = false;

	MaxAlphaArgsFound = NumAlphas;
	MaxNumericArgsFound = NumNumbers;

	// Set up construction input objects
	lNumericFieldBlanks.allocate( NumNumbers );
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

	NominalRforNominalUCalculation.allocate( 1 );
	NominalRforNominalUCalculation( 1 ) = 0.0;
	NominalR.allocate( TotMaterials );
	NominalR( 1 ) = 0.4; // Set these explicity for each material layer to avoid random failures of check for NominalRforNominalUCalculation == 0.0 at end of GetConstructData
	NominalR( 2 ) = 0.4;
	NominalR( 3 ) = 0.4;

	// call to get valid window material types
	ErrorsFound = false;
	GetConstructData( ErrorsFound ); // returns ErrorsFound as false since all layers are valid
	EXPECT_FALSE( ErrorsFound );

	// Clear shared arrays that were allocated in GetConstructData
	Construct.deallocate();

	// call to get invalid window material type
	Material( 2 ).Group = 16; // BlindEquivalentLayer, this layer is invalid in plain windows
	ErrorsFound = false;
	GetConstructData( ErrorsFound ); // returns ErrorsFound as true since layer 2 is invalid
	EXPECT_TRUE( ErrorsFound );

	// dealocate variables
	Construct.deallocate();
	Material.deallocate();
	iListOfObjects.deallocate();
	ObjectStartRecord.deallocate();
	ObjectGotCount.deallocate();
	IDFRecordsGotten.deallocate();
	ObjectDef( 1 ).AlphFieldChks.deallocate();
	ObjectDef( 1 ).NumRangeChks.deallocate();
	ObjectDef.deallocate();
	IDFRecords( 1 ).Alphas.deallocate();
	IDFRecords( 1 ).AlphBlank.deallocate();
	IDFRecords( 1 ).Numbers.deallocate();
	IDFRecords( 1 ).NumBlank.deallocate();
	IDFRecords.deallocate();
	lNumericFieldBlanks.deallocate();
	lAlphaFieldBlanks.deallocate();
	cAlphaFieldNames.deallocate();
	cNumericFieldNames.deallocate();
	cAlphaArgs.deallocate();
	rNumericArgs.deallocate();
	NominalRforNominalUCalculation.deallocate();
	NominalR.deallocate();
}
