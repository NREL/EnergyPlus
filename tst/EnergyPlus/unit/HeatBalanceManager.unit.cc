// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/HeatBalanceManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeatBalanceManager_ProcessZoneData )
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

	}

	TEST_F( EnergyPlusFixture, HeatBalanceManager_GetWindowConstructData )
	{
		// Test get input for window construction object
		// Construction,
		//	 WINDOWWBLIND, !- Name
		//	 GLASS,        !- Outside Layer
		//	 AIRGAP,       !- Layer 2
		//	 GLASS;        !- Layer 3

		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"Construction,",
			" WINDOWWBLIND, !- Name",
			" GLASS,        !- Outside Layer",
			" AIRGAP,       !- Layer 2",
			" GLASS;        !- Layer 3",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input

		TotMaterials = 3;
		Material.allocate( TotMaterials );
		Material( 1 ).Name = "GLASS";
		Material( 2 ).Name = "AIRGAP";
		Material( 3 ).Name = "GLASS";

		// Material layer group index
		Material( 1 ).Group = 3; // WindowGlass
		Material( 2 ).Group = 4; // WindowGas
		Material( 3 ).Group = 3; // WindowGlass

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

	}

}
