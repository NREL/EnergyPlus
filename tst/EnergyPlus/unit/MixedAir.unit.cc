// EnergyPlus::MixedAir Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/MixedAir.hh>

#include "Fixtures/HVACFixture.hh"

using namespace EnergyPlus::MixedAir;

namespace EnergyPlus {

	TEST_F( HVACFixture, MixedAir_ProcessOAControllerTest )
	{
		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"  OutdoorAir:Node,",
			"    Outside Air Inlet Node 1; !- Name",
			"  Controller:OutdoorAir,",
			"    OA Controller 1,         !- Name",
			"    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
			"    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 1,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 1, !- Actuator Node Name",
			"    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    NoEconomizer,            !- Economizer Control Type",
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type",
			"    ProportionalMinimum;     !- Minimum Limit Type",
			"  Controller:OutdoorAir,",
			"    OA Controller 2,         !- Name",
			"    Relief Air Outlet Node 2, !- Relief Air Outlet Node Name",
			"    VAV Sys 2 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 2,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 2, !- Actuator Node Name",
			"    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    NoEconomizer,            !- Economizer Control Type",
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type",
			"    ProportionalMinimum;     !- Minimum Limit Type",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input
		int ControllerNum( 0 ); // Controller number
		int NumArg( 0 );
		int NumNums( 0 );
		int NumAlphas( 0 );
		int IOStat( 0 );
		std::string const CurrentModuleObject = CurrentModuleObjects( CMO_OAController );

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObjects( CMO_OAController ), NumArg, NumAlphas, NumNums );

		Array1D< Real64 > NumArray( NumNums, 0.0 );
		Array1D_string AlphArray( NumAlphas );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNums );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_bool lNumericBlanks( NumNums, true );

		NumOAControllers = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		OAController.allocate( NumOAControllers );

		ControllerNum = 1;

		InputProcessor::GetObjectItem( CurrentModuleObject, ControllerNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		ProcessOAControllerInputs( CurrentModuleObject, ControllerNum, AlphArray, NumAlphas, NumArray, NumNums, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

		EXPECT_FALSE( ErrorsFound );
		EXPECT_EQ( 2, OAController( 1 ).OANode );
		EXPECT_TRUE( OutAirNodeManager::CheckOutAirNodeNumber( OAController( 1 ).OANode ) );

		ControllerNum = 2;
		InputProcessor::GetObjectItem( CurrentModuleObject, ControllerNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		ErrorsFound = false;
		ProcessOAControllerInputs( CurrentModuleObject, ControllerNum, AlphArray, NumAlphas, NumArray, NumNums, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		EXPECT_EQ( 6, OAController( 2 ).OANode );
		EXPECT_FALSE( OutAirNodeManager::CheckOutAirNodeNumber( OAController( 2 ).OANode ) );

	}

}
