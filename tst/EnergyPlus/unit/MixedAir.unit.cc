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

// EnergyPlus::MixedAir Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/OutAirNodeManager.hh>

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::SizingManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, MixedAir_ProcessOAControllerTest )
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

	TEST_F( EnergyPlusFixture, MixedAir_HXBypassOptionTest )
	{
		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"  OutdoorAir:Node,",
			"    Outside Air Inlet Node 1; !- Name",
			"  Controller:OutdoorAir,",
			"    OA Controller 1,         !- Name",
			"    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
			"    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 1,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 1, !- Actuator Node Name",
			"    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type", // No lockout
			"    ProportionalMinimum,     !- Minimum Limit Type",
			"    ,                        !- Minimum Outdoor Air Schedule Name",
			"    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Mechanical Ventilation Controller Name",
			"    ,                        !- Time of Day Economizer Control Schedule Name",
			"    No,                      !- High Humidity Control",
			"    ,                        !- Humidistat Control Zone Name",
			"    ,                        !- High Humidity Outdoor Air Flow Ratio",
			"    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
			"    BypassWhenWithinEconomizerLimits;  !- Heat Recovery Bypass Control Type", // HX bypass should be true
			"  Controller:OutdoorAir,",
			"    OA Controller 2,         !- Name",
			"    Relief Air Outlet Node 2, !- Relief Air Outlet Node Name",
			"    VAV Sys 2 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 2,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 2, !- Actuator Node Name",
			"    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should be locked out for this one, so OA flow should = min OA
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    LockoutWithHeating,               !- Lockout Type", // Lockout with heating is on
			"    ProportionalMinimum,     !- Minimum Limit Type",
			"    ,                        !- Minimum Outdoor Air Schedule Name",
			"    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Mechanical Ventilation Controller Name",
			"    ,                        !- Time of Day Economizer Control Schedule Name",
			"    No,                      !- High Humidity Control",
			"    ,                        !- Humidistat Control Zone Name",
			"    ,                        !- High Humidity Outdoor Air Flow Ratio",
			"    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
			"    BypassWhenWithinEconomizerLimits;  !- Heat Recovery Bypass Control Type", // HX bypass should be false because economizer is locked out
			"  Controller:OutdoorAir,",
			"    OA Controller 3,         !- Name",
			"    Relief Air Outlet Node 3, !- Relief Air Outlet Node Name",
			"    VAV Sys 3 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 3,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 3, !- Actuator Node Name",
			"    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type",
			"    ProportionalMinimum,     !- Minimum Limit Type",
			"    ,                        !- Minimum Outdoor Air Schedule Name",
			"    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Mechanical Ventilation Controller Name",
			"    ,                        !- Time of Day Economizer Control Schedule Name",
			"    No,                      !- High Humidity Control",
			"    ,                        !- Humidistat Control Zone Name",
			"    ,                        !- High Humidity Outdoor Air Flow Ratio",
			"    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
			"    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type", // HX bypass should be true because economizer has opened up
			"  Controller:OutdoorAir,",
			"    OA Controller 4,         !- Name",
			"    Relief Air Outlet Node 4, !- Relief Air Outlet Node Name",
			"    VAV Sys 4 Inlet Node,    !- Return Air Node Name",
			"    Mixed Air Node 4,        !- Mixed Air Node Name",
			"    Outside Air Inlet Node 4, !- Actuator Node Name",
			"    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should not open for this one - lowered the outdoor dry bulb temp for Case 4
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type",
			"    ProportionalMinimum,     !- Minimum Limit Type",
			"    ,                        !- Minimum Outdoor Air Schedule Name",
			"    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Mechanical Ventilation Controller Name",
			"    ,                        !- Time of Day Economizer Control Schedule Name",
			"    No,                      !- High Humidity Control",
			"    ,                        !- Humidistat Control Zone Name",
			"    ,                        !- High Humidity Outdoor Air Flow Ratio",
			"    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
			"    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type" // HX bypass should be true because economizer has opened up
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		GetOAControllerInputs();
		EXPECT_EQ( 2, OAController( 1 ).OANode );
		EXPECT_TRUE( OutAirNodeManager::CheckOutAirNodeNumber( OAController( 1 ).OANode ) );

		EXPECT_EQ( 6, OAController( 2 ).OANode );
		EXPECT_FALSE( OutAirNodeManager::CheckOutAirNodeNumber( OAController( 2 ).OANode ) );

		int OAControllerNum;
		int AirLoopNum;

		AirLoopControlInfo.allocate( 4 ); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
		AirLoopFlow.allocate( 4 ); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
		Node.allocate( 16 ); // will be deallocated by DataLoopNode::clear_state(); in EnergyPlusFixture

		// Initialize common AirLoop data
		for ( AirLoopNum = 1; AirLoopNum <=4; ++AirLoopNum ) {
			AirLoopControlInfo( AirLoopNum ).EconoLockout = false;
			AirLoopControlInfo( AirLoopNum ).NightVent = false;
			AirLoopControlInfo( AirLoopNum ).FanOpMode = DataHVACGlobals::ContFanCycCoil;
			AirLoopControlInfo( AirLoopNum ).LoopFlowRateSet = false;
			AirLoopControlInfo( AirLoopNum ).CheckHeatRecoveryBypassStatus = true;
			AirLoopControlInfo( AirLoopNum ).OASysComponentsSimulated = true;
			AirLoopControlInfo( AirLoopNum ).EconomizerFlowLocked = false;
			AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass = false;
			AirLoopControlInfo( AirLoopNum ).HeatRecoveryResimFlag = false; // Need this to avoid resetting hxbypass, saying this has already been simulated
			AirLoopFlow( AirLoopNum ).DesSupply = 1.0;
		}

		StdBaroPress = StdPressureSeaLevel;
		StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW( StdBaroPress, 20.0, 0.0 );

		// Initialize common OA controller and node data
		for ( OAControllerNum = 1; OAControllerNum <= 4; ++OAControllerNum ) {
			OAController( OAControllerNum ).MinOAMassFlowRate = OAController( OAControllerNum ).MinOA * StdRhoAir;
			OAController( OAControllerNum ).MaxOAMassFlowRate = OAController( OAControllerNum ).MaxOA * StdRhoAir;
			OAController( OAControllerNum ).InletNode = OAController( OAControllerNum ).OANode;
			OAController( OAControllerNum ).RetTemp = 24.0;
			OAController( OAControllerNum ).InletTemp = 20.0; // This is the same as the outdoor air dry bulb for these tests
			OAController( OAControllerNum ).OATemp = 20.0;
			OAController( OAControllerNum ).MixSetTemp = 22.0;
			OAController( OAControllerNum ).ExhMassFlow = 0.0;
			// OAController( OAControllerNum ).InletEnth = needs to be initialized if an enthalpy economizer is tested
			// OAController( OAControllerNum ).RetEnth = needs to be initialized if an enthalpy economizer is tested
			OAController( OAControllerNum ).MixMassFlow = 0.5; // Note this is 50% of design flow set above
			Node( OAControllerNum * 4 ).MassFlowRate = OAController( OAControllerNum ).MixMassFlow; // Return air nodes
			Node( OAControllerNum * 4 ).Temp = OAController( OAControllerNum ).RetTemp; // Return air nodes
			Node( OAControllerNum * 4 ).Enthalpy = Psychrometrics::PsyHFnTdbW( OAController( OAControllerNum ).RetTemp, 0.0 ); // Return air nodes, dry air
			Node( OAControllerNum * 4 - 3 ).TempSetPoint = OAController( OAControllerNum ).MixSetTemp; // Mixed air nodes
			Node( OAControllerNum * 4 - 2).Enthalpy = Psychrometrics::PsyHFnTdbW( OAController( OAControllerNum ).InletTemp, 0.0 );; // OA inlet (actuated) air nodes, dry air
		}

		Real64 expectedOAflow( 0.0 );
		Real64 expectedMinOAflow( 0.0 );

		//Case 1 - economizer active, NoLockout, BypassWhenWithinEconomizerLimits
		// economizer should open to meet the mixed air setpoint assuming dry air to make it simple, HXbypass true
		//   OAFlow = MixFlow*(MixTemp - RetTemp)/(InletTemp - RetTemp)
		AirLoopNum = 1;
		OAControllerNum = 1;
		AirLoopControlInfo( AirLoopNum ).HeatingActiveFlag = true;
		CalcOAController( OAControllerNum, AirLoopNum );

		expectedMinOAflow = 0.2 * StdRhoAir * OAController( OAControllerNum ).MixMassFlow / AirLoopFlow( AirLoopNum ).DesSupply; // For Proportional minimum input
		expectedOAflow = OAController( OAControllerNum ).MixMassFlow * ( OAController( OAControllerNum ).MixSetTemp - OAController( OAControllerNum ).RetTemp ) / ( OAController( OAControllerNum ).InletTemp - OAController( OAControllerNum ).RetTemp );
		EXPECT_NEAR( expectedOAflow, OAController( OAControllerNum ).OAMassFlow, 0.00001 );
		EXPECT_NEAR( OAController( OAControllerNum ).OAMassFlow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAFrac, 0.00001 );
		EXPECT_EQ( expectedMinOAflow, AirLoopFlow( AirLoopNum ).MinOutAir );
		EXPECT_EQ( expectedMinOAflow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAMinFrac );
		EXPECT_TRUE( AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass );
		EXPECT_EQ( 1, OAController( OAControllerNum ).HeatRecoveryBypassStatus );

		//Case 2 - economizer active, LockoutWithHeating, BypassWhenWithinEconomizerLimits
		// economizer should be locked out, OA flow at minimum, HXbypass false
		AirLoopNum = 2;
		OAControllerNum = 2;
		AirLoopControlInfo( AirLoopNum ).HeatingActiveFlag = true;
		OAController( OAControllerNum ).InletTemp = 0.0; // This is the same as the outdoor air dry bulb for these tests
		OAController( OAControllerNum ).OATemp = 0.0;

		CalcOAController( OAControllerNum, AirLoopNum );

		expectedMinOAflow = 0.2 * StdRhoAir * OAController( OAControllerNum ).MixMassFlow / AirLoopFlow( AirLoopNum ).DesSupply; // For Proportional minimum input
		expectedOAflow = expectedMinOAflow;
		EXPECT_NEAR( expectedOAflow, OAController( OAControllerNum ).OAMassFlow, 0.00001 );
		EXPECT_NEAR( OAController( OAControllerNum ).OAMassFlow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAFrac, 0.00001 );
		EXPECT_EQ( expectedMinOAflow, AirLoopFlow( AirLoopNum ).MinOutAir );
		EXPECT_EQ( expectedMinOAflow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAMinFrac );
		EXPECT_FALSE( AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass );
		EXPECT_EQ( 0, OAController( OAControllerNum ).HeatRecoveryBypassStatus );

		//Case 3 - economizer active, NoLockout, BypassWhenOAFlowGreaterThanMinimum (should be same result as Case 1)
		// economizer should open to meet the mixed air setpoint assuming dry air to make it simple, HXbypass true
		//   OAFlow = MixFlow*(MixTemp - RetTemp)/(InletTemp - RetTemp)
		AirLoopNum = 3;
		OAControllerNum = 3;
		AirLoopControlInfo( AirLoopNum ).HeatingActiveFlag = true;
		OAController( OAControllerNum ).InletTemp = 20.0; // This is the same as the outdoor air dry bulb for these tests
		OAController( OAControllerNum ).OATemp = 20.0;
		CalcOAController( OAControllerNum, AirLoopNum );

		expectedMinOAflow = 0.2 * StdRhoAir * OAController( OAControllerNum ).MixMassFlow / AirLoopFlow( AirLoopNum ).DesSupply; // For Proportional minimum input
		expectedOAflow = OAController( OAControllerNum ).MixMassFlow * ( OAController( OAControllerNum ).MixSetTemp - OAController( OAControllerNum ).RetTemp ) / ( OAController( OAControllerNum ).InletTemp - OAController( OAControllerNum ).RetTemp );
		EXPECT_NEAR( expectedOAflow, OAController( OAControllerNum ).OAMassFlow, 0.00001 );
		EXPECT_NEAR( OAController( OAControllerNum ).OAMassFlow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAFrac, 0.00001 );
		EXPECT_EQ( expectedMinOAflow, AirLoopFlow( AirLoopNum ).MinOutAir );
		EXPECT_EQ( expectedMinOAflow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAMinFrac );
		EXPECT_TRUE( AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass );
		EXPECT_EQ( 1, OAController( OAControllerNum ).HeatRecoveryBypassStatus );

		//Case 4 - economizer active, NoLockout, BypassWhenOAFlowGreaterThanMinimum
		// economizer should be at minimum due to cold outdoor temp, OA flow at minimum, HXbypass false
		AirLoopNum = 4;
		OAControllerNum = 4;
		AirLoopControlInfo( AirLoopNum ).HeatingActiveFlag = true;
		OAController( OAControllerNum ).InletTemp = 0.0; // This is the same as the outdoor air dry bulb for these tests
		OAController( OAControllerNum ).OATemp = 0.0;

		CalcOAController( OAControllerNum, AirLoopNum );

		expectedMinOAflow = 0.2 * StdRhoAir * OAController( OAControllerNum ).MixMassFlow / AirLoopFlow( AirLoopNum ).DesSupply; // For Proportional minimum input
		expectedOAflow = expectedMinOAflow;
		EXPECT_NEAR( expectedOAflow, OAController( OAControllerNum ).OAMassFlow, 0.00001 );
		EXPECT_NEAR( OAController( OAControllerNum ).OAMassFlow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAFrac, 0.00001 );
		EXPECT_EQ( expectedMinOAflow, AirLoopFlow( AirLoopNum ).MinOutAir );
		EXPECT_EQ( expectedMinOAflow / OAController( OAControllerNum ).MixMassFlow, AirLoopFlow( AirLoopNum ).OAMinFrac );
		EXPECT_FALSE( AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass );
		EXPECT_EQ( 0, OAController( OAControllerNum ).HeatRecoveryBypassStatus );

	}

	TEST_F( EnergyPlusFixture, CO2ControlDesignOccupancyTest )
	{
		Contaminant.CO2Simulation = true;
		Contaminant.CO2OutdoorSchedPtr = 1;

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"  OutdoorAir:Node,",
			"    Outside Air Inlet Node; !- Name",
			"  Schedule:Constant,",
			"    VentSchedule, !- Name",
			"     , !- Schedule Type Limits Name",
			"     1; !- Hourly value",
			"  Schedule:Constant,",
			"    ZoneADEffSch, !- Name",
			"     , !- Schedule Type Limits Name",
			"     1; !- Hourly value",
			"  Schedule:Constant,",
			"    OAFractionSched, !- Name",
			"     , !- Schedule Type Limits Name",
			"     1; !- Hourly value",
			"  Schedule:Constant,",
			"    CO2AvailSchedule, !- Name",
			"     , !- Schedule Type Limits Name",
			"     1.0; !- Hourly value",
			"  Controller:OutdoorAir,",
			"    OA Controller 1, !- Name",
			"    Relief Air Outlet Node, !- Relief Air Outlet Node Name",
			"    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
			"    Mixed Air Node, !- Mixed Air Node Name",
			"    Outside Air Inlet Node, !- Actuator Node Name",
			"    0.0, !- Minimum Outdoor Air Flow Rate{ m3 / s }",
			"    1.7, !- Maximum Outdoor Air Flow Rate{ m3 / s }",
			"    NoEconomizer, !- Economizer Control Type",
			"    ModulateFlow, !- Economizer Control Action Type",
			"    , !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
			"    , !- Economizer Maximum Limit Enthalpy{ J / kg }",
			"    , !- Economizer Maximum Limit Dewpoint Temperature{ C }",
			"    , !- Electronic Enthalpy Limit Curve Name",
			"    , !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
			"    NoLockout, !- Lockout Type",
			"    FixedMinimum, !- Minimum Limit Type",
			"    OAFractionSched, !- Minimum Outdoor Air Schedule Name",
			"    , !- Minimum Fraction of Outdoor Air Schedule Name",
			"    , !- Maximum Fraction of Outdoor Air Schedule Name",
			"    DCVObject;               !- Mechanical Ventilation Controller Name",
			"  Controller:MechanicalVentilation,",
			"    DCVObject, !- Name",
			"    VentSchedule, !- Availability Schedule Name",
			"    Yes, !- Demand Controlled Ventilation",
			"    ProportionalControlBasedonDesignOccupancy, !- System Outdoor Air Method",
			"     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
			"    West Zone, !- Zone 1 Name",
			"    CM DSOA West Zone, !- Design Specification Outdoor Air Object Name 1",
			"    CM DSZAD West Zone; !- Design Specification Zone Air Distribution Object Name 1",
		} );


		ASSERT_FALSE( process_idf( idf_objects ) );

		AirLoopControlInfo.allocate( 1 );
		AirLoopControlInfo( 1 ).LoopFlowRateSet = true;
		OARequirements.allocate( 1 );
		OARequirements( 1 ).Name = "CM DSOA WEST ZONE";
		OARequirements( 1 ).OAFlowMethod = OAFlowSum;
		OARequirements( 1 ).OAFlowPerPerson = 0.003149;
		OARequirements( 1 ).OAFlowPerArea = 0.000407;

		ZoneAirDistribution.allocate( 1 );
		ZoneAirDistribution( 1 ).Name = "CM DSZAD WEST ZONE";
		ZoneAirDistribution( 1 ).ZoneADEffSchPtr = 4;

		Zone.allocate( 1 );
		Zone( 1 ).Name = "WEST ZONE";
		Zone( 1 ).FloorArea = 10.0;
		Zone( 1 ).ZoneContamControllerSchedIndex = 4;

		AirLoopFlow.allocate( 1 );
		AirLoopFlow( 1 ).OAFrac = 0.01; // DataAirLoop variable (AirloopHVAC)
		AirLoopFlow( 1 ).OAMinFrac = 0.01; // DataAirLoop variable (AirloopHVAC)

		GetOAControllerInputs();

		EXPECT_EQ( 7, VentilationMechanical( 1 ).SystemOAMethod );
		EXPECT_TRUE( OutAirNodeManager::CheckOutAirNodeNumber( OAController( 1 ).OANode ) );
		EXPECT_NEAR( 0.00314899, VentilationMechanical( 1 ).ZoneOAPeopleRate( 1 ), 0.00001 );
		EXPECT_NEAR( 0.000407, VentilationMechanical( 1 ).ZoneOAAreaRate( 1 ), 0.00001 );

		StdRhoAir = 1.2;
		OAController( 1 ).MixMassFlow = 1.7 * StdRhoAir;
		OAController( 1 ).MaxOAMassFlowRate = 1.7 * StdRhoAir;
		AirLoopFlow( 1 ).DesSupply = 1.7;
		VentilationMechanical( 1 ).SchPtr = 1;
		Schedule( 1 ).CurrentValue = 1.0;

		VentilationMechanical( 1 ).ZoneADEffSchPtr( 1 ) = 2;
		Schedule( 2 ).CurrentValue = 1.0;
		TotPeople = 1;
		People.allocate( 1 );
		People( 1 ).Name = "WestPeople";
		People( 1 ).ZonePtr = 1;
		People( 1 ).NumberOfPeople = 3;
		Schedule( 4 ).CurrentValue = 1.0;
		ZoneCO2GainFromPeople.allocate( 1 );
		ZoneCO2GainFromPeople( 1 ) = 3.82E-8;
		OutdoorCO2 = 400;
		ZoneAirCO2.allocate( 1 );
		ZoneAirCO2( 1 ) = 600.0;
		ZoneEquipConfig.allocate( 1 );
		ZoneEquipConfig( 1 ).NumInletNodes = 1;
		ZoneEquipConfig( 1 ).AirDistUnitCool.allocate( 1 );
		ZoneEquipConfig( 1 ).AirDistUnitCool( 1 ).InNode = 10;
		ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		ZoneEquipConfig( 1 ).InletNode( 1 ) = 10;
		Node.allocate( 10 );
		Node( 10 ).Temp = 13.00;
		Node( 10 ).HumRat = 0.008;
		Node( 10 ).MassFlowRate = 1.7 * StdRhoAir;
		OutBaroPress = 101325;
		ZoneSysEnergyDemand.allocate( 1 );

		CalcOAController( 1, 1 );

		EXPECT_NEAR( 0.0194359, OAController( 1 ).OAMassFlow, 0.00001 );
		EXPECT_NEAR( 0.009527, OAController( 1 ).MinOAFracLimit, 0.00001 );

		ZoneAirCO2.deallocate();
		ZoneCO2GainFromPeople.deallocate();
	}

	TEST_F( EnergyPlusFixture, MissingDesignOccupancyTest )
	{

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",

			"Zone,",
			"  WEST ZONE,              !- Name",
			"  0,                      !- Direction of Relative North{ deg }",
			"  0,                      !- X Origin{ m }",
			"  0,                      !- Y Origin{ m }",
			"  0,                      !- Z Origin{ m }",
			"  1,                      !- Type",
			"  1,                      !- Multiplier",
			"  autocalculate,          !- Ceiling Height{ m }",
			"  autocalculate;          !- Volume{ m3 }",

			"Sizing:Zone,",
			"  WEST ZONE,              !- Zone or ZoneList Name",
			"  SupplyAirTemperature,   !- Zone Cooling Design Supply Air Temperature Input Method",
			"  14,                     !- Zone Cooling Design Supply Air Temperature{ C }",
			"  ,                       !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
			"  SupplyAirTemperature,   !- Zone Heating Design Supply Air Temperature Input Method",
			"  40,                     !- Zone Heating Design Supply Air Temperature{ C }",
			"  ,                       !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
			"  0.0085,                 !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
			"  0.008,                  !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
			"  ,                       !- Design Specification Outdoor Air Object Name",
			"  ,                       !- Zone Heating Sizing Factor",
			"  ,                       !- Zone Cooling Sizing Factor",
			"  DesignDay,              !- Cooling Design Air Flow Method",
			"  0,                      !- Cooling Design Air Flow Rate{ m3 / s }",
			"  0.000762,               !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
			"  0,                      !- Cooling Minimum Air Flow{ m3 / s }",
			"  0,                      !- Cooling Minimum Air Flow Fraction",
			"  DesignDay,              !- Heating Design Air Flow Method",
			"  0,                      !- Heating Design Air Flow Rate{ m3 / s }",
			"  0.002032,               !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
			"  0.1415762,              !- Heating Maximum Air Flow{ m3 / s }",
			"  0.3,                    !- Heating Maximum Air Flow Fraction",
			"  West Zone Design Spec Zone Air Dist; !- Design Specification Zone Air Distribution Object Name",

			"  OutdoorAir:Node,",
			"    Outside Air Inlet Node; !- Name",
			"  Schedule:Constant,",
			"    VentSchedule,           !- Name",
			"     ,                      !- Schedule Type Limits Name",
			"     1;                     !- Hourly value",
			"  Schedule:Constant,",
			"    ZoneADEffSch,           !- Name",
			"     ,                      !- Schedule Type Limits Name",
			"     1;                     !- Hourly value",
			"  Schedule:Constant,",
			"    OAFractionSched,        !- Name",
			"     ,                      !- Schedule Type Limits Name",
			"     1;                     !- Hourly value",
			"  Schedule:Constant,",
			"    CO2AvailSchedule,       !- Name",
			"     ,                      !- Schedule Type Limits Name",
			"     1.0;                   !- Hourly value",

			"  Controller:OutdoorAir,",
			"    OA Controller 1,        !- Name",
			"    Relief Air Outlet Node, !- Relief Air Outlet Node Name",
			"    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
			"    Mixed Air Node,         !- Mixed Air Node Name",
			"    Outside Air Inlet Node, !- Actuator Node Name",
			"    0.0,                    !- Minimum Outdoor Air Flow Rate{ m3 / s }",
			"    1.7,                    !- Maximum Outdoor Air Flow Rate{ m3 / s }",
			"    NoEconomizer,           !- Economizer Control Type",
			"    ModulateFlow,           !- Economizer Control Action Type",
			"    ,                       !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
			"    ,                       !- Economizer Maximum Limit Enthalpy{ J / kg }",
			"    ,                       !- Economizer Maximum Limit Dewpoint Temperature{ C }",
			"    ,                       !- Electronic Enthalpy Limit Curve Name",
			"    ,                       !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
			"    NoLockout,              !- Lockout Type",
			"    FixedMinimum,           !- Minimum Limit Type",
			"    OAFractionSched,        !- Minimum Outdoor Air Schedule Name",
			"    ,                       !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                       !- Maximum Fraction of Outdoor Air Schedule Name",
			"    DCVObject;              !- Mechanical Ventilation Controller Name",

			"  Controller:MechanicalVentilation,",
			"    DCVObject, !- Name",
			"    VentSchedule, !- Availability Schedule Name",
			"    Yes, !- Demand Controlled Ventilation",
			"    ZoneSum, !- System Outdoor Air Method",
			"     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
			"    WEST ZONE, !- Zone 1 Name",
			"    , !- Design Specification Outdoor Air Object Name 1",
			"    West Zone Design Spec Zone Air Dist; !- Design Specification Zone Air Distribution Object Name 1",

			"DesignSpecification:ZoneAirDistribution,",
			"  West Zone Design Spec Zone Air Dist, !- Name",
			"  1, !- Zone Air Distribution Effectiveness in Cooling Mode{ dimensionless }",
			"  1; !- Zone Air Distribution Effectiveness in Heating Mode{ dimensionless }",
		} );


		ASSERT_FALSE( process_idf( idf_objects ) );

		AirLoopControlInfo.allocate( 1 );
		AirLoopControlInfo( 1 ).LoopFlowRateSet = true;
		OARequirements.allocate( 1 );
		ZoneAirDistribution.allocate( 1 );
		ZoneAirDistribution( 1 ).Name = "CM DSZAD WEST ZONE";
		ZoneAirDistribution( 1 ).ZoneADEffSchPtr = 4;

		AirLoopFlow.allocate( 1 );
		AirLoopFlow( 1 ).OAFrac = 0.01; // DataAirLoop variable (AirloopHVAC)
		AirLoopFlow( 1 ).OAMinFrac = 0.01; // DataAirLoop variable (AirloopHVAC)

		GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound ); // expect no errors
		GetZoneAirDistribution();
		GetZoneSizingInput();
		DataGlobals::DoZoneSizing = true;
		GetOAControllerInputs();

		EXPECT_EQ( 0.00944, VentilationMechanical( 1 ).ZoneOAPeopleRate( 1 ) );
		EXPECT_EQ( 0.00, VentilationMechanical( 1 ).ZoneOAAreaRate( 1 ) );
		EXPECT_EQ( 0.00, VentilationMechanical( 1 ).ZoneOAFlow( 1 ) );
		EXPECT_EQ( 0.00, VentilationMechanical( 1 ).ZoneOAACH( 1 ) );

	}

	TEST_F( EnergyPlusFixture, MixedAir_TestHXinOASystem )
	{
		std::string const idf_objects = delimited_string( {
			"Version,8.3;",

			"  OutdoorAir:Node,",
			"    Outside Air Inlet Node;  !- Name",

			"  Controller:OutdoorAir,",
			"    OA Controller 1,         !- Name",
			"    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
			"    Air Loop Inlet Node,     !- Return Air Node Name",
			"    Mixed Air Node,          !- Mixed Air Node Name",
			"    Outside Air Inlet Node,  !- Actuator Node Name",
			"    1.0,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    NoEconomizer,            !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
			"    ModulateFlow,            !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    NoLockout,               !- Lockout Type", // No lockout
			"    ProportionalMinimum,     !- Minimum Limit Type",
			"    ,                        !- Minimum Outdoor Air Schedule Name",
			"    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
			"    ,                        !- Mechanical Ventilation Controller Name",
			"    ,                        !- Time of Day Economizer Control Schedule Name",
			"    No,                      !- High Humidity Control",
			"    ,                        !- Humidistat Control Zone Name",
			"    ,                        !- High Humidity Outdoor Air Flow Ratio",
			"    No;                      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",

			"  HeatExchanger:AirToAir:FlatPlate,",
			"    OA Heat Exchanger 1,     !- Name",
			"    ,                        !- Availability Schedule Name",
			"    ParallelFlow,            !- Flow Arrangement Type",
			"    No,                      !- Economizer Lockout",
			"    1,                       !- Ratio of Supply to Secondary hA Values",
			"    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
			"    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
			"    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
			"    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
			"    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
			"    0,                       !- Nominal Electric Power{ W }",
			"    Heat Exchanger Outlet Node2, !- Supply Air Inlet Node Name",
			"    Heat Exchanger Outlet Node,  !- Supply Air Outlet Node Name",
			"    Relief Air Outlet Node,      !- Secondary Air Inlet Node Name",
			"    Heat Exchanger Secondary Outlet Node;  !- Secondary Air Outlet Node Name",

			"  HeatExchanger:AirToAir:FlatPlate,",
			"    OA Heat Exchanger 2,     !- Name",
			"    ,                        !- Availability Schedule Name",
			"    ParallelFlow,            !- Flow Arrangement Type",
			"    No,                      !- Economizer Lockout",
			"    1,                       !- Ratio of Supply to Secondary hA Values",
			"    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
			"    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
			"    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
			"    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
			"    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
			"    0,                       !- Nominal Electric Power{ W }",
			"    Outside Air Inlet Node,  !- Supply Air Inlet Node Name",
			"    Heat Exchanger Outlet Node2,           !- Supply Air Outlet Node Name",
			"    Heat Exchanger Secondary Outlet Node,  !- Secondary Air Inlet Node Name",
			"    Heat Exchanger Secondary Outlet Node2; !- Secondary Air Outlet Node Name",

			"  OutdoorAir:Mixer,",
			"    OA Mixer,                !- Name",
			"    Mixed Air Node,          !- Mixed Air Node Name",
			"    Heat Exchanger Outlet Node, !- Outdoor Air Stream Node Name",
			"    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
			"    Air Loop Inlet Node;     !- Return Air Stream Node Name",

			" AirLoopHVAC:ControllerList,",
			"    OA Sys 1 controller,     !- Name",
			"    Controller:OutdoorAir,   !- Controller 1 Object Type",
			"    OA Controller 1;         !- Controller 1 Name",

			" AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
			"    OA Sys 1 Equipment list, !- Name",
			"    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
			"    OA Heat Exchanger 2,     !- Component 1 Name",
			"    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
			"    OA Heat Exchanger 1,     !- Component 1 Name",
			"    OutdoorAir:Mixer,        !- Component 2 Object Type",
			"    OA Mixer;                !- Component 2 Name",

			" AirLoopHVAC:OutdoorAirSystem,",
			"    OA Sys 1, !- Name",
			"    OA Sys 1 controller,     !- Controller List Name",
			"    OA Sys 1 Equipment list; !- Outdoor Air Equipment List Name",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetOASysInputFlag = true;
		DataGlobals::BeginEnvrnFlag = true;
		int AirloopNum = 1;
		int OASysNum = 1;
		int OAControllerNum = 1;
		AirLoopControlInfo.allocate( AirloopNum ); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
		AirLoopFlow.allocate( AirloopNum ); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
		AirLoopFlow( AirloopNum ).DesSupply = 1.0;
		DataEnvironment::StdRhoAir = 1.2;
		DataEnvironment::OutBaroPress = 101250.0;

		// setup OA system and initialize nodes
		ManageOutsideAirSystem( "OA Sys 1", true, AirloopNum, OASysNum );

		// reset nodes to common property
		for( int i = 1; i <= DataLoopNode::NumOfNodes; ++i ) {
			Node( i ).Temp = 20.0;
			Node( i ).HumRat = 0.01;
			Node( i ).Enthalpy = 45478.0;
			Node( i ).MassFlowRate = 1.0;
			Node( i ).MassFlowRateMaxAvail = 1.0;
			Node( i ).Press = 101250.0;
		}

		// simulate OA system, common node property is propogated
		ManageOutsideAirSystem( "OA Sys 1", true, AirloopNum, OASysNum );

		// change node property at OA inlet and mixer inlet
		Node( 2 ).Temp = 18.0; // reset temps at HX
		Node( 5 ).Temp = 24.0;

		// simulate OA system
		ManageOutsideAirSystem( "OA Sys 1", true, AirloopNum, OASysNum );

		int mixedAirNode = OAController( OAControllerNum ).MixNode;
		int mixerIntletNode = OAController( OAControllerNum ).InletNode;
		EXPECT_EQ( Node( mixedAirNode ).Temp, Node( mixerIntletNode ).Temp );

	}

}
