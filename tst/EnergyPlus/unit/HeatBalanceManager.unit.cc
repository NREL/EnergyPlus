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

// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <ZoneEquipmentManager.hh>
#include <HeatBalanceAirManager.hh>
#include <ScheduleManager.hh>
#include <DataHeatBalFanSys.hh>
#include <DataZoneEquipment.hh>
#include <DataLoopNode.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataHVACGlobals.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::ZoneEquipmentManager;
using namespace EnergyPlus::HeatBalanceAirManager;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataHVACGlobals;

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

	TEST_F( EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData1 )
	{
		// Test get input for ZoneAirMassFlowConservation object

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"Building,",
			"My Building, !- Name",
			"30., !- North Axis{ deg }",
			"City, !- Terrain",
			"0.04, !- Loads Convergence Tolerance Value",
			"0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
			"FullExterior, !- Solar Distribution",
			"25, !- Maximum Number of Warmup Days",
			"6;                       !- Minimum Number of Warmup Days",
			"ZoneAirMassFlowConservation,",
			"Yes, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
			"AddInfiltrationFlow, !- Infiltration Balancing Method",
			"MixingSourceZoneOnly; !- Infiltration Balancing Zones",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input

		// call to process input
		ErrorsFound = false;
		GetProjectControlData( ErrorsFound ); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
		EXPECT_FALSE( ErrorsFound );
		EXPECT_TRUE( ZoneAirMassFlow.EnforceZoneMassBalance );
		EXPECT_TRUE( ZoneAirMassFlow.BalanceMixing );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationTreatment, AddInfiltrationFlow );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationZoneType, MixingSourceZonesOnly );

	}

	TEST_F( EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData2 )
	{
		// Test get input for ZoneAirMassFlowConservation object

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"Building,",
			"My Building, !- Name",
			"30., !- North Axis{ deg }",
			"City, !- Terrain",
			"0.04, !- Loads Convergence Tolerance Value",
			"0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
			"FullExterior, !- Solar Distribution",
			"25, !- Maximum Number of Warmup Days",
			"6;                       !- Minimum Number of Warmup Days",
			"ZoneAirMassFlowConservation,",
			"No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
			"AdjustInfiltrationFlow, !- Infiltration Balancing Method",
			"AllZones;                !- Infiltration Balancing Zones",
			"Zone, Zone 1;",
			"Zone, Zone 2;",
			"ZoneMixing,",
			"Zone 2 Zone Mixing, !- Name",
			"Zone 2, !- Zone Name",
			"Always1, !- Schedule Name",
			"Flow/Zone, !- Design Flow Rate Calculation Method",
			"0.07, !- Design Flow Rate{ m3 / s }",
			", !- Flow Rate per Zone Floor Area{ m3 / s - m2 }",
			", !- Flow Rate per Person{ m3 / s - person }",
			", !- Air Changes per Hour{ 1 / hr }",
			"Zone 1, !- Source Zone Name",
			"0.0;                     !- Delta Temperature{ deltaC }",
			"ZoneInfiltration:DesignFlowRate,",
			"Zone 1 Infil 1, !- Name",
			"Zone 1, !- Zone or ZoneList Name",
			"Always1, !- Schedule Name",
			"flow/zone, !- Design Flow Rate Calculation Method",
			"0.032, !- Design Flow Rate{ m3 / s }",
			", !- Flow per Zone Floor Area{ m3 / s - m2 }",
			", !- Flow per Exterior Surface Area{ m3 / s - m2 }",
			", !- Air Changes per Hour{ 1 / hr }",
			"1, !- Constant Term Coefficient",
			"0, !- Temperature Term Coefficient",
			"0, !- Velocity Term Coefficient",
			"0; !- Velocity Squared Term Coefficient",
			"Schedule:Constant,Always1,,1.0;"

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input

		// call to process input
		ProcessScheduleInput();
		ErrorsFound = false;
		GetProjectControlData( ErrorsFound ); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
		EXPECT_FALSE( ErrorsFound );
		EXPECT_TRUE( ZoneAirMassFlow.EnforceZoneMassBalance );
		EXPECT_FALSE( ZoneAirMassFlow.BalanceMixing );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationTreatment, AdjustInfiltrationFlow );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationZoneType, AllZones );

		// setup mixing and infiltration objects
		NumOfZones = 2;
		ZoneReOrder.allocate( NumOfZones );
		ErrorsFound = false;
		GetZoneData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		AllocateHeatBalArrays();
		ErrorsFound = false;
		GetSimpleAirModelInputs( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		SetZoneMassConservationFlag();
		// setup zone equipment configuration
		ZoneEquipConfig.allocate( NumOfZones );

		ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
		ZoneEquipConfig( 1 ).ActualZoneNum = 1;
		ZoneEquipConfig( 1 ).NumInletNodes = 1;
		ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
		ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
		ZoneEquipConfig( 1 ).ZoneNode = 1;
		ZoneEquipConfig( 1 ).InletNode( 1 ) = 2;
		ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
		ZoneEquipConfig( 1 ).ReturnAirNode = 4;
		ZoneEquipConfig( 1 ).IsControlled = true;
		ZoneEquipConfig( 1 ).AirLoopNum = 1;
		ZoneEquipConfig( 1 ).ReturnFlowSchedPtrNum = ScheduleAlwaysOn;

		ZoneEquipConfig( 2 ).ZoneName = "Zone 2";
		ZoneEquipConfig( 2 ).ActualZoneNum = 2;
		ZoneEquipConfig( 2 ).NumExhaustNodes = 1;
		ZoneEquipConfig( 2 ).ExhaustNode.allocate( 1 );
		ZoneEquipConfig( 2 ).NumInletNodes = 1;
		ZoneEquipConfig( 2 ).InletNode.allocate( 1 );
		ZoneEquipConfig( 2 ).ZoneNode = 5;
		ZoneEquipConfig( 2 ).InletNode( 1 ) = 6;
		ZoneEquipConfig( 2 ).ExhaustNode( 1 ) = 7;
		ZoneEquipConfig( 2 ).ReturnAirNode = 8;
		ZoneEquipConfig( 2 ).IsControlled = true;
		ZoneEquipConfig( 2 ).AirLoopNum = 1;
		ZoneEquipConfig( 2 ).ReturnFlowSchedPtrNum = ScheduleAlwaysOn;

		ZoneEquipInputsFilled = true;
		NumPrimaryAirSys = 1;
		AirLoopFlow.allocate( 1 );
		PrimaryAirSystem.allocate( 1 );
		PrimaryAirSystem( 1 ).OASysExists = true;
		Node.allocate( 8 );

		Node( 1 ).MassFlowRate = 0.0; // Zone 1 zone node
		Node( 2 ).MassFlowRate = 1.0; // Zone 1 inlet node
		Node( 3 ).MassFlowRate = 2.0; // Zone 1 exhaust node
		Node( 4 ).MassFlowRate = 9.0; // Zone 1 return node
		ZoneEquipConfig( 1 ).ZoneExh = 2.0;

		Node( 5 ).MassFlowRate = 0.0; // Zone 2 zone node
		Node( 6 ).MassFlowRate = 2.0; // Zone 2 inlet node
		Node( 7 ).MassFlowRate = 0.0; // Zone 2 exhaust node
		Node( 8 ).MassFlowRate = 8.0; // Zone 2 return node
		ZoneEquipConfig( 2 ).ZoneExh = 0.0;
		AirLoopFlow( 1 ).MaxOutAir = Node( 2 ).MassFlowRate + Node( 6 ).MassFlowRate;
		Infiltration( 1 ).MassFlowRate = 0.5;
		Mixing( 1 ).MixingMassFlowRate = 0.1;

		// call zone air mass balance
		CalcZoneMassBalance();
		EXPECT_EQ( Node( 4 ).MassFlowRate, 0.0 ); // Zone 1 return node (max(0.0, 1-2)
		EXPECT_EQ( Infiltration( 1 ).MassFlowRate, 1.0); // Zone 1 infiltration flow rate (2 - 1)
		EXPECT_EQ( Mixing( 1 ).MixingMassFlowRate, 0.1 ); // Zone 1 to Zone 2 mixing flow rate (unchanged)
		EXPECT_EQ( Node( 8 ).MassFlowRate, 1.0 ); // Zone 2 return node (2 * (2+1-2)/2)

		ZoneReOrder.deallocate();
		ZoneEquipConfig.deallocate();
		Node.deallocate();
		PrimaryAirSystem.deallocate();
		AirLoopFlow.deallocate();
		NumPrimaryAirSys = 0;

	}

	TEST_F( EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData3 )
	{
		// Test get input for ZoneAirMassFlowConservation object

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"Building,",
			"My Building, !- Name",
			"30., !- North Axis{ deg }",
			"City, !- Terrain",
			"0.04, !- Loads Convergence Tolerance Value",
			"0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
			"FullExterior, !- Solar Distribution",
			"25, !- Maximum Number of Warmup Days",
			"6;                       !- Minimum Number of Warmup Days",
			"ZoneAirMassFlowConservation,",
			"No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
			"None, !- Infiltration Balancing Method",
			"Ignored;                !- Infiltration Balancing Zones"
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input

		// call to process input
		ErrorsFound = false;
		GetProjectControlData( ErrorsFound ); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
		EXPECT_FALSE( ErrorsFound );
		EXPECT_FALSE( ZoneAirMassFlow.EnforceZoneMassBalance );
		EXPECT_FALSE( ZoneAirMassFlow.BalanceMixing );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationTreatment, NoInfiltrationFlow );
		EXPECT_EQ( ZoneAirMassFlow.InfiltrationZoneType, 0 );

	}
}
