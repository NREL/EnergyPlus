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

// C++ Headers
#include <algorithm>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ZonePlenum.hh>
#include <DataContaminantBalance.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <PoweredInductionUnits.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ZonePlenum {
	// Module containing simulation routines for both zone return and zone supply plenums

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   November 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage Air Path Zone Return Plenum Components

	// METHODOLOGY EMPLOYED:
	// The Zone Plenum

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::NumOfZones;
	using namespace DataLoopNode;
	using namespace DataHVACGlobals;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutHumRat;

	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyTdbFnHW;
	using Psychrometrics::PsyHFnTdbW;

	// Data
	// DERIVED TYPE DEFINITIONS

	int NumZonePlenums( 0 ); // The Number of ZonePlenums found in the Input
	int NumZoneReturnPlenums( 0 ); // The Number of ZoneReturnPlenums found in the Input
	int NumZoneSupplyPlenums( 0 ); // The Number of ZoneSupplyPlenums found in the Input
	Array1D_bool CheckRetEquipName;
	Array1D_bool CheckSupEquipName;

	namespace {
		bool GetInputFlag( true ); // Flag set to make sure you get input once
		bool InitAirZoneReturnPlenumEnvrnFlag( true );
		bool InitAirZoneReturnPlenumOneTimeFlag( true );
	}
	// SUBROUTINE SPECIFICATIONS FOR MODULE ZONEPLENUM

	// Object Data
	Array1D< ZoneReturnPlenumConditions > ZoneRetPlenCond;
	Array1D< ZoneSupplyPlenumConditions > ZoneSupPlenCond;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		GetInputFlag = true;
		InitAirZoneReturnPlenumEnvrnFlag = true;
		InitAirZoneReturnPlenumOneTimeFlag = true;
		NumZonePlenums = 0;
		NumZoneReturnPlenums = 0;
		NumZoneSupplyPlenums = 0;
		ZoneRetPlenCond.deallocate();
		ZoneSupPlenCond.deallocate();
	}

	void
	SimAirZonePlenum(
		std::string const & CompName,
		int const iCompType,
		int & CompIndex,
		Optional_bool_const FirstHVACIteration, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool_const FirstCall, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool PlenumInletChanged //Autodesk:OPTIONAL Used without PRESENT check
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       March 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the ZonePlenum component simulation for both
		// return and supply plenums.
		// It is called from the SimAirLoopComponent at the system time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataZoneEquipment::ZoneReturnPlenum_Type;
		using DataZoneEquipment::ZoneSupplyPlenum_Type;

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZonePlenumNum; // The ZonePlenum that you are currently loading input into

		// FLOW:

		// Obtains and Allocates ZonePlenum related parameters from input file
		if ( GetInputFlag ) { // First time subroutine has been entered
			GetZonePlenumInput();
			GetInputFlag = false;
		}

		if ( iCompType == ZoneReturnPlenum_Type ) { // 'AirLoopHVAC:ReturnPlenum'
			// Find the correct ZonePlenumNumber
			if ( CompIndex == 0 ) {
				ZonePlenumNum = FindItemInList( CompName, ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZonePlenumName );
				if ( ZonePlenumNum == 0 ) {
					ShowFatalError( "SimAirZonePlenum: AirLoopHVAC:ReturnPlenum not found=" + CompName );
				}
				CompIndex = ZonePlenumNum;
			} else {
				ZonePlenumNum = CompIndex;
				if ( ZonePlenumNum > NumZoneReturnPlenums || ZonePlenumNum < 1 ) {
					ShowFatalError( "SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits( ZonePlenumNum ) + ", Number of AirLoopHVAC:ReturnPlenum=" + TrimSigDigits( NumZoneReturnPlenums ) + ", AirLoopHVAC:ReturnPlenum name=" + CompName );
				}
				if ( CheckRetEquipName( ZonePlenumNum ) ) {
					if ( CompName != ZoneRetPlenCond( ZonePlenumNum ).ZonePlenumName ) {
						ShowFatalError( "SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits( ZonePlenumNum ) + ", AirLoopHVAC:ReturnPlenum name=" + CompName + ", stored AirLoopHVAC:ReturnPlenum Name for that index=" + ZoneRetPlenCond( ZonePlenumNum ).ZonePlenumName );
					}
					CheckRetEquipName( ZonePlenumNum ) = false;
				}
			}

			InitAirZoneReturnPlenum( ZonePlenumNum ); // Initialize all ZonePlenum related parameters

			CalcAirZoneReturnPlenum( ZonePlenumNum );

			UpdateAirZoneReturnPlenum( ZonePlenumNum ); // Update the current ZonePlenum to the outlet nodes

			ReportZoneReturnPlenum( ZonePlenumNum );

		} else if ( iCompType == ZoneSupplyPlenum_Type ) { // 'AirLoopHVAC:SupplyPlenum'
			// Find the correct ZonePlenumNumber
			if ( CompIndex == 0 ) {
				ZonePlenumNum = FindItemInList( CompName, ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZonePlenumName );
				if ( ZonePlenumNum == 0 ) {
					ShowFatalError( "SimAirZonePlenum: AirLoopHVAC:SupplyPlenum not found=" + CompName );
				}
				CompIndex = ZonePlenumNum;
			} else {
				ZonePlenumNum = CompIndex;
				if ( ZonePlenumNum > NumZoneSupplyPlenums || ZonePlenumNum < 1 ) {
					ShowFatalError( "SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits( ZonePlenumNum ) + ", Number of AirLoopHVAC:SupplyPlenum=" + TrimSigDigits( NumZoneReturnPlenums ) + ", AirLoopHVAC:SupplyPlenum name=" + CompName );
				}
				if ( CheckSupEquipName( ZonePlenumNum ) ) {
					if ( CompName != ZoneSupPlenCond( ZonePlenumNum ).ZonePlenumName ) {
						ShowFatalError( "SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits( ZonePlenumNum ) + ", AirLoopHVAC:SupplyPlenum name=" + CompName + ", stored AirLoopHVAC:SupplyPlenum Name for that index=" + ZoneSupPlenCond( ZonePlenumNum ).ZonePlenumName );
					}
					CheckSupEquipName( ZonePlenumNum ) = false;
				}
			}

			InitAirZoneSupplyPlenum( ZonePlenumNum, FirstHVACIteration, FirstCall ); // Initialize all ZonePlenum related parameters

			CalcAirZoneSupplyPlenum( ZonePlenumNum, FirstCall );
			// Update the current ZonePlenum to the outlet nodes
			UpdateAirZoneSupplyPlenum( ZonePlenumNum, PlenumInletChanged, FirstCall );

			ReportZoneSupplyPlenum( ZonePlenumNum );

		} else {
			ShowSevereError( "SimAirZonePlenum: Errors in Plenum=" + CompName );
			ShowContinueError( "ZonePlenum: Unhandled plenum type found:" + TrimSigDigits( iCompType ) );
			ShowFatalError( "Preceding conditions cause termination." );

		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetZonePlenumInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       August 2003, FCW: For each zone with a return air plenum put the ZoneRetPlenCond
		//                       number for the return air plenum in the ZoneEquipConfig array for the zone
		//                       for later access to the zone's return air plenum conditions.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main routine to call other input routines and Get routines

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using NodeInputManager::GetNodeNums;
		using NodeInputManager::InitUniqueNodeCheck;
		using NodeInputManager::CheckUniqueNodes;
		using NodeInputManager::EndUniqueNodeCheck;
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::EquipConfiguration;
		using namespace DataIPShortCuts;
		using PoweredInductionUnits::PIUInducesPlenumAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZonePlenumNum; // The ZonePlenum that you are currently loading input into
		int ZonePlenumLoop;
		int ZoneEquipConfigLoop;
		int NumAlphas;
		int NumNums;
		int NumArgs;
		int NumNodes;
		Array1D_int NodeNums;
		int MaxNums;
		int MaxAlphas;
		int NodeNum;
		int IOStat;
		Array1D< Real64 > NumArray; // Numeric input items for object
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool NodeListError; // Flag for node list error
		bool UniqueNodeError;
		static std::string const RoutineName( "GetZonePlenumInput: " ); // include trailing blank space
		std::string InducedNodeListName;

		// Flow
		GetObjectDefMaxArgs( "AirLoopHVAC:ReturnPlenum", NumArgs, NumAlphas, NumNums );
		MaxNums = NumNums;
		MaxAlphas = NumAlphas;
		GetObjectDefMaxArgs( "AirLoopHVAC:SupplyPlenum", NumArgs, NumAlphas, NumNums );
		MaxNums = max( NumNums, MaxNums );
		MaxAlphas = max( NumAlphas, MaxAlphas );
		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		NumArray.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );
		GetObjectDefMaxArgs( "NodeList", NumArgs, NumAlphas, NumNums );
		NodeNums.dimension( NumArgs, 0 );

		InducedNodeListName = "";

		NumZoneReturnPlenums = GetNumObjectsFound( "AirLoopHVAC:ReturnPlenum" );
		NumZoneSupplyPlenums = GetNumObjectsFound( "AirLoopHVAC:SupplyPlenum" );
		NumZonePlenums = NumZoneReturnPlenums + NumZoneSupplyPlenums;

		if ( NumZoneReturnPlenums > 0 ) ZoneRetPlenCond.allocate( NumZoneReturnPlenums );
		if ( NumZoneSupplyPlenums > 0 ) ZoneSupPlenCond.allocate( NumZoneSupplyPlenums );
		CheckRetEquipName.dimension( NumZoneReturnPlenums, true );
		CheckSupEquipName.dimension( NumZoneSupplyPlenums, true );

		ZonePlenumNum = 0;

		InitUniqueNodeCheck( "AirLoopHVAC:ReturnPlenum" );
		for ( ZonePlenumLoop = 1; ZonePlenumLoop <= NumZoneReturnPlenums; ++ZonePlenumLoop ) {
			++ZonePlenumNum;

			CurrentModuleObject = "AirLoopHVAC:ReturnPlenum";

			GetObjectItem( CurrentModuleObject, ZonePlenumNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZonePlenumName, ZonePlenumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			ZoneRetPlenCond( ZonePlenumNum ).ZonePlenumName = AlphArray( 1 );

			// Check if this zone is also used in another return plenum
			IOStat = FindItemInList( AlphArray( 2 ), ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName, ZonePlenumNum - 1 );
			if ( IOStat != 0 ) {
				ShowSevereError( RoutineName + cAlphaFields( 2 ) + " \"" + AlphArray( 2 ) + "\" is used more than once as a " + CurrentModuleObject + '.' );
				ShowContinueError( "..Only one " + CurrentModuleObject + " object may be connected to a given zone." );
				ShowContinueError( "..occurs in " + CurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			ZoneRetPlenCond( ZonePlenumNum ).ZoneName = AlphArray( 2 );
			// put the X-Ref to the zone heat balance data structure
			ZoneRetPlenCond( ZonePlenumNum ).ActualZoneNum = FindItemInList( AlphArray( 2 ), Zone );
			if ( ZoneRetPlenCond( ZonePlenumNum ).ActualZoneNum == 0 ) {
				ShowSevereError( "For " + CurrentModuleObject + " = " + AlphArray( 1 ) + ", " + cAlphaFields( 2 ) + " = " + AlphArray( 2 ) + " not found." );
				ErrorsFound = true;
				continue;
			}
			//  Check if this zone is used as a controlled zone
			ZoneEquipConfigLoop = FindItemInList( AlphArray( 2 ), ZoneEquipConfig, &EquipConfiguration::ZoneName );
			if ( ZoneEquipConfigLoop != 0 ) {
				ShowSevereError( RoutineName + cAlphaFields( 2 ) + " \"" + AlphArray( 2 ) + "\" is a controlled zone. It cannot be used as a " + CurrentModuleObject );
				ShowContinueError( "..occurs in " + CurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeName = AlphArray( 3 );
			ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_ZoneNode, 1, ObjectIsNotParent );
			//Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
			Zone( ZoneRetPlenCond( ZonePlenumNum ).ActualZoneNum ).SystemZoneNodeNumber = ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum;

			ZoneRetPlenCond( ZonePlenumNum ).OutletNode = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			InducedNodeListName = AlphArray( 5 );
			NodeListError = false;
			GetNodeNums( InducedNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Air, "AirLoopHVAC:ReturnPlenum", ZoneRetPlenCond( ZonePlenumNum ).ZonePlenumName, NodeConnectionType_InducedAir, 1, ObjectIsNotParent, _, cAlphaFields( 5 ) );

			if ( ! NodeListError ) {
				ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes = NumNodes;
				ZoneRetPlenCond( ZonePlenumNum ).InducedNode.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedCO2.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedTemp = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedPressure = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedCO2 = 0.0;
				ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam = 0.0;
				for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
					ZoneRetPlenCond( ZonePlenumNum ).InducedNode( NodeNum ) = NodeNums( NodeNum );
					UniqueNodeError = false;
					CheckUniqueNodes( "Return Plenum Induced Air Nodes", "NodeNumber", UniqueNodeError, _, NodeNums( NodeNum ) );
					if ( UniqueNodeError ) {
						ShowContinueError( "Occurs for ReturnPlenum = " + AlphArray( 1 ) );
						ErrorsFound = true;
					}
					PIUInducesPlenumAir( ZoneRetPlenCond( ZonePlenumNum ).InducedNode( NodeNum ) );
				}
			} else {
				ShowContinueError( "Invalid Induced Air Outlet Node or NodeList name in AirLoopHVAC:ReturnPlenum object = " + ZoneRetPlenCond( ZonePlenumNum ).ZonePlenumName );
				ErrorsFound = true;
			}

			ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes = NumAlphas - 5;

			for ( auto & e : ZoneRetPlenCond ) e.InitFlag = true;

			ZoneRetPlenCond( ZonePlenumNum ).InletNode.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletTemp.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletHumRat.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletEnthalpy.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).InletPressure.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );
			ZoneRetPlenCond( ZonePlenumNum ).ZoneEqNum.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes );

			ZoneRetPlenCond( ZonePlenumNum ).InletNode = 0;
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletTemp = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletHumRat = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletEnthalpy = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).InletPressure = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletTemp = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletHumRat = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletEnthalpy = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).OutletPressure = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat = 0.0;
			ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy = 0.0;

			for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++NodeNum ) {

				ZoneRetPlenCond( ZonePlenumNum ).InletNode( NodeNum ) = GetOnlySingleNode( AlphArray( 5 + NodeNum ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			}

		} // end AirLoopHVAC:ReturnPlenum Loop
		EndUniqueNodeCheck( "AirLoopHVAC:ReturnPlenum" );

		ZonePlenumNum = 0;

		for ( ZonePlenumLoop = 1; ZonePlenumLoop <= NumZoneSupplyPlenums; ++ZonePlenumLoop ) {
			++ZonePlenumNum;

			CurrentModuleObject = "AirLoopHVAC:SupplyPlenum";

			GetObjectItem( CurrentModuleObject, ZonePlenumNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZonePlenumName, ZonePlenumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			ZoneSupPlenCond( ZonePlenumNum ).ZonePlenumName = AlphArray( 1 );

			// Check if this zone is also used in another plenum
			IOStat = FindItemInList( AlphArray( 2 ), ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZoneName, ZonePlenumNum - 1 );
			if ( IOStat != 0 ) {
				ShowSevereError( RoutineName + cAlphaFields( 2 ) + " \"" + AlphArray( 2 ) + "\" is used more than once as a " + CurrentModuleObject + '.' );
				ShowContinueError( "..Only one " + CurrentModuleObject + " object may be connected to a given zone." );
				ShowContinueError( "..occurs in " + CurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			if ( NumZoneReturnPlenums > 0 ) { // Check if this zone is also used in another plenum
				IOStat = FindItemInList( AlphArray( 2 ), ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName );
				if ( IOStat != 0 ) {
					ShowSevereError( RoutineName + cAlphaFields( 2 ) + " \"" + AlphArray( 2 ) + "\" is used more than once as a " + CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum." );
					ShowContinueError( "..Only one " + CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum object may be connected to a given zone." );
					ShowContinueError( "..occurs in " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}
			ZoneSupPlenCond( ZonePlenumNum ).ZoneName = AlphArray( 2 );
			// put the X-Ref to the zone heat balance data structure
			ZoneSupPlenCond( ZonePlenumNum ).ActualZoneNum = FindItemInList( AlphArray( 2 ), Zone );
			if ( ZoneSupPlenCond( ZonePlenumNum ).ActualZoneNum == 0 ) {
				ShowSevereError( "For " + CurrentModuleObject + " = " + AlphArray( 1 ) + ", " + cAlphaFields( 2 ) + " = " + AlphArray( 2 ) + " not found." );
				ErrorsFound = true;
				continue;
			}
			//  Check if this zone is used as a controlled zone
			if ( std::any_of( ZoneEquipConfig.begin(), ZoneEquipConfig.end(), []( EquipConfiguration const & e ){ return e.IsControlled; } ) ) {
				ZoneEquipConfigLoop = FindItemInList( AlphArray( 2 ), ZoneEquipConfig, &EquipConfiguration::ZoneName );
				if ( ZoneEquipConfigLoop != 0 ) {
					ShowSevereError( RoutineName + cAlphaFields( 2 ) + " \"" + AlphArray( 2 ) + "\" is a controlled zone. It cannot be used as a " + CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum." );
					ShowContinueError( "..occurs in " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}
			// Check if this is also used as a return plenum
			//  *** This next IF loop looks wrong.  Sent e-mail to Peter/Brent 8/14/08 for clarification ****
			//      IF (NumZoneReturnPlenums > 0) THEN
			//        IOSTAT=FindItemInList(AlphArray(1),ZoneRetPlenCond%ZoneName,NumZoneReturnPlenums)
			//        IF (IOStat /= 0) THEN
			//          CALL ShowSevereError(RoutineName//'Plenum "'//TRIM(AlphArray(2))//  &
			//                               '" is a controlled zone.  It cannot be used as a '//  &
			//                               'SUPPLY PLENUM or RETURN PLENUM.')
			//          CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
			//          ErrorsFound=.TRUE.
			//        ENDIF
			//      ENDIF

			ZoneSupPlenCond( ZonePlenumNum ).ZoneNodeName = AlphArray( 3 );
			ZoneSupPlenCond( ZonePlenumNum ).ZoneNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_ZoneNode, 1, ObjectIsNotParent );
			//Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
			Zone( ZoneSupPlenCond( ZonePlenumNum ).ActualZoneNum ).SystemZoneNodeNumber = ZoneSupPlenCond( ZonePlenumNum ).ZoneNodeNum;

			ZoneSupPlenCond( ZonePlenumNum ).InletNode = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes = NumAlphas - 4;

			for ( auto & e : ZoneSupPlenCond ) e.InitFlag = true;

			ZoneSupPlenCond( ZonePlenumNum ).OutletNode.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRate.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletTemp.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletHumRat.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletEnthalpy.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );
			ZoneSupPlenCond( ZonePlenumNum ).OutletPressure.allocate( ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes );

			ZoneSupPlenCond( ZonePlenumNum ).OutletNode = 0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRate = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletTemp = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletHumRat = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletEnthalpy = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).OutletPressure = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletTemp = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletHumRat = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletEnthalpy = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletPressure = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).ZoneTemp = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).ZoneHumRat = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).ZoneEnthalpy = 0.0;

			for ( NodeNum = 1; NodeNum <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeNum ) {

				ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeNum ) = GetOnlySingleNode( AlphArray( 4 + NodeNum ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			}

		} // end AirLoopHVAC:SupplyPlenum Loop

		AlphArray.deallocate();
		NumArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();
		NodeNums.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirZoneReturnPlenum( int const ZonePlenumNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the ZonePlenum components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int InducedNode( 0 );
		int InletNodeLoop;
		int ZoneNodeNum;
		int NodeNum;
		int ZonePlenumLoop;
		int PlenumZoneNum;
		int ZoneEquipConfigLoop; // Loop number of ZoneEquipConfig derived type
		int ADUNum; // air distribution unit index
		int NumADUsToPlen; // number of ADUs that might leak to this plenum
		int ADUsToPlenIndex; // index of an ADU that might leak to this plenum in the plenum ADU list

		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyEnvrnFlag( true ); // InitAirZoneReturnPlenumEnvrnFlag
		// static bool MyOneTimeFlag( true ); // InitAirZoneReturnPlenumOneTimeFlag
		////////////////////////////////////////////////////////////////////////////////////
		// FLOW:

		// Do the one time initializations
		if ( InitAirZoneReturnPlenumOneTimeFlag ) {

			// For each zone with a return air plenum put the ZoneRetPlenCond number for the return air plenum
			// in the ZoneEquipConfig array for the zone. This allows direct access of the zone's return air
			// plenum conditions, such as plenum temperature and air flow. Also establish and save connections
			// to the Air Distribution Units. This is needed for the simple duct leakage calculation.

			for ( ZonePlenumLoop = 1; ZonePlenumLoop <= NumZoneReturnPlenums; ++ZonePlenumLoop ) {
				ADUsToPlenIndex = 0;
				NumADUsToPlen = 0;
				if ( ZoneRetPlenCond( ZonePlenumLoop ).NumInletNodes > 0 ) {
					for ( InletNodeLoop = 1; InletNodeLoop <= ZoneRetPlenCond( ZonePlenumLoop ).NumInletNodes; ++InletNodeLoop ) {
						InletNode = ZoneRetPlenCond( ZonePlenumLoop ).InletNode( InletNodeLoop );
						// Loop through ZoneEquipConfig's and look for return air node value = InletNode
						for ( ZoneEquipConfigLoop = 1; ZoneEquipConfigLoop <= NumOfZones; ++ZoneEquipConfigLoop ) {
							if ( ! ZoneEquipConfig( ZoneEquipConfigLoop ).IsControlled ) continue;
							if ( ZoneEquipConfig( ZoneEquipConfigLoop ).ReturnAirNode == InletNode ) {
								ZoneEquipConfig( ZoneEquipConfigLoop ).ReturnZonePlenumCondNum = ZonePlenumLoop;
								ZoneRetPlenCond( ZonePlenumLoop ).ZoneEqNum( InletNodeLoop ) = ZoneEquipConfigLoop;
							}
						}
						// count the ADUs that can leak to this plenum
						for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
							if ( AirDistUnit( ADUNum ).ZoneEqNum == ZoneRetPlenCond( ZonePlenumLoop ).ZoneEqNum( InletNodeLoop ) ) {
								++NumADUsToPlen;
							}
						}
					}
				}
				ZoneRetPlenCond( ZonePlenumLoop ).ADUIndex.allocate( NumADUsToPlen );
				ZoneRetPlenCond( ZonePlenumLoop ).NumADUs = NumADUsToPlen;
				// fill the list of air distribution units that can leak to this plenum
				if ( NumADUsToPlen > 0 ) {
					for ( InletNodeLoop = 1; InletNodeLoop <= ZoneRetPlenCond( ZonePlenumLoop ).NumInletNodes; ++InletNodeLoop ) {
						for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
							if ( AirDistUnit( ADUNum ).ZoneEqNum == ZoneRetPlenCond( ZonePlenumLoop ).ZoneEqNum( InletNodeLoop ) ) {
								++ADUsToPlenIndex;
								ZoneRetPlenCond( ZonePlenumLoop ).ADUIndex( ADUsToPlenIndex ) = ADUNum;
							}
						}
					}
				}
			}

			InitAirZoneReturnPlenumOneTimeFlag = false;

		}

		// Do the Begin Environment initializations
		if ( InitAirZoneReturnPlenumEnvrnFlag && BeginEnvrnFlag ) {

			for ( PlenumZoneNum = 1; PlenumZoneNum <= NumZoneReturnPlenums; ++PlenumZoneNum ) {

				ZoneNodeNum = ZoneRetPlenCond( PlenumZoneNum ).ZoneNodeNum;
				Node( ZoneNodeNum ).Temp = 20.0;
				Node( ZoneNodeNum ).MassFlowRate = 0.0;
				Node( ZoneNodeNum ).Quality = 1.0;
				Node( ZoneNodeNum ).Press = OutBaroPress;
				Node( ZoneNodeNum ).HumRat = OutHumRat;
				Node( ZoneNodeNum ).Enthalpy = PsyHFnTdbW( Node( ZoneNodeNum ).Temp, Node( ZoneNodeNum ).HumRat );

			}

			InitAirZoneReturnPlenumEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			InitAirZoneReturnPlenumEnvrnFlag = true;
		}

		//Transfer the node data to ZoneRetPlenCond data structure
		for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++NodeNum ) {

			InletNode = ZoneRetPlenCond( ZonePlenumNum ).InletNode( NodeNum );
			// Set all of the inlet mass flow variables from the nodes
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate( NodeNum ) = Node( InletNode ).MassFlowRate;
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail( NodeNum ) = Node( InletNode ).MassFlowRateMaxAvail;
			ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail( NodeNum ) = Node( InletNode ).MassFlowRateMinAvail;
			//    ! Set all of the inlet state variables from the inlet nodes
			//    ZoneRetPlenCond(ZonePlenumNum)%InletTemp(NodeNum)         = Node(InletNode)%Temp
			//    ZoneRetPlenCond(ZonePlenumNum)%InletHumRat(NodeNum)       = Node(InletNode)%HumRat
			//    ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy(NodeNum)     = Node(InletNode)%Enthalpy
			ZoneRetPlenCond( ZonePlenumNum ).InletPressure( NodeNum ) = Node( InletNode ).Press;

		}

		ZoneNodeNum = ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum;
		// Set the induced air flow rates and conditions
		for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++NodeNum ) {
			InducedNode = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( NodeNum );
			ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( NodeNum ) = Node( InducedNode ).MassFlowRate;
			ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( NodeNum ) = Node( InducedNode ).MassFlowRateMaxAvail;
			ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( NodeNum ) = Node( InducedNode ).MassFlowRateMinAvail;

			ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( NodeNum ) = Node( ZoneNodeNum ).Temp;
			ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( NodeNum ) = Node( ZoneNodeNum ).HumRat;
			ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( NodeNum ) = Node( ZoneNodeNum ).Enthalpy;
			ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( NodeNum ) = Node( ZoneNodeNum ).Press;
			if ( Contaminant.CO2Simulation ) {
				ZoneRetPlenCond( ZonePlenumNum ).InducedCO2( NodeNum ) = Node( ZoneNodeNum ).CO2;
			}
			if ( Contaminant.GenericContamSimulation ) {
				ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam( NodeNum ) = Node( ZoneNodeNum ).GenContam;
			}
		}

		// Add stuff to calculate conduction inputs to the zone plenum
		// Now load the zone conditions
		ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp = Node( ZoneNodeNum ).Temp;
		ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat = Node( ZoneNodeNum ).HumRat;
		ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy = Node( ZoneNodeNum ).Enthalpy;

	}

	void
	InitAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool const FirstHVACIteration,
		bool const FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the ZonePlenum components.

		// METHODOLOGY EMPLOYED:
		// Similar to the Zone Splitter component but with interactions to the plenum zone.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int ZoneNodeNum;
		int PlenumZoneNum;
		int NodeIndex;

		static bool MyEnvrnFlag( true );
		// FLOW:

		// Do the Begin Environment initializations
		if ( MyEnvrnFlag && BeginEnvrnFlag ) {

			for ( PlenumZoneNum = 1; PlenumZoneNum <= NumZoneSupplyPlenums; ++PlenumZoneNum ) {

				ZoneNodeNum = ZoneSupPlenCond( PlenumZoneNum ).ZoneNodeNum;
				Node( ZoneNodeNum ).Temp = 20.0;
				Node( ZoneNodeNum ).MassFlowRate = 0.0;
				Node( ZoneNodeNum ).Quality = 1.0;
				Node( ZoneNodeNum ).Press = OutBaroPress;
				Node( ZoneNodeNum ).HumRat = OutHumRat;
				Node( ZoneNodeNum ).Enthalpy = PsyHFnTdbW( Node( ZoneNodeNum ).Temp, Node( ZoneNodeNum ).HumRat );

			}

			MyEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.

		InletNode = ZoneSupPlenCond( ZonePlenumNum ).InletNode;
		ZoneNodeNum = ZoneSupPlenCond( ZonePlenumNum ).ZoneNodeNum;

		if ( FirstHVACIteration && FirstCall ) {
			if ( Node( InletNode ).MassFlowRate > 0.0 ) {
				Node( ZoneNodeNum ).MassFlowRate = Node( InletNode ).MassFlowRate;
				for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
					OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
					Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate / ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes;
				}
			}
			if ( Node( InletNode ).MassFlowRateMaxAvail > 0.0 ) {
				Node( ZoneNodeNum ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRateMaxAvail;
				for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
					OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
					Node( OutletNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRateMaxAvail / ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes;
				}
			}

		} //For FirstHVACIteration and FirstCall

		if ( FirstCall ) {

			if ( Node( InletNode ).MassFlowRateMaxAvail == 0.0 ) { //For Node inlet Max Avail = 0.0

				for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
					OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
					Node( OutletNode ).MassFlowRate = 0.0;
					Node( OutletNode ).MassFlowRateMaxAvail = 0.0;
					Node( OutletNode ).MassFlowRateMinAvail = 0.0;
				}

				Node( ZoneNodeNum ).MassFlowRate = 0.0;
				Node( ZoneNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneNodeNum ).MassFlowRateMinAvail = 0.0;

			} //For Node inlet Max Avail = 0.0

			// Add stuff to calculate conduction inputs to the zone plenum
			// Now load the zone conditions
			ZoneSupPlenCond( ZonePlenumNum ).ZoneTemp = Node( ZoneNodeNum ).Temp;
			ZoneSupPlenCond( ZonePlenumNum ).ZoneHumRat = Node( ZoneNodeNum ).HumRat;
			ZoneSupPlenCond( ZonePlenumNum ).ZoneEnthalpy = Node( ZoneNodeNum ).Enthalpy;

			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
				Node( OutletNode ).Press = Node( InletNode ).Press;
				Node( OutletNode ).Quality = Node( InletNode ).Quality;
			}

			Node( ZoneNodeNum ).Press = Node( InletNode ).Press;
			Node( ZoneNodeNum ).Quality = Node( InletNode ).Quality;

		} else { // On the second call from the ZoneEquipManager this is where the flows are passed back to
			// the supply plenum inlet.
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
				ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRate( NodeIndex ) = Node( OutletNode ).MassFlowRate;
				ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail( NodeIndex ) = Node( OutletNode ).MassFlowRateMaxAvail;
				ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail( NodeIndex ) = Node( OutletNode ).MassFlowRateMinAvail;
			}

		} //For FirstCall

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirZoneReturnPlenum( int const ZonePlenumNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNodeNum( 0 ); // inlet node number
		int IndNum( 0 ); // induced air index
		int ADUNum( 0 ); // air distribution unit number
		int ADUListIndex( 0 ); // air distribution unit index in zone return plenum data structure
		Real64 TotIndMassFlowRate( 0.0 ); // total induced air mass flow rate [kg/s]

		// Reset the totals to zero before they are summed.
		ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletTemp = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletHumRat = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletPressure = 0.0;
		ZoneRetPlenCond( ZonePlenumNum ).OutletEnthalpy = 0.0;
		TotIndMassFlowRate = 0.0;

		for ( InletNodeNum = 1; InletNodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++InletNodeNum ) {
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate += ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate( InletNodeNum );
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail += ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail( InletNodeNum );
			ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail += ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail( InletNodeNum );
		}

		if ( ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate > 0.0 ) {

			// "Momentum balance" to get outlet air pressure
			for ( InletNodeNum = 1; InletNodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++InletNodeNum ) {

				ZoneRetPlenCond( ZonePlenumNum ).OutletPressure += ZoneRetPlenCond( ZonePlenumNum ).InletPressure( InletNodeNum ) * ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate( InletNodeNum ) / ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate;
			}

		} else {
			// Mass Flow in air loop is zero and loop is not operating.
			// Arbitrarily set the output to the first inlet leg
			ZoneRetPlenCond( ZonePlenumNum ).OutletPressure = ZoneRetPlenCond( ZonePlenumNum ).InletPressure( 1 );
		}

		// add in the leak flow rate, if any. Don't alter the pressure calc (it is not used anyway)
		for ( ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond( ZonePlenumNum ).NumADUs; ++ADUListIndex ) {
			ADUNum = ZoneRetPlenCond( ZonePlenumNum ).ADUIndex( ADUListIndex );
			if ( AirDistUnit( ADUNum ).UpStreamLeak || AirDistUnit( ADUNum ).DownStreamLeak ) {
				ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate += AirDistUnit( ADUNum ).MassFlowRateUpStrLk + AirDistUnit( ADUNum ).MassFlowRateDnStrLk;
				ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail += AirDistUnit( ADUNum ).MaxAvailDelta;
				ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail += AirDistUnit( ADUNum ).MinAvailDelta;
			}
		}
		// Sum up induced air flow rate
		for ( IndNum = 1; IndNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++IndNum ) {
			TotIndMassFlowRate += ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( IndNum );
		}

		ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate -= TotIndMassFlowRate;

		// Set the Plenum Outlet to the Zone Node conditions
		ZoneRetPlenCond( ZonePlenumNum ).OutletHumRat = ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat;
		ZoneRetPlenCond( ZonePlenumNum ).OutletEnthalpy = ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy;
		ZoneRetPlenCond( ZonePlenumNum ).OutletTemp = ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp;
		// make sure the MassFlowMaxAvail >= MassFlowRate
		ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail = max( ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail, ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate );

	}

	void
	CalcAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool const FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Similar to the Zone Splitter component but with interactions to the plenum zone.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NodeIndex;

		// The first time through the State properties are passed through
		if ( FirstCall ) {
			// Moisture balance to get outlet air humidity ratio
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				ZoneSupPlenCond( ZonePlenumNum ).OutletHumRat( NodeIndex ) = ZoneSupPlenCond( ZonePlenumNum ).ZoneHumRat;
			}

			// Energy balance to get outlet air enthalpy
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				ZoneSupPlenCond( ZonePlenumNum ).OutletEnthalpy( NodeIndex ) = ZoneSupPlenCond( ZonePlenumNum ).ZoneEnthalpy;
			}

			// Set outlet temperatures equal to inlet temperature
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				ZoneSupPlenCond( ZonePlenumNum ).OutletTemp( NodeIndex ) = ZoneSupPlenCond( ZonePlenumNum ).ZoneTemp;
			}

		} else {
			// This is the second time through and this is where the mass flows from the outlets are
			// summed and then assigned upstream to the inlet node.
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail = 0.0;
			ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail = 0.0;
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate += ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRate( NodeIndex );
				ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail += ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail( NodeIndex );
				ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail += ZoneSupPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail( NodeIndex );
			}
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the ZonePlenum Module
	// *****************************************************************************

	void
	UpdateAirZoneReturnPlenum( int const ZonePlenumNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;
		int ZoneNode;
		int InletNodeNum;
		int InducedNode; // the node number of an induced air outlet node
		int IndNum; // the induced air outlet index in ZoneRetPlenCond

		OutletNode = ZoneRetPlenCond( ZonePlenumNum ).OutletNode;
		InletNode = ZoneRetPlenCond( ZonePlenumNum ).InletNode( 1 );
		ZoneNode = ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum;

		// Set the outlet air nodes of the ZonePlenum
		Node( OutletNode ).MassFlowRate = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate;
		Node( OutletNode ).MassFlowRateMaxAvail = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail;
		Node( OutletNode ).MassFlowRateMinAvail = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail;

		Node( ZoneNode ).MassFlowRate = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate;
		Node( ZoneNode ).MassFlowRateMaxAvail = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMaxAvail;
		Node( ZoneNode ).MassFlowRateMinAvail = ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRateMinAvail;
		Node( ZoneNode ).Press = ZoneRetPlenCond( ZonePlenumNum ).OutletPressure;

		Node( OutletNode ).Temp = ZoneRetPlenCond( ZonePlenumNum ).OutletTemp;
		Node( OutletNode ).HumRat = ZoneRetPlenCond( ZonePlenumNum ).OutletHumRat;
		Node( OutletNode ).Enthalpy = ZoneRetPlenCond( ZonePlenumNum ).OutletEnthalpy;
		Node( OutletNode ).Press = ZoneRetPlenCond( ZonePlenumNum ).OutletPressure;
		for ( IndNum = 1; IndNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++IndNum ) {
			InducedNode = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( IndNum );
			Node( InducedNode ).Temp = ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( IndNum );
			Node( InducedNode ).HumRat = ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( IndNum );
			Node( InducedNode ).Enthalpy = ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( IndNum );
			Node( InducedNode ).Press = ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( IndNum );
			if ( Contaminant.CO2Simulation ) {
				Node( InducedNode ).CO2 = ZoneRetPlenCond( ZonePlenumNum ).InducedCO2( IndNum );
			}
			if ( Contaminant.GenericContamSimulation ) {
				Node( InducedNode ).GenContam = ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam( IndNum );
			}
			Node( InducedNode ).Quality = Node( InletNode ).Quality;
		}

		// Set the outlet nodes for properties that are just pass through and not used
		Node( OutletNode ).Quality = Node( InletNode ).Quality;
		Node( ZoneNode ).Quality = Node( InletNode ).Quality;

		// Set the outlet node contaminant properties if needed. The zone contaminant conditions are calculated in ZoneContaminantPredictorCorrector
		if ( Contaminant.CO2Simulation ) {
			if ( ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate > 0.0 ) {
				// CO2 balance to get outlet air CO2
				Node( OutletNode ).CO2 = 0.0;
				for ( InletNodeNum = 1; InletNodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++InletNodeNum ) {
					Node( OutletNode ).CO2 += Node( ZoneRetPlenCond( ZonePlenumNum ).InletNode( InletNodeNum ) ).CO2 * ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate( InletNodeNum ) / ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate;
				}
				Node( ZoneNode ).CO2 = Node( OutletNode ).CO2;
			} else {
				Node( OutletNode ).CO2 = Node( ZoneNode ).CO2;
			}
		}
		if ( Contaminant.GenericContamSimulation ) {
			if ( ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate > 0.0 ) {
				// GenContam balance to get outlet air GenContam
				Node( OutletNode ).GenContam = 0.0;
				for ( InletNodeNum = 1; InletNodeNum <= ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes; ++InletNodeNum ) {
					Node( OutletNode ).GenContam += Node( ZoneRetPlenCond( ZonePlenumNum ).InletNode( InletNodeNum ) ).GenContam * ZoneRetPlenCond( ZonePlenumNum ).InletMassFlowRate( InletNodeNum ) / ZoneRetPlenCond( ZonePlenumNum ).OutletMassFlowRate;
				}
				Node( ZoneNode ).GenContam = Node( OutletNode ).GenContam;
			} else {
				Node( OutletNode ).GenContam = Node( ZoneNode ).GenContam;
			}
		}

	}

	void
	UpdateAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool & PlenumInletChanged,
		bool const FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Similar to the Zone Splitter component but with interactions to the plenum zone.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const FlowRateToler( 0.01 ); // Tolerance for mass flow rate convergence (in kg/s)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;
		int ZoneNode;
		int NodeIndex;

		OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( 1 );
		InletNode = ZoneSupPlenCond( ZonePlenumNum ).InletNode;
		ZoneNode = ZoneSupPlenCond( ZonePlenumNum ).ZoneNodeNum;

		// On the FirstCall the State properties are passed through and the mass flows are not dealt with
		if ( FirstCall ) {
			// Set the outlet nodes for properties that just pass through and not used
			for ( NodeIndex = 1; NodeIndex <= ZoneSupPlenCond( ZonePlenumNum ).NumOutletNodes; ++NodeIndex ) {
				OutletNode = ZoneSupPlenCond( ZonePlenumNum ).OutletNode( NodeIndex );
				Node( OutletNode ).Temp = ZoneSupPlenCond( ZonePlenumNum ).OutletTemp( NodeIndex );
				Node( OutletNode ).HumRat = ZoneSupPlenCond( ZonePlenumNum ).OutletHumRat( NodeIndex );
				Node( OutletNode ).Enthalpy = ZoneSupPlenCond( ZonePlenumNum ).OutletEnthalpy( NodeIndex );
				if ( Contaminant.CO2Simulation ) {
					Node( OutletNode ).CO2 = Node( InletNode ).CO2;
				}
				if ( Contaminant.GenericContamSimulation ) {
					Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
				}
			}

			if ( Contaminant.CO2Simulation ) {
				Node( ZoneNode ).CO2 = Node( InletNode ).CO2;
			}
			if ( Contaminant.GenericContamSimulation ) {
				Node( ZoneNode ).GenContam = Node( InletNode ).GenContam;
			}

		} else {
			// The second time through just updates the mass flow conditions back upstream
			// to the inlet.

			if ( std::abs( Node( InletNode ).MassFlowRate - ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate ) > FlowRateToler ) {
				PlenumInletChanged = true;
			}

			Node( InletNode ).MassFlowRate = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate;
			Node( InletNode ).MassFlowRateMaxAvail = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail;
			Node( InletNode ).MassFlowRateMinAvail = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail;

			Node( ZoneNode ).MassFlowRate = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRate;
			Node( ZoneNode ).MassFlowRateMaxAvail = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMaxAvail;
			Node( ZoneNode ).MassFlowRateMinAvail = ZoneSupPlenCond( ZonePlenumNum ).InletMassFlowRateMinAvail;

		} //For FirstCall

	}

	//        End of Update subroutines for the ZonePlenum Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the ZonePlenum Module
	// *****************************************************************************

	void
	ReportZoneReturnPlenum( int const EP_UNUSED( ZonePlenumNum ) ) // unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// Write(*,*)=ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumPower    Still needs to report the ZonePlenum power from this component

		//ZoneRetPlenCond(ZonePlenumNum)% =

	}

	void
	ReportZoneSupplyPlenum( int const EP_UNUSED( ZonePlenumNum ) ) // unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// Write(*,*)=ZoneSupPlenCond(ZonePlenumNum)%ZonePlenumPower    Still needs to report the ZonePlenum power from this component

		//ZoneSupPlenCond(ZonePlenumNum)% =

	}

	//        End of Reporting subroutines for the ZonePlenum Module
	// *****************************************************************************

} // ZonePlenum

} // EnergyPlus
