// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <OutAirNodeManager.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace OutAirNodeManager {
	// Module containing the routines that deal with the outside air nodes

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   September 1998
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and update the conditions for all the outside
	// air nodes in the problem.

	// METHODOLOGY EMPLOYED:
	// Outside air nodes provide the connection to outside conditions for the
	// EnergyPlus HVAC simulation. The  list of air nodes specified in the input
	// file will be read in. Each air node will be updated to the outside environmental
	// conditions at the start of each EnergyPlus main time step.

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using DataContaminantBalance::Contaminant;
	using DataContaminantBalance::OutdoorCO2;
	using DataContaminantBalance::OutdoorGC;
	//USE DataHVACGlobals, ONLY: FirstTimeStepSysFlag

	// Data
	//MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	//Type declarations in OutAirNodeManager module

	//MODULE VARIABLE DECLARATIONS:

	Array1D_int OutsideAirNodeList; // List of all outside air inlet nodes
	int NumOutsideAirNodes( 0 ); // Number of single outside air nodes
	bool GetOutAirNodesInputFlag( true ); // Flag set to make sure you get input once

	//SUBROUTINE SPECIFICATIONS FOR MODULE OutAirNodeManager

	// Functions

	// Clears the global data in OutAirNodeManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		OutsideAirNodeList.deallocate();
		NumOutsideAirNodes = 0;
		GetOutAirNodesInputFlag = true;
	}

	void
	SetOutAirNodes()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Make sure the outside air nodes are prepared for the HVAC simulation

		// METHODOLOGY EMPLOYED:
		// Use appropriate flag to check for needed action

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( GetOutAirNodesInputFlag ) { // First time subroutine has been entered
			GetOutAirNodesInput(); // Get OutAir Nodes data
			GetOutAirNodesInputFlag = false;
		}
		InitOutAirNodes();

	}

	void
	GetOutAirNodesInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Read in the list of outside air nodes & store in array OutAirInletNodeList

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace NodeInputManager;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOutAirNodesInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumOutAirInletNodeLists;
		int NumOutsideAirNodeSingles;
		int NumNums; // Number of REAL(r64) numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int NumParams;
		Array1D_int NodeNums;
		int NumNodes;
		int IOStat; // Status flag from GetObjectItem
		int NodeNum; // index into NodeNums
		//  INTEGER :: OutAirNodeNum ! index into OutAirInletNodeList
		int OutAirInletNodeListNum; // OUTSIDE AIR INLET NODE LIST index
		int OutsideAirNodeSingleNum; // OUTSIDE AIR NODE index
		int AlphaNum; // index into Alphas
		int ListSize; // size of OutAirInletNodeList
		//  LOGICAL :: AlreadyInList ! flag used for checking for duplicate input
		bool ErrorsFound;
		bool ErrInList;
		int CurSize;
		int NextFluidStreamNum; // Fluid stream index (all outside air inlet nodes need a unique fluid stream number)
		Array1D_int TmpNums;
		std::string CurrentModuleObject; // Object type for getting and error messages
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a

		NumOutAirInletNodeLists = GetNumObjectsFound( "OutdoorAir:NodeList" );
		NumOutsideAirNodeSingles = GetNumObjectsFound( "OutdoorAir:Node" );
		NumOutsideAirNodes = 0;
		ErrorsFound = false;
		NextFluidStreamNum = 1;

		ListSize = 0;
		CurSize = 100;
		TmpNums.dimension( CurSize, 0 );

		GetObjectDefMaxArgs( "NodeList", NumParams, NumAlphas, NumNums );
		NodeNums.dimension( NumParams, 0 );

		GetObjectDefMaxArgs( "OutdoorAir:NodeList", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "OutdoorAir:Node", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		if ( NumOutAirInletNodeLists > 0 ) {
			// Loop over all outside air inlet nodes in the input and count them
			CurrentModuleObject = "OutdoorAir:NodeList";
			for ( OutAirInletNodeListNum = 1; OutAirInletNodeListNum <= NumOutAirInletNodeLists; ++OutAirInletNodeListNum ) {
				GetObjectItem( CurrentModuleObject, OutAirInletNodeListNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				for ( AlphaNum = 1; AlphaNum <= NumAlphas; ++AlphaNum ) {
					ErrInList = false;
					//  To support HVAC diagram, every outside inlet node must have a unique fluid stream number
					//  GetNodeNums will increment the value across a node list, the starting value must be incremented
					//  here across lists and across objects
					GetNodeNums( Alphas( AlphaNum ), NumNodes, NodeNums, ErrInList, NodeType_Air, CurrentModuleObject, CurrentModuleObject, NodeConnectionType_OutsideAir, NextFluidStreamNum, ObjectIsNotParent, IncrementFluidStreamYes, cAlphaFields( AlphaNum ) );
					NextFluidStreamNum += NumNodes;
					if ( ErrInList ) {
						ShowContinueError( "Occurred in " + CurrentModuleObject + ", " + cAlphaFields( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					}
					for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
						// Duplicates here are not a problem, just ignore
						if ( ! any_eq( TmpNums, NodeNums( NodeNum ) ) ) {
							++ListSize;
							if ( ListSize > CurSize ) {
								TmpNums.redimension( CurSize += 100, 0 );
							}
							TmpNums( ListSize ) = NodeNums( NodeNum );
						}
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input." );
			}
		}

		if ( NumOutsideAirNodeSingles > 0 ) {
			// Loop over all single outside air nodes in the input
			CurrentModuleObject = "OutdoorAir:Node";
			for ( OutsideAirNodeSingleNum = 1; OutsideAirNodeSingleNum <= NumOutsideAirNodeSingles; ++OutsideAirNodeSingleNum ) {
				GetObjectItem( CurrentModuleObject, OutsideAirNodeSingleNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				ErrInList = false;
				//  To support HVAC diagram, every outside inlet node must have a unique fluid stream number
				//  GetNodeNums will increment the value across a node list, the starting value must be incremented
				//  here across lists and across objects
				GetNodeNums( Alphas( 1 ), NumNodes, NodeNums, ErrInList, NodeType_Air, CurrentModuleObject, CurrentModuleObject, NodeConnectionType_OutsideAir, NextFluidStreamNum, ObjectIsNotParent, IncrementFluidStreamYes, cAlphaFields( 1 ) );
				NextFluidStreamNum += NumNodes;
				if ( ErrInList ) {
					ShowContinueError( "Occurred in " + CurrentModuleObject + ", " + cAlphaFields( 1 ) + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}

				if ( NumNodes > 1 ) {
					ShowSevereError( CurrentModuleObject + ", " + cAlphaFields( 1 ) + " = " + Alphas( 1 ) );
					ShowContinueError( "...appears to point to a node list, not a single node." );
					ErrorsFound = true;
					continue;
				}

				if ( ! any_eq( TmpNums, NodeNums( 1 ) ) ) {
					++ListSize;
					if ( ListSize > CurSize ) {
						TmpNums.redimension( CurSize += 100, 0 );
					}
					TmpNums( ListSize ) = NodeNums( 1 );
				} else { // Duplicates are a problem
					ShowSevereError( CurrentModuleObject + ", duplicate " + cAlphaFields( 1 ) + " = " + Alphas( 1 ) );
					ShowContinueError( "Duplicate " + cAlphaFields( 1 ) + " might be found in an OutdoorAir:NodeList." );
					ErrorsFound = true;
					continue;
				}

				// Set additional node properties
				if ( NumNums > 0 ) Node( NodeNums( 1 ) ).Height = Numbers( 1 );

			}

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input." );
			}
		}

		if ( ListSize > 0 ) {
			NumOutsideAirNodes = ListSize;
			OutsideAirNodeList = TmpNums( {1,ListSize} );
		}
	}

	void
	InitOutAirNodes()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Sept 1998
		//       MODIFIED       B. Griffith, added EMS override
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize the outside air node data data.  In Particular,
		// set the outside air nodes to the outside conditions at the
		// start of every heat balance time step.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyWFnTdbTwbPb;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutsideAirNodeNum;
		int NodeNum;

		// Do the begin time step initialization
		for ( OutsideAirNodeNum = 1; OutsideAirNodeNum <= NumOutsideAirNodes; ++OutsideAirNodeNum ) {
			NodeNum = OutsideAirNodeList( OutsideAirNodeNum );
			if ( Node( NodeNum ).Height < 0.0 ) {
				// Note -- this setting is different than the DataEnvironment "AT" settings.
				Node( NodeNum ).OutAirDryBulb = OutDryBulbTemp;
				Node( NodeNum ).OutAirWetBulb = OutWetBulbTemp;
			} else {
				Node( NodeNum ).OutAirDryBulb = OutDryBulbTempAt( Node( NodeNum ).Height );
				Node( NodeNum ).OutAirWetBulb = OutWetBulbTempAt( Node( NodeNum ).Height );
			}

			if ( Node( NodeNum ).EMSOverrideOutAirDryBulb ) Node( NodeNum ).OutAirDryBulb = Node( NodeNum ).EMSValueForOutAirDryBulb;

			if ( Node( NodeNum ).EMSOverrideOutAirWetBulb ) {
				Node( NodeNum ).OutAirWetBulb = Node( NodeNum ).EMSValueForOutAirWetBulb;
				Node( NodeNum ).HumRat = PsyWFnTdbTwbPb( Node( NodeNum ).OutAirDryBulb, Node( NodeNum ).OutAirWetBulb, OutBaroPress );
				Node( NodeNum ).Enthalpy = PsyHFnTdbW( Node( NodeNum ).OutAirDryBulb, Node( NodeNum ).HumRat );
			} else {
				Node( NodeNum ).HumRat = OutHumRat;
				Node( NodeNum ).Enthalpy = PsyHFnTdbW( Node( NodeNum ).OutAirDryBulb, OutHumRat );
			}

			Node( NodeNum ).Temp = Node( NodeNum ).OutAirDryBulb;
			Node( NodeNum ).Press = OutBaroPress;
			Node( NodeNum ).Quality = 0.0;
			// Add contaminants
			if ( Contaminant.CO2Simulation ) Node( NodeNum ).CO2 = OutdoorCO2;
			if ( Contaminant.GenericContamSimulation ) Node( NodeNum ).GenContam = OutdoorGC;
		}

	}

	bool
	CheckOutAirNodeNumber( int const NodeNumber ) // Number of node to check to see if in Outside Air list
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Feb 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provide a entry into the OutAirNode List for checking from other routines.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool Okay; // True if found, false if not

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetOutAirNodesInputFlag ) { // First time subroutine has been entered
			GetOutAirNodesInput(); // Get Out Air Nodes data
			GetOutAirNodesInputFlag = false;
			SetOutAirNodes();
		}

		if ( any_eq( OutsideAirNodeList, NodeNumber ) ) {
			Okay = true;
		} else {
			Okay = false;
		}

		return Okay;

	}

	void
	CheckAndAddAirNodeNumber(
		int const NodeNumber, // Number of node to check to see if in Outside Air list
		bool & Okay // True if found, false if not
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// At the time of writing, some items (namely Chillers) have "made up" node
		// names for nodes that are "outside air nodes".  Rather than fatal out, add
		// this subroutine which will check and then add a outside air node, if necessary.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using namespace NodeInputManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_int TmpNums;
		int DummyNumber;
		static bool errFlag( false );

		if ( GetOutAirNodesInputFlag ) { // First time subroutine has been entered
			GetOutAirNodesInput(); // Get Out Air Nodes data
			GetOutAirNodesInputFlag = false;
			SetOutAirNodes();
		}

		Okay = false;

		if ( NumOutsideAirNodes > 0 ) {
			if ( any_eq( OutsideAirNodeList, NodeNumber ) ) {
				Okay = true;
			} else {
				Okay = false;
			}
		} else {
			Okay = false;
		}

		if ( NodeNumber > 0 ) {
			if ( ! Okay ) { // Add new outside air node to list
				OutsideAirNodeList.redimension( ++NumOutsideAirNodes );
				OutsideAirNodeList( NumOutsideAirNodes ) = NodeNumber;
				TmpNums = OutsideAirNodeList;
				//register new node..
				GetNodeNums( NodeID( NodeNumber ), DummyNumber, TmpNums, errFlag, NodeType_Air, "OutdoorAir:Node", "OutdoorAir:Node", NodeConnectionType_OutsideAir, NumOutsideAirNodes, ObjectIsNotParent, IncrementFluidStreamYes );
				if ( Node( NodeNumber ).Height < 0.0 ) {
					Node( NodeNumber ).OutAirDryBulb = OutDryBulbTemp;
					Node( NodeNumber ).OutAirWetBulb = OutWetBulbTemp;
				} else {
					Node( NodeNumber ).OutAirDryBulb = OutDryBulbTempAt( Node( NodeNumber ).Height );
					Node( NodeNumber ).OutAirWetBulb = OutWetBulbTempAt( Node( NodeNumber ).Height );
				}
				Node( NodeNumber ).Temp = Node( NodeNumber ).OutAirDryBulb;
				Node( NodeNumber ).HumRat = OutHumRat;
				Node( NodeNumber ).Enthalpy = PsyHFnTdbW( Node( NodeNumber ).Temp, OutHumRat );
				Node( NodeNumber ).Press = OutBaroPress;
				Node( NodeNumber ).Quality = 0.0;
				// Add contaminants
				if ( Contaminant.CO2Simulation ) Node( NodeNumber ).CO2 = OutdoorCO2;
				if ( Contaminant.GenericContamSimulation ) Node( NodeNumber ).GenContam = OutdoorGC;
			}
		}

	}

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // OutAirNodeManager

} // EnergyPlus
