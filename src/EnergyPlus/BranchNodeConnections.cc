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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataBranchNodeConnections.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace BranchNodeConnections {

	// Module containing the routines dealing with the Branch/Node Connections (CompSets, etc)

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   May 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module encapsulates the connection data necessary for some of the checks
	// needed in the branch-node data

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using DataGlobals::OutputFileDebug;
	using namespace DataLoopNode;
	using namespace DataBranchNodeConnections;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Functions

	void
	RegisterNodeConnection(
		int const NodeNumber, // Number for this Node
		std::string const & NodeName, // Name of this Node
		std::string const & ObjectType, // Type of object this Node is connected to (e.g. Chiller:Electric)
		std::string const & ObjectName, // Name of object this Node is connected to (e.g. MyChiller)
		std::string const & ConnectionType, // Connection Type for this Node (must be valid)
		int const FluidStream, // Count on Fluid Streams
		bool const IsParent, // True when node is a parent node
		bool & errFlag, // Will be True if errors already detected or if errors found here
		Optional_string_const InputFieldName // Input Field Name
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine registers a node connection in the Node Connection data structure.  This
		// structure is intended to help with HVAC diagramming as well as validation of nodes.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "RegisterNodeConnection: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorsFoundHere;
		int Count;
		bool MakeNew;
		int Found;

		ErrorsFoundHere = false;
		if ( ! IsValidConnectionType( ConnectionType ) ) {
			ShowSevereError( RoutineName + "Invalid ConnectionType=" + ConnectionType );
			ShowContinueError( "Occurs for Node=" + NodeName + ", ObjectType=" + ObjectType + ", ObjectName=" + ObjectName );
			ErrorsFoundHere = true;
		}

		MakeNew = true;
		for ( Count = 1; Count <= NumOfNodeConnections; ++Count ) {
			if ( NodeConnections( Count ).NodeNumber != NodeNumber ) continue;
			if ( ! SameString( NodeConnections( Count ).ObjectType, ObjectType ) ) continue;
			if ( ! SameString( NodeConnections( Count ).ObjectName, ObjectName ) ) continue;
			if ( ! SameString( NodeConnections( Count ).ConnectionType, ConnectionType ) ) continue;
			if ( NodeConnections( Count ).FluidStream != FluidStream ) continue;
			if ( ( NodeConnections( Count ).ObjectIsParent && ! IsParent ) || ( ! NodeConnections( Count ).ObjectIsParent && IsParent ) ) {
				ShowSevereError( RoutineName + "Node registered for both Parent and \"not\" Parent" );
				ShowContinueError( "Occurs for Node=" + NodeName + ", ObjectType=" + ObjectType + ", ObjectName=" + ObjectName );
				ErrorsFoundHere = true;
			}
			MakeNew = false;
		}
		if ( MakeNew ) {
			++NumOfNodeConnections;
			if ( NumOfNodeConnections > 1 && NumOfNodeConnections > MaxNumOfNodeConnections ) {
				NodeConnections.redimension( MaxNumOfNodeConnections += NodeConnectionAlloc );
			} else if ( NumOfNodeConnections == 1 ) {
				NodeConnections.allocate( NodeConnectionAlloc );
				MaxNumOfNodeConnections = NodeConnectionAlloc;
			}

			NodeConnections( NumOfNodeConnections ).NodeNumber = NodeNumber;
			NodeConnections( NumOfNodeConnections ).NodeName = NodeName;
			NodeConnections( NumOfNodeConnections ).ObjectType = MakeUPPERCase( ObjectType );
			NodeConnections( NumOfNodeConnections ).ObjectName = ObjectName;
			NodeConnections( NumOfNodeConnections ).ConnectionType = ConnectionType;
			NodeConnections( NumOfNodeConnections ).FluidStream = FluidStream;
			NodeConnections( NumOfNodeConnections ).ObjectIsParent = IsParent;

		}

		if ( has_prefixi( ObjectType, "AirTerminal:" ) ) {
			if ( present( InputFieldName ) ) {
				++NumOfAirTerminalNodes;
				if ( NumOfAirTerminalNodes > 1 && NumOfAirTerminalNodes > MaxNumOfAirTerminalNodes ) {
					AirTerminalNodeConnections.redimension( MaxNumOfAirTerminalNodes += EqNodeConnectionAlloc );
				} else if ( NumOfAirTerminalNodes == 1 ) {
					AirTerminalNodeConnections.allocate( EqNodeConnectionAlloc );
					MaxNumOfAirTerminalNodes = EqNodeConnectionAlloc;
				}

				// Check out AirTerminal inlet/outlet nodes
				Found = FindItemInList( NodeName, AirTerminalNodeConnections, &EqNodeConnectionDef::NodeName, NumOfAirTerminalNodes - 1 );
				if ( Found != 0 ) { // Nodename already used
					ShowSevereError( RoutineName + ObjectType + "=\"" + ObjectName + "\" node name duplicated." );
					ShowContinueError( "NodeName=\"" + NodeName + "\", entered as type=" + ConnectionType );
					ShowContinueError( "In Field=" + InputFieldName() );
					ShowContinueError( "Already used in " + AirTerminalNodeConnections( Found ).ObjectType + "=\"" + AirTerminalNodeConnections( Found ).ObjectName + "\"." );
					ShowContinueError( " as type=" + AirTerminalNodeConnections( Found ).ConnectionType + ", In Field=" + AirTerminalNodeConnections( Found ).InputFieldName );
					ErrorsFoundHere = true;
				} else {
					AirTerminalNodeConnections( NumOfAirTerminalNodes ).NodeName = NodeName;
					AirTerminalNodeConnections( NumOfAirTerminalNodes ).ObjectType = ObjectType;
					AirTerminalNodeConnections( NumOfAirTerminalNodes ).ObjectName = ObjectName;
					AirTerminalNodeConnections( NumOfAirTerminalNodes ).ConnectionType = ConnectionType;
					AirTerminalNodeConnections( NumOfAirTerminalNodes ).InputFieldName = InputFieldName;
				}
			} else {
				ShowSevereError( RoutineName + ObjectType + ", Developer Error: Input Field Name not included." );
				ShowContinueError( "Node names not checked for duplication." );
			}
		}

		if ( ErrorsFoundHere ) {
			errFlag = true;
		}

	}

	bool
	IsValidConnectionType( std::string const & ConnectionType )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function determines if a connection type is valid.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool IsValid;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Count;

		IsValid = false;
		for ( Count = 1; Count <= NumValidConnectionTypes; ++Count ) {
			if ( ConnectionType != ValidConnectionTypes( Count ) ) continue;
			IsValid = true;
			break;
		}

		return IsValid;

	}

	void
	CheckNodeConnections( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes the node connection data structure looking at:
		// 1.  In the NodeConnections list, for any node which appears as a sensor or an
		// actuator, the same node must also appear in the connections list at least once
		// as a node type which is not sensor or actuator or outsideair.
		// 2.  In the NodeConnections list, for any node which appears as a setpoint, the
		// same node must also appear in the connections list at least once as a node type
		// which is not a setpoint or outsideair.
		// 3.  Every ZoneInlet must appear as an outlet from something, otherwise it will
		// do nothing.
		// 4.  Every ZoneExhaust must appear as an inlet to something,
		// otherwise it will do nothing.
		// 5.  Every inlet node should match either an Outlet, ZoneReturn, ZoneExhaust, ReliefAir,
		// or OutsideAir node.
		//  With the current data structure, when checking inlets:
		//    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
		//    b)  If an InletNode's object is not one of the above types, it is valid if the
		//        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
		// 6.  Any given node can only be an inlet once in the list of Non-Parent Node Connections
		// 7.  Any given node can only be an outlet once in the list of Non-Parent Node Connections
		// 8.  non-parent outlet nodes -- must never be an outlet more than once
		// 9.  nodes of type OutsideAirReference must be registered as NodeConnectionType_OutsideAir
		// 10. fluid streams cannot have multiple inlet/outlet nodes on same component
		// 11. zone nodes may not be used as anything else except as a setpoint, sensor or actuator node

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop1;
		int Loop2;
		bool IsValid;
		bool IsInlet;
		bool IsOutlet;
		bool MatchedAtLeastOne;
		int ErrorCounter;
		int Object;
		int StartConnect;
		int EndConnect;
		Array1D_int FluidStreamInletCount;
		Array1D_int FluidStreamOutletCount;
		Array1D_int NodeObjects;
		Array1D_bool FluidStreamCounts;
		int NumObjects;
		int MaxFluidStream;

		ErrorCounter = 0;
		//  Check 1 -- check sensor and actuator nodes
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Sensor ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Actuator ) ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Sensor ) ) continue;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Sensor node did not find a matching node of appropriate type (other than Actuator or Sensor)." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				ErrorsFound = true;
			}
		}

		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Actuator ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Actuator ) ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Sensor ) ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_OutsideAir ) ) continue;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Actuator node did not find a matching node of appropriate type (other than Actuator, Sensor, OutsideAir)." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				ErrorsFound = true;
			}
		}

		// Check 2 -- setpoint nodes
		// Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_SetPoint ) ) continue;
			IsValid = false;
			IsInlet = false;
			IsOutlet = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_SetPoint ) ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_OutsideAir ) ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Inlet ) ) IsInlet = true;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Outlet ) ) IsOutlet = true;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Setpoint node did not find a matching node of appropriate type (other than Setpoint, OutsideAir)." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				ErrorsFound = true;
			}
			if ( ! IsInlet && ! IsOutlet ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Setpoint node did not find a matching node of type Inlet or Outlet." );
				ShowContinueError( "It appears this node is not part of the HVAC system." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				//      ErrorsFound=.TRUE.
			}
		}

		// Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
		//  DO Loop1=1,NumOfNodeConnections
		//    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_SetPoint)) CYCLE
		//    IsValid=.FALSE.
		//    IsInlet=.FALSE.
		//    IsOutlet=.FALSE.
		//    DO Loop2=1, NumOfNodeConnections
		//      IF (Loop1 == Loop2) CYCLE
		//      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
		//      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet)) IsInlet=.TRUE.
		//      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet)) IsOutlet=.TRUE.
		//      IF (IsInlet .or. IsOutlet) EXIT
		//    ENDDO
		//    IF (.not. IsInlet .and. .not. IsOutlet) THEN
		//      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
		//            '", Setpoint node did not find a matching node of type Inlet or Outlet.')
		//      CALL ShowContinueError('It appears this node is not part of the HVAC system.')
		//      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
		//             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
		//      ErrorCounter=ErrorCounter+1
		//      ErrorsFound=.TRUE.
		//    ENDIF
		//  ENDDO

		// Check 3 -- zone inlet nodes -- must be an outlet somewhere
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_ZoneInlet ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Outlet ) ) continue;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", ZoneInlet node did not find an outlet node." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				//      ErrorsFound=.TRUE.
			}
		}

		// Check 4 -- zone exhaust nodes -- must be an inlet somewhere
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_ZoneExhaust ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Inlet ) ) continue;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", ZoneExhaust node did not find a matching inlet node." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				//      ErrorsFound=.TRUE.
			}
		}

		// Check 5 -- return plenum induced air outlet nodes -- must be an inlet somewhere
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_InducedAir ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Inlet ) ) continue;
				IsValid = true;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Return plenum induced air outlet node did not find a matching inlet node." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				ErrorsFound = true;
			}
		}

		// Check 6 -- every inlet should have a matching outlet, zonereturn, zoneexhaust, induced air, reliefair or outsideair
		//    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
		//    b)  If an InletNode's object is not one of the above types, it is valid if the
		//        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Inlet ) ) continue;
			if ( NodeConnections( Loop1 ).ObjectType == "AIRLOOPHVAC" || NodeConnections( Loop1 ).ObjectType == "CONDENSERLOOP" || NodeConnections( Loop1 ).ObjectType == "PLANTLOOP" ) continue;
			IsValid = false;
			MatchedAtLeastOne = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Outlet ) || NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_ZoneReturn ) || NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_ZoneExhaust ) || NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_InducedAir ) || NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_ReliefAir ) || NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_OutsideAir ) ) {
					MatchedAtLeastOne = true;
					continue;
				}
				if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Inlet ) && ( NodeConnections( Loop2 ).ObjectType == "AIRLOOPHVAC" || NodeConnections( Loop2 ).ObjectType == "CONDENSERLOOP" || NodeConnections( Loop2 ).ObjectType == "PLANTLOOP" ) ) {
					MatchedAtLeastOne = true;
					continue;
				}
				IsValid = false;
			}
			if ( ! IsValid && ! MatchedAtLeastOne ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Inlet node did not find an appropriate matching \"outlet\" node." );
				ShowContinueError( "If this is an outdoor air inlet node, it must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				//      ErrorsFound=.TRUE.
			}
		}

		// Check 7 -- non-parent inlet nodes -- must never be an inlet more than once
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			// Only non-parent node connections
			if ( NodeConnections( Loop1 ).ObjectIsParent ) continue;
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Inlet ) ) continue;
			for ( Loop2 = Loop1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop2 ).ObjectIsParent ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Inlet ) ) continue;
				if ( NodeConnections( Loop2 ).NodeNumber == NodeConnections( Loop1 ).NodeNumber ) {
					ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", The same node appears as a non-parent Inlet node more than once." );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop2 ).ObjectType + ", Name=" + NodeConnections( Loop2 ).ObjectName );
					++ErrorCounter;
					//        ErrorsFound=.TRUE.
					break;
				}
			}
		}

		// Check 8 -- non-parent outlet nodes -- must never be an outlet more than once
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			// Only non-parent node connections
			if ( NodeConnections( Loop1 ).ObjectIsParent ) continue;
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Outlet ) ) continue;
			// Skip if DIRECT AIR, because it only has one node which is an outlet, so it dupes the outlet which feeds it
			if ( NodeConnections( Loop1 ).ObjectType == "AIRTERMINAL:SINGLEDUCT:UNCONTROLLED" ) continue;
			IsValid = true;
			for ( Loop2 = Loop1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop2 ).ObjectIsParent ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_Outlet ) ) continue;
				// Skip if DIRECT AIR, because it only has one node which is an outlet, so it dupes the outlet which feeds it
				if ( NodeConnections( Loop2 ).ObjectType == "AIRTERMINAL:SINGLEDUCT:UNCONTROLLED" ) continue;
				if ( NodeConnections( Loop2 ).NodeNumber == NodeConnections( Loop1 ).NodeNumber ) {
					// Skip if one of the
					ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", The same node appears as a non-parent Outlet node more than once." );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop2 ).ObjectType + ", Name=" + NodeConnections( Loop2 ).ObjectName );
					++ErrorCounter;
					//        ErrorsFound=.TRUE.
					break;
				}
			}
		}

		// Check 9 -- nodes of type OutsideAirReference must be registered as NodeConnectionType_OutsideAir
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_OutsideAirReference ) ) continue;
			IsValid = false;
			for ( Loop2 = 1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeNumber != NodeConnections( Loop2 ).NodeNumber ) continue;
				if ( NodeConnections( Loop2 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_OutsideAir ) ) continue;
				IsValid = true;
				break;
			}
			if ( ! IsValid ) {
				ShowSevereError( "Node Connection Error, Node=\"" + NodeConnections( Loop1 ).NodeName + "\", Outdoor Air Reference did not find an appropriate \"outdoor air\" node." );
				ShowContinueError( "This node must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object in order to set its conditions." );
				ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Name=" + NodeConnections( Loop1 ).ObjectName );
				++ErrorCounter;
				//      ErrorsFound=.TRUE.
			}
		}

		// Check 10 -- fluid streams cannot have multiple inlet/outlet nodes on same component
		//  can have multiple inlets with one outlet or vice versa but cannot have multiple both inlet and outlet
		if ( NumOfNodeConnections > 0 ) {
			MaxFluidStream = maxval( NodeConnections, &NodeConnectionDef::FluidStream );
			FluidStreamInletCount.allocate( MaxFluidStream );
			FluidStreamOutletCount.allocate( MaxFluidStream );
			FluidStreamCounts.allocate( MaxFluidStream );
			NodeObjects.allocate( NumOfNodeConnections );
			FluidStreamInletCount = 0;
			FluidStreamOutletCount = 0;
			NodeObjects = 0;
			FluidStreamCounts = false;
			// Following code relies on node connections for single object type/name being grouped together
			Object = 1;
			StartConnect = 1;
			EndConnect = 0;
			NumObjects = 2;
			NodeObjects( 1 ) = 1;
			while ( Object < NumOfNodeConnections ) {
				if ( NodeConnections( Object ).ObjectType != NodeConnections( Object + 1 ).ObjectType || NodeConnections( Object ).ObjectName != NodeConnections( Object + 1 ).ObjectName ) {
					EndConnect = Object + 1;
					NodeObjects( NumObjects ) = EndConnect;
					if ( Object + 1 < NumOfNodeConnections ) ++NumObjects;
				}
				++Object;
			}
			// NodeObjects now contains each consecutive object...
			for ( Object = 1; Object <= NumObjects - 1; ++Object ) {
				IsValid = true;
				FluidStreamInletCount = 0;
				FluidStreamOutletCount = 0;
				FluidStreamCounts = false;
				Loop1 = NodeObjects( Object );
				if ( NodeConnections( Loop1 ).ObjectIsParent ) continue;
				if ( NodeConnections( Loop1 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Inlet ) ) ++FluidStreamInletCount( NodeConnections( Loop1 ).FluidStream );
				if ( NodeConnections( Loop1 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Outlet ) ) ++FluidStreamOutletCount( NodeConnections( Loop1 ).FluidStream );
				for ( Loop2 = Loop1 + 1; Loop2 <= NodeObjects( Object + 1 ) - 1; ++Loop2 ) {
					if ( NodeConnections( Loop2 ).ObjectIsParent ) continue;
					if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Inlet ) ) ++FluidStreamInletCount( NodeConnections( Loop2 ).FluidStream );
					if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Outlet ) ) ++FluidStreamOutletCount( NodeConnections( Loop2 ).FluidStream );
				}
				for ( Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2 ) {
					if ( FluidStreamInletCount( Loop2 ) > 1 && FluidStreamOutletCount( Loop2 ) > 1 ) {
						IsValid = false;
						FluidStreamCounts( Loop2 ) = true;
					}
				}
				if ( ! IsValid ) {
					ShowSevereError( "(Developer) Node Connection Error, Object=" + NodeConnections( Loop1 ).ObjectType + ':' + NodeConnections( Loop1 ).ObjectName );
					ShowContinueError( "Object has multiple connections on both inlet and outlet fluid streams." );
					for ( Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2 ) {
						if ( FluidStreamCounts( Loop2 ) ) ShowContinueError( "...occurs in Fluid Stream [" + RoundSigDigits( Loop2 ) + "]." );
					}
					++ErrorCounter;
					ErrorsFound = true;
				}
			}
			FluidStreamInletCount.deallocate();
			FluidStreamOutletCount.deallocate();
			FluidStreamCounts.deallocate();
			NodeObjects.deallocate();
		}

		//Check 11 - zone nodes may not be used as anything else except as a setpoint, sensor or actuator node
		for ( Loop1 = 1; Loop1 <= NumOfNodeConnections; ++Loop1 ) {
			if ( NodeConnections( Loop1 ).ConnectionType != ValidConnectionTypes( NodeConnectionType_ZoneNode ) ) continue;
			IsValid = true;
			for ( Loop2 = Loop1; Loop2 <= NumOfNodeConnections; ++Loop2 ) {
				if ( Loop1 == Loop2 ) continue;
				if ( NodeConnections( Loop1 ).NodeName == NodeConnections( Loop2 ).NodeName ) {
					if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Sensor ) ) continue;
					if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_Actuator ) ) continue;
					if ( NodeConnections( Loop2 ).ConnectionType == ValidConnectionTypes( NodeConnectionType_SetPoint ) ) continue;
					ShowSevereError( "Node Connection Error, Node Name=\"" + NodeConnections( Loop1 ).NodeName + "\", The same zone node appears more than once." );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop1 ).ObjectType + ", Object Name=" + NodeConnections( Loop1 ).ObjectName );
					ShowContinueError( "Reference Object=" + NodeConnections( Loop2 ).ObjectType + ", Object Name=" + NodeConnections( Loop2 ).ObjectName );
					++ErrorCounter;
					ErrorsFound = true;
				}
			}
		}

		NumNodeConnectionErrors += ErrorCounter;

	}

	bool
	IsParentObject(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine determines if a component name is a parent node.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool IsParent; // True if this combination is a parent

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		IsParent = false;
		for ( Loop = 1; Loop <= NumOfNodeConnections; ++Loop ) {
			if ( NodeConnections( Loop ).ObjectType == ComponentType && NodeConnections( Loop ).ObjectName == ComponentName ) {
				if ( NodeConnections( Loop ).ObjectIsParent ) {
					IsParent = true;
				}
				break;
			}
		}
		if ( ! IsParent ) {
			IsParent = IsParentObjectCompSet( ComponentType, ComponentName );
		}

		return IsParent;

	}

	int
	WhichParentSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine determines which parent node list (number) for a given component name
		// and type.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int WhichOne;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		WhichOne = 0;
		for ( Loop = 1; Loop <= NumOfActualParents; ++Loop ) {
			if ( ParentNodeList( Loop ).CType == ComponentType && ParentNodeList( Loop ).CName == ComponentName ) {
				WhichOne = Loop;
				break;
			}
		}

		return WhichOne;

	}

	void
	GetParentData(
		std::string const & ComponentType,
		std::string const & ComponentName,
		std::string & InletNodeName,
		int & InletNodeNum,
		std::string & OutletNodeName,
		int & OutletNodeNum,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets node data for a given Parent Component Type and Name Name.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER Loop
		bool ErrInObject;
		int Which;

		InletNodeName = BlankString;
		InletNodeNum = 0;
		OutletNodeName = BlankString;
		OutletNodeNum = 0;
		ErrInObject = false;

		Which = WhichParentSet( ComponentType, ComponentName );
		if ( Which != 0 ) {
			InletNodeName = ParentNodeList( Which ).InletNodeName;
			OutletNodeName = ParentNodeList( Which ).OutletNodeName;
			// Get Node Numbers
			InletNodeNum = FindItemInList( InletNodeName, NodeID( {1,NumOfNodes} ), NumOfNodes );
			OutletNodeNum = FindItemInList( OutletNodeName, NodeID( {1,NumOfNodes} ), NumOfNodes );
			//    IF (InletNodeNum == 0 .and. ComponentType /= 'ZONEHVAC:AIRDISTRIBUTIONUNIT') THEN
			//      CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
			//        ', Component Name='//TRIM(ComponentName))
			//      CALL ShowContinueError('..Inlet Node Name, not found='//TRIM(InletNodeName))
			//!      ErrInObject=.TRUE.
			//    ENDIF
			//    IF (OutletNodeNum == 0) THEN
			//      CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
			//        ', Component Name='//TRIM(ComponentName))
			//      CALL ShowContinueError('..Outlet Node Name, not found='//TRIM(OutletNodeName))
			//!      ErrInObject=.TRUE.
			//    ENDIF
		} else if ( IsParentObjectCompSet( ComponentType, ComponentName ) ) {
			Which = WhichCompSet( ComponentType, ComponentName );
			if ( Which != 0 ) {
				InletNodeName = CompSets( Which ).InletNodeName;
				OutletNodeName = CompSets( Which ).OutletNodeName;
				InletNodeNum = FindItemInList( InletNodeName, NodeID( {1,NumOfNodes} ), NumOfNodes );
				OutletNodeNum = FindItemInList( OutletNodeName, NodeID( {1,NumOfNodes} ), NumOfNodes );
				//      IF (InletNodeNum == 0 .and. ComponentType /= 'ZONEHVAC:AIRDISTRIBUTIONUNIT') THEN
				//        CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
				//          ', Component Name='//TRIM(ComponentName))
				//        CALL ShowContinueError('..Inlet Node Name, not found='//TRIM(InletNodeName))
				//  !      ErrInObject=.TRUE.
				//      ENDIF
				//      IF (OutletNodeNum == 0) THEN
				//        CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
				//          ', Component Name='//TRIM(ComponentName))
				//        CALL ShowContinueError('..Outlet Node Name, not found='//TRIM(OutletNodeName))
				//  !      ErrInObject=.TRUE.
				//      ENDIF
			} else {
				ErrInObject = true;
				ShowWarningError( "GetParentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found." );
			}
		} else {
			ErrInObject = true;
			ShowWarningError( "GetParentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found." );
		}

		if ( ErrInObject ) ErrorsFound = true;

	}

	bool
	IsParentObjectCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine determines if a component name is a parent node.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool IsParent; // True if this combination is a parent

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		IsParent = false;
		for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
			if ( CompSets( Loop ).ParentCType == ComponentType && CompSets( Loop ).ParentCName == ComponentName ) {
				IsParent = true;
				break;
			}
		}

		return IsParent;

	}

	int
	WhichCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine determines which comp set (number) for a given component name
		// and type.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int WhichOne;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		WhichOne = 0;
		for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
			if ( CompSets( Loop ).CType == ComponentType && CompSets( Loop ).CName == ComponentName ) {
				WhichOne = Loop;
				break;
			}
		}

		return WhichOne;

	}

	int
	WhichParentCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine determines which comp set (number) for a given component name
		// and type.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int WhichOne;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		WhichOne = 0;
		for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
			if ( CompSets( Loop ).ParentCType == ComponentType && CompSets( Loop ).ParentCName == ComponentName ) {
				WhichOne = Loop;
				break;
			}
		}

		return WhichOne;

	}

	int
	GetNumChildren(
		std::string const & ComponentType,
		std::string const & ComponentName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine counts the number of children for a parent Component Set.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumChildren;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		NumChildren = 0;
		if ( IsParentObject( ComponentType, ComponentName ) ) {
			for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
				if ( CompSets( Loop ).ParentCType == ComponentType && CompSets( Loop ).ParentCName == ComponentName ) {
					++NumChildren;
				}
			}
		}

		return NumChildren;

	}

	void
	GetComponentData(
		std::string const & ComponentType,
		std::string const & ComponentName,
		bool & IsParent,
		int & NumInlets,
		Array1D_string & InletNodeNames,
		Array1D_int & InletNodeNums,
		Array1D_int & InletFluidStreams,
		int & NumOutlets,
		Array1D_string & OutletNodeNames,
		Array1D_int & OutletNodeNums,
		Array1D_int & OutletFluidStreams,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets data for a given Component Type and Name Name.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER Loop
		bool ErrInObject;
		int Which;
		//unused1109  LOGICAL FoundObject

		if ( allocated( InletNodeNames ) ) InletNodeNames.deallocate();
		if ( allocated( InletNodeNums ) ) InletNodeNums.deallocate();
		if ( allocated( InletFluidStreams ) ) InletFluidStreams.deallocate();
		if ( allocated( OutletNodeNames ) ) OutletNodeNames.deallocate();
		if ( allocated( OutletNodeNums ) ) OutletNodeNums.deallocate();
		if ( allocated( OutletFluidStreams ) ) OutletFluidStreams.deallocate();

		NumInlets = 0;
		NumOutlets = 0;

		//  FoundObject=.FALSE.
		IsParent = false;
		for ( Which = 1; Which <= NumOfNodeConnections; ++Which ) {
			if ( NodeConnections( Which ).ObjectType != ComponentType || NodeConnections( Which ).ObjectName != ComponentName ) continue;
			//    FoundObject=.TRUE.
			if ( NodeConnections( Which ).ObjectIsParent ) IsParent = true;
			if ( SameString( NodeConnections( Which ).ConnectionType, "Inlet" ) ) ++NumInlets;
			if ( SameString( NodeConnections( Which ).ConnectionType, "Outlet" ) ) ++NumOutlets;
		}

		InletNodeNames.allocate( NumInlets );
		InletNodeNums.allocate( NumInlets );
		InletFluidStreams.allocate( NumInlets );
		OutletNodeNames.allocate( NumOutlets );
		OutletNodeNums.allocate( NumOutlets );
		OutletFluidStreams.allocate( NumOutlets );

		InletNodeNames = BlankString;
		InletNodeNums = 0;
		InletFluidStreams = 0;
		OutletNodeNames = BlankString;
		OutletNodeNums = 0;
		OutletFluidStreams = 0;
		NumInlets = 0;
		NumOutlets = 0;
		ErrInObject = false;

		//  IF (IsParentObject(ComponentType,ComponentName)) THEN
		//    IsParent=.TRUE.
		//  ENDIF

		for ( Which = 1; Which <= NumOfNodeConnections; ++Which ) {
			if ( NodeConnections( Which ).ObjectType != ComponentType || NodeConnections( Which ).ObjectName != ComponentName ) continue;
			if ( SameString( NodeConnections( Which ).ConnectionType, "Inlet" ) ) {
				++NumInlets;
				InletNodeNames( NumInlets ) = NodeConnections( Which ).NodeName;
				InletNodeNums( NumInlets ) = NodeConnections( Which ).NodeNumber;
				InletFluidStreams( NumInlets ) = NodeConnections( Which ).FluidStream;
			}
			if ( SameString( NodeConnections( Which ).ConnectionType, "Outlet" ) ) {
				++NumOutlets;
				OutletNodeNames( NumOutlets ) = NodeConnections( Which ).NodeName;
				OutletNodeNums( NumOutlets ) = NodeConnections( Which ).NodeNumber;
				OutletFluidStreams( NumOutlets ) = NodeConnections( Which ).FluidStream;
			}
		}
		if ( ErrInObject ) {
			ShowWarningError( "GetComponentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found." );
		}

		if ( ErrInObject ) ErrorsFound = true;

	}

	void
	GetChildrenData(
		std::string const & ComponentType,
		std::string const & ComponentName,
		int & NumChildren,
		Array1S_string ChildrenCType,
		Array1S_string ChildrenCName,
		Array1S_string InletNodeName,
		Array1S_int InletNodeNum,
		Array1S_string OutletNodeName,
		Array1S_int OutletNodeNum,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets children data for given parent node.

		// METHODOLOGY EMPLOYED:
		// Traverses CompSet structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string ChildCType;
		Array1D_string ChildCName;
		Array1D_string ChildInNodeName;
		Array1D_string ChildOutNodeName;
		Array1D_int ChildInNodeNum;
		Array1D_int ChildOutNodeNum;
		int Loop;
		int CountNum;
		bool ErrInObject;
		std::string MatchNodeName;
		std::string ParentInletNodeName;
		std::string ParentOutletNodeName;
		int ParentInletNodeNum;
		int ParentOutletNodeNum;
		//unused1109  LOGICAL Matched
		int CountMatchLoop;

		ChildrenCType = BlankString;
		ChildrenCName = BlankString;
		InletNodeName = BlankString;
		InletNodeNum = 0;
		OutletNodeName = BlankString;
		OutletNodeNum = 0;
		ErrInObject = false;

		if ( IsParentObject( ComponentType, ComponentName ) ) {
			NumChildren = GetNumChildren( ComponentType, ComponentName );
			if ( NumChildren == 0 ) {
				ShowWarningError( "GetChildrenData: Parent Node has no children, node=" + ComponentType + ':' + ComponentName );
			} else {
				GetParentData( ComponentType, ComponentName, ParentInletNodeName, ParentInletNodeNum, ParentOutletNodeName, ParentOutletNodeNum, ErrInObject );
				ChildCType.allocate( NumChildren );
				ChildCName.allocate( NumChildren );
				ChildInNodeName.allocate( NumChildren );
				ChildOutNodeName.allocate( NumChildren );
				ChildInNodeNum.allocate( NumChildren );
				ChildOutNodeNum.allocate( NumChildren );
				ChildCType = BlankString;
				ChildCName = BlankString;
				ChildInNodeName = BlankString;
				ChildOutNodeName = BlankString;
				ChildInNodeNum = 0;
				ChildOutNodeNum = 0;
				CountNum = 0;
				for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
					if ( CompSets( Loop ).ParentCType == ComponentType && CompSets( Loop ).ParentCName == ComponentName ) {
						++CountNum;
						ChildCType( CountNum ) = CompSets( Loop ).CType;
						ChildCName( CountNum ) = CompSets( Loop ).CName;
						ChildInNodeName( CountNum ) = CompSets( Loop ).InletNodeName;
						ChildOutNodeName( CountNum ) = CompSets( Loop ).OutletNodeName;
						// Get Node Numbers
						ChildInNodeNum( CountNum ) = FindItemInList( ChildInNodeName( CountNum ), NodeID( {1,NumOfNodes} ), NumOfNodes );
						//          IF (ChildInNodeNum(CountNum) == 0) THEN
						//            CALL ShowSevereError('GetChildrenData: Inlet Node not previously assigned, Node='//  &
						//                    TRIM(ChildInNodeName(CountNum)))
						//            CALL ShowContinueError('..Component='//TRIM(ChildCType(CountNum))//':'//TRIM(ChildCName(CountNum)))
						//            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
						//            ErrInObject=.TRUE.
						//          ENDIF
						ChildOutNodeNum( CountNum ) = FindItemInList( ChildOutNodeName( CountNum ), NodeID( {1,NumOfNodes} ), NumOfNodes );
						//          IF (ChildOutNodeNum(CountNum) == 0) THEN
						//            CALL ShowSevereError('GetChildrenData: Outlet Node not previously assigned, Node='//  &
						//                    TRIM(ChildOutNodeName(CountNum)))
						//            CALL ShowContinueError('..Component='//TRIM(ChildCType(CountNum))//':'//TRIM(ChildCName(CountNum)))
						//            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
						//            ErrInObject=.TRUE.
						//          ENDIF
					}
				}
				if ( CountNum != NumChildren ) {
					ShowSevereError( "GetChildrenData: Counted nodes not equal to GetNumChildren count" );
					ErrInObject = true;
				} else {
					// Children arrays built.  Now "sort" for flow connection order(?)
					MatchNodeName = ParentInletNodeName;
					CountNum = 0;
					CountMatchLoop = 0;
					while ( CountMatchLoop < NumChildren ) {
						++CountMatchLoop;
						//          Matched=.FALSE.
						for ( Loop = 1; Loop <= NumChildren; ++Loop ) {
							if ( ChildInNodeName( Loop ) == MatchNodeName ) {
								++CountNum;
								ChildrenCType( CountNum ) = ChildCType( Loop );
								ChildrenCName( CountNum ) = ChildCName( Loop );
								InletNodeName( CountNum ) = ChildInNodeName( Loop );
								InletNodeNum( CountNum ) = ChildInNodeNum( Loop );
								OutletNodeName( CountNum ) = ChildOutNodeName( Loop );
								OutletNodeNum( CountNum ) = ChildOutNodeNum( Loop );
								ChildInNodeName( Loop ).clear(); // So it won't match anymore
								//              Matched=.TRUE.
								MatchNodeName = ChildOutNodeName( Loop );
								break;
							}
						}
						//          IF (.not. Matched .and. MatchNodeName /= blank) THEN
						//            IF (CountMatchLoop > 1) THEN
						//              CALL ShowSevereError('GetChildrenData: Sorting for flow connection order..'//  &
						//                                 'Required Child Node, not matched.  Expected Inlet Node='//  &
						//                                 TRIM(MatchNodeName))
						//            ELSE
						//              CALL ShowSevereError('GetChildrenData: Sorting for 1st node in flow connection order..'//  &
						//                                 'Required Child Node, not matched.  Expected Inlet Node='//  &
						//                                 TRIM(MatchNodeName))
						//            ENDIF
						//            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
						//            ErrInObject=.TRUE.
						//          ENDIF
					}
					if ( MatchNodeName != ParentOutletNodeName ) {
						for ( Loop = 1; Loop <= NumChildren; ++Loop ) {
							if ( ChildInNodeName( Loop ).empty() ) continue;
							if ( ChildOutNodeName( Loop ) == ParentOutletNodeName ) break;
							//            CALL ShowSevereError('GetChildrenData: Sorting for flow connection order..'//  &
							//                                 'Required Child Node, not matched.  Expected (Last) Outlet Node='//  &
							//                                 TRIM(MatchNodeName))
							//            CALL ShowContinueError('..does not match Parent Outlet Node='//TRIM(ParentOutletNodeName))
							//            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
							break;
							//          ErrInObject=.TRUE.
						}
					}
					for ( Loop = 1; Loop <= NumChildren; ++Loop ) {
						if ( ChildInNodeName( Loop ).empty() ) continue;
						++CountNum;
						ChildrenCType( CountNum ) = ChildCType( Loop );
						ChildrenCName( CountNum ) = ChildCName( Loop );
						InletNodeName( CountNum ) = ChildInNodeName( Loop );
						InletNodeNum( CountNum ) = ChildInNodeNum( Loop );
						OutletNodeName( CountNum ) = ChildOutNodeName( Loop );
						OutletNodeNum( CountNum ) = ChildOutNodeNum( Loop );
					}
					ChildCType.deallocate();
					ChildCName.deallocate();
					ChildInNodeName.deallocate();
					ChildOutNodeName.deallocate();
					ChildInNodeNum.deallocate();
					ChildOutNodeNum.deallocate();
				}
			}
		} else {
			ShowSevereError( "GetChildrenData: Requested Children Data for non Parent Node=" + ComponentType + ':' + ComponentName );
			ErrInObject = true;
		}

		if ( ErrInObject ) ErrorsFound = true;

	}

	void
	SetUpCompSets(
		std::string const & ParentType, // Parent Object Type
		std::string const & ParentName, // Parent Object Name
		std::string const & CompType, // Component Type
		std::string const & CompName, // Component Name
		std::string const & InletNode, // Inlet Node Name
		std::string const & OutletNode, // Outlet Node Name
		Optional_string_const Description // Description
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets up "Component Sets" as input in the branch
		// lists.  These can be used later to verify that the proper names and
		// inlet/outlet nodes have been input.  This routine assumes that identical
		// "CompSets" cannot be used in multiple places and issues a warning if they are.
		// This subroutine also

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompTypeUC; // Component type in upper case
		std::string ParentTypeUC; // Parent component type in upper case
		int Count;
		int Count2;
		int Found;
		int Found2;

		// Object Data

		ParentTypeUC = MakeUPPERCase( ParentType );
		CompTypeUC = MakeUPPERCase( CompType );
		Found = 0;

		// See if Component-Nodes set is already there - should be unique
		// Try to fill in blanks (passed in as undefined
		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			//    IF (CompTypeUC /= CompSets(Count)%CType .or. CompName /= CompSets(Count)%CName) CYCLE
			if ( CompName != CompSets( Count ).CName ) continue;
			if ( CompTypeUC != "UNDEFINED" ) {
				if ( CompTypeUC != CompSets( Count ).CType ) continue;
			}
			// Component name matches, component type matches or is undefined
			if ( InletNode != "UNDEFINED" ) {
				if ( CompSets( Count ).InletNodeName != "UNDEFINED" ) {
					if ( InletNode != CompSets( Count ).InletNodeName ) continue;
				}
			}
			if ( OutletNode != "UNDEFINED" ) {
				if ( CompSets( Count ).OutletNodeName != "UNDEFINED" ) {
					if ( OutletNode != CompSets( Count ).OutletNodeName ) continue;
				}
			}
			//  See if something undefined and set here
			if ( CompSets( Count ).ParentCType == "UNDEFINED" && CompSets( Count ).ParentCName == "UNDEFINED" ) {
				// Assume this is a further definition for this compset
				CompSets( Count ).ParentCType = ParentTypeUC;
				CompSets( Count ).ParentCName = ParentName;
				if ( present( Description ) ) CompSets( Count ).Description = Description;
				Found = Count;
				break;
			}
		}
		if ( Found == 0 ) {
			for ( Count = 1; Count <= NumCompSets; ++Count ) {
				Found = 0;
				// Test if inlet node has been used before as an inlet node
				// If the matching node name does not belong to the parent object, then error
				// For example a fan may share the same inlet node as the furnace object which is its parent
				if ( InletNode != CompSets( Count ).InletNodeName ) {
					continue;
					// If parent type is "UNDEFINED" then no error
				} else if ( ( ParentTypeUC == "UNDEFINED" ) || ( CompSets( Count ).ParentCType == "UNDEFINED" ) ) {
					// If node name is "UNDEFINED" then no error
				} else if ( InletNode != "UNDEFINED" ) {
					// If the matching node name does not belong to the parent or child object, then error
					// For example a fan may share the same inlet node as the furnace object which is its parent
					if ( ( ParentTypeUC == CompSets( Count ).CType ) && ( ParentName == CompSets( Count ).CName ) ) {
						// OK - The duplicate inlet node belongs to this component's parent
					} else if ( ( CompTypeUC == CompSets( Count ).ParentCType ) && ( CompName == CompSets( Count ).ParentCName ) ) {
						// OK - The duplicate inlet node belongs to a child of this component
					} else {
						// Due to possibility of grandparents or more, if the matching node name
						// belongs to a component that appears as a parent, then OK
						Found2 = 0;
						for ( Count2 = 1; Count2 <= NumCompSets; ++Count2 ) {
							if ( ( CompSets( Count ).CType == CompSets( Count2 ).ParentCType ) && ( CompSets( Count ).CName == CompSets( Count2 ).ParentCName ) ) Found2 = 1;
							if ( ( CompTypeUC == CompSets( Count2 ).ParentCType ) && ( CompName == CompSets( Count2 ).ParentCName ) ) Found2 = 1;
						}
						if ( Found2 == 0 ) {
							ShowWarningError( "Node used as an inlet more than once: " + InletNode );
							ShowContinueError( "  Used by     : " + CompSets( Count ).ParentCType + ", name=" + CompSets( Count ).ParentCName );
							ShowContinueError( "  as inlet for: " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
							ShowContinueError( "  and  by     : " + ParentTypeUC + ", name=" + ParentName );
							ShowContinueError( "  as inlet for: " + CompTypeUC + ", name=" + CompName );
						}
					}
				}
				// Test if outlet node has been used before as an outlet node
				// If the matching node name does not belong to the parent or child object, then error
				// For example a fan may share the same outlet node as the furnace object which is its parent
				if ( OutletNode != CompSets( Count ).OutletNodeName ) {
					continue;
					// If parent type is "UNDEFINED" then no error
				} else if ( ( ParentTypeUC == "UNDEFINED" ) || ( CompSets( Count ).ParentCType == "UNDEFINED" ) ) {
					// If node name is "UNDEFINED" then no error
				} else if ( OutletNode != "UNDEFINED" ) {
					if ( ( ParentTypeUC == CompSets( Count ).CType ) && ( ParentName == CompSets( Count ).CName ) ) {
						// OK - The duplicate outlet node belongs to this component's parent
					} else if ( ( CompTypeUC == CompSets( Count ).ParentCType ) && ( CompName == CompSets( Count ).ParentCName ) ) {
						// OK - The duplicate outlet node belongs to a child of this component
					} else {
						// Due to possibility of grandparents or more, if the matching node name
						// belongs to a component that appears as a parent, then OK
						Found2 = 0;
						for ( Count2 = 1; Count2 <= NumCompSets; ++Count2 ) {
							if ( ( CompSets( Count ).CType == CompSets( Count2 ).ParentCType ) && ( CompSets( Count ).CName == CompSets( Count2 ).ParentCName ) ) Found2 = 1;
							if ( ( CompTypeUC == CompSets( Count2 ).ParentCType ) && ( CompName == CompSets( Count2 ).ParentCName ) ) Found2 = 1;
						}
						// This rule is violated by dual duct units, so let it pass
						if ( ( Found2 == 0 ) && ( ! has_prefixi( CompSets( Count ).CType, "AirTerminal:DualDuct:" ) ) && ( ! has_prefixi( CompTypeUC, "AirTerminal:DualDuct:" ) ) ) {
							ShowWarningError( "Node used as an outlet more than once: " + OutletNode );
							ShowContinueError( "  Used by     : " + CompSets( Count ).ParentCType + ", name=" + CompSets( Count ).ParentCName );
							ShowContinueError( "  as outlet for: " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
							ShowContinueError( "  and  by     : " + ParentTypeUC + ", name=" + ParentName );
							ShowContinueError( "  as outlet for: " + CompTypeUC + ", name=" + CompName );
						}
					}
				}
				if ( CompTypeUC != CompSets( Count ).CType && CompTypeUC != "UNDEFINED" ) continue;
				if ( CompName != CompSets( Count ).CName ) continue;
				Found = Count;
				break;
			}
		}
		if ( Found == 0 ) {
			CompSets.redimension( ++NumCompSets );
			CompSets( NumCompSets ).ParentCType = ParentTypeUC;
			CompSets( NumCompSets ).ParentCName = ParentName;
			CompSets( NumCompSets ).CType = CompTypeUC;
			CompSets( NumCompSets ).CName = CompName;
			CompSets( NumCompSets ).InletNodeName = InletNode;
			CompSets( NumCompSets ).OutletNodeName = OutletNode;
			if ( present( Description ) ) {
				CompSets( NumCompSets ).Description = Description;
			} else {
				CompSets( NumCompSets ).Description = "UNDEFINED";
			}
		}

	}

	void
	TestInletOutletNodes( bool & EP_UNUSED( ErrorsFound ) )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine tests the branches to see if a duplicate inlet node
		// exists under a different name in the sequence; likewise for outlet.

		// METHODOLOGY EMPLOYED:
		// na

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
		int Count;
		int Other;
		Array1D_bool AlreadyNoted;

		// Test component sets created by branches
		AlreadyNoted.dimension( NumCompSets, false );
		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			for ( Other = 1; Other <= NumCompSets; ++Other ) {
				if ( Count == Other ) continue;
				if ( CompSets( Count ).InletNodeName != CompSets( Other ).InletNodeName ) continue;
				if ( AlreadyNoted( Count ) ) continue;
				//  All other values must match
				if ( CompSets( Count ).CType != CompSets( Other ).CType || CompSets( Count ).CName != CompSets( Other ).CName || CompSets( Count ).OutletNodeName != CompSets( Other ).OutletNodeName ) {
					AlreadyNoted( Other ) = true;
					ShowWarningError( "Node used as an inlet more than once: " + CompSets( Count ).InletNodeName );
					ShowContinueError( "  Used by     : " + CompSets( Count ).ParentCType + ", name=" + CompSets( Count ).ParentCName );
					ShowContinueError( "  as inlet for: " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
					ShowContinueError( "  and  by     : " + CompSets( Other ).ParentCType + ", name=" + CompSets( Other ).ParentCName );
					ShowContinueError( "  as inlet for: " + CompSets( Other ).CType + ", name=" + CompSets( Other ).CName );
					//        ErrorsFound=.TRUE.
				}
			}
		}

		AlreadyNoted = false;
		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			for ( Other = 1; Other <= NumCompSets; ++Other ) {
				if ( Count == Other ) continue;
				if ( CompSets( Count ).OutletNodeName != CompSets( Other ).OutletNodeName ) continue;
				if ( AlreadyNoted( Count ) ) continue;
				//  All other values must match
				if ( CompSets( Count ).CType != CompSets( Other ).CType || CompSets( Count ).CName != CompSets( Other ).CName || CompSets( Count ).InletNodeName != CompSets( Other ).InletNodeName ) {
					AlreadyNoted( Other ) = true;
					ShowWarningError( "Node used as an outlet more than once: " + CompSets( Count ).OutletNodeName );
					ShowContinueError( "  Used by      : " + CompSets( Count ).ParentCType + ", name=" + CompSets( Count ).ParentCName );
					ShowContinueError( "  as outlet for: " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
					ShowContinueError( "  and  by      : " + CompSets( Other ).ParentCType + ", name=" + CompSets( Other ).ParentCName );
					ShowContinueError( "  as outlet for: " + CompSets( Other ).CType + ", name=" + CompSets( Other ).CName );
					//        ErrorsFound=.TRUE.
				}
			}
		}

		AlreadyNoted.deallocate();

	}

	void
	TestCompSet(
		std::string const & CompType, // Component Type
		std::string const & CompName, // Component Name
		std::string const & InletNode, // Inlet Node Name
		std::string const & OutletNode, // Outlet Node Name
		std::string const & Description // Description of Node Pair (for warning message)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Register a child component in the CompSets data structure.
		// NOTE:  This function was originally designed to test the stored "Component Sets" to
		// see if there was one of this combination in there.  Thus the name "TestCompSet".
		// However, this was based on a false assumption that input would always be gotten
		// first for the parent object, then for the child object.  But this is often not the
		// case.  Ultimately, the name of this function should be changed or it should be merged
		// into SetUpCompSets.
		// Until then, this function does the following:
		//   a)  Search CompSets for this combination of component type, component name,
		//       inlet node and outlet node.  If component type/name match and the existing
		//       node names are UNDEFINED, this compset is assumed to be a match.
		//   b)  If found, fill in any missing data such as node names or node description
		//   c)  If not found, call SetUpCompSets (with parent type and name UNDEFINED)
		//       to add a new item in the CompSets array

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Count;
		int Found;
		std::string CompTypeUC; // Component type in upper case

		CompTypeUC = MakeUPPERCase( CompType );

		// See if Already there
		Found = 0;
		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			if ( ( CompTypeUC != CompSets( Count ).CType ) && ( CompSets( Count ).CType != "UNDEFINED" ) ) continue;
			if ( CompName != CompSets( Count ).CName ) continue;
			if ( ( InletNode != CompSets( Count ).InletNodeName ) && ( CompSets( Count ).InletNodeName != "UNDEFINED" ) && ( InletNode != "UNDEFINED" ) ) continue;
			if ( ( OutletNode != CompSets( Count ).OutletNodeName ) && ( CompSets( Count ).OutletNodeName != "UNDEFINED" ) && ( OutletNode != "UNDEFINED" ) ) continue;

			Found = Count;
			break;
		}

		if ( Found == 0 ) {
			SetUpCompSets( "UNDEFINED", "UNDEFINED", CompType, CompName, InletNode, OutletNode, Description );
		} else {
			// Fill in node names and component type for previously undefined values:
			//   If the parent object did not specify a component type or inlet or outlet node, then that value
			//   is UNDEFINED in CompSets.  When a component calls TestCompSet, the comp type and inlet and
			//   outlet nodes are known, so they can be filled in for future reference.
			if ( CompSets( Found ).CType == "UNDEFINED" ) CompSets( Found ).CType = CompTypeUC;
			if ( CompSets( Found ).InletNodeName == "UNDEFINED" ) CompSets( Found ).InletNodeName = InletNode;
			if ( CompSets( Found ).OutletNodeName == "UNDEFINED" ) CompSets( Found ).OutletNodeName = OutletNode;
			if ( CompSets( Found ).Description == "UNDEFINED" ) CompSets( Found ).Description = Description;
		}

	}

	void
	TestCompSetInletOutletNodes( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine tests the comp sets to see if a duplicate comp name
		// exists under a different set of inlet/outlet nodes.

		// METHODOLOGY EMPLOYED:
		// na

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
		int Count;
		int Other;
		Array1D_bool AlreadyNoted;

		// Test component sets created by branches
		AlreadyNoted.dimension( NumCompSets, false );
		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			for ( Other = 1; Other <= NumCompSets; ++Other ) {
				if ( Count == Other ) continue;
				if ( CompSets( Count ).CType == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED" ) continue;
				if ( CompSets( Count ).CType != CompSets( Other ).CType || CompSets( Count ).CName != CompSets( Other ).CName ) continue;
				if ( CompSets( Count ).Description != CompSets( Other ).Description ) {
					if ( CompSets( Count ).Description != "UNDEFINED" && CompSets( Other ).Description != "UNDEFINED" ) continue;
				}
				if ( CompSets( Count ).InletNodeName == CompSets( Other ).InletNodeName ) continue;
				if ( CompSets( Count ).OutletNodeName == CompSets( Other ).OutletNodeName ) continue;
				if ( AlreadyNoted( Count ) ) continue;
				//  All other values must match
				AlreadyNoted( Other ) = true;
				ShowSevereError( "Same component name and type has differing Node Names." );
				ShowContinueError( "   Component:    " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
				ShowContinueError( "   Nodes, inlet: " + CompSets( Count ).InletNodeName + ", outlet: " + CompSets( Count ).OutletNodeName );
				ShowContinueError( " & Nodes, inlet: " + CompSets( Other ).InletNodeName + ", outlet: " + CompSets( Other ).OutletNodeName );
				ShowContinueError( "   Node Types:   " + CompSets( Count ).Description + " & " + CompSets( Other ).Description );
				ErrorsFound = true;
			}
		}

		//  AlreadyNoted=.FALSE.
		//  DO Count=1,NumCompSets
		//    DO Other=1,NumCompSets
		//      IF (Count == Other) CYCLE
		//      IF (CompSets(Count)%InletNodeName /= CompSets(Other)%InletNodeName) CYCLE
		//      IF (AlreadyNoted(Count)) CYCLE
		//      !  All other values must match
		//      IF (CompSets(Count)%ParentCType == 'BRANCH' .or. CompSets(Other)%ParentCType == 'BRANCH') CYCLE
		//      IF (CompSets(Count)%Description /= CompSets(Other)%Description) CYCLE
		//      IF (CompSets(Count)%CType == CompSets(Other)%CType) THEN
		//        AlreadyNoted(Other)=.TRUE.
		//        CALL ShowWarningError('Node used as an inlet more than once: '//TRIM(CompSets(Count)%InletNodeName))
		//        CALL ShowContinueError('  Used by     : '//TRIM(CompSets(Count)%ParentCType)//  &
		//                                                         ', name='//TRIM(CompSets(Count)%ParentCName))
		//        CALL ShowContinueError('  as inlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
		//        CALL ShowContinueError('  and  by     : '//TRIM(CompSets(Other)%ParentCType)//  &
		//                                                         ', name='//TRIM(CompSets(Other)%ParentCName))
		//        CALL ShowContinueError('  as inlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
		//        ErrorsFound=.TRUE.
		//      ENDIF
		//    ENDDO
		//  ENDDO

		//  AlreadyNoted=.FALSE.
		//  DO Count=1,NumCompSets
		//    DO Other=1,NumCompSets
		//      IF (Count == Other) CYCLE
		//      IF (CompSets(Count)%OutletNodeName /= CompSets(Other)%OutletNodeName) CYCLE
		//      IF (AlreadyNoted(Count)) CYCLE
		//      !  All other values must match
		//      IF (CompSets(Count)%ParentCType == 'BRANCH' .or. CompSets(Other)%ParentCType == 'BRANCH') CYCLE
		//      IF (CompSets(Count)%Description /= CompSets(Other)%Description) CYCLE
		//      IF (CompSets(Count)%CType /= CompSets(Other)%CType) THEN
		//        AlreadyNoted(Other)=.TRUE.
		//        CALL ShowWarningError('Node used as an outlet more than once: '//TRIM(CompSets(Count)%OutletNodeName))
		//        CALL ShowContinueError('  Used by      : '//TRIM(CompSets(Count)%ParentCType)//  &
		//                                ', name='//TRIM(CompSets(Count)%ParentCName))
		//        CALL ShowContinueError('  as outlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
		//        CALL ShowContinueError('  and  by      : '//TRIM(CompSets(Other)%ParentCType)//  &
		//                                ', name='//TRIM(CompSets(Other)%ParentCName))
		//        CALL ShowContinueError('  as outlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
		//        ErrorsFound=.TRUE.
		//      ENDIF
		//    ENDDO
		//  ENDDO

		AlreadyNoted.deallocate();

	}

	void
	GetNodeConnectionType(
		int const NodeNumber,
		Array1D_int & NodeConnectType,
		bool & errFlag
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Jan 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides a connection type with given node number

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int NodeConnectIndex;
		int NumInList;
		Array1D_int ListArray;

		if ( allocated( NodeConnectType ) ) NodeConnectType.deallocate();

		FindAllNodeNumbersInList( NodeNumber, NodeConnections, NumOfNodeConnections, NumInList, ListArray );

		NodeConnectType.allocate( NumInList );

		if ( NumInList > 0 ) {
			for ( NodeConnectIndex = 1; NodeConnectIndex <= NumInList; ++NodeConnectIndex ) {
				NodeConnectType( NodeConnectIndex ) = FindItemInList( NodeConnections( ListArray( NodeConnectIndex ) ).ConnectionType, ValidConnectionTypes, NumValidConnectionTypes );
			}
		} else {
			if ( NodeNumber > 0 ) {
				ShowWarningError( "Node not found = " + NodeID( NodeNumber ) + '.' );
			} else {
				ShowWarningError( "Invalid node number passed = 0." );
			}
			errFlag = true;
		}

	}

	void
	FindAllNodeNumbersInList(
		int const WhichNumber,
		Array1< DataBranchNodeConnections::NodeConnectionDef > const & NodeConnections,
		int const NumItems,
		int & CountOfItems, // Number of items found
		Array1D_int & AllNumbersInList // Index array to all numbers found
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a number(integer) in a similar list of
		// items and returns the index of the item in the list, if
		// found.

		// METHODOLOGY EMPLOYED:
		// na

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
		int Count; // Counter for DO loops

		CountOfItems = 0;

		if ( allocated( AllNumbersInList ) ) AllNumbersInList.deallocate();

		for ( Count = 1; Count <= NumItems; ++Count ) {
			if ( WhichNumber == NodeConnections( Count ).NodeNumber ) {
				++CountOfItems;
			}
		}

		if ( CountOfItems > 0 ) {

			AllNumbersInList.dimension( CountOfItems, 0 );
			CountOfItems = 0;

			for ( Count = 1; Count <= NumItems; ++Count ) {
				if ( WhichNumber == NodeConnections( Count ).NodeNumber ) {
					++CountOfItems;
					AllNumbersInList( CountOfItems ) = Count;
				}
			}

		}

	}

} // BranchNodeConnections

} // EnergyPlus
