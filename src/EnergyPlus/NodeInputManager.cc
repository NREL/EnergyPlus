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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <NodeInputManager.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <EMSManager.hh>

namespace EnergyPlus {

namespace NodeInputManager {

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1999
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To provide utilities for reading and assigning indices for the
	// nodes in the HVAC loops.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::OutputFileBNDetails;
	using DataGlobals::DisplayAdvancedReportVariables;
	using InputProcessor::GetNumObjectsFound;
	using InputProcessor::GetObjectItem;
	using InputProcessor::FindItemInList;
	using InputProcessor::VerifyName;
	using InputProcessor::MakeUPPERCase;
	using InputProcessor::SameString;
	using InputProcessor::GetObjectDefMaxArgs;
	using General::TrimSigDigits;
	using namespace DataLoopNode;
	using namespace BranchNodeConnections;

	// Data
	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;
	static std::string const fluidNameSteam( "STEAM" );

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	int NumOfNodeLists( 0 ); // Total number of Node Lists in IDF
	int NumOfUniqueNodeNames( 0 ); // Number of Unique Node Names (current)
	// The following is a module level flag because there are several possible "entries" into
	// this module that may need to get the Node Inputs.
	bool GetNodeInputFlag( true ); // Flag to Get Node Input(s)
	Array1D_string TmpNodeID; // Used to "reallocate" name arrays
	Array1D_int NodeRef; // Number of times a Node is "referenced"
	std::string CurCheckContextName; // Used in Uniqueness checks
	Array1D_string UniqueNodeNames; // used in uniqueness checks
	int NumCheckNodes( 0 ); // Num of Unique nodes in check
	int MaxCheckNodes( 0 ); // Current "max" unique nodes in check
	bool NodeVarsSetup( false ); // Setup indicator of node vars for reporting (also that all nodes have been entered)
	Array1D_bool NodeWetBulbRepReq;

	// Object Data
	Array1D< NodeListDef > NodeLists; // Node Lists
	namespace {
		bool CalcMoreNodeInfoMyOneTimeFlag( true ); // one time flag
	}
	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	// Clears the global data in NodeInputManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		CalcMoreNodeInfoMyOneTimeFlag = true;
		NumOfNodeLists = 0;
		NumOfUniqueNodeNames = 0;
		GetNodeInputFlag = true;
		TmpNodeID.deallocate();
		NodeRef.deallocate();
		CurCheckContextName = std::string();
		UniqueNodeNames.deallocate();
		NumCheckNodes = 0;
		MaxCheckNodes = 0;
		NodeVarsSetup = false;
		NodeLists.deallocate();
	}

	void
	GetNodeNums(
		std::string const & Name, // Name for which to obtain information
		int & NumNodes, // Number of nodes accompanying this Name
		Array1S_int NodeNumbers, // Node Numbers accompanying this Name
		bool & ErrorsFound, // True when errors are found...
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		std::string const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		std::string const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_bool_const IncrementFluidStream, // True/False
		Optional_string_const InputFieldName // Input Field Name
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1999
		//       MODIFIED       February 2004, Fluid Type checking/setting
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calls the Node Manager to determine if the
		// entered name has already been assigned and if it is a list
		// or if it is a single node.  If it has not been assigned, then
		// it is a single node and will need to be entered in the Node
		// data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetNodeNums: " );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ThisOne; // Indicator for this Name
		//  CHARACTER(len=20) :: CaseNodeFluidType
		std::string cNodeFluidType;
		std::string ConnectionType;
		int Loop;
		int FluidStreamNum; // Fluid stream number passed to RegisterNodeConnection

		if ( GetNodeInputFlag ) {
			GetNodeListsInput( ErrorsFound );
			GetNodeInputFlag = false;
		}

		if ( NodeFluidType != NodeType_Air && NodeFluidType != NodeType_Water && NodeFluidType != NodeType_Electric && NodeFluidType != NodeType_Steam && NodeFluidType != NodeType_Unknown ) {
			gio::write( cNodeFluidType, fmtLD ) << NodeFluidType;
			strip( cNodeFluidType );
			ShowSevereError( RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid fluid type." );
			ShowContinueError( "..Invalid FluidType=" + cNodeFluidType );
			ErrorsFound = true;
			ShowFatalError( "Preceding issue causes termination." );
		}

		if ( not_blank( Name ) ) {
			ThisOne = FindItemInList( Name, NodeLists );
			if ( ThisOne != 0 ) {
				NumNodes = NodeLists( ThisOne ).NumOfNodesInList;
				NodeNumbers( {1,NumNodes} ) = NodeLists( ThisOne ).NodeNumbers( {1,NumNodes} );
				for ( Loop = 1; Loop <= NumNodes; ++Loop ) {
					if ( NodeFluidType != NodeType_Unknown && Node( NodeNumbers( Loop ) ).FluidType != NodeType_Unknown ) {
						if ( Node( NodeNumbers( Loop ) ).FluidType != NodeFluidType ) {
							ShowSevereError( RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data." );
							if ( present( InputFieldName ) ) ShowContinueError( "...Ref field=" + InputFieldName );
							ShowContinueError( "Existing Fluid type for node, incorrect for request. Node=" + NodeID( NodeNumbers( Loop ) ) );
							ShowContinueError( "Existing Fluid type=" + ValidNodeFluidTypes( Node( NodeNumbers( Loop ) ).FluidType ) + ", Requested Fluid Type=" + ValidNodeFluidTypes( NodeFluidType ) );
							ErrorsFound = true;
						}
					}
					if ( Node( NodeNumbers( Loop ) ).FluidType == NodeType_Unknown ) {
						Node( NodeNumbers( Loop ) ).FluidType = NodeFluidType;
					}
					++NodeRef( NodeNumbers( Loop ) );
				}
			} else {
				ThisOne = AssignNodeNumber( Name, NodeFluidType, ErrorsFound );
				NumNodes = 1;
				NodeNumbers( 1 ) = ThisOne;
			}
		} else {
			NumNodes = 0;
			NodeNumbers( 1 ) = 0;
		}

		// Most calls to this routined use a fixed fluid stream number for all nodes, this is the default
		FluidStreamNum = NodeFluidStream;
		for ( Loop = 1; Loop <= NumNodes; ++Loop ) {
			if ( NodeConnectionType >= 1 && NodeConnectionType <= NumValidConnectionTypes ) {
				ConnectionType = ValidConnectionTypes( NodeConnectionType );
			} else {
				ConnectionType = TrimSigDigits( NodeConnectionType ) + "-unknown";
			}
			// If requested, assign NodeFluidStream to the first node and increment the fluid stream number
			// for each remaining node in the list
			if ( present( IncrementFluidStream ) ) {
				if ( IncrementFluidStream ) FluidStreamNum = NodeFluidStream + ( Loop - 1 );
			}
			RegisterNodeConnection( NodeNumbers( Loop ), NodeID( NodeNumbers( Loop ) ), NodeObjectType, NodeObjectName, ConnectionType, FluidStreamNum, ObjectIsParent, ErrorsFound, InputFieldName );
		}

	}

	void
	GetNodeList(
		std::string const & Name, // Node List Name for which information is obtained
		int & NumNodes, // Number of nodes accompanying this Name
		Array1S_int NodeNumbers, // NodeNumbers accompanying this Name
		bool & errFlag, // Set to true when requested Node List not found
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		std::string const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		std::string const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_string_const InputFieldName // Input Field Name
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1999
		//       MODIFIED       February 2003, Error Flag added
		//                      February 2004, Fluid Type
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called when the Get routines are specifically looking
		// for a Node List.  It should exist.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetNodeList: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Try; // Indicator for this Name

		if ( GetNodeInputFlag ) {
			GetNodeListsInput( errFlag );
			GetNodeInputFlag = false;
		}

		//  FluidType=NodeFluidType

		NumNodes = 0;
		NodeNumbers( 1 ) = 0;
		errFlag = false;

		Try = 0;
		if ( NumOfNodeLists > 0 ) {
			Try = FindItemInList( Name, NodeLists );
		}

		if ( Try != 0 ) {
			GetNodeNums( Name, NumNodes, NodeNumbers, errFlag, NodeFluidType, NodeObjectType, NodeObjectName, NodeConnectionType, NodeFluidStream, ObjectIsParent, _, InputFieldName );
		} else {
			// only valid "error" here is when the Node List is blank
			if ( ! Name.empty() ) {
				ShowSevereError( RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data." );
				if ( present( InputFieldName ) ) ShowContinueError( "...Ref field=" + InputFieldName );
				ShowContinueError( "NodeList not found=\"" + Name + "\"." );
				errFlag = true;
			}
		}

	}

	void
	SetupNodeVarsForReporting()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called when the indicated number of
		// Nodes have been found (TOTAL NODE NUMBER) or when HVAC warmup is
		// complete, whichever condition is reached first.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataErrorTracking::AbortProcessing; // used here to determine if this routine called during fatal error processing
		using DataContaminantBalance::Contaminant;

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
		int NumNode; // Loop Variable
		int Count0;
		std::string ChrOut;
		std::string ChrOut1;
		std::string ChrOut2;

		// Formats
		static gio::Fmt Format_700( "('! #Nodes,<Number of Unique Nodes>')" );
		static gio::Fmt Format_701( "(A)" );
		static gio::Fmt Format_702( "('! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>')" );
		static gio::Fmt Format_703( "('! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>')" );
		static gio::Fmt fmtLD( "*" );

		if ( ! NodeVarsSetup ) {
			if ( ! AbortProcessing ) {
				MoreNodeInfo.allocate( NumOfUniqueNodeNames );
				for ( NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode ) {
					// Setup Report variables for the Nodes for HVAC Reporting, CurrentModuleObject='Node Name'
					SetupOutputVariable( "System Node Temperature [C]", Node( NumNode ).Temp, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRate, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Humidity Ratio [kgWater/kgDryAir]", Node( NumNode ).HumRat, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint Temperature [C]", Node( NumNode ).TempSetPoint, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint High Temperature [C]", Node( NumNode ).TempSetPointHi, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint Low Temperature [C]", Node( NumNode ).TempSetPointLo, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint Humidity Ratio [kgWater/kgDryAir]", Node( NumNode ).HumRatSetPoint, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint Minimum Humidity Ratio [kgWater/kgDryAir]", Node( NumNode ).HumRatMin, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Setpoint Maximum Humidity Ratio [kgWater/kgDryAir]", Node( NumNode ).HumRatMax, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Relative Humidity [%]", MoreNodeInfo( NumNode ).RelHumidity, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Pressure [Pa]", Node( NumNode ).Press, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Standard Density Volume Flow Rate [m3/s]", MoreNodeInfo( NumNode ).VolFlowRateStdRho, "System", "Average", NodeID( NumNode ) );
					if ( Node( NumNode ).FluidType == NodeType_Air || Node( NumNode ).FluidType == NodeType_Water ) { // setup volume flow rate report for actual/current density
						SetupOutputVariable( "System Node Current Density Volume Flow Rate [m3/s]", MoreNodeInfo( NumNode ).VolFlowRateCrntRho, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Current Density [kg/m3]", MoreNodeInfo( NumNode ).Density, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Specific Heat [J/kg-K]", MoreNodeInfo( NumNode ).SpecificHeat, "System", "Average", NodeID( NumNode ) );
					}

					SetupOutputVariable( "System Node Enthalpy [J/kg]", MoreNodeInfo( NumNode ).ReportEnthalpy, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Wetbulb Temperature [C]", MoreNodeInfo( NumNode ).WetBulbTemp, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Dewpoint Temperature [C]", MoreNodeInfo( NumNode ).AirDewPointTemp, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Quality []", Node( NumNode ).Quality, "System", "Average", NodeID( NumNode ) );
					SetupOutputVariable( "System Node Height [m]", Node( NumNode ).Height, "System", "Average", NodeID( NumNode ) );
					if ( DisplayAdvancedReportVariables ) {
						SetupOutputVariable( "System Node Minimum Temperature [C]", Node( NumNode ).TempMin, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Maximum Temperature [C]", Node( NumNode ).TempMax, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Minimum Limit Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateMin, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Maximum Limit Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateMax, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Minimum Available Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateMinAvail, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Maximum Available Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateMaxAvail, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Setpoint Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateSetPoint, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Requested Mass Flow Rate [kg/s]", Node( NumNode ).MassFlowRateRequest, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Last Timestep Temperature [C]", Node( NumNode ).TempLastTimestep, "System", "Average", NodeID( NumNode ) );
						SetupOutputVariable( "System Node Last Timestep Enthalpy [J/kg]", Node( NumNode ).EnthalpyLastTimestep, "System", "Average", NodeID( NumNode ) );

					}
					if ( Contaminant.CO2Simulation ) {
						SetupOutputVariable( "System Node CO2 Concentration [ppm]", Node( NumNode ).CO2, "System", "Average", NodeID( NumNode ) );
					}
					if ( Contaminant.GenericContamSimulation ) {
						SetupOutputVariable( "System Node Generic Air Contaminant Concentration [ppm]", Node( NumNode ).GenContam, "System", "Average", NodeID( NumNode ) );
					}
				}
			}
			NodeVarsSetup = true;

			gio::write( OutputFileBNDetails, Format_701 ) << "! This file shows details about the branches, nodes, and other";
			gio::write( OutputFileBNDetails, Format_701 ) << "! elements of the flow connections.";
			gio::write( OutputFileBNDetails, Format_701 ) << "! This file is intended for use in \"debugging\" potential problems";
			gio::write( OutputFileBNDetails, Format_701 ) << "! that may also be detected by the program, but may be more easily";
			gio::write( OutputFileBNDetails, Format_701 ) << "! identified by \"eye\".";
			gio::write( OutputFileBNDetails, Format_701 ) << "! This file is also intended to support software which draws a";
			gio::write( OutputFileBNDetails, Format_701 ) << "! schematic diagram of the HVAC system.";
			gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
			// Show the node names on the Branch-Node Details file
			gio::write( OutputFileBNDetails, Format_700 );
			gio::write( ChrOut, fmtLD ) << NumOfUniqueNodeNames;
			gio::write( OutputFileBNDetails, Format_701 ) << " #Nodes," + stripped( ChrOut );
			if ( NumOfUniqueNodeNames > 0 ) {
				gio::write( OutputFileBNDetails, Format_702 );
			}
			Count0 = 0;
			for ( NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode ) {
				gio::write( ChrOut, fmtLD ) << NumNode;
				strip( ChrOut );
				gio::write( ChrOut1, fmtLD ) << NodeRef( NumNode );
				strip( ChrOut1 );
				ChrOut2 = ValidNodeFluidTypes( Node( NumNode ).FluidType );
				gio::write( OutputFileBNDetails, Format_701 ) << " Node," + ChrOut + ',' + NodeID( NumNode ) + ',' + ChrOut2 + ',' + ChrOut1;
				if ( NodeRef( NumNode ) == 0 ) ++Count0;
			}
			// Show suspicious node names on the Branch-Node Details file
			if ( Count0 > 0 ) {
				gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
				gio::write( OutputFileBNDetails, Format_701 ) << "! Suspicious nodes have 0 references.  It is normal for some nodes, however.";
				gio::write( OutputFileBNDetails, Format_701 ) << "! Listing nodes with 0 references (culled from previous list):";
				gio::write( OutputFileBNDetails, Format_703 );
				for ( NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode ) {
					if ( NodeRef( NumNode ) > 0 ) continue;
					gio::write( ChrOut, fmtLD ) << NumNode;
					strip( ChrOut );
					gio::write( ChrOut1, fmtLD ) << NodeRef( NumNode );
					strip( ChrOut1 );
					ChrOut2 = ValidNodeFluidTypes( Node( NumNode ).FluidType );
					gio::write( OutputFileBNDetails, Format_701 ) << " Suspicious Node," + ChrOut + ',' + NodeID( NumNode ) + ',' + ChrOut2 + ',' + ChrOut1;
				}
			}
		}

	}

	void
	GetNodeListsInput( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the Node Lists from the IDF and fills the
		// Node List Data Structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetNodeListsInput: " );
		static std::string const CurrentModuleObject( "NodeList" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Variable
		int Loop1; // Loop Variable
		int Loop2; // Loop Variable
		int NumAlphas; // Number of alphas in IDF item
		int NumNumbers; // Number of numerics in IDF item
		int IOStatus; // IOStatus for IDF item (not checked)
		int NCount; // Actual number of node lists
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool flagError; // true when error node list name should be output
		Array1D_string cAlphas;
		Array1D< Real64 > rNumbers;

		ErrorsFound = false;
		GetObjectDefMaxArgs( CurrentModuleObject, NCount, NumAlphas, NumNumbers );
		cAlphas.allocate( NumAlphas );
		rNumbers.allocate( NumNumbers );
		NumOfNodeLists = GetNumObjectsFound( CurrentModuleObject );
		NodeLists.allocate( NumOfNodeLists );
		for ( int i = 1; i <= NumOfNodeLists; ++i ) {
			NodeLists( i ).Name.clear();
			NodeLists( i ).NumOfNodesInList = 0;
		}

		NCount = 0;
		for ( Loop = 1; Loop <= NumOfNodeLists; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, cAlphas, NumAlphas, rNumbers, NumNumbers, IOStatus );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphas( 1 ), NodeLists, NCount, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}
			++NCount;
			NodeLists( NCount ).Name = cAlphas( 1 );
			NodeLists( NCount ).NodeNames.allocate( NumAlphas - 1 );
			NodeLists( NCount ).NodeNames = "";
			NodeLists( NCount ).NodeNumbers.allocate( NumAlphas - 1 );
			NodeLists( NCount ).NodeNumbers = 0;
			NodeLists( NCount ).NumOfNodesInList = NumAlphas - 1;
			if ( NumAlphas <= 1 ) {
				if ( NumAlphas == 1 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphas( 1 ) + "\" does not have any nodes." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=<blank> does not have any nodes or nodelist name." );
				}
				ErrorsFound = true;
				continue;
			}
			//  Put all in, then determine unique
			for ( Loop1 = 1; Loop1 <= NumAlphas - 1; ++Loop1 ) {
				NodeLists( NCount ).NodeNames( Loop1 ) = cAlphas( Loop1 + 1 );
				if ( cAlphas( Loop1 + 1 ).empty() ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + cAlphas( 1 ) + "\", blank node name in list." );
					--NodeLists( NCount ).NumOfNodesInList;
					if ( NodeLists( NCount ).NumOfNodesInList <= 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphas( 1 ) + "\" does not have any nodes." );
						ErrorsFound = true;
						break;
					}
					continue;
				}
				NodeLists( NCount ).NodeNumbers( Loop1 ) = AssignNodeNumber( NodeLists( NCount ).NodeNames( Loop1 ), NodeType_Unknown, ErrorsFound );
				if ( SameString( NodeLists( NCount ).NodeNames( Loop1 ), NodeLists( NCount ).Name ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphas( 1 ) + "\", invalid node name in list." );
					ShowContinueError( "... Node " + TrimSigDigits( Loop1 ) + " Name=\"" + cAlphas( Loop1 + 1 ) + "\", duplicates NodeList Name." );
					ErrorsFound = true;
				}
			}
			// Error on any duplicates
			flagError = true;
			for ( Loop1 = 1; Loop1 <= NodeLists( NCount ).NumOfNodesInList; ++Loop1 ) {
				for ( Loop2 = Loop1 + 1; Loop2 <= NodeLists( NCount ).NumOfNodesInList; ++Loop2 ) {
					if ( NodeLists( NCount ).NodeNumbers( Loop1 ) != NodeLists( NCount ).NodeNumbers( Loop2 ) ) continue;
					if ( flagError ) { // only list nodelist name once
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphas( 1 ) + "\" has duplicate nodes:" );
						flagError = false;
					}
					ShowContinueError( "...list item=" + TrimSigDigits( Loop1 ) + ", \"" + NodeID( NodeLists( NCount ).NodeNumbers( Loop1 ) ) + "\", duplicate list item=" + TrimSigDigits( Loop2 ) + ", \"" + NodeID( NodeLists( NCount ).NodeNumbers( Loop2 ) ) + "\"." );
					ErrorsFound = true;
				}
			}
		}

		for ( Loop = 1; Loop <= NumOfNodeLists; ++Loop ) {
			for ( Loop2 = 1; Loop2 <= NodeLists( Loop ).NumOfNodesInList; ++Loop2 ) {
				for ( Loop1 = 1; Loop1 <= NumOfNodeLists; ++Loop1 ) {
					if ( Loop == Loop1 ) continue; // within a nodelist have already checked to see if node name duplicates nodelist name
					if ( ! SameString( NodeLists( Loop ).NodeNames( Loop2 ), NodeLists( Loop1 ).Name ) ) continue;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + NodeLists( Loop1 ).Name + "\", invalid node name in list." );
					ShowContinueError( "... Node " + TrimSigDigits( Loop2 ) + " Name=\"" + NodeLists( Loop ).NodeNames( Loop2 ) + "\", duplicates NodeList Name." );
					ShowContinueError( "... NodeList=\"" + NodeLists( Loop1 ).Name + "\", is duplicated." );
					ShowContinueError( "... Items in NodeLists must not be the name of another NodeList." );
					ErrorsFound = true;
				}
			}
		}

		cAlphas.deallocate();
		rNumbers.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + CurrentModuleObject + ": Error getting input - causes termination." );
		}

	}

	int
	AssignNodeNumber(
		std::string const & Name, // Name for assignment
		int const NodeFluidType, // must be valid
		bool & ErrorsFound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function assigns a node number to this name.

		// METHODOLOGY EMPLOYED:
		// Look to see if a name has already been entered.  Use the index of
		// the array as the node number, if there.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int AssignNodeNumber;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int NumNode( 0 ); // Loop Variable
		static std::string cNodeFluidType;

		if ( NodeFluidType != NodeType_Air && NodeFluidType != NodeType_Water && NodeFluidType != NodeType_Electric && NodeFluidType != NodeType_Steam && NodeFluidType != NodeType_Unknown ) {
			gio::write( cNodeFluidType, fmtLD ) << NodeFluidType;
			strip( cNodeFluidType );
			ShowSevereError( "AssignNodeNumber: Invalid FluidType=" + cNodeFluidType );
			ErrorsFound = true;
			ShowFatalError( "AssignNodeNumber: Preceding issue causes termination." );
		}

		NumNode = 0;
		if ( NumOfUniqueNodeNames > 0 ) {
			NumNode = FindItemInList( Name, NodeID( {1,NumOfUniqueNodeNames} ), NumOfUniqueNodeNames );
			if ( NumNode > 0 ) {
				AssignNodeNumber = NumNode;
				++NodeRef( NumNode );
				if ( NodeFluidType != NodeType_Unknown ) {
					if ( Node( NumNode ).FluidType != NodeFluidType && Node( NumNode ).FluidType != NodeType_Unknown ) {
						ShowSevereError( "Existing Fluid type for node, incorrect for request. Node=" + NodeID( NumNode ) );
						ShowContinueError( "Existing Fluid type=" + ValidNodeFluidTypes( Node( NumNode ).FluidType ) + ", Requested Fluid Type=" + ValidNodeFluidTypes( NodeFluidType ) );
						ErrorsFound = true;
					}
				}
				if ( Node( NumNode ).FluidType == NodeType_Unknown ) {
					Node( NumNode ).FluidType = NodeFluidType;
				}
			} else {
				++NumOfUniqueNodeNames;
				NumOfNodes = NumOfUniqueNodeNames;

				Node.redimension( NumOfNodes );
				NodeID.redimension( {0,NumOfNodes} );
				NodeRef.redimension( NumOfNodes );
				MarkedNode.redimension( NumOfNodes );
				// Set new item in Node
				Node( NumOfNodes ).FluidType = NodeFluidType;
				NodeRef( NumOfNodes ) = 0;
				NodeID( NumOfUniqueNodeNames ) = Name;

				AssignNodeNumber = NumOfUniqueNodeNames;
			}
		} else {
			Node.allocate( 1 );
			Node( 1 ).FluidType = NodeFluidType;
			// Allocate takes care of defining
			NumOfNodes = 1;
			NodeID.allocate( {0,1} );
			NodeRef.allocate( 1 );
			MarkedNode.allocate( 1 );

			NumOfUniqueNodeNames = 1;
			NodeID( 0 ) = "Undefined";
			NodeID( NumOfUniqueNodeNames ) = Name;
			AssignNodeNumber = 1;
			NodeRef( 1 ) = 0;
		}

		return AssignNodeNumber;

	}

	int
	GetOnlySingleNode(
		std::string const & NodeName,
		bool & errFlag,
		std::string const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		std::string const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_string_const InputFieldName // Input Field Name
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie; adapted from GasAbsorptionChiller;Jason Glazer
		//       DATE WRITTEN   December 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function gets a single node (or error message results) using the
		// node id from the input file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetSingleNodeResult;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOnlySingleNode: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int NumNodes;
		static Array1D_int NodeNums;
		int FluidType;
		std::string ConnectionType;
		static bool firstTime( true );
		int NumParams;
		int NumAlphas;
		int NumNums;

		if ( firstTime ) {
			GetObjectDefMaxArgs( "NodeList", NumParams, NumAlphas, NumNums );
			NodeNums.dimension( NumParams, 0 );
			firstTime = false;
		}

		FluidType = NodeFluidType;

		GetNodeNums( NodeName, NumNodes, NodeNums, errFlag, FluidType, NodeObjectType, NodeObjectName, NodeConnectionType, NodeFluidStream, ObjectIsParent, _, InputFieldName );

		if ( NumNodes > 1 ) {
			ShowSevereError( RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data." );
			if ( present( InputFieldName ) ) ShowContinueError( "...Ref field=" + InputFieldName );
			ShowContinueError( "Only 1st Node used from NodeList=\"" + NodeName + "\"." );
			ShowContinueError( "...a Nodelist may not be valid in this context." );
			errFlag = true;
		} else if ( NumNodes == 0 ) {
			NodeNums( 1 ) = 0;
		}
		if ( NumNodes > 0 ) {
			if ( NodeConnectionType >= 1 && NodeConnectionType <= NumValidConnectionTypes ) {
				ConnectionType = ValidConnectionTypes( NodeConnectionType );
			} else {
				ConnectionType = TrimSigDigits( NodeConnectionType ) + "-unknown";
			}
			//    CALL RegisterNodeConnection(NodeNums(1),NodeID(NodeNums(1)),NodeObjectType,NodeObjectName,  &
			//                                  ConnectionType,NodeFluidStream,ObjectIsParent,errFlag)
		}

		GetSingleNodeResult = NodeNums( 1 );

		return GetSingleNodeResult;

	}

	void
	InitUniqueNodeCheck( std::string const & ContextName )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine begins a process of checking for unique node names
		// in a sequence of nodes.

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
		bool errFlag;

		// Begin set up of Uniqueness context

		if ( GetNodeInputFlag ) {
			GetNodeListsInput( errFlag );
			GetNodeInputFlag = false;
		}

		if ( ! CurCheckContextName.empty() ) {
			ShowFatalError( "Init Uniqueness called for \"" + ContextName + ", but checks for \"" + CurCheckContextName + "\" was already in progress." );
		}
		if ( ContextName == BlankString ) {
			ShowFatalError( "Init Uniqueness called with Blank Context Name" );
		}
		if ( allocated( UniqueNodeNames ) ) {
			UniqueNodeNames.deallocate();
		}

		NumCheckNodes = 0;
		MaxCheckNodes = 100;
		UniqueNodeNames.allocate( MaxCheckNodes );
		CurCheckContextName = ContextName;

	}

	void
	CheckUniqueNodes(
		std::string const & NodeTypes,
		std::string const & CheckType,
		bool & ErrorsFound,
		Optional_string_const CheckName,
		Optional_int_const CheckNumber,
		Optional_string_const ObjectName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks the appropriate input argument for uniqueness.
		// Call CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber)
		// NodeTypes - used in error message (if any produced)
		// CheckType - "NodeName' or 'NodeNumber' (only 1 can be input per time)
		// ErrorsFound - true if error found by routine
		// CheckName - NodeName entered
		// CheckNumber - Node Number entered
		// only 1 of CheckName or CheckNumber need be entered.
		// ObjectName - "Name" field of object (i.e., CurCheckContextName)

		// METHODOLOGY EMPLOYED:
		// checks the current list of items for this (again)

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
		int Found;

		{ auto const nodeType( CheckType );

		if ( nodeType == "NodeName" ) {
			if ( ! present( CheckName ) ) {
				ShowFatalError( "Routine CheckUniqueNodes called with Nodetypes=NodeName, but did not include CheckName argument." );
			}
			if ( ! CheckName().empty() ) {
				Found = FindItemInList( CheckName, UniqueNodeNames, NumCheckNodes );
				if ( Found != 0 ) {
					ShowSevereError( CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found." );
					ShowContinueError( "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + CheckName + "\"." );
					ShowContinueError( "...Nodes must be unique across instances of this object." );
					//          CALL ShowSevereError('Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(CheckName))
					//          CALL ShowContinueError('Context='//TRIM(CurCheckContextName))
					ErrorsFound = true;
				} else {
					++NumCheckNodes;
					if ( NumCheckNodes > MaxCheckNodes ) {
						UniqueNodeNames.redimension( MaxCheckNodes += 100 );
					}
					UniqueNodeNames( NumCheckNodes ) = CheckName;
				}
			}

		} else if ( nodeType == "NodeNumber" ) {
			if ( ! present( CheckNumber ) ) {
				ShowFatalError( "Routine CheckUniqueNodes called with Nodetypes=NodeNumber, but did not include CheckNumber argument." );
			}
			if ( CheckNumber != 0 ) {
				Found = FindItemInList( NodeID( CheckNumber ), UniqueNodeNames, NumCheckNodes );
				if ( Found != 0 ) {
					ShowSevereError( CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found." );
					ShowContinueError( "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + NodeID( CheckNumber ) + "\"." );
					ShowContinueError( "...Nodes must be unique across instances of this object." );
					//          CALL ShowSevereError('Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(NodeID(CheckNumber)))
					//          CALL ShowContinueError('Context='//TRIM(CurCheckContextName))
					ErrorsFound = true;
				} else {
					++NumCheckNodes;
					if ( NumCheckNodes > MaxCheckNodes ) {
						UniqueNodeNames.redimension( MaxCheckNodes += 100 );
					}
					UniqueNodeNames( NumCheckNodes ) = NodeID( CheckNumber );
				}
			}

		} else {
			ShowFatalError( "CheckUniqueNodes called with invalid Check Type=" + CheckType );
			ErrorsFound = true;

		}}

	}

	void
	EndUniqueNodeCheck( std::string const & ContextName )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine marks the end of a unique node check.

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
		// na

		if ( CurCheckContextName != ContextName ) {
			ShowFatalError( "End Uniqueness called for \"" + ContextName + ", but checks for \"" + CurCheckContextName + "\" was in progress." );
		}
		if ( ContextName == BlankString ) {
			ShowFatalError( "End Uniqueness called with Blank Context Name" );
		}
		CurCheckContextName = BlankString;
		if ( allocated( UniqueNodeNames ) ) {
			UniqueNodeNames.deallocate();
		}

	}

	void
	CalcMoreNodeInfo()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate additional node information for reporting

		// METHODOLOGY EMPLOYED:
		// Input is the existing node data plus environment variables. Output is
		// stored in MoreNodeInfo.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdBaroPress;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::StdRhoAir;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::CPCW;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTdpFnWPb;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataGlobals::InitConvTemp;
		using OutputProcessor::ReqReportVariables;
		using OutputProcessor::ReqRepVars;
		using OutputProcessor::NumOfReqVariables;
		using ScheduleManager::GetCurrentScheduleValue;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetGlycolNameByIndex;
		using FluidProperties::NumOfGlycols;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMoreNodeInfo" );
		static std::string const NodeReportingCalc( "NodeReportingCalc:" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iNode; // node loop index
		int iReq; // requested report variables loop index

		static Real64 RhoAirStdInit;
		static Real64 RhoWaterStdInit;
		static Array1D_int NodeWetBulbSchedPtr;
		static Array1D_bool NodeRelHumidityRepReq;
		static Array1D_int NodeRelHumiditySchedPtr;
		static Array1D_bool NodeDewPointRepReq;
		static Array1D_int NodeDewPointSchedPtr;
		static Array1D_bool NodeSpecificHeatRepReq;
		static Array1D_int NodeSpecificHeatSchedPtr;
		static std::vector<std::string> nodeReportingStrings;
		static std::vector<std::string> nodeFluidNames;
		bool ReportWetBulb;
		bool ReportRelHumidity;
		bool ReportDewPoint;
		bool ReportSpecificHeat;
		Real64 SteamDensity;
		Real64 EnthSteamInDry;
		Real64 RhoAirCurrent; // temporary value for current air density f(baro, db , W)
		//  REAL(r64)     :: rRhoVapor
		//  INTEGER,save :: Count=0
		Real64 rho;
		Real64 Cp;
		Real64 rhoStd;

		if ( CalcMoreNodeInfoMyOneTimeFlag ) {
			RhoAirStdInit = StdRhoAir;
			RhoWaterStdInit = RhoH2O( InitConvTemp );
			NodeWetBulbRepReq.allocate( NumOfNodes );
			NodeWetBulbSchedPtr.allocate( NumOfNodes );
			NodeRelHumidityRepReq.allocate( NumOfNodes );
			NodeRelHumiditySchedPtr.allocate( NumOfNodes );
			NodeDewPointRepReq.allocate( NumOfNodes );
			NodeDewPointSchedPtr.allocate( NumOfNodes );
			NodeSpecificHeatRepReq.allocate( NumOfNodes );
			NodeSpecificHeatSchedPtr.allocate( NumOfNodes );
			nodeReportingStrings.reserve( NumOfNodes );
			nodeFluidNames.reserve( NumOfNodes );
			NodeWetBulbRepReq = false;
			NodeWetBulbSchedPtr = 0;
			NodeRelHumidityRepReq = false;
			NodeRelHumiditySchedPtr = 0;
			NodeDewPointRepReq = false;
			NodeDewPointSchedPtr = 0;
			NodeSpecificHeatRepReq = false;
			NodeSpecificHeatSchedPtr = 0;

			for ( iNode = 1; iNode <= NumOfNodes; ++iNode ) {
				nodeReportingStrings.push_back( std::string( NodeReportingCalc + NodeID( iNode ) ) );
				nodeFluidNames.push_back( GetGlycolNameByIndex( Node( iNode ).FluidIndex ) );
				for ( iReq = 1; iReq <= NumOfReqVariables; ++iReq ) {
					if ( SameString( ReqRepVars( iReq ).Key, NodeID( iNode ) ) || ReqRepVars( iReq ).Key.empty() ) {
						if ( SameString( ReqRepVars( iReq ).VarName, "System Node Wetbulb Temperature" ) ) {
							NodeWetBulbRepReq( iNode ) = true;
							NodeWetBulbSchedPtr( iNode ) = ReqRepVars( iReq ).SchedPtr;
						} else if ( SameString( ReqRepVars( iReq ).VarName, "System Node Relative Humidity" ) ) {
							NodeRelHumidityRepReq( iNode ) = true;
							NodeRelHumiditySchedPtr( iNode ) = ReqRepVars( iReq ).SchedPtr;
						} else if ( SameString( ReqRepVars( iReq ).VarName, "System Node Dewpoint Temperature" ) ) {
							NodeDewPointRepReq( iNode ) = true;
							NodeDewPointSchedPtr( iNode ) = ReqRepVars( iReq ).SchedPtr;
						} else if ( SameString( ReqRepVars( iReq ).VarName, "System Node Specific Heat" ) ) {
							NodeSpecificHeatRepReq( iNode ) = true;
							NodeSpecificHeatSchedPtr( iNode ) = ReqRepVars( iReq ).SchedPtr;
						}
					}
				}
				if ( EMSManager::CheckIfNodeMoreInfoSensedByEMS( iNode, "System Node Wetbulb Temperature" ) ) {
					NodeWetBulbRepReq( iNode ) = true;
					NodeWetBulbSchedPtr( iNode ) = 0;
				}
				if ( EMSManager::CheckIfNodeMoreInfoSensedByEMS( iNode, "System Node Relative Humidity" ) ) {
					NodeRelHumidityRepReq( iNode ) = true;
					NodeRelHumiditySchedPtr( iNode ) = 0;
				}
				if ( EMSManager::CheckIfNodeMoreInfoSensedByEMS( iNode, "System Node Dewpoint Temperature" ) ) {
					NodeDewPointRepReq( iNode ) = true;
					NodeDewPointSchedPtr( iNode ) = 0;
				}
				if ( EMSManager::CheckIfNodeMoreInfoSensedByEMS( iNode, "System Node Specific Heat" ) ) {
					NodeSpecificHeatRepReq( iNode ) = true;
					NodeSpecificHeatSchedPtr( iNode ) = 0;
				}


			}
			CalcMoreNodeInfoMyOneTimeFlag = false;
		}

		for ( iNode = 1; iNode <= NumOfNodes; ++iNode ) {
			ReportWetBulb = false;
			ReportRelHumidity = false;
			ReportDewPoint = false;
			ReportSpecificHeat = false;
			if ( NodeWetBulbRepReq( iNode ) && NodeWetBulbSchedPtr( iNode ) > 0 ) {
				ReportWetBulb = ( GetCurrentScheduleValue( NodeWetBulbSchedPtr( iNode ) ) > 0.0 );
			} else if ( NodeWetBulbRepReq( iNode ) && NodeWetBulbSchedPtr( iNode ) == 0 ) {
				ReportWetBulb = true;
			} else if ( Node( iNode ).SPMNodeWetBulbRepReq ) {
				ReportWetBulb = true;
			}
			if ( NodeRelHumidityRepReq( iNode ) && NodeRelHumiditySchedPtr( iNode ) > 0 ) {
				ReportRelHumidity = ( GetCurrentScheduleValue( NodeRelHumiditySchedPtr( iNode ) ) > 0.0 );
			} else if ( NodeRelHumidityRepReq( iNode ) && NodeRelHumiditySchedPtr( iNode ) == 0 ) {
				ReportRelHumidity = true;
			}
			if ( NodeDewPointRepReq( iNode ) && NodeDewPointSchedPtr( iNode ) > 0 ) {
				ReportDewPoint = ( GetCurrentScheduleValue( NodeDewPointSchedPtr( iNode ) ) > 0.0 );
			} else if ( NodeDewPointRepReq( iNode ) && NodeDewPointSchedPtr( iNode ) == 0 ) {
				ReportDewPoint = true;
			}
			if ( NodeSpecificHeatRepReq( iNode ) && NodeSpecificHeatSchedPtr( iNode ) > 0 ) {
				ReportSpecificHeat = ( GetCurrentScheduleValue( NodeSpecificHeatSchedPtr( iNode ) ) > 0.0 );
			} else if ( NodeSpecificHeatRepReq( iNode ) && NodeSpecificHeatSchedPtr( iNode ) == 0 ) {
				ReportSpecificHeat = true;
			}
			// calculate the volume flow rate
			if ( Node( iNode ).FluidType == NodeType_Air ) {
				MoreNodeInfo( iNode ).VolFlowRateStdRho = Node( iNode ).MassFlowRate / RhoAirStdInit;
				// if Node%Press was reliable could be used here.
				RhoAirCurrent = PsyRhoAirFnPbTdbW( OutBaroPress, Node( iNode ).Temp, Node( iNode ).HumRat );
				MoreNodeInfo( iNode ).Density = RhoAirCurrent;
				if ( RhoAirCurrent != 0.0 ) MoreNodeInfo( iNode ).VolFlowRateCrntRho = Node( iNode ).MassFlowRate / RhoAirCurrent;
				MoreNodeInfo( iNode ).ReportEnthalpy = PsyHFnTdbW( Node( iNode ).Temp, Node( iNode ).HumRat );
				if ( ReportWetBulb ) {
					// if Node%Press was reliable could be used here.
					MoreNodeInfo( iNode ).WetBulbTemp = PsyTwbFnTdbWPb( Node( iNode ).Temp, Node( iNode ).HumRat, OutBaroPress, nodeReportingStrings[iNode - 1] );
				} else {
					MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
				}
				if ( ReportDewPoint ) {
					MoreNodeInfo( iNode ).AirDewPointTemp = PsyTdpFnWPb( Node( iNode ).HumRat, OutBaroPress );
				} else {
					MoreNodeInfo( iNode ).AirDewPointTemp = 0.0;
				}
				if ( ReportRelHumidity ) {
					// if Node%Press was reliable could be used here.
					// following routines don't issue psych errors and may be more reliable.
					MoreNodeInfo( iNode ).RelHumidity = 100.0 * PsyRhFnTdbWPb( Node( iNode ).Temp, Node( iNode ).HumRat, OutBaroPress, nodeReportingStrings[iNode - 1] );
					//        rRhoVapor=PsyRhovFnTdbWPb(Node(iNode)%Temp,Node(iNode)%HumRat,OutBaroPress,'NodeReportingCalc:'//TRIM(NodeID(iNode)))
					//        MoreNodeInfo(iNode)%RelHumidity = 100.0 * PsyRhFnTdbRhov(Node(iNode)%Temp,rRhoVapor,  &
					//              'NodeReportingCalc:'//TRIM(NodeID(iNode)))

				} else {
					MoreNodeInfo( iNode ).RelHumidity = 0.0;
				}
				if ( ReportSpecificHeat ) { //only call psych routine if needed.
					MoreNodeInfo( iNode ).SpecificHeat = PsyCpAirFnWTdb( Node( iNode ).HumRat,  Node( iNode ).Temp );
				} else {
					MoreNodeInfo( iNode ).SpecificHeat = 0.0;
				}
			} else if ( Node( iNode ).FluidType == NodeType_Water ) {

				if ( ! ( ( Node( iNode ).FluidIndex > 0 ) && ( Node( iNode ).FluidIndex <= NumOfGlycols ) ) ) {
					rho = RhoWaterStdInit;
					rhoStd = RhoWaterStdInit;
					Cp = CPCW( Node( iNode ).Temp );
				} else {
					Cp = GetSpecificHeatGlycol( nodeFluidNames[iNode - 1], Node( iNode ).Temp, Node( iNode ).FluidIndex, nodeReportingStrings[iNode - 1] );
					rhoStd = GetDensityGlycol( nodeFluidNames[iNode - 1], InitConvTemp, Node( iNode ).FluidIndex, nodeReportingStrings[iNode - 1] );
					rho = GetDensityGlycol( nodeFluidNames[iNode - 1], Node( iNode ).Temp, Node( iNode ).FluidIndex, nodeReportingStrings[iNode - 1] );
				}

				MoreNodeInfo( iNode ).VolFlowRateStdRho = Node( iNode ).MassFlowRate / rhoStd;
				MoreNodeInfo( iNode ).VolFlowRateCrntRho = Node( iNode ).MassFlowRate / rho;
				MoreNodeInfo( iNode ).Density = rho;
				MoreNodeInfo( iNode ).ReportEnthalpy = Cp * Node( iNode ).Temp;
				MoreNodeInfo( iNode ).SpecificHeat =  Cp; //always fill since cp already always being calculated anyway
				MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
				MoreNodeInfo( iNode ).RelHumidity = 100.0;
			} else if ( Node( iNode ).FluidType == NodeType_Steam ) {
				if ( Node( iNode ).Quality == 1.0 ) {
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, Node( iNode ).Temp, Node( iNode ).Quality, Node( iNode ).FluidIndex, RoutineName );
					EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, Node( iNode ).Temp, Node( iNode ).Quality, Node( iNode ).FluidIndex, RoutineName );
					MoreNodeInfo( iNode ).VolFlowRateStdRho = Node( iNode ).MassFlowRate / SteamDensity;
					MoreNodeInfo( iNode ).ReportEnthalpy = EnthSteamInDry;
					MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
					MoreNodeInfo( iNode ).RelHumidity = 0.0;
				} else if ( Node( iNode ).Quality == 0.0 ) { //The node has condensate water through it
					MoreNodeInfo( iNode ).VolFlowRateStdRho = Node( iNode ).MassFlowRate / RhoWaterStdInit;
					MoreNodeInfo( iNode ).ReportEnthalpy = CPCW( Node( iNode ).Temp ) * Node( iNode ).Temp;
					MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
					MoreNodeInfo( iNode ).RelHumidity = 0.0;
				}
			} else if ( Node( iNode ).FluidType == NodeType_Electric ) {
				MoreNodeInfo( iNode ).VolFlowRateStdRho = 0.0;
				MoreNodeInfo( iNode ).ReportEnthalpy = 0.0;
				MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
				MoreNodeInfo( iNode ).RelHumidity = 0.0;
				MoreNodeInfo( iNode ).SpecificHeat = 0.0;
			} else {
				MoreNodeInfo( iNode ).VolFlowRateStdRho = Node( iNode ).MassFlowRate / RhoAirStdInit;
				if ( Node( iNode ).HumRat > 0.0 ) {
					MoreNodeInfo( iNode ).ReportEnthalpy = PsyHFnTdbW( Node( iNode ).Temp, Node( iNode ).HumRat );
					if ( ReportWetBulb ) {
						MoreNodeInfo( iNode ).WetBulbTemp = PsyTwbFnTdbWPb( Node( iNode ).Temp, Node( iNode ).HumRat, StdBaroPress );
					} else {
						MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
					}
					if ( ReportSpecificHeat ) {
						MoreNodeInfo( iNode ).SpecificHeat = PsyCpAirFnWTdb( Node( iNode ).HumRat,  Node( iNode ).Temp );
					} else {
						MoreNodeInfo( iNode ).SpecificHeat = 0.0;
					}
				} else {
					MoreNodeInfo( iNode ).ReportEnthalpy = CPCW( Node( iNode ).Temp ) * Node( iNode ).Temp;
					MoreNodeInfo( iNode ).WetBulbTemp = 0.0;
					MoreNodeInfo( iNode ).SpecificHeat = 0.0;
				}
			}
		}

	}

	void
	MarkNode(
		int const NodeNumber, // Node Number to be marked
		std::string const & ObjectType,
		std::string const & ObjectName,
		std::string const & FieldName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine marks a node -- this node needs to exist in more than one object.

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
		// na

		MarkedNode( NodeNumber ).IsMarked = true;
		MarkedNode( NodeNumber ).ObjectType = ObjectType;
		MarkedNode( NodeNumber ).ObjectName = ObjectName;
		MarkedNode( NodeNumber ).FieldName = FieldName;

	}

	void
	CheckMarkedNodes( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks "marked" nodes.

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
		int NodeNum;

		for ( NodeNum = 1; NodeNum <= NumOfNodes; ++NodeNum ) {
			if ( MarkedNode( NodeNum ).IsMarked ) {
				if ( NodeRef( NodeNum ) == 0 ) {
					ShowSevereError( "Node=\"" + NodeID( NodeNum ) + "\" did not find reference by another object." );
					ShowContinueError( "Object=\"" + MarkedNode( NodeNum ).ObjectType + "\", Name=\"" + MarkedNode( NodeNum ).ObjectName + "\", Field=[" + MarkedNode( NodeNum ).FieldName + ']' );
					ErrorsFound = true;
				}
			}
		}

	}

} // NodeInputManager

} // EnergyPlus
