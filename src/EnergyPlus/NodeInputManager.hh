#ifndef NodeInputManager_hh_INCLUDED
#define NodeInputManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace NodeInputManager {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;
	using DataLoopNode::MarkedNodeData;
	using DataLoopNode::NodeData;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Fstring const Blank;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumOfNodeLists; // Total number of Node Lists in IDF
	extern int NumOfUniqueNodeNames; // Number of Unique Node Names (current)
	// The following is a module level flag because there are several possible "entries" into
	// this module that may need to get the Node Inputs.
	extern bool GetNodeInputFlag; // Flag to Get Node Input(s)
	extern FArray1D_Fstring TmpNodeID; // Used to "reallocate" name arrays
	extern FArray1D_int NodeRef; // Number of times a Node is "referenced"
	extern FArray1D_int TmpNodeRef; // used to reallocate
	extern Fstring CurCheckContextName; // Used in Uniqueness checks
	extern FArray1D_Fstring UniqueNodeNames; // used in uniqueness checks
	extern int NumCheckNodes; // Num of Unique nodes in check
	extern int MaxCheckNodes; // Current "max" unique nodes in check
	extern bool NodeVarsSetup; // Setup indicator of node vars for reporting (also that all nodes have been entered)
	extern FArray1D_bool NodeWetBulbRepReq;

	// Types

	struct NodeListDef // Derived Type for Node Lists
	{
		// Members
		Fstring Name; // Name of this Node List
		int NumOfNodesInList; // Number of Nodes in this Node List
		FArray1D_Fstring NodeNames; // List of Names in this Node List
		FArray1D_int NodeNumbers; // Number of each Node (ref NodeNames) in this Node List

		// Default Constructor
		NodeListDef() :
			Name( MaxNameLength ),
			NumOfNodesInList( 0 ),
			NodeNames( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		NodeListDef(
			Fstring const & Name, // Name of this Node List
			int const NumOfNodesInList, // Number of Nodes in this Node List
			FArray1_Fstring const & NodeNames, // List of Names in this Node List
			FArray1_int const & NodeNumbers // Number of each Node (ref NodeNames) in this Node List
		) :
			Name( MaxNameLength, Name ),
			NumOfNodesInList( NumOfNodesInList ),
			NodeNames( NodeNames ),
			NodeNumbers( NodeNumbers )
		{}

	};

	// Object Data
	extern FArray1D< NodeListDef > NodeLists; // Node Lists
	extern FArray1D< NodeData > TmpNode; // Used to "reallocate" Node Structure
	extern FArray1D< MarkedNodeData > TmpMarkedNode; // Marked nodes must exist somewhere else

	// Functions

	void
	GetNodeNums(
		Fstring const & Name, // Name for which to obtain information
		int & NumNodes, // Number of nodes accompanying this Name
		FArray1S_int NodeNumbers, // Node Numbers accompanying this Name
		bool & ErrorsFound, // True when errors are found...
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		Fstring const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		Fstring const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_bool_const IncrementFluidStream = _, // True/False
		Optional_Fstring_const InputFieldName = _ // Input Field Name
	);

	void
	GetNodeList(
		Fstring const & Name, // Node List Name for which information is obtained
		int & NumNodes, // Number of nodes accompanying this Name
		FArray1S_int NodeNumbers, // NodeNumbers accompanying this Name
		bool & errFlag, // Set to true when requested Node List not found
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		Fstring const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		Fstring const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_Fstring_const InputFieldName = _ // Input Field Name
	);

	void
	SetupNodeVarsForReporting();

	void
	GetNodeListsInput( bool & ErrorsFound );

	int
	AssignNodeNumber(
		Fstring const & Name, // Name for assignment
		int const NodeFluidType, // must be valid
		bool & ErrorsFound
	);

	int
	GetOnlySingleNode(
		Fstring const & NodeName,
		bool & errFlag,
		Fstring const & NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
		Fstring const & NodeObjectName, // Node Object Name (i.e. "MyChiller")
		int const NodeFluidType, // Fluidtype for checking/setting node FluidType
		int const NodeConnectionType, // Node Connection Type (see DataLoopNode)
		int const NodeFluidStream, // Which Fluid Stream (1,2,3,...)
		bool const ObjectIsParent, // True/False
		Optional_Fstring_const InputFieldName = _ // Input Field Name
	);

	void
	InitUniqueNodeCheck( Fstring const & ContextName );

	void
	CheckUniqueNodes(
		Fstring const & NodeTypes,
		Fstring const & CheckType,
		bool & ErrorsFound,
		Optional_Fstring_const CheckName = _,
		Optional_int_const CheckNumber = _,
		Optional_Fstring_const ObjectName = _
	);

	void
	EndUniqueNodeCheck( Fstring const & ContextName );

	void
	CalcMoreNodeInfo();

	void
	MarkNode(
		int const NodeNumber, // Node Number to be marked
		Fstring const & ObjectType,
		Fstring const & ObjectName,
		Fstring const & FieldName
	);

	void
	CheckMarkedNodes( bool & ErrorsFound );

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // NodeInputManager

} // EnergyPlus

#endif
