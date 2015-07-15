#ifndef NodeInputManager_hh_INCLUDED
#define NodeInputManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace NodeInputManager {

	// Using/Aliasing
	using DataLoopNode::MarkedNodeData;
	using DataLoopNode::NodeData;

	// Data
	//MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumOfNodeLists; // Total number of Node Lists in IDF
	extern int NumOfUniqueNodeNames; // Number of Unique Node Names (current)
	// The following is a module level flag because there are several possible "entries" into
	// this module that may need to get the Node Inputs.
	extern bool GetNodeInputFlag; // Flag to Get Node Input(s)
	extern Array1D_int NodeRef; // Number of times a Node is "referenced"
	extern std::string CurCheckContextName; // Used in Uniqueness checks
	extern Array1D_string UniqueNodeNames; // used in uniqueness checks
	extern int NumCheckNodes; // Num of Unique nodes in check
	extern int MaxCheckNodes; // Current "max" unique nodes in check
	extern bool NodeVarsSetup; // Setup indicator of node vars for reporting (also that all nodes have been entered)
	extern Array1D_bool NodeWetBulbRepReq;

	// Types

	struct NodeListDef // Derived Type for Node Lists
	{
		// Members
		std::string Name; // Name of this Node List
		int NumOfNodesInList; // Number of Nodes in this Node List
		Array1D_string NodeNames; // List of Names in this Node List
		Array1D_int NodeNumbers; // Number of each Node (ref NodeNames) in this Node List

		// Default Constructor
		NodeListDef() :
			NumOfNodesInList( 0 )
		{}

		// Member Constructor
		NodeListDef(
			std::string const & Name, // Name of this Node List
			int const NumOfNodesInList, // Number of Nodes in this Node List
			Array1_string const & NodeNames, // List of Names in this Node List
			Array1_int const & NodeNumbers // Number of each Node (ref NodeNames) in this Node List
		) :
			Name( Name ),
			NumOfNodesInList( NumOfNodesInList ),
			NodeNames( NodeNames ),
			NodeNumbers( NodeNumbers )
		{}

	};

	// Object Data
	extern Array1D< NodeListDef > NodeLists; // Node Lists

	// Functions

	// Clears the global data in NodeInputManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

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
		Optional_bool_const IncrementFluidStream = _, // True/False
		Optional_string_const InputFieldName = _ // Input Field Name
	);

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
		Optional_string_const InputFieldName = _ // Input Field Name
	);

	void
	SetupNodeVarsForReporting();

	void
	GetNodeListsInput( bool & ErrorsFound );

	int
	AssignNodeNumber(
		std::string const & Name, // Name for assignment
		int const NodeFluidType, // must be valid
		bool & ErrorsFound
	);

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
		Optional_string_const InputFieldName = _ // Input Field Name
	);

	void
	InitUniqueNodeCheck( std::string const & ContextName );

	void
	CheckUniqueNodes(
		std::string const & NodeTypes,
		std::string const & CheckType,
		bool & ErrorsFound,
		Optional_string_const CheckName = _,
		Optional_int_const CheckNumber = _,
		Optional_string_const ObjectName = _
	);

	void
	EndUniqueNodeCheck( std::string const & ContextName );

	void
	CalcMoreNodeInfo();

	void
	MarkNode(
		int const NodeNumber, // Node Number to be marked
		std::string const & ObjectType,
		std::string const & ObjectName,
		std::string const & FieldName
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

} // NodeInputManager

} // EnergyPlus

#endif
