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

} // NodeInputManager

} // EnergyPlus

#endif
