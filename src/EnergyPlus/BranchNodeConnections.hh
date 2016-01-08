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

#ifndef BranchNodeConnections_hh_INCLUDED
#define BranchNodeConnections_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

// Forward
namespace DataBranchNodeConnections {
	struct NodeConnectionDef;
}

namespace BranchNodeConnections {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

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
		Optional_string_const InputFieldName = _ // Input Field Name
	);

	bool
	IsValidConnectionType( std::string const & ConnectionType );

	void
	CheckNodeConnections( bool & ErrorsFound );

	bool
	IsParentObject(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

	int
	WhichParentSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

	void
	GetParentData(
		std::string const & ComponentType,
		std::string const & ComponentName,
		std::string & InletNodeName,
		int & InletNodeNum,
		std::string & OutletNodeName,
		int & OutletNodeNum,
		bool & ErrorsFound
	);

	bool
	IsParentObjectCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

	int
	WhichCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

	int
	WhichParentCompSet(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

	int
	GetNumChildren(
		std::string const & ComponentType,
		std::string const & ComponentName
	);

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
	);

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
	);

	void
	SetUpCompSets(
		std::string const & ParentType, // Parent Object Type
		std::string const & ParentName, // Parent Object Name
		std::string const & CompType, // Component Type
		std::string const & CompName, // Component Name
		std::string const & InletNode, // Inlet Node Name
		std::string const & OutletNode, // Outlet Node Name
		Optional_string_const Description = _ // Description
	);

	void
	TestInletOutletNodes( bool & ErrorsFound );

	void
	TestCompSet(
		std::string const & CompType, // Component Type
		std::string const & CompName, // Component Name
		std::string const & InletNode, // Inlet Node Name
		std::string const & OutletNode, // Outlet Node Name
		std::string const & Description // Description of Node Pair (for warning message)
	);

	void
	TestCompSetInletOutletNodes( bool & ErrorsFound );

	void
	GetNodeConnectionType(
		int const NodeNumber,
		Array1D_int & NodeConnectType,
		bool & errFlag
	);

	void
	FindAllNodeNumbersInList(
		int const WhichNumber,
		Array1< DataBranchNodeConnections::NodeConnectionDef > const & NodeConnections,
		int const NumItems,
		int & CountOfItems, // Number of items found
		Array1D_int & AllNumbersInList // Index array to all numbers found
	);

} // BranchNodeConnections

} // EnergyPlus

#endif
