#ifndef BranchNodeConnections_hh_INCLUDED
#define BranchNodeConnections_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

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
	FindAllNumbersInList(
		int const WhichNumber,
		Array1A_int const ListOfItems,
		int const NumItems,
		int & CountOfItems, // Number of items found
		Array1D_int & AllNumbersInList // Index array to all numbers found
	);

	template< typename A >
	inline
	void
	FindAllNumbersInList(
		int const WhichNumber,
		MArray1< A, int > const & ListOfItems,
		int const NumItems,
		int & CountOfItems, // Number of items found
		Array1D_int & AllNumbersInList // Index array to all numbers found
	)
	{
		FindAllNumbersInList( WhichNumber, Array1D_int( ListOfItems ), NumItems, CountOfItems, AllNumbersInList );
	}

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

} // BranchNodeConnections

} // EnergyPlus

#endif
