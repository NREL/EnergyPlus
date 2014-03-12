#ifndef BranchNodeConnections_hh_INCLUDED
#define BranchNodeConnections_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace BranchNodeConnections {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Fstring const Blank;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Functions

	void
	RegisterNodeConnection(
		int const NodeNumber, // Number for this Node
		Fstring const & NodeName, // Name of this Node
		Fstring const & ObjectType, // Type of object this Node is connected to (e.g. Chiller:Electric)
		Fstring const & ObjectName, // Name of object this Node is connected to (e.g. MyChiller)
		Fstring const & ConnectionType, // Connection Type for this Node (must be valid)
		int const FluidStream, // Count on Fluid Streams
		bool const IsParent, // True when node is a parent node
		bool & errFlag, // Will be True if errors already detected or if errors found here
		Optional_Fstring_const InputFieldName = _ // Input Field Name
	);

	bool
	IsValidConnectionType( Fstring const & ConnectionType );

	void
	CheckNodeConnections( bool & ErrorsFound );

	bool
	IsParentObject(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	int
	WhichParentSet(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	void
	GetParentData(
		Fstring const & ComponentType,
		Fstring const & ComponentName,
		Fstring & InletNodeName,
		int & InletNodeNum,
		Fstring & OutletNodeName,
		int & OutletNodeNum,
		bool & ErrorsFound
	);

	bool
	IsParentObjectCompSet(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	int
	WhichCompSet(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	int
	WhichParentCompSet(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	int
	GetNumChildren(
		Fstring const & ComponentType,
		Fstring const & ComponentName
	);

	void
	GetComponentData(
		Fstring const & ComponentType,
		Fstring const & ComponentName,
		bool & IsParent,
		int & NumInlets,
		FArray1D_Fstring & InletNodeNames,
		FArray1D_int & InletNodeNums,
		FArray1D_int & InletFluidStreams,
		int & NumOutlets,
		FArray1D_Fstring & OutletNodeNames,
		FArray1D_int & OutletNodeNums,
		FArray1D_int & OutletFluidStreams,
		bool & ErrorsFound
	);

	void
	GetChildrenData(
		Fstring const & ComponentType,
		Fstring const & ComponentName,
		int & NumChildren,
		FArray1S_Fstring ChildrenCType,
		FArray1S_Fstring ChildrenCName,
		FArray1S_Fstring InletNodeName,
		FArray1S_int InletNodeNum,
		FArray1S_Fstring OutletNodeName,
		FArray1S_int OutletNodeNum,
		bool & ErrorsFound
	);

	void
	SetUpCompSets(
		Fstring const & ParentType, // Parent Object Type
		Fstring const & ParentName, // Parent Object Name
		Fstring const & CompType, // Component Type
		Fstring const & CompName, // Component Name
		Fstring const & InletNode, // Inlet Node Name
		Fstring const & OutletNode, // Outlet Node Name
		Optional_Fstring_const Description = _ // Description
	);

	void
	TestInletOutletNodes( bool & ErrorsFound );

	void
	TestCompSet(
		Fstring const & CompType, // Component Type
		Fstring const & CompName, // Component Name
		Fstring const & InletNode, // Inlet Node Name
		Fstring const & OutletNode, // Outlet Node Name
		Fstring const & Description // Description of Node Pair (for warning message)
	);

	void
	TestCompSetInletOutletNodes( bool & ErrorsFound );

	void
	GetNodeConnectionType(
		int const NodeNumber,
		FArray1D_int & NodeConnectType,
		bool & errFlag
	);

	void
	FindAllNumbersInList(
		int const WhichNumber,
		FArray1A_int const ListOfItems,
		int const NumItems,
		int & CountOfItems, // Number of items found
		FArray1D_int & AllNumbersInList // Index array to all numbers found
	);

	template< typename A >
	inline
	void
	FindAllNumbersInList(
		int const WhichNumber,
		MArray1< A, int > const & ListOfItems,
		int const NumItems,
		int & CountOfItems, // Number of items found
		FArray1D_int & AllNumbersInList // Index array to all numbers found
	)
	{
		FindAllNumbersInList( WhichNumber, FArray1D_int( ListOfItems ), NumItems, CountOfItems, AllNumbersInList );
	}

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

} // BranchNodeConnections

} // EnergyPlus

#endif
