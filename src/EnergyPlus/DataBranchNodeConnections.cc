// EnergyPlus Headers
#include <DataBranchNodeConnections.hh>

namespace EnergyPlus {

namespace DataBranchNodeConnections {

	// Module containing the routines dealing with the Branch-Node connections (Component sets, Node Connections)

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   May 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This is a public data only module for Branch/Node connection data.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumCompSets( 0 ); // Number of Component Sets found in branches
	int NumNodeConnectionErrors( 0 ); // Count of node connection errors

	int NumOfNodeConnections( 0 );
	int MaxNumOfNodeConnections( 0 );
	int NodeConnectionAlloc( 1000 );
	int NumOfActualParents( 0 );
	int NumOfAirTerminalNodes( 0 );
	int MaxNumOfAirTerminalNodes( 0 );
	int EqNodeConnectionAlloc( 100 );

	// Object Data
	Array1D< ComponentListData > CompSets;
	Array1D< ParentListData > ParentNodeList;
	Array1D< NodeConnectionDef > NodeConnections;
	Array1D< EqNodeConnectionDef > AirTerminalNodeConnections;

	// Clears the global data in DataBranchNodeConnections.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumCompSets = 0;
		NumNodeConnectionErrors = 0;
		NumOfNodeConnections = 0;
		MaxNumOfNodeConnections = 0;
		NodeConnectionAlloc = 1000;
		NumOfActualParents = 0;
		NumOfAirTerminalNodes = 0;
		MaxNumOfAirTerminalNodes = 0;
		EqNodeConnectionAlloc = 100;
		CompSets.deallocate();
		ParentNodeList.deallocate();
		NodeConnections.deallocate();
		AirTerminalNodeConnections.deallocate();
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

} // DataBranchNodeConnections

} // EnergyPlus
