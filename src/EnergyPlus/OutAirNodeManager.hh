#ifndef OutAirNodeManager_hh_INCLUDED
#define OutAirNodeManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace OutAirNodeManager {

	// Data
	//MODULE PARAMETER DEFINITIONS:

	//Type declarations in OutAirNodeManager module

	//MODULE VARIABLE DECLARATIONS:

	extern Array1D_int OutsideAirNodeList; // List of all outside air inlet nodes
	extern int NumOutsideAirNodes; // Number of single outside air nodes
	extern bool GetOutAirNodesInputFlag; // Flag set to make sure you get input once

	//SUBROUTINE SPECIFICATIONS FOR MODULE OutAirNodeManager

	// Functions

	// Clears the global data in OutAirNodeManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	SetOutAirNodes();

	void
	GetOutAirNodesInput();

	void
	InitOutAirNodes();

	bool
	CheckOutAirNodeNumber( int const NodeNumber ); // Number of node to check to see if in Outside Air list

	void
	CheckAndAddAirNodeNumber(
		int const NodeNumber, // Number of node to check to see if in Outside Air list
		bool & Okay // True if found, false if not
	);

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

} // OutAirNodeManager

} // EnergyPlus

#endif
