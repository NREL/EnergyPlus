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

} // OutAirNodeManager

} // EnergyPlus

#endif
