#ifndef HeatBalanceMovableInsulation_hh_INCLUDED
#define HeatBalanceMovableInsulation_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceMovableInsulation {

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceMovableInsulation

	// Functions

	void
	EvalOutsideMovableInsulation(
		int const SurfNum, // DO loop counter for surfaces
		Real64 & HMovInsul, // Resistance or "h" value of movable insulation
		int & RoughIndexMovInsul, // Roughness index of movable insulation
		Real64 & AbsExt // Absorptivity of outer most layer
	);

	void
	EvalInsideMovableInsulation(
		int const SurfNum, // DO loop counter for surfaces
		Real64 & HMovInsul, // Resistance or "h" value of movable insulation
		Real64 & AbsInt // Inside solar absorptance of movable insulation
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

} // HeatBalanceMovableInsulation

} // EnergyPlus

#endif
