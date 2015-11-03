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

} // HeatBalanceMovableInsulation

} // EnergyPlus

#endif
