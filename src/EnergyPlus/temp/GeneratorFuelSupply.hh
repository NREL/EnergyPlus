#ifndef GeneratorFuelSupply_hh_INCLUDED
#define GeneratorFuelSupply_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace GeneratorFuelSupply {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// <name Public routines, optionally name Private routines within this module>

	// Functions

	void
	GetGeneratorFuelSupplyInput();

	//******************************************************************************

	void
	SetupFuelConstituentData(
		int const FuelSupplyNum,
		bool & ErrorsFound
	);

} // GeneratorFuelSupply

} // EnergyPlus

#endif
