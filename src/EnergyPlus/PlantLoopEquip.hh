#ifndef PlantLoopEquip_hh_INCLUDED
#define PlantLoopEquip_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantLoopEquip {

	// Data
	// SUBROUTINE SPECIFICATION

	// Functions

	void
	SimPlantEquip(
		int const LoopNum, // loop counter
		int const LoopSideNum, // loop counter
		int const BranchNum,
		int const Num,
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
		bool & InitLoopEquip,
		bool const GetCompSizFac // Tells component routine to return the component sizing fraction
	);

} // PlantLoopEquip

} // EnergyPlus

#endif
