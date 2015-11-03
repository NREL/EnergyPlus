#ifndef NonZoneEquipmentManager_hh_INCLUDED
#define NonZoneEquipmentManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace NonZoneEquipmentManager {

	// Data
	// MODULE PARAMETER DEFINITIONS: na
	// MODULE VARIABLE DECLARATIONS: na

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageNonZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimNonZoneEquipment // Simulation convergence flag
	);

} // NonZoneEquipmentManager

} // EnergyPlus

#endif
