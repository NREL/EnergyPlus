#ifndef UFADManager_hh_INCLUDED
#define UFADManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace UFADManager {

	// Data
	// MODULE VARIABLE DECLARATIONS:
	extern Real64 HAT_MX; // HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
	extern Real64 HAT_MXWin; // HAT_MX Convection Coefficient times Area times Temperature for the upper subzone (windows only)
	extern Real64 HA_MX; // HA_MX Convection Coefficient times Area for the upper subzone
	extern Real64 HA_MXWin; // HA_MX Convection Coefficient times Area for the upper subzone (windows only)
	extern Real64 HAT_OC; // HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
	extern Real64 HAT_OCWin; // HAT_OC Convection Coefficient times Area times Temperature for the lower subzone (windows only)
	extern Real64 HA_OC; // HA_OC Convection Coefficient times Area for the lower subzone
	extern Real64 HA_OCWin; // HA_OC Convection Coefficient times Area for the lower subzone (windows only)
	extern Real64 HAT_FLOOR; // HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
	extern Real64 HA_FLOOR; // HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
	extern Real64 HeightFloorSubzoneTop; // Assumed thickness of floor subzone
	extern Real64 ThickOccupiedSubzoneMin; // Minimum thickness of occupied subzone
	extern Real64 HeightIntMass; // Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
	extern Real64 HeightIntMassDefault; // Default height of internal mass surfaces

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageUCSDUFModels(
		int const ZoneNum, // index number for the specified zone
		int const ZoneModelType // type of zone model; UCSDUFI = 6
	);

	void
	InitUCSDUF(
		int const ZoneNum,
		int const ZoneModelType // type of zone model; UCSDUFI = 6
	);

	void
	SizeUCSDUF(
		int const ZoneNum,
		int const ZoneModelType // type of zone model; UCSDUFI = 6
	);

	void
	HcUCSDUF(
		int const ZoneNum,
		Real64 const FractionHeight
	);

	void
	CalcUCSDUI( int const ZoneNum ); // index number for the specified zone

	void
	CalcUCSDUE( int const ZoneNum ); // index number for the specified zone

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // UFADManager

} // EnergyPlus

#endif
