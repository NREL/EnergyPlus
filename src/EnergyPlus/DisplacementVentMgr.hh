#ifndef DisplacementVentMgr_hh_INCLUDED
#define DisplacementVentMgr_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DisplacementVentMgr {

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 HAT_MX; // HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
	extern Real64 HA_MX; // HA_MX Convection Coefficient times Area for the upper subzone
	extern Real64 HAT_OC; // HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
	extern Real64 HA_OC; // HA_OC Convection Coefficient times Area for the lower subzone
	extern Real64 HAT_FLOOR; // HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
	extern Real64 HA_FLOOR; // HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
	extern Real64 HeightFloorSubzoneTop; // Assumed thickness of floor subzone
	extern Real64 ThickOccupiedSubzoneMin; // Minimum thickness of occupied subzone
	extern Real64 HeightIntMass; // Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
	extern Real64 HeightIntMassDefault; // Default height of internal mass surfaces

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageUCSDDVModel( int const ZoneNum ); // index number for the specified zone

	//**************************************************************************************************

	void
	InitUCSDDV( int const ZoneNum );

	//**************************************************************************************************

	void
	HcUCSDDV(
		int const ZoneNum,
		Real64 const FractionHeight
	);

	//**************************************************************************************************

	void
	CalcUCSDDV( int const ZoneNum ); // Which Zonenum

} // DisplacementVentMgr

} // EnergyPlus

#endif
