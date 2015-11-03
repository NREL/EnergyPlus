#ifndef CrossVentMgr_hh_INCLUDED
#define CrossVentMgr_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace CrossVentMgr {

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 HAT_J; // HAT_J Convection Coefficient times Area times Temperature for Jet subzone
	extern Real64 HA_J; // HA_J  Convection Coefficient times Area for Jet subzone
	extern Real64 HAT_R; // HAT_R Convection Coefficient times Area times Temperature for Recirculation subzone
	extern Real64 HA_R; // HA_J  Convection Coefficient times Area for Recirculation subzone
	extern Real64 const Cjet1; // First correlation constant for the jet velocity
	extern Real64 const Cjet2; // Second correlation constant for the jet velocity
	extern Real64 const Crec1; // First correlation constant for the recirculation velocity
	extern Real64 const Crec2; // Second correlation constant for the recirculation velocity
	extern Real64 const CjetTemp; // Correlation constant for the jet temperature rise
	extern Real64 const CrecTemp; // Correlation constant for the recirculation temperature rise
	extern Real64 const CrecFlow1; // First correlation constant for the recirculation flow rate
	extern Real64 const CrecFlow2; // Second correlation constant for the recirculation flow rate

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageUCSDCVModel( int const ZoneNum ); // index number for the specified zone

	//**************************************************************************************************

	void
	InitUCSDCV( int const ZoneNum );

	//**************************************************************************************************

	void
	HcUCSDCV( int const ZoneNum );

	//**************************************************************************************************

	void
	EvolveParaUCSDCV( int const ZoneNum );

	//**************************************************************************************************

	void
	CalcUCSDCV( int const ZoneNum ); // Which Zonenum

} // CrossVentMgr

} // EnergyPlus

#endif
