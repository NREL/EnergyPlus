#ifndef RoomAirModelUserTempPattern_hh_INCLUDED
#define RoomAirModelUserTempPattern_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace RoomAirModelUserTempPattern {

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// MODULE DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// see DataRoomAir

	// SUBROUTINE SPECIFICATIONS FOR MODULE TempDistSimMgr

	// main subsroutine

	// get input routines are in RoomAirManager.cc

	// Routines for transferring data between Heat Balance and Air model domains

	// Routines for actual calculations in TempDist model

	// Functions

	void
	ManageUserDefinedPatterns( int const ZoneNum ); // index number for the specified zone

	//****************************************************

	void
	InitTempDistModel( int const ZoneNum ); // index number for the specified zone

	void
	GetSurfHBDataForTempDistModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

	void
	CalcTempDistModel( int const ZoneNum ); // index number for the specified zone

	void
	FigureSurfMapPattern(
		int const PattrnID,
		int const ZoneNum
	);

	void
	FigureHeightPattern(
		int const PattrnID,
		int const ZoneNum
	);

	void
	FigureTwoGradInterpPattern(
		int const PattrnID,
		int const ZoneNum
	);

	void
	FigureConstGradPattern(
		int const PattrnID,
		int const ZoneNum
	);

	//*****************************************************************************************

	Real64
	FigureNDheightInZone( int const thisHBsurf ); // index in main Surface array

	//***************************************************

	void
	SetSurfHBDataForTempDistModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

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

} // RoomAirModelUserTempPattern

} // EnergyPlus

#endif
