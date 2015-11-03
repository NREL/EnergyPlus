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

	Real64
	OutdoorDryBulbGrad(
		Real64 DryBulbTemp,
		Real64 UpperBound,
		Real64 HiGradient,
		Real64 LowerBound,
		Real64 LowGradient
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

} // RoomAirModelUserTempPattern

} // EnergyPlus

#endif
