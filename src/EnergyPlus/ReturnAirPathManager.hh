#ifndef ReturnAirPathManager_hh_INCLUDED
#define ReturnAirPathManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ReturnAirPathManager {

	// Data
	//MODULE PARAMETER DEFINITIONS
	// na

	//DERIVED TYPE DEFINITIONS
	// na

	//MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE ReturnAirPathManager

	// Functions

	void
	SimReturnAirPath();

	void
	GetReturnAirPathInput();

	void
	InitReturnAirPath( int & ReturnAirPathNum ); // unused1208

	void
	CalcReturnAirPath( int & ReturnAirPathNum );

	void
	ReportReturnAirPath( int & ReturnAirPathNum ); // unused1208

} // ReturnAirPathManager

} // EnergyPlus

#endif
