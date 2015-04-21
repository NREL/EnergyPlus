// EnergyPlus Headers
#include <DataDaylightingDevices.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataDaylightingDevices {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   May 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for variables used in daylighting devices which
	// are shared by several modules.

	// METHODOLOGY EMPLOYED: na
	// REFERENCES: na
	// OTHER NOTES: na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const MaxTZones( 10 ); // Maximum number of transition zones
	int const NumOfAngles( 19 ); // Number of data points on transmittance vs. angle curve

	int const VisibleBeam( 1 ); // Constant for radiation type
	int const SolarBeam( 2 ); // Constant for radiation type
	int const SolarAniso( 3 ); // Constant for radiation type
	int const SolarIso( 4 ); // Constant for radiation type

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	int NumOfTDDPipes( 0 ); // Number of TDD pipes in the input file
	int NumOfShelf( 0 ); // Number of daylighting shelves in the input file

	// Object Data
	Array1D< TDDPipeData > TDDPipe;
	Array1D< ShelfData > Shelf;

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

} // DataDaylightingDevices

} // EnergyPlus
