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

} // DataDaylightingDevices

} // EnergyPlus
