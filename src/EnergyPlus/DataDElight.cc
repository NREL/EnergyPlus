// EnergyPlus Headers
#include <DataDElight.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataDElight {

	// MODULE INFORMATION:
	//       AUTHOR         Robert J. Hitchcock
	//       DATE WRITTEN   August 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for variables used in daylighting
	// calculations by DElight.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	Real64 const M2FT( 3.280840 ); // Length:   Meters * M2FT = Feet
	Real64 const FT22M2( 0.09290304 ); // Area:     SquareFeet * FT22M2 = SquareMeter
	Real64 const M22FT2( 1.0 / FT22M2 ); // Area:     SquareMeter * M22FT2 = SquareFeet
	Real64 const M32FT3( 35.3147 ); // Volume:       CubicMeter * M32FT3 = CubicFeet
	Real64 const LUX2FC( 0.09290304 ); // Illuminance:  Lux * LUX2FC = Footcandles

	// INTERFACE BLOCK SPECIFICATIONS
	// na

} // DataDElight

} // EnergyPlus
