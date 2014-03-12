#ifndef DataDElight_hh_INCLUDED
#define DataDElight_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataDElight {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const M2FT; // Length:   Meters * M2FT = Feet
	extern Real64 const FT22M2; // Area:     SquareFeet * FT22M2 = SquareMeter
	extern Real64 const M22FT2; // Area:     SquareMeter * M22FT2 = SquareFeet
	extern Real64 const M32FT3; // Volume:       CubicMeter * M32FT3 = CubicFeet
	extern Real64 const LUX2FC; // Illuminance:  Lux * LUX2FC = Footcandles

} // DataDElight

} // EnergyPlus

#endif
