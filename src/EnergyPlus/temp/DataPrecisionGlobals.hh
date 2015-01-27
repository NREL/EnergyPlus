#ifndef DataPrecisionGlobals_hh_INCLUDED
#define DataPrecisionGlobals_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataPrecisionGlobals {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const i32; // 6 digits
	extern int const i64; // 12 digits
	extern int const r32;
	extern int const r64;
	extern int const default_prec;
	extern Real64 const constant_zero;
	extern Real64 const constant_one;
	extern Real64 const constant_minusone;
	extern Real64 const constant_twenty;
	extern Real64 const constant_pointfive;
	extern Real64 const EXP_LowerLimit; // In IVF=2.061153622438558d-009 - used 20
	// because it's already used in other parts of the code
	extern Real64 const EXP_UpperLimit; // In IVF=2.353852668370200d+017

} // DataPrecisionGlobals

} // EnergyPlus

#endif
