#ifndef DataIPShortCuts_hh_INCLUDED
#define DataIPShortCuts_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataIPShortCuts {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern FArray1D_string cAlphaFieldNames;
	extern FArray1D_string cNumericFieldNames;
	extern FArray1D_bool lNumericFieldBlanks;
	extern FArray1D_bool lAlphaFieldBlanks;
	extern FArray1D_string cAlphaArgs;
	extern FArray1D< Real64 > rNumericArgs;
	extern std::string cCurrentModuleObject;

} // DataIPShortCuts

} // EnergyPlus

#endif
