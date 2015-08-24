#ifndef DataIPShortCuts_hh_INCLUDED
#define DataIPShortCuts_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

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
	extern Array1D_string cAlphaFieldNames;
	extern Array1D_string cNumericFieldNames;
	extern Array1D_bool lNumericFieldBlanks;
	extern Array1D_bool lAlphaFieldBlanks;
	extern Array1D_string cAlphaArgs;
	extern Array1D< Real64 > rNumericArgs;
	extern std::string cCurrentModuleObject;

	// Clears the global data in DataIPShortCuts.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataIPShortCuts

} // EnergyPlus

#endif
