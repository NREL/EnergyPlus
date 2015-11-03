// EnergyPlus Headers
#include <DataIPShortCuts.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataIPShortCuts {

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   July 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a data holder for field names to be passed into
	// input processing GetObject routines so that individual Get routines do not have
	// to create them. These will be passed in rather than filled by the GetObject
	// routines automatically.  The field names are used for error messages. They are
	// dimensioned to the max alpha/numeric found in the IDD.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing

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
	Array1D_string cAlphaFieldNames;
	Array1D_string cNumericFieldNames;
	Array1D_bool lNumericFieldBlanks;
	Array1D_bool lAlphaFieldBlanks;
	Array1D_string cAlphaArgs;
	Array1D< Real64 > rNumericArgs;
	std::string cCurrentModuleObject;

	// Clears the global data in DataIPShortCuts.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		cAlphaFieldNames.deallocate();
		cAlphaArgs.deallocate();
		lAlphaFieldBlanks.deallocate();
		cNumericFieldNames.deallocate();
		rNumericArgs.deallocate();
		lNumericFieldBlanks.deallocate();
	}

} // DataIPShortCuts

} // EnergyPlus
