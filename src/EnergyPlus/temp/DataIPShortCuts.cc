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
	FArray1D_string cAlphaFieldNames;
	FArray1D_string cNumericFieldNames;
	FArray1D_bool lNumericFieldBlanks;
	FArray1D_bool lAlphaFieldBlanks;
	FArray1D_string cAlphaArgs;
	FArray1D< Real64 > rNumericArgs;
	std::string cCurrentModuleObject;

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

} // DataIPShortCuts

} // EnergyPlus
