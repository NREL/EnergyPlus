#ifndef DataStringGlobals_hh_INCLUDED
#define DataStringGlobals_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataStringGlobals {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern Fstring const UpperCase;
	extern Fstring const LowerCase;
	extern Fstring const AccentedUpperCase;
	extern Fstring const AccentedLowerCase;
	extern Fstring const AllCase;
	extern Fstring const pathChar;
	extern Fstring const altpathChar;
	extern int const PathLimit;
	extern Fstring const CharComma; // comma
	extern Fstring const CharSemicolon; // semicolon
	extern Fstring const CharTab; // tab
	extern Fstring const CharSpace; // space

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Fstring ProgramPath; // Path for Program from Energy+.ini
	extern Fstring CurrentWorkingFolder; // Current working directory for run
	extern Fstring FullName; // Full name of file to open, including path
	extern Fstring IDDVerString; // Version information from the IDD (line 1)
	extern Fstring VerString; // String that represents version information
	extern Fstring MatchVersion; // String to be matched by Version object
	extern Fstring CurrentDateTime; // For printing current date and time at start of run

} // DataStringGlobals

} // EnergyPlus

#endif
