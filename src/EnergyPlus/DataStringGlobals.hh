#ifndef DataStringGlobals_hh_INCLUDED
#define DataStringGlobals_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataStringGlobals {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern std::string const UpperCase;
	extern std::string const LowerCase;
	extern std::string const AccentedUpperCase;
	extern std::string const AccentedLowerCase;
	extern std::string const AllCase;
	extern char const pathChar;
	extern char const altpathChar;
	extern char const CharComma; // comma
	extern char const CharSemicolon; // semicolon
	extern char const CharTab; // tab
	extern char const CharSpace; // space

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern std::string ProgramPath; // Path for Program from Energy+.ini
	extern std::string CurrentWorkingFolder; // Current working directory for run
	extern std::string FullName; // Full name of file to open, including path
	extern std::string IDDVerString; // Version information from the IDD (line 1)
	extern std::string VerString; // String that represents version information
	extern std::string MatchVersion; // String to be matched by Version object
	extern std::string CurrentDateTime; // For printing current date and time at start of run

} // DataStringGlobals

} // EnergyPlus

#endif
