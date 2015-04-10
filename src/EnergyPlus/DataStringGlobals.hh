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

	extern std::string outputAuditFileName;
	extern std::string outputBndFileName;
	extern std::string outputDxfFileName;
	extern std::string outputEioFileName;
	extern std::string outputEndFileName;
	extern std::string outputErrFileName;
	extern std::string outputEsoFileName;
	extern std::string outputMtdFileName;
	extern std::string outputMddFileName;
	extern std::string outputMtrFileName;
	extern std::string outputRddFileName;
	extern std::string outputShdFileName;
	extern std::string outputTblCsvFileName;
	extern std::string outputTblHtmFileName;
	extern std::string outputTblTabFileName;
	extern std::string outputTblTxtFileName;
	extern std::string outputTblXmlFileName;
	extern std::string inputIdfFileName;
	extern	std::string inputIddFileName;
	extern	std::string inputWeatherFileName;
	extern std::string outputAdsFileName;
	extern std::string outputDfsFileName;
	extern std::string outputDelightInFileName;
	extern std::string outputDelightOutFileName;
	extern std::string outputDelightEldmpFileName;
	extern std::string outputDelightDfdmpFileName;
	extern std::string outputMapTabFileName;
	extern std::string outputMapCsvFileName;
	extern std::string outputMapTxtFileName;
	extern std::string outputEddFileName;
	extern std::string outputIperrFileName;
	extern std::string outputDbgFileName;
	extern std::string outputSlnFileName;
	extern std::string outputSciFileName;
	extern std::string outputWrlFileName;
	extern std::string outputZszCsvFileName;
	extern std::string outputZszTabFileName;
	extern std::string outputZszTxtFileName;
	extern std::string outputSszCsvFileName;
	extern std::string outputSszTabFileName;
	extern std::string outputSszTxtFileName;
	extern std::string outputScreenCsvFileName;
	extern std::string outputSqlFileName;
	extern std::string outputSqliteErrFileName;
	extern std::string EnergyPlusIniFileName;
	extern std::string inStatFileName;
	extern std::string TarcogIterationsFileName;
	extern std::string eplusADSFileName;
	extern std::string outputCsvFileName;
	extern std::string outputMtrCsvFileName;
	extern std::string outputRvauditFileName;

	extern std::string weatherFileNameOnly;
	extern std::string idfDirPathName;
	extern std::string idfFileNameOnly;
	extern std::string exeDirectory;

	// MODULE PARAMETER DEFINITIONS:
	extern std::string const UpperCase;
	extern std::string const LowerCase;
	extern std::string const AccentedUpperCase;
	extern std::string const AccentedLowerCase;
	extern std::string const AllCase;
	extern std::string const NL; // Platform newline
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
	extern std::string ProgramPath; // Path for Program from INI file
	extern std::string CurrentWorkingFolder; // Current working directory for run
	extern std::string FullName; // Full name of file to open, including path
	extern std::string IDDVerString; // Version information from the IDD (line 1)
	extern std::string VerString; // String that represents version information
	extern std::string MatchVersion; // String to be matched by Version object
	extern std::string CurrentDateTime; // For printing current date and time at start of run

} // DataStringGlobals

} // EnergyPlus

#endif
