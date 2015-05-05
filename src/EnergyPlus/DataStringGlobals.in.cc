// EnergyPlus Headers
#include <DataStringGlobals.hh>

namespace EnergyPlus {

namespace DataStringGlobals {

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for string variables used in parsing
	// "pieces" of EnergyPlus.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// None!--This module is USEd by other modules; it should not USE anything.

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	std::string const UpperCase( "ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİ" );
	std::string const LowerCase( "abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïğñòóôõöøùúûüı" );
	std::string const AccentedUpperCase( "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİ" );
	std::string const AccentedLowerCase( "àáâãäåæçèéêëìíîïğñòóôõöøùúûüı" );
	std::string const AllCase( "àáâãäåæçèéêëìíîïğñòóôõöøùúûüıÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" );
#ifdef _WIN32
	std::string const NL( "\r\n" ); // Platform newline
#else
	std::string const NL( "\n" ); // Platform newline
#endif
#ifdef _WIN32
	char const pathChar( '\\' );
	char const altpathChar( '/' );
#elif __linux__
	char const pathChar( '/' );
	char const altpathChar( '\\' );
#elif __unix__
	char const pathChar( '/' );
	char const altpathChar( '\\' );
#elif __posix__
	char const pathChar( '/' );
	char const altpathChar( '\\' );
#elif __APPLE__
	char const pathChar( '/' );
	char const altpathChar( '\\' );
#else
#error "Invalid platform detection in DataStringGlobals."
#endif
	char const CharComma( ',' ); // comma
	char const CharSemicolon( ';' ); // semicolon
	char const CharTab( '\t' ); // tab
	char const CharSpace( ' ' ); // space

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	std::string outputAuditFileName("eplusout.audit");
	std::string outputBndFileName("eplusout.bnd");
	std::string outputDxfFileName("eplusout.dxf");
	std::string outputEioFileName("eplusout.eio");
	std::string outputEndFileName("eplusout.end");
	std::string outputErrFileName("eplusout.err");
	std::string outputEsoFileName("eplusout.eso");
	std::string outputMtdFileName("eplusout.mtd");
	std::string outputMddFileName("eplusout.mdd");
	std::string outputMtrFileName("eplusout.mtr");
	std::string outputRddFileName("eplusout.rdd");
	std::string outputShdFileName("eplusout.shd");
	std::string outputTblCsvFileName("eplustbl.csv");
	std::string outputTblHtmFileName("eplustbl.htm");
	std::string outputTblTabFileName("eplustbl.tab");
	std::string outputTblTxtFileName("eplustbl.txt");
	std::string outputTblXmlFileName("eplustbl.xml");
	std::string inputIdfFileName;
	std::string inputIddFileName;
	std::string inputWeatherFileName;
	std::string outputAdsFileName("eplusADS.out");
	std::string outputDfsFileName("eplusout.dfs");
	std::string outputDelightInFileName("eplusout.delightin");
	std::string outputDelightOutFileName("eplusout.delightout");
	std::string outputDelightEldmpFileName("eplusout.delighteldmp");
	std::string outputDelightDfdmpFileName("eplusout.delightdfdmp");
	std::string outputMapTabFileName("eplusmap.tab");
	std::string outputMapCsvFileName("eplusmap.csv");
	std::string outputMapTxtFileName("eplusmap.txt");
	std::string outputEddFileName("eplusout.edd");
	std::string outputIperrFileName("eplusout.iperr");
	std::string outputDbgFileName("eplusout.dbg");
	std::string outputSlnFileName("eplusout.sln");
	std::string outputSciFileName("eplusout.sci");
	std::string outputWrlFileName("eplusout.wrl");
	std::string outputZszCsvFileName("epluszsz.csv");
	std::string outputZszTabFileName("epluszsz.tab");
	std::string outputZszTxtFileName("epluszsz.txt");
	std::string outputSszCsvFileName("eplusssz.csv");
	std::string outputSszTabFileName("eplusssz.tab");
	std::string outputSszTxtFileName("eplusssz.txt");
	std::string outputScreenCsvFileName("eplusscreen.csv");
	std::string outputSqlFileName("eplusout.sql");
	std::string outputSqliteErrFileName("eplussqlite.err");
	std::string EnergyPlusIniFileName;
	std::string inStatFileName;
	std::string TarcogIterationsFileName("TarcogIterations.dbg");
	std::string eplusADSFileName;
	std::string outputCsvFileName("eplusout.csv");
	std::string outputMtrCsvFileName("eplusmtr.csv");
	std::string outputRvauditFileName("eplusout.rvaudit");

	std::string idfFileNameOnly;
	std::string idfDirPathName;
	std::string exeDirectory;

	std::string ProgramPath; // Path for Program from INI file
	std::string CurrentWorkingFolder; // Current working directory for run
	std::string IDDVerString; // Version information from the IDD (line 1)
	std::string VerString( "EnergyPlus, Version ${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}" ); // String that represents version information
	std::string MatchVersion( "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}" ); // String to be matched by Version object
	std::string CurrentDateTime; // For printing current date and time at start of run

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

} // DataStringGlobals

} // EnergyPlus
