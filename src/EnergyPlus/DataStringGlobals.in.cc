// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	std::string const UpperCase( "ABCDEFGHIJKLMNOPQRSTUVWXYZ¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›" );
	std::string const LowerCase( "abcdefghijklmnopqrstuvwxyz‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝" );
	std::string const AccentedUpperCase( "¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›" );
	std::string const AccentedLowerCase( "‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝" );
	std::string const AllCase( "‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" );
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

} // DataStringGlobals

} // EnergyPlus
