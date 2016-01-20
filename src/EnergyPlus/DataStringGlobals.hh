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
