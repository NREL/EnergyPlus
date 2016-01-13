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

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
}


// C++ Headers
#include <cstdlib>
#include <iostream>
#include <exception>

// ObjexxFCL Headers
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <UtilityRoutines.hh>
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CommandLineInterface.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DaylightingManager.hh>
#include <DisplayRoutines.hh>
#include <ExternalInterface.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <NodeInputManager.hh>
#include <OutputReports.hh>
#include <PlantManager.hh>
#include <SimulationManager.hh>
#include <SolarShading.hh>
#include <SQLiteProcedures.hh>
#include <SystemReports.hh>
#include <Timer.h>

namespace EnergyPlus {

namespace UtilityRoutines {
	bool outputErrorHeader( true );
}

void
AbortEnergyPlus()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   December 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine causes the program to halt due to a fatal error.

	// METHODOLOGY EMPLOYED:
	// Puts a message on output files.
	// Closes files.
	// Stops the program.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataSystemVariables;
	using namespace DataTimings;
	using namespace DataErrorTracking;
	using General::RoundSigDigits;
	using NodeInputManager::SetupNodeVarsForReporting;
	using NodeInputManager::CheckMarkedNodes;
	using BranchInputManager::TestBranchIntegrity;
	using BranchNodeConnections::CheckNodeConnections;
	using BranchNodeConnections::TestCompSetInletOutletNodes;
	using SimulationManager::ReportLoopConnections;
	using SystemReports::ReportAirLoopConnections;
	using SolarShading::ReportSurfaceErrors;
	using PlantManager::CheckPlantOnAbort;
	using ExternalInterface::NumExternalInterfaces;
	using ExternalInterface::CloseSocket;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static gio::Fmt fmtLD( "*" );
	static gio::Fmt OutFmt( "('Press ENTER to continue after reading above message>')" );
	static gio::Fmt ETimeFmt( "(I2.2,'hr ',I2.2,'min ',F5.2,'sec')" );

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int tempfl;
	std::string NumWarnings;
	std::string NumSevere;
	std::string NumWarningsDuringWarmup;
	std::string NumSevereDuringWarmup;
	std::string NumWarningsDuringSizing;
	std::string NumSevereDuringSizing;
	std::string Elapsed;
	int Hours; // Elapsed Time Hour Reporting
	int Minutes; // Elapsed Time Minute Reporting
	Real64 Seconds; // Elapsed Time Second Reporting
	bool ErrFound;
	bool TerminalError;
	int write_stat;

	if ( sqlite ) {
		sqlite->updateSQLiteSimulationRecord( true, false );
	}

	AbortProcessing = true;
	if ( AskForConnectionsReport ) {
		AskForConnectionsReport = false; // Set false here in case any further fatal errors in below processing...

		ShowMessage( "Fatal error -- final processing.  More error messages may appear." );
		SetupNodeVarsForReporting();

		ErrFound = false;
		TerminalError = false;
		TestBranchIntegrity( ErrFound );
		if ( ErrFound ) TerminalError = true;
		TestAirPathIntegrity( ErrFound );
		if ( ErrFound ) TerminalError = true;
		CheckMarkedNodes( ErrFound );
		if ( ErrFound ) TerminalError = true;
		CheckNodeConnections( ErrFound );
		if ( ErrFound ) TerminalError = true;
		TestCompSetInletOutletNodes( ErrFound );
		if ( ErrFound ) TerminalError = true;

		if ( ! TerminalError ) {
			ReportAirLoopConnections();
			ReportLoopConnections();
		}

	} else if ( ! ExitDuringSimulations ) {
		ShowMessage( "Warning:  Node connection errors not checked - most system input has not been read (see previous warning)." );
		ShowMessage( "Fatal error -- final processing.  Program exited before simulations began.  See previous error messages." );
	}

	if ( AskForSurfacesReport ) {
		ReportSurfaces();
	}

	ReportSurfaceErrors();
	CheckPlantOnAbort();
	ShowRecurringErrors();
	SummarizeErrors();
	CloseMiscOpenFiles();
	NumWarnings = RoundSigDigits( TotalWarningErrors );
	strip( NumWarnings );
	NumSevere = RoundSigDigits( TotalSevereErrors );
	strip( NumSevere );
	NumWarningsDuringWarmup = RoundSigDigits( TotalWarningErrorsDuringWarmup );
	strip( NumWarningsDuringWarmup );
	NumSevereDuringWarmup = RoundSigDigits( TotalSevereErrorsDuringWarmup );
	strip( NumSevereDuringWarmup );
	NumWarningsDuringSizing = RoundSigDigits( TotalWarningErrorsDuringSizing );
	strip( NumWarningsDuringSizing );
	NumSevereDuringSizing = RoundSigDigits( TotalSevereErrorsDuringSizing );
	strip( NumSevereDuringSizing );

	// catch up with timings if in middle
	Time_Finish = epElapsedTime();
	if ( Time_Finish < Time_Start ) Time_Finish += 24.0 * 3600.0;
	Elapsed_Time = Time_Finish - Time_Start;
#ifdef EP_Detailed_Timings
	epStopTime( "EntireRun=" );
#endif
	if ( Elapsed_Time < 0.0 ) Elapsed_Time = 0.0;
	Hours = Elapsed_Time / 3600.0;
	Elapsed_Time -= Hours * 3600.0;
	Minutes = Elapsed_Time / 60.0;
	Elapsed_Time -= Minutes * 60.0;
	Seconds = Elapsed_Time;
	if ( Seconds < 0.0 ) Seconds = 0.0;
	gio::write( Elapsed, ETimeFmt ) << Hours << Minutes << Seconds;

	ShowMessage( "EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup + " Severe Errors." );
	ShowMessage( "EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing + " Severe Errors." );
	ShowMessage( "EnergyPlus Terminated--Fatal Error Detected. " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed );
	DisplayString( "EnergyPlus Run Time=" + Elapsed );
	tempfl = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( tempfl, DataStringGlobals::outputEndFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "AbortEnergyPlus: Could not open file "+ DataStringGlobals::outputEndFileName +" for output (write)." );
	}
	gio::write( tempfl, fmtLD ) << "EnergyPlus Terminated--Fatal Error Detected. " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed;

	gio::close( tempfl );
#ifdef EP_Detailed_Timings
	epSummaryTimes( Time_Finish - Time_Start );
#endif
	CloseOutOpenFiles();
	// Close the socket used by ExternalInterface. This call also sends the flag "-1" to the ExternalInterface,
	// indicating that E+ terminated with an error.
	if ( NumExternalInterfaces > 0 ) CloseSocket( -1 );
	std::cerr << "Program terminated: " << "EnergyPlus Terminated--Error(s) Detected." << std::endl;
	std::exit( EXIT_FAILURE );

}

void
CloseMiscOpenFiles()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   December 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine scans potential unit numbers and closes
	// any that are still open.

	// METHODOLOGY EMPLOYED:
	// Use INQUIRE to determine if file is open.

	// REFERENCES:
	// na

	// Using/Aliasing
	using DaylightingManager::CloseReportIllumMaps;
	using DaylightingManager::CloseDFSFile;
	using DataGlobals::OutputFileDebug;
	using DataReportingFlags::DebugOutput;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string DebugPosition;

	//      LOGICAL :: exists, opened
	//      INTEGER :: UnitNumber
	//      INTEGER :: ios

	CloseReportIllumMaps();
	CloseDFSFile();

	//  In case some debug output was produced, it appears that the
	//  position on the INQUIRE will not be 'ASIS' (3 compilers tested)
	//  So, will want to keep....

	{ IOFlags flags; gio::inquire( OutputFileDebug, flags ); DebugPosition = flags.POSITION(); }
	if ( DebugPosition != "ASIS" ) {
		DebugOutput = true;
	}
	if ( DebugOutput ) {
		gio::close( OutputFileDebug );
	} else {
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileDebug, flags ); }
	}

}

void
CloseOutOpenFiles()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   April 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine scans potential unit numbers and closes
	// any that are still open.

	// METHODOLOGY EMPLOYED:
	// Use INQUIRE to determine if file is open.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	int const MaxUnitNumber( 1000 );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	bool exists;
	bool opened;
	int UnitNumber;
	int ios;

	for ( UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber ) {
		{ IOFlags flags; gio::inquire( UnitNumber, flags ); exists = flags.exists(); opened = flags.open(); ios = flags.ios(); }
		if ( exists && opened && ios == 0 ) gio::close( UnitNumber );
	}

}

void
EndEnergyPlus()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   December 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine causes the program to terminate when complete (no errors).

	// METHODOLOGY EMPLOYED:
	// Puts a message on output files.
	// Closes files.
	// Stops the program.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataSystemVariables;
	using namespace DataTimings;
	using namespace DataErrorTracking;
	using General::RoundSigDigits;
	using SolarShading::ReportSurfaceErrors;
	using ExternalInterface::NumExternalInterfaces;
	using ExternalInterface::CloseSocket;
	using ExternalInterface::haveExternalInterfaceBCVTB;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt ETimeFmt( "(I2.2,'hr ',I2.2,'min ',F5.2,'sec')" );

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int tempfl;
	std::string NumWarnings;
	std::string NumSevere;
	std::string NumWarningsDuringWarmup;
	std::string NumSevereDuringWarmup;
	std::string NumWarningsDuringSizing;
	std::string NumSevereDuringSizing;
	std::string Elapsed;
	int Hours; // Elapsed Time Hour Reporting
	int Minutes; // Elapsed Time Minute Reporting
	Real64 Seconds; // Elapsed Time Second Reporting
	int write_stat;

	if ( sqlite ) {
		sqlite->updateSQLiteSimulationRecord( true, true );
	}

	ReportSurfaceErrors();
	ShowRecurringErrors();
	SummarizeErrors();
	CloseMiscOpenFiles();
	NumWarnings = RoundSigDigits( TotalWarningErrors );
	strip( NumWarnings );
	NumSevere = RoundSigDigits( TotalSevereErrors );
	strip( NumSevere );
	NumWarningsDuringWarmup = RoundSigDigits( TotalWarningErrorsDuringWarmup );
	strip( NumWarningsDuringWarmup );
	NumSevereDuringWarmup = RoundSigDigits( TotalSevereErrorsDuringWarmup );
	strip( NumSevereDuringWarmup );
	NumWarningsDuringSizing = RoundSigDigits( TotalWarningErrorsDuringSizing );
	strip( NumWarningsDuringSizing );
	NumSevereDuringSizing = RoundSigDigits( TotalSevereErrorsDuringSizing );
	strip( NumSevereDuringSizing );

	Time_Finish = epElapsedTime();
	if ( Time_Finish < Time_Start ) Time_Finish += 24.0 * 3600.0;
	Elapsed_Time = Time_Finish - Time_Start;
#ifdef EP_Detailed_Timings
	epStopTime( "EntireRun=" );
#endif
	Hours = Elapsed_Time / 3600.0;
	Elapsed_Time -= Hours * 3600.0;
	Minutes = Elapsed_Time / 60.0;
	Elapsed_Time -= Minutes * 60.0;
	Seconds = Elapsed_Time;
	if ( Seconds < 0.0 ) Seconds = 0.0;
	gio::write( Elapsed, ETimeFmt ) << Hours << Minutes << Seconds;

	ShowMessage( "EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup + " Severe Errors." );
	ShowMessage( "EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing + " Severe Errors." );
	ShowMessage( "EnergyPlus Completed Successfully-- " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed );
	DisplayString( "EnergyPlus Run Time=" + Elapsed );
	tempfl = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( tempfl, DataStringGlobals::outputEndFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "EndEnergyPlus: Could not open file " + DataStringGlobals::outputEndFileName + " for output (write)." );
	}
	gio::write( tempfl, fmtA ) << "EnergyPlus Completed Successfully-- " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed;
	gio::close( tempfl );
#ifdef EP_Detailed_Timings
	epSummaryTimes( Time_Finish - Time_Start );
#endif
	CloseOutOpenFiles();
	// Close the ExternalInterface socket. This call also sends the flag "1" to the ExternalInterface,
	// indicating that E+ finished its simulation
	if ( ( NumExternalInterfaces > 0 ) && haveExternalInterfaceBCVTB ) CloseSocket( 1 );
	std::cerr << "EnergyPlus Completed Successfully." << std::endl; std::exit( EXIT_SUCCESS );

}

int
GetNewUnitNumber()
{

	// FUNCTION INFORMATION:
	//       AUTHOR         Linda K. Lawrie, adapted from reference
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS FUNCTION:
	// Returns a unit number of a unit that can exist and is not connected.  Note
	// this routine does not magically mark that unit number in use.  In order to
	// have the unit "used", the source code must OPEN the file.

	// METHODOLOGY EMPLOYED:
	// Use Inquire function to find out if proposed unit: exists or is opened.
	// If not, can be used for a new unit number.

	// REFERENCES:
	// Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
	// Developed at Unicomp, Inc.
	// Permission to use, copy, modify, and distribute this
	// software is freely granted, provided that this notice
	// is preserved.

	// USE STATEMENTS:
	// na

//	// Return value
//	int UnitNumber; // Result from scanning currently open files
//
//	// Locals
//	// FUNCTION ARGUMENT DEFINITIONS:
//
//	// FUNCTION PARAMETER DEFINITIONS:
//	//  IO Status Values:
//
//	int const END_OF_RECORD( -2 );
//	int const END_OF_FILE( -1 );
//
//	//  Indicate default input and output units:
//
//	int const DEFAULT_INPUT_UNIT( 5 );
//	int const DEFAULT_OUTPUT_UNIT( 6 );
//
//	//  Indicate number and value of preconnected units
//
//	int const NUMBER_OF_PRECONNECTED_UNITS( 2 );
//	static Array1D_int const PRECONNECTED_UNITS( NUMBER_OF_PRECONNECTED_UNITS, { 5, 6 } );
//
//	//  Largest allowed unit number (or a large number, if none)
//	int const MaxUnitNumber( 1000 );
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// FUNCTION LOCAL VARIABLE DECLARATIONS:
//	bool exists; // File exists
//	bool opened; // Unit is open
//	int ios; // return value from Inquire intrinsic
//
//	for ( UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber ) {
//		if ( UnitNumber == DEFAULT_INPUT_UNIT || UnitNumber == DEFAULT_OUTPUT_UNIT ) continue;
//		if ( any_eq( UnitNumber, PRECONNECTED_UNITS ) ) continue;
//		{ IOFlags flags; gio::inquire( UnitNumber, flags ); exists = flags.exists(); opened = flags.open(); ios = flags.ios(); }
//		if ( exists && ! opened && ios == 0 ) return UnitNumber; // result is set in UnitNumber
//	}
//
//	UnitNumber = -1;
//
//	return UnitNumber;

	return gio::get_unit(); //Autodesk:Note ObjexxFCL::gio system provides this (and protects the F90+ preconnected units {100,101,102})
}

int
FindUnitNumber( std::string const & FileName ) // File name to be searched.
{

	// FUNCTION INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997, adapted from reference
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS FUNCTION:
	// Returns a unit number for the file name that is either opened or exists.

	// METHODOLOGY EMPLOYED:
	// Use Inquire function to find out if proposed unit: exists or is opened.
	// If not, can be used for a new unit number.

	// REFERENCES:
	// Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
	// Developed at Unicomp, Inc.
	// Permission to use, copy, modify, and distribute this
	// software is freely granted, provided that this notice
	// is preserved.

	// USE STATEMENTS:
	// na

	// Return value
	int UnitNumber; // Unit number that should be used

	// Locals
	// FUNCTION ARGUMENT DEFINITIONS:

	// FUNCTION PARAMETER DEFINITIONS:
	//  Largest allowed unit number (or a large number, if none)
	int const MaxUnitNumber( 1000 );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// FUNCTION LOCAL VARIABLE DECLARATIONS:
	std::string TestFileName; // File name returned from opened file
	bool exists; // True if file already exists
	bool opened; // True if file is open
	int ios; // Status indicator from INQUIRE intrinsic

	{ IOFlags flags; gio::inquire( FileName, flags ); exists = flags.exists(); opened = flags.open(); ios = flags.ios(); }
	if ( ! opened ) {
		UnitNumber = GetNewUnitNumber();
		{ IOFlags flags; flags.POSITION( "APPEND" ); gio::open( UnitNumber, FileName, flags ); ios = flags.ios(); }
		if ( ios != 0 ) {
			DisplayString( "FindUnitNumber: Could not open file \"" + FileName + "\" for append." );
		}
	} else {
		std::string::size_type const FileNameLength = len( FileName );
		std::string::size_type TestFileLength;
		std::string::size_type Pos; // Position pointer
		for ( UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber ) {
			{ IOFlags flags; gio::inquire( UnitNumber, flags ); TestFileName = flags.name(); opened = flags.open(); }
			//  Powerstation returns just file name
			//  DVF (Digital Fortran) returns whole path
			TestFileLength = len( TestFileName );
			Pos = index( TestFileName, FileName );
			if ( Pos != std::string::npos ) {
				//  Must be the last part of the file
				if ( Pos + FileNameLength == TestFileLength ) break;
			}
		}
	}

	return UnitNumber;

}

void
ConvertCaseToUpper(
	std::string const & InputString, // Input string
	std::string & OutputString // Output string (in UpperCase)
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Convert a string to upper case

	// METHODOLOGY EMPLOYED:
	// This routine is not dependant upon the ASCII
	// code.  It works by storing the upper and lower case alphabet.  It
	// scans the whole input string.  If it finds a character in the lower
	// case alphabet, it makes an appropriate substitution.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	OutputString = InputString;

	for ( std::string::size_type A = 0; A < len( InputString ); ++A ) {
		std::string::size_type const B = index( LowerCase, InputString[ A ] );
		if ( B != std::string::npos ) {
			OutputString[ A ] = UpperCase[ B ];
		}
	}

}

void
ConvertCaseToLower(
	std::string const & InputString, // Input string
	std::string & OutputString // Output string (in LowerCase)
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Convert a string to lower case

	// METHODOLOGY EMPLOYED:
	// This routine is not dependant upon the ASCII
	// code.  It works by storing the upper and lower case alphabet.  It
	// scans the whole input string.  If it finds a character in the lower
	// case alphabet, it makes an appropriate substitution.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	OutputString = InputString;

	for ( std::string::size_type A = 0; A < len( InputString ); ++A ) {
		std::string::size_type const B = index( UpperCase, InputString[ A ] );
		if ( B != std::string::npos ) {
			OutputString[ A ] = LowerCase[ B ];
		}
	}

}

std::string::size_type
FindNonSpace( std::string const & String ) // String to be scanned
{

	// FUNCTION INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS FUNCTION:
	// This function finds the first non-space character in the passed string
	// and returns that position as the result to the calling program.

	// METHODOLOGY EMPLOYED:
	// Scan string for character not equal to blank.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Return value

	// Locals
	// FUNCTION ARGUMENT DEFINITIONS:

	// FUNCTION PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// FUNCTION LOCAL VARIABLE DECLARATIONS:

	return String.find_first_not_of( ' ' );

}

bool
env_var_on( std::string const & env_var_str )
{

	// FUNCTION INFORMATION:
	//       AUTHOR         Stuart G. Mentzer
	//       DATE WRITTEN   April 2014
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS FUNCTION:
	// Test if a boolean environment variable value is "on" (has value starting with Y or T)

	return ( ( ! env_var_str.empty() ) && is_any_of( env_var_str[ 0 ], "YyTt" ) );
}

void
ShowFatalError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       Kyle Benne
	//                      August 2010
	//                      Added sqlite output
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine puts ErrorMessage with a Fatal designation on
	// designated output files.  Then, the program is aborted.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.
	// Calls AbortEnergyPlus

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataErrorTracking;
	using General::RoundSigDigits;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	ShowErrorMessage( " **  Fatal  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	DisplayString( "**FATAL:" + ErrorMessage );

	ShowErrorMessage( " ...Summary of Errors that led to program termination:", OutUnit1, OutUnit2 );
	ShowErrorMessage( " ..... Reference severe error count=" + RoundSigDigits( TotalSevereErrors ), OutUnit1, OutUnit2 );
	ShowErrorMessage( " ..... Last severe error=" + LastSevereError, OutUnit1, OutUnit2 );
	if ( sqlite ) {
		sqlite->createSQLiteErrorRecord( 1, 2, ErrorMessage, 1 );
	}
	throw std::runtime_error( ErrorMessage );

}

void
ShowSevereError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine puts ErrorMessage with a Severe designation on
	// designated output files.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DoingSizing;
	using DataGlobals::KickOffSimulation;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( ErrorMessage, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	++TotalSevereErrors;
	if ( WarmupFlag && ! DoingSizing && ! KickOffSimulation && ! AbortProcessing ) ++TotalSevereErrorsDuringWarmup;
	if ( DoingSizing ) ++TotalSevereErrorsDuringSizing;
	ShowErrorMessage( " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	LastSevereError = ErrorMessage;

	//  Could set a variable here that gets checked at some point?

	if ( sqlite ) {
		sqlite->createSQLiteErrorRecord( 1, 1, ErrorMessage, 1 );
	}

}

void
ShowSevereMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine puts ErrorMessage with a Severe designation on
	// designated output files.
	// But does not bump the error count so can be used in conjunction with recurring
	// error calls.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( ErrorMessage, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	ShowErrorMessage( " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	LastSevereError = ErrorMessage;

	//  Could set a variable here that gets checked at some point?

	if ( sqlite ) {
		sqlite->createSQLiteErrorRecord( 1, 1, ErrorMessage, 0 );
	}

}

void
ShowContinueError(
	std::string const & Message,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   October 2001
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine displays a 'continued error' message on designated output files.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	ShowErrorMessage( " **   ~~~   ** " + Message, OutUnit1, OutUnit2 );
	if ( sqlite ) {
		sqlite->updateSQLiteErrorRecord( Message );
	}

}

void
ShowContinueErrorTimeStamp(
	std::string const & Message,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   February 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine displays a 'continued error' timestamp message on designated output files.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using General::CreateSysTimeIntervalString;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::CurMnDy;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DoingSizing;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string cEnvHeader;

	if ( WarmupFlag ) {
		if ( ! DoingSizing ) {
			cEnvHeader = " During Warmup, Environment=";
		} else {
			cEnvHeader = " During Warmup & Sizing, Environment=";
		}
	} else {
		if ( ! DoingSizing ) {
			cEnvHeader = " Environment=";
		} else {
			cEnvHeader = " During Sizing, Environment=";
		}
	}

	if ( len( Message ) < 50 ) {
		ShowErrorMessage( " **   ~~~   ** " + Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' + CreateSysTimeIntervalString(), OutUnit1, OutUnit2 );
		if ( sqlite ) {
			sqlite->updateSQLiteErrorRecord( Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' + CreateSysTimeIntervalString() );
		}
	} else {
		ShowErrorMessage( " **   ~~~   ** " + Message );
		ShowErrorMessage( " **   ~~~   ** " + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' + CreateSysTimeIntervalString(), OutUnit1, OutUnit2 );
		if ( sqlite ) {
			sqlite->updateSQLiteErrorRecord( Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' + CreateSysTimeIntervalString() );
		}
	}

}

void
ShowMessage(
	std::string const & Message,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine displays a simple message on designated output files.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	if ( Message.empty() ) {
		ShowErrorMessage( " *************", OutUnit1, OutUnit2 );
	} else {
		ShowErrorMessage( " ************* " + Message, OutUnit1, OutUnit2 );
	}

}

void
ShowWarningError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine puts ErrorMessage with a Warning designation on
	// designated output files.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DoingSizing;
	using DataGlobals::KickOffSimulation;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( ErrorMessage, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	++TotalWarningErrors;
	if ( WarmupFlag && ! DoingSizing && ! KickOffSimulation && ! AbortProcessing ) ++TotalWarningErrorsDuringWarmup;
	if ( DoingSizing ) ++TotalWarningErrorsDuringSizing;
	ShowErrorMessage( " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2 );

	if ( sqlite ) {
		sqlite->createSQLiteErrorRecord( 1, 0, ErrorMessage, 1 );
	}

}

void
ShowWarningMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine puts ErrorMessage with a Warning designation on
	// designated output files.
	// But does not bump the error count so can be used in conjunction with recurring
	// error calls.

	// METHODOLOGY EMPLOYED:
	// Calls ShowErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( ErrorMessage, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	ShowErrorMessage( " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2 );
	if ( sqlite ) {
		sqlite->createSQLiteErrorRecord( 1, 0, ErrorMessage, 0 );
	}

}

void
ShowRecurringSevereErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits, // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits, // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits // optional char string (<=15 length) of units for sum value
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Michael J. Witte
	//       DATE WRITTEN   August 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine stores a recurring ErrorMessage with a Severe designation
	// for output at the end of the simulation with automatic tracking of number
	// of occurences and optional tracking of associated min, max, and sum values

	// METHODOLOGY EMPLOYED:
	// Calls StoreRecurringErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	//  Use for recurring "warning" error messages shown once at end of simulation
	//  with count of occurences and optional max, min, sum

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( Message, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	++TotalSevereErrors;
	StoreRecurringErrorMessage( " ** Severe  ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
ShowRecurringWarningErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits, // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits, // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits // optional char string (<=15 length) of units for sum value
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Michael J. Witte
	//       DATE WRITTEN   August 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine stores a recurring ErrorMessage with a Warning designation
	// for output at the end of the simulation with automatic tracking of number
	// of occurences and optional tracking of associated min, max, and sum values

	// METHODOLOGY EMPLOYED:
	// Calls StoreRecurringErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	//  Use for recurring "warning" error messages shown once at end of simulation
	//  with count of occurences and optional max, min, sum

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( Message, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	++TotalWarningErrors;
	StoreRecurringErrorMessage( " ** Warning ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
ShowRecurringContinueErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits, // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits, // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits // optional char string (<=15 length) of units for sum value
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Michael J. Witte
	//       DATE WRITTEN   August 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine stores a recurring ErrorMessage with a continue designation
	// for output at the end of the simulation with automatic tracking of number
	// of occurences and optional tracking of associated min, max, and sum values

	// METHODOLOGY EMPLOYED:
	// Calls StoreRecurringErrorMessage utility routine.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	//  Use for recurring "warning" error messages shown once at end of simulation
	//  with count of occurences and optional max, min, sum

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;

	for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
		if ( has( Message, MessageSearch( Loop ) ) ) ++MatchCounts( Loop );
	}

	StoreRecurringErrorMessage( " **   ~~~   ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
StoreRecurringErrorMessage(
	std::string const & ErrorMessage, // Message automatically written to "error file" at end of simulation
	int & ErrorMsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ErrorReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ErrorReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ErrorReportSumOf, // Track and report the sum of the values passed to this argument
	std::string const & ErrorReportMaxUnits, // Units for "max" reporting
	std::string const & ErrorReportMinUnits, // Units for "min" reporting
	std::string const & ErrorReportSumUnits // Units for "sum" reporting
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Michael J. Witte
	//       DATE WRITTEN   August 2004
	//       MODIFIED       September 2005;LKL;Added Units
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine stores a recurring ErrorMessage with
	// for output at the end of the simulation with automatic tracking of number
	// of occurences and optional tracking of associated min, max, and sum values

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using namespace DataErrorTracking;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DoingSizing;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// If Index is zero, then assign next available index and reallocate array
	if ( ErrorMsgIndex == 0 ) {
		RecurringErrors.redimension( ++NumRecurringErrors );
		ErrorMsgIndex = NumRecurringErrors;
		// The message string only needs to be stored once when a new recurring message is created
		RecurringErrors( ErrorMsgIndex ).Message = ErrorMessage;
		RecurringErrors( ErrorMsgIndex ).Count = 1;
		if ( WarmupFlag ) RecurringErrors( ErrorMsgIndex ).WarmupCount = 1;
		if ( DoingSizing ) RecurringErrors( ErrorMsgIndex ).SizingCount = 1;

		// For max, min, and sum values, store the current value when a new recurring message is created
		if ( present( ErrorReportMaxOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MaxValue = ErrorReportMaxOf;
			RecurringErrors( ErrorMsgIndex ).ReportMax = true;
			if ( ! ErrorReportMaxUnits.empty() ) {
				RecurringErrors( ErrorMsgIndex ).MaxUnits = ErrorReportMaxUnits;
			}
		}
		if ( present( ErrorReportMinOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MinValue = ErrorReportMinOf;
			RecurringErrors( ErrorMsgIndex ).ReportMin = true;
			if ( ! ErrorReportMinUnits.empty() ) {
				RecurringErrors( ErrorMsgIndex ).MinUnits = ErrorReportMinUnits;
			}
		}
		if ( present( ErrorReportSumOf ) ) {
			RecurringErrors( ErrorMsgIndex ).SumValue = ErrorReportSumOf;
			RecurringErrors( ErrorMsgIndex ).ReportSum = true;
			if ( ! ErrorReportSumUnits.empty() ) {
				RecurringErrors( ErrorMsgIndex ).SumUnits = ErrorReportSumUnits;
			}
		}

	} else if ( ErrorMsgIndex > 0 ) {
		// Do stats and store
		++RecurringErrors( ErrorMsgIndex ).Count;
		if ( WarmupFlag ) ++RecurringErrors( ErrorMsgIndex ).WarmupCount;
		if ( DoingSizing ) ++RecurringErrors( ErrorMsgIndex ).SizingCount;

		if ( present( ErrorReportMaxOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MaxValue = max( ErrorReportMaxOf, RecurringErrors( ErrorMsgIndex ).MaxValue );
			RecurringErrors( ErrorMsgIndex ).ReportMax = true;
		}
		if ( present( ErrorReportMinOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MinValue = min( ErrorReportMinOf, RecurringErrors( ErrorMsgIndex ).MinValue );
			RecurringErrors( ErrorMsgIndex ).ReportMin = true;
		}
		if ( present( ErrorReportSumOf ) ) {
			RecurringErrors( ErrorMsgIndex ).SumValue += ErrorReportSumOf;
			RecurringErrors( ErrorMsgIndex ).ReportSum = true;
		}
	} else {
		// If ErrorMsgIndex < 0, then do nothing
	}

}

void
ShowErrorMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1,
	Optional_int OutUnit2
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   December 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine displays the error messages on the indicated
	// file unit numbers, in addition to the "standard error output"
	// unit.

	// METHODOLOGY EMPLOYED:
	// If arguments OutUnit1 and/or OutUnit2 are present the
	// error message is written to these as well and the standard one.

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataStringGlobals::VerString;
	using DataStringGlobals::IDDVerString;
	using DataGlobals::DoingInputProcessing;
	using DataGlobals::CacheIPErrorFile;
	using DataGlobals::err_stream;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static gio::Fmt ErrorFormat( "(2X,A)" );
	static gio::Fmt fmtA( "(A)" );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	if ( UtilityRoutines::outputErrorHeader && err_stream ) {
		*err_stream << "Program Version," + VerString + ',' + IDDVerString + DataStringGlobals::NL;
		UtilityRoutines::outputErrorHeader = false;
	}

	if ( ! DoingInputProcessing ) {
		if ( err_stream ) *err_stream << "  " << ErrorMessage << DataStringGlobals::NL;
	} else {
		gio::write( CacheIPErrorFile, fmtA ) << ErrorMessage;
	}
	if ( present( OutUnit1 ) ) {
		gio::write( OutUnit1, ErrorFormat ) << ErrorMessage;
	}
	if ( present( OutUnit2 ) ) {
		gio::write( OutUnit2, ErrorFormat ) << ErrorMessage;
	}

}

void
SummarizeErrors()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   March 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a summary of certain errors that might
	// otherwise get lost in the shuffle of many similar messages.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataErrorTracking;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string::size_type StartC;
	std::string::size_type EndC;

	if ( any_gt( MatchCounts, 0 ) ) {
		ShowMessage( "" );
		ShowMessage( "===== Final Error Summary =====" );
		ShowMessage( "The following error categories occurred.  Consider correcting or noting." );
		for ( int Loop = 1; Loop <= SearchCounts; ++Loop ) {
			if ( MatchCounts( Loop ) > 0 ) {
				ShowMessage( Summaries( Loop ) );
				if ( MoreDetails( Loop ) != "" ) {
					StartC = 0;
					EndC = len( MoreDetails( Loop ) ) - 1;
					while ( EndC != std::string::npos ) {
						EndC = index( MoreDetails( Loop ).substr( StartC ), "<CR" );
						ShowMessage( ".." + MoreDetails( Loop ).substr( StartC, EndC ) );
						if ( MoreDetails( Loop ).substr( StartC + EndC, 5 ) == "<CRE>" ) break;
						StartC += EndC + 4;
						EndC = len( MoreDetails( Loop ).substr( StartC ) ) - 1;
					}
				}
			}
		}
		ShowMessage( "" );
	}

}

void
ShowRecurringErrors()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   March 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a summary of certain errors that might
	// otherwise get lost in the shuffle of many similar messages.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataErrorTracking;
	using General::RoundSigDigits;
	using General::strip_trailing_zeros;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	static std::string const StatMessageStart( " **   ~~~   ** " );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;
	std::string StatMessage;
	std::string MaxOut;
	std::string MinOut;
	std::string SumOut;

	if ( NumRecurringErrors > 0 ) {
		ShowMessage( "" );
		ShowMessage( "===== Recurring Error Summary =====" );
		ShowMessage( "The following recurring error messages occurred." );
		for ( Loop = 1; Loop <= NumRecurringErrors; ++Loop ) {
			auto const & error( RecurringErrors( Loop ) );
			// Suppress reporting the count if it is a continue error
			if ( has_prefix( error.Message, " **   ~~~   ** " ) ) {
				ShowMessage( error.Message );
				if ( sqlite ) {
					sqlite->updateSQLiteErrorRecord( error.Message );
				}
			} else {
				ShowMessage( "" );
				ShowMessage( error.Message );
				ShowMessage( StatMessageStart + "  This error occurred " + RoundSigDigits( error.Count ) + " total times;" );
				ShowMessage( StatMessageStart + "  during Warmup " + RoundSigDigits( error.WarmupCount ) + " times;" );
				ShowMessage( StatMessageStart + "  during Sizing " + RoundSigDigits( error.SizingCount ) + " times." );
				if ( sqlite ) {
					if ( has_prefix( error.Message, " ** Warning ** " ) ) {
						sqlite->createSQLiteErrorRecord( 1, 0, error.Message.substr( 15 ), error.Count );
					} else if ( has_prefix( error.Message, " ** Severe  ** " ) ) {
						sqlite->createSQLiteErrorRecord( 1, 1, error.Message.substr( 15 ), error.Count );
					}
				}
			}
			StatMessage = "";
			if ( error.ReportMax ) {
				MaxOut = RoundSigDigits( error.MaxValue, 6 );
				strip_trailing_zeros( MaxOut );
				StatMessage += "  Max=" + MaxOut;
				if ( ! error.MaxUnits.empty() ) StatMessage += ' ' + error.MaxUnits;
			}
			if ( error.ReportMin ) {
				MinOut = RoundSigDigits( error.MinValue, 6 );
				strip_trailing_zeros( MinOut );
				StatMessage += "  Min=" + MinOut;
				if ( ! error.MinUnits.empty() ) StatMessage += ' ' + error.MinUnits;
			}
			if ( error.ReportSum ) {
				SumOut = RoundSigDigits( error.SumValue, 6 );
				strip_trailing_zeros( SumOut );
				StatMessage += "  Sum=" + SumOut;
				if ( ! error.SumUnits.empty() ) StatMessage += ' ' + error.SumUnits;
			}
			if ( error.ReportMax || error.ReportMin || error.ReportSum ) {
				ShowMessage( StatMessageStart + StatMessage );
			}
		}
		ShowMessage( "" );
	}

}

} // EnergyPlus
