// C++ Headers
#include <cstdlib>
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <UtilityRoutines.hh>
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
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

void
AbortEnergyPlus(
	bool const NoIdf, // Set to true when "noidf" was found
	bool const NoIDD // Set to true when "noidd" was found
)
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
	using SQLiteProcedures::UpdateSQLiteSimulationRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Fstring const OutFmt( "('Press ENTER to continue after reading above message>')" );
	static Fstring const ETimeFmt( "(I2.2,'hr ',I2.2,'min ',F5.2,'sec')" );

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int tempfl;
	Fstring NumWarnings( 32 );
	Fstring NumSevere( 32 );
	Fstring NumWarningsDuringWarmup( 32 );
	Fstring NumSevereDuringWarmup( 32 );
	Fstring NumWarningsDuringSizing( 32 );
	Fstring NumSevereDuringSizing( 32 );
	Fstring Elapsed( 32 );
	int Hours; // Elapsed Time Hour Reporting
	int Minutes; // Elapsed Time Minute Reporting
	Real64 Seconds; // Elapsed Time Second Reporting
	bool ErrFound;
	bool TerminalError;
	int write_stat;

	if ( WriteOutputToSQLite ) {
		UpdateSQLiteSimulationRecord( true, false );
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
	NumWarnings = adjustl( NumWarnings );
	NumSevere = RoundSigDigits( TotalSevereErrors );
	NumSevere = adjustl( NumSevere );
	NumWarningsDuringWarmup = RoundSigDigits( TotalWarningErrorsDuringWarmup );
	NumWarningsDuringWarmup = adjustl( NumWarningsDuringWarmup );
	NumSevereDuringWarmup = RoundSigDigits( TotalSevereErrorsDuringWarmup );
	NumSevereDuringWarmup = adjustl( NumSevereDuringWarmup );
	NumWarningsDuringSizing = RoundSigDigits( TotalWarningErrorsDuringSizing );
	NumWarningsDuringSizing = adjustl( NumWarningsDuringSizing );
	NumSevereDuringSizing = RoundSigDigits( TotalSevereErrorsDuringSizing );
	NumSevereDuringSizing = adjustl( NumSevereDuringSizing );

	if ( NoIDD ) {
		DisplayString( "No EnergyPlus Data Dictionary (Energy+.idd) was found.  It is possible " );
		DisplayString( "you \"double-clicked\"EnergyPlus.exe rather than using one of the methods" );
		DisplayString( "to run Energyplus as found in the GettingStarted document in the" );
		DisplayString( "documentation folder.  Using EP-Launch may be best -- " );
		DisplayString( "it provides extra help for new users." );
		ShowMessage( "No EnergyPlus Data Dictionary (Energy+.idd) was found. It is possible you \"double-clicked\" EnergyPlus.exe " );
		ShowMessage( "rather than using one of the methods to run Energyplus as found in the GettingStarted document" );
		ShowMessage( "in the documentation folder.  Using EP-Launch may be best -- it provides extra help for new users." );
		{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutFmt, flags ); }
		gio::read( "*" );
	}

	if ( NoIdf ) {
		DisplayString( "No input file (in.idf) was found.  It is possible you \"double-clicked\"" );
		DisplayString( "EnergyPlus.exe rather than using one of the methods to run Energyplus" );
		DisplayString( "as found in the GettingStarted document in the documentation folder." );
		DisplayString( "Using EP-Launch may be best -- it provides extra help for new users." );
		ShowMessage( "No input file (in.idf) was found.  It is possible you \"double-clicked\" EnergyPlus.exe rather than" );
		ShowMessage( "using one of the methods to run Energyplus as found in the GettingStarted document in the documentation" );
		ShowMessage( "folder.  Using EP-Launch may be best -- it provides extra help for new users." );
		{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutFmt, flags ); }
		gio::read( "*" );
	}

	// catch up with timings if in middle
	Time_Finish = epElapsedTime();
	if ( Time_Finish < Time_Start ) Time_Finish += 24.0 * 3600.0;
	Elapsed_Time = Time_Finish - Time_Start;
#ifdef EP_Detailed_Timings
	epStopTime( "EntireRun=" );
#endif
	if ( Elapsed_Time < 0.0 ) Elapsed_Time = 0.0;
	Hours = Elapsed_Time / 3600.;
	Elapsed_Time -= Hours * 3600.0;
	Minutes = Elapsed_Time / 60.0;
	Elapsed_Time -= Minutes * 60.0;
	Seconds = Elapsed_Time;
	if ( Seconds < 0.0 ) Seconds = 0.0;
	gio::write( Elapsed, ETimeFmt ) << Hours << Minutes << Seconds;

	ShowMessage( "EnergyPlus Warmup Error Summary. During Warmup: " + trim( NumWarningsDuringWarmup ) + " Warning; " + trim( NumSevereDuringWarmup ) + " Severe Errors." );
	ShowMessage( "EnergyPlus Sizing Error Summary. During Sizing: " + trim( NumWarningsDuringSizing ) + " Warning; " + trim( NumSevereDuringSizing ) + " Severe Errors." );
	ShowMessage( "EnergyPlus Terminated--Fatal Error Detected. " + trim( NumWarnings ) + " Warning; " + trim( NumSevere ) + " Severe Errors;" " Elapsed Time=" + trim( Elapsed ) );
	DisplayString( "EnergyPlus Run Time=" + trim( Elapsed ) );
	tempfl = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( tempfl, "eplusout.end", flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "AbortEnergyPlus: Could not open file \"eplusout.end\" for output (write)." );
	}
	gio::write( tempfl, "*" ) << "EnergyPlus Terminated--Fatal Error Detected. " + trim( NumWarnings ) + " Warning; " + trim( NumSevere ) + " Severe Errors;" " Elapsed Time=" + trim( Elapsed );

	gio::close( tempfl );
#ifdef EP_Detailed_Timings
	epSummaryTimes( Time_Finish - Time_Start );
#endif
	CloseOutOpenFiles();
	// Close the socket used by ExternalInterface. This call also sends the flag "-1" to the ExternalInterface,
	// indicating that E+ terminated with an error.
	if ( NumExternalInterfaces > 0 ) CloseSocket( -1 );
	std::cerr << "Program terminated: " << "EnergyPlus Terminated--Error(s) Detected." << std::endl; std::exit( EXIT_FAILURE );

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
	int const MaxUnitNumber( 1000 );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Fstring DebugPosition( 20 );

	//      LOGICAL :: exists, opened
	//      INTEGER :: UnitNumber
	//      INTEGER :: ios

	CloseReportIllumMaps();
	CloseDFSFile();

	//  In case some debug output was produced, it appears that the
	//  position on the INQUIRE will not be 'ASIS' (3 compilers tested)
	//  So, will want to keep....

	{ IOFlags flags; gio::inquire( OutputFileDebug, flags ); DebugPosition = flags.POSITION(); }
	if ( trim( DebugPosition ) != "ASIS" ) {
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
	using SQLiteProcedures::UpdateSQLiteSimulationRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Fstring const ETimeFmt( "(I2.2,'hr ',I2.2,'min ',F5.2,'sec')" );

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int tempfl;
	Fstring NumWarnings( 32 );
	Fstring NumSevere( 32 );
	Fstring NumWarningsDuringWarmup( 32 );
	Fstring NumSevereDuringWarmup( 32 );
	Fstring NumWarningsDuringSizing( 32 );
	Fstring NumSevereDuringSizing( 32 );
	Fstring Elapsed( 32 );
	int Hours; // Elapsed Time Hour Reporting
	int Minutes; // Elapsed Time Minute Reporting
	Real64 Seconds; // Elapsed Time Second Reporting
	int write_stat;

	if ( WriteOutputToSQLite ) {
		UpdateSQLiteSimulationRecord( true, true );
	}

	ReportSurfaceErrors();
	ShowRecurringErrors();
	SummarizeErrors();
	CloseMiscOpenFiles();
	NumWarnings = RoundSigDigits( TotalWarningErrors );
	NumWarnings = adjustl( NumWarnings );
	NumSevere = RoundSigDigits( TotalSevereErrors );
	NumSevere = adjustl( NumSevere );
	NumWarningsDuringWarmup = RoundSigDigits( TotalWarningErrorsDuringWarmup );
	NumWarningsDuringWarmup = adjustl( NumWarningsDuringWarmup );
	NumSevereDuringWarmup = RoundSigDigits( TotalSevereErrorsDuringWarmup );
	NumSevereDuringWarmup = adjustl( NumSevereDuringWarmup );
	NumWarningsDuringSizing = RoundSigDigits( TotalWarningErrorsDuringSizing );
	NumWarningsDuringSizing = adjustl( NumWarningsDuringSizing );
	NumSevereDuringSizing = RoundSigDigits( TotalSevereErrorsDuringSizing );
	NumSevereDuringSizing = adjustl( NumSevereDuringSizing );

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

	ShowMessage( "EnergyPlus Warmup Error Summary. During Warmup: " + trim( NumWarningsDuringWarmup ) + " Warning; " + trim( NumSevereDuringWarmup ) + " Severe Errors." );
	ShowMessage( "EnergyPlus Sizing Error Summary. During Sizing: " + trim( NumWarningsDuringSizing ) + " Warning; " + trim( NumSevereDuringSizing ) + " Severe Errors." );
	ShowMessage( "EnergyPlus Completed Successfully-- " + trim( NumWarnings ) + " Warning; " + trim( NumSevere ) + " Severe Errors;" " Elapsed Time=" + trim( Elapsed ) );
	DisplayString( "EnergyPlus Run Time=" + trim( Elapsed ) );
	tempfl = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( tempfl, "eplusout.end", flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "EndEnergyPlus: Could not open file \"eplusout.end\" for output (write)." );
	}
	gio::write( tempfl, "(A)" ) << "EnergyPlus Completed Successfully-- " + trim( NumWarnings ) + " Warning; " + trim( NumSevere ) + " Severe Errors;" " Elapsed Time=" + trim( Elapsed );
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
//	static FArray1D_int const PRECONNECTED_UNITS( NUMBER_OF_PRECONNECTED_UNITS, { 5, 6 } );
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
FindUnitNumber( Fstring const & FileName ) // File name to be searched.
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
	Fstring TestFileName( 255 ); // File name returned from opened file
	int TestFileLength; // Length from INQUIRE intrinsic
	bool exists; // True if file already exists
	bool opened; // True if file is open
	int Pos; // Position pointer
	int FileNameLength; // Length of requested file
	int ios; // Status indicator from INQUIRE intrinsic

	{ IOFlags flags; gio::inquire( FileName, flags ); exists = flags.exists(); opened = flags.open(); ios = flags.ios(); }
	if ( ! opened ) {
		UnitNumber = GetNewUnitNumber();
		{ IOFlags flags; flags.POSITION( "APPEND" ); gio::open( UnitNumber, FileName, flags ); ios = flags.ios(); }
		if ( ios != 0 ) {
			DisplayString( "FindUnitNumber: Could not open file \"" + trim( FileName ) + "\" for append." );
		}
	} else {
		FileNameLength = len_trim( FileName );
		for ( UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber ) {
			{ IOFlags flags; gio::inquire( UnitNumber, flags ); TestFileName = flags.name(); opened = flags.open(); }
			//  Powerstation returns just file name
			//  DVF (Digital Fortran) returns whole path
			TestFileLength = len_trim( TestFileName );
			Pos = index( TestFileName, FileName );
			if ( Pos != 0 ) {
				//  Must be the last part of the file
				if ( Pos + FileNameLength - 1 == TestFileLength ) break;
			}
		}
	}

	return UnitNumber;

}

void
ConvertCaseToUpper(
	Fstring const & InputString, // Input string
	Fstring & OutputString // Output string (in UpperCase)
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
	int A;
	int B;

	OutputString = " ";

	for ( A = 1; A <= len_trim( InputString ); ++A ) {
		B = index( LowerCase, InputString( A, A ) );
		if ( B != 0 ) {
			OutputString( A, A ) = UpperCase( {B,B} );
		} else {
			OutputString( A, A ) = InputString( A, A );
		}
	}

}

void
ConvertCaseToLower(
	Fstring const & InputString, // Input string
	Fstring & OutputString // Output string (in LowerCase)
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
	int A;
	int B;

	OutputString = " ";

	for ( A = 1; A <= len_trim( InputString ); ++A ) {
		B = index( UpperCase, InputString( A, A ) );
		if ( B != 0 ) {
			OutputString( A, A ) = LowerCase( {B,B} );
		} else {
			OutputString( A, A ) = InputString( A, A );
		}
	}

}

int
FindNonSpace( Fstring const & String ) // String to be scanned
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
	int FindNonSpace;

	// Locals
	// FUNCTION ARGUMENT DEFINITIONS:

	// FUNCTION PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// FUNCTION LOCAL VARIABLE DECLARATIONS:
	int I;
	int ILEN;

	FindNonSpace = 0;
	ILEN = len_trim( String );
	for ( I = 1; I <= ILEN; ++I ) {
		if ( String( I, I ) != " " ) {
			FindNonSpace = I;
			break;
		}
	}

	return FindNonSpace;

}

void
ShowFatalError(
	Fstring const & ErrorMessage,
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
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static bool NoIdf( false );
	static bool NoIDD( false );

	ShowErrorMessage( " **  Fatal  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	DisplayString( "**FATAL:" + trim( ErrorMessage ) );
	if ( index( ErrorMessage, "in.idf missing" ) > 0 ) NoIdf = true;
	if ( index( ErrorMessage, "Energy+.idd missing" ) > 0 ) NoIDD = true;
	ShowErrorMessage( " ...Summary of Errors that led to program termination:", OutUnit1, OutUnit2 );
	ShowErrorMessage( " ..... Reference severe error count=" + trim( RoundSigDigits( TotalSevereErrors ) ), OutUnit1, OutUnit2 );
	ShowErrorMessage( " ..... Last severe error=" + trim( LastSevereError ), OutUnit1, OutUnit2 );
	if ( WriteOutputToSQLite ) {
		CreateSQLiteErrorRecord( 1, 2, ErrorMessage, 1 );
	}
	AbortEnergyPlus( NoIdf, NoIDD );

}

void
ShowSevereError(
	Fstring const & ErrorMessage,
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
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

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
		if ( index( ErrorMessage, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	++TotalSevereErrors;
	if ( WarmupFlag && ! DoingSizing && ! KickOffSimulation && ! AbortProcessing ) ++TotalSevereErrorsDuringWarmup;
	if ( DoingSizing ) ++TotalSevereErrorsDuringSizing;
	ShowErrorMessage( " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	LastSevereError = ErrorMessage;

	//  Could set a variable here that gets checked at some point?

	if ( WriteOutputToSQLite ) {
		CreateSQLiteErrorRecord( 1, 1, ErrorMessage, 1 );
	}

}

void
ShowSevereMessage(
	Fstring const & ErrorMessage,
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
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

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
		if ( index( ErrorMessage, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	ShowErrorMessage( " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2 );
	LastSevereError = ErrorMessage;

	//  Could set a variable here that gets checked at some point?

	if ( WriteOutputToSQLite ) {
		CreateSQLiteErrorRecord( 1, 1, ErrorMessage, 0 );
	}

}

void
ShowContinueError(
	Fstring const & Message,
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
	using SQLiteProcedures::UpdateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

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
	if ( WriteOutputToSQLite ) {
		UpdateSQLiteErrorRecord( Message );
	}

}

void
ShowContinueErrorTimeStamp(
	Fstring const & Message,
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
	using SQLiteProcedures::UpdateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Fstring cEnvHeader( 100 );

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

	if ( len_trim( Message ) < 50 ) {
		ShowErrorMessage( " **   ~~~   ** " + trim( Message ) + trim( cEnvHeader ) + trim( EnvironmentName ) + ", at Simulation time=" + trim( CurMnDy ) + " " + trim( CreateSysTimeIntervalString() ), OutUnit1, OutUnit2 );
		if ( WriteOutputToSQLite ) {
			UpdateSQLiteErrorRecord( trim( Message ) + trim( cEnvHeader ) + trim( EnvironmentName ) + ", at Simulation time=" + trim( CurMnDy ) + " " + trim( CreateSysTimeIntervalString() ) );
		}

	} else {
		ShowErrorMessage( " **   ~~~   ** " + trim( Message ) );
		ShowErrorMessage( " **   ~~~   ** " + trim( cEnvHeader ) + trim( EnvironmentName ) + ", at Simulation time=" + trim( CurMnDy ) + " " + trim( CreateSysTimeIntervalString() ), OutUnit1, OutUnit2 );
		if ( WriteOutputToSQLite ) {
			UpdateSQLiteErrorRecord( trim( Message ) + trim( cEnvHeader ) + trim( EnvironmentName ) + ", at Simulation time=" + trim( CurMnDy ) + " " + trim( CreateSysTimeIntervalString() ) );
		}
	}

}

void
ShowMessage(
	Fstring const & Message,
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

	ShowErrorMessage( " ************* " + Message, OutUnit1, OutUnit2 );

}

void
ShowWarningError(
	Fstring const & ErrorMessage,
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
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

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
		if ( index( ErrorMessage, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	++TotalWarningErrors;
	if ( WarmupFlag && ! DoingSizing && ! KickOffSimulation && ! AbortProcessing ) ++TotalWarningErrorsDuringWarmup;
	if ( DoingSizing ) ++TotalWarningErrorsDuringSizing;
	ShowErrorMessage( " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2 );

	if ( WriteOutputToSQLite ) {
		CreateSQLiteErrorRecord( 1, 0, ErrorMessage, 1 );
	}

}

void
ShowWarningMessage(
	Fstring const & ErrorMessage,
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
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

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
		if ( index( ErrorMessage, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	ShowErrorMessage( " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2 );
	if ( WriteOutputToSQLite ) {
		CreateSQLiteErrorRecord( 1, 0, ErrorMessage, 0 );
	}

}

void
ShowRecurringSevereErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits // optional char string (<=15 length) of units for sum value
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
		if ( index( Message, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	++TotalSevereErrors;
	StoreRecurringErrorMessage( " ** Severe  ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
ShowRecurringWarningErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits // optional char string (<=15 length) of units for sum value
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
		if ( index( Message, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	++TotalWarningErrors;
	StoreRecurringErrorMessage( " ** Warning ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
ShowRecurringContinueErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits // optional char string (<=15 length) of units for sum value
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
		if ( index( Message, trim( MessageSearch( Loop ) ) ) > 0 ) ++MatchCounts( Loop );
	}

	StoreRecurringErrorMessage( " **   ~~~   ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits );

}

void
StoreRecurringErrorMessage(
	Fstring const & ErrorMessage, // Message automatically written to "error file" at end of simulation
	int & ErrorMsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ErrorReportMaxOf, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ErrorReportMinOf, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ErrorReportSumOf, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ErrorReportMaxUnits, // Units for "max" reporting
	Optional_Fstring_const ErrorReportMinUnits, // Units for "min" reporting
	Optional_Fstring_const ErrorReportSumUnits // Units for "sum" reporting
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

	// Object Data
	FArray1D< RecurringErrorData > TempRecurringErrors;

	// If Index is zero, then assign next available index and reallocate array
	if ( ErrorMsgIndex == 0 ) {
		++NumRecurringErrors;
		ErrorMsgIndex = NumRecurringErrors;
		if ( NumRecurringErrors == 1 ) {
			RecurringErrors.allocate( NumRecurringErrors );
		} else if ( NumRecurringErrors > 1 ) {
			TempRecurringErrors.allocate( NumRecurringErrors );
			TempRecurringErrors( {1,NumRecurringErrors - 1} ) = RecurringErrors( {1,NumRecurringErrors - 1} );
			RecurringErrors.deallocate();
			RecurringErrors.allocate( NumRecurringErrors );
			RecurringErrors = TempRecurringErrors;
			TempRecurringErrors.deallocate();
		}
		// The message string only needs to be stored once when a new recurring message is created
		RecurringErrors( ErrorMsgIndex ).Message = trim( ErrorMessage );
		RecurringErrors( ErrorMsgIndex ).Count = 1;
		if ( WarmupFlag ) RecurringErrors( ErrorMsgIndex ).WarmupCount = 1;
		if ( DoingSizing ) RecurringErrors( ErrorMsgIndex ).SizingCount = 1;

		// For max, min, and sum values, store the current value when a new recurring message is created
		if ( present( ErrorReportMaxOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MaxValue = ErrorReportMaxOf;
			RecurringErrors( ErrorMsgIndex ).ReportMax = true;
			if ( present( ErrorReportMaxUnits ) ) {
				RecurringErrors( ErrorMsgIndex ).MaxUnits = ErrorReportMaxUnits;
			}
		}
		if ( present( ErrorReportMinOf ) ) {
			RecurringErrors( ErrorMsgIndex ).MinValue = ErrorReportMinOf;
			RecurringErrors( ErrorMsgIndex ).ReportMin = true;
			if ( present( ErrorReportMinUnits ) ) {
				RecurringErrors( ErrorMsgIndex ).MinUnits = ErrorReportMinUnits;
			}
		}
		if ( present( ErrorReportSumOf ) ) {
			RecurringErrors( ErrorMsgIndex ).SumValue = ErrorReportSumOf;
			RecurringErrors( ErrorMsgIndex ).ReportSum = true;
			if ( present( ErrorReportSumUnits ) ) {
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
	Fstring const & ErrorMessage,
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

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Fstring const ErrorFormat( "(2X,A)" );
	static Fstring const fmtA( "(A)" );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static int TotalErrors( 0 ); // used to determine when to open standard error output file.
	static int StandardErrorOutput;
	int write_stat;
	static bool ErrFileOpened( false );

	if ( TotalErrors == 0 && ! ErrFileOpened ) {
		StandardErrorOutput = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); gio::open( StandardErrorOutput, "eplusout.err", flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			DisplayString( "Trying to display error: \"" + trim( ErrorMessage ) + "\"" );
			ShowFatalError( "ShowErrorMessage: Could not open file \"eplusout.err\" for output (write)." );
		}
		gio::write( StandardErrorOutput, "(A)" ) << "Program Version," + trim( VerString ) + "," + trim( IDDVerString );
		ErrFileOpened = true;
	}

	if ( ! DoingInputProcessing ) {
		++TotalErrors;
		gio::write( StandardErrorOutput, ErrorFormat ) << trim( ErrorMessage );
	} else {
		gio::write( CacheIPErrorFile, fmtA ) << trim( ErrorMessage );
	}
	if ( present( OutUnit1 ) ) {
		gio::write( OutUnit1, ErrorFormat ) << trim( ErrorMessage );
	}
	if ( present( OutUnit2 ) ) {
		gio::write( OutUnit2, ErrorFormat ) << trim( ErrorMessage );
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
	int Loop;
	int StartC;
	int EndC;

	if ( any_gt( MatchCounts, 0 ) ) {
		ShowMessage( " " );
		ShowMessage( "===== Final Error Summary =====" );
		ShowMessage( "The following error categories occurred.  Consider correcting or noting." );
		for ( Loop = 1; Loop <= SearchCounts; ++Loop ) {
			if ( MatchCounts( Loop ) > 0 ) {
				ShowMessage( trim( Summaries( Loop ) ) );
				if ( MoreDetails( Loop ) != " " ) {
					StartC = 1;
					EndC = len_trim( MoreDetails( Loop ) );
					while ( EndC > 0 ) {
						EndC = index( MoreDetails( Loop )( {StartC,_} ), "<CR" );
						ShowMessage( ".." + MoreDetails( Loop )( {StartC,StartC + EndC - 2} ) );
						if ( MoreDetails( Loop )( {StartC + EndC - 1,StartC + EndC + 3} ) == "<CRE>" ) break;
						StartC += EndC + 3;
						EndC = len_trim( MoreDetails( Loop )( {StartC,_} ) );
					}
				}
			}
		}
		ShowMessage( " " );
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
	using General::RemoveTrailingZeros;
	using SQLiteProcedures::UpdateSQLiteErrorRecord;
	using SQLiteProcedures::CreateSQLiteErrorRecord;
	using SQLiteProcedures::WriteOutputToSQLite;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Fstring const StatMessageStart( " **   ~~~   ** " );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;
	Fstring StatMessage( MaxRecurringErrorMsgLength );
	Fstring MaxOut( MaxRecurringErrorMsgLength );
	Fstring MinOut( MaxRecurringErrorMsgLength );
	Fstring SumOut( MaxRecurringErrorMsgLength );

	if ( NumRecurringErrors > 0 ) {
		ShowMessage( " " );
		ShowMessage( "===== Recurring Error Summary =====" );
		ShowMessage( "The following recurring error messages occurred." );
		for ( Loop = 1; Loop <= NumRecurringErrors; ++Loop ) {
			// Suppress reporting the count if it is a continue error
			if ( RecurringErrors( Loop ).Message( 1, 15 ) == " **   ~~~   ** " ) {
				ShowMessage( trim( RecurringErrors( Loop ).Message ) );
				if ( WriteOutputToSQLite ) {
					UpdateSQLiteErrorRecord( trim( RecurringErrors( Loop ).Message ) );
				}
			} else {
				ShowMessage( " " );
				ShowMessage( trim( RecurringErrors( Loop ).Message ) );
				ShowMessage( StatMessageStart + "  This error occurred " + trim( RoundSigDigits( RecurringErrors( Loop ).Count ) ) + " total times;" );
				ShowMessage( StatMessageStart + "  during Warmup " + trim( RoundSigDigits( RecurringErrors( Loop ).WarmupCount ) ) + " times;" );
				ShowMessage( StatMessageStart + "  during Sizing " + trim( RoundSigDigits( RecurringErrors( Loop ).SizingCount ) ) + " times." );
				if ( WriteOutputToSQLite ) {
					if ( RecurringErrors( Loop ).Message( {1,15} ) == " ** Warning ** " ) {
						CreateSQLiteErrorRecord( 1, 0, trim( RecurringErrors( Loop ).Message( 16 ) ), RecurringErrors( Loop ).Count );
					} else if ( RecurringErrors( Loop ).Message( {1,15} ) == " ** Severe  ** " ) {
						CreateSQLiteErrorRecord( 1, 1, trim( RecurringErrors( Loop ).Message( 16 ) ), RecurringErrors( Loop ).Count );
					}
				}
			}
			StatMessage = " ";
			if ( RecurringErrors( Loop ).ReportMax ) {
				MaxOut = RoundSigDigits( RecurringErrors( Loop ).MaxValue, 6 );
				MaxOut = RemoveTrailingZeros( MaxOut );
				StatMessage = trim( StatMessage ) + "  Max=" + trim( MaxOut ) + " " + trim( RecurringErrors( Loop ).MaxUnits );
			}
			if ( RecurringErrors( Loop ).ReportMin ) {
				MinOut = RoundSigDigits( RecurringErrors( Loop ).MinValue, 6 );
				MinOut = RemoveTrailingZeros( MinOut );
				StatMessage = trim( StatMessage ) + "  Min=" + trim( MinOut ) + " " + trim( RecurringErrors( Loop ).MinUnits );
			}
			if ( RecurringErrors( Loop ).ReportSum ) {
				SumOut = RoundSigDigits( RecurringErrors( Loop ).SumValue, 6 );
				SumOut = RemoveTrailingZeros( SumOut );
				StatMessage = trim( StatMessage ) + "  Sum=" + trim( SumOut ) + " " + trim( RecurringErrors( Loop ).SumUnits );
			}
			if ( RecurringErrors( Loop ).ReportMax || RecurringErrors( Loop ).ReportMin || RecurringErrors( Loop ).ReportSum ) {
				ShowMessage( StatMessageStart + trim( StatMessage ) );
			}
		}
		ShowMessage( " " );
	}

}

//     NOTICE
//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
//     and The Regents of the University of California through Ernest Orlando Lawrence
//     Berkeley National Laboratory.  All rights reserved.
//     Portions of the EnergyPlus software package have been developed and copyrighted
//     by other individuals, companies and institutions.  These portions have been
//     incorporated into the EnergyPlus software package under license.   For a complete
//     list of contributors, see "Notice" located in EnergyPlus.f90.
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

} // EnergyPlus
