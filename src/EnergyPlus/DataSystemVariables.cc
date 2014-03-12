// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataSystemVariables.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataSystemVariables {

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   May 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for system (such as environment) variables that are set
	// before a run or set of runs.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataStringGlobals::pathChar;
	using DataStringGlobals::altpathChar;
	using DataStringGlobals::CurrentWorkingFolder;
	using DataStringGlobals::ProgramPath;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const iASCII_CR( 13 ); // endline value when just CR instead of CR/LF
	int const iUnicode_end( 0 ); // endline value when Unicode file
	Fstring const tabchar( 1, CHAR( 9 ) );
	int const GoodIOStatValue( 0 ); // good value for IOStat during reads/writes
	int const MaxTimingStringLength( 250 ); // string length for timing string array

	Fstring const DDOnlyEnvVar( "DDONLY" ); // Only run design days
	Fstring const ReverseDDEnvVar( "REVERSEDD" ); // Reverse DD during run
	Fstring const FullAnnualSimulation( "FULLANNUALRUN" ); // Generate annual run
	Fstring const cDeveloperFlag( "DeveloperFlag" );
	Fstring const cDisplayAllWarnings( "DisplayAllWarnings" );
	Fstring const cDisplayExtraWarnings( "DisplayExtraWarnings" );
	Fstring const cDisplayAdvancedReportVariables( "DisplayAdvancedReportVariables" );
	Fstring const cDisplayUnusedObjects( "DisplayUnusedObjects" );
	Fstring const cDisplayUnusedSchedules( "DisplayUnusedSchedules" );
	Fstring const cDisplayZoneAirHeatBalanceOffBalance( "DisplayZoneAirHeatBalanceOffBalance" );
	Fstring const cSortIDD( "SortIDD" );
	Fstring const cReportDuringWarmup( "ReportDuringWarmup" );
	Fstring const cIgnoreSolarRadiation( "IgnoreSolarRadiation" );
	Fstring const cIgnoreBeamRadiation( "IgnoreBeamRadiation" );
	Fstring const cIgnoreDiffuseRadiation( "IgnoreDiffuseRadiation" );
	Fstring const cSutherlandHodgman( "SutherlandHodgman" );
	Fstring const cMinimalSurfaceVariables( "CreateMinimalSurfaceVariables" );
	Fstring const cMinimalShadowing( "MinimalShadowing" );
	Fstring const cNumThreads( "OMP_NUM_THREADS" );
	Fstring const cepNumThreads( "EP_OMP_NUM_THREADS" );
	Fstring const cNumActiveSims( "cntActv" );
	Fstring const cInputPath1( "epin" ); // EP-Launch setting.  Full path + project name
	Fstring const cInputPath2( "input_path" ); // RunEplus.bat setting.  Full path
	Fstring const cProgramPath( "program_path" );
	Fstring const cTimingFlag( "TimingFlag" );
	Fstring const TrackAirLoopEnvVar( "TRACK_AIRLOOP" ); // To generate a file with runtime statistics
	// for each controller on each air loop
	Fstring const TraceAirLoopEnvVar( "TRACE_AIRLOOP" ); // To generate a trace file with the converged
	// solutions of all controllers on each air loop at each call to SimAirLoop()
	Fstring const TraceHVACControllerEnvVar( "TRACE_HVACCONTROLLER" ); // To generate a trace file for
	//  each individual HVAC controller with all controller iterations

	Fstring const MinReportFrequencyEnvVar( "MINREPORTFREQUENCY" ); // environment var for reporting frequency.

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	bool DDOnly( false ); // TRUE if design days (sizingperiod:*) only are to be run.
	bool ReverseDD( false ); // TRUE if reverse design days (reordering sizingperiod:*) are to be run.
	bool FullAnnualRun( false ); // TRUE if full annual simulation is to be run.
	bool DeveloperFlag( false ); // TRUE if developer flag is turned on. (turns on more displays to console)
	bool TimingFlag( false ); // TRUE if timing flag is turned on. (turns on more timing displays to console)
	bool SutherlandHodgman( true ); // TRUE if SutherlandHodgman algorithm for polygon clipping is to be used.
	bool DetailedSkyDiffuseAlgorithm( false ); // use detailed diffuse shading algorithm for sky (shading transmittance varies)
	bool DetailedSolarTimestepIntegration( false ); // when true, use detailed timestep integration for all solar,shading, etc.
	bool TrackAirLoopEnvFlag( false ); // If TRUE generates a file with runtime statistics for each HVAC
	//  controller on each air loop
	bool TraceAirLoopEnvFlag( false ); // If TRUE generates a trace file with the converged solutions of all
	// HVAC controllers on each air loop at each call to SimAirLoop()
	bool TraceHVACControllerEnvFlag( false ); // If TRUE generates a trace file for each individual HVAC
	// controller with all controller iterations
	bool ReportDuringWarmup( false ); // True when the report outputs even during warmup
	bool ReportDetailedWarmupConvergence( false ); // True when the detailed warmup convergence is requested
	bool UpdateDataDuringWarmupExternalInterface( false ); // variable sets in the external interface.
	// This update the value during the warmup added for FMI
	Real64 Elapsed_Time( 0.0 ); // For showing elapsed time at end of run
	Real64 Time_Start( 0.0 ); // Call to CPU_Time for start time of simulation
	Real64 Time_Finish( 0.0 ); // Call to CPU_Time for end time of simulation
	Fstring cMinReportFrequency( 15 ); // String for minimum reporting frequency
	int MinReportFrequency( -2 ); // Frequency var turned into integer during get report var input.
	bool SortedIDD( true ); // after processing, use sorted IDD to obtain Defs, etc.
	bool lMinimalShadowing( false ); // TRUE if MinimalShadowing is to override Solar Distribution flag
	Fstring TempFullFileName( 500 );
	Fstring envinputpath1( 255 );
	Fstring envinputpath2( 255 );
	Fstring envprogrampath( 255 );
	bool TestAllPaths( false );
	int iEnvSetThreads( 0 );
	bool lEnvSetThreadsInput( false );
	int iepEnvSetThreads( 0 );
	bool lepSetThreadsInput( false );
	int iIDFSetThreads( 0 );
	bool lIDFSetThreadsInput( false );
	int inumActiveSims( 1 );
	bool lnumActiveSims( false );
	int MaxNumberOfThreads( 1 );
	int NumberIntRadThreads( 1 );
	int iNominalTotSurfaces( 0 );
	bool Threading( false );

	// Functions

	void
	CheckForActualFileName(
		Fstring const & originalInputFileName, // name as input for object
		bool & FileFound, // Set to true if file found and is in CheckedFileName
		Fstring & CheckedFileName // Blank if not found.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// With the Windows version, there are subfolders set and the input file names may not
		// be accurate. This searches a few folders (CurrentWorkingFolder, Program folder) to see
		// if the file can be found. (It may have been input with full path so that is checked first.)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Fstring const blank;

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool FileExist;
		static int EchoInputFile; // found unit number for "eplusout.audit"
		static bool firstTime( true );
		Fstring InputFileName( len( originalInputFileName ) ); // save for changing out path characters
		int pos;

		if ( firstTime ) {
			EchoInputFile = FindUnitNumber( "eplusout.audit" );
			envinputpath1 = blank;
			get_environment_variable( cInputPath1, envinputpath1 );
			if ( envinputpath1 != blank ) {
				pos = index( envinputpath1, pathChar, true ); // look backwards for pathChar
				if ( pos != 0 ) envinputpath1 = envinputpath1( 1, pos );
			}
			envinputpath2 = blank;
			get_environment_variable( cInputPath2, envinputpath2 );
			ProgramPath = blank;
			get_environment_variable( cProgramPath, ProgramPath );
			firstTime = false;
		}

		CheckedFileName = blank;
		InputFileName = originalInputFileName;
		pos = index( InputFileName, altpathChar );
		while ( pos > 0 ) {
			InputFileName( pos, pos ) = pathChar;
			pos = index( InputFileName, altpathChar );
		}

		{ IOFlags flags; gio::inquire( trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = InputFileName;
			gio::write( EchoInputFile, "(A)" ) << "found (user input)=" + trim( InputFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (user input)=" + trim( InputFileName );
		}

		// Look relative to input path
		{ IOFlags flags; gio::inquire( trim( envinputpath1 ) + trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = trim( envinputpath1 ) + trim( InputFileName );
			gio::write( EchoInputFile, "(A)" ) << "found (epin)=" + trim( CheckedFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (epin)=" + trim( envinputpath1 ) + trim( InputFileName );
		}

		// Look relative to input path
		{ IOFlags flags; gio::inquire( trim( envinputpath2 ) + trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = trim( envinputpath2 ) + trim( InputFileName );
			gio::write( EchoInputFile, "(A)" ) << "found (input_path)=" + trim( CheckedFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (input_path)=" + trim( envinputpath2 ) + trim( InputFileName );
		}

		// Look relative to program path
		{ IOFlags flags; gio::inquire( trim( envprogrampath ) + trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = trim( envprogrampath ) + trim( InputFileName );
			gio::write( EchoInputFile, "(A)" ) << "found (program_path)=" + trim( CheckedFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (program_path)=" + trim( envprogrampath ) + trim( InputFileName );
		}

		if ( ! TestAllPaths ) return;

		// Look relative to current working folder
		{ IOFlags flags; gio::inquire( trim( CurrentWorkingFolder ) + trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = trim( CurrentWorkingFolder ) + trim( InputFileName );
			gio::write( EchoInputFile, "(A)" ) << "found (CWF)=" + trim( CheckedFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (CWF)=" + trim( CurrentWorkingFolder ) + trim( InputFileName );
		}

		// Look relative to program path
		{ IOFlags flags; gio::inquire( trim( ProgramPath ) + trim( InputFileName ), flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = trim( ProgramPath ) + trim( InputFileName );
			gio::write( EchoInputFile, "(A)" ) << "found (program path - ini)=" + trim( CheckedFileName );
			return;
		} else {
			gio::write( EchoInputFile, "(A)" ) << "not found (program path - ini)=" + trim( ProgramPath ) + trim( InputFileName );
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

} // DataSystemVariables

} // EnergyPlus
