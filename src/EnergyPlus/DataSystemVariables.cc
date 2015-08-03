// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <DataSystemVariables.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <FileSystem.hh>
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
	using namespace FileSystem;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const iASCII_CR( 13 ); // endline value when just CR instead of CR/LF
	int const iUnicode_end( 0 ); // endline value when Unicode file
	char const tabchar( '\t' );
	int const GoodIOStatValue( 0 ); // good value for IOStat during reads/writes
	int const MaxTimingStringLength( 250 ); // string length for timing string array

	std::string const DDOnlyEnvVar( "DDONLY" ); // Only run design days
	std::string const ReverseDDEnvVar( "REVERSEDD" ); // Reverse DD during run
	std::string const FullAnnualSimulation( "FULLANNUALRUN" ); // Generate annual run
	std::string const cDeveloperFlag( "DeveloperFlag" );
	std::string const cDisplayAllWarnings( "DisplayAllWarnings" );
	std::string const cDisplayExtraWarnings( "DisplayExtraWarnings" );
	std::string const cDisplayAdvancedReportVariables( "DisplayAdvancedReportVariables" );
	std::string const cDisplayUnusedObjects( "DisplayUnusedObjects" );
	std::string const cDisplayUnusedSchedules( "DisplayUnusedSchedules" );
	std::string const cDisplayZoneAirHeatBalanceOffBalance( "DisplayZoneAirHeatBalanceOffBalance" );
	std::string const cSortIDD( "SortIDD" );
	std::string const cReportDuringWarmup( "ReportDuringWarmup" );
	std::string const cReportDuringHVACSizingSimulation( "REPORTDURINGHVACSIZINGSIMULATION" );
	std::string const cIgnoreSolarRadiation( "IgnoreSolarRadiation" );
	std::string const cIgnoreBeamRadiation( "IgnoreBeamRadiation" );
	std::string const cIgnoreDiffuseRadiation( "IgnoreDiffuseRadiation" );
	std::string const cSutherlandHodgman( "SutherlandHodgman" );
	std::string const cMinimalSurfaceVariables( "CreateMinimalSurfaceVariables" );
	std::string const cMinimalShadowing( "MinimalShadowing" );
	std::string const cNumThreads( "OMP_NUM_THREADS" );
	std::string const cepNumThreads( "EP_OMP_NUM_THREADS" );
	std::string const cNumActiveSims( "cntActv" );
	std::string const cInputPath1( "epin" ); // EP-Launch setting.  Full path + project name
	std::string const cInputPath2( "input_path" ); // RunEplus.bat setting.  Full path
	std::string const cProgramPath( "program_path" );
	std::string const cTimingFlag( "TimingFlag" );
	std::string const TrackAirLoopEnvVar( "TRACK_AIRLOOP" ); // To generate a file with runtime statistics
	// for each controller on each air loop
	std::string const TraceAirLoopEnvVar( "TRACE_AIRLOOP" ); // To generate a trace file with the converged
	// solutions of all controllers on each air loop at each call to SimAirLoop()
	std::string const TraceHVACControllerEnvVar( "TRACE_HVACCONTROLLER" ); // To generate a trace file for
	//  each individual HVAC controller with all controller iterations

	std::string const MinReportFrequencyEnvVar( "MINREPORTFREQUENCY" ); // environment var for reporting frequency.
	std::string const cDisplayInputInAuditEnvVar( "DISPLAYINPUTINAUDIT" ); // environmental variable that enables the echoing of the input file into the audit file

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
	bool ReportDuringHVACSizingSimulation( false ); // true when reporting outputs during HVAC sizing Simulation
	bool ReportDetailedWarmupConvergence( false ); // True when the detailed warmup convergence is requested
	bool UpdateDataDuringWarmupExternalInterface( false ); // variable sets in the external interface.
	// This update the value during the warmup added for FMI
	Real64 Elapsed_Time( 0.0 ); // For showing elapsed time at end of run
	Real64 Time_Start( 0.0 ); // Call to CPU_Time for start time of simulation
	Real64 Time_Finish( 0.0 ); // Call to CPU_Time for end time of simulation
	std::string cMinReportFrequency; // String for minimum reporting frequency
	int MinReportFrequency( -2 ); // Frequency var turned into integer during get report var input.
	bool SortedIDD( true ); // after processing, use sorted IDD to obtain Defs, etc.
	bool lMinimalShadowing( false ); // TRUE if MinimalShadowing is to override Solar Distribution flag
	std::string TempFullFileName;
	std::string envinputpath1;
	std::string envinputpath2;
	std::string envprogrampath;
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
		std::string const & originalInputFileName, // name as input for object
		bool & FileFound, // Set to true if file found and is in CheckedFileName
		std::string & CheckedFileName // Blank if not found.
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
		static std::string const blank;
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool FileExist( false ); // initialize to false, then override to true if present
		static int EchoInputFile; // found unit number for "eplusout.audit"
		static bool firstTime( true );
		std::string InputFileName; // save for changing out path characters
		std::string::size_type pos;

		if ( firstTime ) {
			EchoInputFile = FindUnitNumber( DataStringGlobals::outputAuditFileName );
			get_environment_variable( cInputPath1, envinputpath1 );
			if ( envinputpath1 != blank ) {
				pos = index( envinputpath1, pathChar, true ); // look backwards for pathChar
				if ( pos != std::string::npos ) envinputpath1.erase( pos + 1 );
			}
			get_environment_variable( cInputPath2, envinputpath2 );
			get_environment_variable( cProgramPath, ProgramPath );
			firstTime = false;
		}

		FileFound = false;
		CheckedFileName = blank;
		InputFileName = originalInputFileName;
		makeNativePath(InputFileName);

		{ IOFlags flags; gio::inquire( InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (user input)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (user input)=" + getAbsolutePath(InputFileName);
		}

		// Look relative to input file path
		{ IOFlags flags; gio::inquire( DataStringGlobals::idfDirPathName + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = DataStringGlobals::idfDirPathName + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (idf)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (idf)=" + getAbsolutePath(DataStringGlobals::idfDirPathName + InputFileName);
		}

		// Look relative to input path
		{ IOFlags flags; gio::inquire( envinputpath1 + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = envinputpath1 + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (epin)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (epin)=" + getAbsolutePath(envinputpath1 + InputFileName);
		}

		// Look relative to input path
		{ IOFlags flags; gio::inquire( envinputpath2 + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = envinputpath2 + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (input_path)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (input_path)=" + getAbsolutePath(envinputpath2 + InputFileName);
		}

		// Look relative to program path
		{ IOFlags flags; gio::inquire( envprogrampath + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = envprogrampath + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (program_path)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (program_path)=" + getAbsolutePath(envprogrampath + InputFileName);
		}

		if ( ! TestAllPaths ) return;

		// Look relative to current working folder
		{ IOFlags flags; gio::inquire( CurrentWorkingFolder + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = CurrentWorkingFolder + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (CWF)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (CWF)=" + getAbsolutePath(CurrentWorkingFolder + InputFileName);
		}

		// Look relative to program path
		{ IOFlags flags; gio::inquire( ProgramPath + InputFileName, flags ); FileExist = flags.exists(); }
		if ( FileExist ) {
			FileFound = true;
			CheckedFileName = ProgramPath + InputFileName;
			gio::write(EchoInputFile, fmtA) << "found (program path - ini)=" + getAbsolutePath(CheckedFileName);
			return;
		} else {
			gio::write(EchoInputFile, fmtA) << "not found (program path - ini)=" + getAbsolutePath(ProgramPath + InputFileName);
		}

	}

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // DataSystemVariables

} // EnergyPlus
