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

} // DataSystemVariables

} // EnergyPlus
