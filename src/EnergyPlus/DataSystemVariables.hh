#ifndef DataSystemVariables_hh_INCLUDED
#define DataSystemVariables_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataSystemVariables {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const iASCII_CR; // endline value when just CR instead of CR/LF
	extern int const iUnicode_end; // endline value when Unicode file
	extern char const tabchar;
	extern int const GoodIOStatValue; // good value for IOStat during reads/writes
	extern int const MaxTimingStringLength; // string length for timing string array

	extern std::string const DDOnlyEnvVar; // Only run design days
	extern std::string const ReverseDDEnvVar; // Reverse DD during run
	extern std::string const FullAnnualSimulation; // Generate annual run
	extern std::string const cDeveloperFlag;
	extern std::string const cDisplayAllWarnings;
	extern std::string const cDisplayExtraWarnings;
	extern std::string const cDisplayAdvancedReportVariables;
	extern std::string const cDisplayUnusedObjects;
	extern std::string const cDisplayUnusedSchedules;
	extern std::string const cDisplayZoneAirHeatBalanceOffBalance;
	extern std::string const cSortIDD;
	extern std::string const cReportDuringWarmup;
	extern std::string const cReportDuringHVACSizingSimulation;
	extern std::string const cIgnoreSolarRadiation;
	extern std::string const cIgnoreBeamRadiation;
	extern std::string const cIgnoreDiffuseRadiation;
	extern std::string const cSutherlandHodgman;
	extern std::string const cMinimalSurfaceVariables;
	extern std::string const cMinimalShadowing;
	extern std::string const cNumThreads;
	extern std::string const cepNumThreads;
	extern std::string const cNumActiveSims;
	extern std::string const cInputPath1; // EP-Launch setting.  Full path + project name
	extern std::string const cInputPath2; // RunEplus.bat setting.  Full path
	extern std::string const cProgramPath;
	extern std::string const cTimingFlag;
	extern std::string const TrackAirLoopEnvVar; // To generate a file with runtime statistics
	// for each controller on each air loop
	extern std::string const TraceAirLoopEnvVar; // To generate a trace file with the converged
	// solutions of all controllers on each air loop at each call to SimAirLoop()
	extern std::string const TraceHVACControllerEnvVar; // To generate a trace file for
	//  each individual HVAC controller with all controller iterations

	extern std::string const MinReportFrequencyEnvVar; // environment var for reporting frequency.
	extern std::string const cDisplayInputInAuditEnvVar; // environmental variable that enables the echoing of the input file into the audit file

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern bool DDOnly; // TRUE if design days (sizingperiod:*) only are to be run.
	extern bool ReverseDD; // TRUE if reverse design days (reordering sizingperiod:*) are to be run.
	extern bool FullAnnualRun; // TRUE if full annual simulation is to be run.
	extern bool DeveloperFlag; // TRUE if developer flag is turned on. (turns on more displays to console)
	extern bool TimingFlag; // TRUE if timing flag is turned on. (turns on more timing displays to console)
	extern bool SutherlandHodgman; // TRUE if SutherlandHodgman algorithm for polygon clipping is to be used.
	extern bool DetailedSkyDiffuseAlgorithm; // use detailed diffuse shading algorithm for sky (shading transmittance varies)
	extern bool DetailedSolarTimestepIntegration; // when true, use detailed timestep integration for all solar,shading, etc.
	extern bool TrackAirLoopEnvFlag; // If TRUE generates a file with runtime statistics for each HVAC
	//  controller on each air loop
	extern bool TraceAirLoopEnvFlag; // If TRUE generates a trace file with the converged solutions of all
	// HVAC controllers on each air loop at each call to SimAirLoop()
	extern bool TraceHVACControllerEnvFlag; // If TRUE generates a trace file for each individual HVAC
	// controller with all controller iterations
	extern bool ReportDuringWarmup; // True when the report outputs even during warmup
	extern bool ReportDuringHVACSizingSimulation; // true when reporting outputs during HVAC sizing Simulation
	extern bool ReportDetailedWarmupConvergence; // True when the detailed warmup convergence is requested
	extern bool UpdateDataDuringWarmupExternalInterface; // variable sets in the external interface.
	// This update the value during the warmup added for FMI
	extern Real64 Elapsed_Time; // For showing elapsed time at end of run
	extern Real64 Time_Start; // Call to CPU_Time for start time of simulation
	extern Real64 Time_Finish; // Call to CPU_Time for end time of simulation
	extern std::string cMinReportFrequency; // String for minimum reporting frequency
	extern int MinReportFrequency; // Frequency var turned into integer during get report var input.
	extern bool SortedIDD; // after processing, use sorted IDD to obtain Defs, etc.
	extern bool lMinimalShadowing; // TRUE if MinimalShadowing is to override Solar Distribution flag
	extern std::string TempFullFileName;
	extern std::string envinputpath1;
	extern std::string envinputpath2;
	extern std::string envprogrampath;
	extern bool TestAllPaths;
	extern int iEnvSetThreads;
	extern bool lEnvSetThreadsInput;
	extern int iepEnvSetThreads;
	extern bool lepSetThreadsInput;
	extern int iIDFSetThreads;
	extern bool lIDFSetThreadsInput;
	extern int inumActiveSims;
	extern bool lnumActiveSims;
	extern int MaxNumberOfThreads;
	extern int NumberIntRadThreads;
	extern int iNominalTotSurfaces;
	extern bool Threading;

	// Functions

	void
	CheckForActualFileName(
		std::string const & originalInputFileName, // name as input for object
		bool & FileFound, // Set to true if file found and is in CheckedFileName
		std::string & CheckedFileName // Blank if not found.
	);

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

} // DataSystemVariables

} // EnergyPlus

#endif
