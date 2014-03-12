#ifndef DataSystemVariables_hh_INCLUDED
#define DataSystemVariables_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Fstring.hh>

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
	extern Fstring const tabchar;
	extern int const GoodIOStatValue; // good value for IOStat during reads/writes
	extern int const MaxTimingStringLength; // string length for timing string array

	extern Fstring const DDOnlyEnvVar; // Only run design days
	extern Fstring const ReverseDDEnvVar; // Reverse DD during run
	extern Fstring const FullAnnualSimulation; // Generate annual run
	extern Fstring const cDeveloperFlag;
	extern Fstring const cDisplayAllWarnings;
	extern Fstring const cDisplayExtraWarnings;
	extern Fstring const cDisplayAdvancedReportVariables;
	extern Fstring const cDisplayUnusedObjects;
	extern Fstring const cDisplayUnusedSchedules;
	extern Fstring const cDisplayZoneAirHeatBalanceOffBalance;
	extern Fstring const cSortIDD;
	extern Fstring const cReportDuringWarmup;
	extern Fstring const cIgnoreSolarRadiation;
	extern Fstring const cIgnoreBeamRadiation;
	extern Fstring const cIgnoreDiffuseRadiation;
	extern Fstring const cSutherlandHodgman;
	extern Fstring const cMinimalSurfaceVariables;
	extern Fstring const cMinimalShadowing;
	extern Fstring const cNumThreads;
	extern Fstring const cepNumThreads;
	extern Fstring const cNumActiveSims;
	extern Fstring const cInputPath1; // EP-Launch setting.  Full path + project name
	extern Fstring const cInputPath2; // RunEplus.bat setting.  Full path
	extern Fstring const cProgramPath;
	extern Fstring const cTimingFlag;
	extern Fstring const TrackAirLoopEnvVar; // To generate a file with runtime statistics
	// for each controller on each air loop
	extern Fstring const TraceAirLoopEnvVar; // To generate a trace file with the converged
	// solutions of all controllers on each air loop at each call to SimAirLoop()
	extern Fstring const TraceHVACControllerEnvVar; // To generate a trace file for
	//  each individual HVAC controller with all controller iterations

	extern Fstring const MinReportFrequencyEnvVar; // environment var for reporting frequency.

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
	extern bool ReportDetailedWarmupConvergence; // True when the detailed warmup convergence is requested
	extern bool UpdateDataDuringWarmupExternalInterface; // variable sets in the external interface.
	// This update the value during the warmup added for FMI
	extern Real64 Elapsed_Time; // For showing elapsed time at end of run
	extern Real64 Time_Start; // Call to CPU_Time for start time of simulation
	extern Real64 Time_Finish; // Call to CPU_Time for end time of simulation
	extern Fstring cMinReportFrequency; // String for minimum reporting frequency
	extern int MinReportFrequency; // Frequency var turned into integer during get report var input.
	extern bool SortedIDD; // after processing, use sorted IDD to obtain Defs, etc.
	extern bool lMinimalShadowing; // TRUE if MinimalShadowing is to override Solar Distribution flag
	extern Fstring TempFullFileName;
	extern Fstring envinputpath1;
	extern Fstring envinputpath2;
	extern Fstring envprogrampath;
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
		Fstring const & originalInputFileName, // name as input for object
		bool & FileFound, // Set to true if file found and is in CheckedFileName
		Fstring & CheckedFileName // Blank if not found.
	);

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

#endif
