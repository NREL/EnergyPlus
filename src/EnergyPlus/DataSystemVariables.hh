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

} // DataSystemVariables

} // EnergyPlus

#endif
