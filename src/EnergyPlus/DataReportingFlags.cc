// EnergyPlus Headers
#include <DataReportingFlags.hh>

namespace EnergyPlus {

namespace DataReportingFlags {

	// Module containing the data and routines dealing with Reporting Flags

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   December 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The module contains various reporting flags and character strings
	// that are used in a small number of modules.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumOfWarmupDays( 0 ); // reinitialized for each environment.
	std::string cWarmupDay;
	bool DisplayPerfSimulationFlag( false ); // True when "Performing Simulation" should be displayed
	bool DoWeatherInitReporting( false ); // Init reporting -- items that go onto OutputFileInits
	bool PrintEndDataDictionary( false ); // Flag for printing "End of Data Dictionary" on output files
	bool IgnoreInteriorWindowTransmission( false ); // True when section "IgnoreInteriorWindowTransmission" is entered
	bool MakeMirroredDetachedShading( true ); // True (default) when Detached Shading Surfaces should be "mirrored"
	bool MakeMirroredAttachedShading( true ); // True (default) when Attached Shading Surfaces should be "mirrored"
	bool DebugOutput( false );
	bool EvenDuringWarmup( false );

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

} // DataReportingFlags

} // EnergyPlus
