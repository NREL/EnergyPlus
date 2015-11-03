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

} // DataReportingFlags

} // EnergyPlus
