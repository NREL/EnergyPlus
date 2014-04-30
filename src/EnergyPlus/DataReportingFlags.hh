#ifndef DataReportingFlags_hh_INCLUDED
#define DataReportingFlags_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataReportingFlags {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfWarmupDays; // reinitialized for each environment.
	extern std::string cWarmupDay;
	extern bool DisplayPerfSimulationFlag; // True when "Performing Simulation" should be displayed
	extern bool DoWeatherInitReporting; // Init reporting -- items that go onto OutputFileInits
	extern bool PrintEndDataDictionary; // Flag for printing "End of Data Dictionary" on output files
	extern bool IgnoreInteriorWindowTransmission; // True when section "IgnoreInteriorWindowTransmission" is entered
	extern bool MakeMirroredDetachedShading; // True (default) when Detached Shading Surfaces should be "mirrored"
	extern bool MakeMirroredAttachedShading; // True (default) when Attached Shading Surfaces should be "mirrored"
	extern bool DebugOutput;
	extern bool EvenDuringWarmup;

} // DataReportingFlags

} // EnergyPlus

#endif
