#ifndef HybridModel_hh_INCLUDED
#define HybridModel_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HybridModel {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// MODULE VARIABLE TYPE DECLARATIONS:
	extern bool FlagHybridModel; // True if hybrid model is activated
	extern bool FlagHMInternalThermalMass; // True if hybrid model (internal thermal mass) is activated
	extern bool FlagHMInfiltration; // True if hybrid model (infiltration) is activated
	extern int NumOfHybridModelZones; // Number of hybrid model zones in the model

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct HybridModelProperties
	{
		// Members
		std::string Name;
		int ZoneMeasuredTemperatureSchedulePtr;
		bool InternalThermalMassCalc;
		bool InfiltrationCalc;
		int ZoneMeasuredTemperatureStartMonth;
		int ZoneMeasuredTemperatureStartDate;
		int ZoneMeasuredTemperatureEndMonth;
		int ZoneMeasuredTemperatureEndDate;

		// Default Constructor
		HybridModelProperties() :
			ZoneMeasuredTemperatureSchedulePtr( 0 ),
			InternalThermalMassCalc( false ),
			InfiltrationCalc( false ),
			ZoneMeasuredTemperatureStartMonth( 0 ),
			ZoneMeasuredTemperatureStartDate( 0 ),
			ZoneMeasuredTemperatureEndMonth( 0 ),
			ZoneMeasuredTemperatureEndDate( 0 )
		{}

	};

	// Object Data
	extern Array1D< HybridModelProperties > HybridModelZone;

	// Functions

	void
	CheckAndReadHybridModelZone();

} // HybridModel

} // EnergyPlus

#endif
