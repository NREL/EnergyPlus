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
	extern int NumOfHybridModelZones; // Number of hybrid model zones in the model

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct HybridModelProperties
	{
		// Members
		int ZonePtr;
		int ZoneMeasuredTemperatureSchedulePtr;
		bool InternalThermalMassCalc;
		bool InfiltrationCalc;
		int ZoneMeasuredTemperatureStartMonth;
		int ZoneMeasuredTemperatureStartDate;
		int ZoneMeasuredTemperatureEndMonth;
		int ZoneMeasuredTemperatureEndDate;

		// Default Constructor
		HybridModelProperties() :
			ZonePtr( 0 ),
			ZoneMeasuredTemperatureSchedulePtr( 0 ),
			InternalThermalMassCalc( false ),
			InfiltrationCalc( false ),
			ZoneMeasuredTemperatureStartMonth( 1 ),
			ZoneMeasuredTemperatureStartDate( 1 ),
			ZoneMeasuredTemperatureEndMonth( 1 ),
			ZoneMeasuredTemperatureEndDate( 1 )
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
