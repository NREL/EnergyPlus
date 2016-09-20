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

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct HybridModelProperties
	{
		// Members
		int ZonePtr;
		int ZoneMeasuredTemperatureSchedulePtr;
		std::string InternalThermalMassCalc;
		std::string InfiltrationCalc;
		int ZoneMeasuredTemperatureStartMonth;
		int ZoneMeasuredTemperatureStartDate;
		int ZoneMeasuredTemperatureEndMonth;
		int ZoneMeasuredTemperatureEndDate;

		// Default Constructor
		HybridModelProperties() :

			ZoneMeasuredTemperatureSchedulePtr(0),
			ZoneMeasuredTemperatureStartMonth(1),
			ZoneMeasuredTemperatureStartDate(1),
			ZoneMeasuredTemperatureEndMonth(1),
			ZoneMeasuredTemperatureEndDate(1)
		{}

		// Member Constructor
		HybridModelProperties(
			
			int const ZonePtr,
			int const ZoneMeasuredTemperatureSchedulePtr, // Zone measured temperature
			std::string const InternalThermalMassCalc, // 
			std::string const InfiltrationCalc, // 
			int const ZoneMeasuredTemperatureStartMonth,
			int const ZoneMeasuredTemperatureStartDate,
			int const ZoneMeasuredTemperatureEndMonth, // 
			int const ZoneMeasuredTemperatureEndDate // 
		) :
			ZonePtr( ZonePtr ),
			ZoneMeasuredTemperatureSchedulePtr(ZoneMeasuredTemperatureSchedulePtr),
			InternalThermalMassCalc(InternalThermalMassCalc),
			InfiltrationCalc(InfiltrationCalc),
			ZoneMeasuredTemperatureStartMonth(ZoneMeasuredTemperatureStartMonth),
			ZoneMeasuredTemperatureStartDate(ZoneMeasuredTemperatureStartDate),
			ZoneMeasuredTemperatureEndMonth(ZoneMeasuredTemperatureEndMonth),
			ZoneMeasuredTemperatureEndDate(ZoneMeasuredTemperatureEndDate)
		{}

	};

	// Object Data
	extern Array1D< HybridModelProperties > HybridModelZone;

	// Functions

	void
	CheckAndReadHybridModelZone();

} // HybridModel

namespace ZoneCapacitanceMultiplierResearchSpecial {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// MODULE VARIABLE TYPE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct ZoneCapacitanceMultiplierResearchSpecialProperties
	{
		// Members
		int ZonePtr;
		double ZoneVolCapMultpSens;
		double ZoneVolCapMultpMoist;
		double ZoneVolCapMultpCO2;
		double ZoneVolCapMultpGenContam;

		// Default Constructor
		ZoneCapacitanceMultiplierResearchSpecialProperties() :

			ZoneVolCapMultpSens(1),
			ZoneVolCapMultpMoist(1),
			ZoneVolCapMultpCO2(1),
			ZoneVolCapMultpGenContam(1)
		{}

		// Member Constructor
		ZoneCapacitanceMultiplierResearchSpecialProperties(

			int const ZonePtr,
			double const ZoneVolCapMultpSens,
			double const ZoneVolCapMultpMoist,
			double const ZoneVolCapMultpCO2, // 
			double const ZoneVolCapMultpGenContam // 
			) :
			ZonePtr(ZonePtr),
			ZoneVolCapMultpSens(ZoneVolCapMultpSens),
			ZoneVolCapMultpMoist(ZoneVolCapMultpMoist),
			ZoneVolCapMultpCO2(ZoneVolCapMultpCO2),
			ZoneVolCapMultpGenContam(ZoneVolCapMultpGenContam)
		{}

	};

	// Object Data
	extern Array1D< ZoneCapacitanceMultiplierResearchSpecialProperties > ZoneCapacitanceMultiplierResearchSpecialZone;

	// Functions

	void
		CheckAndReadZoneCapacitanceMultiplierResearchSpecial();

} // ZoneCapacitanceMultiplierResearchSpecial

} // EnergyPlus

#endif
