#ifndef FaultsManager_hh_INCLUDED
#define FaultsManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace FaultsManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// ControllerTypeEnum
	extern int const iController_AirEconomizer;

	// Input methods for fouling coils
	extern int const iFouledCoil_UARated;
	extern int const iFouledCoil_FoulingFactor;

	// MODULE VARIABLE DECLARATIONS:
	extern int const NumFaultTypes;

	// FaultTypeEnum
	extern int const iFault_TemperatureSensorOffset_OutdoorAir;
	extern int const iFault_HumiditySensorOffset_OutdoorAir;
	extern int const iFault_EnthalpySensorOffset_OutdoorAir;
	extern int const iFault_TemperatureSensorOffset_ReturnAir;
	extern int const iFault_EnthalpySensorOffset_ReturnAir;
	extern int const iFault_Fouling_Coil;
	extern int const iFault_ThermostatOffset;
	extern int const iFault_HumidistatOffset;
	extern int const iFault_Fouling_AirFilter;
	
	// Types of faults under Group Operational Faults in IDD
	//  1. Temperature sensor offset
	//  2. Humidity sensor offset
	//  3. Enthalpy sensor offset
	//  4. Fouling coils
	//  5. Thermostat offset
	//  6. Humidistat offset
	//  7. Fouling air filter
	// coming ...
	//  8. Fouling: chillers, boilers, cooling towers
	//  9. Damper leakage: return air, outdoor air
	//  10. Blockage: pipe
	//  11. Meter: air flow, water flow
	//  12. CO2 sensor
	//  13. Pressure sensor offset
	//  14. more

	extern Array1D_string const cFaults;
	//      'FaultModel:PressureSensorOffset:OutdoorAir   ', &
	//      'FaultModel:TemperatureSensorOffset:SupplyAir ', &
	//      'FaultModel:TemperatureSensorOffset:ZoneAir   ', &
	//      'FaultModel:Blockage:Branch                   ', &
	//      'FaultModel:Dirty:AirFilter                   ', &
	//      'FaultModel:Fouling:Chiller                   ', &
	//      'FaultModel:Fouling:Boiler                    ', &
	//      'FaultModel:Fouling:CoolingTower              ', &
	//      'FaultModel:DamperLeakage:ReturnAir           ', &
	//      'FaultModel:DamperLeakage:OutdoorAir          ' /)

	extern Array1D_int const iFaultTypeEnums;

	extern bool AnyFaultsInModel; // True if there are operationla faults in the model
	extern int NumFaults; // Number of faults (include multiple faults of same type) in the model
	extern int NumFouledCoil; // Total number of fouled coils
	extern int NumFaultyThermostat; // Total number of faulty thermostat with offset
	extern int NumFaultyHumidistat; // Total number of faulty humidistat with offset
	extern int NumFaultyAirFilter;  // Total number of fouled air filters

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct FaultProperties // Derived type for operational faults
	{
		// Members
		std::string Name;
		std::string FaultType; // Fault type
		std::string AvaiSchedule; // Availability schedule
		std::string SeveritySchedule; // Severity schedule, multipliers to the Offset
		std::string ControllerType; // Controller type
		int ControllerTypeEnum;
		std::string ControllerName; // Controller name
		int ControllerID; // Point to a controller associated with the fault
		Real64 Offset; // offset, + means sensor reading is higher than actual value
		bool Status; // for future use
		int AvaiSchedPtr;
		int SeveritySchedPtr;
		int FaultTypeEnum;

		std::string FouledCoilName; // The fouled coil name
		int FouledCoilID; // Point to a fouling coil
		int FoulingInputMethod; // Coil fouling input method
		Real64 UAFouled; // Fouling coil UA under rating conditions
		Real64 Rfw; // Water side fouling factor
		Real64 Rfa; // Air side fouling factor
		Real64 Aout; // Coil outside surface area
		Real64 Aratio; // Inside to outside surface area ratio

		std::string FaultyThermostatName; // The faulty thermostat name
		std::string FaultyHumidistatName; // The faulty humidistat name
		std::string FaultyHumidistatType; // The faulty humidistat type

		std::string FaultyAirFilterFanName;          // The name of the fan corresponding to the fouled air filter
		std::string FaultyAirFilterFanType;          // The type of the fan corresponding to the fouled air filter
		std::string FaultyAirFilterFanCurve;         // The name of the fan curve
		int         FaultyAirFilterFanCurvePtr;      // The index to the curve 
		std::string FaultyAirFilterPressFracSche;    // Schedule describing variations of the fan pressure rise
		int         FaultyAirFilterPressFracSchePtr; // The pointer to the schedule  
		Real64      FaultyAirFilterFanPressInc;      // The increase of the fan pressure due to fouled air filter
		Real64      FaultyAirFilterFanFlowDec;       // The decrease of the fan airflow rate due to fouled air filter

		// Default Constructor
		FaultProperties() :
			Name( "" ),
			FaultType( "" ),
			AvaiSchedule( "" ),
			SeveritySchedule( "" ),
			ControllerType( "" ),
			ControllerTypeEnum( 0 ),
			ControllerName( "" ),
			ControllerID( 0 ),
			Offset( 0.0 ),
			Status( false ),
			AvaiSchedPtr( 0 ),
			SeveritySchedPtr( 0 ),
			FaultTypeEnum( 0 ),
			FouledCoilName( "" ),
			FouledCoilID( 0 ),
			FoulingInputMethod( 0 ),
			UAFouled( 0.0 ),
			Rfw( 0.0 ),
			Rfa( 0.0 ),
			Aout( 0.0 ),
			Aratio( 0.0 ),
			FaultyThermostatName( "" ),
			FaultyHumidistatName( "" ),
			FaultyHumidistatType( "" ),
			FaultyAirFilterFanName( "" ),
			FaultyAirFilterFanType( "" ),
			FaultyAirFilterFanCurve( "" ),
			FaultyAirFilterFanCurvePtr( 0 ),
			FaultyAirFilterPressFracSche( "" ),
			FaultyAirFilterPressFracSchePtr( 0 ),
			FaultyAirFilterFanPressInc( 0.0 ),
			FaultyAirFilterFanFlowDec( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< FaultProperties > Faults;
	extern Array1D< FaultProperties > FouledCoils;
	extern Array1D< FaultProperties > FaultsThermostatOffset;
	extern Array1D< FaultProperties > FaultsHumidistatOffset;
	extern Array1D< FaultProperties > FaultsFouledAirFilters;

	// Functions

	void
	CheckAndReadFaults();

	bool 
	CheckFaultyAirFilterFanCurve(
		std::string const CompName, // name of the fan 
		int const FanCurvePtr       // pointer of the fan curve
	);

	// *****************************************************************************
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

} // FaultsManager

} // EnergyPlus

#endif
