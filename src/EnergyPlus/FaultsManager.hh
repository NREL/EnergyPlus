#ifndef FaultsManager_hh_INCLUDED
#define FaultsManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

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
	// Types of faults under Group Operational Faults in IDD
	//  1. Temperature sensor offset
	//  2. Humidity sensor offset
	//  3. Enthalpy sensor offset
	//  4. Fouling coils
	// coming ...
	//  5. Pressure sensor offset
	//  6. Fouling: chillers, boilers, cooling towers
	//  7. Damper leakage: return air, outdoor air
	//  8. Blockage: pipe
	//  9. Dirty: air filter
	//  10. Meter: air flow, water flow
	//  11. CO2 sensor
	//  12. more
	extern FArray1D_string const cFaults;
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

	extern FArray1D_int const iFaultTypeEnums;

	extern bool AnyFaultsInModel; // True if there are operationla faults in the model
	extern int NumFaults; // Number of faults (include multiple faults of same type) in the model
	extern int NumFouledCoil; // Total number of fouled coils

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

		// Default Constructor
		FaultProperties() :
			ControllerTypeEnum( 0 ),
			ControllerID( 0 ),
			Offset( 0.0 ),
			Status( false ),
			AvaiSchedPtr( 0 ),
			SeveritySchedPtr( 0 ),
			FaultTypeEnum( 0 ),
			FouledCoilID( 0 ),
			FoulingInputMethod( 0 ),
			UAFouled( 0.0 ),
			Rfw( 0.0 ),
			Rfa( 0.0 ),
			Aout( 0.0 ),
			Aratio( 0.0 )
		{}

		// Member Constructor
		FaultProperties(
			std::string const & Name,
			std::string const & FaultType, // Fault type
			std::string const & AvaiSchedule, // Availability schedule
			std::string const & SeveritySchedule, // Severity schedule, multipliers to the Offset
			std::string const & ControllerType, // Controller type
			int const ControllerTypeEnum,
			std::string const & ControllerName, // Controller name
			int const ControllerID, // Point to a controller associated with the fault
			Real64 const Offset, // offset, + means sensor reading is higher than actual value
			bool const Status, // for future use
			int const AvaiSchedPtr,
			int const SeveritySchedPtr,
			int const FaultTypeEnum,
			std::string const & FouledCoilName, // The fouled coil name
			int const FouledCoilID, // Point to a fouling coil
			int const FoulingInputMethod, // Coil fouling input method
			Real64 const UAFouled, // Fouling coil UA under rating conditions
			Real64 const Rfw, // Water side fouling factor
			Real64 const Rfa, // Air side fouling factor
			Real64 const Aout, // Coil outside surface area
			Real64 const Aratio // Inside to outside surface area ratio
		) :
			Name( Name ),
			FaultType( FaultType ),
			AvaiSchedule( AvaiSchedule ),
			SeveritySchedule( SeveritySchedule ),
			ControllerType( ControllerType ),
			ControllerTypeEnum( ControllerTypeEnum ),
			ControllerName( ControllerName ),
			ControllerID( ControllerID ),
			Offset( Offset ),
			Status( Status ),
			AvaiSchedPtr( AvaiSchedPtr ),
			SeveritySchedPtr( SeveritySchedPtr ),
			FaultTypeEnum( FaultTypeEnum ),
			FouledCoilName( FouledCoilName ),
			FouledCoilID( FouledCoilID ),
			FoulingInputMethod( FoulingInputMethod ),
			UAFouled( UAFouled ),
			Rfw( Rfw ),
			Rfa( Rfa ),
			Aout( Aout ),
			Aratio( Aratio )
		{}

	};

	// Object Data
	extern FArray1D< FaultProperties > Faults;
	extern FArray1D< FaultProperties > FouledCoils;

	// Functions

	void
	CheckAndReadFaults();

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
