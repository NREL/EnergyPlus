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
		std::string const & CompName, // name of the fan
		int const FanCurvePtr       // pointer of the fan curve
	);

} // FaultsManager

} // EnergyPlus

#endif
