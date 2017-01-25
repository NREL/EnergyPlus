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

// EnergyPlus Headers
#include <FaultsManager.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <ScheduleManager.hh>
#include <CurveManager.hh>
#include <Fans.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace FaultsManager {

	// MODULE INFORMATION:
	//       AUTHOR         Tianzhen Hong, LBNL
	//       DATE WRITTEN   August 2013
	//       MODIFIED       Sep. 2013 Xiufeng Pang (XP), added fouling coil fault
	//       MODIFIED       Feb. 2015 Rongpeng Zhang, added thermostat/humidistat offset faults
	//       MODIFIED       Apr. 2015 Rongpeng Zhang, added fouling air filter fault
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// This module manages operational faults of buildings and systems.

	// METHODOLOGY EMPLOYED:
	//  Various methods are employed depending types of faults

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::ScheduleAlwaysOn;
	using namespace InputProcessor;

	// Data
	// MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// ControllerTypeEnum
	int const iController_AirEconomizer( 1001 );

	// Input methods for fouling coils
	int const iFouledCoil_UARated( 9001 );
	int const iFouledCoil_FoulingFactor( 9002 );

	// MODULE VARIABLE DECLARATIONS:
	int const NumFaultTypes( 9 );

	// FaultTypeEnum
	int const iFault_TemperatureSensorOffset_OutdoorAir( 101 );
	int const iFault_HumiditySensorOffset_OutdoorAir( 102 );
	int const iFault_EnthalpySensorOffset_OutdoorAir( 103 );
	int const iFault_TemperatureSensorOffset_ReturnAir( 104 );
	int const iFault_EnthalpySensorOffset_ReturnAir( 105 );
	int const iFault_Fouling_Coil( 106 );
	int const iFault_ThermostatOffset( 107 );
	int const iFault_HumidistatOffset( 108 );
	int const iFault_Fouling_AirFilter( 109 );

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

	Array1D_string const cFaults( NumFaultTypes, { "FaultModel:TemperatureSensorOffset:OutdoorAir", "FaultModel:HumiditySensorOffset:OutdoorAir", "FaultModel:EnthalpySensorOffset:OutdoorAir", "FaultModel:TemperatureSensorOffset:ReturnAir", "FaultModel:EnthalpySensorOffset:ReturnAir", "FaultModel:Fouling:Coil", "FaultModel:ThermostatOffset", "FaultModel:HumidistatOffset", "FaultModel:Fouling:AirFilter" } );
	//      'FaultModel:PressureSensorOffset:OutdoorAir   ', &
	//      'FaultModel:TemperatureSensorOffset:SupplyAir ', &
	//      'FaultModel:TemperatureSensorOffset:ZoneAir   ', &
	//      'FaultModel:Blockage:Branch                   ', &
	//      'FaultModel:Fouling:Chiller                   ', &
	//      'FaultModel:Fouling:Boiler                    ', &
	//      'FaultModel:Fouling:CoolingTower              ', &
	//      'FaultModel:DamperLeakage:ReturnAir           ', &
	//      'FaultModel:DamperLeakage:OutdoorAir          ' /)

	Array1D_int const iFaultTypeEnums(NumFaultTypes, { iFault_TemperatureSensorOffset_OutdoorAir, iFault_HumiditySensorOffset_OutdoorAir, iFault_EnthalpySensorOffset_OutdoorAir, iFault_TemperatureSensorOffset_ReturnAir, iFault_EnthalpySensorOffset_ReturnAir, iFault_Fouling_Coil, iFault_ThermostatOffset, iFault_HumidistatOffset, iFault_Fouling_AirFilter });

	bool AnyFaultsInModel( false ); // True if there are operationla faults in the model
	int NumFaults( 0 ); // Number of faults (include multiple faults of same type) in the model
	int NumFouledCoil( 0 ); // Total number of fouled coils
	int NumFaultyThermostat( 0 ); // Total number of faulty thermostat with offset
	int NumFaultyHumidistat( 0 ); // Total number of faulty humidistat with offset
	int NumFaultyAirFilter( 0 ); // Total number of fouled air filters

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< FaultProperties > Faults;
	Array1D< FaultProperties > FouledCoils;
	Array1D< FaultProperties > FaultsThermostatOffset;
	Array1D< FaultProperties > FaultsHumidistatOffset;
	Array1D< FaultProperties > FaultsFouledAirFilters;

	// Functions

	void
	CheckAndReadFaults()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tianzhen Hong
		//       DATE WRITTEN   August 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//  1. Determine if any operational faults are present in a model and set flags
		//  2. Read faults input

		// METHODOLOGY EMPLOYED:
		// Get number of faults-related input objects and assign faults input to data structure

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using ScheduleManager::GetScheduleIndex;
		using CurveManager::GetCurveIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool RunMeOnceFlag( false );

		static bool ErrorsFound( false ); // If errors detected in input
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int IOStatus;
		Array1D_string cAlphaArgs( 10 ); // Alpha input items for object
		static Array1D_bool lAlphaFieldBlanks( 10, false );
		static Array1D_bool lNumericFieldBlanks( 10, false );
		Array1D_string cAlphaFieldNames( 10 );
		Array1D_string cNumericFieldNames( 10 );
		Array1D< Real64 > rNumericArgs( 10 ); // Numeric input items for object

		int i;
		int j; 	//Number of fault objects of type 101-105
		int jFoulingCoil; //Number of fault objects of type 106: fouling coil
		int jFaultyThermostat; //Number of fault objects of type 107: faulty thermostat
		int jFaultyHumidistat; //Number of fault objects of type 108: faulty humidistat
		int jFaultyAirFilter; //Number of fault objects of type 109: fouled air filter
		int jj;
		int iFaults;
		std::string cFault1;

		if ( RunMeOnceFlag ) return;

		// check number of faults
		NumFaults = 0;
		for ( i = 1; i <= NumFaultTypes; ++i ) {
			iFaults = GetNumObjectsFound( cFaults( i ) );
			NumFaults += iFaults;
		}

		// Coil fouling is the 6th fault
		NumFouledCoil = GetNumObjectsFound( cFaults( 6 ) );
		// Faulty thermostat is the 7th fault
		NumFaultyThermostat = GetNumObjectsFound( cFaults( 7 ) );
		// Faulty humidistat is the 8th fault
		NumFaultyHumidistat = GetNumObjectsFound( cFaults( 8 ) );
		// Fouled air filter is the 9th fault
		NumFaultyAirFilter = GetNumObjectsFound( cFaults( 9 ) );

		if ( NumFaults > 0 ) {
			AnyFaultsInModel = true;
		} else {
			AnyFaultsInModel = false;
		}

		if ( ! AnyFaultsInModel ) {
			RunMeOnceFlag = true;
			return;
		}

		// read faults input
		Faults.allocate( NumFaults );
		FouledCoils.allocate( NumFouledCoil );
		FaultsThermostatOffset.allocate( NumFaultyThermostat );
		FaultsHumidistatOffset.allocate( NumFaultyHumidistat );
		FaultsFouledAirFilters.allocate( NumFaultyAirFilter );

		j = 0;
		jFoulingCoil = 0;
		jFaultyThermostat = 0;
		jFaultyHumidistat = 0;
		jFaultyAirFilter = 0;

		for ( i = 1; i <= NumFaultTypes; ++i ) {
			cFault1 = cFaults( i ); // fault object string
			iFaults = GetNumObjectsFound( cFault1 );

			for ( jj = 1; jj <= iFaults; ++jj ) {
				GetObjectItem( cFault1, jj, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( SameString( cFault1, "FaultModel:Fouling:AirFilter" ) ) { // For Fault_type 109: Fouled Air Filters
					++jFaultyAirFilter;

					FaultsFouledAirFilters( jFaultyAirFilter ).FaultType = cFault1;
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsFouledAirFilters( jFaultyAirFilter ).Name = cAlphaArgs( 1 );

					// Informatin of the fan associated with the fouling air filter
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanType = cAlphaArgs( 2 );
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanName = cAlphaArgs( 3 );

					// Check whether the specified fan exsits in the fan list
					if ( FindItemInList( cAlphaArgs( 3 ), Fans::Fan, &Fans::FanEquipConditions::FanName ) != 1 ) {
						ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
						ErrorsFound = true;
					}

					// Assign fault index to the fan object
					for ( int FanNum = 1; FanNum <= Fans::NumFans; ++FanNum ) {
						if ( SameString( Fans::Fan( FanNum ).FanName, cAlphaArgs( 3 ) ) ) {
							Fans::Fan( FanNum ).FaultyFilterFlag = true;
							Fans::Fan( FanNum ).FaultyFilterIndex = jFaultyAirFilter;
							break;
						}
					}

					// Fault availability schedule
					FaultsFouledAirFilters( jFaultyAirFilter ).AvaiSchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FaultsFouledAirFilters( jFaultyAirFilter ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsFouledAirFilters( jFaultyAirFilter ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FaultsFouledAirFilters( jFaultyAirFilter ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Fan pressure increase fraction schedule
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterPressFracSche = cAlphaArgs( 5 );
					if ( lAlphaFieldBlanks( 5 ) ) {
						FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterPressFracSchePtr = -1; // returns schedule value of 1
					} else {
						FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterPressFracSchePtr = GetScheduleIndex( cAlphaArgs( 5 ) );
						if ( FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterPressFracSchePtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Fan curve describing the relationship between fan pressure rise and air flow rate
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanCurve = cAlphaArgs( 6 );
					FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanCurvePtr = GetCurveIndex( cAlphaArgs( 6 ) );
					if ( FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanCurvePtr == 0 ) {
						ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 )  + "\"" );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = \"" + cAlphaArgs( 6 ) + "\" not found."  );
						ErrorsFound = true;
					}

					// Check whether the specified fan curve covers the design operational point of the fan
					if ( !CheckFaultyAirFilterFanCurve( FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanName, FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanCurvePtr ) ) {
						ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 )  + "\"" );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = \"" + cAlphaArgs( 6 ) + "\" does not cover "  );
						ShowContinueError( "the operational point of Fan " + FaultsFouledAirFilters( jFaultyAirFilter ).FaultyAirFilterFanName  );
						ErrorsFound = true;
					}

					//In the fan object, calculate by each time-step: 1) pressure inc value; 2) air flow rate decrease value ......

				} else if ( SameString( cFault1, "FaultModel:HumidistatOffset" ) ) { // For Fault_type 108: HumidistatOffset
					++jFaultyHumidistat;

					FaultsHumidistatOffset( jFaultyHumidistat ).FaultType = cFault1;
					FaultsHumidistatOffset( jFaultyHumidistat ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsHumidistatOffset( jFaultyHumidistat ).Name = cAlphaArgs( 1 );
					FaultsHumidistatOffset( jFaultyHumidistat ).FaultyHumidistatName = cAlphaArgs( 2 );
					FaultsHumidistatOffset( jFaultyHumidistat ).FaultyHumidistatType = cAlphaArgs( 3 );

					if ( SameString( FaultsHumidistatOffset( jFaultyHumidistat ).FaultyHumidistatType, "ThermostatOffsetDependent" ) ) {
					// For Humidistat Offset Type: ThermostatOffsetDependent

						// Related Thermostat Offset Fault Name is required for Humidistat Offset Type: ThermostatOffsetDependent
						if ( lAlphaFieldBlanks( 6 ) ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\": " + cAlphaFieldNames( 6 ) + " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetDependent\"." );
							ErrorsFound = true;
						} else {
							FaultsHumidistatOffset( jFaultyHumidistat ).FaultyThermostatName = cAlphaArgs( 6 );
						}

					} else {
					// For Humidistat Offset Type: ThermostatOffsetIndependent

						// Availability schedule
						FaultsHumidistatOffset( jFaultyHumidistat ).AvaiSchedule = cAlphaArgs( 4 );
						if ( lAlphaFieldBlanks( 4 ) ) {
							FaultsHumidistatOffset( jFaultyHumidistat ).AvaiSchedPtr = -1; // returns schedule value of 1
						} else {
							FaultsHumidistatOffset( jFaultyHumidistat ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
							if ( FaultsHumidistatOffset( jFaultyHumidistat ).AvaiSchedPtr == 0 ) {
								ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
								ErrorsFound = true;
							}
						}

						// Severity schedule
						FaultsHumidistatOffset( jFaultyHumidistat ).SeveritySchedule = cAlphaArgs( 5 );
						if ( lAlphaFieldBlanks( 5 ) ) {
							FaultsHumidistatOffset( jFaultyHumidistat ).SeveritySchedPtr = -1; // returns schedule value of 1
						} else {
							FaultsHumidistatOffset( jFaultyHumidistat ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 5 ) );
							if ( FaultsHumidistatOffset( jFaultyHumidistat ).SeveritySchedPtr == 0 ) {
								ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" not found." );
								ErrorsFound = true;
							}
						}

						// Reference offset value is required for Humidistat Offset Type: ThermostatOffsetIndependent
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\": " + cNumericFieldNames( 1 ) + " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetIndependent\"." );
							ErrorsFound = true;
						} else {
							FaultsHumidistatOffset( jFaultyHumidistat ).Offset = rNumericArgs( 1 );
						}

					}


				} else if ( SameString( cFault1, "FaultModel:ThermostatOffset" ) ) { // For Fault_type 107: ThermostatOffset
					++jFaultyThermostat;

					FaultsThermostatOffset( jFaultyThermostat ).FaultType = cFault1;
					FaultsThermostatOffset( jFaultyThermostat ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsThermostatOffset( jFaultyThermostat ).Name = cAlphaArgs( 1 );
					FaultsThermostatOffset( jFaultyThermostat ).FaultyThermostatName = cAlphaArgs( 2 );

					// Availability schedule
					FaultsThermostatOffset( jFaultyThermostat ).AvaiSchedule = cAlphaArgs( 3 );
					if ( lAlphaFieldBlanks( 3 ) ) {
						FaultsThermostatOffset( jFaultyThermostat ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsThermostatOffset( jFaultyThermostat ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( FaultsThermostatOffset( jFaultyThermostat ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Severity schedule
					FaultsThermostatOffset( jFaultyThermostat ).SeveritySchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FaultsThermostatOffset( jFaultyThermostat ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsThermostatOffset( jFaultyThermostat ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FaultsThermostatOffset( jFaultyThermostat ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Reference offset value is required
					if ( lNumericFieldBlanks( 1 ) ) {
						ShowSevereError( cFault1 + " = \"" + cNumericFieldNames( 1 ) + "\" cannot be blank." );
						ErrorsFound = true;
					} else {
						FaultsThermostatOffset( jFaultyThermostat ).Offset = rNumericArgs( 1 );
					}

				} else if ( SameString( cFault1, "FaultModel:Fouling:Coil" ) ) { // For Fault_type 106: Fouling_Coil
					++jFoulingCoil;

					FouledCoils( jFoulingCoil ).FaultType = cFault1;
					FouledCoils( jFoulingCoil ).FaultTypeEnum = iFaultTypeEnums( i );
					FouledCoils( jFoulingCoil ).Name = cAlphaArgs( 1 );
					FouledCoils( jFoulingCoil ).FouledCoilName = cAlphaArgs( 2 );

					// Availability schedule
					FouledCoils( jFoulingCoil ).AvaiSchedule = cAlphaArgs( 3 );
					if ( lAlphaFieldBlanks( 3 ) ) {
						FouledCoils( jFoulingCoil ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( jFoulingCoil ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( FouledCoils( jFoulingCoil ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Severity schedule
					FouledCoils( jFoulingCoil ).SeveritySchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FouledCoils( jFoulingCoil ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( jFoulingCoil ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FouledCoils( jFoulingCoil ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 5 ) ) );
					if ( SELECT_CASE_var == "FOULEDUARATED" ) {
						FouledCoils( jFoulingCoil ).FoulingInputMethod = iFouledCoil_UARated;

					} else if ( SELECT_CASE_var == "FOULINGFACTOR" ) {
						FouledCoils( jFoulingCoil ).FoulingInputMethod = iFouledCoil_FoulingFactor;

					} else {
						FouledCoils( jFoulingCoil ).FoulingInputMethod = iFouledCoil_UARated;
					}}

					FouledCoils( jFoulingCoil ).UAFouled = rNumericArgs( 1 );
					FouledCoils( jFoulingCoil ).Rfw = rNumericArgs( 2 );
					FouledCoils( jFoulingCoil ).Rfa = rNumericArgs( 3 );
					FouledCoils( jFoulingCoil ).Aout = rNumericArgs( 4 );
					FouledCoils( jFoulingCoil ).Aratio = rNumericArgs( 5 );

				} else { // For Fault_type 101-105, which are of the same input structure
					++j;
					Faults( j ).FaultType = cFault1;
					Faults( j ).FaultTypeEnum = iFaultTypeEnums( i );

					Faults( j ).Name = cAlphaArgs( 1 );
					Faults( j ).AvaiSchedule = cAlphaArgs( 2 );
					// check availability schedule
					if ( lAlphaFieldBlanks( 2 ) ) {
						Faults( j ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						Faults( j ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( Faults( j ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + " = \"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					Faults( j ).SeveritySchedule = cAlphaArgs( 3 );
					// check severity schedule
					if ( lAlphaFieldBlanks( 3 ) ) {
						Faults( j ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						Faults( j ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( Faults( j ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					Faults( j ).ControllerType = cAlphaArgs( 4 );
					// check controller type
					if ( lAlphaFieldBlanks( 4 ) ) {
						ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" blank." );
						ErrorsFound = true;
					} else {
						{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 4 ) ) );
						if ( SELECT_CASE_var == "CONTROLLER:OUTDOORAIR" ) {
							Faults( j ).ControllerTypeEnum = iController_AirEconomizer;

							//CASE ...

						} else {
						}}
					}

					Faults( j ).ControllerName = cAlphaArgs( 5 );
					// check controller name
					if ( lAlphaFieldBlanks( 5 ) ) {
						ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" blank." );
						ErrorsFound = true;
					}

					// offset - degree of fault
					Faults( j ).Offset = rNumericArgs( 1 );
				}
			}
		}

		RunMeOnceFlag = true;

		if ( ErrorsFound ) {
			ShowFatalError( "Errors getting FaultModel input data.  Preceding condition(s) cause termination." );
		}

	}

	bool
	CheckFaultyAirFilterFanCurve(
		std::string const & FanName, // name of the fan
		int const FanCurvePtr      // pointer of the fan curve
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang
		//       DATE WRITTEN   Apr. 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To check whether the fan curve specified in the FaultModel:Fouling:AirFilter object
		// covers the rated operational point of the corresponding fan
		// Return true if the curve covers the fan rated operational point

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using namespace Fans;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FanMaxAirFlowRate; // Design Max Specified Volume Flow Rate of Fan [m3/sec]
		Real64 FanDeltaPress;     // Design Delta Pressure Across the Fan [Pa]
		Real64 FanDeltaPressCal;  // Calculated Delta Pressure Across the Fan [Pa]
		bool   FanFound;          // Whether the fan is found or not

		// FLOW

		FanFound = false;

		for ( int FanNum = 1; FanNum <= NumFans; ++FanNum ) {
			if ( SameString( Fan( FanNum ).FanName, FanName ) ) {
				FanMaxAirFlowRate = Fan( FanNum ).MaxAirFlowRate;
				FanDeltaPress = Fan( FanNum ).DeltaPress;
				FanFound = true;
				break;
			}
		}

		if ( !FanFound ) {
			return false;
		}

		FanDeltaPressCal = CurveValue( FanCurvePtr, FanMaxAirFlowRate );

		return ( ( FanDeltaPressCal > 0.95*FanDeltaPress ) && ( FanDeltaPressCal < 1.05*FanDeltaPress ) );
	}

} // FaultsManager

} // EnergyPlus
