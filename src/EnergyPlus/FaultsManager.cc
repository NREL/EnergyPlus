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
	//       MODIFIED       Sep. 2013, Xiufeng Pang (XP), added fouling coil fault
	//                      Feb. 2015, Rongpeng Zhang, added thermostat/humidistat offset faults
	//                      Apr. 2015, Rongpeng Zhang, added fouling air filter fault
	//                      May. 2016, Rongpeng Zhang, added Chiller/Condenser Supply Water Temperature Sensor fault 
	//                      Jun. 2016, Rongpeng Zhang, added tower scaling fault
	//                      Jul. 2016, Rongpeng Zhang, added Coil Supply Air Temperature Sensor fault
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
	int const NumFaultTypes( 13 );

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
	int const iFault_TemperatureSensorOffset_ChillerSupplyWater( 110 );
	int const iFault_TemperatureSensorOffset_CondenserSupplyWater( 111 );
	int const iFault_TemperatureSensorOffset_CoilSupplyAir( 112 );
	int const iFault_Fouling_Tower( 113 );

	// Types of faults under Group Operational Faults in IDD
	//  1. Temperature sensor offset (FY14)
	//  2. Humidity sensor offset (FY14)
	//  3. Enthalpy sensor offset (FY14)
	//  4. Fouling coils (FY14)
	//  5. Thermostat offset (FY15)
	//  6. Humidistat offset (FY15)
	//  7. Fouling air filter (FY15)
	//  8. Chiller Supply Water Temperature Sensor Offset (FY16)
	//  9. Condenser Supply Water Temperature Sensor Offset (FY16)
	//  10. Cooling Tower Scaling (FY16)
	//  11. Coil Supply Air Temperature Sensor Offset (FY16)
	// coming ...
	//  Fouling: chillers, boilers, cooling towers
	//  Damper leakage: return air, outdoor air
	//  Blockage: pipe
	//  Meter: air flow, water flow
	//  CO2 sensor
	//  Pressure sensor offset
	//  more

	Array1D_string const cFaults( 
	NumFaultTypes, 
	{ "FaultModel:TemperatureSensorOffset:OutdoorAir", 
	"FaultModel:HumiditySensorOffset:OutdoorAir", 
	"FaultModel:EnthalpySensorOffset:OutdoorAir", 
	"FaultModel:TemperatureSensorOffset:ReturnAir", 
	"FaultModel:EnthalpySensorOffset:ReturnAir", 
	"FaultModel:Fouling:Coil", 
	"FaultModel:ThermostatOffset", 
	"FaultModel:HumidistatOffset", 
	"FaultModel:Fouling:AirFilter",
	"FaultModel:TemperatureSensorOffset:ChillerSupplyWater",
	"FaultModel:TemperatureSensorOffset:CondenserSupplyWater",
	"FaultModel:TemperatureSensorOffset:CoilSupplyAir",
	"FaultModel:Fouling:tower"
	} );
	//      'FaultModel:PressureSensorOffset:OutdoorAir   ', &
	//      'FaultModel:TemperatureSensorOffset:SupplyAir ', &
	//      'FaultModel:TemperatureSensorOffset:ZoneAir   ', &
	//      'FaultModel:Blockage:Branch                   ', &
	//      'FaultModel:Fouling:Chiller                   ', &
	//      'FaultModel:Fouling:Boiler                    ', &
	//      'FaultModel:DamperLeakage:ReturnAir           ', &
	//      'FaultModel:DamperLeakage:OutdoorAir          ' /)

	Array1D_int const iFaultTypeEnums(
	NumFaultTypes, 
	{ iFault_TemperatureSensorOffset_OutdoorAir, 
	iFault_HumiditySensorOffset_OutdoorAir, 
	iFault_EnthalpySensorOffset_OutdoorAir, 
	iFault_TemperatureSensorOffset_ReturnAir, 
	iFault_EnthalpySensorOffset_ReturnAir, 
	iFault_Fouling_Coil, 
	iFault_ThermostatOffset, 
	iFault_HumidistatOffset, 
	iFault_Fouling_AirFilter,
	iFault_TemperatureSensorOffset_ChillerSupplyWater,
	iFault_TemperatureSensorOffset_CondenserSupplyWater,
	iFault_TemperatureSensorOffset_CoilSupplyAir,
	iFault_Fouling_Tower
	});

	bool AnyFaultsInModel( false ); // True if there are operationla faults in the model
	int NumFaults( 0 ); // Number of faults (include multiple faults of same type) in the model
	int NumFouledCoil( 0 ); // Total number of fouled coils
	int NumFaultyThermostat( 0 ); // Total number of faulty thermostat with offset
	int NumFaultyHumidistat( 0 ); // Total number of faulty humidistat with offset
	int NumFaultyAirFilter( 0 ); // Total number of fouled air filters
	int NumFaultyChillerSWTSensor( 0 );  // Total number of faulty Chillers Supply Water Temperature Sensor
	int NumFaultyCondenserSWTSensor( 0 );  // Total number of faulty Condenser Supply Water Temperature Sensor
	int NumFaultyTowerScaling( 0 );  // Total number of faulty Towers with Scaling
	int NumFaultyCoilSATSensor( 0 );  // Total number of faulty Coil Supply Air Temperature Sensor
	
	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< FaultProperties > Faults;
	Array1D< FaultProperties > FouledCoils;
	Array1D< FaultProperties > FaultsThermostatOffset;
	Array1D< FaultProperties > FaultsHumidistatOffset;
	Array1D< FaultProperties > FaultsFouledAirFilters;
	Array1D< FaultProperties > FaultsChillerSWTSensor;
	Array1D< FaultProperties > FaultsCondenserSWTSensor;
	Array1D< FaultProperties > FaultsTowerScaling;
	Array1D< FaultProperties > FaultsCoilSATSensor;

	// Functions

	void
	CheckAndReadFaults()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tianzhen Hong
		//       DATE WRITTEN   August 2013
		//       MODIFIED       Sep. 2013, Xiufeng Pang (XP), added fouling coil fault
		//                      Feb. 2015, Rongpeng Zhang, added thermostat/humidistat offset faults
		//                      Apr. 2015, Rongpeng Zhang, added fouling air filter fault
		//                      May. 2016, Rongpeng Zhang, added Chiller/Condenser Supply Water Temperature Sensor fault 
		//                      Jun. 2016, Rongpeng Zhang, added tower scaling fault
		//                      Jul. 2016, Rongpeng Zhang, added Coil Supply Air Temperature Sensor fault
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

		int i; // Index to fault type
		int iFaults;
		int jj; // Index to fault object
		int j = 0; //Number of fault objects of type 101-105
		int jFault_FoulingCoil = 0; //Index to fault objects of type 106: fouling coil
		int jFault_Thermostat = 0; //Index to fault objects of type 107: faulty thermostat
		int jFault_Humidistat = 0; //Index to fault objects of type 108: faulty humidistat
		int jFault_AirFilter = 0; //Index to fault objects of type 109: fouled air filter
		int jFault_TemperatureSensorOffset_ChillerSupplyWater = 0; //Index to fault objects of type 110: chiller SWT sensor offset
		int jFault_TemperatureSensorOffset_CondenserSupplyWater = 0; //Index to fault objects of type 111: condenser SWT sensor offset
		int jFault_TemperatureSensorOffset_CoilSupplyAir = 0; //Index to fault objects of type 112: coil SAT sensor offset
		int jFault_Fouling_Tower = 0; //Index to fault objects of type 113: tower scaling
		std::string cFault_str;

		if ( RunMeOnceFlag ) return;

		// check number of faults
		NumFaults = 0;
		for ( i = 1; i <= NumFaultTypes; ++i ) {
			iFaults = GetNumObjectsFound( cFaults( i ) );
			NumFaults += iFaults;
		}

		// 6th fault: Coil fouling 
		NumFouledCoil = GetNumObjectsFound( cFaults( 6 ) );
		// 7th fault: Faulty thermostat
		NumFaultyThermostat = GetNumObjectsFound( cFaults( 7 ) );
		// 8th fault: Faulty humidistat 
		NumFaultyHumidistat = GetNumObjectsFound( cFaults( 8 ) );
		// 9th fault: Fouled air filter
		NumFaultyAirFilter = GetNumObjectsFound( cFaults( 9 ) );
		// 10th fault: Faulty Chillers Supply Water Temperature Sensor 
		NumFaultyChillerSWTSensor = GetNumObjectsFound( cFaults( 10 ) );
		// 11th fault: Faulty Condenser Supply Water Temperature Sensor
		NumFaultyCondenserSWTSensor = GetNumObjectsFound( cFaults( 11 ) ); 
		// 12th fault: Faulty Towers with Scaling
		NumFaultyTowerScaling = GetNumObjectsFound( cFaults( 12 ) );
		// 13th fault: Faulty Coil Supply Air Temperature Sensor
		NumFaultyCoilSATSensor = GetNumObjectsFound( cFaults( 13 ) );  
	
	
		if ( NumFaults > 0 ) {
			AnyFaultsInModel = true;
		} else {
			AnyFaultsInModel = false;
		}

		if ( ! AnyFaultsInModel ) {
			RunMeOnceFlag = true;
			return;
		}

		// allocate fault array
		Faults.allocate( NumFaults );
		FouledCoils.allocate( NumFouledCoil );
		FaultsThermostatOffset.allocate( NumFaultyThermostat );
		FaultsHumidistatOffset.allocate( NumFaultyHumidistat );
		FaultsFouledAirFilters.allocate( NumFaultyAirFilter );
		FaultsChillerSWTSensor.allocate( NumFaultyChillerSWTSensor );
		FaultsCondenserSWTSensor.allocate( NumFaultyCondenserSWTSensor );
		FaultsTowerScaling.allocate( NumFaultyTowerScaling );
		FaultsCoilSATSensor.allocate( NumFaultyCoilSATSensor );

		// read faults input
		for ( i = 1; i <= NumFaultTypes; ++i ) {
			cFault_str = cFaults( i ); // fault object string
			iFaults = GetNumObjectsFound( cFault_str );

			for ( jj = 1; jj <= iFaults; ++jj ) {
				GetObjectItem( cFault_str, jj, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( SameString( cFault_str, "FaultModel:Fouling:AirFilter" ) ) { // For Fault_type 109: Fouled Air Filters
					++jFault_AirFilter;

					FaultsFouledAirFilters( jFault_AirFilter ).FaultType = cFault_str;
					FaultsFouledAirFilters( jFault_AirFilter ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsFouledAirFilters( jFault_AirFilter ).Name = cAlphaArgs( 1 );

					// Informatin of the fan associated with the fouling air filter
					FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanType = cAlphaArgs( 2 );
					FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanName = cAlphaArgs( 3 );

					// Check whether the specified fan exsits in the fan list
					if ( FindItemInList( cAlphaArgs( 3 ), Fans::Fan, &Fans::FanEquipConditions::FanName ) != 1 ) {
						ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
						ErrorsFound = true;
					}

					// Assign fault index to the fan object
					for ( int FanNum = 1; FanNum <= Fans::NumFans; ++FanNum ) {
						if ( SameString( Fans::Fan( FanNum ).FanName, cAlphaArgs( 3 ) ) ) {
							Fans::Fan( FanNum ).FaultyFilterFlag = true;
							Fans::Fan( FanNum ).FaultyFilterIndex = jFault_AirFilter;
							break;
						}
					}

					// Fault availability schedule
					FaultsFouledAirFilters( jFault_AirFilter ).AvaiSchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FaultsFouledAirFilters( jFault_AirFilter ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsFouledAirFilters( jFault_AirFilter ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FaultsFouledAirFilters( jFault_AirFilter ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Fan pressure increase fraction schedule
					FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterPressFracSche = cAlphaArgs( 5 );
					if ( lAlphaFieldBlanks( 5 ) ) {
						FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterPressFracSchePtr = -1; // returns schedule value of 1
					} else {
						FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterPressFracSchePtr = GetScheduleIndex( cAlphaArgs( 5 ) );
						if ( FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterPressFracSchePtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Fan curve describing the relationship between fan pressure rise and air flow rate
					FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanCurve = cAlphaArgs( 6 );
					FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanCurvePtr = GetCurveIndex( cAlphaArgs( 6 ) );
					if ( FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanCurvePtr == 0 ) {
						ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 )  + "\"" );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = \"" + cAlphaArgs( 6 ) + "\" not found."  );
						ErrorsFound = true;
					}

					// Check whether the specified fan curve covers the design operational point of the fan
					if ( !CheckFaultyAirFilterFanCurve( FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanName, FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanCurvePtr ) ) {
						ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 )  + "\"" );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = \"" + cAlphaArgs( 6 ) + "\" does not cover "  );
						ShowContinueError( "the operational point of Fan " + FaultsFouledAirFilters( jFault_AirFilter ).FaultyAirFilterFanName  );
						ErrorsFound = true;
					}

					//In the fan object, calculate by each time-step: 1) pressure inc value; 2) air flow rate decrease value ......

				} else if ( SameString( cFault_str, "FaultModel:HumidistatOffset" ) ) { // For Fault_type 108: HumidistatOffset
					++jFault_Humidistat;

					FaultsHumidistatOffset( jFault_Humidistat ).FaultType = cFault_str;
					FaultsHumidistatOffset( jFault_Humidistat ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsHumidistatOffset( jFault_Humidistat ).Name = cAlphaArgs( 1 );
					FaultsHumidistatOffset( jFault_Humidistat ).FaultyHumidistatName = cAlphaArgs( 2 );
					FaultsHumidistatOffset( jFault_Humidistat ).FaultyHumidistatType = cAlphaArgs( 3 );

					if ( SameString( FaultsHumidistatOffset( jFault_Humidistat ).FaultyHumidistatType, "ThermostatOffsetDependent" ) ) {
					// For Humidistat Offset Type: ThermostatOffsetDependent

						// Related Thermostat Offset Fault Name is required for Humidistat Offset Type: ThermostatOffsetDependent
						if ( lAlphaFieldBlanks( 6 ) ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\": " + cAlphaFieldNames( 6 ) + " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetDependent\"." );
							ErrorsFound = true;
						} else {
							FaultsHumidistatOffset( jFault_Humidistat ).FaultyThermostatName = cAlphaArgs( 6 );
						}

					} else {
					// For Humidistat Offset Type: ThermostatOffsetIndependent

						// Availability schedule
						FaultsHumidistatOffset( jFault_Humidistat ).AvaiSchedule = cAlphaArgs( 4 );
						if ( lAlphaFieldBlanks( 4 ) ) {
							FaultsHumidistatOffset( jFault_Humidistat ).AvaiSchedPtr = -1; // returns schedule value of 1
						} else {
							FaultsHumidistatOffset( jFault_Humidistat ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
							if ( FaultsHumidistatOffset( jFault_Humidistat ).AvaiSchedPtr == 0 ) {
								ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
								ErrorsFound = true;
							}
						}

						// Severity schedule
						FaultsHumidistatOffset( jFault_Humidistat ).SeveritySchedule = cAlphaArgs( 5 );
						if ( lAlphaFieldBlanks( 5 ) ) {
							FaultsHumidistatOffset( jFault_Humidistat ).SeveritySchedPtr = -1; // returns schedule value of 1
						} else {
							FaultsHumidistatOffset( jFault_Humidistat ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 5 ) );
							if ( FaultsHumidistatOffset( jFault_Humidistat ).SeveritySchedPtr == 0 ) {
								ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" not found." );
								ErrorsFound = true;
							}
						}

						// Reference offset value is required for Humidistat Offset Type: ThermostatOffsetIndependent
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\": " + cNumericFieldNames( 1 ) + " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetIndependent\"." );
							ErrorsFound = true;
						} else {
							FaultsHumidistatOffset( jFault_Humidistat ).Offset = rNumericArgs( 1 );
						}

					}


				} else if ( SameString( cFault_str, "FaultModel:ThermostatOffset" ) ) { // For Fault_type 107: ThermostatOffset
					++jFault_Thermostat;

					FaultsThermostatOffset( jFault_Thermostat ).FaultType = cFault_str;
					FaultsThermostatOffset( jFault_Thermostat ).FaultTypeEnum = iFaultTypeEnums( i );
					FaultsThermostatOffset( jFault_Thermostat ).Name = cAlphaArgs( 1 );
					FaultsThermostatOffset( jFault_Thermostat ).FaultyThermostatName = cAlphaArgs( 2 );

					// Availability schedule
					FaultsThermostatOffset( jFault_Thermostat ).AvaiSchedule = cAlphaArgs( 3 );
					if ( lAlphaFieldBlanks( 3 ) ) {
						FaultsThermostatOffset( jFault_Thermostat ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsThermostatOffset( jFault_Thermostat ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( FaultsThermostatOffset( jFault_Thermostat ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Severity schedule
					FaultsThermostatOffset( jFault_Thermostat ).SeveritySchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FaultsThermostatOffset( jFault_Thermostat ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						FaultsThermostatOffset( jFault_Thermostat ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FaultsThermostatOffset( jFault_Thermostat ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Reference offset value is required
					if ( lNumericFieldBlanks( 1 ) ) {
						ShowSevereError( cFault_str + " = \"" + cNumericFieldNames( 1 ) + "\" cannot be blank." );
						ErrorsFound = true;
					} else {
						FaultsThermostatOffset( jFault_Thermostat ).Offset = rNumericArgs( 1 );
					}

				} else if ( SameString( cFault_str, "FaultModel:Fouling:Coil" ) ) { // For Fault_type 106: Fouling_Coil
					++jFault_FoulingCoil;

					FouledCoils( jFault_FoulingCoil ).FaultType = cFault_str;
					FouledCoils( jFault_FoulingCoil ).FaultTypeEnum = iFaultTypeEnums( i );
					FouledCoils( jFault_FoulingCoil ).Name = cAlphaArgs( 1 );
					FouledCoils( jFault_FoulingCoil ).FouledCoilName = cAlphaArgs( 2 );

					// Availability schedule
					FouledCoils( jFault_FoulingCoil ).AvaiSchedule = cAlphaArgs( 3 );
					if ( lAlphaFieldBlanks( 3 ) ) {
						FouledCoils( jFault_FoulingCoil ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( jFault_FoulingCoil ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( FouledCoils( jFault_FoulingCoil ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Severity schedule
					FouledCoils( jFault_FoulingCoil ).SeveritySchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FouledCoils( jFault_FoulingCoil ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( jFault_FoulingCoil ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FouledCoils( jFault_FoulingCoil ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 5 ) ) );
					if ( SELECT_CASE_var == "FOULEDUARATED" ) {
						FouledCoils( jFault_FoulingCoil ).FoulingInputMethod = iFouledCoil_UARated;

					} else if ( SELECT_CASE_var == "FOULINGFACTOR" ) {
						FouledCoils( jFault_FoulingCoil ).FoulingInputMethod = iFouledCoil_FoulingFactor;

					} else {
						FouledCoils( jFault_FoulingCoil ).FoulingInputMethod = iFouledCoil_UARated;
					}}

					FouledCoils( jFault_FoulingCoil ).UAFouled = rNumericArgs( 1 );
					FouledCoils( jFault_FoulingCoil ).Rfw = rNumericArgs( 2 );
					FouledCoils( jFault_FoulingCoil ).Rfa = rNumericArgs( 3 );
					FouledCoils( jFault_FoulingCoil ).Aout = rNumericArgs( 4 );
					FouledCoils( jFault_FoulingCoil ).Aratio = rNumericArgs( 5 );

				} else { // For Fault_type 101-105, which are of the same input structure
					++j;
					Faults( j ).FaultType = cFault_str;
					Faults( j ).FaultTypeEnum = iFaultTypeEnums( i );

					Faults( j ).Name = cAlphaArgs( 1 );
					Faults( j ).AvaiSchedule = cAlphaArgs( 2 );
					// check availability schedule
					if ( lAlphaFieldBlanks( 2 ) ) {
						Faults( j ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						Faults( j ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( Faults( j ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + " = \"" + cAlphaArgs( 2 ) + "\" not found." );
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
							ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					Faults( j ).ControllerType = cAlphaArgs( 4 );
					// check controller type
					if ( lAlphaFieldBlanks( 4 ) ) {
						ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" blank." );
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
						ShowSevereError( cFault_str + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + " = \"" + cAlphaArgs( 5 ) + "\" blank." );
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
