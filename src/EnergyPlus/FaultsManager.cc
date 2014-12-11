// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>

// EnergyPlus Headers
#include <FaultsManager.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace FaultsManager {

	// MODULE INFORMATION:
	//       AUTHOR         Tianzhen Hong, LBNL
	//       DATE WRITTEN   August 2013
	//       MODIFIED       Sep. 2013 Xiufeng Pang (XP) added fouling coil fault
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
	int const NumFaultTypes( 6 );

	// FaultTypeEnum
	int const iFault_TemperatureSensorOffset_OutdoorAir( 101 );
	int const iFault_HumiditySensorOffset_OutdoorAir( 102 );
	int const iFault_EnthalpySensorOffset_OutdoorAir( 103 );
	int const iFault_TemperatureSensorOffset_ReturnAir( 104 );
	int const iFault_EnthalpySensorOffset_ReturnAir( 105 );
	int const iFault_Fouling_Coil( 106 );
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
	FArray1D_string const cFaults( NumFaultTypes, { "FaultModel:TemperatureSensorOffset:OutdoorAir", "FaultModel:HumiditySensorOffset:OutdoorAir", "FaultModel:EnthalpySensorOffset:OutdoorAir", "FaultModel:TemperatureSensorOffset:ReturnAir", "FaultModel:EnthalpySensorOffset:ReturnAir", "FaultModel:Fouling:Coil" } );
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

	FArray1D_int const iFaultTypeEnums( NumFaultTypes, { iFault_TemperatureSensorOffset_OutdoorAir, iFault_HumiditySensorOffset_OutdoorAir, iFault_EnthalpySensorOffset_OutdoorAir, iFault_TemperatureSensorOffset_ReturnAir, iFault_EnthalpySensorOffset_ReturnAir, iFault_Fouling_Coil } );

	bool AnyFaultsInModel( false ); // True if there are operationla faults in the model
	int NumFaults( 0 ); // Number of faults (include multiple faults of same type) in the model
	int NumFouledCoil( 0 ); // Total number of fouled coils

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	FArray1D< FaultProperties > Faults;
	FArray1D< FaultProperties > FouledCoils;

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
		FArray1D_string cAlphaArgs( 5 ); // Alpha input items for object
		static FArray1D_bool lAlphaFieldBlanks( 5, false );
		static FArray1D_bool lNumericFieldBlanks( 5, false );
		FArray1D_string cAlphaFieldNames( 5 );
		FArray1D_string cNumericFieldNames( 5 );
		FArray1D< Real64 > rNumericArgs( 5 ); // Numeric input items for object

		int i;
		int j;
		int j1;
		int jj;
		int iFaults;
		int iTotalFaults;
		std::string cFault1;

		if ( RunMeOnceFlag ) return;

		// check number of faults
		NumFaults = 0;
		for ( i = 1; i <= NumFaultTypes; ++i ) {
			iFaults = 0;
			iFaults = GetNumObjectsFound( cFaults( i ) );
			NumFaults += iFaults;
		}

		// Coil fouling is the 6th fault
		NumFouledCoil = GetNumObjectsFound( cFaults( 6 ) );

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
		j = 0;
		j1 = 0;
		for ( i = 1; i <= NumFaultTypes; ++i ) {
			cFault1 = cFaults( i );
			iFaults = GetNumObjectsFound( cFault1 );
			for ( jj = 1; jj <= iFaults; ++jj ) {
				GetObjectItem( cFault1, jj, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( SameString( cFault1, "FaultModel:Fouling:Coil" ) ) {
					++j1;

					FouledCoils( j1 ).FaultType = cFault1;
					FouledCoils( j1 ).FaultTypeEnum = iFaultTypeEnums( i );
					FouledCoils( j1 ).Name = cAlphaArgs( 1 );
					FouledCoils( j1 ).FouledCoilName = cAlphaArgs( 2 );

					// Availability schedule
					FouledCoils( j1 ).AvaiSchedule = cAlphaArgs( 3 );
					if ( lAlphaFieldBlanks( 3 ) ) {
						FouledCoils( j1 ).AvaiSchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( j1 ).AvaiSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( FouledCoils( j1 ).AvaiSchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + " = \"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					// Severity schedule
					FouledCoils( j1 ).SeveritySchedule = cAlphaArgs( 4 );
					if ( lAlphaFieldBlanks( 4 ) ) {
						FouledCoils( j1 ).SeveritySchedPtr = -1; // returns schedule value of 1
					} else {
						FouledCoils( j1 ).SeveritySchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( FouledCoils( j1 ).SeveritySchedPtr == 0 ) {
							ShowSevereError( cFault1 + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + " = \"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 5 ) ) );
					if ( SELECT_CASE_var == "FOULEDUARATED" ) {
						FouledCoils( j1 ).FoulingInputMethod = iFouledCoil_UARated;

					} else if ( SELECT_CASE_var == "FOULINGFACTOR" ) {
						FouledCoils( j1 ).FoulingInputMethod = iFouledCoil_FoulingFactor;

					} else {
						FouledCoils( j1 ).FoulingInputMethod = iFouledCoil_UARated;
					}}

					FouledCoils( j1 ).UAFouled = rNumericArgs( 1 );
					FouledCoils( j1 ).Rfw = rNumericArgs( 2 );
					FouledCoils( j1 ).Rfa = rNumericArgs( 3 );
					FouledCoils( j1 ).Aout = rNumericArgs( 4 );
					FouledCoils( j1 ).Aratio = rNumericArgs( 5 );

				} else { // other faults
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
