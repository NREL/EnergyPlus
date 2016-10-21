// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <HybridModel.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HybridModel {

	// MODULE INFORMATION:
	//       AUTHOR         Sang Hoon Lee, Tianzhen Hong, Rongpeng Zhang. LBNL 
	//       DATE WRITTEN   Oct 2015
	//       MODIFIED       
	//       MODIFIED       
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// This module manages hybrid model.

	// METHODOLOGY EMPLOYED:
	//  The model uses measured zone air temperature to calculate internal thermal mass and infiltration air flow rate.

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataHeatBalance;
	using namespace DataPrecisionGlobals;
	using namespace DataRoomAirModel;
	using namespace InputProcessor;
	using DataGlobals::ScheduleAlwaysOn;
	using General::CheckCreatedZoneItemName;
	
	bool FlagHybridModel( false ); // True if hybrid model is activated
	bool FlagHybridModelInf( false ); // True if hybrid model (infiltration) is activated
	int NumOfHybridModelZones( 0 ); // Number of hybrid model zones in the model
	std::string CurrentModuleObject; // to assist in getting input

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data
	Array1D< HybridModelProperties > HybridModelZone;

	// Functions

	void
	CheckAndReadHybridModelZone()
	{

		using ScheduleManager::GetScheduleIndex;

		static bool RunMeOnceFlag( false );
		static bool ErrorsFound( false ); // If errors detected in input
		static Array1D_bool lAlphaFieldBlanks( 10, false );
		static Array1D_bool lNumericFieldBlanks( 10, false );
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int IOStatus;
		int ZonePtr; // Pointer to the zone
		int ZoneListPtr; // Pointer to the zone list
		std::string CurrentModuleObject; // to assist in getting input
		Array1D_string cAlphaArgs( 10 ); // Alpha input items for object
		Array1D_string cAlphaFieldNames( 10 );
		Array1D_string cNumericFieldNames( 10 );
		Array1D< Real64 > rNumericArgs( 10 ); // Numeric input items for object

		if ( RunMeOnceFlag ) return;

		// Read hybrid model input
		CurrentModuleObject = "HybridModel:Zone";
		NumOfHybridModelZones = GetNumObjectsFound( CurrentModuleObject );
		HybridModelZone.allocate( NumOfZones );

		if (NumOfHybridModelZones > 0) {

			RunMeOnceFlag = true;
		
			for ( int HybridModelNum = 1; HybridModelNum <= NumOfHybridModelZones; ++HybridModelNum ) {

				GetObjectItem(CurrentModuleObject, HybridModelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);
				
				ZoneListPtr = 0;
				ZonePtr = FindItemInList( cAlphaArgs( 2 ), Zone );
				if ( ZonePtr == 0 && NumOfZoneLists > 0 ) ZoneListPtr = FindItemInList( cAlphaArgs( 2 ), ZoneList );
				if ( ZonePtr > 0 ) {
					HybridModelZone( ZonePtr ).Name = cAlphaArgs( 1 );
					HybridModelZone( ZonePtr ).InternalThermalMassCalc = SameString( cAlphaArgs( 3 ), "YES" );
					HybridModelZone( ZonePtr ).InfiltrationCalc = SameString( cAlphaArgs( 4 ), "YES" );
					HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex( cAlphaArgs( 5 ));
					HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth = rNumericArgs( 1 );
					HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate = rNumericArgs( 2 );
					HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth = rNumericArgs( 3 );
					HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate = rNumericArgs( 4 );
				} else if ( ZoneListPtr > 0 ) {
					for( int ZonePtrNum = 1; ZonePtrNum < ZoneList( ZoneListPtr ).NumOfZones; ZonePtrNum++ ){
						HybridModelZone( ZonePtr ).Name = cAlphaArgs( 1 );
						ZonePtr = ZoneList( ZoneListPtr ).Zone( ZonePtrNum );
						HybridModelZone( ZonePtr ).InternalThermalMassCalc = SameString( cAlphaArgs( 3 ), "YES" );
						HybridModelZone( ZonePtr ).InfiltrationCalc = SameString( cAlphaArgs( 4 ), "YES" );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex( cAlphaArgs( 5 ));
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth = rNumericArgs( 1 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate = rNumericArgs( 2 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth = rNumericArgs( 3 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate = rNumericArgs( 4 );
					}
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			//Post-processing of Hybrid Model settings 
			for( ZonePtr = 1; ZonePtr <= NumOfZones; ZonePtr++ ){

				// Zone Air Infiltration Rate and Zone Internal Thermal Mass calculations cannot be performed simultaneously
				if( HybridModelZone( ZonePtr ).InternalThermalMassCalc && HybridModelZone( ZonePtr ).InfiltrationCalc ){
					HybridModelZone( ZonePtr ).InfiltrationCalc = false;
					ShowWarningError( CurrentModuleObject + "=\"" + HybridModelZone( ZonePtr ).Name + "\" invalid " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 4 ) + "." );
					ShowContinueError( "Field " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 4 ) + "\" cannot be both set to YES." );
					ShowContinueError( "Field " + cAlphaFieldNames( 4 ) + "\" is changed to NO for the hybrid modeling simulations." );
				}

				// Flags showing Hybrid Modeling settings
				if( HybridModelZone( ZonePtr ).InternalThermalMassCalc || HybridModelZone( ZonePtr ).InfiltrationCalc ){
					FlagHybridModel = true;
					if( HybridModelZone( ZonePtr ).InfiltrationCalc ) FlagHybridModelInf = true;
				}
			}

			//ZoneAirMassFlowConservation should not be activated during the Hybrid Modeling infiltration calculations
			if( FlagHybridModelInf && ZoneAirMassFlow.EnforceZoneMassBalance){
				ZoneAirMassFlow.EnforceZoneMassBalance = false;
				ShowWarningError( "ZoneAirMassFlowConservation is deactivated when Hybrid Modeling is performed." );
			}

			//RoomAirModelType should be Mixing if Hybrid Modeling is performed for the zone
			if( FlagHybridModel ){
				for ( ZonePtr = 1; ZonePtr <= NumOfZones; ZonePtr++ ) {
					if(( HybridModelZone( ZonePtr ).InternalThermalMassCalc || HybridModelZone( ZonePtr ).InfiltrationCalc ) && ( AirModel( ZonePtr ).AirModelType != RoomAirModel_Mixing )){
						AirModel( ZonePtr ).AirModelType = RoomAirModel_Mixing;
						ShowWarningError( "Room Air Model Type should be Mixing if Hybrid Modeling is performed for the zone." );
					}
				}
			}

			if (ErrorsFound) {
				ShowFatalError( "Errors getting Hybrid Model input data. Preceding condition(s) cause termination." );
			}
		}
	}
}

} // EnergyPlus
