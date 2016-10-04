// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <HybridModel.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HybridModel {

	// MODULE INFORMATION:
	//       AUTHOR         Sang Hoon Lee and Tianzhen Hong, LBNL
	//       DATE WRITTEN   May 2015
	//       MODIFIED       
	//       MODIFIED       
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// This module manages hybrid model.

	// METHODOLOGY EMPLOYED:
	//  The model uses measured zone air temperature to calculate internal thermal mass and infiltration air flow rate.

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHeatBalance;
	using namespace InputProcessor;
	using DataGlobals::ScheduleAlwaysOn;
	using General::CheckCreatedZoneItemName;

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
		std::string CurrentModuleObject; // to assist in getting input
		Array1D_string cAlphaArgs( 10 ); // Alpha input items for object
		Array1D_string cAlphaFieldNames( 10 );
		Array1D_string cNumericFieldNames( 10 );
		Array1D< Real64 > rNumericArgs( 10 ); // Numeric input items for object

		if ( RunMeOnceFlag ) return;

		// read hybrid model input
		CurrentModuleObject = "HybridModel:Zone";
		NumOfHybridModelZones = GetNumObjectsFound( CurrentModuleObject );
		HybridModelZone.allocate( NumOfZones );

		if (NumOfHybridModelZones > 0) {
		
			for ( int HybridModelNum = 1; HybridModelNum <= NumOfHybridModelZones; ++HybridModelNum ) {

				GetObjectItem(CurrentModuleObject, HybridModelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);

				ZonePtr = FindItemInList( cAlphaArgs( 2 ), Zone );
				HybridModelZone( ZonePtr ).InternalThermalMassCalc = SameString( cAlphaArgs( 3 ), "YES" );
				HybridModelZone( ZonePtr ).InfiltrationCalc = SameString( cAlphaArgs( 4 ), "YES" );
				HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex( cAlphaArgs( 5 ));
				HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth = rNumericArgs( 1 );
				HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate = rNumericArgs( 2 );
				HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth = rNumericArgs( 3 );
				HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate = rNumericArgs( 4 );
			}

			RunMeOnceFlag = true;

			if (ErrorsFound) {
				ShowFatalError( "Errors getting Hybrid Model input data. Preceding condition(s) cause termination." );
			}
		}

	}

}

} // EnergyPlus
