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
	using DataGlobals::ScheduleAlwaysOn;
	using namespace InputProcessor;
	using General::CheckCreatedZoneItemName;

	// bool AnyHybridInModel( false ); // True if there are hybrid model in the model
	int NumOfHybridModelZones( 0 ); // Number of hybrid model zones in the model
	std::string CurrentModuleObject; // to assist in getting input

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data
	Array1D< HybridModelProperties > HybridModelZone;
	//Array1D< ZoneData > Zone;

	// Functions

	void
	CheckAndReadHybridModelZone()
	{

		using ScheduleManager::GetScheduleIndex;

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


		std::string CurrentModuleObject; // to assist in getting input

		int i; // Number of hybrid model objects
		int item1;
		int Loop;

		if ( RunMeOnceFlag ) return;

		// read hybrid model input
		CurrentModuleObject = "HybridModel:Zone";
		NumOfHybridModelZones = GetNumObjectsFound(CurrentModuleObject);
		HybridModelZone.allocate(NumOfHybridModelZones);
		
		if (NumOfHybridModelZones > 0) {
			
			
			Loop = 0;
			for (i = 1; i <= NumOfHybridModelZones; ++i) {

				GetObjectItem(CurrentModuleObject, i, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);

				int ZonePtr = FindItemInList(cAlphaArgs(2), Zone);
				// HybridModelZone( ZonePtr ).InternalThermalMassCalc = cAlphaArgs(3);
				// HybridModelZone( ZonePtr ).InfiltrationCalc = cAlphaArgs(4);
				// HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex(cAlphaArgs(5));
				// HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth = rNumericArgs(1);
				// HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate = rNumericArgs(2);
				// HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth = rNumericArgs(3);
				// HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate = rNumericArgs(4);

			}

			RunMeOnceFlag = true;

			if (ErrorsFound) {
				ShowFatalError("Errors getting Hybrid Model input data.  Preceding condition(s) cause termination.");
			}
		}

	}

} // HybridModel


//namespace ZoneCapacitanceMultiplierResearchSpecial {
//
//	// MODULE INFORMATION:
//	//       AUTHOR         Sang Hoon Lee and Tianzhen Hong, LBNL
//	//       DATE WRITTEN   September 2015
//	//       MODIFIED       
//	//       MODIFIED       
//	//       RE-ENGINEERED
//
//	// PURPOSE OF THIS MODULE:
//	// This module manages ZoneCapacitanceMultiplier:ResearchSpecial object.
//
//	// METHODOLOGY EMPLOYED:
//	//  The model uses measured zone air temperature to calculate internal thermal mass and infiltration air flow rate.
//
//	// USE STATEMENTS:
//
//	// Using/Aliasing
//	using namespace DataPrecisionGlobals;
//	using namespace DataGlobals;
//	using namespace DataHeatBalance;
//	using DataGlobals::ScheduleAlwaysOn;
//	using namespace InputProcessor;
//	using General::CheckCreatedZoneItemName;
//
//
//	bool AnyZoneCapacitanceMultiplierResearchSpecial(false); // True if there are hybrid model in the model
//	int NumZoneCapacitanceMultiplierResearchSpecial(0); // Number of hybrid model zones in the model
//	std::string CurrentModuleObject; // to assist in getting input
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//
//	// Object Data
//	Array1D< ZoneCapacitanceMultiplierResearchSpecialProperties > ZoneCapacitanceMultiplierResearchSpecialZone;
//	// Array1D< ZoneData > Zone;
//
//	// Functions
//
//	void
//		CheckAndReadZoneCapacitanceMultiplierResearchSpecial()
//	{
//
//		static bool RunMeOnceFlag(false);
//
//		static bool ErrorsFound(false); // If errors detected in input
//		int NumAlphas; // Number of Alphas for each GetobjectItem call
//		int NumNumbers; // Number of Numbers for each GetobjectItem call
//		int IOStatus;
//		Array1D_string cAlphaArgs(10); // Alpha input items for object
//		static Array1D_bool lAlphaFieldBlanks(10, false);
//		static Array1D_bool lNumericFieldBlanks(10, false);
//		Array1D_string cAlphaFieldNames(10);
//		Array1D_string cNumericFieldNames(10);
//		Array1D< Real64 > rNumericArgs(10); // Numeric input items for object
//
//		std::string CurrentModuleObject; // to assist in getting input
//
//		int i; // Number of hybrid model objects
//
//		if (RunMeOnceFlag) return;
//
//		// read hybrid model input
//		CurrentModuleObject = "ZoneCapacitanceMultiplier:ResearchSpecial";
//		NumZoneCapacitanceMultiplierResearchSpecial = GetNumObjectsFound(CurrentModuleObject);
//
//		if (NumZoneCapacitanceMultiplierResearchSpecial > 0) {
//
//			ZoneCapacitanceMultiplierResearchSpecialZone.allocate(NumZoneCapacitanceMultiplierResearchSpecial);
//
//			for (i = 1; i <= NumZoneCapacitanceMultiplierResearchSpecial; ++i) {
//
//				GetObjectItem(CurrentModuleObject, i, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);
//
//				ZoneCapacitanceMultiplierResearchSpecialZone(i).ZonePtr = FindItemInList(cAlphaArgs(1), Zone);
//				ZoneCapacitanceMultiplierResearchSpecialZone(i).ZoneVolCapMultpSens = rNumericArgs(1);
//				ZoneCapacitanceMultiplierResearchSpecialZone(i).ZoneVolCapMultpMoist = rNumericArgs(2);
//				ZoneCapacitanceMultiplierResearchSpecialZone(i).ZoneVolCapMultpCO2 = rNumericArgs(3);
//				ZoneCapacitanceMultiplierResearchSpecialZone(i).ZoneVolCapMultpGenContam = rNumericArgs(4);
//
//			}
//
//			RunMeOnceFlag = true;
//
//			if (ErrorsFound) {
//				ShowFatalError("Errors getting ZoneCapacitanceMultiplierResearchSpecial input data.  Preceding condition(s) cause termination.");
//			}
//		}
//
//	}
//
//} // ZoneCapacitanceMultiplierResearchSpecial
//

} // EnergyPlus
