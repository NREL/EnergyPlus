#ifndef HVACSizingSimulationManager_hh_INCLUDED
#define HVACSizingSimulationManager_hh_INCLUDED

// C++ Headers
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <SizingAnalysisObjects.hh>
#include <DataGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <WeatherManager.hh>
#include <DataEnvironment.hh>
#include <SQLiteProcedures.hh>
#include <DataReportingFlags.hh>
#include <DataErrorTracking.hh>
#include <EMSManager.hh>
#include <HeatBalanceManager.hh>
#include <General.hh>
#include <PlantPipingSystemsManager.hh>
#include <ExteriorEnergyUse.hh>
#include <DataSizing.hh>
#include <DataPlant.hh>
#include <FluidProperties.hh>
#include <PlantManager.hh>
#include <SimulationManager.hh>

namespace EnergyPlus {

	class HVACSizingSimulationManager {
	public:
		static std::string objectName() { return "Advanced Sizing Manager for HVAC Sizing Simulations"; }
		bool oneTimeInit = true;

		int NumCoincidentPlantLoops;
		std::vector< PlantCoinicidentAnalyis > PlantCoincAnalyObjs ;
		bool PlantCoinAnalyRequestsAnotherIteration;

		SizingLoggerFramework SizingLogger;

		void initialize();

		void determineSizingAnalysesNeeded();

		void createNewCoincidentPlantAnalysisObject(
			std::string const & PlantLoopName,
			int const PlantSizingIndex
		);

		void setupSizingAnalyses();

		void processCoincidentPlantSizeAdjustments(
			int const HVACSizingIterCount
		);

		void RedoKickOffAndResize();

	};

	namespace HVACSizingSimulationManagerNamespace {



		extern bool stillNeedToSetupOnce;

		void ManageHVACSizingSimulation(
			bool & ErrorsFound
		);

		void UpdateSizingLogsZoneStep();

	}

}

#endif
