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
#include <UtilityRoutines.hh>

namespace EnergyPlus {

	class HVACSizingSimulationManager {
	public:

		std::vector< PlantCoinicidentAnalysis > plantCoincAnalyObjs ;
		bool plantCoinAnalyRequestsAnotherIteration;

		SizingLoggerFramework sizingLogger;

		void DetermineSizingAnalysesNeeded();
		void SetupSizingAnalyses();

		void RedoKickOffAndResize();
		void PostProcessLogs();
		void ProcessCoincidentPlantSizeAdjustments(
			int const HVACSizingIterCount
		);

		void UpdateSizingLogsZoneStep();
		void UpdateSizingLogsSystemStep();

	private:
		int numCoincidentPlantLoops;

		void CreateNewCoincidentPlantAnalysisObject(
			std::string const plantLoopName,
			int const plantSizingIndex
		);


	};

	extern std::unique_ptr< HVACSizingSimulationManager > hvacSizingSimulationManager;

	void ManageHVACSizingSimulation( bool & ErrorsFound );
}

#endif
