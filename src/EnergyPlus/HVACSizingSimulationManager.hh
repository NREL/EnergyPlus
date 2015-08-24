#ifndef HVACSizingSimulationManager_hh_INCLUDED
#define HVACSizingSimulationManager_hh_INCLUDED

// C++ Headers
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>
#include <DataReportingFlags.hh>
#include <DataSizing.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <EMSManager.hh>
#include <ExteriorEnergyUse.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <PlantManager.hh>
#include <PlantPipingSystemsManager.hh>
#include <SimulationManager.hh>
#include <SizingAnalysisObjects.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

class HVACSizingSimulationManager
{
public:

	std::vector< PlantCoinicidentAnalysis > plantCoincAnalyObjs;
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

	void CreateNewCoincidentPlantAnalysisObject(
		std::string const & PlantLoopName,
		int const PlantSizingIndex
	);

};

extern std::unique_ptr< HVACSizingSimulationManager > hvacSizingSimulationManager;

void ManageHVACSizingSimulation( bool & ErrorsFound );

} // EnergyPlus

#endif
