
// C++ Headers
#include <vector>

// ObjexxFCL Headers

#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <HVACSizingSimulationManager.hh>
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



namespace EnergyPlus { 


	void HVACSizingSimulationManager::initialize() {

		if (this->oneTimeInit ) {
		

			this->oneTimeInit = false;
		}
	}

	void HVACSizingSimulationManager::determineSizingAnalysesNeeded(){
	
		int NumPlantLoopsWithCoincidentSizing;
		bool anyAdvancedSizingNeeded = false;

		using DataSizing::PlantSizData;
		using DataSizing::NumPltSizInput;
		using DataSizing::Coincident;


		//currently the only type of advanced sizing analysis available is for coincident plant sizing
		// expect more specialized sizing analysis objects to be added, so minimizing code here and jump to a worker method once we know an instance is to be created.

		//Loop over PlantSizData struct and find those plant loops that are to use coincident sizing
		for (int i=1; i <= NumPltSizInput; i++){
		
			if (PlantSizData(i).ConcurrenceOption == Coincident ){

				//create an instance of analysis object for each loop
				this->createNewCoincidentPlantAnalysisObject ( PlantSizData(i).PlantLoopName );
				anyAdvancedSizingNeeded = true;
			}
		}

		if (anyAdvancedSizingNeeded) {
			//setup central sizing logger framework
			//how make an instance of SizingLogger?
			SizingLoggerFramework SizingLogger;

		}
	}

	void HVACSizingSimulationManager::createNewCoincidentPlantAnalysisObject(
			std::string const & PlantLoopName
		) {
			using DataPlant::PlantLoop;
			using DataPlant::TotNumLoops;
			using DataPlant::SupplySide;

			PlantCoinicidentAnalyis tmpAnalysisObj;

			tmpAnalysisObj.name = PlantLoopName;
			//find plant loop number
			for (int i = 1; i <= TotNumLoops; i++ ){
			
				if (PlantLoopName == PlantLoop(i).Name) { //found it
					tmpAnalysisObj.PlantLoop = i;
					tmpAnalysisObj.SupplySideInletNodeNum = PlantLoop(i).LoopSide(SupplySide).NodeNumIn;
					tmpAnalysisObj.SizingLogger = SizingLogger;
				}
			}

		 PlantCoincAnalyObjs.push_back( tmpAnalysisObj );
	}

	void HVACSizingSimulationManager::setupSizingAnalyses(){
	
		for (auto &P : PlantCoincAnalyObjs) {
			//call setup routine on each coincident plant analysis object 
			P.SetupPlantLogs();

	}
	
	}

	


	namespace HVACSizingSimulationManagerNamespace { 

//		using namespace DataPrecisionGlobals;
		using namespace WeatherManager;
		using namespace DataGlobals;
		using namespace DataReportingFlags;
		using namespace HeatBalanceManager;

		bool stillNeedToSetupOnce = true;

		HVACSizingSimulationManager SizeSimManagerObj;

		void ManageAdvancedSizing(
			bool & ErrorsFound
		) {
			using DataGlobals::DoOutputReporting;
			using DataGlobals::HVACSizingSimMaxIterations;
			using DataGlobals::WarmupFlag;
			using DataGlobals::DoDesDaySim;
			using DataGlobals::KindOfSim;
			using DataGlobals::ksDesignDay;
			using DataGlobals::ksRunPeriodDesign;
			using DataGlobals::ksRunPeriodWeather;
			using DataGlobals::DayOfSim;
			using DataGlobals::DayOfSimChr;
			using DataGlobals::BeginEnvrnFlag;
			using DataGlobals::EndEnvrnFlag;
			using DataGlobals::emsCallFromBeginNewEvironment;
			using DataGlobals::NumOfDayInEnvrn;
			using DataGlobals::BeginDayFlag;
			using DataGlobals::EndDayFlag;
			using DataEnvironment::EnvironmentName;
			using DataEnvironment::CurMnDy;
			using DataEnvironment::EndMonthFlag;
			using DataEnvironment::CurrentOverallSimDay;
			using DataEnvironment::TotalOverallSimDays;
			using General::TrimSigDigits;

			using EMSManager::ManageEMS;
			using PlantPipingSystemsManager::InitAndSimGroundDomains;
			using ExteriorEnergyUse::ManageExteriorEnergyUse;


			using DataSystemVariables::ReportDuringHVACSizingSimulation;
			using DataErrorTracking::ExitDuringSimulations;
			using DataReportingFlags::NumOfWarmupDays;
			static bool Available; // an environment is available to process
			int HVACSizingIterCount;
			static gio::Fmt Format_700("('Environment:WarmupDays,',I3)");
			static gio::Fmt fmtLD( "*" );

			if (stillNeedToSetupOnce) { // should only enter once but to be safe
				HVACSizingSimulationManager SizeSimManagerObj;
				stillNeedToSetupOnce = false;
			}
			SizeSimManagerObj.determineSizingAnalysesNeeded();

			SizeSimManagerObj.setupSizingAnalyses();



			DisplayString( "Beginning HVAC Sizing Simulation" );
			if (! ReportDuringHVACSizingSimulation) DoOutputReporting = false;

			ResetEnvironmentCounter();

			
	// iterations over set of sizing periods for HVAC sizing Simulation
		// currently just running up to max. 
		for (HVACSizingIterCount = 1; HVACSizingIterCount <= HVACSizingSimMaxIterations; HVACSizingIterCount++) {
		

			//need to extend Environment structure array to distinguish the HVAC Sizing Simulation from the regular run of that sizing period, repeats for each set
			AddDesignSetToEnvironmentStruct(HVACSizingIterCount);


			WarmupFlag = true;
			Available = true;
			while (Available) {

				GetNextEnvironment(Available, ErrorsFound);

				if (!Available) break;
				if (ErrorsFound) break;
				if (!DoDesDaySim)  continue;
				if (KindOfSim == ksRunPeriodWeather) continue;
				if (KindOfSim == ksDesignDay) continue;
				if (KindOfSim == ksRunPeriodDesign) continue;

				if (Environment(Envrn).HVACSizingIterationNum != HVACSizingIterCount) continue;

				if (ReportDuringHVACSizingSimulation){
					if (sqlite->writeOutputToSQLite()) {
						sqlite->sqliteBegin();
						sqlite->createSQLiteEnvironmentPeriodRecord();
						sqlite->sqliteCommit();
					}
				}
				ExitDuringSimulations = true;

				DisplayString("Initializing New Environment Parameters, HVAC Sizing Simulation");

				BeginEnvrnFlag = true;
				EndEnvrnFlag = false;
				//EndMonthFlag = false;
				WarmupFlag = true;
				DayOfSim = 0;
				DayOfSimChr = "0";
				NumOfWarmupDays = 0;

				ManageEMS(emsCallFromBeginNewEvironment); // calling point

				while ((DayOfSim < NumOfDayInEnvrn) || (WarmupFlag)) { // Begin day loop ...

					if (ReportDuringHVACSizingSimulation) {
						if (sqlite->writeOutputToSQLite()) sqlite->sqliteBegin(); // setup for one transaction per day
					}
					++DayOfSim;
					gio::write(DayOfSimChr, fmtLD) << DayOfSim;
					strip(DayOfSimChr);
					if (!WarmupFlag) {
						++CurrentOverallSimDay;
						DisplaySimDaysProgress(CurrentOverallSimDay, TotalOverallSimDays);
					}
					else {
						DayOfSimChr = "0";
					}
					BeginDayFlag = true;
					EndDayFlag = false;

					if (WarmupFlag) {
						++NumOfWarmupDays;
						cWarmupDay = TrimSigDigits(NumOfWarmupDays);
						DisplayString("Warming up {" + cWarmupDay + '}');
					}
					else if (DayOfSim == 1) {
						DisplayString("Starting HVAC Sizing Simulation at " + CurMnDy + " for " + EnvironmentName);
						gio::write(OutputFileInits, Format_700) << NumOfWarmupDays;
					}
					else if (DisplayPerfSimulationFlag) {
						DisplayString("Continuing Simulation at " + CurMnDy + " for " + EnvironmentName);
						DisplayPerfSimulationFlag = false;
					}

					for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...

						BeginHourFlag = true;
						EndHourFlag = false;

						for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
							if (AnySlabsInModel || AnyBasementsInModel){
								InitAndSimGroundDomains();
							}

							BeginTimeStepFlag = true;


							// Set the End__Flag variables to true if necessary.  Note that
							// each flag builds on the previous level.  EndDayFlag cannot be
							// .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
							// EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
							// Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
							// SubTimeStepFlags can/will be set/reset in the HVAC Manager.

							if (TimeStep == NumOfTimeStepInHour) {
								EndHourFlag = true;
								if (HourOfDay == 24) {
									EndDayFlag = true;
									if ((!WarmupFlag) && (DayOfSim == NumOfDayInEnvrn)) {
										EndEnvrnFlag = true;
									}
								}
							}

							ManageWeather();

							ManageExteriorEnergyUse();

							ManageHeatBalance();

							BeginHourFlag = false;
							BeginDayFlag = false;
							BeginEnvrnFlag = false;
							BeginSimFlag = false;
							BeginFullSimFlag = false;

						} // TimeStep loop

						PreviousHour = HourOfDay;

					} // ... End hour loop.

					if (sqlite->writeOutputToSQLite()) sqlite->sqliteCommit(); // one transaction per day

				} // ... End day loop.


			} // ... End environment loop.
		} // End HVAC Sizing Iteration loop
		WarmupFlag = false;
		DoOutputReporting = true;
	
		}

		void UpdateSizingLogger (){
		
		//SizingLogger.
		
		}
	}
}