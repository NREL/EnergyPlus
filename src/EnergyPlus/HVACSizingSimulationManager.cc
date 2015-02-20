
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
#include <FluidProperties.hh>
#include <PlantManager.hh>
#include <SimulationManager.hh>


namespace EnergyPlus { 


	void HVACSizingSimulationManager::DetermineSizingAnalysesNeeded()
	{
		using DataSizing::PlantSizData;
		using DataSizing::NumPltSizInput;
		using DataSizing::Coincident;

		//currently the only type of advanced sizing analysis available is for coincident plant sizing
		// expect more specialized sizing analysis objects to be added, so minimizing code here and jump to a worker method once we know an instance is to be created.

		//Loop over PlantSizData struct and find those plant loops that are to use coincident sizing
		for (int i=1; i <= NumPltSizInput; i++){
		
			if (PlantSizData(i).ConcurrenceOption == Coincident ){

				//create an instance of analysis object for each loop
				CreateNewCoincidentPlantAnalysisObject ( PlantSizData(i).PlantLoopName, i );

			}
		}
		
	}

	void HVACSizingSimulationManager::CreateNewCoincidentPlantAnalysisObject(
		std::string const PlantLoopName,
		int const PlantSizingIndex ) 
	{
		Real64 density;
		Real64 cp;
		using DataPlant::PlantLoop;
		using DataPlant::TotNumLoops;
		using DataPlant::SupplySide;
		using DataGlobals::InitConvTemp;
		using namespace FluidProperties;
		using DataSizing::PlantSizData;


		//find plant loop number
		for (int i = 1; i <= TotNumLoops; i++ ){
			if ( PlantLoopName == PlantLoop( i ).Name ) { //found it

				density = GetDensityGlycol( PlantLoop( i ).FluidName, 
								InitConvTemp, PlantLoop( i ).FluidIndex, 
								"createNewCoincidentPlantAnalysisObject" );
				cp = GetSpecificHeatGlycol( PlantLoop( i ).FluidName, 
								InitConvTemp, PlantLoop( i ).FluidIndex, 
								"createNewCoincidentPlantAnalysisObject" );

				PlantCoinicidentAnalysis tmpAnalysisObj ( // call constructor
					PlantLoopName,
					i,
					PlantLoop( i ).LoopSide( SupplySide ).NodeNumIn,
					density,
					cp,
					PlantSizData( PlantSizingIndex ).NumTimeStepsInAvg,
					PlantSizingIndex );

				plantCoincAnalyObjs.push_back( tmpAnalysisObj ); //store new object in vector
			}
		}
	}

	void HVACSizingSimulationManager::SetupSizingAnalyses()
	{
		using DataLoopNode::Node;
		using DataPlant::PlantReport;
		using DataSizing::PlantSizData;
		using DataSizing::HeatingLoop;
		using DataSizing::CoolingLoop;

		for ( auto &P : plantCoincAnalyObjs ) {
			//call setup log routine for each coincident plant analysis object 
			P.supplyInletNodeFlow_LogIndex = sizingLogger.SetupVariableSizingLog( 
				Node(P.supplySideInletNodeNum).MassFlowRate,
				P.numTimeStepsInAvg );
			P.supplyInletNodeTemp_LogIndex = sizingLogger.SetupVariableSizingLog( 
				Node(P.supplySideInletNodeNum).Temp,
				P.numTimeStepsInAvg );
			if ( PlantSizData(P.plantSizingIndex).LoopType == HeatingLoop ) {
				P.loopDemand_LogIndex = sizingLogger.SetupVariableSizingLog( 
					PlantReport(P.plantLoopIndex ).HeatingDemand,
					P.numTimeStepsInAvg ) ;
			} else if ( PlantSizData(P.plantSizingIndex).LoopType == CoolingLoop ) {
				P.loopDemand_LogIndex = sizingLogger.SetupVariableSizingLog( 
					PlantReport(P.plantLoopIndex ).CoolingDemand,
					P.numTimeStepsInAvg ) ;
			}

		}
	}

	void HVACSizingSimulationManager::PostProcessLogs() 
	{
		//this function calls methods on log objects to do general processing on all the logged data in the framework
		for ( auto &L : sizingLogger.logObjs ) {
			L.AverageSysTimeSteps(); // collapse subtimestep data into zone step data
			L.ProcessRunningAverage(); // apply zone step moving average
		}
	}

	void HVACSizingSimulationManager::ProcessCoincidentPlantSizeAdjustments(
		int const HVACSizingIterCount )
	{

		using namespace DataPlant;
		using namespace PlantManager;
		using namespace DataSizing;
		using DataGlobals::FinalSizingHVACSizingSimIteration;

		//first pass through coincident plant objects to check new sizes and see if more iteration needed
		plantCoinAnalyRequestsAnotherIteration = false;
		for ( auto &P : plantCoincAnalyObjs ) {
			//step 1 find maximum flow rate on concurrent return temp and load
			P.newFoundMassFlowRateTimeStamp = sizingLogger.logObjs[ P.supplyInletNodeFlow_LogIndex ].GetLogVariableDataMax( );
			P.peakMdotCoincidentDemand = sizingLogger.logObjs[ P.loopDemand_LogIndex].GetLogVariableDataAtTimestamp ( P.newFoundMassFlowRateTimeStamp );
			P.peakMdotCoincidentReturnTemp = sizingLogger.logObjs[ P.supplyInletNodeTemp_LogIndex].GetLogVariableDataAtTimestamp ( P.newFoundMassFlowRateTimeStamp );

			// step 2 find maximum load and concurrent flow and return temp
			P.NewFoundMaxDemandTimeStamp = sizingLogger.logObjs[ P.loopDemand_LogIndex ].GetLogVariableDataMax( );
			P.peakDemandMassFlow = sizingLogger.logObjs[ P.supplyInletNodeFlow_LogIndex ].GetLogVariableDataAtTimestamp( P.NewFoundMaxDemandTimeStamp );
			P.peakDemandReturnTemp = sizingLogger.logObjs[ P.supplyInletNodeTemp_LogIndex ].GetLogVariableDataAtTimestamp( P.NewFoundMaxDemandTimeStamp );

			P.ResolveDesignFlowRate( HVACSizingIterCount );
			if ( P.anotherIterationDesired ){
				plantCoinAnalyRequestsAnotherIteration = true;
			}
		}

		// as more sizing adjustments are added this will need to change to consider all not just plant coincident
		FinalSizingHVACSizingSimIteration = plantCoinAnalyRequestsAnotherIteration;

	}
	void HVACSizingSimulationManager::RedoKickOffAndResize()
	{
		using DataGlobals::KickOffSimulation;
		using DataGlobals::RedoSizesHVACSimulation;
		using namespace WeatherManager;
		using namespace SimulationManager;
		bool ErrorsFound( false );
		KickOffSimulation = true;
		RedoSizesHVACSimulation = true;

		ResetEnvironmentCounter();
		SetupSimulation( ErrorsFound );

		KickOffSimulation = false;
		RedoSizesHVACSimulation = false;
	}


	namespace HVACSizingSimulationManagerNamespace { 

		using namespace WeatherManager;
		using namespace DataGlobals;
		using namespace DataReportingFlags;
		using namespace HeatBalanceManager;

		bool stillNeedToSetupOnce = true;

		HVACSizingSimulationManager sizeSimManagerObj;

		void ManageHVACSizingSimulation(
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

			sizeSimManagerObj.DetermineSizingAnalysesNeeded();

			sizeSimManagerObj.SetupSizingAnalyses();

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

			sizeSimManagerObj.PostProcessLogs();

			sizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( HVACSizingIterCount );

			sizeSimManagerObj.RedoKickOffAndResize();

			if ( ! sizeSimManagerObj.plantCoinAnalyRequestsAnotherIteration ) {
				// jump out of for loop, or change for to a while
				break;
			}

			sizeSimManagerObj.sizingLogger.IncrementSizingPeriodSet ( HVACSizingIterCount );

		} // End HVAC Sizing Iteration loop


		WarmupFlag = false;
		DoOutputReporting = true;
		sizeSimManagerObj.~HVACSizingSimulationManager(); //call destructor to free memory etc. 
		return;
		}

		void UpdateSizingLogsZoneStep ()
		{
			sizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		}

		void UpdateSizingLogsSystemStep() 
		{
			sizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
		}
	}
}
