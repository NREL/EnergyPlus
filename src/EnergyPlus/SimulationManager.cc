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

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
}

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <SimulationManager.hh>
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CostEstimateManager.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataBranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataOutputs.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataRuntimeLanguage.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DataZoneEquipment.hh>
#include <DemandManager.hh>
#include <DisplayRoutines.hh>
#include <DualDuct.hh>
#include <EconomicLifeCycleCost.hh>
#include <EconomicTariff.hh>
#include <ElectricPowerServiceManager.hh>
#include <EMSManager.hh>
#include <ExteriorEnergyUse.hh>
#include <ExternalInterface.hh>
#include <FaultsManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceAirManager.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <HVACControllers.hh>
#include <HVACManager.hh>
#include <HVACSizingSimulationManager.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <OutputReports.hh>
#include <PlantManager.hh>
#include <PollutionModule.hh>
#include <PlantPipingSystemsManager.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <SetPointManager.hh>
#include <SizingManager.hh>
#include <SolarShading.hh>
#include <SQLiteProcedures.hh>
#include <SystemReports.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>
#include <ZoneContaminantPredictorCorrector.hh>
#include <ZoneTempPredictorCorrector.hh>
#include <ZoneEquipmentManager.hh>
#include <Timer.h>

namespace EnergyPlus {

// HBIRE_USE_OMP defined, then openMP instructions are used.  Compiler may have to have switch for openmp
// HBIRE_NO_OMP defined, then old code is used without any openmp instructions
// HBIRE - loop in HeatBalanceIntRadExchange.cc

#ifdef HBIRE_USE_OMP
#undef HBIRE_NO_OMP
#else
#define HBIRE_NO_OMP
#endif

namespace SimulationManager {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   January 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains the main driver routine which manages the major
	// control loops of the EnergyPlus simulation.  This module is also
	// responsible for setting the global environment flags for these
	// loops.

	// METHODOLOGY EMPLOYED:
	// This module was constructed from the remnants of (I)BLAST routines
	// SIMBLD (Simulate Building), SIMZG (Simulate Zone Group), and SIMZGD
	// (Simulate Zone Group for a Day).

	// REFERENCES:
	// (I)BLAST legacy code, internal Reverse Engineering documentation,
	// and internal Evolutionary Engineering documentation.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataSizing;
	using namespace DataReportingFlags;
	using namespace DataSystemVariables;
	using namespace HeatBalanceManager;
	using namespace WeatherManager;
	using namespace ExternalInterface;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;
	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	bool RunPeriodsInInput( false );
	bool RunControlInInput( false );

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of SimulationManager should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool PreP_Fatal( false );
	}

	// SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

	// MODULE SUBROUTINES:

	// Functions
	void
	clear_state()
	{
		RunPeriodsInInput = false;
		RunControlInInput = false;
		PreP_Fatal = false;
	}


	void
	ManageSimulation()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   January 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver of the simulation manager module.
		// It contains the main environment-time loops for the building
		// simulation.  This includes the environment loop, a day loop, an
		// hour loop, and a time step loop.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::CurrentOverallSimDay;
		using DataEnvironment::TotalOverallSimDays;
		using DataEnvironment::TotDesDays;
		using DataEnvironment::TotRunDesPersDays;
		using DataEnvironment::EndMonthFlag;
		using InputProcessor::GetNumRangeCheckErrorsFound;
		using InputProcessor::GetNumObjectsFound;
		using SizingManager::ManageSizing;
		using ExteriorEnergyUse::ManageExteriorEnergyUse;
		using OutputReportTabular::WriteTabularReports;
		using OutputReportTabular::OpenOutputTabularFile;
		using OutputReportTabular::CloseOutputTabularFile;
		using OutputReportTabular::ResetTabularReports;
		using DataErrorTracking::AskForConnectionsReport;
		using DataErrorTracking::ExitDuringSimulations;
		using OutputProcessor::SetupTimePointers;
		using OutputProcessor::ReportForTabularReports;
		using CostEstimateManager::SimCostEstimate;
		using EconomicTariff::ComputeTariff; // added for computing annual utility costs
		using EconomicTariff::WriteTabularTariffReports;
		using General::TrimSigDigits;
		using OutputReportPredefined::SetPredefinedTables;
		using HVACControllers::DumpAirLoopStatistics;
		using NodeInputManager::SetupNodeVarsForReporting;
		using NodeInputManager::CheckMarkedNodes;
		using BranchNodeConnections::CheckNodeConnections;
		using BranchNodeConnections::TestCompSetInletOutletNodes;
		using PollutionModule::SetupPollutionMeterReporting;
		using PollutionModule::SetupPollutionCalculations;
		using PollutionModule::CheckPollutionMeterReporting;
		using SystemReports::ReportAirLoopConnections;
		using SystemReports::CreateEnergyReportStructure;
		using BranchInputManager::ManageBranchInput;
		using BranchInputManager::TestBranchIntegrity;
		using BranchInputManager::InvalidBranchDefinitions;
		using MixedAir::CheckControllerLists;
		using EMSManager::CheckIfAnyEMS;
		using EMSManager::ManageEMS;
		using EconomicLifeCycleCost::GetInputForLifeCycleCost;
		using EconomicLifeCycleCost::ComputeLifeCycleCostAndReport;
		using DemandManager::InitDemandManagers;
		using PlantManager::CheckIfAnyPlant;
		using CurveManager::InitCurveReporting;
		using namespace DataTimings;
		using DataSystemVariables::FullAnnualRun;
		using SetPointManager::CheckIfAnyIdealCondEntSetPoint;
		using Psychrometrics::InitializePsychRoutines;
		using FaultsManager::CheckAndReadFaults;
		using PlantPipingSystemsManager::SimulateGroundDomains;
		using PlantPipingSystemsManager::CheckIfAnySlabs;
		using PlantPipingSystemsManager::CheckIfAnyBasements;
		using OutputProcessor::ResetAccumulationWhenWarmupComplete;
		using OutputProcessor::isFinalYear;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Available; // an environment is available to process
		static bool ErrorsFound( false );
		static bool TerminalError( false );
		bool SimsDone;
		bool ErrFound;
		//  REAL(r64) :: t0,t1,st0,st1

		//  CHARACTER(len=70) :: tdstring
		//  CHARACTER(len=138) :: tdstringlong

		int EnvCount;

		// Formats
		static gio::Fmt Format_700( "('Environment:WarmupDays,',I3)" );

		//CreateSQLiteDatabase();
		sqlite = EnergyPlus::CreateSQLiteDatabase();

		if ( sqlite ) {
			sqlite->sqliteBegin();
			sqlite->createSQLiteSimulationsRecord( 1, DataStringGlobals::VerString, DataStringGlobals::CurrentDateTime );
			sqlite->sqliteCommit();
		}

		// FLOW:
		PostIPProcessing();

		InitializePsychRoutines();

		BeginSimFlag = true;
		BeginFullSimFlag = false;
		DoOutputReporting = false;
		DisplayPerfSimulationFlag = false;
		DoWeatherInitReporting = false;
		RunPeriodsInInput = ( GetNumObjectsFound( "RunPeriod" ) > 0 || GetNumObjectsFound( "RunPeriod:CustomRange" ) > 0 || FullAnnualRun );
		AskForConnectionsReport = false; // set to false until sizing is finished

		OpenOutputFiles();
		CheckThreading();
		GetProjectData();
		CheckForMisMatchedEnvironmentSpecifications();
		CheckForRequestedReporting();
		SetPredefinedTables();
		SetPreConstructionInputParameters(); //establish array bounds for constructions early

		SetupTimePointers( "Zone", TimeStepZone ); // Set up Time pointer for HB/Zone Simulation
		SetupTimePointers( "HVAC", TimeStepSys );

		CheckIfAnyEMS();
		CheckIfAnyPlant();
		CheckIfAnySlabs();
		CheckIfAnyBasements();
		CheckIfAnyIdealCondEntSetPoint();
		createFacilityElectricPowerServiceObject();

		ManageBranchInput(); // just gets input and returns.

		DoingSizing = true;
		ManageSizing();

		CheckAndReadFaults();

		BeginFullSimFlag = true;
		SimsDone = false;
		if ( DoDesDaySim || DoWeathSim || DoHVACSizingSimulation ) {
			DoOutputReporting = true;
		}
		DoingSizing = false;

		if ( ( DoZoneSizing || DoSystemSizing || DoPlantSizing ) && ! ( DoDesDaySim || ( DoWeathSim && RunPeriodsInInput ) ) ) {
			ShowWarningError( "ManageSimulation: Input file has requested Sizing Calculations but no Simulations are requested (in SimulationControl object). Succeeding warnings/errors may be confusing." );
		}
		Available = true;

		if ( InvalidBranchDefinitions ) {
			ShowFatalError( "Preceding error(s) in Branch Input cause termination." );
		}

		DisplayString( "Initializing Simulation" );
		KickOffSimulation = true;

		ResetEnvironmentCounter();
		SetupSimulation( ErrorsFound );
		InitCurveReporting();

		AskForConnectionsReport = true; // set to true now that input processing and sizing is done.
		KickOffSimulation = false;
		WarmupFlag = false;
		DoWeatherInitReporting = true;

		//  Note:  All the inputs have been 'gotten' by the time we get here.
		ErrFound = false;
		if ( DoOutputReporting ) {
			DisplayString( "Reporting Surfaces" );

			ReportSurfaces();

			SetupNodeVarsForReporting();
			MetersHaveBeenInitialized = true;
			SetupPollutionMeterReporting();
			UpdateMeterReporting();
			CheckPollutionMeterReporting();
			facilityElectricServiceObj->verifyCustomMetersElecPowerMgr();
			SetupPollutionCalculations();
			InitDemandManagers();

			TestBranchIntegrity( ErrFound );
			if ( ErrFound ) TerminalError = true;
			TestAirPathIntegrity( ErrFound );
			if ( ErrFound ) TerminalError = true;
			CheckMarkedNodes( ErrFound );
			if ( ErrFound ) TerminalError = true;
			CheckNodeConnections( ErrFound );
			if ( ErrFound ) TerminalError = true;
			TestCompSetInletOutletNodes( ErrFound );
			if ( ErrFound ) TerminalError = true;
			CheckControllerLists( ErrFound );
			if ( ErrFound ) TerminalError = true;

			if ( DoDesDaySim || DoWeathSim ) {
				ReportLoopConnections();
				ReportAirLoopConnections();
				ReportNodeConnections();
				// Debug reports
				//      CALL ReportCompSetMeterVariables
				//      CALL ReportParentChildren
			}

			CreateEnergyReportStructure();

			ManageEMS( emsCallFromSetupSimulation ); // point to finish setup processing EMS, sensor ready now

			ProduceRDDMDD();

			if ( TerminalError ) {
				ShowFatalError( "Previous Conditions cause program termination." );
			}
		}

		if ( sqlite ) {
			sqlite->sqliteBegin();
			sqlite->updateSQLiteSimulationRecord( 1, DataGlobals::NumOfTimeStepInHour );
			sqlite->sqliteCommit();
		}

		GetInputForLifeCycleCost(); //must be prior to WriteTabularReports -- do here before big simulation stuff.

		// if user requested HVAC Sizing Simulation, call HVAC sizing simulation manager
		if ( DoHVACSizingSimulation ) {
			ManageHVACSizingSimulation( ErrorsFound );
		}

		ShowMessage( "Beginning Simulation" );
		DisplayString( "Beginning Primary Simulation" );

		ResetEnvironmentCounter();

		EnvCount = 0;
		WarmupFlag = true;

		while ( Available ) {

			GetNextEnvironment( Available, ErrorsFound );

			if ( ! Available ) break;
			if ( ErrorsFound ) break;
			if ( ( ! DoDesDaySim ) && ( KindOfSim != ksRunPeriodWeather ) ) continue;
			if ( ( ! DoWeathSim ) && ( KindOfSim == ksRunPeriodWeather ) ) continue;
			if (KindOfSim == ksHVACSizeDesignDay) continue; // don't run these here, only for sizing simulations

			if (KindOfSim == ksHVACSizeRunPeriodDesign) continue; // don't run these here, only for sizing simulations

			++EnvCount;

			if ( sqlite ) {
				sqlite->sqliteBegin();
				sqlite->createSQLiteEnvironmentPeriodRecord( DataEnvironment::CurEnvirNum, DataEnvironment::EnvironmentName, DataGlobals::KindOfSim );
				sqlite->sqliteCommit();
			}

			ExitDuringSimulations = true;
			SimsDone = true;
			DisplayString( "Initializing New Environment Parameters" );

			BeginEnvrnFlag = true;
			EndEnvrnFlag = false;
			EndMonthFlag = false;
			WarmupFlag = true;
			DayOfSim = 0;
			DayOfSimChr = "0";
			NumOfWarmupDays = 0;
			if ( NumOfDayInEnvrn <= 365 ){
				isFinalYear = true;
			}

			ManageEMS( emsCallFromBeginNewEvironment ); // calling point

			while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) { // Begin day loop ...

				if ( sqlite ) sqlite->sqliteBegin(); // setup for one transaction per day

				++DayOfSim;
				gio::write( DayOfSimChr, fmtLD ) << DayOfSim;
				strip( DayOfSimChr );
				if ( ! WarmupFlag ) {
					++CurrentOverallSimDay;
					DisplaySimDaysProgress( CurrentOverallSimDay, TotalOverallSimDays );
				} else {
					DayOfSimChr = "0";
				}
				BeginDayFlag = true;
				EndDayFlag = false;


				if ( WarmupFlag ) {
					++NumOfWarmupDays;
					cWarmupDay = TrimSigDigits( NumOfWarmupDays );
					DisplayString( "Warming up {" + cWarmupDay + '}' );
				} else if ( DayOfSim == 1 ) {
					DisplayString( "Starting Simulation at " + CurMnDy + " for " + EnvironmentName );
					gio::write( OutputFileInits, Format_700 ) << NumOfWarmupDays;
					ResetAccumulationWhenWarmupComplete();
				} else if ( DisplayPerfSimulationFlag ) {
					DisplayString( "Continuing Simulation at " + CurMnDy + " for " + EnvironmentName );
					DisplayPerfSimulationFlag = false;
				}
				// for simulations that last longer than a week, identify when the last year of the simulation is started
				if ( ( DayOfSim > 365 ) && ( (NumOfDayInEnvrn - DayOfSim) == 364 ) && !WarmupFlag ){
					DisplayString( "Starting last  year of environment at:  " + DayOfSimChr );
					ResetTabularReports();
				}

				for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...

					BeginHourFlag = true;
					EndHourFlag = false;

					for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) {
						if ( AnySlabsInModel || AnyBasementsInModel ) {
							SimulateGroundDomains( false );
						}

						BeginTimeStepFlag = true;
						ExternalInterfaceExchangeVariables();

						// Set the End__Flag variables to true if necessary.  Note that
						// each flag builds on the previous level.  EndDayFlag cannot be
						// .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
						// EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
						// Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
						// SubTimeStepFlags can/will be set/reset in the HVAC Manager.

						if ( TimeStep == NumOfTimeStepInHour ) {
							EndHourFlag = true;
							if ( HourOfDay == 24 ) {
								EndDayFlag = true;
								if ( ( ! WarmupFlag ) && ( DayOfSim == NumOfDayInEnvrn ) ) {
									EndEnvrnFlag = true;
								}
							}
						}

						ManageWeather();

						ManageExteriorEnergyUse();

						ManageHeatBalance();

						//  After the first iteration of HeatBalance, all the 'input' has been gotten
						if ( BeginFullSimFlag ) {
							if ( GetNumRangeCheckErrorsFound() > 0 ) {
								ShowFatalError( "Out of \"range\" values found in input" );
							}
						}

						BeginHourFlag = false;
						BeginDayFlag = false;
						BeginEnvrnFlag = false;
						BeginSimFlag = false;
						BeginFullSimFlag = false;

					} // TimeStep loop

					PreviousHour = HourOfDay;

				} // ... End hour loop.

				if ( sqlite ) sqlite->sqliteCommit(); // one transaction per day

			} // ... End day loop.

			// Need one last call to send latest states to middleware
			ExternalInterfaceExchangeVariables();

		} // ... End environment loop.

		WarmupFlag = false;
		if ( ! SimsDone && DoDesDaySim ) {
			if ( ( TotDesDays + TotRunDesPersDays ) == 0 ) { // if sum is 0, then there was no sizing done.
				ShowWarningError( "ManageSimulation: SizingPeriod:* were requested in SimulationControl but no SizingPeriod:* objects in input." );
			}
		}

		if ( ! SimsDone && DoWeathSim ) {
			if ( ! RunPeriodsInInput ) { // if no run period requested, and sims not done
				ShowWarningError( "ManageSimulation: Weather Simulation was requested in SimulationControl but no RunPeriods in input." );
			}
		}

		if ( sqlite ) sqlite->sqliteBegin(); // for final data to write

#ifdef EP_Detailed_Timings
		epStartTime( "Closeout Reporting=" );
#endif
		SimCostEstimate();

		ComputeTariff(); //     Compute the utility bills

		ReportForTabularReports(); // For Energy Meters (could have other things that need to be pushed to after simulation)

		OpenOutputTabularFile();

		WriteTabularReports(); //     Create the tabular reports at completion of each

		WriteTabularTariffReports();

		ComputeLifeCycleCostAndReport(); //must be after WriteTabularReports and WriteTabularTariffReports

		CloseOutputTabularFile();

		DumpAirLoopStatistics(); // Dump runtime statistics for air loop controller simulation to csv file

#ifdef EP_Detailed_Timings
		epStopTime( "Closeout Reporting=" );
#endif
		CloseOutputFiles();

		// sqlite->createZoneExtendedOutput();
		CreateSQLiteZoneExtendedOutput();

		if ( sqlite ) {
			DisplayString( "Writing final SQL reports" );
			sqlite->sqliteCommit(); // final transactions
			sqlite->initializeIndexes(); // do not create indexes (SQL) until all is done.
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Error condition occurred.  Previous Severe Errors cause termination." );
		}

	}

	void
	GetProjectData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets global project data from the input file.

		// METHODOLOGY EMPLOYED:
		// Use GetObjectItem from the Input Processor

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using DataStringGlobals::MatchVersion;
		using namespace DataConvergParams;
		using namespace DataSystemVariables;
		using DataHVACGlobals::LimitNumSysSteps;
		using DataHVACGlobals::deviationFromSetPtThresholdHtg;
		using DataHVACGlobals::deviationFromSetPtThresholdClg;
		using General::RoundSigDigits;
		using DataEnvironment::DisplayWeatherMissingDataWarnings;
		using DataEnvironment::IgnoreSolarRadiation;
		using DataEnvironment::IgnoreBeamRadiation;
		using DataEnvironment::IgnoreDiffuseRadiation;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_int const Div60( 12, { 1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60 } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string Alphas( 6 );
		Array1D< Real64 > Number( 4 );
		int NumAlpha;
		int NumNumber;
		int IOStat;
		int NumDebugOut;
		int MinInt;
		int Num;
		int Which;
		bool ErrorsFound;
		int Num1;
		int NumA;
		int NumRunControl;
		static std::string VersionID;
		std::string CurrentModuleObject;
		bool CondFDAlgo;
		int Item;

		// Formats
		static gio::Fmt Format_721( "(' Version, ',A)" );
		static gio::Fmt Format_731( "(' Timesteps per Hour, ',I2,', ',I2)" );
		static gio::Fmt Format_733( "(' System Convergence Limits',4(', ',A))" );
		static gio::Fmt Format_741( "(' Simulation Control',$)" );
		static gio::Fmt Format_741_1( "(', ',A,$)" );
		static gio::Fmt Format_751( "(' Output Reporting Tolerances',5(', ',A))" );

		ErrorsFound = false;

		CurrentModuleObject = "Version";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			std::string::size_type const lenVer( len( MatchVersion ) );
			if ( ( lenVer > 0 ) && ( MatchVersion[ lenVer - 1 ] == '0' ) ) {
				Which = static_cast< int >( index( Alphas( 1 ).substr( 0, lenVer - 2 ), MatchVersion.substr( 0, lenVer - 2 ) ) );
			} else {
				Which = static_cast< int >( index( Alphas( 1 ), MatchVersion ) );
			}
			if ( Which != 0 ) {
				ShowWarningError( CurrentModuleObject + ": in IDF=\"" + Alphas( 1 ) + "\" not the same as expected=\"" + MatchVersion + "\"" );
			}
			VersionID = Alphas( 1 );
		} else if ( Num == 0 ) {
			ShowWarningError( CurrentModuleObject + ": missing in IDF, processing for EnergyPlus version=\"" + MatchVersion + "\"" );
		} else {
			ShowSevereError( "Too many " + CurrentModuleObject + " Objects found." );
			ErrorsFound = true;
		}

		// Do Mini Gets on HB Algorithm and by-surface overrides
		CurrentModuleObject = "HeatBalanceAlgorithm";
		Num = GetNumObjectsFound( CurrentModuleObject );
		CondFDAlgo = false;
		if ( Num > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			{ auto const SELECT_CASE_var( Alphas( 1 ) );
			if ( ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) || ( SELECT_CASE_var == "CONDFD" ) || ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCEDETAILED" ) || ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCESIMPLIFIED" ) ) {
				CondFDAlgo = true;
			} else {
			}}
		}
		CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num > 0 ) {
			for ( Item = 1; Item <= Num; ++Item ) {
				GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				{ auto const SELECT_CASE_var( Alphas( 2 ) );
				if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
					CondFDAlgo = true;

				} else {
				}}
			}
		}
		CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num > 0 ) {
			for ( Item = 1; Item <= Num; ++Item ) {
				GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				{ auto const SELECT_CASE_var( Alphas( 3 ) );
				if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
					CondFDAlgo = true;
				} else {
				}}
			}
		}
		CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num > 0 ) {
			for ( Item = 1; Item <= Num; ++Item ) {
				GetObjectItem( CurrentModuleObject, 1, cAlphaArgs, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
				if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
					CondFDAlgo = true;
				} else {
				}}
			}
		}
		CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num > 0 ) {
			for ( Item = 1; Item <= Num; ++Item ) {
				GetObjectItem( CurrentModuleObject, 1, cAlphaArgs, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
				if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
					CondFDAlgo = true;
				} else {
				}}
			}
		}

		CurrentModuleObject = "Timestep";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			NumOfTimeStepInHour = Number( 1 );
			if ( NumOfTimeStepInHour <= 0 || NumOfTimeStepInHour > 60 ) {
				Alphas( 1 ) = RoundSigDigits( NumOfTimeStepInHour );
				ShowWarningError( CurrentModuleObject + ": Requested number (" + Alphas( 1 ) + ") invalid, Defaulted to 4" );
				NumOfTimeStepInHour = 4;
			} else if ( mod( 60, NumOfTimeStepInHour ) != 0 ) {
				MinInt = 9999;
				for ( Num = 1; Num <= 12; ++Num ) {
					if ( std::abs( NumOfTimeStepInHour - Div60( Num ) ) > MinInt ) continue;
					MinInt = NumOfTimeStepInHour - Div60( Num );
					Which = Num;
				}
				ShowWarningError( CurrentModuleObject + ": Requested number (" + RoundSigDigits( NumOfTimeStepInHour ) + ") not evenly divisible into 60, defaulted to nearest (" + RoundSigDigits( Div60( Which ) ) + ")." );
				NumOfTimeStepInHour = Div60( Which );
			}
			if ( CondFDAlgo && NumOfTimeStepInHour < 20 ) {
				ShowWarningError( CurrentModuleObject + ": Requested number (" + RoundSigDigits( NumOfTimeStepInHour ) + ") cannot be used when Conduction Finite Difference algorithm is selected." );
				ShowContinueError( "..." + CurrentModuleObject + " is set to 20." );
				NumOfTimeStepInHour = 20;
			}
			if ( NumOfTimeStepInHour < 4 && GetNumObjectsFound( "Zone" ) > 0 ) {
				ShowWarningError( CurrentModuleObject + ": Requested number (" + RoundSigDigits( NumOfTimeStepInHour ) + ") is less than the suggested minimum of 4." );
				ShowContinueError( "Please see entry for " + CurrentModuleObject + " in Input/Output Reference for discussion of considerations." );
			}
		} else if ( Num == 0 && GetNumObjectsFound( "Zone" ) > 0 && ! CondFDAlgo ) {
			ShowWarningError( "No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 4." );
			NumOfTimeStepInHour = 4;
		} else if ( Num == 0 && ! CondFDAlgo ) {
			NumOfTimeStepInHour = 4;
		} else if ( Num == 0 && GetNumObjectsFound( "Zone" ) > 0 && CondFDAlgo ) {
			ShowWarningError( "No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 20." );
			ShowContinueError( "...Due to presence of Conduction Finite Difference Algorithm selection." );
			NumOfTimeStepInHour = 20;
		} else if ( Num == 0 && CondFDAlgo ) {
			NumOfTimeStepInHour = 20;
		} else {
			ShowSevereError( "Too many " + CurrentModuleObject + " Objects found." );
			ErrorsFound = true;
		}

		TimeStepZone = 1.0 / double( NumOfTimeStepInHour );
		MinutesPerTimeStep = TimeStepZone * 60;
		TimeStepZoneSec = TimeStepZone * SecInHour;

		CurrentModuleObject = "ConvergenceLimits";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			MinInt = int( Number( 1 ) );
			if ( MinInt > MinutesPerTimeStep ) {
				MinInt = MinutesPerTimeStep;
			}
			if ( MinInt < 0 || MinInt > 60 ) {
				ShowWarningError( CurrentModuleObject + ": Requested " + cNumericFieldNames( 1 ) + " (" + RoundSigDigits( MinInt ) + ") invalid. Set to 1 minute." );
				MinTimeStepSys = 1.0 / 60.0;
			} else if ( MinInt == 0 ) { // Set to TimeStepZone
				MinTimeStepSys = TimeStepZone;
			} else {
				MinTimeStepSys = double( MinInt ) / 60.0;
			}
			MaxIter = int( Number( 2 ) );
			if ( MaxIter <= 0 ) {
				MaxIter = 20;
			}
			if ( ! lNumericFieldBlanks( 3 ) ) MinPlantSubIterations = int( Number( 3 ) );
			if ( ! lNumericFieldBlanks( 4 ) ) MaxPlantSubIterations = int( Number( 4 ) );
			// trap bad values
			if ( MinPlantSubIterations < 1 ) MinPlantSubIterations = 1;
			if ( MaxPlantSubIterations < 3 ) MaxPlantSubIterations = 3;
			if ( MinPlantSubIterations > MaxPlantSubIterations ) MaxPlantSubIterations = MinPlantSubIterations + 1;

		} else if ( Num == 0 ) {
			MinTimeStepSys = 1.0 / 60.0;
			MaxIter = 20;
			MinPlantSubIterations = 2;
			MaxPlantSubIterations = 8;
		} else {
			ShowSevereError( "Too many " + CurrentModuleObject + " Objects found." );
			ErrorsFound = true;
		}

		LimitNumSysSteps = int( TimeStepZone / MinTimeStepSys );

		DebugOutput = false;
		EvenDuringWarmup = false;
		CurrentModuleObject = "Output:DebuggingData";
		NumDebugOut = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDebugOut > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat );
			if ( int( Number( 1 ) ) == 1 ) {
				DebugOutput = true;
			}
			if ( int( Number( 2 ) ) == 1 ) {
				EvenDuringWarmup = true;
			}
		}

		CurrentModuleObject = "Output:Diagnostics";
		Num = GetNumObjectsFound( CurrentModuleObject );
		for ( Num1 = 1; Num1 <= Num; ++Num1 ) {
			GetObjectItem( CurrentModuleObject, Num1, Alphas, NumAlpha, Number, NumNumber, IOStat );
			for ( NumA = 1; NumA <= NumAlpha; ++NumA ) {
				if ( SameString( Alphas( NumA ), "DisplayExtraWarnings" ) ) {
					DisplayExtraWarnings = true;
				} else if ( SameString( Alphas( NumA ), "DisplayAdvancedReportVariables" ) ) {
					DisplayAdvancedReportVariables = true;
				} else if ( SameString( Alphas( NumA ), "DisplayAllWarnings" ) ) {
					DisplayAllWarnings = true;
					DisplayExtraWarnings = true;
					DisplayUnusedObjects = true;
					DisplayUnusedSchedules = true;
				} else if ( SameString( Alphas( NumA ), "DisplayUnusedObjects" ) ) {
					DisplayUnusedObjects = true;
				} else if ( SameString( Alphas( NumA ), "DisplayUnusedSchedules" ) ) {
					DisplayUnusedSchedules = true;
				} else if ( SameString( Alphas( NumA ), "DisplayZoneAirHeatBalanceOffBalance" ) ) {
					DisplayZoneAirHeatBalanceOffBalance = true;
				} else if ( SameString( Alphas( NumA ), "DoNotMirrorDetachedShading" ) ) {
					MakeMirroredDetachedShading = false;
				} else if ( SameString( Alphas( NumA ), "DoNotMirrorAttachedShading" ) ) {
					MakeMirroredAttachedShading = false;
				} else if ( SameString( Alphas( NumA ), "IgnoreInteriorWindowTransmission" ) ) {
					IgnoreInteriorWindowTransmission = true;
				} else if ( SameString( Alphas( NumA ), "ReportDuringWarmup" ) ) {
					ReportDuringWarmup = true;
				} else if ( SameString( Alphas( NumA ), "DisplayWeatherMissingDataWarnings" ) ) {
					DisplayWeatherMissingDataWarnings = true;
				} else if ( SameString( Alphas( NumA ), "IgnoreSolarRadiation" ) ) {
					IgnoreSolarRadiation = true;
				} else if ( SameString( Alphas( NumA ), "IgnoreBeamRadiation" ) ) {
					IgnoreBeamRadiation = true;
				} else if ( SameString( Alphas( NumA ), "IgnoreDiffuseRadiation" ) ) {
					IgnoreDiffuseRadiation = true;
				} else if ( SameString( Alphas( NumA ), "DeveloperFlag" ) ) {
					DeveloperFlag = true;
				} else if ( SameString( Alphas( NumA ), "TimingFlag" ) ) {
					TimingFlag = true;
				} else if ( SameString( Alphas( NumA ), "ReportDetailedWarmupConvergence" ) ) {
					ReportDetailedWarmupConvergence = true;
				} else if ( SameString( Alphas( NumA ), "ReportDuringHVACSizingSimulation" ) ) {
					ReportDuringHVACSizingSimulation = true;
				} else if ( SameString( Alphas( NumA ), "CreateMinimalSurfaceVariables" ) ) {
					continue;
					//        CreateMinimalSurfaceVariables=.TRUE.
				} else if ( SameString( Alphas( NumA ), "CreateNormalSurfaceVariables" ) ) {
					continue;
					//        IF (CreateMinimalSurfaceVariables) THEN
					//          CALL ShowWarningError('GetProjectData: '//TRIM(CurrentModuleObject)//'=''//  &
					//             TRIM(Alphas(NumA))//'', prior set=true for this condition reverts to false.')
					//        ENDIF
					//        CreateMinimalSurfaceVariables=.FALSE.
				} else if ( ! Alphas( NumA ).empty() ) {
					ShowWarningError( "GetProjectData: " + CurrentModuleObject + "=\"" + Alphas( NumA ) + "\", Invalid value for field, entered value ignored." );
				}
			}
		}

		CurrentModuleObject = "OutputControl:ReportingTolerances";
		Num = GetNumObjectsFound( CurrentModuleObject );
		if ( Num > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( ! lNumericFieldBlanks( 1 ) ) {
				deviationFromSetPtThresholdHtg = -Number( 1 );
			} else {
				deviationFromSetPtThresholdHtg = -0.2;
			}
			if ( ! lNumericFieldBlanks( 2 ) ) {
				deviationFromSetPtThresholdClg = Number( 2 );
			} else {
				deviationFromSetPtThresholdClg = 0.2;
			}
		}

		DoZoneSizing = false;
		DoSystemSizing = false;
		DoPlantSizing = false;
		DoDesDaySim = true;
		DoWeathSim = true;
		DoHVACSizingSimulation = false;
		HVACSizingSimMaxIterations = 0;
		CurrentModuleObject = "SimulationControl";
		NumRunControl = GetNumObjectsFound( CurrentModuleObject );
		if ( NumRunControl > 0 ) {
			RunControlInInput = true;
			GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( Alphas( 1 ) == "YES" ) DoZoneSizing = true;
			if ( Alphas( 2 ) == "YES" ) DoSystemSizing = true;
			if ( Alphas( 3 ) == "YES" ) DoPlantSizing = true;
			if ( Alphas( 4 ) == "NO" ) DoDesDaySim = false;
			if ( Alphas( 5 ) == "NO" ) DoWeathSim = false;
			if (NumAlpha > 5) {
				if ( Alphas( 6 ) == "YES") DoHVACSizingSimulation = true;
			}
		}
		if ( DDOnly ) {
			DoDesDaySim = true;
			DoWeathSim = false;
		}
		if ( FullAnnualRun ) {
			DoDesDaySim = false;
			DoWeathSim = true;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found getting Project Input" );
		}

		gio::write( OutputFileInits, fmtA ) << "! <Version>, Version ID";
		gio::write( OutputFileInits, Format_721 ) << VersionID;

		gio::write( OutputFileInits, fmtA ) << "! <Timesteps per Hour>, #TimeSteps, Minutes per TimeStep {minutes}";
		gio::write( OutputFileInits, Format_731 ) << NumOfTimeStepInHour << int( MinutesPerTimeStep );

		gio::write( OutputFileInits, fmtA ) << "! <System Convergence Limits>, Minimum System TimeStep {minutes}, Max HVAC Iterations, Minimum Plant Iterations, Maximum Plant Iterations";
		MinInt = MinTimeStepSys * 60.0;
		gio::write( OutputFileInits, Format_733 ) << RoundSigDigits( MinInt ) << RoundSigDigits( MaxIter ) << RoundSigDigits( MinPlantSubIterations ) << RoundSigDigits( MaxPlantSubIterations );

		if ( DoZoneSizing ) {
			Alphas( 1 ) = "Yes";
		} else {
			Alphas( 1 ) = "No";
		}
		if ( DoSystemSizing ) {
			Alphas( 2 ) = "Yes";
		} else {
			Alphas( 2 ) = "No";
		}
		if ( DoPlantSizing ) {
			Alphas( 3 ) = "Yes";
		} else {
			Alphas( 3 ) = "No";
		}
		if ( DoDesDaySim ) {
			Alphas( 4 ) = "Yes";
		} else {
			Alphas( 4 ) = "No";
		}
		if ( DoWeathSim ) {
			Alphas( 5 ) = "Yes";
		} else {
			Alphas( 5 ) = "No";
		}
		if ( DoHVACSizingSimulation ) {
			Alphas( 6 ) = "Yes";
			if ( NumNumber >= 1 ) {
				HVACSizingSimMaxIterations = Number( 1 );
			}
		} else {
			Alphas( 6 ) = "No";
		}

		gio::write( OutputFileInits, fmtA ) << "! <Simulation Control>, Do Zone Sizing, Do System Sizing, Do Plant Sizing, Do Design Days, Do Weather Simulation, Do HVAC Sizing Simulation";
		gio::write( OutputFileInits, Format_741 );
		for ( Num = 1; Num <= 6; ++Num ) {
			gio::write( OutputFileInits, Format_741_1 ) << Alphas( Num );
		}
		gio::write( OutputFileInits );

		gio::write( OutputFileInits, fmtA ) << "! <Output Reporting Tolerances>, Tolerance for Time Heating Setpoint Not Met, Tolerance for Zone Cooling Setpoint Not Met Time";
		gio::write( OutputFileInits, Format_751 ) << RoundSigDigits( std::abs( deviationFromSetPtThresholdHtg ), 3 ) << RoundSigDigits( deviationFromSetPtThresholdClg, 3 );

		//  IF (DisplayExtraWarnings) THEN
		//    Write(OutputFileInits,740)
		//    Write(OutputFileInits,741) (TRIM(Alphas(Num)),Num=1,5)
		//742 Format('! <Display Extra Warnings>, Display Advanced Report Variables, Do Not Mirror Detached Shading')
		//    IF (DisplayAdvancedReportVariables) THEN
		//      NumOut1='Yes'
		//    ELSE
		//      NumOut2='No'
		//    ENDIF
		//    IF (.not. MakeMirroredDetachedShading) THEN
		//      NumOut1='Yes'
		//    ELSE
		//      NumOut2='No'
		//    ENDIF
		//unused0909743 Format(' Display Extra Warnings',2(', ',A))
		//  ENDIF

	}

	void
	CheckForMisMatchedEnvironmentSpecifications()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// In response to CR 7518, this routine will check to see if a proper combination of SimulationControl, RunPeriod,
		// SizingPeriod:*, etc are entered to proceed with a simulation.

		// METHODOLOGY EMPLOYED:
		// For now (8/2008), the routine will query several objects in the input.  And try to produce warnings or
		// fatals as a result.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

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
		int NumZoneSizing;
		int NumSystemSizing;
		int NumPlantSizing;
		int NumDesignDays;
		int NumRunPeriodDesign;
		int NumSizingDays;
		bool WeatherFileAttached;
		bool ErrorsFound;

		ErrorsFound = false;
		NumZoneSizing = GetNumObjectsFound( "Sizing:Zone" );
		NumSystemSizing = GetNumObjectsFound( "Sizing:System" );
		NumPlantSizing = GetNumObjectsFound( "Sizing:Plant" );
		NumDesignDays = GetNumObjectsFound( "SizingPeriod:DesignDay" );
		NumRunPeriodDesign = GetNumObjectsFound( "SizingPeriod:WeatherFileDays" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
		NumSizingDays = NumDesignDays + NumRunPeriodDesign;
		{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); WeatherFileAttached = flags.exists(); }

		if ( RunControlInInput ) {
			if ( DoZoneSizing ) {
				if ( NumZoneSizing > 0 && NumSizingDays == 0 ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Zones has been requested but there are no design environments specified." );
					ShowContinueError( "...Add appropriate SizingPeriod:* objects for your simulation." );
				}
				if ( NumZoneSizing > 0 && NumRunPeriodDesign > 0 && ! WeatherFileAttached ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Zones has been requested; Design period from the weather file requested; but no weather file specified." );
				}
			}
			if ( DoSystemSizing ) {
				if ( NumSystemSizing > 0 && NumSizingDays == 0 ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Systems has been requested but there are no design environments specified." );
					ShowContinueError( "...Add appropriate SizingPeriod:* objects for your simulation." );
				}
				if ( NumSystemSizing > 0 && NumRunPeriodDesign > 0 && ! WeatherFileAttached ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Systems has been requested; Design period from the weather file requested; but no weather file specified." );
				}
			}
			if ( DoPlantSizing ) {
				if ( NumPlantSizing > 0 && NumSizingDays == 0 ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested but there are no design environments specified." );
					ShowContinueError( "...Add appropriate SizingPeriod:* objects for your simulation." );
				}
				if ( NumPlantSizing > 0 && NumRunPeriodDesign > 0 && ! WeatherFileAttached ) {
					ErrorsFound = true;
					ShowSevereError( "CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested; Design period from the weather file requested; but no weather file specified." );
				}
			}
			if ( DoDesDaySim && NumSizingDays == 0 ) {
				ShowWarningError( "CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations, but no design environments specified." );
				ShowContinueError( "...No design environment results produced. For these results, add appropriate SizingPeriod:* objects for your simulation." );
			}
			if ( DoDesDaySim && NumRunPeriodDesign > 0 && ! WeatherFileAttached ) {
				ErrorsFound = true;
				ShowSevereError( "CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations; weather file design environments specified; but no weather file specified." );
			}
			if ( DoWeathSim && ! RunPeriodsInInput ) {
				ShowWarningError( "CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations, but no run periods for weather file specified.  No annual results produced." );
			}
			if ( DoWeathSim && RunPeriodsInInput && ! WeatherFileAttached ) {
				ShowWarningError( "CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations; run periods for weather file specified; but no weather file specified." );
			}
		}
		if ( ! DoDesDaySim && ! DoWeathSim ) {
			ShowWarningError( "\"Do the design day simulations\" and \"Do the weather file simulation\" are both set to \"No\".  No simulations will be performed, and most input will not be read." );
		}
		if ( ! DoZoneSizing && ! DoSystemSizing && ! DoPlantSizing && ! DoDesDaySim && ! DoWeathSim ) {
			ShowSevereError( "All elements of SimulationControl are set to \"No\". No simulations can be done.  Program terminates." );
			ErrorsFound = true;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Program terminates due to preceding conditions." );
		}

	}

	void
	CheckForRequestedReporting()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// EnergyPlus does not automatically produce any results files.  Because of this, users may not request
		// reports and may get confused when nothing is produced.  This routine will provide a warning when
		// results should be produced (either sizing periods or weather files are run) but no reports are
		// requested.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

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
		bool SimPeriods;
		bool ReportingRequested;

		ReportingRequested = false;
		SimPeriods = ( GetNumObjectsFound( "SizingPeriod:DesignDay" ) > 0 || GetNumObjectsFound( "SizingPeriod:WeatherFileDays" ) > 0 || GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" ) > 0 || GetNumObjectsFound( "RunPeriod" ) > 0 );

		if ( ( DoDesDaySim || DoWeathSim ) && SimPeriods ) {
			ReportingRequested = ( GetNumObjectsFound( "Output:Table:SummaryReports" ) > 0 || GetNumObjectsFound( "Output:Table:TimeBins" ) > 0 || GetNumObjectsFound( "Output:Table:Monthly" ) > 0 || GetNumObjectsFound( "Output:Variable" ) > 0 || GetNumObjectsFound( "Output:Meter" ) > 0 || GetNumObjectsFound( "Output:Meter:MeterFileOnly" ) > 0 || GetNumObjectsFound( "Output:Meter:Cumulative" ) > 0 || GetNumObjectsFound( "Output:Meter:Cumulative:MeterFileOnly" ) > 0 );
			// Not testing for : Output:SQLite or Output:EnvironmentalImpactFactors
			if ( ! ReportingRequested ) {
				ShowWarningError( "No reporting elements have been requested. No simulation results produced." );
				ShowContinueError( "...Review requirements such as \"Output:Table:SummaryReports\", \"Output:Table:Monthly\", \"Output:Variable\", \"Output:Meter\" and others." );
			}
		}

	}

	void
	OpenOutputFiles()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine opens all of the input and output files needed for
		// an EnergyPlus run.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataStringGlobals::VerString;

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
		int write_stat;

		// FLOW:
		OutputFileStandard = GetNewUnitNumber();
		StdOutputRecordCount = 0;
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileStandard, DataStringGlobals::outputEsoFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			ShowFatalError( "OpenOutputFiles: Could not open file "+DataStringGlobals::outputEsoFileName+" for output (write)." );
		}
		eso_stream = gio::out_stream( OutputFileStandard );
		gio::write( OutputFileStandard, fmtA ) << "Program Version," + VerString;

		// Open the Initialization Output File
		OutputFileInits = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, DataStringGlobals::outputEioFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			ShowFatalError( "OpenOutputFiles: Could not open file "+DataStringGlobals::outputEioFileName+" for output (write)." );
		}
		gio::write( OutputFileInits, fmtA ) << "Program Version," + VerString;

		// Open the Meters Output File
		OutputFileMeters = GetNewUnitNumber();
		StdMeterRecordCount = 0;
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileMeters, DataStringGlobals::outputMtrFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			ShowFatalError( "OpenOutputFiles: Could not open file "+DataStringGlobals::outputMtrFileName+" for output (write)." );
		}
		mtr_stream = gio::out_stream( OutputFileMeters );
		gio::write( OutputFileMeters, fmtA ) << "Program Version," + VerString;

		// Open the Branch-Node Details Output File
		OutputFileBNDetails = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileBNDetails, DataStringGlobals::outputBndFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			ShowFatalError( "OpenOutputFiles: Could not open file "+DataStringGlobals::outputBndFileName+" for output (write)." );
		}
		gio::write( OutputFileBNDetails, fmtA ) << "Program Version," + VerString;

	}

	void
	CloseOutputFiles()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine closes all of the input and output files needed for
		// an EnergyPlus run.  It also prints the end of data marker for each
		// output file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataOutputs;
		using OutputProcessor::MaxRVariable;
		using OutputProcessor::NumOfRVariable_Setup;
		using OutputProcessor::NumOfRVariable_Sum;
		using OutputProcessor::NumOfRVariable_Meter;
		using OutputProcessor::MaxIVariable;
		using OutputProcessor::NumOfIVariable_Setup;
		using OutputProcessor::NumOfIVariable_Sum;
		using OutputProcessor::NumOfRVariable;
		using OutputProcessor::NumOfIVariable;
		using OutputProcessor::NumEnergyMeters;
		using OutputProcessor::NumVarMeterArrays;
		using OutputProcessor::NumTotalRVariable;
		using OutputProcessor::NumTotalIVariable;
		using OutputProcessor::NumReportList;
		using OutputProcessor::InstMeterCacheSize;
		using OutputReportTabular::maxUniqueKeyCount;
		using OutputReportTabular::MonthlyFieldSetInputCount;
		using SolarShading::maxNumberOfFigures;
		using SolarShading::MAXHCArrayBounds;
		using namespace DataRuntimeLanguage;
		using DataBranchNodeConnections::NumOfNodeConnections;
		using DataBranchNodeConnections::MaxNumOfNodeConnections;
		using DataHeatBalance::CondFDRelaxFactor;
		using DataHeatBalance::HeatTransferAlgosUsed;
		using DataHeatBalance::UseCondFD;
		using DataHeatBalance::CondFDRelaxFactorInput;
		using General::RoundSigDigits;
		using namespace DataSystemVariables; // , ONLY: MaxNumberOfThreads,NumberIntRadThreads,iEnvSetThreads
		using DataSurfaces::MaxVerticesPerSurface;
		using namespace DataTimings;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt EndOfDataFormat( "(\"End of Data\")" ); // Signifies the end of the data block in the output file
		static std::string const ThreadingHeader( "! <Program Control Information:Threads/Parallel Sims>, Threading Supported,Maximum Number of Threads, Env Set Threads (OMP_NUM_THREADS), EP Env Set Threads (EP_OMP_NUM_THREADS), IDF Set Threads, Number of Threads Used (Interior Radiant Exchange), Number Nominal Surfaces, Number Parallel Sims" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EchoInputFile; // found unit number for 'eplusout.audit'
		std::string cEnvSetThreads;
		std::string cepEnvSetThreads;
		std::string cIDFSetThreads;

		EchoInputFile = FindUnitNumber( DataStringGlobals::outputAuditFileName );
		// Record some items on the audit file
		gio::write( EchoInputFile, fmtLD ) << "NumOfRVariable=" << NumOfRVariable_Setup;
		gio::write( EchoInputFile, fmtLD ) << "NumOfRVariable(Total)=" << NumTotalRVariable;
		gio::write( EchoInputFile, fmtLD ) << "NumOfRVariable(Actual)=" << NumOfRVariable;
		gio::write( EchoInputFile, fmtLD ) << "NumOfRVariable(Summed)=" << NumOfRVariable_Sum;
		gio::write( EchoInputFile, fmtLD ) << "NumOfRVariable(Meter)=" << NumOfRVariable_Meter;
		gio::write( EchoInputFile, fmtLD ) << "NumOfIVariable=" << NumOfIVariable_Setup;
		gio::write( EchoInputFile, fmtLD ) << "NumOfIVariable(Total)=" << NumTotalIVariable;
		gio::write( EchoInputFile, fmtLD ) << "NumOfIVariable(Actual)=" << NumOfIVariable;
		gio::write( EchoInputFile, fmtLD ) << "NumOfIVariable(Summed)=" << NumOfIVariable_Sum;
		gio::write( EchoInputFile, fmtLD ) << "MaxRVariable=" << MaxRVariable;
		gio::write( EchoInputFile, fmtLD ) << "MaxIVariable=" << MaxIVariable;
		gio::write( EchoInputFile, fmtLD ) << "NumEnergyMeters=" << NumEnergyMeters;
		gio::write( EchoInputFile, fmtLD ) << "NumVarMeterArrays=" << NumVarMeterArrays;
		gio::write( EchoInputFile, fmtLD ) << "maxUniqueKeyCount=" << maxUniqueKeyCount;
		gio::write( EchoInputFile, fmtLD ) << "maxNumberOfFigures=" << maxNumberOfFigures;
		gio::write( EchoInputFile, fmtLD ) << "MAXHCArrayBounds=" << MAXHCArrayBounds;
		gio::write( EchoInputFile, fmtLD ) << "MaxVerticesPerSurface=" << MaxVerticesPerSurface;
		gio::write( EchoInputFile, fmtLD ) << "NumReportList=" << NumReportList;
		gio::write( EchoInputFile, fmtLD ) << "InstMeterCacheSize=" << InstMeterCacheSize;
		if ( SutherlandHodgman ) {
			gio::write( EchoInputFile, fmtLD ) << "ClippingAlgorithm=SutherlandHodgman";
		} else {
			gio::write( EchoInputFile, fmtLD ) << "ClippingAlgorithm=ConvexWeilerAtherton";
		}
		gio::write( EchoInputFile, fmtLD ) << "MonthlyFieldSetInputCount=" << MonthlyFieldSetInputCount;
		gio::write( EchoInputFile, fmtLD ) << "NumConsideredOutputVariables=" << NumConsideredOutputVariables;
		gio::write( EchoInputFile, fmtLD ) << "MaxConsideredOutputVariables=" << MaxConsideredOutputVariables;

		gio::write( EchoInputFile, fmtLD ) << "numActuatorsUsed=" << numActuatorsUsed;
		gio::write( EchoInputFile, fmtLD ) << "numEMSActuatorsAvailable=" << numEMSActuatorsAvailable;
		gio::write( EchoInputFile, fmtLD ) << "maxEMSActuatorsAvailable=" << maxEMSActuatorsAvailable;
		gio::write( EchoInputFile, fmtLD ) << "numInternalVariablesUsed=" << NumInternalVariablesUsed;
		gio::write( EchoInputFile, fmtLD ) << "numEMSInternalVarsAvailable=" << numEMSInternalVarsAvailable;
		gio::write( EchoInputFile, fmtLD ) << "maxEMSInternalVarsAvailable=" << maxEMSInternalVarsAvailable;

		gio::write( EchoInputFile, fmtLD ) << "NumOfNodeConnections=" << NumOfNodeConnections;
		gio::write( EchoInputFile, fmtLD ) << "MaxNumOfNodeConnections=" << MaxNumOfNodeConnections;
#ifdef EP_Count_Calls
		gio::write( EchoInputFile, fmtLD ) << "NumShadow_Calls=" << NumShadow_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumShadowAtTS_Calls=" << NumShadowAtTS_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumClipPoly_Calls=" << NumClipPoly_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumInitSolar_Calls=" << NumInitSolar_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumAnisoSky_Calls=" << NumAnisoSky_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumDetPolyOverlap_Calls=" << NumDetPolyOverlap_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumCalcPerSolBeam_Calls=" << NumCalcPerSolBeam_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumDetShadowCombs_Calls=" << NumDetShadowCombs_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntSolarDist_Calls=" << NumIntSolarDist_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntRadExchange_Calls=" << NumIntRadExchange_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntRadExchangeZ_Calls=" << NumIntRadExchangeZ_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntRadExchangeMain_Calls=" << NumIntRadExchangeMain_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntRadExchangeOSurf_Calls=" << NumIntRadExchangeOSurf_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumIntRadExchangeISurf_Calls=" << NumIntRadExchangeISurf_Calls;
		gio::write( EchoInputFile, fmtLD ) << "NumMaxInsideSurfIterations=" << NumMaxInsideSurfIterations;
		gio::write( EchoInputFile, fmtLD ) << "NumCalcScriptF_Calls=" << NumCalcScriptF_Calls;
#endif

		gio::write( OutputFileStandard, EndOfDataFormat );
		gio::write( OutputFileStandard, fmtLD ) << "Number of Records Written=" << StdOutputRecordCount;
		if ( StdOutputRecordCount > 0 ) {
			gio::close( OutputFileStandard );
		} else {
			{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileStandard, flags ); }
		}
		eso_stream = nullptr;

		if ( any_eq( HeatTransferAlgosUsed, UseCondFD ) ) { // echo out relaxation factor, it may have been changed by the program
			gio::write( OutputFileInits, fmtA ) << "! <ConductionFiniteDifference Numerical Parameters>, Starting Relaxation Factor, Final Relaxation Factor";
			gio::write( OutputFileInits, fmtA ) << "ConductionFiniteDifference Numerical Parameters, " + RoundSigDigits( CondFDRelaxFactorInput, 3 ) + ", " + RoundSigDigits( CondFDRelaxFactor, 3 );
		}
		// Report number of threads to eio file
		if ( Threading ) {
			if ( iEnvSetThreads == 0 ) {
				cEnvSetThreads = "Not Set";
			} else {
				cEnvSetThreads = RoundSigDigits( iEnvSetThreads );
			}
			if ( iepEnvSetThreads == 0 ) {
				cepEnvSetThreads = "Not Set";
			} else {
				cepEnvSetThreads = RoundSigDigits( iepEnvSetThreads );
			}
			if ( iIDFSetThreads == 0 ) {
				cIDFSetThreads = "Not Set";
			} else {
				cIDFSetThreads = RoundSigDigits( iIDFSetThreads );
			}
			if ( lnumActiveSims ) {
				gio::write( OutputFileInits, fmtA ) << ThreadingHeader;
				gio::write( OutputFileInits, fmtA ) << "Program Control:Threads/Parallel Sims, Yes," + RoundSigDigits( MaxNumberOfThreads ) + ", " + cEnvSetThreads + ", " + cepEnvSetThreads + ", " + cIDFSetThreads + ", " + RoundSigDigits( NumberIntRadThreads ) + ", " + RoundSigDigits( iNominalTotSurfaces ) + ", " + RoundSigDigits( inumActiveSims );
			} else {
				gio::write( OutputFileInits, fmtA ) << ThreadingHeader;
				gio::write( OutputFileInits, fmtA ) << "Program Control:Threads/Parallel Sims, Yes," + RoundSigDigits( MaxNumberOfThreads ) + ", " + cEnvSetThreads + ", " + cepEnvSetThreads + ", " + cIDFSetThreads + ", " + RoundSigDigits( NumberIntRadThreads ) + ", " + RoundSigDigits( iNominalTotSurfaces ) + ", N/A";
			}
		} else { // no threading
			if ( lnumActiveSims ) {
				gio::write( OutputFileInits, fmtA ) << ThreadingHeader;
				gio::write( OutputFileInits, fmtA ) << "Program Control:Threads/Parallel Sims, No," + RoundSigDigits( MaxNumberOfThreads ) + ", N/A, N/A, N/A, N/A, N/A, " + RoundSigDigits( inumActiveSims );
			} else {
				gio::write( OutputFileInits, fmtA ) << ThreadingHeader;
				gio::write( OutputFileInits, fmtA ) << "Program Control:Threads/Parallel Sims, No," + RoundSigDigits( MaxNumberOfThreads ) + ", N/A, N/A, N/A, N/A, N/A, N/A";
			}
		}

		// Close the Initialization Output File
		gio::write( OutputFileInits, EndOfDataFormat );
		gio::close( OutputFileInits );

		// Close the Meters Output File
		gio::write( OutputFileMeters, EndOfDataFormat );
		gio::write( OutputFileMeters, fmtLD ) << "Number of Records Written=" << StdMeterRecordCount;
		if ( StdMeterRecordCount > 0 ) {
			gio::close( OutputFileMeters );
		} else {
			{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeters, flags ); }
		}
		mtr_stream = nullptr;

	}

	void
	SetupSimulation( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith/L. Lawrie
		//       DATE WRITTEN   May 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  execute a few time steps of a simulation to facilitate setting up model
		//  developed to resolve reverse DD problems caused be the differences
		//  that stem from setup and information gathering that occurs during the first pass.

		// METHODOLOGY EMPLOYED:
		// Using global flag (kickoff simulation), only a few time steps are executed.
		// global flag is used in other parts of simulation to terminate quickly.

		// REFERENCES:
		// na

		// Using/Aliasing
		using ExteriorEnergyUse::ManageExteriorEnergyUse;
		using DataEnvironment::EndMonthFlag;
		using DataEnvironment::EnvironmentName;
		using InputProcessor::GetNumRangeCheckErrorsFound;
		using CostEstimateManager::SimCostEstimate;
		using General::TrimSigDigits;
		using namespace DataTimings;
		using PlantPipingSystemsManager::SimulateGroundDomains;
		using PlantPipingSystemsManager::CheckIfAnySlabs;
		using PlantPipingSystemsManager::CheckIfAnyBasements;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Available( false ); // an environment is available to process
		//  integer :: env_iteration=0
		//  CHARACTER(len=32) :: cEnvChar

		//  return  ! remove comment to do 'old way'

		Available = true;

		while ( Available ) { // do for each environment

			GetNextEnvironment( Available, ErrorsFound );

			if ( ! Available ) break;
			if ( ErrorsFound ) break;

			BeginEnvrnFlag = true;
			EndEnvrnFlag = false;
			EndMonthFlag = false;
			WarmupFlag = true;
			DayOfSim = 0;

			++DayOfSim;
			BeginDayFlag = true;
			EndDayFlag = false;

			HourOfDay = 1;

			BeginHourFlag = true;
			EndHourFlag = false;

			TimeStep = 1;

			if ( DeveloperFlag ) DisplayString( "Initializing Simulation - timestep 1:" + EnvironmentName );

			BeginTimeStepFlag = true;

			ManageWeather();

			ManageExteriorEnergyUse();

			ManageHeatBalance();

			//  After the first iteration of HeatBalance, all the 'input' has been gotten
			if ( BeginFullSimFlag ) {
				if ( GetNumRangeCheckErrorsFound() > 0 ) {
					ShowFatalError( "Out of \"range\" values found in input" );
				}
			}

			BeginHourFlag = false;
			BeginDayFlag = false;
			BeginEnvrnFlag = false;
			BeginSimFlag = false;
			BeginFullSimFlag = false;

			//          ! do another timestep=1
			if ( DeveloperFlag ) DisplayString( "Initializing Simulation - 2nd timestep 1:" + EnvironmentName );

			ManageWeather();

			ManageExteriorEnergyUse();

			ManageHeatBalance();

			//         do an end of day, end of environment time step

			HourOfDay = 24;
			TimeStep = NumOfTimeStepInHour;
			EndEnvrnFlag = true;

			if ( DeveloperFlag ) DisplayString( "Initializing Simulation - hour 24 timestep 1:" + EnvironmentName );
			ManageWeather();

			ManageExteriorEnergyUse();

			ManageHeatBalance();

		} // ... End environment loop.

		if ( AnySlabsInModel || AnyBasementsInModel ) {
			SimulateGroundDomains( true );
		}

		if ( ! ErrorsFound ) SimCostEstimate(); // basically will get and check input
		if ( ErrorsFound ) ShowFatalError( "Previous Conditions cause program termination." );

	}

	void
	ReportNodeConnections()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine 'reports' the NodeConnection data structure.  It groups the
		// report/dump by parent, non-parent objects.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using namespace DataBranchNodeConnections;
		using DataGlobals::OutputFileBNDetails;
		using DataLoopNode::NumOfNodes;
		using DataLoopNode::NodeID;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int Loop1;
		int NumParents;
		int NumNonParents;
		int NumNonConnected;
		std::string ChrOut;
		bool ParentComponentFound;

		// Formats
		static gio::Fmt Format_701( "(A)" );
		static gio::Fmt Format_702( "('! <#',A,' Node Connections>,<Number of ',A,' Node Connections>')" );
		static gio::Fmt Format_703( "('! <',A,' Node Connection>,<Node Name>,<Node ObjectType>,<Node ObjectName>,','<Node ConnectionType>,<Node FluidStream>')" );
		static gio::Fmt Format_705( "('! <#NonConnected Nodes>,<Number of NonConnected Nodes>',/,' #NonConnected Nodes,',A)" );
		static gio::Fmt Format_706( "('! <NonConnected Node>,<NonConnected Node Number>,<NonConnected Node Name>')" );

		NonConnectedNodes.dimension( NumOfNodes, true );

		NumParents = 0;
		NumNonParents = 0;
		for ( Loop = 1; Loop <= NumOfNodeConnections; ++Loop ) {
			if ( NodeConnections( Loop ).ObjectIsParent ) continue;
			++NumNonParents;
		}
		NumParents = NumOfNodeConnections - NumNonParents;
		ParentNodeList.allocate( NumParents );

		//  Do Parent Objects
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_702 ) << "Parent" << "Parent";
		gio::write( ChrOut, fmtLD ) << NumParents;
		gio::write( OutputFileBNDetails, Format_701 ) << " #Parent Node Connections," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_703 ) << "Parent";

		for ( Loop = 1; Loop <= NumOfNodeConnections; ++Loop ) {
			if ( ! NodeConnections( Loop ).ObjectIsParent ) continue;
			NonConnectedNodes( NodeConnections( Loop ).NodeNumber ) = false;
			gio::write( ChrOut, fmtLD ) << NodeConnections( Loop ).FluidStream;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << " Parent Node Connection," + NodeConnections( Loop ).NodeName + ',' + NodeConnections( Loop ).ObjectType + ',' + NodeConnections( Loop ).ObjectName + ',' + NodeConnections( Loop ).ConnectionType + ',' + ChrOut;
			// Build ParentNodeLists
			if ( SameString( NodeConnections( Loop ).ConnectionType, "Inlet" ) || SameString( NodeConnections( Loop ).ConnectionType, "Outlet" ) ) {
				ParentComponentFound = false;
				for ( Loop1 = 1; Loop1 <= NumOfActualParents; ++Loop1 ) {
					if ( ParentNodeList( Loop1 ).CType != NodeConnections( Loop ).ObjectType || ParentNodeList( Loop1 ).CName != NodeConnections( Loop ).ObjectName ) continue;
					ParentComponentFound = true;
					{ auto const SELECT_CASE_var( MakeUPPERCase( NodeConnections( Loop ).ConnectionType ) );
					if ( SELECT_CASE_var == "INLET" ) {
						ParentNodeList( Loop1 ).InletNodeName = NodeConnections( Loop ).NodeName;
					} else if ( SELECT_CASE_var == "OUTLET" ) {
						ParentNodeList( Loop1 ).OutletNodeName = NodeConnections( Loop ).NodeName;
					}}
				}
				if ( ! ParentComponentFound ) {
					++NumOfActualParents;
					ParentNodeList( NumOfActualParents ).CType = NodeConnections( Loop ).ObjectType;
					ParentNodeList( NumOfActualParents ).CName = NodeConnections( Loop ).ObjectName;
					{ auto const SELECT_CASE_var( MakeUPPERCase( NodeConnections( Loop ).ConnectionType ) );
					if ( SELECT_CASE_var == "INLET" ) {
						ParentNodeList( NumOfActualParents ).InletNodeName = NodeConnections( Loop ).NodeName;
					} else if ( SELECT_CASE_var == "OUTLET" ) {
						ParentNodeList( NumOfActualParents ).OutletNodeName = NodeConnections( Loop ).NodeName;
					}}
				}
			}
		}

		//  Do non-Parent Objects
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_702 ) << "Non-Parent" << "Non-Parent";
		gio::write( ChrOut, fmtLD ) << NumNonParents;
		gio::write( OutputFileBNDetails, Format_701 ) << " #Non-Parent Node Connections," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_703 ) << "Non-Parent";

		for ( Loop = 1; Loop <= NumOfNodeConnections; ++Loop ) {
			if ( NodeConnections( Loop ).ObjectIsParent ) continue;
			NonConnectedNodes( NodeConnections( Loop ).NodeNumber ) = false;
			gio::write( ChrOut, fmtLD ) << NodeConnections( Loop ).FluidStream;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << " Non-Parent Node Connection," + NodeConnections( Loop ).NodeName + ',' + NodeConnections( Loop ).ObjectType + ',' + NodeConnections( Loop ).ObjectName + ',' + NodeConnections( Loop ).ConnectionType + ',' + ChrOut;
		}

		NumNonConnected = 0;
		for ( Loop = 1; Loop <= NumOfNodes; ++Loop ) {
			if ( NonConnectedNodes( Loop ) ) ++NumNonConnected;
		}

		if ( NumNonConnected > 0 ) {
			gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
			gio::write( ChrOut, fmtLD ) << NumNonConnected;
			gio::write( OutputFileBNDetails, Format_705 ) << stripped( ChrOut );
			gio::write( OutputFileBNDetails, Format_706 );
			for ( Loop = 1; Loop <= NumOfNodes; ++Loop ) {
				if ( ! NonConnectedNodes( Loop ) ) continue;
				gio::write( ChrOut, fmtLD ) << Loop;
				strip( ChrOut );
				gio::write( OutputFileBNDetails, Format_701 ) << " NonConnected Node," + ChrOut + ',' + NodeID( Loop );
			}
		}

		NonConnectedNodes.deallocate();

	}

	void
	ReportLoopConnections()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2001
		//       MODIFIED       March 2003; added other reporting
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports on the node connections in various parts of the
		// HVAC syste: Component Sets, Air Loop, Plant and Condenser Loop, Supply and
		// return air paths, controlled zones.
		// This information should be useful in diagnosing node connection input errors.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using namespace DataAirLoop;
		using namespace DataBranchNodeConnections;
		using DataLoopNode::NumOfNodes;
		using DataLoopNode::NodeID;
		using namespace DataHVACGlobals;
		using namespace DataPlant;
		using namespace DataZoneEquipment;
		using OutAirNodeManager::OutsideAirNodeList;
		using OutAirNodeManager::NumOutsideAirNodes;
		using DataErrorTracking::AbortProcessing; // used here to turn off Node Connection Error reporting
		using DataErrorTracking::AskForConnectionsReport;
		using DualDuct::ReportDualDuctConnections;
		using DataGlobals::OutputFileBNDetails;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const errstring( "**error**" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string ChrOut;
		std::string ChrOut2;
		std::string ChrOut3;
		std::string LoopString;
		std::string ChrName;
		int Count;
		int Count1;
		int LoopSideNum;
		int Num;
		static bool WarningOut( true );
		int NumOfControlledZones;

		// Formats
		static gio::Fmt Format_700( "('! <#Component Sets>,<Number of Component Sets>')" );
		static gio::Fmt Format_701( "(A)" );
		static gio::Fmt Format_702( "('! <Component Set>,<Component Set Count>,<Parent Object Type>,<Parent Object Name>,','<Component Type>,<Component Name>,<Inlet Node ID>,<Outlet Node ID>,<Description>')" );
		static gio::Fmt Format_707( "(1X,A)" );
		static gio::Fmt Format_713( "(A)" );
		static gio::Fmt Format_720( "('! <#Zone Equipment Lists>,<Number of Zone Equipment Lists>')" );
		static gio::Fmt Format_721( "(A)" );
		static gio::Fmt Format_722( "('! <Zone Equipment List>,<Zone Equipment List Count>,<Zone Equipment List Name>,<Zone Name>,<Number of Components>')" );
		static gio::Fmt Format_723( "('! <Zone Equipment Component>,<Component Count>,<Component Type>,<Component Name>,','<Zone Name>,<Heating Priority>,<Cooling Priority>')" );

		// Report outside air node names on the Branch-Node Details file
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_701 ) << "! #Outdoor Air Nodes,<Number of Outdoor Air Nodes>";
		gio::write( ChrOut, fmtLD ) << NumOutsideAirNodes;
		gio::write( OutputFileBNDetails, Format_701 ) << " #Outdoor Air Nodes," + stripped( ChrOut );
		if ( NumOutsideAirNodes > 0 ) {
			gio::write( OutputFileBNDetails, Format_701 ) << "! <Outdoor Air Node>,<NodeNumber>,<Node Name>";
		}
		for ( Count = 1; Count <= NumOutsideAirNodes; ++Count ) {
			gio::write( ChrOut, fmtLD ) << OutsideAirNodeList( Count );
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << " Outdoor Air Node," + ChrOut + ',' + NodeID( OutsideAirNodeList( Count ) );
		}
		// Component Sets
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_700 );
		gio::write( ChrOut, fmtLD ) << NumCompSets;
		gio::write( OutputFileBNDetails, Format_701 ) << " #Component Sets," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_702 );

		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			gio::write( ChrOut, fmtLD ) << Count;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << " Component Set," + ChrOut + ',' + CompSets( Count ).ParentCType + ',' + CompSets( Count ).ParentCName + ',' + CompSets( Count ).CType + ',' + CompSets( Count ).CName + ',' + CompSets( Count ).InletNodeName + ',' + CompSets( Count ).OutletNodeName + ',' + CompSets( Count ).Description;

			if ( CompSets( Count ).ParentCType == "UNDEFINED" || CompSets( Count ).InletNodeName == "UNDEFINED" || CompSets( Count ).OutletNodeName == "UNDEFINED" ) {
				if ( AbortProcessing && WarningOut ) {
					ShowWarningError( "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have been retrieved." );
					WarningOut = false;
				}
				if (CompSets(Count).CType == "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE") continue;
				ShowWarningError( "Node Connection Error for object " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
				ShowContinueError( "  " + CompSets( Count ).Description + " not on any Branch or Parent Object" );
				ShowContinueError( "  Inlet Node : " + CompSets( Count ).InletNodeName );
				ShowContinueError( "  Outlet Node: " + CompSets( Count ).OutletNodeName );
				++NumNodeConnectionErrors;
				if ( SameString( CompSets( Count ).CType, "SolarCollector:UnglazedTranspired" ) ) {
					ShowContinueError( "This report does not necessarily indicate a problem for a MultiSystem Transpired Collector" );
				}
			}
			if ( CompSets( Count ).Description == "UNDEFINED" ) {
				if ( AbortProcessing && WarningOut ) {
					ShowWarningError( "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have been retrieved." );
					WarningOut = false;
				}
				if (CompSets(Count).CType == "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE") continue;
				ShowWarningError( "Potential Node Connection Error for object " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
				ShowContinueError( "  Node Types are still UNDEFINED -- See Branch/Node Details file for further information" );
				ShowContinueError( "  Inlet Node : " + CompSets( Count ).InletNodeName );
				ShowContinueError( "  Outlet Node: " + CompSets( Count ).OutletNodeName );
				++NumNodeConnectionErrors;

			}
		}

		for ( Count = 1; Count <= NumCompSets; ++Count ) {
			for ( Count1 = Count + 1; Count1 <= NumCompSets; ++Count1 ) {
				if ( CompSets( Count ).CType != CompSets( Count1 ).CType ) continue;
				if ( CompSets( Count ).CName != CompSets( Count1 ).CName ) continue;
				if ( CompSets( Count ).InletNodeName != CompSets( Count1 ).InletNodeName ) continue;
				if ( CompSets( Count ).OutletNodeName != CompSets( Count1 ).OutletNodeName ) continue;
				if ( AbortProcessing && WarningOut ) {
					ShowWarningError( "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have been retrieved." );
					WarningOut = false;
				}
				ShowWarningError( "Component plus inlet/outlet node pair used more than once:" );
				ShowContinueError( "  Component  : " + CompSets( Count ).CType + ", name=" + CompSets( Count ).CName );
				ShowContinueError( "  Inlet Node : " + CompSets( Count ).InletNodeName );
				ShowContinueError( "  Outlet Node: " + CompSets( Count ).OutletNodeName );
				ShowContinueError( "  Used by    : " + CompSets( Count ).ParentCType + ' ' + CompSets( Count ).ParentCName );
				ShowContinueError( "  and  by    : " + CompSets( Count1 ).ParentCType + ' ' + CompSets( Count1 ).ParentCName );
				++NumNodeConnectionErrors;
			}
		}
		//  Plant Loops
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( ChrOut, fmtLD ) << NumPlantLoops;
		strip( ChrOut );
		gio::write( OutputFileBNDetails, Format_713 ) << "! <# Plant Loops>,<Number of Plant Loops>";
		gio::write( OutputFileBNDetails, Format_707 ) << "#Plant Loops," + ChrOut;
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop>,<Plant Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Branch>,<Outlet Branch>,<Loop Name>,<Loop Type>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop Type>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop Supply Connection>,<Plant Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Plant Loop Return Connection>,<Plant Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>";
		for ( Count = 1; Count <= NumPlantLoops; ++Count ) {
			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				//  Plant Supply Side Loop
				// Demandside and supplyside is parametrized in DataPlant
				if ( LoopSideNum == DemandSide ) {
					LoopString = "Demand";
				} else if ( LoopSideNum == SupplySide ) {
					LoopString = "Supply";
				}

				gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop," + PlantLoop( Count ).Name + ',' + LoopString + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).NodeNameIn + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).BranchList + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).ConnectList;
				//  Plant Supply Side Splitter
				for ( Num = 1; Num <= PlantLoop( Count ).LoopSide( LoopSideNum ).NumSplitters; ++Num ) {
					if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Exists ) {
						gio::write( ChrOut, fmtLD ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).TotalOutletNodes;
						gio::write( OutputFileBNDetails, Format_713 ) << "   Plant Loop Connector,Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString + ',' + stripped( ChrOut );
						for ( Count1 = 1; Count1 <= PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).TotalOutletNodes; ++Count1 ) {
							gio::write( ChrOut, fmtLD ) << Count1;
							ChrOut2 = BlankString;
							ChrOut3 = BlankString;
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumIn <= 0 ) {
								ChrOut2 = errstring;
							}
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumOut( Count1 ) <= 0 ) {
								ChrOut3 = errstring;
							}
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << "     Plant Loop Connector Branches," + stripped( ChrOut ) + ",Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ','; }
							if ( ChrOut2 != errstring ) {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumIn ).Name + ','; }
							} else {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << ChrOut2 + ','; }
							}
							if ( ChrOut3 != errstring ) {
								gio::write( OutputFileBNDetails, Format_713 ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumOut( Count1 ) ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							} else {
								gio::write( OutputFileBNDetails, Format_713 ) << ChrOut3 + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							}
							gio::write( OutputFileBNDetails, Format_713 ) << "     Plant Loop Connector Nodes,   " + stripped( ChrOut ) + ",Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).NodeNameIn + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).NodeNameOut( Count1 ) + ',' + PlantLoop( Count ).Name + ',' + LoopString;
						}
					}
				}
				//  Plant Supply Side Mixer
				for ( Num = 1; Num <= PlantLoop( Count ).LoopSide( LoopSideNum ).NumMixers; ++Num ) {
					if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Exists ) {
						gio::write( ChrOut, fmtLD ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).TotalInletNodes;
						gio::write( OutputFileBNDetails, Format_713 ) << "   Plant Loop Connector,Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString + ',' + stripped( ChrOut ); //',Supply,'//  &
						for ( Count1 = 1; Count1 <= PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).TotalInletNodes; ++Count1 ) {
							gio::write( ChrOut, fmtLD ) << Count1;
							ChrOut2 = BlankString;
							ChrOut3 = BlankString;
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumIn( Count1 ) <= 0 ) {
								ChrOut2 = errstring;
							}
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumOut <= 0 ) {
								ChrOut3 = errstring;
							}
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << "     Plant Loop Connector Branches," + stripped( ChrOut ) + ",Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ','; }
							if ( ChrOut2 != errstring ) {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumIn( Count1 ) ).Name + ','; }
							} else {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << ChrOut2 + ','; }
							}
							if ( ChrOut3 != errstring ) {
								gio::write( OutputFileBNDetails, Format_713 ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumOut ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							} else {
								gio::write( OutputFileBNDetails, Format_713 ) << ChrOut3 + ',' + PlantLoop( Count ).Name + ",Supply";
							}
							gio::write( OutputFileBNDetails, Format_713 ) << "     Plant Loop Connector Nodes,   " + stripped( ChrOut ) + ",Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).NodeNameIn( Count1 ) + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).NodeNameOut + ',' + PlantLoop( Count ).Name + ',' + LoopString;
						}
					}
				}
			}
			gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop Supply Connection," + PlantLoop( Count ).Name + ',' + PlantLoop( Count ).LoopSide( SupplySide ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( DemandSide ).NodeNameIn;
			gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop Return Connection," + PlantLoop( Count ).Name + ',' + PlantLoop( Count ).LoopSide( DemandSide ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( SupplySide ).NodeNameIn;

		} //  Plant Demand Side Loop

		//  Condenser Loops
		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( ChrOut, fmtLD ) << NumCondLoops;
		strip( ChrOut );
		gio::write( OutputFileBNDetails, Format_713 ) << "! <# Condenser Loops>,<Number of Condenser Loops>";
		gio::write( OutputFileBNDetails, Format_707 ) << "#Condenser Loops," + ChrOut;
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop>,<Condenser Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Branch>,<Outlet Branch>,<Loop Name>,<Loop Type>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop Type>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop Supply Connection>,<Condenser Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <Condenser Loop Return Connection>,<Condenser Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>";

		for ( Count = NumPlantLoops + 1; Count <= TotNumLoops; ++Count ) {
			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				//  Plant Supply Side Loop
				// Demandside and supplyside is parametrized in DataPlant
				if ( LoopSideNum == DemandSide ) {
					LoopString = "Demand";
				} else if ( LoopSideNum == SupplySide ) {
					LoopString = "Supply";
				}

				gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop," + PlantLoop( Count ).Name + ',' + LoopString + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).NodeNameIn + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).BranchList + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).ConnectList;
				//  Plant Supply Side Splitter
				for ( Num = 1; Num <= PlantLoop( Count ).LoopSide( LoopSideNum ).NumSplitters; ++Num ) {
					if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Exists ) {
						gio::write( ChrOut, fmtLD ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).TotalOutletNodes;
						gio::write( OutputFileBNDetails, Format_713 ) << "   Plant Loop Connector,Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString + ',' + stripped( ChrOut );
						for ( Count1 = 1; Count1 <= PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).TotalOutletNodes; ++Count1 ) {
							gio::write( ChrOut, fmtLD ) << Count1;
							ChrOut2 = BlankString;
							ChrOut3 = BlankString;
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumIn <= 0 ) {
								ChrOut2 = errstring;
							}
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumOut( Count1 ) <= 0 ) {
								ChrOut3 = errstring;
							}
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << "     Plant Loop Connector Branches," + stripped( ChrOut ) + ",Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ','; }
							if ( ChrOut2 != errstring ) {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumIn ).Name + ','; }
							} else {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << ChrOut2 + ','; }
							}
							if ( ChrOut3 != errstring ) {
								gio::write( OutputFileBNDetails, Format_713 ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).BranchNumOut( Count1 ) ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							} else {
								gio::write( OutputFileBNDetails, Format_713 ) << ChrOut3 + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							}
							gio::write( OutputFileBNDetails, Format_713 ) << "     Plant Loop Connector Nodes,   " + stripped( ChrOut ) + ",Splitter," + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).Name + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).NodeNameIn + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Splitter( Num ).NodeNameOut( Count1 ) + ',' + PlantLoop( Count ).Name + ',' + LoopString;
						}
					}
				}
				//  Plant Supply Side Mixer
				for ( Num = 1; Num <= PlantLoop( Count ).LoopSide( LoopSideNum ).NumMixers; ++Num ) {
					if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Exists ) {
						gio::write( ChrOut, fmtLD ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).TotalInletNodes;
						gio::write( OutputFileBNDetails, Format_713 ) << "   Plant Loop Connector,Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString + ',' + stripped( ChrOut ); //',Supply,'//  &
						for ( Count1 = 1; Count1 <= PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).TotalInletNodes; ++Count1 ) {
							gio::write( ChrOut, fmtLD ) << Count1;
							ChrOut2 = BlankString;
							ChrOut3 = BlankString;
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumIn( Count1 ) <= 0 ) {
								ChrOut2 = errstring;
							}
							if ( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumOut <= 0 ) {
								ChrOut3 = errstring;
							}
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << "     Plant Loop Connector Branches," + stripped( ChrOut ) + ",Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ','; }
							if ( ChrOut2 != errstring ) {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumIn( Count1 ) ).Name + ','; }
							} else {
								{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_713, flags ) << ChrOut2 + ','; }
							}
							if ( ChrOut3 != errstring ) {
								gio::write( OutputFileBNDetails, Format_713 ) << PlantLoop( Count ).LoopSide( LoopSideNum ).Branch( PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).BranchNumOut ).Name + ',' + PlantLoop( Count ).Name + ',' + LoopString;
							} else {
								gio::write( OutputFileBNDetails, Format_713 ) << ChrOut3 + ',' + PlantLoop( Count ).Name + ",Supply";
							}
							gio::write( OutputFileBNDetails, Format_713 ) << "     Plant Loop Connector Nodes,   " + stripped( ChrOut ) + ",Mixer," + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).Name + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).NodeNameIn( Count1 ) + ',' + PlantLoop( Count ).LoopSide( LoopSideNum ).Mixer( Num ).NodeNameOut + ',' + PlantLoop( Count ).Name + ',' + LoopString;
						}
					}
				}
			}
			gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop Supply Connection," + PlantLoop( Count ).Name + ',' + PlantLoop( Count ).LoopSide( SupplySide ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( DemandSide ).NodeNameIn;
			gio::write( OutputFileBNDetails, Format_713 ) << " Plant Loop Return Connection," + PlantLoop( Count ).Name + ',' + PlantLoop( Count ).LoopSide( DemandSide ).NodeNameOut + ',' + PlantLoop( Count ).LoopSide( SupplySide ).NodeNameIn;

		} //  Plant Demand Side Loop

		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		NumOfControlledZones = 0;
		for ( Count = 1; Count <= NumOfZones; ++Count ) {
			if ( ! allocated( ZoneEquipConfig ) ) continue;
			if ( ZoneEquipConfig( Count ).IsControlled ) ++NumOfControlledZones;
		}
		gio::write( ChrOut, fmtLD ) << NumOfControlledZones;
		strip( ChrOut );
		if ( NumOfControlledZones > 0 ) {
			gio::write( OutputFileBNDetails, Format_713 ) << "! <# Controlled Zones>,<Number of Controlled Zones>";
			gio::write( OutputFileBNDetails, Format_707 ) << "#Controlled Zones," + ChrOut;
			gio::write( OutputFileBNDetails, Format_713 ) << "! <Controlled Zone>,<Controlled Zone Name>,<Equip List Name>,<Control List Name>,<Zone Node Name>,<Return Air Node Name>,<# Inlet Nodes>,<# Exhaust Nodes>";
			gio::write( OutputFileBNDetails, Format_713 ) << "! <Controlled Zone Inlet>,<Inlet Node Count>,<Controlled Zone Name>,<Supply Air Inlet Node Name>,<SD Sys:Cooling/Heating [DD:Cooling] Inlet Node Name>,<DD Sys:Heating Inlet Node Name>";
			gio::write( OutputFileBNDetails, Format_713 ) << "! <Controlled Zone Exhaust>,<Exhaust Node Count>,<Controlled Zone Name>,<Exhaust Air Node Name>";
			for ( Count = 1; Count <= NumOfZones; ++Count ) {
				if ( ! ZoneEquipConfig( Count ).IsControlled ) continue;
				gio::write( ChrOut, fmtLD ) << ZoneEquipConfig( Count ).NumInletNodes;
				gio::write( ChrOut2, fmtLD ) << ZoneEquipConfig( Count ).NumExhaustNodes;
				strip( ChrOut );
				strip( ChrOut2 );
				gio::write( OutputFileBNDetails, Format_713 ) << " Controlled Zone," + ZoneEquipConfig( Count ).ZoneName + ',' + ZoneEquipConfig( Count ).EquipListName + ',' + ZoneEquipConfig( Count ).ControlListName + ',' + NodeID( ZoneEquipConfig( Count ).ZoneNode ) + ',' + NodeID( ZoneEquipConfig( Count ).ReturnAirNode ) + ',' + ChrOut + ',' + ChrOut2;
				for ( Count1 = 1; Count1 <= ZoneEquipConfig( Count ).NumInletNodes; ++Count1 ) {
					gio::write( ChrOut, fmtLD ) << Count1;
					strip( ChrOut );
					ChrName = NodeID( ZoneEquipConfig( Count ).AirDistUnitHeat( Count1 ).InNode );
					if ( ChrName == "Undefined" ) ChrName = "N/A";
					gio::write( OutputFileBNDetails, Format_713 ) << "   Controlled Zone Inlet," + ChrOut + ',' + ZoneEquipConfig( Count ).ZoneName + ',' + NodeID( ZoneEquipConfig( Count ).InletNode( Count1 ) ) + ',' + NodeID( ZoneEquipConfig( Count ).AirDistUnitCool( Count1 ).InNode ) + ',' + ChrName;
				}
				for ( Count1 = 1; Count1 <= ZoneEquipConfig( Count ).NumExhaustNodes; ++Count1 ) {
					gio::write( ChrOut, fmtLD ) << Count1;
					strip( ChrOut );
					gio::write( OutputFileBNDetails, Format_713 ) << "   Controlled Zone Exhaust," + ChrOut + ',' + ZoneEquipConfig( Count ).ZoneName + ',' + NodeID( ZoneEquipConfig( Count ).ExhaustNode( Count1 ) );
				}
			}

			//Report Zone Equipment Lists to BND File
			gio::write( OutputFileBNDetails, Format_721 ) << "! ===============================================================";
			gio::write( OutputFileBNDetails, Format_720 );
			gio::write( ChrOut, fmtLD ) << NumOfControlledZones;
			gio::write( OutputFileBNDetails, Format_721 ) << " #Zone Equipment Lists," + stripped( ChrOut );
			gio::write( OutputFileBNDetails, Format_722 );
			gio::write( OutputFileBNDetails, Format_723 );

			for ( Count = 1; Count <= NumOfZones; ++Count ) {
				// Zone equipment list array parallels controlled zone equipment array, so
				// same index finds corresponding data from both arrays
				if ( ! ZoneEquipConfig( Count ).IsControlled ) continue;
				gio::write( ChrOut, fmtLD ) << Count;
				gio::write( ChrOut2, fmtLD ) << ZoneEquipList( Count ).NumOfEquipTypes;
				gio::write( OutputFileBNDetails, Format_721 ) << " Zone Equipment List," + stripped( ChrOut ) + ',' + ZoneEquipList( Count ).Name + ',' + ZoneEquipConfig( Count ).ZoneName + ',' + stripped( ChrOut2 );

				for ( Count1 = 1; Count1 <= ZoneEquipList( Count ).NumOfEquipTypes; ++Count1 ) {
					gio::write( ChrOut, fmtLD ) << Count1;
					gio::write( ChrOut2, fmtLD ) << ZoneEquipList( Count ).CoolingPriority( Count1 );
					gio::write( ChrOut3, fmtLD ) << ZoneEquipList( Count ).HeatingPriority( Count1 );
					gio::write( OutputFileBNDetails, Format_721 ) << "   Zone Equipment Component," + stripped( ChrOut ) + ',' + ZoneEquipList( Count ).EquipType( Count1 ) + ',' + ZoneEquipList( Count ).EquipName( Count1 ) + ',' + ZoneEquipConfig( Count ).ZoneName + ',' + stripped( ChrOut2 ) + ',' + stripped( ChrOut3 );
				}
			}
		}

		//Report Dual Duct Dampers to BND File
		ReportDualDuctConnections();

		if ( NumNodeConnectionErrors == 0 ) {
			ShowMessage( "No node connection errors were found." );
		} else {
			gio::write( ChrOut, fmtLD ) << NumNodeConnectionErrors;
			strip( ChrOut );
			if ( NumNodeConnectionErrors > 1 ) {
				ShowMessage( "There were " + ChrOut + " node connection errors noted." );
			} else {
				ShowMessage( "There was " + ChrOut + " node connection error noted." );
			}
		}

		AskForConnectionsReport = false;

	}

	void
	ReportParentChildren()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reports parent compsets with ensuing children data.

		// METHODOLOGY EMPLOYED:
		// Uses IsParentObject,GetNumChildren,GetChildrenData

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataGlobals::OutputFileDebug;
		using General::TrimSigDigits;
		using namespace DataBranchNodeConnections;
		using namespace BranchNodeConnections;

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
		int Loop;
		int Loop1;
		Array1D_string ChildCType;
		Array1D_string ChildCName;
		Array1D_string ChildInNodeName;
		Array1D_string ChildOutNodeName;
		Array1D_int ChildInNodeNum;
		Array1D_int ChildOutNodeNum;
		int NumChildren;
		bool ErrorsFound;

		ErrorsFound = false;
		gio::write( OutputFileDebug, fmtA ) << "Node Type,CompSet Name,Inlet Node,OutletNode";
		for ( Loop = 1; Loop <= NumOfActualParents; ++Loop ) {
			NumChildren = GetNumChildren( ParentNodeList( Loop ).CType, ParentNodeList( Loop ).CName );
			if ( NumChildren > 0 ) {
				ChildCType.allocate( NumChildren );
				ChildCName.allocate( NumChildren );
				ChildInNodeName.allocate( NumChildren );
				ChildOutNodeName.allocate( NumChildren );
				ChildInNodeNum.allocate( NumChildren );
				ChildOutNodeNum.allocate( NumChildren );
				ChildCType = BlankString;
				ChildCName = BlankString;
				ChildInNodeName = BlankString;
				ChildOutNodeName = BlankString;
				ChildInNodeNum = 0;
				ChildOutNodeNum = 0;
				GetChildrenData( ParentNodeList( Loop ).CType, ParentNodeList( Loop ).CName, NumChildren, ChildCType, ChildCName, ChildInNodeName, ChildInNodeNum, ChildOutNodeName, ChildOutNodeNum, ErrorsFound );
				if ( Loop > 1 ) gio::write( OutputFileDebug, "(1X,60('='))" );
				gio::write( OutputFileDebug, fmtA ) << " Parent Node," + ParentNodeList( Loop ).CType + ':' + ParentNodeList( Loop ).CName + ',' + ParentNodeList( Loop ).InletNodeName + ',' + ParentNodeList( Loop ).OutletNodeName;
				for ( Loop1 = 1; Loop1 <= NumChildren; ++Loop1 ) {
					gio::write( OutputFileDebug, fmtA ) << "..ChildNode," + ChildCType( Loop1 ) + ':' + ChildCName( Loop1 ) + ',' + ChildInNodeName( Loop1 ) + ',' + ChildOutNodeName( Loop1 );
				}
				ChildCType.deallocate();
				ChildCName.deallocate();
				ChildInNodeName.deallocate();
				ChildOutNodeName.deallocate();
				ChildInNodeNum.deallocate();
				ChildOutNodeNum.deallocate();
			} else {
				if ( Loop > 1 ) gio::write( OutputFileDebug, "(1X,60('='))" );
				gio::write( OutputFileDebug, fmtA ) << " Parent Node (no children)," + ParentNodeList( Loop ).CType + ':' + ParentNodeList( Loop ).CName + ',' + ParentNodeList( Loop ).InletNodeName + ',' + ParentNodeList( Loop ).OutletNodeName;
			}
		}

	}

	void
	ReportCompSetMeterVariables()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reports comp set meter variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::OutputFileDebug;
		using namespace DataBranchNodeConnections;
		using namespace BranchNodeConnections;
		using namespace DataGlobalConstants;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int Loop1;
		int NumVariables;
		Array1D_int VarIndexes;
		Array1D_int VarIDs;
		Array1D_int IndexTypes;
		Array1D_int VarTypes;
		Array1D_string UnitsStrings;
		Array1D_string VarNames;
		Array1D_int ResourceTypes;
		Array1D_string EndUses;
		Array1D_string Groups;

		gio::write( OutputFileDebug, fmtA ) << " CompSet,ComponentType,ComponentName,NumMeteredVariables";
		gio::write( OutputFileDebug, fmtA ) << " RepVar,ReportIndex,ReportID,ReportName,Units,ResourceType,EndUse,Group,IndexType";

		for ( Loop = 1; Loop <= NumCompSets; ++Loop ) {
			NumVariables = GetNumMeteredVariables( CompSets( Loop ).CType, CompSets( Loop ).CName );
			gio::write( OutputFileDebug, "(1X,'CompSet,',A,',',A,',',I5)" ) << CompSets( Loop ).CType << CompSets( Loop ).CName << NumVariables;
			if ( NumVariables <= 0 ) continue;
			VarIndexes.dimension( NumVariables, 0 );
			VarIDs.dimension( NumVariables, 0 );
			IndexTypes.dimension( NumVariables, 0 );
			VarTypes.dimension( NumVariables, 0 );
			VarNames.allocate( NumVariables );
			UnitsStrings.allocate( NumVariables );
			ResourceTypes.dimension( NumVariables, 0 );
			EndUses.allocate( NumVariables );
			Groups.allocate( NumVariables );
			GetMeteredVariables( CompSets( Loop ).CType, CompSets( Loop ).CName, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, VarNames, _, VarIDs );
			for ( Loop1 = 1; Loop1 <= NumVariables; ++Loop1 ) {
				gio::write( OutputFileDebug, "(1X,'RepVar,',I5,',',I5,',',A,',[',A,'],',A,',',A,',',A,',',I5)" ) << VarIndexes( Loop1 ) << VarIDs( Loop1 ) << VarNames( Loop1 ) << UnitsStrings( Loop1 ) << GetResourceTypeChar( ResourceTypes( Loop1 ) ) << EndUses( Loop1 ) << Groups( Loop1 ) << IndexTypes( Loop1 );
			}
			VarIndexes.deallocate();
			IndexTypes.deallocate();
			VarTypes.deallocate();
			VarIDs.deallocate();
			VarNames.deallocate();
			UnitsStrings.deallocate();
			ResourceTypes.deallocate();
			EndUses.deallocate();
			Groups.deallocate();
		}

	}

	void
	PostIPProcessing()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This provides post processing (for errors, etc) directly after the InputProcessor
		// finishes.  Code originally in the Input Processor.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		//using SQLiteProcedures::CreateSQLiteDatabase;
		using InputProcessor::PreProcessorCheck;
		using InputProcessor::OverallErrorFlag;
		using InputProcessor::CompactObjectsCheck;
		using InputProcessor::ParametricObjectsCheck;
		using InputProcessor::GetNumSectionsFound;
		using InputProcessor::PreScanReportingVariables;
		using InputProcessor::NumOutOfRangeErrorsFound;
		using InputProcessor::NumBlankReqFieldFound;
		using InputProcessor::NumMiscErrorsFound;
		using FluidProperties::FluidIndex_Water;
		using FluidProperties::FluidIndex_EthyleneGlycol;
		using FluidProperties::FluidIndex_PropoleneGlycol;
		using FluidProperties::FindGlycol;

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
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool PreP_Fatal( false ); // True if a preprocessor flags a fatal error
		////////////////////////////////////////////////////////////////////////////////////

		DoingInputProcessing = false;

		PreProcessorCheck( PreP_Fatal ); // Check Preprocessor objects for warning, severe, etc errors.

		CheckCachedIPErrors();

		if ( PreP_Fatal ) {
			ShowFatalError( "Preprocessor condition(s) cause termination." );
		}

		if ( OverallErrorFlag ) {
			ShowFatalError( "IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination." );
		}

		CompactObjectsCheck(); // Check to see if Compact Objects (CompactHVAC, etc) are in input file.
		// If so, ExpandObjects didn't get called...
		ParametricObjectsCheck(); // check to see if any parametric objects are in the input file
		// parametric preprocessor was not run

		if ( NumOutOfRangeErrorsFound + NumBlankReqFieldFound + NumMiscErrorsFound > 0 ) {
			ShowSevereError( "IP: Out of \"range\" values and/or blank required fields found in input" );
			ShowFatalError( "IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination." );
		}

		if ( GetNumSectionsFound( "DISPLAYALLWARNINGS" ) > 0 ) {
			DisplayAllWarnings = true;
			DisplayExtraWarnings = true;
			DisplayUnusedSchedules = true;
			DisplayUnusedObjects = true;
		}

		if ( GetNumSectionsFound( "DISPLAYEXTRAWARNINGS" ) > 0 ) {
			DisplayExtraWarnings = true;
		}

		if ( GetNumSectionsFound( "DISPLAYUNUSEDOBJECTS" ) > 0 ) {
			DisplayUnusedObjects = true;
		}

		if ( GetNumSectionsFound( "DISPLAYUNUSEDSCHEDULES" ) > 0 ) {
			DisplayUnusedSchedules = true;
		}

		if ( GetNumSectionsFound( "DisplayZoneAirHeatBalanceOffBalance" ) > 0 ) {
			DisplayZoneAirHeatBalanceOffBalance = true;
		}

		if ( GetNumSectionsFound( "DISPLAYADVANCEDREPORTVARIABLES" ) > 0 ) {
			DisplayAdvancedReportVariables = true;
		}

		//Set up more globals - process fluid input.
		FluidIndex_Water = FindGlycol( "Water" );
		FluidIndex_EthyleneGlycol = FindGlycol( "EthyleneGlycol" );
		FluidIndex_PropoleneGlycol = FindGlycol( "PropoleneGlycol" );

		PreScanReportingVariables();

	}

	void
	CheckCachedIPErrors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine displays the cached error messages after the preprocessor
		// errors have been checked and produced.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iostatus;
		std::string ErrorMessage;

		gio::close( CacheIPErrorFile );
		gio::open( CacheIPErrorFile, DataStringGlobals::outputIperrFileName );
		iostatus = 0;
		while ( iostatus == 0 ) {
			{ IOFlags flags; gio::read( CacheIPErrorFile, fmtA, flags ) >> ErrorMessage; iostatus = flags.ios(); }
			if ( iostatus != 0 ) break;
			if ( is_blank( ErrorMessage ) ) continue;
			ShowErrorMessage( ErrorMessage );
			if ( sqlite ) {
				// Following code relies on specific formatting of Severes, Warnings, and continues
				// that occur in the IP processing.  Later ones -- i.e. Fatals occur after the
				// automatic sending of error messages to SQLite are turned on.
				if ( ErrorMessage[ 4 ] == 'S' ) {
					sqlite->createSQLiteErrorRecord( 1, 1, ErrorMessage, 0 );
				} else if ( ErrorMessage[ 4 ] == 'W' ) {
					sqlite->createSQLiteErrorRecord( 1, 0, ErrorMessage, 0 );
				} else if ( ErrorMessage[ 6 ] == '~' ) {
					sqlite->updateSQLiteErrorRecord( ErrorMessage );
				}
			}
		}

		{ IOFlags flags; flags.DISPOSE( "delete" ); gio::close( CacheIPErrorFile, flags ); }

	}

	void
	CheckThreading()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Check number of threads available versus number of surfaces, etc.

		// METHODOLOGY EMPLOYED:
		// Check Max Threads (OMP_NUM_THREADS) = MaxNumberOfThreads, iEnvSetThreads
		// Check EP Max Threads (EP_OMP_NUM_THREADS) = iepEnvSetThreads
		// Check if IDF input (ProgramControl) = iIDFSetThreads
		// Check # active sims (cntActv) = inumActiveSims [report only?]

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSystemVariables;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;
#if defined(_OPENMP) && defined(HBIRE_USE_OMP)
		using omp_lib::omp_get_max_threads;
		using omp_lib::omp_get_num_threads;
		using omp_lib::omp_set_num_threads;
#endif

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string cEnvValue;
		int ios;
		int TotHTSurfs; // Number of BuildingSurface:Detailed items to obtain
		int TotDetailedWalls; // Number of Wall:Detailed items to obtain
		int TotDetailedRoofs; // Number of RoofCeiling:Detailed items to obtain
		int TotDetailedFloors; // Number of Floor:Detailed items to obtain
		int TotHTSubs; // Number of FenestrationSurface:Detailed items to obtain
		int TotIntMass; // Number of InternalMass items to obtain
		// Simple Surfaces (Rectangular)
		int TotRectExtWalls; // Number of Exterior Walls to obtain
		int TotRectIntWalls; // Number of Adiabatic Walls to obtain
		int TotRectIZWalls; // Number of Interzone Walls to obtain
		int TotRectUGWalls; // Number of Underground to obtain
		int TotRectRoofs; // Number of Roofs to obtain
		int TotRectCeilings; // Number of Adiabatic Ceilings to obtain
		int TotRectIZCeilings; // Number of Interzone Ceilings to obtain
		int TotRectGCFloors; // Number of Floors with Ground Contact to obtain
		int TotRectIntFloors; // Number of Adiabatic Walls to obtain
		int TotRectIZFloors; // Number of Interzone Floors to obtain
		int TotRectWindows;
		int TotRectDoors;
		int TotRectGlazedDoors;
		int TotRectIZWindows;
		int TotRectIZDoors;
		int TotRectIZGlazedDoors;
		int NumAlphas;
		int NumNumbers;

		// Figure out how many surfaces there are.
		TotHTSurfs = GetNumObjectsFound( "BuildingSurface:Detailed" );
		TotDetailedWalls = GetNumObjectsFound( "Wall:Detailed" );
		TotDetailedRoofs = GetNumObjectsFound( "RoofCeiling:Detailed" );
		TotDetailedFloors = GetNumObjectsFound( "Floor:Detailed" );
		TotHTSubs = GetNumObjectsFound( "FenestrationSurface:Detailed" );
		TotIntMass = GetNumObjectsFound( "InternalMass" );
		TotRectWindows = GetNumObjectsFound( "Window" );
		TotRectDoors = GetNumObjectsFound( "Door" );
		TotRectGlazedDoors = GetNumObjectsFound( "GlazedDoor" );
		TotRectIZWindows = GetNumObjectsFound( "Window:Interzone" );
		TotRectIZDoors = GetNumObjectsFound( "Door:Interzone" );
		TotRectIZGlazedDoors = GetNumObjectsFound( "GlazedDoor:Interzone" );
		TotRectExtWalls = GetNumObjectsFound( "Wall:Exterior" );
		TotRectIntWalls = GetNumObjectsFound( "Wall:Adiabatic" );
		TotRectIZWalls = GetNumObjectsFound( "Wall:Interzone" );
		TotRectUGWalls = GetNumObjectsFound( "Wall:Underground" );
		TotRectRoofs = GetNumObjectsFound( "Roof" );
		TotRectCeilings = GetNumObjectsFound( "Ceiling:Adiabatic" );
		TotRectIZCeilings = GetNumObjectsFound( "Ceiling:Interzone" );
		TotRectGCFloors = GetNumObjectsFound( "Floor:GroundContact" );
		TotRectIntFloors = GetNumObjectsFound( "Floor:Adiabatic" );
		TotRectIZFloors = GetNumObjectsFound( "Floor:Interzone" );

		iNominalTotSurfaces = TotHTSurfs + TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors + TotHTSubs + TotIntMass + TotRectWindows + TotRectDoors + TotRectGlazedDoors + TotRectIZWindows + TotRectIZDoors + TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls + TotRectRoofs + TotRectCeilings + TotRectIZCeilings + TotRectGCFloors + TotRectIntFloors + TotRectIZFloors;

#ifdef HBIRE_USE_OMP
		MaxNumberOfThreads = MAXTHREADS();
		Threading = true;

		get_environment_variable( cNumThreads, cEnvValue );
		if ( ! cEnvValue.empty() ) {
			lEnvSetThreadsInput = true;
			{ IOFlags flags; gio::read( cEnvValue, fmtLD, flags ) >> iEnvSetThreads; ios = flags.ios(); }
			if ( ios != 0 ) iEnvSetThreads = MaxNumberOfThreads;
			if ( iEnvSetThreads == 0 ) iEnvSetThreads = MaxNumberOfThreads;
		}

		get_environment_variable( cepNumThreads, cEnvValue );
		if ( ! cEnvValue.empty() ) {
			lepSetThreadsInput = true;
			{ IOFlags flags; gio::read( cEnvValue, fmtLD, flags ) >> iepEnvSetThreads; ios = flags.ios(); }
			if ( ios != 0 ) iepEnvSetThreads = MaxNumberOfThreads;
			if ( iepEnvSetThreads == 0 ) iepEnvSetThreads = MaxNumberOfThreads;
		}

		cCurrentModuleObject = "ProgramControl";
		if ( GetNumObjectsFound( cCurrentModuleObject ) > 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, ios, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			iIDFSetThreads = int( rNumericArgs( 1 ) );
			lIDFsetThreadsInput = true;
			if ( iIDFSetThreads <= 0 ) {
				iIDFSetThreads = MaxNumberOfThreads;
				if ( lEnvSetThreadsInput ) iIDFSetThreads = iEnvSetThreads;
				if ( lepSetThreadsInput ) iIDFSetThreads = iepEnvSetThreads;
			}
			if ( iIDFSetThreads > MaxNumberOfThreads ) {
				ShowWarningError( "CheckThreading: Your chosen number of threads=[" + RoundSigDigits( iIDFSetThreads ) + "] is greater than the maximum number of threads=[" + RoundSigDigits( MaxNumberOfThreads ) + "]." );
				ShowContinueError( "...execution time for this run may be degraded." );
			}
		}

		if ( iNominalTotSurfaces <= 30 ) {
			NumberIntRadThreads = 1;
			if ( lEnvSetThreadsInput ) NumberIntRadThreads = iEnvSetThreads;
			if ( lepSetThreadsInput ) NumberIntRadThreads = iepEnvSetThreads;
			if ( lIDFSetThreadsInput ) NumberIntRadThreads = iIDFSetThreads;
		} else {
			NumberIntRadThreads = MaxNumberOfThreads;
			if ( lEnvSetThreadsInput ) NumberIntRadThreads = iEnvSetThreads;
			if ( lepSetThreadsInput ) NumberIntRadThreads = iepEnvSetThreads;
			if ( lIDFSetThreadsInput ) NumberIntRadThreads = iIDFSetThreads;
		}
#else
		Threading = false;
		cCurrentModuleObject = "ProgramControl";
		if ( GetNumObjectsFound( cCurrentModuleObject ) > 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, ios, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			int iIDFsetThreadsInput = int( rNumericArgs( 1 ) );
			if ( iIDFSetThreads > 1 ) {
				ShowWarningError( "CheckThreading: " + cCurrentModuleObject + " is not available in this version." );
				ShowContinueError( "...user requested [" + RoundSigDigits( iIDFsetThreadsInput ) + "] threads." );
			}
		}
		MaxNumberOfThreads = 1;
#endif
		// just reporting
		get_environment_variable( cNumActiveSims, cEnvValue );
		if ( ! cEnvValue.empty() ) {
			lnumActiveSims = true;
			{ IOFlags flags; gio::read( cEnvValue, fmtLD, flags ) >> inumActiveSims; ios = flags.ios(); }
		}

	}

} // SimulationManager

// EXTERNAL SUBROUTINES:

void
Resimulate(
	bool & ResimExt, // Flag to resimulate the exterior energy use simulation
	bool & ResimHB, // Flag to resimulate the heat balance simulation (including HVAC)
	bool & ResimHVAC // Flag to resimulate the HVAC simulation
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   August 2005
	//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine is called as necessary by the Demand Manager to resimulate some of the modules that have
	// already been simulated for the current timestep.  For example, if LIGHTS are demand limited, the lighting
	// power is reduced which also impacts the zone internal heat gains and therefore requires that the entire
	// zone heat balance must be resimulated.

	// METHODOLOGY EMPLOYED:
	// If the zone heat balance must be resimulated, all the major subroutines are called sequentially in order
	// to recalculate the impacts of demand limiting.  This routine is called from ManageHVAC _before_ any variables
	// are reported or histories are updated.  This routine can be called multiple times without the overall
	// simulation moving forward in time.
	// If only HVAC components are demand limited, then the HVAC system is resimulated, not the entire heat balance.
	// Similarly, if ony exterior lights and equipment are demand limited, it is only necessary to resimulate the
	// exterior energy use, not the entire heat balance, nor the HVAC system.
	// Below is the hierarchy of subroutine calls.  The calls marked with an asterisk are resimulated here.
	// ManageSimulation
	//     ManageWeather
	//     ManageDemand
	//   * ManageExteriorEnergyUse
	//     ManageHeatBalance
	//       * InitHeatBalance
	//             PerformSolarCalculations
	//         ManageSurfaceHeatBalance
	//           * InitSurfaceHeatBalance
	//                 ManageInternalHeatGains
	//           * CalcHeatBalanceOutsideSurf
	//           * CalcHeatBalanceInsideSurf
	//             ManageAirHeatBalance
	//                *InitAirHeatBalance
	//                 CalcHeatBalanceAir
	//                   * CalcAirFlow
	//                   * ManageRefrigeratedCaseRacks
	//                     ManageHVAC
	//                       * ManageZoneAirUpdates 'GET ZONE SETPOINTS'
	//                       * ManageZoneAirUpdates 'PREDICT'
	//                       * SimHVAC
	//                         UpdateDataandReport
	//                 ReportAirHeatBalance
	//             UpdateFinalSurfaceHeatBalance
	//             UpdateThermalHistories
	//             UpdateMoistureHistories
	//             ManageThermalComfort
	//             ReportSurfaceHeatBalance
	//         RecKeepHeatBalance
	//         ReportHeatBalance

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DemandManager::DemandManagerExtIterations;
	using DemandManager::DemandManagerHBIterations;
	using DemandManager::DemandManagerHVACIterations;
	using ExteriorEnergyUse::ManageExteriorEnergyUse;
	using HeatBalanceSurfaceManager::InitSurfaceHeatBalance;
	using HeatBalanceAirManager::InitAirHeatBalance;
	using RefrigeratedCase::ManageRefrigeratedCaseRacks;
	using ZoneTempPredictorCorrector::ManageZoneAirUpdates;
	using DataHeatBalFanSys::iGetZoneSetPoints;
	using DataHeatBalFanSys::iPredictStep;
	using HVACManager::SimHVAC;
	//using HVACManager::CalcAirFlowSimple;
	using DataHVACGlobals::UseZoneTimeStepHistory; // , InitDSwithZoneHistory
	using ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates;
	using DataContaminantBalance::Contaminant;

	using DataHeatBalance::ZoneAirMassFlow;
	using namespace ZoneEquipmentManager;
	//using ZoneEquipmentManager::CalcAirFlowSimple;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Real64 ZoneTempChange( 0.0 ); // Dummy variable needed for calling ManageZoneAirUpdates

	// FLOW:
	if ( ResimExt ) {
		ManageExteriorEnergyUse();

		++DemandManagerExtIterations;
	}

	if ( ResimHB ) {
		// Surface simulation
		InitSurfaceHeatBalance();
		HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf();
		HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf();

		// Air simulation
		InitAirHeatBalance();
		ManageRefrigeratedCaseRacks();

		++DemandManagerHBIterations;
		ResimHVAC = true; // Make sure HVAC is resimulated too
	}

	if ( ResimHVAC ) {
		// HVAC simulation
		ManageZoneAirUpdates( iGetZoneSetPoints, ZoneTempChange, false, UseZoneTimeStepHistory, 0.0 );
		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iGetZoneSetPoints, false, UseZoneTimeStepHistory, 0.0 );
		CalcAirFlowSimple( 0, ZoneAirMassFlow.EnforceZoneMassBalance );
		ManageZoneAirUpdates( iPredictStep, ZoneTempChange, false, UseZoneTimeStepHistory, 0.0 );
		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iPredictStep, false, UseZoneTimeStepHistory, 0.0 );
		SimHVAC();

		++DemandManagerHVACIterations;
	}

}


} // EnergyPlus
