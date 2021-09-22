// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
}

// C++ Headers
#include <memory>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CostEstimateManager.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EconomicLifeCycleCost.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ExternalInterface.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HVACSizingSimulationManager.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/OutputReports.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantPipingSystemsManager.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <EnergyPlus/api/datatransfer.h>

namespace EnergyPlus {
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
    using namespace DataSizing;
    using namespace DataSystemVariables;
    using namespace HeatBalanceManager;
    using namespace WeatherManager;
    using namespace ExternalInterface;

    // MODULE PARAMETER DEFINITIONS:

    void ManageSimulation(EnergyPlusData &state)
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

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using BranchInputManager::ManageBranchInput;
        using BranchInputManager::TestBranchIntegrity;
        using BranchNodeConnections::CheckNodeConnections;
        using BranchNodeConnections::TestCompSetInletOutletNodes;
        using CostEstimateManager::SimCostEstimate;
        using CurveManager::InitCurveReporting;
        using DemandManager::InitDemandManagers;
        using EconomicLifeCycleCost::ComputeLifeCycleCostAndReport;
        using EconomicLifeCycleCost::GetInputForLifeCycleCost;
        using EconomicTariff::ComputeTariff; // added for computing annual utility costs
        using EconomicTariff::WriteTabularTariffReports;
        using EMSManager::CheckIfAnyEMS;
        using EMSManager::ManageEMS;
        using ExteriorEnergyUse::ManageExteriorEnergyUse;
        using FaultsManager::CheckAndReadFaults;
        using HVACControllers::DumpAirLoopStatistics;
        using MixedAir::CheckControllerLists;
        using NodeInputManager::CheckMarkedNodes;
        using NodeInputManager::SetupNodeVarsForReporting;
        using OutputProcessor::ReportForTabularReports;
        using OutputProcessor::ResetAccumulationWhenWarmupComplete;
        using OutputProcessor::SetupTimePointers;
        using OutputReportPredefined::SetPredefinedTables;
        using OutputReportTabular::CloseOutputTabularFile;
        using OutputReportTabular::OpenOutputTabularFile;
        using OutputReportTabular::ResetTabularReports;
        using OutputReportTabular::WriteTabularReports;
        using PlantManager::CheckIfAnyPlant;
        using PlantPipingSystemsManager::CheckIfAnyBasements;
        using PlantPipingSystemsManager::CheckIfAnySlabs;
        using PlantPipingSystemsManager::SimulateGroundDomains;
        using PollutionModule::CheckPollutionMeterReporting;
        using PollutionModule::SetupPollutionCalculations;
        using PollutionModule::SetupPollutionMeterReporting;
        using Psychrometrics::InitializePsychRoutines;
        using SetPointManager::CheckIfAnyIdealCondEntSetPoint;
        using SizingManager::ManageSizing;
        using SystemReports::CreateEnergyReportStructure;
        using SystemReports::ReportAirLoopConnections;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool Available; // an environment is available to process
        bool ErrorsFound(false);
        bool TerminalError(false);
        bool SimsDone;
        bool ErrFound;
        bool oneTimeUnderwaterBoundaryCheck = true;
        bool AnyUnderwaterBoundaries = false;
        int EnvCount;

        state.files.outputControl.getInput(state);
        state.dataResultsFramework->resultsFramework->setupOutputOptions(state);

        state.files.debug.ensure_open(state, "OpenOutputFiles", state.files.outputControl.dbg);

        // CreateSQLiteDatabase();
        state.dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(state);

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->sqliteBegin();
            state.dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(
                1, state.dataStrGlobals->VerStringVar, state.dataStrGlobals->CurrentDateTime);
            state.dataSQLiteProcedures->sqlite->sqliteCommit();
        }

        PostIPProcessing(state);

        state.dataGlobal->BeginSimFlag = true;
        state.dataGlobal->BeginFullSimFlag = false;
        state.dataGlobal->DoOutputReporting = false;
        state.dataReportFlag->DisplayPerfSimulationFlag = false;
        state.dataReportFlag->DoWeatherInitReporting = false;
        state.dataSimulationManager->RunPeriodsInInput =
            (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RunPeriod") > 0 ||
             state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RunPeriod:CustomRange") > 0 || state.dataSysVars->FullAnnualRun);
        state.dataErrTracking->AskForConnectionsReport = false; // set to false until sizing is finished

        OpenOutputFiles(state);
        GetProjectData(state);
        InitializePsychRoutines(state);
        CheckForMisMatchedEnvironmentSpecifications(state);
        CheckForRequestedReporting(state);
        SetPredefinedTables(state);
        SetPreConstructionInputParameters(state); // establish array bounds for constructions early

        SetupTimePointers(
            state, OutputProcessor::SOVTimeStepType::Zone, state.dataGlobal->TimeStepZone); // Set up Time pointer for HB/Zone Simulation
        SetupTimePointers(state, OutputProcessor::SOVTimeStepType::HVAC, TimeStepSys);

        CheckIfAnyEMS(state);
        CheckIfAnyPlant(state);
        CheckIfAnySlabs(state);
        CheckIfAnyBasements(state);
        CheckIfAnyIdealCondEntSetPoint(state);
        createFacilityElectricPowerServiceObject(state);
        createCoilSelectionReportObj(state);

        ManageBranchInput(state); // just gets input and returns.

        // Create a new plugin manager which starts up the Python interpreter
        state.dataPluginManager->pluginManager = std::make_unique<EnergyPlus::PluginManagement::PluginManager>(state);

        state.dataGlobal->DoingSizing = true;
        ManageSizing(state);

        state.dataGlobal->BeginFullSimFlag = true;
        SimsDone = false;
        if (state.dataGlobal->DoDesDaySim || state.dataGlobal->DoWeathSim || state.dataGlobal->DoHVACSizingSimulation) {
            state.dataGlobal->DoOutputReporting = true;
        }
        state.dataGlobal->DoingSizing = false;

        if ((state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing) &&
            !(state.dataGlobal->DoDesDaySim || (state.dataGlobal->DoWeathSim && state.dataSimulationManager->RunPeriodsInInput))) {
            ShowWarningError(state,
                             "ManageSimulation: Input file has requested Sizing Calculations but no Simulations are requested (in SimulationControl "
                             "object). Succeeding warnings/errors may be confusing.");
        }
        Available = true;

        if (state.dataBranchInputManager->InvalidBranchDefinitions) {
            ShowFatalError(state, "Preceding error(s) in Branch Input cause termination.");
        }

        DisplayString(state, "Adjusting Air System Sizing");
        SizingManager::ManageSystemSizingAdjustments(state);

        DisplayString(state, "Adjusting Standard 62.1 Ventilation Sizing");
        SizingManager::ManageSystemVentilationAdjustments(state);

        DisplayString(state, "Initializing Simulation");
        state.dataGlobal->KickOffSimulation = true;

        ResetEnvironmentCounter(state);
        SetupSimulation(state, ErrorsFound);

        CheckAndReadFaults(state);

        InitCurveReporting(state);

        state.dataErrTracking->AskForConnectionsReport = true; // set to true now that input processing and sizing is done.
        state.dataGlobal->KickOffSimulation = false;
        state.dataGlobal->WarmupFlag = false;
        state.dataReportFlag->DoWeatherInitReporting = true;

        //  Note:  All the inputs have been 'gotten' by the time we get here.
        ErrFound = false;
        if (state.dataGlobal->DoOutputReporting) {
            DisplayString(state, "Reporting Surfaces");

            ReportSurfaces(state);

            SetupNodeVarsForReporting(state);
            state.dataGlobal->MetersHaveBeenInitialized = true;
            SetupPollutionMeterReporting(state);
            SystemReports::AllocateAndSetUpVentReports(state);
            if (state.dataPluginManager->pluginManager) {
                EnergyPlus::PluginManagement::PluginManager::setupOutputVariables(state);
            }
            UpdateMeterReporting(state);
            CheckPollutionMeterReporting(state);
            state.dataElectPwrSvcMgr->facilityElectricServiceObj->verifyCustomMetersElecPowerMgr(state);
            SetupPollutionCalculations(state);
            InitDemandManagers(state);
            TestBranchIntegrity(state, ErrFound);
            if (ErrFound) TerminalError = true;
            TestAirPathIntegrity(state, ErrFound);
            if (ErrFound) TerminalError = true;
            CheckMarkedNodes(state, ErrFound);
            if (ErrFound) TerminalError = true;
            CheckNodeConnections(state, ErrFound);
            if (ErrFound) TerminalError = true;
            TestCompSetInletOutletNodes(state, ErrFound);
            if (ErrFound) TerminalError = true;
            CheckControllerLists(state, ErrFound);
            if (ErrFound) TerminalError = true;

            if (state.dataGlobal->DoDesDaySim || state.dataGlobal->DoWeathSim) {
                ReportLoopConnections(state);
                ReportAirLoopConnections(state);
                ReportNodeConnections(state);
                // Debug reports
                //      CALL ReportCompSetMeterVariables
                //      CALL ReportParentChildren
            }
            CreateEnergyReportStructure(state);
            bool anyEMSRan;
            ManageEMS(state,
                      EMSManager::EMSCallFrom::SetupSimulation,
                      anyEMSRan,
                      ObjexxFCL::Optional_int_const()); // point to finish setup processing EMS, sensor ready now

            ProduceRDDMDD(state);

            if (TerminalError) {
                ShowFatalError(state, "Previous Conditions cause program termination.");
            }
        }

        // up until this point, output vars, meters, actuators, etc., may not have been registered; they are now
        state.dataPluginManager->fullyReady = true;

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->sqliteBegin();
            state.dataSQLiteProcedures->sqlite->updateSQLiteSimulationRecord(1, state.dataGlobal->NumOfTimeStepInHour);
            state.dataSQLiteProcedures->sqlite->sqliteCommit();
        }

        GetInputForLifeCycleCost(state); // must be prior to WriteTabularReports -- do here before big simulation stuff.

        // check for variable latitude/location/etc
        WeatherManager::ReadVariableLocationOrientation(state);

        // if user requested HVAC Sizing Simulation, call HVAC sizing simulation manager
        if (state.dataGlobal->DoHVACSizingSimulation) {
            ManageHVACSizingSimulation(state, ErrorsFound);
        }

        ShowMessage(state, "Beginning Simulation");
        DisplayString(state, "Beginning Primary Simulation");

        ResetEnvironmentCounter(state);

        EnvCount = 0;
        state.dataGlobal->WarmupFlag = true;

        while (Available) {
            if (state.dataGlobal->stopSimulation) break;

            GetNextEnvironment(state, Available, ErrorsFound);

            if (!Available) break;
            if (ErrorsFound) break;
            if ((!state.dataGlobal->DoDesDaySim) && (state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::RunPeriodWeather)) continue;
            if ((!state.dataGlobal->DoWeathSim) && (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) continue;
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay)
                continue; // don't run these here, only for sizing simulations

            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign)
                continue; // don't run these here, only for sizing simulations

            ++EnvCount;

            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->sqliteBegin();
                state.dataSQLiteProcedures->sqlite->createSQLiteEnvironmentPeriodRecord(
                    state.dataEnvrn->CurEnvirNum, state.dataEnvrn->EnvironmentName, state.dataGlobal->KindOfSim);
                state.dataSQLiteProcedures->sqlite->sqliteCommit();
            }

            state.dataErrTracking->ExitDuringSimulations = true;
            SimsDone = true;
            DisplayString(state, "Initializing New Environment Parameters");

            state.dataGlobal->BeginEnvrnFlag = true;
            if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay) &&
                (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                     .suppressBegEnvReset)) {
                // user has input in SizingPeriod:DesignDay directing to skip begin environment rests, for accuracy-with-speed as zones can more
                // easily converge fewer warmup days are allowed
                DisplayString(state, "Design Day Fast Warmup Mode: Suppressing Initialization of New Environment Parameters");
                state.dataGlobal->beginEnvrnWarmStartFlag = true;
            } else {
                state.dataGlobal->beginEnvrnWarmStartFlag = false;
            }
            state.dataGlobal->EndEnvrnFlag = false;
            state.dataEnvrn->EndMonthFlag = false;
            state.dataGlobal->WarmupFlag = true;
            state.dataGlobal->DayOfSim = 0;
            state.dataGlobal->DayOfSimChr = "0";
            state.dataReportFlag->NumOfWarmupDays = 0;
            if (state.dataEnvrn->CurrentYearIsLeapYear) {
                if (state.dataGlobal->NumOfDayInEnvrn <= 366) {
                    state.dataOutputProcessor->isFinalYear = true;
                }
            } else {
                if (state.dataGlobal->NumOfDayInEnvrn <= 365) {
                    state.dataOutputProcessor->isFinalYear = true;
                }
            }

            HVACManager::ResetNodeData(state); // Reset here, because some zone calcs rely on node data (e.g. ZoneITEquip)

            bool anyEMSRan;
            ManageEMS(state, EMSManager::EMSCallFrom::BeginNewEnvironment, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point

            while ((state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn) || (state.dataGlobal->WarmupFlag)) { // Begin day loop ...
                if (state.dataGlobal->stopSimulation) break;

                if (state.dataSQLiteProcedures->sqlite) state.dataSQLiteProcedures->sqlite->sqliteBegin(); // setup for one transaction per day

                ++state.dataGlobal->DayOfSim;
                state.dataGlobal->DayOfSimChr = fmt::to_string(state.dataGlobal->DayOfSim);
                if (!state.dataGlobal->WarmupFlag) {
                    ++state.dataEnvrn->CurrentOverallSimDay;
                    DisplaySimDaysProgress(state, state.dataEnvrn->CurrentOverallSimDay, state.dataEnvrn->TotalOverallSimDays);
                } else {
                    state.dataGlobal->DayOfSimChr = "0";
                }
                state.dataGlobal->BeginDayFlag = true;
                state.dataGlobal->EndDayFlag = false;

                if (state.dataGlobal->WarmupFlag) {
                    ++state.dataReportFlag->NumOfWarmupDays;
                    state.dataReportFlag->cWarmupDay = fmt::to_string(state.dataReportFlag->NumOfWarmupDays);
                    DisplayString(state, "Warming up {" + state.dataReportFlag->cWarmupDay + '}');
                } else if (state.dataGlobal->DayOfSim == 1) {
                    if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                        DisplayString(state, "Starting Simulation at " + state.dataEnvrn->CurMnDyYr + " for " + state.dataEnvrn->EnvironmentName);
                    } else {
                        DisplayString(state, "Starting Simulation at " + state.dataEnvrn->CurMnDy + " for " + state.dataEnvrn->EnvironmentName);
                    }
                    static constexpr std::string_view Format_700("Environment:WarmupDays,{:3}\n");
                    print(state.files.eio, Format_700, state.dataReportFlag->NumOfWarmupDays);
                    ResetAccumulationWhenWarmupComplete(state);
                } else if (state.dataReportFlag->DisplayPerfSimulationFlag) {
                    if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                        DisplayString(state, "Continuing Simulation at " + state.dataEnvrn->CurMnDyYr + " for " + state.dataEnvrn->EnvironmentName);
                    } else {
                        DisplayString(state, "Continuing Simulation at " + state.dataEnvrn->CurMnDy + " for " + state.dataEnvrn->EnvironmentName);
                    }
                    state.dataReportFlag->DisplayPerfSimulationFlag = false;
                }
                // for simulations that last longer than a week, identify when the last year of the simulation is started
                if ((state.dataGlobal->DayOfSim > 365) && ((state.dataGlobal->NumOfDayInEnvrn - state.dataGlobal->DayOfSim) == 364) &&
                    !state.dataGlobal->WarmupFlag) {
                    DisplayString(state, "Starting last  year of environment at:  " + state.dataGlobal->DayOfSimChr);
                    ResetTabularReports(state);
                }

                for (state.dataGlobal->HourOfDay = 1; state.dataGlobal->HourOfDay <= 24; ++state.dataGlobal->HourOfDay) { // Begin hour loop ...
                    if (state.dataGlobal->stopSimulation) break;

                    state.dataGlobal->BeginHourFlag = true;
                    state.dataGlobal->EndHourFlag = false;

                    for (state.dataGlobal->TimeStep = 1; state.dataGlobal->TimeStep <= state.dataGlobal->NumOfTimeStepInHour;
                         ++state.dataGlobal->TimeStep) {
                        if (state.dataGlobal->stopSimulation) break;

                        if (state.dataGlobal->AnySlabsInModel || state.dataGlobal->AnyBasementsInModel) {
                            SimulateGroundDomains(state, false);
                        }

                        if (AnyUnderwaterBoundaries) {
                            WeatherManager::UpdateUnderwaterBoundaries(state);
                        }

                        if (state.dataEnvrn->varyingLocationSchedIndexLat > 0 || state.dataEnvrn->varyingLocationSchedIndexLong > 0 ||
                            state.dataEnvrn->varyingOrientationSchedIndex > 0) {
                            WeatherManager::UpdateLocationAndOrientation(state);
                        }

                        state.dataGlobal->BeginTimeStepFlag = true;
                        ExternalInterfaceExchangeVariables(state);

                        // Set the End__Flag variables to true if necessary.  Note that
                        // each flag builds on the previous level.  EndDayFlag cannot be
                        // .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
                        // EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
                        // Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
                        // SubTimeStepFlags can/will be set/reset in the HVAC Manager.

                        if (state.dataGlobal->TimeStep == state.dataGlobal->NumOfTimeStepInHour) {
                            state.dataGlobal->EndHourFlag = true;
                            if (state.dataGlobal->HourOfDay == 24) {
                                state.dataGlobal->EndDayFlag = true;
                                if ((!state.dataGlobal->WarmupFlag) && (state.dataGlobal->DayOfSim == state.dataGlobal->NumOfDayInEnvrn)) {
                                    state.dataGlobal->EndEnvrnFlag = true;
                                }
                            }
                        }

                        ManageWeather(state);

                        ManageExteriorEnergyUse(state);

                        ManageHeatBalance(state);

                        if (oneTimeUnderwaterBoundaryCheck) {
                            AnyUnderwaterBoundaries = WeatherManager::CheckIfAnyUnderwaterBoundaries(state);
                            oneTimeUnderwaterBoundaryCheck = false;
                        }

                        state.dataGlobal->BeginHourFlag = false;
                        state.dataGlobal->BeginDayFlag = false;
                        state.dataGlobal->BeginEnvrnFlag = false;
                        state.dataGlobal->BeginSimFlag = false;
                        state.dataGlobal->BeginFullSimFlag = false;
                    } // TimeStep loop

                    state.dataGlobal->PreviousHour = state.dataGlobal->HourOfDay;

                } // ... End hour loop.

                if (state.dataSQLiteProcedures->sqlite) state.dataSQLiteProcedures->sqlite->sqliteCommit(); // one transaction per day

            } // ... End day loop.

            // Need one last call to send latest states to middleware
            ExternalInterfaceExchangeVariables(state);

        } // ... End environment loop.

        state.dataGlobal->WarmupFlag = false;
        if (!SimsDone && state.dataGlobal->DoDesDaySim) {
            if ((state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays) == 0) { // if sum is 0, then there was no sizing done.
                ShowWarningError(state,
                                 "ManageSimulation: SizingPeriod:* were requested in SimulationControl but no SizingPeriod:* objects in input.");
            }
        }

        if (!SimsDone && state.dataGlobal->DoWeathSim) {
            if (!state.dataSimulationManager->RunPeriodsInInput) { // if no run period requested, and sims not done
                ShowWarningError(state, "ManageSimulation: Weather Simulation was requested in SimulationControl but no RunPeriods in input.");
            }
        }

        PlantManager::CheckOngoingPlantWarnings(state);

        if (state.dataSQLiteProcedures->sqlite) state.dataSQLiteProcedures->sqlite->sqliteBegin(); // for final data to write

#ifdef EP_Detailed_Timings
        epStartTime("Closeout Reporting=");
#endif
        SimCostEstimate(state);

        ComputeTariff(state); //     Compute the utility bills

        EMSManager::checkForUnusedActuatorsAtEnd(state);
        EMSManager::checkSetpointNodesAtEnd(state);

        ReportForTabularReports(state); // For Energy Meters (could have other things that need to be pushed to after simulation)

        OpenOutputTabularFile(state);

        WriteTabularReports(state); //     Create the tabular reports at completion of each

        WriteTabularTariffReports(state);

        ComputeLifeCycleCostAndReport(state); // must be after WriteTabularReports and WriteTabularTariffReports

        CloseOutputTabularFile(state);

        DumpAirLoopStatistics(state); // Dump runtime statistics for air loop controller simulation to csv file

#ifdef EP_Detailed_Timings
        epStopTime("Closeout Reporting=");
#endif
        CloseOutputFiles(state);

        // state.dataSQLiteProcedures->sqlite->createZoneExtendedOutput();
        CreateSQLiteZoneExtendedOutput(state);

        if (state.dataSQLiteProcedures->sqlite) {
            DisplayString(state, "Writing final SQL reports");
            state.dataSQLiteProcedures->sqlite->sqliteCommit();      // final transactions
            state.dataSQLiteProcedures->sqlite->initializeIndexes(); // do not create indexes (SQL) until all is done.
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Error condition occurred.  Previous Severe Errors cause termination.");
        }
    }

    void GetProjectData(EnergyPlusData &state)
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

        // Using/Aliasing
        using DataStringGlobals::MatchVersion;
        using namespace DataSystemVariables;
        auto &deviationFromSetPtThresholdClg = state.dataHVACGlobal->deviationFromSetPtThresholdClg;
        auto &deviationFromSetPtThresholdHtg = state.dataHVACGlobal->deviationFromSetPtThresholdHtg;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_int const Div60(12, {1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string Alphas(10);
        Array1D<Real64> Number(4);
        int NumAlpha;
        int NumNumber;
        int IOStat;
        int NumDebugOut;
        int MinInt;
        int Num;
        int Which;
        bool ErrorsFound;
        int NumRunControl;
        std::string VersionID;
        std::string CurrentModuleObject;
        bool CondFDAlgo;
        int Item;

        ErrorsFound = false;

        CurrentModuleObject = "Version";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            std::string::size_type const lenVer(len(MatchVersion));
            if ((lenVer > 0) && (MatchVersion[lenVer - 1] == '0')) {
                Which = static_cast<int>(index(Alphas(1).substr(0, lenVer - 2), MatchVersion.substr(0, lenVer - 2)));
            } else {
                Which = static_cast<int>(index(Alphas(1), MatchVersion));
            }
            if (Which != 0) {
                ShowWarningError(state, CurrentModuleObject + ": in IDF=\"" + Alphas(1) + "\" not the same as expected=\"" + MatchVersion + "\"");
            }
            VersionID = Alphas(1);
        } else if (Num == 0) {
            ShowWarningError(state, CurrentModuleObject + ": missing in IDF, processing for EnergyPlus version=\"" + MatchVersion + "\"");
        } else {
            ShowSevereError(state, "Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        // Do Mini Gets on HB Algorithm and by-surface overrides
        CurrentModuleObject = "HeatBalanceAlgorithm";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        CondFDAlgo = false;
        if (Num > 0) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            {
                auto const SELECT_CASE_var(Alphas(1));
                if ((SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") || (SELECT_CASE_var == "CONDFD") ||
                    (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCEDETAILED") || (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCESIMPLIFIED")) {
                    CondFDAlgo = true;
                } else {
                }
            }
        }
        CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         Alphas,
                                                                         NumAlpha,
                                                                         Number,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(Alphas(2));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;

                    } else {
                    }
                }
            }
        }
        CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         1,
                                                                         Alphas,
                                                                         NumAlpha,
                                                                         Number,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(Alphas(3));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;
                    } else {
                    }
                }
            }
        }
        CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         1,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlpha,
                                                                         Number,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;
                    } else {
                    }
                }
            }
        }
        CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         1,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlpha,
                                                                         Number,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;
                    } else {
                    }
                }
            }
        }

        CurrentModuleObject = "Timestep";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            state.dataGlobal->NumOfTimeStepInHour = Number(1);
            if (state.dataGlobal->NumOfTimeStepInHour <= 0 || state.dataGlobal->NumOfTimeStepInHour > 60) {
                Alphas(1) = fmt::to_string(state.dataGlobal->NumOfTimeStepInHour);
                ShowWarningError(state, CurrentModuleObject + ": Requested number (" + Alphas(1) + ") invalid, Defaulted to 4");
                state.dataGlobal->NumOfTimeStepInHour = 4;
            } else if (mod(60, state.dataGlobal->NumOfTimeStepInHour) != 0) {
                MinInt = 9999;
                for (Num = 1; Num <= 12; ++Num) {
                    if (std::abs(state.dataGlobal->NumOfTimeStepInHour - Div60(Num)) > MinInt) continue;
                    MinInt = state.dataGlobal->NumOfTimeStepInHour - Div60(Num);
                    Which = Num;
                }
                ShowWarningError(state,
                                 format("{}: Requested number ({}) not evenly divisible into 60, defaulted to nearest ({}).",
                                        CurrentModuleObject,
                                        state.dataGlobal->NumOfTimeStepInHour,
                                        Div60(Which)));
                state.dataGlobal->NumOfTimeStepInHour = Div60(Which);
            }
            if (CondFDAlgo && state.dataGlobal->NumOfTimeStepInHour < 20) {
                ShowWarningError(state,
                                 format("{}: Requested number ({}) cannot be used when Conduction Finite Difference algorithm is selected.",
                                        CurrentModuleObject,
                                        state.dataGlobal->NumOfTimeStepInHour));
                ShowContinueError(state, "..." + CurrentModuleObject + " is set to 20.");
                state.dataGlobal->NumOfTimeStepInHour = 20;
            }
            if (state.dataGlobal->NumOfTimeStepInHour < 4 && state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Zone") > 0) {
                ShowWarningError(state,
                                 format("{}: Requested number ({}) is less than the suggested minimum of 4.",
                                        CurrentModuleObject,
                                        state.dataGlobal->NumOfTimeStepInHour));
                ShowContinueError(state,
                                  "Please see entry for " + CurrentModuleObject + " in Input/Output Reference for discussion of considerations.");
            }
        } else if (Num == 0 && state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Zone") > 0 && !CondFDAlgo) {
            ShowWarningError(state, "No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 4.");
            state.dataGlobal->NumOfTimeStepInHour = 4;
        } else if (Num == 0 && !CondFDAlgo) {
            state.dataGlobal->NumOfTimeStepInHour = 4;
        } else if (Num == 0 && state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Zone") > 0 && CondFDAlgo) {
            ShowWarningError(state, "No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 20.");
            ShowContinueError(state, "...Due to presence of Conduction Finite Difference Algorithm selection.");
            state.dataGlobal->NumOfTimeStepInHour = 20;
        } else if (Num == 0 && CondFDAlgo) {
            state.dataGlobal->NumOfTimeStepInHour = 20;
        } else {
            ShowSevereError(state, "Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        state.dataGlobal->TimeStepZone = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
        state.dataGlobal->MinutesPerTimeStep = state.dataGlobal->TimeStepZone * 60;
        state.dataGlobal->TimeStepZoneSec = state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;

        CurrentModuleObject = "ConvergenceLimits";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            MinInt = int(Number(1));
            if (MinInt > state.dataGlobal->MinutesPerTimeStep) {
                MinInt = state.dataGlobal->MinutesPerTimeStep;
            }
            if (MinInt < 0 || MinInt > 60) {
                ShowWarningError(
                    state,
                    format(
                        "{}: Requested {} ({}) invalid. Set to 1 minute.", CurrentModuleObject, state.dataIPShortCut->cNumericFieldNames(1), MinInt));
                state.dataConvergeParams->MinTimeStepSys = 1.0 / 60.0;
            } else if (MinInt == 0) { // Set to TimeStepZone
                state.dataConvergeParams->MinTimeStepSys = state.dataGlobal->TimeStepZone;
            } else {
                state.dataConvergeParams->MinTimeStepSys = double(MinInt) / 60.0;
            }
            state.dataConvergeParams->MaxIter = int(Number(2));
            if (state.dataConvergeParams->MaxIter <= 0) {
                state.dataConvergeParams->MaxIter = 20;
            }
            if (!state.dataIPShortCut->lNumericFieldBlanks(3)) state.dataConvergeParams->MinPlantSubIterations = int(Number(3));
            if (!state.dataIPShortCut->lNumericFieldBlanks(4)) state.dataConvergeParams->MaxPlantSubIterations = int(Number(4));
            // trap bad values
            if (state.dataConvergeParams->MinPlantSubIterations < 1) state.dataConvergeParams->MinPlantSubIterations = 1;
            if (state.dataConvergeParams->MaxPlantSubIterations < 3) state.dataConvergeParams->MaxPlantSubIterations = 3;
            if (state.dataConvergeParams->MinPlantSubIterations > state.dataConvergeParams->MaxPlantSubIterations)
                state.dataConvergeParams->MaxPlantSubIterations = state.dataConvergeParams->MinPlantSubIterations + 1;

        } else if (Num == 0) {
            state.dataConvergeParams->MinTimeStepSys = 1.0 / 60.0;
            state.dataConvergeParams->MaxIter = 20;
            state.dataConvergeParams->MinPlantSubIterations = 2;
            state.dataConvergeParams->MaxPlantSubIterations = 8;
        } else {
            ShowSevereError(state, "Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        state.dataHVACGlobal->LimitNumSysSteps = int(state.dataGlobal->TimeStepZone / state.dataConvergeParams->MinTimeStepSys);

        state.dataReportFlag->DebugOutput = false;
        state.dataReportFlag->EvenDuringWarmup = false;
        CurrentModuleObject = "Output:DebuggingData";
        NumDebugOut = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumDebugOut > 1) {
            ShowWarningError(state, CurrentModuleObject + ": More than 1 occurrence of this object found, only first will be used.");
        }
        if (NumDebugOut > 0) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state, CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat);
            if (NumAlpha >= 1) {
                state.dataReportFlag->DebugOutput = UtilityRoutines::SameString(Alphas(1), "Yes");
            }
            if (NumAlpha >= 2) {
                state.dataReportFlag->EvenDuringWarmup = UtilityRoutines::SameString(Alphas(2), "Yes");
            }
        }

        {
            CurrentModuleObject = "Output:Diagnostics";
            Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
            if (Num > 1) {
                // Let it slide, but warn
                // ErrorsFound = true;
                ShowWarningError(state, CurrentModuleObject + ": More than 1 occurrence of this object found, only first will be used.");
            }
            auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);

            if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
                auto &instancesValue = instances.value();
                for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                    auto const &fields = instance.value();
                    auto const &thisObjectName = instance.key();
                    state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, thisObjectName);

                    auto diagnosticsExtensibles = fields.find("diagnostics");
                    if (diagnosticsExtensibles != fields.end()) {
                        auto diagnosticsExtensiblesArray = diagnosticsExtensibles.value();
                        for (auto diagnosticsExtensible : diagnosticsExtensiblesArray) {

                            // We want to avoid cryptic failures such as this one: "[json.exception.out_of_range.403] key 'key' not found"
                            // Which happens if you put an "empty" entry in the extensible portion
                            auto it = diagnosticsExtensible.find("key");
                            if (it == diagnosticsExtensible.end()) {
                                ShowWarningError(state, CurrentModuleObject + ": empty key found, consider removing it to avoid this warning.");
                                continue;
                            }
                            std::string diagnosticName = it->get<std::string>();

                            if (UtilityRoutines::SameString(diagnosticName, "DisplayExtraWarnings")) {
                                state.dataGlobal->DisplayExtraWarnings = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayAdvancedReportVariables")) {
                                state.dataGlobal->DisplayAdvancedReportVariables = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayAllWarnings")) {
                                state.dataGlobal->DisplayAllWarnings = true;
                                state.dataGlobal->DisplayExtraWarnings = true;
                                state.dataGlobal->DisplayUnusedObjects = true;
                                state.dataGlobal->DisplayUnusedSchedules = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayUnusedObjects")) {
                                state.dataGlobal->DisplayUnusedObjects = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayUnusedSchedules")) {
                                state.dataGlobal->DisplayUnusedSchedules = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayZoneAirHeatBalanceOffBalance")) {
                                state.dataGlobal->DisplayZoneAirHeatBalanceOffBalance = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DoNotMirrorDetachedShading")) {
                                state.dataReportFlag->MakeMirroredDetachedShading = false;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DoNotMirrorAttachedShading")) {
                                state.dataReportFlag->MakeMirroredAttachedShading = false;
                            } else if (UtilityRoutines::SameString(diagnosticName, "ReportDuringWarmup")) {
                                state.dataSysVars->ReportDuringWarmup = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DisplayWeatherMissingDataWarnings")) {
                                state.dataEnvrn->DisplayWeatherMissingDataWarnings = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "IgnoreSolarRadiation")) { // TODO: Not a valid key choice
                                state.dataEnvrn->IgnoreSolarRadiation = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "IgnoreBeamRadiation")) { // TODO: Not a valid key choice
                                state.dataEnvrn->IgnoreBeamRadiation = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "IgnoreDiffuseRadiation")) { // TODO: Not a valid key choice
                                state.dataEnvrn->IgnoreDiffuseRadiation = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "DeveloperFlag")) { // TODO: Not a valid key choice
                                state.dataSysVars->DeveloperFlag = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "TimingFlag")) { // TODO: Not a valid key choice
                                state.dataSysVars->TimingFlag = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "ReportDetailedWarmupConvergence")) {
                                state.dataSysVars->ReportDetailedWarmupConvergence = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "ReportDuringHVACSizingSimulation")) {
                                state.dataSysVars->ReportDuringHVACSizingSimulation = true;
                            } else if (UtilityRoutines::SameString(diagnosticName, "CreateMinimalSurfaceVariables")) { // TODO: Not a valid key choice
                                continue;
                                //        CreateMinimalSurfaceVariables=.TRUE.
                            } else if (UtilityRoutines::SameString(diagnosticName, "CreateNormalSurfaceVariables")) { // TODO: Not a valid key choice
                                continue;
                                //        IF (CreateMinimalSurfaceVariables) THEN
                                //          CALL ShowWarningError(state, 'GetProjectData: '//TRIM(CurrentModuleObject)//'=''//  &
                                //             TRIM(diagnosticName)//'', prior set=true for this condition reverts to false.')
                                //        ENDIF
                                //        CreateMinimalSurfaceVariables=.FALSE.
                            } else if (!diagnosticName.empty()) {
                                ShowWarningError(state,
                                                 "GetProjectData: " + CurrentModuleObject + "=\"" + diagnosticName +
                                                     "\", Invalid value for field, entered value ignored.");
                            }
                        }
                    }

                    // Don't process the duplicate ones
                    break;
                }
            }
        }

        CurrentModuleObject = "OutputControl:ReportingTolerances";
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 0) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (!state.dataIPShortCut->lNumericFieldBlanks(1)) {
                deviationFromSetPtThresholdHtg = -Number(1);
            } else {
                deviationFromSetPtThresholdHtg = -0.2;
            }
            if (!state.dataIPShortCut->lNumericFieldBlanks(2)) {
                deviationFromSetPtThresholdClg = Number(2);
            } else {
                deviationFromSetPtThresholdClg = 0.2;
            }
        }

        state.dataGlobal->DoZoneSizing = false;
        state.dataGlobal->DoSystemSizing = false;
        state.dataGlobal->DoPlantSizing = false;
        state.dataGlobal->DoDesDaySim = true;
        state.dataGlobal->DoWeathSim = true;
        state.dataGlobal->DoHVACSizingSimulation = false;
        state.dataGlobal->HVACSizingSimMaxIterations = 0;
        CurrentModuleObject = "SimulationControl";
        NumRunControl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumRunControl > 0) {
            state.dataSimulationManager->RunControlInInput = true;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     1,
                                                                     Alphas,
                                                                     NumAlpha,
                                                                     Number,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (Alphas(1) == "YES") state.dataGlobal->DoZoneSizing = true;
            if (Alphas(2) == "YES") state.dataGlobal->DoSystemSizing = true;
            if (Alphas(3) == "YES") state.dataGlobal->DoPlantSizing = true;
            if (Alphas(4) == "NO") state.dataGlobal->DoDesDaySim = false;
            if (Alphas(5) == "NO") state.dataGlobal->DoWeathSim = false;
            if (NumAlpha > 5) {
                if (Alphas(6) == "YES") state.dataGlobal->DoHVACSizingSimulation = true;
            }
        }
        if (state.dataSysVars->DDOnly) {
            state.dataGlobal->DoDesDaySim = true;
            state.dataGlobal->DoWeathSim = false;
        }
        if (state.dataSysVars->FullAnnualRun) {
            state.dataGlobal->DoDesDaySim = false;
            state.dataGlobal->DoWeathSim = true;
        }

        CurrentModuleObject = "PerformancePrecisionTradeoffs";
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (Num > 1) {
            ErrorsFound = true;
            ShowFatalError(state, "GetProjectData: Only one (\"1\") " + CurrentModuleObject + " object per simulation is allowed.");
        }
        state.dataGlobal->createPerfLog = Num > 0;
        std::string overrideModeValue = "Normal";
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, thisObjectName);
                if (fields.find("use_coil_direct_solutions") != fields.end()) {
                    state.dataGlobal->DoCoilDirectSolutions =
                        UtilityRoutines::MakeUPPERCase(fields.at("use_coil_direct_solutions").get<std::string>()) == "YES";
                }
                if (fields.find("zone_radiant_exchange_algorithm") != fields.end()) {
                    state.dataHeatBalIntRadExchg->CarrollMethod =
                        UtilityRoutines::MakeUPPERCase(fields.at("zone_radiant_exchange_algorithm").get<std::string>()) == "CARROLLMRT";
                }
                if (fields.find("use_representative_surfaces_for_calculations") != fields.end()) {
                    state.dataSurface->UseRepresentativeSurfaceCalculations =
                        UtilityRoutines::MakeUPPERCase(fields.at("use_representative_surfaces_for_calculations").get<std::string>()) == "YES";
                }
                bool overrideTimestep(false);
                bool overrideZoneAirHeatBalAlg(false);
                bool overrideMinNumWarmupDays(false);
                bool overrideBeginEnvResetSuppress(false);
                bool overrideMaxZoneTempDiff(false);
                bool overrideSystemTimestep(false);
                bool overrideMaxAllowedDelTemp(false);
                bool overridePsychTsatFnPb(false);
                state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded = true;
                if (fields.find("override_mode") != fields.end()) {
                    overrideModeValue = UtilityRoutines::MakeUPPERCase(fields.at("override_mode").get<std::string>());
                    if (overrideModeValue == "NORMAL") {
                        // no overrides
                    } else if (overrideModeValue == "MODE01") {
                        // Zone Time step (TimeStep object) will be set to one timestep per hour
                        overrideTimestep = true;
                    } else if (overrideModeValue == "MODE02") {
                        // Mode01 plus ZoneAirHeatBalanceAlgorithm will be set to Euler
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                    } else if (overrideModeValue == "MODE03") {
                        // Mode02 plus Minimum Number of Warmup Days will be set to 1
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                    } else if (overrideModeValue == "MODE04") {
                        // Mode03 plus Begin Environment Reset Mode will be set to SuppressAllBeginEnvironmentResets
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                        overrideBeginEnvResetSuppress = true;
                    } else if (overrideModeValue == "MODE05") {
                        // Mode04 plus Minimun System Timestep will be set to 1hr
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                        overrideBeginEnvResetSuppress = true;
                        overrideSystemTimestep = true;
                    } else if (overrideModeValue == "MODE06") {
                        // Mode05 plus cubic spline interpolations in replacement of the original psychrometric function PsychTsatFnPb
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                        overrideBeginEnvResetSuppress = true;
                        overrideSystemTimestep = true;
                        overridePsychTsatFnPb = true;
                    } else if (overrideModeValue == "MODE07") {
                        // Mode06 plus internal variable MaxZoneTempDiff will be set to 1.00
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                        overrideBeginEnvResetSuppress = true;
                        overrideSystemTimestep = true;
                        overrideMaxZoneTempDiff = true;
                        overridePsychTsatFnPb = true;
                    } else if (overrideModeValue == "MODE08") {
                        // Mode07 plus internal variable MaxAllowedDelTemp will be set to 0.1
                        overrideTimestep = true;
                        overrideZoneAirHeatBalAlg = true;
                        overrideMinNumWarmupDays = true;
                        overrideBeginEnvResetSuppress = true;
                        overrideSystemTimestep = true;
                        overrideMaxZoneTempDiff = true;
                        overrideMaxAllowedDelTemp = true;
                        overridePsychTsatFnPb = true;
                    } else if (overrideModeValue == "ADVANCED") {
                        bool advancedModeUsed = false;
                        if (fields.find("maxzonetempdiff") != fields.end()) { // not required field, has default value
                            state.dataConvergeParams->MaxZoneTempDiff = fields.at("maxzonetempdiff").get<Real64>();
                            ShowWarningError(state,
                                             format("PerformancePrecisionTradeoffs using the Advanced Override Mode, MaxZoneTempDiff set to: {:.4R}",
                                                    state.dataConvergeParams->MaxZoneTempDiff));
                            advancedModeUsed = true;
                        }
                        if (fields.find("maxalloweddeltemp") != fields.end()) { // not required field, has default value
                            state.dataHeatBal->MaxAllowedDelTemp = fields.at("maxalloweddeltemp").get<Real64>();
                            ShowWarningError(
                                state,
                                format("PerformancePrecisionTradeoffs using the Advanced Override Mode, MaxAllowedDelTemp set to: {:.4R}",
                                       state.dataHeatBal->MaxAllowedDelTemp));
                            advancedModeUsed = true;
                        }
                        if (advancedModeUsed) {
                            ShowContinueError(state,
                                              "...Care should be used when using the Advanced Override Mode. Results may be significantly different "
                                              "than a simulation not using this mode.");
                        } else {
                            ShowWarningError(
                                state, "PerformancePrecisionTradeoffs using the Advanced Override Mode but no specific parameters have been set.");
                        }
                    } else {
                        ShowSevereError(state, "Invalid over ride mode specified in PerformancePrecisionTradeoffs object: " + overrideModeValue);
                    }

                    if (overrideTimestep) {
                        ShowWarningError(state, "Due to PerformancePrecisionTradeoffs Override Mode, the Number of TimeSteps has been changed to 1.");
                        state.dataGlobal->NumOfTimeStepInHour = 1;
                        state.dataGlobal->TimeStepZone = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
                        state.dataGlobal->MinutesPerTimeStep = state.dataGlobal->TimeStepZone * 60;
                        state.dataGlobal->TimeStepZoneSec = state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;
                    }
                    if (overrideZoneAirHeatBalAlg) {
                        ShowWarningError(
                            state,
                            "Due to PerformancePrecisionTradeoffs Override Mode, the ZoneAirHeatBalanceAlgorithm has been changed to EulerMethod.");
                        state.dataHeatBal->OverrideZoneAirSolutionAlgo = true;
                    }
                    if (overrideMinNumWarmupDays) {
                        ShowWarningError(
                            state, "Due to PerformancePrecisionTradeoffs Override Mode, the Minimum Number of Warmup Days has been changed to 1.");
                        state.dataHeatBal->MinNumberOfWarmupDays = 1;
                    }
                    if (overrideBeginEnvResetSuppress) {
                        ShowWarningError(state,
                                         "Due to PerformancePrecisionTradeoffs Override Mode, the Begin Environment Reset Mode has been changed to "
                                         "SuppressAllBeginEnvironmentResets.");
                        state.dataEnvrn->forceBeginEnvResetSuppress = true;
                    }
                    if (overrideSystemTimestep) {
                        ShowWarningError(
                            state, "Due to PerformancePrecisionTradeoffs Override Mode, the minimum System TimeSteps has been changed to 1 hr.");
                        int MinTimeStepSysOverrideValue = 60.0;
                        if (MinTimeStepSysOverrideValue > state.dataGlobal->MinutesPerTimeStep) {
                            MinTimeStepSysOverrideValue = state.dataGlobal->MinutesPerTimeStep;
                        }
                        state.dataConvergeParams->MinTimeStepSys = MinTimeStepSysOverrideValue / 60.0;
                        state.dataHVACGlobal->LimitNumSysSteps = int(state.dataGlobal->TimeStepZone / state.dataConvergeParams->MinTimeStepSys);
                    }
                    if (overridePsychTsatFnPb) {
                        ShowWarningError(state,
                                         "Due to PerformancePrecisionTradeoffs Override Mode, the saturated temperature will be calculated using "
                                         "cubic spline interpolations in replacement of PsychTsatFnPb .");
                        // Mode06 CSpline interpolation (64 Pa bin size + 20/16 bit)
                        state.dataPsychrometrics->useInterpolationPsychTsatFnPb = true;
#ifdef EP_cache_PsyTsatFnPb
                        state.dataPsychCache->tsatprecision_bits = 20;
#endif
                    }
                    if (overrideMaxZoneTempDiff) {
                        ShowWarningError(
                            state, "Due to PerformancePrecisionTradeoffs Override Mode, internal variable MaxZoneTempDiff will be set to 1.0 .");
                        state.dataConvergeParams->MaxZoneTempDiff = 1.0;
                    }
                    if (overrideMaxAllowedDelTemp) {
                        ShowWarningError(
                            state, "Due to PerformancePrecisionTradeoffs Override Mode, internal variable MaxAllowedDelTemp will be set to 0.1 .");
                        state.dataHeatBal->MaxAllowedDelTemp = 0.1;
                    }
                }
            }
        }
        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting Project Input");
        }

        print(state.files.eio, "{}\n", "! <Version>, Version ID");
        static constexpr std::string_view Format_721(" Version, {}\n");
        print(state.files.eio, Format_721, VersionID);

        print(state.files.eio, "{}\n", "! <Timesteps per Hour>, #TimeSteps, Minutes per TimeStep {minutes}");
        static constexpr std::string_view Format_731(" Timesteps per Hour, {:2}, {:2}\n");
        print(state.files.eio, Format_731, state.dataGlobal->NumOfTimeStepInHour, state.dataGlobal->MinutesPerTimeStep);

        print(state.files.eio,
              "{}\n",
              "! <System Convergence Limits>, Minimum System TimeStep {minutes}, Max HVAC Iterations, Minimum Plant "
              "Iterations, Maximum Plant Iterations");
        MinInt = state.dataConvergeParams->MinTimeStepSys * 60.0;
        static constexpr std::string_view Format_733(" System Convergence Limits, {}, {}, {}, {}\n");
        print(state.files.eio,
              Format_733,
              MinInt,
              state.dataConvergeParams->MaxIter,
              state.dataConvergeParams->MinPlantSubIterations,
              state.dataConvergeParams->MaxPlantSubIterations);

        if (state.dataGlobal->DoZoneSizing) {
            Alphas(1) = "Yes";
        } else {
            Alphas(1) = "No";
        }
        if (state.dataGlobal->DoSystemSizing) {
            Alphas(2) = "Yes";
        } else {
            Alphas(2) = "No";
        }
        if (state.dataGlobal->DoPlantSizing) {
            Alphas(3) = "Yes";
        } else {
            Alphas(3) = "No";
        }
        if (state.dataGlobal->DoDesDaySim) {
            Alphas(4) = "Yes";
        } else {
            Alphas(4) = "No";
        }
        if (state.dataGlobal->DoWeathSim) {
            Alphas(5) = "Yes";
        } else {
            Alphas(5) = "No";
        }
        if (state.dataGlobal->DoHVACSizingSimulation) {
            Alphas(6) = "Yes";
            if (NumNumber >= 1) {
                state.dataGlobal->HVACSizingSimMaxIterations = Number(1);
            }
        } else {
            Alphas(6) = "No";
        }

        print(state.files.eio,
              "{}\n",
              "! <Simulation Control>, Do Zone Sizing, Do System Sizing, Do Plant Sizing, Do Design Days, Do Weather "
              "Simulation, Do HVAC Sizing Simulation");
        print(state.files.eio, " Simulation Control");
        for (Num = 1; Num <= 6; ++Num) {
            print(state.files.eio, ", {}", Alphas(Num));
        }
        print(state.files.eio, "\n");

        // Performance Precision Tradeoffs
        if (state.dataGlobal->DoCoilDirectSolutions) {
            Alphas(1) = "Yes";
            ShowWarningError(state, "PerformancePrecisionTradeoffs: Coil Direct Solution simulation is selected.");
        } else {
            Alphas(1) = "No";
        }
        if (state.dataHeatBalIntRadExchg->CarrollMethod) {
            Alphas(2) = "CarrollMRT";
            ShowWarningError(state, "PerformancePrecisionTradeoffs: Carroll MRT radiant exchange method is selected.");
        } else {
            Alphas(2) = "ScriptF";
        }
        Alphas(3) = overrideModeValue;
        Alphas(4) = fmt::to_string(state.dataGlobal->NumOfTimeStepInHour);
        if (state.dataHeatBal->OverrideZoneAirSolutionAlgo) {
            Alphas(5) = "Yes";
        } else {
            Alphas(5) = "No";
        }
        Alphas(6) = fmt::to_string(state.dataHeatBal->MinNumberOfWarmupDays);
        if (state.dataEnvrn->forceBeginEnvResetSuppress) {
            Alphas(7) = "Yes";
        } else {
            Alphas(7) = "No";
        }
        Alphas(8) = format("{:.1R}", state.dataConvergeParams->MinTimeStepSys * 60.0);
        Alphas(9) = format("{:.3R}", state.dataConvergeParams->MaxZoneTempDiff);
        Alphas(10) = format("{:.4R}", state.dataHeatBal->MaxAllowedDelTemp);
        std::string pptHeader = "! <Performance Precision Tradeoffs>, Use Coil Direct Simulation, "
                                "Zone Radiant Exchange Algorithm, Override Mode, Number of Timestep In Hour, "
                                "Force Euler Method, Minimum Number of Warmup Days, Force Suppress All Begin Environment Resets, "
                                "Minimum System Timestep, MaxZoneTempDiff, MaxAllowedDelTemp";
        print(state.files.eio, "{}\n", pptHeader);
        print(state.files.eio, " Performance Precision Tradeoffs");
        for (Num = 1; Num <= 10; ++Num) {
            print(state.files.eio, ", {}", Alphas(Num));
        }
        print(state.files.eio, "\n");

        print(state.files.eio,
              "{}\n",
              "! <Output Reporting Tolerances>, Tolerance for Time Heating Setpoint Not Met, Tolerance for Zone Cooling Setpoint Not Met Time");
        // Formats
        static constexpr std::string_view Format_751(" Output Reporting Tolerances, {:.3R}, {:.3R}, \n");

        print(state.files.eio, Format_751, std::abs(deviationFromSetPtThresholdHtg), deviationFromSetPtThresholdClg);

        //  IF (DisplayExtraWarnings) THEN
        //    Write(OutputFileInits,740)
        //    Write(OutputFileInits,741) (TRIM(Alphas(Num)),Num=1,5)
        // 742 Format('! <Display Extra Warnings>, Display Advanced Report Variables, Do Not Mirror Detached Shading')
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
        // unused0909743 Format(' Display Extra Warnings',2(', ',A))
        //  ENDIF
        if (state.dataGlobal->createPerfLog) {
            writeIntialPerfLogValues(state, overrideModeValue);
        }
    }

    void writeIntialPerfLogValues(EnergyPlusData &state, std::string const &currentOverrideModeValue)
    // write the input related portions of the .perflog
    // J.Glazer February 2020
    {
        UtilityRoutines::appendPerfLog(state,
                                       "Program, Version, TimeStamp",
                                       state.dataStrGlobals->VerStringVar); // this string already includes three portions and has commas
        UtilityRoutines::appendPerfLog(state, "Use Coil Direct Solution", bool_to_string(state.dataGlobal->DoCoilDirectSolutions));
        if (state.dataHeatBalIntRadExchg->CarrollMethod) {
            UtilityRoutines::appendPerfLog(state, "Zone Radiant Exchange Algorithm", "CarrollMRT");
        } else {
            UtilityRoutines::appendPerfLog(state, "Zone Radiant Exchange Algorithm", "ScriptF");
        }
        UtilityRoutines::appendPerfLog(state, "Override Mode", currentOverrideModeValue);
        UtilityRoutines::appendPerfLog(state, "Number of Timesteps per Hour", fmt::to_string(state.dataGlobal->NumOfTimeStepInHour));
        UtilityRoutines::appendPerfLog(state, "Minimum Number of Warmup Days", fmt::to_string(state.dataHeatBal->MinNumberOfWarmupDays));
        UtilityRoutines::appendPerfLog(state, "SuppressAllBeginEnvironmentResets", bool_to_string(state.dataEnvrn->forceBeginEnvResetSuppress));
        UtilityRoutines::appendPerfLog(state, "Minimum System Timestep", format("{:.1R}", state.dataConvergeParams->MinTimeStepSys * 60.0));
        UtilityRoutines::appendPerfLog(state, "MaxZoneTempDiff", format("{:.2R}", state.dataConvergeParams->MaxZoneTempDiff));
        UtilityRoutines::appendPerfLog(state, "MaxAllowedDelTemp", format("{:.4R}", state.dataHeatBal->MaxAllowedDelTemp));
    }

    std::string bool_to_string(bool logical)
    {
        if (logical) {
            return ("True");
        } else {
            return ("False");
        }
    }

    void CheckForMisMatchedEnvironmentSpecifications(EnergyPlusData &state)
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
        NumZoneSizing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Sizing:Zone");
        NumSystemSizing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Sizing:System");
        NumPlantSizing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Sizing:Plant");
        NumDesignDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay");
        NumRunPeriodDesign = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays") +
                             state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        NumSizingDays = NumDesignDays + NumRunPeriodDesign;

        WeatherFileAttached = FileSystem::fileExists(state.files.inputWeatherFilePath.filePath);

        if (state.dataSimulationManager->RunControlInInput) {
            if (state.dataGlobal->DoZoneSizing) {
                if (NumZoneSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError(
                        state, "CheckEnvironmentSpecifications: Sizing for Zones has been requested but there are no design environments specified.");
                    ShowContinueError(state, "...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumZoneSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    "CheckEnvironmentSpecifications: Sizing for Zones has been requested; Design period from the weather file "
                                    "requested; but no weather file specified.");
                }
            }
            if (state.dataGlobal->DoSystemSizing) {
                if (NumSystemSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError(
                        state,
                        "CheckEnvironmentSpecifications: Sizing for Systems has been requested but there are no design environments specified.");
                    ShowContinueError(state, "...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumSystemSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    "CheckEnvironmentSpecifications: Sizing for Systems has been requested; Design period from the weather file "
                                    "requested; but no weather file specified.");
                }
            }
            if (state.dataGlobal->DoPlantSizing) {
                if (NumPlantSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    "CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested but there are no design "
                                    "environments specified.");
                    ShowContinueError(state, "...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumPlantSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    "CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested; Design period from the weather "
                                    "file requested; but no weather file specified.");
                }
            }
            if (state.dataGlobal->DoDesDaySim && NumSizingDays == 0) {
                ShowWarningError(state,
                                 "CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations, but no design "
                                 "environments specified.");
                ShowContinueError(
                    state,
                    "...No design environment results produced. For these results, add appropriate SizingPeriod:* objects for your simulation.");
            }
            if (state.dataGlobal->DoDesDaySim && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                ErrorsFound = true;
                ShowSevereError(state,
                                "CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations; weather file design "
                                "environments specified; but no weather file specified.");
            }
            if (state.dataGlobal->DoWeathSim && !state.dataSimulationManager->RunPeriodsInInput) {
                ShowWarningError(state,
                                 "CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations, but no run periods for "
                                 "weather file specified.  No annual results produced.");
            }
            if (state.dataGlobal->DoWeathSim && state.dataSimulationManager->RunPeriodsInInput && !WeatherFileAttached) {
                ShowWarningError(state,
                                 "CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations; run periods for weather "
                                 "file specified; but no weather file specified.");
            }
        }
        if (!state.dataGlobal->DoDesDaySim && !state.dataGlobal->DoWeathSim) {
            ShowWarningError(state,
                             "\"Do the design day simulations\" and \"Do the weather file simulation\" are both set to \"No\".  No simulations will "
                             "be performed, and most input will not be read.");
        }
        if (!state.dataGlobal->DoZoneSizing && !state.dataGlobal->DoSystemSizing && !state.dataGlobal->DoPlantSizing &&
            !state.dataGlobal->DoDesDaySim && !state.dataGlobal->DoWeathSim) {
            ShowSevereError(state, "All elements of SimulationControl are set to \"No\". No simulations can be done.  Program terminates.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Program terminates due to preceding conditions.");
        }
    }

    void CheckForRequestedReporting(EnergyPlusData &state)
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool SimPeriods;
        bool ReportingRequested;

        ReportingRequested = false;
        SimPeriods = (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay") > 0 ||
                      state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays") > 0 ||
                      state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType") > 0 ||
                      state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RunPeriod") > 0);

        if ((state.dataGlobal->DoDesDaySim || state.dataGlobal->DoWeathSim) && SimPeriods) {
            ReportingRequested = (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Table:SummaryReports") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Table:TimeBins") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Table:Monthly") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Variable") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Meter") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Meter:MeterFileOnly") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Meter:Cumulative") > 0 ||
                                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:Meter:Cumulative:MeterFileOnly") > 0);
            // Not testing for : Output:SQLite or Output:EnvironmentalImpactFactors
            if (!ReportingRequested) {
                ShowWarningError(state, "No reporting elements have been requested. No simulation results produced.");
                ShowContinueError(state,
                                  "...Review requirements such as \"Output:Table:SummaryReports\", \"Output:Table:Monthly\", \"Output:Variable\", "
                                  "\"Output:Meter\" and others.");
            }
        }
    }

    std::unique_ptr<std::ostream> OpenStreamFile(EnergyPlusData &state, const fs::path &filePath, std::ios_base::openmode mode)
    {
        auto result = std::make_unique<std::ofstream>(filePath, mode);
        if (!result->good()) {
            ShowFatalError(state, "OpenOutputFiles: Could not open file " + filePath.string() + " for output (write).");
        }
        return result;
    }

    std::unique_ptr<fmt::ostream> OpenFmtStreamFile(EnergyPlusData &state, const fs::path &filePath)
    {
        std::unique_ptr<fmt::ostream> result = nullptr;
#ifdef _WIN32
        auto filePathStr = filePath.string();
        auto path = filePathStr.c_str();
#else
        auto path = filePath.c_str();
#endif
        try {
            auto f = fmt::output_file(path, fmt::buffer_size = (2 << 17));
            result = std::make_unique<fmt::ostream>(std::move(f));
        } catch (const std::system_error &error) {
            ShowSevereError(state, error.what());
            ShowFatalError(state, "OpenOutputFiles: Could not open file " + filePath.string() + " for output (write).");
        }
        return result;
    }

    void OpenOutputFiles(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine opens all of the input and output files needed for
        // an EnergyPlus run.

        state.dataGlobal->StdOutputRecordCount = 0;
        state.files.eso.ensure_open(state, "OpenOutputFiles", state.files.outputControl.eso);
        print(state.files.eso, "Program Version,{}\n", state.dataStrGlobals->VerStringVar);

        // Open the Initialization Output File
        state.files.eio.ensure_open(state, "OpenOutputFiles", state.files.outputControl.eio);
        print(state.files.eio, "Program Version,{}\n", state.dataStrGlobals->VerStringVar);

        // Open the Meters Output File
        state.files.mtr.ensure_open(state, "OpenOutputFiles", state.files.outputControl.mtr);
        print(state.files.mtr, "Program Version,{}\n", state.dataStrGlobals->VerStringVar);

        // Open the Branch-Node Details Output File
        state.files.bnd.ensure_open(state, "OpenOutputFiles", state.files.outputControl.bnd);
        print(state.files.bnd, "Program Version,{}\n", state.dataStrGlobals->VerStringVar);
    }

    void CloseOutputFiles(EnergyPlusData &state)
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
        using namespace DataRuntimeLanguage;
        using namespace DataSystemVariables;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view EndOfDataString("End of Data"); // Signifies the end of the data block in the output file

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string cEnvSetThreads;
        std::string cepEnvSetThreads;
        std::string cIDFSetThreads;

        state.files.audit.ensure_open(state, "CloseOutputFiles", state.files.outputControl.audit);
        constexpr static auto variable_fmt{" {}={:12}\n"};
        // Record some items on the audit file
        print(state.files.audit, variable_fmt, "NumOfRVariable", state.dataOutputProcessor->NumOfRVariable_Setup);
        print(state.files.audit, variable_fmt, "NumOfRVariable(Total)", state.dataOutputProcessor->NumTotalRVariable);
        print(state.files.audit, variable_fmt, "NumOfRVariable(Actual)", state.dataOutputProcessor->NumOfRVariable);
        print(state.files.audit, variable_fmt, "NumOfRVariable(Summed)", state.dataOutputProcessor->NumOfRVariable_Sum);
        print(state.files.audit, variable_fmt, "NumOfRVariable(Meter)", state.dataOutputProcessor->NumOfRVariable_Meter);
        print(state.files.audit, variable_fmt, "NumOfIVariable", state.dataOutputProcessor->NumOfIVariable_Setup);
        print(state.files.audit, variable_fmt, "NumOfIVariable(Total)", state.dataOutputProcessor->NumTotalIVariable);
        print(state.files.audit, variable_fmt, "NumOfIVariable(Actual)", state.dataOutputProcessor->NumOfIVariable);
        print(state.files.audit, variable_fmt, "NumOfIVariable(Summed)", state.dataOutputProcessor->NumOfIVariable_Sum);
        print(state.files.audit, variable_fmt, "MaxRVariable", state.dataOutputProcessor->MaxRVariable);
        print(state.files.audit, variable_fmt, "MaxIVariable", state.dataOutputProcessor->MaxIVariable);
        print(state.files.audit, variable_fmt, "NumEnergyMeters", state.dataOutputProcessor->NumEnergyMeters);
        print(state.files.audit, variable_fmt, "NumVarMeterArrays", state.dataOutputProcessor->NumVarMeterArrays);
        print(state.files.audit, variable_fmt, "maxUniqueKeyCount", state.dataOutRptTab->maxUniqueKeyCount);
        print(state.files.audit, variable_fmt, "maxNumberOfFigures", state.dataSolarShading->maxNumberOfFigures);
        print(state.files.audit, variable_fmt, "MAXHCArrayBounds", state.dataSolarShading->MAXHCArrayBounds);
        print(state.files.audit, variable_fmt, "MaxVerticesPerSurface", state.dataSurface->MaxVerticesPerSurface);
        print(state.files.audit, variable_fmt, "NumReportList", state.dataOutputProcessor->NumReportList);
        print(state.files.audit, variable_fmt, "InstMeterCacheSize", state.dataOutputProcessor->InstMeterCacheSize);
        if (state.dataSysVars->SutherlandHodgman) {
            if (state.dataSysVars->SlaterBarsky) {
                print(state.files.audit, " {}\n", "ClippingAlgorithm=SlaterBarskyandSutherlandHodgman");
            } else {
                print(state.files.audit, " {}\n", "ClippingAlgorithm=SutherlandHodgman");
            }
        } else {
            print(state.files.audit, "{}\n", "ClippingAlgorithm=ConvexWeilerAtherton");
        }
        print(state.files.audit, variable_fmt, "MonthlyFieldSetInputCount", state.dataOutRptTab->MonthlyFieldSetInputCount);
        print(state.files.audit, variable_fmt, "NumConsideredOutputVariables", state.dataOutput->NumConsideredOutputVariables);
        print(state.files.audit, variable_fmt, "MaxConsideredOutputVariables", state.dataOutput->MaxConsideredOutputVariables);

        print(state.files.audit, variable_fmt, "numActuatorsUsed", state.dataRuntimeLang->numActuatorsUsed);
        print(state.files.audit, variable_fmt, "numEMSActuatorsAvailable", state.dataRuntimeLang->numEMSActuatorsAvailable);
        print(state.files.audit, variable_fmt, "maxEMSActuatorsAvailable", state.dataRuntimeLang->maxEMSActuatorsAvailable);
        print(state.files.audit, variable_fmt, "numInternalVariablesUsed", state.dataRuntimeLang->NumInternalVariablesUsed);
        print(state.files.audit, variable_fmt, "numEMSInternalVarsAvailable", state.dataRuntimeLang->numEMSInternalVarsAvailable);
        print(state.files.audit, variable_fmt, "maxEMSInternalVarsAvailable", state.dataRuntimeLang->maxEMSInternalVarsAvailable);

        print(state.files.audit, variable_fmt, "NumOfNodeConnections", state.dataBranchNodeConnections->NumOfNodeConnections);
        print(state.files.audit, variable_fmt, "MaxNumOfNodeConnections", state.dataBranchNodeConnections->MaxNumOfNodeConnections);
#ifdef EP_Count_Calls
        print(state.files.audit, variable_fmt, "NumShadow_Calls", state.dataTimingsData->NumShadow_Calls);
        print(state.files.audit, variable_fmt, "NumShadowAtTS_Calls", state.dataTimingsData->NumShadowAtTS_Calls);
        print(state.files.audit, variable_fmt, "NumClipPoly_Calls", state.dataTimingsData->NumClipPoly_Calls);
        print(state.files.audit, variable_fmt, "NumInitSolar_Calls", state.dataTimingsData->NumInitSolar_Calls);
        print(state.files.audit, variable_fmt, "NumAnisoSky_Calls", state.dataTimingsData->NumAnisoSky_Calls);
        print(state.files.audit, variable_fmt, "NumDetPolyOverlap_Calls", state.dataTimingsData->NumDetPolyOverlap_Calls);
        print(state.files.audit, variable_fmt, "NumCalcPerSolBeam_Calls", state.dataTimingsData->NumCalcPerSolBeam_Calls);
        print(state.files.audit, variable_fmt, "NumDetShadowCombs_Calls", state.dataTimingsData->NumDetShadowCombs_Calls);
        print(state.files.audit, variable_fmt, "NumIntSolarDist_Calls", state.dataTimingsData->NumIntSolarDist_Calls);
        print(state.files.audit, variable_fmt, "NumIntRadExchange_Calls", state.dataTimingsData->NumIntRadExchange_Calls);
        print(state.files.audit, variable_fmt, "NumIntRadExchangeZ_Calls", state.dataTimingsData->NumIntRadExchangeZ_Calls);
        print(state.files.audit, variable_fmt, "NumIntRadExchangeMain_Calls", state.dataTimingsData->NumIntRadExchangeMain_Calls);
        print(state.files.audit, variable_fmt, "NumIntRadExchangeOSurf_Calls", state.dataTimingsData->NumIntRadExchangeOSurf_Calls);
        print(state.files.audit, variable_fmt, "NumIntRadExchangeISurf_Calls", state.dataTimingsData->NumIntRadExchangeISurf_Calls);
        print(state.files.audit, variable_fmt, "NumMaxInsideSurfIterations", state.dataTimingsData->NumMaxInsideSurfIterations);
        print(state.files.audit, variable_fmt, "NumCalcScriptF_Calls", state.dataTimingsData->NumCalcScriptF_Calls);
#endif

        print(state.files.eso, "{}\n", EndOfDataString);
        if (state.dataGlobal->StdOutputRecordCount > 0) {
            print(state.files.eso, variable_fmt, "Number of Records Written", state.dataGlobal->StdOutputRecordCount);
            state.files.eso.close();
        } else {
            state.files.eso.del();
        }

        if (state.dataHeatBal->AnyCondFD) { // echo out relaxation factor, it may have been changed by the program
            print(
                state.files.eio, "{}\n", "! <ConductionFiniteDifference Numerical Parameters>, Starting Relaxation Factor, Final Relaxation Factor");
            print(state.files.eio,
                  "ConductionFiniteDifference Numerical Parameters, {:.3R}, {:.3R}\n",
                  state.dataHeatBal->CondFDRelaxFactorInput,
                  state.dataHeatBal->CondFDRelaxFactor);
        }
        // Report number of threads to eio file
        static constexpr std::string_view ThreadingHeader(
            "! <Program Control Information:Threads/Parallel Sims>, Threading Supported,Maximum Number of "
            "Threads, Env Set Threads (OMP_NUM_THREADS), EP Env Set Threads (EP_OMP_NUM_THREADS), IDF Set "
            "Threads, Number of Threads Used (Interior Radiant Exchange), Number Nominal Surfaces, Number "
            "Parallel Sims");
        print(state.files.eio, "{}\n", ThreadingHeader);
        static constexpr std::string_view ThreadReport("Program Control:Threads/Parallel Sims, {},{}, {}, {}, {}, {}, {}, {}\n");
        if (state.dataSysVars->Threading) {
            if (state.dataSysVars->iEnvSetThreads == 0) {
                cEnvSetThreads = "Not Set";
            } else {
                cEnvSetThreads = fmt::to_string(state.dataSysVars->iEnvSetThreads);
            }
            if (state.dataSysVars->iepEnvSetThreads == 0) {
                cepEnvSetThreads = "Not Set";
            } else {
                cepEnvSetThreads = fmt::to_string(state.dataSysVars->iepEnvSetThreads);
            }
            if (state.dataSysVars->iIDFSetThreads == 0) {
                cIDFSetThreads = "Not Set";
            } else {
                cIDFSetThreads = fmt::to_string(state.dataSysVars->iIDFSetThreads);
            }
            if (state.dataSysVars->lnumActiveSims) {
                print(state.files.eio,
                      ThreadReport,
                      "Yes",
                      state.dataSysVars->MaxNumberOfThreads,
                      cEnvSetThreads,
                      cepEnvSetThreads,
                      cIDFSetThreads,
                      state.dataSysVars->NumberIntRadThreads,
                      state.dataSysVars->iNominalTotSurfaces,
                      state.dataSysVars->inumActiveSims);
            } else {
                print(state.files.eio,
                      ThreadReport,
                      "Yes",
                      state.dataSysVars->MaxNumberOfThreads,
                      cEnvSetThreads,
                      cepEnvSetThreads,
                      cIDFSetThreads,
                      state.dataSysVars->NumberIntRadThreads,
                      state.dataSysVars->iNominalTotSurfaces,
                      "N/A");
            }
        } else { // no threading
            if (state.dataSysVars->lnumActiveSims) {
                print(state.files.eio,
                      ThreadReport,
                      "No",
                      state.dataSysVars->MaxNumberOfThreads,
                      "N/A",
                      "N/A",
                      "N/A",
                      "N/A",
                      "N/A",
                      state.dataSysVars->inumActiveSims);
            } else {
                print(state.files.eio, ThreadReport, "No", state.dataSysVars->MaxNumberOfThreads, "N/A", "N/A", "N/A", "N/A", "N/A", "N/A");
            }
        }

        // Close the Initialization Output File
        print(state.files.eio, "{}\n", EndOfDataString);
        state.files.eio.close();

        // Close the Meters Output File
        print(state.files.mtr, "{}\n", EndOfDataString);
        print(state.files.mtr, " Number of Records Written={:12}\n", state.dataGlobal->StdMeterRecordCount);
        if (state.dataGlobal->StdMeterRecordCount > 0) {
            state.files.mtr.close();
        } else {
            state.files.mtr.del();
        }

        // Close the External Shading Output File
        state.files.shade.close();
    }

    void SetupSimulation(EnergyPlusData &state, bool &ErrorsFound)
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

        // Using/Aliasing
        using CostEstimateManager::SimCostEstimate;
        using ExteriorEnergyUse::ManageExteriorEnergyUse;

        using PlantPipingSystemsManager::CheckIfAnyBasements;
        using PlantPipingSystemsManager::CheckIfAnySlabs;
        using PlantPipingSystemsManager::SimulateGroundDomains;

        bool Available = true;

        while (Available) { // do for each environment

            GetNextEnvironment(state, Available, ErrorsFound);

            if (!Available) break;
            if (ErrorsFound) break;

            state.dataGlobal->BeginEnvrnFlag = true;
            state.dataGlobal->EndEnvrnFlag = false;
            state.dataEnvrn->EndMonthFlag = false;
            state.dataGlobal->WarmupFlag = true;
            state.dataGlobal->DayOfSim = 0;

            ++state.dataGlobal->DayOfSim;
            state.dataGlobal->BeginDayFlag = true;
            state.dataGlobal->EndDayFlag = false;

            state.dataGlobal->HourOfDay = 1;

            state.dataGlobal->BeginHourFlag = true;
            state.dataGlobal->EndHourFlag = false;

            state.dataGlobal->TimeStep = 1;

            if (state.dataSysVars->DeveloperFlag) DisplayString(state, "Initializing Simulation - timestep 1:" + state.dataEnvrn->EnvironmentName);

            state.dataGlobal->BeginTimeStepFlag = true;

            ManageWeather(state);

            ManageExteriorEnergyUse(state);

            ManageHeatBalance(state);

            state.dataGlobal->BeginHourFlag = false;
            state.dataGlobal->BeginDayFlag = false;
            state.dataGlobal->BeginEnvrnFlag = false;
            state.dataGlobal->BeginSimFlag = false;
            state.dataGlobal->BeginFullSimFlag = false;

            //          ! do another timestep=1
            if (state.dataSysVars->DeveloperFlag)
                DisplayString(state, "Initializing Simulation - 2nd timestep 1:" + state.dataEnvrn->EnvironmentName);

            ManageWeather(state);

            ManageExteriorEnergyUse(state);

            ManageHeatBalance(state);

            //         do an end of day, end of environment time step

            state.dataGlobal->HourOfDay = 24;
            state.dataGlobal->TimeStep = state.dataGlobal->NumOfTimeStepInHour;
            state.dataGlobal->EndEnvrnFlag = true;

            if (state.dataSysVars->DeveloperFlag)
                DisplayString(state, "Initializing Simulation - hour 24 timestep 1:" + state.dataEnvrn->EnvironmentName);
            ManageWeather(state);

            ManageExteriorEnergyUse(state);

            ManageHeatBalance(state);

        } // ... End environment loop.

        if (state.dataGlobal->AnySlabsInModel || state.dataGlobal->AnyBasementsInModel) {
            SimulateGroundDomains(state, true);
        }

        if (!ErrorsFound) SimCostEstimate(state); // basically will get and check input
        if (ErrorsFound) ShowFatalError(state, "Previous conditions cause program termination.");
    }

    void ReportNodeConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine 'reports' the NodeConnection data structure.  It groups the
        // report/dump by parent, non-parent objects.

        // Using/Aliasing
        using namespace DataBranchNodeConnections;

        // Formats
        static constexpr std::string_view Format_702("! <#{0} Node Connections>,<Number of {0} Node Connections>\n");
        static constexpr std::string_view Format_703(
            "! <{} Node Connection>,<Node Name>,<Node ObjectType>,<Node ObjectName>,<Node ConnectionType>,<Node FluidStream>\n");

        state.dataBranchNodeConnections->NonConnectedNodes.dimension(state.dataLoopNodes->NumOfNodes, true);

        int NumNonParents = 0;
        for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop) {
            if (state.dataBranchNodeConnections->NodeConnections(Loop).ObjectIsParent) continue;
            ++NumNonParents;
        }
        const auto NumParents = state.dataBranchNodeConnections->NumOfNodeConnections - NumNonParents;
        state.dataBranchNodeConnections->ParentNodeList.allocate(NumParents);

        //  Do Parent Objects
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, Format_702, "Parent");
        print(state.files.bnd, " #Parent Node Connections,{}\n", NumParents);
        print(state.files.bnd, Format_703, "Parent");

        for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop) {
            if (!state.dataBranchNodeConnections->NodeConnections(Loop).ObjectIsParent) continue;
            state.dataBranchNodeConnections->NonConnectedNodes(state.dataBranchNodeConnections->NodeConnections(Loop).NodeNumber) = false;
            print(state.files.bnd,
                  " Parent Node Connection,{},{},{},{},{}\n",
                  state.dataBranchNodeConnections->NodeConnections(Loop).NodeName,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ObjectType,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ObjectName,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType,
                  state.dataBranchNodeConnections->NodeConnections(Loop).FluidStream);
            // Build ParentNodeLists
            if (UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType, "Inlet") ||
                UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType, "Outlet")) {
                bool ParentComponentFound = false;
                for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfActualParents; ++Loop1) {
                    if (state.dataBranchNodeConnections->ParentNodeList(Loop1).CType !=
                            state.dataBranchNodeConnections->NodeConnections(Loop).ObjectType ||
                        state.dataBranchNodeConnections->ParentNodeList(Loop1).CName !=
                            state.dataBranchNodeConnections->NodeConnections(Loop).ObjectName)
                        continue;
                    ParentComponentFound = true;
                    {
                        auto const SELECT_CASE_var(
                            UtilityRoutines::MakeUPPERCase(state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType));
                        if (SELECT_CASE_var == "INLET") {
                            state.dataBranchNodeConnections->ParentNodeList(Loop1).InletNodeName =
                                state.dataBranchNodeConnections->NodeConnections(Loop).NodeName;
                        } else if (SELECT_CASE_var == "OUTLET") {
                            state.dataBranchNodeConnections->ParentNodeList(Loop1).OutletNodeName =
                                state.dataBranchNodeConnections->NodeConnections(Loop).NodeName;
                        }
                    }
                }
                if (!ParentComponentFound) {
                    ++state.dataBranchNodeConnections->NumOfActualParents;
                    state.dataBranchNodeConnections->ParentNodeList(state.dataBranchNodeConnections->NumOfActualParents).CType =
                        state.dataBranchNodeConnections->NodeConnections(Loop).ObjectType;
                    state.dataBranchNodeConnections->ParentNodeList(state.dataBranchNodeConnections->NumOfActualParents).CName =
                        state.dataBranchNodeConnections->NodeConnections(Loop).ObjectName;
                    {
                        auto const SELECT_CASE_var(
                            UtilityRoutines::MakeUPPERCase(state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType));
                        if (SELECT_CASE_var == "INLET") {
                            state.dataBranchNodeConnections->ParentNodeList(state.dataBranchNodeConnections->NumOfActualParents).InletNodeName =
                                state.dataBranchNodeConnections->NodeConnections(Loop).NodeName;
                        } else if (SELECT_CASE_var == "OUTLET") {
                            state.dataBranchNodeConnections->ParentNodeList(state.dataBranchNodeConnections->NumOfActualParents).OutletNodeName =
                                state.dataBranchNodeConnections->NodeConnections(Loop).NodeName;
                        }
                    }
                }
            }
        }

        //  Do non-Parent Objects
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, Format_702, "Non-Parent");
        print(state.files.bnd, " #Non-Parent Node Connections,{}\n", NumNonParents);
        print(state.files.bnd, Format_703, "Non-Parent");

        for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop) {
            if (state.dataBranchNodeConnections->NodeConnections(Loop).ObjectIsParent) continue;
            state.dataBranchNodeConnections->NonConnectedNodes(state.dataBranchNodeConnections->NodeConnections(Loop).NodeNumber) = false;
            print(state.files.bnd,
                  " Non-Parent Node Connection,{},{},{},{},{}\n",
                  state.dataBranchNodeConnections->NodeConnections(Loop).NodeName,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ObjectType,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ObjectName,
                  state.dataBranchNodeConnections->NodeConnections(Loop).ConnectionType,
                  state.dataBranchNodeConnections->NodeConnections(Loop).FluidStream);
        }

        int NumNonConnected = 0;
        for (int Loop = 1; Loop <= state.dataLoopNodes->NumOfNodes; ++Loop) {
            if (state.dataBranchNodeConnections->NonConnectedNodes(Loop)) ++NumNonConnected;
        }

        if (NumNonConnected > 0) {
            print(state.files.bnd, "{}\n", "! ===============================================================");
            static constexpr std::string_view Format_705("! <#NonConnected Nodes>,<Number of NonConnected Nodes>\n #NonConnected Nodes,{}\n");
            print(state.files.bnd, Format_705, NumNonConnected);
            static constexpr std::string_view Format_706("! <NonConnected Node>,<NonConnected Node Number>,<NonConnected Node Name>");
            print(state.files.bnd, "{}\n", Format_706);
            for (int Loop = 1; Loop <= state.dataLoopNodes->NumOfNodes; ++Loop) {
                if (!state.dataBranchNodeConnections->NonConnectedNodes(Loop)) continue;
                print(state.files.bnd, " NonConnected Node,{},{}\n", Loop, state.dataLoopNodes->NodeID(Loop));
            }
        }

        state.dataBranchNodeConnections->NonConnectedNodes.deallocate();
    }

    void ReportLoopConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2001
        //       MODIFIED       March 2003; added other reporting
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the node connections in various parts of the
        // HVAC systen: Component Sets, Air Loop, Plant and Condenser Loop, Supply and
        // return air paths, controlled zones.
        // This information should be useful in diagnosing node connection input errors.

        // Using/Aliasing
        using namespace DataAirLoop;
        using namespace DataBranchNodeConnections;
        using namespace DataHVACGlobals;
        using namespace DataPlant;
        using namespace DataZoneEquipment;
        using DualDuct::ReportDualDuctConnections;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr static auto errstring("**error**");

        // Formats
        static constexpr std::string_view Format_700("! <#Component Sets>,<Number of Component Sets>");
        static constexpr std::string_view Format_702("! <Component Set>,<Component Set Count>,<Parent Object Type>,<Parent Object Name>,<Component "
                                                     "Type>,<Component Name>,<Inlet Node ID>,<Outlet Node ID>,<Description>");
        static constexpr std::string_view Format_720("! <#Zone Equipment Lists>,<Number of Zone Equipment Lists>");
        static constexpr std::string_view Format_722(
            "! <Zone Equipment List>,<Zone Equipment List Count>,<Zone Equipment List Name>,<Zone Name>,<Number of Components>");
        static constexpr std::string_view Format_723(
            "! <Zone Equipment Component>,<Component Count>,<Component Type>,<Component Name>,<Zone Name>,<Heating "
            "Priority>,<Cooling Priority>");

        // Report outside air node names on the Branch-Node Details file
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, "{}\n", "! #Outdoor Air Nodes,<Number of Outdoor Air Nodes>");
        print(state.files.bnd, " #Outdoor Air Nodes,{}\n", state.dataOutAirNodeMgr->NumOutsideAirNodes);
        if (state.dataOutAirNodeMgr->NumOutsideAirNodes > 0) {
            print(state.files.bnd, "{}\n", "! <Outdoor Air Node>,<NodeNumber>,<Node Name>");
        }
        for (int Count = 1; Count <= state.dataOutAirNodeMgr->NumOutsideAirNodes; ++Count) {
            print(state.files.bnd,
                  " Outdoor Air Node,{},{}\n",
                  state.dataOutAirNodeMgr->OutsideAirNodeList(Count),
                  state.dataLoopNodes->NodeID(state.dataOutAirNodeMgr->OutsideAirNodeList(Count)));
        }
        // Component Sets
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, "{}\n", Format_700);
        print(state.files.bnd, " #Component Sets,{}\n", state.dataBranchNodeConnections->NumCompSets);
        print(state.files.bnd, "{}\n", Format_702);

        for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
            print(state.files.bnd,
                  " Component Set,{},{},{},{},{},{},{},{}\n",
                  Count,
                  state.dataBranchNodeConnections->CompSets(Count).ParentCType,
                  state.dataBranchNodeConnections->CompSets(Count).ParentCName,
                  state.dataBranchNodeConnections->CompSets(Count).CType,
                  state.dataBranchNodeConnections->CompSets(Count).CName,
                  state.dataBranchNodeConnections->CompSets(Count).InletNodeName,
                  state.dataBranchNodeConnections->CompSets(Count).OutletNodeName,
                  state.dataBranchNodeConnections->CompSets(Count).Description);

            if (state.dataBranchNodeConnections->CompSets(Count).ParentCType == "UNDEFINED" ||
                state.dataBranchNodeConnections->CompSets(Count).InletNodeName == "UNDEFINED" ||
                state.dataBranchNodeConnections->CompSets(Count).OutletNodeName == "UNDEFINED") {
                if (state.dataErrTracking->AbortProcessing && state.dataSimulationManager->WarningOut) {
                    ShowWarningError(state,
                                     "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    state.dataSimulationManager->WarningOut = false;
                }
                ShowWarningError(state,
                                 "Node Connection Error for object " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                     ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                ShowContinueError(state, "  " + state.dataBranchNodeConnections->CompSets(Count).Description + " not on any Branch or Parent Object");
                ShowContinueError(state, "  Inlet Node : " + state.dataBranchNodeConnections->CompSets(Count).InletNodeName);
                ShowContinueError(state, "  Outlet Node: " + state.dataBranchNodeConnections->CompSets(Count).OutletNodeName);
                ++state.dataBranchNodeConnections->NumNodeConnectionErrors;
                if (UtilityRoutines::SameString(state.dataBranchNodeConnections->CompSets(Count).CType, "SolarCollector:UnglazedTranspired")) {
                    ShowContinueError(state, "This report does not necessarily indicate a problem for a MultiSystem Transpired Collector");
                }
            }
            if (state.dataBranchNodeConnections->CompSets(Count).Description == "UNDEFINED") {
                if (state.dataErrTracking->AbortProcessing && state.dataSimulationManager->WarningOut) {
                    ShowWarningError(state,
                                     "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    state.dataSimulationManager->WarningOut = false;
                }
                ShowWarningError(state,
                                 "Potential Node Connection Error for object " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                     ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                ShowContinueError(state, "  Node Types are still UNDEFINED -- See Branch/Node Details file for further information");
                ShowContinueError(state, "  Inlet Node : " + state.dataBranchNodeConnections->CompSets(Count).InletNodeName);
                ShowContinueError(state, "  Outlet Node: " + state.dataBranchNodeConnections->CompSets(Count).OutletNodeName);
                ++state.dataBranchNodeConnections->NumNodeConnectionErrors;
            }
        }

        for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
            for (int Count1 = Count + 1; Count1 <= state.dataBranchNodeConnections->NumCompSets; ++Count1) {
                if (state.dataBranchNodeConnections->CompSets(Count).CType != state.dataBranchNodeConnections->CompSets(Count1).CType) continue;
                if (state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Count1).CName) continue;
                if (state.dataBranchNodeConnections->CompSets(Count).InletNodeName != state.dataBranchNodeConnections->CompSets(Count1).InletNodeName)
                    continue;
                if (state.dataBranchNodeConnections->CompSets(Count).OutletNodeName !=
                    state.dataBranchNodeConnections->CompSets(Count1).OutletNodeName)
                    continue;
                if (state.dataErrTracking->AbortProcessing && state.dataSimulationManager->WarningOut) {
                    ShowWarningError(state,
                                     "Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    state.dataSimulationManager->WarningOut = false;
                }
                ShowWarningError(state, "Component plus inlet/outlet node pair used more than once:");
                ShowContinueError(state,
                                  "  Component  : " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                ShowContinueError(state, "  Inlet Node : " + state.dataBranchNodeConnections->CompSets(Count).InletNodeName);
                ShowContinueError(state, "  Outlet Node: " + state.dataBranchNodeConnections->CompSets(Count).OutletNodeName);
                ShowContinueError(state,
                                  "  Used by    : " + state.dataBranchNodeConnections->CompSets(Count).ParentCType + ' ' +
                                      state.dataBranchNodeConnections->CompSets(Count).ParentCName);
                ShowContinueError(state,
                                  "  and  by    : " + state.dataBranchNodeConnections->CompSets(Count1).ParentCType + ' ' +
                                      state.dataBranchNodeConnections->CompSets(Count1).ParentCName);
                ++state.dataBranchNodeConnections->NumNodeConnectionErrors;
            }
        }
        //  Plant Loops
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, "{}\n", "! <# Plant Loops>,<Number of Plant Loops>");
        print(state.files.bnd, " #Plant Loops,{}\n", state.dataHVACGlobal->NumPlantLoops);
        print(state.files.bnd,
              "{}\n",
              "! <Plant Loop>,<Plant Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>");
        print(
            state.files.bnd, "{}\n", "! <Plant Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>");
        print(state.files.bnd,
              "{}\n",
              "! <Plant Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Branch>,<Outlet Branch>,<Loop "
              "Name>,<Loop Type>");
        print(state.files.bnd,
              "{}\n",
              "! <Plant Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop "
              "Type>");
        print(state.files.bnd,
              "{}\n",
              "! <Plant Loop Supply Connection>,<Plant Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>");
        print(state.files.bnd,
              "{}\n",
              "! <Plant Loop Return Connection>,<Plant Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>");
        for (int Count = 1; Count <= state.dataHVACGlobal->NumPlantLoops; ++Count) {
            for (int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                //  Plant Supply Side Loop
                // Demandside and supplyside is parametrized in DataPlant
                const auto LoopString = [&]() {
                    if (LoopSideNum == DemandSide) {
                        return "Demand";
                    } else if (LoopSideNum == SupplySide) {
                        return "Supply";
                    } else {
                        return "";
                    }
                }();

                print(state.files.bnd,
                      " Plant Loop,{},{},{},{},{},{}\n",
                      state.dataPlnt->PlantLoop(Count).Name,
                      LoopString,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).NodeNameIn,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).NodeNameOut,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).BranchList,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).ConnectList);
                //  Plant Supply Side Splitter
                if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Exists) {
                    print(state.files.bnd,
                          "   Plant Loop Connector,Splitter,{},{},{},{}\n",
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name,
                          state.dataPlnt->PlantLoop(Count).Name,
                          LoopString,
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes);
                    for (int Count1 = 1; Count1 <= state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes; ++Count1) {
                        print(state.files.bnd,
                              "     Plant Loop Connector Branches,{},Splitter,{},",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name);

                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn <= 0) {
                            print(state.files.bnd, "{},\n", errstring);
                        } else {
                            print(state.files.bnd,
                                  "{},",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn)
                                      .Name);
                        }

                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1) <= 0) {
                            print(state.files.bnd, "{},{},{}\n", errstring, state.dataPlnt->PlantLoop(Count).Name, LoopString);
                        } else {
                            print(state.files.bnd,
                                  "{},{},{}\n",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1))
                                      .Name,
                                  state.dataPlnt->PlantLoop(Count).Name,
                                  LoopString);
                        }

                        print(state.files.bnd,
                              "     Plant Loop Connector Nodes,   {},Splitter,{},{},{},{},{}\n",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameIn,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameOut(Count1),
                              state.dataPlnt->PlantLoop(Count).Name,
                              LoopString);
                    }
                }

                //  Plant Supply Side Mixer
                if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Exists) {
                    print(state.files.bnd,
                          "   Plant Loop Connector,Mixer,{},{},{},{}\n",
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name,
                          state.dataPlnt->PlantLoop(Count).Name,
                          LoopString,
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes); //',Supply,'//  &

                    for (int Count1 = 1; Count1 <= state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes; ++Count1) {
                        print(state.files.bnd,
                              "     Plant Loop Connector Branches,{},Mixer,{},",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name);
                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1) <= 0) {
                            print(state.files.bnd, "{},", errstring);
                        } else {
                            print(state.files.bnd,
                                  "{},",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1))
                                      .Name);
                        }
                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut <= 0) {
                            print(state.files.bnd, "{},{},Supply\n", errstring, state.dataPlnt->PlantLoop(Count).Name);
                        } else {
                            print(state.files.bnd,
                                  "{},{},{}\n",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut)
                                      .Name,
                                  state.dataPlnt->PlantLoop(Count).Name,
                                  LoopString);
                        }
                        print(state.files.bnd,
                              "     Plant Loop Connector Nodes,   {},Mixer,{},{},{},{},{}\n",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameIn(Count1),
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameOut,
                              state.dataPlnt->PlantLoop(Count).Name,
                              LoopString);
                    }
                }
            }
            print(state.files.bnd,
                  " Plant Loop Supply Connection,{},{},{}\n",
                  state.dataPlnt->PlantLoop(Count).Name,
                  state.dataPlnt->PlantLoop(Count).LoopSide(SupplySide).NodeNameOut,
                  state.dataPlnt->PlantLoop(Count).LoopSide(DemandSide).NodeNameIn);
            print(state.files.bnd,
                  " Plant Loop Return Connection,{},{},{}\n",
                  state.dataPlnt->PlantLoop(Count).Name,
                  state.dataPlnt->PlantLoop(Count).LoopSide(DemandSide).NodeNameOut,
                  state.dataPlnt->PlantLoop(Count).LoopSide(SupplySide).NodeNameIn);

        } //  Plant Demand Side Loop

        //  Condenser Loops
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, "{}\n", "! <# Condenser Loops>,<Number of Condenser Loops>");
        print(state.files.bnd, " #Condenser Loops,{}\n", state.dataHVACGlobal->NumCondLoops);
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop>,<Condenser Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>");
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>");
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Branch>,<Outlet Branch>,<Loop "
              "Name>,<Loop Type>");
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Node>,<Outlet Node>,<Loop "
              "Name>,<Loop Type>");
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop Supply Connection>,<Condenser Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>");
        print(state.files.bnd,
              "{}\n",
              "! <Condenser Loop Return Connection>,<Condenser Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>");

        for (int Count = state.dataHVACGlobal->NumPlantLoops + 1; Count <= state.dataPlnt->TotNumLoops; ++Count) {
            for (int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                //  Plant Supply Side Loop
                // Demandside and supplyside is parametrized in DataPlant
                const auto LoopString = [&]() {
                    if (LoopSideNum == DemandSide) {
                        return "Demand";
                    } else if (LoopSideNum == SupplySide) {
                        return "Supply";
                    } else {
                        return "";
                    }
                }();

                print(state.files.bnd,
                      " Plant Loop,{},{},{},{},{},{}\n",
                      state.dataPlnt->PlantLoop(Count).Name,
                      LoopString,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).NodeNameIn,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).NodeNameOut,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).BranchList,
                      state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).ConnectList);
                //  Plant Supply Side Splitter
                if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Exists) {
                    print(state.files.bnd,
                          "   Plant Loop Connector,Splitter,{},{},{},{}\n",
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name,
                          state.dataPlnt->PlantLoop(Count).Name,
                          LoopString,
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes);
                    for (int Count1 = 1; Count1 <= state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes; ++Count1) {
                        print(state.files.bnd,
                              "     Plant Loop Connector Branches,{},Splitter,{},",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name);

                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn <= 0) {
                            print(state.files.bnd, "{},", errstring);
                        } else {
                            print(state.files.bnd,
                                  "{},",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn)
                                      .Name);
                        }
                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1) <= 0) {
                            print(state.files.bnd, "{},{},{}\n", errstring, state.dataPlnt->PlantLoop(Count).Name, LoopString);
                        } else {

                            print(state.files.bnd,
                                  "{},{},{}\n",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1))
                                      .Name,
                                  state.dataPlnt->PlantLoop(Count).Name,
                                  LoopString);
                        }

                        print(state.files.bnd,
                              "     Plant Loop Connector Nodes,   {},Splitter,{},{},{},{},{}\n",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameIn,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameOut(Count1),
                              state.dataPlnt->PlantLoop(Count).Name,
                              LoopString);
                    }
                }

                //  Plant Supply Side Mixer
                if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Exists) {
                    const auto totalInletNodes = state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes;
                    print(state.files.bnd,
                          "   Plant Loop Connector,Mixer,{},{},{},{}\n",
                          state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name,
                          state.dataPlnt->PlantLoop(Count).Name,
                          LoopString,
                          totalInletNodes); //',Supply,'//  &

                    for (int Count1 = 1; Count1 <= totalInletNodes; ++Count1) {
                        print(state.files.bnd,
                              "     Plant Loop Connector Branches,{},Mixer,{},",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name);

                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1) <= 0) {
                            print(state.files.bnd, "{},", errstring);
                        } else {
                            print(state.files.bnd,
                                  "{},",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1))
                                      .Name);
                        }
                        if (state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut <= 0) {
                            print(state.files.bnd, "{},{},{}\n", errstring, state.dataPlnt->PlantLoop(Count).Name, LoopString);
                        } else {
                            print(state.files.bnd,
                                  "{},{},{}\n",
                                  state.dataPlnt->PlantLoop(Count)
                                      .LoopSide(LoopSideNum)
                                      .Branch(state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut)
                                      .Name,
                                  state.dataPlnt->PlantLoop(Count).Name,
                                  LoopString);
                        }
                        print(state.files.bnd,
                              "     Plant Loop Connector Nodes,   {},Mixer,{},{},{},{},{}\n",
                              Count1,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name,
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameIn(Count1),
                              state.dataPlnt->PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameOut,
                              state.dataPlnt->PlantLoop(Count).Name,
                              LoopString);
                    }
                }
            }
            print(state.files.bnd,
                  " Plant Loop Supply Connection,{},{},{}\n",
                  state.dataPlnt->PlantLoop(Count).Name,
                  state.dataPlnt->PlantLoop(Count).LoopSide(SupplySide).NodeNameOut,
                  state.dataPlnt->PlantLoop(Count).LoopSide(DemandSide).NodeNameIn);
            print(state.files.bnd,
                  " Plant Loop Return Connection,{},{},{}\n",
                  state.dataPlnt->PlantLoop(Count).Name,
                  state.dataPlnt->PlantLoop(Count).LoopSide(DemandSide).NodeNameOut,
                  state.dataPlnt->PlantLoop(Count).LoopSide(SupplySide).NodeNameIn);

        } //  Plant Demand Side Loop

        print(state.files.bnd, "{}\n", "! ===============================================================");
        int NumOfControlledZones = 0;
        for (int Count = 1; Count <= state.dataGlobal->NumOfZones; ++Count) {
            if (!allocated(state.dataZoneEquip->ZoneEquipConfig)) continue;
            if (state.dataZoneEquip->ZoneEquipConfig(Count).IsControlled) ++NumOfControlledZones;
        }

        if (NumOfControlledZones > 0) {
            print(state.files.bnd, "{}\n", "! <# Controlled Zones>,<Number of Controlled Zones>");
            print(state.files.bnd, " #Controlled Zones,{}\n", NumOfControlledZones);
            print(state.files.bnd,
                  "{}\n",
                  "! <Controlled Zone>,<Controlled Zone Name>,<Equip List Name>,<Control List Name>,<Zone Node Name>,<# Inlet Nodes>,<# Exhaust "
                  "Nodes>,<# Return Nodes>");
            print(state.files.bnd,
                  "{}\n",
                  "! <Controlled Zone Inlet>,<Inlet Node Count>,<Controlled Zone Name>,<Supply Air Inlet Node Name>,<SD Sys:Cooling/Heating "
                  "[DD:Cooling] Inlet Node Name>,<DD Sys:Heating Inlet Node Name>");
            print(state.files.bnd, "{}\n", "! <Controlled Zone Exhaust>,<Exhaust Node Count>,<Controlled Zone Name>,<Exhaust Air Node Name>");

            for (int Count = 1; Count <= state.dataGlobal->NumOfZones; ++Count) {
                if (!state.dataZoneEquip->ZoneEquipConfig(Count).IsControlled) continue;

                print(state.files.bnd,
                      " Controlled Zone,{},{},{},{},{},{},{}\n",
                      state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                      state.dataZoneEquip->ZoneEquipConfig(Count).EquipListName,
                      state.dataZoneEquip->ZoneEquipConfig(Count).ControlListName,
                      state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).ZoneNode),
                      state.dataZoneEquip->ZoneEquipConfig(Count).NumInletNodes,
                      state.dataZoneEquip->ZoneEquipConfig(Count).NumExhaustNodes,
                      state.dataZoneEquip->ZoneEquipConfig(Count).NumReturnNodes);
                for (int Count1 = 1; Count1 <= state.dataZoneEquip->ZoneEquipConfig(Count).NumInletNodes; ++Count1) {
                    auto ChrName = state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).AirDistUnitHeat(Count1).InNode);
                    if (ChrName == "Undefined") ChrName = "N/A";
                    print(state.files.bnd,
                          "   Controlled Zone Inlet,{},{},{},{},{}\n",
                          Count1,
                          state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).InletNode(Count1)),
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).AirDistUnitCool(Count1).InNode),
                          ChrName);
                }
                for (int Count1 = 1; Count1 <= state.dataZoneEquip->ZoneEquipConfig(Count).NumExhaustNodes; ++Count1) {
                    print(state.files.bnd,
                          "   Controlled Zone Exhaust,{},{},{}\n",
                          Count1,
                          state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).ExhaustNode(Count1)));
                }
                for (int Count1 = 1; Count1 <= state.dataZoneEquip->ZoneEquipConfig(Count).NumReturnNodes; ++Count1) {
                    print(state.files.bnd,
                          "   Controlled Zone Return,{},{},{}\n",
                          Count1,
                          state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(Count).ReturnNode(Count1)));
                }
            }

            // Report Zone Equipment Lists to BND File
            print(state.files.bnd, "{}\n", "! ===============================================================");
            print(state.files.bnd, "{}\n", Format_720);
            print(state.files.bnd, " #Zone Equipment Lists,{}\n", NumOfControlledZones);
            print(state.files.bnd, "{}\n", Format_722);
            print(state.files.bnd, "{}\n", Format_723);

            for (int Count = 1; Count <= state.dataGlobal->NumOfZones; ++Count) {
                // Zone equipment list array parallels controlled zone equipment array, so
                // same index finds corresponding data from both arrays
                if (!state.dataZoneEquip->ZoneEquipConfig(Count).IsControlled) continue;

                print(state.files.bnd,
                      " Zone Equipment List,{},{},{},{}\n",
                      Count,
                      state.dataZoneEquip->ZoneEquipList(Count).Name,
                      state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                      state.dataZoneEquip->ZoneEquipList(Count).NumOfEquipTypes);

                for (int Count1 = 1; Count1 <= state.dataZoneEquip->ZoneEquipList(Count).NumOfEquipTypes; ++Count1) {
                    print(state.files.bnd,
                          "   Zone Equipment Component,{},{},{},{},{},{}\n",
                          Count1,
                          state.dataZoneEquip->ZoneEquipList(Count).EquipType(Count1),
                          state.dataZoneEquip->ZoneEquipList(Count).EquipName(Count1),
                          state.dataZoneEquip->ZoneEquipConfig(Count).ZoneName,
                          state.dataZoneEquip->ZoneEquipList(Count).CoolingPriority(Count1),
                          state.dataZoneEquip->ZoneEquipList(Count).HeatingPriority(Count1));
                }
            }
        }

        // Report Dual Duct Dampers to BND File
        ReportDualDuctConnections(state);

        if (state.dataBranchNodeConnections->NumNodeConnectionErrors == 0) {
            ShowMessage(state, "No node connection errors were found.");
        } else {
            if (state.dataBranchNodeConnections->NumNodeConnectionErrors > 1) {
                ShowMessage(state, format("There were {} node connection errors noted.", state.dataBranchNodeConnections->NumNodeConnectionErrors));
            } else {
                ShowMessage(state, format("There was {} node connection error noted.", state.dataBranchNodeConnections->NumNodeConnectionErrors));
            }
        }

        state.dataErrTracking->AskForConnectionsReport = false;
    }

    void ReportParentChildren(EnergyPlusData &state)
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
        print(state.files.debug, "{}\n", "Node Type,CompSet Name,Inlet Node,OutletNode");
        for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfActualParents; ++Loop) {
            NumChildren = GetNumChildren(
                state, state.dataBranchNodeConnections->ParentNodeList(Loop).CType, state.dataBranchNodeConnections->ParentNodeList(Loop).CName);
            if (NumChildren > 0) {
                ChildCType.allocate(NumChildren);
                ChildCName.allocate(NumChildren);
                ChildInNodeName.allocate(NumChildren);
                ChildOutNodeName.allocate(NumChildren);
                ChildInNodeNum.allocate(NumChildren);
                ChildOutNodeNum.allocate(NumChildren);
                ChildCType = std::string{};
                ChildCName = std::string{};
                ChildInNodeName = std::string{};
                ChildOutNodeName = std::string{};
                ChildInNodeNum = 0;
                ChildOutNodeNum = 0;
                GetChildrenData(state,
                                state.dataBranchNodeConnections->ParentNodeList(Loop).CType,
                                state.dataBranchNodeConnections->ParentNodeList(Loop).CName,
                                NumChildren,
                                ChildCType,
                                ChildCName,
                                ChildInNodeName,
                                ChildInNodeNum,
                                ChildOutNodeName,
                                ChildOutNodeNum,
                                ErrorsFound);
                if (Loop > 1) print(state.files.debug, "{}\n", std::string(60, '='));

                print(state.files.debug,
                      " Parent Node,{}:{},{},{}\n",
                      state.dataBranchNodeConnections->ParentNodeList(Loop).CType,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).CName,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).InletNodeName,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).OutletNodeName);
                for (Loop1 = 1; Loop1 <= NumChildren; ++Loop1) {
                    print(state.files.debug,
                          "..ChildNode,{}:{},{},{}\n",
                          ChildCType(Loop1),
                          ChildCName(Loop1),
                          ChildInNodeName(Loop1),
                          ChildOutNodeName(Loop1));
                }
                ChildCType.deallocate();
                ChildCName.deallocate();
                ChildInNodeName.deallocate();
                ChildOutNodeName.deallocate();
                ChildInNodeNum.deallocate();
                ChildOutNodeNum.deallocate();
            } else {
                if (Loop > 1) print(state.files.debug, "{}\n", std::string(60, '='));
                print(state.files.debug,
                      " Parent Node (no children),{}:{},{},{}\n",
                      state.dataBranchNodeConnections->ParentNodeList(Loop).CType,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).CName,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).InletNodeName,
                      state.dataBranchNodeConnections->ParentNodeList(Loop).OutletNodeName);
            }
        }
    }

    void ReportCompSetMeterVariables(EnergyPlusData &state)
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
        Array1D<OutputProcessor::TimeStepType> IndexTypes;
        Array1D<OutputProcessor::VariableType> VarTypes;
        Array1D<OutputProcessor::Unit> unitsForVar; // units from enum for each variable
        Array1D_string VarNames;
        std::map<int, DataGlobalConstants::ResourceType> ResourceTypes;
        Array1D_string EndUses;
        Array1D_string Groups;

        print(state.files.debug, "{}\n", " CompSet,ComponentType,ComponentName,NumMeteredVariables");
        print(state.files.debug, "{}\n", " RepVar,ReportIndex,ReportID,ReportName,Units,ResourceType,EndUse,Group,IndexType");

        for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
            NumVariables = GetNumMeteredVariables(
                state, state.dataBranchNodeConnections->CompSets(Loop).CType, state.dataBranchNodeConnections->CompSets(Loop).CName);
            print(state.files.debug,
                  "CompSet, {}, {}, {:5}\n",
                  state.dataBranchNodeConnections->CompSets(Loop).CType,
                  state.dataBranchNodeConnections->CompSets(Loop).CName,
                  NumVariables);
            if (NumVariables <= 0) continue;
            VarIndexes.dimension(NumVariables, 0);
            VarIDs.dimension(NumVariables, 0);
            IndexTypes.dimension(NumVariables, 0);
            VarTypes.dimension(NumVariables, 0);
            VarNames.allocate(NumVariables);
            unitsForVar.allocate(NumVariables);

            for (int idx = 1; idx <= NumVariables; ++idx) {
                ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
            }

            EndUses.allocate(NumVariables);
            Groups.allocate(NumVariables);
            GetMeteredVariables(state,
                                state.dataBranchNodeConnections->CompSets(Loop).CType,
                                state.dataBranchNodeConnections->CompSets(Loop).CName,
                                VarIndexes,
                                VarTypes,
                                IndexTypes,
                                unitsForVar,
                                ResourceTypes,
                                EndUses,
                                Groups,
                                VarNames,
                                VarIDs);
            for (Loop1 = 1; Loop1 <= NumVariables; ++Loop1) {
                print(state.files.debug,
                      "RepVar,{:5},{:5},{},[{}],{},{},{},{:5}\n",
                      VarIndexes(Loop1),
                      VarIDs(Loop1),
                      VarNames(Loop1),
                      unitEnumToString(unitsForVar(Loop1)),
                      GetResourceTypeChar(ResourceTypes.at(Loop1)),
                      EndUses(Loop1),
                      Groups(Loop1)
                      // TODO: Should call OutputProcessor::StandardTimeStepTypeKey(IndexTypes(Loop1)) to return "Zone" or "HVAC"
                      ,
                      static_cast<int>(IndexTypes(Loop1)));
            }
            VarIndexes.deallocate();
            IndexTypes.deallocate();
            VarTypes.deallocate();
            VarIDs.deallocate();
            VarNames.deallocate();
            unitsForVar.deallocate();
            EndUses.deallocate();
            Groups.deallocate();
        }
    }

    void PostIPProcessing(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2010

        // PURPOSE OF THIS SUBROUTINE:
        // This provides post processing (for errors, etc) directly after the InputProcessor
        // finishes.  Code originally in the Input Processor.

        // Using/Aliasing
        // using SQLiteProcedures::CreateSQLiteDatabase;
        using FluidProperties::FindGlycol;

        state.dataGlobal->DoingInputProcessing = false;

        state.dataInputProcessing->inputProcessor->preProcessorCheck(
            state, state.dataSimulationManager->PreP_Fatal); // Check Preprocessor objects for warning, severe, etc errors.

        if (state.dataSimulationManager->PreP_Fatal) {
            ShowFatalError(state, "Preprocessor condition(s) cause termination.");
        }

        state.dataInputProcessing->inputProcessor->preScanReportingVariables(state);
    }

} // namespace SimulationManager

// EXTERNAL SUBROUTINES:

void Resimulate(EnergyPlusData &state,
                bool &ResimExt, // Flag to resimulate the exterior energy use simulation
                bool &ResimHB,  // Flag to resimulate the heat balance simulation (including HVAC)
                bool &ResimHVAC // Flag to resimulate the HVAC simulation
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
    using ExteriorEnergyUse::ManageExteriorEnergyUse;
    using HeatBalanceAirManager::InitAirHeatBalance;
    using HeatBalanceSurfaceManager::InitSurfaceHeatBalance;
    using HVACManager::SimHVAC;
    using RefrigeratedCase::ManageRefrigeratedCaseRacks;
    using ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates;
    using ZoneTempPredictorCorrector::ManageZoneAirUpdates;
    using namespace ZoneEquipmentManager;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneTempChange(0.0); // Dummy variable needed for calling ManageZoneAirUpdates

    if (ResimExt) {
        ManageExteriorEnergyUse(state);

        ++state.dataDemandManager->DemandManagerExtIterations;
    }

    if (ResimHB) {
        // Surface simulation
        InitSurfaceHeatBalance(state);
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state);

        // Air simulation
        InitAirHeatBalance(state);
        ManageRefrigeratedCaseRacks(state);

        ++state.dataDemandManager->DemandManagerHBIterations;
        ResimHVAC = true; // Make sure HVAC is resimulated too
    }

    if (ResimHVAC) {
        // HVAC simulation
        ManageZoneAirUpdates(state,
                             DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints,
                             ZoneTempChange,
                             false,
                             state.dataHVACGlobal->UseZoneTimeStepHistory,
                             0.0);
        if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
            ManageZoneContaminanUpdates(
                state, DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints, false, state.dataHVACGlobal->UseZoneTimeStepHistory, 0.0);
        CalcAirFlowSimple(
            state, 0, state.dataHeatBal->ZoneAirMassFlow.AdjustZoneMixingFlow, state.dataHeatBal->ZoneAirMassFlow.AdjustZoneInfiltrationFlow);
        ManageZoneAirUpdates(
            state, DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep, ZoneTempChange, false, state.dataHVACGlobal->UseZoneTimeStepHistory, 0.0);
        if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
            ManageZoneContaminanUpdates(
                state, DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep, false, state.dataHVACGlobal->UseZoneTimeStepHistory, 0.0);
        SimHVAC(state);

        ++state.dataDemandManager->DemandManagerHVACIterations;
    }
}

} // namespace EnergyPlus
