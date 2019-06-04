// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CommandLineInterface.hh>
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
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
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
#include <EMSManager.hh>
#include <EconomicLifeCycleCost.hh>
#include <EconomicTariff.hh>
#include <ElectricPowerServiceManager.hh>
#include <ExteriorEnergyUse.hh>
#include <ExternalInterface.hh>
#include <FaultsManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACControllers.hh>
#include <HVACManager.hh>
#include <HVACSizingSimulationManager.hh>
#include <HeatBalanceAirManager.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessing/InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <OutputReports.hh>
#include <Plant/PlantManager.hh>
#include <PlantPipingSystemsManager.hh>
#include <PollutionModule.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ResultsSchema.hh>
#include <ReportCoilSelection.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SimulationManager.hh>
#include <SizingManager.hh>
#include <SolarShading.hh>
#include <SurfaceGeometry.hh>
#include <SystemReports.hh>
#include <Timer.h>
#include <UtilityRoutines.hh>
#include <Vectors.hh>
#include <WeatherManager.hh>
#include <ZoneContaminantPredictorCorrector.hh>
#include <ZoneEquipmentManager.hh>
#include <ZoneTempPredictorCorrector.hh>

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
    static ObjexxFCL::gio::Fmt fmtLD("*");
    static ObjexxFCL::gio::Fmt fmtA("(A)");

    // DERIVED TYPE DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    bool RunPeriodsInInput(false);
    bool RunControlInInput(false);

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of SimulationManager should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool PreP_Fatal(false);
    } // namespace

    // SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

    // MODULE SUBROUTINES:

    // Functions
    void clear_state()
    {
        RunPeriodsInInput = false;
        RunControlInInput = false;
        PreP_Fatal = false;
    }

    void ManageSimulation()
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
        using DataEnvironment::CurMnDy;
        using DataEnvironment::CurrentOverallSimDay;
        using DataEnvironment::EndMonthFlag;
        using DataEnvironment::EnvironmentName;
        using DataEnvironment::TotalOverallSimDays;
        using DataEnvironment::TotDesDays;
        using DataEnvironment::TotRunDesPersDays;
        using DataHVACGlobals::TimeStepSys;

        using BranchInputManager::InvalidBranchDefinitions;
        using BranchInputManager::ManageBranchInput;
        using BranchInputManager::TestBranchIntegrity;
        using BranchNodeConnections::CheckNodeConnections;
        using BranchNodeConnections::TestCompSetInletOutletNodes;
        using CostEstimateManager::SimCostEstimate;
        using CurveManager::InitCurveReporting;
        using DataErrorTracking::AskForConnectionsReport;
        using DataErrorTracking::ExitDuringSimulations;
        using DemandManager::InitDemandManagers;
        using EconomicLifeCycleCost::ComputeLifeCycleCostAndReport;
        using EconomicLifeCycleCost::GetInputForLifeCycleCost;
        using EconomicTariff::ComputeTariff; // added for computing annual utility costs
        using EconomicTariff::WriteTabularTariffReports;
        using EMSManager::CheckIfAnyEMS;
        using EMSManager::ManageEMS;
        using ExteriorEnergyUse::ManageExteriorEnergyUse;
        using General::TrimSigDigits;
        using HVACControllers::DumpAirLoopStatistics;
        using MixedAir::CheckControllerLists;
        using NodeInputManager::CheckMarkedNodes;
        using NodeInputManager::SetupNodeVarsForReporting;
        using OutputProcessor::ReportForTabularReports;
        using OutputProcessor::SetupTimePointers;
        using OutputReportPredefined::SetPredefinedTables;
        using OutputReportTabular::CloseOutputTabularFile;
        using OutputReportTabular::OpenOutputTabularFile;
        using OutputReportTabular::ResetTabularReports;
        using OutputReportTabular::WriteTabularReports;
        using PlantManager::CheckIfAnyPlant;
        using PollutionModule::CheckPollutionMeterReporting;
        using PollutionModule::SetupPollutionCalculations;
        using PollutionModule::SetupPollutionMeterReporting;
        using SizingManager::ManageSizing;
        using SystemReports::CreateEnergyReportStructure;
        using SystemReports::ReportAirLoopConnections;
        using namespace DataTimings;
        using DataSystemVariables::FullAnnualRun;
        using FaultsManager::CheckAndReadFaults;
        using OutputProcessor::isFinalYear;
        using OutputProcessor::ResetAccumulationWhenWarmupComplete;
        using PlantPipingSystemsManager::CheckIfAnyBasements;
        using PlantPipingSystemsManager::CheckIfAnySlabs;
        using PlantPipingSystemsManager::SimulateGroundDomains;
        using Psychrometrics::InitializePsychRoutines;
        using SetPointManager::CheckIfAnyIdealCondEntSetPoint;
        using WeatherManager::CheckIfAnyUnderwaterBoundaries;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool Available; // an environment is available to process
        static bool ErrorsFound(false);
        static bool TerminalError(false);
        bool SimsDone;
        bool ErrFound;
        bool oneTimeUnderwaterBoundaryCheck = true;
        bool AnyUnderwaterBoundaries = false;
        int EnvCount;

        // Formats
        static ObjexxFCL::gio::Fmt Format_700("('Environment:WarmupDays,',I3)");

        // CreateSQLiteDatabase();
        sqlite = EnergyPlus::CreateSQLiteDatabase();

        if (sqlite) {
            sqlite->sqliteBegin();
            sqlite->createSQLiteSimulationsRecord(1, DataStringGlobals::VerString, DataStringGlobals::CurrentDateTime);
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
        RunPeriodsInInput =
            (inputProcessor->getNumObjectsFound("RunPeriod") > 0 || inputProcessor->getNumObjectsFound("RunPeriod:CustomRange") > 0 || FullAnnualRun);
        AskForConnectionsReport = false; // set to false until sizing is finished

        OpenOutputFiles();
        GetProjectData();
        CheckForMisMatchedEnvironmentSpecifications();
        CheckForRequestedReporting();
        SetPredefinedTables();
        SetPreConstructionInputParameters(); // establish array bounds for constructions early

        SetupTimePointers("Zone", TimeStepZone); // Set up Time pointer for HB/Zone Simulation
        SetupTimePointers("HVAC", TimeStepSys);

        CheckIfAnyEMS();
        CheckIfAnyPlant();
        CheckIfAnySlabs();
        CheckIfAnyBasements();
        CheckIfAnyIdealCondEntSetPoint();
        createFacilityElectricPowerServiceObject();
        createCoilSelectionReportObj();

        ManageBranchInput(); // just gets input and returns.

        DoingSizing = true;
        ManageSizing();

        BeginFullSimFlag = true;
        SimsDone = false;
        if (DoDesDaySim || DoWeathSim || DoHVACSizingSimulation) {
            DoOutputReporting = true;
        }
        DoingSizing = false;

        if ((DoZoneSizing || DoSystemSizing || DoPlantSizing) && !(DoDesDaySim || (DoWeathSim && RunPeriodsInInput))) {
            ShowWarningError("ManageSimulation: Input file has requested Sizing Calculations but no Simulations are requested (in SimulationControl "
                             "object). Succeeding warnings/errors may be confusing.");
        }
        Available = true;

        if (InvalidBranchDefinitions) {
            ShowFatalError("Preceding error(s) in Branch Input cause termination.");
        }

        DisplayString("Adjusting Air System Sizing");
        SizingManager::ManageSystemSizingAdjustments();

        DisplayString("Adjusting Standard 62.1 Ventilation Sizing");
        SizingManager::ManageSystemVentilationAdjustments();

        DisplayString("Initializing Simulation");
        KickOffSimulation = true;

        ResetEnvironmentCounter();
        SetupSimulation(ErrorsFound);

        CheckAndReadFaults();

        InitCurveReporting();

        AskForConnectionsReport = true; // set to true now that input processing and sizing is done.
        KickOffSimulation = false;
        WarmupFlag = false;
        DoWeatherInitReporting = true;

        //  Note:  All the inputs have been 'gotten' by the time we get here.
        ErrFound = false;
        if (DoOutputReporting) {
            DisplayString("Reporting Surfaces");

            ReportSurfaces();

            SetupNodeVarsForReporting();
            MetersHaveBeenInitialized = true;
            SetupPollutionMeterReporting();
            UpdateMeterReporting();
            CheckPollutionMeterReporting();
            facilityElectricServiceObj->verifyCustomMetersElecPowerMgr();
            SetupPollutionCalculations();
            InitDemandManagers();

            TestBranchIntegrity(ErrFound);
            if (ErrFound) TerminalError = true;
            TestAirPathIntegrity(ErrFound);
            if (ErrFound) TerminalError = true;
            CheckMarkedNodes(ErrFound);
            if (ErrFound) TerminalError = true;
            CheckNodeConnections(ErrFound);
            if (ErrFound) TerminalError = true;
            TestCompSetInletOutletNodes(ErrFound);
            if (ErrFound) TerminalError = true;
            CheckControllerLists(ErrFound);
            if (ErrFound) TerminalError = true;

            if (DoDesDaySim || DoWeathSim) {
                ReportLoopConnections();
                ReportAirLoopConnections();
                ReportNodeConnections();
                // Debug reports
                //      CALL ReportCompSetMeterVariables
                //      CALL ReportParentChildren
            }

            CreateEnergyReportStructure();
            bool anyEMSRan;
            ManageEMS(emsCallFromSetupSimulation, anyEMSRan); // point to finish setup processing EMS, sensor ready now

            ProduceRDDMDD();

            if (TerminalError) {
                ShowFatalError("Previous Conditions cause program termination.");
            }
        }

        if (sqlite) {
            sqlite->sqliteBegin();
            sqlite->updateSQLiteSimulationRecord(1, DataGlobals::NumOfTimeStepInHour);
            sqlite->sqliteCommit();
        }

        GetInputForLifeCycleCost(); // must be prior to WriteTabularReports -- do here before big simulation stuff.

        // check for variable latitude/location/etc
        WeatherManager::ReadVariableLocationOrientation();

        // if user requested HVAC Sizing Simulation, call HVAC sizing simulation manager
        if (DoHVACSizingSimulation) {
            ManageHVACSizingSimulation(ErrorsFound);
        }

        ShowMessage("Beginning Simulation");
        DisplayString("Beginning Primary Simulation");

        ResetEnvironmentCounter();

        EnvCount = 0;
        WarmupFlag = true;

        while (Available) {

            GetNextEnvironment(Available, ErrorsFound);

            if (!Available) break;
            if (ErrorsFound) break;
            if ((!DoDesDaySim) && (KindOfSim != ksRunPeriodWeather)) continue;
            if ((!DoWeathSim) && (KindOfSim == ksRunPeriodWeather)) continue;
            if (KindOfSim == ksHVACSizeDesignDay) continue; // don't run these here, only for sizing simulations

            if (KindOfSim == ksHVACSizeRunPeriodDesign) continue; // don't run these here, only for sizing simulations

            ++EnvCount;

            if (sqlite) {
                sqlite->sqliteBegin();
                sqlite->createSQLiteEnvironmentPeriodRecord(DataEnvironment::CurEnvirNum, DataEnvironment::EnvironmentName, DataGlobals::KindOfSim);
                sqlite->sqliteCommit();
            }

            ExitDuringSimulations = true;
            SimsDone = true;
            DisplayString("Initializing New Environment Parameters");

            BeginEnvrnFlag = true;
            EndEnvrnFlag = false;
            EndMonthFlag = false;
            WarmupFlag = true;
            DayOfSim = 0;
            DayOfSimChr = "0";
            NumOfWarmupDays = 0;
            if (NumOfDayInEnvrn <= 365) {
                isFinalYear = true;
            }

            HVACManager::ResetNodeData(); // Reset here, because some zone calcs rely on node data (e.g. ZoneITEquip)

            bool anyEMSRan;
            ManageEMS(emsCallFromBeginNewEvironment, anyEMSRan); // calling point

            while ((DayOfSim < NumOfDayInEnvrn) || (WarmupFlag)) { // Begin day loop ...

                if (sqlite) sqlite->sqliteBegin(); // setup for one transaction per day

                ++DayOfSim;
                ObjexxFCL::gio::write(DayOfSimChr, fmtLD) << DayOfSim;
                strip(DayOfSimChr);
                if (!WarmupFlag) {
                    ++CurrentOverallSimDay;
                    DisplaySimDaysProgress(CurrentOverallSimDay, TotalOverallSimDays);
                } else {
                    DayOfSimChr = "0";
                }
                BeginDayFlag = true;
                EndDayFlag = false;

                if (WarmupFlag) {
                    ++NumOfWarmupDays;
                    cWarmupDay = TrimSigDigits(NumOfWarmupDays);
                    DisplayString("Warming up {" + cWarmupDay + '}');
                } else if (DayOfSim == 1) {
                    if (KindOfSim == ksRunPeriodWeather) {
                        DisplayString("Starting Simulation at " + DataEnvironment::CurMnDyYr + " for " + EnvironmentName);
                    } else {
                        DisplayString("Starting Simulation at " + DataEnvironment::CurMnDy + " for " + EnvironmentName);
                    }
                    ObjexxFCL::gio::write(OutputFileInits, Format_700) << NumOfWarmupDays;
                    ResetAccumulationWhenWarmupComplete();
                } else if (DisplayPerfSimulationFlag) {
                    if (KindOfSim == ksRunPeriodWeather) {
                        DisplayString("Continuing Simulation at " + DataEnvironment::CurMnDyYr + " for " + EnvironmentName);
                    } else {
                        DisplayString("Continuing Simulation at " + DataEnvironment::CurMnDy + " for " + EnvironmentName);
                    }
                    DisplayPerfSimulationFlag = false;
                }
                // for simulations that last longer than a week, identify when the last year of the simulation is started
                if ((DayOfSim > 365) && ((NumOfDayInEnvrn - DayOfSim) == 364) && !WarmupFlag) {
                    DisplayString("Starting last  year of environment at:  " + DayOfSimChr);
                    ResetTabularReports();
                }

                for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...

                    BeginHourFlag = true;
                    EndHourFlag = false;

                    for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
                        if (AnySlabsInModel || AnyBasementsInModel) {
                            SimulateGroundDomains(false);
                        }

                        if (AnyUnderwaterBoundaries) {
                            WeatherManager::UpdateUnderwaterBoundaries();
                        }

                        if (DataEnvironment::varyingLocationSchedIndexLat > 0 || DataEnvironment::varyingLocationSchedIndexLong > 0 ||
                            DataEnvironment::varyingOrientationSchedIndex > 0) {
                            WeatherManager::UpdateLocationAndOrientation();
                        }

                        BeginTimeStepFlag = true;
                        ExternalInterfaceExchangeVariables();

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

                        if (oneTimeUnderwaterBoundaryCheck) {
                            AnyUnderwaterBoundaries = WeatherManager::CheckIfAnyUnderwaterBoundaries();
                            oneTimeUnderwaterBoundaryCheck = false;
                        }

                        BeginHourFlag = false;
                        BeginDayFlag = false;
                        BeginEnvrnFlag = false;
                        BeginSimFlag = false;
                        BeginFullSimFlag = false;

                    } // TimeStep loop

                    PreviousHour = HourOfDay;

                } // ... End hour loop.

                if (sqlite) sqlite->sqliteCommit(); // one transaction per day

            } // ... End day loop.

            // Need one last call to send latest states to middleware
            ExternalInterfaceExchangeVariables();

        } // ... End environment loop.

        WarmupFlag = false;
        if (!SimsDone && DoDesDaySim) {
            if ((TotDesDays + TotRunDesPersDays) == 0) { // if sum is 0, then there was no sizing done.
                ShowWarningError("ManageSimulation: SizingPeriod:* were requested in SimulationControl but no SizingPeriod:* objects in input.");
            }
        }

        if (!SimsDone && DoWeathSim) {
            if (!RunPeriodsInInput) { // if no run period requested, and sims not done
                ShowWarningError("ManageSimulation: Weather Simulation was requested in SimulationControl but no RunPeriods in input.");
            }
        }

        PlantManager::CheckOngoingPlantWarnings();

        if (sqlite) sqlite->sqliteBegin(); // for final data to write

#ifdef EP_Detailed_Timings
        epStartTime("Closeout Reporting=");
#endif
        SimCostEstimate();

        ComputeTariff(); //     Compute the utility bills

        EMSManager::checkForUnusedActuatorsAtEnd();

        ReportForTabularReports(); // For Energy Meters (could have other things that need to be pushed to after simulation)

        OpenOutputTabularFile();

        WriteTabularReports(); //     Create the tabular reports at completion of each

        WriteTabularTariffReports();

        ComputeLifeCycleCostAndReport(); // must be after WriteTabularReports and WriteTabularTariffReports

        CloseOutputTabularFile();

        DumpAirLoopStatistics(); // Dump runtime statistics for air loop controller simulation to csv file

#ifdef EP_Detailed_Timings
        epStopTime("Closeout Reporting=");
#endif
        CloseOutputFiles();

        // sqlite->createZoneExtendedOutput();
        CreateSQLiteZoneExtendedOutput();

        if (sqlite) {
            DisplayString("Writing final SQL reports");
            sqlite->sqliteCommit();      // final transactions
            sqlite->initializeIndexes(); // do not create indexes (SQL) until all is done.
        }

        if (ErrorsFound) {
            ShowFatalError("Error condition occurred.  Previous Severe Errors cause termination.");
        }
    }

    void GetProjectData()
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
        using namespace DataConvergParams;
        using namespace DataSystemVariables;
        using DataEnvironment::DisplayWeatherMissingDataWarnings;
        using DataEnvironment::IgnoreBeamRadiation;
        using DataEnvironment::IgnoreDiffuseRadiation;
        using DataEnvironment::IgnoreSolarRadiation;
        using DataHVACGlobals::deviationFromSetPtThresholdClg;
        using DataHVACGlobals::deviationFromSetPtThresholdHtg;
        using DataHVACGlobals::LimitNumSysSteps;
        using General::RoundSigDigits;
        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_int const Div60(12, {1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string Alphas(6);
        Array1D<Real64> Number(4);
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
        static ObjexxFCL::gio::Fmt Format_721("(' Version, ',A)");
        static ObjexxFCL::gio::Fmt Format_731("(' Timesteps per Hour, ',I2,', ',I2)");
        static ObjexxFCL::gio::Fmt Format_733("(' System Convergence Limits',4(', ',A))");
        static ObjexxFCL::gio::Fmt Format_741("(' Simulation Control',$)");
        static ObjexxFCL::gio::Fmt Format_741_1("(', ',A,$)");
        static ObjexxFCL::gio::Fmt Format_751("(' Output Reporting Tolerances',5(', ',A))");

        ErrorsFound = false;

        CurrentModuleObject = "Version";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num == 1) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            std::string::size_type const lenVer(len(MatchVersion));
            if ((lenVer > 0) && (MatchVersion[lenVer - 1] == '0')) {
                Which = static_cast<int>(index(Alphas(1).substr(0, lenVer - 2), MatchVersion.substr(0, lenVer - 2)));
            } else {
                Which = static_cast<int>(index(Alphas(1), MatchVersion));
            }
            if (Which != 0) {
                ShowWarningError(CurrentModuleObject + ": in IDF=\"" + Alphas(1) + "\" not the same as expected=\"" + MatchVersion + "\"");
            }
            VersionID = Alphas(1);
        } else if (Num == 0) {
            ShowWarningError(CurrentModuleObject + ": missing in IDF, processing for EnergyPlus version=\"" + MatchVersion + "\"");
        } else {
            ShowSevereError("Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        // Do Mini Gets on HB Algorithm and by-surface overrides
        CurrentModuleObject = "HeatBalanceAlgorithm";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        CondFDAlgo = false;
        if (Num > 0) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
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
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              Item,
                                              Alphas,
                                              NumAlpha,
                                              Number,
                                              NumNumber,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
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
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              1,
                                              Alphas,
                                              NumAlpha,
                                              Number,
                                              NumNumber,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
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
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              1,
                                              cAlphaArgs,
                                              NumAlpha,
                                              Number,
                                              NumNumber,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(cAlphaArgs(2));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;
                    } else {
                    }
                }
            }
        }
        CurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num > 0) {
            for (Item = 1; Item <= Num; ++Item) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              1,
                                              cAlphaArgs,
                                              NumAlpha,
                                              Number,
                                              NumNumber,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                {
                    auto const SELECT_CASE_var(cAlphaArgs(2));
                    if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                        CondFDAlgo = true;
                    } else {
                    }
                }
            }
        }

        CurrentModuleObject = "Timestep";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num == 1) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            NumOfTimeStepInHour = Number(1);
            if (NumOfTimeStepInHour <= 0 || NumOfTimeStepInHour > 60) {
                Alphas(1) = RoundSigDigits(NumOfTimeStepInHour);
                ShowWarningError(CurrentModuleObject + ": Requested number (" + Alphas(1) + ") invalid, Defaulted to 4");
                NumOfTimeStepInHour = 4;
            } else if (mod(60, NumOfTimeStepInHour) != 0) {
                MinInt = 9999;
                for (Num = 1; Num <= 12; ++Num) {
                    if (std::abs(NumOfTimeStepInHour - Div60(Num)) > MinInt) continue;
                    MinInt = NumOfTimeStepInHour - Div60(Num);
                    Which = Num;
                }
                ShowWarningError(CurrentModuleObject + ": Requested number (" + RoundSigDigits(NumOfTimeStepInHour) +
                                 ") not evenly divisible into 60, defaulted to nearest (" + RoundSigDigits(Div60(Which)) + ").");
                NumOfTimeStepInHour = Div60(Which);
            }
            if (CondFDAlgo && NumOfTimeStepInHour < 20) {
                ShowWarningError(CurrentModuleObject + ": Requested number (" + RoundSigDigits(NumOfTimeStepInHour) +
                                 ") cannot be used when Conduction Finite Difference algorithm is selected.");
                ShowContinueError("..." + CurrentModuleObject + " is set to 20.");
                NumOfTimeStepInHour = 20;
            }
            if (NumOfTimeStepInHour < 4 && inputProcessor->getNumObjectsFound("Zone") > 0) {
                ShowWarningError(CurrentModuleObject + ": Requested number (" + RoundSigDigits(NumOfTimeStepInHour) +
                                 ") is less than the suggested minimum of 4.");
                ShowContinueError("Please see entry for " + CurrentModuleObject + " in Input/Output Reference for discussion of considerations.");
            }
        } else if (Num == 0 && inputProcessor->getNumObjectsFound("Zone") > 0 && !CondFDAlgo) {
            ShowWarningError("No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 4.");
            NumOfTimeStepInHour = 4;
        } else if (Num == 0 && !CondFDAlgo) {
            NumOfTimeStepInHour = 4;
        } else if (Num == 0 && inputProcessor->getNumObjectsFound("Zone") > 0 && CondFDAlgo) {
            ShowWarningError("No " + CurrentModuleObject + " object found.  Number of TimeSteps in Hour defaulted to 20.");
            ShowContinueError("...Due to presence of Conduction Finite Difference Algorithm selection.");
            NumOfTimeStepInHour = 20;
        } else if (Num == 0 && CondFDAlgo) {
            NumOfTimeStepInHour = 20;
        } else {
            ShowSevereError("Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        TimeStepZone = 1.0 / double(NumOfTimeStepInHour);
        MinutesPerTimeStep = TimeStepZone * 60;
        TimeStepZoneSec = TimeStepZone * SecInHour;

        CurrentModuleObject = "ConvergenceLimits";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num == 1) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            MinInt = int(Number(1));
            if (MinInt > MinutesPerTimeStep) {
                MinInt = MinutesPerTimeStep;
            }
            if (MinInt < 0 || MinInt > 60) {
                ShowWarningError(CurrentModuleObject + ": Requested " + cNumericFieldNames(1) + " (" + RoundSigDigits(MinInt) +
                                 ") invalid. Set to 1 minute.");
                MinTimeStepSys = 1.0 / 60.0;
            } else if (MinInt == 0) { // Set to TimeStepZone
                MinTimeStepSys = TimeStepZone;
            } else {
                MinTimeStepSys = double(MinInt) / 60.0;
            }
            MaxIter = int(Number(2));
            if (MaxIter <= 0) {
                MaxIter = 20;
            }
            if (!lNumericFieldBlanks(3)) MinPlantSubIterations = int(Number(3));
            if (!lNumericFieldBlanks(4)) MaxPlantSubIterations = int(Number(4));
            // trap bad values
            if (MinPlantSubIterations < 1) MinPlantSubIterations = 1;
            if (MaxPlantSubIterations < 3) MaxPlantSubIterations = 3;
            if (MinPlantSubIterations > MaxPlantSubIterations) MaxPlantSubIterations = MinPlantSubIterations + 1;

        } else if (Num == 0) {
            MinTimeStepSys = 1.0 / 60.0;
            MaxIter = 20;
            MinPlantSubIterations = 2;
            MaxPlantSubIterations = 8;
        } else {
            ShowSevereError("Too many " + CurrentModuleObject + " Objects found.");
            ErrorsFound = true;
        }

        LimitNumSysSteps = int(TimeStepZone / MinTimeStepSys);

        DebugOutput = false;
        EvenDuringWarmup = false;
        CurrentModuleObject = "Output:DebuggingData";
        NumDebugOut = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (NumDebugOut > 0) {
            inputProcessor->getObjectItem(CurrentModuleObject, 1, Alphas, NumAlpha, Number, NumNumber, IOStat);
            if (int(Number(1)) == 1) {
                DebugOutput = true;
            }
            if (int(Number(2)) == 1) {
                EvenDuringWarmup = true;
            }
        }

        CurrentModuleObject = "Output:Diagnostics";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        for (Num1 = 1; Num1 <= Num; ++Num1) {
            inputProcessor->getObjectItem(CurrentModuleObject, Num1, Alphas, NumAlpha, Number, NumNumber, IOStat);
            for (NumA = 1; NumA <= NumAlpha; ++NumA) {
                if (UtilityRoutines::SameString(Alphas(NumA), "DisplayExtraWarnings")) {
                    DisplayExtraWarnings = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayAdvancedReportVariables")) {
                    DisplayAdvancedReportVariables = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayAllWarnings")) {
                    DisplayAllWarnings = true;
                    DisplayExtraWarnings = true;
                    DisplayUnusedObjects = true;
                    DisplayUnusedSchedules = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayUnusedObjects")) {
                    DisplayUnusedObjects = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayUnusedSchedules")) {
                    DisplayUnusedSchedules = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayZoneAirHeatBalanceOffBalance")) {
                    DisplayZoneAirHeatBalanceOffBalance = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DoNotMirrorDetachedShading")) {
                    MakeMirroredDetachedShading = false;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DoNotMirrorAttachedShading")) {
                    MakeMirroredAttachedShading = false;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "IgnoreInteriorWindowTransmission")) {
                    IgnoreInteriorWindowTransmission = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "ReportDuringWarmup")) {
                    ReportDuringWarmup = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DisplayWeatherMissingDataWarnings")) {
                    DisplayWeatherMissingDataWarnings = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "IgnoreSolarRadiation")) {
                    IgnoreSolarRadiation = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "IgnoreBeamRadiation")) {
                    IgnoreBeamRadiation = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "IgnoreDiffuseRadiation")) {
                    IgnoreDiffuseRadiation = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "DeveloperFlag")) {
                    DeveloperFlag = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "TimingFlag")) {
                    TimingFlag = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "ReportDetailedWarmupConvergence")) {
                    ReportDetailedWarmupConvergence = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "ReportDuringHVACSizingSimulation")) {
                    ReportDuringHVACSizingSimulation = true;
                } else if (UtilityRoutines::SameString(Alphas(NumA), "CreateMinimalSurfaceVariables")) {
                    continue;
                    //        CreateMinimalSurfaceVariables=.TRUE.
                } else if (UtilityRoutines::SameString(Alphas(NumA), "CreateNormalSurfaceVariables")) {
                    continue;
                    //        IF (CreateMinimalSurfaceVariables) THEN
                    //          CALL ShowWarningError('GetProjectData: '//TRIM(CurrentModuleObject)//'=''//  &
                    //             TRIM(Alphas(NumA))//'', prior set=true for this condition reverts to false.')
                    //        ENDIF
                    //        CreateMinimalSurfaceVariables=.FALSE.
                } else if (!Alphas(NumA).empty()) {
                    ShowWarningError("GetProjectData: " + CurrentModuleObject + "=\"" + Alphas(NumA) +
                                     "\", Invalid value for field, entered value ignored.");
                }
            }
        }

        CurrentModuleObject = "OutputControl:ReportingTolerances";
        Num = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (Num > 0) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (!lNumericFieldBlanks(1)) {
                deviationFromSetPtThresholdHtg = -Number(1);
            } else {
                deviationFromSetPtThresholdHtg = -0.2;
            }
            if (!lNumericFieldBlanks(2)) {
                deviationFromSetPtThresholdClg = Number(2);
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
        NumRunControl = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (NumRunControl > 0) {
            RunControlInInput = true;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          1,
                                          Alphas,
                                          NumAlpha,
                                          Number,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (Alphas(1) == "YES") DoZoneSizing = true;
            if (Alphas(2) == "YES") DoSystemSizing = true;
            if (Alphas(3) == "YES") DoPlantSizing = true;
            if (Alphas(4) == "NO") DoDesDaySim = false;
            if (Alphas(5) == "NO") DoWeathSim = false;
            if (NumAlpha > 5) {
                if (Alphas(6) == "YES") DoHVACSizingSimulation = true;
            }
        }
        if (DDOnly) {
            DoDesDaySim = true;
            DoWeathSim = false;
        }
        if (FullAnnualRun) {
            DoDesDaySim = false;
            DoWeathSim = true;
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found getting Project Input");
        }

        ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Version>, Version ID";
        ObjexxFCL::gio::write(OutputFileInits, Format_721) << VersionID;

        ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Timesteps per Hour>, #TimeSteps, Minutes per TimeStep {minutes}";
        ObjexxFCL::gio::write(OutputFileInits, Format_731) << NumOfTimeStepInHour << int(MinutesPerTimeStep);

        ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <System Convergence Limits>, Minimum System TimeStep {minutes}, Max HVAC Iterations, Minimum Plant "
                                             "Iterations, Maximum Plant Iterations";
        MinInt = MinTimeStepSys * 60.0;
        ObjexxFCL::gio::write(OutputFileInits, Format_733) << RoundSigDigits(MinInt) << RoundSigDigits(MaxIter) << RoundSigDigits(MinPlantSubIterations)
                                                << RoundSigDigits(MaxPlantSubIterations);

        if (DoZoneSizing) {
            Alphas(1) = "Yes";
        } else {
            Alphas(1) = "No";
        }
        if (DoSystemSizing) {
            Alphas(2) = "Yes";
        } else {
            Alphas(2) = "No";
        }
        if (DoPlantSizing) {
            Alphas(3) = "Yes";
        } else {
            Alphas(3) = "No";
        }
        if (DoDesDaySim) {
            Alphas(4) = "Yes";
        } else {
            Alphas(4) = "No";
        }
        if (DoWeathSim) {
            Alphas(5) = "Yes";
        } else {
            Alphas(5) = "No";
        }
        if (DoHVACSizingSimulation) {
            Alphas(6) = "Yes";
            if (NumNumber >= 1) {
                HVACSizingSimMaxIterations = Number(1);
            }
        } else {
            Alphas(6) = "No";
        }

        ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Simulation Control>, Do Zone Sizing, Do System Sizing, Do Plant Sizing, Do Design Days, Do Weather "
                                             "Simulation, Do HVAC Sizing Simulation";
        ObjexxFCL::gio::write(OutputFileInits, Format_741);
        for (Num = 1; Num <= 6; ++Num) {
            ObjexxFCL::gio::write(OutputFileInits, Format_741_1) << Alphas(Num);
        }
        ObjexxFCL::gio::write(OutputFileInits);

        ObjexxFCL::gio::write(OutputFileInits, fmtA)
            << "! <Output Reporting Tolerances>, Tolerance for Time Heating Setpoint Not Met, Tolerance for Zone Cooling Setpoint Not Met Time";
        ObjexxFCL::gio::write(OutputFileInits, Format_751) << RoundSigDigits(std::abs(deviationFromSetPtThresholdHtg), 3)
                                                << RoundSigDigits(deviationFromSetPtThresholdClg, 3);

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
    }

    void CheckForMisMatchedEnvironmentSpecifications()
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
        NumZoneSizing = inputProcessor->getNumObjectsFound("Sizing:Zone");
        NumSystemSizing = inputProcessor->getNumObjectsFound("Sizing:System");
        NumPlantSizing = inputProcessor->getNumObjectsFound("Sizing:Plant");
        NumDesignDays = inputProcessor->getNumObjectsFound("SizingPeriod:DesignDay");
        NumRunPeriodDesign = inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileDays") +
                             inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileConditionType");
        NumSizingDays = NumDesignDays + NumRunPeriodDesign;
        {
            IOFlags flags;
            ObjexxFCL::gio::inquire(DataStringGlobals::inputWeatherFileName, flags);
            WeatherFileAttached = flags.exists();
        }

        if (RunControlInInput) {
            if (DoZoneSizing) {
                if (NumZoneSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError(
                        "CheckEnvironmentSpecifications: Sizing for Zones has been requested but there are no design environments specified.");
                    ShowContinueError("...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumZoneSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError("CheckEnvironmentSpecifications: Sizing for Zones has been requested; Design period from the weather file "
                                    "requested; but no weather file specified.");
                }
            }
            if (DoSystemSizing) {
                if (NumSystemSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError(
                        "CheckEnvironmentSpecifications: Sizing for Systems has been requested but there are no design environments specified.");
                    ShowContinueError("...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumSystemSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError("CheckEnvironmentSpecifications: Sizing for Systems has been requested; Design period from the weather file "
                                    "requested; but no weather file specified.");
                }
            }
            if (DoPlantSizing) {
                if (NumPlantSizing > 0 && NumSizingDays == 0) {
                    ErrorsFound = true;
                    ShowSevereError("CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested but there are no design "
                                    "environments specified.");
                    ShowContinueError("...Add appropriate SizingPeriod:* objects for your simulation.");
                }
                if (NumPlantSizing > 0 && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                    ErrorsFound = true;
                    ShowSevereError("CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested; Design period from the weather "
                                    "file requested; but no weather file specified.");
                }
            }
            if (DoDesDaySim && NumSizingDays == 0) {
                ShowWarningError("CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations, but no design "
                                 "environments specified.");
                ShowContinueError(
                    "...No design environment results produced. For these results, add appropriate SizingPeriod:* objects for your simulation.");
            }
            if (DoDesDaySim && NumRunPeriodDesign > 0 && !WeatherFileAttached) {
                ErrorsFound = true;
                ShowSevereError("CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations; weather file design "
                                "environments specified; but no weather file specified.");
            }
            if (DoWeathSim && !RunPeriodsInInput) {
                ShowWarningError("CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations, but no run periods for "
                                 "weather file specified.  No annual results produced.");
            }
            if (DoWeathSim && RunPeriodsInInput && !WeatherFileAttached) {
                ShowWarningError("CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations; run periods for weather "
                                 "file specified; but no weather file specified.");
            }
        }
        if (!DoDesDaySim && !DoWeathSim) {
            ShowWarningError("\"Do the design day simulations\" and \"Do the weather file simulation\" are both set to \"No\".  No simulations will "
                             "be performed, and most input will not be read.");
        }
        if (!DoZoneSizing && !DoSystemSizing && !DoPlantSizing && !DoDesDaySim && !DoWeathSim) {
            ShowSevereError("All elements of SimulationControl are set to \"No\". No simulations can be done.  Program terminates.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError("Program terminates due to preceding conditions.");
        }
    }

    void CheckForRequestedReporting()
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
        SimPeriods =
            (inputProcessor->getNumObjectsFound("SizingPeriod:DesignDay") > 0 ||
             inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileDays") > 0 ||
             inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileConditionType") > 0 || inputProcessor->getNumObjectsFound("RunPeriod") > 0);

        if ((DoDesDaySim || DoWeathSim) && SimPeriods) {
            ReportingRequested =
                (inputProcessor->getNumObjectsFound("Output:Table:SummaryReports") > 0 ||
                 inputProcessor->getNumObjectsFound("Output:Table:TimeBins") > 0 || inputProcessor->getNumObjectsFound("Output:Table:Monthly") > 0 ||
                 inputProcessor->getNumObjectsFound("Output:Variable") > 0 || inputProcessor->getNumObjectsFound("Output:Meter") > 0 ||
                 inputProcessor->getNumObjectsFound("Output:Meter:MeterFileOnly") > 0 ||
                 inputProcessor->getNumObjectsFound("Output:Meter:Cumulative") > 0 ||
                 inputProcessor->getNumObjectsFound("Output:Meter:Cumulative:MeterFileOnly") > 0);
            // Not testing for : Output:SQLite or Output:EnvironmentalImpactFactors
            if (!ReportingRequested) {
                ShowWarningError("No reporting elements have been requested. No simulation results produced.");
                ShowContinueError("...Review requirements such as \"Output:Table:SummaryReports\", \"Output:Table:Monthly\", \"Output:Variable\", "
                                  "\"Output:Meter\" and others.");
            }
        }
    }

    void OpenStreamFile(const std::string &fileName, int &unitNumber, std::ostream *&out_stream)
    {
        int write_stat;
        unitNumber = GetNewUnitNumber();
        {
            IOFlags flags;
            flags.ACTION("write");
            flags.STATUS("UNKNOWN");
            ObjexxFCL::gio::open(unitNumber, fileName, flags);
            write_stat = flags.ios();
        }
        if (write_stat != 0) {
            ShowFatalError("OpenOutputFiles: Could not open file " + fileName + " for output (write).");
        }
        out_stream = ObjexxFCL::gio::out_stream(unitNumber);
    }

    void OpenOutputJsonFiles()
    {

        //// timeSeriesAndTabularEnabled() will return true if only timeSeriesAndTabular is set, that's the only time we write to that file
        if (ResultsFramework::OutputSchema->timeSeriesAndTabularEnabled()) {
            if (ResultsFramework::OutputSchema->JSONEnabled()) {
                OpenStreamFile(DataStringGlobals::outputJsonFileName, jsonOutputStreams.OutputFileJson, jsonOutputStreams.json_stream);
            }
            if (ResultsFramework::OutputSchema->CBOREnabled()) {
                OpenStreamFile(DataStringGlobals::outputCborFileName, jsonOutputStreams.OutputFileCBOR, jsonOutputStreams.cbor_stream);
            }
            if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                OpenStreamFile(DataStringGlobals::outputMsgPackFileName, jsonOutputStreams.OutputFileMsgPack, jsonOutputStreams.msgpack_stream);
            }
        }
        //// timeSeriesEnabled() will return true if timeSeries is set, so we can write meter reports
        if (ResultsFramework::OutputSchema->timeSeriesEnabled()) {
            // Output detailed Zone time series file
            if (ResultsFramework::OutputSchema->RIDetailedZoneTSData.rDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIDetailedZoneTSData.iDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSZoneJsonFileName, jsonOutputStreams.OutputFileTSZoneJson,
                                   jsonOutputStreams.json_TSstream_Zone);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSZoneCborFileName, jsonOutputStreams.OutputFileTSZoneCBOR,
                                   jsonOutputStreams.cbor_TSstream_Zone);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSZoneMsgPackFileName, jsonOutputStreams.OutputFileTSZoneMsgPack,
                                   jsonOutputStreams.msgpack_TSstream_Zone);
                }
            }

            // Output detailed HVAC time series file
            if (ResultsFramework::OutputSchema->RIDetailedHVACTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIDetailedHVACTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSHvacJsonFileName, jsonOutputStreams.OutputFileTSHVACJson,
                                   jsonOutputStreams.json_TSstream_HVAC);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSHvacCborFileName, jsonOutputStreams.OutputFileTSHVACCBOR,
                                   jsonOutputStreams.cbor_TSstream_HVAC);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSHvacMsgPackFileName, jsonOutputStreams.OutputFileTSHVACMsgPack,
                                   jsonOutputStreams.msgpack_TSstream_HVAC);
                }
            }

            // Output timestep time series file
            if (ResultsFramework::OutputSchema->RITimestepTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RITimestepTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSJsonFileName, jsonOutputStreams.OutputFileTSJson, jsonOutputStreams.json_TSstream);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSCborFileName, jsonOutputStreams.OutputFileTSCBOR, jsonOutputStreams.cbor_TSstream);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputTSMsgPackFileName, jsonOutputStreams.OutputFileTSMsgPack,
                                   jsonOutputStreams.msgpack_TSstream);
                }
            }

            // Output hourly time series file
            if (ResultsFramework::OutputSchema->RIHourlyTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIHourlyTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputHRJsonFileName, jsonOutputStreams.OutputFileHRJson, jsonOutputStreams.json_HRstream);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputHRCborFileName, jsonOutputStreams.OutputFileHRCBOR, jsonOutputStreams.cbor_HRstream);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputHRMsgPackFileName, jsonOutputStreams.OutputFileHRMsgPack,
                                   jsonOutputStreams.msgpack_HRstream);
                }
            }

            // Output daily time series file
            if (ResultsFramework::OutputSchema->RIDailyTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIDailyTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputDYJsonFileName, jsonOutputStreams.OutputFileDYJson, jsonOutputStreams.json_DYstream);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputDYCborFileName, jsonOutputStreams.OutputFileDYCBOR, jsonOutputStreams.cbor_DYstream);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputDYMsgPackFileName, jsonOutputStreams.OutputFileDYMsgPack,
                                   jsonOutputStreams.msgpack_DYstream);
                }
            }

            // Output monthly time series file
            if (ResultsFramework::OutputSchema->RIMonthlyTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIMonthlyTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputMNJsonFileName, jsonOutputStreams.OutputFileMNJson, jsonOutputStreams.json_MNstream);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputMNCborFileName, jsonOutputStreams.OutputFileMNCBOR, jsonOutputStreams.cbor_MNstream);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputMNMsgPackFileName, jsonOutputStreams.OutputFileMNMsgPack,
                                   jsonOutputStreams.msgpack_MNstream);
                }
            }

            // Output run period time series file
            if (ResultsFramework::OutputSchema->RIRunPeriodTSData.iDataFrameEnabled() ||
                ResultsFramework::OutputSchema->RIRunPeriodTSData.rDataFrameEnabled()) {
                if (ResultsFramework::OutputSchema->JSONEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputSMJsonFileName, jsonOutputStreams.OutputFileSMJson, jsonOutputStreams.json_SMstream);
                }
                if (ResultsFramework::OutputSchema->CBOREnabled()) {
                    OpenStreamFile(DataStringGlobals::outputSMCborFileName, jsonOutputStreams.OutputFileSMCBOR, jsonOutputStreams.cbor_SMstream);
                }
                if (ResultsFramework::OutputSchema->MsgPackEnabled()) {
                    OpenStreamFile(DataStringGlobals::outputSMMsgPackFileName, jsonOutputStreams.OutputFileSMMsgPack,
                                   jsonOutputStreams.msgpack_SMstream);
                }
            }
        }
    }

    void OpenOutputFiles()
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
        {
            IOFlags flags;
            flags.ACTION("write");
            flags.STATUS("UNKNOWN");
            ObjexxFCL::gio::open(OutputFileStandard, DataStringGlobals::outputEsoFileName, flags);
            write_stat = flags.ios();
        }
        if (write_stat != 0) {
            ShowFatalError("OpenOutputFiles: Could not open file " + DataStringGlobals::outputEsoFileName + " for output (write).");
        }
        eso_stream = ObjexxFCL::gio::out_stream(OutputFileStandard);
        ObjexxFCL::gio::write(OutputFileStandard, fmtA) << "Program Version," + VerString;

        // Open the Initialization Output File
        OutputFileInits = GetNewUnitNumber();
        {
            IOFlags flags;
            flags.ACTION("write");
            flags.STATUS("UNKNOWN");
            ObjexxFCL::gio::open(OutputFileInits, DataStringGlobals::outputEioFileName, flags);
            write_stat = flags.ios();
        }
        if (write_stat != 0) {
            ShowFatalError("OpenOutputFiles: Could not open file " + DataStringGlobals::outputEioFileName + " for output (write).");
        }
        eio_stream = ObjexxFCL::gio::out_stream(OutputFileInits);
        ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Program Version," + VerString;

        // Open the Meters Output File
        OutputFileMeters = GetNewUnitNumber();
        StdMeterRecordCount = 0;
        {
            IOFlags flags;
            flags.ACTION("write");
            flags.STATUS("UNKNOWN");
            ObjexxFCL::gio::open(OutputFileMeters, DataStringGlobals::outputMtrFileName, flags);
            write_stat = flags.ios();
        }
        if (write_stat != 0) {
            ShowFatalError("OpenOutputFiles: Could not open file " + DataStringGlobals::outputMtrFileName + " for output (write).");
        }
        mtr_stream = ObjexxFCL::gio::out_stream(OutputFileMeters);
        ObjexxFCL::gio::write(OutputFileMeters, fmtA) << "Program Version," + VerString;

        // Open the Branch-Node Details Output File
        OutputFileBNDetails = GetNewUnitNumber();
        {
            IOFlags flags;
            flags.ACTION("write");
            flags.STATUS("UNKNOWN");
            ObjexxFCL::gio::open(OutputFileBNDetails, DataStringGlobals::outputBndFileName, flags);
            write_stat = flags.ios();
        }
        if (write_stat != 0) {
            ShowFatalError("OpenOutputFiles: Could not open file " + DataStringGlobals::outputBndFileName + " for output (write).");
        }
        ObjexxFCL::gio::write(OutputFileBNDetails, fmtA) << "Program Version," + VerString;
    }

    void CloseOutputFiles()
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
        using OutputProcessor::InstMeterCacheSize;
        using OutputProcessor::MaxIVariable;
        using OutputProcessor::MaxRVariable;
        using OutputProcessor::NumEnergyMeters;
        using OutputProcessor::NumOfIVariable;
        using OutputProcessor::NumOfIVariable_Setup;
        using OutputProcessor::NumOfIVariable_Sum;
        using OutputProcessor::NumOfRVariable;
        using OutputProcessor::NumOfRVariable_Meter;
        using OutputProcessor::NumOfRVariable_Setup;
        using OutputProcessor::NumOfRVariable_Sum;
        using OutputProcessor::NumReportList;
        using OutputProcessor::NumTotalIVariable;
        using OutputProcessor::NumTotalRVariable;
        using OutputProcessor::NumVarMeterArrays;
        using OutputReportTabular::maxUniqueKeyCount;
        using OutputReportTabular::MonthlyFieldSetInputCount;
        using SolarShading::MAXHCArrayBounds;
        using SolarShading::maxNumberOfFigures;
        using namespace DataRuntimeLanguage;
        using DataBranchNodeConnections::MaxNumOfNodeConnections;
        using DataBranchNodeConnections::NumOfNodeConnections;
        using DataHeatBalance::CondFDRelaxFactor;
        using DataHeatBalance::CondFDRelaxFactorInput;
        using DataHeatBalance::HeatTransferAlgosUsed;
        using DataHeatBalance::UseCondFD;
        using General::RoundSigDigits;
        using namespace DataSystemVariables; // , ONLY: MaxNumberOfThreads,NumberIntRadThreads,iEnvSetThreads
        using DataSurfaces::MaxVerticesPerSurface;
        using namespace DataTimings;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt EndOfDataFormat("(\"End of Data\")"); // Signifies the end of the data block in the output file
        static std::string const ThreadingHeader("! <Program Control Information:Threads/Parallel Sims>, Threading Supported,Maximum Number of "
                                                 "Threads, Env Set Threads (OMP_NUM_THREADS), EP Env Set Threads (EP_OMP_NUM_THREADS), IDF Set "
                                                 "Threads, Number of Threads Used (Interior Radiant Exchange), Number Nominal Surfaces, Number "
                                                 "Parallel Sims");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EchoInputFile; // found unit number for 'eplusout.audit'
        std::string cEnvSetThreads;
        std::string cepEnvSetThreads;
        std::string cIDFSetThreads;

        EchoInputFile = FindUnitNumber(DataStringGlobals::outputAuditFileName);
        // Record some items on the audit file
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfRVariable=" << NumOfRVariable_Setup;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfRVariable(Total)=" << NumTotalRVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfRVariable(Actual)=" << NumOfRVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfRVariable(Summed)=" << NumOfRVariable_Sum;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfRVariable(Meter)=" << NumOfRVariable_Meter;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfIVariable=" << NumOfIVariable_Setup;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfIVariable(Total)=" << NumTotalIVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfIVariable(Actual)=" << NumOfIVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfIVariable(Summed)=" << NumOfIVariable_Sum;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MaxRVariable=" << MaxRVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MaxIVariable=" << MaxIVariable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumEnergyMeters=" << NumEnergyMeters;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumVarMeterArrays=" << NumVarMeterArrays;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "maxUniqueKeyCount=" << maxUniqueKeyCount;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "maxNumberOfFigures=" << maxNumberOfFigures;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MAXHCArrayBounds=" << MAXHCArrayBounds;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MaxVerticesPerSurface=" << MaxVerticesPerSurface;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumReportList=" << NumReportList;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "InstMeterCacheSize=" << InstMeterCacheSize;
        if (SutherlandHodgman) {
            ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "ClippingAlgorithm=SutherlandHodgman";
        } else {
            ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "ClippingAlgorithm=ConvexWeilerAtherton";
        }
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MonthlyFieldSetInputCount=" << MonthlyFieldSetInputCount;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumConsideredOutputVariables=" << NumConsideredOutputVariables;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MaxConsideredOutputVariables=" << MaxConsideredOutputVariables;

        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "numActuatorsUsed=" << numActuatorsUsed;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "numEMSActuatorsAvailable=" << numEMSActuatorsAvailable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "maxEMSActuatorsAvailable=" << maxEMSActuatorsAvailable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "numInternalVariablesUsed=" << NumInternalVariablesUsed;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "numEMSInternalVarsAvailable=" << numEMSInternalVarsAvailable;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "maxEMSInternalVarsAvailable=" << maxEMSInternalVarsAvailable;

        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumOfNodeConnections=" << NumOfNodeConnections;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "MaxNumOfNodeConnections=" << MaxNumOfNodeConnections;
#ifdef EP_Count_Calls
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumShadow_Calls=" << NumShadow_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumShadowAtTS_Calls=" << NumShadowAtTS_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumClipPoly_Calls=" << NumClipPoly_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumInitSolar_Calls=" << NumInitSolar_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumAnisoSky_Calls=" << NumAnisoSky_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumDetPolyOverlap_Calls=" << NumDetPolyOverlap_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumCalcPerSolBeam_Calls=" << NumCalcPerSolBeam_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumDetShadowCombs_Calls=" << NumDetShadowCombs_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntSolarDist_Calls=" << NumIntSolarDist_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntRadExchange_Calls=" << NumIntRadExchange_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntRadExchangeZ_Calls=" << NumIntRadExchangeZ_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntRadExchangeMain_Calls=" << NumIntRadExchangeMain_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntRadExchangeOSurf_Calls=" << NumIntRadExchangeOSurf_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumIntRadExchangeISurf_Calls=" << NumIntRadExchangeISurf_Calls;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumMaxInsideSurfIterations=" << NumMaxInsideSurfIterations;
        ObjexxFCL::gio::write(EchoInputFile, fmtLD) << "NumCalcScriptF_Calls=" << NumCalcScriptF_Calls;
#endif

        ObjexxFCL::gio::write(OutputFileStandard, EndOfDataFormat);
        ObjexxFCL::gio::write(OutputFileStandard, fmtLD) << "Number of Records Written=" << StdOutputRecordCount;
        if (StdOutputRecordCount > 0) {
            ObjexxFCL::gio::close(OutputFileStandard);
        } else {
            {
                IOFlags flags;
                flags.DISPOSE("DELETE");
                ObjexxFCL::gio::close(OutputFileStandard, flags);
            }
        }
        eso_stream = nullptr;

        if (any_eq(HeatTransferAlgosUsed, UseCondFD)) { // echo out relaxation factor, it may have been changed by the program
            ObjexxFCL::gio::write(OutputFileInits, fmtA)
                << "! <ConductionFiniteDifference Numerical Parameters>, Starting Relaxation Factor, Final Relaxation Factor";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "ConductionFiniteDifference Numerical Parameters, " + RoundSigDigits(CondFDRelaxFactorInput, 3) +
                                                     ", " + RoundSigDigits(CondFDRelaxFactor, 3);
        }
        // Report number of threads to eio file
        if (Threading) {
            if (iEnvSetThreads == 0) {
                cEnvSetThreads = "Not Set";
            } else {
                cEnvSetThreads = RoundSigDigits(iEnvSetThreads);
            }
            if (iepEnvSetThreads == 0) {
                cepEnvSetThreads = "Not Set";
            } else {
                cepEnvSetThreads = RoundSigDigits(iepEnvSetThreads);
            }
            if (iIDFSetThreads == 0) {
                cIDFSetThreads = "Not Set";
            } else {
                cIDFSetThreads = RoundSigDigits(iIDFSetThreads);
            }
            if (lnumActiveSims) {
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << ThreadingHeader;
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Program Control:Threads/Parallel Sims, Yes," + RoundSigDigits(MaxNumberOfThreads) + ", " +
                                                         cEnvSetThreads + ", " + cepEnvSetThreads + ", " + cIDFSetThreads + ", " +
                                                         RoundSigDigits(NumberIntRadThreads) + ", " + RoundSigDigits(iNominalTotSurfaces) + ", " +
                                                         RoundSigDigits(inumActiveSims);
            } else {
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << ThreadingHeader;
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Program Control:Threads/Parallel Sims, Yes," + RoundSigDigits(MaxNumberOfThreads) + ", " +
                                                         cEnvSetThreads + ", " + cepEnvSetThreads + ", " + cIDFSetThreads + ", " +
                                                         RoundSigDigits(NumberIntRadThreads) + ", " + RoundSigDigits(iNominalTotSurfaces) + ", N/A";
            }
        } else { // no threading
            if (lnumActiveSims) {
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << ThreadingHeader;
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Program Control:Threads/Parallel Sims, No," + RoundSigDigits(MaxNumberOfThreads) +
                                                         ", N/A, N/A, N/A, N/A, N/A, " + RoundSigDigits(inumActiveSims);
            } else {
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << ThreadingHeader;
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Program Control:Threads/Parallel Sims, No," + RoundSigDigits(MaxNumberOfThreads) +
                                                         ", N/A, N/A, N/A, N/A, N/A, N/A";
            }
        }

        // Close the Initialization Output File
        ObjexxFCL::gio::write(OutputFileInits, EndOfDataFormat);
        ObjexxFCL::gio::close(OutputFileInits);
        eio_stream = nullptr;

        // Close the Meters Output File
        ObjexxFCL::gio::write(OutputFileMeters, EndOfDataFormat);
        ObjexxFCL::gio::write(OutputFileMeters, fmtLD) << "Number of Records Written=" << StdMeterRecordCount;
        if (StdMeterRecordCount > 0) {
            ObjexxFCL::gio::close(OutputFileMeters);
        } else {
            {
                IOFlags flags;
                flags.DISPOSE("DELETE");
                ObjexxFCL::gio::close(OutputFileMeters, flags);
            }
        }
        mtr_stream = nullptr;

        // Close the External Shading Output File

        if (OutputFileShadingFrac > 0) {
            ObjexxFCL::gio::close(OutputFileShadingFrac);
        }
    }

    void SetupSimulation(bool &ErrorsFound)
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
        using DataEnvironment::EndMonthFlag;
        using DataEnvironment::EnvironmentName;
        using ExteriorEnergyUse::ManageExteriorEnergyUse;
        using General::TrimSigDigits;
        using namespace DataTimings;
        using PlantPipingSystemsManager::CheckIfAnyBasements;
        using PlantPipingSystemsManager::CheckIfAnySlabs;
        using PlantPipingSystemsManager::SimulateGroundDomains;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool Available(false); // an environment is available to process
        //  integer :: env_iteration=0
        //  CHARACTER(len=32) :: cEnvChar

        //  return  ! remove comment to do 'old way'

        Available = true;

        while (Available) { // do for each environment

            GetNextEnvironment(Available, ErrorsFound);

            if (!Available) break;
            if (ErrorsFound) break;

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

            if (DeveloperFlag) DisplayString("Initializing Simulation - timestep 1:" + EnvironmentName);

            BeginTimeStepFlag = true;

            ManageWeather();

            ManageExteriorEnergyUse();

            ManageHeatBalance();

            BeginHourFlag = false;
            BeginDayFlag = false;
            BeginEnvrnFlag = false;
            BeginSimFlag = false;
            BeginFullSimFlag = false;

            //          ! do another timestep=1
            if (DeveloperFlag) DisplayString("Initializing Simulation - 2nd timestep 1:" + EnvironmentName);

            ManageWeather();

            ManageExteriorEnergyUse();

            ManageHeatBalance();

            //         do an end of day, end of environment time step

            HourOfDay = 24;
            TimeStep = NumOfTimeStepInHour;
            EndEnvrnFlag = true;

            if (DeveloperFlag) DisplayString("Initializing Simulation - hour 24 timestep 1:" + EnvironmentName);
            ManageWeather();

            ManageExteriorEnergyUse();

            ManageHeatBalance();

        } // ... End environment loop.

        if (AnySlabsInModel || AnyBasementsInModel) {
            SimulateGroundDomains(true);
        }

        if (!ErrorsFound) SimCostEstimate(); // basically will get and check input
        if (ErrorsFound) ShowFatalError("Previous conditions cause program termination.");
    }

    void ReportNodeConnections()
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
        using DataGlobals::OutputFileBNDetails;
        using DataLoopNode::NodeID;
        using DataLoopNode::NumOfNodes;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int Loop1;
        int NumParents;
        int NumNonParents;
        int NumNonConnected;
        std::string ChrOut;
        bool ParentComponentFound;

        // Formats
        static ObjexxFCL::gio::Fmt Format_701("(A)");
        static ObjexxFCL::gio::Fmt Format_702("('! <#',A,' Node Connections>,<Number of ',A,' Node Connections>')");
        static ObjexxFCL::gio::Fmt Format_703(
            "('! <',A,' Node Connection>,<Node Name>,<Node ObjectType>,<Node ObjectName>,','<Node ConnectionType>,<Node FluidStream>')");
        static ObjexxFCL::gio::Fmt Format_705("('! <#NonConnected Nodes>,<Number of NonConnected Nodes>',/,' #NonConnected Nodes,',A)");
        static ObjexxFCL::gio::Fmt Format_706("('! <NonConnected Node>,<NonConnected Node Number>,<NonConnected Node Name>')");

        NonConnectedNodes.dimension(NumOfNodes, true);

        NumParents = 0;
        NumNonParents = 0;
        for (Loop = 1; Loop <= NumOfNodeConnections; ++Loop) {
            if (NodeConnections(Loop).ObjectIsParent) continue;
            ++NumNonParents;
        }
        NumParents = NumOfNodeConnections - NumNonParents;
        ParentNodeList.allocate(NumParents);

        //  Do Parent Objects
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_702) << "Parent"
                                                    << "Parent";
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumParents;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " #Parent Node Connections," + stripped(ChrOut);
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_703) << "Parent";

        for (Loop = 1; Loop <= NumOfNodeConnections; ++Loop) {
            if (!NodeConnections(Loop).ObjectIsParent) continue;
            NonConnectedNodes(NodeConnections(Loop).NodeNumber) = false;
            ObjexxFCL::gio::write(ChrOut, fmtLD) << NodeConnections(Loop).FluidStream;
            strip(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " Parent Node Connection," + NodeConnections(Loop).NodeName + ',' +
                                                               NodeConnections(Loop).ObjectType + ',' + NodeConnections(Loop).ObjectName + ',' +
                                                               NodeConnections(Loop).ConnectionType + ',' + ChrOut;
            // Build ParentNodeLists
            if (UtilityRoutines::SameString(NodeConnections(Loop).ConnectionType, "Inlet") ||
                UtilityRoutines::SameString(NodeConnections(Loop).ConnectionType, "Outlet")) {
                ParentComponentFound = false;
                for (Loop1 = 1; Loop1 <= NumOfActualParents; ++Loop1) {
                    if (ParentNodeList(Loop1).CType != NodeConnections(Loop).ObjectType ||
                        ParentNodeList(Loop1).CName != NodeConnections(Loop).ObjectName)
                        continue;
                    ParentComponentFound = true;
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(NodeConnections(Loop).ConnectionType));
                        if (SELECT_CASE_var == "INLET") {
                            ParentNodeList(Loop1).InletNodeName = NodeConnections(Loop).NodeName;
                        } else if (SELECT_CASE_var == "OUTLET") {
                            ParentNodeList(Loop1).OutletNodeName = NodeConnections(Loop).NodeName;
                        }
                    }
                }
                if (!ParentComponentFound) {
                    ++NumOfActualParents;
                    ParentNodeList(NumOfActualParents).CType = NodeConnections(Loop).ObjectType;
                    ParentNodeList(NumOfActualParents).CName = NodeConnections(Loop).ObjectName;
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(NodeConnections(Loop).ConnectionType));
                        if (SELECT_CASE_var == "INLET") {
                            ParentNodeList(NumOfActualParents).InletNodeName = NodeConnections(Loop).NodeName;
                        } else if (SELECT_CASE_var == "OUTLET") {
                            ParentNodeList(NumOfActualParents).OutletNodeName = NodeConnections(Loop).NodeName;
                        }
                    }
                }
            }
        }

        //  Do non-Parent Objects
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_702) << "Non-Parent"
                                                    << "Non-Parent";
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumNonParents;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " #Non-Parent Node Connections," + stripped(ChrOut);
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_703) << "Non-Parent";

        for (Loop = 1; Loop <= NumOfNodeConnections; ++Loop) {
            if (NodeConnections(Loop).ObjectIsParent) continue;
            NonConnectedNodes(NodeConnections(Loop).NodeNumber) = false;
            ObjexxFCL::gio::write(ChrOut, fmtLD) << NodeConnections(Loop).FluidStream;
            strip(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " Non-Parent Node Connection," + NodeConnections(Loop).NodeName + ',' +
                                                               NodeConnections(Loop).ObjectType + ',' + NodeConnections(Loop).ObjectName + ',' +
                                                               NodeConnections(Loop).ConnectionType + ',' + ChrOut;
        }

        NumNonConnected = 0;
        for (Loop = 1; Loop <= NumOfNodes; ++Loop) {
            if (NonConnectedNodes(Loop)) ++NumNonConnected;
        }

        if (NumNonConnected > 0) {
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
            ObjexxFCL::gio::write(ChrOut, fmtLD) << NumNonConnected;
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_705) << stripped(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_706);
            for (Loop = 1; Loop <= NumOfNodes; ++Loop) {
                if (!NonConnectedNodes(Loop)) continue;
                ObjexxFCL::gio::write(ChrOut, fmtLD) << Loop;
                strip(ChrOut);
                ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " NonConnected Node," + ChrOut + ',' + NodeID(Loop);
            }
        }

        NonConnectedNodes.deallocate();
    }

    void ReportLoopConnections()
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

        // Using/Aliasing
        using namespace DataAirLoop;
        using namespace DataBranchNodeConnections;
        using DataLoopNode::NodeID;
        using DataLoopNode::NumOfNodes;
        using namespace DataHVACGlobals;
        using namespace DataPlant;
        using namespace DataZoneEquipment;
        using DataErrorTracking::AbortProcessing; // used here to turn off Node Connection Error reporting
        using DataErrorTracking::AskForConnectionsReport;
        using DataGlobals::OutputFileBNDetails;
        using DualDuct::ReportDualDuctConnections;
        using OutAirNodeManager::NumOutsideAirNodes;
        using OutAirNodeManager::OutsideAirNodeList;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const errstring("**error**");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string ChrOut;
        std::string ChrOut2;
        std::string ChrOut3;
        std::string LoopString;
        std::string ChrName;
        int Count;
        int Count1;
        int LoopSideNum;
        static bool WarningOut(true);
        int NumOfControlledZones;

        // Formats
        static ObjexxFCL::gio::Fmt Format_700("('! <#Component Sets>,<Number of Component Sets>')");
        static ObjexxFCL::gio::Fmt Format_701("(A)");
        static ObjexxFCL::gio::Fmt Format_702("('! <Component Set>,<Component Set Count>,<Parent Object Type>,<Parent Object Name>,','<Component "
                                   "Type>,<Component Name>,<Inlet Node ID>,<Outlet Node ID>,<Description>')");
        static ObjexxFCL::gio::Fmt Format_707("(1X,A)");
        static ObjexxFCL::gio::Fmt Format_713("(A)");
        static ObjexxFCL::gio::Fmt Format_720("('! <#Zone Equipment Lists>,<Number of Zone Equipment Lists>')");
        static ObjexxFCL::gio::Fmt Format_721("(A)");
        static ObjexxFCL::gio::Fmt Format_722(
            "('! <Zone Equipment List>,<Zone Equipment List Count>,<Zone Equipment List Name>,<Zone Name>,<Number of Components>')");
        static ObjexxFCL::gio::Fmt Format_723("('! <Zone Equipment Component>,<Component Count>,<Component Type>,<Component Name>,','<Zone Name>,<Heating "
                                   "Priority>,<Cooling Priority>')");

        // Report outside air node names on the Branch-Node Details file
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! #Outdoor Air Nodes,<Number of Outdoor Air Nodes>";
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumOutsideAirNodes;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " #Outdoor Air Nodes," + stripped(ChrOut);
        if (NumOutsideAirNodes > 0) {
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! <Outdoor Air Node>,<NodeNumber>,<Node Name>";
        }
        for (Count = 1; Count <= NumOutsideAirNodes; ++Count) {
            ObjexxFCL::gio::write(ChrOut, fmtLD) << OutsideAirNodeList(Count);
            strip(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " Outdoor Air Node," + ChrOut + ',' + NodeID(OutsideAirNodeList(Count));
        }
        // Component Sets
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_700);
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumCompSets;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " #Component Sets," + stripped(ChrOut);
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_702);

        for (Count = 1; Count <= NumCompSets; ++Count) {
            ObjexxFCL::gio::write(ChrOut, fmtLD) << Count;
            strip(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << " Component Set," + ChrOut + ',' + CompSets(Count).ParentCType + ',' +
                                                               CompSets(Count).ParentCName + ',' + CompSets(Count).CType + ',' +
                                                               CompSets(Count).CName + ',' + CompSets(Count).InletNodeName + ',' +
                                                               CompSets(Count).OutletNodeName + ',' + CompSets(Count).Description;

            if (CompSets(Count).ParentCType == "UNDEFINED" || CompSets(Count).InletNodeName == "UNDEFINED" ||
                CompSets(Count).OutletNodeName == "UNDEFINED") {
                if (AbortProcessing && WarningOut) {
                    ShowWarningError("Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    WarningOut = false;
                }
                ShowWarningError("Node Connection Error for object " + CompSets(Count).CType + ", name=" + CompSets(Count).CName);
                ShowContinueError("  " + CompSets(Count).Description + " not on any Branch or Parent Object");
                ShowContinueError("  Inlet Node : " + CompSets(Count).InletNodeName);
                ShowContinueError("  Outlet Node: " + CompSets(Count).OutletNodeName);
                ++NumNodeConnectionErrors;
                if (UtilityRoutines::SameString(CompSets(Count).CType, "SolarCollector:UnglazedTranspired")) {
                    ShowContinueError("This report does not necessarily indicate a problem for a MultiSystem Transpired Collector");
                }
            }
            if (CompSets(Count).Description == "UNDEFINED") {
                if (AbortProcessing && WarningOut) {
                    ShowWarningError("Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    WarningOut = false;
                }
                ShowWarningError("Potential Node Connection Error for object " + CompSets(Count).CType + ", name=" + CompSets(Count).CName);
                ShowContinueError("  Node Types are still UNDEFINED -- See Branch/Node Details file for further information");
                ShowContinueError("  Inlet Node : " + CompSets(Count).InletNodeName);
                ShowContinueError("  Outlet Node: " + CompSets(Count).OutletNodeName);
                ++NumNodeConnectionErrors;
            }
        }

        for (Count = 1; Count <= NumCompSets; ++Count) {
            for (Count1 = Count + 1; Count1 <= NumCompSets; ++Count1) {
                if (CompSets(Count).CType != CompSets(Count1).CType) continue;
                if (CompSets(Count).CName != CompSets(Count1).CName) continue;
                if (CompSets(Count).InletNodeName != CompSets(Count1).InletNodeName) continue;
                if (CompSets(Count).OutletNodeName != CompSets(Count1).OutletNodeName) continue;
                if (AbortProcessing && WarningOut) {
                    ShowWarningError("Node Connection errors shown during \"fatal error\" processing may be false because not all inputs may have "
                                     "been retrieved.");
                    WarningOut = false;
                }
                ShowWarningError("Component plus inlet/outlet node pair used more than once:");
                ShowContinueError("  Component  : " + CompSets(Count).CType + ", name=" + CompSets(Count).CName);
                ShowContinueError("  Inlet Node : " + CompSets(Count).InletNodeName);
                ShowContinueError("  Outlet Node: " + CompSets(Count).OutletNodeName);
                ShowContinueError("  Used by    : " + CompSets(Count).ParentCType + ' ' + CompSets(Count).ParentCName);
                ShowContinueError("  and  by    : " + CompSets(Count1).ParentCType + ' ' + CompSets(Count1).ParentCName);
                ++NumNodeConnectionErrors;
            }
        }
        //  Plant Loops
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumPlantLoops;
        strip(ChrOut);
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <# Plant Loops>,<Number of Plant Loops>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_707) << "#Plant Loops," + ChrOut;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Plant Loop>,<Plant Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Plant Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Plant Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector "
                                                       "Name>,<Inlet Branch>,<Outlet Branch>,<Loop Name>,<Loop Type>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Plant Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector "
                                                       "Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop Type>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Plant Loop Supply Connection>,<Plant Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Plant Loop Return Connection>,<Plant Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>";
        for (Count = 1; Count <= NumPlantLoops; ++Count) {
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                //  Plant Supply Side Loop
                // Demandside and supplyside is parametrized in DataPlant
                if (LoopSideNum == DemandSide) {
                    LoopString = "Demand";
                } else if (LoopSideNum == SupplySide) {
                    LoopString = "Supply";
                }

                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                    << " Plant Loop," + PlantLoop(Count).Name + ',' + LoopString + ',' + PlantLoop(Count).LoopSide(LoopSideNum).NodeNameIn + ',' +
                           PlantLoop(Count).LoopSide(LoopSideNum).NodeNameOut + ',' + PlantLoop(Count).LoopSide(LoopSideNum).BranchList + ',' +
                           PlantLoop(Count).LoopSide(LoopSideNum).ConnectList;
                //  Plant Supply Side Splitter
                if (PlantLoop(Count).LoopSide(LoopSideNum).SplitterExists) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes;
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "   Plant Loop Connector,Splitter," +
                                                                       PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',' +
                                                                       PlantLoop(Count).Name + ',' + LoopString + ',' + stripped(ChrOut);
                    for (Count1 = 1; Count1 <= PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes; ++Count1) {
                        ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                        ChrOut2 = BlankString;
                        ChrOut3 = BlankString;
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn <= 0) {
                            ChrOut2 = errstring;
                        }
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1) <= 0) {
                            ChrOut3 = errstring;
                        }
                        {
                            IOFlags flags;
                            flags.ADVANCE("No");
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                << "     Plant Loop Connector Branches," + stripped(ChrOut) + ",Splitter," +
                                       PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',';
                        }
                        if (ChrOut2 != errstring) {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                    << PlantLoop(Count)
                                               .LoopSide(LoopSideNum)
                                               .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn)
                                               .Name +
                                           ',';
                            }
                        } else {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags) << ChrOut2 + ',';
                            }
                        }
                        if (ChrOut3 != errstring) {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                                << PlantLoop(Count)
                                           .LoopSide(LoopSideNum)
                                           .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1))
                                           .Name +
                                       ',' + PlantLoop(Count).Name + ',' + LoopString;
                        } else {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << ChrOut3 + ',' + PlantLoop(Count).Name + ',' + LoopString;
                        }
                        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                            << "     Plant Loop Connector Nodes,   " + stripped(ChrOut) + ",Splitter," +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',' +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameIn + ',' +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameOut(Count1) + ',' + PlantLoop(Count).Name + ',' +
                                   LoopString;
                    }
                }

                //  Plant Supply Side Mixer
                if (PlantLoop(Count).LoopSide(LoopSideNum).MixerExists) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes;
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                        << "   Plant Loop Connector,Mixer," + PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',' +
                               PlantLoop(Count).Name + ',' + LoopString + ',' + stripped(ChrOut); //',Supply,'//  &
                    for (Count1 = 1; Count1 <= PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes; ++Count1) {
                        ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                        ChrOut2 = BlankString;
                        ChrOut3 = BlankString;
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1) <= 0) {
                            ChrOut2 = errstring;
                        }
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut <= 0) {
                            ChrOut3 = errstring;
                        }
                        {
                            IOFlags flags;
                            flags.ADVANCE("No");
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                << "     Plant Loop Connector Branches," + stripped(ChrOut) + ",Mixer," +
                                       PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',';
                        }
                        if (ChrOut2 != errstring) {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                    << PlantLoop(Count)
                                               .LoopSide(LoopSideNum)
                                               .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1))
                                               .Name +
                                           ',';
                            }
                        } else {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags) << ChrOut2 + ',';
                            }
                        }
                        if (ChrOut3 != errstring) {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                                << PlantLoop(Count)
                                           .LoopSide(LoopSideNum)
                                           .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut)
                                           .Name +
                                       ',' + PlantLoop(Count).Name + ',' + LoopString;
                        } else {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << ChrOut3 + ',' + PlantLoop(Count).Name + ",Supply";
                        }
                        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "     Plant Loop Connector Nodes,   " + stripped(ChrOut) + ",Mixer," +
                                                                           PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',' +
                                                                           PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameIn(Count1) +
                                                                           ',' + PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameOut +
                                                                           ',' + PlantLoop(Count).Name + ',' + LoopString;
                    }
                }
            }
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << " Plant Loop Supply Connection," + PlantLoop(Count).Name + ',' +
                                                               PlantLoop(Count).LoopSide(SupplySide).NodeNameOut + ',' +
                                                               PlantLoop(Count).LoopSide(DemandSide).NodeNameIn;
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << " Plant Loop Return Connection," + PlantLoop(Count).Name + ',' +
                                                               PlantLoop(Count).LoopSide(DemandSide).NodeNameOut + ',' +
                                                               PlantLoop(Count).LoopSide(SupplySide).NodeNameIn;

        } //  Plant Demand Side Loop

        //  Condenser Loops
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumCondLoops;
        strip(ChrOut);
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <# Condenser Loops>,<Number of Condenser Loops>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_707) << "#Condenser Loops," + ChrOut;
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Condenser Loop>,<Condenser Loop Name>,<Loop Type>,<Inlet Node Name>,<Outlet Node Name>,<Branch List>,<Connector List>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Condenser Loop Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Condenser Loop Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector "
                                                       "Name>,<Inlet Branch>,<Outlet Branch>,<Loop Name>,<Loop Type>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Condenser Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector "
                                                       "Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop Type>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Condenser Loop Supply Connection>,<Condenser Loop Name>,<Supply Side Outlet Node Name>,<Demand Side Inlet Node Name>";
        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
            << "! <Condenser Loop Return Connection>,<Condenser Loop Name>,<Demand Side Outlet Node Name>,<Supply Side Inlet Node Name>";

        for (Count = NumPlantLoops + 1; Count <= TotNumLoops; ++Count) {
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                //  Plant Supply Side Loop
                // Demandside and supplyside is parametrized in DataPlant
                if (LoopSideNum == DemandSide) {
                    LoopString = "Demand";
                } else if (LoopSideNum == SupplySide) {
                    LoopString = "Supply";
                }

                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                    << " Plant Loop," + PlantLoop(Count).Name + ',' + LoopString + ',' + PlantLoop(Count).LoopSide(LoopSideNum).NodeNameIn + ',' +
                           PlantLoop(Count).LoopSide(LoopSideNum).NodeNameOut + ',' + PlantLoop(Count).LoopSide(LoopSideNum).BranchList + ',' +
                           PlantLoop(Count).LoopSide(LoopSideNum).ConnectList;
                //  Plant Supply Side Splitter
                if (PlantLoop(Count).LoopSide(LoopSideNum).SplitterExists) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes;
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "   Plant Loop Connector,Splitter," +
                                                                       PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',' +
                                                                       PlantLoop(Count).Name + ',' + LoopString + ',' + stripped(ChrOut);
                    for (Count1 = 1; Count1 <= PlantLoop(Count).LoopSide(LoopSideNum).Splitter.TotalOutletNodes; ++Count1) {
                        ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                        ChrOut2 = BlankString;
                        ChrOut3 = BlankString;
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn <= 0) {
                            ChrOut2 = errstring;
                        }
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1) <= 0) {
                            ChrOut3 = errstring;
                        }
                        {
                            IOFlags flags;
                            flags.ADVANCE("No");
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                << "     Plant Loop Connector Branches," + stripped(ChrOut) + ",Splitter," +
                                       PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',';
                        }
                        if (ChrOut2 != errstring) {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                    << PlantLoop(Count)
                                               .LoopSide(LoopSideNum)
                                               .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumIn)
                                               .Name +
                                           ',';
                            }
                        } else {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags) << ChrOut2 + ',';
                            }
                        }
                        if (ChrOut3 != errstring) {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                                << PlantLoop(Count)
                                           .LoopSide(LoopSideNum)
                                           .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Splitter.BranchNumOut(Count1))
                                           .Name +
                                       ',' + PlantLoop(Count).Name + ',' + LoopString;
                        } else {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << ChrOut3 + ',' + PlantLoop(Count).Name + ',' + LoopString;
                        }
                        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                            << "     Plant Loop Connector Nodes,   " + stripped(ChrOut) + ",Splitter," +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.Name + ',' +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameIn + ',' +
                                   PlantLoop(Count).LoopSide(LoopSideNum).Splitter.NodeNameOut(Count1) + ',' + PlantLoop(Count).Name + ',' +
                                   LoopString;
                    }
                }

                //  Plant Supply Side Mixer
                if (PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Exists) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes;
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                        << "   Plant Loop Connector,Mixer," + PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',' +
                               PlantLoop(Count).Name + ',' + LoopString + ',' + stripped(ChrOut); //',Supply,'//  &
                    for (Count1 = 1; Count1 <= PlantLoop(Count).LoopSide(LoopSideNum).Mixer.TotalInletNodes; ++Count1) {
                        ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                        ChrOut2 = BlankString;
                        ChrOut3 = BlankString;
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1) <= 0) {
                            ChrOut2 = errstring;
                        }
                        if (PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut <= 0) {
                            ChrOut3 = errstring;
                        }
                        {
                            IOFlags flags;
                            flags.ADVANCE("No");
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                << "     Plant Loop Connector Branches," + stripped(ChrOut) + ",Mixer," +
                                       PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',';
                        }
                        if (ChrOut2 != errstring) {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags)
                                    << PlantLoop(Count)
                                               .LoopSide(LoopSideNum)
                                               .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumIn(Count1))
                                               .Name +
                                           ',';
                            }
                        } else {
                            {
                                IOFlags flags;
                                flags.ADVANCE("No");
                                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713, flags) << ChrOut2 + ',';
                            }
                        }
                        if (ChrOut3 != errstring) {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                                << PlantLoop(Count)
                                           .LoopSide(LoopSideNum)
                                           .Branch(PlantLoop(Count).LoopSide(LoopSideNum).Mixer.BranchNumOut)
                                           .Name +
                                       ',' + PlantLoop(Count).Name + ',' + LoopString;
                        } else {
                            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << ChrOut3 + ',' + PlantLoop(Count).Name + ",Supply";
                        }
                        ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "     Plant Loop Connector Nodes,   " + stripped(ChrOut) + ",Mixer," +
                                                                           PlantLoop(Count).LoopSide(LoopSideNum).Mixer.Name + ',' +
                                                                           PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameIn(Count1) +
                                                                           ',' + PlantLoop(Count).LoopSide(LoopSideNum).Mixer.NodeNameOut +
                                                                           ',' + PlantLoop(Count).Name + ',' + LoopString;
                    }
                }
            }
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << " Plant Loop Supply Connection," + PlantLoop(Count).Name + ',' +
                                                               PlantLoop(Count).LoopSide(SupplySide).NodeNameOut + ',' +
                                                               PlantLoop(Count).LoopSide(DemandSide).NodeNameIn;
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << " Plant Loop Return Connection," + PlantLoop(Count).Name + ',' +
                                                               PlantLoop(Count).LoopSide(DemandSide).NodeNameOut + ',' +
                                                               PlantLoop(Count).LoopSide(SupplySide).NodeNameIn;

        } //  Plant Demand Side Loop

        ObjexxFCL::gio::write(OutputFileBNDetails, Format_701) << "! ===============================================================";
        NumOfControlledZones = 0;
        for (Count = 1; Count <= NumOfZones; ++Count) {
            if (!allocated(ZoneEquipConfig)) continue;
            if (ZoneEquipConfig(Count).IsControlled) ++NumOfControlledZones;
        }
        ObjexxFCL::gio::write(ChrOut, fmtLD) << NumOfControlledZones;
        strip(ChrOut);
        if (NumOfControlledZones > 0) {
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <# Controlled Zones>,<Number of Controlled Zones>";
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_707) << "#Controlled Zones," + ChrOut;
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Controlled Zone>,<Controlled Zone Name>,<Equip List Name>,<Control List Name>,<Zone "
                                                           "Node Name>,<# Inlet Nodes>,<# Exhaust Nodes>,<# Return Nodes>";
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "! <Controlled Zone Inlet>,<Inlet Node Count>,<Controlled Zone Name>,<Supply Air Inlet "
                                                           "Node Name>,<SD Sys:Cooling/Heating [DD:Cooling] Inlet Node Name>,<DD Sys:Heating Inlet "
                                                           "Node Name>";
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                << "! <Controlled Zone Exhaust>,<Exhaust Node Count>,<Controlled Zone Name>,<Exhaust Air Node Name>";
            for (Count = 1; Count <= NumOfZones; ++Count) {
                if (!ZoneEquipConfig(Count).IsControlled) continue;
                ObjexxFCL::gio::write(ChrOut, fmtLD) << ZoneEquipConfig(Count).NumInletNodes;
                ObjexxFCL::gio::write(ChrOut2, fmtLD) << ZoneEquipConfig(Count).NumExhaustNodes;
                ObjexxFCL::gio::write(ChrOut3, fmtLD) << ZoneEquipConfig(Count).NumReturnNodes;
                strip(ChrOut);
                strip(ChrOut2);
                strip(ChrOut3);
                ObjexxFCL::gio::write(OutputFileBNDetails, Format_713)
                    << " Controlled Zone," + ZoneEquipConfig(Count).ZoneName + ',' + ZoneEquipConfig(Count).EquipListName + ',' +
                           ZoneEquipConfig(Count).ControlListName + ',' + NodeID(ZoneEquipConfig(Count).ZoneNode) + ',' + ChrOut + ',' + ChrOut2 +
                           ',' + ChrOut3;
                for (Count1 = 1; Count1 <= ZoneEquipConfig(Count).NumInletNodes; ++Count1) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                    strip(ChrOut);
                    ChrName = NodeID(ZoneEquipConfig(Count).AirDistUnitHeat(Count1).InNode);
                    if (ChrName == "Undefined") ChrName = "N/A";
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "   Controlled Zone Inlet," + ChrOut + ',' + ZoneEquipConfig(Count).ZoneName +
                                                                       ',' + NodeID(ZoneEquipConfig(Count).InletNode(Count1)) + ',' +
                                                                       NodeID(ZoneEquipConfig(Count).AirDistUnitCool(Count1).InNode) + ',' + ChrName;
                }
                for (Count1 = 1; Count1 <= ZoneEquipConfig(Count).NumExhaustNodes; ++Count1) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                    strip(ChrOut);
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "   Controlled Zone Exhaust," + ChrOut + ',' + ZoneEquipConfig(Count).ZoneName +
                                                                       ',' + NodeID(ZoneEquipConfig(Count).ExhaustNode(Count1));
                }
                for (Count1 = 1; Count1 <= ZoneEquipConfig(Count).NumReturnNodes; ++Count1) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                    strip(ChrOut);
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_713) << "   Controlled Zone Return," + ChrOut + ',' + ZoneEquipConfig(Count).ZoneName +
                                                                       ',' + NodeID(ZoneEquipConfig(Count).ReturnNode(Count1));
                }
            }

            // Report Zone Equipment Lists to BND File
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_721) << "! ===============================================================";
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_720);
            ObjexxFCL::gio::write(ChrOut, fmtLD) << NumOfControlledZones;
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_721) << " #Zone Equipment Lists," + stripped(ChrOut);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_722);
            ObjexxFCL::gio::write(OutputFileBNDetails, Format_723);

            for (Count = 1; Count <= NumOfZones; ++Count) {
                // Zone equipment list array parallels controlled zone equipment array, so
                // same index finds corresponding data from both arrays
                if (!ZoneEquipConfig(Count).IsControlled) continue;
                ObjexxFCL::gio::write(ChrOut, fmtLD) << Count;
                ObjexxFCL::gio::write(ChrOut2, fmtLD) << ZoneEquipList(Count).NumOfEquipTypes;
                ObjexxFCL::gio::write(OutputFileBNDetails, Format_721) << " Zone Equipment List," + stripped(ChrOut) + ',' + ZoneEquipList(Count).Name + ',' +
                                                                   ZoneEquipConfig(Count).ZoneName + ',' + stripped(ChrOut2);

                for (Count1 = 1; Count1 <= ZoneEquipList(Count).NumOfEquipTypes; ++Count1) {
                    ObjexxFCL::gio::write(ChrOut, fmtLD) << Count1;
                    ObjexxFCL::gio::write(ChrOut2, fmtLD) << ZoneEquipList(Count).CoolingPriority(Count1);
                    ObjexxFCL::gio::write(ChrOut3, fmtLD) << ZoneEquipList(Count).HeatingPriority(Count1);
                    ObjexxFCL::gio::write(OutputFileBNDetails, Format_721)
                        << "   Zone Equipment Component," + stripped(ChrOut) + ',' + ZoneEquipList(Count).EquipType(Count1) + ',' +
                               ZoneEquipList(Count).EquipName(Count1) + ',' + ZoneEquipConfig(Count).ZoneName + ',' + stripped(ChrOut2) + ',' +
                               stripped(ChrOut3);
                }
            }
        }

        // Report Dual Duct Dampers to BND File
        ReportDualDuctConnections();

        if (NumNodeConnectionErrors == 0) {
            ShowMessage("No node connection errors were found.");
        } else {
            ObjexxFCL::gio::write(ChrOut, fmtLD) << NumNodeConnectionErrors;
            strip(ChrOut);
            if (NumNodeConnectionErrors > 1) {
                ShowMessage("There were " + ChrOut + " node connection errors noted.");
            } else {
                ShowMessage("There was " + ChrOut + " node connection error noted.");
            }
        }

        AskForConnectionsReport = false;
    }

    void ReportParentChildren()
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
        ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "Node Type,CompSet Name,Inlet Node,OutletNode";
        for (Loop = 1; Loop <= NumOfActualParents; ++Loop) {
            NumChildren = GetNumChildren(ParentNodeList(Loop).CType, ParentNodeList(Loop).CName);
            if (NumChildren > 0) {
                ChildCType.allocate(NumChildren);
                ChildCName.allocate(NumChildren);
                ChildInNodeName.allocate(NumChildren);
                ChildOutNodeName.allocate(NumChildren);
                ChildInNodeNum.allocate(NumChildren);
                ChildOutNodeNum.allocate(NumChildren);
                ChildCType = BlankString;
                ChildCName = BlankString;
                ChildInNodeName = BlankString;
                ChildOutNodeName = BlankString;
                ChildInNodeNum = 0;
                ChildOutNodeNum = 0;
                GetChildrenData(ParentNodeList(Loop).CType,
                                ParentNodeList(Loop).CName,
                                NumChildren,
                                ChildCType,
                                ChildCName,
                                ChildInNodeName,
                                ChildInNodeNum,
                                ChildOutNodeName,
                                ChildOutNodeNum,
                                ErrorsFound);
                if (Loop > 1) ObjexxFCL::gio::write(OutputFileDebug, "(1X,60('='))");
                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << " Parent Node," + ParentNodeList(Loop).CType + ':' + ParentNodeList(Loop).CName + ',' +
                                                         ParentNodeList(Loop).InletNodeName + ',' + ParentNodeList(Loop).OutletNodeName;
                for (Loop1 = 1; Loop1 <= NumChildren; ++Loop1) {
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "..ChildNode," + ChildCType(Loop1) + ':' + ChildCName(Loop1) + ',' + ChildInNodeName(Loop1) +
                                                             ',' + ChildOutNodeName(Loop1);
                }
                ChildCType.deallocate();
                ChildCName.deallocate();
                ChildInNodeName.deallocate();
                ChildOutNodeName.deallocate();
                ChildInNodeNum.deallocate();
                ChildOutNodeNum.deallocate();
            } else {
                if (Loop > 1) ObjexxFCL::gio::write(OutputFileDebug, "(1X,60('='))");
                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << " Parent Node (no children)," + ParentNodeList(Loop).CType + ':' + ParentNodeList(Loop).CName +
                                                         ',' + ParentNodeList(Loop).InletNodeName + ',' + ParentNodeList(Loop).OutletNodeName;
            }
        }
    }

    void ReportCompSetMeterVariables()
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
        Array1D<OutputProcessor::Unit> unitsForVar; // units from enum for each variable
        Array1D_string VarNames;
        Array1D_int ResourceTypes;
        Array1D_string EndUses;
        Array1D_string Groups;

        ObjexxFCL::gio::write(OutputFileDebug, fmtA) << " CompSet,ComponentType,ComponentName,NumMeteredVariables";
        ObjexxFCL::gio::write(OutputFileDebug, fmtA) << " RepVar,ReportIndex,ReportID,ReportName,Units,ResourceType,EndUse,Group,IndexType";

        for (Loop = 1; Loop <= NumCompSets; ++Loop) {
            NumVariables = GetNumMeteredVariables(CompSets(Loop).CType, CompSets(Loop).CName);
            ObjexxFCL::gio::write(OutputFileDebug, "(1X,'CompSet,',A,',',A,',',I5)") << CompSets(Loop).CType << CompSets(Loop).CName << NumVariables;
            if (NumVariables <= 0) continue;
            VarIndexes.dimension(NumVariables, 0);
            VarIDs.dimension(NumVariables, 0);
            IndexTypes.dimension(NumVariables, 0);
            VarTypes.dimension(NumVariables, 0);
            VarNames.allocate(NumVariables);
            unitsForVar.allocate(NumVariables);
            ResourceTypes.dimension(NumVariables, 0);
            EndUses.allocate(NumVariables);
            Groups.allocate(NumVariables);
            GetMeteredVariables(CompSets(Loop).CType,
                                CompSets(Loop).CName,
                                VarIndexes,
                                VarTypes,
                                IndexTypes,
                                unitsForVar,
                                ResourceTypes,
                                EndUses,
                                Groups,
                                VarNames,
                                _,
                                VarIDs);
            for (Loop1 = 1; Loop1 <= NumVariables; ++Loop1) {
                ObjexxFCL::gio::write(OutputFileDebug, "(1X,'RepVar,',I5,',',I5,',',A,',[',A,'],',A,',',A,',',A,',',I5)")
                    << VarIndexes(Loop1) << VarIDs(Loop1) << VarNames(Loop1) << unitEnumToString(unitsForVar(Loop1))
                    << GetResourceTypeChar(ResourceTypes(Loop1)) << EndUses(Loop1) << Groups(Loop1) << IndexTypes(Loop1);
            }
            VarIndexes.deallocate();
            IndexTypes.deallocate();
            VarTypes.deallocate();
            VarIDs.deallocate();
            VarNames.deallocate();
            unitsForVar.deallocate();
            ResourceTypes.deallocate();
            EndUses.deallocate();
            Groups.deallocate();
        }
    }

    void PostIPProcessing()
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
        using FluidProperties::FluidIndex_EthyleneGlycol;
        using FluidProperties::FluidIndex_PropoleneGlycol;
        using FluidProperties::FluidIndex_Water;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool PreP_Fatal( false ); // True if a preprocessor flags a fatal error
        ////////////////////////////////////////////////////////////////////////////////////

        DoingInputProcessing = false;

        inputProcessor->preProcessorCheck(PreP_Fatal); // Check Preprocessor objects for warning, severe, etc errors.

        if (PreP_Fatal) {
            ShowFatalError("Preprocessor condition(s) cause termination.");
        }

        // Set up more globals - process fluid input.
        FluidIndex_Water = FindGlycol("Water");
        FluidIndex_EthyleneGlycol = FindGlycol("EthyleneGlycol");
        FluidIndex_PropoleneGlycol = FindGlycol("PropoleneGlycol");

        inputProcessor->preScanReportingVariables();
    }

} // namespace SimulationManager

// EXTERNAL SUBROUTINES:

void Resimulate(bool &ResimExt, // Flag to resimulate the exterior energy use simulation
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
    using namespace DataPrecisionGlobals;
    using DataHeatBalFanSys::iGetZoneSetPoints;
    using DataHeatBalFanSys::iPredictStep;
    using DemandManager::DemandManagerExtIterations;
    using DemandManager::DemandManagerHBIterations;
    using DemandManager::DemandManagerHVACIterations;
    using ExteriorEnergyUse::ManageExteriorEnergyUse;
    using HeatBalanceAirManager::InitAirHeatBalance;
    using HeatBalanceSurfaceManager::InitSurfaceHeatBalance;
    using HVACManager::SimHVAC;
    using RefrigeratedCase::ManageRefrigeratedCaseRacks;
    using ZoneTempPredictorCorrector::ManageZoneAirUpdates;
    // using HVACManager::CalcAirFlowSimple;
    using DataContaminantBalance::Contaminant;
    using DataHeatBalance::ZoneAirMassFlow;
    using DataHVACGlobals::UseZoneTimeStepHistory; // , InitDSwithZoneHistory
    using ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates;
    using namespace ZoneEquipmentManager;
    // using ZoneEquipmentManager::CalcAirFlowSimple;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneTempChange(0.0); // Dummy variable needed for calling ManageZoneAirUpdates

    // FLOW:
    if (ResimExt) {
        ManageExteriorEnergyUse();

        ++DemandManagerExtIterations;
    }

    if (ResimHB) {
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

    if (ResimHVAC) {
        // HVAC simulation
        ManageZoneAirUpdates(iGetZoneSetPoints, ZoneTempChange, false, UseZoneTimeStepHistory, 0.0);
        if (Contaminant.SimulateContaminants) ManageZoneContaminanUpdates(iGetZoneSetPoints, false, UseZoneTimeStepHistory, 0.0);
        CalcAirFlowSimple(0, ZoneAirMassFlow.EnforceZoneMassBalance);
        ManageZoneAirUpdates(iPredictStep, ZoneTempChange, false, UseZoneTimeStepHistory, 0.0);
        if (Contaminant.SimulateContaminants) ManageZoneContaminanUpdates(iPredictStep, false, UseZoneTimeStepHistory, 0.0);
        SimHVAC();

        ++DemandManagerHVACIterations;
    }
}

} // namespace EnergyPlus
