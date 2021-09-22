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

// C++ Headers
#include <cmath>
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACCooledBeam.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>

namespace EnergyPlus::SizingManager {

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   December 2000
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module contains the data and routines relating to managing the sizing
// simulations.

// Using/Aliasing
using namespace HeatBalanceManager;
using namespace WeatherManager;
using namespace DataSizing;
using DataStringGlobals::CharComma;
using DataStringGlobals::CharSpace;
using DataStringGlobals::CharTab;

void ManageSizing(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the sizing simulations (using design day condiions)
    // for zones, central air systems, and central plants and zone heating and cooling

    // METHODOLOGY EMPLOYED:
    // Design day simulations are run with the zones supplied with "Ideal Loads",
    // yielding zone design supply air flow rates and zone heating and cooling capacities.
    // Design day simulations are run again with central air systems supplied by
    // purchased hot and cold water, yielding central heating and cooling capacities.

    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &SysSizPeakDDNum(state.dataSize->SysSizPeakDDNum);

    // Using/Aliasing
    using SimAirServingZones::ManageAirLoops;
    using SimAirServingZones::UpdateSysSizing;
    using ZoneEquipmentManager::ManageZoneEquipment;
    using ZoneEquipmentManager::RezeroZoneSizingArrays;
    using ZoneEquipmentManager::UpdateZoneSizing;
    using namespace OutputReportPredefined;

    using OutputReportTabular::AllocateLoadComponentArrays;
    using OutputReportTabular::ComputeLoadComponentDecayCurve;
    using OutputReportTabular::DeallocateLoadComponentArrays;
    using OutputReportTabular::hasSizingPeriodsDays;
    using OutputReportTabular::isCompLoadRepReq;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("ManageSizing: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool Available(false); // an environment is available to process
    bool ErrorsFound(false);
    bool SimAir(false);
    bool SimZoneEquip(false);
    int TimeStepInDay(0); // time step number
    int LastMonth(0);
    int LastDayOfMonth(0);
    int CtrlZoneNum(0);       // controlled zone index
    int ZoneNum(0);           // index into the Zone data array for the controlled zone
    Real64 TempAtPeak(0.0);   // Outside temperature at peak cooling/heating for reporting
    Real64 HumRatAtPeak(0.0); // Outside humidity ratio at peak cooling/heating for reporting
    int TimeStepAtPeak(0);    // time step number at heat or cool peak
    int DDNum(0);             // Design Day index
    int AirLoopNum(0);        // air loop index
    //  EXTERNAL            ReportZoneSizing
    //  EXTERNAL            ReportSysSizing
    std::string curName;
    int NumSizingPeriodsPerformed;
    int numZoneSizeIter; // number of times to repeat zone sizing calcs. 1 normal, 2 load component reporting
    int iZoneCalcIter;   // index for repeating the zone sizing calcs
    bool isUserReqCompLoadReport;
    Real64 DOASHeatGainRateAtHtPk(0.0); // zone heat gain rate from the DOAS at the heating peak [W]
    Real64 DOASHeatGainRateAtClPk(0.0); // zone heat gain rate from the DOAS at the cooling peak [W]
    Real64 TStatSetPtAtPk(0.0);         // thermostat set point at peak

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);

    TimeStepInDay = 0;
    state.dataSize->SysSizingRunDone = false;
    state.dataSize->ZoneSizingRunDone = false;
    curName = "Unknown";
    GetOARequirements(state);      // get the OA requirements object
    GetZoneAirDistribution(state); // get zone air distribution objects
    GetZoneHVACSizing(state);      // get zone HVAC sizing object
    GetAirTerminalSizing(state);   // get air terminal sizing object
    GetSizingParams(state);        // get the building level sizing paramets
    GetZoneSizingInput(state);     // get the Zone Sizing input
    GetSystemSizingInput(state);   // get the System Sizing input
    GetPlantSizingInput(state);    // get the Plant Sizing input

    // okay, check sizing inputs vs desires vs requirements
    if (state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing) {
        if ((state.dataSize->NumSysSizInput > 0 && state.dataSize->NumZoneSizingInput == 0) ||
            (!state.dataGlobal->DoZoneSizing && state.dataGlobal->DoSystemSizing && state.dataSize->NumSysSizInput > 0)) {
            ShowSevereError(state, std::string{RoutineName} + "Requested System Sizing but did not request Zone Sizing.");
            ShowContinueError(state, "System Sizing cannot be done without Zone Sizing");
            ShowFatalError(state, "Program terminates for preceding conditions.");
        }
    }

    // determine if the second set of zone sizing calculations should be performed
    // that include a pulse for the load component reporting
    isUserReqCompLoadReport = isCompLoadRepReq(state); // check getinput structure if load component report is requested
    bool fileHasSizingPeriodDays =
        hasSizingPeriodsDays(state); // check getinput if SizingPeriod:DesignDays or SizingPeriod:WeatherFileDays are present
    if (state.dataGlobal->DoZoneSizing && (state.dataSize->NumZoneSizingInput > 0) && fileHasSizingPeriodDays) {
        state.dataGlobal->CompLoadReportIsReq = isUserReqCompLoadReport;
    } else { // produce a warning if the user asked for the report but it will not be generated because sizing is not done
        if (isUserReqCompLoadReport) {
            if (fileHasSizingPeriodDays) {
                ShowWarningError(
                    state,
                    std::string{RoutineName} +
                        "The ZoneComponentLoadSummary report was requested but no sizing objects were found so that report cannot be generated.");
            } else {
                ShowWarningError(state,
                                 std::string{RoutineName} + "The ZoneComponentLoadSummary report was requested but no SizingPeriod:DesignDay or "
                                                            "SizingPeriod:WeatherFileDays objects were found so that report cannot be generated.");
            }
        }
    }
    if (state.dataGlobal->CompLoadReportIsReq) { // if that report is created then zone sizing calculations are repeated
        numZoneSizeIter = 2;
    } else {
        numZoneSizeIter = 1;
    }

    if ((state.dataGlobal->DoZoneSizing) && (state.dataSize->NumZoneSizingInput == 0)) {
        ShowWarningError(
            state,
            std::string{RoutineName} +
                "For a zone sizing run, there must be at least 1 Sizing:Zone input object. SimulationControl Zone Sizing option ignored.");
    }

    if ((state.dataSize->NumZoneSizingInput > 0) &&
        (state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing)) {

        if (state.dataGlobal->DoDesDaySim || state.dataGlobal->DoWeathSim) {
            state.dataGlobal->DoOutputReporting = false;
        }
        state.dataGlobal->DoOutputReporting = false;
        state.dataGlobal->ZoneSizingCalc = true;
        Available = true;

        if (state.dataSize->SizingFileColSep == CharComma) {
            state.files.zsz.filePath = state.files.outputZszCsvFilePath;
        } else if (state.dataSize->SizingFileColSep == CharTab) {
            state.files.zsz.filePath = state.files.outputZszTabFilePath;
        } else {
            state.files.zsz.filePath = state.files.outputZszTxtFilePath;
        }

        state.files.zsz.ensure_open(state, "ManageSizing", state.files.outputControl.zsz);

        ShowMessage(state, "Beginning Zone Sizing Calculations");

        ResetEnvironmentCounter(state);
        state.dataGlobal->KickOffSizing = true;
        SetupZoneSizing(state, ErrorsFound); // Should only be done ONCE
        state.dataGlobal->KickOffSizing = false;

        for (iZoneCalcIter = 1; iZoneCalcIter <= numZoneSizeIter; ++iZoneCalcIter) { // normally this is performed once but if load component
            // report is requested, these are repeated with a pulse in
            // each zone.

            // set flag if performing a "pulse" set of sizing calcs
            // the pulse simulation needs to be done first (the 1 in the following line) otherwise
            // the difference seen in the loads in the epluspls and epluszsz files are not
            // simple decreasing curves but appear as amost random fluctuations.
            state.dataGlobal->isPulseZoneSizing = (state.dataGlobal->CompLoadReportIsReq && (iZoneCalcIter == 1));

            Available = true;

            ResetEnvironmentCounter(state);
            state.dataSize->CurOverallSimDay = 0;
            NumSizingPeriodsPerformed = 0;
            while (Available) { // loop over environments

                GetNextEnvironment(state, Available, ErrorsFound); // get an environment

                if (!Available) break;
                if (ErrorsFound) break;

                // check that environment is one of the design days
                if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                    continue;
                }

                ++NumSizingPeriodsPerformed;

                state.dataGlobal->BeginEnvrnFlag = true;
                if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay) &&
                    (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                         .suppressBegEnvReset)) {
                    // user has input in SizingPeriod:DesignDay directing to skip begin environment rests, for accuracy-with-speed as zones can
                    // more easily converge fewer warmup days are allowed
                    DisplayString(state, "Suppressing Initialization of New Environment Parameters");
                    state.dataGlobal->beginEnvrnWarmStartFlag = true;
                } else {
                    state.dataGlobal->beginEnvrnWarmStartFlag = false;
                }
                state.dataGlobal->EndEnvrnFlag = false;
                state.dataEnvrn->EndMonthFlag = false;
                state.dataGlobal->WarmupFlag = true;
                state.dataGlobal->DayOfSim = 0;
                state.dataGlobal->DayOfSimChr = "0";
                state.dataSize->CurEnvirNumSimDay = 1;
                ++state.dataSize->CurOverallSimDay;
                while ((state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn) || (state.dataGlobal->WarmupFlag)) { // Begin day loop ...

                    ++state.dataGlobal->DayOfSim;
                    if (!state.dataGlobal->WarmupFlag && state.dataGlobal->DayOfSim > 1) {
                        ++state.dataSize->CurEnvirNumSimDay;
                    }

                    state.dataGlobal->DayOfSimChr = fmt::to_string(state.dataGlobal->DayOfSim);
                    state.dataGlobal->BeginDayFlag = true;
                    state.dataGlobal->EndDayFlag = false;

                    if (state.dataGlobal->WarmupFlag) {
                        DisplayString(state, "Warming up");
                    } else { // (.NOT.WarmupFlag)
                        if (state.dataGlobal->DayOfSim == 1) {
                            if (!state.dataGlobal->isPulseZoneSizing) {
                                DisplayString(state, "Performing Zone Sizing Simulation");
                            } else {
                                DisplayString(state, "Performing Zone Sizing Simulation for Load Component Report");
                            }
                            DisplayString(state,
                                          fmt::format("...for Sizing Period: #{} {}", NumSizingPeriodsPerformed, state.dataEnvrn->EnvironmentName));
                        }
                        UpdateZoneSizing(state, DataGlobalConstants::CallIndicator::BeginDay);
                        UpdateFacilitySizing(state, DataGlobalConstants::CallIndicator::BeginDay);
                    }

                    for (state.dataGlobal->HourOfDay = 1; state.dataGlobal->HourOfDay <= 24; ++state.dataGlobal->HourOfDay) { // Begin hour loop ...

                        state.dataGlobal->BeginHourFlag = true;
                        state.dataGlobal->EndHourFlag = false;

                        for (state.dataGlobal->TimeStep = 1; state.dataGlobal->TimeStep <= state.dataGlobal->NumOfTimeStepInHour;
                             ++state.dataGlobal->TimeStep) { // Begin time step (TINC) loop ...

                            state.dataGlobal->BeginTimeStepFlag = true;

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

                            // set flag for pulse used in load component reporting
                            state.dataGlobal->doLoadComponentPulseNow = CalcdoLoadComponentPulseNow(state,
                                                                                                    state.dataGlobal->isPulseZoneSizing,
                                                                                                    state.dataGlobal->WarmupFlag,
                                                                                                    state.dataGlobal->HourOfDay,
                                                                                                    state.dataGlobal->TimeStep,
                                                                                                    state.dataGlobal->KindOfSim);

                            ManageWeather(state);

                            if (!state.dataGlobal->WarmupFlag) {
                                TimeStepInDay =
                                    (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                                if (state.dataGlobal->HourOfDay == 1 && state.dataGlobal->TimeStep == 1) {
                                    state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString =
                                        fmt::format("{}/{}", state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth);
                                }
                                state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).Temp(TimeStepInDay) = state.dataEnvrn->OutDryBulbTemp;
                                state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).HumRat(TimeStepInDay) = state.dataEnvrn->OutHumRat;
                                state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).Press(TimeStepInDay) = state.dataEnvrn->OutBaroPress;
                            }

                            ManageHeatBalance(state);

                            state.dataGlobal->BeginHourFlag = false;
                            state.dataGlobal->BeginDayFlag = false;
                            state.dataGlobal->BeginEnvrnFlag = false;
                            state.dataGlobal->BeginSimFlag = false;

                        } // ... End time step (TINC) loop.

                        state.dataGlobal->PreviousHour = state.dataGlobal->HourOfDay;

                    } // ... End hour loop.

                    if (state.dataGlobal->EndDayFlag) {
                        UpdateZoneSizing(state, DataGlobalConstants::CallIndicator::EndDay);
                        UpdateFacilitySizing(state, DataGlobalConstants::CallIndicator::EndDay);
                    }

                    if (!state.dataGlobal->WarmupFlag && (state.dataGlobal->DayOfSim > 0) &&
                        (state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn)) {
                        ++state.dataSize->CurOverallSimDay;
                    }

                } // ... End day loop.

                LastMonth = state.dataEnvrn->Month;
                LastDayOfMonth = state.dataEnvrn->DayOfMonth;

            } // ... End environment loop

            if (NumSizingPeriodsPerformed > 0) {
                UpdateZoneSizing(state, DataGlobalConstants::CallIndicator::EndZoneSizingCalc);
                UpdateFacilitySizing(state, DataGlobalConstants::CallIndicator::EndZoneSizingCalc);
                state.dataSize->ZoneSizingRunDone = true;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + "No Sizing periods were performed for Zone Sizing. No Zone Sizing calculations saved.");
                ErrorsFound = true;
            }

            if (state.dataGlobal->isPulseZoneSizing && state.dataSizingManager->runZeroingOnce) {
                RezeroZoneSizingArrays(state); // zero all arrays related to zone sizing.
                state.dataSizingManager->runZeroingOnce = false;
            }
        } // loop that repeats the zone sizing calcs for the load component report, if requested

        // both the pulse and normal zone sizing is complete so now post processing of the results is performed
        if (state.dataGlobal->CompLoadReportIsReq) {
            // call the routine that computes the decay curve
            ComputeLoadComponentDecayCurve(state);
            // remove some of the arrays used to derive the decay curves
            DeallocateLoadComponentArrays(state);
        }
    }

    state.dataGlobal->ZoneSizingCalc = false;
    state.dataGlobal->DoOutputReporting = false;
    state.dataEnvrn->Month = LastMonth;
    state.dataEnvrn->DayOfMonth = LastDayOfMonth;

    if ((state.dataGlobal->DoSystemSizing) && (state.dataSize->NumSysSizInput == 0) && (state.dataSizingManager->NumAirLoops > 0)) {
        ShowWarningError(
            state,
            std::string{RoutineName} +
                "For a system sizing run, there must be at least 1 Sizing:System object input. SimulationControl System Sizing option ignored.");
    }

    if ((state.dataSize->NumSysSizInput > 0) && (state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing) && !ErrorsFound) {

        ShowMessage(state, "Beginning System Sizing Calculations");

        state.dataGlobal->SysSizingCalc = true;
        Available = true;
        if (state.dataSize->SizingFileColSep == CharComma) {
            state.files.ssz.filePath = state.files.outputSszCsvFilePath;
        } else if (state.dataSize->SizingFileColSep == CharTab) {
            state.files.ssz.filePath = state.files.outputSszTabFilePath;
        } else {
            state.files.ssz.filePath = state.files.outputSszTxtFilePath;
        }
        state.files.ssz.ensure_open(state, "ManageSizing", state.files.outputControl.ssz);

        SimAir = true;
        SimZoneEquip = true;

        ManageZoneEquipment(state, true, SimZoneEquip, SimAir);
        ManageAirLoops(state, true, SimAir, SimZoneEquip);
        SizingManager::UpdateTermUnitFinalZoneSizing(state); // AirDistUnits have been loaded now so TermUnitSizing values are all in place
        SimAirServingZones::SizeSysOutdoorAir(state);        // System OA can be sized now that TermUnitFinalZoneSizing is initialized
        ResetEnvironmentCounter(state);
        state.dataSize->CurEnvirNumSimDay = 0;
        state.dataSize->CurOverallSimDay = 0;
        NumSizingPeriodsPerformed = 0;
        while (Available) { // loop over environments

            GetNextEnvironment(state, Available, ErrorsFound); // get an environment

            // check that environment is one of the design days
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                continue;
            }

            if (!Available) break;
            if (ErrorsFound) break;

            ++NumSizingPeriodsPerformed;

            state.dataGlobal->BeginEnvrnFlag = true;
            if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay) &&
                (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                     .suppressBegEnvReset)) {
                // user has input in SizingPeriod:DesignDay directing to skip begin environment rests, for accuracy-with-speed as zones can more
                // easily converge fewer warmup days are allowed
                DisplayString(state, "Suppressing Initialization of New Environment Parameters");
                state.dataGlobal->beginEnvrnWarmStartFlag = true;
            } else {
                state.dataGlobal->beginEnvrnWarmStartFlag = false;
            }
            state.dataGlobal->EndEnvrnFlag = false;
            state.dataGlobal->WarmupFlag = false;
            state.dataGlobal->DayOfSim = 0;
            state.dataGlobal->DayOfSimChr = "0";
            state.dataSize->CurEnvirNumSimDay = 1;
            ++state.dataSize->CurOverallSimDay;

            while ((state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn) || (state.dataGlobal->WarmupFlag)) { // Begin day loop ...

                ++state.dataGlobal->DayOfSim;
                if (!state.dataGlobal->WarmupFlag && state.dataGlobal->DayOfSim > 1) {
                    ++state.dataSize->CurEnvirNumSimDay;
                }
                state.dataGlobal->DayOfSimChr = fmt::to_string(state.dataGlobal->DayOfSim);
                state.dataGlobal->BeginDayFlag = true;
                state.dataGlobal->EndDayFlag = false;

                if (state.dataGlobal->WarmupFlag) {
                    DisplayString(state, "Warming up");
                } else { // (.NOT.WarmupFlag)
                    if (state.dataGlobal->DayOfSim == 1) {
                        DisplayString(state, "Calculating System sizing");
                        DisplayString(state,
                                      fmt::format("...for Sizing Period: #{} {}", NumSizingPeriodsPerformed, state.dataEnvrn->EnvironmentName));
                    }
                    UpdateSysSizing(state, DataGlobalConstants::CallIndicator::BeginDay);
                }

                for (state.dataGlobal->HourOfDay = 1; state.dataGlobal->HourOfDay <= 24; ++state.dataGlobal->HourOfDay) { // Begin hour loop ...

                    state.dataGlobal->BeginHourFlag = true;
                    state.dataGlobal->EndHourFlag = false;

                    for (state.dataGlobal->TimeStep = 1; state.dataGlobal->TimeStep <= state.dataGlobal->NumOfTimeStepInHour;
                         ++state.dataGlobal->TimeStep) { // Begin time step (TINC) loop ...

                        state.dataGlobal->BeginTimeStepFlag = true;

                        // Set the End__Flag variables to true if necessary.  Note that
                        // each flag builds on the previous level.  EndDayFlag cannot be
                        // .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
                        // EndEnvrnFlag and the EndSimFlag cannot be set during warmup.

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

                        UpdateSysSizing(state, DataGlobalConstants::CallIndicator::DuringDay);

                        state.dataGlobal->BeginHourFlag = false;
                        state.dataGlobal->BeginDayFlag = false;
                        state.dataGlobal->BeginEnvrnFlag = false;

                    } // ... End time step (TINC) loop.

                    state.dataGlobal->PreviousHour = state.dataGlobal->HourOfDay;

                } // ... End hour loop.

                if (state.dataGlobal->EndDayFlag) UpdateSysSizing(state, DataGlobalConstants::CallIndicator::EndDay);

                if (!state.dataGlobal->WarmupFlag && (state.dataGlobal->DayOfSim > 0) &&
                    (state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn)) {
                    ++state.dataSize->CurOverallSimDay;
                }

            } // ... End day loop.

        } // ... End environment loop

        if (NumSizingPeriodsPerformed > 0) {
            UpdateSysSizing(state, DataGlobalConstants::CallIndicator::EndSysSizingCalc);
            state.dataSize->SysSizingRunDone = true;
        } else {
            ShowSevereError(state,
                            std::string{RoutineName} + "No Sizing periods were performed for System Sizing. No System Sizing calculations saved.");
            ErrorsFound = true;
        }
    } else if ((state.dataSize->NumZoneSizingInput > 0) &&
               (state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing)) {
        // If zone sizing but no system sizing - still need to set up system zone equipment and transfer zone sizing data to
        // TermUnitFinalZoneSizing
        state.dataGlobal->SysSizingCalc = true; // set true here so equipment does not try to size yet
        SimAir = true;
        SimZoneEquip = true;

        ManageZoneEquipment(state, true, SimZoneEquip, SimAir);
        SizingManager::UpdateTermUnitFinalZoneSizing(state); // AirDistUnits have been loaded now so TermUnitSizing values are all in place
    }
    state.dataGlobal->SysSizingCalc = false;

    // report sizing results to eio file
    if (state.dataSize->ZoneSizingRunDone) {
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            ZoneNum = state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum;
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow > 0.0) {
                TimeStepAtPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax;
                DDNum = state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDDNum;
                if (DDNum > 0 && TimeStepAtPeak > 0) {
                    TempAtPeak = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak);
                    HumRatAtPeak = state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak);
                    DOASHeatGainRateAtClPk = state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DOASHeatAddSeq(TimeStepAtPeak);
                    TStatSetPtAtPk = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolTstatTempSeq(TimeStepAtPeak);
                } else {
                    TempAtPeak = 0.0;
                    HumRatAtPeak = 0.0;
                    DOASHeatGainRateAtClPk = 0.0;
                    TStatSetPtAtPk = 0.0;
                }
                ReportZoneSizing(state,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                 "Cooling",
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesDay,
                                 state.dataSize->CoolPeakDateHrMin(CtrlZoneNum),
                                 TempAtPeak,
                                 HumRatAtPeak,
                                 state.dataHeatBal->Zone(ZoneNum).FloorArea,
                                 state.dataHeatBal->Zone(ZoneNum).TotOccupants,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA,
                                 DOASHeatGainRateAtClPk);
                curName = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName;
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnClCalcDesLd, curName, state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnClUserDesLd, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad);
                if (state.dataHeatBal->Zone(ZoneNum).FloorArea != 0.0) {
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchZnClUserDesLdPerArea,
                                     curName,
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad / state.dataHeatBal->Zone(ZoneNum).FloorArea);
                }
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnClCalcDesAirFlow,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow,
                                 3);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnClUserDesAirFlow,
                                 curName,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow,
                                 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClDesDay, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesDay);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkTime, curName, state.dataSize->CoolPeakDateHrMin(CtrlZoneNum));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkTstatTemp, curName, TStatSetPtAtPk);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnClPkIndTemp,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnClPkIndHum,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak,
                                 5);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkOATemp, curName, TempAtPeak);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkOAHum, curName, HumRatAtPeak, 5);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnClPkOAMinFlow, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkDOASHeatGain, curName, DOASHeatGainRateAtClPk);
            } else {
                curName = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClCalcDesLd, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClUserDesLd, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClUserDesLdPerArea, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClCalcDesAirFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClUserDesAirFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClDesDay, curName, "N/A");
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkTime, curName, "N/A");
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkTstatTemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkIndTemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkIndHum, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkOATemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkOAHum, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkOAMinFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnClPkDOASHeatGain, curName, 0.0, 1);
            }
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow > 0.0) {
                TimeStepAtPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax;
                DDNum = state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDDNum;
                if (DDNum > 0 && TimeStepAtPeak > 0) {
                    TempAtPeak = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak);
                    HumRatAtPeak = state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak);
                    DOASHeatGainRateAtHtPk = state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DOASHeatAddSeq(TimeStepAtPeak);
                    TStatSetPtAtPk = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatTstatTempSeq(TimeStepAtPeak);
                } else {
                    TempAtPeak = 0.0;
                    HumRatAtPeak = 0.0;
                    DOASHeatGainRateAtHtPk = 0.0;
                    TStatSetPtAtPk = 0.0;
                }
                ReportZoneSizing(state,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                 "Heating",
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesDay,
                                 state.dataSize->HeatPeakDateHrMin(CtrlZoneNum),
                                 TempAtPeak,
                                 HumRatAtPeak,
                                 state.dataHeatBal->Zone(ZoneNum).FloorArea,
                                 state.dataHeatBal->Zone(ZoneNum).TotOccupants,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA,
                                 DOASHeatGainRateAtHtPk);
                curName = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName;
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnHtCalcDesLd, curName, state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnHtUserDesLd, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad);
                if (state.dataHeatBal->Zone(ZoneNum).FloorArea != 0.0) {
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchZnHtUserDesLdPerArea,
                                     curName,
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad / state.dataHeatBal->Zone(ZoneNum).FloorArea);
                }
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnHtCalcDesAirFlow,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow,
                                 3);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnHtUserDesAirFlow,
                                 curName,
                                 state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow,
                                 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtDesDay, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesDay);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkTime, curName, state.dataSize->HeatPeakDateHrMin(CtrlZoneNum));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkTstatTemp, curName, TStatSetPtAtPk);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnHtPkIndTemp,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchZnHtPkIndHum,
                                 curName,
                                 state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak,
                                 5);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkOATemp, curName, TempAtPeak);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkOAHum, curName, HumRatAtPeak, 5);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchZnHtPkOAMinFlow, curName, state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkDOASHeatGain, curName, DOASHeatGainRateAtHtPk);
            } else {
                curName = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtCalcDesLd, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtUserDesLd, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtUserDesLdPerArea, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtCalcDesAirFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtUserDesAirFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtDesDay, curName, "N/A");
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkTime, curName, "N/A");
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkTstatTemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkIndTemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkIndHum, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkOATemp, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkOAHum, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkOAMinFlow, curName, 0.0, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchZnHtPkDOASHeatGain, curName, 0.0, 1);
            }
        }
        // Deallocate arrays no longer needed
        state.dataSize->ZoneSizing.deallocate();
        // CalcZoneSizing.deallocate();
    }
    if (state.dataSize->SysSizingRunDone) {
        for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
            curName = FinalSysSizing(AirLoopNum).AirPriLoopName;
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchSysSizCalcClAir, curName, CalcSysSizing(AirLoopNum).DesCoolVolFlow);
            if (std::abs(CalcSysSizing(AirLoopNum).DesCoolVolFlow) <= 1.e-8) {
                ShowWarningError(state,
                                 std::string{RoutineName} +
                                     "Calculated Cooling Design Air Flow Rate for System=" + FinalSysSizing(AirLoopNum).AirPriLoopName + " is zero.");
                ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
            }
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchSysSizUserClAir, curName, FinalSysSizing(AirLoopNum).DesCoolVolFlow);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchSysSizCalcHtAir, curName, CalcSysSizing(AirLoopNum).DesHeatVolFlow);
            if (std::abs(CalcSysSizing(AirLoopNum).DesHeatVolFlow) <= 1.e-8) {
                ShowWarningError(state,
                                 std::string{RoutineName} +
                                     "Calculated Heating Design Air Flow Rate for System=" + FinalSysSizing(AirLoopNum).AirPriLoopName + " is zero.");
                ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
            }
            std::string coolPeakLoadKind;
            std::string coolPeakDDDate;
            int coolPeakDD = 0;
            Real64 coolCap = 0.;
            if (FinalSysSizing(AirLoopNum).CoolingPeakLoadType == SensibleCoolingLoad) {
                coolPeakLoadKind = "Sensible";
                coolPeakDDDate = SysSizPeakDDNum(AirLoopNum).cSensCoolPeakDDDate;
                coolPeakDD = SysSizPeakDDNum(AirLoopNum).SensCoolPeakDD;
                coolCap = FinalSysSizing(AirLoopNum).SensCoolCap;
            } else if (FinalSysSizing(AirLoopNum).CoolingPeakLoadType == TotalCoolingLoad) {
                coolPeakLoadKind = "Total";
                coolPeakDDDate = SysSizPeakDDNum(AirLoopNum).cTotCoolPeakDDDate;
                coolPeakDD = SysSizPeakDDNum(AirLoopNum).TotCoolPeakDD;
                coolCap = FinalSysSizing(AirLoopNum).TotCoolCap;
            }
            if (coolPeakDD > 0) {
                ReportSysSizing(state,
                                curName,
                                "Cooling",
                                coolPeakLoadKind,
                                coolCap,
                                CalcSysSizing(AirLoopNum).DesCoolVolFlow,
                                FinalSysSizing(AirLoopNum).DesCoolVolFlow,
                                FinalSysSizing(AirLoopNum).CoolDesDay,
                                coolPeakDDDate,
                                SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk(coolPeakDD));
            } else {
                ReportSysSizing(state,
                                curName,
                                "Cooling",
                                coolPeakLoadKind,
                                coolCap,
                                CalcSysSizing(AirLoopNum).DesCoolVolFlow,
                                FinalSysSizing(AirLoopNum).DesCoolVolFlow,
                                FinalSysSizing(AirLoopNum).CoolDesDay,
                                coolPeakDDDate,
                                0);
            }
            int heatPeakDD = SysSizPeakDDNum(AirLoopNum).HeatPeakDD;
            if (heatPeakDD > 0) {
                ReportSysSizing(state,
                                curName,
                                "Heating",
                                "Sensible",
                                FinalSysSizing(AirLoopNum).HeatCap,
                                CalcSysSizing(AirLoopNum).DesHeatVolFlow,
                                FinalSysSizing(AirLoopNum).DesHeatVolFlow,
                                FinalSysSizing(AirLoopNum).HeatDesDay,
                                SysSizPeakDDNum(AirLoopNum).cHeatPeakDDDate,
                                SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk(heatPeakDD));
            } else {
                ReportSysSizing(state,
                                curName,
                                "Heating",
                                "Sensible",
                                FinalSysSizing(AirLoopNum).HeatCap,
                                CalcSysSizing(AirLoopNum).DesHeatVolFlow,
                                FinalSysSizing(AirLoopNum).DesHeatVolFlow,
                                FinalSysSizing(AirLoopNum).HeatDesDay,
                                SysSizPeakDDNum(AirLoopNum).cHeatPeakDDDate,
                                0);
            }
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchSysSizUserHtAir, curName, FinalSysSizing(AirLoopNum).DesHeatVolFlow);
        }
        // Deallocate arrays no longer needed
        state.dataSize->SysSizing.deallocate();
    }

    if ((state.dataGlobal->DoPlantSizing) && (state.dataSize->NumPltSizInput == 0)) {
        ShowWarningError(
            state,
            std::string{RoutineName} +
                "For a plant sizing run, there must be at least 1 Sizing:Plant object input. SimulationControl Plant Sizing option ignored.");
    }

    if ((state.dataSize->NumPltSizInput > 0) && (state.dataGlobal->DoPlantSizing) && !ErrorsFound) {

        ShowMessage(state, "Beginning Plant Sizing Calculations");
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Program terminates due to preceding conditions.");
    }
}

bool CalcdoLoadComponentPulseNow(EnergyPlusData &state,
                                 bool const isPulseZoneSizing,
                                 bool const WarmupFlag,
                                 int const HourOfDay,
                                 int const TimeStep,
                                 DataGlobalConstants::KindOfSim const KindOfSim)
{
    // This routine decides whether or not to do a Load Component Pulse.  True when yes it should, false when in shouldn't
    // This check looks to do the pulse at the first time step of the 10th hour of the day while not in warmup mode.
    // This needs to be done not just on the first day of a simulation because when the user picks a design day derived from
    // an attached weather file the design day is not necessarily the first day of the simulation.

    int const HourDayToPulse(10);
    int const TimeStepToPulse(1);

    if ((isPulseZoneSizing) && (!WarmupFlag) && (HourOfDay == HourDayToPulse) && (TimeStep == TimeStepToPulse) &&
        ((KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodDesign) || (state.dataGlobal->DayOfSim == 1))) {
        return true;
    } else {
        return false;
    }
}

void ManageSystemSizingAdjustments(EnergyPlusData &state)
{
    // This routine adjusts system sizing outcomes based on how the zone air terminals finish out their sizing.
    // The zone models are executed to trigger their sizing routines
    // Then the air terminal units are scanned to sum design flow rates. Every air terminal connected to a particular air loop is summed for
    //  1. minimum heating flow rate, 2. maximum heating flow rate, and 3. maximum flow rate.
    // the summed values are used to "Adjust" the system sizing results
    // the corrected values are used to autosize the central heating flow ratio, if set to autosize by the user.

    // Also store zone level flow information for Standard 62.1 calculations, Vpz, Vpz_min, Vdz, and Vdz_min for both cooling and heating

    auto &AirDistUnit(state.dataDefineEquipment->AirDistUnit);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &sd_airterminal(state.dataSingleDuct->sd_airterminal);

    if ((state.dataSize->NumSysSizInput > 0) && (state.dataGlobal->DoSystemSizing)) { // only if there is system sizing

        // call zone component models to execute their component sizing routines
        bool t_SimZoneEquip(true);
        bool t_SimAir(false);
        state.dataGlobal->BeginEnvrnFlag = true; // trigger begin envrn blocks in zone equipment models
        ZoneEquipmentManager::ManageZoneEquipment(state, true, t_SimZoneEquip, t_SimAir);
        state.dataGlobal->BeginEnvrnFlag = false;

        for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
            // Mine data from ATUs to find new design heating flow rates and new maximum flow rates
            Real64 airLoopMaxFlowRateSum(0.0);
            Real64 airLoopHeatingMinimumFlowRateSum(0.0);
            Real64 airLoopHeatingMaximumFlowRateSum(0.0);

            // sum up heating and max flows for any single duct systems, store 62.1 values by zone
            if (allocated(sd_airterminal) && state.dataSingleDuct->NumSDAirTerminal > 0) {
                for (int singleDuctATUNum = 1; singleDuctATUNum <= state.dataSingleDuct->NumSDAirTerminal; ++singleDuctATUNum) {
                    if (AirLoopNum == sd_airterminal(singleDuctATUNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(sd_airterminal(singleDuctATUNum).ADUNum).TermUnitSizingNum;
                        airLoopMaxFlowRateSum += sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate;

                        state.dataSize->VpzClgByZone(termUnitSizingIndex) =
                            sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate; // store std 62.1 values

                        if (sd_airterminal(singleDuctATUNum).SysType_Num == SingleDuct::SysType::SingleDuctConstVolReheat ||
                            sd_airterminal(singleDuctATUNum).SysType_Num == SingleDuct::SysType::SingleDuctConstVolNoReheat) {
                            airLoopHeatingMinimumFlowRateSum += sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate;
                            airLoopHeatingMaximumFlowRateSum += sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate;

                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate; // store std 62.1 values
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) =
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate; // store std 62.1 values
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate; // store std 62.1 values

                        } else {
                            airLoopHeatingMinimumFlowRateSum +=
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate * sd_airterminal(singleDuctATUNum).ZoneMinAirFrac;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) =
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate *
                                sd_airterminal(singleDuctATUNum).ZoneMinAirFrac; // store std 62.1 values
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate *
                                sd_airterminal(singleDuctATUNum).ZoneMinAirFrac;                // store std 62.1 values
                            if (sd_airterminal(singleDuctATUNum).MaxHeatAirVolFlowRate > 0.0) { // VS fan ATU has this non zero, so use it
                                airLoopHeatingMaximumFlowRateSum += sd_airterminal(singleDuctATUNum).MaxHeatAirVolFlowRate;
                                state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                    sd_airterminal(singleDuctATUNum).MaxHeatAirVolFlowRate; // store std 62.1 values
                            } else {
                                if (sd_airterminal(singleDuctATUNum).DamperHeatingAction == SingleDuct::Action::ReverseAction) {
                                    airLoopHeatingMaximumFlowRateSum += sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate;
                                    state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                        sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate; // store std 62.1 values
                                } else if (sd_airterminal(singleDuctATUNum).DamperHeatingAction == SingleDuct::Action::ReverseActionWithLimits) {
                                    airLoopHeatingMaximumFlowRateSum +=
                                        max(sd_airterminal(singleDuctATUNum).MaxAirVolFlowRateDuringReheat,
                                            (sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate * sd_airterminal(singleDuctATUNum).ZoneMinAirFrac));
                                    state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                        max(sd_airterminal(singleDuctATUNum).MaxAirVolFlowRateDuringReheat,
                                            (sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate *
                                             sd_airterminal(singleDuctATUNum).ZoneMinAirFrac)); // store std 62.1 values
                                } else {
                                    airLoopHeatingMaximumFlowRateSum +=
                                        sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate * sd_airterminal(singleDuctATUNum).ZoneMinAirFrac;
                                    state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                        sd_airterminal(singleDuctATUNum).MaxAirVolFlowRate *
                                        sd_airterminal(singleDuctATUNum).ZoneMinAirFrac; // store std 62.1 values
                                }
                            }
                        }
                        // single-path air terminal so Vdz = Vpz
                        state.dataSize->VdzClgByZone(termUnitSizingIndex) =
                            state.dataSize->VpzClgByZone(termUnitSizingIndex); // store std 62.1 values
                        state.dataSize->VdzMinClgByZone(termUnitSizingIndex) =
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex); // store std 62.1 values
                        state.dataSize->VdzHtgByZone(termUnitSizingIndex) =
                            state.dataSize->VpzHtgByZone(termUnitSizingIndex); // store std 62.1 values
                        state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) =
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex); // store std 62.1 values
                    }
                }
            }

            // sum up heating and max flows for any dual duct air terminals
            if (allocated(state.dataDualDuct->dd_airterminal) && state.dataDualDuct->NumDDAirTerminal > 0) {
                for (int dualDuctATUNum = 1; dualDuctATUNum <= state.dataDualDuct->NumDDAirTerminal; ++dualDuctATUNum) {
                    if (AirLoopNum == state.dataDualDuct->dd_airterminal(dualDuctATUNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(state.dataDualDuct->dd_airterminal(dualDuctATUNum).ADUNum).TermUnitSizingNum;
                        airLoopMaxFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                        state.dataSize->VpzClgByZone(termUnitSizingIndex) =
                            state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate; // store std 62.1 value

                        if (state.dataDualDuct->dd_airterminal(dualDuctATUNum).DamperType == DualDuct::DualDuctDamper::ConstantVolume) {
                            airLoopHeatingMaximumFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                            airLoopHeatingMinimumFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate; // store std 62.1 value
                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate; // store std 62.1 value
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate; // store std 62.1 value
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) = state.dataSize->VpzClgByZone(termUnitSizingIndex);
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = state.dataSize->VpzMinClgByZone(termUnitSizingIndex);
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) = state.dataSize->VpzHtgByZone(termUnitSizingIndex);
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);

                        } else if (state.dataDualDuct->dd_airterminal(dualDuctATUNum).DamperType == DualDuct::DualDuctDamper::VariableVolume) {
                            airLoopHeatingMaximumFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                            airLoopHeatingMinimumFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate *
                                                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).ZoneMinAirFrac;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate *
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).ZoneMinAirFrac; // store std 62.1 value
                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate; // store std 62.1 value
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate *
                                state.dataDualDuct->dd_airterminal(dualDuctATUNum).ZoneMinAirFrac; // store std 62.1 value
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) = state.dataSize->VpzClgByZone(termUnitSizingIndex);
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = state.dataSize->VpzMinClgByZone(termUnitSizingIndex);
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) = state.dataSize->VpzHtgByZone(termUnitSizingIndex);
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                        } else if (state.dataDualDuct->dd_airterminal(dualDuctATUNum).DamperType == DualDuct::DualDuctDamper::OutdoorAir) {
                            airLoopHeatingMaximumFlowRateSum += state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                            // Calculate the design OA flow rate for this zone
                            bool UseOccSchFlag = false;
                            bool UseMinOASchFlag = false;
                            Real64 designOAductFlow(0.0);
                            designOAductFlow =
                                DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                              state.dataDualDuct->dd_airterminal(dualDuctATUNum).OARequirementsPtr,
                                                                              state.dataDualDuct->dd_airterminal(dualDuctATUNum).ActualZoneNum,
                                                                              UseOccSchFlag,
                                                                              UseMinOASchFlag);
                            airLoopHeatingMinimumFlowRateSum += designOAductFlow;
                            // is this a dual duct is dual path for Std 62.1 ?? not sure, assume not because Vpz = Vdz
                            // anyDualPathAirTerminals = true;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = designOAductFlow; // not sure about this
                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) = designOAductFlow;    // no heating for this terminal
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) = designOAductFlow;
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) = state.dataDualDuct->dd_airterminal(dualDuctATUNum).MaxAirVolFlowRate;
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = designOAductFlow;
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) = designOAductFlow;
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = designOAductFlow;
                        }
                    }
                }
            }

            // sum up heating and max flows for any PIU air terminals
            if (allocated(state.dataPowerInductionUnits->PIU) && state.dataPowerInductionUnits->NumPIUs > 0) {
                for (int pIUATUNum = 1; pIUATUNum <= state.dataPowerInductionUnits->NumPIUs; ++pIUATUNum) {
                    if (AirLoopNum == state.dataPowerInductionUnits->PIU(pIUATUNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(state.dataPowerInductionUnits->PIU(pIUATUNum).ADUNum).TermUnitSizingNum;
                        airLoopMaxFlowRateSum += state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                        if (state.dataPowerInductionUnits->PIU(pIUATUNum).UnitType_Num ==
                            DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat) {
                            airLoopHeatingMaximumFlowRateSum += state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            airLoopHeatingMinimumFlowRateSum += state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;

                            // dual path for std 62.1
                            state.dataSize->VpzClgByZone(termUnitSizingIndex) = state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                                   state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxTotAirVolFlow; // which is constant for series PIU
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum)
                                    .MaxTotAirVolFlow; // min dz is the same as max because series PIU has constant discharge volume

                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow; // runs at minimum primary for heating always
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow; // runs at minimum primary for heating always
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxTotAirVolFlow; // which is constant for series PIU
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxTotAirVolFlow; // which is constant for series PIU

                            // store Ep for 62.1 calculations
                            state.dataSize->TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFraction =
                                state.dataSize->VpzMinClgByZone(termUnitSizingIndex) /
                                state.dataSize->VdzClgByZone(termUnitSizingIndex); // min primary divided by max total
                            state.dataSize->TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFractionHtg =
                                state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) / state.dataSize->VdzHtgByZone(termUnitSizingIndex);

                        } else if (state.dataPowerInductionUnits->PIU(pIUATUNum).UnitType_Num ==
                                   DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat) {
                            airLoopHeatingMaximumFlowRateSum += state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            airLoopHeatingMinimumFlowRateSum += state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;

                            // dual path for std 62.1
                            state.dataSize->VpzClgByZone(termUnitSizingIndex) = state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                                                                   state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow;
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum)
                                    .MaxPriAirVolFlow; // for Parallel PIU expect Fan off durign max cooling, so discharge is all primary
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                    state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow +
                                state.dataPowerInductionUnits->PIU(pIUATUNum)
                                    .MaxSecAirVolFlow; // expect secondary fan to be running at min cooling, for reheat

                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow; // primary at minimum
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow; // primary at minimum
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                    state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow +
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxSecAirVolFlow; // expect min primary and CV fan running
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) =
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MinPriAirFlowFrac *
                                    state.dataPowerInductionUnits->PIU(pIUATUNum).MaxPriAirVolFlow +
                                state.dataPowerInductionUnits->PIU(pIUATUNum).MaxSecAirVolFlow; // expect min primary and CV fan running

                            state.dataSize->TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFraction =
                                state.dataSize->VpzMinClgByZone(termUnitSizingIndex) /
                                state.dataSize->VdzClgByZone(termUnitSizingIndex); // min primary divided by max total
                            state.dataSize->TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFractionHtg =
                                state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) / state.dataSize->VdzHtgByZone(termUnitSizingIndex);
                        }
                    }
                }
            }

            auto &IndUnit = state.dataHVACSingleDuctInduc->IndUnit;
            auto &NumIndUnits = state.dataHVACSingleDuctInduc->NumIndUnits;

            // sum up heating and max flows for any four pipe induction units
            // dual path for std 62.1
            if (allocated(IndUnit) && (NumIndUnits > 0)) {
                for (int indUnitNum = 1; indUnitNum <= NumIndUnits; ++indUnitNum) {
                    if (AirLoopNum == IndUnit(indUnitNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(IndUnit(indUnitNum).ADUNum).TermUnitSizingNum;
                        airLoopHeatingMaximumFlowRateSum += IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        airLoopHeatingMinimumFlowRateSum += IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        airLoopMaxFlowRateSum += IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        // store Std 62.1 values, CV system
                        state.dataSize->VpzClgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VdzClgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxTotAirVolFlow;
                        state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxTotAirVolFlow;
                        state.dataSize->VpzHtgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxPriAirMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VdzHtgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxTotAirVolFlow;
                        state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = IndUnit(indUnitNum).MaxTotAirVolFlow;
                    }
                }
            }

            // sum up heating and max flows for any two pipe constant volume cooled beam terminal units
            if (allocated(state.dataHVACCooledBeam->CoolBeam) && (state.dataHVACCooledBeam->NumCB > 0)) {
                for (int coolBeamNum = 1; coolBeamNum <= state.dataHVACCooledBeam->NumCB; ++coolBeamNum) {
                    if (AirLoopNum == state.dataHVACCooledBeam->CoolBeam(coolBeamNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(state.dataHVACCooledBeam->CoolBeam(coolBeamNum).ADUNum).TermUnitSizingNum;
                        airLoopHeatingMaximumFlowRateSum += state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        airLoopHeatingMinimumFlowRateSum += state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        airLoopMaxFlowRateSum += state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;

                        // store std 62.1 values, beam will actually have secondary flow but that is not part of the model since it uses non air
                        // system term, we have no secondary flow rate information to work with
                        state.dataSize->VpzClgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VpzHtgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VdzClgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VdzHtgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                        state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = state.dataHVACCooledBeam->CoolBeam(coolBeamNum).MaxAirVolFlow;
                    }
                }
            }

            // sum up heating and max flows for any four pipe cooled beam terminal units (the only one using the airTerminalPtr at this point)
            if (allocated(AirDistUnit) && state.dataDefineEquipment->NumAirDistUnits > 0) {
                for (int aDUNum = 1; aDUNum <= state.dataDefineEquipment->NumAirDistUnits; ++aDUNum) {
                    if (AirDistUnit(aDUNum).airTerminalPtr.get() != nullptr) {
                        if (AirLoopNum == AirDistUnit(aDUNum).airTerminalPtr->getAirLoopNum()) {
                            airLoopHeatingMaximumFlowRateSum += AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            airLoopHeatingMinimumFlowRateSum += AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            airLoopMaxFlowRateSum += AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            // store Std 62.1 values, have no modeling of secondary flow rates for induced flow from beam
                            int termUnitSizingIndex = AirDistUnit(aDUNum).airTerminalPtr->getTermUnitSizingIndex();
                            state.dataSize->VpzClgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VpzHtgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VdzClgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VdzHtgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                            state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = AirDistUnit(aDUNum).airTerminalPtr->getPrimAirDesignVolFlow();
                        }
                    }
                }
            }

            // sum up flows for any air terminal mixers
            if (allocated(state.dataSingleDuct->SysATMixer) && (state.dataSingleDuct->NumATMixers > 0)) {
                for (int aTMixerNum = 1; aTMixerNum <= state.dataSingleDuct->NumATMixers; ++aTMixerNum) {
                    if (AirLoopNum == state.dataSingleDuct->SysATMixer(aTMixerNum).AirLoopNum) {
                        int termUnitSizingIndex = AirDistUnit(state.dataSingleDuct->SysATMixer(aTMixerNum).ADUNum).TermUnitSizingNum;
                        airLoopHeatingMaximumFlowRateSum += state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        airLoopHeatingMinimumFlowRateSum += state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        airLoopMaxFlowRateSum += state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;

                        state.dataSize->VpzClgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VpzMinClgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VpzHtgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VpzMinHtgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        // the ZoneHVAC devices will have secondary flow but how to get it, future work
                        state.dataSize->VdzClgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VdzMinClgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VdzHtgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                        state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) = state.dataSingleDuct->SysATMixer(aTMixerNum).DesignPrimaryAirVolRate;
                    }
                }
            }

            std::string curName = FinalSysSizing(AirLoopNum).AirPriLoopName;
            BaseSizer::reportSizerOutput(
                state, "AirLoopHVAC", curName, "Sum of Air Terminal Maximum Heating Flow Rates [m3/s]", airLoopHeatingMaximumFlowRateSum);
            BaseSizer::reportSizerOutput(
                state, "AirLoopHVAC", curName, "Sum of Air Terminal Minimum Heating Flow Rates [m3/s]", airLoopHeatingMinimumFlowRateSum);
            BaseSizer::reportSizerOutput(state, "AirLoopHVAC", curName, "Sum of Air Terminal Maximum Flow Rates [m3/s]", airLoopMaxFlowRateSum);

            // Adjust system sizing info
            if (allocated(FinalSysSizing)) {
                // correct sizing design heating volume flow rate based on finalized air terminal unit operation

                if (FinalSysSizing(AirLoopNum).SizingOption ==
                    NonCoincident) { // If non-coincident sizing method for this air loop, the we can use these sum's from air terminals directly
                    FinalSysSizing(AirLoopNum).DesHeatVolFlow = max(airLoopHeatingMaximumFlowRateSum, FinalSysSizing(AirLoopNum).DesHeatVolFlow);
                    FinalSysSizing(AirLoopNum).DesMainVolFlow = max(airLoopMaxFlowRateSum, FinalSysSizing(AirLoopNum).DesMainVolFlow);
                    if (FinalSysSizing(AirLoopNum).sysSizeCoolingDominant) {
                        FinalSysSizing(AirLoopNum).DesCoolVolFlow = FinalSysSizing(AirLoopNum).DesMainVolFlow;
                        FinalSysSizing(AirLoopNum).MassFlowAtCoolPeak = FinalSysSizing(AirLoopNum).DesCoolVolFlow * state.dataEnvrn->StdRhoAir;
                    } else if (FinalSysSizing(AirLoopNum).sysSizeHeatingDominant) { // make sure cooling is at least at minimum.
                        FinalSysSizing(AirLoopNum).DesCoolVolFlow = max(airLoopHeatingMinimumFlowRateSum, FinalSysSizing(AirLoopNum).DesCoolVolFlow);
                        FinalSysSizing(AirLoopNum).MassFlowAtCoolPeak = FinalSysSizing(AirLoopNum).DesCoolVolFlow * state.dataEnvrn->StdRhoAir;
                    }
                } else if (FinalSysSizing(AirLoopNum).SizingOption == Coincident) {

                    if (FinalSysSizing(AirLoopNum).sysSizeCoolingDominant) { // use minimum heating flow sum from air terminals
                        // know that minimum heating flow is a hard minimum regardless of concurrence situation, so make sure that design is at
                        // least that high.
                        FinalSysSizing(AirLoopNum).DesHeatVolFlow = max(airLoopHeatingMinimumFlowRateSum, FinalSysSizing(AirLoopNum).DesHeatVolFlow);
                        FinalSysSizing(AirLoopNum).DesMainVolFlow = max(airLoopHeatingMinimumFlowRateSum, FinalSysSizing(AirLoopNum).DesMainVolFlow);
                        FinalSysSizing(AirLoopNum).DesCoolVolFlow = FinalSysSizing(AirLoopNum).DesMainVolFlow;
                        FinalSysSizing(AirLoopNum).MassFlowAtCoolPeak = FinalSysSizing(AirLoopNum).DesCoolVolFlow * state.dataEnvrn->StdRhoAir;
                    } else if (FinalSysSizing(AirLoopNum).sysSizeHeatingDominant) { // use maximum heating flow sum from air terminals
                        FinalSysSizing(AirLoopNum).DesHeatVolFlow = max(airLoopHeatingMaximumFlowRateSum, FinalSysSizing(AirLoopNum).DesHeatVolFlow);
                        FinalSysSizing(AirLoopNum).DesMainVolFlow = max(airLoopHeatingMaximumFlowRateSum, FinalSysSizing(AirLoopNum).DesMainVolFlow);
                        // make sure cooling is at least at minimum.
                        FinalSysSizing(AirLoopNum).DesCoolVolFlow = max(airLoopHeatingMinimumFlowRateSum, FinalSysSizing(AirLoopNum).DesCoolVolFlow);
                        FinalSysSizing(AirLoopNum).MassFlowAtCoolPeak = FinalSysSizing(AirLoopNum).DesCoolVolFlow * state.dataEnvrn->StdRhoAir;
                    }
                }
                // report out adjusted design flow rates
                BaseSizer::reportSizerOutput(
                    state, "AirLoopHVAC", curName, "Adjusted Heating Design Air Flow Rate [m3/s]", FinalSysSizing(AirLoopNum).DesHeatVolFlow);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchSysSizAdjustedHtAir, curName, FinalSysSizing(AirLoopNum).DesHeatVolFlow, 4);
                BaseSizer::reportSizerOutput(
                    state, "AirLoopHVAC", curName, "Adjusted Cooling Design Air Flow Rate [m3/s]", FinalSysSizing(AirLoopNum).DesCoolVolFlow);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchSysSizAdjustedClAir, curName, FinalSysSizing(AirLoopNum).DesCoolVolFlow, 4);
                BaseSizer::reportSizerOutput(
                    state, "AirLoopHVAC", curName, "Adjusted Main Design Air Flow Rate [m3/s]", FinalSysSizing(AirLoopNum).DesMainVolFlow);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchSysSizAdjustedMainAir, curName, FinalSysSizing(AirLoopNum).DesMainVolFlow, 4);

                // Autosize central heating min system air flow rate, using corrected design heating flow, using maximum heating flow summation
                if (FinalSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized) {
                    if (FinalSysSizing(AirLoopNum).DesMainVolFlow > 0.0) { // protect div by zero
                        FinalSysSizing(AirLoopNum).SysAirMinFlowRat =
                            FinalSysSizing(AirLoopNum).DesHeatVolFlow / FinalSysSizing(AirLoopNum).DesMainVolFlow;
                    } else { // big trouble anyway.
                        FinalSysSizing(AirLoopNum).SysAirMinFlowRat = 1.0;
                    }
                    BaseSizer::reportSizerOutput(
                        state, "AirLoopHVAC", curName, "Calculated Heating Air Flow Ratio []", FinalSysSizing(AirLoopNum).SysAirMinFlowRat);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchSysSizCalcHeatFlowRatio, curName, FinalSysSizing(AirLoopNum).SysAirMinFlowRat, 4);
                    BaseSizer::reportSizerOutput(
                        state, "AirLoopHVAC", curName, "User Heating Air Flow Ratio []", FinalSysSizing(AirLoopNum).SysAirMinFlowRat);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchSysSizUserHeatFlowRatio, curName, FinalSysSizing(AirLoopNum).SysAirMinFlowRat, 4);
                } else {
                    BaseSizer::reportSizerOutput(
                        state, "AirLoopHVAC", curName, "User Heating Air Flow Ratio []", FinalSysSizing(AirLoopNum).SysAirMinFlowRat);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchSysSizUserHeatFlowRatio, curName, FinalSysSizing(AirLoopNum).SysAirMinFlowRat, 4);
                    Real64 calcSysAirMinFlowRat(0.0);
                    if (FinalSysSizing(AirLoopNum).DesMainVolFlow > 0.0) { // protect div by zero
                        calcSysAirMinFlowRat = FinalSysSizing(AirLoopNum).DesHeatVolFlow / FinalSysSizing(AirLoopNum).DesMainVolFlow;
                    }
                    BaseSizer::reportSizerOutput(state, "AirLoopHVAC", curName, "Calculated Heating Air Flow Ratio []", calcSysAirMinFlowRat);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchSysSizCalcHeatFlowRatio, curName, calcSysAirMinFlowRat, 4);
                }
            }
        }

    } // if doing any system sizing
}

void ManageSystemVentilationAdjustments(EnergyPlusData &state)
{
    auto &AirDistUnit(state.dataDefineEquipment->AirDistUnit);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &TermUnitFinalZoneSizing(state.dataSize->TermUnitFinalZoneSizing);
    auto &VbzByZone(state.dataSize->VbzByZone);
    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);

    // redo std 62.1 calculations using latest information on zone flows and report to tables

    // redo 62.1 zone calculations with final (or user) zone terminal flow sizes, only redo calculations that might change with final flows
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        int SysSizNum = UtilityRoutines::FindItemInList(
            FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
        if (FinalSysSizing(AirLoopNum).OAAutoSized &&
            (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP || state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) &&
            state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones > 1 && FinalSysSizing(AirLoopNum).LoadSizeType != Ventilation) {

            // Loop over all zones connected to air loop, redo both cooling and heating calcs for Zdz minimum discharge outdoor air fraction for
            // each zone
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                if (state.dataSize->VdzMinClgByZone(termUnitSizingIndex) > 0.0) {
                    state.dataSize->ZdzClgByZone(termUnitSizingIndex) =
                        min(1.0, TermUnitFinalZoneSizing(termUnitSizingIndex).VozClgByZone / state.dataSize->VdzMinClgByZone(termUnitSizingIndex));
                } else { // would divide by zero, so set to max ??
                    state.dataSize->ZdzClgByZone(termUnitSizingIndex) = 1.0;
                }
                if (state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) > 0.0) {
                    state.dataSize->ZdzHtgByZone(termUnitSizingIndex) =
                        min(1.0, TermUnitFinalZoneSizing(termUnitSizingIndex).VozHtgByZone / state.dataSize->VdzMinHtgByZone(termUnitSizingIndex));
                } else { // would divide by zero, so set to max
                    state.dataSize->ZdzHtgByZone(termUnitSizingIndex) = 1.0;
                }
            }
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                if (state.dataSize->VdzMinClgByZone(termUnitSizingIndex) > 0.0) {
                    state.dataSize->ZdzClgByZone(termUnitSizingIndex) =
                        min(1.0, TermUnitFinalZoneSizing(termUnitSizingIndex).VozClgByZone / state.dataSize->VdzMinClgByZone(termUnitSizingIndex));
                } else { // would divide by zero, so set to max ??
                    state.dataSize->ZdzClgByZone(termUnitSizingIndex) = 1.0;
                }
                if (state.dataSize->VdzMinHtgByZone(termUnitSizingIndex) > 0.0) {
                    state.dataSize->ZdzHtgByZone(termUnitSizingIndex) =
                        min(1.0, TermUnitFinalZoneSizing(termUnitSizingIndex).VozHtgByZone / state.dataSize->VdzMinHtgByZone(termUnitSizingIndex));
                } else { // would divide by zero, so set to max
                    state.dataSize->ZdzHtgByZone(termUnitSizingIndex) = 1.0;
                }
            } // end loop over zones on air loop to calculate Zdz values

            // Sum Voz values for System Vou, in E+ the Vbz value has now been corrected to remove population Diversity, so we add the term back
            // in here directly to get Vou, now corrected again to only apply D to the people part
            state.dataSize->VouBySys(AirLoopNum) =
                state.dataSize->DBySys(AirLoopNum) * state.dataSize->SumRpxPzBySys(AirLoopNum) + state.dataSize->SumRaxAzBySys(AirLoopNum);
            // redo VpzClgSumBySys( AirLoopNum ) with latest values, for reporting
            state.dataSize->VpzClgSumBySys(AirLoopNum) = 0.0;
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                state.dataSize->VpzClgSumBySys(AirLoopNum) += state.dataSize->VdzClgByZone(termUnitSizingIndex);
            }
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                int MatchingCooledZoneNum = General::FindNumberInList(
                    termUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, AirToZoneNodeInfo(AirLoopNum).NumZonesCooled);
                if (MatchingCooledZoneNum == 0) {
                    state.dataSize->VpzClgSumBySys(AirLoopNum) += state.dataSize->VdzClgByZone(termUnitSizingIndex);
                }
            }

            // Fill Vps for cooling VRP calculation, use cooling design flow rate as adjusted in ManageSystemSizingAdjustments ( to use
            // conincident sizing result if available for block air flow
            state.dataSize->VpsClgBySys(AirLoopNum) = FinalSysSizing(SysSizNum).DesCoolVolFlow;

            // Fill Vps for heating VRP calculation, use heating min by zone from air terminal scan in ManageSystemSizingAdjustments
            state.dataSize->VpsHtgBySys(AirLoopNum) = 0.0;
            state.dataSize->VpzHtgSumBySys(AirLoopNum) = 0.0; // for reporting only
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                state.dataSize->VpsHtgBySys(AirLoopNum) += state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                state.dataSize->VpzHtgSumBySys(AirLoopNum) += state.dataSize->VpzHtgByZone(termUnitSizingIndex);
            }
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                int MatchingCooledZoneNum = General::FindNumberInList(
                    termUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, AirToZoneNodeInfo(AirLoopNum).NumZonesCooled);
                if (MatchingCooledZoneNum == 0) {
                    state.dataSize->VpsHtgBySys(AirLoopNum) += state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                    state.dataSize->VpzHtgSumBySys(AirLoopNum) += state.dataSize->VpzHtgByZone(termUnitSizingIndex);
                }
            }
            // Fill Xs values
            state.dataSize->XsBySysCool(AirLoopNum) = state.dataSize->VouBySys(AirLoopNum) / state.dataSize->VpsClgBySys(AirLoopNum);
            state.dataSize->XsBySysHeat(AirLoopNum) = state.dataSize->VouBySys(AirLoopNum) / state.dataSize->VpsHtgBySys(AirLoopNum);

            // Loop over zones and calculate Evz for each for both cooling and heating, and find mins
            state.dataSize->EvzMinBySysCool(AirLoopNum) = 1.0;
            state.dataSize->EvzMinBySysHeat(AirLoopNum) = 1.0;

            // make two passes, one for cooled zone and one for heated zones, if some zones are duplicate, it's OK, it'll just redo the same calcs
            for (int coolHeatPass = 1; coolHeatPass <= 2; ++coolHeatPass) {
                int numZones = 0;
                if (coolHeatPass == 1) {
                    numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
                } else {
                    numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
                }
                for (int zoneNum = 1; zoneNum <= numZones; ++zoneNum) {
                    int termUnitSizingIndex = 0;
                    if (coolHeatPass == 1) {
                        termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                    } else {
                        termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                    }
                    Real64 Er = TermUnitFinalZoneSizing(termUnitSizingIndex)
                                    .ZoneSecondaryRecirculation; // user input in Zone Air Distribution design spec object

                    if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                        if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                            state.dataSize->EvzByZoneHeat(termUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                        } else {
                            state.dataSize->EvzByZoneHeat(termUnitSizingIndex) = 0.75;
                        }
                        state.dataSize->EvzByZoneCool(termUnitSizingIndex) = state.dataSize->EvzByZoneHeat(termUnitSizingIndex);
                    } else if (Er > 0.0) { // 62.1 ventilation rate procedure - multi path zone
                        // Find Evz for cooling
                        Real64 Ep_Clg =
                            TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFraction; // as adjusted in ManageSystemSizingAdjustments();
                        Real64 Fa_Clg = Ep_Clg + (1.0 - Ep_Clg) * Er;
                        state.dataSize->FaByZoneCool(termUnitSizingIndex) = Fa_Clg;
                        Real64 Fb_Clg = Ep_Clg;
                        state.dataSize->FbByZoneCool(termUnitSizingIndex) = Fb_Clg;
                        Real64 Ez_Clg =
                            TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling; // user input in Zone Air Distribution design spec object
                        Real64 Fc_Clg = 1.0 - (1.0 - Ez_Clg) * (1.0 - Er) * (1 - Ep_Clg);
                        state.dataSize->FcByZoneCool(termUnitSizingIndex) = Fc_Clg;
                        state.dataSize->EvzByZoneCool(termUnitSizingIndex) =
                            (Fa_Clg + state.dataSize->XsBySysCool(AirLoopNum) * Fb_Clg - state.dataSize->ZdzClgByZone(termUnitSizingIndex) * Fc_Clg) /
                            Fa_Clg;
                        // note that SimAirServingZones::LimitZoneVentEff is intended only for single path per I/O ref

                        // find Evz for heating
                        Real64 Ep_Htg =
                            TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFractionHtg; // as adjusted in ManageSystemSizingAdjustments();
                        Real64 Fa_Htg = Ep_Htg + (1.0 - Ep_Htg) * Er;
                        state.dataSize->FaByZoneHeat(termUnitSizingIndex) = Fa_Htg;
                        Real64 Fb_Htg = Ep_Htg;
                        state.dataSize->FbByZoneCool(termUnitSizingIndex) = Fb_Htg;
                        Real64 Ez_Htg =
                            TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating; // user input in Zone Air Distribution design spec object
                        Real64 Fc_Htg = 1.0 - (1.0 - Ez_Htg) * (1.0 - Er) * (1 - Ep_Htg);
                        state.dataSize->FcByZoneHeat(termUnitSizingIndex) = Fc_Htg;
                        state.dataSize->EvzByZoneHeat(termUnitSizingIndex) =
                            (Fa_Htg + state.dataSize->XsBySysHeat(AirLoopNum) * Fb_Htg - state.dataSize->ZdzHtgByZone(termUnitSizingIndex) * Fc_Htg) /
                            Fa_Htg;

                    } else { // 62.1 ventilation rate procedure - single path zone
                        state.dataSize->EvzByZoneCool(termUnitSizingIndex) =
                            1.0 + state.dataSize->XsBySysCool(AirLoopNum) - state.dataSize->ZdzClgByZone(termUnitSizingIndex);
                        SimAirServingZones::LimitZoneVentEff(state,
                                                             state.dataSize->XsBySysCool(AirLoopNum),
                                                             VbzByZone(termUnitSizingIndex) / state.dataSize->EvzByZoneCool(termUnitSizingIndex),
                                                             termUnitSizingIndex,
                                                             state.dataSize->EvzByZoneCool(termUnitSizingIndex));
                        state.dataSize->EvzByZoneHeat(termUnitSizingIndex) =
                            1.0 + state.dataSize->XsBySysHeat(AirLoopNum) - state.dataSize->ZdzHtgByZone(termUnitSizingIndex);
                        SimAirServingZones::LimitZoneVentEff(state,
                                                             state.dataSize->XsBySysHeat(AirLoopNum),
                                                             VbzByZone(termUnitSizingIndex) / state.dataSize->EvzByZoneHeat(termUnitSizingIndex),
                                                             termUnitSizingIndex,
                                                             state.dataSize->EvzByZoneHeat(termUnitSizingIndex));
                    }

                    if (state.dataSize->EvzByZoneCool(termUnitSizingIndex) < state.dataSize->EvzMinBySysCool(AirLoopNum)) {
                        state.dataSize->EvzMinBySysCool(AirLoopNum) = state.dataSize->EvzByZoneCool(termUnitSizingIndex);
                    }
                    if (state.dataSize->EvzByZoneHeat(termUnitSizingIndex) < state.dataSize->EvzMinBySysHeat(AirLoopNum)) {
                        state.dataSize->EvzMinBySysHeat(AirLoopNum) = state.dataSize->EvzByZoneHeat(termUnitSizingIndex);
                    }
                } // end loop over zones on air loop to calculate Evz by zone and find mins

                // calculate Vot for both cooling and heating
                state.dataSize->VotClgBySys(AirLoopNum) = state.dataSize->VouBySys(AirLoopNum) / state.dataSize->EvzMinBySysCool(AirLoopNum);
                state.dataSize->VotHtgBySys(AirLoopNum) = state.dataSize->VouBySys(AirLoopNum) / state.dataSize->EvzMinBySysHeat(AirLoopNum);
                // the design zone ventilation value is based on the larger of the system-level cooling Vot and/or heating Vot
                FinalSysSizing(AirLoopNum).DesOutAirVolFlow = max(state.dataSize->VotClgBySys(AirLoopNum), state.dataSize->VotHtgBySys(AirLoopNum));
            }
        } // system OA is autosized and VRP
        else if ((FinalSysSizing(AirLoopNum).OAAutoSized && state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP &&
                  state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones == 1)) { // single zone VRP
            int termUnitSizingIndex = 0;
            termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(1);
            if (termUnitSizingIndex == 0) {
                termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(1);
            }
            // single zone cooling
            state.dataSize->VotClgBySys(AirLoopNum) = VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling;
            state.dataSize->EvzByZoneCool(termUnitSizingIndex) = TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling;
            state.dataSize->EvzMinBySysCool(AirLoopNum) = state.dataSize->EvzByZoneCool(termUnitSizingIndex);
            state.dataSize->VpsClgBySys(AirLoopNum) = FinalSysSizing(SysSizNum).DesCoolVolFlow;
            state.dataSize->VpzClgSumBySys(AirLoopNum) = state.dataSize->VdzClgByZone(termUnitSizingIndex);
            // single zone heating
            state.dataSize->VotHtgBySys(AirLoopNum) = VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating;
            state.dataSize->EvzByZoneHeat(termUnitSizingIndex) = TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating;
            state.dataSize->EvzMinBySysHeat(AirLoopNum) = state.dataSize->EvzByZoneHeat(termUnitSizingIndex);
            state.dataSize->VpsHtgBySys(AirLoopNum) = state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
            state.dataSize->VpzHtgSumBySys(AirLoopNum) = state.dataSize->VpzHtgByZone(termUnitSizingIndex);

            // the design zone ventilation value is based on the larger of the system-level cooling Vot and/or heating Vot
            FinalSysSizing(AirLoopNum).DesOutAirVolFlow = max(state.dataSize->VotClgBySys(AirLoopNum), state.dataSize->VotHtgBySys(AirLoopNum));
            // Fill Xs values for reporting
            state.dataSize->XsBySysCool(AirLoopNum) = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / state.dataSize->VpsClgBySys(AirLoopNum);
            state.dataSize->XsBySysHeat(AirLoopNum) = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / state.dataSize->VpsHtgBySys(AirLoopNum);

        } else { // not vrp, zone sum, fill out values that still apply
            // redo VpzClgSumBySys( AirLoopNum ) with latest values, for reporting
            state.dataSize->VpzClgSumBySys(AirLoopNum) = 0.0;
            // Fill Vps for cooling VRP calculation, use cooling design flow rate as adjusted in ManageSystemSizingAdjustments ( to use
            // conincident sizing result if available for block air flow
            state.dataSize->VpsClgBySys(AirLoopNum) = FinalSysSizing(SysSizNum).DesCoolVolFlow;
            // Fill Vps for heating VRP calculation, use heating min by zone from air terminal scan in ManageSystemSizingAdjustments
            state.dataSize->VpsHtgBySys(AirLoopNum) = 0.0;
            state.dataSize->VpzHtgSumBySys(AirLoopNum) = 0.0; // for reporting only
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                state.dataSize->VpzClgSumBySys(AirLoopNum) += state.dataSize->VdzClgByZone(termUnitSizingIndex);
                state.dataSize->VpsHtgBySys(AirLoopNum) += state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                state.dataSize->VpzHtgSumBySys(AirLoopNum) += state.dataSize->VpzHtgByZone(termUnitSizingIndex);
            }
            for (int zoneNum = 1; zoneNum <= AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++zoneNum) {
                int termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                int MatchingCooledZoneNum = General::FindNumberInList(
                    termUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, AirToZoneNodeInfo(AirLoopNum).NumZonesCooled);
                if (MatchingCooledZoneNum == 0) {
                    state.dataSize->VpzClgSumBySys(AirLoopNum) += state.dataSize->VdzClgByZone(termUnitSizingIndex);
                    state.dataSize->VpsHtgBySys(AirLoopNum) += state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                    state.dataSize->VpzHtgSumBySys(AirLoopNum) += state.dataSize->VpzHtgByZone(termUnitSizingIndex);
                }
            }
        }
    } // airloop loop

    // write out predefined standard 62.1 report data, total of 8 tables
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {

        // System Ventilation Requirements for Cooling (table 1)
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClSumVpz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpzClgSumBySys(AirLoopNum),
                                                 4); // Vpz-sum
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PsBySys(AirLoopNum),
                                                 4); // Ps
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClSumPz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PzSumBySys(AirLoopNum),
                                                 4); // Pz-sum
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62svrClD, FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->DBySys(AirLoopNum), 4); // D
        // Origin of D
        int SysSizNum = UtilityRoutines::FindItemInList(
            FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
        if (state.dataSize->SysSizInput(SysSizNum).OccupantDiversity == AutoSize) {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrClDorg, FinalSysSizing(AirLoopNum).AirPriLoopName, "Calculated from schedules");
        } else {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrClDorg, FinalSysSizing(AirLoopNum).AirPriLoopName, "User-specified");
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClVou,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VouBySys(AirLoopNum),
                                                 4); // Vou
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClVps,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpsClgBySys(AirLoopNum),
                                                 4); // Vps
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClXs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->XsBySysCool(AirLoopNum),
                                                 4); // Xs
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClEv,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->EvzMinBySysCool(AirLoopNum),
                                                 4); // Ev
        // Ev Calculation Methodology
        if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrClEvMthd,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     "Standard 62.1 Ventilation Rate Procedure");
        } else if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrClEvMthd,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     "Standard 62.1 Simplified Procedure");
        } else {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrClEvMthd, FinalSysSizing(AirLoopNum).AirPriLoopName, "Not calculated");
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClVot,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VotClgBySys(AirLoopNum),
                                                 4);          // Vot
        if (state.dataSize->VpsClgBySys(AirLoopNum) != 0.0) { // Move here
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrClPercOA,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     state.dataSize->VotClgBySys(AirLoopNum) / state.dataSize->VpsClgBySys(AirLoopNum),
                                                     4); //%OA
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClEnvironmentOfPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PeakPsOccurrenceEnvironmentStringBySys(AirLoopNum));
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrClTimeOfPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PeakPsOccurrenceDateTimeStringBySys(AirLoopNum));

        // system ventilation requirements for heating ( table 2 )
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtSumVpz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpzHtgSumBySys(AirLoopNum),
                                                 4); // Vpz-sum
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PsBySys(AirLoopNum),
                                                 4); // Ps
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtSumPz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PzSumBySys(AirLoopNum),
                                                 4); // Pz-sum
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62svrHtD, FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->DBySys(AirLoopNum), 4); // D
        // Origin of D
        if (state.dataSize->SysSizInput(SysSizNum).OccupantDiversity == AutoSize) {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrHtDorg, FinalSysSizing(AirLoopNum).AirPriLoopName, "Calculated from schedules");
        } else {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrHtDorg, FinalSysSizing(AirLoopNum).AirPriLoopName, "User-specified");
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtVou,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VouBySys(AirLoopNum),
                                                 4); // Vou
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtVps,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpsHtgBySys(AirLoopNum),
                                                 4); // Vps
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtXs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->XsBySysHeat(AirLoopNum),
                                                 4); // Xs
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtEv,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->EvzMinBySysHeat(AirLoopNum),
                                                 4); // Ev
        // Ev Calculation Methodology
        if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrHtEvMthd,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     "Standard 62.1 Ventilation Rate Procedure");
        } else if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrHtEvMthd,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     "Standard 62.1 Simplified Procedure");
        } else {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchS62svrHtEvMthd, FinalSysSizing(AirLoopNum).AirPriLoopName, "Not calculated");
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtVot,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VotHtgBySys(AirLoopNum),
                                                 4); // Vot
        if (state.dataSize->VpsHtgBySys(AirLoopNum) != 0.0) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svrHtPercOA,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     state.dataSize->VotHtgBySys(AirLoopNum) / state.dataSize->VpsHtgBySys(AirLoopNum),
                                                     4); //%OA
        }
        // heating time of peak Ps is the same as for cooling (for now)
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtEnvironmentOfPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PeakPsOccurrenceEnvironmentStringBySys(AirLoopNum));
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svrHtTimeOfPs,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->PeakPsOccurrenceDateTimeStringBySys(AirLoopNum));

        // Zone ventilation parameters, (table 3)
        // make two passes, one for cooled zones and one for heated zones, if a zone is the same on the second pass, skip it
        for (int coolHeatPass = 1; coolHeatPass <= 2; ++coolHeatPass) {
            int numZones = 0;
            if (coolHeatPass == 1) {
                numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            } else {
                numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            }
            for (int zoneNum = 1; zoneNum <= numZones; ++zoneNum) {
                int termUnitSizingIndex = 0;
                int MatchingCooledZoneNum = 0;
                if (coolHeatPass == 1) {
                    termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                } else {
                    termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                    MatchingCooledZoneNum = General::FindNumberInList(
                        termUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, AirToZoneNodeInfo(AirLoopNum).NumZonesCooled);
                }
                if (MatchingCooledZoneNum == 0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpAlN,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name); // Air loop name
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpRp,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).DesOAFlowPPer,
                                                             6); // Rp
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpPz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).TotPeopleInZone,
                                                             4); // Pz
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpRa,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).DesOAFlowPerArea,
                                                             6); // Ra
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpAz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).TotalZoneFloorArea); // Az
                    OutputReportPredefined::PreDefTableEntry(
                        state,
                        state.dataOutRptPredefined->pdchS62zvpVbz,
                        TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                        VbzByZone(termUnitSizingIndex),
                        4); // Vbz, now corrected so that Vbz does not already have system population term multiplied into it
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpClEz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling,
                                                             4); // Ez-clg
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling > 0.0) {
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchS62zvpClVoz,
                                                                 TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                 VbzByZone(termUnitSizingIndex) /
                                                                     TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling,
                                                                 4); // Voz-clg
                    }
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zvpHtEz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating,
                                                             3); // Ez-htg
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating != 0.0) {
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchS62zvpHtVoz,
                                                                 TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                 VbzByZone(termUnitSizingIndex) /
                                                                     TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating,
                                                                 4); // Voz-htg
                    }
                }
            }
        }

        // System Ventilation Parameters, (Table 4)

        // first do some summations needed
        Real64 RpPzSum(0.0);
        Real64 RaAzSum(0.0);
        Real64 AzSum(0.0);
        Real64 VbzSum(0.0);
        Real64 VozClgSum(0.0);
        Real64 VozHtgSum(0.0);
        Real64 VdzClgSum(0.0);
        Real64 VdzHtgSum(0.0);
        Real64 VpzMinClgSum(0.0);
        Real64 VpzMinHtgSum(0.0);
        // make two passes, one for cooled zones and one for heated zones, if a zone is the same on the second pass, skip it
        for (int coolHeatPass = 1; coolHeatPass <= 2; ++coolHeatPass) {
            int numZones = 0;
            if (coolHeatPass == 1) {
                numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            } else {
                numZones = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            }
            for (int zoneNum = 1; zoneNum <= numZones; ++zoneNum) {
                int termUnitSizingIndex = 0;
                int MatchingCooledZoneNum = 0;
                if (coolHeatPass == 1) {
                    termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                } else {
                    termUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                    MatchingCooledZoneNum = General::FindNumberInList(
                        termUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, AirToZoneNodeInfo(AirLoopNum).NumZonesCooled);
                }
                if (MatchingCooledZoneNum == 0) {

                    // Zone ventilation parameters, (table 3)
                    RpPzSum +=
                        TermUnitFinalZoneSizing(termUnitSizingIndex).DesOAFlowPPer * TermUnitFinalZoneSizing(termUnitSizingIndex).TotPeopleInZone;
                    RaAzSum += TermUnitFinalZoneSizing(termUnitSizingIndex).DesOAFlowPerArea *
                               TermUnitFinalZoneSizing(termUnitSizingIndex).TotalZoneFloorArea;
                    AzSum += TermUnitFinalZoneSizing(termUnitSizingIndex).TotalZoneFloorArea;
                    VbzSum += VbzByZone(termUnitSizingIndex);
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling != 0.0) {
                        VozClgSum += VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling;
                    }
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating != 0.0) {
                        VozHtgSum += VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating;
                    }

                    VpzMinClgSum += state.dataSize->VpzMinClgByZone(termUnitSizingIndex);
                    VdzClgSum += state.dataSize->VdzClgByZone(termUnitSizingIndex);
                    VpzMinHtgSum += state.dataSize->VpzMinHtgByZone(termUnitSizingIndex);
                    VdzHtgSum += state.dataSize->VdzMinHtgByZone(termUnitSizingIndex);

                    // Zone Ventilation Calculations for Cooling Design, (Table 5)
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdAlN,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             AirToZoneNodeInfo(AirLoopNum).AirLoopName); // Air loop name
                    for (int iAirDistUnit = 1; iAirDistUnit <= state.dataDefineEquipment->NumAirDistUnits; ++iAirDistUnit) {
                        if (AirDistUnit(iAirDistUnit).TermUnitSizingNum == termUnitSizingIndex) {
                            OutputReportPredefined::PreDefTableEntry(state,
                                                                     state.dataOutRptPredefined->pdchS62zcdBox,
                                                                     TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                     AirDistUnit(iAirDistUnit).EquipType(1)); // use first type of equipment listed
                            break; // if it has been found no more searching is needed
                        }
                    }
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdVpz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VpzClgByZone(termUnitSizingIndex),
                                                             4); // Vpz LS:
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdVdz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VdzClgByZone(termUnitSizingIndex),
                                                             4); // Vdz
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdVpzmin,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VpzMinClgByZone(termUnitSizingIndex),
                                                             4); // Vpz-min
                    // Vpz-min, simplified procedure?
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).VpzMinByZoneSPSized) {
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchS62zcdVpzminSPSize, TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName, "Yes");
                    } else {
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchS62zcdVpzminSPSize, TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName, "No");
                    }
                    Real64 VozClg = 0.0;
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling > 0.0) {
                        VozClg = VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffCooling;
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchS62zcdVozclg,
                                                                 TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                 VozClg,
                                                                 4); // Voz-clg
                    }
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdZpz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->ZdzClgByZone(termUnitSizingIndex),
                                                             4); // Zpz = Voz/Vpz (see eq 6-5 in 62.1-2010)
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdEp,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFraction,
                                                             4); // Ep
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdEr,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneSecondaryRecirculation,
                                                             4); // Er
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdFa,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FaByZoneCool(termUnitSizingIndex),
                                                             4); // Fa
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdFb,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FbByZoneCool(termUnitSizingIndex),
                                                             4); // Fb
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdFc,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FcByZoneCool(termUnitSizingIndex),
                                                             4); // Fc
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zcdEvz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->EvzByZoneCool(termUnitSizingIndex),
                                                             4); // Evz

                    // Zone Ventilation Calculations for Heating Design (Table 7)
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdAlN,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             AirToZoneNodeInfo(AirLoopNum).AirLoopName); // Air loop name
                    for (int iAirDistUnit = 1; iAirDistUnit <= state.dataDefineEquipment->NumAirDistUnits; ++iAirDistUnit) {
                        if (AirDistUnit(iAirDistUnit).TermUnitSizingNum == termUnitSizingIndex) {
                            OutputReportPredefined::PreDefTableEntry(state,
                                                                     state.dataOutRptPredefined->pdchS62zhdBox,
                                                                     TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                     AirDistUnit(iAirDistUnit).EquipType(1)); // use first type of equipment listed
                            break; // if it has been found no more searching is needed
                        }
                    }
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdVpz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VpzHtgByZone(termUnitSizingIndex),
                                                             4); // Vpz
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdVdz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VdzHtgByZone(termUnitSizingIndex),
                                                             4); // Vdz
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdVpzmin,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->VpzMinHtgByZone(termUnitSizingIndex),
                                                             4); // Vpz-min
                    // Vpz-min, simplified procedure?
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).VpzMinByZoneSPSized) {
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchS62zhdVpzminSPSize, TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName, "Yes");
                    } else {
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchS62zhdVpzminSPSize, TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName, "No");
                    }
                    Real64 VozHtg = 0.0;
                    if (TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating != 0.0) {
                        VozHtg = VbzByZone(termUnitSizingIndex) / TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneADEffHeating;
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchS62zhdVozhtg,
                                                                 TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                                 VozHtg,
                                                                 4); // Voz-htg
                    }
                    // Outdoor Air Details Report - Design Zone Outdoor Airflow - Voz
                    Real64 VozMax = std::max(VozHtg, VozClg); // take larger of the heating and cooling Voz values
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchOaMvDesZnOa, TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName, VozMax, 4);
                    state.dataOutRptPredefined->TotalVozMax +=
                        VozMax * state.dataHeatBal->Zone(zoneNum).Multiplier * state.dataHeatBal->Zone(zoneNum).ListMultiplier;
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdZpz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZpzHtgByZone,
                                                             4); // Zpz = Voz/Vpz (see eq 6-5 in 62.1-2010)
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdEp,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZonePrimaryAirFractionHtg,
                                                             4); // Ep
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdEr,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneSecondaryRecirculation,
                                                             4); // Er
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdFa,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FaByZoneHeat(termUnitSizingIndex),
                                                             4); // Fa
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdFb,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FbByZoneHeat(termUnitSizingIndex),
                                                             4); // Fb
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdFc,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->FcByZoneHeat(termUnitSizingIndex),
                                                             4); // Fc
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchS62zhdEvz,
                                                             TermUnitFinalZoneSizing(termUnitSizingIndex).ZoneName,
                                                             state.dataSize->EvzByZoneHeat(termUnitSizingIndex),
                                                             4); // Evz
                }
            }
        }

        // System Ventilation Parameters, (Table 4)
        if (state.dataSize->PzSumBySys(AirLoopNum) != 0.0) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svpRp,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     RpPzSum / state.dataSize->PzSumBySys(AirLoopNum),
                                                     6); // Average Rp for system
        }
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62svpPz, FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->PzSumBySys(AirLoopNum));
        if (AzSum != 0.0) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchS62svpRa,
                                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                     RaAzSum / AzSum,
                                                     6); // average Ra for system
        }
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svpAz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 AzSum,
                                                 4); // Az sum
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62svpVbz, FinalSysSizing(AirLoopNum).AirPriLoopName, VbzSum, 4);
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svpClVoz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VozClgSum,
                                                 4); // Voz-clg
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62svpHtVoz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VozHtgSum,
                                                 4); // Voz-htg

        // System Ventilation Calculations for Cooling Design (Table 6)
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62scdVpz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpzClgSumBySys(AirLoopNum),
                                                 4); // Vpz-sum
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62scdVps,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpsClgBySys(AirLoopNum),
                                                 4); // Vps
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62scdVpzmin, FinalSysSizing(AirLoopNum).AirPriLoopName, VpzMinClgSum, 4); // Vpz-min
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62scdVdz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VdzClgSum,
                                                 4); // Vdz-sum
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62scdVozclg,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VozClgSum,
                                                 4); // Voz-clg
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62scdEvz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->EvzMinBySysCool(AirLoopNum),
                                                 4); // Evz-min

        // System Ventilation Calculations for Heating Design (Table 8)
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62shdVpz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpzHtgSumBySys(AirLoopNum),
                                                 4); // Vpz-sum
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62shdVps,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->VpsHtgBySys(AirLoopNum),
                                                 4); // Vps
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62shdVdz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VdzHtgSum,
                                                 4); // Vdz-sum
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62shdVpzmin, FinalSysSizing(AirLoopNum).AirPriLoopName, VpzMinHtgSum, 4); // Vpz-min
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62shdVozhtg,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 VozHtgSum,
                                                 4); // Voz-htg
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchS62shdEvz,
                                                 FinalSysSizing(AirLoopNum).AirPriLoopName,
                                                 state.dataSize->EvzMinBySysHeat(AirLoopNum),
                                                 4); // Evz-min

    } // loop over air loops for table writing
}

void DetermineSystemPopulationDiversity(EnergyPlusData &state)
{

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &SysSizInput(state.dataSize->SysSizInput);

    // determine Pz sum, Ps, and D for each air system for standard 62.1

    // first determine if any airloops use VRP, if not then don't need to march thru year of schedules for performance
    bool anyVRPinModel(false);
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_VRP || FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) {
            anyVRPinModel = true;
            break;
        }
    }
    // First get the design (max) level of people in all zones connected to air loop
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        int SysSizNum = UtilityRoutines::FindItemInList(
            FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
        // only retrieve data if the occupant density is set to be autosized
        if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizInput(SysSizNum).OccupantDiversity == AutoSize) {
            state.dataSize->PzSumBySys(AirLoopNum) = 0.0;
            state.dataSize->PsBySys(AirLoopNum) = 0.0;
            for (int zoneNumOnLoop = 1; zoneNumOnLoop <= state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones; ++zoneNumOnLoop) {
                int CtrlZoneNum = state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).ActualZoneNumber(zoneNumOnLoop);
                for (int PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                    if (state.dataHeatBal->People(PeopleNum).ZonePtr == state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum) {
                        state.dataSize->PzSumBySys(AirLoopNum) +=
                            (state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                             state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).Multiplier *
                             state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).ListMultiplier);
                    }
                }
            }
        }
    }

    if (!anyVRPinModel) {
        for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
            state.dataSize->DBySys(AirLoopNum) = 1.0;
        }
        return; // early return to not march through schedules
    }

    DisplayString(state, "Standard 62.1 Ventilation Rate Procedure: Determine System Occupant Diversity");
    // now march through all zone timesteps for entire year to find the concurrent max
    int DaysInYear(366);  // assume leap year
    int dayOfWeekType(1); // assume year starts on Sunday
    WeatherManager::CalcSpecialDayTypes(state);
    for (int DayLoop = 1; DayLoop <= DaysInYear; ++DayLoop) { // loop over all days in year
        state.dataEnvrn->HolidayIndex = state.dataWeatherManager->SpecialDayTypes(DayLoop);
        state.dataEnvrn->DayOfYear_Schedule = DayLoop;
        state.dataEnvrn->DayOfWeek = dayOfWeekType;
        ++dayOfWeekType;
        if (dayOfWeekType > 7) dayOfWeekType = 1;
        for (int hrOfDay = 1; hrOfDay <= 24; ++hrOfDay) {                         // loop over all hours in day
            state.dataGlobal->HourOfDay = hrOfDay;                                // avoid crash in schedule manager
            for (int TS = 1; TS <= state.dataGlobal->NumOfTimeStepInHour; ++TS) { // loop over all timesteps in hour
                state.dataGlobal->TimeStep = TS;                                  // avoid crash in schedule manager
                Real64 TSfraction(0.0);
                if (state.dataGlobal->NumOfTimeStepInHour > 0.0) TSfraction = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
                for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) { // loop over all the air systems
                    int SysSizNum = UtilityRoutines::FindItemInList(
                        FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
                    if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
                    if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizInput(SysSizNum).OccupantDiversity == AutoSize) {

                        // Loop over all zones connected to air loop
                        Real64 TotConcurrentPeopleOnSys = 0.0;
                        for (int zoneNumOnLoop = 1; zoneNumOnLoop <= state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones; ++zoneNumOnLoop) {
                            int CtrlZoneNum = state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).ActualZoneNumber(zoneNumOnLoop);

                            for (int PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                                if (state.dataHeatBal->People(PeopleNum).ZonePtr == state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum) {
                                    Real64 PeopleInZone =
                                        (state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                                         state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).Multiplier *
                                         state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).ListMultiplier);
                                    Real64 schMultiplier = ScheduleManager::LookUpScheduleValue(
                                        state, state.dataHeatBal->People(PeopleNum).NumberOfPeoplePtr, hrOfDay, TS);
                                    PeopleInZone = PeopleInZone * schMultiplier;
                                    TotConcurrentPeopleOnSys += PeopleInZone;
                                }
                            }
                        }
                        if (TotConcurrentPeopleOnSys >= state.dataSize->PsBySys(AirLoopNum)) {
                            state.dataSize->PsBySys(AirLoopNum) = TotConcurrentPeopleOnSys; // store max concurrent occupancy on system
                            // store timing description of Last occurrence of max
                            int Month(0);
                            int DayOfMonth(0);
                            General::InvOrdinalDay(DayLoop, Month, DayOfMonth, 1);
                            Real64 TimeHrsFraction = (double(hrOfDay) - 1.0) + double(TS) * TSfraction;
                            int TimeHrsInt = int(TimeHrsFraction);
                            int TimeMinsInt = nint((TimeHrsFraction - TimeHrsInt) * 60.0);
                            if (TimeMinsInt == 60) {
                                ++TimeHrsInt;
                                TimeMinsInt = 0;
                            }
                            state.dataSize->PeakPsOccurrenceDateTimeStringBySys(AirLoopNum) =
                                format("{:02}/{:02} {:02}:{:02}", Month, DayOfMonth, TimeHrsInt, TimeMinsInt);
                            state.dataSize->PeakPsOccurrenceEnvironmentStringBySys(AirLoopNum) = "Full Year Schedule";
                        }
                    } // if autosizied and VRP
                }     // air loops
            }
        }
    }

    // compute D for standard 62.1 by system
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        int SysSizNum = UtilityRoutines::FindItemInList(
            FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable

        // compute D if set to autosize
        if (SysSizInput(SysSizNum).OccupantDiversity == AutoSize) {
            if (state.dataSize->PzSumBySys(AirLoopNum) > 0.0) {
                state.dataSize->DBySys(AirLoopNum) = state.dataSize->PsBySys(AirLoopNum) / state.dataSize->PzSumBySys(AirLoopNum);
            } else {
                state.dataSize->DBySys(AirLoopNum) = 1.0;
            }
            state.dataSize->DBySys(AirLoopNum) = min(1.0, state.dataSize->DBySys(AirLoopNum));
        } else {
            // set the occupant diversity based on user-specified value
            state.dataSize->DBySys(AirLoopNum) = SysSizInput(SysSizNum).OccupantDiversity;
        }

        // For single zone systems, D should be 1.0.
        if (state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones == 1) {
            state.dataSize->DBySys(AirLoopNum) = 1.0;
            ShowWarningError(
                state,
                format("The {} air loop serves a single zone. The Occupant Diversity was calculated or set to a value less than 1.0. Single-zone air "
                       "loops should have an Occupant Diversity of 1.0. The Occupant Diversity value for that air loop has been reset to 1.0",
                       FinalSysSizing(AirLoopNum).AirPriLoopName));
        }
    }
}

void GetOARequirements(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad - FSEC
    //       DATE WRITTEN   February 2010

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the OA Requirements object and stores it in
    // appropriate data structure.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.
    // This object requires only a name where the default values are assumed
    // if subsequent fields are not entered.

    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMaxValue;

    static constexpr std::string_view RoutineName("GetOARequirements: "); // include trailing blank space

    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int TotalArgs;           // Total number of alpha and numeric arguments (max) for a
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // If errors detected in input

    std::string CurrentModuleObject; // for ease in getting objects
    Array1D_string Alphas;           // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D<Real64> Numbers;         // Numeric input items for object
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

    CurrentModuleObject = "DesignSpecification:OutdoorAir";
    int numOARequirements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
    std::string cCurrentModuleObject2 = "DesignSpecification:OutdoorAir:SpaceList";
    int numOARequirementsLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject2);
    state.dataSize->NumOARequirements = numOARequirements + numOARequirementsLists;

    Alphas.allocate(NumAlphas);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNumbers);
    Numbers.dimension(NumNumbers, 0.0);
    lAlphaBlanks.dimension(NumAlphas, true);
    lNumericBlanks.dimension(NumNumbers, true);

    if (state.dataSize->NumOARequirements > 0) {
        state.dataSize->OARequirements.allocate(state.dataSize->NumOARequirements);

        // Start Loading the System Input
        for (int OAIndex = 1; OAIndex <= numOARequirements; ++OAIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     OAIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataSize->OARequirements(OAIndex).Name = Alphas(1);

            ProcessInputOARequirements(state,
                                       CurrentModuleObject,
                                       OAIndex,
                                       Alphas,
                                       NumAlphas,
                                       Numbers,
                                       NumNumbers,
                                       lNumericBlanks,
                                       lAlphaBlanks,
                                       cAlphaFields,
                                       cNumericFields,
                                       ErrorsFound);
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        // DesignSpecification:OutdoorAir:SpaceList
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto const instances = ip->epJSON.find(cCurrentModuleObject2);
        if (instances != ip->epJSON.end()) {
            auto const &objectSchemaProps = ip->getObjectSchemaProps(state, cCurrentModuleObject2);
            auto &instancesValue = instances.value();
            int oaIndex = numOARequirements; // add lists to the end of the same array

            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                ++oaIndex;
                auto const &objectFields = instance.value();
                auto &thisOAReq = state.dataSize->OARequirements(oaIndex);
                ip->markObjectAsUsed(cCurrentModuleObject2, instance.key());
                std::string thisOAReqName = UtilityRoutines::MakeUPPERCase(instance.key());

                if (UtilityRoutines::FindItemInList(thisOAReqName, state.dataSize->OARequirements) > 0) {
                    ShowSevereError(state,
                                    std::string(RoutineName) + cCurrentModuleObject2 + "=\"" + thisOAReqName +
                                        "\" is a duplicate DesignSpecification:OutdoorAir name.");
                    ErrorsFound = true;
                }
                thisOAReq.Name = thisOAReqName;

                // List of spaces and DSOA names
                thisOAReq.numDSOA = 0;
                auto extensibles = objectFields.find("space_specs");
                auto const &extensionSchemaProps = objectSchemaProps["space_specs"]["items"]["properties"];
                if (extensibles != objectFields.end()) {
                    auto extensiblesArray = extensibles.value();
                    for (auto extensibleInstance : extensiblesArray) {
                        // Zones and spaces are not created yet, validate space names in ZoneEquipmentManager::SetupZoneSizingArrays
                        std::string thisSpaceName = ip->getAlphaFieldValue(extensibleInstance, extensionSchemaProps, "space_name");
                        thisOAReq.dsoaSpaceNames.emplace_back(thisSpaceName);
                        std::string thisDsoaName =
                            ip->getAlphaFieldValue(extensibleInstance, extensionSchemaProps, "space_design_specification_outdoor_air_object_name");
                        int thisDsoaNum = UtilityRoutines::FindItemInList(thisDsoaName, state.dataSize->OARequirements, oaIndex);
                        if (thisDsoaNum > 0) {
                            thisOAReq.dsoaIndexes.emplace_back(thisDsoaNum);
                            ++thisOAReq.numDSOA;
                        } else {
                            ShowSevereError(state, std::string(RoutineName) + cCurrentModuleObject2 + "=" + thisOAReq.Name);
                            ShowContinueError(state, "DesignSpecification:OutdoorAir=" + thisDsoaName + " not found.");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            if (ErrorsFound) {
                ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
            }
        }
    }
}

void ProcessInputOARequirements(EnergyPlusData &state,
                                std::string const &CurrentModuleObject,
                                int const OAIndex,
                                Array1D_string const &Alphas,
                                int &NumAlphas,
                                Array1D<Real64> const &Numbers,
                                int &NumNumbers,
                                [[maybe_unused]] Array1D_bool const &lNumericBlanks, // Unused
                                Array1D_bool const &lAlphaBlanks,
                                Array1D_string const &cAlphaFields,
                                [[maybe_unused]] Array1D_string const &cNumericFields, // Unused
                                bool &ErrorsFound                                      // If errors found in input
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad - FSEC
    //       DATE WRITTEN   February 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the OA Requirements object and stores it in
    // appropriate data structure.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.
    // This object requires only a name where the default values are assumed
    // if subsequent fields are not entered.

    // REFERENCES:
    // na

    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMaxValue;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetOARequirements: "); // include trailing blank space

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    if (NumAlphas > 1) {
        if (UtilityRoutines::SameString(Alphas(2), "Flow/Person")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowPPer;
        } else if (UtilityRoutines::SameString(Alphas(2), "Flow/Zone")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlow;
        } else if (UtilityRoutines::SameString(Alphas(2), "Flow/Area")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowPerArea;
        } else if (UtilityRoutines::SameString(Alphas(2), "AirChanges/Hour")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowACH;
        } else if (UtilityRoutines::SameString(Alphas(2), "Sum")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowSum;
        } else if (UtilityRoutines::SameString(Alphas(2), "Maximum")) {
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowMax;
        } else if (UtilityRoutines::SameString(Alphas(2),
                                               "INDOORAIRQUALITYPROCEDURE")) { // Indoor Air Quality Procedure based on ASHRAE Standard 62.1-2007
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = ZOAM_IAQP;
        } else if (UtilityRoutines::SameString(
                       Alphas(2), "PROPORTIONALCONTROLBASEDONOCCUPANCYSCHEDULE")) { // Proportional Control based on ASHRAE Standard 62.1-2004
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = ZOAM_ProportionalControlSchOcc;
        } else if (UtilityRoutines::SameString(
                       Alphas(2), "PROPORTIONALCONTROLBASEDONDESIGNOCCUPANCY")) { // Proportional Control based on ASHRAE Standard 62.1-2004
            state.dataSize->OARequirements(OAIndex).OAFlowMethod = ZOAM_ProportionalControlDesOcc;
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->OARequirements(OAIndex).Name + "\",");
            ShowContinueError(state, "...Invalid " + cAlphaFields(2) + "=\"" + Alphas(2) + "\",");
            ShowContinueError(state,
                              "...Valid choices are Flow/Person, Flow/Zone, Flow/Area, AirChanges/Hour, Sum, Maximum, IndoorAirQualityProcedure, "
                              "ProportionalControlBasedOnDesignOccupancy, and ProportionalControlBasedOnOccupancySchedule.");
            ErrorsFound = true;
        }
    } else {
        // default value for Outdoor Air Method
        state.dataSize->OARequirements(OAIndex).OAFlowMethod = OAFlowPPer;
    }
    if (NumNumbers > 0) {
        state.dataSize->OARequirements(OAIndex).OAFlowPerPerson = Numbers(1);
    } else {
        // default value for Outdoor Air Flow per Person when per person flow is counted
        state.dataSize->OARequirements(OAIndex).OAFlowPerPerson = 0.00944;
    }
    // if one of the methods that should not use the flow per person field is chosen then zero out the flow per person to avoid it
    // being counted later #4378
    if (state.dataSize->OARequirements(OAIndex).OAFlowMethod != OAFlowPPer && state.dataSize->OARequirements(OAIndex).OAFlowMethod != OAFlowSum &&
        state.dataSize->OARequirements(OAIndex).OAFlowMethod != OAFlowMax &&
        state.dataSize->OARequirements(OAIndex).OAFlowMethod != ZOAM_ProportionalControlSchOcc &&
        state.dataSize->OARequirements(OAIndex).OAFlowMethod != ZOAM_ProportionalControlDesOcc &&
        state.dataSize->OARequirements(OAIndex).OAFlowMethod != ZOAM_IAQP) {
        state.dataSize->OARequirements(OAIndex).OAFlowPerPerson = 0.0;
    }
    // remaining fields default to 0
    if (NumNumbers > 1) {
        if (state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowPerArea ||
            state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowSum || state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowMax) {
            state.dataSize->OARequirements(OAIndex).OAFlowPerArea = Numbers(2);
        } else if (state.dataSize->OARequirements(OAIndex).OAFlowMethod == ZOAM_ProportionalControlSchOcc ||
                   state.dataSize->OARequirements(OAIndex).OAFlowMethod == ZOAM_ProportionalControlDesOcc ||
                   state.dataSize->OARequirements(OAIndex).OAFlowMethod == ZOAM_IAQP) {
            state.dataSize->OARequirements(OAIndex).OAFlowPerArea = Numbers(2);
        } else {
            state.dataSize->OARequirements(OAIndex).OAFlowPerArea = 0.0;
        }
    }
    if (NumNumbers > 2) {
        if (state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlow || state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowSum ||
            state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowMax || state.dataSize->OARequirements(OAIndex).OAFlowMethod == ZOAM_IAQP) {
            state.dataSize->OARequirements(OAIndex).OAFlowPerZone = Numbers(3);
        } else {
            state.dataSize->OARequirements(OAIndex).OAFlowPerZone = 0.0;
        }
    }

    if (NumNumbers > 3) {
        if (state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowACH || state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowSum ||
            state.dataSize->OARequirements(OAIndex).OAFlowMethod == OAFlowMax || state.dataSize->OARequirements(OAIndex).OAFlowMethod == ZOAM_IAQP) {
            state.dataSize->OARequirements(OAIndex).OAFlowACH = Numbers(4);
        } else {
            state.dataSize->OARequirements(OAIndex).OAFlowACH = 0.0;
        }
    }

    // Set default schedule
    state.dataSize->OARequirements(OAIndex).OAFlowFracSchPtr = DataGlobalConstants::ScheduleAlwaysOn;
    if (NumAlphas > 2) {
        if (!lAlphaBlanks(3)) {
            state.dataSize->OARequirements(OAIndex).OAFlowFracSchPtr = GetScheduleIndex(state, Alphas(3));
            if (state.dataSize->OARequirements(OAIndex).OAFlowFracSchPtr > 0) {
                if (!CheckScheduleValueMinMax(state, state.dataSize->OARequirements(OAIndex).OAFlowFracSchPtr, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->OARequirements(OAIndex).Name + "\",");
                    ShowContinueError(state, "Error found in " + cAlphaFields(3) + " = " + Alphas(3));
                    ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->OARequirements(OAIndex).Name + "\",");
                ShowContinueError(state, "...Not Found " + cAlphaFields(3) + "=\"" + Alphas(3) + "\".");
                ErrorsFound = true;
            }
        }
    }

    if (NumAlphas > 3) {
        if (!lAlphaBlanks(4)) {
            state.dataSize->OARequirements(OAIndex).OAPropCtlMinRateSchPtr = GetScheduleIndex(state, Alphas(4));
            if (state.dataSize->OARequirements(OAIndex).OAPropCtlMinRateSchPtr > 0) {
                if (!CheckScheduleValueMinMax(state, state.dataSize->OARequirements(OAIndex).OAPropCtlMinRateSchPtr, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->OARequirements(OAIndex).Name + "\",");
                    ShowContinueError(state, "Error found in " + cAlphaFields(4) + " = " + Alphas(4));
                    ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->OARequirements(OAIndex).Name + "\",");
                ShowContinueError(state, "...Not Found " + cAlphaFields(4) + "=\"" + Alphas(4) + "\".");
                ErrorsFound = true;
            }
        }
    }
}

void GetZoneAirDistribution(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         T. Hong - LBNL
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the zone air distribution objects and stores it in
    // appropriate data structure.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.
    // This object requires only a name where the default values are assumed
    // if subsequent fields are not entered.

    // Using/Aliasing
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetZoneAirDistribution: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;  // Number of Alphas for each GetObjectItem call
    int NumNumbers; // Number of Numbers for each GetObjectItem call
    int TotalArgs;  // Total number of alpha and numeric arguments (max) for a
    int IOStatus;   // Used in GetObjectItem
    int ZADIndex;
    bool ErrorsFound(false); // If errors detected in input

    std::string CurrentModuleObject; // for ease in getting objects
    Array1D_string Alphas;           // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D<Real64> Numbers;         // Numeric input items for object
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

    CurrentModuleObject = "DesignSpecification:ZoneAirDistribution";
    state.dataSize->NumZoneAirDistribution = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);

    Alphas.allocate(NumAlphas);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNumbers);
    Numbers.dimension(NumNumbers, 0.0);
    lAlphaBlanks.dimension(NumAlphas, true);
    lNumericBlanks.dimension(NumNumbers, true);

    if (state.dataSize->NumZoneAirDistribution > 0) {
        state.dataSize->ZoneAirDistribution.allocate(state.dataSize->NumZoneAirDistribution);

        // Start Loading the zone air distribution input
        for (ZADIndex = 1; ZADIndex <= state.dataSize->NumZoneAirDistribution; ++ZADIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     ZADIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataSize->ZoneAirDistribution(ZADIndex).Name = Alphas(1);

            // Zone Air Distribution Effectiveness in Cooling Mode
            if (NumNumbers > 0) {
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffCooling = Numbers(1);
            } else {
                // default value
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffCooling = 1.0;
            }

            // Zone Air Distribution Effectiveness in Heating Mode
            if (NumNumbers > 1) {
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffHeating = Numbers(2);
            } else {
                // default value
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffHeating = 1.0;
            }

            // Zone Secondary Recirculation Fraction
            if (NumNumbers > 2) {
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneSecondaryRecirculation = Numbers(3);
            } else {
                // default value
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneSecondaryRecirculation = 0.0;
            }

            // Zone Ventilation Efficiency
            if (NumNumbers > 3) {
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneVentilationEff = Numbers(4);
            } else {
                // default value
                state.dataSize->ZoneAirDistribution(ZADIndex).ZoneVentilationEff = 0.0;
            }

            if (NumAlphas > 1) {
                if (!lAlphaBlanks(2)) {
                    state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffSchName = Alphas(2);
                    state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffSchPtr = GetScheduleIndex(state, Alphas(2));
                    if (state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffSchPtr > 0) {
                        if (!CheckScheduleValueMinMax(state, state.dataSize->ZoneAirDistribution(ZADIndex).ZoneADEffSchPtr, ">", 0.0)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                state.dataSize->ZoneAirDistribution(ZADIndex).Name + "\",");
                            ShowContinueError(state, "Error found in " + cAlphaFields(2) + " = " + Alphas(2));
                            ShowContinueError(state, "Schedule values must be >0.0)");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataSize->ZoneAirDistribution(ZADIndex).Name +
                                            "\",");
                        ShowContinueError(state, "...Not Found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }
}

void GetSizingParams(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the Sizing Parameters object and stores it in
    // appropriate data structure.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;  // Number of Alphas for each GetObjectItem call
    int NumNumbers; // Number of Numbers for each GetObjectItem call
    int IOStatus;   // Used in GetObjectItem
    int NumSizParams;
    int Temp;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Sizing:Parameters";
    NumSizParams = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (NumSizParams == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (state.dataIPShortCut->lNumericFieldBlanks(1) || state.dataIPShortCut->rNumericArgs(1) < 0.0) {
            state.dataSize->GlobalHeatSizingFactor = 1.0;
        } else {
            state.dataSize->GlobalHeatSizingFactor = state.dataIPShortCut->rNumericArgs(1);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(2) || state.dataIPShortCut->rNumericArgs(2) < 0.0) {
            state.dataSize->GlobalCoolSizingFactor = 1.0;
        } else {
            state.dataSize->GlobalCoolSizingFactor = state.dataIPShortCut->rNumericArgs(2);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(3) || state.dataIPShortCut->rNumericArgs(3) <= 0.0) {
            state.dataSize->NumTimeStepsInAvg = state.dataGlobal->NumOfTimeStepInHour;
        } else {
            state.dataSize->NumTimeStepsInAvg = int(state.dataIPShortCut->rNumericArgs(3));
        }
    } else if (NumSizParams == 0) {
        state.dataSize->GlobalHeatSizingFactor = 1.0;
        state.dataSize->GlobalCoolSizingFactor = 1.0;
        state.dataSize->NumTimeStepsInAvg = state.dataGlobal->NumOfTimeStepInHour;
    } else {
        ShowFatalError(state, cCurrentModuleObject + ": More than 1 occurrence of this object; only 1 allowed");
    }

    if (state.dataSize->NumTimeStepsInAvg < state.dataGlobal->NumOfTimeStepInHour) {
        ShowWarningError(state,
                         format("{}: note {} entered value=[{}] is less than 1 hour (i.e., {} timesteps).",
                                cCurrentModuleObject,
                                state.dataIPShortCut->cNumericFieldNames(3),
                                state.dataSize->NumTimeStepsInAvg,
                                state.dataGlobal->NumOfTimeStepInHour));
    }

    cCurrentModuleObject = "OutputControl:Sizing:Style";
    Temp = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (Temp == 0) {
        state.dataIPShortCut->cAlphaArgs(1) = "Comma";
        state.dataSize->SizingFileColSep = CharComma; // comma
    } else if (Temp == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (state.dataIPShortCut->cAlphaArgs(1) == "COMMA") {
            state.dataSize->SizingFileColSep = CharComma; // comma
            state.dataIPShortCut->cAlphaArgs(1) = "Comma";
        } else if (state.dataIPShortCut->cAlphaArgs(1) == "TAB") {
            state.dataSize->SizingFileColSep = CharTab; // tab
            state.dataIPShortCut->cAlphaArgs(1) = "Tab";
        } else if (state.dataIPShortCut->cAlphaArgs(1) == "FIXED" || state.dataIPShortCut->cAlphaArgs(1) == "SPACE") {
            state.dataSize->SizingFileColSep = CharSpace; // space
            state.dataIPShortCut->cAlphaArgs(1) = "Space";
        } else {
            state.dataSize->SizingFileColSep = CharComma; // comma
            ShowWarningError(state,
                             cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + " entered value=\"" +
                                 state.dataIPShortCut->cAlphaArgs(1) + "\", Commas will be used to separate fields.");
            state.dataIPShortCut->cAlphaArgs(1) = "Comma";
        }
        print(state.files.eio, "! <Sizing Output Files>,Style\n");
        print(state.files.eio, "Sizing Output Files,{}\n", state.dataIPShortCut->cAlphaArgs(1));
    }
}

void GetZoneSizingInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       Mangesh Basarkar, 06/2011: Specifying zone outside air based on design specification object
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for zone sizing objects and stores it in
    // appropriate data structures.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneSizIndex;        // loop index
    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    int NumDesDays;          // Number of design days in input
    int NumSizingZoneStatements;
    int Item;
    int Item1;
    int ZLItem;
    bool errFlag;
    Array1D_string ZoneNames;
    int NumZones;
    int NumZoneLists;
    int OAIndex;  // Index of design specification object
    int ObjIndex; // Index of zone air distribution effectiveness object name
    bool DesHeatMaxAirFlowPerAreaUsrInp;
    bool DesHeatMaxAirFlowUsrInp;
    bool DesHeatMaxAirFlowFracUsrInp;

    struct GlobalMiscObject
    {
        // Members
        std::string Name;
        int ZoneOrZoneListPtr;
        int NumOfZones;
        int StartPtr;
        bool ZoneListActive;

        // Default Constructor
        GlobalMiscObject() : ZoneOrZoneListPtr(0), NumOfZones(0), StartPtr(0), ZoneListActive(false)
        {
        }
    };

    // Object Data
    Array1D<ZoneListData> ZoneListNames;
    Array1D<GlobalMiscObject> SizingZoneObjects;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Sizing:Zone";
    NumSizingZoneStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    SizingZoneObjects.allocate(NumSizingZoneStatements);

    if (NumSizingZoneStatements > 0) {
        errFlag = false;
        GetZoneAndZoneListNames(state, errFlag, NumZones, ZoneNames, NumZoneLists, ZoneListNames);
    }

    cCurrentModuleObject = "Sizing:Zone";
    state.dataSize->NumZoneSizingInput = 0;
    errFlag = false;
    for (Item = 1; Item <= NumSizingZoneStatements; ++Item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Item,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        SizingZoneObjects(Item).Name = state.dataIPShortCut->cAlphaArgs(1);

        Item1 = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), ZoneNames, NumZones);
        ZLItem = 0;
        if (Item1 == 0 && NumZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), ZoneListNames);
        if (Item1 > 0) {
            SizingZoneObjects(Item).StartPtr = state.dataSize->NumZoneSizingInput + 1;
            ++state.dataSize->NumZoneSizingInput;
            SizingZoneObjects(Item).NumOfZones = 1;
            SizingZoneObjects(Item).ZoneListActive = false;
            SizingZoneObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            SizingZoneObjects(Item).StartPtr = state.dataSize->NumZoneSizingInput + 1;
            state.dataSize->NumZoneSizingInput += ZoneListNames(ZLItem).NumOfZones;
            SizingZoneObjects(Item).NumOfZones = ZoneListNames(ZLItem).NumOfZones;
            SizingZoneObjects(Item).ZoneListActive = true;
            SizingZoneObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(state,
                            cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(1) + " not found.");
            ErrorsFound = true;
            errFlag = true;
        }
    }

    if (errFlag) {
        ShowSevereError(state, "GetZoneSizingInput: Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataSize->NumZoneSizingInput = 0;
    }

    if (state.dataSize->NumZoneSizingInput > 0) {
        NumDesDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        if (NumDesDays == 0 && (state.dataGlobal->DoZoneSizing || state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing)) {
            ShowSevereError(state, "Zone Sizing calculations need SizingPeriod:* input. None found.");
            ErrorsFound = true;
        }
        state.dataSize->ZoneSizingInput.allocate(state.dataSize->NumZoneSizingInput);

        ZoneSizIndex = 0;
        for (Item = 1; Item <= NumSizingZoneStatements; ++Item) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            for (Item1 = 1; Item1 <= SizingZoneObjects(Item).NumOfZones; ++Item1) {
                ++ZoneSizIndex;
                if (!SizingZoneObjects(Item).ZoneListActive) {
                    if (SizingZoneObjects(Item).ZoneOrZoneListPtr > 0) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName = ZoneNames(SizingZoneObjects(Item).ZoneOrZoneListPtr);
                    } else {
                        // Invalid zone, will be caught later
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName = "Invalid Zone Name";
                    }
                } else { // Zone list active
                    if (SizingZoneObjects(Item).ZoneOrZoneListPtr > 0 && ZoneListNames(SizingZoneObjects(Item).ZoneOrZoneListPtr).Zones(Item1) > 0) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName =
                            ZoneNames(ZoneListNames(SizingZoneObjects(Item).ZoneOrZoneListPtr).Zones(Item1));
                    } else {
                        // Invalid zone, will be caught later
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName = "Invalid Zone Name";
                    }
                }
                bool const nameEmpty = UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                if (nameEmpty && !SizingZoneObjects(Item).ZoneListActive) {
                    ShowContinueError(state, "Zone may have been entered in a ZoneList assignment.");
                }

                //  A2, \field Zone Cooling Design Supply Air Temperature Input Method
                //      \required-field
                //      \type choice
                //      \key SupplyAirTemperature
                //      \key TemperatureDifference
                //      \default SupplyAirTemperature
                {
                    auto const coolingSATMethod(state.dataIPShortCut->cAlphaArgs(2));
                    if (coolingSATMethod == "SUPPLYAIRTEMPERATURE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnCoolDgnSAMethod = SupplyAirTemperature;
                    } else if (coolingSATMethod == "TEMPERATUREDIFFERENCE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnCoolDgnSAMethod = TemperatureDifference;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(
                            state, "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\"");
                        ShowContinueError(state, "... valid values are SupplyAirTemperature or TemperatureDifference.");
                        ErrorsFound = true;
                    }
                }
                //  N1, \field Zone Cooling Design Supply Air Temperature
                //      \type real
                //      \units C
                //      \note Zone Cooling Design Supply Air Temperature is only used when Zone Cooling Design
                //      \note Supply Air Temperature Input Method = SupplyAirTemperature
                Real64 lowTempLimit = 0.0;
                if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTemp = 0.0;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnCoolDgnSAMethod == SupplyAirTemperature) {
                    ReportTemperatureInputError(state, cCurrentModuleObject, 1, lowTempLimit, false, ErrorsFound);
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTemp = state.dataIPShortCut->rNumericArgs(1);
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTemp = 0.0;
                }
                //  N2, \field Zone Cooling Design Supply Air Temperature Difference
                //      \type real
                //      \units delta C
                //      \note Zone Cooling Design Supply Air Temperature is only used when Zone Cooling Design
                //      \note Supply Air Temperature Input Method = TemperatureDifference
                //      \note The absolute of this value is value will be subtracted from room temperature
                //      \note at peak load to calculate Zone Cooling Design Supply Air Temperature.
                if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTempDiff = 0.0;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnCoolDgnSAMethod == TemperatureDifference) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTempDiff = state.dataIPShortCut->rNumericArgs(2);
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTempDiff = 0.0;
                }
                //  A3, \field Zone Heating Design Supply Air Temperature Input Method
                //      \required-field
                //      \type choice
                //      \key SupplyAirTemperature
                //      \key TemperatureDifference
                //      \default SupplyAirTemperature
                {
                    auto const heatingSATMethod(state.dataIPShortCut->cAlphaArgs(3));
                    if (heatingSATMethod == "SUPPLYAIRTEMPERATURE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnHeatDgnSAMethod = SupplyAirTemperature;
                    } else if (heatingSATMethod == "TEMPERATUREDIFFERENCE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnHeatDgnSAMethod = TemperatureDifference;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(
                            state, "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\"");
                        ShowContinueError(state, "... valid values are SupplyAirTemperature or TemperatureDifference.");
                        ErrorsFound = true;
                    }
                }
                //  N3, \field Zone Heating Design Supply Air Temperature
                //      \type real
                //      \units C
                //      \note Zone Heating Design Supply Air Temperature is only used when Zone Heating Design
                //      \note Supply Air Temperature Input Method = SupplyAirTemperature
                if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTemp = 0.0;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnHeatDgnSAMethod == SupplyAirTemperature) {
                    ReportTemperatureInputError(state, cCurrentModuleObject, 3, lowTempLimit, false, ErrorsFound);
                    ReportTemperatureInputError(
                        state, cCurrentModuleObject, 3, state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesTemp, true, ErrorsFound);
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTemp = state.dataIPShortCut->rNumericArgs(3);
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTemp = 0.0;
                }
                //  N4, \field Zone Heating Design Supply Air Temperature Difference
                //      \type real
                //      \units deltaC
                //      \note Zone Heating Design Supply Air Temperature is only used when Zone Heating Design
                //      \note Supply Air Temperature Input Method = TemperatureDifference
                //      \note The absolute of this value is value will be added to room temperature
                //      \note at peak load to calculate Zone Heating Design Supply Air Temperature.
                if (state.dataIPShortCut->lNumericFieldBlanks(4)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTempDiff = 0.0;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).ZnHeatDgnSAMethod == TemperatureDifference) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTempDiff = state.dataIPShortCut->rNumericArgs(4);
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesTempDiff = 0.0;
                }
                //  N5, \field Zone Cooling Design Supply Air Humidity Ratio
                //      \required-field
                //      \minimum 0.0
                //      \type real
                //      \units kgWater/kgDryAir
                if (state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesHumRat = 0.0;
                } else if (state.dataIPShortCut->rNumericArgs(5) < 0.0) {
                    ShowSevereError(state,
                                    format("{}: incorrect {}: {:.2R}",
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cNumericFieldNames(5),
                                           state.dataIPShortCut->rNumericArgs(5)));
                    ShowContinueError(state, ".. value should not be negative. Occurs in Sizing Object=" + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolDesHumRat = state.dataIPShortCut->rNumericArgs(5);
                }
                //  N6, \field Zone Heating Design Supply Air Humidity Ratio
                //      \required-field
                //      \minimum 0.0
                //      \type real
                //      \units kgWater/kgDryAir
                if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesHumRat = 0.0;
                } else if (state.dataIPShortCut->rNumericArgs(6) < 0.0) {
                    ShowSevereError(state,
                                    format("{}: incorrect {}: {:.2R}",
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cNumericFieldNames(6),
                                           state.dataIPShortCut->rNumericArgs(6)));
                    ShowContinueError(state, ".. value should not be negative. Occurs in Sizing Object=" + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatDesHumRat = state.dataIPShortCut->rNumericArgs(6);
                }
                //  A4, \field Design Specification Outdoor Air Object Name
                //      \type object-list
                //      \object-list DesignSpecificationOutdoorAirNames
                state.dataSize->ZoneSizingInput(ZoneSizIndex).DesignSpecOAObjName = state.dataIPShortCut->cAlphaArgs(4);

                // Getting zone OA parameters from Design Specification object
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    OAIndex = UtilityRoutines::FindItemInList(state.dataSize->ZoneSizingInput(ZoneSizIndex).DesignSpecOAObjName,
                                                              state.dataSize->OARequirements);
                    if (OAIndex > 0) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneDesignSpecOAIndex = OAIndex;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(state,
                                          "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                              "\".");
                        ErrorsFound = true;
                    }
                } else { // If no design spec object specified, i.e. no OA, then leave ZoneDesignSpecOAIndex = 0
                }

                //  N7, \field Zone Heating Sizing Factor
                //      \note if blank, global heating sizing factor from Sizing:Parameters is used.
                //      \minimum> 0
                if (state.dataIPShortCut->lNumericFieldBlanks(7) || state.dataIPShortCut->rNumericArgs(7) == 0.0) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatSizingFactor = state.dataSize->GlobalHeatSizingFactor;
                } else if (state.dataIPShortCut->rNumericArgs(7) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(7),
                                             state.dataIPShortCut->rNumericArgs(7)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatSizingFactor = state.dataIPShortCut->rNumericArgs(7);
                }
                //  N8, \field Zone Cooling Sizing Factor
                //      \note if blank, global cooling sizing factor from Sizing:Parameters is used.
                //      \minimum> 0
                if (state.dataIPShortCut->lNumericFieldBlanks(8) || state.dataIPShortCut->rNumericArgs(8) == 0.0) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolSizingFactor = state.dataSize->GlobalCoolSizingFactor;
                } else if (state.dataIPShortCut->rNumericArgs(8) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(8),
                                             state.dataIPShortCut->rNumericArgs(8)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolSizingFactor = state.dataIPShortCut->rNumericArgs(8);
                }
                //  N9, \field Cooling Design Air Flow Rate
                //      \type real
                //      \units m3/s
                //      \minimum 0
                //      \default 0
                //      \note This input is used if Cooling Design Air Flow Method is Flow/Zone
                //      \note This value will be multiplied by the global or zone sizing factor and
                //      \note by zone multipliers.
                if (state.dataIPShortCut->lNumericFieldBlanks(9)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolAirFlow = 0.0;
                } else if (state.dataIPShortCut->rNumericArgs(9) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(9),
                                             state.dataIPShortCut->rNumericArgs(9)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolAirFlow = state.dataIPShortCut->rNumericArgs(9);
                }
                //  N10,\field Cooling Minimum Air Flow per Zone Floor Area
                //      \type real
                //      \units m3/s-m2
                //      \minimum 0
                //      \default .000762
                //      \note default is .15 cfm/ft2
                //      \note This input is used if Cooling Design Air Flow Method is design day with limit
                if (state.dataIPShortCut->lNumericFieldBlanks(10)) {
                    if (state.dataIPShortCut->rNumericArgs(10) <= 0.0) { // in case someone changes the default in the IDD
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlowPerArea = 0.000762;
                    } else {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlowPerArea = state.dataIPShortCut->rNumericArgs(10);
                    }
                } else if (state.dataIPShortCut->rNumericArgs(10) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(108),
                                             state.dataIPShortCut->rNumericArgs(10)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlowPerArea = state.dataIPShortCut->rNumericArgs(10);
                }
                //  N11,\field Cooling Minimum Air Flow
                //      \type real
                //      \units m3/s
                //      \minimum 0
                //      \default 0
                //      \note This input is used if Cooling Design Air Flow Method is design day with limit
                if (state.dataIPShortCut->lNumericFieldBlanks(11)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlow = 0.0;
                } else if (state.dataIPShortCut->rNumericArgs(11) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(11),
                                             state.dataIPShortCut->rNumericArgs(11)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlow = state.dataIPShortCut->rNumericArgs(11);
                }
                //  N12,\field Cooling Minimum Air Flow Fraction
                if (state.dataIPShortCut->rNumericArgs(12) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(12),
                                             state.dataIPShortCut->rNumericArgs(12)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesCoolMinAirFlowFrac = state.dataIPShortCut->rNumericArgs(12);
                }

                //  N13,\field Heating Design Air Flow Rate
                //      \type real
                //      \units m3/s
                //      \minimum 0
                //      \default 0
                //      \note This input is used if Heating Design Air Flow Method is Flow/Zone.
                //      \note This value will be multiplied by the global or zone sizing factor and
                //      \note by zone multipliers.
                if (state.dataIPShortCut->lNumericFieldBlanks(13)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatAirFlow = 0.0;
                } else if (state.dataIPShortCut->rNumericArgs(13) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(13),
                                             state.dataIPShortCut->rNumericArgs(13)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatAirFlow = state.dataIPShortCut->rNumericArgs(13);
                }
                //  N14,\field Heating Maximum Air Flow per Zone Floor Area
                //      \type real
                //      \units m3/s-m2
                //      \minimum 0
                //      \default .002032
                //      \note default is .40 cfm/ft2
                //      \note This input is not currently used for autosizing any of the components.
                DesHeatMaxAirFlowPerAreaUsrInp = false;
                if (state.dataIPShortCut->lNumericFieldBlanks(14)) {
                    if (state.dataIPShortCut->rNumericArgs(14) <= 0.0) { // in case someone changes the default in the IDD
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowPerArea = 0.002032;
                    } else {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowPerArea = state.dataIPShortCut->rNumericArgs(14);
                    }
                } else if (state.dataIPShortCut->rNumericArgs(14) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(14),
                                             state.dataIPShortCut->rNumericArgs(14)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowPerArea = state.dataIPShortCut->rNumericArgs(14);
                    DesHeatMaxAirFlowPerAreaUsrInp = true;
                }
                //  N15,\field Heating Maximum Air Flow
                //      \type real
                //      \units m3/s
                //      \minimum 0
                //      \default .1415762
                //      \note default is 300 cfm
                //      \note This input is not currently used for autosizing any of the components.
                DesHeatMaxAirFlowUsrInp = false;
                if (state.dataIPShortCut->lNumericFieldBlanks(15)) {
                    if (state.dataIPShortCut->rNumericArgs(15) <= 0.0) { // in case someone changes the default in the IDD
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlow = 0.1415762;
                    } else {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlow = state.dataIPShortCut->rNumericArgs(15);
                    }
                } else if (state.dataIPShortCut->rNumericArgs(15) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(15),
                                             state.dataIPShortCut->rNumericArgs(15)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlow = state.dataIPShortCut->rNumericArgs(15);
                    DesHeatMaxAirFlowUsrInp = true;
                }
                //  N16;\field Heating Maximum Air Flow Fraction
                //      \note fraction of the Heating Design Air Flow Rate
                //      \note This input is not currently used for autosizing any of the components.
                //      \type real
                //      \minimum 0
                //      \default 0.3
                DesHeatMaxAirFlowFracUsrInp = false;
                if (state.dataIPShortCut->lNumericFieldBlanks(16)) {
                    if (state.dataIPShortCut->rNumericArgs(16) <= 0.0) { // in case someone changes the default in the IDD
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowFrac = 0.3;
                    } else {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowFrac = state.dataIPShortCut->rNumericArgs(16);
                    }
                } else if (state.dataIPShortCut->rNumericArgs(16) < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state,
                                      format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                             state.dataIPShortCut->cNumericFieldNames(16),
                                             state.dataIPShortCut->rNumericArgs(16)));
                    ErrorsFound = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowFrac = state.dataIPShortCut->rNumericArgs(16);
                    DesHeatMaxAirFlowFracUsrInp = true;
                }
                // make sure the user specified inputs of the previous 3 inputs override the defaults
                if (DesHeatMaxAirFlowPerAreaUsrInp || DesHeatMaxAirFlowUsrInp || DesHeatMaxAirFlowFracUsrInp) {
                    if (!DesHeatMaxAirFlowPerAreaUsrInp) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowPerArea = 0.0;
                    }
                    if (!DesHeatMaxAirFlowUsrInp) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlow = 0.0;
                    }
                    if (!DesHeatMaxAirFlowFracUsrInp) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DesHeatMaxAirFlowFrac = 0.0;
                    }
                }

                //  A7, \field Zone Air Distribution Object Name and add its inputs
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneAirDistEffObjName = state.dataIPShortCut->cAlphaArgs(7);
                    ObjIndex = UtilityRoutines::FindItemInList(state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneAirDistEffObjName,
                                                               state.dataSize->ZoneAirDistribution);
                    if (ObjIndex > 0) {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneADEffCooling =
                            state.dataSize->ZoneAirDistribution(ObjIndex).ZoneADEffCooling;
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneADEffHeating =
                            state.dataSize->ZoneAirDistribution(ObjIndex).ZoneADEffHeating;
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneSecondaryRecirculation =
                            state.dataSize->ZoneAirDistribution(ObjIndex).ZoneSecondaryRecirculation;
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneAirDistributionIndex = ObjIndex;
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneVentilationEff =
                            state.dataSize->ZoneAirDistribution(ObjIndex).ZoneVentilationEff;
                    } else {
                        // generate a warning message
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(state,
                                          "... not found " + state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) +
                                              "\".");
                        ErrorsFound = true;
                    }
                } else {
                    // assume defaults
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneADEffCooling = 1.0;
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneADEffHeating = 1.0;
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneSecondaryRecirculation = 0.0;
                }

                {
                    auto const coolAirDesMethod(state.dataIPShortCut->cAlphaArgs(5));
                    if (coolAirDesMethod == "DESIGNDAY") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolAirDesMethod = FromDDCalc;
                    } else if (coolAirDesMethod == "FLOW/ZONE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolAirDesMethod = InpDesAirFlow;
                    } else if (coolAirDesMethod == "DESIGNDAYWITHLIMIT") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolAirDesMethod = DesAirFlowWithLim;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(state,
                                          "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) +
                                              "\".");
                        ShowContinueError(state, "... valid values are DesignDay, Flow/Zone or DesignDayWithLimit.");
                        ErrorsFound = true;
                    }
                }
                {
                    auto const heatAirDesMethod(state.dataIPShortCut->cAlphaArgs(6));
                    if (heatAirDesMethod == "DESIGNDAY") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatAirDesMethod = FromDDCalc;
                    } else if (heatAirDesMethod == "FLOW/ZONE") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatAirDesMethod = InpDesAirFlow;
                    } else if (heatAirDesMethod == "DESIGNDAYWITHLIMIT") {
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatAirDesMethod = DesAirFlowWithLim;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(state,
                                          "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + state.dataIPShortCut->cAlphaArgs(6) +
                                              "\".");
                        ShowContinueError(state, "... valid values are DesignDay, Flow/Zone or DesignDayWithLimit.");
                        ErrorsFound = true;
                    }
                }
                if (state.dataIPShortCut->cAlphaArgs(8) == "YES") {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).AccountForDOAS = true;
                } else {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).AccountForDOAS = false;
                }
                if (state.dataSize->ZoneSizingInput(ZoneSizIndex).AccountForDOAS) {
                    {
                        auto const DOASControlMethod(state.dataIPShortCut->cAlphaArgs(9));
                        if (DOASControlMethod == "NEUTRALSUPPLYAIR") {
                            state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy = DOANeutralSup;
                        } else if (DOASControlMethod == "NEUTRALDEHUMIDIFIEDSUPPLYAIR") {
                            state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy = DOANeutralDehumSup;
                        } else if (DOASControlMethod == "COLDSUPPLYAIR") {
                            state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy = DOACoolSup;
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                            ShowContinueError(state,
                                              "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" +
                                                  state.dataIPShortCut->cAlphaArgs(9) + "\".");
                            ShowContinueError(state, "... valid values are NeutralSupplyAir, NeutralDehumidifiedSupplyAir or ColdSupplyAir.");
                            ErrorsFound = true;
                        }
                    }
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint = state.dataIPShortCut->rNumericArgs(17);
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint = state.dataIPShortCut->rNumericArgs(18);
                    if (state.dataIPShortCut->rNumericArgs(17) > 0.0 && state.dataIPShortCut->rNumericArgs(18) > 0.0 &&
                        state.dataIPShortCut->rNumericArgs(17) >= state.dataIPShortCut->rNumericArgs(18)) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                        ShowContinueError(state, "... Dedicated Outside Air Low Setpoint for Design must be less than the High Setpoint");
                        ErrorsFound = true;
                    }
                }
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, cCurrentModuleObject + ": Errors found in getting input. Program terminates.");
    }
}

void ReportTemperatureInputError(
    EnergyPlusData &state, std::string cObjectName, int const paramNum, Real64 comparisonTemperature, bool const shouldFlagSevere, bool &ErrorsFound)
{
    if (state.dataIPShortCut->rNumericArgs(paramNum) < comparisonTemperature) {
        if (shouldFlagSevere) { // heating supply air temperature is lower than cooling supply air temperature--not allowed
            ShowSevereError(state, cObjectName + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" has invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}] is less than {}=[{:.2R}]",
                                     state.dataIPShortCut->cNumericFieldNames(paramNum),
                                     state.dataIPShortCut->rNumericArgs(paramNum),
                                     state.dataIPShortCut->cNumericFieldNames(paramNum - 2),
                                     state.dataIPShortCut->rNumericArgs(paramNum - 2)));
            ShowContinueError(state, format("This is not allowed.  Please check and revise your input."));
            ErrorsFound = true;
        } else { // then input is lower than comparison tempeature--just produce a warning for user to check input
            ShowWarningError(state, cObjectName + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" has invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}] is less than [{:.2R}]",
                                     state.dataIPShortCut->cNumericFieldNames(paramNum),
                                     state.dataIPShortCut->rNumericArgs(paramNum),
                                     comparisonTemperature));
            ShowContinueError(state, format("Please check your input to make sure this is correct."));
        }
    }
}

void GetZoneAndZoneListNames(
    EnergyPlusData &state, bool &ErrorsFound, int &NumZones, Array1D_string &ZoneNames, int &NumZoneLists, Array1D<ZoneListData> &ZoneListNames)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Get Zone and ZoneList Names so Sizing:Zone can use global ZoneList.
    // This is not a full validation of these objects -- only enough to fill
    // structures for the Sizing:Zone object.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Item;
    int Found;
    int Item1;
    int NumAlphas;
    int NumNumbers;
    int IOStatus;
    bool InErrFlag; // Preserve (no current use) the input status of ErrorsFound

    InErrFlag = ErrorsFound;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Zone";
    NumZones = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    ZoneNames.allocate(NumZones);

    for (Item = 1; Item <= NumZones; ++Item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Item,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // validation, but no error
        Found = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), ZoneNames, Item - 1);
        if (Found == 0) {
            ZoneNames(Item) = state.dataIPShortCut->cAlphaArgs(1);
        } else {
            ZoneNames(Item) = "xxxxx";
        }
    }

    cCurrentModuleObject = "ZoneList";
    NumZoneLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    ZoneListNames.allocate(NumZoneLists);

    for (Item = 1; Item <= NumZoneLists; ++Item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Item,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // validation, but no error
        Found = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), ZoneListNames, Item - 1);
        if (Found == 0) {
            ZoneListNames(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
        } else {
            ZoneListNames(Item).Name = "xxxxx";
        }
        ZoneListNames(Item).Zones.allocate(NumAlphas - 1);
        ZoneListNames(Item).NumOfZones = NumAlphas - 1;
        for (Item1 = 2; Item1 <= NumAlphas; ++Item1) {
            Found = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(Item1), ZoneNames, NumZones);
            ZoneListNames(Item).Zones(Item1 - 1) = Found;
        }
    }
}

void GetSystemSizingInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for System Sizing objects and stores it in
    // appropriate data structures.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.

    // Using/Aliasing

    // Sizing:System;
    constexpr int iNameAlphaNum = 1;                          // A1, \field AirLoop Name
    constexpr int iLoadTypeSizeAlphaNum = 2;                  // A2, \field Type of Load to Size On
    constexpr int iCoolCapControlAlphaNum = 11;               // A11 \field Central Cooling Capacity Control Method
    constexpr int iDesignOAVolFlowNumericNum = 1;             // N1, \field Design Outdoor Air Flow Rate
    constexpr int iMinSysAirFlowRatioNumericNum = 2;          // N2, \field Minimum System Air Flow Ratio
    constexpr int iPreheatDesignTempNumericNum = 3;           // N3, \field Preheat Design Temperature
    constexpr int iPreheatDesignHumRatNumericNum = 4;         // N4, \field Preheat Design Humidity Ratio
    constexpr int iPrecoolDesignTempNumericNum = 5;           // N5, \field Precool Design Temperature
    constexpr int iPrecoolDesignHumRatNumericNum = 6;         // N6, \field Precool Design Humidity Ratio
    constexpr int iCentralCoolDesignSATempNumericNum = 7;     // N7, \field Central Cooling Design Supply Air Temperature
    constexpr int iCentralHeatDesignSATempNumericNum = 8;     // N8, \field Central Heating Design Supply Air Temperature
    constexpr int iSizingOptionAlphaNum = 3;                  //  A3, \field Sizing Option
    constexpr int i100PercentOACoolingAlphaNum = 4;           //  A4, \field 100% Outdoor Air in Cooling
    constexpr int i100PercentOAHeatingAlphaNum = 5;           //  A5, \field 100% Outdoor Air in Heating
    constexpr int iCentralCoolDesignSAHumRatNumericNum = 9;   // N9, \field Central Cooling Design Supply Air Humidity Ratio
    constexpr int iCentralHeatDesignSAHumRatNumericNum = 10;  // N10, \field Central Heating Design Supply Air Humidity Ratio
    constexpr int iCoolSAFMAlphaNum = 6;                      // A6, \field Cooling Design Air Flow Method
    constexpr int iMaxCoolAirVolFlowNumericNum = 11;          // N11, \field Cooling Design Air Flow Rate {m3/s}
    constexpr int iCoolFlowPerFloorAreaNumericNum = 12;       // N12, \field Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}
    constexpr int iCoolFlowPerFracCoolNumericNum = 13;        // N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
    constexpr int iCoolFlowPerCoolCapNumericNum = 14;         // N14, \field Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}
    constexpr int iHeatSAFMAlphaNum = 7;                      // A7, \field Heating Design Air Flow Method
    constexpr int iMaxHeatAirVolFlowNumericNum = 15;          // N15, \field Heating Design Air Flow Rate {m3/s}
    constexpr int iHeatFlowPerFloorAreaNumericNum = 16;       // N16, \field Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}
    constexpr int iHeatFlowPerFracHeatNumericNum = 17;        // N17, \field Fraction of Autosized Design Heating Supply Air Flow Rate {-}
    constexpr int iHeatFlowPerFracCoolNumericNum = 18;        // N18, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
    constexpr int iHeatFlowPerHeatCapNumericNum = 19;         // N19, \field Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
    constexpr int iSystemOASMethodAlphaNum = 8;               // A8,  \field System Outdoor Air Method
    constexpr int iZoneMaxOAFractionNumericNum = 20;          // N20, \field Zone Maximum Outdoor Air Fraction
    constexpr int iCoolCAPMAlphaNum(9);                       // A9, \field Cooling Design Capacity Method
    constexpr int iCoolDesignCapacityNumericNum(21);          // N21, \field Cooling Design Capacity {W}
    constexpr int iCoolCapacityPerFloorAreaNumericNum(22);    // N22, \field Cooling Design Capacity Per Floor Area {W/m2}
    constexpr int iCoolFracOfAutosizedCapacityNumericNum(23); // N23, \field Fraction of Autosized Cooling Design Capacity {-}
    constexpr int iHeatCAPMAlphaNum(10);                      // A10, \field Heating Design Capacity Method
    constexpr int iHeatDesignCapacityNumericNum(24);          // N24, \field Heating Design Capacity {W}
    constexpr int iHeatCapacityPerFloorAreaNumericNum(25);    // N25, \field Heating Design Capacity Per Floor Area {W/m2}
    constexpr int iHeatFracOfAutosizedCapacityNumericNum(26); // N26, \field Fraction of Autosized Cooling Design Capacity {-}
    constexpr int iOccupantDiversity = 27;                    // N26, \field Occupant Diversity

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SysSizIndex;         // loop index
    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    int NumDesDays;          // Number of design days in input

    auto &SysSizInput(state.dataSize->SysSizInput);

    state.dataSizingManager->NumAirLoops = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC");
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Sizing:System";
    state.dataSize->NumSysSizInput = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataSize->NumSysSizInput > 0) {
        NumDesDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        if (NumDesDays == 0 && (state.dataGlobal->DoSystemSizing || state.dataGlobal->DoPlantSizing)) {
            ShowSevereError(state, "System Sizing calculations need SizingPeriod:* input. None found.");
            ErrorsFound = true;
        }
        SysSizInput.allocate(state.dataSize->NumSysSizInput);
    }

    for (SysSizIndex = 1; SysSizIndex <= state.dataSize->NumSysSizInput; ++SysSizIndex) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SysSizIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(iNameAlphaNum), cCurrentModuleObject, ErrorsFound);

        SysSizInput(SysSizIndex).AirPriLoopName = state.dataIPShortCut->cAlphaArgs(iNameAlphaNum);
        {
            auto const loadSizeType(state.dataIPShortCut->cAlphaArgs(iLoadTypeSizeAlphaNum));
            if (loadSizeType == "SENSIBLE") {
                SysSizInput(SysSizIndex).LoadSizeType = Sensible;
                // } else if ( loadSizeType == "LATENT" ) {
                // SysSizInput( SysSizIndex ).LoadSizeType = Latent;
            } else if (loadSizeType == "TOTAL") {
                SysSizInput(SysSizIndex).LoadSizeType = Total;
            } else if (loadSizeType == "VENTILATIONREQUIREMENT") {
                SysSizInput(SysSizIndex).LoadSizeType = Ventilation;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iLoadTypeSizeAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iLoadTypeSizeAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are Sensible, Total, or VentilationRequirement.");
                ErrorsFound = true;
            }
        }
        // assign CoolingPeakLoadType based on LoadSizeType for now
        if (SysSizInput(SysSizIndex).LoadSizeType == Sensible) {
            SysSizInput(SysSizIndex).CoolingPeakLoadType = SensibleCoolingLoad;
        } else if (SysSizInput(SysSizIndex).LoadSizeType == Total) {
            SysSizInput(SysSizIndex).CoolingPeakLoadType = TotalCoolingLoad;
        } else {
            SysSizInput(SysSizIndex).CoolingPeakLoadType = SensibleCoolingLoad;
        }
        // set the CoolCapControl input
        SysSizInput(SysSizIndex).CoolCapControl = VAV;
        {
            auto const CoolCapCtrl(state.dataIPShortCut->cAlphaArgs(iCoolCapControlAlphaNum));
            if (CoolCapCtrl == "VAV") {
                SysSizInput(SysSizIndex).CoolCapControl = VAV;
            } else if (CoolCapCtrl == "BYPASS") {
                SysSizInput(SysSizIndex).CoolCapControl = Bypass;
            } else if (CoolCapCtrl == "VT") {
                SysSizInput(SysSizIndex).CoolCapControl = VT;
            } else if (CoolCapCtrl == "ONOFF") {
                SysSizInput(SysSizIndex).CoolCapControl = OnOff;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iCoolCapControlAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iCoolCapControlAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are VAV, Bypass, VT, or OnOff.");
                ErrorsFound = true;
            }
        }
        {
            auto const sizingOption(state.dataIPShortCut->cAlphaArgs(iSizingOptionAlphaNum));
            if (sizingOption == "COINCIDENT") {
                SysSizInput(SysSizIndex).SizingOption = Coincident;
            } else if (sizingOption == "NONCOINCIDENT") {
                SysSizInput(SysSizIndex).SizingOption = NonCoincident;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iSizingOptionAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iSizingOptionAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are Coincident or NonCoincident.");
                ErrorsFound = true;
            }
        }
        {
            auto const coolOAOption(state.dataIPShortCut->cAlphaArgs(i100PercentOACoolingAlphaNum));
            if (coolOAOption == "YES") {
                SysSizInput(SysSizIndex).CoolOAOption = AllOA;
            } else if (coolOAOption == "NO") {
                SysSizInput(SysSizIndex).CoolOAOption = MinOA;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(i100PercentOACoolingAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(i100PercentOACoolingAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are Yes or No.");
                ErrorsFound = true;
            }
        }
        {
            auto const heatOAOption(state.dataIPShortCut->cAlphaArgs(i100PercentOAHeatingAlphaNum));
            if (heatOAOption == "YES") {
                SysSizInput(SysSizIndex).HeatOAOption = 1;
            } else if (heatOAOption == "NO") {
                SysSizInput(SysSizIndex).HeatOAOption = 2;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(i100PercentOAHeatingAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(i100PercentOAHeatingAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are Yes or No.");
                ErrorsFound = true;
            }
        }

        //  N1, \field Design Outdoor Air Flow Rate
        //      \type real
        //      \default autosize
        //      \minimum 0.0
        // int const  iDesignOAVolFlowNumericNum = 1;     // N1, \field Design Outdoor Air Flow Rate
        if (state.dataIPShortCut->lNumericFieldBlanks(iDesignOAVolFlowNumericNum)) {
            SysSizInput(SysSizIndex).DesOutAirVolFlow = AutoSize;
        } else if (state.dataIPShortCut->rNumericArgs(iDesignOAVolFlowNumericNum) < 0.0 &&
                   state.dataIPShortCut->rNumericArgs(iDesignOAVolFlowNumericNum) != AutoSize) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iDesignOAVolFlowNumericNum),
                                     state.dataIPShortCut->rNumericArgs(iDesignOAVolFlowNumericNum)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).DesOutAirVolFlow = state.dataIPShortCut->rNumericArgs(iDesignOAVolFlowNumericNum);
        }
        if (SysSizInput(SysSizIndex).DesOutAirVolFlow == AutoSize) {
            SysSizInput(SysSizIndex).OAAutoSized = true;
        }

        //  N2, \field Minimum System Air Flow Ratio
        //      \required-field
        //      \type real
        //      \minimum 0.0
        //      \maximum 1.0
        // int const iMinSysAirFlowRatioNumericNum = 2;  // N2, \field Minimum System Air Flow Ratio
        if (state.dataIPShortCut->lNumericFieldBlanks(iMinSysAirFlowRatioNumericNum)) {
            SysSizInput(SysSizIndex).SysAirMinFlowRat = 0.0;
        } else if ((state.dataIPShortCut->rNumericArgs(iMinSysAirFlowRatioNumericNum) < 0.0) &&
                   (state.dataIPShortCut->rNumericArgs(iMinSysAirFlowRatioNumericNum) != DataSizing::AutoSize)) {
            ShowSevereError(state,
                            cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iMinSysAirFlowRatioNumericNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iMinSysAirFlowRatioNumericNum),
                                     state.dataIPShortCut->rNumericArgs(iMinSysAirFlowRatioNumericNum)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).SysAirMinFlowRat = state.dataIPShortCut->rNumericArgs(iMinSysAirFlowRatioNumericNum);
            if (state.dataIPShortCut->rNumericArgs(iMinSysAirFlowRatioNumericNum) == DataSizing::AutoSize) {
                SysSizInput(SysSizIndex).SysAirMinFlowRatWasAutoSized = true;
            }
        }
        // int const iPreheatDesignTempNumericNum = 3; // N3, \field Preheat Design Temperature
        // int const iPreheatDesignHumRatNumericNum = 4; // N4, \field Preheat Design Humidity Ratio
        // int const iPrecoolDesignTempNumericNum = 5; // N5, \field Precool Design Temperature
        // int const iPrecoolDesignHumRatNumericNum = 6; // N6, \field Precool Design Humidity Ratio
        // int const iCentralCoolDesignSATempNumericNum = 7; // N7, \field Central Cooling Design Supply Air Temperature
        // int const iCentralHeatDesignSATempNumericNum = 8; // N8, \field Central Heating Design Supply Air Temperature
        // int const iCentralCoolDesignSAHumRatNumericNum = 9; // N9, \field Central Cooling Design Supply Air Humidity Ratio
        // int const iCentralHeatDesignSAHumRatNumericNum = 10; // N10, \field Central Heating Design Supply Air Humidity Ratio
        SysSizInput(SysSizIndex).PreheatTemp = state.dataIPShortCut->rNumericArgs(iPreheatDesignTempNumericNum);
        SysSizInput(SysSizIndex).PreheatHumRat = state.dataIPShortCut->rNumericArgs(iPreheatDesignHumRatNumericNum);
        SysSizInput(SysSizIndex).PrecoolTemp = state.dataIPShortCut->rNumericArgs(iPrecoolDesignTempNumericNum);
        SysSizInput(SysSizIndex).PrecoolHumRat = state.dataIPShortCut->rNumericArgs(iPrecoolDesignHumRatNumericNum);
        SysSizInput(SysSizIndex).CoolSupTemp = state.dataIPShortCut->rNumericArgs(iCentralCoolDesignSATempNumericNum);
        SysSizInput(SysSizIndex).HeatSupTemp = state.dataIPShortCut->rNumericArgs(iCentralHeatDesignSATempNumericNum);
        SysSizInput(SysSizIndex).CoolSupHumRat = state.dataIPShortCut->rNumericArgs(iCentralCoolDesignSAHumRatNumericNum);
        SysSizInput(SysSizIndex).HeatSupHumRat = state.dataIPShortCut->rNumericArgs(iCentralHeatDesignSAHumRatNumericNum);
        //  N11, \field Cooling Design Air Flow Rate
        //      \note This input is used if Cooling Design Air Flow Method is Flow/System
        //      \note This value will *not* be multiplied by any sizing factor or by zone multipliers.
        //      \note If using zone multipliers, this value must be large enough to serve the multiplied zones.
        //      \type real
        //      \units m3/s
        //      \minimum 0
        //      \default 0
        // int const iCoolSAFMAlphaNum = 6; // A6, \field Cooling Design Air Flow Method
        // int const iMaxCoolAirVolFlowNumericNum = 11; // N11, \field Cooling Design Air Flow Rate {m3/s}
        // int const iCoolFlowPerFloorAreaNumericNum = 12; // N12, \field Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}
        // int const iCoolFlowPerFracCoolNumericNum = 13; // N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
        // int const iCoolFlowPerCoolCapNumericNum = 14; // N14, \field Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}

        if (state.dataIPShortCut->lNumericFieldBlanks(iMaxCoolAirVolFlowNumericNum)) {
            SysSizInput(SysSizIndex).DesCoolAirFlow = 0.0;
        } else if (state.dataIPShortCut->rNumericArgs(iMaxCoolAirVolFlowNumericNum) < 0.0) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iMaxCoolAirVolFlowNumericNum),
                                     state.dataIPShortCut->rNumericArgs(iMaxCoolAirVolFlowNumericNum)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).DesCoolAirFlow = state.dataIPShortCut->rNumericArgs(iMaxCoolAirVolFlowNumericNum);
        }
        //  N12;\field Heating Design Air Flow Rate
        //      \note This input is used if Heating Design Air Flow Method is Flow/System
        //      \note This value will *not* be multiplied by any sizing factor or by zone multipliers.
        //      \note If using zone multipliers, this value must be large enough to serve the multiplied zones.
        //      \type real
        //      \units m3/s
        //      \minimum 0
        //      \default 0
        // int const iHeatSAFMAlphaNum = 7; // A7, \field Heating Design Air Flow Method
        // int const iMaxHeatAirVolFlowNumericNum = 12; // N15, \field Heating Design Air Flow Rate {m3/s}
        // int const iHeatFlowPerFloorAreaNumericNum = 16; // N16, \field Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}
        // int const iHeatFlowPerFracHeatNumericNum = 17; // N17, \field Fraction of Autosized Design Heating Supply Air Flow Rate {-}
        // int const iHeatFlowPerFracCoolNumericNum = 18; // N18, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
        // int const iHeatFlowPerHeatCapNumericNum = 19; // N19, \field Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
        // add input fields for other cooling sizing methods
        if (state.dataIPShortCut->lNumericFieldBlanks(iMaxHeatAirVolFlowNumericNum)) {
            SysSizInput(SysSizIndex).DesHeatAirFlow = 0.0;
        } else if (state.dataIPShortCut->rNumericArgs(iMaxHeatAirVolFlowNumericNum) < 0.0) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iMaxHeatAirVolFlowNumericNum),
                                     state.dataIPShortCut->rNumericArgs(iMaxHeatAirVolFlowNumericNum)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).DesHeatAirFlow = state.dataIPShortCut->rNumericArgs(iMaxHeatAirVolFlowNumericNum);
        }
        //  N13;\field Maximum Zone Outdoor Air Fraction
        //      \type real
        //      \default 1.0
        //      \minimum> 0.0
        //      \units dimensionless
        // int const iSystemOASMethodAlphaNum = 8; // A8,  \field System Outdoor Air Method
        // int const iZoneMaxOAFractionNumericNum = 13; // N20, \field Zone Maximum Outdoor Air Fraction

        // add input fields for other heating sizing methods
        if (state.dataIPShortCut->lNumericFieldBlanks(iZoneMaxOAFractionNumericNum)) {
            SysSizInput(SysSizIndex).MaxZoneOAFraction = 0.0;
        } else if (state.dataIPShortCut->rNumericArgs(iZoneMaxOAFractionNumericNum) < 0.0) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iZoneMaxOAFractionNumericNum),
                                     state.dataIPShortCut->rNumericArgs(iZoneMaxOAFractionNumericNum)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).MaxZoneOAFraction = state.dataIPShortCut->rNumericArgs(iZoneMaxOAFractionNumericNum);
        }
        {
            auto const coolAirDesMethod(state.dataIPShortCut->cAlphaArgs(iCoolSAFMAlphaNum));
            if (coolAirDesMethod == "DESIGNDAY") {
                SysSizInput(SysSizIndex).CoolAirDesMethod = FromDDCalc;
            } else if (coolAirDesMethod == "FLOW/SYSTEM") {
                SysSizInput(SysSizIndex).CoolAirDesMethod = InpDesAirFlow;
            } else if (coolAirDesMethod == "FLOWPERFLOORAREA") {
                SysSizInput(SysSizIndex).CoolAirDesMethod = InpDesAirFlow;
                SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FlowPerFloorArea;
                SysSizInput(SysSizIndex).FlowPerFloorAreaCooled = state.dataIPShortCut->rNumericArgs(iCoolFlowPerFloorAreaNumericNum);
            } else if (coolAirDesMethod == "FRACTIONOFAUTOSIZEDCOOLINGAIRFLOW") {
                SysSizInput(SysSizIndex).CoolAirDesMethod = FromDDCalc;
                SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FractionOfAutosizedCoolingAirflow;
                SysSizInput(SysSizIndex).FractionOfAutosizedCoolingAirflow = state.dataIPShortCut->rNumericArgs(iCoolFlowPerFracCoolNumericNum);
            } else if (coolAirDesMethod == "FLOWPERCOOLINGCAPACITY") {
                SysSizInput(SysSizIndex).CoolAirDesMethod = FromDDCalc;
                SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FlowPerCoolingCapacity;
                SysSizInput(SysSizIndex).FlowPerCoolingCapacity = state.dataIPShortCut->rNumericArgs(iCoolFlowPerCoolCapNumericNum);
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iCoolSAFMAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iCoolSAFMAlphaNum) + "\".");
                ShowContinueError(state,
                                  "... valid values are DesignDay, Flow/System, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, or "
                                  "FlowPerCoolingCapacity.");
                ErrorsFound = true;
            }
        }
        {
            auto const heatAirDesMethod(state.dataIPShortCut->cAlphaArgs(iHeatSAFMAlphaNum));
            if (heatAirDesMethod == "DESIGNDAY") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
            } else if (heatAirDesMethod == "FLOW/SYSTEM") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = InpDesAirFlow;
            } else if (heatAirDesMethod == "FLOWPERFLOORAREA") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = InpDesAirFlow;
                SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FlowPerFloorArea;
                SysSizInput(SysSizIndex).FlowPerFloorAreaHeated = state.dataIPShortCut->rNumericArgs(iHeatFlowPerFloorAreaNumericNum);
            } else if (heatAirDesMethod == "FRACTIONOFAUTOSIZEDHEATINGAIRFLOW") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
                SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FractionOfAutosizedHeatingAirflow;
                SysSizInput(SysSizIndex).FractionOfAutosizedHeatingAirflow = state.dataIPShortCut->rNumericArgs(iHeatFlowPerFracHeatNumericNum);
            } else if (heatAirDesMethod == "FRACTIONOFAUTOSIZEDCOOLINGAIRFLOW") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
                SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FractionOfAutosizedCoolingAirflow;
                SysSizInput(SysSizIndex).FractionOfAutosizedCoolingAirflow = state.dataIPShortCut->rNumericArgs(iHeatFlowPerFracCoolNumericNum);
            } else if (heatAirDesMethod == "FLOWPERHEATINGCAPACITY") {
                SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
                SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FlowPerHeatingCapacity;
                SysSizInput(SysSizIndex).FlowPerHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatFlowPerHeatCapNumericNum);
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iHeatSAFMAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iHeatSAFMAlphaNum) + "\".");
                ShowContinueError(state,
                                  "... valid values are DesignDay, Flow/System, FlowPerFloorArea, FractionOfAutosizedHeatingAirflow, or "
                                  "FlowPerHeatingCapacity.");
                ErrorsFound = true;
            }
        }
        {
            auto const systemOAMethod(state.dataIPShortCut->cAlphaArgs(iSystemOASMethodAlphaNum));
            if (systemOAMethod == "ZONESUM") {
                SysSizInput(SysSizIndex).SystemOAMethod = SOAM_ZoneSum;
            } else if (systemOAMethod == "STANDARD62.1VENTILATIONRATEPROCEDURE") {
                SysSizInput(SysSizIndex).SystemOAMethod = SOAM_VRP;
                if (SysSizInput(SysSizIndex).LoadSizeType == Ventilation) {
                    ShowWarningError(
                        state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid combination of inputs.");
                    ShowContinueError(state,
                                      state.dataIPShortCut->cAlphaFieldNames(iLoadTypeSizeAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iLoadTypeSizeAlphaNum) + " and " +
                                          state.dataIPShortCut->cAlphaFieldNames(iSystemOASMethodAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iSystemOASMethodAlphaNum) + ".");
                    ShowContinueError(state, "Resetting System Outdoor Air Method to ZoneSum.");
                    SysSizInput(SysSizIndex).SystemOAMethod = SOAM_ZoneSum;
                } else {
                    if (SysSizInput(SysSizIndex).DesOutAirVolFlow > 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                        ShowContinueError(state,
                                          "SystemOAMethod is set to VRP and " + state.dataIPShortCut->cNumericFieldNames(iDesignOAVolFlowNumericNum) +
                                              " > 0, user entry will be ignored.");
                    }
                }
            } else if (systemOAMethod == "STANDARD62.1SIMPLIFIEDPROCEDURE") {
                SysSizInput(SysSizIndex).SystemOAMethod = SOAM_SP;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
                ShowContinueError(state,
                                  "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iSystemOASMethodAlphaNum) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(iSystemOASMethodAlphaNum) + "\".");
                ShowContinueError(state, "... valid values are ZoneSum or Standard62.1VentilationRateProcedure.");
                ErrorsFound = true;
            }
        }

        // Determine SysSizInput electric Cooling design capacity sizing method
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum), "COOLINGDESIGNCAPACITY")) {
            SysSizInput(SysSizIndex).CoolingCapMethod = CoolingDesignCapacity;
            // SysSizInput( SysSizIndex ).ScaledCoolingCapacity = AutoSize can be set to autosize cooling capacity
            SysSizInput(SysSizIndex).ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(iCoolDesignCapacityNumericNum);
            if (SysSizInput(SysSizIndex).ScaledCoolingCapacity < 0.0 && SysSizInput(SysSizIndex).ScaledCoolingCapacity != AutoSize) {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  format("Illegal {} = {:.7T}",
                                         state.dataIPShortCut->cNumericFieldNames(iCoolDesignCapacityNumericNum),
                                         state.dataIPShortCut->rNumericArgs(iCoolDesignCapacityNumericNum)));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum), "CAPACITYPERFLOORAREA")) {
            SysSizInput(SysSizIndex).CoolingCapMethod = CapacityPerFloorArea;
            if (!state.dataIPShortCut->lNumericFieldBlanks(iCoolCapacityPerFloorAreaNumericNum)) {
                SysSizInput(SysSizIndex).ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(iCoolCapacityPerFloorAreaNumericNum);
                if (SysSizInput(SysSizIndex).ScaledCoolingCapacity <= 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iCoolCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum));
                    ShowContinueError(state,
                                      format("Illegal {} = {:.7T}",
                                             state.dataIPShortCut->cNumericFieldNames(iCoolCapacityPerFloorAreaNumericNum),
                                             state.dataIPShortCut->rNumericArgs(iCoolCapacityPerFloorAreaNumericNum)));
                    ErrorsFound = true;
                } else if (SysSizInput(SysSizIndex).ScaledCoolingCapacity == AutoSize) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iCoolCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Illegal " + state.dataIPShortCut->cNumericFieldNames(iCoolCapacityPerFloorAreaNumericNum) + " = Autosize");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  "Input for " + state.dataIPShortCut->cAlphaFieldNames(iCoolCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum));
                ShowContinueError(state,
                                  "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iCoolCapacityPerFloorAreaNumericNum));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum), "FRACTIONOFAUTOSIZEDCOOLINGCAPACITY")) {
            SysSizInput(SysSizIndex).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
            if (!state.dataIPShortCut->lNumericFieldBlanks(iCoolFracOfAutosizedCapacityNumericNum)) {
                SysSizInput(SysSizIndex).ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(iCoolFracOfAutosizedCapacityNumericNum);
                if (SysSizInput(SysSizIndex).ScaledCoolingCapacity < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      format("Illegal {} = {:.7T}",
                                             state.dataIPShortCut->cNumericFieldNames(iCoolFracOfAutosizedCapacityNumericNum),
                                             state.dataIPShortCut->rNumericArgs(iCoolFracOfAutosizedCapacityNumericNum)));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  "Input for " + state.dataIPShortCut->cAlphaFieldNames(iCoolCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum));
                ShowContinueError(state,
                                  "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iCoolFracOfAutosizedCapacityNumericNum));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum), "NONE")) {
            SysSizInput(SysSizIndex).CoolingCapMethod = None;
            SysSizInput(SysSizIndex).ScaledCoolingCapacity = 0.0;
        } else {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iCoolCAPMAlphaNum) + "=\"" +
                                  state.dataIPShortCut->cAlphaArgs(iCoolCAPMAlphaNum) + "\".");
            ShowContinueError(state,
                              "... valid values are CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, or None.");
            ErrorsFound = true;
        }

        // Determine SysSizInput electric heating design capacity sizing method
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "HEATINGDESIGNCAPACITY")) {
            SysSizInput(SysSizIndex).HeatingCapMethod = HeatingDesignCapacity;
            // SysSizInput( SysSizIndex ).ScaledHeatingCapacity = AutoSize can be set to autosize heating capacity
            SysSizInput(SysSizIndex).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum);
            if (SysSizInput(SysSizIndex).ScaledHeatingCapacity < 0.0 && SysSizInput(SysSizIndex).ScaledHeatingCapacity != AutoSize) {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  format("Illegal {} = {:.7T}",
                                         state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                         state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum)));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "CAPACITYPERFLOORAREA")) {
            SysSizInput(SysSizIndex).HeatingCapMethod = CapacityPerFloorArea;
            if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                SysSizInput(SysSizIndex).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                if (SysSizInput(SysSizIndex).ScaledHeatingCapacity <= 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      format("Illegal {} = {:.7T}",
                                             state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                             state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                    ErrorsFound = true;
                } else if (SysSizInput(SysSizIndex).ScaledHeatingCapacity == AutoSize) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Illegal " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                ShowContinueError(state,
                                  "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "FRACTIONOFAUTOSIZEDHEATINGCAPACITY")) {
            SysSizInput(SysSizIndex).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
            if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                SysSizInput(SysSizIndex).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                if (SysSizInput(SysSizIndex).ScaledHeatingCapacity < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                    ShowContinueError(state,
                                      format("Illegal {} = {:.7T}",
                                             state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                             state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + SysSizInput(SysSizIndex).AirPriLoopName);
                ShowContinueError(state,
                                  "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                ShowContinueError(state,
                                  "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "NONE")) {
            SysSizInput(SysSizIndex).HeatingCapMethod = None;
            SysSizInput(SysSizIndex).ScaledHeatingCapacity = 0.0;
        } else {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              "... incorrect " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + "=\"" +
                                  state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum) + "\".");
            ShowContinueError(state,
                              "... valid values are HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedHeatingCapacity, or None.");
            ErrorsFound = true;
        }

        //  N27; \field Occupant Diversity
        //      \type real
        //      \maximum 1.0
        //      \minimum> 0.0
        // int const  iOccupantDiversity = 27;     // N27, \field Occupant Diversity
        if (state.dataIPShortCut->lNumericFieldBlanks(iOccupantDiversity)) {
            SysSizInput(SysSizIndex).OccupantDiversity = AutoSize;
        } else if (state.dataIPShortCut->rNumericArgs(iOccupantDiversity) <= 0.0 &&
                   state.dataIPShortCut->rNumericArgs(iOccupantDiversity) != AutoSize) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be negative.",
                                     state.dataIPShortCut->cNumericFieldNames(iOccupantDiversity),
                                     state.dataIPShortCut->rNumericArgs(iOccupantDiversity)));
            ErrorsFound = true;
        } else if (state.dataIPShortCut->rNumericArgs(iOccupantDiversity) > 1.0 &&
                   state.dataIPShortCut->rNumericArgs(iOccupantDiversity) != AutoSize) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(iNameAlphaNum) + "\", invalid data.");
            ShowContinueError(state,
                              format("... incorrect {}=[{:.2R}],  value should not be greater than 1.0.",
                                     state.dataIPShortCut->cNumericFieldNames(iOccupantDiversity),
                                     state.dataIPShortCut->rNumericArgs(iOccupantDiversity)));
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).OccupantDiversity = state.dataIPShortCut->rNumericArgs(iOccupantDiversity);
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, cCurrentModuleObject + ": Errors found in getting input. Program terminates.");
    }
}

void GetPlantSizingInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   October 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for Plant Sizing objects and stores it in
    // appropriate data structures.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PltSizIndex;         // loop index
    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    int NumDesDays;          // Number of design days in input
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Sizing:Plant";
    state.dataSize->NumPltSizInput = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataSize->NumPltSizInput > 0) {
        NumDesDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays") +
                     state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        if (NumDesDays == 0 && state.dataGlobal->DoPlantSizing) {
            ShowSevereError(state, "Plant Sizing calculations need SizingPeriod:* input");
            ErrorsFound = true;
        }
        state.dataSize->PlantSizData.allocate(state.dataSize->NumPltSizInput);
        for (auto &e : state.dataSize->PlantSizData) {
            e.PlantLoopName.clear();
            e.ExitTemp = 0.0;
            e.DeltaT = 0.0;
            e.LoopType = 0;
            e.DesVolFlowRate = 0.0;
        }
        for (int i = 1; i <= state.dataSize->NumPltSizInput; ++i) {
            state.dataSize->PlantSizData(i).ConcurrenceOption = NonCoincident;
            state.dataSize->PlantSizData(i).NumTimeStepsInAvg = 1;
        }
    }

    for (PltSizIndex = 1; PltSizIndex <= state.dataSize->NumPltSizInput; ++PltSizIndex) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 PltSizIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSize->PlantSizData(PltSizIndex).PlantLoopName = state.dataIPShortCut->cAlphaArgs(1);
        state.dataSize->PlantSizData(PltSizIndex).ExitTemp = state.dataIPShortCut->rNumericArgs(1);
        state.dataSize->PlantSizData(PltSizIndex).DeltaT = state.dataIPShortCut->rNumericArgs(2);
        if (NumNumbers > 2) {
            state.dataSize->PlantSizData(PltSizIndex).NumTimeStepsInAvg = state.dataIPShortCut->rNumericArgs(3);
        } else {
            state.dataSize->PlantSizData(PltSizIndex).NumTimeStepsInAvg = 1.0;
        }

        {
            auto const loopType(state.dataIPShortCut->cAlphaArgs(2));
            if (loopType == "HEATING") {
                state.dataSize->PlantSizData(PltSizIndex).LoopType = HeatingLoop;
            } else if (loopType == "COOLING") {
                state.dataSize->PlantSizData(PltSizIndex).LoopType = CoolingLoop;
            } else if (loopType == "CONDENSER") {
                state.dataSize->PlantSizData(PltSizIndex).LoopType = CondenserLoop;
            } else if (loopType == "STEAM") {
                state.dataSize->PlantSizData(PltSizIndex).LoopType = SteamLoop;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                ShowContinueError(state,
                                  "...incorrect " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ShowContinueError(state, R"(...Valid values are "Heating", "Cooling", "Condenser" or "Steam".)");
                ErrorsFound = true;
            }
        }

        if (NumAlphas > 2) {
            {
                auto const concurrenceOption(state.dataIPShortCut->cAlphaArgs(3));
                if (concurrenceOption == "NONCOINCIDENT") {
                    state.dataSize->PlantSizData(PltSizIndex).ConcurrenceOption = NonCoincident;
                } else if (concurrenceOption == "COINCIDENT") {
                    state.dataSize->PlantSizData(PltSizIndex).ConcurrenceOption = Coincident;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(
                        state, "...incorrect " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                    ShowContinueError(state, R"(...Valid values are "NonCoincident" or "Coincident".)");
                    ErrorsFound = true;
                }
            }
        }
        if (NumAlphas > 3) {
            {
                auto const sizingFactorOption(state.dataIPShortCut->cAlphaArgs(4));
                if (sizingFactorOption == "NONE") {
                    state.dataSize->PlantSizData(PltSizIndex).SizingFactorOption = NoSizingFactorMode;
                } else if (sizingFactorOption == "GLOBALHEATINGSIZINGFACTOR") {
                    state.dataSize->PlantSizData(PltSizIndex).SizingFactorOption = GlobalHeatingSizingFactorMode;
                } else if (sizingFactorOption == "GLOBALCOOLINGSIZINGFACTOR") {
                    state.dataSize->PlantSizData(PltSizIndex).SizingFactorOption = GlobalCoolingSizingFactorMode;
                } else if (sizingFactorOption == "LOOPCOMPONENTSIZINGFACTOR") {
                    state.dataSize->PlantSizData(PltSizIndex).SizingFactorOption = LoopComponentSizingFactorMode;
                }
            }
        }
        SetupEMSInternalVariable(state,
                                 "Plant Design Volume Flow Rate",
                                 state.dataSize->PlantSizData(PltSizIndex).PlantLoopName,
                                 "[m3/s]",
                                 state.dataSize->PlantSizData(PltSizIndex).DesVolFlowRate);
    }

    if (ErrorsFound) {
        ShowFatalError(state, cCurrentModuleObject + ": Errors found in getting input. Program terminates.");
    }
}

void SetupZoneSizing(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         L. Lawrie/F. Buhl
    //       DATE WRITTEN   March 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  execute a few (1) time steps of a simulation to facilitate setting up model for zone sizing
    //  developed to resolve reverse DD problems caused be the differences
    //  that stem from setup and information gathering that occurs during the first pass.

    // METHODOLOGY EMPLOYED:
    // Using global flag (kickoff sizing simulation), only a few time steps are executed.
    // global flag is used in other parts of simulation to terminate quickly.

    bool Available = true;

    state.dataSize->CurOverallSimDay = 0;
    while (Available) { // do for each environment

        GetNextEnvironment(state, Available, ErrorsFound);

        if (!Available) break;
        if (ErrorsFound) break;

        // check that environment is one of the design days
        if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
            continue;
        }

        state.dataGlobal->BeginEnvrnFlag = true;
        state.dataGlobal->EndEnvrnFlag = false;
        state.dataEnvrn->EndMonthFlag = false;
        state.dataGlobal->WarmupFlag = true;
        state.dataGlobal->DayOfSim = 0;

        state.dataSize->CurEnvirNumSimDay = 1;
        ++state.dataSize->CurOverallSimDay;

        ++state.dataGlobal->DayOfSim;
        state.dataGlobal->BeginDayFlag = true;
        state.dataGlobal->EndDayFlag = false;

        state.dataGlobal->HourOfDay = 1;

        state.dataGlobal->BeginHourFlag = true;
        state.dataGlobal->EndHourFlag = false;

        state.dataGlobal->TimeStep = 1;

        state.dataGlobal->BeginTimeStepFlag = true;

        ManageWeather(state);

        ManageHeatBalance(state);

        state.dataGlobal->BeginHourFlag = false;
        state.dataGlobal->BeginDayFlag = false;
        state.dataGlobal->BeginEnvrnFlag = false;
        state.dataGlobal->BeginSimFlag = false;
        state.dataGlobal->BeginFullSimFlag = false;

        //          ! do another timestep=1
        ManageWeather(state);

        ManageHeatBalance(state);

        //         do an end of day, end of environment time step

        state.dataGlobal->HourOfDay = 24;
        state.dataGlobal->TimeStep = state.dataGlobal->NumOfTimeStepInHour;
        state.dataGlobal->EndEnvrnFlag = true;

        ManageWeather(state);

        ManageHeatBalance(state);

    } // ... End environment loop.
}

void ReportZoneSizing(EnergyPlusData &state,
                      std::string const &ZoneName,   // the name of the zone
                      std::string const &LoadType,   // the description of the input variable
                      Real64 const CalcDesLoad,      // the value from the sizing calculation [W]
                      Real64 const UserDesLoad,      // the value from the sizing calculation modified by user input [W]
                      Real64 const CalcDesFlow,      // calculated design air flow rate [m3/s]
                      Real64 const UserDesFlow,      // user input or modified design air flow rate [m3/s]
                      std::string const &DesDayName, // the name of the design day that produced the peak
                      std::string const &PeakHrMin,  // time stamp of the peak
                      Real64 const PeakTemp,         // temperature at peak [C]
                      Real64 const PeakHumRat,       // humidity ratio at peak [kg water/kg dry air]
                      Real64 const FloorArea,        // zone floor area [m2]
                      Real64 const TotOccs,          // design number of occupants for the zone
                      Real64 const MinOAVolFlow,     // zone design minimum outside air flow rate [m3/s]
                      Real64 const DOASHeatAddRate   // zone design heat addition rate from the DOAS [W]
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Decenber 2001
    //       MODIFIED       August 2008, Greg Stark
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes one item of zone sizing data to the "eio" file..

    if (state.dataSizingManager->ReportZoneSizingMyOneTimeFlag) {
        static constexpr std::string_view Format_990(
            "! <Zone Sizing Information>, Zone Name, Load Type, Calc Des Load {W}, User Des Load {W}, Calc Des Air Flow "
            "Rate {m3/s}, User Des Air Flow Rate {m3/s}, Design Day Name, Date/Time of Peak, Temperature at Peak {C}, "
            "Humidity Ratio at Peak {kgWater/kgDryAir}, Floor Area {m2}, # Occupants, Calc Outdoor Air Flow Rate {m3/s}, "
            "Calc DOAS Heat Addition Rate {W}");
        print(state.files.eio, "{}\n", Format_990);
        state.dataSizingManager->ReportZoneSizingMyOneTimeFlag = false;
    }

    static constexpr std::string_view Format_991(
        " Zone Sizing Information, {}, {}, {:.5R}, {:.5R}, {:.5R}, {:.5R}, {}, {}, {:.5R}, {:.5R}, {:.5R}, {:.5R}, {:.5R}, {:.5R}\n");
    print(state.files.eio,
          Format_991,
          ZoneName,
          LoadType,
          CalcDesLoad,
          UserDesLoad,
          CalcDesFlow,
          UserDesFlow,
          DesDayName,
          PeakHrMin,
          PeakTemp,
          PeakHumRat,
          FloorArea,
          TotOccs,
          MinOAVolFlow,
          DOASHeatAddRate);

    // BSLLC Start
    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->addSQLiteZoneSizingRecord(ZoneName,
                                                                      LoadType,
                                                                      CalcDesLoad,
                                                                      UserDesLoad,
                                                                      CalcDesFlow,
                                                                      UserDesFlow,
                                                                      DesDayName,
                                                                      PeakHrMin,
                                                                      PeakTemp,
                                                                      PeakHumRat,
                                                                      MinOAVolFlow,
                                                                      DOASHeatAddRate);
    }
    // BSLLC Finish
}

// Writes system sizing data to EIO file using one row per system
void ReportSysSizing(EnergyPlusData &state,
                     std::string const &SysName,      // the name of the zone
                     std::string const &LoadType,     // either "Cooling" or "Heating"
                     std::string const &PeakLoadKind, // either "Sensible" or "Total"
                     Real64 const &UserDesCap,        // User  Design Capacity
                     Real64 const &CalcDesVolFlow,    // Calculated  Design Air Flow Rate
                     Real64 const &UserDesVolFlow,    // User Design Air Flow Rate
                     std::string const &DesDayName,   // the name of the design day that produced the peak
                     std::string const &DesDayDate,   // the date that produced the peak
                     int const &TimeStepIndex         // time step of the peak
)
{

    if (state.dataSizingManager->ReportSysSizingMyOneTimeFlag) {
        print(state.files.eio,
              "{}\n",
              "! <System Sizing Information>, System Name, Load Type, Peak Load Kind, User Design Capacity, Calc Des Air "
              "Flow Rate [m3/s], User Des Air Flow Rate [m3/s], Design Day Name, Date/Time of Peak");
        state.dataSizingManager->ReportSysSizingMyOneTimeFlag = false;
    }
    std::string dateHrMin = DesDayDate + " " + TimeIndexToHrMinString(state, TimeStepIndex);
    print(state.files.eio,
          " System Sizing Information, {}, {}, {}, {:.2R}, {:.5R}, {:.5R}, {}, {}\n",
          SysName,
          LoadType,
          PeakLoadKind,
          UserDesCap,
          CalcDesVolFlow,
          UserDesVolFlow,
          DesDayName,
          dateHrMin);

    // BSLLC Start
    if (state.dataSQLiteProcedures->sqlite)
        state.dataSQLiteProcedures->sqlite->addSQLiteSystemSizingRecord(
            SysName, LoadType, PeakLoadKind, UserDesCap, CalcDesVolFlow, UserDesVolFlow, DesDayName, dateHrMin);
    // BSLLC Finish
}

// convert an index for the timestep of the day into a hour minute string in the format 00:00
std::string TimeIndexToHrMinString(EnergyPlusData &state, int timeIndex)
{
    int tMinOfDay = timeIndex * state.dataGlobal->MinutesPerTimeStep;
    int tHr = int(tMinOfDay / 60.);
    int tMin = tMinOfDay - tHr * 60;
    return format(PeakHrMinFmt, tHr, tMin);
}

void GetZoneHVACSizing(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Nigusse - FSEC
    //       DATE WRITTEN   July 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the ZoneHVAC sizing methods object and stores it in
    // appropriate data structure.

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.
    // This object requires only a name where the default values are assumed
    // if subsequent fields are not entered.

    // Using/Aliasing

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetZoneHVACSizing: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int iHeatSAFMAlphaNum;                     // get input index to Zone HVAC sizing heat supp air flow method
    int iCoolSAFMAlphaNum;                     // get input index to Zone HVAC sizing cool supp air flow method
    int iMaxCoolAirVolFlowNumericNum;          // get input index to Zone HVAC sizing cool supply air flow
    int iMaxHeatAirVolFlowNumericNum;          // get input index to Zone HVAC sizing heat supply air flow
    int iNoCoolHeatSAFMAlphaNum;               // get input index to Zone HVAC sizing no cool/heat supply air flow
    int iMaxNoCoolHeatAirVolFlowNumericNum;    // get input index to Zone HVAC sizing no cool/heat supply air flow
    int iCoolFlowPerFloorAreaNumericNum;       // get input index to Zone HVAC sizing cool flow per floor area
    int iCoolFlowPerFracCoolNumericNum;        // get input index to Zone HVAC sizing cool flow per fraction cool
    int iCoolFlowPerCoolCapNumericNum;         // get input index to Zone HVAC sizing cool flow per cooling cap
    int iHeatFlowPerFloorAreaNumericNum;       // get input index to Zone HVAC sizing heat flow per floor area
    int iHeatFlowPerFracCoolNumericNum;        // get input index to Zone HVAC sizing heat flow per fraction heat
    int iHeatFlowPerHeatCapNumericNum;         // get input index to Zone HVAC sizing heat flow per heating cap
    int iNoCoolHeatFlowPerFloorAreaNumericNum; // get input index to Zone HVAC sizing no cool/heat FPA
    int iNoCoolHeatFlowPerFracCoolNumericNum;  // get input index to Zone HVAC sizing no cool/heat FPFC
    int iNoCoolHeatFlowPerFracHeatNumericNum;  // get input index to Zone HVAC sizing no cool/heat FPFH

    int iCoolCAPMAlphaNum;                      // get input index to Zone HVAC sizing chilled water flow method
    int iCoolDesignCapacityNumericNum;          // get input index to Zone HVAC sizing chilled water flow
    int iCoolCapacityPerFloorAreaNumericNum;    // get input index to Zone HVAC sizing cooling capacity per floor area
    int iCoolFracOfAutosizedCapacityNumericNum; // get input index to Zone HVAC sizing capacity as fraction autozized cooling capacity

    int iHeatCAPMAlphaNum;                      // get input index to Zone HVAC sizing heating capacity
    int iHeatDesignCapacityNumericNum;          // get input index to Zone HVAC sizing heating design capacity
    int iHeatCapacityPerFloorAreaNumericNum;    // get input index to Zone HVAC sizing heating capacity per floor area
    int iHeatFracOfAutosizedCapacityNumericNum; // get input index to Zone HVAC sizing capacity as fraction autozized cooling capacity

    iCoolSAFMAlphaNum = 2;               // get input index to Zone HVAC sizing heat supp air flow method
    iMaxCoolAirVolFlowNumericNum = 1;    // get input index to Zone HVAC sizing cool supply air flow
    iCoolFlowPerFloorAreaNumericNum = 2; // get input index to Zone HVAC sizing cool flow per floor area
    iCoolFlowPerFracCoolNumericNum = 3;  // get input index to Zone HVAC sizing cool flow per fraction cool
    iCoolFlowPerCoolCapNumericNum = 4;   // get input index to Zone HVAC sizing cool flow per cooling cap

    iNoCoolHeatSAFMAlphaNum = 3;               // get input index to Zone HVAC sizing no cool/heat supply air flow
    iMaxNoCoolHeatAirVolFlowNumericNum = 5;    // get input index to Zone HVAC sizing no cool/heat supply air flow
    iNoCoolHeatFlowPerFloorAreaNumericNum = 6; // get input index to Zone HVAC sizing no cool/heat FPA
    iNoCoolHeatFlowPerFracCoolNumericNum = 7;  // get input index to Zone HVAC sizing no cool/heat FPFC
    iNoCoolHeatFlowPerFracHeatNumericNum = 8;  // get input index to Zone HVAC sizing no cool/heat FPFH

    iHeatSAFMAlphaNum = 4;                // get input index to Zone HVAC sizing cool supp air flow method
    iMaxHeatAirVolFlowNumericNum = 9;     // get input index to Zone HVAC sizing heat supply air flow
    iHeatFlowPerFloorAreaNumericNum = 10; // get input index to Zone HVAC sizing heat flow per floor area
    iHeatFlowPerFracCoolNumericNum = 11;  // get input index to Zone HVAC sizing heat flow per fraction heat
    iHeatFlowPerHeatCapNumericNum = 12;   // get input index to Zone HVAC sizing heat flow per heating cap

    iCoolCAPMAlphaNum = 5;                       // get input index to Zone HVAC sizing cooling design capacity method
    iCoolDesignCapacityNumericNum = 13;          // get input index to Zone HVAC sizing cooling design capacity
    iCoolCapacityPerFloorAreaNumericNum = 14;    // get input index to Zone HVAC sizing cooling design capacity per floor area
    iCoolFracOfAutosizedCapacityNumericNum = 15; // get input index to Zone HVAC sizing as a fraction of cooling design capacity

    iHeatCAPMAlphaNum = 6;                       // get input index to Zone HVAC sizing heating capacity
    iHeatDesignCapacityNumericNum = 16;          // get input index to Zone HVAC sizing heating design capacity
    iHeatCapacityPerFloorAreaNumericNum = 17;    // get input index to Zone HVAC sizing heating capacity per floor area
    iHeatFracOfAutosizedCapacityNumericNum = 18; // get input index to Zone HVAC sizing capacity as fraction autozized heating capacity

    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int TotalArgs;           // Total number of alpha and numeric arguments (max) for a
    int IOStatus;            // Used in GetObjectItem
    int zSIndex;             // index of "DesignSpecification:ZoneHVAC:Sizing" objects
    bool ErrorsFound(false); // If errors detected in input
    //  REAL(r64) :: CalcAmt

    std::string CurrentModuleObject; // for ease in getting objects
    Array1D_string Alphas;           // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D<Real64> Numbers;         // Numeric input items for object
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

    CurrentModuleObject = "DesignSpecification:ZoneHVAC:Sizing";
    state.dataSize->NumZoneHVACSizing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);

    Alphas.allocate(NumAlphas);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNumbers);
    Numbers.dimension(NumNumbers, 0.0);
    lAlphaBlanks.dimension(NumAlphas, true);
    lNumericBlanks.dimension(NumNumbers, true);

    if (state.dataSize->NumZoneHVACSizing > 0) {
        state.dataSize->ZoneHVACSizing.allocate(state.dataSize->NumZoneHVACSizing);

        // Start Loading the System Input
        for (zSIndex = 1; zSIndex <= state.dataSize->NumZoneHVACSizing; ++zSIndex) {

            Alphas = "";
            cAlphaFields = "";
            cNumericFields = "";
            Numbers = 0;
            lAlphaBlanks = true;
            lNumericBlanks = true;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     zSIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataSize->ZoneHVACSizing(zSIndex).Name = Alphas(1);

            // Determine supply air flow rate sizing method for cooling mode
            if (UtilityRoutines::SameString(Alphas(iCoolSAFMAlphaNum), "SupplyAirFlowRate")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingSAFMethod = SupplyAirFlowRate;

                if (!lNumericBlanks(iMaxCoolAirVolFlowNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow = Numbers(iMaxCoolAirVolFlowNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iMaxCoolAirVolFlowNumericNum), Numbers(iMaxCoolAirVolFlowNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iMaxCoolAirVolFlowNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolSAFMAlphaNum), "FlowPerFloorArea")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingSAFMethod = FlowPerFloorArea;
                if (!lNumericBlanks(iCoolFlowPerFloorAreaNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow = Numbers(iCoolFlowPerFloorAreaNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iCoolFlowPerFloorAreaNumericNum), Numbers(iCoolFlowPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input cooling supply air flow per unit conditioned area is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolFlowPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolSAFMAlphaNum), "FractionOfAutosizedCoolingAirflow")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingSAFMethod = FractionOfAutosizedCoolingAirflow;
                if (!lNumericBlanks(iCoolFlowPerFracCoolNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow = Numbers(iCoolFlowPerFracCoolNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iCoolFlowPerFracCoolNumericNum), Numbers(iCoolFlowPerFracCoolNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input fraction of cooling supply air flow rate is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolFlowPerFracCoolNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolSAFMAlphaNum), "FlowPerCoolingCapacity")) {

                state.dataSize->ZoneHVACSizing(zSIndex).CoolingSAFMethod = FlowPerCoolingCapacity;
                if (!lNumericBlanks(iCoolFlowPerCoolCapNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow = Numbers(iCoolFlowPerCoolCapNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iCoolFlowPerCoolCapNumericNum), Numbers(iCoolFlowPerCoolCapNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input cooling supply air flow per unit cooling capacity is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolFlowPerCoolCapNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolSAFMAlphaNum), "None") || lAlphaBlanks(iCoolSAFMAlphaNum)) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingSAFMethod = None;
                state.dataSize->ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow = 0.0;
                // cooling supply air flow rate will not be sized, may be cooling coil does not exist
            } else {
                ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                ShowContinueError(state, "Illegal " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                ErrorsFound = true;
            }
            // Determine supply air flow rate sizing method for heating mode
            if (UtilityRoutines::SameString(Alphas(iHeatSAFMAlphaNum), "SupplyAirFlowRate")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingSAFMethod = SupplyAirFlowRate;
                if (!lNumericBlanks(iMaxHeatAirVolFlowNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow = Numbers(iMaxHeatAirVolFlowNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;

                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iMaxHeatAirVolFlowNumericNum), Numbers(iMaxHeatAirVolFlowNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iMaxHeatAirVolFlowNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatSAFMAlphaNum), "FlowPerFloorArea")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingSAFMethod = FlowPerFloorArea;
                if (!lNumericBlanks(iHeatFlowPerFloorAreaNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow = Numbers(iHeatFlowPerFloorAreaNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iHeatFlowPerFloorAreaNumericNum), Numbers(iHeatFlowPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input heating supply air flow per unit conditioned area is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatFlowPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatSAFMAlphaNum), "FractionOfAutosizedHeatingAirflow")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingSAFMethod = FractionOfAutosizedHeatingAirflow;
                if (!lNumericBlanks(iHeatFlowPerFracCoolNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow = Numbers(iHeatFlowPerFracCoolNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iHeatFlowPerFracCoolNumericNum), Numbers(iHeatFlowPerFracCoolNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input fraction of heating supply air flow rate is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatFlowPerFracCoolNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatSAFMAlphaNum), "FlowPerHeatingCapacity")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingSAFMethod = FlowPerHeatingCapacity;
                if (!lNumericBlanks(iHeatFlowPerHeatCapNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow = Numbers(iHeatFlowPerHeatCapNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow <= 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iHeatFlowPerHeatCapNumericNum), Numbers(iHeatFlowPerHeatCapNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input heating supply air flow per unit heating capacity is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatFlowPerHeatCapNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatSAFMAlphaNum), "None") || lAlphaBlanks(iHeatSAFMAlphaNum)) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingSAFMethod = None;
                state.dataSize->ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow = 0.0;
            } else {
                ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                ShowContinueError(state, "Illegal " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                ErrorsFound = true;
            }

            // Determine supply air flow rate sizing method when cooling or heating is not needed
            if (UtilityRoutines::SameString(Alphas(iNoCoolHeatSAFMAlphaNum), "SupplyAirFlowRate")) {
                state.dataSize->ZoneHVACSizing(zSIndex).NoCoolHeatSAFMethod = SupplyAirFlowRate;
                if (!lNumericBlanks(iMaxNoCoolHeatAirVolFlowNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow = Numbers(iMaxNoCoolHeatAirVolFlowNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum),
                                                 Numbers(iMaxNoCoolHeatAirVolFlowNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iNoCoolHeatSAFMAlphaNum), "FlowPerFloorArea")) {
                state.dataSize->ZoneHVACSizing(zSIndex).NoCoolHeatSAFMethod = FlowPerFloorArea;
                if (!lNumericBlanks(iNoCoolHeatFlowPerFloorAreaNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum),
                                                 Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input supply air flow per unit floor area during no cooling or heating area is saved in
                        // ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iNoCoolHeatSAFMAlphaNum), "FractionOfAutosizedCoolingAirflow")) {
                state.dataSize->ZoneHVACSizing(zSIndex).NoCoolHeatSAFMethod = FractionOfAutosizedCoolingAirflow;
                if (!lNumericBlanks(iNoCoolHeatFlowPerFracCoolNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerFracCoolNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum),
                                                 Numbers(iNoCoolHeatFlowPerFracCoolNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input frcation of cooling supply air flow rate during no cooling or heating area is saved in
                        // ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iNoCoolHeatSAFMAlphaNum), "FractionOfAutosizedHeatingAirflow")) {
                state.dataSize->ZoneHVACSizing(zSIndex).NoCoolHeatSAFMethod = FractionOfAutosizedHeatingAirflow;
                if (!lNumericBlanks(iNoCoolHeatFlowPerFracHeatNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerFracHeatNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum),
                                                 Numbers(iNoCoolHeatFlowPerFracHeatNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    } else {
                        // user input frcation of heating supply air flow rate during no cooling or heating area is saved in
                        // ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iNoCoolHeatSAFMAlphaNum), "None") || lAlphaBlanks(iNoCoolHeatSAFMAlphaNum)) {
                state.dataSize->ZoneHVACSizing(zSIndex).NoCoolHeatSAFMethod = None;
                state.dataSize->ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow = 0.0;
            } else {
                ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                ShowContinueError(state, "Illegal " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                ErrorsFound = true;
            }

            // Determine cooling design capacity of zoneHVAC equipment
            if (UtilityRoutines::SameString(Alphas(iCoolCAPMAlphaNum), "CoolingDesignCapacity")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingCapMethod = CoolingDesignCapacity;
                if (!lNumericBlanks(iCoolDesignCapacityNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity = Numbers(iCoolDesignCapacityNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iCoolDesignCapacityNumericNum), Numbers(iCoolDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolCAPMAlphaNum), "CapacityPerFloorArea")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingCapMethod = CapacityPerFloorArea;
                if (!lNumericBlanks(iCoolCapacityPerFloorAreaNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity = Numbers(iCoolCapacityPerFloorAreaNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity <= 0.0) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iCoolCapacityPerFloorAreaNumericNum),
                                                 Numbers(iCoolCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iCoolCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolCAPMAlphaNum), "FractionOfAutosizedCoolingCapacity")) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
                if (!lNumericBlanks(iCoolFracOfAutosizedCapacityNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity = Numbers(iCoolFracOfAutosizedCapacityNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iCoolFracOfAutosizedCapacityNumericNum),
                                                 Numbers(iCoolFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iCoolFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iCoolCAPMAlphaNum), "None") || lAlphaBlanks(iCoolCAPMAlphaNum)) {
                state.dataSize->ZoneHVACSizing(zSIndex).CoolingCapMethod = None;
            } else {
                ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                ShowContinueError(state, "Illegal " + cAlphaFields(iCoolCAPMAlphaNum) + " = " + Alphas(iCoolCAPMAlphaNum));
                ErrorsFound = true;
            }

            // Determine heating design capacity of zone HVAC equipment
            if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingCapMethod = HeatingDesignCapacity;
                if (!lNumericBlanks(iHeatDesignCapacityNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity = Numbers(iHeatDesignCapacityNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(
                            state,
                            format("Illegal {} = {:.7T}", cNumericFields(iHeatDesignCapacityNumericNum), Numbers(iHeatDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingCapMethod = CapacityPerFloorArea;
                if (!lNumericBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity = Numbers(iHeatCapacityPerFloorAreaNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iHeatCapacityPerFloorAreaNumericNum),
                                                 Numbers(iHeatCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                        // Autosized input is not allowed
                    } else if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state, "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFields(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!lNumericBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity = Numbers(iHeatFracOfAutosizedCapacityNumericNum);
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity == AutoSize)
                        state.dataSize->ZoneHVACSizing(zSIndex).RequestAutoSize = true;
                    if (state.dataSize->ZoneHVACSizing(zSIndex).ScaledHeatingCapacity < 0.0 &&
                        state.dataSize->ZoneHVACSizing(zSIndex).ScaledCoolingCapacity != AutoSize) {
                        ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFields(iHeatFracOfAutosizedCapacityNumericNum),
                                                 Numbers(iHeatFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                    ShowContinueError(state, "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFields(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "None") || lAlphaBlanks(iHeatCAPMAlphaNum)) {
                state.dataSize->ZoneHVACSizing(zSIndex).HeatingCapMethod = None;
            } else {
                ShowSevereError(state, CurrentModuleObject + " = " + state.dataSize->ZoneHVACSizing(zSIndex).Name);
                ShowContinueError(state, "Illegal " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }
        }
    }

    Alphas.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    Numbers.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
    }
}

void GetAirTerminalSizing(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         M.J. Witte
    //       DATE WRITTEN   February 2017

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for the AirTerminal sizing methods object and stores it in
    // appropriate data structure.

    static constexpr std::string_view RoutineName("GetAirTerminalSizing: "); // include trailing blank space

    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int TotalArgs;           // Total number of alpha and numeric arguments (max) for a
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // If errors detected in input
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "DesignSpecification:AirTerminal:Sizing";
    state.dataSize->NumAirTerminalSizingSpec = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);

    if (state.dataSize->NumAirTerminalSizingSpec > 0) {
        state.dataSize->AirTerminalSizingSpec.allocate(state.dataSize->NumAirTerminalSizingSpec);

        // Start Loading the System Input
        for (int zSIndex = 1; zSIndex <= state.dataSize->NumAirTerminalSizingSpec; ++zSIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     zSIndex,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            auto &thisATSizing(state.dataSize->AirTerminalSizingSpec(zSIndex));
            thisATSizing.Name = state.dataIPShortCut->cAlphaArgs(1);
            thisATSizing.DesSensCoolingFrac = state.dataIPShortCut->rNumericArgs(1);
            thisATSizing.DesCoolSATRatio = state.dataIPShortCut->rNumericArgs(2);
            thisATSizing.DesSensHeatingFrac = state.dataIPShortCut->rNumericArgs(3);
            thisATSizing.DesHeatSATRatio = state.dataIPShortCut->rNumericArgs(4);
            thisATSizing.MinOAFrac = state.dataIPShortCut->rNumericArgs(5);
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
    }
}

// Update the sizing for the entire facilty to gather values for reporting - Glazer January 2017
void UpdateFacilitySizing([[maybe_unused]] EnergyPlusData &state, DataGlobalConstants::CallIndicator const CallIndicator)
{
    int NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;

    auto &CalcFacilitySizing(state.dataSize->CalcFacilitySizing);
    auto &CalcFinalFacilitySizing(state.dataSize->CalcFinalFacilitySizing);

    //  test if allocated here
    if (!CalcFacilitySizing.allocated()) {
        CalcFacilitySizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            CalcFacilitySizing(DDNum).DOASHeatAddSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).DOASLatAddSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).CoolOutHumRatSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).CoolOutTempSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).CoolZoneTempSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).CoolLoadSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).HeatOutHumRatSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).HeatOutTempSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).HeatZoneTempSeq.allocate(NumOfTimeStepInDay);
            CalcFacilitySizing(DDNum).HeatLoadSeq.allocate(NumOfTimeStepInDay);

            CalcFacilitySizing(DDNum).DOASHeatAddSeq = 0.;
            CalcFacilitySizing(DDNum).DOASLatAddSeq = 0.;
            CalcFacilitySizing(DDNum).CoolOutHumRatSeq = 0.;
            CalcFacilitySizing(DDNum).CoolOutTempSeq = 0.;
            CalcFacilitySizing(DDNum).CoolZoneTempSeq = 0.;
            CalcFacilitySizing(DDNum).CoolLoadSeq = 0.;
            CalcFacilitySizing(DDNum).HeatOutHumRatSeq = 0.;
            CalcFacilitySizing(DDNum).HeatOutTempSeq = 0.;
            CalcFacilitySizing(DDNum).HeatZoneTempSeq = 0.;
            CalcFacilitySizing(DDNum).HeatLoadSeq = 0.;
        }
    }
    if (!CalcFinalFacilitySizing.DOASHeatAddSeq.allocated()) {
        CalcFinalFacilitySizing.DOASHeatAddSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.DOASLatAddSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.CoolOutHumRatSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.CoolOutTempSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.CoolZoneTempSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.CoolLoadSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.HeatOutHumRatSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.HeatOutTempSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.HeatZoneTempSeq.allocate(NumOfTimeStepInDay);
        CalcFinalFacilitySizing.HeatLoadSeq.allocate(NumOfTimeStepInDay);

        CalcFinalFacilitySizing.DOASHeatAddSeq = 0.;
        CalcFinalFacilitySizing.DOASLatAddSeq = 0.;
        CalcFinalFacilitySizing.CoolOutHumRatSeq = 0.;
        CalcFinalFacilitySizing.CoolOutTempSeq = 0.;
        CalcFinalFacilitySizing.CoolZoneTempSeq = 0.;
        CalcFinalFacilitySizing.CoolLoadSeq = 0.;
        CalcFinalFacilitySizing.HeatOutHumRatSeq = 0.;
        CalcFinalFacilitySizing.HeatOutTempSeq = 0.;
        CalcFinalFacilitySizing.HeatZoneTempSeq = 0.;
        CalcFinalFacilitySizing.HeatLoadSeq = 0.;
    }
    if (CallIndicator == DataGlobalConstants::CallIndicator::BeginDay) {
        CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatDDNum = state.dataSize->CurOverallSimDay;
        CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolDDNum = state.dataSize->CurOverallSimDay;
    } else if (CallIndicator == DataGlobalConstants::CallIndicator::DuringDay) {
        int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
        // save the results of the ideal zone component calculation in the CalcZoneSizing sequence variables
        Real64 sumCoolLoad = 0.;
        Real64 sumHeatLoad = 0.;
        Real64 wghtdCoolZoneTemp = 0.;
        Real64 wghtdHeatZoneTemp = 0.;
        Real64 wghtdCoolHumRat = 0.;
        Real64 wghtdHeatHumRat = 0.;
        Real64 wghtdCoolDOASHeatAdd = 0.;
        Real64 wghtdCoolDOASLatAdd = 0.;
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            Real64 curCoolLoad = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepInDay);
            if (curCoolLoad > 0.0) {
                sumCoolLoad += curCoolLoad;
                wghtdCoolZoneTemp +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay) * curCoolLoad;
                wghtdCoolHumRat +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepInDay) * curCoolLoad;
                wghtdCoolDOASHeatAdd +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatAddSeq(TimeStepInDay) * curCoolLoad;
                wghtdCoolDOASLatAdd +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASLatAddSeq(TimeStepInDay) * curCoolLoad;
            }
            Real64 curHeatLoad = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepInDay);
            if (curHeatLoad > 0.0) {
                sumHeatLoad += curHeatLoad;
                wghtdHeatZoneTemp +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay) * curHeatLoad;
                wghtdHeatHumRat +=
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepInDay) * curHeatLoad;
            }
        }

        CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolLoadSeq(TimeStepInDay) = sumCoolLoad;
        CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatLoadSeq(TimeStepInDay) = sumHeatLoad;

        if (sumCoolLoad != 0.) {
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolZoneTempSeq(TimeStepInDay) = wghtdCoolZoneTemp / sumCoolLoad;
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolOutHumRatSeq(TimeStepInDay) = wghtdCoolHumRat / sumCoolLoad;
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).DOASHeatAddSeq(TimeStepInDay) = wghtdCoolDOASHeatAdd / sumCoolLoad;
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).DOASLatAddSeq(TimeStepInDay) = wghtdCoolDOASLatAdd / sumCoolLoad;
        }
        if (sumHeatLoad != 0.) {
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatZoneTempSeq(TimeStepInDay) = wghtdHeatZoneTemp / sumHeatLoad;
            CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatOutHumRatSeq(TimeStepInDay) = wghtdHeatHumRat / sumHeatLoad;
        }

    } else if (CallIndicator == DataGlobalConstants::CallIndicator::EndDay) {
        for (int TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex) {
            if (CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolLoadSeq(TimeStepIndex) >
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).DesCoolLoad) {
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).DesCoolLoad =
                    CalcFacilitySizing(state.dataSize->CurOverallSimDay).CoolLoadSeq(TimeStepIndex);
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).TimeStepNumAtCoolMax = TimeStepIndex;
            }
            if (CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatLoadSeq(TimeStepIndex) >
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).DesHeatLoad) {
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).DesHeatLoad =
                    CalcFacilitySizing(state.dataSize->CurOverallSimDay).HeatLoadSeq(TimeStepIndex);
                CalcFacilitySizing(state.dataSize->CurOverallSimDay).TimeStepNumAtHeatMax = TimeStepIndex;
            }
        }

    } else if (CallIndicator == DataGlobalConstants::CallIndicator::EndZoneSizingCalc) {
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            if (CalcFacilitySizing(DDNum).DesCoolLoad > CalcFinalFacilitySizing.DesCoolLoad) {
                CalcFinalFacilitySizing.DesCoolLoad = CalcFacilitySizing(DDNum).DesCoolLoad;
                CalcFinalFacilitySizing.TimeStepNumAtCoolMax = CalcFacilitySizing(DDNum).TimeStepNumAtCoolMax;
                CalcFinalFacilitySizing.CoolDDNum = CalcFacilitySizing(DDNum).CoolDDNum;
                for (int TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex) {
                    CalcFinalFacilitySizing.CoolOutHumRatSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).CoolOutHumRatSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.CoolOutTempSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).CoolOutTempSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.CoolZoneTempSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).CoolZoneTempSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.DOASHeatAddSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).DOASHeatAddSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.DOASLatAddSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).DOASLatAddSeq(TimeStepIndex);
                }
            }
            if (CalcFacilitySizing(DDNum).DesHeatLoad > CalcFinalFacilitySizing.DesHeatLoad) {
                CalcFinalFacilitySizing.DesHeatLoad = CalcFacilitySizing(DDNum).DesHeatLoad;
                CalcFinalFacilitySizing.TimeStepNumAtHeatMax = CalcFacilitySizing(DDNum).TimeStepNumAtHeatMax;
                CalcFinalFacilitySizing.HeatDDNum = CalcFacilitySizing(DDNum).HeatDDNum;
                for (int TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex) {
                    CalcFinalFacilitySizing.HeatOutHumRatSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).HeatOutHumRatSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.HeatOutTempSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).HeatOutTempSeq(TimeStepIndex);
                    CalcFinalFacilitySizing.HeatZoneTempSeq(TimeStepIndex) = CalcFacilitySizing(DDNum).HeatZoneTempSeq(TimeStepIndex);
                }
            }
        }
    }
}

void UpdateTermUnitFinalZoneSizing(EnergyPlusData &state)
{
    // Move data from FinalZoneSizing to TermUnitFinalZoneSizing and apply terminal unit sizing adjustments
    // Called once to initialize before system sizing
    // M.J. Witte, July 2017

    for (int termUnitSizingIndex = 1; termUnitSizingIndex <= state.dataSize->NumAirTerminalUnits; ++termUnitSizingIndex) {
        auto &thisTUFZSizing(state.dataSize->TermUnitFinalZoneSizing(termUnitSizingIndex));
        auto &thisTUSizing(state.dataSize->TermUnitSizing(termUnitSizingIndex));
        int ctrlZoneNum = thisTUSizing.CtrlZoneNum;
        auto const &thisFZSizing(state.dataSize->FinalZoneSizing(ctrlZoneNum));

        // Copy everything from FinalZoneSizing to TermUnitFinalZoneSizing
        thisTUFZSizing = thisFZSizing;
        thisTUFZSizing.ADUName = thisTUSizing.ADUName;

        if (state.dataSize->NumAirTerminalSizingSpec > 0) {
            // Apply DesignSpecification:AirTerminal:Sizing adjustments - default ratios are 1.0
            Real64 minOAFrac = thisTUSizing.SpecMinOAFrac;
            // Outdoor air
            thisTUFZSizing.MinOA = thisFZSizing.MinOA * minOAFrac;
            thisTUFZSizing.TotalOAFromPeople = thisFZSizing.TotalOAFromPeople * minOAFrac;
            thisTUFZSizing.TotalOAFromArea = thisFZSizing.TotalOAFromArea * minOAFrac;
            Real64 minOACoolMassFlow = thisTUFZSizing.MinOA * thisFZSizing.DesCoolDens;
            Real64 minOAHeatMassFlow = thisTUFZSizing.MinOA * thisFZSizing.DesHeatDens;
            // Cooling
            Real64 coolFlowRatio = 1.0;
            if (thisTUSizing.SpecDesCoolSATRatio > 0.0) {
                coolFlowRatio = thisTUSizing.SpecDesSensCoolingFrac / thisTUSizing.SpecDesCoolSATRatio;
            } else {
                coolFlowRatio = thisTUSizing.SpecDesSensCoolingFrac;
            }
            Real64 coolLoadRatio = thisTUSizing.SpecDesSensCoolingFrac;
            thisTUFZSizing.DesCoolLoad = thisFZSizing.DesCoolLoad * coolLoadRatio;
            thisTUFZSizing.CoolMassFlow = thisFZSizing.CoolMassFlow * coolFlowRatio; // this field in TUFSizing doesn't appear to be used
            thisTUFZSizing.CoolLoadSeq = thisFZSizing.CoolLoadSeq * coolLoadRatio;   // this field in TUFSizing doesn't appear to be used
            thisTUFZSizing.NonAirSysDesCoolLoad = thisFZSizing.NonAirSysDesCoolLoad * coolLoadRatio;
            thisTUFZSizing.NonAirSysDesCoolVolFlow = thisFZSizing.NonAirSysDesCoolVolFlow * coolFlowRatio;
            // Adjust DesCoolVolFlow, DesCoolMassFlow, and CoolFlowSeq with cooling frac, SAT ratio, and minOA frac adjustments
            thisTUFZSizing.DesCoolVolFlow = thisTUSizing.applyTermUnitSizingCoolFlow(thisFZSizing.DesCoolVolFlow, thisFZSizing.DesCoolVolFlowNoOA);
            thisTUFZSizing.DesCoolVolFlow = max(thisTUFZSizing.DesCoolVolFlow, thisTUFZSizing.MinOA);
            thisTUFZSizing.DesCoolVolFlowNoOA = thisFZSizing.DesCoolVolFlowNoOA * coolFlowRatio;
            thisTUFZSizing.DesCoolMassFlow = thisTUFZSizing.DesCoolVolFlow * thisFZSizing.DesCoolDens;
            thisTUFZSizing.DesCoolMassFlow = max(thisTUFZSizing.DesCoolMassFlow, minOACoolMassFlow);
            thisTUFZSizing.DesCoolMassFlowNoOA = thisTUFZSizing.DesCoolVolFlowNoOA * thisFZSizing.DesCoolDens;
            for (int timeIndex = 1; timeIndex <= (state.dataGlobal->NumOfTimeStepInHour * 24); ++timeIndex) {
                thisTUFZSizing.CoolFlowSeq(timeIndex) =
                    thisTUSizing.applyTermUnitSizingCoolFlow(thisFZSizing.CoolFlowSeq(timeIndex), thisFZSizing.CoolFlowSeqNoOA(timeIndex));
                thisTUFZSizing.CoolFlowSeq(timeIndex) = max(thisTUFZSizing.CoolFlowSeq(timeIndex), minOACoolMassFlow);
                thisTUFZSizing.CoolFlowSeqNoOA(timeIndex) = thisFZSizing.CoolFlowSeqNoOA(timeIndex) * coolFlowRatio;
            }
            // Adjust for possible MinOA impact on DesCoolVolFlowMin, with cooling frac adjustment but no SAT adjustment
            thisTUFZSizing.DesCoolMinAirFlow =
                thisFZSizing.DesCoolMinAirFlow * thisTUSizing.SpecDesSensCoolingFrac; // no SAT adjustment, this is a straight flow rate input
            thisTUFZSizing.DesCoolMinAirFlow2 =
                thisFZSizing.DesCoolMinAirFlow2 * thisTUSizing.SpecDesSensCoolingFrac; // no SAT adjustment, this is based on area
            thisTUFZSizing.DesCoolVolFlowMin = max(thisTUFZSizing.DesCoolMinAirFlow,
                                                   thisTUFZSizing.DesCoolMinAirFlow2,
                                                   thisTUFZSizing.DesCoolVolFlow * thisTUFZSizing.DesCoolMinAirFlowFrac);

            // Heating
            Real64 heatFlowRatio = 1.0;
            if (thisTUSizing.SpecDesHeatSATRatio > 0.0) {
                heatFlowRatio = thisTUSizing.SpecDesSensHeatingFrac / thisTUSizing.SpecDesHeatSATRatio;
            } else {
                heatFlowRatio = thisTUSizing.SpecDesSensHeatingFrac;
            }
            Real64 heatLoadRatio = thisTUSizing.SpecDesSensHeatingFrac;
            thisTUFZSizing.DesHeatLoad = thisFZSizing.DesHeatLoad * heatLoadRatio;
            thisTUFZSizing.HeatMassFlow = thisFZSizing.HeatMassFlow * heatFlowRatio; // this field in TUFSizing doesn't appear to be used
            thisTUFZSizing.HeatLoadSeq = thisFZSizing.HeatLoadSeq * heatLoadRatio;   // this field in TUFSizing doesn't appear to be used
            thisTUFZSizing.NonAirSysDesHeatLoad = thisFZSizing.NonAirSysDesHeatLoad * heatLoadRatio;
            thisTUFZSizing.NonAirSysDesHeatVolFlow = thisFZSizing.NonAirSysDesHeatVolFlow * heatFlowRatio;
            // Adjust DesHeatVolFlow, DesHeatMassFlow, and HeatFlowSeq with Heating frac, SAT ratio, and minOA frac adjustments
            thisTUFZSizing.DesHeatVolFlow = thisTUSizing.applyTermUnitSizingHeatFlow(thisFZSizing.DesHeatVolFlow, thisFZSizing.DesHeatVolFlowNoOA);
            thisTUFZSizing.DesHeatVolFlow = max(thisTUFZSizing.DesHeatVolFlow, thisTUFZSizing.MinOA);
            thisTUFZSizing.DesHeatVolFlowNoOA = thisFZSizing.DesHeatVolFlowNoOA * heatFlowRatio;
            thisTUFZSizing.DesHeatMassFlow = thisTUFZSizing.DesHeatVolFlow * thisFZSizing.DesHeatDens;
            thisTUFZSizing.DesHeatMassFlow = max(thisTUFZSizing.DesHeatMassFlow, minOAHeatMassFlow);
            thisTUFZSizing.DesHeatMassFlowNoOA = thisTUFZSizing.DesHeatVolFlowNoOA * thisFZSizing.DesHeatDens;
            for (int timeIndex = 1; timeIndex <= (state.dataGlobal->NumOfTimeStepInHour * 24); ++timeIndex) {
                thisTUFZSizing.HeatFlowSeq(timeIndex) =
                    thisTUSizing.applyTermUnitSizingHeatFlow(thisFZSizing.HeatFlowSeq(timeIndex), thisFZSizing.HeatFlowSeqNoOA(timeIndex));
                thisTUFZSizing.HeatFlowSeq(timeIndex) = max(thisTUFZSizing.HeatFlowSeq(timeIndex), minOAHeatMassFlow);
                thisTUFZSizing.HeatFlowSeqNoOA(timeIndex) = thisFZSizing.HeatFlowSeqNoOA(timeIndex) * heatFlowRatio;
            }
            // DesHeatVolFlowMax is a mixed bag, so just repeat the original comparison from UpdateZoneSizing using the new flows
            thisTUFZSizing.DesHeatMaxAirFlow =
                thisFZSizing.DesHeatMaxAirFlow * thisTUSizing.SpecDesSensHeatingFrac; // no SAT adjustment, this is a straight flow rate input
            thisTUFZSizing.DesHeatMaxAirFlow2 =
                thisFZSizing.DesHeatMaxAirFlow2 * thisTUSizing.SpecDesSensHeatingFrac; // no SAT adjustment, this is based on area
            thisTUFZSizing.DesHeatVolFlowMax =
                max(thisTUFZSizing.DesHeatMaxAirFlow,
                    thisTUFZSizing.DesHeatMaxAirFlow2,
                    max(thisTUFZSizing.DesCoolVolFlow, thisTUFZSizing.DesHeatVolFlow) * thisTUFZSizing.DesHeatMaxAirFlowFrac);
            // Outdoor air fractions
            if (thisTUFZSizing.DesCoolVolFlow > 0.0) {
                thisTUFZSizing.DesCoolOAFlowFrac = min(thisFZSizing.MinOA / thisTUFZSizing.DesCoolVolFlow, 1.0);
            } else {
                thisTUFZSizing.DesCoolOAFlowFrac = 0.0;
            }
            if (thisTUFZSizing.DesHeatVolFlow > 0.0) {
                thisTUFZSizing.DesHeatOAFlowFrac = min(thisFZSizing.MinOA / thisTUFZSizing.DesHeatVolFlow, 1.0);
            } else {
                thisTUFZSizing.DesHeatOAFlowFrac = 0.0;
            }
        }
    }
}
} // namespace EnergyPlus::SizingManager
