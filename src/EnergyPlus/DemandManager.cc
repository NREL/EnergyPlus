// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::DemandManager {

// MODULE INFORMATION:
//       AUTHOR         Peter Graham Ellis
//       DATE WRITTEN   July 2005
//       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module provides controls for demand limiting various loads.

// METHODOLOGY EMPLOYED:
// ManageDemand is called from within the ManageHVAC routine after the first pass through SimHVAC, but
// _before_ any variables are reported or histories are updated.  If the metered demand is above the
// limit action is taken using the various demand managers to reduce loads.  Exterior energy use, zone
// heat balance, and HVAC system are then resimulated as necessary.  It is possible to iterate several
// times through ManageDemand before the final demand managers are established and the timestep can be
// completed.

constexpr std::array<std::string_view, static_cast<int>(ManagerType::Num)> ManagerNamesUC{"DEMANDMANAGER:EXTERIORLIGHTS",
                                                                                          "DEMANDMANAGER:LIGHTS",
                                                                                          "DEMANDMANAGER:ELECTRICEQUIPMENT",
                                                                                          "DEMANDMANAGER:THERMOSTATS",
                                                                                          "DEMANDMANAGER:VENTILATION"};
constexpr std::array<std::string_view, static_cast<int>(ManagePriorityType::Num)> ManagePriorityNamesUC{"SEQUENTIAL", "OPTIMAL", "ALL"};
constexpr std::array<std::string_view, static_cast<int>(ManagerLimit::Num)> ManagerLimitNamesUC{"OFF", "FIXED", "VARIABLE", "REDUCTIONRATIO"};
constexpr std::array<std::string_view, static_cast<int>(ManagerLimit::Num)> ManagerLimitVentNamesUC{"OFF", "FIXEDRATE", "VARIABLE", "REDUCTIONRATIO"};
constexpr std::array<std::string_view, static_cast<int>(ManagerSelection::Num)> ManagerSelectionNamesUC{"ALL", "ROTATEMANY", "ROTATEONE"};

void ManageDemand(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Locals
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ListNum;

    if (state.dataDemandManager->GetInput && !state.dataGlobal->DoingSizing) {
        GetDemandManagerInput(state);
        GetDemandManagerListInput(state);
        state.dataDemandManager->GetInput = false;
    }

    if (state.dataDemandManager->NumDemandManagerList > 0) {

        if (state.dataGlobal->WarmupFlag) {
            state.dataDemandManager->BeginDemandSim = true;
            if (state.dataDemandManager->ClearHistory) {
                // Clear historical variables
                for (ListNum = 1; ListNum <= state.dataDemandManager->NumDemandManagerList; ++ListNum) {
                    state.dataDemandManager->DemandManagerList(ListNum).History = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).MeterDemand = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).AverageDemand = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).PeakDemand = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).ScheduledLimit = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).DemandLimit = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).AvoidedDemand = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).OverLimit = 0.0;
                    state.dataDemandManager->DemandManagerList(ListNum).OverLimitDuration = 0.0;
                } // ListNum

                // Clear demand manager variables
                for (auto &e : state.dataDemandManager->DemandMgr) {
                    e.Active = false;
                    e.ElapsedTime = 0;
                    e.ElapsedRotationTime = 0;
                    e.RotatedLoadNum = 0;
                }
            }
            state.dataDemandManager->ClearHistory = false;
        }

        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing) {

            if (state.dataDemandManager->BeginDemandSim) {
                state.dataDemandManager->BeginDemandSim = false;
                state.dataDemandManager->ClearHistory = true;
            }

            state.dataDemandManager->DemandManagerExtIterations = 0;
            state.dataDemandManager->DemandManagerHBIterations = 0;
            state.dataDemandManager->DemandManagerHVACIterations = 0;

            state.dataDemandManager->firstTime = true;
            state.dataDemandManager->ResimExt = false;
            state.dataDemandManager->ResimHB = false;
            state.dataDemandManager->ResimHVAC = false;

            while (state.dataDemandManager->firstTime || state.dataDemandManager->ResimExt || state.dataDemandManager->ResimHB ||
                   state.dataDemandManager->ResimHVAC) {
                state.dataDemandManager->firstTime = false;

                Resimulate(state, state.dataDemandManager->ResimExt, state.dataDemandManager->ResimHB, state.dataDemandManager->ResimHVAC);
                state.dataDemandManager->ResimExt = false;
                state.dataDemandManager->ResimHB = false;
                state.dataDemandManager->ResimHVAC = false;

                SurveyDemandManagers(state); // Determines which Demand Managers can reduce demand

                for (ListNum = 1; ListNum <= state.dataDemandManager->NumDemandManagerList; ++ListNum) {
                    SimulateDemandManagerList(
                        state, ListNum, state.dataDemandManager->ResimExt, state.dataDemandManager->ResimHB, state.dataDemandManager->ResimHVAC);
                } // ListNum

                ActivateDemandManagers(state); // Sets limits on loads

                if (state.dataDemandManager->DemandManagerExtIterations + state.dataDemandManager->DemandManagerHBIterations +
                        state.dataDemandManager->DemandManagerHVACIterations >
                    500) {
                    // This error can only happen if there is a bug in the code
                    ShowFatalError(state, "Too many DemandManager iterations. (>500)");
                    break;
                }
            }

            for (ListNum = 1; ListNum <= state.dataDemandManager->NumDemandManagerList; ++ListNum) {
                ReportDemandManagerList(state, ListNum);
            } // ListNum
        }
    }
}

void SimulateDemandManagerList(EnergyPlusData &state,
                               int const ListNum,
                               bool &ResimExt, // Flag to resimulate the exterior energy use simulation
                               bool &ResimHB,  // Flag to resimulate the heat balance simulation (including HVAC)
                               bool &ResimHVAC // Flag to resimulate the HVAC simulation
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MgrNum;
    int MgrPtr;
    Real64 AverageDemand;
    Real64 OverLimit;
    bool OnPeak;

    auto &DemandManagerList(state.dataDemandManager->DemandManagerList);
    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    DemandManagerList(ListNum).ScheduledLimit = GetCurrentScheduleValue(state, DemandManagerList(ListNum).LimitSchedule);
    DemandManagerList(ListNum).DemandLimit = DemandManagerList(ListNum).ScheduledLimit * DemandManagerList(ListNum).SafetyFraction;

    DemandManagerList(ListNum).MeterDemand =
        GetInstantMeterValue(state, DemandManagerList(ListNum).Meter, OutputProcessor::TimeStepType::Zone) / state.dataGlobal->TimeStepZoneSec +
        GetInstantMeterValue(state, DemandManagerList(ListNum).Meter, OutputProcessor::TimeStepType::System) /
            (TimeStepSys * DataGlobalConstants::SecInHour);

    // Calculate average demand over the averaging window including the current timestep meter demand
    AverageDemand = DemandManagerList(ListNum).AverageDemand +
                    (DemandManagerList(ListNum).MeterDemand - DemandManagerList(ListNum).History(1)) / DemandManagerList(ListNum).AveragingWindow;

    if (DemandManagerList(ListNum).PeakSchedule == 0) {
        OnPeak = true;
    } else {
        if (GetCurrentScheduleValue(state, DemandManagerList(ListNum).PeakSchedule) == 1) {
            OnPeak = true;
        } else {
            OnPeak = false;
        }
    }

    if (OnPeak) {
        OverLimit = AverageDemand - DemandManagerList(ListNum).DemandLimit;

        if (OverLimit > 0.0) {

            switch (DemandManagerList(ListNum).ManagerPriority) {
            case ManagePriorityType::Sequential: { // Activate first Demand Manager that can reduce demand

                for (MgrNum = 1; MgrNum <= DemandManagerList(ListNum).NumOfManager; ++MgrNum) {
                    MgrPtr = DemandManagerList(ListNum).Manager(MgrNum);

                    if (DemandMgr(MgrPtr).CanReduceDemand) {
                        DemandMgr(MgrPtr).Activate = true;

                        switch (DemandMgr(MgrPtr).Type) {
                        case ManagerType::ExtLights: {
                            ResimExt = true;
                        } break;
                        case ManagerType::Lights:
                        case ManagerType::ElecEquip: {
                            ResimHB = true;
                            ResimHVAC = true;
                        } break;
                        case ManagerType::Thermostats:
                        case ManagerType::Ventilation: {
                            ResimHVAC = true;
                        } break;
                        default:
                            break;
                        }

                        break; // Leave the loop
                    }
                } // MgrNum

            } break;
            case ManagePriorityType::Optimal: {
                // Not yet implemented

            } break;
            case ManagePriorityType::All: { // Activate ALL Demand Managers that can reduce demand

                for (MgrNum = 1; MgrNum <= DemandManagerList(ListNum).NumOfManager; ++MgrNum) {
                    MgrPtr = DemandManagerList(ListNum).Manager(MgrNum);

                    if (DemandMgr(MgrPtr).CanReduceDemand) {
                        DemandMgr(MgrPtr).Activate = true;

                        switch (DemandMgr(MgrPtr).Type) {
                        case ManagerType::ExtLights: {
                            ResimExt = true;
                        } break;
                        case ManagerType::Lights:
                        case ManagerType::ElecEquip: {
                            ResimHB = true;
                            ResimHVAC = true;
                        } break;
                        case ManagerType::Thermostats:
                        case ManagerType::Ventilation: {
                            ResimHVAC = true;
                        } break;
                        default:
                            break;
                        }
                    }
                } // MgrNum
            } break;
            default:
                break;
            }
        }
    }
}

void GetDemandManagerListInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the DEMAND MANAGER LIST input from the input file.

    // METHODOLOGY EMPLOYED:
    // Standard EnergyPlus methodology.

    // Using/Aliasing
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool ErrorsFound(false);

    constexpr std::string_view cCurrentModuleObject = "DemandManagerAssignmentList";
    state.dataDemandManager->NumDemandManagerList = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataDemandManager->NumDemandManagerList > 0) {

        state.dataDemandManager->DemandManagerList.allocate(state.dataDemandManager->NumDemandManagerList);

        for (int ListNum = 1; ListNum <= state.dataDemandManager->NumDemandManagerList; ++ListNum) {

            auto &thisDemandMgrList = state.dataDemandManager->DemandManagerList(ListNum);

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     ListNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            thisDemandMgrList.Name = state.dataIPShortCut->cAlphaArgs(1);

            thisDemandMgrList.Meter = GetMeterIndex(state, state.dataIPShortCut->cAlphaArgs(2));

            if (thisDemandMgrList.Meter == 0) {
                ShowSevereError(state, format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(2), state.dataIPShortCut->cAlphaArgs(2)));
                ShowContinueError(state, format("Entered in {} = {}", cCurrentModuleObject, thisDemandMgrList.Name));
                ErrorsFound = true;

            } else {
                if ((state.dataOutputProcessor->EnergyMeters(thisDemandMgrList.Meter).ResourceType == "Electricity") ||
                    (state.dataOutputProcessor->EnergyMeters(thisDemandMgrList.Meter).ResourceType == "ElectricityNet")) {
                } else {
                    ShowSevereError(state,
                                    format("{} = \"{}\" invalid value {} = \"{}\".",
                                           cCurrentModuleObject,
                                           thisDemandMgrList.Name,
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           state.dataIPShortCut->cAlphaArgs(2)));
                    ShowContinueError(state, "Only Electricity and ElectricityNet meters are currently allowed.");
                    ErrorsFound = true;
                }
            }

            // Further checking for conflicting DEMAND MANAGER LISTs

            if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                thisDemandMgrList.LimitSchedule = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));

                if (thisDemandMgrList.LimitSchedule == 0) {
                    ShowSevereError(state,
                                    format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                           cCurrentModuleObject,
                                           thisDemandMgrList.Name,
                                           state.dataIPShortCut->cAlphaFieldNames(3),
                                           state.dataIPShortCut->cAlphaArgs(3)));
                    ErrorsFound = true;
                }
            }

            thisDemandMgrList.SafetyFraction = state.dataIPShortCut->rNumericArgs(1);

            if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                thisDemandMgrList.BillingSchedule = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));

                if (thisDemandMgrList.BillingSchedule == 0) {
                    ShowSevereError(state,
                                    format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                           cCurrentModuleObject,
                                           thisDemandMgrList.Name,
                                           state.dataIPShortCut->cAlphaFieldNames(4),
                                           state.dataIPShortCut->cAlphaArgs(4)));
                    ErrorsFound = true;
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                thisDemandMgrList.PeakSchedule = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));

                if (thisDemandMgrList.PeakSchedule == 0) {
                    ShowSevereError(state,
                                    format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                           cCurrentModuleObject,
                                           thisDemandMgrList.Name,
                                           state.dataIPShortCut->cAlphaFieldNames(5),
                                           state.dataIPShortCut->cAlphaArgs(5)));
                    ErrorsFound = true;
                }
            }

            thisDemandMgrList.AveragingWindow = max(int(state.dataIPShortCut->rNumericArgs(2) / state.dataGlobal->MinutesPerTimeStep), 1);
            // Round to nearest timestep
            // Can make this fancier to include windows that do not fit the timesteps
            thisDemandMgrList.History.allocate(thisDemandMgrList.AveragingWindow);
            thisDemandMgrList.History = 0.0;

            // Validate Demand Manager Priority
            thisDemandMgrList.ManagerPriority = static_cast<ManagePriorityType>(
                getEnumerationValue(ManagePriorityNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(6))));
            ErrorsFound = ErrorsFound || (thisDemandMgrList.ManagerPriority == ManagePriorityType::Invalid);

            // Get DEMAND MANAGER Type and Name pairs
            thisDemandMgrList.NumOfManager = int((NumAlphas - 6) / 2.0);

            if (thisDemandMgrList.NumOfManager > 0) {
                thisDemandMgrList.Manager.allocate(thisDemandMgrList.NumOfManager);
                for (int MgrNum = 1; MgrNum <= thisDemandMgrList.NumOfManager; ++MgrNum) {

                    auto &thisManager = thisDemandMgrList.Manager(MgrNum);
                    // Validate DEMAND MANAGER Type
                    ManagerType MgrType = static_cast<ManagerType>(
                        getEnumerationValue(ManagerNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(MgrNum * 2 + 5))));
                    if (MgrType != ManagerType::Invalid) {
                        thisManager =
                            UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(MgrNum * 2 + 6), state.dataDemandManager->DemandMgr);
                        if (thisManager == 0) {
                            ShowSevereError(state,
                                            format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                                   cCurrentModuleObject,
                                                   thisDemandMgrList.Name,
                                                   state.dataIPShortCut->cAlphaFieldNames(MgrNum * 2 + 6),
                                                   state.dataIPShortCut->cAlphaArgs(MgrNum * 2 + 6)));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid value {} = \"{}\".",
                                               cCurrentModuleObject,
                                               thisDemandMgrList.Name,
                                               state.dataIPShortCut->cAlphaFieldNames(MgrNum * 2 + 5),
                                               state.dataIPShortCut->cAlphaArgs(MgrNum * 2 + 5)));
                        ErrorsFound = true;
                    }

                    // Check that each is not already referenced using %DemandManagerList field

                } // MgrNum
            }

            // Setup report variables
            SetupOutputVariable(state,
                                "Demand Manager Meter Demand Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.MeterDemand,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Average Demand Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.AverageDemand,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Peak Demand Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.PeakDemand,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Scheduled Limit Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.ScheduledLimit,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Demand Limit Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.DemandLimit,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Over Limit Power",
                                OutputProcessor::Unit::W,
                                thisDemandMgrList.OverLimit,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                thisDemandMgrList.Name);

            SetupOutputVariable(state,
                                "Demand Manager Over Limit Time",
                                OutputProcessor::Unit::hr,
                                thisDemandMgrList.OverLimitDuration,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                thisDemandMgrList.Name);

            if (ErrorsFound) {
                ShowFatalError(state, format("Errors found in processing input for {}.", cCurrentModuleObject));
            }

        } // ListNum

        // Iteration diagnostic reporting for all DEMAND MANAGER LISTs
        SetupOutputVariable(state,
                            "Demand Manager Exterior Energy Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerExtIterations,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "ManageDemand");

        SetupOutputVariable(state,
                            "Demand Manager Heat Balance Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerHBIterations,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "ManageDemand");

        SetupOutputVariable(state,
                            "Demand Manager HVAC Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerHVACIterations,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "ManageDemand");
    }
}

void GetDemandManagerInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the DEMAND MANAGER input from the input file.

    // Using/Aliasing
    using MixedAir::GetOAController;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumDemandMgrExtLights;
    int NumDemandMgrLights;
    int NumDemandMgrElecEquip;
    int NumDemandMgrThermostats;
    int NumDemandMgrVentilation;
    int MgrNum;
    int StartIndex;
    int EndIndex;
    int LoadNum;
    int LoadPtr;
    int NumAlphas;            // Number of elements in the alpha array
    int NumNums;              // Number of elements in the numeric array
    int MaxAlphas;            // Max number of elements in the alpha array
    int MaxNums;              // Max number of elements in the numeric array
    int NumParams;            // Number of arguments total in an ObjectDef
    int IOStat;               // IO Status when calling get input subroutine
    Array1D_string AlphArray; // Character string data
    Array1D<Real64> NumArray; // Numeric data
    bool ErrorsFound(false);
    std::string CurrentModuleObject; // for ease in renaming.
    int Item;
    int Item1;

    MaxAlphas = 0;
    MaxNums = 0;
    CurrentModuleObject = "DemandManager:ExteriorLights";
    NumDemandMgrExtLights = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (NumDemandMgrExtLights > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
    }
    CurrentModuleObject = "DemandManager:Lights";
    NumDemandMgrLights = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (NumDemandMgrLights > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
    }
    CurrentModuleObject = "DemandManager:ElectricEquipment";
    NumDemandMgrElecEquip = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (NumDemandMgrElecEquip > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
    }
    CurrentModuleObject = "DemandManager:Thermostats";
    NumDemandMgrThermostats = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (NumDemandMgrThermostats > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
    }
    CurrentModuleObject = "DemandManager:Ventilation";
    NumDemandMgrVentilation = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (NumDemandMgrVentilation > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
    }

    state.dataDemandManager->NumDemandMgr =
        NumDemandMgrExtLights + NumDemandMgrLights + NumDemandMgrElecEquip + NumDemandMgrThermostats + NumDemandMgrVentilation;

    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    if (state.dataDemandManager->NumDemandMgr > 0) {
        AlphArray.dimension(MaxAlphas, std::string());
        NumArray.dimension(MaxNums, 0.0);

        DemandMgr.allocate(state.dataDemandManager->NumDemandMgr);
        state.dataDemandManager->UniqueDemandMgrNames.reserve(state.dataDemandManager->NumDemandMgr);

        // Get input for DemandManager:ExteriorLights
        StartIndex = 1;
        EndIndex = NumDemandMgrExtLights;

        CurrentModuleObject = "DemandManager:ExteriorLights";

        for (MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     MgrNum - StartIndex + 1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataDemandManager->UniqueDemandMgrNames,
                                                     AlphArray(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            DemandMgr(MgrNum).Name = AlphArray(1);

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::ExtLights;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           AlphArray(2)));
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            DemandMgr(MgrNum).LimitControl =
                static_cast<ManagerLimit>(getEnumerationValue(ManagerLimitNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(3))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).LimitControl == ManagerLimit::Invalid);

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            DemandMgr(MgrNum).SelectionControl =
                static_cast<ManagerSelection>(getEnumerationValue(ManagerSelectionNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(4))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).SelectionControl == ManagerSelection::Invalid);

            if (NumArray(4) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(4);

            DemandMgr(MgrNum).NumOfLoads = NumAlphas - 4;

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);

                for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataExteriorEnergyUse->ExteriorLights);

                    if (LoadPtr > 0) {
                        DemandMgr(MgrNum).Load(LoadNum) = LoadPtr;

                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                               CurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4),
                                               AlphArray(LoadNum + 4)));
                        ErrorsFound = true;
                    }
                } // LoadNum
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid value for number of loads.", CurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
                ErrorsFound = true;
            }

        } // MgrNum

        // Get input for DemandManager:Lights
        StartIndex = EndIndex + 1;
        EndIndex += NumDemandMgrLights;

        CurrentModuleObject = "DemandManager:Lights";

        for (MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     MgrNum - StartIndex + 1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataDemandManager->UniqueDemandMgrNames,
                                                     AlphArray(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            DemandMgr(MgrNum).Name = AlphArray(1);

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::Lights;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           AlphArray(2)));
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            DemandMgr(MgrNum).LimitControl =
                static_cast<ManagerLimit>(getEnumerationValue(ManagerLimitNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(3))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).LimitControl == ManagerLimit::Invalid);

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            DemandMgr(MgrNum).SelectionControl =
                static_cast<ManagerSelection>(getEnumerationValue(ManagerSelectionNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(4))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).SelectionControl == ManagerSelection::Invalid);

            if (NumArray(4) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(4);

            // Count actual pointers to controlled zones
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum) {
                LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataInternalHeatGains->lightsObjects);
                if (LoadPtr > 0) {
                    DemandMgr(MgrNum).NumOfLoads += state.dataInternalHeatGains->lightsObjects(LoadPtr).numOfSpaces;
                } else {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->Lights);
                    if (LoadPtr > 0) {
                        ++DemandMgr(MgrNum).NumOfLoads;
                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                               CurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4),
                                               AlphArray(LoadNum + 4)));
                        ErrorsFound = true;
                    }
                }
            }

            //      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                LoadNum = 0;
                for (Item = 1; Item <= NumAlphas - 4; ++Item) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataInternalHeatGains->lightsObjects);
                    if (LoadPtr > 0) {
                        for (Item1 = 1; Item1 <= state.dataInternalHeatGains->lightsObjects(LoadPtr).numOfSpaces; ++Item1) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = state.dataInternalHeatGains->lightsObjects(LoadPtr).spaceStartPtr + Item1 - 1;
                        }
                    } else {
                        LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataHeatBal->Lights);
                        if (LoadPtr > 0) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = LoadPtr;
                        }
                    }
                } // LoadNum
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid value for number of loads.", CurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
                ErrorsFound = true;
            }

        } // MgrNum

        // Get input for DemandManager:ElectricEquipment
        StartIndex = EndIndex + 1;
        EndIndex += NumDemandMgrElecEquip;

        CurrentModuleObject = "DemandManager:ElectricEquipment";

        for (MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     MgrNum - StartIndex + 1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataDemandManager->UniqueDemandMgrNames,
                                                     AlphArray(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            DemandMgr(MgrNum).Name = AlphArray(1);

            DemandMgr(MgrNum).Type = ManagerType::ElecEquip;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           AlphArray(2)));
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            DemandMgr(MgrNum).LimitControl =
                static_cast<ManagerLimit>(getEnumerationValue(ManagerLimitNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(3))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).LimitControl == ManagerLimit::Invalid);

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            DemandMgr(MgrNum).SelectionControl =
                static_cast<ManagerSelection>(getEnumerationValue(ManagerSelectionNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(4))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).SelectionControl == ManagerSelection::Invalid);

            if (NumArray(4) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(4);

            // Count actual pointers to controlled zones
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum) {
                LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataInternalHeatGains->zoneElectricObjects);
                if (LoadPtr > 0) {
                    DemandMgr(MgrNum).NumOfLoads += state.dataInternalHeatGains->zoneElectricObjects(LoadPtr).numOfSpaces;
                } else {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->ZoneElectric);
                    if (LoadPtr > 0) {
                        ++DemandMgr(MgrNum).NumOfLoads;
                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                               CurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4),
                                               AlphArray(LoadNum + 4)));
                        ErrorsFound = true;
                    }
                }
            }

            //      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                LoadNum = 0;
                for (Item = 1; Item <= NumAlphas - 4; ++Item) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataInternalHeatGains->zoneElectricObjects);
                    if (LoadPtr > 0) {
                        for (Item1 = 1; Item1 <= state.dataInternalHeatGains->zoneElectricObjects(LoadPtr).numOfSpaces; ++Item1) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = state.dataInternalHeatGains->zoneElectricObjects(LoadPtr).spaceStartPtr + Item1 - 1;
                        }
                    } else {
                        LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataHeatBal->ZoneElectric);
                        if (LoadPtr > 0) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = LoadPtr;
                        }
                    }
                } // LoadNum
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid value for number of loads.", CurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
                ErrorsFound = true;
            }

        } // MgrNum

        // Get input for DemandManager:Thermostats
        StartIndex = EndIndex + 1;
        EndIndex += NumDemandMgrThermostats;

        CurrentModuleObject = "DemandManager:Thermostats";

        for (MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     MgrNum - StartIndex + 1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataDemandManager->UniqueDemandMgrNames,
                                                     AlphArray(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            DemandMgr(MgrNum).Name = AlphArray(1);

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::Thermostats;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           AlphArray(2)));
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            DemandMgr(MgrNum).LimitControl =
                static_cast<ManagerLimit>(getEnumerationValue(ManagerLimitNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(3))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).LimitControl == ManagerLimit::Invalid);

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);
            DemandMgr(MgrNum).UpperLimit = NumArray(3);

            if (DemandMgr(MgrNum).LowerLimit > DemandMgr(MgrNum).UpperLimit) {
                ShowSevereError(state, format("Invalid input for {} = {}", CurrentModuleObject, AlphArray(1)));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(2),
                                         NumArray(2),
                                         state.dataIPShortCut->cNumericFieldNames(3),
                                         NumArray(3)));
                ShowContinueError(
                    state,
                    format("{} cannot be greater than {}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->cNumericFieldNames(3)));
                ErrorsFound = true;
            }

            // Validate Selection Control
            DemandMgr(MgrNum).SelectionControl =
                static_cast<ManagerSelection>(getEnumerationValue(ManagerSelectionNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(4))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).SelectionControl == ManagerSelection::Invalid);

            if (NumArray(5) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(5);

            // Count actual pointers to controlled zones
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum) {
                LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataZoneCtrls->TStatObjects);
                if (LoadPtr > 0) {
                    DemandMgr(MgrNum).NumOfLoads += state.dataZoneCtrls->TStatObjects(LoadPtr).NumOfZones;
                } else {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataZoneCtrls->TempControlledZone);
                    if (LoadPtr > 0) {
                        ++DemandMgr(MgrNum).NumOfLoads;
                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                               CurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4),
                                               AlphArray(LoadNum + 4)));
                        ErrorsFound = true;
                    }
                }
            }

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                LoadNum = 0;
                for (Item = 1; Item <= NumAlphas - 4; ++Item) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataZoneCtrls->TStatObjects);
                    if (LoadPtr > 0) {
                        for (Item1 = 1; Item1 <= state.dataZoneCtrls->TStatObjects(LoadPtr).NumOfZones; ++Item1) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = state.dataZoneCtrls->TStatObjects(LoadPtr).TempControlledZoneStartPtr + Item1 - 1;
                        }
                    } else {
                        LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataZoneCtrls->TempControlledZone);
                        if (LoadPtr > 0) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = LoadPtr;
                        }
                    }
                } // LoadNum
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid value for number of loads.", CurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
                ErrorsFound = true;
            }
        } // MgrNum

        // Get input for DemandManager:Ventilation
        StartIndex = EndIndex + 1;
        EndIndex += NumDemandMgrVentilation;

        CurrentModuleObject = "DemandManager:Ventilation";

        for (MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     MgrNum - StartIndex + 1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataDemandManager->UniqueDemandMgrNames,
                                                     AlphArray(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            DemandMgr(MgrNum).Name = AlphArray(1);

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::Ventilation;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           AlphArray(2)));
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            DemandMgr(MgrNum).LimitControl =
                static_cast<ManagerLimit>(getEnumerationValue(ManagerLimitVentNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(3))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).LimitControl == ManagerLimit::Invalid);

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            if (DemandMgr(MgrNum).LimitControl == ManagerLimit::Fixed) DemandMgr(MgrNum).FixedRate = NumArray(2);
            if (DemandMgr(MgrNum).LimitControl == ManagerLimit::ReductionRatio) DemandMgr(MgrNum).ReductionRatio = NumArray(3);

            DemandMgr(MgrNum).LowerLimit = NumArray(4);

            // Validate Selection Control
            DemandMgr(MgrNum).SelectionControl =
                static_cast<ManagerSelection>(getEnumerationValue(ManagerSelectionNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(4))));
            ErrorsFound = ErrorsFound || (DemandMgr(MgrNum).SelectionControl == ManagerSelection::Invalid);

            if (NumArray(5) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(5);

            // Count number of string fields for loading Controller:OutdoorAir names. This number must be increased in case if
            // new string field is added or decreased if string fields are removed.
            int AlphaShift = 4;

            // Count actual pointers to air controllers
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - AlphaShift; ++LoadNum) {
                LoadPtr = GetOAController(state, AlphArray(LoadNum + AlphaShift));
                if (LoadPtr > 0) {
                    ++DemandMgr(MgrNum).NumOfLoads;
                } else {
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" not found.",
                                           CurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(LoadNum + AlphaShift),
                                           AlphArray(LoadNum + AlphaShift)));
                    ErrorsFound = true;
                }
            }

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                for (LoadNum = 1; LoadNum <= NumAlphas - AlphaShift; ++LoadNum) {
                    LoadPtr = GetOAController(state, AlphArray(LoadNum + AlphaShift));
                    if (LoadPtr > 0) {
                        DemandMgr(MgrNum).Load(LoadNum) = LoadPtr;
                    }
                }
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid value for number of loads.", CurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
                ErrorsFound = true;
            }
        } // MgrNum

        AlphArray.deallocate();
        NumArray.deallocate();
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in processing input for demand managers. Preceding condition causes termination.");
    }
}

void SurveyDemandManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Checks to see if any demand managers can reduce the load

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MgrNum;
    int LoadNum;
    int LoadPtr;
    bool CanReduceDemand;

    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    for (MgrNum = 1; MgrNum <= state.dataDemandManager->NumDemandMgr; ++MgrNum) {

        DemandMgr(MgrNum).CanReduceDemand = false;

        if (!DemandMgr(MgrNum).Available) continue;
        if (DemandMgr(MgrNum).LimitControl == ManagerLimit::Off) continue;

        if (DemandMgr(MgrNum).Active) continue; // This works for FIXED control action, but not VARIABLE
        // VARIABLE control could actually reduce demand farther, even if active already

        for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
            LoadPtr = DemandMgr(MgrNum).Load(LoadNum);

            // Check if this load can reduce demand
            // Assume FIXED control action for now, needs more sophisticated check for VARIABLE control
            LoadInterface(state, DemandAction::CheckCanReduce, MgrNum, LoadPtr, CanReduceDemand);

            if (CanReduceDemand) {
                DemandMgr(MgrNum).CanReduceDemand = true;
                break; // If any one load can reduce demand, then the whole demand manager can reduce demand
            }

        } // LoadNum

    } // MgrNum
}

void ActivateDemandManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:

    // METHODOLOGY EMPLOYED:

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MgrNum;
    int LoadNum;
    int LoadPtr;
    int RotatedLoadNum;
    bool CanReduceDemand;

    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    for (MgrNum = 1; MgrNum <= state.dataDemandManager->NumDemandMgr; ++MgrNum) {

        if (DemandMgr(MgrNum).Activate) {
            DemandMgr(MgrNum).Activate = false;
            DemandMgr(MgrNum).Active = true;

            switch (DemandMgr(MgrNum).SelectionControl) {
            case ManagerSelection::All: {
                // Turn ON limiting on all loads
                for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                    LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                    LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                } // LoadNum

            } break;
            case ManagerSelection::Many: { // All loads are limited except for one
                if (DemandMgr(MgrNum).NumOfLoads > 1) {

                    // Turn ON limiting on all loads
                    for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                        LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                        LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                    } // LoadNum

                    // Set next rotated load (from last time it was active)
                    RotatedLoadNum = DemandMgr(MgrNum).RotatedLoadNum;
                    ++RotatedLoadNum;
                    if (RotatedLoadNum > DemandMgr(MgrNum).NumOfLoads) RotatedLoadNum = 1;
                    DemandMgr(MgrNum).RotatedLoadNum = RotatedLoadNum;

                    // Turn OFF limiting for the new rotated load
                    LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                    LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);
                } else {
                    // Turn ON limiting for the one and only load
                    LoadPtr = DemandMgr(MgrNum).Load(1);
                    LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                }

            } break;
            case ManagerSelection::One: { // Only one load is limited
                if (DemandMgr(MgrNum).NumOfLoads > 1) {
                    // Turn OFF limiting on all loads
                    for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                        LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                        LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);
                    } // LoadNum

                    // Set next rotated load (from last time it was active)
                    RotatedLoadNum = DemandMgr(MgrNum).RotatedLoadNum;
                    ++RotatedLoadNum;
                    if (RotatedLoadNum > DemandMgr(MgrNum).NumOfLoads) RotatedLoadNum = 1;
                    DemandMgr(MgrNum).RotatedLoadNum = RotatedLoadNum;

                    // Turn ON limiting for the new rotated load
                    LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                    LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                } else {
                    // Turn ON limiting for the one and only load
                    LoadPtr = DemandMgr(MgrNum).Load(1);
                    LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                }
            } break;
            default:
                break;
            }
        }

    } // MgrNum
}

void UpdateDemandManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Expires limits and rotates loads after specified time duration.
    // It updates availability flags, expires managers that ended in the last timestep, etc.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MgrNum;
    int LoadNum;
    int LoadPtr;
    bool Available;
    bool CanReduceDemand;
    int RotatedLoadNum;

    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    for (MgrNum = 1; MgrNum <= state.dataDemandManager->NumDemandMgr; ++MgrNum) {

        // Check availability
        //    IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
        //      Available = .TRUE.  ! No schedule defaults to available
        //    ELSE
        if (GetCurrentScheduleValue(state, DemandMgr(MgrNum).AvailSchedule) > 0.0) {
            Available = true;
        } else {
            Available = false;
        }
        //    END IF

        DemandMgr(MgrNum).Available = Available;

        // Update demand manager status
        if (Available) {

            if (DemandMgr(MgrNum).Active) {

                DemandMgr(MgrNum).ElapsedTime += state.dataGlobal->MinutesPerTimeStep;

                // Check for expiring limit duration
                if (DemandMgr(MgrNum).ElapsedTime >= DemandMgr(MgrNum).LimitDuration) {
                    DemandMgr(MgrNum).ElapsedTime = 0;
                    DemandMgr(MgrNum).ElapsedRotationTime = 0;
                    DemandMgr(MgrNum).Active = false;

                    // Demand Manager is not available, remove demand limits from all loads
                    for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                        LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                        LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);
                    } // LoadNum

                } else {

                    switch (DemandMgr(MgrNum).SelectionControl) {
                    case ManagerSelection::All: {
                        // Do nothing; limits remain on all loads

                    } break;
                    case ManagerSelection::Many: { // All loads are limited except for one
                        DemandMgr(MgrNum).ElapsedRotationTime += state.dataGlobal->MinutesPerTimeStep;

                        if (DemandMgr(MgrNum).ElapsedRotationTime >= DemandMgr(MgrNum).RotationDuration) {
                            DemandMgr(MgrNum).ElapsedRotationTime = 0;

                            if (DemandMgr(MgrNum).NumOfLoads > 1) {
                                // Turn ON limiting for the old rotated load
                                RotatedLoadNum = DemandMgr(MgrNum).RotatedLoadNum;
                                LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                                LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);

                                // Set next rotated load
                                ++RotatedLoadNum;
                                if (RotatedLoadNum > DemandMgr(MgrNum).NumOfLoads) RotatedLoadNum = 1;
                                DemandMgr(MgrNum).RotatedLoadNum = RotatedLoadNum;

                                // Turn OFF limiting for the new rotated load
                                LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                                LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);
                            }
                        }

                    } break;
                    case ManagerSelection::One: { // Only one load is limited
                        DemandMgr(MgrNum).ElapsedRotationTime += state.dataGlobal->MinutesPerTimeStep;

                        if (DemandMgr(MgrNum).ElapsedRotationTime >= DemandMgr(MgrNum).RotationDuration) {
                            DemandMgr(MgrNum).ElapsedRotationTime = 0;

                            if (DemandMgr(MgrNum).NumOfLoads > 1) {
                                // Turn OFF limiting for the old rotated load
                                RotatedLoadNum = DemandMgr(MgrNum).RotatedLoadNum;
                                LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                                LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);

                                // Set next rotated load
                                ++RotatedLoadNum;
                                if (RotatedLoadNum > DemandMgr(MgrNum).NumOfLoads) RotatedLoadNum = 1;
                                DemandMgr(MgrNum).RotatedLoadNum = RotatedLoadNum;

                                // Turn ON limiting for the new rotated load
                                LoadPtr = DemandMgr(MgrNum).Load(RotatedLoadNum);
                                LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                            }
                        }
                    } break;
                    default:
                        break;
                    }
                }
            }

        } else { // Demand Manager is not available
            DemandMgr(MgrNum).Active = false;

            // Demand Manager is not available, remove demand limits from all loads
            for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                LoadInterface(state, DemandAction::ClearLimit, MgrNum, LoadPtr, CanReduceDemand);
            } // LoadNum
        }

    } // MgrNum
}

void ReportDemandManagerList(EnergyPlusData &state, int const ListNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates report variables.

    // METHODOLOGY EMPLOYED:
    // Standard EnergyPlus methodology.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 BillingPeriod;
    int Item;
    int AveragingWindow;
    bool OnPeak;
    Real64 OverLimit;

    auto &DemandManagerList(state.dataDemandManager->DemandManagerList);

    if (DemandManagerList(ListNum).BillingSchedule == 0) {
        BillingPeriod = state.dataEnvrn->Month;
    } else {
        BillingPeriod = GetCurrentScheduleValue(state, DemandManagerList(ListNum).BillingSchedule);
    }

    if (DemandManagerList(ListNum).BillingPeriod != BillingPeriod) {
        // Reset variables for new billing period
        // DemandManagerList(ListNum)%History = 0.0        ! Don't reset--continue from previous billing period
        // DemandManagerList(ListNum)%AverageDemand = 0.0  ! Don't reset--continue from previous billing period
        DemandManagerList(ListNum).PeakDemand = 0.0;
        DemandManagerList(ListNum).OverLimitDuration = 0.0;

        DemandManagerList(ListNum).BillingPeriod = BillingPeriod;
    }

    // Add new timestep to demand history and subtract oldest timestep
    AveragingWindow = DemandManagerList(ListNum).AveragingWindow;
    DemandManagerList(ListNum).AverageDemand += (DemandManagerList(ListNum).MeterDemand - DemandManagerList(ListNum).History(1)) / AveragingWindow;

    // Update demand history
    for (Item = 1; Item <= AveragingWindow - 1; ++Item) {
        DemandManagerList(ListNum).History(Item) = DemandManagerList(ListNum).History(Item + 1);
    }
    DemandManagerList(ListNum).History(AveragingWindow) = DemandManagerList(ListNum).MeterDemand;

    if (DemandManagerList(ListNum).PeakSchedule == 0) {
        OnPeak = true;
    } else {
        if (GetCurrentScheduleValue(state, DemandManagerList(ListNum).PeakSchedule) == 1) {
            OnPeak = true;
        } else {
            OnPeak = false;
        }
    }

    if (OnPeak) {
        DemandManagerList(ListNum).PeakDemand = max(DemandManagerList(ListNum).AverageDemand, DemandManagerList(ListNum).PeakDemand);

        OverLimit = DemandManagerList(ListNum).AverageDemand - DemandManagerList(ListNum).ScheduledLimit;
        if (OverLimit > 0.0) {
            DemandManagerList(ListNum).OverLimit = OverLimit;
            DemandManagerList(ListNum).OverLimitDuration += (state.dataGlobal->MinutesPerTimeStep / 60.0);
        } else {
            DemandManagerList(ListNum).OverLimit = 0.0;
        }

    } else {
        DemandManagerList(ListNum).OverLimit = 0.0;
    }
}

void LoadInterface(EnergyPlusData &state, DemandAction const Action, int const MgrNum, int const LoadPtr, bool &CanReduceDemand)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides a universal interface to handle all communication with the various load objects.
    // Demand managers for new types of loads can be easily added with a new CASE statement in this subroutine
    // and new GetInput code.

    // Using/Aliasing
    using MixedAir::OAGetFlowRate;
    using MixedAir::OAGetMinFlowRate;
    using MixedAir::OASetDemandManagerVentilationFlow;
    using MixedAir::OASetDemandManagerVentilationState;

    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 LowestPower;

    CanReduceDemand = false;

    switch (DemandMgr(MgrNum).Type) {
    case ManagerType::ExtLights: {
        LowestPower = state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
        if (Action == DemandAction::CheckCanReduce) {
            if (state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).Power > LowestPower) CanReduceDemand = true;
        } else if (Action == DemandAction::SetLimit) {
            state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).ManageDemand = true;
            state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).DemandLimit = LowestPower;
        } else if (Action == DemandAction::ClearLimit) {
            state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).ManageDemand = false;
        }
    } break;
    case ManagerType::Lights: {
        LowestPower = state.dataHeatBal->Lights(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
        if (Action == DemandAction::CheckCanReduce) {
            if (state.dataHeatBal->Lights(LoadPtr).Power > LowestPower) CanReduceDemand = true;
        } else if (Action == DemandAction::SetLimit) {
            state.dataHeatBal->Lights(LoadPtr).ManageDemand = true;
            state.dataHeatBal->Lights(LoadPtr).DemandLimit = LowestPower;
        } else if (Action == DemandAction::ClearLimit) {
            state.dataHeatBal->Lights(LoadPtr).ManageDemand = false;
        }

    } break;
    case ManagerType::ElecEquip: {
        LowestPower = state.dataHeatBal->ZoneElectric(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
        if (Action == DemandAction::CheckCanReduce) {
            if (state.dataHeatBal->ZoneElectric(LoadPtr).Power > LowestPower) CanReduceDemand = true;
        } else if (Action == DemandAction::SetLimit) {
            state.dataHeatBal->ZoneElectric(LoadPtr).ManageDemand = true;
            state.dataHeatBal->ZoneElectric(LoadPtr).DemandLimit = LowestPower;
        } else if (Action == DemandAction::ClearLimit) {
            state.dataHeatBal->ZoneElectric(LoadPtr).ManageDemand = false;
        }
    } break;
    case ManagerType::Thermostats: {
        if (Action == DemandAction::CheckCanReduce) {
            if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(state.dataZoneCtrls->TempControlledZone(LoadPtr).ActualZoneNum) >
                    DemandMgr(MgrNum).LowerLimit ||
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(state.dataZoneCtrls->TempControlledZone(LoadPtr).ActualZoneNum) <
                    DemandMgr(MgrNum).UpperLimit)
                CanReduceDemand = true; // Heating | Cooling
        } else if (Action == DemandAction::SetLimit) {
            state.dataZoneCtrls->TempControlledZone(LoadPtr).ManageDemand = true;
            state.dataZoneCtrls->TempControlledZone(LoadPtr).HeatingResetLimit = DemandMgr(MgrNum).LowerLimit;
            state.dataZoneCtrls->TempControlledZone(LoadPtr).CoolingResetLimit = DemandMgr(MgrNum).UpperLimit;
        } else if (Action == DemandAction::ClearLimit) {
            state.dataZoneCtrls->TempControlledZone(LoadPtr).ManageDemand = false;
        }
        if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
            if (state.dataHeatBalFanSys->ComfortControlType(state.dataZoneCtrls->TempControlledZone(LoadPtr).ActualZoneNum) !=
                DataHVACGlobals::ThermostatType::Uncontrolled) {
                if (Action == DemandAction::CheckCanReduce) {
                    if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(state.dataZoneCtrls->ComfortControlledZone(LoadPtr).ActualZoneNum) >
                            DemandMgr(MgrNum).LowerLimit ||
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(state.dataZoneCtrls->ComfortControlledZone(LoadPtr).ActualZoneNum) <
                            DemandMgr(MgrNum).UpperLimit)
                        CanReduceDemand = true; // Heating
                } else if (Action == DemandAction::SetLimit) {
                    state.dataZoneCtrls->ComfortControlledZone(LoadPtr).ManageDemand = true;
                    state.dataZoneCtrls->ComfortControlledZone(LoadPtr).HeatingResetLimit = DemandMgr(MgrNum).LowerLimit;
                    state.dataZoneCtrls->ComfortControlledZone(LoadPtr).CoolingResetLimit = DemandMgr(MgrNum).UpperLimit;
                } else if (Action == DemandAction::ClearLimit) {
                    state.dataZoneCtrls->ComfortControlledZone(LoadPtr).ManageDemand = false;
                }
            }
        }
    } break;
    case ManagerType::Ventilation: {
        Real64 FlowRate(0);
        FlowRate = OAGetFlowRate(state, LoadPtr);
        if (Action == DemandAction::CheckCanReduce) {
            CanReduceDemand = true;
        } else if (Action == DemandAction::SetLimit) {
            OASetDemandManagerVentilationState(state, LoadPtr, true);
            if (DemandMgr(MgrNum).LimitControl == ManagerLimit::Fixed) {
                OASetDemandManagerVentilationFlow(state, LoadPtr, DemandMgr(MgrNum).FixedRate);
            } else if (DemandMgr(MgrNum).LimitControl == ManagerLimit::ReductionRatio) {
                Real64 DemandRate(0);
                DemandRate = FlowRate * DemandMgr(MgrNum).ReductionRatio;
                OASetDemandManagerVentilationFlow(state, LoadPtr, DemandRate);
            }
        } else if (Action == DemandAction::ClearLimit) {
            OASetDemandManagerVentilationState(state, LoadPtr, false);
        }
    } break;
    default:
        break;
    }
}

void InitDemandManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provide external call to get Demand manager input after
    // appropriate initializations.

    if (state.dataDemandManager->GetInput) {
        GetDemandManagerInput(state);
        GetDemandManagerListInput(state);
        state.dataDemandManager->GetInput = false;
    }
}

} // namespace EnergyPlus::DemandManager
