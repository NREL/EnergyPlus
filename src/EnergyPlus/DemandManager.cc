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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
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
        GetInstantMeterValue(state, DemandManagerList(ListNum).Meter, OutputProcessor::TimeStepType::TimeStepZone) /
            state.dataGlobal->TimeStepZoneSec +
        GetInstantMeterValue(state, DemandManagerList(ListNum).Meter, OutputProcessor::TimeStepType::TimeStepSystem) /
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

            {
                auto const SELECT_CASE_var(DemandManagerList(ListNum).ManagerPriority);

                if (SELECT_CASE_var == ManagePriorityType::ManagerPrioritySequential) { // Activate first Demand Manager that can reduce demand

                    for (MgrNum = 1; MgrNum <= DemandManagerList(ListNum).NumOfManager; ++MgrNum) {
                        MgrPtr = DemandManagerList(ListNum).Manager(MgrNum);

                        if (DemandMgr(MgrPtr).CanReduceDemand) {
                            DemandMgr(MgrPtr).Activate = true;

                            {
                                auto const SELECT_CASE_var1(DemandMgr(MgrPtr).Type);
                                if (SELECT_CASE_var1 == ManagerType::ManagerTypeExtLights) {
                                    ResimExt = true;

                                } else if ((SELECT_CASE_var1 == ManagerType::ManagerTypeLights) ||
                                           (SELECT_CASE_var1 == ManagerType::ManagerTypeElecEquip)) {
                                    ResimHB = true;
                                    ResimHVAC = true;

                                } else if ((SELECT_CASE_var1 == ManagerType::ManagerTypeThermostats) ||
                                           (SELECT_CASE_var1 == ManagerType::ManagerTypeVentilation)) {
                                    ResimHVAC = true;
                                }
                            }

                            break; // Leave the loop
                        }
                    } // MgrNum

                } else if (SELECT_CASE_var == ManagePriorityType::ManagerPriorityOptimal) {
                    // Not yet implemented

                } else if (SELECT_CASE_var == ManagePriorityType::ManagerPriorityAll) { // Activate ALL Demand Managers that can reduce demand

                    for (MgrNum = 1; MgrNum <= DemandManagerList(ListNum).NumOfManager; ++MgrNum) {
                        MgrPtr = DemandManagerList(ListNum).Manager(MgrNum);

                        if (DemandMgr(MgrPtr).CanReduceDemand) {
                            DemandMgr(MgrPtr).Activate = true;

                            {
                                auto const SELECT_CASE_var1(DemandMgr(MgrPtr).Type);
                                if (SELECT_CASE_var1 == ManagerType::ManagerTypeExtLights) {
                                    ResimExt = true;

                                } else if ((SELECT_CASE_var1 == ManagerType::ManagerTypeLights) ||
                                           (SELECT_CASE_var1 == ManagerType::ManagerTypeElecEquip)) {
                                    ResimHB = true;
                                    ResimHVAC = true;

                                } else if ((SELECT_CASE_var1 == ManagerType::ManagerTypeThermostats) ||
                                           (SELECT_CASE_var1 == ManagerType::ManagerTypeVentilation)) {
                                    ResimHVAC = true;
                                }
                            }
                        }
                    } // MgrNum
                }
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
    int ListNum;
    int MgrNum;
    int NumAlphas;            // Number of elements in the alpha array
    int NumNums;              // Number of elements in the numeric array
    int IOStat;               // IO Status when calling get input subroutine
    Array1D_string AlphArray; // Character string data
    Array1D<Real64> NumArray; // Numeric data
    std::string Units;        // String for meter units
    bool ErrorsFound(false);
    std::string CurrentModuleObject; // for ease in renaming.

    CurrentModuleObject = "DemandManagerAssignmentList";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, ListNum, NumAlphas, NumNums);

    auto &DemandManagerList(state.dataDemandManager->DemandManagerList);
    auto &DemandMgr(state.dataDemandManager->DemandMgr);

    state.dataDemandManager->NumDemandManagerList = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

    if (state.dataDemandManager->NumDemandManagerList > 0) {
        AlphArray.dimension(NumAlphas, std::string());
        NumArray.dimension(NumNums, 0.0);

        DemandManagerList.allocate(state.dataDemandManager->NumDemandManagerList);

        for (ListNum = 1; ListNum <= state.dataDemandManager->NumDemandManagerList; ++ListNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     ListNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

            DemandManagerList(ListNum).Name = AlphArray(1);

            DemandManagerList(ListNum).Meter = GetMeterIndex(state, AlphArray(2));

            if (DemandManagerList(ListNum).Meter == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + AlphArray(2));
                ShowContinueError(state, "Entered in " + CurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;

            } else {
                {
                    auto const SELECT_CASE_var(state.dataOutputProcessor->EnergyMeters(DemandManagerList(ListNum).Meter).ResourceType);
                    if ((SELECT_CASE_var == "Electricity") || (SELECT_CASE_var == "ElectricityNet")) {
                        Units = "[W]"; // For setup of report variables

                    } else {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value " +
                                            state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\".");
                        ShowContinueError(state, "Only Electricity and ElectricityNet meters are currently allowed.");
                        ErrorsFound = true;
                    }
                }
            }

            // Further checking for conflicting DEMAND MANAGER LISTs

            if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                DemandManagerList(ListNum).LimitSchedule = GetScheduleIndex(state, AlphArray(3));

                if (DemandManagerList(ListNum).LimitSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            DemandManagerList(ListNum).SafetyFraction = NumArray(1);

            if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                DemandManagerList(ListNum).BillingSchedule = GetScheduleIndex(state, AlphArray(4));

                if (DemandManagerList(ListNum).BillingSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                DemandManagerList(ListNum).PeakSchedule = GetScheduleIndex(state, AlphArray(5));

                if (DemandManagerList(ListNum).PeakSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + AlphArray(5) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            DemandManagerList(ListNum).AveragingWindow = max(int(NumArray(2) / state.dataGlobal->MinutesPerTimeStep), 1);
            // Round to nearest timestep
            // Can make this fancier to include windows that do not fit the timesteps
            DemandManagerList(ListNum).History.allocate(DemandManagerList(ListNum).AveragingWindow);
            DemandManagerList(ListNum).History = 0.0;

            // Validate Demand Manager Priority
            {
                auto const SELECT_CASE_var(AlphArray(6));
                if (SELECT_CASE_var == "SEQUENTIAL") {
                    DemandManagerList(ListNum).ManagerPriority = ManagePriorityType::ManagerPrioritySequential;

                } else if (SELECT_CASE_var == "OPTIMAL") {
                    DemandManagerList(ListNum).ManagerPriority = ManagePriorityType::ManagerPriorityOptimal;

                } else if (SELECT_CASE_var == "ALL") {
                    DemandManagerList(ListNum).ManagerPriority = ManagePriorityType::ManagerPriorityAll;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value " +
                                        state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + AlphArray(6) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Get DEMAND MANAGER Type and Name pairs
            DemandManagerList(ListNum).NumOfManager = int((NumAlphas - 6) / 2.0);

            if (DemandManagerList(ListNum).NumOfManager > 0) {
                DemandManagerList(ListNum).Manager.allocate(DemandManagerList(ListNum).NumOfManager);

                for (MgrNum = 1; MgrNum <= DemandManagerList(ListNum).NumOfManager; ++MgrNum) {

                    // Validate DEMAND MANAGER Type
                    {
                        auto const SELECT_CASE_var(AlphArray(MgrNum * 2 + 5));
                        if ((SELECT_CASE_var == "DEMANDMANAGER:LIGHTS") || (SELECT_CASE_var == "DEMANDMANAGER:EXTERIORLIGHTS") ||
                            (SELECT_CASE_var == "DEMANDMANAGER:ELECTRICEQUIPMENT") || (SELECT_CASE_var == "DEMANDMANAGER:THERMOSTATS") ||
                            (SELECT_CASE_var == "DEMANDMANAGER:VENTILATION")) {

                            DemandManagerList(ListNum).Manager(MgrNum) = UtilityRoutines::FindItemInList(AlphArray(MgrNum * 2 + 6), DemandMgr);

                            if (DemandManagerList(ListNum).Manager(MgrNum) == 0) {
                                ShowSevereError(state,
                                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(MgrNum * 2 + 6) + "=\"" + AlphArray(MgrNum * 2 + 6) +
                                                    "\" not found.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(state,
                                            CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value " +
                                                state.dataIPShortCut->cAlphaFieldNames(MgrNum * 2 + 5) + "=\"" + AlphArray(MgrNum * 2 + 5) + "\".");
                            ErrorsFound = true;
                        }
                    }

                    // Check that each is not already referenced using %DemandManagerList field

                } // MgrNum
            }

            // Setup report variables
            SetupOutputVariable(state,
                                "Demand Manager Meter Demand Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).MeterDemand,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Average Demand Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).AverageDemand,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Peak Demand Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).PeakDemand,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Scheduled Limit Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).ScheduledLimit,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Demand Limit Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).DemandLimit,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Over Limit Power",
                                OutputProcessor::Unit::W,
                                DemandManagerList(ListNum).OverLimit,
                                "Zone",
                                "Average",
                                DemandManagerList(ListNum).Name);

            SetupOutputVariable(state,
                                "Demand Manager Over Limit Time",
                                OutputProcessor::Unit::hr,
                                DemandManagerList(ListNum).OverLimitDuration,
                                "Zone",
                                "Sum",
                                DemandManagerList(ListNum).Name);

            if (ErrorsFound) {
                ShowFatalError(state, "Errors found in processing input for " + CurrentModuleObject);
            }

        } // ListNum

        AlphArray.deallocate();
        NumArray.deallocate();

        // Iteration diagnostic reporting for all DEMAND MANAGER LISTs
        SetupOutputVariable(state,
                            "Demand Manager Exterior Energy Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerExtIterations,
                            "Zone",
                            "Sum",
                            "ManageDemand");

        SetupOutputVariable(state,
                            "Demand Manager Heat Balance Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerHBIterations,
                            "Zone",
                            "Sum",
                            "ManageDemand");

        SetupOutputVariable(state,
                            "Demand Manager HVAC Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataDemandManager->DemandManagerHVACIterations,
                            "Zone",
                            "Sum",
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

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::ManagerTypeExtLights;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            {
                auto const SELECT_CASE_var(AlphArray(3));
                if (SELECT_CASE_var == "OFF") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitOff;

                } else if (SELECT_CASE_var == "FIXED") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitFixed;

                } else if (SELECT_CASE_var == "VARIABLE") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitVariable;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\".");
                    ShowContinueError(state, "...value must be one of Off, Fixed, or Variable.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            {
                auto const SELECT_CASE_var(AlphArray(4));
                if (SELECT_CASE_var == "ALL") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionAll;

                } else if (SELECT_CASE_var == "ROTATEONE") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionOne;

                } else if (SELECT_CASE_var == "ROTATEMANY") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionMany;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\".");
                    ShowContinueError(state, "...value must be one of All, RotateOne, or RotateMany.");
                    ErrorsFound = true;
                }
            }

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
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4) + "=\"" + AlphArray(LoadNum + 4) + "\" not found.");
                        ErrorsFound = true;
                    }
                } // LoadNum
            } else {
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value for number of loads.");
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

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::ManagerTypeLights;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            {
                auto const SELECT_CASE_var(AlphArray(3));
                if (SELECT_CASE_var == "OFF") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitOff;

                } else if (SELECT_CASE_var == "FIXED") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitFixed;

                } else if (SELECT_CASE_var == "VARIABLE") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitVariable;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\".");
                    ShowContinueError(state, "...value must be one of Off, Fixed, or Variable.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            {
                auto const SELECT_CASE_var(AlphArray(4));
                if (SELECT_CASE_var == "ALL") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionAll;

                } else if (SELECT_CASE_var == "ROTATEONE") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionOne;

                } else if (SELECT_CASE_var == "ROTATEMANY") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionMany;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\".");
                    ShowContinueError(state, "...value must be one of All, RotateOne, or RotateMany.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(4) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(4);

            // Count actual pointers to controlled zones
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum) {
                LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->LightsObjects);
                if (LoadPtr > 0) {
                    DemandMgr(MgrNum).NumOfLoads += state.dataHeatBal->LightsObjects(LoadPtr).NumOfZones;
                } else {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->Lights);
                    if (LoadPtr > 0) {
                        ++DemandMgr(MgrNum).NumOfLoads;
                    } else {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4) + "=\"" + AlphArray(LoadNum + 4) + "\" not found.");
                        ErrorsFound = true;
                    }
                }
            }

            //      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                LoadNum = 0;
                for (Item = 1; Item <= NumAlphas - 4; ++Item) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataHeatBal->LightsObjects);
                    if (LoadPtr > 0) {
                        for (Item1 = 1; Item1 <= state.dataHeatBal->LightsObjects(LoadPtr).NumOfZones; ++Item1) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = state.dataHeatBal->LightsObjects(LoadPtr).StartPtr + Item1 - 1;
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
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value for number of loads.");
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

            DemandMgr(MgrNum).Type = ManagerType::ManagerTypeElecEquip;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            {
                auto const SELECT_CASE_var(AlphArray(3));
                if (SELECT_CASE_var == "OFF") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitOff;

                } else if (SELECT_CASE_var == "FIXED") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitFixed;

                } else if (SELECT_CASE_var == "VARIABLE") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitVariable;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\".");
                    ShowContinueError(state, "...value must be one of Off, Fixed, or Variable.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);

            // Validate Selection Control
            {
                auto const SELECT_CASE_var(AlphArray(4));
                if (SELECT_CASE_var == "ALL") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionAll;

                } else if (SELECT_CASE_var == "ROTATEONE") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionOne;

                } else if (SELECT_CASE_var == "ROTATEMANY") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionMany;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\".");
                    ShowContinueError(state, "...value must be one of All, RotateOne, or RotateMany.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(4) == 0.0)
                DemandMgr(MgrNum).RotationDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).RotationDuration = NumArray(4);

            // Count actual pointers to controlled zones
            DemandMgr(MgrNum).NumOfLoads = 0;
            for (LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum) {
                LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->ZoneElectricObjects);
                if (LoadPtr > 0) {
                    DemandMgr(MgrNum).NumOfLoads += state.dataHeatBal->ZoneElectricObjects(LoadPtr).NumOfZones;
                } else {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(LoadNum + 4), state.dataHeatBal->ZoneElectric);
                    if (LoadPtr > 0) {
                        ++DemandMgr(MgrNum).NumOfLoads;
                    } else {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4) + "=\"" + AlphArray(LoadNum + 4) + "\" not found.");
                        ErrorsFound = true;
                    }
                }
            }

            //      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

            if (DemandMgr(MgrNum).NumOfLoads > 0) {
                DemandMgr(MgrNum).Load.allocate(DemandMgr(MgrNum).NumOfLoads);
                LoadNum = 0;
                for (Item = 1; Item <= NumAlphas - 4; ++Item) {
                    LoadPtr = UtilityRoutines::FindItemInList(AlphArray(Item + 4), state.dataHeatBal->ZoneElectricObjects);
                    if (LoadPtr > 0) {
                        for (Item1 = 1; Item1 <= state.dataHeatBal->ZoneElectricObjects(LoadPtr).NumOfZones; ++Item1) {
                            ++LoadNum;
                            DemandMgr(MgrNum).Load(LoadNum) = state.dataHeatBal->ZoneElectricObjects(LoadPtr).StartPtr + Item1 - 1;
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
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value for number of loads.");
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

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::ManagerTypeThermostats;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            {
                auto const SELECT_CASE_var(AlphArray(3));
                if (SELECT_CASE_var == "OFF") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitOff;

                } else if (SELECT_CASE_var == "FIXED") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitFixed;

                } else if (SELECT_CASE_var == "VARIABLE") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitVariable;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\".");
                    ShowContinueError(state, "...value must be one of Off, Fixed, or Variable.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            DemandMgr(MgrNum).LowerLimit = NumArray(2);
            DemandMgr(MgrNum).UpperLimit = NumArray(3);

            if (DemandMgr(MgrNum).LowerLimit > DemandMgr(MgrNum).UpperLimit) {
                ShowSevereError(state, "Invalid input for " + CurrentModuleObject + " = " + AlphArray(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(2),
                                         NumArray(2),
                                         state.dataIPShortCut->cNumericFieldNames(3),
                                         NumArray(3)));
                ShowContinueError(
                    state, state.dataIPShortCut->cNumericFieldNames(2) + " cannot be greater than " + state.dataIPShortCut->cNumericFieldNames(3));
                ErrorsFound = true;
            }

            // Validate Selection Control
            {
                auto const SELECT_CASE_var(AlphArray(4));
                if (SELECT_CASE_var == "ALL") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionAll;

                } else if (SELECT_CASE_var == "ROTATEONE") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionOne;

                } else if (SELECT_CASE_var == "ROTATEMANY") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionMany;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\".");
                    ShowContinueError(state, "...value must be one of All, RotateOne, or RotateMany.");
                    ErrorsFound = true;
                }
            }

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
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(LoadNum + 4) + "=\"" + AlphArray(LoadNum + 4) + "\" not found.");
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
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value for number of loads.");
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

            DemandMgr(MgrNum).Type = DemandMgr(MgrNum).Type = ManagerType::ManagerTypeVentilation;

            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                DemandMgr(MgrNum).AvailSchedule = GetScheduleIndex(state, AlphArray(2));

                if (DemandMgr(MgrNum).AvailSchedule == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                DemandMgr(MgrNum).AvailSchedule = DataGlobalConstants::ScheduleAlwaysOn;
            }

            // Validate Limiting Control
            {
                auto const SELECT_CASE_var(AlphArray(3));
                if (SELECT_CASE_var == "OFF") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitOff;

                } else if (SELECT_CASE_var == "FIXEDRATE") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitFixed;

                } else if (SELECT_CASE_var == "REDUCTIONRATIO") {
                    DemandMgr(MgrNum).LimitControl = Limit::ManagerLimitReductionRatio;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphArray(3) + "\".");
                    ShowContinueError(state, "...value must be one of Off, FixedRate, or ReductionRatio.");
                    ErrorsFound = true;
                }
            }

            if (NumArray(1) == 0.0)
                DemandMgr(MgrNum).LimitDuration = state.dataGlobal->MinutesPerTimeStep;
            else
                DemandMgr(MgrNum).LimitDuration = NumArray(1);

            if (DemandMgr(MgrNum).LimitControl == Limit::ManagerLimitFixed) DemandMgr(MgrNum).FixedRate = NumArray(2);
            if (DemandMgr(MgrNum).LimitControl == Limit::ManagerLimitReductionRatio) DemandMgr(MgrNum).ReductionRatio = NumArray(3);

            DemandMgr(MgrNum).LowerLimit = NumArray(4);

            // Validate Selection Control
            {
                auto const SELECT_CASE_var(AlphArray(4));
                if (SELECT_CASE_var == "ALL") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionAll;

                } else if (SELECT_CASE_var == "ROTATEONE") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionOne;

                } else if (SELECT_CASE_var == "ROTATEMANY") {
                    DemandMgr(MgrNum).SelectionControl = Selection::ManagerSelectionMany;

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value" +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + AlphArray(4) + "\".");
                    ShowContinueError(state, "...value must be one of All, RotateOne, or RotateMany.");
                    ErrorsFound = true;
                }
            }

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
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(LoadNum + AlphaShift) + "=\"" + AlphArray(LoadNum + AlphaShift) +
                                        "\" not found.");
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
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid value for number of loads.");
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
        if (DemandMgr(MgrNum).LimitControl == Limit::ManagerLimitOff) continue;

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

            {
                auto const SELECT_CASE_var(DemandMgr(MgrNum).SelectionControl);

                if (SELECT_CASE_var == Selection::ManagerSelectionAll) {
                    // Turn ON limiting on all loads
                    for (LoadNum = 1; LoadNum <= DemandMgr(MgrNum).NumOfLoads; ++LoadNum) {
                        LoadPtr = DemandMgr(MgrNum).Load(LoadNum);
                        LoadInterface(state, DemandAction::SetLimit, MgrNum, LoadPtr, CanReduceDemand);
                    } // LoadNum

                } else if (SELECT_CASE_var == Selection::ManagerSelectionMany) { // All loads are limited except for one
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

                } else if (SELECT_CASE_var == Selection::ManagerSelectionOne) { // Only one load is limited
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
                }
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

                    {
                        auto const SELECT_CASE_var(DemandMgr(MgrNum).SelectionControl);
                        if (SELECT_CASE_var == Selection::ManagerSelectionAll) {
                            // Do nothing; limits remain on all loads

                        } else if (SELECT_CASE_var == Selection::ManagerSelectionMany) { // All loads are limited except for one
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

                        } else if (SELECT_CASE_var == Selection::ManagerSelectionOne) { // Only one load is limited
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
                        }
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

    {
        auto const SELECT_CASE_var(DemandMgr(MgrNum).Type);

        if (SELECT_CASE_var == ManagerType::ManagerTypeExtLights) {
            LowestPower = state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
            if (Action == DemandAction::CheckCanReduce) {
                if (state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).Power > LowestPower) CanReduceDemand = true;
            } else if (Action == DemandAction::SetLimit) {
                state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).ManageDemand = true;
                state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).DemandLimit = LowestPower;
            } else if (Action == DemandAction::ClearLimit) {
                state.dataExteriorEnergyUse->ExteriorLights(LoadPtr).ManageDemand = false;
            }

        } else if (SELECT_CASE_var == ManagerType::ManagerTypeLights) {
            LowestPower = state.dataHeatBal->Lights(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
            if (Action == DemandAction::CheckCanReduce) {
                if (state.dataHeatBal->Lights(LoadPtr).Power > LowestPower) CanReduceDemand = true;
            } else if (Action == DemandAction::SetLimit) {
                state.dataHeatBal->Lights(LoadPtr).ManageDemand = true;
                state.dataHeatBal->Lights(LoadPtr).DemandLimit = LowestPower;
            } else if (Action == DemandAction::ClearLimit) {
                state.dataHeatBal->Lights(LoadPtr).ManageDemand = false;
            }

        } else if (SELECT_CASE_var == ManagerType::ManagerTypeElecEquip) {
            LowestPower = state.dataHeatBal->ZoneElectric(LoadPtr).DesignLevel * DemandMgr(MgrNum).LowerLimit;
            if (Action == DemandAction::CheckCanReduce) {
                if (state.dataHeatBal->ZoneElectric(LoadPtr).Power > LowestPower) CanReduceDemand = true;
            } else if (Action == DemandAction::SetLimit) {
                state.dataHeatBal->ZoneElectric(LoadPtr).ManageDemand = true;
                state.dataHeatBal->ZoneElectric(LoadPtr).DemandLimit = LowestPower;
            } else if (Action == DemandAction::ClearLimit) {
                state.dataHeatBal->ZoneElectric(LoadPtr).ManageDemand = false;
            }

        } else if (SELECT_CASE_var == ManagerType::ManagerTypeThermostats) {
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
                if (state.dataHeatBalFanSys->ComfortControlType(state.dataZoneCtrls->TempControlledZone(LoadPtr).ActualZoneNum) > 0) {
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

        } else if (SELECT_CASE_var == ManagerType::ManagerTypeVentilation) {
            Real64 FlowRate(0);
            FlowRate = OAGetFlowRate(state, LoadPtr);
            if (Action == DemandAction::CheckCanReduce) {
                CanReduceDemand = true;
            } else if (Action == DemandAction::SetLimit) {
                OASetDemandManagerVentilationState(state, LoadPtr, true);
                if (DemandMgr(MgrNum).LimitControl == Limit::ManagerLimitFixed) {
                    OASetDemandManagerVentilationFlow(state, LoadPtr, DemandMgr(MgrNum).FixedRate);
                } else if (DemandMgr(MgrNum).LimitControl == Limit::ManagerLimitReductionRatio) {
                    Real64 DemandRate(0);
                    DemandRate = FlowRate * DemandMgr(MgrNum).ReductionRatio;
                    OASetDemandManagerVentilationFlow(state, LoadPtr, DemandRate);
                }
            } else if (Action == DemandAction::ClearLimit) {
                OASetDemandManagerVentilationState(state, LoadPtr, false);
            }
        }
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
